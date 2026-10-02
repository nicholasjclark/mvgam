#' Write an assembled Stan program to disk
#'
#' `save_model` names a path rather than a directory, and brms appends
#' no extension of its own, so the path is taken as given. The parent
#' directory has to exist: creating one silently would put a Stan file
#' somewhere the user did not ask for.
#'
#' @param stancode Character string holding the assembled program.
#' @param path File path to write to.
#' @return `path`, invisibly.
#' @noRd
write_stan_program <- function(stancode, path) {
  checkmate::assert_string(stancode, min.chars = 1)
  checkmate::assert_string(path, min.chars = 1)
  parent <- dirname(path)
  if (!dir.exists(parent)) {
    stop(insight::format_error(c(
      "Cannot write the Stan program to 'save_model'.",
      x = paste0("The directory '", parent, "' does not exist."),
      i = "Create it first, or give a path under a directory that exists."
    )))
  }
  writeLines(stancode, con = path)
  invisible(path)
}

#' Generate Stan Components for mvgam Formula
#'
#' Internal shared infrastructure function that serves as the single source of
#' truth for Stan code generation. Used by both model fitting (\code{mvgam()}) 
#' and inspection functions (\code{stancode()}, \code{standata()}).
#'
#' @param formula An \code{mvgam_formula} object containing observation and trend formulas
#' @param data Data frame containing response and predictor variables
#' @param family Response distribution family, defaults to gaussian()
#' @param prior Prior specifications from \code{get_prior()} or \code{set_prior()}
#' @param data2 Optional additional data for predictions
#' @param sample_prior Whether to sample from priors only ("no", "yes", "only")
#' @param knots Optional knot locations for smooth terms
#' @param drop_unused_levels Whether to drop unused factor levels
#' @param backend Stan backend to use ("rstan" or "cmdstanr")
#' @param threads Number of threads for threading
#' @param normalize Whether to normalize design matrices
#' @param save_model Path to write the assembled Stan program to
#' @param silent Verbosity level (0-2)
#' @param stanvars Additional stanvars to include
#' @param validate Whether to validate generated Stan code
#' @param ... Additional arguments passed to downstream functions
#'
#' @return Named list containing:
#'   \describe{
#'     \item{combined_components}{List with \code{stancode} and \code{standata} elements}
#'     \item{obs_setup}{Observation model setup from brms}
#'     \item{trend_setup}{Trend model setup from brms (NULL if no trends)}
#'     \item{mv_spec}{Multivariate specification object}
#'   }
#'
#' @details
#' This function is the single Stan code-generation entry point used by
#' \code{mvgam()}, \code{stancode()}, and \code{standata()}. It processes
#' formulas, validates time series structure, sets up brms models, generates
#' stanvars, assembles Stan code, and applies polishing. The polished Stan
#' code is byte-identical across the three call sites.
#'
#' @noRd
build_stan_components <- function(formula, data, family = gaussian(),
                                 prior = NULL, data2 = NULL,
                                 sample_prior = "no",
                                 knots = NULL, drop_unused_levels = TRUE,
                                 backend = "rstan",
                                 threads = getOption("mc.cores", 1),
                                 normalize = TRUE, save_model = NULL,
                                 silent = 1L,
                                 stanvars = NULL, validate = TRUE,
                                 trend_map = NULL,
                                 loadings_prior = NULL, ...) {

  # Input validation
  checkmate::assert_class(formula, "mvgam_formula")
  checkmate::assert_data_frame(data, min.rows = 1)
  if (!is.null(data2)) {
    checkmate::assert(
      checkmate::check_data_frame(data2),
      checkmate::check_list(data2, min.len = 1, names = "named"),
      .var.name = "data2"
    )
  }
  if (!is.null(save_model)) checkmate::assert_string(save_model)
  if (!is.null(stanvars)) checkmate::assert_class(stanvars, "stanvars")
  if (!is.null(prior)) checkmate::assert_class(prior, "brmsprior")
  checkmate::assert_choice(backend, c("rstan", "cmdstanr"))
  checkmate::assert_int(threads, lower = 1)
  checkmate::assert_int(silent, lower = 0, upper = 2)
  # `stancode()` and `standata()` reach here without passing through
  # `mvgam()`, which records `silent` for its own call. The mock brms
  # fit below takes its verbosity from the option.
  old_silent <- options(mvgam.silent = silent)
  on.exit(options(old_silent), add = TRUE)

  # One list carries the brms code-generation options from here to
  # the mock fit and on to every regeneration underneath it, so the
  # program, its data and the prior table cannot be built under
  # different bases or different factor levels. It validates its own
  # arguments.
  codegen <- mvgam_codegen_options(
    knots = knots,
    sample_prior = sample_prior,
    drop_unused_levels = drop_unused_levels,
    normalize = normalize
  )

  # Extract components from mvgam_formula
  obs_formula <- formula$formula
  trend_formula <- formula$trend_formula

  # The family a univariate `bf()` names becomes the model's. Every
  # response's family is checked before any step below uses `family`.
  resolved <- resolve_observation_family(obs_formula, family)
  obs_formula <- resolved$formula
  family <- resolved$family

  # Refusing an absent or incomplete column here covers every route to
  # a program, `stancode()` and `standata()` as well as `mvgam()`.
  validate_model_columns(data, obs_formula, family, trend_formula, data2)

  # Parse multivariate trends and validate. The trend names the time
  # and series columns, which key a closure unit below.
  mv_spec <- parse_multivariate_trends(obs_formula, trend_formula)
  axis_names <- spec_axis_vars(mv_spec$trend_specs)

  # Closure-unit families (`nmix()` and its variants, `occ()`, and the
  # multi-response families) carry per-data Stan stanvars that are built
  # at fit time from the user's observation data: unit indexing
  # arrays, per-unit upper truncation (K_max), per-unit max obs
  # (Y_max), and the family-specific lpdf function block. The
  # arrays change with the data, so the resolution is deferred
  # to here rather than baked into the family() constructor. A
  # closure-unit family models its response alone, which
  # `resolve_observation_family()` has established.
  if (uses_closure_unit_layout(family)) {
    main_formula <- obs_arm_main_formula(obs_formula)
    dpar_forms <- if (inherits(obs_formula, "brmsformula")) {
      obs_formula$pforms %||% list()
    } else {
      list()
    }
    response_var <- unname(response_columns(obs_formula)[1L])
    has_obs_covs <- length(
      attr(stats::terms(main_formula), "term.labels")
    ) > 0L
    has_det_covs <- "p" %in% names(dpar_forms) &&
      length(attr(stats::terms(dpar_forms$p), "term.labels")) > 0L
    family <- prepare_closure_unit_family(
      family,
      data               = data,
      response_var       = response_var,
      has_obs_covariates = has_obs_covs,
      has_det_covariates = has_det_covs,
      prior              = prior,
      series_var         = axis_names$series_var,
      time_var           = axis_names$time_var
    )
  }

  # Each response's family, read once the closure-unit family above
  # carries its data.
  families <- formula_families(obs_formula, family)

  # Custom families (e.g. tweedie()) carry their own Stan function
  # block + data stanvars in attr(family, "mvgam_stanvars"). They
  # belong on the observation submodel only; the trend submodel
  # uses gaussian() and reusing them there would duplicate the
  # function block + the M data int and explode the final
  # c.stanvars merge.
  obs_stanvars <- attach_family_stanvars(stanvars, families)
  trend_stanvars_in <- stanvars

  mv_spec$trend_specs <- prepare_trend_specs(
    mv_spec$trend_specs, trend_map, loadings_prior, data, data2
  )

  # `n_lv` belongs to the trend that carries the factors, so it is
  # written on the constructor inside `trend_formula`. Passed to
  # `mvgam()` itself it lands in `...`, which is the pass-through to
  # brms and Stan, and nothing there reads it: asked for one factor
  # on two series, the fit came back with two and said nothing.
  refuse_top_level_n_lv(list(...)$n_lv)

  # Two cases need brms threading suppressed at the stancode level.
  # Both rewrite the local `threads` so brms's downstream
  # `partial_log_lik_lpmf` emission stays off; the compile-time
  # `cpp_options$stan_threads` setting is enabled independently by
  # `mvgam_single()` / `.compile_model_cmdstanr()`, so mvgam-emitted
  # `reduce_sum` calls still parallelise.
  #
  # 1. brms-native + `trend_formula`: silent no-op of the user's
  #    parallelism request; surface a one-time warning so they know.
  # 2. closure-unit / multi-response: silent. mvgam ships its own
  #    `partial_sum_<family>_lpmf` + `reduce_sum`; letting brms
  #    nest a `partial_log_lik_lpmf` wrapper around them produces
  #    an out-of-scope signature error on the transformed-data
  #    args (`visit_idx`, `log_n_lookup`, etc.).
  warn_threads_trend_brms_native(threads, family, mv_spec)
  if (suppress_brms_threading(threads, family, mv_spec)) {
    threads <- 1L
  }

  # Setup observation model using lightweight brms.
  # Simplex multi-response families (`diri`, `multi`, `categ`)
  # impose a hard sum-to-zero constraint on the loadings matrix `Z`
  # via Stan's `sum_to_zero_vector[K]`, so they need Stan >= 2.36.
  if (is_simplex_response_family(family)) {
    assert_stan_version(
      backend, "2.36.0",
      feature = "'sum_to_zero_vector[K]' for simplex families"
    )
  }
  # `com_binomial()` reads its denominator from its own response's
  # `trials()` term.
  forms <- response_formulas(obs_formula)
  for (key in names(families)) {
    if (is_com_binomial_family(families[[key]])) {
      assert_com_binomial_trials(forms[[key]])
    }
  }
  # Families whose brms fallback prior would be unsuitable carry an
  # mvgam default; `response_default_priors()` is the one definition
  # `get_prior()` also reports. `merge_default_priors()` drops any
  # default the user named, so a user prior still wins.
  prior <- merge_default_priors(
    response_default_priors(obs_formula, family), prior
  )

  # Filter priors: only pass observation-related priors to observation setup
  obs_priors <- filter_priors_by_side(prior, "obs")
  
  # What both submodels are built under. Named once, because the two
  # calls below differ only in their formula, data, family, priors and
  # stanvars, and an argument added to one and not the other builds the
  # observation and trend halves under different settings without
  # saying so.
  shared_setup <- list(
    data2 = data2,
    codegen = codegen,
    backend = backend,
    threads = threads,
    silent = silent
  )

  obs_setup <- do.call(setup_brms_lightweight, c(
    list(
      formula = obs_formula,
      data = data,
      family = family,
      prior = obs_priors,
      stanvars = obs_stanvars
    ),
    shared_setup,
    list(...)
  ))

  trend_priors <- NULL
  trend_setup <- if (mv_spec$has_trends) {
    checkmate::assert_class(mv_spec$base_formula, "formula")
    trend_priors <- filter_priors_by_side(prior, "trend")

    # Each response's column, named by the key brms suffixes its data
    # and parameters with: the frame is read by the one and the
    # program is written with the other.
    response_vars <- response_columns(obs_formula)

    # The axes are resolved here once, and the dimensions they give
    # travel on the spec
    components <- extract_and_validate_trend_components(
      data, mv_spec, response_vars, family = family
    )
    trend_data <- components$trend_data
    mv_spec <- components$enhanced_mv_spec
    trend_metadata <- components$metadata

    do.call(setup_brms_lightweight, c(
      list(
        formula = mv_spec$base_formula,
        data = trend_data,  # Use reduced trend data
        # Trends are gaussian processes per architecture
        family = gaussian(),
        prior = remove_trend_suffix_from_priors(
          trend_priors, mv_spec$trend_specs, mv_spec$base_formula,
          trend_data, codegen
        ),
        stanvars = trend_stanvars_in,
        # Mark this as the trend invocation so
        # setup_brms_lightweight() skips the empty-obs-formula
        # placeholder injection (the injected pin assignment would
        # be orphaned by mvgam's downstream trend-stancode rewriter).
        is_trend_setup = TRUE
      ),
      shared_setup,
      list(...)
    ))
  } else {
    NULL
  }

  # A model with no trend skips the branch above, where the record is
  # built. Which series a model has, when each was last observed and
  # whether the responses are the series are facts about the frame,
  # and post-fit takes its labels, its forecast horizons and the
  # response axis of a wide frame from them.
  if (!mv_spec$has_trends) {
    trend_metadata <- trendless_trend_metadata(
      data, "time", "series", response_columns(obs_formula)
    )
  }

  combined_components <- generate_combined_stancode(
    obs_setup = obs_setup,
    trend_setup = trend_setup,
    trend_specs = mv_spec$trend_specs,
    prior = trend_priors,
    backend = backend
  )

  # Polish Stan code for consistent formatting and spacing
  combined_components$stancode <- paste(
    polish_generated_stan_code(combined_components$stancode),
    collapse = "\n"
  )

  # Parse the polished program, which is the one compiled. Polishing
  # moves statements between blocks, and a parse of the unpolished
  # program cannot see a statement moved away from the scope it needs.
  # `backend` picks the parser the user compiles with: simplex families
  # need Stan >= 2.36 through cmdstanr and fail under rstan's bundled
  # parser.
  if (isTRUE(validate)) {
    validate_stan_code(
      combined_components$stancode,
      backend = backend,
      silent = 1
    )
  }

  # `save_model` writes the program mvgam assembles, not the brms
  # program it starts from, since the assembled one is what gets
  # compiled and is the only one that names the trend.
  if (!is.null(save_model)) {
    write_stan_program(combined_components$stancode, save_model)
  }
  
  # The two designs one likelihood sees together, checked once the
  # data is assembled.
  warn_confounded_design(combined_components$standata)

  # Return all components needed for mvgam object creation
  return(list(
    combined_components = combined_components,
    obs_setup = obs_setup,
    trend_setup = trend_setup,
    mv_spec = mv_spec,
    trend_metadata = trend_metadata
  ))
}

#' Generate the Stan components for an `mvgam_formula`
#'
#' The single code-generation entry point behind `mvgam()`,
#' `stancode()` and `standata()`. Delegates to
#' `build_stan_components()`, which runs brms's code generator three
#' times: once for the lightweight skeleton, once for the base Stan
#' code and once for the base Stan data. All three validate the same
#' data frame, so anything brms reports about that data would
#' otherwise reach the user three times over for one model.
#'
#' @inheritParams build_stan_components
#' @inherit build_stan_components return
#'
#' @noRd
generate_stan_components_mvgam_formula <- function(...) {
  warn_once_per_call(build_stan_components(...))
}


#' Is the trend scale confounded with the observation scale?
#'
#' `ZMVN()` gives the latent state no temporal structure, so on a
#' single series its innovations and the observation residuals are
#' both iid draws with nothing to tell them apart: only
#' `sigma_trend^2 + sigma^2` is identified and the split between
#' them is whatever the priors say. Two series are enough to break
#' the tie, since `Sigma_trend` then carries a cross-series
#' covariance the observation noise cannot produce.
#'
#' A family with no residual scale is unaffected: a Poisson has no
#' `sigma` for the trend to trade against.
#'
#' @param mv_spec The multivariate trend spec.
#' @param family The observation family.
#' @param n_series Length of the resolved series axis.
#' @return `TRUE` when the two scales are confounded.
#' @noRd
zmvn_scale_confounded <- function(mv_spec, family, n_series) {
  checkmate::assert_int(n_series, lower = 1L)
  identical(trend_spec_head(mv_spec$trend_specs)$trend, "ZMVN") &&
    n_series == 1L && "sigma" %in% family$dpars
}


#' Warn once when a single-series ZMVN cannot split its scales
#' @noRd
warn_zmvn_single_series <- function(mv_spec, family, n_series) {
  if (!zmvn_scale_confounded(mv_spec, family, n_series)) return()
  warn_once(
    c(
      "A 'ZMVN()' trend on one series shares its scale with 'sigma'.",
      x = "The data identify only the sum of the two variances.",
      x = "The priors set the split between 'sigma_trend' and 'sigma'.",
      i = "Add series, or use a trend with temporal structure such as 'AR()'.",
      i = "Or set a prior that pins one of the two scales."
    ),
    "mvgam_zmvn_single_series"
  )
}


# Internal: the observation and trend designs stacked on one row per
# observation.
#
# An observation's linear predictor takes its own design row together
# with the trend design row at
# `times_trend[obs_trend_time, obs_trend_series]`. Stacking the two
# gives the columns one likelihood sees at once, and the rank of that
# matrix settles whether their coefficients are separately
# identified.
#
# Covered: the parametric designs, the smooth bases and the Gaussian
# process bases, on both sides. A grouping random effect is left out.
# brms passes it as a sparse expansion against its own index, and
# partial pooling identifies it by shrinkage.
#
# `NULL` where no comparison exists: a fit carrying one design alone,
# a multivariate formula whose responses hold designs of their own,
# and a `by = lv_axis()` trend whose second dimension counts factors.
#'@noRd
stacked_design_matrix <- function(sdata) {
  n <- sdata$N
  n_trend <- sdata$N_trend
  idx <- obs_rows_to_trend_rows(sdata)
  if (is.null(n) || is.null(n_trend) || is.null(idx) || anyNA(idx)) {
    return(NULL)
  }
  if (!identical(ncol(sdata$times_trend),
                 as.integer(sdata$N_series_trend))) {
    return(NULL)
  }
  is_design <- function(nm) {
    v <- sdata[[nm]]
    is.numeric(v) && length(dim(v)) == 2L &&
      !grepl("_prior_", nm, fixed = TRUE) &&
      !identical(nm, "times_trend")
  }
  nms <- Filter(is_design, names(sdata))
  trend_nms <- Filter(function(nm) {
    grepl("_trend$", nm) && nrow(sdata[[nm]]) == n_trend
  }, nms)
  obs_nms <- Filter(function(nm) {
    !grepl("_trend$", nm) && nrow(sdata[[nm]]) == n
  }, nms)
  # One observation design named `X`. A multivariate formula keys its
  # designs by response, and those belong to separate likelihoods.
  if (!("X" %in% obs_nms) || !length(trend_nms)) {
    return(NULL)
  }
  # A block carries its own column names where brms wrote them, and
  # a basis column is numbered otherwise, which keeps every column of
  # the result nameable in the notice.
  labelled <- function(nm, rows) {
    block <- as.matrix(sdata[[nm]])
    if (!is.null(rows)) {
      block <- block[rows, , drop = FALSE]
    }
    cols <- colnames(block)
    if (is.null(cols) || !all(nzchar(cols))) {
      cols <- paste0(nm, "[", seq_len(ncol(block)), "]")
    } else {
      cols <- paste0(nm, ":", cols)
    }
    colnames(block) <- cols
    block
  }
  obs_part <- do.call(cbind, lapply(obs_nms, labelled, rows = NULL))
  trend_part <- do.call(cbind, lapply(trend_nms, labelled, rows = idx))
  cbind(obs_part, trend_part)
}


# Internal: the columns of a stacked design that repeat a direction
# the others already span, or none.
#
# A column of zeros moves no fitted value and shares a direction with
# nothing. The empty-observation placeholder is one, pinned at zero
# by its prior. A rank taken on raw columns follows their scales, and
# the observation design is centred where the trend design is not,
# so each column is scaled to unit length first.
#
# @param m The matrix `stacked_design_matrix()` returns, or `NULL`
# @return Character vector of column names
#'@noRd
confounded_columns <- function(m) {
  if (is.null(m)) {
    return(character(0L))
  }
  norms <- sqrt(colSums(m^2))
  m <- m[, norms > 0, drop = FALSE]
  if (ncol(m) < 2L) {
    return(character(0L))
  }
  q <- qr(sweep(m, 2L, norms[norms > 0], "/"))
  if (q$rank >= ncol(m)) {
    return(character(0L))
  }
  colnames(m)[q$pivot[seq.int(q$rank + 1L, ncol(m))]]
}


# Internal: warn once when the two designs share a direction.
#
# A term written into both formulas moves the same fitted values from
# either side. The model still samples, and the priors alone set the
# split between the two coefficients. `y ~ 1` with
# `~ series + AR(p = 1)` is one such pairing and a reasonable model
# to write. This names the columns involved and continues.
#'@noRd
warn_confounded_design <- function(sdata) {
  dependent <- confounded_columns(stacked_design_matrix(sdata))
  if (!length(dependent)) {
    return(invisible(NULL))
  }
  warn_once(
    c(
      "The observation and trend designs are collinear.",
      x = paste0("Columns that repeat a direction of the other side: ",
                 paste(dependent, collapse = ", "), "."),
      i = "The priors alone set how the effect splits between the sides.",
      i = "Drop the term from one formula or pin one side with a prior."
    ),
    "mvgam_confounded_design"
  )
}

# Internal: which case makes brms's `partial_log_lik_lpmf` emission
# unwanted for the obs-side `setup_brms_lightweight()` call. Two cases
# need the gate, and both are decided from the same three inputs, so
# one classification serves the two questions asked of them.
#
#   1. **Closure-unit and multi-response families.** These ship their
#      own `partial_sum_<family>_lpmf` + inner `reduce_sum` call via
#      mvgam-side stanvars. brms's outer `partial_log_lik_lpmf` would
#      nest a second wrapper around `nmix_lpmf` / `occ_lpmf` /
#      simplex / mvn-style lpmfs, and the brms function signature
#      cannot see transformed-data args (`visit_idx`, `log_n_lookup`,
#      etc.), which fails Stan validation. The fit-time
#      `cpp_options$stan_threads = TRUE` travels separately through
#      `mvgam_single()` / `.compile_model_cmdstanr()`, and the inner
#      `reduce_sum` still parallelises. Only brms's double-wrapping
#      is stopped.
#
#   2. **brms-native families combined with a `trend_formula`.** brms
#      moves both the `mu` declaration and every linpred assignment
#      into `partial_log_lik_lpmf` inside `functions {}`, and mvgam's
#      obs-side trend injector searches `model {}` alone, which leaves
#      the assignment it splices the trend addition into out of reach.
#      The combination compiles to a silent serial fit or crashes.
#
# Case 1 is silent, since mvgam's own threading still works. Case 2
# warns, through `warn_threads_trend_brms_native()`.
#'@noRd
brms_threading_case <- function(threads, family, mv_spec) {
  if (!is.numeric(threads) || !isTRUE(threads > 1)) return("none")
  if (uses_closure_unit_layout(family)) return("closure_unit")
  if (is_multi_response_family(family)) return("multi_response")
  if (!is.null(mv_spec) && isTRUE(mv_spec$has_trends)) {
    return("trend_native")
  }
  "none"
}

#'@noRd
suppress_brms_threading <- function(threads, family, mv_spec) {
  !identical(brms_threading_case(threads, family, mv_spec), "none")
}

#'@noRd
threads_no_op_for_trend_brms_native <- function(threads, family, mv_spec) {
  identical(
    brms_threading_case(threads, family, mv_spec), "trend_native"
  )
}

#'@noRd
warn_threads_trend_brms_native <- function(threads, family, mv_spec) {
  if (!threads_no_op_for_trend_brms_native(threads, family, mv_spec)) {
    return()
  }
  # This warning is raised on every fit. A batch script that re-fits
  # after a config change keeps seeing it each time its threads
  # request is dropped.
  insight::format_warning(c(
    "'threads' is not supported for a brms family with a 'trend_formula'.",
    x = "brms threads a likelihood that the latent trend has to join.",
    i = "The model samples serially.",
    i = "Closure-unit and multi-response families thread as requested."
  ))
}


#' Generate Stan Model Code for mvgam Formula
#'
#' Generate complete Stan model code for an \code{mvgam_formula} object before
#' model fitting. This allows users to inspect the generated Stan code for
#' debugging, understanding, or modification purposes.
#'
#' @param object An \code{mvgam_formula} object created by \code{mvgam_formula()}.
#' @param data A data frame containing all variables referenced in the formula.
#' @inheritParams mvgam
#' @param prior An optional \code{brmsprior} object containing custom prior
#'   specifications. Can be created using \code{brms::prior()}.
#' @param data2 Optional data frame for out-of-sample prediction points.
#' @param sample_prior Character string indicating whether to sample from priors
#'   only. Options: \code{"no"} (default), \code{"yes"}, \code{"only"}.
#' @param knots Optional named list of knot positions for spline terms, passed
#'   to the basis constructors for both the observation and trend formulas.
#' @param drop_unused_levels Logical; should unused factor levels be dropped?
#'   Default is \code{TRUE}.
#' @param backend Character string specifying Stan backend. Options:
#'   \code{"rstan"} (default) or \code{"cmdstanr"}.
#' @param threads Number of threads to use for parallelization. Default is
#'   \code{getOption("mc.cores", 1)}.
#' @param normalize Logical; should brms drop the normalising constants from
#'   its sampling statements? Default is \code{TRUE}.
#' @param save_model File path to write the assembled Stan program to. If
#'   \code{NULL} (default), nothing is written.
#' @param silent Integer controlling verbosity, following the
#'   \pkg{brms} convention. 0 prints Stan's exceptions as well as its
#'   progress, 1 (the default) prints progress alone, and 2 prints
#'   nothing.
#' @param stanvars Optional \code{\link[brms]{stanvar}} object holding
#'   user Stan code to add to the program, merged with the stanvars
#'   mvgam generates for the trend.
#' @param validate Logical; should the generated Stan code be validated?
#'   Default is \code{TRUE}.
#' @param ... Additional arguments passed to internal functions.
#'
#' @return A character string containing complete, polished Stan model code with data,
#'   parameters, model, and generated quantities blocks for both observation
#'   and trend components. The returned code is automatically polished for
#'   consistent formatting and is identical to the code used internally by
#'   \code{mvgam()} for model fitting.
#'
#' @details
#' This function uses the shared Stan code generation infrastructure 
#' (\code{generate_stan_components_mvgam_formula()}) to ensure identical output 
#' to what \code{mvgam()} uses internally. The Stan code is automatically 
#' polished for consistent formatting, with empty lines removed and whitespace 
#' trimmed. This guarantees that \code{stancode()} produces exactly the same 
#' model code that would be compiled and fit by \code{mvgam()}.
#'
#' @seealso
#' \code{\link{mvgam_formula}}, \code{\link{get_prior.mvgam_formula}},
#' \code{\link[brms]{standata}}, \code{\link{mvgam}}
#'
#' @export
stancode.mvgam_formula <- function(object, data, family = gaussian(),
                                   prior = NULL, data2 = NULL,
                                   sample_prior = "no",
                                   knots = NULL, drop_unused_levels = TRUE,
                                   backend = "rstan",
                                   threads = getOption("mc.cores", 1),
                                   normalize = TRUE, save_model = NULL,
                                   silent = 1L, stanvars = NULL,
                                   validate = TRUE, ...) {
  reject_removed_args(list(...), fn = "stancode")
  stancode <- mvgam_formula_component(
    "stancode", object, data, family, prior, data2, sample_prior,
    knots, drop_unused_levels, backend, threads, normalize,
    save_model, silent, stanvars, validate, ...
  )
  class(stancode) <- c("mvgamstancode", "stancode", "character")
  return(stancode)
}


#' One component of the program an `mvgam_formula` describes
#'
#' `stancode()` and `standata()` take different components of one
#' generated object and agree on everything reaching the generator:
#' the same arguments in the same order, and the same refusal when
#' the result arrives without the component named.
#'
#' @param component `"stancode"` or `"standata"`.
#' @param object,data,family,prior,data2,sample_prior,knots Arguments
#'   of `stancode.mvgam_formula()`, forwarded unchanged.
#' @param drop_unused_levels,backend,threads,normalize Likewise.
#' @param save_model,silent,stanvars,validate,... Likewise.
#' @return The named component of the generated result.
#' @noRd
mvgam_formula_component <- function(component, object, data, family,
                                    prior, data2, sample_prior, knots,
                                    drop_unused_levels, backend,
                                    threads, normalize, save_model,
                                    silent, stanvars, validate, ...) {
  checkmate::assert_choice(component, c("stancode", "standata"))
  generated <- generate_stan_components_mvgam_formula(
    formula = object, data = data, family = family, prior = prior,
    data2 = data2, sample_prior = sample_prior,
    knots = knots, drop_unused_levels = drop_unused_levels,
    backend = backend, threads = threads, normalize = normalize,
    save_model = save_model, silent = silent, stanvars = stanvars,
    validate = validate,
    ...
  )
  generated$combined_components[[component]]
}

#' Stan program and data from a formula carrying a `trend_formula`
#'
#' @description
#' `stancode()` and `standata()` delegate a plain formula to
#' \pkg{brms}. A `trend_formula` in the same call names an mvgam
#' model, and these methods build the \code{\link{mvgam_formula}} that
#' call describes before generating the program. A call supplying no
#' `trend_formula` delegates to \pkg{brms} unchanged.
#'
#' @param object A `formula` or `brmsformula`.
#' @param ... Arguments for \code{\link{stancode.mvgam_formula}} or for
#'   \pkg{brms}, `data` and `family` among them.
#'
#' @return A character string of Stan code, or a list of Stan data.
#'
#' @seealso \code{\link{mvgam_formula}},
#'   \code{\link{stancode.mvgam_formula}},
#'   \code{\link{standata.mvgam_formula}}
#'
#' @rdname trend_formula_generation
#' @export
stancode.formula <- function(object, ...) {
  dots <- list(...)
  if (is.null(dots$trend_formula)) {
    return(NextMethod())
  }
  route_trend_formula(object, dots, stancode)
}

#' @rdname trend_formula_generation
#' @export
stancode.brmsformula <- function(object, ...) {
  dots <- list(...)
  if (is.null(dots$trend_formula)) {
    return(NextMethod())
  }
  route_trend_formula(object, dots, stancode)
}

#' @rdname trend_formula_generation
#' @export
standata.formula <- function(object, ...) {
  dots <- list(...)
  if (is.null(dots$trend_formula)) {
    return(NextMethod())
  }
  route_trend_formula(object, dots, standata)
}

#' @rdname trend_formula_generation
#' @export
standata.brmsformula <- function(object, ...) {
  dots <- list(...)
  if (is.null(dots$trend_formula)) {
    return(NextMethod())
  }
  route_trend_formula(object, dots, standata)
}

#' Generate Stan Data for mvgam Formula
#'
#' Generate complete Stan data list for an \code{mvgam_formula} object before
#' model fitting. This allows users to inspect the generated data structure for
#' debugging, understanding, or modification purposes.
#'
#' @param object An \code{mvgam_formula} object created by \code{mvgam_formula()}.
#' @param data A data frame containing all variables referenced in the formula.
#' @inheritParams mvgam
#' @param prior An optional \code{brmsprior} object containing custom prior
#'   specifications. Can be created using \code{brms::prior()}.
#' @param data2 Optional data frame for out-of-sample prediction points.
#' @param sample_prior Character string indicating whether to sample from priors
#'   only. Options: \code{"no"} (default), \code{"yes"}, \code{"only"}.
#' @param knots Optional named list of knot positions for spline terms, passed
#'   to the basis constructors for both the observation and trend formulas.
#' @param drop_unused_levels Logical; should unused factor levels be dropped?
#'   Default is \code{TRUE}.
#' @param stanvars Optional \code{stanvars} object containing additional Stan
#'   variables, parameters, or functions.
#' @param backend Character string specifying Stan backend. Options:
#'   \code{"rstan"} (default) or \code{"cmdstanr"}.
#' @param threads Number of threads to use for parallelization. Default is
#'   \code{getOption("mc.cores", 1)}.
#' @param normalize Logical; should brms drop the normalising constants
#'   from its sampling statements? Default is \code{TRUE}.
#' @param save_model File path to write the assembled Stan program to.
#'   If \code{NULL} (default), nothing is written.
#' @param silent Integer controlling verbosity, following the \pkg{brms}
#'   convention. 0 prints Stan's exceptions as well as its progress, 1
#'   (the default) prints progress alone, and 2 prints nothing.
#' @param validate Logical; should the assembled Stan code be validated?
#'   Default is \code{TRUE}.
#' @param ... Additional arguments passed to internal functions.
#'
#' @return A named list containing all data for Stan model including observation
#'   and trend data components. Structure matches exactly what would be passed to 
#'   Stan during fitting by \code{mvgam()}. This data is generated using the same
#'   shared infrastructure as \code{mvgam()}, ensuring consistency.
#'
#' @details
#' This function uses the shared Stan data generation infrastructure
#' (\code{generate_stan_components_mvgam_formula()}) to ensure identical output
#' to what \code{mvgam()} uses internally. The returned data structure includes
#' all observation data, trend mappings, dimensions, and stanvars needed for
#' model compilation and fitting.
#'
#' @seealso
#' \code{\link{mvgam_formula}}, \code{\link{get_prior.mvgam_formula}},
#' \code{\link[brms]{stancode}}, \code{\link{mvgam}}
#'
#' @export
standata.mvgam_formula <- function(object, data, family = gaussian(),
                                   prior = NULL, data2 = NULL,
                                   sample_prior = "no",
                                   knots = NULL, drop_unused_levels = TRUE,
                                   backend = "rstan",
                                   threads = getOption("mc.cores", 1),
                                   normalize = TRUE, save_model = NULL,
                                   silent = 1L, stanvars = NULL,
                                   validate = TRUE, ...) {
  reject_removed_args(list(...), fn = "standata")
  standata <- mvgam_formula_component(
    "standata", object, data, family, prior, data2, sample_prior,
    knots, drop_unused_levels, backend, threads, normalize,
    save_model, silent, stanvars, validate, ...
  )

  # Validate Stan data structure follows brms conventions
  if (!is.list(standata) || length(standata) == 0L) {
    stop_mvgam_fault(
      "The generated Stan data must be a non-empty list.",
      paste0("Got class '", class(standata)[1L], "' of length ",
             length(standata), ".")
    )
  }

  return(standata)
}
