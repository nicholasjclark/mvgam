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

  # The family a univariate `bf()` names becomes the model's, and
  # every response's family is checked, before anything below reads
  # `family`.
  resolved <- resolve_observation_family(obs_formula, family)
  obs_formula <- resolved$formula
  family <- resolved$family

  # Closure-unit families (nmix, future occ / royle_nichols /
  # poisson_poisson) carry per-data Stan stanvars that are built
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
      has_det_covariates = has_det_covs
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

  # Parse multivariate trends and validate
  if (is.null(mv_spec <- parse_multivariate_trends(obs_formula, trend_formula))) {
    stop(insight::format_error(c(
      "Failed to parse trend formula specification.",
      i = cli::format_inline(
        "Check your {.arg trend_formula} syntax and constructor arguments."
      )
    )))
  }

  # Apply the top-level `trend_map` alias to the parsed trend
  # specs. Collision detection: error if the user supplied
  # `trend_map` both at `mvgam(..., trend_map = ...)` AND on the
  # trend constructor (e.g. `AR(trend_map = ...)`).
  mv_spec$trend_specs <- apply_trend_map_alias(
    mv_spec$trend_specs, trend_map
  )

  # Normalise each spec's raw `trend_map` (matrix / data.frame /
  # character code) to a canonical numeric Z. Stashed on
  # `spec$fixed_Z`; `spec$n_lv` is reconciled to `ncol(Z)`. Stan
  # emission downstream reads `fixed_Z` and skips the sampled-Z
  # path when it is non-NULL.
  mv_spec$trend_specs <- normalise_trend_map_on_specs(
    mv_spec$trend_specs, data
  )

  # Normalise the structured-loadings prior once at the top
  # level and attach to each trend spec. The same spec is shared
  # across multivariate trends because mvgam fits one shared
  # trend component across responses.
  loadings_prior_spec <- normalise_loadings_prior(
    loadings_prior, data2 = data2, data = data
  )
  if (!is.null(loadings_prior_spec)) {
    mv_spec$trend_specs <- attach_loadings_prior_spec(
      mv_spec$trend_specs, loadings_prior_spec
    )
  }

  # `n_lv` belongs to the trend that carries the factors, so it is
  # written on the constructor inside `trend_formula`. Passed to
  # `mvgam()` itself it lands in `...`, which is the pass-through to
  # brms and Stan, and nothing there reads it: asked for one factor
  # on two series, the fit came back with two and said nothing.
  refuse_top_level_n_lv(list(...)$n_lv)

  # Whether this trend has a factor form at all, read from the
  # registry that records it. Asked here because every route a
  # factor can be requested by has landed by now: the constructor's
  # `n_lv` and a `trend_map` normalised to a fixed `Z`.
  enforce_factor_support_against_specs(mv_spec$trend_specs)

  # The `n_lv` ceiling, shared with `jsdgam()` so more factors than
  # series is refused in one place. Reads `n_lv` from the possibly
  # nested trend spec list.
  enforce_n_lv_ceiling_against_data(
    mv_spec$trend_specs, data, family = family
  )

  # PW trends define their own intercept via `m_trend`. An
  # observation-side intercept competes with it for the same
  # constant offset, so soft-warn the user once per session.
  warn_pw_obs_intercept(mv_spec, obs_formula)
  warn_zmvn_single_series(mv_spec, family, data)

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

  if (is.null(obs_setup <- do.call(setup_brms_lightweight, c(
    list(
      formula = obs_formula,
      data = data,
      family = family,
      prior = obs_priors,
      stanvars = obs_stanvars
    ),
    shared_setup,
    list(...)
  )))) {
    stop(insight::format_error(c(
      "Failed to setup observation model with brms.",
      i = cli::format_inline(
        "Check your {.arg formula} and {.arg data} compatibility."
      )
    )))
  }

  # Initialize trend_priors outside conditional block
  trend_priors <- NULL
  
  # Setup trend model if trends are specified
  trend_setup <- if (mv_spec$has_trends) {
    # Validate trend model prerequisites
    checkmate::assert_class(mv_spec$base_formula, "formula")
    
    # Filter priors: only pass trend-related priors to trend setup
    trend_priors <- filter_priors_by_side(prior, "trend")
    
    # `trend_spec_head()` takes the first of a multivariate set and
    # the spec itself for a univariate one.
    axis_names <- spec_axis_vars(mv_spec$trend_specs)
    time_var <- axis_names$time_var
    series_var <- axis_names$series_var
    
    # Each response's column, named by the key brms suffixes its data
    # and parameters with: the frame is read by the one and the
    # program is written with the other.
    response_vars <- response_columns(obs_formula)
    
    # Consolidated trend processing - replaces dual path architecture
    components <- extract_and_validate_trend_components(
      data, mv_spec, response_vars, time_var, series_var, trend_formula
    )
    trend_data <- components$trend_data
    mv_spec <- components$enhanced_mv_spec  # Already has dimensions injected
    trend_metadata <- components$metadata
    
    if (is.null(trend_result <- do.call(setup_brms_lightweight, c(
      list(
        formula = mv_spec$base_formula,
        data = trend_data,  # Use reduced trend data
        # Trends are gaussian processes per architecture
        family = gaussian(),
        prior = remove_trend_suffix_from_priors(
          trend_priors, mv_spec$trend_specs, mv_spec$base_formula,
          trend_data
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
    )))) {
      stop(insight::format_error(c(
        "Failed to setup trend model with brms.",
        i = cli::format_inline(
          "Check your {.arg trend_formula} and {.arg data} compatibility."
        )
      )))
    }
    trend_result
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

  # Generate combined Stan code and data using existing infrastructure
  combined_components <- generate_combined_stancode_and_data(
    obs_setup = obs_setup,
    trend_setup = trend_setup,
    mv_spec = mv_spec,
    prior = trend_priors,  # Pass unfiltered trend priors to mvgam functions
    backend = backend
  )

  # Validate result structure with specific error locations
  if (is.null(combined_components)) {
    stop(insight::format_error(c(
      "Stan component generation returned NULL result.",
      i = cli::format_inline(
        "This indicates a failure in {.fn generate_combined_stancode_and_data}."
      )
    )))
  }

  if (!is.list(combined_components)) {
    stop(insight::format_error(c(
      cli::format_inline(
        "Stan component generation returned invalid type: {.cls {class(combined_components)}}."
      ),
      i = cli::format_inline(
        "Expected list from {.fn generate_combined_stancode_and_data}."
      )
    )))
  }

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
#' @param data The modelling data.
#' @return `TRUE` when the two scales are confounded.
#' @noRd
zmvn_scale_confounded <- function(mv_spec, family, data) {
  specs <- mv_spec$trend_specs
  if (is.null(specs)) return(FALSE)
  specs <- if (is_multivariate_trend_specs(specs)) specs else list(specs)
  has_zmvn <- any(vapply(
    specs, function(sp) identical(sp$trend, "ZMVN"), logical(1L)
  ))
  if (!has_zmvn) return(FALSE)

  # `specs` is a list of specifications by the line above, and an
  # unnamed one of length one for a univariate trend.
  # `trend_spec_head()` requires the multivariate predicate before
  # taking a first element, which an unnamed list fails. The column
  # comes from the first specification.
  series_var <- spec_axis_vars(specs[[1L]])$series_var
  if (!series_var %in% colnames(data)) return(FALSE)
  if (length(unique(data[[series_var]])) > 1L) return(FALSE)

  "sigma" %in% family$dpars
}


#' Warn once when a single-series ZMVN cannot split its scales
#' @noRd
warn_zmvn_single_series <- function(mv_spec, family, data) {
  if (!zmvn_scale_confounded(mv_spec, family, data)) return()
  warn_confound(
    "zmvn_single_series",
    paste0(
      "A 'ZMVN()' trend on one series shares its scale with the ",
      "observation error. The latent state has no temporal ",
      "structure. Only the sum of the two variances is identified: ",
      "the priors alone set the split between 'sigma_trend' and ",
      "'sigma'. To separate them, add series or choose a trend ",
      "with temporal structure such as 'AR()' or 'RW()'. ",
      "Alternatively, set a prior that says which scale you mean ",
      "to pin."
    )
  )
}


# Internal: soft-warn when a PW trend coincides with an
# observation-side intercept. Both terms shift the linear
# predictor by a constant, so leaving both in the model creates
# a non-identifiable pair. Mirrors the Prophet convention of
# fitting with no obs intercept.
#'@noRd
warn_pw_obs_intercept <- function(mv_spec, obs_formula) {
  trend_specs <- mv_spec$trend_specs
  if (is.null(trend_specs)) return()
  specs_list <- if (is_multivariate_trend_specs(trend_specs)) {
    trend_specs
  } else {
    list(trend_specs)
  }
  has_pw <- any(vapply(
    specs_list,
    function(s) identical(s$trend, "PW"),
    logical(1L)
  ))
  if (!has_pw) return()
  if (!has_obs_intercept(obs_formula)) return()
  warn_confound(
    "pw_obs_intercept",
    paste0(
      "Observation formula has an intercept while the trend is ",
      "PW. The PW trend's 'm_trend' parameter and the observation ",
      "intercept compete for the same constant offset. Consider ",
      "fitting with a no-intercept observation formula (e.g. ",
      "'y ~ -1' or 'y ~ 0 + ...'). The PW intercept is then ",
      "uniquely identified."
    )
  )
}


# Internal: one emission path for the confound notices.
#
# Each of these names a pairing the model still samples under, where
# the priors alone divide an effect between two terms. They are held
# back under `TESTTHAT` for the reason recorded in FINDINGS: a
# once-per-session notice asserted in a suite reports whatever was
# raised before it.
#'@noRd
warn_confound <- function(id, message) {
  if (isTRUE(identical(Sys.getenv("TESTTHAT"), "true"))) {
    return(invisible(NULL))
  }
  rlang::warn(
    message,
    .frequency = "once",
    .frequency_id = paste0("mvgam_", id)
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
  tt <- sdata$times_trend
  oi <- sdata$obs_trend_time
  os <- sdata$obs_trend_series
  if (is.null(n) || is.null(n_trend) || is.null(tt) ||
        is.null(oi) || is.null(os)) {
    return(NULL)
  }
  if (!identical(ncol(tt), as.integer(sdata$N_series_trend))) {
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
  idx <- tt[cbind(as.integer(oi), as.integer(os))]
  if (anyNA(idx)) {
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


# Internal: warn once when the two designs share a direction.
#
# A term written into both formulas moves the same fitted values from
# either side. The model still samples, and the priors alone set the
# split between the two coefficients. `y ~ 1` with
# `~ series + AR(p = 1)` is one such pairing and a reasonable model
# to write. This names the columns involved and continues.
#'@noRd
warn_confounded_design <- function(sdata) {
  m <- stacked_design_matrix(sdata)
  if (is.null(m) || ncol(m) < 2L) {
    return(invisible(NULL))
  }
  # A rank taken on raw columns follows their scales: the
  # observation design is centred and the trend design is not.
  norms <- sqrt(colSums(m^2))
  norms[norms == 0] <- 1
  q <- qr(sweep(m, 2L, norms, "/"))
  if (q$rank >= ncol(m)) {
    return(invisible(NULL))
  }
  dependent <- colnames(m)[q$pivot[seq.int(q$rank + 1L, ncol(m))]]
  warn_confound(
    "confounded_design",
    paste0(
      "The observation and trend designs share ",
      ncol(m) - q$rank, " direction",
      if (ncol(m) - q$rank > 1L) "s" else "", ". Stacked, they hold ",
      ncol(m), " columns of rank ", q$rank,
      ". The remaining columns carry ",
      paste(dependent, collapse = ", "),
      ". A coefficient on one side and its partner on the other move ",
      "the same fitted values, and the priors alone set the split ",
      "between them. Drop the repeated term from one formula, or set ",
      "a prior pinning the side that carries it."
    )
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
  if (isTRUE(identical(Sys.getenv("TESTTHAT"), "true"))) return()
  if (!threads_no_op_for_trend_brms_native(threads, family, mv_spec)) {
    return()
  }
  # This warning is raised on every fit. A batch script that re-fits
  # after a config change keeps seeing it each time its threads
  # request is dropped.
  insight::format_warning(
    paste0(
      "`threads_per_chain > 1` is ignored for brms-native ",
      "families combined with a `trend_formula`. mvgam's trend ",
      "injector cannot reach the linear predictor that brms places ",
      "inside `partial_log_lik_lpmf`. The model compiles and ",
      "samples serially. Closure-unit families (`occ()`, `nmix()`) ",
      "are unaffected and continue to thread. Multi-response ",
      "families (`diri()`, `mvn()`, `mvt()`, `multi()`, `categ()`) ",
      "also continue to thread."
    )
  )
}


# Internal: detect whether the RHS of an observation formula
# implies a fitted intercept term. Returns TRUE for `y ~ 1`,
# `y ~ x`, `y ~ s(x)`, etc. (anything that brms / mgcv would
# expand with an implicit intercept) and FALSE for explicit no-
# intercept forms `y ~ -1 + ...`, `y ~ 0 + ...`.
#'@noRd
has_obs_intercept <- function(formula) {
  if (is.null(formula) || !inherits(formula, "formula")) {
    return(FALSE)
  }
  attr(stats::terms(formula), "intercept") == 1L
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
  out <- generated$combined_components[[component]]
  if (is.null(out)) {
    stop(insight::format_error(c(
      cli::format_inline(
        "Stan generation missing {.field {component}} component."
      ),
      i = cli::format_inline(
        "The {.fn generate_combined_stancode_and_data} result is incomplete."
      )
    )))
  }
  out
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
  if (!is.list(standata)) {
    stop(insight::format_error(c(
      cli::format_inline(
        "Generated Stan data is not a list: {.cls {class(standata)}}."
      ),
      i = "Stan requires named list structure for data."
    )))
  }

  if (length(standata) == 0) {
    stop(insight::format_error(c(
      "Generated Stan data list is empty.",
      i = "No data components were successfully generated."
    )))
  }

  return(standata)
}
