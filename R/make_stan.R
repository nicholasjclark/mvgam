#' Write an assembled Stan program to disk
#'
#' `save_model` names a file path, used as given. The parent directory
#' has to exist. mvgam writes a Stan file only where the user asks.
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
    )), call. = FALSE)
  }
  writeLines(stancode, con = path)
  invisible(path)
}

#' The Stan program, data and setups of an `mvgam_formula`
#'
#' The one code generator for `mvgam()`, `stancode()` and `standata()`.
#' It resolves the families and the trend and sets up the observation
#' and trend models through brms. It then assembles, polishes and
#' validates the program. The three callers compile and report one
#' program.
#'
#' @param formula An `mvgam_formula`
#' @param data Data frame of the response and predictors
#' @param family Observation family
#' @param prior A `brmsprior`, or NULL
#' @param data2 Further data brms takes, or NULL
#' @param sample_prior,knots,drop_unused_levels,normalize brms code
#'   generation options, gathered by `mvgam_codegen_options()`
#' @param backend "rstan" or "cmdstanr"
#' @param threads Threads per chain: a count, a `brms::threading()`
#'   object or `NULL` for one
#' @param save_model Path to write the assembled program to, or NULL
#' @param silent Verbosity, 0 to 2
#' @param stanvars User stanvars, or NULL
#' @param validate Whether to parse the polished program
#' @param trend_map,loadings_prior Factor loadings structure and prior,
#'   from `mvgam()`
#' @param ... Arguments for brms
#'
#' @return List of `combined_components` (the `stancode` and
#'   `standata`), `obs_setup`, `trend_setup` (NULL without a trend),
#'   `mv_spec` and `trend_metadata`
#' @noRd
build_stan_components <- function(formula, data, family = gaussian(),
                                 prior = NULL, data2 = NULL,
                                 sample_prior = "no",
                                 knots = NULL, drop_unused_levels = TRUE,
                                 backend = "rstan",
                                 threads = NULL,
                                 normalize = TRUE, save_model = NULL,
                                 silent = 1L,
                                 stanvars = NULL, validate = TRUE,
                                 trend_map = NULL,
                                 loadings_prior = NULL, ...) {

  checkmate::assert_class(formula, "mvgam_formula")
  checkmate::assert_data_frame(data, min.rows = 1)
  if (!is.null(data2)) {
    checkmate::assert(
      checkmate::check_data_frame(data2),
      checkmate::check_list(data2, min.len = 1, names = "named"),
      .var.name = "data2"
    )
  }
  checkmate::assert_string(save_model, null.ok = TRUE)
  checkmate::assert_class(stanvars, "stanvars", null.ok = TRUE)
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE)
  checkmate::assert_choice(backend, c("rstan", "cmdstanr"))
  threads <- thread_count(threads)
  checkmate::assert_int(silent, lower = 0, upper = 2)
  # `stancode()` and `standata()` reach here without passing through
  # `mvgam()`. The mock brms fit below takes its verbosity from the
  # option.
  local_mvgam_silent(silent)

  # One list carries the brms code-generation options to the mock fit
  # and to every later regeneration. The program, its data and the
  # prior table then share one set of bases and factor levels.
  # `mvgam_codegen_options()` validates its own arguments.
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

  # Closure-unit families (`nmix()` and its variants, `occ()` and the
  # multi-response families) build their stanvars from the data. The
  # stanvars hold the unit index arrays, the truncation `K_max` of each
  # unit, its largest count `Y_max` and the family's lpdf. A
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
      time_var           = axis_names$time_var,
      dpar_forms         = dpar_forms
    )
  }

  # Each response's family, taken after `prepare_closure_unit_family()`
  # has attached its data.
  families <- formula_families(obs_formula, family)

  # A custom family such as `tweedie()` carries its function block and
  # data as stanvars in `attr(family, "mvgam_stanvars")`. They belong
  # to the observation model. The gaussian trend model takes the
  # user's stanvars alone. A second copy would declare its functions
  # and data twice.
  obs_stanvars <- attach_family_stanvars(stanvars, families)
  trend_stanvars_in <- stanvars

  mv_spec$trend_specs <- prepare_trend_specs(
    mv_spec$trend_specs, trend_map, loadings_prior, data, data2
  )

  # `n_lv` belongs on the trend constructor inside `trend_formula`.
  # Given to `mvgam()` it would land in `...`, which brms and Stan
  # receive. Neither uses it.
  refuse_top_level_n_lv(list(...)$n_lv)

  # brms writes its `partial_log_lik_lpmf` only for `threads > 1`.
  # `brms_threading_case()` names the cases where that function breaks
  # the program. The compiler's thread option is set apart from this.
  # mvgam's own `reduce_sum` calls still run in parallel.
  threading_case <- brms_threading_case(threads, family, mv_spec)
  if (identical(threading_case, "trend_native")) {
    insight::format_warning(c(
      paste0("'threads' is not supported for a brms family with a ",
             "'trend_formula'."),
      x = "brms threads a likelihood that the latent trend has to join.",
      i = "The model samples serially.",
      i = "Closure-unit and multi-response families thread as requested."
    ))
  }
  if (!identical(threading_case, "none")) {
    threads <- 1L
  }

  # Simplex multi-response families (`diri`, `multi`, `categ`) hold
  # the loadings `Z` to sum to zero with Stan's `sum_to_zero_vector`,
  # which needs Stan 2.36.
  if (is_simplex_response_family(family)) {
    assert_stan_version(
      backend, "2.36.0",
      feature = "'sum_to_zero_vector[K]' for simplex families"
    )
  }
  # `com_binomial()` takes its denominator from its own response's
  # `trials()` term.
  forms <- response_formulas(obs_formula)
  for (key in names(families)) {
    if (is_com_binomial_family(families[[key]])) {
      assert_com_binomial_trials(forms[[key]])
    }
    assert_modelled_dpar_links(forms[[key]], families[[key]])
  }
  # Families whose brms default prior is unsuitable carry an mvgam
  # default. `response_default_priors()` defines them for `get_prior()`
  # as well. `merge_default_priors()` drops any default the user named.
  prior <- merge_default_priors(
    response_default_priors(obs_formula, family), prior
  )
  obs_priors <- filter_priors_by_side(prior, "obs")

  # The settings both submodels are built under. The two calls below
  # differ only in their formula, data, family, priors and stanvars. A
  # setting given to one call alone would build the observation and
  # trend halves under different settings without a message.
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

    # The axes are resolved here once.
    # `extract_and_validate_trend_components()` stores the dimensions
    # they give on the spec.
    components <- extract_and_validate_trend_components(
      data, mv_spec, obs_formula, family = family
    )
    trend_data <- components$trend_data
    mv_spec <- components$enhanced_mv_spec
    trend_metadata <- components$metadata

    do.call(setup_brms_lightweight, c(
      list(
        formula = mv_spec$base_formula,
        data = trend_data,
        # The trend program is a gaussian model on a placeholder
        # response
        family = gaussian(),
        prior = remove_trend_suffix_from_priors(
          trend_priors, mv_spec$trend_specs, mv_spec$base_formula,
          trend_data, codegen
        ),
        stanvars = trend_stanvars_in,
        # The trend program takes no placeholder for an empty
        # observation formula. Its pin assignment would outlive the
        # rename of the trend program's statements.
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
      data, "time", "series", obs_formula
    )
  }

  combined_components <- generate_combined_stancode(
    obs_setup = obs_setup,
    trend_setup = trend_setup,
    trend_specs = mv_spec$trend_specs,
    prior = trend_priors
  )

  combined_components$stancode <- paste(
    polish_generated_stan_code(combined_components$stancode),
    collapse = "\n"
  )

  # Parse the polished program, the one compiled. Polishing moves
  # statements between blocks. A parse of the unpolished program misses
  # a statement moved away from the scope it needs. `backend` picks the
  # parser the user compiles with. Simplex families need Stan >= 2.36
  # through cmdstanr and fail under rstan's bundled parser.
  if (isTRUE(validate)) {
    validate_stan_code(
      combined_components$stancode,
      backend = backend,
      silent = 1
    )
  }

  # The assembled program, the one compiled
  if (!is.null(save_model)) {
    write_stan_program(combined_components$stancode, save_model)
  }

  # The two designs one likelihood sees together, checked once the
  # data is assembled.
  warn_confounded_design(combined_components$standata,
                         combined_components$stancode)

  list(
    combined_components = combined_components,
    obs_setup = obs_setup,
    trend_setup = trend_setup,
    mv_spec = mv_spec,
    trend_metadata = trend_metadata
  )
}

#' Generate the Stan components for an `mvgam_formula`
#'
#' The code-generation entry point of `mvgam()`, `stancode()` and
#' `standata()`. Delegates to `build_stan_components()`, which runs
#' brms's code generator three times: once for the lightweight
#' skeleton, once for the base Stan code and once for the base Stan
#' data. All three validate the same data frame. Each brms notice
#' about that data would otherwise reach the user three times for one
#' model.
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
#' `ZMVN()` gives the latent state no temporal structure. On a single
#' series its innovations and the observation residuals are then both
#' iid draws. The data cannot tell them apart. Only
#' `sigma_trend^2 + sigma^2` is identified. The priors set the split
#' between them. Two series break the tie: `Sigma_trend` then carries a
#' cross-series covariance the observation noise cannot produce.
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
#'
#' @inheritParams zmvn_scale_confounded
#' @return `NULL`, invisibly
#' @noRd
warn_zmvn_single_series <- function(mv_spec, family, n_series) {
  if (zmvn_scale_confounded(mv_spec, family, n_series)) {
    warn_once(
      c(
        "A 'ZMVN()' trend on one series shares its scale with 'sigma'.",
        x = "The data identify only the sum of the two variances.",
        x = "The priors set the split between 'sigma_trend' and 'sigma'.",
        i = paste0("Add series, or use a trend with temporal structure ",
                   "such as 'AR()'."),
        i = "Or set a prior that pins one of the two scales."
      ),
      "mvgam_zmvn_single_series"
    )
  }
  invisible(NULL)
}

#' The observation and trend designs on one row per observation
#'
#' An observation's linear predictor takes its own design row with the
#' trend design row at `times_trend[obs_trend_time, obs_trend_series]`.
#' Stacked, the two give the columns one likelihood sees at once. The
#' rank of that matrix settles whether their coefficients are
#' separately identified.
#'
#' The parametric designs, the smooth bases and the Gaussian process
#' bases of both sides enter. Of the observation designs only those of
#' the mean enter. brms names a distributional parameter's designs
#' after it, such as `X_sigma`. Those join a different predictor. A
#' grouping effect stays out. brms passes it as a sparse expansion
#' against its own index. Partial pooling identifies it by shrinkage.
#' A trend formula holding only an intercept has no design matrix.
#' `trend_intercept` adds its column of ones.
#'
#' @param sdata The assembled Stan data
#' @param trend_intercept Whether the trend program declares
#'   `Intercept_trend`
#' @return A matrix, or NULL where no comparison exists: one design
#'   alone, a multivariate formula whose responses hold their own
#'   designs, a `by = lv_axis()` trend whose second axis counts factors
#' @noRd
stacked_design_matrix <- function(sdata, trend_intercept = FALSE) {
  checkmate::assert_list(sdata)
  checkmate::assert_flag(trend_intercept)
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
    !grepl("_trend$", nm) && !grepl("^[[:alpha:]]+_[[:alpha:]]", nm) &&
      nrow(sdata[[nm]]) == n
  }, nms)
  # One observation design named `X`. A multivariate formula keys its
  # designs by response. Each belongs to a separate likelihood.
  if (!("X" %in% obs_nms) || !(length(trend_nms) || trend_intercept)) {
    return(NULL)
  }
  # A block keeps the column names brms wrote. A block without them
  # has its columns numbered. Every column of the result is then
  # named in the notice.
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
  trend_part <- do.call(cbind, c(
    lapply(trend_nms, labelled, rows = idx),
    if (trend_intercept) {
      list(matrix(1, n, 1L, dimnames = list(NULL, "Intercept_trend")))
    }
  ))
  cbind(obs_part, trend_part)
}

#' The columns of a stacked design that repeat a direction of the others
#'
#' A column of zeros moves no fitted value and shares a direction with
#' nothing. The empty-observation placeholder is one, pinned at zero
#' by its prior. A rank taken on raw columns follows their scales. The
#' observation design is centred. The trend design is left as it is.
#' Each column is scaled to unit length first.
#'
#' @param m The matrix `stacked_design_matrix()` returns, or NULL
#' @return Character vector of column names
#' @noRd
confounded_columns <- function(m) {
  checkmate::assert_matrix(m, mode = "numeric", null.ok = TRUE)
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

#' Warn once when the two designs share a direction
#'
#' A term written into both formulas moves the same fitted values from
#' either side. The model still samples. The priors alone set the split
#' between the two coefficients. `y ~ 1` with `~ series + AR(p = 1)` is
#' one such pairing and a reasonable model to write. The warning names
#' the columns involved.
#'
#' @param sdata The assembled Stan data
#' @param stancode The assembled program, which declares any trend
#'   intercept
#' @return `NULL`, invisibly
#' @noRd
warn_confounded_design <- function(sdata, stancode) {
  checkmate::assert_list(sdata)
  checkmate::assert_string(stancode)
  trend_intercept <- "Intercept_trend" %in% brms_declared_params(stancode)
  dependent <- confounded_columns(
    stacked_design_matrix(sdata, trend_intercept)
  )
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

#' Which case makes brms's threaded likelihood unwanted
#'
#' With `threads > 1` brms wraps the likelihood in a
#' `partial_log_lik_lpmf` function. Two cases break under it.
#'
#' A closure-unit family, the multi-response ones among them, writes
#' its own `partial_sum_<family>_lpmf` and `reduce_sum` call. A brms wrapper
#' around them cannot see the transformed data they take, such as
#' `visit_idx`. The program then fails to parse. These families still
#' run their own `reduce_sum` in parallel.
#'
#' A brms family with a `trend_formula` has its `mu` built inside the
#' wrapper in the functions block. mvgam adds the trend to `mu` in the
#' model block, where that statement is absent.
#'
#' @param threads Threads per chain
#' @param family The observation family
#' @param mv_spec The model specification, or NULL
#' @return "none", "closure_unit" or "trend_native"
#' @noRd
brms_threading_case <- function(threads, family, mv_spec) {
  checkmate::assert_list(mv_spec, null.ok = TRUE)
  if (!is.numeric(threads) || !isTRUE(threads > 1)) {
    return("none")
  }
  if (uses_closure_unit_layout(family)) {
    return("closure_unit")
  }
  if (isTRUE(mv_spec$has_trends)) "trend_native" else "none"
}


#' Stan code and data of an mvgam model
#'
#' The Stan program and data list of an mvgam model. For an
#' [mvgam_formula()] they are generated without fitting, as `mvgam()`
#' would build them from the same arguments. For an `mvgam` model they
#' are the ones the model was built with. Pass the two to \pkg{rstan}
#' or \pkg{cmdstanr} to run the model outside mvgam. The program
#' shows how the trend enters the linear predictor.
#'
#' @inheritParams mvgam
#' @param object An [mvgam_formula()], or an `mvgam` model fitted or
#'   built with `run_model = FALSE`.
#' @param data A data frame holding every variable the formulas use.
#' @param prior A `brmsprior` of priors to set, from [brms::prior()].
#'   [get_prior.mvgam_formula()] lists the classes.
#' @param data2 A named list of objects the model uses that are not
#'   variables in `data`, as in [brms::brm()].
#' @param sample_prior `"no"` (the default), `"yes"` or `"only"`, as in
#'   [brms::brm()].
#' @param knots Optional named list of knot positions for spline terms,
#'   used by the observation and trend formulas alike.
#' @param drop_unused_levels Logical. `TRUE` (the default) drops factor
#'   levels without observations.
#' @param backend `"rstan"` (the default) or `"cmdstanr"`.
#' @param threads Number of threads within each chain. Defaults to 1.
#' @param normalize Logical. `TRUE` (the default) keeps the normalising
#'   constants of the sampling statements.
#' @param save_model File path to write the Stan program to, or `NULL`
#'   (the default) to write nothing.
#' @param silent Integer verbosity, as in [brms::brm()]. 0 prints
#'   Stan's exceptions and progress, 1 (the default) prints progress
#'   and 2 prints nothing.
#' @param stanvars Optional [brms::stanvar()] object of Stan code to
#'   add to the program, merged with the code mvgam writes for the
#'   trend.
#' @param validate Logical. `TRUE` (the default) checks the program
#'   with the Stan parser.
#' @param ... For an [mvgam_formula()], other arguments of [mvgam()].
#'   A name [mvgam()] does not take is refused. For an `mvgam` model,
#'   unused. Anything passed is refused.
#'
#' @return `stancode()` returns the Stan program as a character string
#'   of class `mvgamstancode`, which prints line by line. `standata()`
#'   returns the named list of Stan data.
#'
#' @examples
#' set.seed(1)
#' dat <- sim_mvgam(family = poisson(), n_series = 2L,
#'                  n_timepoints = 40L, trend_model = AR())$data_train
#' mf <- mvgam_formula(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1))
#'
#' # The program of the model, before any fitting. The trend's
#' # parameters carry a `_trend` suffix.
#' code <- stancode(mf, data = dat, family = poisson())
#' code
#'
#' # The data passed to that program: 60 observations of 2 series
#' # over 30 time points
#' sdata <- standata(mf, data = dat, family = poisson())
#' sdata[c("N", "N_series_trend", "N_time_trend")]
#'
#' # Next, fit the model with mvgam(), which compiles this program
#'
#' @seealso [mvgam()], [mvgam_formula()], [get_prior.mvgam_formula()].
#'   [methods_md()] writes the same model as equations for a methods
#'   section.
#' @family model fitting
#' @export
stancode.mvgam_formula <- function(object, data, family = gaussian(),
                                   prior = NULL, data2 = NULL,
                                   sample_prior = "no",
                                   knots = NULL, drop_unused_levels = TRUE,
                                   backend = "rstan",
                                   threads = 1L,
                                   normalize = TRUE, save_model = NULL,
                                   silent = 1L, stanvars = NULL,
                                   validate = TRUE, ...) {
  reject_removed_args(list(...), fn = "stancode")
  refuse_unread_dots(list(...), mvgam_dots_args(), "stancode")
  stancode <- mvgam_formula_component(
    "stancode", object, data, family, prior, data2, sample_prior,
    knots, drop_unused_levels, backend, threads, normalize,
    save_model, silent, stanvars, validate, ...
  )
  as_mvgam_stancode(stancode)
}


#' One component of the program an `mvgam_formula` describes
#'
#' `stancode()` and `standata()` take different components of one
#' generated object. Both pass the generator the same arguments in the
#' same order.
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
#' \pkg{brms}. A `trend_formula` in the same call, or a family mvgam
#' builds such as [tweedie()] or [nmix()], names an mvgam model. These
#' methods then build the \code{\link{mvgam_formula}} that call
#' describes and generate its program.
#'
#' @param object A `formula`, or a `brmsformula` or `mvbrmsformula`
#'   (both of class `bform`).
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
  if (!names_mvgam_model(object, dots)) {
    return(NextMethod())
  }
  route_to_mvgam_formula(object, dots, stancode)
}

#' @rdname trend_formula_generation
#' @export
stancode.bform <- stancode.formula

#' @rdname trend_formula_generation
#' @export
standata.formula <- function(object, ...) {
  dots <- list(...)
  if (!names_mvgam_model(object, dots)) {
    return(NextMethod())
  }
  route_to_mvgam_formula(object, dots, standata)
}

#' @rdname trend_formula_generation
#' @export
standata.bform <- standata.formula

#' @rdname stancode.mvgam_formula
#' @export
standata.mvgam_formula <- function(object, data, family = gaussian(),
                                   prior = NULL, data2 = NULL,
                                   sample_prior = "no",
                                   knots = NULL, drop_unused_levels = TRUE,
                                   backend = "rstan",
                                   threads = 1L,
                                   normalize = TRUE, save_model = NULL,
                                   silent = 1L, stanvars = NULL,
                                   validate = TRUE, ...) {
  reject_removed_args(list(...), fn = "standata")
  refuse_unread_dots(list(...), mvgam_dots_args(), "standata")
  standata <- mvgam_formula_component(
    "standata", object, data, family, prior, data2, sample_prior,
    knots, drop_unused_levels, backend, threads, normalize,
    save_model, silent, stanvars, validate, ...
  )

  if (!is.list(standata) || length(standata) == 0L) {
    stop_mvgam_fault(
      "The generated Stan data must be a non-empty list.",
      paste0("Got class '", class(standata)[1L], "' of length ",
             length(standata), ".")
    )
  }

  standata
}


#' @rdname stancode.mvgam_formula
#' @export
stancode.mvgam <- function(object, ...) {
  checkmate::assert_class(object, "mvgam")
  rlang::check_dots_empty()
  as_mvgam_stancode(object$stancode)
}


#' @rdname stancode.mvgam_formula
#' @export
standata.mvgam <- function(object, ...) {
  checkmate::assert_class(object, "mvgam")
  rlang::check_dots_empty()
  object$standata
}


#' Class a Stan program for printing
#'
#' @param code Character string of Stan code
#' @return `code` with class `c("mvgamstancode", "stancode",
#'   "character")`
#' @noRd
as_mvgam_stancode <- function(code) {
  checkmate::assert_string(code)
  structure(code, class = c("mvgamstancode", "stancode", "character"))
}
