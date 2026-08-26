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
#' @param sparse Whether to use sparse matrix representations
#' @param knots Optional knot locations for smooth terms
#' @param drop_unused_levels Whether to drop unused factor levels
#' @param backend Stan backend to use ("rstan" or "cmdstanr")
#' @param threads Number of threads for threading
#' @param normalize Whether to normalize design matrices
#' @param save_model Path to save compiled Stan model
#' @param stan_funs Additional Stan functions to include
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
                                                   sample_prior = "no", sparse = NULL,
                                                   knots = NULL, drop_unused_levels = TRUE,
                                                   backend = "rstan",
                                                   threads = getOption("mc.cores", 1),
                                                   normalize = TRUE, save_model = NULL,
                                                   stan_funs = NULL, silent = 1L,
                                                   stanvars = NULL, validate = TRUE,
                                                   trend_map = NULL,
                                                   loadings_prior = NULL, ...) {

  # Input validation
  checkmate::assert_class(formula, "mvgam_formula")
  checkmate::assert_data_frame(data, min.rows = 1)
  if (!is.null(knots)) checkmate::assert_list(knots, names = "named")
  if (!is.null(data2)) {
    checkmate::assert(
      checkmate::check_data_frame(data2),
      checkmate::check_list(data2, min.len = 1, names = "named"),
      .var.name = "data2"
    )
  }
  if (!is.null(sparse)) checkmate::assert_logical(sparse, len = 1)
  if (!is.null(save_model)) checkmate::assert_string(save_model)
  if (!is.null(stanvars)) checkmate::assert_class(stanvars, "stanvars")
  if (!is.null(prior)) checkmate::assert_class(prior, "brmsprior")
  checkmate::assert_choice(sample_prior, c("no", "yes", "only"))
  checkmate::assert_logical(drop_unused_levels, len = 1)
  checkmate::assert_choice(backend, c("rstan", "cmdstanr"))
  checkmate::assert_int(threads, lower = 1)
  checkmate::assert_logical(normalize, len = 1)
  checkmate::assert_int(silent, lower = 0, upper = 2)

  # Extract components from mvgam_formula
  obs_formula <- formula$formula
  trend_formula <- formula$trend_formula

  # Validate family parameter when not embedded in formula
  if (!has_embedded_families(obs_formula)) {
    if (is.character(family)) {
      family <- get(family, mode = "function")()
    }
    if (!inherits(family, "family")) {
      stop(insight::format_error(
        cli::format_inline(
          "The {.field family} parameter must be a family object or function name."
        )
      ))
    }
    # Block multi-category families that require 3D linear predictors
    validate_supported_family(family)
  }
  # Closure-unit families (nmix, future occ / royle_nichols /
  # poisson_poisson) carry per-data Stan stanvars that are built
  # at fit time from the user's observation data: unit indexing
  # arrays, per-unit upper truncation (K_max), per-unit max obs
  # (Y_max), and the family-specific lpdf function block. The
  # arrays change with the data, so the resolution is deferred
  # to here rather than baked into the family() constructor.
  if (is_closure_unit_family(family)) {
    # obs_formula may be either a plain `formula` or a brms
    # `brmsformula` carrying dpar sub-formulas in `$pforms`.
    if (inherits(obs_formula, "brmsformula")) {
      main_formula <- obs_formula$formula
      dpar_forms   <- obs_formula$pforms %||% list()
    } else {
      main_formula <- obs_formula
      dpar_forms   <- list()
    }
    response_var <- as.character(main_formula[[2L]])
    main_terms <- tryCatch(
      stats::terms(main_formula), error = function(e) NULL
    )
    has_obs_covs <- if (is.null(main_terms)) FALSE else
      length(attr(main_terms, "term.labels")) > 0L
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

  # Custom families (e.g. tweedie()) carry their own Stan function
  # block + data stanvars in attr(family, "mvgam_stanvars"). They
  # belong on the observation submodel only; the trend submodel
  # uses gaussian() and reusing them there would duplicate the
  # function block + the M data int and explode the final
  # c.stanvars merge.
  obs_stanvars <- attach_family_stanvars(stanvars, family)
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

  # Wrapper-layer `n_lv` ceiling gate: shared by `mvgam()` and
  # `jsdgam()` so the iid vs MGP ceiling decision lives in one
  # place. Runs after `attach_loadings_prior_spec()` so the
  # `mv_spec$trend_specs$loadings_prior` carries the resolved
  # `column_shrinkage`; reads `n_lv` from the (possibly nested)
  # trend spec list.
  enforce_n_lv_ceiling_against_data(
    mv_spec$trend_specs, data, loadings_prior
  )

  # PW trends define their own intercept via `m_trend`. An
  # observation-side intercept competes with it for the same
  # constant offset, so soft-warn the user once per session.
  warn_pw_obs_intercept(mv_spec, obs_formula)

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
  if (is_com_binomial_family(family)) {
    assert_com_binomial_trials(obs_formula)
  }
  # Families whose brms fallback prior would be unsuitable carry an
  # mvgam default; `family_default_priors()` is the one definition
  # `get_prior()` also reports. `merge_default_priors()` drops any
  # default the user named, so a user prior still wins.
  prior <- merge_default_priors(
    family_default_priors(family), prior, obs_formula, family
  )

  # Filter priors: only pass observation-related priors to observation setup
  obs_priors <- filter_obs_priors(prior)
  
  if (is.null(obs_setup <- setup_brms_lightweight(
    formula = obs_formula,
    data = data,
    family = family,
    prior = obs_priors,
    data2 = data2,
    sample_prior = sample_prior,
    sparse = sparse,
    knots = knots,
    drop_unused_levels = drop_unused_levels,
    backend = backend,
    threads = threads,
    normalize = normalize,
    save_model = save_model,
    stan_funs = stan_funs,
    stanvars = obs_stanvars,
    silent = silent,
    ...
  ))) {
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
    trend_priors <- filter_trend_priors(prior)
    
    # Extract response variables and time series structure for trend validation
    response_vars <- extract_response_names(obs_formula)
    
    # Extract time/series variables from trend specs following existing pattern
    if (is_multivariate_trend_specs(mv_spec$trend_specs)) {
      first_spec <- mv_spec$trend_specs[[1]]
      time_var <- first_spec$time_var %||% first_spec$time %||% "time"
      series_var <- first_spec$series_var %||% first_spec$series %||% "series"
    } else {
      time_var <- mv_spec$trend_specs$time_var %||% mv_spec$trend_specs$time %||% "time"
      series_var <- mv_spec$trend_specs$series_var %||% mv_spec$trend_specs$series %||% "series"
    }
    
    # Extract response variable names for mapping generation
    response_vars <- extract_response_names(obs_formula)
    
    # Consolidated trend processing - replaces dual path architecture
    components <- extract_and_validate_trend_components(
      data, mv_spec, response_vars, time_var, series_var, trend_formula
    )
    trend_data <- components$trend_data
    mv_spec <- components$enhanced_mv_spec  # Already has dimensions injected
    trend_metadata <- components$metadata
    
    if (is.null(trend_result <- setup_brms_lightweight(
      formula = mv_spec$base_formula,
      data = trend_data,  # Use reduced trend data
      family = gaussian(), # Trends are gaussian processes per architecture
      prior = remove_trend_suffix_from_priors(trend_priors, mv_spec$trend_specs, mv_spec$base_formula, trend_data),
      data2 = data2,
      sample_prior = sample_prior,
      sparse = sparse,
      knots = knots,
      drop_unused_levels = drop_unused_levels,
      backend = backend,
      threads = threads,
      normalize = normalize,
      save_model = save_model,
      stan_funs = stan_funs,
      stanvars = trend_stanvars_in,
      silent = silent,
      # Mark this as the trend invocation so
      # setup_brms_lightweight() skips the empty-obs-formula
      # placeholder injection (the injected pin assignment would
      # be orphaned by mvgam's downstream trend-stancode rewriter).
      is_trend_setup = TRUE,
      ...
    ))) {
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

  # Generate combined Stan code and data using existing infrastructure
  combined_components <- generate_combined_stancode_and_data(
    obs_setup = obs_setup,
    trend_setup = trend_setup,
    mv_spec = mv_spec,
    validate = validate,
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
  combined_components$stancode <- paste(polish_generated_stan_code(combined_components$stancode), collapse = "\n")
  
  # Return all components needed for mvgam object creation
  return(list(
    combined_components = combined_components,
    obs_setup = obs_setup,
    trend_setup = trend_setup,
    mv_spec = mv_spec,
    trend_metadata = if (exists("trend_metadata")) trend_metadata else NULL
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


# Internal: soft-warn when a PW trend coincides with an
# observation-side intercept. Both terms shift the linear
# predictor by a constant, so leaving both in the model creates
# a non-identifiable pair. Mirrors the Prophet convention of
# fitting with no obs intercept.
#'@noRd
warn_pw_obs_intercept <- function(mv_spec, obs_formula) {
  if (isTRUE(identical(Sys.getenv("TESTTHAT"), "true"))) return()
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
  rlang::warn(
    paste0(
      "Observation formula has an intercept while the trend is ",
      "PW. The PW trend's 'm_trend' parameter and the observation ",
      "intercept compete for the same constant offset. Consider ",
      "fitting with a no-intercept observation formula (e.g. ",
      "'y ~ -1' or 'y ~ 0 + ...') so the PW intercept is ",
      "uniquely identified."
    ),
    .frequency = "once",
    .frequency_id = "mvgam_pw_obs_intercept"
  )
}

# Internal: decide whether to suppress brms's `partial_log_lik_lpmf`
# emission for the obs-side `setup_brms_lightweight()` call. Two
# distinct cases need the gate:
#
#   1. **Closure-unit and multi-response families.** These ship their
#      own `partial_sum_<family>_lpmf` + inner `reduce_sum` call via
#      mvgam-side stanvars. brms's outer `partial_log_lik_lpmf` would
#      nest a second wrapper around `nmix_lpmf` / `occ_lpmf` /
#      simplex / mvn-style lpmfs, but the brms function signature
#      cannot see transformed-data args (`visit_idx`, `log_n_lookup`,
#      etc.), so Stan validation fails. The fit-time
#      `cpp_options$stan_threads = TRUE` still flows separately via
#      `mvgam_single()` / `.compile_model_cmdstanr()`, so the inner
#      `reduce_sum` still parallelises; we just stop brms from
#      double-wrapping.
#
#   2. **brms-native families combined with a `trend_formula`.** brms
#      moves both the `mu` declaration and every linpred assignment
#      into `partial_log_lik_lpmf` inside `functions {}`, and mvgam's
#      obs-side trend injector (`R/stan_assembly.R:1346-1411` for
#      `mu +=`, `:1571-1602` for `mu[n] = ...`) only searches
#      `model {}`, so it cannot find the assignment it needs to splice
#      the trend addition into. The combination compiles to a silent
#      serial fit or hard crashes.
#
# Case 1 is silent (mvgam's threading still works). Case 2 emits a
# one-time warning so the user knows their parallelism request is
# being ignored.
#'@noRd
suppress_brms_threading <- function(threads, family, mv_spec) {
  if (!is.numeric(threads) || !isTRUE(threads > 1)) return(FALSE)
  if (is_closure_unit_family(family)) return(TRUE)
  if (is_multi_response_family(family)) return(TRUE)
  if (!is.null(mv_spec) && isTRUE(mv_spec$has_trends)) return(TRUE)
  FALSE
}

#'@noRd
threads_no_op_for_trend_brms_native <- function(threads, family, mv_spec) {
  if (!is.numeric(threads) || !isTRUE(threads > 1)) return(FALSE)
  if (is.null(mv_spec) || !isTRUE(mv_spec$has_trends)) return(FALSE)
  if (is_closure_unit_family(family)) return(FALSE)
  if (is_multi_response_family(family)) return(FALSE)
  TRUE
}

#'@noRd
warn_threads_trend_brms_native <- function(threads, family, mv_spec) {
  if (isTRUE(identical(Sys.getenv("TESTTHAT"), "true"))) return()
  if (!threads_no_op_for_trend_brms_native(threads, family, mv_spec)) {
    return()
  }
  # Fire per fit rather than once per session so a batch script
  # that re-fits after a config change keeps seeing the warning
  # every time its threads request is dropped.
  rlang::warn(
    paste0(
      "`threads_per_chain > 1` is currently ignored for brms-native ",
      "families combined with a `trend_formula`. mvgam's trend ",
      "injector and brms's `partial_log_lik_lpmf` placement are not ",
      "yet compatible, so the model compiles and samples serially. ",
      "Closure-unit families (`occ()`, `nmix()`) and multi-response ",
      "families (`diri()`, `mvn()`, `mvt()`, `multinomial()`, ",
      "`categorical()`) are unaffected and continue to thread."
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
#' @param sparse Logical; should sparse matrix operations be used? Default is
#'   \code{NULL} (automatic selection).
#' @param knots Optional list of knot positions for spline terms.
#' @param drop_unused_levels Logical; should unused factor levels be dropped?
#'   Default is \code{TRUE}.
#' @param backend Character string specifying Stan backend. Options:
#'   \code{"rstan"} (default) or \code{"cmdstanr"}.
#' @param threads Number of threads to use for parallelization. Default is
#'   \code{getOption("mc.cores", 1)}.
#' @param normalize Logical; should data be normalized for efficient sampling?
#'   Default is \code{TRUE}.
#' @param save_model File path to save the compiled Stan model. If \code{NULL}
#'   (default), the model is not saved.
#' @param stan_funs Optional character string containing additional Stan functions.
#' @param silent Integer controlling verbosity. 0 = silent, 1 = some output,
#'   2 = verbose. Default is 1.
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
                                   sample_prior = "no", sparse = NULL,
                                   knots = NULL, drop_unused_levels = TRUE,
                                   backend = "rstan",
                                   threads = getOption("mc.cores", 1),
                                   normalize = TRUE, save_model = NULL,
                                   stan_funs = NULL, silent = 1L,
                                   validate = TRUE, ...) {

  # Generate all Stan components using shared function
  combined_components <- generate_stan_components_mvgam_formula(
    formula = object, data = data, family = family, prior = prior,
    data2 = data2, sample_prior = sample_prior, sparse = sparse,
    knots = knots, drop_unused_levels = drop_unused_levels,
    backend = backend, threads = threads, normalize = normalize,
    save_model = save_model, stan_funs = stan_funs, silent = silent,
    validate = validate,
    ...
  )

  # Validate and return stancode component
  if (is.null(combined_components$combined_components$stancode)) {
    stop(insight::format_error(c(
      cli::format_inline(
        "Stan code generation missing {.field stancode} component."
      ),
      i = cli::format_inline(
        "The {.fn generate_combined_stancode_and_data} result is incomplete."
      )
    )))
  }

  # Add mvgam-specific stancode class with brms compatibility
  stancode <- combined_components$combined_components$stancode
  
  class(stancode) <- c("mvgamstancode", "stancode", "character")
  return(stancode)
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
#' @param sparse Logical; should sparse matrix operations be used? Default is
#'   \code{NULL} (automatic selection).
#' @param knots Optional list of knot positions for spline terms.
#' @param drop_unused_levels Logical; should unused factor levels be dropped?
#'   Default is \code{TRUE}.
#' @param stanvars Optional \code{stanvars} object containing additional Stan
#'   variables, parameters, or functions.
#' @param threads Number of threads to use for parallelization. Default is
#'   \code{getOption("mc.cores", 1)}.
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
                                   sample_prior = "no", sparse = NULL,
                                   knots = NULL, drop_unused_levels = TRUE,
                                   stanvars = NULL, threads = getOption("mc.cores", 1),
                                   ...) {

  # Generate all Stan components using shared function
  combined_components <- generate_stan_components_mvgam_formula(
    formula = object, data = data, family = family, prior = prior,
    data2 = data2, sample_prior = sample_prior, sparse = sparse,
    knots = knots, drop_unused_levels = drop_unused_levels,
    stanvars = stanvars, threads = threads, ...
  )

  # Validate and return standata component
  if (is.null(combined_components$combined_components$standata)) {
    stop(insight::format_error(c(
      cli::format_inline(
        "Stan data generation missing {.field standata} component."
      ),
      i = cli::format_inline(
        "The {.fn generate_combined_stancode_and_data} result is incomplete."
      )
    )))
  }

  # Validate Stan data structure follows brms conventions
  standata <- combined_components$combined_components$standata
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
