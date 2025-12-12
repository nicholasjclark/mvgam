#' Generate Complete Stan Components for mvgam Formula
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
#' This function implements the DRY (Don't Repeat Yourself) consolidation of Stan
#' code generation. It processes formulas, validates time series structure, sets up
#' brms models, generates stanvars, assembles Stan code, and applies polishing.
#' The polished Stan code is identical whether accessed via \code{mvgam()},
#' \code{stancode()}, or \code{standata()}.
#'
#' @noRd
generate_stan_components_mvgam_formula <- function(formula, data, family = gaussian(),
                                                   prior = NULL, data2 = NULL,
                                                   sample_prior = "no", sparse = NULL,
                                                   knots = NULL, drop_unused_levels = TRUE,
                                                   backend = "rstan", 
                                                   threads = getOption("mc.cores", 1),
                                                   normalize = TRUE, save_model = NULL,
                                                   stan_funs = NULL, silent = 1L, 
                                                   stanvars = NULL, validate = TRUE, ...) {
  
  # Input validation
  checkmate::assert_class(formula, "mvgam_formula")
  checkmate::assert_data_frame(data, min.rows = 1)
  if (!is.null(knots)) checkmate::assert_list(knots, names = "named")
  if (!is.null(data2)) {
    checkmate::assert(
      checkmate::check_data_frame(data2),
      checkmate::check_list(data2, types = "data.frame", min.len = 1),
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
      insight::format_error(
        "The {.field family} parameter must be a family object or function name."
      )
    }
    # Block multi-category families that require 3D linear predictors
    validate_supported_family(family)
  }

  # Parse multivariate trends and validate
  if (is.null(mv_spec <- parse_multivariate_trends(obs_formula, trend_formula))) {
    insight::format_error(
      "Failed to parse trend formula specification.",
      "Check your {.arg trend_formula} syntax and constructor arguments."
    )
  }

  # Setup observation model using lightweight brms
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
    stanvars = stanvars,
    silent = silent,
    ...
  ))) {
    insight::format_error(
      "Failed to setup observation model with brms.",
      "Check your {.arg formula} and {.arg data} compatibility."
    )
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
      stanvars = stanvars,
      silent = silent,
      ...
    ))) {
      insight::format_error(
        "Failed to setup trend model with brms.",
        "Check your {.arg trend_formula} and {.arg data} compatibility."
      )
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
    prior = trend_priors  # Pass unfiltered trend priors to mvgam functions
  )

  # Validate result structure with specific error locations
  if (is.null(combined_components)) {
    insight::format_error(
      "Stan component generation returned NULL result.",
      "This indicates a failure in {.fn generate_combined_stancode_and_data}."
    )
  }

  if (!is.list(combined_components)) {
    insight::format_error(
      "Stan component generation returned invalid type: {.cls {class(combined_components)}}.",
      "Expected list from {.fn generate_combined_stancode_and_data}."
    )
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

#' Generate Stan Model Code for mvgam Formula
#'
#' Generate complete Stan model code for an \code{mvgam_formula} object before
#' model fitting. This allows users to inspect the generated Stan code for
#' debugging, understanding, or modification purposes.
#'
#' @param object An \code{mvgam_formula} object created by \code{mvgam_formula()}.
#' @param data A data frame containing all variables referenced in the formula.
#' @param family A family object or character string specifying the response
#'   distribution. If \code{object} contains embedded family specifications,
#'   this parameter can be omitted.
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
#' @examples
#' \dontrun{
#' # Create mvgam_formula object
#' mf <- mvgam_formula(count ~ temperature, trend_formula = ~ AR(p = 1))
#'
#' # Generate Stan code
#' stan_code <- stancode(mf, data = ecology_data, family = poisson())
#' cat(stan_code)
#'
#' # With custom priors
#' custom_priors <- brms::prior("normal(0, 0.5)", class = "ar1_trend") +
#'                  brms::prior("exponential(2)", class = "sigma_trend")
#' stan_code_custom <- stancode(mf, data = ecology_data,
#'                              family = poisson(), prior = custom_priors)
#' }
#'
#' @seealso
#' \code{\link{mvgam_formula}}, \code{\link{get_prior.mvgam_formula}},
#' \code{\link{standata}}, \code{\link{mvgam}}
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
    insight::format_error(
      "Stan code generation missing {.field stancode} component.",
      "The {.fn generate_combined_stancode_and_data} result is incomplete."
    )
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
#' @param family A family object or character string specifying the response
#'   distribution. If \code{object} contains embedded family specifications,
#'   this parameter can be omitted.
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
#'   shared infrastructure as \code{mvgam()}, ensuring complete consistency.
#'
#' @details
#' This function uses the shared Stan data generation infrastructure
#' (\code{generate_stan_components_mvgam_formula()}) to ensure identical output
#' to what \code{mvgam()} uses internally. The returned data structure includes
#' all observation data, trend mappings, dimensions, and stanvars needed for
#' model compilation and fitting.
#'
#' @examples
#' \dontrun{
#' # Create mvgam_formula object
#' mf <- mvgam_formula(count ~ temperature, trend_formula = ~ AR(p = 1))
#'
#' # Generate Stan data
#' stan_data <- standata(mf, data = ecology_data, family = poisson())
#' str(stan_data)
#'
#' # With custom priors
#' custom_priors <- brms::prior("normal(0, 0.5)", class = "ar1_trend") +
#'                  brms::prior("exponential(2)", class = "sigma_trend")
#' stan_data_custom <- standata(mf, data = ecology_data,
#'                              family = poisson(), prior = custom_priors)
#' }
#'
#' @seealso
#' \code{\link{mvgam_formula}}, \code{\link{get_prior.mvgam_formula}},
#' \code{\link{stancode}}, \code{\link{mvgam}}
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
    insight::format_error(
      "Stan data generation missing {.field standata} component.",
      "The {.fn generate_combined_stancode_and_data} result is incomplete."
    )
  }

  # Validate Stan data structure follows brms conventions
  standata <- combined_components$combined_components$standata
  if (!is.list(standata)) {
    insight::format_error(
      "Generated Stan data is not a list: {.cls {class(standata)}}.",
      "Stan requires named list structure for data."
    )
  }

  if (length(standata) == 0) {
    insight::format_error(
      "Generated Stan data list is empty.",
      "No data components were successfully generated."
    )
  }

  return(standata)
}
