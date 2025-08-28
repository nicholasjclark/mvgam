# Internal shared function for both stancode and standata
# @noRd
generate_stan_components_mvgam_formula <- function(formula, data, family = gaussian(),
                                                   prior = NULL, data2 = NULL,
                                                   sample_prior = "no", sparse = NULL,
                                                   knots = NULL, drop_unused_levels = TRUE,
                                                   backend = "rstan", 
                                                   threads = getOption("mc.cores", 1),
                                                   normalize = TRUE, save_model = NULL,
                                                   stan_funs = NULL, silent = 1L, 
                                                   stanvars = NULL, ...) {
  
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
  }

  # Parse multivariate trends and validate
  if (is.null(mv_spec <- parse_multivariate_trends(obs_formula, trend_formula))) {
    insight::format_error(
      "Failed to parse trend formula specification.",
      "Check your {.arg trend_formula} syntax and constructor arguments."
    )
  }

  # Setup observation model using lightweight brms
  if (is.null(obs_setup <- setup_brms_lightweight(
    formula = obs_formula,
    data = data,
    family = family,
    prior = prior,
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

  # Setup trend model if trends are specified
  trend_setup <- if (mv_spec$has_trends) {
    if (is.null(trend_result <- setup_brms_lightweight(
      formula = mv_spec$base_formula,
      data = data,
      family = gaussian(), # Trends are gaussian processes per architecture
      prior = prior,
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

  # Validate time series structure and inject dimensions
  if (mv_spec$has_trends) {
    validation_result <- validate_time_series_for_trends(data, mv_spec$trend_specs)
    if (is.null(validation_result) || is.null(validation_result$dimensions)) {
      insight::format_error(
        "Time series validation failed for trend specification.",
        "Ensure your data has proper {.field time} and {.field series} structure."
      )
    }

    # Inject dimensions back into trend specifications
    if (is_multivariate_trend_specs(mv_spec$trend_specs)) {
      # Multivariate: inject dimensions into each response-specific trend spec
      for (response_name in names(mv_spec$trend_specs)) {
        mv_spec$trend_specs[[response_name]]$dimensions <- validation_result$dimensions
      }
    } else {
      # Univariate: inject dimensions directly into trend object
      mv_spec$trend_specs$dimensions <- validation_result$dimensions
    }
  }

  # Generate combined Stan code and data using existing infrastructure
  combined_components <- generate_combined_stancode_and_data(
    obs_setup = obs_setup,
    trend_setup = trend_setup,
    mv_spec = mv_spec
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

  return(combined_components)
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
#' @return A character string containing complete Stan model code with data,
#'   parameters, model, and generated quantities blocks for both observation
#'   and trend components.
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
                                   stan_funs = NULL, silent = 1L, ...) {

  # Generate all Stan components using shared function
  combined_components <- generate_stan_components_mvgam_formula(
    formula = object, data = data, family = family, prior = prior,
    data2 = data2, sample_prior = sample_prior, sparse = sparse,
    knots = knots, drop_unused_levels = drop_unused_levels,
    backend = backend, threads = threads, normalize = normalize,
    save_model = save_model, stan_funs = stan_funs, silent = silent,
    ...
  )

  # Validate and return stancode component
  if (is.null(combined_components$stancode)) {
    insight::format_error(
      "Stan code generation missing {.field stancode} component.",
      "The {.fn generate_combined_stancode_and_data} result is incomplete."
    )
  }

  # Add mvgam-specific stancode class with brms compatibility
  stancode <- combined_components$stancode
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
#'   and trend data components. Structure matches what would be passed to Stan
#'   during fitting.
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
  if (is.null(combined_components$standata)) {
    insight::format_error(
      "Stan data generation missing {.field standata} component.",
      "The {.fn generate_combined_stancode_and_data} result is incomplete."
    )
  }

  # Validate Stan data structure follows brms conventions
  standata <- combined_components$standata
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
