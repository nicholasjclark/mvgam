#' Trend Type Dispatcher System for mvgam
#'
#' This file implements the trend dispatcher system with extensible trend type 
#'   management and comprehensive validation.
#'
#' @author Nicholas J Clark

#' Validate trend objects
#'
#' Validates trend objects using checkmate, insight, and rlang patterns for 
#'   consistent error handling across the package.
#'
#' @param trend_obj A trend object to validate
#' @param ... Additional arguments (not currently used)
#'
#' @return The validated trend object
#' @noRd
validate.mvgam_trend <- function(trend_obj, ...) {
  
  # Checkmate parameter validation
  checkmate::assert_class(trend_obj, "mvgam_trend")
  checkmate::assert_string(trend_obj$trend, min.chars = 1)
  checkmate::assert_character(trend_obj$tpars, min.len = 1)
  checkmate::assert_string(trend_obj$forecast_fun, min.chars = 1)
  checkmate::assert_string(trend_obj$stancode_fun, min.chars = 1)
  checkmate::assert_list(trend_obj$bounds, min.len = 1)
  checkmate::assert_list(trend_obj$characteristics, min.len = 1)
  
  # Check function references exist (will be implemented during Week 9-12)
  # For now, just validate the names are provided
  if (is.null(trend_obj$forecast_fun) || trend_obj$forecast_fun == "") {
    insight::format_error(
      "Forecast function name missing.",
      "Trend objects must specify a {.field forecast_fun}."
    )
  }
  
  # Dynamic factor model constraints
  validate_dynamic_factor_constraints(trend_obj)
  
  return(trend_obj)
}

#' Validate dynamic factor model constraints
#'
#' Checks identifiability constraints for dynamic factor models (n_lv > 0).
#'   Uses insight for user-friendly error messages and rlang for session 
#'   warnings.
#'
#' @param trend_obj A trend object to validate
#'
#' @noRd
validate_dynamic_factor_constraints <- function(trend_obj) {
  
  n_lv <- trend_obj$n_lv
  
  # Check if this is a dynamic factor model
  if (!is.null(n_lv) && n_lv > 0) {
    
    # Validate n_lv parameter
    checkmate::assert_int(n_lv, lower = 1, upper = 20)
    
    # Constraint 1: No groupings allowed with dynamic factors
    if (trend_obj$gr != 'NA') {
      insight::format_error(
        "Dynamic factor models cannot use hierarchical groupings.",
        "You specified {.field n_lv = {n_lv}} and {.field gr = '{trend_obj$gr}'}.",
        "The factor structure provides the grouping mechanism.",
        "Please set {.field gr = 'NA'} or remove {.field n_lv}."
      )
    }
    
    if (trend_obj$subgr != 'series') {
      insight::format_error(
        "Dynamic factor models cannot use custom subgroupings.",
        "You specified {.field n_lv = {n_lv}} and {.field subgr = '{trend_obj$subgr}'}.",
        "The factor structure handles series relationships.",
        "Please use {.field subgr = 'series'} or remove {.field n_lv}."
      )
    }
    
    # Constraint 2: No moving average terms allowed
    if (trend_obj$ma) {
      insight::format_error(
        "Dynamic factor models cannot include moving average terms.",
        "You specified {.field n_lv = {n_lv}} and {.field ma = TRUE}.",
        "MA terms create identifiability issues with factor structures.",
        "Please set {.field ma = FALSE} or remove {.field n_lv}."
      )
    }
    
    # Constraint 3: Trend type must support factors
    if (!trend_obj$characteristics$supports_factors) {
      insight::format_error(
        "Trend type {.field {trend_obj$trend}} does not support dynamic factor models.",
        "You specified {.field n_lv = {n_lv}} with an incompatible trend type.",
        "Supported trend types: {.field RW}, {.field AR}, {.field VAR}."
      )
    }
    
    # One-time warning about variance constraints
    rlang::warn(
      insight::format_warning(
        "Dynamic factor model detected ({.field n_lv = {n_lv}}).",
        "Trend variance will be fixed for identifiability.",
        "Factor loadings provide the primary source of variation."
      ),
      .frequency = "once",
      .frequency_id = "mvgam_dynamic_factor_variance"
    )
  }
  
  # Additional constraint: Groupings require correlation  
  if (trend_obj$gr != 'NA' && !trend_obj$cor) {
    insight::format_error(
      "Hierarchical groupings require correlation structure.",
      "You specified {.field gr = '{trend_obj$gr}'} but {.field cor = FALSE}.",
      "Please set {.field cor = TRUE} when using groupings."
    )
  }
}

#' Get available trend type choices
#'
#' Returns a character vector of available built-in trend types.
#'
#' @return Character vector of trend type names
#' @noRd
mvgam_trend_choices <- function() {
  c("RW", "AR", "VAR", "GP", "CAR", "PW", "ZMVN")
}

#' Check if object is a mvgam trend
#'
#' Tests whether an object is a valid mvgam trend specification.
#'
#' @param x Object to test
#' @return Logical indicating if x is a mvgam trend
#' @noRd
is.mvgam_trend <- function(x) {
  inherits(x, "mvgam_trend")
}

#' Build trend label for display
#'
#' Creates human-readable labels for trend objects based on their parameters.
#'   Used for printing and formula display.
#'
#' @param type Base trend type name
#' @param cor Logical indicating correlation structure
#' @param ma Logical indicating moving average terms
#' @param gr Grouping variable
#' @param n_lv Number of latent variables (for dynamic factors)
#' @param p Order parameter (for AR/VAR models)
#'
#' @return Character string label
#' @noRd
build_trend_label <- function(type, cor = FALSE, ma = FALSE, gr = 'NA', 
                              n_lv = NULL, p = NULL) {
  
  # Start with base type
  label <- type
  
  # Add order if provided
  if (!is.null(p) && p > 1) {
    label <- paste0(label, p)
  }
  
  # Add modifiers
  modifiers <- character(0)
  
  if (!is.null(n_lv) && n_lv > 0) {
    modifiers <- c(modifiers, paste0("lv=", n_lv))
  }
  
  if (ma) {
    modifiers <- c(modifiers, "ma")
  }
  
  if (cor) {
    modifiers <- c(modifiers, "cor")
  }
  
  if (gr != 'NA') {
    modifiers <- c(modifiers, "hier")
  }
  
  # Combine with parentheses if modifiers exist
  if (length(modifiers) > 0) {
    label <- paste0(label, "(", paste(modifiers, collapse = ","), ")")
  }
  
  return(label)
}

#' Validate trend order parameter
#'
#' Standardized validation for trend order parameters (p). Provides consistent 
#'   error messages across trend types.
#'
#' @param p Order parameter to validate
#' @param max_order Maximum allowed order for this trend type
#' @param trend_type Name of trend type for error messages
#'
#' @noRd
validate_trend_order <- function(p, max_order, trend_type) {
  checkmate::assert_int(p, lower = 1, upper = max_order)
  
  if (p > max_order) {
    insight::format_error(
      "{.field {trend_type}} order too high.",
      "You specified {.field p = {p}} but maximum allowed is {.field {max_order}}.",
      "Please reduce the order or consider a different trend type."
    )
  }
}

#' Validate correlation requirements
#'
#' Checks and auto-corrects correlation settings for hierarchical models.
#'   Issues one-time warnings when auto-correction occurs.
#'
#' @param gr Grouping variable
#' @param cor Correlation setting
#'
#' @return Corrected correlation setting (logical)
#' @noRd
validate_correlation_requirements <- function(gr, cor) {
  if (gr != 'NA' && !cor) {
    rlang::warn(
      insight::format_warning(
        "Hierarchical grouping specified without correlation.",
        "Setting {.field cor = TRUE} automatically for {.field gr = '{gr}'}."
      ),
      .frequency = "once",
      .frequency_id = "mvgam_hierarchical_correlation"
    )
    return(TRUE)  # Force cor = TRUE
  }
  return(cor)
}

#' Create custom trend types
#'
#' Allows users to define custom trend specifications. This is the main 
#'   extension point for adding new trend types.
#'
#' @param trend Character string naming the trend type
#' @param tpars Character vector of trend-specific parameter names
#' @param forecast_fun Character string naming the forecasting function
#' @param stancode_fun Character string naming the Stan code generation function
#' @param standata_fun Character string naming the Stan data preparation function
#' @param bounds Named list of parameter bounds for prior specification
#' @param characteristics Named list of trend characteristics and capabilities
#' @param ... Additional parameters to store in the trend object
#'
#' @return A custom mvgam trend object
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a custom GARCH trend type
#' GARCH <- function(p = 1, q = 1) {
#'   custom_trend(
#'     trend = "GARCH",
#'     tpars = c("alpha", "beta", "sigma_trend"),
#'     forecast_fun = "forecast_garch_rcpp",
#'     stancode_fun = "generate_garch_stan",
#'     standata_fun = "prepare_garch_data",
#'     bounds = list(
#'       alpha = c(0, 1),
#'       beta = c(0, 1),
#'       sigma_trend = c(0, Inf)
#'     ),
#'     characteristics = list(
#'       supports_predictors = TRUE,
#'       supports_correlation = FALSE,
#'       supports_factors = FALSE,
#'       max_order = max(p, q),
#'       requires_sorting = TRUE
#'     ),
#'     p = p, q = q
#'   )
#' }
#' }
custom_trend <- function(trend, tpars, forecast_fun, stancode_fun, 
                         standata_fun = NULL, bounds = list(), 
                         characteristics = list(), ...) {
  
  # Input validation
  checkmate::assert_string(trend, min.chars = 1)
  checkmate::assert_character(tpars, min.len = 1)
  checkmate::assert_string(forecast_fun, min.chars = 1)
  checkmate::assert_string(stancode_fun, min.chars = 1)
  checkmate::assert_list(bounds)
  checkmate::assert_list(characteristics)
  
  # Build trend object
  trend_obj <- structure(list(
    trend = trend,
    label = trend,  # Can be updated by user
    tpars = tpars,
    monitor_pars = c(tpars, "trend"),
    extract_pars = tpars,
    forecast_fun = forecast_fun,
    stancode_fun = stancode_fun,
    standata_fun = standata_fun,
    bounds = bounds,
    characteristics = characteristics,
    ...
  ), class = c("mvgam_trend", "custom"))
  
  # Validate the custom trend
  validate(trend_obj)
  
  return(trend_obj)
}

#' Print method for mvgam trend objects
#'
#' Provides informative printing of trend specifications.
#'
#' @param x A mvgam_trend object
#' @param ... Additional arguments (not currently used)
#'
#' @export
print.mvgam_trend <- function(x, ...) {
  cat("mvgam trend specification:\n")
  cat("  Type:", x$trend, "\n")
  cat("  Label:", x$label, "\n")
  cat("  Parameters:", paste(x$tpars, collapse = ", "), "\n")
  
  if (!is.null(x$n_lv) && x$n_lv > 0) {
    cat("  Dynamic factors:", x$n_lv, "\n")
  }
  
  if (!is.null(x$cor) && x$cor) {
    cat("  Correlation: enabled\n")
  }
  
  if (!is.null(x$ma) && x$ma) {
    cat("  Moving average: enabled\n")
  }
  
  if (!is.null(x$gr) && x$gr != 'NA') {
    cat("  Hierarchical grouping:", x$gr, "\n")
  }
  
  invisible(x)
}