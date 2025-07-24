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

#' Trend type registry for extensible dispatch
#'
#' Central registry of all available trend types for formula parsing and
#'   validation. New trend types are automatically included when registered.
#'
#' @return Character vector of trend type names
#' @noRd
mvgam_trend_registry <- function() {
  # Built-in trend types
  builtin_trends <- c("RW", "AR", "VAR", "GP", "CAR", "PW", "ZMVN")
  
  # Custom trend types (can be extended by users)
  custom_trends <- names(.mvgam_custom_trends)
  
  return(c(builtin_trends, custom_trends))
}

#' Get available trend type choices
#'
#' Returns a character vector of available trend types from the registry.
#'
#' @return Character vector of trend type names
#' @noRd
mvgam_trend_choices <- function() {
  mvgam_trend_registry()
}

#' Generate trend constructor pattern for formula parsing
#'
#' Creates a regex pattern that matches all registered trend constructors.
#'   This pattern is used by formula parsing functions to identify trend terms.
#'
#' @return Character string containing regex pattern
#' @noRd
mvgam_trend_pattern <- function() {
  trend_types <- mvgam_trend_registry()
  paste0("\\b(", paste(trend_types, collapse = "|"), ")\\s*\\([^)]*\\)")
}

#' Environment to store custom trend types
#'
#' Internal environment for storing user-registered custom trend types.
#' @noRd
.mvgam_custom_trends <- new.env(parent = emptyenv())

#' Register a custom trend type
#'
#' Adds a custom trend type to the registry, making it available for
#'   formula parsing and dispatch.
#'
#' @param trend_name Character string naming the trend type
#' @param constructor_fun Function that creates the trend object
#'
#' @return Invisible NULL
#' @noRd
register_custom_trend <- function(trend_name, constructor_fun) {
  checkmate::assert_string(trend_name, min.chars = 1)
  checkmate::assert_function(constructor_fun)
  
  # Store in custom trends registry
  .mvgam_custom_trends[[trend_name]] <- constructor_fun
  
  invisible(NULL)
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

#' Validate grouping arguments for trend constructors
#'
#' Validates and processes gr and subgr arguments with consistent error handling.
#'   Handles the logic for hierarchical groupings and series creation.
#'
#' @param gr Grouping variable (as character from deparse0)
#' @param subgr Subgrouping variable (as character from deparse0)
#'
#' @return List with validated gr and subgr values
#' @noRd
validate_grouping_arguments <- function(gr, subgr) {
  
  # Set default subgr if no grouping specified
  if (gr == 'NA') {
    subgr <- 'series'
  }
  
  # Validate hierarchical grouping requirements
  if (gr != 'NA') {
    if (subgr == 'NA') {
      insight::format_error(
        'Hierarchical grouping requires subgrouping specification.',
        'You specified {.field gr = "{gr}"} but {.field subgr = NA}.',
        'Please provide a valid subgrouping variable.'
      )
    } else if (subgr == 'series') {
      insight::format_error(
        'Invalid subgrouping for hierarchical models.',
        'You cannot use {.field subgr = "series"} with {.field gr = "{gr}"}.',
        'The series variable is created internally from gr and subgr.'
      )
    }
  }
  
  return(list(gr = gr, subgr = subgr))
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

#' Find trend constructor terms in formula
#'
#' Extracts trend constructor function calls from formula terms using the
#'   centralized registry pattern.
#'
#' @param x Formula, terms object, or character vector
#'
#' @return Character vector of trend constructor terms
#' @noRd
find_trend_terms <- function(x) {
  if (is.character(x)) {
    terms_char <- x
  } else {
    terms_char <- attr(terms(x), "term.labels")
  }
  
  # Use centralized pattern from registry
  trend_pattern <- mvgam_trend_pattern()
  
  # Find all matching terms
  trend_matches <- character(0)
  for (term in terms_char) {
    matches <- regmatches(term, gregexpr(trend_pattern, term))[[1]]
    if (length(matches) > 0) {
      trend_matches <- c(trend_matches, matches)
    }
  }
  
  return(unique(trend_matches))
}

#' Extract regular terms from formula
#'
#' Removes trend constructor calls from formula terms, leaving only regular
#'   predictors.
#'
#' @param formula_terms Character vector of term labels
#'
#' @return Character vector of regular predictor terms
#' @noRd
extract_regular_terms <- function(formula_terms) {
  
  # Use centralized pattern from registry
  trend_pattern <- mvgam_trend_pattern()
  
  regular_terms <- character(0)
  
  for (term in formula_terms) {
    # Remove trend constructor calls from the term
    cleaned_term <- gsub(trend_pattern, "", term)
    
    # Clean up extra spaces and operators
    cleaned_term <- gsub("\\s+\\+\\s+", " + ", cleaned_term)
    cleaned_term <- gsub("^\\s*\\+\\s*|\\s*\\+\\s*$", "", cleaned_term)
    cleaned_term <- gsub("\\s+", " ", cleaned_term)
    cleaned_term <- trimws(cleaned_term)
    
    # Only keep non-empty terms
    if (nzchar(cleaned_term) && cleaned_term != "+") {
      regular_terms <- c(regular_terms, cleaned_term)
    }
  }
  
  return(unique(regular_terms))
}

#' Parse trend formula
#'
#' Extracts trend model specifications from a formula and separates them
#'   from regular predictors.
#'
#' @param trend_formula A formula object containing trend specifications
#' @param data The data frame for validation (optional)
#'
#' @return List containing parsed formula components
#' @noRd
parse_trend_formula <- function(trend_formula, data = NULL) {
  
  # Input validation
  checkmate::assert_class(trend_formula, "formula")
  
  if (attr(terms(trend_formula), "response") > 0) {
    insight::format_error(
      "Response variable not allowed in trend formula.",
      "Trend formulas should only contain predictors.",
      "Remove the response variable from {.field trend_formula}."
    )
  }
  
  # Get formula terms
  formula_terms <- attr(terms(trend_formula), "term.labels")
  
  if (length(formula_terms) == 0) {
    insight::format_error(
      "Empty trend formula provided.",
      "The {.field trend_formula} must contain at least one term.",
      "Example: {.code ~ RW()} or {.code ~ s(time) + AR()}"
    )
  }
  
  # Extract trend constructor terms using registry
  trend_terms <- find_trend_terms(formula_terms)
  regular_terms <- extract_regular_terms(formula_terms)
  
  # Validate we have at least one trend model
  if (length(trend_terms) == 0) {
    available_trends <- paste(mvgam_trend_choices(), collapse = "(), ")
    insight::format_error(
      "No trend model specified in trend_formula.",
      "At least one trend constructor is required.",
      "Available: {.field {available_trends}()}."
    )
  }
  
  # Parse trend constructor calls
  trend_components <- vector("list", length(trend_terms))
  names(trend_components) <- paste0("trend", seq_along(trend_terms))
  
  for (i in seq_along(trend_terms)) {
    trend_components[[i]] <- eval_trend_constructor(trend_terms[i])
  }
  
  # Create base formula without trend constructors
  base_formula <- if (length(regular_terms) > 0) {
    reformulate(regular_terms, response = NULL)
  } else {
    ~ 1  # Intercept only
  }
  
  # Determine primary trend model
  trend_model <- if (length(trend_components) == 1) {
    trend_components[[1]]
  } else {
    NULL  # Multiple trends handled separately
  }
  
  return(list(
    base_formula = base_formula,
    trend_components = trend_components,
    trend_model = trend_model,
    trend_terms = trend_terms,
    regular_terms = regular_terms,
    original_formula = trend_formula
  ))
}

#' Evaluate trend constructor from string
#'
#' Safely evaluates a trend constructor call string.
#'
#' @param trend_call Character string containing the trend constructor call
#'
#' @return A validated mvgam_trend object
#' @noRd
eval_trend_constructor <- function(trend_call) {
  
  tryCatch({
    # Parse and evaluate the expression
    expr <- str2expression(trend_call)[[1]]
    trend_obj <- eval(expr, envir = parent.frame(2))
    
    # Validate result
    if (!is.mvgam_trend(trend_obj)) {
      insight::format_error(
        "Invalid trend constructor result.",
        "Expression {.code {trend_call}} did not produce a valid trend object.",
        "Check that you're using a supported trend constructor."
      )
    }
    
    return(trend_obj)
    
  }, error = function(e) {
    insight::format_error(
      "Failed to evaluate trend constructor.",
      "Error in {.code {trend_call}}: {e$message}",
      "Check the syntax and parameters of your trend constructor call."
    )
  })
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