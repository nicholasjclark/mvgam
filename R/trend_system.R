#' Trend System Infrastructure for mvgam
#'
#' @description
#' Complete trend infrastructure including registry management, validation,
#' formula parsing, and trend constructor functions. This file consolidates
#' the core trend system components for mvgam-brms integration.
#'
#' @section Architecture:
#' The trend system follows a layered architecture:
#' - **Registry Layer**: Manages available trend types and their properties
#' - **Validation Layer**: Validates trend specifications and factor
#'   compatibility
#' - **Parsing Layer**: Parses trend formulas and dispatches to constructors
#' - **Constructor Layer**: Individual trend constructor functions
#'   (RW, AR, VAR, etc.)

# =============================================================================
# SECTION 1: TREND REGISTRY SYSTEM
# =============================================================================
# WHY: The registry system is essential for mvgam-brms integration because
# it enables dynamic dispatch to appropriate Stan code generators while
# maintaining type safety and factor model compatibility validation.
# Without centralized registration, the system cannot determine which
# trends support factor models or locate the correct Stan code generators.

# Global trend registry
trend_registry <- new.env(parent = emptyenv())
#' Register a Trend Type
#'
#' @description
#' Register a new trend type with the mvgam trend system.
#' Automatically handles factor model compatibility and validation.
#'
#' @param name Character string name of the trend type
#' @param supports_factors Logical indicating if trend supports factor models (n_lv parameter)
#' @param generator_func Function that generates Stan code for this trend type
#' @param incompatibility_reason Character string explaining why factor models aren't supported (if applicable)
#' @return Invisibly returns TRUE on successful registration
#' @export
register_trend_type <- function(name, supports_factors = FALSE, generator_func,
                               incompatibility_reason = NULL) {
  checkmate::assert_string(name, min.chars = 1)
  checkmate::assert_logical(supports_factors, len = 1)
  checkmate::assert_function(generator_func, args = c("trend_spec", "data_info"))

  if (!supports_factors && is.null(incompatibility_reason)) {
    incompatibility_reason <- get_default_incompatibility_reason(name)
  }

  trend_registry[[name]] <- list(
    supports_factors = supports_factors,
    generator = generator_func,
    incompatibility_reason = incompatibility_reason
  )

  invisible(TRUE)
}

#' Get Trend Type Information
#'
#' @description
#' Retrieve information about a registered trend type.
#'
#' @param name Character string name of the trend type
#' @return List containing trend type information
#' @noRd
get_trend_info <- function(name) {
  checkmate::assert_string(name, min.chars = 1)

  if (!exists(name, envir = trend_registry)) {
    available_trends <- ls(trend_registry)
    stop(insight::format_error(
      paste0("Unknown trend type: {.field ", name, "}"),
      paste0("Available types: {.val ", paste(available_trends, collapse = ", "), "}")
    ))
  }

  trend_registry[[name]]
}

#' List Available Trend Types
#'
#' @description
#' List all registered trend types and their factor model support.
#'
#' @return Data frame with trend types and their properties
#' @export
list_trend_types <- function() {
  trend_names <- ls(trend_registry)

  if (length(trend_names) == 0) {
    return(data.frame(
      trend_type = character(0),
      supports_factors = logical(0),
      incompatibility_reason = character(0)
    ))
  }

  trend_info <- lapply(trend_names, function(name) {
    info <- trend_registry[[name]]
    data.frame(
      trend_type = name,
      supports_factors = info$supports_factors,
      incompatibility_reason = info$incompatibility_reason %||% "",
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, trend_info)
}

#' Get Factor Compatible Trends
#'
#' @description
#' Get list of trend types that support factor models.
#'
#' @return Character vector of factor-compatible trend names
#' @noRd
get_factor_compatible_trends <- function() {
  trend_names <- ls(trend_registry)
  factor_compatible <- character()

  for (name in trend_names) {
    if (trend_registry[[name]]$supports_factors) {
      factor_compatible <- c(factor_compatible, name)
    }
  }

  factor_compatible
}

#' Get Default Incompatibility Reason
#'
#' @description
#' Generate default reason why a trend type doesn't support factor models.
#'
#' @param name Trend type name
#' @return Character string with incompatibility reason
#' @noRd
get_default_incompatibility_reason <- function(name) {
  default_reasons <- list(
    "PW" = "Piecewise trends require series-specific changepoint modeling",
    "PWlinear" = "Piecewise linear trends require series-specific changepoint modeling",
    "PWlogistic" = "Piecewise logistic trends require series-specific changepoint modeling",
    "CAR" = "Continuous-time AR requires series-specific irregular time intervals"
  )

  default_reasons[[name]] %||% "Series-specific dynamics not compatible with factor structure"
}

#' Register Core Trend Types
#'
#' @description
#' Register all core mvgam trend types. Called automatically during package load.
#'
#' @return Invisibly returns TRUE
#' @noRd
register_core_trends <- function() {
  # Factor-compatible trends (stationary dynamics work with factor structure)
  register_trend_type("AR", supports_factors = TRUE, generate_ar_trend_stanvars)
  register_trend_type("RW", supports_factors = TRUE, generate_rw_trend_stanvars)
  register_trend_type("VAR", supports_factors = TRUE, generate_var_trend_stanvars)
  register_trend_type("ZMVN", supports_factors = TRUE, generate_zmvn_trend_stanvars)

  # Factor-incompatible trends (series-specific dynamics)
  register_trend_type("CAR", supports_factors = FALSE, generate_car_trend_stanvars)
  register_trend_type("PW", supports_factors = FALSE, generate_pw_trend_stanvars)

  invisible(TRUE)
}

#' Register Custom Trend Type
#'
#' @description
#' User-facing function to register custom trend types.
#'
#' @param name Character string name for the custom trend type
#' @param supports_factors Logical indicating if the trend supports factor models
#' @param generator_func Function that generates Stan code for this trend type.
#'   Must accept arguments (trend_spec, data_info) and return list of stanvars.
#' @param incompatibility_reason Optional character string explaining why factor
#'   models aren't supported (if supports_factors = FALSE)
#'
#' @return Invisibly returns TRUE on successful registration
#' @export
#'
#' @examples
#' \dontrun{
#' # Register a custom trend type
#' my_trend_generator <- function(trend_spec, data_info) {
#'   # Implementation here
#'   list(stanvar(...))
#' }
#'
#' register_custom_trend("MyTrend",
#'                      supports_factors = TRUE,
#'                      generator_func = my_trend_generator)
#' }
register_custom_trend <- function(name, supports_factors = FALSE, generator_func,
                                 incompatibility_reason = NULL) {

  # Additional validation for user-facing function
  if (exists(name, envir = trend_registry)) {
    rlang::warn(
      c(paste0("Overwriting existing trend type: ", name),
        "i" = "This will replace the existing registration"),
      .frequency = "once",
      .frequency_id = paste0("trend_overwrite_", name)
    )
  }

  register_trend_type(name, supports_factors, generator_func, incompatibility_reason)
}

#' Check if Registry is Initialized
#'
#' @description
#' Check if the trend registry has been initialized with core trends.
#'
#' @return Logical indicating if registry is initialized
#' @noRd
is_registry_initialized <- function() {
  core_trends <- c("AR", "RW", "VAR", "ZMVN", "CAR", "PW")
  all(core_trends %in% ls(trend_registry))
}

#' Initialize Registry if Needed
#'
#' @description
#' Initialize the trend registry if it hasn't been done yet.
#' Called automatically by functions that need the registry.
#'
#' @return Invisibly returns TRUE
#' @noRd
ensure_registry_initialized <- function() {
  if (!is_registry_initialized()) {
    register_core_trends()
  }
  invisible(TRUE)
}

# =============================================================================
# SECTION 2: TREND PARAMETER SYSTEM (brms-inspired)
# =============================================================================
# WHY: Following brms design patterns for prior() objects, we create a flexible
# parameter specification system that allows easy combination with `+` operator
# and standardized conditional parameter handling.

#' Create trend parameter specifications
#'
#' Creates parameter specifications for trend constructors following brms design
#'   patterns. Parameters can be combined using the `+` operator and support
#'   conditional inclusion, bounds, monitoring flags, and labels.
#'
#' @param name Character string specifying the parameter name
#' @param bounds Numeric vector of length 2 specifying lower and upper bounds
#' @param monitor Logical indicating if parameter should be monitored post-fit
#' @param label Character string describing the parameter for documentation
#' @param condition Logical or expression that determines if parameter is included
#'
#' @return Object of class `trend_param` containing parameter specification
#' @export
#'
#' @examples
#' # Basic parameter
#' sigma_param <- trend_param("sigma", bounds = c(0, Inf), label = "innovation_sd")
#'
#' # Conditional parameter
#' theta_param <- trend_param("theta", bounds = c(-1, 1),
#'                           condition = ma, label = "ma_coefficient")
#'
#' # Combine parameters
#' all_params <- sigma_param + theta_param
trend_param <- function(name, bounds = NULL, monitor = TRUE,
                       label = NULL, condition = TRUE) {
  checkmate::assert_string(name, min.chars = 1)
  checkmate::assert_numeric(bounds, len = 2, null.ok = TRUE)
  checkmate::assert_logical(monitor, len = 1)
  checkmate::assert_string(label, null.ok = TRUE)

  # Create data frame following brms pattern
  out <- data.frame(
    name = name,
    bounds_lower = if(!is.null(bounds)) bounds[1] else NA_real_,
    bounds_upper = if(!is.null(bounds)) bounds[2] else NA_real_,
    monitor = monitor,
    label = label %||% name,
    condition = deparse(substitute(condition)),  # Store condition as string
    stringsAsFactors = FALSE
  )

  class(out) <- c("trend_param", "data.frame")
  return(out)
}

#' Combine trend parameters
#' @export
`+.trend_param` <- function(e1, e2) {
  if (is.null(e2)) return(e1)
  if (!is.trend_param(e2)) {
    stop(insight::format_error("Cannot add '{class(e2)[1]}' objects to trend parameters."))
  }
  c(e1, e2)
}

#' @export
c.trend_param <- function(x, ..., replace = FALSE) {
  dots <- list(...)
  if (all(sapply(dots, is.trend_param))) {
    out <- do.call(rbind, list(x, ...))
    if (replace) {
      # Handle duplicates by keeping last occurrence
      out <- out[!duplicated(out$name, fromLast = TRUE), ]
    }
    class(out) <- c("trend_param", "data.frame")
  } else {
    stop(insight::format_error("All objects must be 'trend_param' class."))
  }
  out
}

#' @export
is.trend_param <- function(x) {
  inherits(x, "trend_param")
}

#' @export
print.trend_param <- function(x, ...) {
  cat("Trend parameter specification:\n")
  for (i in seq_len(nrow(x))) {
    row <- x[i, ]
    cat(sprintf("  %s", row$name))
    if (!is.na(row$bounds_lower) && !is.na(row$bounds_upper)) {
      cat(sprintf(" [%.2f, %.2f]", row$bounds_lower, row$bounds_upper))
    }
    if (!row$monitor) cat(" (not monitored)")
    if (!is.na(row$label) && row$label != row$name) {
      cat(sprintf(" (%s)", row$label))
    }
    cat("\n")
  }
  invisible(x)
}

#' Evaluate trend parameter conditions
#'
#' Internal function that evaluates conditional expressions in trend parameters
#'   to determine which parameters should be included in the current context.
#'
#' @param param_spec A trend_param object
#' @param envir Environment for evaluating conditions
#'
#' @return Filtered trend_param object with only active parameters
#' @noRd
evaluate_param_conditions <- function(param_spec, envir = parent.frame()) {
  if (!is.trend_param(param_spec)) {
    stop(insight::format_error("Input must be a 'trend_param' object."))
  }

  # Evaluate conditions for each parameter
  keep_rows <- logical(nrow(param_spec))
  for (i in seq_len(nrow(param_spec))) {
    condition_str <- param_spec$condition[i]
    if (is.na(condition_str) || condition_str == "TRUE") {
      keep_rows[i] <- TRUE
    } else if (condition_str == "FALSE") {
      keep_rows[i] <- FALSE
    } else {
      tryCatch({
        condition_expr <- parse(text = condition_str)[[1]]
        keep_rows[i] <- eval(condition_expr, envir = envir)
      }, error = function(e) {
        # If condition can't be evaluated, default to TRUE
        keep_rows[i] <- TRUE
      })
    }
  }

  # Filter to active parameters
  active_params <- param_spec[keep_rows, ]
  class(active_params) <- c("trend_param", "data.frame")
  return(active_params)
}

# =============================================================================
# SECTION 3: TREND VALIDATION AND PARSING
# =============================================================================
# WHY: Trend validation ensures data integrity and prevents runtime errors
# during Stan model compilation. Formula parsing enables complex multivariate
# trend specifications while maintaining compatibility with brms syntax.
# This layer bridges user-friendly R formulas to internal trend objects.

#' Process trend parameters with bounds and monitoring flags
#'
#' @description
#' This function processes trend model parameters by adding a "_trend" suffix to
#' avoid naming conflicts with brms observation model parameters. It handles
#' parameter names, bounds, and monitoring flags in a single operation.
#'
#' @details
#' All trend parameters are defined as arrays in Stan for consistency, even
#' when n_series = 1. This simplifies forecasting functions and ensures uniform
#' parameter handling across all trend types.
#'
#' When creating custom trend types, define parameters using their base names.
#' This function will automatically add the "_trend" suffix and handle bounds
#' and monitoring flags consistently.
#'
#' @param param_specs Named list where each element is either:
#'   - A numeric vector of length 2 (bounds): c(lower, upper) - monitored by default
#'   - A list with 'bounds', 'monitor', and 'label' elements:
#'     list(bounds = c(0, 1), monitor = FALSE, label = "description")
#'   - NULL (parameter not included when conditional)
#' @return List with three elements:
#'   - tpars: Character vector of all parameter names with _trend suffix
#'   - monitor_pars: Character vector of parameters to monitor (subset of tpars)
#'   - bounds: Named list of bounds with updated parameter names
#' @examples
#' # Define parameters with bounds and monitoring
#' param_specs <- list(
#'   sigma = c(0, Inf),  # Monitored by default
#'   ar = c(-1, 1),      # Monitored by default
#'   LV_innovations = list(bounds = NULL, monitor = FALSE),  # Not monitored
#'   theta = NULL        # Conditional parameter
#' )
#' result <- process_trend_params(param_specs)
#' # result$tpars: c("sigma_trend", "ar_trend", "LV_innovations_trend")
#' # result$monitor_pars: c("sigma_trend", "ar_trend")  # LV_innovations excluded
#' # result$bounds: list(sigma_trend = c(0, Inf), ar_trend = c(-1, 1))
#'
#' @noRd
process_trend_params <- function(param_specs, envir = parent.frame()) {
  # Handle case where no trend-specific parameters are defined
  if (is.null(param_specs) || (is.trend_param(param_specs) && nrow(param_specs) == 0)) {
    return(list(tpars = character(0), monitor_pars = character(0), bounds = list()))
  }

  checkmate::assert_class(param_specs, "trend_param")

  # Evaluate conditions to get active parameters
  active_params <- evaluate_param_conditions(param_specs, envir)

  if (nrow(active_params) == 0) {
    return(list(tpars = character(0), monitor_pars = character(0), bounds = list()))
  }

  # Process parameter names with _trend suffix
  processed_names <- character(nrow(active_params))
  bounds_list <- list()
  monitor_params <- character(0)

  for (i in seq_len(nrow(active_params))) {
    row <- active_params[i, ]

    # Add _trend suffix if not already present
    param_name <- if (!grepl("_trend$", row$name)) {
      paste0(row$name, "_trend")
    } else {
      row$name
    }

    processed_names[i] <- param_name

    # Store bounds if specified
    if (!is.na(row$bounds_lower) && !is.na(row$bounds_upper)) {
      bounds_list[[param_name]] <- c(row$bounds_lower, row$bounds_upper)
    }

    # Track monitored parameters
    if (row$monitor) {
      monitor_params <- c(monitor_params, param_name)
    }
  }

  return(list(
    tpars = processed_names,
    monitor_pars = monitor_params,
    bounds = bounds_list
  ))
}

#' Trend type registry for extensible dispatch
#'
#' Central registry of all available trend types for formula parsing and
#'   validation. New trend types are automatically included when registered.
#'
#' @return Character vector of trend type namesL
#' @noRd
mvgam_trend_registry <- function() {
  # All trend types from the single registry environment
  ls(trend_registry)
}

#' Get available trend type choices
#'
#' Returns a character vector of available trend types from the registry.
#'
#' @return Character vector of trend type names
#' @export
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
  # Updated pattern to handle nested parentheses
  paste0("\\b(", paste(trend_types, collapse = "|"), ")\\s*\\([^)]*(?:\\([^)]*\\)[^)]*)*\\)")
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
  validate_trend(trend_obj)

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
    # If character input, search directly
    terms_char <- x
  } else {
    # If formula input, extract term labels
    terms_char <- attr(terms(x), "term.labels")
  }

  # Use mvgam-style approach: grep for each trend type
  trend_types <- mvgam_trend_registry()
  trend_matches <- character(0)

  for (trend_type in trend_types) {
    # Look for trend_type followed by opening parenthesis (like mvgam's dynamic() detection)
    pattern <- paste0(trend_type, '\\s*\\(')
    which_trends <- grep(pattern, terms_char, fixed = FALSE)

    if (length(which_trends) > 0) {
      # Extract the full function calls
      for (idx in which_trends) {
        term <- terms_char[idx]
        # Find all instances of this trend type in this term
        matches <- gregexpr(pattern, term)[[1]]
        for (match_start in matches) {
          if (match_start > 0) {
            # Extract from match start to end of term (simple approach)
            # Find the function call - count parentheses
            remaining_text <- substr(term, match_start, nchar(term))
            paren_count <- 0
            end_pos <- 0

            for (i in seq_len(nchar(remaining_text))) {
              char <- substr(remaining_text, i, i)
              if (char == "(") paren_count <- paren_count + 1
              if (char == ")") {
                paren_count <- paren_count - 1
                if (paren_count == 0) {
                  end_pos <- i
                  break
                }
              }
            }

            if (end_pos > 0) {
              full_call <- substr(remaining_text, 1, end_pos)
              trend_matches <- c(trend_matches, full_call)
            }
          }
        }
      }
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

#' Parse trend formula with brms-inspired validation
#'
#' Extracts trend model specifications from a formula using robust validation
#'   patterns inspired by brms' validate_formula and mvgam's interpret_mvgam.
#'
#' @param trend_formula A formula object containing trend specifications
#' @param data The data frame for validation (optional)
#'
#' @return List containing parsed formula components
#' @noRd
parse_trend_formula <- function(trend_formula, data = NULL) {

  # Input validation with brms-inspired error handling
  checkmate::assert_class(trend_formula, "formula")

  # Safe formula parsing with try() like brms
  tf_safe <- try(terms(trend_formula, keep.order = TRUE), silent = TRUE)
  if (inherits(tf_safe, "try-error")) {
    insight::format_error(
      "Invalid formula syntax.",
      "The {.field trend_formula} could not be parsed.",
      "Check for balanced parentheses and valid R syntax."
    )
  }

  # Check for response variable (brms pattern)
  if (attr(tf_safe, "response") > 0) {
    insight::format_error(
      "Response variable not allowed in trend formula.",
      "Trend formulas should only contain predictors.",
      "Remove the response variable from {.field trend_formula}."
    )
  }

  # Handle dot expansion if data provided (brms pattern)
  if (!is.null(data)) {
    # Expand dots in formula using stats::terms with data
    tf_expanded <- try(terms(trend_formula, data = data, keep.order = TRUE), silent = TRUE)
    if (!inherits(tf_expanded, "try-error")) {
      tf_safe <- tf_expanded
    }
  }

  # Extract term labels (mvgam pattern)
  tf <- attr(tf_safe, 'term.labels')

  # Validate non-empty formula (brms pattern)
  if (length(tf) == 0) {
    insight::format_error(
      "Empty trend formula provided.",
      "The {.field trend_formula} must contain at least one term.",
      "Example: {.code ~ RW()} or {.code ~ s(time) + AR()}"
    )
  }

  # Find trend terms using mvgam-style detection with brms-inspired robustness
  trend_types <- mvgam_trend_registry()
  trend_indices <- integer(0)

  for (trend_type in trend_types) {
    # Use mvgam's approach: grep with fixed=TRUE (like dynamic() detection)
    which_trends <- grep(paste0(trend_type, '('), tf, fixed = TRUE)
    if (length(which_trends) > 0) {
      trend_indices <- c(trend_indices, which_trends)
    }
  }

  # Remove duplicates and maintain order
  trend_indices <- unique(sort(trend_indices))
  trend_terms <- tf[trend_indices]

  # Regular terms are everything else
  regular_indices <- setdiff(seq_along(tf), trend_indices)
  regular_terms <- if (length(regular_indices) > 0) tf[regular_indices] else character(0)

  # Validate we have at least one trend model (brms-style error)
  if (length(trend_terms) == 0) {
    available_trends <- paste(mvgam_trend_choices(), collapse = "(), ")
    stop(insight::format_error(
      "No trend model specified in trend_formula.",
      "At least one trend constructor is required.",
      "Available constructors: {.field {available_trends}()}."
    ))
  }
  
  # Validate we have exactly one trend type per response - only one allowed per formula
  if (length(trend_terms) > 1) {
    stop(insight::format_error(
      "Multiple trend constructors detected in single response formula.",
      paste("Found:", paste(trend_terms, collapse = ", ")),
      "Only one trend constructor is allowed per response variable.",
      "For multivariate models, use separate trend formulas per response."
    ))
  }

  # Parse trend constructor calls with error handling (brms pattern)
  trend_components <- vector("list", length(trend_terms))
  names(trend_components) <- paste0("trend", seq_along(trend_terms))

  for (i in seq_along(trend_terms)) {
    trend_components[[i]] <- eval_trend_constructor(trend_terms[i])
  }

  # Validate trend components for any remaining conflicts
  validate_trend_components(trend_components)

  # Create base formula without trend constructors (brms pattern)
  # offsets not allowed in trend_formula
  offset_attr <- attr(tf_safe, 'offset')

  if (!is.null(offset_attr)) {
    insight::format_error(
      "Offsets not allowed in trend_formula.",
      "Check for invalid syntax in {.field trend_formula}."
    )
  }

  base_formula <- if (length(regular_terms) > 0) {
      try(reformulate(regular_terms, response = NULL), silent = TRUE)
  } else {
      ~ 1  # Intercept only
  }

  # Ensure formula reconstruction succeeded
  if (inherits(base_formula, "try-error")) {
    insight::format_error(
      "Failed to reconstruct base formula.",
      "Regular terms could not be combined into a valid formula.",
      "Check for invalid predictor syntax in {.field trend_formula}."
    )
  }

  # Since we enforce single trend type, trend_model is always the first component
  trend_model <- trend_components[[1]]

  return(list(
    base_formula = base_formula,
    trend_components = trend_components,
    trend_model = trend_model,
    trend_terms = trend_terms,
    regular_terms = regular_terms,
    offset_terms = character(0),
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

# =============================================================================
# SECTION 3: TREND CONSTRUCTOR FUNCTIONS
# =============================================================================
# WHY: Trend constructors provide the user-facing API for creating trend
# specifications. They must handle parameter validation, set appropriate
# defaults, and create properly structured trend objects that integrate
# seamlessly with the brms ecosystem. This layer abstracts Stan complexity.

#' Trend Model Constructors for \pkg{mvgam}
#'
#' Specify trend models for multivariate State-Space models in \pkg{mvgam}.
#' These constructor functions create trend specifications for various temporal
#' dynamics including random walks (RW), autoregressive models (AR, VAR, CAR),
#' Gaussian processes (GP), and piecewise trends (PW). These functions do not
#' evaluate their arguments – they exist purely to help set up models with
#' particular trend structures.
#'
#' @param ma \code{Logical}. Include moving average terms of order \code{1}?
#'   Default is \code{FALSE}.
#'
#' @param cor \code{Logical}. Include correlated process errors as part of a
#'   multivariate normal process model? If \code{TRUE} and if
#'   \code{n_series > 1} in the supplied data, a fully structured covariance
#'   matrix will be estimated for the process errors. Default is \code{FALSE}.
#'   Note: For \code{VAR()} models, correlation is always enabled (\code{cor = TRUE})
#'   as this is essential for optimal performance.
#'
#' @param p For `AR()` models: A positive integer or vector of positive integers
#'   specifying the autoregressive lag(s). Can be a single value like \code{p = 1}
#'   for AR(1), or a vector like \code{p = c(1, 12, 24)} for seasonal models with
#'   multiple lags. For `VAR()` models: A positive integer specifying the VAR order.
#'   For `CAR()` models: Must be \code{1} (continuous time AR(1) process).
#'
#' @param time The unquoted name of the variable that represents time in the
#'   supplied `data`. This variable should be either a `numeric` or `integer`
#'   variable. Defaults to `time` to align with brms conventions, allowing
#'   flexible time variable naming without requiring explicit "time" columns.
#'   When using the default, a one-time warning will be issued.
#'
#' @param series The unquoted name of the variable that represents the series
#'   identifier in the supplied `data`. This variable should be either a
#'   `character` or `factor` variable. Defaults to `series` following mvgam
#'   conventions, allowing flexible series variable naming. When using the
#'   default, a one-time warning will be issued.
#'
#' @details
#' **Important**: Only ONE trend constructor is allowed per `trend_formula`.
#' For complex temporal dynamics, use flexible parameters within a single trend type:
#' \itemize{
#'   \item For seasonal patterns: `AR(p = c(1, 12))` instead of `RW() + AR(p = 12)`
#'   \item For multiple time scales: `AR(p = c(1, 7, 30))` for daily, weekly, monthly
#'   \item For multivariate dynamics: `VAR(p = 2)` captures cross-series relationships
#' }
#'
#' @param gr An optional grouping variable, which must be a `factor` in the
#'   supplied `data`, for setting up hierarchical residual correlation
#'   structures. If specified, this will automatically set `cor = TRUE` and set
#'   up a model where the residual correlations for a specific level of `gr`
#'   are modelled hierarchically:
#'
#'   \eqn{\Omega_{group} = \alpha_{cor}\Omega_{global} +
#'   (1 - \alpha_{cor})\Omega_{group, local}},
#'
#'   where \eqn{\Omega_{global}} is a *global* correlation matrix,
#'   \eqn{\Omega_{group, local}} is a *local deviation* correlation matrix and
#'   \eqn{\alpha_{cor}} is a weighting parameter controlling how strongly the
#'   local correlation matrix \eqn{\Omega_{group}} is shrunk towards the global
#'   correlation matrix \eqn{\Omega_{global}} (larger values of
#'   \eqn{\alpha_{cor}} indicate a greater degree of shrinkage, i.e. a greater
#'   degree of partial pooling).
#'
#'   When used within a `VAR()` model, this essentially sets up a hierarchical
#'   panel vector autoregression where both the autoregressive and correlation
#'   matrices are learned hierarchically. If `gr` is supplied then `subgr`
#'   *must* also be supplied.
#'
#' @param subgr A subgrouping `factor` variable specifying which element in
#'   `data` represents the different time series. Defaults to `series`, but
#'   note that models that use the hierarchical correlations, where the
#'   `subgr` time series are measured in each level of `gr`, *should not*
#'   include a `series` element in `data`. Rather, this element will be created
#'   internally based on the supplied variables for `gr` and `subgr`.
#'
#'   For example, if you are modelling temporal counts for a group of species
#'   (labelled as `species` in `data`) across three different geographical
#'   regions (labelled as `region`), and you would like the residuals to be
#'   correlated within regions, then you should specify `gr = region` and
#'   `subgr = species`. Internally, `mvgam()` will create the `series` element
#'   for the data using:
#'
#'   `series = interaction(group, subgroup, drop = TRUE)`
#'
#' @return An object of class \code{mvgam_trend}, which contains a list of
#'   arguments to be interpreted by the parsing functions in \pkg{mvgam}.
#'
#' @rdname trend_constructors
#'
#' @details Use `vignette("mvgam_overview")` to see the full details of
#'   available stochastic trend types in \pkg{mvgam}, or view the rendered
#'   version on the package website at:
#'   https://nicholasjclark.github.io/mvgam/articles/mvgam_overview.html
#'
#' @section Parameter Naming Convention:
#' All trend model parameters automatically receive a "_trend" suffix to prevent
#' naming conflicts with observation model parameters. For example:
#' \itemize{
#'   \item \code{sigma} becomes \code{sigma_trend}
#'   \item \code{theta} becomes \code{theta_trend} (when \code{ma = TRUE})
#'   \item \code{Sigma} becomes \code{Sigma_trend} (when \code{cor = TRUE})
#'   \item \code{ar[p]} becomes \code{ar_trend[p]} (for AR trends)
#'   \item \code{A[p]} becomes \code{A_trend[p]} (for VAR trends)
#' }
#'
#' This naming convention is applied consistently across all trend types and must
#' be considered when:
#' \itemize{
#'   \item Specifying priors (use \code{prior(normal(0, 1), class = sigma_trend)})
#'   \item Extracting parameters from fitted models
#'   \item Creating custom trend types
#' }
#'
#' @section Custom Trend Development:
#' When creating custom trend types, define parameters and bounds using their
#' base names (e.g., "sigma", "alpha"). The \code{process_trend_params()} function
#' will automatically add the "_trend" suffix and handle bounds consistently.
#'
#' Example custom trend constructor pattern:
#' \preformatted{
#' custom_trend <- function(...) {
#'   # Define parameters with bounds using base names
#'   param_bounds <- list(
#'     decay = c(0, 1),
#'     amplitude = c(0, Inf),
#'     phase = NULL  # NULL means no bounds needed
#'   )
#'
#'   # Process automatically
#'   processed <- process_trend_params(param_bounds)
#'
#'   # Use in trend object
#'   structure(list(
#'     trend = "Custom",
#'     tpars = processed$tpars,  # c("decay_trend", "amplitude_trend")
#'     bounds = processed$bounds # list(decay_trend = c(0, 1), ...)
#'   ), class = "mvgam_trend")
#' }
#' }
#'
#' @author Nicholas J Clark
#'
#' @examples
#' \donttest{
#' # Basic trend model usage with defaults (will issue warnings)
#' set.seed(0)
#' simdat <- sim_mvgam(
#'   T = 50,
#'   n_series = 3,
#'   prop_trend = 0.6
#' )
#' mod1 <- mvgam(
#'   y ~ s(season, bs = "cc"),
#'   trend_formula = ~ RW(),
#'   data = simdat$data_train,
#'   chains = 2,
#'   silent = 2
#' )
#'
#' # Using custom time and series variable names with simulated data
#' set.seed(123)
#' weekly_data <- sim_mvgam(
#'   T = 52,
#'   n_series = 2,
#'   prop_trend = 0.5
#' )$data_train
#'
#' # Rename variables to demonstrate custom naming
#' weekly_data$week <- weekly_data$time
#' weekly_data$species <- weekly_data$series
#' weekly_data$temp <- rnorm(nrow(weekly_data))
#'
#' mod2 <- mvgam(
#'   y ~ temp,
#'   trend_formula = ~ AR(time = week, series = species, p = 1),
#'   data = weekly_data,
#'   chains = 2,
#'   silent = 2
#' )
#'
#' # Seasonal AR model with multiple lags using portal_data
#' data(portal_data)
#' mod3 <- mvgam(
#'   captures ~ s(ndvi_ma12) + s(mintemp),
#'   trend_formula = ~ AR(p = c(1, 12)),  # Annual seasonality
#'   data = portal_data,
#'   family = nb(),
#'   chains = 2,
#'   silent = 2
#' )
#'
#' # Hierarchical models with custom time and series variables
#' set.seed(456)
#' multisite_data <- rbind(
#'   sim_mvgam(T = 30, n_series = 3, prop_trend = 0.7)$data_train %>%
#'     dplyr::mutate(region = "north"),
#'   sim_mvgam(T = 30, n_series = 3, prop_trend = 0.7)$data_train %>%
#'     dplyr::mutate(region = "south")
#' ) %>%
#'   dplyr::mutate(
#'     region = as.factor(region),
#'     unit_id = paste0(region, "_", series),
#'     timestep = time,
#'     temperature = rnorm(n())
#'   ) %>%
#'   dplyr::select(-series)  # Remove series for hierarchical structure
#'
#' mod4 <- mvgam(
#'   y ~ temperature,
#'   trend_formula = ~ VAR(
#'     time = timestep,
#'     gr = region,
#'     subgr = unit_id,
#'     p = 2
#'   ),
#'   data = multisite_data,
#'   chains = 2,
#'   silent = 2
#' )
#'
#' # Multiple species across sites using portal_data
#' portal_subset <- portal_data %>%
#'   dplyr::mutate(
#'     survey_date = time,
#'     species_site = series,
#'     count = captures,
#'     habitat = "grassland",
#'     temperature = mintemp
#'   )
#'
#' mod5 <- mvgam(
#'   count ~ habitat + temperature,
#'   trend_formula = ~ RW(time = survey_date, series = species_site),
#'   data = portal_subset,
#'   family = nb(),
#'   chains = 2,
#'   silent = 2
#' )
#'
#' # A short example to illustrate CAR(1) models
#' # Function to simulate CAR1 data with seasonality
#' sim_corcar1 <- function(n = 125,
#'                         phi = 0.5,
#'                         sigma = 2,
#'                         sigma_obs = 0.75) {
#'   # Sample irregularly spaced time intervals
#'   time_dis <- c(1, runif(n - 1, 0, 5))
#'
#'   # Set up the latent dynamic process
#'   x <- vector(length = n)
#'   x[1] <- -0.3
#'   for (i in 2:n) {
#'     # Zero-distances will cause problems in sampling, so mvgam uses a
#'     # minimum threshold; this simulation function emulates that process
#'     if (time_dis[i] == 0) {
#'       x[i] <- rnorm(
#'         1,
#'         mean = (phi^1e-3) * x[i - 1],
#'         sd = sigma * sqrt((1 - phi^(2 * 1e-3)) / (1 - phi^2))
#'       )
#'     } else {
#'       x[i] <- rnorm(
#'         1,
#'         mean = (phi^time_dis[i]) * x[i - 1],
#'         sd = sigma * sqrt((1 - phi^(2 * time_dis[i])) / (1 - phi^2))
#'       )
#'     }
#'   }
#'
#'   # Add 12-month seasonality
#'   cov1 <- sin(2 * pi * (1:n) / 12)
#'   cov2 <- cos(2 * pi * (1:n) / 12)
#'   beta1 <- runif(1, 0.3, 0.7)
#'   beta2 <- runif(1, 0.2, 0.5)
#'   seasonality <- beta1 * cov1 + beta2 * cov2
#'
#'   # Take Gaussian observations with error and return
#'   data.frame(
#'     y = rnorm(n, mean = x + seasonality, sd = sigma_obs),
#'     season = rep(1:12, 20)[1:n],
#'     time = cumsum(time_dis)
#'   )
#' }
#'
#' # Sample two time series
#' set.seed(99)
#' dat <- rbind(
#'   dplyr::bind_cols(
#'     sim_corcar1(phi = 0.65, sigma_obs = 0.55),
#'     data.frame(series = "series1")
#'   ),
#'   dplyr::bind_cols(
#'     sim_corcar1(phi = 0.8, sigma_obs = 0.35),
#'     data.frame(series = "series2")
#'   )
#' ) %>%
#'   dplyr::mutate(series = as.factor(series))
#'
#' # mvgam with CAR(1) trends and series-level seasonal smooths
#' mod <- mvgam(
#'   formula = y ~ -1,
#'   trend_formula = ~ s(season, bs = 'cc', k = 5, by = trend),
#'   trend_model = CAR(),
#'   priors = c(
#'     prior(exponential(3), class = sigma),
#'     prior(beta(4, 4), class = sigma_obs)
#'   ),
#'   data = dat,
#'   family = gaussian(),
#'   chains = 2,
#'   silent = 2
#' )
#'
#' # View usual summaries and plots
#' summary(mod)
#' conditional_effects(mod, type = 'expected')
#' plot(mod, type = 'trend', series = 1)
#' plot(mod, type = 'trend', series = 2)
#' plot(mod, type = 'residuals', series = 1)
#' plot(mod, type = 'residuals', series = 2)
#' mcmc_plot(
#'   mod,
#'   variable = "ar1",
#'   regex = TRUE,
#'   type = "hist"
#' )
#'
#' # Now an example illustrating hierarchical dynamics
#' set.seed(123)
#'
#' # Simulate three species monitored in three different regions
#' simdat1 <- sim_mvgam(
#'   trend_model = VAR(),  # cor = TRUE is now automatic for VAR models
#'   prop_trend = 0.95,
#'   n_series = 3,
#'   mu = c(1, 2, 3)
#' )
#' simdat2 <- sim_mvgam(
#'   trend_model = VAR(),  # cor = TRUE is now automatic for VAR models
#'   prop_trend = 0.95,
#'   n_series = 3,
#'   mu = c(1, 2, 3)
#' )
#' simdat3 <- sim_mvgam(
#'   trend_model = VAR(),  # cor = TRUE is now automatic for VAR models
#'   prop_trend = 0.95,
#'   n_series = 3,
#'   mu = c(1, 2, 3)
#' )
#'
#' # Set up the data but DO NOT include 'series'
#' all_dat <- rbind(
#'   simdat1$data_train %>%
#'     dplyr::mutate(region = "qld"),
#'   simdat2$data_train %>%
#'     dplyr::mutate(region = "nsw"),
#'   simdat3$data_train %>%
#'     dplyr::mutate(region = "vic")
#' ) %>%
#'   dplyr::mutate(
#'     species = gsub("series", "species", series),
#'     species = as.factor(species),
#'     region = as.factor(region)
#'   ) %>%
#'   dplyr::arrange(series, time) %>%
#'   dplyr::select(-series)
#'
#' # Check priors for a hierarchical AR1 model
#' get_mvgam_priors(
#'   formula = y ~ species,
#'   trend_model = AR(gr = region, subgr = species),
#'   data = all_dat
#' )
#'
#' # Fit the model
#' mod <- mvgam(
#'   formula = y ~ species,
#'   trend_model = AR(gr = region, subgr = species),
#'   data = all_dat,
#'   chains = 2,
#'   silent = 2
#' )
#'
#' # Check standard outputs
#' summary(mod)
#' conditional_effects(mod, type = "link")
#'
#' # Inspect posterior estimates for the correlation weighting parameter
#' mcmc_plot(mod, variable = "alpha_cor", type = "hist")
#' }
#' @export
RW = function(
    time = NA,
    series = NA,
    ma = FALSE,
    cor = FALSE,
    gr = NA,
    subgr = NA,
    n_lv = NULL) {
  # Process time argument
  time <- deparse0(substitute(time))
  time_was_default <- (time == "NA")
  if (time == "NA") time <- "time"  # Default to 'time' when NA

  # Process series argument
  series <- deparse0(substitute(series))
  series_was_default <- (series == "NA")
  if (series == "NA") series <- "series"  # Default to 'series' when NA

  # Issue warnings for default usage using modular functions
  if (time_was_default) warn_default_time_variable()
  if (series_was_default) warn_default_series_variable()

  # Input validation using checkmate
  checkmate::assert_logical(ma, len = 1)
  checkmate::assert_logical(cor, len = 1)

  # Process and validate grouping arguments
  gr <- deparse0(substitute(gr))
  subgr <- deparse0(substitute(subgr))
  groupings <- validate_grouping_arguments(gr, subgr)
  gr <- groupings$gr
  subgr <- groupings$subgr

  # Validate and adjust correlation requirements
  cor <- validate_correlation_requirements(gr, cor)

  # Define ONLY trend-specific parameters (Gaussian innovation infrastructure is automatic)
  # RW only has MA coefficient when ma = TRUE, otherwise no trend-specific parameters
  param_specs <- if (ma) {
    trend_param("theta1", bounds = c(-1, 1), label = "moving_average_coefficient_lag_1")
  } else {
    NULL  # No trend-specific parameters for basic RW
  }

  # Define trend characteristics and Gaussian innovation settings
  characteristics <- list(
    supports_predictors = TRUE,
    supports_correlation = TRUE,
    supports_factors = TRUE,
    supports_hierarchical = TRUE,  # RW supports gr/subgr
    innovation_type = "gaussian_shared",  # Uses shared Gaussian innovation system
    max_order = 1,
    requires_sorting = TRUE,
    # Gaussian innovation settings (automatic parameters)
    uses_sigma_trend = TRUE,          # Always has sigma_trend (innovation SD)
    uses_correlation = cor || gr != 'NA',  # Uses Sigma_trend/L_Omega_trend when needed
    monitor_innovations = ma          # Monitor LV_innovations only for MA models
  )

  # Process trend-specific parameters only
  processed_params <- process_trend_params(param_specs, envir = environment())

  # Create complete parameter info for storage
  param_info <- list(
    parameters = param_specs,  # Original trend_param specifications
    characteristics = characteristics
  )

  # Create trend object with dispatcher integration
  out <- structure(
    list(
      trend = 'RW',
      ma = ma,
      cor = cor,
      time = time,
      series = series,
      gr = gr,
      subgr = subgr,
      n_lv = n_lv,
      label = build_trend_label('RW', cor = cor, ma = ma, gr = gr, n_lv = n_lv),
      tpars = processed_params$tpars,
      forecast_fun = 'forecast_rw_rcpp',
      stancode_fun = 'rw_stan_code',
      standata_fun = 'rw_stan_data',
      bounds = processed_params$bounds,
      param_info = param_info,  # Complete trend specification (parameters + characteristics)
      shared_innovations = TRUE  # RW uses shared Gaussian innovation system
    ),
    class = 'mvgam_trend'
  )

  # Validate using dispatcher system
  validate_trend(out)

  return(out)
}

#' @rdname trend_constructors
#' @export
AR = function(time = NA, series = NA, p = 1, ma = FALSE, cor = FALSE, gr = NA, subgr = NA, n_lv = NULL) {
  # Process time argument
  time <- deparse0(substitute(time))
  time_was_default <- (time == "NA")
  if (time == "NA") time <- "time"  # Default to 'time' when NA

  # Process series argument
  series <- deparse0(substitute(series))
  series_was_default <- (series == "NA")
  if (series == "NA") series <- "series"  # Default to 'series' when NA

  # Issue warnings for default usage using modular functions
  if (time_was_default) warn_default_time_variable()
  if (series_was_default) warn_default_series_variable()

  # Validate AR order parameter - can be integer or vector of integers
  if (length(p) == 1) {
    checkmate::assert_int(p, lower = 1)
    ar_lags <- 1:p
    max_lag <- p
  } else {
    checkmate::assert_integerish(p, lower = 1, unique = TRUE, sorted = TRUE)
    ar_lags <- p
    max_lag <- max(p)
  }

  # Input validation using checkmate
  checkmate::assert_logical(ma, len = 1)
  checkmate::assert_logical(cor, len = 1)

  # Process and validate grouping arguments
  gr <- deparse0(substitute(gr))
  subgr <- deparse0(substitute(subgr))
  groupings <- validate_grouping_arguments(gr, subgr)
  gr <- groupings$gr
  subgr <- groupings$subgr

  # Validate and adjust correlation requirements
  cor <- validate_correlation_requirements(gr, cor)

  # Define ONLY trend-specific parameters (Gaussian innovation infrastructure is automatic)
  # Always use ar{lag}_trend naming for absolute consistency (ar1_trend, ar12_trend, etc.)
  param_specs <- NULL
  for (lag in ar_lags) {
    ar_param <- trend_param(paste0("ar", lag), bounds = c(-1, 1),
                           label = paste0("ar_coefficient_lag_", lag))
    if (is.null(param_specs)) {
      param_specs <- ar_param
    } else {
      param_specs <- param_specs + ar_param
    }
  }

  # Add MA coefficient if specified (future: could support multiple MA lags)
  if (ma) {
    param_specs <- param_specs + trend_param("theta1", bounds = c(-1, 1), label = "moving_average_coefficient_lag_1")
  }

  # Define trend characteristics and Gaussian innovation settings
  characteristics <- list(
    supports_predictors = TRUE,
    supports_correlation = TRUE,
    supports_factors = TRUE,
    supports_hierarchical = TRUE,  # AR supports gr/subgr
    innovation_type = "gaussian_shared",  # Uses shared Gaussian innovation system
    max_order = max_lag,
    requires_sorting = TRUE,
    # Gaussian innovation settings (automatic parameters)
    uses_sigma_trend = TRUE,          # Always has sigma_trend (innovation SD)
    uses_correlation = cor || gr != 'NA',  # Uses Sigma_trend/L_Omega_trend when needed
    monitor_innovations = ma          # Monitor LV_innovations only for MA models
  )

  # Process trend-specific parameters only
  processed_params <- process_trend_params(param_specs, envir = environment())

  # Create complete parameter info for storage
  param_info <- list(
    parameters = param_specs,  # Trend-specific parameters only
    characteristics = characteristics
  )

  # Create trend object with dispatcher integration
  out <- structure(
    list(
      trend = if(length(p) == 1) paste0('AR', p) else paste0('AR(', paste(p, collapse = ','), ')'),
      p = p,
      ar_lags = ar_lags,
      max_lag = max_lag,
      ma = ma,
      cor = cor,
      time = time,
      series = series,
      gr = gr,
      subgr = subgr,
      n_lv = n_lv,
      label = build_trend_label('AR', cor = cor, ma = ma, gr = gr, n_lv = n_lv, p = max_lag),
      tpars = processed_params$tpars,  # Only trend-specific parameters
      forecast_fun = 'forecast_ar_rcpp',
      stancode_fun = 'ar_stan_code',
      standata_fun = 'ar_stan_data',
      bounds = processed_params$bounds,
      param_info = param_info,  # Complete specification (trend params + characteristics)
      shared_innovations = TRUE  # AR uses shared Gaussian innovation system
    ),
    class = 'mvgam_trend'
  )

  # Validate using dispatcher system
  validate_trend(out)

  return(out)
}

#' @rdname trend_constructors
#' @export
CAR = function(time = NA, series = NA, p = 1, n_lv = NULL) {
  # Factor model validation - CAR trends don't support factor models
  if (!is.null(n_lv)) {
    stop(insight::format_error(
      "Factor models ({.field n_lv}) not supported for CAR trends.",
      "Continuous-time AR requires series-specific irregular time intervals.",
      "Remove {.field n_lv} parameter or use factor-compatible trends: AR, RW, VAR, ZMVN"
    ), call. = FALSE)
  }

  # Process time argument
  time <- deparse0(substitute(time))
  time_was_default <- (time == "NA")
  if (time == "NA") time <- "time"  # Default to 'time' when NA

  # Process series argument
  series <- deparse0(substitute(series))
  series_was_default <- (series == "NA")
  if (series == "NA") series <- "series"  # Default to 'series' when NA

  # Issue warnings for default usage using modular functions
  if (time_was_default) warn_default_time_variable()
  if (series_was_default) warn_default_series_variable()

  validate_pos_integer(p)
  if (p > 1) {
    stop("Argument 'p' must be = 1", call. = FALSE)
  }

  # Define ONLY trend-specific parameters (Gaussian innovation infrastructure is automatic)
  param_specs <- trend_param("ar1", bounds = c(-1, 1), label = "autoregressive_coefficient")

  # Define trend characteristics and Gaussian innovation settings
  characteristics <- list(
    supports_predictors = TRUE,
    supports_correlation = FALSE,  # CAR doesn't support correlation
    supports_factors = FALSE,      # CAR doesn't support factor models
    supports_hierarchical = FALSE, # CAR doesn't support gr/subgr
    innovation_type = "gaussian_shared",  # Uses shared Gaussian innovation system
    max_order = 1,
    requires_sorting = TRUE,
    # Gaussian innovation settings (automatic parameters)
    uses_sigma_trend = TRUE,       # Always has sigma_trend (innovation SD)
    uses_correlation = FALSE,      # Never uses correlation for CAR
    monitor_innovations = FALSE    # CAR never monitors innovations (no MA support)
  )

  # Process trend-specific parameters only
  processed_params <- process_trend_params(param_specs, envir = environment())

  # Create complete parameter info for storage
  param_info <- list(
    parameters = param_specs,  # Trend-specific parameters only
    characteristics = characteristics
  )

  out <- structure(
    list(
      trend = 'CAR',
      ma = FALSE,
      cor = FALSE,
      time = time,
      series = series,
      unit = 'time',
      gr = 'NA',
      subgr = 'series',
      label = build_trend_label('CAR'),
      tpars = processed_params$tpars,  # Only trend-specific parameters
      forecast_fun = 'forecast_car_rcpp',
      stancode_fun = 'car_stan_code',
      standata_fun = 'car_stan_data',
      bounds = processed_params$bounds,
      param_info = param_info,  # Complete specification (trend params + characteristics)
      shared_innovations = FALSE  # CAR handles its own innovations due to irregular time sampling
    ),
    class = 'mvgam_trend'
  )
}

#' @rdname trend_constructors
#' @export
VAR = function(time = NA, series = NA, p = 1, ma = FALSE, gr = NA, subgr = NA, n_lv = NULL) {
  # Process time argument
  time <- deparse0(substitute(time))
  time_was_default <- (time == "NA")
  if (time == "NA") time <- "time"  # Default to 'time' when NA

  # Process series argument
  series <- deparse0(substitute(series))
  series_was_default <- (series == "NA")
  if (series == "NA") series <- "series"  # Default to 'series' when NA

  # Issue warnings for default usage using modular functions
  if (time_was_default) warn_default_time_variable()
  if (series_was_default) warn_default_series_variable()

  # Validate VAR order parameter - any positive integer
  checkmate::assert_int(p, lower = 1)

  # Input validation using checkmate
  checkmate::assert_logical(ma, len = 1)

  # Process and validate grouping arguments
  gr <- deparse0(substitute(gr))
  subgr <- deparse0(substitute(subgr))
  groupings <- validate_grouping_arguments(gr, subgr)
  gr <- groupings$gr
  subgr <- groupings$subgr

  # VAR models always use correlation (cor = TRUE) for optimal performance
  cor <- TRUE

  # Define ONLY trend-specific parameters (Gaussian innovation infrastructure is automatic)
  # VAR uses A{lag}_trend naming for transition matrices and theta{lag}_trend for MA
  param_specs <- trend_param("A1", bounds = c(-1, 1), label = "var_transition_matrix_lag_1")

  # Add additional lag parameters for p > 1
  if (p > 1) {
    for (lag in 2:p) {
      param_specs <- param_specs + trend_param(paste0("A", lag), bounds = c(-1, 1),
                                               label = paste0("var_transition_matrix_lag_", lag))
    }
  }

  # Add MA coefficient if specified (future: could support multiple MA lags)
  if (ma) {
    param_specs <- param_specs + trend_param("theta1", bounds = c(-1, 1), label = "moving_average_coefficient_lag_1")
  }

  # Define trend characteristics and Gaussian innovation settings
  characteristics <- list(
    supports_predictors = TRUE,
    supports_correlation = TRUE,
    supports_factors = TRUE,
    supports_hierarchical = TRUE,  # VAR supports gr/subgr
    innovation_type = "gaussian_var",  # VAR uses special covariance estimation
    max_order = p,
    requires_sorting = TRUE,
    # Gaussian innovation settings (automatic parameters)
    uses_sigma_trend = FALSE,        # VAR estimates full Sigma matrix instead
    uses_correlation = TRUE,         # VAR always estimates correlation structure
    monitor_innovations = ma         # Monitor LV_innovations only for MA models
  )

  # Process trend-specific parameters only
  processed_params <- process_trend_params(param_specs, envir = environment())

  # Create complete parameter info for storage
  param_info <- list(
    parameters = param_specs,  # Trend-specific parameters only
    characteristics = characteristics
  )

  # Create trend object with dispatcher integration
  out <- structure(
    list(
      trend = paste0('VAR', p),
      p = p,
      ma = ma,
      cor = cor,
      time = time,
      series = series,
      gr = gr,
      subgr = subgr,
      n_lv = n_lv,
      label = build_trend_label('VAR', cor = cor, ma = ma, gr = gr, n_lv = n_lv, p = p),
      tpars = processed_params$tpars,  # Only trend-specific parameters
      forecast_fun = 'forecast_var_rcpp',
      stancode_fun = 'var_stan_code',
      standata_fun = 'var_stan_data',
      bounds = processed_params$bounds,
      param_info = param_info,  # Complete specification (trend params + characteristics)
      shared_innovations = FALSE  # VAR handles its own covariance structure for stationarity
    ),
    class = 'mvgam_trend'
  )

  # Validate using dispatcher system
  validate_trend(out)

  return(out)
}

#' Specify dynamic Gaussian process trends in \pkg{mvgam} models
#'
#' Set up low-rank approximate Gaussian Process trend models using Hilbert
#' basis expansions in \pkg{mvgam}. This function does not evaluate its
#' arguments – it exists purely to help set up a model with particular GP
#' trend models.
#'
#' @param ... unused
#'
#' @return An object of class \code{mvgam_trend}, which contains a list of
#'   arguments to be interpreted by the parsing functions in \pkg{mvgam}.
#'
#' @details A GP trend is estimated for each series using Hilbert space
#'   approximate Gaussian Processes. In `mvgam`, latent squared exponential GP
#'   trends are approximated using by default \code{20} basis functions and
#'   using a multiplicative factor of `c = 5/4`, which saves computational
#'   costs compared to fitting full GPs while adequately estimating GP
#'   \code{alpha} and \code{rho} parameters.
#'
#' @rdname GP
#'
#' @author Nicholas J Clark
#'
#' @references Riutort-Mayol G, Burkner PC, Andersen MR, Solin A and Vehtari A
#'   (2023). Practical Hilbert space approximate Bayesian Gaussian processes for
#'   probabilistic programming. Statistics and Computing 33, 1.
#'   https://doi.org/10.1007/s11222-022-10167-2
#'
#' @seealso \code{\link[brms]{gp}}
#'
#' @export
GP = function(time = NA, series = NA, ...) {
  # Issue deprecation warning
  rlang::warn(
    paste0(
      "GP() trend models are deprecated and will be removed in a future version.\n",
      "Use Gaussian Process terms in trend_formula instead: ~ gp(time, k = 10)\n",
      "Combined with other trend models: trend_model = AR() or RW()"
    ),
    class = "mvgam_deprecation_warning",
    .frequency = "once",
    .frequency_id = "gp_deprecation"
  )

  # Process time argument
  time <- deparse0(substitute(time))
  time_was_default <- (time == "NA")
  if (time == "NA") time <- "time"  # Default to 'time' when NA

  # Process series argument
  series <- deparse0(substitute(series))
  series_was_default <- (series == "NA")
  if (series == "NA") series <- "series"  # Default to 'series' when NA

  # Issue warnings for default usage using modular functions
  if (time_was_default) warn_default_time_variable()
  if (series_was_default) warn_default_series_variable()

  out <- structure(
    list(
      trend = 'GP',
      ma = FALSE,
      cor = FALSE,
      time = time,
      series = series,
      unit = 'time',
      gr = 'NA',
      subgr = 'series',
      label = match.call()
    ),
    class = 'mvgam_trend',
    param_info = list(
      param_names = c('trend', 'alpha_gp', 'rho_gp', 'b_gp'),
      labels = c('trend_estimates', 'marginal_deviation', 'length_scale', 'basis_coefficients')
    )
  )
}

#' Specify piecewise linear or logistic trends in \pkg{mvgam} models
#'
#' Set up piecewise linear or logistic trend models in \code{mvgam}. These
#' functions do not evaluate their arguments – they exist purely to help set up
#' a model with particular piecewise trend models.
#'
#' @param time The unquoted name of the variable that represents time in the
#'   supplied `data`. This variable should be either a `numeric` or `integer`
#'   variable. Defaults to `time` to align with brms conventions, allowing
#'   flexible time variable naming without requiring explicit "time" columns.
#'   When using the default, a one-time warning will be issued.
#'
#' @param series The unquoted name of the variable that represents the series
#'   identifier in the supplied `data`. This variable should be either a
#'   `character` or `factor` variable. Defaults to `series` following mvgam
#'   conventions, allowing flexible series variable naming. When using the
#'   default, a one-time warning will be issued.
#'
#' @param cap The unquoted name of the variable in `data` that specifies the
#'   carrying capacity for logistic growth models. Required when `growth = 'logistic'`.
#'   This variable should be numeric and can vary by time and series if necessary.
#'   Defaults to `cap` when not specified, with a warning for logistic models.
#'
#' @param n_changepoints A non-negative integer specifying the number of
#'   potential changepoints. Potential changepoints are selected uniformly from
#'   the first `changepoint_range` proportion of timepoints in \code{data}.
#'   Default is `10`.
#'
#' @param changepoint_range Proportion of history in \code{data} in which trend
#'   changepoints will be estimated. Defaults to `0.8` for the first 80%.
#'
#' @param changepoint_scale Parameter modulating the flexibility of the
#'   automatic changepoint selection by altering the scale parameter of a
#'   Laplace distribution. The resulting prior will be
#'   `double_exponential(0, changepoint_scale)`. Large values will allow many
#'   changepoints and a more flexible trend, while small values will allow few
#'   changepoints. Default is `0.05`.
#'
#' @param growth Character string specifying either `'linear'` or `'logistic'`
#'   growth of the trend. If `'logistic'`, the `cap` argument must specify the
#'   variable containing maximum saturation points for the trend (see
#'   details and examples in \code{\link{mvgam}} for more information). Default
#'   is `'linear'`.
#'
#' @author Nicholas J Clark
#'
#' @references Taylor, Sean J., and Benjamin Letham. "Forecasting at scale."
#'   The American Statistician 72.1 (2018): 37–45.
#'
#' @return An object of class \code{mvgam_trend}, which contains a list of
#'   arguments to be interpreted by the parsing functions in \code{mvgam}.
#'
#' @details
#' *Offsets and intercepts*:
#' For each of these trend models, an offset parameter is included in the trend
#' estimation process. This parameter will be incredibly difficult to identify
#' if you also include an intercept in the observation formula. For that
#' reason, it is highly recommended that you drop the intercept from the
#' formula (i.e. `y ~ x + 0` or `y ~ x - 1`, where `x` are your optional
#' predictor terms).
#'
#' *Logistic growth and the cap variable*:
#' When forecasting growth, there is often some maximum achievable point that a
#' time series can reach. For example, total market size, total population size
#' or carrying capacity in population dynamics. It can be advantageous for the
#' forecast to saturate at or near this point so that predictions are more
#' sensible.
#'
#' This function allows you to make forecasts using a logistic growth trend
#' model, with a specified carrying capacity. Note that this capacity does not
#' need to be static over time; it can vary with each series × timepoint
#' combination if necessary. But you must supply a `cap` value for each
#' observation in the data when using `growth = 'logistic'`.
#'
#' For observation families that use a non-identity link function, the `cap`
#' value will be internally transformed to the link scale (i.e. your specified
#' `cap` will be log-transformed if you are using a `poisson()` or `nb()`
#' family). It is therefore important that you specify the `cap` values on the
#' scale of your outcome. Note also that no missing values are allowed in
#' `cap`.
#'
#' @rdname piecewise_trends
#'
#' @examples
#' \donttest{
#' # Basic linear piecewise trend with defaults (will issue warnings)
#' set.seed(101)
#' linear_data <- data.frame(
#'   y = rnorm(50, mean = 1:50, sd = 2),
#'   time = 1:50,
#'   series = factor("series_1")
#' )
#'
#' mod1 <- mvgam(
#'   y ~ 0,
#'   trend_model = PW(growth = "linear"),
#'   data = linear_data,
#'   chains = 2,
#'   silent = 2
#' )
#'
#' # Custom variable names for time and series
#' growth_data <- data.frame(
#'   count = rpois(60, exp(seq(0, 2, length.out = 60))),
#'   week = 1:60,
#'   population = factor("pop_A"),
#'   carrying_capacity = 100
#' )
#'
#' mod2 <- mvgam(
#'   count ~ 0,
#'   trend_model = PW(
#'     time = week,
#'     series = population,
#'     cap = carrying_capacity,
#'     growth = "logistic",
#'     n_changepoints = 5
#'   ),
#'   family = poisson(),
#'   data = growth_data,
#'   chains = 2,
#'   silent = 2
#' )
#'
#' # Example of logistic growth with possible changepoints
#' dNt <- function(r, N, k) {
#'   r * N * (k - N)
#' }
#'
#' Nt <- function(r, N, t, k) {
#'   for (i in 1:(t - 1)) {
#'     if (i %in% c(5, 15, 25, 41, 45, 60, 80)) {
#'       N[i + 1] <- max(
#'         1,
#'         N[i] + dNt(r + runif(1, -0.1, 0.1), N[i], k)
#'       )
#'     } else {
#'       N[i + 1] <- max(1, N[i] + dNt(r, N[i], k))
#'     }
#'   }
#'   N
#' }
#'
#' set.seed(11)
#' expected <- Nt(0.004, 2, 100, 30)
#' plot(expected, xlab = "Time")
#'
#' y <- rpois(100, expected)
#' plot(y, xlab = "Time")
#'
#' mod_data <- data.frame(
#'   y = y,
#'   time = 1:100,
#'   cap = 35,
#'   series = as.factor("series_1")
#' )
#' plot_mvgam_series(data = mod_data)
#'
#' mod3 <- mvgam(
#'   y ~ 0,
#'   trend_model = PW(growth = "logistic"),  # Uses default 'cap' variable
#'   family = poisson(),
#'   data = mod_data,
#'   chains = 2,
#'   silent = 2
#' )
#' summary(mod3)
#'
#' hc <- hindcast(mod3)
#' plot(hc)
#'
#' library(ggplot2)
#' mcmc_plot(mod3, variable = "delta_trend", regex = TRUE) +
#'   scale_y_discrete(labels = mod3$trend_model$changepoints) +
#'   labs(
#'     y = "Potential changepoint",
#'     x = "Rate change"
#'   )
#'
#' how_to_cite(mod3)
#' }
#'
#' @export
PW = function(
  time = NA,
  series = NA,
  cap = NA,
  n_changepoints = 10,
  changepoint_range = 0.8,
  changepoint_scale = 0.05,
  growth = 'linear',
  n_lv = NULL
) {
  # Factor model validation - PW trends don't support factor models
  if (!is.null(n_lv)) {
    stop(insight::format_error(
      "Factor models ({.field n_lv}) not supported for PW trends.",
      "Piecewise trends require series-specific changepoint modeling.",
      "Remove {.field n_lv} parameter or use factor-compatible trends: AR, RW, VAR, ZMVN"
    ), call. = FALSE)
  }
  # Process time argument
  time <- deparse0(substitute(time))
  time_was_default <- (time == "NA")
  if (time == "NA") time <- "time"  # Default to 'time' when NA

  # Process series argument
  series <- deparse0(substitute(series))
  series_was_default <- (series == "NA")
  if (series == "NA") series <- "series"  # Default to 'series' when NA

  # Process cap argument
  cap <- deparse0(substitute(cap))
  cap_was_default <- (cap == "NA")
  if (cap == "NA") cap <- "cap"  # Default to 'cap' when NA

  # Issue warnings for default usage using modular functions
  if (time_was_default) warn_default_time_variable()
  if (series_was_default) warn_default_series_variable()
  if (cap_was_default && growth == 'logistic') {
    rlang::warn(
      paste0(
        "Using default variable name 'cap' for logistic growth carrying capacity.\n",
        "Specify explicitly with: PW(cap = your_cap_variable, growth = 'logistic')"
      ),
      class = "mvgam_default_variable_warning",
      .frequency = "once",
      .frequency_id = "pw_cap_default"
    )
  }

  # Validate arguments
  growth <- match.arg(growth, choices = c('linear', 'logistic'))
  validate_proportional(changepoint_range)
  validate_pos_integer(n_changepoints)
  validate_pos_real(changepoint_scale)

  # Check for required cap variable in logistic models
  if (growth == 'logistic' && cap == "NA") {
    stop(insight::format_error(
      "Logistic growth models require a {.field cap} variable.",
      "Either provide {.field cap} argument or ensure 'cap' column exists in data.",
      "Example: PW(cap = carrying_capacity, growth = 'logistic')"
    ))
  }

  trend_value <- 'PWlinear'
  if (growth == 'logistic') {
    trend_value = 'PWlogistic'
  }
  out <- structure(
    list(
      trend = trend_value,
      n_changepoints = n_changepoints,
      changepoint_range = changepoint_range,
      changepoint_scale = changepoint_scale,
      growth = growth,
      time = time,
      series = series,
      cap = cap,
      ma = FALSE,
      cor = FALSE,
      unit = 'time',
      gr = 'NA',
      subgr = 'series',
      label = match.call(),
      shared_innovations = FALSE  # PW uses changepoint_scale instead of Gaussian innovations
    ),
    class = 'mvgam_trend',
    param_info = list(
      param_names = c('trend', 'delta_trend', 'k_trend', 'm_trend'),
      labels = c('trend_estimates', 'rate_changes', 'growth_rate', 'offset_parameter')
    )
  )
}

#' Specify correlated residual processes in \pkg{mvgam}
#'
#' Set up latent correlated multivariate Gaussian residual processes in
#' \pkg{mvgam}. This function does not evaluate its arguments – it exists
#' purely to help set up a model with particular error processes
#'
#' @param unit The unquoted name of the variable that represents the unit of
#'   analysis in `data` over which latent residuals should be correlated. This
#'   variable should be either a `numeric` or `integer` variable in the
#'   supplied `data`. Defaults to `time` to be consistent with other
#'   functionalities in \pkg{mvgam}, though note that the data need not be time
#'   series in this case. See examples below for further details and
#'   explanations
#'
#' @param gr An optional grouping variable, which must be a `factor` in the
#'   supplied `data`, for setting up hierarchical residual correlation
#'   structures. If specified, this will automatically set up a model where the
#'   residual correlations for a specific level of `gr` are modelled
#'   hierarchically:
#'
#'   \eqn{\Omega_{group} = p\Omega_{global} + (1 - p)\Omega_{group, local}},
#'
#'   where \eqn{\Omega_{global}} is a *global* correlation matrix,
#'   \eqn{\Omega_{group, local}} is a *local deviation* correlation matrix, and
#'   \eqn{p} is a weighting parameter controlling how strongly the local
#'   correlation matrix \eqn{\Omega_{group}} is shrunk towards the global
#'   correlation matrix \eqn{\Omega_{global}}. If `gr` is supplied then `subgr`
#'   *must* also be supplied
#'
#' @param subgr A subgrouping `factor` variable specifying which element in
#'   `data` represents the different observational units. Defaults to `series`
#'   to be consistent with other functionalities in \pkg{mvgam}, though note
#'   that the data need not be time series in this case
#'
#'   Models that use the hierarchical correlations (by supplying a value for
#'   `gr`) *should not* include a `series` element in `data`. Rather, this
#'   element will be created internally based on the supplied variables for `gr`
#'   and `subgr`
#'
#'   For example, if you are modelling counts for a group of species (labelled
#'   as `species` in the data) across sampling sites (labelled as `site` in the
#'   data) in three different geographical regions (labelled as `region`), and
#'   you would like the residuals to be correlated within regions, then you
#'   should specify `unit = site`, `gr = region`, and `subgr = species`
#'
#'   Internally, `mvgam()` will appropriately order the data by `unit` (in this
#'   case, by `site`) and create the `series` element for the data using
#'   something like:
#'
#'   `series = as.factor(paste0(group, '_', subgroup))`
#'
#' @return An object of class \code{mvgam_trend}, which contains a list of
#'   arguments to be interpreted by the parsing functions in \pkg{mvgam}
#'
#' @examples
#' \donttest{
#' # Simulate counts of four species over ten sampling locations
#' set.seed(42)
#' site_dat <- data.frame(
#'   site = rep(1:10, 4),
#'   species = as.factor(sort(rep(letters[1:4], 10))),
#'   y = c(NA, rpois(39, 3))
#' )
#' head(site_dat)
#'
#' # Set up a correlated residual (i.e. Joint Species Distribution) model
#' trend_model <- ZMVN(unit = site, subgr = species)
#' mod <- mvgam(
#'   y ~ species,
#'   trend_model = ZMVN(unit = site, subgr = species),
#'   data = site_dat,
#'   chains = 2,
#'   silent = 2
#' )
#'
#' # Inspect the estimated species-species residual covariances
#' mcmc_plot(mod, variable = "Sigma", regex = TRUE, type = "hist")
#'
#' # A hierarchical correlation example
#' set.seed(789)
#' Sigma <- matrix(
#'   c(1, -0.4, 0.5,
#'     -0.4, 1, 0.3,
#'     0.5, 0.3, 1),
#'   byrow = TRUE,
#'   nrow = 3
#' )
#'
#' make_site_dat <- function(...) {
#'   errors <- mgcv::rmvn(
#'     n = 30,
#'     mu = c(0.6, 0.8, 1.8),
#'     V = Sigma
#'   )
#'   site_dat <- do.call(rbind, lapply(1:3, function(spec) {
#'     data.frame(
#'       y = rpois(30, lambda = exp(errors[, spec])),
#'       species = paste0("species", spec),
#'       site = 1:30
#'     )
#'   }))
#'   site_dat
#' }
#'
#' site_dat <- rbind(
#'   make_site_dat() %>%
#'     dplyr::mutate(group = "group1"),
#'   make_site_dat() %>%
#'     dplyr::mutate(group = "group2")
#' ) %>%
#'   dplyr::mutate(
#'     species = as.factor(species),
#'     group = as.factor(group)
#'   )
#'
#' # Fit the hierarchical correlated residual model
#' mod <- mvgam(
#'   y ~ species,
#'   trend_model = ZMVN(unit = site, gr = group, subgr = species),
#'   data = site_dat
#' )
#'
#' # Inspect the estimated species-species residual covariances
#' mcmc_plot(mod, variable = "Sigma", regex = TRUE, type = "hist")
#' }
#'
#' @export
ZMVN = function(unit = time, gr = NA, subgr = series) {
  # Validate the supplied groupings and correlation argument
  unit <- deparse0(substitute(unit))
  gr <- deparse0(substitute(gr))
  subgr <- deparse0(substitute(subgr))
  if (subgr == 'NA') {
    stop('argument "subgr" cannot be NA', call. = FALSE)
  }

  if (unit == 'NA') {
    stop('argument "unit" cannot be NA', call. = FALSE)
  }

  out <- structure(
    list(
      trend = 'ZMVN',
      ma = FALSE,
      cor = TRUE,
      unit = unit,
      gr = gr,
      subgr = subgr,
      label = match.call(),
      shared_innovations = TRUE  # ZMVN uses shared Gaussian innovation system
    ),
    class = 'mvgam_trend',
    param_info = list(
      param_names = c('trend', 'tau', 'sigma', 'theta', 'Sigma', 'error'),
      labels = c('trend_estimates', 'precision_parameter', 'standard_deviation', 'correlation_parameter', 'covariance_matrix', 'process_errors')
    )
  )
}
