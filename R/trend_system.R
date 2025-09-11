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
#' @param prior_spec Named list of prior specifications for trend parameters (optional)
#' @return Invisibly returns TRUE on successful registration
#' @export
register_trend_type <- function(name, supports_factors = FALSE, generator_func,
                               incompatibility_reason = NULL, prior_spec = NULL) {
  checkmate::assert_string(name, min.chars = 1)
  checkmate::assert_logical(supports_factors, len = 1)
  checkmate::assert_function(generator_func, args = c("trend_specs", "data_info"))
  checkmate::assert_list(prior_spec, null.ok = TRUE, names = "named")
  
  # Validate prior_spec structure if provided
  if (!is.null(prior_spec)) {
    for (param_name in names(prior_spec)) {
      param_spec <- prior_spec[[param_name]]
      if (!is.list(param_spec)) {
        stop(insight::format_error(
          paste0("Invalid prior specification for parameter {.field ", param_name, "}"),
          "Each prior specification must be a named list with 'default', 'bounds', and 'description' elements."
        ))
      }
      required_fields <- c("default", "bounds", "description")
      missing_fields <- setdiff(required_fields, names(param_spec))
      if (length(missing_fields) > 0) {
        stop(insight::format_error(
          paste0("Missing required fields in prior specification for {.field ", param_name, "}"),
          paste0("Missing: {.val ", paste(missing_fields, collapse = ", "), "}")
        ))
      }
    }
  }

  if (!supports_factors && is.null(incompatibility_reason)) {
    incompatibility_reason <- get_default_incompatibility_reason(name)
  }

  trend_registry[[name]] <- list(
    supports_factors = supports_factors,
    generator = generator_func,
    incompatibility_reason = incompatibility_reason,
    prior_spec = prior_spec
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

#' Auto-Register Trend Types Using Convention-Based Discovery
#'
#' @description
#' Automatically discovers and registers trend types based on naming conventions.
#' FAILS FAST with clear errors when conventions are not followed.
#'
#' **Convention**: For trend type "FOO", you MUST define:
#' - `generate_foo_trend_stanvars()` function for Stan code generation
#' - `foo_trend_properties()` function returning list(supports_factors = TRUE/FALSE, incompatibility_reason = "...")
#'
#' @return Invisibly returns TRUE, or STOPS with clear error on any failure
#' @noRd
auto_register_trend_types <- function() {
  # No input parameters to validate
  
  # Get all generate_*_trend_stanvars functions
  all_functions <- ls(getNamespace("mvgam"))
  generator_pattern <- "^generate_(.+)_trend_stanvars$"
  generator_functions <- grep(generator_pattern, all_functions, value = TRUE)
  
  if (length(generator_functions) == 0) {
    stop(insight::format_error(
      "No trend generator functions found.",
      "Expected functions like {.field generate_ar_trend_stanvars}, {.field generate_rw_trend_stanvars}, etc.",
      "Check that Stan assembly functions follow naming convention."
    ))
  }
  
  # Extract trend type names from function names
  trend_types <- gsub(generator_pattern, "\\1", generator_functions)
  trend_types <- toupper(trend_types)  # Convert to uppercase (AR, RW, VAR, etc.)
  
  # Register each discovered trend type
  for (i in seq_along(trend_types)) {
    trend_type <- trend_types[i]
    generator_func_name <- generator_functions[i]
    
    # Get the actual generator function
    generator_func <- get(generator_func_name, envir = getNamespace("mvgam"))
    
    # Get trend properties - REQUIRED, no defaults
    properties_func_name <- paste0(tolower(trend_type), "_trend_properties")
    
    if (!exists(properties_func_name, envir = getNamespace("mvgam"))) {
      stop(insight::format_error(
        "Missing required properties function for trend type {.field {trend_type}}.",
        "You must define {.field {properties_func_name}()} that returns list(supports_factors = TRUE/FALSE, incompatibility_reason = '...').",
        "This ensures explicit declaration of trend capabilities."
      ))
    }
    
    # Get properties function and call it
    properties_func <- get(properties_func_name, envir = getNamespace("mvgam"))
    trend_info <- properties_func()
    
    # Validate properties function output
    validate_trend_properties(trend_info, trend_type, properties_func_name)
    
    # Register with validated properties
    register_trend_type(
      name = trend_type,
      supports_factors = trend_info$supports_factors,
      generator_func = generator_func,
      incompatibility_reason = trend_info$incompatibility_reason
    )
  }
  
  invisible(TRUE)
}

#' Register Core Trend Types
#'
#' @description
#' Register all core mvgam trend types using auto-discovery.
#' Called automatically during package load.
#'
#' @return Invisibly returns TRUE
#' @noRd
register_core_trends <- function() {
  auto_register_trend_types()
}

#' Validate Trend Properties Function Output
#'
#' @description
#' Validates that a trend properties function returns the required structure.
#' FAILS FAST with clear errors for missing or invalid properties.
#'
#' @param trend_info Output from trend properties function
#' @param trend_type Trend type name for context
#' @param func_name Properties function name for context
#' @return Invisibly returns TRUE, or STOPS with clear error
#' @noRd
validate_trend_properties <- function(trend_info, trend_type, func_name) {
  # Input validation with checkmate
  checkmate::assert_string(trend_type, min.chars = 1)
  checkmate::assert_string(func_name, min.chars = 1)
  
  if (!is.list(trend_info)) {
    stop(insight::format_error(
      "Function {.field {func_name}()} must return a list.",
      "Got {.field {class(trend_info)}} instead.",
      "Fix: return list(supports_factors = TRUE/FALSE, incompatibility_reason = '...')"
    ))
  }
  
  required_fields <- c("supports_factors")
  missing_fields <- setdiff(required_fields, names(trend_info))
  
  if (length(missing_fields) > 0) {
    stop(insight::format_error(
      "Function {.field {func_name}()} missing required fields: {.field {paste(missing_fields, collapse = ', ')}}.",
      "Required structure: list(supports_factors = TRUE/FALSE, incompatibility_reason = '...')",
      "The supports_factors field is mandatory for trend type {.field {trend_type}}."
    ))
  }
  
  if (!is.logical(trend_info$supports_factors) || length(trend_info$supports_factors) != 1) {
    stop(insight::format_error(
      "Field {.field supports_factors} must be a single logical value (TRUE or FALSE).",
      "Got {.field {trend_info$supports_factors}} of type {.field {class(trend_info$supports_factors)}}.",
      "Fix {.field {func_name}()} to return supports_factors = TRUE or FALSE."
    ))
  }
  
  if (!trend_info$supports_factors && 
      (is.null(trend_info$incompatibility_reason) || !is.character(trend_info$incompatibility_reason))) {
    stop(insight::format_error(
      "Trends with {.field supports_factors = FALSE} must provide {.field incompatibility_reason}.",
      "Fix {.field {func_name}()} to include incompatibility_reason = 'explanation why factors not supported'.",
      "This helps users understand why factor models don't work with {.field {trend_type}} trends."
    ))
  }
  
  invisible(TRUE)
}

# Core Trend Properties Functions
# These define the capabilities of each built-in trend type

#' AR Trend Properties
#' @noRd
ar_trend_properties <- function() {
  list(
    supports_factors = TRUE,
    incompatibility_reason = NULL
  )
}

#' RW Trend Properties  
#' @noRd
rw_trend_properties <- function() {
  list(
    supports_factors = TRUE,
    incompatibility_reason = NULL
  )
}

#' VAR Trend Properties
#' @noRd
var_trend_properties <- function() {
  list(
    supports_factors = TRUE,
    incompatibility_reason = NULL
  )
}

#' ZMVN Trend Properties
#' @noRd
zmvn_trend_properties <- function() {
  list(
    supports_factors = TRUE,
    incompatibility_reason = NULL
  )
}

#' CAR Trend Properties
#' @noRd
car_trend_properties <- function() {
  list(
    supports_factors = FALSE,
    incompatibility_reason = "Continuous-time AR requires series-specific irregular time intervals, incompatible with factor structure"
  )
}

#' PW Trend Properties
#' @noRd
pw_trend_properties <- function() {
  list(
    supports_factors = FALSE,
    incompatibility_reason = "Piecewise trends require series-specific changepoint modeling, incompatible with factor structure"
  )
}

#' Register Custom Trend Type
#'
#' @description
#' User-facing function to register custom trend types. For maximum future-proofing,
#' consider using the convention-based approach instead (see Details).
#'
#' @details
#' **Recommended Convention-Based Approach**:
#' 
#' Instead of calling this function, define these functions and let auto-discovery handle registration:
#' 1. `generate_mytrend_trend_stanvars(trend_specs, data_info)` - Stan code generator
#' 2. `mytrend_trend_properties()` - Returns list(supports_factors = TRUE/FALSE, incompatibility_reason = "...")
#' 
#' This approach requires zero manual registration calls and is automatically future-proof.
#'
#' @param name Character string name for the custom trend type
#' @param supports_factors Logical indicating if the trend supports factor models
#' @param generator_func Function that generates Stan code for this trend type.
#'   Must accept arguments (trend_specs, data_info) and return list of stanvars.
#' @param incompatibility_reason Optional character string explaining why factor
#'   models aren't supported (if supports_factors = FALSE)
#'
#' @return Invisibly returns TRUE on successful registration
#' @export
#'
#' @examples
#' \dontrun{
#' # RECOMMENDED: Convention-based approach (auto-discovered)
#' generate_garch_trend_stanvars <- function(trend_specs, data_info) {
#'   # GARCH trend implementation
#'   list(brms::stanvar(...))
#' }
#' 
#' garch_trend_properties <- function() {
#'   list(
#'     supports_factors = TRUE,
#'     incompatibility_reason = NULL
#'   )
#' }
#' # No registration call needed! Auto-discovered on package load.
#' 
#' # ALTERNATIVE: Manual registration (discouraged)
#' register_custom_trend("GARCH",
#'                      supports_factors = TRUE,
#'                      generator_func = generate_garch_trend_stanvars)
#' }
register_custom_trend <- function(name, supports_factors = FALSE, generator_func,
                                 incompatibility_reason = NULL) {
  # Input validation with checkmate
  checkmate::assert_string(name, min.chars = 1)
  checkmate::assert_logical(supports_factors, len = 1)
  checkmate::assert_function(generator_func)
  checkmate::assert_string(incompatibility_reason, null.ok = TRUE)

  # Guide users toward convention-based approach
  rlang::inform(
    c(
      paste0("Registering custom trend type: ", name),
      "i" = "Consider using convention-based approach for future-proofing:",
      "i" = paste0("Define generate_", tolower(name), "_trend_stanvars() and ", tolower(name), "_trend_properties()"),
      "i" = "This eliminates the need for manual registration calls."
    ),
    .frequency = "once",
    .frequency_id = paste0("convention_guide_", name)
  )

  # Check for existing registration
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

# -----------------------------------------------------------------------------
# Monitor Parameter Generation for Trend Objects
# -----------------------------------------------------------------------------

#' Generate Monitor Parameters for Trend Objects
#'
#' Automatically discovers which parameters should be monitored for a given
#' trend specification based on the trend type and configuration.
#' This implements the metadata storage design from Step 1.3.
#'
#' @param trend_spec An mvgam_trend object
#' @return Character vector of parameter names to monitor
#' @noRd
generate_monitor_params <- function(trend_spec) {
  checkmate::assert_list(trend_spec, min.len = 1)

  # Extract trend type (normalize for registry lookup)
  trend_type <- normalize_trend_type(trend_spec$trend %||% trend_spec$trend_type)

  # Base parameters that most trends share
  base_params <- c("sigma_trend")

  # Trend-specific parameters
  trend_specific <- switch(trend_type,
    "RW" = generate_rw_monitor_params(trend_spec),
    "AR" = generate_ar_monitor_params(trend_spec),
    "VAR" = generate_var_monitor_params(trend_spec),
    "CAR" = generate_car_monitor_params(trend_spec),
    "ZMVN" = generate_zmvn_monitor_params(trend_spec),
    "PW" = generate_pw_monitor_params(trend_spec),
    stop(insight::format_error(
      "Unknown trend type: {.field {trend_type}}",
      "Supported types: RW, AR, VAR, CAR, ZMVN, PW"
    ))
  )

  # Add correlation parameters if enabled
  # Note: Sigma_trend is computed from sigma_trend * L_Omega_trend in Stan,
  # so users should not place priors on it directly
  correlation_params <- if (trend_spec$cor %||% FALSE) {
    "L_Omega_trend"
  } else {
    character(0)
  }

  # Add factor model parameters if enabled
  factor_params <- if (!is.null(trend_spec$n_lv)) {
    c("Z")
  } else {
    character(0)
  }

  # Combine all parameters
  all_params <- unique(c(base_params, trend_specific, correlation_params, factor_params))

  return(all_params)
}

#' Generate RW-specific monitor parameters
#' @param trend_spec RW trend specification
#' @return Character vector of RW-specific parameters
#' @noRd
generate_rw_monitor_params <- function(trend_spec) {
  params <- character(0)

  # MA parameters if enabled
  if (trend_spec$ma %||% FALSE) {
    params <- c(params, "theta1_trend")
  }

  return(params)
}

#' Generate AR-specific monitor parameters
#' @param trend_spec AR trend specification
#' @return Character vector of AR-specific parameters
#' @noRd
generate_ar_monitor_params <- function(trend_spec) {
  # AR coefficients for each lag
  lags <- trend_spec$p %||% trend_spec$lags %||% 1
  if (is.list(lags)) lags <- unlist(lags)

  # Handle different lag specifications correctly:
  # p=2 means AR(2) with lags 1:2 (standard AR model interpretation)
  # p=c(1,3) means specific lags 1,3 (sparse AR model)
  if (length(lags) == 1 && is.numeric(lags)) {
    # Single integer: create standard AR(p) with all lags 1:p
    ar_params <- paste0("ar", 1:lags, "_trend")
  } else {
    # Vector or multiple values: use specific lags only
    ar_params <- paste0("ar", lags, "_trend")
  }

  return(ar_params)
}

#' Generate VAR-specific monitor parameters
#' @param trend_spec VAR trend specification
#' @return Character vector of VAR-specific parameters
#' @noRd
generate_var_monitor_params <- function(trend_spec) {
  # VAR uses hierarchical hyperparameters following Heaps 2022 methodology
  
  # Base VAR hyperparameters - always present
  var_params <- c("Amu_trend", "Aomega_trend")
  
  # Add MA hyperparameters if VARMA model
  if (trend_spec$ma %||% FALSE) {
    var_params <- c(var_params, "Dmu_trend", "Domega_trend")
  }
  
  # Add group-specific parameters if hierarchical
  if (!is.null(trend_spec$gr) && trend_spec$gr != "NA") {
    var_params <- c(var_params, "sigma_group_trend")
  }
  
  # VAR uses variance-correlation decomposition (sigma_trend + L_Omega_trend)
  # These are handled by base_params and correlation_params in the main function
  
  return(var_params)
}

#' Generate CAR-specific monitor parameters
#' @param trend_spec CAR trend specification
#' @return Character vector of CAR-specific parameters
#' @noRd
generate_car_monitor_params <- function(trend_spec) {
  # CAR uses ar1_trend following standard _trend suffix convention
  return(c("ar1_trend"))
}

#' Generate ZMVN-specific monitor parameters
#' @param trend_spec ZMVN trend specification
#' @return Character vector of ZMVN-specific parameters
#' @noRd
generate_zmvn_monitor_params <- function(trend_spec) {
  # ZMVN only needs base variance/correlation parameters
  return(character(0))
}

#' Generate PW-specific monitor parameters
#' @param trend_spec PW trend specification
#' @return Character vector of PW-specific parameters
#' @noRd
generate_pw_monitor_params <- function(trend_spec) {
  # Piecewise trend parameters
  return(c("k_trend", "m_trend", "delta_trend"))
}

#' Normalize trend type for consistent lookup
#' @param trend_type Raw trend type from constructor
#' @return Normalized trend type for registry lookup
#' @noRd
normalize_trend_type <- function(trend_type) {
  # Remove lag specifications: AR1 -> AR, VAR2 -> VAR
  gsub("\\d+|\\(.*\\)", "", trend_type)
}

#' Add Monitor Parameters to Trend Object
#'
#' Enhances an existing trend object with auto-discovered monitor parameters.
#' This implements the metadata storage design from Step 1.3.
#'
#' @param trend_obj An mvgam_trend object
#' @return Enhanced trend object with monitor_params field
#' @noRd
add_monitor_params <- function(trend_obj) {
  checkmate::assert_list(trend_obj)

  # Generate monitor parameters
  monitor_params <- generate_monitor_params(trend_obj)

  # Add to trend object
  trend_obj$monitor_params <- monitor_params

  return(trend_obj)
}

# -----------------------------------------------------------------------------
# Ultra-Efficient Forecast Metadata Generation
# -----------------------------------------------------------------------------

#' Generate Forecast Metadata for Ultra-Fast Dispatch
#'
#' Creates minimal forecast metadata optimized for maximum runtime speed.
#' Stores only function name and required parameter list for lazy extraction
#' and zero-overhead forecasting calls.
#'
#' @param trend_spec An mvgam_trend object
#' @return List with function_name and required_params for efficient dispatch
#' @noRd
generate_forecast_metadata <- function(trend_spec) {
  checkmate::assert_list(trend_spec, min.len = 1)

  # Extract and normalize trend type
  trend_type <- normalize_trend_type(trend_spec$trend %||% trend_spec$trend_type)

  # Convention-based function naming: "AR" → forecast_ar_rcpp
  function_name <- paste0("forecast_", tolower(trend_type), "_rcpp")

  # Get minimal required parameters for lazy extraction
  required_params <- generate_forecast_required_params(trend_spec, trend_type)

  return(list(
    function_name = function_name,
    required_params = required_params
  ))
}

#' Generate Required Parameters for Ultra-Fast Forecasting
#'
#' Determines minimal set of parameters needed for forecasting dispatch.
#' Always a subset of monitor_params optimized for fast extraction.
#'
#' @param trend_spec An mvgam_trend object
#' @param trend_type Normalized trend type
#' @return Character vector of minimal required parameter names
#' @noRd
generate_forecast_required_params <- function(trend_spec, trend_type) {
  # Get all monitor parameters
  all_monitor_params <- generate_monitor_params(trend_spec)

  # Filter to minimal required set for each trend type
  switch(trend_type,
    "RW" = filter_rw_forecast_params(all_monitor_params, trend_spec),
    "AR" = filter_ar_forecast_params(all_monitor_params, trend_spec),
    "VAR" = filter_var_forecast_params(all_monitor_params, trend_spec),
    "CAR" = filter_car_forecast_params(all_monitor_params, trend_spec),
    "ZMVN" = filter_zmvn_forecast_params(all_monitor_params, trend_spec),
    "PW" = filter_pw_forecast_params(all_monitor_params, trend_spec),
    stop(insight::format_error(
      "Unknown trend type for forecasting: {.field {trend_type}}",
      "Supported types: RW, AR, VAR, CAR, ZMVN, PW"
    ))
  )
}

#' Filter RW parameters for minimal forecasting requirements
#' @param monitor_params All monitor parameters for RW
#' @param trend_spec RW trend specification
#' @return Minimal required parameters for fast RW forecasting
#' @noRd
filter_rw_forecast_params <- function(monitor_params, trend_spec) {
  # RW minimally needs: variance + optional MA + correlation
  required <- "sigma_trend"

  # Add essential extras present in monitor_params
  extras <- intersect(monitor_params, c("theta1_trend", "L_Omega_trend", "Sigma_trend"))
  c(required, extras)
}

#' Filter AR parameters for minimal forecasting requirements
#' @param monitor_params All monitor parameters for AR
#' @param trend_spec AR trend specification
#' @return Minimal required parameters for fast AR forecasting
#' @noRd
filter_ar_forecast_params <- function(monitor_params, trend_spec) {
  # AR minimally needs: coefficients + variance
  ar_coeffs <- monitor_params[grepl("^ar\\d+_trend$", monitor_params)]
  required <- c(ar_coeffs, "sigma_trend")

  # Add correlation if present
  correlation_params <- intersect(monitor_params, c("L_Omega_trend", "Sigma_trend"))
  c(required, correlation_params)
}

#' Filter VAR parameters for minimal forecasting requirements
#' @param monitor_params All monitor parameters for VAR
#' @param trend_spec VAR trend specification
#' @return Minimal required parameters for fast VAR forecasting
#' @noRd
filter_var_forecast_params <- function(monitor_params, trend_spec) {
  # VAR minimally needs: coefficient matrices + covariance
  var_matrices <- monitor_params[grepl("^A_trend\\[", monitor_params)]
  c(var_matrices, "Sigma_trend")
}

#' Filter CAR parameters for minimal forecasting requirements
#' @param monitor_params All monitor parameters for CAR
#' @param trend_spec CAR trend specification
#' @return Minimal required parameters for fast CAR forecasting
#' @noRd
filter_car_forecast_params <- function(monitor_params, trend_spec) {
  # CAR minimally needs: AR coefficient + variance
  c("ar1", "sigma_trend")
}

#' Filter ZMVN parameters for minimal forecasting requirements
#' @param monitor_params All monitor parameters for ZMVN
#' @param trend_spec ZMVN trend specification
#' @return Minimal required parameters for fast ZMVN forecasting
#' @noRd
filter_zmvn_forecast_params <- function(monitor_params, trend_spec) {
  # ZMVN minimally needs: variance + optional correlation
  required <- "sigma_trend"
  correlation_params <- intersect(monitor_params, c("L_Omega_trend", "Sigma_trend"))
  c(required, correlation_params)
}

#' Filter PW parameters for minimal forecasting requirements
#' @param monitor_params All monitor parameters for PW
#' @param trend_spec PW trend specification
#' @return Minimal required parameters for fast PW forecasting
#' @noRd
filter_pw_forecast_params <- function(monitor_params, trend_spec) {
  # PW minimally needs: all growth parameters
  c("k_trend", "m_trend", "delta_trend")
}

#' Add Forecast Metadata to Trend Object
#'
#' Enhances trend object with ultra-efficient forecast dispatch metadata.
#' Optimized for maximum runtime speed with minimal storage overhead.
#'
#' @param trend_obj An mvgam_trend object
#' @return Enhanced trend object with forecast_metadata field
#' @noRd
add_forecast_metadata <- function(trend_obj) {
  checkmate::assert_list(trend_obj)

  # Generate minimal forecast metadata for fast dispatch
  forecast_metadata <- generate_forecast_metadata(trend_obj)

  # Add to trend object
  trend_obj$forecast_metadata <- forecast_metadata

  return(trend_obj)
}

# -----------------------------------------------------------------------------
# Summary Labels Generation for User-Friendly Parameter Display
# -----------------------------------------------------------------------------

#' Generate Summary Labels for Trend Parameters
#'
#' Creates user-friendly display labels for trend parameters in summaries.
#' Maps technical parameter names to descriptive labels for better readability.
#'
#' @param trend_spec An mvgam_trend object
#' @return Named character vector mapping parameter names to display labels
#' @noRd
generate_summary_labels <- function(trend_spec) {
  checkmate::assert_list(trend_spec, min.len = 1)

  # Get monitor parameters to create labels for
  monitor_params <- generate_monitor_params(trend_spec)

  # Extract trend type for context
  trend_type <- normalize_trend_type(trend_spec$trend %||% trend_spec$trend_type)

  # Generate labels for each monitor parameter
  labels <- character(length(monitor_params))
  names(labels) <- monitor_params

  for (param in monitor_params) {
    labels[param] <- generate_parameter_label(param, trend_type, trend_spec)
  }

  return(labels)
}

#' Generate User-Friendly Label for Individual Parameter
#'
#' Creates descriptive label for a single parameter based on its name and context.
#'
#' @param param_name Technical parameter name (e.g., "ar1_trend", "sigma_trend")
#' @param trend_type Normalized trend type
#' @param trend_spec Complete trend specification for context
#' @return Character string with user-friendly label
#' @noRd
generate_parameter_label <- function(param_name, trend_type, trend_spec) {
  # Handle common parameter patterns using if-else for proper character assignment
  if (param_name == "sigma_trend") {
    return("Trend innovation standard deviation")
  } else if (param_name == "Sigma_trend") {
    return("Trend innovation covariance matrix")
  } else if (param_name == "L_Omega_trend") {
    return("Trend correlation matrix (Cholesky factor)")
  } else if (grepl("^ar\\d+_trend$", param_name)) {
    lag <- gsub("ar(\\d+)_trend", "\\1", param_name)
    return(paste0("AR(", lag, ") coefficient"))
  } else if (grepl("^A_trend\\[", param_name)) {
    lag <- gsub("A_trend\\[(\\d+)\\]", "\\1", param_name)
    return(paste0("VAR coefficient matrix (lag ", lag, ")"))
  } else if (grepl("^theta\\d+_trend$", param_name)) {
    lag <- gsub("theta(\\d+)_trend", "\\1", param_name)
    return(paste0("MA(", lag, ") coefficient"))
  } else if (param_name == "k_trend") {
    return("Piecewise growth rate")
  } else if (param_name == "m_trend") {
    return("Piecewise offset parameter")
  } else if (param_name == "delta_trend") {
    return("Piecewise changepoint adjustments")
  } else if (param_name == "ar1") {
    return("CAR(1) coefficient")
  } else if (param_name == "Z") {
    return("Factor loadings matrix")
  } else if (grepl("_group", param_name)) {
    return(gsub("_", " ", gsub("_trend", "", param_name)))
  } else {
    # Default: clean up technical name
    clean_name <- gsub("_trend$", "", param_name)
    clean_name <- gsub("_", " ", clean_name)
    # Capitalize first letter
    return(paste0(toupper(substring(clean_name, 1, 1)), substring(clean_name, 2)))
  }
}

#' Add Summary Labels to Trend Object
#'
#' Enhances trend object with user-friendly parameter display labels.
#' Enables clean parameter presentation in summaries and print methods.
#'
#' @param trend_obj An mvgam_trend object
#' @return Enhanced trend object with summary_labels field
#' @noRd
add_summary_labels <- function(trend_obj) {
  checkmate::assert_list(trend_obj)

  # Generate summary labels
  summary_labels <- generate_summary_labels(trend_obj)

  # Add to trend object
  trend_obj$summary_labels <- summary_labels

  return(trend_obj)
}

#' Add Complete Metadata to Trend Object
#'
#' Convenience function that adds all metadata components:
#' monitor_params, forecast_metadata, and summary_labels.
#'
#' @param trend_obj An mvgam_trend object
#' @return Fully enhanced trend object with all metadata
#' @noRd
add_complete_metadata <- function(trend_obj) {
  # Add all metadata components sequentially
  trend_obj <- add_monitor_params(trend_obj)
  trend_obj <- add_forecast_metadata(trend_obj)
  trend_obj <- add_summary_labels(trend_obj)
  return(trend_obj)
}

# =============================================================================
# SECTION 3: ENHANCED MVGAM_TREND OBJECT SPECIFICATION
# =============================================================================

#' Enhanced mvgam_trend Object Field Specification
#'
#' @description
#' Comprehensive documentation of required and optional fields for the enhanced
#' mvgam_trend S3 class structure. This specification enables self-contained
#' trend objects that provide all necessary information for validation,
#' Stan code generation, and post-processing without external lookups.
#'
#' @section Core Required Fields:
#' \describe{
#'   \item{trend}{Character string. Normalized trend type name for stanvar
#'     generation dispatch. Examples: "AR", "VAR", "RW", "CAR", "PW", "ZMVN".
#'     Used in convention-based lookup: "AR" → generate_ar_trend_stanvars()}
#'   \item{time}{Character string. Name of time variable in user's data.
#'     Default "time" with warning when not explicitly specified.}
#'   \item{series}{Character string. Name of series identifier variable in data.
#'     Default "series" with warning when not explicitly specified.}
#'   \item{class}{Must include "mvgam_trend" for method dispatch.}
#' }
#'
#' @section Self-Contained Validation Fields:
#' \describe{
#'   \item{validation_rules}{Character vector. Validation rules this trend requires.
#'     Replaces hard-coded conditionals throughout validation functions.
#'     Must use approved strings from validation_rule_vocabulary.
#'     Examples: c("requires_regular_intervals", "supports_factors", "supports_hierarchical")}
#' }
#'
#' @section Self-Contained Parameter Monitoring Fields:
#' \describe{
#'   \item{monitor_params}{Character vector. Stan parameters to track post-fit.
#'     Automatically includes response suffixes in multivariate contexts.
#'     Examples: c("ar1_trend", "sigma_trend", "L_Omega_trend")}
#'   \item{tpars}{Character vector. All trend-specific parameter names with
#'     "_trend" suffix for Stan compatibility. Generated from param_info.}
#'   \item{bounds}{Named list. Parameter bounds for prior specification.
#'     Format: list(ar1_trend = c(-1, 1), sigma_trend = c(0, Inf))}
#' }
#'
#' @section Self-Contained Forecasting Metadata Fields:
#' \describe{
#'   \item{forecast_metadata}{List. Complete forecasting function information:
#'     \itemize{
#'       \item{function_name}{Character. Forecasting function name (e.g., "forecast_ar_rcpp")}
#'       \item{required_args}{Character vector. Required arguments from fitted model}
#'       \item{max_horizon}{Integer. Maximum forecasting steps supported}
#'       \item{dependencies}{Character vector. Requirements like "needs_last_state"}
#'     }}
#' }
#'
#' @section Configuration Parameters (Trend-Specific):
#' \describe{
#'   \item{p}{Integer or vector. Order parameter for AR/VAR models.
#'     Examples: 1 (AR1), c(1,12) (seasonal AR), 2 (VAR2)}
#'   \item{ma}{Logical. Whether moving average terms are included.}
#'   \item{cor}{Logical. Whether correlation structure is enabled.
#'     VAR models always set this to TRUE for optimal performance.}
#'   \item{gr}{Character string. Grouping variable name for hierarchical models.
#'     "NA" indicates no grouping.}
#'   \item{subgr}{Character string. Subgrouping variable name.
#'     Default "series" but can be customized for hierarchical models.}
#'   \item{n_lv}{Integer. Number of latent variables for factor models.
#'     Only allowed when validation_rules includes "supports_factors".}
#'   \item{cap}{Character string. Carrying capacity variable for logistic growth.
#'     Required for PW models with growth = "logistic".}
#'   \item{growth}{Character string. Growth type for piecewise models:
#'     "linear" or "logistic".}
#'   \item{n_changepoints}{Integer. Number of changepoints for piecewise models.}
#'   \item{changepoint_range}{Numeric. Proportion of history for changepoints.}
#'   \item{changepoint_scale}{Numeric. Scale parameter for changepoint priors.}
#' }
#'
#' @section Display and Documentation Fields:
#' \describe{
#'   \item{label}{Character string. Human-readable description for printing.
#'     Auto-generated from trend type and parameters if not provided.}
#'   \item{summary_labels}{List. Naming patterns for parameter summaries:
#'     \itemize{
#'       \item{parameter_labels}{List mapping parameter names to display labels}
#'       \item{factor_labels}{List for factor loading label patterns}
#'       \item{group_labels}{List for hierarchical parameter labels}
#'     }}
#' }
#'
#' @section Internal Processing Fields:
#' \describe{
#'   \item{param_info}{List containing:
#'     \itemize{
#'       \item{parameters}{trend_param object with complete specifications}
#'       \item{characteristics}{List of trend capabilities and settings}
#'     }}
#'   \item{shared_innovations}{Logical. Whether trend uses shared Gaussian
#'     innovation system (TRUE) or handles own innovations (FALSE).
#'     Most trends use shared system; exceptions: CAR, VAR, PW.}
#'   \item{dimensions}{List. Pre-calculated time series dimensions (populated during validation):
#'     \itemize{
#'       \item{n_time}{Integer. Number of time points}
#'       \item{n_series}{Integer. Number of series}
#'       \item{n_obs}{Integer. Total observations}
#'       \item{time_var}{Character. Time variable name}
#'       \item{series_var}{Character. Series variable name}
#'       \item{time_range}{Numeric vector. c(min_time, max_time)}
#'       \item{unique_times}{Vector. All unique time values}
#'     }}
#'   \item{response_context}{Character string. Response name for multivariate models.
#'     NULL for univariate models, populated during multivariate parsing.}
#' }
#'
#' @section Field Relationships and Validation Rules:
#' The validation_rules field determines which other fields are valid:
#' \itemize{
#'   \item{"supports_factors" + n_lv}: Factor models allowed
#'   \item{"incompatible_with_factors" + n_lv}: Error thrown
#'   \item{"supports_hierarchical" + gr/subgr}: Hierarchical models allowed
#'   \item{"requires_regular_intervals"}: Regular time validation triggered
#'   \item{"allows_irregular_intervals"}: CAR-style irregular time handling
#' }
#'
#' @section Convention-Based Function Dispatch:
#' The trend field enables automatic function lookup:
#' \itemize{
#'   \item{Stan generation}: "AR" → generate_ar_trend_stanvars()
#'   \item{Forecasting}: forecast_metadata$function_name → that function
#'   \item{No manual registry entries needed}
#' }
#'
#' @section Response Suffix Handling (Multivariate):
#' In multivariate contexts, certain fields are automatically modified:
#' \itemize{
#'   \item{monitor_params}: "_count", "_biomass" suffixes added to parameter names
#'   \item{summary_labels}: Response-specific labels generated automatically
#'   \item{response_context}: Set to response name for tracking
#' }
#'
#' @section Backward Compatibility:
#' During transition period, old fields may still be present:
#' \itemize{
#'   \item{trend_model}: Legacy field, use trend instead
#'   \item{trend_type}: Legacy field, use trend instead
#'   \item{forecast_fun}: Legacy field, use forecast_metadata$function_name
#'   \item{stancode_fun}: Legacy field, replaced by convention-based lookup
#'   \item{standata_fun}: Legacy field, replaced by convention-based lookup
#' }
#'
#' @section Class Structure Requirements:
#' Objects must:
#' \itemize{
#'   \item{Have class c("mvgam_trend")}
#'   \item{Pass validate_mvgam_trend() checks}
#'   \item{Include all required core fields}
#'   \item{Use approved validation_rules vocabulary}
#'   \item{Have consistent field types and relationships}
#' }
#'
#' @section Example Complete Object:
#' \preformatted{
#' ar_trend <- structure(list(
#'   # Core required fields
#'   trend = "AR",
#'   time = "time",
#'   series = "series",
#'
#'   # Self-contained validation
#'   validation_rules = c(
#'     "requires_regular_intervals",
#'     "supports_factors",
#'     "supports_hierarchical"
#'   ),
#'
#'   # Self-contained monitoring
#'   monitor_params = c("ar1_trend", "sigma_trend"),
#'   tpars = c("ar1_trend"),
#'   bounds = list(ar1_trend = c(-1, 1)),
#'
#'   # Self-contained forecasting
#'   forecast_metadata = list(
#'     function_name = "forecast_ar_rcpp",
#'     required_args = c("ar_coefficients", "last_state"),
#'     max_horizon = Inf,
#'     dependencies = c("needs_ar_coeffs")
#'   ),
#'
#'   # Configuration
#'   p = 1, ma = FALSE, cor = FALSE, n_lv = NULL,
#'
#'   # Auto-generated during processing
#'   label = "AR1",
#'   shared_innovations = TRUE,
#'   param_info = list(...)
#' ), class = "mvgam_trend")
#' }
#'
#' @section Design Principles:
#' \itemize{
#'   \item{Self-contained}: Each object contains all needed metadata}
#'   \item{Convention-based}: Minimal configuration, maximum automation}
#'   \item{Extensible}: New fields can be added without breaking existing trends}
#'   \item{Validated}: Structure enforced through validate_mvgam_trend()}
#'   \item{Consistent}: All trends follow identical patterns}
#' }
#'
#' @name mvgam_trend_specification
#' @author Nicholas J Clark
NULL

#' Validation Rules Vocabulary
#'
#' @description
#' Central vocabulary of approved validation rule strings. These constants
#' prevent typos and provide clear documentation of available validation
#' behaviors. All validation_rules fields in mvgam_trend objects must use
#' strings from this vocabulary.
#'
#' @section Time Series Validation Rules:
#' \describe{
#'   \item{rule_requires_regular_intervals}{"requires_regular_intervals" - Trend
#'     requires evenly spaced time points. Used by: AR, VAR, RW, ZMVN, PW.
#'     Triggers: validate_regular_time_intervals()}
#'   \item{rule_allows_irregular_intervals}{"allows_irregular_intervals" - Trend
#'     can handle irregular time spacing. Used by: CAR only.
#'     Triggers: calculate_car_time_distances(), skips regular interval validation}
#' }
#'
#' @section Factor Model Validation Rules:
#' \describe{
#'   \item{rule_supports_factors}{"supports_factors" - Trend compatible with
#'     factor models (n_lv parameter allowed). Used by: AR, VAR, RW, ZMVN.
#'     Triggers: validate_factor_compatibility(), allows n_lv specification}
#'   \item{rule_incompatible_with_factors}{"incompatible_with_factors" - Trend
#'     cannot be used with factor models. Used by: CAR, PW.
#'     Triggers: Error when n_lv is specified}
#' }
#'
#' @section Hierarchical Model Validation Rules:
#' \describe{
#'   \item{rule_supports_hierarchical}{"supports_hierarchical" - Trend supports
#'     gr/subgr grouping parameters. Used by: AR, VAR, RW, ZMVN.
#'     Triggers: validate_grouping_structure(), allows gr/subgr specification}
#'   \item{rule_requires_hierarchical}{"requires_hierarchical" - Trend requires
#'     grouping structure. Currently unused, reserved for future trends.}
#' }
#'
#' @section Seasonal Model Validation Rules:
#' \describe{
#'   \item{rule_requires_seasonal_period}{"requires_seasonal_period" - Trend
#'     requires specification of seasonal period parameter. Used by: seasonal AR models.
#'     Triggers: validate_seasonal_period_specification()}
#'   \item{rule_supports_multiple_seasonality}{"supports_multiple_seasonality" - Trend
#'     can handle multiple seasonal periods simultaneously. Used by: flexible seasonal models.
#'     Triggers: validate_multiple_seasonal_periods()}
#'   \item{rule_incompatible_with_seasonal_smooths}{"incompatible_with_seasonal_smooths" - Trend
#'     conflicts with seasonal smooth terms in observation formula. Used by: trends with built-in seasonality.
#'     Triggers: Error when seasonal smooths detected in observation formula}
#' }
#'
#' @section Data Structure Validation Rules:
#' \describe{
#'   \item{rule_requires_balanced_panels}{"requires_balanced_panels" - All series
#'     must have observations at all time points. Used by: some multivariate models.
#'     Triggers: validate_balanced_panel_structure()}
#'   \item{rule_requires_minimum_series_count}{"requires_minimum_series_count" - Trend
#'     requires minimum number of series for identification. Used by: factor models, some VAR specifications.
#'     Triggers: validate_minimum_series_count()}
#' }
#'
#' @section Usage in Trend Constructors:
#' Use these constants when creating validation_rules vectors:
#' \preformatted{
#' # Example: Standard AR trend supports regular intervals, factors, and hierarchical models
#' validation_rules <- c(
#'   rule_requires_regular_intervals,
#'   rule_supports_factors,
#'   rule_supports_hierarchical
#' )
#'
#' # Example: Seasonal AR trend with multiple periods
#' validation_rules <- c(
#'   rule_requires_regular_intervals,
#'   rule_supports_factors,
#'   rule_supports_hierarchical,
#'   rule_supports_multiple_seasonality
#' )
#'
#' # Example: CAR trend allows irregular intervals but no factors/hierarchy
#' validation_rules <- c(
#'   rule_allows_irregular_intervals,
#'   rule_incompatible_with_factors
#' )
#'
#' # Example: Factor model requiring minimum series count
#' validation_rules <- c(
#'   rule_requires_regular_intervals,
#'   rule_supports_factors,
#'   rule_requires_minimum_series_count
#' )
#'
#' # Or use pre-built combinations:
#' validation_rules <- stationary_trend_rules
#' validation_rules <- seasonal_trend_rules
#' }
#'
#' @section Rule Interpreter Integration:
#' These constants map to validation functions in the rule interpreter:
#' \itemize{
#'   \item{rule_requires_regular_intervals → validate_regular_time_intervals()}
#'   \item{rule_supports_factors → validate_factor_compatibility()}
#'   \item{rule_supports_hierarchical → validate_grouping_structure()}
#'   \item{rule_incompatible_with_factors → error when n_lv specified}
#' }
#'
#' @name validation_rules_vocabulary
#' @author Nicholas J Clark
NULL

# Validation Rules Constants (grouped by category)
# Time Series Rules
rule_requires_regular_intervals <- "requires_regular_intervals"
rule_allows_irregular_intervals <- "allows_irregular_intervals"

# Factor Model Rules
rule_supports_factors <- "supports_factors"
rule_incompatible_with_factors <- "incompatible_with_factors"

# Hierarchical Model Rules
rule_supports_hierarchical <- "supports_hierarchical"
rule_requires_hierarchical <- "requires_hierarchical"
rule_incompatible_with_hierarchical <- "incompatible_with_hierarchical"

# Seasonal Model Rules
rule_requires_seasonal_period <- "requires_seasonal_period"
rule_supports_multiple_seasonality <- "supports_multiple_seasonality"
rule_incompatible_with_seasonal_smooths <- "incompatible_with_seasonal_smooths"

# Data Structure Rules
rule_requires_balanced_panels <- "requires_balanced_panels"
rule_requires_minimum_series_count <- "requires_minimum_series_count"

# Complete validation rules vocabulary for validation
validation_rule_vocabulary <- c(
  # Time rules
  rule_requires_regular_intervals,
  rule_allows_irregular_intervals,
  # Factor rules
  rule_supports_factors,
  rule_incompatible_with_factors,
  # Hierarchy rules
  rule_supports_hierarchical,
  rule_requires_hierarchical,
  # Seasonal rules
  rule_requires_seasonal_period,
  rule_supports_multiple_seasonality,
  rule_incompatible_with_seasonal_smooths,
  # Data structure rules
  rule_requires_balanced_panels,
  rule_requires_minimum_series_count
)

# Pre-built rule combinations for common trend patterns
stationary_trend_rules <- c(
  rule_requires_regular_intervals,
  rule_supports_factors,
  rule_supports_hierarchical
)

irregular_trend_rules <- c(
  rule_allows_irregular_intervals,
  rule_incompatible_with_factors
)

changepoint_trend_rules <- c(
  rule_requires_regular_intervals,
  rule_incompatible_with_factors
)

seasonal_trend_rules <- c(
  rule_requires_regular_intervals,
  rule_supports_factors,
  rule_supports_hierarchical,
  rule_requires_seasonal_period
)

factor_model_trend_rules <- c(
  rule_requires_regular_intervals,
  rule_supports_factors,
  rule_requires_minimum_series_count
)

# =============================================================================
# SECTION 4: TREND VALIDATION AND PARSING
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
  validate_mvgam_trend(trend_obj)

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
  
  # Check for intercept-only formula (~ 1) or no-intercept formula (~ -1)
  # Both should default to ZMVN
  is_intercept_only <- length(tf) == 0 && attr(tf_safe, 'intercept') == 1
  is_no_intercept_only <- length(tf) == 0 && attr(tf_safe, 'intercept') == 0
  is_simple_formula <- is_intercept_only || is_no_intercept_only

  # Validate that we have some meaningful formula structure
  # Allow ~ 1, ~ -1, and formulas with actual terms
  if (length(tf) == 0 && !is_simple_formula) {
    stop(insight::format_error(
      "Invalid trend formula structure.",
      "The {.field trend_formula} has no terms and no intercept specification.",
      "Use {.code ~ 1}, {.code ~ -1}, or include predictors/trend constructors."
    ))
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

  # Allow any formula without explicit trend constructors to default to ZMVN
  # This covers: intercept-only (~ 1), no-intercept (~ -1), and regular formulas (~ gp(time))
  has_explicit_trends <- length(trend_terms) > 0
  should_default_to_zmvn <- !has_explicit_trends

  # Validate we have exactly one trend type per response - only one allowed per formula
  if (length(trend_terms) > 1) {
    stop(insight::format_error(
      "Multiple trend constructors detected in single response formula.",
      paste("Found:", paste(trend_terms, collapse = ", ")),
      "Only one trend constructor is allowed per response variable.",
      "For multivariate models, use separate trend formulas per response."
    ))
  }

  # Handle formulas without explicit trend constructors (default to ZMVN)
  if (should_default_to_zmvn) {
    # Create proper mvgam_trend object using constructor with default arguments
    # ZMVN(time = NA, series = NA, cor = TRUE, n_lv = NULL) where NA becomes "time"/"series"
    trend_components <- list(trend1 = ZMVN())
  } else {
    # Parse trend constructor calls with error handling (brms pattern)
    trend_components <- vector("list", length(trend_terms))
    names(trend_components) <- paste0("trend", seq_along(trend_terms))

    for (i in seq_along(trend_terms)) {
      trend_components[[i]] <- eval_trend_constructor(trend_terms[i])
    }
  }

  # Validate trend components for any remaining conflicts
  validate_trend_components(trend_components)

  # Create base formula without trend constructors using structure-preserving rlang approach
  offset_attr <- attr(tf_safe, 'offset')

  if (!is.null(offset_attr)) {
    stop(insight::format_error(
      "Offsets not allowed in trend_formula.",
      "Check for invalid syntax in {.field trend_formula}."
    ))
  }

  # Use rlang-based approach to preserve complex formula structures like (1|series)
  base_formula <- parse_base_formula_safe(trend_formula, trend_terms)

  # Since we enforce single trend type, trend_model is always the first component
  trend_model <- trend_components[[1]]
  
  # Calculate dimensions from data for proper parameter filtering
  if (!is.null(data)) {
    # Use existing infrastructure to calculate dimensions
    dimensions <- extract_time_series_dimensions(
      data = data,
      time_var = trend_model$time,
      series_var = trend_model$series,
      trend_type = trend_model$trend
    )
    
    # Add dimensions to trend_model for filtering
    trend_model$dimensions <- dimensions
  }

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
  checkmate::assert_string(trend_call)
  
  # Parse the expression
  expr <- str2expression(trend_call)[[1]]
  
  # Use package namespace where trend constructors are defined
  pkg_env <- asNamespace("mvgam")
  trend_obj <- eval(expr, envir = pkg_env)
  
  # Validate result
  if (!is.mvgam_trend(trend_obj)) {
    insight::format_error(
      "Invalid trend constructor result.",
      "Expression {.code {trend_call}} did not produce a valid trend object.",
      "Check that you're using a supported trend constructor."
    )
  }
  
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

  # Basic input validation for trend-specific parameters
  checkmate::assert_logical(ma, len = 1)
  checkmate::assert_logical(cor, len = 1)

  # Use helper function for clean object creation
  # Complex logic (grouping, correlation requirements, parameter processing)
  # moved to validation and Stan assembly layers
  trend_obj <- create_mvgam_trend(
    "RW",  # Base trend type used for ALL dispatch
    .time = substitute(time),
    .series = substitute(series),
    .gr = substitute(gr),
    .subgr = substitute(subgr),
    # Store parameters as-is (processing moved to Stan assembly)
    ma = ma,
    cor = cor,
    n_lv = n_lv
  )

  # Legacy validation (will be replaced by enhanced validation layer)
  validate_mvgam_trend(trend_obj)

  return(trend_obj)
}

#' @rdname trend_constructors
#' @export
AR = function(time = NA, series = NA, p = 1, ma = FALSE, cor = FALSE, gr = NA, subgr = NA, n_lv = NULL) {
  # Validate AR order parameter
  if (length(p) == 1) {
    checkmate::assert_int(p, lower = 1)
  } else {
    checkmate::assert_integerish(p, lower = 1, unique = TRUE, sorted = TRUE)
  }
  
  # Basic input validation
  checkmate::assert_logical(ma, len = 1)
  checkmate::assert_logical(cor, len = 1)
  
  # Use helper function for clean object creation
  # Complex logic moved to validation and Stan assembly layers
  trend_obj <- create_mvgam_trend(
    "AR",  # Base trend type used for ALL dispatch
    .time = substitute(time),
    .series = substitute(series),
    .gr = substitute(gr),
    .subgr = substitute(subgr),
    # Store parameters as-is (processing happens in validation/Stan assembly)
    p = p,
    ma = ma,
    cor = cor,
    n_lv = n_lv
  )
  
  return(trend_obj)
}

#' @rdname trend_constructors
#' @export
CAR = function(time = NA, series = NA) {
  # CAR only supports first-order continuous autoregression (p=1)
  # Use helper function for clean object creation
  # All validation logic moved to validation layer (handles irregular time intervals, etc.)
  create_mvgam_trend(
    "CAR",  # Base trend type used for ALL dispatch
    .time = substitute(time),
    .series = substitute(series),
    # CAR doesn't support gr, subgr, or n_lv - leave them as NULL
    # Store parameters as-is
    p = 1,        # CAR is always first-order
    ma = FALSE,   # CAR doesn't support MA
    cor = FALSE   # CAR doesn't support correlation
  )
}

#' @rdname trend_constructors
#' @export
VAR = function(time = NA, series = NA, p = 1, ma = FALSE, gr = NA, subgr = NA, n_lv = NULL) {
  # Validate VAR order parameter
  checkmate::assert_int(p, lower = 1)
  
  # Basic input validation
  checkmate::assert_logical(ma, len = 1)
  
  # Use helper function for clean object creation
  # Complex logic moved to validation and Stan assembly layers
  trend_obj <- create_mvgam_trend(
    "VAR",  # Base trend type used for ALL dispatch
    .time = substitute(time),
    .series = substitute(series),
    .gr = substitute(gr),
    .subgr = substitute(subgr),
    # Store parameters as-is (processing happens in validation/Stan assembly)
    p = p,
    ma = ma,
    cor = TRUE,  # VAR models always use correlation for optimal performance
    n_lv = n_lv
  )
  
  return(trend_obj)
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
PW = function(time = NA, series = NA, cap = NA, n_changepoints = 10, 
              changepoint_range = 0.8, changepoint_scale = 0.05, 
              growth = 'linear', n_lv = NULL) {
  # Validate arguments
  growth <- match.arg(growth, choices = c('linear', 'logistic'))
  checkmate::assert_number(changepoint_range, lower = 0, upper = 1)
  checkmate::assert_int(n_changepoints, lower = 1)
  checkmate::assert_number(changepoint_scale, lower = 0)
  
  # PW doesn't support factor models - validate n_lv
  if (!is.null(n_lv)) {
    stop(insight::format_error(
      "Factor models ({.field n_lv}) not supported for PW trends.",
      "Piecewise trends require series-specific changepoint modeling.",
      "Remove {.field n_lv} parameter or use factor-compatible trends: AR, RW, VAR, ZMVN"
    ), call. = FALSE)
  }
  
  # Check for required cap variable in logistic models
  cap_expr <- substitute(cap)
  if (growth == 'logistic' && identical(cap_expr, quote(NA))) {
    stop(insight::format_error(
      "Logistic growth models require a {.field cap} variable.",
      "Either provide {.field cap} argument or ensure 'cap' column exists in data.",
      "Example: PW(cap = carrying_capacity, growth = 'logistic')"
    ), call. = FALSE)
  }
  
  trend_obj <- create_mvgam_trend(
    "PW",  # Base trend type used for ALL dispatch
    .time = substitute(time),
    .series = substitute(series),
    .cap = cap_expr,
    n_changepoints = n_changepoints,
    changepoint_range = changepoint_range,
    changepoint_scale = changepoint_scale,
    growth = growth,
    n_lv = n_lv
  )
  return(trend_obj)
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
ZMVN = function(time = NA, series = NA, gr = NA, subgr = NA, n_lv = NULL) {
  # Basic parameter validation for n_lv if provided
  if (!is.null(n_lv)) {
    checkmate::assert_int(n_lv, lower = 1, null.ok = TRUE)
  }
  
  # Use helper function for clean object creation
  # All validation logic moved to validation layer
  create_mvgam_trend(
    "ZMVN",  # Base trend type used for ALL dispatch
    .time = substitute(time),
    .series = substitute(series),
    .gr = substitute(gr),
    .subgr = substitute(subgr),
    # Store parameters as-is
    n_lv = n_lv,
    ma = FALSE,   # ZMVN doesn't support MA
    cor = TRUE    # ZMVN always has correlation structure
  )
}

# =============================================================================
# SECTION 6: TREND CONSTRUCTOR HELPER FUNCTIONS
# =============================================================================
# WHY: These helper functions simplify trend constructor development and
# ensure consistency across all trend types. They provide universal defaults,
# automatic validation rule assignment, and standardized creation patterns.

#' Get Universal mvgam Trend Defaults
#'
#' @description
#' Returns universal default values that apply to all trend types.
#' Trend-specific defaults should be handled within individual constructors.
#'
#' @return Named list of universal default values
#' @noRd
get_mvgam_trend_defaults <- function() {
  list(
    # Universal variable defaults
    time = "time",
    series = "series",
    gr = "NA",
    subgr = "NA",

    # Universal behavior defaults
    ma = FALSE,
    cor = FALSE,
    n_lv = NULL,

    # Universal metadata defaults (NULL allows trend-specific exclusion logic to determine)
    shared_innovations = NULL,

    # Placeholder validation rules (to be auto-assigned by trend type)
    validation_rules = character(0),

    # Placeholder metadata (to be auto-generated)
    monitor_params = character(0),
    forecast_metadata = list(),
    summary_labels = character(0)
  )
}

#' Apply mvgam Trend Defaults
#'
#' @description
#' Fills in missing fields in a trend object with appropriate defaults.
#' Applies universal defaults first, then auto-generates metadata fields.
#'
#' @param trend_obj Partial trend object (list)
#' @return Complete trend object with all fields filled
#' @noRd
apply_mvgam_trend_defaults <- function(trend_obj) {
  checkmate::assert_list(trend_obj)
  checkmate::assert_character(trend_obj$trend, len = 1, min.chars = 1,
                             null.ok = FALSE, .var.name = "trend_obj$trend")

  # Get universal defaults
  defaults <- get_mvgam_trend_defaults()

  # Apply universal defaults for missing fields
  for (field in names(defaults)) {
    if (!field %in% names(trend_obj)) {
      trend_obj[[field]] <- defaults[[field]]
    }
  }

  # Auto-assign validation rules based on trend type
  if (length(trend_obj$validation_rules) == 0) {
    trend_obj$validation_rules <- get_default_validation_rules(trend_obj$trend)
  }

  # Auto-generate metadata if missing
  if (length(trend_obj$monitor_params) == 0) {
    trend_obj$monitor_params <- generate_monitor_params(trend_obj)
  }

  if (length(trend_obj$forecast_metadata) == 0) {
    trend_obj$forecast_metadata <- generate_forecast_metadata(trend_obj)
  }

  if (length(trend_obj$summary_labels) == 0) {
    trend_obj$summary_labels <- generate_summary_labels(trend_obj)
  }

  return(trend_obj)
}

#' Get Default Validation Rules by Trend Type
#'
#' @description
#' Automatically assigns appropriate validation rules based on trend type.
#' This makes adding new trends easier - just specify the trend type and
#' get sensible rule defaults.
#'
#' @param trend_type Character string, the trend type
#' @return Character vector of validation rule strings
#' @noRd
get_default_validation_rules <- function(trend_type) {
  checkmate::assert_string(trend_type, min.chars = 1)

  # Define rule sets for easy extension
  stationary_trend_rules <- c(
    rule_requires_regular_intervals,
    rule_supports_factors,
    rule_supports_hierarchical
  )

  irregular_trend_rules <- c(
    rule_allows_irregular_intervals,
    rule_incompatible_with_factors,
    rule_incompatible_with_hierarchical
  )

  changepoint_trend_rules <- c(
    rule_requires_regular_intervals,
    rule_incompatible_with_factors,
    rule_supports_hierarchical
  )

  multivariate_trend_rules <- c(
    rule_requires_regular_intervals,
    rule_supports_factors,
    rule_supports_hierarchical,
    rule_requires_minimum_series_count
  )

  # Assign rules based on trend type
  rules <- switch(trend_type,
    "RW" = stationary_trend_rules,
    "AR" = stationary_trend_rules,
    "VAR" = multivariate_trend_rules,
    "CAR" = irregular_trend_rules,
    "PW" = changepoint_trend_rules,
    "ZMVN" = multivariate_trend_rules,

    # Default for unknown trend types (extensible)
    stationary_trend_rules
  )

  return(rules)
}

#' Create mvgam Trend Object
#'
#' @description
#' Helper function to create consistent mvgam_trend objects with automatic
#' defaults and validation. Used by all trend constructors.
#'
#' @param trend_type Base trend type (e.g., "AR", "RW", "VAR")
#' @param ... Additional trend-specific parameters
#' @param .time Time variable (quoted or unquoted)
#' @param .series Series variable (quoted or unquoted)
#' @param .gr Grouping variable (quoted or unquoted)
#' @param .subgr Subgrouping variable (quoted or unquoted)
#' @param .validation_rules Optional override for validation rules
#' @return mvgam_trend object
#' @export
create_mvgam_trend <- function(trend_type, ...,
                               .time = NULL,
                               .series = NULL,
                               .gr = NULL,
                               .subgr = NULL,
                               .cap = NULL,
                               .validation_rules = NULL) {
  checkmate::assert_string(trend_type, min.chars = 1)

  # Helper to process substituted arguments - handles both quoted and unquoted
  process_arg <- function(x) {
    if (is.null(x)) return("NA")  # Return "NA" for missing arguments
    if (is.character(x)) return(x)
    deparsed <- deparse0(x)
    # Remove surrounding quotes if present (from quoted strings)
    if (grepl('^".*"$', deparsed)) {
      return(substr(deparsed, 2, nchar(deparsed) - 1))
    }
    # Check if it's an undefined symbol (like when substitute(gr) returns 'gr' symbol)
    if (deparsed %in% c("gr", "subgr", "time", "series", "cap")) {
      return("NA")  # These are undefined symbols, not actual values
    }
    deparsed
  }
  
  # Process all variables consistently
  time_var <- process_arg(.time)
  series_var <- process_arg(.series)
  gr_var <- process_arg(.gr)
  subgr_var <- process_arg(.subgr)
  cap_var <- process_arg(.cap)

  # Handle default variable names
  time_was_default <- (time_var == "NA")
  if (time_var == "NA") time_var <- "time"

  series_was_default <- (series_var == "NA")
  if (series_var == "NA") series_var <- "series"
  
  # Handle grouping defaults
  if (gr_var == "NA") gr_var <- "NA"
  if (subgr_var == "NA") subgr_var <- "NA"
  
  # Handle cap default
  if (cap_var == "NA") cap_var <- "cap"

  # Issue warnings for defaults (following existing pattern)
  if (time_was_default) warn_default_time_variable()
  if (series_was_default) warn_default_series_variable()

  # Create base trend object
  trend_obj <- list(
    trend = trend_type,
    time = time_var,
    series = series_var,
    gr = gr_var,
    subgr = subgr_var,
    cap = cap_var,
    ...
  )

  # Override validation rules if provided
  if (!is.null(.validation_rules)) {
    trend_obj$validation_rules <- .validation_rules
  }

  # Apply defaults and auto-generate metadata
  trend_obj <- apply_mvgam_trend_defaults(trend_obj)

  # Set class
  class(trend_obj) <- "mvgam_trend"

  # Validate the complete object
  validate_mvgam_trend(trend_obj)

  # Add consistent dispatch metadata
  trend_obj <- add_consistent_dispatch_metadata(trend_obj)

  return(trend_obj)
}

#' Validate Trend Dispatch Consistency
#'
#' @description
#' Ensures all trend-related dispatch functions use consistent naming.
#' This function checks that Stan generation, forecasting, and other
#' dispatch functions follow the convention: trend_type + function_suffix.
#'
#' @param trend_obj mvgam_trend object
#' @return Logical TRUE if consistent, stops with error if not
#' @noRd
validate_trend_dispatch_consistency <- function(trend_obj) {
  checkmate::assert_class(trend_obj, "mvgam_trend")

  trend_type <- trend_obj$trend
  if (is.null(trend_type)) {
    stop("Trend object must have 'trend' field")
  }

  # Define expected function naming patterns
  expected_patterns <- list(
    stanvar_generator = paste0("generate_", tolower(trend_type), "_trend_stanvars"),
    forecast_function = paste0("forecast_", tolower(trend_type), "_rcpp"),
    monitor_generator = paste0("generate_", tolower(trend_type), "_monitor_params")
  )

  # Check forecast metadata if present
  if (!is.null(trend_obj$forecast_metadata)) {
    expected_forecast <- expected_patterns$forecast_function
    actual_forecast <- trend_obj$forecast_metadata$function_name

    if (!is.null(actual_forecast) && actual_forecast != expected_forecast) {
      stop(insight::format_error(
        "Inconsistent forecast function naming",
        "Expected: {.field {expected_forecast}}",
        "Got: {.field {actual_forecast}}",
        "All dispatch functions must follow pattern: trend_type + function_suffix"
      ))
    }
  }

  invisible(TRUE)
}

#' Get Trend Dispatch Function Name
#'
#' @description
#' Generates consistent function names for trend dispatch based on convention.
#' Ensures absolute consistency throughout the system.
#'
#' @param trend_type Base trend type (e.g., "AR", "RW", "VAR")
#' @param function_type Type of function ("stanvar", "forecast", "monitor")
#' @return String with properly formatted function name
#' @noRd
get_trend_dispatch_function <- function(trend_type, function_type) {
  checkmate::assert_string(trend_type)
  checkmate::assert_choice(function_type, c("stanvar", "forecast", "monitor"))

  trend_lower <- tolower(trend_type)

  switch(function_type,
    stanvar = paste0("generate_", trend_lower, "_trend_stanvars"),
    forecast = paste0("forecast_", trend_lower, "_rcpp"),
    monitor = paste0("generate_", trend_lower, "_monitor_params")
  )
}

#' Enhance Trend Object with Consistent Dispatch
#'
#' @description
#' Automatically adds consistent dispatch function names to trend object.
#' Ensures all dispatch follows the same convention.
#'
#' @param trend_obj mvgam_trend object
#' @return Enhanced trend object with consistent dispatch metadata
#' @noRd
add_consistent_dispatch_metadata <- function(trend_obj) {
  trend_type <- trend_obj$trend

  # Add forecast metadata with consistent naming
  if (is.null(trend_obj$forecast_metadata)) {
    trend_obj$forecast_metadata <- list(
      function_name = get_trend_dispatch_function(trend_type, "forecast")
    )
  }

  # Add monitor params generator name
  trend_obj$monitor_generator <- get_trend_dispatch_function(trend_type, "monitor")

  # Add stanvar generator name
  trend_obj$stanvar_generator <- get_trend_dispatch_function(trend_type, "stanvar")

  return(trend_obj)
}
