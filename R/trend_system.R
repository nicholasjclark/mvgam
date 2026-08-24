#' Trend System Infrastructure for mvgam
#'
#' @description
#' Trend infrastructure including registry management, validation,
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
        stop(insight::format_error(c(
          cli::format_inline(
            "Invalid prior specification for parameter {.field {param_name}}."
          ),
          i = "Each prior specification must be a named list with 'default', 'bounds', and 'description' elements."
        )))
      }
      required_fields <- c("default", "bounds", "description")
      missing_fields <- setdiff(required_fields, names(param_spec))
      if (length(missing_fields) > 0) {
        stop(insight::format_error(c(
          cli::format_inline(
            "Missing required fields in prior specification for {.field {param_name}}."
          ),
          x = cli::format_inline("Missing: {.val {missing_fields}}")
        )))
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
    stop(insight::format_error(c(
      cli::format_inline("Unknown trend type: {.field {name}}"),
      i = cli::format_inline("Available types: {.val {available_trends}}")
    )))
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
    stop(insight::format_error(c(
      "No trend generator functions found.",
      x = cli::format_inline(
        "Expected functions like {.field generate_ar_trend_stanvars}, {.field generate_rw_trend_stanvars}, etc."
      ),
      i = "Check that Stan assembly functions follow naming convention."
    )))
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
      stop(insight::format_error(c(
        cli::format_inline(
          "Missing required properties function for trend type {.field {trend_type}}."
        ),
        x = cli::format_inline(
          "You must define {.field {properties_func_name}()} that returns list(supports_factors = TRUE/FALSE, incompatibility_reason = '...')."
        ),
        i = "This ensures explicit declaration of trend capabilities."
      )))
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
    stop(insight::format_error(c(
      cli::format_inline(
        "Function {.field {func_name}()} must return a list."
      ),
      x = cli::format_inline(
        "Got {.field {class(trend_info)}} instead."
      ),
      i = "Fix: return list(supports_factors = TRUE/FALSE, incompatibility_reason = '...')"
    )))
  }

  required_fields <- c("supports_factors")
  missing_fields <- setdiff(required_fields, names(trend_info))

  if (length(missing_fields) > 0) {
    stop(insight::format_error(c(
      cli::format_inline(
        "Function {.field {func_name}()} missing required fields: {.field {missing_fields}}."
      ),
      x = "Required structure: list(supports_factors = TRUE/FALSE, incompatibility_reason = '...')",
      i = cli::format_inline(
        "The supports_factors field is mandatory for trend type {.field {trend_type}}."
      )
    )))
  }

  if (!is.logical(trend_info$supports_factors) || length(trend_info$supports_factors) != 1) {
    stop(insight::format_error(c(
      cli::format_inline(
        "Field {.field supports_factors} must be a single logical value (TRUE or FALSE)."
      ),
      x = cli::format_inline(
        "Got {.field {trend_info$supports_factors}} of type {.field {class(trend_info$supports_factors)}}."
      ),
      i = cli::format_inline(
        "Fix {.field {func_name}()} to return supports_factors = TRUE or FALSE."
      )
    )))
  }

  if (!trend_info$supports_factors &&
      (is.null(trend_info$incompatibility_reason) || !is.character(trend_info$incompatibility_reason))) {
    stop(insight::format_error(c(
      cli::format_inline(
        "Trends with {.field supports_factors = FALSE} must provide {.field incompatibility_reason}."
      ),
      x = cli::format_inline(
        "Fix {.field {func_name}()} to include incompatibility_reason = 'explanation why factors not supported'."
      ),
      i = cli::format_inline(
        "This helps users understand why factor models don't work with {.field {trend_type}} trends."
      )
    )))
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
# WHY: Following brms design patterns for prior() objects, we create a
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
#'
#' @param e1,e2 `trend_param` objects (or `NULL`) to combine.
#'
#' @return A `trend_param` object holding the row-bound union of the
#'   supplied trend parameters, with later definitions of a parameter
#'   overriding earlier ones of the same name.
#'
#' @export
`+.trend_param` <- function(e1, e2) {
  if (is.null(e2)) return(e1)
  if (!is.trend_param(e2)) {
    stop(insight::format_error(
      cli::format_inline(
        "Cannot add {.val {class(e2)[1]}} objects to trend parameters."
      )
    ))
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

#' Test whether an object is a `trend_param`
#'
#' @param x An object to test.
#'
#' @return A single logical: `TRUE` if `x` inherits from the
#'   `trend_param` class, `FALSE` otherwise.
#'
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
  trend_type <- get_trend_name(trend_spec)

  # Base parameters that most trends share
  base_params <- c("sigma_trend")

  # Trend-specific parameters
  trend_specific <- switch(trend_type,
    "RW" = generate_rw_monitor_params(trend_spec),
    "AR" = generate_ar_monitor_params(trend_spec),
    "VAR" = generate_var_monitor_params(trend_spec),
    "CAR" = generate_car_monitor_params(trend_spec),
    "ZMVN" = character(0),
    "PW" = generate_pw_monitor_params(trend_spec),
    stop(insight::format_error(c(
      cli::format_inline(
        "Unknown trend type: {.field {trend_type}}"
      ),
      i = "Supported types: RW, AR, VAR, CAR, ZMVN, PW"
    )))
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

  # Estimated innovation degrees of freedom. Listed here so the
  # parameter reaches `get_prior()` alongside `sigma_trend` and the
  # autoregressive coefficients, rather than being overridable only by
  # a user who already knows the class name. Absent when the
  # innovations are Gaussian or the degrees of freedom are fixed.
  df_params <- if (is.na(trend_spec$df %||% Inf)) {
    "nu_trend"
  } else {
    character(0)
  }

  # Add hierarchical correlation parameters if grouping is specified
  hierarchical_params <- if (!is.null(trend_spec$gr) && trend_spec$gr != "NA") {
    c("alpha_cor_trend", "L_Omega_global_trend", "L_deviation_group_trend", "sigma_group_trend")
  } else {
    character(0)
  }

  # Combine all parameters
  all_params <- unique(c(
    base_params, trend_specific, correlation_params, factor_params,
    df_params, hierarchical_params
  ))

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
    lag_vec <- 1:lags
  } else {
    # Vector or multiple values: use specific lags only
    lag_vec <- lags
  }
  ar_params <- paste0("ar", lag_vec, "_trend")

  # Under hierarchical sharing the per-series ar{lag}_trend
  # vectors still exist (one normal draw per series, per lag)
  # AND there is a population mean + scale per lag. Add those
  # so `get_prior()` surfaces an editable row for each.
  sharing <- trend_spec$coef_sharing %||% "none"
  if (sharing == "hierarchical") {
    ar_params <- c(
      ar_params,
      paste0("mu_ar", lag_vec, "_trend"),
      paste0("sigma_ar", lag_vec, "_trend")
    )
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


# Internal: pull the canonical trend constructor name from a
# trend spec object (an mvgam_trend, the bare result of
# `AR()` / `VAR()` / `RW()` / `CAR()` / `PW()` / `ZMVN()`).
# Other callers across `R/` already use the
# `spec$trend %||% spec$trend_type` pattern (see line ~638);
# this helper centralises it and runs the same
# `normalize_trend_type()` strip used downstream.
#'@noRd
get_trend_name <- function(trend_spec) {
  if (is.null(trend_spec)) return(NA_character_)
  raw <- trend_spec$trend %||% trend_spec$trend_type
  if (is.null(raw) || !length(raw)) return(NA_character_)
  normalize_trend_type(as.character(raw)[1L])
}

# -----------------------------------------------------------------------------
# Forecast Metadata Generation
# -----------------------------------------------------------------------------

#' Generate Forecast Metadata for Ultra-Fast Dispatch
#'
#' Creates minimal forecast metadata for fast runtime dispatch.
#' Stores only function name and required parameter list for lazy extraction
#' and zero-overhead forecasting calls.
#'
#' @param trend_spec An mvgam_trend object
#' @return List with function_name and required_params for dispatch
#' @noRd
generate_forecast_metadata <- function(trend_spec) {
  checkmate::assert_list(trend_spec, min.len = 1)

  # Extract and normalize trend type
  trend_type <- get_trend_name(trend_spec)

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
#' Always a subset of monitor_params, kept minimal for fast extraction.
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
    stop(insight::format_error(c(
      cli::format_inline(
        "Unknown trend type for forecasting: {.field {trend_type}}"
      ),
      i = "Supported types: RW, AR, VAR, CAR, ZMVN, PW"
    )))
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
  trend_type <- get_trend_name(trend_spec)

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
#' @param trend_spec Trend specification for context
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

# =============================================================================
# SECTION 3: MVGAM_TREND OBJECT SPECIFICATION
# =============================================================================

#' mvgam_trend Object Field Specification
#'
#' @description
#' Documentation of required and optional fields for the
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
#'   \item{forecast_metadata}{List. Forecasting function information:
#'     \describe{
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
#'     \describe{
#'       \item{parameter_labels}{List mapping parameter names to display labels}
#'       \item{factor_labels}{List for factor loading label patterns}
#'       \item{group_labels}{List for hierarchical parameter labels}
#'     }}
#' }
#'
#' @section Internal Processing Fields:
#' \describe{
#'   \item{param_info}{List containing:
#'     \describe{
#'       \item{parameters}{trend_param object with parameter specifications}
#'       \item{characteristics}{List of trend capabilities and settings}
#'     }}
#'   \item{shared_innovations}{Logical. Whether trend uses shared Gaussian
#'     innovation system (TRUE) or handles own innovations (FALSE).
#'     Most trends use shared system; exceptions: CAR, VAR, PW.}
#'   \item{dimensions}{List. Pre-calculated time series dimensions (populated during validation):
#'     \describe{
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
#'   \item "supports_factors" + n_lv: Factor models allowed
#'   \item "incompatible_with_factors" + n_lv: Error thrown
#'   \item "supports_hierarchical" + gr/subgr: Hierarchical models allowed
#'   \item "requires_regular_intervals": Regular time validation triggered
#'   \item "allows_irregular_intervals": CAR-style irregular time handling
#' }
#'
#' @section Convention-Based Function Dispatch:
#' The trend field enables automatic function lookup:
#' \itemize{
#'   \item Stan generation: "AR" → generate_ar_trend_stanvars()
#'   \item Forecasting: forecast_metadata$function_name → that function
#'   \item No manual registry entries needed
#' }
#'
#' @section Response Suffix Handling (Multivariate):
#' In multivariate contexts, certain fields are automatically modified:
#' \itemize{
#'   \item monitor_params: "_count", "_biomass" suffixes added to parameter names
#'   \item summary_labels: Response-specific labels generated automatically
#'   \item response_context: Set to response name for tracking
#' }
#'
#' @section Backward Compatibility:
#' During transition period, old fields may still be present:
#' \itemize{
#'   \item trend_model: Legacy field, use trend instead
#'   \item trend_type: Legacy field, use trend instead
#'   \item forecast_fun: Legacy field, use forecast_metadata$function_name
#'   \item stancode_fun: Legacy field, replaced by convention-based lookup
#'   \item standata_fun: Legacy field, replaced by convention-based lookup
#' }
#'
#' @section Class Structure Requirements:
#' Objects must:
#' \itemize{
#'   \item Have class c("mvgam_trend")
#'   \item Pass validate_mvgam_trend() checks
#'   \item Include all required core fields
#'   \item Use approved validation_rules vocabulary
#'   \item Have consistent field types and relationships
#' }
#'
#' @section Example Object:
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
#'   \item Self-contained: each object contains all needed metadata
#'   \item Convention-based: minimal configuration, maximum automation
#'   \item Extensible: new fields can be added without breaking existing trends
#'   \item Validated: structure enforced through `validate_mvgam_trend()`
#'   \item Consistent: all trends follow identical patterns
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
#'     can handle multiple seasonal periods simultaneously. Used by: multi-seasonal models.
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

# Validation rules vocabulary
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
    # Look for trend_type followed by opening parenthesis
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
#' Extracts trend model specifications from a formula using validation
#'   patterns inspired by brms' validate_formula and mvgam's interpret_mvgam.
#'
#' @param trend_formula A formula object containing trend specifications
#' @param data The data frame for validation (optional)
#'
#' @return List containing parsed formula components
#' @noRd
parse_trend_formula <- function(trend_formula, data = NULL, response_vars = NULL, .precomputed_dimensions = NULL) {

  # Input validation with brms-inspired error handling
  checkmate::assert_class(trend_formula, "formula")
  if (!is.null(response_vars)) {
    checkmate::assert_character(response_vars, min.len = 1, any.missing = FALSE)
  }
  if (!is.null(.precomputed_dimensions)) {
    checkmate::assert_list(.precomputed_dimensions, names = "named")
  }

  # Capture the formula's environment so trend constructor args
  # that reference user-defined variables (e.g. an inline matrix
  # passed to `trend_map`) can be resolved at evaluation time.
  formula_env <- environment(trend_formula) %||% parent.frame()

  # Safe formula parsing with try() like brms
  tf_safe <- try(terms(trend_formula, keep.order = TRUE), silent = TRUE)
  if (inherits(tf_safe, "try-error")) {
    stop(insight::format_error(c(
      "Invalid formula syntax.",
      x = cli::format_inline(
        "The {.field trend_formula} could not be parsed."
      ),
      i = "Check for balanced parentheses and valid R syntax."
    )))
  }

  # Check for response variable (brms pattern)
  if (attr(tf_safe, "response") > 0) {
    stop(insight::format_error(c(
      "Response variable not allowed in trend formula.",
      x = "Trend formulas should only contain predictors.",
      i = cli::format_inline(
        "Remove the response variable from {.field trend_formula}."
      )
    )))
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
    stop(insight::format_error(c(
      "Invalid trend formula structure.",
      x = cli::format_inline(
        "The {.field trend_formula} has no terms and no intercept specification."
      ),
      i = cli::format_inline(
        "Use {.code ~ 1}, {.code ~ -1}, or include predictors/trend constructors."
      )
    )))
  }

  # Find trend terms using mvgam-style detection with brms-inspired robustness
  trend_types <- mvgam_trend_registry()
  trend_indices <- integer(0)

  for (trend_type in trend_types) {
    # Detect trend constructor invocations via fixed grep
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
    stop(insight::format_error(c(
      "Multiple trend constructors detected in single response formula.",
      x = paste("Found:", paste(trend_terms, collapse = ", ")),
      x = "Only one trend constructor is allowed per response variable.",
      i = "For multivariate models, use separate trend formulas per response."
    )))
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
      trend_components[[i]] <- eval_trend_constructor(
        trend_terms[i], formula_env = formula_env
      )
    }
  }

  # Validate trend components for any remaining conflicts
  validate_trend_components(trend_components)

  # Create base formula without trend constructors using structure-preserving rlang approach
  offset_attr <- attr(tf_safe, 'offset')

  if (!is.null(offset_attr)) {
    stop(insight::format_error(c(
      "Offsets not allowed in trend_formula.",
      i = cli::format_inline(
        "Check for invalid syntax in {.field trend_formula}."
      )
    )))
  }

  # Use rlang-based approach to preserve complex formula structures like (1|series)
  base_formula <- parse_base_formula_safe(trend_formula, trend_terms)

  # Since we enforce single trend type, trend_model is always the first component
  trend_model <- trend_components[[1]]

  # Validate CAR restrictions early in parsing flow
  # CAR models use irregular time intervals that vary by series,
  # making shared trend covariates incompatible only in multivariate models
  if (identical(trend_model$trend, "CAR") && length(regular_terms) > 0 && !is.null(data)) {
    # Determine if this is a multivariate model by checking series count
    series_var <- trend_model$series %||% "series"
    time_var <- trend_model$time %||% "time"
    
    # Try to count series - if series column exists, count unique values
    # If series column doesn't exist, assume univariate (single series)
    n_series <- 1  # Default assumption for missing series column
    if (series_var %in% names(data)) {
      n_series <- length(unique(data[[series_var]]))
    }
    
    # Only restrict CAR models with covariates if multivariate (n_series > 1)
    if (n_series > 1) {
      stop(insight::format_error(c(
        cli::format_inline(
          "Multivariate CAR models cannot include trend covariates in {.field trend_formula}."
        ),
        x = "CAR models use irregular time intervals that vary by series in multivariate settings.",
        i = "Remove covariates from the trend formula or use a different trend type.",
        i = "Note: Univariate CAR models (single series) can include trend covariates."
      )), call. = FALSE)
    }
  }

  # Calculate dimensions from data for proper parameter filtering
  if (!is.null(data)) {
    # Add regular_terms to trend_model for covariate extraction
    trend_model$regular_terms <- regular_terms
    
    # Use precomputed dimensions - no fallback in ultra-DRY architecture
    if (!is.null(.precomputed_dimensions)) {
      dimensions <- .precomputed_dimensions
    } else if (!is.null(data)) {
      stop(insight::format_error(c(
        "Missing precomputed dimensions in ultra-DRY architecture.",
        x = "When data is provided, precomputed dimensions must be supplied.",
        i = "Check that calling function is passing dimensions correctly."
      )), call. = FALSE)
    } else {
      # No data provided - dimensions not needed for formula parsing only
      dimensions <- NULL
    }

    # Add dimensions to trend_model for filtering if available
    if (!is.null(dimensions)) {
      trend_model$dimensions <- dimensions
      trend_model$metadata <- dimensions$metadata
    }
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
#' @param formula_env Optional environment from the originating
#'   trend formula. Used so constructor arguments that reference
#'   user-scope variables (e.g. an inline matrix passed to
#'   `trend_map`) resolve correctly. Falls back to the mvgam
#'   namespace if NULL (legacy / direct callers).
#'
#' @return A validated mvgam_trend object
#' @noRd
eval_trend_constructor <- function(trend_call, formula_env = NULL) {
  checkmate::assert_string(trend_call)

  # Parse the expression
  expr <- str2expression(trend_call)[[1]]

  # Build a child environment of the user's formula scope so any
  # symbols inside the constructor call (variables, matrices,
  # data.frames) are visible. Trend constructors themselves are
  # always resolvable because the mvgam namespace sits on the
  # search path. Fall back to the package namespace when no
  # formula env is supplied.
  pkg_env <- asNamespace("mvgam")
  eval_env <- if (is.null(formula_env)) {
    pkg_env
  } else {
    new.env(parent = formula_env)
  }
  trend_obj <- eval(expr, envir = eval_env)

  # Validate result
  if (!is.mvgam_trend(trend_obj)) {
    stop(insight::format_error(c(
      "Invalid trend constructor result.",
      x = cli::format_inline(
        "Expression {.code {trend_call}} did not produce a valid trend object."
      ),
      i = "Check that you're using a supported trend constructor."
    )))
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
#' @return The `mvgam_trend` object `x`, returned invisibly.
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
# with the brms ecosystem. This layer abstracts Stan complexity.

#' Trend Model Constructors for \pkg{mvgam}
#'
#' Specify trend models for multivariate State-Space models in \pkg{mvgam}.
#' These constructor functions create trend specifications for various temporal
#' dynamics including random walks (RW), autoregressive models (AR, VAR, CAR),
#' and piecewise trends (PW). These functions do not evaluate their arguments
#' – they exist purely to help set up models with particular trend structures.
#'
#' @param df Degrees of freedom for the latent process innovations.
#'   Defaults to \code{Inf}, which gives the Gaussian innovations mvgam
#'   has always used, since a t with infinite degrees of freedom is a
#'   normal. Set \code{df = NA} to estimate them, or supply a number
#'   above \code{2} to fix them.
#'
#'   Finite degrees of freedom let the latent process absorb an
#'   occasional large shock without inflating \code{sigma_trend}
#'   everywhere, which suits boom-and-bust population series and
#'   outbreak dynamics. The innovations follow a multivariate t, which
#'   shares the innovation \emph{scale} across series at each time
#'   point. Large innovations therefore tend to occur together, but
#'   each series keeps its own direction and magnitude: a shock in one
#'   series says nothing about the sign of the others, and a series can
#'   sit out an event that moves its neighbours. Correlation between
#'   series is governed by \code{cor} exactly as it is for Gaussian
#'   innovations; the tail behaviour is layered on top of it.
#'
#'   When \code{df = NA} the degrees of freedom are estimated as
#'   \code{nu_trend}, with a default \code{gamma(4, 0.3)} prior that
#'   can be replaced through the \code{prior} argument, for example
#'   \code{prior(gamma(2, 0.1), class = "nu_trend")}. Note that
#'   \code{nu_trend} does not yet appear in the
#'   \code{\link{get_prior}} table, so the class name has to come
#'   from here.
#'
#'   Two caveats. The bound of \code{2} is required rather than
#'   conventional: below it the innovations have no finite variance and
#'   the stationary initialisation of an autoregressive trend is
#'   undefined. And the degrees of freedom are informed only through
#'   the tail of the latent process, so they are weakly identified in
#'   short series; expect the posterior to lean on its prior below
#'   roughly 200 time points. Not available for \code{VAR()}, which
#'   samples its states directly, or \code{PW()}, which has no
#'   innovations.
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
#' @param p Specification of the autoregressive lag set. The
#'   semantics differ slightly across trend types:
#'   * For `AR()` models: a positive integer or a positive
#'     integer vector. A scalar \code{p = k} is the textbook
#'     AR(k) interpretation and is expanded to consecutive
#'     lags \code{1:k}, so the fit estimates \code{ar1_trend},
#'     \code{ar2_trend}, ..., \code{ark_trend}. A vector
#'     \code{p = c(...)} selects a sparse lag set:
#'     \code{p = c(1, 12)} declares only \code{ar1_trend} and
#'     \code{ar12_trend} (seasonal AR with no intermediate
#'     lags), and \code{p = c(2, 4)} declares only
#'     \code{ar2_trend} and \code{ar4_trend} (no \code{ar1}
#'     or \code{ar3}).
#'   * For `VAR()` models: a positive integer. A scalar
#'     \code{p = k} is the VAR(k) interpretation with
#'     consecutive coefficient matrices for lags \code{1:k}.
#'     Sparse-lag vector \code{p} is not supported and will
#'     not be added: the Heaps-2022 stationary joint-
#'     distribution initialisation assumes consecutive
#'     companion-form structure, so the sparse case has no
#'     companion-form analogue with the same identified
#'     stationary covariance. Use \code{AR(p = c(...))} for
#'     sparse-lag autoregression on a single series.
#'   * For `CAR()` models: must be \code{1} (continuous-time
#'     AR(1) process).
#'
#' @param time The unquoted name of the variable that represents time in the
#'   supplied `data`. This variable should be either a `numeric` or `integer`
#'   variable. Defaults to `time` to align with brms conventions, allowing
#'   any time variable name without requiring explicit "time" columns.
#'   When using the default, a one-time warning will be issued.
#'
#' @param series The unquoted name of the variable that represents the series
#'   identifier in the supplied `data`. This variable should be either a
#'   `character` or `factor` variable. Defaults to `series` following mvgam
#'   conventions, allowing any series variable name. When using the
#'   default, a one-time warning will be issued.
#'
#' @details
#' **Important**: Only ONE trend constructor is allowed per `trend_formula`.
#' For complex temporal dynamics, use the parameter options of a single trend type:
#' \itemize{
#'   \item For seasonal patterns: `AR(p = c(1, 12))` instead of `RW() + AR(p = 12)`
#'   \item For multiple time scales: `AR(p = c(1, 7, 30))` for daily, weekly, monthly
#'   \item For multivariate dynamics: `VAR(p = 2)` captures cross-series relationships
#' }
#'
#' @note **VAR fits and `init = 0`**: VAR uses the Heaps-2022
#'   stationary joint-distribution initialisation. Setting
#'   `init = 0` in the call to [mvgam()] starts all parameters
#'   at zero on the unconstrained scale, which collapses the
#'   stationary covariance matrix and prevents the chain from
#'   making any valid first step. Leave the sampler's `init`
#'   argument at its default (random) for VAR fits. AR / RW /
#'   CAR / ZMVN are unaffected.
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
#'   Two constraints currently apply to hierarchical trend models:
#'   \itemize{
#'     \item `gr` must be constant within each series (each series belongs to
#'       a single group). Models with `gr` varying within a series are
#'       rejected at validation time.
#'     \item Groups must be balanced (the same number of series in each
#'       group). Unbalanced designs are not yet supported by the underlying
#'       Stan template; if you supply unbalanced data without an explicit
#'       `subgr` argument, the model will fail at Stan initialisation. See
#'       the package issue tracker for the planned ragged-array support.
#'   }
#'
#' @param coef_sharing Character string, one of `"none"`,
#'   `"shared"`, or `"hierarchical"`, controlling how AR
#'   coefficients vary across latent series in `AR()`. The
#'   default `"none"` estimates an independent coefficient
#'   vector per series (one `ar{lag}_trend` entry per latent
#'   process per lag). `"shared"` collapses to a single
#'   `ar{lag}_shared` scalar per lag, broadcast across all
#'   series in `transformed parameters`. `"hierarchical"` adds
#'   per-lag population-mean (`mu_ar{lag}_trend`) and scale
#'   (`sigma_ar{lag}_trend`) hyperparameters with a pooled
#'   `ar{lag}_trend[j] ~ normal(mu_ar{lag}_trend,
#'   sigma_ar{lag}_trend)`.
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
#' @section AR coefficient-sharing surface:
#' The `coef_sharing` argument selects how AR coefficients
#' vary across the latent series. The three settings and the
#' Stan parameters each declares are:
#' \tabular{ll}{
#'   \strong{coef_sharing}    \tab \strong{Stan parameters with priors} \cr
#'   `"none"`                 \tab `ar{lag}_trend` per series \cr
#'   `"shared"`               \tab `ar{lag}_shared` (one per lag) \cr
#'   `"hierarchical"`         \tab `mu_ar{lag}_trend`, `sigma_ar{lag}_trend`, `ar{lag}_trend` per series \cr
#' }
#' All variants synthesise the same `ar{lag}_trend[j]` symbol
#' in `transformed parameters`, so downstream code
#' (forecasting, IRF, FEVD, summary printing) is unchanged.
#' Custom priors can be set on any sampled parameter via the
#' standard `brms::set_prior(class = "<name>")` route; call
#' `get_prior(mvgam_formula(...))` to see the exact parameter
#' set surfaced by the current `coef_sharing` value.
#'
#' @param n_lv The number of latent factors to estimate for dynamic
#'   factor models. When `n_lv` is smaller than the number of series,
#'   the latent processes are modelled as `n_lv` factors with estimated
#'   loadings onto the series. Defaults to `NULL`, in which case one
#'   latent process is used per series.
#' @param trend_map Optional `data.frame` specifying which latent
#'   process each series maps onto, giving fixed (rather than estimated)
#'   loadings for dynamic factor models. See \code{\link{mvgam}} for the
#'   required format. Defaults to `NULL`.
#'
#' @rdname trend_constructors
#'
#' @details Use `vignette("mvgam_overview")` to see the full details of
#'   available stochastic trend types in \pkg{mvgam}, or view the rendered
#'   version on the package website at:
#'   https://nicholasjclark.github.io/mvgam/articles/mvgam_overview.html
#'
#'   For a worked example that interrogates a fitted `VAR()` trend
#'   with `irf()`, `fevd()` and `stability()`, see
#'   [https://nicholasjclark.github.io/mvgam/articles/var.html](https://nicholasjclark.github.io/mvgam/articles/var.html).
#'   For the `trend_map` shortcut used to fit dynamic factor models
#'   with fewer latent processes than observed series, see
#'   `vignette("dfm", package = "mvgam")`.
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
#' @section Identification:
#' Factor-model fits (\code{n_lv < n_series}) sample the loadings
#' matrix `Z` unconstrained and identify it post-hoc via thin QR
#' decomposition in generated quantities, following Heaps & Jermyn
#' (2024). The identified loadings `Z_tilde` and rotated factor
#' paths `lv_trend_tilde` (for AR / RW / VAR) are saved alongside
#' the unrotated `Z` and `lv_trend`; downstream resolvers prefer
#' the identified versions when present. \code{qr_thin_R()}
#' guarantees a non-negative diagonal on `Z_tilde`, removing the
#' \eqn{2^k} sign-mode equivalence by construction. Per-factor
#' scalar parameters (`ar1_trend`, `sigma_trend`, `theta1_trend`,
#' `L_Omega_trend`) remain in the unrotated latent basis; for
#' VAR-trend factor models the lag-coefficient array also rotates
#' (`A_trend_tilde[lag] = Q_tilde * A_trend[lag] * Q_tilde'`).
#'
#' Supplying \code{trend_map} bypasses the QR identification step
#' entirely so the user-encoded fixed entries are preserved exactly
#' on `Z`. Combine a free factor model with the optional
#' \code{loadings_prior} argument on \code{mvgam()} to swap the
#' default iid Student-t prior on `Z` for a structured matrix-normal
#' prior built from per-series features and / or pairwise distance
#' matrices; see \code{\link{mvgam}} for the full surface.
#'
#' Setting \code{loadings_prior = "mgp"} (or
#' \code{loadings_prior = list(column_shrinkage = "mgp")}) switches
#' the column scaling to the multiplicative gamma process prior of
#' Bhattacharya & Dunson (2011), which shrinks later columns of `Z`
#' toward zero with increasing strength. Under this prior `n_lv`
#' acts as a truncation ceiling rather than the exact factor count;
#' \code{\link{active_factors}} reports the posterior distribution of
#' the active column count.
#'
#' @references
#' Heaps, S. E. and Jermyn, I. H. (2024). Structured prior
#' distributions for the covariance matrix in latent factor
#' models. \emph{Statistics and Computing}, 34:143.
#' \doi{10.1007/s11222-024-10454-0}
#'
#' Heaps, S. E. (2023). Enforcing stationarity through the prior
#' in vector autoregressions. \emph{Journal of Computational and
#' Graphical Statistics}, 32:74-83. (VAR stationarity prior used
#' for \code{VAR()} trends.)
#'
#' Clark, N. J., Ernest, S. K. M., Senyondo, H., Simonis, J.,
#' White, E. P., Yenni, G. M. and Karunarathna, K. A. N. K.
#' (2025). Beyond single-species models: multispecies forecasts
#' for ecological predictability. \emph{PeerJ}, 13:e18929.
#'
#' @author Nicholas J Clark
#'
#' @examples
#' \donttest{
#' # Simulate three Gaussian series driven by a correlated VAR(1)
#' # process so the cross-series dependencies are recoverable.
#' set.seed(0)
#' simdat <- sim_mvgam(
#'   family       = gaussian(),
#'   n_series     = 3L,
#'   n_timepoints = 120L,
#'   trend_model  = VAR(),
#'   prop_trend   = 0.95
#' )
#'
#' # Fit a VAR(1) state-space model. The trend constructor goes
#' # inside `trend_formula`; the obs side carries only the
#' # intercept.
#' mod <- mvgam(
#'   y ~ 1,
#'   trend_formula = ~ VAR(p = 1),
#'   data          = simdat$data_train,
#'   family        = gaussian(),
#'   chains        = 2,
#'   silent        = 2
#' )
#' summary(mod, include_betas = FALSE)
#'
#' # `variable = "trend_params"` is a keyword shortcut that pulls
#' # every trend-dynamics parameter (A_trend, sigma_trend, the
#' # Sigma_trend covariance, etc.) in one call. See ?mvgam_draws
#' # for the full keyword set ("betas", "obs_params",
#' # "smooth_params", "trend_betas", "trend_params",
#' # "trend_smooth_params").
#' mcmc_plot(mod, variable = "trend_params", type = "intervals")
#'
#' # Post-fit sweep on the same VAR(1). `residual_cor()` pulls
#' # the innovation correlation matrix between the three series.
#' # `irf()` shows how a one-time shock to one series ripples
#' # through the others over h steps; `fevd()` reports the share
#' # of each series' forecast-error variance attributable to each
#' # shock; `stability()` summarises the long-run dynamics
#' # (reactivity = peak amplification of a shock; mean_return_rate
#' # = speed of decay back to the stationary distribution).
#' residual_cor(mod)
#' plot(irf(mod, h = 8L), series = 1)
#' plot(fevd(mod, h = 8L))
#' head(stability(mod)[, c("reactivity", "mean_return_rate")])
#'
#' # Forecast the held-out cells. `forecast()` propagates
#' # uncertainty through the VAR; `plot(fc, series = i)` overlays
#' # the in-sample hindcast and the out-of-sample interval.
#' fc <- forecast(mod, newdata = simdat$data_test)
#' plot(fc, series = 1)
#'
#' # Continuous-time AR(1) on irregularly-spaced data. Recipe 6
#' # of sim_mvgam() defaults to CAR() with U(1, 6) time gaps.
#' set.seed(7)
#' simdat_car <- sim_mvgam(
#'   type         = 6L,
#'   family       = gaussian(),
#'   n_series     = 1L,
#'   n_timepoints = 120L
#' )
#'
#' # Fit the CAR(1) model. ar1_trend[1] is the continuous-time
#' # decay parameter and adapts to the per-step time gap.
#' mod_car <- mvgam(
#'   y ~ 1,
#'   trend_formula = ~ CAR(),
#'   data          = simdat_car$data_train,
#'   family        = gaussian(),
#'   chains        = 2,
#'   silent        = 2
#' )
#' mcmc_plot(mod_car, variable = "trend_params", type = "intervals")
#'
#' # Other trend constructors swap in the same place. For example
#' # an AR(1) on a single series:
#' #   trend_formula = ~ AR(p = 1)
#' # or a zero-mean multivariate normal residual prior for cross-
#' # series correlations:
#' #   trend_formula = ~ ZMVN()
#'
#' # Hierarchical VAR. Two regions, two outcomes per region. The
#' # `gr` factor identifies the grouping unit (region) and `subgr`
#' # identifies the within-group dimension (outcome). The fit
#' # estimates a population innovation correlation across outcomes
#' # plus per-region deviations; `alpha_cor_trend` (Beta(2, 2) by
#' # default) controls the partial-pooling blend between them.
#' # Simulate data with a genuine cross-outcome correlation (~0.6)
#' # so the population estimate is recoverable, not floating on
#' # noise.
#' set.seed(3)
#' n_t <- 30L; rho <- 0.6
#' Sigma <- matrix(c(1, rho, rho, 1), 2L, 2L)
#' L <- chol(Sigma)
#' regions <- c("A", "B")
#' hdat <- do.call(rbind, lapply(regions, function(r) {
#'   eps <- matrix(rnorm(n_t * 2L), n_t, 2L) %*% L
#'   data.frame(
#'     time    = rep(seq_len(n_t), 2L),
#'     region  = factor(r, levels = regions),
#'     outcome = factor(rep(c("gdp", "cons"), each = n_t),
#'                       levels = c("gdp", "cons")),
#'     y       = c(eps[, 1L], eps[, 2L])
#'   )
#' }))
#' hdat$series <- factor(paste(hdat$region, hdat$outcome, sep = "_"))
#'
#' mod_hv <- mvgam(
#'   y ~ 1,
#'   trend_formula = ~ VAR(
#'     p = 1, gr = region, subgr = outcome, cor = TRUE
#'   ),
#'   data    = hdat,
#'   family  = gaussian(),
#'   chains  = 1,
#'   iter    = 600,
#'   warmup  = 300,
#'   silent  = 2
#' )
#'
#' # Marginal posterior for the pooling weight. Mass near 1 means
#' # the per-region correlations track the population correlation;
#' # mass near 0 lets the regions diverge.
#' mcmc_plot(mod_hv, variable = "alpha_cor_trend", type = "hist")
#'
#' # `residual_cor()` defaults to the population (across-outcome)
#' # correlation. Passing `groups = TRUE` returns a list of
#' # per-region correlations alongside the population entry, so
#' # the across-region heterogeneity can be inspected directly.
#' residual_cor(mod_hv)
#' residual_cor(mod_hv, groups = TRUE)
#' }
#'
#' @export
RW = function(
    time = NA,
    series = NA,
    ma = FALSE,
    cor = FALSE,
    gr = NA,
    subgr = NA,
    n_lv = NULL,
    trend_map = NULL,
    df = Inf) {

  # Basic input validation for trend-specific parameters
  checkmate::assert_logical(ma, len = 1)
  checkmate::assert_logical(cor, len = 1)
  assert_trend_map_input(trend_map)

  # Use helper function for clean object creation
  # Complex logic (grouping, correlation requirements, parameter processing)
  # moved to validation and Stan assembly layers. Raw `trend_map`
  # input is stashed on the spec; normalisation via
  # `normalise_trend_map()` happens at fit time when data is in
  # scope.
  trend_obj <- create_mvgam_trend(
    "RW",  # Base trend type used for ALL dispatch
    df = assert_trend_df(df),
    .time = substitute(time),
    .series = substitute(series),
    .gr = substitute(gr),
    .subgr = substitute(subgr),
    # Store parameters as-is (processing moved to Stan assembly)
    ma = ma,
    cor = cor,
    n_lv = n_lv,
    trend_map = trend_map
  )

  # Validate the assembled trend object
  validate_mvgam_trend(trend_obj)

  return(trend_obj)
}

#' @rdname trend_constructors
#' @export
AR = function(time = NA, series = NA, p = 1, ma = FALSE, cor = FALSE,
              gr = NA, subgr = NA, n_lv = NULL, trend_map = NULL,
              coef_sharing = c("none", "shared", "hierarchical"),
              df = Inf) {
  # Validate AR order parameter
  if (length(p) == 1) {
    checkmate::assert_int(p, lower = 1)
  } else {
    checkmate::assert_integerish(p, lower = 1, unique = TRUE, sorted = TRUE)
  }

  # Basic input validation
  checkmate::assert_logical(ma, len = 1)
  checkmate::assert_logical(cor, len = 1)
  assert_trend_map_input(trend_map)
  coef_sharing <- match.arg(coef_sharing)

  # Use helper function for clean object creation. Raw
  # `trend_map` is stashed on the spec; normalisation happens at
  # fit time via `normalise_trend_map()`.
  trend_obj <- create_mvgam_trend(
    "AR",  # Base trend type used for ALL dispatch
    df = assert_trend_df(df),
    .time = substitute(time),
    .series = substitute(series),
    .gr = substitute(gr),
    .subgr = substitute(subgr),
    # Store parameters as-is (processing happens in validation/Stan assembly)
    p = p,
    ma = ma,
    cor = cor,
    n_lv = n_lv,
    trend_map = trend_map,
    coef_sharing = coef_sharing
  )

  return(trend_obj)
}

#' @rdname trend_constructors
#' @export
CAR = function(time = NA, series = NA, df = Inf) {
  # CAR only supports first-order continuous autoregression (p=1)
  # Use helper function for clean object creation
  # All validation logic moved to validation layer (handles irregular time intervals, etc.)
  create_mvgam_trend(
    "CAR",  # Base trend type used for ALL dispatch
    df = assert_trend_df(df),
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
VAR = function(time = NA, series = NA, p = 1, ma = FALSE, cor = TRUE,
               gr = NA, subgr = NA, n_lv = NULL, trend_map = NULL) {
  # VAR is by definition multivariate with correlated innovations;
  # `cor` is accepted for API symmetry with AR / RW / ZMVN but
  # cannot be FALSE. Users who want independent per-series
  # innovations should reach for AR() instead.
  if (isFALSE(cor)) {
    stop(insight::format_error(c(
      "VAR(cor = FALSE) is not supported.",
      x = paste0(
        "VAR processes are correlated by definition; the ",
        "innovation covariance matrix is part of the model."
      ),
      i = paste0(
        "Use AR(p = ", p, ") if you want independent per-series ",
        "autoregressions."
      )
    )))
  }
  # Validate VAR order parameter. Scalar p (e.g. p = 2) is the
  # standard interpretation: include AR coefficient matrices
  # for consecutive lags 1..p. Sparse-lag vector p (e.g.
  # p = c(2, 4)) is not supported. Reason: the Heaps-2022
  # stationary joint-distribution initialisation that VAR uses
  # assumes consecutive companion-form structure, so the sparse
  # case has no companion-form analogue with the same identified
  # stationary covariance. Use AR(p = c(...)) for sparse-lag
  # autoregression on a single series.
  if (length(p) != 1L) {
    stop(insight::format_error(c(
      paste0(
        "Sparse-lag VAR (vector 'p') is not supported."
      ),
      x = paste0(
        "Got 'p' of length ", length(p), ": ",
        paste(p, collapse = ", "), "."
      ),
      i = paste0(
        "Pass a scalar 'p' (e.g. p = 2) for consecutive lags ",
        "1..p. Use AR(p = c(...)) for sparse-lag autoregression ",
        "on a single series."
      )
    )))
  }
  checkmate::assert_int(p, lower = 1)

  # Basic input validation
  checkmate::assert_logical(ma, len = 1)
  assert_trend_map_input(trend_map)

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
    n_lv = n_lv,
    trend_map = trend_map
  )

  return(trend_obj)
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
#'   any time variable name without requiring explicit "time" columns.
#'   When using the default, a one-time warning will be issued.
#'
#' @param series The unquoted name of the variable that represents the series
#'   identifier in the supplied `data`. This variable should be either a
#'   `character` or `factor` variable. Defaults to `series` following mvgam
#'   conventions, allowing any series variable name. When using the
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
#' @inheritParams AR
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
#' # Linear PW on a single Poisson series. `y ~ -1` removes the
#' # observation intercept so the PW trend's `m_trend` parameter
#' # is the unique constant offset (otherwise the two compete for
#' # the same constant on the link scale).
#' set.seed(2024)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L)
#'
#' mod <- mvgam(
#'   y ~ -1,
#'   trend_formula = ~ PW(growth = "linear"),
#'   data          = simdat$data_train,
#'   family        = poisson(),
#'   chains        = 2,
#'   silent        = 2
#' )
#' summary(mod, include_betas = FALSE)
#'
#' # PW exposes the base growth rate (k_trend), offset (m_trend)
#' # and the changepoint rate deltas (delta_trend[changepoint,
#' # series]). Visualise the full set with the trend_params
#' # keyword.
#' mcmc_plot(mod, variable = "trend_params", type = "intervals")
#' }
#'
#' @export
PW = function(time = NA, series = NA, cap = NA, n_changepoints = 10,
              changepoint_range = 0.8, changepoint_scale = 0.05,
              growth = 'linear', n_lv = NULL, trend_map = NULL) {
  # Validate arguments
  growth <- match.arg(growth, choices = c('linear', 'logistic'))
  checkmate::assert_number(changepoint_range, lower = 0, upper = 1)
  checkmate::assert_int(n_changepoints, lower = 1)
  checkmate::assert_number(changepoint_scale, lower = 0)

  # PW doesn't support factor models. Reject n_lv and the
  # user-facing fixed-loadings surface (trend_map) separately so
  # the error names exactly what the user supplied.
  if (!is.null(n_lv)) {
    stop(insight::format_error(c(
      cli::format_inline(
        "Factor models ({.field n_lv}) not supported for PW trends."
      ),
      x = "Piecewise trends require series-specific changepoint modeling.",
      i = cli::format_inline(
        "Remove {.field n_lv} parameter or use factor-compatible trends: AR, RW, VAR, ZMVN"
      )
    )), call. = FALSE)
  }
  if (!is.null(trend_map)) {
    stop(insight::format_error(c(
      cli::format_inline(
        "Factor-loading specification {.field trend_map} not supported for PW trends."
      ),
      x = "Piecewise trends define series-specific changepoint dynamics.",
      i = cli::format_inline(
        "Drop {.field trend_map} or use a factor-compatible trend: AR, RW, VAR, ZMVN."
      )
    )), call. = FALSE)
  }

  # Check for required cap variable in logistic models
  cap_expr <- substitute(cap)
  if (growth == 'logistic' && identical(cap_expr, quote(NA))) {
    stop(insight::format_error(c(
      cli::format_inline(
        "Logistic growth models require a {.field cap} variable."
      ),
      x = cli::format_inline(
        "Either provide {.field cap} argument or ensure 'cap' column exists in data."
      ),
      i = "Example: PW(cap = carrying_capacity, growth = 'logistic')"
    )), call. = FALSE)
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
#' @inheritParams AR
#' @param time The unquoted name of the variable that represents the unit of
#'   analysis in `data` over which latent residuals should be correlated. This
#'   variable should be either a `numeric` or `integer` variable in the
#'   supplied `data`. Defaults to `time` to be consistent with other
#'   functionalities in \pkg{mvgam}, though note that the data need not be time
#'   series in this case. See examples below for further details and
#'   explanations.
#'
#'   Unlike \code{\link{AR}} / \code{\link{VAR}} / \code{\link{RW}}, ZMVN
#'   does not require the `unit` values to be regularly spaced. The
#'   likelihood is \eqn{x_{u, :} \sim MVN(0, \Sigma)} with \eqn{\Sigma}
#'   indexed by series only, so \eqn{\Delta u} never enters the math.
#'   Use ZMVN for spatial or otherwise non-temporal grouping factors
#'   where gaps in the `unit` axis are natural (e.g. dropped sites,
#'   stratified k-fold refits, irregular sampling grids).
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
#' @section Identification:
#' Factor-model fits (\code{n_lv < n_series}) sample the loadings
#' matrix `Z` unconstrained and identify it post-hoc via thin QR
#' decomposition in generated quantities, following Heaps & Jermyn
#' (2024). The identified loadings `Z_tilde` and rotated factor
#' paths `lv_trend_tilde` are saved alongside `Z` and `lv_trend`;
#' downstream resolvers prefer the identified versions when
#' present. \code{qr_thin_R()} guarantees a non-negative diagonal
#' on `Z_tilde`, removing the \eqn{2^k} sign-mode equivalence by
#' construction. The per-block scale matrices `L_Omega_trend` /
#' `Sigma_trend` remain in the unrotated latent basis.
#'
#' Supplying \code{trend_map} bypasses the QR identification step
#' entirely so the user-encoded fixed entries are preserved exactly
#' on `Z`. Combine a free factor model with the optional
#' \code{loadings_prior} argument on \code{mvgam()} to swap the
#' default iid Student-t prior on `Z` for a structured matrix-normal
#' prior built from per-series features and / or pairwise distance
#' matrices; see \code{\link{mvgam}} for the full surface.
#'
#' Setting \code{loadings_prior = "mgp"} (or
#' \code{loadings_prior = list(column_shrinkage = "mgp")}) switches
#' the column scaling to the multiplicative gamma process prior of
#' Bhattacharya & Dunson (2011), which shrinks later columns of `Z`
#' toward zero with increasing strength. Under this prior `n_lv`
#' acts as a truncation ceiling rather than the exact factor count;
#' \code{\link{active_factors}} reports the posterior distribution of
#' the active column count.
#'
#' @references
#' Heaps, S. E. and Jermyn, I. H. (2024). Structured prior
#' distributions for the covariance matrix in latent factor
#' models. \emph{Statistics and Computing}, 34:143.
#' \doi{10.1007/s11222-024-10454-0}
#'
#' @seealso \code{\link{AR}}, \code{\link{VAR}}, \code{\link{RW}},
#'   \code{\link{CAR}}, \code{\link{mvgam}}, \code{\link{jsdgam}},
#'   \code{\link{residual_cor}}, \code{\link{ordinate}}. The
#'   `dfm` vignette walks through dynamic factor models built with
#'   a partial `trend_map`
#'   (\code{vignette("dfm", package = "mvgam")}); for the
#'   integrated species distribution model pattern that shares one
#'   latent process across several observation families, see
#'   \url{https://nicholasjclark.github.io/mvgam/articles/idm.html}.
#'
#' @examples
#' \donttest{
#' # Simulate four correlated Gaussian series. ZMVN is a single-
#' # snapshot residual prior, so the recoverable structure is the
#' # cross-series covariance.
#' set.seed(2)
#' simdat <- sim_mvgam(
#'   family       = gaussian(),
#'   n_series     = 4L,
#'   n_timepoints = 120L,
#'   trend_model  = VAR(),
#'   prop_trend   = 0.9
#' )
#'
#' mod <- mvgam(
#'   y ~ 1,
#'   trend_formula = ~ ZMVN(),
#'   data          = simdat$data_train,
#'   family        = gaussian(),
#'   chains        = 2,
#'   silent        = 2
#' )
#' summary(mod, include_betas = FALSE)
#'
#' # All trend-side latent dynamics in one summary view. For ZMVN
#' # the meaningful entries are sigma_trend (per-series SDs),
#' # L_Omega_trend (Cholesky of the correlation matrix) and
#' # Sigma_trend (the implied covariance).
#' mcmc_plot(mod, variable = "trend_params", type = "intervals")
#' }
#'
#' @export
ZMVN = function(time = NA, series = NA, gr = NA, subgr = NA,
                 n_lv = NULL, cor = TRUE, trend_map = NULL,
                 df = Inf) {
  # Basic parameter validation for n_lv if provided
  if (!is.null(n_lv)) {
    checkmate::assert_int(n_lv, lower = 1, null.ok = TRUE)
  }
  # `cor` is accepted for API symmetry with AR / VAR but must be
  # TRUE: ZMVN is the zero-mean multivariate normal latent prior
  # and correlated factors are its definitional purpose. Use a
  # different trend type for uncorrelated series-level noise.
  checkmate::assert_flag(cor)
  assert_trend_map_input(trend_map)
  if (!isTRUE(cor)) {
    stop(insight::format_error(c(
      "'cor = FALSE' is not supported for 'ZMVN()'.",
      x = paste0("ZMVN always has correlation structure (it is the ",
                 "zero-mean multivariate normal latent prior)."),
      i = paste0("For uncorrelated series-level noise, use ",
                 "'RW()' or 'AR()' with 'cor = FALSE' instead.")
    )))
  }

  # Use helper function for clean object creation
  # All validation logic moved to validation layer
  create_mvgam_trend(
    "ZMVN",  # Base trend type used for ALL dispatch
    df = assert_trend_df(df),
    .time = substitute(time),
    .series = substitute(series),
    .gr = substitute(gr),
    .subgr = substitute(subgr),
    # Store parameters as-is
    n_lv = n_lv,
    ma = FALSE,   # ZMVN doesn't support MA
    cor = TRUE,   # ZMVN always has correlation structure
    trend_map = trend_map
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
#' @return Trend object with all fields filled
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

  # ZMVN is `x ~ MVN(0, Sigma)` with Sigma parameterised across
  # series only; time enters as a stacking dimension, not as an
  # autoregressive lag. Unlike VAR, the math is invariant to
  # `Delta t`, so the regular-intervals rule should not fire. The
  # remaining multivariate rules (factor support, hierarchical
  # grouping, minimum series count) still apply.
  static_multivariate_trend_rules <- c(
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
    "ZMVN" = static_multivariate_trend_rules,

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
#' @param .cap Optional carrying-capacity variable (quoted or unquoted) for
#'   logistic piecewise trends
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
  if (time_var == "NA") time_var <- "time"
  if (series_var == "NA") series_var <- "series"

  # Handle grouping defaults
  if (gr_var == "NA") gr_var <- "NA"
  if (subgr_var == "NA") subgr_var <- "NA"

  # Handle cap default
  if (cap_var == "NA") cap_var <- "cap"


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

  # Validate the assembled object
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
      stop(insight::format_error(c(
        "Inconsistent forecast function naming.",
        x = cli::format_inline("Expected: {.field {expected_forecast}}"),
        x = cli::format_inline("Got: {.field {actual_forecast}}"),
        i = "All dispatch functions must follow pattern: trend_type + function_suffix"
      )))
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
#' @return Trend object with consistent dispatch metadata
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


# Validate innovation degrees of freedom supplied to a trend
# constructor. `Inf` keeps the Gaussian innovations mvgam has always
# used, since a t with infinite degrees of freedom is a normal. `NA`
# estimates them. A finite value fixes them and must exceed 2: the
# stationary initialisation of an autoregressive trend divides by
# `sqrt(1 - phi^2)`, which presumes the innovations have a finite
# second moment, and a t has one only above 2 degrees of freedom.
#' @noRd
assert_trend_df <- function(df) {
  usage <- paste0(
    "Use 'df = Inf' for Gaussian innovations, 'df = NA' to estimate ",
    "the degrees of freedom, or a number above 2 to fix them."
  )
  if (length(df) != 1L || !(is.numeric(df) || is.logical(df))) {
    stop(insight::format_error(c(
      "Argument 'df' must be a single number, 'Inf' or 'NA'.",
      x = paste0("Got an object of class '", class(df)[1], "' of length ",
                 length(df), "."),
      i = usage
    )))
  }
  if (is.na(df)) {
    return(NA_real_)
  }
  df <- as.numeric(df)
  if (is.infinite(df)) {
    if (df < 0) {
      stop(insight::format_error(c(
        "Argument 'df' must be positive.",
        x = "Got '-Inf'.",
        i = usage
      )))
    }
    return(Inf)
  }
  if (df <= 2) {
    stop(insight::format_error(c(
      "Argument 'df' must be greater than 2.",
      x = paste0("Got 'df = ", df, "'."),
      i = paste0(
        "At or below 2 the innovations have no finite variance, so the ",
        "stationary initialisation of an autoregressive trend is undefined."
      )
    )))
  }
  df
}
