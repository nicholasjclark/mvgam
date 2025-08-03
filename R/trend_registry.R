#' Trend Type Registry for mvgam
#'
#' @description
#' Centralized system for registering and managing trend types in mvgam.
#' Automatically handles factor model compatibility validation and dispatch.

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

#' Validate Factor Model Compatibility
#'
#' @description
#' Check if a trend specification is compatible with factor models.
#'
#' @param trend_spec Trend specification object
#' @return Invisibly returns TRUE if compatible, stops with error if not
#' @noRd
validate_factor_compatibility <- function(trend_spec) {
  # Only validate if n_lv is specified (indicating factor model intent)
  if (is.null(trend_spec$n_lv)) {
    return(invisible(TRUE))
  }
  
  # Check if trend type supports factor models
  trend_info <- get_trend_info(trend_spec$trend_type)
  
  if (!trend_info$supports_factors) {
    stop(insight::format_error(
      paste0("Factor models ({.field n_lv}) not supported for {.field ", trend_spec$trend_type, "} trends."),
      trend_info$incompatibility_reason,
      paste0("Remove {.field n_lv} parameter or use factor-compatible trends: {.val ", paste(get_factor_compatible_trends(), collapse = ", "), "}")
    ))
  }
  
  invisible(TRUE)
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
  register_trend_type("AR", supports_factors = FALSE, generate_ar_trend_stanvars)
  register_trend_type("RW", supports_factors = TRUE, generate_rw_trend_stanvars)
  register_trend_type("VAR", supports_factors = TRUE, generate_var_trend_stanvars)
  register_trend_type("Factor", supports_factors = TRUE, generate_factor_trend_stanvars)
  register_trend_type("None", supports_factors = FALSE, generate_none_trend_stanvars)
  
  # Factor-incompatible trends (series-specific dynamics)
  register_trend_type("CAR", supports_factors = FALSE, generate_car_trend_stanvars)
  
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
  core_trends <- c("AR", "RW", "VAR", "Factor", "None", "CAR")
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