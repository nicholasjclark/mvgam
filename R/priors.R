#' Prior Specification and Inspection System for mvgam
#'
#' @description
#' Complete prior extraction, combination, and inspection system for mvgam
#' models. This file provides functions for working with priors in both
#' observation and trend components, leveraging the native brms brmsprior
#' class throughout.
#'
#' @section Architecture:
#' The prior system uses brmsprior objects directly for maximum compatibility:
#' - **Extraction Layer**: Get priors from observation and trend models
#' - **Combination Layer**: Merge observation and trend priors seamlessly
#' - **Inspection Layer**: User-facing functions for prior specification
#' - **Validation Layer**: Ensure prior specifications are valid
#'
#' @section Design Decisions:
#' - Uses native brmsprior class throughout (no custom mvgamprior class)
#' - Trend parameters distinguished via _trend suffix convention
#' - Optional attributes for mvgam-specific metadata when needed
#' - Direct pass-through to brms for observation model priors

# =============================================================================
# SECTION 1: COMMON TREND PRIOR SPECIFICATIONS
# =============================================================================
# WHY: Shared prior specifications enable DRY principle for parameters used
# across multiple trend types (e.g., sigma_trend used by RW, AR, CAR trends).
# Centralized definitions ensure consistency and easier maintenance.

#' Common Prior Specifications for Trend Parameters
#'
#' @description
#' Shared prior specifications for trend parameters that are used across
#' multiple trend types. This ensures consistent defaults and reduces
#' duplication in trend registrations. Each specification contains default
#' Stan distribution strings, parameter bounds, descriptions, and dimension
#' information.
#'
#' @format Named list with parameter specifications. Each element is a list with:
#' \describe{
#'   \item{default}{Character string of Stan distribution (e.g., "exponential(2)")}
#'   \item{bounds}{Numeric vector c(lower, upper) with NA for unbounded}
#'   \item{description}{Character string describing the parameter}
#'   \item{dimension}{Character string: "vector", "matrix", or "scalar"}
#' }
#' 
#' Available common parameters:
#' \describe{
#'   \item{sigma_trend}{Innovation standard deviation (RW, AR, CAR trends)}
#'   \item{LV}{Latent variables (all trends with state-space structure)}
#'   \item{ar1_trend}{AR(1) coefficient (AR, CAR trends)}
#'   \item{LV_raw}{Raw latent variables for non-centered parameterization}
#'   \item{Z}{Factor loadings matrix for factor models}
#' }
#' 
#' @examples
#' # Access default for sigma_trend
#' common_trend_priors$sigma_trend$default  # "exponential(2)"
#' 
#' # Get bounds for ar1_trend  
#' common_trend_priors$ar1_trend$bounds     # c(-1, 1)
#' 
#' @seealso \code{\link{register_trend_type}} for using prior specifications
#' @noRd
common_trend_priors <- list(
  sigma_trend = list(
    default = "exponential(2)",
    bounds = c(0, NA),
    description = "Innovation standard deviation",
    dimension = "vector"
  ),
  
  LV = list(
    default = "std_normal()",
    bounds = c(NA, NA),
    description = "Latent variables",
    dimension = "matrix"
  ),
  
  ar1_trend = list(
    default = "normal(0, 0.5)",
    bounds = c(-1, 1),
    description = "AR(1) coefficient",
    dimension = "vector"
  ),
  
  LV_raw = list(
    default = "std_normal()",
    bounds = c(NA, NA),
    description = "Raw latent variables (non-centered)",
    dimension = "matrix"
  ),
  
  Z = list(
    default = "std_normal()",
    bounds = c(NA, NA),
    description = "Factor loadings matrix",
    dimension = "matrix"
  )
)

# =============================================================================
# SECTION 2: PRIOR HELPER FUNCTIONS
# =============================================================================
# WHY: Helper functions for working with brmsprior objects enable consistent
# handling of both observation and trend priors while maintaining full brms
# compatibility. The _trend suffix convention provides clear separation without
# requiring a custom class.

#' Check if Prior Object Contains Trend Priors
#' 
#' @param prior A brmsprior object
#' @return Logical indicating if any trend priors are present
#' @noRd
has_trend_priors <- function(prior) {
  if (!inherits(prior, "brmsprior")) {
    return(FALSE)
  }
  
  # Check for _trend suffix in class column
  any(grepl("_trend$", prior$class))
}

#' Extract Trend Priors from Combined Prior Object
#' 
#' @param prior A brmsprior object containing both observation and trend priors
#' @return A brmsprior object containing only trend priors
#' @noRd
extract_trend_priors_only <- function(prior) {
  checkmate::assert_class(prior, "brmsprior")
  
  trend_rows <- grepl("_trend$", prior$class)
  trend_priors <- prior[trend_rows, , drop = FALSE]
  
  # Maintain brmsprior class
  class(trend_priors) <- class(prior)
  attr(trend_priors, "class2") <- attr(prior, "class2")
  
  return(trend_priors)
}

#' Extract Observation Priors from Combined Prior Object
#' 
#' @param prior A brmsprior object containing both observation and trend priors
#' @return A brmsprior object containing only observation priors
#' @noRd
extract_observation_priors_only <- function(prior) {
  checkmate::assert_class(prior, "brmsprior")
  
  obs_rows <- !grepl("_trend$", prior$class)
  obs_priors <- prior[obs_rows, , drop = FALSE]
  
  # Maintain brmsprior class
  class(obs_priors) <- class(prior)
  attr(obs_priors, "class2") <- attr(prior, "class2")
  
  return(obs_priors)
}

#' Add Trend Component Attribute to Prior Object
#' 
#' @param prior A brmsprior object
#' @param component Character vector indicating component for each row
#' @return The prior object with trend_component attribute added
#' @noRd
add_trend_component_attr <- function(prior, component = NULL) {
  checkmate::assert_class(prior, "brmsprior")
  
  if (is.null(component)) {
    # Auto-detect based on _trend suffix
    component <- ifelse(grepl("_trend$", prior$class), "trend", "observation")
  }
  
  checkmate::assert_character(component, len = nrow(prior))
  attr(prior, "trend_component") <- component
  
  return(prior)
}

# =============================================================================
# SECTION 2: PRIOR EXTRACTION FUNCTIONS
# =============================================================================
# WHY: Separate extraction for observation and trend priors enables modular
# prior specification while maintaining consistency with brms patterns.

#' Extract Observation Model Priors
#' 
#' @param formula Observation model formula
#' @param data Data frame
#' @param family Response distribution family
#' @param ... Additional arguments passed to brms::get_prior
#' @return A brmsprior object with observation model priors
#' @noRd
extract_observation_priors <- function(formula, data, 
                                        family = gaussian(), ...) {
  # Accept both formula and brmsformula objects for distributional models
  if (!inherits(formula, c("formula", "brmsformula", "bform"))) {
    stop(insight::format_error("Formula must be a formula or brmsformula object"))
  }
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_class(family, "family")
  
  # Use brms::get_prior directly for observation model
  obs_priors <- brms::get_prior(
    formula = formula,
    data = data,
    family = family,
    ...
  )
  
  return(obs_priors)
}

#' Extract Trend Model Priors
#' 
#' @param trend_formula Trend formula specification
#' @param data Data frame
#' @param response_names Character vector of response variable names
#' @return A brmsprior object with trend model priors
#' @noRd
extract_trend_priors <- function(trend_formula, data, response_names = NULL) {
  if (!is.null(trend_formula)) {
    checkmate::assert_formula(trend_formula)
  }
  checkmate::assert_data_frame(data, min.rows = 1)
  if (!is.null(response_names)) {
    checkmate::assert_character(response_names, min.len = 1)
  }
  
  if (is.null(trend_formula)) {
    # No trend model - return empty brmsprior
    empty_prior <- data.frame(
      prior = character(0),
      class = character(0),
      coef = character(0),
      group = character(0),
      resp = character(0),
      dpar = character(0),
      nlpar = character(0),
      lb = character(0),
      ub = character(0),
      source = character(0),
      stringsAsFactors = FALSE
    )
    class(empty_prior) <- c("brmsprior", "data.frame")
    return(empty_prior)
  }
  
  # Parse trend formula to determine trend type
  trend_spec <- parse_trend_formula(trend_formula, data)
  
  # Generate priors based on trend type using convention-based dispatch
  trend_priors <- generate_trend_priors(trend_spec, response_names)
  
  return(trend_priors)
}

#' Parse Trend Formula to Extract Trend Specification
#' 
#' @param trend_formula Trend formula or list of trend formulas
#' @param data Data frame for validation
#' @return List containing trend specifications
#' @noRd
parse_trend_formula <- function(trend_formula, data) {
  # This is a placeholder - actual implementation would parse
  # the trend formula to extract trend type and parameters
  # For now, return a basic structure
  
  if (is.list(trend_formula)) {
    # Multivariate case - parse each formula
    trend_specs <- lapply(names(trend_formula), function(resp) {
      spec <- parse_single_trend_formula(trend_formula[[resp]], data)
      spec$response <- resp
      spec
    })
    names(trend_specs) <- names(trend_formula)
    return(trend_specs)
  } else {
    # Univariate case
    return(parse_single_trend_formula(trend_formula, data))
  }
}

#' Parse Single Trend Formula
#' 
#' @param formula Single trend formula
#' @param data Data frame
#' @return List with trend type and parameters
#' @noRd
# NOTE: This function has been removed as it's replaced by the actual
# mvgam trend parsing system. The parse_trend_formula() function now
# calls the real trend system in trend_system.R which provides the
# complete mvgam_trend objects with monitor_params metadata.

#' Generate Trend Priors from Monitor Parameters
#' 
#' @param trend_spec Trend specification from parse_trend_formula
#' @param response_names Character vector of response names for
#'   multivariate models
#' @return A brmsprior object with trend priors
#' @noRd
generate_trend_priors <- function(trend_spec, response_names = NULL) {
  checkmate::assert_list(trend_spec)
  if (!is.null(response_names)) {
    checkmate::assert_character(response_names, min.len = 1)
  }
  
  # Handle the actual mvgam trend specification structure
  if (is.list(trend_spec) && "trend_model" %in% names(trend_spec)) {
    # Extract the trend model specification
    trend_model <- trend_spec$trend_model
    
    if (inherits(trend_model, "mvgam_trend")) {
      # Use the new integrated approach: generate priors from monitor_params
      return(generate_trend_priors_from_monitor_params(trend_model))
    }
  }
  
  # Fallback: return empty prior if structure not recognized
  return(create_empty_brmsprior())
}

#' Generate Trend Priors from Monitor Parameters
#' 
#' @description
#' Generate priors for trend parameters using the monitor_params metadata
#' from the trend object. This integrates with the existing trend dispatcher
#' system and automatically works for all trend types.
#' 
#' @param trend_obj A mvgam_trend object with monitor_params metadata
#' @return A brmsprior object with trend priors
#' @noRd
generate_trend_priors_from_monitor_params <- function(trend_obj) {
  checkmate::assert_class(trend_obj, "mvgam_trend")
  
  # Get monitor parameters that need priors
  monitor_params <- trend_obj$monitor_params
  
  if (length(monitor_params) == 0) {
    return(create_empty_brmsprior())
  }
  
  # Generate prior specifications for each parameter
  prior_data <- lapply(monitor_params, function(param) {
    create_trend_parameter_prior(param, trend_obj)
  })
  
  # Combine into data frame
  combined_priors <- do.call(rbind, prior_data)
  
  # Convert to brmsprior object
  class(combined_priors) <- c("brmsprior", "data.frame")
  
  return(combined_priors)
}

#' Create Prior Specification for Single Trend Parameter
#' 
#' @param param_name Character string parameter name (e.g., "ar1_trend")
#' @param trend_obj mvgam_trend object for context
#' @return Single-row data frame with prior specification
#' @noRd
create_trend_parameter_prior <- function(param_name, trend_obj) {
  checkmate::assert_string(param_name)
  checkmate::assert_class(trend_obj, "mvgam_trend")
  
  # Get default prior and bounds for this parameter type
  prior_info <- get_default_trend_parameter_prior(param_name, trend_obj)
  
  # Create single row of brmsprior structure
  data.frame(
    prior = prior_info$prior,
    class = param_name,  # Parameter name becomes the class
    coef = "",
    group = "",
    resp = "",
    dpar = "", 
    nlpar = "",
    lb = prior_info$lb,
    ub = prior_info$ub,
    source = "default",
    stringsAsFactors = FALSE
  )
}

#' Get Default Prior Information for Trend Parameter
#' 
#' @param param_name Character string parameter name
#' @param trend_obj mvgam_trend object for context
#' @return List with prior, lb, ub elements
#' @noRd
get_default_trend_parameter_prior <- function(param_name, trend_obj) {
  checkmate::assert_string(param_name)
  
  # Check for trend-specific customization first
  trend_type <- trend_obj$trend
  custom_function <- paste0("get_", tolower(trend_type), "_parameter_prior")
  
  if (exists(custom_function, mode = "function")) {
    custom_prior <- get(custom_function, mode = "function")
    result <- custom_prior(param_name, trend_obj)
    if (!is.null(result)) {
      return(result)
    }
  }
  
  # Use parameter-type-based defaults
  get_parameter_type_default_prior(param_name)
}

#' Get Default Prior Based on Parameter Type
#' 
#' @param param_name Character string parameter name
#' @return List with prior, lb, ub elements
#' @noRd
get_parameter_type_default_prior <- function(param_name) {
  checkmate::assert_string(param_name)
  
  # Pattern matching for common parameter types
  if (grepl("^ar[0-9]+_trend$", param_name)) {
    # AR coefficients: typically bounded [-1, 1] for stationarity
    return(list(prior = "", lb = "", ub = ""))
  } else if (grepl("sigma.*_trend$", param_name)) {
    # Variance parameters: positive with lower bound
    return(list(prior = "", lb = "0", ub = ""))
  } else if (grepl("L_Omega.*_trend$", param_name)) {
    # Correlation matrix Cholesky factors
    return(list(prior = "", lb = "", ub = ""))
  } else if (grepl("theta.*_trend$", param_name)) {
    # Theta parameters (e.g., CAR): typically bounded [0, 1]
    return(list(prior = "", lb = "0", ub = "1"))
  } else if (grepl("A[0-9]+_trend$", param_name)) {
    # VAR coefficient matrices
    return(list(prior = "", lb = "", ub = ""))
  } else if (grepl(".*_trend$", param_name)) {
    # Generic trend parameter
    return(list(prior = "", lb = "", ub = ""))
  } else if (param_name == "Z") {
    # Factor loading matrix (factor models)
    return(list(prior = "", lb = "", ub = ""))
  } else {
    # Other parameters (non-trend parameters in mixed contexts)
    return(list(prior = "", lb = "", ub = ""))
  }
}

#' Create Empty brmsprior Object
#' 
#' @return Empty brmsprior data frame
#' @noRd
create_empty_brmsprior <- function() {
  empty_prior <- data.frame(
    prior = character(0),
    class = character(0),
    coef = character(0),
    group = character(0),
    resp = character(0),
    dpar = character(0),
    nlpar = character(0),
    lb = character(0),
    ub = character(0),
    source = character(0),
    stringsAsFactors = FALSE
  )
  class(empty_prior) <- c("brmsprior", "data.frame")
  return(empty_prior)
}

# =============================================================================
# SECTION 3: TREND-SPECIFIC PRIOR CUSTOMIZATION (OPTIONAL)
# =============================================================================
# WHY: While the integrated system handles most cases automatically via
# monitor_params, some trends may need custom prior logic. These functions
# provide trend-specific customization when the default parameter-type-based
# approach isn't sufficient.

# Note: These functions are optional. If they don't exist, the system falls
# back to parameter-type-based defaults. This provides flexibility while
# maintaining the convention-based approach.

#' Get AR-Specific Parameter Prior (Optional Customization)
#' 
#' @param param_name Character string parameter name
#' @param trend_obj mvgam_trend object
#' @return List with prior, lb, ub elements, or NULL for default handling
#' @noRd
get_ar_parameter_prior <- function(param_name, trend_obj) {
  # AR trends can have custom logic for stationarity constraints
  if (grepl("^ar[0-9]+_trend$", param_name)) {
    # For AR coefficients, we might want tighter bounds for stability
    return(list(prior = "", lb = "-0.99", ub = "0.99"))
  }
  
  # Return NULL to use default parameter-type handling
  return(NULL)
}

#' Get CAR-Specific Parameter Prior (Optional Customization)
#' 
#' @param param_name Character string parameter name  
#' @param trend_obj mvgam_trend object
#' @return List with prior, lb, ub elements, or NULL for default handling
#' @noRd
get_car_parameter_prior <- function(param_name, trend_obj) {
  # CAR has some special parameter handling
  if (param_name == "ar1") {
    # Legacy CAR parameter without _trend suffix
    return(list(prior = "", lb = "0", ub = "1"))
  }
  
  # Return NULL to use default parameter-type handling
  return(NULL)
}

# =============================================================================
# SECTION 4: PRIOR COMBINATION FUNCTIONS
# =============================================================================
# WHY: Combining observation and trend priors into a single brmsprior object
# enables seamless use with brms functions while maintaining clear separation
# via the _trend suffix convention.

#' Combine Observation and Trend Priors
#' 
#' @param obs_priors A brmsprior object with observation model priors
#' @param trend_priors A brmsprior object with trend model priors
#' @return A combined brmsprior object
#' @noRd
combine_obs_trend_priors <- function(obs_priors, trend_priors) {
  checkmate::assert_class(obs_priors, "brmsprior")
  checkmate::assert_class(trend_priors, "brmsprior")
  checkmate::assert_data_frame(obs_priors, min.rows = 0)
  checkmate::assert_data_frame(trend_priors, min.rows = 0)
  
  # Simple row binding maintains brmsprior structure
  combined <- rbind(obs_priors, trend_priors)
  
  # Preserve brmsprior class and attributes
  class(combined) <- c("brmsprior", "data.frame")
  attr(combined, "class2") <- attr(obs_priors, "class2")
  
  # Optionally add trend_component attribute for easy filtering
  combined <- add_trend_component_attr(combined)
  
  return(combined)
}

#' Get Complete Prior Specification for a Trend Type
#'
#' @description
#' Retrieves the complete prior specification for a given trend type by
#' merging trend-specific priors from the registry with shared defaults
#' from common_trend_priors. This enables trends to override common defaults
#' where needed while inheriting shared specifications.
#'
#' @param trend_type Character string specifying the trend type (e.g., "AR", "RW")
#' @return Named list of prior specifications, or NULL if trend type not found
#' 
#' @details
#' The function works by:
#' 1. Retrieving trend-specific prior_spec from the trend registry
#' 2. For each parameter, using trend-specific specification if available
#' 3. Falling back to common_trend_priors for parameters not specified
#' 4. Returning NULL if the trend type is not registered
#' 
#' @examples
#' # Get prior spec for RW trend (hypothetical)
#' rw_spec <- get_trend_prior_spec("RW")
#' rw_spec$sigma_trend$default  # "exponential(2)" from common_trend_priors
#' 
#' @seealso \code{\link{register_trend_type}}, \code{common_trend_priors}
#' @noRd
get_trend_prior_spec <- function(trend_type) {
  checkmate::assert_character(trend_type, len = 1, min.chars = 1, 
                             any.missing = FALSE)
  
  # Get trend info from registry
  if (!exists(trend_type, envir = trend_registry)) {
    return(NULL)
  }
  
  trend_info <- get(trend_type, envir = trend_registry)
  trend_specific_priors <- trend_info$prior_spec
  
  # Start with empty result
  result <- list()
  
  # If trend has specific prior specifications, use them
  if (!is.null(trend_specific_priors)) {
    result <- trend_specific_priors
  }
  
  # For any parameters not specified by trend, check if they exist in common priors
  # This allows trends to inherit common specifications they don't override
  if (length(result) > 0) {
    param_names <- names(result)
    for (param_name in param_names) {
      # If parameter references common_trend_priors, resolve it
      if (is.character(result[[param_name]]) && 
          length(result[[param_name]]) == 1 && 
          startsWith(result[[param_name]], "common_trend_priors.")) {
        common_param <- sub("common_trend_priors\\.", "", result[[param_name]])
        if (common_param %in% names(common_trend_priors)) {
          result[[param_name]] <- common_trend_priors[[common_param]]
        }
      }
    }
  }
  
  return(result)
}

#' Build Dynamic AR Prior Specification for Non-Continuous Lags
#'
#' @description
#' Generates prior specifications for AR models with arbitrary lag structures,
#' including non-continuous lags like AR(p = c(1, 12, 24)). Creates individual
#' ar{lag}_trend specifications for each lag while sharing common parameters.
#'
#' @param lags Numeric vector of lag values (e.g., c(1, 12, 24))
#' @param ar_prior_base Named list with AR coefficient prior specification 
#'   (defaults to common_trend_priors$ar1_trend)
#' @param include_sigma Logical indicating whether to include sigma_trend 
#'   (defaults to TRUE)
#' @param include_common Logical indicating whether to include common parameters
#'   like LV (defaults to TRUE)
#'   
#' @return Named list of prior specifications for all AR parameters
#' 
#' @details
#' For AR(p = c(1, 12, 24)), this creates:
#' - ar1_trend: AR coefficient for lag 1
#' - ar12_trend: AR coefficient for lag 12  
#' - ar24_trend: AR coefficient for lag 24
#' - sigma_trend: Innovation standard deviation (if include_sigma = TRUE)
#' - LV, LV_raw: Latent variable specifications (if include_common = TRUE)
#' 
#' @examples
#' # Standard AR(1) specification
#' ar1_spec <- build_ar_prior_spec(1)
#' 
#' # Seasonal AR with lags 1, 12, 24
#' seasonal_spec <- build_ar_prior_spec(c(1, 12, 24))
#' 
#' # Custom AR coefficient prior
#' custom_spec <- build_ar_prior_spec(
#'   lags = c(1, 4), 
#'   ar_prior_base = list(
#'     default = "normal(0, 0.3)",
#'     bounds = c(-0.8, 0.8),
#'     description = "Constrained AR coefficient"
#'   )
#' )
#' 
#' @seealso \code{\link{get_trend_prior_spec}}, \code{common_trend_priors}
#' @noRd
build_ar_prior_spec <- function(lags, ar_prior_base = NULL, 
                               include_sigma = TRUE, 
                               include_common = TRUE) {
  checkmate::assert_numeric(lags, min.len = 1, any.missing = FALSE, 
                           finite = TRUE)
  checkmate::assert_list(ar_prior_base, null.ok = TRUE, names = "named")
  checkmate::assert_logical(include_sigma, len = 1, any.missing = FALSE)
  checkmate::assert_logical(include_common, len = 1, any.missing = FALSE)
  
  # Validate lags are positive integers
  if (any(lags <= 0) || any(lags != as.integer(lags))) {
    stop(insight::format_error(
      "Invalid lag specification",
      "All lags must be positive integers"
    ))
  }
  
  # Use default AR prior if not specified
  if (is.null(ar_prior_base)) {
    ar_prior_base <- common_trend_priors$ar1_trend
  }
  
  # Validate ar_prior_base structure
  required_fields <- c("default", "bounds", "description")
  missing_fields <- setdiff(required_fields, names(ar_prior_base))
  if (length(missing_fields) > 0) {
    stop(insight::format_error(
      "Invalid ar_prior_base specification",
      paste0("Missing required fields: {.val ", 
             paste(missing_fields, collapse = ", "), "}")
    ))
  }
  
  result <- list()
  
  # Generate ar{lag}_trend specifications for each lag
  for (lag in lags) {
    param_name <- paste0("ar", lag, "_trend")
    result[[param_name]] <- list(
      default = ar_prior_base$default,
      bounds = ar_prior_base$bounds,
      description = paste0("AR(", lag, ") coefficient"),
      dimension = ar_prior_base$dimension %||% "vector"
    )
  }
  
  # Add sigma_trend if requested
  if (include_sigma) {
    result$sigma_trend <- common_trend_priors$sigma_trend
  }
  
  # Add common trend parameters if requested
  if (include_common) {
    result$LV <- common_trend_priors$LV
    result$LV_raw <- common_trend_priors$LV_raw
  }
  
  return(result)
}

# =============================================================================
# SECTION 5: UTILITY FUNCTIONS
# =============================================================================
# WHY: Utility functions provide common operations needed across the prior
# system.

#' Null-coalescing Operator
#' 
#' @param x Value to check
#' @param y Default value if x is NULL
#' @return x if not NULL, otherwise y
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}