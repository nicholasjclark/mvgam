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
# SECTION 1: PRIOR HELPER FUNCTIONS
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