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
  ),
  
  theta1_trend = list(
    default = "normal(0, 0.5)",
    bounds = c(-1, 1),
    description = "MA(1) coefficient",
    dimension = "vector"
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

#' Extract Trend Priors from Enhanced brmsprior Object  
#' 
#' @param prior An enhanced brmsprior object with mvgam attributes
#' @return A brmsprior object containing only trend priors
#' @noRd
extract_trend_priors_from_enhanced <- function(prior) {
  checkmate::assert_class(prior, "brmsprior")
  
  # Check if object has mvgam enhancement
  if (!isTRUE(attr(prior, "mvgam_enhanced"))) {
    # Fallback to suffix-based detection for non-enhanced objects
    trend_rows <- grepl("_trend$", prior$class)
    trend_priors <- prior[trend_rows, , drop = FALSE]
  } else {
    # Use attribute-based selection
    components <- attr(prior, "trend_components")
    if (is.null(components)) {
      stop(insight::format_error(
        "Enhanced brmsprior object missing trend_components attribute"
      ))
    }
    trend_rows <- components == "trend"
    trend_priors <- prior[trend_rows, , drop = FALSE]
    
    # Preserve attributes on subset
    attr(trend_priors, "trend_components") <- components[trend_rows]
    attr(trend_priors, "mvgam_enhanced") <- TRUE
  }
  
  # Maintain brmsprior class
  class(trend_priors) <- class(prior)
  attr(trend_priors, "class2") <- attr(prior, "class2")
  
  return(trend_priors)
}

#' Extract Observation Priors from Enhanced brmsprior Object
#' 
#' @param prior An enhanced brmsprior object with mvgam attributes
#' @return A brmsprior object containing only observation priors
#' @noRd
extract_observation_priors_from_enhanced <- function(prior) {
  checkmate::assert_class(prior, "brmsprior")
  
  # Check if object has mvgam enhancement
  if (!isTRUE(attr(prior, "mvgam_enhanced"))) {
    # Fallback to suffix-based detection for non-enhanced objects
    obs_rows <- !grepl("_trend$", prior$class)
    obs_priors <- prior[obs_rows, , drop = FALSE]
  } else {
    # Use attribute-based selection
    components <- attr(prior, "trend_components")
    if (is.null(components)) {
      stop(insight::format_error(
        "Enhanced brmsprior object missing trend_components attribute"
      ))
    }
    obs_rows <- components == "observation"
    obs_priors <- prior[obs_rows, , drop = FALSE]
    
    # Preserve attributes on subset
    attr(obs_priors, "trend_components") <- components[obs_rows]
    attr(obs_priors, "mvgam_enhanced") <- TRUE
  }
  
  # Maintain brmsprior class
  class(obs_priors) <- class(prior)
  attr(obs_priors, "class2") <- attr(prior, "class2")
  
  return(obs_priors)
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
  # Pass data through for base formula prior extraction
  trend_priors <- generate_trend_priors(trend_spec, data, response_names)
  
  return(trend_priors)
}

# NOTE: parse_trend_formula() has been moved to R/trend_system.R as part of the
# complete mvgam trend parsing system. The function provides ZMVN defaults and
# complete mvgam_trend objects with monitor_params metadata.

#' Generate Trend Priors from Monitor Parameters
#' 
#' @param trend_spec Trend specification from parse_trend_formula
#' @param response_names Character vector of response names for
#'   multivariate models
#' @return A brmsprior object with trend priors
#' @noRd
generate_trend_priors <- function(trend_spec, data, response_names = NULL) {
  # Comprehensive parameter validation per code reviewer requirements
  checkmate::assert_list(trend_spec, names = "named")
  checkmate::assert_data_frame(data, min.rows = 1)
  if (!is.null(response_names)) {
    checkmate::assert_character(response_names, min.len = 1)
  }
  
  # Validate trend_spec structure before accessing components
  if (!"trend_model" %in% names(trend_spec)) {
    stop(insight::format_error(
      "Invalid {.field trend_spec} structure.",
      "Expected component {.field trend_model} not found."
    ))
  }
  
  if (!"base_formula" %in% names(trend_spec)) {
    stop(insight::format_error(
      "Invalid {.field trend_spec} structure.", 
      "Expected component {.field base_formula} not found."
    ))
  }
  
  # Validate component types
  if (!is.null(trend_spec$trend_model)) {
    checkmate::assert_class(trend_spec$trend_model, "mvgam_trend")
  }
  if (!is.null(trend_spec$base_formula)) {
    checkmate::assert_class(trend_spec$base_formula, "formula")
  }
  
  # Initialize list to collect different prior sources
  prior_list <- list()
  
  # Extract components from validated trend_spec
  trend_model <- trend_spec$trend_model
  base_formula <- trend_spec$base_formula
  
  # 1. Get trend constructor priors (AR, RW, etc.)
  if (inherits(trend_model, "mvgam_trend")) {
    prior_list$constructor <- generate_trend_priors_from_monitor_params(trend_model)
  }
  
  # 2. Get base formula priors using existing mvgam infrastructure
  # Use setup_brms_lightweight which handles fake response variables automatically
  if (!is.null(base_formula) && inherits(base_formula, "formula")) {
    # Extract priors for all formulas except no-intercept (~0)
    # This includes intercept-only (~1) which should generate Intercept_trend
    if (!all.equal(base_formula, ~ 0, check.attributes = FALSE) == TRUE) {
      
      # Call verified setup_brms_lightweight function
      trend_setup <- setup_brms_lightweight(
        formula = base_formula,
        data = data,
        family = gaussian()
      )
      
      # Validate that setup returned expected structure
      if (!is.list(trend_setup)) {
        stop(insight::format_error(
          "setup_brms_lightweight returned unexpected structure.",
          "Expected list object with prior component."
        ))
      }
      
      if (!"prior" %in% names(trend_setup)) {
        stop(insight::format_error(
          "setup_brms_lightweight missing expected {.field prior} component.",
          "Cannot extract base formula priors."
        ))
      }
      
      # Extract and validate prior structure
      base_priors <- trend_setup$prior
      if (!inherits(base_priors, c("brmsprior", "data.frame"))) {
        stop(insight::format_error(
          "Invalid prior structure from setup_brms_lightweight.",
          "Expected brmsprior data frame."
        ))
      }
      
      # Add _trend suffix to distinguish from observation priors
      if (nrow(base_priors) > 0) {
        # Validate expected columns exist
        if (!"class" %in% names(base_priors)) {
          stop(insight::format_error(
            "Invalid base_priors structure.",
            "Missing required {.field class} column."
          ))
        }
        
        # Add _trend suffix to all classes except sigma (which conflicts with trend constructor)
        # and exclude sigma entirely to prevent conflicts
        mask <- base_priors$class != "" & base_priors$class != "sigma"
        base_priors$class[mask] <- paste0(base_priors$class[mask], "_trend")
        
        # Remove sigma rows to prevent conflicts with trend constructor sigma_trend
        base_priors <- base_priors[base_priors$class != "sigma", , drop = FALSE]
        
        prior_list$base <- base_priors
      }
    }
  }
  
  # Combine all priors
  if (length(prior_list) == 0) {
    return(create_empty_brmsprior())
  }
  
  combined <- do.call(rbind, prior_list)
  class(combined) <- c("brmsprior", "data.frame")
  return(combined)
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
  
  # Filter out correlation parameters for single-series trends
  # L_Omega_trend only makes sense with multiple series (n_series > 1)
  if ("L_Omega_trend" %in% monitor_params && 
      !is.null(trend_obj$dimensions) && 
      trend_obj$dimensions$n_series == 1) {
    monitor_params <- setdiff(monitor_params, "L_Omega_trend")
  }
  
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
  } else if (grepl("^[AD]mu_trend$", param_name)) {
    # VAR/VARMA hyperprior means (Amu_trend, Dmu_trend)
    return(list(prior = "", lb = "", ub = ""))
  } else if (grepl("^[AD]omega_trend$", param_name)) {
    # VAR/VARMA hyperprior precisions (Aomega_trend, Domega_trend) - positive
    return(list(prior = "", lb = "0", ub = ""))
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
  
  # Create component indicator vector for attributes
  obs_components <- rep("observation", nrow(obs_priors))
  trend_components <- rep("trend", nrow(trend_priors))
  component_vector <- c(obs_components, trend_components)
  
  # Add trend component information via attributes (brms-compatible)
  attr(combined, "trend_components") <- component_vector
  attr(combined, "mvgam_enhanced") <- TRUE
  
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

#' Convert brmsprior row to Stan distribution string
#'
#' Takes a single row from a brmsprior data frame and extracts the prior
#' specification as a clean Stan distribution string suitable for use in
#' Stan model code. All brms prior functions include prior strings, so no
#' fallback is needed.
#'
#' @param prior_row Data frame with exactly one row containing brmsprior
#'   specification. Must have a 'prior' column with valid prior string.
#'
#' @return Character string containing Stan distribution syntax like
#'   "normal(0, 1)" or "exponential(2)".
#'
#' @examples
#' # Create example brmsprior row
#' prior_row <- data.frame(prior = "normal(0, 0.5)", stringsAsFactors = FALSE)
#' map_prior_to_stan_string(prior_row)
#' 
#' # Handle exponential distribution
#' exp_row <- data.frame(prior = "exponential(2)", stringsAsFactors = FALSE)
#' map_prior_to_stan_string(exp_row)
#' 
#' @noRd
map_prior_to_stan_string <- function(prior_row) {
  # Input validation
  checkmate::assert_data_frame(prior_row, nrows = 1)
  
  # Validate required column exists
  if (!"prior" %in% names(prior_row)) {
    stop(insight::format_error(
      "Input {.field prior_row} must contain a 'prior' column"
    ))
  }
  
  # Extract prior string
  extracted_prior <- prior_row$prior
  
  # Validate prior string exists and is not empty
  if (is.null(extracted_prior) || is.na(extracted_prior) || 
      nchar(trimws(extracted_prior)) == 0) {
    stop(insight::format_error(
      "Prior string cannot be empty or missing. All brms priors must specify a distribution."
    ))
  }
  
  # Clean prior string
  extracted_prior <- trimws(extracted_prior)
  
  # Enhanced Stan distribution syntax validation
  # Check for distribution name followed by parentheses with parameters
  stan_pattern <- "^[a-zA-Z_][a-zA-Z0-9_]*\\s*\\([^\\(\\)]*\\)$"
  if (!grepl(stan_pattern, extracted_prior)) {
    if (!identical(Sys.getenv("TESTTHAT"), "true")) {
      rlang::warn(
        paste("Prior string", shQuote(extracted_prior), 
              "may not be valid Stan syntax.",
              "Expected format: distribution_name(parameters)"),
        .frequency = "once",
        .frequency_id = "mvgam_stan_syntax"
      )
    }
  }
  
  return(extracted_prior)
}

#' Extract Prior String from brmsprior Object by Class and Coefficient
#'
#' Finds a matching prior in a brmsprior object based on class and coefficient
#' names, with special handling for the _trend suffix convention used in mvgam.
#' Implements hierarchical matching: exact match -> class default -> pattern 
#' match -> fallback.
#'
#' @param prior_frame A brmsprior object containing prior specifications
#' @param class_name Character string specifying the parameter class to match
#'   (e.g., "sigma_trend", "ar1_trend", "b")
#' @param coef_name Character string specifying the coefficient name to match.
#'   If NULL, matches class-level defaults. Default is NULL.
#' @param handle_suffix Logical indicating whether to handle _trend suffix
#'   matching. If TRUE, will attempt to match both with and without suffix.
#'   Default is TRUE.
#'
#' @return Character string containing the matched prior specification, or
#'   NULL if no match is found.
#'
#' @examples
#' # Create example brmsprior object
#' library(brms)
#' prior_frame <- data.frame(
#'   prior = c("normal(0, 1)", "exponential(1)", ""),
#'   class = c("b", "sigma_trend", "ar1_trend"),
#'   coef = c("x1", "", ""),
#'   stringsAsFactors = FALSE
#' )
#' class(prior_frame) <- c("brmsprior", "prior_frame", "data.frame")
#' 
#' # Extract specific coefficient prior
#' extract_prior_string(prior_frame, "b", "x1")
#' 
#' # Extract class-level default
#' extract_prior_string(prior_frame, "sigma_trend")
#' 
#' @noRd
extract_prior_string <- function(prior_frame, class_name, coef_name = NULL, 
                                 handle_suffix = TRUE) {
  # Input validation
  checkmate::assert_class(prior_frame, "brmsprior")
  checkmate::assert_string(class_name, min.chars = 1)
  checkmate::assert_string(coef_name, null.ok = TRUE)
  checkmate::assert_logical(handle_suffix, len = 1)
  
  # Validate required columns exist
  required_cols <- c("prior", "class", "coef")
  missing_cols <- setdiff(required_cols, names(prior_frame))
  if (length(missing_cols) > 0) {
    stop(insight::format_error(
      "brmsprior object missing required columns: {.field {missing_cols}}"
    ))
  }
  
  # Strategy 1: Exact class and coef match
  if (!is.null(coef_name)) {
    exact_match <- subset(prior_frame, 
                         class == class_name & coef == coef_name)
    if (nrow(exact_match) > 0) {
      return(get_best_prior_match(exact_match))
    }
  }
  
  # Strategy 2: Class match with empty coef (class-level default)
  class_default <- subset(prior_frame, 
                         class == class_name & (coef == "" | is.na(coef)))
  if (nrow(class_default) > 0) {
    return(get_best_prior_match(class_default))
  }
  
  # Strategy 3: Handle suffix matching if enabled
  if (handle_suffix && !is.null(coef_name)) {
    # Try matching with _trend suffix added
    if (!grepl("_trend$", coef_name)) {
      trend_coef <- paste0(coef_name, "_trend")
      trend_match <- subset(prior_frame,
                           class == class_name & coef == trend_coef)
      if (nrow(trend_match) > 0) {
        return(get_best_prior_match(trend_match))
      }
    }
    
    # Try matching with _trend suffix removed
    if (grepl("_trend$", coef_name)) {
      base_coef <- gsub("_trend$", "", coef_name)
      base_match <- subset(prior_frame,
                          class == class_name & coef == base_coef)
      if (nrow(base_match) > 0) {
        return(get_best_prior_match(base_match))
      }
    }
  }
  
  # Strategy 4: Pattern matching for complex coefficient names
  if (!is.null(coef_name)) {
    # Create safe regex pattern from coef_name
    safe_pattern <- gsub("([.()^${}+*?|\\\\\\[\\]])", "\\\\\\1", coef_name)
    pattern_match <- subset(prior_frame,
                           class == class_name & grepl(safe_pattern, coef))
    if (nrow(pattern_match) > 0) {
      return(get_best_prior_match(pattern_match))
    }
  }
  
  # No match found
  return(NULL)
}

#' Get Best Prior Match from Multiple Candidates
#'
#' When multiple rows match the search criteria, prioritize user-specified
#' priors over defaults and non-empty priors over empty ones.
#'
#' @param matches Data frame subset of brmsprior with matching rows
#' @return Character string of best prior, or NULL if no valid prior found
#' @noRd
get_best_prior_match <- function(matches) {
  if (nrow(matches) == 0) {
    return(NULL)
  }
  
  # Priority 1: User-specified priors (source != "default")
  if ("source" %in% names(matches)) {
    user_priors <- subset(matches, source != "default")
    if (nrow(user_priors) > 0) {
      matches <- user_priors
    }
  }
  
  # Priority 2: Non-empty prior strings
  non_empty <- subset(matches, 
                     !is.na(prior) & nchar(trimws(prior)) > 0)
  if (nrow(non_empty) > 0) {
    matches <- non_empty
  }
  
  # Return first (best) match
  prior_string <- matches$prior[1]
  
  # Handle empty/missing priors
  if (is.na(prior_string) || nchar(trimws(prior_string)) == 0) {
    return(NULL)
  }
  
  return(trimws(prior_string))
}

#' Map Trend Priors from brmsprior Object
#'
#' @description
#' Extracts relevant priors from a brmsprior object for a specific trend type,
#' handling ar{lag}_trend patterns and other trend-specific parameters. Uses
#' the trend registry system for extensibility and the extract_prior_string()
#' helper for parameter matching.
#'
#' @param prior A brmsprior object containing prior specifications
#' @param trend_type Character string specifying the trend type (e.g., "AR", "RW")
#' @return Named list of Stan distribution strings ready for stanvar generators.
#'   Parameter names match those expected by the trend's Stan generator function.
#'   Returns empty list if trend_type is not registered or no matching priors found.
#'
#' @details
#' This function leverages the existing trend registry system for extensibility:
#' 1. Gets prior specification from \code{get_trend_prior_spec(trend_type)}
#' 2. For each parameter in the specification, uses \code{extract_prior_string()}
#'    to find matching prior in the brmsprior object
#' 3. Handles special patterns like ar{lag}_trend for AR models with multiple lags
#' 4. Returns Stan distribution strings ready for immediate use in stanvar generators
#' 
#' The function is designed to be extensible - no hardcoded trend types. New trends
#' automatically work if properly registered with \code{register_trend_type()}.
#'
#' @examples
#' \dontrun{
#' # Create priors using brms functions
#' library(brms)
#' my_priors <- c(
#'   prior("exponential(1)", class = "sigma_trend"),
#'   prior("normal(0, 0.3)", class = "ar1_trend"),
#'   prior("normal(0, 1)", class = "b")  # observation model prior
#' )
#' 
#' # Map priors for AR trend
#' ar_priors <- map_trend_priors(my_priors, "AR")
#' # Returns: list(sigma_trend = "exponential(1)", ar1_trend = "normal(0, 0.3)")
#' 
#' # Alternative using set_prior()
#' trend_priors <- c(
#'   set_prior("cauchy(0, 5)", class = "sigma_trend"),
#'   set_prior("normal(0, 0.5)", class = "ar1_trend")
#' )
#' rw_priors <- map_trend_priors(trend_priors, "RW")
#' # Returns: list(sigma_trend = "cauchy(0, 5)")
#' 
#' # For AR models with multiple lags
#' ar_seasonal_priors <- c(
#'   prior("normal(0, 0.3)", class = "ar1_trend"),   # lag 1
#'   prior("normal(0, 0.2)", class = "ar12_trend"),  # lag 12 (seasonal)
#'   prior("exponential(2)", class = "sigma_trend")
#' )
#' seasonal_mapped <- map_trend_priors(ar_seasonal_priors, "AR")
#' # Returns: list(ar1_trend = "normal(0, 0.3)", 
#' #               ar12_trend = "normal(0, 0.2)",
#' #               sigma_trend = "exponential(2)")
#' }
#'
#' @seealso \code{\link{get_trend_prior_spec}}, \code{\link{extract_prior_string}},
#'   \code{\link{register_trend_type}}, \code{\link[brms]{prior}}, \code{\link[brms]{set_prior}}
#' @noRd
map_trend_priors <- function(prior, trend_type) {
  # Input validation
  checkmate::assert_class(prior, "brmsprior")
  checkmate::assert_character(trend_type, len = 1, min.chars = 1, 
                             any.missing = FALSE)
  
  # Get prior specification for this trend type from registry
  trend_prior_spec <- get_trend_prior_spec(trend_type)
  
  # Return empty list if trend type not registered
  if (is.null(trend_prior_spec)) {
    if (!identical(Sys.getenv("TESTTHAT"), "true")) {
      rlang::warn(
        insight::format_warning(
          "Trend type {.field {trend_type}} not found in registry. ",
          "No priors will be mapped."
        ),
        .frequency = "once",
        .frequency_id = "mvgam_trend_registry"
      )
    }
    return(list())
  }
  
  # Initialize result list
  mapped_priors <- list()
  
  # Extract priors for each parameter in the trend specification
  for (param_name in names(trend_prior_spec)) {
    
    # Handle special AR lag patterns (ar1_trend, ar2_trend, ar12_trend, etc.)
    if (grepl("^ar\\d+_trend$", param_name)) {
      # Extract specific AR lag parameter
      prior_string <- extract_prior_string(prior, param_name, 
                                          handle_suffix = TRUE)
    } else {
      # Standard parameter extraction
      prior_string <- extract_prior_string(prior, param_name, 
                                          handle_suffix = TRUE)
    }
    
    # Add to result if prior was found
    if (!is.null(prior_string)) {
      # Convert to Stan string and store
      temp_prior_row <- data.frame(prior = prior_string, 
                                   stringsAsFactors = FALSE)
      mapped_priors[[param_name]] <- map_prior_to_stan_string(temp_prior_row)
    }
  }
  
  return(mapped_priors)
}

#' Get Trend Parameter Prior with Fallback to Common Default
#'
#' @description
#' Centralized helper for any trend generator to access user-defined priors
#' with automatic fallback to common defaults. This function provides the
#' foundation for simple, DRY prior resolution across all trend types.
#'
#' @param prior A brmsprior object containing custom prior specifications, or NULL
#' @param param_name Character string parameter name (e.g., "sigma_trend", "ar1_trend")
#' @return Character string containing Stan prior distribution (e.g., "exponential(2)")
#'   or empty string if no prior is specified (Stan will use its defaults)
#'
#' @details
#' Resolution strategy:
#' 1. **User specification first**: Checks the provided brmsprior object for the parameter
#' 2. **Common default fallback**: Uses `common_trend_priors` if parameter is available
#' 3. **Empty string fallback**: Returns "" if no specification found (Stan defaults)
#'
#' This design ensures maximum extensibility - any new trend type can call this
#' function for any parameter and get consistent behavior. New parameters can
#' be added to `common_trend_priors` and automatically work across all trends.
#'
#' @examples
#' \dontrun{
#' # User specified custom sigma_trend prior
#' my_priors <- c(prior("exponential(1)", class = "sigma_trend"))
#' get_trend_parameter_prior(my_priors, "sigma_trend")  # "exponential(1)"
#'
#' # No user specification, use common default
#' get_trend_parameter_prior(NULL, "sigma_trend")  # "exponential(2)"
#'
#' # Parameter not in common_trend_priors
#' get_trend_parameter_prior(NULL, "custom_param")  # ""
#'
#' # Usage in trend generators
#' sigma_prior <- get_trend_parameter_prior(prior, "sigma_trend")
#' stan_code <- glue("sigma_trend ~ {sigma_prior};")
#' }
#'
#' @seealso \code{\link{extract_prior_string}}, \code{common_trend_priors}
#' @noRd
get_trend_parameter_prior <- function(prior = NULL, param_name) {
  # Input validation
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE, .var.name = "prior")
  checkmate::assert_character(param_name, len = 1, min.chars = 1, 
                             any.missing = FALSE, .var.name = "param_name")
  
  # Strategy 1: Try user specification first
  if (!is.null(prior)) {
    user_prior <- extract_prior_string(prior, param_name, handle_suffix = TRUE)
    if (!is.null(user_prior)) {
      # Defensive check for helper function return
      if (!is.character(user_prior)) {
        stop(insight::format_error(
          "extract_prior_string returned non-character value for parameter {.field {param_name}}"
        ))
      }
      
      # Convert to clean Stan string
      temp_prior_row <- data.frame(prior = user_prior, stringsAsFactors = FALSE)
      stan_string <- map_prior_to_stan_string(temp_prior_row)
      
      # Validate the result
      if (!is.character(stan_string) || length(stan_string) != 1) {
        stop(insight::format_error(
          "map_prior_to_stan_string returned invalid result for parameter {.field {param_name}}"
        ))
      }
      
      return(stan_string)
    }
  }
  
  # Strategy 2: Fallback to common default if available
  if (!is.list(common_trend_priors)) {
    stop(insight::format_error(
      "common_trend_priors must be a list structure"
    ))
  }
  
  if (param_name %in% names(common_trend_priors)) {
    default_spec <- common_trend_priors[[param_name]]
    if (!is.list(default_spec) || !"default" %in% names(default_spec)) {
      stop(insight::format_error(
        "Invalid structure for common_trend_priors parameter {.field {param_name}}"
      ))
    }
    return(default_spec$default)
  }
  
  # Strategy 3: Empty string fallback (Stan will use its defaults)
  return("")
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

# =============================================================================
# SECTION 6: MVGAM FORMULA INTERFACE
# =============================================================================
# WHY: Provides a consistent interface for model specification that extends
# brms functionality with state-space trend components. The mvgam_formula
# constructor and associated S3 methods enable clean integration with brms
# inspection functions without masking or conflicts.

#' Create an mvgam Formula Object
#' 
#' @description
#' Constructs a lightweight mvgam formula object that combines observation and 
#' trend model formulas. This provides a clean interface for model specification
#' that can be used with inspection functions like \code{get_prior()}, 
#' \code{make_stancode()}, and \code{make_standata()}.
#' 
#' @param formula An object of class \code{formula}, \code{brmsformula}, or
#'   \code{mvbrmsformula} describing the observation model.
#' @param trend_formula An optional formula describing trend dynamics. Default
#'   NULL results in pure brms equivalent model. See Details for trend syntax.
#' 
#' @details
#' The mvgam_formula object is a minimal container that pairs an observation
#' formula with an optional trend formula. Data, family, and other model
#' specifications are provided when calling inspection or fitting functions:
#' 
#' \code{get_prior(mvgam_formula(y ~ x, ~ AR()), data = dat, family = poisson())}
#' 
#' When \code{trend_formula = NULL}, the model reduces to a pure brms 
#' observation model. When specified, the trend_formula can include:
#' \itemize{
#'   \item Trend constructors: \code{RW()}, \code{AR()}, \code{CAR()}, 
#'     \code{ZMVN()}, \code{VAR()}, \code{PW()}
#'   \item Covariates that affect trend dynamics
#'   \item Smooth terms using mgcv syntax: \code{s()}, \code{te()}, \code{ti()}, \code{t2()}
#'   \item Random effects: \code{(1|group)}, \code{(slope|group)}
#'   \item Gaussian processes: \code{gp()}
#' }
#' 
#' @section Trend Formula Restrictions:
#' The trend_formula has specific restrictions to maintain compatibility with
#' State-Space dynamics and prevent conflicts between observation-level and
#' trend-level modeling:
#' 
#' \strong{Forbidden Terms:}
#' \itemize{
#'   \item brms addition-terms: \code{weights()}, \code{cens()}, \code{trunc()},
#'     \code{mi()}, \code{trials()}, \code{rate()}, \code{vreal()}, \code{vint()},
#'     \code{subset()}, \code{index()}
#'   \item brms autocorrelation: \code{ar()}, \code{ma()}, \code{arma()},
#'     \code{cosy()}, \code{unstr()}, \code{autocor()}
#'   \item Offset terms: \code{offset()}
#' }
#' 
#' \strong{Rationale:}
#' Addition-terms modify observation model behavior (weights, censoring, exposure,
#' etc.) and should be specified in the main observation formula. Autocorrelation
#' terms conflict with mvgam's State-Space dynamics. All forbidden terms remain
#' fully supported in the main observation formula.
#' 
#' \strong{Examples:}
#' \preformatted{
#' # Correct usage
#' mvgam_formula(y ~ x + weights(w), trend_formula = ~ AR(p = 1))
#' mvgam_formula(count ~ offset(log_exposure), trend_formula = ~ RW())
#' 
#' # Incorrect usage (will error)
#' mvgam_formula(y ~ x, trend_formula = ~ AR() + weights(w))
#' mvgam_formula(y ~ x, trend_formula = ~ RW() + offset(z))
#' }
#' 
#' @return An object of class \code{c("mvgam_formula", base_class)} where
#'   base_class is the class of the input formula, containing:
#' \itemize{
#'   \item \code{formula}: The observation model formula
#'   \item \code{trend_formula}: The trend model formula (or NULL)
#' }
#' 
#' @examples
#' \dontrun{
#' # Create formula specifications
#' mf1 <- mvgam_formula(y ~ x + (1|group))
#' mf2 <- mvgam_formula(count ~ treatment, trend_formula = ~ AR(p = 1))
#' mf3 <- mvgam_formula(mvbind(y1, y2) ~ x, trend_formula = ~ VAR(lags = 1))
#' 
#' # Use with inspection functions (data provided to the function)
#' priors <- get_prior(mf2, data = ecology_data, family = poisson())
#' stancode <- make_stancode(mf2, data = ecology_data, family = poisson())
#' }
#' 
#' @seealso 
#' \code{\link{get_prior.mvgam_formula}}, \code{\link{make_stancode}}, 
#' \code{\link{make_standata}}, \code{\link{mvgam}}
#' 
#' @export
mvgam_formula <- function(formula, trend_formula = NULL) {
  
  # Validate formula parameter - must be formula, brmsformula, or mvbrmsformula
  checkmate::assert(
    checkmate::check_formula(formula),
    checkmate::check_class(formula, "brmsformula"),
    checkmate::check_class(formula, "mvbrmsformula"),
    .var.name = "formula"
  )
  
  # Validate trend_formula if provided
  if (!is.null(trend_formula)) {
    checkmate::assert_formula(trend_formula, .var.name = "trend_formula")
    
    # Use comprehensive trend formula validation from validations.R
    validate_single_trend_formula(trend_formula, context = "trend_formula")
  }
  
  # Determine and store formula type for later use
  if (inherits(formula, "mvbrmsformula")) {
    formula_class <- "mvbrmsformula"
  } else if (inherits(formula, "brmsformula")) {
    formula_class <- "brmsformula"
  } else {
    formula_class <- "formula"
  }
  
  # Create structure with formulas and class metadata
  out <- list(
    formula = formula,
    trend_formula = trend_formula
  )
  
  # Set S3 class hierarchy preserving original formula class
  if (formula_class == "mvbrmsformula") {
    class(out) <- c("mvgam_formula", "mvbrmsformula")
  } else if (formula_class == "brmsformula") {
    class(out) <- c("mvgam_formula", "brmsformula")
  } else {
    class(out) <- c("mvgam_formula", "formula")
  }
  attr(out, "formula_class") <- formula_class
  
  return(out)
}

#' Print method for mvgam_formula objects
#' 
#' @param x An mvgam_formula object
#' @param ... Ignored
#' @return The object invisibly
#' @export
print.mvgam_formula <- function(x, ...) {
  cat("mvgam_formula object\n")
  cat("Observation formula: ")
  print(x$formula)
  if (!is.null(x$trend_formula)) {
    cat("Trend formula: ")
    print(x$trend_formula)
  } else {
    cat("Trend formula: NULL (no trend component)\n")
  }
  invisible(x)
}

#' Get Prior Specifications for Model Objects
#'
#' @description
#' S3 generic function for extracting prior specifications from various model
#' objects. This function provides a unified interface for prior inspection
#' across different model types, with full brms compatibility and extensions
#' for mvgam State-Space models.
#'
#' @param object Model specification object (formula, brmsformula, mvgam_formula, etc.)
#' @param ... Additional arguments passed to methods
#'
#' @details
#' This generic function dispatches to appropriate methods based on object class:
#' \itemize{
#'   \item \code{formula}: Delegates to \code{brms::get_prior}
#'   \item \code{brmsformula}: Delegates to \code{brms::get_prior}  
#'   \item \code{mvgam_formula}: Uses mvgam's extended prior system
#'   \item Default: Falls back to \code{brms::get_prior}
#' }
#'
#' @return A \code{brmsprior} data frame with prior specifications
#'
#' @examples
#' \dontrun{
#' # Works with regular formulas (delegates to brms)
#' get_prior(y ~ x + (1|group), data = dat)
#' 
#' # Works with mvgam_formula objects  
#' mf <- mvgam_formula(y ~ x, trend_formula = ~ AR(p = 1))
#' get_prior(mf, data = dat)
#' }
#'
#' @seealso \code{\link{mvgam_formula}}, \code{\link[brms]{get_prior}}
#' @export
get_prior <- function(object, ...) {
  UseMethod("get_prior")
}

#' Default method for get_prior - delegates to brms
#'
#' @param object Model specification object
#' @param ... Additional arguments passed to \code{brms::get_prior}
#' @return A \code{brmsprior} data frame
#' @export
get_prior.default <- function(object, ...) {
  brms::get_prior(object, ...)
}

#' Formula method for get_prior - explicit brms delegation
#'
#' @param object A formula object
#' @param ... Additional arguments passed to \code{brms::get_prior}
#' @return A \code{brmsprior} data frame
#' @export  
get_prior.formula <- function(object, ...) {
  brms::get_prior(object, ...)
}

#' brmsformula method for get_prior - explicit brms delegation
#'
#' @param object A brmsformula object
#' @param ... Additional arguments passed to \code{brms::get_prior}
#' @return A \code{brmsprior} data frame
#' @export
get_prior.brmsformula <- function(object, ...) {
  brms::get_prior(object, ...)
}

#' Detect Embedded Families in Formula Objects
#' 
#' Checks if a formula object contains embedded family specifications
#' (e.g., bf(y1 ~ x, family = poisson()) + bf(y2 ~ x, family = gaussian()))
#' 
#' @param formula A formula, brmsformula, or mvbrmsformula object
#' @return Logical indicating whether embedded families are present
#' @noRd
has_embedded_families <- function(formula) {
  checkmate::assert_multi_class(formula, c("formula", "brmsformula", "mvbrmsformula"))
  
  if (inherits(formula, "mvbrmsformula") && !is.null(formula$forms)) {
    # Multivariate case: check if any bf() component has embedded family
    return(any(sapply(formula$forms, function(x) !is.null(x$family))))
  } else if (inherits(formula, "brmsformula")) {
    # Single brmsformula with potential embedded family
    return(!is.null(formula$family))
  }
  
  # Regular formula objects cannot have embedded families
  return(FALSE)
}

#' Extract Prior Specifications for mvgam Formula Objects
#'
#' @description
#' Extracts and combines prior specifications for both observation and trend 
#' components of an mvgam model. This method provides a unified interface
#' for prior inspection before model fitting with full brms compatibility.
#' When \code{trend_formula = NULL}, this function behaves identically to 
#' \code{brms::get_prior}, ensuring perfect brms compatibility for observation-only
#' models.
#'
#' @param object An object of class \code{mvgam_formula} created by 
#'   \code{\link{mvgam_formula}}
#' @param data A data frame containing the variables in the model (required)
#' @param family A description of the response distribution and link function.
#'   Default is \code{gaussian()}. Not required if formula contains embedded
#'   families via \code{bf()} specifications.
#' @param ... Additional arguments passed to \code{brms::get_prior}
#'
#' @return A \code{brmsprior} data frame combining observation and trend priors
#'   with an additional \code{trend_component} column distinguishing:
#'   \itemize{
#'     \item \code{"observation"}: Parameters from the observation model
#'     \item \code{"trend"}: Parameters from the trend model (with _trend suffix)
#'   }
#'   The returned object is fully compatible with \code{brms::set_prior()} and
#'   \code{brms::prior()} functions for customizing priors.
#'
#' @details
#' When a trend formula is specified, the function:
#' \enumerate{
#'   \item Extracts observation model priors using \code{brms::get_prior}
#'   \item Extracts trend model priors using mvgam's trend system
#'   \item Combines them into a unified \code{brmsprior} object
#'   \item Adds the \code{trend_component} column for easy filtering
#' }
#' 
#' The function handles embedded families automatically when using \code{bf()}
#' specifications and supports all brms family types for observation models
#' while trend components are always modeled as Gaussian State-Space processes.
#'
#' @examples
#' \dontrun{
#' # Create mvgam_formula objects
#' mf1 <- mvgam_formula(y ~ x + (1|group))  # No trend
#' mf2 <- mvgam_formula(count ~ treatment, ~ AR(p = 1))  # With trend
#' 
#' # Get priors (identical to brms when no trend)
#' priors1 <- get_prior(mf1, data = dat, family = poisson())
#' 
#' # Get combined priors with trend parameters
#' priors2 <- get_prior(mf2, data = ecology_data, family = poisson())
#' 
#' # Filter by component
#' obs_priors <- priors2[priors2$trend_component == "observation", ]
#' trend_priors <- priors2[priors2$trend_component == "trend", ]
#' 
#' # Use with brms functions for customization
#' custom_priors <- brms::set_prior("normal(0, 0.5)", class = "ar1_trend", 
#'                                 prior = priors2)
#' }
#'
#' @seealso \code{\link{mvgam_formula}}, \code{\link[brms]{get_prior}}, 
#'   \code{\link[brms]{set_prior}}, \code{\link[brms]{prior}}
#' @export
get_prior.mvgam_formula <- function(object, data, family = gaussian(), ...) {
  
  # Input validation (required by CLAUDE.md standards)
  checkmate::assert_class(object, "mvgam_formula")
  checkmate::assert_data_frame(data, min.rows = 1)
  
  # Extract formula components from mvgam_formula object
  formula <- object[[1]]  # object$formula triggers S3 dispatch issues
  trend_formula <- object[[2]]  # object$trend_formula
  
  # Validate family parameter conditionally based on formula type
  if (!has_embedded_families(formula)) {
    checkmate::assert_class(family, "family")
  }
  
  # Validate formula structure before proceeding
  if (length(formula) < 3) {
    stop(insight::format_error(
      "Formula missing response variable.",
      "Ensure formula has form: y ~ predictors"
    ))
  }
  
  # Extract observation priors with embedded family support
  if (has_embedded_families(formula)) {
    # Let brms handle embedded families - don't pass family parameter
    obs_priors <- brms::get_prior(formula = formula, data = data, ...)
  } else {
    # Pass family parameter for non-embedded cases  
    obs_priors <- brms::get_prior(formula = formula, data = data, family = family, ...)
  }
  
  # Handle case where no trend formula is specified
  if (is.null(trend_formula)) {
    # Add attribute-based metadata for consistency
    component_vector <- rep("observation", nrow(obs_priors))
    attr(obs_priors, "trend_components") <- component_vector
    attr(obs_priors, "mvgam_enhanced") <- TRUE
    return(obs_priors)
  }
  
  # Extract response variable names for trend prior generation
  response_names <- extract_response_names(formula)
  
  # Extract trend model priors using existing helper function
  trend_priors <- extract_trend_priors(
    trend_formula = trend_formula,
    data = data,
    response_names = response_names
  )
  
  # Combine observation and trend priors using existing helper function
  combined_priors <- combine_obs_trend_priors(obs_priors, trend_priors)
  
  return(combined_priors)
}

# =============================================================================
# SECTION 7: ENHANCED BRMSPRIOR METHODS FOR MVGAM
# =============================================================================
# WHY: Custom methods provide enhanced functionality while maintaining full
# brms compatibility. The print method shows trend component information when
# mvgam attributes are present, and the c.brmsprior override preserves
# attributes during prior combination operations.

#' Enhanced Print Method for brmsprior Objects
#' 
#' @description
#' Enhanced print method for brmsprior objects that displays trend component
#' information when mvgam attributes are present, while maintaining identical
#' behavior for standard brms objects.
#' 
#' @param x A brmsprior object
#' @param ... Additional arguments (ignored)
#' @return The object invisibly
#' @export
print.brmsprior <- function(x, ...) {
  # Check if this is an mvgam-enhanced brmsprior object
  if (isTRUE(attr(x, "mvgam_enhanced"))) {
    # Enhanced printing for mvgam objects
    components <- attr(x, "trend_components")
    
    if (!is.null(components)) {
      # Count components
      n_obs <- sum(components == "observation")
      n_trend <- sum(components == "trend")
      
      cat("Priors for mvgam model\n")
      
      if (n_obs > 0) {
        cat("Observation model priors (", n_obs, " parameter", 
            if(n_obs > 1) "s" else "", "):\n", sep = "")
        obs_priors <- x[components == "observation", ]
        print(as.data.frame(obs_priors), row.names = FALSE)
        cat("\n")
      }
      
      if (n_trend > 0) {
        cat("Trend model priors (", n_trend, " parameter", 
            if(n_trend > 1) "s" else "", "):\n", sep = "")
        trend_priors <- x[components == "trend", ]
        print(as.data.frame(trend_priors), row.names = FALSE)
        cat("\n")
      }
      
      cat("Use brms::set_prior() or mvgam::set_prior() to modify priors.\n")
      
    } else {
      # Fallback to standard printing if attributes are missing
      NextMethod("print")
    }
  } else {
    # Standard brms printing for non-enhanced objects
    NextMethod("print")
  }
  
  invisible(x)
}

#' Enhanced c.brmsprior Method with Attribute Preservation
#' 
#' @description
#' Override of brms c.brmsprior method that preserves mvgam attributes during
#' prior combination operations. Falls back to standard brms behavior when
#' no mvgam attributes are present.
#' 
#' @param ... brmsprior objects to combine
#' @return Combined brmsprior object with preserved attributes
#' @export
c.brmsprior <- function(...) {
  dots <- list(...)
  
  # Check if any objects have mvgam enhancement
  has_mvgam_attrs <- any(sapply(dots, function(x) isTRUE(attr(x, "mvgam_enhanced"))))
  
  if (has_mvgam_attrs) {
    # Enhanced combination with attribute preservation
    
    # Extract attributes from all enhanced objects
    all_components <- list()
    for (i in seq_along(dots)) {
      obj <- dots[[i]]
      if (isTRUE(attr(obj, "mvgam_enhanced"))) {
        components <- attr(obj, "trend_components")
        if (!is.null(components)) {
          all_components[[i]] <- components
        } else {
          # Auto-detect for objects missing components
          all_components[[i]] <- ifelse(grepl("_trend$", obj$class), "trend", "observation")
        }
      } else {
        # Standard brms object - assume observation components
        all_components[[i]] <- rep("observation", nrow(obj))
      }
    }
    
    # Standard brms combination
    dots_df <- lapply(dots, as.data.frame)
    combined <- do.call(rbind, dots_df)
    class(combined) <- c("brmsprior", "data.frame")
    
    # Preserve first object's class2 attribute if it exists
    first_class2 <- attr(dots[[1]], "class2")
    if (!is.null(first_class2)) {
      attr(combined, "class2") <- first_class2
    }
    
    # Combine all component vectors
    combined_components <- do.call(c, all_components)
    
    # Add mvgam attributes to result
    attr(combined, "trend_components") <- combined_components
    attr(combined, "mvgam_enhanced") <- TRUE
    
    return(combined)
    
  } else {
    # No mvgam attributes - use standard brms behavior
    dots_df <- lapply(dots, as.data.frame)
    combined <- do.call(rbind, dots_df)
    class(combined) <- c("brmsprior", "data.frame")
    
    # Preserve first object's class2 attribute if it exists
    first_class2 <- attr(dots[[1]], "class2")
    if (!is.null(first_class2)) {
      attr(combined, "class2") <- first_class2
    }
    
    return(combined)
  }
}

#' Enhanced +.brmsprior Method with Attribute Preservation
#' 
#' @description
#' Override of brms +.brmsprior method that preserves mvgam attributes during
#' prior combination operations using the + operator. Calls the enhanced
#' c.brmsprior method to maintain attribute preservation.
#' 
#' @param x First brmsprior object
#' @param y Second brmsprior object
#' @return Combined brmsprior object with preserved attributes
#' @export
`+.brmsprior` <- function(x, y) {
  c.brmsprior(x, y)
}

# =============================================================================
# SECTION: ENHANCED PRIOR SPECIFICATION FUNCTIONS
# =============================================================================

#' Convert brms lb/ub columns to mvgam bound format for consistency
#' 
#' @param prior_obj A brmsprior object potentially containing lb/ub columns
#' @return The same object with bound column instead of lb/ub columns
#' @noRd
standardize_brmsprior_columns <- function(prior_obj) {
  if ("lb" %in% names(prior_obj) && "ub" %in% names(prior_obj)) {
    # Validate columns exist and have compatible types
    checkmate::assert_numeric(prior_obj$lb, null.ok = TRUE, any.missing = TRUE)
    checkmate::assert_numeric(prior_obj$ub, null.ok = TRUE, any.missing = TRUE)
    
    # Convert lb/ub to bound column for consistency with safe string handling
    bound_strings <- vapply(seq_len(nrow(prior_obj)), function(i) {
      lb_val <- prior_obj$lb[i]
      ub_val <- prior_obj$ub[i] 
      
      # Handle infinite values safely
      lb_finite <- !is.na(lb_val) && is.finite(lb_val)
      ub_finite <- !is.na(ub_val) && is.finite(ub_val)
      
      if (lb_finite && ub_finite) {
        sprintf("<lower=%.10g,upper=%.10g>", lb_val, ub_val)
      } else if (lb_finite) {
        sprintf("<lower=%.10g>", lb_val)
      } else if (ub_finite) {
        sprintf("<upper=%.10g>", ub_val)
      } else {
        ""
      }
    }, character(1))
    
    prior_obj$bound <- bound_strings
    prior_obj$lb <- NULL
    prior_obj$ub <- NULL
  }
  
  return(prior_obj)
}

#' Set Prior for mvgam Models
#'
#' @description
#' Wrapper function around \code{brms::set_prior} that provides a unified
#' interface for setting priors on both observation and trend model parameters.
#' Automatically routes observation parameters to brms while handling trend 
#' parameters (\code{_trend} suffix) through the mvgam system. Maintains full 
#' brms syntax compatibility while extending support for trend-specific parameters.
#'
#' @param prior A character string defining the prior distribution in Stan syntax
#'   (e.g., \code{"normal(0, 1)"}, \code{"exponential(2)"}).
#' @param class A character string specifying the parameter class. For trend
#'   parameters, use the \code{_trend} suffix (e.g., \code{"ar1_trend"},
#'   \code{"sigma_trend"}). Defaults to \code{"b"} (regression coefficients).
#' @param coef Character string specifying a specific coefficient name.
#'   Defaults to empty string (all coefficients in class).
#' @param group Character string specifying a grouping factor.
#'   Defaults to empty string (population-level effect).
#' @param resp Character string specifying a response variable in
#'   multivariate models. Defaults to empty string.
#' @param dpar Character string specifying a distributional parameter.
#'   Defaults to empty string.
#' @param nlpar Character string specifying a non-linear parameter.
#'   Defaults to empty string.
#' @param lb Numeric value specifying lower bound constraint.
#'   Defaults to \code{NA} (no lower bound).
#' @param ub Numeric value specifying upper bound constraint.
#'   Defaults to \code{NA} (no upper bound).
#' @param check Logical indicating whether to validate prior specification.
#'   Defaults to \code{TRUE}.
#'
#' @return A \code{brmsprior} object enhanced with mvgam attributes. Fully 
#'   compatible with \code{brms::set_prior} return values and supports 
#'   combination with \code{+} operator.
#'
#' @details
#' This function provides several key advantages over using \code{brms::set_prior} 
#' directly:
#' 
#' \strong{Unified Interface:}
#' - Single function for both observation and trend parameters
#' - Automatic parameter type detection and routing
#' - Consistent mvgam attribute enhancement across all outputs
#' 
#' \strong{Observation Parameters:}
#' - Routed directly to \code{brms::set_prior} with identical behavior
#' - Full brms syntax compatibility maintained
#' - All standard brms parameter classes supported (\code{"b"}, \code{"Intercept"}, 
#'   \code{"sd"}, \code{"cor"}, etc.)
#' 
#' \strong{Trend Parameters:}
#' - Identified by \code{_trend} suffix in class name (e.g., \code{"ar1_trend"}, 
#'   \code{"sigma_trend"})
#' - Creates brmsprior objects using brms-compatible structure
#' - Enhanced with mvgam attributes for proper downstream handling
#' - Supports same bound constraints as brms parameters
#' 
#' \strong{Integration Benefits:}
#' - All outputs work seamlessly with existing brms workflow
#' - Enhanced objects provide better printing and summary information
#' - Automatic compatibility with \code{get_prior.mvgam_formula()} system
#' - Future-proof extension point for additional mvgam functionality
#'
#' @examples
#' \dontrun{
#' # Observation model priors (routed to brms::set_prior)
#' obs_prior <- set_prior("normal(0, 2)", class = "Intercept")
#' obs_coef <- set_prior("normal(0, 0.5)", class = "b", coef = "x1")
#' 
#' # Trend model priors (handled by mvgam system)
#' ar_prior <- set_prior("normal(0, 0.5)", class = "ar1_trend")
#' sigma_prior <- set_prior("exponential(2)", class = "sigma_trend")
#' 
#' # Bound constraints work for both types
#' bounded_prior <- set_prior("normal(0, 1)", class = "ar1_trend", 
#'                           lb = -0.99, ub = 0.99)
#' 
#' # Combine multiple priors with + operator
#' all_priors <- obs_prior + obs_coef + ar_prior + sigma_prior
#' 
#' # Use with mvgam()
#' model <- mvgam(y ~ x, trend_model = AR(), data = dat, prior = all_priors)
#' 
#' # Enhanced printing shows parameter organization
#' print(all_priors)
#' }
#'
#' @seealso \code{\link{get_prior.mvgam_formula}}, \code{\link[brms]{set_prior}}, 
#'   \code{\link{prior}}, \code{\link{mvgam_formula}}
#'   
#' @importFrom checkmate assert_string assert_number assert_logical assert_character assert_numeric
#' @importFrom insight format_error
#' @importFrom rlang warn
#' @export
set_prior <- function(prior, class = "b", coef = "", group = "", resp = "", 
                     dpar = "", nlpar = "", lb = NA, ub = NA, check = TRUE) {
  
  # Comprehensive input validation
  # Handle different input types for prior parameter
  if (!is.character(prior) && !inherits(prior, "brmsprior") && !is.list(prior)) {
    stop(insight::format_error(
      "Invalid {.field prior} input type: {.cls {class(prior)}}.",
      "Expected character string, brmsprior object, or list."
    ))
  }
  
  # Validate other parameters based on input type
  if (is.character(prior)) {
    checkmate::assert_string(prior)
    # Validate other parameters for character input
    checkmate::assert_character(class, null.ok = TRUE)
    checkmate::assert_character(coef, null.ok = TRUE) 
    checkmate::assert_character(group, null.ok = TRUE)
    checkmate::assert_character(resp, null.ok = TRUE)
    checkmate::assert_character(dpar, null.ok = TRUE)
    checkmate::assert_character(nlpar, null.ok = TRUE)
    checkmate::assert_numeric(lb, null.ok = TRUE)
    checkmate::assert_numeric(ub, null.ok = TRUE)
    
    # Validate bounds constraint
    if (!is.null(lb) && !is.null(ub)) {
      if (any(!is.na(lb) & !is.na(ub) & lb >= ub)) {
        stop(insight::format_error(
          "Invalid bounds constraint.",
          "Lower bound must be less than upper bound: lb < ub."
        ))
      }
    }
    
    # Validate vectorization consistency for character input
    param_lengths <- c(
      length(class %||% ""), length(coef %||% ""), length(group %||% ""),
      length(resp %||% ""), length(dpar %||% ""), length(nlpar %||% ""),
      if (!is.null(lb)) length(lb) else 1,
      if (!is.null(ub)) length(ub) else 1
    )
    # Remove length 1 (scalar) entries and check consistency
    non_scalar_lengths <- param_lengths[param_lengths > 1]
    if (length(unique(non_scalar_lengths)) > 1) {
      stop(insight::format_error(
        "Parameter vectors must have consistent lengths.",
        "Found lengths: {.val {unique(non_scalar_lengths)}}"
      ))
    }
  }
  
  checkmate::assert_logical(check, len = 1)
  
  # Helper function for NULL coalescing
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  # =========================================================================
  # MAIN PROCESSING LOGIC
  # =========================================================================
  
  if (inherits(prior, "brmsprior")) {
    # Handle brmsprior object input
    if (check) {
      # Basic validation of brmsprior object
      required_cols <- c("prior", "class", "coef", "group", "resp", "dpar", "nlpar", "bound", "source")
      if (!all(required_cols %in% names(prior))) {
        stop(insight::format_error(
          "Invalid brmsprior object structure.",
          "Missing required columns: {.field {setdiff(required_cols, names(prior))}}"
        ))
      }
    }
    
    # Check if any rows contain trend parameters and add attributes
    trend_components <- ifelse(grepl("_trend$", prior$class), "trend", "observation")
    attr(prior, "trend_components") <- trend_components
    attr(prior, "mvgam_enhanced") <- TRUE
    
    return(prior)
    
  } else if (is.list(prior)) {
    # Handle list input - batch operations
    if (length(prior) == 0) {
      stop(insight::format_error("Empty list provided for {.field prior}."))
    }
    
    # Handle vectorized inputs with pairwise assignment (following brms pattern)
    # Ensure proper length handling following R recycling rules
    prior_length <- length(prior)
    class_length <- length(class)
    
    # Determine maximum length for recycling
    max_length <- max(prior_length, class_length)
    
    # Warn if lengths are incompatible (non-multiple lengths)
    if (max_length %% min(prior_length, class_length) != 0) {
      if (!identical(Sys.getenv("TESTTHAT"), "true")) {
        rlang::warn(
          "Length mismatch in vectorized prior specification.",
          "Shorter arguments will be recycled.",
          .frequency = "once",
          .frequency_id = "mvgam_length_mismatch"
        )
      }
    }
    
    # Process pairwise assignments
    processed_priors <- vector("list", max_length)
    for (i in seq_len(max_length)) {
      # Use recycling for indices
      prior_idx <- ((i - 1) %% prior_length) + 1
      class_idx <- ((i - 1) %% class_length) + 1
      
      p <- prior[[prior_idx]]
      c <- class[class_idx]
      
      if (is.character(p)) {
        # Recursive call with single class value (pairwise)
        processed_priors[[i]] <- set_prior(p, class = c, coef = coef %||% "", 
                                           group = group %||% "", resp = resp %||% "", 
                                           dpar = dpar %||% "", nlpar = nlpar %||% "",
                                           lb = lb, ub = ub, check = check)
      } else if (inherits(p, "brmsprior")) {
        # Handle brmsprior elements
        processed_priors[[i]] <- set_prior(p, check = check)
      } else {
        stop(insight::format_error(
          "Invalid list element type: {.cls {class(p)}}.",
          "List elements must be character strings or brmsprior objects."
        ))
      }
    }
    
    # Combine all processed priors
    combined <- do.call(rbind, processed_priors)
    class(combined) <- c("brmsprior", "data.frame")
    
    # Determine trend components for combined result
    trend_components <- ifelse(grepl("_trend$", combined$class), "trend", "observation")
    attr(combined, "trend_components") <- trend_components
    attr(combined, "mvgam_enhanced") <- TRUE
    
    return(combined)
    
  } else {
    # Handle character string input - most common case
    
    # Determine parameter type(s) for routing
    classes <- class %||% "b"
    has_trend_params <- any(grepl("_trend$", classes))
    has_obs_params <- any(!grepl("_trend$", classes))
    
    if (has_trend_params && !has_obs_params) {
      # Pure trend parameters - handle directly
      
      # Convert bounds to brms-compatible string format  
      bound_string <- ""
      if (!is.na(lb) && !is.na(ub)) {
        bound_string <- sprintf("<lower=%s,upper=%s>", lb, ub)
      } else if (!is.na(lb)) {
        bound_string <- sprintf("<lower=%s>", lb)
      } else if (!is.na(ub)) {
        bound_string <- sprintf("<upper=%s>", ub)
      }
      
      # Basic prior validation if check=TRUE
      if (check) {
        if (!grepl("\\(.*\\)", prior)) {
          stop(insight::format_error(
            "Invalid prior specification: {.field {prior}}.",
            "Prior must be valid Stan syntax (e.g., 'normal(0, 1)')."
          ))
        }
      }
      
      # Create brmsprior object using exact brms structure
      trend_prior <- data.frame(
        prior = as.character(prior),
        class = as.character(classes),
        coef = as.character(coef %||% ""),
        group = as.character(group %||% ""),
        resp = as.character(resp %||% ""),
        dpar = as.character(dpar %||% ""),
        nlpar = as.character(nlpar %||% ""),
        bound = as.character(bound_string),
        source = "user",
        stringsAsFactors = FALSE
      )
      
      # Set proper class hierarchy
      class(trend_prior) <- c("brmsprior", "data.frame")
      
      # Add mvgam attributes
      attr(trend_prior, "trend_components") <- rep("trend", nrow(trend_prior))
      attr(trend_prior, "mvgam_enhanced") <- TRUE
      
      return(trend_prior)
      
    } else if (has_obs_params && !has_trend_params) {
      # Pure observation parameters - delegate to brms
      obs_prior <- brms::set_prior(
        prior = prior, 
        class = classes, 
        coef = coef, 
        group = group,
        resp = resp, 
        dpar = dpar, 
        nlpar = nlpar,
        lb = lb,
        ub = ub,
        check = check
      )
      
      # Ensure consistent column structure for combination operations
      obs_prior <- standardize_brmsprior_columns(obs_prior)
      
      # Add mvgam attributes for consistency
      attr(obs_prior, "trend_components") <- rep("observation", nrow(obs_prior))
      attr(obs_prior, "mvgam_enhanced") <- TRUE
      
      return(obs_prior)
      
    } else {
      # Mixed parameters or edge case - delegate to brms with warning
      if (has_trend_params && has_obs_params) {
        if (!identical(Sys.getenv("TESTTHAT"), "true")) {
          rlang::warn(
            "Mixed trend and observation parameters detected in single call.",
            "Consider using separate set_prior() calls for clarity.",
            .frequency = "once",
            .frequency_id = "mvgam_mixed_params"
          )
        }
      }
      
      # Default delegation to brms
      obs_prior <- brms::set_prior(
        prior = prior, 
        class = classes, 
        coef = coef, 
        group = group,
        resp = resp, 
        dpar = dpar, 
        nlpar = nlpar,
        lb = lb,
        ub = ub,
        check = check
      )
      
      # Ensure consistent column structure for combination operations
      obs_prior <- standardize_brmsprior_columns(obs_prior)
      
      # Add mvgam attributes
      trend_components <- ifelse(grepl("_trend$", obs_prior$class), "trend", "observation")
      attr(obs_prior, "trend_components") <- trend_components
      attr(obs_prior, "mvgam_enhanced") <- TRUE
      
      return(obs_prior)
    }
  }
}

