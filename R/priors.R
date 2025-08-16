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
  checkmate::assert_formula(formula)
  checkmate::assert_data_frame(data, min.rows = 1)
  
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
parse_single_trend_formula <- function(formula, data) {
  # Extract trend constructor from formula
  # This is simplified - actual implementation would be more sophisticated
  
  formula_str <- deparse(formula)
  
  # Default ZMVN for simple formulas
  if (formula_str %in% c("~1", "~-1", "~ 1", "~ -1")) {
    return(list(trend = "ZMVN", cor = TRUE))
  }
  
  # Check for trend constructors
  if (grepl("AR\\(", formula_str)) {
    # Extract AR parameters
    return(list(trend = "AR", p = 1, cor = FALSE))
  } else if (grepl("RW\\(", formula_str)) {
    return(list(trend = "RW"))
  } else if (grepl("VAR\\(", formula_str)) {
    return(list(trend = "VAR", p = 1))
  } else if (grepl("CAR\\(", formula_str)) {
    return(list(trend = "CAR", cor = TRUE))
  } else if (grepl("PW\\(", formula_str)) {
    return(list(trend = "PW", growth = "linear"))
  } else {
    # Default to ZMVN
    return(list(trend = "ZMVN", cor = TRUE))
  }
}

#' Generate Trend Priors Based on Trend Type
#' 
#' @param trend_spec Trend specification from parse_trend_formula
#' @param response_names Character vector of response names for
#'   multivariate models
#' @return A brmsprior object with trend priors
#' @noRd
generate_trend_priors <- function(trend_spec, response_names = NULL) {
  # Convention-based dispatch to trend-specific prior generators
  
  if (is.list(trend_spec) && !is.null(names(trend_spec))) {
    # Multivariate case - generate priors for each response
    all_priors <- list()
    
    for (resp in names(trend_spec)) {
      spec <- trend_spec[[resp]]
      generator_name <- paste0("get_", tolower(spec$trend), "_priors")
      
      if (exists(generator_name, mode = "function")) {
        generator <- get(generator_name, mode = "function")
        resp_priors <- generator(spec)
        # Add response suffix
        resp_priors$resp <- resp
        all_priors[[resp]] <- resp_priors
      }
    }
    
    # Combine all response-specific priors
    combined <- do.call(rbind, all_priors)
    class(combined) <- c("brmsprior", "data.frame")
    return(combined)
    
  } else {
    # Univariate case
    generator_name <- paste0("get_", tolower(trend_spec$trend), "_priors")
    
    if (exists(generator_name, mode = "function")) {
      generator <- get(generator_name, mode = "function")
      return(generator(trend_spec))
    } else {
      # Return empty prior if generator not found
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
  }
}

# =============================================================================
# SECTION 3: TREND-SPECIFIC PRIOR GENERATORS
# =============================================================================
# WHY: Each trend type has specific parameters that need priors. These
# generators follow the _trend suffix convention for clear separation from
# observation model parameters.

#' Generate AR Trend Priors
#' 
#' @param trend_spec AR trend specification
#' @return A brmsprior object with AR-specific priors
#' @noRd
get_ar_priors <- function(trend_spec) {
  priors <- data.frame(
    prior = c("", ""),
    class = c("ar1_trend", "sigma_trend"),
    coef = c("", ""),
    group = c("", ""),
    resp = c("", ""),
    dpar = c("", ""),
    nlpar = c("", ""),
    lb = c("", "0"),
    ub = c("", ""),
    source = c("default", "default"),
    stringsAsFactors = FALSE
  )
  
  # Add correlation priors if needed
  if (isTRUE(trend_spec$cor)) {
    cor_priors <- data.frame(
      prior = "",
      class = "L_Omega_trend",
      coef = "",
      group = "",
      resp = "",
      dpar = "",
      nlpar = "",
      lb = "",
      ub = "",
      source = "default",
      stringsAsFactors = FALSE
    )
    priors <- rbind(priors, cor_priors)
  }
  
  class(priors) <- c("brmsprior", "data.frame")
  return(priors)
}

#' Generate RW Trend Priors
#' 
#' @param trend_spec RW trend specification
#' @return A brmsprior object with RW-specific priors
#' @noRd
get_rw_priors <- function(trend_spec) {
  priors <- data.frame(
    prior = "",
    class = "sigma_trend",
    coef = "",
    group = "",
    resp = "",
    dpar = "",
    nlpar = "",
    lb = "0",
    ub = "",
    source = "default",
    stringsAsFactors = FALSE
  )
  
  class(priors) <- c("brmsprior", "data.frame")
  return(priors)
}

#' Generate VAR Trend Priors
#' 
#' @param trend_spec VAR trend specification
#' @return A brmsprior object with VAR-specific priors
#' @noRd
get_var_priors <- function(trend_spec) {
  p <- trend_spec$p %||% 1
  
  # Create priors for each lag
  priors <- data.frame(
    prior = "",
    class = paste0("A", p, "_trend"),
    coef = "",
    group = "",
    resp = "",
    dpar = "",
    nlpar = "",
    lb = "",
    ub = "",
    source = "default",
    stringsAsFactors = FALSE
  )
  
  # Add covariance priors
  cov_priors <- data.frame(
    prior = c("", ""),
    class = c("L_Omega_trend", "sigma_trend"),
    coef = c("", ""),
    group = c("", ""),
    resp = c("", ""),
    dpar = c("", ""),
    nlpar = c("", ""),
    lb = c("", "0"),
    ub = c("", ""),
    source = c("default", "default"),
    stringsAsFactors = FALSE
  )
  
  priors <- rbind(priors, cov_priors)
  class(priors) <- c("brmsprior", "data.frame")
  return(priors)
}

#' Generate CAR Trend Priors
#' 
#' @param trend_spec CAR trend specification
#' @return A brmsprior object with CAR-specific priors
#' @noRd
get_car_priors <- function(trend_spec) {
  priors <- data.frame(
    prior = c("", ""),
    class = c("sigma_trend", "theta1_trend"),
    coef = c("", ""),
    group = c("", ""),
    resp = c("", ""),
    dpar = c("", ""),
    nlpar = c("", ""),
    lb = c("0", "0"),
    ub = c("", "1"),
    source = c("default", "default"),
    stringsAsFactors = FALSE
  )
  
  # Add correlation priors if needed
  if (isTRUE(trend_spec$cor)) {
    cor_priors <- data.frame(
      prior = "",
      class = "L_Omega_trend",
      coef = "",
      group = "",
      resp = "",
      dpar = "",
      nlpar = "",
      lb = "",
      ub = "",
      source = "default",
      stringsAsFactors = FALSE
    )
    priors <- rbind(priors, cor_priors)
  }
  
  class(priors) <- c("brmsprior", "data.frame")
  return(priors)
}

#' Generate PW Trend Priors
#' 
#' @param trend_spec PW trend specification
#' @return A brmsprior object with PW-specific priors
#' @noRd
get_pw_priors <- function(trend_spec) {
  priors <- data.frame(
    prior = "",
    class = "sigma_trend",
    coef = "",
    group = "",
    resp = "",
    dpar = "",
    nlpar = "",
    lb = "0",
    ub = "",
    source = "default",
    stringsAsFactors = FALSE
  )
  
  # Add growth rate priors
  growth_priors <- data.frame(
    prior = "",
    class = "growth_trend",
    coef = "",
    group = "",
    resp = "",
    dpar = "",
    nlpar = "",
    lb = "",
    ub = "",
    source = "default",
    stringsAsFactors = FALSE
  )
  
  priors <- rbind(priors, growth_priors)
  
  # Add capacity prior for logistic growth
  if (trend_spec$growth == "logistic") {
    cap_priors <- data.frame(
      prior = "",
      class = "cap_trend",
      coef = "",
      group = "",
      resp = "",
      dpar = "",
      nlpar = "",
      lb = "0",
      ub = "",
      source = "default",
      stringsAsFactors = FALSE
    )
    priors <- rbind(priors, cap_priors)
  }
  
  class(priors) <- c("brmsprior", "data.frame")
  return(priors)
}

#' Generate ZMVN Trend Priors
#' 
#' @param trend_spec ZMVN trend specification
#' @return A brmsprior object with ZMVN-specific priors
#' @noRd
get_zmvn_priors <- function(trend_spec) {
  if (isTRUE(trend_spec$cor)) {
    # Correlated ZMVN
    priors <- data.frame(
      prior = c("", ""),
      class = c("L_Omega_trend", "sigma_trend"),
      coef = c("", ""),
      group = c("", ""),
      resp = c("", ""),
      dpar = c("", ""),
      nlpar = c("", ""),
      lb = c("", "0"),
      ub = c("", ""),
      source = c("default", "default"),
      stringsAsFactors = FALSE
    )
  } else {
    # Independent ZMVN
    priors <- data.frame(
      prior = "",
      class = "sigma_trend",
      coef = "",
      group = "",
      resp = "",
      dpar = "",
      nlpar = "",
      lb = "0",
      ub = "",
      source = "default",
      stringsAsFactors = FALSE
    )
  }
  
  class(priors) <- c("brmsprior", "data.frame")
  return(priors)
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