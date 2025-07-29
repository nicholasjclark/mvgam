#' Parse Multivariate Trend Specifications
#'
#' @description
#' Parses trend formulas for multivariate models, handling response-specific
#' trend specifications and creating appropriate mappings for brms integration.
#'
#' @param formula Main observation model formula (may be multivariate)
#' @param trend_formula Trend formula specification (may be response-specific)
#' @return List containing parsed trend specifications
#' @noRd
parse_multivariate_trends <- function(formula, trend_formula = NULL) {
  checkmate::assert_formula(formula)
  
  # Handle missing trend formula
  if (is.null(trend_formula)) {
    return(list(
      has_trends = FALSE,
      is_multivariate = FALSE,
      response_names = NULL,
      trend_specs = NULL,
      base_formula = NULL
    ))
  }
  
  # Validate trend formula structure
  trend_validation <- mvgam:::validate_bf_trend_formula(trend_formula)
  
  # Check if main formula is multivariate
  is_mv_main <- is_multivariate_formula(formula)
  
  # Parse response names from main formula
  response_names <- if (is_mv_main) {
    extract_response_names(formula)
  } else {
    NULL
  }
  
  # Handle response-specific trend formulas
  if (inherits(trend_formula, "brmsterms") || 
      inherits(trend_formula, "mvbrmsterms")) {
    
    # Extract response-specific trend specifications
    trend_specs <- extract_response_trends(trend_formula, response_names)
    
    # Create base formula for brms setup
    base_formula <- create_trend_base_formula(trend_specs)
    
  } else {
    # Single trend formula applied to all responses
    trend_specs <- if (is_mv_main && !is.null(response_names)) {
      # Apply same trend to all responses
      setNames(
        replicate(length(response_names), trend_formula, simplify = FALSE),
        response_names
      )
    } else {
      list(main = trend_formula)
    }
    
    base_formula <- trend_formula
  }
  
  return(list(
    has_trends = TRUE,
    is_multivariate = is_mv_main,
    response_names = response_names,
    trend_specs = trend_specs,
    base_formula = base_formula,
    validation = trend_validation
  ))
}

#' Check if Formula is Multivariate
#' @param formula Formula to check
#' @return Logical indicating if formula contains multivariate response
#' @noRd
is_multivariate_formula <- function(formula) {
  checkmate::assert_formula(formula)
  
  # Check for mvbind() or cbind() in response
  response_terms <- as.character(formula)[2]
  grepl("mvbind\\(|cbind\\(", response_terms, perl = TRUE)
}

#' Extract Response Names from Multivariate Formula
#' @param formula Multivariate formula
#' @return Character vector of response variable names
#' @noRd
extract_response_names <- function(formula) {
  checkmate::assert_formula(formula)
  
  response_side <- as.character(formula)[2]
  
  # Extract from mvbind() or cbind()
  if (grepl("mvbind\\(", response_side)) {
    # Parse mvbind(y1, y2, y3) structure
    inner <- gsub(".*mvbind\\((.*)\\).*", "\\1", response_side)
  } else if (grepl("cbind\\(", response_side)) {
    # Parse cbind(y1, y2) structure
    inner <- gsub(".*cbind\\((.*)\\).*", "\\1", response_side)
  } else {
    return(NULL)
  }
  
  # Split by commas and clean whitespace
  responses <- trimws(strsplit(inner, ",")[[1]])
  
  # Remove any function calls or transformations
  responses <- gsub("\\s*\\(.*\\)\\s*", "", responses)
  responses <- gsub("\\s+", "", responses)
  
  return(responses)
}

#' Extract Response-Specific Trend Specifications
#' @param trend_formula brms formula object with response-specific trends
#' @param response_names Character vector of response names
#' @return Named list of trend specifications per response
#' @noRd
extract_response_trends <- function(trend_formula, response_names) {
  checkmate::assert_character(response_names, null.ok = TRUE)
  
  # Check if trend_formula is already processed brms terms
  if (inherits(trend_formula, c("brmsterms", "mvbrmsterms"))) {
    trend_terms <- trend_formula
  } else {
    # Use brms internal functions to parse the formula structure
    trend_terms <- try(brms::brmsterms(trend_formula), silent = TRUE)
    
    if (inherits(trend_terms, "try-error")) {
      stop(insight::format_error(
        "Could not parse trend_formula structure.",
        "Ensure proper bf() syntax for response-specific trends."
      ))
    }
  }
  
  # Extract terms for each response
  trend_specs <- list()
  
  if (inherits(trend_terms, "mvbrmsterms")) {
    # Multivariate terms - extract each response
    for (i in seq_along(trend_terms$terms)) {
      resp_name <- names(trend_terms$terms)[i]
      if (is.null(resp_name) && i <= length(response_names)) {
        resp_name <- response_names[i]
      }
      
      if (!is.null(resp_name)) {
        trend_specs[[resp_name]] <- trend_terms$terms[[i]]$formula
      }
    }
  } else {
    # Single response trend - apply to main or first response
    resp_name <- if (!is.null(response_names)) response_names[1] else "main"
    trend_specs[[resp_name]] <- trend_terms$formula
  }
  
  return(trend_specs)
}

#' Create Base Formula for Trend Setup
#' @param trend_specs Named list of trend specifications
#' @return Formula object suitable for brms setup
#' @noRd
create_trend_base_formula <- function(trend_specs) {
  checkmate::assert_list(trend_specs, min.len = 1)
  
  # Use the first trend specification as base
  base_spec <- trend_specs[[1]]
  
  if (inherits(base_spec, "formula")) {
    return(base_spec)
  } else if (inherits(base_spec, "brmsterms")) {
    return(base_spec$formula)
  } else {
    # Fallback: create minimal trend formula
    return(~ 1)
  }
}