#' Mu Transformation System
#'
#' Linear pipeline for Stan code mu construction analysis and transformation.

#' Analyze Stan Code for Mu Transformation
#'
#' @description
#' Comprehensive analysis of Stan code to determine mu construction patterns
#' and required transformations for trend injection.
#'
#' @param stancode Character string containing complete Stan model code
#' @param response_names Character vector of response names for multivariate models.
#'   If NULL, assumes univariate model. Default NULL.
#'
#' @return List with transformation requirements:
#'   \describe{
#'     \item{model_type}{Character: "univariate" or "multivariate"}
#'     \item{response_names}{Character vector or NULL}
#'     \item{glm_info}{List with GLM patterns and parameters}
#'     \item{mu_info}{List with mu construction patterns and injection points}
#'     \item{strategy}{Character indicating transformation approach}
#'   }
#'
#' @details
#' Analyzes Stan code once to provide all information needed for linear
#' transformation pipeline. Handles both univariate and multivariate models.
#'
#' @examples
#' # Univariate model
#' analysis <- analyze_stan_code(stancode)
#'
#' # Multivariate model  
#' analysis <- analyze_stan_code(stancode, c("count", "biomass"))
#'
#' @noRd
analyze_stan_code <- function(stancode, response_names = NULL) {
  checkmate::assert_string(stancode, min.chars = 1)
  checkmate::assert_character(response_names, min.len = 1, null.ok = TRUE)
  
  # Determine model type
  is_multivariate <- !is.null(response_names) && length(response_names) > 1
  model_type <- if (is_multivariate) "multivariate" else "univariate"
  
  # GLM pattern detection
  glm_info <- detect_glm_patterns(stancode, response_names, is_multivariate)
  
  # Mu construction analysis  
  mu_info <- analyze_mu_construction(stancode, glm_info)
  
  # Determine transformation strategy
  strategy <- determine_strategy(glm_info, mu_info, is_multivariate)
  
  list(
    model_type = model_type,
    response_names = response_names,
    glm_info = glm_info,
    mu_info = mu_info,
    strategy = strategy
  )
}

#' Detect GLM Patterns in Stan Code
#'
#' @description
#' Detects GLM usage patterns and extracts parameters for conversion.
#' Handles both univariate and multivariate cases.
#'
#' @param stancode Character string of Stan code
#' @param response_names Character vector of response names (NULL for univariate)
#' @param is_multivariate Logical indicating model type
#' @return List with GLM detection results
#' @noRd
detect_glm_patterns <- function(stancode, response_names, is_multivariate) {
  if (is_multivariate) {
    # Multivariate: per-response detection
    response_usage <- detect_glm_usage(stancode, response_names)
    glm_responses <- names(response_usage)[response_usage]
    
    # Extract GLM info per response
    glm_by_response <- list()
    for (resp in glm_responses) {
      types <- find_glm_types_for_response(stancode, resp)
      params <- if (length(types) > 0) extract_glm_params_for_response(stancode, resp, types[1]) else NULL
      glm_by_response[[resp]] <- list(types = types, params = params)
    }
    
    list(
      has_glm = length(glm_responses) > 0,
      model_pattern = "multivariate",
      glm_responses = glm_responses,
      glm_by_response = glm_by_response
    )
  } else {
    # Univariate: global detection
    glm_types <- detect_glm_usage(stancode)
    glm_params <- if (length(glm_types) > 0) extract_glm_params(stancode, glm_types[1]) else NULL
    
    list(
      has_glm = length(glm_types) > 0,
      model_pattern = "univariate",
      glm_types = glm_types,
      glm_params = glm_params
    )
  }
}

#' Analyze Mu Construction Patterns
#'
#' @description
#' Classifies mu construction patterns and determines injection strategy.
#' 
#' @param stancode Character string of Stan code
#' @param glm_info List with GLM detection results
#' @return List with mu pattern analysis
#' @noRd
analyze_mu_construction <- function(stancode, glm_info) {
  # Extract mu lines from model block
  mu_lines <- extract_mu_lines(stancode)
  
  # Classify pattern type
  pattern_type <- classify_pattern(stancode, mu_lines, glm_info)
  
  # Find injection points
  injection_points <- find_injection_points(stancode, mu_lines, pattern_type)
  
  list(
    pattern_type = pattern_type,
    mu_lines = mu_lines,
    injection_points = injection_points,
    has_mu_construction = length(mu_lines) > 0
  )
}

#' Determine Transformation Strategy
#'
#' @description
#' Determines the transformation approach based on GLM and mu analysis.
#'
#' @param glm_info List with GLM detection results
#' @param mu_info List with mu pattern analysis
#' @param is_multivariate Logical indicating model type
#' @return Character string indicating strategy
#' @noRd
determine_strategy <- function(glm_info, mu_info, is_multivariate) {
  if (is_multivariate) {
    return("multivariate")
  }
  
  pattern <- mu_info$pattern_type
  has_glm <- glm_info$has_glm
  
  if (has_glm && pattern == "none") {
    "glm_convert"
  } else if (has_glm && pattern == "explicit") {
    "hybrid"
  } else if (pattern == "explicit") {
    "direct_inject"
  } else if (pattern == "nonlinear") {
    "loop_inject"
  } else {
    "minimal"
  }
}

#' Supporting Functions

#' Extract GLM Parameters from Stan Code
#' @noRd
extract_glm_params <- function(stancode, glm_type) {
  # Find GLM function call
  pattern <- paste0(glm_type, "_l(pdf|pmf)\\s*\\(([^)]+)\\)")
  matches <- regmatches(stancode, regexpr(pattern, stancode))
  
  if (length(matches) == 0) return(NULL)
  
  # Parse parameters: Y | Xc, mu, b
  call_content <- gsub(paste0("^", glm_type, "_l(pdf|pmf)\\s*\\(|\\)$"), "", matches[1])
  parts <- strsplit(call_content, "\\|")[[1]]
  
  if (length(parts) < 2) return(NULL)
  
  response_var <- trimws(parts[1])
  predictor_part <- trimws(parts[2])
  pred_params <- trimws(strsplit(predictor_part, ",")[[1]])
  
  list(
    response = response_var,
    design_matrix = if (length(pred_params) >= 1) pred_params[1] else NULL,
    intercept = if (length(pred_params) >= 2) pred_params[2] else NULL,
    coefficients = if (length(pred_params) >= 3) pred_params[3] else NULL,
    other_params = if (length(pred_params) > 3) pred_params[4:length(pred_params)] else NULL
  )
}

#' Extract Mu Lines from Model Block
#' @noRd
extract_mu_lines <- function(stancode) {
  lines <- strsplit(stancode, "\n")[[1]]
  
  # Find model block bounds
  model_start <- grep("^\\s*model\\s*\\{", lines)
  if (length(model_start) == 0) return(character(0))
  
  # Find end of model block
  brace_count <- 1
  model_end <- model_start[1]
  for (i in (model_start[1] + 1):length(lines)) {
    brace_count <- brace_count + 
      lengths(regmatches(lines[i], gregexpr("\\{", lines[i]))) - 
      lengths(regmatches(lines[i], gregexpr("\\}", lines[i])))
    if (brace_count == 0) {
      model_end <- i
      break
    }
  }
  
  model_lines <- lines[(model_start[1] + 1):(model_end - 1)]
  
  # Find mu assignment/increment lines
  mu_pattern <- "\\bmu\\s*(\\[.*?\\])?\\s*(\\+=|=)"
  mu_lines <- model_lines[grepl(mu_pattern, model_lines)]
  
  # Exclude likelihood function calls
  mu_lines[!grepl("_l(pdf|pmf)\\s*\\(", mu_lines)]
}

#' Classify Mu Construction Pattern
#' @noRd
classify_pattern <- function(stancode, mu_lines, glm_info) {
  has_glm <- glm_info$has_glm
  has_mu <- length(mu_lines) > 0
  has_indexed_mu <- any(grepl("\\bmu\\s*\\[", mu_lines))
  
  if (has_glm && !has_mu) {
    "glm_only"
  } else if (has_glm && has_mu) {
    "hybrid"
  } else if (has_mu && has_indexed_mu) {
    "nonlinear"
  } else if (has_mu) {
    "explicit"
  } else {
    "none"
  }
}

#' Find Trend Injection Points
#' @noRd
find_injection_points <- function(stancode, mu_lines, pattern_type) {
  if (pattern_type %in% c("explicit", "hybrid")) {
    # After last mu += line
    if (length(mu_lines) > 0) {
      return(list(type = "after_mu", target = mu_lines[length(mu_lines)]))
    }
  } else if (pattern_type == "nonlinear") {
    # Within loop structures
    loop_mu_lines <- mu_lines[grepl("\\bmu\\s*\\[", mu_lines)]
    if (length(loop_mu_lines) > 0) {
      return(list(type = "loop_wrap", target = loop_mu_lines))
    }
  }
  
  list(type = "fallback", target = character(0))
}

#' Find GLM Types for Specific Response
#' @noRd
find_glm_types_for_response <- function(stancode, response_name) {
  response_pattern <- paste0("Y_", response_name)
  glm_types <- c("normal_id_glm", "poisson_log_glm", "neg_binomial_2_log_glm", 
                "bernoulli_logit_glm", "ordered_logistic_glm", "categorical_logit_glm")
  
  found_types <- character(0)
  for (glm_type in glm_types) {
    pattern <- paste0(glm_type, "_l(pdf|pmf)\\([^)]*", response_pattern)
    if (grepl(pattern, stancode)) {
      found_types <- c(found_types, glm_type)
    }
  }
  found_types
}

#' Extract GLM Parameters for Specific Response
#' @noRd
extract_glm_params_for_response <- function(stancode, response_name, glm_type) {
  response_pattern <- paste0("Y_", response_name)
  pattern <- paste0(glm_type, "_l(pdf|pmf)\\([^)]*", response_pattern, "[^)]*\\)")
  matches <- regmatches(stancode, regexpr(pattern, stancode))
  
  if (length(matches) == 0) return(NULL)
  
  # Extract parameters from matched call
  call_content <- gsub(paste0("^", glm_type, "_l(pdf|pmf)\\s*\\(|\\)$"), "", matches[1])
  parts <- strsplit(call_content, "\\|")[[1]]
  
  if (length(parts) < 2) return(NULL)
  
  response_var <- trimws(parts[1])
  predictor_part <- trimws(parts[2])
  pred_params <- trimws(strsplit(predictor_part, ",")[[1]])
  
  list(
    response = response_var,
    design_matrix = if (length(pred_params) >= 1) pred_params[1] else NULL,
    intercept = if (length(pred_params) >= 2) pred_params[2] else NULL,
    coefficients = if (length(pred_params) >= 3) pred_params[3] else NULL,
    other_params = if (length(pred_params) > 3) pred_params[4:length(pred_params)] else NULL
  )
}