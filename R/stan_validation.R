#' Stan Code Validation System for mvgam
#'
#' @description
#' Provides Stan code validation using both rstan and cmdstanr backends,
#' leveraging existing mvgam backend functions for consistency.

#' Validate Stan Model Code
#' 
#' @description
#' Validates Stan model code using appropriate backend (rstan or cmdstanr).
#' Leverages existing eval_silent function from mvgam::backends.R.
#' 
#' @param model Character string containing Stan model code
#' @param backend Character string specifying backend ("rstan" or "cmdstanr")
#' @param silent Numeric indicating verbosity level (1 = minimal, 2 = silent)
#' @param ... Additional arguments passed to validation functions
#' @return Character string of validated Stan model code
#' @noRd
validate_stan_code <- function(model, backend = "rstan", silent = 1, ...) {
  checkmate::assert_string(model, min.chars = 1)
  checkmate::assert_choice(backend, c("rstan", "cmdstanr"))
  checkmate::assert_number(silent, lower = 0, upper = 2)
  
  if (backend == "rstan") {
    return(parse_model_rstan(model, silent = silent, ...))
  } else {
    return(parse_model_cmdstanr(model, silent = silent, ...))
  }
}

#' Parse Stan Model Code with rstan
#' 
#' @description
#' Validates Stan model code using rstan::stanc.
#' Based on existing mvgam patterns and brms approach.
#' 
#' @param model Stan model code
#' @param silent Numeric indicating verbosity level
#' @param ... Additional arguments passed to rstan::stanc
#' @return Validated Stan model code
#' @noRd
parse_model_rstan <- function(model, silent = 1, ...) {
  checkmate::assert_string(model, min.chars = 1)
  
  # Check if rstan is available
  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop(insight::format_error(
      "Package {.pkg rstan} is required for Stan code validation.",
      "Install rstan or use cmdstanr backend."
    ))
  }
  
  # Parse Stan model code with rstan using existing eval_silent
  out <- eval_silent(
    rstan::stanc(model_code = model, ...),
    type = "message", 
    try = TRUE, 
    silent = silent >= 1L
  )
  
  if (inherits(out, "try-error")) {
    stop(insight::format_error(
      "Stan code validation failed with rstan backend.",
      "Check Stan syntax and model structure.",
      "Error details: {attr(out, 'condition')$message}"
    ))
  }
  
  # Return validated model code
  if (!is.null(out$model_code)) {
    return(out$model_code)
  } else {
    stop(insight::format_error(
      "Stan code validation did not return model code.",
      "rstan::stanc failed to produce validated output."
    ))
  }
}

#' Parse Stan Model Code with cmdstanr
#' 
#' @description
#' Validates Stan model code using cmdstanr::cmdstan_model without compilation.
#' Based on existing mvgam patterns in backends.R.
#' 
#' @param model Stan model code
#' @param silent Numeric indicating verbosity level
#' @param ... Additional arguments passed to cmdstanr functions
#' @return Validated Stan model code
#' @noRd
parse_model_cmdstanr <- function(model, silent = 1, ...) {
  checkmate::assert_string(model, min.chars = 1)
  
  # Check if cmdstanr is available
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop(insight::format_error(
      "Package {.pkg cmdstanr} is required for Stan code validation.",
      "Install cmdstanr or use rstan backend."
    ))
  }
  
  # Write Stan model to temporary file
  temp_file <- try({
    cmdstanr::write_stan_file(model)
  }, silent = TRUE)
  
  if (inherits(temp_file, "try-error")) {
    stop(insight::format_error(
      "Failed to write Stan model to temporary file.",
      "Check Stan code format and system permissions."
    ))
  }
  
  # Validate using cmdstan_model without compilation, using existing eval_silent
  out <- eval_silent(
    cmdstanr::cmdstan_model(temp_file, compile = FALSE, ...),
    type = "message", 
    try = TRUE, 
    silent = silent > 0L
  )
  
  if (inherits(out, "try-error")) {
    stop(insight::format_error(
      "Stan code validation failed with cmdstanr backend.",
      "Check Stan syntax and model structure.",
      "Error details: {attr(out, 'condition')$message}"
    ))
  }
  
  # Check syntax and return code
  syntax_check <- try({
    out$check_syntax(quiet = TRUE)
    paste(out$code(), collapse = "\n")
  }, silent = TRUE)
  
  if (inherits(syntax_check, "try-error")) {
    stop(insight::format_error(
      "Stan syntax check failed.",
      "Model contains syntax errors that prevent validation."
    ))
  }
  
  return(syntax_check)
}

# Stan Code Structure and Syntax Validation Utilities
# ===================================================

#' Validate Stan Code Structure
#' 
#' @description
#' Validates that Stan code contains required blocks (data, parameters, model).
#' 
#' @param stan_code Character string containing Stan model code
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_stan_code_structure <- function(stan_code) {
  checkmate::assert_string(stan_code, min.chars = 1)
  
  # Required Stan blocks
  required_blocks <- c("data", "parameters", "model")
  
  # Check for each required block
  missing_blocks <- character(0)
  
  for (block in required_blocks) {
    # Pattern to match block declaration
    block_pattern <- paste0("\\b", block, "\\s*\\{")
    
    if (!grepl(block_pattern, stan_code, ignore.case = FALSE)) {
      missing_blocks <- c(missing_blocks, block)
    }
  }
  
  if (length(missing_blocks) > 0) {
    stop(insight::format_error(
      "Missing required Stan block{?s}: {.field {missing_blocks}}",
      "Stan models must contain data, parameters, and model blocks."
    ))
  }
  
  invisible(TRUE)
}

#' Validate Stan Syntax
#' 
#' @description
#' Validates basic Stan syntax including brace balancing.
#' 
#' @param stan_code Character string containing Stan model code
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_stan_syntax <- function(stan_code) {
  checkmate::assert_string(stan_code, min.chars = 1)
  
  # Check brace balancing
  if (!are_braces_balanced(stan_code)) {
    stop(insight::format_error(
      "Unbalanced braces detected in Stan code.",
      "Check that all opening braces {{}} have matching closing braces."
    ))
  }
  
  # Additional syntax checks could be added here
  # For now, focus on the most common issue (unbalanced braces)
  
  invisible(TRUE)
}

#' Check if Braces are Balanced
#' 
#' @description
#' Checks if opening and closing braces are properly balanced in Stan code.
#' 
#' @param stan_code Character string containing Stan model code
#' @return Logical indicating whether braces are balanced
#' @noRd
are_braces_balanced <- function(stan_code) {
  checkmate::assert_string(stan_code)
  
  # Split into individual characters
  chars <- unlist(strsplit(stan_code, "", fixed = TRUE))
  
  # Track brace depth
  depth <- 0
  
  for (char in chars) {
    if (char == "{") {
      depth <- depth + 1
    } else if (char == "}") {
      depth <- depth - 1
      
      # If depth goes negative, we have unmatched closing brace
      if (depth < 0) {
        return(FALSE)
      }
    }
  }
  
  # Return TRUE only if depth is exactly 0 (all braces matched)
  return(depth == 0)
}