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