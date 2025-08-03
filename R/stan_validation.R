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
validate_stan_code <- function(stan_code, backend = "rstan", ...) {
  checkmate::assert_string(stan_code)
  checkmate::assert_choice(backend, c("rstan", "cmdstanr"))

  # Handle empty string case - always error for empty code
  if (nchar(stan_code) == 0) {
    stop(insight::format_error(
      "Empty Stan code provided.",
      "Stan code must contain at least one character."
    ))
  }

  # Primary validation using rstan::stanc() (most comprehensive and up-to-date)
  if (backend == "rstan") {
    if (!requireNamespace("rstan", quietly = TRUE)) {
      stop(insight::format_error(
        "Package {.pkg rstan} is required for Stan code validation.",
        "Install rstan or use cmdstanr backend."
      ))
    }

    # Always let rstan::stanc() errors show directly - no masking
    rstan::stanc(model_code = stan_code, verbose = FALSE, ...)
    invisible(TRUE)
  } else {
    # cmdstanr backend (fallback)
    return(parse_model_cmdstanr(stan_code, ...))
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

  # Write Stan model to temporary file - let errors bubble up
  temp_file <- cmdstanr::write_stan_file(model)

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

  # Check syntax and return code - let errors bubble up
  out$check_syntax(quiet = TRUE)
  return(paste(out$code(), collapse = "\n"))
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
#' @param silent Logical indicating whether to suppress error messages
#' @return Logical indicating whether syntax is valid (if silent=TRUE), otherwise invisible TRUE or stops with error
#' @noRd
# validate_stan_syntax removed - functionality merged into validate_stan_code

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

# Comprehensive Stan Code Validation
# ==================================

#' Validate Combined Stan Code Result
#'
#' @description
#' Performs comprehensive validation of a combined Stan code result including
#' syntax checking, data-code compatibility, and structure validation.
#'
#' @param result List containing stancode, standata, and has_trends fields
#' @param silent Logical indicating whether to suppress validation messages
#' @return List with validation results (valid, syntax_valid, data_valid)
#' @noRd
validate_combined_stancode <- function(result, silent = FALSE) {
  checkmate::assert_list(result, names = "named")
  checkmate::assert_flag(silent)

  # Initialize validation results
  validation <- list(
    valid = FALSE,
    syntax_valid = FALSE,
    data_valid = FALSE,
    errors = character(0)
  )

  # Check required fields
  required_fields <- c("stancode", "standata", "has_trends")
  missing_fields <- setdiff(required_fields, names(result))

  if (length(missing_fields) > 0) {
    validation$errors <- c(validation$errors,
                          paste("Missing required fields:",
                                paste(missing_fields, collapse = ", ")))
    return(validation)
  }

  # Validate Stan code syntax - let errors bubble up naturally
  validate_stan_code_structure(result$stancode)
  validate_stan_code(result$stancode)  # Use unified validation function
  validation$syntax_valid <- TRUE

  # Validate data-code compatibility - let errors bubble up naturally
  validate_data_code_compatibility(result$stancode, result$standata)
  validation$data_valid <- TRUE

  # Overall validation status
  validation$valid <- validation$syntax_valid && validation$data_valid

  return(validation)
}

#' Validate Data-Code Compatibility
#'
#' @description
#' Checks that all data variables declared in Stan code are present in standata
#' and that data types are compatible.
#'
#' @param stan_code Character string containing Stan model code
#' @param stan_data List containing Stan data
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_data_code_compatibility <- function(stan_code, stan_data) {
  checkmate::assert_string(stan_code, min.chars = 1)
  checkmate::assert_list(stan_data, names = "named")

  # Extract data block from Stan code (split into lines first)
  code_lines <- strsplit(stan_code, "\n")[[1]]
  data_block <- extract_code_block(code_lines, "data")

  # Parse variable declarations from data block
  required_vars <- parse_data_declarations(data_block)

  # Check that all required variables are present in stan_data
  missing_vars <- setdiff(required_vars, names(stan_data))

  if (length(missing_vars) > 0) {
    stop(insight::format_error(
      "Missing required data variables: {.field {missing_vars}}",
      "Stan data block declares variables not provided in standata.",
      "Add missing variables to standata or remove from Stan data block."
    ))
  }

  invisible(TRUE)
}

#' Parse Data Declarations from Stan Data Block
#'
#' @description
#' Extracts variable names from Stan data block declarations.
#' This is a simplified parser for basic variable declarations.
#'
#' @param data_block Character string containing Stan data block content
#' @return Character vector of declared variable names
#' @noRd
parse_data_declarations <- function(data_block) {
  checkmate::assert_string(data_block)

  if (nchar(data_block) == 0) {
    return(character(0))
  }

  # Split into lines and clean up
  lines <- strsplit(data_block, "\n")[[1]]
  lines <- trimws(lines)
  lines <- lines[nchar(lines) > 0]  # Remove empty lines
  lines <- lines[!grepl("^//", lines)]  # Remove comment lines

  var_names <- character(0)

  for (line in lines) {
    # Look for variable declarations (simplified pattern)
    # Pattern: type<constraints> variable_name;
    # Examples: "int N;", "vector[N] y;", "real<lower=0> sigma;"

    # Remove inline comments
    line <- sub("//.*$", "", line)
    line <- trimws(line)

    if (nchar(line) == 0) next

    # Basic pattern for variable declarations
    # This is a simplified approach - a full parser would be more robust
    if (grepl(";\\s*$", line)) {  # Line ends with semicolon
      # Extract variable name (last word before semicolon)
      clean_line <- gsub(";\\s*$", "", line)  # Remove semicolon
      tokens <- strsplit(clean_line, "\\s+")[[1]]

      if (length(tokens) >= 2) {
        # Variable name is typically the last token
        var_name <- tokens[length(tokens)]

        # Remove array subscripts if present: variable[N] -> variable
        var_name <- gsub("\\[.*\\]", "", var_name)

        # Remove any remaining special characters
        var_name <- gsub("[^a-zA-Z0-9_]", "", var_name)

        if (nchar(var_name) > 0) {
          var_names <- c(var_names, var_name)
        }
      }
    }
  }

  return(unique(var_names))
}

#' Check Basic Semicolon Syntax in Stan Code
#'
#' @description
#' Performs basic checks for missing semicolons in Stan variable declarations.
#' This is a simplified check focusing on obvious syntax errors.
#'
#' @param stan_code Character string containing Stan model code
#' @return Logical indicating whether semicolon syntax appears correct
#' @noRd
check_semicolon_syntax <- function(stan_code) {
  checkmate::assert_string(stan_code)

  # Extract code blocks that should have semicolon-terminated statements (split into lines first)
  code_lines <- strsplit(stan_code, "\n")[[1]]
  data_block <- extract_code_block(code_lines, "data")
  param_block <- extract_code_block(code_lines, "parameters")

  # Check each block for syntax issues
  blocks_to_check <- list(
    data = data_block,
    parameters = param_block
  )

  for (block_name in names(blocks_to_check)) {
    block_content <- blocks_to_check[[block_name]]

    if (nchar(block_content) > 0) {
      if (!check_block_semicolons(block_content)) {
        return(FALSE)
      }
    }
  }

  return(TRUE)
}

#' Check Semicolons in a Stan Code Block
#'
#' @description
#' Checks for missing semicolons in variable declarations within a Stan block.
#'
#' @param block_content Character string containing Stan block content
#' @return Logical indicating whether semicolon syntax is correct
#' @noRd
check_block_semicolons <- function(block_content) {
  checkmate::assert_string(block_content)

  if (nchar(block_content) == 0) {
    return(TRUE)
  }

  # Split into lines and clean up
  lines <- strsplit(block_content, "\n")[[1]]
  lines <- trimws(lines)
  lines <- lines[nchar(lines) > 0]  # Remove empty lines
  lines <- lines[!grepl("^//", lines)]  # Remove comment-only lines

  for (line in lines) {
    # Remove inline comments
    line <- sub("//.*$", "", line)
    line <- trimws(line)

    if (nchar(line) == 0) next

    # Skip lines that are just braces or control structures
    if (grepl("^[{}]\\s*$", line)) next

    # Check if line looks like a variable declaration or statement
    # Basic pattern: looks like a type declaration or assignment
    if (grepl("\\b(int|real|vector|matrix|array)\\b", line, ignore.case = FALSE) ||
        grepl("\\w+\\s*[~=]", line)) {

      # This line should end with a semicolon
      if (!grepl(";\\s*$", line)) {
        return(FALSE)
      }
    }
  }

  return(TRUE)
}
