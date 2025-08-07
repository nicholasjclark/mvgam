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

#' Validate Stan Code Fragment
#'
#' @description
#' Validates individual Stan code fragments (from stanvars) without requiring
#' complete model structure. Checks syntax, braces, and basic content without
#' enforcing the presence of all Stan blocks.
#'
#' @param fragment Character string containing Stan code fragment
#' @param expected_content Optional character vector of strings expected in fragment
#' @param expected_block Optional character string of expected block type ("functions", "data", etc.)
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_stan_code_fragment <- function(fragment, expected_content = NULL, expected_block = NULL) {
  checkmate::assert_string(fragment, min.chars = 1)

  # Basic syntax validation: check for balanced braces
  if (!are_braces_balanced(fragment)) {
    stop(insight::format_error(
      "Stan code fragment has unbalanced braces.",
      "Check for missing opening or closing braces in fragment."
    ))
  }

  # Check for expected content if provided
  if (!is.null(expected_content)) {
    missing_content <- character(0)
    for (content in expected_content) {
      if (!grepl(content, fragment, fixed = TRUE)) {
        missing_content <- c(missing_content, content)
      }
    }

    if (length(missing_content) > 0) {
      stop(insight::format_error(
        "Stan code fragment missing expected content:",
        paste(missing_content, collapse = ", "),
        "Fragment may be incomplete or incorrectly generated."
      ))
    }
  }

  # Check for expected block declaration if provided
  if (!is.null(expected_block)) {
    block_pattern <- paste0(expected_block, "\\s*\\{")
    if (!grepl(block_pattern, fragment)) {
      stop(insight::format_error(
        "Stan code fragment missing expected block: {.field {expected_block}}",
        "Fragment should contain '{expected_block} {{' declaration."
      ))
    }
  }

  # Basic content validation: should not be just whitespace
  if (nchar(trimws(fragment)) == 0) {
    stop(insight::format_error(
      "Stan code fragment is empty or contains only whitespace."
    ))
  }

  invisible(TRUE)
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


#' Validate Stan Data Structure
#'
#' @description
#' Validates that Stan data has proper structure and types.
#'
#' @param stan_data List containing Stan data
#' @return Invisible TRUE if valid, stops/warns for issues
#' @noRd
validate_stan_data_structure <- function(stan_data) {
  checkmate::assert_list(stan_data)

  # Check for empty data
  if (length(stan_data) == 0) {
    stop(insight::format_error(
      "Empty Stan data provided.",
      "Stan models require at least some data elements."
    ))
  }

  # Check data types (Stan expects numeric, integer, or array types)
  valid_types <- c("numeric", "integer", "logical", "matrix", "array")

  for (name in names(stan_data)) {
    data_type <- class(stan_data[[name]])[1]

    if (!data_type %in% valid_types && !is.numeric(stan_data[[name]])) {
      insight::format_warning(
        "Unexpected data type for Stan element '{name}': {data_type}",
        "Stan typically expects numeric, integer, or matrix types."
      )
    }
  }

  invisible(TRUE)
}

#' Check if Object is Valid Stanvar
#'
#' @description
#' Validates that an object has the structure expected for a brms stanvar.
#'
#' @param stanvar Object to validate
#' @return Logical indicating whether object is a valid stanvar
#' @noRd
is_valid_stanvar <- function(stanvar) {
  # Handle NULL input
  if (is.null(stanvar)) {
    return(FALSE)
  }

  # Must be a list
  if (!is.list(stanvar)) {
    return(FALSE)
  }

  # Check if it's a brms stanvar object (has required brms components)
  required_components <- c("scode", "block")
  if (!all(required_components %in% names(stanvar))) {
    return(FALSE)
  }

  # scode must be character string (can be empty for data block stanvars)
  if (!is.character(stanvar$scode) || length(stanvar$scode) != 1) {
    return(FALSE)
  }

  # block must be valid Stan block name (brms uses abbreviated forms)
  valid_blocks <- c("data", "tdata", "parameters", "tparameters",
                   "model", "genquant",
                   "transformed_data", "transformed_parameters", "generated_quantities")
  if (!is.character(stanvar$block) || length(stanvar$block) != 1 ||
      !stanvar$block %in% valid_blocks) {
    return(FALSE)
  }

  return(TRUE)
}

#' Validate Trend Specification
#'
#' Checks if a trend specification is valid for code generation.
#'
#' @param trend_spec Trend specification object
#' @return Logical indicating validity
#' @noRd
validate_trend_spec <- function(trend_spec) {
  if (!is.list(trend_spec)) return(FALSE)
  if (is.null(trend_spec$trend_type)) return(FALSE)
  if (!trend_spec$trend_type %in% list_trend_types()$trend_type) return(FALSE)

  TRUE
}

#' Validate Time Series Structure for Trends
#'
#' @description
#' Ensures time series data is compatible with specified trend models.
#' Leverages existing mvgam validation functions.
#'
#' @param data Data to validate (data.frame or list)
#' @param trend_spec Trend specification containing trend model info
#' @param silent Verbosity level
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_time_series_for_trends <- function(data, trend_spec, silent = 1) {
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_list(trend_spec)

  # Use existing mvgam validation function
  # Pass the complete trend_spec (mvgam_trend object) for variable names
  validated_data <- validate_series_time(
      data = data,
      name = "data",
      trend_model = trend_spec,
      check_levels = TRUE,
      check_times = TRUE
    )
  
  # Additional brms-specific validations for time series
  # Only validate temporal structure for trends that require it
  if (trend_model %in% c("RW", "AR", "VAR")) {
    check_regular_time_intervals(data, silent = silent)
  }

  if (silent < 2) {
    message("Time series structure validated for trend model: ", trend_model)
  }

  invisible(TRUE)
}

#'Argument validation functions
#'@param data Data to be validated (list or data.frame)
#'@noRd
validate_series_time = function(
    data,
    name = 'data',
    trend_model,
    check_levels = TRUE,
    check_times = TRUE
) {
  # Extract variable names from the mvgam_trend object
  time_var <- trend_model$time
  series_var <- trend_model$series

  # Now we only need the character trend_model string
  trend_model_type <- trend_model$trend_model

  # Validate any grouping structure
  data <- validate_grouping_structure(
    data = data,
    trend_model = trend_model,
    name = name
  )

  # brms only accepts data.frames, so validate that first
  checkmate::assert_data_frame(data, .var.name = name)

  # Ungroup any grouped data
  data %>%
    dplyr::ungroup() -> data

  # Check that series variable exists and is a factor
  if (!series_var %in% colnames(data)) {
    stop(glue::glue("{name} does not contain a '{series_var}' variable"), call. = FALSE)
  }

  if (!is.factor(data[[series_var]])) {
    stop(insight::format_error(
      "Variable '{series_var}' must be a factor.",
      "Convert to factor using: data${series_var} <- factor(data${series_var})"
    ), call. = FALSE)
  }

  # Check for unused factor levels in series variable
  data <- validate_factor_levels(data, series_var, name, auto_drop = FALSE)

  # Check that time variable exists
  if (!time_var %in% colnames(data)) {
    stop(glue::glue("{name} does not contain a '{time_var}' variable"), call. = FALSE)
  }

  # Series factor must have all unique levels present if this is a
  # forecast check
  if (check_levels) {
    if (!all(levels(data[[series_var]]) %in% unique(data[[series_var]]))) {
      stop(
        glue::glue(
          'Mismatch between factor levels of "{series_var}" and unique values of "{series_var}"\n',
          'Use\n  `setdiff(levels(data${series_var}), unique(data${series_var}))` \nand\n',
          '  `intersect(levels(data${series_var}), unique(data${series_var}))`\nfor guidance'
        ),
        call. = FALSE
      )
    }
  }

  # Ensure each series has an observation, even if NA, for each
  # unique timepoint (only for trend models that require discrete time with
  # regularly spaced sampling intervals)
  if (check_times) {
    all_times_avail = function(time, min_time, max_time) {
      identical(
        as.numeric(sort(time)),
        as.numeric(seq.int(from = min_time, to = max_time))
      )
    }
    min_time <- as.numeric(min(data[[time_var]]))
    max_time <- as.numeric(max(data[[time_var]]))
    data.frame(series = data[[series_var]], time = data[[time_var]]) %>%
      dplyr::group_by(series) %>%
      dplyr::summarise(
        all_there = all_times_avail(time, min_time, max_time)
      ) -> checked_times
    if (any(checked_times$all_there == FALSE)) {
      stop(
        "One or more series in ",
        name,
        " is missing observations for one or more timepoints",
        call. = FALSE
      )
    }
  }

  return(data)
}

#' Validate Grouping Structure
#'
#' @description
#' Validates that grouping variables (gr, subgr) exist, are factors, and have
#' proper hierarchical structure for trend models that use grouping.
#'
#' @param data Data frame to validate
#' @param trend_model mvgam_trend object with grouping specifications
#' @param name Name of data object for error messages
#' @return Original data (validation only, no transformation)
#' @noRd
validate_grouping_structure = function(data, trend_model, name = 'data') {
  checkmate::assert_data_frame(data)

  # Extract variable names from trend_model
  gr_var <- if (!is.null(trend_model$gr) && trend_model$gr != 'NA') trend_model$gr else NULL
  subgr_var <- if (!is.null(trend_model$subgr) && trend_model$subgr != 'NA') trend_model$subgr else NULL

  # If no grouping is used, return early
  if (is.null(gr_var) && is.null(subgr_var)) {
    return(data)
  }

  # Validate grouping variables if they exist
  if (!is.null(gr_var)) {
    # Check gr variable exists and is factor
    if (!gr_var %in% names(data)) {
      stop(insight::format_error(
        "{name} does not contain grouping variable '{gr_var}'.",
        "The grouping variable '{gr_var}' was specified in the trend constructor but is missing from the data."
      ), call. = FALSE)
    }

    if (!is.factor(data[[gr_var]])) {
      stop(insight::format_error(
        "Grouping variable '{gr_var}' must be a factor.",
        "Convert to factor using: data${gr_var} <- factor(data${gr_var})"
      ), call. = FALSE)
    }

    # Check for unused factor levels in gr variable
    data <- validate_factor_levels(data, gr_var, name, auto_drop = FALSE)
  }

  if (!is.null(subgr_var)) {
    # Check subgr variable exists and is factor
    if (!subgr_var %in% names(data)) {
      stop(insight::format_error(
        "{name} does not contain subgrouping variable '{subgr_var}'.",
        "The subgrouping variable '{subgr_var}' was specified in the trend constructor but is missing from the data."
      ), call. = FALSE)
    }

    if (!is.factor(data[[subgr_var]])) {
      stop(insight::format_error(
        "Subgrouping variable '{subgr_var}' must be a factor.",
        "Convert to factor using: data${subgr_var} <- factor(data${subgr_var})"
      ), call. = FALSE)
    }

    # Check for unused factor levels in subgr variable
    data <- validate_factor_levels(data, subgr_var, name, auto_drop = FALSE)
  }

  # If both gr and subgr are specified, validate hierarchical structure
  if (!is.null(gr_var) && !is.null(subgr_var)) {
    validate_complete_grouping(data, gr_var, subgr_var, name)
  }

  return(data)
}

#' Validate Factor Levels
#'
#' @description
#' Checks for unused factor levels that could cause Stan indexing issues.
#' Issues warnings for validation phase, allows auto-dropping in preparation phase.
#'
#' @param data Data frame containing the factor variable
#' @param var_name Name of the factor variable to check
#' @param data_name Name of the data object (for error messages)
#' @param auto_drop Whether to automatically drop unused levels
#' @return Modified data if auto_drop=TRUE, otherwise original data
#' @noRd
validate_factor_levels <- function(data, var_name, data_name = "data", auto_drop = FALSE) {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(var_name)
  checkmate::assert_flag(auto_drop)

  if (!var_name %in% names(data)) {
    return(data)  # Variable doesn't exist, will be caught elsewhere
  }

  if (!is.factor(data[[var_name]])) {
    return(data)  # Not a factor, will be caught elsewhere
  }

  var_data <- data[[var_name]]
  used_levels <- unique(var_data)
  all_levels <- levels(var_data)
  unused_levels <- setdiff(all_levels, used_levels)

  if (length(unused_levels) > 0) {
    if (auto_drop) {
      # Auto-drop unused levels
      data[[var_name]] <- droplevels(var_data)
    } else {
      # Warn about unused levels
      rlang::warn(
        message = insight::format_warning(
          "Factor variable '{var_name}' in {data_name} has unused levels: {paste(unused_levels, collapse = ', ')}.",
          "Consider using droplevels() to remove unused factor levels.",
          "This may cause indexing issues in Stan model compilation."
        ),
        .frequency = "once"
      )
    }
  }

  return(data)
}

#' Validate Complete Grouping Structure
#'
#' @description
#' For hierarchical models, checks that each level of the grouping variable
#' contains observations for all levels of the subgrouping variable.
#'
#' @param data Data frame
#' @param gr_var Name of the grouping variable
#' @param subgr_var Name of the subgrouping variable
#' @param data_name Name of the data object (for error messages)
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_complete_grouping <- function(data, gr_var, subgr_var, data_name = "data") {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(gr_var)
  checkmate::assert_string(subgr_var)

  # Check that each level of gr contains all possible levels of subgr
  grouping_summary <- data %>%
    dplyr::group_by(!!rlang::sym(gr_var)) %>%
    dplyr::summarise(
      n_subgroups = dplyr::n_distinct(!!rlang::sym(subgr_var)),
      .groups = "drop"
    )

  total_subgroups <- dplyr::n_distinct(data[[subgr_var]])

  if (any(grouping_summary$n_subgroups != total_subgroups)) {
    stop(insight::format_error(
      "Incomplete grouping structure in {data_name}.",
      "Some levels of '{gr_var}' do not contain all levels of '{subgr_var}'.",
      "Each group must contain observations for all subgroups."
    ), call. = FALSE)
  }

  invisible(TRUE)
}

#' Check Regular Time Intervals
#'
#' @description
#' Ensures time series has regular sampling intervals for State-Space models.
#'
#' @param data Data to check
#' @param silent Verbosity level
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
check_regular_time_intervals <- function(data, silent = 1) {
  if (!"time" %in% names(data)) {
    return(invisible(TRUE))  # Will be caught by main validation
  }

  # Check for regular intervals within each series
  if ("series" %in% names(data)) {
    data %>%
      dplyr::group_by(series) %>%
      dplyr::summarise(
        time_diffs = list(diff(sort(unique(time)))),
        regular = length(unique(diff(sort(unique(time))))) <= 1,
        .groups = "drop"
      ) -> time_checks

    irregular_series <- time_checks$series[!time_checks$regular]

    if (length(irregular_series) > 0) {
      stop(insight::format_error(
        "Irregular time intervals detected in series: {paste(irregular_series, collapse = ', ')}",
        "State-Space models require regular sampling intervals.",
        "Consider using CAR models for irregular time series."
      ))
    }
  } else {
    # Single series case
    time_diffs <- diff(sort(unique(data$time)))
    if (length(unique(time_diffs)) > 1) {
      stop(insight::format_error(
        "Irregular time intervals detected in time series.",
        "State-Space models require regular sampling intervals.",
        "Time differences: {paste(unique(time_diffs), collapse = ', ')}"
      ))
    }
  }

  invisible(TRUE)
}

#' Validate Combined Stan Data
#'
#' @description
#' Validates that combined Stan data is consistent and contains required elements.
#'
#' @param combined_data List of combined Stan data
#' @param obs_data List of observation Stan data
#' @param trend_data List of trend Stan data
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_combined_standata <- function(combined_data, obs_data, trend_data) {
  checkmate::assert_list(combined_data, names = "named")
  checkmate::assert_list(obs_data, names = "named")
  checkmate::assert_list(trend_data, names = "named")

  # Check that essential observation data elements are preserved
  essential_obs <- c("N", "K", "y", "X")
  missing_obs <- setdiff(essential_obs, names(combined_data))
  missing_obs <- missing_obs[missing_obs %in% names(obs_data)]

  if (length(missing_obs) > 0) {
    stop(insight::format_error(
      "Essential observation data elements missing from combined data:",
      paste(missing_obs, collapse = ", ")
    ))
  }

  # Check for dimensional consistency
  if ("N" %in% names(combined_data) && "n" %in% names(combined_data)) {
    if (combined_data$N != combined_data$n) {
      insight::format_warning(
        "Potential dimension mismatch between N ({combined_data$N}) and n ({combined_data$n}).",
        "Check observation and trend data compatibility."
      )
    }
  }

  # Check that trend data makes sense
  if ("n_series" %in% names(combined_data)) {
    if (combined_data$n_series < 1) {
      stop(insight::format_error(
        "Invalid number of series: {combined_data$n_series}",
        "Must be at least 1 for mvgam models."
      ))
    }
  }

  invisible(TRUE)
}

#'@noRd
deparse_variable = function(...) {
  deparse0(substitute(...))
}

#'@noRd
as_one_logical = function(x, allow_na = FALSE) {
  s <- substitute(x)
  x <- as.logical(x)
  if (length(x) != 1L || anyNA(x) && !allow_na) {
    s <- deparse0(s, max_char = 100L)
    stop("Cannot coerce '", s, "' to a single logical value.", call. = FALSE)
  }
  x
}

#'@noRd
as_one_integer <- function(x, allow_na = FALSE) {
  s <- substitute(x)
  x <- suppressWarnings(as.integer(x))
  if (length(x) != 1L || anyNA(x) && !allow_na) {
    s <- deparse0(s, max_char = 100L)
    stop("Cannot coerce '", s, "' to a single integer value.", call. = FALSE)
  }
  x
}

#'@noRd
deparse0 <- function(x, max_char = NULL, ...) {
  out <- collapse(deparse(x, ...))
  if (isTRUE(max_char > 0)) {
    out <- substr(out, 1L, max_char)
  }
  out
}

#'@noRd
collapse <- function(..., sep = "") {
  paste(..., sep = sep, collapse = "")
}

#'@noRd
validate_silent <- function(silent) {
  silent <- as_one_integer(silent)
  if (silent < 0 || silent > 2) {
    stop("'silent' must be between 0 and 2.", call. = FALSE)
  }
  silent
}

#'@noRd
validate_gr_subgr = function(gr, subgr, cor) {
  gr <- deparse0(substitute(gr))
  subgr <- deparse0(substitute(subgr))

  if (gr != 'NA') {
    if (subgr == 'NA') {
      stop(
        'argument "subgr" must be supplied if "gr" is also supplied',
        call. = FALSE
      )
    }
  }

  if (subgr != 'NA') {
    if (gr == 'NA') {
      stop(
        'argument "gr" must be supplied if "subgr" is also supplied',
        call. = FALSE
      )
    } else {
      cor <- TRUE
    }
  }

  list(.group = gr, .subgroup = subgr, .cor = cor)
}

#'@noRd
validate_proportional = function(x) {
  s <- substitute(x)
  x <- base::suppressWarnings(as.numeric(x))
  if (length(x) != 1L || anyNA(x)) {
    stop("Argument '", s, "' must be a single numeric value", call. = FALSE)
  }

  if (x < 0 || x > 1) {
    stop(
      "Argument '",
      s,
      "' must be a proportion ranging from 0 to 1, inclusive",
      call. = FALSE
    )
  }
}

#'@noRd
validate_equaldims = function(x, y) {
  s <- substitute(x)
  q <- substitute(y)

  if (NCOL(x) != NCOL(y)) {
    stop(
      "Argument '",
      s,
      "' and argument '",
      q,
      "' must have equal dimensions",
      call. = FALSE
    )
  }

  if (NROW(x) != NROW(y)) {
    stop(
      "Argument '",
      s,
      "' and argument '",
      q,
      "' must have equal dimensions",
      call. = FALSE
    )
  }
}

#'@noRd
validate_pos_integer = function(x) {
  s <- substitute(x)
  x <- base::suppressWarnings(as.numeric(x))
  if (length(x) != 1L || anyNA(x)) {
    stop("Argument '", s, "' must be a single numeric value", call. = FALSE)
  }

  if (sign(x) != 1) {
    stop("Argument '", s, "' must be a positive integer", call. = FALSE)
  } else {
    if (x %% 1 != 0) {
      stop("Argument '", s, "' must be a positive integer", call. = FALSE)
    }
  }
}

#'@noRd
validate_pos_integers = function(x) {
  s <- substitute(x)

  val_pos = function(y, s) {
    y <- base::suppressWarnings(as.numeric(y))
    if (sign(y) != 1) {
      stop("Negative values in ", s, " detected", call. = FALSE)
    } else {
      if (y %% 1 != 0) {
        stop("Non-integer values in ", s, " detected", call. = FALSE)
      }
    }
  }
  res <- lapply(seq_along(x), function(i) val_pos(x[i], s))
}

#'@noRd
validate_even <- function(x) {
  s <- substitute(x)
  x <- base::suppressWarnings(as.numeric(x))
  if (x %% 2 != 0) {
    stop("Argument '", s, "'  must be an even integer", call. = FALSE)
  }
}

#'@noRd
validate_pos_real = function(x) {
  s <- substitute(x)
  x <- base::suppressWarnings(as.numeric(x))
  if (length(x) != 1L || anyNA(x)) {
    stop("Argument '", s, "' must be a single numeric value", call. = FALSE)
  }

  if (sign(x) != 1) {
    stop("Argument '", s, "' must be a positive real value", call. = FALSE)
  }
}

#'@noRd
as_one_character <- function(x, allow_na = FALSE) {
  s <- substitute(x)
  x <- as.character(x)
  if (length(x) != 1L || anyNA(x) && !allow_na) {
    s <- deparse0(s, max_char = 100L)
    stop("Cannot coerce '", s, "' to a single character value.", call. = FALSE)
  }
  x
}
