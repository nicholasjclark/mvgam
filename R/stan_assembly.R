#' Stan Code Assembly System for mvgam
#'
#' @description
#' Complete Stan code generation and validation pipeline for mvgam-brms
#' integration. This file consolidates the two-stage assembly system,
#' trend-specific Stan code generators, and validation framework.
#'
#' @section Architecture:
#' The Stan assembly system follows a two-stage pipeline:
#' - **Stage 1**: brms generates base Stan code; mvgam produces trend stanvars
#' - **Stage 2**: mvgam injects trend effects into linear predictors
#' - **Validation**: Comprehensive Stan code structure and syntax validation
#' - **Generators**: Trend-specific Stan code generation for all trend types

# =============================================================================
# SECTION 1: STAN ASSEMBLY ORCHESTRATION
# =============================================================================
# WHY: The two-stage assembly system is critical for mvgam-brms integration
# because it allows mvgam to extend brms models without modifying brms
# internals. Stage 1 leverages brms' robust model compilation, while Stage 2
# adds mvgam-specific trend dynamics through stanvars injection.#' @noRd
generate_combined_stancode <- function(obs_setup, trend_setup = NULL,
                                      trend_spec = NULL, backend = "rstan",
                                      validate = TRUE, silent = 1) {
  checkmate::assert_list(obs_setup, names = "named")
  checkmate::assert_list(trend_setup, null.ok = TRUE)
  checkmate::assert_list(trend_spec, null.ok = TRUE)
  checkmate::assert_choice(backend, c("rstan", "cmdstanr"))
  checkmate::assert_flag(validate)
  checkmate::assert_number(silent)

  # If no trend specification, return observation model as-is
  if (is.null(trend_setup) || is.null(trend_spec)) {
    insight::format_warning(
      "No trend specification provided.",
      "Returning observation model without trend components."
    )

    return(list(
      stancode = obs_setup$stancode,
      standata = obs_setup$standata,
      has_trends = FALSE
    ))
  }

  # Stage 1: Extract trend stanvars from brms setup
  if (silent < 2) {
    message("Stage 1: Extracting trend stanvars from brms setup...")
  }

  trend_stanvars <- extract_trend_stanvars_from_setup(trend_setup, trend_spec)

  # Generate base observation Stan code with trend stanvars
  base_stancode <- generate_base_stancode_with_stanvars(
    obs_setup,
    trend_stanvars,
    backend = backend,
    silent = silent
  )

  # Stage 2: Modify observation linear predictor with trend injection
  if (silent < 2) {
    message("Stage 2: Injecting trend into observation linear predictor...")
  }

  combined_stancode <- inject_trend_into_linear_predictor(
    base_stancode,
    trend_stanvars,
    trend_spec
  )

  # Combine Stan data from both models
  combined_standata <- combine_stan_data(obs_setup$standata, trend_setup$standata)

  # Validate final Stan code if requested
  if (validate) {
    if (silent < 2) {
      message("Validating combined Stan code...")
    }

    validated_code <- validate_stan_code(
      combined_stancode,
      backend = backend,
      silent = silent
    )

    combined_stancode <- validated_code
  }

  # Apply any necessary autoformatting
  if (exists(".autoformat", envir = asNamespace("mvgam"))) {
    combined_stancode <- mvgam:::.autoformat(
      combined_stancode,
      backend = backend,
      silent = silent >= 1
    )
  }

  return(list(
    stancode = combined_stancode,
    standata = combined_standata,
    has_trends = TRUE,
    trend_spec = trend_spec,
    backend = backend
  ))
}

#' Generate Base Stan Code with Stanvars
#'
#' @description
#' Generates base brms Stan code incorporating trend stanvars.
#' Uses brms::make_stancode with injected stanvars.
#'
#' @param obs_setup List containing observation model setup
#' @param trend_stanvars List of trend stanvars to inject
#' @param backend Character string specifying backend
#' @param silent Numeric indicating verbosity level
#' @return Character string of base Stan code with stanvars
#' @noRd
generate_base_stancode_with_stanvars <- function(obs_setup, trend_stanvars,
                                                backend = "rstan", silent = 1) {
  checkmate::assert_list(obs_setup)
  checkmate::assert_list(trend_stanvars)

  # Combine existing stanvars with trend stanvars
  # brms requires stanvars to be a list of stanvar objects, not concatenated vector
  all_stanvars <- list()
  if (!is.null(obs_setup$stanvars)) {
    if (inherits(obs_setup$stanvars, "stanvar")) {
      all_stanvars <- list(obs_setup$stanvars)
    } else {
      all_stanvars <- obs_setup$stanvars
    }
  }
  if (!is.null(trend_stanvars)) {
    if (inherits(trend_stanvars, "stanvar")) {
      all_stanvars <- c(all_stanvars, list(trend_stanvars))
    } else {
      all_stanvars <- c(all_stanvars, trend_stanvars)
    }
  }
  if (length(all_stanvars) == 0) all_stanvars <- NULL

  # Generate Stan code using brms with combined stanvars
  base_code <- try({
    brms::make_stancode(
      formula = obs_setup$formula,
      data = obs_setup$data,
      family = obs_setup$family,
      stanvars = all_stanvars,
      prior = obs_setup$prior
    )
  }, silent = TRUE)

  if (inherits(base_code, "try-error")) {
    error_msg <- attr(base_code, 'condition')$message
    stop(insight::format_error(
      "Failed to generate base Stan code with trend stanvars.",
      "Check observation formula and trend stanvar compatibility.",
      paste("Error:", error_msg)
    ))
  }

  return(base_code)
}

#' Combine Stan Data
#'
#' @description
#' Combines Stan data from observation and trend models, handling conflicts.
#'
#' @param obs_data List containing observation model Stan data
#' @param trend_data List containing trend model Stan data
#' @return List of combined Stan data
#' @noRd
combine_stan_data <- function(obs_data, trend_data) {
  checkmate::assert_list(obs_data)
  checkmate::assert_list(trend_data)

  # Start with observation data as base
  combined_data <- obs_data

  # Add trend-specific data elements
  trend_only_names <- setdiff(names(trend_data), names(obs_data))
  combined_data[trend_only_names] <- trend_data[trend_only_names]

  # Handle conflicts by preferring trend data for trend-related elements
  trend_priority_patterns <- c("^trend_", "^n_trend", "^ar_", "^var_", "^rw_")

  for (pattern in trend_priority_patterns) {
    matching_names <- grep(pattern, names(trend_data), value = TRUE)
    combined_data[matching_names] <- trend_data[matching_names]
  }

  # Validate combined data consistency
  validate_combined_standata(combined_data, obs_data, trend_data)

  return(combined_data)
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

#' Create Stanvar Objects
#'
#' @description
#' Helper function to create brms stanvar objects for trend injection.
#'
#' @param x Data object to include in Stan model
#' @param name Character string name for the object in Stan
#' @param scode Character string Stan code type declaration
#' @return brms stanvar object
#' @noRd
stanvar <- function(x, name, scode = "data") {
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop(insight::format_error(
      "Package {.pkg brms} is required for stanvar creation."
    ))
  }

  # Use brms::stanvar if available
  if (exists("stanvar", where = asNamespace("brms"))) {
    return(brms::stanvar(x = x, name = name, scode = scode))
  } else {
    # Fallback structure if brms::stanvar not available
    structure(
      list(
        name = name,
        sdata = x,
        scode = scode
      ),
      class = "stanvar"
    )
  }
}

#' Integration with Enhanced mvgam Function
#'
#' @description
#' Integration point for enhanced mvgam function to use two-stage assembly.
#' Includes time series validation to ensure data compatibility.
#'
#' @param obs_setup Observation model setup from setup_brms_lightweight
#' @param trend_setup Trend model setup from setup_brms_lightweight
#' @param trend_spec Parsed trend specification
#' @param data Original data for validation
#' @param backend Stan backend to use
#' @param validate Whether to validate Stan code
#' @param silent Verbosity level
#' @return List ready for mvgam model fitting
#' @noRd
prepare_mvgam_stancode <- function(obs_setup, trend_setup, trend_spec,
                                  data = NULL, backend = "rstan",
                                  validate = TRUE, silent = 1) {

  # Validate time series structure if data provided and trends specified
  if (!is.null(data) && !is.null(trend_spec)) {
    validate_time_series_for_trends(data, trend_spec, silent = silent)
  }

  # Generate combined Stan code and data
  stan_components <- generate_combined_stancode(
    obs_setup = obs_setup,
    trend_setup = trend_setup,
    trend_spec = trend_spec,
    backend = backend,
    validate = validate,
    silent = silent
  )

  # Return in format expected by mvgam fitting functions
  return(list(
    model_code = stan_components$stancode,
    model_data = stan_components$standata,
    has_trends = stan_components$has_trends,
    trend_specification = trend_spec,
    backend = backend,
    validation_passed = validate
  ))
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

  # Extract trend model type for validation
  trend_model <- trend_spec$type %||% "None"

  # Use existing mvgam validation function
  # This will check for proper time/series structure
  validated_data <- try({
    validate_series_time(
      data = data,
      name = "data",
      trend_model = trend_model,
      check_levels = TRUE,
      check_times = TRUE
    )
  }, silent = TRUE)

  if (inherits(validated_data, "try-error")) {
    stop(insight::format_error(
      "Time series validation failed for trend model '{trend_model}'.",
      "Data structure incompatible with specified trends.",
      "Error: {attr(validated_data, 'condition')$message}"
    ))
  }

  # Additional brms-specific validations for time series
  if (trend_model != "None") {
    # Ensure regular time intervals for State-Space models
    if (trend_model %in% c("RW", "AR", "VAR")) {
      check_regular_time_intervals(data, silent = silent)
    }
  }

  if (silent < 2) {
    message("Time series structure validated for trend model: ", trend_model)
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

# Data Extraction and Processing Utilities
# ========================================

#' Extract Time Data Components
#'
#' @description
#' Extracts time-related data components from a dataset for Stan models.
#'
#' @param data Data frame containing time series data
#' @return List with time components or empty list if no time data found
#' @noRd
extract_time_data <- function(data) {
  checkmate::assert_data_frame(data)

  if (!"time" %in% names(data)) {
    return(list())
  }

  time_vals <- data$time
  unique_times <- sort(unique(time_vals))

  return(list(
    time = unique_times,
    n_time = length(unique_times)
  ))
}

#' Extract Series Data Components
#'
#' @description
#' Extracts series-related data components from a dataset for Stan models.
#'
#' @param data Data frame containing series data
#' @return List with series components or empty list if no series data found
#' @noRd
extract_series_data <- function(data) {
  checkmate::assert_data_frame(data)

  if (!"series" %in% names(data)) {
    return(list())
  }

  series_vals <- data$series
  unique_series <- sort(unique(series_vals))

  return(list(
    series = unique_series,
    n_series = length(unique_series)
  ))
}

#' Merge Stan Data from Multiple Sources
#'
#' @description
#' Merges Stan data lists from different sources (e.g., observation and trend models).
#' Handles conflicts by preferring trend-specific data elements.
#'
#' @param base_data List containing base Stan data
#' @param trend_data List containing trend-specific Stan data
#' @return List of merged Stan data
#' @noRd
merge_stan_data <- function(base_data, trend_data) {
  checkmate::assert_list(base_data)
  checkmate::assert_list(trend_data)

  # Start with base data
  merged_data <- base_data

  # Flatten trend_data if it has nested structure
  if (any(sapply(trend_data, is.list))) {
    flattened_trend <- list()
    for (name in names(trend_data)) {
      if (is.list(trend_data[[name]]) && !is.data.frame(trend_data[[name]])) {
        # Add nested elements with prefix
        nested_elements <- trend_data[[name]]
        for (nested_name in names(nested_elements)) {
          flattened_trend[[nested_name]] <- nested_elements[[nested_name]]
        }
      } else {
        flattened_trend[[name]] <- trend_data[[name]]
      }
    }
    trend_data <- flattened_trend
  }

  # Identify conflicting names
  conflicting_names <- intersect(names(base_data), names(trend_data))

  # Warn about conflicts but prefer trend data
  if (length(conflicting_names) > 0) {
    insight::format_warning(
      "Data conflict detected for: {.field {conflicting_names}}",
      "Using trend data values."
    )
  }

  # Add all trend data (overwriting conflicts)
  for (name in names(trend_data)) {
    merged_data[[name]] <- trend_data[[name]]
  }

  return(merged_data)
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

# Stan Code Processing Utilities
# ==============================

#' Extract Code Block from Stan Code
#'
#' @description
#' Extracts a specific block (e.g., "data", "parameters", "model") from Stan code.
#'
#' @param code_lines Character vector of Stan code lines
#' @param block_name Character string name of block to extract
#' @return Character string of block contents or NULL if not found
#' @noRd
extract_code_block <- function(code_lines, block_name) {
  checkmate::assert_character(code_lines)
  checkmate::assert_string(block_name)

  # Find block start line
  block_pattern <- paste0("^\\s*", block_name, "\\s*\\{\\s*$")
  start_line <- which(grepl(block_pattern, code_lines))

  if (length(start_line) == 0) {
    return(NULL)
  }

  start_line <- start_line[1]  # Take first match

  # Find matching closing brace
  end_line <- find_matching_brace(code_lines, start_line)

  if (length(end_line) == 0) {
    return(NULL)
  }

  # Extract block content (excluding the block declaration and closing brace)
  if (end_line > start_line + 1) {
    block_content <- code_lines[(start_line + 1):(end_line - 1)]
    return(paste(block_content, collapse = "\n"))
  } else {
    return("")  # Empty block
  }
}

#' Find Matching Brace
#'
#' @description
#' Finds the line number of the closing brace that matches an opening brace.
#'
#' @param code_lines Character vector of Stan code lines
#' @param start_line Integer line number containing opening brace
#' @return Integer line number of matching closing brace or integer(0) if not found
#' @noRd
find_matching_brace <- function(code_lines, start_line) {
  checkmate::assert_character(code_lines)
  checkmate::assert_integerish(start_line)

  # Handle empty input
  if (length(code_lines) == 0 || length(start_line) == 0) {
    return(integer(0))
  }

  if (start_line < 1 || start_line > length(code_lines)) {
    return(integer(0))
  }

  # Count braces starting from start_line
  depth <- 0
  found_opening <- FALSE

  for (i in start_line:length(code_lines)) {
    line <- code_lines[i]

    # Count opening and closing braces in this line
    opening_braces <- lengths(regmatches(line, gregexpr("\\{", line)))
    closing_braces <- lengths(regmatches(line, gregexpr("\\}", line)))

    depth <- depth + opening_braces - closing_braces

    # Mark that we've seen at least one opening brace
    if (opening_braces > 0) {
      found_opening <- TRUE
    }

    # If depth returns to 0 after seeing an opening brace, we found the match
    if (found_opening && depth == 0) {
      return(i)
    }

    # If depth goes negative, braces are unbalanced
    if (depth < 0) {
      return(integer(0))
    }
  }

  # No matching brace found
  return(integer(0))
}

# Feature Detection Utilities
# ===========================

#' Check if Stanvars Have Time Component
#'
#' @description
#' Checks if stanvars contain time-related components.
#'
#' @param stanvars List of stanvar objects
#' @return Logical indicating presence of time components
#' @noRd
has_time_component <- function(stanvars) {
  checkmate::assert_list(stanvars)

  # Look for time-related patterns in stanvar code
  time_patterns <- c("n_time", "time_vals", "time_data", "\\btime\\b")

  for (stanvar in stanvars) {
    if (is_valid_stanvar(stanvar)) {
      scode <- stanvar$scode

      for (pattern in time_patterns) {
        if (grepl(pattern, scode)) {
          return(TRUE)
        }
      }
    }
  }

  return(FALSE)
}

#' Check if Stanvars Have Series Component
#'
#' @description
#' Checks if stanvars contain series-related components.
#'
#' @param stanvars List of stanvar objects
#' @return Logical indicating presence of series components
#' @noRd
has_series_component <- function(stanvars) {
  checkmate::assert_list(stanvars)

  # Look for series-related patterns in stanvar code
  series_patterns <- c("n_series", "series_data", "\\bseries\\b")

  for (stanvar in stanvars) {
    if (is_valid_stanvar(stanvar)) {
      scode <- stanvar$scode

      for (pattern in series_patterns) {
        if (grepl(pattern, scode)) {
          return(TRUE)
        }
      }
    }
  }

  return(FALSE)
}

#' Check if Stanvars Have Correlation Component
#'
#' @description
#' Checks if stanvars contain correlation/covariance components.
#'
#' @param stanvars List of stanvar objects
#' @return Logical indicating presence of correlation components
#' @noRd
has_correlation_component <- function(stanvars) {
  checkmate::assert_list(stanvars)

  # Look for correlation-related patterns in stanvar code
  corr_patterns <- c("cov_matrix", "corr_matrix", "Sigma", "correlation", "covariance")

  for (stanvar in stanvars) {
    if (is_valid_stanvar(stanvar)) {
      scode <- stanvar$scode

      for (pattern in corr_patterns) {
        if (grepl(pattern, scode)) {
          return(TRUE)
        }
      }
    }
  }

  return(FALSE)
}

# Integration Support Utilities
# =============================

#' Prepare Stanvars for brms Integration
#'
#' @description
#' Prepares stanvars for integration with brms by filtering invalid ones
#' and ensuring proper format.
#'
#' @param stanvars List of stanvar objects
#' @return List of valid stanvars ready for brms
#' @noRd
prepare_stanvars_for_brms <- function(stanvars) {
  checkmate::assert_list(stanvars)

  if (length(stanvars) == 0) {
    return(list())
  }

  # Filter to valid stanvars only
  valid_stanvars <- list()

  for (name in names(stanvars)) {
    stanvar <- stanvars[[name]]

    # brms stanvars are containers - validate the actual stanvar element
    stanvar_element <- if (inherits(stanvar, "stanvars") && length(stanvar) == 1) {
      stanvar[[1]]
    } else {
      stanvar
    }

    if (is_valid_stanvar(stanvar_element)) {
      valid_stanvars[[name]] <- stanvar
    } else {
      insight::format_warning(
        "Skipping invalid stanvar: {.field {name}}",
        "Stanvar must have valid 'scode' and 'block' components."
      )
    }
  }

  return(valid_stanvars)
}

# Missing Integration Functions
# ============================

#' Generate Trend Injection Stanvars
#'
#' @description
#' Central dispatcher function that routes trend specifications to appropriate
#' trend-specific stanvar generators. This function bridges the gap between
#' the Stan assembly system and individual trend generators.
#'
#' @param trend_spec List containing trend specification with trend_type or trend_model
#' @param data_info List containing data dimensions and structure information
#' @return List of stanvars for trend implementation, or empty list for None trends
#' @noRd
generate_trend_injection_stanvars <- function(trend_spec, data_info) {
  checkmate::assert_list(trend_spec, names = "named")
  checkmate::assert_list(data_info, names = "named")

  # Extract trend type from spec (handle both trend_type and trend_model for compatibility)
  trend_type <- trend_spec$trend_type %||% trend_spec$trend_model
  
  if (is.null(trend_type)) {
    insight::format_warning(
      "No trend type specified in trend_spec.",
      "Returning empty stanvars list."
    )
    return(list())
  }
  
  # Handle None trend case
  if (trend_type == "None") {
    return(list())
  }
  
  # Construct generator function name following existing naming convention
  generator_name <- paste0("generate_", tolower(trend_type), "_trend_stanvars")
  
  # Check if generator function exists
  if (!exists(generator_name, mode = "function", envir = asNamespace("mvgam"))) {
    available_generators <- ls(
      pattern = "generate_.*_trend_stanvars", 
      envir = asNamespace("mvgam")
    )
    
    stop(insight::format_error(
      paste("Trend generator function", generator_name, "not found."),
      paste("Available generators:", paste(available_generators, collapse = ", ")),
      paste("Trend type specified:", trend_type)
    ))
  }
  
  # Dispatch to appropriate generator
  do.call(generator_name, list(trend_spec, data_info))
}

#' Extract Trend Stanvars from Setup
#'
#' @description
#' Extracts trend stanvars from brms setup and generates additional trend-specific stanvars.
#' Integrates with the trend injection system.
#'
#' @param trend_setup List containing trend model setup
#' @param trend_spec List containing trend specification
#' @return List of trend stanvars
#' @noRd
extract_trend_stanvars_from_setup <- function(trend_setup, trend_spec) {
  checkmate::assert_list(trend_setup, names = "named")
  checkmate::assert_list(trend_spec, names = "named")

  # Extract base stanvars from trend setup
  base_stanvars <- trend_setup$stanvars %||% list()

  # Generate trend-specific stanvars if trend spec is provided
  # Handle both trend_type and trend_model for compatibility
  trend_type <- trend_spec$trend_type %||% trend_spec$trend_model
  trend_stanvars <- if (!is.null(trend_spec) && !is.null(trend_type)) {
    # Prepare data info for trend stanvar generation
    data_info <- list(
      n_series = trend_spec$n_series %||% 1,
      n_lv = trend_spec$n_lv,
      n_time = trend_spec$n_time,
      n_groups = trend_spec$n_groups,
      n_subgroups = trend_spec$n_subgroups
    )

    # Use the existing trend injection system
    generate_trend_injection_stanvars(trend_spec, data_info)
  } else {
    list()
  }

  # Combine base and trend-specific stanvars
  combined_stanvars <- c(base_stanvars, trend_stanvars)

  return(combined_stanvars)
}

#' Inject Trend into Linear Predictor
#'
#' @description
#' Injects trend effects into Stan linear predictor by modifying the transformed parameters
#' or model block to add trend components to mu.
#'
#' @param base_stancode Character string of base Stan code
#' @param trend_stanvars List of trend stanvars
#' @param trend_spec List containing trend specification
#' @return Modified Stan code with trend injection
#' @noRd
inject_trend_into_linear_predictor <- function(base_stancode, trend_stanvars, trend_spec) {
  checkmate::assert_string(base_stancode)
  checkmate::assert_list(trend_stanvars)
  checkmate::assert_list(trend_spec)

  # If no trends, return unchanged
  if (length(trend_stanvars) == 0 || is.null(trend_spec$trend_model)) {
    return(base_stancode)
  }

  # Split code into lines for easier manipulation
  code_lines <- strsplit(base_stancode, "\n", fixed = TRUE)[[1]]

  # Find the transformed parameters block or create one if it doesn't exist
  tp_block_start <- which(grepl("^\\s*transformed\\s+parameters\\s*\\{", code_lines))

  if (length(tp_block_start) == 0) {
    # No transformed parameters block exists, create one before model block
    model_block_start <- which(grepl("^\\s*model\\s*\\{", code_lines))

    if (length(model_block_start) == 0) {
      stop(insight::format_error(
        "Cannot find model block in Stan code for trend injection.",
        "Stan code structure is invalid."
      ))
    }

    # Insert transformed parameters block before model block
    new_tp_block <- c(
      "transformed parameters {",
      "  // Combined linear predictor with trend effects",
      "  vector[N] mu_combined = mu;",
      "",
      "  // Add trend effects if available",
      "  if (size(trend) > 0) {",
      "    mu_combined += trend[obs_ind];",
      "  }",
      "}"
    )

    # Insert the new block
    code_lines <- c(
      code_lines[1:(model_block_start[1] - 1)],
      new_tp_block,
      "",
      code_lines[model_block_start[1]:length(code_lines)]
    )

  } else {
    # Transformed parameters block exists, modify it
    tp_start <- tp_block_start[1]
    tp_end <- find_matching_brace(code_lines, tp_start)

    if (length(tp_end) == 0) {
      stop(insight::format_error(
        "Cannot find end of transformed parameters block.",
        "Stan code structure may be invalid."
      ))
    }

    # Find mu declaration and modify it
    mu_lines <- which(grepl("vector\\[.*\\]\\s+mu\\s*=", code_lines[(tp_start+1):(tp_end-1)]))

    if (length(mu_lines) > 0) {
      # mu is declared in transformed parameters, modify it
      mu_line_idx <- tp_start + mu_lines[1]

      # Insert trend addition after mu declaration
      trend_addition <- c(
        "",
        "  // Add trend effects",
        "  if (size(trend) > 0) {",
        "    mu += trend[obs_ind];",
        "  }"
      )

      code_lines <- c(
        code_lines[1:mu_line_idx],
        trend_addition,
        code_lines[(mu_line_idx+1):length(code_lines)]
      )

    } else {
      # mu not found in transformed parameters, add combined predictor
      trend_addition <- c(
        "  // Combined linear predictor with trend effects",
        "  vector[N] mu_combined = mu;",
        "  if (size(trend) > 0) {",
        "    mu_combined += trend[obs_ind];",
        "  }"
      )

      # Insert before closing brace of transformed parameters
      code_lines <- c(
        code_lines[1:(tp_end-1)],
        trend_addition,
        code_lines[tp_end:length(code_lines)]
      )

      # Update likelihood to use mu_combined instead of mu
      code_lines <- gsub("\\bmu\\b", "mu_combined", code_lines)
    }
  }

  # Rejoin the code
  modified_code <- paste(code_lines, collapse = "\n")

  return(modified_code)
}

# Main Assembly Functions
# =======================

#' Assemble Complete mvgam Stan Code
#'
#' @description
#' Main orchestrator function that assembles complete Stan code for mvgam models
#' by combining observation and trend components.
#'
#' @param obs_formula Formula for observation model
#' @param trend_stanvars List of trend stanvars
#' @param data Data for the model
#' @param family Family specification
#' @param backend Stan backend ("rstan" or "cmdstanr")
#' @param validate Whether to validate final Stan code
#' @return Character string of complete Stan code
#' @noRd
assemble_mvgam_stan_code <- function(obs_formula, trend_stanvars = NULL, data,
                                    family = gaussian(), backend = "rstan",
                                    validate = TRUE) {
  checkmate::assert_class(obs_formula, "formula")
  checkmate::assert_list(trend_stanvars, null.ok = TRUE)
  checkmate::assert_data_frame(data)

  # Generate base brms Stan code
  base_stancode <- generate_base_brms_stancode(
    formula = obs_formula,
    data = data,
    family = family,
    stanvars = trend_stanvars,
    backend = backend
  )

  # If no trend stanvars, return base code
  if (is.null(trend_stanvars) || length(trend_stanvars) == 0) {
    if (validate) {
      base_stancode <- validate_stan_code(base_stancode, backend = backend)
    }
    return(base_stancode)
  }

  # Extract trend specification from stanvars (if available)
  trend_spec <- extract_trend_spec_from_stanvars(trend_stanvars)

  # Inject trend effects into linear predictor
  final_stancode <- inject_trend_into_linear_predictor(
    base_stancode,
    trend_stanvars,
    trend_spec
  )

  # Validate final code if requested
  if (validate) {
    final_stancode <- validate_stan_code(final_stancode, backend = backend)
  }

  return(final_stancode)
}

#' Assemble Complete mvgam Stan Data
#'
#' @description
#' Assembles complete Stan data for mvgam models by combining observation
#' and trend data components.
#'
#' @param obs_formula Formula for observation model
#' @param trend_stanvars List of trend stanvars
#' @param data Data for the model
#' @param family Family specification
#' @return List of complete Stan data
#' @noRd
assemble_mvgam_stan_data <- function(obs_formula, trend_stanvars = NULL, data,
                                    family = gaussian()) {
  checkmate::assert_class(obs_formula, "formula")
  checkmate::assert_list(trend_stanvars, null.ok = TRUE)
  checkmate::assert_data_frame(data)

  # Generate base brms Stan data
  base_standata <- generate_base_brms_standata(
    formula = obs_formula,
    data = data,
    family = family,
    stanvars = trend_stanvars
  )

  # If no trend stanvars, return base data
  if (is.null(trend_stanvars) || length(trend_stanvars) == 0) {
    return(base_standata)
  }

  # Extract trend-specific data from stanvars
  trend_data <- extract_trend_data_from_stanvars(trend_stanvars, data)

  # Merge base and trend data
  final_standata <- merge_stan_data(base_standata, trend_data)

  # Validate final data structure
  validate_stan_data_structure(final_standata)

  return(final_standata)
}

#' Generate Base brms Stan Code
#'
#' @description
#' Generates base Stan code using brms with optional stanvars injection.
#'
#' @param formula Formula for the model
#' @param data Data for the model
#' @param family Family specification
#' @param stanvars Optional stanvars to inject
#' @param backend Stan backend
#' @return Character string of Stan code
#' @noRd
generate_base_brms_stancode <- function(formula, data, family = gaussian(),
                                       stanvars = NULL, backend = "rstan") {
  checkmate::assert_class(formula, "formula")
  checkmate::assert_data_frame(data)

  # Use brms to generate Stan code
  stancode <- try({
    brms::make_stancode(
      formula = formula,
      data = data,
      family = family,
      stanvars = stanvars,
      backend = backend
    )
  }, silent = TRUE)

  if (inherits(stancode, "try-error")) {
    stop(insight::format_error(
      "Failed to generate base brms Stan code.",
      "Check formula, data, and family specifications.",
      "brms error: {attr(stancode, 'condition')$message}"
    ))
  }

  return(stancode)
}

#' Generate Base brms Stan Data
#'
#' @description
#' Generates base Stan data using brms with optional stanvars injection.
#'
#' @param formula Formula for the model
#' @param data Data for the model
#' @param family Family specification
#' @param stanvars Optional stanvars to inject
#' @return List of Stan data
#' @noRd
generate_base_brms_standata <- function(formula, data, family = gaussian(),
                                       stanvars = NULL) {
  checkmate::assert_class(formula, "formula")
  checkmate::assert_data_frame(data)

  # Use brms to generate Stan data
  standata <- try({
    brms::make_standata(
      formula = formula,
      data = data,
      family = family,
      stanvars = stanvars
    )
  }, silent = TRUE)

  if (inherits(standata, "try-error")) {
    stop(insight::format_error(
      "Failed to generate base brms Stan data.",
      "Check formula, data, and family specifications.",
      "brms error: {attr(standata, 'condition')$message}"
    ))
  }

  return(standata)
}

# Helper Functions for Stan Assembly
# ==================================

#' Extract Trend Specification from Stanvars
#'
#' @description
#' Extracts trend specification information from stanvars metadata.
#'
#' @param trend_stanvars List of trend stanvars
#' @return List with trend specification or NULL
#' @noRd
extract_trend_spec_from_stanvars <- function(trend_stanvars) {
  if (is.null(trend_stanvars) || length(trend_stanvars) == 0) {
    return(NULL)
  }

  # Look for trend specification in stanvar metadata
  for (stanvar in trend_stanvars) {
    if (is_valid_stanvar(stanvar)) {
      # Check if stanvar name indicates trend type
      if (grepl("^(rw|ar|var|car|pw|zmvn)_", stanvar$name)) {
        trend_model <- gsub("^([a-zA-Z]+)_.*", "\\1", stanvar$name)
        return(list(
          trend_model = toupper(trend_model),
          n_series = 1,  # Default values
          n_lv = 1
        ))
      }
    }
  }

  # Fallback: return minimal trend specification
  return(list(
    trend_model = "RW",
    n_series = 1,
    n_lv = 1
  ))
}

#' Extract Trend Data from Stanvars
#'
#' @description
#' Extracts trend-specific data components from stanvars and original data.
#'
#' @param trend_stanvars List of trend stanvars
#' @param data Original data frame
#' @return List of trend data components
#' @noRd
extract_trend_data_from_stanvars <- function(trend_stanvars, data) {
  if (is.null(trend_stanvars) || length(trend_stanvars) == 0) {
    return(list())
  }

  trend_data <- list()

  # Extract time-related data if available
  time_data <- extract_time_data(data)
  if (length(time_data) > 0) {
    trend_data <- c(trend_data, time_data)
  }

  # Extract series-related data if available
  series_data <- extract_series_data(data)
  if (length(series_data) > 0) {
    trend_data <- c(trend_data, series_data)
  }

  # Look for additional data in stanvars metadata
  for (stanvar in trend_stanvars) {
    if (is_valid_stanvar(stanvar) && !is.null(stanvar$sdata)) {
      # If stanvar has data component, include it
      stanvar_name <- stanvar$name
      trend_data[[stanvar_name]] <- stanvar$sdata
    }
  }

  return(trend_data)
}

# Stan Component Combination Utilities
# ====================================

#' Combine Stan Components
#'
#' @description
#' Low-level utility function that combines observation Stan code, observation
#' data, and trend stanvars into a complete Stan model specification.
#'
#' This function merges stanvars into the observation code using brms stanvar
#' injection system and combines the data components.
#'
#' @param obs_stancode Character string containing observation model Stan code
#' @param obs_standata List containing observation model data
#' @param trend_stanvars List of stanvar objects for trend models
#' @return List with combined stancode, standata, and has_trends flag
#' @noRd
combine_stan_components <- function(obs_stancode, obs_standata, trend_stanvars) {
  checkmate::assert_string(obs_stancode, min.chars = 1)
  checkmate::assert_list(obs_standata, names = "named")
  checkmate::assert_list(trend_stanvars)

  # Track whether trends were actually added
  has_trends <- length(trend_stanvars) > 0

  # Start with observation code and data
  combined_code <- obs_stancode
  combined_data <- obs_standata

  # If no trend stanvars, return observation model as-is
  if (!has_trends) {
    return(list(
      stancode = combined_code,
      standata = combined_data,
      has_trends = FALSE
    ))
  }

  # Process each stanvar and inject into code
  for (stanvar_name in names(trend_stanvars)) {
    stanvar <- trend_stanvars[[stanvar_name]]

    if (is_valid_stanvar(stanvar)) {
      # Inject stanvar code into appropriate Stan block
      combined_code <- inject_stanvar_code(combined_code, stanvar)

      # Add stanvar data if present
      if (!is.null(stanvar$sdata)) {
        combined_data <- merge_stan_data(combined_data, stanvar$sdata)
      }
    }
  }

  return(list(
    stancode = combined_code,
    standata = combined_data,
    has_trends = has_trends
  ))
}

#' Inject Stanvar Code into Stan Model
#'
#' @description
#' Injects a stanvar's code into the appropriate Stan block of existing code.
#'
#' @param stan_code Character string containing existing Stan code
#' @param stanvar Stanvar object to inject
#' @return Updated Stan code with stanvar injected
#' @noRd
inject_stanvar_code <- function(stan_code, stanvar) {
  checkmate::assert_string(stan_code)

  if (!is_valid_stanvar(stanvar)) {
    return(stan_code)
  }

  block <- stanvar$block %||% "parameters"
  scode <- stanvar$scode %||% ""

  if (nchar(scode) == 0) {
    return(stan_code)
  }

  # Find the target block in Stan code
  block_pattern <- paste0("\\b", block, "\\s*\\{")

  if (!grepl(block_pattern, stan_code)) {
    # If block doesn't exist, append at end
    stan_code <- paste0(stan_code, "\n", block, " {\n", scode, "\n}\n")
  } else {
    # Find insertion point within existing block
    # This is a simplified implementation - more sophisticated block parsing
    # would be needed for production use

    # For now, append to end of existing block content
    # Find the closing brace of the target block
    block_start <- regexpr(block_pattern, stan_code)
    if (block_start > 0) {
      # Simple approach: add content before the last closing brace
      # This assumes the last } belongs to our target block (simplified)
      last_brace <- tail(gregexpr("\\}", stan_code)[[1]], 1)
      if (last_brace > 0) {
        before_brace <- substr(stan_code, 1, last_brace - 1)
        after_brace <- substr(stan_code, last_brace, nchar(stan_code))
        stan_code <- paste0(before_brace, "\n", scode, "\n", after_brace)
      }
    }
  }

  return(stan_code)
}

# =============================================================================
# SECTION 2: TREND-SPECIFIC STAN CODE GENERATORS
# =============================================================================
# WHY: Each trend type requires specialized Stan code with unique parameters,
# priors, and computational patterns. Generators provide modular, extensible
# Stan code creation that integrates seamlessly with the registry system
# and maintains consistency across trend types.

#' @noRd
compute_trend_data_info <- function(data_specs) {
  list(
    n_obs = data_specs$n_obs %||% 100,
    n_series = data_specs$n_series %||% 1,
    series_var = data_specs$series_var %||% "series"
  )
}

# Shared Utility Functions for Factor Model Consistency
# These functions ensure all factor-compatible trends use identical patterns

#' Combine Stanvars Robustly
#'
#' Single function to handle all stanvar combination patterns used in mvgam.
#' Handles NULL values, individual stanvars, stanvars collections, lists, and mixed inputs.
#' Preserves proper class structure by using brms c() method exclusively.
#'
#' @param ... Stanvar components to combine (can be NULL, stanvars, lists, or mixed)
#' @return Combined stanvars collection with proper class structure, or NULL if all inputs are NULL
#' @noRd
combine_stanvars <- function(...) {
  components <- list(...)

  # Flatten nested structures and filter valid components
  valid_components <- list()
  for (component in components) {
    if (!is.null(component)) {
      if (inherits(component, "stanvars")) {
        # Direct stanvars object
        valid_components <- append(valid_components, list(component))
      } else if (is.list(component)) {
        # Handle lists that might contain stanvars (from some generators)
        for (item in component) {
          if (!is.null(item) && inherits(item, "stanvars")) {
            valid_components <- append(valid_components, list(item))
          }
        }
      } else {
        stop("Invalid component type: ", class(component), ". Expected stanvars, list, or NULL.")
      }
    }
  }

  # Return NULL if no valid components
  if (length(valid_components) == 0) {
    return(NULL)
  }

  # Start with first component and combine the rest using brms c() method
  result <- valid_components[[1]]
  if (length(valid_components) > 1) {
    for (i in 2:length(valid_components)) {
      result <- c(result, valid_components[[i]])
    }
  }

  return(result)
}

# =============================================================================
# SHARED GAUSSIAN INNOVATION SYSTEM
# =============================================================================
# WHY: Most trend types (RW, AR, VAR, CAR, ZMVN) use Gaussian innovations with
# common parameters (sigma_trend, raw_innovations, correlation matrices). This
# system provides unified generation of these shared stanvars to avoid duplication
# across trend-specific generators and ensure consistent naming/structure.
# MA transformations are handled by individual trend generators sequentially.

#' Generate Shared Gaussian Innovation Parameters
#'
#' Creates stanvar objects for parameters common to Gaussian innovation trends.
#' Generates raw innovations only - MA transformations handled by trend generators.
#'
#' @param n_lv Number of latent variables
#' @param n_series Number of time series
#' @param cor Logical, whether to include correlation parameters
#' @param factor_model Logical, whether this is a factor model
#' @param hierarchical_info List with hierarchical structure info (NULL for simple models)
#' @return List of stanvar objects for shared parameters
#' @noRd
generate_shared_innovation_stanvars <- function(n_lv, n_series, cor = FALSE,
                                               factor_model = FALSE,
                                               hierarchical_info = NULL) {

  # Determine effective dimension for innovations
  effective_dim <- if (factor_model) n_lv else n_series

  # Check for hierarchical structure
  is_hierarchical <- !is.null(hierarchical_info) && hierarchical_info$has_groups

  # Create individual stanvar components
  stanvar_components <- list()

  if (is_hierarchical) {
    # Hierarchical case: leverage existing hierarchical correlation functions
    n_groups <- hierarchical_info$n_groups
    n_subgroups <- effective_dim

    # Add existing hierarchical correlation infrastructure
    hierarchical_functions <- generate_hierarchical_functions_injectors()
    hierarchical_params <- generate_hierarchical_correlation_parameter_injectors(n_groups, n_subgroups)
    hierarchical_priors <- generate_hierarchical_correlation_model_injectors(n_groups)

    # Add to component list
    stanvar_components <- append(stanvar_components,
                                list(hierarchical_functions, hierarchical_params, hierarchical_priors))

    # Add sigma_trend parameter for hierarchical case
    sigma_stanvar <- brms::stanvar(
      name = "sigma_trend",
      scode = paste0("vector<lower=0>[", effective_dim, "] sigma_trend;"),
      block = "parameters"
    )
    stanvar_components <- append(stanvar_components, list(sigma_stanvar))

  } else {
    # Simple case: non-hierarchical innovations

    # 1. sigma_trend - innovation standard deviations
    sigma_code <- if (effective_dim == 1) {
      "vector<lower=0>[1] sigma_trend;"
    } else {
      paste0("vector<lower=0>[", effective_dim, "] sigma_trend;")
    }

    sigma_stanvar <- brms::stanvar(
      name = "sigma_trend",
      scode = sigma_code,
      block = "parameters"
    )
    stanvar_components <- append(stanvar_components, list(sigma_stanvar))

    # 2. Correlation parameters (only if cor = TRUE and multivariate)
    if (cor && effective_dim > 1) {
      # Cholesky factor for correlation matrix
      l_omega_stanvar <- brms::stanvar(
        name = "L_Omega_trend",
        scode = paste0("cholesky_factor_corr[", effective_dim, "] L_Omega_trend;"),
        block = "parameters"
      )
      stanvar_components <- append(stanvar_components, list(l_omega_stanvar))

      # Derived covariance matrix in transformed parameters
      sigma_matrix_code <- paste0(
        "matrix[", effective_dim, ", ", effective_dim, "] Sigma_trend = ",
        "diag_pre_multiply(sigma_trend, L_Omega_trend);"
      )

      sigma_matrix_stanvar <- brms::stanvar(
        name = "Sigma_trend",
        scode = sigma_matrix_code,
        block = "tparameters"
      )
      stanvar_components <- append(stanvar_components, list(sigma_matrix_stanvar))
    }
  }

  # 4. Raw innovations parameter (Stan will sample these with std_normal prior)
  raw_innovations_stanvar <- brms::stanvar(
    name = "raw_innovations",
    scode = paste0("matrix[n, ", effective_dim, "] raw_innovations;"),
    block = "parameters"
  )
  stanvar_components <- append(stanvar_components, list(raw_innovations_stanvar))

  # 5. Final innovations in transformed parameters (after correlation/MA transformation)
  if (is_hierarchical) {
    # Hierarchical case: innovations depend on group structure
    final_innovations_code <- paste0("
    // Final innovations after applying hierarchical correlations
    matrix[n, ", effective_dim, "] LV_innovations;

    // Apply group-specific correlations to raw innovations
    for (g in 1:n_groups) {
      // Derived group-specific correlation matrices (using existing combine_cholesky)
      array[n_groups] cholesky_factor_corr[", effective_dim, "] L_Omega_group;
      for (g_idx in 1:n_groups) {
        L_Omega_group[g_idx] = combine_cholesky(L_Omega_global, L_deviation_group[g_idx], alpha_cor);
      }

      // Transform raw innovations using group correlations
      matrix[", effective_dim, ", ", effective_dim, "] Sigma_group = diag_pre_multiply(sigma_trend, L_Omega_group[g]);
      // Apply to group time points (individual generators will specify the indexing)
    }")
  } else if (cor && effective_dim > 1) {
    # Simple correlated case
    final_innovations_code <- paste0("
    // Final innovations after applying correlations
    matrix[n, ", effective_dim, "] LV_innovations;

    // Apply correlation transformation to raw innovations
    for (i in 1:n) {
      LV_innovations[i, :] = (Sigma_trend * raw_innovations[i, :]')';
    }")
  } else {
    # Uncorrelated case
    final_innovations_code <- paste0("
    // Final innovations (uncorrelated case)
    matrix[n, ", effective_dim, "] LV_innovations;

    // Apply scaling to raw innovations
    for (i in 1:n) {
      for (j in 1:", effective_dim, ") {
        LV_innovations[i, j] = sigma_trend[j] * raw_innovations[i, j];
      }
    }")
  }

  final_innovations_stanvar <- brms::stanvar(
    name = "final_innovations",
    scode = final_innovations_code,
    block = "tparameters"
  )
  stanvar_components <- append(stanvar_components, list(final_innovations_stanvar))

  # Combine all components using do.call to handle the list properly
  return(do.call(combine_stanvars, stanvar_components))
}

#' Generate Standard Priors for Gaussian Innovations
#'
#' Creates standard priors for shared Gaussian innovation parameters.
#'
#' @param effective_dim Effective dimension (n_lv for factor models, n_series otherwise)
#' @param cor Logical, whether correlation parameters exist
#' @param is_hierarchical Logical, whether using hierarchical structure
#' @return Stanvar object with prior code
#' @noRd
generate_innovation_priors <- function(effective_dim, cor = FALSE, is_hierarchical = FALSE) {

  if (is_hierarchical) {
    # Hierarchical priors are handled by generate_hierarchical_correlation_model_injectors
    prior_code <- c(
      "// Raw innovations prior",
      "to_vector(raw_innovations) ~ std_normal();"
    )
  } else {
    # Simple case priors
    prior_code <- c(
      "// Shared Gaussian innovation priors",
      "sigma_trend ~ exponential(1);"
    )

    if (cor && effective_dim > 1) {
      prior_code <- c(prior_code,
        "L_Omega_trend ~ lkj_corr_cholesky(2);"
      )
    }

    prior_code <- c(prior_code,
      "to_vector(raw_innovations) ~ std_normal();"
    )
  }

  brms::stanvar(
    name = "innovation_priors",
    scode = paste(prior_code, collapse = "\n  "),
    block = "model"
  )
}

#' Extract Hierarchical Information from Data Specifications
#'
#' Processes data specifications to extract hierarchical grouping structure.
#'
#' @param data_info Data information list
#' @param trend_spec Trend specification list
#' @return List with hierarchical structure information
#' @noRd
extract_hierarchical_info <- function(data_info, trend_spec) {

  has_groups <- !is.null(trend_spec$gr) && trend_spec$gr != 'NA'

  if (!has_groups) {
    return(NULL)
  }

  list(
    has_groups = TRUE,
    n_groups = data_info$n_groups %||% 1,
    n_subgroups = data_info$n_subgroups %||% data_info$n_lv %||% data_info$n_series,
    gr_var = trend_spec$gr,
    subgr_var = trend_spec$subgr %||% 'NA'
  )
}

#' Generate Data Block Injections for Matrix Z
#'
#' @param is_factor_model Logical indicating if this is a factor model
#' @param n_lv Number of latent variables
#' @param n_series Number of observed series
#' @return List of data block stanvars
#' @noRd
generate_matrix_z_data_injectors <- function(is_factor_model, n_lv, n_series) {
  # Create individual stanvar objects
  n_lv_stanvar <- brms::stanvar(
    x = n_lv,
    name = "n_lv",
    scode = "int<lower=1> n_lv;",
    block = "data"
  )

  n_series_stanvar <- brms::stanvar(
    x = n_series,
    name = "n_series",
    scode = "int<lower=1> n_series;",
    block = "data"
  )

  # Combine using brms c() method to create proper stanvars collection
  return(c(n_lv_stanvar, n_series_stanvar))
}

#' Generate Parameter Block Injections for Matrix Z
#'
#' @param is_factor_model Logical indicating if this is a factor model
#' @param n_lv Number of latent variables
#' @param n_series Number of observed series
#' @return List of parameter block stanvars
#' @noRd
generate_matrix_z_parameter_injectors <- function(is_factor_model, n_lv, n_series) {
  if (is_factor_model) {
    # Factor model: estimate Z in parameters for dimensionality reduction
    z_matrix_stanvar <- brms::stanvar(
      name = "Z",
      scode = glue::glue("matrix[n_series, n_lv] Z;"),
      block = "parameters"
    )
    return(z_matrix_stanvar)
  } else {
    # Return NULL for empty case
    return(NULL)
  }
}

#' Generate Transformed Data Block Injections for Matrix Z
#'
#' @param is_factor_model Logical indicating if this is a factor model
#' @param n_lv Number of latent variables
#' @param n_series Number of observed series
#' @return List of transformed data block stanvars
#' @noRd
generate_matrix_z_transformed_data_injectors <- function(is_factor_model, n_lv, n_series) {
  if (!is_factor_model) {
    # Non-factor model: diagonal Z in transformed data
    z_matrix_stanvar <- brms::stanvar(
      name = "Z",
      scode = glue::glue("matrix[n_series, n_lv] Z = diag_matrix(rep_vector(1.0, n_lv));"),
      block = "tdata"
    )
    return(z_matrix_stanvar)
  } else {
    # Return NULL for empty case
    return(NULL)
  }
}

#' Generate Matrix Z Stanvars (Consolidated Utility)
#'
#' Combines all matrix Z injection functions for factor/non-factor models.
#' This provides a single interface for matrix Z generation across all Stan blocks.
#'
#' @param is_factor_model Logical indicating if this is a factor model
#' @param n_lv Number of latent variables
#' @param n_series Number of observed series
#' @return List of stanvars for matrix Z across all required blocks
#' @noRd
generate_matrix_z_stanvars <- function(is_factor_model, n_lv, n_series) {
  # Get individual stanvar components
  data_stanvars <- generate_matrix_z_data_injectors(is_factor_model, n_lv, n_series)
  param_stanvars <- generate_matrix_z_parameter_injectors(is_factor_model, n_lv, n_series)
  tdata_stanvars <- generate_matrix_z_transformed_data_injectors(is_factor_model, n_lv, n_series)

  # Combine using brms's native c() method for stanvars
  # This should preserve proper class structure
  combined_stanvars <- data_stanvars
  if (!is.null(param_stanvars)) {
    combined_stanvars <- c(combined_stanvars, param_stanvars)
  }
  if (!is.null(tdata_stanvars)) {
    combined_stanvars <- c(combined_stanvars, tdata_stanvars)
  }

  return(combined_stanvars)
}

#' Generate Factor Model Priors (Consolidated Utility)
#'
#' Provides standardized priors for factor models with fixed variance=1 constraint.
#' Only generates priors when is_factor_model=TRUE.
#'
#' @param is_factor_model Logical indicating if this is a factor model
#' @param n_lv Number of latent variables
#' @return List of stanvars for factor model priors
#' @noRd
generate_factor_model_priors <- function(is_factor_model, n_lv) {
  if (!is_factor_model) {
    return(NULL)
  }

  # Factor model: fixed variance=1 for identifiability, priors for Z
  factor_lv_priors <- brms::stanvar(
    name = "factor_lv_priors",
    scode = "to_vector(LV_raw) ~ std_normal();",
    block = "model"
  )

  factor_z_priors <- brms::stanvar(
    name = "factor_z_priors",
    scode = "to_vector(Z) ~ normal(0, 1);",
    block = "model"
  )

  return(combine_stanvars(factor_lv_priors, factor_z_priors))
}

#' Generate Transformed Parameters Block Injections for Trend Computation
#'
#' WHY: All trends must use the same computation pattern:
#' trend[i,s] = dot_product(Z[s,:], LV[i,:]) + mu_trend[ytimes[i,s]]
#'
#' @param n_lv Number of latent variables
#' @param n_series Number of observed series
#' @return List of transformed parameters block stanvars
#' @noRd
generate_trend_computation_transformed_parameters_injectors <- function(n_lv, n_series) {
  # Create individual stanvar
  trend_computation_stanvar <- brms::stanvar(
    name = "trend",
    scode = glue::glue("
      // Derived latent trends using universal computation pattern
      matrix[n, n_series] trend;

      // Universal trend computation: state-space dynamics + linear predictors
      // dot_product captures dynamic component, mu_trend captures trend_formula
      for (i in 1:n) {{
        for (s in 1:n_series) {{
          trend[i, s] = dot_product(Z[s, :], LV[i, :]) + mu_trend[ytimes[i, s]];
        }}
      }}
    "),
    block = "tparameters"
  )

  return(trend_computation_stanvar)
}

#' Generate Model Block Injections for Factor Model Priors
#'
#' WHY: Factor models need fixed variance=1 constraints for identifiability
#' since the scale is captured by the loading matrix Z.
#'
#' @param is_factor_model Logical indicating if this is a factor model
#' @param n_lv Number of latent variables
#' @return List of model block stanvars
#' @noRd
generate_factor_model_model_injectors <- function(is_factor_model, n_lv) {
  if (is_factor_model) {
    # Factor model: fixed variance=1 for identifiability, priors for Z
    lv_priors_stanvar <- brms::stanvar(
      name = "factor_lv_priors",
      scode = "to_vector(LV_raw) ~ std_normal();",
      block = "model"
    )

    z_priors_stanvar <- brms::stanvar(
      name = "factor_z_priors",
      scode = "to_vector(Z) ~ normal(0, 1);",
      block = "model"
    )

    # Combine using brms c() method
    return(c(lv_priors_stanvar, z_priors_stanvar))
  } else {
    return(NULL)
  }
}

#' Generate Functions Block Injections for Hierarchical Correlations
#'
#' WHY: All trends (AR, VAR, CAR, ZMVN) need the same hierarchical correlation
#' machinery when groups are specified.
#'
#' @return List of functions block stanvars
#' @noRd
generate_hierarchical_functions_injectors <- function() {
  return(brms::stanvar(
    name = "combine_cholesky",
    scode = "
      /* Function to compute a partially pooled correlation matrix
       * Combines global correlation structure with group-specific deviations
       * alpha controls mixing: 1 = pure global, 0 = pure local
       */
      matrix combine_cholesky(matrix global_chol_cor, matrix local_chol_cor,
                              real alpha) {
        int dim = rows(local_chol_cor);
        matrix[dim, dim] global_cor = multiply_lower_tri_self_transpose(global_chol_cor);
        matrix[dim, dim] local_cor = multiply_lower_tri_self_transpose(local_chol_cor);
        matrix[dim, dim] combined_chol_cor;
        combined_chol_cor = cholesky_decompose(alpha * global_cor
                                               + (1 - alpha) * local_cor);
        return combined_chol_cor;
      }
    ",
    block = "functions"
  ))
}

#' Generate Parameters Block Injections for Hierarchical Correlations
#'
#' @param n_groups Number of groups for hierarchical modeling
#' @param n_subgroups Number of subgroups (typically n_lv)
#' @return List of parameters block stanvars
#' @noRd
generate_hierarchical_correlation_parameter_injectors <- function(n_groups, n_subgroups) {
  l_omega_global <- brms::stanvar(
    name = "L_Omega_global",
    scode = glue::glue("cholesky_factor_corr[{n_subgroups}] L_Omega_global;"),
    block = "parameters"
  )

  l_deviation_group <- brms::stanvar(
    name = "L_deviation_group",
    scode = glue::glue("array[{n_groups}] cholesky_factor_corr[{n_subgroups}] L_deviation_group;"),
    block = "parameters"
  )

  alpha_cor <- brms::stanvar(
    name = "alpha_cor",
    scode = "real<lower=0, upper=1> alpha_cor;",
    block = "parameters"
  )

  return(combine_stanvars(l_omega_global, l_deviation_group, alpha_cor))
}

#' Generate Model Block Injections for Hierarchical Correlation Priors
#'
#' @param n_groups Number of groups for hierarchical modeling
#' @return List of model block stanvars
#' @noRd
generate_hierarchical_correlation_model_injectors <- function(n_groups) {
  alpha_cor_prior <- brms::stanvar(
    name = "alpha_cor_prior",
    scode = "alpha_cor ~ beta(3, 2);",
    block = "model"
  )

  l_omega_global_prior <- brms::stanvar(
    name = "L_Omega_global_prior",
    scode = "L_Omega_global ~ lkj_corr_cholesky(1);",
    block = "model"
  )

  l_deviation_group_prior <- brms::stanvar(
    name = "L_deviation_group_prior",
    scode = glue::glue("for (g in 1:{n_groups}) {{ L_deviation_group[g] ~ lkj_corr_cholesky(6); }}"),
    block = "model"
  )

  return(combine_stanvars(alpha_cor_prior, l_omega_global_prior, l_deviation_group_prior))
}

#' Generate Trend Injection Stanvars
#'
#' Dispatches to appropriate trend generator based on trend_spec type.
#'
#' @param trend_spec Trend specification object
#' @param data_info Data information list
#' @return List of stanvars for trend injection
#' @noRd
generate_trend_injection_stanvars <- function(trend_spec, data_info) {
  # Validate inputs
  checkmate::assert_list(trend_spec)
  checkmate::assert_list(data_info)

  # Validate factor model compatibility if n_lv is specified
  validate_factor_compatibility(trend_spec)

  # Validate n_lv parameter constraints
  if (!is.null(trend_spec$n_lv)) {
    n_series <- data_info$n_series %||% 1
    if (trend_spec$n_lv > n_series) {
      stop(insight::format_error(
        "{.field n_lv} cannot exceed {.field n_series}.",
        paste0("Got n_lv = ", trend_spec$n_lv, " and n_series = ", n_series, "."),
        "Use fewer or equal latent variables than observed series."
      ))
    }
    # n_lv < n_series → Factor model (estimated matrix Z)
    # n_lv = n_series → Non-factor model (diagonal matrix Z)
    # Both cases are valid after factor compatibility check above
  }

  # Get trend type - handle both trend_type and trend field names for compatibility
  trend_type <- trend_spec$trend_type %||% trend_spec$trend
  if (is.null(trend_type)) {
    stop(insight::format_error(
      "trend_spec must contain {.field trend_type} or {.field trend} field"
    ))
  }

  # Handle PW variations - PWlinear and PWlogistic both use PW generator
  # Preserve original type information for PW generator
  if (trend_type %in% c("PWlinear", "PWlogistic")) {
    trend_spec$type <- if (trend_type == "PWlogistic") "logistic" else "linear"
    trend_type <- "PW"
  }

  # Get trend info from registry
  trend_info <- get_trend_info(trend_type)
  if (is.null(trend_info)) {
    stop(insight::format_error(
      "Unknown trend type: {.field {trend_type}}"
    ))
  }

  # Extract hierarchical information for shared innovation system
  hierarchical_info <- extract_hierarchical_info(data_info, trend_spec)

  # Determine if this trend uses shared Gaussian innovations
  uses_shared_innovations <- trend_spec$shared_innovations %||%
                            (!trend_type %in% c("PW", "VAR", "None"))  # PW, VAR, and None opt out

  # Generate shared innovation stanvars if needed
  shared_stanvars <- NULL
  if (uses_shared_innovations) {
    # Extract relevant parameters for shared system
    n_lv <- trend_spec$n_lv %||% data_info$n_lv %||% data_info$n_series %||% 1
    n_series <- data_info$n_series %||% 1
    cor <- trend_spec$cor %||% FALSE
    factor_model <- !is.null(trend_spec$n_lv) && trend_spec$n_lv < n_series

    # Generate shared innovation stanvars
    shared_stanvars <- generate_shared_innovation_stanvars(
      n_lv = n_lv,
      n_series = n_series,
      cor = cor,
      factor_model = factor_model,
      hierarchical_info = hierarchical_info
    )

    # Add shared priors
    effective_dim <- if (factor_model) n_lv else n_series
    is_hierarchical <- !is.null(hierarchical_info) && hierarchical_info$has_groups

    shared_priors <- generate_innovation_priors(
      effective_dim = effective_dim,
      cor = cor,
      is_hierarchical = is_hierarchical
    )

    shared_stanvars$priors <- shared_priors
  }

  # Generate trend-specific stanvars using the appropriate generator
  trend_stanvars <- trend_info$generator(trend_spec, data_info)

  # Combine shared and trend-specific stanvars
  if (!is.null(shared_stanvars)) {
    # shared_stanvars is a named list of stanvars objects, preserve the names
    # Combine with trend stanvars while preserving names from both sources
    combined_stanvars <- combine_stanvars(shared_stanvars, trend_stanvars)
    return(combined_stanvars)
  } else {
    return(trend_stanvars)
  }
}

#' Random Walk Trend Generator
#'
#' Generates Stan code components for random walk trends.
#'
#' @param trend_spec Trend specification for RW model
#' @param data_info Data information including dimensions
#' @return List of stanvars for RW trend
#' @noRd
generate_rw_trend_stanvars <- function(trend_spec, data_info) {
  # Extract dimensions
  n_lv <- trend_spec$n_lv %||% 1
  n_series <- data_info$n_series %||% 1
  is_factor_model <- !is.null(trend_spec$n_lv) && n_lv < n_series

  # Build components list
  components <- list()

  # 1. Matrix Z for factor models
  matrix_z <- generate_matrix_z_stanvars(is_factor_model, n_lv, n_series)
  if (!is.null(matrix_z)) {
    components <- append(components, list(matrix_z))
  }

  # 2. MA parameters if needed
  if (trend_spec$ma %||% FALSE) {
    ma_components <- generate_ma_components(n_lv)
    components <- append(components, ma_components)
  }

  # 3. RW dynamics (always needed)
  dynamics <- generate_rw_dynamics(n_lv, has_ma = trend_spec$ma %||% FALSE)
  if (!is.null(dynamics)) {
    components <- append(components, list(dynamics))
  }

  # 4. Trend computation
  trend_computation <- generate_trend_computation_transformed_parameters_injectors(n_lv, n_series)
  if (!is.null(trend_computation)) {
    components <- append(components, list(trend_computation))
  }

  # 5. Factor model priors if applicable
  if (is_factor_model) {
    factor_priors <- generate_factor_model_model_injectors(is_factor_model, n_lv)
    if (!is.null(factor_priors)) {
      components <- append(components, list(factor_priors))
    }
  }

  # Use the robust combine_stanvars function
  return(do.call(combine_stanvars, components))
}

#' Generate MA Components for Trend Models
#' @noRd
generate_ma_components <- function(n_lv) {
  list(
    # MA coefficient parameter
    theta1_param = brms::stanvar(
      name = "theta1_trend",
      scode = glue::glue("vector<lower=-1,upper=1>[{n_lv}] theta1_trend;"),
      block = "parameters"
    ),
    # MA prior
    theta1_prior = brms::stanvar(
      name = "theta1_trend_prior",
      scode = "theta1_trend ~ normal(0, 0.5);",
      block = "model"
    )
  )
}

#' Generate RW Dynamics Stan Code
#' @noRd
generate_rw_dynamics <- function(n_lv, has_ma = FALSE) {
  if (has_ma) {
    # RW with MA transformation
    brms::stanvar(
      name = "LV",
      scode = glue::glue("
        // Latent states with RW dynamics
        matrix[n, {n_lv}] LV;
        matrix[n, {n_lv}] LV_innovations_transformed = LV_innovations;

        // Apply MA(1) transformation if needed
        for (i in 2:n) {{
          for (j in 1:{n_lv}) {{
            LV_innovations_transformed[i, j] += theta1_trend[j] * LV_innovations_transformed[i-1, j];
          }}
        }}

        // Apply RW dynamics
        LV[1, :] = LV_innovations_transformed[1, :];
        for (i in 2:n) {{
          LV[i, :] = LV[i-1, :] + LV_innovations_transformed[i, :];
        }}
      "),
      block = "tparameters"
    )
  } else {
    # Simple RW dynamics
    brms::stanvar(
      name = "LV",
      scode = glue::glue("
        // Latent states with simple RW dynamics
        matrix[n, {n_lv}] LV;

        // Apply RW dynamics using innovations from shared system
        LV[1, :] = LV_innovations[1, :];
        for (i in 2:n) {{
          LV[i, :] = LV[i-1, :] + LV_innovations[i, :];
        }}
      "),
      block = "tparameters"
    )
  }
}

#' Generate AR Dynamics Stan Code
#' @noRd
generate_ar_dynamics <- function(n_lv, ar_lags, has_ma = FALSE) {
  # Build AR coefficient declarations
  ar_params <- paste0(
    sapply(ar_lags, function(lag) {
      glue::glue("vector<lower=-1,upper=1>[{n_lv}] ar{lag}_trend;")
    }),
    collapse = "\n    "
  )

  # Build AR dynamics computation
  ar_terms <- paste0(
    sapply(ar_lags, function(lag) {
      glue::glue("ar{lag}_trend[j] * LV[i-{lag}, j]")
    }),
    collapse = " + "
  )

  # Max lag for initialization
  max_lag <- max(ar_lags)

  if (has_ma) {
    # AR with MA transformation
    list(
      # AR parameters
      ar_params = brms::stanvar(
        name = "ar_params",
        scode = ar_params,
        block = "parameters"
      ),
      # Dynamics
      dynamics = brms::stanvar(
        name = "LV",
        scode = glue::glue("
          // Latent states with AR dynamics
          matrix[n, {n_lv}] LV;
          matrix[n, {n_lv}] LV_innovations_transformed = LV_innovations;

          // Apply MA(1) transformation
          for (i in 2:n) {{
            for (j in 1:{n_lv}) {{
              LV_innovations_transformed[i, j] += theta1_trend[j] * LV_innovations_transformed[i-1, j];
            }}
          }}

          // Initialize first {max_lag} time points
          for (i in 1:{max_lag}) {{
            LV[i, :] = LV_innovations_transformed[i, :];
          }}

          // AR dynamics for remaining time points
          for (i in {max_lag + 1}:n) {{
            for (j in 1:{n_lv}) {{
              LV[i, j] = {ar_terms} + LV_innovations_transformed[i, j];
            }}
          }}
        "),
        block = "tparameters"
      )
    )
  } else {
    # Simple AR dynamics
    list(
      # AR parameters
      ar_params = brms::stanvar(
        name = "ar_params",
        scode = ar_params,
        block = "parameters"
      ),
      # Dynamics
      dynamics = brms::stanvar(
        name = "LV",
        scode = glue::glue("
          // Latent states with AR dynamics
          matrix[n, {n_lv}] LV;

          // Initialize first {max_lag} time points
          for (i in 1:{max_lag}) {{
            LV[i, :] = LV_innovations[i, :];
          }}

          // AR dynamics for remaining time points
          for (i in {max_lag + 1}:n) {{
            for (j in 1:{n_lv}) {{
              LV[i, j] = {ar_terms} + LV_innovations[i, j];
            }}
          }}
        "),
        block = "tparameters"
      )
    )
  }
}

#' AR Trend Generator (Simplified with Shared Innovations)
#' @noRd
generate_ar_trend_stanvars <- function(trend_spec, data_info) {
  # Extract dimensions
  n_lv <- trend_spec$n_lv %||% 1
  n_series <- data_info$n_series %||% 1
  ar_lags <- trend_spec$ar_lags %||% (1:trend_spec$p)
  is_factor_model <- !is.null(trend_spec$n_lv) && n_lv < n_series

  # Build components list
  components <- list()

  # 1. Matrix Z for factor models
  matrix_z <- generate_matrix_z_stanvars(is_factor_model, n_lv, n_series)
  if (!is.null(matrix_z)) {
    components <- append(components, list(matrix_z))
  }

  # 2. MA parameters if needed
  if (trend_spec$ma %||% FALSE) {
    ma_components <- generate_ma_components(n_lv)
    components <- append(components, ma_components)
  }

  # 3. AR dynamics (always needed)
  dynamics <- generate_ar_dynamics(n_lv, ar_lags, has_ma = trend_spec$ma %||% FALSE)
  if (!is.null(dynamics)) {
    components <- append(components, list(dynamics))
  }

  # 4. AR priors
  ar_priors <- brms::stanvar(
    name = "ar_priors",
    scode = paste0(
      sapply(ar_lags, function(lag) {
        glue::glue("ar{lag}_trend ~ normal(0, 0.5);")
      }),
      collapse = "\n  "
    ),
    block = "model"
  )
  components <- append(components, list(ar_priors))

  # 5. Trend computation
  trend_computation <- generate_trend_computation_transformed_parameters_injectors(n_lv, n_series)
  if (!is.null(trend_computation)) {
    components <- append(components, list(trend_computation))
  }

  # 6. Factor model priors if applicable
  if (is_factor_model) {
    factor_priors <- generate_factor_model_model_injectors(is_factor_model, n_lv)
    if (!is.null(factor_priors)) {
      components <- append(components, list(factor_priors))
    }
  }

  # Use the robust combine_stanvars function
  return(do.call(combine_stanvars, components))
}

# [Old RW implementation code removed - now using modular approach with shared innovations]

#' VAR Trend Generator
#'
#' Generates Stan code components for vector autoregressive trends.
#' Supports factor models, hierarchical correlations, and consistent matrix Z patterns.
#'
#' @param trend_spec Trend specification for VAR model
#' @param data_info Data information including dimensions
#' @return List of stanvars for VAR trend
#' @noRd
generate_var_trend_stanvars <- function(trend_spec, data_info) {
  # Extract key parameters
  n_lv <- trend_spec$n_lv %||% 1
  lags <- trend_spec$lags %||% 1
  n <- data_info$n_obs
  n_series <- data_info$n_series %||% 1
  use_grouping <- !is.null(trend_spec$gr) && trend_spec$gr != 'NA'
  unit_var <- trend_spec$unit %||% "time"

  # Determine if this is a factor model (n_lv < n_series)
  is_factor_model <- !is.null(trend_spec$n_lv) && n_lv < n_series

  # Generate block-specific injectors for matrix Z
  matrix_z_stanvars <- generate_matrix_z_stanvars(is_factor_model, n_lv, n_series)

  # Start with matrix Z stanvars
  result_stanvars <- matrix_z_stanvars

  # VAR data block (needed for all cases)
  var_data_stanvar <- brms::stanvar(
    x = list(
      n_lv = n_lv,
      n_lags = lags,
      lv_coefs = array(0, dim = c(n_lv, n_lv, lags))
    ),
    name = "var_data",
    scode = glue::glue("
    int<lower=1> n_lv;
    int<lower=1> n_lags;
    array[n_lv, n_lv, n_lags] real lv_coefs;
    ")
  )

  if (use_grouping) {
    # Hierarchical VAR case: VAR(p = 1, unit = site, gr = group, subgr = species)
    n_groups <- data_info$n_groups %||% 1
    n_subgroups <- data_info$n_subgroups %||% n_lv

    # Note: Hierarchical correlation utilities are now handled centrally
    # to avoid stanvar name duplication across trend generators

    # VAR-specific parameters for hierarchical case
    var_hierarchical_params_stanvar <- brms::stanvar(
      name = "var_hierarchical_params",
      scode = glue::glue("
        // VAR coefficient matrices for each lag
        array[{lags}] matrix[{n_lv}, {n_lv}] A;
        // Latent states
        matrix[n, {n_lv}] LV;
      "),
      block = "parameters"
    )

    # Hierarchical VAR model implementation
    var_hierarchical_model_stanvar <- brms::stanvar(
      name = "var_hierarchical_model",
      scode = glue::glue("
        // Derived hierarchical correlation matrices
        array[{n_groups}] cholesky_factor_corr[{n_subgroups}] L_Omega_group;
        for (g in 1 : {n_groups}) {{
          L_Omega_group[g] = combine_cholesky(L_Omega_global, L_deviation_group[g],
                                              alpha_cor);
        }}

        // Hierarchical VAR process with group-specific correlations
        for (i in 1:n_{unit_var}) {{
          for (g in 1:{n_groups}) {{
            // VAR dynamics with hierarchical residual correlation
            for (t in ({lags}+1):n) {{
              vector[{n_subgroups}] mu = rep_vector(0, {n_subgroups});
              for (lag in 1:{lags}) {{
                mu += A[lag] * LV[t-lag, :]';
              }}
              to_vector(LV[group_unit_indices[i, g], t]) ~ multi_normal_cholesky(mu, L_Omega_group[g]);
            }}
          }}
        }}

        // VAR coefficient priors
        for (lag in 1:{lags}) {{
          to_vector(A[lag]) ~ normal(0, 0.5);
        }}
      "),
      block = "model"
    )

    # Note: Hierarchical priors are now handled centrally

    # Combine stanvars for hierarchical case (without hierarchical utilities)
    result_stanvars <- combine_stanvars(result_stanvars, var_data_stanvar, 
                                      var_hierarchical_params_stanvar,
                                      var_hierarchical_model_stanvar)

  } else {
    # Simple VAR case (no grouping)
    if (is_factor_model) {
      # Factor model: fixed variance in covariance, estimate Z
      var_params_stanvar <- brms::stanvar(
        name = "var_params",
        scode = glue::glue("
        parameters {{
          // VAR coefficient matrices for each lag
          array[{lags}] matrix[{n_lv}, {n_lv}] A;
          // Innovation correlation matrix (variances fixed to 1)
          corr_matrix[{n_lv}] Omega;
          // Latent states
          matrix[n, {n_lv}] LV;
        }}
        "),
        block = "parameters"
      )

      # VAR model block for factor case
      var_model_stanvar <- brms::stanvar(
        name = "var_model",
        scode = glue::glue("
        model {{
          // VAR process with fixed variance = 1
          for (t in ({lags}+1):n) {{
            vector[{n_lv}] mu = rep_vector(0, {n_lv});
            for (lag in 1:{lags}) {{
              mu += A[lag] * LV[t-lag, :]';
            }}
            LV[t, :] ~ multi_normal(mu', Omega);
          }}

          // Priors for factor model
          for (lag in 1:{lags}) {{
            to_vector(A[lag]) ~ normal(0, 0.5);
          }}
          Omega ~ lkj_corr(2);
        }}
        "),
        block = "model"
      )
    } else {
      # Non-factor model: estimate full covariance, diagonal Z
      var_params_stanvar <- brms::stanvar(
        name = "var_params",
        scode = glue::glue("
        parameters {{
          // VAR coefficient matrices for each lag
          array[{lags}] matrix[{n_lv}, {n_lv}] A;
          // Innovation covariance matrix
          cov_matrix[{n_lv}] Sigma;
          // Latent states
          matrix[n, {n_lv}] LV;
        }}
        "),
        block = "parameters"
      )

      # VAR model block for non-factor case
      var_model_stanvar <- brms::stanvar(
        name = "var_model",
        scode = glue::glue("
        model {{
          // VAR process
          for (t in ({lags}+1):n) {{
            vector[{n_lv}] mu = rep_vector(0, {n_lv});
            for (lag in 1:{lags}) {{
              mu += A[lag] * LV[t-lag, :]';
            }}
            LV[t, :] ~ multi_normal(mu', Sigma);
          }}

          // Priors for non-factor model
          for (lag in 1:{lags}) {{
            to_vector(A[lag]) ~ normal(0, 0.5);
          }}
          Sigma ~ inv_wishart({n_lv} + 1, diag_matrix(rep_vector(1, {n_lv})));
        }}
        "),
        block = "model"
      )
    }

    # Combine stanvars for non-hierarchical case
    result_stanvars <- combine_stanvars(result_stanvars, var_data_stanvar,
                                      var_params_stanvar, var_model_stanvar)

    # Use shared factor model priors if applicable
    if (is_factor_model) {
      factor_priors <- generate_factor_model_model_injectors(is_factor_model, n_lv)
      result_stanvars <- combine_stanvars(result_stanvars, factor_priors)
    }
  }

  # Add trend computation stanvars
  trend_computation <- generate_trend_computation_transformed_parameters_injectors(n_lv, n_series)
  result_stanvars <- combine_stanvars(result_stanvars, trend_computation)

  return(result_stanvars)
}

#' AR Trend Generator
#'
#' Generates Stan code components for autoregressive trends.
#' Supports factor models, hierarchical correlations, and consistent matrix Z patterns.
#'
#' @param trend_spec Trend specification for AR model
#' @param data_info Data information including dimensions
#' @return List of stanvars for AR trend
#' @noRd
generate_ar_trend_stanvars <- function(trend_spec, data_info) {
  # Extract key parameters
  n_lv <- trend_spec$n_lv %||% 1
  lags <- trend_spec$lags %||% 1
  n <- data_info$n_obs
  n_series <- data_info$n_series %||% 1
  use_grouping <- !is.null(trend_spec$gr) && trend_spec$gr != 'NA'
  unit_var <- trend_spec$unit %||% "time"

  # Determine if this is a factor model (n_lv < n_series)
  is_factor_model <- !is.null(trend_spec$n_lv) && n_lv < n_series

  # Generate block-specific injectors for matrix Z
  matrix_z_stanvars <- generate_matrix_z_stanvars(is_factor_model, n_lv, n_series)

  # Start with matrix Z stanvars
  result_stanvars <- matrix_z_stanvars

  if (use_grouping) {
    # Hierarchical AR case: AR(p = 1, unit = site, gr = group, subgr = species)
    n_groups <- data_info$n_groups %||% 1
    n_subgroups <- data_info$n_subgroups %||% n_lv

    # Note: Hierarchical correlation utilities are now handled centrally
    # to avoid stanvar name duplication across trend generators

    # AR-specific parameters for hierarchical case
    ar_hierarchical_params_stanvar <- brms::stanvar(
      name = "ar_hierarchical_params",
      scode = glue::glue("
        // AR coefficients for each latent variable
        matrix[{n_lv}, {lags}] phi;
        // Latent states
        matrix[n, {n_lv}] LV;
      "),
      block = "parameters"
    )

    # Hierarchical AR model implementation
    ar_hierarchical_model_stanvar <- brms::stanvar(
      name = "ar_hierarchical_model",
      scode = glue::glue("
        // Derived hierarchical correlation matrices
        array[{n_groups}] cholesky_factor_corr[{n_subgroups}] L_Omega_group;
        for (g in 1 : {n_groups}) {{
          L_Omega_group[g] = combine_cholesky(L_Omega_global, L_deviation_group[g],
                                              alpha_cor);
        }}

        // Hierarchical AR process with group-specific correlations
        for (i in 1:n_{unit_var}) {{
          for (g in 1:{n_groups}) {{
            // AR dynamics with hierarchical residual correlation
            for (t in ({lags}+1):n) {{
              vector[{n_subgroups}] mu = rep_vector(0, {n_subgroups});
              for (lag in 1:{lags}) {{
                for (j in 1:{n_lv}) {{
                  mu[j] += phi[j, lag] * LV[t-lag, j];
                }}
              }}
              to_vector(LV[group_unit_indices[i, g], t]) ~ multi_normal_cholesky(mu, L_Omega_group[g]);
            }}
          }}
        }}

        // AR coefficient priors
        to_vector(phi) ~ normal(0, 0.5);
      "),
      block = "model"
    )

    # Note: Hierarchical priors are now handled centrally

    # Combine stanvars for hierarchical case (without hierarchical utilities)
    result_stanvars <- combine_stanvars(result_stanvars, ar_hierarchical_params_stanvar,
                                      ar_hierarchical_model_stanvar)

  } else {
    # Simple AR case (no grouping)
    if (is_factor_model) {
      # Factor model: fixed variance = 1, estimate Z
      ar_params_stanvar <- brms::stanvar(
        name = "ar_params",
        scode = glue::glue("
        parameters {{
          // AR coefficients for each latent variable
          matrix[{n_lv}, {lags}] phi;
          // Raw latent states for factor model (variance = 1)
          matrix[n, {n_lv}] LV_raw;
        }}
        "),
        block = "parameters"
      )

      ar_transformed_stanvar <- brms::stanvar(
        name = "ar_transformed",
        scode = glue::glue("
        transformed parameters {{
          // Apply AR dynamics with fixed variance = 1
          matrix[n, {n_lv}] LV;
          LV = LV_raw;

          for (j in 1:{n_lv}) {{
            for (t in ({lags}+1):n) {{
              real mu = 0;
              for (lag in 1:{lags}) {{
                mu += phi[j, lag] * LV[t-lag, j];
              }}
              LV[t, j] = mu + LV_raw[t, j];
            }}
          }}
        }}
        "),
        block = "tparameters"
      )
    } else {
      # Non-factor model: estimate variances, diagonal Z
      ar_params_stanvar <- brms::stanvar(
        name = "ar_params",
        scode = glue::glue("
        parameters {{
          // AR coefficients for each latent variable
          matrix[{n_lv}, {lags}] phi;
          // Innovation standard deviations
          vector<lower=0>[{n_lv}] sigma;
          // Latent states
          matrix[n, {n_lv}] LV;
        }}
        "),
        block = "parameters"
      )

      # AR model block for non-factor case
      ar_model_stanvar <- brms::stanvar(
        name = "ar_model",
        scode = glue::glue("
        model {{
          // AR process for each series independently
          for (j in 1:{n_lv}) {{
            for (t in ({lags}+1):n) {{
              real mu = 0;
              for (lag in 1:{lags}) {{
                mu += phi[j, lag] * LV[t-lag, j];
              }}
              LV[t, j] ~ normal(mu, sigma[j]);
            }}
          }}

          // Priors for non-factor model
          to_vector(phi) ~ normal(0, 0.5);
          sigma ~ student_t(3, 0, 2.5);
        }}
        "),
        block = "model"
      )
    }

    # Combine stanvars for non-hierarchical case
    if (is_factor_model) {
      result_stanvars <- combine_stanvars(result_stanvars, ar_params_stanvar, ar_transformed_stanvar)

      # Use shared factor model priors
      factor_priors <- generate_factor_model_priors(is_factor_model, n_lv)

      # Add AR-specific priors for factor model
      ar_factor_priors_stanvar <- brms::stanvar(
        name = "ar_factor_priors",
        scode = glue::glue("
        model {{
          // AR coefficient priors for factor model
          to_vector(phi) ~ normal(0, 0.5);
        }}
        "),
        block = "model"
      )

      result_stanvars <- combine_stanvars(result_stanvars, factor_priors, ar_factor_priors_stanvar)
    } else {
      result_stanvars <- combine_stanvars(result_stanvars, ar_params_stanvar, ar_model_stanvar)
    }
  }

  # Add trend computation stanvars
  trend_computation <- generate_trend_computation_transformed_parameters_injectors(n_lv, n_series)
  result_stanvars <- combine_stanvars(result_stanvars, trend_computation)

  return(result_stanvars)
}

#' Calculate Time Distances for CAR Models
#'
#' Calculate temporal distances between observations for continuous-time AR.
#' Uses pmax(1e-3, dis_time) to prevent zero distances.
#'
#' @param data_info Data information containing data, time variable, and series
#' @return Matrix of time distances [n, n_series]
#' @noRd
calculate_car_time_distances <- function(data_info) {
  data <- data_info$data
  time_var <- data_info$time_var %||% "time"
  series_var <- data_info$series_var %||% "series"

  # Prepare time and series data
  all_times <- data.frame(
    series = as.numeric(data[[series_var]]),
    time = data[[time_var]]
  ) %>%
    dplyr::group_by(series) %>%
    dplyr::arrange(time) %>%
    dplyr::mutate(
      time_lag = dplyr::lag(time),
      dis_time = time - time_lag,
      dis_time = ifelse(is.na(dis_time), 1, dis_time),
      dis_time = pmax(1e-3, dis_time)  # Critical: cannot let distance go to zero
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(time, series)

  # Convert to matrix format [n_time, n_series]
  n_time <- length(unique(all_times$time))
  n_series <- length(unique(all_times$series))

  time_dis <- matrix(
    NA,
    nrow = n_time,
    ncol = n_series
  )

  for (s in seq_len(n_series)) {
    series_data <- all_times[all_times$series == s, ]
    time_dis[seq_len(nrow(series_data)), s] <- series_data$dis_time
  }

  return(time_dis)
}

#' CAR Trend Generator
#'
#' Generates Stan code components for continuous-time autoregressive trends.
#' Does NOT support factor models or hierarchical correlations.
#'
#' @param trend_spec Trend specification for CAR model
#' @param data_info Data information including dimensions
#' @return List of stanvars for CAR trend
#' @noRd
generate_car_trend_stanvars <- function(trend_spec, data_info) {
  # Extract key parameters
  n_lv <- trend_spec$n_lv %||% data_info$n_series %||% 1
  n <- data_info$n_obs
  n_series <- data_info$n_series %||% 1

  # CAR does not support factor models (continuous-time AR requires
  # series-specific temporal evolution)
  if (!is.null(trend_spec$n_lv) && trend_spec$n_lv < n_series) {
    stop(insight::format_error(
      "CAR trends do not support factor models (n_lv < n_series).",
      "Continuous-time AR requires series-specific temporal evolution modeling."
    ))
  }

  # CAR does not support hierarchical correlations
  if (!is.null(trend_spec$gr) && trend_spec$gr != 'NA') {
    rlang::warn(
      "CAR trends do not support hierarchical correlations; ignoring 'gr' parameter",
      .frequency = "once",
      .frequency_id = "CAR_group_error"
    )
  }

  # Calculate time distances for continuous-time AR evolution
  time_dis <- calculate_car_time_distances(data_info)

  # Time distance data for continuous-time AR
  time_dis_data_stanvar <- brms::stanvar(
    x = time_dis,
    name = "time_dis",
    scode = glue::glue("  array[n, n_series] real<lower=0> time_dis;"),
    block = "data"
  )

  # CAR parameters (continuous-time AR1)
  car_params_stanvar <- brms::stanvar(
    name = "car_params",
    scode = glue::glue("
  // CAR AR1 parameters
  vector<lower=-1,upper=1>[n_lv] ar1;

  // latent state SD terms
  vector<lower=0>[n_lv] sigma;

  // raw latent states
  matrix[n, n_lv] LV_raw;"),
    block = "parameters"
  )

  # CAR transformed parameters (continuous-time evolution only)
  car_lv_evolution_stanvar <- brms::stanvar(
    name = "car_lv_evolution",
    scode = glue::glue("
  // CAR latent variable evolution
  matrix[n, n_lv] LV;

  // Apply continuous-time AR evolution
  LV = LV_raw .* rep_matrix(sigma', rows(LV_raw));

  for (j in 1 : n_lv) {{
    LV[1, j] += mu_trend[ytimes_trend[1, j]];
    for (i in 2 : n) {{
      LV[i, j] += mu_trend[ytimes_trend[i, j]]
                  + pow(ar1[j], time_dis[i, j])
                    * (LV[i - 1, j] - mu_trend[ytimes_trend[i - 1, j]]);
    }}
  }}"),
    block = "tparameters"
  )

  # Use shared trend computation utility (consistent with all other trends)
  trend_computation <- generate_trend_computation_transformed_parameters_injectors(n_lv, n_series)

  # CAR model priors
  car_priors_stanvar <- brms::stanvar(
    name = "car_priors",
    scode = "
  // CAR priors
  ar1 ~ std_normal();
  sigma ~ exponential(3);
  to_vector(LV_raw) ~ std_normal();",
    block = "model"
  )

  # Combine all stanvars
  result_stanvars <- combine_stanvars(time_dis_data_stanvar, car_params_stanvar,
                                    car_lv_evolution_stanvar, trend_computation,
                                    car_priors_stanvar)

  return(result_stanvars)
}

#' GP Trend Generator
#'
#' Generates Stan code components for Gaussian process trends.
#'
#' @param trend_spec Trend specification for GP model
#' @param data_info Data information including dimensions
#' @return List of stanvars for GP trend
#' @noRd
# GP trends are handled via trend_formula, not as temporal dynamics
generate_gp_injection_stanvars <- function(trend_spec, data_info) {
  stop(insight::format_error(
    "GP trends are handled via trend_formula, not as temporal dynamics.",
    "Use: trend_formula = ~ gp(time, k = 10)..."
  ))
}

# Factor trend generator removed - factor models are now a capability of compatible trend types (AR, RW, VAR)
# Factor models are detected by n_lv < n_series on compatible trends, not as a separate trend type

#' ZMVN Trend Generator
#'
#' Generates Stan code components for zero-mean multivariate normal trends.
#'
#' @param trend_spec Trend specification for ZMVN model
#' @param data_info Data information including dimensions
#' @return List of stanvars for ZMVN trend
#' @noRd
generate_zmvn_trend_stanvars <- function(trend_spec, data_info) {
  # Extract key parameters following original ZMVN pattern
  n_lv <- trend_spec$n_lv %||% data_info$n_lv %||% data_info$n_series %||% 1
  n <- data_info$n_obs
  n_series <- data_info$n_series %||% 1
  use_grouping <- !is.null(trend_spec$gr) && trend_spec$gr != 'NA'
  unit_var <- trend_spec$unit %||% "time"

  # Determine if this is a factor model (n_lv < n_series)
  is_factor_model <- !is.null(trend_spec$n_lv) && n_lv < n_series

  # Generate block-specific injectors for matrix Z
  matrix_z_stanvars <- generate_matrix_z_stanvars(is_factor_model, n_lv, n_series)

  # Start with matrix Z stanvars
  result_stanvars <- matrix_z_stanvars

  if (use_grouping) {
    # Hierarchical ZMVN case: ZMVN(unit = site, gr = group, subgr = species)
    n_groups <- data_info$n_groups %||% 1
    n_subgroups <- data_info$n_subgroups %||% n_lv

    # Note: Hierarchical correlation utilities are now handled centrally
    # to avoid stanvar name duplication across trend generators

    # ZMVN-specific parameters for hierarchical case
    zmvn_hierarchical_params_stanvar <- brms::stanvar(
      name = "zmvn_hierarchical_params",
      scode = glue::glue("
        // Latent states for ZMVN
        matrix[n, {n_lv}] LV;
      "),
      block = "parameters"
    )

    # Hierarchical ZMVN model implementation
    zmvn_hierarchical_model_stanvar <- brms::stanvar(
      name = "zmvn_hierarchical_model",
      scode = glue::glue("
        // Derived hierarchical correlation matrices
        array[{n_groups}] cholesky_factor_corr[{n_subgroups}] L_Omega_group;
        for (g in 1 : {n_groups}) {{
          L_Omega_group[g] = combine_cholesky(L_Omega_global, L_deviation_group[g],
                                              alpha_cor);
        }}

        // ZMVN residual correlation by {unit_var}
        for (i in 1:n_{unit_var}) {{
          for (g in 1:{n_groups}) {{
            to_vector(LV[group_unit_indices[i, g]]) ~ multi_normal_cholesky(rep_vector(0, {n_subgroups}),
                                                                             L_Omega_group[g]);
          }}
        }}
      "),
      block = "model"
    )

    # Note: Hierarchical priors are now handled centrally

    # Combine stanvars for hierarchical case (without hierarchical utilities)
    result_stanvars <- combine_stanvars(result_stanvars, zmvn_hierarchical_params_stanvar,
                                      zmvn_hierarchical_model_stanvar)

  } else {
    # Simple ZMVN case: ZMVN(unit = site, subgr = species)
    zmvn_simple_params_stanvar <- brms::stanvar(
      name = "zmvn_simple_params",
      scode = glue::glue("
        // correlation matrix for ZMVN
        cholesky_factor_corr[{n_lv}] L_Omega;
        // Latent states for ZMVN
        matrix[n, {n_lv}] LV;
      "),
      block = "parameters"
    )

    zmvn_simple_model_stanvar <- brms::stanvar(
      name = "zmvn_simple_model",
      scode = glue::glue("
        // Simple ZMVN residual correlation by {unit_var}
        L_Omega ~ lkj_corr_cholesky(2);
        for (i in 1:n_{unit_var}) {{
          to_vector(LV[{unit_var}_indices[i]]) ~ multi_normal_cholesky(rep_vector(0, {n_lv}), L_Omega);
        }}
      "),
      block = "model"
    )

    # Combine stanvars for simple case
    result_stanvars <- combine_stanvars(result_stanvars, zmvn_simple_params_stanvar,
                                      zmvn_simple_model_stanvar)
  }

  # Add trend computation stanvars
  trend_computation <- generate_trend_computation_transformed_parameters_injectors(n_lv, n_series)
  result_stanvars <- combine_stanvars(result_stanvars, trend_computation)

  # Add factor model priors if applicable
  if (is_factor_model) {
    factor_priors <- generate_factor_model_priors(is_factor_model, n_lv)
    result_stanvars <- combine_stanvars(result_stanvars, factor_priors)
  }

  return(result_stanvars)
}

#' PW Trend Generator
#'
#' Generates Stan code components for piecewise trends.
#' Supports both linear and logistic piecewise trends with proper Prophet-style implementations.
#'
#' @param trend_spec Trend specification for PW model
#' @param data_info Data information including dimensions
#' @return List of stanvars for PW trend
#' @noRd
generate_pw_trend_stanvars <- function(trend_spec, data_info) {
  # Extract key parameters
  n_lv <- trend_spec$n_lv %||% 1
  n_changepoints <- trend_spec$n_changepoints %||% 5
  changepoint_scale <- trend_spec$changepoint_scale %||% 0.1
  trend_type <- trend_spec$type %||% "linear"  # "linear" or "logistic"
  n <- data_info$n_obs
  n_series <- data_info$n_series %||% 1

  # Validate trend type
  if (!trend_type %in% c("linear", "logistic")) {
    stop(insight::format_error(
      "Piecewise trend type must be 'linear' or 'logistic'.",
      paste0("Got type = '", trend_type, "'."),
      "Use type = 'linear' or type = 'logistic'."
    ))
  }

  # PW trends do not support factor models (series-specific changepoints required)
  is_factor_model <- FALSE

  # Generate block-specific injectors for matrix Z
  matrix_z_stanvars <- generate_matrix_z_stanvars(is_factor_model, n_lv, n_series)

  # Start components list
  components <- list()
  if (!is.null(matrix_z_stanvars)) {
    components <- append(components, list(matrix_z_stanvars))
  }

  # Functions block - Prophet-style piecewise functions
  pw_functions_stanvar <- brms::stanvar(
    name = "pw_functions",
    scode = "
      matrix get_changepoint_matrix(vector t, vector t_change, int T, int S) {
        /* Function to sort changepoints */

        /* credit goes to the Prophet development team at Meta (https://github.com/facebook/prophet/tree/main)*/
        matrix[T, S] A;
        row_vector[S] a_row;
        int cp_idx;
        A = rep_matrix(0, T, S);
        a_row = rep_row_vector(0, S);
        cp_idx = 1;
        for (i in 1 : T) {
          while ((cp_idx <= S) && (t[i] >= t_change[cp_idx])) {
            a_row[cp_idx] = 1;
            cp_idx = cp_idx + 1;
          }
          A[i] = a_row;
        }
        return A;
      }

      vector logistic_gamma(real k, real m, vector delta, vector t_change, int S) {
        /* Function to compute a logistic trend with changepoints */

        /* credit goes to the Prophet development team at Meta (https://github.com/facebook/prophet/tree/main)*/
        vector[S] gamma; // adjusted offsets, for piecewise continuity
        vector[S + 1] k_s; // actual rate in each segment
        real m_pr;
        k_s = append_row(k, k + cumulative_sum(delta));
        m_pr = m; // The offset in the previous segment
        for (i in 1 : S) {
          gamma[i] = (t_change[i] - m_pr) * (1 - k_s[i] / k_s[i + 1]);
          m_pr = m_pr + gamma[i]; // update for the next segment
        }
        return gamma;
      }

      vector logistic_trend(real k, real m, vector delta, vector t, vector cap,
                            matrix A, vector t_change, int S) {
        /* Function to adjust a logistic trend using a carrying capacity */

        /* credit goes to the Prophet development team at Meta (https://github.com/facebook/prophet/tree/main)*/
        vector[S] gamma;
        gamma = logistic_gamma(k, m, delta, t_change, S);
        return cap .* inv_logit((k + A * delta) .* (t - (m + A * gamma)));
      }

      vector linear_trend(real k, real m, vector delta, vector t, matrix A,
                          vector t_change) {
        /* Function to compute a linear trend with changepoints */

        /* credit goes to the Prophet development team at Meta (https://github.com/facebook/prophet/tree/main)*/
        return (k + A * delta) .* t + (m + A * (-t_change .* delta));
      }
    ",
    block = "functions"
  )

  # Data block - piecewise-specific data components
  pw_data_stanvar <- brms::stanvar(
    x = list(
      n_changepoints = n_changepoints,
      t_change = seq(0.1, 0.9, length.out = n_changepoints),  # Default changepoint times
      changepoint_scale = changepoint_scale
    ),
    name = "pw_data",
    scode = glue::glue("
      int<lower=0> n_changepoints; // number of potential trend changepoints
      vector[n_changepoints] t_change; // times of potential changepoints
      real<lower=0> changepoint_scale; // scale of changepoint shock prior
    "),
    block = "data"
  )

  # Initialize logistic data variable
  pw_logistic_data_stanvar <- NULL
  
  # Logistic-specific data (carrying capacity)
  if (trend_type == "logistic") {
    pw_logistic_data_stanvar <- brms::stanvar(
      x = matrix(10, nrow = n, ncol = n_series),  # Default carrying capacity
      name = "pw_logistic_data",
      scode = glue::glue("matrix[n, n_series] cap; // carrying capacities"),
      block = "data"
    )
  }

  # Transformed data block - changepoint matrix computation
  pw_transformed_data_stanvar <- brms::stanvar(
    name = "pw_transformed_data",
    scode = glue::glue("
      // sorted changepoint matrix
      matrix[n, n_changepoints] A = get_changepoint_matrix(time, t_change, n, n_changepoints);
    "),
    block = "tdata"
  )

  # Parameters block - piecewise trend parameters
  pw_parameters_stanvar <- brms::stanvar(
    name = "pw_parameters",
    scode = glue::glue("
      // base trend growth rates
      vector[n_series] k_trend;

      // trend offset parameters
      vector[n_series] m_trend;

      // trend rate adjustments per series
      matrix[n_changepoints, n_series] delta_trend;
    "),
    block = "parameters"
  )

  # Transformed parameters block - trend computation (type-specific)
  if (trend_type == "logistic") {
    pw_transformed_parameters_stanvar <- brms::stanvar(
      name = "pw_transformed_parameters",
      scode = glue::glue("
        // raw latent variables (logistic piecewise trends)
        matrix[n, n_series] LV;

        // logistic trend estimates
        for (s in 1 : n_series) {{
          LV[1 : n, s] = logistic_trend(k_trend[s], m_trend[s],
                                        to_vector(delta_trend[ : , s]), time,
                                        to_vector(cap[ : , s]), A, t_change,
                                        n_changepoints);
        }}
      "),
      block = "tparameters"
    )
  } else {
    # Linear type
    pw_transformed_parameters_stanvar <- brms::stanvar(
      name = "pw_transformed_parameters",
      scode = glue::glue("
        // raw latent variables (linear piecewise trends)
        matrix[n, n_series] LV;

        // linear trend estimates
        for (s in 1 : n_series) {{
          LV[1 : n, s] = linear_trend(k_trend[s], m_trend[s],
                                      to_vector(delta_trend[ : , s]), time, A,
                                      t_change);
        }}
      "),
      block = "tparameters"
    )
  }

  # Add trend computation stanvars
  trend_computation <- generate_trend_computation_transformed_parameters_injectors(n_lv, n_series)

  # Add model block priors
  pw_model_stanvar <- brms::stanvar(
    name = "pw_model",
    scode = glue::glue("
      // trend parameter priors
      m_trend ~ student_t(3, 0, 2.5);
      k_trend ~ std_normal();
      to_vector(delta_trend) ~ double_exponential(0, changepoint_scale);
    "),
    block = "model"
  )

  # Add all required components to the list
  components <- append(components, list(
    pw_functions_stanvar, 
    pw_data_stanvar, 
    pw_transformed_data_stanvar,
    pw_parameters_stanvar, 
    pw_transformed_parameters_stanvar, 
    trend_computation,
    pw_model_stanvar
  ))

  # Add logistic-specific data if needed
  if (!is.null(pw_logistic_data_stanvar)) {
    components <- append(components, list(pw_logistic_data_stanvar))
  }

  # Use robust combination
  return(do.call(combine_stanvars, components))
}

#' PWlinear Trend Generator
#'
#' Generates Stan code components for linear piecewise trends.
#' Wrapper around PW generator with type = "linear".
#'
#' @param trend_spec Trend specification for PWlinear model
#' @param data_info Data information including dimensions
#' @return List of stanvars for PWlinear trend
#' @noRd
generate_pwlinear_trend_stanvars <- function(trend_spec, data_info) {
  # Force linear type for PWlinear
  trend_spec$type <- "linear"

  # Delegate to main PW generator
  generate_pw_trend_stanvars(trend_spec, data_info)
}

#' PWlogistic Trend Generator
#'
#' Generates Stan code components for logistic piecewise trends.
#' Wrapper around PW generator with type = "logistic".
#'
#' @param trend_spec Trend specification for PWlogistic model
#' @param data_info Data information including dimensions
#' @return List of stanvars for PWlogistic trend
#' @noRd
generate_pwlogistic_trend_stanvars <- function(trend_spec, data_info) {
  # Force logistic type for PWlogistic
  trend_spec$type <- "logistic"

  # Delegate to main PW generator
  generate_pw_trend_stanvars(trend_spec, data_info)
}

#' None Trend Generator (placeholder)
#'
#' Returns empty stanvars for no-trend models.
#'
#' @param trend_spec Trend specification (ignored)
#' @param data_info Data information (ignored)
#' @return Empty list
#' @noRd
generate_none_trend_stanvars <- function(trend_spec, data_info) {
  list()  # No trend components needed
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

# =============================================================================
# SECTION 3: STAN CODE VALIDATION SYSTEM
# =============================================================================
# WHY: Stan code validation prevents runtime compilation failures and ensures
# generated code follows Stan syntax rules. This layer catches errors early
# and provides actionable error messages, critical for user experience and
# debugging the two-stage assembly system.
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
