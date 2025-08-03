#' Combined Stan Code Generation for mvgam
#'
#' @description
#' Implements the complete two-stage Stan assembly system with validation,
#' bringing together trend stanvar extraction and linear predictor modification.

#' Generate Combined Stan Code
#' 
#' @description
#' Main function for two-stage Stan assembly. Combines observation and trend
#' models into a single Stan program with proper missing data handling.
#' 
#' @param obs_setup List containing brms setup for observation model
#' @param trend_setup List containing brms setup for trend model  
#' @param trend_spec List containing trend specification details
#' @param backend Character string specifying Stan backend ("rstan" or "cmdstanr")
#' @param validate Logical indicating whether to validate final Stan code
#' @param silent Numeric indicating verbosity level
#' @return List containing combined Stan code and data
#' @noRd
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
  all_stanvars <- c(obs_setup$stanvars, trend_stanvars)
  
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
    stop(insight::format_error(
      "Failed to generate base Stan code with trend stanvars.",
      "Check observation formula and trend stanvar compatibility.",
      "Error: {attr(base_code, 'condition')$message}"
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
  # Must be a list
  if (!is.list(stanvar)) {
    return(FALSE)
  }
  
  # Must have name and scode components
  required_components <- c("name", "scode")
  if (!all(required_components %in% names(stanvar))) {
    return(FALSE)
  }
  
  # Name must be non-empty string
  if (!is.character(stanvar$name) || length(stanvar$name) != 1 || nchar(stanvar$name) == 0) {
    return(FALSE)
  }
  
  # scode must be non-empty string
  if (!is.character(stanvar$scode) || length(stanvar$scode) != 1 || nchar(stanvar$scode) == 0) {
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
    
    if (is_valid_stanvar(stanvar)) {
      valid_stanvars[[name]] <- stanvar
    } else {
      insight::format_warning(
        "Skipping invalid stanvar: {.field {name}}",
        "Stanvar must have 'name' and 'scode' components."
      )
    }
  }
  
  return(valid_stanvars)
}

# Missing Integration Functions
# ============================

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
#' @param obs_code Character string containing observation model Stan code
#' @param obs_data List containing observation model data
#' @param trend_stanvars List of stanvar objects for trend models
#' @return List with combined stancode, standata, and has_trends flag
#' @noRd
combine_stan_components <- function(obs_code, obs_data, trend_stanvars) {
  checkmate::assert_string(obs_code, min.chars = 1)
  checkmate::assert_list(obs_data, names = "named")
  checkmate::assert_list(trend_stanvars)
  
  # Track whether trends were actually added
  has_trends <- length(trend_stanvars) > 0
  
  # Start with observation code and data
  combined_code <- obs_code
  combined_data <- obs_data
  
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

