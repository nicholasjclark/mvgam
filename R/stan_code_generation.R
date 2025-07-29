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

