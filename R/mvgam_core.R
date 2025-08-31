# ==============================================================================
# MVGAM CORE: Enhanced Model Fitting and Multiple Imputation Support
# ==============================================================================
# This file consolidates the core mvgam fitting architecture, dual-object
# system, and multiple imputation capabilities. The single-fit dual-object
# architecture enables seamless brms ecosystem integration while preserving
# mvgam-specific State-Space modeling functionality.

# The mvgam function provides unified entry point that handles both
# single datasets and multiple imputation scenarios transparently, ensuring
# consistent behavior across different input types.

#' mvgam Function with Single-Fit Architecture
#'
#' @description
#' mvgam implementation using single-fit dual-object architecture
#' with native multiple imputation support and brms ecosystem integration.
#'
#' @param formula Main observation model formula (supports brms syntax)
#' @param trend_formula Trend formula specification (may be response-specific)
#' @param data Data frame or list of multiply imputed datasets
#' @param backend Stan backend (defaults to "cmdstanr")
#' @param combine Logical, pool multiple imputation results (default TRUE)
#' @param family Family specification (supports all brms families)
#' @param ... Additional arguments passed to Stan fitting
#' @return mvgam object with dual brmsfit-like structure
#' @export
mvgam <- function(formula, trend_formula = NULL, data = NULL,
                           backend = getOption("brms.backend", "cmdstanr"),
                           combine = TRUE, family = poisson(), ...) {

  # Input validation
  checkmate::assert_formula(formula)
  checkmate::assert(
    checkmate::check_data_frame(data),
    checkmate::check_list(data, types = "data.frame"),
    .var.name = "data"
  )
  checkmate::assert_character(backend, len = 1)
  checkmate::assert_logical(combine, len = 1)

  # Handle multiple imputation input
  if (is.list(data) && !is.data.frame(data)) {
    if (combine) {
      return(mvgam_multiple(formula, trend_formula, data, backend,
                           combine = TRUE, ...))
    } else {
      return(mvgam_multiple(formula, trend_formula, data, backend,
                           combine = FALSE, ...))
    }
  }

  # Single dataset processing
  mvgam_object <- mvgam_single_dataset(
    formula = formula,
    trend_formula = trend_formula,
    data = data,
    backend = backend,
    family = family,
    ...
  )

  return(mvgam_object)
}

# ------------------------------------------------------------------------------
# SINGLE DATASET PROCESSING
# ------------------------------------------------------------------------------
# Core processing pipeline for single datasets using the two-stage Stan
# assembly system with brms ecosystem integration.

#' Process Single Dataset with Enhanced Architecture
#' @param formula Main formula
#' @param trend_formula Trend formula
#' @param data Single data frame
#' @param backend Stan backend
#' @param family Family specification
#' @param ... Additional arguments
#' @return mvgam object
#' @noRd
mvgam_single_dataset <- function(formula, trend_formula, data, backend,
                                family, ...) {

  # Parse multivariate trends and validate
  mv_spec <- parse_multivariate_trends(formula, trend_formula)

  # Validate formula compatibility with brms
  if (!is.null(trend_formula)) {
    validate_autocor_separation(formula, trend_formula)
  }

  # Setup observation model using lightweight brms
  obs_setup <- setup_brms_lightweight(
    formula = formula,
    data = data,
    family = family,
    ...
  )

  # Setup trend model if trends are specified
  trend_setup <- if (mv_spec$has_trends) {
    setup_brms_lightweight(
      formula = mv_spec$base_formula,
      data = data,
      family = gaussian(), # Trends are typically gaussian processes
      ...
    )
  } else {
    NULL
  }

  # Validate time series structure and inject dimensions if trends are specified
  if (mv_spec$has_trends) {
    # Extract response variable names for mapping generation using enhanced function
    response_vars <- extract_response_names(formula)
    
    validation_result <- validate_time_series_for_trends(
      data, 
      mv_spec$trend_specs,
      response_vars = response_vars
    )
    
    # Inject dimensions back into trend specifications for stanvar generation
    # Standardized structure: univariate=direct, multivariate=named list
    if (is_multivariate_trend_specs(mv_spec$trend_specs)) {
      # Multivariate: inject dimensions into each response-specific trend spec
      for (response_name in names(mv_spec$trend_specs)) {
        mv_spec$trend_specs[[response_name]]$dimensions <- validation_result$dimensions
      }
    } else {
      # Univariate: inject dimensions directly into trend object
      mv_spec$trend_specs$dimensions <- validation_result$dimensions
    }
  }

  # Note: Legacy trend stanvar extraction removed - modern system handles this automatically in generate_combined_stancode()

  # Generate combined Stan code and data using modern system
  combined_components <- generate_combined_stancode_and_data(
    obs_setup = obs_setup,
    trend_setup = trend_setup,
    mv_spec = mv_spec
  )

  # Fit the combined model
  combined_fit <- fit_mvgam_model(
    stancode = combined_components$stancode,
    standata = combined_components$standata,
    backend = backend,
    ...
  )

  # Create mvgam object from combined fit
  mvgam_object <- create_mvgam_from_combined_fit(
    combined_fit = combined_fit,
    obs_setup = obs_setup,
    trend_setup = trend_setup,
    mv_spec = mv_spec
  )

  return(mvgam_object)
}

# ------------------------------------------------------------------------------
# TREND STANVAR EXTRACTION
# ------------------------------------------------------------------------------
# Extracts and generates trend-specific stanvars to enable proper injection
# into the combined Stan model while maintaining compatibility with brms.

# Note: Legacy trend stanvar generation functions have been removed.
# The modern system in stan_assembly.R using generate_trend_injection_stanvars()
# provides the same functionality with better integration.

# ------------------------------------------------------------------------------
# COMBINED STAN CODE GENERATION
# ------------------------------------------------------------------------------
# Orchestrates the combination of observation and trend models into a single
# Stan program while maintaining separate parameterizations for ecosystem
# compatibility.

#' Generate Combined Stan Code and Data Using Modern System
#' @param obs_setup Observation model setup
#' @param trend_setup Trend model setup
#' @param mv_spec Multivariate specification
#' @return List with combined stancode and standata
#' @noRd
generate_combined_stancode_and_data <- function(obs_setup, trend_setup, mv_spec) {

  # Extract trend_specs from mv_spec for the new system
  trend_specs <- if (mv_spec$has_trends && !is.null(mv_spec$trend_specs)) {
    # Pass the complete trend_specs (handles both univariate and multivariate)
    mv_spec$trend_specs
  } else {
    NULL
  }

  # Use the enhanced two-stage assembly system from stan_assembly.R
  result <- generate_combined_stancode(
    obs_setup = obs_setup,
    trend_setup = trend_setup,
    trend_specs = trend_specs,  # Updated parameter name
    validate = TRUE,
    silent = 1
  )

  return(result)
}

# ------------------------------------------------------------------------------
# MODEL FITTING
# ------------------------------------------------------------------------------
# Orchestrates the actual Stan model fitting using the appropriate backend
# while maintaining compatibility with both rstan and cmdstanr.

#' Fit mvgam Model Using Stan
#' @param stancode Stan model code
#' @param standata Stan data list
#' @param backend Stan backend to use
#' @param ... Additional Stan fitting arguments
#' @return Stan fit object
#' @noRd
fit_mvgam_model <- function(stancode, standata, backend = "cmdstanr", ...) {

  # This is a placeholder for actual Stan fitting
  # Real implementation would use rstan or cmdstanr to fit the model
  stop("This function is not yet operational")
}

# ==============================================================================
# DUAL OBJECT SYSTEM: BRMSFIT-LIKE STRUCTURE FROM COMBINED FIT
# ==============================================================================
# Creates the dual-object mvgam structure that provides separate brmsfit-like
# interfaces for observation and trend components while maintaining ecosystem
# compatibility and enabling specialized plotting/prediction methods.

#' Create mvgam Object from Combined Stan Fit
#'
#' @description
#' Creates the dual-object mvgam structure from a single Stan fit containing
#' both observation and trend components. This enables seamless brms ecosystem
#' integration while preserving mvgam-specific functionality.
#'
#' @param combined_fit Stan fit object from combined model
#' @param obs_setup brms setup components for observation model
#' @param trend_setup brms setup components for trend model
#' @param mv_spec Multivariate trend specification from
#'   parse_multivariate_trends()
#' @return mvgam object with dual brmsfit-like structure
#' @noRd
create_mvgam_from_combined_fit <- function(combined_fit, obs_setup,
                                          trend_setup = NULL,
                                          mv_spec = NULL) {
  checkmate::assert_class(combined_fit, "stanfit")
  checkmate::assert_list(obs_setup, names = "named")
  checkmate::assert_list(trend_setup, names = "named", null.ok = TRUE)
  checkmate::assert_list(mv_spec, names = "named", null.ok = TRUE)

  # Create observation brmsfit-like object
  obs_fit <- create_observation_brmsfit(combined_fit, obs_setup, mv_spec)

  # Create trend brmsfit-like object (if trends exist)
  trend_fit <- if (!is.null(trend_setup) && !is.null(mv_spec$has_trends) &&
                  mv_spec$has_trends) {
    create_trend_brmsfit(combined_fit, trend_setup, mv_spec)
  } else {
    NULL
  }

  # Extract mvgam-specific components
  mvgam_components <- extract_mvgam_components(combined_fit, obs_setup,
                                              trend_setup, mv_spec)

  # Create main mvgam object
  mvgam_object <- structure(
    list(
      # Core components
      obs_fit = obs_fit,
      trend_fit = trend_fit,
      combined_fit = combined_fit,

      # Model specifications
      formula = obs_setup$formula,
      trend_formula = if (!is.null(trend_setup)) trend_setup$formula else NULL,
      family = obs_setup$family,

      # Data and setup
      data = obs_setup$data,
      stancode = obs_setup$stancode,
      standata = obs_setup$standata,

      # Multivariate specifications
      mv_spec = mv_spec,
      response_names = mv_spec$response_names %||% NULL,

      # mvgam-specific components
      trend_components = mvgam_components$trend_components,
      series_info = mvgam_components$series_info,
      time_info = mvgam_components$time_info,

      # Compatibility metadata
      brms_version = utils::packageVersion("brms"),
      mvgam_version = utils::packageVersion("mvgam"),
      creation_time = Sys.time()
    ),
    class = c("mvgam", "brmsfit")
  )

  # Add model comparison methods compatibility
  mvgam_object$criteria <- list()

  # Store call information
  mvgam_object$call <- match.call(sys.function(sys.parent()),
                                 sys.call(sys.parent()))

  return(mvgam_object)
}

# ------------------------------------------------------------------------------
# OBSERVATION BRMSFIT CREATION
# ------------------------------------------------------------------------------
# Creates brmsfit-like objects for observation model components, enabling
# full compatibility with brms ecosystem tools and methods.

#' Create Observation brmsfit-like Object
#' @param combined_fit Stan fit with combined model
#' @param obs_setup Observation model setup components
#' @param mv_spec Multivariate specification
#' @return brmsfit-like object for observations
#' @noRd
create_observation_brmsfit <- function(combined_fit, obs_setup, mv_spec) {
  # Extract observation-specific parameters from combined fit
  obs_params <- extract_observation_parameters(combined_fit)

  # Create brmsfit structure
  obs_brmsfit <- structure(
    list(
      # Stan fit with observation parameters only
      fit = subset_stanfit_parameters(combined_fit, obs_params$names),

      # Model components
      formula = obs_setup$formula,
      data = obs_setup$data,
      family = obs_setup$family,
      prior = obs_setup$prior,

      # Stan components (observation-focused)
      stancode = obs_setup$stancode,
      standata = obs_setup$standata,

      # Metadata
      algorithm = combined_fit@sim$algorithm %||% "sampling",
      backend = "cmdstanr", # Will be updated based on actual backend
      version = brms::brms_version()
    ),
    class = c("brmsfit")
  )

  return(obs_brmsfit)
}

# ------------------------------------------------------------------------------
# TREND BRMSFIT CREATION
# ------------------------------------------------------------------------------
# Creates brmsfit-like objects for trend model components, allowing specialized
# trend analysis while maintaining brms ecosystem compatibility.

#' Create Trend brmsfit-like Object
#' @param combined_fit Stan fit with combined model
#' @param trend_setup Trend model setup components
#' @param mv_spec Multivariate specification
#' @return brmsfit-like object for trends
#' @noRd
create_trend_brmsfit <- function(combined_fit, trend_setup, mv_spec) {
  # Extract trend-specific parameters from combined fit
  trend_params <- extract_trend_parameters(combined_fit, mv_spec)

  # Create brmsfit structure for trends
  trend_brmsfit <- structure(
    list(
      # Stan fit with trend parameters only
      fit = subset_stanfit_parameters(combined_fit, trend_params$names),

      # Model components
      formula = trend_setup$formula,
      data = trend_setup$data,
      family = gaussian(), # Trends are typically gaussian processes
      prior = trend_setup$prior,

      # Stan components (trend-focused)
      stancode = trend_setup$stancode,
      standata = trend_setup$standata,

      # Trend-specific metadata
      trend_specs = mv_spec$trend_specs,
      trend_types = trend_params$types,

      # Standard metadata
      algorithm = combined_fit@sim$algorithm %||% "sampling",
      backend = "cmdstanr",
      version = brms::brms_version()
    ),
    class = c("brmsfit")
  )

  return(trend_brmsfit)
}

# ------------------------------------------------------------------------------
# PARAMETER EXTRACTION
# ------------------------------------------------------------------------------
# Extracts and categorizes parameters from combined Stan fit to enable proper
# separation into observation and trend components for ecosystem compatibility.

#' Extract Observation Parameters from Combined Fit
#' @param combined_fit Stan fit object
#' @return List with observation parameter names and metadata
#' @noRd
extract_observation_parameters <- function(combined_fit) {
  all_params <- combined_fit@sim$pars_oi

  # Identify observation parameters (brms conventions)
  # NOTE: These parameter naming patterns are based on brms 2.x conventions.
  # brms 3.0 may introduce naming changes
  # (see: https://github.com/paul-buerkner/brms/milestone/20)
  # This extraction logic should be reviewed and updated when brms 3.0 is
  # released to ensure compatibility with any new parameter naming schemes.
  obs_patterns <- c(
    "^b_",          # Fixed effects
    "^sd_",         # Random effects standard deviations
    "^cor_",        # Correlations
    "^r_",          # Random effects
    "^s_",          # Smooth terms
    "^sds_",        # Smooth standard deviations
    "^sigma",       # Error terms
    "^shape",       # Shape parameters
    "^nu",          # Degrees of freedom
    "^phi",         # Dispersion parameters
    "lp__",         # Log posterior
    "lprior"        # Log prior
  )

  obs_params <- all_params[grepl(paste(obs_patterns, collapse = "|"),
                                all_params)]

  return(list(
    names = obs_params,
    count = length(obs_params)
  ))
}

#' Extract Trend Parameters from Combined Fit
#' @param combined_fit Stan fit object
#' @param mv_spec Multivariate specification
#' @return List with trend parameter names and metadata
#' @noRd
extract_trend_parameters <- function(combined_fit, mv_spec) {
  all_params <- combined_fit@sim$pars_oi

  # Identify trend parameters (mvgam conventions)
  trend_patterns <- c(
    "^trend",        # Trend states
    "^sigma_trend",  # Trend innovations
    "^ar_trend",     # AR coefficients
    "^A_trend",      # VAR coefficients
    "^theta_trend",  # MA coefficients
    "^mu_trend",     # Trend linear predictors
    "^L_trend",      # Cholesky factors for correlations
    "^rho_trend"     # Correlation parameters
  )

  trend_params <- all_params[grepl(paste(trend_patterns, collapse = "|"),
                                  all_params)]

  # Identify trend types from specifications
  trend_types <- if (!is.null(mv_spec$trend_specs)) {
    sapply(mv_spec$trend_specs, function(spec) {
      if (inherits(spec, "mvgam_trend")) {
        spec$trend_type
      } else {
        "unknown"
      }
    })
  } else {
    NULL
  }

  return(list(
    names = trend_params,
    count = length(trend_params),
    types = trend_types
  ))
}

#' Subset Stan Fit to Specific Parameters
#' @param stanfit Original Stan fit object
#' @param param_names Character vector of parameter names to keep
#' @return Stan fit object with subset of parameters
#' @noRd
subset_stanfit_parameters <- function(stanfit, param_names) {
  # This is a placeholder - actual implementation would require
  # creating a new stanfit object with subset of parameters
  stop("This function is not yet operational")
}

# ------------------------------------------------------------------------------
# COMPONENT EXTRACTION
# ------------------------------------------------------------------------------
# Extracts mvgam-specific metadata and information from the combined fit to
# enable specialized State-Space model functionality and analysis.

#' Extract mvgam-Specific Components
#' @param combined_fit Stan fit object
#' @param obs_setup Observation setup
#' @param trend_setup Trend setup
#' @param mv_spec Multivariate specification
#' @return List of mvgam-specific components
#' @noRd
extract_mvgam_components <- function(combined_fit, obs_setup, trend_setup,
                                    mv_spec) {
  # Extract time series information
  time_info <- extract_time_information(obs_setup$data)

  # Extract series information
  series_info <- extract_series_information(obs_setup$data, mv_spec)

  # Extract trend components if available
  trend_components <- if (!is.null(mv_spec$has_trends) &&
                         mv_spec$has_trends) {
    extract_trend_component_info(combined_fit, mv_spec)
  } else {
    NULL
  }

  return(list(
    time_info = time_info,
    series_info = series_info,
    trend_components = trend_components
  ))
}

#' Extract Time Information from Data
#' @param data Model data frame
#' @return List with time-related metadata
#' @noRd
extract_time_information <- function(data) {
  if ("time" %in% names(data)) {
    list(
      n_timepoints = length(unique(data$time)),
      time_range = range(data$time, na.rm = TRUE),
      time_spacing = diff(sort(unique(data$time)))[1],
      has_time = TRUE
    )
  } else {
    list(has_time = FALSE)
  }
}

#' Extract Series Information from Data
#' @param data Model data frame
#' @param mv_spec Multivariate specification
#' @return List with series-related metadata
#' @noRd
extract_series_information <- function(data, mv_spec) {
  series_info <- list()

  if ("series" %in% names(data)) {
    series_info$n_series <- length(unique(data$series))
    series_info$series_names <- unique(data$series)
    series_info$has_series = TRUE
  } else {
    series_info$has_series <- FALSE
  }

  # Add multivariate response information
  if (!is.null(mv_spec$response_names)) {
    series_info$response_names <- mv_spec$response_names
    series_info$n_responses <- length(mv_spec$response_names)
    series_info$is_multivariate <- TRUE
  } else {
    series_info$is_multivariate <- FALSE
  }

  return(series_info)
}

#' Extract Trend Component Information
#' @param combined_fit Stan fit object
#' @param mv_spec Multivariate specification
#' @return List with trend component metadata
#' @noRd
extract_trend_component_info <- function(combined_fit, mv_spec) {
  trend_info <- list()

  if (!is.null(mv_spec$trend_specs)) {
    trend_info$specifications <- mv_spec$trend_specs
    trend_info$n_trends <- length(mv_spec$trend_specs)

    # Extract trend types
    trend_info$types <- sapply(mv_spec$trend_specs, function(spec) {
      if (inherits(spec, "mvgam_trend")) {
        spec$trend_type
      } else {
        "custom"
      }
    })
  }

  return(trend_info)
}

# ==============================================================================
# MULTIPLE IMPUTATION SUPPORT: RUBIN'S RULES POOLING
# ==============================================================================
# Provides comprehensive multiple imputation support using Rubin's rules for
# proper uncertainty quantification when dealing with missing data in State-
# Space models, ensuring valid statistical inference.

#' Multiple Imputation Support for mvgam
#'
#' @description
#' Handles multiple imputation datasets and pools results using Rubin's rules.
#' Provides seamless integration with mvgam's State-Space modeling framework.
#'
#' @param formula Main observation model formula
#' @param trend_formula Trend formula specification
#' @param data_list List of multiply imputed datasets
#' @param backend Stan backend to use
#' @param combine Logical, whether to pool results using Rubin's rules
#' @param ... Additional arguments passed to mvgam()
#' @return mvgam_pooled object or list of individual fits
#' @noRd
mvgam_multiple <- function(formula, trend_formula = NULL, data_list,
                          backend = NULL, combine = TRUE, ...) {
  checkmate::assert_formula(formula)
  checkmate::assert_list(data_list, min.len = 2, types = "data.frame")
  checkmate::assert_logical(combine, len = 1)

  # Validate multiple imputation datasets
  validate_multiple_imputation_datasets(data_list)

  # Fit individual models to each imputed dataset
  individual_fits <- fit_multiple_imputation_models(
    formula = formula,
    trend_formula = trend_formula,
    data_list = data_list,
    backend = backend,
    ...
  )

  # Return combined or individual results
  if (combine) {
    pooled_fit <- pool_mvgam_fits(individual_fits)
    return(pooled_fit)
  } else {
    return(individual_fits)
  }
}

# ------------------------------------------------------------------------------
# DATASET VALIDATION
# ------------------------------------------------------------------------------
# Ensures that multiple imputation datasets meet requirements for valid
# statistical inference, including structural consistency and proper handling
# of time series identifiers.

#' Validate Multiple Imputation Datasets
#' @param data_list List of imputed datasets
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_multiple_imputation_datasets <- function(data_list) {
  checkmate::assert_list(data_list, min.len = 2)

  # Check all elements are data frames
  if (!all(sapply(data_list, is.data.frame))) {
    stop(insight::format_error(
      "All elements in data_list must be data.frames.",
      "Found non-data.frame elements in imputation list."
    ))
  }

  # Get reference structure from first dataset
  ref_data <- data_list[[1]]
  ref_names <- names(ref_data)
  ref_nrow <- nrow(ref_data)

  # Validate consistency across datasets
  for (i in seq_along(data_list)[-1]) {
    current_data <- data_list[[i]]

    # Check column names match
    if (!identical(names(current_data), ref_names)) {
      stop(insight::format_error(
        paste("Dataset", i, "has different column names than dataset 1."),
        "All imputed datasets must have identical structure."
      ))
    }

    # Check number of rows match
    if (nrow(current_data) != ref_nrow) {
      stop(insight::format_error(
        paste("Dataset", i, "has", nrow(current_data), "rows, expected",
             ref_nrow),
        "All imputed datasets must have same number of observations."
      ))
    }

    # Check essential columns (time, series) are identical
    essential_cols <- intersect(c("time", "series"), ref_names)
    for (col in essential_cols) {
      if (!identical(ref_data[[col]], current_data[[col]])) {
        stop(insight::format_error(
          paste("Column", col, "differs between datasets."),
          "Time and series identifiers must be identical across imputations."
        ))
      }
    }
  }

  # Validate missing data patterns
  validate_missing_patterns(data_list)

  invisible(TRUE)
}

#' Validate Missing Data Patterns
#' @param data_list List of imputed datasets
#' @return Invisible TRUE if valid, warns about potential issues
#' @noRd
validate_missing_patterns <- function(data_list) {
  n_datasets <- length(data_list)
  dataset_names <- names(data_list[[1]])

  # Check for variables that should not be imputed
  non_imputable <- c("time", "series", "weights", "trials")
  present_non_imputable <- intersect(non_imputable, dataset_names)

  for (col in present_non_imputable) {
    values_list <- lapply(data_list, function(d) d[[col]])

    # Check if any differences exist
    reference_values <- values_list[[1]]
    for (i in 2:n_datasets) {
      if (!identical(reference_values, values_list[[i]])) {
        insight::format_warning(
          paste("Column", col, "varies between imputed datasets."),
          "This may indicate improper imputation of structural variables.",
          .frequency = "once"
        )
      }
    }
  }

  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# INDIVIDUAL MODEL FITTING
# ------------------------------------------------------------------------------
# Fits separate mvgam models to each imputed dataset using consistent
# specifications to enable proper pooling of results.

#' Fit Models to Multiple Imputation Datasets
#' @param formula Main formula
#' @param trend_formula Trend formula
#' @param data_list List of datasets
#' @param backend Stan backend
#' @param ... Additional arguments
#' @return List of fitted mvgam objects
#' @noRd
fit_multiple_imputation_models <- function(formula, trend_formula, data_list,
                                          backend, ...) {
  n_datasets <- length(data_list)

  insight::format_message(
    paste("Fitting mvgam models to", n_datasets, "imputed datasets..."),
    "This may take some time depending on model complexity."
  )

  # Fit individual models
  fits <- vector("list", n_datasets)
  names(fits) <- paste0("imputation_", seq_len(n_datasets))

  for (i in seq_len(n_datasets)) {
    insight::format_message(
      paste("Fitting imputation", i, "of", n_datasets, "...")
    )

    # Fit model to current imputed dataset
    fits[[i]] <- mvgam_single_imputation(
      formula = formula,
      trend_formula = trend_formula,
      data = data_list[[i]],
      backend = backend,
      imputation_id = i,
      ...
    )

    # Add imputation metadata
    fits[[i]]$imputation_id <- i
    fits[[i]]$n_imputations <- n_datasets
  }

  return(fits)
}

#' Fit mvgam to Single Imputation Dataset
#' @param formula Main formula
#' @param trend_formula Trend formula
#' @param data Single imputed dataset
#' @param backend Stan backend
#' @param imputation_id ID of current imputation
#' @param ... Additional arguments
#' @return Single mvgam fit object
#' @noRd
mvgam_single_imputation <- function(formula, trend_formula, data, backend,
                                   imputation_id, ...) {
  # Parse multivariate trends
  mv_spec <- parse_multivariate_trends(formula, trend_formula)

  # Setup observation and trend models using lightweight brms
  obs_setup <- setup_brms_lightweight(formula, data, ...)

  trend_setup <- if (mv_spec$has_trends) {
    setup_brms_lightweight(mv_spec$base_formula, data,
                          family = gaussian(), ...)
  } else {
    NULL
  }

  # Generate combined Stan code and data
  combined_components <- generate_combined_stancode_and_data(obs_setup,
                                                           trend_setup,
                                                           mv_spec)

  # Fit the combined model
  combined_fit <- fit_mvgam_model(
    stancode = combined_components$stancode,
    standata = combined_components$standata,
    backend = backend,
    ...
  )

  # Create mvgam object from combined fit
  mvgam_object <- create_mvgam_from_combined_fit(
    combined_fit = combined_fit,
    obs_setup = obs_setup,
    trend_setup = trend_setup,
    mv_spec = mv_spec
  )

  return(mvgam_object)
}

# ------------------------------------------------------------------------------
# RUBIN'S RULES POOLING
# ------------------------------------------------------------------------------
# Implements proper multiple imputation pooling using Rubin's rules to combine
# parameter estimates while appropriately accounting for between-imputation
# variability in uncertainty quantification.

#' Pool mvgam Fits Using Rubin's Rules
#' @param fits List of individual mvgam fits
#' @return mvgam_pooled object with combined estimates
#' @noRd
pool_mvgam_fits <- function(fits) {
  checkmate::assert_list(fits, min.len = 2)

  # Validate all fits are mvgam objects
  if (!all(sapply(fits, function(x) inherits(x, "mvgam")))) {
    stop(insight::format_error(
      "All fits must be mvgam objects.",
      "Cannot pool fits of different types."
    ))
  }

  insight::format_message("Pooling results using Rubin's rules...")

  # Extract parameter estimates from each fit
  estimates_list <- lapply(fits, extract_fit_estimates)

  # Apply Rubin's rules pooling
  pooled_estimates <- apply_rubins_rules(estimates_list)

  # Create pooled mvgam object
  pooled_fit <- create_pooled_mvgam(fits[[1]], pooled_estimates)

  # Store individual fits and metadata
  attr(pooled_fit, "individual_fits") <- fits
  attr(pooled_fit, "n_imputations") <- length(fits)
  attr(pooled_fit, "pooling_method") <- "rubins_rules"
  attr(pooled_fit, "pooled_time") <- Sys.time()

  # Set appropriate class
  class(pooled_fit) <- c("mvgam_pooled", "mvgam", "brmsfit")

  return(pooled_fit)
}

#' Extract Parameter Estimates from mvgam Fit
#' @param fit Single mvgam fit object
#' @return List containing parameter estimates and uncertainties
#' @noRd
extract_fit_estimates <- function(fit) {
  # Extract posterior samples for key parameters
  obs_params <- extract_posterior_samples(fit$obs_fit)

  trend_params <- if (!is.null(fit$trend_fit)) {
    extract_posterior_samples(fit$trend_fit)
  } else {
    NULL
  }

  return(list(
    observation = obs_params,
    trend = trend_params,
    combined = extract_posterior_samples(fit$combined_fit)
  ))
}

#' Apply Rubin's Rules for Multiple Imputation Pooling
#' @param estimates_list List of parameter estimates from each imputation
#' @return List containing pooled estimates with proper uncertainty
#' @noRd
apply_rubins_rules <- function(estimates_list) {
  n_imputations <- length(estimates_list)

  # Get parameter names from first estimation
  param_structure <- estimates_list[[1]]

  pooled_results <- list()

  # Pool each parameter type
  for (param_type in names(param_structure)) {
    if (!is.null(param_structure[[param_type]])) {
      param_estimates <- lapply(estimates_list, function(x) x[[param_type]])
      pooled_results[[param_type]] <- pool_parameter_estimates(param_estimates)
    }
  }

  return(pooled_results)
}

#' Pool Parameter Estimates Using Rubin's Rules
#' @param param_list List of parameter estimates across imputations
#' @return List with pooled mean, variance, and diagnostics
#' @noRd
pool_parameter_estimates <- function(param_list) {
  n_imputations <- length(param_list)

  # Calculate means across imputations
  means <- lapply(param_list, function(x) apply(x, 2, mean))
  pooled_mean <- Reduce("+", means) / n_imputations

  # Calculate within-imputation variance
  within_var <- lapply(param_list, function(x) apply(x, 2, var))
  within_variance <- Reduce("+", within_var) / n_imputations

  # Calculate between-imputation variance
  mean_diffs <- lapply(means, function(m) m - pooled_mean)
  between_variance <- Reduce("+", lapply(mean_diffs, function(d) d^2)) /
    (n_imputations - 1)

  # Total variance using Rubin's rules
  total_variance <- within_variance + (1 + 1/n_imputations) * between_variance

  # Calculate degrees of freedom and other diagnostics
  relative_increase <- (1 + 1/n_imputations) * between_variance /
    within_variance
  degrees_freedom <- (n_imputations - 1) *
    (1 + within_variance / ((1 + 1/n_imputations) * between_variance))^2

  return(list(
    mean = pooled_mean,
    variance = total_variance,
    se = sqrt(total_variance),
    within_variance = within_variance,
    between_variance = between_variance,
    relative_increase = relative_increase,
    degrees_freedom = degrees_freedom,
    n_imputations = n_imputations
  ))
}

#' Create Pooled mvgam Object
#' @param template_fit Template mvgam object to base structure on
#' @param pooled_estimates Pooled parameter estimates
#' @return mvgam_pooled object
#' @noRd
create_pooled_mvgam <- function(template_fit, pooled_estimates) {
  # Start with template structure
  pooled_fit <- template_fit

  # Replace parameter estimates with pooled versions
  pooled_fit$pooled_estimates <- pooled_estimates

  # Update metadata
  pooled_fit$pooled <- TRUE
  pooled_fit$pooling_diagnostics <- extract_pooling_diagnostics(pooled_estimates)

  return(pooled_fit)
}

#' Extract Pooling Diagnostics
#' @param pooled_estimates Pooled parameter estimates
#' @return List of pooling diagnostic information
#' @noRd
extract_pooling_diagnostics <- function(pooled_estimates) {
  diagnostics <- list()

  for (param_type in names(pooled_estimates)) {
    if (!is.null(pooled_estimates[[param_type]])) {
      est <- pooled_estimates[[param_type]]

      diagnostics[[param_type]] <- list(
        n_imputations = est$n_imputations,
        avg_relative_increase = mean(est$relative_increase, na.rm = TRUE),
        avg_degrees_freedom = mean(est$degrees_freedom, na.rm = TRUE),
        fraction_missing_info = est$relative_increase /
          (1 + est$relative_increase)
      )
    }
  }

  return(diagnostics)
}
