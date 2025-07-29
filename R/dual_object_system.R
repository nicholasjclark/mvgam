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
#' @param mv_spec Multivariate trend specification from parse_multivariate_trends()
#' @return mvgam object with dual brmsfit-like structure
#' @noRd
create_mvgam_from_combined_fit <- function(combined_fit, obs_setup, trend_setup = NULL, 
                                           mv_spec = NULL) {
  checkmate::assert_class(combined_fit, "stanfit")
  checkmate::assert_list(obs_setup, names = "named")
  checkmate::assert_list(trend_setup, names = "named", null.ok = TRUE)
  checkmate::assert_list(mv_spec, names = "named", null.ok = TRUE)
  
  # Create observation brmsfit-like object
  obs_fit <- create_observation_brmsfit(combined_fit, obs_setup, mv_spec)
  
  # Create trend brmsfit-like object (if trends exist)
  trend_fit <- if (!is.null(trend_setup) && !is.null(mv_spec$has_trends) && mv_spec$has_trends) {
    create_trend_brmsfit(combined_fit, trend_setup, mv_spec)
  } else {
    NULL
  }
  
  # Extract mvgam-specific components
  mvgam_components <- extract_mvgam_components(combined_fit, obs_setup, trend_setup, mv_spec)
  
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
  mvgam_object$call <- match.call(sys.function(sys.parent()), sys.call(sys.parent()))
  
  return(mvgam_object)
}

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

#' Extract Observation Parameters from Combined Fit
#' @param combined_fit Stan fit object
#' @return List with observation parameter names and metadata
#' @noRd
extract_observation_parameters <- function(combined_fit) {
  all_params <- combined_fit@sim$pars_oi
  
  # Identify observation parameters (brms conventions)
  # NOTE: These parameter naming patterns are based on brms 2.x conventions.
  # brms 3.0 may introduce naming changes (see: https://github.com/paul-buerkner/brms/milestone/20)
  # This extraction logic should be reviewed and updated when brms 3.0 is released
  # to ensure compatibility with any new parameter naming schemes.
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
  
  obs_params <- all_params[grepl(paste(obs_patterns, collapse = "|"), all_params)]
  
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
    "^phi_trend",    # AR/VAR coefficients
    "^mu_trend",     # Trend linear predictors
    "^L_trend",      # Cholesky factors for correlations
    "^rho_trend"     # Correlation parameters
  )
  
  trend_params <- all_params[grepl(paste(trend_patterns, collapse = "|"), all_params)]
  
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
  # For now, return the original fit with metadata about subset
  
  subset_fit <- stanfit
  attr(subset_fit, "mvgam_subset") <- param_names
  
  return(subset_fit)
}

#' Extract mvgam-Specific Components
#' @param combined_fit Stan fit object
#' @param obs_setup Observation setup
#' @param trend_setup Trend setup
#' @param mv_spec Multivariate specification
#' @return List of mvgam-specific components
#' @noRd
extract_mvgam_components <- function(combined_fit, obs_setup, trend_setup, mv_spec) {
  # Extract time series information
  time_info <- extract_time_information(obs_setup$data)
  
  # Extract series information
  series_info <- extract_series_information(obs_setup$data, mv_spec)
  
  # Extract trend components if available
  trend_components <- if (!is.null(mv_spec$has_trends) && mv_spec$has_trends) {
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