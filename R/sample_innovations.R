#' Process Error Innovation Sampling for Trend Models
#'
#' @description
#' Infrastructure for sampling process errors (innovations) from trend
#' model distributions. All mvgam trend models use multivariate normal
#' innovations with different covariance parameterizations.
#'
#' @details
#' This module implements a pattern-based sampling system where trend
#' types are mapped to one of four covariance patterns:
#'
#' \itemize{
#'   \item \code{none}: Deterministic trends (PW) - no innovations
#'   \item \code{diagonal}: Independent innovations per series (CAR)
#'   \item \code{cholesky_scaled}: Correlated via L_Sigma =
#'     diag(sigma) * L_Omega (RW, AR, ZMVN)
#'   \item \code{full_covariance}: Direct covariance matrix Sigma (VAR)
#' }
#'
#' New trend types only need to declare their covariance pattern to
#' integrate with this sampling infrastructure.
#'
#' @name sample_innovations
#' @keywords internal
NULL


#' Covariance Pattern Constants
#'
#' Maps trend types to their covariance parameterization pattern.
#' New trend types should be added here with their appropriate pattern.
#'
#' @noRd
trend_covariance_patterns <- list(
  # Cholesky-scaled patterns: L_Sigma = diag(sigma_trend) %*% L_Omega_trend
  RW = "cholesky_scaled",
  AR = "cholesky_scaled",
  ZMVN = "cholesky_scaled",

  # Full covariance patterns: direct Sigma_trend matrix
  VAR = "full_covariance",

  # Diagonal patterns: independent innovations, sigma_trend only
  CAR = "diagonal",

  # No innovations: deterministic trends
  PW = "none",
  None = "none"
)


#' Get Covariance Pattern for Trend Type
#'
#' Retrieves the covariance pattern associated with a trend type.
#' Falls back to "cholesky_scaled" for unknown types (most common).
#'
#' @param trend_type Character string specifying the trend type
#'   (e.g., "AR", "RW", "VAR", "ZMVN", "CAR", "PW")
#'
#' @return Character string: one of "none", "diagonal",
#'   "cholesky_scaled", or "full_covariance"
#'
#' @noRd
get_covariance_pattern <- function(trend_type) {
  checkmate::assert_string(trend_type, min.chars = 1)

  # Normalize trend type (handle variations like "AR1", "VAR2")
  base_type <- gsub("[0-9]+$", "", toupper(trend_type))

  pattern <- trend_covariance_patterns[[base_type]]

  if (is.null(pattern)) {
    # Default to cholesky_scaled for unknown types (most common pattern)
    if (!identical(Sys.getenv("TESTTHAT"), "true")) {
      rlang::warn(
        insight::format_warning(
          c(
            paste0("Unknown trend type '", trend_type, "' encountered."),
            i = "Defaulting to 'cholesky_scaled' covariance pattern.",
            i = "Register custom trends in trend_covariance_patterns."
          )
        ),
        .frequency = "once",
        .frequency_id = paste0("unknown_trend_cov_", trend_type)
      )
    }
    pattern <- "cholesky_scaled"
  }

  pattern
}


#' Get Observation Structure for Innovation Sampling
#'
#' Extracts time and series indices for each observation in newdata.
#' Uses existing mvgam infrastructure for handling all series creation
#' strategies (explicit, hierarchical, multivariate).
#'
#' @param object mvgam object with fitted trend model
#' @param newdata Data frame with prediction covariates. If NULL, uses
#'   training data from object.
#'
#' @return List with components:
#'   \itemize{
#'     \item \code{time}: Integer vector of time indices (1-based)
#'     \item \code{series}: Factor/character vector of series identifiers
#'     \item \code{series_int}: Integer vector of series indices (1-based)
#'     \item \code{n_obs}: Number of observations
#'     \item \code{n_times}: Number of unique time points
#'     \item \code{n_series}: Number of unique series
#'     \item \code{unique_times}: Sorted unique time indices
#'   }
#'
#' @noRd
get_observation_structure <- function(object, newdata = NULL) {
  checkmate::assert_class(object, "mvgam")

  # Use training data if newdata not provided
  if (is.null(newdata)) {
    newdata <- object$obs_data
    if (is.null(newdata)) {
      newdata <- object$data
    }
  }

  checkmate::assert_data_frame(newdata, min.rows = 1)

  # Get variable names from stored metadata
  metadata <- object$trend_metadata

  if (is.null(metadata)) {
    # Fallback for models without trend_metadata
    time_var <- "time"
    series_var <- "series"
  } else {
    time_var <- metadata$variables$time_var
    if (is.null(time_var)) time_var <- "time"
    series_var <- metadata$variables$series_var
    if (is.null(series_var)) series_var <- "series"
  }

  # Prepare data with standardized time/series attributes
  # Handles all series creation strategies
  data_prepared <- ensure_mvgam_variables(
    data = newdata,
    parsed_trend = NULL,
    time_var = time_var,
    series_var = series_var,
    response_vars = NULL,
    metadata = metadata
  )

  # Validate that data preparation succeeded
  checkmate::assert_data_frame(data_prepared, min.rows = 1)
  if (is.null(attr(data_prepared, "mvgam_time"))) {
    stop(insight::format_error(
      c(
        "Failed to prepare data with mvgam time attributes.",
        i = paste0("Check that {.field ", time_var, "} exists in data.")
      )
    ))
  }
  if (is.null(attr(data_prepared, "mvgam_series"))) {
    stop(insight::format_error(
      c(
        "Failed to prepare data with mvgam series attributes.",
        i = paste0("Check that {.field ", series_var, "} exists in data.")
      )
    ))
  }

  # Extract indices using existing accessor functions
  time_indices <- get_time_for_grouping(data_prepared)
  series_indices <- get_series_for_grouping(data_prepared)

  # Convert series to integer indices for matrix operations
  if (is.factor(series_indices)) {
    series_int <- as.integer(series_indices)
    series_levels <- levels(series_indices)
  } else {
    series_factor <- as.factor(series_indices)
    series_int <- as.integer(series_factor)
    series_levels <- levels(series_factor)
  }

  unique_times <- sort(unique(time_indices))

  list(
    time = time_indices,
    series = series_indices,
    series_int = series_int,
    series_levels = series_levels,
    n_obs = nrow(newdata),
    n_times = length(unique_times),
    n_series = length(series_levels),
    unique_times = unique_times
  )
}


#' Get Trend Type from mvgam Object
#'
#' Extracts the base trend type from a fitted mvgam object.
#'
#' @param object mvgam object with fitted trend model
#'
#' @return Character string with trend type (e.g., "AR", "RW", "VAR"),
#'   or "None" if no trend model present.
#'
#' @noRd
get_trend_type <- function(object) {
  checkmate::assert_class(object, "mvgam")

  trend_model <- object$trend_model

  if (is.null(trend_model)) {
    return("None")
  }

  # Check for trend field (standard mvgam_trend objects)
  if (!is.null(trend_model$trend)) {
    return(trend_model$trend)
  }

  # Check for trend_type field (alternative naming)
  if (!is.null(trend_model$trend_type)) {
    return(trend_model$trend_type)
  }

  # Check for class-based identification
  if (inherits(trend_model, "mvgam_trend")) {
    # Extract from class name if available
    classes <- class(trend_model)
    trend_classes <- grep("^[A-Z]+$", classes, value = TRUE)
    if (length(trend_classes) > 0) {
      return(trend_classes[1])
    }
  }

  # Default to None if trend type cannot be determined
  "None"
}


#' Check if Model Has Stochastic Trend
#'
#' Determines if the model has a trend component that produces
#' stochastic innovations (i.e., not PW or None).
#'
#' @param object mvgam object
#'
#' @return Logical indicating if model has stochastic innovations
#'
#' @noRd
has_stochastic_trend <- function(object) {
  trend_type <- get_trend_type(object)
  pattern <- get_covariance_pattern(trend_type)
  pattern != "none"
}
