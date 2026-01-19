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
#' @details
#' Lookup order:
#' 1. `trend_components$types[1]` - primary source used by summary/print
#' 2. `trend_metadata$trend$trend_type` - fallback metadata source
#' 3. `trend_formula` existence check - issues warning if type unclear
#' 4. Returns "None" if no trend model detected
#'
#' @noRd
get_trend_type <- function(object) {
  checkmate::assert_class(object, "mvgam")

  # Primary source: trend_components$types (used by summary/print)
  trend_comps <- object$trend_components
  if (!is.null(trend_comps) && !is.null(trend_comps$types)) {
    trend_type <- trend_comps$types[1]
    if (!is.null(trend_type) && !is.na(trend_type)) {
      return(trend_type)
    }
  }

  # Secondary source: trend_metadata$trend$trend_type
  metadata <- object$trend_metadata
  if (!is.null(metadata) && !is.null(metadata$trend)) {
    trend_type <- metadata$trend$trend_type
    if (!is.null(trend_type) && !is.na(trend_type)) {
      return(trend_type)
    }
  }

  # Tertiary: check trend_formula existence (indicates trends present)
  if (!is.null(object$trend_formula)) {
    if (!identical(Sys.getenv("TESTTHAT"), "true")) {
      rlang::warn(
        insight::format_warning(
          c(
            "Could not determine trend type from object structure.",
            i = paste0(
              "Trend formula present but type not stored in ",
              "'trend_components' or 'trend_metadata'."
            )
          )
        ),
        .frequency = "once",
        .frequency_id = "unknown_trend_type"
      )
    }
  }

  # Default to None if no trend model present
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


#' Required Parameter Names by Covariance Pattern
#'
#' Defines which posterior parameters are needed for each pattern type.
#'
#' @noRd
covariance_param_specs <- list(
  cholesky_scaled = list(
    simple = c("sigma_trend", "L_Omega_trend"),
    hierarchical = c("L_Omega_global_trend", "L_deviation_group_trend",
                     "alpha_cor_trend", "sigma_group_trend")
  ),
  full_covariance = list(
    simple = c("Sigma_trend"),
    hierarchical = c("Sigma_group_trend")
  ),
  diagonal = list(
    simple = c("sigma_trend"),
    hierarchical = c("sigma_group_trend")
  ),
  none = list(simple = character(0), hierarchical = character(0))
)


#' Get Trend Covariance Structure from Posterior
#'
#' Extracts covariance parameters from posterior draws based on the
#' trend model's covariance pattern. Uses trend_metadata as ground truth
#' for structure determination.
#'
#' @param object mvgam object with fitted trend model
#' @param ndraws Number of posterior draws to extract (NULL = all).
#'   Mutually exclusive with draw_ids.
#' @param draw_ids Specific draw indices to use. Mutually exclusive
#'   with ndraws.
#'
#' @return List with components:
#'   \itemize{
#'     \item \code{pattern}: Covariance pattern type
#'     \item \code{n_series}: Number of series
#'     \item \code{hierarchical}: Whether model uses hierarchical structure
#'     \item \code{has_correlations}: Whether cor=TRUE in trend constructor
#'     \item \code{ndraws}: Number of posterior draws extracted
#'     \item \code{params}: List of extracted parameter matrices
#'     \item \code{group_info}: Group structure (if hierarchical)
#'   }
#'   Cholesky factors kept as vectors for memory efficiency; use
#'   cholesky_to_matrix() to reconstruct.
#'
#' @noRd
get_trend_covariance_structure <- function(object, ndraws = NULL,
                                           draw_ids = NULL) {
  validate_covariance_inputs(object, ndraws, draw_ids)

  metadata <- object$trend_metadata
  if (is.null(metadata)) {
    stop(insight::format_error(
      c(
        "trend_metadata required for covariance parameter extraction.",
        i = "This object may have been created with an older mvgam version."
      )
    ))
  }

  trend_type <- get_trend_type(object)
  pattern <- get_covariance_pattern(trend_type)

  specs <- object$trend_components$specifications
  has_correlations <- if (inherits(specs, "mvgam_trend")) {
    isTRUE(specs$cor)
  } else {
    isTRUE(specs[[1]]$cor)
  }

  if (pattern == "none") {
    return(list(
      pattern = "none",
      n_series = 0L,
      hierarchical = FALSE,
      has_correlations = FALSE,
      ndraws = 0L,
      params = list(),
      group_info = NULL
    ))
  }

  n_series <- object$series_info$n_series %||% object$trend_components$n_trends
  hierarchical <- !is.null(metadata$variables$gr_var) &&
                  !is.na(metadata$variables$gr_var) &&
                  metadata$variables$gr_var != "NA"

  draws_mat <- posterior::as_draws_matrix(object$fit)
  draw_indices <- resolve_draw_indices(nrow(draws_mat), ndraws, draw_ids)
  draws_mat <- draws_mat[draw_indices, , drop = FALSE]

  effective_pattern <- pattern
  if (pattern == "cholesky_scaled" && !has_correlations) {
    effective_pattern <- "diagonal"
  }

  param_names <- covariance_param_specs[[effective_pattern]][[
    if (hierarchical) "hierarchical" else "simple"
  ]]
  params <- extract_named_params(draws_mat, param_names)

  list(
    pattern = pattern,
    n_series = as.integer(n_series),
    hierarchical = hierarchical,
    has_correlations = has_correlations,
    ndraws = length(draw_indices),
    params = params,
    group_info = if (hierarchical) get_group_info(object$standata) else NULL
  )
}


#' @noRd
validate_covariance_inputs <- function(object, ndraws, draw_ids) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_int(ndraws, lower = 1, null.ok = TRUE)
  checkmate::assert_integerish(draw_ids, lower = 1, null.ok = TRUE)

  if (!is.null(ndraws) && !is.null(draw_ids)) {
    stop(insight::format_error(
      "Cannot specify both 'ndraws' and 'draw_ids'."
    ))
  }

  if (is.null(object$fit)) {
    stop(insight::format_error(
      "mvgam object has no fitted posterior draws."
    ))
  }

  invisible(NULL)
}


#' @noRd
resolve_draw_indices <- function(total_draws, ndraws, draw_ids) {
  if (!is.null(draw_ids)) {
    if (max(draw_ids) > total_draws) {
      stop(insight::format_error(c(
        "Requested 'draw_ids' exceed available draws.",
        x = paste0("Max requested: ", max(draw_ids),
                   ", available: ", total_draws, ".")
      )))
    }
    return(as.integer(draw_ids))
  }

  if (!is.null(ndraws)) {
    if (ndraws > total_draws) {
      stop(insight::format_error(c(
        "Requested 'ndraws' exceeds available draws.",
        x = paste0("Requested: ", ndraws, ", available: ", total_draws, ".")
      )))
    }
    return(seq_len(ndraws))
  }

  seq_len(total_draws)
}


#' @noRd
extract_named_params <- function(draws_mat, param_names) {
  all_cols <- colnames(draws_mat)
  params <- lapply(param_names, function(name) {
    extract_posterior_param(draws_mat, all_cols, name)
  })
  names(params) <- param_names
  params
}


#' @noRd
extract_posterior_param <- function(draws_mat, all_cols, param_name) {
  # Match indexed params (param[...]) or scalar (param)
  indexed_pattern <- paste0("^", param_name, "\\[")
  scalar_pattern <- paste0("^", param_name, "$")

  indexed_cols <- grep(indexed_pattern, all_cols, value = TRUE)

  if (length(indexed_cols) > 0) {
    # Sort by numeric index for consistent ordering
    idx_nums <- as.integer(gsub(".*\\[(\\d+).*", "\\1", indexed_cols))
    indexed_cols <- indexed_cols[order(idx_nums)]
    return(as.matrix(draws_mat[, indexed_cols, drop = FALSE]))
  }

  scalar_cols <- grep(scalar_pattern, all_cols, value = TRUE)
  if (length(scalar_cols) > 0) {
    return(as.matrix(draws_mat[, scalar_cols[1], drop = FALSE]))
  }

  stop(insight::format_error(c(
    paste0("Parameter '", param_name, "' not found in posterior."),
    i = "Check that model was fitted with expected covariance structure."
  )))
}


#' @noRd
get_group_info <- function(standata) {
  list(
    n_groups = standata$N_groups_trend,
    n_subgroups = standata$N_subgroups_trend,
    group_inds = standata$group_inds_trend
  )
}


#' Convert Cholesky Vector to Matrix
#'
#' Reconstructs a lower-triangular Cholesky factor matrix from
#' Stan's column-major vector storage.
#'
#' @param chol_vec Numeric vector of Cholesky elements
#' @param dim Dimension of the square matrix
#' @return Lower-triangular matrix
#'
#' @noRd
cholesky_to_matrix <- function(chol_vec, dim) {
  L <- matrix(0, nrow = dim, ncol = dim)
  L[lower.tri(L, diag = TRUE)] <- chol_vec
  L
}


# ============================================================================
# Pattern-Specific Innovation Samplers
# ============================================================================
#
# All trend models sample from MVN with different covariance
# parameterizations. These functions transform standard normal samples
# z ~ N(0,I) into innovations with the appropriate covariance structure.
#
# Design: Vectorize where possible, minimize memory allocations.
# Per-draw loops only when unavoidable (different covariance per draw).


#' Sample Innovations from Trend Model
#'
#' Main entry point for innovation sampling. Dispatches to pattern-specific
#' transform functions based on covariance structure.
#'
#' @param cov_structure List from `get_trend_covariance_structure()`
#'   containing: pattern, n_series, ndraws, params, has_correlations.
#' @param obs_structure List from `get_observation_structure()` containing:
#'   time, series_int, n_obs, n_times, n_series, unique_times.
#'
#' @return Matrix `[ndraws x n_obs]` of sampled innovations, where each
#'   observation gets the innovation for its (time, series) combination.
#'
#' @details
#' Algorithm:
#' 1. Generate standard normals for (time, series) grid
#' 2. Transform by covariance pattern
#' 3. Map grid innovations to observations via linear indexing
#'
#' @noRd
sample_innovations <- function(cov_structure, obs_structure) {
  # Validate input structures
  checkmate::assert_list(cov_structure, min.len = 1)
  checkmate::assert_names(
    names(cov_structure),
    must.include = c("pattern", "ndraws", "n_series", "params")
  )
  checkmate::assert_list(obs_structure, min.len = 1)
  checkmate::assert_names(
    names(obs_structure),
    must.include = c("n_obs", "n_times", "series_int", "time", "unique_times")
  )

  pattern <- cov_structure$pattern
  ndraws <- cov_structure$ndraws

  checkmate::assert_string(pattern)
  checkmate::assert_int(ndraws, lower = 0)

  # Return zeros for deterministic trends (pattern="none") or when
  # no draws requested (ndraws=0, which can occur during validation)
  if (pattern == "none" || ndraws == 0L) {
    effective_ndraws <- if (ndraws == 0L) 1L else ndraws
    return(matrix(0, effective_ndraws, obs_structure$n_obs))
  }

  n_times <- obs_structure$n_times
  n_series <- cov_structure$n_series

  checkmate::assert_int(n_times, lower = 1)
  checkmate::assert_int(n_series, lower = 1)

  # When cor=FALSE, use diagonal path (independent innovations per series)
  effective_pattern <- pattern
  if (pattern == "cholesky_scaled" && !isTRUE(cov_structure$has_correlations)) {
    effective_pattern <- "diagonal"
  }

  # Generate standard normals for entire (time, series) grid
  z <- matrix(
    rnorm(ndraws * n_times * n_series),
    ndraws,
    n_times * n_series
  )

  # Transform by covariance pattern
  innovations_flat <- switch(
    effective_pattern,
    diagonal = transform_diagonal_innovations(
      z, cov_structure$params, n_times, n_series, ndraws
    ),
    cholesky_scaled = transform_cholesky_innovations(
      z, cov_structure$params, n_times, n_series, ndraws
    ),
    full_covariance = transform_full_cov_innovations(
      z, cov_structure$params, n_times, n_series, ndraws
    ),
    stop(insight::format_error(
      paste0("Unknown covariance pattern: '", effective_pattern, "'.")
    ))
  )

  # Map (time, series) grid to observations
  map_innovations_to_obs(innovations_flat, n_times, n_series, obs_structure)
}


#' Transform Innovations: Diagonal Pattern
#'
#' For models with independent innovations per series (CAR, or RW/AR/ZMVN
#' with cor=FALSE). Vectorized implementation without per-draw loops.
#'
#' @param z Matrix `[ndraws x (n_times * n_series)]` of standard normals
#' @param params List with `sigma_trend` matrix `[ndraws x n_series]`
#' @param n_times Number of unique time points
#' @param n_series Number of series
#' @param ndraws Number of posterior draws
#'
#' @return Matrix `[ndraws x (n_times * n_series)]` of transformed innovations
#'
#' @details
#' Layout: columns ordered as (t1_s1, t2_s1, ..., tT_s1, t1_s2, ..., tT_sS).
#' Each series block of n_times columns gets scaled by that series' sigma.
#'
#' @noRd
transform_diagonal_innovations <- function(z, params, n_times, n_series,
                                           ndraws) {
  checkmate::assert_matrix(z, nrows = ndraws, ncols = n_times * n_series)
  checkmate::assert_int(n_times, lower = 1)
  checkmate::assert_int(n_series, lower = 1)
  checkmate::assert_int(ndraws, lower = 1)

  sigma <- params$sigma_trend
  checkmate::assert_matrix(sigma, nrows = ndraws, ncols = n_series)

  # Create column indices: each series index repeated n_times
  # Result: (1,1,...,1, 2,2,...,2, ..., S,S,...,S) with n_times repetitions
  sigma_expanded <- sigma[
    ,
    rep(seq_len(n_series), each = n_times),
    drop = FALSE
  ]

  z * sigma_expanded
}


#' Transform Innovations: Cholesky-Scaled Pattern
#'
#' For correlated innovations via L_Sigma = diag(sigma) %*% L_Omega.
#' Requires per-draw loop because covariance matrices vary across draws.
#'
#' @param z Matrix `[ndraws x (n_times * n_series)]` of standard normals
#' @param params List with:
#'   - `sigma_trend`: matrix `[ndraws x n_series]` of innovation SDs
#'   - `L_Omega_trend`: matrix `[ndraws x n_chol_elements]` of Cholesky
#'     factors (lower tri, column-major)
#' @param n_times Number of unique time points
#' @param n_series Number of series
#' @param ndraws Number of posterior draws
#'
#' @return Matrix `[ndraws x (n_times * n_series)]` of transformed innovations
#'
#' @details
#' For each draw d:
#' 1. Reconstruct L_Omega from vector storage
#' 2. Compute L_Sigma = diag(sigma) %*% L_Omega via row scaling
#' 3. Transform: innovations = z %*% t(L_Sigma)
#'
#' @noRd
transform_cholesky_innovations <- function(z, params, n_times, n_series,
                                           ndraws) {
  checkmate::assert_matrix(z, nrows = ndraws, ncols = n_times * n_series)
  checkmate::assert_int(n_times, lower = 1)
  checkmate::assert_int(n_series, lower = 1)
  checkmate::assert_int(ndraws, lower = 1)

  sigma <- params$sigma_trend
  L_omega_vec <- params$L_Omega_trend

  checkmate::assert_matrix(sigma, nrows = ndraws, ncols = n_series)
  n_chol_elements <- n_series * (n_series + 1) / 2
  checkmate::assert_matrix(
    L_omega_vec,
    nrows = ndraws,
    ncols = n_chol_elements
  )

  # Pre-allocate result matrix
  result <- matrix(0, ndraws, n_times * n_series)

  for (d in seq_len(ndraws)) {
    # Reconstruct L_Omega (correlation Cholesky)
    L_omega <- cholesky_to_matrix(L_omega_vec[d, ], n_series)

    # L_Sigma = diag(sigma) %*% L_Omega
    # Multiply each row i of L_omega by sigma[i] to avoid
    # constructing the diagonal matrix explicitly
    L_sigma <- L_omega * sigma[d, ]

    # Reshape z for this draw: [n_times x n_series]
    z_d <- matrix(z[d, ], n_times, n_series, byrow = FALSE)

    # Transform: x = z %*% t(L) gives MVN with cov L %*% t(L)
    result[d, ] <- as.vector(z_d %*% t(L_sigma))
  }

  result
}


#' Transform Innovations: Full Covariance Pattern
#'
#' For VAR models with direct Sigma matrix. Requires Cholesky decomposition
#' per draw, which is O(n^3) where n = n_series.
#'
#' @param z Matrix `[ndraws x (n_times * n_series)]` of standard normals
#' @param params List with `Sigma_trend`: matrix `[ndraws x n_series^2]`
#'   containing flattened covariance matrices (column-major)
#' @param n_times Number of unique time points
#' @param n_series Number of series
#' @param ndraws Number of posterior draws
#'
#' @return Matrix `[ndraws x (n_times * n_series)]` of transformed innovations
#'
#' @details
#' For each draw d:
#' 1. Reconstruct Sigma from vector storage
#' 2. Compute L = chol(Sigma) using R's LAPACK-based implementation
#' 3. Transform: innovations = z %*% t(L)
#'
#' @noRd
transform_full_cov_innovations <- function(z, params, n_times, n_series,
                                           ndraws) {
  checkmate::assert_matrix(z, nrows = ndraws, ncols = n_times * n_series)
  checkmate::assert_int(n_times, lower = 1)
  checkmate::assert_int(n_series, lower = 1)
  checkmate::assert_int(ndraws, lower = 1)

  Sigma_vec <- params$Sigma_trend
  checkmate::assert_matrix(Sigma_vec, nrows = ndraws, ncols = n_series^2)

  # Pre-allocate result matrix
  result <- matrix(0, ndraws, n_times * n_series)

  for (d in seq_len(ndraws)) {
    # Reconstruct Sigma matrix (column-major storage)
    Sigma_d <- matrix(Sigma_vec[d, ], n_series, n_series)

    # Compute Cholesky decomposition
    # R's chol() returns upper triangular, transpose for lower
    L_d <- t(chol(Sigma_d))

    # Reshape z for this draw
    z_d <- matrix(z[d, ], n_times, n_series, byrow = FALSE)

    # Transform
    result[d, ] <- as.vector(z_d %*% t(L_d))
  }

  result
}


#' Map Grid Innovations to Observations
#'
#' Maps innovations from (time, series) grid to observation vector using
#' linear indexing. Multiple observations at the same (time, series)
#' receive identical innovations.
#'
#' @param innovations_flat Matrix `[ndraws x (n_times * n_series)]` with
#'   layout: columns ordered as (t1_s1, t2_s1, ..., tT_s1, t1_s2, ..., tT_sS)
#' @param n_times Number of unique time points
#' @param n_series Number of series
#' @param obs_structure List from `get_observation_structure()`
#'
#' @return Matrix `[ndraws x n_obs]` of innovations for each observation
#'
#' @details
#' Linear indexing converts (time, series) to grid position. For example,
#' (time=2, series=3) with n_times=10 becomes grid position 2 + (3-1)*10 = 22.
#' Grid layout: time varies fastest within each series block.
#'
#' @noRd
map_innovations_to_obs <- function(innovations_flat, n_times, n_series,
                                   obs_structure) {
  checkmate::assert_matrix(
    innovations_flat,
    ncols = n_times * n_series,
    any.missing = FALSE
  )
  checkmate::assert_int(n_times, lower = 1)
  checkmate::assert_int(n_series, lower = 1)

  # Convert observation (time, series) to grid indices
  time_idx <- match(obs_structure$time, obs_structure$unique_times)
  series_idx <- obs_structure$series_int

  # Validate time matching succeeded
  if (any(is.na(time_idx))) {
    stop(insight::format_error(
      "Failed to match observation times to unique time points."
    ))
  }

  # Validate series indices are within bounds
  checkmate::assert_integerish(
    series_idx,
    lower = 1,
    upper = n_series,
    any.missing = FALSE
  )

  # Linear index into flattened (time, series) grid
  # Grid layout: time varies fastest within each series block
  grid_idx <- time_idx + (series_idx - 1L) * n_times

  # Column selection
  innovations_flat[, grid_idx, drop = FALSE]
}
