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

  # The trained model already knows its series structure
  # (object$standata$N_series_trend). For single-series models the
  # newdata typically has no `series` column at all (e.g. brms drops
  # columns the formula doesn't reference). Treat single-series as a
  # first-class shortcut here: build the observation structure
  # directly from object metadata + newdata's time column, without
  # invoking ensure_mvgam_variables.
  n_series_trained <- object$standata$N_series_trend %||%
    object$trend_metadata$dimensions$n_series %||%
    length(levels(as.factor(attr(object$obs_data %||% object$data,
                                  "mvgam_series"))))

  is_single_series <- !is.null(n_series_trained) && n_series_trained == 1L
  has_explicit_series <- series_var %in% names(newdata)

  if (is_single_series && !has_explicit_series) {
    # Pull the trained level so series indices line up with the
    # fitted model's encoding.
    train_series <- attr(object$obs_data %||% object$data, "mvgam_series")
    level_label <- if (!is.null(train_series)) {
      as.character(train_series[1L])
    } else {
      "1"
    }
    return(build_single_series_observation_structure(
      newdata, time_var, level_label
    ))
  }

  # Otherwise: prepare data with standardized time/series attributes.
  # For multivariate fits the data may not carry an explicit `series`
  # column (mvbind builds it implicitly); pass response_names so
  # ensure_mvgam_variables can recreate the multivariate series.
  data_prepared <- ensure_mvgam_variables(
    data = newdata,
    parsed_trend = NULL,
    time_var = time_var,
    series_var = series_var,
    response_vars = object$response_names,
    metadata = metadata
  )

  # Validate that data preparation succeeded
  checkmate::assert_data_frame(data_prepared, min.rows = 1)
  if (is.null(attr(data_prepared, "mvgam_time"))) {
    stop(insight::format_error(c(
      "Failed to prepare data with mvgam time attributes.",
      i = cli::format_inline(
        "Check that {.field {time_var}} exists in data."
      )
    )))
  }
  if (is.null(attr(data_prepared, "mvgam_series"))) {
    stop(insight::format_error(c(
      "Failed to prepare data with mvgam series attributes.",
      i = cli::format_inline(
        "Check that {.field {series_var}} exists in data."
      )
    )))
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


#' Build a single-series observation structure directly
#'
#' Shortcut for trained models with `n_series == 1`. The trained model
#' already encodes the (single) series identity in its standata, so we
#' don't need to round-trip through `ensure_mvgam_variables`. We just
#' read newdata's time column, repeat the trained series label across
#' all rows, and produce the same structure
#' `get_observation_structure()` returns for the multi-series path.
#'
#' @param newdata Data frame; must have a column matching `time_var`.
#' @param time_var Character; name of the time column.
#' @param level_label Character; the single trained series level used
#'   for the integer/factor mapping. Pulled from
#'   `attr(object$obs_data, "mvgam_series")[1]`.
#'
#' @noRd
build_single_series_observation_structure <- function(newdata, time_var,
                                                       level_label) {
  if (!time_var %in% names(newdata)) {
    stop(insight::format_error(c(
      paste0("Required time variable '", time_var,
             "' not found in newdata."),
      i = "Add a time column or pass newdata that retains it."
    )))
  }
  time_indices <- newdata[[time_var]]
  unique_times <- sort(unique(time_indices))
  n_obs <- nrow(newdata)
  series_factor <- factor(rep(level_label, n_obs), levels = level_label)
  list(
    time = time_indices,
    series = series_factor,
    series_int = rep(1L, n_obs),
    series_levels = level_label,
    n_obs = n_obs,
    n_times = length(unique_times),
    n_series = 1L,
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


#' Sample Process Errors for a Fitted mvgam Object
#'
#' Top-level entry point for innovation sampling. Composes the
#' observation structure, the trend covariance structure, and the
#' pattern-specific transform into a single matrix of process-error
#' draws aligned with the observation grid.
#'
#' @param object A fitted `mvgam` object.
#' @param ndraws Optional number of posterior draws to use. Mutually
#'   exclusive with `draw_ids`. Defaults to all available draws.
#' @param newdata Optional data frame of prediction covariates. If
#'   `NULL`, the observation grid is taken from the training data.
#' @param draw_ids Optional integer vector of specific draw indices.
#'   Mutually exclusive with `ndraws`.
#'
#' @return Numeric matrix `[ndraws x n_obs]` of sampled innovations,
#'   in the same column order as `newdata` (or training data) rows.
#'   Returns a matrix of zeros for deterministic-trend models (PW,
#'   None) so callers can add it unconditionally.
#'
#' @details
#' For deterministic trends the routine short-circuits and returns a
#' single-row zero matrix unless `ndraws` or `draw_ids` is supplied,
#' in which case the requested row count is returned.
#'
#' @noRd
sample_process_errors <- function(object, ndraws = NULL, newdata = NULL,
                                   draw_ids = NULL) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_int(ndraws, lower = 1, null.ok = TRUE)
  checkmate::assert_integerish(draw_ids, lower = 1, null.ok = TRUE)
  if (!is.null(ndraws) && !is.null(draw_ids)) {
    stop(insight::format_error(
      "Cannot specify both 'ndraws' and 'draw_ids'."
    ))
  }

  obs_structure <- get_observation_structure(object, newdata)

  if (!has_stochastic_trend(object)) {
    n_rows <- if (!is.null(draw_ids)) {
      length(draw_ids)
    } else if (!is.null(ndraws)) {
      ndraws
    } else {
      1L
    }
    return(matrix(0, n_rows, obs_structure$n_obs))
  }

  cov_structure <- get_trend_covariance_structure(
    object, ndraws = ndraws, draw_ids = draw_ids
  )
  sample_innovations(cov_structure, obs_structure)
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
#'   Cholesky factors are kept as flat per-draw vectors (in row-major
#'   order from posterior sort); reshape via
#'   matrix(vec, dim, dim, byrow = TRUE) to get L.
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
  # standata is the ground truth: hierarchical Stan code declares
  # N_groups_trend / N_subgroups_trend; absence means non-hierarchical.
  hierarchical <- !is.null(object$standata) &&
                  !is.null(object$standata$N_groups_trend)

  draws_mat <- posterior::as_draws_matrix(object$fit)
  draw_indices <- resolve_draw_indices(nrow(draws_mat), ndraws, draw_ids)
  draws_mat <- draws_mat[draw_indices, , drop = FALSE]

  effective_pattern <- pattern
  if (pattern == "cholesky_scaled" && !has_correlations) {
    effective_pattern <- "diagonal"
  }

  group_info <- if (hierarchical) get_group_info(object$standata) else NULL
  n_series_int <- as.integer(n_series)

  # Dispatch posterior-parameter extraction on (hierarchical, pattern).
  # Multi-index params (Cholesky factors, full covariance, hierarchical
  # diagonal) require explicit named-column lookup; extract_named_params()
  # sorts only by first index, which is brittle for 2D+ arrays where the
  # secondary order then depends on the source storage convention.
  # The fallback covers patterns that are single-index (e.g. simple
  # diagonal: just "sigma_trend").
  dispatch_key <- paste0(if (hierarchical) "hier" else "flat", ".",
                          effective_pattern)
  params <- switch(
    dispatch_key,
    "hier.cholesky_scaled"  = extract_hierarchical_cholesky_params(
                                draws_mat, group_info),
    "hier.diagonal"         = extract_hierarchical_diagonal_params(
                                draws_mat, group_info),
    "flat.cholesky_scaled"  = extract_simple_cholesky_params(
                                draws_mat, n_series_int),
    "flat.full_covariance"  = extract_simple_full_cov_params(
                                draws_mat, n_series_int),
    {
      param_names <- covariance_param_specs[[effective_pattern]][[
        if (hierarchical) "hierarchical" else "simple"
      ]]
      extract_named_params(draws_mat, param_names)
    }
  )

  list(
    pattern = pattern,
    n_series = as.integer(n_series),
    hierarchical = hierarchical,
    has_correlations = has_correlations,
    ndraws = length(draw_indices),
    params = params,
    group_info = group_info
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


#' Extract Simple Cholesky Parameters as Structured Arrays
#'
#' Builds a per-draw `[ndraws, n, n]` array for `L_Omega_trend` and
#' `[ndraws, n]` matrix for `sigma_trend` via direct column-name
#' lookup. Mirrors `extract_hierarchical_cholesky_params()` for the
#' non-hierarchical case so reconstruction is independent of any
#' sort-order assumption in `extract_named_params()`.
#'
#' @noRd
extract_simple_cholesky_params <- function(draws_mat, n_series) {
  checkmate::assert_matrix(draws_mat, min.rows = 1, min.cols = 1)
  checkmate::assert_int(n_series, lower = 1)
  ndraws <- nrow(draws_mat)
  all_cols <- colnames(draws_mat)

  pull_col <- function(name) {
    if (!name %in% all_cols) {
      stop(insight::format_error(c(
        paste0("Posterior parameter '", name, "' not found."),
        i = "Required for simple Cholesky covariance."
      )))
    }
    as.numeric(draws_mat[, name])
  }

  sigma <- matrix(0, ndraws, n_series)
  for (s in seq_len(n_series)) {
    sigma[, s] <- pull_col(sprintf("sigma_trend[%d]", s))
  }

  L_omega <- array(0, c(ndraws, n_series, n_series))
  for (j in seq_len(n_series)) {
    for (i in seq_len(n_series)) {
      L_omega[, i, j] <- pull_col(sprintf("L_Omega_trend[%d,%d]", i, j))
    }
  }

  list(sigma_trend = sigma, L_Omega_trend = L_omega)
}


#' Extract Simple Full Covariance Parameters as Structured Arrays
#'
#' Builds a `[ndraws, n, n]` array for `Sigma_trend` via direct
#' column-name lookup so reconstruction is order-safe.
#'
#' @noRd
extract_simple_full_cov_params <- function(draws_mat, n_series) {
  checkmate::assert_matrix(draws_mat, min.rows = 1, min.cols = 1)
  checkmate::assert_int(n_series, lower = 1)
  ndraws <- nrow(draws_mat)
  all_cols <- colnames(draws_mat)

  pull_col <- function(name) {
    if (!name %in% all_cols) {
      stop(insight::format_error(c(
        paste0("Posterior parameter '", name, "' not found."),
        i = "Required for full-covariance trend."
      )))
    }
    as.numeric(draws_mat[, name])
  }

  Sigma <- array(0, c(ndraws, n_series, n_series))
  for (j in seq_len(n_series)) {
    for (i in seq_len(n_series)) {
      Sigma[, i, j] <- pull_col(sprintf("Sigma_trend[%d,%d]", i, j))
    }
  }

  list(Sigma_trend = Sigma)
}


#' Extract Hierarchical Cholesky Parameters as Structured Arrays
#'
#' Builds named arrays for hierarchical Cholesky covariance from posterior
#' draws via direct column-name lookup. Unlike `extract_named_params()`
#' (which sorts only by first index), this preserves multi-index ordering
#' so downstream code can use `arr[d, g, i, j]` directly.
#'
#' @return List with components:
#'   - `alpha_cor_trend`: numeric vector length ndraws
#'   - `L_Omega_global_trend`: array `[ndraws, n_sub, n_sub]`
#'   - `L_deviation_group_trend`: array `[ndraws, n_groups, n_sub, n_sub]`
#'   - `sigma_group_trend`: array `[ndraws, n_groups, n_sub]`
#'
#' @noRd
extract_hierarchical_cholesky_params <- function(draws_mat, group_info) {
  checkmate::assert_matrix(draws_mat, min.rows = 1, min.cols = 1)
  checkmate::assert_list(group_info)
  checkmate::assert_names(
    names(group_info),
    must.include = c("n_groups", "n_subgroups")
  )

  ndraws <- nrow(draws_mat)
  n_groups <- as.integer(group_info$n_groups)
  n_sub <- as.integer(group_info$n_subgroups)
  all_cols <- colnames(draws_mat)

  pull_col <- function(name) {
    if (!name %in% all_cols) {
      stop(insight::format_error(c(
        paste0("Posterior parameter '", name, "' not found."),
        i = "Required for hierarchical Cholesky covariance."
      )))
    }
    as.numeric(draws_mat[, name])
  }

  # alpha_cor_trend: scalar per draw
  alpha <- pull_col("alpha_cor_trend")

  # L_Omega_global_trend: 2D matrix per draw, shape [n_sub, n_sub]
  L_global <- array(0, c(ndraws, n_sub, n_sub))
  for (j in seq_len(n_sub)) {
    for (i in seq_len(n_sub)) {
      L_global[, i, j] <- pull_col(
        sprintf("L_Omega_global_trend[%d,%d]", i, j)
      )
    }
  }

  # L_deviation_group_trend: per-group matrix, shape [n_groups, n_sub, n_sub]
  L_dev <- array(0, c(ndraws, n_groups, n_sub, n_sub))
  for (g in seq_len(n_groups)) {
    for (j in seq_len(n_sub)) {
      for (i in seq_len(n_sub)) {
        L_dev[, g, i, j] <- pull_col(
          sprintf("L_deviation_group_trend[%d,%d,%d]", g, i, j)
        )
      }
    }
  }

  # sigma_group_trend: per-group SDs, shape [n_groups, n_sub]
  sigma_grp <- array(0, c(ndraws, n_groups, n_sub))
  for (g in seq_len(n_groups)) {
    for (s in seq_len(n_sub)) {
      sigma_grp[, g, s] <- pull_col(
        sprintf("sigma_group_trend[%d,%d]", g, s)
      )
    }
  }

  list(
    alpha_cor_trend = alpha,
    L_Omega_global_trend = L_global,
    L_deviation_group_trend = L_dev,
    sigma_group_trend = sigma_grp
  )
}


#' Extract Hierarchical Diagonal Parameters Broadcast to Per-Series Sigma
#'
#' For hierarchical models with `cor = FALSE` (RW(gr=), AR(gr=), ZMVN(gr=)
#' with diagonal innovation covariance), the posterior holds per-group SDs
#' as `sigma_group_trend[g, k]`. The innovation sampler operates on a flat
#' per-series `sigma_trend[d, s]` matrix, so we broadcast each series to
#' its group's `k`-th entry, where `k` is the series' index within its
#' group as ordered in the Stan codegen (matches the `if
#' (group_inds_trend[s] == g_idx) { k += 1; ... }` loop in
#' `generate_hierarchical_correlation_parameters()`).
#'
#' @return List with `sigma_trend`: matrix `[ndraws x n_series]`
#'
#' @noRd
extract_hierarchical_diagonal_params <- function(draws_mat, group_info) {
  checkmate::assert_matrix(draws_mat, min.rows = 1, min.cols = 1)
  checkmate::assert_list(group_info)
  checkmate::assert_names(
    names(group_info),
    must.include = c("n_groups", "n_subgroups", "group_inds")
  )

  ndraws <- nrow(draws_mat)
  n_groups <- as.integer(group_info$n_groups)
  n_sub <- as.integer(group_info$n_subgroups)
  group_inds <- as.integer(group_info$group_inds)
  n_series <- length(group_inds)
  all_cols <- colnames(draws_mat)

  pull_col <- function(name) {
    if (!name %in% all_cols) {
      stop(insight::format_error(c(
        paste0("Posterior parameter '", name, "' not found."),
        i = "Required for hierarchical diagonal covariance."
      )))
    }
    as.numeric(draws_mat[, name])
  }

  # Sub-index of each series within its group, matching Stan loop order.
  sub_idx <- integer(n_series)
  group_counter <- integer(n_groups)
  for (s in seq_len(n_series)) {
    g <- group_inds[s]
    group_counter[g] <- group_counter[g] + 1L
    sub_idx[s] <- group_counter[g]
  }

  sigma_trend <- matrix(0, ndraws, n_series)
  for (s in seq_len(n_series)) {
    sigma_trend[, s] <- pull_col(
      sprintf("sigma_group_trend[%d,%d]", group_inds[s], sub_idx[s])
    )
  }

  list(sigma_trend = sigma_trend)
}


#' @noRd
get_group_info <- function(standata) {
  list(
    n_groups = standata$N_groups_trend,
    n_subgroups = standata$N_subgroups_trend,
    group_inds = standata$group_inds_trend
  )
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

  # Transform by covariance pattern. Hierarchical Cholesky uses a
  # different structure (per-group convex combination of correlations),
  # so route it to its dedicated transform.
  is_hier_chol <- isTRUE(cov_structure$hierarchical) &&
                  effective_pattern == "cholesky_scaled"

  if (is_hier_chol) {
    innovations_flat <- transform_hierarchical_cholesky_innovations(
      z, cov_structure$params, n_times, n_series, ndraws,
      cov_structure$group_info
    )
  } else {
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
  }

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
#' @param params List from `extract_simple_cholesky_params()`:
#'   - `sigma_trend`: matrix `[ndraws x n_series]` of innovation SDs
#'   - `L_Omega_trend`: array `[ndraws x n_series x n_series]`
#' @param n_times Number of unique time points
#' @param n_series Number of series
#' @param ndraws Number of posterior draws
#'
#' @return Matrix `[ndraws x (n_times * n_series)]` of transformed innovations
#'
#' @details
#' For each draw d:
#' 1. Slice L_Omega from the per-draw 3D array
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
  L_omega_arr <- params$L_Omega_trend

  checkmate::assert_matrix(sigma, nrows = ndraws, ncols = n_series)
  if (!identical(dim(L_omega_arr),
                 as.integer(c(ndraws, n_series, n_series)))) {
    stop(insight::format_error(c(
      "Unexpected dimensions for 'L_Omega_trend'.",
      x = paste0("Got: ", paste(dim(L_omega_arr), collapse = "x"),
                 ", expected: ", ndraws, "x", n_series, "x", n_series, ".")
    )))
  }

  # Pre-allocate result matrix
  result <- matrix(0, ndraws, n_times * n_series)

  for (d in seq_len(ndraws)) {
    L_omega <- L_omega_arr[d, , ]

    # L_Sigma = diag(sigma) %*% L_Omega via row scaling
    L_sigma <- L_omega * as.numeric(sigma[d, ])

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
#' @param params List from `extract_simple_full_cov_params()`:
#'   - `Sigma_trend`: array `[ndraws x n_series x n_series]`
#' @param n_times Number of unique time points
#' @param n_series Number of series
#' @param ndraws Number of posterior draws
#'
#' @return Matrix `[ndraws x (n_times * n_series)]` of transformed innovations
#'
#' @details
#' For each draw d:
#' 1. Slice Sigma from the per-draw 3D array
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

  Sigma_arr <- params$Sigma_trend
  if (!identical(dim(Sigma_arr),
                 as.integer(c(ndraws, n_series, n_series)))) {
    stop(insight::format_error(c(
      "Unexpected dimensions for 'Sigma_trend'.",
      x = paste0("Got: ", paste(dim(Sigma_arr), collapse = "x"),
                 ", expected: ", ndraws, "x", n_series, "x", n_series, ".")
    )))
  }

  # Pre-allocate result matrix
  result <- matrix(0, ndraws, n_times * n_series)

  for (d in seq_len(ndraws)) {
    Sigma_d <- Sigma_arr[d, , ]

    # R's chol() returns upper triangular; transpose for lower.
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


#' Transform Innovations: Hierarchical Cholesky Pattern
#'
#' For models declared with `gr=` grouping. The per-group covariance is
#' assembled via Stan's `combine_cholesky()` (see R/stan_assembly.R) as a
#' convex combination of the global correlation and per-group deviations,
#' then re-Choleskied and scaled by per-group SDs:
#' \itemize{
#'   \item C_global = L_g L_g^T
#'   \item C_local[g] = L_d[g] L_d[g]^T
#'   \item L_grp[g] = chol(alpha * C_global + (1 - alpha) * C_local[g])
#'   \item L_full[g] = diag(sigma_grp[g]) %*% L_grp[g]
#' }
#' Different groups are independent (block-diagonal full covariance).
#'
#' @param z Matrix `[ndraws x (n_times * n_series)]` of standard normals.
#' @param params List of structured arrays from
#'   `extract_hierarchical_cholesky_params()`:
#'   `alpha_cor_trend`, `L_Omega_global_trend`,
#'   `L_deviation_group_trend`, `sigma_group_trend`.
#' @param n_times Number of unique time points.
#' @param n_series Total number of series across all groups.
#' @param ndraws Number of posterior draws.
#' @param group_info List with `n_groups`, `n_subgroups`, `group_inds`.
#'
#' @return Matrix `[ndraws x (n_times * n_series)]` of transformed
#'   innovations laid out in the same column order as the diagonal /
#'   simple-Cholesky transforms.
#'
#' @noRd
transform_hierarchical_cholesky_innovations <- function(z, params, n_times,
                                                        n_series, ndraws,
                                                        group_info) {
  checkmate::assert_matrix(z, nrows = ndraws, ncols = n_times * n_series)
  checkmate::assert_int(n_times, lower = 1)
  checkmate::assert_int(n_series, lower = 1)
  checkmate::assert_int(ndraws, lower = 1)
  checkmate::assert_list(group_info)
  checkmate::assert_names(
    names(group_info),
    must.include = c("n_groups", "n_subgroups", "group_inds")
  )

  n_groups <- as.integer(group_info$n_groups)
  n_sub <- as.integer(group_info$n_subgroups)
  group_inds <- as.integer(group_info$group_inds)

  checkmate::assert_int(n_groups, lower = 1)
  checkmate::assert_int(n_sub, lower = 1)
  checkmate::assert_integerish(
    group_inds,
    lower = 1, upper = n_groups,
    len = n_series, any.missing = FALSE
  )

  alpha <- params$alpha_cor_trend
  L_glob_arr <- params$L_Omega_global_trend
  L_dev_arr <- params$L_deviation_group_trend
  sigma_arr <- params$sigma_group_trend

  checkmate::assert_numeric(alpha, len = ndraws, any.missing = FALSE)
  # Local helper: dim() returns integer; build expected as integer too
  # so identical() doesn't trip on a numeric/integer mismatch.
  check_dims <- function(arr, name, expected) {
    expected <- as.integer(expected)
    if (!identical(dim(arr), expected)) {
      stop(insight::format_error(c(
        paste0("Unexpected dimensions for '", name, "'."),
        x = paste0("Got: ", paste(dim(arr), collapse = "x"),
                   ", expected: ", paste(expected, collapse = "x"), ".")
      )))
    }
  }

  # L_Omega_global: [ndraws, n_sub, n_sub]
  checkmate::assert_array(
    L_glob_arr, d = 3,
    any.missing = FALSE
  )
  check_dims(L_glob_arr, "L_Omega_global_trend",
             c(ndraws, n_sub, n_sub))
  # L_deviation_group: [ndraws, n_groups, n_sub, n_sub]
  check_dims(L_dev_arr, "L_deviation_group_trend",
             c(ndraws, n_groups, n_sub, n_sub))
  # sigma_group: [ndraws, n_groups, n_sub]
  check_dims(sigma_arr, "sigma_group_trend",
             c(ndraws, n_groups, n_sub))

  # Each series's rank within its group (1-based). Vectorized via ave().
  within_pos <- as.integer(
    ave(seq_along(group_inds), group_inds, FUN = seq_along)
  )

  # Pre-compute series indices grouped by group_id
  series_by_group <- split(seq_len(n_series), group_inds)

  # Pre-compute (start, end) column ranges per series in z / result
  series_col_start <- (seq_len(n_series) - 1L) * n_times + 1L

  result <- matrix(0, ndraws, n_times * n_series)

  for (d in seq_len(ndraws)) {
    # drop = FALSE not needed for 3D->2D slice; R returns a matrix.
    L_glob_d <- L_glob_arr[d, , ]
    glob_cor <- tcrossprod(L_glob_d)
    alpha_d <- alpha[d]
    one_minus_alpha_d <- 1 - alpha_d

    for (g in seq_len(n_groups)) {
      L_dev_dg <- L_dev_arr[d, g, , ]
      local_cor <- tcrossprod(L_dev_dg)

      combined_cor <- alpha_d * glob_cor + one_minus_alpha_d * local_cor

      # R's chol() is upper-tri; transpose to match Stan's lower-tri
      # cholesky_decompose() convention.
      L_grp <- t(chol(combined_cor))

      # Row-scale by per-group sigmas (equivalent to diag(sigma) %*% L)
      sigma_dg <- sigma_arr[d, g, ]
      L_full <- L_grp * sigma_dg

      series_g <- series_by_group[[g]]

      # Stack z columns for these series into [n_times, n_sub] in
      # within-group order. z layout: cols (s-1)*n_times + 1:n_times.
      z_g <- matrix(0, n_times, n_sub)
      for (k in seq_along(series_g)) {
        s <- series_g[k]
        col_range <- series_col_start[s] + (0:(n_times - 1L))
        z_g[, within_pos[s]] <- z[d, col_range]
      }

      # Transform: x = z %*% t(L) gives MVN with cov L %*% t(L)
      innov_g <- z_g %*% t(L_full)

      # Place innovations back into result
      for (k in seq_along(series_g)) {
        s <- series_g[k]
        col_range <- series_col_start[s] + (0:(n_times - 1L))
        result[d, col_range] <- innov_g[, within_pos[s]]
      }
    }
  }

  result
}
