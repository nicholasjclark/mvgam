#' A linear predictor vector placed on its time-by-series grid
#'
#' Rows follow `obs_struct$unique_times` in their stored sorted order.
#' Columns follow the integer series codes in `obs_struct$series_int`.
#'
#' @param lp_vec Numeric vector of linear predictor values, one per
#'   observation in the order of `obs_struct$time`
#' @param obs_struct Observation structure from
#'   `get_observation_structure()`, holding `time`, `series_int`,
#'   `unique_times` and `n_series`
#' @return Numeric matrix with one row per unique time and one column
#'   per series
#' @noRd
reshape_linpred_to_grid <- function(lp_vec, obs_struct) {
  checkmate::assert_numeric(lp_vec)
  checkmate::assert_list(obs_struct)
  unique_times <- obs_struct$unique_times
  n_time <- length(unique_times)
  n_series <- as.integer(obs_struct$n_series)

  out <- matrix(0, nrow = n_time, ncol = n_series)
  t_idx <- match(obs_struct$time, unique_times)
  s_idx <- as.integer(obs_struct$series_int)

  # These guards report a mismatched length or an out-of-range index
  # at its source. A downstream subscript error hides the cause.
  if (length(lp_vec) != length(t_idx)) {
    stop_shape_fault("The linear predictor", length(lp_vec),
                     length(t_idx))
  }
  if (anyNA(t_idx)) {
    stop_mvgam_fault(
      "Every observation time must lie on the unique-time grid.",
      paste0(sum(is.na(t_idx)), " observation time(s) fall outside it.")
    )
  }
  if (any(s_idx < 1L | s_idx > n_series)) {
    stop_mvgam_fault(
      "Series indices fall outside the [1, n_series] range.",
      paste0(sum(s_idx < 1L | s_idx > n_series), " of ",
             length(s_idx), " indices are out of range.")
    )
  }

  for (j in seq_along(lp_vec)) {
    out[t_idx[j], s_idx[j]] <- lp_vec[j]
  }
  out
}


#' A time-by-series grid flattened to observation order
#'
#' The inverse of `reshape_linpred_to_grid()`. The forecast path uses it
#' to line up per-draw trend output with the observation-side linear
#' predictor before the observation family applies.
#'
#' @param grid Numeric matrix with one row per unique time and one
#'   column per series
#' @param obs_struct Observation structure from
#'   `get_observation_structure()`, holding `time`, `series_int` and
#'   `unique_times`
#' @return Numeric vector with one value per observation, in the order
#'   of `obs_struct$time`
#' @noRd
flatten_grid_to_obs_order <- function(grid, obs_struct) {
  checkmate::assert_matrix(grid)
  checkmate::assert_list(obs_struct)
  unique_times <- obs_struct$unique_times
  n_obs <- length(obs_struct$time)
  out <- numeric(n_obs)
  t_idx <- match(obs_struct$time, unique_times)
  s_idx <- as.integer(obs_struct$series_int)
  for (j in seq_len(n_obs)) {
    out[j] <- grid[t_idx[j], s_idx[j]]
  }
  out
}
