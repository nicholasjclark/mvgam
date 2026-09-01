# Internal: reshape an ordered length-`nobs` linpred vector into
# a `[n_unique_times, n_series]` matrix using the `(time,
# series_int)` mapping `get_observation_structure` produces.
# Rows of the returned matrix correspond to
# `obs_struct$unique_times` in their stored (sorted) order;
# columns correspond to the series-level integer codes from
# `obs_struct$series_int`.
#'@noRd
reshape_linpred_to_grid <- function(lp_vec, obs_struct) {
  unique_times <- obs_struct$unique_times
  n_time <- length(unique_times)
  n_series <- as.integer(obs_struct$n_series)

  out <- matrix(0, nrow = n_time, ncol = n_series)
  t_idx <- match(obs_struct$time, unique_times)
  s_idx <- as.integer(obs_struct$series_int)

  # Sanity guards: extract_component_linpred should never hand
  # back a vector with mismatched length or out-of-range indices,
  # but flag the failure crisply if it does so callers see the
  # real cause rather than a downstream subscript error.
  if (length(lp_vec) != length(t_idx)) {
    stop(insight::format_error(c(
      "Linpred vector length does not match observation count.",
      x = paste0(
        "Got length(lp_vec) = ", length(lp_vec),
        ", expected ", length(t_idx), "."
      )
    )))
  }
  if (any(is.na(t_idx))) {
    stop(insight::format_error(c(
      "Observation times missing from the unique-time grid."
    )))
  }
  if (any(s_idx < 1L | s_idx > n_series)) {
    stop(insight::format_error(c(
      "Series indices outside the [1, n_series] range."
    )))
  }

  for (j in seq_along(lp_vec)) {
    out[t_idx[j], s_idx[j]] <- lp_vec[j]
  }
  out
}


# Internal: inverse of `reshape_linpred_to_grid`. Flatten a
# `[n_unique_times, n_series]` matrix back into the
# observation-ordered vector that matches `obs_struct$time` /
# `obs_struct$series_int` row-for-row. Used by `forecast.mvgam`
# to align per-draw kernel output with the obs-side linpred
# vector before combining and applying the observation family.
#'@noRd
flatten_grid_to_obs_order <- function(grid, obs_struct) {
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
