# Per-draw trend-formula linear predictor for forecast.mvgam.
# Returns the deterministic covariate-side `mu_trend[t, s]`
# without the latent state contribution. The kernel uses the
# centred convention `(trend - linpred)`, so this helper is
# called at both the training time grid (to detrend past
# trend values) and the forecast horizon (to re-trend after
# propagation).
#
# `mu_trend` is a Stan transformed parameter that is not stored
# in the posterior, so it must be recomputed via the brms
# machinery -- `extract_component_linpred(..., component =
# "trend", incl_latent_state = FALSE)` already covers smooths,
# tensor products, monotonic effects, random effects, and the
# Hilbert-space `gp(x, k)` basis, and extrapolates cleanly to
# newdata times beyond the training grid (no latent-state
# fallback runs when `incl_latent_state = FALSE`).
#
# Contract:
#   fit       fitted mvgam object
#   draw_id   1-based integer draw index into `as_draws_matrix`
#   newdata   data.frame; rows are the (time, series) cells the
#              caller wants the linpred evaluated at
#   value     [n_unique_times, n_series] matrix, or NULL when
#              the fit has no `trend_formula` (no covariate-side
#              contribution -- the centred convention reduces to
#              `trend - 0 = trend`)
#'@noRd
extract_trend_linpred <- function(fit, draw_id, newdata) {
  checkmate::assert_class(fit, "mvgam")
  checkmate::assert_int(draw_id, lower = 1L)
  checkmate::assert_data_frame(newdata, min.rows = 1L)

  # No trend_formula -> no covariate-side linpred to extract.
  # Return NULL so the caller can branch on it; the kernel
  # treats a NULL `linpreds` slot as a zero matrix.
  if (is.null(fit$trend_model)) return(NULL)

  full_mat <- extract_component_linpred(
    mvgam_fit = fit,
    newdata = newdata,
    component = "trend",
    incl_latent_state = FALSE
  )

  # Multivariate trend formulas return a per-response named list
  # of matrices. Forecasting for multivariate response models is
  # outside this batch; raise a clean error pointing at the
  # univariate path.
  if (is.list(full_mat) && !is.matrix(full_mat)) {
    stop(insight::format_error(c(
      paste0(
        "Multivariate-response trend formulas are not yet ",
        "supported by 'extract_trend_linpred'."
      ),
      x = paste0(
        "Got a per-response list of length ", length(full_mat),
        "; expected a single [ndraws, nobs] matrix."
      ),
      i = paste0(
        "Forecasting for multivariate response models is pending."
      )
    )))
  }

  total_draws <- nrow(full_mat)
  if (draw_id > total_draws) {
    stop(insight::format_error(c(
      "'draw_id' exceeds the number of posterior draws.",
      x = paste0("Got draw_id = ", draw_id,
                 ", total draws = ", total_draws, ".")
    )))
  }

  lp_vec <- as.numeric(full_mat[draw_id, ])
  obs_struct <- get_observation_structure(fit, newdata = newdata)

  reshape_linpred_to_grid(lp_vec, obs_struct)
}


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
