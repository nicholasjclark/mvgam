# Per-draw last-state extraction for forecast.mvgam. One
# dispatcher with per-trend branches that pulls together the
# structured `last_state` list AND the `params` list
# `propagate_trend()` consumes.
#
# Contract for one draw:
#   list(
#     params     = <named list, see propagate_trend(@details)>,
#     last_state = list(
#       trends   = matrix[max_lag, n_series],
#       errors   = matrix[max_ma, n_series] | NULL,
#       linpreds = matrix[max_lag, n_series],  # filled by caller
#       time     = numeric(n_series)           # CAR only
#     )
#   )


# Internal: pull the per-draw `(params, last_state)` pair for
# one trend type. Returns NULL for fits with no trend.
#
# `fit`     fitted mvgam object
# `draw_id` integer draw index (1-based) into `as_draws_df(fit)`
#
# Defensive against legacy fits saved before the trend_metadata
# enrichments: re-runs `enrich_trend_metadata` on the fly when
# the cached fields are absent so loaded older fits still work.
#'@noRd
extract_last_state <- function(fit, draw_id) {
  checkmate::assert_class(fit, "mvgam")
  checkmate::assert_int(draw_id, lower = 1L)

  meta <- get_enriched_trend_metadata(fit)
  if (is.null(meta) || is.null(meta$trend_type)) return(NULL)

  # `as_draws_matrix` returns a `[ndraws, nvars]` posterior
  # matrix with no chain/iteration/draw metadata columns.
  # Coerce one row to a plain named numeric vector so the
  # per-cell lookups below behave as expected -- a row subset
  # of a `draws_matrix` keeps the matrix class, on which
  # `[["nm"]]` raises "subscript out of bounds". Going via
  # `setNames(as.numeric(...), colnames(...))` also sidesteps
  # the "Dropping 'draws_df' class" warnings raised by
  # `[.draws_df`.
  draws_mat <- posterior::as_draws_matrix(fit$fit)
  total_draws <- nrow(draws_mat)
  if (draw_id > total_draws) {
    stop(insight::format_error(c(
      "'draw_id' exceeds the number of posterior draws.",
      x = paste0("Got draw_id = ", draw_id,
                 ", total draws = ", total_draws, ".")
    )))
  }
  one_draw <- setNames(
    as.numeric(draws_mat[draw_id, ]),
    colnames(draws_mat)
  )

  n_series <- as.integer(fit$standata$N_series_trend %||% 1L)
  n_lv <- as.integer(fit$standata$N_lv_trend %||% n_series)

  # Per the trend-system architecture docs, factor models set
  # `n_lv < n_series` and the latent dynamics live in `n_lv`
  # space, with a Z matrix projecting to observed series.
  # Hierarchical fits expand `n_lv = N_groups * N_subgroups`.
  # Both cases require a dedicated extraction path; the current
  # dispatcher supports only `n_lv == n_series`.
  if (n_lv != n_series) {
    stop(insight::format_error(c(
      paste0(
        "Factor or hierarchical trends (n_lv != n_series) are ",
        "not yet supported by 'extract_last_state'."
      ),
      x = paste0(
        "Got n_lv = ", n_lv, ", n_series = ", n_series, "."
      ),
      i = paste0(
        "Hierarchical and factor trend support is pending."
      )
    )))
  }

  switch(
    meta$trend_type,
    "RW" = extract_rw_state(one_draw, meta, n_series, n_lv, fit),
    "AR" = extract_ar_state(one_draw, meta, n_series, n_lv, fit),
    "VAR" = extract_var_state(one_draw, meta, n_series, n_lv, fit),
    "CAR" = extract_car_state(one_draw, meta, n_series, fit),
    "ZMVN" = extract_zmvn_state(one_draw, meta, n_series, n_lv),
    stop(insight::format_error(c(
      paste0(
        "Trend type '", meta$trend_type,
        "' is not supported by 'extract_last_state'."
      ),
      i = "Supported: 'RW', 'AR', 'VAR', 'CAR', 'ZMVN'."
    )))
  )
}


# Internal: get the enriched trend_metadata, computing the
# kernel-relevant extras on the fly when the saved fit predates
# the fit-time enrichment helpers.
#'@noRd
get_enriched_trend_metadata <- function(fit) {
  meta <- fit$trend_metadata
  if (is.null(meta)) return(NULL)
  # Re-enrich only when all enrichment fields are missing.
  # Partial enrichment is treated as missing so a regression
  # that leaves some fields unset still gets repaired here.
  if (!is.null(meta$ar_lags) && !is.null(meta$ma_lags) &&
      !is.null(meta$max_lag)) {
    return(meta)
  }
  enrich_trend_metadata(meta, fit$mv_spec$trend_specs)
}


# ----- per-trend extraction helpers -------------------------------

# Internal: pull the last `max_lag` rows of the [N_time, N_series]
# `trend[t, s]` posterior matrix for one draw, sliced to the first
# `n_series` columns. Reason: the kernel applies the brms-centred
# convention `(trend - linpred)`, so passing `trend[t, s]` here
# with the matching `mu_trend[t, s]` as `linpreds` lets the kernel
# recover the latent process internally.
#'@noRd
extract_trend_history <- function(one_draw, n_series, n_lv, max_lag,
                                    n_time) {
  if (max_lag == 0L) {
    return(matrix(0, nrow = 0L, ncol = n_series))
  }
  start_t <- n_time - max_lag + 1L
  out <- matrix(0, nrow = max_lag, ncol = n_series)
  for (t in seq_len(max_lag)) {
    abs_t <- start_t + t - 1L
    for (s in seq_len(n_series)) {
      nm <- paste0("trend[", abs_t, ",", s, "]")
      out[t, s] <- as.numeric(one_draw[[nm]])
    }
  }
  out
}


# Internal: build the diagonal AR-coefficient matrix per active
# lag from a `one_draw` row. For each lag k in `ar_lags`, pulls
# `ar<k>_trend[1..n_lv]` and constructs a length-`n_series`
# vector (broadcasting the single-element `n_lv == 1` shared-AR
# case to all series).
#'@noRd
extract_ar_coefs <- function(one_draw, ar_lags, n_series, n_lv) {
  m_a <- length(ar_lags)
  out <- matrix(0, nrow = m_a, ncol = n_series)
  for (k in seq_len(m_a)) {
    lag <- ar_lags[k]
    nms <- paste0("ar", lag, "_trend[", seq_len(n_lv), "]")
    vec <- as.numeric(one_draw[nms])
    out[k, ] <- broadcast_to_series(vec, n_series)
  }
  out
}


# Internal: broadcast a length-`n_lv` parameter vector to
# `n_series`. When `n_lv == 1` (shared parameter across series),
# repeat the single value across series. When `n_lv == n_series`,
# return as-is. When `1 < n_lv < n_series` (latent-factor or
# hierarchical fits), take the first `n_series` elements; the
# full mapping contract for those cases is pending hierarchical
# trend support.
#'@noRd
broadcast_to_series <- function(vec, n_series) {
  if (length(vec) == n_series) return(vec)
  if (length(vec) == 1L) return(rep(vec, n_series))
  vec[seq_len(n_series)]
}


# Internal: pull `sigma_trend` per series and (optionally) build
# the innovation covariance `Sigma` from `L_Omega_trend`.
#'@noRd
extract_sigma_and_cov <- function(one_draw, n_series, n_lv,
                                    has_cor) {
  sigma_nms <- paste0("sigma_trend[", seq_len(n_lv), "]")
  sigma_vec <- broadcast_to_series(
    as.numeric(one_draw[sigma_nms]), n_series
  )
  Sigma <- if (has_cor) {
    L <- matrix(0, nrow = n_series, ncol = n_series)
    for (i in seq_len(n_series)) {
      for (j in seq_len(i)) {
        nm <- paste0("L_Omega_trend[", i, ",", j, "]")
        L[i, j] <- as.numeric(one_draw[[nm]])
      }
    }
    diag(sigma_vec) %*% tcrossprod(L) %*% diag(sigma_vec)
  } else {
    diag(sigma_vec^2, nrow = n_series)
  }
  list(sigma = sigma_vec, Sigma = Sigma)
}


# Internal: pull `theta1_trend` per series when the trend has an
# MA component. Returns a length-`n_series` numeric vector.
#'@noRd
extract_ma_coefs <- function(one_draw, n_series, n_lv) {
  nms <- paste0("theta1_trend[", seq_len(n_lv), "]")
  broadcast_to_series(as.numeric(one_draw[nms]), n_series)
}


# Internal: empty `errors` history when the trend has no MA
# terms. propagate_trend treats NULL or zero-row identically.
#'@noRd
empty_errors <- function() NULL


# Internal: pull the last `max_ma` rows of the MA-applied
# innovation matrix `ma_innovations_trend[t, j]` from a single
# posterior draw. The kernel uses these as the past
# `errors[t - j]` terms when propagating forward, so they must
# come from the actual posterior draws of the in-sample MA
# innovations rather than fresh zero seeds.
#'@noRd
extract_ma_innovations <- function(one_draw, n_series, n_lv,
                                     max_ma, n_time) {
  if (max_ma == 0L) return(NULL)
  start_t <- n_time - max_ma + 1L
  out <- matrix(0, nrow = max_ma, ncol = n_series)
  for (t in seq_len(max_ma)) {
    abs_t <- start_t + t - 1L
    for (s in seq_len(n_series)) {
      nm <- paste0("ma_innovations_trend[", abs_t, ",", s, "]")
      val <- one_draw[[nm]]
      if (is.null(val)) {
        # Some configurations may not expose ma_innovations_trend
        # in the posterior (e.g. when the user has excluded it
        # via `exclude`). Fall back to zero — the kernel default.
        return(NULL)
      }
      out[t, s] <- as.numeric(val)
    }
  }
  out
}


# RW / AR / ARMA share the same extraction skeleton: pull
# sigma / Sigma / optional MA / trend history, optionally pull
# AR coefficients per active lag. RW skips the AR pull because
# the kernel hardcodes the coefficient at 1.
#'@noRd
extract_rw_state <- function(one_draw, meta, n_series, n_lv, fit) {
  extract_arma_state(one_draw, meta, n_series, n_lv, fit,
                      pull_ar = FALSE)
}

#'@noRd
extract_ar_state <- function(one_draw, meta, n_series, n_lv, fit) {
  extract_arma_state(one_draw, meta, n_series, n_lv, fit,
                      pull_ar = TRUE)
}

#'@noRd
extract_arma_state <- function(one_draw, meta, n_series, n_lv, fit,
                                pull_ar) {
  n_time <- as.integer(fit$standata$N_time_trend)
  scov <- extract_sigma_and_cov(one_draw, n_series, n_lv,
                                 meta$has_cor)
  params <- list(sigma = scov$sigma, Sigma = scov$Sigma)
  if (pull_ar) {
    params$ar <- extract_ar_coefs(one_draw, meta$ar_lags,
                                    n_series, n_lv)
  }
  max_ma <- if (length(meta$ma_lags) > 0L) max(meta$ma_lags) else 0L
  if (length(meta$ma_lags) > 0L) {
    params$theta <- extract_ma_coefs(one_draw, n_series, n_lv)
  }
  list(
    params = params,
    last_state = list(
      trends = extract_trend_history(one_draw, n_series, n_lv,
                                       meta$max_lag, n_time),
      errors = extract_ma_innovations(one_draw, n_series, n_lv,
                                        max_ma, n_time),
      linpreds = matrix(0, nrow = meta$max_lag, ncol = n_series)
    )
  )
}


# VAR(p) and VARMA(p, 1). Pulls the Heaps-transformed
# `A_trend[i, j, lag]` array per active lag (the kernel reads
# this directly), the standard sigma + Sigma pair, and the
# MA coefficient cube from `D_raw_trend[i, j, 1]` when present.
# VAR fits always have `cor = TRUE` (multivariate innovations).
#'@noRd
extract_var_state <- function(one_draw, meta, n_series, n_lv, fit) {
  n_time <- as.integer(fit$standata$N_time_trend)
  m_a <- length(meta$ar_lags)
  # Stan declares A_trend as `array[N_lags_trend] matrix[N_lv,
  # N_lv]`, so the index order is `A_trend[lag, i, j]` (array
  # slot first, then matrix row/col). We pack into an
  # [n_series, n_series, n_lags] R cube matching the contract
  # of `propagate_trend(..., params = list(A = ...))`.
  A_cube <- array(0, dim = c(n_series, n_series, m_a))
  for (k in seq_len(m_a)) {
    lag <- meta$ar_lags[k]
    for (i in seq_len(n_series)) {
      for (j in seq_len(n_series)) {
        nm <- paste0("A_trend[", lag, ",", i, ",", j, "]")
        A_cube[i, j, k] <- as.numeric(one_draw[[nm]])
      }
    }
  }
  scov <- extract_sigma_and_cov(one_draw, n_series, n_lv,
                                 has_cor = TRUE)
  params <- list(A = A_cube,
                  sigma = scov$sigma,
                  Sigma = scov$Sigma)
  if (length(meta$ma_lags) > 0L) {
    m_b <- length(meta$ma_lags)
    # D_raw_trend follows the same `array[lag] matrix[...]`
    # convention: index as `D_raw_trend[lag, i, j]`.
    B_cube <- array(0, dim = c(n_series, n_series, m_b))
    for (j_lag in seq_len(m_b)) {
      lag <- meta$ma_lags[j_lag]
      for (i in seq_len(n_series)) {
        for (j in seq_len(n_series)) {
          nm <- paste0("D_raw_trend[", lag, ",", i, ",", j, "]")
          B_cube[i, j, j_lag] <- as.numeric(one_draw[[nm]])
        }
      }
    }
    params$theta_cube <- B_cube
  }
  list(
    params = params,
    last_state = list(
      trends = extract_trend_history(one_draw, n_series, n_lv,
                                       meta$max_lag, n_time),
      errors = empty_errors(),
      linpreds = matrix(0, nrow = meta$max_lag, ncol = n_series)
    )
  )
}


# CAR(1): continuous-time AR(1). Pulls phi per series from
# `ar1_trend[s]`, sigma per series from `sigma_trend[s]`, the
# last latent state from the trailing row of `trend[t, s]`, and
# the last observed time per series so the caller can build the
# forecast gap vector via diff(c(last_time, newdata_times)).
#'@noRd
extract_car_state <- function(one_draw, meta, n_series, fit) {
  n_lv <- as.integer(fit$standata$N_lv_trend %||% n_series)
  n_time <- as.integer(fit$standata$N_time_trend)
  phi_nms <- paste0("ar1_trend[", seq_len(n_lv), "]")
  phi <- broadcast_to_series(
    as.numeric(one_draw[phi_nms]), n_series
  )
  sigma_nms <- paste0("sigma_trend[", seq_len(n_lv), "]")
  sigma <- broadcast_to_series(
    as.numeric(one_draw[sigma_nms]), n_series
  )
  trends_hist <- extract_trend_history(
    one_draw, n_series, n_lv, max_lag = 1L, n_time = n_time
  )
  last_time <- extract_last_observed_times(fit, n_series)
  list(
    params = list(phi = phi, sigma = sigma),
    last_state = list(
      trends = trends_hist,
      errors = empty_errors(),
      linpreds = matrix(0, nrow = 1L, ncol = n_series),
      time = last_time
    )
  )
}


# Internal: pull the last observed time per series for CAR
# forecasting. Reads the time variable named on
# `fit$trend_metadata$variables$time_var` from the fit's stored
# obs/trend data. Falls back to the highest unique time in the
# trend data when the per-series tail is unavailable.
#'@noRd
extract_last_observed_times <- function(fit, n_series) {
  meta <- fit$trend_metadata
  time_var <- meta$variables$time_var %||% "time"
  series_var <- meta$variables$series_var %||% "series"
  d <- fit$obs_data %||% fit$data
  if (is.null(d) || is.null(d[[time_var]])) {
    return(rep(NA_real_, n_series))
  }
  if (!is.null(d[[series_var]])) {
    series_fac <- as.factor(d[[series_var]])
    out <- vapply(
      levels(series_fac)[seq_len(n_series)],
      function(lv) {
        ts <- d[[time_var]][series_fac == lv]
        if (length(ts) == 0L) NA_real_ else max(ts, na.rm = TRUE)
      },
      numeric(1L)
    )
    return(out)
  }
  rep(max(d[[time_var]], na.rm = TRUE), n_series)
}


# ZMVN: no temporal recursion. Each forecast step is an
# independent MVN draw, so the only params field is `Sigma`.
# `last_state$trends` is a zero-row matrix (max_lag = 0).
#'@noRd
extract_zmvn_state <- function(one_draw, meta, n_series, n_lv) {
  scov <- extract_sigma_and_cov(one_draw, n_series, n_lv,
                                 has_cor = TRUE)
  list(
    params = list(Sigma = scov$Sigma, sigma = scov$sigma),
    last_state = list(
      trends = matrix(0, nrow = 0L, ncol = n_series),
      errors = empty_errors(),
      linpreds = matrix(0, nrow = 0L, ncol = n_series)
    )
  )
}
