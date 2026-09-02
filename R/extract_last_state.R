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
extract_last_state <- function(fit, draw_id, draws_mat = NULL) {
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
  #
  # Callers running a per-draw loop (forecast.mvgam) build the
  # matrix once and pass it via `draws_mat` to skip the
  # `as_draws_matrix` cost on every iteration.
  if (is.null(draws_mat)) {
    draws_mat <- posterior::as_draws_matrix(fit$fit)
  }
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

  # A factor model runs its recursion in n_lv-dimensional latent
  # space, and a per-draw `[n_series, n_lv]` Z projects the
  # propagated trajectory back to observed series scale. The RW,
  # AR, VAR and ZMVN extractors all take that grain; the whitelist
  # below refuses the trend types whose generators emit no factor
  # variant. `forecast.mvgam`:`propagate_one_draw` applies the
  # projection once `propagate_trend()` returns. Hierarchical fits
  # with `n_lv = n_groups * n_subgroups > n_series` need a
  # dedicated extraction path and error out below.
  #
  # Comparing the two Stan dimensions cannot answer this. A factor
  # fit reaches `n_lv = n_series` under an MGP loadings prior or a
  # `by = lv_axis()` term, and a non-factor fit carries the same
  # pair with an identity Z, so the comparison reads a genuine
  # factor fit as series-grain and hands `ar1_trend[k]`, indexed by
  # latent column, to the recursion for series k.
  is_factor <- !is.null(detect_factor_n_lv(fit, n_series))
  if (n_lv > n_series) {
    stop(insight::format_error(c(
      paste0(
        "Hierarchical trends (n_lv > n_series) are not yet ",
        "supported by 'extract_last_state'."
      ),
      x = paste0(
        "Got n_lv = ", n_lv, ", n_series = ", n_series, "."
      ),
      i = "Hierarchical trend support is pending."
    )))
  }
  if (is_factor &&
      !meta$trend_type %in% c("RW", "AR", "VAR", "ZMVN")) {
    stop(insight::format_error(c(
      paste0(
        "Factor trend variants of '", meta$trend_type,
        "' are not yet supported by 'extract_last_state'."
      ),
      x = paste0(
        "Got n_lv = ", n_lv, ", n_series = ", n_series, "."
      ),
      i = paste0(
        "Factor forecast support currently covers RW / AR / VAR ",
        "and ZMVN."
      )
    )))
  }

  # In factor mode extraction happens at the LV grain
  # (`lv_trend[t, k]`, `sigma_trend[k]`, `ar_trend[k]`), and the
  # caller projects the propagated `[h, n_lv]` trajectory back to
  # series scale via Z; see `apply_factor_projection()` in
  # forecast.mvgam.R.
  #
  # A QR-identified fit also stores `lv_trend_tilde` alongside a
  # rotated `Z_tilde`, and `Z lv` equals `Z_tilde lv_tilde`. Only
  # the matching pair reconstructs the trend, so the whole
  # forecast reads one basis. It is the model basis, because every
  # other quantity the recursion consumes -- `ar<k>_trend`,
  # `A_trend`, `sigma_trend`, `L_Omega_trend` -- is stated for the
  # `Z` the model sampled, and rotating the state alone would
  # leave those behind.
  state_dim <- if (is_factor) n_lv else n_series
  state_var <- if (is_factor) "lv_trend" else "trend"

  out <- switch(
    meta$trend_type,
    "RW" = extract_rw_state(one_draw, meta, state_dim, n_lv, fit,
                             state_var = state_var),
    "AR" = extract_ar_state(one_draw, meta, state_dim, n_lv, fit,
                             state_var = state_var),
    "VAR" = extract_var_state(one_draw, meta, state_dim, n_lv, fit,
                                state_var = state_var),
    "CAR" = extract_car_state(one_draw, meta, n_series, fit),
    "ZMVN" = extract_zmvn_state(one_draw, state_dim, n_lv, fit),
    "PW" = extract_pw_state(one_draw, meta, n_series, n_lv, fit),
    stop(insight::format_error(c(
      paste0(
        "Trend type '", meta$trend_type,
        "' is not supported by 'extract_last_state'."
      ),
      i = paste0(
        "Supported: 'RW', 'AR', 'VAR', 'CAR', 'ZMVN', 'PW'."
      )
    )))
  )
  # Tag the state with the LV dimensionality when non-trivial
  # so the caller knows to apply Z projection after
  # propagation.
  if (is_factor) {
    out$n_lv_active <- n_lv
  }
  out
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
# `n_series` columns. This grid carries `mu_trend`, so the caller
# subtracts the trend linear predictor to recover the zero-mean
# latent state that the recursion advances.
#'@noRd
extract_trend_history <- function(one_draw, n_series, n_lv, max_lag,
                                    n_time, state_var = "trend") {
  if (max_lag == 0L) {
    return(matrix(0, nrow = 0L, ncol = n_series))
  }
  # `state_var` selects the parameter grid this reads from:
  # `"trend"` for the series-grain full-rank fits (default), or
  # `"lv_trend"` when the caller is running a factor model and
  # wants the LV-grain state that `propagate_trend()` will step
  # forward before projecting back to series scale via Z.
  start_t <- n_time - max_lag + 1L
  out <- matrix(0, nrow = max_lag, ncol = n_series)
  for (t in seq_len(max_lag)) {
    abs_t <- start_t + t - 1L
    for (s in seq_len(n_series)) {
      nm <- paste0(state_var, "[", abs_t, ",", s, "]")
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
# full mapping contract for those cases requires hierarchical
# trend support that 'extract_last_state' does not yet have.
#'@noRd
broadcast_to_series <- function(vec, n_series) {
  if (length(vec) == n_series) return(vec)
  if (length(vec) == 1L) return(rep(vec, n_series))
  vec[seq_len(n_series)]
}


# Internal: pull `sigma_trend` per series and (optionally) build
# the innovation covariance `Sigma` from `L_Omega_trend`.
#
# A grouped trend carries neither of those. Its scales are
# `sigma_group_trend[g, k]` and its correlations are a population
# factor plus a per-group deviation, so it is read through the same
# helper the innovation transform uses and assembled as one
# block-diagonal covariance over the series, groups being independent.
#'@noRd
extract_sigma_and_cov <- function(one_draw, n_series, n_lv,
                                    has_cor, standata = NULL) {
  group_info <- hierarchical_group_info(standata)
  if (!is.null(group_info)) {
    # Hierarchical scales are indexed by observed series, so this
    # branch cannot answer a latent-grain request. No generator
    # emits that combination today; say so plainly if one ever
    # does, rather than failing on a length assertion downstream.
    if (!identical(as.integer(n_series),
                     length(group_info$group_inds))) {
      stop(insight::format_error(c(
        "Grouped trend covariance requested at the wrong grain.",
        x = paste0(
          "Got n = ", n_series, ", group indices = ",
          length(group_info$group_inds), "."
        ),
        i = paste0(
          "Hierarchical scales are per observed series, not per ",
          "latent factor."
        )
      )))
    }
    return(extract_hierarchical_sigma_and_cov(
      one_draw, n_series, group_info
    ))
  }
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
    # `diag(x)` for a length-one x builds an x-by-x identity rather
    # than a 1x1 matrix holding x, so the scaling is applied by row
    # and column instead.
    sigma_vec * tcrossprod(L) * rep(sigma_vec, each = n_series)
  } else {
    diag(sigma_vec^2, nrow = n_series)
  }
  list(sigma = sigma_vec, Sigma = Sigma)
}


# Internal: the group structure of a hierarchical trend, or NULL when
# the trend is not grouped. `standata` is the fit's Stan data, which is
# where the group index of each series is recorded.
#'@noRd
hierarchical_group_info <- function(standata) {
  if (is.null(standata) || is.null(standata$N_groups_trend)) {
    return(NULL)
  }
  get_group_info(standata)
}


# Internal: per-series scales and the block-diagonal innovation
# covariance of a grouped trend, for one posterior draw.
#'@noRd
extract_hierarchical_sigma_and_cov <- function(one_draw, n_series,
                                                group_info) {
  n_groups <- as.integer(group_info$n_groups)
  n_sub <- as.integer(group_info$n_subgroups)
  group_inds <- as.integer(group_info$group_inds)
  checkmate::assert_integerish(
    group_inds, lower = 1, upper = n_groups, len = n_series,
    any.missing = FALSE
  )

  read_matrix <- function(prefix, group = NULL) {
    nms <- stan_matrix_names(prefix, n_sub, n_sub, lead = group)
    matrix(as.numeric(one_draw[nms]), nrow = n_sub, ncol = n_sub)
  }

  alpha <- as.numeric(one_draw[[HIER_COV_PARS$alpha]])
  L_global <- read_matrix(HIER_COV_PARS$global)

  # Each series' position within its own group, which is the row of
  # that group's covariance the series occupies.
  within_pos <- as.integer(
    stats::ave(seq_along(group_inds), group_inds, FUN = seq_along)
  )

  sigma_vec <- numeric(n_series)
  Sigma <- matrix(0, nrow = n_series, ncol = n_series)
  for (g in seq_len(n_groups)) {
    sigma_g <- as.numeric(one_draw[
      stan_vector_names(HIER_COV_PARS$sigma, n_sub, lead = g)
    ])
    L_dev <- read_matrix(HIER_COV_PARS$deviation, group = g)
    L_full <- hierarchical_group_cholesky(
      alpha = alpha, L_global = L_global, L_deviation = L_dev,
      sigma = sigma_g
    )
    series_g <- which(group_inds == g)
    pos_g <- within_pos[series_g]
    sigma_vec[series_g] <- sigma_g[pos_g]
    Sigma[series_g, series_g] <- tcrossprod(L_full)[pos_g, pos_g,
                                                    drop = FALSE]
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
        # `ma_innovations_trend` is not in the posterior (e.g.
        # excluded via `exclude`). Returning NULL seeds the
        # kernel's MA history with zeros, which biases the
        # first forecast step by `theta * e[T]` for an ARMA
        # model. Emit a one-time warning so the caller knows
        # the fallback was taken.
        if (!identical(Sys.getenv("TESTTHAT"), "true")) {
          rlang::warn(
            paste0(
              "ARMA MA innovation 'ma_innovations_trend' is ",
              "not in the posterior; first forecast step uses ",
              "zero past innovations and may be biased."
            ),
            .frequency = "once",
            .frequency_id = "mvgam_ma_innov_missing"
          )
        }
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
extract_rw_state <- function(one_draw, meta, n_series, n_lv, fit,
                              state_var = "trend") {
  extract_arma_state(one_draw, meta, n_series, n_lv, fit,
                      pull_ar = FALSE, state_var = state_var)
}

#'@noRd
extract_ar_state <- function(one_draw, meta, n_series, n_lv, fit,
                              state_var = "trend") {
  extract_arma_state(one_draw, meta, n_series, n_lv, fit,
                      pull_ar = TRUE, state_var = state_var)
}

#'@noRd
extract_arma_state <- function(one_draw, meta, n_series, n_lv, fit,
                                pull_ar, state_var = "trend") {
  n_time <- as.integer(fit$standata$N_time_trend)
  # In factor mode the caller passes `n_series = n_lv` so the LV-
  # grain propagation is dimensionally consistent; the AR / sigma
  # extractors and `broadcast_to_series()` therefore return
  # length-n_lv vectors, which is what the kernel expects.
  scov <- extract_sigma_and_cov(one_draw, n_series, n_lv,
                                 meta$has_cor, fit$standata)
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
                                       meta$max_lag, n_time,
                                       state_var = state_var),
      errors = extract_ma_innovations(one_draw, n_series, n_lv,
                                        max_ma, n_time)
    )
  )
}


# VAR(p) and VARMA(p, 1). Pulls the Heaps-transformed
# `A_trend[i, j, lag]` array per active lag (the kernel reads
# this directly), the standard sigma + Sigma pair, and the
# MA coefficient cube from `D_raw_trend[i, j, 1]` when present.
# VAR fits always have `cor = TRUE` (multivariate innovations).
#'@noRd
extract_var_state <- function(one_draw, meta, n_series, n_lv, fit,
                                state_var = "trend") {
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
                                 has_cor = TRUE, standata = fit$standata)
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
                                       meta$max_lag, n_time,
                                       state_var = state_var),
      errors = empty_errors()
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
  d <- mvgam_training_data(fit)
  if (is.null(d) || is.null(d[[time_var]])) {
    return(rep(NA_real_, n_series))
  }
  # Entry `s` of the answer is the last time trend column `s` was
  # observed at, so the series have to be walked in the trend's own
  # order. Sorting the raw column instead gives a permutation of that
  # order, and truncating it to `n_series` hides the disagreement
  # rather than raising it.
  series_fac <- axis_row_series(fit, d)
  if (!is.null(series_fac)) {
    out <- vapply(
      levels(series_fac),
      function(lv) {
        ts <- d[[time_var]][which(series_fac == lv)]
        if (length(ts) == 0L) NA_real_ else max(ts, na.rm = TRUE)
      },
      numeric(1L)
    )
    return(out[seq_len(n_series)])
  }
  rep(max(d[[time_var]], na.rm = TRUE), n_series)
}


# ZMVN: no temporal recursion. Each forecast step is an
# independent MVN draw, so the only params field is `Sigma`.
# `last_state$trends` is a zero-row matrix (max_lag = 0).
#
# In factor mode `n_series` arrives as the LV count, so the same
# read gives `sigma_trend[1..n_lv]` and the `n_lv` square
# `L_Omega_trend` the latent draws are actually scaled by.
#'@noRd
extract_zmvn_state <- function(one_draw, n_series, n_lv, fit) {
  scov <- extract_sigma_and_cov(one_draw, n_series, n_lv,
                                 has_cor = TRUE,
                                 standata = fit$standata)
  list(
    params = list(Sigma = scov$Sigma, sigma = scov$sigma),
    last_state = list(
      trends = matrix(0, nrow = 0L, ncol = n_series),
      errors = empty_errors()
    )
  )
}


# PW (piecewise linear / logistic): pulls the per-series
# growth (`k_trend[s]`), intercept (`m_trend[s]`), and
# changepoint-effect matrix (`delta_trend[i, s]`) from the
# posterior, plus the fit-time changepoint times
# `t_change_trend` from `standata`. The latter is not a
# sampled parameter -- it is fixed at fit time on a regular
# grid over the training history. The `cap_trend` data array
# (logistic only) is also pulled here so the forecast caller
# can read it without re-touching standata.
#'@noRd
extract_pw_state <- function(one_draw, meta, n_series, n_lv,
                                fit) {
  k_nms <- paste0("k_trend[", seq_len(n_lv), "]")
  m_nms <- paste0("m_trend[", seq_len(n_lv), "]")
  k_vec <- broadcast_to_series(
    as.numeric(one_draw[k_nms]), n_series
  )
  m_vec <- broadcast_to_series(
    as.numeric(one_draw[m_nms]), n_series
  )

  # `delta_trend` is declared `matrix[N_change_trend, N_lv_trend]`
  # in Stan, so the posterior names are `delta_trend[i, j]` with
  # `i` the changepoint index and `j` the latent series.
  n_change <- as.integer(fit$standata$N_change_trend %||% 0L)
  delta <- matrix(0, nrow = n_change, ncol = n_series)
  if (n_change > 0L) {
    for (i in seq_len(n_change)) {
      for (j in seq_len(n_lv)) {
        nm <- paste0("delta_trend[", i, ",", j, "]")
        val <- one_draw[[nm]]
        if (is.null(val)) next
        # Broadcast n_lv = 1 to all series.
        if (n_lv == 1L) {
          delta[i, ] <- as.numeric(val)
        } else if (j <= n_series) {
          delta[i, j] <- as.numeric(val)
        }
      }
    }
  }
  t_change <- as.numeric(fit$standata$t_change_trend %||%
                           numeric(0L))
  # `cap_trend` is a fit-time `[N_time_trend, N_series_trend]`
  # data matrix (logistic only); store it on `last_state` so
  # the forecast caller can read it without re-traversing
  # standata. Linear fits store it as NULL.
  cap <- fit$standata$cap_trend
  list(
    params = list(
      k = k_vec,
      m = m_vec,
      delta = delta,
      t_change = t_change
    ),
    last_state = list(
      trends = matrix(0, nrow = 0L, ncol = n_series),
      errors = empty_errors(),
      cap_train = cap
    )
  )
}
