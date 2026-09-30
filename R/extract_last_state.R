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
#'@noRd
extract_last_state <- function(fit, draw_id, draws_mat = NULL) {
  checkmate::assert_class(fit, "mvgam")
  checkmate::assert_int(draw_id, lower = 1L)

  meta <- fit$trend_metadata
  if (is.null(meta$trend_type)) return(NULL)

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
  # One draw as a plain one-row matrix, the shape
  # `read_draws_vector()` and `read_draws_matrix()` take.
  one_draw <- matrix(as.numeric(draws_mat[draw_id, ]), nrow = 1L,
                     dimnames = list(NULL, colnames(draws_mat)))

  n_series <- as.integer(fit$standata$N_series_trend %||% 1L)
  n_lv <- as.integer(fit$standata$N_lv_trend %||% n_series)

  # A factor model runs its recursion in n_lv-dimensional latent
  # space, and a per-draw `[n_series, n_lv]` Z projects the
  # propagated trajectory back to observed series scale. Every trend
  # the registry marks as factor-capable extracts at that grain, and
  # model building refuses a factor model on any other.
  # `forecast.mvgam`:`propagate_one_draw` applies the
  # projection once `propagate_trend()` returns.
  #
  # Comparing the two Stan dimensions cannot answer this. A factor
  # fit reaches `n_lv = n_series` under an MGP loadings prior or a
  # `by = lv_axis()` term, and a non-factor fit carries the same
  # pair with an identity Z, so the comparison reads a genuine
  # factor fit as series-grain and hands `ar1_trend[k]`, indexed by
  # latent column, to the recursion for series k.
  is_factor <- !is.null(detect_factor_n_lv(fit, n_series))
  if (n_lv > n_series) {
    stop_mvgam_fault(
      "The trend has more latent states than series.",
      paste0("Got n_lv = ", n_lv, ", n_series = ", n_series, ".")
    )
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
  # `Phi_trend`, `sigma_trend`, `L_Omega_trend` -- is stated for the
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
  draw_block(one_draw, state_var, n_time, n_series,
             rows = seq.int(n_time - max_lag + 1L, n_time))
}


# Internal: build the diagonal AR-coefficient matrix per active
# lag from a `one_draw` row. For each lag k in `ar_lags`, pulls
# `ar<k>_trend[1..n_lv]` and constructs a length-`n_series`
# vector (broadcasting the single-element `n_lv == 1` shared-AR
# case to all series).
#'@noRd
extract_ar_coefs <- function(one_draw, ar_lags, n_series, n_lv) {
  out <- matrix(0, nrow = length(ar_lags), ncol = n_series)
  for (k in seq_along(ar_lags)) {
    out[k, ] <- broadcast_to_series(
      draw_vector(one_draw, paste0("ar", ar_lags[k], "_trend"), n_lv),
      n_series
    )
  }
  out
}


# Internal: a parameter vector at the grain of the state being
# stepped. One value is a coefficient shared across series and
# repeats. A factor fit arrives with `n_series` set to its latent
# count, and every other fit carries one value per series, which
# leaves any other length a fault in the caller.
#'@noRd
broadcast_to_series <- function(vec, n_series) {
  if (length(vec) == n_series) return(vec)
  if (length(vec) == 1L) return(rep(vec, n_series))
  stop(insight::format_error(c(
    "A trend parameter's length differs from the state it describes.",
    x = paste0("Values: ", length(vec), ", series: ", n_series, "."),
    i = "Each series takes its own value or all share one."
  )), call. = FALSE)
}


# Internal: one draw's vector parameter, `prefix[1..n]` or
# `prefix[lead, 1..n]`, through the reader every draws consumer
# shares. `one_draw` is a one-row draws matrix.
#'@noRd
draw_vector <- function(one_draw, prefix, n, lead = NULL) {
  as.numeric(read_draws_vector(one_draw, prefix, n, lead = lead))
}


# Internal: one draw's matrix parameter as a `length(rows) x n_col`
# matrix. `lead` selects one slot of an array of matrices.
#'@noRd
draw_block <- function(one_draw, prefix, n_row, n_col, lead = NULL,
                       rows = seq_len(n_row)) {
  matrix(read_draws_matrix(one_draw, prefix, n_row, n_col, lead = lead,
                           rows = rows),
         nrow = length(rows), ncol = n_col)
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
        i = "Hierarchical scales are indexed by observed series."
      )))
    }
    return(extract_hierarchical_sigma_and_cov(
      one_draw, n_series, group_info
    ))
  }
  sigma_vec <- broadcast_to_series(
    draw_vector(one_draw, "sigma_trend", n_lv), n_series
  )
  Sigma <- if (has_cor) {
    L <- draw_block(one_draw, "L_Omega_trend", n_series, n_series)
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
    draw_block(one_draw, prefix, n_sub, n_sub, lead = group)
  }

  alpha <- draws_columns(one_draw, HIER_COV_PARS$alpha,
                         HIER_COV_PARS$alpha)
  L_global <- read_matrix(HIER_COV_PARS$global)

  sigma_vec <- numeric(n_series)
  Sigma <- matrix(0, nrow = n_series, ncol = n_series)
  for (g in seq_len(n_groups)) {
    sigma_g <- draw_vector(one_draw, HIER_COV_PARS$sigma, n_sub, lead = g)
    L_dev <- read_matrix(HIER_COV_PARS$deviation, group = g)
    L_full <- hierarchical_group_cholesky(
      alpha = alpha, L_global = L_global, L_deviation = L_dev,
      sigma = sigma_g
    )
    # A group's k-th row belongs to its k-th member series.
    members <- group_info$members[[g]]
    sigma_vec[members] <- sigma_g
    Sigma[members, members] <- tcrossprod(L_full)
  }
  list(sigma = sigma_vec, Sigma = Sigma)
}


# Internal: pull `theta1_trend` per series when the trend has an
# MA component. Returns a length-`n_series` numeric vector.
#'@noRd
extract_ma_coefs <- function(one_draw, n_series, n_lv) {
  broadcast_to_series(draw_vector(one_draw, "theta1_trend", n_lv),
                      n_series)
}


# Internal: the last `max_ma` rows of the scaled innovation matrix
# `scaled_innovations_trend[t, j]` from a single posterior draw.
# The kernel uses these as the past `errors[t - j]` terms when
# propagating forward, and each one is the innovation that draw
# sampled at that occasion.
#'@noRd
extract_innovation_history <- function(one_draw, n_series, n_lv,
                                       max_ma, n_time) {
  if (max_ma == 0L) return(NULL)
  # The kernel multiplies its MA coefficient by a past innovation,
  # which is what `scaled_innovations_trend` holds.
  # `ma_innovations_trend` holds the moving average already formed
  # from it, and seeding with that would apply the coefficient twice.
  draw_block(one_draw, "scaled_innovations_trend", n_time, n_series,
             rows = seq.int(n_time - max_ma + 1L, n_time))
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
      errors = extract_innovation_history(one_draw, n_series, n_lv,
                                          max_ma, n_time)
    )
  )
}


# VAR(p) and VARMA(p, 1). Pulls the stationary coefficient matrix
# `Phi_trend[lag, i, j]` of each lag, which the kernel uses as it is,
# the sigma and Sigma pair, and the moving-average matrix
# `Theta_trend[1, i, j]` when present.
# VAR fits always have `cor = TRUE` (multivariate innovations).
#'@noRd
extract_var_state <- function(one_draw, meta, n_series, n_lv, fit,
                                state_var = "trend") {
  n_time <- as.integer(fit$standata$N_time_trend)
  m_a <- length(meta$ar_lags)
  # Stan declares Phi_trend as `array[N_lags_trend] matrix[N_lv,
  # N_lv]`, and its draws are indexed `Phi_trend[lag, i, j]`: the
  # array slot first, then the matrix row and column. We pack into an
  # [n_series, n_series, n_lags] R cube matching the contract
  # of `propagate_trend(..., params = list(A = ...))`.
  A_cube <- array(0, dim = c(n_series, n_series, m_a))
  for (k in seq_len(m_a)) {
    A_cube[, , k] <- draw_block(one_draw, "Phi_trend", n_series, n_series,
                                lead = meta$ar_lags[k])
  }
  scov <- extract_sigma_and_cov(one_draw, n_series, n_lv,
                                 has_cor = TRUE, standata = fit$standata)
  params <- list(A = A_cube,
                  sigma = scov$sigma,
                  Sigma = scov$Sigma)
  errors <- NULL
  if (length(meta$ma_lags) > 0L) {
    # The program applies `Theta_trend`, the stationary transform of
    # `D_trend`, and indexes it `Theta_trend[lag, i, j]` like
    # `Phi_trend`.
    m_b <- length(meta$ma_lags)
    B_cube <- array(0, dim = c(n_series, n_series, m_b))
    for (j_lag in seq_len(m_b)) {
      B_cube[, , j_lag] <- draw_block(one_draw, "Theta_trend", n_series,
                                      n_series, lead = meta$ma_lags[j_lag])
    }
    params$theta_cube <- B_cube
    errors <- varma_last_innovation(one_draw, A_cube, B_cube[, , 1L],
                                    meta$ar_lags, n_series, n_time)
  }
  list(
    params = params,
    last_state = list(
      trends = extract_trend_history(one_draw, n_series, n_lv,
                                       meta$max_lag, n_time,
                                       state_var = state_var),
      errors = errors
    )
  )
}


# Internal: the innovation at the last fitted occasion of a
# VARMA(p, 1), which the first forecast step multiplies by `D`.
#
# The program never stores it. Its likelihood centres `lv_t` on
#   mu_t = sum_i A_i lv_{t-i} + D e_{t-1},  e_t = lv_t - mu_t,
# taking a state before the first occasion from `init_trend`. That
# vector follows `Omega_trend`'s companion order, most recent first:
# block `k <= p` holds `lv_{1-k}` and block `p + 1` holds `e_0`. The
# same recursion run forward here gives `e_T`.
#
# @return A `1 x n` matrix holding `e_T`
#'@noRd
varma_last_innovation <- function(one_draw, A_cube, D, ar_lags, n,
                                  n_time) {
  p <- max(ar_lags)
  init <- draw_vector(one_draw, "init_trend", (p + 1L) * n)
  init_block <- function(k) init[((k - 1L) * n + 1L):(k * n)]
  lv <- draw_block(one_draw, "lv_trend", n_time, n)
  e_prev <- init_block(p + 1L)
  for (t in seq_len(n_time)) {
    mu <- as.numeric(D %*% e_prev)
    for (k in seq_along(ar_lags)) {
      i <- ar_lags[k]
      lagged <- if (t - i <= 0L) init_block(i - t + 1L) else lv[t - i, ]
      mu <- mu + as.numeric(A_cube[, , k] %*% lagged)
    }
    e_prev <- lv[t, ] - mu
  }
  matrix(e_prev, nrow = 1L)
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
  phi <- broadcast_to_series(draw_vector(one_draw, "ar1_trend", n_lv),
                             n_series)
  sigma <- broadcast_to_series(draw_vector(one_draw, "sigma_trend", n_lv),
                               n_series)
  trends_hist <- extract_trend_history(
    one_draw, n_series, n_lv, max_lag = 1L, n_time = n_time
  )
  last_time <- extract_last_observed_times(fit, n_series)
  list(
    params = list(phi = phi, sigma = sigma),
    last_state = list(
      trends = trends_hist,
      errors = NULL,
      time = last_time
    )
  )
}


# Internal: the last time each series was observed at, in the order
# the trend numbers its columns. The axes record holds it.
#'@noRd
extract_last_observed_times <- function(fit, n_series) {
  recorded <- mvgam_axes(fit)$series$last_time
  # One entry per trend column. Subsetting to `n_series` would pad
  # with `NA` or drop a series without saying which.
  if (length(recorded) != n_series) {
    stop_mvgam_fault(
      "The recorded last times must span the trend's series.",
      paste0("Recorded: ", length(recorded), ", series: ", n_series, ".")
    )
  }
  as.numeric(recorded)
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
      errors = NULL
    )
  )
}


# PW (piecewise linear / logistic): pulls the per-series
# growth (`k_trend[s]`) and the changepoint-effect matrix
# (`delta_trend[i, s]`) from the posterior, plus the fit-time
# changepoint times `t_change_trend` from `standata`. The
# latter is a fixed quantity, set at fit time on a regular
# grid over the training history. A logistic fit adds the
# `m_trend[s]` offset. The `cap_trend` data array of a
# logistic fit is also pulled here, which saves the forecast
# caller a second pass over standata.
#'@noRd
extract_pw_state <- function(one_draw, meta, n_series, n_lv,
                                fit) {
  k_vec <- broadcast_to_series(draw_vector(one_draw, "k_trend", n_lv),
                               n_series)
  # The logistic form samples `m_trend`. For the linear form, the
  # observation formula supplies the level and the offset is zero.
  # `meta` is the fit's `trend_metadata`, which persists the growth.
  m_vec <- if (pw_is_logistic(meta)) {
    broadcast_to_series(draw_vector(one_draw, "m_trend", n_lv), n_series)
  } else {
    rep(0, n_series)
  }

  # `delta_trend` is declared `matrix[N_change_trend, N_lv_trend]`
  # in Stan, so the posterior names are `delta_trend[i, j]` with
  # `i` the changepoint index and `j` the latent series.
  n_change <- as.integer(fit$standata$N_change_trend %||% 0L)
  delta <- matrix(0, nrow = n_change, ncol = n_series)
  if (n_change > 0L) {
    raw <- draw_block(one_draw, "delta_trend", n_change, n_lv)
    for (i in seq_len(n_change)) {
      delta[i, ] <- broadcast_to_series(raw[i, ], n_series)
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
      errors = NULL,
      cap_train = cap
    )
  )
}
