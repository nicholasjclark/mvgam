# Last-state extraction for forecast.mvgam. One dispatcher calls a
# per-trend extractor over every draw of a forecast at once. Each
# extractor returns the arrays `last_state_of_draw()` slices into the
# `params` and `last_state` lists `propagate_trend()` takes.
#
# Every trend defines `extract_<type>_state(draws, meta, n_series,
# n_lv, fit, state_var)`. `draws` holds one row per forecast draw. The
# extractor returns
#   list(
#     params = <named list, see propagate_trend(@details)>,
#     fixed = <named list of parameters shared by every draw> | NULL,
#     last_state = list(
#       trends = array[ndraws, max_lag, n_series],
#       errors = array[ndraws, max_ma, n_series] | NULL
#     )
#   )
# The first dimension of every element of `params` and `last_state`
# indexes the draws. `n_series` is the dimension of the state stepped
# forward: the factor count of a factor model, the series count
# otherwise.


#' The draws a forecast propagates from
#'
#' The matrix has a column for every parameter of the fit. It adds
#' columns for the states the extractors below take: the trend or
#' factor path, the innovations of an MA trend, a VARMA's starting
#' states and the slope of a local linear trend.
#'
#' @param fit Fitted `mvgam` object
#' @param draw_ids Integer draw indices, or `NULL` for every draw
#' @return Numeric matrix, one row per draw
#' @noRd
forecast_draws <- function(fit, draw_ids = NULL) {
  checkmate::assert_class(fit, "mvgam")
  subset_draws_rows(
    as_plain_matrix(fit_parameter_draws(
      fit$fit,
      states = c("trend", "lv_trend", "scaled_innovations_trend",
                 "init_trend", "slope_trend")
    )),
    draw_ids = draw_ids
  )
}


#' The trend parameters and last states of a forecast's draws
#'
#' @param fit Fitted `mvgam` object
#' @param draws_mat Matrix from `forecast_draws()`, one row per draw to
#'   extract, or `NULL` to build it from every draw of the fit
#' @return List holding `params`, `fixed` and `last_state` as the
#'   extractors return them. A factor fit adds `n_lv_active`. A fit
#'   without a trend gives `NULL`.
#' @noRd
extract_last_states <- function(fit, draws_mat = NULL) {
  checkmate::assert_class(fit, "mvgam")
  checkmate::assert_matrix(draws_mat, min.rows = 1L, null.ok = TRUE)

  meta <- fit$trend_metadata
  if (is.null(meta$trend_type)) return(NULL)

  if (is.null(draws_mat)) {
    draws_mat <- forecast_draws(fit)
  }

  n_series <- as.integer(fit$standata$N_series_trend %||% 1L)
  n_lv <- as.integer(fit$standata$N_lv_trend %||% n_series)

  # A factor model runs its recursion in the n_lv-dimensional latent
  # space. A per-draw `[n_series, n_lv]` Z maps the propagated
  # trajectory to the series scale. `propagate_one_draw()` in
  # forecast.mvgam.R applies it as `fc_state %*% t(Z_slice)` after
  # `propagate_trend()` returns. Every trend the registry marks as
  # factor-capable extracts at the latent grain. Model building
  # refuses a factor model on any other trend.
  #
  # The two Stan dimensions cannot identify a factor fit. A factor fit
  # has `n_lv = n_series` under an MGP loadings prior or a
  # `by = lv_axis()` term. A fit without factors has the same pair with
  # an identity Z. A test on the dimensions would treat that factor
  # fit as series-grain. The recursion for series k would then receive
  # `ar1_trend[k]`, which is indexed by latent column.
  is_factor <- !is.null(detect_factor_n_lv(fit, n_series))
  if (n_lv > n_series) {
    stop_mvgam_fault(
      "The trend has more latent states than series.",
      paste0("Got n_lv = ", n_lv, ", n_series = ", n_series, ".")
    )
  }
  # In factor mode the extractors take the latent-grain parameters
  # (`lv_trend[t, k]`, `sigma_trend[k]`, `ar_trend[k]`).
  #
  # A QR-identified fit also stores `lv_trend_tilde` with a rotated
  # `Z_tilde`. The products `Z lv` and `Z_tilde lv_tilde` are equal.
  # Only a matching pair reconstructs the trend. The forecast uses the
  # model basis throughout. The other quantities the recursion uses
  # (`ar<k>_trend`, `Phi_trend`, `sigma_trend`, `L_Omega_trend`) are
  # stated for the sampled `Z`. A rotated state would mix two bases.
  state_dim <- if (is_factor) n_lv else n_series
  state_var <- if (is_factor) "lv_trend" else "trend"

  # Each trend's extractor takes the state at its own grain. A trend
  # without factors has `state_dim` equal to the series count.
  out <- trend_method(meta$trend_type, trend_method_templates[["last_state"]])(
    draws_mat, meta, state_dim, n_lv, fit, state_var = state_var
  )
  # The degrees of freedom each draw's innovations take, absent for a
  # Gaussian trend. Forecast innovations then follow the law the model
  # was fitted with.
  out$params$df <- extract_nu_trend_draws(draws_mat, fit)
  # The caller applies the Z projection when `n_lv_active` is set.
  if (is_factor) {
    out$n_lv_active <- n_lv
  }
  out
}


#' The trend parameters and last state of one draw
#'
#' @param states List from `extract_last_states()`
#' @param i Integer row of the draw in the matrix the states came from
#' @return List holding `params` and `last_state` as `propagate_trend()`
#'   takes them. A factor fit adds `n_lv_active`.
#' @noRd
last_state_of_draw <- function(states, i) {
  checkmate::assert_list(states)
  checkmate::assert_int(i, lower = 1L)
  slice <- function(x) lapply(x, draw_slice, i = i)
  out <- list(
    params = c(slice(states$params), states$fixed),
    last_state = slice(states$last_state)
  )
  out$n_lv_active <- states$n_lv_active
  out
}


#' One draw of an array whose first dimension holds the draws
#'
#' A vector gives the draw's scalar. A matrix gives its row. A higher
#' array gives the draw's array with the first dimension dropped,
#' whatever the extent of the others.
#'
#' @param x Numeric vector or array, or `NULL`
#' @param i Integer draw row
#' @return The draw's value, or `NULL`
#' @noRd
draw_slice <- function(x, i) {
  d <- dim(x)
  if (is.null(d)) {
    return(x[i])
  }
  if (length(d) == 2L) {
    return(x[i, ])
  }
  # Column-major storage puts draw `i` of each cell `i` positions into
  # a block of `d[1]`
  array(x[(seq_len(prod(d[-1L])) - 1L) * d[1L] + i], dim = d[-1L])
}


# ----- per-trend extraction helpers -------------------------------

#' The last rows of each draw's latent trend
#'
#' The `trend[t, s]` grid includes `mu_trend`. The caller subtracts the
#' trend linear predictor to recover the zero-mean state the recursion
#' advances.
#'
#' @param draws Draws matrix named by parameter, one row per draw
#' @param n_series Integer number of state columns
#' @param max_lag Integer number of rows to return
#' @param n_time Integer number of fitted time points
#' @param state_var Character parameter name, `"trend"` for the series
#'   grain or `"lv_trend"` for the factor grain
#' @return Numeric array `[ndraws, max_lag, n_series]`
#' @noRd
extract_trend_history <- function(draws, n_series, max_lag, n_time,
                                  state_var = "trend") {
  checkmate::assert_matrix(draws, min.rows = 1L)
  checkmate::assert_int(n_series, lower = 1L)
  checkmate::assert_int(max_lag, lower = 0L)
  checkmate::assert_int(n_time, lower = 1L)
  checkmate::assert_string(state_var)
  if (max_lag == 0L) {
    return(empty_history(draws, n_series))
  }
  read_draws_matrix(draws, state_var, n_time, n_series,
                    rows = seq.int(n_time - max_lag + 1L, n_time))
}


#' A history of no rows for every draw
#'
#' @param draws Draws matrix, one row per draw
#' @param n_series Integer number of state columns
#' @return Numeric array `[ndraws, 0, n_series]`
#' @noRd
empty_history <- function(draws, n_series) {
  array(0, dim = c(nrow(draws), 0L, n_series))
}


#' Each draw's AR coefficients at each active lag
#'
#' Takes `ar<k>_trend` for each lag `k`. A single shared coefficient is
#' repeated over the series.
#'
#' @param draws Draws matrix named by parameter, one row per draw
#' @param ar_lags Integer vector of active AR lags
#' @param n_series Integer number of state columns
#' @param n_lv Integer number of stored coefficients per lag
#' @return Numeric array `[ndraws, length(ar_lags), n_series]`
#' @noRd
extract_ar_coefs <- function(draws, ar_lags, n_series, n_lv) {
  checkmate::assert_matrix(draws, min.rows = 1L)
  checkmate::assert_integerish(ar_lags, lower = 1)
  checkmate::assert_int(n_series, lower = 1L)
  checkmate::assert_int(n_lv, lower = 1L)
  out <- array(0, dim = c(nrow(draws), length(ar_lags), n_series))
  for (k in seq_along(ar_lags)) {
    out[, k, ] <- broadcast_to_series(
      read_draws_vector(draws, paste0("ar", ar_lags[k], "_trend"), n_lv),
      n_series
    )
  }
  out
}


#' A parameter at the grain of the state being stepped
#'
#' A single column is a coefficient shared across series and repeats. A
#' factor fit passes its latent count as `n_series`. Any number of
#' columns other than 1 or `n_series` is an error.
#'
#' @param vals Numeric matrix, one row per draw
#' @param n_series Integer number of state columns
#' @return Numeric matrix `[ndraws, n_series]`
#' @noRd
broadcast_to_series <- function(vals, n_series) {
  checkmate::assert_matrix(vals, mode = "numeric", min.cols = 1L)
  checkmate::assert_int(n_series, lower = 1L)
  if (ncol(vals) == n_series) return(vals)
  if (ncol(vals) == 1L) return(vals[, rep(1L, n_series), drop = FALSE])
  stop(insight::format_error(c(
    "A trend parameter's length differs from the state it describes.",
    x = paste0("Values: ", ncol(vals), ", series: ", n_series, "."),
    i = "Each series takes its own value or all share one."
  )), call. = FALSE)
}


#' Each draw's innovation scales and covariance
#'
#' Takes `sigma_trend`, then builds the covariance from `Sigma_trend`
#' or `L_Omega_trend` when the trend is correlated. A grouped trend has
#' neither parameter. Its scales are `sigma_group_trend[g, k]` and its
#' correlations combine a population factor with a per-group deviation.
#' Groups are independent, giving a block-diagonal covariance.
#'
#' @param draws Draws matrix named by parameter, one row per draw
#' @param n_series Integer number of state columns
#' @param n_lv Integer number of stored scales
#' @param has_cor Logical. `TRUE` for correlated innovations. `NULL`
#'   counts as `FALSE`.
#' @param standata Stan data list of the fit, or `NULL`
#' @return List holding the scales `sigma` `[ndraws, n_series]` and the
#'   covariances `Sigma` `[ndraws, n_series, n_series]`
#' @noRd
extract_sigma_and_cov <- function(draws, n_series, n_lv,
                                  has_cor, standata = NULL) {
  checkmate::assert_matrix(draws, min.rows = 1L)
  checkmate::assert_int(n_series, lower = 1L)
  checkmate::assert_int(n_lv, lower = 1L)
  checkmate::assert_flag(has_cor, null.ok = TRUE)
  checkmate::assert_list(standata, null.ok = TRUE)
  has_cor <- isTRUE(has_cor)
  group_info <- hierarchical_group_info(standata)
  if (!is.null(group_info)) {
    # Hierarchical scales are indexed by observed series. A grouped
    # factor model would request the latent grain. Model building
    # refuses that combination.
    if (!identical(as.integer(n_series),
                   length(group_info$group_inds))) {
      stop_mvgam_fault(
        "Grouped trend covariance was requested at the latent grain.",
        paste0("Got n = ", n_series, ", group indices = ",
               length(group_info$group_inds), ".")
      )
    }
    return(extract_hierarchical_sigma_and_cov(draws, n_series, group_info))
  }
  sigma <- broadcast_to_series(
    read_draws_vector(draws, "sigma_trend", n_lv), n_series
  )
  if (has_cor && stores_innovation_cov(draws)) {
    return(list(
      sigma = sigma,
      Sigma = read_draws_matrix(draws, "Sigma_trend", n_series, n_series)
    ))
  }
  Sigma <- array(0, dim = c(nrow(draws), n_series, n_series))
  if (has_cor) {
    L <- read_draws_matrix(draws, "L_Omega_trend", n_series, n_series)
    for (d in seq_len(nrow(draws))) {
      # The row and column products scale `L L'` for a vector of any
      # length. `diag(x)` with a length-one x builds an x-by-x identity.
      Sigma[d, , ] <- sigma[d, ] * tcrossprod(draw_slice(L, d)) *
        rep(sigma[d, ], each = n_series)
    }
  } else {
    for (s in seq_len(n_series)) {
      Sigma[, s, s] <- sigma[, s]^2
    }
  }
  list(sigma = sigma, Sigma = Sigma)
}


#' The group structure of a hierarchical trend
#'
#' The Stan data records the group index of each series.
#'
#' @param standata Stan data list of the fit, or `NULL`
#' @return Group information from `get_group_info()`, or `NULL` for a
#'   trend without groups
#' @noRd
hierarchical_group_info <- function(standata) {
  checkmate::assert_list(standata, null.ok = TRUE)
  if (is.null(standata) || is.null(standata$N_groups_trend)) {
    return(NULL)
  }
  get_group_info(standata)
}


#' Each draw's scales and block-diagonal covariance of a grouped trend
#'
#' @param draws Draws matrix named by parameter, one row per draw
#' @param n_series Integer number of series
#' @param group_info Group information from `get_group_info()`
#' @return List holding the scales `sigma` `[ndraws, n_series]` and the
#'   covariances `Sigma` `[ndraws, n_series, n_series]`
#' @noRd
extract_hierarchical_sigma_and_cov <- function(draws, n_series,
                                               group_info) {
  checkmate::assert_matrix(draws, min.rows = 1L)
  checkmate::assert_int(n_series, lower = 1L)
  checkmate::assert_list(group_info)
  n_groups <- as.integer(group_info$n_groups)
  n_sub <- as.integer(group_info$n_subgroups)
  group_inds <- as.integer(group_info$group_inds)
  checkmate::assert_integerish(
    group_inds, lower = 1, upper = n_groups, len = n_series,
    any.missing = FALSE
  )
  ndraws <- nrow(draws)
  alpha <- draws_columns(draws, HIER_COV_PARS$alpha, HIER_COV_PARS$alpha)
  L_global <- read_draws_matrix(draws, HIER_COV_PARS$global, n_sub, n_sub)
  sigma <- matrix(0, nrow = ndraws, ncol = n_series)
  Sigma <- array(0, dim = c(ndraws, n_series, n_series))
  for (g in seq_len(n_groups)) {
    sigma_g <- read_draws_vector(draws, HIER_COV_PARS$sigma, n_sub,
                                 lead = g)
    L_dev <- read_draws_matrix(draws, HIER_COV_PARS$deviation, n_sub,
                               n_sub, lead = g)
    # A group's k-th row belongs to its k-th member series.
    members <- group_info$members[[g]]
    sigma[, members] <- sigma_g
    for (d in seq_len(ndraws)) {
      L_full <- hierarchical_group_cholesky(
        alpha = alpha[d], L_global = draw_slice(L_global, d),
        L_deviation = draw_slice(L_dev, d), sigma = sigma_g[d, ]
      )
      Sigma[d, members, members] <- tcrossprod(L_full)
    }
  }
  list(sigma = sigma, Sigma = Sigma)
}


#' The last innovations of each draw
#'
#' Takes the rows of `scaled_innovations_trend`, the innovations the
#' draw sampled. The kernel multiplies them by the MA coefficient as
#' past errors. `ma_innovations_trend` holds the moving average already
#' formed and would apply the coefficient twice.
#'
#' @param draws Draws matrix named by parameter, one row per draw
#' @param n_series Integer number of state columns
#' @param max_ma Integer number of rows to return
#' @param n_time Integer number of fitted time points
#' @return Numeric array `[ndraws, max_ma, n_series]`, or `NULL` when
#'   `max_ma` is 0
#' @noRd
extract_innovation_history <- function(draws, n_series, max_ma, n_time) {
  checkmate::assert_matrix(draws, min.rows = 1L)
  checkmate::assert_int(n_series, lower = 1L)
  checkmate::assert_int(max_ma, lower = 0L)
  checkmate::assert_int(n_time, lower = 1L)
  if (max_ma == 0L) return(NULL)
  read_draws_matrix(draws, "scaled_innovations_trend", n_time, n_series,
                    rows = seq.int(n_time - max_ma + 1L, n_time))
}


#' Each draw's random walk parameters and last state
#'
#' The kernel fixes the random walk coefficient at 1. This extractor
#' skips the AR coefficients.
#'
#' @param draws Draws matrix named by parameter, one row per draw
#' @param meta Trend metadata list of the fit
#' @param n_series Integer number of state columns
#' @param n_lv Integer number of stored coefficients per parameter
#' @param fit Fitted `mvgam` object
#' @param state_var Character parameter name of the state grid
#' @return List holding `params` and `last_state`
#' @noRd
extract_rw_state <- function(draws, meta, n_series, n_lv, fit,
                             state_var = "trend") {
  extract_arma_state(draws, meta, n_series, n_lv, fit,
                     pull_ar = FALSE, state_var = state_var)
}


#' Each draw's AR parameters and last state
#'
#' @inheritParams extract_rw_state
#' @return List holding `params` and `last_state`
#' @noRd
extract_ar_state <- function(draws, meta, n_series, n_lv, fit,
                             state_var = "trend") {
  extract_arma_state(draws, meta, n_series, n_lv, fit,
                     pull_ar = TRUE, state_var = state_var)
}


#' Each draw's RW, AR or ARMA parameters and last state
#'
#' Takes the innovation scales, their covariance and the trend
#' history. An ARMA trend adds its MA coefficient. In factor mode the
#' caller passes the latent count as `n_series`. Every parameter then
#' has one column per latent factor.
#'
#' @inheritParams extract_rw_state
#' @param pull_ar Logical. `TRUE` adds the AR coefficient of each
#'   active lag.
#' @return List holding `params` and `last_state`
#' @noRd
extract_arma_state <- function(draws, meta, n_series, n_lv, fit,
                               pull_ar, state_var = "trend") {
  checkmate::assert_matrix(draws, min.rows = 1L)
  checkmate::assert_list(meta)
  checkmate::assert_int(n_series, lower = 1L)
  checkmate::assert_int(n_lv, lower = 1L)
  checkmate::assert_class(fit, "mvgam")
  checkmate::assert_flag(pull_ar)
  checkmate::assert_string(state_var)
  n_time <- as.integer(fit$standata$N_time_trend)
  scov <- extract_sigma_and_cov(draws, n_series, n_lv,
                                meta$has_cor, fit$standata)
  params <- list(sigma = scov$sigma, Sigma = scov$Sigma)
  if (pull_ar) {
    params$ar <- extract_ar_coefs(draws, meta$ar_lags, n_series, n_lv)
  }
  max_ma <- if (length(meta$ma_lags) > 0L) max(meta$ma_lags) else 0L
  if (length(meta$ma_lags) > 0L) {
    params$theta <- broadcast_to_series(
      read_draws_vector(draws, "theta1_trend", n_lv), n_series
    )
  }
  list(
    params = params,
    last_state = list(
      trends = extract_trend_history(draws, n_series, meta$max_lag,
                                     n_time, state_var = state_var),
      errors = extract_innovation_history(draws, n_series, max_ma, n_time)
    )
  )
}


#' Each draw's VAR or VARMA parameters and last state
#'
#' Takes the stationary coefficient matrix `Phi_trend[lag, i, j]` of
#' each lag. The kernel uses these matrices unchanged. A VARMA(p, 1)
#' trend adds the moving-average matrix `Theta_trend[1, i, j]`. VAR
#' innovations are always correlated.
#'
#' @inheritParams extract_rw_state
#' @return List holding `params` and `last_state`
#' @noRd
extract_var_state <- function(draws, meta, n_series, n_lv, fit,
                              state_var = "trend") {
  checkmate::assert_matrix(draws, min.rows = 1L)
  checkmate::assert_list(meta)
  checkmate::assert_int(n_series, lower = 1L)
  checkmate::assert_int(n_lv, lower = 1L)
  checkmate::assert_class(fit, "mvgam")
  checkmate::assert_string(state_var)
  n_time <- as.integer(fit$standata$N_time_trend)
  # Stan declares Phi_trend as `array[N_lags_trend] matrix[N_lv,
  # N_lv]`. Its draws are indexed `Phi_trend[lag, i, j]`: the array
  # slot first, then the matrix row and column. A draw's slice of the
  # array built here is the `[n_series, n_series, n_lags]` cube
  # `propagate_trend(..., params = list(A = ...))` takes.
  lag_cube <- function(name, lags) {
    out <- array(0, dim = c(nrow(draws), n_series, n_series, length(lags)))
    for (k in seq_along(lags)) {
      out[, , , k] <- read_draws_matrix(draws, name, n_series, n_series,
                                        lead = lags[k])
    }
    out
  }
  A <- lag_cube("Phi_trend", meta$ar_lags)
  scov <- extract_sigma_and_cov(draws, n_series, n_lv,
                                has_cor = TRUE, standata = fit$standata)
  params <- list(A = A, sigma = scov$sigma, Sigma = scov$Sigma)
  errors <- NULL
  if (length(meta$ma_lags) > 0L) {
    # The program applies `Theta_trend`, the stationary transform of
    # `D_trend`. Its draws are indexed `Theta_trend[lag, i, j]` like
    # `Phi_trend`.
    B <- lag_cube("Theta_trend", meta$ma_lags)
    params$theta_cube <- B
    p <- max(meta$ar_lags)
    init <- read_draws_vector(draws, "init_trend", (p + 1L) * n_series)
    lv <- read_draws_matrix(draws, "lv_trend", n_time, n_series)
    errors <- array(0, dim = c(nrow(draws), 1L, n_series))
    for (d in seq_len(nrow(draws))) {
      errors[d, 1L, ] <- varma_last_innovation(
        init[d, ], draw_slice(lv, d), draw_slice(A, d),
        draw_slice(B, d)[, , 1L], meta$ar_lags
      )
    }
  }
  list(
    params = params,
    last_state = list(
      trends = extract_trend_history(draws, n_series, meta$max_lag,
                                     n_time, state_var = state_var),
      errors = errors
    )
  )
}


#' The innovation at the last fitted occasion of a VARMA(p, 1)
#'
#' The first forecast step multiplies this innovation by `D`. The Stan
#' program never stores it. Its likelihood centres `lv_t` on
#' `mu_t = sum_i A_i lv_{t-i} + D e_{t-1}`, with `e_t = lv_t - mu_t`.
#' States before the first occasion come from `init_trend`, which
#' follows the companion order of `Omega_trend` with the most recent
#' first. Block `k <= p` holds `lv_{1-k}` and block `p + 1` holds
#' `e_0`. Running the same recursion forward gives `e_T`.
#'
#' @param init Numeric vector of one draw's `init_trend`
#' @param lv Numeric matrix of one draw's `lv_trend`, `[n_time, n]`
#' @param A_cube Numeric array of AR matrices, one slice per lag
#' @param D Numeric MA matrix
#' @param ar_lags Integer vector of active AR lags
#' @return Numeric vector holding `e_T`
#' @noRd
varma_last_innovation <- function(init, lv, A_cube, D, ar_lags) {
  checkmate::assert_numeric(init, any.missing = FALSE)
  checkmate::assert_matrix(lv, mode = "numeric")
  checkmate::assert_array(A_cube, d = 3L)
  checkmate::assert_numeric(D)
  checkmate::assert_integerish(ar_lags, lower = 1, min.len = 1L)
  n <- ncol(lv)
  init_block <- function(k) init[((k - 1L) * n + 1L):(k * n)]
  e_prev <- init_block(max(ar_lags) + 1L)
  for (t in seq_len(nrow(lv))) {
    mu <- as.numeric(D %*% e_prev)
    for (k in seq_along(ar_lags)) {
      i <- ar_lags[k]
      lagged <- if (t - i <= 0L) init_block(i - t + 1L) else lv[t - i, ]
      mu <- mu + as.numeric(A_cube[, , k] %*% lagged)
    }
    e_prev <- lv[t, ] - mu
  }
  e_prev
}


#' Each draw's continuous-time AR(1) parameters and last state
#'
#' Takes `phi` from `ar1_trend[s]`, the scales from `sigma_trend[s]`
#' and the state at the last occasion of the fitted grid. A fit with
#' correlated innovations adds `Sigma`, their covariance over a gap of
#' one. `forecast_context_car()` measures the forecast gaps from that
#' last occasion.
#'
#' @inheritParams extract_rw_state
#' @return List holding `params` and `last_state`
#' @noRd
extract_car_state <- function(draws, meta, n_series, n_lv, fit,
                              state_var = "trend") {
  checkmate::assert_matrix(draws, min.rows = 1L)
  checkmate::assert_list(meta)
  checkmate::assert_int(n_series, lower = 1L)
  checkmate::assert_int(n_lv, lower = 1L)
  checkmate::assert_class(fit, "mvgam")
  checkmate::assert_string(state_var)
  n_time <- as.integer(fit$standata$N_time_trend)
  phi <- broadcast_to_series(read_draws_vector(draws, "ar1_trend", n_lv),
                             n_series)
  has_cor <- isTRUE(meta$has_cor)
  scov <- extract_sigma_and_cov(draws, n_series, n_lv, has_cor,
                                fit$standata)
  list(
    params = c(list(phi = phi, sigma = scov$sigma),
               if (has_cor) list(Sigma = scov$Sigma)),
    last_state = list(
      trends = extract_trend_history(draws, n_series, max_lag = 1L,
                                     n_time = n_time,
                                     state_var = state_var),
      errors = NULL
    )
  )
}


#' The last time each series was observed
#'
#' Taken from the axes record, in the order the trend numbers its
#' columns.
#'
#' @param fit Fitted `mvgam` object
#' @param n_series Integer number of trend series
#' @return Numeric vector of length `n_series`
#' @noRd
extract_last_observed_times <- function(fit, n_series) {
  checkmate::assert_class(fit, "mvgam")
  checkmate::assert_int(n_series, lower = 1L)
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


#' Each draw's local linear trend parameters and last state
#'
#' Takes the level and the slope at the last occasion, the level and
#' slope scales and the slope damping. The slope shocks share the
#' correlation of the level shocks. `cov2cor()` recovers it from the
#' level covariance, grouped or not. Sampled factor loadings give one
#' slope scale and one damping, broadcast to every factor.
#'
#' @inheritParams extract_rw_state
#' @return List holding `params` and `last_state`
#' @noRd
extract_llt_state <- function(draws, meta, n_series, n_lv, fit,
                              state_var = "trend") {
  checkmate::assert_matrix(draws, min.rows = 1L)
  checkmate::assert_list(meta)
  checkmate::assert_int(n_series, lower = 1L)
  checkmate::assert_int(n_lv, lower = 1L)
  checkmate::assert_class(fit, "mvgam")
  checkmate::assert_string(state_var)
  n_time <- as.integer(fit$standata$N_time_trend)
  scov <- extract_sigma_and_cov(draws, n_series, n_lv, meta$has_cor,
                                fit$standata)
  n_par <- function(prefix) {
    sum(startsWith(colnames(draws), paste0(prefix, "[")))
  }
  sigma_slope <- broadcast_to_series(
    read_draws_vector(draws, "sigma_slope_trend", n_par("sigma_slope_trend")),
    n_series
  )
  phi_slope <- if (n_par("ar1_slope_trend") > 0L) {
    broadcast_to_series(
      read_draws_vector(draws, "ar1_slope_trend", n_par("ar1_slope_trend")),
      n_series
    )
  } else {
    matrix(1, nrow = nrow(draws), ncol = n_series)
  }
  Sigma_slope <- scov$Sigma
  for (d in seq_len(nrow(draws))) {
    Sigma_slope[d, , ] <- sigma_slope[d, ] *
      stats::cov2cor(draw_slice(scov$Sigma, d)) *
      rep(sigma_slope[d, ], each = n_series)
  }
  slope <- read_draws_matrix(draws, "slope_trend", n_time, n_series,
                             rows = n_time)
  list(
    params = list(
      sigma = scov$sigma, Sigma = scov$Sigma, sigma_slope = sigma_slope,
      Sigma_slope = Sigma_slope, phi_slope = phi_slope
    ),
    last_state = list(
      trends = extract_trend_history(draws, n_series, max_lag = 1L,
                                     n_time = n_time,
                                     state_var = state_var),
      errors = NULL,
      slope = matrix(slope, nrow = nrow(draws), ncol = n_series)
    )
  )
}


#' Each draw's zero-mean multivariate normal trend parameters
#'
#' The trend has no temporal recursion. Each forecast step is an
#' independent multivariate normal draw with covariance `Sigma`.
#' `last_state$trends` has no rows. In factor mode `n_series` holds the
#' latent count. The extractor then takes `sigma_trend[1..n_lv]` and the
#' `n_lv` square `L_Omega_trend` that scale the latent draws.
#'
#' @inheritParams extract_rw_state
#' @return List holding `params` and `last_state`
#' @noRd
extract_zmvn_state <- function(draws, meta, n_series, n_lv, fit,
                               state_var = "trend") {
  checkmate::assert_matrix(draws, min.rows = 1L)
  checkmate::assert_list(meta)
  checkmate::assert_int(n_series, lower = 1L)
  checkmate::assert_int(n_lv, lower = 1L)
  checkmate::assert_class(fit, "mvgam")
  checkmate::assert_string(state_var)
  scov <- extract_sigma_and_cov(draws, n_series, n_lv,
                                has_cor = TRUE,
                                standata = fit$standata)
  list(
    params = list(Sigma = scov$Sigma, sigma = scov$sigma),
    last_state = list(trends = empty_history(draws, n_series),
                      errors = NULL)
  )
}


#' Each draw's piecewise trend parameters
#'
#' Takes the growth rate `k_trend[s]` and the changepoint effects
#' `delta_trend[i, s]` from the draws. The changepoint times
#' `t_change_trend` come from the Stan data, shared by every draw.
#' Model building fixes them on a regular grid over the training
#' history. A logistic fit adds the offset `m_trend[s]`.
#' `forecast_context_pw()` takes the capacities of the new times from
#' `newdata`.
#'
#' @inheritParams extract_rw_state
#' @param state_var Character parameter name of the state grid. The
#'   piecewise trend has no stored state and ignores it.
#' @return List holding `params`, `fixed` and `last_state`
#' @noRd
extract_pw_state <- function(draws, meta, n_series, n_lv, fit,
                             state_var = "trend") {
  checkmate::assert_matrix(draws, min.rows = 1L)
  checkmate::assert_list(meta)
  checkmate::assert_int(n_series, lower = 1L)
  checkmate::assert_int(n_lv, lower = 1L)
  checkmate::assert_class(fit, "mvgam")
  checkmate::assert_string(state_var)
  ndraws <- nrow(draws)
  k <- broadcast_to_series(read_draws_vector(draws, "k_trend", n_lv),
                           n_series)
  # The logistic form samples `m_trend`. For the linear form, the
  # observation formula supplies the level and the offset is zero.
  # `meta` is the fit's `trend_metadata`, which persists the growth.
  m <- if (pw_is_logistic(meta)) {
    broadcast_to_series(read_draws_vector(draws, "m_trend", n_lv),
                        n_series)
  } else {
    matrix(0, nrow = ndraws, ncol = n_series)
  }

  # Stan declares `delta_trend` as `matrix[N_change_trend, N_lv_trend]`.
  # Its draws are named `delta_trend[i, j]`, with `i` the changepoint
  # index and `j` the latent series.
  n_change <- as.integer(fit$standata$N_change_trend %||% 0L)
  delta <- array(0, dim = c(ndraws, n_change, n_series))
  if (n_change > 0L) {
    raw <- read_draws_matrix(draws, "delta_trend", n_change, n_lv)
    for (i in seq_len(n_change)) {
      delta[, i, ] <- broadcast_to_series(
        matrix(raw[, i, ], nrow = ndraws), n_series
      )
    }
  }
  list(
    params = list(k = k, m = m, delta = delta),
    fixed = list(
      t_change = as.numeric(fit$standata$t_change_trend %||% numeric(0L))
    ),
    last_state = list(trends = empty_history(draws, n_series),
                      errors = NULL)
  )
}
