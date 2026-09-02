# Unit tests for `extract_last_state()` in
# `R/extract_last_state.R`. Each test builds a minimal mock
# mvgam fit (deterministic posterior draws + standata +
# enriched trend_metadata) and asserts the shape and values
# of the (`params`, `last_state`) tuple the function returns.
#
# Mocks let us exercise every branch of the per-trend
# dispatcher (RW / AR consecutive / AR sparse / ARMA / AR
# cor=TRUE / VAR / CAR / ZMVN) without any Stan compilation,
# so the tests run in milliseconds and live in CI.


# Build a mock draws_matrix from a named list of scalar /
# vector / matrix parameter values. The single posterior
# draw is fixed; tests assert on it directly.
make_draws <- function(values) {
  flat <- numeric(0)
  nms <- character(0)
  for (nm in names(values)) {
    v <- values[[nm]]
    if (is.matrix(v) || is.array(v)) {
      d <- dim(v)
      idx <- as.matrix(do.call(expand.grid,
                                 lapply(d, seq_len)))
      flat <- c(flat, as.numeric(v[idx]))
      labs <- apply(idx, 1, function(r) {
        paste0(nm, "[", paste(r, collapse = ","), "]")
      })
      nms <- c(nms, labs)
    } else if (length(v) == 1L) {
      flat <- c(flat, v)
      nms <- c(nms, nm)
    } else {
      flat <- c(flat, v)
      nms <- c(nms, paste0(nm, "[", seq_along(v), "]"))
    }
  }
  m <- matrix(flat, nrow = 1L,
               dimnames = list(NULL, nms))
  posterior::as_draws_matrix(m)
}


# Minimal mvgam-class object built around a draws_matrix.
# `n_lv` is the Stan trend dimension, which every fit carries and
# which equals the series count whenever no factors were asked for.
# `spec_n_lv` is the `n_lv` the user requested, which only a factor
# fit carries and which a real fit stores on its trend spec. The two
# are the same number on a factor fit and say different things
# everywhere else, so a mock that supplied only the first could not
# represent either kind faithfully.
make_mock_fit <- function(draws, n_series, n_lv, n_time,
                            trend_metadata,
                            obs_data = NULL,
                            spec_n_lv = NULL) {
  fit <- list(
    fit = draws,
    standata = list(
      N_series_trend = n_series,
      N_lv_trend = n_lv,
      N_time_trend = n_time
    ),
    mv_spec = list(trend_specs = structure(
      list(n_lv = spec_n_lv), class = "mvgam_trend"
    )),
    trend_metadata = trend_metadata,
    obs_data = obs_data
  )
  class(fit) <- "mvgam"
  fit
}


# ----- RW ---------------------------------------------------------

test_that("RW returns sigma + Sigma + 1-row trend history", {
  n_series <- 2L; n_time <- 5L
  trend_mat <- matrix(seq_len(n_time * n_series),
                       nrow = n_time, byrow = TRUE)
  draws <- make_draws(list(
    sigma_trend = c(0.3, 0.7),
    trend = trend_mat
  ))
  meta <- list(trend_type = "RW", ar_lags = 1L,
               ma_lags = integer(0), max_lag = 1L,
               has_cor = FALSE)
  fit <- make_mock_fit(draws, n_series, n_lv = 2L,
                        n_time, meta)
  res <- extract_last_state(fit, 1L)
  expect_named(res$params, c("sigma", "Sigma"))
  expect_equal(res$params$sigma, c(0.3, 0.7))
  expect_equal(diag(res$params$Sigma), c(0.09, 0.49))
  expect_identical(dim(res$last_state$trends), c(1L, 2L))
  # Last row of trend matrix is the (n_time, ·) slice.
  expect_equal(as.numeric(res$last_state$trends[1, ]),
                as.numeric(trend_mat[n_time, ]))
  expect_null(res$last_state$errors)
})


test_that("RW with cor=TRUE builds Sigma from L_Omega", {
  n_series <- 2L; n_time <- 3L
  L <- matrix(c(1, 0, 0.5, sqrt(1 - 0.5^2)),
               nrow = 2L, byrow = TRUE)
  draws <- make_draws(list(
    sigma_trend = c(1, 1),
    L_Omega_trend = L,
    trend = matrix(0, nrow = n_time, ncol = n_series)
  ))
  meta <- list(trend_type = "RW", ar_lags = 1L,
               ma_lags = integer(0), max_lag = 1L,
               has_cor = TRUE)
  fit <- make_mock_fit(draws, n_series, n_lv = 2L,
                        n_time, meta)
  res <- extract_last_state(fit, 1L)
  # sigma = c(1, 1) so Sigma = L L'.
  expected <- tcrossprod(L)
  expect_equal(unname(res$params$Sigma), expected,
                tolerance = 1e-12)
})


# ----- AR consecutive --------------------------------------------

test_that("AR(p = 1) pulls ar1_trend per series", {
  n_series <- 2L; n_time <- 5L
  draws <- make_draws(list(
    sigma_trend = c(0.5, 0.5),
    ar1_trend = c(0.4, 0.6),
    trend = matrix(seq_len(n_time * n_series),
                   nrow = n_time, byrow = TRUE)
  ))
  meta <- list(trend_type = "AR", ar_lags = 1L,
               ma_lags = integer(0), max_lag = 1L,
               has_cor = FALSE)
  fit <- make_mock_fit(draws, n_series, n_lv = 2L,
                        n_time, meta)
  res <- extract_last_state(fit, 1L)
  expect_named(res$params, c("sigma", "Sigma", "ar"))
  # ar is a [n_lags, n_series] matrix.
  expect_identical(dim(res$params$ar), c(1L, 2L))
  expect_equal(as.numeric(res$params$ar[1, ]), c(0.4, 0.6))
})


test_that("AR(p = 3) pulls ar1/ar2/ar3 per series", {
  n_series <- 2L; n_time <- 6L
  draws <- make_draws(list(
    sigma_trend = c(0.5, 0.5),
    ar1_trend = c(0.1, 0.2),
    ar2_trend = c(0.3, 0.4),
    ar3_trend = c(0.5, 0.6),
    trend = matrix(0, nrow = n_time, ncol = n_series)
  ))
  meta <- list(trend_type = "AR", ar_lags = c(1L, 2L, 3L),
               ma_lags = integer(0), max_lag = 3L,
               has_cor = FALSE)
  fit <- make_mock_fit(draws, n_series, n_lv = 2L,
                        n_time, meta)
  res <- extract_last_state(fit, 1L)
  expect_identical(dim(res$params$ar), c(3L, 2L))
  expect_equal(as.numeric(res$params$ar[1, ]), c(0.1, 0.2))
  expect_equal(as.numeric(res$params$ar[2, ]), c(0.3, 0.4))
  expect_equal(as.numeric(res$params$ar[3, ]), c(0.5, 0.6))
  expect_identical(dim(res$last_state$trends), c(3L, 2L))
})


# ----- AR sparse-lag ---------------------------------------------

test_that("AR(p = c(1, 3, 12)) pulls only the active lag params", {
  n_series <- 2L; n_time <- 15L
  draws <- make_draws(list(
    sigma_trend = c(0.5, 0.5),
    ar1_trend = c(0.7, 0.5),
    ar3_trend = c(-0.1, 0.05),
    ar12_trend = c(0.2, 0.3),
    trend = matrix(0, nrow = n_time, ncol = n_series)
  ))
  meta <- list(trend_type = "AR",
               ar_lags = c(1L, 3L, 12L),
               ma_lags = integer(0), max_lag = 12L,
               has_cor = FALSE)
  fit <- make_mock_fit(draws, n_series, n_lv = 2L,
                        n_time, meta)
  res <- extract_last_state(fit, 1L)
  expect_identical(dim(res$params$ar), c(3L, 2L))
  expect_equal(as.numeric(res$params$ar[1, ]), c(0.7, 0.5))
  expect_equal(as.numeric(res$params$ar[2, ]), c(-0.1, 0.05))
  expect_equal(as.numeric(res$params$ar[3, ]), c(0.2, 0.3))
  # Trend history must cover the maximum lag (12 rows).
  expect_identical(dim(res$last_state$trends), c(12L, 2L))
})


# ----- ARMA (MA innovations history) -----------------------------

test_that("ARMA pulls theta and the last MA-innovation row", {
  n_series <- 2L; n_time <- 5L
  ma_inn <- matrix(seq.int(1, n_time * n_series) / 10,
                    nrow = n_time, byrow = TRUE)
  draws <- make_draws(list(
    sigma_trend = c(0.5, 0.5),
    ar1_trend = c(0.4, 0.6),
    theta1_trend = c(0.2, -0.3),
    ma_innovations_trend = ma_inn,
    trend = matrix(0, nrow = n_time, ncol = n_series)
  ))
  meta <- list(trend_type = "AR", ar_lags = 1L,
               ma_lags = 1L, max_lag = 1L,
               has_cor = FALSE)
  fit <- make_mock_fit(draws, n_series, n_lv = 2L,
                        n_time, meta)
  res <- extract_last_state(fit, 1L)
  expect_true("theta" %in% names(res$params))
  expect_equal(res$params$theta, c(0.2, -0.3))
  expect_identical(dim(res$last_state$errors), c(1L, 2L))
  # Last row of ma_innovations_trend.
  expect_equal(as.numeric(res$last_state$errors[1, ]),
                as.numeric(ma_inn[n_time, ]))
})


# ----- VAR -------------------------------------------------------

test_that("VAR pulls Heaps-transformed A_trend with lag-first indexing", {
  n_series <- 2L; n_time <- 4L
  A1 <- matrix(c(0.5, 0.1, -0.2, 0.6),
                nrow = 2L, byrow = TRUE)
  L <- matrix(c(1, 0, 0.3, sqrt(1 - 0.09)),
               nrow = 2L, byrow = TRUE)
  # Stan declares A_trend as array[N_lags] matrix[N_lv, N_lv];
  # the draws-matrix names are A_trend[lag, i, j].
  A_arr <- array(0, dim = c(1L, n_series, n_series))
  A_arr[1L, , ] <- A1
  draws <- make_draws(list(
    sigma_trend = c(1, 1),
    L_Omega_trend = L,
    A_trend = A_arr,
    trend = matrix(0, nrow = n_time, ncol = n_series)
  ))
  meta <- list(trend_type = "VAR", ar_lags = 1L,
               ma_lags = integer(0), max_lag = 1L,
               has_cor = TRUE)
  fit <- make_mock_fit(draws, n_series, n_lv = 2L,
                        n_time, meta)
  res <- extract_last_state(fit, 1L)
  expect_named(res$params, c("A", "sigma", "Sigma"))
  expect_identical(dim(res$params$A), c(2L, 2L, 1L))
  expect_equal(res$params$A[, , 1], A1)
})


# ----- CAR -------------------------------------------------------

test_that("CAR pulls phi, sigma, trend history, and last times", {
  n_series <- 2L; n_time <- 6L
  obs_data <- data.frame(
    time = c(1, 4, 7, 10, 12, 15,
              2, 5, 8, 11, 13, 16),
    series = factor(rep(c("a", "b"), each = 6L),
                     levels = c("a", "b"))
  )
  draws <- make_draws(list(
    ar1_trend = c(0.6, 0.8),
    sigma_trend = c(0.3, 0.4),
    trend = matrix(seq_len(n_time * n_series),
                   nrow = n_time, byrow = TRUE)
  ))
  meta <- list(trend_type = "CAR", ar_lags = 1L,
               ma_lags = integer(0), max_lag = 1L,
               has_cor = FALSE,
               variables = list(time_var = "time",
                                  series_var = "series"))
  fit <- make_mock_fit(draws, n_series, n_lv = 2L,
                        n_time, meta, obs_data = obs_data)
  res <- extract_last_state(fit, 1L)
  expect_named(res$params, c("phi", "sigma"))
  expect_equal(res$params$phi, c(0.6, 0.8))
  expect_equal(res$params$sigma, c(0.3, 0.4))
  expect_equal(unname(res$last_state$time), c(15, 16))
})


# ----- ZMVN ------------------------------------------------------

test_that("ZMVN returns Sigma + sigma and an empty trend history", {
  n_series <- 2L; n_time <- 5L
  L <- diag(2L)
  draws <- make_draws(list(
    sigma_trend = c(0.5, 0.5),
    L_Omega_trend = L
  ))
  meta <- list(trend_type = "ZMVN",
               ar_lags = integer(0),
               ma_lags = integer(0),
               max_lag = 0L, has_cor = TRUE)
  fit <- make_mock_fit(draws, n_series, n_lv = 2L,
                        n_time, meta)
  res <- extract_last_state(fit, 1L)
  expect_true("Sigma" %in% names(res$params))
  expect_identical(dim(res$last_state$trends), c(0L, 2L))
  # The recursion is advanced zero-mean, so the state carries no
  # linear-predictor field for a caller to fill.
  expect_false("linpreds" %in% names(res$last_state))
})


test_that("factor ZMVN keeps the latent scale it was fitted with", {
  # The identified latent variables are not standard normals:
  # `sigma_trend` and `L_Omega_trend` carry the scale, and
  # `propagate_zmvn()` draws from that covariance. Reading it at
  # the LV grain is what keeps a factor forecast's intervals the
  # width the model implies.
  n_series <- 3L
  n_lv <- 2L
  draws <- make_draws(list(
    sigma_trend = c(0.6, 0.3),
    # Lower triangular with unit-norm rows, so `tcrossprod()`
    # gives a correlation matrix and the scales stay readable.
    L_Omega_trend = matrix(c(1, 0.5, 0, sqrt(0.75)), 2L, 2L)
  ))
  meta <- list(trend_type = "ZMVN",
                 ar_lags = integer(0),
                 ma_lags = integer(0),
                 max_lag = 0L, has_cor = TRUE)
  fit <- make_mock_fit(draws, n_series, n_lv = n_lv, 5L, meta,
                        spec_n_lv = n_lv)
  res <- extract_last_state(fit, 1L)
  expect_identical(res$n_lv_active, n_lv)
  # Square in the latent dimension, not the series dimension.
  expect_identical(dim(res$params$Sigma), c(n_lv, n_lv))
  expect_equal(sqrt(diag(res$params$Sigma)), c(0.6, 0.3))
})


# ----- Dispatch errors -------------------------------------------

test_that("Factor fits (n_lv < n_series) tag the LV grain", {
  # Since the factor-forecast support landed, the dispatcher
  # runs AR/RW/VAR in n_lv-dimensional latent space and reads
  # `lv_trend[t, k]` for the state history. The mock exposes
  # those columns so the same recursion machinery can walk
  # them, and the returned list carries `n_lv_active = n_lv`
  # to signal to `propagate_one_draw` that a Z projection is
  # needed.
  n_series <- 3L
  n_lv <- 2L
  n_time <- 5L
  lv_trend_mat <- matrix(rnorm(n_time * n_lv), n_time, n_lv)
  colnames(lv_trend_mat) <- NULL
  draws <- make_draws(list(
    sigma_trend = c(1, 1),
    ar1_trend = c(0.5, 0.3),
    lv_trend = lv_trend_mat
  ))
  meta <- list(trend_type = "AR", ar_lags = 1L,
               ma_lags = integer(0), max_lag = 1L,
               has_cor = FALSE)
  fit <- make_mock_fit(draws, n_series = n_series, n_lv = n_lv,
                        n_time = n_time, meta, spec_n_lv = n_lv)
  res <- extract_last_state(fit, 1L)
  expect_identical(res$n_lv_active, n_lv)
  expect_identical(dim(res$last_state$trends), c(1L, n_lv))
  expect_length(res$params$sigma, n_lv)
})


test_that("Hierarchical fits (n_lv > n_series) still error", {
  draws <- make_draws(list(sigma_trend = c(1, 1, 1)))
  meta <- list(trend_type = "AR", ar_lags = 1L,
               ma_lags = integer(0), max_lag = 1L,
               has_cor = FALSE)
  fit <- make_mock_fit(draws, n_series = 2L, n_lv = 3L,
                        n_time = 5L, meta)
  expect_error(extract_last_state(fit, 1L),
                "not yet supported")
})


test_that("draw_id beyond available draws errors informatively", {
  draws <- make_draws(list(sigma_trend = c(1, 1),
                            trend = matrix(0, 5L, 2L)))
  meta <- list(trend_type = "RW", ar_lags = 1L,
               ma_lags = integer(0), max_lag = 1L,
               has_cor = FALSE)
  fit <- make_mock_fit(draws, n_series = 2L, n_lv = 2L,
                        n_time = 5L, meta)
  expect_error(extract_last_state(fit, 99L),
                "exceeds the number of posterior draws")
})


test_that("Unsupported trend type errors with the right message", {
  draws <- make_draws(list(sigma_trend = c(1, 1)))
  # `BOGUS` is not in the dispatcher's switch -- the fall-
  # through stop() fires with the supported-types message.
  meta <- list(trend_type = "BOGUS", ar_lags = integer(0),
               ma_lags = integer(0), max_lag = 0L,
               has_cor = FALSE)
  fit <- make_mock_fit(draws, n_series = 2L, n_lv = 2L,
                        n_time = 5L, meta)
  expect_error(extract_last_state(fit, 1L),
                "BOGUS")
})


# ----- PW (piecewise) posterior pulling --------------------------

test_that("PW pulls k / m / delta from posterior and t_change from standata", {
  n_series <- 2L; n_lv <- 2L; n_time <- 20L
  n_change <- 3L
  # Delta matrix [n_change, n_lv] with distinct values so we
  # can verify each cell is pulled correctly.
  delta_post <- matrix(
    c(0.10, 0.20, 0.30, -0.10, -0.20, -0.30),
    nrow = n_change, ncol = n_lv, byrow = FALSE
  )
  draws <- make_draws(list(
    k_trend = c(0.05, 0.15),
    m_trend = c(0.4, 0.6),
    delta_trend = delta_post
  ))
  # Standata carries t_change_trend (fixed at fit time) and
  # cap_trend (logistic only); both are pulled by the helper.
  t_change_data <- c(5, 10, 15)
  cap_data <- matrix(10, nrow = n_time, ncol = n_series)
  meta <- list(trend_type = "PW", ar_lags = integer(0),
               ma_lags = integer(0), max_lag = 0L,
               has_cor = FALSE)
  fit <- make_mock_fit(draws, n_series, n_lv, n_time, meta)
  fit$standata$N_change_trend <- n_change
  fit$standata$t_change_trend <- t_change_data
  fit$standata$cap_trend <- cap_data
  res <- extract_last_state(fit, 1L)
  expect_named(res$params,
                c("k", "m", "delta", "t_change"))
  expect_equal(res$params$k, c(0.05, 0.15))
  expect_equal(res$params$m, c(0.4, 0.6))
  expect_identical(dim(res$params$delta),
                    c(n_change, n_series))
  expect_equal(res$params$delta, delta_post,
                tolerance = 1e-12)
  expect_equal(res$params$t_change, c(5, 10, 15))
  # cap is carried on last_state for the forecast caller.
  expect_identical(dim(res$last_state$cap_train),
                    c(n_time, n_series))
})


test_that("PW handles no-changepoint case gracefully", {
  draws <- make_draws(list(
    k_trend = c(0.05, 0.15),
    m_trend = c(0.4, 0.6)
  ))
  meta <- list(trend_type = "PW", ar_lags = integer(0),
               ma_lags = integer(0), max_lag = 0L,
               has_cor = FALSE)
  fit <- make_mock_fit(draws, n_series = 2L, n_lv = 2L,
                        n_time = 10L, meta)
  fit$standata$N_change_trend <- 0L
  fit$standata$t_change_trend <- numeric(0L)
  res <- extract_last_state(fit, 1L)
  expect_identical(dim(res$params$delta), c(0L, 2L))
  expect_length(res$params$t_change, 0L)
})
