# Unit tests for `extract_last_states()` and `last_state_of_draw()` in
# `R/extract_last_state.R`. Each test builds a minimal mock
# mvgam fit (deterministic posterior draws + standata +
# enriched trend_metadata) and asserts the shape and values
# of the (`params`, `last_state`) tuple one draw receives.
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
                            data = NULL,
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
    data = data
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
  res <- last_state_of_draw(extract_last_states(fit), 1L)
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
  res <- last_state_of_draw(extract_last_states(fit), 1L)
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
  res <- last_state_of_draw(extract_last_states(fit), 1L)
  expect_named(res$params, c("sigma", "Sigma", "ar"))
  # ar is a [n_lags, n_series] matrix.
  expect_identical(dim(res$params$ar), c(1L, 2L))
  expect_equal(as.numeric(res$params$ar[1, ]), c(0.4, 0.6))
})


test_that("AR(p = 3) pulls ar1/ar2/ar3 per series", {
  n_series <- 2L; n_time <- 6L
  trend_mat <- matrix(seq_len(n_time * n_series),
                       nrow = n_time, byrow = TRUE)
  draws <- make_draws(list(
    sigma_trend = c(0.5, 0.5),
    ar1_trend = c(0.1, 0.2),
    ar2_trend = c(0.3, 0.4),
    ar3_trend = c(0.5, 0.6),
    trend = trend_mat
  ))
  meta <- list(trend_type = "AR", ar_lags = c(1L, 2L, 3L),
               ma_lags = integer(0), max_lag = 3L,
               has_cor = FALSE)
  fit <- make_mock_fit(draws, n_series, n_lv = 2L,
                        n_time, meta)
  res <- last_state_of_draw(extract_last_states(fit), 1L)
  expect_identical(dim(res$params$ar), c(3L, 2L))
  expect_equal(as.numeric(res$params$ar[1, ]), c(0.1, 0.2))
  expect_equal(as.numeric(res$params$ar[2, ]), c(0.3, 0.4))
  expect_equal(as.numeric(res$params$ar[3, ]), c(0.5, 0.6))
  # The history is the final max_lag rows, in their own order.
  expect_equal(unname(res$last_state$trends),
               unname(trend_mat[4:6, ]))
})


# ----- AR sparse-lag ---------------------------------------------

test_that("AR(p = c(1, 3, 12)) pulls only the active lag params", {
  n_series <- 2L; n_time <- 15L
  trend_mat <- matrix(seq_len(n_time * n_series),
                       nrow = n_time, byrow = TRUE)
  draws <- make_draws(list(
    sigma_trend = c(0.5, 0.5),
    ar1_trend = c(0.7, 0.5),
    ar3_trend = c(-0.1, 0.05),
    ar12_trend = c(0.2, 0.3),
    trend = trend_mat
  ))
  meta <- list(trend_type = "AR",
               ar_lags = c(1L, 3L, 12L),
               ma_lags = integer(0), max_lag = 12L,
               has_cor = FALSE)
  fit <- make_mock_fit(draws, n_series, n_lv = 2L,
                        n_time, meta)
  res <- last_state_of_draw(extract_last_states(fit), 1L)
  expect_identical(dim(res$params$ar), c(3L, 2L))
  expect_equal(as.numeric(res$params$ar[1, ]), c(0.7, 0.5))
  expect_equal(as.numeric(res$params$ar[2, ]), c(-0.1, 0.05))
  expect_equal(as.numeric(res$params$ar[3, ]), c(0.2, 0.3))
  # The history covers the maximum lag: the final 12 rows, in the
  # order they were recorded.
  expect_equal(unname(res$last_state$trends),
               unname(trend_mat[4:15, ]))
})


# ----- ARMA (MA innovations history) -----------------------------

test_that("ARMA pulls theta and the last raw innovation row", {
  n_series <- 2L; n_time <- 5L
  raw_inn <- matrix(seq.int(1, n_time * n_series) / 10,
                    nrow = n_time, byrow = TRUE)
  # The formed moving average is given values of its own, which lets
  # the assertions below name which of the two the seed came from.
  formed <- raw_inn * 3
  draws <- make_draws(list(
    sigma_trend = c(0.5, 0.5),
    ar1_trend = c(0.4, 0.6),
    theta1_trend = c(0.2, -0.3),
    scaled_innovations_trend = raw_inn,
    ma_innovations_trend = formed,
    trend = matrix(0, nrow = n_time, ncol = n_series)
  ))
  meta <- list(trend_type = "AR", ar_lags = 1L,
               ma_lags = 1L, max_lag = 1L,
               has_cor = FALSE)
  fit <- make_mock_fit(draws, n_series, n_lv = 2L,
                        n_time, meta)
  res <- last_state_of_draw(extract_last_states(fit), 1L)
  expect_equal(res$params$theta, c(0.2, -0.3))
  # The kernel multiplies its coefficient by a past innovation, which
  # makes the raw row the seed it needs. The formed moving average
  # carries the coefficient already, three times the raw row here.
  expect_identical(dim(res$last_state$errors), c(1L, 2L))
  expect_equal(as.numeric(res$last_state$errors[1, ]),
                as.numeric(raw_inn[n_time, ]))
})


# ----- VAR -------------------------------------------------------

test_that("VAR pulls Heaps-transformed Phi_trend with lag-first indexing", {
  n_series <- 2L; n_time <- 4L
  A1 <- matrix(c(0.5, 0.1, -0.2, 0.6),
                nrow = 2L, byrow = TRUE)
  L <- matrix(c(1, 0, 0.3, sqrt(1 - 0.09)),
               nrow = 2L, byrow = TRUE)
  # Stan declares Phi_trend as array[N_lags] matrix[N_lv, N_lv];
  # the draws-matrix names are Phi_trend[lag, i, j].
  A_arr <- array(0, dim = c(1L, n_series, n_series))
  A_arr[1L, , ] <- A1
  draws <- make_draws(list(
    sigma_trend = c(1, 1),
    L_Omega_trend = L,
    Phi_trend = A_arr,
    trend = matrix(0, nrow = n_time, ncol = n_series)
  ))
  meta <- list(trend_type = "VAR", ar_lags = 1L,
               ma_lags = integer(0), max_lag = 1L,
               has_cor = TRUE)
  fit <- make_mock_fit(draws, n_series, n_lv = 2L,
                        n_time, meta)
  res <- last_state_of_draw(extract_last_states(fit), 1L)
  expect_equal(res$params$A[, , 1], A1)
  # Unit scales leave the innovation covariance at L L'
  expect_equal(unname(res$params$Sigma), tcrossprod(L))
})


test_that("VARMA seeds the forecast with the last innovation it fitted", {
  # The program applies `Theta_trend`, the transform of `D_trend`, and
  # never stores its innovations. It centres `lv_t` on
  # `A_1 lv_{t-1} + A_2 lv_{t-2} + D e_{t-1}`. The path below runs on
  # one time axis from t = -1, and `init_trend` is then packed in the
  # companion order `Omega_trend` gives it, most recent first:
  # `(lv_0, lv_{-1}, e_0)`. The extractor has to recover the last
  # innovation. `D_trend` carries values the program never applies.
  n <- 2L
  n_time <- 6L
  A <- list(matrix(c(0.5, 0.1, -0.2, 0.3), n, n),
            matrix(c(0.2, 0, 0.1, -0.1), n, n))
  D <- matrix(c(0.4, -0.2, 0.1, 0.3), n, n)
  set.seed(31L)
  # Row r of `path` and `E` is occasion r - 2. Rows 1 and 2 are the two
  # occasions before the first.
  path <- matrix(0, n_time + 2L, n)
  E <- matrix(stats::rnorm((n_time + 2L) * n), n_time + 2L, n)
  path[1:2, ] <- matrix(stats::rnorm(2L * n), 2L, n)
  for (r in 3:(n_time + 2L)) {
    path[r, ] <- A[[1L]] %*% path[r - 1L, ] + A[[2L]] %*% path[r - 2L, ] +
      D %*% E[r - 1L, ] + E[r, ]
  }
  init <- c(path[2L, ], path[1L, ], E[2L, ])
  lv <- path[3:(n_time + 2L), ]
  A_arr <- array(0, dim = c(2L, n, n))
  A_arr[1L, , ] <- A[[1L]]
  A_arr[2L, , ] <- A[[2L]]
  draws <- make_draws(list(
    sigma_trend = c(1, 1),
    L_Omega_trend = diag(n),
    Phi_trend = A_arr,
    Theta_trend = array(D, dim = c(1L, n, n)),
    D_trend = array(-D, dim = c(1L, n, n)),
    init_trend = init,
    lv_trend = lv,
    trend = lv
  ))
  meta <- list(trend_type = "VAR", ar_lags = 1:2, ma_lags = 1L,
               max_lag = 2L, has_cor = TRUE)
  fit <- make_mock_fit(draws, n, n_lv = n, n_time, meta)
  res <- last_state_of_draw(extract_last_states(fit), 1L)
  expect_equal(res$params$theta_cube[, , 1L], D)
  expect_equal(as.numeric(res$last_state$errors), E[n_time + 2L, ],
               tolerance = 1e-12)
})


# ----- CAR -------------------------------------------------------

test_that("CAR pulls phi, sigma and the trend history", {
  n_series <- 2L; n_time <- 6L
  draws <- make_draws(list(
    ar1_trend = c(0.6, 0.8),
    sigma_trend = c(0.3, 0.4),
    trend = matrix(seq_len(n_time * n_series),
                   nrow = n_time, byrow = TRUE)
  ))
  meta <- list(trend_type = "CAR", ar_lags = 1L, ma_lags = integer(0),
               max_lag = 1L, has_cor = FALSE)
  fit <- make_mock_fit(draws, n_series, n_lv = 2L, n_time, meta)
  res <- last_state_of_draw(extract_last_states(fit), 1L)
  expect_named(res$params, c("phi", "sigma"))
  expect_equal(res$params$phi, c(0.6, 0.8))
  expect_equal(res$params$sigma, c(0.3, 0.4))
  expect_equal(as.numeric(res$last_state$trends), c(11, 12))

  # Correlated innovations add their unit-time covariance, and
  # estimated degrees of freedom reach the forecast with the draw.
  L <- matrix(c(1, 0.5, 0, sqrt(0.75)), 2L, 2L)
  draws_cor <- make_draws(list(
    ar1_trend = c(0.6, 0.8),
    sigma_trend = c(0.3, 0.4),
    L_Omega_trend = L,
    nu_trend = 5,
    trend = matrix(0, nrow = n_time, ncol = n_series)
  ))
  meta$has_cor <- TRUE
  res_cor <- last_state_of_draw(extract_last_states(
    make_mock_fit(draws_cor, n_series, n_lv = 2L, n_time, meta)
  ), 1L)
  expect_equal(unname(res_cor$params$Sigma),
               tcrossprod(L * c(0.3, 0.4)), tolerance = 1e-12)
  expect_identical(res_cor$params$df, 5)

  # A forecast steps with the stored covariance. On a CAR fit it
  # differs from the scales and correlation factor multiplied out.
  stored <- matrix(c(0.09, 0.04, 0.04, 0.16), 2L, 2L)
  draws_stored <- make_draws(list(
    ar1_trend = c(0.6, 0.8),
    sigma_trend = c(0.3, 0.4),
    L_Omega_trend = L,
    Sigma_trend = stored,
    trend = matrix(0, nrow = n_time, ncol = n_series)
  ))
  res_stored <- last_state_of_draw(extract_last_states(
    make_mock_fit(draws_stored, n_series, n_lv = 2L, n_time, meta)
  ), 1L)
  expect_equal(unname(res_stored$params$Sigma), stored)
})


# ----- LLT -------------------------------------------------------

test_that("LLT hands on the last level and slope and the slope covariance", {
  n_series <- 2L; n_time <- 4L
  slope <- matrix(c(0.1, 0.2, 0.3, 0.4, -0.1, -0.2, -0.3, -0.4), n_time)
  L <- t(chol(matrix(c(1, 0.5, 0.5, 1), 2L)))
  draws <- make_draws(list(
    sigma_trend = c(0.5, 1),
    L_Omega_trend = L,
    sigma_slope_trend = c(0.1, 0.2),
    ar1_slope_trend = c(0.9, 0.7),
    slope_trend = slope,
    trend = matrix(1:8, n_time)
  ))
  meta <- list(trend_type = "LLT", ar_lags = 1L, ma_lags = integer(0),
               max_lag = 1L, has_cor = TRUE)
  fit <- make_mock_fit(draws, n_series, n_lv = 2L, n_time, meta)
  res <- last_state_of_draw(extract_last_states(fit), 1L)
  expect_equal(as.numeric(res$last_state$trends), c(4, 8))
  expect_equal(res$last_state$slope, c(0.4, -0.4))
  expect_equal(res$params$phi_slope, c(0.9, 0.7))
  # The slope shocks share the level's correlation, 0.5
  expect_equal(res$params$Sigma_slope,
               matrix(c(0.01, 0.01, 0.01, 0.04), 2L))
})


# ----- ZMVN ------------------------------------------------------

test_that("ZMVN returns the innovation covariance and an empty history", {
  n_series <- 2L; n_time <- 5L
  L <- t(chol(matrix(c(1, 0.3, 0.3, 1), 2L)))
  draws <- make_draws(list(
    sigma_trend = c(0.5, 2),
    L_Omega_trend = L
  ))
  meta <- list(trend_type = "ZMVN",
               ar_lags = integer(0),
               ma_lags = integer(0),
               max_lag = 0L, has_cor = TRUE)
  fit <- make_mock_fit(draws, n_series, n_lv = 2L,
                        n_time, meta)
  res <- last_state_of_draw(extract_last_states(fit), 1L)
  expect_equal(res$params$Sigma, matrix(c(0.25, 0.3, 0.3, 4), 2L))
  expect_identical(nrow(res$last_state$trends), 0L)
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
  res <- last_state_of_draw(extract_last_states(fit), 1L)
  expect_identical(res$n_lv_active, n_lv)
  # Square in the latent dimension, not the series dimension.
  expect_identical(dim(res$params$Sigma), c(n_lv, n_lv))
  expect_equal(sqrt(diag(res$params$Sigma)), c(0.6, 0.3))
})


# ----- Dispatch errors -------------------------------------------

test_that("Factor fits (n_lv < n_series) tag the LV grain", {
  # For a factor fit, the dispatcher runs AR/RW/VAR in
  # n_lv-dimensional latent space and reads `lv_trend[t, k]` for
  # the state history. The mock exposes
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
  res <- last_state_of_draw(extract_last_states(fit), 1L)
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
  expect_error(extract_last_states(fit),
                "more latent states than series")
})


test_that("each draw receives its own parameters and state", {
  # Two draws extracted together. A slice indexing the wrong draw would
  # hand draw 2 a covariance or a history from draw 1.
  n_series <- 2L; n_time <- 3L
  values <- list(
    list(sigma = c(1, 2), cor = 0, ar = c(0.4, 0.6), last = c(1, 2)),
    list(sigma = c(3, 0.5), cor = 0.6, ar = c(-0.2, 0.1), last = c(7, 8))
  )
  rows <- lapply(values, function(v) {
    L <- matrix(c(1, v$cor, 0, sqrt(1 - v$cor^2)), 2L)
    make_draws(list(
      sigma_trend = v$sigma, ar1_trend = v$ar, L_Omega_trend = L,
      trend = rbind(matrix(0, n_time - 1L, n_series), v$last)
    ))
  })
  draws <- posterior::as_draws_matrix(do.call(rbind, lapply(rows, unclass)))
  meta <- list(trend_type = "AR", ar_lags = 1L,
               ma_lags = integer(0), max_lag = 1L,
               has_cor = TRUE)
  states <- extract_last_states(
    make_mock_fit(draws, n_series, n_lv = 2L, n_time, meta)
  )
  for (d in 1:2) {
    v <- values[[d]]
    res <- last_state_of_draw(states, d)
    L <- matrix(c(1, v$cor, 0, sqrt(1 - v$cor^2)), 2L)
    expect_equal(res$params$Sigma, diag(v$sigma) %*% tcrossprod(L) %*%
                   diag(v$sigma))
    expect_equal(as.numeric(res$params$ar), v$ar)
    expect_equal(as.numeric(res$last_state$trends), v$last)
  }
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
  expect_error(extract_last_states(fit), "BOGUS")
})


# ----- PW (piecewise) posterior pulling --------------------------

test_that("a logistic PW pulls k / m / delta and t_change from standata", {
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
  # Standata carries t_change_trend, fixed at fit time
  t_change_data <- c(5, 10, 15)
  # The logistic form samples `m_trend`, and `trend_metadata`
  # persists the growth for post-fit callers.
  meta <- list(trend_type = "PW", ar_lags = integer(0),
               ma_lags = integer(0), max_lag = 0L,
               has_cor = FALSE, pw_growth = "logistic")
  fit <- make_mock_fit(draws, n_series, n_lv, n_time, meta)
  fit$standata$N_change_trend <- n_change
  fit$standata$t_change_trend <- t_change_data
  res <- last_state_of_draw(extract_last_states(fit), 1L)
  expect_named(res$params,
                c("k", "m", "delta", "t_change"))
  expect_equal(res$params$k, c(0.05, 0.15))
  expect_equal(res$params$m, c(0.4, 0.6))
  expect_identical(dim(res$params$delta),
                    c(n_change, n_series))
  expect_equal(res$params$delta, delta_post,
                tolerance = 1e-12)
  expect_equal(res$params$t_change, c(5, 10, 15))
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
  res <- last_state_of_draw(extract_last_states(fit), 1L)
  expect_identical(dim(res$params$delta), c(0L, 2L))
  expect_length(res$params$t_change, 0L)
  # This stub omits the growth, which resolves to linear. A linear
  # PW takes its level from the observation formula, and the offset
  # passed to the kernel is zero for every series.
  expect_equal(res$params$m, c(0, 0))
})
