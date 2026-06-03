# CI tests for the trend-propagation dispatcher in
# `R/trend_propagation.R`. Covers every supported trend type
# plus the sparse-lag, MA, and cross-series-correlation paths.


# ---- None ----------------------------------------------------------

test_that("propagate_trend('None') returns a zero matrix", {
  out <- propagate_trend("None", h = 20L, n_series = 3L)
  expect_identical(dim(out), c(20L, 3L))
  expect_true(all(out == 0))
})


# ---- ZMVN ----------------------------------------------------------

test_that("propagate_trend(ZMVN()) returns IID MVN draws", {
  set.seed(11L)
  Sigma <- matrix(c(1, 0.5, 0.5, 1), 2L, 2L)
  out <- propagate_trend(ZMVN(), params = list(Sigma = Sigma),
                          h = 200L, n_series = 2L)
  expect_identical(dim(out), c(200L, 2L))
  # Sample covariance should approximate Sigma; check off-diag.
  expect_lt(abs(stats::cor(out[, 1L], out[, 2L]) - 0.5), 0.2)
})


# ---- RW + AR -------------------------------------------------------

test_that("propagate_trend(RW()) returns correctly-shaped output", {
  out <- propagate_trend(RW(), params = list(sigma = 0.5),
                          h = 60L, n_series = 1L)
  expect_identical(dim(out), c(60L, 1L))
  expect_true(all(is.finite(out)))
})


test_that("propagate_trend(AR(p = 1)) is approximately stationary", {
  set.seed(42L)
  reps <- replicate(500L, propagate_trend(
    AR(p = 1L), params = list(ar = 0.6, sigma = 1),
    h = 1L, n_series = 1L
  ))
  marginal_var <- stats::var(as.numeric(reps))
  # Theoretical marginal var = sigma^2 / (1 - ar1^2) = 1 / 0.64 = 1.5625
  expect_lt(abs(marginal_var - 1.5625), 0.4)
})


test_that("propagate_trend(AR(p = c(1, 3, 12))) accepts sparse lags", {
  out <- propagate_trend(
    AR(p = c(1L, 3L, 12L)),
    params = list(ar = c(0.4, 0.2, 0.1), sigma = 0.5),
    h = 100L, n_series = 1L
  )
  expect_identical(dim(out), c(100L, 1L))
  expect_true(all(is.finite(out)))
})


test_that("propagate_trend(AR(p = 2)) for 4 series with shared coefs", {
  out <- propagate_trend(
    AR(p = 2L),
    params = list(ar = c(0.5, 0.2), sigma = rep(0.4, 4L)),
    h = 50L, n_series = 4L
  )
  expect_identical(dim(out), c(50L, 4L))
  expect_true(all(is.finite(out)))
})


test_that("propagate_trend(AR(p = 2, ma = TRUE)) ARMA path works", {
  out <- propagate_trend(
    AR(p = 2L, ma = TRUE),
    params = list(ar = c(0.5, 0.2), theta = 0.3, sigma = 0.5),
    h = 50L, n_series = 1L
  )
  expect_identical(dim(out), c(50L, 1L))
  expect_true(all(is.finite(out)))
})


# ---- VAR ----------------------------------------------------------

test_that("propagate_trend(VAR(p = 1)) accepts explicit A and Sigma", {
  A <- array(0, dim = c(3L, 3L, 1L))
  diag(A[, , 1L]) <- c(0.6, 0.5, 0.4)
  A[1L, 2L, 1L] <- 0.2
  out <- propagate_trend(
    VAR(p = 1L),
    params = list(A = A, Sigma = diag(3L) * 0.25),
    h = 40L, n_series = 3L
  )
  expect_identical(dim(out), c(40L, 3L))
  expect_true(all(is.finite(out)))
})


test_that("propagate_trend(VAR()) errors when A is missing", {
  expect_error(
    propagate_trend(
      VAR(p = 1L), params = list(Sigma = diag(3L)),
      h = 20L, n_series = 3L
    ),
    "VAR propagation requires 'A'"
  )
})


# ---- CAR ----------------------------------------------------------

test_that("propagate_trend(CAR()) accepts irregular intervals", {
  set.seed(7L)
  out <- propagate_trend(
    CAR(), params = list(phi = 0.7, sigma = 0.5),
    h = 40L, n_series = 2L,
    time = stats::runif(40L, 1, 6)
  )
  expect_identical(dim(out), c(40L, 2L))
  expect_true(all(is.finite(out)))
})


test_that("propagate_trend(CAR()) errors when time is missing", {
  expect_error(
    propagate_trend(
      CAR(), params = list(phi = 0.7, sigma = 0.5),
      h = 10L, n_series = 1L
    ),
    "'time' is required"
  )
})


test_that("CAR(1) phi <= 0 / >= 1 guard fires through dispatcher", {
  expect_error(
    propagate_trend(
      CAR(), params = list(phi = 1.0, sigma = 0.5),
      h = 5L, n_series = 1L, time = rep(1, 5L)
    ),
    "phi"
  )
  expect_error(
    propagate_trend(
      CAR(), params = list(phi = -0.5, sigma = 0.5),
      h = 5L, n_series = 1L, time = rep(1, 5L)
    ),
    "phi"
  )
})


# ---- Reproducibility ----------------------------------------------

test_that("propagate_trend is reproducible under set.seed", {
  set.seed(99L)
  a <- propagate_trend(AR(p = 1L), list(sigma = 0.5),
                        h = 30L, n_series = 1L)
  set.seed(99L)
  b <- propagate_trend(AR(p = 1L), list(sigma = 0.5),
                        h = 30L, n_series = 1L)
  expect_identical(a, b)
})


# ---- last_state contract ------------------------------------------

test_that("propagate_trend accepts a caller-supplied last_state", {
  ls <- list(
    trends = matrix(0.5, nrow = 3L, ncol = 2L),
    errors = matrix(0, nrow = 1L, ncol = 2L),
    linpreds = matrix(0, nrow = 3L, ncol = 2L),
    time = NULL
  )
  out <- propagate_trend(
    AR(p = 3L),
    params = list(ar = c(0.5, 0.2, 0.1), sigma = rep(0.4, 2L)),
    h = 10L, n_series = 2L, last_state = ls
  )
  expect_identical(dim(out), c(10L, 2L))
})


# ---- Unsupported trend type ---------------------------------------

test_that("propagate_trend errors on unsupported trend types", {
  fake_trend <- structure(
    list(trend = "PWlinear"), class = "mvgam_trend"
  )
  expect_error(
    propagate_trend(fake_trend, list(), h = 10L, n_series = 1L),
    "not supported"
  )
})
