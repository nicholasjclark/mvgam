# The tests of test-sample-innovations.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()

test_that("a draw a step from a unit root settles at its closed form", {
  # `ar1 = 1 - 1e-7` needs 10^8 weights to settle, and a truncated sum
  # would call the draw explosive and keep its innovation variance, a
  # factor of 5 x 10^6 too small. The draw takes the companion solve,
  # which the closed forms below check. The ARMA(1, 1) variance is
  # `(1 + 2 ar1 theta + theta^2) / (1 - ar1^2)`.
  ar1 <- c(1 - 1e-7, 0.99995)
  theta <- c(0, 0.4)
  f <- ar_stationary_factor(list(
    lags = 1L, phi = list(matrix(ar1, 1L)), theta = matrix(theta, 1L)
  ))
  want <- (1 + 2 * ar1 * theta + theta^2) / (1 - ar1^2)
  expect_equal(diag(matrix(f[1L, , ], 2L, 2L)), want, tolerance = 1e-6)
  # A unit root has no stationary distribution.
  expect_true(is.na(stationary_factor(1L, list(1))[1L, 1L]))
})
