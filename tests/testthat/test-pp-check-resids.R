# Unit tests for the diagnostic resid pp_check builders, run
# against hand-built residual / fitted draws matrices so no Stan
# fit is required.

# Reproducible (ndraws x nobs) draws matrix; default mimics
# unit-variance DS residuals at a moderate sample size.
.make_resid_draws <- function(
  ndraws = 200L, nobs = 60L, sd = 1.0, seed = 1L
) {
  set.seed(seed)
  matrix(stats::rnorm(ndraws * nobs, sd = sd), nrow = ndraws, ncol = nobs)
}

test_that("the pACF of a series with gaps stays a partial correlation", {
  # `pacf()` fed a gappy ACF through its recursion and drew bands
  # beyond [-1, 1]. The pACF drops the missing values.
  mat <- .make_resid_draws(ndraws = 50L)
  mat[, c(5:12, 30:36)] <- NA_real_
  bands <- mvgam:::build_resid_lag_panel(mat, lag_type = "pacf")$data
  expect_true(all(abs(c(bands$q025, bands$q975)) <= 1))
})

test_that("resid_vs_fitted plots one point per obs or pools the draws", {
  resid_mat <- .make_resid_draws(ndraws = 100L, nobs = 50L)
  fitted_mat <- .make_resid_draws(
    ndraws = 100L, nobs = 50L, sd = 5.0, seed = 2L
  )
  per_obs <- mvgam:::build_resid_vs_fitted_panel(
    resid_mat, fitted_mat, per_obs = TRUE
  )
  pooled <- mvgam:::build_resid_vs_fitted_panel(
    resid_mat, fitted_mat, per_obs = FALSE
  )
  expect_equal(nrow(per_obs$data), 50L)
  expect_equal(nrow(pooled$data), 100L * 50L)
})

# End-to-end integration tests for `pp_check.mvgam` + the
# composer require a real Stan fit because pp_check resolves
# posterior::ndraws / formula::lhs / family at runtime against
# the brmsfit. Those tests live in tests/local/, where the
# val_*.rds fixtures are available; CI exercises the internal
# builders above.


test_that("resid_series_rows takes one series in time order", {
  # Residual columns follow the rows of the training frame, which may
  # interleave or shuffle the series. The ACF counted lags across
  # species on a frame that listed every species at each time.
  d <- data.frame(
    time = c(3, 1, 2, 2, 1, 3),
    series = factor(c("b", "a", "b", "a", "b", "a"), levels = c("b", "a")),
    y = 1:6
  )
  fit <- structure(
    list(data = d, formula = y ~ 1, family = stats::poisson(),
         trend_metadata = mock_axis_record(d)),
    class = "mvgam"
  )
  expect_identical(mvgam:::resid_series_rows(fit, d)$rows, c(5L, 3L, 1L))
  expect_error(mvgam:::resid_series_rows(fit, d, "all"), "one series")
})
