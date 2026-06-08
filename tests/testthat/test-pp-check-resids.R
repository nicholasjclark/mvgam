# Unit tests for the four diagnostic resid pp_check types and
# the 4-panel composer. Exercises the internal builders directly
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

test_that("build_resid_lag_panel returns a ggplot for acf and pacf", {
  mat <- .make_resid_draws()
  p_acf <- mvgam:::build_resid_lag_panel(mat, lag_type = "acf")
  p_pacf <- mvgam:::build_resid_lag_panel(mat, lag_type = "pacf")
  expect_ggplot(p_acf)
  expect_ggplot(p_pacf)
  expect_equal(p_acf$labels$title, "ACF")
  expect_equal(p_pacf$labels$title, "pACF")
})

test_that("build_resid_lag_panel rejects unknown lag_type", {
  mat <- .make_resid_draws()
  expect_error(
    mvgam:::build_resid_lag_panel(mat, lag_type = "ccf")
  )
})

test_that("acf lag-band ordering nests outer >= inner", {
  mat <- .make_resid_draws()
  p <- mvgam:::build_resid_lag_panel(mat, lag_type = "acf")
  # Three nested geom_segment layers; their data carries q025/q975
  # (outer), q100/q900 (mid), and q250/q750 (inner). Pull the data
  # from the inner-most ribbon layer.
  seg_layers <- Filter(
    function(l) inherits(l$geom, "GeomSegment"),
    p$layers
  )
  expect_length(seg_layers, 3L)
  df <- seg_layers[[1L]]$data
  expect_true(all(df$q025 <= df$q100))
  expect_true(all(df$q100 <= df$q250))
  expect_true(all(df$q750 <= df$q900))
  expect_true(all(df$q900 <= df$q975))
})

test_that("build_resid_qq_panel returns a ggplot with stat_qq layers", {
  mat <- .make_resid_draws()
  p <- mvgam:::build_resid_qq_panel(mat)
  expect_ggplot(p)
  expect_equal(p$labels$title, "Normal Q-Q Plot")
  has_qq <- any(vapply(
    p$layers,
    function(l) inherits(l$stat, "StatQq") ||
      inherits(l$stat, "StatQqLine"),
    logical(1L)
  ))
  expect_true(has_qq)
})

test_that("resid_vs_fitted per_obs = TRUE plots one point per obs", {
  resid_mat <- .make_resid_draws(ndraws = 100L, nobs = 50L)
  fitted_mat <- .make_resid_draws(
    ndraws = 100L, nobs = 50L, sd = 5.0, seed = 2L
  )
  p_per <- mvgam:::build_resid_vs_fitted_panel(
    resid_mat, fitted_mat, per_obs = TRUE
  )
  expect_ggplot(p_per)
  expect_equal(nrow(p_per$data), 50L)
})

test_that("resid_vs_fitted per_obs = FALSE pools draw x obs", {
  resid_mat <- .make_resid_draws(ndraws = 100L, nobs = 50L)
  fitted_mat <- .make_resid_draws(
    ndraws = 100L, nobs = 50L, sd = 5.0, seed = 2L
  )
  p_pool <- mvgam:::build_resid_vs_fitted_panel(
    resid_mat, fitted_mat, per_obs = FALSE
  )
  expect_equal(nrow(p_pool$data), 100L * 50L)
})

test_that("build_resid_vs_fitted_panel includes a gam smoother layer", {
  resid_mat <- .make_resid_draws(ndraws = 50L, nobs = 40L)
  fitted_mat <- .make_resid_draws(
    ndraws = 50L, nobs = 40L, sd = 3.0, seed = 5L
  )
  p <- mvgam:::build_resid_vs_fitted_panel(
    resid_mat, fitted_mat, per_obs = TRUE
  )
  smooth_layer <- Filter(
    function(l) inherits(l$stat, "StatSmooth"), p$layers
  )
  expect_length(smooth_layer, 1L)
})

# End-to-end integration tests for `pp_check.mvgam` + the
# composer require a real Stan fit because pp_check resolves
# posterior::ndraws / formula::lhs / family at runtime against
# the brmsfit. Those tests live in tests/local/, where the
# val_*.rds fixtures are available; CI exercises the internal
# builders above.
