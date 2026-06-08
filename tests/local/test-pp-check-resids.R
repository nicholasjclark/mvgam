# End-to-end tests for the four diagnostic `pp_check` resid
# types and the `mvgam_resid_panel` composer, against a cached
# val_*.rds fit so the full pp_check + posterior_epred +
# residuals pipeline is exercised.

CACHE_DIR <- "fixtures"
fit <- readRDS(file.path(CACHE_DIR, "val_mvgam_ar1_fx.rds"))

test_that("resid_acf returns a ggplot with the expected title", {
  p <- pp_check(fit, type = "resid_acf", ndraws = 50L)
  expect_ggplot(p)
  expect_equal(p$labels$title, "ACF")
})

test_that("resid_pacf returns a ggplot distinct from resid_acf", {
  p_acf <- pp_check(fit, type = "resid_acf", ndraws = 50L)
  p_pacf <- pp_check(fit, type = "resid_pacf", ndraws = 50L)
  expect_ggplot(p_pacf)
  expect_equal(p_pacf$labels$title, "pACF")
  # Plot-level data carries the per-lag quantile bands; ACF and
  # pACF compute different functions and so produce different
  # values at each lag.
  expect_false(identical(p_acf$data, p_pacf$data))
})

test_that("resid_qq returns a Q-Q ggplot", {
  p <- pp_check(fit, type = "resid_qq", ndraws = 50L)
  expect_ggplot(p)
  expect_equal(p$labels$title, "Normal Q-Q Plot")
})

n_obs <- nrow(fit$data %||% fit$obs_data)

test_that("resid_vs_fitted per_obs default is one point per obs", {
  p <- pp_check(fit, type = "resid_vs_fitted", ndraws = 50L)
  expect_ggplot(p)
  expect_equal(nrow(p$data), n_obs)
})

test_that("resid_vs_fitted per_obs = FALSE pools draw x obs", {
  p <- pp_check(
    fit, type = "resid_vs_fitted",
    ndraws = 50L, per_obs = FALSE
  )
  expect_equal(nrow(p$data), 50L * n_obs)
})

test_that("mvgam_resid_panel returns a 4-panel patchwork", {
  p <- mvgam:::mvgam_resid_panel(fit, ndraws = 30L)
  expect_true(inherits(p, "patchwork"))
})
