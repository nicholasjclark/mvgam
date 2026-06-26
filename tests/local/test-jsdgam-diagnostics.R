# Fixture-dependent diagnostic tests on cached jsdgam fits.
# Requires `pkgdown/jsdgam_cache/mod_baseline.rds`, built locally
# via `Rscript tests/local/jsdgam_vignette_fits.R`. Out of the CI
# sweep on purpose: convergence is slow under the trait kernel.

suppressPackageStartupMessages({
  devtools::load_all(quiet = TRUE)
  library(testthat)
})

mod_baseline <- readRDS("pkgdown/jsdgam_cache/mod_baseline.rds")

test_that("default diagnostic surfaces hide raw Z[ when Z_tilde[ exists", {
  v <- variables(mod_baseline)
  expect_false(any(grepl("^Z\\[", v)))
  expect_true(any(grepl("^Z_tilde\\[", v)))
  expect_false(any(grepl("^sigma_trend\\[", v)))
  expect_false(any(grepl("^L_Omega_trend\\[", v)))
  ps <- posterior_summary(mod_baseline)
  expect_false(any(grepl("^Z\\[", rownames(ps))))
  rh <- rhat(mod_baseline)
  expect_false(any(grepl("^Z\\[", names(rh))))
})

test_that("as_draws_array honours an explicit `variable = 'Z'` request", {
  drws <- as_draws_array(mod_baseline, variable = "Z", regex = TRUE)
  cols <- posterior::variables(drws)
  # Both raw and identified loadings surface when explicitly asked.
  expect_true(any(grepl("^Z\\[", cols)))
  expect_true(any(grepl("^Z_tilde\\[", cols)))
})

test_that("active_factors + plot.mvgam_active_factors integrate on cached fit", {
  mod_mgp <- readRDS("pkgdown/jsdgam_cache/mod_mgp.rds")
  af <- active_factors(mod_mgp)
  expect_s3_class(af, "mvgam_active_factors")
  p <- plot(af)
  expect_s3_class(p, "ggplot")
})

test_that("compare_loadings integrates on two cached fits", {
  mod_trt <- readRDS("pkgdown/jsdgam_cache/mod_traits.rds")
  p <- compare_loadings(
    mod_baseline, mod_trt,
    labels = c("uninformed", "trait-informed")
  )
  expect_s3_class(p, "ggplot")
  # Facet mode round-trips too.
  p_f <- compare_loadings(
    mod_baseline, mod_trt,
    labels = c("uninformed", "trait-informed"), facet = TRUE
  )
  expect_s3_class(p_f, "ggplot")
})
