# log_lik.mvgam integrity + end-to-end downstream consumer tests.
#
# Per-obs numerical concordance against brms is not meaningful for
# AR-bearing fixtures: brms's ar(cov = TRUE) returns the joint MVN
# log-density on the time series scale, distributed across
# observations differently than the conditional Poisson / Beta /
# Binomial log densities mvgam computes obs-by-obs. The two are
# different objects, not different estimates of the same object.
#
# What this file checks instead:
#   - log_lik.mvgam returns a finite [ndraws x nobs] matrix per
#     fixture family covered by the rebuilt method.
#   - Multivariate fits return the per-obs SUM across responses
#     (matching brms::log_lik for mvbind fits).
#   - loo / waic / pp_check(loo_pit_overlay) all run end-to-end and
#     return finite estimates on AR fixtures.
#
# Lives in tests/local because it needs the cached fixture pairs.

source("setup_tests_local.R")
source("concordance_helpers.R")


# Per-fixture integrity check: log_lik returns a finite [ndraws x
# nobs] matrix with no NA / NaN cells.
assert_log_lik_integrity <- function(mvgam_fit, newdata,
                                     process_error = TRUE,
                                     resp = NULL) {
  ll <- log_lik(mvgam_fit, newdata = newdata,
                process_error = process_error, resp = resp)
  testthat::expect_true(is.matrix(ll))
  testthat::expect_equal(ncol(ll), nrow(newdata))
  testthat::expect_true(all(is.finite(ll)))
  invisible(ll)
}


# -- Gaussian multivariate ---------------------------------------------

test_that("Gaussian multivariate: joint log_lik sums across responses", {
  require_fixtures("val_brms_mv_gauss.rds", "val_mvgam_mv_gauss.rds")
  brms_fit <- load_brms("mv_gauss")
  mvgam_fit <- load_mvgam("mv_gauss")
  newdata <- mvgam_fit$data
  joint <- log_lik(mvgam_fit, newdata = newdata)
  per_y1 <- log_lik(mvgam_fit, newdata = newdata, resp = "y1")
  per_y2 <- log_lik(mvgam_fit, newdata = newdata, resp = "y2")
  # brms returns per-obs sum across responses; mvgam mirrors this.
  testthat::expect_equal(dim(joint), dim(per_y1))
  testthat::expect_equal(joint, per_y1 + per_y2, tolerance = 1e-10)
  brms_ll <- brms::log_lik(brms_fit, newdata = newdata)
  testthat::expect_equal(dim(brms_ll), dim(joint))
})


# -- Family coverage ---------------------------------------------------

test_that("Beta AR(1): log_lik returns finite matrix of correct shape", {
  require_fixtures("val_brms_beta_ar1.rds", "val_mvgam_beta_ar1.rds")
  mvgam_fit <- load_mvgam("beta_ar1")
  assert_log_lik_integrity(mvgam_fit, mvgam_fit$data)
})

test_that("Binomial AR(1): log_lik returns finite matrix of correct shape", {
  require_fixtures("val_brms_binom_ar1.rds", "val_mvgam_binom_ar1.rds")
  mvgam_fit <- load_mvgam("binom_ar1")
  assert_log_lik_integrity(mvgam_fit, mvgam_fit$data)
})

test_that("Poisson AR(1) + fixed: log_lik returns finite matrix", {
  require_fixtures("val_brms_ar1_fx.rds", "val_mvgam_ar1_fx.rds")
  mvgam_fit <- load_mvgam("ar1_fx")
  assert_log_lik_integrity(mvgam_fit, mvgam_fit$data)
})

test_that("Hurdle Poisson AR(1): log_lik returns finite matrix", {
  require_fixtures("val_brms_hurdle_poisson_ar1.rds",
                   "val_mvgam_hurdle_poisson_ar1.rds")
  mvgam_fit <- load_mvgam("hurdle_poisson_ar1")
  assert_log_lik_integrity(mvgam_fit, mvgam_fit$data)
})

test_that("Hurdle NegBinomial AR(1): log_lik returns finite matrix", {
  require_fixtures("val_brms_hurdle_negbinomial_ar1.rds",
                   "val_mvgam_hurdle_negbinomial_ar1.rds")
  mvgam_fit <- load_mvgam("hurdle_negbinomial_ar1")
  assert_log_lik_integrity(mvgam_fit, mvgam_fit$data)
})

test_that("Zero-inflated Poisson AR(1): log_lik returns finite matrix", {
  require_fixtures("val_brms_zero_inflated_poisson_ar1.rds",
                   "val_mvgam_zero_inflated_poisson_ar1.rds")
  mvgam_fit <- load_mvgam("zero_inflated_poisson_ar1")
  assert_log_lik_integrity(mvgam_fit, mvgam_fit$data)
})


# -- End-to-end downstream consumers -----------------------------------

test_that("loo.mvgam runs end-to-end and returns a finite estimate", {
  require_fixtures("val_brms_ar1_fx.rds", "val_mvgam_ar1_fx.rds")
  mvgam_fit <- load_mvgam("ar1_fx")
  loo_mvgam <- suppressWarnings(loo::loo(mvgam_fit))
  testthat::expect_s3_class(loo_mvgam, "loo")
  testthat::expect_true(
    is.finite(loo_mvgam$estimates["elpd_loo", "Estimate"])
  )
})

test_that("waic.mvgam returns a loo::waic object", {
  require_fixtures("val_brms_ar1_fx.rds", "val_mvgam_ar1_fx.rds")
  mvgam_fit <- load_mvgam("ar1_fx")
  w <- suppressWarnings(waic(mvgam_fit))
  testthat::expect_s3_class(w, "waic")
  testthat::expect_true(is.finite(w$estimates["waic", "Estimate"]))
})

test_that("pp_check loo_pit_overlay renders with PSIS weights", {
  require_fixtures("val_brms_ar1_fx.rds", "val_mvgam_ar1_fx.rds")
  mvgam_fit <- load_mvgam("ar1_fx")
  plt <- suppressWarnings(
    pp_check(mvgam_fit, type = "loo_pit_overlay", ndraws = 100)
  )
  testthat::expect_s3_class(plt, "ggplot")
})
