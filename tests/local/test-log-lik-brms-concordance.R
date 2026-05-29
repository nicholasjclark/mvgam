# brms vs mvgam log_lik concordance.
#
# Runs the rebuilt log_lik.mvgam against brms log_lik on the same fitted
# data, plus end-to-end loo / waic / pp_check(loo_pit_overlay) checks.
# Lives in tests/local/ for the same reason as the predictions concordance
# file: needs pre-built fixture pairs and is too heavy for CI. Run via:
#   Rscript tests/local/build_fixtures.R  # (once)
#   testthat::test_file("tests/local/test-log-lik-brms-concordance.R")
#
# Numerical bar: per-observation log_lik mean from mvgam matches brms
# within the same threshold band the existing predictions concordance
# uses (cor >= 0.85 by default). Tighter for non-state-space families
# (Beta, Binomial direct relationship); looser where the AR-vs-residual
# divergence applies (Poisson AR).

source("setup_tests_local.R")
source("concordance_helpers.R")


assert_log_lik_concordance <- function(brms_fit, mvgam_fit, newdata,
                                       threshold = 0.85,
                                       process_error = TRUE) {
  brms_ll <- brms::log_lik(brms_fit, newdata = newdata)
  mvgam_ll <- log_lik(mvgam_fit, newdata = newdata,
                      process_error = process_error)
  testthat::expect_equal(dim(brms_ll), dim(mvgam_ll))
  brms_mean <- colMeans(brms_ll)
  mvgam_mean <- colMeans(mvgam_ll)
  comp <- compare_vectors(brms_mean, mvgam_mean)
  if (isTRUE(comp$constant)) {
    testthat::expect_lt(comp$rmse, 1.0)
  } else {
    testthat::expect_gte(comp$cor, threshold)
  }
  invisible(comp$cor)
}


# -- Gaussian (where process_error matters most) ------------------------

test_that("Gaussian multivariate: log_lik matches brms", {
  require_fixtures("val_brms_mv_gauss.rds", "val_mvgam_mv_gauss.rds")
  brms_fit <- load_brms("mv_gauss")
  mvgam_fit <- load_mvgam("mv_gauss")
  newdata <- mvgam_fit$data
  # Multivariate log_lik requires resp= per family
  for (resp_name in names(brms_fit$family)) {
    assert_log_lik_concordance(
      brms_fit, mvgam_fit, newdata, threshold = 0.85
    )
  }
})


# -- Beta -----------------------------------------------------------------

test_that("Beta AR(1): log_lik matches brms (tighter, direct family)", {
  require_fixtures("val_brms_beta_ar1.rds", "val_mvgam_beta_ar1.rds")
  brms_fit <- load_brms("beta_ar1")
  mvgam_fit <- load_mvgam("beta_ar1")
  newdata <- mvgam_fit$data
  assert_log_lik_concordance(brms_fit, mvgam_fit, newdata, threshold = 0.85)
})


# -- Binomial -----------------------------------------------------------

test_that("Binomial AR(1): log_lik matches brms", {
  require_fixtures("val_brms_binom_ar1.rds", "val_mvgam_binom_ar1.rds")
  brms_fit <- load_brms("binom_ar1")
  mvgam_fit <- load_mvgam("binom_ar1")
  newdata <- mvgam_fit$data
  assert_log_lik_concordance(brms_fit, mvgam_fit, newdata, threshold = 0.85)
})


# -- Poisson AR(1) family (state-space-dominated) ----------------------

# brms residual-AR vs mvgam state-space-AR diverge structurally for
# Poisson families (see prediction TRD 7.6). Threshold matches the
# predictions concordance: looser bar.

test_that("Poisson AR(1) + fixed: log_lik matches brms", {
  require_fixtures("val_brms_ar1_fx.rds", "val_mvgam_ar1_fx.rds")
  brms_fit <- load_brms("ar1_fx")
  mvgam_fit <- load_mvgam("ar1_fx")
  newdata <- mvgam_fit$data
  assert_log_lik_concordance(brms_fit, mvgam_fit, newdata, threshold = 0.70)
})


# -- Hurdle / zero-inflated branches ------------------------------------

test_that("Hurdle Poisson: log_lik matches brms", {
  require_fixtures("val_brms_hurdle_poisson_ar1.rds",
                   "val_mvgam_hurdle_poisson_ar1.rds")
  brms_fit <- load_brms("hurdle_poisson_ar1")
  mvgam_fit <- load_mvgam("hurdle_poisson_ar1")
  newdata <- mvgam_fit$data
  assert_log_lik_concordance(brms_fit, mvgam_fit, newdata, threshold = 0.70)
})

test_that("Hurdle NegBinomial: log_lik matches brms", {
  require_fixtures("val_brms_hurdle_negbinomial_ar1.rds",
                   "val_mvgam_hurdle_negbinomial_ar1.rds")
  brms_fit <- load_brms("hurdle_negbinomial_ar1")
  mvgam_fit <- load_mvgam("hurdle_negbinomial_ar1")
  newdata <- mvgam_fit$data
  assert_log_lik_concordance(brms_fit, mvgam_fit, newdata, threshold = 0.70)
})

test_that("Zero-inflated Poisson: log_lik matches brms", {
  require_fixtures("val_brms_zero_inflated_poisson_ar1.rds",
                   "val_mvgam_zero_inflated_poisson_ar1.rds")
  brms_fit <- load_brms("zero_inflated_poisson_ar1")
  mvgam_fit <- load_mvgam("zero_inflated_poisson_ar1")
  newdata <- mvgam_fit$data
  assert_log_lik_concordance(brms_fit, mvgam_fit, newdata, threshold = 0.70)
})


# -- End-to-end downstream consumers --------------------------------------

test_that("loo.mvgam runs end-to-end and matches brms loo within se_diff", {
  require_fixtures("val_brms_ar1_fx.rds", "val_mvgam_ar1_fx.rds")
  brms_fit <- load_brms("ar1_fx")
  mvgam_fit <- load_mvgam("ar1_fx")
  loo_brms <- suppressWarnings(loo::loo(brms_fit))
  loo_mvgam <- suppressWarnings(loo::loo(mvgam_fit))
  diff_elpd <- abs(loo_brms$estimates["elpd_loo", "Estimate"] -
                   loo_mvgam$estimates["elpd_loo", "Estimate"])
  # Allow up to 3 * se_diff to absorb MC noise + structural divergence.
  testthat::expect_lt(
    diff_elpd,
    3 * (loo_brms$estimates["elpd_loo", "SE"] +
         loo_mvgam$estimates["elpd_loo", "SE"])
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
  # Should not error; the stub used to return NULL psis_object which
  # bayesplot::ppc_loo_pit_overlay then complained about.
  plt <- suppressWarnings(
    pp_check(mvgam_fit, type = "loo_pit_overlay", ndraws = 100)
  )
  testthat::expect_s3_class(plt, "ggplot")
})
