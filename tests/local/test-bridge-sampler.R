# Local end-to-end tests for bridge_sampler.mvgam +
# bayes_factor.mvgam. These live in tests/local/ because bridge
# sampling needs a real stanfit (fixture-dependent), and each
# fixture that came from disk needs one full refit via
# `update(fit, recompile = TRUE)` before bridge sampling can
# read `log_prob` off it. CI's testthat sweep must not depend on
# this.
#
# Coverage:
#
#   1. Univariate Poisson AR(1) fit: bridge_sampler returns a
#      finite logml.
#   2. Univariate Gaussian AR(1) fit: same.
#   3. Multi-response Gaussian fit (mvbf, no cross-response
#      trend): bridge_sampler returns a finite joint logml.
#   4. IPM article's mvbf pair (`mod_joint` vs `mod_wrong_trend`,
#      three-response count + p/a + p/o joint fit): bayes_factor
#      picks the correctly-specified fit.
#
# Deterministic-within-noise: bridge sampling is stochastic on
# fixed draws. Two calls on the same recompiled fit agree to
# ~0.5 on the log scale under the default 1000-draw budget
# (Gronau 2020 §3.4).

library("testthat")
if (!requireNamespace("bridgesampling", quietly = TRUE)) {
  message("bridgesampling not installed; skipping bridge-sampler tests")
} else {
  devtools::load_all()

  # Recompile each RDS fixture up front so `bridge_sampler()`
  # can read `log_prob` from a live compiled Stan model. Each
  # `update(recompile = TRUE)` takes ~30-60s; caching the
  # results here keeps per-test runtime dominated by the actual
  # bridge sampling call.
  fit_pois <- update(
    readRDS("tests/local/fixtures/val_mvgam_ar1_int.rds"),
    recompile = TRUE
  )
  fit_gauss <- update(
    readRDS("tests/local/fixtures/val_mvgam_gauss_ar1_n150.rds"),
    recompile = TRUE
  )
  fit_mvbf <- update(
    readRDS("tests/local/fixtures/val_mvgam_mv_gauss.rds"),
    recompile = TRUE
  )
  fit_joint <- update(
    readRDS("pkgdown/ipm_cache/mod_joint.rds"),
    recompile = TRUE
  )
  fit_wrong <- update(
    readRDS("pkgdown/ipm_cache/mod_wrong_trend.rds"),
    recompile = TRUE
  )

  frac_tol_logml <- 0.5

  test_that("bridge_sampler returns finite logml on Poisson AR(1)", {
    bs <- bridge_sampler(fit_pois, silent = TRUE)
    expect_s3_class(bs, "bridge")
    expect_true(is.finite(bs$logml))
  })

  test_that("bridge_sampler returns finite logml on Gaussian AR(1)", {
    bs <- bridge_sampler(fit_gauss, silent = TRUE)
    expect_s3_class(bs, "bridge")
    expect_true(is.finite(bs$logml))
  })

  test_that("bridge_sampler returns a finite joint logml on mvbf", {
    bs <- bridge_sampler(fit_mvbf, silent = TRUE)
    expect_s3_class(bs, "bridge")
    expect_true(is.finite(bs$logml))
  })

  test_that("bridge_sampler is deterministic within Monte Carlo noise", {
    bs1 <- bridge_sampler(fit_pois, silent = TRUE)
    bs2 <- bridge_sampler(fit_pois, silent = TRUE)
    expect_equal(bs1$logml, bs2$logml, tolerance = frac_tol_logml)
  })

  test_that("bayes_factor picks the correctly-specified IPM fit", {
    bs_joint <- bridge_sampler(fit_joint, silent = TRUE)
    bs_wrong <- bridge_sampler(fit_wrong, silent = TRUE)
    expect_true(is.finite(bs_joint$logml))
    expect_true(is.finite(bs_wrong$logml))
    log_bf <- bs_joint$logml - bs_wrong$logml
    # Joint (correct) should dominate mis-specified trend. Bridge
    # sampling has less horizon noise than LFO at fc_horizon = 12
    # (elpd_diff = 7.6 +- 6.4), so the log BF should sit well
    # above zero on the same data.
    expect_true(log_bf > 0)

    bf <- bayes_factor(fit_joint, fit_wrong)
    expect_s3_class(bf, "bayes_factor")
    expect_true(is.finite(bf$bf))
    expect_true(bf$bf > 1)
  })

  test_that("bayes_factor accepts pre-computed bridge objects", {
    bs <- bridge_sampler(fit_pois, silent = TRUE)
    bf <- bayes_factor(bs, bs)
    expect_s3_class(bf, "bayes_factor")
    # BF of a fit against itself is 1 on the ratio scale.
    expect_equal(bf$bf, 1, tolerance = 1e-8)
  })

  test_that("bridge_sampler recompile = TRUE round-trips off RDS", {
    stale <- readRDS("tests/local/fixtures/val_mvgam_ar1_int.rds")
    bs <- bridge_sampler(stale, recompile = TRUE, silent = TRUE)
    expect_s3_class(bs, "bridge")
    expect_true(is.finite(bs$logml))
  })
}
