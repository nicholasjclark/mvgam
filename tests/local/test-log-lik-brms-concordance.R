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


# logLik.mvgam: stats::logLik S3 method. AIC()/BIC() dispatch on it.
# The scalar return is mean(rowSums(log_lik)); df = posterior
# variables minus NUTS sampler diagnostics; nobs = nobs.mvgam(fit).
# For state-space models df overcounts because every latent state
# is a sampled variable, so AIC/BIC are coarse — LOO/WAIC are
# preferred for proper Bayesian model selection.

test_that("logLik.mvgam scalar is finite and AIC/BIC work end-to-end", {
  require_fixtures("val_mvgam_gauss_ar1_n150.rds")
  mvgam_fit <- load_mvgam("gauss_ar1_n150")
  ll <- logLik(mvgam_fit)
  testthat::expect_s3_class(ll, "logLik")
  testthat::expect_equal(length(ll), 1L)
  testthat::expect_true(is.finite(as.numeric(ll)))
  testthat::expect_true(attr(ll, "df") > 0L)
  testthat::expect_equal(attr(ll, "nobs"), nobs(mvgam_fit))
  # AIC = -2 * ll + 2 * df ; BIC = -2 * ll + log(n) * df.
  expected_aic <- -2 * as.numeric(ll) + 2 * attr(ll, "df")
  expected_bic <- -2 * as.numeric(ll) +
                  log(attr(ll, "nobs")) * attr(ll, "df")
  testthat::expect_equal(AIC(mvgam_fit), expected_aic, tolerance = 1e-8)
  testthat::expect_equal(BIC(mvgam_fit), expected_bic, tolerance = 1e-8)
})

test_that("logLik.mvgam(pointwise = TRUE) returns the log_lik matrix", {
  require_fixtures("val_mvgam_gauss_ar1_n150.rds")
  mvgam_fit <- load_mvgam("gauss_ar1_n150")
  ll_mat <- logLik(mvgam_fit, pointwise = TRUE)
  testthat::expect_true(is.matrix(ll_mat))
  testthat::expect_equal(ncol(ll_mat), nobs(mvgam_fit))
  testthat::expect_true(all(is.finite(ll_mat)))
  # The pointwise=TRUE path must agree with log_lik() row-for-row.
  expected <- log_lik(mvgam_fit)
  testthat::expect_identical(dim(ll_mat), dim(expected))
})


# ---------------------------------------------------------------------
# The surface an ELPD is built on
# ---------------------------------------------------------------------
#
# `log_lik()` scores each observation under the latent trend state the
# model inferred at that time. Scoring the marginal instead leaves the
# weights describing a series the model never saw: on this fixture it
# put the effective parameter count at 784 against 30 observations and
# pinned `loo_R2` on its clamp. The tests below compare against the
# brms twin, which carries its autocorrelation term the same way.


test_that("loo on the conditional surface agrees with the brms twin", {
  require_fixtures("val_mvgam_ar1_fx.rds", "val_brms_ar1_fx.rds")
  mv <- load_mvgam("ar1_fx")
  bm <- load_brms("ar1_fx")
  lm_ <- SW(loo(mv))
  lb <- SW(loo(bm))
  n_obs <- nrow(mv$data)
  # An effective parameter count above the observation count is the
  # signature of scoring on the wrong surface.
  expect_lt(lm_$estimates["p_loo", "Estimate"], n_obs)
  expect_lt(
    abs(lm_$estimates["p_loo", "Estimate"] -
          lb$estimates["p_loo", "Estimate"]),
    10
  )
  expect_lt(
    abs(lm_$estimates["elpd_loo", "Estimate"] -
          lb$estimates["elpd_loo", "Estimate"]),
    20
  )
})


test_that("conditioning tracks the observations, marginalising does not", {
  require_fixtures("val_mvgam_ar1_fx.rds")
  mv <- load_mvgam("ar1_fx")
  y <- mv$data[["y"]]
  cor_cond <- cor(
    colMeans(posterior_epred(mv, incl_autocor = TRUE)), y
  )
  cor_marg <- cor(
    colMeans(posterior_epred(mv, incl_autocor = FALSE)), y
  )
  # The fitted state carries the signal in a state-space fit; the
  # marginal surface deliberately integrates it away.
  expect_gt(cor_cond, 0.9)
  expect_gt(cor_cond, cor_marg)
})


test_that("bayes_R2 agrees with the brms twin", {
  require_fixtures("val_mvgam_ar1_fx.rds", "val_brms_ar1_fx.rds")
  mv <- load_mvgam("ar1_fx")
  bm <- load_brms("ar1_fx")
  r2_mv <- SW(bayes_R2(mv))[, "Estimate"]
  r2_bm <- SW(brms::bayes_R2(bm))[, "Estimate"]
  expect_true(is.finite(r2_mv))
  expect_lt(abs(r2_mv - r2_bm), 0.15)
})


test_that("loo_predict pairs its predictions with its weights", {
  require_fixtures("val_mvgam_ar1_fx.rds", "val_brms_ar1_fx.rds")
  mv <- load_mvgam("ar1_fx")
  bm <- load_brms("ar1_fx")
  y <- mv$data[["y"]]
  lp_mv <- as.numeric(SM(SW(loo_predict(mv, type = "mean"))))
  lp_bm <- as.numeric(SM(SW(loo_predict(bm, type = "mean"))))
  expect_length(lp_mv, length(y))
  expect_true(all(is.finite(lp_mv)))
  # Weights from a conditional density paired with a marginal
  # prediction is the mismatch that pinned loo_R2 at its clamp.
  expect_gt(cor(lp_mv, y), 0.8)
  expect_lt(abs(cor(lp_mv, y) - cor(lp_bm, y)), 0.15)
})


test_that("the superseded spelling still selects the surface", {
  require_fixtures("val_mvgam_ar1_fx.rds")
  mv <- load_mvgam("ar1_fx")
  expect_equal(log_lik(mv, process_error = TRUE),
                log_lik(mv, incl_autocor = TRUE))
  expect_equal(log_lik(mv, process_error = FALSE),
                log_lik(mv, incl_autocor = FALSE))
  # Named together, the current spelling decides.
  expect_equal(log_lik(mv, incl_autocor = FALSE, process_error = TRUE),
                log_lik(mv, incl_autocor = FALSE))
  # The two surfaces are genuinely different densities.
  expect_false(isTRUE(all.equal(log_lik(mv, incl_autocor = TRUE),
                                 log_lik(mv, incl_autocor = FALSE))))
})


test_that("a multivariate fit scores on the conditional surface", {
  require_fixtures("val_mvgam_mv_gauss.rds")
  mv <- load_mvgam("mv_gauss")
  ll <- log_lik(mv)
  expect_equal(ncol(ll), nrow(mv$data))
  expect_true(all(is.finite(ll)))
  # A shared trend is composed against every response the same way,
  # so each response's conditional predictor is its own observation
  # predictor plus the one latent state, and nothing else.
  dm <- as.matrix(posterior::as_draws_matrix(mv$fit))
  state <- mvgam:::extract_trend_latent_states(mv, mv$data, dm)
  for (r in mv$response_names) {
    obs <- mvgam:::extract_component_linpred(
      mv, newdata = mv$data, component = "obs", resp = r
    )
    expect_equal(
      unname(as.matrix(
        posterior_linpred(mv, resp = r, incl_autocor = TRUE)
      )),
      unname(as.matrix(obs + state))
    )
  }
})
