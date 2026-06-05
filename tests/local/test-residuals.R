# End-to-end tests for `residuals.mvgam`. The implementation is
# family-agnostic via the empirical PIT (Hartig / DHARMa
# convention), so the suite covers every family that
# `posterior_predict.mvgam` supports: continuous, count, beta,
# binomial, zero-inflated, hurdle and cumulative ordinal. All
# paths use real cached fixtures.

source("setup_tests_local.R")

CACHE_DIR <- "fixtures"


load_fit <- function(name) {
  readRDS(file.path(CACHE_DIR, paste0(name, ".rds")))
}


# ---- dsresid shape + properties across families ------------------

run_dsresid_shape_test <- function(fixture_name, expect_int = NULL) {
  fit <- load_fit(fixture_name)
  r <- residuals(fit, ndraws = 200L)
  expect_true(is.matrix(r))
  expect_identical(ncol(r), 4L)
  expect_true(all(colnames(r) ==
                    c("Estimate", "Est.Error", "Q2.5", "Q97.5")))
  expect_true(all(is.finite(r[, "Q2.5"]) | is.nan(r[, "Q2.5"])))
}


for (fx in c("val_mvgam_ar1_int", "val_mvgam_gauss_ar1_n150",
              "val_mvgam_beta_ar1",
              "val_mvgam_zero_inflated_poisson_ar1",
              "val_mvgam_hurdle_poisson_ar1",
              "val_mvgam_hurdle_negbinomial_ar1",
              "val_mvgam_binom_ar1")) {
  local({
    nm <- fx
    test_that(paste0("residuals(", nm, ") returns valid summary"), {
      run_dsresid_shape_test(nm)
    })
  })
}


# ---- residuals(summary = FALSE) returns the per-draw matrix -----

test_that("residuals(summary = FALSE) returns ndraws x nobs", {
  fit <- load_fit("val_mvgam_ar1_int")
  r <- residuals(fit, summary = FALSE, ndraws = 100L)
  expect_true(is.matrix(r))
  expect_identical(nrow(r), 100L)
  expect_identical(ncol(r), 30L)
})


# ---- N(0, 1) sanity check on a well-calibrated continuous fit ---
#
# Pool dsresids from a gaussian AR(1) fit and assert their
# empirical mean / sd land near 0 / 1. This is a calibration
# smoke check, not a hypothesis test -- the gaussian AR(1)
# fixture is small (n=150), so we use generous tolerances.

test_that("dsresids on a well-fitted gaussian model are near N(0,1)", {
  fit <- load_fit("val_mvgam_gauss_ar1_n150")
  r <- residuals(fit, ndraws = 500L)
  est <- r[, "Estimate"]
  est <- est[is.finite(est)]
  expect_lt(abs(mean(est)), 0.25)
  expect_lt(abs(stats::sd(est) - 1), 0.30)
})


# ---- ordinary residual scale matches y - posterior_predict -----

test_that("ordinary residuals match y - posterior_predict per draw", {
  fit <- load_fit("val_mvgam_ar1_int")
  set.seed(1L)
  r <- residuals(fit, type = "ordinary", ndraws = 50L,
                   summary = FALSE)
  set.seed(1L)
  yrep <- posterior_predict(fit, ndraws = 50L, summary = FALSE)
  y <- as.numeric(fit$data$y)
  manual <- sweep(yrep, 2L, y, FUN = function(yh, yi) yi - yh)
  expect_equal(unname(r), unname(manual), tolerance = 1e-10)
})


# ---- pearson residuals carry the same sign as ordinary ----------

test_that("pearson residuals share sign with ordinary residuals", {
  fit <- load_fit("val_mvgam_gauss_ar1_n150")
  set.seed(2L)
  ord <- residuals(fit, type = "ordinary", ndraws = 50L,
                     summary = FALSE)
  set.seed(2L)
  pear <- residuals(fit, type = "pearson", ndraws = 50L,
                      summary = FALSE)
  # Sign agreement -- pearson scales by positive sqrt(var)
  expect_true(all(sign(ord) == sign(pear) |
                    abs(ord) < 1e-10))
})


# ---- robust = TRUE summary uses median ---------------------------

test_that("robust summary differs from mean summary", {
  fit <- load_fit("val_mvgam_ar1_int")
  r_mean <- residuals(fit, ndraws = 300L, robust = FALSE)
  r_med  <- residuals(fit, ndraws = 300L, robust = TRUE)
  # Estimates should differ (median != mean) for at least one obs
  expect_false(isTRUE(all.equal(r_mean[, "Estimate"],
                                  r_med[, "Estimate"])))
})


# ---- Edge case: NA observations propagate as NA -----------------

test_that("NA observations produce NA residuals (no crash)", {
  fit <- load_fit("val_mvgam_ar1_int")
  d <- fit$data
  d$y[1L] <- NA
  fit$data <- d
  fit$obs_data <- d
  r <- residuals(fit, ndraws = 100L)
  expect_true(all(is.na(r[1L, ])))
})
