# Unit tests for `R/scoring_kernels.R`. Each test asserts a
# known-answer property or a structural contract; together they
# pin the per-cell kernel correctness and the shared
# apply_*_score() wrapper behaviour (NA guard, output shape,
# column naming).


# ----- Shared utility tests ---------------------------------------

test_that("in_central_pi flags truth inside / outside the PI", {
  fc <- stats::rnorm(2000L, mean = 0, sd = 1)
  # Truth at 0 lies inside any sensible central PI.
  expect_equal(in_central_pi(fc, 0, 0.9), 1L)
  # Truth at 5 lies far outside.
  expect_equal(in_central_pi(fc, 5, 0.9), 0L)
})


test_that("log_offset matches log(x + 0.001) and is monotone", {
  expect_equal(log_offset(0), log(0.001))
  expect_equal(log_offset(1), log(1.001))
  expect_true(log_offset(2) > log_offset(1))
})


test_that("apply_univariate_score returns [h, 2] with NA on NA truth", {
  truth <- c(1, NA, 3)
  fc <- matrix(stats::rnorm(30L), nrow = 10L, ncol = 3L)
  out <- apply_univariate_score(truth, fc, function(t, f, ...) {
    c(0.5, 1L)
  })
  expect_identical(dim(out), c(3L, 2L))
  expect_identical(colnames(out), c("score", "in_interval"))
  expect_true(is.na(out[2, 1]))
  expect_true(is.na(out[2, 2]))
  expect_equal(out[1, ], c(score = 0.5, in_interval = 1))
})


test_that("stack_fcs_per_horizon groups by horizon, not by series", {
  # Two series, h = 3, ndraws = 4. Each cell encodes
  # 100 * series + 10 * draw + time.
  fc_a <- matrix(c(111, 112, 113,
                    121, 122, 123,
                    131, 132, 133,
                    141, 142, 143),
                  nrow = 4L, byrow = TRUE)
  fc_b <- fc_a + 100
  stacked <- stack_fcs_per_horizon(list(fc_a, fc_b))
  expect_length(stacked, 3L)
  # First horizon -> series stacked, columns are draws.
  expect_identical(dim(stacked[[1]]), c(2L, 4L))
  # Series A's draw-1 / horizon-1 value is fc_a[1, 1] = 111.
  expect_equal(stacked[[1]][1, 1], 111)
  expect_equal(stacked[[1]][2, 1], 211)
})


# ----- CRPS -------------------------------------------------------

test_that("CRPS of a degenerate forecast (all draws equal truth) is 0", {
  fc <- matrix(2.5, nrow = 100L, ncol = 1L)
  out <- crps_mcmc_object(truth = 2.5, fc = fc)
  expect_equal(as.numeric(out[1, "score"]), 0)
  expect_equal(as.numeric(out[1, "in_interval"]), 1)
})


test_that("CRPS of a wider forecast is larger than a narrower one", {
  truth <- 0
  fc_narrow <- matrix(stats::rnorm(2000L, sd = 0.1),
                       nrow = 1000L, ncol = 2L)
  fc_wide <- matrix(stats::rnorm(2000L, sd = 1),
                     nrow = 1000L, ncol = 2L)
  s_narrow <- crps_mcmc_object(c(truth, truth), fc_narrow)
  s_wide <- crps_mcmc_object(c(truth, truth), fc_wide)
  expect_true(as.numeric(s_wide[1, "score"]) >
              as.numeric(s_narrow[1, "score"]))
})


# ----- DRPS -------------------------------------------------------

test_that("DRPS of a degenerate count forecast equal to truth is 0", {
  fc <- matrix(7L, nrow = 100L, ncol = 1L)
  out <- drps_mcmc_object(truth = 7L, fc = fc)
  expect_equal(as.numeric(out[1, "score"]), 0)
  expect_equal(as.numeric(out[1, "in_interval"]), 1)
})


test_that("DRPS skips NA truth and leaves the row NA", {
  fc <- matrix(rpois(200L, 5), nrow = 100L, ncol = 2L)
  out <- drps_mcmc_object(truth = c(5L, NA), fc = fc)
  expect_true(!is.na(out[1, "score"]))
  expect_true(is.na(out[2, "score"]))
})


# ----- SIS --------------------------------------------------------

test_that("SIS with a degenerate forecast equal to truth is 0", {
  fc <- matrix(3, nrow = 100L, ncol = 1L)
  out <- sis_mcmc_object(truth = 3, fc = fc)
  expect_equal(as.numeric(out[1, "score"]), 0)
})


test_that("SIS penalises a miss outside the PI", {
  # Forecast: tight around 0; truth at 10 (far above the 95% PI).
  fc <- matrix(stats::rnorm(1000L, sd = 0.1),
                nrow = 1000L, ncol = 1L)
  out_in <- sis_mcmc_object(0, fc)
  out_miss <- sis_mcmc_object(10, fc)
  expect_true(as.numeric(out_miss[1, "score"]) >
              as.numeric(out_in[1, "score"]))
  expect_equal(as.numeric(out_miss[1, "in_interval"]), 0)
})


# ----- Brier ------------------------------------------------------

test_that("Brier of a perfect binary forecast is 0", {
  # All draws predict 1 and truth is 1 -> MSE = 0.
  fc <- matrix(1, nrow = 100L, ncol = 1L)
  out <- brier_mcmc_object(truth = 1, fc = fc)
  expect_equal(as.numeric(out[1, "score"]), 0)
  expect_true(is.na(out[1, "in_interval"]))
})


test_that("Brier MSE matches mean((truth - fc)^2)", {
  truth <- 1
  fc <- matrix(c(0, 1, 0.5, 1, 1), nrow = 5L, ncol = 1L)
  out <- brier_mcmc_object(truth = truth, fc = fc)
  expect_equal(as.numeric(out[1, "score"]),
                mean((truth - fc[, 1])^2))
})


# ----- Energy -----------------------------------------------------

test_that("Energy score returns a length-h numeric vector", {
  truths <- matrix(c(0, 1, 0.5, 1.5,
                     -1, 0), nrow = 2L, ncol = 3L)
  fcs <- list(
    matrix(stats::rnorm(60L), nrow = 20L, ncol = 3L),
    matrix(stats::rnorm(60L), nrow = 20L, ncol = 3L)
  )
  out <- energy_mcmc_object(truths, fcs)
  expect_length(out, 3L)
  expect_true(all(is.finite(out)))
})


test_that("Energy of a degenerate forecast (draws = truth) is ~0", {
  truths <- matrix(c(1, 2, 3, 4), nrow = 2L, ncol = 2L)
  # 50 draws each identical to the per-series truth.
  fcs <- lapply(seq_len(2L), function(s) {
    matrix(rep(truths[s, ], each = 50L), nrow = 50L)
  })
  out <- energy_mcmc_object(truths, fcs)
  expect_true(all(out < 1e-6))
})


# ----- Variogram --------------------------------------------------

test_that("Variogram score returns a length-h numeric vector", {
  truths <- matrix(c(0, 1, 0.5, 1.5,
                     -1, 0), nrow = 2L, ncol = 3L)
  fcs <- list(
    matrix(stats::rnorm(60L), nrow = 20L, ncol = 3L),
    matrix(stats::rnorm(60L), nrow = 20L, ncol = 3L)
  )
  out <- variogram_mcmc_object(truths, fcs)
  expect_length(out, 3L)
  expect_true(all(is.finite(out)))
  expect_true(all(out >= 0))
})


test_that("Variogram score accepts per-series weights", {
  truths <- matrix(c(0, 1, 0.5, 1.5), nrow = 2L, ncol = 2L)
  fcs <- list(
    matrix(stats::rnorm(40L), nrow = 20L, ncol = 2L),
    matrix(stats::rnorm(40L), nrow = 20L, ncol = 2L)
  )
  w <- c(0.5, 2)
  out <- variogram_mcmc_object(truths, fcs, weights = w)
  expect_length(out, 2L)
  expect_true(all(is.finite(out)))
})


# ----- Log score --------------------------------------------------

test_that("Log score is finite and decreases as fc concentrates on truth", {
  fc_wide <- matrix(stats::rnorm(1000L, sd = 2),
                     nrow = 500L, ncol = 2L)
  fc_narrow <- matrix(stats::rnorm(1000L, sd = 0.3),
                       nrow = 500L, ncol = 2L)
  s_wide <- logs_mcmc_object(truth = c(0, 0), fc = fc_wide)
  s_narrow <- logs_mcmc_object(truth = c(0, 0), fc = fc_narrow)
  expect_true(all(is.finite(s_wide[, "score"])))
  # Sharper forecast concentrated on truth -> lower (better) log score.
  expect_true(mean(s_narrow[, "score"]) <
              mean(s_wide[, "score"]))
})


# ----- DSS --------------------------------------------------------

test_that("DSS matches its analytic formula on Gaussian draws", {
  truth <- 0
  fc <- matrix(stats::rnorm(5000L, mean = 0, sd = 1),
                nrow = 5000L, ncol = 1L)
  out <- dss_mcmc_object(truth, fc)
  mu <- mean(fc); s <- sd(fc)
  expected <- ((truth - mu) / s)^2 + 2 * log(s)
  # Sample-based estimator vs analytic: match within sampling
  # noise on 5000 draws.
  expect_equal(as.numeric(out[1, "score"]), expected,
                tolerance = 0.05)
})


# ----- Quantile (pinball) score -----------------------------------

test_that("Quantile score is 0 when truth equals the alpha quantile", {
  fc <- matrix(seq.int(1L, 100L), nrow = 100L, ncol = 1L)
  # 0.5 quantile of 1..100 is 50.5; pick truth at the median.
  out <- qs_mcmc_object(truth = 50.5, fc = fc, alpha = 0.5)
  # Pinball at the alpha-quantile is identically zero.
  expect_lt(as.numeric(out[1, "score"]), 1e-8)
})


test_that("Quantile score differs across alpha and is finite-positive", {
  # When truth lies in the upper tail, the 0.95-quantile pinball
  # loss applies the larger asymmetric weight (0.95 vs 0.05),
  # so QS_0.95 is the larger of the two. Asserting that
  # direction confirms the pinball loss is computed with the
  # correct asymmetric weighting (alpha * positive_residual +
  # (1 - alpha) * negative_residual).
  fc <- matrix(stats::rnorm(1000L), nrow = 1000L, ncol = 1L)
  truth <- 3
  s_lower <- qs_mcmc_object(truth, fc, alpha = 0.05)
  s_upper <- qs_mcmc_object(truth, fc, alpha = 0.95)
  expect_true(all(is.finite(c(as.numeric(s_lower[1, "score"]),
                              as.numeric(s_upper[1, "score"])))))
  expect_true(as.numeric(s_upper[1, "score"]) >
              as.numeric(s_lower[1, "score"]))
})


# ----- Threshold-weighted CRPS ------------------------------------

test_that("twCRPS with (a = -Inf, b = Inf) equals plain CRPS", {
  truth <- 1
  fc <- matrix(stats::rnorm(500L), nrow = 500L, ncol = 1L)
  plain <- crps_mcmc_object(truth, fc)
  tw <- twcrps_mcmc_object(truth, fc)
  expect_equal(as.numeric(tw[1, "score"]),
                as.numeric(plain[1, "score"]),
                tolerance = 1e-8)
})


test_that("twCRPS with a > truth focuses scoring on the upper tail", {
  truth <- 0
  fc <- matrix(stats::rnorm(500L), nrow = 500L, ncol = 1L)
  plain <- crps_mcmc_object(truth, fc)
  # Threshold above truth -> the chaining function flattens
  # everything below it, reducing the contribution from the
  # bulk of the predictive distribution.
  tw <- twcrps_mcmc_object(truth, fc, lower = 2)
  expect_true(as.numeric(tw[1, "score"]) <
              as.numeric(plain[1, "score"]))
})


# ----- Threshold-weighted Energy ----------------------------------

test_that("twEnergy returns a length-h vector for multivariate forecasts", {
  truths <- matrix(c(0, 1, 0.5, 1.5), nrow = 2L, ncol = 2L)
  fcs <- list(
    matrix(stats::rnorm(40L), nrow = 20L, ncol = 2L),
    matrix(stats::rnorm(40L), nrow = 20L, ncol = 2L)
  )
  out <- twenergy_mcmc_object(truths, fcs, lower = -1, upper = 2)
  expect_length(out, 2L)
  expect_true(all(is.finite(out)))
})
