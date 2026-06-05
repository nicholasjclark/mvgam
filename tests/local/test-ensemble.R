# End-to-end tests for `ensemble.mvgam_forecast()`. Reuses the
# paired forecast fixtures cached under
# `tests/local/fixtures/forecast_recovery/` by
# `test-forecast-misspec.R`. Each `pair_<truth>_truth_<fit>_fit_seedN.rds`
# bundle stores `list(sim, fit, fc)`, so two bundles fit on the
# SAME simulated data give a matching pair of `mvgam_forecast`
# objects we can hand to `ensemble()`.
#
# The pair-AR fixtures (AR(1) Poisson on AR(0.7) truth vs RW
# Poisson on the same truth) are the simplest available -- both
# have a single response, two series, the same training and test
# windows, and 200 forecast draws each.

source("setup_tests_local.R")

CACHE_DIR <- "fixtures/forecast_recovery"


# ---- Fixture loaders ---------------------------------------------

load_pair <- function(seed = 1001L) {
  correct <- readRDS(file.path(
    CACHE_DIR, paste0("pair_ar_truth_ar_fit_seed", seed, ".rds")
  ))
  misspec <- readRDS(file.path(
    CACHE_DIR, paste0("pair_ar_truth_rw_fit_seed", seed, ".rds")
  ))
  list(fc_a = correct$fc, fc_b = misspec$fc,
       sim = correct$sim)
}


# ---- Default (even-weight) ensembling ----------------------------

test_that("ensemble.mvgam_forecast returns valid mvgam_forecast", {
  pair <- load_pair(1001L)
  ens <- ensemble(pair$fc_a, pair$fc_b, ndraws = 500L,
                    seed = 7L)
  expect_s3_class(ens, "mvgam_forecast")
  expect_identical(
    names(ens$hindcasts), names(pair$fc_a$hindcasts)
  )
  expect_identical(
    names(ens$forecasts), names(pair$fc_a$forecasts)
  )
  # Every series must have exactly `ndraws` rows
  hc_rows <- vapply(ens$hindcasts, NROW, integer(1L))
  fc_rows <- vapply(ens$forecasts, NROW, integer(1L))
  expect_true(all(hc_rows == 500L))
  expect_true(all(fc_rows == 500L))
  # Forecast / hindcast columns are preserved
  expect_identical(
    vapply(ens$forecasts, ncol, integer(1L)),
    vapply(pair$fc_a$forecasts, ncol, integer(1L))
  )
  expect_identical(
    vapply(ens$hindcasts, ncol, integer(1L)),
    vapply(pair$fc_a$hindcasts, ncol, integer(1L))
  )
  # Default weights are even and recorded as attributes
  expect_equal(unname(attr(ens, "weights")), c(0.5, 0.5))
  expect_length(attr(ens, "weights"), 2L)
  expect_equal(sum(attr(ens, "ndraws_per_model")), 500L)
})


test_that("ensemble preserves test arms from the first input", {
  pair <- load_pair(1001L)
  ens <- ensemble(pair$fc_a, pair$fc_b, ndraws = 200L, seed = 1L)
  expect_identical(ens$test_observations,
                    pair$fc_a$test_observations)
  expect_identical(ens$test_times, pair$fc_a$test_times)
  expect_identical(ens$train_observations,
                    pair$fc_a$train_observations)
  expect_identical(ens$train_times, pair$fc_a$train_times)
  expect_identical(ens$series_names, pair$fc_a$series_names)
})


# ---- User-specified weights --------------------------------------

test_that("numeric weights are normalised + drive draw split", {
  pair <- load_pair(1001L)
  ens <- ensemble(pair$fc_a, pair$fc_b,
                    weights = c(0.7, 0.3),
                    ndraws = 1000L, seed = 2L)
  w <- attr(ens, "weights")
  np <- attr(ens, "ndraws_per_model")
  expect_equal(unname(w), c(0.7, 0.3))
  # Largest-remainder rounding preserves the total
  expect_equal(sum(np), 1000L)
  expect_equal(unname(np), c(700L, 300L))
})


test_that("unnormalised numeric weights are accepted", {
  pair <- load_pair(1001L)
  ens <- ensemble(pair$fc_a, pair$fc_b,
                    weights = c(3, 1),
                    ndraws = 400L, seed = 3L)
  expect_equal(unname(attr(ens, "weights")), c(0.75, 0.25))
  expect_equal(sum(attr(ens, "ndraws_per_model")), 400L)
})


# ---- Reproducibility ---------------------------------------------

test_that("seed= makes ensemble bit-exactly reproducible", {
  pair <- load_pair(1001L)
  ens1 <- ensemble(pair$fc_a, pair$fc_b, ndraws = 200L,
                     seed = 42L)
  ens2 <- ensemble(pair$fc_a, pair$fc_b, ndraws = 200L,
                     seed = 42L)
  expect_identical(ens1$forecasts, ens2$forecasts)
  expect_identical(ens1$hindcasts, ens2$hindcasts)
})


# ---- Downstream compatibility: score / plot pipelines ------------

test_that("score(ensemble_fc) returns finite values per series", {
  pair <- load_pair(1001L)
  ens <- ensemble(pair$fc_a, pair$fc_b, ndraws = 300L,
                    seed = 9L)
  sc <- score(ens, score = "crps")
  for (nm in setdiff(names(sc), "all_series")) {
    expect_true(all(is.finite(sc[[nm]]$score)))
  }
})


# ---- Validation errors -------------------------------------------

test_that("series-name mismatch errors informatively", {
  pair <- load_pair(1001L)
  fc_b_mut <- pair$fc_b
  fc_b_mut$series_names <- factor(
    paste0("other_", as.character(fc_b_mut$series_names)),
    levels = paste0("other_",
                     as.character(fc_b_mut$series_names))
  )
  expect_error(
    ensemble(pair$fc_a, fc_b_mut),
    "Series names must match"
  )
})


test_that("forecast-horizon mismatch errors informatively", {
  pair <- load_pair(1001L)
  fc_b_mut <- pair$fc_b
  # Trim one column off every forecast matrix
  fc_b_mut$forecasts <- lapply(
    fc_b_mut$forecasts,
    function(m) m[, -1L, drop = FALSE]
  )
  expect_error(
    ensemble(pair$fc_a, fc_b_mut),
    "Forecast horizons must match"
  )
})


test_that("hindcast-length mismatch errors informatively", {
  pair <- load_pair(1001L)
  fc_b_mut <- pair$fc_b
  fc_b_mut$hindcasts <- lapply(
    fc_b_mut$hindcasts,
    function(m) m[, -1L, drop = FALSE]
  )
  expect_error(
    ensemble(pair$fc_a, fc_b_mut),
    "Hindcast lengths must match"
  )
})


test_that("test-observation mismatch errors informatively", {
  pair <- load_pair(1001L)
  fc_b_mut <- pair$fc_b
  fc_b_mut$test_observations[[1L]][1L] <-
    fc_b_mut$test_observations[[1L]][1L] + 1
  expect_error(
    ensemble(pair$fc_a, fc_b_mut),
    "Test observations must match"
  )
})


test_that("single-input call errors", {
  pair <- load_pair(1001L)
  expect_error(
    ensemble(pair$fc_a),
    "at least two"
  )
})


test_that("non-forecast input via ... errors", {
  pair <- load_pair(1001L)
  expect_error(
    ensemble(pair$fc_a, foo = list()),
    "mvgam_forecast"
  )
})


test_that("wrong-length numeric weights error", {
  pair <- load_pair(1001L)
  expect_error(
    ensemble(pair$fc_a, pair$fc_b, weights = c(0.5, 0.3, 0.2)),
    "one entry per model"
  )
})


test_that("negative numeric weights error", {
  pair <- load_pair(1001L)
  expect_error(
    ensemble(pair$fc_a, pair$fc_b, weights = c(-0.5, 1.5)),
    "non-negative"
  )
})


test_that("zero-total numeric weights error", {
  pair <- load_pair(1001L)
  expect_error(
    ensemble(pair$fc_a, pair$fc_b, weights = c(0, 0)),
    "positive total"
  )
})


# ---- Score interpolation between two very different models ------
#
# The AR(1) and RW fits on AR(0.7) Poisson truth produce
# substantially different forecast distributions: AR(1) is
# correctly specified (variance saturates at the stationary
# marginal), RW is misspecified (variance grows linearly with
# horizon, over-covering). An even-weighted ensemble of the
# two should produce a pooled CRPS that sits between the two
# individual pooled CRPS values.
#
# CRPS is not strictly convex in the forecast distribution, so
# we allow a small slack on each side to absorb the
# resample-induced Monte Carlo noise.

pool_crps <- function(fc) {
  sc <- score(fc, score = "crps")
  vals <- numeric(0L)
  for (nm in setdiff(names(sc), "all_series")) {
    vals <- c(vals, sc[[nm]]$score)
  }
  mean(vals[is.finite(vals)])
}


test_that(paste0(
  "ensemble pooled CRPS falls between the two member ",
  "models' pooled CRPS"
), {
  per_seed <- lapply(c(1001L, 1002L, 1003L), function(seed) {
    pair <- load_pair(seed)
    ens <- ensemble(pair$fc_a, pair$fc_b, ndraws = 1000L,
                      seed = seed)
    list(
      a   = pool_crps(pair$fc_a),
      b   = pool_crps(pair$fc_b),
      ens = pool_crps(ens)
    )
  })
  # Pool across seeds: average each side's CRPS over the three
  # paired runs. Doing so gives a stable estimate insensitive
  # to per-seed Monte Carlo wiggle.
  crps_a <- mean(vapply(per_seed, `[[`, numeric(1L), "a"))
  crps_b <- mean(vapply(per_seed, `[[`, numeric(1L), "b"))
  crps_e <- mean(vapply(per_seed, `[[`, numeric(1L), "ens"))
  # The two members must actually differ; otherwise the test
  # has no power. AR(1) is correctly specified, RW is misspec,
  # so the gap should be visible.
  expect_true(abs(crps_a - crps_b) > 0.05)
  lo <- min(crps_a, crps_b)
  hi <- max(crps_a, crps_b)
  slack <- 0.05 * (hi - lo)
  expect_gte(crps_e, lo - slack)
  expect_lte(crps_e, hi + slack)
})


test_that(paste0(
  "weighting an ensemble toward the better model improves ",
  "pooled CRPS"
), {
  per_seed <- lapply(c(1001L, 1002L, 1003L), function(seed) {
    pair <- load_pair(seed)
    # `fc_a` is the correctly-specified AR(1) fit and so has
    # the lower (better) CRPS. Weighting the ensemble heavily
    # toward `fc_a` should produce an ensemble CRPS closer to
    # `fc_a`'s than to `fc_b`'s.
    ens_even <- ensemble(pair$fc_a, pair$fc_b,
                            ndraws = 1000L, seed = seed)
    ens_a    <- ensemble(pair$fc_a, pair$fc_b,
                            weights = c(0.9, 0.1),
                            ndraws = 1000L, seed = seed)
    list(
      a       = pool_crps(pair$fc_a),
      even    = pool_crps(ens_even),
      heavy_a = pool_crps(ens_a)
    )
  })
  crps_a       <- mean(vapply(per_seed, `[[`, numeric(1L), "a"))
  crps_even    <- mean(vapply(per_seed, `[[`, numeric(1L), "even"))
  crps_heavy_a <- mean(vapply(per_seed, `[[`, numeric(1L),
                                "heavy_a"))
  # Heavy-on-the-correct-model ensemble must score better
  # (lower CRPS) than the even-weighted ensemble.
  expect_lt(crps_heavy_a, crps_even)
  # And must be closer to the correct model's own CRPS than
  # the even-weighted ensemble is.
  expect_lt(abs(crps_heavy_a - crps_a),
              abs(crps_even - crps_a))
})


# ---- Sanity: ensemble distribution lies between member CDFs ------
#
# For any single forecast cell, the marginal empirical CDF of the
# even-weighted ensemble must lie weakly between the per-model
# CDFs evaluated at the same threshold. This is the defining
# property of a mixture distribution.

test_that("ensemble marginal CDF lies between member CDFs", {
  pair <- load_pair(1001L)
  ens <- ensemble(pair$fc_a, pair$fc_b, ndraws = 2000L,
                    seed = 11L)
  # Use the first series, first horizon
  draws_a <- pair$fc_a$forecasts[[1L]][, 1L]
  draws_b <- pair$fc_b$forecasts[[1L]][, 1L]
  draws_e <- ens$forecasts[[1L]][, 1L]
  # Threshold = median of the combined draws
  thr <- stats::median(c(draws_a, draws_b))
  cdf_a <- mean(draws_a <= thr)
  cdf_b <- mean(draws_b <= thr)
  cdf_e <- mean(draws_e <= thr)
  lo <- min(cdf_a, cdf_b)
  hi <- max(cdf_a, cdf_b)
  # Allow a small Monte-Carlo slack from the resample
  expect_true(cdf_e >= lo - 0.05 && cdf_e <= hi + 0.05)
})
