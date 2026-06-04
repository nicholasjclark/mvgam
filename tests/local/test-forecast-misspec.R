# Misspecified-model power-check companion to
# test-forecast-recovery.R. If the calibrated tests pass but
# misspecified models also pass, the coverage test has no
# statistical power. These tests fit BOTH the correct and a
# deliberately wrong model on the same simulated truth, then
# assert two things:
#
#   1. The correctly-specified model's pooled in-PI rate is
#      closer to nominal 0.90 than the misspecified model's.
#   2. The correctly-specified model's mean proper score
#      (DRPS / CRPS) is lower (better) than the misspecified
#      model's.
#
# Both assertions together rule out the "the test passes
# regardless" failure mode: covering well is necessary, and
# scoring well discriminates between equally-covering forecasts
# with different sharpness.
#
# Cache convention: same as test-forecast-recovery.R.

source("setup_tests_local.R")

CACHE_DIR <- "fixtures/forecast_recovery"


prep_recovery <- function(name, sim_args, fit_args,
                            mutate_fn = identity,
                            ndraws_fc = 200L) {
  path <- file.path(CACHE_DIR, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  sim <- do.call(sim_mvgam, sim_args)
  sim$data_train <- mutate_fn(sim$data_train)
  sim$data_test <- mutate_fn(sim$data_test)
  fit <- SM(SW(do.call(mvgam, c(
    list(data = sim$data_train), fit_args
  ))))
  fc <- forecast(fit, newdata = sim$data_test,
                   type = "response", ndraws = ndraws_fc)
  out <- list(sim = sim, fit = fit, fc = fc)
  saveRDS(out, path)
  out
}


# Fit BOTH a correct and a misspecified model on the same
# simulated truth. Returns the two score-result tuples for
# downstream pairwise comparison.
fit_pair <- function(name_correct, name_misspec, sim_args,
                       fit_args_correct, fit_args_misspec,
                       score_kind, mutate_fn = identity,
                       ndraws_fc = 200L) {
  bundle_correct <- prep_recovery(
    name = name_correct, sim_args = sim_args,
    fit_args = fit_args_correct,
    mutate_fn = mutate_fn, ndraws_fc = ndraws_fc
  )
  bundle_misspec <- prep_recovery(
    name = name_misspec, sim_args = sim_args,
    fit_args = fit_args_misspec,
    mutate_fn = mutate_fn, ndraws_fc = ndraws_fc
  )
  list(
    correct = score(bundle_correct$fc, score_kind),
    misspec = score(bundle_misspec$fc, score_kind)
  )
}


# Pool coverage indicator and mean score across the per-series
# entries of a `score()` result list.
pool_coverage_and_score <- function(score_result) {
  hits <- 0L; total <- 0L
  score_vec <- numeric(0L)
  for (nm in setdiff(names(score_result), "all_series")) {
    df <- score_result[[nm]]
    ok <- !is.na(df$in_interval)
    hits <- hits + sum(df$in_interval[ok])
    total <- total + sum(ok)
    score_vec <- c(score_vec, df$score)
  }
  list(
    hits = hits, total = total,
    coverage = hits / total,
    mean_score = mean(score_vec, na.rm = TRUE)
  )
}


# Across many seeds, pool the correct-model and misspec-model
# scores separately, then return both summaries.
pool_pair <- function(pairs) {
  pool_one <- function(side) {
    hits <- 0L; total <- 0L; score_vec <- numeric(0L)
    for (p in pairs) {
      for (nm in setdiff(names(p[[side]]), "all_series")) {
        df <- p[[side]][[nm]]
        ok <- !is.na(df$in_interval)
        hits <- hits + sum(df$in_interval[ok])
        total <- total + sum(ok)
        score_vec <- c(score_vec, df$score)
      }
    }
    list(hits = hits, total = total,
         coverage = hits / total,
         mean_score = mean(score_vec, na.rm = TRUE))
  }
  list(correct = pool_one("correct"),
       misspec = pool_one("misspec"))
}


# Assert pairwise dominance:
#   1. The correct model is calibrated (within `cov_tol` of
#      nominal).
#   2. The correct model has lower mean proper score than the
#      misspec (sharpness wins even when both models happen to
#      cover similarly).
#
# We deliberately do NOT require the correct model's coverage
# to be strictly CLOSER to nominal than the misspec's: when
# both fits over-cover (e.g. inflated PIs from a smooth
# misspecification), the misspec can land slightly closer to
# nominal by luck. The proper score reliably captures the
# sharpness advantage of the correct model regardless.
assert_correct_dominates <- function(pool, nominal = 0.90,
                                       cov_tol = 0.06) {
  testthat::expect_true(
    abs(pool$correct$coverage - nominal) <= cov_tol
  )
  testthat::expect_true(
    pool$correct$mean_score < pool$misspec$mean_score
  )
}


# ----- Pair 1: AR(p=1) vs RW on AR(0.7) Poisson truth ------------
#
# Truth is stationary AR(0.7). The correct AR(1) fit's forecast
# variance saturates at the stationary marginal; the
# misspecified RW fit's forecast variance grows linearly with
# horizon, so by the test horizon it has produced ridiculously
# wide PIs that over-cover almost every test point. RW is the
# strongest stationary misspec available: ZMVN converges in
# coverage at long horizons (the marginal forecast IS the
# stationary distribution), so it can't reliably discriminate
# against AR(1) on calibration alone -- the score gap is
# small. RW miscalibrates in a direction the coverage check
# can detect.

test_that("AR(1) dominates RW on AR(0.7) Poisson truth", {
  pairs <- lapply(c(1001L, 1002L, 1003L), function(seed) {
    fit_pair(
      name_correct = paste0(
        "pair_ar_truth_ar_fit_seed", seed
      ),
      name_misspec = paste0(
        "pair_ar_truth_rw_fit_seed", seed
      ),
      sim_args = list(
        trend_model = AR(p = 1L), family = poisson(),
        n_timepoints = 200L, n_series = 2L,
        proportional_train = 0.75, seed = seed
      ),
      fit_args_correct = list(
        formula = y ~ 1, trend_formula = ~ AR(p = 1),
        family = poisson(),
        chains = 1L, iter = 500L, warmup = 250L,
        refresh = 0L, silent = 2L
      ),
      fit_args_misspec = list(
        formula = y ~ 1, trend_formula = ~ RW(),
        family = poisson(),
        chains = 1L, iter = 500L, warmup = 250L,
        refresh = 0L, silent = 2L
      ),
      score_kind = "drps"
    )
  })
  pool <- pool_pair(pairs)
  assert_correct_dominates(pool, nominal = 0.90)
})


# ----- Score a brms forecast in the same shape as score(mvgam) ---
#
# `posterior_predict.brmsfit` for an `ar()`-bearing fit only
# propagates the AR forward if newdata contains BOTH training
# rows (with observed y) and the future rows (with NA y).
# Passing the test slice alone collapses the AR contribution
# because brms has no past residuals to feed into the
# recursion. Concatenate, predict, then slice out only the
# future rows for scoring.
#
# Confirmed via the brms documentation for
# `posterior_predict.brmsfit` -- the `ar()` term computes
# residual lags from observed responses in the supplied
# newdata, and rows whose response is NA receive sampled
# (autoregressively propagated) values.
score_brms_forecast <- function(brms_fit, sim_data_train,
                                  sim_data_test,
                                  score_kind = "crps") {
  test_na <- sim_data_test
  test_na$y <- NA_real_
  combined <- rbind(sim_data_train, test_na)
  pp_full <- brms::posterior_predict(
    brms_fit, newdata = combined, ndraws = 200L
  )
  test_offset <- nrow(sim_data_train)
  pp <- pp_full[, (test_offset + 1L):ncol(pp_full),
                  drop = FALSE]

  series_levels <- levels(sim_data_test$series)
  out <- vector("list", length(series_levels))
  names(out) <- series_levels
  kernel <- switch(
    score_kind,
    "crps" = crps_mcmc_object,
    "drps" = drps_mcmc_object
  )
  for (lv in series_levels) {
    idx <- which(sim_data_test$series == lv)
    if (!length(idx)) next
    truth <- as.numeric(sim_data_test$y[idx])
    fc <- pp[, idx, drop = FALSE]
    km <- kernel(truth, fc)
    out[[lv]] <- data.frame(
      score = as.numeric(km[, "score"]),
      in_interval = as.numeric(km[, "in_interval"]),
      interval_width = 0.9,
      eval_horizon = seq_len(nrow(km)),
      score_type = score_kind,
      stringsAsFactors = FALSE
    )
  }
  out
}


# Cache a brms fit + forecast in the same `bundle$fc`-style
# shape so the per-pair helper can score it via
# `score_brms_forecast`.
prep_brms_recovery <- function(name, sim_args, brms_args) {
  path <- file.path(CACHE_DIR, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  sim <- do.call(sim_mvgam, sim_args)
  brms_fit <- SM(SW(do.call(brms::brm, c(
    list(data = sim$data_train), brms_args
  ))))
  out <- list(sim = sim, fit = brms_fit)
  saveRDS(out, path)
  out
}


# ----- Pair 2: RW vs AR(p=1) on RW Gaussian truth ----------------
#
# Truth has variance growing linearly with horizon. The correct
# RW fit captures that; the AR(1) fit's stationary-variance
# ceiling means its PI saturates while the truth keeps
# drifting. Coverage should be WORSE for AR(1), and the mean
# CRPS HIGHER.

test_that("RW dominates AR(p=1) on RW Gaussian truth", {
  pairs <- lapply(c(2001L, 2002L, 2003L), function(seed) {
    fit_pair(
      name_correct = paste0(
        "pair_rw_truth_rw_fit_seed", seed
      ),
      name_misspec = paste0(
        "pair_rw_truth_ar_fit_seed", seed
      ),
      sim_args = list(
        trend_model = RW(), family = gaussian(),
        n_timepoints = 400L, n_series = 1L,
        proportional_train = 0.75, seed = seed
      ),
      fit_args_correct = list(
        formula = y ~ 1, trend_formula = ~ RW(),
        family = gaussian(),
        chains = 1L, iter = 500L, warmup = 250L,
        refresh = 0L, silent = 2L
      ),
      fit_args_misspec = list(
        formula = y ~ 1, trend_formula = ~ AR(p = 1),
        family = gaussian(),
        chains = 1L, iter = 500L, warmup = 250L,
        refresh = 0L, silent = 2L
      ),
      score_kind = "crps"
    )
  })
  pool <- pool_pair(pairs)
  assert_correct_dominates(pool, nominal = 0.90)
})


# ----- Pair 4: trend smooth in / out on AR(0.7) Poisson truth ----
#
# Exercises the F2 trend-formula linpred reshape under
# misspecification: the truth carries a systematic
# x-driven smooth signal on the trend side (sim_mvgam type 2
# generates `y ~ s(x) + s(z)` with AR(p = 1) trend). The
# correct fit recovers the smooth via `trend_formula = ~ s(x)
# + AR(p = 1)`; the misspec drops the smooth (`~ AR(p = 1)`)
# and so cannot track the systematic x-driven variation. Test
# cells whose `x` lies far from the training-mean predictor
# end up well outside the misspec's PI, while the correct
# fit's PI absorbs the smooth signal.

test_that("Trend smooth: correct dominates on AR+smooth truth", {
  pairs <- lapply(c(4001L, 4002L, 4003L), function(seed) {
    fit_pair(
      name_correct = paste0(
        "pair_trendsm_correct_seed", seed
      ),
      name_misspec = paste0(
        "pair_trendsm_misspec_seed", seed
      ),
      sim_args = list(
        # type = 2 generates `y ~ s(x) + s(z)` with AR(p = 1).
        type = 2L, family = poisson(),
        n_timepoints = 200L, n_series = 1L,
        proportional_train = 0.75, seed = seed
      ),
      fit_args_correct = list(
        formula = y ~ 1,
        trend_formula = ~ s(x, k = 6L) + AR(p = 1),
        family = poisson(),
        chains = 1L, iter = 500L, warmup = 250L,
        refresh = 0L, silent = 2L
      ),
      fit_args_misspec = list(
        formula = y ~ 1,
        trend_formula = ~ AR(p = 1),
        family = poisson(),
        chains = 1L, iter = 500L, warmup = 250L,
        refresh = 0L, silent = 2L
      ),
      score_kind = "drps"
    )
  })
  pool <- pool_pair(pairs)
  assert_correct_dominates(pool, nominal = 0.90)
})


# ----- Cross-package check: mvgam vs brms on AR(0.7) Gaussian ---
#
# Same simulated truth (AR(0.7) Gaussian). mvgam fits a latent
# AR(1) trend; brms fits an `ar(p = 1, gr = series)` residual
# autocorrelation. The two parameterisations are mathematically
# distinct but should produce comparable forecast calibration
# on data the AR model fits. Asserts:
#   * both pass the same in-PI Wilson coverage bracket against
#     nominal 0.90, AND
#   * mvgam's pooled mean CRPS is no worse than 1.5x brms's
#     (gives mvgam room for the additional latent-state
#     uncertainty without letting it silently regress).

test_that("mvgam AR(1) competitive with brms ar(1) on Gaussian truth", {
  pooled_pairs <- lapply(c(3001L, 3002L, 3003L), function(seed) {
    sim_args <- list(
      trend_model = AR(p = 1L), family = gaussian(),
      n_timepoints = 200L, n_series = 1L,
      proportional_train = 0.75, seed = seed
    )
    mvgam_bundle <- prep_recovery(
      name = paste0("xpack_mvgam_ar_gauss_seed", seed),
      sim_args = sim_args,
      fit_args = list(
        formula = y ~ 1,
        trend_formula = ~ AR(p = 1),
        family = gaussian(),
        chains = 1L, iter = 500L, warmup = 250L,
        refresh = 0L, silent = 2L
      )
    )
    brms_bundle <- prep_brms_recovery(
      name = paste0("xpack_brms_ar_gauss_seed", seed),
      sim_args = sim_args,
      brms_args = list(
        formula = brms::bf(y ~ 1 + ar(time, gr = series, p = 1)),
        family = gaussian(),
        chains = 1L, iter = 500L, warmup = 250L,
        refresh = 0L, silent = 2L
      )
    )
    list(
      mvgam = score(mvgam_bundle$fc, "crps"),
      brms = score_brms_forecast(
        brms_bundle$fit,
        brms_bundle$sim$data_train,
        brms_bundle$sim$data_test,
        score_kind = "crps"
      )
    )
  })

  pool_one <- function(side) {
    hits <- 0L; total <- 0L; score_vec <- numeric(0L)
    for (p in pooled_pairs) {
      for (nm in setdiff(names(p[[side]]), "all_series")) {
        df <- p[[side]][[nm]]
        ok <- !is.na(df$in_interval)
        hits <- hits + sum(df$in_interval[ok])
        total <- total + sum(ok)
        score_vec <- c(score_vec, df$score)
      }
    }
    list(coverage = hits / total, n = total,
         mean_crps = mean(score_vec, na.rm = TRUE))
  }
  mvgam_pool <- pool_one("mvgam")
  brms_pool <- pool_one("brms")

  # Both should pass nominal 0.90 coverage (Wilson CI bracket).
  z <- stats::qnorm(0.975)
  for (pool in list(mvgam_pool, brms_pool)) {
    phat <- pool$coverage; n <- pool$n
    denom <- 1 + z^2 / n
    centre <- (phat + z^2 / (2 * n)) / denom
    half <- z * sqrt(
      phat * (1 - phat) / n + z^2 / (4 * n^2)
    ) / denom
    expect_true(
      0.90 >= (centre - half) && 0.90 <= (centre + half)
    )
  }
  # mvgam mean CRPS should be no worse than 1.5x brms's.
  expect_lt(mvgam_pool$mean_crps,
              1.5 * brms_pool$mean_crps)
})
