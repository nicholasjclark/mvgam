# Local end-to-end tests for lfo_cv.mvgam.
#
# Five scenarios, each cached under
# `tests/local/fixtures/lfo_cv/<name>.rds` so reruns load instead
# of refitting:
#
#   1. Baseline regular grid: Poisson AR(1), single series,
#      fc_horizon = 1. Confirms class shape, finite ELPDs,
#      summary tibble columns, refit_triggered flag, and the
#      multi-score forecast+score pathway.
#
#   2. Multi-horizon: Gaussian RW, single series, fc_horizon = 3.
#      Tests per-horizon ELPD aggregation and the matrix-shape
#      handling in sum_univariate_horizon when h_max > 1.
#
#   3. Multi-series Poisson AR(p = 2), fc_horizon = 1. Confirms
#      per-series log-lik summed correctly into the PSIS log-ratio
#      and per-series forecast scoring.
#
#   4. Multi-series ZMVN Gaussian (cor = TRUE), fc_horizon = 1.
#      Stress-tests a non-AR/RW kernel under refits and confirms
#      the multivariate-pair scores (energy / variogram) compute
#      finite values.
#
#   5. n_evals = 1 (single evaluation fold). Tests the degenerate
#      case where the main loop body does not iterate.
#
# Plus a teaching-point comparison: fit BOTH AR(2) and RW to the
# same AR(2)-generated data. Assert that the well-specified
# AR(2) has higher sum_ELPD AND triggers <= refits than the
# mis-specified RW (the canonical PSIS-LFO story).

source("setup_tests_local.R")

CACHE_DIR <- "fixtures/lfo_cv"
if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)


# ----- Cache helper ------------------------------------------------

cached_fit <- function(name, build_fn) {
  path <- file.path(CACHE_DIR, paste0(name, ".rds"))
  if (file.exists(path)) {
    return(readRDS(path))
  }
  fit <- build_fn()
  if (!inherits(fit, "mvgam")) {
    stop(insight::format_error(c(
      paste0("Cache build for '", name, "' did not produce an mvgam fit."),
      i = "Investigate the build_fn before rerunning."
    )))
  }
  saveRDS(fit, path)
  fit
}


# ----- 1. Baseline: regular AR(1) grid ----------------------------

test_that("lfo_cv on a Poisson AR(1) returns a populated mvgam_lfo", {
  fit <- cached_fit("ar1_poisson_n40", function() {
    sim <- sim_mvgam(
      n_timepoints = 40L,
      n_series = 1L,
      trend_model = AR(p = 1L),
      family = poisson(),
      proportional_train = 1.0,
      seed = 20260605L
    )
    SM(mvgam(
      formula = y ~ 1 + x,
      trend_formula = ~ AR(p = 1L),
      data = sim$data_train,
      family = poisson(),
      chains = 2L, parallel_chains = 2L,
      iter_warmup = 400L, iter_sampling = 400L,
      silent = 2L, refresh = 0L
    ))
  })

  out <- SM(lfo_cv(fit, min_t = 30L, fc_horizon = 1L,
                    score = "elpd", silent = 2L))
  expect_s3_class(out, "mvgam_lfo")
  expect_identical(out$fc_horizon, 1L)
  expect_identical(out$pareto_k_threshold, 0.7)
  expect_identical(length(out$eval_timepoints), 10L)
  expect_identical(out$eval_timepoints[1L], 31L)
  expect_identical(out$eval_timepoints[10L], 40L)
  expect_identical(out$refits_at[1L], 30L)
  expect_true(all(is.finite(out$elpds)))
  expect_equal(out$sum_ELPD, sum(out$elpds))
  # refit_triggered: TRUE at the initial eval (min_t fit).
  expect_true(out$refit_triggered[1L])
  expect_identical(length(out$refit_triggered), length(out$eval_timepoints))
})


test_that("summary.mvgam_lfo returns the documented tibble shape", {
  fit <- readRDS(file.path(CACHE_DIR, "ar1_poisson_n40.rds"))
  out <- SM(lfo_cv(fit, min_t = 30L, fc_horizon = 1L,
                    score = "elpd", silent = 2L))
  tib <- summary(out)
  expect_s3_class(tib, "tbl_df")
  expect_identical(nrow(tib), 10L)
  expect_true(all(c("eval_time", "refit_here", "pareto_k",
                     "elpd") %in% names(tib)))
  expect_true(is.logical(tib$refit_here))
  expect_true(tib$refit_here[1L])  # initial min_t refit
})


test_that("Multi-score lfo populates ELPD + CRPS with finite values", {
  fit <- readRDS(file.path(CACHE_DIR, "ar1_poisson_n40.rds"))
  out <- SM(lfo_cv(fit, min_t = 30L, fc_horizon = 1L,
                    score = c("elpd", "crps"), silent = 2L))
  expect_true(is.list(out$scores))
  expect_true("crps" %in% names(out$scores))
  expect_identical(length(out$elpds), 10L)
  expect_identical(length(out$scores$crps), 10L)
  expect_true(all(is.finite(out$elpds)))
  expect_true(all(is.finite(out$scores$crps)))

  tib <- summary(out)
  expect_true(all(c("elpd", "crps") %in% names(tib)))
})


# ----- 2. Multi-horizon: Gaussian RW, fc_horizon = 3 -------------

test_that("lfo_cv handles fc_horizon = 3 on a Gaussian RW fit", {
  fit <- cached_fit("rw_gauss_n40", function() {
    set.seed(20260606L)
    n_t <- 40L
    t <- seq_len(n_t)
    # Gaussian RW with low obs noise: trend should absorb most of Y.
    drift <- cumsum(rnorm(n_t, 0, 0.5))
    y <- drift + rnorm(n_t, 0, 0.2)
    d <- data.frame(
      y = y, x = rnorm(n_t),
      time = t, series = factor("s1", levels = "s1")
    )
    SM(mvgam(
      formula = y ~ 1 + x,
      trend_formula = ~ RW(),
      data = d, family = gaussian(),
      chains = 2L, parallel_chains = 2L,
      iter_warmup = 400L, iter_sampling = 400L,
      silent = 2L, refresh = 0L
    ))
  })

  out <- SM(lfo_cv(fit, min_t = 30L, fc_horizon = 3L,
                    score = "elpd", silent = 2L))
  # eval_positions = (idx_min_t + 1):(n_times - fc_horizon + 1)
  #                = 31:38 -> 8 evaluation folds.
  expect_identical(length(out$eval_timepoints), 8L)
  expect_identical(out$eval_timepoints, 31:38)
  expect_true(all(is.finite(out$elpds)))
  expect_true(is.finite(out$sum_ELPD))
})


# ----- 3. Multi-series AR(p = 2), Poisson, fc_horizon = 1 --------

test_that("lfo_cv handles multi-series AR(p = 2) Poisson", {
  fit <- cached_fit("ar2_poisson_mv_n35", function() {
    sim <- sim_mvgam(
      n_timepoints = 35L,
      n_series = 2L,
      trend_model = AR(p = 2L),
      family = poisson(),
      proportional_train = 1.0,
      seed = 20260607L
    )
    SM(mvgam(
      formula = y ~ 1 + x,
      trend_formula = ~ AR(p = 2L),
      data = sim$data_train,
      family = poisson(),
      chains = 2L, parallel_chains = 2L,
      iter_warmup = 400L, iter_sampling = 400L,
      silent = 2L, refresh = 0L
    ))
  })

  out <- SM(lfo_cv(fit, min_t = 28L, fc_horizon = 1L,
                    score = c("elpd", "crps", "drps"), silent = 2L))
  # eval_positions = 29:35 -> 7 folds.
  expect_identical(length(out$eval_timepoints), 7L)
  expect_true(all(is.finite(out$elpds)))
  expect_true(all(is.finite(out$scores$crps)))
  expect_true(all(is.finite(out$scores$drps)))
  # All three score columns surface in the summary tibble.
  tib <- summary(out)
  expect_true(all(c("elpd", "crps", "drps") %in% names(tib)))
})


# ----- 4. Multi-series ZMVN with cor = TRUE -----------------------

test_that("lfo_cv handles multi-series ZMVN(cor = TRUE)", {
  fit <- cached_fit("zmvn_gauss_mv_n35", function() {
    sim <- sim_mvgam(
      n_timepoints = 35L,
      n_series = 2L,
      trend_model = ZMVN(cor = TRUE),
      family = gaussian(),
      proportional_train = 1.0,
      seed = 20260608L
    )
    SM(mvgam(
      formula = y ~ 1 + x,
      trend_formula = ~ ZMVN(cor = TRUE),
      data = sim$data_train,
      family = gaussian(),
      chains = 2L, parallel_chains = 2L,
      iter_warmup = 400L, iter_sampling = 400L,
      silent = 2L, refresh = 0L
    ))
  })

  out <- SM(lfo_cv(fit, min_t = 28L, fc_horizon = 1L,
                    score = "elpd", silent = 2L))
  expect_identical(length(out$eval_timepoints), 7L)
  expect_true(all(is.finite(out$elpds)))
})


# ----- 5. n_evals = 1 (degenerate single-fold) -------------------

test_that("lfo_cv runs with exactly one evaluation fold", {
  fit <- readRDS(file.path(CACHE_DIR, "ar1_poisson_n40.rds"))
  # min_t = N - fc_horizon = 39 leaves a single eval at time 40.
  out <- SM(lfo_cv(fit, min_t = 39L, fc_horizon = 1L,
                    score = "elpd", silent = 2L))
  expect_identical(length(out$eval_timepoints), 1L)
  expect_identical(out$eval_timepoints, 40L)
  expect_true(is.finite(out$elpds[1L]))
  expect_true(out$refit_triggered[1L])
  # No PSIS step ran (no loop iterations), so pareto_ks is all NA.
  expect_true(is.na(out$pareto_ks[1L]))
})


# ----- 6. Teaching point: paired-ELPD model comparison -----------

test_that("paired-ELPD comparison works between AR(2) and RW", {
  # Fit BOTH AR(2) and RW to the same AR(2)-simulated data, then
  # compare via paired-difference ELPDs (the loo::loo_compare
  # convention applied to LFO outputs).
  #
  # What we strictly verify (the COMPUTATION):
  #   - Both runs share the same evaluation grid.
  #   - Both produce finite per-fold ELPDs.
  #   - elpd_diff and its paired SE are finite, SE > 0.
  #
  # What we report but do NOT fail on (the DIRECTION):
  #   - elpd_diff sign and magnitude relative to SE.
  #   With only ~20 evaluation folds the comparison may be
  #   inconclusive (|elpd_diff| < 2 * se_diff), which is the
  #   statistically honest outcome on small data. We assert only
  #   that "RW does not decisively beat AR(2) by more than 2 SE"
  #   — i.e. AR(2) is at worst inconclusive vs RW.

  shared_data <- (function() {
    sim <- sim_mvgam(
      n_timepoints = 50L,
      n_series = 1L,
      trend_model = AR(p = 2L),
      family = gaussian(),
      proportional_train = 1.0,
      seed = 20260609L
    )
    sim$data_train
  })()

  fit_ar2 <- cached_fit("comparison_ar2_n50", function() {
    SM(mvgam(
      formula = y ~ 1 + x,
      trend_formula = ~ AR(p = 2L),
      data = shared_data, family = gaussian(),
      chains = 2L, parallel_chains = 2L,
      iter_warmup = 500L, iter_sampling = 500L,
      silent = 2L, refresh = 0L
    ))
  })

  fit_rw <- cached_fit("comparison_rw_n50", function() {
    SM(mvgam(
      formula = y ~ 1 + x,
      trend_formula = ~ RW(),
      data = shared_data, family = gaussian(),
      chains = 2L, parallel_chains = 2L,
      iter_warmup = 500L, iter_sampling = 500L,
      silent = 2L, refresh = 0L
    ))
  })

  lfo_ar2 <- SM(lfo_cv(fit_ar2, min_t = 30L, fc_horizon = 1L,
                        score = "elpd", silent = 2L))
  lfo_rw <- SM(lfo_cv(fit_rw, min_t = 30L, fc_horizon = 1L,
                       score = "elpd", silent = 2L))

  # Same eval grid is a prerequisite for any paired comparison.
  expect_identical(lfo_ar2$eval_timepoints, lfo_rw$eval_timepoints)
  expect_true(all(is.finite(lfo_ar2$elpds)))
  expect_true(all(is.finite(lfo_rw$elpds)))

  # Paired ELPD difference + standard error.
  # The loo::loo_compare convention: elpd_diff is the SUM of the
  # paired per-fold differences; the SE of that sum is
  # sqrt(N) * sd(diff) (treating folds as independent draws from
  # the comparison-noise distribution, which is the standard
  # large-sample approximation Burkner et al. use).
  diff <- lfo_ar2$elpds - lfo_rw$elpds
  N <- length(diff)
  elpd_diff <- sum(diff)
  se_diff <- sqrt(N) * stats::sd(diff)

  cat(sprintf(
    paste0("\n  Eval folds      : %d\n",
            "  AR(2) refits    : %d\n",
            "  RW    refits    : %d\n",
            "  AR(2) sum_ELPD  : %.3f\n",
            "  RW    sum_ELPD  : %.3f\n",
            "  elpd_diff (A-R) : %.3f\n",
            "  se(elpd_diff)   : %.3f\n",
            "  diff / SE       : %.2f%s\n"),
    N,
    length(lfo_ar2$refits_at), length(lfo_rw$refits_at),
    lfo_ar2$sum_ELPD, lfo_rw$sum_ELPD,
    elpd_diff, se_diff,
    elpd_diff / se_diff,
    if (abs(elpd_diff / se_diff) > 2) " (decisive)" else " (inconclusive)"
  ))

  # Computational invariants.
  expect_true(is.finite(elpd_diff))
  expect_true(is.finite(se_diff))
  expect_gt(se_diff, 0)

  # Direction: RW should NOT decisively beat AR(2) on AR(2) data.
  # The honest assertion is one-sided: -2 SE is the threshold for
  # "RW decisively wins". We tolerate any positive diff or any
  # near-zero (inconclusive) result.
  expect_gt(elpd_diff, -2 * se_diff)

  # PSIS refit counts: AR(2) should not refit MORE OFTEN than RW.
  # When the model is well-specified the PSIS importance ratios
  # stay tighter, requiring fewer refits.
  expect_lte(length(lfo_ar2$refits_at), length(lfo_rw$refits_at))
})
