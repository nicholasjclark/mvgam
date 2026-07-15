# Unit tests for `lfo_cv.mvgam()` in `R/lfo_cv.mvgam.R`. The
# method orchestrates a sequence of `update.mvgam` refits and
# `log_lik.mvgam` / `forecast.mvgam` / `score.mvgam_forecast`
# calls; the algorithmic primitives are exercised by their own
# test files. These tests stub the primitives via
# `local_mocked_bindings()` and assert:
#
#   * Argument validation (score subset, min_t guard, fc_horizon).
#   * Return-shape contract (mvgam_lfo class with the documented
#     slot names; per-score arrays sized to n_evals).
#   * The deprecated `data` arg forwards to `newdata` with a
#     warning.
#   * `summary.mvgam_lfo()` returns a tibble with one row per
#     evaluation timepoint and the expected columns.
#   * `print.mvgam_lfo()` runs without error.


# Minimal mock mvgam fit; enough surface for lfo_cv to traverse
# (data frame access, trend_metadata, family).
make_lfo_mock <- function(n_time = 40L, n_series = 1L) {
  d <- data.frame(
    time = rep(seq_len(n_time), n_series),
    series = factor(rep(paste0("s", seq_len(n_series)),
                         each = n_time),
                     levels = paste0("s", seq_len(n_series))),
    y = stats::rpois(n_time * n_series, lambda = 3)
  )
  fit <- list(
    fit = structure(list(), class = "fake_stanfit"),
    data = d,
    obs_data = d,
    formula = stats::as.formula("y ~ 1"),
    family = poisson(),
    trend_metadata = list(
      trend_type = "AR",
      variables = list(time_var = "time", series_var = "series"),
      dimensions = list(n_series = n_series)
    ),
    backend = "rstan"
  )
  class(fit) <- "mvgam"
  fit
}


# ----- Validation arms --------------------------------------------

test_that("score arg validation rejects unknown rules", {
  fit <- make_lfo_mock()
  expect_error(
    lfo_cv(fit, min_t = 30L, score = "nonsense"),
    "Must be a subset"
  )
})


test_that("fc_horizon must be a positive integer", {
  fit <- make_lfo_mock()
  expect_error(
    lfo_cv(fit, min_t = 30L, fc_horizon = 0L),
    "fc_horizon"
  )
})


test_that("pareto_k_threshold must be in [0, 1]", {
  fit <- make_lfo_mock()
  expect_error(
    lfo_cv(fit, min_t = 30L, pareto_k_threshold = 1.5),
    "pareto_k_threshold"
  )
})


test_that("min_t leaving no eval room errors informatively", {
  fit <- make_lfo_mock(n_time = 40L)
  expect_error(
    lfo_cv(fit, min_t = 40L, fc_horizon = 1L),
    "no room for evaluation"
  )
})


test_that("deprecated data arg forwards to newdata with warning", {
  fit <- make_lfo_mock(n_time = 40L)
  newdat <- fit$data
  # Stub the heavy machinery so we can reach the deprecation
  # branch without actually fitting.
  testthat::local_mocked_bindings(
    update.mvgam = function(object, ...) object,
    log_lik = function(object, ...) {
      matrix(stats::rnorm(5L * nrow(newdat)),
             nrow = 5L, ncol = nrow(newdat))
    },
    forecast = function(object, ...) {
      list(forecasts = setNames(
        list(matrix(0, nrow = 5L, ncol = 1L)),
        levels(newdat$series)
      ))
    },
    score = function(object, score, ...) {
      list(all_series = data.frame(score = 0))
    }
  )
  expect_warning(
    lfo_cv(fit, min_t = 30L, fc_horizon = 1L, data = newdat,
            silent = 2L),
    "data.*deprecated"
  )
})


# ----- Return-shape contract --------------------------------------

# Helper: stub the primitives so a small mock fit can roll through
# lfo_cv end-to-end without any real Stan calls.
stub_lfo_primitives <- function(fit, ndraws = 5L,
                                 pareto_k = 0.2,
                                 loglik_fill = -1.0) {
  newdat <- fit$data
  n_obs <- nrow(newdat)
  testthat::local_mocked_bindings(
    update.mvgam = function(object, ...) object,
    log_lik = function(object, ...) {
      matrix(loglik_fill, nrow = ndraws, ncol = n_obs)
    },
    forecast = function(object, ..., newdata = NULL) {
      n_h <- if (is.null(newdata)) 1L else
        length(unique(newdata[[
          object$trend_metadata$variables$time_var
        ]]))
      n_series <- length(levels(object$data[[
        object$trend_metadata$variables$series_var
      ]]))
      fmats <- replicate(n_series,
                          matrix(0, nrow = ndraws, ncol = n_h),
                          simplify = FALSE)
      names(fmats) <- levels(object$data[[
        object$trend_metadata$variables$series_var
      ]])
      list(forecasts = fmats)
    },
    score = function(object, score, ...) {
      list(all_series = data.frame(score = 1.0))
    },
    .package = c("mvgam", "mvgam", "mvgam", "mvgam"),
    .env = parent.frame()
  )
  invisible(NULL)
}


test_that("Return object has all documented mvgam_lfo slots", {
  fit <- make_lfo_mock(n_time = 35L)
  ndraws <- 5L
  newdat <- fit$data
  testthat::local_mocked_bindings(
    update.mvgam = function(object, ...) object,
    log_lik = function(object, ...) {
      matrix(-1.0, nrow = ndraws, ncol = nrow(newdat))
    }
  )
  out <- lfo_cv(fit, min_t = 30L, fc_horizon = 1L,
                 score = "elpd", silent = 2L)
  expect_s3_class(out, "mvgam_lfo")
  required <- c("elpds", "sum_ELPD", "scores", "pareto_ks",
                "eval_timepoints", "refits_at",
                "pareto_k_threshold",
                "pareto_k_threshold_used", "fc_horizon")
  expect_true(all(required %in% names(out)))
  expect_identical(out$fc_horizon, 1L)
  # Default fit uses the adaptive threshold: the user-supplied
  # slot stays NULL, and `pareto_k_threshold_used` carries the
  # numeric value that the refit gate actually applied.
  expect_null(out$pareto_k_threshold)
  expect_type(out$pareto_k_threshold_used, "double")
  expect_lte(out$pareto_k_threshold_used, 0.7)
  expect_true(min_t_in_refits <- 30L %in% out$refits_at)
  # eval window is (min_t + 1):(N - fc_horizon + 1) = 31:35,
  # so 5 evaluations on a 35-step series with fc_horizon = 1.
  expect_identical(length(out$eval_timepoints), 5L)
  expect_identical(length(out$elpds), 5L)
  expect_null(out$scores)
})


test_that("Non-ELPD score adds named entries to scores list", {
  fit <- make_lfo_mock(n_time = 35L)
  ndraws <- 5L
  newdat <- fit$data
  testthat::local_mocked_bindings(
    update.mvgam = function(object, ...) object,
    log_lik = function(object, ...) {
      matrix(-1.0, nrow = ndraws, ncol = nrow(newdat))
    },
    forecast = function(object, ..., newdata = NULL) {
      n_h <- if (is.null(newdata)) 1L else
        length(unique(newdata$time))
      list(forecasts = list(s1 = matrix(0, ndraws, n_h)))
    },
    score = function(object, score, ...) {
      list(all_series = data.frame(score = 0.5))
    }
  )
  out <- lfo_cv(fit, min_t = 30L, fc_horizon = 1L,
                 score = c("elpd", "crps"), silent = 2L)
  expect_true(is.list(out$scores))
  expect_true("crps" %in% names(out$scores))
  expect_identical(length(out$scores$crps), 5L)
  # All forecast() calls stubbed to score = 0.5 per window.
  expect_true(all(out$scores$crps == 0.5))
})


# ----- summary / print methods -----------------------------------

test_that("summary.mvgam_lfo returns a tibble with one row per fold", {
  fit <- make_lfo_mock(n_time = 35L)
  ndraws <- 5L
  newdat <- fit$data
  testthat::local_mocked_bindings(
    update.mvgam = function(object, ...) object,
    log_lik = function(object, ...) {
      matrix(-1.0, nrow = ndraws, ncol = nrow(newdat))
    },
    forecast = function(object, ..., newdata = NULL) {
      n_h <- if (is.null(newdata)) 1L else
        length(unique(newdata$time))
      list(forecasts = list(s1 = matrix(0, ndraws, n_h)))
    },
    score = function(object, score, ...) {
      list(all_series = data.frame(score = 0.5))
    }
  )
  out <- lfo_cv(fit, min_t = 30L, fc_horizon = 1L,
                 score = c("elpd", "crps"), silent = 2L)
  tib <- summary(out)
  expect_s3_class(tib, "tbl_df")
  expect_identical(nrow(tib), length(out$eval_timepoints))
  expect_true(all(c("eval_time", "refit_here", "pareto_k",
                     "elpd", "crps") %in% names(tib)))
  expect_true(is.logical(tib$refit_here))
  expect_true(tib$refit_here[1L])  # min_t triggered the initial refit
})


test_that("print.mvgam_lfo runs without error and returns invisibly", {
  fit <- make_lfo_mock(n_time = 35L)
  ndraws <- 5L
  newdat <- fit$data
  testthat::local_mocked_bindings(
    update.mvgam = function(object, ...) object,
    log_lik = function(object, ...) {
      matrix(-1.0, nrow = ndraws, ncol = nrow(newdat))
    }
  )
  out <- lfo_cv(fit, min_t = 30L, fc_horizon = 1L,
                 score = "elpd", silent = 2L)
  expect_invisible(print(out))
  expect_output(print(out), "Approximate leave-future-out")
  expect_output(print(out), "ELPD")
})


# ----- Multi-series ----------------------------------------------

test_that("lfo_cv runs on a 2-series fit", {
  fit <- make_lfo_mock(n_time = 35L, n_series = 2L)
  ndraws <- 5L
  newdat <- fit$data
  testthat::local_mocked_bindings(
    update.mvgam = function(object, ...) object,
    log_lik = function(object, ...) {
      matrix(-1.0, nrow = ndraws, ncol = nrow(newdat))
    }
  )
  out <- lfo_cv(fit, min_t = 30L, fc_horizon = 1L,
                 score = "elpd", silent = 2L)
  expect_s3_class(out, "mvgam_lfo")
  expect_identical(length(out$eval_timepoints), 5L)
  expect_identical(length(out$elpds), 5L)
})


# ----- Index-based design: arbitrary time starts ------------------

# Build a mock fit whose time column is shifted by `offset`, so
# times run (offset + 1):(offset + n_time). Exercises that lfo_cv
# arithmetic is position-based, not value-based.
make_lfo_mock_shifted <- function(n_time = 35L, offset = 2009L) {
  d <- data.frame(
    time = seq.int(offset + 1L, offset + n_time),
    series = factor("s1", levels = "s1"),
    y = stats::rpois(n_time, lambda = 3)
  )
  fit <- list(
    fit = structure(list(), class = "fake_stanfit"),
    data = d, obs_data = d,
    formula = stats::as.formula("y ~ 1"),
    family = poisson(),
    trend_metadata = list(
      trend_type = "AR",
      variables = list(time_var = "time", series_var = "series"),
      dimensions = list(n_series = 1L)
    ),
    backend = "rstan"
  )
  class(fit) <- "mvgam"
  fit
}


test_that("min_t works as a time VALUE for non-1-indexed times", {
  # Times run 2010..2044. With min_t = 2039, expect evals at
  # 2040..2044 (5 points), refits_at = 2039.
  fit <- make_lfo_mock_shifted(n_time = 35L, offset = 2009L)
  ndraws <- 5L
  newdat <- fit$data
  testthat::local_mocked_bindings(
    update.mvgam = function(object, ...) object,
    log_lik = function(object, ...) {
      matrix(-1.0, nrow = ndraws, ncol = nrow(newdat))
    }
  )
  out <- lfo_cv(fit, min_t = 2039L, fc_horizon = 1L,
                 score = "elpd", silent = 2L)
  expect_identical(length(out$eval_timepoints), 5L)
  expect_identical(out$eval_timepoints, 2040:2044)
  expect_true(2039L %in% out$refits_at)
})


test_that("default min_t picks the 30th observed time for n=35", {
  fit <- make_lfo_mock_shifted(n_time = 35L, offset = 2009L)
  ndraws <- 5L
  newdat <- fit$data
  testthat::local_mocked_bindings(
    update.mvgam = function(object, ...) object,
    log_lik = function(object, ...) {
      matrix(-1.0, nrow = ndraws, ncol = nrow(newdat))
    }
  )
  out <- lfo_cv(fit, fc_horizon = 1L,
                 score = "elpd", silent = 2L)
  # Default base_idx = 30, capped by n_times - 10 - fc_horizon =
  # 35 - 11 = 24. So base_idx = 24, min_t = all_unique_times[24]
  # = 2009 + 24 = 2033.
  expect_true(2033L %in% out$refits_at)
  expect_identical(out$eval_timepoints[1L], 2034L)
})


# ----- Irregular (CAR-style) grid --------------------------------

test_that("lfo_cv handles irregular CAR-style time grids", {
  # Observed times have gaps (e.g., monthly samples missing some
  # months). 12 observed times in a 20-month span.
  obs_times <- c(1L, 2L, 4L, 5L, 7L, 9L, 10L, 13L, 15L,
                  17L, 19L, 20L)
  d <- data.frame(
    time = obs_times,
    series = factor("s1", levels = "s1"),
    y = stats::rpois(length(obs_times), lambda = 3)
  )
  fit <- list(
    fit = structure(list(), class = "fake_stanfit"),
    data = d, obs_data = d,
    formula = stats::as.formula("y ~ 1"),
    family = poisson(),
    trend_metadata = list(
      trend_type = "CAR",
      variables = list(time_var = "time", series_var = "series"),
      dimensions = list(n_series = 1L)
    ),
    backend = "rstan"
  )
  class(fit) <- "mvgam"
  ndraws <- 5L
  testthat::local_mocked_bindings(
    update.mvgam = function(object, ...) object,
    log_lik = function(object, ...) {
      matrix(-1.0, nrow = ndraws, ncol = nrow(d))
    }
  )
  # min_t = 9 is the 6th observed time. Expect 6 evaluations at
  # the next 6 observed times: 10, 13, 15, 17, 19, 20.
  out <- lfo_cv(fit, min_t = 9L, fc_horizon = 1L,
                 score = "elpd", silent = 2L)
  expect_identical(out$eval_timepoints,
                    c(10L, 13L, 15L, 17L, 19L, 20L))
  expect_identical(length(out$elpds), 6L)
})


# ----- New error conditions --------------------------------------

test_that("Mismatched per-series time grids error", {
  d <- data.frame(
    time = c(1:30, 5:34),  # series s1: 1..30, s2: 5..34
    series = factor(rep(c("s1", "s2"), each = 30L)),
    y = stats::rpois(60L, lambda = 3)
  )
  fit <- list(
    fit = structure(list(), class = "fake_stanfit"),
    data = d, obs_data = d,
    formula = stats::as.formula("y ~ 1"),
    family = poisson(),
    trend_metadata = list(
      trend_type = "AR",
      variables = list(time_var = "time", series_var = "series"),
      dimensions = list(n_series = 2L)
    ),
    backend = "rstan"
  )
  class(fit) <- "mvgam"
  expect_error(
    lfo_cv(fit, min_t = 20L, score = "elpd", silent = 2L),
    "share the same time grid"
  )
})


test_that("fc_horizon >= n_times errors with focused message", {
  fit <- make_lfo_mock(n_time = 10L)
  expect_error(
    lfo_cv(fit, min_t = 5L, fc_horizon = 12L,
            score = "elpd", silent = 2L),
    "exceeds the available time span"
  )
})


test_that("min_t not in observed times errors informatively", {
  fit <- make_lfo_mock_shifted(n_time = 35L, offset = 2009L)
  expect_error(
    lfo_cv(fit, min_t = 25L, fc_horizon = 1L,
            score = "elpd", silent = 2L),
    "is not an observed time"
  )
})


test_that("min_t too late errors with the largest valid value", {
  fit <- make_lfo_mock(n_time = 35L)
  expect_error(
    lfo_cv(fit, min_t = 35L, fc_horizon = 1L,
            score = "elpd", silent = 2L),
    "leaves no room for evaluation"
  )
})


# ---- loo_compare.mvgam_lfo ---------------------------------------

mk_mvgam_lfo <- function(elpds, eval_timepoints = NULL,
                          fc_horizon = 1L, pareto_ks = NULL,
                          refit_triggered = NULL) {
  n <- length(elpds)
  if (is.null(eval_timepoints)) eval_timepoints <- seq_len(n) + 30L
  if (is.null(pareto_ks)) pareto_ks <- rep(0.3, n)
  if (is.null(refit_triggered)) refit_triggered <- rep(FALSE, n)
  structure(
    list(
      elpds = elpds,
      eval_timepoints = eval_timepoints,
      fc_horizon = fc_horizon,
      pareto_ks = pareto_ks,
      refit_triggered = refit_triggered,
      refits_at = integer(0),
      pareto_k_threshold = 0.7
    ),
    class = "mvgam_lfo"
  )
}


test_that("loo_compare.mvgam_lfo orders by elpd_diff descending", {
  m1 <- mk_mvgam_lfo(c(-2, -2.5, -3, -2.2, -2.7))
  # Non-uniform offset so sd(diff) > 0 and se_diff is meaningful.
  m2 <- mk_mvgam_lfo(c(-3, -3.7, -3.5, -3.4, -3.1))
  cmp <- loo_compare(m1, m2)
  expect_s3_class(cmp, "compare.loo")
  expect_equal(nrow(cmp), 2L)
  expect_equal(cmp$elpd_diff[1L], 0)
  expect_lt(cmp$elpd_diff[2L], 0)
  # se_diff[best] is 0 by construction; se_diff[worst] > 0 because
  # the per-fold differences vary across folds.
  expect_equal(cmp$se_diff[1L], 0)
  expect_gt(cmp$se_diff[2L], 0)
})


test_that("loo_compare.mvgam_lfo errors when eval_timepoints differ", {
  m1 <- mk_mvgam_lfo(c(-1, -2, -3), eval_timepoints = c(10, 11, 12))
  m2 <- mk_mvgam_lfo(c(-1, -2, -3), eval_timepoints = c(20, 21, 22))
  expect_error(loo_compare(m1, m2), "eval_timepoints")
})


test_that("loo_compare.mvgam_lfo errors when fc_horizon differs", {
  m1 <- mk_mvgam_lfo(c(-1, -2, -3), fc_horizon = 1L)
  m2 <- mk_mvgam_lfo(c(-1, -2, -3), fc_horizon = 3L)
  expect_error(loo_compare(m1, m2), "fc_horizon")
})


test_that("loo_compare.mvgam_lfo errors when ELPDs are missing", {
  m1 <- mk_mvgam_lfo(c(-1, -2, -3))
  m2 <- mk_mvgam_lfo(c(-1, -2, -3))
  m2$elpds <- NULL
  expect_error(loo_compare(m1, m2), "no ELPDs")
})


test_that("loo_compare.mvgam_lfo SE matches paired-diff convention", {
  # Construct two models where per-fold diff is known exactly:
  # m2$elpds = m1$elpds - c(1, 1, 1) so diff = (1,1,1), sd = 0
  # se_diff = sqrt(3) * 0 = 0; elpd_diff = -3
  m1 <- mk_mvgam_lfo(c(-1, -2, -3))
  m2 <- mk_mvgam_lfo(c(-2, -3, -4))
  cmp <- loo_compare(m1, m2)
  expect_equal(cmp$elpd_diff[2L], -3, tolerance = 1e-10)
  expect_equal(cmp$se_diff[2L], 0, tolerance = 1e-10)
})


test_that("loo_compare.mvgam_lfo emits Sivula diagnostic columns", {
  m1 <- mk_mvgam_lfo(c(-1, -2, -3))
  m2 <- mk_mvgam_lfo(c(-2, -3, -4))
  cmp <- loo_compare(m1, m2)
  expect_true(all(c("p_worse", "diag_diff", "diag_elpd") %in%
                    colnames(cmp)))
  # Reference row is NA-guarded on p_worse because se_diff = 0.
  expect_true(is.na(cmp$p_worse[1L]))
  # Candidate row has finite elpd_diff = -3 and se_diff = 0, so
  # p_worse is NA there too (the guard triggers whenever a se_diff
  # entry is zero, which holds for both rows in this exact-diff
  # scenario).
  expect_true(is.na(cmp$p_worse[2L]))
  # n_folds = 3 -> both rows carry N < 100 in diag_elpd.
  expect_true(all(grepl("N < 100", cmp$diag_elpd)))
})


test_that("mvgam_loo_compare_diagnostics computes p_worse correctly", {
  # candidate model 3 units worse with SE 1: p_worse = P(Z > 3)
  d <- mvgam_ps_khat_threshold  # ensures the file is loaded
  out <- mvgam:::mvgam_loo_compare_diagnostics(
    elpd_diff = c(0, -3),
    se_diff = c(0, 1),
    n_pointwise = c(200L, 200L),
    pareto_k_list = list(c(0.1, 0.2), c(0.1, 0.2))
  )
  expect_true(is.na(out$p_worse[1L]))
  expect_equal(out$p_worse[2L], pnorm(0, mean = -3, sd = 1),
               tolerance = 1e-12)
  expect_identical(out$diag_diff[1L], "")
  expect_identical(out$diag_diff[2L], "|elpd_diff| < 4")
  expect_identical(out$diag_elpd, c("", ""))
})


test_that("mvgam_loo_compare_diagnostics threshold overlays", {
  # Small n triggers diag_elpd = "N < 100"; high pareto_k on
  # the second model overlays "; k_psis > 0.7".
  out <- mvgam:::mvgam_loo_compare_diagnostics(
    elpd_diff = c(0, -5),
    se_diff = c(0, 2),
    n_pointwise = c(50L, 50L),
    pareto_k_list = list(c(0.1, 0.3), c(0.8, 0.4))
  )
  expect_identical(out$diag_elpd[1L], "N < 100")
  expect_identical(out$diag_elpd[2L], "N < 100; k_psis > 0.7")
  # |elpd_diff| = 5 > 4 so diag_diff stays empty.
  expect_identical(out$diag_diff, c("", ""))
})


# ---- loo_model_weights.mvgam_lfo --------------------------------

test_that("loo_model_weights.mvgam_lfo returns pseudobma_weights summing to 1", {
  m1 <- mk_mvgam_lfo(c(-1, -2, -3))
  m2 <- mk_mvgam_lfo(c(-3, -4, -5))
  w <- loo_model_weights(m1, m2)
  expect_s3_class(w, "pseudobma_weights")
  expect_length(w, 2L)
  expect_equal(sum(w), 1, tolerance = 1e-12)
  # m1 has higher total ELPD (-6 vs -12), so weight[1] > weight[2].
  expect_gt(w[1L], w[2L])
})


test_that("loo_model_weights.mvgam_lfo single model returns 1", {
  m1 <- mk_mvgam_lfo(c(-1, -2, -3))
  w <- loo_model_weights(m1)
  expect_length(w, 1L)
  expect_equal(unname(w[1L]), 1, tolerance = 1e-12)
})


test_that("loo_model_weights.mvgam_lfo errors on grid mismatch", {
  m1 <- mk_mvgam_lfo(c(-1, -2, -3), eval_timepoints = c(10, 11, 12))
  m2 <- mk_mvgam_lfo(c(-1, -2, -3), eval_timepoints = c(20, 21, 22))
  expect_error(loo_model_weights(m1, m2), "eval_timepoints")
})


test_that("loo_model_weights.mvgam_lfo errors when ELPDs are missing", {
  m1 <- mk_mvgam_lfo(c(-1, -2, -3))
  m2 <- mk_mvgam_lfo(c(-1, -2, -3))
  m2$elpds <- NULL
  expect_error(loo_model_weights(m1, m2), "no ELPDs")
})


test_that("loo_model_weights.mvgam_lfo method='stacking' errs without $log_lik", {
  m1 <- mk_mvgam_lfo(c(-1, -2, -3))
  m2 <- mk_mvgam_lfo(c(-2, -3, -4))
  expect_error(
    loo_model_weights(m1, m2, method = "stacking"),
    "log_lik"
  )
})


test_that("loo_model_weights.mvgam_lfo method='stacking' runs with $log_lik", {
  # Build two LFO shells with synthetic per-draw log-density
  # matrices. Model A's log-densities are systematically higher,
  # so stacking should put more weight on it. The PSIS step
  # inside loo's stacking optimiser warns about Pareto-k on this
  # synthetic data; that is incidental to the test (real LFO log
  # densities are smoother) and we tolerate it explicitly here.
  set.seed(42L)
  m_a <- mk_mvgam_lfo(c(-1, -1, -1))
  m_b <- mk_mvgam_lfo(c(-2, -2, -2))
  n_draws <- 100L
  n_eval  <- 30L
  m_a$log_lik <- matrix(
    stats::rnorm(n_draws * n_eval, mean = -1, sd = 0.3),
    nrow = n_draws
  )
  m_b$log_lik <- matrix(
    stats::rnorm(n_draws * n_eval, mean = -2, sd = 0.3),
    nrow = n_draws
  )
  w <- withCallingHandlers(
    loo_model_weights(m_a, m_b, method = "stacking"),
    warning = function(cnd) {
      if (grepl("Pareto k", conditionMessage(cnd))) {
        invokeRestart("muffleWarning")
      }
    }
  )
  expect_length(w, 2L)
  expect_equal(sum(w), 1, tolerance = 1e-8)
  expect_gt(w[["m_a"]], w[["m_b"]])
  expect_match(attr(w, "method"), "stacking")
})


test_that("loo_model_weights.mvgam_lfo method='stacking' errs on width mismatch", {
  m_a <- mk_mvgam_lfo(c(-1, -1, -1))
  m_b <- mk_mvgam_lfo(c(-2, -2, -2))
  m_a$log_lik <- matrix(stats::rnorm(100L * 30L), nrow = 100L)
  m_b$log_lik <- matrix(stats::rnorm(100L * 25L), nrow = 100L)
  expect_error(
    loo_model_weights(m_a, m_b, method = "stacking"),
    "different widths"
  )
})


test_that("loo_model_weights.mvgam_lfo rejects unknown method", {
  m1 <- mk_mvgam_lfo(c(-1, -2, -3))
  m2 <- mk_mvgam_lfo(c(-3, -4, -5))
  expect_error(loo_model_weights(m1, m2, method = "stacking-energy"))
})


# Adaptive Pareto-k threshold (Vehtari, Simpson, Gelman, Yao,
# Gabry 2024). Formula: min(1 - 1 / log10(S), 0.7). Draws S is
# taken from the posterior; unit tests exercise the helper
# directly on synthetic S values so no fit is required.

test_that("mvgam_ps_khat_threshold tightens with small S", {
  # S = 200 -> 1 - 1/log10(200) = 0.5654...
  expect_equal(mvgam:::mvgam_ps_khat_threshold(200L),
               1 - 1 / log10(200), tolerance = 1e-8)
  # S = 500 -> 1 - 1/log10(500) = 0.6289...
  expect_equal(mvgam:::mvgam_ps_khat_threshold(500L),
               1 - 1 / log10(500), tolerance = 1e-8)
})

test_that("mvgam_ps_khat_threshold clamps at 0.7 for large S", {
  # Formula is monotone increasing in S; once 1 - 1 / log10(S)
  # exceeds 0.7, the min() clamps and the classical guarantee
  # holds. S = 5000 -> 1 - 1/3.699 = 0.7297 -> clamp to 0.7.
  expect_equal(mvgam:::mvgam_ps_khat_threshold(5000L), 0.7)
  expect_equal(mvgam:::mvgam_ps_khat_threshold(1000000L), 0.7)
})

test_that("mvgam_ps_khat_threshold rejects S < 2", {
  # S = 1 would divide by log10(1) = 0. The helper rejects it up
  # front so no downstream call has to guard against -Inf.
  expect_error(mvgam:::mvgam_ps_khat_threshold(1L))
  expect_error(mvgam:::mvgam_ps_khat_threshold(0L))
})
