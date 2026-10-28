# `lfo_cv.mvgam()` runs here on stubbed refits and log likelihoods.
# The methods for its results follow. Real fits are scored in
# tests/local/test-grain-mvbf-wide.R.

# A mock fit of Poisson counts on the given times and series
lfo_mock_fit <- function(time, series = rep("s1", length(time)),
                         trend_type = "AR") {
  structure(
    list(
      fit = structure(list(), class = "fake_stanfit"),
      data = data.frame(time = time, series = factor(series),
                        y = stats::rpois(length(time), lambda = 3)),
      formula = y ~ 1,
      family = poisson(),
      trend_metadata = list(trend_type = trend_type),
      backend = "rstan"
    ),
    class = "mvgam"
  )
}

make_lfo_mock <- function(n_time = 35L, n_series = 1L, offset = 0L) {
  lfo_mock_fit(
    rep(offset + seq_len(n_time), n_series),
    rep(paste0("s", seq_len(n_series)), each = n_time)
  )
}

# A refit returns the mock unchanged. Every observation has log density
# -1 under each of five draws unless `loglik` is given. A `crps` value
# stubs the forecast draws and their score.
local_lfo_stubs <- function(fit, loglik = NULL, crps = NULL,
                            env = parent.frame()) {
  ll <- loglik %||% matrix(-1, 5L, nrow(fit$data))
  stubs <- list(
    update.mvgam = function(object, ...) object,
    log_lik = function(object, ...) ll
  )
  if (!is.null(crps)) {
    stubs$window_predictive <- function(fit, fc_data) {
      structure(list(forecasts = list(s1 = matrix(0, 5L, 1L))),
                class = "mvgam_forecast")
    }
    stubs$score <- function(object, score, ...) {
      list(all_series = data.frame(score = crps))
    }
  }
  do.call(testthat::local_mocked_bindings, c(stubs, list(.env = env)))
}


test_that("lfo_cv refuses arguments it cannot evaluate", {
  fit <- make_lfo_mock()
  expect_error(lfo_cv(fit, score = "nonsense"), "Must be a subset")
  expect_error(lfo_cv(fit, fc_horizon = 0L), "fc_horizon")
  expect_error(lfo_cv(fit, pareto_k_threshold = 1.5), "pareto_k_threshold")
  expect_error(lfo_cv(fit, fc_horizon = 40L),
               "exceeds the available time span")
  expect_error(lfo_cv(fit, min_t = 35L), "largest valid 'min_t' is 34")
  expect_error(lfo_cv(make_lfo_mock(offset = 2009L), min_t = 25L),
               "must be an observed time")
  # Series s1 runs 1 to 30 and s2 runs 5 to 34
  ragged <- lfo_mock_fit(c(1:30, 5:34), rep(c("s1", "s2"), each = 30L))
  expect_error(lfo_cv(ragged, min_t = 20L), "share one time grid")
})


test_that("the data argument of mvgam 1.x is an alias of newdata", {
  fit <- make_lfo_mock()
  local_lfo_stubs(fit)
  withr::local_envvar(TESTTHAT = "")
  expect_warning(
    lfo_cv(fit, min_t = 30L, data = fit$data, silent = 2L),
    "'data' is deprecated"
  )
  expect_error(
    lfo_cv(fit, min_t = 30L, data = fit$data, newdata = fit$data,
           silent = 2L),
    "deprecated alias 'data'"
  )
})


test_that("lfo_cv scores each origin after the first refit", {
  fit <- make_lfo_mock()
  local_lfo_stubs(fit, crps = 0.5)
  out <- lfo_cv(fit, min_t = 30L, score = c("elpd", "crps"), silent = 2L)
  expect_s3_class(out, "mvgam_lfo")
  expect_identical(out$eval_timepoints, 31:35)
  expect_identical(out$refits_at[1L], 30L)
  # A constant log density of -1 gives every fold an ELPD of -1
  expect_equal(out$elpds, rep(-1, 5L))
  expect_identical(out$scores$crps, rep(0.5, 5L))
  expect_identical(out$n_obs, rep(1L, 5L))
  # The adaptive threshold for five draws
  expect_equal(out$pareto_k_threshold, 1 - 1 / log10(5))
  tib <- summary(out)
  expect_named(tib, c("eval_time", "refit_here", "pareto_k", "n_obs",
                      "elpd", "crps"))
  expect_true(tib$refit_here[1L])
})


test_that("a fold's score sums its responses and its observed horizons", {
  one <- function(v) list(all_series = data.frame(score = v))
  expect_identical(lfo_aggregate_score(one(c(0.5, NA, 1))), 1.5)
  # A fold with no observation has no score
  expect_identical(lfo_aggregate_score(one(c(NA_real_, NA_real_))),
                   NA_real_)
  # A long-frame multivariate forecast scores each response
  expect_identical(
    lfo_aggregate_score(list(count = one(1), pa = one(c(2, NA)))), 3
  )
})


test_that("lfo_cv runs on a 2-series fit with a gap in sampling", {
  fit <- make_lfo_mock(n_series = 2L)
  # Neither series was sampled at time 31
  ll <- matrix(-1, 5L, nrow(fit$data))
  ll[, fit$data$time == 31L] <- NA_real_
  local_lfo_stubs(fit, loglik = ll)
  out <- lfo_cv(fit, min_t = 30L, silent = 2L)
  expect_identical(length(out$elpds), 5L)
  # Nothing was observed between the refit and time 32. That fold has
  # no importance ratio and no Pareto k
  expect_identical(is.na(out$pareto_ks), c(TRUE, TRUE, FALSE, FALSE, FALSE))
})


test_that("min_t is a time value on any time origin or grid", {
  years <- make_lfo_mock(offset = 2009L)
  local_lfo_stubs(years)
  out <- lfo_cv(years, min_t = 2039L, silent = 2L)
  expect_identical(out$eval_timepoints, 2040:2044)
  # The default is the 24th time, leaving 10 origins and the horizon
  expect_identical(lfo_cv(years, silent = 2L)$eval_timepoints[1L], 2034L)

  # On an irregular CAR grid the origins are the observed times
  car <- lfo_mock_fit(c(1L, 2L, 4L, 5L, 7L, 9L, 10L, 13L, 15L, 17L, 19L,
                        20L), trend_type = "CAR")
  local_lfo_stubs(car)
  expect_identical(lfo_cv(car, min_t = 9L, silent = 2L)$eval_timepoints,
                   c(10L, 13L, 15L, 17L, 19L, 20L))
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


test_that("plot.mvgam_lfo keeps an unfittable Pareto k off the floor", {
  # `loo:::do_psis_i()` gives an infinite k for a tail of fewer than
  # five draws. Every k is infinite for twenty draws or fewer. Those
  # points stay above the threshold.
  all_inf <- mk_mvgam_lfo(c(-2, -2.5, -3), pareto_ks = rep(Inf, 3L))
  g <- plot(all_inf)
  ks <- g$data$value[g$data$facet == "Pareto k"]
  expect_false(any(ks == -Inf))
  expect_true(all(is.infinite(ks) & ks > 0))
  # An infinite k is drawn as unreliable
  expect_identical(
    unique(g$data$colour[g$data$facet == "Pareto k"]), "outlier"
  )

  # An infinite k is drawn at the largest finite one
  mixed <- mk_mvgam_lfo(c(-2, -2.5, -3), pareto_ks = c(0.2, Inf, 0.6))
  g2 <- plot(mixed)
  ks2 <- g2$data$value[g2$data$facet == "Pareto k"]
  expect_identical(ks2, c(0.2, 0.6, 0.6))
})


test_that("plot.mvgam_lfo draws a panel per score without notices", {
  # A named quantile column makes `data.frame()` warn. A warning fails
  # this test.
  scored <- mk_mvgam_lfo(c(-2, -2.5, -3))
  scored$scores <- list(crps = c(1.1, 1.4, 0.9), dss = c(2, 3, 4))
  # The first fold is scored on a refit and has no Pareto k. ggplot2
  # warns on a missing value it is asked to draw.
  scored$pareto_ks[1L] <- NA_real_
  g <- plot(scored)
  expect_no_warning(ggplot2::ggplot_build(g))
  dss <- "Dawid-Sebastiani score"
  expect_identical(as.character(unique(g$data$facet)),
                   c("Pareto k", "ELPD", "CRPS", dss))
  # Every score is lower-is-better. Its worst fold is the outlier
  expect_identical(g$data$colour[g$data$facet == dss],
                   c("inlier", "inlier", "outlier"))

  # A regular grid is labelled in whole steps. An irregular CAR grid
  # keeps its own fractional times.
  x_breaks <- function(times) {
    built <- ggplot2::ggplot_build(
      plot(mk_mvgam_lfo(c(-2, -2.5, -3), eval_timepoints = times))
    )
    stats::na.omit(built$layout$panel_params[[1L]]$x$breaks)
  }
  expect_true(all(x_breaks(31:33) %% 1 == 0))
  expect_true(any(x_breaks(c(0.4, 0.9, 1.3)) %% 1 != 0))
})


test_that("print.mvgam_lfo aligns every row", {
  scored <- mk_mvgam_lfo(c(-2, -2.5, -3))
  scored$pareto_k_threshold <- 2 / 3
  scored$pareto_k_threshold_adaptive <- TRUE
  scored$scores <- list(crps = c(1, 2, 3))
  out <- capture.output(print(scored))
  expect_identical(out[-1L], c(
    "  fc_horizon         : 1",
    "  pareto_k_threshold : 0.667 (adaptive)",
    "  evaluation points  : 3",
    "  refits             : 0",
    "  ELPD               : sum = -7.5, mean = -2.5",
    "  crps               : sum = 6, mean = 2"
  ))
})


test_that("loo_compare.mvgam_lfo pairs the folds of each model", {
  m1 <- mk_mvgam_lfo(c(-2, -2.5, -3, -2.2, -2.7))
  m2 <- mk_mvgam_lfo(c(-3, -3.7, -3.5, -3.4, -3.1))
  cmp <- loo_compare(m1, m2)
  expect_s3_class(cmp, "compare.loo")
  expect_identical(rownames(cmp), c("m1", "m2"))
  expect_equal(cmp$elpd_diff, c(0, sum(m2$elpds - m1$elpds)))
  expect_equal(cmp$se_diff,
               c(0, sqrt(5) * stats::sd(m2$elpds - m1$elpds)))
  expect_equal(cmp$elpd_lfo, c(sum(m1$elpds), sum(m2$elpds)))
  # A constant difference has no spread. The normal probability is then
  # undefined
  m3 <- mk_mvgam_lfo(m1$elpds - 1)
  exact <- loo_compare(m1, m3)
  expect_equal(exact$se_diff, c(0, 0))
  expect_true(all(is.na(exact$p_worse)))
  expect_identical(exact$diag_elpd, c("N < 100", "N < 100"))
})


test_that("compared LFO results must share a grid and hold ELPDs", {
  m1 <- mk_mvgam_lfo(c(-1, -2, -3), eval_timepoints = c(10, 11, 12))
  shifted <- mk_mvgam_lfo(c(-1, -2, -3), eval_timepoints = c(20, 21, 22))
  longer <- mk_mvgam_lfo(c(-1, -2, -3), eval_timepoints = c(10, 11, 12),
                         fc_horizon = 3L)
  expect_error(loo_compare(m1, shifted), "'eval_timepoints'")
  expect_error(loo_compare(m1, longer), "'fc_horizon'")
  expect_error(loo_model_weights(m1, shifted), "'eval_timepoints'")
  unscored <- m1
  unscored$elpds <- NULL
  expect_error(loo_compare(m1, unscored), "no ELPDs")
  expect_error(loo_model_weights(m1, unscored), "no ELPDs")
})


test_that("mvgam_loo_compare_diagnostics computes p_worse correctly", {
  # A candidate 3 units worse with SE 1
  out <- mvgam:::mvgam_loo_compare_diagnostics(
    elpd_diff = c(0, -3),
    se_diff = c(0, 1),
    n_pointwise = c(200L, 200L),
    pareto_k_list = list(c(0.1, 0.2), c(0.1, 0.2))
  )
  expect_true(is.na(out$p_worse[1L]))
  expect_equal(out$p_worse[2L], pnorm(0, mean = -3, sd = 1),
               tolerance = 1e-12)
  expect_identical(out$diag_diff, c("", "|elpd_diff| < 4"))
  expect_identical(out$diag_elpd, c("", ""))
})


test_that("mvgam_loo_compare_diagnostics threshold overlays", {
  # Few observations mark both rows. A high Pareto k adds a second note
  out <- mvgam:::mvgam_loo_compare_diagnostics(
    elpd_diff = c(0, -5),
    se_diff = c(0, 2),
    n_pointwise = c(50L, 50L),
    pareto_k_list = list(c(0.1, 0.3), c(0.8, 0.4))
  )
  expect_identical(out$diag_elpd, c("N < 100", "N < 100; k_psis > 0.7"))
  expect_identical(out$diag_diff, c("", ""))
})


test_that("pseudo-BMA weights are the softmax of the summed ELPDs", {
  m1 <- mk_mvgam_lfo(c(-1, -2, -3))
  m2 <- mk_mvgam_lfo(c(-3, -4, -5))
  w <- loo_model_weights(m1, m2)
  expect_s3_class(w, "pseudobma_weights")
  expect_equal(unname(c(w)), exp(c(-6, -12)) / sum(exp(c(-6, -12))))
  expect_equal(unname(c(loo_model_weights(m1))), 1)
})


test_that("stacking weights favour the model with higher log densities", {
  set.seed(42L)
  m_a <- mk_mvgam_lfo(c(-1, -1, -1))
  m_b <- mk_mvgam_lfo(c(-2, -2, -2))
  expect_error(loo_model_weights(m_a, m_b, method = "stacking"),
               "log_lik")
  m_a$log_lik <- matrix(stats::rnorm(3000L, -1, 0.3), nrow = 100L)
  m_b$log_lik <- matrix(stats::rnorm(3000L, -2, 0.3), nrow = 100L)
  w <- loo_model_weights(m_a, m_b, method = "stacking")
  expect_equal(sum(w), 1, tolerance = 1e-8)
  expect_gt(w[["m_a"]], w[["m_b"]])
  m_b$log_lik <- m_b$log_lik[, 1:25]
  expect_error(loo_model_weights(m_a, m_b, method = "stacking"),
               "different widths")
})


test_that("the adaptive Pareto k threshold stops at 0.7", {
  # min(1 - 1 / log10(S), 0.7) for S draws (Vehtari et al. 2024)
  expect_equal(mvgam:::mvgam_ps_khat_threshold(200L), 1 - 1 / log10(200))
  expect_equal(mvgam:::mvgam_ps_khat_threshold(5000L), 0.7)
  # The formula divides by zero at S = 1
  expect_error(mvgam:::mvgam_ps_khat_threshold(1L), ">= 2")
})
