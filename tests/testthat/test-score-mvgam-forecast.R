# CI unit tests for `score.mvgam_forecast()` in
# `R/score.mvgam_forecast.R`. Mock `mvgam_forecast` objects let
# us pin the return-shape contract, the dispatch routing, the
# validation guards, and the per-series / all_series aggregation
# without any Stan compilation. The local recovery suite covers
# the on-real-fits half of the testing surface.


# Build a mock `mvgam_forecast` with two series, `h` test points
# per series, and `ndraws` posterior draws per cell.
make_mock_forecast <- function(n_series = 2L, h = 5L,
                                 ndraws = 30L, type = "response",
                                 family = "gaussian",
                                 with_test = TRUE) {
  series_names <- factor(
    paste0("s", seq_len(n_series)),
    levels = paste0("s", seq_len(n_series))
  )
  forecasts <- lapply(seq_len(n_series), function(s) {
    matrix(stats::rnorm(ndraws * h, mean = s, sd = 1),
            nrow = ndraws, ncol = h)
  })
  names(forecasts) <- levels(series_names)
  test_observations <- if (with_test) {
    out <- lapply(seq_len(n_series), function(s) rep(s, h))
    names(out) <- levels(series_names)
    out
  } else {
    NULL
  }
  structure(
    list(
      type = type,
      series_names = series_names,
      forecasts = if (with_test) forecasts else NULL,
      hindcasts = list(),
      test_observations = test_observations,
      test_times = NULL,
      train_observations = list(),
      train_times = list(),
      family = family
    ),
    class = "mvgam_forecast"
  )
}


# ----- Return-shape contract --------------------------------------

test_that("score() is a generic dispatching on mvgam_forecast", {
  fc <- make_mock_forecast()
  expect_true(is.function(score))
  expect_no_error(score(fc, score = "crps"))
})


test_that("Univariate scorers return per-series df + all_series", {
  fc <- make_mock_forecast(n_series = 2L, h = 5L)
  out <- score(fc, "crps")
  expect_type(out, "list")
  expect_named(out, c("s1", "s2", "all_series"))
  expect_s3_class(out[["s1"]], "data.frame")
  expect_identical(
    sort(names(out[["s1"]])),
    sort(c("score", "in_interval", "interval_width",
            "eval_horizon", "score_type"))
  )
  expect_identical(nrow(out[["s1"]]), 5L)
  expect_true(all(out[["s1"]]$score_type == "crps"))
  expect_true(all(out[["s1"]]$interval_width == 0.9))
  expect_identical(out[["s1"]]$eval_horizon, 1:5)
})


test_that("all_series row-sums the per-series score column", {
  fc <- make_mock_forecast(n_series = 2L, h = 4L)
  out <- score(fc, "crps")
  expect_identical(nrow(out$all_series), 4L)
  expect_true(all(out$all_series$score_type == "sum_crps"))
  expect_equal(
    out$all_series$score,
    out$s1$score + out$s2$score
  )
})


test_that("Multivariate scorers return per-series coverage + joint", {
  fc <- make_mock_forecast(n_series = 2L, h = 4L)
  out <- score(fc, "energy")
  expect_named(out, c("s1", "s2", "all_series"))
  # Per-series rows carry NA score and a coverage indicator.
  expect_true(all(is.na(out$s1$score)))
  expect_true(all(out$s1$in_interval %in% c(0, 1)))
  # all_series carries the joint multivariate score.
  expect_identical(nrow(out$all_series), 4L)
  expect_true(all(out$all_series$score_type == "energy"))
  expect_true(all(is.finite(out$all_series$score)))
})


# ----- Dispatch routing -------------------------------------------

test_that("Every scorer routes without error and returns the expected shape", {
  set.seed(42L)
  fc <- make_mock_forecast(n_series = 2L, h = 3L, ndraws = 40L)
  univariate <- c("crps", "drps", "sis", "logs", "dss",
                   "qs", "twcrps")
  for (s in univariate) {
    out <- score(fc, score = s)
    expect_named(out, c("s1", "s2", "all_series"))
    expect_identical(unique(out$s1$score_type), s)
    expect_identical(unique(out$all_series$score_type),
                       paste0("sum_", s))
    expect_identical(nrow(out$s1), 3L)
  }
  multivariate <- c("energy", "variogram", "twenergy")
  for (s in multivariate) {
    out <- score(fc, score = s)
    expect_named(out, c("s1", "s2", "all_series"))
    expect_identical(unique(out$all_series$score_type), s)
    expect_identical(nrow(out$all_series), 3L)
  }
})


test_that("qs uses alpha and twcrps uses lower/upper", {
  set.seed(11L)
  fc <- make_mock_forecast(n_series = 1L, h = 3L, ndraws = 80L)
  s_median <- score(fc, "qs", alpha = 0.5)$s1$score
  s_upper <- score(fc, "qs", alpha = 0.95)$s1$score
  expect_false(isTRUE(all.equal(s_median, s_upper)))

  s_plain <- score(fc, "crps")$s1$score
  s_tw <- score(fc, "twcrps")$s1$score
  expect_equal(s_tw, s_plain, tolerance = 1e-8)
})


# ----- Brier family guard ----------------------------------------

test_that("Brier requires a 'bernoulli' family", {
  fc <- make_mock_forecast(family = "gaussian")
  expect_error(score(fc, "brier"), "bernoulli")
})


test_that("Brier accepted on a bernoulli mock fit", {
  fc <- make_mock_forecast(family = "bernoulli", h = 3L,
                             ndraws = 40L)
  # Forecasts are draws in (0, 1); truths are 0 / 1.
  fc$forecasts <- lapply(fc$forecasts, function(m) {
    stats::plogis(m)
  })
  fc$test_observations <- lapply(fc$test_observations, function(v) {
    rep(1L, length(v))
  })
  out <- score(fc, "brier")
  expect_identical(unique(out$s1$score_type), "brier")
  expect_true(all(is.finite(out$s1$score)))
})


# ----- Validation / error paths -----------------------------------

test_that("type = 'trend' errors with a clear message", {
  fc <- make_mock_forecast(type = "trend")
  expect_error(score(fc, "crps"), "trend")
})


test_that("Missing forecasts / test_observations errors crisply", {
  fc <- make_mock_forecast(with_test = FALSE)
  expect_error(score(fc, "crps"), "no held-out forecasts")
})


test_that("Unknown score name errors via match.arg", {
  fc <- make_mock_forecast()
  expect_error(score(fc, "not_a_score"), "should be one of")
})


test_that("interval_width outside [0.05, 0.95] errors", {
  fc <- make_mock_forecast()
  expect_error(score(fc, "crps", interval_width = 0.99))
  expect_error(score(fc, "crps", interval_width = 0.001))
})


test_that("Empty per-series forecasts produce empty per-series df", {
  fc <- make_mock_forecast(n_series = 2L, h = 3L)
  fc$forecasts[["s1"]] <- matrix(NA_real_, nrow = 30L, ncol = 0L)
  fc$test_observations[["s1"]] <- numeric(0L)
  out <- score(fc, "crps")
  expect_identical(nrow(out$s1), 0L)
  expect_identical(nrow(out$s2), 3L)
  # all_series only aggregates non-empty per-series entries.
  expect_identical(nrow(out$all_series), 3L)
  expect_equal(out$all_series$score, out$s2$score)
})


test_that("Multivariate scoring on inconsistent horizons errors", {
  fc <- make_mock_forecast(n_series = 2L, h = 4L)
  # Trim series 1's forecast horizon to 3 (mismatched).
  fc$forecasts[["s1"]] <- fc$forecasts[["s1"]][, 1:3, drop = FALSE]
  fc$test_observations[["s1"]] <-
    fc$test_observations[["s1"]][1:3]
  expect_error(
    score(fc, "energy"),
    "Multivariate scoring requires a shared forecast horizon"
  )
})


# ----- Routing parity vs direct kernel calls ----------------------

test_that("score(fc, 'crps') score column equals crps_mcmc_object", {
  set.seed(99L)
  fc <- make_mock_forecast(n_series = 1L, h = 4L, ndraws = 50L)
  truth <- fc$test_observations[["s1"]]
  fmat <- fc$forecasts[["s1"]]
  via_dispatch <- score(fc, "crps")$s1$score
  direct <- as.numeric(crps_mcmc_object(truth, fmat)[, "score"])
  expect_equal(via_dispatch, direct)
})


test_that("score(fc, 'energy')$all_series$score equals energy_mcmc_object", {
  set.seed(99L)
  fc <- make_mock_forecast(n_series = 2L, h = 3L, ndraws = 40L)
  truths <- do.call(rbind, fc$test_observations)
  via_dispatch <- score(fc, "energy")$all_series$score
  direct <- energy_mcmc_object(truths, fc$forecasts)
  expect_equal(via_dispatch, direct)
})
