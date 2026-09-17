# CI unit tests for `compare_scores()`, `compare_elpds()` and
# `print.mvgam_forecast()`. Same mock-object strategy as
# test-score-mvgam-forecast.R: build tiny `mvgam_forecast` and
# `mvgam_lfo` shells to pin the contract without any Stan
# compilation.


# ---- Mock helpers ------------------------------------------------

make_mock_forecast <- function(n_series = 2L, h = 5L,
                                 ndraws = 30L, type = "response",
                                 family = "gaussian",
                                 obs_offset = 0,
                                 seed = 1L) {
  set.seed(seed)
  series_names <- factor(
    paste0("s", seq_len(n_series)),
    levels = paste0("s", seq_len(n_series))
  )
  forecasts <- lapply(seq_len(n_series), function(s) {
    matrix(stats::rnorm(ndraws * h, mean = s, sd = 1),
            nrow = ndraws, ncol = h)
  })
  names(forecasts) <- levels(series_names)
  test_observations <- lapply(
    seq_len(n_series), function(s) rep(s + obs_offset, h)
  )
  names(test_observations) <- levels(series_names)
  structure(
    list(
      type = type,
      series_names = series_names,
      forecasts = forecasts,
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


make_mock_lfo <- function(elpds, eval_timepoints = NULL,
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


# ---- compare_scores --------------------------------------------

test_that("compare_scores returns a tidy long-format frame for crps", {
  fc1 <- make_mock_forecast(seed = 1L)
  fc2 <- make_mock_forecast(seed = 2L)
  out <- compare_scores(fc1, fc2, score = "crps")
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("model", "series", "eval_horizon",
                       "score", "in_interval"))
  expect_s3_class(out$model, "factor")
  # Level order decides the legend and the colour assignment, and for
  # `series` the order the panels are drawn in, with all_series last.
  # Comparing the sets alone passed on a swap.
  expect_identical(levels(out$model), c("fc1", "fc2"))
  # 2 series + 1 all_series = 3; 5 horizons; 2 models = 30 rows.
  expect_identical(nrow(out), 30L)
  expect_identical(levels(out$series), c("s1", "s2", "all_series"))
})


test_that("compare_scores honours model_names", {
  fc1 <- make_mock_forecast(seed = 1L)
  fc2 <- make_mock_forecast(seed = 2L)
  out <- compare_scores(fc1, fc2, score = "crps",
                         model_names = c("alpha", "beta"))
  # The names arrive in the order they were supplied.
  expect_identical(levels(out$model), c("alpha", "beta"))
})


test_that("compare_scores forwards score_args to score()", {
  # The default fixture puts every observation at the centre of its
  # own predictive distribution, where an interval of any width
  # contains it: 20 of 20 scored rows covered at both 0.9 and 0.5.
  # A row count was all that remained to assert, and it is the same
  # count the test above already makes. One SD of offset separates
  # the two widths.
  fc1 <- make_mock_forecast(seed = 1L, obs_offset = 1)
  fc2 <- make_mock_forecast(seed = 2L, obs_offset = 1)
  wide <- compare_scores(fc1, fc2, score = "crps")
  narrow <- compare_scores(fc1, fc2, score = "crps",
                            score_args = list(interval_width = 0.5))
  expect_identical(nrow(narrow), 30L)
  # The default interval still covers every scored row; halving it
  # leaves a handful. Dropping `score_args` returns 20 here.
  expect_identical(sum(wide$in_interval, na.rm = TRUE), 20)
  expect_lt(sum(narrow$in_interval, na.rm = TRUE), 10L)
})


test_that("compare_scores carries multivariate scores in all_series", {
  fc1 <- make_mock_forecast(seed = 1L)
  fc2 <- make_mock_forecast(seed = 2L)
  out <- compare_scores(fc1, fc2, score = "energy")
  # all_series rows have the joint score; per-series rows are NA.
  all_rows <- subset(out, series == "all_series")
  per_rows <- subset(out, series != "all_series")
  expect_true(all(!is.na(all_rows$score)))
  expect_true(all(is.na(per_rows$score)))
})


test_that("compare_scores errors on a single forecast", {
  fc1 <- make_mock_forecast()
  expect_error(compare_scores(fc1, score = "crps"),
               "at least two")
})


test_that("compare_scores errors on a non-mvgam_forecast arg", {
  fc1 <- make_mock_forecast()
  bad <- list(forecasts = NULL)
  expect_error(compare_scores(fc1, bad, score = "crps"),
               "not an mvgam_forecast")
})


# ---- compare_elpds ---------------------------------------------

test_that("compare_elpds returns a long-format frame", {
  m1 <- make_mock_lfo(c(-1, -2, -3))
  m2 <- make_mock_lfo(c(-3, -4, -5))
  out <- compare_elpds(m1, m2)
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("model", "eval_time", "elpd",
                       "pareto_k", "refit_here"))
  expect_identical(nrow(out), 6L)
  expect_identical(levels(out$model), c("m1", "m2"))
})


test_that("compare_elpds errors on grid mismatch", {
  m1 <- make_mock_lfo(c(-1, -2, -3), eval_timepoints = c(10, 11, 12))
  m2 <- make_mock_lfo(c(-1, -2, -3), eval_timepoints = c(20, 21, 22))
  expect_error(compare_elpds(m1, m2), "eval_timepoints")
})


test_that("compare_elpds errors on a single LFO arg", {
  m1 <- make_mock_lfo(c(-1, -2, -3))
  expect_error(compare_elpds(m1), "at least two")
})


test_that("compare_elpds honours model_names", {
  m1 <- make_mock_lfo(c(-1, -2, -3))
  m2 <- make_mock_lfo(c(-3, -4, -5))
  out <- compare_elpds(m1, m2, model_names = c("fast", "slow"))
  expect_identical(levels(out$model), c("fast", "slow"))
})


# ---- print.mvgam_forecast --------------------------------------

test_that("print.mvgam_forecast emits a single-screen header", {
  fc <- make_mock_forecast(n_series = 2L, h = 5L, ndraws = 30L)
  # cli::cli_inform routes through rlang's signal mechanism, not
  # the regular message() stream, so testthat::capture_messages()
  # is the right capture; capture.output(type = "message") returns
  # an empty vector for this output.
  msg <- testthat::capture_messages(print(fc))
  blob <- paste(msg, collapse = "\n")
  expect_lt(nchar(blob), 1000L)
  expect_match(blob, "mvgam_forecast")
  expect_match(blob, "series:.*2")
  expect_match(blob, "draws:.*30")
})


test_that("print.mvgam_forecast adds an ensemble bullet when weighted", {
  fc <- make_mock_forecast()
  attr(fc, "weights") <- c(spline = 0.2, ar = 0.5, var = 0.3)
  msg <- testthat::capture_messages(print(fc))
  expect_match(paste(msg, collapse = "\n"), "ensemble:.*3 components")
})


test_that("summary.mvgam_compare_scores joint -> wide horizon x model", {
  fc1 <- make_mock_forecast(seed = 1L)
  fc2 <- make_mock_forecast(seed = 2L)
  out <- compare_scores(fc1, fc2, score = "energy")
  s <- summary(out)
  expect_s3_class(s, "tbl_df")
  # Joint score: idvar = eval_horizon, one column per model. A reader
  # compares across the columns in the order they are given, and the
  # set alone passed on a reordering.
  expect_identical(names(s), c("eval_horizon", "fc1", "fc2"))
  expect_identical(nrow(s), 5L)
})


test_that(
  "summary.mvgam_compare_scores univariate -> series + horizon x model", {
  fc1 <- make_mock_forecast(seed = 1L)
  fc2 <- make_mock_forecast(seed = 2L)
  out <- compare_scores(fc1, fc2, score = "crps")
  s <- summary(out)
  expect_identical(names(s),
                   c("series", "eval_horizon", "fc1", "fc2"))
  # 2 series x 5 horizons = 10 rows.
  expect_identical(nrow(s), 10L)
})


test_that("plot.mvgam_compare_scores returns ggplot for univariate", {
  fc1 <- make_mock_forecast(seed = 1L)
  fc2 <- make_mock_forecast(seed = 2L)
  out <- compare_scores(fc1, fc2, score = "crps")
  p <- plot(out)
  expect_s3_class(p, "ggplot")
  # Default = facet by series, so plot$facet$params$facets is named.
  expect_true("series" %in% names(p$facet$params$facets))
})


test_that("plot.mvgam_compare_scores joint score gets one panel", {
  fc1 <- make_mock_forecast(seed = 1L)
  fc2 <- make_mock_forecast(seed = 2L)
  out <- compare_scores(fc1, fc2, score = "energy")
  p <- plot(out)
  expect_s3_class(p, "ggplot")
  # Joint scores drop facet; FacetNull params have no `facets`.
  expect_length(p$facet$params$facets, 0L)
})


test_that("plot.mvgam_compare_scores honours facet = FALSE", {
  fc1 <- make_mock_forecast(seed = 1L)
  fc2 <- make_mock_forecast(seed = 2L)
  out <- compare_scores(fc1, fc2, score = "crps")
  p <- plot(out, facet = FALSE)
  expect_length(p$facet$params$facets, 0L)
})


test_that("plot.mvgam_compare_scores relative pivot drops the baseline", {
  fc1 <- make_mock_forecast(seed = 1L)
  fc2 <- make_mock_forecast(seed = 2L)
  out <- compare_scores(fc1, fc2, score = "crps")
  p <- plot(out, relative = "fc1")
  expect_s3_class(p, "ggplot")
  # Baseline dropped, so only fc2 remains.
  expect_identical(levels(p$data$model), "fc2")
  # The Δ in the y-label confirms the relative branch was taken.
  expect_match(p$labels$y, "Δ")
})


test_that("plot.mvgam_compare_scores errors on unknown baseline", {
  fc1 <- make_mock_forecast(seed = 1L)
  fc2 <- make_mock_forecast(seed = 2L)
  out <- compare_scores(fc1, fc2, score = "crps")
  expect_error(plot(out, relative = "ghost"), "not in x\\$model")
})


test_that("plot.mvgam_compare_elpds returns ggplot + cumulative arg works", {
  m1 <- make_mock_lfo(c(-1, -2, -3))
  m2 <- make_mock_lfo(c(-3, -4, -5))
  out <- compare_elpds(m1, m2)
  p <- plot(out)
  expect_s3_class(p, "ggplot")
  p_cum <- plot(out, cumulative = TRUE)
  expect_match(p_cum$labels$y, "Cumulative")
  # m1 cumulative is c(-1, -3, -6); confirm last value matches.
  m1_cum <- p_cum$data[p_cum$data$model == "m1", "elpd",
                        drop = TRUE]
  expect_equal(tail(m1_cum, 1L), -6, tolerance = 1e-10)
})


test_that("plot.mvgam_compare_elpds relative drops baseline", {
  m1 <- make_mock_lfo(c(-1, -2, -3))
  m2 <- make_mock_lfo(c(-3, -4, -5))
  p <- plot(compare_elpds(m1, m2), relative = "m1")
  expect_identical(levels(p$data$model), "m2")
  expect_match(p$labels$y, "Δ")
})


test_that("print.mvgam_forecast handles hindcast-only fits", {
  fc <- make_mock_forecast()
  fc$forecasts <- NULL
  # Give it a hindcast so the print isn't completely empty.
  fc$hindcasts <- list(s1 = matrix(0, nrow = 10L, ncol = 8L),
                        s2 = matrix(0, nrow = 10L, ncol = 8L))
  msg <- testthat::capture_messages(print(fc))
  blob <- paste(msg, collapse = "\n")
  expect_match(blob, "hindcast.*8 timepoints")
  expect_match(blob, "forecast.*0")
})
