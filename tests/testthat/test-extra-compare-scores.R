# The tests of test-compare-scores.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()


test_that("plot.mvgam_compare_scores facets a univariate score by series", {
  skip_if_not_installed("scoringRules")
  fc1 <- make_mock_forecast(seed = 1L)
  fc2 <- make_mock_forecast(seed = 2L)
  out <- compare_scores(fc1, fc2, score = "crps")
  p <- plot(out)
  # The default facets by series. `plot$facet$params$facets` then
  # names `series`.
  expect_true("series" %in% names(p$facet$params$facets))
})


test_that("plot.mvgam_compare_scores honours facet = FALSE", {
  skip_if_not_installed("scoringRules")
  fc1 <- make_mock_forecast(seed = 1L)
  fc2 <- make_mock_forecast(seed = 2L)
  out <- compare_scores(fc1, fc2, score = "crps")
  p <- plot(out, facet = FALSE)
  expect_length(p$facet$params$facets, 0L)
})


test_that("plot.mvgam_compare_scores relative pivot drops the baseline", {
  skip_if_not_installed("scoringRules")
  fc1 <- make_mock_forecast(seed = 1L)
  fc2 <- make_mock_forecast(seed = 2L)
  out <- compare_scores(fc1, fc2, score = "crps")
  p <- plot(out, relative = "fc1")
  # The baseline is dropped and each cell holds fc2 minus fc1
  expect_identical(levels(p$data$model), "fc2")
  cells <- out[out$series != "all_series", ]
  diffs <- cells$score[cells$model == "fc2"] -
    cells$score[cells$model == "fc1"]
  plotted <- p$data[order(p$data$series, p$data$eval_horizon), ]
  expect_equal(plotted$score, diffs)
  expect_error(plot(out, relative = "ghost"), "must name a model")
})


test_that("plot.mvgam_compare_elpds draws cumulative ELPD", {
  m1 <- make_mock_lfo(c(-1, -2, -3))
  m2 <- make_mock_lfo(c(-3, -4, -5))
  out <- compare_elpds(m1, m2)
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
  expect_equal(p$data$elpd, c(-2, -2, -2))
  expect_match(p$labels$y, "Δ")
})
