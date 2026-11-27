# The tests of test-lfo-cv-mvgam.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()


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
