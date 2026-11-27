# The tests of test-kfold.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()

test_that("a fold is held out of every response or refused", {
  # A response left unmasked would keep the fold in the likelihood
  set.seed(3L)
  d <- data.frame(y1 = rnorm(12), y2 = rnorm(12), x = rnorm(12))
  prefit <- mvgam(
    brms::bf(brms::mvbind(y1, y2) ~ x) + brms::set_rescor(FALSE),
    data = d, family = gaussian(), run_model = FALSE, silent = 2,
    validate = FALSE
  )
  masked <- mvgam:::mask_heldout_response(prefit, d, 1:3)
  expect_true(all(is.na(masked$y1[1:3])))
  expect_true(all(is.na(masked$y2[1:3])))
  d$y2 <- NULL
  expect_error(mvgam:::mask_heldout_response(prefit, d, 1:3), "'y2'")
})

test_that("plot.mvgam_kfold() returns a ggplot with two facets", {
  pointwise <- c(g1 = -1.2, g2 = -0.8, g3 = -2.4)
  pareto_k <- c(g1 = 0.3, g2 = NA, g3 = 0.5)
  out <- mvgam:::build_mvgam_kfold(
    pointwise = pointwise, pareto_k = pareto_k,
    refit_groups = "g2", n_refit_folds = 1L, K = 3L, group = "site",
    pareto_k_threshold = 0.7, exact = FALSE
  )
  p <- plot(out)
  # One panel for the ELPD and one for the Pareto k
  built <- ggplot2::ggplot_build(p)
  expect_equal(NROW(built$layout$layout), 2L)
  # Group labels stay level. Overlapping ones are dropped.
  expect_true(p$scales$get_scales("x")$guide$params$check.overlap)
})
