# The tests of test-trend-map.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()

test_that("a constructor's trend_map reaches the Stan data", {
  set.seed(1L)
  d <- data.frame(
    series = factor(rep(c("a", "b", "c"), each = 10L)),
    time = rep(1:10, 3L),
    y = rpois(30L, 5)
  )
  tm <- data.frame(series = factor(c("a", "b", "c")),
                   trend = c(1L, 1L, 2L))
  prefit <- mvgam(
    y ~ 1, trend_formula = ~ AR(p = 1, trend_map = tm), data = d,
    family = poisson(), run_model = FALSE, validate = FALSE
  )
  expect_equal(unname(prefit$standata$Z),
               matrix(c(1, 1, 0, 0, 0, 1), nrow = 3L))
})
