# The tests of test-mvgam-core.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()

test_that("a misspelt argument is refused before the model is built", {
  skip_if_not_installed("cmdstanr")
  d <- data.frame(y = rpois(20, 3), time = 1:20, series = factor("a"))
  # The sampler would refuse `familly` only after the model compiled
  expect_error(
    mvgam(y ~ 1, trend_formula = ~ AR(), data = d, familly = poisson(),
          run_model = FALSE),
    "Did you mean 'family'?"
  )
  # cmdstanr's own `step_size` is close to `stepsize` and passes
  expect_s3_class(
    mvgam(y ~ 1, trend_formula = ~ AR(), data = d, family = poisson(),
          run_model = FALSE, silent = 2, validate = FALSE,
          refresh = 0, step_size = 0.1),
    "mvgam_prefit"
  )
})
