# The tests of test-brms-wrappers.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()

test_that("posterior_interval.mvgam respects pars / variable / prob", {
  stub <- make_wrapper_stub()
  # The brms default `prob = 0.95`
  expect_identical(colnames(posterior_interval(stub)), c("2.5%", "97.5%"))
  expect_identical(colnames(posterior_interval(stub, prob = 0.5)),
                   c("25%", "75%"))
  expect_identical(rownames(posterior_interval(stub, variable = "b_x")),
                   "b_x")
})
