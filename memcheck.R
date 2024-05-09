devtools::load_all()
library(testthat)
test_that("run sim AR and VAR functions for memory checks", {
  expect_no_error(capture_output(sim_mvgam(family = gaussian(),
                                           trend_model = RW())))
  expect_no_error(capture_output(sim_mvgam(family = gaussian(),
                                           trend_model = AR(p = 1))))
  expect_no_error(capture_output(sim_mvgam(family = gaussian(),
                                           trend_model = AR(p = 2))))
  expect_no_error(capture_output(sim_mvgam(family = gaussian(),
                                           trend_model = AR(p = 3))))
  expect_no_error(capture_output(sim_mvgam(family = gaussian(),
                                           trend_model = VAR())))
  expect_no_error(capture_output(sim_mvgam(family = gaussian(),
                                           trend_model = VAR(cor = TRUE))))
})
