context("sim_mvgam")

#### Test basic error and warning messages ####
test_that("family must be correctly specified", {
  expect_error(sim_mvgam(family = 'bogan',
                         trend_model = 'RW',
                         trend_rel = 0.5),
               'family not recognized')
})

test_that("trend_model must be correctly specified", {
  expect_error(sim_mvgam(family = gaussian(),
                         trend_model = 'AR4',
                         trend_rel = 0.5))
})

test_that("trend_rel must be a valid proportion", {
  expect_error(sim_mvgam(family = gaussian(),
                         trend_model = 'AR2',
                         trend_rel = -0.1),
               "Argument 'trend_rel' must be a proportion ranging from 0 to 1, inclusive")
})

test_that("n_lv must be a positive integer", {
  expect_error(sim_mvgam(family = gaussian(),
                         trend_model = 'AR2',
                         trend_rel = 0.4,
                         n_lv = 0.5),
               "Argument 'n_lv' must be a positive integer")
})

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

