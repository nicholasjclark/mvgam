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
               'Argument "trend_rel" must be a proportion ranging from 0 to 1, inclusive')
})

test_that("n_lv must be a positive integer", {
  expect_error(sim_mvgam(family = gaussian(),
                         trend_model = 'AR2',
                         trend_rel = 0.4,
                         n_lv = 0.5),
               'Argument "n_lv" must be a positive integer')
})

test_that('low trend_rel should give a weak / absent trend', {
  set.seed(100)
  data <- sim_mvgam(family = gaussian(),
                    trend_model = 'RW',
                    trend_rel = 0.1)
  expect_doppelganger('low trend_rel gives weak trend',
                      plot_mvgam_series(data = data$data_train))
})

test_that('high trend_rel should give a strong trend', {
  set.seed(100)
  data <- sim_mvgam(family = gaussian(),
                    trend_model = 'RW',
                    trend_rel = 0.9)
  expect_doppelganger('high trend_rel gives strong trend',
                      plot_mvgam_series(data = data$data_train))
})


# test_that('test-sim_mvgam', {
#   library(mvgam)
#   dat <- sim_mvgam()
#   mod <- mvgam(y ~ 1,
#                use_stan = TRUE,
#                data = dat$data_train)
# })