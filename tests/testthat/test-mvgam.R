context("mvgam")

test_that("family must be correctly specified", {
  expect_error(mod <- mvgam(y ~ s(season),
                            trend_model = 'AR1',
                            data = beta_data$data_train,
                            family = 'besta',
                            run_model = FALSE),
               'family not recognized')
})

test_that("trend_model must be correctly specified", {
  expect_error(mod <- mvgam(y ~ s(season),
                            trend_model = 'AR11',
                            data = beta_data$data_train,
                            family = betar(),
                            run_model = FALSE))
})

