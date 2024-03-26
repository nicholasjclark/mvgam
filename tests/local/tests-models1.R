source("setup_tests_local.R")

test_that("lfo_cv working properly", {
  gaus_data <- sim_mvgam(family = gaussian(),
                         T = 60,
                         trend_model = 'AR1',
                         seasonality = 'shared',
                         mu = c(-1, 0, 1),
                         prop_trend = 0.5,
                         prop_missing = 0.2)
  gaus_ar1fc <- mvgam(y ~ s(series, bs = 're') +
                        s(season, bs = 'cc', k = 5) - 1,
                      trend_model = AR(),
                      data = gaus_data$data_train,
                      newdata = gaus_data$data_test,
                      family = gaussian(),
                      samples = 300)

  lfcv <- lfo_cv(gaus_ar1fc, min_t = 42)
  expect_true(inherits(lfcv, 'mvgam_lfo'))
  expect_true(all.equal(lfcv$eval_timepoints, c(43,44)))
})

# Beta model with trend_formula (use meanfield to ensure that works)
beta_data <- sim_mvgam(family = betar(),
                       trend_model = AR(),
                       prop_trend = 0.5,
                       T = 60)

test_that("variational methods working properly", {
  beta_gpfc <- mvgam(y ~ series,
                     trend_formula = ~ s(season, bs = 'cc', k = 5),
                     trend_model = AR(cor = TRUE),
                     data = beta_data$data_train,
                     newdata = beta_data$data_test,
                     family = betar(),
                     algorithm = 'meanfield')
  expect_true(inherits(beta_gpfc, 'mvgam'))

  beta_gpfc <- mvgam(y ~ series,
                     trend_formula = ~ s(season, bs = 'cc', k = 5),
                     trend_model = AR(cor = TRUE),
                     data = beta_data$data_train,
                     newdata = beta_data$data_test,
                     family = betar(),
                     algorithm = 'fullrank')
  expect_true(inherits(beta_gpfc, 'mvgam'))

  loomod <- loo(beta_gpfc)
  expect_true(inherits(loomod, 'psis_loo'))
})

