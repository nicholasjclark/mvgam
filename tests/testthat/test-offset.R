context("offsets")

test_that("offset incorporated into link-level linpred for beta", {
  beta_data$data_train$pop <- as.numeric(beta_data$data_train$series) + 0.5
  beta_data$data_test$pop <- as.numeric(beta_data$data_test$series) + 0.5
  testmod <- mvgam(y ~ s(season, bs = 'cc') +
                     offset(pop) + s(series, bs = 're'),
                   trend_model = 'GP',
                   data = beta_data$data_train,
                   family = betar(),
                   run_model = FALSE)
  stancode <- testmod$model_file

  # Offset should be recorded in the mgcv model
  expect_true(!is.null(attr(testmod$mgcv_model$terms, 'offset')))

  # Offset should be in the linpred calculation
  expect_true(expect_match2(stancode,
                            'eta = X * b + off_set;'))

  # Offset should be inv_logit in the model declaration
  expect_true(expect_match2(stancode,
                            '* append_row(b, 1.0) + off_set[obs_ind])'))


  # Offset should be provided in 'data'
  expect_true(expect_match2(stancode,
                            'vector[total_obs] off_set;'))

  # Data for the offset vector should also be incorporated
  # in model_data
  expect_true(!is.null(testmod$model_data$off_set))
})

test_that("offset incorporated into link-level linpred for NB", {
  data = data.frame(out = rpois(100, lambda = 5),
                    pop = rnorm(100),
                    time = 1:100)
  testmod <- mvgam(out ~ 1 +
                      offset(pop),
                    trend_model = 'GP',
                    data = data,
                    family = nb(),
                    run_model = FALSE)
  stancode <- testmod$model_file

  # Offset should be recorded in the mgcv model
  expect_true(!is.null(attr(testmod$mgcv_model$terms, 'offset')))

  # Offset should be in the linpred calculation
  expect_true(expect_match2(stancode,
                            'eta = X * b + off_set;'))

  # Offset should be exponentiated in the model declaration
  expect_true(expect_match2(stancode,
                            '* append_row(b, 1.0) + off_set[obs_ind])'))

  # Offset should be provided in 'data'
  expect_true(expect_match2(stancode,
                            'vector[total_obs] off_set;'))

  # Data for the offset vector should also be incorporated
  # in model_data
  expect_true(!is.null(testmod$model_data$off_set))
})


test_that("offset not allowed in trend_formula", {
  data = data.frame(out = rpois(100, lambda = 5),
                    x = rnorm(100),
                    pop = rnorm(100),
                    time = 1:100)
  expect_error(mvgam(out ~ 1,
                     trend_formula = ~ x + offset(pop),
                     trend_model = 'AR1',
                     data = data,
                     family = nb(),
                     run_model = FALSE),
               'Offsets not allowed in argument "trend_formula"')
})
