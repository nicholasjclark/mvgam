context("offsets")

test_that("offset incorporated into link-level linpred for beta", {
  beta_data$data_train$pop <- as.numeric(beta_data$data_train$series) + 0.5
  beta_data$data_test$pop <- as.numeric(beta_data$data_test$series) + 0.5
  testmod <- mvgam(y ~ s(season, bs = 'cc') +
                     offset(pop),
                   trend_model = 'GP',
                   data = beta_data$data_train,
                   family = betar(),
                   run_model = FALSE)
  stancode <- testmod$model_file

  # Offset should be recorded in the mgcv model
  expect_true(!is.null(attr(testmod$mgcv_model$terms, 'offset')))

  # Offset should be in the linpred calculation
  expect_true(expect_match2(stancode,
                            'eta = X * b + offset;'))

  # Offset should be inv_logit in the model declaration
  expect_true(expect_match2(stancode,
                            'inv_logit(append_col(flat_xs, flat_trends) * append_row(b, 1.0) + offset[obs_ind])'))


  # Offset should be provided in 'data'
  expect_true(expect_match2(stancode,
                            'int<lower=0> obs_ind[n_nonmissing];'))

  # Data for the offset vector should also be incorporated
  # in model_data
  expect_true(!is.null(testmod$model_data$offset))
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
                            'eta = X * b + offset;'))

  # Offset should be exponentiated in the model declaration
  expect_true(expect_match2(stancode,
                            'exp(append_col(flat_xs, flat_trends) * append_row(b, 1.0) + offset[obs_ind])'))

  # Offset should be provided in 'data'
  expect_true(expect_match2(stancode,
                            'int<lower=0> obs_ind[n_nonmissing];'))

  # Data for the offset vector should also be incorporated
  # in model_data
  expect_true(!is.null(testmod$model_data$offset))
})

