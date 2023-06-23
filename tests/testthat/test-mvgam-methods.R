context("tests for mvgam and mvgam_forecast class methods")

test_that("as.data.frame and friends have resonable outputs", {
  out <- as.data.frame(gaus_ar1, variable = 'betas')
  expect_s3_class(out, "data.frame")
  expect_equal(names(out)[1], "(Intercept)")

  out <- as.data.frame(gaus_ar1, variable = 'trend_params')
  expect_s3_class(out, "data.frame")
  expect_equal(names(out)[1], "sigma[1]")

  out <- as.data.frame(gaus_ar1, variable = 'obs_params')
  expect_s3_class(out, "data.frame")
  expect_equal(names(out)[1], "sigma_obs[1]")

  out <- as.matrix(beta_gp, variable = 'obs_params')
  expect_true(inherits(out, "matrix"))
  expect_equal(dimnames(out)[[2]][1], "phi[1]")
})

test_that("coef has resonable outputs", {
  out <- coef(gaus_ar1)
  expect_equal(rownames(out)[1], "(Intercept)")
  expect_equal(dim(out), c(9, 5))
})

test_that("logLik has reasonable ouputs", {
  expect_equal(dim(logLik(gaus_ar1, n_cores = 1)),
               c(3000, NROW(gaus_ar1$obs_data)))
})

test_that("predict has reasonable outputs", {
  gaus_preds <- predict(gaus_ar1, n_cores = 1, type = 'link')
  expect_equal(dim(gaus_preds),
               c(3000, NROW(gaus_data$data_train)))

  beta_preds <- predict(beta_gp, n_cores = 1, type = 'response')
  expect_equal(dim(beta_preds),
               c(3000, NROW(beta_data$data_train)))
  expect_lt(max(beta_preds), 1.00000001)
  expect_gt(max(beta_preds), -0.0000001)
})

test_that("forecast and friends have reasonable outputs", {
  expect_error(forecast(beta_gp),
               'newdata must be supplied to compute forecasts')
  hc <- hindcast(beta_gp)
  expect_s3_class(hc, 'mvgam_forecast')
  expect_true(is.null(hc$forecasts))
  expect_equal(dim(hc$hindcasts[[1]]),
               c(3000, NROW(beta_data$data_train) /
                   length(unique(beta_data$data_train$series))))
  expect_equal(hc$train_observations[[1]],
               beta_data$data_train$y[which(beta_data$data_train$series == 'series_1')])

  fc <- forecast(beta_gpfc)
  expect_s3_class(fc, 'mvgam_forecast')
  expect_equal(dim(fc$forecasts[[1]]),
               c(3000, NROW(beta_data$data_test) /
                   length(unique(beta_data$data_test$series))))
  expect_equal(fc$test_observations[[1]],
               beta_data$data_test$y[which(beta_data$data_test$series == 'series_1')])
})
