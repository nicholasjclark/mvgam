context("tests for mvgam and mvgam_forecast class methods")

test_that("model.frame gives expected output structure", {
  mod_data <- model.frame(gaus_ar1)

  # model.frame should give the exact same data used for modelling, including
  # any NAs in the outcome variable
  expect_true(identical(data.frame(y = mod_data$y,
                                   season = mod_data$season,
                                   series = mod_data$series),
                        data.frame(y = gaus_data$data_train$y,
                                   season = gaus_data$data_train$season,
                                   series = gaus_data$data_train$series)))

  # The setup mgcv model should have the same number of observations, but with
  # these NAs imputed
  mod_data <- model.frame(gaus_ar1$mgcv_model)
  expect_equal(NROW(mod_data), NROW(gaus_data$data_train))

  expect_false(any(is.na(mod_data)))
})

test_that("as.data.frame and friends have resonable outputs", {
  out <- as.data.frame(gaus_ar1, variable = 'betas')
  expect_s3_class(out, "data.frame")
  expect_equal(names(out)[1], "s(season).1")

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
  expect_equal(rownames(out)[1], "s(season).1")
  expect_equal(dim(out), c(11, 5))
})

test_that("logLik has reasonable ouputs", {
  liks <- logLik(gaus_ar1)
  expect_equal(dim(liks),
               c(1200, NROW(gaus_ar1$obs_data)))
  # NAs in observations should propagate for likelihood calculations
  expect_true(all(is.na(liks[,which(is.na(gaus_ar1$obs_data$y))])))
})

test_that("predict has reasonable outputs", {
  gaus_preds <- predict(gaus_ar1, type = 'link')
  expect_equal(dim(gaus_preds),
               c(1200, NROW(gaus_data$data_train)))

  beta_preds <- predict(beta_gp, type = 'response')
  expect_equal(dim(beta_preds),
               c(300, NROW(beta_data$data_train)))
  expect_lt(max(beta_preds), 1.00000001)
  expect_gt(max(beta_preds), -0.0000001)
})

test_that("get_predict has reasonable outputs", {
gaus_preds <- predict(gaus_ar1, type = 'link', process_error = FALSE)
meffects_preds <- get_predict(gaus_ar1, type = 'link')
expect_true(NROW(meffects_preds) == NCOL(gaus_preds))
expect_true(identical(meffects_preds$estimate,
                      apply(gaus_preds, 2, median)))
})

test_that("forecast and friends have reasonable outputs", {
  expect_error(forecast(beta_gp),
               'newdata must be supplied to compute forecasts')
  hc <- hindcast(beta_gp)
  expect_s3_class(hc, 'mvgam_forecast')
  expect_true(is.null(hc$forecasts))
  expect_equal(dim(hc$hindcasts[[1]]),
               c(300, NROW(beta_data$data_train) /
                   length(unique(beta_data$data_train$series))))
  expect_equal(hc$train_observations[[1]],
               beta_data$data_train$y[which(beta_data$data_train$series == 'series_1')])

  fc <- forecast(beta_gpfc)
  expect_s3_class(fc, 'mvgam_forecast')
  expect_equal(dim(fc$forecasts[[1]]),
               c(1200, NROW(beta_data$data_test) /
                   length(unique(beta_data$data_test$series))))
  expect_equal(fc$test_observations[[1]],
               beta_data$data_test$y[which(beta_data$data_test$series == 'series_1')])
})
