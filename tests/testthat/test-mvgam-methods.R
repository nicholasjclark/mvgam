context("class methods")

expect_ggplot <- function(object, ...) {
  testthat::expect_true(is(object, "ggplot"), ...)
}

SM <- suppressMessages
SW <- suppressWarnings

test_that("conditional_effects works properly", {
  effects <- conditional_effects(mvgam:::mvgam_example1)
  lapply(effects, expect_ggplot)

  effects <- conditional_effects(mvgam:::mvgam_example2)
  lapply(effects, expect_ggplot)
})

test_that("mcmc_plot works properly", {
  expect_ggplot(mcmc_plot(mvgam:::mvgam_example1, type = "dens"))
  expect_ggplot(mcmc_plot(mvgam:::mvgam_example1,
                          type = "scatter",
                          variable = variables(mvgam:::mvgam_example1)$observation_betas[2:3, 1]))
  expect_error(mcmc_plot(mvgam:::mvgam_example1, type = "density"), "Invalid plot type")
  expect_ggplot(SW(mcmc_plot(mvgam:::mvgam_example2, type = "neff")))
  expect_ggplot(mcmc_plot(mvgam:::mvgam_example3, type = "acf"))
  expect_silent(p <- mcmc_plot(mvgam:::mvgam_example3, type = "areas"))
  expect_error(mcmc_plot(mvgam:::mvgam_example3, type = "hex"),
               "Exactly 2 parameters must be selected")
  expect_ggplot(mcmc_plot(mvgam:::mvgam_example4))
})

test_that("pp_check works properly", {
  expect_ggplot(SM(pp_check(mvgam:::mvgam_example1)))
  expect_ggplot(SM(pp_check(mvgam:::mvgam_example1,
                         newdata = mvgam:::mvgam_example1$obs_data[1:10, ])))
  expect_ggplot(pp_check(mvgam:::mvgam_example2, "stat", ndraws = 5))
  expect_ggplot(SM(pp_check(mvgam:::mvgam_example3, "error_binned")))
  pp <- pp_check(mvgam:::mvgam_example4,
                 type = "ribbon",
                 x = "season")
  expect_ggplot(pp)
  pp <- pp_check(mvgam:::mvgam_example2, type = "violin_grouped",
                 group = "season",
                 newdata = mvgam:::mvgam_example2$obs_data[1:10, ])
  expect_ggplot(pp)
  expect_ggplot(pp_check(mvgam:::mvgam_example4, prefix = "ppd"))
})

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
  gaus_preds <- predict(gaus_ar1, type = 'link', summary = FALSE)
  expect_equal(dim(gaus_preds),
               c(1200, NROW(gaus_data$data_train)))

  beta_preds <- predict(beta_gp, type = 'response', summary = FALSE)
  expect_equal(dim(beta_preds),
               c(300, NROW(beta_data$data_train)))
  expect_lt(max(beta_preds), 1.00000001)
  expect_gt(max(beta_preds), -0.0000001)

  expect_error(predict(beta_gp, type = 'latent_N'),
               '"latent_N" type only available for N-mixture models',
               fixed = TRUE)
})

test_that("get_predict has reasonable outputs", {
gaus_preds <- predict(gaus_ar1, type = 'link', process_error = FALSE,
                      summary = FALSE)
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
