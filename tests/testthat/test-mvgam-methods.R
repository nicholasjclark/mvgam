context("class methods")

test_that("conditional_effects works properly", {
  effects <- conditional_effects(mvgam:::mvgam_example1)
  lapply(effects, expect_ggplot)

  effects <- conditional_effects(mvgam:::mvgam_example2)
  lapply(effects, expect_ggplot)

  effects <- conditional_effects(mvgam:::mvgam_example3)
  lapply(effects, expect_ggplot)

  effects <- conditional_effects(mvgam:::mvgam_example4)
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
  expect_ggplot(SW(SM(pp_check(mvgam:::mvgam_example1))))
  expect_ggplot(SW(SM(pp_check(mvgam:::mvgam_example1,
                         newdata = mvgam:::mvgam_example1$obs_data[1:10, ]))))
  expect_ggplot(SW(SM(pp_check(mvgam:::mvgam_example2, "stat", ndraws = 5))))
  expect_ggplot(SW(SM(pp_check(mvgam:::mvgam_example3, "error_binned"))))
  pp <- SW(SM(pp_check(object = mvgam:::mvgam_example4,
                 type = "ribbon_grouped",
                 group = "series",
                 x = "season")))
  expect_ggplot(pp)
  pp <- SW(SM(pp_check(mvgam:::mvgam_example2, type = "violin_grouped",
                 group = "season",
                 newdata = mvgam:::mvgam_example2$obs_data[1:10, ])))
  expect_ggplot(pp)
  expect_ggplot(SW(SM(pp_check(mvgam:::mvgam_example4, prefix = "ppd"))))
})

test_that("model.frame gives expected output structure", {
  mod_data <- model.frame(mvgam:::mvgam_example1)

  # model.frame should give the exact same data used for modelling, including
  # any NAs in the outcome variable
  expect_true(identical(data.frame(y = mod_data$y,
                                   season = mod_data$season),
                        data.frame(y = mvgam:::mvgam_example1$obs_data$y,
                                   season = mvgam:::mvgam_example1$obs_data$season)))

  # The setup mgcv model should have the same number of observations, but with
  # these NAs imputed
  mod_data <- model.frame(mvgam:::mvgam_example1$mgcv_model)
  expect_equal(NROW(mod_data), NROW(mvgam:::mvgam_example1$obs_data))
  expect_false(any(is.na(mod_data)))
})

test_that("as.data.frame and friends have resonable outputs", {
  out <- as.data.frame(mvgam:::mvgam_example4, variable = 'betas')
  expect_s3_class(out, "data.frame")
  expect_equal(names(out), c("(Intercept)",
                             "seriesseries_2",
                             "seriesseries_3"))

  out <- as.data.frame(mvgam:::mvgam_example4, variable = 'trend_params')
  expect_s3_class(out, "data.frame")
  expect_equal(names(out)[1], "A[1,1]")

  out <- as.data.frame(mvgam:::mvgam_example4, variable = 'obs_params')
  expect_s3_class(out, "data.frame")
  expect_equal(names(out), c("sigma_obs[1]", "sigma_obs[2]", "sigma_obs[3]"))

  out <- as.matrix(mvgam:::mvgam_example2, variable = 'obs_params')
  expect_true(inherits(out, "matrix"))
  expect_equal(dimnames(out)[[2]], c("sigma_obs[1]", "sigma_obs[2]", "sigma_obs[3]"))
})

test_that("coef has resonable outputs", {
  out <- coef(mvgam:::mvgam_example1)
  expect_equal(rownames(out), c("(Intercept)",
                                "s(season).1",
                                "s(season).2",
                                "s(season).3",
                                "s(season).4"))
  expect_equal(dim(out), c(5, 5))
})

test_that("logLik has reasonable ouputs", {
  liks <- logLik(mvgam:::mvgam_example4)
  expect_equal(dim(liks),
               c(30, NROW(mvgam:::mvgam_example2$obs_data)))
  # NAs in observations should propagate for likelihood calculations
  expect_true(all(is.na(liks[,which(is.na(mvgam:::mvgam_example2$obs_data$y))])))
})

test_that("predict has reasonable outputs", {
  gaus_preds <- predict(mvgam:::mvgam_example4, type = 'link', summary = FALSE)
  expect_equal(dim(gaus_preds),
               c(30, NROW(mvgam:::mvgam_example2$obs_data)))

  gaus_preds <- predict(mvgam:::mvgam_example3, type = 'response', summary = FALSE)
  expect_equal(dim(gaus_preds),
               c(30, NROW(mvgam:::mvgam_example3$obs_data)))

  expect_error(predict(mvgam:::mvgam_example1, type = 'latent_N'),
               '"latent_N" type only available for N-mixture models',
               fixed = TRUE)
})

test_that("get_predict has reasonable outputs", {
gaus_preds <- predict(mvgam:::mvgam_example1, type = 'link',
                      process_error = FALSE,
                      summary = FALSE)
meffects_preds <- get_predict(mvgam:::mvgam_example1, type = 'link')
expect_true(NROW(meffects_preds) == NCOL(gaus_preds))
expect_true(identical(meffects_preds$estimate,
                      apply(gaus_preds, 2, median)))
})

test_that("hindcast has reasonable outputs", {
  expect_error(forecast(mvgam:::mvgam_example2),
               'newdata must be supplied to compute forecasts')
  hc <- hindcast(mvgam:::mvgam_example1)
  expect_s3_class(hc, 'mvgam_forecast')
  expect_true(is.null(hc$forecasts))
  expect_equal(dim(hc$hindcasts[[1]]),
               c(30, NROW(mvgam:::mvgam_example2$obs_data) /
                   nlevels(mvgam:::mvgam_example2$obs_data$series)))
  expect_equal(hc$train_observations[[1]],
               mvgam:::mvgam_example2$obs_data$y[
                 which(mvgam:::mvgam_example2$obs_data$series == 'series_1')])
})

test_that("forecast has reasonable outputs", {
  skip_on_cran()
  set.seed(1234)
  mvgam_examp_dat <- sim_mvgam(family = gaussian(),
                               T = 40,
                               prop_missing = 0.1)
  newdat <- mvgam_examp_dat$data_test
  fc <- forecast(object = mvgam:::mvgam_example4,
                 newdata = newdat)
  expect_s3_class(fc, 'mvgam_forecast')
  expect_equal(dim(fc$forecasts[[1]]),
               c(30, NROW(newdat) /
                   nlevels(newdat$series)))
  expect_equal(fc$test_observations[[1]],
               newdat$y[which(newdat$series == 'series_1')])
})
