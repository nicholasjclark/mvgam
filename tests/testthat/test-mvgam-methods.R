context("class methods")

test_that("inverse links working", {
  expect_true(is(mvgam:::family_invlinks('gaussian'), 'function'))
  expect_true(is(mvgam:::family_invlinks('Gamma'), 'function'))
  expect_true(is(mvgam:::family_invlinks('beta_binomial'), 'function'))
})

test_that("series_to_mvgam working", {
  data("sunspots")
  series <- cbind(sunspots, sunspots)
  colnames(series) <- c('blood', 'bone')
  expect_true(inherits(series_to_mvgam(series,
                                       frequency(series),
                                       0.85),
                       'list'))

  # An xts object example
  dates <- seq(as.Date("2001-05-01"), length=30, by="quarter")
  data  <- cbind(c(gas = rpois(30, cumprod(1+rnorm(30, mean = 0.01, sd = 0.001)))),
                 c(oil = rpois(30, cumprod(1+rnorm(30, mean = 0.01, sd = 0.001)))))
  series <- xts::xts(x = data, order.by = dates)
  colnames(series) <- c('gas', 'oil')
  expect_list(series_to_mvgam(series,
                              freq = 4,
                              train_prop = 0.85))
})

test_that("stancode and standata working properly", {
  simdat <- sim_mvgam()
  mod <- mvgam(y ~ s(season) +
                 s(time, by = series),
               family = poisson(),
               data = simdat$data_train,
               run_model = FALSE)

  expect_character(stancode(mod))

  expect_list(standata(mod))
})

# Skip remaining time-consuming tests on CRAN
skip_on_cran()

test_that("add_residuals working properly", {
  mod <- mvgam:::mvgam_example1
  oldresids <- mod$resids
  mod <- add_residuals(mod)

  expect_true(all(unlist(lapply(seq_along(oldresids), function(x){
    all.equal(dim(oldresids[[x]]), dim(mod$resids[[x]]))
  }))))
})

test_that("mcmc diagnostics working properly", {
  expect_true(inherits(nuts_params(mvgam:::mvgam_example1),
                       'data.frame'))
  expect_true(inherits(nuts_params(mvgam:::mvgam_example2),
                       'data.frame'))

  expect_true(inherits(rhat(mvgam:::mvgam_example1),
                       'numeric'))
  expect_true(inherits(rhat(mvgam:::mvgam_example4),
                       'numeric'))

  expect_true(inherits(SW(neff_ratio(mvgam:::mvgam_example1)),
                       'numeric'))
  expect_true(inherits(SW(neff_ratio(mvgam:::mvgam_example4)),
                       'numeric'))
})

test_that("compute_edf working properly", {
  mod <- mvgam:::mvgam_example1
  expect_no_error(capture_output(mvgam:::compute_edf(mod$mgcv_model,
                                                     mod,
                                                     'rho',
                                                     'sigma_raw',
                                                     conservative = FALSE)))

  mod <- mvgam:::mvgam_example4
  expect_no_error(capture_output(mvgam:::compute_edf(mod$trend_mgcv_model,
                                                     mod,
                                                     'rho_trend',
                                                     'sigma_raw_trend',
                                                     conservative = TRUE)))
})

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
  expect_silent(p <- mcmc_plot(mvgam:::mvgam_example3, type = "areas"))
  expect_error(mcmc_plot(mvgam:::mvgam_example3, type = "hex"),
               "Exactly 2 parameters must be selected")
  expect_ggplot(mcmc_plot(mvgam:::mvgam_example4))
  expect_no_error(SW(pairs(mvgam:::mvgam_example2)))
  expect_no_error(SW(pairs(mvgam:::mvgam_example4,
                           variable = c('sigma'), regex = TRUE)))
})

test_that("pp_check and ppc work properly", {
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

  expect_no_error(ppc(mvgam:::mvgam_example4))
  expect_error(ppc(mvgam:::mvgam_example1, type = 'banana'))
  expect_no_error(ppc(mvgam:::mvgam_example1, type = 'hist'))
  expect_error(ppc(mvgam:::mvgam_example3, type = 'rootogram'),
               'Rootograms not supported for checking non-count data')
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
                             "s(season).1",
                             "s(season).2",
                             "s(season).3"))

  out <- as.data.frame(mvgam:::mvgam_example4, variable = 'trend_params')
  expect_s3_class(out, "data.frame")
  expect_equal(names(out)[1], "alpha_gp[1]")

  out <- as.data.frame(mvgam:::mvgam_example4, variable = 'obs_params')
  expect_s3_class(out, "data.frame")
  expect_equal(names(out), c("sigma_obs[1]", "sigma_obs[2]"))

  out <- as.matrix(mvgam:::mvgam_example2, variable = 'obs_params')
  expect_true(inherits(out, "matrix"))
  expect_equal(dimnames(out)[[2]], c("sigma_obs[1]", "sigma_obs[2]"))
})

test_that("coef has resonable outputs", {
  out <- coef(mvgam:::mvgam_example1)
  expect_equal(rownames(out), c("(Intercept)",
                                "s(season).1",
                                "s(season).2",
                                "s(season).3"))
  expect_equal(dim(out), c(4, 5))
})

test_that("logLik has reasonable ouputs", {
  liks <- logLik(mvgam:::mvgam_example4)
  expect_equal(dim(liks),
               c(5, NROW(mvgam:::mvgam_example2$obs_data)))
  # NAs in observations should propagate for likelihood calculations
  expect_true(all(is.na(liks[,which(is.na(mvgam:::mvgam_example2$obs_data$y))])))
})

test_that("predict has reasonable outputs", {
  gaus_preds <- predict(mvgam:::mvgam_example4, type = 'link', summary = FALSE)
  expect_equal(dim(gaus_preds),
               c(5, NROW(mvgam:::mvgam_example2$obs_data)))

  gaus_preds <- predict(mvgam:::mvgam_example3, type = 'response', summary = FALSE)
  expect_equal(dim(gaus_preds),
               c(5, NROW(mvgam:::mvgam_example3$obs_data)))

  expect_error(predict(mvgam:::mvgam_example1, type = 'latent_N'),
               '"latent_N" type only available for N-mixture models',
               fixed = TRUE)

  preds <- predict(mvgam:::mvgam_example3, type = 'terms')
  expect_true(inherits(preds, 'list'))
  expect_true(all.equal(names(preds$obs_effects), c('fit', 'se.fit')))
  expect_true(is.null(preds$process_effects))

  preds <- predict(mvgam:::mvgam_example2, type = 'terms')
  expect_true(inherits(preds, 'list'))
  expect_true(length(preds$obs_effects) == 0)
  expect_true(!is.null(preds$process_effects))

  preds <- predict(mvgam:::mvgam_example4, type = 'terms',
                   summary = FALSE)
  expect_true(inherits(preds, 'list'))
  expect_true(is.matrix(preds$obs_effects[[1]]))
})

test_that("get_predict has reasonable outputs", {
  gaus_preds <- predict(mvgam:::mvgam_example1, type = 'link',
                        process_error = FALSE,
                        summary = FALSE)
  meffects_preds <- get_predict(mvgam:::mvgam_example1,
                                newdata = mvgam:::mvgam_example1$obs_data,
                                type = 'link')
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
               c(5, NROW(mvgam:::mvgam_example2$obs_data) /
                   nlevels(mvgam:::mvgam_example2$obs_data$series)))
  expect_equal(hc$train_observations[[1]],
               mvgam:::mvgam_example2$obs_data$y[
                 which(mvgam:::mvgam_example2$obs_data$series == 'series_1')])
})

test_that("plot_mvgam_resids gives reasonable outputs", {
  expect_ggplot(plot_mvgam_resids(mvgam:::mvgam_example1))
  expect_ggplot(plot_mvgam_resids(mvgam:::mvgam_example2))
})

test_that("plot_mvgam_series gives reasonable outputs", {
  simdat <- sim_mvgam()
  expect_ggplot(plot_mvgam_series(data = simdat$data_train,
                                  newdata = simdat$data_test))
  expect_ggplot(plot_mvgam_series(data = simdat$data_train,
                                  newdata = simdat$data_test,
                                  series = 'all'))
  expect_ggplot(plot_mvgam_series(data = simdat$data_train,
                                  newdata = simdat$data_test,
                                  lines = FALSE))
  expect_ggplot(plot_mvgam_series(data = simdat$data_train,
                                  newdata = simdat$data_test,
                                  lines = FALSE,
                                  series = 'all'))

  # Should also work for list data
  dat_train <- list()
  for(i in 1:NCOL(simdat$data_train)){
    dat_train[[i]] <- simdat$data_train[,i]
  }
  names(dat_train) <- colnames(simdat$data_train)

  dat_test <- list()
  for(i in 1:NCOL(simdat$data_test)){
    dat_test[[i]] <- simdat$data_test[,i]
  }
  names(dat_test) <- colnames(simdat$data_test)

  expect_ggplot(plot_mvgam_series(data = dat_train))
  expect_ggplot(plot_mvgam_series(data = dat_train,
                                  newdata = dat_test))
  expect_ggplot(plot_mvgam_series(data = dat_train,
                                  newdata = dat_test,
                                  series = 'all'))
  expect_ggplot(plot_mvgam_series(data = dat_train,
                                  newdata = dat_test,
                                  lines = FALSE))
  expect_ggplot(plot_mvgam_series(data = dat_train,
                                  newdata = dat_test,
                                  lines = FALSE,
                                  series = 'all'))

  # And for mvgam objects
  expect_ggplot(plot_mvgam_series(object = mvgam:::mvgam_example1,
                                    series = 1))
  expect_no_error(SW(plot(mvgam:::mvgam_example1, type = 'series')))
})

test_that("forecast and ensemble have reasonable outputs", {
  set.seed(1234)
  mvgam_examp_dat <- sim_mvgam(family = gaussian(),
                               T = 40,
                               prop_missing = 0.1,
                               n_series = 2)
  newdat <- mvgam_examp_dat$data_test
  fc <- forecast(object = mvgam:::mvgam_example4,
                 newdata = newdat)
  expect_s3_class(fc, 'mvgam_forecast')
  expect_equal(dim(fc$forecasts[[1]]),
               c(5, NROW(newdat) /
                   nlevels(newdat$series)))
  expect_equal(fc$test_observations[[1]],
               newdat$y[which(newdat$series == 'series_1')])

  # Check that ensemble.mvgam_forecast works
  fc2 <- forecast(object = mvgam:::mvgam_example3,
                  newdata = newdat)

  fc_ens <- ensemble(fc, fc2, ndraws = 3000)
  expect_equal(dim(fc_ens$forecasts[[1]]),
               c(3000, NROW(newdat) /
                   nlevels(newdat$series)))
  expect_equal(fc_ens$test_observations[[1]],
               newdat$y[which(newdat$series == 'series_1')])


  fc_ens <- ensemble(fc, fc2, ndraws = 19)
  expect_equal(dim(fc_ens$forecasts[[1]]),
               c(19, NROW(newdat) /
                   nlevels(newdat$series)))

  # ndraws must be positive integer
  expect_error(ensemble(fc, fc2, ndraws = 0))
})

test_that("ensemble gives equal pooling", {
  set.seed(1234)
  mvgam_examp_dat <- sim_mvgam(family = gaussian(),
                               T = 40,
                               prop_missing = 0.1,
                               n_series = 2)
  newdat <- mvgam_examp_dat$data_test
  fc <- forecast(object = mvgam:::mvgam_example4,
                 newdata = newdat)
  fc2 <- forecast(object = mvgam:::mvgam_example3,
                  newdata = newdat)

  # Replace casts with dummy data
  fc$hindcasts = lapply(fc$hindcasts,
                        \(series_hcs) matrix(4, 1, ncol(series_hcs)))
  fc2$hindcasts = lapply(fc2$hindcasts,
                         \(series_hcs) matrix(5, 10, ncol(series_hcs)))
  fc$forecasts = lapply(fc$forecasts,
                        \(series_fcs) matrix(1, 1, ncol(series_fcs)))
  fc2$forecasts = lapply(fc2$forecasts,
                         \(series_fcs) matrix(2, 10, ncol(series_fcs)))

  n_draws <- 500
  fc_ens <- ensemble(fc, fc2, ndraws = n_draws)

  # Expect that roughly 50% of hindcasts should be a 4 and 50% a 5
  four_props <- unlist(lapply(seq_along(fc_ens$hindcasts), function(x){
    length(which(fc_ens$hindcasts[[x]] == 4)) / length(fc_ens$hindcasts[[x]])
  }), use.names = FALSE)
  expect_equal(four_props, rep(0.5, 2),  tolerance = 0.03)

  # Expect that roughly 50% of forecasts should be a 1 and 50% a 2
  two_props <- unlist(lapply(seq_along(fc_ens$hindcasts), function(x){
    length(which(fc_ens$forecasts[[x]] == 2)) / length(fc_ens$forecasts[[x]])
  }), use.names = FALSE)
  expect_equal(two_props, rep(0.5, 2), tolerance = 0.03)
})

