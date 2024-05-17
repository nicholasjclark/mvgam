context("example post-processing")

# Skip example testing as they are a bit time-consuming
skip_on_cran()
test_that("fitted() gives correct dimensions", {
  expect_equal(NROW(mvgam:::mvgam_examp_dat$data_train),
               NROW(fitted(mvgam:::mvgam_example1)))

  expect_equal(NROW(mvgam:::mvgam_examp_dat$data_train),
               NROW(fitted(mvgam:::mvgam_example2)))

  expect_equal(NROW(mvgam:::mvgam_examp_dat$data_train),
               NROW(fitted(mvgam:::mvgam_example3)))

  expect_equal(NROW(mvgam:::mvgam_examp_dat$data_train),
               NROW(fitted(mvgam:::mvgam_example4)))
})

test_that("residuals() gives correct dimensions", {
  expect_equal(NROW(mvgam:::mvgam_examp_dat$data_train),
               NROW(residuals(mvgam:::mvgam_example1)))

  expect_equal(NROW(mvgam:::mvgam_examp_dat$data_train),
               NROW(residuals(mvgam:::mvgam_example2)))

  expect_equal(NROW(mvgam:::mvgam_examp_dat$data_train),
               NROW(residuals(mvgam:::mvgam_example3)))

  expect_equal(NROW(mvgam:::mvgam_examp_dat$data_train),
               NROW(residuals(mvgam:::mvgam_example4,
                              robust = TRUE)))

  expect_equal(NROW(mvgam:::mvgam_examp_dat$data_train),
               NROW(residuals(mvgam:::mvgam_example5,
                              summary = FALSE)))
})

test_that("variable extraction works correctly", {
  expect_true(inherits(as.matrix(mvgam:::mvgam_example4,
                                 'A', regex = TRUE),
                       'matrix'))
  expect_true(inherits(as.matrix(mvgam:::mvgam_example4,
                                 'Sigma', regex = TRUE),
                       'matrix'))
  expect_true(inherits(as.matrix(mvgam:::mvgam_example5,
                                 'rho_gp', regex = TRUE),
                       'matrix'))
  expect_true(inherits(as_draws(mvgam:::mvgam_example5,
                                 'rho_gp', regex = TRUE),
                       'draws'))
  expect_true(inherits(as_draws(mvgam:::mvgam_example1,
                                'obs_params', regex = TRUE),
                       'draws'))
  expect_true(inherits(as_draws_df(mvgam:::mvgam_example1,
                                'obs_params', regex = TRUE),
                       'draws'))
  expect_true(inherits(as_draws_matrix(mvgam:::mvgam_example4,
                                   'obs_params'),
                       'draws'))
  expect_true(inherits(as_draws_matrix(mvgam:::mvgam_example4,
                                       'trend_params'),
                       'draws'))
  expect_true(inherits(as_draws_list(mvgam:::mvgam_example4,
                                       'betas'),
                       'draws'))
  expect_true(inherits(as_draws_rvars(mvgam:::mvgam_example4,
                                     'trend_betas'),
                       'draws'))
})

test_that("hindcast() works correctly", {
  hc <- hindcast(mvgam:::mvgam_example1)
  expect_true(inherits(hc$hindcasts, 'list'))
  expect_equal(NROW(mvgam:::mvgam_examp_dat$data_train) /
                 NCOL(mvgam:::mvgam_example1$ytimes),
               NCOL(hc$hindcasts$series_1))

  hc <- hindcast(mvgam:::mvgam_example1, type = 'expected')
  expect_true(inherits(hc$hindcasts, 'list'))
  expect_equal(NROW(mvgam:::mvgam_examp_dat$data_train) /
                 NCOL(mvgam:::mvgam_example1$ytimes),
               NCOL(hc$hindcasts$series_1))

  hc <- hindcast(mvgam:::mvgam_example4)
  expect_true(inherits(hc$hindcasts, 'list'))
  expect_equal(NROW(mvgam:::mvgam_examp_dat$data_train) /
                 NCOL(mvgam:::mvgam_example4$ytimes),
               NCOL(hc$hindcasts$series_1))

  hc <- hindcast(mvgam:::mvgam_example4, type = 'expected')
  expect_true(inherits(hc$hindcasts, 'list'))
  expect_equal(NROW(mvgam:::mvgam_examp_dat$data_train) /
                 NCOL(mvgam:::mvgam_example4$ytimes),
               NCOL(hc$hindcasts$series_1))

  hc <- hindcast(mvgam:::mvgam_example5, type = 'trend')
  expect_true(inherits(hc$hindcasts, 'list'))
  expect_equal(NROW(mvgam:::mvgam_examp_dat$data_train) /
                 NCOL(mvgam:::mvgam_example5$ytimes),
               NCOL(hc$hindcasts$series_1))
})

test_that("predict() works correctly", {
  expect_equal(dim(predict(mvgam:::mvgam_example1, type = 'expected',
                           process_error = FALSE)),
               dim(predict(mvgam:::mvgam_example2, type = 'expected',
                           process_error = FALSE)))

  expect_equal(dim(predict(mvgam:::mvgam_example1, type = 'expected',
                           process_error = TRUE)),
               dim(predict(mvgam:::mvgam_example2, type = 'expected',
                           process_error = TRUE)))

  expect_equal(dim(posterior_linpred(mvgam:::mvgam_example1, type = 'expected',
                           process_error = FALSE)),
               dim(posterior_linpred(mvgam:::mvgam_example2, type = 'expected',
                           process_error = FALSE)))

  expect_equal(dim(posterior_linpred(mvgam:::mvgam_example1, type = 'expected',
                                     process_error = FALSE,
                                     ndraws = 33)),
               dim(posterior_linpred(mvgam:::mvgam_example2, type = 'expected',
                                     process_error = FALSE,
                                     ndraws = 33)))

  expect_equal(dim(predict(mvgam:::mvgam_example3, type = 'expected',
                           process_error = FALSE)),
               dim(predict(mvgam:::mvgam_example4, type = 'expected',
                           process_error = FALSE)))

  expect_equal(NROW(predict(mvgam:::mvgam_example1,
                           newdata = mvgam:::mvgam_examp_dat$data_test,
                           process_error = FALSE)),
               NROW(mvgam:::mvgam_examp_dat$data_test))
  expect_equal(NROW(predict(mvgam:::mvgam_example5,
                            newdata = mvgam:::mvgam_examp_dat$data_test,
                            process_error = TRUE)),
               NROW(mvgam:::mvgam_examp_dat$data_test))

  expect_equal(dim(predict(mvgam:::mvgam_example1,
                           newdata = mvgam:::mvgam_examp_dat$data_test,
                           process_error = FALSE)),
               dim(predict(mvgam:::mvgam_example2,
                           newdata = mvgam:::mvgam_examp_dat$data_test,
                           process_error = FALSE)))

  expect_equal(dim(predict(mvgam:::mvgam_example4,
                           newdata = mvgam:::mvgam_examp_dat$data_test,
                           process_error = TRUE)),
               dim(predict(mvgam:::mvgam_example5,
                           newdata = mvgam:::mvgam_examp_dat$data_test,
                           process_error = TRUE)))

  expect_equal(dim(predict(mvgam:::mvgam_example3,
                           newdata = mvgam:::mvgam_examp_dat$data_test,
                           process_error = FALSE)),
               dim(predict(mvgam:::mvgam_example4,
                           newdata = mvgam:::mvgam_examp_dat$data_test,
                           process_error = FALSE)))
})

test_that("mcmc_plot() works correctly", {
  expect_ggplot(mcmc_plot(mvgam:::mvgam_example1))
  expect_ggplot(mcmc_plot(mvgam:::mvgam_example2))
  expect_ggplot(mcmc_plot(mvgam:::mvgam_example3))
  expect_ggplot(mcmc_plot(mvgam:::mvgam_example4))

  expect_ggplot(mcmc_plot(mvgam:::mvgam_example1, variable = 'trend_params'))
  expect_ggplot(mcmc_plot(mvgam:::mvgam_example2, variable = 'trend_params'))
  expect_ggplot(mcmc_plot(mvgam:::mvgam_example3, variable = 'trend_params'))
  expect_ggplot(mcmc_plot(mvgam:::mvgam_example4, variable = 'trend_params'))
  expect_ggplot(mcmc_plot(mvgam:::mvgam_example5, variable = 'trend_params'))
})

test_that("marginaleffects works correctly", {
  expect_ggplot(plot_slopes(mvgam:::mvgam_example1,
                            variables = 'season',
                            condition = 'season',
                            type = 'link'))
  expect_ggplot(plot_slopes(mvgam:::mvgam_example2,
                            variables = 'season',
                            condition = 'season',
                            type = 'link'))
  expect_ggplot(plot_slopes(mvgam:::mvgam_example3,
                            variables = 'season',
                            condition = 'season',
                            type = 'link'))
  expect_ggplot(plot_slopes(mvgam:::mvgam_example4,
                            variables = 'season',
                            condition = 'season',
                            type = 'link'))
  expect_ggplot(plot_slopes(mvgam:::mvgam_example5,
                            variables = 'season',
                            condition = 'season',
                            type = 'link'))

  expect_ggplot(plot_predictions(mvgam:::mvgam_example1,
                                 condition = 'season',
                                 type = 'link'))
  expect_ggplot(plot_predictions(mvgam:::mvgam_example2,
                                 condition = 'season',
                                 type = 'link'))
  expect_ggplot(plot_predictions(mvgam:::mvgam_example3,
                                 condition = 'season',
                                 type = 'link'))
  expect_ggplot(plot_predictions(mvgam:::mvgam_example4,
                                 condition = 'season',
                                 type = 'link'))
  expect_ggplot(plot_predictions(mvgam:::mvgam_example5,
                                 condition = 'season',
                                 type = 'link'))
})

test_that("plot_mvgam... functions work properly", {
  expect_no_error(plot_mvgam_fc(mvgam:::mvgam_example1))
  expect_no_error(plot_mvgam_fc(mvgam:::mvgam_example2))
  expect_no_error(plot(mvgam:::mvgam_example4, type = 'forecast'))
  expect_no_error(SW(plot(mvgam:::mvgam_example3, type = 'smooths')))
  expect_no_error(SW(plot(mvgam:::mvgam_example3, type = 'smooths',
                       realisations = TRUE)))
  expect_no_error(plot_mvgam_smooth(mvgam:::mvgam_example1,
                                    smooth = 1,
                                    derivatives = TRUE))
  expect_no_error(plot_mvgam_smooth(mvgam:::mvgam_example1,
                                    smooth = 1,
                                    residuals = TRUE))
  expect_no_error(plot_mvgam_smooth(mvgam:::mvgam_example1,
                                    smooth = 1,
                                    realisations = TRUE))
  expect_error(plot_mvgam_smooth(mvgam:::mvgam_example4,
                                    smooth = 1))
  expect_no_error(plot_mvgam_smooth(mvgam:::mvgam_example4,
                                         smooth = 1,
                                         trend_effects = TRUE))
  expect_no_error(plot_mvgam_smooth(mvgam:::mvgam_example4,
                                    smooth = 1,
                                    derivatives = TRUE,
                                    trend_effects = TRUE))
  expect_no_error(plot_mvgam_smooth(mvgam:::mvgam_example4,
                                    derivatives = TRUE,
                                    residuals = TRUE,
                                    trend_effects = TRUE))
  expect_no_error(plot_mvgam_smooth(mvgam:::mvgam_example5,
                                    realisations = TRUE))
  expect_no_error(plot_mvgam_smooth(mvgam:::mvgam_example5,
                                    realisations = TRUE,
                                    newdata = mvgam:::mvgam_examp_dat$data_test))
  expect_message(plot(mvgam:::mvgam_example3, type = 'pterms'),
                 'No parametric terms in model formula')
  expect_message(plot(mvgam:::mvgam_example1, type = 're'))
  expect_error(plot(mvgam:::mvgam_example1, type = 'factors'))
  expect_no_error(plot(mvgam:::mvgam_example5, type = 'factors'))
  expect_no_error(plot_mvgam_trend(mvgam:::mvgam_example1))
  expect_no_error(plot_mvgam_trend(mvgam:::mvgam_example4))
  expect_no_error(plot_mvgam_trend(mvgam:::mvgam_example4,
                                   derivatives = TRUE))
  expect_no_error(plot_mvgam_trend(mvgam:::mvgam_example1,
                                   realisations = TRUE))
  expect_no_error(plot_mvgam_trend(mvgam:::mvgam_example5,
                                   derivatives = TRUE))
  expect_no_error(plot_mvgam_trend(mvgam:::mvgam_example5,
                                   realisations = TRUE))
  expect_no_error(plot_mvgam_series(object = mvgam:::mvgam_example4))
})

test_that("summaries all work", {
  expect_no_error(capture_output(summary(mvgam:::mvgam_example1)))
  expect_no_error(capture_output(summary(mvgam:::mvgam_example2)))
  expect_no_error(capture_output(summary(mvgam:::mvgam_example3)))
  expect_no_error(capture_output(summary(mvgam:::mvgam_example4)))
  expect_no_error(capture_output(summary(mvgam:::mvgam_example5)))

  expect_no_error(capture_output(variables(mvgam:::mvgam_example1)))
  expect_no_error(capture_output(variables(mvgam:::mvgam_example2)))
  expect_no_error(capture_output(variables(mvgam:::mvgam_example3)))
  expect_no_error(capture_output(variables(mvgam:::mvgam_example4)))
  expect_no_error(capture_output(variables(mvgam:::mvgam_example5)))

  expect_no_error(capture_output(mvgam:::mvgam_example1))
  expect_no_error(capture_output(mvgam:::mvgam_example2))
  expect_no_error(capture_output(mvgam:::mvgam_example3))
  expect_no_error(capture_output(mvgam:::mvgam_example4))
  expect_no_error(capture_output(mvgam:::mvgam_example5))
})

test_that("dynamic factor investigations work", {
  lvcors <- lv_correlations(mvgam:::mvgam_example5)
  expect_true(inherits(lvcors, 'list'))
  expect_true(all.equal(dim(lvcors$mean_correlations),
                        c(nlevels(mvgam:::mvgam_example5$obs_data$series),
                          nlevels(mvgam:::mvgam_example5$obs_data$series))))
  expect_true(mvgam:::mvgam_example5$use_lv)
  expect_no_error(plot_mvgam_factors(mvgam:::mvgam_example4))
  expect_no_error(plot_mvgam_factors(mvgam:::mvgam_example5))
  facconts <- plot_mvgam_factors(mvgam:::mvgam_example4,
                                 plot = FALSE)
  expect_true(inherits(facconts, 'data.frame'))
})


test_that("evaluate() functions working", {
  mod <- mvgam:::mvgam_example1
  out <- eval_mvgam(mod,
                    fc_horizon = 6,
                    n_samples = 100,
                    n_cores = 1)
  expect_true(inherits(out, 'list'))
  expect_true(all(names(out) ==
                    levels(mod$obs_data$series)))
  expect_true(NROW(out[[1]]) == 6)

  mod <- mvgam:::mvgam_example4
  out <- eval_mvgam(mod,
                    fc_horizon = 2,
                    n_samples = 100,
                    n_cores = 1)
  expect_true(inherits(out, 'list'))
  expect_true(all(names(out) ==
                    levels(mod$obs_data$series)))
  expect_true(NROW(out[[1]]) == 2)

  mod <- mvgam:::mvgam_example5
  out <- eval_mvgam(mod,
                    fc_horizon = 2,
                    n_samples = 100,
                    n_cores = 1)
  expect_true(inherits(out, 'list'))
  expect_true(all(names(out) ==
                    levels(mod$obs_data$series)))
  expect_true(NROW(out[[1]]) == 2)

  expect_no_error(compare_mvgams(mvgam:::mvgam_example2,
                                 mvgam:::mvgam_example4,
                                 n_samples = 100,
                                 n_evaluations = 2,
                                 n_cores = 1))
})

test_that("lfo_cv() working", {
  lfs <- SW(lfo_cv(mvgam:::mvgam_example1,
                min_t = 27,
                fc_horizon = 1))
  expect_true(inherits(lfs, 'mvgam_lfo'))
  expect_no_error(plot(lfs))

  lfs <- SW(lfo_cv(mvgam:::mvgam_example5,
                min_t = 27,
                fc_horizon = 1))
  expect_true(inherits(lfs, 'mvgam_lfo'))
  expect_no_error(plot(lfs))
})

test_that("forecast() works correctly", {
  fc <- forecast(mvgam:::mvgam_example1,
                 newdata = mvgam:::mvgam_examp_dat$data_test)
  expect_true(inherits(fc$hindcasts, 'list'))
  expect_true(inherits(fc$forecasts, 'list'))
  expect_equal(NROW(mvgam:::mvgam_examp_dat$data_test) /
                 NCOL(mvgam:::mvgam_example1$ytimes),
               NCOL(fc$forecasts$series_1),
               length(fc$test_observations$series_1))

  sc <- score(fc)
  expect_true(inherits(sc, 'list'))
  expect_true(all.equal(names(sc),
                        c(levels(mvgam:::mvgam_examp_dat$data_test$series),
                          'all_series')))
  expect_error(score(fc, score = 'elpd'))
  expect_no_error(score(fc, score = 'energy'))
  expect_no_error(score(fc, score = 'variogram'))
  expect_no_error(score(fc, score = 'sis'))

  fc <- forecast(mvgam:::mvgam_example1,
                 newdata = mvgam:::mvgam_examp_dat$data_test,
                 type = 'expected')
  expect_true(inherits(fc$hindcasts, 'list'))
  expect_true(inherits(fc$forecasts, 'list'))
  expect_equal(NROW(mvgam:::mvgam_examp_dat$data_test) /
                 NCOL(mvgam:::mvgam_example1$ytimes),
               NCOL(fc$forecasts$series_1),
               length(fc$test_observations$series_1))

  fc <- forecast(mvgam:::mvgam_example2,
                 newdata = mvgam:::mvgam_examp_dat$data_test,
                 type = 'link')
  expect_true(inherits(fc$hindcasts, 'list'))
  expect_true(inherits(fc$forecasts, 'list'))
  expect_equal(NROW(mvgam:::mvgam_examp_dat$data_test) /
                 NCOL(mvgam:::mvgam_example2$ytimes),
               NCOL(fc$forecasts$series_1),
               length(fc$test_observations$series_1))
  sc <- score(fc, score = 'elpd')
  expect_true(inherits(sc, 'list'))
  expect_true(all.equal(names(sc),
                        c(levels(mvgam:::mvgam_examp_dat$data_test$series),
                          'all_series')))

  fc <- forecast(mvgam:::mvgam_example2,
                 newdata = mvgam:::mvgam_examp_dat$data_test,
                 type = 'expected')
  expect_true(inherits(fc$hindcasts, 'list'))
  expect_true(inherits(fc$forecasts, 'list'))
  expect_equal(NROW(mvgam:::mvgam_examp_dat$data_test) /
                 NCOL(mvgam:::mvgam_example2$ytimes),
               NCOL(fc$forecasts$series_1),
               length(fc$test_observations$series_1))

  fc <- forecast(mvgam:::mvgam_example3,
                 newdata = mvgam:::mvgam_examp_dat$data_test)
  expect_true(inherits(fc$hindcasts, 'list'))
  expect_true(inherits(fc$forecasts, 'list'))
  expect_equal(NROW(mvgam:::mvgam_examp_dat$data_test) /
                 NCOL(mvgam:::mvgam_example3$ytimes),
               NCOL(fc$forecasts$series_1),
               length(fc$test_observations$series_1))

  fc <- forecast(mvgam:::mvgam_example3,
                 newdata = mvgam:::mvgam_examp_dat$data_test,
                 type = 'expected')
  expect_true(inherits(fc$hindcasts, 'list'))
  expect_true(inherits(fc$forecasts, 'list'))
  expect_equal(NROW(mvgam:::mvgam_examp_dat$data_test) /
                 NCOL(mvgam:::mvgam_example3$ytimes),
               NCOL(fc$forecasts$series_1),
               length(fc$test_observations$series_1))

  fc <- forecast(mvgam:::mvgam_example4,
                 newdata = mvgam:::mvgam_examp_dat$data_test)
  expect_true(inherits(fc$hindcasts, 'list'))
  expect_true(inherits(fc$forecasts, 'list'))
  expect_no_error(plot(fc))
  expect_no_error(plot(fc, hide_xlabels = TRUE))
  expect_no_error(plot(fc, ylab = 'banana'))
  expect_no_error(plot(fc, realisations = TRUE))
  expect_equal(NROW(mvgam:::mvgam_examp_dat$data_test) /
                 NCOL(mvgam:::mvgam_example4$ytimes),
               NCOL(fc$forecasts$series_1),
               length(fc$test_observations$series_1))

  fc <- forecast(mvgam:::mvgam_example4,
                 newdata = mvgam:::mvgam_examp_dat$data_test,
                 type = 'expected')
  expect_true(inherits(fc$hindcasts, 'list'))
  expect_true(inherits(fc$forecasts, 'list'))
  expect_no_error(plot(fc, hide_xlabels = TRUE))
  expect_no_error(plot(fc, ylab = 'banana'))
  expect_no_error(plot(fc, realisations = TRUE))
  expect_equal(NROW(mvgam:::mvgam_examp_dat$data_test) /
                 NCOL(mvgam:::mvgam_example4$ytimes),
               NCOL(fc$forecasts$series_1),
               length(fc$test_observations$series_1))

  fc <- forecast(mvgam:::mvgam_example5,
                 newdata = mvgam:::mvgam_examp_dat$data_test,
                 type = 'expected')
  expect_true(inherits(fc$hindcasts, 'list'))
  expect_true(inherits(fc$forecasts, 'list'))
  expect_no_error(plot(fc, hide_xlabels = TRUE))
  expect_no_error(plot(fc, ylab = 'banana'))
  expect_no_error(plot(fc, realisations = TRUE))
  expect_equal(NROW(mvgam:::mvgam_examp_dat$data_test) /
                 NCOL(mvgam:::mvgam_example5$ytimes),
               NCOL(fc$forecasts$series_1),
               length(fc$test_observations$series_1))
})

test_that("loo() works correctly", {
  options(mc.cores = 1)
  expect_loo(SW(loo(mvgam:::mvgam_example1)))
  expect_loo(SW(loo(mvgam:::mvgam_example2)))
  expect_loo(SW(loo(mvgam:::mvgam_example3)))
  expect_loo(SW(loo(mvgam:::mvgam_example4)))
  expect_loo(SW(loo(mvgam:::mvgam_example5)))

  p <- SW(loo_compare(mvgam:::mvgam_example1,
                      mvgam:::mvgam_example2,
                      model_names = c('banana')))
  expect_true(inherits(p, 'compare.loo'))

  p <- SW(loo_compare(mvgam:::mvgam_example1,
                      mvgam:::mvgam_example2,
                      mvgam:::mvgam_example3,
                      mvgam:::mvgam_example4,
                      mvgam:::mvgam_example5))
  expect_true(inherits(p, 'compare.loo'))
})
