context("example post-processing")

expect_ggplot <- function(object, ...) {
  testthat::expect_true(is(object, "ggplot"), ...)
}

expect_loo <- function(object, ...) {
  testthat::expect_true(is(object, "psis_loo"), ...)
}

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

})

test_that("predict() works correctly", {
  expect_equal(dim(predict(mvgam:::mvgam_example1, type = 'expected',
                           process_error = FALSE)),
               dim(predict(mvgam:::mvgam_example2, type = 'expected',
                           process_error = FALSE)))

  expect_equal(dim(predict(mvgam:::mvgam_example3, type = 'expected',
                           process_error = FALSE)),
               dim(predict(mvgam:::mvgam_example4, type = 'expected',
                           process_error = FALSE)))

  expect_equal(dim(predict(mvgam:::mvgam_example1,
                           newdata = mvgam:::mvgam_examp_dat$data_test,
                           process_error = FALSE)),
               dim(predict(mvgam:::mvgam_example2,
                           newdata = mvgam:::mvgam_examp_dat$data_test,
                           process_error = FALSE)))

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
})

test_that("plot_mvgam... functions work properly", {
  expect_no_error(plot_mvgam_fc(mvgam:::mvgam_example1))
  expect_no_error(plot_mvgam_fc(mvgam:::mvgam_example2))
  expect_no_error(plot(mvgam:::mvgam_example4, type = 'forecast'))
  expect_no_error(plot(mvgam:::mvgam_example3, type = 'smooths'))
  expect_no_error(plot(mvgam:::mvgam_example3, type = 'smooths',
                       realisations = TRUE))
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
  expect_message(plot(mvgam:::mvgam_example3, type = 'pterms'),
                 'No parametric terms in model formula')
  expect_message(plot(mvgam:::mvgam_example1, type = 're'))
  expect_error(plot(mvgam:::mvgam_example1, type = 'factors'))
  expect_no_error(plot_mvgam_trend(mvgam:::mvgam_example1))
  expect_no_error(plot_mvgam_trend(mvgam:::mvgam_example4))
  expect_no_error(plot_mvgam_series(object = mvgam:::mvgam_example4))
})

# Skip forecast and loo testing as they are a bit time-consuming
test_that("forecast() works correctly", {
  skip_on_cran()
  fc <- forecast(mvgam:::mvgam_example1,
                 newdata = mvgam:::mvgam_examp_dat$data_test)
  expect_true(inherits(fc$hindcasts, 'list'))
  expect_true(inherits(fc$forecasts, 'list'))
  expect_equal(NROW(mvgam:::mvgam_examp_dat$data_test) /
                 NCOL(mvgam:::mvgam_example1$ytimes),
               NCOL(fc$forecasts$series_1),
               length(fc$test_observations$series_1))

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
                 newdata = mvgam:::mvgam_examp_dat$data_test)
  expect_true(inherits(fc$hindcasts, 'list'))
  expect_true(inherits(fc$forecasts, 'list'))
  expect_equal(NROW(mvgam:::mvgam_examp_dat$data_test) /
                 NCOL(mvgam:::mvgam_example2$ytimes),
               NCOL(fc$forecasts$series_1),
               length(fc$test_observations$series_1))

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
  expect_equal(NROW(mvgam:::mvgam_examp_dat$data_test) /
                 NCOL(mvgam:::mvgam_example4$ytimes),
               NCOL(fc$forecasts$series_1),
               length(fc$test_observations$series_1))

  fc <- forecast(mvgam:::mvgam_example4,
                 newdata = mvgam:::mvgam_examp_dat$data_test,
                 type = 'expected')
  expect_true(inherits(fc$hindcasts, 'list'))
  expect_true(inherits(fc$forecasts, 'list'))
  expect_equal(NROW(mvgam:::mvgam_examp_dat$data_test) /
                 NCOL(mvgam:::mvgam_example4$ytimes),
               NCOL(fc$forecasts$series_1),
               length(fc$test_observations$series_1))
})

test_that("loo() works correctly", {
  skip_on_cran()
  expect_loo(suppressWarnings(loo(mvgam:::mvgam_example1)))
  expect_loo(suppressWarnings(loo(mvgam:::mvgam_example2)))
  expect_loo(suppressWarnings(loo(mvgam:::mvgam_example3)))
  expect_loo(suppressWarnings(loo(mvgam:::mvgam_example4)))
})
