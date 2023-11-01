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

test_that("forecast() works correctly", {

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

test_that("loo() works correctly", {
  expect_loo(suppressWarnings(loo(mvgam:::mvgam_example1)))
  expect_loo(suppressWarnings(loo(mvgam:::mvgam_example2)))
  expect_loo(suppressWarnings(loo(mvgam:::mvgam_example3)))
  expect_loo(suppressWarnings(loo(mvgam:::mvgam_example4)))
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

