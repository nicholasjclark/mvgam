context("marginaleffects")

skip_on_cran()

test_that("data_grid gives expected output structure", {
  # Dataframe example
  simdat <- sim_mvgam(n_series = 3, prop_trend = 1,
                      trend_model = GP(),
                      mu = 1.5)

  out <- SW(data_grid(season = unique,
                   year = mean,
                   newdata = simdat$data_test))
  expect_true(all(out$year == mean(simdat$data_test$year)))

  myfunc = function(x){
    c(mean(x, na.rm = TRUE),
      max(x, na.rm = TRUE) + 22.4,
      min(x, na.rm = TRUE) - 11.7)
  }
  out <- data_grid(time = myfunc, newdata = simdat$data_test)
  expect_true(NROW(out) == 3)

  # A list example
  out <- data_grid(season = fivenum,
                   year = mean,
                   newdata = mvgam:::mvgam_example5$obs_data)
  expect_true(all.equal(names(out), names(mvgam:::mvgam_example5$obs_data)))
  expect_true(all(out$year == mean(mvgam:::mvgam_example5$obs_data$year)))
})

test_that("get_data gives expected output structure", {
  plot_data <- get_data(mvgam:::mvgam_example2)
  obs_data <- mvgam:::mvgam_example2$obs_data

  # get_data should give the exact same data used for modelling, including
  # any NAs in the outcome variable
  expect_true(identical(data.frame(y = plot_data$y,
                                   season = plot_data$season,
                                   series = plot_data$series),
                        data.frame(y = obs_data$y,
                                   season = obs_data$season,
                                   series = obs_data$series)))
})

test_that("get_predict gives expected output structure", {
  preds <- get_predict(mvgam:::mvgam_example4)
  expect_equal(NROW(preds), NROW(mvgam:::mvgam_example4$obs_data))

  preds <- get_predict(mvgam:::mvgam_example2)
  expect_equal(NROW(preds), NROW(mvgam:::mvgam_example2$obs_data))
})

test_that("averages give expected output structures", {
  ems <- avg_predictions(mvgam:::mvgam_example3)
  expect_equal(NROW(ems), 1)
  expect_true(all(c("estimate", "conf.low", "conf.high") %in% colnames(ems)))

  ems <- avg_predictions(mvgam:::mvgam_example4,
                         variables = list(season = c(1, 6, 12)))
  expect_equal(NROW(ems), 3)
  expect_true(all(c("season", "estimate",
                    "conf.low", "conf.high") %in% colnames(ems)))

  ems <- avg_predictions(mvgam:::mvgam_example5,
                         variables = list(season = c(1, 6, 12)))
  expect_equal(NROW(ems), 3)
  expect_true(all(c("season", "estimate",
                    "conf.low", "conf.high") %in% colnames(ems)))

  ems <- predictions(mvgam:::mvgam_example2, by = 'series')
  expect_equal(NROW(ems), nlevels(mvgam:::mvgam_example3$obs_data$series))
})

