context("marginaleffects")

skip_on_cran()

test_that("data_grid gives expected output structure", {
  # Dataframe example
  simdat <- sim_mvgam(n_series = 3, prop_trend = 1,
                      trend_model = GP(),
                      mu = 1.5)

  out <- SW(mvgam:::data_grid(season = unique,
                              year = mean,
                              newdata = simdat$data_test))
  expect_true(all(out$year == mean(simdat$data_test$year)))

  myfunc = function(x){
    c(mean(x, na.rm = TRUE),
      max(x, na.rm = TRUE) + 22.4,
      min(x, na.rm = TRUE) - 11.7)
  }
  out <- mvgam:::data_grid(time = myfunc, newdata = simdat$data_test)
  expect_true(NROW(out) == 3)

  # A list example
  out <- mvgam:::data_grid(season = fivenum,
                           year = mean,
                           newdata = mvgam:::mvgam_example4$obs_data)
  expect_true(all.equal(names(out),
                        names(mvgam:::mvgam_example4$obs_data)))
  expect_true(all(out$year == mean(mvgam:::mvgam_example4$obs_data$year)))
})

test_that("get_data gives expected output structure", {
  plot_data <- insight::get_data(mvgam:::mvgam_example2)
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
  preds <- marginaleffects::get_predict(
    mvgam:::mvgam_example4,
    newdata = mvgam:::mvgam_example4$obs_data
  )
  expect_equal(NROW(preds),
               length(mvgam:::mvgam_example4$obs_data$y))

  preds <- marginaleffects::get_predict(
    mvgam:::mvgam_example2,
    newdata = mvgam:::mvgam_example2$obs_data
  )
  expect_equal(NROW(preds), NROW(mvgam:::mvgam_example2$obs_data))
})

test_that("averages give expected output structures", {
  ems <- marginaleffects::avg_predictions(
    mvgam:::mvgam_example3
  )
  expect_equal(NROW(ems), 1)
  expect_true(all(c("estimate",
                    "conf.low",
                    "conf.high") %in% colnames(ems)))

  ems <- marginaleffects::avg_predictions(
    mvgam:::mvgam_example4,
    variables = list(season = c(1, 6, 12))
  )
  expect_equal(NROW(ems), 3)
  expect_true(all(c("season", "estimate",
                    "conf.low", "conf.high") %in% colnames(ems)))

  ems <- marginaleffects::avg_predictions(
    mvgam:::mvgam_example4,
    variables = list(season = c(1, 6, 12))
  )
  expect_equal(NROW(ems), 3)
  expect_true(all(c("season", "estimate",
                    "conf.low", "conf.high") %in% colnames(ems)))

  ems <- marginaleffects::predictions(
    mvgam:::mvgam_example2,
    by = 'series'
  )
  expect_equal(NROW(ems),
               nlevels(mvgam:::mvgam_example3$obs_data$series))
})

test_that("comparisons give expected output structures", {
  cmp <- marginaleffects::comparisons(
    mvgam:::mvgam_example2,
    variables = 'series',
    by = 'time'
  )
  expect_equal(levels(as.factor(cmp$contrast)),
               c("series_2 - series_1"))

  cmp <- marginaleffects::comparisons(
    mvgam:::mvgam_example2,
    variables = list(series = 'pairwise'),
    by = 'time'
  )
  expect_equal(levels(as.factor(cmp$contrast)),
               c("series_2 - series_1"))

  cmp <- marginaleffects::comparisons(
    mvgam:::mvgam_example2,
    newdata = marginaleffects::datagrid(time = c(1, 6, 9),
                                        series = unique),
    variables = list(series = 'pairwise'),
    by = 'time'
  )
  expect_equal(levels(as.factor(cmp$contrast)),
               c("series_2 - series_1"))
  expect_equal(NROW(cmp), 3)
  expect_equal(unique(cmp$time),
               c(1, 6, 9))
})
