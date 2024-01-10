context("marginaleffects")

test_that("get_data gives expected output structure", {
  plot_data <- get_data(gaus_ar1)

  # get_data should give the exact same data used for modelling, including
  # any NAs in the outcome variable
  expect_true(identical(data.frame(y = plot_data$y,
                                   season = plot_data$season,
                                   series = plot_data$series),
                        data.frame(y = gaus_data$data_train$y,
                                   season = gaus_data$data_train$season,
                                   series = gaus_data$data_train$series)))
})

test_that("get_predict gives expected output structure", {
  preds <- get_predict(gaus_ar1)
  expect_equal(NROW(preds), NROW(gaus_data$data_train))
})

test_that("averages give expected output structures", {
  ems <- avg_predictions(gaus_ar1)
  expect_equal(NROW(ems), 1)
  expect_true(all(c("estimate", "conf.low", "conf.high") %in% colnames(ems)))

  ems <- avg_predictions(gaus_ar1, variables = list(season = c(1, 6, 12)))
  expect_equal(NROW(ems), 3)
  expect_true(all(c("season", "estimate",
                    "conf.low", "conf.high") %in% colnames(ems)))

  ems <- predictions(gaus_ar1, by = 'series')
  expect_equal(NROW(ems), 3)

})



