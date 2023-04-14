context("plot_mvgam_fc")

test_that('gaussian hindcasts/forecasts are stable', {
  # Hindcasts from within Stan shouldn't change much, despite any
  # improvements to the Stan code for efficiency
  expect_doppelganger('gaussian hindcasts are stable',
                      plot(gaus_ar1, type = 'forecast'))

  # Forecasts from within Stan also shouldn't change much
  expect_doppelganger('gaussian forecasts are stable',
                      plot(gaus_ar1, type = 'forecast',
                           newdata = gaus_data$data_test))
})

test_that('mvgam gaussian forecasts agree with Stan', {
  # Forecasts made from within Stan should broadly agree with forecasts
  # made from forecast()
  score_stan <- plot_mvgam_fc(gaus_ar1fc, series = 1,
                              newdata = gaus_data$data_test,
                              return_score = TRUE)
  score_mvgam <- plot_mvgam_fc(gaus_ar1, series = 1,
                               newdata = gaus_data$data_test,
                               return_score = TRUE)
  expect_equal(score_mvgam$score,
               score_stan$score,
               tolerance = 0.1)
})
