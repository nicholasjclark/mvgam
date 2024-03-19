context("n_mixture")

poisdat <- sim_mvgam()

test_that("only count data allowed for nmixtures", {
  gaus_data$data_train$cap <- 100
  expect_error(mvgam(y ~ s(season),
                     trend_formula = ~ trend,
                     family = nmix(),
                     data = gaus_data$data_train),
               'Values < 0 not allowed for count family responses',
               fixed = TRUE)
})

test_that("cap must be supplied in data", {
  expect_error(get_mvgam_priors(formula = y ~ s(season),
                                trend_formula = ~ s(season) +
                                  trend,
                                trend_model = 'None',
                                family = nmix(),
                                data = poisdat$data_train),
               'Max abundances must be supplied as a variable named "cap" for N-mixture models',
               fixed = TRUE)

  poisdat$data_train$cap <- 100
  expect_error(mvgam(formula = y ~ s(season),
                     trend_formula = ~ s(season) +
                       trend,
                     trend_model = 'None',
                     family = nmix(),
                     data = poisdat$data_train,
                     newdata = poisdat$data_test),
               '"data" and "newdata" have different numbers of columns',
               fixed = TRUE)

  poisdat$data_test$emu <- 50
  expect_error(mvgam(formula = y ~ s(season),
                     trend_formula = ~ s(season) +
                       trend,
                     trend_model = 'None',
                     family = nmix(),
                     data = poisdat$data_train,
                     newdata = poisdat$data_test),
               'Max abundances must be supplied in test data as a variable named "cap" for N-mixture models',
               fixed = TRUE)
})

poisdat$data_train$cap <- 100
poisdat$data_test$cap <- rpois(NROW(poisdat$data_test),
                               lambda = 5) +
  max(poisdat$data_test$y)

test_that("latent process intercept is allowed in nmixtures", {
  prior_df <- get_mvgam_priors(formula = y ~ s(season),
                   trend_formula = ~ s(season) +
                     trend,
                   trend_model = 'None',
                   family = nmix(),
                   data = poisdat$data_train)
  expect_true(any(grepl('(Intercept)_trend',
                        prior_df$param_name, fixed = TRUE)))

  mod <- mvgam(formula = y ~ s(season),
               trend_formula = ~ s(season) +
                 trend,
               trend_model = 'None',
               family = nmix(),
               data = poisdat$data_train,
               newdata = poisdat$data_test,
               priors = prior(std_normal(),
                              class = '(Intercept)_trend'),
               run_model = FALSE)
  expect_true(any(grepl('(Intercept)_trend',
                        mod$model_file, fixed = TRUE)))
  expect_true(any(grepl('b_raw_trend[1] ~ std_normal();',
                        mod$model_file, fixed = TRUE)))

  # Can also test that 'cap' is properly included in model_data
  # The caps should be arranged by series and then by time
  train_cap = poisdat$data_train %>%
    dplyr::arrange(series, time) %>%
    dplyr::pull(cap)
  test_cap = poisdat$data_test %>%
    dplyr::arrange(series, time) %>%
    dplyr::pull(cap)
  expect_true(all(mod$model_data$cap ==
                    c(train_cap,
                      test_cap)))
})
