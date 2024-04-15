context("n_mixture")

set.seed(100)
beta_data <- sim_mvgam(family = betar(),
                       trend_model = 'GP',
                       trend_rel = 0.5,
                       T = 60)
gaus_data <- sim_mvgam(family = gaussian(),
                       T = 60,
                       trend_model = 'AR1',
                       seasonality = 'shared',
                       mu = c(-1, 0, 1),
                       trend_rel = 0.5,
                       prop_missing = 0.2)
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

  poisdat$data_train$cap <- rpois(NROW(poisdat$data_train),
                                  lambda = 5) +
    max(poisdat$data_train$y, na.rm = TRUE)
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

poisdat$data_train$cap <- rpois(NROW(poisdat$data_train),
                               lambda = 5) +
  max(poisdat$data_train$y, na.rm = TRUE)
poisdat$data_test$cap <- rpois(NROW(poisdat$data_test),
                               lambda = 5) +
  max(poisdat$data_test$y, na.rm = TRUE)

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
                    c(train_cap, test_cap)))
})
