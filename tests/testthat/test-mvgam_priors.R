context("mvgam_priors")

test_that("family must be correctly specified", {
  expect_error(get_mvgam_priors(y ~ s(season),
                                trend_model = 'AR1',
                                data = beta_data$data_train,
                                family = 'besta'),
               'family not recognized')
})

test_that("trend_model must be correctly specified", {
  expect_error(get_mvgam_priors(y ~ s(season),
                                trend_model = 'AR11',
                                data = beta_data$data_train,
                                family = betar()))
})

test_that("response variable must follow family-specific restrictions", {
  expect_error(get_mvgam_priors(y ~ s(season),
                            trend_model = 'AR1',
                            data = gaus_data$data_train,
                            family = lognormal()),
               'Values <= 0 not allowed for lognormal responses')

  expect_error(get_mvgam_priors(y ~ s(season),
                                trend_model = 'AR1',
                                data = gaus_data$data_train,
                                family = poisson()),
               'Values < 0 not allowed for poisson responses')
})

test_that("specified priors appear in the Stan code", {

  priors <- get_mvgam_priors(formula = y ~ s(season, bs = 'cc'),
                             trend_model = 'GP',
                             data = beta_data$data_train,
                             family = betar())
  priors$prior[3] <- "alpha_gp ~ normal(-1, 0.75);"
  stancode <- mvgam(y ~ s(season, bs = 'cc'),
                    trend_model = 'GP',
                    data = beta_data$data_train,
                    family = betar(),
                    priors = priors,
                    run_model = FALSE)$model_file
  expect_true(expect_match2(stancode,
                            'alpha_gp ~ normal(-1, 0.75);'))

  # Now the same using brms functionality; this time use a bound as well
  priors <- prior(normal(-1, 0.75), class = alpha_gp, ub = 1)
  stancode <- mvgam(y ~ s(season, bs = 'cc'),
                    trend_model = 'GP',
                    data = beta_data$data_train,
                    family = betar(),
                    priors = priors,
                    run_model = FALSE)$model_file
  expect_true(expect_match2(stancode,
                            'alpha_gp ~ normal(-1, 0.75);'))
  expect_true(expect_match2(stancode,
                            'vector<lower=0, upper=1>[n_series] alpha_gp;'))
})

test_that("specified trend_formula priors appear in the Stan code", {

  priors <- get_mvgam_priors(formula = y ~ 1,
                             trend_formula = ~ s(season, bs = 'cc') +
                               year,
                             trend_model = 'AR1',
                             data = beta_data$data_train,
                             family = betar())
  priors$prior[5] <- "year_trend ~ uniform(-2, 1);"

  stancode <- mvgam(formula = y ~ 1,
                    trend_formula = ~ s(season, bs = 'cc') +
                      year,
                    trend_model = 'AR1',
                    data = beta_data$data_train,
                    family = betar(),
                    priors = priors,
                    run_model = FALSE)$model_file
  expect_true(expect_match2(stancode,
                            'b_raw_trend[1] ~ uniform(-2, 1);'))
})


test_that("priors on parametric effects behave correctly", {

  priors <- get_mvgam_priors(formula = y ~ s(season, bs = 'cc'),
                             trend_model = 'GP',
                             data = beta_data$data_train,
                             family = betar())
  priors$prior[1] <- "(Intercept) ~ normal(-1, 0.75);"
  stancode <- mvgam(y ~ s(season, bs = 'cc'),
                    trend_model = 'GP',
                    data = beta_data$data_train,
                    family = betar(),
                    priors = priors,
                    run_model = FALSE)$model_file
  expect_true(expect_match2(stancode,
                            'b_raw[1] ~ normal(-1, 0.75);'))

  # Now the same using brms functionality
  priors <- prior(normal(-1, 0.75), class = Intercept)
  stancode <- mvgam(formula = y ~ s(season, bs = 'cc'),
                    trend_model = 'GP',
                    data = beta_data$data_train,
                    family = betar(),
                    priors = priors,
                    run_model = FALSE)$model_file
  expect_true(expect_match2(stancode,
                            'b_raw[1] ~ normal(-1, 0.75);'))

  # Bounds not allowed on parametric effect priors yet
  priors <- prior(normal(-1, 0.75), class = `Intercept`, lb = 0)
  expect_warning(mvgam(formula = y ~ s(season, bs = 'cc'),
                       trend_model = 'GP',
                       data = beta_data$data_train,
                       family = betar(),
                       priors = priors,
                       run_model = FALSE))
})
