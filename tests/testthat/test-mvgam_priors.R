context("mvgam_priors")

test_that("get_mvgam_priors finds all classes for which priors can be specified", {
  beta_data$data_train$cov <- rnorm(NROW(beta_data$data_train))
  beta_data$data_train$cov2 <- rnorm(NROW(beta_data$data_train))

  expect_equal(get_mvgam_priors(y ~ s(season) +
                                  cov + cov2 + cov*cov2,
                                data = beta_data$data_train,
                                family = betar())$param_info,

               c("(Intercept)", "cov fixed effect", "cov2 fixed effect",
                 "cov:cov2 fixed effect", "s(season) smooth parameters",
                 "Beta precision parameter"))
})

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

test_that("default intercept prior should match brms implementation", {
  simdat <- sim_mvgam(family = gaussian(), mu = 500)
  def_prior <- get_mvgam_priors(y ~ s(season),
                                trend_model = 'AR1',
                                data = simdat$data_train,
                                family = gaussian())$prior[1]

  expect_equal(trimws(strsplit(def_prior, "[~]")[[1]][2]),
               paste0(brms::get_prior(y ~ 1, data = data.frame(y = simdat$data_train$y),
                                      family = gaussian())$prior[1], ';'))

  # Now try student
  def_prior <- get_mvgam_priors(y ~ s(season),
                                trend_model = 'AR1',
                                data = simdat$data_train,
                                family = student_t())$prior[1]

  expect_equal(trimws(strsplit(def_prior, "[~]")[[1]][2]),
               paste0(brms::get_prior(y ~ 1, data = data.frame(y = simdat$data_train$y),
                                      family = student_t())$prior[1], ';'))

  # Now Poisson
  simdat <- sim_mvgam(family = poisson(), mu = 0)
  def_prior <- get_mvgam_priors(y ~ s(season),
                                trend_model = 'AR1',
                                data = simdat$data_train,
                                family = poisson())$prior[1]

  expect_equal(trimws(strsplit(def_prior, "[~]")[[1]][2]),
               paste0(brms::get_prior(y ~ 1, data = data.frame(y = simdat$data_train$y),
                                      family = poisson())$prior[1], ';'))

  # Now Beta
  simdat <- sim_mvgam(family = betar(), mu = 0)
  def_prior <- get_mvgam_priors(y ~ s(season),
                                trend_model = 'AR1',
                                data = simdat$data_train,
                                family = betar())$prior[1]

  expect_equal(trimws(strsplit(def_prior, "[~]")[[1]][2]),
               paste0(brms::get_prior(y ~ 1, data = data.frame(y = simdat$data_train$y),
                                      family = brms::Beta())$prior[1], ';'))

  # Now Negative Binomial
  simdat <- sim_mvgam(family = nb(), mu = 0)
  def_prior <- get_mvgam_priors(y ~ s(season),
                                trend_model = 'AR1',
                                data = simdat$data_train,
                                family = nb())$prior[1]

  expect_equal(trimws(strsplit(def_prior, "[~]")[[1]][2]),
               paste0(brms::get_prior(y ~ 1, data = data.frame(y = simdat$data_train$y),
                                      family = brms::negbinomial())$prior[1], ';'))
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
