context("mvgam")

test_that("family must be correctly specified", {
  expect_error(mod <- mvgam(y ~ s(season),
                            trend_model = AR(),
                            data = beta_data$data_train,
                            family = 'besta',
                            run_model = FALSE),
               'family not recognized')
})

test_that("response variable must be specified", {
  expect_error(mod <- mvgam( ~ s(season),
                             trend_model = AR(),
                             data = beta_data$data_train,
                             family = betar(),
                             run_model = FALSE),
               'response variable is missing from formula')
})

test_that("drift deprecation message works", {
  expect_message(mvgam(y ~ s(series, bs = 're') +
                         s(season, bs = 'cc', k = 8) +
                         s(time, bs = 'moi', k = 8),
                       trend_model = RW(ma = TRUE),
                       drift = TRUE,
                       data = gaus_data$data_train,
                       newdata = gaus_data$data_test,
                       family = gaussian(),
                       run_model = FALSE),
                 'The "drift" argument is deprecated; use fixed effects of "time" instead')
})

test_that("id to link smooths not allowed yet", {
  expect_error(mod <- mvgam(y ~ s(time, id = 1) +
                               s(time, by = series, id = 1),
                             data = beta_data$data_train,
                             family = betar(),
                             run_model = FALSE),
               'smooth terms with the "id" argument not yet supported by mvgam')
})

test_that("response variable must follow family-specific restrictions", {
  expect_error(mod <- mvgam(y ~ s(season),
                            trend_model = AR(),
                            data = gaus_data$data_train,
                            family = lognormal(),
                            run_model = FALSE),
               'Values <= 0 not allowed for lognormal responses')

  expect_error(mod <- mvgam(y ~ s(season),
                            trend_model = AR(),
                            data = gaus_data$data_train,
                            family = poisson(),
                            run_model = FALSE),
               'Values < 0 not allowed for count family responses')
})

test_that("trend_model must be correctly specified", {
  expect_error(SW(mod <- mvgam(y ~ s(season),
                            trend_model = 'AR11',
                            data = beta_data$data_train,
                            family = betar(),
                            run_model = FALSE)))
})

test_that("outcome variable must be present in data", {
  data = data.frame(out = rnorm(100),
                    temp = rnorm(100),
                    time = 1:100)
  expect_error(mod <- mvgam(formula = y ~ dynamic(temp, rho = 20),
                            data = data,
                            family = gaussian(),
                            run_model = FALSE),
               'variable y not found in data')
})

test_that("series levels must match unique entries in series", {
  levels(beta_data$data_train$series) <- paste0('series_', 1:6)
  expect_error(mvgam(y ~ s(season),
                     trend_model = GP(),
                     data = beta_data$data_train,
                     newdata = beta_data$data_test,
                     family = betar(),
                     run_model = FALSE))
})

test_that("missing values not allowed in predictors", {
  # Include missing vals in training data
  simdat <- sim_mvgam()
  simdat$data_train$season[4] <- NA
  expect_error(mvgam(y ~ s(season),
                     trend_model = GP(),
                     data = simdat$data_train,
                     newdata = simdat$data_test,
                     run_model = FALSE))

  # Include missing vals in testing data
  simdat <- sim_mvgam()
  simdat$data_test$season[4] <- NA
  expect_error(mvgam(y ~ s(season),
                     data = simdat$data_train,
                     newdata = simdat$data_test,
                     run_model = FALSE))
})

test_that("all series must have observations for all unique timepoints", {
  data <- sim_mvgam()
  data$data_train <- data$data_train[-2,]
  expect_error(mod <- mvgam(y ~ s(season),
                            trend_model = AR(),
                            data = data$data_train,
                            family = poisson(),
                            run_model = FALSE),
               'One or more series in data is missing observations for one or more timepoints')

  data <- sim_mvgam()
  data$data_test <- data$data_test[-2,]
  expect_error(mod <- mvgam(y ~ s(season),
                            trend_model = AR(),
                            data = data$data_train,
                            newdata = data$data_test,
                            family = poisson(),
                            run_model = FALSE),
               'One or more series in newdata is missing observations for one or more timepoints')
})

test_that("rho argument must be positive numeric", {
  data = data.frame(out = rnorm(100),
                    temp = rnorm(100),
                    time = 1:100)
  expect_error(mod <- mvgam(formula = out ~ dynamic(temp, rho = -1),
                            data = data,
                            family = gaussian(),
                            run_model = FALSE),
               'Argument "rho" in dynamic() must be a positive value',
               fixed = TRUE)
})

# Skip remaining tests on CRAN as they are slightly time-consuming
skip_on_cran()

test_that("JAGS setups should work", {
# JAGS setup should work, whether installed or not
  simdat <- sim_mvgam()
  mod <- mvgam(y ~ s(season),
               trend_model = RW(),
               data = simdat$data_train,
               family = poisson(),
               use_stan = FALSE,
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(mod$drift == FALSE)

  mod <- mvgam(y ~ s(season),
               trend_model = AR(),
               data = simdat$data_train,
               family = poisson(),
               use_stan = FALSE,
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(mod$drift == FALSE)

  mod <- mvgam(y ~ s(season),
               trend_model = AR(p = 2),
               data = simdat$data_train,
               family = poisson(),
               use_stan = FALSE,
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(mod$drift == FALSE)

  mod <- mvgam(y ~ s(season),
               trend_model = AR(),
               data = simdat$data_train,
               family = nb(),
               use_stan = FALSE,
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(mod$drift == FALSE)

  mod <- mvgam(y ~ s(season),
               trend_model = AR(p = 3),
               data = simdat$data_train,
               family = nb(),
               use_stan = FALSE,
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(mod$drift == FALSE)

  # mod <- mvgam(y ~ s(season),
  #              trend_model = AR(),
  #              data = simdat$data_train,
  #              family = tweedie(),
  #              use_stan = FALSE,
  #              run_model = FALSE)
  # expect_true(inherits(mod, 'mvgam_prefit'))
  # expect_true(mod$drift == FALSE)

  mod <- mvgam(y ~ s(season),
               trend_model = AR(p = 3),
               data = simdat$data_train,
               family = gaussian(),
               use_stan = FALSE,
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(mod$drift == FALSE)

  expect_true(inherits(get_mvgam_priors(y ~ s(season),
                                        trend_model = 'RW',
                                        data = simdat$data_train,
                                        family = gaussian(),
                                        use_stan = FALSE),
                       'data.frame'))
  mod <- mvgam(y ~ s(season),
               trend_model = RW(),
               data = simdat$data_train,
               family = gaussian(),
               use_stan = FALSE,
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(mod$drift == FALSE)
})

test_that("trend = 'None' works for State Space", {
  mod <- mvgam(y ~ s(series, bs = 're'),
               trend_formula = ~ s(season, bs = 'cc', k = 8) +
                 s(time, bs = 'moi', k = 8),
               trend_model = 'None',
               data = gaus_data$data_train,
               newdata = gaus_data$data_test,
               family = gaussian(),
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))

  expect_true(any(grepl(trimws("LV[i, j] ~ normal(trend_mus[ytimes_trend[i, j]], sigma[j]);"),
                        trimws(mod$model_file),
                        fixed = TRUE)))

  expect_true(any(grepl(trimws("trend[i, s] = dot_product(Z[s,  : ], LV[i,  : ]);"),
                        trimws(mod$model_file),
                        fixed = TRUE)))
})

test_that("noncentring working properly for a range of models", {
  # First check messages
  expect_message(mvgam(y ~ s(series, bs = 're') +
                         s(season, bs = 'cc', k = 8) +
                         s(time, bs = 'moi', k = 8),
                       trend_model = RW(ma = TRUE),
                       data = gaus_data$data_train,
                       newdata = gaus_data$data_test,
                       family = gaussian(),
                       noncentred = TRUE,
                       run_model = FALSE),
                 'Non-centering of trends currently not available for this model')

  expect_message(mvgam(y ~ s(series, bs = 're') +
                         s(season, bs = 'cc', k = 8) +
                         s(time, bs = 'moi', k = 8),
                       trend_model = RW(cor = TRUE),
                       data = gaus_data$data_train,
                       newdata = gaus_data$data_test,
                       family = gaussian(),
                       noncentred = TRUE,
                       run_model = FALSE),
                 'Non-centering of trends currently not available for this model')

  expect_message(mvgam(y ~ s(series, bs = 're') +
                         s(season, bs = 'cc', k = 8) +
                         s(time, bs = 'moi', k = 8),
                       trend_model = AR(p = 2, cor = TRUE),
                       data = gaus_data$data_train,
                       newdata = gaus_data$data_test,
                       family = gaussian(),
                       noncentred = TRUE,
                       run_model = FALSE),
                 'Non-centering of trends currently not available for this model')

  expect_message(mvgam(y ~ s(series, bs = 're') +
                         s(season, bs = 'cc', k = 8) +
                         s(season, series, bs = 'sz'),
                       trend_model = VAR(),
                       data = gaus_data$data_train,
                       newdata = gaus_data$data_test,
                       family = gaussian(),
                       noncentred = TRUE,
                       run_model = FALSE),
                 'Non-centering of trends currently not available for this model')

  # Now check that the non-centering is incorporated properly
  mod <- mvgam(y ~ s(series, bs = 're') +
                 s(season, bs = 'cc', k = 8) +
                 s(time, bs = 'moi', k = 8),
               trend_model = RW(),
               data = gaus_data$data_train,
               newdata = gaus_data$data_test,
               family = gaussian(),
               noncentred = TRUE,
               run_model = FALSE)

  # Model file should have the non-centred trend parameterisation now
  expect_true(
    any(grepl(trimws("trend = trend_raw .* rep_matrix(sigma', rows(trend_raw));"),
              trimws(mod$model_file),
              fixed = TRUE))
  )

  expect_true(
    any(grepl(trimws("trend[2 : n, s] += trend[1 : (n - 1), s];"),
              trimws(mod$model_file),
              fixed = TRUE))
  )

  expect_true(
    any(grepl(trimws("to_vector(trend_raw) ~ std_normal();"),
              trimws(mod$model_file),
              fixed = TRUE))
  )

  expect_true(
    any(grepl(trimws("b[b_idx_s_time_] = abs(b_raw[b_idx_s_time_]) * 1;"),
              trimws(mod$model_file),
              fixed = TRUE))
  )

  mod <- mvgam(y ~ s(series, bs = 're') +
                 s(season, bs = 'cc', k = 8) +
                 s(season, series, bs = 'sz'),
               trend_model = AR(p = 3),
               priors = c(prior(beta(2,2), class = ar1,
                              lb = 0, ub = 1),
                          prior(exponential(3.466), class = sigma)),
               data = gaus_data$data_train,
               newdata = gaus_data$data_test,
               family = gaussian(),
               noncentred = TRUE,
               run_model = FALSE)

  expect_true(
    any(grepl(trimws("ar1 ~ beta(2, 2);"),
              trimws(mod$model_file),
              fixed = TRUE))
  )

  expect_true(
    any(grepl(trimws("sigma ~ exponential(3.466);"),
              trimws(mod$model_file),
              fixed = TRUE))
  )

  expect_true(
    any(grepl(trimws("trend = trend_raw .* rep_matrix(sigma', rows(trend_raw));"),
              trimws(mod$model_file),
              fixed = TRUE))
  )

  expect_true(
    any(grepl(trimws("trend[2, s] += ar1[s] * trend[1, s];"),
              trimws(mod$model_file),
              fixed = TRUE))
  )

  expect_true(
    any(grepl(trimws("trend[3, s] += ar1[s] * trend[2, s] + ar2[s] * trend[1, s];"),
              trimws(mod$model_file),
              fixed = TRUE))
  )

  expect_true(
    any(grepl(trimws("to_vector(trend_raw) ~ std_normal();"),
              trimws(mod$model_file),
              fixed = TRUE))
  )
})

test_that("prior_only works", {
  mod <- mvgam(y ~ s(season),
               trend_model = AR(p = 2),
               data = gaus_data$data_train,
               prior_simulation = TRUE,
               family = gaussian(),
               threads = 2,
               run_model = FALSE)
  expect_no_error(capture_output(code(mod)))
  expect_true(!any(grepl('likelihood functions',
                         mod$model_file, fixed = TRUE)))
  expect_true(!any(grepl('flat_ys ~ ',
                         mod$model_file, fixed = TRUE)))

  mod <- mvgam(y ~ 1,
               trend_formula = ~ s(season) + s(trend, bs = 're'),
               trend_model = VAR(ma = TRUE),
               data = gaus_data$data_train,
               prior_simulation = TRUE,
               family = gaussian(),
               threads = 2,
               run_model = FALSE)
  expect_no_error(capture_output(code(mod)))
  expect_true(!any(grepl('likelihood functions',
                         mod$model_file, fixed = TRUE)))
  expect_true(!any(grepl('flat_ys ~ ',
                         mod$model_file, fixed = TRUE)))
  mod <- mvgam(y ~ 1,
               trend_formula = ~ s(season) + s(trend, bs = 're'),
               trend_model = CAR(),
               data = gaus_data$data_train,
               prior_simulation = TRUE,
               family = gaussian(),
               threads = 2,
               run_model = FALSE)
  expect_no_error(capture_output(code(mod)))
  expect_true(!any(grepl('likelihood functions',
                         mod$model_file, fixed = TRUE)))
  expect_true(!any(grepl('flat_ys ~ ',
                         mod$model_file, fixed = TRUE)))

  # trend_map not yet allowed for CAR1 dynamics
  trend_map <- data.frame(series = unique(gaus_data$data_train$series),
                          trend = c(1, 1, 2))
  expect_error(mvgam(y ~ 1,
                     trend_formula = ~ s(season) + s(trend, bs = 're'),
                     trend_model = CAR(),
                     trend_map = trend_map,
                     data = gaus_data$data_train,
                     prior_simulation = TRUE,
                     family = gaussian(),
                     threads = 2,
                     run_model = FALSE),
               'cannot yet use trend mapping for CAR1 dynamics')

  mod <- mvgam(y ~ s(season),
               trend_model = AR(p = 3),
               data = beta_data$data_train,
               prior_simulation = TRUE,
               family = betar(),
               run_model = FALSE)
  expect_no_error(capture_output(code(mod)))
  expect_true(!any(grepl('likelihood functions',
                         mod$model_file, fixed = TRUE)))
  expect_true(!any(grepl('flat_ys ~ ',
                         mod$model_file, fixed = TRUE)))

  mod <- mvgam(y ~ 1,
               trend_formula = ~ s(season) + s(trend, bs = 're'),
               trend_model = AR(cor = TRUE, ma = TRUE),
               data = beta_data$data_train,
               prior_simulation = TRUE,
               family = betar(),
               run_model = FALSE)
  expect_no_error(capture_output(code(mod)))
  expect_true(!any(grepl('likelihood functions',
                         mod$model_file, fixed = TRUE)))
  expect_true(!any(grepl('flat_ys ~ ',
                         mod$model_file, fixed = TRUE)))
})

test_that("time not required in data if this is a no trend model", {
  data = data.frame(out = rnorm(100),
                    temp = rnorm(100))
  mod <- mvgam(formula = out ~ dynamic(temp, rho = 20),
                            data = data,
                            family = gaussian(),
                            run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
})

test_that("median coefs should be stored in the mgcv object", {
  expect_true(identical(unname(coef(mvgam:::mvgam_example2$mgcv_model)),
            coef(mvgam:::mvgam_example2)[,2]))
})

test_that("empty obs formula is allowed, even if no trend_formula", {
  mod <- mvgam(formula = y ~ -1,
               trend_model = AR(),
               data_train = gaus_data$data_train,
               family = gaussian(),
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
})

test_that("empty obs formula allowed if trend_formula supplied", {
  mod <- mvgam(formula = y ~ -1,
               trend_formula = ~ s(season),
               trend_model = AR(),
               data = gaus_data$data_train,
               family = gaussian(),
               run_model = FALSE)

  # Check that the intercept coefficient is correctly fixed at zero
  expect_true(any(grepl('// (Intercept) fixed at zero', mod$model_file,
                        fixed = TRUE)))
  expect_true(any(grepl('b[1] = 0;', mod$model_file,
                        fixed = TRUE)))
})

test_that("share_obs_params working", {
  # Standard beta
  mod <- mvgam(y ~ s(season, by = series),
               trend_model = RW(cor = TRUE),
               family = betar(),
               data = beta_data$data_train,
               share_obs_params = TRUE,
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(any(grepl('real<lower=0>phi;',
                        gsub(' ', '', mod$model_file), fixed = TRUE)))
  expect_true(any(grepl('phi_vec[1:n,s]=rep_vector(phi,n);',
                        gsub(' ', '', mod$model_file), fixed = TRUE)))

  # State-space beta
  mod <- mvgam(y ~ -1,
               trend_formula = ~ s(season, by = trend),
               trend_model = RW(cor = TRUE),
               family = betar(),
               data = beta_data$data_train,
               share_obs_params = TRUE,
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(any(grepl('real<lower=0>phi;',
                        gsub(' ', '', mod$model_file), fixed = TRUE)))
  expect_true(any(grepl('phi_vec[1:n,s]=rep_vector(phi,n);',
                        gsub(' ', '', mod$model_file), fixed = TRUE)))

  # Standard gaussian
  mod <- mvgam(y ~ s(season, by = series),
               trend_model = RW(cor = TRUE),
               family = gaussian(),
               data = gaus_data$data_train,
               share_obs_params = TRUE,
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(any(grepl('real<lower=0>sigma_obs;',
                        gsub(' ', '', mod$model_file), fixed = TRUE)))
  expect_true(any(grepl('sigma_obs_vec[1:n,s]=rep_vector(sigma_obs,n);',
                        gsub(' ', '', mod$model_file), fixed = TRUE)))

  # State-space gaussian
  mod <- mvgam(y ~ -1,
               trend_formula = ~ s(season, by = trend) + s(trend, bs = 're'),
               trend_model = RW(cor = TRUE),
               family = gaussian(),
               data = gaus_data$data_train,
               share_obs_params = TRUE,
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(any(grepl('real<lower=0>sigma_obs;',
                        gsub(' ', '', mod$model_file), fixed = TRUE)))
  expect_true(any(grepl('sigma_obs_vec[1:n,s]=rep_vector(sigma_obs,n);',
                        gsub(' ', '', mod$model_file), fixed = TRUE)))


  # Standard student
  mod <- mvgam(y ~ s(season, by = series),
               trend_model = RW(cor = TRUE),
               family = student_t(),
               data = gaus_data$data_train,
               share_obs_params = TRUE,
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(any(grepl('real<lower=0>sigma_obs;',
                        gsub(' ', '', mod$model_file), fixed = TRUE)))
  expect_true(any(grepl('sigma_obs_vec[1:n,s]=rep_vector(sigma_obs,n);',
                        gsub(' ', '', mod$model_file), fixed = TRUE)))

  # State-space student
  mod <- mvgam(y ~ -1,
               trend_formula = ~ s(season, by = trend),
               trend_model = RW(cor = TRUE),
               family = student_t(),
               data = gaus_data$data_train,
               share_obs_params = TRUE,
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(any(grepl('real<lower=0>sigma_obs;',
                        gsub(' ', '', mod$model_file), fixed = TRUE)))
  expect_true(any(grepl('sigma_obs_vec[1:n,s]=rep_vector(sigma_obs,n);',
                        gsub(' ', '', mod$model_file), fixed = TRUE)))

  # Standard lognormal
  simdat <- sim_mvgam(family = Gamma())
  mod <- mvgam(y ~ s(season, by = series),
               trend_model = RW(cor = TRUE),
               family = lognormal(),
               data = simdat$data_train,
               share_obs_params = TRUE,
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(any(grepl('real<lower=0>sigma_obs;',
                        gsub(' ', '', mod$model_file), fixed = TRUE)))
  expect_true(any(grepl('sigma_obs_vec[1:n,s]=rep_vector(sigma_obs,n);',
                        gsub(' ', '', mod$model_file), fixed = TRUE)))

  # State-space lognormal
  mod <- mvgam(y ~ -1,
               trend_formula = ~ s(season, by = trend),
               trend_model = RW(cor = TRUE),
               family = lognormal(),
               data = simdat$data_train,
               share_obs_params = TRUE,
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(any(grepl('real<lower=0>sigma_obs;',
                        gsub(' ', '', mod$model_file), fixed = TRUE)))
  expect_true(any(grepl('sigma_obs_vec[1:n,s]=rep_vector(sigma_obs,n);',
                        gsub(' ', '', mod$model_file), fixed = TRUE)))
  # Standard Gamma
  mod <- mvgam(y ~ s(season, by = series),
               trend_model = RW(cor = TRUE),
               family = Gamma(),
               data = simdat$data_train,
               share_obs_params = TRUE,
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(any(grepl('real<lower=0>shape;',
                        gsub(' ', '', mod$model_file), fixed = TRUE)))
  expect_true(any(grepl('shape_vec[1:n,s]=rep_vector(shape,n);',
                        gsub(' ', '', mod$model_file), fixed = TRUE)))

  # State-space Gamma
  mod <- mvgam(y ~ -1,
               trend_formula = ~ s(season, by = trend),
               trend_model = RW(cor = TRUE),
               family = Gamma(),
               data = simdat$data_train,
               share_obs_params = TRUE,
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(any(grepl('real<lower=0>shape;',
                        gsub(' ', '', mod$model_file), fixed = TRUE)))
  expect_true(any(grepl('shape_vec[1:n,s]=rep_vector(shape,n);',
                        gsub(' ', '', mod$model_file), fixed = TRUE)))
})

test_that("trend_map is behaving propoerly", {
  sim <- sim_mvgam(n_series = 3)
  mod_data <- sim$data_train
  trend_map <- data.frame(series = unique(mod_data$series),
                          trend = c(1,1,2))
  mod_map <- mvgam(y ~ s(season, bs = 'cc'),
                   trend_map = trend_map,
                   trend_model = AR(),
                   data = mod_data,
                   run_model = FALSE)

  expect_true(identical(mod_map$model_data$Z,
                        matrix(c(1,0,1,0,0,1), ncol = 2, byrow = TRUE)))
  expect_true(mod_map$use_lv)

  # Ill-specified trend_map
  trend_map <- data.frame(series = c('series_1', 'series_1','series_2'),
                          trend = c(1,1,2))
  expect_error(mvgam(y ~ s(season, bs = 'cc'),
                trend_map = trend_map,
                trend_model = AR(),
                data = mod_data,
                run_model = FALSE),
               'Argument "trend_map" must have an entry for every unique time series in "data"',
               fixed = TRUE)
})

test_that("models with only random effects should work without error", {
  sim <- sim_mvgam(n_series = 3)
  mod_data <- sim$data_train
  mod_map <- mvgam(y ~ s(series, bs = 're'),
                   data = mod_data,
                   run_model = FALSE)
  expect_true(inherits(mod_map, 'mvgam_prefit'))
})

test_that("models with only fs smooths should work without error", {
  sim <- sim_mvgam(n_series = 3)
  mod_data <- sim$data_train
  mod_map <- mvgam(y ~ s(season, series, bs = 'fs'),
                   data = mod_data,
                   run_model = FALSE)
  expect_true(inherits(mod_map, 'mvgam_prefit'))
})

test_that("trend_formula setup is working properly", {
  sim <- sim_mvgam(n_series = 3)
  mod_data <- sim$data_train
  mod_map <- mvgam(y ~ s(series, bs = 're'),
                   trend_formula = ~ s(season, bs = 'cc'),
                   trend_model = AR(),
                   data = mod_data,
                   run_model = FALSE)
  expect_true(identical(mod_map$model_data$Z,
                        matrix(c(1,0,0,0,1,0,0,0,1),
                               nrow = 3, byrow = TRUE)))
  expect_true(mod_map$use_lv)
  expect_true(!is.null(mod_map$trend_mgcv_model))
  expect_equal(colnames(model.frame(mod_map, trend_effects = TRUE)),
                        c('trend_y', 'season'))

  expect_equal(colnames(get_data(mod_map)),
               c('y', 'series', 'time', 'season'))

  expect_error(mvgam(y ~ 1,
                     trend_formula = 1 ~ s(series, bs = 're') +
                       s(season, bs = 'cc'),
                     trend_model = AR(),
                     data = mod_data,
                     run_model = FALSE),
               'Argument "trend_formula" should not have a left-hand side',
               fixed = TRUE)

  expect_error(mvgam(y ~ 1,
                     trend_formula = ~ s(series, bs = 're') + s(season, bs = 'cc'),
                     trend_model = AR(),
                     data = mod_data,
                     run_model = FALSE),
               'Argument "trend_formula" should not have the identifier "series" in it.\nUse "trend" instead for varying effects',
               fixed = TRUE)

})

# Check that parametric effect priors are properly incorporated in the
# model for a wide variety of model forms
test_that("parametric effect priors correctly incorporated in models", {
  mod_data <- mvgam:::mvgam_examp_dat
  mod_data$data_train$x1 <-
    rnorm(NROW(mod_data$data_train))
  mod_data$data_train$x2 <-
    rnorm(NROW(mod_data$data_train))
  mod_data$data_train$x3 <-
    rnorm(NROW(mod_data$data_train))

  # Observation formula; no trend
  mod <- mvgam(y ~ s(season) + series:x1 +
                 series:x2 + series:x3,
               trend_model = 'None',
               data = mod_data$data_train,
               family = gaussian(),
               run_model = FALSE)

  expect_true(any(grepl('// prior for seriesseries_2:x1...',
                        mod$model_file, fixed = TRUE)))
  expect_true(any(grepl('// prior for (Intercept)...',
                        mod$model_file, fixed = TRUE)))

  para_names <- paste0(paste0('// prior for seriesseries_', 1:2,
                              paste0(':x', 1:3, '...')))
  for(i in seq_along(para_names)){
    expect_true(any(grepl(para_names[i],
                          mod$model_file, fixed = TRUE)))
  }

  priors <- get_mvgam_priors(y ~ s(season) + series:x1 +
                               series:x2 + series:x3,
                             trend_model = 'None',
                             data = mod_data$data_train,
                             family = gaussian())
  expect_true(any(grepl('seriesseries_1:x2',
                        priors$param_name)))
  expect_true(any(grepl('seriesseries_2:x3',
                        priors$param_name)))


  # Observation formula; complex trend
  mod <- mvgam(y ~ s(season) + series:x1 + series:x2 + series:x3,
               trend_model = VAR(ma = TRUE),
               data = mod_data$data_train,
               family = gaussian(),
               run_model = FALSE)

  expect_true(any(grepl('// prior for seriesseries_2:x1...',
                        mod$model_file, fixed = TRUE)))
  expect_true(any(grepl('// prior for (Intercept)...',
                        mod$model_file, fixed = TRUE)))

  para_names <- paste0(paste0('// prior for seriesseries_', 1:2,
                              paste0(':x', 1:3, '...')))
  for(i in seq_along(para_names)){
    expect_true(any(grepl(para_names[i],
                          mod$model_file, fixed = TRUE)))
  }

  priors <- get_mvgam_priors(y ~ s(season) + series:x1 +
                               series:x2 + series:x3,
                             trend_model = VAR(ma = TRUE),
                             data = mod_data$data_train,
                             family = gaussian())
  expect_true(any(grepl('seriesseries_1:x2',
                        priors$param_name)))
  expect_true(any(grepl('seriesseries_2:x3',
                        priors$param_name)))

  # Trend formula; RW
  mod <- mvgam(y ~ 1,
               trend_formula = ~ s(season) + trend:x1 +
                 trend:x2 + trend:x3,
               trend_model = RW(),
               data = mod_data$data_train,
               noncentred = TRUE,
               family = gaussian(),
               run_model = FALSE)

  expect_true(any(grepl('// prior for (Intercept)...',
                        mod$model_file, fixed = TRUE)))

  para_names <- paste0(paste0('// prior for trendtrend', 1:2,
                              paste0(':x', 1:3, '_trend...')))
  for(i in seq_along(para_names)){
    expect_true(any(grepl(para_names[i],
                          mod$model_file, fixed = TRUE)))
  }

  priors <- get_mvgam_priors(y ~ 1,
                             trend_formula = ~ s(season) + trend:x1 +
                               trend:x2 + trend:x3,
                             trend_model = RW(),
                             data = mod_data$data_train,
                             family = gaussian())
  expect_true(any(grepl('trendtrend1:x1_trend',
                        priors$param_name)))
  expect_true(any(grepl('trendtrend2:x3_trend',
                        priors$param_name)))

  # Trend formula; VARMA
  mod <- mvgam(y ~ 1,
               trend_formula = ~ s(season) + trend:x1 + trend:x2 + trend:x3,
               trend_model = VAR(ma = TRUE),
               data = mod_data$data_train,
               family = gaussian(),
               run_model = FALSE)

  expect_true(any(grepl('// prior for (Intercept)...',
                        mod$model_file, fixed = TRUE)))

  para_names <- paste0(paste0('// prior for trendtrend', 1:2,
                              paste0(':x', 1:3, '_trend...')))
  for(i in seq_along(para_names)){
    expect_true(any(grepl(para_names[i],
                          mod$model_file, fixed = TRUE)))
  }

  priors <- get_mvgam_priors(y ~ 1,
                             trend_formula = ~ s(season) + trend:x1 + trend:x2 + trend:x3,
                             trend_model = RW(),
                             data = mod_data$data_train,
                             family = gaussian())
  expect_true(any(grepl('trendtrend1:x1_trend',
                        priors$param_name)))
  expect_true(any(grepl('trendtrend2:x3_trend',
                        priors$param_name)))
})

