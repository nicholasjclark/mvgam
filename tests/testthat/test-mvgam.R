context("mvgam")

test_that("family must be correctly specified", {
  expect_error(mod <- mvgam(y ~ s(season),
                            trend_model = 'AR1',
                            data = beta_data$data_train,
                            family = 'besta',
                            run_model = FALSE),
               'family not recognized')
})

test_that("response variable must be specified", {
expect_error(mod <- mvgam( ~ s(season),
                          trend_model = 'AR1',
                          data = beta_data$data_train,
                          family = 'besta',
                          run_model = FALSE),
             'response variable is missing from formula')
})

test_that("response variable must follow family-specific restrictions", {
  expect_error(mod <- mvgam(y ~ s(season),
                             trend_model = 'AR1',
                             data = gaus_data$data_train,
                             family = lognormal(),
                             run_model = FALSE),
               'Values <= 0 not allowed for lognormal responses')

  expect_error(mod <- mvgam(y ~ s(season),
                            trend_model = 'AR1',
                            data = gaus_data$data_train,
                            family = poisson(),
                            run_model = FALSE),
               'Values < 0 not allowed for poisson responses')
})

test_that("trend_model must be correctly specified", {
  expect_error(mod <- mvgam(y ~ s(season),
                            trend_model = 'AR11',
                            data = beta_data$data_train,
                            family = betar(),
                            run_model = FALSE))
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

test_that("all series must have observations for all unique timepoints", {
  data <- sim_mvgam()
  data$data_train <- data$data_train[-2,]
  expect_error(mod <- mvgam(y ~ s(season),
                            trend_model = 'AR1',
                            data = data$data_train,
                            family = poisson(),
                            run_model = FALSE),
               'One or more series in data is missing observations for one or more timepoints')

  data <- sim_mvgam()
  data$data_test <- data$data_test[-2,]
  expect_error(mod <- mvgam(y ~ s(season),
                            trend_model = 'AR1',
                            data = data$data_train,
                            newdata = data$data_test,
                            family = poisson(),
                            run_model = FALSE),
               'One or more series in newdata is missing observations for one or more timepoints')
})

test_that("median coefs should be stored in the mgcv object", {
  expect_true(identical(unname(coef(gaus_ar1$mgcv_model)),
            coef(gaus_ar1)[,2]))
})

test_that("missing values not allowed in predictors", {
  # Include missing vals in training data
  simdat <- sim_mvgam()
  simdat$data_train$season[4] <- NA
  expect_error(mvgam(y ~ s(season),
                     trend_model = 'GP',
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

test_that("series levels must match unique entries in series", {
  # Include missing vals in training data
  simdat <- sim_mvgam()
  levels(simdat$data_train$series) <- paste0('series_', 1:6)
  expect_error(mvgam(y ~ s(season),
                     trend_model = 'GP',
                     data = simdat$data_train,
                     newdata = simdat$data_test,
                     run_model = FALSE))
})

test_that("trend_map is behaving propoerly", {
  sim <- sim_mvgam(n_series = 3)
  mod_data <- sim$data_train
  trend_map <- data.frame(series = unique(mod_data$series),
                          trend = c(1,1,2))
  mod_map <- mvgam(y ~ s(season, bs = 'cc'),
                   trend_map = trend_map,
                   trend_model = 'AR1',
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
                trend_model = 'AR1',
                data = mod_data,
                run_model = FALSE),
               'Argument "trend_map" must have an entry for every unique time series in "data"',
               fixed = TRUE)
})

test_that("trend_formula setup is working properly", {
  sim <- sim_mvgam(n_series = 3)
  mod_data <- sim$data_train
  mod_map <- mvgam(y ~ s(series, bs = 're'),
                   trend_formula = ~ s(season, bs = 'cc'),
                   trend_model = 'AR1',
                   data = mod_data,
                   run_model = FALSE)
  expect_true(identical(mod_map$model_data$Z,
                        matrix(c(1,0,0,0,1,0,0,0,1), nrow = 3, byrow = TRUE)))
  expect_true(mod_map$use_lv)
  expect_true(!is.null(mod_map$trend_mgcv_model))
  expect_equal(colnames(model.frame(mod_map, trend_effects = TRUE)),
                        c('trend_y', 'season'))

  expect_equal(colnames(get_data(mod_map)),
               c('y', 'series', 'time', 'season'))

  expect_error(mvgam(y ~ 1,
                     trend_formula = 1 ~ s(series, bs = 're') + s(season, bs = 'cc'),
                     trend_model = 'AR1',
                     data = mod_data,
                     run_model = FALSE),
               'Argument "trend_formula" should not have a left-hand side',
               fixed = TRUE)

  expect_error(mvgam(y ~ 1,
                     trend_formula = ~ s(series, bs = 're') + s(season, bs = 'cc'),
                     trend_model = 'AR1',
                     data = mod_data,
                     run_model = FALSE),
               'Argument "trend_formula" should not have the identifier "series" in it.\nUse "trend" instead for varying effects',
               fixed = TRUE)

})



