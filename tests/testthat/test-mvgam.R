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
               'argument "rho" in dynamic() must be a positive value',
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
