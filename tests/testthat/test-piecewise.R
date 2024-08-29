context("piecewise")
# Simulate data from a piecewise logistic trend
ts <- mvgam:::piecewise_logistic(t = 1:100,
                                 cap = 8.5,
                                 deltas = extraDistr::rlaplace(10, 0, 0.025),
                                 k = 0.075,
                                 m = 0,
                                 changepoint_ts = sample(1:100, 10))
y <- rnorm(100, ts, 0.75)

# Don't put 'cap' variable in dataframe
df <- data.frame(y = y,
                 time = 1:100,
                 series = as.factor('series1'),
                 #cap = 8.75,
                 fake = rnorm(100))

test_that("logistic should error if cap is missing", {
  expect_error(mvgam(formula = y ~ 0,
                     data = df,
                     trend_model = PW(growth = 'logistic',
                                      n_changepoints = 10),
                     # priors = prior(normal(2, 5), class = k_trend),
                     family = gaussian(),
                     run_model = TRUE,
                     return_model_data = TRUE),
               'Capacities must be supplied as a variable named "cap" for logistic growth')
})

# Now include some missing values in 'cap'
df <- data.frame(y = y,
                 time = 1:100,
                 series = as.factor('series1'),
                 cap = sample(c(8.75, NA), 100, TRUE),
                 fake = rnorm(100))

test_that("logistic should error if cap has NAs", {
  expect_error(mvgam(formula = y ~ 0,
                     data = df,
                     trend_model = PW(growth = 'logistic',
                                      n_changepoints = 10),
                     priors = prior(normal(2, 5), class = k_trend),
                     family = gaussian(),
                     run_model = TRUE,
                     return_model_data = TRUE),
               'Missing values found for some "cap" terms')
})

# Missing values can also happen when transforming to the link scale
y <- rpois(100, ts + 5)
df <- data.frame(y = y,
                 time = 1:100,
                 series = as.factor('series1'),
                 cap = -1,
                 fake = rnorm(100))

test_that("logistic should error if cap has NAs after link transformation", {
  expect_error(mvgam(formula = y ~ 0,
                     data = df,
                     trend_model = PW(growth = 'logistic',
                                      n_changepoints = 10),
                     family = poisson(),
                     run_model = TRUE,
                     return_model_data = TRUE),
               paste0('Missing or infinite values found for some "cap" terms\n',
               'after transforming to the log link scale'))
})

# Make sure cap is in the right order
y <- rpois(100, ts + 5)
df <- rbind(data.frame(y = y,
                 time = 1:100,
                 series = as.factor('series1'),
                 cap = y + 20,
                 fake = rnorm(100)),
            data.frame(y = y + 2,
                       time = 1:100,
                       series = as.factor('series2'),
                       cap = y + 22,
                       fake = rnorm(100)))

test_that("logistic caps should be included in the correct order", {
  skip_on_cran()
  mod <- mvgam(formula = y ~ 0,
                     data = df,
                     trend_model = PW(growth = 'logistic',
                                      n_changepoints = 10),
                     family = poisson(),
                     run_model = FALSE,
                     return_model_data = TRUE)

  # caps should now be logged and in a matrix [1:n_timepoints, 1:n_series]
  expect_true(all(mod$model_data$cap ==
        log(cbind(df %>%
                    dplyr::filter(series == 'series1') %>%
                    dplyr::arrange(time) %>%
                    dplyr::pull(cap),
                  df %>%
                    dplyr::filter(series == 'series2') %>%
                    dplyr::arrange(time) %>%
                    dplyr::pull(cap)))))
})

test_that("piecewise models fit and forecast without error", {
  skip_on_cran()
  # Example of logistic growth with possible changepoints
  # Simple logistic growth model
  dNt = function(r, N, k){
    r * N * (k - N)
  }

  # Iterate growth through time
  Nt = function(r, N, t, k) {
    for (i in 1:(t - 1)) {

      # population at next time step is current population + growth,
      # but we introduce several 'shocks' as changepoints
      if(i %in% c(5, 15, 25, 41, 45, 60, 80)){
        N[i + 1] <- max(1, N[i] + dNt(r + runif(1, -0.1, 0.1),
                                      N[i], k))
      } else {
        N[i + 1] <- max(1, N[i] + dNt(r, N[i], k))
      }
    }
    N
  }

  # Simulate expected values
  set.seed(555)
  expected <- Nt(0.004, 2, 100, 30)

  # Take Poisson draws
  y <- rpois(100, expected)

  # Assemble data into dataframe and model. We set a
  # fixed carrying capacity of 35 for this example, but note that
  # this value is not required to be fixed at each timepoint
  mod_data <- data.frame(y = y,
                         time = 1:100,
                         cap = 35,
                         series = as.factor('series_1'))
  dat_train <- mod_data %>%
    dplyr::filter(time <= 90)
  dat_test <- mod_data %>%
    dplyr::filter(time > 90)

  # The intercept is nonidentifiable when using piecewise
  # trends because the trend functions have their own offset
  # parameters 'm'; it is recommended to always drop intercepts
  # when using these trend models
  mod <- mvgam(y ~ 0,
               trend_model = PW(growth = 'logistic'),
               family = poisson(),
               data = dat_train,
               chains = 2,
               silent = 2)
  # Compute and plot forecasts
  fc <- forecast(mod, newdata = dat_test, type = 'trend')
  expect_no_error(capture_output(plot(fc)))

  # Should also work for piecewise linear
  mod <- SW(mvgam(y ~ 0,
               trend_model = PW(growth = 'linear',
                                n_changepoints = 5),
               family = poisson(),
               data = dat_train,
               chains = 2,
               silent = 2))
  # Compute and plot forecasts
  fc <- forecast(mod, newdata = dat_test, type = 'trend')
  expect_no_error(capture_output(plot(fc)))
})
