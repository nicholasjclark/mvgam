context("RW,AR,VAR,CAR")

skip_on_cran()

test_that("ma and cor options should work for trends other than VAR", {
  test <- mvgam(y ~ s(series, bs = 're') +
                       s(season, bs = 'cc') - 1,
                     trend_model = AR(p = 1, ma = TRUE),
                     data = gaus_data$data_train,
                     family = gaussian(),
                     run_model = FALSE)
  expect_true(inherits(test, 'mvgam_prefit'))
  expect_true(any(grepl('vector<lower=-1,upper=1>[n_series]theta;',
                        gsub(' ', '', test$model_file), fixed = TRUE)))
  expect_true(attr(test$model_data, 'trend_model') == 'AR1')

  # Drift terms allowed
  test <- mvgam(y ~ s(series, bs = 're') +
                  s(season, bs = 'cc') - 1,
                trend_model = AR(p = 1, ma = TRUE),
                drift = TRUE,
                data = gaus_data$data_train,
                family = gaussian(),
                run_model = FALSE)
  expect_true(inherits(test, 'mvgam_prefit'))
  expect_true(any(grepl('vector<lower=-1,upper=1>[n_series]theta;',
                        gsub(' ', '', test$model_file), fixed = TRUE)))
  expect_true(attr(test$model_data, 'trend_model') == 'AR1')
  expect_true(any(grepl('drift~std_normal();',
                        gsub(' ', '', test$model_file), fixed = TRUE)))
  expect_true(any(grepl('vector[n_series]drift;',
                        gsub(' ', '', test$model_file), fixed = TRUE)))
  expect_true(test$drift)

  test <- mvgam(y ~ s(series, bs = 're') +
                       s(season, bs = 'cc') - 1,
                     trend_model = AR(p = 1, cor = TRUE),
                     data = gaus_data$data_train,
                     family = gaussian(),
                     run_model = FALSE)
  expect_true(inherits(test, 'mvgam_prefit'))
  expect_true(any(grepl('error[i]~multi_normal_cholesky(trend_zeros,L_Sigma);',
                        gsub(' ', '', test$model_file), fixed = TRUE)))
  expect_true(attr(test$model_data, 'trend_model') == 'AR1')

  test <- mvgam(y ~ s(series, bs = 're') +
                       s(season, bs = 'cc') - 1,
                     trend_model = RW(ma = TRUE),
                     data = gaus_data$data_train,
                     family = gaussian(),
                     run_model = FALSE)
  expect_true(inherits(test, 'mvgam_prefit'))
  expect_true(any(grepl('vector<lower=-1,upper=1>[n_series]theta;',
                        gsub(' ', '', test$model_file), fixed = TRUE)))
  expect_true(attr(test$model_data, 'trend_model') == 'RW')

  test <- mvgam(y ~ s(series, bs = 're') +
                       s(season, bs = 'cc') - 1,
                     trend_model = RW(cor = TRUE),
                     data = gaus_data$data_train,
                     family = gaussian(),
                     run_model = FALSE)
  expect_true(inherits(test, 'mvgam_prefit'))
  expect_true(any(grepl('error[i]~multi_normal_cholesky(trend_zeros,L_Sigma);',
                        gsub(' ', '', test$model_file), fixed = TRUE)))
  expect_true(attr(test$model_data, 'trend_model') == 'RW')
})

test_that("VARMAs are set up correctly", {
  var <- mvgam(y ~ s(series, bs = 're') +
                   s(season, bs = 'cc') - 1,
                 trend_model = VAR(),
                 data = gaus_data$data_train,
                 family = gaussian(),
                 run_model = FALSE)
  expect_true(inherits(var, 'mvgam_prefit'))

  var <- mvgam(y ~ s(series, bs = 're') +
                 gp(time, c = 5/4, k = 20) - 1,
               trend_model = VAR(),
               data = gaus_data$data_train,
               family = gaussian(),
               run_model = FALSE)
  expect_true(inherits(var, 'mvgam_prefit'))

  varma <- mvgam(y ~ s(series, bs = 're') +
                   s(season, bs = 'cc') - 1,
                 trend_model = 'VARMA',
                 data = gaus_data$data_train,
                 family = gaussian(),
                 run_model = FALSE)

  expect_true(any(grepl('// unconstrained ma inverse partial autocorrelations',
                        varma$model_file, fixed = TRUE)))

  varma <- mvgam(y ~ s(series, bs = 're'),
                 trend_formula = ~ gp(time, by = trend, c = 5/4),
                 trend_model = VAR(ma = TRUE),
                 data = gaus_data$data_train,
                 family = gaussian(),
                 run_model = FALSE)

  expect_true(any(grepl('// unconstrained ma inverse partial autocorrelations',
                        varma$model_file, fixed = TRUE)))

  expect_error(mvgam(y ~ s(series, bs = 're'),
                 trend_formula = ~ s(season, bs = 'cc'),
                 trend_model = VAR(ma = TRUE),
                 drift = TRUE,
                 data = gaus_data$data_train,
                 family = gaussian(),
                 run_model = FALSE),
               'drift terms not allowed for VAR or GP models')
})

# Replicate CAR1 example
# Function to simulate CAR1 data with seasonality
sim_corcar1 = function(n = 120,
                       phi = 0.5,
                       sigma = 1,
                       sigma_obs = 0.75){
# Sample irregularly spaced time intervals
time_dis <- c(0, runif(n - 1, -0.1, 1))
time_dis[time_dis < 0] <- 0; time_dis <- time_dis * 5

# Set up the latent dynamic process
x <- vector(length = n); x[1] <- -0.3
for(i in 2:n){
  # zero-distances will cause problems in sampling, so mvgam uses a
  # minimum threshold; this simulation function emulates that process
  if(time_dis[i] == 0){
    x[i] <- rnorm(1, mean = (phi ^ 1e-12) * x[i - 1], sd = sigma)
   } else {
     x[i] <- rnorm(1, mean = (phi ^ time_dis[i]) * x[i - 1], sd = sigma)
   }
 }

# Add 12-month seasonality
 cov1 <- sin(2 * pi * (1 : n) / 12); cov2 <- cos(2 * pi * (1 : n) / 12)
 beta1 <- runif(1, 0.3, 0.7); beta2 <- runif(1, 0.2, 0.5)
 seasonality <- beta1 * cov1 + beta2 * cov2

# Take Gaussian observations with error and return
 data.frame(y = rnorm(n, mean = x + seasonality, sd = sigma),
            season = rep(1:12, 20)[1:n],
            time = cumsum(time_dis))
}

# Sample two time series
dat <- rbind(dplyr::bind_cols(sim_corcar1(phi = 0.65,
                                          sigma_obs = 0.55),
                              data.frame(series = 'series1')),
             dplyr::bind_cols(sim_corcar1(phi = 0.8,
                              sigma_obs = 0.35),
                              data.frame(series = 'series2'))) %>%
       dplyr::mutate(series = as.factor(series))

test_that("CAR1 sets up correctly", {
  # mvgam with CAR(1) trends and series-level seasonal smooths
  mod <- mvgam(formula = y ~ s(season, bs = 'cc',
                               k = 5, by = series),
               trend_model = CAR(),
               data = dat,
               family = gaussian(),
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(exists('time_dis', mod$model_data))
  expect_true(exists('index..time..index', mod$obs_data))
  expect_true(attr(mod$model_data, 'trend_model') == 'CAR1')
  expect_true(any(grepl('vector<lower=0,upper=1.5>[n_series]ar1;',
                        gsub(' ', '', mod$model_file), fixed = TRUE)))

  # Drift terms allowed
  mod <- mvgam(formula = y ~ s(season, bs = 'cc',
                               k = 5, by = series),
               trend_model = CAR(),
               drift = TRUE,
               data = dat,
               family = gaussian(),
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(exists('time_dis', mod$model_data))
  expect_true(exists('index..time..index', mod$obs_data))
  expect_true(attr(mod$model_data, 'trend_model') == 'CAR1')
  expect_true(any(grepl('drift~std_normal();',
                        gsub(' ', '', mod$model_file), fixed = TRUE)))
  expect_true(any(grepl('vector[n_series]drift;',
                        gsub(' ', '', mod$model_file), fixed = TRUE)))
  expect_true(mod$drift)

  # Will work for regularly-spaced data as well
  mod <- mvgam(formula = y ~ s(season, bs = 'cc',
                               k = 5, by = series),
               trend_model = CAR(),
               data = gaus_data$data_train,
               family = gaussian(),
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(exists('time_dis', mod$model_data))
  expect_true(all.equal(mean(mod$model_data$time_dis[2:NROW(mod$model_data$time_dis),]),
                        max(mod$model_data$time_dis[2:NROW(mod$model_data$time_dis),]),
                        min(mod$model_data$time_dis[2:NROW(mod$model_data$time_dis),]),
                        1L))
  expect_true(exists('index..time..index', mod$obs_data))
  expect_true(attr(mod$model_data, 'trend_model') == 'CAR1')
})
