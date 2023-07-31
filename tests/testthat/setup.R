# Setup models for tests
library("testthat")
library("mvgam")

expect_match2 <- function(object, regexp) {
  any(grepl(regexp, object, fixed = TRUE))
}

#### Fit two models for each testing combination to ensure
# Stan-based forecasts and mvgam-based forecasts are similar;
# use 1000 posterior samples for each chain so out of sample forecast
# scores can be compared with more precision ####
# Simple Gaussian models
set.seed(100)
gaus_data <- sim_mvgam(family = gaussian(),
                       T = 60,
                       trend_model = 'AR1',
                       seasonality = 'shared',
                       mu = c(-1, 0, 1),
                       trend_rel = 0.5,
                       prop_missing = 0.2)
gaus_ar1 <- mvgam(y ~ s(series, bs = 're') +
                    s(season, bs = 'cc') - 1,
                  trend_model = 'AR1',
                  data = gaus_data$data_train,
                  family = gaussian(),
                  samples = 300,
                  parallel = FALSE)
gaus_ar1fc <- mvgam(y ~ s(series, bs = 're') +
                      s(season, bs = 'cc') - 1,
                  trend_model = 'AR1',
                  data = gaus_data$data_train,
                  newdata = gaus_data$data_test,
                  family = gaussian(),
                  samples = 300,
                  parallel = FALSE)

# Simple Beta models
set.seed(100)
beta_data <- sim_mvgam(family = betar(),
                       trend_model = 'GP',
                       trend_rel = 0.5,
                       T = 60)
beta_gp <- mvgam(y ~ s(season, bs = 'cc'),
                  trend_model = 'GP',
                  data = beta_data$data_train,
                  family = betar(),
                 samples = 300,
                 parallel = FALSE)
beta_gpfc <- mvgam(y ~ s(season, bs = 'cc'),
                    trend_model = 'GP',
                    data = beta_data$data_train,
                    newdata = beta_data$data_test,
                    family = betar(),
                   samples = 300,
                   parallel = FALSE)
