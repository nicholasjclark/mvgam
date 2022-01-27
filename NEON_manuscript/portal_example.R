# A distributed lag example, using the Portal capture data
# Load Portal rodents data
library(mvgam)
library(forecast)
library(mgcv)
portal_dat <- read.csv('NEON_manuscript/rodents_data.csv', as.is = T)

# Keep data from 2004 onwards to make models quicker to estimate
portal_dat %>%
  dplyr::filter(year >= 2004) %>%
  dplyr::group_by(year, month) %>%
  dplyr::slice_head(n = 1) -> portal_dat_all

# Slight modifiction of Wood's lag matrix function by using the means
# for any missing (should only impact the very first observations of the lag
# matrix and won't make much impact on the estimates)
lagard <- function(x,n.lag=6) {
  n <- length(x); X <- matrix(NA,n,n.lag)
  for (i in 1:n.lag) X[i:n,i] <- x[i:n-i+1]
  X[is.na(X)] <- mean(X, na.rm = T)
  X
}

# Organise all data needed for modelling into a list
data_all <- list(lag=matrix(0:5,nrow(portal_dat_all),6,byrow=TRUE),
            y = portal_dat_all$PP,
            season = portal_dat_all$month,
            year = portal_dat_all$year,
            series = rep(as.factor('series1'), nrow(portal_dat_all)))
data_all$precip <- lagard(portal_dat_all$precipitation)
data_all$mintemp <- lagard(portal_dat_all$mintemp)

# Create training and testing sets
data_train <- list(lag = data_all$lag[1:174,],
                   y = data_all$y[1:174],
                   series = data_all$series[1:174],
                   season = data_all$season[1:174],
                   year = data_all$year[1:174],
                   precip = data_all$precip[1:174,],
                   mintemp = data_all$mintemp[1:174,])
data_test <- list(lag = data_all$lag[175:length(data_all$y),],
                   y = data_all$y[175:length(data_all$y)],
                   series = data_all$series[175:length(data_all$y)],
                   season = data_all$season[175:length(data_all$y)],
                   year = data_all$year[175:length(data_all$y)],
                   precip = data_all$precip[175:length(data_all$y),],
                  mintemp = data_all$mintemp[175:length(data_all$y),])

# Fit a dynamic GAM with a distributed lag term for precipitation and
# an AR1 process for the trend
test <- mvjagam(formula =  y ~ s(season, bs = "cc", k = 12) +
                   te(precip, lag, k = c(8, 3)),
                 knots = list(season = c(0.5, 12.5)),
                 data_train = data_train,
                 data_test = data_test,
                 family = 'poisson',
                 n.burnin = 5000,
                 trend_model = 'AR1',
                 auto_update = F)

plot_mvgam_fc(test, series = 1, data_test = data_test)

