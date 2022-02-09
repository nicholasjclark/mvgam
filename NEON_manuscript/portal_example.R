# A distributed lag example, using the Portal capture data
# Load Portal rodents data (change to your wd if needed)
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

# Create training and testing sets; start at observation 7 so that the mean-imputed
# lags are not included
data_train <- list(lag = data_all$lag[7:174,],
                   y = data_all$y[7:174],
                   series = data_all$series[7:174],
                   season = data_all$season[7:174],
                   year = data_all$year[7:174],
                   precip = data_all$precip[7:174,],
                   mintemp = data_all$mintemp[7:174,])
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
                  te(mintemp, lag, k = c(8, 4)) +
                  te(precip, lag, k = c(8, 4)),
                knots = list(season = c(0.5, 12.5)),
                data_train = data_train,
                data_test = data_test,
                family = 'poisson',
                chains = 4,
                burnin = 2000,
                trend_model = 'AR1')

plot_mvgam_fc(test, series = 1, data_test = data_test, ylim = c(0, 100))
plot_mvgam_trend(test, series = 1, data_test = data_test)
plot_mvgam_uncertainty(test, series = 1, data_test = data_test)
plot_mvgam_smooth(test, series = 1, smooth ='season')
plot_mvgam_smooth(test, series = 1, smooth = 2)
plot_mvgam_smooth(test, series = 1, smooth = 3)
plot_mvgam_trace(test, 'trend')
summary_mvgam(test)

# For visualising how covariate functions change with different lags, use
# the predict_mvgam function
# Set up prediction data by zeroing out all covariates apart from the covariate of
# interest
newdata <- data_test
newdata$year <- rep(0, length(newdata$year))
newdata$season <- rep(0, length(newdata$season))
newdata$precip <- matrix(0, ncol = ncol(newdata$precip),
                         nrow = nrow(newdata$precip))

# Set up plot colours and initiate plot window
cols <- viridis::inferno(5)
plot(1, type = "n",
     xlab = 'Mintemp',
     ylab = 'Predicted response function',
     xlim = c(min(data_train$mintemp), max(data_train$mintemp)),
     ylim = c(-0.6, 0.6))

for(i in 1:5){
  # Set up prediction matrix for mintemp with lag i as the prediction sequence
  newdata$mintemp <- matrix(0, ncol = ncol(newdata$precip),
                            nrow = nrow(newdata$precip))
  newdata$mintemp[,i] <- seq(min(data_train$mintemp),
                             max(data_train$mintemp),
                             length.out = length(newdata$year))

  # Predict on the link scale (intercept still included in prediction by default)
  preds <- predict_mvgam(test, series = 1, newdata = newdata, type = 'link')

  # Calculate prediction quantiles
  probs = c(0.05, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.95)
  cred <- sapply(1:NCOL(preds),
                 function(n) quantile(preds[,n],
                                      probs = probs))

  # Plot expected function posterior median in varying colours per lag; subtract out the
  # intercept so that the function is roughly centred on zero
  pred_upper <- cred[4,] - test$mgcv_model$coefficients[1]
  pred_lower <- cred[6,] - test$mgcv_model$coefficients[1]
  pred_vals <- seq(min(data_train$mintemp),
                   max(data_train$mintemp),
                   length.out = length(newdata$year))
  polygon(c(pred_vals, rev(pred_vals)), c(pred_upper, rev(pred_lower)),
          col = scales::alpha(cols[i], 0.6), border = scales::alpha(cols[i], 0.7))
  lines(pred_vals, cred[5,] - test$mgcv_model$coefficients[1],
        col = scales::alpha(cols[i], 0.8), lwd = 2.5)
}
abline(h = 0, lty = 'dashed')
legend('topleft',legend=paste0('lag', seq(0, 4)),
       bg = 'white', bty = 'n',
       col=cols,lty=1,lwd=6)


# Initiate particle filter
pfilter_mvgam_init(test, data_assim = data_test)

# Get next two observations for assimilation
data_assim <- lapply(data_test, function(x){
  if(is.matrix(x)){
    matrix(x[1:3,], ncol = NCOL(x))
  } else {
    x[1:3]
  }
})
names(data_assim) <- names(data_test)

# Assimilate
pfilter_mvgam_online(data_assim = data_assim,
                     n_cores = 2,
                     kernel_lambda = 1)

# Forecast
fc <- pfilter_mvgam_fc(file_path = 'pfilter', n_cores = 2,
                       data_test = data_test, ylim = c(0, 100))

par(mfrow = c(1,2))
plot_mvgam_fc(test, series = 1, data_test = data_test, ylim = c(0, 100))
fc$series1()
