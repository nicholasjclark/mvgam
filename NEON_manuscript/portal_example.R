# Load Portal rodents data
portal_dat <- read.csv('NEON_manuscript/rodents_data.csv', as.is = T)

# Function to get Dunn-Smyth residuals
ds_resids = function(truth, fitted, size){
  dsres_out <- matrix(NA, length(truth), 1)
  for(i in 1:length(truth)){
    a <- pnbinom(as.vector(truth[i]) - 1, mu = fitted[i], size = size)
    b <- pnbinom(as.vector(truth[i]), mu = fitted[i], size = size)
    u <- runif(n = 1, min = a, max = b)
    dsres_out[i, ] <- qnorm(u)
  }
  resids <- dsres_out[,1]
  resids[is.infinite(resids)] <- NaN
  resids
}

# Convert PP catches to timeseries and then to mvgam format
catches <- portal_dat$PP
plot(catches, type = 'l')

# Keep only data from the year 2004 onward for this example to make the series more manageable
library(mvgam)
library(forecast)
series <- xts::as.xts(subset(ts(catches, start = c(1979, 9), frequency = 12),
                             start = c(293)))
head(series)
colnames(series) <- c('PP')
PP_data <- series_to_mvgam(series, freq = 12, train_prop = 0.9)
rm(series, portal_dat)

# Fit standard mgcv GAM to the training data using an AR model for the residuals
# and evaluate Dunn-Smyth residuals for any remaining autocorrelation
library(mgcv)
PP_data$data_train$time <- seq(1:NROW(PP_data$data_train))
gam_mod <- gamm(y ~ s(season, bs = 'cc', k = 8) +
                 s(year, bs = 'gp', k = 4) + ti(season, year),
               data = PP_data$data_train,
               family = 'poisson', correlation = corAR1(form = ~ time))
summary(gam_mod$gam)

# From the gamm help file, we can extract model residuals from the LME component
# Predicted values
fv <- exp(fitted(gam_mod$lme))

# Dunn-Smyth residuals (use large size to approximate Poisson)
gam_resids <- ds_resids(truth = gam_mod$gam$y,
                        fitted = fv,
                        size = 1000)
acf(gam_resids, na.action = na.pass)

# Which kind of model might be suitable for the trend?
plot(gam_resids, type = 'l')
forecast::auto.arima(gam_resids)

## Pearson residuals
gam_pearson_resids <- (gam_mod$gam$y - fv)/sqrt(fv)
acf(gam_pearson_resids)

## STRONG AUTOCORRELATION, REGARDLESS OF THE AR1 TERM OR CHOICE OF RESIDUALS

# gam.check always asks for larger K for the year term, which suggests it wants to model
# the autocorrelation
gam.check(gam_mod$gam)

# Equivalent model using mvgam's dynamic trend component
PP_data$data_train$time <- NULL
df_gam_mod <- mvjagam(data_train = PP_data$data_train,
                      data_test = PP_data$data_test,
               formula = y ~ s(season, bs = c('cc'), k = 8) +
                 s(year, bs = 'gp', k = 4) + ti(season, year),
               trend_model = 'AR1',
               use_nb = T,
               n.burnin = 5000,
               n.iter = 1000,
               thin = 1,
               auto_update = F,
               upper_bounds = 100)

# ACF of DS residuals shows no remaining autocorrelation
acf(df_gam_mod$resids$PP, na.action = na.pass)

# Smooth function plots
plot(gam_mod$gam, select = 1)
plot_mvgam_smooth(df_gam_mod, 1, 'season')
plot(gam_mod$gam, select = 2)
plot_mvgam_smooth(df_gam_mod, 1, 'year')

# Forecasts for the mgcv model
Xp <- predict(gam_mod$gam, newdata = PP_data$data_test, type = 'lpmatrix')
vc <- vcov(gam_mod$gam)
sim <- MASS::mvrnorm(dim(MCMCvis::MCMCchains(df_gam_mod$jags_output, 'ypred'))[1],
                     mu = coef(gam_mod$gam), Sigma = vc)
dims_needed <- dim(exp(Xp %*% t(sim)))
mus <- as.vector(exp(Xp %*% t(sim)))
fits <- rpois(prod(dims_needed), lambda = mus)
fits <- t(matrix(fits, nrow = dims_needed[1], ncol = dims_needed[2]))
cred_ints <- apply(fits, 2, hpd)
plot(cred_ints[3,] ~ seq(1:NCOL(cred_ints)), type = 'l',
     ylim = c(0, max(cred_ints)))
lines(cred_ints[1,]~ seq(1:NCOL(cred_ints)))
cred_ints <- apply(fits, 2, function(x) hpd(x, 0.68))
lines(cred_ints[3,]~ seq(1:NCOL(cred_ints)), col = 'red')
lines(cred_ints[1,]~ seq(1:NCOL(cred_ints)), col = 'red')
points(PP_data$data_test$y)

# Forecast and trend for the dynamic gam model
plot_mvgam_fc(df_gam_mod, 1, data_test = PP_data$data_test)
plot_mvgam_trend(df_gam_mod, 1, data_test = PP_data$data_test)

# Initiate particles by assimilating the next observation in data_test
pfilter_mvgam_init(object = df_gam_mod, n_particles = 20000, n_cores = 3,
                   data_assim = PP_data$data_test)

# Assimilate some of the next observations
pfilter_mvgam_online(data_assim = PP_data$data_test[1:9,], n_cores = 5,
                     kernel_lambda = 1)

# Forecast from particles using the covariate information in remaining data_test observations
fc <- pfilter_mvgam_fc(file_path = 'pfilter', n_cores = 3,
                       data_test = PP_data$data_test, ylim = c(0, 100))
par(mfrow=c(1,2))
plot_mvgam_fc(df_gam_mod, 1, data_test = PP_data$data_test, ylim = c(0, 100))
fc$PP()
