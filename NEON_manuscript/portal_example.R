# Load Portal rodents data
library(mvgam)
library(forecast)
library(mgcv)
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

# Keep only data from the year 2004 onward for this example to make the series more manageable
series <- xts::as.xts(subset(ts(catches, start = c(1979, 9), frequency = 12),
                             start = c(293)))
head(series)
colnames(series) <- c('PP')
plot(series)
PP_data <- series_to_mvgam(series, freq = 12, train_prop = 0.9)
rm(series, portal_dat)

# Fit standard GAM to the training data using an AR3 model for the residuals.
# For a GAMM model, we cannot use negative binomial so will stick with Poisson (even though
# a negative binomial is likely more appropriate here). We add a 'time' index for the
# form of the residual autocorrelation and use a 1st derivative penalty for the year effect
# to prevent linear extrapolation of the yearly trend when forecasting
PP_data$data_train$time <- seq(1:NROW(PP_data$data_train))
gam_mod <- gamm(y ~ s(season, bs = 'cc', k = 8) +
                 s(year, bs = 'gp', k = 4, m = 1) + ti(season, year,
                                                       m = c(2, 1)),
               data = PP_data$data_train,
               family = 'poisson', correlation = corARMA(form = ~ time, p = 3))
summary(gam_mod$gam)

# Evaluate Dunn-Smyth residuals for any remaining autocorrelation
# From the gamm help file, we can extract model residuals using fits from the LME component
fv <- exp(fitted(gam_mod$lme))

# Dunn-Smyth residuals (use large size to approximate Poisson)
gam_resids <- ds_resids(truth = gam_mod$gam$y,
                        fitted = fv,
                        size = 1000)
acf(gam_resids, na.action = na.pass)

# Standardized residuals also show autocorrelation
acf(residuals(gam_mod$lme,type="normalized"),main="standardized residual ACF")

# Which kind of model might be suitable for the trend?
plot(residuals(gam_mod$lme,type="normalized"), type = 'l')
forecast::auto.arima(gam_resids)

# DS and standardized residuals both still show strong autocorrelation. Do Pearson residuals?
gam_pearson_resids <- (gam_mod$gam$y - fv)/sqrt(fv)
acf(gam_pearson_resids)
forecast::auto.arima(gam_pearson_resids)

# Strong autocorrelation remainsin both sets, not clear how this impacts inference but it violates
# model assumptions. As expected, gam.check always asks for larger K for the year terms,
# which also highlights existing autocorrelation
gam.check(gam_mod$gam)

# Now a model using mvgam
# Note that
# the model will predict for the data_test observations by considering the outcomes as
# missing for these observations. We also incorporate prior knowledge about upper bounds on
# this series, which exist due to limits on the number of traps used in each trapping session
PP_data$data_train$time <- NULL
df_gam_mod <- mvjagam(data_train = PP_data$data_train,
                      data_test = PP_data$data_test,
               formula = y ~ s(season, bs = c('cc'), k = 12) +
                 s(year, bs = 'gp', k = 3, m = 1) +
                 ti(season, year, m = c(2, 1)),
               trend_model = 'AR3',
               family = 'nb',
               n.burnin = 1000,
               auto_update = F,
               upper_bounds = 100)
summary_mvgam(df_gam_mod)
summary(df_gam_mod$mgcv_model)

# ACF of DS residuals shows no remaining autocorrelation
plot_mvgam_resids(df_gam_mod, 1)

# Smooth function plots from each model
par(mfrow=c(1,2))
plot(gam_mod$gam, select = 1)
plot_mvgam_smooth(df_gam_mod, 1, 'season')
plot(gam_mod$gam, select = 2)
plot_mvgam_smooth(df_gam_mod, 1, 'year')
par(mfrow=c(1,1))

# Plot forecast for the next year from the mgcv model against the test data
Xp <- predict(gam_mod$gam, newdata = PP_data$data_test[1:12,], type = 'lpmatrix')
vc <- vcov(gam_mod$gam)
sim <- MASS::mvrnorm(dim(MCMCvis::MCMCchains(df_gam_mod$jags_output, 'ypred'))[1],
                     mu = coef(gam_mod$gam), Sigma = vc)
dims_needed <- dim(exp(Xp %*% t(sim)))
mus <- as.vector(exp(Xp %*% t(sim)))
fits <- rpois(prod(dims_needed), lambda = mus)
fits <- t(matrix(fits, nrow = dims_needed[1], ncol = dims_needed[2]))

# Plotting 90% and 68% HPD intervals
par(mfrow=c(1,2))
cred_ints <- apply(fits, 2, function(x) hpd(x, 0.9))
yupper <- 100
plot(cred_ints[3,] ~ seq(1:NCOL(cred_ints)), type = 'l',
     col = rgb(1,0,0, alpha = 0),
     ylim = c(0, yupper),
     ylab = 'Predicted counts of PP',
     xlab = 'Forecast horizon',
     main = 'mgcv')
polygon(c(seq(1:(NCOL(cred_ints))), rev(seq(1:NCOL(cred_ints)))),
        c(cred_ints[1,],rev(cred_ints[3,])),
        col = rgb(150, 0, 0, max = 255, alpha = 100), border = NA)
cred_ints <- apply(fits, 2, function(x) hpd(x, 0.68))
polygon(c(seq(1:(NCOL(cred_ints))), rev(seq(1:NCOL(cred_ints)))),
        c(cred_ints[1,],rev(cred_ints[3,])),
        col = rgb(150, 0, 0, max = 255, alpha = 180), border = NA)
lines(cred_ints[2,], col = rgb(150, 0, 0, max = 255), lwd = 2, lty = 'dashed')
points(PP_data$data_test$y[1:12], pch = 16)

# The mgcv model overpredicts for the out of sample test set and has unreasonably narrow
# prediction intervals, though admittedly I don't know how to easily
# incorporate the autocorrelation component into this forecast.

# Forecast for the dynamic gam model on the same y-axis scale
fits <- MCMCvis::MCMCchains(df_gam_mod$jags_output, 'ypred')
fits <- fits[,(NROW(df_gam_mod$obs_data)+1):(NROW(df_gam_mod$obs_data)+13)]
cred_ints <- apply(fits, 2, function(x) hpd(x, 0.9))
plot(cred_ints[3,] ~ seq(1:NCOL(cred_ints)), type = 'l',
     col = rgb(1,0,0, alpha = 0),
     ylim = c(0, yupper),
     ylab = 'Predicted counts of PP',
     xlab = 'Forecast horizon',
     main = 'df_gam')
polygon(c(seq(1:(NCOL(cred_ints))), rev(seq(1:NCOL(cred_ints)))),
        c(cred_ints[1,],rev(cred_ints[3,])),
        col = rgb(150, 0, 0, max = 255, alpha = 100), border = NA)
cred_ints <- apply(fits, 2, function(x) hpd(x, 0.68))
polygon(c(seq(1:(NCOL(cred_ints))), rev(seq(1:NCOL(cred_ints)))),
        c(cred_ints[1,],rev(cred_ints[3,])),
        col = rgb(150, 0, 0, max = 255, alpha = 180), border = NA)
lines(cred_ints[2,], col = rgb(150, 0, 0, max = 255), lwd = 2, lty = 'dashed')
points(PP_data$data_test$y, pch = 16)

# Have a look at the estimated dynamic trend component and AR parameters
par(mfrow = c(1,1))
plot_mvgam_trend(df_gam_mod, 1, data_test = PP_data$data_test)
MCMCvis::MCMCtrace(df_gam_mod$jags_output, c('phi','ar1', 'ar2', 'ar3'),
                   n.eff = T, pdf = F)
par(mfrow = c(1,1))

## Online forecasting from the dynamic gam
# Initiate particle filter by assimilating the next observation in data_test
pfilter_mvgam_init(object = df_gam_mod, n_particles = 10000, n_cores = 4,
                   data_assim = PP_data$data_test)

# Assimilate some of the next observations in the series using the
# kernel smoothing sequential monte carlo algorithm
pfilter_mvgam_online(data_assim = PP_data$data_test[1:5,], n_cores = 4,
                     kernel_lambda = 1)

# Forecast from particles using the covariate information in remaining data_test observations
fc <- pfilter_mvgam_fc(file_path = 'pfilter', n_cores = 4,
                       data_test = PP_data$data_test, ylim = c(0, 100),
                       plot_legend = F)

# Compare the updated forecast to the original forecast
par(mfrow=c(1,2))
plot_mvgam_fc(df_gam_mod, 1, data_test = PP_data$data_test, ylim = c(0, 100))
fc$PP()


