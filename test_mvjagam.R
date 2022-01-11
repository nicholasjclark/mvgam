#### Testing the mvjagam function ####
library(mvgam)
library(dplyr)

# Replicate the lynx analysis from the ESA 2018 GAM course, with some minor adjustments
library(ggplot2)
library(tidyr)
data(lynx)
lynx_full = data.frame(year=1821:1934, population = as.numeric(lynx))
head(lynx_full)
ggplot(aes(x=year, y=population), data= lynx_full)+
  geom_point()+
  geom_line()
acf(lynx_full$population)



# There is a clear ~10-year cyclic pattern to the data, so I create a 'season'
# term that can be used to model this effect and give a better representation
# of the data generating process
plot(stl(ts(lynx_full$population, frequency = 19), s.window = 'periodic'))
lynx_full$season <- (lynx_full$year %%19)+1
lynx_full$y <- lynx_full$population
lynx_full$series <- factor('series1')
# Add lag indicators and fit the nonlinear lag model that gave the best
# one step ahead point forecasts in the workshop example (order 2)
mean_pop_l = mean(log(lynx_full$population))
lynx_full = mutate(lynx_full,
                   popl = log(population),
                   lag1 = lag(popl,1, default = mean_pop_l),
                   lag2 = lag(popl,2, default = mean_pop_l),
                   lag3 = lag(popl,3, default = mean_pop_l),
                   lag4 = lag(popl,4, default = mean_pop_l),
                   lag5 = lag(popl,5, default = mean_pop_l),
                   lag6 = lag(popl,6, default = mean_pop_l))

# Split the data into training (first 75 years)
lynx_train = lynx_full[1:75,]

# and testing: we will test on the next 5 years of data
lynx_test = lynx_full[76:80,]

# The best-forecasting model in the course was with nonlinear smooths of
# lags 1 and 2; the only difference here is that I also include a cyclic smooth
# for the 10-year cycles
lynx_mgcv = gam(population~s(season, bs = 'cc') +
                s(lag1, k = 5) + s(lag2, k = 5),
                knots = list(season = c(0.5, 19.5)),
                data = lynx_train, family = "poisson",
                method = "REML")
summary(lynx_mgcv)
plot(lynx_mgcv, pages=1, shade=T)

# This model captures most of the deviance in the series and the
# functions are all confidently estimated to be non-zero and non-flat. There is
# something strange going on with the seasonal cyclic pattern, but we will
# look at that more later on. Anyway, so far, so good. Now for some forecasts
# for the out of sample period

# Use the uncertainties around smooth functions to generate a set of possible
# coefficients to draw from when simulating forecast paths
coef_sim <- gam.mh(lynx_mgcv)$bs

# Function to perform forecast simulations from the nonlinear lag model
# in a recursive fasion
recurse_nonlin = function(model, lagged_vals, h){
  # Initiate state vector
  states <- rep(NA, length = h + 2)
  # Last two values of the predicted log density begin the state vector
  states[1] <- as.numeric(exp(lagged_vals[2]))
  states[2] <- as.numeric(exp(lagged_vals[1]))
  # Get a random sample of the smooth coefficient uncertainty matrix
  # to use for the entire forecast horizon of this particular path
  gam_coef_index <- sample(seq(1, NROW(coef_sim)), 1, T)
  # For each following timestep, recursively predict based on the
  # predictions at each previous lag
  for (t in 3:(h + 2)) {
    # Build the linear predictor matrix using the two previous lags
    # of the (log) density
    newdata <- data.frame(lag1 = log(states[t-1]),
                          lag2 = log(states[t-2]),
                          season = lynx_test$season[t-2])
    colnames(newdata) <- c('lag1', 'lag2', 'season')
    Xp <- predict(model, newdata = newdata, type = 'lpmatrix')
    # Posterior predictions
    mu <- rpois(1, lambda = exp(Xp %*% coef_sim[gam_coef_index,]))
    states[t] <- mu
  }
  states[-c(1:2)]
}

# Create the mgcv gam's forecast distribution by generating 2000 simulated
# forecast paths. Each path is fed the true observed values for the last two lags
# of the first out of sample timepoint, but they can deviate when simulating ahead
# depending on their particular draw of possible coefficients from the model's
# estimated covariance matrix. Note, this is a bit slow and could easily be
# parallelised to speed up computations
gam_sims <- matrix(NA, nrow = 2000, ncol = 5)
for(i in 1:2000){
  gam_sims[i,] <- recurse_nonlin(lynx_mgcv,
                                 lagged_vals = head(lynx_test,1)[c(7,8)],
                                 h = 5)
}

# Plot the mgcv model's out of sample forecast for the next 5 years ahead
par(mfrow=c(1,2))
cred_ints <- apply(gam_sims, 2, function(x) hpd(x, 0.95))
yupper <- max(lynx_full$population) * 1.25
plot(cred_ints[3,] ~ seq(1:NCOL(cred_ints)), type = 'l',
     col = rgb(1,0,0, alpha = 0),
     ylim = c(0, yupper),
     ylab = 'Predicted lynx trappings',
     xlab = 'Forecast horizon',
     main = 'mgcv')
polygon(c(seq(1:(NCOL(cred_ints))), rev(seq(1:NCOL(cred_ints)))),
        c(cred_ints[1,],rev(cred_ints[3,])),
        col = rgb(150, 0, 0, max = 255, alpha = 100), border = NA)
cred_ints <- apply(gam_sims, 2, function(x) hpd(x, 0.68))
polygon(c(seq(1:(NCOL(cred_ints))), rev(seq(1:NCOL(cred_ints)))),
        c(cred_ints[1,],rev(cred_ints[3,])),
        col = rgb(150, 0, 0, max = 255, alpha = 180), border = NA)
lines(cred_ints[2,], col = rgb(150, 0, 0, max = 255), lwd = 2, lty = 'dashed')
points(lynx_test$population[1:5], pch = 16)
lines(lynx_test$population[1:5])

# Not a great forecast. The general shape is correct, but basically none of the
# observations lie within the model's 95% uncertainty intervals.

# Now fit an mvgam model; it essentially fits a similar model to the above
# but with linear lag coefficients within a full time series model
# for the errors, rather than smoothing splines that do not incorporate a concept
# of the future

# Add variables needed for mvgam models

lynx_mvgam <- mvjagam(data_train = lynx_train,
               data_test = lynx_test,
               formula = y ~ s(season, bs = 'cc', k=10),
               knots = list(season = c(0.5, 10.5)),
               family = 'poisson',
               use_lv = F,
               trend_model = 'AR1',
               n.burnin = 4000,
               n.iter = 1000,
               thin = 1,
               auto_update = F)
plot_mvgam_smooth(lynx_mvgam, smooth = 'season')
plot_mvgam_trend(lynx_mvgam)
plot_mvgam_fc(lynx_mvgam)
plot_mvgam_uncertainty(lynx_mvgam, data_test = lynx_test)

# Calculate the out of sample forecast from the mvgam model
fits <- MCMCvis::MCMCchains(lynx_mvgam$jags_output, 'ypred')
fits <- fits[,(NROW(lynx_mvgam$obs_data)+1):(NROW(lynx_mvgam$obs_data)+5)]
cred_ints <- apply(fits, 2, function(x) hpd(x, 0.95))
plot(cred_ints[3,] ~ seq(1:NCOL(cred_ints)), type = 'l',
     col = rgb(1,0,0, alpha = 0),
     ylim = c(0, yupper),
     ylab = '',
     xlab = 'Forecast horizon',
     main = 'mvgam')
polygon(c(seq(1:(NCOL(cred_ints))), rev(seq(1:NCOL(cred_ints)))),
        c(cred_ints[1,],rev(cred_ints[3,])),
        col = rgb(150, 0, 0, max = 255, alpha = 100), border = NA)
cred_ints <- apply(fits, 2, function(x) hpd(x, 0.68))
polygon(c(seq(1:(NCOL(cred_ints))), rev(seq(1:NCOL(cred_ints)))),
        c(cred_ints[1,],rev(cred_ints[3,])),
        col = rgb(150, 0, 0, max = 255, alpha = 180), border = NA)
lines(cred_ints[2,], col = rgb(150, 0, 0, max = 255), lwd = 2, lty = 'dashed')
points(lynx_test$population[1:5], pch = 16)
lines(lynx_test$population[1:5])

# The mvgam has much more realistic uncertainty than the mgcv version, with all
# out of sample observations falling within the model's 95% credible intervals
# Have a look at this model's summary to see what is being estimated
summary_mvgam(lynx_mvgam)

# Now inspect each model's estimated 10-year cyclic pattern
par(mfrow=c(1,2))
plot(lynx_mgcv, select=1, shade=T)
plot_mvgam_smooth(lynx_mvgam, 1, 'season')

# The mgcv verison includes a fairly wiggly and strange function for the
# cyclic effect. I postulate that it is overfitting the nonlinear lag functions
# (i.e. the lag effects are allowed to wiggle too much and overfit), making it challenging
# to also estimate the cyclic pattern. If we drop the nonlinear lag functions and reduce
# the maximum complexity of the cyclic smooth, can this be remedied and forecasts improved?
lynx_mgcv2 = gam(population ~ s(season, bs = 'cc', k = 5) +
                 lag1 + lag2,
                 knots = list(season = c(0.5, 10.5)),
                 data = lynx_train, family = "poisson",
                 method = "REML")
summary(lynx_mgcv2)
par(mfrow=c(1,2))
plot(lynx_mgcv2, select=1, shade=T)
plot_mvgam_smooth(lynx_mvgam, 1, 'season')

# The cyclic smooth looks better here. How are the forecasts?

# Re-plot the first mgcv model's out of sample forecast
par(mfrow=c(1,2))
cred_ints <- apply(gam_sims, 2, function(x) hpd(x, 0.95))
yupper <- max(lynx_full$population) * 1.25
plot(cred_ints[3,] ~ seq(1:NCOL(cred_ints)), type = 'l',
     col = rgb(1,0,0, alpha = 0),
     ylim = c(0, yupper),
     ylab = 'Predicted lynx trappings',
     xlab = 'Forecast horizon',
     main = 'mgcv (smooth lags)')
polygon(c(seq(1:(NCOL(cred_ints))), rev(seq(1:NCOL(cred_ints)))),
        c(cred_ints[1,],rev(cred_ints[3,])),
        col = rgb(150, 0, 0, max = 255, alpha = 100), border = NA)
cred_ints <- apply(gam_sims, 2, function(x) hpd(x, 0.68))
polygon(c(seq(1:(NCOL(cred_ints))), rev(seq(1:NCOL(cred_ints)))),
        c(cred_ints[1,],rev(cred_ints[3,])),
        col = rgb(150, 0, 0, max = 255, alpha = 180), border = NA)
lines(cred_ints[2,], col = rgb(150, 0, 0, max = 255), lwd = 2, lty = 'dashed')
points(lynx_test$population[1:5], pch = 16)
lines(lynx_test$population[1:5])

# Now calculate the linear lag model's forecasts and plot
coef_sim <- gam.mh(lynx_mgcv2)$bs
gam_sims <- matrix(NA, nrow = 2000, ncol = 5)
for(i in 1:2000){
  gam_sims[i,] <- recurse_nonlin(lynx_mgcv2,
                                 lagged_vals = head(lynx_test,1)[c(7,8)],
                                 h = 5)
}
cred_ints <- apply(gam_sims, 2, function(x) hpd(x, 0.95))
yupper <- max(lynx_full$population) * 1.25
plot(cred_ints[3,] ~ seq(1:NCOL(cred_ints)), type = 'l',
     col = rgb(1,0,0, alpha = 0),
     ylim = c(0, yupper),
     ylab = 'Predicted lynx trappings',
     xlab = 'Forecast horizon',
     main = 'mgcv (linear lags)')
polygon(c(seq(1:(NCOL(cred_ints))), rev(seq(1:NCOL(cred_ints)))),
        c(cred_ints[1,],rev(cred_ints[3,])),
        col = rgb(150, 0, 0, max = 255, alpha = 100), border = NA)
cred_ints <- apply(gam_sims, 2, function(x) hpd(x, 0.68))
polygon(c(seq(1:(NCOL(cred_ints))), rev(seq(1:NCOL(cred_ints)))),
        c(cred_ints[1,],rev(cred_ints[3,])),
        col = rgb(150, 0, 0, max = 255, alpha = 180), border = NA)
lines(cred_ints[2,], col = rgb(150, 0, 0, max = 255), lwd = 2, lty = 'dashed')
points(lynx_test$population[1:5], pch = 16)
lines(lynx_test$population[1:5])

# The forecast is certainly better than previously but still misses the mark on nearly
# all of the observations, and it has perhaps even more unrealistically narrow confidence
# intervals. Of course this is just one out of sample test of the model, and to really
# determine which model is most appropriate for forecasting we would want to run many of these
# tests using a rolling window approach.

# We can also view the mvgam's posterior predictions for the entire series (testing and training)
par(mfrow=c(1,1))
plot_mvgam_fc(lynx_mvgam, data_test = lynx_test)

# And the estimated trend
par(mfrow=c(1,1))
plot_mvgam_trend(lynx_mvgam, data_test = lynx_test)

# Have a look at the model's residuals, which are posterior medians of Dunn-Smyth
# randomised quantile residuals so should follow approximate normality. We are primarily
# looking for a lack of autocorrelation, which would suggest our AR2 model is appropriate for the
# latent trend
plot_mvgam_resids(lynx_mvgam)

 # Test that the series_to_mvgam function works to show how ts or xts objects can easily
# be converted into the correct format
library(xts)
library(forecast)
data("AirPassengers")
series <- xts::as.xts(floor(AirPassengers/25))
colnames(series) <- c('Air')
fake_data <- series_to_mvgam(series, freq = 12, train_prop = 0.75)
rm(series)

# Fit a well-specified model in which the GAM component captures the repeated
# seasonality and the latent trend captures AR parameters up to order 3
mod <- mvjagam(data_train = fake_data$data_train,
               data_test = fake_data$data_test,
               formula = y ~ s(season, bs = c('cc')),
               knots = list(season = c(0.5, 12.5)),
               family = 'nb',
               use_lv = F,
               trend_model = 'AR3',
               n.burnin = 1000,
               auto_update = F)

# Check the model summary and plot the smooth term
summary_mvgam(mod)
summary(mod$mgcv_model)
plot_mvgam_resids(mod, 1)
plot_mvgam_smooth(mod, 1, 'season')

# Fit a mis-specified model for testing the model comparison functions by
# smoothing on a white noise covariate
fake_data$data_train$fake_cov <- rnorm(NROW(fake_data$data_train))
fake_data$data_test$fake_cov <- rnorm(NROW(fake_data$data_test))
mod2 <- mvjagam(data_train = fake_data$data_train,
               data_test = fake_data$data_test,
               formula = y ~ s(fake_cov, k = 3),
               family = 'poisson',
               use_lv = F,
               trend_model = 'RW',
               n.burnin = 10,
               n.iter = 1000,
               thin = 1,
               auto_update = F)
summary_mvgam(mod2)
plot_mvgam_smooth(mod2, 1, 'fake_cov')
plot_mvgam_resids(mod2)

# Compare the models using rolling forecast DRPS evaluation
compare_mvgams(mod, mod2, fc_horizon = 6,
               n_evaluations = 30, n_cores = 3)

# Plot the posterior distribution of in-sample and out-of-sample predictions
# with true observations overlaid as points
plot_mvgam_fc(mod, series = 1, data_test = fake_data$data_test)

# Plot the estimated latent trend
plot_mvgam_trend(mod, series = 1, data_test = fake_data$data_test)

# Plot estimated contributions to forecast uncertainty
plot_mvgam_uncertainty(mod, series = 1, data_test = fake_data$data_test)

# Plot estimated trend parameters
MCMCvis::MCMCtrace(mod$jags_output, c('phi', 'ar1', 'ar2', 'ar3'), pdf = F,
                   n.eff = T)
par(mfrow=c(1,1))

# Initiate particles by assimilating the next observation in data_test
pfilter_mvgam_init(object = mod, n_particles = 10000, n_cores = 2,
                   data_assim = fake_data$data_test)

# Assimilate some of the next out of sample observations
pfilter_mvgam_online(data_assim = fake_data$data_test[1:7,], n_cores = 2,
                     kernel_lambda = 1)

# Forecast from particles using the covariate information in remaining data_test observations
fc <- pfilter_mvgam_fc(file_path = 'pfilter', n_cores = 2,
                       data_test = fake_data$data_test, ylim = c(0, 40))
par(mfrow=c(1,2))
plot_mvgam_fc(mod, series = 1, data_test = fake_data$data_test,
              ylim = c(0, 40))
fc$Air()
points(c(fake_data$data_train$y,
         fake_data$data_test$y), pch = 16)

# Remove the particles
unlink('pfilter', recursive = T)

# Google Trends example; tick paralysis and related search interests in Queensland, Australia
terms = c("tick bite",
          "tick paralysis",
          "dog tick", "la nina")
trends <- gtrendsR::gtrends(terms, geo = "AU-QLD",
                            time = "all", onlyInterest = T)

trends$interest_over_time %>%
  tidyr::spread(keyword, hits) %>%
  dplyr::select(-geo, -time, -gprop, -category) %>%
  dplyr::mutate(date = lubridate::ymd(date)) %>%
  dplyr::mutate(year = lubridate::year(date)) %>%
  dplyr::filter(year > 2012) %>%
  dplyr::select(-year)-> gtest
series <- xts::xts(x = gtest[,-1], order.by = gtest$date)
plot(series, legend.loc = 'topleft')
trends_data <- series_to_mvgam(series, freq = 12, train_prop = 0.9)

# mvgam model with hierarchical seasonality, recognising known upper bounds for
# Google trends (cannot go above 100)
trends_mod <- mvjagam(data_train = trends_data$data_train,
                      data_test = trends_data$data_test,
                 formula = y ~ s(season, k = 6, m = 2, bs = 'cc') +
                   s(season, by = series, k = 10, m = 1) - 1,
                 knots = list(season = c(0.5, 12.5)),
                 use_lv = T,
                 trend_model = 'AR3',
                 n_lv = 3,
                 family = 'nb',
                 n.burnin = 1000,
                 n.iter = 1000,
                 thin = 1,
                 upper_bounds = rep(100, length(terms)),
                 auto_update = F)
summary_mvgam(trends_mod)

# A fixed seasonality model for comparison
trends_mod2 <- mvjagam(data_train = trends_data$data_train,
                      data_test = trends_data$data_test,
                      formula = y ~ s(season, k = 6, bs = 'cc') - 1,
                      knots = list(season = c(0.5, 12.5)),
                      use_lv = T,
                      trend_model = 'AR3',
                      n_lv = 3,
                      family = 'nb',
                      n.burnin = 1000,
                      n.iter = 1000,
                      thin = 1,
                      upper_bounds = rep(100, length(terms)),
                      auto_update = F)
summary_mvgam(trends_mod2)

# Compare the models using rolling forecast DRPS evaluation. Here we focus on
# near-term forecasts (horizon = 3) when comparing model performances
par(mfrow = c(1,1))
compare_mvgams(trends_mod, trends_mod2, fc_horizon = 3,
               n_cores = 3, n_evaluations = 20, n_samples = 2500)

# Look at Dunn-Smyth residuals for some series from preferred model (Model 1)
plot_mvgam_resids(trends_mod, 1)
plot_mvgam_resids(trends_mod, 2)
plot_mvgam_resids(trends_mod, 3)

# Plot posterior predictive distributions
par(mfrow = c(4, 1))
plot_mvgam_fc(object = trends_mod, series = 1, ylim = c(0, 100))
plot_mvgam_fc(object = trends_mod, series = 2, ylim = c(0, 100))
plot_mvgam_fc(object = trends_mod, series = 3, ylim = c(0, 100))
plot_mvgam_fc(object = trends_mod, series = 4, ylim = c(0, 100))

# Plot trend estimates
par(mfrow = c(4, 1))
plot_mvgam_trend(object = trends_mod, series = 1)
plot_mvgam_trend(object = trends_mod, series = 2)
plot_mvgam_trend(object = trends_mod, series = 3)
plot_mvgam_trend(object = trends_mod, series = 4)
par(mfrow = c(1,1))

# Plot partial smooths of seasonality for each series
plot_mvgam_smooth(object = trends_mod, series = 1, smooth = 'season')
plot_mvgam_smooth(object = trends_mod, series = 2, smooth = 'season')
plot_mvgam_smooth(object = trends_mod, series = 3, smooth = 'season')
plot_mvgam_smooth(object = trends_mod, series = 4, smooth = 'season')

# Inspect traces of smooth penalties
trends_mod$smooth_param_details
MCMCvis::MCMCtrace(trends_mod$jags_output, 'rho', pdf = F, n.eff = TRUE)

# Inspect traces of latent variable AR components and lv loadings
MCMCvis::MCMCtrace(trends_mod$jags_output, c('phi','ar1','ar2','ar3'),
                   pdf = F, n.eff = TRUE)
MCMCvis::MCMCtrace(trends_mod$jags_output, c('lv_coefs'), pdf = F,
                   n.eff = TRUE)

# Plot uncertainty components
par(mfrow = c(2, 2))
plot_mvgam_uncertainty(object = trends_mod, series = 1, data_test = trends_data$data_test,
                       legend_position = 'bottomleft')
plot_mvgam_uncertainty(object = trends_mod, series = 2, data_test = trends_data$data_test,
                       legend_position = 'bottomleft')
plot_mvgam_uncertainty(object = trends_mod, series = 3, data_test = trends_data$data_test,
                       legend_position = 'bottomleft')
plot_mvgam_uncertainty(object = trends_mod, series = 4, data_test = trends_data$data_test,
                       legend_position = 'bottomleft')
par(mfrow = c(1,1))

# Plot trend correlations
correlations <- lv_correlations(object = trends_mod)

library(ggplot2)
mean_correlations <- correlations$mean_correlations
mean_correlations[upper.tri(mean_correlations)] <- NA
mean_correlations <- data.frame(mean_correlations)
ggplot(mean_correlations %>%
         tibble::rownames_to_column("series1") %>%
         tidyr::pivot_longer(-c(series1), names_to = "series2", values_to = "Correlation"),
       aes(x = series1, y = series2)) + geom_tile(aes(fill = Correlation)) +
  scale_fill_gradient2(low="darkred", mid="white", high="darkblue",
                       midpoint = 0,
                       breaks = seq(-1,1,length.out = 5),
                       limits = c(-1, 1),
                       name = 'Trend\ncorrelation') + labs(x = '', y = '') + theme_dark() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# Initiate particles by assimilating the next observation in data_test
pfilter_mvgam_init(object = trends_mod,
                   data_assim = trends_data$data_test,
                   n_particles = 30000, n_cores = 3)

# Assimilate next two observations per series as a test
pfilter_mvgam_online(data_assim =
                       trends_data$data_test[1:(length(unique(trends_data$data_test$series)) * 3),],
                     file_path = 'pfilter', n_cores = 3,
                     kernel_lambda = 1)

# Forecast from particles using the covariate information in remaining data_test observations
fc <- pfilter_mvgam_fc(file_path = 'pfilter', n_cores = 3,
                       data_test = trends_data$data_test,
                       return_forecasts = T, ylim = c(0, 100))

# Inspect forecasts for a few series and overlay true values in the test horizon
fc$fc_plots$`dog tick`()
points(rbind(trends_data$data_train, trends_data$data_test) %>%
         dplyr::filter(series == 'dog tick') %>%
         dplyr::select(year, season, y) %>%
         dplyr::distinct() %>%
         dplyr::arrange(year, season) %>%
         dplyr::pull(y), pch = 16)

fc$fc_plots$`la nina`()
points(rbind(trends_data$data_train, trends_data$data_test) %>%
         dplyr::filter(series == 'la nina') %>%
         dplyr::select(year, season, y) %>%
         dplyr::distinct() %>%
         dplyr::arrange(year, season) %>%
         dplyr::pull(y), pch = 16)

fc$fc_plots$`tick bite`()
points(rbind(trends_data$data_train, trends_data$data_test) %>%
         dplyr::filter(series == 'tick bite') %>%
         dplyr::select(year, season, y) %>%
         dplyr::distinct() %>%
         dplyr::arrange(year, season) %>%
         dplyr::pull(y), pch = 16)

fc$fc_plots$`tick paralysis`()
points(rbind(trends_data$data_train, trends_data$data_test) %>%
         dplyr::filter(series == 'tick paralysis') %>%
         dplyr::select(year, season, y) %>%
         dplyr::distinct() %>%
         dplyr::arrange(year, season) %>%
         dplyr::pull(y), pch = 16)
par(mfrow = c(1,1))

# Remove the particles
unlink('pfilter', recursive = T)
