#### Testing the mvjagam function ####
library(mvgam)
library(dplyr)

# Test that the series_to_mvgam function works to show how ts or xts objects can easily
# be converted into the correct format
library(xts)
library(forecast)
data("AirPassengers")
series <- xts::as.xts(floor(AirPassengers))
colnames(series) <- c('Air')
fake_data <- series_to_mvgam(series, freq = 12, train_prop = 0.75)

plot(series)
hist(series)
plot(stl(series, s.window = 'periodic'))

autoplot(forecast(ets(series)))
rm(series)

# Fit a well-specified model in which the GAM component captures the repeated
# seasonality and the latent trend captures AR parameters up to order 3
mod <- mvjagam(data_train = fake_data$data_train,
               data_test = fake_data$data_test,
               formula = y ~ s(season, bs = c('cc'), k = 12),
               knots = list(season = c(0.5, 12.5)),
               family = 'nb',
               use_lv = F,
               trend_model = 'AR3',
               n.burnin = 5000,
               auto_update = F)

# Check the model summary and plot the smooth term
summary_mvgam(mod)
summary(mod$mgcv_model)
plot_mvgam_resids(mod, 1)
plot_mvgam_smooth(mod, 1, 'season')

# Compare the model's forecast to one from an automatic exponential smoothing model
par(mfrow = c(1,2))
ets_fc <- forecast(ets(series), h = NROW(fake_data$data_test))
plot_mvgam_fc(mod, ylim = c(min(ets_fc$x),
                            max(ets_fc$upper)))
plot(ets_fc)

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
                       data_test = fake_data$data_test,
                       ylim = c(0, max(fake_data$data_test$y)*1.25))
par(mfrow=c(1,2))
plot_mvgam_fc(mod, series = 1, data_test = fake_data$data_test,
              ylim = c(0, max(fake_data$data_test$y)*1.25))
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
