#### Testing the mvjagam function ####

# TO DO:
# 3. include some diagnostics on particle filter heterogeneity in weights etc...
# 4. compare particle filtered vs re-calibrated forecast for some simulations or trends / neon data

library(mvgam)
library(dplyr)

# Test that the series_to_mvgam function works to show how ts or xts objects can easily
# be converted into the correct format
library(xts)
library(forecast)
data("AirPassengers")
series <- xts::as.xts(floor(AirPassengers/25))
colnames(series) <- c('Air')
fake_data <- series_to_mvgam(series, freq = 12, train_prop = 0.75)
rm(series)

mod <- mvjagam(data_train = fake_data$data_train,
               data_test = fake_data$data_test,
               formula = y ~ s(season, bs = c('cc')),
               family = 'nb',
               use_lv = F,
               trend_model = 'AR3',
               n.burnin = 4000,
               n.iter = 1000,
               thin = 1,
               auto_update = F)

# An awful model to test the model comparison functions
fake_data$data_train$fake_cov <- rnorm(NROW(fake_data$data_train))
fake_data$data_test$fake_cov <- rnorm(NROW(fake_data$data_test))
mod2 <- mvjagam(data_train = fake_data$data_train,
               data_test = fake_data$data_test,
               formula = y ~ s(fake_cov, k = 3),
               family = 'nb',
               use_lv = F,
               trend_model = 'RW',
               n.burnin = 10,
               n.iter = 100,
               thin = 1,
               auto_update = F)

# Testing rolling DRPS evaluation
roll_eval_mvgam(mod, n_samples = 1000,
                fc_horizon = 5, n_cores = 2)
roll_eval_mvgam(mod2, n_samples = 1000,
                fc_horizon = 5, n_cores = 2)

# Summary plots and diagnostics
plot(mod$resids$Air)
lines(mod$resids$Air)
acf(mod$resids$Air)
pacf(mod$resids$Air)
plot_mvgam_smooth(mod, series=1, 'season')
plot_mvgam_fc(mod2, series = 1)
plot_mvgam_trend(mod, series = 1)
plot_mvgam_uncertainty(mod, series=1, data_test = fake_data$data_test)
MCMCvis::MCMCtrace(mod$jags_output, c('phi', 'ar1', 'ar2', 'ar3'), pdf = F,
                   n.eff = T)
par(mfrow=c(1,1))

# Initiate particles by assimilating the next observation in data_test
pfilter_mvgam_init(object = mod, n_particles = 10000, n_cores = 2,
                   data_assim = fake_data$data_test)

# Assimilate some observations
pfilter_mvgam_online(data_assim = fake_data$data_test[1:2,], n_cores = 2,
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
                 use_lv = T,
                 trend_model = 'AR3',
                 n_lv = 3,
                 family = 'nb',
                 n.burnin = 5000,
                 n.iter = 1000,
                 thin = 1,
                 upper_bounds = rep(100, length(terms)),
                 auto_update = F)

# Poor model for comparison
trends_mod2 <- mvjagam(data_train = trends_data$data_train,
                      data_test = trends_data$data_test,
                      formula = y ~ s(season, k = 3, bs = 'cc') - 1,
                      use_lv = T,
                      trend_model = 'RW',
                      n_lv = 2,
                      family = 'poisson',
                      n.burnin = 50,
                      n.iter = 500,
                      thin = 1,
                      upper_bounds = rep(100, length(terms)),
                      auto_update = F)

mod_eval <- roll_eval_mvgam(object = trends_mod, n_samples = 1000,
                            fc_horizon = 5, n_cores = 2)
mod2_eval <- roll_eval_mvgam(object = trends_mod2, n_samples = 1000,
                            fc_horizon = 5, n_cores = 2)

# Total sum of rolling forecast evaluation DRPS (lower is better)
mod_eval$sum_drps
mod2_eval$sum_drps

# Summaries of DRPS
mod_eval$drps_summary
mod2_eval$drps_summary

# Summary by forecast horizon
mod_eval$drps_horizon_summary
mod2_eval$drps_horizon_summary

# Look at Dunn-Smyth residuals for some series
hist(trends_mod$resids$`dog tick`)
plot(trends_mod$resids$`dog tick`)
lines(trends_mod$resids$`dog tick`)
acf(trends_mod$resids$`dog tick`)
pacf(trends_mod$resids$`dog tick`)

hist(trends_mod$resids$`la nina`)
plot(trends_mod$resids$`la nina`)
lines(trends_mod$resids$`la nina`)
acf(trends_mod$resids$`la nina`)
pacf(trends_mod$resids$`la nina`)

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

# Inspect traces of latent variable AR components and lv penalties
MCMCvis::MCMCtrace(trends_mod$jags_output, c('phi','ar1','ar2','ar3'), pdf = F, n.eff = TRUE)
MCMCvis::MCMCtrace(trends_mod$jags_output, c('lv_coefs'), pdf = F, n.eff = TRUE)

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
