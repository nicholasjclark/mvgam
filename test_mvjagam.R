#### Testing the mvjagam function ####

# TO DO:
# 1. write filtering and online filtering functions
# 2. include option to inspect uncertainty contributions when forecasting from particle filter
# 3. include some diagnostics on particle filter heterogeneity in weights etc...
# 4. compare particle filtered vs re-calibrated forecast for some simulations or trends / neon data
# 5. add generic plot_smooth functions to show the GAM partial effects, with names

library(mvgam)
library(dplyr)

# Test that the series_to_mvgam function works to show how ts or xts objects can easily
# be converted into the correct format
library(xts)
library(forecast)
data("AirPassengers")
series <- xts::as.xts(floor(AirPassengers/25))
colnames(series) <- c('Air')
fake_data <- series_to_mvgam(series, freq = 12, train_prop = 0.85)
mod <- mvjagam(data_train = fake_data$data_train,
               formula = y ~ s(season, bs = c('cc')) +
                 s(year) + ti(season, year) - 1,
                use_nb = F,
                  n.burnin = 10000,
                  n.iter = 1000,
                  thin = 1,
                  auto_update = F)
plot_mvgam_fc(mod, series = 1)
plot_mvgam_trend(mod, series = 1)

# Initiate particles by assimilating the next observation in data_test
pfilter_mvgam_init(object = mod, n_particles = 5000, n_cores = 3,
                   data_assim = fake_data$data_test)

# Forecast from particles using the covariate information in remaining data_test observations
fc <- pfilter_mvgam_fc(file_path = 'pfilter', n_cores = 3,
                       data_test = fake_data$data_test)
fc$Air()

# Remove the particles
unlink('pfilter', recursive = T)

# Google Trends example; tick paralysis and related search interests in Queensland, Australia
terms = c("tick bite",
          "tick paralysis",
          "dog tick", "la nina",
          "remove tick")
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

# mvgam model with hierarchical seasonality
trends_mod <- mvjagam(data_train = trends_data$data_train,
                      data_test = trends_data$data_test,
                 formula = y ~ s(season, k = 6, m = 2, bs = 'cc') +
                   s(season, by = series, k = 10, m = 1) - 1,
                 use_lv = T,
                 n_lv = 3,
                 use_nb = T,
                 n.burnin = 5000,
                 n.iter = 1000,
                 thin = 1,
                 upper_bounds = rep(100, length(terms)),
                 auto_update = F)

# Plot posterior predictive distributions
par(mfrow = c(3, 1))
plot_mvgam_fc(object = trends_mod, series = 1)
plot_mvgam_fc(object = trends_mod, series = 2)
plot_mvgam_fc(object = trends_mod, series = 3)

# Plot trend estimates
par(mfrow = c(3, 1))
plot_mvgam_trend(object = trends_mod, series = 1)
plot_mvgam_trend(object = trends_mod, series = 2)
plot_mvgam_trend(object = trends_mod, series = 3)
par(mfrow = c(1,1))

# Inspect traces of smooth penalties
trends_mod$smooth_param_details
MCMCvis::MCMCtrace(trends_mod$jags_output, 'rho', pdf = F, n.eff = TRUE)

# Plot uncertainty components
par(mfrow = c(3, 2))
plot_mvgam_uncertainty(object = trends_mod, series = 1, data_test = trends_data$data_test,
                       data_train = trends_data$data_train,
                       legend_position = 'bottomleft')
plot_mvgam_uncertainty(object = trends_mod, series = 2, data_test = trends_data$data_test,
                       data_train = trends_data$data_train,
                       legend_position = 'bottomleft')
plot_mvgam_uncertainty(object = trends_mod, series = 3, data_test = trends_data$data_test,
                       data_train = trends_data$data_train,
                       legend_position = 'bottomleft')
plot_mvgam_uncertainty(object = trends_mod, series = 4, data_test = trends_data$data_test,
                       data_train = trends_data$data_train,
                       legend_position = 'bottomleft')
plot_mvgam_uncertainty(object = trends_mod, series = 5, data_test = trends_data$data_test,
                       data_train = trends_data$data_train,
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
library(mvgam)
pfilter_mvgam_init(object = trends_mod,
                   data_assim = trends_data$data_test,
                   n_particles = 5000, n_cores = 5)

# Assimilate next observation
pfilter_mvgam_online(data_assim = trends_data$data_test[1:(length(unique(trends_data$data_test$series)) * 2),],
                     file_path = 'pfilter', n_cores = 5,
                     kernel_lambda = 1)


# Forecast from particles using the covariate information in remaining data_test observations
fc <- pfilter_mvgam_fc(file_path = 'pfilter', n_cores = 5,
                       data_test = trends_data$data_test,
                       return_forecasts = T)

# Inspect forecasts for a few series
par(mfrow = c(1, 2))
plot_mvgam_fc(object = trends_mod, series = 1)
fc$fc_plots$`dog tick`()
par(mfrow = c(1, 2))
plot_mvgam_fc(object = trends_mod, series = 2)
fc$fc_plots$`la nina`()
par(mfrow = c(1, 2))
plot_mvgam_fc(object = trends_mod, series = 3)
fc$fc_plots$`remove tick`()
par(mfrow = c(1, 2))
plot_mvgam_fc(object = trends_mod, series = 4)
fc$fc_plots$`tick bite`()
par(mfrow = c(1, 2))
plot_mvgam_fc(object = trends_mod, series = 5)
fc$fc_plots$`tick paralysis`()
par(mfrow = c(1,1))


# Remove the particles
unlink('pfilter', recursive = T)

# Simulate data with shared seasonal structure and correlated trends
set.seed(110)
n_series = 3
sim_data <- sim_mvgam(T = 120,
                      n_series = n_series,
                      prop_missing = 0.25,
                      train_prop = 0.85,
                      seasonality = 'shared')

# Fit the multivariate gam with a shared seasonal pattern
test <- mvjagam(data_train = sim_data$data_train,
                data_test = sim_data$data_test,
                formula = y ~ s(season, k = 12, m = 2, bs = 'cc') - 1,
                use_nb = TRUE,
                use_mv = T,
                n.burnin = 5000,
                n.iter = 1000,
                thin = 1,
                auto_update = F)

# Inspect contributions to forecast uncertainty for each series
plot_mvgam_uncertainty(series=2,
                       data_train = sim_data$data_train,
                       data_test = sim_data$data_test,
                       object = test)
