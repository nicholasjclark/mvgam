#### Testing the mvjagam function ####

# TO DO:
# 1. write filtering and online filtering functions
# 2. include option to inspect uncertainty contributions when forecasting from particle filter
# 3. include some diagnostics on particle filter heterogeneity in weights etc...
# 4. update plot fc function to work for particles by showing all observations to that point as points and then
# showing the pf forecast distribution (similar to a plotted forecast::forecast object)
# 5. compare particle filtered vs re-calibrated forecast for some simulations or trends / neon data

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
                  n.burnin = 1000,
                  n.iter = 1000,
                  thin = 1,
                  auto_update = F)
plot_mvgam_fc(mod, series = 1, data_train = fake_data$data_train)
plot_mvgam_trend(mod, series = 1, data_train = fake_data$data_train)

# Initiate particles by assimilating the next observation in data_test
pfilter_mvgam_init(object = mod, data_train = fake_data$data_train,
                   data_assim = fake_data$data_test)

# Forecast from particles using the covariate information in remaining data_test observations
fc <- pfilter_mvgam_fc(file_path = 'pfilter', n_cores = 5, data_test = fake_data$data_test)

library(mvforecast)
plot_mvforecast(t(fc$Air), non_negative = T)
points(fake_data$data_test$y[-1])

# Remove the particles
unlink('pfilter', recursive = T)

# Google Trends example; tick paralysis and related search interests in Queensland, Australia
terms = c("paralysis tick dog",
          "tick bite",
          "la nina",
          "tick paralysis",
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
head(gtest)

series <- xts::xts(x = gtest[,-1], order.by = gtest$date)
head(series)
plot(series, legend.loc = 'topleft')
trends_data <- series_to_mvgam(series, freq = 12, train_prop = 0.9)

# Multivariate trend model with hierarchical seasonality
trends_mod <- mvjagam(data_train = trends_data$data_train,
                 formula = y ~ s(season, k = 6, m = 2, bs = 'cc') +
                               s(season, by = series, m = 1, k = 12) - 1,
                 use_lv = T,
                 n_lv = 3,
                 use_nb = TRUE,
                 n.burnin = 500,
                 n.iter = 500,
                 thin = 1,
                 auto_update = F,
                 upper_bounds = rep(100, NCOL(trends_data$data_train)))

# Plot posterior predictive distributions
par(mfrow = c(3, 2))
plot_mvgam_fc(object = trends_mod, series = 1,
              data_train = trends_data$data_train)
plot_mvgam_fc(object = trends_mod, series = 2,
              data_train = trends_data$data_train)
plot_mvgam_fc(object = trends_mod, series = 3,
              data_train = trends_data$data_train)
plot_mvgam_fc(object = trends_mod, series = 4,
              data_train = trends_data$data_train)
plot_mvgam_fc(object = trends_mod, series = 5,
              data_train = trends_data$data_train)

# Plot trend estimates
par(mfrow = c(3, 2))
plot_mvgam_trend(object = trends_mod, series = 1,
              data_train = trends_data$data_train)
plot_mvgam_trend(object = trends_mod, series = 2,
              data_train = trends_data$data_train)
plot_mvgam_trend(object = trends_mod, series = 3,
              data_train = trends_data$data_train)
plot_mvgam_trend(object = trends_mod, series = 4,
                 data_train = trends_data$data_train)
plot_mvgam_trend(object = trends_mod, series = 5,
                 data_train = trends_data$data_train)
par(mfrow = c(1,1))

# Inspect strengths of trend components
MCMCvis::MCMCtrace(trends_mod$jags_output, 'trend_comp', pdf = F,
                   main_den = levels(trends_data$data_train$series),
                   main_tr = levels(trends_data$data_train$series),
                   n.eff = T)

# Inspect traces of smooth penalties
trends_mod$smooth_param_details
MCMCvis::MCMCtrace(trends_mod$jags_output, 'rho', pdf = F, n.eff = TRUE)

# Inspect traces for latent variable loadings
MCMCvis::MCMCtrace(trends_mod$jags_output, 'lv_coefs', pdf = F, n.eff = TRUE)
MCMCvis::MCMCtrace(trends_mod$jags_output, 'penalty', pdf = F, n.eff = TRUE)

# Plot trend correlations
correlations <- lv_correlations(object = trends_mod, data_train = trends_data$data_train)

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
pfilter_mvgam_init(object = trends_mod, data_train = trends_data$data_train,
                   data_assim = trends_data$data_test)

# Forecast from particles using the covariate information in remaining data_test observations
fc <- pfilter_mvgam_fc(file_path = 'pfilter', n_cores = 5,
                       data_test = trends_data$data_test)

# Inspect forecasts for a few series
library(mvforecast)
plot_mvforecast(t(fc$`la nina`), non_negative = T)
points((trends_data$data_test %>%
         dplyr::filter(series == 'la nina') %>%
         dplyr::pull(y))[-1])

plot_mvforecast(t(fc$`remove tick`), non_negative = T)
points((trends_data$data_test %>%
          dplyr::filter(series == 'remove tick') %>%
          dplyr::pull(y))[-1])

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
