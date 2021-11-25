#### Testing the mvjagam function ####
library(mvgam)
library(dplyr)

# Test that the series_to_mvgam function works to show how ts or xts objects can easily
# be converted into the correct format
library(xts)
dates <- seq(as.Date("2017-05-01"), length=30, by="quarter")
data  <- cbind(c(gas = rpois(30, cumprod(1+rnorm(30, mean = 0.01, sd = 0.001)))),
               c(oil = rpois(30, cumprod(1+rnorm(30, mean = 0.01, sd = 0.001)))))
series <- xts(x = data, order.by = dates)
colnames(series) <- c('gas', 'oil')
head(series)
series_to_mvgam(series, freq = 4, train_prop = 0.85)

# Google Trends example; tick paralysis and related search interests in Queensland, Australia
terms = c("paralysis tick dog",
          "tick bite",
          "la nina",
          "tick paralysis",
          "dog tick")
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

series <- xts(x = gtest[,-1], order.by = gtest$date)
head(series)
plot(series, legend.loc = 'topleft')
trends_data <- series_to_mvgam(series, freq = 12, train_prop = 0.95)

# Multivariate trend model with hierarchical seasonality
trends_mod <- mvjagam(data_train = trends_data$data_train,
                 data_test = trends_data$data_test,
                 formula = y ~ s(season, k = 6, m = 2, bs = 'cc') +
                               s(season, by = series, m = 1, k = 12) - 1,
                 use_nb = TRUE,
                 use_mv = T,
                 n.burnin = 25000,
                 n.iter = 5000,
                 thin = 5,
                 auto_update = F)

# Plot posterior predictive distributions
par(mfrow = c(3, 2))
plot_mvgam_fc(object = trends_mod, series = 1, data_test = trends_data$data_test,
              data_train = trends_data$data_train)
plot_mvgam_fc(object = trends_mod, series = 2, data_test = trends_data$data_test,
              data_train = trends_data$data_train)
plot_mvgam_fc(object = trends_mod, series = 3, data_test = trends_data$data_test,
              data_train = trends_data$data_train)
plot_mvgam_fc(object = trends_mod, series = 4, data_test = trends_data$data_test,
              data_train = trends_data$data_train)
plot_mvgam_fc(object = trends_mod, series = 5, data_test = trends_data$data_test,
              data_train = trends_data$data_train)

# Plot trend estimates
par(mfrow = c(3, 2))
plot_mvgam_trend(object = trends_mod, series = 1, data_test = trends_data$data_test,
              data_train = trends_data$data_train)
plot_mvgam_trend(object = trends_mod, series = 2, data_test = trends_data$data_test,
              data_train = trends_data$data_train)
plot_mvgam_trend(object = trends_mod, series = 3, data_test = trends_data$data_test,
              data_train = trends_data$data_train)
plot_mvgam_trend(object = trends_mod, series = 4, data_test = trends_data$data_test,
                 data_train = trends_data$data_train)
plot_mvgam_trend(object = trends_mod, series = 5, data_test = trends_data$data_test,
                 data_train = trends_data$data_train)
par(mfrow = c(1,1))

# Inspect strengths of trend components
MCMCvis::MCMCtrace(trends_mod$jags_output, 'trend_comp', pdf = F)

# Inspect traces of smooth penalties
trends_mod$smooth_param_details
MCMCvis::MCMCtrace(trends_mod$jags_output, 'rho', pdf = F, n.eff = TRUE)

# Inspect traces for latent variable loadings
MCMCvis::MCMCtrace(trends_mod$jags_output, 'lv_coefs', pdf = F, n.eff = TRUE)

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


# Simulate data with shared seasonal structure and correlated trends
set.seed(110)
n_series = 6
sim_data <- sim_mvgam(T = 120,
                      n_series = n_series,
                      prop_missing = 0.25,
                      train_prop = 0.85,
                      seasonality = 'shared')

# Inspect empirical trend correlations
round(sim_data$true_corrs, 4)

# Plot the global seasonal pattern
plot(sim_data$global_seasonality, type = 'l')

# Fit the multivariate gam with a shared seasonal pattern
test <- mvjagam(data_train = sim_data$data_train,
                data_test = sim_data$data_test,
                formula = y ~ s(season, k = 12, m = 2, bs = 'cc') - 1,
                use_nb = TRUE,
                use_mv = T,
                n_lv = 6,
                n.burnin = 5000,
                n.iter = 1000,
                thin = 1,
                auto_update = F)
mod_dic <- dic.samples(test$jags_model, 500)

# Try equivalent model that allows series' seasonality to deviate smoothly from global seasonality
# test2 <- mvjagam(data_train = sim_data$data_train,
#                 data_test = sim_data$data_test,
#                 formula = y ~ s(season, k = 8, m = 2, bs = 'cc') +
#                   s(season, by = series, m = 1, k = 10) - 1,
#                 use_nb = TRUE,
#                 use_mv = T,
#                 n.adapt = 2000,
#                 n.burnin = 5000,
#                 n.iter = 1000,
#                 thin = 1,
#                 auto_update = F)

# Equivalent univariate trend model
test2 <- mvjagam(data_train = sim_data$data_train,
                data_test = sim_data$data_test,
                formula = y ~ s(season, k = 12, m = 2, bs = 'cc') - 1,
                use_nb = TRUE,
                use_mv = F,
                n.burnin = 5000,
                n.iter = 1000,
                thin = 1,
                auto_update = F)
mod2_dic <- dic.samples(test2$jags_model, 500)

# Which model fits better in terms of DIC?
hpd(mod_dic$deviance, 0.5)
hpd(mod2_dic$deviance, 0.5)

# Plot posterior predictive distributions
plot_mvgam_fc(object = test2, series = 1, data_test = sim_data$data_test,
              data_train = sim_data$data_train)
plot_mvgam_fc(object = test2, series = 2, data_test = sim_data$data_test,
              data_train = sim_data$data_train)
plot_mvgam_fc(object = test2, series = 3, data_test = sim_data$data_test,
              data_train = sim_data$data_train)
plot_mvgam_fc(object = test2, series = 4, data_test = sim_data$data_test,
              data_train = sim_data$data_train)

# Plot posterior trend estimates
plot_mvgam_trend(object = test2, series = 1, data_test = sim_data$data_test,
                 data_train = sim_data$data_train)
plot_mvgam_trend(object = test2, series = 2, data_test = sim_data$data_test,
                 data_train = sim_data$data_train)
plot_mvgam_trend(object = test2, series = 3, data_test = sim_data$data_test,
                 data_train = sim_data$data_train)
plot_mvgam_trend(object = test2, series = 4, data_test = sim_data$data_test,
                 data_train = sim_data$data_train)

# Plot estimates of trend relative importance per series (default for simulations is 0.2)
MCMCvis::MCMCtrace(test2$jags_output, 'trend_comp', pdf = F)

# Calculate latent trend correlations
correlations <- lv_correlations(object = test2, data_train = sim_data$data_train)


# Plot trend correlations
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

# True trend correlations
round(sim_data$true_corrs, 4)

# Compare to truth by calculating proportion of true associations (>0.2) that were correctly detected
truth <- sim_data$true_corrs
truth[abs(truth) < 0.2] <- 0
truth[upper.tri(truth)] <- NA
diag(truth) <- 0
estimated <- correlations$mean_correlations
estimated[abs(truth) < 0.2] <- 0
estimated[upper.tri(estimated)] <- NA
diag(estimated) <- 0

# Proportion of true pairwise correlations that the model estimated wrongly (wrong sign of correlation)
corr_dif_signs <- (sign(truth) - sign(estimated)) / 2
corr_dif_signs[upper.tri(corr_dif_signs)] <- NA
data.frame(correlations_wrong = length(which(abs(corr_dif_signs) > 0)),
           total_correlations = length(which(abs(truth) > 0)))

#### Even when using a fairly large number of series (14) with reasonable missingness (25%),
# we still find that the univariate model does nearly as well when estimating trend correlations. Need
# to use DRPS to see at what point a multivariate model is worth the extra computational cost
