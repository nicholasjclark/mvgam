#### Testing the mvjagam function ####
library(mvgam)
library(dplyr)

# Simulate data
n_series = 5
sim_data <- sim_mvgam(T = 100,
                      n_series = n_series,
                      prop_missing = 0.15,
                      train_prop = 0.85,
                      seasonality = 'hierarchical')
round(sim_data$true_corrs, 4)

# The stable seasonal pattern
plot(sim_data$global_seasonality, type = 'l')

# Fit the multivariate gam with a shared seasonal pattern. Use at least 5 latent factors
# to allow for more precise calculation of trend covariances among series
test <- mvjagam(data_train = sim_data$data_train,
                data_test = sim_data$data_test,
                formula = y ~ s(season, k = 12, m = 2, bs = 'cc') - 1,
                use_nb = TRUE,
                use_mv = T,
                n_lv = 5,
                n.adapt = 1000,
                n.burnin = 2000,
                n.iter = 1000,
                thin = 1,
                auto_update = F)
mod_dic <- dic.samples(test$jags_model, 500)

# Try equivalent model that allows series' seasonality to deviate smoothly from global seasonality
test2 <- mvjagam(data_train = sim_data$data_train,
                data_test = sim_data$data_test,
                formula = y ~ s(season, k = 8, m = 2, bs = 'cc') +
                  s(season, by = series, m = 1, k = 10) - 1,
                use_nb = TRUE,
                use_mv = T,
                n_lv = 5,
                n.adapt = 1000,
                n.burnin = 2000,
                n.iter = 1000,
                thin = 1,
                auto_update = F)
mod2_dic <- dic.samples(test2$jags_model, 500)

# Model 2 should fit better
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
round(sim_data$true_corrs, 4)

# Compare to truth by calculating proportion of true associations (>0.1) that were correctly detected
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

