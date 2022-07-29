#### Hypothesis testing for NEON tick abundance forecasts ####
#### Load data and functions ####
library(mvgam)
library(dplyr)
library(ggplot2)
library(viridis)
data("all_neon_tick_data")
source('neon_utility_functions.R')

# Prep data
all_data <- prep_neon_data(species = 'Ambloyomma_americanum', split_prop = 0.8)

#### Set hypothesis formulae ####
# NULL. There is no seasonal pattern to be estimated, and we simply let the latent
# factors and site-level effects of growing days influence the series dynamics
null_hyp = y ~ s(siteID, bs = 're') +
  s(cum_gdd, k = 5) +
  s(cum_gdd, siteID, k = 5, m = 1, bs = 'fs')

# 1. Do all series share same seasonal pattern, with any remaining variation due to
# non-seasonal local variation captured by the trends?
hyp1 = y ~
  s(siteID, bs = 're') +
  s(cum_gdd, k = 5) +
  s(cum_gdd, siteID, k = 5, m = 1, bs = 'fs') +
  # Global cyclic seasonality term (smooth)
  s(season, k = 12, m = 2, bs = 'cc')

# 2. Is there evidence for global seasonality but each site's seasonal pattern deviates
# based on more local conditions?
hyp2 = y ~
  s(siteID, bs = 're') +
  s(cum_gdd, k = 5) +
  s(cum_gdd, siteID, k = 5, m = 1, bs = 'fs') +
  s(season, m = 2, bs = 'cc', k = 12) +
  # Site-level deviations from global pattern, which can be wiggly (m=1 to reduce concurvity);
  # If these dominate, they will have smaller smoothing parameters and the global seasonality
  # will become less important (larger smoothing parameter). Sites with the smallest smooth
  # parameters are those that deviate the most from the global seasonality
  s(season, siteID, m = 1, k = 6, bs = 'fs')

# 3. Is there evidence for global seasonality but each plot's seasonal pattern deviates
# based on even more local conditions than above (i.e. site-level is not as informative)?
# If evidence of gdd effects, can also let use a global smoother and then site-level
# deviations for a 'hierarchical' setup
hyp3 = y ~
  s(siteID, bs = 're') +
  s(cum_gdd, k = 5) +
  s(cum_gdd, siteID, k = 5, m = 1, bs = 'fs') +
  s(season, m = 2, bs = 'cc', k = 12) +
  # Series-level deviations from global pattern
  s(season, series, m = 1, k = 4, bs = 'fs')

fit_null <- fit_mvgam(data_train = all_data$data_train,
                      data_test = all_data$data_test,
                      formula = null_hyp,
                      formula_name = 'Null_hyp',
                      family = 'nb',
                      use_lv = T,
                      n_lv = floor(length(unique(all_data$data_train$series))/2),
                      burnin = burnin,
                      n_samples = n_samples,
                      interval_width = 0.9)

fit_hyp1 <- fit_mvgam(data_train = all_data$data_train,
                      data_test = all_data$data_test,
                      formula = hyp1,
                      formula_name = 'Hyp1',
                      knots = list(season = c(0.5, 52.5)),
                      family = 'nb',
                      use_lv = T,
                      n_lv = floor(length(unique(all_data$data_train$series))/2),
                      burnin = burnin,
                      n_samples = n_samples,
                      interval_width = 0.9)

fit_hyp2 <- fit_mvgam(data_train = all_data$data_train,
                      data_test = all_data$data_test,
                      formula = hyp2,
                      formula_name = 'Hyp2',
                      knots = list(season = c(0.5, 52.5)),
                      family = 'nb',
                      use_lv = T,
                      n_lv = floor(length(unique(all_data$data_train$series))/2),
                      burnin = burnin,
                      n_samples = n_samples,
                      interval_width = 0.9)

fit_hyp3 <- fit_mvgam(data_train = all_data$data_train,
                      data_test = all_data$data_test,
                      formula = hyp3,
                      formula_name = 'Hyp3',
                      knots = list(season = c(0.5, 52.5)),
                      family = 'nb',
                      use_lv = T,
                      n_lv = floor(length(unique(all_data$data_train$series))/2),
                      burnin = burnin,
                      n_samples = n_samples,
                      interval_width = 0.9)

# Save models
dir.create('Results', recursive = T, showWarnings = F)
save(fit_null,
     fit_hyp1,
     fit_hyp2,
     fit_hyp3,
     file = 'Results/amb_models.rda')
