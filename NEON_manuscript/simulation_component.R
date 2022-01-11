#### Simulation component of mvgam / NEON manuscript ####
library(mvgam)
library(dplyr)
source('neon_utility_functions.R')

# Use hierarchical seasonality for all;
# trend components: 0.3 and 0.7
# N_series: 4, 12
# Length: 6 years
# Missingness: none, 10%, 50%
set.seed(110)
run_parameters <- expand.grid(n_series = c(4, 12),
                              T = c(72),
                              prop_missing = c(0, 0.1, 0.5),
                              trend_rel = c(0.3, 0.7),
                              stringsAsFactors = F)

# Run models and extract performance indicators
sim_results <- lapply(seq_len(nrow(run_parameters)), function(x){
  cat('\nModelling row', x, '\n')
  sim_data <- sim_mvgam(T = run_parameters$T[x],
                        n_series = run_parameters$n_series[x],
                        prop_missing = run_parameters$prop_missing[x],
                        train_prop = 0.833,
                        trend_rel = run_parameters$trend_rel[x],
                        seasonality = 'hierarchical')

  # Fit a multivariate gam with no seasonality (null model)
  t <- Sys.time()
  nullmod <- mvjagam(data_train = sim_data$data_train,
                  data_test = sim_data$data_test,
                  formula = y ~ s(series, bs = 're'),
                  family = 'nb',
                  use_lv = F,
                  n.burnin = 25000,
                  n.iter = 5000,
                  thin = 5,
                  auto_update = T)
  difftime(Sys.time(), t, units = "mins")[[1]] -> comp_time

  # Record model computational times and summary of key effective sample sizes
  model_efficiency <- data.frame(model = c('null', 'hierarchical'),
                                 comp_time = NA,
                                 beta_eff_lower = NA,
                                 beta_eff_med = NA,
                                 beta_eff_upper = NA,
                                 ypred_eff_lower = NA,
                                 ypred_eff_med = NA,
                                 ypred_eff_upper = NA)

  model_efficiency[1,] <- c('null', comp_time,
                            hpd(MCMCvis::MCMCsummary(nullmod$jags_output, 'b')$n.eff, 0.9),
                            hpd(MCMCvis::MCMCsummary(nullmod$jags_output, 'ypred')$n.eff, 0.9))

  # Calculate model out of sample DRPS and 90% interval coverage
  model_drps <- data.frame(model = c('null', 'hierarchical', 'mgcv_hierarchical'),
                           drps_lower = NA,
                           drps_med = NA,
                           drps_upper = NA)
  all_drps <- do.call(rbind, lapply(seq_len(run_parameters$n_series[x]), function(series){
    calculate_drps(out_gam_mod = nullmod, data_test = sim_data$data_test,
                   data_train = sim_data$data_train, series = series, interval_width = 0.9)
  })) %>%
    dplyr::mutate(norm_drps = DRPS * (1 / Horizon))

  model_drps[1,] <- c('null', quantile(all_drps$norm_drps, probs = c(0.1, 0.5, 0.9), na.rm = T))

  model_coverages <- data.frame(model = c('null', 'hierarchical', 'mgcv_hierarchical'),
                           coverage = NA)
  model_coverages[1,] <- c('null', sum(all_drps$In_90, na.rm = T) / length(which(!is.na(all_drps$Truth))))

  # Calculate model in-sample deviance
  model_dics <- data.frame(model = c('null', 'hierarchical'),
                           dic_lower = NA,
                           dic_med = NA,
                           dic_upper = NA)
  model_dics[1,] <- c('null', hpd(dic.samples(nullmod$jags_model, 1000)$deviance, 0.9))

  # Calculate trend correlations
  model_correlations <- data.frame(model = c('null', 'hierarchical'),
                                  correlations_wrong = NA,
                                  total_correlations = NA)
  correlations <- lv_correlations(object = nullmod, data_train = sim_data$data_train)

  # Compare to truth by calculating proportion of true associations (>0.2) that were correctly detected
  compare_correlations = function(truth, estimated){
    truth[abs(truth) < 0.2] <- 0
    truth[upper.tri(truth)] <- NA
    diag(truth) <- 0
    estimated[abs(truth) < 0.2] <- 0
    estimated[upper.tri(estimated)] <- NA
    diag(estimated) <- 0
    corr_dif_signs <- (sign(truth) - sign(estimated)) / 2
    corr_dif_signs[upper.tri(corr_dif_signs)] <- NA
    data.frame(correlations_wrong = length(which(abs(corr_dif_signs) > 0)),
               total_correlations = length(which(abs(truth) > 0)))
  }

  model_correlations[1,] <- c('null', compare_correlations(sim_data$true_corrs,
                                                           correlations$mean_correlations))

  # Fit a multivariate gam with hierarchical seasonality
  t <- Sys.time()
  hier_mod <- mvjagam(data_train = sim_data$data_train,
                      data_test = sim_data$data_test,
                      formula = y ~ s(season, k = 4, m = 2, bs = 'cc') +
                                        s(season, by = series, m = 1, k = 8),
                      family = 'nb',
                      trend_model = 'AR3',
                      use_lv = T,
                      n.burnin = 25000,
                      n.iter = 5000,
                      thin = 5,
                      auto_update = T)
  difftime(Sys.time(), t, units = "mins")[[1]] -> comp_time

  # Extract all performance information
  model_efficiency[2,] <- c('hierarchical', comp_time,
                            hpd(MCMCvis::MCMCsummary(hier_mod$jags_output, 'b')$n.eff, 0.9),
                            hpd(MCMCvis::MCMCsummary(hier_mod$jags_output, 'ypred')$n.eff, 0.9))
  all_drps <- do.call(rbind, lapply(seq_len(run_parameters$n_series[x]), function(series){
    calculate_drps(out_gam_mod = hier_mod, data_test = sim_data$data_test,
                   data_train = sim_data$data_train, series = series, interval_width = 0.9)
  })) %>%
    dplyr::mutate(norm_drps = DRPS * (1 / Horizon))

  model_drps[2,] <- c('hierarchical', quantile(all_drps$norm_drps, probs = c(0.1, 0.5, 0.9), na.rm = T))
  model_dics[2,] <- c('hierarchical', hpd(dic.samples(hier_mod$jags_model, 1000)$deviance, 0.9))
  model_coverages[2,] <- c('hierarchical', sum(all_drps$In_90, na.rm = T) / length(which(!is.na(all_drps$Truth))))
  correlations <- lv_correlations(object = hier_mod, data_train = sim_data$data_train)
  model_correlations[2,] <- c('hierarchical', compare_correlations(sim_data$true_corrs,
                                                             correlations$mean_correlations))

  # Fit a standard mgcv version of the hierarchical model as a final comparison, including
  # non-wiggly series-specific year smooths with penalty on the first derivative
  mgcv_hier_mod <- gam(y ~ s(season, k = 4, m = 2, bs = 'cc') +
                         s(season, by = series, m = 1, k = 8) +
                         s(year, by = series, k = 3, bs = 'gp', m = 1),
                       family = nb(), data = sim_data$data_train)

  # Extract posterior predictions from the mgcv model and calculate out of sample DRPS
  Xp <- rbind(predict(mgcv_hier_mod, type = 'lpmatrix'),
              predict(mgcv_hier_mod, newdata = sim_data$data_test, type = 'lpmatrix'))
  vc <- vcov(mgcv_hier_mod)
  sim <- MASS::mvrnorm(dim(MCMCvis::MCMCchains(hier_mod$jags_output, 'ypred'))[1],
                       mu = coef(mgcv_hier_mod), Sigma = vc)
  dims_needed <- dim(exp(Xp %*% t(sim)))
  mus <- as.vector(exp(Xp %*% t(sim)))
  fits <- rnbinom(prod(dims_needed), mu = mus, size = mgcv_hier_mod$family$getTheta(TRUE))
  fits <- t(matrix(fits, nrow = dims_needed[1], ncol = dims_needed[2]))

  all_drps <- do.call(rbind, lapply(seq_len(run_parameters$n_series[x]), function(series){
    calculate_drps(out_gam_mod = hier_mod, pred_matrix = fits, data_test = sim_data$data_test,
                   data_train = sim_data$data_train, series = series, interval_width = 0.9)
  })) %>%
    dplyr::mutate(norm_drps = DRPS * (1 / Horizon))
  model_drps[3,] <- c('mgcv_hierarchical', quantile(all_drps$norm_drps, probs = c(0.1, 0.5, 0.9), na.rm = T))
  model_coverages[3,] <- c('mgcv_hierarchical', sum(all_drps$In_90, na.rm = T) / length(which(!is.na(all_drps$Truth))))

  rm(hier_mod, mgcv_hier_mod, nullmod)
  gc()

  # Return performance information
  model_efficiency$T= run_parameters$T[x]
  model_efficiency$n_series = run_parameters$n_series[x]
  model_efficiency$size_obs = run_parameters$size_obs[x]
  model_efficiency$prop_missing = run_parameters$prop_missing[x]
  list(model_correlations = model_correlations,
       model_dics = model_dics,
       model_drps = model_drps,
       model_coverages = model_coverages,
       model_efficiencies = model_efficiency)

})
dir.create('Results')
save(sim_results, file = 'Results/sim_results.rda')
