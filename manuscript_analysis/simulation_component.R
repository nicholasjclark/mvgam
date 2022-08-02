#### Simulation component of mvgam / NEON manuscript ####
library(mvgam)
library(dplyr)
source('neon_utility_functions.R')

# Use hierarchical seasonality for all;
# trend components: 0.3 and 0.7
# N_series: 2, 4, 12
# Length: 6 years
# Missingness: none, 10%, 50%
run_parameters <- expand.grid(n_series = c(2, 4, 12),
                              T = c(72),
                              prop_missing = c(0, 0.1, 0.5),
                              trend_rel = c(0.3, 0.7),
                              stringsAsFactors = F)

# Run each simulation scenario 4 times (a total of 72 simulations)
run_parameters <- rbind(run_parameters,
                        run_parameters,
                        run_parameters,
                        run_parameters)

# Run models and extract performance indicators
sim_results <- lapply(seq_len(nrow(run_parameters)), function(x){
  cat('\nModelling row', x, '\n')
  sim_data <- sim_mvgam(T = run_parameters$T[x],
                        n_series = run_parameters$n_series[x],
                        prop_missing = run_parameters$prop_missing[x],
                        train_prop = 0.833,
                        mu_obs = 6,
                        phi_obs = 5,
                        family = 'nb',
                        trend_rel = run_parameters$trend_rel[x],
                        seasonality = 'hierarchical')

  # Fit a Random Walk dynamic factor DGAM with no seasonality (null model)
  nullmod <- mvgam(data_train = sim_data$data_train,
                   data_test = sim_data$data_test,
                   formula = y ~ 1,
                   family = 'nb',
                   use_lv = TRUE,
                   trend_model = 'RW',
                   use_stan = TRUE)

  # Calculate model out of sample DRPS and 90% interval coverage
  model_drps <- data.frame(model = c('null', 'hierarchical', 'mgcv_hierarchical', 'mgcv_autoregressive'),
                           drps_lower = NA,
                           drps_med = NA,
                           drps_upper = NA)
  all_drps <- do.call(rbind, lapply(seq_len(run_parameters$n_series[x]), function(series){
    calculate_drps(out_gam_mod = nullmod, data_test = sim_data$data_test,
                   data_train = sim_data$data_train, series = series, interval_width = 0.9)
  })) %>%
    dplyr::mutate(norm_drps = DRPS * (1 / Horizon))

  model_drps[1,] <- c('null', quantile(all_drps$norm_drps, probs = c(0.1, 0.5, 0.9), na.rm = T))

  model_coverages <- data.frame(model = c('null', 'hierarchical', 'mgcv_hierarchical',
                                          'mgcv_autoregressive'),
                                coverage = NA)
  model_coverages[1,] <- c('null', sum(all_drps$In_90, na.rm = T) / length(which(!is.na(all_drps$Truth))))

  # Calculate trend correlations
  model_correlations <- data.frame(model = c('null', 'hierarchical'),
                                   correlations_wrong = NA,
                                   total_correlations = NA)
  correlations <- lv_correlations(object = nullmod)

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

  # Fit a dynamic factor DGAM with hierarchical seasonality
  hier_mod <- mvgam(data_train = sim_data$data_train,
                    data_test = sim_data$data_test,
                    formula = y ~ s(season, k = 10, m = 2, bs = 'cc') +
                      s(season, by = series, m = 1, k = 6),
                    knots = list(season = c(0.5, 12.5)),
                    family = 'nb',
                    use_lv = TRUE,
                    trend_model = 'RW',
                    use_stan = TRUE)

  # Extract all performance information
  all_drps <- do.call(rbind, lapply(seq_len(run_parameters$n_series[x]), function(series){
    calculate_drps(out_gam_mod = hier_mod, data_test = sim_data$data_test,
                   data_train = sim_data$data_train, series = series, interval_width = 0.9)
  })) %>%
    dplyr::mutate(norm_drps = DRPS * (1 / Horizon))

  model_drps[2,] <- c('hierarchical', quantile(all_drps$norm_drps, probs = c(0.1, 0.5, 0.9), na.rm = T))
  model_coverages[2,] <- c('hierarchical', sum(all_drps$In_90, na.rm = T) / length(which(!is.na(all_drps$Truth))))
  correlations <- lv_correlations(object = hier_mod)
  model_correlations[2,] <- c('hierarchical', compare_correlations(sim_data$true_corrs,
                                                                   correlations$mean_correlations))

  # Fit a standard mgcv version of the hierarchical model as a final comparison, including
  # hierarchical yearly smooths to capture the long-term trend
  mgcv_hier_mod <- gam(y ~ s(season, k = 10, m = 2, bs = 'cc') +
                         s(season, by = series, m = 1, k = 6) +
                         s(year, k = 4) +
                         s(year, by = series, k = 4, m = 1),
                       knots = list(season = c(0.5, 12.5)),
                       family = nb(), data = sim_data$data_train)

  # Extract posterior predictions from the mgcv model and calculate out of sample DRPS
  Xp <- rbind(predict(mgcv_hier_mod, type = 'lpmatrix'),
              predict(mgcv_hier_mod, newdata = sim_data$data_test, type = 'lpmatrix'))
  vc <- vcov(mgcv_hier_mod)
  sim <- MASS::mvrnorm(dim(MCMCvis::MCMCchains(hier_mod$model_output, 'ypred'))[1],
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
  model_drps[3,] <- c('mgcv_hierarchical',
                      quantile(all_drps$norm_drps, probs = c(0.1, 0.5, 0.9), na.rm = T))
  model_coverages[3,] <- c('mgcv_hierarchical',
                           sum(all_drps$In_90, na.rm = T) / length(which(!is.na(all_drps$Truth))))

  # Finally, fit an mgcv version with a stochastic trend and hierarchical seasonality, but only
  # do this if prop_missing is 0 or 0.1 (if 0.5, there will not be enough complete.cases observations
  # to estimate model parameters; this is one of the major drawbacks of the autoregressive observation
  # model form)
  if(run_parameters$prop_missing[x] < 0.5){
    rbind(sim_data$data_train,
          sim_data$data_test) %>%
      # A lag of n_series is needed to ensure the lagged value is for the correct
      # series at each timepoint
      dplyr::mutate(lag1 = log(lag(y, run_parameters$n_series[x]) + 0.01)) -> sim_dat_lags
    sim_dat_lags_train <- sim_dat_lags[1:NROW(sim_data$data_train),]
    sim_dat_lags_test <- sim_dat_lags[(NROW(sim_data$data_train)+1):NROW(sim_dat_lags),]
    mgcv_lag_mod <- gam(y ~ s(season, k = 10, m = 2, bs = 'cc') +
                          s(season, by = series, m = 1, k = 6) +
                          # Ensure a separate AR1 function
                          # is estimated for each series
                          lag1*series,
                        knots = list(season = c(0.5, 12.5)),
                        family = nb(), data = sim_dat_lags_train)

    # Define a function to iteratively propagate a forecast forward according
    # to the AR1 observation model
    recurse_ar1 = function(model, n_series, lagged_vals, h, log_lags = F){
      # Initiate state matrix
      states <- matrix(NA, ncol = n_series, nrow = h+1)
      # Last values of the conditional expectations begins the state matrix
      if(log_lags){
        states[1,] <- exp(as.numeric(lagged_vals))
      } else {
        states[1,] <- as.numeric(lagged_vals)
      }

      # Get a random sample of the smooth coefficient uncertainty matrix
      # to use for the entire forecast horizon of this particular path
      gam_coef_index <- sample(seq(1, NROW(coef_sim)), size = 1)
      # For each following timestep, recursively predict based on the
      # predictions at each previous lag
      for (t in 2:(h + 1)) {
        newdata <- sim_dat_lags_test[(t-1):((t-1)+n_series-1), ]
        if(log_lags){
          newdata$lag1 <- log(states[(t-1), ] + 0.01)
        } else {
          newdata$lag1 <- states[t-1, ]
        }

        colnames(newdata) <- colnames(sim_dat_lags_test)
        Xp <- predict(model, newdata = newdata, type = 'lpmatrix')
        # Calculate the posterior predictions for this timepoint
        mu <- rnbinom(n_series, mu = exp(Xp %*% coef_sim[gam_coef_index,]),
                      size = model$family$getTheta(TRUE))
        # Fill in the state vector and iterate to the next timepoint
        states[t, ] <- mu
      }
      # Return the forecast path
      as.vector(t(states[-c(1),]))
    }

    # Draw 1000 parameter estimates from the GAM posterior and use these to propagate the forecast
    # for 10 timesteps ahead
    coef_sim <- gam.mh(mgcv_lag_mod)$bs

    # Forecasting will fail if there are NAs around the beginning of the test sample, because
    # these NAs will propagate into the lagged estimates. So if there are any, we will impute them
    # from the posterior predictions of the hierarchical GAM above
    if(anyNA(sim_dat_lags_test$lag1[1:run_parameters$n_series[x]])){
      lag_inits <- log(colMeans(fits)[(NROW(sim_data$data_train)+1):
                                        (NROW(sim_data$data_train)+run_parameters$n_series[x])] + 0.01)
    } else {
      lag_inits <- sim_dat_lags_test$lag1[1:run_parameters$n_series[x]]
    }

    # Propagate the stochastic trend model forward to calculate the forecast distribution
    library(parallel)
    cl <- parallel::makePSOCKcluster(parallel::detectCores() - 2)
    setDefaultCluster(cl)
    clusterExport(NULL, c('mgcv_lag_mod',
                          'run_parameters',
                          'lag_inits',
                          'sim_dat_lags_test',
                          'recurse_ar1',
                          'x',
                          'coef_sim'),
                  envir = environment())
    clusterEvalQ(cl, library(mgcv))
    pbapply::pboptions(type = "none")
    gam_sims <- do.call(rbind, pbapply::pblapply(1:1000, function(i){
      recurse_ar1(model = mgcv_lag_mod,
                  n_series = run_parameters$n_series[x],
                  lagged_vals = lag_inits,
                  h = length(unique(sim_dat_lags_test$time)),
                  log_lags = TRUE)
    }, cl = cl))
    stopCluster(cl)

    # Calculate forecast evaluation scores
    all_drps <- do.call(rbind, lapply(seq_len(run_parameters$n_series[x]), function(series){
      calculate_drps(out_gam_mod = hier_mod,
                     pred_matrix = cbind(fits[1:1000,1:NROW(sim_data$data_train)], gam_sims),
                     data_test = sim_data$data_test,
                     data_train = sim_data$data_train,
                     series = series, interval_width = 0.9)
    })) %>%
      dplyr::mutate(norm_drps = DRPS * (1 / Horizon))
    model_drps[4,] <- c('mgcv_autoregressive', quantile(all_drps$norm_drps,
                                                        probs = c(0.1, 0.5, 0.9), na.rm = T))
    model_coverages[4,] <- c('mgcv_autoregressive',
                             sum(all_drps$In_90, na.rm = T) / length(which(!is.na(all_drps$Truth))))

  }

  rm(hier_mod, mgcv_hier_mod, nullmod)
  gc()

  # Return performance information
  list(model_correlations = model_correlations,
       model_drps = model_drps,
       model_coverages = model_coverages)

})
dir.create('Results')
save(sim_results, file = 'Results/sim_results.rda')
