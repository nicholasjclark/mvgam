#### NEON mvgam utility functions ####
# Utility functions, too specific to put into the package but useful for our study
calculate_pit = function(out_gam_mod, series, data_test, data_train){

  # Pull out predictions and truths for the specific series
  ends <- seq(0, dim(MCMCvis::MCMCchains(out_gam_mod$jags_output, 'ypred'))[2],
              length.out = NCOL(out_gam_mod$ytimes) + 1)
  starts <- ends + 1
  starts <- c(1, starts[-c(1, (NCOL(out_gam_mod$ytimes)+1))])
  ends <- ends[-1]

  site_name <- levels(data_train$series)[series]
  last_obs <- max(data_train %>%
                    dplyr::left_join(data_train %>%
                                       dplyr::select(year, season) %>%
                                       dplyr::distinct() %>%
                                       dplyr::arrange(year, season) %>%
                                       dplyr::mutate(time = dplyr::row_number()),
                                     by = c('season', 'year')) %>%
                    dplyr::filter(series == site_name) %>%
                    dplyr::pull(time))
  truth <- data_test %>%
    dplyr::filter(series == site_name) %>%
    dplyr::arrange(year, season) %>%
    dplyr::pull(y)

  preds <- MCMCvis::MCMCchains(out_gam_mod$jags_output, 'ypred')[,starts[series]:ends[series]]
  preds <- t(preds[, (last_obs +1):NCOL(preds)])
  if(any(!is.na(truth))){

    preds <- preds[!is.na(truth),1:NCOL(preds)]
    truth <- truth[!is.na(truth)]

    # calculate emipirical cumulative distribution function as
    # Portion of (y_predicted <= y_true)
    n_pred <- ncol(preds)
    P_x <- vapply(seq_along(truth),
                  function(i) {
                    sum(preds[i, ] <= truth[i]) / n_pred
                  },
                  .0)

    P_xm1 <- vapply(seq_along(truth),
                    function(i) {
                      sum(preds[i,] <= truth[i] - 1) / n_pred
                    },
                    .0)
    # 100 replicates for randomised PIT
    u <- replicate(100, P_xm1 + stats::runif(length(truth)) * (P_x - P_xm1))
    pit_hist <- hist(u, breaks = seq(0, 1, by = 0.1), plot = F)$density
    pit_hist <- pit_hist / sum(pit_hist)
  } else {
    pit_hist <- NULL
  }

  return(pit_hist)
}

calculate_drps = function(out_gam_mod, pred_matrix = NULL, series, data_test, data_train, interval_width){

  drps_score <- function(truth, fc, interval_width = 0.9){
    nsum <- 1000
    Fy = ecdf(fc)
    ysum <- 0:nsum
    indicator <- ifelse(ysum - truth >= 0, 1, 0)
    score <- sum((indicator - Fy(ysum))^2)

    # Is value within 90% HPD?
    interval <- hpd(fc, interval_width)
    in_interval <- ifelse(truth <= interval[3] & truth >= interval[1], 1, 0)
    return(c(score, in_interval))
  }

  drps_mcmc_object <- function(truth, fc, interval_width = 0.9){
    indices_keep <- which(!is.na(truth))
    scores <- matrix(NA, nrow = max(indices_keep), ncol = 2)
    for(i in indices_keep){
      scores[i,] <- drps_score(truth = as.vector(truth)[i],
                               fc = fc[i,], interval_width)
    }
    scores
  }

  ends <- seq(0, dim(MCMCvis::MCMCchains(out_gam_mod$jags_output, 'ypred'))[2],
              length.out = NCOL(out_gam_mod$ytimes) + 1)
  starts <- ends + 1
  starts <- c(1, starts[-c(1, (NCOL(out_gam_mod$ytimes)+1))])
  ends <- ends[-1]

  site_name <- levels(data_train$series)[series]
  last_obs <- max(data_train %>%
                    dplyr::left_join(data_train %>%
                                       dplyr::select(year, season) %>%
                                       dplyr::distinct() %>%
                                       dplyr::arrange(year, season) %>%
                                       dplyr::mutate(time = dplyr::row_number()),
                                     by = c('season', 'year')) %>%
                    dplyr::filter(series == site_name) %>%
                    dplyr::pull(time))
  truth <- data_test %>%
    dplyr::filter(series == site_name) %>%
    dplyr::arrange(year, season) %>%
    dplyr::pull(y)

  if(is.null(pred_matrix)){
    preds <- MCMCvis::MCMCchains(out_gam_mod$jags_output, 'ypred')[,starts[series]:ends[series]]
    preds <- t(preds[, (last_obs +1):NCOL(preds)])
  } else {
    preds <- t(pred_matrix[, (last_obs +1):NCOL(pred_matrix)])
  }

  if(any(!is.na(truth))){
    DRPS <- data.frame(drps_mcmc_object(truth, preds))
    DRPS <- cbind(truth[1:NROW(DRPS)], DRPS, seq(1:NROW(DRPS)))
  } else {
    DRPS <- data.frame(matrix(NA, nrow = length(truth), ncol = 2))
    DRPS <- cbind(truth[1:NROW(DRPS)], DRPS, seq(1:NROW(DRPS)))
  }

  colnames(DRPS) <- c('Truth', 'DRPS', paste0('In_', interval_width * 100), 'Horizon')
  DRPS$Series <- site_name
  DRPS %>%
    dplyr::select(Series, Horizon, dplyr::everything())
}

plot_mvgam_season = function(out_gam_mod, series, data_test, data_train,
                             xlab = 'Season'){

  pred_dat <- expand.grid(series = levels(data_train$series)[series],
                          season = seq(min(data_train$season),
                                       max(data_train$season), length.out = 100),
                          year = mean(data_train$year),
                          cum_gdd = mean(data_train$cum_gdd, na.rm = T),
                          siteID = as.character(unique(data_train$siteID[which(data_train$series ==
                                                                                 levels(data_train$series)[series])])))
  Xp <- predict(out_gam_mod$mgcv_model, newdata = pred_dat, type = 'lpmatrix')
  betas <- MCMCvis::MCMCchains(out_gam_mod$jags_output, 'b')
  plot((Xp %*% betas[1,]) ~ pred_dat$season, ylim = range((Xp %*% betas[1,]) + 2.5,
                                                                         (Xp %*% betas[1,]) - 2.5),

       col = rgb(150, 0, 0, max = 255, alpha = 10), type = 'l',
       ylab = paste0('s(season) for ', levels(data_train$series)[series]),
       xlab = xlab, xaxt = 'n')
  axis(1, at = seq(0, 26, by = 5), labels = seq(0, 26, by = 5) + 14, cex.axis = 1)
  rect(xleft = 4, xright = 18, ybottom = -100, ytop = 100, col = 'gray90',
       border = NA)
  text(x=11,y=max((Xp %*% betas[1,]) + 2.3), labels = 'Peak tick season')
  for(i in 2:1000){
    lines((Xp %*% betas[i,]) ~ pred_dat$season,

          col = rgb(150, 0, 0, max = 255, alpha = 10))
  }
  box()

}

plot_mvgam_gdd = function(out_gam_mod, series, data_test, data_train,
                          mean_gdd, sd_gdd,
                          xlab = 'Season'){

  pred_dat <- expand.grid(series = levels(data_train$series)[series],
                          season = 20,
                          year = mean(data_train$year),
                          cum_gdd = seq(min(data_train$cum_gdd[which(data_train$series ==
                                                                       levels(data_train$series)[series])],
                                            na.rm = T),
                                        max(data_train$cum_gdd[which(data_train$series ==
                                                                       levels(data_train$series)[series])],
                                            na.rm = T), length.out = 100),
                          siteID = as.character(unique(data_train$siteID[which(data_train$series ==
                                                                                 levels(data_train$series)[series])])))
  Xp <- predict(out_gam_mod$mgcv_model, newdata = pred_dat, type = 'lpmatrix')
  betas <- MCMCvis::MCMCchains(out_gam_mod$jags_output, 'b')
  preds <- matrix(NA, nrow = 1000, ncol = length(pred_dat$cum_gdd))
  for(i in 1:1000){
    preds[i,] <- rnbinom(length(pred_dat$cum_gdd), mu = exp((Xp %*% betas[i,])),
                         size = MCMCvis::MCMCsummary(out_gam_mod$jags_output, 'r')$mean)
  }
  int <- apply(preds,
               2, hpd, 0.95)
  preds_last <- preds[1,]
  covar_vals <- (pred_dat$cum_gdd * sd_gdd) + mean_gdd
  plot(preds_last ~ covar_vals,
       type = 'l', ylim = c(0, max(int) + 2),
       col = rgb(1,0,0, alpha = 0),
       ylab = paste0('Predicted peak count for ', levels(data_train$series)[series]),
       xlab = 'Cumulative growing degree days')
  int[int<0] <- 0
  polygon(c(covar_vals, rev(covar_vals)),
          c(int[1,],rev(int[3,])),
          col = rgb(150, 0, 0, max = 255, alpha = 100), border = NA)
  int <- apply(preds,
               2, hpd, 0.8)
  int[int<0] <- 0
  polygon(c(covar_vals, rev(covar_vals)),
          c(int[1,],rev(int[3,])),
          col = rgb(150, 0, 0, max = 255, alpha = 180), border = NA)
  lines(int[2,] ~ covar_vals, col = rgb(150, 0, 0, max = 255), lwd = 2, lty = 'dashed')

  rug(((data_train$cum_gdd * sd_gdd) + mean_gdd)[which(data_train$series ==
                                                         levels(data_train$series)[series])],
      lwd = 1.75, ticksize = 0.025)
}

prep_neon_data = function(species = 'Ambloyomma_americanum', split_prop = 0.9){
  # Prep data for modelling
  if(species == 'Ambloyomma_americanum'){

    plotIDs <- c('SCBI_013', 'SERC_001', 'SERC_005', 'SERC_006',
                 'SERC_002', 'SERC_012', 'KONZ_025', 'UKFS_001',
                 'UKFS_004', 'UKFS_003', 'ORNL_002', 'ORNL_040',
                 'ORNL_008', 'ORNL_007', 'ORNL_009', 'ORNL_003',
                 'TALL_001', 'TALL_008', 'TALL_002')
    model_dat <- all_neon_tick_data %>%
      dplyr::mutate(target = amblyomma_americanum) %>%
      dplyr::filter(plotID %in% plotIDs) %>%
      dplyr::select(Year, epiWeek, plotID, target) %>%
      dplyr::mutate(epiWeek = as.numeric(epiWeek)) %>%
      dplyr::filter(Year > 2014 & Year < 2021) %>%
      dplyr::mutate(Year_orig = Year)

    model_dat %>%
      dplyr::full_join(expand.grid(plotID = unique(model_dat$plotID),
                                   Year_orig = unique(model_dat$Year_orig),
                                   epiWeek = seq(1, 52))) %>%
      dplyr::left_join(all_neon_tick_data %>%
                         dplyr::select(siteID, plotID) %>%
                         dplyr::distinct()) %>%
      # Remove winter tick abundances as we are not interested in modelling them
      dplyr::filter(epiWeek > 14) %>%
      dplyr::filter(epiWeek < 41) %>%
      dplyr::mutate(series = plotID,
                    season = epiWeek - 14,
                    year = as.vector(scale(Year_orig)),
                    y = target) %>%
      dplyr::select(-Year, -epiWeek, -target) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(Year_orig, season, series) -> model_dat

    model_dat %>%
      dplyr::left_join(all_neon_tick_data %>%
                         dplyr::ungroup() %>%
                         dplyr::select(Year, siteID, cum_sdd, cum_gdd) %>%
                         dplyr::mutate(Year_orig = Year) %>%
                         dplyr::select(-Year) %>%
                         dplyr::distinct()) -> model_dat

    model_dat = model_dat %>%
      # Only include sites that have multiple plots
      dplyr::filter(siteID %in% c('SERC', 'TALL', 'UKFS', 'ORNL'),
                    Year_orig <= 2019) %>%
      # ID variables need to be factors for JAGS modelling
      dplyr::mutate(plotID = factor(plotID),
                    siteID = factor(siteID),
                    series = factor(series))
  }

  if(species == 'Ixodes_scapularis'){

    plotIDs <- c('BLAN_012', 'BLAN_005','SCBI_013','SCBI_002',
                 'SERC_001','SERC_005','SERC_006','SERC_012','ORNL_007')
    model_dat <- all_neon_tick_data %>%
      dplyr::mutate(target = ixodes_scapularis) %>%
      dplyr::filter(plotID %in% plotIDs) %>%
      dplyr::select(Year, epiWeek, plotID, target) %>%
      dplyr::mutate(epiWeek = as.numeric(epiWeek)) %>%
      dplyr::filter(Year > 2014 & Year < 2021) %>%
      dplyr::mutate(Year_orig = Year)

    model_dat %>%
      dplyr::full_join(expand.grid(plotID = unique(model_dat$plotID),
                                   Year_orig = unique(model_dat$Year_orig),
                                   epiWeek = seq(1, 52))) %>%
      dplyr::left_join(all_neon_tick_data %>%
                         dplyr::select(siteID, plotID) %>%
                         dplyr::distinct()) %>%
      # Remove winter tick abundances as we are not interested in modelling them
      dplyr::filter(epiWeek > 14) %>%
      dplyr::filter(epiWeek < 41) %>%
      dplyr::mutate(series = plotID,
                    season = epiWeek - 14,
                    year = as.vector(scale(Year_orig)),
                    y = target) %>%
      dplyr::select(-Year, -epiWeek, -target) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(Year_orig, season, series) -> model_dat

    model_dat %>%
      dplyr::left_join(all_neon_tick_data %>%
                         dplyr::ungroup() %>%
                         dplyr::select(Year, siteID, cum_sdd, cum_gdd) %>%
                         dplyr::mutate(Year_orig = Year) %>%
                         dplyr::select(-Year) %>%
                         dplyr::distinct()) -> model_dat

    model_dat = model_dat %>%
      # Only include sites that have multiple plots
      dplyr::filter(siteID %in% c('SERC', 'BLAN', 'SCBI'),
                    Year_orig <= 2019) %>%
      # ID variables need to be factors for JAGS modelling
      dplyr::mutate(plotID = factor(plotID),
                    siteID = factor(siteID),
                    series = factor(series))
  }

  # Scale the environmental covariate and store the mean and sd for later plotting
  sd_gdd <- sd(model_dat$cum_gdd)
  mean_gdd <- mean(model_dat$cum_gdd)
  model_dat$cum_gdd <- (model_dat$cum_gdd - mean_gdd) / sd_gdd

  # Split into training and testing
  data_train = model_dat[1:(floor(nrow(model_dat) * split_prop)),]
  data_test = model_dat[((floor(nrow(model_dat) * split_prop)) + 1):nrow(model_dat),]

  list(data_train = data_train, data_test = data_test,
       species = species, sd_gdd = sd_gdd, mean_gdd = mean_gdd)

}

fit_mvgam = function(data_train,
                     data_test,
                     formula,
                     formula_name,
                     family = 'nb',
                     use_lv = TRUE,
                     n_lv = 5,
                     phi_prior,
                     tau_prior,
                     knots,
                     n.burnin = 1000,
                     n.iter = 1000,
                     thin = 2,
                     auto_update = FALSE,
                     interval_width = 0.9){

  # Condition the model on the observed data
  if(missing(knots)){
    out_gam_mod <- mvjagam(formula = formula,
                           data_train = data_train,
                           data_test = data_test,
                           n.burnin = n.burnin,
                           n.iter = n.iter,
                           thin = thin,
                           auto_update = auto_update,
                           use_lv = use_lv,
                           n_lv = n_lv,
                           family = family,
                           phi_prior = phi_prior,
                           tau_prior = tau_prior)
  } else {
    out_gam_mod <- mvjagam(formula = formula,
                           data_train = data_train,
                           data_test = data_test,
                           n.burnin = n.burnin,
                           n.iter = n.iter,
                           knots = knots,
                           thin = thin,
                           auto_update = auto_update,
                           use_lv = use_lv,
                           n_lv = n_lv,
                           family = family,
                           phi_prior = phi_prior,
                           tau_prior = tau_prior)
  }


  #### If GAM component is LESS supported, we should see evidence in the form of: ####
  # 1. Poorer convergence of smoothing parameter estimates, suggesting the model
  # is more 'mis-specified' and harder to fit
  rho_summary <- MCMCvis::MCMCsummary(out_gam_mod$jags_output,
                                       c('rho'), HPD = TRUE)

  # 2. Stronger residual correlations, suggesting we are missing some site-level structure
  # Calculate the correlation matrix from the latent trends
    correlations <- lv_correlations(object = out_gam_mod, data_train = data_train)

    # Plot trend correlations
    library(ggplot2)
    mean_correlations <- correlations$mean_correlations
    mean_correlations[upper.tri(mean_correlations)] <- NA
    mean_correlations <- data.frame(mean_correlations)
    resid_corr_plot <- ggplot(mean_correlations %>%
             tibble::rownames_to_column("series1") %>%
             tidyr::pivot_longer(-c(series1), names_to = "series2", values_to = "Correlation"),
           aes(x = series1, y = series2)) + geom_tile(aes(fill = Correlation)) +
      scale_fill_gradient2(low="darkred", mid="white", high="darkblue",
                           midpoint = 0,
                           breaks = seq(-1,1,length.out = 5),
                           limits = c(-1, 1),
                           name = 'Trend\ncorrelation') + labs(x = '', y = '') + theme_dark() +
      theme(axis.text.x = element_text(angle = 45, hjust=1))


  resid_corr_summary <- summary(as.vector(as.matrix(mean_correlations)))


  # 4. Poorer forecasts
  # Calculate Discrete Rank Probability Score and nominal coverage of 90% credible interval for
  # each unique series
  all_drps <- do.call(rbind, lapply(seq_len(length(unique(data_train$series))), function(series){
    calculate_drps(out_gam_mod = out_gam_mod, data_test = data_test,
                   data_train = data_train, series = series, interval_width = 0.9)
  }))
  all_drps$Formula = formula_name

  # Calculate PIT histograms per series
  all_pit <- data.frame(do.call(rbind, lapply(seq_len(length(unique(data_train$series))), function(series){
    calculate_pit(out_gam_mod = out_gam_mod, data_test = data_test,
                   data_train = data_train, series = series)
  })))
  all_pit$Formula = formula_name

    out <- list(out_gam_mod = out_gam_mod,
                rho_summary = data.frame('rhat_lower' = as.vector(summary(rho_summary$Rhat)[3]),
                                         'rhat_upper' = as.vector(summary(rho_summary$Rhat)[4]),
                                         'n_eff_lower' = as.vector(summary(rho_summary$n.eff)[3]),
                                         'n_eff_upper' = as.vector(summary(rho_summary$n.eff)[4]),
                                         'Formula' = formula_name),
                resid_corr_plot = resid_corr_plot,
                resid_corr_summary = data.frame('Corr_lower' = as.vector(resid_corr_summary[2]),
                                                'Corr_upper' = as.vector(resid_corr_summary[5]),
                                                'Formula' = formula_name),
                mean_correlations = mean_correlations,
                DRPS_scores = all_drps,
                PIT_scores = all_pit)
return(out)

}
