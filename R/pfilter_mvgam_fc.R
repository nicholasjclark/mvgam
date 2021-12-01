#'Forecast from a particle filtered mvjagam object
#'
#'This function generates a forecast from a set of particles that each captures a unique proposal about
#'the current state of the system that was modelled in the mvjagam object. The covariate and timepoint information
#'from \code{data_test} is used to generate the gam component forecast, while the trends are run forward in time
#'according to their state space dynamics
#'
#'@param data_test A \code{dataframe} of test data containing at least 'series', 'season', 'year' and
#''in_season' for the forecast horizon, in addition to any other variables included in the linear predictor of \code{formula}
#'@param n_cores \code{integer} specifying number of cores for generating particle forecasts in parallel
#'@param file_path \code{character} string specifying the file path where the particles have been saved
#'@export
pfilter_mvgam_fc = function(file_path = 'pfilter',
                            n_cores = 2,
                            data_test){

  if(file.exists(paste0(file_path, '/particles.rda'))){
    load(paste0(file_path, '/particles.rda'))
  } else {
    stop('file_path either does not exist or does not contain a .rda particle list')
  }

  # Function to simulate ahead for random walks with drift
  sim_rwdrift = function(phi, tau, state, h){
    states <- rep(NA, length = h + 1)
    states[1] <- state
    for (t in 2:(h + 1)) {
      states[t] <- rnorm(1, phi + states[t - 1], 1 / tau)
    }
    states[-1]
  }

  # Get all observations that have not yet been assimilated
  data_test %>%
    dplyr::arrange(year, season, series) -> data_test
  last_row <- max(which(data_test$season == last_assim[1] & data_test$year == last_assim[2]))
  series_test <- data_test[(last_row + 1):NROW(data_test),]

  n_series <- length(particles[[1]]$trend_states)
  fc_horizon <- NROW(series_test) / n_series

  # Generate linear predictor matrix
  Xp <- predict(mgcv_model,
                newdata = series_test,
                type = 'lpmatrix')

  # GAM component linear predictions
  gam_preds <- lapply(seq_len(n_series), function(series){
  gam_preds <- matrix(NA, nrow = dim(betas)[1], ncol = fc_horizon)
  for(i in 1:dim(betas)[1]){
    gam_preds[i,] <- gam_comps[i,] * (Xp[which(as.numeric(series_test$series) == series),] %*% betas[i,])
  }
  gam_preds
  })

  # Run particles forward in time to generate their forecasts
  cl <- parallel::makePSOCKcluster(n_cores)
  setDefaultCluster(cl)
  clusterExport(NULL, c('particles',
                        'sim_rwdrift',
                        'fc_horizon',
                        'gam_preds',
                        'gam_comps',
                        'n_series'),
                envir = environment())

  pbapply::pboptions(type = "none")
  particle_fcs <- pbapply::pblapply(seq_along(particles), function(x){
    use_lv <- particles[[x]]$use_lv

    if(use_lv){

      # Run the latent variables forward fc_horizon timesteps
      n_lv <- particles[[x]]$n_lv
      next_lvs <- do.call(cbind, lapply(seq_len(n_lv), function(lv){
        sim_rwdrift(phi = particles[[x]]$phi[lv],
                    tau = particles[[x]]$tau,
                    state = particles[[x]]$lv_states[lv],
                    h = fc_horizon)
      }))

      # Multiply with loadings to generate the series' forecast trend states
      trends <- next_lvs %*% t(particles[[x]]$lv_coefs)

    } else {
      # Run the trends forward fc_horizon timesteps
      trends <- do.call(cbind, lapply(seq_len(n_series), function(series){
        sim_rwdrift(phi = particles[[x]]$phi[series],
                    tau = particles[[x]]$tau[series],
                    state = particles[[x]]$trend_states[series],
                    h = fc_horizon)
      }))
    }

    # Calculate forecast distributions for the particle
    series_fcs <- lapply(seq_len(n_series), function(series){
      trend_preds <- t(trends[,series] %*% t(1 - gam_comps[,series]))
      full_preds <- matrix(NA, nrow = NROW(trend_preds), ncol = fc_horizon)
      for(i in 1:NROW(trend_preds)){
        trunc_preds <- rnbinom(fc_horizon, mu = exp(gam_preds[[series]][i, ] + trend_preds[i, ]),
                               size = particles[[x]]$size)

        if(!is.null(particles[[x]]$upper_bounds)){
          trunc_preds[trunc_preds > particles[[x]]$upper_bounds[series]] <- particles[[x]]$upper_bounds[series]
        }

        full_preds[i,] <- trunc_preds
      }
      full_preds
    })

    series_fcs
  }, cl = cl)
  stopCluster(cl)
  rm(gam_preds)

  # Extract particle weights and create importance sampling index
  weights <- (unlist(lapply(seq_along(particles), function(x){
    tail(particles[[x]]$weight, 1)})))
  weights[is.na(weights)] <- 1
  index <- sample.int(length(weights), length(weights), replace = TRUE,
                      prob = weights + 0.1)

  # Weighted forecast for each series
  series_fcs <- lapply(seq_len(n_series), function(series){
    indexed_forecasts <- do.call(rbind, lapply(seq_along(index), function(x){
      particle_fcs[[x]][[series]]
    }))
    fc_sample <- sample(1:NROW(indexed_forecasts), 1000, F)
    indexed_forecasts <- indexed_forecasts[fc_sample,]
    indexed_forecasts
  })
  names(series_fcs) <- levels(data_test$series)

  return(series_fcs)
}
