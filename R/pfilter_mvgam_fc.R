#'Forecast from a particle filtered mvjagam object
#'
#'This function generates a forecast from a set of particles that each capture a unique proposal about
#'the current state of the system that was modelled in the mvjagam object. The covariate and timepoint information
#'from \code{data_test} is used to generate the GAM component forecast, while the trends are run forward in time
#'according to their state space dynamics. The forecast is a weighted ensemble, with weights determined by
#'each particle's proposal likelihood prior to the most recent assimilation step
#'
#'@param data_test A \code{dataframe} of test data containing at least 'series', 'season', 'year' and
#''in_season' for the forecast horizon, in addition to any other variables included in the linear predictor of \code{formula}
#'@param n_cores \code{integer} specifying number of cores for generating particle forecasts in parallel
#'@param file_path \code{character} string specifying the file path where the particles have been saved
#'@param legend_position The location may also be specified by setting x to a single keyword from the
#'list "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center".
#'This places the legend on the inside of the plot frame at the given location.
#'@param return_forecasts \code{logical}. If \code{TRUE}, the returned list object will contain plots of forecasts
#'as well as the forecast objects (each as a \code{matrix} of dimension \code{n_particles} x \code{horizon})
#'@return A named \code{list} contaning functions that call base \code{R} plots of each series' forecast. Optionally
#'the actual forecasts are returned within the \code{list} as a separate \code{list} of \code{matrices}
#'@export
pfilter_mvgam_fc = function(file_path = 'pfilter',
                            n_cores = 2,
                            data_test,
                            legend_position = 'topleft',
                            return_forecasts = FALSE){

  if(file.exists(paste0(file_path, '/particles.rda'))){
    load(paste0(file_path, '/particles.rda'))
  } else {
    stop('file_path either does not exist or does not contain a .rda particle list')
  }

  # Function to simulate trends / latent factors ahead using random walks with drift
  sim_rwdrift = function(phi, tau, state, h){
    states <- rep(NA, length = h + 1)
    states[1] <- state
    for (t in 2:(h + 1)) {
      states[t] <- rnorm(1, phi + states[t - 1], sqrt(1 / tau))
    }
    states[-1]
  }

  # Get all observations that have not yet been assimilated
  data_test %>%
    dplyr::arrange(year, season, series) -> data_test
  last_row <- max(which(data_test$season == last_assim[1] & data_test$year == last_assim[2]))
  series_test <- data_test[(last_row + 1):NROW(data_test),]
  n_series <- (length(levels(data_test$series)))
  fc_horizon <- NROW(series_test) / n_series

  # Generate linear predictor matrix
  Xp <- predict(mgcv_model,
                newdata = series_test,
                type = 'lpmatrix')

  # Run particles forward in time to generate their forecasts
  cl <- parallel::makePSOCKcluster(n_cores)
  setDefaultCluster(cl)
  clusterExport(NULL, c('particles',
                        'Xp',
                        'sim_rwdrift',
                        'series_test',
                        'fc_horizon',
                        'n_series',
                        'hpd'),
                envir = environment())

  pbapply::pboptions(type = "none")
  particle_fcs <- pbapply::pblapply(seq_along(particles), function(x){
    use_lv <- particles[[x]]$use_lv

    if(use_lv){

      # Run the latent variables forward fc_horizon timesteps
      lv_preds <- do.call(rbind, lapply(seq_len(particles[[x]]$n_lv), function(lv){
        sim_rwdrift(phi = particles[[x]]$phi[lv],
                    tau = particles[[x]]$tau,
                    state = particles[[x]]$lv_states[lv],
                    h = fc_horizon)
      }))
      series_fcs <- lapply(seq_len(n_series), function(series){
        trend_preds <- as.numeric(t(lv_preds) %*% particles[[x]]$lv_coefs[series,]) *
          (1 - particles[[x]]$gam_comp[series])
        trunc_preds <- rnbinom(fc_horizon,
                               mu = exp(as.vector(particles[[x]]$gam_comp[series] *
                                                    (Xp[which(as.numeric(series_test$series) == series),] %*%
                                                       particles[[x]]$betas)) +
                                          (trend_preds)),
                               size = particles[[x]]$size)
        trunc_preds
      })

    } else {
      # Run the trends forward fc_horizon timesteps
      series_fcs <- lapply(seq_len(n_series), function(series){
          trend_preds <- sim_rwdrift(phi = particles[[x]]$phi[series],
                                        tau = particles[[x]]$tau[series],
                                        state = particles[[x]]$trend_states[series],
                                        h = fc_horizon) * (1 - particles[[x]]$gam_comp[series])
          trunc_preds <- rnbinom(fc_horizon,
                                 mu = exp(as.vector(particles[[x]]$gam_comp[series] *
                                            (Xp[which(as.numeric(series_test$series) == series),] %*%
                                               particles[[x]]$betas)) +
                                                        (trend_preds)),
                                 size = particles[[x]]$size)
          trunc_preds
      })
    }

    series_fcs
  }, cl = cl)
  stopCluster(cl)

  # Extract particle weights and create importance sampling index
  weights <- (unlist(lapply(seq_along(particles), function(x){
    tail(particles[[x]]$weight, 1)})))
  weights <- weights / max(weights)
  index <- sample.int(length(weights), length(weights), replace = TRUE,
                      prob = weights + 0.0001)

  # Weighted forecast for each series
  fc_samples <- sample(index, 5000, T)
  series_fcs <- lapply(seq_len(n_series), function(series){
    indexed_forecasts <- do.call(rbind, lapply(seq_along(fc_samples), function(x){
      particle_fcs[[fc_samples[x]]][[series]]
    }))
    indexed_forecasts
  })
  names(series_fcs) <- levels(data_test$series)

  # Generate plots of forecasts for each series
  obs_data %>%
    dplyr::arrange(year, season, series) -> obs_data

  plot_series_fc = function(series, preds){
    all_obs <- obs_data$y[which(as.numeric(obs_data$series) == series)]
    assimilated <- obs_data$assimilated[which(as.numeric(obs_data$series) == series)]
    preds_last <- c(all_obs, preds[1,])
    int <- apply(preds,
                  2, hpd, 0.9)
    if(!is.null(particles[[1]]$upper_bounds)){
      upper_lim <- min(c(particles[[1]]$upper_bounds[series],
                         (max(c(all_obs, int[3,]), na.rm = T) + 4)))
    } else {
      upper_lim <- max(c(all_obs, int[3,]), na.rm = T) + 4
    }

    plot(preds_last,
         type = 'l', ylim = c(0, upper_lim),
         col = rgb(1,0,0, alpha = 0),
         ylab = paste0('Estimated counts for ', levels(obs_data$series)[series]),
         xlab = 'Time')

    int[int<0] <- 0
    polygon(c(seq((length(all_obs) + 1), length(preds_last)),
              rev(seq((length(all_obs) + 1), length(preds_last)))),
            c(int[1,],rev(int[3,])),
            col = rgb(150, 0, 0, max = 255, alpha = 100), border = NA)
    int <- apply(preds,
                 2, hpd, 0.68)
    int[int<0] <- 0
    polygon(c(seq((length(all_obs) + 1), length(preds_last)),
              rev(seq((length(all_obs) + 1), length(preds_last)))),
            c(int[1,],rev(int[3,])),
            col = rgb(150, 0, 0, max = 255, alpha = 180), border = NA)
    lines(seq((length(all_obs) + 1), length(preds_last)),
          int[2,], col = rgb(150, 0, 0, max = 255), lwd = 2, lty = 'dashed')
    lines(all_obs)
    points(x = which(assimilated == 'no'),
           y = all_obs[which(assimilated == 'no')], pch = 16)
    points(x = which(assimilated == 'yes'), y = all_obs[which(assimilated == 'yes')],
           pch = 8, col = rgb(150, 0, 0, max = 255))
    legend(legend_position,legend=c("Calibration","Assimilation"),
           bg = 'white',
           col=c('black',
                 rgb(150, 0, 0, max = 255)),pch = c(16, 8))
  }

  fc_plots <- lapply(seq_len(n_series), function(series){
    function(){plot_series_fc(series, preds = series_fcs[[series]])}
  })
  names(fc_plots) <- levels(obs_data$series)

  if(!return_forecasts){
    out <- fc_plots
  } else {
    out <- list(fc_plots = fc_plots,
                forecasts = series_fcs)
  }

return(out)
}
