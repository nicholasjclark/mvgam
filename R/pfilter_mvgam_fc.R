#'Forecast from a particle filtered mvjagam object
#'
#'This function generates a forecast from a set of particles that each capture a unique proposal about
#'the current state of the system that was modelled in the mvjagam object. The covariate and timepoint information
#'from \code{data_test} is used to generate the GAM component forecast, while the trends are run forward in time
#'according to their state space dynamics. The forecast is a weighted ensemble, with weights determined by
#'each particle's proposal likelihood prior to the most recent assimilation step
#'
#'@param data_test A \code{dataframe} or \code{list} of test data containing at least 'series' and time',
#'in addition to any other variables included in the linear predictor of \code{formula}
#'@param n_cores \code{integer} specifying number of cores for generating particle forecasts in parallel
#'@param file_path \code{character} string specifying the file path where the particles have been saved
#'@param plot_legend \code{logical} stating whether to include a legend to highlight which observations
#'were used for calibration and which were assimilated by the particle filter
#'@param legend_position The legend location may be specified by setting x to a single keyword from the
#'list "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center".
#'This places the legend on the inside of the plot frame at the given location.
#'@param ylim Optional \code{vector} of y-axis limits (min, max). The same limits will be used for all plots
#'@param return_forecasts \code{logical}. If \code{TRUE}, the returned list object will contain plots of forecasts
#'as well as the forecast objects (each as a \code{matrix} of dimension \code{n_particles} x \code{horizon})
#'@return A named \code{list} contaning functions that call base \code{R} plots of each series' forecast. Optionally
#'the actual forecasts are returned within the \code{list} as a separate \code{list} of \code{matrices}
#'@export
pfilter_mvgam_fc = function(file_path = 'pfilter',
                            n_cores = 2,
                            data_test,
                            plot_legend = TRUE,
                            legend_position = 'topleft',
                            ylim,
                            return_forecasts = FALSE){

  if(file.exists(paste0(file_path, '/particles.rda'))){
    load(paste0(file_path, '/particles.rda'))
  } else {
    stop('file_path either does not exist or does not contain a .rda particle list')
  }

  # Extract particle weights and create importance sampling index
  weights <- (unlist(lapply(seq_along(particles), function(x){
    tail(particles[[x]]$weight, 1)})))
  weights <- weights / max(weights)
  index <- sample.int(length(weights), length(weights), replace = TRUE,
                      prob = weights)
  fc_samples <- sample(index, min(10000, length(weights)), T)

  # Function to simulate trends / latent factors ahead using ar3 model
  sim_ar3 = function(phi, ar1, ar2, ar3, tau, state, h){
    states <- rep(NA, length = h + 3)
    states[1] <- state[1]
    states[2] <- state[2]
    states[3] <- state[3]
    for (t in 4:(h + 3)) {
      states[t] <- rnorm(1, phi + ar1*states[t - 1] +
                           ar2*states[t - 2] +
                           ar3*states[t - 3], sqrt(1 / tau))
    }
    states[-c(1:3)]
  }

  if(missing(ylim)){
    ylim <- c(NA, NA)
  }

  # Get all observations that have not yet been assimilated
  if(class(data_test)[1] == 'list'){

    if(!'series' %in% names(data_test)){
      data_test$series <- factor('series1')
    }

    if(!'time' %in% names(data_test)){
      stop('data_test does not contain a "time" column')
    }

    data_test_orig <- data_test
    list_names <- names(data_test_orig)
    data_test = data.frame(time = data_test$time,
                            series = data_test$series) %>%
      dplyr::mutate(index = dplyr::row_number()) %>%
      dplyr::arrange(time, series)

    data_test_orig <- lapply(data_test_orig, function(x){
      if(is.matrix(x)){
        matrix(x[data_test$index,], ncol = NCOL(x))
      } else {
        x[data_test$index]
      }

    })
    names(data_test_orig) <- list_names
    last_row <- max(which(data_test$time == last_assim))

    series_test <- lapply(data_test_orig, function(x){
      if(is.matrix(x)){
        matrix(x[(last_row + 1):NROW(data_test),], ncol = NCOL(x))
      } else {
        x[(last_row + 1):NROW(data_test)]
      }
    })

  } else {
    if(!'series' %in% colnames(data_test)){
      data_test$series <- factor('series1')
    }

    if(!'time' %in% colnames(data_test)){
      stop('data_test does not contain a "time" column')
    }

    data_test %>%
      dplyr::arrange(time, series) -> data_test
    last_row <- max(which(data_test$time == last_assim))
    series_test <- data_test[(last_row + 1):NROW(data_test),]
  }

  n_series <- (length(levels(data_test$series)))

  if(class(series_test)[1] == 'list'){
    fc_horizon <- length(series_test$series) / n_series
  } else {
    fc_horizon <- NROW(series_test) / n_series
  }

  # Generate linear predictor matrix
  Xp <- predict(mgcv_model,
                newdata = series_test,
                type = 'lpmatrix')

  # Run particles forward in time to generate their forecasts
  cl <- parallel::makePSOCKcluster(n_cores)
  setDefaultCluster(cl)
  clusterExport(NULL, c('particles',
                        'Xp',
                        'sim_ar3',
                        'series_test',
                        'fc_horizon',
                        'n_series',
                        'hpd'),
                envir = environment())

  pbapply::pboptions(type = "none")
  particle_fcs <- pbapply::pblapply(fc_samples, function(x){
    use_lv <- particles[[x]]$use_lv

    if(use_lv){

      # Run the latent variables forward fc_horizon timesteps
      lv_preds <- do.call(rbind, lapply(seq_len(particles[[x]]$n_lv), function(lv){
        sim_ar3(phi = particles[[x]]$phi[lv],
                ar1 = particles[[x]]$ar1[lv],
                ar2 = particles[[x]]$ar2[lv],
                ar3 = particles[[x]]$ar3[lv],
                tau = particles[[x]]$tau[lv],
                state = particles[[x]]$lv_states[[lv]],
                h = fc_horizon)
      }))
      series_fcs <- lapply(seq_len(n_series), function(series){
        trend_preds <- as.numeric(t(lv_preds) %*% particles[[x]]$lv_coefs[series,])

        if(particles[[x]]$family == 'Negative binomial'){
          fc <- rnbinom(fc_horizon,
                        mu = exp(as.vector((Xp[which(as.numeric(series_test$series) == series),] %*%
                                              particles[[x]]$betas)) +
                                   (trend_preds)),
                        size = particles[[x]]$size[series])
        }

        if(particles[[x]]$family == 'Poisson'){
          fc <- rpois(fc_horizon,
                        lambda = exp(as.vector((Xp[which(as.numeric(series_test$series) == series),] %*%
                                              particles[[x]]$betas)) +
                                   (trend_preds)))
        }

        if(particles[[x]]$family == 'Tweedie'){
          fc <- rpois(fc_horizon,
                        lambda = mgcv::rTweedie(
                          mu = exp(as.vector((Xp[which(as.numeric(series_test$series) == series),] %*%
                                                  particles[[x]]$betas)) +
                                       (trend_preds)),
                          p = particles[[x]]$p,
                          phi = particles[[x]]$twdis))
        }
        fc
      })

    } else {
      # Run the trends forward fc_horizon timesteps
      series_fcs <- lapply(seq_len(n_series), function(series){
          trend_preds <- sim_ar3(phi = particles[[x]]$phi[series],
                                 ar1 = particles[[x]]$ar1[series],
                                 ar2 = particles[[x]]$ar2[series],
                                 ar3 = particles[[x]]$ar3[series],
                                 tau = particles[[x]]$tau[series],
                                 state = particles[[x]]$trend_states[[series]],
                                 h = fc_horizon)

          if(particles[[x]]$family == 'Negative binomial'){
            fc <-  rnbinom(fc_horizon,
                           mu = exp(as.vector((Xp[which(as.numeric(series_test$series) == series),] %*%
                                                 particles[[x]]$betas)) + (trend_preds)),
                           size = particles[[x]]$size[series])
          }

          if(particles[[x]]$family == 'Poisson'){
            fc <-  rpois(fc_horizon,
                           lambda = exp(as.vector((Xp[which(as.numeric(series_test$series) == series),] %*%
                                                 particles[[x]]$betas)) + (trend_preds)))
          }

          if(particles[[x]]$family == 'Tweedie'){
            fc <-  rpois(fc_horizon,
                         lambda = mgcv::rTweedie(mu = exp(as.vector((Xp[which(as.numeric(series_test$series) == series),] %*%
                                                                       particles[[x]]$betas)) + (trend_preds)),
                                                 p = particles[[x]]$p,
                                                 phi = particles[[x]]$twdis))
          }

          fc
      })
    }

    series_fcs
  }, cl = cl)
  stopCluster(cl)


  # Weighted forecast for each series
  series_fcs <- lapply(seq_len(n_series), function(series){
    indexed_forecasts <- do.call(rbind, lapply(seq_along(particle_fcs), function(x){
      particle_fcs[[x]][[series]]
    }))
    indexed_forecasts
  })
  names(series_fcs) <- levels(data_test$series)

  # Generate plots of forecasts for each series
  if(class(obs_data)[1] != 'list'){
    obs_data %>%
      dplyr::arrange(time, series) -> obs_data
  }

  plot_series_fc = function(series, preds, ylim, plot_legend = TRUE){
    all_obs <- obs_data$y[which(as.numeric(obs_data$series) == series)]
    assimilated <- obs_data$assimilated[which(as.numeric(obs_data$series) == series)]
    preds_last <- c(all_obs, preds[1,])
    int <- apply(preds,
                  2, hpd, 0.95)
    if(!is.null(particles[[1]]$upper_bounds)){
      upper_lim <- min(c(particles[[1]]$upper_bounds[series],
                         (max(c(all_obs, int[3,]), na.rm = T) + 4)))
    } else {
      upper_lim <- max(c(all_obs, int[3,]), na.rm = T) + 4
    }

    if(is.na(ylim[1])){
      ylim <- c(0, upper_lim)
    }

    # Plot quantiles of the forecast distribution
    probs = c(0.05, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.95)
    cred <- sapply(1:NCOL(preds),
                   function(n) quantile(preds[,n],
                                        probs = probs))

    c_light <- c("#DCBCBC")
    c_light_highlight <- c("#C79999")
    c_mid <- c("#B97C7C")
    c_mid_highlight <- c("#A25050")
    c_dark <- c("#8F2727")
    c_dark_highlight <- c("#7C0000")

    plot(1, type = "n",
         xlab = 'Time',
         ylab = paste0('Predictions for ', levels(obs_data$series)[series]),
         xlim = c(0, length(preds_last)),
         ylim = ylim)

    polygon(c(seq((length(all_obs) + 1), length(preds_last)),
              rev(seq((length(all_obs) + 1), length(preds_last)))),
            c(cred[1,], rev(cred[9,])),
            col = c_light, border = NA)
    polygon(c(seq((length(all_obs) + 1), length(preds_last)),
              rev(seq((length(all_obs) + 1), length(preds_last)))),
            c(cred[2,], rev(cred[8,])),
            col = c_light_highlight, border = NA)
    polygon(c(seq((length(all_obs) + 1), length(preds_last)),
              rev(seq((length(all_obs) + 1), length(preds_last)))),
            c(cred[3,], rev(cred[7,])),
            col = c_mid, border = NA)
    polygon(c(seq((length(all_obs) + 1), length(preds_last)),
              rev(seq((length(all_obs) + 1), length(preds_last)))),
            c(cred[4,], rev(cred[6,])),
            col = c_mid_highlight, border = NA)
    lines(seq((length(all_obs) + 1), length(preds_last)), cred[5,], col = c_dark, lwd = 2.5)

    points(x = which(assimilated == 'no'),
           y = all_obs[which(assimilated == 'no')], pch = 16,
           col = 'white', cex = 0.65)
    points(x = which(assimilated == 'no'),
           y = all_obs[which(assimilated == 'no')], pch = 16,
           col = 'black', cex = 0.55)
    points(x = which(assimilated == 'yes'), y = all_obs[which(assimilated == 'yes')],
           pch = 8, cex = 0.75, col = 'white')
    points(x = which(assimilated == 'yes'), y = all_obs[which(assimilated == 'yes')],
           pch = 8, cex = 0.65, col = c_dark)
    abline(v = max(which(assimilated == 'yes')), lty = 'dashed')

    if(plot_legend){
      legend(legend_position,
             cex = 0.9,
             legend=c("Trained","Assimilated"),
             bg = 'white',
             col = c('black', c_dark),
             text.col = c('black', c_dark),
             bty = 'n',
             pch = c(16, 8),
             ncol = 1)
    }
  }

  fc_plots <- lapply(seq_len(n_series), function(series){
    function(){plot_series_fc(series, preds = series_fcs[[series]], ylim, plot_legend)}
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
