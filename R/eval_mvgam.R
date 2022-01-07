#'Evaluate forecasts from a fitted mvjagam object at a specific timepoint
#'
#'This function generates a set of samples representing fixed parameters estimated from the full
#'\code{mvjagam} model and latent trend states at a given point in time. The trends are rolled forward
#'a total of \code{fc_horizon} timesteps according to their estimated state space dynamics to
#'generate an 'out-of-sample' forecast that is evaluated against the true observations in the horizon window.
#'This function therefore simulates a situation where the model's parameters had already been estimated but
#'we have only observed data up to the evaluation timepoint and would like to generate forecasts from the
#'latent trends. Evaluation involves calculating the Discrete Rank Probability Score and a binary indicator
#'for whether or not the true value lies within the forecast's 90% prediction interval
#'
#'@param object \code{list} object returned from \code{mvjagam}
#'@param n_samples \code{integer} specifying the number of samples to generate from the model's
#'posterior distribution
#'@param eval_timepoint \code{integer} indexing the timepoint that represents our last 'observed'
#'set of outcome data
#'@param fc_horizon \code{integer} specifying the length of the forecast horizon for evaluating forecasts
#'@param n_cores \code{integer} specifying number of cores for generating particle forecasts in parallel
#'@return A \code{list} object containing information on specific evaluations for each series
#'@seealso \code{roll_eval_mvgam}, \code{compare_mvgams}
#'@export
eval_mvgam = function(object,
                      n_samples = 5000,
                      eval_timepoint = 3,
                      fc_horizon = 3,
                      n_cores = 2){

  #### 1. Generate linear predictor matrix for covariates and extract trend estimates at timepoint
  data_train <- object$obs_data
  n_series <- NCOL(object$ytimes)

  # Check evaluation timepoint
  all_times <- (object$obs_data %>%
                  dplyr::select(year, season) %>%
                  dplyr::distinct() %>%
                  dplyr::arrange(year, season) %>%
                  dplyr::mutate(time = dplyr::row_number())) %>%
    dplyr::pull(time)

  if(!eval_timepoint %in% all_times){
    stop('Evaluation timepoint does not exist in original training data')
  }

  # Filter training data to correct point (just following evaluation timepoint)
  (object$obs_data %>%
      dplyr::select(year, season) %>%
      dplyr::distinct() %>%
      dplyr::arrange(year, season) %>%
      dplyr::mutate(time = dplyr::row_number())) %>%
    dplyr::left_join(object$obs_data,
                     by = c('season', 'year')) %>%
    dplyr::arrange(year, season, series) %>%
    dplyr::filter(time > (eval_timepoint ) &
                    time <= (eval_timepoint + fc_horizon)) -> data_assim

  # Linear predictor matrix for the evaluation observations
  Xp <- predict(object$mgcv_model,
                newdata = data_assim,
                type = 'lpmatrix')

  # Extract trend / latent variable estimates at the correct timepoint
  if(object$use_lv){
    n_lv <- object$n_lv
    ends <- seq(0, dim(MCMCvis::MCMCchains(object$jags_output, 'LV'))[2],
                length.out = n_lv + 1)
    starts <- ends + 1
    starts <- c(1, starts[-c(1, (n_lv + 1))])
    ends <- ends[-1]
    lvs <- lapply(seq_len(n_lv), function(lv){
      lv_estimates <- MCMCvis::MCMCchains(object$jags_output, 'LV')[,starts[lv]:ends[lv]]
      lv_estimates[,(eval_timepoint - 2):eval_timepoint]
    })

    taus <- MCMCvis::MCMCchains(object$jags_output, 'tau_fac')
    trends <- NULL
  } else {
    ends <- seq(0, dim(MCMCvis::MCMCchains(object$jags_output, 'trend'))[2],
                length.out = n_series + 1)
    starts <- ends + 1
    starts <- c(1, starts[-c(1, (n_series + 1))])
    ends <- ends[-1]
    trends <- lapply(seq_len(n_series), function(series){
      trend_estimates <- MCMCvis::MCMCchains(object$jags_output, 'trend')[,starts[series]:ends[series]]
      as.matrix(trend_estimates[,(eval_timepoint - 2):eval_timepoint])
    })

    taus <- MCMCvis::MCMCchains(object$jags_output, 'tau')
    lvs <- NULL
    n_lv <- NULL
  }

  # If use_lv, extract latent variable loadings
  if(object$use_lv){
    lv_coefs <- lapply(seq_len(n_series), function(series){
      lv_indices <- seq(1, n_series * n_lv, by = n_series) + (series - 1)
      as.matrix(MCMCvis::MCMCchains(object$jags_output, 'lv_coefs')[,lv_indices])
    })
  } else {
    lv_coefs <- NULL
  }

  # Beta coefficients for GAM component
  betas <- MCMCvis::MCMCchains(object$jags_output, 'b')

  # Phi estimates for latent trend drift terms
  phis <- MCMCvis::MCMCchains(object$jags_output, 'phi')

  # AR term estimates
  ar1s <- MCMCvis::MCMCchains(object$jags_output, 'ar1')
  ar2s <- MCMCvis::MCMCchains(object$jags_output, 'ar2')
  ar3s <- MCMCvis::MCMCchains(object$jags_output, 'ar3')

  # Negative binomial size estimate
  sizes <- as.matrix(rep(hpd(MCMCvis::MCMCchains(object$jags_output, 'r'))[2],
                         dim(betas)[1]))

  # Generate sample sequence for n_samples
  if(n_samples < dim(phis)[1]){
    sample_seq <- sample(seq_len(dim(phis)[1]), size = n_samples, replace = F)
  } else {
    sample_seq <- sample(seq_len(dim(phis)[1]), size = n_samples, replace = T)
  }


  #### 2. Run trends forward fc_horizon steps to generate the forecast distribution ####
  use_lv <- object$use_lv
  upper_bounds <- object$upper_bounds

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

  # Run particles forward in time to generate their forecasts
  if(n_cores > 1){

  cl <- parallel::makePSOCKcluster(n_cores)
  setDefaultCluster(cl)
  clusterExport(NULL, c('use_lv',
                        'n_lv',
                        'fc_horizon',
                        'data_assim',
                        'Xp',
                        'betas',
                        'taus',
                        'phis',
                        'ar1s',
                        'ar2s',
                        'ar3s',
                        'lvs',
                        'lv_coefs',
                        'trends',
                        'sizes',
                        'n_series',
                        'upper_bounds',
                        'sim_ar3',
                        'hpd'),
                envir = environment())

  pbapply::pboptions(type = "none")
  particle_fcs <- pbapply::pblapply(sample_seq, function(x){

    if(use_lv){
      # Sample a last state estimate for the latent variables
      samp_index <- x
      last_lvs <- lapply(seq_along(lvs), function(lv){
        lvs[[lv]][samp_index, ]
      })

      # Sample drift and AR parameters
      phi <- phis[samp_index, ]
      ar1 <- ar1s[samp_index, ]
      ar2 <- ar2s[samp_index, ]
      ar3 <- ar3s[samp_index, ]

      # Sample lv precision
      tau <- taus[samp_index,]

      # Sample lv loadings
      lv_coefs <- do.call(rbind, lapply(seq_len(n_series), function(series){
        lv_coefs[[series]][samp_index,]
      }))

      # Sample beta coefs
      betas <- betas[samp_index, ]

      # Sample a negative binomial size parameter
      size <- sizes[samp_index, ]

      # Run the latent variables forward fc_horizon timesteps
      lv_preds <- do.call(rbind, lapply(seq_len(n_lv), function(lv){
        sim_ar3(phi = phi[lv],
                ar1 = ar1[lv],
                ar2 = ar2[lv],
                ar3 = ar3[lv],
                tau = tau,
                state = last_lvs[[lv]],
                h = fc_horizon)
      }))

      series_fcs <- lapply(seq_len(n_series), function(series){
        trend_preds <- as.numeric(t(lv_preds) %*% lv_coefs[series,])
        trunc_preds <- rnbinom(fc_horizon,
                               mu = exp(as.vector((Xp[which(as.numeric(data_assim$series) == series),] %*%
                                                       betas)) + (trend_preds)),
                               size = size)
        trunc_preds
      })

    } else {
      # Run the trends forward fc_horizon timesteps
      # Sample index for the particle
      samp_index <- x

      # Sample beta coefs
      betas <- betas[samp_index, ]

      # Sample last state estimates for the trends
      last_trends <- lapply(seq_along(trends), function(trend){
        trends[[trend]][samp_index, ]
      })

      # Sample AR parameters
      phi <- phis[samp_index, ]
      ar1 <- ar1s[samp_index, ]
      ar2 <- ar2s[samp_index, ]
      ar3 <- ar3s[samp_index, ]

      # Sample trend precisions
      tau <- taus[samp_index,]

      # Sample a negative binomial size parameter
      size <- sizes[samp_index, ]

      series_fcs <- lapply(seq_len(n_series), function(series){
        trend_preds <- sim_ar3(phi = phi[series],
                               ar1 = ar1[series],
                               ar2 = ar2[series],
                               ar3 = ar3[series],
                               tau = tau[series],
                               state = last_trends[[series]],
                               h = fc_horizon)
        fc <-  rnbinom(fc_horizon,
                       mu = exp(as.vector((Xp[which(as.numeric(data_assim$series) == series),] %*%
                                               betas)) + (trend_preds)),
                       size = size)
        fc
      })
    }

    series_fcs
  }, cl = cl)
  stopCluster(cl)

  } else {
    particle_fcs <- lapply(sample_seq, function(x){

      if(use_lv){
        # Sample a last state estimate for the latent variables
        samp_index <- x
        last_lvs <- lapply(seq_along(lvs), function(lv){
          lvs[[lv]][samp_index, ]
        })

        # Sample drift and AR parameters
        phi <- phis[samp_index, ]
        ar1 <- ar1s[samp_index, ]
        ar2 <- ar2s[samp_index, ]
        ar3 <- ar3s[samp_index, ]

        # Sample lv precision
        tau <- taus[samp_index,]

        # Sample lv loadings
        lv_coefs <- do.call(rbind, lapply(seq_len(n_series), function(series){
          lv_coefs[[series]][samp_index,]
        }))

        # Sample beta coefs
        betas <- betas[samp_index, ]

        # Sample a negative binomial size parameter
        size <- sizes[samp_index, ]

        # Run the latent variables forward fc_horizon timesteps
        lv_preds <- do.call(rbind, lapply(seq_len(n_lv), function(lv){
          sim_ar3(phi = phi[lv],
                  ar1 = ar1[lv],
                  ar2 = ar2[lv],
                  ar3 = ar3[lv],
                  tau = tau,
                  state = last_lvs[[lv]],
                  h = fc_horizon)
        }))

        series_fcs <- lapply(seq_len(n_series), function(series){
          trend_preds <- as.numeric(t(lv_preds) %*% lv_coefs[series,])
          trunc_preds <- rnbinom(fc_horizon,
                                 mu = exp(as.vector((Xp[which(as.numeric(data_assim$series) == series),] %*%
                                                       betas)) + (trend_preds)),
                                 size = size)
          trunc_preds
        })

      } else {
        # Run the trends forward fc_horizon timesteps
        # Sample index for the particle
        samp_index <- x

        # Sample beta coefs
        betas <- betas[samp_index, ]

        # Sample last state estimates for the trends
        last_trends <- lapply(seq_along(trends), function(trend){
          trends[[trend]][samp_index, ]
        })

        # Sample AR parameters
        phi <- phis[samp_index, ]
        ar1 <- ar1s[samp_index, ]
        ar2 <- ar2s[samp_index, ]
        ar3 <- ar3s[samp_index, ]

        # Sample trend precisions
        tau <- taus[samp_index,]

        # Sample a negative binomial size parameter
        size <- sizes[samp_index, ]

        series_fcs <- lapply(seq_len(n_series), function(series){
          trend_preds <- sim_ar3(phi = phi[series],
                                 ar1 = ar1[series],
                                 ar2 = ar2[series],
                                 ar3 = ar3[series],
                                 tau = tau[series],
                                 state = last_trends[[series]],
                                 h = fc_horizon)
          fc <-  rnbinom(fc_horizon,
                         mu = exp(as.vector((Xp[which(as.numeric(data_assim$series) == series),] %*%
                                               betas)) + (trend_preds)),
                         size = size)
          fc
        })
      }

      series_fcs
    })

  }

  # Final forecast distribution
  series_fcs <- lapply(seq_len(n_series), function(series){
    indexed_forecasts <- do.call(rbind, lapply(seq_along(particle_fcs), function(x){
      particle_fcs[[x]][[series]]
    }))
    indexed_forecasts
  })
  names(series_fcs) <- levels(data_assim$series)

  # Evaluate against the truth
  series_truths <- lapply(seq_len(n_series), function(series){
    data_assim[which(as.numeric(data_assim$series) == series),'y']
  })

  # Default evaluation metric is the Discrete Rank Probability Score
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

  # Wrapper to operate on all observations in fc_horizon
  drps_mcmc_object <- function(truth, fc, interval_width = 0.9){
    indices_keep <- which(!is.na(truth))
    if(length(indices_keep) == 0){
      scores = data.frame('drps' = rep(NA, length(truth)),
                          'interval' = rep(NA, length(truth)))
    } else {
      scores <- matrix(NA, nrow = length(truth), ncol = 2)
      for(i in indices_keep){
        scores[i,] <- drps_score(truth = as.vector(truth)[i],
                                 fc = fc[,i], interval_width)
      }
    }
    scores
  }

  # Calculate DRPS and interval coverage per series
  series_drps <- lapply(seq_len(n_series), function(series){
    DRPS <- data.frame(drps_mcmc_object(as.vector(as.matrix(series_truths[[series]])),
                                        series_fcs[[series]]))
    colnames(DRPS) <- c('drps','in_interval')
    DRPS$eval_horizon <- seq(1, fc_horizon)
    DRPS$eval_season <- data_assim[which(as.numeric(data_assim$series) == series),]$season
    DRPS$eval_year <- data_assim[which(as.numeric(data_assim$series) == series),]$year
    DRPS
  })
  names(series_drps) <- levels(data_assim$series)

  return(series_drps)

}
