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

  # Check arguments
  if(class(object) != 'mvgam'){
    stop('argument "object" must be of class "mvgam"')
  }

  if(object$trend_model == 'None'){
    stop('cannot compute rolling forecasts for mvgams that have no trend model',
         call. = FALSE)
  }

  if(sign(fc_horizon) != 1){
    stop('argument "fc_horizon" must be a positive integer',
         call. = FALSE)
  } else {
    if(fc_horizon%%1 != 0){
      stop('argument "fc_horizon" must be a positive integer',
           call. = FALSE)
    }
  }

  if(sign(eval_timepoint) != 1){
    stop('argument "eval_timepoint" must be a positive integer',
         call. = FALSE)
  } else {
    if(eval_timepoint%%1 != 0){
      stop('argument "eval_timepoint" must be a positive integer',
           call. = FALSE)
    }
  }

  # Convert stanfit objects to coda samples
  if(class(object$model_output) == 'stanfit'){
    object$model_output <- coda::mcmc.list(lapply(1:NCOL(object$model_output),
                                                  function(x) coda::mcmc(as.array(object$model_output)[,x,])))
  }

  #### 1. Generate linear predictor matrix for covariates and extract trend estimates at timepoint
  data_train <- object$obs_data
  n_series <- NCOL(object$ytimes)

  # Check evaluation timepoint
  if(class(object$obs_data)[1] == 'list'){
    all_times <- (data.frame(time = object$obs_data$time)  %>%
                         dplyr::select(time) %>%
                         dplyr::distinct() %>%
                         dplyr::arrange(time) %>%
                         dplyr::mutate(time = dplyr::row_number())) %>%
      dplyr::pull(time)

  } else {
    all_times <- (object$obs_data %>%
                         dplyr::select(time) %>%
                         dplyr::distinct() %>%
                         dplyr::arrange(time) %>%
                         dplyr::mutate(time = dplyr::row_number())) %>%
      dplyr::pull(time)
  }

  if(!eval_timepoint %in% all_times){
    stop('Evaluation timepoint does not exist in original training data')
  }

  # Filter training data to correct point (just following evaluation timepoint)
  if(class(object$obs_data)[1] == 'list'){

    times <- (data.frame(time = object$obs_data$time) %>%
        dplyr::select(time) %>%
        dplyr::distinct() %>%
        dplyr::arrange(time) %>%
        dplyr::mutate(time = dplyr::row_number())) %>%
      dplyr::pull(time)

    data_assim <- lapply(object$obs_data, function(x){
      if(is.matrix(x)){
        matrix(x[which(times > (eval_timepoint) &
                  times <= (eval_timepoint + fc_horizon)),],
               ncol = NCOL(x))
      } else {
        x[which(times > (eval_timepoint) &
                  times <= (eval_timepoint + fc_horizon))]
      }

    })


  } else {
    (object$obs_data %>%
       dplyr::select(time) %>%
       dplyr::distinct() %>%
       dplyr::arrange(time) %>%
       dplyr::mutate(time = dplyr::row_number())) %>%
      dplyr::left_join(object$obs_data,
                       by = c('time')) %>%
      dplyr::arrange(time, series) %>%
      dplyr::filter(time > (eval_timepoint ) &
                      time <= (eval_timepoint + fc_horizon)) -> data_assim
  }


  # Linear predictor matrix for the evaluation observations
  Xp <- predict(object$mgcv_model,
                newdata = data_assim,
                type = 'lpmatrix')

  # Extract trend / latent variable estimates at the correct timepoint
  if(object$use_lv){
    n_lv <- object$n_lv
    ends <- seq(0, dim(MCMCvis::MCMCchains(object$model_output, 'LV'))[2],
                length.out = n_lv + 1)
    starts <- ends + 1
    starts <- c(1, starts[-c(1, (n_lv + 1))])
    ends <- ends[-1]
    lvs <- lapply(seq_len(n_lv), function(lv){
      lv_estimates <- MCMCvis::MCMCchains(object$model_output, 'LV')[,starts[lv]:ends[lv]]
      lv_estimates[,(eval_timepoint - 2):eval_timepoint]
    })

    taus <- MCMCvis::MCMCchains(object$model_output, 'penalty')

    trends <- NULL
  } else {
    if(object$trend_model == 'None'){
      trends <- lapply(seq_len(n_series), function(series){
        trend_estimates <- matrix(0, ncol = NROW(object$ytimes),
                                  nrow = dim(MCMCvis::MCMCchains(object$model_output, 'b'))[1])
        as.matrix(trend_estimates[,(eval_timepoint - 2):eval_timepoint])
      })

      taus <- matrix(0, nrow = dim(MCMCvis::MCMCchains(object$model_output, 'b'))[1],
                     ncol = NCOL(object$ytimes))
    } else {
    ends <- seq(0, dim(MCMCvis::MCMCchains(object$model_output, 'trend'))[2],
                length.out = n_series + 1)
    starts <- ends + 1
    starts <- c(1, starts[-c(1, (n_series + 1))])
    ends <- ends[-1]
    trends <- lapply(seq_len(n_series), function(series){
      trend_estimates <- MCMCvis::MCMCchains(object$model_output, 'trend')[,starts[series]:ends[series]]
      as.matrix(trend_estimates[,(eval_timepoint - 2):eval_timepoint])
    })

    taus <- MCMCvis::MCMCchains(object$model_output, 'tau')
    }

    lvs <- NULL
    n_lv <- NULL
  }

  # If use_lv, extract latent variable loadings
  if(object$use_lv){
    lv_coefs <- lapply(seq_len(n_series), function(series){
      lv_indices <- seq(1, n_series * n_lv, by = n_series) + (series - 1)
      as.matrix(MCMCvis::MCMCchains(object$model_output, 'lv_coefs')[,lv_indices])
    })
  } else {
    lv_coefs <- NULL
  }

  # Beta coefficients for GAM component
  betas <- MCMCvis::MCMCchains(object$model_output, 'b')

  # Phi estimates for latent trend drift terms
  if(object$drift){
    phis <- MCMCvis::MCMCchains(object$model_output, 'phi')
  } else {
    if(object$use_lv){
      phis <- matrix(0, nrow = NROW(betas), ncol = object$n_lv)
    } else {
      phis <- matrix(0, nrow = NROW(betas), ncol = n_series)
    }
  }

  # AR term estimates
  if(object$trend_model %in% c('RW', 'None')){
    if(object$use_lv){
      ar1s <- matrix(0, nrow = NROW(betas), ncol = object$n_lv)
      ar2s <- matrix(0, nrow = NROW(betas), ncol = object$n_lv)
      ar3s <- matrix(0, nrow = NROW(betas), ncol = object$n_lv)
    } else {
      ar1s <- matrix(0, nrow = NROW(betas), ncol = n_series)
      ar2s <- matrix(0, nrow = NROW(betas), ncol = n_series)
      ar3s <- matrix(0, nrow = NROW(betas), ncol = n_series)
    }
  }

  if(object$trend_model == 'AR1'){
    ar1s <- MCMCvis::MCMCchains(object$model_output, 'ar1')

    if(object$use_lv){
      ar2s <- matrix(0, nrow = NROW(betas), ncol = object$n_lv)
      ar3s <- matrix(0, nrow = NROW(betas), ncol = object$n_lv)
    } else {
      ar2s <- matrix(0, nrow = NROW(betas), ncol = n_series)
      ar3s <- matrix(0, nrow = NROW(betas), ncol = n_series)
    }
  }

  if(object$trend_model == 'AR2'){
    ar1s <- MCMCvis::MCMCchains(object$model_output, 'ar1')
    ar2s <- MCMCvis::MCMCchains(object$model_output, 'ar2')

    if(object$use_lv){
      ar3s <- matrix(0, nrow = NROW(betas), ncol = object$n_lv)
    } else {
      ar3s <- matrix(0, nrow = NROW(betas), ncol = n_series)
    }
  }

  if(object$trend_model == 'AR3'){
    ar1s <- MCMCvis::MCMCchains(object$model_output, 'ar1')
    ar2s <- MCMCvis::MCMCchains(object$model_output, 'ar2')
    ar3s <- MCMCvis::MCMCchains(object$model_output, 'ar3')
  }

  # Family-specific parameters
  if(object$family == 'Negative Binomial'){
    sizes <- MCMCvis::MCMCchains(object$model_output, 'r')
  } else {
    sizes <- NULL
  }

  if(object$family == 'Tweedie'){
    twdis <- MCMCvis::MCMCchains(object$model_output, 'twdis')
    p <- matrix(1.5, nrow = NROW(betas), ncol = n_series)
  } else {
    twdis <- p <- NULL
  }

  family <- object$family

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
                        'family',
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
                        'p',
                        'twdis',
                        'n_series',
                        'upper_bounds',
                        'sim_ar3'),
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

      # Family-specific parameters
      if(family == 'Negative Binomial'){
        size <- sizes[samp_index, ]
      }

      if(family == 'Tweedie'){
        twdis <- twdis[samp_index, ]
        p <- p[samp_index]
      }

      # Run the latent variables forward fc_horizon timesteps
      lv_preds <- do.call(rbind, lapply(seq_len(n_lv), function(lv){
        sim_ar3(phi = phi[lv],
                ar1 = ar1[lv],
                ar2 = ar2[lv],
                ar3 = ar3[lv],
                tau = tau[lv],
                state = last_lvs[[lv]],
                h = fc_horizon)
      }))

      series_fcs <- lapply(seq_len(n_series), function(series){
        trend_preds <- as.numeric(t(lv_preds) %*% lv_coefs[series,])

        if(family == 'Negative Binomial'){
          fc <- rnbinom(fc_horizon,
                                 mu = exp(as.vector((Xp[which(as.numeric(data_assim$series) == series),] %*%
                                                       betas)) + (trend_preds)),
                                 size = size[series])
        }

        if(family == 'Poisson'){
          fc <- rpois(fc_horizon,
                        lambda = exp(as.vector((Xp[which(as.numeric(data_assim$series) == series),] %*%
                                              betas)) + (trend_preds)))
        }

        if(family == 'Tweedie'){
          fc <- rpois(fc_horizon,
                      lambda = mgcv::rTweedie(
                        mu = exp(as.vector((Xp[which(as.numeric(data_assim$series) == series),] %*%
                                              betas)) + (trend_preds)),
                        p = p,
                        phi = twdis[series]))
        }

        fc
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

      # Family-specific parameters
      if(family == 'Negative Binomial'){
        size <- sizes[samp_index, ]
      }

      if(family == 'Tweedie'){
        twdis <- twdis[samp_index, ]
        p <- p[samp_index]
      }

      series_fcs <- lapply(seq_len(n_series), function(series){
        trend_preds <- sim_ar3(phi = phi[series],
                               ar1 = ar1[series],
                               ar2 = ar2[series],
                               ar3 = ar3[series],
                               tau = tau[series],
                               state = last_trends[[series]],
                               h = fc_horizon)
        if(family == 'Negative Binomial'){
          fc <-  rnbinom(fc_horizon,
                         mu = exp(as.vector((Xp[which(as.numeric(data_assim$series) == series),] %*%
                                               betas)) + (trend_preds)),
                         size = size[series])
        }

        if(family == 'Poisson'){
          fc <-  rpois(fc_horizon,
                         lambda = exp(as.vector((Xp[which(as.numeric(data_assim$series) == series),] %*%
                                               betas)) + (trend_preds)))
        }

        if(family == 'Tweedie'){
          fc <-  rpois(fc_horizon,
                       lambda = mgcv::rTweedie(
                         mu = exp(as.vector((Xp[which(as.numeric(data_assim$series) == series),] %*%
                                               betas)) + (trend_preds)),
                         p = p,
                         phi = twdis[series]))
        }

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

        # Family-specific parameters
        if(family == 'Negative Binomial'){
          size <- sizes[samp_index, ]
        }

        if(family == 'Tweedie'){
          twdis <- twdis[samp_index, ]
          p <- p[samp_index]
        }

        # Run the latent variables forward fc_horizon timesteps
        lv_preds <- do.call(rbind, lapply(seq_len(n_lv), function(lv){
          sim_ar3(phi = phi[lv],
                  ar1 = ar1[lv],
                  ar2 = ar2[lv],
                  ar3 = ar3[lv],
                  tau = tau[lv],
                  state = last_lvs[[lv]],
                  h = fc_horizon)
        }))

        series_fcs <- lapply(seq_len(n_series), function(series){
          trend_preds <- as.numeric(t(lv_preds) %*% lv_coefs[series,])

          if(family == 'Negative Binomial'){
            fc <- rnbinom(fc_horizon,
                          mu = exp(as.vector((Xp[which(as.numeric(data_assim$series) == series),] %*%
                                                betas)) + (trend_preds)),
                          size = size[series])
          }

          if(family == 'Poisson'){
            fc <- rpois(fc_horizon,
                        lambda = exp(as.vector((Xp[which(as.numeric(data_assim$series) == series),] %*%
                                                  betas)) + (trend_preds)))
          }

          if(family == 'Tweedie'){
            fc <- rpois(fc_horizon,
                        lambda = mgcv::rTweedie(
                          mu = exp(as.vector((Xp[which(as.numeric(data_assim$series) == series),] %*%
                                                betas)) + (trend_preds)),
                          p = p,
                          phi = twdis[series]))
          }

          fc
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

        # Family-specific parameters
        if(family == 'Negative Binomial'){
          size <- sizes[samp_index, ]
        }

        if(family == 'Tweedie'){
          twdis <- twdis[samp_index, ]
          p <- p[samp_index]
        }

        series_fcs <- lapply(seq_len(n_series), function(series){
          trend_preds <- sim_ar3(phi = phi[series],
                                 ar1 = ar1[series],
                                 ar2 = ar2[series],
                                 ar3 = ar3[series],
                                 tau = tau[series],
                                 state = last_trends[[series]],
                                 h = fc_horizon)
          if(family == 'Negative Binomial'){
            fc <-  rnbinom(fc_horizon,
                           mu = exp(as.vector((Xp[which(as.numeric(data_assim$series) == series),] %*%
                                                 betas)) + (trend_preds)),
                           size = size[series])
          }

          if(family == 'Poisson'){
            fc <-  rpois(fc_horizon,
                         lambda = exp(as.vector((Xp[which(as.numeric(data_assim$series) == series),] %*%
                                                   betas)) + (trend_preds)))
          }

          if(family == 'Tweedie'){
            fc <-  rpois(fc_horizon,
                         lambda = mgcv::rTweedie(
                           mu = exp(as.vector((Xp[which(as.numeric(data_assim$series) == series),] %*%
                                                 betas)) + (trend_preds)),
                           p = p,
                           phi = twdis[series]))
          }

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
    if(class(object$obs_data) == 'list'){
      data_assim[['y']][which(as.numeric(data_assim$series) == series)]
    } else {
      data_assim[which(as.numeric(data_assim$series) == series),'y']
    }

  })

  # Default evaluation metric is the Discrete Rank Probability Score
  drps_score <- function(truth, fc, interval_width = 0.9){
    nsum <- 1000
    Fy = ecdf(fc)
    ysum <- 0:nsum
    indicator <- ifelse(ysum - truth >= 0, 1, 0)
    score <- sum((indicator - Fy(ysum))^2)

    # Is value within empirical interval?
    interval <- quantile(fc, probs = c((1-interval_width)/2, (interval_width + (1-interval_width)/2)))
    in_interval <- ifelse(truth <= interval[2] & truth >= interval[1], 1, 0)
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
    DRPS
  })
  names(series_drps) <- levels(data_assim$series)

  return(series_drps)

}
