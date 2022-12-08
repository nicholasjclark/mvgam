#'Initiate particles for online filtering from a fitted mvgam object
#'
#'This function generates a set of particles that each captures a unique proposal about
#'the current state of the system. The next observation in \code{data_assim} is assimilated
#'and particles are weighted by their proposal's multivariate composite likelihood to update the model's
#'forecast distribution
#'
#'@param object \code{list} object returned from \code{mvgam}
#'@param newdata A \code{dataframe} or \code{list} of test data containing at least one more observation per series
#'(beyond the last observation seen by the model in \code{object}) to be assimilated by the particle filter.
#'Should at least contain 'series' and 'time' for the one-step ahead horizon,
#'in addition to any other variables included in the linear predictor of \code{object}
#'@param data_assim Deprecated. Still works in place of \code{newdata} but users are recommended to use
#'\code{newdata} instead for more seamless integration into `R` workflows
#'@param n_particles \code{integer} specifying the number of unique particles to generate for tracking the
#'latent system state
#'@param file_path \code{character} string specifying the file path for saving the initiated particles
#'@param n_cores \code{integer} specifying number of cores for generating particle forecasts in parallel
#'@return A \code{list} object of \code{length = n_particles} containing information on parameters and
#'current state estimates for each particle is generated and saved, along with other important information
#'from the original model, to an \code{.rda} object in \code{file_path}
#'@export
pfilter_mvgam_init = function(object,
                              newdata,
                              data_assim,
                              n_particles = 1000,
                              file_path = 'pfilter',
                              n_cores = 2){

  # Check arguments
  if(class(object) != 'mvgam'){
    stop('argument "object" must be of class "mvgam"')
  }

  if(!missing("newdata")){
    data_assim <- newdata
  }

#### 1. Generate linear predictor matrix for the next timepoint and extract last trend estimates
# (NOTE, all series must have observations for the next timepoint, even if they are NAs!!!!) ####
data_train <- object$obs_data
n_series <- NCOL(object$ytimes)

# Variable name checks
if(class(object$obs_data)[1] == 'list'){
  if(!'time' %in% names(data_assim)){
    stop('data_assim does not contain a "time" column')
  }

  if(!'series' %in% names(data_assim)){
    data_assim$series <- factor('series1')
  }

} else {
  if(!'time' %in% colnames(data_assim)){
    stop('data_assim does not contain a "time" column')
  }

  if(!'series' %in% colnames(data_assim)){
    data_assim$series <- factor('series1')
  }
}

# Next observation for assimilation (ensure data_assim is arranged correctly)
if(class(object$obs_data)[1] == 'list'){

  # Find indices of next observation
  temp_dat = data.frame(time = data_assim$time,
                        series = data_assim$series) %>%
    dplyr::mutate(index = dplyr::row_number()) %>%
    dplyr::arrange(time, series)
  indices_assim <- temp_dat[1:n_series,'index']

  # Get list object into correct format for lpmatrix prediction
  series_test <- lapply(data_assim, function(x){
    if(is.matrix(x)){
      matrix(x[indices_assim,], ncol = NCOL(x))
    } else {
      x[indices_assim]
    }

  })

} else {
  data_assim %>%
    dplyr::arrange(time, series) -> data_assim
  series_test <- data_assim[1:n_series,]
}


if(length(unique(series_test$time)) > 1){
  stop('data_assim should have one observation per series for the next timepoint')
}

# Linear predictor matrix for the next observation
suppressWarnings(Xp  <- try(predict(object$mgcv_model,
                                    newdata = series_test,
                                    type = 'lpmatrix'),
                                 silent = TRUE))

if(inherits(Xp, 'try-error')){
  testdat <- data.frame(time = series_test$time)

  terms_include <- names(object$mgcv_model$coefficients)[which(!names(object$mgcv_model$coefficients) %in% '(Intercept)')]
  if(length(terms_include) > 0){
    newnames <- vector()
    newnames[1] <- 'time'
    for(i in 1:length(terms_include)){
      testdat <- cbind(testdat, data.frame(series_test[[terms_include[i]]]))
      newnames[i+1] <- terms_include[i]
    }
    colnames(testdat) <- newnames
  }

  suppressWarnings(Xp  <- predict(object$mgcv_model,
                                       newdata = testdat,
                                       type = 'lpmatrix'))
}

# Extract last trend / latent variable and precision estimates
if(object$use_lv){
  n_lv <- object$n_lv
  if(object$fit_engine == 'stan'){
    lvs <- lapply(seq_len(n_lv), function(lv){
      inds_lv <- seq(lv, dim(MCMCvis::MCMCchains(object$model_output, 'LV'))[2], by = n_lv)
      lv_estimates <- MCMCvis::MCMCchains(object$model_output, 'LV')[,inds_lv]
      # Need to only use estimates from the training period
      s_name <- levels(object$obs_data$series)[1]
      if(class(object$obs_data)[1] == 'list'){
        end_train <- data.frame(y = object$obs_data$y,
                                series = object$obs_data$series,
                                time = object$obs_data$time) %>%
          dplyr::filter(series == s_name) %>%
          NROW()
      } else {
        end_train <- object$obs_data %>%
          dplyr::filter(series == s_name) %>%
          NROW()
      }

      lv_estimates <- lv_estimates[,1:end_train]
      lv_estimates[,(NCOL(lv_estimates)-2):(NCOL(lv_estimates))]
    })

  } else {
    ends <- seq(0, dim(MCMCvis::MCMCchains(object$model_output, 'LV'))[2],
                length.out = n_lv + 1)
    starts <- ends + 1
    starts <- c(1, starts[-c(1, (n_lv + 1))])
    ends <- ends[-1]
    lvs <- lapply(seq_len(n_lv), function(lv){
      lv_estimates <- MCMCvis::MCMCchains(object$model_output, 'LV')[,starts[lv]:ends[lv]]

      # Need to only use estimates from the training period
      s_name <- levels(object$obs_data$series)[1]
      end_train <- data.frame(series = object$obs_data$series) %>%
        dplyr::filter(series == s_name) %>%
        NROW()
      lv_estimates <- lv_estimates[,1:end_train]
      lv_estimates[,(NCOL(lv_estimates)-2):(NCOL(lv_estimates))]
    })
  }

  taus <- MCMCvis::MCMCchains(object$model_output, 'penalty')
  trends <- NULL
} else {
  ends <- seq(0, dim(MCMCvis::MCMCchains(object$model_output, 'trend'))[2],
              length.out = n_series + 1)
  starts <- ends + 1
  starts <- c(1, starts[-c(1, (n_series + 1))])
  ends <- ends[-1]
  trends <- lapply(seq_len(n_series), function(series){
    if(object$fit_engine == 'stan'){
      trend_estimates <- MCMCvis::MCMCchains(object$model_output, 'trend')[,seq(series,
                                                                                dim(MCMCvis::MCMCchains(object$model_output,
                                                                                                        'trend'))[2],
                                                                                by = NCOL(object$ytimes))]
    } else {
      trend_estimates <- MCMCvis::MCMCchains(object$model_output, 'trend')[,starts[series]:ends[series]]
    }

    # Need to only use estimates from the training period
    s_name <- levels(object$obs_data$series)[1]
    end_train <- data.frame(series = object$obs_data$series) %>%
      dplyr::filter(series == s_name) %>%
      NROW()
    trend_estimates <- trend_estimates[,1:end_train]

    # Only need last 3 timesteps if this is not a GP trend model
    if(object$trend_model == 'GP'){
      out <- trend_estimates
    } else {
      out <- trend_estimates[,(NCOL(trend_estimates)-2):(NCOL(trend_estimates))]
    }
    out
  })

  if(object$trend_model == 'GP'){
    taus <- NULL
  } else {
    taus <- MCMCvis::MCMCchains(object$model_output, 'tau')
  }

  lvs <- NULL
}

# If use_lv, extract latent variable loadings
if(object$use_lv){

  lv_coefs <- lapply(seq_len(n_series), function(series){
    if(object$fit_engine == 'stan'){
      coef_start <- min(which(sort(rep(1:n_series, n_lv)) == series))
      coef_end <- coef_start + n_lv - 1
      as.matrix(MCMCvis::MCMCchains(object$model_output, 'lv_coefs')[,coef_start:coef_end])
    } else {
      lv_indices <- seq(1, n_series * n_lv, by = n_series) + (series - 1)
      as.matrix(MCMCvis::MCMCchains(object$model_output, 'lv_coefs')[,lv_indices])
    }

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
if(object$trend_model == 'GP'){
  alpha_gps <- MCMCvis::MCMCchains(object$model_output, 'alpha_gp')
  rho_gps <- MCMCvis::MCMCchains(object$model_output, 'rho_gp')
  ar1s <- NULL
  ar2s <- NULL
  ar3s <- NULL
}

if(object$trend_model == 'RW'){
  if(object$use_lv){
    alpha_gps <- NULL
    rho_gps <- NULL
    ar1s <- matrix(1, nrow = NROW(betas), ncol = object$n_lv)
    ar2s <- matrix(0, nrow = NROW(betas), ncol = object$n_lv)
    ar3s <- matrix(0, nrow = NROW(betas), ncol = object$n_lv)
  } else {
    alpha_gps <- NULL
    rho_gps <- NULL
    ar1s <- matrix(1, nrow = NROW(betas), ncol = n_series)
    ar2s <- matrix(0, nrow = NROW(betas), ncol = n_series)
    ar3s <- matrix(0, nrow = NROW(betas), ncol = n_series)
  }
}

if(object$trend_model == 'AR1'){
  ar1s <- MCMCvis::MCMCchains(object$model_output, 'ar1')
  alpha_gps <- NULL
  rho_gps <- NULL
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
  alpha_gps <- NULL
  rho_gps <- NULL

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
  alpha_gps <- NULL
  rho_gps <- NULL
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

# Generate sample sequence for n_particles
if(n_particles < dim(phis)[1]){
  sample_seq <- sample(seq_len(dim(phis)[1]), size = n_particles, replace = F)
} else {
  sample_seq <- sample(seq_len(dim(phis)[1]), size = n_particles, replace = T)
}

#### 2. Generate particles and calculate their proposal weights ####
use_lv <- object$use_lv
truth <- series_test$y
last_assim <- unique(series_test$time)
upper_bounds <- object$upper_bounds
trend_model <- object$trend_model

cl <- parallel::makePSOCKcluster(n_cores)
setDefaultCluster(cl)
clusterExport(NULL, c('use_lv',
                      'trend_model',
                      'Xp',
                      'betas',
                      'series_test',
                      'truth',
                      'last_assim',
                      'taus',
                      'phis',
                      'ar1s',
                      'ar2s',
                      'ar3s',
                      'alpha_gps',
                      'rho_gps',
                      'lvs',
                      'lv_coefs',
                      'trends',
                      'family',
                      'sizes',
                      'twdis',
                      'p',
                      'n_series',
                      'n_particles',
                      'upper_bounds'),
              envir = environment())

pbapply::pboptions(type = "none")

particles <- pbapply::pblapply(sample_seq, function(x){

  samp_index <- x

  if(is.null(alpha_gps)){
    # Sample AR parameters
    phi <- phis[samp_index, ]
    ar1 <- ar1s[samp_index, ]
    ar2 <- ar2s[samp_index, ]
    ar3 <- ar3s[samp_index, ]
    alpha_gp <- rho_gp <- 0

    # Sample trend precisions
    tau <- taus[samp_index,]
  } else {
    phi <- ar1 <- ar2 <- ar3 <- tau <- 0
    alpha_gp <- alpha_gps[samp_index, ]
    rho_gp <- rho_gps[samp_index, ]
  }

  # Sample beta coefs
  betas <- betas[samp_index, ]

  # Family-specific parameters
  size <- sizes[samp_index, ]
  twdis <- twdis[samp_index, ]
  p <- p[samp_index]

  if(use_lv){

    # Sample a last state estimate for the latent variables
  last_lvs <- lapply(seq_along(lvs), function(lv){
    lvs[[lv]][samp_index, ]
  })

  # Sample lv loadings
  lv_coefs <- do.call(rbind, lapply(seq_len(n_series), function(series){
    lv_coefs[[series]][samp_index,]
  }))

  # Run the latent variables forward one timestep
  if(is.null(alpha_gps)){
    next_lvs <- do.call(cbind, lapply(seq_along(lvs), function(lv){
      sim_ar3(h = 1,
              phi = phi[lv],
              ar1 = ar1[lv],
              ar2 = ar2[lv],
              ar3 = ar3[lv],
              tau = tau[lv],
              last_trends = last_lvs[[lv]][1:3])
    }))
  } else {
    next_lvs <- do.call(cbind, lapply(seq_along(lvs), function(lv){
      sim_gp(last_trends = last_lvs[[lv]],
             h = 1,
             alpha_gp = alpha_gp[lv],
             rho_gp = rho_gp[lv])
    }))
  }

  # Multiply lv states with loadings to generate each series' forecast trend state
  trends <- as.numeric(next_lvs %*% t(lv_coefs))

  # Include previous states for each lv
  if(is.null(alpha_gps)){
    next_lvs <- lapply(seq_along(lvs), function(lv){
      as.vector(c(as.numeric(last_lvs[[lv]][2]),
                  as.numeric(last_lvs[[lv]][3]),
                  next_lvs[lv]))
    })
  } else {
    next_lvs <- lapply(seq_along(lvs), function(lv){
      as.vector(c(as.numeric(last_lvs[[lv]]),
                  next_lvs[lv]))
    })
  }
  names(next_lvs) <- paste0('lv', seq(1:length(lvs)))

  # Calculate weight for the particle in the form of a composite likelihood
  liks <- unlist(lapply(seq_len(n_series), function(series){

    if(is.na(truth[series])){
      weight <- 1
    } else {
      if(family == 'Negative Binomial'){
        weight <- (dnbinom(truth[series], size = size[series],
                               mu = exp(((Xp[which(as.numeric(series_test$series) == series),] %*% betas)) +
                                          (trends[series]))))
      }

      if(family == 'Poisson'){
        weight <- (dpois(truth[series],
                               lambda = exp(((Xp[which(as.numeric(series_test$series) == series),] %*% betas)) +
                                          (trends[series]))))
      }

      if(family == 'Tweedie'){
        weight <- exp(mgcv::ldTweedie(y = truth[series],
                                       mu = exp(((Xp[which(as.numeric(series_test$series) == series),] %*% betas)) +
                                            (trends[series])),
                                       p = p,
                                       phi = twdis[series],
                                       all.derivs = F)[,1])
      }

    }
    weight
  }))
  weight <- prod(liks, na.rm = T)
  n_lv <- length(lvs)
  trends <- NULL

  } else {
    next_lvs = NULL
    n_lv = NULL
    lv_coefs = NULL

    # Sample last state estimates for the trends
    last_trends <- lapply(seq_along(trends), function(trend){
      trends[[trend]][samp_index, ]
    })

    # Run the trends forward one timestep
    if(is.null(alpha_gps)){
      trends <- do.call(cbind, lapply(seq_along(trends), function(trend){
        sim_ar3(h = 1,
                phi = phi[trend],
                ar1 = ar1[trend],
                ar2 = ar2[trend],
                ar3 = ar3[trend],
                tau = tau[trend],
                last_trends = last_trends[[trend]][1:3])
      }))
    } else {
      trends <- do.call(cbind, lapply(seq_along(trends), function(trend){
        sim_gp(last_trends = last_trends[[trend]],
               h = 1,
               alpha_gp = alpha_gp[trend],
               rho_gp = rho_gp[trend])
      }))
    }

    # Calculate weight for the particle in the form of a composite likelihood
    liks <- unlist(lapply(seq_len(n_series), function(series){

      if(is.na(truth[series])){
        weight <- 1
      } else {
        if(family == 'Negative Binomial'){
          weight <- (dnbinom(truth[series], size = size[series],
                             mu = exp(((Xp[which(as.numeric(series_test$series) == series),] %*% betas)) +
                                        (trends[series]))))
        }

        if(family == 'Poisson'){
          weight <- (dpois(truth[series],
                               lambda = exp(((Xp[which(as.numeric(series_test$series) == series),] %*% betas)) +
                                              (trends[series]))))
        }

        if(family == 'Tweedie'){
          weight <- exp(mgcv::ldTweedie(y = truth[series],
                                            mu = exp(((Xp[which(as.numeric(series_test$series) == series),] %*% betas)) +
                                                       (trends[series])),
                                            p = p,
                                            phi = twdis[series],
                                            all.derivs = F)[,1])
        }

      }
      weight
    }))

    weight <- prod(liks, na.rm = T)

    # Include previous states for each trend
    if(is.null(alpha_gps)){
      trends <- lapply(seq_len(n_series), function(trend){
        as.vector(c(last_trends[[trend]][2], last_trends[[trend]][3], trends[trend]))
      })
    } else {
      trends <- lapply(seq_len(n_series), function(trend){
        as.vector(c(last_trends[[trend]], trends[trend]))
      })
    }

    names(trends) <- paste0('series', seq_len(n_series))
}

  # Store important particle-specific information for later filtering
  if(family == 'Poisson'){
    if(trend_model == 'GP'){
      output <- list(use_lv = use_lv,
                     n_lv = n_lv,
                     family = family,
                     trend_model = trend_model,
                     lv_states = next_lvs,
                     lv_coefs = lv_coefs,
                     betas = as.numeric(betas),
                     alpha_gp = as.numeric(alpha_gp),
                     rho_gp = as.numeric(rho_gp),
                     trend_states = lapply(last_trends, unname),
                     weight = weight,
                     liks = liks,
                     upper_bounds = upper_bounds,
                     last_assim = last_assim)
    } else {
      output <- list(use_lv = use_lv,
                     n_lv = n_lv,
                     family = family,
                     trend_model = trend_model,
                     lv_states = next_lvs,
                     lv_coefs = lv_coefs,
                     betas = as.numeric(betas),
                     tau = as.numeric(tau),
                     phi = as.numeric(phi),
                     ar1 = as.numeric(ar1),
                     ar2 = as.numeric(ar2),
                     ar3 = as.numeric(ar3),
                     trend_states = trends,
                     weight = weight,
                     liks = liks,
                     upper_bounds = upper_bounds,
                     last_assim = last_assim)
    }

  }

  if(family == 'Negative Binomial'){
    if(trend_model == 'GP'){
      output <- list(use_lv = use_lv,
                     n_lv = n_lv,
                     family = family,
                     trend_model = trend_model,
                     lv_states = next_lvs,
                     lv_coefs = lv_coefs,
                     betas = as.numeric(betas),
                     size = as.numeric(size),
                     alpha_gp = as.numeric(alpha_gp),
                     rho_gp = as.numeric(rho_gp),
                     trend_states = lapply(last_trends, unname),
                     weight = weight,
                     liks = liks,
                     upper_bounds = upper_bounds,
                     last_assim = last_assim)
    } else {
      output <- list(use_lv = use_lv,
                     n_lv = n_lv,
                     family = family,
                     trend_model = trend_model,
                     lv_states = next_lvs,
                     lv_coefs = lv_coefs,
                     betas = as.numeric(betas),
                     size = as.numeric(size),
                     tau = as.numeric(tau),
                     phi = as.numeric(phi),
                     ar1 = as.numeric(ar1),
                     ar2 = as.numeric(ar2),
                     ar3 = as.numeric(ar3),
                     trend_states = trends,
                     weight = weight,
                     liks = liks,
                     upper_bounds = upper_bounds,
                     last_assim = last_assim)
    }

  }

  if(family == 'Tweedie'){
    if(trend_model == 'GP'){
      output <- list(use_lv = use_lv,
                     n_lv = n_lv,
                     family = family,
                     trend_model = trend_model,
                     lv_states = next_lvs,
                     lv_coefs = lv_coefs,
                     betas = as.numeric(betas),
                     p = as.numeric(p),
                     twdis = as.numeric(twdis),
                     alpha_gp = as.numeric(alpha_gp),
                     rho_gp = as.numeric(rho_gp),
                     trend_states = lapply(last_trends, unname),
                     weight = weight,
                     liks = liks,
                     upper_bounds = upper_bounds,
                     last_assim = last_assim)
    } else {
      output <- list(use_lv = use_lv,
                     n_lv = n_lv,
                     family = family,
                     trend_model = trend_model,
                     lv_states = next_lvs,
                     lv_coefs = lv_coefs,
                     betas = as.numeric(betas),
                     p = as.numeric(p),
                     twdis = as.numeric(twdis),
                     tau = as.numeric(tau),
                     phi = as.numeric(phi),
                     ar1 = as.numeric(ar1),
                     ar2 = as.numeric(ar2),
                     ar3 = as.numeric(ar3),
                     trend_states = trends,
                     weight = weight,
                     liks = liks,
                     upper_bounds = upper_bounds,
                     last_assim = last_assim)
    }

  }

  output

}, cl = cl)
stopCluster(cl)

# Calculate ESS and save outputs for later online filtering
mgcv_model <- object$mgcv_model
series_test$assimilated <- 'yes'

if(class(data_train)[1] == 'list'){

  data_train$assimilated <- rep('no', length(data_train$y))
  obs_data <- lapply(seq_along(data_train), function(x){

    if(is.matrix(data_train[[x]])){
      rbind(data_train[[x]], series_test[[x]])
    } else {
      if(is.factor(data_train[[x]])){
        factor(unlist(list(data_train[[x]], series_test[[x]])),
               levels = levels(data_train[[x]]))
      } else {
        c(data_train[[x]], series_test[[x]])
      }
    }
  })

  names(obs_data) <- names(series_test)

} else {
  data_train$assimilated <- 'no'
  data_train %>%
    dplyr::bind_rows(series_test) -> obs_data
}

# For multivariate models, gather information on each particle's ranked proposal
# to update weights so that we can target particles that perform well across the
# full set of series, rather than performing extremely well for one at the expense
# of the others
if(NCOL(object$ytimes) > 1){
lik_weights <- do.call(rbind, lapply(seq_along(particles), function(x){
  particles[[x]]$liks
}))

series_sds <- apply(lik_weights, 2, function(x) sd(x))
if(all(series_sds == 0)){
  # Do nothing if all series have equal weights due to resampling or all NAs in
  # last observation
} else {
  # Else update weights using ranking information
  lik_weights <- apply(apply(lik_weights[, !series_sds  == 0 ], 2, rank), 1, prod)
  lik_weights <- lik_weights / max(lik_weights)

  particles <- lapply(seq_along(particles), function(x){
    particles[[x]]$weight <- lik_weights[x] * particles[[x]]$weight
    particles[[x]]
  })
}
}

weights <- (unlist(lapply(seq_along(particles), function(x){
  tail(particles[[x]]$weight, 1)})))
weights <- weights / sum(weights)
ess <- 1 / sum(weights^2)
dir.create(file_path, recursive = T, showWarnings = F)
cat('Saving particles to', paste0(file_path, '/particles.rda'), '\n',
    'ESS =',  ess, '\n')
save(particles, mgcv_model, obs_data, last_assim,
     ess = ess, file = paste0(file_path, '/particles.rda'))
}

