#'Residual calculations for a fitted mvgam object
#'
#'This function takes a fitted \code{mvgam} object and returns distributions of Dunn-Smyth residuals
#'
#'@param object \code{list} object returned from \code{mvgam}
#'@param n_cores \code{integer} specifying number of cores for generating residual distributions in parallel
#'@author Nicholas J Clark
#'@details Dunn-Smyth residual distributions are calculated for each series in the fitted object
#'@return A \code{list} of residual distributions
get_mvgam_resids = function(object, n_cores = 1){


preds <- MCMCvis::MCMCchains(object$model_output, 'ypred')
n_series <- NCOL(object$ytimes)
obs_series <- object$obs_data$series
series_levels <- levels(obs_series)
family <- object$family
obs_data <- object$obs_dat
fit_engine <- object$fit_engine

if(family == 'Negative Binomial'){
  rs <- MCMCvis::MCMCchains(object$model_output, 'r')
} else {
  rs <- NULL
}

# Functions for calculating randomised quantile (Dunn-Smyth) residuals
ds_resids_nb = function(truth, fitted, draw, size){
  dsres_out <- matrix(NA, length(truth), 1)
  for(i in 1:length(truth)){
    if(is.na(truth[i])){
      a <- pnbinom(as.vector(draw[i]) - 1, mu = fitted[i], size = size)
      b <- pnbinom(as.vector(draw[i]), mu = fitted[i], size = size)
    } else {
      a <- pnbinom(as.vector(truth[i]) - 1, mu = fitted[i], size = size)
      b <- pnbinom(as.vector(truth[i]), mu = fitted[i], size = size)
    }

    u <- runif(n = 1, min = a, max = b)
    if(u <= 0){
      u <- runif(n = 1, min = 0.0000001, max = 0.01)
    }
    if(u >=1){
      u <- runif(n = 1, min = 0.99, max = 0.9999999)
    }
    dsres_out[i, ] <- qnorm(u)
  }
  resids <- dsres_out[,1]
  resids[is.infinite(resids)] <- NaN
  resids
}

ds_resids_pois = function(truth, fitted, draw){
  dsres_out <- matrix(NA, length(truth), 1)
  for(i in 1:length(truth)){
    if(is.na(truth[i])){
      a <- ppois(as.vector(draw[i]) - 1, lambda = fitted[i])
      b <- ppois(as.vector(draw[i]), lambda = fitted[i])
    } else {
      a <- ppois(as.vector(truth[i]) - 1, lambda = fitted[i])
      b <- ppois(as.vector(truth[i]), lambda = fitted[i])
    }

    u <- runif(n = 1, min = a, max = b)
    if(u <= 0){
      u <- runif(n = 1, min = 0.0000001, max = 0.01)
    }
    if(u >=1){
      u <- runif(n = 1, min = 0.99, max = 0.9999999)
    }
    dsres_out[i, ] <- qnorm(u)
  }
  resids <- dsres_out[,1]
  resids[is.infinite(resids)] <- NaN
  resids
}

ds_resids_tw = function(truth, fitted, draw){
  dsres_out <- matrix(NA, length(truth), 1)
  for(i in 1:length(truth)){
    if(is.na(truth[i])){
      a <- ppois(as.vector(draw[i]) - 1, lambda = fitted[i])
      b <- ppois(as.vector(draw[i]), lambda = fitted[i])
    } else {
      a <- ppois(as.vector(truth[i]) - 1, lambda = fitted[i])
      b <- ppois(as.vector(truth[i]), lambda = fitted[i])
    }

    u <- runif(n = 1, min = a, max = b)
    if(u <= 0){
      u <- runif(n = 1, min = 0.0000001, max = 0.01)
    }
    if(u >=1){
      u <- runif(n = 1, min = 0.99, max = 0.9999999)
    }
    dsres_out[i, ] <- qnorm(u)
  }
  resids <- dsres_out[,1]
  resids[is.infinite(resids)] <- NaN
  resids
}

# Pull out starting and ending indices for each series in the object
ends <- seq(0, dim(preds)[2],
            length.out = n_series + 1)
starts <- ends + 1
starts <- c(1, starts[-c(1, (n_series+1))])
ends <- ends[-1]

# Calculate DS residual distributiosn in parallel
cl <- parallel::makePSOCKcluster(n_cores)
setDefaultCluster(cl)
clusterExport(NULL, c('ds_resids_nb',
                      'ds_resids_pois',
                      'ds_resids_tw',
                      'n_series',
                      'obs_series',
                      'series_levels',
                      'family',
                      'rs',
                      'preds',
                      'ends',
                      'starts',
                      'obs_series',
                      'obs_data',
                      'fit_engine'),
              envir = environment())
clusterEvalQ(cl, library(dplyr))
clusterEvalQ(cl, library(MCMCvis))

pbapply::pboptions(type = "none")
series_resids <- pbapply::pblapply(seq_len(n_series), function(series){
  if(class(obs_data)[1] == 'list'){
    n_obs <- data.frame(series = obs_series) %>%
      dplyr::filter(series == !!(series_levels[series])) %>%
      nrow()
  } else {
    n_obs <- obs_data %>%
      dplyr::filter(series == !!(series_levels[series])) %>%
      nrow()
  }

  if(fit_engine == 'stan'){
    preds <- preds[,seq(series,
                        dim(preds)[2],
                        by = n_series)]
  } else {
    preds <- preds[,starts[series]:ends[series]]
  }

  if(class(obs_data)[1] == 'list'){
    obj_dat <- data.frame(y = obs_data$y,
                          series = factor(obs_series,
                                          levels = series_levels))
    truth <- as.vector(obj_dat %>%
                         dplyr::filter(series == !!(series_levels[series])) %>%
                         dplyr::pull(y))
  } else {
    truth <- as.vector(obs_data %>%
                         dplyr::filter(series == !!(series_levels[series])) %>%
                         dplyr::pull(y))
  }


  if(NROW(preds) > 1200){
    sample_seq <- sample(1:NROW(preds), 1200, F)
  } else {
    sample_seq <- 1:NROW(preds)
  }

  if(family == 'Poisson'){
   resids <- do.call(rbind, lapply(sample_seq, function(x){
      suppressWarnings(ds_resids_pois(truth = truth,
                                      fitted = preds[x, ],
                                      draw = preds[x, ]))
    }))
  }

  if(family == 'Negative Binomial'){
    size <- rs[,series]
    resids <- do.call(rbind, lapply(sample_seq, function(x){
      suppressWarnings(ds_resids_nb(truth = truth,
                                    fitted = preds[x, ],
                                    draw = preds[x, ],
                                    size = size[x]))
    }))
  }

  if(family == 'Tweedie'){
    resids <- do.call(rbind, lapply(sample_seq, function(x){
      suppressWarnings(ds_resids_tw(truth = truth,
                                    fitted = preds[x, ],
                                    draw = preds[x, ]))
    }))
  }

resids

}, cl = cl)
stopCluster(cl)
names(series_resids) <- series_levels
return(series_resids)
}
