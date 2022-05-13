#'Residual calculations for a fitted mvjagam object
#'
#'This function takes a fitted \code{mvjagam} object and returns distributions of Dunn-Smyth residuals
#'
#'@param object \code{list} object returned from \code{mvjagam}
#'@param n_cores \code{integer} specifying number of cores for generating residual distributions in parallel
#'@author Nicholas J Clark
#'@details Dunn-Smyth residual distributions are calculated for each series in the fitted object
#'@return A \code{list} of residual distributions
get_mvgam_resids = function(object, n_cores = 1){

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
ends <- seq(0, dim(MCMCvis::MCMCchains(object$model_output, 'ypred'))[2],
            length.out = NCOL(object$ytimes) + 1)
starts <- ends + 1
starts <- c(1, starts[-c(1, (NCOL(object$ytimes)+1))])
ends <- ends[-1]


# Calculate DS residual distributiosn in parallel
cl <- parallel::makePSOCKcluster(n_cores)
setDefaultCluster(cl)
clusterExport(NULL, c('ds_resids_nb',
                      'ds_resids_pois',
                      'ds_resids_tw',
                      'ends',
                      'starts',
                      'object'),
              envir = environment())
clusterEvalQ(cl, library(dplyr))
clusterEvalQ(cl, library(MCMCvis))

pbapply::pboptions(type = "none")
series_resids <- pbapply::pblapply(seq_len(NCOL(object$ytimes)), function(series){
  if(class(object$obs_data)[1] == 'list'){
    n_obs <- data.frame(series = object$obs_data$series) %>%
      dplyr::filter(series == !!(levels(object$obs_data$series)[series])) %>%
      nrow()
  } else {
    n_obs <- object$obs_data %>%
      dplyr::filter(series == !!(levels(object$obs_data$series)[series])) %>%
      nrow()
  }
  preds <- MCMCvis::MCMCchains(object$model_output, 'ypred')[,starts[series]:ends[series]]

  if(class(object$obs_data)[1] == 'list'){
    obj_dat <- data.frame(y = object$obs_data$y,
                          series = factor(object$obs_data$series,
                                          levels = levels(object$obs_data$series)))
    truth <- as.vector(obj_dat %>%
                         dplyr::filter(series == !!(levels(obj_dat$series)[series])) %>%
                         dplyr::pull(y))
  } else {
    truth <- as.vector(object$obs_data %>%
                         dplyr::filter(series == !!(levels(object$obs_data$series)[series])) %>%
                         dplyr::pull(y))
  }


  if(NROW(preds) > 2000){
    sample_seq <- sample(1:NROW(preds), 2000, F)
  } else {
    sample_seq <- 1:NROW(preds)
  }

  if(object$family == 'Poisson'){
   resids <- do.call(rbind, lapply(sample_seq, function(x){
      suppressWarnings(ds_resids_pois(truth = truth,
                                      fitted = preds[x, ],
                                      draw = preds[x, ]))
    }))
  }

  if(object$family == 'Negative Binomial'){
    size <- MCMCvis::MCMCchains(object$model_output, 'r')[,series]
    resids <- do.call(rbind, lapply(sample_seq, function(x){
      suppressWarnings(ds_resids_nb(truth = truth,
                                    fitted = preds[x, ],
                                    draw = preds[x, ],
                                    size = size[x]))
    }))
  }

  if(object$family == 'Tweedie'){
    resids <- do.call(rbind, lapply(sample_seq, function(x){
      suppressWarnings(ds_resids_tw(truth = truth,
                                    fitted = preds[x, ],
                                    draw = preds[x, ]))
    }))
  }

resids

}, cl = cl)
stopCluster(cl)
names(series_resids) <- levels(object$obs_data$series)
return(series_resids)
}
