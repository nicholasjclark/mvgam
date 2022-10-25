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

  # Check arguments
  if(sign(n_cores) != 1){
    stop('argument "n_cores" must be a positive integer',
         call. = FALSE)
  } else {
    if(n_cores%%1 != 0){
      stop('argument "n_cores" must be a positive integer',
           call. = FALSE)
    }
  }

# Extract necessary model elements; for Stan models, expectations are
# stored on the link scale (log)
if(object$fit_engine == 'stan'){
  preds <- exp(MCMCvis::MCMCchains(object$model_output, 'mus'))
} else {
  preds <- MCMCvis::MCMCchains(object$model_output, 'mus')
}

n_series <- NCOL(object$ytimes)
obs_series <- object$obs_data$series
series_levels <- levels(obs_series)
family <- object$family
obs_data <- object$obs_dat
fit_engine <- object$fit_engine

# Create sequences of posterior draws for calculating residual distributions
sample_seq <- 1:NROW(preds)
draw_seq <- sample(sample_seq, length(sample_seq), replace = FALSE)

if(family == 'Negative Binomial'){
  rs <- MCMCvis::MCMCchains(object$model_output, 'r')
} else {
  rs <- NULL
}

# Functions for calculating randomised quantile (Dunn-Smyth) residuals
ds_resids_nb = function(truth, fitted, draw, size){
  na_obs <- is.na(truth)
  a_obs <- pnbinom(as.vector(truth[!na_obs]) - 1,
                                   mu = fitted[!na_obs], size = size)
  b_obs <- pnbinom(as.vector(truth[!na_obs]),
                                   mu = fitted[!na_obs], size = size)
  u_obs <- runif(n = length(draw[!na_obs]),
                 min = pmin(a_obs, b_obs), max = pmax(a_obs, b_obs))

  if(any(is.na(truth))){
    a_na <- pnbinom(as.vector(draw[na_obs]) - 1,
                                    mu = fitted[na_obs], size = size)
    b_na <- pnbinom(as.vector(draw[na_obs]),
                                    mu = fitted[na_obs], size = size)
    u_na <- runif(n = length(draw[na_obs]),
                  min = pmin(a_na, b_na), max = pmax(a_na, b_na))
    u <- vector(length = length(truth))
    u[na_obs] <- u_na
    u[!na_obs] <- u_obs
  } else {
    u <- u_obs
  }
  dsres_out <- qnorm(u)
  dsres_out[is.infinite(dsres_out)] <- NaN
  dsres_out
}

ds_resids_pois = function(truth, fitted, draw){
  na_obs <- is.na(truth)
  a_obs <- ppois(as.vector(truth[!na_obs]) - 1,
                                   lambda = fitted[!na_obs])
  b_obs <- ppois(as.vector(truth[!na_obs]),
                                   lambda = fitted[!na_obs])
  u_obs <- runif(n = length(draw[!na_obs]),
                 min = pmin(a_obs, b_obs), max = pmax(a_obs, b_obs))

  if(any(is.na(truth))){
    a_na <- ppois(as.vector(draw[na_obs]) - 1,
                                    lambda = fitted[na_obs])
    b_na <- ppois(as.vector(draw[na_obs]),
                                    lambda = fitted[na_obs])
    u_na <- runif(n = length(draw[na_obs]),
                  min = pmin(a_na, b_na), max = pmax(a_na, b_na))
    u <- vector(length = length(truth))
    u[na_obs] <- u_na
    u[!na_obs] <- u_obs
  } else {
    u <- u_obs
  }
  dsres_out <- qnorm(u)
  dsres_out[is.infinite(dsres_out)] <- NaN
  dsres_out
}

ds_resids_tw = function(truth, fitted, draw){
  na_obs <- is.na(truth)
  a_obs <- ppois(as.vector(truth[!na_obs]) - 1,
                                 lambda = fitted[!na_obs])
  b_obs <- ppois(as.vector(truth[!na_obs]),
                                 lambda = fitted[!na_obs])
  u_obs <- runif(n = length(draw[!na_obs]),
                 min = pmin(a_obs, b_obs), max = pmax(a_obs, b_obs))

  if(any(is.na(truth))){
    a_na <- ppois(as.vector(draw[na_obs]) - 1,
                                  lambda = fitted[na_obs])
    b_na <- ppois(as.vector(draw[na_obs]),
                                  lambda = fitted[na_obs])
    u_na <- runif(n = length(draw[na_obs]),
                  min = pmin(a_na, b_na), max = pmax(a_na, b_na))
    u <- vector(length = length(truth))
    u[na_obs] <- u_na
    u[!na_obs] <- u_obs
  } else {
    u <- u_obs
  }
  dsres_out <- qnorm(u)
  dsres_out[is.infinite(dsres_out)] <- NaN
  dsres_out
}

# Pull out starting and ending indices for each series in the object
ends <- seq(0, dim(preds)[2],
            length.out = n_series + 1)
starts <- ends + 1
starts <- c(1, starts[-c(1, (n_series+1))])
ends <- ends[-1]

# Calculate DS residual distributions in parallel
cl <- parallel::makePSOCKcluster(n_cores)
setDefaultCluster(cl)
clusterExport(NULL, c('ds_resids_nb',
                      'ds_resids_pois',
                      'ds_resids_tw',
                      'sample_seq',
                      'draw_seq',
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

  truth_mat <- matrix(rep(truth, NROW(preds)),
                      nrow = NROW(preds),
                      byrow = TRUE)

  if(family == 'Poisson'){
    resids <- matrix(ds_resids_pois(truth = as.vector(truth_mat),
                                    fitted = as.vector(preds),
                                    draw = as.vector(preds[draw_seq,])),
                     nrow = NROW(preds))

   # resids <- do.call(rbind, lapply(sample_seq, function(x){
   #    suppressWarnings(ds_resids_pois(truth = truth,
   #                                    fitted = preds[x, ],
   #                                    draw = preds[draw_seq[x], ]))
   #  }))
  }

  if(family == 'Negative Binomial'){
    size <- rs[,series]
    size_mat <- matrix(rep(size, NCOL(preds)),
                       ncol = NCOL(preds))
    resids <- matrix(ds_resids_nb(truth = as.vector(truth_mat),
                                    fitted = as.vector(preds),
                                    draw = as.vector(preds[draw_seq,]),
                                  size = as.vector(size_mat)),
                     nrow = NROW(preds))

    # resids <- do.call(rbind, lapply(sample_seq, function(x){
    #   suppressWarnings(ds_resids_nb(truth = truth,
    #                                 fitted = preds[x, ],
    #                                 draw = preds[draw_seq[x], ],
    #                                 size = size[x]))
    # }))
  }

  if(family == 'Tweedie'){
    resids <- matrix(ds_resids_tw(truth = as.vector(truth_mat),
                                    fitted = as.vector(preds),
                                    draw = as.vector(preds[draw_seq,])),
                     nrow = NROW(preds))
    # resids <- do.call(rbind, lapply(sample_seq, function(x){
    #   suppressWarnings(ds_resids_tw(truth = truth,
    #                                 fitted = preds[x, ],
    #                                 draw = preds[draw_seq[x], ]))
    # }))
  }

resids

}, cl = cl)
stopCluster(cl)
names(series_resids) <- series_levels
return(series_resids)
}
