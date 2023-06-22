#'@title Extract hindcasts for a fitted \code{mvgam} object
#'@name hindcast.mvgam
#'@importFrom stats predict
#'@param object \code{list} object returned from \code{mvgam}. See [mvgam()]
#'@param series Either a \code{integer} specifying which series in the set is to be forecast,
#'or the character string \code{'all'}, specifying that all series should be forecast. This is preferable
#'if the fitted model contained multivariate trends (either as a dynamic factor or \code{VAR} process),
#'as it saves recomputing the full set of trends for each series individually
#'@param type When this has the value \code{link}, the linear predictor is calculated on the log link scale.
#'When \code{response} is used, the predictions take uncertainty in the observation process into account to return
#'predictions on the outcome (discrete) scale (default). When \code{trend} is used, only the hindcast distribution for the
#'latent trend is returned
#'@param ... Ignored
#'@details Posterior retrodictions are drawn from the fitted \code{mvgam} and
#'organized into a convenient format
#'@return An object of class \code{mvgam_forecast} containing hindcast distributions.
#'See \code{\link{mvgam_forecast-class}} for details.
#'
#'@export
hindcast <- function(object, ...){
  UseMethod("hindcast", object)
}

#'@rdname hindcast.mvgam
#'@method hindcast mvgam
#'@export
hindcast.mvgam = function(object, series = 'all',
                          type = 'response',
                          ...){

  # Check arguments
  if (!(inherits(object, "mvgam"))) {
    stop('argument "object" must be of class "mvgam"')
  }

  if(is.character(series)){
    if(series != 'all'){
      stop('argument "series" must be either a positive integer or "all"',
           call. =  FALSE)
    }
  } else {
    if(sign(series) != 1){
      stop('argument "series" must be either a positive integer or "all"',
           call. = FALSE)
    } else {
      if(series%%1 != 0){
        stop('argument "series" must be either a positive integer or "all"',
             call. = FALSE)
      }
    }

    if(series > NCOL(object$ytimes)){
      stop(paste0('object only contains data / predictions for ', NCOL(object$ytimes), ' series'),
           call. = FALSE)
    }
  }

  type <- match.arg(arg = type, choices = c("link", "response", "trend"))

  if(series != 'all'){
    s_name <- levels(data_train$series)[series]
  }
  n_series <- NCOL(object$ytimes)

last_train <- max(object$obs_data$time)

if(series == 'all'){
  data_train <- object$obs_data
  ends <- seq(0, dim(mcmc_chains(object$model_output, 'ypred'))[2],
              length.out = NCOL(object$ytimes) + 1)
  starts <- ends + 1
  starts <- c(1, starts[-c(1, (NCOL(object$ytimes)+1))])
  ends <- ends[-1]

  # Extract hindcasts for storing in the returned object
  series_hcs <- lapply(seq_len(n_series), function(series){
    to_extract <- switch(type,
                         'link' = 'mus',
                         'response' = 'ypred',
                         'trend' = 'trend')
    if(object$fit_engine == 'stan'){

      preds <- mcmc_chains(object$model_output, to_extract)[,seq(series,
                                                                 dim(mcmc_chains(object$model_output, 'ypred'))[2],
                                                                 by = NCOL(object$ytimes))][,1:last_train]
    } else {
      preds <- mcmc_chains(object$model_output, to_extract)[,starts[series]:ends[series]][,1:last_train]
    }
    preds
  })
  names(series_hcs) <- levels(data_train$series)

  series_obs <- lapply(seq_len(n_series), function(series){
    s_name <- levels(object$obs_data$series)[series]
    data.frame(series = object$obs_data$series,
               time = object$obs_data$time,
               y = object$obs_data$y) %>%
      dplyr::filter(series == s_name) %>%
      dplyr::arrange(time) %>%
      dplyr::pull(y)
  })
  names(series_obs) <- levels(data_train$series)

} else {
  data_train <- object$obs_data
  ends <- seq(0, dim(mcmc_chains(object$model_output, 'ypred'))[2],
              length.out = NCOL(object$ytimes) + 1)
  starts <- ends + 1
  starts <- c(1, starts[-c(1, (NCOL(object$ytimes)+1))])
  ends <- ends[-1]
  to_extract <- switch(type,
                       'link' = 'mus',
                       'response' = 'ypred',
                       'trend' = 'trend')

  # Extract forecasts
  if(object$fit_engine == 'stan'){
    preds <- mcmc_chains(object$model_output, to_extract)[,seq(series,
                                                               dim(mcmc_chains(object$model_output, 'ypred'))[2],
                                                               by = NCOL(object$ytimes))]
  } else {
    preds <- mcmc_chains(object$model_output, to_extract)[,starts[series]:ends[series]]
  }

  # Extract hindcasts
  series_hcs <- list(preds[,1:last_train])
  names(series_hcs) <- s_name

  # Training observations
  series_obs <- list(data.frame(series = object$obs_data$series,
                                time = object$obs_data$time,
                                y = object$obs_data$y) %>%
                       dplyr::filter(series == s_name) %>%
                       dplyr::arrange(time) %>%
                       dplyr::pull(y))
  names(series_obs) <- s_name
}

series_fcs <- structure(list(call = object$call,
                             trend_call = object$trend_call,
                             family = object$family,
                             trend_model = object$trend_model,
                             drift = object$drift,
                             use_lv = object$use_lv,
                             fit_engine = object$fit_engine,
                             type = type,
                             series_names = levels(data_train$series),
                             train_observations = series_obs,
                             test_observations = NULL,
                             hindcasts = series_hcs,
                             forecasts = NULL),
                        class = 'mvgam_forecast')

return(series_fcs)
}
