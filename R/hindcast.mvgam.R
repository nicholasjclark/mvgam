#'@title Extract hindcasts for a fitted \code{mvgam} object
#'@name hindcast.mvgam
#'@importFrom stats predict
#'@inheritParams predict.mvgam
#'@param series Either a \code{integer} specifying which series in the set is to be forecast,
#'or the character string \code{'all'}, specifying that all series should be forecast. This is preferable
#'if the fitted model contained multivariate trends (either as a dynamic factor or \code{VAR} process),
#'as it saves recomputing the full set of trends for each series individually
#'@param ... Ignored
#'@details Posterior retrodictions are drawn from the fitted \code{mvgam} and
#'organized into a convenient format
#'@return An object of class \code{mvgam_forecast} containing hindcast distributions.
#'See \code{\link{mvgam_forecast-class}} for details.
#'#'@seealso \code{\link{forecast.mvgam}}
#'@export
hindcast <- function(object, ...){
  UseMethod("hindcast", object)
}

#'@rdname hindcast.mvgam
#'@method hindcast mvgam
#' @examples
#' \dontrun{
#' simdat <- sim_mvgam(n_series = 3, trend_model = 'AR1')
#' mod <- mvgam(y ~ s(season, bs = 'cc'),
#'             trend_model = 'AR1',
#'             data = simdat$data_train)
#'
#' # Hindcasts on response scale
#' hc <- hindcast(mod)
#' str(hc)
#' plot(hc, series = 1)
#' plot(hc, series = 2)
#' plot(hc, series = 3)
#'
#' # Hindcasts as expectations
#' hc <- hindcast(mod, type = 'expected')
#' str(hc)
#' plot(hc, series = 1)
#' plot(hc, series = 2)
#' plot(hc, series = 3)
#'
#' # Estimated latent trends
#' hc <- hindcast(mod, type = 'trend')
#' str(hc)
#' plot(hc, series = 1)
#' plot(hc, series = 2)
#' plot(hc, series = 3)
#' }
#'@export
hindcast.mvgam = function(object, series = 'all',
                          type = 'response',
                          ...){

  # Check arguments
  if(!(inherits(object, "mvgam"))) {
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

  type <- match.arg(arg = type, choices = c("link", "response", "trend",
                                            "expected", "latent_N", "detection"))

  if(series != 'all'){
    s_name <- levels(data_train$series)[series]
  }
  n_series <- NCOL(object$ytimes)

last_train <- max(object$obs_data$time) -
  (min(object$obs_data$time) - 1)

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
                         'expected' = 'mus',
                         'response' = 'ypred',
                         'trend' = 'trend',
                         'latent_N' = 'mus',
                         'detection' = 'mus')
    if(object$fit_engine == 'stan'){
      preds <- mcmc_chains(object$model_output, to_extract)[,seq(series,
                                                                 dim(mcmc_chains(object$model_output, 'ypred'))[2],
                                                                 by = NCOL(object$ytimes))][,1:last_train]
    } else {
      preds <- mcmc_chains(object$model_output, to_extract)[,starts[series]:ends[series]][,1:last_train]
    }

    if(type %in% c('expected', 'latent_N', 'detection')){

      # Extract family-specific parameters for this series
      family_pars <- extract_family_pars(object = object)
      par_extracts <- lapply(seq_along(family_pars), function(j){
        if(is.matrix(family_pars[[j]])){
          family_pars[[j]][, series]
        } else {
          family_pars[[j]]
        }
      })
      names(par_extracts) <- names(family_pars)

      # Compute expectations as one long vector
      Xpmat <- matrix(as.vector(preds))
      attr(Xpmat, 'model.offset') <- 0

      if(object$family == 'nmix'){
        if(type != 'expected'){
          preds <- mcmc_chains(object$model_output, 'detprob')[,object$ytimes[1:last_train, series]]
          Xpmat <- matrix(qlogis(as.vector(preds)))
          attr(Xpmat, 'model.offset') <- 0
        }
        latent_lambdas <- as.vector(mcmc_chains(object$model_output, 'trend')[,seq(series,
                                                                                   dim(mcmc_chains(object$model_output, 'ypred'))[2],
                                                                                   by = NCOL(object$ytimes))][,1:last_train])

        latent_lambdas <- exp(latent_lambdas)
        n_draws <- dim(mcmc_chains(object$model_output, 'ypred'))[1]
        cap <- as.vector(t(replicate(n_draws,
                                     object$obs_data$cap[which(as.numeric(object$obs_data$series) == series)])))
      } else {
        latent_lambdas <- NULL
        cap <- NULL
      }

      if(type == 'latent_N'){
        preds <- mcmc_chains(object$model_output, 'latent_ypred')[,seq(series,
                                                                       dim(mcmc_chains(object$model_output, 'ypred'))[2],
                                                                       by = NCOL(object$ytimes))][,1:last_train]
      } else {
        preds <- matrix(as.vector(mvgam_predict(family = object$family,
                                                Xp = Xpmat,
                                                latent_lambdas = latent_lambdas,
                                                cap = cap,
                                                type = type,
                                                betas = 1,
                                                family_pars = par_extracts)),
                        nrow = NROW(preds))
      }

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
                       'expected' = 'mus',
                       'response' = 'ypred',
                       'trend' = 'trend',
                       'latent_N' = 'mus',
                       'detection' = 'mus')

  # Extract forecasts
  if(object$fit_engine == 'stan'){
    preds <- mcmc_chains(object$model_output, to_extract)[,seq(series,
                                                               dim(mcmc_chains(object$model_output, 'ypred'))[2],
                                                               by = NCOL(object$ytimes))]
  } else {
    preds <- mcmc_chains(object$model_output, to_extract)[,starts[series]:ends[series]]
  }

  if(type %in% c('expected', 'latent_N', 'detection')){

    # Compute expectations as one long vector
    Xpmat <- matrix(as.vector(preds))
    attr(Xpmat, 'model.offset') <- 0

    family_pars <- extract_family_pars(object = object)
    par_extracts <- lapply(seq_along(family_pars), function(j){
      if(is.matrix(family_pars[[j]])){
        family_pars[[j]][, series]
      } else {
        family_pars[[j]]
      }
    })
    names(par_extracts) <- names(family_pars)

    if(object$family == 'nmix'){
      if(type != 'expected'){
        preds <- mcmc_chains(object$model_output, 'detprob')[,object$ytimes[, series]]
        Xpmat <- matrix(qlogis(as.vector(preds)))
        attr(Xpmat, 'model.offset') <- 0
      }

      latent_lambdas <- as.vector(mcmc_chains(object$model_output, 'trend')[,seq(series,
                                                                                 dim(mcmc_chains(object$model_output, 'ypred'))[2],
                                                                                 by = NCOL(object$ytimes))])
      latent_lambdas <- exp(latent_lambdas)
      cap <- object$obs_data$cap[which(as.numeric(object$obs_data$series) == series)]

      n_draws <- dim(mcmc_chains(object$model_output, 'ypred'))[1]
      cap <- as.vector(t(replicate(n_draws,
                                   object$obs_data$cap[which(as.numeric(object$obs_data$series) == series)])))

    } else {
      latent_lambdas <- NULL
      cap <- NULL
    }

    if(type == 'latent_N'){
      preds <- mcmc_chains(object$model_output, 'latent_ypred')[,seq(series,
                                                                     dim(mcmc_chains(object$model_output, 'ypred'))[2],
                                                                     by = NCOL(object$ytimes))]
    } else {
    preds <- matrix(as.vector(mvgam_predict(family = object$family,
                                            Xp = Xpmat,
                                            latent_lambdas = latent_lambdas,
                                            cap = cap,
                                            type = type,
                                            betas = 1,
                                            family_pars = par_extracts)),
                    nrow = NROW(preds))
    }
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
                             train_times = unique(data_train$time),
                             test_observations = NULL,
                             test_times = NULL,
                             hindcasts = series_hcs,
                             forecasts = NULL),
                        class = 'mvgam_forecast')

return(series_fcs)
}
