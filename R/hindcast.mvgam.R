#'@title Extract hindcasts for a fitted \code{mvgam} object
#'@name hindcast.mvgam
#'@importFrom stats predict
#'@inheritParams predict.mvgam
#'@param ... Ignored
#'@details Posterior retrodictions are drawn from the fitted \code{mvgam} and
#'organized into a convenient format
#'@return An object of class \code{mvgam_forecast} containing hindcast distributions.
#'See \code{\link{mvgam_forecast-class}} for details.
#'
#'@seealso \code{\link{forecast.mvgam}}
#'@export
hindcast <- function(object, ...){
  UseMethod("hindcast", object)
}

#'@rdname hindcast.mvgam
#'@method hindcast mvgam
#' @examples
#' \donttest{
#' simdat <- sim_mvgam(n_series = 3, trend_model = AR())
#' mod <- mvgam(y ~ s(season, bs = 'cc'),
#'             trend_model = AR(),
#'             noncentred = TRUE,
#'             data = simdat$data_train,
#'             chains = 2)
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
hindcast.mvgam = function(object,
                          type = 'response',
                          ...){

  # Check arguments
  series <- 'all'
  type <- match.arg(arg = type,
                    choices = c("link", "response", "trend",
                                "expected", "latent_N", "detection"))

  data_train <- object$obs_data
  data_train <- validate_series_time(data_train,
                                     trend_model = attr(object$model_data,
                                                             'trend_model'))
  last_train <- max(data_train$index..time..index) -
    (min(data_train$index..time..index) - 1)

  n_series <- NCOL(object$ytimes)
  n_predcols <- dim(mcmc_chains(object$model_output, 'ypred'))
  ends <- seq(0, n_predcols[2], length.out = NCOL(object$ytimes) + 1)
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
    if(object$family == 'nmix' & type == 'link'){
      to_extract <- 'trend'
    }

    if(object$fit_engine == 'stan'){
      preds <- mcmc_chains(object$model_output, to_extract)[,seq(series,
                                                                 n_predcols[2],
                                                                 by = n_series),
                                                            drop = FALSE][,1:last_train]
    } else {
      preds <- mcmc_chains(object$model_output, to_extract)[,starts[series]:ends[series],
                                                            drop = FALSE][,1:last_train]
    }

    if(object$family == 'nmix' & type == 'link'){
      preds <- exp(preds)
    }

    if(type %in% c('expected', 'latent_N', 'detection')){

      # Extract family-specific parameters for this series
      family_pars <- extract_family_pars(object = object)
      par_extracts <- lapply(seq_along(family_pars), function(j){
        if(is.matrix(family_pars[[j]])){
          as.vector(matrix(rep(as.vector(family_pars[[j]][, series]),
                               NCOL(preds)),
                           nrow = NROW(preds),
                           byrow = FALSE))

        } else {
          as.vector(matrix(rep(family_pars[[j]],
                               NCOL(preds)),
                           nrow = NROW(preds),
                           byrow = FALSE))
        }
      })
      names(par_extracts) <- names(family_pars)

      # Add trial information if this is a Binomial model
      if(object$family %in% c('binomial', 'beta_binomial')){
        trials <- as.vector(matrix(rep(as.vector(attr(object$mgcv_model,
                                                      'trials')[,series]),
                                       NROW(preds)),
                                   nrow = NROW(preds),
                                   byrow = TRUE))
        par_extracts$trials <- trials
      }

      # Compute expectations as one long vector
      Xpmat <- matrix(as.vector(preds))
      attr(Xpmat, 'model.offset') <- 0

      if(object$family == 'nmix'){
        preds <- mcmc_chains(object$model_output,
                             'detprob')[,object$ytimes[1:last_train, series],
                                        drop = FALSE]
        Xpmat <- matrix(qlogis(as.vector(preds)))
        attr(Xpmat, 'model.offset') <- 0
        latent_lambdas <- as.vector(mcmc_chains(object$model_output,
                                                'trend')[,seq(series,
                                                              n_predcols[2],
                                                              by = n_series),
                                                         drop = FALSE][,1:last_train])

        latent_lambdas <- exp(latent_lambdas)
        cap <- as.vector(t(replicate(n_predcols[1],
                                     object$obs_data$cap[which(as.numeric(object$obs_data$series) == series)])))
      } else {
        latent_lambdas <- NULL; cap <- NULL
      }

      if(type == 'latent_N'){
        preds <- mcmc_chains(object$model_output,
                             'latent_ypred')[,seq(series,
                                                  n_predcols[2],
                                                  by = n_series),
                                             drop = FALSE][,1:last_train]
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
    s_name <- levels(data_train$series)[series]
    data.frame(series = data_train$series,
               time = data_train$index..time..index,
               y = data_train$y) %>%
      dplyr::filter(series == s_name) %>%
      dplyr::arrange(time) %>%
      dplyr::pull(y)
  })
  names(series_obs) <- levels(data_train$series)

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
