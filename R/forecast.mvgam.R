#'@title Extract or compute hindcasts and forecasts for a fitted \code{mvgam} object
#'@name forecast.mvgam
#'@importFrom stats predict
#'@importFrom rlang missing_arg
#'@inheritParams predict.mvgam
#'@param newdata Optional \code{dataframe} or \code{list} of test data containing the same variables
#'that were included in the original `data` used to fit the model. If included, the
#'covariate information in \code{newdata} will be used to generate forecasts from the fitted model equations. If
#'this same \code{newdata} was originally included in the call to \code{mvgam}, then forecasts have already been
#'produced by the generative model and these will simply be extracted and plotted. However if no \code{newdata} was
#'supplied to the original model call, an assumption is made that the \code{newdata} supplied here comes sequentially
#'after the data supplied in the original model (i.e. we assume there is no time gap between the last
#'observation of series 1 in the original data and the first observation for series 1 in \code{newdata})
#'@param data_test Deprecated. Still works in place of \code{newdata} but users are recommended to use
#'\code{newdata} instead for more seamless integration into `R` workflows
#'@param n_cores Deprecated. Parallel processing is no longer supported
#'@param ... Ignored
#'@details Posterior predictions are drawn from the fitted \code{mvgam} and used to simulate a forecast distribution
#'@return An object of class \code{mvgam_forecast} containing hindcast and forecast distributions.
#'See \code{\link{mvgam_forecast-class}} for details.
#'@seealso \code{\link{hindcast}}, \code{\link{score}}, \code{\link{ensemble}}
#'@export
forecast <- function(object, ...){
  UseMethod("forecast", object)
}

#'@rdname forecast.mvgam
#'@method forecast mvgam
#' @examples
#' \donttest{
#' simdat <- sim_mvgam(n_series = 3, trend_model = AR())
#' mod <- mvgam(y ~ s(season, bs = 'cc', k = 6),
#'             trend_model = AR(),
#'             noncentred = TRUE,
#'             data = simdat$data_train,
#'             chains = 2,
#'             silent = 2)
#'
#' # Hindcasts on response scale
#' hc <- hindcast(mod)
#' str(hc)
#' plot(hc, series = 1)
#' plot(hc, series = 2)
#' plot(hc, series = 3)
#'
#' # Forecasts on response scale
#' fc <- forecast(mod, newdata = simdat$data_test)
#' str(fc)
#' plot(fc, series = 1)
#' plot(fc, series = 2)
#' plot(fc, series = 3)
#'
#' # Forecasts as expectations
#' fc <- forecast(mod, newdata = simdat$data_test, type = 'expected')
#' plot(fc, series = 1)
#' plot(fc, series = 2)
#' plot(fc, series = 3)
#'
#' # Dynamic trend extrapolations
#' fc <- forecast(mod, newdata = simdat$data_test, type = 'trend')
#' plot(fc, series = 1)
#' plot(fc, series = 2)
#' plot(fc, series = 3)
#' }
#'@export
forecast.mvgam = function(object,
                          newdata,
                          data_test,
                          n_cores = 1,
                          type = 'response',
                          ...){
  # Check arguments
  validate_pos_integer(n_cores)
  if(n_cores > 1L){
    message('argument "n_cores" is deprecated')
  }

  if(!missing("newdata")){
    data_test <- newdata
  }

  if(missing("newdata") & missing(data_test) & is.null(object$test_data)){
    stop('newdata must be supplied to compute forecasts',
         call. = FALSE)
  }

  type <- match.arg(arg = type, choices = c("link", "response",
                                            "trend", "expected",
                                            "detection", "latent_N"))

  if(inherits(object, 'jsdgam')){
    orig_trend_model <- attr(object$model_data, 'prepped_trend_model')
  } else {
    orig_trend_model <- object$trend_model
  }

  data_train <- validate_series_time(object$obs_data,
                                     trend_model = orig_trend_model)
  n_series <- NCOL(object$ytimes)

  # Check whether a forecast has already been computed
  forecasts_exist <- FALSE
  if(!is.null(object$test_data) && !missing(data_test)){
    object$test_data <- validate_series_time(object$test_data,
                                             trend_model = orig_trend_model)
    data_test <- validate_series_time(data_test,
                                      trend_model = orig_trend_model)
    if(max(data_test$index..time..index) <=
       max(object$test_data$index..time..index)){
      forecasts_exist <- TRUE
    } else {
      data.frame(time = data_test$time) %>%
        dplyr::mutate(rowid = dplyr::row_number()) %>%
        dplyr::filter(time > max(object$test_data$index..time..index)) %>%
        dplyr::pull(rowid) -> idx
      if(inherits(data_test, 'list')){
        data_arranged <- data_test
        data_arranged <- lapply(data_test, function(x){
          if(is.matrix(x)){
            matrix(x[idx,], ncol = NCOL(x))
          } else {
            x[idx]
          }
        })
        names(data_arranged) <- names(data_test)
        data_test <- data_arranged
      } else {
        data_test <- data_test[idx, ]
      }
    }
  }

  if(!is.null(object$test_data) && missing(data_test)){
    forecasts_exist <- TRUE
  }

  if(is.null(object$test_data)){
    data_test <- validate_series_time(data_test, name = 'newdata',
                                      trend_model = orig_trend_model)
    data.frame(series = object$obs_data$series,
               time = object$obs_data$time) %>%
      dplyr::group_by(series) %>%
      dplyr::summarise(maxt = max(time)) -> series_max_ts

    data.frame(series = data_test$series,
               time = data_test$time) %>%
      dplyr::mutate(orig_rows = dplyr::row_number()) %>%
      dplyr::left_join(series_max_ts, by = 'series') %>%
      dplyr::filter(time > maxt) %>%
      dplyr::pull(orig_rows) -> idx

    if(inherits(data_test, 'list')){
      data_arranged <- data_test
      data_arranged <- lapply(data_test, function(x){
        if(is.matrix(x)){
          matrix(x[idx,], ncol = NCOL(x))
        } else {
          x[idx]
        }
      })
      names(data_arranged) <- names(data_test)
      data_test <- data_arranged
    } else {
      data_test <- data_test[idx, ]
    }
  }

  # Only compute forecasts if they don't already exist!
  if(!forecasts_exist){
    resp_terms <- as.character(terms(formula(object))[[2]])
    if(length(resp_terms) == 1){
      out_name <- as.character(terms(formula(object))[[2]])
    } else {
      if(any(grepl('cbind', resp_terms))){
        resp_terms <- resp_terms[-grepl('cbind', resp_terms)]
        out_name <- resp_terms[1]
      }
    }

    if(out_name != 'y'){
      data_test$y <- data_test[[out_name]]
    }

    if(!missing(data_test)){
      if(!'y' %in% names(data_test)){
        data_test$y <- rep(NA, NROW(data_test))
      }
      data_test <- validate_series_time(data_test, name = 'newdata',
                                        trend_model = orig_trend_model)
    }

    # Generate draw-specific forecasts
    fc_preds <- forecast_draws(object = object,
                               type = type,
                               series = 'all',
                               data_test = data_test,
                               n_cores = n_cores,
                               ...)

    # Extract forecasts into the correct format
    series_fcs <- lapply(seq_len(n_series), function(series){
      indexed_forecasts <- do.call(rbind, lapply(seq_along(fc_preds), function(x){
        fc_preds[[x]][[series]]
      }))
      indexed_forecasts
    })
    names(series_fcs) <- levels(data_test$series)

    # Extract hindcasts
    data_train <- validate_series_time(object$obs_data,
                                       trend_model = orig_trend_model)
    ends <- seq(0, dim(mcmc_chains(object$model_output, 'ypred'))[2],
                length.out = NCOL(object$ytimes) + 1)
    starts <- ends + 1
    starts <- c(1, starts[-c(1, (NCOL(object$ytimes)+1))])
    ends <- ends[-1]

    series_hcs <- lapply(seq_len(n_series), function(series){
      to_extract <- switch(type,
                           'link' = 'mus',
                           'expected' = 'mus',
                           'response' = 'ypred',
                           'trend' = 'trend',
                           'latent_N' = 'mus',
                           'detection' = 'mus')
      if(object$family == 'nmix' &
         type == 'link'){
        to_extract <- 'trend'
      }
      if(object$fit_engine == 'stan'){

        preds <- mcmc_chains(object$model_output, to_extract)[,seq(series,
                                                                   dim(mcmc_chains(object$model_output, 'ypred'))[2],
                                                                   by = NCOL(object$ytimes)),
                                                              drop = FALSE]
      } else {
        preds <- mcmc_chains(object$model_output, to_extract)[,starts[series]:ends[series],
                                                              drop = FALSE]
      }

      if(object$family == 'nmix' &
         type == 'link'){
        preds <- exp(preds)
      }

      if(type %in% c('expected', 'latent_N', 'detection')){

        # Compute expectations as one long vector
        Xpmat <- matrix(as.vector(preds))
        attr(Xpmat, 'model.offset') <- 0

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
          trials <- as.vector(matrix(rep(as.vector(attr(object$mgcv_model, 'trials')[,series]),
                                         NROW(preds)),
                                     nrow = NROW(preds),
                                     byrow = TRUE))
          par_extracts$trials <- trials
        }

        if(object$family == 'nmix'){
          preds <- mcmc_chains(object$model_output, 'detprob')[,object$ytimes[, series],
                                                               drop = FALSE]
          Xpmat <- matrix(qlogis(as.vector(preds)))
          attr(Xpmat, 'model.offset') <- 0
          latent_lambdas <- as.vector(mcmc_chains(object$model_output, 'trend')[,seq(series,
                                                                                     dim(mcmc_chains(object$model_output, 'ypred'))[2],
                                                                                     by = NCOL(object$ytimes)),
                                                                                drop = FALSE])
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
                                                                         by = NCOL(object$ytimes)),
                                                                    drop = FALSE]
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
    names(series_hcs) <- levels(data_test$series)

    # Extract observations
    series_obs <- lapply(seq_len(n_series), function(series){
      s_name <- levels(object$obs_data$series)[series]
      data.frame(series = object$obs_data$series,
                 time = object$obs_data$index..time..index,
                 y = object$obs_data$y) %>%
        dplyr::filter(series == s_name) %>%
        dplyr::arrange(time) %>%
        dplyr::pull(y)
    })
    names(series_obs) <- levels(data_test$series)

    series_test <- lapply(seq_len(n_series), function(series){
      s_name <- levels(object$obs_data$series)[series]
      data.frame(series = data_test$series,
                 time = data_test$index..time..index,
                 y = data_test$y) %>%
        dplyr::filter(series == s_name) %>%
        dplyr::arrange(time) %>%
        dplyr::pull(y)
    })
    names(series_test) <- levels(data_test$series)

  } else {
    # If forecasts already exist, simply extract them
    data_test <- validate_series_time(object$test_data,
                                      trend_model = orig_trend_model)
    last_train <- max(object$obs_data$index..time..index) -
      (min(object$obs_data$index..time..index) - 1)

    data_train <- validate_series_time(object$obs_data,
                                       trend_model = orig_trend_model)
    ends <- seq(0, dim(mcmc_chains(object$model_output, 'ypred'))[2],
                length.out = NCOL(object$ytimes) + 1)
    starts <- ends + 1
    starts <- c(1, starts[-c(1, (NCOL(object$ytimes)+1))])
    ends <- ends[-1]

    series_fcs <- lapply(seq_len(n_series), function(series){
      to_extract <- switch(type,
                           'link' = 'mus',
                           'expected' = 'mus',
                           'response' = 'ypred',
                           'trend' = 'trend',
                           'latent_N' = 'mus',
                           'detection' = 'mus')
      if(object$family == 'nmix' &
         type == 'link'){
        to_extract <- 'trend'
      }

      if(object$fit_engine == 'stan'){

        preds <- mcmc_chains(object$model_output, to_extract)[,seq(series,
                                                                   dim(mcmc_chains(object$model_output, 'ypred'))[2],
                                                                   by = NCOL(object$ytimes)),
                                                              drop = FALSE]
      } else {
        preds <- mcmc_chains(object$model_output, to_extract)[,starts[series]:ends[series],
                                                              drop = FALSE]
      }

      if(object$family == 'nmix' &
         type == 'link'){
        preds <- exp(preds)
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

        # Add trial information if this is a Binomial model
        if(object$family %in% c('binomial', 'beta_binomial')){
          trials <- as.vector(matrix(rep(as.vector(attr(object$mgcv_model, 'trials')[,series]),
                                         NROW(mus)),
                                     nrow = NROW(mus),
                                     byrow = TRUE))
          par_extracts$trials <- trials
        }

        if(object$family == 'nmix'){
          preds <- mcmc_chains(object$model_output, 'detprob')[,object$ytimes[, series],
                                                               drop = FALSE]
          Xpmat <- matrix(qlogis(as.vector(preds)))
          attr(Xpmat, 'model.offset') <- 0
          n_draws <- dim(mcmc_chains(object$model_output, 'ypred'))[1]
          n_cols <- dim(mcmc_chains(object$model_output, 'ypred'))[2]
          latent_lambdas <- as.vector(mcmc_chains(object$model_output, 'trend')[,seq(series,
                                                                                     n_cols,
                                                                                     by = NCOL(object$ytimes)),
                                                                                drop = FALSE])
          latent_lambdas <- exp(latent_lambdas)
          cap <- as.vector(t(replicate(n_draws,
                                       c(object$obs_data$cap[which(as.numeric(object$obs_data$series) == series)],
                                         object$test_data$cap[which(as.numeric(object$test_data$series) == series)]))))

        } else {
          latent_lambdas <- NULL; cap <- NULL
        }

        if(type == 'latent_N'){
          preds <- mcmc_chains(object$model_output, 'latent_ypred')[,seq(series,
                                                                         dim(mcmc_chains(object$model_output, 'ypred'))[2],
                                                                         by = NCOL(object$ytimes)),
                                                                    drop = FALSE]
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
      preds[,(last_train+1):NCOL(preds)]
    })
    names(series_fcs) <- levels(data_train$series)

    # Extract hindcasts for storing in the returned object
    series_hcs <- lapply(seq_len(n_series), function(series){
      to_extract <- switch(type,
                           'link' = 'mus',
                           'expected' = 'mus',
                           'response' = 'ypred',
                           'trend' = 'trend',
                           'latent_N' = 'mus',
                           'detection' = 'mus')
      if(object$family == 'nmix' &
         type == 'link'){
        to_extract <- 'trend'
      }
      if(object$fit_engine == 'stan'){

        preds <- mcmc_chains(object$model_output, to_extract)[,seq(series,
                                                                   dim(mcmc_chains(object$model_output, 'ypred'))[2],
                                                                   by = NCOL(object$ytimes)),
                                                              drop = FALSE][,1:last_train]
      } else {
        preds <- mcmc_chains(object$model_output, to_extract)[,starts[series]:ends[series],
                                                              drop = FALSE][,1:last_train]
      }

      if(object$family == 'nmix' &
         type == 'link'){
        preds <- exp(preds)
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

        # Add trial information if this is a Binomial model
        if(object$family %in% c('binomial', 'beta_binomial')){
          trials <- as.vector(matrix(rep(as.vector(attr(object$mgcv_model, 'trials')[1:last_train, series]),
                                         NROW(mus)),
                                     nrow = NROW(mus),
                                     byrow = TRUE))
          par_extracts$trials <- trials
        }

        if(object$family == 'nmix'){
          preds <- mcmc_chains(object$model_output, 'detprob')[,object$ytimes[1:last_train, series],
                                                               drop = FALSE]
          Xpmat <- matrix(qlogis(as.vector(preds)))
          attr(Xpmat, 'model.offset') <- 0
          n_draws <- dim(mcmc_chains(object$model_output, 'ypred'))[1]
          n_cols <- dim(mcmc_chains(object$model_output, 'ypred'))[2]
          latent_lambdas <- as.vector(mcmc_chains(object$model_output, 'trend')[,seq(series,
                                                                                     n_cols,
                                                                                     by = NCOL(object$ytimes)),
                                                                                drop = FALSE][,1:last_train])
          latent_lambdas <- exp(latent_lambdas)
          cap <- as.vector(t(replicate(n_draws,
                                       object$obs_data$cap[which(as.numeric(object$test_data$series) == series)])))

        } else {
          latent_lambdas <- NULL
          cap <- NULL
        }

        if(type == 'latent_N'){
          preds <- mcmc_chains(object$model_output, 'latent_ypred')[,seq(series,
                                                                         dim(mcmc_chains(object$model_output, 'ypred'))[2],
                                                                         by = NCOL(object$ytimes)),
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
      s_name <- levels(object$obs_data$series)[series]
      data.frame(series = object$obs_data$series,
                 time = object$obs_data$index..time..index,
                 y = object$obs_data$y) %>%
        dplyr::filter(series == s_name) %>%
        dplyr::arrange(time) %>%
        dplyr::pull(y)
    })
    names(series_obs) <- levels(data_train$series)

    series_test <- lapply(seq_len(n_series), function(series){
      s_name <- levels(object$obs_data$series)[series]
      data.frame(series = object$test_data$series,
                 time = object$test_data$index..time..index,
                 y = object$test_data$y) %>%
        dplyr::filter(series == s_name) %>%
        dplyr::arrange(time) %>%
        dplyr::pull(y)
    })
    names(series_test) <- levels(data_train$series)
  }

  series_fcs <- structure(list(call = object$call,
                               trend_call = object$trend_call,
                               family = object$family,
                               family_pars = if(type == 'link'){
                                 extract_family_pars(object = object)
                               } else {
                                 NULL
                               },
                               trend_model = object$trend_model,
                               drift = object$drift,
                               use_lv = object$use_lv,
                               fit_engine = object$fit_engine,
                               type = type,
                               series_names = factor(levels(data_train$series),
                                                     levels = levels(data_train$series)),
                               train_observations = series_obs,
                               train_times = unique(data_train$index..time..index),
                               test_observations = series_test,
                               test_times = unique(data_test$index..time..index),
                               hindcasts = series_hcs,
                               forecasts = series_fcs),
                          class = 'mvgam_forecast')
  return(series_fcs)
}

#'Compute forecasts using a posterior distribution
#'@noRd
forecast_draws = function(object,
                          type = 'response',
                          series = 'all',
                          data_test,
                          n_cores = 1,
                          n_samples,
                          ending_time,
                          b_uncertainty = TRUE,
                          trend_uncertainty = TRUE,
                          obs_uncertainty = TRUE){

  # Check arguments
  validate_pos_integer(n_cores)
  if(inherits(object, 'jsdgam')){
    orig_trend_model <- attr(object$model_data, 'prepped_trend_model')
  } else {
    orig_trend_model <- object$trend_model
  }
  data_test <- validate_series_time(data_test, name = 'newdata',
                                    trend_model = orig_trend_model)
  data_test <- sort_data(data_test)
  n_series <- NCOL(object$ytimes)
  use_lv <- object$use_lv

  if(series != 'all'){
    s_name <- levels(data_test$series)[series]
  }

  # Generate the observation model linear predictor matrix,
  # ensuring the test data is sorted correctly (by time and then series)
  if(inherits(data_test, 'list')){
    Xp <- obs_Xp_matrix(newdata = sort_data(data_test),
                        mgcv_model = object$mgcv_model)

    if(series != 'all'){
      obs_keep <- data.frame(y = data_test$y,
                             series = data_test$series,
                             time = data_test$index..time..index,
                             rowid = 1:length(data_test$y)) %>%
        dplyr::filter(series == s_name) %>%
        dplyr::arrange(time) %>%
        dplyr::pull(rowid)
      series_test <- data.frame(y = data_test$y,
                                series = data_test$series,
                                time = data_test$index..time..index,
                                rowid = 1:length(data_test$y)) %>%
        dplyr::filter(series == s_name) %>%
        dplyr::arrange(time)
      Xp <- Xp[obs_keep, ]
    } else {
      series_test <- NULL
    }
  } else {
    if(series != 'all'){
      series_test <- data_test %>%
        dplyr::filter(series == s_name) %>%
        dplyr::arrange(index..time..index)
      Xp <- obs_Xp_matrix(newdata = series_test,
                          mgcv_model = object$mgcv_model)
    } else {
      Xp <- obs_Xp_matrix(newdata = sort_data(data_test),
                          mgcv_model = object$mgcv_model)
      series_test <- NULL
    }
  }

  # Generate linear predictor matrix from trend mgcv model, ensuring
  # the test data is sorted correctly (by time and then series)
  if(!is.null(object$trend_call)){
    Xp_trend <- trend_Xp_matrix(newdata = sort_data(data_test),
                                trend_map = object$trend_map,
                                series = series,
                                mgcv_model = object$trend_mgcv_model,
                                forecast = TRUE)

    # For trend_formula models with autoregressive processes,
    # the process model operates as: AR * (process[t - 1] - mu[t-1]])
    # We therefore need the values of mu at the end of the training set
    # to correctly propagate the process model forward
    if(use_lv & attr(object$model_data, 'trend_model') != 'GP'){
      # Get the observed trend predictor matrix
      newdata <- trend_map_data_prep(object$obs_data,
                                     object$trend_map,
                                     forecast = TRUE)
      Xp_trend_last <- predict(object$trend_mgcv_model,
                               newdata = newdata,
                               type = 'lpmatrix')

      # Ensure the last three values are used, in case the obs_data
      # was not supplied in order
      data.frame(time = newdata$index..time..index,
                 series = newdata$series,
                 row_id = 1:length(newdata$index..time..index)) %>%
        dplyr::arrange(time, series) %>%
        dplyr::pull(row_id) -> sorted_inds
      n_processes <- length(unique(object$trend_map$trend))
      linpred_order <- tail(sorted_inds, 3 * n_processes)

      # Deal with any offsets
      if(!all(attr(Xp_trend_last, 'model.offset') == 0)){
        offset_vec <- attr(Xp_trend_last, 'model.offset')
        offset_last <- offset_vec[linpred_order]
        offset_last[is.na(offset_last)] <- 0
        full_offset <- c(offset_last, attr(Xp_trend, 'model.offset'))
      } else {
        full_offset <- 0
      }

      # Bind the last 3 linpred rows with the forecast linpred rows
      Xp_trend <- rbind(Xp_trend_last[linpred_order, , drop = FALSE],
                        Xp_trend)
      attr(Xp_trend, 'model.offset') <- full_offset
    }

  } else {
    Xp_trend <- NULL
  }

  # No need to compute in parallel if there was no trend model
  nmix_notrend <- FALSE
  if(!inherits(orig_trend_model, 'mvgam_trend') &
     object$family == 'nmix'){
    nmix_notrend <- TRUE
  }
  if(attr(object$model_data, 'trend_model') == 'None' |
     nmix_notrend){
    if(type == 'trend' & !nmix_notrend & !use_lv){
      stop('No trend_model was used in this model',
           call. = FALSE)
    }

    all_preds <- predict(object, type = type, newdata = data_test,
                         summary = FALSE)
    fc_preds <- lapply(seq_len(NROW(all_preds)), function(draw){
      lapply(seq_len(n_series), function(series){
        all_preds[draw, which(data_test$series == levels(data_test$series)[series])]
      })
    })

  } else {
    # Else compute forecasts including dynamic trend components

    # Set forecast horizon
    if(series != 'all'){
      fc_horizon <- NROW(series_test)
    } else {
      fc_horizon <- length(unique(data_test$index..time..index))
    }

    # Beta coefficients for GAM observation component
    betas <- mcmc_chains(object$model_output, 'b')

    # Generate sample sequence for n_samples
    if(missing(n_samples)){
      sample_seq <- 1:dim(betas)[1]
    } else {
      if(n_samples < dim(betas)[1]){
        sample_seq <- sample(seq_len(dim(betas)[1]),
                             size = n_samples, replace = FALSE)
      } else {
        sample_seq <- sample(seq_len(dim(betas)[1]),
                             size = n_samples, replace = TRUE)
      }
    }

    # Beta coefficients for GAM trend component
    if(!is.null(object$trend_call)){
      betas_trend <- mcmc_chains(object$model_output, 'b_trend')
    } else {
      betas_trend <- NULL
    }

    # Family of model
    family <- object$family

    # Family-specific parameters
    family_pars <- extract_family_pars(object = object, newdata = data_test)

    # Add trial information if this is a Binomial model
    if(object$family %in% c('binomial', 'beta_binomial')){
      resp_terms <- as.character(terms(formula(object$call))[[2]])
      resp_terms <- resp_terms[-grepl('cbind', resp_terms)]
      trial_name <- resp_terms[2]

      if(!exists(trial_name, data_test)){
        stop(paste0('Variable ', trial_name, ' not found in newdata'),
             call. = FALSE)
      }

      trial_df <- data.frame(series = data_test$series,
                             time = data_test$index..time..index,
                             trial = data_test[[trial_name]])
      trials <- matrix(NA, nrow = fc_horizon, ncol = n_series)
      for(i in 1:n_series){
        trials[,i] <- trial_df %>%
          dplyr::filter(series == levels(data_test$series)[i]) %>%
          dplyr::arrange(time) %>%
          dplyr::pull(trial)
      }
    } else {
      trials <- NULL
    }

    # Trend model
    trend_model <- attr(object$model_data, 'trend_model')

    # Calculate time_dis if this is a CAR1 model
    if(trend_model == 'CAR1'){
      data_test$index..time..index <- data_test$index..time..index +
        max(object$obs_data$index..time..index)
      time_dis <- add_corcar(model_data = list(),
                             data_train = object$obs_data,
                             data_test = data_test)[[1]]
      time_dis <- time_dis[-c(1:max(object$obs_data$index..time..index)),]
    } else {
      time_dis <- NULL
    }

    # Trend-specific parameters
    if(missing(ending_time)){
      trend_pars <- extract_trend_pars(object = object,
                                       keep_all_estimates = FALSE)
    } else {
      trend_pars <- extract_trend_pars(object = object,
                                       keep_all_estimates = FALSE,
                                       ending_time = ending_time)
    }

    # Any model in which an autoregressive process was included should be
    # considered as VAR1 for forecasting purposes as this will make use of the
    # faster c++ functions
    if(trend_model == 'CAR1'){
      if(!'last_lvs' %in% names(trend_pars)){
        trend_pars$last_lvs <- trend_pars$last_trends
      }
    } else {
      if('Sigma' %in% names(trend_pars) |
         'sigma' %in% names(trend_pars) |
         'tau' %in% names(trend_pars)){
        trend_model <- 'VAR1'
        if(!'last_lvs' %in% names(trend_pars)){
          trend_pars$last_lvs <- trend_pars$last_trends
        }
      }
    }

    # Loop over draws and compute forecasts (in serial at the moment)
    fc_preds <- lapply(seq_len(dim(betas)[1]), function(i){
      # Sample index
      samp_index <- i

      # Sample beta coefs
      if(b_uncertainty){
        betas <- betas[samp_index, ]
      } else {
        betas <- betas[1, ]
      }

      if(!is.null(betas_trend)){
        if(b_uncertainty){
          betas_trend <- betas_trend[samp_index, ]
        } else {
          betas_trend <- betas_trend[1, ]
        }
      }

      # Return predictions
      # Sample general trend-specific parameters
      if(trend_uncertainty){
        general_trend_pars <- extract_general_trend_pars(trend_pars = trend_pars,
                                                         samp_index = samp_index)
      } else {
        general_trend_pars <- extract_general_trend_pars(trend_pars = trend_pars,
                                                         samp_index = 1)
      }

      if(use_lv || trend_model %in% c('VAR1', 'PWlinear', 'PWlogistic', 'CAR1')){
        if(trend_model == 'PWlogistic'){
          if(!(exists('cap', where = data_test))) {
            stop('Capacities must also be supplied in "newdata" for logistic growth predictions',
                 call. = FALSE)
          }
          family_links <- eval(parse(text = family))
          if(family_links()$family == 'Gamma'){
            family_links <- Gamma(link = 'log')
          }
          cap <- data.frame(series = data_test$series,
                            time = data_test$index..time..index,
                            cap = suppressWarnings(linkfun(data_test$cap,
                                                           link = family_links()$link)))

          if(any(is.na(cap$cap)) | any(is.infinite(cap$cap))){
            stop(paste0('Missing or infinite values found for some "cap" terms\n',
                        'after transforming to the ',
                        family$link, ' link scale'),
                 call. = FALSE)
          }

        } else {
          cap <- NULL
        }

        # Propagate all trends / lvs forward jointly using sampled trend parameters
        trends <- forecast_trend(trend_model = trend_model,
                                 use_lv = use_lv,
                                 trend_pars = general_trend_pars,
                                 h = fc_horizon,
                                 betas_trend = betas_trend,
                                 Xp_trend = Xp_trend,
                                 time = unique(data_test$index..time..index -
                                                 min(object$obs_data$index..time..index) + 1),
                                 cap = cap,
                                 time_dis = time_dis)
      }

      # Loop across series and produce the next trend estimate
      trend_states <- lapply(seq_len(n_series), function(series){

        # Sample series- and trend-specific parameters
        trend_extracts <- extract_series_trend_pars(series = series,
                                                    samp_index = samp_index,
                                                    trend_pars = trend_pars,
                                                    use_lv = use_lv)

        if(use_lv || trend_model %in% c('VAR1', 'PWlinear', 'PWlogistic', 'CAR1')){
          if(use_lv){
            # Multiply lv states with loadings to generate the series' forecast trend state
            out <- as.numeric(trends %*% trend_extracts$lv_coefs)
          } else if(trend_model %in% c('VAR1', 'PWlinear', 'PWlogistic', 'CAR1')){
            out <- trends[,series]
          }

        } else {
          # Propagate the series-specific trends forward
          out <- forecast_trend(trend_model = trend_model,
                                use_lv = FALSE,
                                trend_pars = trend_extracts,
                                h = fc_horizon,
                                betas_trend = betas_trend,
                                Xp_trend = Xp_trend,
                                time = sort(unique(data_test$index..time..index)),
                                time_dis = NULL)
        }
        out
      })

      if(type == 'trend'){
        out <- trend_states
      } else {
        trend_states <- do.call(cbind, trend_states)
        out <- lapply(seq_len(n_series), function(series){

          if(family == 'nmix'){
            Xpmat <- Xp[which(as.numeric(data_test$series) == series),]
            latent_lambdas <- exp(trend_states[, series])
            pred_betas <- betas
            cap <- data_test$cap[which(as.numeric(data_test$series) == series)]
          } else {
            Xpmat <- cbind(Xp[which(as.numeric(data_test$series) == series),],
                           trend_states[, series])
            latent_lambdas <- NULL
            pred_betas <- c(betas, 1)
            cap <- NULL
          }

          if(!is.null(attr(Xp, 'model.offset'))){
            attr(Xpmat, 'model.offset') <-
              attr(Xp, 'model.offset')[which(as.numeric(data_test$series) == series)]

            attr(Xpmat, 'model.offset')[is.na(attr(Xpmat, 'model.offset'))] <- 0
          }

          # Family-specific parameters
          family_extracts <- lapply(seq_along(family_pars), function(x){
            if(is.matrix(family_pars[[x]])){
              if(obs_uncertainty){
                family_pars[[x]][samp_index, series]
              } else {
                family_pars[[x]][1, series]
              }
            } else {
              if(obs_uncertainty){
                family_pars[[x]][samp_index]
              } else {
                family_pars[[x]][1]
              }
            }
          })
          names(family_extracts) <- names(family_pars)

          # Add trial information if this is a Binomial model
          if(family %in% c('binomial', 'beta_binomial')){
            family_extracts$trials <- trials[,series]
          }

          mvgam_predict(family = family,
                        Xp = Xpmat,
                        latent_lambdas = latent_lambdas,
                        cap = cap,
                        type = type,
                        betas = pred_betas,
                        family_pars = family_extracts)
        })
      }
      out
    })
  }

  return(fc_preds)
}
