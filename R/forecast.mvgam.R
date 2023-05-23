#'@title Compute out of sample forecasts for a fitted \code{mvgam} object
#'@name forecast.mvgam
#'@param object \code{list} object returned from \code{mvgam}
#'@param newdata Optional \code{dataframe} or \code{list} of test data containing at least 'series' and 'time'
#'in addition to any other variables included in the linear predictor of the original \code{formula}. If included, the
#'covariate information in \code{newdata} will be used to generate forecasts from the fitted model equations. If
#'this same \code{newdata} was originally included in the call to \code{mvgam}, then forecasts have already been
#'produced by the generative model and these will simply be extracted and plotted. However if no \code{newdata} was
#'supplied to the original model call, an assumption is made that the \code{newdata} supplied here comes sequentially
#'after the data supplied as \code{data} in the original model (i.e. we assume there is no time gap between the last
#'observation of series 1 in \code{data} and the first observation for series 1 in \code{newdata}).
#'@param data_test Deprecated. Still works in place of \code{newdata} but users are recommended to use
#'\code{newdata} instead for more seamless integration into `R` workflows
#'@param series Either a \code{integer} specifying which series in the set is to be forecast,
#'or the character string \code{'all'}, specifying that all series should be forecast. This is preferable
#'if the fitted model contained multivariate trends (either as a dynamic factor or \code{VAR} process),
#'as it saves recomputing the full set of trends for each series individually
#'@param n_cores \code{integer} specifying number of cores for generating forecasts in parallel
#'@param type When this has the value \code{link}, the linear predictor is calculated on the log link scale.
#'When \code{response} is used, the predictions take uncertainty in the observation process into account to return
#'predictions on the outcome (discrete) scale (default). When \code{trend} is used, only the forecast distribution for the
#'latent trend is returned.
#'@details Posterior predictions are drawn from the fitted \code{mvgam} and used to simulate a forecast distribution
#'@return An object of class \code{mvgam_forecast} containing hindcast and forecast distributions
#'for the required series
#'
NULL
#'@export
forecast <- function(x, what, ...){
  UseMethod("forecast")
}

#'@rdname forecast.mvgam
#'@method forecast mvgam
#'@export
forecast.mvgam = function(object, newdata, data_test, series = 'all',
                          n_cores = 1,
                          type = 'response'){
  # Check arguments
  if(class(object) != 'mvgam'){
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

  if(!missing("newdata")){
    data_test <- newdata
  }

  if(missing("newdata") & missing(data_test) & is.null(object$test_data)){
    stop('newdata must be supplied to compute forecasts',
         call. = FALSE)
  }

  type <- match.arg(arg = type, choices = c("link", "response", "trend"))
  data_train <- object$obs_data

  if(series != 'all'){
    s_name <- levels(data_train$series)[series]
  }
  n_series <- NCOL(object$ytimes)

  # Check whether a forecast has already been computed
  forecasts_exist <- FALSE
  if(!is.null(object$test_data) && !missing(data_test)){
    if(max(data_test$time) <= max(object$test_data$time)){
      forecasts_exist <- TRUE
    } else {
      data_test %>%
        dplyr::filter(time > max(object$test_data$time)) -> data_test
    }
  }

  if(!is.null(object$test_data) && missing(data_test)){
    forecasts_exist <- TRUE
  }

  if(is.null(object$test_data)){
    data_test %>%
      dplyr::filter(time > max(object$obs_data$time)) -> data_test
  }

  # Only compute forecasts if they don't already exist!
  if(!forecasts_exist){

  # Ensure outcome is labelled 'y' when feeding data to the model for simplicity
  if(terms(formula(object$call))[[2]] != 'y'){
    data_test$y <- data_test[[terms(formula(object$call))[[2]]]]
  }

  if(!missing(data_test)){

    if(!'y' %in% names(data_test)){
      data_test$y <- rep(NA, NROW(data_test))
    }

    if(class(data_test)[1] == 'list'){
      if(!'time' %in% names(data_test)){
        stop('data_test does not contain a "time" column')
      }

      if(!'series' %in% names(data_test)){
        data_test$series <- factor('series1')
      }

    } else {
      if(!'time' %in% colnames(data_test)){
        stop('data_test does not contain a "time" column')
      }

      if(!'series' %in% colnames(data_test)){
        data_test$series <- factor('series1')
      }
    }
  }

  # Generate the linear predictor matrix
  if(class(data_test)[1] == 'list'){
    suppressWarnings(Xp  <- try(predict(object$mgcv_model,
                                             newdata = data_test,
                                             type = 'lpmatrix'),
                                     silent = TRUE))

    if(inherits(Xp, 'try-error')){
      testdat <- data.frame(time = data_test$time)

      terms_include <- names(object$mgcv_model$coefficients)[which(!names(object$mgcv_model$coefficients)
                                                                   %in% '(Intercept)')]
      if(length(terms_include) > 0){
        newnames <- vector()
        newnames[1] <- 'time'
        for(i in 1:length(terms_include)){
          testdat <- cbind(testdat, data.frame(data_test[[terms_include[i]]]))
          newnames[i+1] <- terms_include[i]
        }
        colnames(testdat) <- newnames
      }

      suppressWarnings(Xp  <- predict(object$mgcv_model,
                                           newdata = testdat,
                                           type = 'lpmatrix'))
    }

    if(series != 'all'){
      obs_keep <- data.frame(y = data_test$y,
                             series = data_test$series,
                             time = data_test$time,
                             rowid = 1:length(data_test$y)) %>%
        dplyr::filter(series == s_name) %>%
        dplyr::arrange(time) %>%
        dplyr::pull(rowid)
      series_test <- data.frame(y = data_test$y,
                                series = data_test$series,
                                time = data_test$time,
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
        dplyr::arrange(time)
      Xp <- predict(object$mgcv_model,
                    newdata = series_test,
                    type = 'lpmatrix')
    } else {
      Xp <- predict(object$mgcv_model,
                    newdata = data_test,
                    type = 'lpmatrix')
      series_test <- NULL
    }

  }

  if(series != 'all'){
    fc_horizon <- NROW(series_test)
  } else {
    fc_horizon <- length(unique(data_test$time))
  }

  # Beta coefficients for GAM component
  betas <- mvgam:::mcmc_chains(object$model_output, 'b')

  # Family of model
  family <- object$family

  # Family-specific parameters
  family_pars <- mvgam:::extract_family_pars(object = object)

  # Trend model
  trend_model <- object$trend_model
  use_lv <- object$use_lv

  # Trend-specific parameters; keep only the last 3 estimates for RW / AR trends
  # as we don't need any prior to that for propagating the trends forward. For GP
  # trends, we have to keep all estimates through time

  if(object$trend_model == 'VAR1'){
    trend_pars <- mvgam:::extract_trend_pars(object = object,
                                             keep_all_estimates = FALSE,
                                             ending_time = max(object$obs_data$time))
    all_trends <- mvgam:::propagate_var_fcs(trend_pars,
                                    n_samples = 500,
                                    sample_seq = 1:500,
                                    eval_timepoint = max(object$obs_data$time),
                                    n_series = n_series,
                                    fc_horizon = fc_horizon)
    sample_seq <- sample(1:500, dim(betas)[1], TRUE)
  } else {
    trend_pars <- mvgam:::extract_trend_pars(object = object,
                                             keep_all_estimates = FALSE)
    all_trends <- NULL
  }

  # Produce forecasts
  use_lv <- object$use_lv
  cl <- parallel::makePSOCKcluster(n_cores)
  setDefaultCluster(cl)
  clusterExport(NULL, c('family',
                        'family_pars',
                        'trend_model',
                        'trend_pars',
                        'type',
                        'use_lv',
                        'betas',
                        'n_series',
                        'data_test',
                        'series',
                        'series_test',
                        'Xp',
                        'fc_horizon',
                        'all_trends',
                        'sample_seq'),
                envir = environment())

  pbapply::pboptions(type = "none")

  fc_preds <- pbapply::pblapply(seq_len(dim(betas)[1]), function(i){
    # Sample index
    samp_index <- i

    # Sample beta coefs
    betas <- betas[samp_index, ]

    # Return predictions
    if(series == 'all'){

      # Sample general trend-specific parameters
      general_trend_pars <- mvgam:::extract_general_trend_pars(trend_pars = trend_pars,
                                                               samp_index = samp_index)

      if(use_lv){
        # Propagate the lvs forward using the sampled trend parameters
        trends <- mvgam:::forecast_trend(trend_model = trend_model,
                                         use_lv = use_lv,
                                         trend_pars = general_trend_pars,
                                         h = fc_horizon)
      }

      # Loop across series and produce the next trend estimate
      trend_states <- lapply(seq_len(n_series), function(series){

        # Sample series- and trend-specific parameters
        trend_extracts <- mvgam:::extract_series_trend_pars(series = series,
                                                            samp_index = samp_index,
                                                            trend_pars = trend_pars,
                                                            use_lv = use_lv)

        if(use_lv || trend_model == 'VAR1'){
          if(use_lv){
            # Multiply lv states with loadings to generate the series' forecast trend state
            out <- as.numeric(trends %*% trend_extracts$lv_coefs)
          }

          if(trend_model == 'VAR1'){
            out <- all_trends[sample_seq[i], , series]
          }

        } else {
          # Propagate the series-specific trends forward
          out <- mvgam:::forecast_trend(trend_model = trend_model,
                                        use_lv = FALSE,
                                        trend_pars = trend_extracts,
                                        h = fc_horizon)
        }
        out
      })

      if(type == 'trend'){
        out <- trend_states
      } else {
        trend_states <- do.call(cbind, trend_states)
        out <- lapply(seq_len(n_series), function(series){

          Xpmat <- cbind(Xp[which(as.numeric(data_test$series) == series),],
                         trend_states[, series])
          attr(Xpmat, 'model.offset') <- attr(Xp, 'model.offset')

          # Family-specific parameters
          family_extracts <- lapply(seq_along(family_pars), function(x){
            if(is.matrix(family_pars[[x]])){
              family_pars[[x]][samp_index, series]
            } else {
              family_pars[[x]][samp_index]
            }
          })
          names(family_extracts) <- names(family_pars)

          mvgam:::mvgam_predict(family = family,
                                Xp = Xpmat,
                                type = 'response',
                                betas = c(betas, 1),
                                family_pars = family_extracts)
        })
      }

    } else {

      if(trend_model == 'VAR1'){
        trends <- all_trends[sample_seq[i], , series]
      } else {
        # Sample series- and trend-specific parameters
        trend_extracts <- mvgam:::extract_series_trend_pars(series = series,
                                                            samp_index = samp_index,
                                                            trend_pars = trend_pars,
                                                            use_lv = use_lv)

        # Propagate the series' trend forward using the sampled trend parameters
        trends <- mvgam:::forecast_trend(trend_model = trend_model,
                                         use_lv = use_lv,
                                         trend_pars = trend_extracts,
                                         h = fc_horizon)

        if(use_lv){
          # Multiply lv states with loadings to generate the series' forecast trend state
          trends <- as.numeric(trends %*% trend_extracts$lv_coefs)
        }
      }

      if(type == 'trend'){
        out <- trends
      } else {

        # Sample the series' family-specific parameters
        family_extracts <- lapply(seq_along(family_pars), function(x){
          if(is.matrix(family_pars[[x]])){
            family_pars[[x]][samp_index, series]
          } else {
            family_pars[[x]][samp_index]
          }
        })
        names(family_extracts) <- names(family_pars)

        # Generate predictions
        Xpmat <- cbind(Xp, trends)
        attr(Xpmat, 'model.offset') <- attr(Xp, 'model.offset')
        out <- mvgam:::mvgam_predict(family = family,
                                     Xp = Xpmat,
                                     type = type,
                                     betas = c(betas, 1),
                                     family_pars = family_extracts)
      }
    }

    out
  }, cl = cl)
  stopCluster(cl)

  if(series == 'all'){
    series_fcs <- lapply(seq_len(n_series), function(series){
      indexed_forecasts <- do.call(rbind, lapply(seq_along(fc_preds), function(x){
        fc_preds[[x]][[series]]
      }))
      indexed_forecasts
    })
    names(series_fcs) <- levels(data_test$series)

    # Extract hindcasts for storing in the returned object
    data_train <- object$obs_data
    ends <- seq(0, dim(mcmc_chains(object$model_output, 'ypred'))[2],
                length.out = NCOL(object$ytimes) + 1)
    starts <- ends + 1
    starts <- c(1, starts[-c(1, (NCOL(object$ytimes)+1))])
    ends <- ends[-1]

    series_hcs <- lapply(seq_len(n_series), function(series){
      to_extract <- ifelse(type == 'trend', 'trend', 'ypred')
    if(object$fit_engine == 'stan'){

      preds <- mcmc_chains(object$model_output, to_extract)[,seq(series,
                                                              dim(mcmc_chains(object$model_output, 'ypred'))[2],
                                                              by = NCOL(object$ytimes))]
    } else {
      preds <- mcmc_chains(object$model_output, to_extract)[,starts[series]:ends[series]]
    }
      preds
    })
    names(series_hcs) <- levels(data_test$series)

    series_obs <- lapply(seq_len(n_series), function(series){
      s_name <- levels(object$obs_data$series)[series]
      data.frame(series = object$obs_data$series,
                 time = object$obs_data$time,
                 y = object$obs_data$y) %>%
        dplyr::filter(series == s_name) %>%
        dplyr::arrange(time) %>%
        dplyr::pull(y)
    })
    names(series_obs) <- levels(data_test$series)

    series_test <- lapply(seq_len(n_series), function(series){
      s_name <- levels(object$obs_data$series)[series]
      data.frame(series = data_test$series,
                 time = data_test$time,
                 y = data_test$y) %>%
        dplyr::filter(series == s_name) %>%
        dplyr::arrange(time) %>%
        dplyr::pull(y)
    })
    names(series_test) <- levels(data_test$series)

  } else {
    series_fcs <- list(do.call(rbind, fc_preds))
    names(series_fcs) <- s_name

    # Extract hindcasts for storing in the returned object
    data_train <- object$obs_data
    ends <- seq(0, dim(mcmc_chains(object$model_output, 'ypred'))[2],
                length.out = NCOL(object$ytimes) + 1)
    starts <- ends + 1
    starts <- c(1, starts[-c(1, (NCOL(object$ytimes)+1))])
    ends <- ends[-1]
    to_extract <- ifelse(type == 'trend', 'trend', 'ypred')
    if(object$fit_engine == 'stan'){
      preds <- mcmc_chains(object$model_output, to_extract)[,seq(series,
                                                              dim(mcmc_chains(object$model_output, 'ypred'))[2],
                                                              by = NCOL(object$ytimes))]
    } else {
      preds <- mcmc_chains(object$model_output, to_extract)[,starts[series]:ends[series]]
    }
    series_hcs <- list(preds)
    names(series_hcs) <- s_name

    series_obs <- list(data.frame(series = object$obs_data$series,
               time = object$obs_data$time,
               y = object$obs_data$y) %>%
      dplyr::filter(series == s_name) %>%
      dplyr::arrange(time) %>%
      dplyr::pull(y))
    names(series_obs) <- s_name

    series_test <- list(data.frame(series = data_test$series,
                                  time = data_test$time,
                                  y = data_test$y) %>%
                         dplyr::filter(series == s_name) %>%
                         dplyr::arrange(time) %>%
                         dplyr::pull(y))
    names(series_test) <- s_name
  }

  } else {
    # If forecasts already exist, simply extract them
    last_train <- max(object$obs_data$time)

    if(series == 'all'){
      data_train <- object$obs_data
      ends <- seq(0, dim(mcmc_chains(object$model_output, 'ypred'))[2],
                  length.out = NCOL(object$ytimes) + 1)
      starts <- ends + 1
      starts <- c(1, starts[-c(1, (NCOL(object$ytimes)+1))])
      ends <- ends[-1]

      series_fcs <- lapply(seq_len(n_series), function(series){
        to_extract <- ifelse(type == 'trend', 'trend', 'ypred')
        if(object$fit_engine == 'stan'){

          preds <- mcmc_chains(object$model_output, to_extract)[,seq(series,
                                                                     dim(mcmc_chains(object$model_output, 'ypred'))[2],
                                                                     by = NCOL(object$ytimes))]
        } else {
          preds <- mcmc_chains(object$model_output, to_extract)[,starts[series]:ends[series]][,1:last_train]
        }
        preds[,(last_train+1):NCOL(preds)]
      })
      names(series_fcs) <- levels(data_train$series)

      # Extract hindcasts for storing in the returned object
      series_hcs <- lapply(seq_len(n_series), function(series){
        to_extract <- ifelse(type == 'trend', 'trend', 'ypred')
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

      series_test <- lapply(seq_len(n_series), function(series){
        s_name <- levels(object$obs_data$series)[series]
        data.frame(series = object$test_data$series,
                   time = object$test_data$time,
                   y = object$test_data$y) %>%
          dplyr::filter(series == s_name) %>%
          dplyr::arrange(time) %>%
          dplyr::pull(y)
      })
      names(series_test) <- levels(data_train$series)

    } else {
      data_train <- object$obs_data
      ends <- seq(0, dim(mcmc_chains(object$model_output, 'ypred'))[2],
                  length.out = NCOL(object$ytimes) + 1)
      starts <- ends + 1
      starts <- c(1, starts[-c(1, (NCOL(object$ytimes)+1))])
      ends <- ends[-1]
      to_extract <- ifelse(type == 'trend', 'trend', 'ypred')

      # Extract forecasts
      if(object$fit_engine == 'stan'){
        preds <- mcmc_chains(object$model_output, to_extract)[,seq(series,
                                                                   dim(mcmc_chains(object$model_output, 'ypred'))[2],
                                                                   by = NCOL(object$ytimes))]
      } else {
        preds <- mcmc_chains(object$model_output, to_extract)[,starts[series]:ends[series]]
      }
      series_fcs <- list(preds[,(last_train+1):NCOL(preds)])
      names(series_hcs) <- s_name

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

      # Testing observations
      series_test <- list(data.frame(series = object$test_data$series,
                                     time = object$test_data$time,
                                     y = object$test_data$y) %>%
                            dplyr::filter(series == s_name) %>%
                            dplyr::arrange(time) %>%
                            dplyr::pull(y))
      names(series_test) <- s_name
    }
  }

  series_fcs <- structure(list(call = object$call,
                               family = object$family,
                               trend_model = object$trend_model,
                               use_lv = object$use_lv,
                               fit_engine = object$fit_engine,
                               type = type,
                               series_names = levels(data_train$series),
                               train_observations = series_obs,
                               test_observations = series_test,
                               hindcasts = series_hcs,
                               forecasts = series_fcs),
                          class = 'mvgam_forecast')
  return(series_fcs)
}
