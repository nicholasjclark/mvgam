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
#'@param series \code{integer} specifying which series in the set is to be forecast
#'@param n_cores \code{integer} specifying number of cores for generating forecasts in parallel
#'@param type When this has the value \code{link}, the linear predictor is calculated on the log link scale.
#'When \code{response} is used, the predictions take uncertainty in the observation process into account to return
#'predictions on the outcome (discrete) scale (default). When \code{trend} is used, only the forecast distribution for the
#'latent trend is returned.
#'@details Posterior predictions are drawn from the fitted \code{mvgam} and used to simulate a forecast distribution
#'@return A \code{matrix} of the forecast distribution
#'
NULL
#'@export
forecast <- function(x, what, ...){
  UseMethod("forecast")
}

#'@rdname forecast.mvgam
#'@method forecast mvgam
#'@export
forecast.mvgam = function(object, newdata, data_test, series = 1,
                          n_cores = 1,
                          type = 'response'){
  # Check arguments
  if(class(object) != 'mvgam'){
    stop('argument "object" must be of class "mvgam"')
  }

  if(sign(series) != 1){
    stop('argument "series" must be a positive integer',
         call. = FALSE)
  } else {
    if(series%%1 != 0){
      stop('argument "series" must be a positive integer',
           call. = FALSE)
    }
  }

  if(!missing("newdata")){
    data_test <- newdata
  }

  # Ensure outcome is labelled 'y' when feeding data to the model for simplicity
  if(terms(formula(object$call))[[2]] != 'y'){
    data_test$y <- data_test[[terms(formula(object$call))[[2]]]]
  }

  type <- match.arg(arg = type, choices = c("link", "response", "trend"))
  data_train <- object$obs_data

  # Add variables to data_test if missing
  s_name <- levels(data_train$series)[series]
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
    series_test <- data_test %>%
      dplyr::filter(series == s_name) %>%
      dplyr::arrange(time)
    Xp <- predict(object$mgcv_model,
                  newdata = series_test,
                  type = 'lpmatrix')
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
  trend_pars <- mvgam:::extract_trend_pars(object = object,
                                           keep_all_estimates = FALSE)

  # Produce forecasts
  use_lv <- object$use_lv
  cl <- parallel::makePSOCKcluster(n_cores)
  setDefaultCluster(cl)
  clusterExport(NULL, c('family',
                        'family_pars',
                        'trend_model',
                        'trend_pars',
                        'use_lv',
                        'betas',
                        'series',
                        'series_test',
                        'Xp'),
                envir = environment())

  pbapply::pboptions(type = "none")

  fc_preds <- do.call(rbind, pbapply::pblapply(seq_len(dim(betas)[1]), function(i){
    # Sample index
    samp_index <- i

    # Sample beta coefs
    betas <- betas[samp_index, ]

    # Sample family-specific parameters
    family_extracts <- lapply(seq_along(family_pars), function(x){
      if(is.matrix(family_pars[[x]])){
        family_pars[[x]][samp_index, series]
      } else {
        family_pars[[x]][samp_index]
      }
    })
    names(family_extracts) <- names(family_pars)

    # Sample series- and trend-specific parameters
    trend_extracts <- mvgam:::extract_series_trend_pars(series = series,
                                                        samp_index = samp_index,
                                                        trend_pars = trend_pars,
                                                        use_lv = use_lv)

    # Propagate the series' trend forward using the sampled trend parameters
    trends <- mvgam:::forecast_trend(trend_model = trend_model,
                                     use_lv = use_lv,
                                     trend_pars = trend_extracts,
                                     h = NROW(series_test))

    if(trend_model == 'VAR1'){
      trends <- trends[,series]
    }

    if(use_lv){
      # Multiply lv states with loadings to generate the series' forecast trend state
      trends <- as.numeric(trends %*% trend_extracts$lv_coefs)
    }

    # Return predictions
    if(type == 'trend'){
      out <- trends
    } else {
      Xpmat <- cbind(Xp, trends)
      attr(Xpmat, 'model.offset') <- attr(Xp, 'model.offset')
      out <- mvgam:::mvgam_predict(family = family,
                           Xp = Xpmat,
                           type = type,
                           betas = c(betas, 1),
                           family_pars = family_extracts)
    }

    out
  }, cl = cl))
  stopCluster(cl)

  return(fc_preds)
}
