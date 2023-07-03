#'Predict from the GAM component of an mvgam model
#'@importFrom parallel clusterExport stopCluster setDefaultCluster
#'@importFrom stats predict
#'@param object \code{list} object returned from \code{mvgam}
#'@param newdata Optional \code{dataframe} or \code{list} of test data containing the
#'variables included in the linear predictor of \code{formula}. If not supplied,
#'predictions are generated for the original observations used for the model fit.
#'@param data_test Deprecated. Still works in place of \code{newdata} but users are recommended to use
#'\code{newdata} instead for more seamless integration into `R` workflows
#'@param n_cores \code{integer} specifying number of cores for generating predictions in parallel
#'@param type When this has the value \code{link} (default) the linear predictor is calculated on the link scale.
#'If \code{expected} is used, predictions reflect the expectation of the response (the mean)
#'but ignore uncertainty in the observation process. When \code{response} is used, the predictions take uncertainty in the observation process into account to return
#'predictions on the outcome scale
#'@param ... Ignored
#'@details Note that for all types of predictions, expected uncertainty in the process model is
#'accounted for only by using draws from the trend  SD parameters. If a `trend_formula`
#'was supplied in the model, predictions for this component will
#'also be incorporated. But the trend predictions will ignore autocorrelation
#'coefficients or GP length scale coefficients, ultimately assuming the process is stationary.
#'This method is similar to the types of posterior predictions returned from `brms` models
#'when using autocorrelated error predictions for newdata.
#'This function is therefore more suited to posterior simulation from the GAM components
#'of a \code{mvgam} model, while the forecasting functions
#'\code{\link{plot_mvgam_fc}} and \code{\link{forecast.mvgam}} are better suited to generate h-step ahead forecasts
#'that respect the temporal dynamics of estimated latent trends.
#'@return A \code{matrix} of dimension \code{n_samples x new_obs}, where \code{n_samples} is the number of
#'posterior samples from the fitted object and \code{n_obs} is the number of test observations in \code{newdata}
#'@export
predict.mvgam = function(object, newdata, data_test, type = 'link',
                         n_cores = 1, ...){

  # Argument checks
  if(!missing("data_test")){
    newdata <- data_test
  }
  if(missing(newdata)){
    newdata <- object$obs_data
  }
  validate_pos_integer(n_cores)
  type <- match.arg(arg = type, choices = c("link",
                                            "expected",
                                            "response"))

  # If a linear predictor was supplied for the latent process models, calculate
  # predictions by assuming the trend is stationary (this is basically what brms)
  # does when predicting for autocor() models
  if(!is.null(object$trend_call)){

    # Linear predictor matrix for the latent process models
    Xp <- trend_Xp_matrix(newdata = data_test,
                          trend_map = object$trend_map,
                          series = 'all',
                          mgcv_model = object$trend_mgcv_model)

    # Extract process error estimates
    if(object$trend_model %in% c('RW','AR1','AR2','AR3')){
      family_pars <- list(sigma_obs = mcmc_chains(object$model_output,
                                                  'sigma'))
    }

    # Indicators of which trend to use for each observation
    newdata %>%
      dplyr::left_join(object$trend_map,
                       by = 'series') -> newdata_trend
    trend_ind <- as.numeric(newdata_trend$trend)

    # Beta coefficients for GAM process model component
    betas <- mcmc_chains(object$model_output, 'b_trend')

    # Loop across all posterior samples and calculate
    # trend predictions
    cl <- parallel::makePSOCKcluster(n_cores)
    setDefaultCluster(cl)
    clusterExport(NULL, c('betas',
                          'family_pars',
                          'Xp',
                          'trend_ind'),
                  envir = environment())

    pbapply::pboptions(type = "none")
    trend_predictions <- do.call(rbind, pbapply::pblapply(seq_len(dim(betas)[1]),
                                                          function(x){

                                                            # Trend-specific SD parameters
                                                            par_extracts <- lapply(seq_along(family_pars), function(j){
                                                              if(is.matrix(family_pars[[j]])){
                                                                family_pars[[j]][x, trend_ind]
                                                              } else {
                                                                family_pars[[j]][x]
                                                              }
                                                            })
                                                            names(par_extracts) <- names(family_pars)
                                                            mvgam_predict(family = 'gaussian',
                                                                          Xp = Xp,
                                                                          type = 'response',
                                                                          betas = betas[x,],
                                                                          family_pars = par_extracts)
                                                          }, cl = cl))
    stopCluster(cl)

  } else if(object$trend_model != 'None'){
    # If no linear predictor for the trends but a dynamic trend model was used,
    # simulate from stationary time series for the trends

    n_draws <- dim(mcmc_chains(object$model_output, 'b'))[1]
    series_ind <- as.numeric(newdata$series)

    # Draw from fixed sigma for latent variable models
    if(object$use_lv & is.null(object$trend_map)){
      if(object$trend_model != 'GP'){
        trends <- array(rnorm(n_draws * object$n_lv * NROW(newdata),
                              mean = 0,
                              sd = 0.1),
                        dim = c(n_draws, object$n_lv, NROW(newdata)))
      } else {
        trends <- array(rnorm(n_draws * object$n_lv * NROW(newdata),
                              mean = 0,
                              sd = 0.25),
                        dim = c(n_draws, object$n_lv, NROW(newdata)))
      }

      lv_coefs <- mcmc_chains(object$model_output, 'lv_coefs')

      trend_predictions <- matrix(NA, nrow = n_draws,
                                  ncol = NROW(newdata))
      for(i in 1:n_draws){
        for(x in 1:NROW(newdata)){
          trend_predictions[i, x] <- t(trends[i,,series_ind[x]]) %*%
            matrix(lv_coefs[i,], nrow = object$n_lv)[,series_ind[x]]
        }
      }
    }

    if(!object$use_lv){

      if(object$trend_model %in% c('RW','AR1','AR2','AR3','VAR1')){
        family_pars <- list(sigma_obs = mcmc_chains(object$model_output,
                                                    'sigma'))
      }
      if(object$trend_model %in% c('GP')){
        family_pars <- list(sigma_obs = mcmc_chains(object$model_output,
                                                    'alpha_gp'))
      }

      # Indicators of which trend to use for each observation
      if(!is.null(object$trend_map)){
        newdata %>%
          dplyr::left_join(object$trend_map,
                           by = 'series') -> newdata_trend
        trend_ind <- as.numeric(newdata_trend$trend)
      } else {
        trend_ind <- as.numeric(newdata$series)
      }

      # Create a fake design matrix of 1s
      betas <- matrix(0,
                      ncol = 1,
                      nrow = dim(mcmc_chains(object$model_output, 'b'))[1])
      Xp <- matrix(1, ncol = 1, nrow = NROW(newdata))
      attr(Xp, 'model.offset') <- 0

      # Loop across all posterior samples and calculate
      # trend predictions
      cl <- parallel::makePSOCKcluster(n_cores)
      setDefaultCluster(cl)
      clusterExport(NULL, c('betas',
                            'family_pars',
                            'Xp',
                            'trend_ind'),
                    envir = environment())

      pbapply::pboptions(type = "none")
      trend_predictions <- do.call(rbind,
                                   pbapply::pblapply(seq_len(dim(betas)[1]),
                                                     function(x){

                                                       # Trend-specific SD parameters
                                                       par_extracts <- lapply(seq_along(family_pars), function(j){
                                                         if(is.matrix(family_pars[[j]])){
                                                           family_pars[[j]][x, trend_ind]
                                                         } else {
                                                           family_pars[[j]][x]
                                                         }
                                                       })
                                                       names(par_extracts) <- names(family_pars)
                                                       mvgam_predict(family = 'gaussian',
                                                                     Xp = Xp,
                                                                     type = 'response',
                                                                     betas = betas[x,],
                                                                     family_pars = par_extracts)
                                                     }, cl = cl))
      stopCluster(cl)
    }
  } else {
    trend_predictions <- NULL
  }

  #### Once trend predictions are made, calculate observation predictions ####
  # Generate linear predictor matrix from the mgcv observation model
  Xp <- obs_Xp_matrix(newdata = newdata,
                      mgcv_model = object$mgcv_model)

  # Beta coefficients for GAM component
  betas <- mcmc_chains(object$model_output, 'b')

  # Family of model
  family <- object$family

  # Family-specific parameters
  family_pars <- extract_family_pars(object = object)

  # Determine which series each observation belongs to
  series_ind <- as.numeric(newdata$series)

  # Loop across all posterior samples and calculate observation model
  # predictions
  cl <- parallel::makePSOCKcluster(n_cores)
  setDefaultCluster(cl)

  trend_model <- object$trend_model

  clusterExport(NULL, c('betas',
                        'family_pars',
                        'trend_model',
                        'family',
                        'trend_predictions',
                        'type',
                        'Xp',
                        'series_ind'),
                envir = environment())

  pbapply::pboptions(type = "none")
  predictions <- do.call(rbind, pbapply::pblapply(seq_len(dim(betas)[1]), function(x){

    # Family-specific parameters
    par_extracts <- lapply(seq_along(family_pars), function(j){
      if(is.matrix(family_pars[[j]])){
        family_pars[[j]][x, series_ind]
      } else {
        family_pars[[j]][x]
      }
    })
    names(par_extracts) <- names(family_pars)

    # Set up Xp matrix to include the latent process predictions
    # if necessary
    if(trend_model != 'None'){
      Xpmat <- cbind(as.matrix(Xp),
                     trend_predictions[x,])
      attr(Xpmat, 'model.offset') <- attr(Xp, 'model.offset')
      betas_pred <- c(betas[x,], 1)
    } else {
      Xpmat <- Xp
      betas_pred <- betas[x, ]
    }

    # Calculate predictions
    mvgam_predict(family = family,
                  Xp = Xpmat,
                  type = type,
                  betas = betas_pred,
                  family_pars = par_extracts)
  }, cl = cl))
  stopCluster(cl)

  return(predictions)
}

