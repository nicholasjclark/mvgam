#'Predict from the GAM component of an mvgam model
#'@importFrom stats predict
#'@param object Object of class `mvgam`
#'@param newdata Optional \code{dataframe} or \code{list} of test data containing the
#'variables included in the linear predictor of \code{formula}. If not supplied,
#'predictions are generated for the original observations used for the model fit.
#'@param data_test Deprecated. Still works in place of \code{newdata} but users are recommended to use
#'\code{newdata} instead for more seamless integration into `R` workflows
#'@param type When this has the value \code{link} (default) the linear predictor is calculated on the link scale.
#'If \code{expected} is used, predictions reflect the expectation of the response (the mean)
#'but ignore uncertainty in the observation process. When \code{response} is used,
#'the predictions take uncertainty in the observation process into account to return
#'predictions on the outcome scale. When \code{variance} is used, the variance of the response
#'with respect to the mean (mean-variance relationship) is returned
#'@param process_error Logical. If \code{TRUE} and a dynamic trend model was fit,
#'expected uncertainty in the process model is accounted for by using draws
#'from the latent trend SD parameters. If \code{FALSE}, uncertainty in the latent trend
#'component is ignored when calculating predictions
#'@param ... Ignored
#'@details Note that for all types of predictions for models that did not include
#'a `trend_formula`, uncertainty in the dynamic trend
#'component can be ignored by setting \code{process_error = FALSE}. However,
#'if a `trend_formula` was supplied in the model, predictions for this component cannot be
#'ignored. If \code{process_error = TRUE}, trend predictions will ignore autocorrelation
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
predict.mvgam = function(object, newdata,
                         data_test,
                         type = 'link',
                         process_error = TRUE, ...){

  # Argument checks
  if(!missing("data_test")){
    newdata <- data_test
  }
  if(missing(newdata)){
    newdata <- object$obs_data
  }

  # newdata needs to have a 'series' indicator in it for integrating
  # over the trend uncertainties
  if(!'series' %in% names(newdata)){
    newdata$series <- factor(rep(levels(object$obs_data$series)[1],
                                 length(newdata[[1]])),
                             levels = levels(object$obs_data$series))
  }

  type <- match.arg(arg = type, choices = c("link",
                                            "expected",
                                            "response",
                                            "variance"))

  # If a linear predictor was supplied for the latent process models, calculate
  # predictions by assuming the trend is stationary (this is basically what brms)
  # does when predicting for autocor() models
  if(!is.null(object$trend_call)){

    # Linear predictor matrix for the latent process models
    Xp <- trend_Xp_matrix(newdata = newdata,
                          trend_map = object$trend_map,
                          series = 'all',
                          mgcv_model = object$trend_mgcv_model)

    # Extract process error estimates
    if(object$trend_model %in% c('RW','AR1','AR2','AR3')){
      family_pars <- list(sigma_obs = mcmc_chains(object$model_output,
                                                  'sigma'))
    }

    if(object$trend_model %in% c('VAR1')){
      family_pars <- list(sigma_obs =
                            mcmc_chains(object$model_output, 'Sigma')[ ,
                                                                       seq(1, NCOL(object$ytimes)^2,
                                                                           by = NCOL(object$ytimes)+1)])
    }

    # Indicators of which trend to use for each observation
    if(inherits(newdata, 'list')){
      data.frame(series = newdata$series) %>%
        dplyr::left_join(object$trend_map,
                         by = 'series') %>%
        dplyr::pull(trend) -> trend_inds
      newdata_trend <- newdata
      newdata_trend$trend <- trend_inds
    } else {
      newdata %>%
        dplyr::left_join(object$trend_map,
                         by = 'series') -> newdata_trend
    }
    trend_ind <- as.numeric(newdata_trend$trend)

    # Beta coefficients for GAM process model component
    betas <- mcmc_chains(object$model_output, 'b_trend')

    # Family parameters spread into a vector
    family_extracts <- lapply(seq_along(family_pars), function(j){
      if(is.matrix(family_pars[[j]])){
        as.vector(family_pars[[j]][, trend_ind])
      } else {
        family_pars[[j]]
      }
    })
    names(family_extracts) <- names(family_pars)

    # Pre-multiply the linear predictors
    all_linpreds <- as.matrix(as.vector(t(apply(as.matrix(betas), 1,
                                                function(row) Xp %*% row +
                                                  attr(Xp, 'model.offset')))))
    attr(all_linpreds, 'model.offset') <- 0

    # Trend stationary predictions
    trend_predictions <- mvgam_predict(family = 'gaussian',
                                       Xp = all_linpreds,
                                       type = 'response',
                                       betas = 1,
                                       family_pars = family_extracts)

  } else if(object$trend_model != 'None' & process_error){
    # If no linear predictor for the trends but a dynamic trend model was used,
    # and the process_error flag is set to TRUE,
    # simulate from stationary time series to capture uncertainty
    # in the dynamic trend component

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

      # Family parameters spread into a vector
      family_extracts <- lapply(seq_along(family_pars), function(j){
        if(is.matrix(family_pars[[j]])){
          as.vector(family_pars[[j]][, trend_ind])
        } else {
          family_pars[[j]]
        }
      })
      names(family_extracts) <- names(family_pars)

      # Pre-multiply the linear predictors
      all_linpreds <- as.matrix(as.vector(t(apply(as.matrix(betas), 1,
                                                  function(row) Xp %*% row +
                                                    attr(Xp, 'model.offset')))))
      attr(all_linpreds, 'model.offset') <- 0

      # Trend stationary predictions
      trend_predictions <- mvgam_predict(family = 'gaussian',
                                         Xp = all_linpreds,
                                         type = 'response',
                                         betas = 1,
                                         family_pars = family_extracts)

    }
  } else {
    # If no trend_model was used, or if process_error == FALSE,
    # ignore uncertainty in any latent trend component
    trend_predictions <- 0
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

  # Family parameters spread into a vector
  family_extracts <- lapply(seq_along(family_pars), function(j){
    if(is.matrix(family_pars[[j]])){
      as.vector(family_pars[[j]][, series_ind])
    } else {
      family_pars[[j]]
    }
  })
  names(family_extracts) <- names(family_pars)

  # Pre-multiply the linear predictors, including any offset and trend
  # predictions if applicable
  all_linpreds <- as.matrix(as.vector(t(apply(as.matrix(betas), 1,
                                              function(row) Xp %*% row +
                                                attr(Xp, 'model.offset')))) +
                              trend_predictions)
  attr(all_linpreds, 'model.offset') <- 0

  # Calculate vectorized predictions
  predictions_vec <- mvgam_predict(family = family,
                                   Xp = all_linpreds,
                                   type = type,
                                   betas = 1,
                                   family_pars = family_extracts)

  # Convert back to matrix
  predictions <- matrix(predictions_vec, nrow = NROW(betas))
  return(predictions)
}

