#'Predict from the GAM component of an mvgam model
#'@importFrom stats predict
#'@inheritParams brms::fitted.brmsfit
#'@param object \code{list} object returned from \code{mvgam}. See [mvgam()]
#'@param newdata Optional \code{dataframe} or \code{list} of test data containing the
#'variables included in the linear predictor of \code{formula}. If not supplied,
#'predictions are generated for the original observations used for the model fit.
#'@param data_test Deprecated. Still works in place of \code{newdata} but users are recommended to use
#'\code{newdata} instead for more seamless integration into `R` workflows
#'@param type When this has the value \code{link} (default) the linear predictor is
#'calculated on the link scale.
#'If \code{expected} is used, predictions reflect the expectation of the response (the mean)
#'but ignore uncertainty in the observation process. When \code{response} is used,
#'the predictions take uncertainty in the observation process into account to return
#'predictions on the outcome scale. When \code{variance} is used, the variance of the response
#'with respect to the mean (mean-variance relationship) is returned.
#'When `type = "terms"`, each component of the linear predictor is
#'returned separately in the form of a `list` (possibly with standard
#'errors, if `summary = TRUE`): this includes parametric model components,
#'followed by each smooth component, but excludes any offset and any intercept.
#'Two special cases are also allowed:
#' type `latent_N` will return the estimated latent abundances from an
#' N-mixture distribution, while type `detection` will return the estimated
#' detection probability from an N-mixture distribution
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
#' @return Predicted values on the appropriate scale.
#'   If \code{summary = FALSE} and `type != "terms"`,
#'   the output is a matrix of dimension `n_draw x n_observations`
#'   containing predicted values for each posterior draw in `object`.
#'
#'   If \code{summary = TRUE} and `type != "terms"`, the output is an \code{n_observations} x \code{E}
#'   matrix. The number of summary statistics \code{E} is equal to \code{2 +
#'   length(probs)}: The \code{Estimate} column contains point estimates (either
#'   mean or median depending on argument \code{robust}), while the
#'   \code{Est.Error} column contains uncertainty estimates (either standard
#'   deviation or median absolute deviation depending on argument
#'   \code{robust}). The remaining columns starting with \code{Q} contain
#'   quantile estimates as specified via argument \code{probs}.
#'
#'   If `type = "terms"` and `summary = FALSE`, the output is a named `list`
#'   containing a separate slot for each effect, with the effects returned as
#'   matrices of dimension `n_draw x 1`. If `summary = TRUE`, the output resembles that
#'   from \code{\link[mgcv]{predict.gam}} when using the call
#'   `predict.gam(object, type = "terms", se.fit = TRUE)`, where mean contributions
#'   from each effect are returned in `matrix` form while standard errors (representing
#'   the interval: `(max(probs) - min(probs)) / 2`) are returned in a separate `matrix`
#'@examples
#'\dontrun{
#'# Simulate 4 time series with hierarchical seasonality
#'# and independent AR1 dynamic processes
#'set.seed(111)
#'simdat <- sim_mvgam(seasonality = 'hierarchical',
#'                    trend_model = 'AR1',
#'                    family = gaussian())
#'
#'# Fit a model with shared seasonality
#'mod1 <- mvgam(y ~ s(season, bs = 'cc', k = 6),
#'              data = simdat$data_train,
#'              family = gaussian(),
#'              trend_model = AR(),
#'              burnin = 300,
#'              samples = 300,
#'              chains = 2)
#'
#'# Generate predictions against observed data
#'preds <- predict(mod1, summary = TRUE)
#'head(preds)
#'
#'# Generate predictions against test data
#'preds <- predict(mod1, newdata = simdat$data_test, summary = TRUE)
#'head(preds)
#'}
#'@export
predict.mvgam = function(object,
                         newdata,
                         data_test,
                         type = 'link',
                         process_error = TRUE,
                         summary = TRUE,
                         robust = FALSE,
                         probs = c(0.025, 0.975),
                         ...){

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
                                            "variance",
                                            "latent_N",
                                            "detection",
                                            "terms"))

  if(type == 'latent_N' & object$family != 'nmix'){
    stop('"latent_N" type only available for N-mixture models',
         call. = FALSE)
  }

  if(type == 'detection' & object$family != 'nmix'){
    stop('"detection" type only available for N-mixture models',
         call. = FALSE)
  }

  # terms is the easiest return type, so evaluate it first
  if(type == 'terms'){
    out <- list()
    out$obs_effects <- terms_preds(object = object,
                                   newdata = newdata,
                                   summary = summary,
                                   robust = robust,
                                   probs = probs,
                                   trend_effects = FALSE)
    if(!is.null(object$trend_call)){
      out$process_effects <- terms_preds(object = object,
                                         newdata = newdata,
                                         summary = summary,
                                         robust = robust,
                                         probs = probs,
                                         trend_effects = TRUE)
    }
  } else {


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
      if(attr(object$model_data, 'trend_model') %in% c('RW','AR1','AR2','AR3','CAR1')){
        if(object$family == 'nmix'){
          family_pars <- list(sigma_obs = .Machine$double.eps)
        } else {
          family_pars <- list(sigma_obs = mcmc_chains(object$model_output,
                                                      'sigma'))
        }
      }

      if(attr(object$model_data, 'trend_model') %in% c('VAR1')){
        if(object$use_lv){
          family_pars <- list(sigma_obs =
                                mcmc_chains(object$model_output, 'Sigma')[ ,
                                                                           seq(1, object$n_lv^2,
                                                                               by = object$n_lv+1)])
        } else {
          family_pars <- list(sigma_obs =
                                mcmc_chains(object$model_output, 'Sigma')[ ,
                                                                           seq(1, NCOL(object$ytimes)^2,
                                                                               by = NCOL(object$ytimes)+1)])
        }
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
      if(!process_error){
        family_extracts <- list(sigma_obs = .Machine$double.eps)
      }
      trend_predictions <- mvgam_predict(family = 'gaussian',
                                         Xp = all_linpreds,
                                         type = 'response',
                                         betas = 1,
                                         family_pars = family_extracts)

    } else if(attr(object$model_data, 'trend_model') != 'None' & process_error){
      # If no linear predictor for the trends but a dynamic trend model was used,
      # and the process_error flag is set to TRUE,
      # simulate from stationary time series to capture uncertainty
      # in the dynamic trend component

      n_draws <- dim(mcmc_chains(object$model_output, 'b'))[1]
      series_ind <- as.numeric(newdata$series)

      # Draw from fixed sigma for latent variable models
      if(object$use_lv & is.null(object$trend_map)){
        if(attr(object$model_data, 'trend_model') != 'GP'){
          trends <- array(rnorm(n_draws * object$n_lv * length(newdata[[1]]),
                                mean = 0,
                                sd = 0.1),
                          dim = c(n_draws, object$n_lv, length(newdata[[1]])))
        } else {
          trends <- array(rnorm(n_draws * object$n_lv * length(newdata[[1]]),
                                mean = 0,
                                sd = 0.25),
                          dim = c(n_draws, object$n_lv, length(newdata[[1]])))
        }

        lv_coefs <- mcmc_chains(object$model_output, 'lv_coefs')

        trend_predictions <- matrix(NA, nrow = n_draws,
                                    ncol = length(newdata[[1]]))
        for(i in 1:n_draws){
          for(x in 1:length(newdata[[1]])){
            trend_predictions[i, x] <- t(trends[i,,series_ind[x]]) %*%
              matrix(lv_coefs[i,], nrow = object$n_lv)[,series_ind[x]]
          }
        }
        trend_predictions <- as.vector(trend_predictions)

      }

      if(!object$use_lv){

        if(attr(object$model_data, 'trend_model') %in%
           c('RW','AR1','AR2','AR3','VAR1','CAR1')){
          family_pars <- list(sigma_obs = mcmc_chains(object$model_output,
                                                      'sigma'))
        }
        if(attr(object$model_data, 'trend_model') %in% c('GP')){
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
        Xp <- matrix(1, ncol = 1, nrow = length(newdata$time))
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

    # Family parameters spread into long vectors
    family_extracts <- lapply(seq_along(family_pars), function(j){
      if(is.matrix(family_pars[[j]])){
        as.vector(family_pars[[j]][, series_ind])
      } else {
        as.vector(matrix(rep(family_pars[[j]],
                             NROW(Xp)),
                         nrow = NROW(betas),
                         byrow = FALSE))
      }
    })
    names(family_extracts) <- names(family_pars)

    # Add trial information if this is a Binomial model
    if(object$family %in% c('binomial', 'beta_binomial')){
      resp_terms <- as.character(terms(formula(object))[[2]])
      resp_terms <- resp_terms[-grepl('cbind', resp_terms)]
      trial_name <- resp_terms[2]
      if(!trial_name %in% names(newdata)){
        stop(paste0('variable ', trial_name, ' not found in newdata'),
             call. = FALSE)
      }

      trials <- newdata[[trial_name]]
      trials <- as.vector(matrix(rep(as.vector(trials),
                                     NROW(betas)),
                                 nrow = NROW(betas),
                                 byrow = TRUE))
      family_extracts$trials <- trials
    }

    # Pre-multiply the linear predictors, including any offset and trend
    # predictions if applicable
    if(family == 'nmix'){
      all_linpreds <- as.matrix(as.vector(t(apply(as.matrix(betas), 1,
                                                  function(row) Xp %*% row +
                                                    attr(Xp, 'model.offset')))))
      latent_lambdas <- exp(trend_predictions)

      if(!(exists('cap', where = newdata))) {
        stop('Max abundances must be supplied as a variable named "cap" for N-mixture models',
             call. = FALSE)
      }

      validate_pos_integers(newdata$cap)

      if(any(is.na(newdata$cap)) | any(is.infinite(newdata$cap))){
        stop(paste0('Missing or infinite values found for some "cap" terms'),
             call. = FALSE)
      }
      cap <- as.vector(t(replicate(NROW(betas), newdata$cap)))

    } else {
      all_linpreds <- as.matrix(as.vector(t(apply(as.matrix(betas), 1,
                                                  function(row) Xp %*% row +
                                                    attr(Xp, 'model.offset')))) +
                                  trend_predictions)
      latent_lambdas <- NULL
      cap <- NULL
    }

    attr(all_linpreds, 'model.offset') <- 0

    # Calculate vectorized predictions
    predictions_vec <- mvgam_predict(family = family,
                                     Xp = all_linpreds,
                                     latent_lambdas = latent_lambdas,
                                     cap = cap,
                                     type = type,
                                     betas = 1,
                                     family_pars = family_extracts)

    # Convert back to matrix
    preds <- matrix(predictions_vec, nrow = NROW(betas))

    if(summary){
      Qupper <- apply(preds, 2, quantile, probs = max(probs), na.rm = TRUE)
      Qlower <- apply(preds, 2, quantile, probs = min(probs), na.rm = TRUE)

      if(robust){
        estimates <- apply(preds, 2, median, na.rm = TRUE)
        errors <- apply(abs(preds - estimates), 2, median, na.rm = TRUE)
      } else {
        estimates <- apply(preds, 2, mean, na.rm = TRUE)
        errors <- apply(preds, 2, sd, na.rm = TRUE)
      }

      out <- cbind(estimates, errors, Qlower, Qupper)
      colnames(out) <- c('Estimate', 'Est.Error', paste0('Q', 100*min(probs)),
                         paste0('Q', 100*max(probs)))
    } else {
      out <- preds
    }
  }
  return(out)
}

#' Term-specific predictions and uncertainties
#' @noRd
terms_preds = function(object, newdata, summary = TRUE,
                       robust = FALSE, probs = c(0.025, 0.975),
                       trend_effects = FALSE){

  if(trend_effects){
    Xp <- trend_Xp_matrix(newdata = newdata,
                          trend_map = object$trend_map,
                          series = 'all',
                          mgcv_model = object$trend_mgcv_model)
    betas <- mcmc_chains(object$model_output, 'b_trend')
    effect_names <- colnames(predict(object$trend_mgcv_model,
                                         type = 'terms',
                                         se.fit = FALSE))
    effect_names <- gsub('series', 'trend',
                         effect_names, fixed = TRUE)
    coef_names <- names(coef(object$trend_mgcv_model))
    coef_names <- gsub('series', 'trend',
                       coef_names, fixed = TRUE)
  } else {
    Xp <- obs_Xp_matrix(newdata = newdata,
                        mgcv_model = object$mgcv_model)
    betas <- mcmc_chains(object$model_output, 'b')
    effect_names <- colnames(predict(object$mgcv_model,
                                         type = 'terms',
                                         se.fit = FALSE))
    coef_names <- names(coef(object$trend_mgcv_model))
  }

  # Contributions considering full uncertainties
  contributions <- serrors <- vector(mode = 'list',
                                     length = length(effect_names))
  for(i in seq_along(effect_names)){
    effect_idxs <- grep(effect_names[i],
                        coef_names, fixed = TRUE)
    linpred <- as.matrix(as.vector(t(apply(as.matrix(betas[, effect_idxs, drop = FALSE]), 1,
                                           function(row) Xp[, effect_idxs, drop = FALSE]
                                           %*% row))))
    contributions[[i]] <- matrix(linpred, nrow = NROW(betas))

    if(summary){
      serrors[[i]] <- (apply(contributions[[i]], 2,
                             function(x) quantile(x, probs = max(probs))) -
                         apply(contributions[[i]], 2,
                               function(x) quantile(x, probs = min(probs)))) / 2
      if(robust){
        contributions[[i]] <- apply(contributions[[i]], 2, median)
      } else {
        contributions[[i]] <- apply(contributions[[i]], 2, mean)
      }
    }
  }
  if(summary){
    out <- list()
    contributions <- do.call(cbind, contributions)
    serrors <- do.call(cbind, serrors)
    colnames(contributions) <- colnames(serrors) <- effect_names
    out$fit <- contributions
    out$se.fit <- serrors
  } else {
    names(contributions) <- effect_names
    out <- contributions
  }
  return(out)
}
