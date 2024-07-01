#' Draws from the Expected Value of the Posterior Predictive Distribution
#'
#' Compute posterior draws of the expected value of the posterior predictive
#' distribution (i.e. the conditional expectation).
#' Can be performed for the data used to fit the model (posterior
#' predictive checks) or for new data. By definition, these predictions have
#' smaller variance than the posterior predictions performed by the
#' \code{\link{posterior_predict.mvgam}} method. This is because only the
#' uncertainty in the expected value of the posterior predictive distribution is
#' incorporated in the draws computed by \code{posterior_epred} while the
#' residual error is ignored there. However, the estimated means of both methods
#' averaged across draws should be very similar.
#' @importFrom rstantools posterior_epred
#' @inheritParams predict.mvgam
#' @param ndraws Positive `integer` indicating how many posterior draws should be used.
#' If `NULL` (the default) all draws are used.
#' @param process_error Logical. If \code{TRUE} and \code{newdata} is supplied,
#' expected uncertainty in the process model is accounted for by using draws
#' from any latent trend SD parameters. If \code{FALSE}, uncertainty in the latent
#' trend component is ignored when calculating predictions. If no \code{newdata} is
#' supplied, draws from the fitted model's posterior predictive distribution will be used
#' (which will always include uncertainty in any latent trend components)
#' @method posterior_epred mvgam
#' @details Note that for all types of predictions for models that did not include
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
#' @return A \code{matrix} of dimension \code{n_samples x new_obs},
#' where \code{n_samples} is the number of posterior samples from the fitted object
#' and \code{n_obs} is the number of observations in \code{newdata}
#' @seealso \code{\link{hindcast.mvgam}} \code{\link{posterior_linpred.mvgam}} \code{\link{posterior_predict.mvgam}}
#' @aliases posterior_epred
#' @examples
#' \donttest{
#' # Simulate some data and fit a model
#' simdat <- sim_mvgam(n_series = 1, trend_model = 'AR1')
#' mod <- mvgam(y ~ s(season, bs = 'cc'),
#'             trend_model = AR(),
#'             noncentred = TRUE,
#'             data = simdat$data_train)
#'
#'# Compute posterior expectations
#'expectations <- posterior_epred(mod)
#'str(expectations)
#'}
#' @export
posterior_epred.mvgam = function(object,
                                 newdata,
                                 data_test,
                                 ndraws = NULL,
                                 process_error = TRUE, ...){

  if(missing(newdata) & missing(data_test)){
    out <- .mvgam_fitted(object, type = 'expected')
  } else {
    out <- predict(object,
                   newdata = newdata,
                   data_test = data_test,
                   process_error = process_error,
                   type = 'expected',
                   summary = FALSE)
  }

  if(!is.null(ndraws)){
    validate_pos_integer(ndraws)
    if(ndraws > NROW(out)){
    } else {
      idx <- sample(1:NROW(out), ndraws, replace = FALSE)
      out <- out[idx, ]
    }
  }
  return(out)
}

#' Posterior Draws of the Linear Predictor
#'
#' Compute posterior draws of the linear predictor, that is draws before
#' applying any link functions or other transformations. Can be performed for
#' the data used to fit the model (posterior predictive checks) or for new data.
#' @importFrom rstantools posterior_linpred
#' @inheritParams posterior_epred.mvgam
#' @param transform Logical; if \code{FALSE}
#'  (the default), draws of the linear predictor are returned.
#'  If \code{TRUE}, draws of the transformed linear predictor,
#'  i.e. the conditional expectation, are returned.
#'
#' @seealso \code{\link{posterior_epred.mvgam}} \code{\link{posterior_predict.mvgam}}
#' @method posterior_linpred mvgam
#' @details Note that for all types of predictions for models that did not include
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
#' @return A \code{matrix} of dimension \code{n_samples x new_obs},
#' where \code{n_samples} is the number of posterior samples from the fitted object
#' and \code{n_obs} is the number of observations in \code{newdata}
#' @seealso \code{\link{hindcast.mvgam}} \code{\link{posterior_epred.mvgam}} \code{\link{posterior_predict.mvgam}}
#' @examples
#' \donttest{
#' # Simulate some data and fit a model
#' simdat <- sim_mvgam(n_series = 1, trend_model = 'AR1')
#' mod <- mvgam(y ~ s(season, bs = 'cc'),
#'              trend_model = AR(),
#'              noncentred = TRUE,
#'              data = simdat$data_train,
#'              chains = 2)
#'
#'# Extract linear predictor values
#'linpreds <- posterior_linpred(mod)
#'str(linpreds)
#'}
#' @export
posterior_linpred.mvgam = function(object,
                                   transform = FALSE,
                                   newdata,
                                   ndraws = NULL,
                                   data_test,
                                   process_error = TRUE,
                                   ...){

  if(transform){
    type <- 'expected'
  } else {
    type <- 'link'
  }

  out <- predict(object,
                 newdata = newdata,
                 data_test = data_test,
                 process_error = process_error,
                 type = type,
                 summary = FALSE)

  if(!is.null(ndraws)){
    validate_pos_integer(ndraws)
    if(ndraws > NROW(out)){
    } else {
      idx <- sample(1:NROW(out), ndraws, replace = FALSE)
      out <- out[idx, ]
    }
  }
  return(out)
}

#' Draws from the Posterior Predictive Distribution
#'
#' Compute posterior draws of the posterior predictive distribution. Can be
#' performed for the data used to fit the model (posterior predictive checks) or
#' for new data. By definition, these draws have higher variance than draws
#' of the expected value of the posterior predictive distribution computed by
#' \code{\link{posterior_epred.mvgam}}. This is because the residual error
#' is incorporated in \code{posterior_predict}. However, the estimated means of
#' both methods averaged across draws should be very similar.
#' @importFrom rstantools posterior_predict
#' @inheritParams predict.mvgam
#' @inheritParams posterior_epred.mvgam
#' @param process_error Logical. If \code{TRUE} and \code{newdata} is supplied,
#' expected uncertainty in the process model is accounted for by using draws
#' from any latent trend SD parameters. If \code{FALSE}, uncertainty in the latent
#' trend component is ignored when calculating predictions. If no \code{newdata} is
#' supplied, draws from the fitted model's posterior predictive distribution will be used
#' (which will always include uncertainty in any latent trend components)
#' @method posterior_predict mvgam
#' @details Note that for all types of predictions for models that did not include
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
#' @return A \code{matrix} of dimension \code{n_samples x new_obs},
#' where \code{n_samples} is the number of posterior samples from the fitted object
#' and \code{n_obs} is the number of observations in \code{newdata}
#' @seealso \code{\link{hindcast.mvgam}} \code{\link{posterior_linpred.mvgam}} \code{\link{posterior_epred.mvgam}}
#' @examples
#' \dontrun{
#' # Simulate some data and fit a model
#' simdat <- sim_mvgam(n_series = 1, trend_model = 'AR1')
#' mod <- mvgam(y ~ s(season, bs = 'cc'),
#'             trend_model = 'AR1',
#'             data = simdat$data_train)
#'
#'# Compute posterior predictions
#'predictions <- posterior_predict(mod)
#'str(predictions)
#'}
#' @export
posterior_predict.mvgam = function(object,
                                   newdata,
                                   data_test,
                                   ndraws = NULL,
                                   process_error = TRUE, ...){

  out <- predict(object,
                 newdata = newdata,
                 data_test = data_test,
                 process_error = process_error,
                 type = 'response',
                 summary = FALSE)

  if(!is.null(ndraws)){
    validate_pos_integer(ndraws)
    if(ndraws > NROW(out)){
    } else {
      idx <- sample(1:NROW(out), ndraws, replace = FALSE)
      out <- out[idx, ]
    }
  }
  return(out)
}

#' Expected Values of the Posterior Predictive Distribution
#'
#' This method extracts posterior estimates of the fitted values
#' (i.e. the actual predictions, included estimates for any trend states,
#' that were obtained when fitting the model). It also includes an option
#' for obtaining summaries of the computed draws.
#'
#' @inheritParams brms::fitted.brmsfit
#' @inheritParams predict.mvgam
#' @param object An object of class `mvgam`
#' @details This method gives the actual fitted values from the model (i.e. what you
#' will see if you generate hindcasts from the fitted model using \code{\link{hindcast.mvgam}}
#' with `type = 'expected'`). These
#' predictions can be overly precise if a flexible dynamic trend component was included
#' in the model. This is in contrast to the set of predict functions (i.e.
#' \code{\link{posterior_epred.mvgam}} or \code{\link{predict.mvgam}}), which will assume
#' any dynamic trend component has reached stationarity when returning hypothetical predictions
#' @return An \code{array} of predicted \emph{mean} response values.
#'   If \code{summary = FALSE} the output resembles those of
#'   \code{\link{posterior_epred.mvgam}} and \code{\link{predict.mvgam}}.
#'
#'   If \code{summary = TRUE} the output is an \code{n_observations} x \code{E}
#'   matrix. The number of summary statistics \code{E} is equal to \code{2 +
#'   length(probs)}: The \code{Estimate} column contains point estimates (either
#'   mean or median depending on argument \code{robust}), while the
#'   \code{Est.Error} column contains uncertainty estimates (either standard
#'   deviation or median absolute deviation depending on argument
#'   \code{robust}). The remaining columns starting with \code{Q} contain
#'   quantile estimates as specified via argument \code{probs}.
#' @seealso \code{\link{hindcast.mvgam}}
#' @examples
#' \dontrun{
#' # Simulate some data and fit a model
#' simdat <- sim_mvgam(n_series = 1, trend_model = 'AR1')
#' mod <- mvgam(y ~ s(season, bs = 'cc'),
#'             trend_model = 'AR1',
#'             data = simdat$data_train,
#'             chains = 2,
#'             burnin = 300,
#'             samples = 300)
#'
#'# Extract fitted values (posterior expectations)
#'expectations <- fitted(mod)
#'str(expectations)
#'}
#' @export
fitted.mvgam <- function(object, process_error = TRUE,
                        scale = c("response", "linear"),
                        summary = TRUE, robust = FALSE,
                        probs = c(0.025, 0.975), ...) {

  scale <- match.arg(scale)
  type <- switch(scale, "response" = "expected",
                  "linear" = "link")
  preds <- .mvgam_fitted(object = object, type = type)

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

  return(out)
}

#' @noRd
.mvgam_fitted = function(object, type = 'expected'){

    # Extract the linear predictor draws
    mus <- mcmc_chains(object$model_output, 'mus')

    # Need to know which series each observation belongs to so we can
    # pull out appropriate family-level parameters (overdispersions, shapes, etc...)
    if(is.null(object$test_data)){
      all_dat <- data.frame(series = object$obs_data$series,
                            time = object$obs_data$time,
                            y = object$obs_data$y) %>%
        dplyr::arrange(series, time)
    } else {
      all_dat <- data.frame(series = c(object$obs_data$series,
                                       object$test_data$series),
                            time = c(object$obs_data$time,
                                     object$test_data$time),
                            y = c(object$obs_data$y,
                                  object$test_data$y)) %>%
        dplyr::arrange(series, time)
    }

    obs <- all_dat$y
    series_obs <- as.numeric(all_dat$series)

    # Family-specific parameters
    family <- object$family
    family_pars <- extract_family_pars(object = object)
    n_series <- NCOL(object$ytimes)

    # Family parameters spread into a vector
    family_extracts <- lapply(seq_along(family_pars), function(j){
      if(is.matrix(family_pars[[j]])){
        as.vector(family_pars[[j]][, series_obs])
      } else {
        family_pars[[j]][]
      }
    })
    names(family_extracts) <- names(family_pars)

    # Add trial information if this is a Binomial model
    if(object$family %in% c('binomial', 'beta_binomial')){
      trials <- as.vector(matrix(rep(as.vector(attr(object$mgcv_model, 'trials')),
                                     NROW(mus)),
                                 nrow = NROW(mus),
                                 byrow = TRUE))
      family_extracts$trials <- trials
    }

    # Expectations as a vector
    Xp <- as.matrix(as.vector(mus))
    attr(Xp, 'model.offset') <- 0

    if(family == 'nmix'){
      latent_lambdas <- exp(as.vector(mcmc_chains(object$model_output, 'trend')))
      n_draws <- dim(mcmc_chains(object$model_output, 'ypred'))[1]
      cap <- as.vector(t(replicate(n_draws, object$obs_data$cap)))
    } else {
      latent_lambdas <- NULL
      cap <- NULL
    }
    pred_vec <- mvgam_predict(family = family,
                              family_pars = family_extracts,
                              latent_lambdas = latent_lambdas,
                              cap = cap,
                              type = type,
                              Xp = Xp,
                              betas = 1)

    # Convert back to matrix and return
    pred_mat <- matrix(pred_vec, nrow = NROW(mus))
    return(pred_mat)
  }
