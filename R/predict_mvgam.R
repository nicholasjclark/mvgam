#'Plot mvjagam posterior predictions for a specified series
#'@param object \code{list} object returned from \code{mvjagam}
#'@param series \code{integer} specifying which series in the set is to be plotted
#'@param newdata A \code{dataframe} or \code{list} of test data containing at least 'series', 'season' and 'year'
#'for prediction, in addition to any other variables included in the linear predictor of \code{formula}
#'@param type When this has the value \code{link} (default) the linear predictor is calculated on the log link scale.
#'When \code{response} is used, the predictions take uncertainty in the obsevation process into account to return
#'predictions on the outcome (discrete) scale. Note that for both sets of predictions, the temporal
#'dynamics of any fitted latent trends will be ignored but the precisions of latent trends / factors will
#'be used to give more realistic estimates of uncertainty surrounding predictions. In essence, the predictions are what
#'the model would expect to see if the latent trends were all centred at \code{zero}
#'@details Posterior predictions are calculated from the fitted \code{mvjagam} object.
#'@return A \code{matrix} of dimension \code{n_samples x new_obs}, where \code{n_samples} is the number of
#'posterior samples from the fitted object and \code{n_obs} is the number of test observations in \code{newdata}
#'in which the \code{series} variable matches the suppled \code{series} argument
#'@export
predict_mvgam = function(object, series = 1, newdata, type = 'link'){

  family <- match.arg(arg = type, choices = c("link", "response"))

  # Generate linear predictor matrix from the mgcv component
  Xp <- predict(object$mgcv_model,
                newdata = newdata,
                type = 'lpmatrix')

  # Beta coefficients for GAM component
  betas <- MCMCvis::MCMCchains(object$jags_output, 'b')

  # Negative binomial size estimate
  sizes <- MCMCvis::MCMCchains(object$jags_output, 'r')

  # Latent trend precisions and loadings
  if(object$use_lv){
    taus <- MCMCvis::MCMCchains(object$jags_output, 'penalty')

    n_series <- NCOL(object$ytimes)
    n_lv <- object$n_lv
    lv_coefs <- lapply(seq_len(n_series), function(series){
      lv_indices <- seq(1, n_series * n_lv, by = n_series) + (series - 1)
      as.matrix(MCMCvis::MCMCchains(object$jags_output, 'lv_coefs')[,lv_indices])
    })
  } else {
    taus <- MCMCvis::MCMCchains(object$jags_output, 'tau')
  }

  # Loop across all posterior samples and calculate predictions on the outcome scale
  predictions <- do.call(rbind, lapply(seq_len(dim(betas)[1]), function(x){

    if(object$use_lv){
      lv_preds <- do.call(cbind, lapply(seq_len(object$n_lv), function(lv){
        rnorm(length(newdata$series), 0, sqrt(1 / taus[x,lv]))
      }))

      if(type == 'link'){
        as.vector(((Xp[which(as.numeric(newdata$series) == series),] %*% betas[x,])) +
                           ( lv_preds %*% lv_coefs[[series]][x,]))
      } else {
        rnbinom(n = length(newdata$series), size = sizes[x, series],
                mu = exp(((Xp[which(as.numeric(newdata$series) == series),] %*% betas[x,])) +
                           ( lv_preds %*% lv_coefs[[series]][x,])))
      }

    } else {
      if(type == 'link'){
        as.vector(((Xp[which(as.numeric(newdata$series) == series),] %*% betas[x,])) +
                           (rnorm(length(newdata$series), 0, sqrt(1 / taus[x,series]))))
      } else {
        rnbinom(n = length(newdata$series), size = sizes[x, series],
                mu = exp(((Xp[which(as.numeric(newdata$series) == series),] %*% betas[x,])) +
                           (rnorm(length(newdata$series), 0, sqrt(1 / taus[x,series])))))
      }

    }

  }))

  return(predictions)
}
