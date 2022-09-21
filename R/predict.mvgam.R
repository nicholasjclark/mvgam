#'Plot mvgam posterior predictions for a specified series
#'@param object \code{list} object returned from \code{mvgam}
#'@param series \code{integer} specifying which series in the set is to be predicted
#'@param newdata Optional \code{dataframe} or \code{list} of test data containing at least 'series' and 'time
#'for prediction, in addition to any other variables included in the linear predictor of \code{formula}. If not supplied,
#'predictions are generated for the original observations used for the model fit.
#'@param data_test Deprecated. Still works in place of \code{newdata} but users are recommended to use
#'\code{newdata} instead for more seamless integration into `R` workflows
#'@param n_cores \code{integer} specifying number of cores for generating predictions in parallel
#'@param type When this has the value \code{link} (default) the linear predictor is calculated on the log link scale.
#'When \code{response} is used, the predictions take uncertainty in the observation process into account to return
#'predictions on the outcome (discrete) scale.
#'@details Note that for both sets of predictions, the temporal
#'dynamics of any fitted latent trends will be ignored but the precisions of latent trends / factors, or the
#'covariance function if a Gaussian process trend was implemented in the model, will
#'be used to give more realistic estimates of uncertainty surrounding predictions. In essence, the predictions are what
#'the model would expect to see if the latent trends were all centred at \code{zero}. This function is therefore more
#'suited to posterior simulation for interrogating a model's estimated smooth functions, while the forecasting functions
#'\code{\link{plot_mvgam_fc}} and \code{\link{forecast.mvgam}} are better suited to generate h-step ahead forecasts
#'that respect the temporal dynamics of estimated latent trends.
#'@return A \code{matrix} of dimension \code{n_samples x new_obs}, where \code{n_samples} is the number of
#'posterior samples from the fitted object and \code{n_obs} is the number of test observations in \code{newdata}
#'in which the \code{series} variable matches the supplied \code{series} argument
#'@export
predict.mvgam = function(object, series = 1, newdata, data_test, type = 'link',
                         n_cores = 1){

  # Argument checks
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

  if(!missing("data_test")){
    newdata <- data_test
  }

  type <- match.arg(arg = type, choices = c("link", "response"))

  # Generate linear predictor matrix from the mgcv component
  if(missing(newdata)){
    newdata <- object$obs_data
  }

  s_name <- levels(object$obs_data$series)[series]

  # Filter the data so that only observations for the specified series are used
  pred_inds <- which(newdata$series == s_name)
  dat_names <- names(newdata)

  if(class(newdata)[1] == 'list'){
    newdata <- lapply(seq_along(newdata), function(x){
      if(is.matrix(newdata[[x]])){
        newdata[[x]][pred_inds,]
      } else {
        newdata[[x]][pred_inds]
      }
    })
  } else {
    newdata <- newdata[pred_inds, ] %>%
      dplyr::arrange(time)
  }
  names(newdata) <- dat_names

  # Generate the linear predictor matrix
  suppressWarnings(Xp  <- try(predict(object$mgcv_model,
                                      newdata = newdata,
                                      type = 'lpmatrix'),
                              silent = TRUE))

  if(inherits(Xp, 'try-error')){
    testdat <- data.frame(time = newdata$time)
    terms_include <- names(object$mgcv_model$coefficients)[which(!names(object$mgcv_model$coefficients)
                                                                 %in% '(Intercept)')]
    if(length(terms_include) > 0){
      newnames <- vector()
      newnames[1] <- 'time'
      for(i in 1:length(terms_include)){
        testdat <- cbind(testdat, data.frame(newdata[[terms_include[i]]]))
        newnames[i+1] <- terms_include[i]
      }
      colnames(testdat) <- newnames
    }

    suppressWarnings(Xp  <- predict(object$mgcv_model,
                                    newdata = testdat,
                                    type = 'lpmatrix'))
  }

  # Beta coefficients for GAM component
  betas <- MCMCvis::MCMCchains(object$model_output, 'b')

  # Family of model
  family <- object$family

  # Negative binomial size estimate
  if(family == 'Negative Binomial'){
    sizes <- MCMCvis::MCMCchains(object$model_output, 'r')
  } else {
    sizes <- NULL
  }

  # Tweedie parameters
  if(family == 'Tweedie'){
    twdiss <- MCMCvis::MCMCchains(object$model_output, 'twdis')
    ps <- matrix(1.5, nrow = NROW(betas), ncol = NCOL(object$ytimes))
  } else {
    twdiss <- NULL
    ps <- NULL
  }

  # Latent trend precisions and loadings
  if(object$use_lv){
    taus <- MCMCvis::MCMCchains(object$model_output, 'penalty')

    n_series <- NCOL(object$ytimes)
    n_lv <- object$n_lv
    lv_coefs <- lapply(seq_len(n_series), function(series){
      if(object$fit_engine == 'stan'){
        coef_start <- min(which(sort(rep(1:n_series, n_lv)) == series))
        coef_end <- coef_start + n_lv - 1
        as.matrix(MCMCvis::MCMCchains(object$model_output, 'lv_coefs')[,coef_start:coef_end])
      } else {
        lv_indices <- seq(1, n_series * n_lv, by = n_series) + (series - 1)
        as.matrix(MCMCvis::MCMCchains(object$model_output, 'lv_coefs')[,lv_indices])
      }

    })
  } else {
    if(object$trend_model %in% c('GP', 'None')){
      taus <- NULL
    } else {
      taus <- MCMCvis::MCMCchains(object$model_output, 'tau')
      # Prior sampling can sometimes result in NaN for tau if sigma
      # is sampled at zero
      taus[is.na(taus)] <- 0.001
    }
    lv_coefs <- NULL
  }

  if(object$trend_model == 'GP'){
    alpha_gps <- MCMCvis::MCMCchains(object$model_output, 'alpha_gp')
    rho_gps <- MCMCvis::MCMCchains(object$model_output, 'rho_gp')
  } else {
    alpha_gps <- NULL
    rho_gps <- NULL
  }
  # Loop across all posterior samples and calculate predictions on the outcome scale
  use_lv <- object$use_lv
  cl <- parallel::makePSOCKcluster(n_cores)
  setDefaultCluster(cl)
  clusterExport(NULL, c('use_lv',
                        'taus',
                        'alpha_gps',
                        'rho_gps',
                        'lv_coefs',
                        'betas',
                        'sizes',
                        'twdiss',
                        'ps',
                        'newdata',
                        'Xp'),
                envir = environment())

  pbapply::pboptions(type = "none")

  predictions <- do.call(rbind, pbapply::pblapply(seq_len(dim(betas)[1]), function(x){

    if(use_lv){
      lv_preds <- do.call(cbind, lapply(seq_len(n_lv), function(lv){
        rnorm(length(which(newdata$series == s_name)), 0, sqrt(1 / taus[x,lv]))
      }))

      if(type == 'link'){
        out <- as.vector(((matrix(Xp[which(newdata$series == s_name),], ncol = NCOL(Xp),
                                     nrow = length(which(newdata$series == s_name))) %*%
                             betas[x,])) +
                           ( lv_preds %*% lv_coefs[[series]][x,]))
      }

      if(type == 'response'){
        if(family == 'Negative Binomial'){
          out <- rnbinom(n = length(which(newdata$series == s_name)), size = sizes[x, series],
                  mu = exp(((matrix(Xp[which(newdata$series == s_name),], ncol = NCOL(Xp)) %*%
                               betas[x,])) +
                             ( lv_preds %*% lv_coefs[[series]][x,])))
        }

        if(family == 'Tweedie'){
          out <- rpois(n = length(which(newdata$series == s_name)),
                lambda = mgcv::rTweedie(
                  mu = exp(((matrix(Xp[which(newdata$series == s_name),], ncol = NCOL(Xp)) %*%
                               betas[x,])) +
                             (lv_preds %*% lv_coefs[[series]][x,])),
                  p = ps[x],
                  phi = twdiss[x, series]))
        }

        if(family == 'Poisson'){
          out <- rpois(n = length(which(newdata$series == s_name)),
                lambda = exp(((matrix(Xp[which(newdata$series == s_name),], ncol = NCOL(Xp)) %*%
                                 betas[x,])) +
                             (lv_preds %*% lv_coefs[[series]][x,])))
        }
      }


    } else {
      if(type == 'link'){
        if(object$trend_model == 'GP'){
          Sigma <- alpha_gps[x, series]^2 * exp(-(rho_gps[x, series]) *
                                                  outer(1:length(which(newdata$series == s_name)),
                                                        1:length(which(newdata$series == s_name)), "-")^2) +
            diag(1e-4, length(which(newdata$series == s_name)))

          out <- as.vector(((matrix(Xp[which(newdata$series == s_name),], ncol = NCOL(Xp)) %*%
                               betas[x,])) +
                             MASS::mvrnorm(1, rep(0,length(which(newdata$series == s_name))), Sigma))
        } else if(object$trend_model == 'None'){
          out <- as.vector(((matrix(Xp[which(newdata$series == s_name),], ncol = NCOL(Xp)) %*%
                               betas[x,])))
        } else {
          out <- as.vector(((matrix(Xp[which(newdata$series == s_name),], ncol = NCOL(Xp)) %*%
                               betas[x,])) +
                             (rnorm(length(which(newdata$series == s_name)),
                                    0, sqrt(1 / taus[x,series]))))
        }
      }

      if(type == 'response'){
        if(family == 'Negative Binomial'){
          if(object$trend_model == 'GP'){
            Sigma <- alpha_gps[x, series]^2 * exp(-(rho_gps[x, series]) *
                                                    outer(1:length(which(newdata$series == s_name)),
                                                          1:length(which(newdata$series == s_name)), "-")^2) +
              diag(1e-4, length(which(newdata$series == s_name)))
            out <- rnbinom(n = length(which(newdata$series == s_name)), size = sizes[x, series],
                           mu = exp(((matrix(Xp[which(newdata$series == s_name),], ncol = NCOL(Xp)) %*%
                                        betas[x,])) +
                                      (MASS::mvrnorm(1, rep(0,length(which(newdata$series == s_name))), Sigma))))
          } else if(object$trend_model == 'None'){
            out <- rnbinom(n = length(which(newdata$series == s_name)), size = sizes[x, series],
                           mu = exp(((matrix(Xp[which(newdata$series == s_name),], ncol = NCOL(Xp)) %*%
                                        betas[x,]))))
            } else {
            out <- rnbinom(n = length(which(newdata$series == s_name)), size = sizes[x, series],
                           mu = exp(((matrix(Xp[which(newdata$series == s_name),], ncol = NCOL(Xp)) %*%
                                        betas[x,])) +
                                      (rnorm(length(which(newdata$series == s_name)), 0, sqrt(1 / taus[x,series])))))
          }

        }

        if(family == 'Tweedie'){
          if(object$trend_model == 'None'){
            out <- rpois(n = length(which(newdata$series == s_name)),
                         lambda = mgcv::rTweedie(
                           mu = exp(((matrix(Xp[which(newdata$series == s_name),], ncol = NCOL(Xp)) %*%
                                        betas[x,]))),
                           p = ps[x],
                           phi = twdiss[x, series]))
          } else {
            out <- rpois(n = length(which(newdata$series == s_name)),
                         lambda = mgcv::rTweedie(
                           mu = exp(((matrix(Xp[which(newdata$series == s_name),], ncol = NCOL(Xp)) %*%
                                        betas[x,])) +
                                      (rnorm(length(which(newdata$series == s_name)),
                                             0, sqrt(1 / taus[x,series])))),
                           p = ps[x],
                           phi = twdiss[x, series]))
          }

        }

        if(family == 'Poisson'){
          if(object$trend_model == 'GP'){
            Sigma <- alpha_gps[x, series]^2 * exp(-(rho_gps[x, series]) *
                                                    outer(1:length(which(newdata$series == s_name)),
                                                          1:length(which(newdata$series == s_name)), "-")^2) +
              diag(1e-4, length(which(newdata$series == s_name)))
            out <- rpois(n = length(which(newdata$series == s_name)),
                         lambda = exp(((matrix(Xp[which(newdata$series == s_name),], ncol = NCOL(Xp)) %*%
                                          betas[x,])) +
                                        (MASS::mvrnorm(1, rep(0,length(which(newdata$series == s_name))), Sigma))))
          } else if(object$trend_model == 'None'){
            out <- rpois(n = length(which(newdata$series == s_name)),
                         lambda = exp(((matrix(Xp[which(newdata$series == s_name),], ncol = NCOL(Xp)) %*%
                                          betas[x,]))))
            } else {
            out <- rpois(n = length(which(newdata$series == s_name)),
                         lambda = exp(((matrix(Xp[which(newdata$series == s_name),], ncol = NCOL(Xp)) %*%
                                          betas[x,])) +
                                        (rnorm(length(which(newdata$series == s_name)),
                                               0, sqrt(1 / taus[x,series])))))
          }
        }
      }

    }
    out

  }, cl = cl))
  stopCluster(cl)

  return(predictions)
}
