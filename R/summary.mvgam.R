#'Summary for a fitted mvgam object
#'
#'These functions take a fitted \code{mvgam} object and return various useful summaries
#'@param object \code{list} object returned from \code{mvgam}
#'@author Nicholas J Clark
#'@details `summary.mvgam` and `summary.mvgam_prefit` return brief summaries of the model's call is printed, along with posterior intervals for
#'some of the key parameters in the model. Note that some smooths have extra penalties on the null space,
#'so summaries for the \code{rho} parameters may include more penalty terms than the number of smooths in
#'the original model formula.
#'
#'`coef.mvgam` returns either summaries or full posterior estimates for `GAM` component
#'coefficients
#'@return For `summary.mvgam` and `summary.mvgam_prefit`, Aa\code{list} is printed
#'on-screen showing the summaries for the model
#'
#'For `coef.mvgam`, either a \code{matrix} of posterior coefficient distributions
#'(if \code{summarise == FALSE} or \code{data.frame} of coefficient summaries)
#'@export
summary.mvgam = function(object){

#### Standard summary of formula and model arguments ####
message("GAM formula:")
print(object$call)
message()

message("Family:")
cat(paste0(object$family, '\n'))
message()

message("Link function:")
cat(paste0(family_links(object$family), '\n'))
message()

message("Trend model:")
cat(paste0(object$trend_model, '\n'))
message()

if(object$use_lv){
  message("N latent factors:")
  cat(object$n_lv, '\n')
  message()
}

message('N series:')
cat(NCOL(object$ytimes), '\n')
message()

if(!is.null(object$upper_bounds)){
  message('Upper bounds:')
  cat(object$upper_bounds, '\n')
  message()
}

message('N timepoints:')
cat(max(object$obs_data$time), '\n')
message()

if(object$fit_engine == 'jags'){
  message('Status:')
  cat('Fitted using JAGS', '\n')
  message()
}

if(object$fit_engine == 'stan'){
  message('Status:')
  cat('Fitted using Stan', '\n')
  message()
}

if(object$family == 'negative binomial'){
  message("Dispersion parameter estimates:")
  print(mcmc_summary(object$model_output, 'phi')[,c(3:7)])
  message()
}

if(object$family == 'beta'){
  message("Precision parameter estimates:")
  print(mcmc_summary(object$model_output, 'phi')[,c(3:7)])
  message()
}

if(object$family == 'tweedie'){
  message("Dispersion parameter estimates:")
  print(mcmc_summary(object$model_output, 'phi')[,c(3:7)])
  message()
}

if(object$family == 'gaussian'){
  message("Observation error parameter estimates:")
  print(mcmc_summary(object$model_output, 'sigma_obs')[,c(3:7)])
  message()
}

if(object$family == 'student'){
  message("Observation error parameter estimates:")
  print(mcmc_summary(object$model_output, 'sigma_obs')[,c(3:7)])
  message()

  message("Observation df parameter estimates:")
  print(mcmc_summary(object$model_output, 'nu')[,c(3:7)])
  message()
}

if(object$family == 'lognormal'){
  message("log(observation error) parameter estimates:")
  print(mcmc_summary(object$model_output, 'sigma_obs')[,c(3:7)])
  message()
}

message("GAM coefficient (beta) estimates:")
coef_names <- names(object$mgcv_model$coefficients)
mvgam_coefs <- mcmc_summary(object$model_output, 'b')[,c(3:7)]
rownames(mvgam_coefs) <- coef_names
print(mvgam_coefs)
message()

smooth_labs <- do.call(rbind, lapply(seq_along(object$mgcv_model$smooth), function(x){
  data.frame(label = object$mgcv_model$smooth[[x]]$label,
             class = class(object$mgcv_model$smooth[[x]])[1])
}))

if(any(smooth_labs$class == 'random.effect')){
  re_smooths <- smooth_labs %>%
    dplyr::filter(class == 'random.effect') %>%
    dplyr::pull(label)

  if(object$fit_engine == 'jags'){
    re_sds <- mcmc_summary(object$model_output,
                                   paste0('sigma_raw',
                                          seq_along(re_smooths)))[,c(3:7)]

    re_mus <- mcmc_summary(object$model_output,
                                   paste0('mu_raw',
                                          seq_along(re_smooths)))[,c(3:7)]
  } else {
    re_sds <- mcmc_summary(object$model_output, 'sigma_raw',
                                   ISB = TRUE)[,c(3:7)]

    re_mus <- mcmc_summary(object$model_output, 'mu_raw',
                                   ISB = TRUE)[,c(3:7)]
  }

  rownames(re_sds) <- rownames(re_mus) <- re_smooths

  message("GAM random effect population mean estimates:")
  print(re_mus)
  message()

  message("GAM random effect population SD estimates:")
  print(re_sds)
  message()
}

if(any(!is.na(object$sp_names)) & !all(smooth_labs$class == 'random.effect')){
  message("GAM smoothing parameter (rho) estimates:")
  rho_coefs <- mcmc_summary(object$model_output, 'rho')[,c(3:7)]
  rownames(rho_coefs) <- object$sp_names

  # Don't print random effect lambdas as they follow the prior distribution
  if(any(smooth_labs$class == 'random.effect')){
    re_smooths <- smooth_labs %>%
      dplyr::filter(class == 'random.effect') %>%
      dplyr::pull(label)

  print(rho_coefs[!rownames(rho_coefs) %in% re_smooths,])

  } else {
    print(rho_coefs)
  }
  message()
}

if(object$use_lv){
  if(object$trend_model != 'None'){
    if(object$trend_model == 'RW'){
      if(object$drift){
        message("Latent trend drift estimates:")
        print(mcmc_summary(object$model_output, c('drift'))[,c(3:7)])
        message()
      } else {
      }
    }

    if(object$trend_model == 'GP'){
      message("Latent trend length scale (rho) estimates:")
      print(mcmc_summary(object$model_output, c('rho_gp'))[,c(3:7)])
      message()
    }

    if(object$trend_model == 'AR1'){
      if(object$drift){
        message("Latent trend drift and AR parameter estimates:")
        print(mcmc_summary(object$model_output, c('drift', 'ar1'))[,c(3:7)])
        message()
      } else {
        message("Latent trend AR parameter estimates:")
        print(mcmc_summary(object$model_output, c('ar1'))[,c(3:7)])
        message()
      }
    }

    if(object$trend_model == 'AR2'){
      if(object$drift){
        message("Latent trend drift and AR parameter estimates:")
        print(mcmc_summary(object$model_output, c('drift', 'ar1', 'ar2'))[,c(3:7)])
        message()
      } else {
        message("Latent trend AR parameter estimates:")
        print(mcmc_summary(object$model_output, c('ar1', 'ar2'))[,c(3:7)])
        message()
      }
    }

    if(object$trend_model == 'AR3'){
      if(object$drift){
        message("Latent trend drift and AR parameter estimates:")
        print(mcmc_summary(object$model_output, c('drift', 'ar1', 'ar2', 'ar3'))[,c(3:7)])
        message()
      } else {
        message("Latent trend AR parameter estimates:")
        print(mcmc_summary(object$model_output, c('ar1', 'ar2', 'ar3'))[,c(3:7)])
        message()
      }
    }
  }
}

if(!object$use_lv){
  if(object$trend_model != 'None'){
    if(object$trend_model == 'RW'){
      if(object$drift){
        message("Latent trend drift and sigma estimates:")
        print(mcmc_summary(object$model_output, c('drift', 'sigma'))[,c(3:7)])
        message()
      } else {
        message("Latent trend variance estimates:")
        print(mcmc_summary(object$model_output, c('sigma'))[,c(3:7)])
        message()
      }
    }

    if(object$trend_model == 'VAR1'){
      if(object$drift){
        message("Latent trend drift and VAR parameter estimates:")
        print(mcmc_summary(object$model_output, c('drift', 'A', 'Sigma'))[,c(3:7)])
        message()
      } else {
        message("Latent trend VAR parameter estimates:")
        print(mcmc_summary(object$model_output, c('A', 'Sigma'))[,c(3:7)])
        message()
      }
    }

    if(object$trend_model == 'AR1'){
      if(object$drift){
        message("Latent trend drift and AR parameter estimates:")
        print(mcmc_summary(object$model_output, c('drift', 'ar1', 'sigma'))[,c(3:7)])
        message()
      } else {
        message("Latent trend parameter AR estimates:")
        print(mcmc_summary(object$model_output, c('ar1', 'sigma'))[,c(3:7)])
        message()
      }
    }

    if(object$trend_model == 'AR2'){
      if(object$drift){
        message("Latent trend drift and AR parameter estimates:")
        print(mcmc_summary(object$model_output, c('drift', 'ar1', 'ar2', 'sigma'))[,c(3:7)])
        message()
      } else {
        message("Latent trend AR parameter estimates:")
        print(mcmc_summary(object$model_output, c('ar1', 'ar2', 'sigma'))[,c(3:7)])
        message()
      }
    }

    if(object$trend_model == 'AR3'){
      if(object$drift){
        message("Latent trend drift and AR parameter estimates:")
        print(mcmc_summary(object$model_output, c('drift', 'ar1', 'ar2', 'ar3', 'sigma'))[,c(3:7)])
        message()
      } else {
        message("Latent trend AR parameter estimates:")
        print(mcmc_summary(object$model_output, c('ar1', 'ar2', 'ar3', 'sigma'))[,c(3:7)])
        message()
      }
    }

    if(object$trend_model == 'GP'){
        message("Latent trend marginal deviation (alpha) and length scale (rho) estimates:")
        print(mcmc_summary(object$model_output, c('alpha_gp', 'rho_gp'))[,c(3:7)])
        message()

    }
  }
}

if(object$fit_engine == 'stan'){
  message('Stan MCMC diagnostics')
  check_all_diagnostics(object$model_output,
                        max_treedepth = object$max_treedepth)
  message()
}

if(object$fit_engine == 'jags'){
  message('JAGS MCMC diagnostics')
  rhats <- mcmc_summary(object$model_output)[,6]
  if(any(rhats > 1.05)){
    cat('Rhats above 1.05 found for',
        length(which(rhats > 1.05)),
        'parameters\n*Diagnose further to investigate why the chains have not mixed\n')
  } else {
    cat('Rhat looks reasonable for all parameters\n')
  }

  message()
}

}

#' @rdname summary.mvgam
#' @export
summary.mvgam_prefit = function(object){
  message("GAM formula:")
  print(object$call)
  message()

  message("Family:")
  cat(paste0(object$family, '\n'))
  message()

  message("Link function:")
  cat(paste0(family_links(object$family), '\n'))
  message()

  message("Trend model:")
  cat(paste0(object$trend_model, '\n'))
  message()

  if(object$use_lv){
    message("N latent factors:")
    cat(object$n_lv, '\n')
    message()
  }

  message('N series:')
  cat(NCOL(object$ytimes), '\n')
  message()

  message('N timepoints:')
  cat(max(object$obs_data$time), '\n')
  message()

  message('Status:')
  cat('Not fitted', '\n')
  message()
}

#' @rdname summary.mvgam
#' @export
#'@title Extract mvgam beta coefficients from the GAM component
#'@param object \code{list} object returned from \code{mvgam}
#'@param summarise \code{logical}. Summaries of coefficients will be returned
#'if \code{TRUE}. Otherwise the full posterior distribution will be returned
#'
#'@method coef mvgam
#'@export
coef.mvgam = function(object, summarise = TRUE){
  coef_names <- names(object$mgcv_model$coefficients)

  if(summarise){
    mvgam_coefs <- mcmc_summary(object$model_output, 'b')[,c(3:7)]
    rownames(mvgam_coefs) <- coef_names
  } else {
    mvgam_coefs <- MCMCvis::MCMCchains(object$model_output, 'b')
    colnames(mvgam_coefs) <- coef_names
  }

  return(mvgam_coefs)
}
