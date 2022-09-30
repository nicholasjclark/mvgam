#'Summary for a fitted mvgam object
#'
#'This function takes a fitted \code{mvgam} object and prints various useful summaries from it
#'
#'@param object \code{list} object returned from \code{mvgam}
#'@author Nicholas J Clark
#'@details A brief summary of the model's call is printed, along with posterior intervals for
#'some of the key parameters in the model. Note that some smooths have extra penalties on the null space,
#'so summaries for the \code{rho} parameters may include more penalty terms than the number of smooths in
#'the original model formula.
#'@return A \code{list} is printed on-screen showing the summaries for the model
#'@export
summary.mvgam = function(object){

#### Standard summary of formula and model argumements ####
message("GAM formula:")
print(object$call)
message()

message("Family:")
cat(paste0(object$family, '\n'))
message()

message("Link function:")
cat(paste0('log', '\n'))
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

message('N observations:')
if(class(object$obs_data)[1] == 'list'){
  cat(length(object$obs_data$y), '\n')
} else {
  cat(NROW(object$obs_data), '\n')
}
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

if(object$family == 'Negative Binomial'){
  message("Dispersion parameter estimates:")
  print(MCMCvis::MCMCsummary(object$model_output, 'r')[,c(3:7)])
  message()
}

if(object$family == 'Tweedie'){
  message("Dispersion parameter estimates:")
  print(MCMCvis::MCMCsummary(object$model_output, 'twdis')[,c(3:7)])
  message()
}

message("GAM coefficient (beta) estimates:")
coef_names <- names(object$mgcv_model$coefficients)
mvgam_coefs <- MCMCvis::MCMCsummary(object$model_output, 'b')[,c(3:7)]
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
    re_sds <- MCMCvis::MCMCsummary(object$model_output,
                                   paste0('sigma_raw',
                                          seq_along(re_smooths)))[,c(3:7)]

    re_mus <- MCMCvis::MCMCsummary(object$model_output,
                                   paste0('mu_raw',
                                          seq_along(re_smooths)))[,c(3:7)]
  } else {
    re_sds <- MCMCvis::MCMCsummary(object$model_output, 'sigma_raw',
                                   ISB = TRUE)[,c(3:7)]

    re_mus <- MCMCvis::MCMCsummary(object$model_output, 'mu_raw',
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

if(any(grep('rho', rownames(MCMCvis::MCMCsummary(object$model_output))))){
  message("GAM smoothing parameter (rho) estimates:")
  rho_coefs <- MCMCvis::MCMCsummary(object$model_output, 'rho')[,c(3:7)]

  name_starts <- unlist(purrr::map(object$pregam$smooth, 'first.sp'))
  name_ends <- unlist(purrr::map(object$pregam$smooth, 'last.sp'))

  rho_names <- unlist(lapply(seq(1:length(object$mgcv_model$smooth)), function(i){

    number_seq <- seq(1:(1 + name_ends[i] - name_starts[i]))
    number_seq[1] <- ''
    paste0(rep(object$mgcv_model$smooth[[i]]$label,
               length(number_seq)),
           number_seq)
  }))
  rownames(rho_coefs) <- rho_names

  # Don't print random effect lambdas as they follow the prior distribution
  if(any(smooth_labs$class == 'random.effect')){
    re_smooths <- smooth_labs %>%
      dplyr::filter(class == 'random.effect') %>%
      dplyr::pull(label)

  print(rho_coefs[!rho_names %in% re_smooths,])

  } else {
    print(rho_coefs)
  }
  message()
}

if(object$use_lv){
  if(object$trend_model != 'None'){
    if(object$trend_model == 'RW'){
      if(object$drift){
        message("Latent trend drift (phi) estimates:")
        print(MCMCvis::MCMCsummary(object$model_output, c('phi'))[,c(3:7)])
        message()
      } else {
      }
    }

    if(object$trend_model == 'AR1'){
      if(object$drift){
        message("Latent trend drift (phi) and parameter estimates:")
        print(MCMCvis::MCMCsummary(object$model_output, c('phi', 'ar1'))[,c(3:7)])
        message()
      } else {
        message("Latent trend parameter estimates:")
        print(MCMCvis::MCMCsummary(object$model_output, c('ar1'))[,c(3:7)])
        message()
      }
    }

    if(object$trend_model == 'AR2'){
      if(object$drift){
        message("Latent trend drift (phi) and parameter estimates:")
        print(MCMCvis::MCMCsummary(object$model_output, c('phi', 'ar1', 'ar2'))[,c(3:7)])
        message()
      } else {
        message("Latent trend parameter estimates:")
        print(MCMCvis::MCMCsummary(object$model_output, c('ar1', 'ar2'))[,c(3:7)])
        message()
      }
    }

    if(object$trend_model == 'AR3'){
      if(object$drift){
        message("Latent trend drift (phi) and parameter estimates:")
        print(MCMCvis::MCMCsummary(object$model_output, c('phi', 'ar1', 'ar2', 'ar3'))[,c(3:7)])
        message()
      } else {
        message("Latent trend parameter estimates:")
        print(MCMCvis::MCMCsummary(object$model_output, c('ar1', 'ar2', 'ar3'))[,c(3:7)])
        message()
      }
    }
  }
}

if(!object$use_lv){
  if(object$trend_model != 'None'){
    if(object$trend_model == 'RW'){
      if(object$drift){
        message("Latent trend drift (phi) and sigma estimates:")
        print(MCMCvis::MCMCsummary(object$model_output, c('phi', 'sigma'))[,c(3:7)])
        message()
      } else {
        message("Latent trend variance estimates:")
        print(MCMCvis::MCMCsummary(object$model_output, c('sigma'))[,c(3:7)])
        message()
      }
    }

    if(object$trend_model == 'AR1'){
      if(object$drift){
        message("Latent trend drift (phi) and parameter estimates:")
        print(MCMCvis::MCMCsummary(object$model_output, c('phi', 'ar1', 'sigma'))[,c(3:7)])
        message()
      } else {
        message("Latent trend parameter estimates:")
        print(MCMCvis::MCMCsummary(object$model_output, c('ar1', 'sigma'))[,c(3:7)])
        message()
      }
    }

    if(object$trend_model == 'AR2'){
      if(object$drift){
        message("Latent trend drift (phi) and parameter estimates:")
        print(MCMCvis::MCMCsummary(object$model_output, c('phi', 'ar1', 'ar2', 'sigma'))[,c(3:7)])
        message()
      } else {
        message("Latent trend parameter estimates:")
        print(MCMCvis::MCMCsummary(object$model_output, c('ar1', 'ar2', 'sigma'))[,c(3:7)])
        message()
      }
    }

    if(object$trend_model == 'AR3'){
      if(object$drift){
        message("Latent trend drift (phi) and parameter estimates:")
        print(MCMCvis::MCMCsummary(object$model_output, c('phi', 'ar1', 'ar2', 'ar3', 'sigma'))[,c(3:7)])
        message()
      } else {
        message("Latent trend parameter estimates:")
        print(MCMCvis::MCMCsummary(object$model_output, c('ar1', 'ar2', 'ar3', 'sigma'))[,c(3:7)])
        message()
      }
    }

    if(object$trend_model == 'GP'){
        message("Latent trend marginal deviation (alpha) and length scale (rho) estimates:")
        print(MCMCvis::MCMCsummary(object$model_output, c('alpha_gp', 'rho_gp'))[,c(3:7)])
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
  rhats <- MCMCvis::MCMCsummary(object$model_output)[,6]
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


#'@export
summary.mvgam_prefit = function(object){
  message("GAM formula:")
  print(object$call)
  message()

  message("Family:")
  cat(paste0(object$family, '\n'))
  message()

  message("Link function:")
  cat(paste0('log', '\n'))
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

  message('N observations per series:')
  if(class(object$obs_data) == 'list'){
    cat(length(object$obs_data$y) / NCOL(object$ytimes), '\n')
  } else {
    cat(NROW(object$obs_data) / NCOL(object$ytimes), '\n')
  }
  message()

  message('Status:')
  cat('Not fitted', '\n')
  message()
}

