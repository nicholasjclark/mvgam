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
  # Grab the sim2jam object, which is needed to
  # calculate effective degrees of freedom for smooth terms
  jam = object$jam_model

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
  cat('Fitted using runjags::run.jags()', '\n')
  message()
}

if(object$fit_engine == 'stan'){
  message('Status:')
  cat('Fitted using rstan::stan()', '\n')
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

#### Summary table for smooth functions ####
coef_names <- names(object$mgcv_model$coefficients)
if(length(object$mgcv_model$smooth) > 0){
  m <- length(object$mgcv_model$smooth)

  edf_table <- matrix(NA, nrow = m, ncol = 2)
  for(i in 1:m){
    start <- object$mgcv_model$smooth[[i]]$first.para
    stop <- object$mgcv_model$smooth[[i]]$last.para
    edf_table[i, 1] <- sum(jam$edf[start:stop])
    edf_table[i, 2] <- sum(object$mgcv_model$smooth[[i]]$df)
  }
  dimnames(edf_table) <- list(unlist(purrr::map(object$mgcv_model$smooth, 'label')),
                              c("edf", "df"))

  message('GAM smooth term estimated degrees of freedom:')
  printCoefmat(edf_table, digits = 4, signif.stars = T)
  message()
}

message("GAM coefficient (beta) estimates:")
mvgam_coefs <- MCMCvis::MCMCsummary(object$model_output, 'b')[,c(3:7)]
rownames(mvgam_coefs) <- coef_names
print(mvgam_coefs)
message()

if(length(object$mgcv_model$smooth) > 0){
  message("GAM smoothing parameter (rho) estimates:")
  rho_coefs <- MCMCvis::MCMCsummary(object$model_output, 'rho')[,c(3:7)]

  name_starts <- unlist(purrr:::map(jam$smooth, 'first.sp'))
  name_ends <- unlist(purrr:::map(jam$smooth, 'last.sp'))

  rho_names <- unlist(lapply(seq(1:length(object$mgcv_model$smooth)), function(i){

    number_seq <- seq(1:(1 + name_ends[i] - name_starts[i]))
    number_seq[1] <- ''

    paste0(rep(object$mgcv_model$smooth[[i]]$label,
               length(number_seq)),
           number_seq)
  }))
  rownames(rho_coefs) <- rho_names
  print(rho_coefs)
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
  if(object$use_lv || object$trend_model == 'GP'){
    max_treedepth <- 12
  } else {
    max_treedepth <- 10
  }
  check_all_diagnostics(object$model_output, max_treedepth = max_treedepth)
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

