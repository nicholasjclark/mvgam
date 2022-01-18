#'Plot mvjagam trace plots
#'
#'This function returns MCMC trace plots for the specified parameter(s)
#'
#'@param object \code{list} object returned from \code{mvjagam}
#'@param param \code{character} vector specifying which parameter(s) are to be plotted. Valid options are:
#''rho' (smoothing penalties), 'trend' (which returns traces for all latent trend parameters,
#'including drift and autoregressive terms), 'b' (traces for all smooth beta coefficients) and
#''r' (the Negative Binomial overdispersion parameter)
#'\code{newdata} just as you would when predicting from a \code{\link[mgcv]{gam}} model
#'@return A \code{\link[MCMCvis]{MCMCtrace}} traceplot
#'@seealso \code{\link[MCMCvis]{MCMCtrace}}
#'@export
plot_mvgam_trace = function(object, param = 'rho'){

  param <- match.arg(arg = param, choices = c("rho", "trend", "b", "r"))

  if(param == 'rho'){
    param_names <- unlist(lapply(seq(1:length(object$mgcv_model$smooth)), function(i){

      number_seq <- seq(1:(1 + object$mgcv_model$smooth[[i]]$null.space.dim +
                             (length(object$mgcv_model$smooth[[i]]$sp) - 1)))
      number_seq[1] <- ''

      paste0(rep(object$mgcv_model$smooth[[i]]$label,
                 1 + object$mgcv_model$smooth[[i]]$null.space.dim +
                   (length(object$mgcv_model$smooth[[i]]$sp) - 1)),
             number_seq)
    }))
  }

  if(param == 'b'){
    param_names <- names(object$mgcv_model$coefficients)
  }

  if(param == 'trend'){
    param <- c('phi', 'ar1', 'ar2', 'ar3')
    if(object$use_lv){
      param_names <- rownames(MCMCvis::MCMCsummary(object$jags_output, param))
    } else {
      param <- c(param, 'tau')
      param_names <- rownames(MCMCvis::MCMCsummary(object$jags_output, param))
      param_names[which(param_names == 'tau')] <- 'trend_precision'
    }
    param_names <- gsub('phi', 'drift', param_names)
  }

  MCMCvis::MCMCtrace(trends_mod$jags_output, param,
                     pdf = FALSE,
                     n.eff = TRUE,
                     Rhat = TRUE,
                     main_den = param_names,
                     main_tr = param_names)
}
