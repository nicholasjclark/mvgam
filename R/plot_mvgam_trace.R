#'Plot mvjagam trace plots
#'
#'This function returns MCMC trace plots for the specified parameter(s)
#'
#'@param object \code{list} object returned from \code{mvjagam}
#'@param param \code{character} vector specifying which parameter(s) are to be plotted. Valid options are:
#''rho' (smoothing penalties), 'b' (traces for all smooth beta coefficients)
#'@param overlay_prior \code{logical}. If \code{TRUE} and \code{param} is 'rho' or 'trend', the default prior densities
#'are also shown to evaluate how informative the data were for shaping the posterior estimates. Note that these
#'densities assume that the priors for 'rho', 'ar' and 'tau' were not changed from their default values when the
#'model was originaly fitted
#'\code{newdata} just as you would when predicting from a \code{\link[mgcv]{gam}} model
#'@return A \code{\link[MCMCvis]{MCMCtrace}} traceplot.
#'@seealso \code{\link[MCMCvis]{MCMCtrace}}
#'@export
plot_mvgam_trace = function(object, param = 'rho', overlay_prior = TRUE){

  # Check arguments
  param <- match.arg(arg = param, choices = c("rho", "b"))

  if(class(object) != 'mvgam'){
    stop('argument "object" must be of class "mvgam"')
  }

  if(param == 'rho'){
    param_names <- object$sp_names
    prior_mat <- matrix(NA, nrow = dim(MCMCvis::MCMCchains(object$model_output, 'rho'))[1],
                        ncol = length(param_names))
    for(i in 1:(length(param_names))){
      prior_mat[,i] <- log(rexp(dim(MCMCvis::MCMCchains(object$model_output, 'rho'))[1], 0.05))
    }

    if(overlay_prior){
      MCMCvis::MCMCtrace(object$model_output, param,
                         priors = prior_mat,
                         col_pr = 'grey60',
                         post_zm = F,
                         pdf = FALSE,
                         n.eff = TRUE,
                         Rhat = TRUE,
                         main_den = param_names,
                         main_tr = param_names,
                         col_den = "#A25050",
                         lwd_den = 2.5,
                         lwd_pr = 2.5,
                         col_txt = 'black',
                         xlab_den = 'Prior (grey) vs posterior (red)')
    } else {
      MCMCvis::MCMCtrace(object$model_output, param,
                         post_zm = F,
                         pdf = FALSE,
                         n.eff = TRUE,
                         Rhat = TRUE,
                         main_den = param_names,
                         main_tr = param_names,
                         col_den = "#A25050",
                         lwd_den = 2.5,
                         lwd_pr = 2.5,
                         col_txt = 'black',
                         xlab_den = 'Posterior estimate')
    }

  }

  if(param == 'b'){
    param_names <- names(object$mgcv_model$coefficients)

    MCMCvis::MCMCtrace(object$model_output, param,
                       pdf = FALSE,
                       n.eff = TRUE,
                       Rhat = TRUE,
                       main_den = param_names,
                       main_tr = param_names,
                       col_den = "#A25050",
                       lwd_den = 2.5,
                       col_txt = 'black',
                       xlab_den = 'Posterior estimate')
  }

}
