#'@title Calculate randomized quantile residuals for mvgam objects
#'@name add_residuals.mvgam
#'@param object \code{list} object returned from \code{mvgam}. See [mvgam()]
#'@param ... unused
#'@details For each series, randomized quantile (i.e. Dunn-Smyth) residuals are calculated for inspecting model diagnostics
#'If the fitted model is appropriate then Dunn-Smyth residuals will be standard normal in distribution and no
#'autocorrelation will be evident. When a particular observation is missing, the residual is calculated by comparing independent
#'draws from the model's posterior distribution
#'@return A list object of class `mvgam` with residuals included in the `'resids'` slot
#'@export
add_residuals <- function(object, ...){
  UseMethod("add_residuals", object)
}

#'@rdname add_residuals.mvgam
#'@method add_residuals mvgam
#'@export
add_residuals.mvgam = function(object, ...){
  resids <- dsresids_vec(object)
  object$resids <- resids
  return(object)
}
