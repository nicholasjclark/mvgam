#'@title Extract DIC information from fitted mvgam objects
#'@name dic.mvgam
#'@param object \code{list} object returned from \code{mvjagam}
#'@return A summary of the DIC and penalised DIC from the fitted model
#'
NULL
#'@export
dic <- function(x, what, ...){
  UseMethod("dic")
}

#'@rdname dic.mvgam
#'@method dic mvgam
#'@export
dic.mvgam = function(object){
  runjags::extract(object$jags_model,
                   "dic", n.iter = 1000)
}
