#'Extract model.frame from a fitted mvgam object
#'
#'
#'@inheritParams stats::model.frame
#'@param trend_effects \code{logical}, return the model.frame from the
#'observation model (if \code{FALSE}) or from the underlying process
#'model (if\code{TRUE})
#'@param ... Ignored
#'@author Nicholas J Clark
#'@return A \code{matrix} containing the fitted model frame
#'@export
model.frame.mvgam = function(formula, trend_effects = FALSE, ...){
  # Check trend_effects
  if(trend_effects){
    if(is.null(formula$trend_call)){
      out <- NULL
    } else {
      out <- stats::model.frame(formula$trend_mgcv_model)
    }
  } else {
    out <- stats::model.frame(formula$mgcv_model)
  }
  return(out)
}
