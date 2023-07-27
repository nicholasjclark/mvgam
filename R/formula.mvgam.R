#'Extract model.frame from a fitted mvgam object
#'
#'
#'@inheritParams stats::formula
#'@param trend_effects \code{logical}, return the model.frame from the
#'observation model (if \code{FALSE}) or from the underlying process
#'model (if\code{TRUE})
#'@param ... Ignored
#'@author Nicholas J Clark
#'@return A \code{matrix} containing the fitted model frame
#'@export
formula.mvgam = function(x, trend_effects = FALSE, ...){
  # Check trend_effects
  if(trend_effects){
    if(is.null(formula$trend_call)){
      stop('no trend_formula exists so there is no trend-level model.frame')
    }
  }

  if(!trend_effects){
    out <- x$call
  } else {
    out <- x$trend_call
    out <- update(out, as.formula(paste('trend_y', '~.')))
  }
  return(out)
}
