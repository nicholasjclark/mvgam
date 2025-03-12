#'Extract formulae from \pkg{mvgam} objects
#'
#'@rdname formula.mvgam
#'@param x `mvgam`, `jsdgam` or `mvgam_prefit` object
#'@param trend_effects \code{logical}, return the formula from the
#'observation model (if \code{FALSE}) or from the underlying process
#'model (if\code{TRUE})
#'@param ... Ignored
#'@author Nicholas J Clark
#'@return A \code{formula} object
#'@export
formula.mvgam = function(x, trend_effects = FALSE, ...) {
  # Check trend_effects
  if (trend_effects) {
    if (is.null(x$trend_call)) {
      stop('no trend_formula exists so there is no trend-level model.frame')
    }
  }

  if (!trend_effects) {
    out <- x$call
  } else {
    out <- x$trend_call
    out <- update(out, as.formula(paste('trend_y', '~.')))
  }
  return(out)
}

#'@rdname formula.mvgam
#'@export
formula.mvgam_prefit = function(x, trend_effects = FALSE, ...) {
  # Check trend_effects
  if (trend_effects) {
    if (is.null(x$trend_call)) {
      stop('no trend_formula exists so there is no trend-level model.frame')
    }
  }

  if (!trend_effects) {
    out <- x$call
  } else {
    out <- x$trend_call
    out <- update(out, as.formula(paste('trend_y', '~.')))
  }
  return(out)
}
