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

    # Identify response variable
    resp_terms <- as.character(rlang::f_lhs(formula$call))
    if(length(resp_terms) == 1L){
      resp <- resp_terms
    } else {
      if(any(grepl('cbind', resp_terms))){
        resp_terms <- resp_terms[-grepl('cbind', resp_terms)]
        resp <- resp_terms[1]
      }
    }

    # Now add the observed response, in case there are any
    # NAs there that need to be updated
    out[,resp] <- formula$obs_data$y

    # Ensure 'cap' is included if this is an N-mixture model
    if(formula$family == 'nmix'){
      out$cap <- formula$obs_data$cap
    }
  }
  return(out)
}

#' @inheritParams model.frame.mvgam
#' @rdname model.frame.mvgam
#' @export
model.frame.mvgam_prefit = function(formula, trend_effects = FALSE, ...){
  # Check trend_effects
  if(trend_effects){
    if(is.null(formula$trend_call)){
      out <- NULL
    } else {
      out <- stats::model.frame(formula$trend_mgcv_model)
    }
  } else {
    out <- stats::model.frame(formula$mgcv_model)

    # Identify response variable
    resp_terms <- as.character(rlang::f_lhs(formula$call))
    if(length(resp_terms) == 1L){
      resp <- resp_terms
    } else {
      if(any(grepl('cbind', resp_terms))){
        resp_terms <- resp_terms[-grepl('cbind', resp_terms)]
        resp <- resp_terms[1]
      }
    }

    # Now add the observed response, in case there are any
    # NAs there that need to be updated
    out[,resp] <- formula$obs_data$y
  }

  # Ensure 'cap' is included if this is an N-mixture model
  if(formula$family == 'nmix'){
    out$cap <- formula$obs_data$cap
  }

  return(out)
}
