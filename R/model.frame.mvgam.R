#'Extract model.frame from a fitted \pkg{mvgam} object
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

    # Any other required variables, needed for grouped models
    if(!inherits(attr(formula$model_data, 'trend_model'), 'mvgam_trend') &
       !inherits(formula$trend_model, 'mvgam_trend')){
      trend_model <- list(trend_model = attr(formula$model_data, 'trend_model'),
                          unit = 'time',
                          gr = 'NA',
                          subgr = 'series')
    }

    if(inherits(attr(formula$model_data, 'trend_model'), 'mvgam_trend')){
      trend_model <- attr(formula$model_data, 'trend_model')
    }

    if(inherits(formula$trend_model, 'mvgam_trend')){
      trend_model <- formula$trend_model
    }

    other_vars <- c(trend_model$unit,
                    trend_model$gr,
                    trend_model$subgr)
    if(!is.null(attr(formula$model_data, 'prepped_trend_model'))){
      prepped_model <- attr(formula$model_data, 'prepped_trend_model')
      other_vars <- c(other_vars,
                      c(prepped_model$unit,
                        prepped_model$gr,
                        prepped_model$subgr))
    }
    other_vars <- setdiff(unique(other_vars),
                          c('NA', colnames(out)))

    if(length(other_vars)){
      orig_names <- colnames(out)
      for(i in 1:length(other_vars)){
        out <- cbind(out, formula$obs_data[[other_vars[i]]])
      }
      colnames(out) <- c(orig_names, other_vars)
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

    # Ensure 'cap' is included if this is an N-mixture model
    if(formula$family == 'nmix'){
      out$cap <- formula$obs_data$cap
    }

    # Any other required variables, needed for grouped models
    if(!inherits(attr(formula$model_data, 'trend_model'), 'mvgam_trend')){
      trend_model <- list(trend_model = attr(formula$model_data, 'trend_model'),
                          unit = 'time',
                          gr = 'NA',
                          subgr = 'series')
    } else {
      trend_model <- attr(formula$model_data, 'trend_model')
    }
    other_vars <- c(trend_model$unit,
                    trend_model$gr,
                    trend_model$subgr)
    if(!is.null(attr(formula$model_data, 'prepped_trend_model'))){
      prepped_model <- attr(formula$model_data, 'prepped_trend_model')
      other_vars <- c(other_vars,
                      c(prepped_model$unit,
                        prepped_model$gr,
                        prepped_model$subgr))
    }
    other_vars <- setdiff(unique(other_vars),
                          c('NA', colnames(out)))

    if(length(other_vars)){
      orig_names <- colnames(out)
      for(i in 1:length(other_vars)){
        out <- cbind(out, formula$obs_data[[other_vars[i]]])
      }
      colnames(out) <- c(orig_names, other_vars)
    }
  }

  return(out)
}
