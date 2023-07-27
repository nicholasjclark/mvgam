#' Helper functions for mvgam marginaleffects calculations
#' @importFrom stats coef model.frame
#' @inheritParams marginaleffects::get_coef
#' @inheritParams marginaleffects::set_coef
#' @inheritParams marginaleffects::get_vcov
#' @inheritParams marginaleffects::get_predict
#' @inheritParams insight::get_data
#' @inheritParams insight::find_predictors
#' @param process_error `logical`. If `TRUE`, uncertainty in the latent
#' process (or trend) model is incorporated in predictions
#' @param n_cores `Integer` specifying number of cores to use for
#' generating predictions
#' @name mvgam_marginaleffects
#' @author Nicholas J Clark
NULL

#' Functions needed for working with marginaleffects
#' @rdname mvgam_marginaleffects
#' @export
get_coef.mvgam <- function(model, trend_effects = FALSE, ...) {

  # Check trend_effects
  if(trend_effects){
    if(is.null(model$trend_call)){
      stop('no trend_formula exists so there no trend-level coefficients')
    }
  }

  if(!trend_effects){
    b <- coef(model$mgcv_model)
  } else {
    b <- coef(model$trend_mgcv_model)
  }
  return(b)
}

#' @rdname mvgam_marginaleffects
#' @export
set_coef.mvgam <- function(model, coefs,
                           trend_effects = FALSE, ...) {

  # Check trend_effects
  if(trend_effects){
    if(is.null(model$trend_call)){
      stop('no trend_formula exists so there no trend-level coefficients')
    }
  }

  out <- model

  if(!trend_effects){
    out$mgcv_model$coefficients <- coefs
  } else {
    out$trend_mgcv_model$coefficients <- coefs
  }
  return(out)
}

#' @rdname mvgam_marginaleffects
#' @export
get_vcov.mvgam <- function(model,
                             vcov = NULL,
                             ...) {
  if (!is.null(vcov) && !is.logical(vcov)) {
    insight::format_warning("The `vcov` argument is not supported for models of this class.")
  }
  return(NULL)
}

#' @rdname mvgam_marginaleffects
#' @export
get_predict.mvgam <- function(model, newdata,
                              type = 'response',
                              process_error = FALSE,
                              n_cores = 1,
                              ...) {
  preds <- predict(object = model,
                   newdata = newdata,
                   type = type,
                   process_error = process_error,
                   n_cores = n_cores,
                   ...)
  out <- data.frame(
    rowid = seq_len(NCOL(preds)),
    estimate = apply(preds, 2, median))

  attr(out, "posterior_draws") <- t(preds)
  return(out)
}


#' Functions needed for getting data / objects with insight
#' @rdname mvgam_marginaleffects
#' @export
get_data.mvgam = function (x, source = "environment", verbose = TRUE, ...) {

  mf <- tryCatch({
    elements <- c(TRUE, FALSE)
    mf_list <- insight::compact_list(lapply(elements, function(e) {
      model.frame(x, trend_effects = e)
    }))
    mf_data <- mf_list[[1]]
    if (length(mf_list) > 1) {
      for (i in 2:length(mf_list)) {
        cn <- setdiff(colnames(mf_list[[i]]), colnames(mf_data))
        if (length(cn))
          mf_data <- cbind(mf_data, mf_list[[i]][, cn,
                                                 drop = FALSE])
      }
    }
    mf_data
  }, error = function(x) {
    NULL
  })
  insight:::.prepare_get_data(x, mf, effects = "all", verbose = verbose)
}

#' @rdname mvgam_marginaleffects
#' @export
find_predictors.mvgam = function(x, effects = c('fixed',
                                                'random',
                                                'all'),
                                 component = c('all',
                                               'conditional',
                                               'zi',
                                               'zero_inflated',
                                               'dispersion',
                                               'instruments',
                                               'correlation',
                                               'smooth_terms'),
                                 flatten = FALSE,
                                 verbose = TRUE,
                                 ...){
  obs_preds <- insight::find_predictors(x$mgcv_model,
                                        effects = effects,
                                        component = component,
                                        flatten = flatten)
  if(!is.null(x$trend_call)){
    preds <- list()
    trend_preds <- insight::find_predictors(x$trend_mgcv_model,
                                            effects = effects,
                                            component = component,
                                            flatten = flatten)

    if(flatten){
      preds <- unique(c(obs_preds, trend_preds))
    } else {
      preds$conditional <- unique(c(obs_preds$conditional,
                                    trend_preds$conditional))
    }

  } else {
    preds <- obs_preds
  }
  return(preds)
}
