#'@title Compute pointwise Log-Likelihoods from fitted `mvgam` objects
#'
#'@importFrom parallel setDefaultCluster stopCluster
#'@param object \code{list} object returned from \code{mvgam}
#'@param linpreds Optional `matrix` of linear predictor draws to use for calculating
#'poitwise log-likelihoods
#'@param newdata Optional `data.frame` of `list` object specifying which series each column
#'in `linpreds` belongs to. If `linpreds` is supplied, then `newdata` must also be supplied
#'@param family_pars Optional `list` containing posterior draws of
#'family-specific parameters (i.e. shape, scale or overdispersion parameters). Required if
#'`linpreds` and `newdata` are supplied
#'@param ... Ignored
#'@return A `matrix` of dimension `n_samples x n_observations` containing the pointwise
#'log-likelihood draws for all observations in `newdata`. If no `newdata` is supplied,
#'log-likelihood draws are returned for all observations that were originally fed to
#'the model (training observations and, if supplied to the
#'original model via the `newdata` argument in \code{\link{mvgam}},
#'testing observations)
#'@export
logLik.mvgam = function(object, linpreds, newdata,
                        family_pars, ...){

  if(!missing(linpreds) & missing(newdata)){
    stop('argument "newdata" must be supplied when "linpreds" is supplied')
  }

  if(!missing(linpreds) & missing(family_pars)){
    stop('argument "family_pars" must be supplied when "linpreds" is supplied')
  }

  if(!missing(newdata) & missing(linpreds)){
    stop('argument "linpreds" must be supplied when "newdata" is supplied')
  }

  if(!missing(family_pars) & missing(linpreds)){
    stop('argument "linpreds" must be supplied when "family_pars" is supplied')
  }

  # Extract the linear predictor draws
  if(missing(linpreds)){
    mus <- mcmc_chains(object$model_output, 'mus')
  } else {
    mus <- linpreds
  }

  # Need to know which series each observation belongs to so we can
  # pull out appropriate family-level parameters (overdispersions, shapes, etc...)
  if(!missing(newdata)){
    all_dat <- data.frame(series = newdata$series,
                          y = newdata$y)
  } else {
    if(is.null(object$test_data)){
      all_dat <- data.frame(series = object$obs_data$series,
                            time = object$obs_data$time,
                            y = object$obs_data$y) %>%
        dplyr::arrange(time, series)
    } else {
      all_dat <- data.frame(series = c(object$obs_data$series,
                                       object$test_data$series),
                            time = c(object$obs_data$time,
                                     object$test_data$time),
                            y = c(object$obs_data$y,
                                  object$test_data$y)) %>%
        dplyr::arrange(time, series)
    }
  }

  obs <- all_dat$y
  series_obs <- as.numeric(all_dat$series)

  # Family-specific parameters
  family <- object$family

  if(missing(family_pars)){
    family_pars <- extract_family_pars(object = object)
    n_series <- NCOL(object$ytimes)
  } else {
    n_series <- length(object$series_names)
  }

  # Family parameters spread into a vector
  family_extracts <- lapply(seq_along(family_pars), function(j){
    if(is.matrix(family_pars[[j]])){
      as.vector(family_pars[[j]][, series_obs])
    } else {
      family_pars[[j]][]
    }
  })
  names(family_extracts) <- names(family_pars)

  # Create a truth matrix that can also be spread to a vector
  truth_mat <- matrix(rep(obs, NROW(mus)),
                      nrow = NROW(mus),
                      byrow = TRUE)

  # Log-likelihood as a vector
  Xp <- as.matrix(as.vector(mus))
  attr(Xp, 'model.offset') <- 0
  log_lik_vec <- mvgam_predict(family = family,
                               family_pars = family_extracts,
                               truth = as.vector(truth_mat),
                               type = 'link',
                               Xp = Xp,
                               betas = 1,
                               density = TRUE)

  # Convert back to matrix and return
  log_lik_mat <- matrix(log_lik_vec, nrow = NROW(mus))
  return(log_lik_mat)
}
