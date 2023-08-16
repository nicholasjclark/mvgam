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
#'@param n_cores \code{integer} specifying number of cores for calculating likelihoods in parallel
#'@param ... Ignored
#'@return A `matrix` of dimension `n_samples x n_observations` containing the pointwise
#'log-likelihood draws for all observations in `newdata`. If no `newdata` is supplied,
#'log-likelihood draws are returned for all observations that were originally fed to
#'the model (training observations and, if supplied to the
#'original model via the `newdata` argument in \code{\link{mvgam}},
#'testing observations)
#'@export
logLik.mvgam = function(object, linpreds, newdata,
                        family_pars, n_cores = 1, ...){

  validate_pos_integer(n_cores)

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

  # Calculate log-likelihoods
  cl <- parallel::makePSOCKcluster(n_cores)
  setDefaultCluster(cl)
  clusterExport(NULL, c('n_series',
                        'series_obs',
                        'obs',
                        'family_pars',
                        'mus'),
                envir = environment())

  clusterExport(cl = cl,
                unclass(lsf.str(envir = asNamespace("mvgam"),
                                all = T)),
                envir = as.environment(asNamespace("mvgam"))
  )

  pbapply::pboptions(type = "none")
  log_lik_mat <- do.call(rbind, pbapply::pblapply(seq_len(dim(mus)[1]),
                                                  function(samp_index){

    # Loop across each observation in obs and calculate the log-likelihood
    liks <- unlist(lapply(seq_along(obs), function(x){

      # Family-specific parameters
      family_extracts <- lapply(seq_along(family_pars), function(j){
        if(is.matrix(family_pars[[j]])){
          family_pars[[j]][samp_index, series_obs[x]]
        } else {
          family_pars[[j]][samp_index]
        }
      })
      names(family_extracts) <- names(family_pars)

      # Log-likelihood
      Xp <- t(as.matrix(mus[samp_index, x]))
      attr(Xp, 'model.offset') <- 0
      mvgam_predict(family = family,
                    family_pars = family_extracts,
                    truth = obs[x],
                    type = 'link',
                    Xp = Xp,
                    betas = 1,
                    density = TRUE)

    }))

  }, cl = cl))
  stopCluster(cl)

  return(log_lik_mat)
}
