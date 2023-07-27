#'@title Compute pointwise Log-Likelihoods from fitted `mvgam` objects
#'
#'@importFrom parallel setDefaultCluster stopCluster
#'@param object \code{list} object returned from \code{mvgam}
#'@param n_cores \code{integer} specifying number of cores for calculating likelihoods in parallel
#'@param ... Ignored
#'@return A `matrix` of dimension `n_samples x n_observations` containing the pointwise
#'log-likelihood draws for all observations (training observations and, if supplied to the
#'original model via the `newdata` argument in \code{\link{mvgam}},
#'testing observations)
#'@export
logLik.mvgam = function(object, n_cores = 1, ...){

  if(sign(n_cores) != 1){
    stop('argument "n_cores" must be a positive integer',
         call. = FALSE)
  } else {
    if(n_cores%%1 != 0){
      stop('argument "n_cores" must be a positive integer',
           call. = FALSE)
    }
  }

  # Extract the location predictions from the fitted model
  mus <- mcmc_chains(object$model_output, 'mus')

  # Need to know which series each observation belongs to so we can
  # pull out appropriate family-level parameters (overdispersions, shapes, etc...)
  if(is.null(object$test_data)){
    all_dat <- object$obs_data %>%
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
  obs <- all_dat$y
  series_obs <- as.numeric(all_dat$series)

  # Family-specific parameters
  family <- object$family
  family_pars <- extract_family_pars(object = object)
  n_series <- NCOL(object$ytimes)

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
                            Xp = Xp,
                            betas = 1,
                            density = TRUE)

    }))

  }, cl = cl))
  stopCluster(cl)

  return(log_lik_mat)
}
