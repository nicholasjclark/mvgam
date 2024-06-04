#'@title Compute pointwise Log-Likelihoods from fitted `mvgam` objects
#'
#'@importFrom parallel setDefaultCluster stopCluster
#'@param object \code{list} object returned from \code{mvgam}
#'@param linpreds Optional `matrix` of linear predictor draws to use for calculating
#'pointwise log-likelihoods
#'@param newdata Optional `data.frame` or `list` object specifying which series each column
#'in `linpreds` belongs to. If `linpreds` is supplied, then `newdata` must also be supplied
#'@param family_pars Optional `list` containing posterior draws of
#'family-specific parameters (i.e. shape, scale or overdispersion parameters). Required if
#'`linpreds` and `newdata` are supplied
#'@param include_forecast Logical. If `newdata` were fed to the model to compute
#'forecasts, should the log-likelihood draws for these observations also be returned.
#'Defaults to `TRUE`
#'@param ... Ignored
#'@return A `matrix` of dimension `n_samples x n_observations` containing the pointwise
#'log-likelihood draws for all observations in `newdata`. If no `newdata` is supplied,
#'log-likelihood draws are returned for all observations that were originally fed to
#'the model (training observations and, if supplied to the
#'original model via the `newdata` argument in \code{\link{mvgam}},
#'testing observations)
#' @examples
#' \dontrun{
#' # Simulate some data and fit a model
#' simdat <- sim_mvgam(n_series = 1, trend_model = 'AR1')
#' mod <- mvgam(y ~ s(season, bs = 'cc', k = 6),
#'              trend_model = AR(),
#'              data = simdat$data_train,
#'              burnin = 300,
#'              samples = 300,
#'              chains = 2)
#'
#'# Extract logLikelihood values
#'lls <- logLik(mod)
#'str(lls)
#'}
#'@export
logLik.mvgam = function(object,
                        linpreds,
                        newdata,
                        family_pars,
                        include_forecast = TRUE,
                        ...){

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

    if(object$family == 'nmix'){
      all_dat$cap <- newdata$cap
    }

  } else {
    if(is.null(object$test_data)){
      all_dat <- data.frame(series = object$obs_data$series,
                            time = object$obs_data$time,
                            y = object$obs_data$y) %>%
        dplyr::arrange(time, series)

      if(object$family == 'nmix'){
        all_dat$cap <- data.frame(series = object$obs_data$series,
                                  time = object$obs_data$time,
                                  cap = object$obs_data$cap) %>%
          dplyr::select(series, time, cap) %>%
          dplyr::arrange(time, series) %>%
          dplyr::pull(cap)
      }

    } else {
      all_dat <- data.frame(series = c(object$obs_data$series,
                                       object$test_data$series),
                            time = c(object$obs_data$time,
                                     object$test_data$time),
                            y = c(object$obs_data$y,
                                  object$test_data$y)) %>%
        dplyr::arrange(time, series)

      if(object$family == 'nmix'){
        all_dat$cap <- data.frame(series =  c(object$obs_data$series,
                                              object$test_data$series),
                                  time = c(object$obs_data$time,
                                           object$test_data$time),
                                  cap = c(object$obs_data$cap,
                                          object$test_data$cap)) %>%
          dplyr::select(series, time, cap) %>%
          dplyr::arrange(time, series) %>%
          dplyr::pull(cap)
      }

    }
  }

  obs <- all_dat$y
  series_obs <- as.numeric(all_dat$series)

  # Supply forecast NAs if include_forecast is FALSE
  if(!is.null(object$test_data) & !include_forecast & missing(newdata)){
    n_fc_obs <- length(object$test_data$y)
    n_obs <- length(obs)
    obs[((n_obs - n_fc_obs) + 1):n_obs] <- NA
  }

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
      as.vector(matrix(rep(family_pars[[j]],
                           NCOL(mus)),
                       nrow = NROW(mus),
                       byrow = FALSE))
    }
  })
  names(family_extracts) <- names(family_pars)

  # Add trial information if this is a Binomial model
  if(object$family %in% c('binomial', 'beta_binomial')){
    trials <- as.vector(matrix(rep(as.vector(attr(object$mgcv_model, 'trials')),
                                   NROW(mus)),
                               nrow = NROW(mus),
                               byrow = TRUE))
    family_extracts$trials <- trials
  }

  # Create a truth matrix that can also be spread to a vector
  truth_mat <- matrix(rep(obs, NROW(mus)),
                      nrow = NROW(mus),
                      byrow = TRUE)

  # Log-likelihood as a vector
  Xp <- as.matrix(as.vector(mus))
  attr(Xp, 'model.offset') <- 0

  if(family == 'nmix'){
    Xp <- as.matrix(qlogis(as.vector(mcmc_chains(object$model_output, 'detprob'))))
    attr(Xp, 'model.offset') <- 0
    latent_lambdas <- exp(as.vector(mcmc_chains(object$model_output, 'trend')))
    cap_mat <- matrix(rep(all_dat$cap, NROW(mus)),
                        nrow = NROW(mus),
                        byrow = TRUE)
    cap <- as.vector(cap_mat)
  } else {
    latent_lambdas <- NULL
    cap <- NULL
  }
  log_lik_vec <- mvgam_predict(family = family,
                               family_pars = family_extracts,
                               truth = as.vector(truth_mat),
                               latent_lambdas = latent_lambdas,
                               cap = cap,
                               type = 'link',
                               Xp = Xp,
                               betas = 1,
                               density = TRUE)

  # Convert back to matrix and return
  log_lik_mat <- matrix(log_lik_vec, nrow = NROW(mus))
  return(log_lik_mat)
}
