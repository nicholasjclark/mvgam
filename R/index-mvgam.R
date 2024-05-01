#' Index \code{mvgam} objects
#' @aliases variables
#'
#' Index variables and their `mgcv` coefficient names
#'
#' @param x A \code{mvgam} object or another \R object for which
#' the methods are defined.
#' @param ... Arguments passed to individual methods (if applicable).
#'
#' @name index-mvgam
NULL

#' @rdname index-mvgam
#' @importFrom posterior variables
#' @param x \code{list} object returned from \code{mvgam}. See [mvgam()]
#' @method variables mvgam
#' @return a `list` object of the variables that can be extracted, along
#' with their aliases
#' @examples
#' \dontrun{
#' simdat <- sim_mvgam(n_series = 1, trend_model = 'AR1')
#' mod <- mvgam(y ~ s(season, bs = 'cc', k = 6),
#'              trend_model = AR(),
#'              data = simdat$data_train,
#'             burnin = 300,
#'             samples = 300,
#'             chains = 2)
#' variables(mod)
#' }
#' @export
#' @export variables
variables.mvgam = function(x, ...){
  parnames <- dimnames(x$model_output)$parameters

  # Observation distribution parameters
  if(any(grepl(paste(c('sigma_obs', 'phi', 'nu', 'shape'), collapse = '|'),
       parnames))){
    observation_pars <- data.frame(orig_name = parnames[grepl(paste(c('sigma_obs',
                                                                      'phi', 'nu',
                                                                      'shape'),
                                                                    collapse = '|'),
                                                              parnames)],
                                   alias = NA)
  } else {
    observation_pars <- NULL
  }

  # Linear predictor parameters
  observation_linpreds <- data.frame(orig_name = parnames[grepl('mus[',
                                                                parnames,
                                                                fixed = TRUE)],
                                     alias = NA)

  if(!is.null(x$trend_call)){
    trend_linpreds <- data.frame(orig_name = parnames[grepl('trend_mus[',
                                                                  parnames,
                                                                  fixed = TRUE)],
                                       alias = NA)
  } else {
    trend_linpreds <- NULL
  }

  # Posterior predictions
  posterior_preds <- data.frame(orig_name = parnames[grepl('ypred[',
                                                                parnames,
                                                                fixed = TRUE)],
                                     alias = NA)

  # Beta coefficient parameters
  b_names <- colnames(mcmc_chains(x$model_output, 'b'))
  mgcv_names <- names(coef(x$mgcv_model))
  observation_betas <- data.frame(orig_name = b_names, alias = mgcv_names)

  if(!is.null(x$trend_call)){
    b_names <- colnames(mcmc_chains(x$model_output, 'b_trend'))
    mgcv_names <- gsub('series', 'trend',
                       paste0(names(coef(x$trend_mgcv_model)), '_trend'))
    trend_betas <- data.frame(orig_name = b_names, alias = mgcv_names)
  } else {
    trend_betas <- NULL
  }

  # Population parameters from hierarchical (random) effects
  if(any(unlist(purrr::map(x$mgcv_model$smooth, inherits, 'random.effect')))){
    re_labs <- unlist(lapply(purrr::map(x$mgcv_model$smooth, 'term'),
                             paste, collapse = ','))[
      unlist(purrr::map(x$mgcv_model$smooth, inherits, 'random.effect'))]
    observation_re_params  <- data.frame(orig_name = c(
      rownames(mcmc_summary(x$model_output, 'mu_raw',
                           ISB = TRUE)),
      rownames(mcmc_summary(x$model_output, 'sigma_raw',
                                    ISB = TRUE))),
      alias = c(paste0('mean(',re_labs,')'),
                paste0('sd(',re_labs,')')))
  } else {
    observation_re_params <- NULL
  }

  trend_re_params <- NULL
  if(!is.null(x$trend_call)){
    if(any(unlist(purrr::map(x$trend_mgcv_model$smooth, inherits, 'random.effect')))){
      re_labs <- unlist(lapply(purrr::map(x$trend_mgcv_model$smooth, 'term'),
                               paste, collapse = ','))[
        unlist(purrr::map(x$trend_mgcv_model$smooth, inherits, 'random.effect'))]
      re_labs <- gsub('series', 'trend', re_labs)
      trend_re_params  <- data.frame(orig_name = c(
        rownames(mcmc_summary(x$model_output, 'mu_raw_trend',
                              ISB = TRUE)),
        rownames(mcmc_summary(x$model_output, 'sigma_raw_trend',
                              ISB = TRUE))),
        alias = c(paste0('mean(',re_labs,')_trend'),
                  paste0('sd(',re_labs,')_trend')))
    } else {
      trend_re_params <- NULL
    }
  }

  # Smoothing parameters
  if(any(grepl('rho[', parnames, fixed = TRUE))){
    observation_smoothpars <- data.frame(orig_name = parnames[grepl('rho[',
                                                           parnames,
                                                           fixed = TRUE)],
                                alias = paste0(x$sp_names, '_rho'))
  } else {
    observation_smoothpars <- NULL
  }

  if(any(grepl('rho_trend[', parnames, fixed = TRUE))){
    trend_smoothpars <- data.frame(orig_name = parnames[grepl('rho_trend[',
                                                                    parnames,
                                                                    fixed = TRUE)],
                                         alias = paste0(x$trend_sp_names, '_rho_trend'))
  } else {
    trend_smoothpars <- NULL
  }

  # Trend state parameters
  if(any(grepl('trend[', parnames, fixed = TRUE) &
           !grepl('_trend[', parnames, fixed = TRUE))){
    trend_states <- grepl('trend[', parnames, fixed = TRUE) &
      !grepl('_trend[', parnames, fixed = TRUE)
    trends <- data.frame(orig_name = parnames[trend_states],
                         alias = NA)
  } else {
    trends <- NULL
  }

  # Trend dynamics parameters
  if(any(grepl(paste(c('sigma', 'alpha_gp',
                       'rho_gp',
                       'ar1', 'ar2',
                       'ar3', 'A',
                       'Sigma', 'error', 'theta',
                       'k_trend', 'delta_trend', 'm_trend'), collapse = '|'),
               parnames) &
         !grepl('sigma_obs', parnames, fixed = TRUE) &
         !grepl('sigma_raw', parnames, fixed = TRUE))){
    trend_pars <- grepl(paste(c('sigma', 'alpha_gp',
                                'rho_gp',
                                'ar1', 'ar2',
                                'ar3', 'A',
                                'Sigma', 'error', 'theta',
                                'k_trend', 'delta_trend', 'm_trend'), collapse = '|'),
                        parnames) &
      !grepl('sigma_obs', parnames, fixed = TRUE) &
      !grepl('sigma_raw', parnames, fixed = TRUE)
    trend_pars <- data.frame(orig_name = parnames[trend_pars],
                                   alias = NA)
  } else {
    trend_pars <- NULL
  }

  return(list(observation_pars = observation_pars,
              observation_linpreds = observation_linpreds,
              observation_betas = observation_betas,
              observation_smoothpars = observation_smoothpars,
              observation_re_params = observation_re_params,
              posterior_preds = posterior_preds,
              trend_pars = trend_pars,
              trend_linpreds = trend_linpreds,
              trend_betas = trend_betas,
              trend_smoothpars = trend_smoothpars,
              trend_re_params = trend_re_params,
              trends = trends))
}
