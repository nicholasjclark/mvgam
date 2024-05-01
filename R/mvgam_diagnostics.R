#' Extract diagnostic quantities of \pkg{mvgam} models
#'
#' Extract quantities that can be used to diagnose sampling behavior
#' of the algorithms applied by \pkg{Stan} at the back-end of \pkg{mvgam}.
#'
#' @name mvgam_diagnostics
#' @aliases nuts_params rhat neff_ratio
#'
#' @param object,x A \code{mvgam} object.
#' @param pars An optional character vector of parameter names.
#'   For \code{nuts_params} these will be NUTS sampler parameter
#'   names rather than model parameters. If pars is omitted
#'   all parameters are included.
#' @param ... Arguments passed to individual methods.
#'
#' @return The exact form of the output depends on the method.
#' @examples
#' \dontrun{
#' simdat <- sim_mvgam(n_series = 1, trend_model = 'AR1')
#' mod <- mvgam(y ~ s(season, bs = 'cc', k = 6),
#'             trend_model = AR(),
#'             data = simdat$data_train,
#'             burnin = 300,
#'             samples = 300,
#'             chains = 2)
#' np <- nuts_params(mod)
#' head(np)
#'
#' # extract the number of divergence transitions
#' sum(subset(np, Parameter == "divergent__")$Value)
#'
#' head(neff_ratio(mod))
#' }
#' @details For more details see
#'   \code{\link[bayesplot:bayesplot-extractors]{bayesplot-extractors}}.
#'
NULL

#' @rdname mvgam_diagnostics
#' @importFrom bayesplot nuts_params
#' @export nuts_params
#' @export
nuts_params.mvgam <- function(object, pars = NULL, ...) {
  bayesplot::nuts_params(object$model_output, pars = pars, ...)
}

#' @rdname mvgam_diagnostics
#' @importFrom bayesplot log_posterior
#' @export
log_posterior.mvgam <- function(object, ...) {
  bayesplot::log_posterior(object$model_output, ...)
}

#' @rdname mvgam_diagnostics
#' @importFrom posterior rhat
#' @export
rhat.mvgam <- function(x, pars = NULL, ...) {
  # bayesplot uses outdated rhat code from rstan
  # bayesplot::rhat(object$fit, pars = pars, ...)
  if(is.null(pars)){
    vars_extract <- variables(x)
    draws <- as_draws_array(x,
                            variable = unlist(purrr::map(vars_extract, 'orig_name')),
                            use_alias = FALSE)
  } else {
    draws <- as_draws_array(x,
                            variable = pars)
  }

  tmp <- posterior::summarise_draws(draws, rhat = posterior::rhat)
  rhat <- tmp$rhat
  names(rhat) <- tmp$variable
  rhat
}

#' @rdname mvgam_diagnostics
#' @importFrom bayesplot neff_ratio
#' @importFrom brms ndraws
#' @export neff_ratio
#' @export
neff_ratio.mvgam <- function(object, pars = NULL, ...) {
  # bayesplot uses outdated ess code from rstan
  # bayesplot::neff_ratio(object$fit, pars = pars, ...)
  if(is.null(pars)){
    vars_extract <- unlist(purrr::map(variables(object), 'orig_name'))
    vars_extract <- vars_extract[-grep('ypred', vars_extract)]
    draws <- as_draws_array(object,
                            variable = vars_extract,
                            use_alias = FALSE)
  } else {
    draws <- as_draws_array(object,
                            variable = pars)
  }
  tmp <- posterior::summarise_draws(
    draws, ess_bulk = posterior::ess_bulk, ess_tail = posterior::ess_tail
  )
  # min of ess_bulk and ess_tail mimics definition of posterior::rhat.default
  ess <- matrixStats::rowMins(cbind(tmp$ess_bulk, tmp$ess_tail))
  names(ess) <- tmp$variable
  ess / brms::ndraws(draws)
}

#' @rdname mvgam_diagnostics
#' @importFrom bayesplot neff_ratio
#' @export neff_ratio
#' @export
neff_ratio.mvgam <- function(object, pars = NULL, ...) {
  # bayesplot uses outdated ess code from rstan
  # bayesplot::neff_ratio(object$fit, pars = pars, ...)
  if(is.null(pars)){
    vars_extract <- unlist(purrr::map(variables(object), 'orig_name'))
    vars_extract <- vars_extract[-grep('ypred', vars_extract)]
    draws <- as_draws_array(object,
                            variable = vars_extract,
                            use_alias = FALSE)
  } else {
    draws <- as_draws_array(object,
                            variable = pars)
  }
  tmp <- posterior::summarise_draws(
    draws, ess_bulk = posterior::ess_bulk, ess_tail = posterior::ess_tail
  )
  # min of ess_bulk and ess_tail mimics definition of posterior::rhat.default
  ess <- matrixStats::rowMins(cbind(tmp$ess_bulk, tmp$ess_tail))
  names(ess) <- tmp$variable
  ess / ndraws(draws)
}
