#' Widely applicable information criterion for mvgam models
#'
#' Passes the pointwise log likelihood of [log_lik.mvgam()] to
#' [loo::waic()]. [loo.mvgam()] estimates the same ELPD by PSIS and
#' reports Pareto k diagnostics that say when to distrust it. Vehtari
#' et al. (2017) recommend it over WAIC.
#'
#' @inheritParams loo.mvgam
#' @param ... Further arguments to [log_lik.mvgam()], such as `ndraws`.
#' @param compare,resp,pointwise,model_names The arguments of
#'   [brms::waic.brmsfit]. `resp` scopes a multivariate fit to one
#'   response. Supplying one of the others raises an error.
#'   [loo_compare()] ranks several `waic()` results.
#'
#' @return A `waic` object (see [loo::waic()]), or with
#'   `by_series = TRUE` a data frame of per-series estimates.
#'
#' @seealso [loo.mvgam()] estimates the same ELPD by PSIS.
#'   [lfo_cv.mvgam()] scores forecasts from rolling origins. The
#'   `r mvgam_article("forecast_evaluation", "forecast evaluation article")`
#'   compares models by approximate and exact cross-validation.
#'
#' @references
#' Vehtari, A., Gelman, A. and Gabry, J. (2017). Practical
#' Bayesian model evaluation using leave-one-out cross-validation
#' and WAIC. \emph{Statistics and Computing}, 27:1413-1432.
#' \doi{10.1007/s11222-016-9696-4}
#'
#' @examples
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR())
#'
#' mod <- mvgam(y ~ s(x),
#'               trend_formula = ~ AR(p = 1),
#'               data    = simdat$data_train,
#'               family  = poisson(),
#'               chains  = 2, silent = 2)
#'
#' # The WAIC estimate of the ELPD, with its effective number of
#' # parameters p_waic
#' waic(mod)
#'
#' # The PSIS estimate of the same ELPD. Its Pareto k diagnostics mark
#' # the observations it estimates poorly
#' loo(mod)
#'
#' # Next, loo_compare() ranks several models by either estimate
#' }
#'
#' @family model comparison
#' @method waic mvgam
#' @export
waic.mvgam <- function(x, ..., compare = TRUE, resp = NULL,
                       pointwise = FALSE, model_names = NULL,
                       incl_autocor = TRUE,
                       incl_dynamics = NULL,
                       by_series = FALSE) {
  require_fitted_model(x, "waic")
  incl_autocor <- resolve_incl_autocor(
    incl_autocor = incl_autocor,
    legacy = incl_dynamics,
    autocor_supplied = !missing(incl_autocor)
  )
  refuse_unsupported_ic_args(c(
    pointwise = isTRUE(pointwise),
    compare = !missing(compare),
    model_names = !is.null(model_names)
  ), "waic")
  logliks <- log_lik(
    x, resp = resp, incl_autocor = incl_autocor, ...
  )
  logliks <- clean_ll(logliks)
  if (isTRUE(by_series)) {
    return(per_series_ic(x, logliks, criterion = "waic"))
  }
  loo::waic(logliks)
}

#' @importFrom loo waic
#' @export
loo::waic
