#' Widely Applicable Information Criterion for mvgam models
#'
#' Compute the WAIC by passing pointwise log-likelihoods from
#' [log_lik.mvgam()] to [loo::waic()]. Provided for brms parity; the
#' [loo.mvgam()] method (LOO-PSIS) is usually preferred for posterior
#' model comparison.
#'
#' @param x An mvgam fit.
#' @param ... Additional arguments passed to [log_lik.mvgam()] (e.g.
#'   `newdata`, `ndraws`, `draw_ids`).
#' @param compare,resp,pointwise,model_names Accepted for
#'   [brms::waic.brmsfit] parity. `resp` is passed through to
#'   [log_lik.mvgam()] for multivariate response selection;
#'   `pointwise` is not yet supported and raises a clear error;
#'   `compare` and `model_names` are no-ops for single-model WAIC.
#' @param incl_autocor Logical, default `TRUE`. Passed to
#'   [log_lik.mvgam()] as its argument of the same name, so each
#'   observation is scored on the conditional surface, under the latent
#'   trend state the model inferred at that time.
#' @param incl_dynamics Superseded by `incl_autocor` and still
#'   accepted, so calls written against it keep working. `TRUE` maps
#'   to `incl_autocor = TRUE` and `FALSE` to `incl_autocor = FALSE`.
#'   When only `incl_dynamics` is given it decides; when both are
#'   given `incl_autocor` decides and `incl_dynamics` is ignored.
#' @param by_series Logical, default `FALSE`. When `TRUE`, return a
#'   data frame with one row per series (`series` column) and per-
#'   series WAIC estimates instead of a single `loo::waic` object.
#'   Matches the `spOccupancy::waicOcc(by.sp = TRUE)` workflow for
#'   ranking per-series fit in joint-species distribution models.
#'   See [loo.mvgam()] for the column-to-series mapping convention.
#'
#' @return A `loo::waic` object.
#'
#' @seealso [loo.mvgam()], [log_lik.mvgam()],
#'   [lfo_cv.mvgam()] for rolling-time CV,
#'   [kfold.mvgam()] for grouped k-fold CV with selective refit.
#'
#' @references
#' Vehtari, A., Gelman, A. and Gabry, J. (2017). Practical
#' Bayesian model evaluation using leave-one-out cross-validation
#' and WAIC. \emph{Statistics and Computing}, 27:1413-1432.
#' \doi{10.1007/s11222-016-9696-4}
#'
#' @examples
#' \donttest{
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
#' waic(mod)
#' }
#'
#' @method waic mvgam
#' @export
waic.mvgam <- function(x, ..., compare = TRUE, resp = NULL,
                       pointwise = FALSE, model_names = NULL,
                       incl_autocor = TRUE,
                       incl_dynamics = NULL,
                       by_series = FALSE) {
  incl_autocor <- resolve_incl_autocor(
    incl_autocor = incl_autocor,
    legacy = incl_dynamics,
    autocor_supplied = !missing(incl_autocor)
  )
  if (isTRUE(pointwise)) {
    stop(insight::format_error(c(
      "{.field pointwise = TRUE} is not yet supported on mvgam waic.",
      i = "Compute WAIC in-memory by leaving {.field pointwise = FALSE}."
    )))
  }
  logliks <- log_lik(
    x, resp = resp, incl_autocor = incl_autocor, ...
  )
  # Drop the all-NA columns a missing response leaves behind, the same
  # way loo.mvgam does, so both criteria score the same observations.
  logliks <- clean_ll(x, logliks)
  if (isTRUE(by_series)) {
    return(per_series_ic(x, logliks, criterion = "waic"))
  }
  loo::waic(logliks)
}

#' @importFrom loo waic
#' @export
loo::waic
