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
#' @param incl_dynamics Logical, default `FALSE`. Maps to the
#'   `process_error` argument on [log_lik.mvgam()].
#'
#' @return A `loo::waic` object.
#'
#' @seealso [loo.mvgam()], [log_lik.mvgam()].
#'
#' @references
#' Vehtari, A., Gelman, A. and Gabry, J. (2017). Practical
#' Bayesian model evaluation using leave-one-out cross-validation
#' and WAIC. \emph{Statistics and Computing}, 27:1413-1432.
#' \doi{10.1007/s11222-016-9696-4}
#'
#' @method waic mvgam
#' @export
waic.mvgam <- function(x, ..., compare = TRUE, resp = NULL,
                       pointwise = FALSE, model_names = NULL,
                       incl_dynamics = FALSE) {
  if (isTRUE(pointwise)) {
    stop(insight::format_error(c(
      "{.field pointwise = TRUE} is not yet supported on mvgam waic.",
      i = "Compute WAIC in-memory by leaving {.field pointwise = FALSE}."
    )))
  }
  loo::waic(log_lik(x, resp = resp, process_error = incl_dynamics, ...))
}

#' @importFrom loo waic
#' @export
loo::waic
