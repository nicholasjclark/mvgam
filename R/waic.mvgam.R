#' Widely Applicable Information Criterion for mvgam models
#'
#' Compute the WAIC by passing pointwise log-likelihoods from
#' [log_lik.mvgam()] to [loo::waic()]. Provided for brms parity; the
#' [loo.mvgam()] method (LOO-PSIS) is usually preferred for posterior
#' model comparison.
#'
#' @param x An mvgam fit.
#' @param ... Additional arguments passed to [log_lik.mvgam()] (e.g.
#'   `newdata`, `process_error`, `ndraws`, `draw_ids`).
#' @param compare,resp,pointwise,model_names Accepted for brms-parity but
#'   currently ignored or passed through; multi-response WAIC and model
#'   comparison are routed through [loo_compare.mvgam()] / `loo::waic` as
#'   appropriate.
#'
#' @return A `loo::waic` object.
#'
#' @seealso [loo.mvgam()], [log_lik.mvgam()].
#'
#' @method waic mvgam
#' @export
waic.mvgam <- function(x, ..., compare = TRUE, resp = NULL,
                       pointwise = FALSE, model_names = NULL) {
  loo::waic(log_lik(x, resp = resp, ...))
}

#' @importFrom loo waic
#' @export
loo::waic
