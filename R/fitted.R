#' Expected Values from the Posterior Distribution
#'
#' Compute fitted values for an mvgam model, returning the expected
#' value E\[Y | X\] (or the linear predictor, with `scale = "linear"`)
#' at the training observations by default. Mirrors the brms
#' [`fitted.brmsfit()`][brms::fitted.brmsfit] interface as closely as
#' the underlying mvgam posterior methods permit.
#'
#' @param object An object of class `mvgam`.
#' @param newdata An optional data.frame of new predictor values. If
#'   `NULL` (the default), the training data stored in `object` is
#'   used. This is what makes the call "fitted" rather than a general
#'   prediction.
#' @param re_formula Formula for random effects. If `NULL` (the
#'   default), all random effects are included. Use `NA` to exclude
#'   all random effects.
#' @param scale Character; one of `"response"` (default) or `"linear"`.
#'   `"response"` returns expected values on the response scale via
#'   [posterior_epred.mvgam()]. `"linear"` returns the linear predictor
#'   on the link scale via [posterior_linpred.mvgam()].
#' @param resp Character specifying which response variable for
#'   multivariate models. If `NULL`, fitted values are returned for
#'   all responses (a named list).
#' @param ndraws Positive integer specifying the number of posterior
#'   draws to use. If `NULL` (the default), all draws are used.
#' @param summary Logical. If `TRUE` (the default), returns summary
#'   statistics. If `FALSE`, returns the full matrix of posterior
#'   draws.
#' @param robust Logical. If `FALSE` (the default), uses mean and
#'   standard deviation for summaries. If `TRUE`, uses median and
#'   median absolute deviation (MAD).
#' @param probs Numeric vector of probabilities for quantile
#'   computation. Default is `c(0.025, 0.975)` for 95% credible
#'   intervals.
#' @param process_error Logical. If `TRUE` (the default), uses the
#'   full posterior draws of trend parameters. If `FALSE`, fixes the
#'   trend at its posterior mean for faster computation. Stochastic
#'   process innovations are not added here (only by
#'   [posterior_predict.mvgam()]); fitted values stay deterministic
#'   functions of the parameter draws.
#' @param allow_new_levels Logical. If `TRUE`, allows predictions for
#'   new factor levels not seen during training. Default is `FALSE`.
#' @param sample_new_levels Character specifying how to sample new
#'   levels. Either `"uncertainty"` (default), `"gaussian"`, or
#'   `"old_levels"`.
#' @param ... Additional arguments passed to the underlying posterior
#'   methods.
#'
#' @return If `summary = FALSE`, returns a matrix of posterior draws
#'   with dimensions `[ndraws x nobs]`.
#'
#'   If `summary = TRUE`, returns a matrix with columns:
#'   \itemize{
#'     \item `Estimate`: Point estimate (mean or median depending on
#'       `robust`)
#'     \item `Est.Error`: Uncertainty estimate (sd or mad depending on
#'       `robust`)
#'     \item `Q*`: Quantile columns corresponding to values in `probs`
#'   }
#'
#'   For multivariate models with `resp = NULL`, returns a named list
#'   of matrices (one per response variable) — each summarised
#'   independently when `summary = TRUE`.
#'
#' @details
#' `fitted.mvgam()` follows the brms convention: it returns the
#' expected value E\[Y | X\] (or the linear predictor with `scale =
#' "linear"`) and does **not** include observation-level noise or the
#' state-space process noise that [posterior_predict.mvgam()] adds.
#' Use `predict()` if you want predictive samples that include those
#' noise components.
#'
#' Two brms `fitted()` arguments are not yet supported and will be
#' ignored if passed via `...`:
#' \itemize{
#'   \item `dpar` / `nlpar`: distributional and non-linear parameter
#'     selection (waiting on broader distributional regression
#'     support in mvgam).
#'   \item `draw_ids` / `sort`: draw subsetting by index and series
#'     sorting are not yet plumbed through the underlying mvgam
#'     posterior methods. Use `ndraws` for subsetting.
#' }
#'
#' @seealso [posterior_epred.mvgam()], [posterior_linpred.mvgam()],
#'   [posterior_predict.mvgam()], [predict.mvgam()],
#'   [`brms::fitted.brmsfit()`][brms::fitted.brmsfit].
#'
#' @examples
#' \dontrun{
#' fit <- mvgam(count ~ s(time), data = data, family = poisson())
#'
#' # Default: fitted values on the response scale, summarised
#' fv <- fitted(fit)
#' head(fv)
#'
#' # Linear-scale (link) fitted values
#' fv_link <- fitted(fit, scale = "linear")
#'
#' # Raw posterior draws of fitted values
#' draws <- fitted(fit, summary = FALSE)
#' dim(draws)
#' }
#'
#' @method fitted mvgam
#' @export
fitted.mvgam <- function(object,
                         newdata = NULL,
                         re_formula = NULL,
                         scale = c("response", "linear"),
                         resp = NULL,
                         ndraws = NULL,
                         summary = TRUE,
                         robust = FALSE,
                         probs = c(0.025, 0.975),
                         process_error = TRUE,
                         allow_new_levels = FALSE,
                         sample_new_levels = "uncertainty",
                         ...) {
  checkmate::assert_class(object, "mvgam")
  scale <- match.arg(scale)
  checkmate::assert_data_frame(newdata, null.ok = TRUE)
  checkmate::assert_logical(process_error, len = 1, any.missing = FALSE)
  checkmate::assert_int(ndraws, lower = 1, null.ok = TRUE)
  checkmate::assert(
    checkmate::check_class(re_formula, "formula"),
    checkmate::check_true(is.na(re_formula)),
    checkmate::check_null(re_formula)
  )
  checkmate::assert_logical(allow_new_levels, len = 1, any.missing = FALSE)
  checkmate::assert_choice(
    sample_new_levels,
    choices = c("uncertainty", "gaussian", "old_levels")
  )
  checkmate::assert_string(resp, null.ok = TRUE)
  checkmate::assert_logical(summary, len = 1, any.missing = FALSE)
  checkmate::assert_logical(robust, len = 1, any.missing = FALSE)
  checkmate::assert_numeric(
    probs, lower = 0, upper = 1, min.len = 1, any.missing = FALSE
  )

  posterior_call <- if (scale == "response") {
    posterior_epred
  } else {
    posterior_linpred
  }
  draws <- posterior_call(
    object,
    newdata = newdata,
    process_error = process_error,
    ndraws = ndraws,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels,
    resp = resp,
    ...
  )

  if (!summary) {
    return(draws)
  }

  if (is.list(draws) && !is.matrix(draws)) {
    return(lapply(draws, summarize_predictions, probs = probs, robust = robust))
  }
  summarize_predictions(draws, probs = probs, robust = robust)
}
