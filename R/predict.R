#' Draws from the Posterior Predictive Distribution
#'
#' Compute posterior predictions for mvgam models. This method is a
#'   convenience wrapper around [posterior_predict.mvgam()] that
#'   optionally returns summary statistics instead of raw draws.
#'
#' @param object An object of class `mvgam`.
#' @param newdata An optional data.frame containing new predictor values.
#'   If `NULL` (the default), the original training data is used.
#' @param process_error Logical. If `TRUE` (the default), trend uncertainty
#'   is included in predictions. If `FALSE`, predictions use the
#'   posterior mean of trend parameters, reducing uncertainty.
#' @param ndraws Positive integer specifying the number of posterior draws
#'   to use. If `NULL` (the default), all draws are used.
#' @param draw_ids Integer vector specifying which draws to use. If `NULL`,
#'   draws are selected based on `ndraws`.
#' @param re_formula Formula for random effects. If `NULL` (the default),
#'   all random effects are included. Use `NA` to exclude all random
#'   effects.
#' @param allow_new_levels Logical. If `TRUE`, allows predictions for new
#'   factor levels not seen during training. Default is `FALSE`.
#' @param sample_new_levels Character specifying how to sample new levels.
#'   Either `"uncertainty"` (default), `"gaussian"`, or `"old_levels"`.
#' @param resp Character specifying which response variable to predict for
#'   multivariate models. If `NULL`, predictions are returned for all
#'   responses.
#' @param summary Logical. If `TRUE` (the default), returns summary
#'   statistics. If `FALSE`, returns the full matrix of posterior draws.
#' @param robust Logical. If `FALSE` (the default), uses mean and standard
#'   deviation for summaries. If `TRUE`, uses median and median absolute
#'   deviation (MAD).
#' @param probs Numeric vector of probabilities for quantile computation.
#'   Default is `c(0.025, 0.975)` for 95% credible intervals. Can be a
#'   single value or multiple values.
#' @param ... Additional arguments passed to [posterior_predict.mvgam()].
#'
#' @return If `summary = FALSE`, returns a matrix of posterior draws with
#'   dimensions `[ndraws x nobs]`.
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
#' @seealso [posterior_predict.mvgam()], [posterior_epred.mvgam()],
#'   [posterior_linpred.mvgam()], [fitted.mvgam()]
#'
#' @examples
#' \dontrun{
#' # Fit a model
#' fit <- mvgam(count ~ s(time), data = data, family = poisson())
#'
#' # Get summary predictions (default)
#' pred <- predict(fit)
#' head(pred)
#'
#' # Get raw posterior draws
#' draws <- predict(fit, summary = FALSE)
#' dim(draws)
#'
#' # Predictions without trend uncertainty
#' pred_fixed <- predict(fit, process_error = FALSE)
#' }
#'
#' @export
predict.mvgam <- function(object,
                          newdata = NULL,
                          process_error = TRUE,
                          ndraws = NULL,
                          draw_ids = NULL,
                          re_formula = NULL,
                          allow_new_levels = FALSE,
                          sample_new_levels = "uncertainty",
                          resp = NULL,
                          summary = TRUE,
                          robust = FALSE,
                          probs = c(0.025, 0.975),
                          ...) {

  # Input validation
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_data_frame(newdata, null.ok = TRUE)
  checkmate::assert_logical(process_error, len = 1, any.missing = FALSE)
  checkmate::assert_int(ndraws, lower = 1, null.ok = TRUE)
  checkmate::assert_integerish(draw_ids, lower = 1, null.ok = TRUE)
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
    probs,
    lower = 0,
    upper = 1,
    min.len = 1,
    any.missing = FALSE
  )

  # Get posterior predictive draws
  pp_draws <- posterior_predict(
    object,
    newdata = newdata,
    process_error = process_error,
    ndraws = ndraws,
    draw_ids = draw_ids,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels,
    resp = resp,
    ...
  )

  # Return raw draws if summary not requested
  if (!summary) {
    return(pp_draws)
  }

  # Compute summary statistics
  summarize_predictions(pp_draws, probs = probs, robust = robust)
}


#' Summarize Posterior Prediction Draws
#'
#' Computes summary statistics from a matrix of posterior draws following
#'   the brms output format.
#'
#' @param draws Matrix of posterior draws with dimensions `[ndraws x nobs]`.
#' @param probs Numeric vector of probabilities for quantile computation.
#' @param robust Logical. If `FALSE`, uses mean and sd. If `TRUE`, uses
#'   median and mad.
#'
#' @return Matrix with columns for point estimate, uncertainty, and
#'   quantiles.
#'
#' @noRd
summarize_predictions <- function(draws, probs, robust) {
  checkmate::assert_matrix(
    draws,
    mode = "numeric",
    min.rows = 1,
    min.cols = 1
  )
  checkmate::assert_numeric(probs, lower = 0, upper = 1, min.len = 1)
  checkmate::assert_logical(robust, len = 1)

  # Compute point estimates and uncertainty
  if (robust) {
    estimate <- apply(draws, 2, stats::median)
    est_error <- apply(draws, 2, stats::mad)
  } else {
    estimate <- colMeans(draws)
    est_error <- apply(draws, 2, stats::sd)
  }

  # Compute quantiles
  quantiles <- apply(draws, 2, stats::quantile, probs = probs)

  # Reshape vector to matrix when single quantile requested
  if (length(probs) == 1) {
    quantiles <- matrix(quantiles, nrow = 1)
  }

  # Create quantile column names following brms convention (Q2.5, Q97.5)
  quantile_names <- paste0("Q", probs * 100)

  # Combine into output matrix
  out <- cbind(
    Estimate = estimate,
    Est.Error = est_error,
    t(quantiles)
  )
  colnames(out)[3:ncol(out)] <- quantile_names

  out
}
