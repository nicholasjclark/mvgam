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
#'   on the link scale via [posterior_linpred.mvgam()]. Ignored when
#'   `components` is not `"response"`.
#' @param components Character; one of `"response"` (default),
#'   `"latent_state"`, or `"detection"`. Selects which posterior
#'   component to return.
#'
#'   * `"response"` keeps the brms-default behaviour: marginal
#'     expected values at the per-row grain, scaled via `scale`.
#'   * `"latent_state"` (closure-unit families only) routes through
#'     [predict.mvgam()] with `type = "latent_state"`, returning the
#'     conditional latent state per closure unit (`P(z = 1 | y)` for
#'     `occ()`, posterior `N` for `nmix()` variants).
#'   * `"detection"` (closure-unit families only) routes through
#'     [predict.mvgam()] with `type = "detection"`, returning per-
#'     visit detection probabilities.
#'
#'   Modelled on the `flocker::fitted_flocker(components = ...)`
#'   argument so flocker users have a one-call equivalent.
#' @param unit_level Logical or `NULL` (default). When `TRUE` for a
#'   closure-unit detection family and `components = "response"`,
#'   aggregates the per-visit response matrix to the per-unit grain
#'   (sum across visits within a unit) via the shared
#'   `aggregate_closure_unit_visits()` helper. Ignored when
#'   `components != "response"` (those grains are fixed by the
#'   component definition) and when the family is not a
#'   visit-aggregating closure-unit family.
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
#' \donttest{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR())
#' mod <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1),
#'               data    = simdat$data_train,
#'               family  = poisson(),
#'               chains  = 2, silent = 2)
#'
#' # Summary matrix of fitted values on the response scale.
#' ft <- fitted(mod)
#' head(ft)
#'
#' # Per-cell residuals on the response scale (observed minus
#' # posterior mean). Use `pp_check(mod, type = "resid_ribbon")`
#' # for a graphical view that respects the model family.
#' resids <- simdat$data_train$y - ft[, "Estimate"]
#' summary(resids)
#' }
#'
#' @method fitted mvgam
#' @export
fitted.mvgam <- function(object,
                         newdata = NULL,
                         re_formula = NULL,
                         scale = c("response", "linear"),
                         components = c("response", "latent_state",
                                         "detection"),
                         unit_level = NULL,
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
  components <- match.arg(components)
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
  checkmate::assert_logical(unit_level, len = 1, null.ok = TRUE,
                              any.missing = FALSE)
  checkmate::assert_numeric(
    probs, lower = 0, upper = 1, min.len = 1, any.missing = FALSE
  )

  # `latent_state` / `detection` components delegate to predict()
  # which already runs the family-availability gate and dispatches
  # to the per-family kernel (posterior_latent_N / posterior_occupancy
  # / posterior_detection). `unit_level` is a no-op for these
  # components because their grain is fixed by the component
  # definition (latent_state is per-unit, detection is per-visit).
  if (components != "response") {
    if (isTRUE(unit_level) || isFALSE(unit_level)) {
      warning(insight::format_warning(c(
        paste0("'unit_level' ignored for components = '",
               components, "'."),
        i = "Component grain is fixed (latent_state per-unit, detection per-visit)."
      )))
    }
    pred <- predict(
      object,
      newdata  = newdata,
      type     = components,
      ndraws   = ndraws,
      resp     = resp,
      summary  = FALSE,
      ...
    )
    return(summarise_or_pass(
      pred, summary = summary, probs = probs, robust = robust
    ))
  }

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

  # Closure-unit per-unit aggregation. `unit_level = NULL` keeps the
  # per-row default; `TRUE` collapses visits to closure-unit totals
  # via the shared sum-of-visits aggregator that pp_check.mvgam and
  # residuals.mvgam already use. The predicate restricts the
  # aggregation to detection families (occ / nmix); mv-custom
  # families operate at the row grain regardless.
  if (isTRUE(unit_level)) {
    if (!needs_closure_unit_aggregation(object$family)) {
      warning(insight::format_warning(c(
        "'unit_level = TRUE' ignored for this family.",
        i = "Per-unit aggregation applies to detection families (occ() / nmix())."
      )))
    } else if (is.list(draws) && !is.matrix(draws)) {
      stop(insight::format_error(c(
        "'unit_level = TRUE' is not supported on multi-response fits.",
        i = "Pass `resp = '<name>'` to scope to one response first."
      )))
    } else {
      nd <- newdata %||% object$data
      draws <- aggregate_closure_unit_visits(object, nd, draws)$yrep_unit
    }
  }

  summarise_or_pass(draws, summary = summary, probs = probs,
                     robust = robust)
}

# Shared summary/pass-through used by both the response and the
# component-delegated paths. Mirrors the original behaviour at the
# tail of fitted.mvgam (named-list of matrices on mv fits, single
# matrix otherwise).
#'@noRd
summarise_or_pass <- function(draws, summary, probs, robust) {
  if (!summary) return(draws)
  if (is.list(draws) && !is.matrix(draws)) {
    return(lapply(draws, summarize_predictions, probs = probs,
                  robust = robust))
  }
  summarize_predictions(draws, probs = probs, robust = robust)
}
