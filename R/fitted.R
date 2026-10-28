#' Expected values of an mvgam model
#'
#' Expected values E\[Y | X\] of an mvgam model, or its linear
#' predictor with `scale = "linear"`, at the training observations by
#' default. The interface follows
#' [`fitted.brmsfit()`][brms::fitted.brmsfit].
#'
#' @param object An object of class `mvgam`.
#' @param scale Character; one of `"response"` (default) or `"linear"`.
#'   `"response"` returns expected values on the response scale via
#'   [posterior_epred.mvgam()]. `"linear"` returns the linear predictor
#'   on the link scale via [posterior_linpred.mvgam()]. Ignored when
#'   `components` is not `"response"`.
#' @param components Character; one of `"response"` (default),
#'   `"latent_state"`, or `"detection"`. The quantity to return:
#'
#'   * `"response"`: the expected value of each row of the data, on
#'     the scale `scale` sets.
#'   * `"latent_state"`, for `nmix()` and `occ()` only: the latent
#'     state of each closure unit given its detections. That is the
#'     abundance `N` for `nmix()` and the probability of occupancy
#'     `P(z = 1 | y)` for `occ()`. [predict.mvgam()] returns the same
#'     with `type = "latent_state"`.
#'   * `"detection"`, for `nmix()` and `occ()` only: the detection
#'     probability of each visit, as `type = "detection"` of
#'     [predict.mvgam()] returns it.
#'
#'   The argument matches `components` of `flocker::fitted_flocker()`.
#' @param unit_level Logical or `NULL` (default). When `TRUE` for a
#'   closure-unit detection family and `components = "response"`,
#'   sums the per-visit response matrix over the visits of each unit.
#'   Other `components` have fixed grains and ignore it. So do families
#'   that do not aggregate visits.
#' @inheritParams posterior_epred.mvgam
#' @inheritParams predict.mvgam
#' @param incl_autocor Logical; whether the fitted values include the
#'   latent trend state the model inferred at each time. Defaults to
#'   `TRUE` for the training data, as in
#'   [`brms::fitted.brmsfit()`][brms::fitted.brmsfit]. Defaults to
#'   `FALSE` with `newdata`. Under `TRUE` the observed response minus
#'   the fitted value is the ordinary residual of [residuals.mvgam()].
#' @param ... Unused. Anything passed here is refused.
#'
#' @inherit predict.mvgam return
#'
#' @details
#' As in brms, fitted values leave out the observation noise.
#' [posterior_predict.mvgam()] adds it. Use [predict.mvgam()] for
#' predictive draws.
#'
#' For the training data the fitted values include the trend state the
#' model inferred at each time. [hindcast.mvgam()] returns the same
#' state. With `newdata`, or under `incl_autocor = FALSE`, the fitted
#' values describe the covariate part of the model. `process_error` then
#' sets the trend's part. `FALSE` keeps the trend's covariate effects.
#' `TRUE` also samples a state per draw from the trend's dynamics.
#'
#' The brms arguments `dpar`, `nlpar` and `sort` are refused.
#' [posterior_linpred.mvgam()] returns a distributional parameter
#' through its own `dpar`. Rows come back in the order of the frame.
#'
#' @seealso [residuals.mvgam()] compares the fitted values with the
#'   data. [hindcast.mvgam()] returns the fitted latent states.
#'   [forecast.mvgam()] predicts future times.
#'
#' @examples
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR())
#' mod <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
#'               data    = simdat$data_train,
#'               family  = poisson(),
#'               chains  = 2, silent = 2)
#'
#' # Expected counts at the training observations: the posterior mean,
#' # error and 95% interval of each
#' head(fitted(mod))
#'
#' # The covariate effects alone, without the inferred trend state
#' head(fitted(mod, incl_autocor = FALSE))
#'
#' # One row per posterior draw and one column per observation
#' dim(fitted(mod, summary = FALSE))
#'
#' # Next: residuals() and pp_check() compare the fit with the data
#' }
#'
#' @family predictions
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
                         draw_ids = NULL,
                         summary = TRUE,
                         robust = FALSE,
                         probs = c(0.025, 0.975),
                         process_error = FALSE,
                         incl_autocor = is.null(newdata),
                         allow_new_levels = FALSE,
                         sample_new_levels = "uncertainty",
                         ...) {
  checkmate::assert_class(object, "mvgam")
  require_fitted_model(object, "fitted")
  scale <- match.arg(scale)
  components <- match.arg(components)
  checkmate::assert_data_frame(newdata, null.ok = TRUE)
  checkmate::assert_logical(process_error, len = 1, any.missing = FALSE)
  checkmate::assert_logical(incl_autocor, len = 1, any.missing = FALSE)
  validate_draw_selectors(ndraws, draw_ids)
  validate_group_level_args(re_formula, allow_new_levels, sample_new_levels)
  checkmate::assert_string(resp, null.ok = TRUE)
  checkmate::assert_logical(summary, len = 1, any.missing = FALSE)
  checkmate::assert_logical(robust, len = 1, any.missing = FALSE)
  checkmate::assert_logical(unit_level, len = 1, null.ok = TRUE,
                              any.missing = FALSE)
  checkmate::assert_numeric(
    probs, lower = 0, upper = 1, min.len = 1, any.missing = FALSE
  )
  rlang::check_dots_empty()

  # `latent_state` / `detection` components delegate to predict()
  # which already runs the family-availability gate and dispatches
  # to the per-family kernel (posterior_latent_N / posterior_occupancy
  # / posterior_detection). `unit_level` is a no-op for these
  # components because their grain is fixed by the component
  # definition (latent_state is per-unit, detection is per-visit).
  if (components != "response") {
    if (isTRUE(unit_level) || isFALSE(unit_level)) {
      insight::format_warning(c(
        paste0("'unit_level' ignored for components = '",
               components, "'."),
        i = paste0(
          "Component grain is fixed: 'latent_state' has one value ",
          "per unit and 'detection' one per visit."
        )
      ))
    }
    pred <- predict(
      object,
      newdata  = newdata,
      type     = components,
      ndraws   = ndraws,
      draw_ids = draw_ids,
      resp     = resp,
      summary  = FALSE
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
    incl_autocor = incl_autocor,
    ndraws = ndraws,
    draw_ids = draw_ids,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels,
    resp = resp
  )

  # Closure-unit per-unit aggregation. `unit_level = NULL` keeps the
  # per-row default; `TRUE` collapses visits to closure-unit totals
  # via the shared sum-of-visits aggregator that pp_check.mvgam and
  # residuals.mvgam already use. The predicate restricts the
  # aggregation to detection families (occ / nmix); mv-custom
  # families operate at the row grain regardless.
  if (isTRUE(unit_level)) {
    if (!is_closure_unit_family(object$family)) {
      insight::format_warning(c(
        "'unit_level = TRUE' ignored for this family.",
        i = paste0("Per-unit aggregation applies to the detection ",
                   "families occ() and nmix().")
      ))
    } else if (is_per_response(draws)) {
      stop(insight::format_error(c(
        "'unit_level = TRUE' is not supported on multi-response fits.",
        i = "Pass `resp = '<name>'` to scope to one response first."
      )))
    } else {
      nd <- prediction_frame(object, newdata)
      draws <- aggregate_closure_unit_visits(object, nd, draws)$yrep_unit
    }
  }

  summarise_or_pass(draws, summary = summary, probs = probs,
                    robust = robust)
}
