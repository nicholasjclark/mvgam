#' Combine forecasts from \pkg{mvgam} models into a weighted
#' ensemble
#'
#' Build a single `mvgam_forecast` from the hindcast and forecast
#' draws of two or more `mvgam_forecast` objects. Each input supplies
#' a share of the draws set by its weight. Weights are even by
#' default.
#'
#' @name ensemble.mvgam_forecast
#'
#' @param object An `mvgam_forecast` object returned by
#'   [forecast.mvgam()] or [hindcast.mvgam()].
#' @param ... More `mvgam_forecast` objects to combine.
#' @param weights Either `NULL` (the default, equivalent to even
#'   weighting), or a numeric vector with one non-negative entry per
#'   supplied forecast. The weights are normalised to sum to 1.
#'   Strategies such as stacking need the fitted models. Compute them
#'   with [loo_model_weights.mvgam()] on the fits, or with
#'   [loo_model_weights.mvgam_lfo()] on their [lfo_cv()] results. Pass
#'   the numeric result here.
#' @param ndraws Positive integer giving the total number of
#'   ensemble draws to return per series. Defaults to `5000`.
#'   A model allocated more draws than it holds is sampled with
#'   replacement.
#' @param seed Optional integer seed. The session's random state is
#'   restored when the call returns.
#'
#' @details Per-model weights are split into integer draw counts
#'   that sum to `ndraws` by the largest-remainder rule, after
#'   `brms::round_largest_remainder()`. Each model then gives one set
#'   of sampled draws. Every series, in the hindcasts and in the
#'   forecasts, takes that same set. The ensemble keeps the dependence
#'   between series and the pairing of each hindcast with its forecast.
#'
#'   The returned object takes its series names, family, training data
#'   and test data from the first input. All inputs must share the
#'   series names, the hindcast lengths, the forecast horizons and, when
#'   present, the test observations.
#'
#'   A multivariate fit on a wide frame gives one forecast whose series
#'   are its responses. The ensemble is a forecast over those series. On
#'   a long frame the ensemble keeps one forecast per response. Every
#'   response takes the same draws.
#'
#' @return An object of class `mvgam_forecast` containing the
#'   ensembled hindcasts and forecasts. Suitable for
#'   [plot.mvgam_forecast()] and [score.mvgam_forecast()].
#'   Attribute `weights` records the per-model weights used.
#'   Attribute `ndraws_per_model` records the integer draw
#'   counts drawn from each input.
#'
#' @author Nicholas J Clark
#'
#' @seealso [score.mvgam_forecast()] scores the ensemble.
#'   [loo_model_weights.mvgam_lfo()] computes weights from forecast
#'   performance. [pp_average.mvgam()] averages predictions in place of
#'   forecasts. The
#'   `r mvgam_article("forecast_evaluation", "forecast evaluation article")`
#'   weights an ensemble by leave-future-out stacking weights.
#'
#' @template ex-forecast-models
#' @templateVar seed 1
#' @examples
#' \dontrun{
#' fc_gam <- forecast(mod_gam, newdata = simdat$data_test)
#' fc_ar <- forecast(mod_ar, newdata = simdat$data_test)
#'
#' # An ensemble that draws 30% of its draws from the GAM
#' ens <- ensemble(fc_gam, fc_ar, weights = c(0.3, 0.7))
#' attr(ens, "ndraws_per_model")
#' plot(ens)
#'
#' # The ensemble scored against its components
#' scores <- compare_scores(fc_gam, fc_ar, ens, score = "drps",
#'                          model_names = c("GAM", "AR", "Ensemble"))
#' plot(scores, relative = "GAM")
#' }
#'
#' @family forecasting
#' @export
ensemble <- function(object, ...) {
  UseMethod("ensemble", object)
}


#' @rdname ensemble.mvgam_forecast
#' @method ensemble mvgam_forecast
#' @export
ensemble.mvgam_forecast <- function(object, ..., weights = NULL,
                                     ndraws = 5000L,
                                     seed = NULL) {
  checkmate::assert_class(object, "mvgam_forecast")
  checkmate::assert_integerish(ndraws, lower = 1L, len = 1L)
  local_seed(seed)
  ndraws <- as.integer(ndraws)
  split <- mvgam_split_models(object, ...,
                                class = "mvgam_forecast")
  if (length(split$other) > 0L) {
    stop(insight::format_error(c(
      "Only 'mvgam_forecast' objects can be passed via '...'.",
      x = paste0(
        "Got named extra args: ",
        paste0("'", names(split$other), "'", collapse = ", "), "."
      )
    )), call. = FALSE)
  }
  assert_compared_objects(split$models, "mvgam_forecast", "ensemble")
  models <- lapply(split$models, forecast_parts)
  n_models <- length(models)
  w <- stats::setNames(
    if (is.null(weights)) {
      rep(1 / n_models, n_models)
    } else {
      mvgam_normalize_weights(weights, n_models)
    },
    names(models)
  )
  ndraws_per <- mvgam_round_largest_remainder(w * ndraws)

  # A long-frame multivariate forecast holds one forecast per response.
  # Every response takes the same draw rows. The ensemble then keeps
  # the dependence between responses.
  per_response <- vapply(models, is_forecast_list, logical(1L))
  if (any(per_response)) {
    resps <- names(models[[1L]])
    if (!all(per_response) ||
          !all(vapply(models, function(m) identical(names(m), resps),
                      logical(1L)))) {
      stop(insight::format_error(c(
        "Every forecast must hold the same responses.",
        i = paste0("Ensemble forecasts of fits that share their ",
                   "response formulas.")
      )), call. = FALSE)
    }
    by_resp <- lapply(stats::setNames(resps, resps), function(r) {
      lapply(models, `[[`, r)
    })
    lapply(by_resp, validate_forecast_compatibility)
    draw_idx <- resolve_ensemble_draws(by_resp[[1L]], ndraws_per)
    out <- split$models[[1L]]
    for (r in resps) {
      out[[r]] <- ensemble_slots(by_resp[[r]], draw_idx, w, ndraws_per)
    }
    return(out)
  }
  validate_forecast_compatibility(models)
  # A posterior draw is a joint object. Row r of every series, in
  # both slots, comes from one draw. Taking rows separately per series
  # or per slot would pair unrelated draws. Model m gives one set of
  # `ndraws_per[m]` rows. Every series in both slots takes those rows.
  draw_idx <- resolve_ensemble_draws(models, ndraws_per)
  ensemble_slots(models, draw_idx, w, ndraws_per)
}


#' One ensembled forecast from the draw rows of each input
#'
#' @param models List of `mvgam_forecast` objects
#' @param draw_idx List of integer row vectors from
#'   `resolve_ensemble_draws()`, one per model
#' @param w Named numeric vector of normalised weights
#' @param ndraws_per Named integer vector of draws taken per model
#' @return The first model with its hindcast and forecast slots
#'   replaced by the stacked draws, carrying `weights` and
#'   `ndraws_per_model` attributes
#' @noRd
ensemble_slots <- function(models, draw_idx, w, ndraws_per) {
  out <- models[[1L]]
  # `[<-` keeps a NULL forecast slot. `$<-` would drop the element.
  out["hindcasts"] <- list(combine_slot(models, "hindcasts", draw_idx))
  out["forecasts"] <- list(combine_slot(models, "forecasts", draw_idx))
  attr(out, "weights") <- w
  attr(out, "ndraws_per_model") <- ndraws_per
  out
}


#' Refuse forecasts an ensemble cannot align
#'
#' Every forecast must share the series names, the hindcast lengths,
#' the forecast horizons and any test observations of the first. Within
#' one forecast, every draw matrix must hold the same number of draws.
#'
#' @param models List of at least two `mvgam_forecast` objects
#' @return `NULL`, invisibly, or an error at the first mismatch
#' @noRd
validate_forecast_compatibility <- function(models) {
  checkmate::assert_list(models, min.len = 2L)
  for (i in seq_along(models)) {
    n_rows <- unique(vapply(
      c(models[[i]]$hindcasts, models[[i]]$forecasts), NROW, integer(1L)
    ))
    if (length(n_rows) != 1L) {
      stop(insight::format_error(c(
        "Every draw matrix of one forecast must hold the same draws.",
        x = paste0("Input ", i, " holds ",
                   paste(sort(n_rows), collapse = ", "), " draws."),
        i = "Pass forecasts as 'forecast()' or 'hindcast()' returns them."
      )), call. = FALSE)
    }
  }
  shape <- function(m) {
    list(
      "series names" = as.character(m$series_names),
      "hindcast lengths" = vapply(m$hindcasts, NCOL, integer(1L)),
      "forecast horizons" = vapply(m$forecasts, NCOL, integer(1L)),
      "test observations" = m$test_observations
    )
  }
  ref <- shape(models[[1L]])
  for (i in seq.int(2L, length(models))) {
    cand <- shape(models[[i]])
    # A forecast without test observations is compared on the rest
    if (is.null(cand$`test observations`)) {
      cand["test observations"] <- list(ref$`test observations`)
    }
    differs <- !mapply(identical, ref, cand)
    if (any(differs)) {
      stop(insight::format_error(c(
        "Every forecast in an ensemble must align with the first.",
        x = paste0("Input ", i, " differs in its ",
                   names(ref)[which(differs)[1L]], "."),
        i = "Forecast every model over the same 'newdata'."
      )), call. = FALSE)
    }
  }
  invisible(NULL)
}


#' The draw rows an ensemble takes from each forecast
#'
#' Drawn once per model to index every series in both the hindcast and
#' forecast slots by the same posterior draws. A model holding fewer
#' rows than its allocation is sampled with replacement.
#' `validate_forecast_compatibility()` has checked that every draw
#' matrix of one model holds the same draws. The first gives the count.
#'
#' @param models List of `mvgam_forecast` objects
#' @param ndraws_per Integer vector of draws to take from each model
#' @return List of integer row vectors, one per model
#' @noRd
resolve_ensemble_draws <- function(models, ndraws_per) {
  checkmate::assert_list(models, min.len = 1L)
  checkmate::assert_integerish(ndraws_per, lower = 0,
                               len = length(models))
  lapply(seq_along(models), function(m) {
    take <- ndraws_per[m]
    if (take == 0L) return(integer(0L))
    n_avail <- NROW(c(models[[m]]$hindcasts, models[[m]]$forecasts)[[1L]])
    sample(seq_len(n_avail), take, replace = take > n_avail)
  })
}


#' One forecast slot stacked across models
#'
#' Takes the rows `draw_idx[[m]]` names from model `m` for every series
#' and binds them by row.
#'
#' @param models List of `mvgam_forecast` objects
#' @param slot Character slot name, `"hindcasts"` or `"forecasts"`
#' @param draw_idx List of integer row vectors from
#'   `resolve_ensemble_draws()`, one per model
#' @return List of draw matrices named by series, one per series, or
#'   `NULL` for a slot the first model leaves empty
#' @noRd
combine_slot <- function(models, slot, draw_idx) {
  checkmate::assert_list(models, min.len = 1L)
  checkmate::assert_choice(slot, c("hindcasts", "forecasts"))
  checkmate::assert_list(draw_idx, len = length(models))
  first <- models[[1L]][[slot]]
  if (is.null(first)) return(NULL)
  lapply(stats::setNames(seq_along(first), names(first)), function(s) {
    do.call(rbind, lapply(seq_along(models), function(m) {
      models[[m]][[slot]][[s]][draw_idx[[m]], , drop = FALSE]
    }))
  })
}
