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
#'   with [loo_model_weights.mvgam()] on the fits and pass the numeric
#'   result.
#' @param ndraws Positive integer giving the total number of
#'   ensemble draws to return per series. Defaults to `5000`.
#'   A model allocated more draws than it holds is sampled with
#'   replacement.
#' @param seed Optional integer seed for reproducibility.
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
#' @return An object of class `mvgam_forecast` containing the
#'   ensembled hindcasts and forecasts. Suitable for
#'   [plot.mvgam_forecast()] and [score.mvgam_forecast()].
#'   Attribute `weights` records the per-model weights used.
#'   Attribute `ndraws_per_model` records the integer draw
#'   counts drawn from each input.
#'
#' @author Nicholas J Clark
#'
#' @seealso [forecast.mvgam()], [hindcast.mvgam()],
#'   [score.mvgam_forecast()], [pp_average.mvgam()],
#'   [loo_model_weights.mvgam()]. The online article
#'   \url{https://nicholasjclark.github.io/mvgam/articles/forecast_evaluation.html}
#'   shows `ensemble()` weighted by stacking weights derived
#'   from `loo_model_weights.mvgam_lfo()`.
#'
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
  models <- split$models
  n_models <- length(models)
  validate_forecast_compatibility(models)
  w <- if (is.null(weights)) {
    rep(1 / n_models, n_models)
  } else {
    mvgam_normalize_weights(weights, n_models)
  }
  ndraws_per <- mvgam_round_largest_remainder(w * ndraws)
  names(w) <- names(ndraws_per) <- names(models)

  # A posterior draw is a joint object. Row r of every series, in
  # both slots, comes from one draw. Taking rows separately per series
  # or per slot would pair unrelated draws. Model m gives one set of
  # `ndraws_per[m]` rows. Every series in both slots takes those rows.
  draw_idx <- resolve_ensemble_draws(models, ndraws_per)
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
#' @param models List of `mvgam_forecast` objects
#' @return `NULL`, invisibly, or an error at the first mismatch
#' @noRd
validate_forecast_compatibility <- function(models) {
  checkmate::assert_list(models)
  if (length(models) < 2L) {
    stop(insight::format_error(
      "Ensembling requires at least two 'mvgam_forecast' inputs."
    ), call. = FALSE)
  }
  ref <- models[[1L]]
  ref_series <- ref$series_names
  ref_n_hcs <- length(ref$hindcasts)
  ref_n_fcs <- length(ref$forecasts)
  ref_fc_horizons <- vapply(
    ref$forecasts,
    function(x) ncol(x), integer(1L)
  )
  ref_hc_lengths <- vapply(
    ref$hindcasts,
    function(x) ncol(x), integer(1L)
  )
  ref_test_obs <- ref$test_observations
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
  for (i in seq.int(2L, length(models))) {
    cand <- models[[i]]
    if (!identical(as.character(cand$series_names),
                    as.character(ref_series))) {
      stop(insight::format_error(c(
        "Series names must match for all forecast inputs.",
        x = paste0(
          "Input ", i,
          " has different series than the first input."
        )
      )), call. = FALSE)
    }
    if (length(cand$hindcasts) != ref_n_hcs ||
          length(cand$forecasts) != ref_n_fcs) {
      stop(insight::format_error(c(
        paste0(
          "All forecast inputs must have the same number of ",
          "series in 'hindcasts' and 'forecasts'."
        ),
        x = paste0(
          "Input ", i, " has ", length(cand$hindcasts),
          " hindcast series and ", length(cand$forecasts),
          " forecast series."
        )
      )), call. = FALSE)
    }
    cand_fc_horizons <- vapply(
      cand$forecasts, function(x) ncol(x), integer(1L)
    )
    cand_hc_lengths <- vapply(
      cand$hindcasts, function(x) ncol(x), integer(1L)
    )
    if (!identical(cand_fc_horizons, ref_fc_horizons)) {
      stop(insight::format_error(c(
        "Forecast horizons must match for all forecast inputs.",
        x = paste0(
          "Input ", i, " forecast horizons differ from the ",
          "first input."
        )
      )), call. = FALSE)
    }
    if (!identical(cand_hc_lengths, ref_hc_lengths)) {
      stop(insight::format_error(c(
        "Hindcast lengths must match for all forecast inputs.",
        x = paste0(
          "Input ", i, " hindcast lengths differ from the ",
          "first input."
        )
      )), call. = FALSE)
    }
    if (!is.null(ref_test_obs) &&
          !is.null(cand$test_observations) &&
          !identical(cand$test_observations, ref_test_obs)) {
      stop(insight::format_error(c(
        "Test observations must match for all forecast inputs.",
        x = paste0(
          "Input ", i, " has different test observations than ",
          "the first input."
        )
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
