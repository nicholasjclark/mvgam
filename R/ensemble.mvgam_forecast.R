#' Combine forecasts from \pkg{mvgam} models into a weighted
#' ensemble
#'
#' Build a single `mvgam_forecast` whose hindcast and forecast
#' draws are a multinomially-resampled combination of two or more
#' input `mvgam_forecast` objects. Defaults to even weighting, but
#' a numeric vector of user-supplied per-model weights is also
#' accepted (auto-normalised to sum to 1).
#'
#' @name ensemble.mvgam_forecast
#'
#' @param object An `mvgam_forecast` object returned by
#'   [forecast.mvgam()] or [hindcast.mvgam()].
#' @param ... More `mvgam_forecast` objects to combine.
#' @param weights Either `NULL` (the default, equivalent to even
#'   weighting), or a numeric vector with one entry per supplied
#'   forecast. Non-negative numeric weights are auto-normalised
#'   so that they sum to 1. Strategy strings such as
#'   `"stacking"` are intentionally not accepted here because
#'   computing them requires the fitted models, not just the
#'   forecast draws; pre-compute via
#'   [loo::loo_model_weights()] / [loo_model_weights.mvgam()]
#'   on the fits and pass the numeric result.
#' @param ndraws Positive integer giving the total number of
#'   ensemble draws to return per series. Defaults to `5000`.
#'   If `ndraws` exceeds the total number of input draws,
#'   sampling is done with replacement.
#' @param seed Optional integer seed for reproducibility.
#'
#' @details Per-model weights are split into integer draw counts
#'   that sum to `ndraws` using the largest-remainder rule
#'   (largest-remainder rule, after `brms::round_largest_remainder()`),
#'   then that
#'   many rows are sampled (with replacement when a model has
#'   fewer draws than its assigned share) from each input's
#'   hindcasts and forecasts. The returned object inherits its
#'   `series_names`, `family`, `trend_model`, training arms and
#'   test arms from the first input. All forecast objects must
#'   share matching `series_names`, hindcast / forecast counts,
#'   forecast horizons and (when present) test observations.
#'
#' @return An object of class `mvgam_forecast` containing the
#'   ensembled hindcasts and forecasts. Suitable for
#'   [plot.mvgam_forecast()] and [score.mvgam_forecast()].
#'   Attribute `weights` records the per-model weights actually
#'   used; attribute `ndraws_per_model` records the integer draw
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
    )))
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

  # Sample `ndraws_per[m]` rows from model m once, then take
  # those same rows for every series and for both hindcasts and
  # forecasts, before row-binding across models into a single
  # (ndraws x H) matrix per series.
  #
  # One selection, not one per series: a posterior draw is a
  # joint object, and the dependence between series lives in
  # which draws are taken together. Choosing rows separately per
  # series pairs series 1's draw 40 with series 2's draw 900 and
  # throws that dependence away. Two series identical on every
  # draw came back correlated 0.05, and any joint score, energy
  # or variogram, was then scoring an ensemble that had no
  # cross-series structure left. Choosing separately per arm
  # breaks the hindcast-to-forecast pairing the same way, so a
  # per-draw trajectory through `plot()` was two unrelated
  # halves.
  series_names <- names(models[[1L]]$forecasts)
  draw_idx <- resolve_ensemble_draws(models, ndraws_per)
  ens_hcs <- combine_arm(models, "hindcasts", draw_idx)
  ens_fcs <- combine_arm(models, "forecasts", draw_idx)
  names(ens_hcs) <- series_names
  names(ens_fcs) <- series_names

  out <- models[[1L]]
  out$hindcasts <- ens_hcs
  out$forecasts <- ens_fcs
  attr(out, "weights") <- w
  attr(out, "ndraws_per_model") <- ndraws_per
  out
}


# Internal: check that every forecast in `models` agrees on the
# slots an ensemble needs to align across (series, horizons,
# test observations). Errors via `insight::format_error` on
# first mismatch.
#'@noRd
validate_forecast_compatibility <- function(models) {
  if (length(models) < 2L) {
    stop(insight::format_error(
      "Ensembling requires at least two 'mvgam_forecast' inputs."
    ))
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
      )))
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
      )))
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
      )))
    }
    if (!identical(cand_hc_lengths, ref_hc_lengths)) {
      stop(insight::format_error(c(
        "Hindcast lengths must match for all forecast inputs.",
        x = paste0(
          "Input ", i, " hindcast lengths differ from the ",
          "first input."
        )
      )))
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
      )))
    }
  }
  invisible(NULL)
}


# Internal: the rows to take from each model, drawn once so every
# series and both arms are indexed by the same posterior draws.
# Sampled with replacement when a model holds fewer rows than its
# allocation. Returns a list of integer vectors, one per model.
#' @noRd
resolve_ensemble_draws <- function(models, ndraws_per) {
  lapply(seq_along(models), function(m) {
    take <- ndraws_per[m]
    if (take == 0L) return(integer(0L))
    n_avail <- NROW(models[[m]]$forecasts[[1L]])
    sample(seq_len(n_avail), take, replace = take > n_avail)
  })
}


# Internal: stack a per-series draw matrix arm (e.g. "forecasts"
# or "hindcasts") across models, taking the rows `draw_idx[[m]]`
# names from model m for every series. Returns an unnamed list of
# (sum(lengths(draw_idx)) x H) matrices.
#' @noRd
combine_arm <- function(models, arm, draw_idx) {
  n_series <- length(models[[1L]][[arm]])
  lapply(seq_len(n_series), function(s) {
    pieces <- lapply(seq_along(models), function(m) {
      idx <- draw_idx[[m]]
      if (!length(idx)) return(NULL)
      mat <- models[[m]][[arm]][[s]]
      # An arm with fewer rows than the one the draws were
      # chosen from cannot be indexed by them; wrap so the
      # pairing stays as close as the shapes allow.
      if (max(idx) > NROW(mat)) {
        idx <- ((idx - 1L) %% NROW(mat)) + 1L
      }
      mat[idx, , drop = FALSE]
    })
    do.call(rbind, pieces[!vapply(pieces, is.null, logical(1L))])
  })
}
