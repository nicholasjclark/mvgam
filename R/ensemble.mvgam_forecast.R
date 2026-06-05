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
#'   ([brms::round_largest_remainder()] convention), then that
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
#'   [loo_model_weights.mvgam()]
#'
#' @examples
#' \donttest{
#' sim <- sim_mvgam(family = gaussian())
#' m1 <- mvgam(y ~ 1, trend_formula = ~ AR(),
#'              data = sim$data_train, newdata = sim$data_test,
#'              family = gaussian(), chains = 1, silent = 2)
#' m2 <- mvgam(y ~ 1, trend_formula = ~ RW(),
#'              data = sim$data_train, newdata = sim$data_test,
#'              family = gaussian(), chains = 1, silent = 2)
#' fc1 <- forecast(m1)
#' fc2 <- forecast(m2)
#'
#' # Even-weighted ensemble (default)
#' ens <- ensemble(fc1, fc2)
#'
#' # User-supplied weights (auto-normalised)
#' ens_weighted <- ensemble(fc1, fc2, weights = c(0.7, 0.3))
#'
#' # Weights derived from loo model weights on the fits
#' w <- loo_model_weights(m1, m2)
#' ens_loo <- ensemble(fc1, fc2, weights = as.numeric(w))
#' }
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
  if (!is.null(seed)) {
    if (exists(".Random.seed", envir = .GlobalEnv)) {
      rng_old <- get(".Random.seed", envir = .GlobalEnv)
      on.exit(assign(".Random.seed", rng_old, envir = .GlobalEnv))
    }
    set.seed(seed)
  }
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

  # Per-series, sample `ndraws_per[m]` rows from model m's
  # hindcasts and forecasts (with replacement when the model
  # has fewer rows than its allocation), then row-bind across
  # models to produce a single (ndraws x H) matrix per series.
  series_names <- names(models[[1L]]$forecasts)
  ens_hcs <- combine_arm(models, "hindcasts", ndraws_per)
  ens_fcs <- combine_arm(models, "forecasts", ndraws_per)
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


# Internal: stack a per-series draw matrix arm (e.g. "forecasts"
# or "hindcasts") across models, taking `ndraws_per[m]` rows
# from model m for each series (sampled with replacement when
# the model has fewer rows than its allocation). Returns an
# unnamed list of (sum(ndraws_per) x H) matrices.
#'@noRd
combine_arm <- function(models, arm, ndraws_per) {
  n_series <- length(models[[1L]][[arm]])
  lapply(seq_len(n_series), function(s) {
    pieces <- lapply(seq_along(models), function(m) {
      mat <- models[[m]][[arm]][[s]]
      n_avail <- NROW(mat)
      take <- ndraws_per[m]
      if (take == 0L) return(NULL)
      idx <- sample(seq_len(n_avail), take,
                     replace = take > n_avail)
      mat[idx, , drop = FALSE]
    })
    do.call(rbind, pieces[!vapply(pieces, is.null, logical(1L))])
  })
}
