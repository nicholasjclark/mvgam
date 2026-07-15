# Approximate leave-future-out cross-validation for mvgam fits.
#
# Implements the PSIS-LFO algorithm of Burkner, Gabry & Vehtari
# (2020): refit the model on the first `min_t` observations, then
# roll forward one step at a time. Between refits, Pareto Smoothed
# Importance Sampling (PSIS) reweights the posterior to
# approximate a fresh fit; a refit only happens when the Pareto k
# diagnostic crosses a stability threshold.
#
# ELPD uses the standard PSIS algorithm directly. Non-ELPD scores
# (crps, drps, sis, brier, energy, variogram) are computed by
# resampling forecast draws according to the PSIS weights before
# routing through `score.mvgam_forecast`, which is the canonical
# Monte Carlo equivalent of evaluating the score under the
# PSIS-weighted posterior.
#
# Built on forecast.mvgam (F3 / F4 / F4b), score.mvgam_forecast
# (F6) and log_lik.mvgam.


#' Approximate leave-future-out cross-validation of fitted mvgam
#' objects
#'
#' @description Approximate leave-future-out (LFO) cross-validation
#'   uses an expanding training window to evaluate a model on its
#'   forecasting ability. The algorithm mirrors the
#'   [loo PSIS-LFO vignette](https://mc-stan.org/loo/articles/loo2-lfo.html)
#'   of Burkner, Gabry and Vehtari (2020): refit the model on the
#'   first `min_t` observations and compute an exact `fc_horizon`
#'   -ahead score; for each subsequent time point, reweight the
#'   posterior with Pareto Smoothed Importance Sampling (PSIS) and
#'   only refit when the Pareto-k diagnostic exceeds
#'   `pareto_k_threshold`.
#'
#' @name lfo_cv.mvgam
#' @importFrom stats update logLik quantile
#'
#' @param object A fitted [mvgam][mvgam::mvgam] object.
#' @param newdata Optional `data.frame` containing the response,
#'   `time`, `series` and any covariates required by the model
#'   formula. When `NULL`, the original training data
#'   (`object$obs_data` or `object$data`) is used. All series must
#'   share the same set of observed time values.
#' @param data Deprecated. Use `newdata` instead.
#' @param min_t Integer; the time *value* at which the initial
#'   training window ends. Must be a time value present in the
#'   data. The first forecast window covers the next `fc_horizon`
#'   observed times. When `NULL`, defaults to the time value at
#'   the 30th observed time point (ratcheted down for shorter
#'   series) and adjusted to leave at least 10 evaluation folds
#'   when possible.
#' @param fc_horizon Integer; the number of *observed time steps*
#'   ahead evaluated at each fold. For regular grids this is the
#'   familiar forecast horizon; for irregular CAR grids it is the
#'   next `fc_horizon` observed times (the CAR kernel absorbs the
#'   gaps via `time_dis`). Default `1`.
#' @param pareto_k_threshold Proportion in `[0, 1]`; the Pareto
#'   shape value above which the PSIS approximation is considered
#'   unstable and a refit is triggered. `NULL` (default) uses the
#'   adaptive threshold `min(1 - 1 / log10(S), 0.7)`, where `S` is
#'   the number of posterior draws (Vehtari, Simpson, Gelman, Yao
#'   & Gabry 2024). The adaptive rule tightens the threshold for
#'   fits with few posterior draws where PSIS is less reliable and
#'   clamps at `0.7` once `S` is large enough for the classical
#'   guarantee to hold. Pass an explicit numeric to override.
#' @param score Character vector of scoring rules to compute at
#'   each fold. Must be a subset of `c("elpd", "crps", "drps",
#'   "sis", "brier", "energy", "variogram")`. ELPD uses the
#'   PSIS log-likelihood approximation directly; the other
#'   scores are computed by sampling forecast draws from the
#'   PSIS-weighted posterior and routing through
#'   [score.mvgam_forecast]. Defaults to `"elpd"`.
#' @param silent Verbosity level between `0` and `2`. See
#'   [mvgam] for the contract.
#' @param ... Currently unused.
#'
#' @return A `list` of class `mvgam_lfo` containing:
#'   * `elpds` — vector of approximate ELPDs at each evaluation
#'     time point (if `"elpd"` is in `score`).
#'   * `scores` — named list of vectors, one per requested non-ELPD
#'     score (`NULL` if no non-ELPD score requested).
#'   * `pareto_ks` — Pareto-k diagnostic at each evaluation step.
#'   * `eval_timepoints` — integer vector of the times evaluated.
#'   * `refits_at` — integer vector of time points where the model
#'     was refit.
#'   * `pareto_k_threshold`: the threshold argument as supplied
#'     (numeric, or `NULL` when the adaptive default was used).
#'   * `pareto_k_threshold_used`: the effective numeric threshold
#'     actually applied inside the refit gate (equal to
#'     `pareto_k_threshold` when a numeric was supplied, or the
#'     adaptive value when `NULL` was supplied).
#'   * `fc_horizon` — the horizon used at each fold.
#'
#' @references
#' Paul-Christian Burkner, Jonah Gabry and Aki Vehtari (2020).
#' Approximate leave-future-out cross-validation for Bayesian time
#' series models. *Journal of Statistical Computation and
#' Simulation*. 90:14, 2499-2523.
#'
#' Aki Vehtari, Daniel Simpson, Andrew Gelman, Yuling Yao and
#' Jonah Gabry (2024). Pareto smoothed importance sampling.
#' *Journal of Machine Learning Research*. 25(72), 1-58.
#'
#' @seealso [forecast.mvgam], [hindcast.mvgam],
#'   [score.mvgam_forecast], [log_lik.mvgam], [update.mvgam],
#'   [loo.mvgam()] for PSIS-LOO over individual observations,
#'   [kfold.mvgam()] for grouped k-fold CV (the same selective-
#'   refit pattern generalised to arbitrary grouping factors).
#'   The online article
#'   \url{https://nicholasjclark.github.io/mvgam/articles/forecast_evaluation.html}
#'   walks through `lfo_cv()` end-to-end on rodent count data,
#'   including the Pareto-k refit gate and ensemble weighting
#'   via `loo_model_weights.mvgam_lfo()`.
#'
#' @examples
#' \donttest{
#' set.seed(11)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 80L, trend_model = AR())
#'
#' mod <- mvgam(
#'   y ~ s(x),
#'   trend_formula = ~ AR(p = 1),
#'   data    = simdat$data_train,
#'   family  = poisson(),
#'   chains  = 2, silent = 2
#' )
#'
#' # `min_t` sets the first time index from which to predict; here
#' # the model starts forecasting at t = 50 and steps forward one
#' # observation at a time. Refits only fire when PSIS Pareto-k
#' # diagnostic exceeds the threshold.
#' lfo <- lfo_cv(mod, min_t = 50L, fc_horizon = 1L,
#'                pareto_k_threshold = 0.7)
#' sum(lfo$elpds)
#' }
#'
#' @author Nicholas J Clark
#' @export
lfo_cv <- function(object, ...) {
  UseMethod("lfo_cv", object)
}


#' @rdname lfo_cv.mvgam
#' @method lfo_cv mvgam
#' @export
lfo_cv.mvgam <- function(object,
                          newdata = NULL,
                          min_t = NULL,
                          fc_horizon = 1L,
                          pareto_k_threshold = NULL,
                          score = "elpd",
                          save_log_lik = FALSE,
                          silent = 1L,
                          ...,
                          data = NULL) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_data_frame(newdata, null.ok = TRUE)
  checkmate::assert_int(fc_horizon, lower = 1L)
  checkmate::assert_number(pareto_k_threshold,
                            lower = 0, upper = 1,
                            null.ok = TRUE)
  checkmate::assert_int(min_t, lower = 1L, null.ok = TRUE)
  checkmate::assert_flag(save_log_lik)
  checkmate::assert_int(silent, lower = 0L, upper = 2L)
  allowed_scores <- c("elpd", "crps", "drps", "sis", "brier",
                       "energy", "variogram")
  checkmate::assert_subset(score, allowed_scores, empty.ok = FALSE)
  score <- unique(score)

  # Backward-compat: accept the deprecated `data` arg.
  if (!is.null(data)) {
    if (is.null(newdata)) {
      warning(insight::format_warning(c(
        "'data' is deprecated; use 'newdata' instead.",
        i = "Forwarding the supplied value to 'newdata'."
      )), call. = FALSE)
      newdata <- data
    } else {
      warning(insight::format_warning(
        "Both 'data' and 'newdata' supplied; using 'newdata'."
      ), call. = FALSE)
    }
  }

  all_data <- newdata %||% mvgam_training_data(object)
  if (is.null(all_data)) {
    stop(insight::format_error(c(
      "No data available to roll the LFO window over.",
      i = "Pass the full data frame via 'newdata'."
    )))
  }

  time_var <- object$trend_metadata$variables$time_var %||% "time"
  series_var <- object$trend_metadata$variables$series_var %||%
    "series"
  if (!all(c(time_var, series_var) %in% names(all_data))) {
    stop(insight::format_error(c(
      paste0("'newdata' must contain '", time_var,
             "' and '", series_var, "' columns."),
      i = paste0("Got columns: ",
                 paste(names(all_data), collapse = ", "), ".")
    )))
  }
  # Per-series time grids must all match. Async-series LFO is a
  # follow-up; for now an informative error rather than silent
  # misalignment.
  series_fac <- factor(all_data[[series_var]])
  series_time_sets <- lapply(
    split(as.integer(all_data[[time_var]]), series_fac),
    function(t) sort(unique(t))
  )
  if (length(series_time_sets) > 1L) {
    ref_times <- series_time_sets[[1L]]
    mismatched <- vapply(
      series_time_sets[-1L],
      function(s) !identical(s, ref_times), logical(1L)
    )
    if (any(mismatched)) {
      bad <- names(series_time_sets[-1L])[mismatched]
      stop(insight::format_error(c(
        "'lfo_cv()' requires all series to share the same time grid.",
        x = paste0("Series with a different time grid: ",
                   paste(bad, collapse = ", "), "."),
        i = paste0("Align series to a common time grid (NA-pad ",
                   "the response where needed), or evaluate each ",
                   "series in a separate 'lfo_cv()' call.")
      )))
    }
  }

  all_unique_times <- sort(unique(as.integer(all_data[[time_var]])))
  n_times <- length(all_unique_times)

  # fc_horizon must leave at least one training observation.
  if (fc_horizon >= n_times) {
    stop(insight::format_error(c(
      paste0("'fc_horizon' (", fc_horizon,
             ") exceeds the available time span (",
             n_times, " observed times)."),
      i = paste0("Choose 'fc_horizon' < ", n_times,
                 " so there is room for at least one ",
                 "training observation.")
    )))
  }

  # Resolve min_t to a time VALUE. Default picks a sensible training
  # window from a ladder (30 / 20 / 10 / 1) and indexes into the
  # observed times so it works for any integer start (1..T,
  # 2010..2044, julian days, etc.).
  if (is.null(min_t)) {
    base_idx <- if (n_times > 30L) 30L else
      if (n_times > 20L) 20L else
        if (n_times > 10L) 10L else 1L
    # Adjust to leave at least 10 evaluation folds when possible.
    base_idx <- max(1L, min(base_idx, n_times - 10L - fc_horizon))
    min_t <- all_unique_times[base_idx]
  }

  if (!min_t %in% all_unique_times) {
    stop(insight::format_error(c(
      paste0("'min_t' = ", min_t,
             " is not an observed time."),
      i = paste0("Pass a value in {",
                 min(all_unique_times), "..",
                 max(all_unique_times),
                 "} that appears in the data.")
    )))
  }

  idx_min_t <- match(min_t, all_unique_times)
  if (idx_min_t > n_times - fc_horizon) {
    largest_valid <- all_unique_times[n_times - fc_horizon]
    stop(insight::format_error(c(
      paste0("'min_t' (", min_t,
             ") leaves no room for evaluation."),
      x = paste0("With 'fc_horizon' = ", fc_horizon,
                 " the largest valid 'min_t' is ",
                 largest_valid, "."),
      i = paste0("For a single evaluation fold, set ",
                 "min_t = ", largest_valid, ".")
    )))
  }

  # Internal arithmetic is over observation POSITIONS in
  # all_unique_times (the "k"th evaluation forecasts at the kth
  # observed time after min_t). This decouples the algorithm from
  # the user's integer time start and naturally accommodates
  # irregular CAR grids: the CAR kernel absorbs time gaps via
  # 'time_dis' downstream.
  eval_positions <- seq.int(idx_min_t + 1L,
                              n_times - fc_horizon + 1L)
  eval_timepoints <- all_unique_times[eval_positions]
  n_evals <- length(eval_positions)

  # Pre-allocate per-score storage.
  elpds <- if ("elpd" %in% score) {
    rep(NA_real_, n_evals)
  } else {
    NULL
  }
  other_scores <- setdiff(score, "elpd")
  score_arrays <- if (length(other_scores) > 0L) {
    setNames(
      replicate(length(other_scores),
                 rep(NA_real_, n_evals),
                 simplify = FALSE),
      other_scores
    )
  } else {
    NULL
  }
  pareto_ks <- rep(NA_real_, n_evals)
  refits_at <- integer(0)
  # Parallel to eval_timepoints: TRUE at each eval where a fresh
  # refit was performed (initial fit at min_t plus any
  # Pareto-k-threshold-triggered refits). Read by
  # `summary.mvgam_lfo` to populate the `refit_here` tibble column.
  refit_triggered <- logical(n_evals)
  refit_triggered[1L] <- TRUE  # initial refit at min_t

  # Initial refit at min_t (training position = idx_min_t).
  if (silent < 1L) {
    cat("LFO refit at training time", min_t, "...\n")
  }
  splits <- lfo_cv_split(all_data, last_train = min_t,
                          fc_horizon = fc_horizon,
                          time_var = time_var)
  fit_past <- update(object, newdata = splits$data_train,
                      silent = silent)
  refits_at <- c(refits_at, min_t)

  # log_lik on the FULL data so we can index into it at any
  # future window. The marginal log density (process_error = TRUE
  # in posterior_linpred under log_lik.mvgam) is what PSIS-LFO
  # expects for ELPD.
  loglik_past <- log_lik(fit_past, newdata = all_data)
  idx_refit <- idx_min_t

  # Resolve the numeric threshold applied inside the refit gate.
  # The adaptive rule from Vehtari et al. (2024) tightens the
  # threshold when the posterior draw count `S` is small and
  # clamps at 0.7 once `S` is large enough for the classical
  # guarantee to hold. `pareto_k_threshold` on the return object
  # preserves whatever the user passed (numeric or NULL);
  # `pareto_k_threshold_used` is the effective value read by the
  # refit gate below and by print / plot methods for display.
  pareto_k_threshold_used <- if (is.null(pareto_k_threshold)) {
    mvgam_ps_khat_threshold(nrow(loglik_past))
  } else {
    pareto_k_threshold
  }

  # Compute scores at the very first evaluation window.
  first_window_times <- all_unique_times[
    (idx_min_t + 1L):(idx_min_t + fc_horizon)
  ]
  updates <- scores_at_window(
    fit = fit_past, all_data = all_data,
    time_var = time_var, series_var = series_var,
    window_times = first_window_times,
    score_names = score,
    elpds = elpds, score_arrays = score_arrays,
    eval_idx = 1L, loglik = loglik_past,
    psis_log_weights = NULL,
    silent = silent
  )
  elpds <- updates$elpds
  score_arrays <- updates$score_arrays

  # Optional accumulator of per-observation log-densities at
  # held-out points under the LFO-weighted posterior, in the
  # n_draws x n_eval_obs shape that `loo::loo_model_weights()`
  # consumes for proper log-score stacking. Only allocated when
  # save_log_lik = TRUE because the matrix can be many megabytes.
  log_lik_acc <- if (isTRUE(save_log_lik)) {
    lfo_collect_fold_loglik(
      loglik = loglik_past, all_data = all_data,
      time_var = time_var, window_times = first_window_times,
      psis_log_weights = NULL
    )
  } else {
    NULL
  }

  # Walk forward over observation positions in all_unique_times.
  # Guard against the degenerate single-fold case (n_evals == 1L):
  # seq.int(2L, 1L) is c(2L, 1L) descending, which would iterate
  # with bogus k_eval. Skip the loop entirely when there's only
  # the initial fold to score.
  if (n_evals >= 2L) for (k_eval in seq.int(2L, n_evals)) {
    k <- eval_positions[k_eval]
    eval_time <- all_unique_times[k]
    if (silent < 1L) {
      cat("LFO eval at time", eval_time, "...\n")
    }

    # PSIS importance weights for moving from the idx_refit fit
    # to one trained through position k - 1. The log-ratio is
    # the cumulative log density of observations at positions
    # (idx_refit + 1):(k - 1).
    last_obs_positions <- seq.int(idx_refit + 1L, k - 1L)
    if (length(last_obs_positions) == 0L) {
      psis_lw <- NULL
      pareto_ks[k_eval] <- NA_real_
    } else {
      last_obs_times <- all_unique_times[last_obs_positions]
      last_obs_idx <- which(
        as.integer(all_data[[time_var]]) %in% last_obs_times
      )
      logratio <- lfo_sum_rows(
        loglik_past[, last_obs_idx, drop = FALSE]
      )
      psis_obj <- suppressWarnings(loo::psis(logratio))
      pareto_ks[k_eval] <- loo::pareto_k_values(psis_obj)[1L]
      psis_lw <- loo::weights.importance_sampling(
        psis_obj, normalize = TRUE
      )[, 1L]
    }

    if (!is.na(pareto_ks[k_eval]) &&
        pareto_ks[k_eval] > pareto_k_threshold_used) {
      idx_refit <- k - 1L
      refit_time <- all_unique_times[idx_refit]
      if (silent < 1L) {
        cat("  Pareto k", round(pareto_ks[k_eval], 3),
            "> threshold; refitting through time",
            refit_time, "\n")
      }
      splits <- lfo_cv_split(all_data, last_train = refit_time,
                              fc_horizon = fc_horizon,
                              time_var = time_var)
      fit_past <- update(fit_past, newdata = splits$data_train,
                          silent = silent)
      refits_at <- c(refits_at, refit_time)
      refit_triggered[k_eval] <- TRUE
      loglik_past <- log_lik(fit_past, newdata = all_data)
      psis_lw <- NULL  # Fresh fit; no reweighting needed.
    }

    window_times <- all_unique_times[k:(k + fc_horizon - 1L)]
    updates <- scores_at_window(
      fit = fit_past, all_data = all_data,
      time_var = time_var, series_var = series_var,
      window_times = window_times,
      score_names = score,
      elpds = elpds, score_arrays = score_arrays,
      eval_idx = k_eval, loglik = loglik_past,
      psis_log_weights = psis_lw,
      silent = silent
    )
    elpds <- updates$elpds
    score_arrays <- updates$score_arrays
    if (isTRUE(save_log_lik)) {
      log_lik_acc <- cbind(
        log_lik_acc,
        lfo_collect_fold_loglik(
          loglik = loglik_past, all_data = all_data,
          time_var = time_var, window_times = window_times,
          psis_log_weights = psis_lw
        )
      )
    }
  }

  sum_elpd <- if (!is.null(elpds)) sum(elpds, na.rm = TRUE) else NA

  structure(
    list(
      elpds = elpds,
      sum_ELPD = sum_elpd,
      scores = score_arrays,
      pareto_ks = pareto_ks,
      eval_timepoints = eval_timepoints,
      refits_at = refits_at,
      refit_triggered = refit_triggered,
      pareto_k_threshold = pareto_k_threshold,
      pareto_k_threshold_used = pareto_k_threshold_used,
      fc_horizon = fc_horizon,
      log_lik = log_lik_acc
    ),
    class = "mvgam_lfo"
  )
}


# Internal: extract the per-draw log-density matrix for the
# observations in `window_times` from the full-data loglik
# matrix, resampling draw rows by the PSIS weights when present.
# Returns an n_draws x n_window_obs matrix at draws representative
# of the LFO-adjusted posterior, ready for column-bind into a
# cumulative log-density store that `loo::loo_model_weights()`
# can stack on.
#
# When `psis_log_weights` is NULL the caller is at a fresh refit,
# so the full-data loglik IS the LFO loglik and no resampling
# is needed.
#'@noRd
lfo_collect_fold_loglik <- function(loglik, all_data, time_var,
                                      window_times,
                                      psis_log_weights) {
  fc_idx <- which(
    as.integer(all_data[[time_var]]) %in% window_times
  )
  if (length(fc_idx) == 0L) {
    return(matrix(NA_real_, nrow = nrow(loglik), ncol = 0L))
  }
  mat <- if (is.null(psis_log_weights)) {
    loglik[, fc_idx, drop = FALSE]
  } else {
    n_draws <- nrow(loglik)
    probs <- exp(psis_log_weights -
                   lfo_log_sum_exp(psis_log_weights))
    idx <- sample.int(n_draws, n_draws, replace = TRUE,
                       prob = probs)
    loglik[idx, fc_idx, drop = FALSE]
  }
  # Drop columns where every draw is NA. NAs arise at gaps in the
  # observed time series (y is missing) and would crash loo's
  # stacking optimiser, which rejects NAs in its log-likelihood
  # input. Within-column NAs (rare) are similarly removed by
  # dropping the column rather than imputing.
  keep <- colSums(is.na(mat)) == 0L
  mat[, keep, drop = FALSE]
}


# Internal: compute the requested scores at one evaluation window
# (rows of `all_data` whose time is in `window_times`). Writes
# into `elpds[eval_idx]` and `score_arrays[[s]][eval_idx]` and
# returns the updated containers.
#
# `window_times` is the explicit vector of time values to score,
# resolved by the caller from `all_unique_times[k:(k+fc_horizon-1)]`
# so the same code path handles regular and irregular grids
# uniformly.
#
# When `psis_log_weights` is supplied (PSIS-approximate step), the
# ELPD uses the log-sum-exp formula with the weights, and the
# non-ELPD scores resample forecast draw rows by `exp(lw)` to
# produce a forecast distribution under the PSIS-weighted
# posterior.
#'@noRd
scores_at_window <- function(fit, all_data, time_var, series_var,
                              window_times,
                              score_names,
                              elpds, score_arrays, eval_idx,
                              loglik, psis_log_weights,
                              silent) {
  fc_idx <- which(
    as.integer(all_data[[time_var]]) %in% window_times
  )
  if (length(fc_idx) == 0L) {
    return(list(elpds = elpds, score_arrays = score_arrays))
  }

  # ELPD.
  if ("elpd" %in% score_names) {
    per_draw_loglik <- lfo_sum_rows(
      loglik[, fc_idx, drop = FALSE]
    )
    if (is.null(psis_log_weights)) {
      elpds[eval_idx] <- lfo_log_mean_exp(per_draw_loglik)
    } else {
      elpds[eval_idx] <- lfo_log_sum_exp(
        psis_log_weights + per_draw_loglik
      )
    }
  }

  # Other scores: compute via forecast() + score().
  other_scores <- setdiff(score_names, "elpd")
  if (length(other_scores) > 0L) {
    fc_data <- all_data[fc_idx, , drop = FALSE]
    fc <- forecast(fit, newdata = fc_data, type = "response")
    if (!is.null(psis_log_weights)) {
      # Resample forecast draw rows under PSIS weights.
      n_draws <- nrow(fc$forecasts[[1L]])
      probs <- exp(psis_log_weights -
                     lfo_log_sum_exp(psis_log_weights))
      idx <- sample.int(n_draws, n_draws, replace = TRUE,
                          prob = probs)
      fc$forecasts <- lapply(fc$forecasts, function(m) {
        m[idx, , drop = FALSE]
      })
    }
    for (sc in other_scores) {
      s_out <- tryCatch(
        score(fc, score = sc),
        error = function(e) {
          if (silent < 2L) {
            cat("  score = ", sc, " failed at eval ", eval_idx,
                ": ", conditionMessage(e), "\n", sep = "")
          }
          NULL
        }
      )
      score_arrays[[sc]][eval_idx] <- lfo_aggregate_score(s_out, sc)
    }
  }

  list(elpds = elpds, score_arrays = score_arrays)
}


# Internal: extract a single scalar per-fold score from the
# score.mvgam_forecast return shape. score() yields a per-series
# data.frame with rows per horizon plus an `all_series` summary;
# we report the all_series sum for univariate-score outputs and
# the all_series row for multivariate (energy / variogram).
#'@noRd
lfo_aggregate_score <- function(s_out, sc) {
  if (is.null(s_out)) return(NA_real_)
  if ("all_series" %in% names(s_out)) {
    all_df <- s_out$all_series
    if (is.data.frame(all_df) && "score" %in% names(all_df)) {
      return(sum(all_df$score, na.rm = TRUE))
    }
  }
  # Fallback: sum every per-series score.
  vals <- vapply(s_out, function(d) {
    if (is.data.frame(d) && "score" %in% names(d)) {
      sum(d$score, na.rm = TRUE)
    } else {
      NA_real_
    }
  }, numeric(1L))
  sum(vals, na.rm = TRUE)
}


# Internal: split `data` into (train, test) on time <= last_train
# vs time in the next fc_horizon observed times after last_train.
# `last_train` is a time VALUE (must be present in the data).
# Comparisons use <= and ordered set membership so the function
# is correct for both regular and irregular CAR grids.
#'@noRd
lfo_cv_split <- function(data, last_train, fc_horizon,
                          time_var = "time") {
  t_vec <- as.integer(data[[time_var]])
  unique_times <- sort(unique(t_vec))
  idx_last <- match(last_train, unique_times)
  if (is.na(idx_last)) {
    # Caller should have validated; defensive fallback uses <=.
    train_idx <- which(t_vec <= last_train)
    test_idx <- which(t_vec > last_train)
  } else {
    train_times <- unique_times[seq_len(idx_last)]
    test_upper <- min(idx_last + fc_horizon, length(unique_times))
    test_times <- unique_times[
      seq.int(idx_last + 1L, test_upper)
    ]
    train_idx <- which(t_vec %in% train_times)
    test_idx <- which(t_vec %in% test_times)
  }
  list(
    data_train = data[train_idx, , drop = FALSE],
    data_test  = data[test_idx, , drop = FALSE]
  )
}


# Internal: numerically stable log-sum-exp.
#'@noRd
lfo_log_sum_exp <- function(x) {
  if (length(x) == 0L) return(-Inf)
  m <- max(x)
  m + log(sum(exp(x - m)))
}


# Internal: numerically stable log-mean-exp.
#'@noRd
lfo_log_mean_exp <- function(x) {
  lfo_log_sum_exp(x) - log(length(x))
}


# Internal: per-draw sum across observations. For a single-column
# input, returns the column with NAs stripped.
#'@noRd
lfo_sum_rows <- function(x) {
  if (NCOL(x) > 1L) {
    rowSums(x, na.rm = TRUE)
  } else {
    as.numeric(x)[!is.na(x)]
  }
}


#' Plot Pareto-k and per-fold scores from an `mvgam_lfo` object
#'
#' @description Renders a faceted ggplot of the per-fold Pareto-k
#'   diagnostic and every populated score (ELPD plus any non-ELPD
#'   scores requested at fit time). Outlier points are highlighted
#'   per these thresholds: Pareto-k above `pareto_k_threshold` and
#'   ELPD below its 15% quantile.
#'
#' @importFrom graphics layout axis lines abline polygon points
#' @param x An object of class `mvgam_lfo`.
#' @param ... Currently unused.
#' @return A `ggplot` object.
#'
#' @export
plot.mvgam_lfo <- function(x, ...) {
  obj <- x
  ks <- obj$pareto_ks
  ks[is.infinite(ks)] <-
    suppressWarnings(max(ks[!is.infinite(ks)], na.rm = TRUE))

  # Read the effective (numeric) threshold. Adaptive-default fits
  # (`pareto_k_threshold = NULL`) carry the applied value on
  # `pareto_k_threshold_used`; fits fitted before that slot
  # existed fall back to `pareto_k_threshold`.
  threshold_val <- obj$pareto_k_threshold_used %||%
    obj$pareto_k_threshold
  panels <- list()
  panels$pareto_ks <- data.frame(
    eval = obj$eval_timepoints,
    value = ks,
    threshold = threshold_val,
    facet = "Pareto K"
  )
  if (!is.null(obj$elpds)) {
    panels$elpds <- data.frame(
      eval = obj$eval_timepoints,
      value = obj$elpds,
      threshold = stats::quantile(obj$elpds, probs = 0.15,
                                    na.rm = TRUE),
      facet = "ELPD"
    )
  }
  if (!is.null(obj$scores)) {
    for (sc in names(obj$scores)) {
      panels[[sc]] <- data.frame(
        eval = obj$eval_timepoints,
        value = obj$scores[[sc]],
        threshold = stats::quantile(obj$scores[[sc]],
                                       probs = 0.85, na.rm = TRUE),
        facet = paste0(toupper(sc))
      )
    }
  }
  long <- do.call(rbind, panels)
  long$colour <- ifelse(
    (long$facet == "Pareto K" & long$value > long$threshold) |
      (long$facet == "ELPD" & long$value < long$threshold) |
      (long$facet %in% c("CRPS", "DRPS", "SIS", "BRIER",
                          "ENERGY", "VARIOGRAM") &
         long$value > long$threshold),
    "outlier", "inlier"
  )

  ggplot2::ggplot(
    long,
    ggplot2::aes(x = .data$eval, y = .data$value)
  ) +
    ggplot2::facet_wrap(~ .data$facet, ncol = 1,
                         scales = "free_y") +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = .data$threshold),
      colour = "#A25050", linetype = "dashed", linewidth = 1
    ) +
    ggplot2::geom_line(linewidth = 0.5, colour = "grey30") +
    ggplot2::geom_point(shape = 16, colour = "white", size = 2) +
    ggplot2::geom_point(
      ggplot2::aes(colour = .data$colour),
      shape = 16, show.legend = FALSE, size = 1.5
    ) +
    ggplot2::scale_colour_manual(
      values = c(inlier = "grey30", outlier = "#8F2727")
    ) +
    ggplot2::labs(x = "Evaluation time", y = NULL) +
    mvgam_theme()
}


#' Posterior summary of an `mvgam_lfo` object
#'
#' @description Returns a long-format tibble with one row per
#'   evaluation time point. Columns include the Pareto-k
#'   diagnostic, a refit flag, and the per-fold values of every
#'   requested score (ELPD plus any non-ELPD scores). Mirrors the
#'   single-layer summary convention used for `mvgam_forecast`,
#'   `mvgam_irf` and `mvgam_fevd`.
#'
#' @param object An object of class `mvgam_lfo`.
#' @param ... Currently unused.
#' @return A `tibble` (`tbl_df`) with one row per evaluation time
#'   point.
#'
#' @method summary mvgam_lfo
#' @export
summary.mvgam_lfo <- function(object, ...) {
  out <- data.frame(
    eval_time = object$eval_timepoints,
    # `refit_here` is TRUE when the evaluation at that time used
    # a fresh fit (either the initial refit at min_t or a
    # subsequent Pareto-k-triggered refit). Tracked as a parallel
    # logical vector during the lfo loop so the flag is robust to
    # irregular time grids.
    refit_here = object$refit_triggered,
    pareto_k = object$pareto_ks
  )
  if (!is.null(object$elpds)) {
    out$elpd <- object$elpds
  }
  if (!is.null(object$scores)) {
    for (sc in names(object$scores)) {
      out[[sc]] <- object$scores[[sc]]
    }
  }
  rownames(out) <- NULL
  class(out) <- c("tbl_df", "tbl", "data.frame")
  out
}


#' Compare leave-future-out evaluations across models
#'
#' Pairwise ELPD comparison for two or more `mvgam_lfo` objects,
#' modelled after `loo::loo_compare()` but operating on the per-fold
#' ELPDs accumulated by [lfo_cv()]. Returns a comparison table sorted
#' from best to worst by sum ELPD, with the standard error of the
#' difference computed pair-wise against the best model using the
#' aligned per-fold differences.
#'
#' @param x A `mvgam_lfo` object.
#' @param ... Additional `mvgam_lfo` objects to compare against `x`.
#' @param model_names Optional character vector of model labels;
#'   defaults to the deparsed call names.
#'
#' @return A `data.frame` (subclass `compare.loo`) with one row per
#'   model, sorted by `elpd_diff` descending. Columns:
#'   \describe{
#'     \item{`elpd_diff`}{Sum-ELPD difference vs. the best model
#'       (zero for the best).}
#'     \item{`se_diff`}{Standard error of `elpd_diff` using the
#'       per-fold paired-difference SE, scaled by `sqrt(N)`. Zero
#'       for the best model.}
#'     \item{`elpd_lfo`}{Sum of per-fold ELPDs.}
#'     \item{`se_elpd_lfo`}{Standard error of `elpd_lfo`.}
#'   }
#'
#' @details All compared `mvgam_lfo` objects must have been built
#'   over the same evaluation grid (same `eval_timepoints` and same
#'   `fc_horizon`). Models with non-aligned grids raise an error
#'   because paired differences would mix observations.
#'
#' @seealso [lfo_cv()], [loo_model_weights.mvgam_lfo()],
#'   [compare_elpds()], [plot.mvgam_compare_elpds()]
#'
#' @importFrom loo loo_compare
#' @method loo_compare mvgam_lfo
#' @export
loo_compare.mvgam_lfo <- function(x, ..., model_names = NULL) {
  checkmate::assert_class(x, "mvgam_lfo")
  extras <- list(...)
  for (m in extras) {
    checkmate::assert_class(m, "mvgam_lfo")
  }
  models <- c(list(x), extras)

  if (is.null(model_names)) {
    nms <- c(deparse(substitute(x)),
             vapply(substitute(...()), deparse, character(1L)))
    model_names <- nms
  }
  checkmate::assert_character(model_names, len = length(models),
                              any.missing = FALSE)

  # Align grids: every model must have the same eval_timepoints and
  # fc_horizon so per-fold differences are meaningful.
  ref_times <- models[[1L]]$eval_timepoints
  ref_h <- models[[1L]]$fc_horizon
  for (i in seq_along(models)) {
    if (!identical(models[[i]]$eval_timepoints, ref_times)) {
      stop(insight::format_error(c(
        "Cannot compare: eval_timepoints differ across models.",
        x = paste0("Model ", i, " has a different evaluation grid ",
                   "than model 1."),
        i = paste0("Refit lfo_cv() on each model with the same ",
                   "min_t and fc_horizon, against the same data.")
      )))
    }
    if (!identical(models[[i]]$fc_horizon, ref_h)) {
      stop(insight::format_error(c(
        "Cannot compare: fc_horizon differs across models.",
        x = paste0("Model ", i, " uses fc_horizon = ",
                   models[[i]]$fc_horizon,
                   "; model 1 uses ", ref_h, ".")
      )))
    }
  }

  for (i in seq_along(models)) {
    if (is.null(models[[i]]$elpds)) {
      stop(insight::format_error(c(
        paste0("Model ", i, " has no ELPDs; nothing to compare."),
        i = paste0("Call lfo_cv(..., score = 'elpd') (or include 'elpd' ",
                   "in the score vector) to populate ELPDs.")
      )))
    }
  }

  # Build per-model sum ELPD + standard error of the sum, then
  # paired-difference SE against the best model.
  elpd_mat <- vapply(models, function(m) m$elpds, numeric(length(ref_times)))
  if (!is.matrix(elpd_mat)) {
    elpd_mat <- matrix(elpd_mat, nrow = length(ref_times),
                       ncol = length(models))
  }
  n_folds <- nrow(elpd_mat)
  sum_elpd <- colSums(elpd_mat, na.rm = TRUE)
  se_sum <- sqrt(n_folds) * apply(elpd_mat, 2L, stats::sd, na.rm = TRUE)

  best <- which.max(sum_elpd)
  diff_mat <- elpd_mat - elpd_mat[, best]
  elpd_diff <- colSums(diff_mat, na.rm = TRUE)
  se_diff <- sqrt(n_folds) * apply(diff_mat, 2L, stats::sd, na.rm = TRUE)
  se_diff[best] <- 0
  elpd_diff[best] <- 0

  ord <- order(elpd_diff, decreasing = TRUE)
  pareto_k_list <- lapply(models, function(m) m$pareto_ks)
  diag_cols <- mvgam_loo_compare_diagnostics(
    elpd_diff = elpd_diff[ord],
    se_diff = se_diff[ord],
    n_pointwise = n_folds,
    pareto_k_list = pareto_k_list[ord]
  )
  out <- data.frame(
    elpd_diff = elpd_diff[ord],
    se_diff = se_diff[ord],
    p_worse = diag_cols$p_worse,
    diag_diff = diag_cols$diag_diff,
    diag_elpd = diag_cols$diag_elpd,
    elpd_lfo = sum_elpd[ord],
    se_elpd_lfo = se_sum[ord],
    row.names = model_names[ord],
    stringsAsFactors = FALSE
  )
  class(out) <- c("compare.loo", "matrix", "data.frame")
  out
}


#' Model weights from `mvgam_lfo` objects
#'
#' @description Derives ensemble weights from a set of `mvgam_lfo`
#'   objects. Two methods are supported (Yao et al. 2018):
#'   \itemize{
#'     \item `"pseudo-BMA"` (default): softmax of total LFO ELPD
#'       across models. Cheap, requires only the per-step ELPDs
#'       that every `mvgam_lfo` already carries. Collapses to a
#'       near-degenerate vector when models differ by more than
#'       ~10 ELPD units.
#'     \item `"stacking"`: convex optimisation on the pointwise
#'       LFO log-density matrix. Gives much softer weights than
#'       pseudo-BMA and is generally preferred. Requires the
#'       `$log_lik` matrix, which `lfo_cv()` only populates when
#'       called with `save_log_lik = TRUE` (off by default
#'       because the matrix can be many megabytes for long
#'       rolling windows). Errors with a hint to refit if the
#'       matrix is missing on any input.
#'   }
#'   Useful for combining multiple forecasts via
#'   [ensemble.mvgam_forecast()] when the candidate models were
#'   evaluated on the same rolling-origin grid.
#'
#' @param x An `mvgam_lfo` object.
#' @param ... Further `mvgam_lfo` objects. Models are weighted in
#'   the order they are passed.
#' @param method Character. Either `"pseudo-BMA"` (default) or
#'   `"stacking"`. See description.
#' @param model_names Optional character vector of model names.
#'   Defaults to the deparsed argument names.
#'
#' @return A named numeric vector of class `"pseudobma_weights"`
#'   (method = pseudo-BMA) or `"stacking_weights"` (method =
#'   stacking) summing to 1, with one entry per model.
#'
#' @references
#' Yao, Y., Vehtari, A., Simpson, D., Gelman, A. (2018). Using
#' stacking to average Bayesian predictive distributions.
#' *Bayesian Analysis* 13(3): 917-1003.
#' \doi{10.1214/17-BA1091}
#'
#' @seealso [loo_compare.mvgam_lfo()], [lfo_cv()],
#'   [ensemble.mvgam_forecast()], [compare_elpds()],
#'   [plot.mvgam_compare_elpds()]
#'
#' @importFrom loo loo_model_weights
#' @method loo_model_weights mvgam_lfo
#' @export
loo_model_weights.mvgam_lfo <- function(x, ...,
                                          method = "pseudo-BMA",
                                          model_names = NULL) {
  checkmate::assert_class(x, "mvgam_lfo")
  checkmate::assert_choice(method,
                            c("pseudo-BMA", "stacking"))
  extras <- list(...)
  for (m in extras) {
    checkmate::assert_class(m, "mvgam_lfo")
  }
  models <- c(list(x), extras)
  if (is.null(model_names)) {
    nms <- c(deparse(substitute(x)),
             vapply(substitute(...()), deparse, character(1L)))
    model_names <- nms
  }
  checkmate::assert_character(model_names, len = length(models),
                              any.missing = FALSE)

  # All models must share the same evaluation grid; otherwise the
  # paired comparison underlying either method would mix
  # observations from different rolling-origin schemes.
  ref_times <- models[[1L]]$eval_timepoints
  for (i in seq_along(models)) {
    if (!identical(models[[i]]$eval_timepoints, ref_times)) {
      stop(insight::format_error(c(
        "Cannot weight: eval_timepoints differ across models.",
        x = paste0("Model ", i, " has a different evaluation grid ",
                   "than model 1."),
        i = paste0("Refit lfo_cv() on each model with the same ",
                   "min_t and fc_horizon against the same data.")
      )))
    }
  }

  if (identical(method, "stacking")) {
    return(stack_mvgam_lfo(models, model_names))
  }

  # Pseudo-BMA: softmax of total LFO ELPD.
  total_elpd <- vapply(models, function(m) {
    if (is.null(m$elpds)) {
      stop(insight::format_error(c(
        "An mvgam_lfo object has no ELPDs; cannot weight.",
        i = paste0("Call lfo_cv(..., score = 'elpd') (or include 'elpd' ",
                   "in the score vector) to populate ELPDs.")
      )))
    }
    sum(m$elpds, na.rm = TRUE)
  }, numeric(1L))

  shifted <- total_elpd - max(total_elpd)
  w <- exp(shifted) / sum(exp(shifted))
  names(w) <- model_names

  class(w) <- "pseudobma_weights"
  attr(w, "method") <- "pseudo-BMA (mvgam_lfo)"
  w
}


# Internal: log-score stacking on a list of mvgam_lfo objects.
# Requires each $log_lik to be present (populated by lfo_cv with
# save_log_lik = TRUE) and identically shaped (same number of
# held-out evaluation observations across models). Delegates the
# convex optimisation to loo::loo_model_weights().
#'@noRd
stack_mvgam_lfo <- function(models, model_names) {
  ll_list <- lapply(seq_along(models), function(i) {
    ll <- models[[i]]$log_lik
    if (is.null(ll) || !is.matrix(ll) || ncol(ll) == 0L) {
      stop(insight::format_error(c(
        "Stacking needs the pointwise log-likelihood matrix.",
        x = paste0("Model ", i, " ('", model_names[i],
                   "') has no '$log_lik' slot."),
        i = paste0("Re-run lfo_cv() with save_log_lik = TRUE on ",
                   "every candidate model, then call again.")
      )))
    }
    ll
  })
  ref_cols <- ncol(ll_list[[1L]])
  for (i in seq_along(ll_list)) {
    if (ncol(ll_list[[i]]) != ref_cols) {
      stop(insight::format_error(c(
        "Pointwise log-likelihood matrices have different widths.",
        x = paste0("Model ", i, " has ", ncol(ll_list[[i]]),
                   " columns; model 1 has ", ref_cols, "."),
        i = paste0("Re-run lfo_cv() on all models against the ",
                   "same newdata and rolling-origin grid.")
      )))
    }
  }

  # Drop observations where any candidate's log-likelihood is NA
  # (typically gaps in the observed time series). The comparison
  # has to stay paired observation-for-observation across models.
  na_cols <- Reduce("|",
                     lapply(ll_list, function(m) {
                       colSums(is.na(m)) > 0L
                     }))
  if (any(na_cols)) {
    ll_list <- lapply(ll_list, function(m) {
      m[, !na_cols, drop = FALSE]
    })
  }

  # Collapse each model's per-draw matrix to the per-observation
  # marginal LFO log-predictive density: log mean exp over draws.
  # This bypasses the inner PSIS step that
  # `loo::loo_model_weights()` runs on raw log-likelihood
  # matrices, which would fire Pareto-k warnings because our rows
  # are already LFO-resampled draws (not a posterior in the shape
  # PSIS-LOO expects).
  lpd_point <- vapply(ll_list, function(m) {
    apply(m, 2L, lfo_log_mean_exp)
  }, numeric(ncol(ll_list[[1L]])))
  w <- loo::stacking_weights(lpd_point)
  names(w) <- model_names
  attr(w, "method") <- "stacking (mvgam_lfo)"
  w
}


#' Print headline summary of an `mvgam_lfo` object
#'
#' @description Prints a compact overview of the LFO run: horizon,
#'   threshold, number of refits, and per-score totals / means.
#'   For the full per-fold table, call [summary.mvgam_lfo()].
#'
#' @param x An object of class `mvgam_lfo`.
#' @param ... Currently unused.
#'
#' @method print mvgam_lfo
#' @export
print.mvgam_lfo <- function(x, ...) {
  # Report the numeric threshold actually used at the refit gate.
  # Adaptive-default fits stash it on `pareto_k_threshold_used`;
  # older fits (before that slot existed) fall back to the
  # user-supplied `pareto_k_threshold`.
  threshold_val <- x$pareto_k_threshold_used %||% x$pareto_k_threshold
  adaptive_label <- if (is.null(x$pareto_k_threshold)) " (adaptive)" else ""
  cat("Approximate leave-future-out cross-validation\n")
  cat("  fc_horizon         :", x$fc_horizon, "\n")
  cat("  pareto_k_threshold :", threshold_val,
      adaptive_label, "\n", sep = "")
  cat("  evaluation points  :", length(x$eval_timepoints), "\n")
  cat("  refits             :", length(x$refits_at), "\n")
  if (!is.null(x$elpds)) {
    cat("  ELPD               : sum =",
        format(round(sum(x$elpds, na.rm = TRUE), 2)),
        " mean =",
        format(round(mean(x$elpds, na.rm = TRUE), 2)),
        "\n")
  }
  if (!is.null(x$scores)) {
    for (sc in names(x$scores)) {
      cat("  ", sc, " : sum =",
          format(round(sum(x$scores[[sc]], na.rm = TRUE), 2)),
          " mean =",
          format(round(mean(x$scores[[sc]], na.rm = TRUE), 2)),
          "\n", sep = "")
    }
  }
  invisible(x)
}
