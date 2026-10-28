# Approximate leave-future-out cross-validation for mvgam fits, by the
# PSIS-LFO algorithm of Burkner, Gabry and Vehtari (2020). Scores other
# than the ELPD resample the forecast draws by their PSIS weights and
# pass them to `score.mvgam_forecast()`.


#' Approximate leave-future-out cross-validation of fitted mvgam
#' objects
#'
#' @description
#' Scores a model on its forecasts. The model is fitted to the times up
#' to `min_t` and scored on the next `fc_horizon` observed times. The
#' origin then steps forward one time at a time. Pareto smoothed
#' importance sampling (PSIS) reweights the posterior to each new
#' origin. The model is refitted only where the Pareto k exceeds
#' `pareto_k_threshold`. The algorithm is that of Burkner, Gabry and
#' Vehtari (2020), as in the
#' [loo PSIS-LFO vignette](https://mc-stan.org/loo/articles/loo2-lfo.html).
#'
#' @name lfo_cv.mvgam
#' @importFrom stats update logLik quantile
#'
#' @param object A fitted [mvgam][mvgam::mvgam] object.
#' @param newdata Optional `data.frame` holding the response, the time
#'   and series columns and the covariates. `NULL` (the default) takes
#'   the training data. Every series must share its observed times,
#'   except under `CAR()`, whose series may be observed at their own
#'   times.
#' @param data Deprecated alias for `newdata`.
#' @param min_t The time at which the first training window ends. It
#'   must be a time present in the data, which need not be whole for an
#'   irregular `CAR()` grid. `NULL` (the default) takes the 30th
#'   observed time, earlier for a shorter series, leaving at least 10
#'   evaluation origins where the data allow.
#' @param fc_horizon Integer number of observed times after each origin
#'   that are scored, whatever the gaps between them. Defaults to 1.
#' @param pareto_k_threshold Numeric. An origin whose Pareto k exceeds
#'   it triggers a refit. `NULL` (the default) takes the adaptive
#'   threshold `min(1 - 1 / log10(S), 0.7)` for `S` posterior draws
#'   (Vehtari et al. 2024). It is stricter for a fit with few draws,
#'   where PSIS is less reliable.
#' @param score Character vector of scores to compute at each origin,
#'   from `"elpd"` (the default), `"crps"`, `"drps"`, `"sis"`,
#'   `"brier"`, `"energy"` and `"variogram"`. The ELPD takes the PSIS
#'   log-likelihood. The others score forecast draws resampled by the
#'   PSIS weights, with [score.mvgam_forecast()].
#' @param save_log_lik Logical. `TRUE` keeps the held-out log densities
#'   under the reweighted posterior, an `[ndraws, n_eval_obs]` matrix.
#'   [loo_model_weights.mvgam_lfo()] stacks models by it. Defaults to
#'   `FALSE`, the matrix being large.
#' @param silent Integer in `\{0, 1, 2\}`. `0` prints each refit
#'   and its sampler start and finish lines. `1` (default) and `2`
#'   print nothing from the refits.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A list of class `mvgam_lfo`, with one element per origin in
#'   each vector.
#'   * `elpds`: the approximate ELPD of each origin, `NULL` unless
#'     `"elpd"` is in `score`. `sum_ELPD` is their sum.
#'   * `scores`: a named list with one vector for each other score.
#'   * `pareto_ks`: the Pareto k of each origin's importance weights,
#'     `NA` where the model was refitted.
#'   * `eval_timepoints`: the first scored time of each origin.
#'   * `n_obs`: the number of responses each origin scores.
#'   * `refits_at`: the last training time of each refit.
#'   * `refit_triggered`: `TRUE` at each origin scored on a refit.
#'   * `pareto_k_threshold`: the threshold used. `TRUE` in
#'     `pareto_k_threshold_adaptive` marks the adaptive rule.
#'   * `fc_horizon`: the horizon.
#'   * `log_lik`: the held-out log densities when `save_log_lik = TRUE`,
#'     otherwise `NULL`.
#'
#' @references
#' Paul-Christian Burkner, Jonah Gabry and Aki Vehtari (2020).
#' Approximate leave-future-out cross-validation for Bayesian time
#' series models. *Journal of Statistical Computation and
#' Simulation*. 90:14, 2499-2523.
#' \doi{10.1080/00949655.2020.1783262}
#'
#' Aki Vehtari, Daniel Simpson, Andrew Gelman, Yuling Yao and
#' Jonah Gabry (2024). Pareto smoothed importance sampling.
#' *Journal of Machine Learning Research*. 25(72), 1-58.
#' <https://jmlr.org/papers/v25/19-556.html>
#'
#' @seealso [loo_compare.mvgam_lfo()] ranks models by their
#'   leave-future-out ELPD. [loo_model_weights.mvgam_lfo()] weights
#'   them. [loo.mvgam()] approximates leave-one-out cross-validation.
#'   [kfold.mvgam()] runs grouped k-fold cross-validation. The
#'   `r mvgam_article("forecast_evaluation", "forecast evaluation article")`
#'   runs `lfo_cv()` on rodent counts, with its Pareto k refits and
#'   stacking weights.
#'
#' @examples
#' \dontrun{
#' set.seed(11)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR())
#'
#' mod <- mvgam(
#'   y ~ s(x),
#'   trend_formula = ~ AR(p = 1),
#'   data    = simdat$data_train,
#'   family  = poisson(),
#'   chains  = 2, silent = 2
#' )
#'
#' # Forecasts one time step after each origin from t = 60, scored by
#' # ELPD and CRPS. The model is refitted only where the Pareto k is high.
#' lfo <- lfo_cv(mod, min_t = 60L, score = c("elpd", "crps"))
#' lfo
#'
#' # Pareto k, ELPD and CRPS at each origin. Points past the dashed
#' # line are highlighted
#' plot(lfo)
#'
#' # One row per origin, with the refits in 'refit_here'
#' summary(lfo)
#'
#' # Next, rank models by their forecasts with loo_compare()
#' }
#'
#' @author Nicholas J Clark
#' @family forecasting
#' @family model comparison
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
  # A time value, which on a CAR grid is rarely whole. Membership in the
  # observed times is checked below.
  checkmate::assert_number(min_t, null.ok = TRUE, finite = TRUE)
  checkmate::assert_flag(save_log_lik)
  checkmate::assert_int(silent, lower = 0L, upper = 2L)
  allowed_scores <- c("elpd", "crps", "drps", "sis", "brier",
                       "energy", "variogram")
  checkmate::assert_subset(score, allowed_scores, empty.ok = FALSE)
  score <- unique(score)
  rlang::check_dots_empty()

  newdata <- mvgam_use_alias(newdata, data, c("newdata", "data"))

  all_data <- prediction_frame(object, newdata)

  time_var <- axis_vars(object)$time_var
  # Only the time column is required. `axis_row_series()` assigns rows
  # to series from the axis record. A hierarchical frame derives its
  # series. A wide `mvbf()` frame has no series column.
  assert_axis_column(all_data, time_var, "time")
  # A rolling origin needs every series on one time grid. A
  # response-keyed frame puts every response on every row. The record
  # returns `NULL` for it, its responses sharing the grid.
  series_fac <- axis_row_series(object, all_data)
  # A trend that completes its grid holds a state for every series at
  # every time. Each fold then scores the series observed in it.
  if (!is.null(series_fac) &&
      !trend_completes_time_grid(first_trend_spec(object))) {
    refuse_ragged_trend_grid(
      all_data[[time_var]], series_fac,
      headline = "'lfo_cv()' requires all series to share one time grid.",
      remedy = "A series with its own times takes its own 'lfo_cv()' call."
    )
  }

  # The origins run over every time the evaluation frame holds, which
  # may extend past the training grid. The series assignment above
  # comes from the fit.
  all_unique_times <- sort(unique(all_data[[time_var]]))
  n_times <- length(all_unique_times)

  # fc_horizon must leave at least one training observation.
  if (fc_horizon >= n_times) {
    stop(insight::format_error(c(
      paste0("'fc_horizon' (", fc_horizon,
             ") exceeds the available time span (",
             n_times, " observed times)."),
      i = paste0("Choose 'fc_horizon' < ", n_times,
                 " to leave room for at least one ",
                 "training observation.")
    )))
  }

  # The default `min_t` is the 30th, 20th, 10th or first observed time,
  # the largest the series allows. Indexing the observed times works for
  # any time origin, such as years or Julian days.
  if (is.null(min_t)) {
    base_idx <- if (n_times > 30L) 30L else
      if (n_times > 20L) 20L else
        if (n_times > 10L) 10L else 1L
    # At least 10 evaluation origins where the data allow
    base_idx <- max(1L, min(base_idx, n_times - 10L - fc_horizon))
    min_t <- all_unique_times[base_idx]
  }

  if (!min_t %in% all_unique_times) {
    # The first few occasions are listed. A value inside the range is
    # rarely an occasion of an irregular grid.
    shown <- utils::head(all_unique_times, 6L)
    stop(insight::format_error(c(
      "'min_t' must be an observed time.",
      x = paste0("Got min_t = ", min_t, "."),
      x = paste0(
        "The fit has ", n_times, " occasions, beginning ",
        paste(format(shown, trim = TRUE), collapse = ", "),
        if (n_times > length(shown)) ", ..." else "",
        " and ending ", format(max(all_unique_times), trim = TRUE),
        "."
      ),
      i = "Pass one of those values."
    )), call. = FALSE)
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

  # The arithmetic below is over positions in `all_unique_times`. The
  # CAR kernel takes the gaps between them from `time_dis`.
  eval_positions <- seq.int(idx_min_t + 1L,
                              n_times - fc_horizon + 1L)
  eval_timepoints <- all_unique_times[eval_positions]
  n_evals <- length(eval_positions)

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
  refits_at <- all_unique_times[0L]
  # `TRUE` at each origin the model was refitted for. The first refit is
  # at `min_t`. Later refits follow a Pareto k above the threshold.
  # `summary.mvgam_lfo()` reports it as `refit_here`.
  refit_triggered <- logical(n_evals)
  refit_triggered[1L] <- TRUE

  if (silent < 1L) {
    cat("LFO refit at training time", min_t, "...\n")
  }
  train_frame <- lfo_training_frame(object, all_data,
                                     last_train = min_t,
                                     time_var = time_var)
  fit_past <- refit_on_held_out(object, train_frame, silent)
  refits_at <- c(refits_at, min_t)

  # The log likelihood of every row, indexed by each later window. It
  # scores each observation under the latent state inferred at its
  # time, the density an ELPD is built from. The refit held the later
  # rows with their responses masked. It inferred a state at each of
  # their times.
  loglik_past <- log_lik(fit_past, newdata = all_data)
  idx_refit <- idx_min_t

  # The responses each fold scores. A fold's score is a sum over them.
  # Folds of series observed at their own times hold different
  # numbers.
  eval_n_obs <- vapply(eval_positions, function(k) {
    rows <- rows_at_times(
      all_data, time_var, all_unique_times[k:(k + fc_horizon - 1L)]
    )
    sum(!is.na(loglik_past[1L, rows]))
  }, integer(1L))

  # The threshold the refits are judged by, which the result reports.
  # The adaptive rule of Vehtari et al. (2024) is stricter for few draws
  # and stops at 0.7.
  pareto_k_threshold_used <- resolve_pareto_k_threshold(
    pareto_k_threshold, nrow(loglik_past)
  )

  first_window_times <- all_unique_times[
    (idx_min_t + 1L):(idx_min_t + fc_horizon)
  ]
  updates <- scores_at_window(
    fit = fit_past, all_data = all_data,
    time_var = time_var,
    window_times = first_window_times,
    score_names = score,
    elpds = elpds, score_arrays = score_arrays,
    eval_idx = 1L, loglik = loglik_past,
    psis_log_weights = NULL
  )
  elpds <- updates$elpds
  score_arrays <- updates$score_arrays

  # The held-out log densities under the reweighted posterior, an
  # `[ndraws, n_eval_obs]` matrix for `loo::loo_model_weights()`
  log_lik_acc <- if (isTRUE(save_log_lik)) {
    lfo_collect_fold_loglik(
      loglik = loglik_past, all_data = all_data,
      time_var = time_var, window_times = first_window_times,
      psis_log_weights = NULL
    )
  } else {
    NULL
  }

  for (k_eval in seq_len(n_evals)[-1L]) {
    k <- eval_positions[k_eval]
    eval_time <- all_unique_times[k]
    if (silent < 1L) {
      cat("LFO eval at time", eval_time, "...\n")
    }

    # PSIS weights from the last refit to a fit through position
    # k - 1. The log ratio is the summed log density of the
    # observations at positions (idx_refit + 1):(k - 1). The bound is
    # checked because `seq.int()` counts down when it is empty.
    logratio <- if (k - 1L >= idx_refit + 1L) {
      last_obs_times <- all_unique_times[seq.int(idx_refit + 1L, k - 1L)]
      sum_measured(loglik_past[
        , rows_at_times(all_data, time_var, last_obs_times), drop = FALSE
      ], dims = 1L)
    }
    # With no response observed since the last refit, the posterior
    # is the refit's own and there is nothing to reweight. A gap in
    # sampling does this: every series is missing at those times and
    # `sum_measured()` returns `NA` for every draw.
    if (is.null(logratio) || all(is.na(logratio))) {
      psis_lw <- NULL
      pareto_ks[k_eval] <- NA_real_
    } else {
      psis_obj <- with_pareto_k_handled(loo::psis(logratio, r_eff = 1))
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
      train_frame <- lfo_training_frame(object, all_data,
                                         last_train = refit_time,
                                         time_var = time_var)
      fit_past <- refit_on_held_out(fit_past, train_frame, silent)
      refits_at <- c(refits_at, refit_time)
      refit_triggered[k_eval] <- TRUE
      loglik_past <- log_lik(fit_past, newdata = all_data)
      psis_lw <- NULL
    }

    window_times <- all_unique_times[k:(k + fc_horizon - 1L)]
    updates <- scores_at_window(
      fit = fit_past, all_data = all_data,
      time_var = time_var,
      window_times = window_times,
      score_names = score,
      elpds = elpds, score_arrays = score_arrays,
      eval_idx = k_eval, loglik = loglik_past,
      psis_log_weights = psis_lw
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
      n_obs = eval_n_obs,
      refits_at = refits_at,
      refit_triggered = refit_triggered,
      pareto_k_threshold = pareto_k_threshold_used,
      pareto_k_threshold_adaptive = is.null(pareto_k_threshold),
      fc_horizon = fc_horizon,
      log_lik = log_lik_acc
    ),
    class = "mvgam_lfo"
  )
}


#' Held-out log-densities of one window under the LFO posterior
#'
#' With PSIS weights the draw rows are resampled by weight, giving
#' draws representative of the LFO-adjusted posterior. Without weights
#' the caller is at a fresh refit, where the full-data log-likelihood
#' is already the LFO one. Columns holding a missing value are dropped.
#' The stacking optimiser of `loo::loo_model_weights()` refuses them.
#'
#' @param loglik Numeric log-likelihood matrix of the full data, one row
#'   per draw
#' @param all_data Data frame of the full data
#' @param time_var Character name of the time column
#' @param window_times Vector of the time values in the window
#' @param psis_log_weights Numeric vector of PSIS log weights, one per
#'   draw, or `NULL`
#' @return Numeric matrix with one row per draw and one column per
#'   scored observation in the window
#' @noRd
lfo_collect_fold_loglik <- function(loglik, all_data, time_var,
                                      window_times,
                                      psis_log_weights) {
  checkmate::assert_matrix(loglik)
  checkmate::assert_data_frame(all_data)
  checkmate::assert_string(time_var)
  checkmate::assert_atomic_vector(window_times)
  checkmate::assert_numeric(psis_log_weights, len = nrow(loglik),
                            null.ok = TRUE)
  fc_idx <- rows_at_times(all_data, time_var, window_times)
  if (length(fc_idx) == 0L) {
    return(matrix(NA_real_, nrow = nrow(loglik), ncol = 0L))
  }
  mat <- if (is.null(psis_log_weights)) {
    loglik[, fc_idx, drop = FALSE]
  } else {
    loglik[psis_resample(psis_log_weights), fc_idx, drop = FALSE]
  }
  # A missing response leaves its column `NA`
  keep <- colSums(is.na(mat)) == 0L
  mat[, keep, drop = FALSE]
}


#' The requested scores at one evaluation window
#'
#' The caller resolves `window_times` from the ordered unique times,
#' covering regular and irregular grids alike. With PSIS weights the
#' ELPD is the weighted log-sum-exp. The other scores then resample the
#' predictive draw rows by weight.
#'
#' @param fit Refitted `mvgam` object
#' @param all_data Data frame of the full data
#' @param time_var Character name of the time column
#' @param window_times Vector of the time values in the window
#' @param score_names Character vector of the scores to compute
#' @param elpds Numeric vector of per-window ELPDs
#' @param score_arrays Named list of per-window score vectors
#' @param eval_idx Integer index of this window
#' @param loglik Numeric log-likelihood matrix of the full data
#' @param psis_log_weights Numeric vector of PSIS log weights, or
#'   `NULL`
#' @return List holding the updated `elpds` and `score_arrays`
#' @noRd
scores_at_window <- function(fit, all_data, time_var,
                              window_times,
                              score_names,
                              elpds, score_arrays, eval_idx,
                              loglik, psis_log_weights) {
  checkmate::assert_class(fit, "mvgam")
  checkmate::assert_data_frame(all_data)
  checkmate::assert_string(time_var)
  checkmate::assert_atomic_vector(window_times)
  checkmate::assert_character(score_names, min.len = 1L)
  checkmate::assert_numeric(elpds, null.ok = TRUE)
  checkmate::assert_list(score_arrays, null.ok = TRUE)
  checkmate::assert_int(eval_idx, lower = 1L)
  checkmate::assert_matrix(loglik)
  checkmate::assert_numeric(psis_log_weights, null.ok = TRUE)
  fc_idx <- rows_at_times(all_data, time_var, window_times)
  if (length(fc_idx) == 0L) {
    return(list(elpds = elpds, score_arrays = score_arrays))
  }

  if ("elpd" %in% score_names) {
    per_draw_loglik <- sum_measured(loglik[, fc_idx, drop = FALSE],
                                    dims = 1L)
    if (is.null(psis_log_weights)) {
      elpds[eval_idx] <- log_mean_exp(per_draw_loglik)
    } else {
      elpds[eval_idx] <- log_sum_exp(
        psis_log_weights + per_draw_loglik
      )
    }
  }

  # The other scores take predictive draws of the same latent states
  # the ELPD used. `window_predictive()` explains why these are not
  # `forecast()` draws.
  other_scores <- setdiff(score_names, "elpd")
  if (length(other_scores) > 0L) {
    fc <- window_predictive(fit, all_data[fc_idx, , drop = FALSE])
    if (!is.null(psis_log_weights)) {
      idx <- psis_resample(psis_log_weights)
      fc$forecasts <- lapply(fc$forecasts, function(m) {
        m[idx, , drop = FALSE]
      })
    }
    for (sc in other_scores) {
      score_arrays[[sc]][eval_idx] <- lfo_aggregate_score(
        score(fc, score = sc)
      )
    }
  }

  list(elpds = elpds, score_arrays = score_arrays)
}


#' Draw indices resampled by PSIS weight
#'
#' The resampled draws represent the reweighted posterior.
#'
#' @param log_weights Numeric vector of PSIS log weights, one per draw
#' @return Integer vector of draw indices, as long as `log_weights`
#' @noRd
psis_resample <- function(log_weights) {
  checkmate::assert_numeric(log_weights, any.missing = FALSE,
                            min.len = 1L)
  n_draws <- length(log_weights)
  sample.int(n_draws, n_draws, replace = TRUE,
             prob = exp(log_weights - log_sum_exp(log_weights)))
}


#' One scalar score per fold from the output of `score()`
#'
#' Sums the `all_series` scores. A multivariate fit on a long frame
#' scores each response. Its fold score sums the responses. A fold with
#' no observation has no score.
#'
#' @param s_out List returned by `score()`, or `NULL`
#' @return Numeric scalar score
#' @noRd
lfo_aggregate_score <- function(s_out) {
  checkmate::assert_list(s_out, null.ok = TRUE)
  if (is.null(s_out)) return(NA_real_)
  if (is.null(s_out$all_series)) {
    return(sum_measured(vapply(s_out, lfo_aggregate_score, numeric(1L))))
  }
  sum_measured(s_out$all_series$score)
}


#' The frame a window ending at `last_train` is refitted on
#'
#' Every occasion after `last_train` is held out by masking its
#' response with `mask_heldout_response()`. The occasions stay in the
#' frame. Deleting the rows would shorten the trend grid. The refit
#' would then estimate no latent state past `last_train`. `log_lik()`
#' would score the held-out rows against the training mean. Masking
#' keeps `N_time_trend` at the full grid. Each held-out occasion then
#' carries a state the trend kernel runs forward from the last observed
#' one. A refit at `m` gives the `h`-step forecast state
#' `p(z_{m+h} | y_{1:m})`. The PSIS weights supply the sequential
#' conditioning that turns these into `p(y_k | y_{1:k-1})`.
#'
#' The loo vignette deletes the rows. Its models are
#' observation-driven, with the conditioning information in the data.
#' Here the conditioning information is a latent parameter. The
#' held-out responses contribute no likelihood term, leaving the
#' parameter posterior unchanged.
#'
#' @param object Fitted `mvgam` object
#' @param data Data frame of the full data
#' @param last_train Time value of the last training occasion. Matching
#'   against the ordered unique times handles irregular CAR grids.
#' @param time_var Character name of the time column
#' @return `data` with every response after `last_train` set to `NA`
#' @noRd
lfo_training_frame <- function(object, data, last_train,
                                time_var = "time") {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_data_frame(data)
  checkmate::assert_atomic_vector(last_train, len = 1L)
  checkmate::assert_string(time_var)
  t_vec <- data[[time_var]]
  unique_times <- sort(unique(t_vec))
  idx_last <- match(last_train, unique_times)
  held_rows <- if (is.na(idx_last)) {
    which(t_vec > last_train)
  } else {
    which(t_vec %in% unique_times[-seq_len(idx_last)])
  }
  mask_heldout_response(object, data, held_rows)
}


#' The rows of a data frame observed at any of a set of times
#'
#' Compares the time values without truncation. A continuous time grid
#' needs exact matches.
#'
#' @param data Data frame holding the time column
#' @param time_var Character name of the time column
#' @param times Vector of time values
#' @return Integer vector of row indices
#' @noRd
rows_at_times <- function(data, time_var, times) {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(time_var)
  checkmate::assert_atomic_vector(times)
  which(data[[time_var]] %in% times)
}


#' Plot Pareto-k and per-fold scores from an `mvgam_lfo` object
#'
#' @description One panel for the Pareto k of each fold and one for
#'   each score. The dashed line in each panel is the threshold above
#'   which points are highlighted. For Pareto k it is the threshold
#'   [lfo_cv()] used. For ELPD it is the 15% quantile, with lower
#'   values highlighted. For the other scores it is the 85% quantile.
#'
#' @param x An object of class `mvgam_lfo`.
#' @param ... Unused. Anything passed here is refused.
#' @return A `ggplot` object.
#'
#' @export
plot.mvgam_lfo <- function(x, ...) {
  rlang::check_dots_empty()
  t <- x$eval_timepoints
  panels <- c(
    list(pareto_k_panel(t, x$pareto_ks, x$pareto_k_threshold)),
    if (!is.null(x$elpds)) {
      list(cv_panel(t, x$elpds, "ELPD", probs = 0.15, worse_above = FALSE))
    },
    lapply(names(x$scores), function(sc) {
      cv_panel(t, x$scores[[sc]], score_label(sc), probs = 0.85)
    })
  )
  cv_diagnostic_plot(do.call(rbind, panels)) +
    scale_x_steps("Evaluation time", whole = all(t == round(t)))
}


#' Posterior summary of an `mvgam_lfo` object
#'
#' @description One row per evaluation time. `refit_here` is `TRUE`
#'   where the model was refitted for that origin. `pareto_k` is the
#'   Pareto k of the importance weights, `NA` at a refit. `n_obs`
#'   counts the responses scored. One column follows for each score
#'   requested from [lfo_cv()].
#'
#' @param object An object of class `mvgam_lfo`.
#' @param ... Unused. Anything passed here is refused.
#' @return A `tibble` with columns `eval_time`, `refit_here`,
#'   `pareto_k`, `n_obs` and one per score.
#'
#' @method summary mvgam_lfo
#' @export
summary.mvgam_lfo <- function(object, ...) {
  rlang::check_dots_empty()
  out <- data.frame(
    eval_time = object$eval_timepoints,
    refit_here = object$refit_triggered,
    pareto_k = object$pareto_ks,
    # The number of responses the fold scores
    n_obs = object$n_obs
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
#' Ranks two or more [lfo_cv()] results by their summed ELPD, in the
#' layout of [loo::loo_compare()]. The standard error of each
#' difference comes from the paired per-fold differences with the best
#' model. The results must share one evaluation grid, with the same
#' `min_t`, `fc_horizon` and data.
#'
#' @inheritParams compare_scores
#' @param x An `mvgam_lfo` object.
#' @param ... Further `mvgam_lfo` objects.
#'
#' @return A `compare.loo` table with one row per model, best first.
#'   \describe{
#'     \item{`elpd_diff`}{Summed ELPD difference from the best model,
#'       zero for the best.}
#'     \item{`se_diff`}{Standard error of `elpd_diff`,
#'       `sqrt(N) * sd(fold_diff)` over the `N` folds.}
#'     \item{`p_worse`, `diag_diff`, `diag_elpd`}{The diagnostics of
#'       [loo_compare.mvgam()], with folds in place of observations.}
#'     \item{`elpd_lfo`}{Summed per-fold ELPD.}
#'     \item{`se_elpd_lfo`}{Standard error of `elpd_lfo`.}
#'   }
#'
#' @seealso [lfo_cv()], [loo_model_weights.mvgam_lfo()],
#'   [compare_elpds()], [plot.mvgam_compare_elpds()]
#'
#' @importFrom loo loo_compare
#' @family model comparison
#' @method loo_compare mvgam_lfo
#' @export
loo_compare.mvgam_lfo <- function(x, ..., model_names = NULL) {
  models <- list(x, ...)
  assert_compared_objects(models, "mvgam_lfo", "loo_compare")
  model_names <- compared_object_names(
    models, as.list(substitute(list(x, ...)))[-1L], model_names
  )

  assert_aligned_lfo(models)
  ref_times <- models[[1L]]$eval_timepoints

  for (i in seq_along(models)) {
    if (is.null(models[[i]]$elpds)) {
      stop(insight::format_error(c(
        paste0("Model ", i, " has no ELPDs to compare."),
        i = "Include 'elpd' in the 'score' argument of lfo_cv()."
      )))
    }
  }

  elpd_mat <- vapply(models, function(m) m$elpds, numeric(length(ref_times)))
  if (!is.matrix(elpd_mat)) {
    elpd_mat <- matrix(elpd_mat, nrow = length(ref_times),
                       ncol = length(models))
  }
  n_folds <- nrow(elpd_mat)
  sum_elpd <- colSums(elpd_mat, na.rm = TRUE)
  se_sum <- apply(elpd_mat, 2L, se_of_sum)

  best <- which.max(sum_elpd)
  diff_mat <- elpd_mat - elpd_mat[, best]
  elpd_diff <- colSums(diff_mat, na.rm = TRUE)
  se_diff <- apply(diff_mat, 2L, se_of_sum)
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
#' @description Weights for combining models evaluated by [lfo_cv()]
#'   on one rolling-origin grid, for example in
#'   [ensemble.mvgam_forecast()]. Yao et al. (2018) describe both
#'   methods.
#'   \itemize{
#'     \item `"pseudo-BMA"`: the softmax of each model's summed ELPD.
#'       A gap of more than about 10 ELPD units gives the better model
#'       nearly all the weight.
#'     \item `"stacking"`: weights that maximise the log score of the
#'       combined predictive over the held-out observations. They
#'       spread weight more evenly. Each result needs the pointwise
#'       matrix that `lfo_cv(save_log_lik = TRUE)` stores.
#'   }
#'
#' @inheritParams compare_scores
#' @param x An `mvgam_lfo` object.
#' @param ... Further `mvgam_lfo` objects, weighted in the order given.
#' @param method `"pseudo-BMA"` (the default) or `"stacking"`.
#'
#' @return A named numeric vector of weights summing to 1, of class
#'   `"pseudobma_weights"` or `"stacking_weights"`.
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
  checkmate::assert_choice(method,
                            c("pseudo-BMA", "stacking"))
  models <- list(x, ...)
  assert_compared_objects(models, "mvgam_lfo", "loo_model_weights",
                          min_n = 1L)
  model_names <- compared_object_names(
    models, as.list(substitute(list(x, ...)))[-1L], model_names
  )

  assert_aligned_lfo(models)

  if (identical(method, "stacking")) {
    return(stack_mvgam_lfo(models, model_names))
  }

  total_elpd <- vapply(models, function(m) {
    if (is.null(m$elpds)) {
      stop(insight::format_error(c(
        "An mvgam_lfo object has no ELPDs to weight.",
        i = "Include 'elpd' in the 'score' argument of lfo_cv()."
      )))
    }
    sum(m$elpds, na.rm = TRUE)
  }, numeric(1L))

  w <- softmax(total_elpd)
  names(w) <- model_names

  class(w) <- "pseudobma_weights"
  attr(w, "method") <- "pseudo-BMA (mvgam_lfo)"
  w
}


#' Log-score stacking weights for LFO results
#'
#' Each result needs the `log_lik` matrix that
#' `lfo_cv(save_log_lik = TRUE)` stores. Every matrix must have the
#' same number of held-out observations. `loo::loo_model_weights()`
#' solves the stacking problem.
#'
#' @param models List of `mvgam_lfo` objects
#' @param model_names Character vector of labels, one per model
#' @return Stacking weights from `loo::loo_model_weights()`
#' @noRd
stack_mvgam_lfo <- function(models, model_names) {
  checkmate::assert_list(models, min.len = 1L)
  checkmate::assert_character(model_names, len = length(models))
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
                   " columns, model 1 has ", ref_cols, "."),
        i = paste0("Run 'lfo_cv()' again on all models with the same ",
                   "'newdata' and rolling-origin grid.")
      )), call. = FALSE)
    }
  }

  # An observation missing from any model is dropped from all of them.
  # Stacking pairs the models observation by observation.
  na_cols <- Reduce("|",
                     lapply(ll_list, function(m) {
                       colSums(is.na(m)) > 0L
                     }))
  if (any(na_cols)) {
    ll_list <- lapply(ll_list, function(m) {
      m[, !na_cols, drop = FALSE]
    })
  }

  # The log predictive density of each observation is the log mean
  # exp over the resampled draws. `loo::stacking_weights()` takes it
  # directly. `loo::loo_model_weights()` would run PSIS-LOO on draws
  # that are already reweighted.
  lpd_point <- vapply(ll_list, function(m) {
    apply(m, 2L, log_mean_exp)
  }, numeric(ncol(ll_list[[1L]])))
  w <- loo::stacking_weights(lpd_point)
  names(w) <- model_names
  attr(w, "method") <- "stacking (mvgam_lfo)"
  w
}


#' Print headline summary of an `mvgam_lfo` object
#'
#' @description Prints the horizon, the Pareto k threshold and the
#'   counts of evaluations and refits. Each score is given as its sum
#'   and mean over folds.
#'   [summary.mvgam_lfo()] returns the per-fold table.
#'
#' @param x An object of class `mvgam_lfo`.
#' @param ... Unused.
#'
#' @return The `mvgam_lfo` object `x`, returned invisibly.
#'
#' @method print mvgam_lfo
#' @export
print.mvgam_lfo <- function(x, ...) {
  totals <- function(v) {
    paste0("sum = ", format(round(sum(v, na.rm = TRUE), 2)),
           ", mean = ", format(round(mean(v, na.rm = TRUE), 2)))
  }
  rows <- c(
    fc_horizon = x$fc_horizon,
    pareto_k_threshold = pareto_k_threshold_label(x, digits = 3L),
    `evaluation points` = length(x$eval_timepoints),
    refits = length(x$refits_at)
  )
  if (!is.null(x$elpds)) rows["ELPD"] <- totals(x$elpds)
  for (sc in names(x$scores)) rows[sc] <- totals(x$scores[[sc]])
  cat_field_rows("Approximate leave-future-out cross-validation", rows)
  invisible(x)
}


#' Print a title and aligned `name : value` rows
#'
#' @param title Character title line
#' @param rows Named character vector of values
#' @return `NULL`, invisibly
#' @noRd
cat_field_rows <- function(title, rows) {
  checkmate::assert_string(title)
  checkmate::assert_atomic_vector(rows, names = "unique")
  cat(title, "\n", sep = "")
  cat(paste0("  ", format(names(rows)), " : ", rows, "\n"), sep = "")
  invisible(NULL)
}


#' The predictive draws for one evaluation window
#'
#' A fold is held out by masking its response. The masked rows keep the
#' window's occasions on the refit's time grid. Their latent states
#' were sampled with the parameters. The draws come from the
#' `hindcast()` builders and are returned as the `mvgam_forecast` that
#' [score.mvgam_forecast()] takes. `forecast()` extends the grid past
#' its last occasion and does not apply here.
#'
#' @param fit The refit for the current window.
#' @param fc_data The window's rows of the evaluation frame, carrying
#'   the responses actually observed there.
#' @return An `mvgam_forecast` holding one draw matrix and one vector
#'   of held-out observations per series.
#' @noRd
window_predictive <- function(fit, fc_data) {
  checkmate::assert_class(fit, "mvgam")
  checkmate::assert_data_frame(fc_data, min.rows = 1L)
  series_levels <- fitted_series_levels(fit)
  window <- build_training_arms(fit, series_levels, data = fc_data)
  arms <- build_hindcast_arms(
    fit, window, type = "response",
    draw_idx = seq_len(posterior::ndraws(fit)),
    obs_uncertainty = TRUE
  )
  # The window's observations go where a forecast keeps its held-out
  # truths. `score()` scores the draws against them.
  new_mvgam_forecast(
    fit, "response", resp = NULL, reported = series_levels,
    training = NULL, hindcasts = NULL, forecasts = arms,
    fc_grid = list(
      observations = window$observations,
      times = column_times(fit, "response", NULL, window)
    )
  )
}


#' Refuse LFO results evaluated on different grids
#'
#' A paired difference across two grids would pair ELPDs from different
#' occasions.
#'
#' @param models List of `mvgam_lfo` objects
#' @return `TRUE`, invisibly, or an error
#' @noRd
assert_aligned_lfo <- function(models) {
  checkmate::assert_list(models, min.len = 1L)
  ref <- models[[1L]]
  for (i in seq_along(models)[-1L]) {
    same <- c(
      eval_timepoints = identical(models[[i]]$eval_timepoints,
                                  ref$eval_timepoints),
      fc_horizon = identical(models[[i]]$fc_horizon, ref$fc_horizon)
    )
    if (!all(same)) {
      stop(insight::format_error(c(
        "Compared LFO results must share one evaluation grid.",
        x = paste0("Model ", i, " differs from model 1 in ",
                   paste0("'", names(same)[!same], "'",
                          collapse = " and "), "."),
        i = paste0("Run 'lfo_cv()' on each model with the same ",
                   "'min_t', 'fc_horizon' and data.")
      )), call. = FALSE)
    }
  }
  invisible(TRUE)
}
