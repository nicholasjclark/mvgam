#' LOO information criteria for \pkg{mvgam} models
#'
#' Extract the LOOIC (leave-one-out information criterion) using [loo::loo()].
#'
#' @importFrom loo loo is.loo
#'
#' @param x A fitted `mvgam` or `jsdgam` object.
#' @param compare,resp,pointwise,moment_match,reloo,k_threshold,save_psis,moment_match_args,reloo_args
#'   The arguments of [brms::loo.brmsfit]. mvgam uses two of them.
#'   `resp` scopes a multivariate fit to one response. `save_psis`
#'   keeps the smoothing object. `pointwise` would stream the density.
#'   `moment_match` and `reloo` would refit the model, configured by
#'   `k_threshold` and the `*_args` lists. `compare` and `model_names`
#'   would rank several models, which [loo_compare.mvgam()] does.
#'   mvgam runs none of these. Supplying one raises an error naming
#'   it.
#'
#' @param incl_autocor Logical, default `TRUE`. Passed to
#'   [log_lik.mvgam()]. `TRUE` scores each observation under the latent
#'   trend state the model inferred at that time. The ELPD then
#'   describes the series that was observed. `FALSE` scores the
#'   deterministic submodel alone.
#' @param incl_dynamics Deprecated alias for `incl_autocor`.
#'
#' @param by_series Logical, default `FALSE`. `TRUE` returns a data
#'   frame with one row per series and columns `series`, `elpd`,
#'   `se_elpd`, `p` and `n_obs`, as `spOccupancy::waicOcc(by.sp = TRUE)`
#'   ranks species. An occupancy or N-mixture fit is scored per site,
#'   each site counted in its species.
#'
#' @param ... For `loo()`, further arguments to [loo::loo()]. For
#'   `loo_compare()`, further fitted models.
#' @rdname loo.mvgam
#'
#' @return For `loo.mvgam`, an object of class `psis_loo` (see
#'   [loo::loo()]). For `loo_compare.mvgam`, a `compare.loo` table with
#'   one row per model, best first (see [loo::loo_compare()]). Three
#'   columns follow Sivula et al. (2025). `p_worse` is the normal
#'   probability that the model predicts worse than the best one.
#'   `diag_diff` holds `"|elpd_diff| < 4"` for a difference too small
#'   for its SE to be trusted. `diag_elpd` holds `"N < 100"` for too
#'   few observations and `"k_psis > 0.7"` for an unreliable PSIS
#'   approximation.
#'
#' @details
#' When comparing two (or more) fitted `mvgam` models, we can estimate the
#' difference in their in-sample predictive accuracies using the Expected Log
#' Predictive Density (ELPD). This metric can be approximated using Pareto
#' Smoothed Importance Sampling (PSIS), which re-weights posterior draws to
#' approximate predictions for a datapoint had it not been included in the
#' original model fit (i.e. leave-one-out cross-validation).
#'
#' See [loo::loo()] and [loo::loo_compare()] for further details on how this
#' importance sampling works.
#'
#' A fit with a latent trend routinely strains this approximation.
#' Leaving an observation out keeps its influence on the state the
#' model inferred at that time. The re-weighted density still
#' conditions on a state estimated from that observation. The ELPD
#' then measures conditional in-sample fit. The effective number of
#' parameters climbs towards the number of observations. Check the
#' Pareto \eqn{k} diagnostics before trusting a comparison.
#' [lfo_cv.mvgam()] refits along the time axis and scores each window
#' against data the model has not seen. [forecast.mvgam()] with
#' [score.mvgam_forecast()] scores a held-out horizon.
#'
#' @references
#' Vehtari, A., Gelman, A. and Gabry, J. (2017). Practical
#' Bayesian model evaluation using leave-one-out cross-validation
#' and WAIC. \emph{Statistics and Computing}, 27:1413-1432.
#' \doi{10.1007/s11222-016-9696-4}
#'
#' Burkner, P.-C., Gabry, J. and Vehtari, A. (2020). Approximate
#' leave-future-out cross-validation for Bayesian time series
#' models. \emph{Journal of Statistical Computation and
#' Simulation}, 90(14):2499-2523.
#' \doi{10.1080/00949655.2020.1783262}
#'
#' Sivula, T., Magnusson, M., Matamoros, A. A. and Vehtari, A. (2025).
#' Uncertainty in Bayesian leave-one-out cross-validation based model
#' comparison. \emph{Bayesian Analysis}. \doi{10.1214/25-BA1569}
#'
#' @author Nicholas J Clark
#'
#' @seealso [lfo_cv.mvgam()] scores forecasts by refitting along the
#'   time axis. [kfold.mvgam()] runs grouped k-fold cross-validation.
#'   [loo_model_weights.mvgam()] weights models by their predictions.
#'   The
#'   `r mvgam_article("forecast_evaluation", "forecast evaluation article")`
#'   compares models by approximate and exact cross-validation.
#'
#' @template ex-loo-models
#' @examples
#' \dontrun{
#' # The same smooth of x replaced by a linear effect
#' mod_lin <- update(mod, formula. = y ~ x)
#'
#' # The ELPD estimate, with Pareto k diagnostics for each observation
#' loo(mod)
#'
#' # The better model comes first, with elpd_diff = 0. The diagnostic
#' # columns say how far the difference can be trusted
#' loo_compare(mod, mod_lin)
#'
#' # Next: lfo_cv() scores each model's forecasts of data it has not
#' # seen
#' }
#'
#' @family model comparison
#' @export
loo.mvgam <- function(x, ...,
                      compare = TRUE,
                      resp = NULL,
                      pointwise = FALSE,
                      moment_match = FALSE,
                      reloo = FALSE,
                      k_threshold = 0.7,
                      save_psis = FALSE,
                      moment_match_args = list(),
                      reloo_args = list(),
                      model_names = NULL,
                      incl_autocor = TRUE,
                      incl_dynamics = NULL,
                      by_series = FALSE) {
  require_fitted_model(x, "loo")
  incl_autocor <- resolve_incl_autocor(
    incl_autocor = incl_autocor,
    alias = incl_dynamics,
    autocor_supplied = !missing(incl_autocor),
    alias_name = "incl_dynamics"
  )
  # `moment_match` and `reloo` refit the model at each high-k
  # observation. The other brms arguments configure those refits,
  # stream the density or rank several models.
  if (isTRUE(moment_match) || isTRUE(reloo)) {
    stop(insight::format_error(c(
      "'moment_match' and 'reloo' are not supported.",
      x = "Each option refits the model once per high-k observation.",
      i = paste0("Inspect the Pareto k diagnostics from 'loo()' and ",
                 "refit on the remaining observations if needed.")
    )), call. = FALSE)
  }
  refuse_unsupported_ic_args(c(
    pointwise = isTRUE(pointwise),
    compare = !missing(compare),
    k_threshold = !missing(k_threshold),
    moment_match_args = length(moment_match_args) > 0L,
    reloo_args = length(reloo_args) > 0L,
    model_names = !is.null(model_names)
  ), "loo")

  # The rest of `...` reaches `loo::loo()`. Its matrix
  # method ignores a name it does not take. A misspelt argument would
  # give an ELPD on the defaults with no message. Taking the accepted
  # names from its formals follows any change in its signature.
  refuse_unread_dots(
    list(...),
    names(formals(utils::getS3method(
      "loo", "matrix",
      envir = asNamespace("loo")
    ))),
    "loo"
  )

  # An ELPD describes the observations in hand. Each column is scored
  # under the state the model inferred at that time. Scoring under a
  # fresh draw from the trend's marginal dynamics would describe a
  # series the model never saw. The effective parameter count then
  # exceeds the number of observations.
  logliks <- log_lik(x, incl_autocor = incl_autocor, resp = resp)
  logliks <- clean_ll(logliks)

  releffs <- mvgam_r_eff_log_lik(x, logliks)
  if (isTRUE(by_series)) {
    return(per_series_ic(x, logliks, criterion = "loo"))
  }
  out <- loo::loo(logliks, r_eff = releffs, save_psis = save_psis, ...)
  # Carry the scored columns onto the result so a caller weighting
  # predictions by these importance weights can narrow them to the
  # same observations.
  attr(out, "scored_columns") <- attr(logliks, "scored_columns")
  if (!is.null(out$psis_object)) {
    attr(out$psis_object, "scored_columns") <-
      attr(logliks, "scored_columns")
  }
  out
}

#' @param model_names Character vector of row names for the comparison,
#'   one per model. `NULL` takes them from the call.
#' @param criterion `"loo"` (the default) or `"waic"`.
#'
#' @rdname loo.mvgam
#'
#' @export
loo_compare.mvgam <- function(
  x,
  ...,
  criterion = c("loo", "waic"),
  model_names = NULL,
  incl_autocor = TRUE,
  incl_dynamics = NULL
) {
  incl_autocor <- resolve_incl_autocor(
    incl_autocor = incl_autocor,
    alias = incl_dynamics,
    autocor_supplied = !missing(incl_autocor),
    alias_name = "incl_dynamics"
  )
  criterion <- match.arg(criterion)
  models <- list(x, ...)
  assert_compared_objects(models, "mvgam", "loo_compare")
  score <- if (criterion == "loo") loo else waic
  estimates <- stats::setNames(
    lapply(models, score, incl_autocor = incl_autocor),
    compared_object_names(
      models, as.list(substitute(list(x, ...)))[-1L], model_names
    )
  )
  cmp <- loo_compare(estimates)
  # Row order in `cmp` differs from `estimates`. Pareto-k
  # diagnostics only apply on the loo path (WAIC has no PSIS).
  est_ord <- match(rownames(cmp), names(estimates))
  n_pw <- vapply(estimates[est_ord], function(e) {
    if (!is.null(e$pointwise)) nrow(e$pointwise) else NA_integer_
  }, integer(1L))
  pareto_k_list <- if (criterion == "loo") {
    lapply(estimates[est_ord], function(e) e$diagnostics$pareto_k)
  } else {
    NULL
  }
  diag_cols <- mvgam_loo_compare_diagnostics(
    elpd_diff = as.numeric(cmp[, "elpd_diff"]),
    se_diff = as.numeric(cmp[, "se_diff"]),
    n_pointwise = n_pw,
    pareto_k_list = pareto_k_list
  )
  # `cmp` is a `compare.loo` matrix. A data frame keeps its class and
  # row names and also holds the character diagnostic columns.
  out <- data.frame(
    cmp[, , drop = FALSE],
    p_worse = diag_cols$p_worse,
    diag_diff = diag_cols$diag_diff,
    diag_elpd = diag_cols$diag_elpd,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  rownames(out) <- rownames(cmp)
  class(out) <- c("compare.loo", "matrix", "data.frame")
  out
}

#' @export
#' @importFrom loo loo
loo::loo

#' @export
#' @importFrom loo loo_compare
loo::loo_compare

#' Refuse the brms information-criterion arguments mvgam does not run
#'
#' @param supplied Named logical vector, `TRUE` for each argument the
#'   caller supplied
#' @param fn Name of the calling generic
#' @return `TRUE`, invisibly, or an error
#' @noRd
refuse_unsupported_ic_args <- function(supplied, fn) {
  checkmate::assert_logical(supplied, any.missing = FALSE,
                            names = "unique")
  checkmate::assert_string(fn)
  if (!any(supplied)) {
    return(invisible(TRUE))
  }
  used <- names(supplied)[supplied]
  stop(insight::format_error(c(
    paste0(
      paste0("'", used, "'", collapse = ", "),
      if (length(used) > 1L) " are" else " is",
      " not supported by '", fn, "()'."
    ),
    if (any(c("compare", "model_names") %in% used)) {
      c(i = "Rank models with 'loo_compare()'.")
    },
    i = paste0("'?", fn, ".mvgam' lists the arguments mvgam uses.")
  )), call. = FALSE)
}

#' Information criteria for each series
#'
#' Used by `loo(by_series = TRUE)` and `waic(by_series = TRUE)`. A
#' closure-unit log-likelihood matrix holds one column per unit. Each
#' unit belongs to one series.
#'
#' @param x Fitted `mvgam` object
#' @param logliks Log-likelihood matrix returned by `clean_ll()`
#' @param criterion `"loo"` or `"waic"`
#' @return Data frame with columns `series`, `elpd`, `se_elpd`, `p` and
#'   `n_obs`, one row per series
#' @noRd
per_series_ic <- function(x, logliks,
                            criterion = c("loo", "waic")) {
  checkmate::assert_class(x, "mvgam")
  checkmate::assert_matrix(logliks)
  criterion <- match.arg(criterion)
  col_series <- per_obs_series_labels(x)
  # `clean_ll()` drops the columns with no density and records which
  # remain. The labels are narrowed the same way. An observation with
  # no response has nothing to score.
  col_series <- narrow_to_scored(col_series, logliks)
  if (length(col_series) != ncol(logliks)) {
    stop(insight::format_error(c(
      "The log likelihood could not be split by series.",
      x = paste0(
        "The log likelihood has ", ncol(logliks), " scored columns. ",
        "The data hold ",
        length(col_series), " scored observations."
      ),
      i = "The fit's stored data may differ from the data it was fitted to."
    )), call. = FALSE)
  }
  by_idx <- split(seq_along(col_series),
                  factor(col_series, levels = unique(col_series)))
  est_name <- if (identical(criterion, "loo")) "elpd_loo" else "elpd_waic"
  p_name   <- if (identical(criterion, "loo")) "p_loo"    else "p_waic"
  rows <- lapply(names(by_idx), function(sp) {
    idx <- by_idx[[sp]]
    ll  <- logliks[, idx, drop = FALSE]
    ic  <- if (identical(criterion, "loo")) {
      loo::loo(ll, r_eff = mvgam_r_eff_log_lik(x, ll))
    } else {
      loo::waic(ll)
    }
    data.frame(
      series  = sp,
      elpd    = ic$estimates[est_name, "Estimate"],
      se_elpd = ic$estimates[est_name, "SE"],
      p       = ic$estimates[p_name, "Estimate"],
      n_obs   = length(idx),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' The series of each log-likelihood column
#'
#' The axis record assigns rows to series. A hierarchical fit derives
#' them from its grouping columns. A closure-unit fit has one column
#' per unit, labelled by the unit's first visit. Multi-response
#' families hold every species in each row and are refused.
#'
#' @param x Fitted `mvgam` object
#' @return Character vector of series labels, one per column of
#'   `log_lik(x)`
#' @noRd
per_obs_series_labels <- function(x) {
  checkmate::assert_class(x, "mvgam")
  if (is_multi_response_family(x$family)) {
    stop(insight::format_error(c(
      "'by_series = TRUE' is not supported for multi-response families.",
      x = paste0("Family '", resolve_family_name(x$family) %||% "?",
                 "' holds every species in each row."),
      i = "Use 'by_series = FALSE'."
    )), call. = FALSE)
  }
  data <- x$data %||% data.frame()
  series <- axis_row_series(x, data, required = TRUE)
  if (is.null(series)) {
    stop(insight::format_error(c(
      "'by_series = TRUE' is not supported for a response-keyed fit.",
      x = "The series of this model are its responses.",
      x = "Each row contains every response.",
      i = "Pass 'resp' to score one response."
    )), call. = FALSE)
  }
  # `loglik_col_values()` sets the closure-unit grain for every
  # caller. `kfold()` passes its fold keys through the same call.
  loglik_col_values(x, data, as.character(series))
}

#' Drop unscorable columns from a log-likelihood matrix
#'
#' A row whose response is missing contributes no density. Its column
#' has no finite value and is dropped. The `scored_columns` attribute
#' records the columns kept. A prediction weighted by the PSIS object
#' is narrowed to the same columns with `narrow_to_scored()`.
#'
#' @param logliks Numeric log-likelihood matrix, one row per draw
#' @return `logliks` without its unscorable columns, its other
#'   non-finite values refilled
#' @noRd
clean_ll <- function(logliks) {
  checkmate::assert_matrix(logliks, mode = "numeric")
  scored <- which(!apply(logliks, 2, function(x) all(!is.finite(x))))
  logliks <- logliks[, scored, drop = FALSE]

  n_replaced <- sum(!is.finite(logliks))
  if (n_replaced == 0L) {
    attr(logliks, "scored_columns") <- scored
    return(logliks)
  }

  # A column that is finite for most draws and not for a few means
  # those draws give the observation no support. The rest of the
  # posterior scores it. Dropping the column would discard the
  # observation. The bad draws are refilled from the finite ones,
  # keeping the column's own spread.
  #
  # The refill runs under a fixed seed and restores the caller's
  # stream. `loo()` then returns the same value on every call for one
  # fit. A `loo_compare()` is reproducible.
  samp_noinf <- function(x) {
    x_finite <- x[is.finite(x)]
    bad <- !is.finite(x)
    # Indexing avoids `sample(x_finite, ...)`. Given a single finite
    # value of 3.7, `sample()` treats it as `sample.int(3)`. It returns
    # integers from 1 to 3, which would enter `loo()` as
    # log-densities.
    x[bad] <- x_finite[
      sample.int(length(x_finite), sum(bad), replace = TRUE)
    ]
    x
  }
  local_seed(1L)
  logliks <- apply(logliks, 2, samp_noinf)

  # An ELPD computed from refilled draws is optimistic by however much
  # of the likelihood was unusable. The warning reports it.
  warn_once(
    c(
      paste0(
        n_replaced,
        " log-likelihood values were NaN or infinite. They were ",
        "refilled from the finite draws of their own observation."
      ),
      i = paste0(
        "The reported ELPD is optimistic by whatever those draws ",
        "would have contributed. It usually means the family gave ",
        "an observation zero density under part of the posterior."
      )
    ),
    "mvgam_clean_ll_refill"
  )

  attr(logliks, "scored_columns") <- scored
  logliks
}
