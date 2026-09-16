#' LOO information criteria for \pkg{mvgam} models
#'
#' Extract the LOOIC (leave-one-out information criterion) using [loo::loo()].
#'
#' @importFrom loo loo is.loo
#'
#' @param x Object of class `mvgam` or `jsdgam`
#'
#' @param compare,resp,pointwise,moment_match,reloo,k_threshold,save_psis,moment_match_args,reloo_args
#'   Accepted for [brms::loo.brmsfit] parity. Two are honoured: `resp`
#'   scopes a multivariate fit to one response, and `save_psis` keeps
#'   the smoothing object. The rest name machinery mvgam does not
#'   have. `pointwise` streams the density, `moment_match` and `reloo`
#'   refit the model and `k_threshold` with the `*_args` lists
#'   configure those refits, while `compare` and `model_names` rank
#'   several models, which [loo_compare()] does. Supplying any of them
#'   raises an error naming it.
#'
#' @param incl_autocor Logical, default `TRUE`. Passed to
#'   [log_lik.mvgam()] as its argument of the same name, so each
#'   observation is scored on the conditional surface, under the latent
#'   trend state the model inferred at that time. That is what makes an
#'   ELPD describe the series that was observed; `FALSE` scores the
#'   deterministic submodel alone.
#' @param incl_dynamics Superseded by `incl_autocor` and still
#'   accepted, so calls written against it keep working. `TRUE` maps
#'   to `incl_autocor = TRUE` and `FALSE` to `incl_autocor = FALSE`.
#'   When only `incl_dynamics` is given it decides; when both are
#'   given `incl_autocor` decides and `incl_dynamics` is ignored.
#'
#' @param by_series Logical, default `FALSE`. When `TRUE`, return a
#'   data frame with one row per series (`series` column) and per-series
#'   ELPD estimates instead of a single `psis_loo` object. Matches the
#'   `spOccupancy::waicOcc(by.sp = TRUE)` workflow for ranking species-
#'   level fit in joint-species distribution models. Requires a `series`
#'   column on the fit's data. For closure-unit families the log_lik
#'   columns are at the per-unit grain; each unit is mapped to a series
#'   via the first row of its visit block.
#'
#' @param ... Further arguments passed to [loo::loo()]
#'
#' @rdname loo.mvgam
#'
#' @return For `loo.mvgam`, an object of class `psis_loo` (see [loo::loo()]
#' for details). For `loo_compare.mvgam`, an object of class `compare.loo`
#' (see [loo::loo_compare()] for details).
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
#' A fit carrying a latent trend strains this approximation, and not
#' occasionally. Leaving an observation out does not remove its
#' influence on the state the model inferred at that time, so the
#' density being re-weighted still conditions on a state that saw it.
#' The ELPD then describes conditional in-sample fit rather than
#' out-of-sample accuracy, and the effective number of parameters
#' climbs towards the number of observations. Read the Pareto \eqn{k}
#' diagnostics before trusting a comparison, and prefer
#' [lfo_cv.mvgam()], which refits along the time axis and scores each
#' window against data the model has not seen. [forecast.mvgam()] with
#' [score.mvgam_forecast()] serves the same purpose on a held-out
#' horizon.
#'
#' @references
#' Vehtari, A., Gelman, A. and Gabry, J. (2017). Practical
#' Bayesian model evaluation using leave-one-out cross-validation
#' and WAIC. \emph{Statistics and Computing}, 27:1413-1432.
#' \doi{10.1007/s11222-016-9696-4}
#'
#' Bürkner, P.-C., Gabry, J. and Vehtari, A. (2020). Approximate
#' leave-future-out cross-validation for Bayesian time series
#' models. \emph{Journal of Statistical Computation and
#' Simulation}, 90(14):2499-2523.
#' \doi{10.1080/00949655.2020.1783262}
#'
#' @author Nicholas J Clark
#'
#' @seealso [waic.mvgam()], [log_lik.mvgam()],
#'   [lfo_cv.mvgam()] for time-block (rolling) CV,
#'   [kfold.mvgam()] for grouped k-fold CV with optional
#'   selective refit.
#'
#' @examples
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR())
#'
#' mod1 <- mvgam(y ~ s(x),
#'                trend_formula = ~ AR(p = 1),
#'                data    = simdat$data_train,
#'                family  = poisson(),
#'                chains  = 2, silent = 2)
#' mod2 <- mvgam(y ~ 1,
#'                trend_formula = ~ AR(p = 1),
#'                data    = simdat$data_train,
#'                family  = poisson(),
#'                chains  = 2, silent = 2)
#'
#' # `elpd_loo` is the expected log pointwise predictive density;
#' # `p_loo` is the effective number of parameters; `looic` is
#' # `-2 * elpd_loo`. The Pareto-k diagnostic table reports per-
#' # observation k values -- values above 0.7 (or above 1) mean
#' # the leave-one-out approximation may be unreliable for those
#' # cells and motivate either `moment_match = TRUE` or k-fold
#' # cross-validation.
#' loo(mod1)
#'
#' # Model comparison by ELPD difference. `loo_compare` orders
#' # models from best (top row, `elpd_diff = 0`) to worst.
#' # A negative `elpd_diff` against the best model that exceeds
#' # roughly 4 standard errors (|elpd_diff / se_diff| > 4) is a
#' # firm preference for the top model.
#' loo_compare(mod1, mod2)
#' }
#'
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
    legacy = incl_dynamics,
    autocor_supplied = !missing(incl_autocor)
  )
  # brms-parity arguments that need machinery we do not yet have. Fail
  # fast rather than silently ignoring; users picking these flags expect
  # them to do something.
  if (isTRUE(pointwise)) {
    stop(insight::format_error(c(
      cli::format_inline(paste0(
        "{.field pointwise = TRUE} streaming log-likelihood is not ",
        "supported on mvgam."
      )),
      i = cli::format_inline(
        "Compute LOO in-memory by leaving {.field pointwise = FALSE}."
      )
    )))
  }
  if (isTRUE(moment_match) || isTRUE(reloo)) {
    stop(insight::format_error(c(
      cli::format_inline(paste0(
        "{.field moment_match} and {.field reloo} are not ",
        "supported on mvgam."
      )),
      i = paste0(
        "These options require model refits. Inspect the Pareto k ",
        "diagnostics from 'loo()' and refit without problematic ",
        "observations if needed."
      )
    )))
  }

  # The remaining brms-parity arguments describe machinery this
  # method does not run: `k_threshold` and the `*_args` lists
  # configure the refits refused above, and `compare` / `model_names`
  # rank several models, which `loo_compare()` does. Each is refused
  # where supplied.
  unhonoured <- c(
    compare = !missing(compare),
    k_threshold = !missing(k_threshold),
    moment_match_args = length(moment_match_args) > 0L,
    reloo_args = length(reloo_args) > 0L,
    model_names = !is.null(model_names)
  )
  if (any(unhonoured)) {
    stop(insight::format_error(c(
      "Arguments this method cannot honour were supplied.",
      x = paste0(
        "Supplied: ",
        paste0("'", names(unhonoured)[unhonoured], "'", collapse = ", "),
        "."
      ),
      i = paste0(
        "Rank models with 'loo_compare()'; 'k_threshold' and the ",
        "'*_args' lists configure refits mvgam does not run."
      )
    )))
  }

  # The refusals above name the arguments this method declines by
  # design. What remains of `...` reaches `loo::loo()`, whose matrix
  # method drops a name it does not read, so a misspelling returned an
  # ELPD computed on the defaults with nothing said. The receiver's
  # own formals are the set that survives a change in its signature.
  refuse_unread_dots(
    list(...),
    names(formals(utils::getS3method(
      "loo", "matrix",
      envir = asNamespace("loo")
    ))),
    "loo"
  )

  # An ELPD is a statement about the observations in hand, so each
  # column is scored under the state the model inferred at that time
  # rather than under a fresh draw from the trend's marginal
  # dynamics. Marginalising instead leaves the weights describing a
  # series the model never saw, which is what drove an effective
  # parameter count above the number of observations.
  logliks <- log_lik(x, incl_autocor = incl_autocor, resp = resp)
  logliks <- clean_ll(x, logliks)

  # Compute relative effective sample size for PSIS.
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

#' @importFrom loo loo_compare
#'
#' @param x Object of class `mvgam`
#'
#' @param ... More \code{mvgam} objects
#'
#' @param model_names If `NULL` (the default) will use model names derived
#' from deparsing the call. Otherwise will use the passed values as model names
#'
#' @param criterion Information criterion used for comparison. One of
#'   `"loo"` (default) or `"waic"`.
#'
#' @param incl_autocor Logical, passed through to [loo.mvgam()] /
#'   [waic.mvgam()]. Default `TRUE` to match [loo.mvgam()].
#'
#' @param incl_dynamics Superseded by `incl_autocor` and still
#'   accepted; ignored when `incl_autocor` is also given.
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
    legacy = incl_dynamics,
    autocor_supplied = !missing(incl_autocor)
  )
  criterion <- match.arg(criterion)
  models <- split_mod_dots(x, ..., model_names = model_names)
  estimates <- named_list(names(models))
  for (i in seq_along(models)) {
    estimates[[i]] <- if (criterion == "loo") {
      loo(models[[i]], incl_autocor = incl_autocor)
    } else {
      waic(models[[i]], incl_autocor = incl_autocor)
    }
  }
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
  # `cmp` is a `compare.loo` matrix; preserve its class + row
  # names but widen into a data.frame so string diagnostic
  # columns can sit alongside the numeric ones.
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

#'@noRd
split_mod_dots = function(x, ..., model_names = NULL, other = TRUE) {
  dots <- list(x, ...)
  names <- substitute(list(x, ...), env = parent.frame())[-1]
  names <- ulapply(names, deparse)

  if (!is.null(model_names)) {
    names <- model_names
  }

  if (length(names)) {
    if (!length(names(dots))) {
      names(dots) <- names
    } else {
      has_no_name <- !nzchar(names(dots))
      names(dots)[has_no_name] <- names[has_no_name]
    }
  }
  is_mvgam <- unlist(lapply(dots, function(y) inherits(y, 'mvgam')))
  models <- dots[is_mvgam]
  out <- dots[!is_mvgam]

  if (length(out)) {
    stop(
      "Only model objects can be passed to '...' for this method.",
      call. = FALSE
    )
  }
  models
}

#'@noRd
named_list = function(names, values = NULL) {
  if (!is.null(values)) {
    if (length(values) <= 1L) {
      values <- replicate(length(names), values)
    }
    values <- as.list(values)
    stopifnot(length(values) == length(names))
  } else {
    values <- vector("list", length(names))
  }
  setNames(values, names)
}

# Compute per-series information criterion estimates from a
# pointwise log-likelihood matrix. Used by both loo.mvgam(by_series
# = TRUE) and waic.mvgam(by_series = TRUE) so the column-to-series
# mapping logic lives in one place. Returns a data frame with one
# row per series: (series, elpd, se_elpd, p, n_obs), where
# `elpd` / `p` come from the selected criterion. Closure-unit
# log_lik matrices are at the per-unit grain; each unit is mapped
# to a series via the first row of its visit block (closure units
# are defined within a series, so every row in a unit shares the
# same series label).
#'@noRd
per_series_ic <- function(x, logliks,
                            criterion = c("loo", "waic")) {
  criterion <- match.arg(criterion)
  col_series <- per_obs_series_labels(x)
  # `clean_ll()` drops the columns with no density and records
  # which survived. Narrow the labels the same way rather than
  # pairing them with the frame's full width: an observation with
  # no response contributes nothing to score and is not an
  # arithmetic mismatch to report to the user.
  col_series <- narrow_to_scored(col_series, logliks)
  if (length(col_series) != ncol(logliks)) {
    stop(insight::format_error(c(
      "Cannot split log_lik by series.",
      x = paste0(
        "log_lik has ", ncol(logliks), " scored columns but the ",
        "data implies ", length(col_series), " observations."
      ),
      i = paste0(
        "The fit's stored data may not match the layout the model ",
        "was fitted with."
      )
    )))
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

# Map each log_lik column to a series label, matching the grain
# log_lik returns. For detection-family closure-unit fits (occ /
# nmix variants) log_lik is per-unit; every visit row in a unit
# carries the same species, and the first visit row of each unit
# names it. Which series a row belongs to is a property of the
# model, and the axis record states it: a hierarchical fit names
# its series from grouping columns, with no series column present.
# Multi-response custom families (mvn / mvt / diri / multi /
# categ) place species on the K-vector axis WITHIN a unit. No
# species there has rows of its own, and by_series is rejected up
# front.

#'@noRd
per_obs_series_labels <- function(x) {
  if (is_multi_response_family(x$family)) {
    stop(insight::format_error(c(
      "by_series = TRUE is not meaningful for multi-response families.",
      x = paste0(
        "Family '", resolve_family_name(x$family) %||% "?",
        "' places species on the K-vector axis within each closure ",
        "unit. A species has no rows of its own."
      ),
      i = "Use loo()/waic() without by_series to score per closure unit."
    )))
  }
  data <- mvgam_training_data(x) %||% data.frame()
  series <- axis_row_series(x, data, required = TRUE)
  if (is.null(series)) {
    stop(insight::format_error(c(
      "by_series = TRUE is not meaningful for a response-keyed fit.",
      x = paste0(
        "The series of this model are its responses, and every row ",
        "carries all of them at once."
      ),
      i = paste0(
        "Score the whole fit by calling loo()/waic() with ",
        "by_series = FALSE."
      )
    )))
  }
  # `loglik_col_values()` settles the closure-unit grain for every
  # caller, and `kfold()` puts its fold keys through the same call.
  loglik_col_values(x, data, as.character(series))
}

#' Drop unscorable columns from a log-likelihood matrix
#'
#' A row whose response is missing contributes no density, so its
#' column is all `NA` and cannot be scored. Which columns survived is
#' recorded on the result: anything paired with the scored matrix
#' afterwards, such as a prediction weighted by the PSIS object built
#' from it, has to be narrowed to the same columns or the two describe
#' different observations.
#'
#'@noRd
clean_ll = function(x, logliks) {
  # First remove any columns that are all NA (these had missing observations)
  scored <- which(!apply(logliks, 2, function(x) all(!is.finite(x))))
  logliks <- logliks[, scored, drop = FALSE]

  n_replaced <- sum(!is.finite(logliks))
  if (n_replaced == 0L) {
    attr(logliks, "scored_columns") <- scored
    return(logliks)
  }

  # A column that is finite for most draws and not for a few says the
  # model gave those draws no support for that observation. Dropping
  # the column would discard an observation the rest of the posterior
  # scores perfectly well, so the draws are refilled from the finite
  # ones, which keeps the column's own spread.
  #
  # Under a fixed seed, and restoring the caller's stream. Drawing
  # from whatever state the session happened to be in made `loo()`
  # answer differently on each call for the same fit, so a
  # `loo_compare()` could not be reproduced, and re-running it could
  # reorder the models.
  samp_noinf = function(x) {
    x_finite <- x[is.finite(x)]
    bad <- !is.finite(x)
    # Index rather than `sample(x_finite, ...)`: given a single
    # finite value of 3.7, `sample()` reads it as `sample.int(3)`
    # and returns integers from 1 to 3, which then enter `loo()`
    # as log-densities. Only positive values take that branch, so
    # it hides until a continuous family puts a density above e.
    x[bad] <- x_finite[
      sample.int(length(x_finite), sum(bad), replace = TRUE)
    ]
    x
  }
  local_seed(1L)
  logliks <- apply(logliks, 2, samp_noinf)

  # The refill is a repair, and the elpd it feeds is optimistic by
  # however much of the likelihood was unusable, so say so rather
  # than let a number stand unqualified.
  if (!identical(Sys.getenv("TESTTHAT"), "true")) {
    rlang::warn(
      insight::format_message(c(
        paste0(
          n_replaced,
          " log-likelihood values were not finite. They were refilled ",
          "from the finite draws of their own observation."
        ),
        i = paste0(
          "The reported ELPD is optimistic by whatever those draws ",
          "would have contributed. It usually means the family gave ",
          "an observation no support under part of the posterior."
        )
      )),
      .frequency = "once",
      .frequency_id = "mvgam_clean_ll_refill"
    )
  }

  # return
  attr(logliks, "scored_columns") <- scored
  logliks
}
