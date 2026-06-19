#' LOO information criteria for \pkg{mvgam} models
#'
#' Extract the LOOIC (leave-one-out information criterion) using [loo::loo()].
#'
#' @importFrom loo loo is.loo
#'
#' @param x Object of class `mvgam` or `jsdgam`
#'
#' @param compare,resp,pointwise,moment_match,reloo,k_threshold,save_psis,moment_match_args,reloo_args,model_names
#'   Accepted for [brms::loo.brmsfit] parity. `resp` is passed through to
#'   [log_lik.mvgam()] for multivariate response selection; `pointwise`,
#'   `moment_match`, `reloo` and their `*_args` companions require
#'   refit / streaming machinery that mvgam does not yet support — v1
#'   raises a clear error when any of them is requested. `compare`,
#'   `k_threshold`, `save_psis` and `model_names` pass through to
#'   [loo::loo()] or are no-ops for single-model evaluation.
#'
#' @param incl_dynamics Logical, default `FALSE`. Maps to the
#'   `process_error` argument on [log_lik.mvgam()]. When `FALSE` (default)
#'   the trend is fixed at its posterior mean, giving PSIS weights that
#'   reflect parameter uncertainty alone. Set `TRUE` to fold sampled
#'   trend realisations into the per-observation log-likelihood, useful
#'   for continuous-family fits where the observation noise is small
#'   relative to the trend.
#'
#' @param by_species Logical, default `FALSE`. When `TRUE`, return a
#'   data frame with one row per series (`species` column) and per-series
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
#' Note: In-sample predictive metrics such as PSIS-LOO can sometimes be overly
#' optimistic for models that include process error components (e.g. those with
#' `trend_model`, `trend_formula`, or `factor_formula`). Consider using
#' out-of-sample evaluations for further scrutiny (see
#' \code{\link{forecast.mvgam}}, \code{\link{score.mvgam_forecast}},
#' \code{\link{lfo_cv}}).
#'
#' @references
#' Vehtari, A., Gelman, A. and Gabry, J. (2017). Practical
#' Bayesian model evaluation using leave-one-out cross-validation
#' and WAIC. \emph{Statistics and Computing}, 27:1413-1432.
#' \doi{10.1007/s11222-016-9696-4}
#'
#' @author Nicholas J Clark
#'
#' @examples
#' \donttest{
#' #--------------------------------------------------
#' # Simulate 4 time series with hierarchical seasonality
#' # and independent AR1 dynamic processes
#' #--------------------------------------------------
#' set.seed(111)
#'
#' simdat <- sim_mvgam(
#'   seasonality = 'hierarchical',
#'   trend_model = AR(),
#'   family = gaussian()
#' )
#'
#' # Fit a model with shared seasonality
#' mod1 <- mvgam(
#'   y ~ s(season, bs = 'cc', k = 6),
#'   data = rbind(simdat$data_train, simdat$data_test),
#'   family = gaussian(),
#'   chains = 2,
#'   silent = 2
#' )
#'
#' conditional_effects(mod1)
#'
#' mc.cores.def <- getOption('mc.cores')
#' options(mc.cores = 1)
#' loo(mod1)
#'
#' # Fit a model with hierarchical seasonality
#' mod2 <- update(
#'   mod1,
#'   formula = y ~ s(season, bs = 'cc', k = 6) +
#'     s(season, series, bs = 'fs', xt = list(bs = 'cc'), k = 4),
#'   chains = 2,
#'   silent = 2
#' )
#'
#' conditional_effects(mod2)
#' loo(mod2)
#'
#' # Add AR1 dynamic errors to mod2
#' mod3 <- update(
#'   mod2,
#'   trend_model = AR(),
#'   chains = 2,
#'   silent = 2
#' )
#'
#' conditional_effects(mod3)
#' plot(mod3, type = 'trend')
#' loo(mod3)
#'
#' #--------------------------------------------------
#' # Compare models using LOO
#' #--------------------------------------------------
#' loo_compare(mod1, mod2, mod3)
#' options(mc.cores = mc.cores.def)
#'
#' #--------------------------------------------------
#' # Compare forecast abilities using LFO-CV
#' #--------------------------------------------------
#'
#' lfo_mod2 <- lfo_cv(mod2, min_t = 92)
#' lfo_mod3 <- lfo_cv(mod3, min_t = 92)
#'
#' # Plot forecast ELPD differences
#' plot(
#'   y = lfo_mod2$elpds - lfo_mod3$elpds,
#'   x = lfo_mod2$eval_timepoints,
#'   pch = 16,
#'   ylab = 'ELPD_mod2 - ELPD_mod3',
#'   xlab = 'Evaluation timepoint'
#' )
#'
#' abline(h = 0, lty = 'dashed')
#' }
#'
#' @seealso [waic.mvgam()], [log_lik.mvgam()],
#'   [lfo_cv.mvgam()] for time-block (rolling) CV,
#'   [kfold.mvgam()] for grouped k-fold CV with optional
#'   selective refit.
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
                      incl_dynamics = FALSE,
                      by_species = FALSE) {
  # brms-parity arguments that need machinery we do not yet have. Fail
  # fast rather than silently ignoring; users picking these flags expect
  # them to do something.
  if (isTRUE(pointwise)) {
    stop(insight::format_error(c(
      "{.field pointwise = TRUE} streaming log-likelihood is not yet supported on mvgam.",
      i = "Compute LOO in-memory by leaving {.field pointwise = FALSE}."
    )))
  }
  if (isTRUE(moment_match) || isTRUE(reloo)) {
    stop(insight::format_error(c(
      "{.field moment_match} and {.field reloo} are not yet supported on mvgam.",
      i = "These require model refits; revisit once the C++ trend extrapolator lands."
    )))
  }

  # Brms-parity log-likelihood path. `incl_dynamics` maps to the
  # `process_error` argument on log_lik.mvgam: FALSE (default) fixes the
  # trend at its posterior mean so PSIS weights are not dominated by
  # latent-state variance; TRUE folds sampled trend realisations into
  # the per-observation log-density.
  logliks <- log_lik(x, process_error = incl_dynamics, resp = resp)
  logliks <- clean_ll(x, logliks)

  # Compute relative effective sample size for PSIS.
  chains <- posterior::nchains(posterior::as_draws_array(x$fit))
  n_per_chain <- NROW(logliks) / chains
  releffs <- loo::relative_eff(
    exp(logliks),
    chain_id = sort(rep(seq_len(chains), n_per_chain))
  )
  if (isTRUE(by_species)) {
    return(per_species_ic(x, logliks, criterion = "loo"))
  }
  loo::loo(logliks, r_eff = releffs, save_psis = save_psis, ...)
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
#' @param incl_dynamics Logical, passed through to [loo.mvgam()] /
#'   [waic.mvgam()]. Default `FALSE` to match [loo.mvgam()].
#'
#' @rdname loo.mvgam
#'
#' @export
loo_compare.mvgam <- function(
  x,
  ...,
  criterion = c("loo", "waic"),
  model_names = NULL,
  incl_dynamics = FALSE
) {
  criterion <- match.arg(criterion)
  models <- split_mod_dots(x, ..., model_names = model_names)
  estimates <- named_list(names(models))
  for (i in seq_along(models)) {
    estimates[[i]] <- if (criterion == "loo") {
      loo(models[[i]], incl_dynamics = incl_dynamics)
    } else {
      waic(models[[i]], incl_dynamics = incl_dynamics)
    }
  }
  loo_compare(estimates)
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

# Compute per-species information criterion estimates from a
# pointwise log-likelihood matrix. Used by both loo.mvgam(by_species
# = TRUE) and waic.mvgam(by_species = TRUE) so the column-to-species
# mapping logic lives in one place. Returns a data frame with one
# row per series: (species, elpd, se_elpd, p, n_obs), where
# `elpd` / `p` come from the selected criterion. Closure-unit
# log_lik matrices are at the per-unit grain; each unit is mapped
# to a series via the first row of its visit block (closure units
# are defined within a series, so every row in a unit shares the
# same species label).
#'@noRd
per_species_ic <- function(x, logliks,
                            criterion = c("loo", "waic")) {
  criterion <- match.arg(criterion)
  col_species <- per_obs_species_labels(x, ncol(logliks))
  if (length(col_species) != ncol(logliks)) {
    stop(insight::format_error(c(
      "Cannot split log_lik by species: column count mismatch.",
      x = paste0(
        "log_lik has ", ncol(logliks), " columns but the data ",
        "implies ", length(col_species), " observations."
      ),
      i = "by_species = TRUE assumes clean_ll() did not drop any columns."
    )))
  }
  chains <- posterior::nchains(posterior::as_draws_array(x$fit))
  n_per_chain <- NROW(logliks) / chains
  chain_id <- sort(rep(seq_len(chains), n_per_chain))
  by_idx <- split(seq_along(col_species),
                  factor(col_species, levels = unique(col_species)))
  est_name <- if (identical(criterion, "loo")) "elpd_loo" else "elpd_waic"
  p_name   <- if (identical(criterion, "loo")) "p_loo"    else "p_waic"
  rows <- lapply(names(by_idx), function(sp) {
    idx <- by_idx[[sp]]
    ll  <- logliks[, idx, drop = FALSE]
    ic  <- if (identical(criterion, "loo")) {
      r_eff <- loo::relative_eff(exp(ll), chain_id = chain_id)
      loo::loo(ll, r_eff = r_eff)
    } else {
      loo::waic(ll)
    }
    data.frame(
      species = sp,
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
# shares the same species, so reading the first visit row of each
# unit is sufficient. For row-grain fits the data's `series` column
# maps directly. Multi-response custom families (mvn / mvt / diri
# / multi / categ) put species on the K-vector axis WITHIN a unit
# rather than across units; by_species is not meaningful there
# and is rejected up front.
#'@noRd
per_obs_species_labels <- function(x, n_cols) {
  if (is_multi_response_family(x$family)) {
    stop(insight::format_error(c(
      "by_species = TRUE is not meaningful for multi-response families.",
      x = paste0(
        "Family '", resolve_family_name(x$family) %||% "?",
        "' puts species on the per-unit K-vector axis, not as ",
        "separate rows."
      ),
      i = "Use loo()/waic() without by_species to score per closure unit."
    )))
  }
  data <- x$data %||% data.frame()
  if (!"series" %in% names(data)) {
    stop(insight::format_error(c(
      "by_species = TRUE requires a 'series' column on the fit's data.",
      i = "Add a series factor before fitting if you want per-series IC."
    )))
  }
  series_col <- as.character(data$series)
  if (is_closure_unit_family(x$family)) {
    arrs <- build_closure_unit_arrays(
      data,
      response_var = closure_unit_response_var(x$formula),
      default_cap = closure_unit_default_cap(x$family),
      unit_grouping_vars = closure_unit_grouping(x$family)
    )
    series_col[arrs$visit_idx[, 1L]]
  } else {
    series_col
  }
}

#'@noRd
clean_ll = function(x, logliks) {
  # First remove any columns that are all NA (these had missing observations)
  logliks <- logliks[, !apply(logliks, 2, function(x) all(!is.finite(x)))]

  # Next resample any remaining non-finite values (occasionally happens with
  # some observation families)
  samp_noinf = function(x) {
    x_finite <- x[is.finite(x)]
    x[!is.finite(x)] <- sample(
      x_finite,
      length(x[!is.finite(x)]),
      replace = TRUE
    )
    x
  }
  logliks <- apply(logliks, 2, samp_noinf)

  # return
  logliks
}
