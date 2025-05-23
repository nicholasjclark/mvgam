#' LOO information criteria for \pkg{mvgam} models
#'
#' Extract the LOOIC (leave-one-out information criterion) using
#' [loo::loo()]
#' @importFrom loo loo is.loo
#' @param x Object of class `mvgam` or `jsdgam`
#' @param incl_dynamics Deprecated and currently ignored
#' @param ... Additional arguments for [loo::loo()]
#' @rdname loo.mvgam
#' @return  for `loo.mvgam`, an object of class `psis_loo` (see [loo::loo()]
#' for details). For `loo_compare.mvgam`, an object of class `compare.loo` (
#' [loo::loo_compare()] for details)
#' @details When comparing two (or more) fitted `mvgam` models, we can estimate the
#' difference in their in-sample predictive accuracies using the Expcted Log Predictive
#' Density (ELPD). This metric can be approximated using Pareto Smoothed Importance Sampling
#' (PSIS), which is a method to re-weight posterior draws to approximate what predictions the
#' models might have made for a given datapoint had that datapoint not been included in the
#' original model fit (i.e. if we were to run a leave-one-out cross-validation and then made
#' a prediction for the held-out datapoint). See details from [loo::loo()] and [loo::loo_compare()]
#' for further information on how this importance sampling works.
#'
#' Note that in-sample predictive metrics such as PSIS-LOO can sometimes be overly optimistic for
#' models that included process error components (such as those with `trend_model`,
#' `trend_formula` or `factor_formula` included). It is therefore recommended to perhaps use
#' out-of-sample evaluations for further scrutiny of these models (see for example
#' \code{\link{forecast.mvgam}}, \code{\link{score.mvgam_forecast}} and \code{\link{lfo_cv}})
#'@examples
#'\donttest{
#'# Simulate 4 time series with hierarchical seasonality
#'# and independent AR1 dynamic processes
#'set.seed(111)
#'simdat <- sim_mvgam(seasonality = 'hierarchical',
#'                    trend_model = AR(),
#'                    family = gaussian())
#'
#'# Fit a model with shared seasonality
#'mod1 <- mvgam(y ~ s(season, bs = 'cc', k = 6),
#'              data = rbind(simdat$data_train,
#'              simdat$data_test),
#'              family = gaussian(),
#'              chains = 2,
#'              silent = 2)
#'
#'# Inspect the model and calculate LOO
#'conditional_effects(mod1)
#'mc.cores.def <- getOption('mc.cores')
#'options(mc.cores = 1)
#'loo(mod1)
#'
#'# Now fit a model with hierarchical seasonality
#'mod2 <- update(mod1,
#'               formula = y ~ s(season, bs = 'cc', k = 6) +
#'               s(season, series, bs = 'fs',
#'               xt = list(bs = 'cc'), k = 4),
#'               chains = 2,
#'               silent = 2)
#'conditional_effects(mod2)
#'loo(mod2)
#'
#'# Now add AR1 dynamic errors to mod2
#'mod3 <- update(mod2,
#'               trend_model = AR(),
#'               chains = 2,
#'               silent = 2)
#'conditional_effects(mod3)
#'plot(mod3, type = 'trend')
#'loo(mod3)
#'
#'# Compare models using LOO
#'loo_compare(mod1, mod2, mod3)
#'options(mc.cores = mc.cores.def)
#'
#'# Compare forecast abilities using an expanding training window and
#'# forecasting ahead 1 timepoint from each window; the first window by includes
#'# the first 92 timepoints (of the 100 that were simulated)
#'max(mod2$obs_data$time)
#'lfo_mod2 <- lfo_cv(mod2, min_t = 92)
#'lfo_mod3 <- lfo_cv(mod3, min_t = 92)
#'
#'# Take the difference in forecast ELPDs; a model with higher ELPD is preferred,
#'# so negative values here indicate that mod3 gave better forecasts for a particular
#'# out of sample timepoint
#'plot(y = lfo_mod2$elpds - lfo_mod3$elpds,
#'     x = lfo_mod2$eval_timepoints, pch = 16,
#'     ylab = 'ELPD_mod2 - ELPD_mod3',
#'     xlab = 'Evaluation timepoint')
#'abline(h = 0, lty = 'dashed')
#'}
#' @export
loo.mvgam <- function(x, incl_dynamics = FALSE, ...) {
  # Families with observation error components can give strange log-likelihood estimates
  # if process error components were also included in the model (this is because the
  # observation error estimates may be very small); use incl_dynamics = TRUE for these
  # families to ensure all errors are propagated appropriately when calculating the
  # log-likelihood
  incl_dynamics <- FALSE
  if (
    x$family %in%
      c(
        "gaussian",
        "lognormal",
        "student"
      )
  ) {
    incl_dynamics <- TRUE
  }

  if (x$family == 'nmix' | incl_dynamics) {
    logliks <- logLik(x, include_forecast = FALSE)
  } else {
    x$series_names <- levels(x$obs_data$series)
    logliks <- logLik(
      x,
      linpreds = predict(
        x,
        newdata = x$obs_data,
        type = 'link',
        summary = FALSE,
        process_error = FALSE
      ),
      newdata = x$obs_data,
      family_pars = extract_family_pars(x),
      include_forecast = FALSE
    )
  }

  logliks <- clean_ll(x, logliks)
  releffs <- loo::relative_eff(
    exp(logliks),
    chain_id = sort(rep(
      1:x$model_output@sim$chains,
      (NROW(logliks) /
        x$model_output@sim$chains)
    ))
  )
  loo::loo(logliks, r_eff = releffs, ...)
}

#' @importFrom loo loo_compare
#' @param x Object of class `mvgam`
#' @param ... More \code{mvgam} objects.
#' @param model_names If `NULL` (the default) will use model names derived
#' from deparsing the call. Otherwise will use the passed values as model names.
#' @param incl_dynamics Deprecated and currently ignored
#' @rdname loo.mvgam
#' @export
loo_compare.mvgam <- function(
  x,
  ...,
  model_names = NULL,
  incl_dynamics = FALSE
) {
  models <- split_mod_dots(x, ..., model_names = model_names)
  loos <- named_list(names(models))
  for (i in seq_along(models)) {
    loos[[i]] <- loo(models[[i]], incl_dynamics = incl_dynamics)
  }
  loo_compare(loos)
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
