# Thin brms-parity wrappers on a fitted `mvgam` object. Each
# method mirrors the corresponding `brms::*.brmsfit` signature
# and delegates to the primitive that already exists in mvgam
# (`as.matrix.mvgam`, `posterior_predict.mvgam`,
# `mvgam_ranef_metadata`) so users can drop into the brms
# workflow without per-method shims.


#' Posterior credible intervals on a fitted \pkg{mvgam} model
#'
#' Returns quantile-based credible intervals for posterior
#' draws of the requested parameters. Mirrors
#' [brms::posterior_interval.brmsfit()] and dispatches through
#' [as.matrix.mvgam()] (so all `variable` shortcuts and
#' aliasing apply unchanged).
#'
#' @param object A fitted `mvgam` object.
#' @param pars Deprecated brms alias for `variable`. If supplied
#'   and `variable` is `NULL`, `pars` is used.
#' @param variable Optional character vector of parameter names
#'   or a `variables(object)` keyword shortcut. Passed straight
#'   through to [as.matrix.mvgam()].
#' @param prob Numeric in `(0, 1)`. The credible-interval mass
#'   to report. Defaults to `0.95`.
#' @param ... Unused; present for S3 / brms-parity.
#'
#' @return A numeric matrix with one row per parameter and two
#'   columns reporting the lower and upper interval bounds.
#'
#' @author Nicholas J Clark
#'
#' @seealso [brms::posterior_interval.brmsfit()],
#'   [as.matrix.mvgam()], [predictive_interval.mvgam()].
#'
#' @method posterior_interval mvgam
#' @export
posterior_interval.mvgam <- function(object, pars = NA,
                                      variable = NULL, prob = 0.95,
                                      ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_number(prob, lower = 0, upper = 1)
  if (is.null(variable) && !identical(pars, NA)) {
    variable <- pars
  }
  draws <- as.matrix(object, variable = variable, ...)
  rstantools::posterior_interval(draws, prob = prob)
}


#' @importFrom rstantools posterior_interval
#' @export
rstantools::posterior_interval


#' Posterior predictive credible intervals on a fitted
#' \pkg{mvgam} model
#'
#' Returns quantile-based credible intervals for posterior
#' predictive draws. Mirrors [brms::predictive_interval.brmsfit()]
#' and routes through [posterior_predict.mvgam()].
#'
#' @param object A fitted `mvgam` object.
#' @param prob Numeric in `(0, 1)`. The credible-interval mass
#'   to report. Defaults to `0.9` (matching brms).
#' @param ... Forwarded to [posterior_predict.mvgam()].
#'
#' @return A numeric matrix with one row per observation and two
#'   columns reporting the lower and upper interval bounds.
#'
#' @author Nicholas J Clark
#'
#' @seealso [brms::predictive_interval.brmsfit()],
#'   [posterior_predict.mvgam()], [posterior_interval.mvgam()].
#'
#' @method predictive_interval mvgam
#' @export
predictive_interval.mvgam <- function(object, prob = 0.9, ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_number(prob, lower = 0, upper = 1)
  preds <- posterior_predict(object, ...)
  rstantools::predictive_interval(preds, prob = prob)
}


#' @importFrom rstantools predictive_interval
#' @export
rstantools::predictive_interval


#' Test posterior hypotheses on a fitted \pkg{mvgam} model
#'
#' Wraps [brms::hypothesis()] so it works on `mvgam` fits.
#' `brms::hypothesis.brmsfit` is the default dispatch (mvgam is
#' a `brmsfit` subclass), but its internal helpers expect a
#' brmsfit-shaped Stan stancode / data layout that mvgam does
#' not store, so the brms-fit method raises an `rbind` error on
#' an mvgam object. This wrapper bypasses that path: it pulls
#' posterior draws via [posterior::as_draws_df()] and forwards
#' them to `brms::hypothesis.default`, which evaluates the
#' hypothesis string against the draws data frame directly.
#'
#' @param x A fitted `mvgam` object.
#' @param hypothesis Character vector of hypothesis strings.
#'   Each string uses the standard `brms::hypothesis` grammar
#'   (e.g. `"ar1_trend[1] > 0"`, `"sigma_trend[1] < 1"`,
#'   `"(b_x - b_z) = 0"`).
#' @param alpha One minus the credible-interval mass. Defaults
#'   to `0.05` (90%-CI for one-sided, 95%-CI for two-sided).
#' @param robust Logical. Use median / MAD instead of mean / SD
#'   when summarising. Defaults to `FALSE`.
#' @param ... Forwarded to `brms::hypothesis`.
#'
#' @return A `brmshypothesis` object. See [brms::hypothesis()].
#'
#' @author Nicholas J Clark
#'
#' @seealso [brms::hypothesis()], [posterior::as_draws_df()],
#'   [variables.mvgam()].
#'
#' @examples
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR())
#'
#' mod <- mvgam(y ~ s(x),
#'               trend_formula = ~ AR(p = 1),
#'               data    = simdat$data_train,
#'               family  = poisson(),
#'               chains  = 2, silent = 2)
#'
#' # Single hypothesis.
#' hypothesis(mod, "ar1_trend[1] > 0")
#'
#' # Multiple hypotheses in one call.
#' hypothesis(mod, c("ar1_trend[1] > 0",
#'                    "sigma_trend[1] < 1"))
#' }
#'
#' @importFrom brms hypothesis
#' @method hypothesis mvgam
#' @export
hypothesis.mvgam <- function(x, hypothesis, alpha = 0.05,
                              robust = FALSE, ...) {
  checkmate::assert_class(x, "mvgam")
  checkmate::assert_character(hypothesis, min.len = 1L,
                                any.missing = FALSE)
  checkmate::assert_number(alpha, lower = 0, upper = 1)
  checkmate::assert_flag(robust)
  # Under the raw stanfit names every parameter mvgam aliases is
  # unreachable: `b_elev` is `b[1]` there, `sd_block__Intercept` is
  # `sd_1[1]`, and a hypothesis naming either was refused as a
  # parameter the model does not have.
  draws <- as.data.frame(
    posterior::as_draws_df(extract_mvgam_draws(x))
  )
  brms::hypothesis(draws, hypothesis = hypothesis,
                    alpha = alpha, robust = robust, ...)
}


#' Bridge sampling and Bayes factors for `mvgam` fits
#'
#' Log marginal likelihood via bridge sampling (Gronau, Singmann
#' & Wagenmakers 2020) and paired Bayes factors, delegating to the
#' `bridgesampling` package on the underlying `stanfit`. Bridge
#' sampling answers the M-closed model-choice question that
#' predictive scoring (\code{\link{loo.mvgam}},
#' \code{\link{lfo_cv.mvgam}}) cannot: which model does the data
#' support as the data-generating process, weighted by the prior?
#' On state-space fits this is a natural complement to LFO because
#' the answer does not depend on the choice of forecast horizon.
#'
#' The routing bypasses the internal brms `bridge_sampler.brmsfit`
#' method, whose `restructure()` and `update_misc_env()` calls assume a
#' brmsfit that mvgam objects do not conform to. Instead the S3
#' method resolves the requirements itself and delegates directly
#' to `bridgesampling::bridge_sampler()` on `object$fit`.
#'
#' Bridge sampling requires the Stan model to use normalized
#' probability densities (`_lpdf` / `_lpmf`), not the un-normalized
#' variants (`_lupdf` / `_lupmf`). mvgam's shipped emitters already
#' satisfy this; the method errors early with an informative
#' message if the constraint is violated (typically because a user
#' hand-edited the Stan code).
#'
#' @section Multivariate fits: On an `mvbf` fit the Stan program
#'   computes a single joint log posterior pooling every response
#'   family's likelihood, so `bridge_sampler()` naturally estimates
#'   the joint log marginal likelihood
#'   \eqn{\log p(y_{1}, \ldots, y_{K} \mid \mathrm{model})}. The
#'   bridge-sampling estimator variance grows with parameter
#'   dimension: for multi-response, factor or VAR fits pass
#'   `samples = 10000` or larger, and consider
#'   `method = "warp3"` when the default `"normal"` returns high
#'   `re2` (relative squared error).
#'
#' @param samples An object of class `mvgam` (the argument name
#'   `samples` matches the `bridgesampling` generic).
#' @param recompile Logical. When `TRUE`, refits the model via
#'   [update.mvgam()] before running the bridge sampler. Set
#'   this when the fit was loaded from disk and its compiled
#'   Stan DSO is no longer valid in the current R session (the
#'   usual RDS-reload symptom is
#'   `"the model object is not created or not valid"`). The
#'   refit runs through the same backend the original fit used
#'   (rstan or cmdstanr) and redraws the posterior, so estimates
#'   from `recompile = TRUE` are stochastic in both the sampler
#'   and the bridge estimator. `...` is passed only to
#'   `bridgesampling::bridge_sampler()`; to customise the refit
#'   itself, call `update()` explicitly with the desired
#'   arguments first and then pass the recompiled fit here with
#'   `recompile = FALSE`. Defaults to `FALSE`.
#' @param x1,x2 `mvgam` fits, or `bridge` objects returned by
#'   `bridge_sampler()`, to compare.
#' @param log Logical. Return the Bayes factor on the log scale
#'   when `TRUE`; ratio scale otherwise.
#' @param ... Additional arguments forwarded to
#'   `bridgesampling::bridge_sampler()` (for example `samples`,
#'   `method`, `cores`).
#'
#' @return `bridge_sampler.mvgam()` returns a `bridge` object with
#'   a finite `logml`. `bayes_factor.mvgam()` returns a
#'   `bayes_factor` object with the numeric ratio (or log-ratio).
#'
#' @references
#' Gronau QF, Singmann H and Wagenmakers E-J (2020).
#' bridgesampling: An R package for estimating normalizing
#' constants. *Journal of Statistical Software*. 92(10).
#'
#' @seealso \code{\link{loo.mvgam}}, \code{\link{lfo_cv.mvgam}}.
#'
#' @author Nicholas J Clark
#'
#' @importFrom brms bridge_sampler
#' @method bridge_sampler mvgam
#' @export
bridge_sampler.mvgam <- function(samples, recompile = FALSE, ...) {
  checkmate::assert_class(samples, "mvgam")
  checkmate::assert_flag(recompile)
  if (!requireNamespace("bridgesampling", quietly = TRUE)) {
    stop(insight::format_error(c(
      "Package 'bridgesampling' is required.",
      i = "Install it with install.packages('bridgesampling')."
    )))
  }
  # Bridge sampling reads the joint density off the compiled model,
  # so a density that dropped its normalising constant moves the log
  # marginal likelihood by an unknown amount. The offset cancels in
  # a Bayes factor only when both models carry priors matching in
  # family, hyperparameters and dimension, which is exactly what a
  # comparison across trend structures does not. Reading both
  # spellings covers brms, which writes `_lupdf`, and mvgam, which
  # writes `~`.
  offenders <- mvgam_unnormalized_terms(samples$stancode)
  if (length(offenders) > 0L) {
    stop(insight::format_error(c(
      "The Stan model must be normalized to run bridge_sampler().",
      x = paste0(
        "Densities dropping a normalising constant: ",
        paste(offenders, collapse = ", "), "."
      ),
      i = paste0(
        "Refit with 'normalize = TRUE', which is the default. A ",
        "model built with 'normalize = FALSE' samples faster, but ",
        "its log marginal likelihood carries an offset that does ",
        "not cancel between models of different dimension."
      )
    )))
  }
  if (isTRUE(recompile)) {
    # `...` is reserved for bridgesampling::bridge_sampler() and
    # would collide with mvgam()'s own argument names (e.g.
    # `silent` differs in type). Callers who need to customise
    # the refit should call `update()` themselves first, then
    # pass the recompiled fit with `recompile = FALSE`.
    samples <- update(samples, recompile = TRUE)
  }
  bridgesampling::bridge_sampler(samples$fit, ...)
}


#' @rdname bridge_sampler.mvgam
#' @importFrom brms bayes_factor
#' @method bayes_factor mvgam
#' @export
bayes_factor.mvgam <- function(x1, x2, log = FALSE, ...) {
  checkmate::assert(
    checkmate::check_class(x1, "mvgam"),
    checkmate::check_class(x1, "bridge"),
    combine = "or", .var.name = "x1"
  )
  checkmate::assert(
    checkmate::check_class(x2, "mvgam"),
    checkmate::check_class(x2, "bridge"),
    combine = "or", .var.name = "x2"
  )
  checkmate::assert_flag(log)
  bs1 <- if (inherits(x1, "bridge")) x1 else bridge_sampler(x1, ...)
  bs2 <- if (inherits(x2, "bridge")) x2 else bridge_sampler(x2, ...)
  bridgesampling::bayes_factor(bs1, bs2, log = log)
}


#' Number of levels per grouping factor in a fitted
#' \pkg{mvgam} model
#'
#' Returns a named list of integer counts: one entry per
#' grouping factor with the number of levels it contains.
#' Mirrors [brms::ngrps.brmsfit()] and returns `NULL` when the
#' fit has no observation-side random effects.
#'
#' @param object A fitted `mvgam` object.
#' @param ... Unused; present for S3 / brms-parity.
#'
#' @return A named list of integer scalars, or `NULL` if no
#'   group-level effects are present.
#'
#' @author Nicholas J Clark
#'
#' @seealso [brms::ngrps.brmsfit()], [ranef.mvgam()],
#'   [VarCorr.mvgam()].
#'
#' @method ngrps mvgam
#' @export
ngrps.mvgam <- function(object, ...) {
  checkmate::assert_class(object, "mvgam")
  meta <- mvgam_ranef_metadata(object)
  if (is.null(meta)) {
    return(NULL)
  }
  as.list(lengths(meta$group_levels))
}


#' @importFrom brms ngrps
#' @export
brms::ngrps


#' Posterior predictive errors on a fitted \pkg{mvgam} model
#'
#' Returns posterior draws of the response-scale prediction
#' error (observed minus predicted) for each observation.
#' Mirrors [brms::predictive_error.brmsfit()].
#'
#' @param object A fitted `mvgam` object.
#' @param newdata Optional `data.frame` to compute errors on.
#'   `NULL` (the default) uses the training data.
#' @param method One of `"posterior_predict"` (default) or
#'   `"posterior_epred"`. Selects the predicted-value primitive
#'   from which errors are taken.
#' @param resp,re_formula,re.form,ndraws,draw_ids,sort
#'   Forwarded to the prediction primitive; matched to brms's
#'   signature.
#' @param ... Unused; present for S3 / brms-parity.
#'
#' @return A numeric matrix of dimension
#'   `[n_draws x n_observations]` containing observed minus
#'   predicted for each draw and observation.
#'
#' @details
#' This is the quantity [residuals.mvgam()] returns under
#' `type = "ordinary"`, and it reads the same surface: in sample the
#' prediction carries the latent trend state the model inferred at
#' that time, while `newdata` the fit never saw integrates over the
#' trend dynamics instead.
#'
#' @author Nicholas J Clark
#'
#' @seealso [brms::predictive_error.brmsfit()], [residuals.mvgam()],
#'   [posterior_predict.mvgam()], [posterior_epred.mvgam()].
#'
#' @method predictive_error mvgam
#' @export
predictive_error.mvgam <- function(object, newdata = NULL,
                                    re_formula = NULL,
                                    re.form = NULL,
                                    method = "posterior_predict",
                                    resp = NULL, ndraws = NULL,
                                    draw_ids = NULL, sort = FALSE,
                                    ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_choice(
    method, c("posterior_predict", "posterior_epred")
  )
  if (!is.null(re.form) && is.null(re_formula)) {
    re_formula <- re.form
  }
  resolve_resp(object, resp, required = TRUE,
               caller = "predictive_error()")
  data <- if (is.null(newdata)) object$data else newdata
  resp_name <- response_column(object, resp)
  if (!resp_name %in% names(data)) {
    stop(insight::format_error(c(
      paste0(
        "Cannot compute predictive errors: response '",
        resp_name, "' not in 'newdata'."
      ),
      i = paste0(
        "Pass a 'newdata' that includes the response column ",
        "or omit 'newdata' to use the training data."
      )
    )))
  }
  pred_fun <- if (identical(method, "posterior_predict")) {
    posterior_predict
  } else {
    posterior_epred
  }
  # The same quantity `residuals(type = "ordinary")` returns, so it
  # reads the same surface: in sample, the state the model inferred.
  preds <- do.call(pred_fun, diagnostic_surface_args(
    list(object, newdata = newdata, re_formula = re_formula,
         resp = resp, ndraws = ndraws, draw_ids = draw_ids, ...),
    is.null(newdata)
  ))
  y <- as.numeric(data[[resp_name]])
  if (ncol(preds) != length(y)) {
    stop(insight::format_error(c(
      "Internal: predictor output and response length disagree.",
      x = paste0(
        "Predictor columns: ", ncol(preds),
        "; response observations: ", length(y), "."
      )
    )))
  }
  out <- sweep(preds, 2L, y, FUN = function(p, obs) obs - p)
  if (isTRUE(sort)) {
    out <- out[, order(y), drop = FALSE]
  }
  out
}


#' @importFrom rstantools predictive_error
#' @export
rstantools::predictive_error


# Deprecated brms aliases. `marginal_smooths()`, `marginal_effects()`
# and `as.mcmc()` are covered by `conditional_smooths()`,
# `conditional_effects()` and `posterior::as_draws_array()`, and
# carry no mvgam method.
#
# `parnames()` and `nsamples()` do need one. An `mvgam` object also
# inherits from `brmsfit`, so a call to either resolves to the brms
# method, which reads the raw stanfit and reports parameters and
# draws that `variables()` and `ndraws()` deliberately filter out.
# Without delegation, code calling the deprecated spelling would
# disagree with the current spelling on the same fit. Delegating
# keeps the two answers identical while brms still raises its own
# deprecation warning.

#' @importFrom brms parnames
#' @method parnames mvgam
#' @export
parnames.mvgam <- function(x, ...) {
  variables(x, ...)
}


#' @importFrom brms nsamples
#' @method nsamples mvgam
#' @export
nsamples.mvgam <- function(object, ...) {
  posterior::ndraws(posterior::as_draws(object$fit))
}


# Internal: training data for a fitted `mvgam`. Prefers
# `obs_data` (the post-fit canonical slot) and falls back to
# `data` (raw input). Returns NULL when neither slot is set.
# Defined once so every caller reads the training data the same
# way. Assertion is intentionally
# omitted: called in low-overhead inner loops where the caller
# has already validated.
#'@noRd
mvgam_training_data <- function(object) {
  object$obs_data %||% object$data
}


# ============================================================
# brms helper-parity dispatch methods
# ============================================================
#
# mvgam-side dispatch methods that brms's default dispatchers do
# not know about. Each method forwards to an existing mvgam helper:
#
#   default_prior.mvgam_formula -> get_prior.mvgam_formula
#   default_prior.mvgam         -> get_prior.mvgam
#   standata.mvgam              -> object$standata
#
# stancode.mvgam / stancode.mvgam_formula / standata.mvgam_formula
# already exist; brms::make_stancode and brms::make_standata are
# thin wrappers around stancode() / standata() so they dispatch
# correctly via the existing methods with no extra code.

#' Stan data for a fitted mvgam model
#'
#' Returns the Stan data list that was generated at fit time
#' and stored on `object$standata`. Mirrors [stancode.mvgam()];
#' useful for re-running the same model with `rstan` or
#' `cmdstanr` directly, or for inspecting which design matrices
#' brms produced from the formula and data.
#'
#' @param object A fitted `mvgam` model.
#' @param ... Currently unused; present for S3 generic dispatch.
#' @return A named list of Stan data.
#' @method standata mvgam
#' @export
standata.mvgam <- function(object, ...) {
  checkmate::assert_class(object, "mvgam")
  if (is.null(object$standata)) {
    stop(insight::format_error(c(
      "Stan data not found in mvgam object.",
      i = paste0(
        "The model may have been fitted with an older version ",
        "that did not store the Stan data list."
      )
    )))
  }
  object$standata
}

#' Default priors for an mvgam_formula
#'
#' Alias for [get_prior.mvgam_formula()] under the brms
#' `default_prior()` generic. mvgam routes both names to the
#' same prior-construction code, so this method exists purely
#' so users coming from newer brms releases can call
#' `default_prior(mf, data = ...)` interchangeably with
#' `get_prior(mf, data = ...)`.
#'
#' @inheritParams get_prior.mvgam_formula
#' @return A `brmsprior` data frame.
#' @method default_prior mvgam_formula
#' @export
default_prior.mvgam_formula <- function(object, data,
                                        family = gaussian(), ...) {
  get_prior.mvgam_formula(
    object, data = data, family = family, ...
  )
}

#' Default priors for a fitted mvgam model
#'
#' Alias for [get_prior.mvgam()] under the brms
#' `default_prior()` generic. Returns the prior table the model was
#' fit with, the one [prior_summary()] reports.
#'
#' @param object A fitted `mvgam` model.
#' @param ... Unused; an argument given here is refused.
#' @return A `brmsprior` data frame.
#' @method default_prior mvgam
#' @export
default_prior.mvgam <- function(object, ...) {
  get_prior.mvgam(object, ...)
}


#' Extract sampler control parameters from a fitted mvgam model
#'
#' Method on `brms::control_params()` for mvgam fits. Reads the
#' NUTS control settings (adapt_delta, max_treedepth, stepsize,
#' etc.) from the underlying stanfit's `@stan_args` slot. The
#' brms default method only dispatches on `brmsfit`, so without
#' this method `control_params(fit)` fails on an mvgam object.
#'
#' @param x A fitted `mvgam` model.
#' @param ... Currently unused.
#' @return Named list of NUTS control settings (same shape as
#'   `brms::control_params()` on a `brmsfit`); empty list when
#'   no NUTS args are stored (e.g. variational fits).
#' @method control_params mvgam
#' @export
control_params.mvgam <- function(x, ...) {
  checkmate::assert_class(x, "mvgam")
  fit_obj <- x$fit
  if (!isS4(fit_obj) ||
        !"stan_args" %in% methods::slotNames(fit_obj)) {
    return(list())
  }
  args <- fit_obj@stan_args
  if (length(args) == 0L) return(list())
  ctrl <- args[[1L]]$control
  ctrl %||% list()
}

#' Extract initial values used in a fitted mvgam model
#'
#' Method on `brms::inits()` for mvgam fits. Reads the per-chain
#' `init` argument from the underlying stanfit's `@stan_args`
#' slot. The brms default method only dispatches on `brmsfit`,
#' so without this method `inits(fit)` fails on an mvgam object.
#'
#' @param x A fitted `mvgam` model.
#' @param ... Currently unused.
#' @return List with one element per chain, mirroring the `init`
#'   argument passed to the sampler.
#' @method inits mvgam
#' @export
inits.mvgam <- function(x, ...) {
  checkmate::assert_class(x, "mvgam")
  fit_obj <- x$fit
  if (!isS4(fit_obj) ||
        !"stan_args" %in% methods::slotNames(fit_obj)) {
    return(list())
  }
  args <- fit_obj@stan_args
  if (length(args) == 0L) return(list())
  lapply(args, function(a) a$init)
}
