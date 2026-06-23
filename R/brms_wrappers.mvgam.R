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
#' \donttest{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 60L, trend_model = AR())
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
#' @export hypothesis
#' @method hypothesis mvgam
#' @export
hypothesis.mvgam <- function(x, hypothesis, alpha = 0.05,
                              robust = FALSE, ...) {
  checkmate::assert_class(x, "mvgam")
  checkmate::assert_character(hypothesis, min.len = 1L,
                                any.missing = FALSE)
  checkmate::assert_number(alpha, lower = 0, upper = 1)
  checkmate::assert_flag(robust)
  draws <- as.data.frame(posterior::as_draws_df(x$fit))
  brms::hypothesis(draws, hypothesis = hypothesis,
                    alpha = alpha, robust = robust, ...)
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
#' @author Nicholas J Clark
#'
#' @seealso [brms::predictive_error.brmsfit()],
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
  data <- if (is.null(newdata)) object$data else newdata
  resp_name <- mvgam_response_name(object)
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
  preds <- pred_fun(
    object, newdata = newdata, re_formula = re_formula,
    resp = resp, ndraws = ndraws, draw_ids = draw_ids, ...
  )
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


# Deprecated brms aliases (marginal_smooths / marginal_effects /
# parnames / nsamples / as.mcmc) were dropped. brms itself
# deprecated the first four; the modern replacements
# (`conditional_smooths()`, `conditional_effects()`,
# `variables()`, `posterior::ndraws()`) are already exported on
# mvgam. `as.mcmc.mvgam` exposed the coda interface, which
# `posterior::as_draws_array(x)` / `as.array(x)` cover for
# downstream tooling; the coda dependency went with it.


# Internal: return the response variable's name from the fit's
# observation formula. Single source of truth for predictive
# methods that need the column.
#'@noRd
mvgam_response_name <- function(object) {
  checkmate::assert_class(object, "mvgam")
  f <- object$formula
  if (inherits(f, "brmsformula")) f <- f$formula
  all.vars(f[[2L]])[1L]
}


# Internal: training data for a fitted `mvgam`. Prefers
# `obs_data` (the post-fit canonical slot) and falls back to
# `data` (raw input). Returns NULL when neither slot is set.
# Single source of truth so every downstream consumer reads the
# training data the same way. Assertion is intentionally
# omitted: called in low-overhead inner loops where the caller
# has already validated.
#'@noRd
mvgam_training_data <- function(object) {
  object$obs_data %||% object$data
}


# ============================================================
# brms helper-parity re-exports + dispatch (Task #189)
# ============================================================
#
# Re-export the brms generics so users can call them without
# the `brms::` namespace prefix, and add the few mvgam-side
# dispatch methods that brms's default dispatchers do not know
# about. Each method here forwards to an existing mvgam helper:
#
#   default_prior.mvgam_formula -> get_prior.mvgam_formula
#   default_prior.mvgam         -> get_prior.mvgam
#   standata.mvgam              -> object$standata
#
# stancode.mvgam / stancode.mvgam_formula / standata.mvgam_formula
# already exist; brms::make_stancode and brms::make_standata are
# thin wrappers around stancode() / standata() so they dispatch
# correctly via the existing methods with no extra code.

#' @importFrom brms stancode
#' @export stancode
NULL

#' @importFrom brms standata
#' @export standata
NULL

#' @importFrom brms make_stancode
#' @export make_stancode
NULL

#' @importFrom brms make_standata
#' @export make_standata
NULL

#' @importFrom brms default_prior
#' @export default_prior
NULL

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
#' `default_prior()` generic. Returns the literal prior table
#' the model was fit with (`object$prior`).
#'
#' @param object A fitted `mvgam` model.
#' @param ... Currently unused; present for S3 generic dispatch.
#' @return A `brmsprior` data frame.
#' @method default_prior mvgam
#' @export
default_prior.mvgam <- function(object, ...) {
  get_prior.mvgam(object, ...)
}


# ============================================================
# brms prior / family / formula / stanvar re-exports
# ============================================================
# Re-export the user-facing brms helpers that mvgam workflows
# routinely reach for, so users do not need to type `brms::`
# for the prior-writing, family-constructing, formula-wrapping
# and stanvar-building APIs. All zero-code re-exports; the
# generics live in brms and dispatch unchanged.

#' @importFrom brms set_prior
#' @export set_prior
NULL

#' @importFrom brms prior
#' @export prior
NULL

#' @importFrom brms prior_
#' @export prior_
NULL

#' @importFrom brms prior_string
#' @export prior_string
NULL

#' @importFrom brms empty_prior
#' @export empty_prior
NULL

#' @importFrom brms as.brmsprior
#' @export as.brmsprior
NULL

#' @importFrom brms is.brmsprior
#' @export is.brmsprior
NULL

#' @importFrom brms validate_prior
#' @export validate_prior
NULL

#' @importFrom brms prior_draws
#' @export prior_draws
NULL

#' @importFrom brms prior_samples
#' @export prior_samples
NULL

#' @importFrom brms brmsfamily
#' @export brmsfamily
NULL

# Re-export brms-specific response families so `library(mvgam)`
# alone exposes them; users no longer need to type `brms::Beta()`
# etc. Standard base-R families (gaussian, poisson, Gamma,
# binomial, ...) are already on the search path.
#' @importFrom brms Beta
#' @export Beta
NULL

#' @importFrom brms exponential
#' @export exponential
NULL

#' @importFrom brms lognormal
#' @export lognormal
NULL

#' @importFrom brms student
#' @export student
NULL

#' @importFrom brms bernoulli
#' @export bernoulli
NULL

#' @importFrom brms negbinomial
#' @export negbinomial
NULL

#' @importFrom brms beta_binomial
#' @export beta_binomial
NULL

#' @importFrom brms custom_family
#' @export custom_family
NULL

#' @importFrom brms brmsformula
#' @export brmsformula
NULL

#' @importFrom brms bf
#' @export bf
NULL

#' @importFrom brms mvbrmsformula
#' @export mvbrmsformula
NULL

#' @importFrom brms is.brmsformula
#' @export is.brmsformula
NULL

#' @importFrom brms is.mvbrmsformula
#' @export is.mvbrmsformula
NULL

#' @importFrom brms stanvar
#' @export stanvar
NULL

#' @importFrom brms validate_newdata
#' @export validate_newdata
NULL

#' @importFrom brms constant
#' @export constant
NULL

#' @importFrom brms inits
#' @export inits
NULL

#' @importFrom brms control_params
#' @export control_params
NULL

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
