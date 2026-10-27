# Methods of brms and rstantools generics for an `mvgam` object. Each
# takes the signature of its `brmsfit` method and calls an mvgam
# function such as `as.matrix.mvgam()` or `posterior_predict.mvgam()`.


#' Posterior credible intervals on a fitted \pkg{mvgam} model
#'
#' Central quantile intervals of the posterior draws of parameters. The
#' parameters are chosen as in [as.matrix.mvgam()].
#'
#' @inheritParams as.matrix.mvgam
#' @param object A fitted `mvgam` object.
#' @param pars Deprecated alias for `variable`.
#' @param prob Numeric in `(0, 1)`. The probability mass each interval
#'   holds. Defaults to `0.95`.
#' @param ... Further arguments of [as.matrix.mvgam()], such as
#'   `regex`.
#'
#' @return A numeric matrix with one row per parameter and two columns,
#'   the lower and upper bounds.
#'
#' @author Nicholas J Clark
#'
#' @seealso [predictive_interval.mvgam()] gives intervals of predictive
#'   draws. [summary.mvgam()] reports intervals with convergence
#'   diagnostics.
#'
#' @examples
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                     n_timepoints = 120L, trend_model = AR())
#' mod <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1),
#'              data = simdat$data_train, family = poisson(),
#'              chains = 2, silent = 2)
#'
#' # 90 percent intervals of the autoregression and its innovation SD
#' posterior_interval(mod, variable = c("ar1_trend", "sigma_trend"),
#'                    prob = 0.9)
#'
#' # Next: summary() reports these with convergence diagnostics
#' }
#'
#' @family posterior summaries
#' @method posterior_interval mvgam
#' @export
posterior_interval.mvgam <- function(object, pars = NA,
                                      variable = NULL, prob = 0.95,
                                      ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_number(prob, lower = 0, upper = 1)
  variable <- mvgam_use_alias(variable, if (!identical(pars, NA)) pars)
  draws <- as.matrix(object, variable = variable, ...)
  rstantools::posterior_interval(draws, prob = prob)
}


#' @importFrom rstantools posterior_interval
#' @export
rstantools::posterior_interval


#' Posterior predictive intervals on a fitted \pkg{mvgam} model
#'
#' Central quantile intervals of the draws [posterior_predict.mvgam()]
#' returns, one per observation.
#'
#' @param object A fitted `mvgam` object.
#' @param prob Numeric in `(0, 1)`. The probability mass each interval
#'   holds. Defaults to `0.9`.
#' @param ... Arguments of [posterior_predict.mvgam()], such as
#'   `newdata` or `incl_autocor`.
#'
#' @return A numeric matrix with one row per observation and two
#'   columns, the lower and upper bounds.
#'
#' @author Nicholas J Clark
#'
#' @seealso [posterior_interval.mvgam()] gives intervals of parameters.
#'   [pp_check.mvgam()] plots predictive draws against the data.
#'
#' @examples
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                     n_timepoints = 120L, trend_model = AR())
#' mod <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1),
#'              data = simdat$data_train, family = poisson(),
#'              chains = 2, silent = 2)
#'
#' # 90 percent intervals, one row per observation
#' intervals <- predictive_interval(mod)
#' head(intervals)
#'
#' # The share of observed counts the intervals cover
#' y <- simdat$data_train$y
#' mean(y >= intervals[, 1] & y <= intervals[, 2])
#'
#' # Next: pp_check() plots the predictive draws against the data
#' }
#'
#' @family predictions
#' @method predictive_interval mvgam
#' @export
predictive_interval.mvgam <- function(object, prob = 0.9, ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_number(prob, lower = 0, upper = 1)
  rstantools::predictive_interval(posterior_predict(object, ...),
                                  prob = prob)
}


#' @importFrom rstantools predictive_interval
#' @export
rstantools::predictive_interval


#' Test posterior hypotheses on a fitted \pkg{mvgam} model
#'
#' The `mvgam` method of [brms::hypothesis()], with the arguments and
#' parameter names of `brms::hypothesis.brmsfit()`. By default a
#' hypothesis names population-level coefficients without their `b_`
#' prefix, as `"x > 0"` tests `b_x`. `class = NULL` names any
#' parameter by the full name [variables.mvgam()] lists, such as
#' `ar1_trend[series_1]` or `sigma`. That help page sets out how the
#' names are formed.
#'
#' @inheritParams predict.mvgam
#' @param x A fitted `mvgam` object.
#' @param hypothesis Character vector of hypothesis strings in the
#'   [brms::hypothesis()] grammar, such as `"x > 0"` or
#'   `"(x - z) = 0"`.
#' @param class The class prefix the names in `hypothesis` omit.
#'   Defaults to `"b"`. `NULL` or `""` takes every parameter by its
#'   full name.
#' @param group The grouping factor of a group-level class, as
#'   `class = "sd", group = "site"` tests `sd_site__Intercept` as
#'   `Intercept`.
#' @param alpha One minus the credible-interval mass. Defaults
#'   to `0.05` (90%-CI for one-sided, 95%-CI for two-sided).
#' @param seed Optional integer seed for the random draws of an
#'   evidence ratio. The session's random state is restored when the
#'   call returns.
#' @param ... Forwarded to `brms::hypothesis`.
#'
#' @return A `brmshypothesis` object. See [brms::hypothesis()].
#'
#' @author Nicholas J Clark
#'
#' @seealso [brms::hypothesis()], [variables.mvgam()].
#'
#' @examples
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR())
#'
#' mod <- mvgam(y ~ x,
#'               trend_formula = ~ AR(p = 1),
#'               data    = simdat$data_train,
#'               family  = poisson(),
#'               chains  = 2, silent = 2)
#'
#' # A population-level coefficient, named as fixef() names it.
#' hypothesis(mod, "x > 0")
#'
#' # Any parameter by its full name.
#' hypothesis(mod, c("ar1_trend[series_1] > 0",
#'                   "sigma_trend[series_1] < 1"),
#'            class = NULL)
#' }
#'
#' @importFrom brms hypothesis
#' @family posterior summaries
#' @method hypothesis mvgam
#' @export
hypothesis.mvgam <- function(x, hypothesis, class = "b", group = "",
                             alpha = 0.05, robust = FALSE, seed = NULL,
                             ...) {
  checkmate::assert_class(x, "mvgam")
  checkmate::assert_character(hypothesis, min.len = 1L,
                              any.missing = FALSE)
  checkmate::assert_string(class, null.ok = TRUE)
  checkmate::assert_string(group)
  checkmate::assert_number(alpha, lower = 0, upper = 1)
  checkmate::assert_flag(robust)
  checkmate::assert_count(seed, null.ok = TRUE)
  local_seed(seed)
  # `brms::hypothesis.brmsfit()` needs the brmsfit internals mvgam
  # does not store. Its draws come from the projection every draws
  # method shares, which names `b[1]` as `b_x`.
  draws <- as.data.frame(
    posterior::as_draws_df(extract_mvgam_draws(x))
  )
  # The prefix `brms::hypothesis.brmsfit()` builds from `class` and
  # `group`, which a hypothesis leaves off its names
  prefix <- if (length(class) && nzchar(class)) {
    paste0(class, if (nzchar(group)) paste0("_", group, "__") else "_")
  } else {
    ""
  }
  if (nzchar(prefix)) {
    keep <- startsWith(names(draws), prefix)
    draws <- stats::setNames(draws[keep],
                             substring(names(draws)[keep],
                                       nchar(prefix) + 1L))
  }
  brms::hypothesis(draws, hypothesis = hypothesis,
                   alpha = alpha, robust = robust, ...)
}


#' Bridge sampling and Bayes factors for `mvgam` fits
#'
#' Estimates the log marginal likelihood of a fit by bridge sampling
#' (Gronau, Singmann and Wagenmakers 2020) and compares two fits by
#' their Bayes factor. The marginal likelihood weighs how well a model
#' explains the data under its prior. [loo.mvgam()] and
#' [lfo_cv.mvgam()] score predictions of held-out data instead. A
#' Bayes factor sets no forecast horizon, which makes it a complement
#' to leave-future-out scores for state-space models. It also
#' depends on the priors, which predictive scores barely do.
#'
#' The Stan program must keep the normalising constants of its
#' densities. mvgam builds it that way by default. A fit built with
#' `normalize = FALSE` is refused. Bridge sampling also needs the draws
#' of every parameter, which a fit keeps under
#' `save_pars = save_pars(all = TRUE)`.
#'
#' @section Multivariate fits: The Stan program of an `mvbf()` fit has
#'   one joint log posterior over every response. `bridge_sampler()`
#'   then estimates the joint log marginal likelihood
#'   \eqn{\log p(y_{1}, \ldots, y_{K} \mid \mathrm{model})}. The
#'   variance of the estimate grows with the number of parameters. For
#'   multi-response, factor or VAR fits, pass `repetitions` above 1 or
#'   `method = "warp3"` when the default method reports a high `re2`
#'   (relative squared error).
#'
#' @param samples An `mvgam` fit. The name follows the
#'   \pkg{bridgesampling} generic.
#' @param recompile Logical. Bridge sampling evaluates the density
#'   through a compiled rstan model. A fit without one, sampled by
#'   cmdstanr or loaded with [readRDS()] in a new session, has one
#'   compiled, with its draws kept. `TRUE` compiles one for any fit. Defaults to `FALSE`.
#' @param x1,x2 `mvgam` fits, or `bridge` objects returned by
#'   `bridge_sampler()`, to compare.
#' @param log Logical. `TRUE` returns the log Bayes factor.
#' @param ... Arguments of `bridgesampling::bridge_sampler()`, such as
#'   `method`, `repetitions` or `cores`.
#'
#' @return `bridge_sampler()` returns a `bridge` object holding the
#'   log marginal likelihood in `logml`. `bayes_factor()` returns a
#'   `bayes_factor` object.
#'
#' @examples
#' \dontrun{
#' set.seed(4)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                     n_timepoints = 80L, trend_model = AR())
#'
#' # Bridge sampling needs many draws and every parameter's draws. It
#' # evaluates the density through rstan. A fit sampled by rstan needs
#' # no second compile.
#' mod_ar <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
#'                 data = simdat$data_train, family = poisson(),
#'                 save_pars = save_pars(all = TRUE), backend = "rstan",
#'                 chains = 2, iter = 3000, silent = 2)
#' mod_rw <- update(mod_ar, trend_formula = ~ RW())
#'
#' # The log marginal likelihood of each model
#' bs_ar <- bridge_sampler(mod_ar, silent = TRUE)
#' bs_rw <- bridge_sampler(mod_rw, silent = TRUE)
#' bs_ar
#'
#' # A Bayes factor above 1 favours the AR(1) trend. Passing the
#' # bridge objects reuses their estimates.
#' bayes_factor(bs_ar, bs_rw)
#'
#' # Next, compare their forecasts with loo() or lfo_cv()
#' }
#'
#' @references
#' Gronau QF, Singmann H and Wagenmakers E-J (2020).
#' bridgesampling: An R package for estimating normalizing
#' constants. *Journal of Statistical Software*. 92(10).
#'
#' @seealso [loo.mvgam()], [lfo_cv.mvgam()]. The
#'   `r mvgam_article("forecast_evaluation", "forecast evaluation article")`
#'   compares models by their forecasts.
#'
#' @author Nicholas J Clark
#'
#' @importFrom brms bridge_sampler
#' @family model comparison
#' @method bridge_sampler mvgam
#' @export
bridge_sampler.mvgam <- function(samples, recompile = FALSE, ...) {
  checkmate::assert_class(samples, "mvgam")
  checkmate::assert_flag(recompile)
  insight::check_if_installed("bridgesampling")
  # Bridge sampling evaluates the joint density of the compiled model.
  # A density without its normalising constant shifts the log marginal
  # likelihood by an unknown offset. The offset cancels in a Bayes
  # factor only between models whose densities match in family,
  # hyperparameters and dimension. Models with different trends do
  # not match. brms writes an unnormalised density as `_lupdf` and
  # mvgam writes it as `~`. The check looks for both spellings.
  offenders <- mvgam_unnormalized_terms(samples$stancode)
  if (length(offenders) > 0L) {
    stop(insight::format_error(c(
      "The Stan model must be normalized to run bridge_sampler().",
      x = paste0(
        "Densities dropping a normalising constant: ",
        paste(offenders, collapse = ", "), "."
      ),
      i = "Refit with 'normalize = TRUE'."
    )))
  }
  # bridgesampling evaluates the density through the fit's compiled
  # rstan model. A cmdstanr fit has none, nor does one loaded with
  # `readRDS()` in a new session. As in brms, one is compiled and the draws are kept.
  fit <- samples$fit
  if (recompile || !has_rstan_model(fit)) {
    fit <- with_rstan_model(fit, samples$stancode, samples$standata)
  }
  # As in brms, a failure is reported with the two causes a user can
  # remedy
  tryCatch(
    bridgesampling::bridge_sampler(fit, ...),
    error = function(e) {
      stop(insight::format_error(c(
        "Bridge sampling failed.",
        x = conditionMessage(e),
        i = paste0("Bridge sampling needs the draws of every parameter. ",
                   "Refit with 'save_pars = save_pars(all = TRUE)'."),
        i = "A fit loaded from disk may need 'recompile = TRUE'."
      )), call. = FALSE)
    }
  )
}


#' Whether a stanfit holds a compiled rstan model
#'
#' The test of `brms:::has_rstan_model()`.
#'
#' @param fit A `stanfit`
#' @return A single logical
#' @noRd
has_rstan_model <- function(fit) {
  isTRUE(nzchar(fit@stanmodel@model_cpp$model_cppname)) &&
    length(ls(fit@.MISC)) > 0L
}


#' A stanfit with a freshly compiled rstan model, its draws kept
#'
#' As `brms:::add_rstan_model()`, the program is compiled with rstan
#' and run for no chains. Its model and environment replace those of
#' `fit`.
#'
#' @param fit A `stanfit`
#' @param stancode,standata The program and data of the model
#' @return `fit` with its `stanmodel` and `.MISC` slots replaced
#' @noRd
with_rstan_model <- function(fit, stancode, standata) {
  checkmate::assert_class(fit, "stanfit")
  checkmate::assert_string(stancode)
  checkmate::assert_list(standata)
  model <- compile_model(stancode, backend = "rstan",
                         threads = validate_threads(NULL),
                         opencl = validate_opencl(NULL))
  # rstan reports an empty run in a message
  empty <- eval_silent(rstan::sampling(model, data = standata, chains = 0),
                       type = "message")
  fit@stanmodel <- empty@stanmodel
  fit@.MISC <- empty@.MISC
  fit
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
#' The number of levels of each grouping factor of the observation
#' formula, as [brms::ngrps.brmsfit()] gives it.
#'
#' @param object A fitted `mvgam` object.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A named list of integers, one per grouping factor, or
#'   `NULL` for a model without group-level effects.
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
  rlang::check_dots_empty()
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
#' Posterior draws of the prediction error, observed minus predicted,
#' of each observation on the response scale. The arguments are those
#' of [brms::predictive_error.brmsfit()].
#'
#' @inheritParams posterior_epred.mvgam
#' @param method `"posterior_predict"` (the default) takes the errors
#'   from predictive draws, `"posterior_epred"` from expected values.
#' @param re.form The \pkg{lme4} name for `re_formula`.
#' @param sort Logical. `TRUE` orders the columns by the observed
#'   response. Defaults to `FALSE`.
#' @param ... Further arguments of the `method` function, such as
#'   `incl_autocor` or `allow_new_levels`.
#'
#' @return An `[ndraws, nobs]` matrix of observed minus predicted
#'   values.
#'
#' @details
#' The errors are those [residuals.mvgam()] returns with
#' `type = "ordinary"`. In sample, each prediction carries the latent
#' trend state the model inferred at that time. A prediction for
#' `newdata` integrates over the trend dynamics.
#'
#' @author Nicholas J Clark
#'
#' @seealso [brms::predictive_error.brmsfit()], [residuals.mvgam()],
#'   [posterior_predict.mvgam()], [posterior_epred.mvgam()].
#'
#' @family predictions
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
  validate_draw_selectors(ndraws, draw_ids)
  if (!is.null(re.form) && is.null(re_formula)) {
    re_formula <- re.form
  }
  resolve_resp(object, resp, required = TRUE,
               caller = "predictive_error()")
  data <- prediction_frame(object, newdata)
  resp_name <- response_column(object, resp)
  if (!resp_name %in% names(data)) {
    stop(insight::format_error(c(
      paste0("'newdata' lacks the response column '", resp_name, "'."),
      i = "Omit 'newdata' to use the training data."
    )))
  }
  pred_fun <- if (identical(method, "posterior_predict")) {
    posterior_predict
  } else {
    posterior_epred
  }
  # In sample, the prediction carries the inferred latent state, as
  # `residuals(type = "ordinary")` does.
  preds <- do.call(pred_fun, diagnostic_surface_args(
    list(object, newdata = newdata, re_formula = re_formula,
         resp = resp, ndraws = ndraws, draw_ids = draw_ids, ...),
    is.null(newdata)
  ))
  y <- as.numeric(data[[resp_name]])
  if (ncol(preds) != length(y)) {
    stop_mvgam_fault(
      "The predictions and the response differ in length.",
      paste0("Got ", ncol(preds), " prediction columns for ", length(y),
             " observations.")
    )
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


# Deprecated brms names. `marginal_smooths()`, `marginal_effects()`
# and `as.mcmc()` have no mvgam method. Their replacements are
# `conditional_smooths()`, `conditional_effects()` and
# `posterior::as_draws_array()`.
#
# An `mvgam` object inherits from `brmsfit`. Without a method,
# `parnames()` and `nsamples()` would take the brms method. That
# method lists every stanfit parameter and draw, including those
# `variables()` and `ndraws()` leave out. The mvgam methods return
# what `variables()` and `ndraws()` return. brms still warns that
# the names are deprecated.

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
  checkmate::assert_class(object, "mvgam")
  rlang::check_dots_empty()
  require_fitted_model(object, "nsamples")
  posterior::ndraws(object)
}


#' The frame a prediction is made for
#'
#' `newdata` when the caller gives it, with the `time` and `series`
#' columns of a `jsdgam()` fit filled in. Otherwise the training data.
#'
#' @param object A fitted `mvgam` object
#' @param newdata The caller's frame, or `NULL`
#' @return A data frame
#' @noRd
prediction_frame <- function(object, newdata = NULL) {
  if (!is.null(newdata)) {
    if (NROW(newdata) == 0L) {
      stop(insight::format_error(
        "'newdata' requires at least one row."
      ), call. = FALSE)
    }
    return(jsdgam_axis_columns(object, newdata))
  }
  out <- object$data
  if (is.null(out)) {
    stop(insight::format_error(c(
      "'newdata' is required for this fit.",
      x = "The fitted object's training-data slot is empty."
    )), call. = FALSE)
  }
  out
}


#' Sampler control settings of a fitted mvgam model
#'
#' The NUTS settings the model was sampled with, such as
#' `adapt_delta`, `max_treedepth` and `stepsize`.
#'
#' @param x A fitted `mvgam` model.
#' @param ... Unused. Anything passed here is refused.
#' @return A named list, as [brms::control_params()] returns for a
#'   `brmsfit`. A fit with no NUTS settings, such as a variational
#'   fit, gives an empty list.
#' @method control_params mvgam
#' @export
control_params.mvgam <- function(x, ...) {
  checkmate::assert_class(x, "mvgam")
  rlang::check_dots_empty()
  mvgam_sampler_inheritance(x)$control %||% list()
}

#' Initial values of a fitted mvgam model
#'
#' The `init` argument each chain started from.
#'
#' @param x A fitted `mvgam` model.
#' @param ... Unused. Anything passed here is refused.
#' @return A list with one element per chain.
#' @method inits mvgam
#' @export
inits.mvgam <- function(x, ...) {
  checkmate::assert_class(x, "mvgam")
  rlang::check_dots_empty()
  lapply(recorded_stan_args(x$fit), function(chain) chain$init)
}
