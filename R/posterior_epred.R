#' Posterior Expected Values for mvgam Models
#'
#' @description
#' Extract expected values (on response scale) from fitted mvgam models.
#' Combines observation and trend components, then applies family-appropriate
#' transformations to convert from link scale to response scale.
#'
#' @name posterior_epred
NULL


#' Compute Expected Values from Linear Predictor
#'
#' Transforms linear predictor values to expected values (response scale) using
#' family-appropriate transformations. Most families use simple inverse link,
#' but some require distributional parameters.
#'
#' @param linpred Matrix `\\[ndraws x nobs\\]` of one response's linear
#'   predictor.
#' @param family That response's family object, with `$family` (name)
#'   and `$linkinv` (function).
#' @param trials Optional vector of trial counts for binomial family where
#'   E\[Y\] = p * trials. Length 1 or ncol(linpred).
#' @param family_pars Optional named list of ``\\[ndraws x nobs\\]`` matrices
#'   holding the distributional parameters a family's mean needs beyond
#'   the predictor, as named by `epred_extra_dpars()`.
#'
#' @return Matrix `\\[ndraws x nobs\\]` of expected values on response scale.
#'
#' @noRd
compute_family_epred <- function(linpred, family, trials = NULL,
                                 family_pars = NULL) {
  checkmate::assert_matrix(linpred)

  # Validate family structure
  if (is.null(family$family) || is.null(family$linkinv)) {
    stop(insight::format_error(
      cli::format_inline(
        "{.field family} object missing required components: {.field $family} and {.field $linkinv}."
      )
    ))
  }

  family_name <- resolve_family_name(family)
  checkmate::assert_string(family_name)

  checkmate::assert_numeric(trials, lower = 0, min.len = 1, null.ok = TRUE)

  # A family named in `epred_extra_dpars()` has a mean that differs from
  # the inverse link of its predictor. Such a family needs a kernel that
  # computes its mean. The families needing extra parameters are the same
  # families needing a kernel. One registry lists both. Adding a family
  # requires one edit. `log_lik.mvgam()` dispatches to its own kernels the
  # same way.
  if (length(epred_extra_dpars(family)) > 0L &&
      !is.null(epred_kernel(family_name))) {
    return(family_mean_from_kernel(
      family_name, family$linkinv(linpred), family_pars, trials
    ))
  }

  # Dispatch based on family
  epred <- switch(
    family_name,

    # Simple families: E[Y] = g^-1(eta)
    "gaussian" = ,
    "poisson" = ,
    "bernoulli" = ,
    "beta" = ,
    "gamma" = ,
    "negbinomial" = ,
    "student" = family$linkinv(linpred),


    # Count families requiring trials: E[Y] = p * trials
    "binomial" = ,
    "beta_binomial" = {
      if (is.null(trials)) {
        stop(insight::format_error(
          cli::format_inline(
            "Family {.val {family_name}} requires {.field trials} argument for computing expected values."
          )
        ))
      }
      family$linkinv(linpred) * data2draws(trials, dim(linpred))
    },


    # `expected_from_linpred()` hands every closure-unit family to its
    # kernel before this dispatch. The branch is what a caller reaching
    # the dispatch directly meets.
    "nmix" = ,
    "nmix_royle_nichols" = ,
    "nmix_poisson_poisson" = stop_mvgam_fault(
      paste0("Family '", family_name, "' reached the unit-free mean ",
             "dispatch."),
      paste0("A closure-unit family's mean needs the unit's other ",
             "visits and the fit's 'p' draws.")
    ),

    # Tweedie (compound Poisson-gamma): E[Y | mu, phi, theta] = mu
    # for all theta in [1, 2], including the boundary cases (scaled
    # Poisson at theta = 1, Gamma at theta = 2). The point mass at
    # zero is absorbed into mu without any Jensen correction.
    "tweedie" = family$linkinv(linpred),

    # Beta negative binomial: the family is parameterised with E[Y] = mu
    # exactly. The shifted tail parameter absorbs the alpha > 1 constraint
    # that makes the mean exist.
    "beta_nb" = family$linkinv(linpred),

    # Default: try inverse link with warning for unknown families
    {
      warn_once(
        c(
          paste0("Unknown family '", family_name, "', using inverse link."),
          "i" = "Expected values may not be correct for complex families."
        ),
        paste0("epred_unknown_family_", family_name)
      )
      family$linkinv(linpred)
    }
  )

  epred
}


#' Compute Var\[Y | theta\] per draw for a family
#'
#' Companion to `compute_family_epred()` that returns per-draw conditional
#' variances of the response given the parameter draws. Used by
#' `predict.mvgam(type = "variance")` to expose the mean-variance
#' relationship of the observation family.
#'
#' @param mu Numeric matrix of E\[Y|theta\] per draw (``\\[ndraws x nobs\\]``),
#'   already on the response scale (typically the return value of
#'   `compute_family_epred()` or `posterior_epred.mvgam()`).
#' @param family A family or brmsfamily object; `family$family` selects
#'   the variance formula.
#' @param sigma Optional ``\\[ndraws x nobs\\]`` matrix of `sigma` draws.
#'   Required for `gaussian` (Var = sigma^2). Other families ignore it.
#' @param shape Optional ``\\[ndraws x nobs\\]`` matrix of `shape` draws.
#'   Required for `negbinomial` (Var = mu + mu^2/shape) and `gamma`
#'   (Var = mu^2/shape).
#' @param phi Optional ``\\[ndraws x nobs\\]`` matrix of `phi` draws.
#'   Required for `beta` (Var = mu(1-mu)/(1+phi)). Here `phi` is the brms
#'   precision parameter, equal to shape1 + shape2 of the underlying Beta.
#'   A larger `phi` gives a narrower distribution.
#' @param nu Optional ``\\[ndraws x nobs\\]`` matrix of `nu` (degrees of
#'   freedom) draws. Required for `student`
#'   (Var = sigma^2 * nu / (nu - 2) when nu > 2; `Inf` otherwise).
#' @param trials Optional numeric vector (length `ncol(mu)` or 1) of trial
#'   counts for `binomial` (Var = trials * p * (1-p) where p = mu/trials).
#'
#' @return Numeric matrix ``\\[ndraws x nobs\\]`` of conditional variances
#'   on the response scale.
#'
#' @noRd
compute_family_variance <- function(mu, family, sigma = NULL,
                                    shape = NULL, phi = NULL,
                                    nu = NULL, trials = NULL) {
  # Multivariate dispatch: recurse per response.
  if (is.list(mu) && !is.matrix(mu)) {
    checkmate::assert_list(family, names = "named")
    result <- lapply(names(mu), function(resp_name) {
      compute_family_variance(
        mu = mu[[resp_name]],
        family = family[[resp_name]],
        sigma = if (is.list(sigma)) sigma[[resp_name]] else sigma,
        shape = if (is.list(shape)) shape[[resp_name]] else shape,
        phi = if (is.list(phi)) phi[[resp_name]] else phi,
        nu = if (is.list(nu)) nu[[resp_name]] else nu,
        trials = trials
      )
    })
    names(result) <- names(mu)
    return(result)
  }

  checkmate::assert_matrix(mu)
  if (is.null(family$family)) {
    stop(insight::format_error(
      "'family' object is missing the 'family' component."
    ))
  }
  family_name <- resolve_family_name(family)

  require_dpar <- function(value, dpar_name) {
    if (is.null(value)) {
      stop_mvgam_fault(
        paste0("Family '", family_name, "' needs '", dpar_name,
               "' for its variance."),
        "The caller passed no draws for it."
      )
    }
    checkmate::assert_matrix(value)
    if (nrow(value) != nrow(mu) || ncol(value) != ncol(mu)) {
      stop_shape_fault(paste0("Parameter '", dpar_name, "'"),
                       dim(value), dim(mu))
    }
    value
  }

  switch(
    family_name,
    "gaussian" = {
      s <- require_dpar(sigma, "sigma")
      s^2
    },
    "poisson" = mu,
    "bernoulli" = mu * (1 - mu),
    "binomial" = {
      if (is.null(trials)) {
        stop(insight::format_error(
          "Family 'binomial' requires 'trials' for variance computation."
        ))
      }
      checkmate::assert_numeric(trials, lower = 0)
      if (any(trials == 0)) {
        warn_once(
          c(
            "'trials' contains zeros.",
            i = "Variance is NaN for binomial observations with 0 trials."
          ),
          "binomial_variance_zero_trials"
        )
      }
      trials_mat <- data2draws(trials, dim(mu))
      p <- mu / trials_mat
      p * (1 - p) * trials_mat
    },
    "negbinomial" = {
      k <- require_dpar(shape, "shape")
      mu + mu^2 / k
    },
    "gamma" = {
      k <- require_dpar(shape, "shape")
      mu^2 / k
    },
    "beta" = {
      p <- require_dpar(phi, "phi")
      mu * (1 - mu) / (1 + p)
    },
    "student" = {
      s <- require_dpar(sigma, "sigma")
      df <- require_dpar(nu, "nu")
      # Var[Y] = sigma^2 * nu/(nu-2) for nu > 2; undefined otherwise.
      # Return Inf for the undefined region. Downstream summaries then
      # propagate non-finiteness. The value is not converted to NA.
      v <- s^2 * df / (df - 2)
      v[df <= 2] <- Inf
      v
    },
    "lognormal" = {
      # brms parameterisation: mu is meanlog, sigma is sdlog. mu_full
      # arriving here is the response-scale mean exp(meanlog + sdlog^2/2)
      # (compute_family_epred handles the Jensen correction). So
      # Var[Y] = E[Y]^2 * (exp(sdlog^2) - 1).
      s <- require_dpar(sigma, "sigma")
      mu^2 * (exp(s^2) - 1)
    },
    stop(insight::format_error(c(
      paste0(
        "type = \"variance\" is not implemented for family '", family_name, "'."
      ),
      x = paste0(
        "Supported families: gaussian, student, lognormal, poisson, ",
        "bernoulli, binomial, negbinomial, gamma, beta."
      ),
      i = paste0(
        "For unsupported families, draw with type = \"response\" and ",
        "compute the variance empirically."
      )
    )))
  )
}


#' Extract Posterior Expected Values from mvgam Models
#'
#' @description
#' Extract expected values (on response scale) from fitted mvgam models.
#' Combines observation and trend components, then applies the inverse link
#' function to transform from link scale to response scale.
#'
#' @param object A fitted mvgam object from [mvgam()].
#' @param newdata Optional data frame with covariates for prediction. If
#'   NULL, uses original training data stored in the model object.
#' @param process_error Logical; if `TRUE`, the trend adds a sampled
#'   innovation to its deterministic submodel. Each posterior draw
#'   takes one innovation from the distribution the trend settles
#'   into. An autoregressive trend settles into its stationary
#'   distribution: an AR(1) draws at \eqn{\sigma^2/(1-\rho^2)}. The
#'   Jensen correction on a log link is
#'   \eqn{\sigma^2/(2(1-\rho^2))}. A random walk lacks a stationary
#'   distribution and a `ZMVN()` trend lacks dynamics. Both draw their
#'   innovations directly. `CAR()` uses one stationary covariance at
#'   every time of an irregular grid and draws from it. If `FALSE`,
#'   the default, the trend contributes its deterministic submodel
#'   alone, at its own per-draw values. The argument applies under
#'   `incl_autocor = FALSE`. Conditioning on the fitted state already
#'   fixes every innovation.
#'
#'   Each call draws new innovations. Two calls on one fit give different
#'   results. Set a seed to reproduce a result.
#' @param incl_autocor Logical; which prediction surface the trend
#'   contribution comes from. `FALSE`, the default, omits the fitted
#'   `trend[t, s]` and uses the two submodels' covariate structure. The
#'   effect is the same at the first time point as at the fiftieth. `TRUE`
#'   uses the latent state the model inferred at each time. That is the
#'   quantity [hindcast()] returns. The ELPD is computed from it.
#'   [loo_R2.mvgam()] and [bayes_R2.mvgam()] request this surface. An R^2
#'   describes the observed series. A row whose time falls outside the
#'   fitted grid is refused under `TRUE`. [forecast.mvgam()] predicts
#'   those times.
#'
#'   The default suits a model whose covariates explain most of the
#'   signal. When most of the series-level variation lies in the trend,
#'   as in a fit with few covariates and a strong autoregressive process,
#'   this surface has little to say. [hindcast.mvgam()] and
#'   [forecast.mvgam()] are more informative there.
#' @param ndraws Positive integer specifying number of posterior draws to
#'   use. NULL (default) uses all available draws.
#' @param draw_ids Optional integer vector selecting a subset of posterior
#'   draw indices to use. NULL (default) uses all draws (subject to
#'   `ndraws`).
#' @param re_formula Group-level terms to include: `NULL` (the default)
#'   for every one, `NA` for none. A formula choosing some of them is not
#'   supported.
#' @param allow_new_levels Logical; accepted for brms compatibility.
#'   A grouping level the model never saw is refused whatever this is
#'   set to. A new level has no fitted random effect. For a new series,
#'   it has no latent state to propagate. Predict for levels the fit
#'   knows, or refit with the new levels included.
#' @param sample_new_levels Character; accepted for brms
#'   compatibility and not used, since new levels are refused. See
#'   `allow_new_levels`.
#' @param resp Character specifying which response variable for
#'   multivariate models. NULL (default) returns predictions for all
#'   responses.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return Matrix with dimensions `\\[ndraws x nobs\\]` containing expected
#'   values on response scale. Each row is one posterior draw, each column
#'   is one observation from newdata.
#'
#'   For multivariate models with resp = NULL, returns a named list of
#'   matrices (one per response variable).
#'
#' @details
#' Expected values are computed as E\[Y|X\] by applying the inverse link
#' function to the linear predictor. For most families this is simply
#' `linkinv(eta)`:
#' \itemize{
#'   \item Gaussian, Poisson, Bernoulli, Beta, Gamma, NegBinomial, Student:
#'     E\[Y\] = linkinv(eta)
#' }
#'
#' Some families require additional parameters:
#' \itemize{
#'   \item Binomial, Beta-binomial: E\[Y\] = p * trials (requires trials)
#'   \item Lognormal: E\[Y\] = exp(mu + sigma^2/2) (requires sigma)
#' }
#'
#' Unsupported families:
#' \itemize{
#'   \item N-mixture, Tweedie: require specialized computation
#' }
#'
#' @seealso [posterior_linpred.mvgam()] for link scale predictions,
#'   [posterior_predict.mvgam()] for posterior predictive samples,
#'   \[forecast.mvgam\] and \[hindcast.mvgam\] for the deterministic
#'   state-extrapolating prediction surface. That surface takes the
#'   state from the fitted `lv_trend` posterior draws, with no Monte
#'   Carlo integration.
#'
#' @examples
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR())
#' mod <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1),
#'               data    = simdat$data_train,
#'               family  = poisson(),
#'               chains  = 2, silent = 2)
#'
#' # Posterior mean of the response, marginalised over the trend's
#' # stochastic dynamics. See [forecast.mvgam()] for the
#' # state-extrapolating alternative.
#' ep <- posterior_epred(mod, ndraws = 50L)
#' dim(ep)
#'
#' # Summarise across draws to get the posterior mean and 90% CI
#' # for each cell, then compare against the observed series.
#' ep_summary <- t(apply(
#'   ep, 2L, quantile, probs = c(0.05, 0.5, 0.95)
#' ))
#' head(ep_summary)
#' }
#'
#' @importFrom brms posterior_epred
#' @family predictions
#' @method posterior_epred mvgam
#' @export
posterior_epred.mvgam <- function(object, newdata = NULL,
                                  process_error = FALSE,
                                  incl_autocor = FALSE,
                                  ndraws = NULL,
                                  draw_ids = NULL,
                                  re_formula = NULL,
                                  allow_new_levels = FALSE,
                                  sample_new_levels = "uncertainty",
                                  resp = NULL,
                                  ...) {
  # Validate mvgam-specific parameters
  checkmate::assert_class(object, "mvgam")
  require_fitted_model(object, "posterior_epred")
  checkmate::assert_logical(process_error, len = 1)
  trend_state <- autocor_to_trend_state(incl_autocor)
  validate_draw_selectors(ndraws, draw_ids)
  rlang::check_dots_empty()

  newdata <- prediction_frame(object, newdata)

  # The linear predictor and the distributional parameters are drawn by
  # separate extractions. Each one subsamples on its own. A requested count
  # is converted to concrete indices before either runs. Without that
  # conversion, the two subsamples come from unrelated iterations.
  draw_ids <- resolve_draw_ids(object, ndraws, draw_ids)
  if (!is.null(draw_ids)) {
    ndraws <- NULL
  }

  # Get linear predictor (handles obs+trend combination)
  linpred <- get_combined_linpred(
    mvgam_fit = object,
    newdata = newdata,
    process_error = process_error,
    trend_state = trend_state,
    draw_ids = draw_ids,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels,
    resp = resp
  )

  # The marginal expectation integrates over the trend's dynamics. Sampled
  # innovations are carried on the link scale before the inverse link is
  # applied. This matches the analytic correction brms makes for an AR-cov
  # model (sigma^2/(1-ar^2)/2 for a Poisson log link). It works the same
  # way across families, links and trend types. `get_combined_linpred()`
  # samples the innovations once. A second set drawn here would double
  # the process variance in every marginal prediction.

  # A model with several responses returns a result for each response when
  # `resp` names none, as brms does.
  if (is.list(linpred) && !is.matrix(linpred)) {
    return(lapply(stats::setNames(nm = names(linpred)), function(r) {
      response_epred(object, linpred[[r]], newdata, draw_ids, resp = r,
                     re_formula = re_formula,
                     allow_new_levels = allow_new_levels,
                     sample_new_levels = sample_new_levels)
    }))
  }
  response_epred(object, linpred, newdata, draw_ids, resp = resp,
                 re_formula = re_formula,
                 allow_new_levels = allow_new_levels,
                 sample_new_levels = sample_new_levels)
}


#' One response's expectation as `posterior_epred()` reports it
#'
#' An ordinal response has no mean brms reports: its expectation is
#' the probability of each category, which is what brms's own
#' `posterior_epred()` returns. Every other family reports its mean.
#'
#' @param object A fitted `mvgam` object.
#' @param linpred `[ndraws x nobs]` link-scale predictor.
#' @param newdata Data the predictor was built for.
#' @param draw_ids Draws the predictor was taken at, or `NULL` for all.
#' @param resp The response's key, or `NULL`.
#' @return A matrix, or an `[ndraws x nobs x ncat]` array for an
#'   ordinal response.
#' @noRd
response_epred <- function(object, linpred, newdata, draw_ids, resp,
                           re_formula = NULL,
                           allow_new_levels = FALSE,
                           sample_new_levels = "uncertainty") {
  family <- model_families(object, resp)
  if (is_ordinal_family(family)) {
    return(ordinal_category_probs(
      object, linpred, family, draw_ids = draw_ids, newdata = newdata,
      resp = resp
    ))
  }
  expected_from_linpred(
    object, linpred, newdata = newdata, draw_ids = draw_ids, resp = resp,
    re_formula = re_formula, allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels
  )
}


#' The family's mean for a linear predictor already on the link scale
#'
#' The inverse link gives this mean only for a family whose expectation
#' is its predictor. A binomial mean is `trials * p`. A zero-inflated
#' mean is `(1 - zi) * mu`. A surface that applies the inverse link alone
#' reports a probability where a count was asked for, or a rate before
#' its mixing probability.
#'
#' Trials come from `extract_trials_for_family()`. Families that need
#' more than `(linpred, trials)` are named by `epred_extra_dpars()`. A
#' mixture at zero needs its mixing probability. A lognormal needs the
#' dispersion its Jensen correction uses. A Conway-Maxwell-binomial has
#' no closed form in `mu` alone. The extra draws are resolved against the
#' same iterations the predictor came from.
#'
#' A closure-unit family's mean depends on sibling rows. An occupancy is
#' thinned by the visit's detection probability. A composition normalises
#' across the K rows of its site. Neither is a function of one row's
#' predictor. Each has a dedicated kernel. This function passes the
#' predictor to that kernel. The inverse link alone would give the raw
#' predictor for a composition and the occupancy without its
#' detection.
#'
#' An ordinal response's mean is its expected ordered level, on the
#' scale that `posterior_predict()` draws from. `posterior_epred()`
#' reports the category probabilities through `response_epred()`.
#'
#' Each response uses its own family, trials and parameters. A response
#' of a model with several responses stores its parameters under its own
#' key, `hu_count` for `hu`. Looking them up without that scope returns
#' none.
#'
#' @param object A fitted `mvgam` object.
#' @param linpred `[ndraws x nobs]` link-scale predictor, or a named
#'   list of them for a multivariate fit.
#' @param newdata Data the predictor was built for. The function looks up
#'   trials and any per-observation parameter in it.
#' @param draw_ids Draws the predictor was taken at. The extra parameters
#'   are taken at the same iterations.
#' @param resp The response's key, or `NULL`.
#' @return Matrix of expected values, or a named list of them.
#' @noRd
expected_from_linpred <- function(object, linpred, newdata = NULL,
                                  draw_ids = NULL, resp = NULL,
                                  re_formula = NULL,
                                  allow_new_levels = FALSE,
                                  sample_new_levels = "uncertainty") {
  if (is.list(linpred) && !is.matrix(linpred)) {
    return(lapply(stats::setNames(nm = names(linpred)), function(r) {
      expected_from_linpred(
        object, linpred[[r]], newdata = newdata, draw_ids = draw_ids,
        resp = r, re_formula = re_formula,
        allow_new_levels = allow_new_levels,
        sample_new_levels = sample_new_levels
      )
    }))
  }
  family <- model_families(object, resp)
  if (uses_closure_unit_layout(family)) {
    epred_fn <- dispatch_closure_unit_method(family, "epred")
    return(epred_fn(
      object, newdata = newdata, draw_ids = draw_ids,
      linpred = linpred
    ))
  }
  if (is_ordinal_family(family)) {
    return(ordinal_category_mean(ordinal_category_probs(
      object, linpred, family, draw_ids = draw_ids, newdata = newdata,
      resp = resp
    )))
  }
  compute_family_epred(
    linpred     = linpred,
    family      = family,
    trials      = extract_trials_for_family(object, family, newdata),
    family_pars = resolve_family_pars(
      object,
      dpar_names = epred_extra_dpars(family),
      ndraws = nrow(linpred),
      nobs = ncol(linpred),
      draw_ids = draw_ids,
      newdata = newdata,
      resp = resp,
      re_formula = re_formula,
      allow_new_levels = allow_new_levels,
      sample_new_levels = sample_new_levels
    )
  )
}


#' Distributional parameters a family's mean needs beyond the predictor
#'
#' Most families have `E[Y]` equal to the inverse link of the linear
#' predictor. For them, the predictor is all `compute_family_epred()`
#' needs. The families named here need additional parameters. A mixture
#' at zero, a Jensen correction or a sum over the support puts another
#' parameter in the mean. Naming them in one place lets `posterior_epred()`
#' resolve those draws before it dispatches. Otherwise each family would
#' find, too late, that it had received only the predictor.
#'
#' @param family A family or brmsfamily object
#' @return Character vector of `dpar` names, empty when the mean needs
#'   nothing beyond the predictor
#'
#' @noRd
epred_extra_dpars <- function(family) {
  if (is.null(family) || is.null(family$family)) {
    return(character(0))
  }
  epred_extra_dpars_for(resolve_family_name(family))
}


#' @rdname epred_extra_dpars
#' @noRd
epred_extra_dpars_for <- function(family_name) {
  switch(
    family_name,
    "lognormal" = "sigma",
    "hurdle_poisson" = ,
    "hurdle_gamma" = "hu",
    "hurdle_negbinomial" = c("hu", "shape"),
    "hurdle_lognormal" = c("hu", "sigma"),
    "zero_inflated_poisson" = ,
    "zero_inflated_negbinomial" = ,
    "zero_inflated_beta" = ,
    "zero_inflated_binomial" = ,
    "zero_inflated_beta_binomial" = "zi",
    "zero_one_inflated_beta" = c("zoi", "coi"),
    "shifted_lognormal" = c("sigma", "ndt"),
    "gen_extreme_value" = c("sigma", "xi"),
    "com_binomial" = "nu",
    "asym_laplace" = c("sigma", "quantile"),
    "zero_inflated_asym_laplace" = c("zi", "sigma", "quantile"),
    "com_poisson" = ,
    "discrete_weibull" = "shape",
    "wiener" = c("bs", "ndt", "bias"),
    character(0)
  )
}


#' The mean kernel a family's `E[Y]` is defined by
#'
#' Each family's mean is defined in one `posterior_epred_<family>()`
#' function copied from brms. `log_lik.mvgam()` looks up that function by
#' name to reach its own per-family densities. The mean is then written
#' once, in one kernel, with no second copy in a dispatch branch.
#'
#' @param family_name Resolved family name
#' @return The kernel, or `NULL` when the family has none
#'
#' @noRd
epred_kernel <- function(family_name) {
  # The existence check returns `NULL` for a family with no kernel. A
  # kernel that exists but fails to load still raises.
  nm <- paste0("posterior_epred_", family_name)
  if (!exists(nm, mode = "function", envir = asNamespace("mvgam"))) {
    return(NULL)
  }
  get(nm, mode = "function", envir = asNamespace("mvgam"))
}


#' Mean of a family whose mean differs from its inverse link
#'
#' Applies the per-family mean that its brms counterpart uses. The two
#' agree on what `posterior_epred()` returns. Each mean has one
#' definition. A kernel and a dispatch branch cannot drift apart.
#' `mu` here is the base distribution's parameter, already on the
#' response scale.
#'
#' @param family_name Resolved family name
#' @param mu Numeric matrix ``\\[ndraws x nobs\\]`` of the base
#'   distribution's parameter
#' @param family_pars Named list of `dpar` draw matrices, as
#'   `epred_extra_dpars()` asked for
#' @param trials Trial counts, for the zero-inflated binomial
#' @return Numeric matrix ``\\[ndraws x nobs\\]`` of `E[Y]`
#'
#' @noRd
family_mean_from_kernel <- function(family_name, mu, family_pars,
                                    trials = NULL) {
  kernel <- epred_kernel(family_name)
  if (is.null(kernel)) {
    stop(insight::format_error(
      paste0("No mean kernel is registered for family '",
             family_name, "'.")
    ))
  }

  needed <- epred_extra_dpars_for(family_name)
  missing <- setdiff(needed, names(family_pars))
  if (length(missing) > 0L) {
    stop(insight::format_error(c(
      paste0("Family '", family_name,
             "' needs more than its linear predictor for E[Y]."),
      x = paste0("Missing: ", paste(shQuote(missing), collapse = ", "), "."),
      i = paste0("Pass them as 'family_pars', the mixing probability, ",
                 "shift or dispersion that separates E[Y] from 'mu'.")
    )), call. = FALSE)
  }

  for (dpar in needed) {
    value <- family_pars[[dpar]]
    checkmate::assert_matrix(value)
    if (!identical(dim(value), dim(mu))) {
      stop_shape_fault(paste0("Parameter '", dpar, "'"), dim(value),
                       dim(mu))
    }
  }

  prep <- list(
    dpars = c(list(mu = mu), family_pars),
    ndraws = nrow(mu),
    nobs = ncol(mu),
    data = list(trials = trials)
  )
  kernel(prep)
}


#' Extract trials from model object or newdata for binomial families
#'
#' @param object mvgam model object
#' @param family One response's family
#' @param newdata Data frame for predictions (NULL uses training data)
#' @return Numeric vector of trials or NULL for non-binomial families
#'
#' @details
#' Trials are extracted with the following precedence:
#' \enumerate{
#'   \item For training data predictions (newdata = NULL):
#'         object$standata$trials or object$data$trials
#'   \item For new data predictions: newdata$trials
#' }
#'
#' @noRd
extract_trials_for_family <- function(object, family, newdata) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_multi_class(family, c("family", "brmsfamily"))
  checkmate::assert_data_frame(newdata, null.ok = TRUE)

  # `resolve_family_name()` looks up a custom family's own name. brms
  # records that name as "custom". `com_binomial()` also takes trials.
  family_name <- resolve_family_name(family)

  binomial_families <- c("binomial", "beta_binomial",
                         "zero_inflated_binomial",
                         "zero_inflated_beta_binomial",
                         "com_binomial")

  if (!family_name %in% binomial_families) {
    return(NULL)
  }

  # Predictions cover every row of the target data, including rows whose
  # response was missing. The denominator is resolved against that data.
  # It is not reused from `standata`, which is sized to the likelihood's
  # observed rows.
  pred_data <- prediction_frame(object, newdata)
  trials <- resolve_trials_denominator(object$formula, pred_data) %||%
    pred_data$trials

  # `standata` covers only the data the model was fitted to. Reusing it
  # for a grid the caller supplied would give each row another
  # observation's denominator. The error goes unnoticed whenever the two
  # carry the same number of rows.
  if (is.null(trials) && is.null(newdata)) {
    trials <- object$standata$trials
  }

  if (is.null(trials)) {
    wanted <- all.vars(find_aterm_call(object$formula, "trials") %||%
                         list())
    wanted <- setdiff(wanted, names(pred_data))
    stop(insight::format_error(c(
      cli::format_inline(
        "Family {.val {family_name}} requires {.field trials} data."
      ),
      x = if (length(wanted)) {
        cli::format_inline(
          "{.code newdata} has no column {.field {wanted}}."
        )
      } else {
        cli::format_inline("No {.field trials} column was found.")
      },
      i = cli::format_inline(
        "Supply the denominator for every row being predicted, either as the column named in {.code y | trials(n) ~ ...} or as a {.code trials} column."
      )
    )))
  }

  # A denominator of zero is a legal binomial observation. No trials were
  # run. The response is necessarily zero. The expected value is zero too.
  # brms accepts a zero denominator, since `data_response()` rejects only
  # negatives. Zero is also the natural padding for a cell the likelihood
  # never saw. The bound here matches that padding.
  checkmate::assert_numeric(trials, lower = 0, finite = TRUE,
                            any.missing = FALSE, min.len = 1,
                            .var.name = "trials")

  trials
}


# ============================================================================
# Family-Specific Expected Value Functions
# ============================================================================
# These functions compute expected values (E[Y]) for different observation
# families. Adapted from brms package (Buerkner, 2017) with modifications
# for mvgam's State-Space model structure.
#
# References:
#   Buerkner, P. C. (2017). brms: An R Package for Bayesian Multilevel Models
#   Using Stan. Journal of Statistical Software, 80(1), 1-28.
#   doi:10.18637/jss.v080.i01
#
# Original author: Paul-Christian Buerkner (brms package)
# Adapted by: mvgam authors
# ============================================================================

# --- Helper functions ---

#' Expand Data to Posterior Draw Dimensions
#'
#' Converts vector or matrix data to dimensions that match the posterior
#' draws, for vectorized multiplication. Broadcasts observation-level data
#' (trials, rate denominators) across all posterior draws.
#'
#' @param x Numeric vector or matrix to expand
#' @param dim Integer vector specifying target dimensions:
#'   \itemize{
#'     \item Length 2: c(ndraws, nobs) for univariate models
#'     \item Length 3: c(ndraws, nobs, ncats) for categorical models
#'   }
#'
#' @return Matrix or array with data expanded to match posterior draws.
#'   For 2D: each row (draw) contains the same observation values.
#'   For 3D: observation data replicated across draws dimension.
#'
#' @noRd
data2draws <- function(x, dim) {
  checkmate::assert_numeric(x)
  checkmate::assert_integerish(dim, lower = 1, min.len = 2, max.len = 3,
                               any.missing = FALSE)

  if (length(dim) == 2) {
    # Expand vector to matrix: each draw (row) gets same data values
    # dim[1] = ndraws, dim[2] = nobs
    if (!length(x) %in% c(1, dim[2])) {
      stop_shape_fault("Observation data", length(x), dim[2])
    }
    matrix(x, nrow = dim[1], ncol = dim[2], byrow = TRUE)
  } else {
    # Expand to 3D array for categorical/compositional families
    # dim[1] = ndraws, dim[2:3] = observation dimensions
    if (!(length(x) == 1 || identical(dim(x), as.integer(dim[2:3])))) {
      stop_shape_fault("A distributional parameter", dim(x), dim[2:3])
    }
    aperm(array(x, dim = c(dim[2:3], dim[1])), perm = c(3, 1, 2))
  }
}

#' Get Expected Dimensions of mu Parameter
#'
#' Returns the expected dimensions c(ndraws, nobs) for the main distributional
#' parameter mu based on the prediction preparation object.
#'
#' @param prep List containing prediction preparation with elements:
#'   \itemize{
#'     \item ndraws: Number of posterior draws
#'     \item nobs: Number of observations
#'   }
#'
#' @return Integer vector c(ndraws, nobs)
#'
#' @noRd
dim_mu <- function(prep) {
  checkmate::assert_list(prep)
  checkmate::assert_int(prep$ndraws, lower = 1)
  checkmate::assert_int(prep$nobs, lower = 1)
  c(prep$ndraws, prep$nobs)
}

#' Multiply Distributional Parameter by Rate Denominator
#'
#' Adjusts expected values for count models with exposure or offset terms.
#' When a rate denominator is present (e.g., exposure time, area), the
#' expected count is mu * rate_denom.
#'
#' @param dpar Matrix of distributional parameter values (mu on response scale)
#' @param prep List containing prediction preparation with optional element:
#'   \itemize{
#'     \item data$rate_denom: Vector of rate denominators (length 1 or nobs)
#'   }
#'
#' @return Matrix of adjusted expected values. If no rate_denom present,
#'   returns dpar unchanged.
#'
#' @noRd
multiply_dpar_rate_denom <- function(dpar, prep) {
  checkmate::assert_matrix(dpar)
  checkmate::assert_list(prep)

  if (is.null(prep$data$rate_denom)) {
    return(dpar)
  }

  rate_denom <- prep$data$rate_denom
  checkmate::assert_numeric(rate_denom, min.len = 1)

  # Expand rate_denom to match dpar dimensions
  rate_denom_mat <- data2draws(rate_denom, dim(dpar))
  dpar * rate_denom_mat
}

# --- Simple families: E[Y] = mu ---

#' @noRd
posterior_epred_gaussian <- function(prep) prep$dpars$mu

#' @noRd
posterior_epred_student <- function(prep) prep$dpars$mu

#' @noRd
posterior_epred_skew_normal <- function(prep) prep$dpars$mu

#' @noRd
posterior_epred_exponential <- function(prep) prep$dpars$mu

#' @noRd
posterior_epred_gamma <- function(prep) prep$dpars$mu

#' @noRd
posterior_epred_weibull <- function(prep) prep$dpars$mu

#' @noRd
posterior_epred_frechet <- function(prep) prep$dpars$mu

#' @noRd
posterior_epred_inverse.gaussian <- function(prep) prep$dpars$mu

#' @noRd
posterior_epred_exgaussian <- function(prep) prep$dpars$mu

#' @noRd
posterior_epred_beta <- function(prep) prep$dpars$mu

#' @noRd
posterior_epred_von_mises <- function(prep) prep$dpars$mu

#' @noRd
posterior_epred_bernoulli <- function(prep) prep$dpars$mu

# --- Count families with rate denominator support ---

#' @noRd
posterior_epred_poisson <- function(prep) {
  multiply_dpar_rate_denom(prep$dpars$mu, prep)
}

#' @noRd
posterior_epred_negbinomial <- function(prep) {
  multiply_dpar_rate_denom(prep$dpars$mu, prep)
}

#' @noRd
posterior_epred_negbinomial2 <- function(prep) {
  multiply_dpar_rate_denom(prep$dpars$mu, prep)
}

#' @noRd
posterior_epred_geometric <- function(prep) {
  multiply_dpar_rate_denom(prep$dpars$mu, prep)
}

# --- Families requiring distributional parameters ---

#' @noRd
posterior_epred_lognormal <- function(prep) {
  with(prep$dpars, exp(mu + sigma^2 / 2))
}

#' @noRd
posterior_epred_shifted_lognormal <- function(prep) {
  with(prep$dpars, exp(mu + sigma^2 / 2) + ndt)
}

#' @noRd
posterior_epred_binomial <- function(prep) {
  trials <- data2draws(prep$data$trials, dim_mu(prep))
  prep$dpars$mu * trials
}

#' The beta-binomial mean is the binomial mean. Its dispersion leaves
#' `mu` unchanged.
#' @noRd
posterior_epred_beta_binomial <- posterior_epred_binomial

#' @noRd
posterior_epred_gen_extreme_value <- function(prep) {
  with(prep$dpars, mu + sigma * (gamma(1 - xi) - 1) / xi)
}

#' @noRd
posterior_epred_asym_laplace <- function(prep) {
  with(prep$dpars,
       mu + sigma * (1 - 2 * quantile) / (quantile * (1 - quantile)))
}

#' Wiener diffusion expected value from Wabersich & Vandekerckhove (2014)
#' doi:10.1016/j.jmp.2009.01.006
#' @noRd
posterior_epred_wiener <- function(prep) {
  with(prep$dpars,
       ndt - bias / mu + bs / mu *
         (exp(-2 * mu * bias) - 1) / (exp(-2 * mu * bs) - 1))
}

# --- Zero-inflated families ---

#' @noRd
posterior_epred_zero_inflated_poisson <- function(prep) {
  with(prep$dpars, mu * (1 - zi))
}

#' @noRd
posterior_epred_zero_inflated_negbinomial <-
  posterior_epred_zero_inflated_poisson

#' @noRd
posterior_epred_zero_inflated_binomial <- function(prep) {
  trials <- data2draws(prep$data$trials, dim_mu(prep))
  prep$dpars$mu * trials * (1 - prep$dpars$zi)
}

#' The zero-inflated beta-binomial mean is the zero-inflated binomial
#' mean. Its dispersion leaves `mu` unchanged.
#' @noRd
posterior_epred_zero_inflated_beta_binomial <-
  posterior_epred_zero_inflated_binomial

#' @noRd
posterior_epred_zero_inflated_beta <- posterior_epred_zero_inflated_poisson

#' @noRd
posterior_epred_zero_one_inflated_beta <- function(prep) {
  with(prep$dpars, zoi * coi + mu * (1 - zoi))
}

#' @noRd
posterior_epred_zero_inflated_asym_laplace <- function(prep) {
  posterior_epred_asym_laplace(prep) * (1 - prep$dpars$zi)
}

# --- Hurdle families ---

#' @noRd
posterior_epred_hurdle_poisson <- function(prep) {
  with(prep$dpars, mu / (1 - exp(-mu)) * (1 - hu))
}

#' @noRd
posterior_epred_hurdle_negbinomial <- function(prep) {
  with(prep$dpars, mu / (1 - (shape / (mu + shape))^shape) * (1 - hu))
}

#' @noRd
posterior_epred_hurdle_gamma <- function(prep) {
  with(prep$dpars, mu * (1 - hu))
}

#' @noRd
posterior_epred_hurdle_lognormal <- function(prep) {
  with(prep$dpars, exp(mu + sigma^2 / 2) * (1 - hu))
}

# --- Distribution mean helper functions ---

#' Mean of discrete Weibull distribution
#'
#' Computes E\[Y\] for discrete Weibull via truncated series summation.
#' Based on brms implementation by Paul-Christian Buerkner.
#'
#' @param mu Location parameter in unit interval
#' @param shape Positive shape parameter
#' @param M Maximum series terms to evaluate
#' @param thres Convergence threshold
#' @return Matrix of expected values matching input dimensions
#' @noRd
mean_discrete_weibull <- function(mu, shape, M = 1000, thres = 0.001) {
  opt_M <- ceiling(max((log(thres) / log(mu))^(1 / shape)))
  if (opt_M <= M) {
    M <- opt_M
  } else {
    warn_once(
      c("Approximating the mean of discrete_weibull may be inaccurate.",
        "i" = "Series did not converge within M terms."),
      "mean_discrete_weibull_convergence"
    )
  }
  out <- 0
  for (y in seq_len(M)) {
    out <- out + mu^y^shape
  }
  out
}

#' Mean of COM-Poisson distribution
#'
#' Computes E\[Y\] for Conway-Maxwell-Poisson via series approximation.
#' Based on brms implementation by Paul-Christian Buerkner.
#' Uses closed-form approximation when mu^shape >= 1.5 and mu >= 1.5.
#'
#' @param mu Location parameter (mode parameterization)
#' @param shape Positive shape parameter
#' @param M Maximum series terms to evaluate
#' @param thres Convergence threshold
#' @param approx Use closed-form approximation when appropriate
#' @return Matrix of expected values matching input dimensions
#' @noRd
mean_com_poisson <- function(mu, shape, M = 10000, thres = 1e-16,
                             approx = TRUE) {
  if (isTRUE(any(shape <= 0))) {
    stop(insight::format_error("shape must be positive."))
  }
  if (isTRUE(any(shape == Inf))) {
    stop(insight::format_error("shape must be finite."))
  }


  out <- rep(NA, length(mu))
  dim(out) <- dim(mu)

  # shape == 1 implies Poisson distribution
  use_poisson <- shape == 1
  if (any(use_poisson)) {
    out[use_poisson] <- mu[use_poisson]
  }

  if (approx) {
    # Closed-form approximation (doi:10.1007/s10463-017-0629-6)
    use_approx <- mu^shape >= 1.5 & mu >= 1.5 & is.na(out)
    if (any(use_approx)) {
      mu_a <- mu[use_approx]
      shape_a <- shape[use_approx]
      term <- 1 - (shape_a - 1) / (2 * shape_a) * mu_a^(-1) -
        (shape_a^2 - 1) / (24 * shape_a^2) * mu_a^(-2) -
        (shape_a^2 - 1) / (24 * shape_a^3) * mu_a^(-3)
      out[use_approx] <- mu_a * term
    }
  }

  # Direct series computation for remaining cases
  use_exact <- is.na(out)
  if (any(use_exact)) {
    series <- com_poisson_log_series(
      log(mu[use_exact]), shape[use_exact], M = M, thres = thres
    )
    if (!series$converged) {
      warn_once(
        c("Approximating the mean of com_poisson may be inaccurate.",
          "i" = "Series did not converge within M terms."),
        "mean_com_poisson_convergence"
      )
    }
    out[use_exact] <- exp(series$log_moment - series$log_z)
  }
  out
}

# --- Families requiring helper functions ---

#' @noRd
posterior_epred_discrete_weibull <- function(prep) {
  mean_discrete_weibull(prep$dpars$mu, prep$dpars$shape)
}

#' @noRd
posterior_epred_com_poisson <- function(prep) {
  mean_com_poisson(prep$dpars$mu, prep$dpars$shape)
}

# ============================================================================
# Ordinal Family Support
# ============================================================================
# The predictive distribution of the ordinal families: cumulative,
# sratio, cratio, acat and hurdle_cumulative.
#
# Adapted from brms package (Buerkner, 2017) with modifications for mvgam.
# Original author: Paul-Christian Buerkner (brms package)
# ============================================================================

#' Whether a family models an ordered categorical response
#'
#' brms marks each of its ordinal families with the special
#' `"ordinal"`: cumulative, sratio, cratio, acat and
#' hurdle_cumulative. Checking the mark includes the hurdle variant.
#'
#' @param family A family object, or `NULL`
#' @return A single logical
#' @noRd
is_ordinal_family <- function(family) {
  checkmate::assert_multi_class(
    family, c("family", "brmsfamily"), null.ok = TRUE
  )
  "ordinal" %in% family$specials
}


#' Expectation of a function of the ordered level under an ordinal epred
#'
#' An ordinal epred holds a probability for each category. A summary on
#' the response scale is an expectation over the ordered levels. Each
#' category's level is its name on the third margin. That name is the
#' value `posterior_predict()` draws: `1..K`, with `0` for the hurdle
#' category of `hurdle_cumulative()`. Every ordinal summary is then on the
#' scale of the draws themselves.
#'
#' @param epred Array ``\\[ndraws x nobs x ncat\\]`` of category
#'   probabilities, its third margin named by level
#' @param f Function applied to the level before averaging
#' @return Numeric matrix ``\\[ndraws x nobs\\]``
#'
#' @noRd
ordinal_category_moment <- function(epred, f) {
  checkmate::assert_array(epred, mode = "numeric", d = 3L)
  levels <- ordinal_levels(epred)
  out <- matrix(0, nrow = dim(epred)[1L], ncol = dim(epred)[2L])
  for (k in seq_along(levels)) {
    out <- out + f(levels[k]) * epred[, , k]
  }
  out
}


#' The ordered level that each category of an ordinal epred represents
#'
#' @param probs Array ``\\[ndraws x nobs x ncat\\]`` of category
#'   probabilities, its third margin named by level
#' @return Integer vector of length `ncat`
#' @noRd
ordinal_levels <- function(probs) {
  levels <- dimnames(probs)[[3L]]
  checkmate::assert_character(levels, len = dim(probs)[3L],
                              any.missing = FALSE)
  as.integer(levels)
}


#' Expected ordered level under an ordinal epred
#'
#' @param epred Array ``\\[ndraws x nobs x ncat\\]`` of category
#'   probabilities, its third margin named by level
#' @return Numeric matrix ``\\[ndraws x nobs\\]`` of expected levels
#'
#' @noRd
ordinal_category_mean <- function(epred) {
  ordinal_category_moment(epred, function(k) k)
}


#' Conditional variance of an ordinal response
#'
#' `E[k^2] - E[k]^2` over the ordered levels, which is the variance of
#' the category `posterior_predict()` draws.
#'
#' @param epred Array ``\\[ndraws x nobs x ncat\\]`` of category
#'   probabilities
#' @return Numeric matrix ``\\[ndraws x nobs\\]`` of variances
#'
#' @noRd
ordinal_category_variance <- function(epred) {
  ordinal_category_moment(epred, function(k) k^2) -
    ordinal_category_mean(epred)^2
}


#' Ordinal thresholds on the scale of mvgam's linear predictor
#'
#' brms centres the design before sampling. The sampled `Intercept[k]`
#' cuts a predictor built from the centred columns. The predictor mvgam
#' builds after fitting uses the uncentred design, as brms's own
#' predictions do. The thresholds that cut it are the `b_Intercept[k]`
#' that brms writes in generated quantities. The two differ by the slopes
#' times the covariate means. Using the first set shifts every category
#' probability by that much. A response of a model with several responses
#' uses its own, `b_<resp>_Intercept[k]`.
#'
#' @param object A fitted `mvgam` object
#' @param draw_ids Draws the predictor was taken at, or `NULL` for all
#' @param resp The response's key, or `NULL`
#' @return Matrix `[ndraws x nthres]`, thresholds in increasing index
#' @noRd
ordinal_thresholds <- function(object, draw_ids = NULL, resp = NULL) {
  checkmate::assert_class(object, "mvgam")
  draws <- posterior::as_draws_matrix(object$fit)
  prefix <- paste0("b", response_suffix(object, resp), "_Intercept")
  pattern <- paste0("^", prefix, "\\[([0-9]+)\\]$")
  cols <- grep(pattern, colnames(draws), value = TRUE)
  if (length(cols) == 0L) {
    stop(insight::format_error(paste0(
      "The fit lacks ordinal thresholds '", prefix, "[k]'."
    )))
  }
  cols <- cols[order(as.integer(sub(pattern, "\\1", cols)))]
  thres <- matrix(as.numeric(draws[, cols, drop = FALSE]),
                  ncol = length(cols))
  subset_draws_rows(thres, draw_ids = draw_ids)
}


#' Category probabilities of an ordinal response
#'
#' The category probabilities of an ordinal family's predictive
#' distribution. `posterior_epred()` reports them. `log_lik()` takes the
#' observed category's probability from them. `posterior_predict()` draws
#' a category from them. `disc` and `hu` (for the hurdle variant) come
#' from `resolve_family_pars()`. That function takes a sampled parameter
#' from the draws and rebuilds a modelled one from its own formula. brms
#' fixes `disc` at one unless it is given a formula. A modelled `disc` is
#' a local of the model block and leaves no draw. The posterior holds no
#' value for it. Its fallback is one.
#'
#' @param object A fitted `mvgam` object
#' @param linpred `[ndraws x nobs]` link-scale predictor of `mu`
#' @param family The response's ordinal family
#' @param draw_ids Draws `linpred` was taken at, or `NULL` for all
#' @param newdata Data `linpred` was built for
#' @param resp The response's key, or `NULL`
#' @return Array `[ndraws x nobs x ncat]`, as `ordinal_probs()` gives
#' @noRd
ordinal_category_probs <- function(object, linpred, family,
                                   draw_ids = NULL, newdata = NULL,
                                   resp = NULL) {
  pars <- resolve_family_pars(
    object,
    dpar_names = setdiff(family$dpars, "mu"),
    ndraws = nrow(linpred),
    nobs = ncol(linpred),
    draw_ids = draw_ids,
    newdata = newdata,
    resp = resp
  )
  ordinal_probs(
    eta = linpred,
    thres = ordinal_thresholds(object, draw_ids = draw_ids, resp = resp),
    family = family,
    disc = pars$disc,
    hu = pars$hu
  )
}


#' Category probabilities from an ordinal predictor
#'
#' Adapted from brms. Each threshold is set against the predictor.
#' Families that brms marks `"thres_minus_eta"` use
#' `disc * (thres - eta)`. Families marked `"eta_minus_thres"` use
#' `disc * (eta - thres)`. The family's inverse link then turns the result
#' into category probabilities. A family that brms marks `"extra_cat"` is
#' the hurdle variant. It puts its hurdle probability on a category of its
#' own, placed before the ordered categories. Those categories are scaled
#' by `1 - hu`.
#'
#' @param eta `[ndraws x nobs]` predictor of `mu`
#' @param thres `[ndraws x nthres]` thresholds
#' @param family An ordinal brms family
#' @param disc `[ndraws x nobs]` discrimination, or `NULL` for brms's
#'   fixed value of one
#' @param hu `[ndraws x nobs]` hurdle probability, used only by a family
#'   marked `"extra_cat"`
#' @return Array `[ndraws x nobs x ncat]` whose third margin is named by
#'   the ordered level each category represents: `1..K`, with `0` first
#'   for the hurdle category
#' @noRd
ordinal_probs <- function(eta, thres, family, disc = NULL, hu = NULL) {
  checkmate::assert_matrix(eta, mode = "numeric")
  ndraws <- nrow(eta)
  nobs <- ncol(eta)
  checkmate::assert_matrix(thres, mode = "numeric", nrows = ndraws,
                           min.cols = 1L)
  nthres <- ncol(thres)
  if (is.null(disc)) {
    disc <- matrix(1, nrow = ndraws, ncol = nobs)
  }
  checkmate::assert_matrix(disc, nrows = ndraws, ncols = nobs)

  # One cell per draw, observation and threshold
  eta3 <- array(eta, c(ndraws, nobs, nthres))
  thres3 <- aperm(array(thres, c(ndraws, nthres, nobs)), c(1L, 3L, 2L))
  offset <- if ("eta_minus_thres" %in% family$specials) {
    eta3 - thres3
  } else {
    thres3 - eta3
  }
  x <- array(disc, c(ndraws, nobs, nthres)) * offset
  probs <- switch(
    resolve_family_name(family),
    sratio = inv_link_sratio(x, family$link),
    cratio = inv_link_cratio(x, family$link),
    acat = inv_link_acat(x, family$link),
    inv_link_cumulative(x, family$link)
  )
  levels <- seq_len(nthres + 1L)
  if ("extra_cat" %in% family$specials) {
    checkmate::assert_matrix(hu, nrows = ndraws, ncols = nobs)
    probs <- abind::abind(hu, probs * as.vector(1 - hu), along = 3L)
    levels <- c(0L, levels)
  }
  dimnames(probs) <- list(NULL, NULL, levels)
  probs
}


#' The cell of each observed level in an array of category probabilities
#'
#' @param probs Category probabilities from `ordinal_category_probs()`
#' @param y Observed ordered levels, `NA` where a row was not measured
#' @return Index matrix of `[draw, observation, category]` rows, one per
#'   draw and observation, draw fastest. A missing `y` gives an `NA`
#'   category.
#' @noRd
ordinal_cells <- function(probs, y) {
  levels <- ordinal_levels(probs)
  category <- match(y, levels)
  unknown <- !is.na(y) & is.na(category)
  if (any(unknown)) {
    stop(insight::format_error(c(
      "An ordinal response has a value outside its levels.",
      x = paste0("Found: ", paste(unique(y[unknown]), collapse = ", "),
                 "."),
      i = paste0("The levels are ", paste(levels, collapse = ", "), ".")
    )), call. = FALSE)
  }
  ndraws <- dim(probs)[1L]
  nobs <- dim(probs)[2L]
  cbind(
    rep(seq_len(ndraws), nobs),
    rep(seq_len(nobs), each = ndraws),
    rep(category, each = ndraws)
  )
}


#' Log density of each observed ordered level
#'
#' @inheritParams ordinal_cells
#' @return `[ndraws x nobs]` matrix, `NA` in a column whose `y` is
#'   missing
#' @noRd
ordinal_log_lik <- function(probs, y) {
  matrix(log(probs[ordinal_cells(probs, y)]), nrow = dim(probs)[1L])
}


#' The interval of the predictive distribution each observed level fills
#'
#' `P(Y < y)` and `P(Y <= y)` per draw, the bounds the Dunn-Smyth
#' randomised quantile residual draws between.
#'
#' @inheritParams ordinal_cells
#' @return List of `[ndraws x nobs]` matrices `lower` and `upper`, `NA`
#'   in a column whose `y` is missing
#' @noRd
ordinal_pit_bounds <- function(probs, y) {
  cells <- ordinal_cells(probs, y)
  below <- aperm(apply(probs, c(1L, 2L), cumsum), c(2L, 3L, 1L))
  ndraws <- dim(probs)[1L]
  upper <- matrix(below[cells], nrow = ndraws)
  list(lower = upper - matrix(probs[cells], nrow = ndraws), upper = upper)
}


#' Draw ordered levels from category probabilities
#'
#' @param probs Category probabilities from `ordinal_category_probs()`
#' @return `[ndraws x nobs]` integer matrix of levels
#' @noRd
ordinal_draws <- function(probs) {
  levels <- ordinal_levels(probs)
  ndraws <- dim(probs)[1L]
  nobs <- dim(probs)[2L]
  u <- matrix(stats::runif(ndraws * nobs), nrow = ndraws, ncol = nobs)
  category <- matrix(1L, nrow = ndraws, ncol = nobs)
  below <- matrix(0, nrow = ndraws, ncol = nobs)
  for (k in seq_len(length(levels) - 1L)) {
    below <- below + probs[, , k]
    category <- category + (u > below)
  }
  matrix(levels[category], nrow = ndraws, ncol = nobs)
}


# --- Ordinal link inverses, adapted from brms::distributions.R ---
# Each takes `x`, an array `[ndraws x nobs x nthres]` of the threshold
# offsets that `ordinal_probs()` forms. It returns `[ndraws x nobs x ncat]`
# category probabilities.

#' Inverse Link for Cumulative Models
#'
#' `P(Y <= k) = F(x_k)`, differenced into category probabilities.
#'
#' @param x Array `[ndraws x nobs x nthres]` of threshold offsets
#' @param link Name of the family's link
#' @return Array `[ndraws x nobs x ncat]` of category probabilities
#' @noRd
inv_link_cumulative <- function(x, link) {
  checkmate::assert_array(x, mode = "numeric", d = 3L)
  checkmate::assert_string(link)

  x <- inv_link(x, link)
  ndim <- length(dim(x))
  dim_noncat <- dim(x)[-ndim]
  ones_arr <- array(1, dim = c(dim_noncat, 1))
  zeros_arr <- array(0, dim = c(dim_noncat, 1))
  abind::abind(x, ones_arr) - abind::abind(zeros_arr, x)
}

#' Inverse Link for Sequential Ratio Models
#'
#' `P(Y = k | Y >= k) = F(x_k)`.
#'
#' @inheritParams inv_link_cumulative
#' @return Array `[ndraws x nobs x ncat]` of category probabilities
#' @noRd
inv_link_sratio <- function(x, link) {
  checkmate::assert_array(x, mode = "numeric", d = 3L)
  checkmate::assert_string(link)

  x <- inv_link(x, link)
  ndim <- length(dim(x))
  dim_noncat <- dim(x)[-ndim]
  nthres <- dim(x)[ndim]
  marg_noncat <- seq_along(dim(x))[-ndim]
  ones_arr <- array(1, dim = c(dim_noncat, 1))
  dim_t <- c(nthres, dim_noncat)
  Sx_cumprod <- aperm(
    array(apply(1 - x, marg_noncat, cumprod), dim = dim_t),
    perm = c(marg_noncat + 1, 1)
  )
  abind::abind(x, ones_arr) * abind::abind(ones_arr, Sx_cumprod)
}

#' Inverse Link for Continuation Ratio Models
#'
#' `P(Y > k | Y >= k) = F(x_k)`.
#'
#' @inheritParams inv_link_cumulative
#' @return Array `[ndraws x nobs x ncat]` of category probabilities
#' @noRd
inv_link_cratio <- function(x, link) {
  checkmate::assert_array(x, mode = "numeric", d = 3L)
  checkmate::assert_string(link)

  x <- inv_link(x, link)
  ndim <- length(dim(x))
  dim_noncat <- dim(x)[-ndim]
  nthres <- dim(x)[ndim]
  marg_noncat <- seq_along(dim(x))[-ndim]
  ones_arr <- array(1, dim = c(dim_noncat, 1))
  dim_t <- c(nthres, dim_noncat)
  x_cumprod <- aperm(
    array(apply(x, marg_noncat, cumprod), dim = dim_t),
    perm = c(marg_noncat + 1, 1)
  )
  abind::abind(1 - x, ones_arr) * abind::abind(ones_arr, x_cumprod)
}

#' Inverse Link for Adjacent Category Models
#'
#' `P(Y = k + 1 | Y in {k, k + 1}) = F(x_k)`.
#'
#' @inheritParams inv_link_cumulative
#' @return Array `[ndraws x nobs x ncat]` of category probabilities
#' @noRd
inv_link_acat <- function(x, link) {
  checkmate::assert_array(x, mode = "numeric", d = 3L)
  checkmate::assert_string(link)

  ndim <- length(dim(x))
  dim_noncat <- dim(x)[-ndim]
  nthres <- dim(x)[ndim]
  marg_noncat <- seq_along(dim(x))[-ndim]
  ones_arr <- array(1, dim = c(dim_noncat, 1))
  dim_t <- c(nthres, dim_noncat)

  if (link == "logit") {
    # Faster evaluation for logit link
    exp_x_cumprod <- aperm(
      array(apply(exp(x), marg_noncat, cumprod), dim = dim_t),
      perm = c(marg_noncat + 1, 1)
    )
    out <- abind::abind(ones_arr, exp_x_cumprod)
  } else {
    x <- inv_link(x, link)
    x_cumprod <- aperm(
      array(apply(x, marg_noncat, cumprod), dim = dim_t),
      perm = c(marg_noncat + 1, 1)
    )
    Sx_cumprod_rev <- apply(
      1 - x[, , rev(seq_len(nthres)), drop = FALSE],
      marg_noncat, cumprod
    )
    Sx_cumprod_rev <- aperm(
      array(Sx_cumprod_rev, dim = dim_t),
      perm = c(marg_noncat + 1, 1)
    )
    Sx_cumprod_rev <- Sx_cumprod_rev[, , rev(seq_len(nthres)), drop = FALSE]
    out <- abind::abind(ones_arr, x_cumprod) *
      abind::abind(Sx_cumprod_rev, ones_arr)
  }
  catsum <- array(apply(out, marg_noncat, sum), dim = dim_noncat)
  sweep(out, marg_noncat, catsum, "/")
}
