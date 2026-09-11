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

  # Validate trials when provided
  if (!is.null(trials)) {
    checkmate::assert_numeric(trials, min.len = 1)
    if (length(trials) != 1 && length(trials) != ncol(linpred)) {
      stop(insight::format_error(
        cli::format_inline(
          "{.field trials} must be length 1 or match number of observations ({ncol(linpred)})."
        )
      ))
    }
  }

  # A family named in `epred_extra_dpars()` has a mean that is not the
  # inverse link of its predictor, and a kernel that says what it is
  # instead. Routing on that registry keeps one list rather than two:
  # the families needing extra parameters and the families needing a
  # kernel are the same families, and adding one means editing one
  # place. `log_lik.mvgam()` dispatches to its own kernels the same way.
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
      prob <- family$linkinv(linpred)
      # Column-wise multiplication: each column by its trial count
      if (length(trials) == 1) {
        prob * trials
      } else {
        sweep(prob, 2, trials, `*`)
      }
    },


    # `expected_from_linpred()` hands every closure-unit family to its
    # kernel before this dispatch. The branch is what a caller reaching
    # the dispatch directly meets.
    "nmix" = ,
    "nmix_royle_nichols" = ,
    "nmix_poisson_poisson" = stop(insight::format_error(c(
      paste0(
        "Family '", family_name,
        "' has no mean this dispatch can compute."
      ),
      i = paste0(
        "A closure-unit family's mean reads the unit's other visits. ",
        "It needs the fit's `p` draws and the arrays that the ",
        "kernels in dispatch_closure_unit_method() supply."
      )
    ))),

    # Tweedie (compound Poisson-gamma): E[Y | mu, phi, theta] = mu
    # for all theta in [1, 2], including the boundary cases (scaled
    # Poisson at theta = 1, Gamma at theta = 2). The point mass at
    # zero is absorbed into mu without any Jensen correction.
    "tweedie" = family$linkinv(linpred),

    # Beta negative binomial: the family is parameterised so that
    # E[Y] = mu exactly, with the shifted tail parameter absorbing
    # the alpha > 1 constraint that makes the mean exist at all.
    "beta_nb" = family$linkinv(linpred),

    # Default: try inverse link with warning for unknown families
    {
      if (!identical(Sys.getenv("TESTTHAT"), "true")) {
        rlang::warn(
          c(
            paste0("Unknown family '", family_name, "', using inverse link."),
            "i" = "Expected values may not be correct for complex families."
          ),
          .frequency = "once",
          .frequency_id = paste0("epred_unknown_family_", family_name)
        )
      }
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
#' @param sigma Optional ``\\[ndraws x nobs\\]`` matrix of `sigma` draws. Required
#'   for `gaussian` (Var = sigma^2). Other families ignore it.
#' @param shape Optional ``\\[ndraws x nobs\\]`` matrix of `shape` draws. Required
#'   for `negbinomial` (Var = mu + mu^2/shape) and `gamma`
#'   (Var = mu^2/shape).
#' @param phi Optional ``\\[ndraws x nobs\\]`` matrix of `phi` draws. Required for
#'   `beta` (Var = mu(1-mu)/(1+phi)). `phi` here is the brms precision
#'   parameter (shape1 + shape2 of the underlying Beta), not an
#'   overdispersion.
#' @param nu Optional ``\\[ndraws x nobs\\]`` matrix of `nu` (degrees of
#'   freedom) draws. Required for `student`
#'   (Var = sigma^2 * nu / (nu - 2) when nu > 2; `Inf` otherwise).
#' @param trials Optional numeric vector (length `ncol(mu)` or 1) of trial
#'   counts for `binomial` (Var = trials * p * (1-p) where p = mu/trials).
#'
#' @return Numeric matrix ``\\[ndraws x nobs\\]`` of conditional variances on the
#'   response scale.
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
      stop(insight::format_error(c(
        paste0(
          "Family '", family_name, "' requires '", dpar_name,
          "' for variance computation."
        ),
        i = "Internal: check predict_variance() extraction path."
      )))
    }
    checkmate::assert_matrix(value)
    if (nrow(value) != nrow(mu) || ncol(value) != ncol(mu)) {
      stop(insight::format_error(c(
        paste0(
          "Dimension mismatch for '", dpar_name, "' in variance computation."
        ),
        x = paste0(
          "Got [", nrow(value), " x ", ncol(value), "]; expected [",
          nrow(mu), " x ", ncol(mu), "]."
        )
      )))
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
      if (any(trials == 0) &&
          !identical(Sys.getenv("TESTTHAT"), "true")) {
        rlang::warn(
          c(
            "'trials' contains zeros.",
            i = "Variance is NaN for binomial observations with 0 trials."
          ),
          .frequency = "once",
          .frequency_id = "binomial_variance_zero_trials"
        )
      }
      if (length(trials) == 1L) {
        p <- mu / trials
        p * (1 - p) * trials
      } else {
        trials_mat <- matrix(trials, nrow = nrow(mu), ncol = ncol(mu),
                             byrow = TRUE)
        p <- mu / trials_mat
        p * (1 - p) * trials_mat
      }
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
      # Return Inf for the undefined region so downstream summaries
      # propagate non-finiteness rather than silently NA.
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
#' @param process_error Logical; if `TRUE`, the trend contributes a
#'   sampled innovation on top of its deterministic submodel, drawn per
#'   posterior draw from the distribution the trend settles into. For an
#'   autoregressive trend that is the stationary distribution, so an
#'   AR(1) draws at \eqn{\sigma^2/(1-\rho^2)} rather than at the
#'   innovation variance, and the resulting Jensen correction on a log
#'   link is \eqn{\sigma^2/(2(1-\rho^2))}. A random walk has no
#'   stationary distribution and a `ZMVN()` trend has no dynamics to
#'   settle into, so both draw their innovations directly; `CAR()` does
#'   the same, its decay depending on the gap between observations. If
#'   `FALSE`, the default, the trend contributes its deterministic
#'   submodel alone, still at its own per-draw values, so `FALSE` is
#'   not a collapse to a posterior mean. Read only under
#'   `incl_autocor = FALSE`, since conditioning on the fitted state
#'   leaves no innovation to sample.
#'
#'   The innovations are drawn afresh on each call, so two calls on one
#'   fit give different answers. Set a seed for a reproducible one.
#' @param incl_autocor Logical; which prediction surface the trend
#'   contribution comes from. `FALSE`, the default, keeps the fitted
#'   `trend[t, s]` out and answers from the two submodels' covariate
#'   structure, so an effect reads the same whether it is asked at the
#'   first time point or the fiftieth. `TRUE` reads the latent state
#'   the model inferred at each time, which is the quantity
#'   [hindcast()] returns and the one an ELPD is built from;
#'   [loo_R2.mvgam()] and [bayes_R2.mvgam()] ask for it, because an
#'   R^2 describes the series that was observed rather than a
#'   counterfactual one. A row whose time falls outside the fitted grid
#'   has no such state and takes the per-series marginal.
#'
#'   The default suits a model whose covariates carry the signal. When
#'   most of the series-level variation sits in the trend instead, as
#'   in a fit with few covariates and a strong autoregressive process,
#'   this surface has little left to say; [hindcast.mvgam()] and
#'   [forecast.mvgam()] are the better reading there.
#' @param ndraws Positive integer specifying number of posterior draws to
#'   use. NULL (default) uses all available draws.
#' @param draw_ids Optional integer vector selecting a subset of posterior
#'   draw indices to use. NULL (default) uses all draws (subject to
#'   `ndraws`).
#' @param re_formula Formula for random effects. NULL (default) includes
#'   all random effects, NA excludes all random effects.
#' @param allow_new_levels Logical; accepted for brms compatibility.
#'   A grouping level the model never saw is refused whatever this is
#'   set to, because a new level has no fitted random effect and, for
#'   a new series, no latent state to propagate. Predict for levels
#'   the fit knows, or refit with the new levels included.
#' @param sample_new_levels Character; accepted for brms
#'   compatibility and not used, since new levels are refused. See
#'   `allow_new_levels`.
#' @param resp Character specifying which response variable for
#'   multivariate models. NULL (default) returns predictions for all
#'   responses.
#' @param ... Additional arguments passed to internal methods.
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
#'   state-extrapolating prediction surface (state read from the
#'   fitted `lv_trend` posterior draws instead of integrated over
#'   via Monte Carlo).
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
  checkmate::assert_logical(process_error, len = 1)
  trend_state <- autocor_to_trend_state(incl_autocor)
  checkmate::assert_integerish(draw_ids, lower = 1, null.ok = TRUE,
                                any.missing = FALSE)

  # Handle newdata = NULL (use training data)
  if (is.null(newdata)) {
    if (is.null(object$data)) {
      stop(insight::format_error(
        cli::format_inline(
          "No training data found in model object. Please provide {.field newdata} explicitly."
        )
      ))
    }
    newdata <- object$data
  }

  # The linear predictor and the distributional parameters are drawn
  # by separate extractions, so a requested count is materialised as
  # concrete indices before either runs; otherwise the two subsample
  # independently and answer from unrelated iterations.
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

  # The marginal expectation integrates over the trend's dynamics by
  # carrying sampled innovations on the link scale before the inverse
  # link is applied, which reaches the same place as the analytic
  # correction brms makes for an AR-cov model (sigma^2/(1-ar^2)/2 for
  # a Poisson log link) while working uniformly across families,
  # links and trend types. `get_combined_linpred()` samples them, and
  # samples them once: drawing a second set here as well put twice
  # the process variance into every marginal prediction.

  # A model with several responses answers for each when `resp` names
  # none, as brms does.
  if (is.list(linpred) && !is.matrix(linpred)) {
    return(lapply(stats::setNames(nm = names(linpred)), function(r) {
      response_epred(object, linpred[[r]], newdata, draw_ids, resp = r)
    }))
  }
  response_epred(object, linpred, newdata, draw_ids, resp = resp)
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
response_epred <- function(object, linpred, newdata, draw_ids, resp) {
  family <- model_families(object, resp)
  if (is_ordinal_family(family)) {
    return(ordinal_category_probs(
      object, linpred, family, draw_ids = draw_ids, newdata = newdata,
      resp = resp
    ))
  }
  expected_from_linpred(
    object, linpred, newdata = newdata, draw_ids = draw_ids, resp = resp
  )
}


#' The family's mean for a linear predictor already on the link scale
#'
#' The inverse link answers this only for a family whose expectation
#' is its predictor. A binomial mean is `trials * p` and a
#' zero-inflated mean is `(1 - zi) * mu`, so a surface that applies
#' the inverse link alone reports a probability where a count was
#' asked for, or a rate before its mixing probability.
#'
#' Trials come from `extract_trials_for_family()`. Families needing
#' more than `(linpred, trials)` are named by `epred_extra_dpars()`:
#' a mixture at zero needs its mixing probability, a lognormal the
#' dispersion its Jensen correction uses, and a
#' Conway-Maxwell-binomial has no closed form in `mu` alone. Those
#' draws are resolved against the same iterations the predictor came
#' from.
#'
#' A closure-unit family's mean reads sibling rows: an occupancy is
#' thinned by the visit's detection probability, and a composition
#' normalises across the K rows of its site. Neither is a function of
#' one row's predictor, so each has a kernel that says what it is,
#' and this routes to that kernel with the predictor it was handed.
#' Applying the inverse link instead returned the raw predictor for
#' a composition and the occupancy without its detection.
#'
#' An ordinal response's mean is its expected ordered level, the
#' scale `posterior_predict()` draws on. `posterior_epred()` reports
#' the category probabilities instead, through `response_epred()`.
#'
#' Each response reads its own family, trials and parameters. A
#' response of a model with several stores its parameters under its
#' own key, `hu_count` for `hu`, and reading them unscoped found
#' none.
#'
#' @param object A fitted `mvgam` object.
#' @param linpred `[ndraws x nobs]` link-scale predictor, or a named
#'   list of them for a multivariate fit.
#' @param newdata Data the predictor was built for, used to read
#'   trials and any per-observation parameter.
#' @param draw_ids Draws the predictor was taken at, so the extra
#'   parameters are read at the same iterations.
#' @param resp The response's key, or `NULL`.
#' @return Matrix of expected values, or a named list of them.
#' @noRd
expected_from_linpred <- function(object, linpred, newdata = NULL,
                                  draw_ids = NULL, resp = NULL) {
  if (is.list(linpred) && !is.matrix(linpred)) {
    return(lapply(stats::setNames(nm = names(linpred)), function(r) {
      expected_from_linpred(
        object, linpred[[r]], newdata = newdata, draw_ids = draw_ids,
        resp = r
      )
    }))
  }
  family <- model_families(object, resp)
  if (is_closure_unit_family(family)) {
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
      resp = resp
    )
  )
}


#' Distributional parameters a family's mean needs beyond the predictor
#'
#' Most families have `E[Y]` equal to the inverse link of the linear
#' predictor, so the predictor is all `compute_family_epred()` needs.
#' The families named here do not: a mixture at zero, a Jensen
#' correction or a sum over the support puts another parameter in the
#' mean. Naming them in one place is what lets `posterior_epred()`
#' resolve those draws before it dispatches, rather than each family
#' discovering at the last moment that it was handed the predictor
#' alone.
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
#' Each family's mean lives in one `posterior_epred_<family>()`
#' function copied from brms. Looking it up by name is how
#' `log_lik.mvgam()` reaches its own per-family densities, and it means
#' a family's mean is written once rather than once in a kernel and
#' again in a dispatch branch.
#'
#' @param family_name Resolved family name
#' @return The kernel, or `NULL` when the family has none
#'
#' @noRd
epred_kernel <- function(family_name) {
  # Asked rather than attempted, so a family with no kernel is
  # answered `NULL` while a kernel that fails to load still raises.
  nm <- paste0("posterior_epred_", family_name)
  if (!exists(nm, mode = "function", envir = asNamespace("mvgam"))) {
    return(NULL)
  }
  get(nm, mode = "function", envir = asNamespace("mvgam"))
}


#' Mean of a family whose mean is not its inverse link
#'
#' Applies the per-family mean its brms counterpart uses, so the two
#' agree on what `posterior_epred()` returns and there is one
#' definition of each mean rather than a kernel and a dispatch branch
#' free to drift apart. `mu` here is the base distribution's
#' parameter, already on the response scale.
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
      i = paste0(
        "Resolve them with `resolve_family_pars()` and pass them as ",
        "'family_pars'. Depending on the family, a mixing ",
        "probability, a shift or a dispersion separates E[Y] from ",
        "the base distribution's parameter."
      )
    )))
  }

  for (dpar in needed) {
    value <- family_pars[[dpar]]
    checkmate::assert_matrix(value)
    if (nrow(value) != nrow(mu) || ncol(value) != ncol(mu)) {
      stop(insight::format_error(c(
        paste0("Dimension mismatch for '", dpar, "' in E[Y]."),
        x = paste0("Got [", nrow(value), " x ", ncol(value),
                   "]; expected [", nrow(mu), " x ", ncol(mu), "].")
      )))
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

  # `resolve_family_name()` reads a custom family's own name, which
  # brms records as "custom", and `com_binomial()` takes trials too.
  family_name <- resolve_family_name(family)

  binomial_families <- c("binomial", "beta_binomial",
                         "zero_inflated_binomial",
                         "zero_inflated_beta_binomial",
                         "com_binomial")

  if (!family_name %in% binomial_families) {
    return(NULL)
  }

  # Predictions cover every row of the target data, including rows
  # whose response was missing, so the denominator is resolved
  # against that data rather than reused from `standata`, which is
  # sized to the likelihood's observed rows.
  pred_data <- if (is.null(newdata)) object$data else newdata
  trials <- resolve_trials_denominator(object$formula, pred_data) %||%
    pred_data$trials

  # `standata` answers only for the data the model was fitted to.
  # Reusing it for a grid the caller supplied would hand each row
  # another observation's denominator, and would go unnoticed
  # whenever the two happen to carry the same number of rows.
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

  # A denominator of zero is a legal binomial observation: no trials
  # were run, so the response is necessarily zero and the expected
  # value with it. brms accepts it (`data_response()` rejects only
  # negatives), and it is the natural padding for a cell the
  # likelihood never saw, so the bound here matches.
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
#' Converts vector or matrix data to appropriate dimensions for vectorized
#' multiplication with posterior draws. Used to broadcast observation-level
#' data (trials, rate denominators) across all posterior draws.
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
      stop(insight::format_error(
        cli::format_inline(
          "Length of {.field x} must be 1 or {dim[2]}, not {length(x)}."
        )
      ))
    }
    matrix(x, nrow = dim[1], ncol = dim[2], byrow = TRUE)
  } else {
    # Expand to 3D array for categorical/compositional families
    # dim[1] = ndraws, dim[2:3] = observation dimensions
    if (!(length(x) == 1 || identical(dim(x), as.integer(dim[2:3])))) {
      stop(insight::format_error(
        cli::format_inline(
          "Dimension of {.field x} must match dim[2:3]."
        )
      ))
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

#' Beta dispersion is included in mu; formula identical to binomial
#' @noRd
posterior_epred_beta_binomial <- function(prep) {
  trials <- data2draws(prep$data$trials, dim_mu(prep))
  prep$dpars$mu * trials
}

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
posterior_epred_zero_inflated_negbinomial <- function(prep) {
  with(prep$dpars, mu * (1 - zi))
}

#' @noRd
posterior_epred_zero_inflated_binomial <- function(prep) {
  trials <- data2draws(prep$data$trials, dim_mu(prep))
  prep$dpars$mu * trials * (1 - prep$dpars$zi)
}

#' Beta dispersion is included in mu; formula identical to zi_binomial
#' @noRd
posterior_epred_zero_inflated_beta_binomial <- function(prep) {
  trials <- data2draws(prep$data$trials, dim_mu(prep))
  prep$dpars$mu * trials * (1 - prep$dpars$zi)
}

#' @noRd
posterior_epred_zero_inflated_beta <- function(prep) {
  with(prep$dpars, mu * (1 - zi))
}

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
    if (!identical(Sys.getenv("TESTTHAT"), "true")) {
      rlang::warn(
        c("Approximating the mean of discrete_weibull may be inaccurate.",
          "i" = "Series did not converge within M terms."),
        .frequency = "once",
        .frequency_id = "mean_discrete_weibull_convergence"
      )
    }
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
    mu_e <- mu[use_exact]
    shape_e <- shape[use_exact]
    log_mu <- log(mu_e)
    log_thres <- log(thres)

    # First 2 terms of series
    log_num <- shape_e * log_mu
    log_Z <- log1p(exp(shape_e * log_mu))
    lfac <- 0
    k <- 2
    converged <- FALSE

    while (!converged && k <= M) {
      log_k <- log(k)
      lfac <- lfac + log_k
      term <- shape_e * (k * log_mu - lfac)
      log_num <- log(exp(log_num) + exp(log_k + term))
      log_Z <- log(exp(log_Z) + exp(term))
      converged <- all(term <= log_thres)
      k <- k + 1
    }

    if (!converged) {
      if (!identical(Sys.getenv("TESTTHAT"), "true")) {
        rlang::warn(
          c("Approximating the mean of com_poisson may be inaccurate.",
            "i" = "Series did not converge within M terms."),
          .frequency = "once",
          .frequency_id = "mean_com_poisson_convergence"
        )
      }
    }
    out[use_exact] <- exp(log_num - log_Z)
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
#' hurdle_cumulative. Reading the mark keeps the hurdle variant in,
#' which a list of four names had left out.
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
#' An ordinal epred holds a probability per category rather than a mean,
#' so a summary on the response scale is an expectation over the ordered
#' levels. Each category's level is its name on the third margin, the
#' value `posterior_predict()` draws: `1..K`, with `0` for the hurdle
#' category of `hurdle_cumulative()`. Every ordinal summary is then on
#' the scale of the draws themselves.
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


#' The ordered level each category of an ordinal epred stands for
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
#' brms centres the design before sampling, so the sampled
#' `Intercept[k]` cut a predictor built from the centred columns. The
#' predictor mvgam builds after fitting uses the uncentred design, as
#' brms's own predictions do, and the thresholds that cut it are the
#' `b_Intercept[k]` brms writes in generated quantities. The two differ
#' by the slopes times the covariate means, and reading the first set
#' shifted every category probability by that much. A response of a
#' model with several reads its own, `b_<resp>_Intercept[k]`.
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
      "The fit carries no ordinal thresholds named '", prefix, "[k]'."
    )))
  }
  cols <- cols[order(as.integer(sub(pattern, "\\1", cols)))]
  thres <- matrix(as.numeric(draws[, cols, drop = FALSE]),
                  ncol = length(cols))
  subset_draws_rows(thres, draw_ids = draw_ids)
}


#' Category probabilities of an ordinal response
#'
#' The one account of an ordinal family's predictive distribution:
#' `posterior_epred()` reports it, `log_lik()` reads the observed
#' category's probability from it and `posterior_predict()` draws a
#' category from it. `disc`, and `hu` for the hurdle variant, come from
#' `resolve_family_pars()`, which reads a sampled parameter off the
#' draws and rebuilds a modelled one from its own formula. brms fixes
#' `disc` at one unless it is given a formula. A modelled `disc` is a
#' local of the model block and leaves no draw behind, which is why
#' reading the posterior for it fell back to one.
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
#' Adapted from brms. Every threshold is set against the predictor,
#' as `disc * (thres - eta)` for the families brms marks
#' `"thres_minus_eta"` and as `disc * (eta - thres)` for those it marks
#' `"eta_minus_thres"`, and the family's inverse link turns the result
#' into category probabilities. A family brms marks `"extra_cat"`, the
#' hurdle variant, puts its hurdle probability on a category of its
#' own ahead of the ordered ones and scales those by `1 - hu`.
#'
#' @param eta `[ndraws x nobs]` predictor of `mu`
#' @param thres `[ndraws x nthres]` thresholds
#' @param family An ordinal brms family
#' @param disc `[ndraws x nobs]` discrimination, or `NULL` for brms's
#'   fixed value of one
#' @param hu `[ndraws x nobs]` hurdle probability, read only by a family
#'   marked `"extra_cat"`
#' @return Array `[ndraws x nobs x ncat]` whose third margin is named by
#'   the ordered level each category stands for: `1..K`, and `0` first
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
      "An ordinal response holds a value that is none of its levels.",
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
# offsets `ordinal_probs()` forms, and returns `[ndraws x nobs x ncat]`
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
