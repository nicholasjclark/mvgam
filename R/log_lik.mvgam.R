#' Pointwise log-likelihoods from a fitted mvgam model
#'
#' Compute the per-observation log-likelihood for each posterior draw,
#' matching `brms::log_lik` semantics. The result is an `S x N` matrix where
#' `S` is the number of posterior draws and `N` is the number of observations
#' in `newdata` (or the training data when `newdata = NULL`). Used by
#' [loo::loo()], [loo::waic()], [loo::psis()] and bayesplot PSIS-PIT helpers.
#'
#' @param object An mvgam fit.
#' @param newdata Optional data frame containing the response and all
#'   covariates referenced by the model. `NULL` (default) uses the original
#'   training data.
#' @param re_formula Random-effects formula passed to the prediction
#'   primitives. `NULL` includes all random effects.
#' @param resp Character; response name for multivariate models. Ignored for
#'   univariate fits.
#' @param ndraws Optional integer; subsample to `ndraws` posterior draws.
#' @param draw_ids Optional integer vector of draw indices to use (mutually
#'   exclusive with `ndraws`).
#' @param process_error Logical. When `TRUE` (default) each posterior draw
#'   carries a sampled latent-trend realisation through the linear
#'   predictor, integrating the trend's stochastic dynamics into the
#'   log-likelihood. When `FALSE` the trend is fixed at its posterior
#'   mean, returning a goodness-of-fit log-likelihood that ignores
#'   process noise.
#' @param ... Ignored.
#'
#' @return Numeric matrix `[ndraws x nobs]` of pointwise log densities.
#'
#' @details
#' The implementation dispatches by `object$family$family` to per-family log
#' density helpers. brms's parameterisations are reproduced verbatim
#' (`negbinomial` uses the `neg_binomial_2` mean-shape parameterisation;
#' `beta` uses `(mu, phi)`; `hurdle_*` and `zero_inflated_*` mix a point mass
#' at zero with the corresponding base family). Trials for `binomial` /
#' `beta_binomial` are extracted from `newdata` or from the model object via
#' the existing [extract_trials_for_family()] helper.
#'
#' For state-space-dominated fits, `process_error = TRUE` is the default and
#' matches the behaviour LOO / PSIS / WAIC expect. Set `process_error = FALSE`
#' only when computing per-observation goodness-of-fit independently of the
#' latent dynamics (e.g. for marginaleffects-style covariate effects).
#'
#' @seealso [loo.mvgam()], [waic.mvgam()],
#'   [posterior_linpred.mvgam()].
#'
#' @method log_lik mvgam
#' @export
log_lik.mvgam <- function(object,
                          newdata = NULL,
                          re_formula = NULL,
                          resp = NULL,
                          ndraws = NULL,
                          draw_ids = NULL,
                          process_error = TRUE,
                          ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_data_frame(newdata, null.ok = TRUE)
  checkmate::assert_int(ndraws, lower = 1, null.ok = TRUE)
  checkmate::assert_integerish(draw_ids, lower = 1, null.ok = TRUE)
  checkmate::assert_logical(process_error, len = 1)
  checkmate::assert_string(resp, null.ok = TRUE)

  newdata <- newdata %||% object$data

  # Link-scale linear predictor with optional trend realisations baked in
  linpred <- posterior_linpred(
    object,
    newdata = newdata,
    process_error = process_error,
    ndraws = ndraws,
    draw_ids = draw_ids,
    re_formula = re_formula,
    resp = resp
  )

  # Multivariate fits return a named list of [ndraws x nobs] matrices,
  # one per response. brms's log_lik returns the JOINT per-observation
  # log density when resp is NULL: a [ndraws x nobs] matrix where each
  # column sums the per-response log densities for that observation
  # row (verified against brms::log_lik on mvbind Gaussian fixtures).
  # Subset to a single response when resp is named.
  if (is.list(linpred) && !is.matrix(linpred)) {
    if (!is.null(resp)) {
      linpred <- linpred[[resp]]
    } else {
      resp_names <- names(linpred)
      per_resp <- lapply(resp_names, function(r) {
        log_lik_single_response(
          object = object,
          newdata = newdata,
          linpred = linpred[[r]],
          resp = r,
          draw_ids = draw_ids
        )
      })
      if (length(per_resp) == 1L) {
        return(per_resp[[1L]])
      }
      return(Reduce(`+`, per_resp))
    }
  }

  log_lik_single_response(
    object = object,
    newdata = newdata,
    linpred = linpred,
    resp = resp,
    draw_ids = draw_ids
  )
}


# Univariate log-lik path. Pulled out so the multivariate branch can
# loop over responses and stitch the [ndraws x sum(nobs)] matrix back
# together.
log_lik_single_response <- function(object, newdata, linpred, resp,
                                    draw_ids) {
  family_obj <- if (!is.null(resp)) {
    get_family_for_resp(object, resp)
  } else {
    object$family
  }
  family_name <- family_obj$family
  family_link <- family_obj$link

  # Observed response on the data scale
  y <- extract_response_for_log_lik(object, newdata, resp)

  # Distributional parameters (sigma, shape, hu, zi, ...) as [ndraws x nobs].
  # Multivariate fits store dpars as `<dpar>_<resp>` in the posterior;
  # extract under that name then rename back to the bare key so the
  # per-family helpers see a uniform structure.
  dpar_names <- get_family_dpars(family_name)
  family_pars <- if (length(dpar_names) > 0) {
    extract_names <- if (!is.null(resp)) {
      paste0(dpar_names, "_", resp)
    } else {
      dpar_names
    }
    out <- extract_dpars_from_stanfit(
      stanfit = object$fit,
      dpar_names = extract_names,
      ndraws = nrow(linpred),
      nobs = ncol(linpred),
      draw_ids = draw_ids
    )
    names(out) <- dpar_names
    out
  } else {
    list()
  }

  # Ordinal families need threshold and disc draws from the posterior
  # in addition to the standard dpars.
  if (family_name %in% c("cumulative", "sratio", "cratio", "acat")) {
    family_pars$thres <- extract_ordinal_thresholds(
      object,
      ndraws = nrow(linpred)
    )
    family_pars$disc <- extract_ordinal_disc(
      object,
      ndraws = nrow(linpred),
      nobs = ncol(linpred)
    )
  }

  # Trials for binomial-family responses
  trials <- extract_trials_for_family(object, family_obj, newdata)

  dispatch_log_lik(
    family_name = family_name,
    link = family_link,
    linpred = linpred,
    y = y,
    family_pars = family_pars,
    trials = trials
  )
}

# Generic re-export so users can call log_lik() without attaching brms first
#' @importFrom rstantools log_lik
#' @export
rstantools::log_lik


# Pull the response column out of newdata (or the training data) for the
# requested response. Binomial single-trial responses can arrive as a 2-col
# matrix from cbind() syntax; reduce to integer success counts.
extract_response_for_log_lik <- function(object, newdata, resp) {
  if (!is.null(resp)) {
    y_name <- resp
  } else {
    y_name <- as.character(object$formula$resp)
    if (length(y_name) == 0 || y_name == "") {
      lhs <- if (inherits(object$formula, "brmsformula")) {
        object$formula$formula[[2L]]
      } else {
        object$formula[[2L]]
      }
      y_name <- all.vars(lhs)[1L]
    }
  }
  if (!y_name %in% names(newdata)) {
    stop(insight::format_error(
      cli::format_inline(
        "Response variable {.val {y_name}} not found in {.field newdata}."
      )
    ))
  }
  y <- newdata[[y_name]]
  if (is.matrix(y) && ncol(y) == 2L) {
    # cbind(success, failure) → take success column
    y <- y[, 1L]
  }
  as.numeric(y)
}


# Family dispatcher. Returns [ndraws x nobs] matrix of log densities.
#
# Extension hook: hierarchical detection-error families (nmix and
# successors) need extra inputs beyond linpred + the standard dpars
# (latent abundance draws, detection probabilities, capacity per
# observation). Add them by extending `family_pars` upstream in
# `log_lik.mvgam` (e.g. pulling `trend` / `detprob` / `cap` via
# `as_draws_matrix(object, variable = ...)` when family_name == "nmix")
# and reading them from `family_pars` inside the per-family helper. No
# dispatcher changes needed.
dispatch_log_lik <- function(family_name, link, linpred, y,
                             family_pars, trials) {
  fn_name <- paste0("log_lik_", family_name)
  fn <- tryCatch(
    get(fn_name, mode = "function", envir = asNamespace("mvgam")),
    error = function(e) NULL
  )
  if (is.null(fn)) {
    stop(insight::format_error(
      cli::format_inline(
        "Family {.val {family_name}} is not yet supported by log_lik.mvgam."
      )
    ))
  }
  fn(
    linpred = linpred,
    link = link,
    y = y,
    family_pars = family_pars,
    trials = trials
  )
}


# -------------------------------------------------------------------------
# Per-family log densities
# -------------------------------------------------------------------------
# Each helper receives `linpred` [ndraws x nobs] on the link scale, the
# observed `y`, the distributional parameters as a named list of
# [ndraws x nobs] matrices, the `trials` vector for binomial-family, and the
# link string. Returns [ndraws x nobs] of log densities matching brms's
# *_lpdf / *_lpmf parameterisations.

# Apply the inverse link to a linpred matrix.
.linkinv <- function(linpred, link) {
  switch(link,
    identity = linpred,
    log = exp(linpred),
    logit = stats::plogis(linpred),
    probit = stats::pnorm(linpred),
    cloglog = 1 - exp(-exp(linpred)),
    inverse = 1 / linpred,
    sqrt = linpred^2,
    log1p = expm1(linpred),
    stop(insight::format_error(
      cli::format_inline("Unknown link function: {.val {link}}.")
    ))
  )
}

# Wrap a per-observation function call to keep return shape [ndraws x nobs]
# regardless of the underlying density signature.
.apply_log_density <- function(linpred, y, fn) {
  out <- matrix(NA_real_, nrow = nrow(linpred), ncol = ncol(linpred))
  for (j in seq_along(y)) {
    out[, j] <- fn(y[j], j)
  }
  out
}

log_lik_gaussian <- function(linpred, link, y, family_pars, trials) {
  mu <- .linkinv(linpred, link)
  sigma <- family_pars$sigma
  .apply_log_density(linpred, y, function(yj, j) {
    stats::dnorm(yj, mean = mu[, j], sd = sigma[, j], log = TRUE)
  })
}

log_lik_student <- function(linpred, link, y, family_pars, trials) {
  mu <- .linkinv(linpred, link)
  sigma <- family_pars$sigma
  nu <- family_pars$nu
  .apply_log_density(linpred, y, function(yj, j) {
    # location-scale Student t: standardise then apply dt
    z <- (yj - mu[, j]) / sigma[, j]
    stats::dt(z, df = nu[, j], log = TRUE) - log(sigma[, j])
  })
}

log_lik_lognormal <- function(linpred, link, y, family_pars, trials) {
  mu <- linpred  # meanlog lives on the linpred scale regardless of link
  sigma <- family_pars$sigma
  .apply_log_density(linpred, y, function(yj, j) {
    stats::dlnorm(yj, meanlog = mu[, j], sdlog = sigma[, j], log = TRUE)
  })
}

log_lik_gamma <- function(linpred, link, y, family_pars, trials) {
  mu <- .linkinv(linpred, link)
  shape <- family_pars$shape
  .apply_log_density(linpred, y, function(yj, j) {
    rate <- shape[, j] / mu[, j]
    stats::dgamma(yj, shape = shape[, j], rate = rate, log = TRUE)
  })
}

log_lik_weibull <- function(linpred, link, y, family_pars, trials) {
  mu <- .linkinv(linpred, link)
  shape <- family_pars$shape
  scale <- mu / gamma(1 + 1 / shape)
  .apply_log_density(linpred, y, function(yj, j) {
    stats::dweibull(yj, shape = shape[, j], scale = scale[, j], log = TRUE)
  })
}

log_lik_exponential <- function(linpred, link, y, family_pars, trials) {
  mu <- .linkinv(linpred, link)
  .apply_log_density(linpred, y, function(yj, j) {
    stats::dexp(yj, rate = 1 / mu[, j], log = TRUE)
  })
}

log_lik_beta <- function(linpred, link, y, family_pars, trials) {
  mu <- .linkinv(linpred, link)
  phi <- family_pars$phi
  .apply_log_density(linpred, y, function(yj, j) {
    a <- mu[, j] * phi[, j]
    b <- (1 - mu[, j]) * phi[, j]
    stats::dbeta(yj, shape1 = a, shape2 = b, log = TRUE)
  })
}

log_lik_bernoulli <- function(linpred, link, y, family_pars, trials) {
  prob <- .linkinv(linpred, link)
  .apply_log_density(linpred, y, function(yj, j) {
    stats::dbinom(yj, size = 1L, prob = prob[, j], log = TRUE)
  })
}

log_lik_binomial <- function(linpred, link, y, family_pars, trials) {
  prob <- .linkinv(linpred, link)
  trials <- as.integer(trials)
  .apply_log_density(linpred, y, function(yj, j) {
    stats::dbinom(yj, size = trials[j], prob = prob[, j], log = TRUE)
  })
}

log_lik_beta_binomial <- function(linpred, link, y, family_pars, trials) {
  if (!requireNamespace("extraDistr", quietly = TRUE)) {
    stop(insight::format_error(
      "Package {.pkg extraDistr} is required for beta_binomial log_lik."
    ))
  }
  mu <- .linkinv(linpred, link)
  phi <- family_pars$phi
  trials <- as.integer(trials)
  .apply_log_density(linpred, y, function(yj, j) {
    extraDistr::dbbinom(
      yj, size = trials[j],
      alpha = mu[, j] * phi[, j],
      beta = (1 - mu[, j]) * phi[, j],
      log = TRUE
    )
  })
}

log_lik_poisson <- function(linpred, link, y, family_pars, trials) {
  mu <- .linkinv(linpred, link)
  .apply_log_density(linpred, y, function(yj, j) {
    stats::dpois(yj, lambda = mu[, j], log = TRUE)
  })
}

log_lik_negbinomial <- function(linpred, link, y, family_pars, trials) {
  mu <- .linkinv(linpred, link)
  shape <- family_pars$shape
  .apply_log_density(linpred, y, function(yj, j) {
    stats::dnbinom(yj, mu = mu[, j], size = shape[, j], log = TRUE)
  })
}

log_lik_geometric <- function(linpred, link, y, family_pars, trials) {
  mu <- .linkinv(linpred, link)
  # Geometric is NB with size = 1
  .apply_log_density(linpred, y, function(yj, j) {
    stats::dnbinom(yj, mu = mu[, j], size = 1, log = TRUE)
  })
}

# Hurdle Poisson: P(Y=0) = hu; P(Y=k>0) = (1 - hu) * dpois(k|mu) /
# (1 - exp(-mu))
log_lik_hurdle_poisson <- function(linpred, link, y, family_pars, trials) {
  mu <- .linkinv(linpred, link)
  hu <- family_pars$hu
  .apply_log_density(linpred, y, function(yj, j) {
    if (yj == 0) {
      log(hu[, j])
    } else {
      log1p(-hu[, j]) +
        stats::dpois(yj, lambda = mu[, j], log = TRUE) -
        log1p(-exp(-mu[, j]))
    }
  })
}

# Hurdle NegBin: P(Y=0) = hu; P(Y=k>0) = (1 - hu) * dnbinom(k|mu, shape) /
# (1 - P(Y_NB = 0))
log_lik_hurdle_negbinomial <- function(linpred, link, y, family_pars, trials) {
  mu <- .linkinv(linpred, link)
  hu <- family_pars$hu
  shape <- family_pars$shape
  .apply_log_density(linpred, y, function(yj, j) {
    if (yj == 0) {
      log(hu[, j])
    } else {
      p0 <- stats::dnbinom(0, mu = mu[, j], size = shape[, j])
      log1p(-hu[, j]) +
        stats::dnbinom(yj, mu = mu[, j], size = shape[, j], log = TRUE) -
        log1p(-p0)
    }
  })
}

# Hurdle Gamma: P(Y=0) = hu; P(Y>0) density = (1 - hu) * dgamma(y|shape, rate)
log_lik_hurdle_gamma <- function(linpred, link, y, family_pars, trials) {
  mu <- .linkinv(linpred, link)
  hu <- family_pars$hu
  shape <- family_pars$shape
  .apply_log_density(linpred, y, function(yj, j) {
    if (yj == 0) {
      log(hu[, j])
    } else {
      log1p(-hu[, j]) +
        stats::dgamma(yj, shape = shape[, j],
                      rate = shape[, j] / mu[, j], log = TRUE)
    }
  })
}

log_lik_hurdle_lognormal <- function(linpred, link, y, family_pars, trials) {
  mu <- linpred
  hu <- family_pars$hu
  sigma <- family_pars$sigma
  .apply_log_density(linpred, y, function(yj, j) {
    if (yj == 0) {
      log(hu[, j])
    } else {
      log1p(-hu[, j]) +
        stats::dlnorm(yj, meanlog = mu[, j], sdlog = sigma[, j],
                      log = TRUE)
    }
  })
}

# Zero-inflated Poisson: P(Y=0) = zi + (1 - zi) * exp(-mu);
# P(Y=k>0) = (1 - zi) * dpois(k|mu)
log_lik_zero_inflated_poisson <- function(linpred, link, y,
                                          family_pars, trials) {
  mu <- .linkinv(linpred, link)
  zi <- family_pars$zi
  .apply_log_density(linpred, y, function(yj, j) {
    if (yj == 0) {
      # log(zi + (1-zi) * exp(-mu)) computed with log-sum-exp
      log_zi <- log(zi[, j])
      log_pois0 <- log1p(-zi[, j]) + stats::dpois(0, lambda = mu[, j],
                                                  log = TRUE)
      pmax <- pmax(log_zi, log_pois0)
      pmax + log(exp(log_zi - pmax) + exp(log_pois0 - pmax))
    } else {
      log1p(-zi[, j]) + stats::dpois(yj, lambda = mu[, j], log = TRUE)
    }
  })
}

log_lik_zero_inflated_negbinomial <- function(linpred, link, y,
                                              family_pars, trials) {
  mu <- .linkinv(linpred, link)
  zi <- family_pars$zi
  shape <- family_pars$shape
  .apply_log_density(linpred, y, function(yj, j) {
    if (yj == 0) {
      log_zi <- log(zi[, j])
      log_nb0 <- log1p(-zi[, j]) +
        stats::dnbinom(0, mu = mu[, j], size = shape[, j], log = TRUE)
      pmax <- pmax(log_zi, log_nb0)
      pmax + log(exp(log_zi - pmax) + exp(log_nb0 - pmax))
    } else {
      log1p(-zi[, j]) +
        stats::dnbinom(yj, mu = mu[, j], size = shape[, j], log = TRUE)
    }
  })
}

log_lik_zero_inflated_binomial <- function(linpred, link, y,
                                           family_pars, trials) {
  prob <- .linkinv(linpred, link)
  zi <- family_pars$zi
  trials <- as.integer(trials)
  .apply_log_density(linpred, y, function(yj, j) {
    if (yj == 0) {
      log_zi <- log(zi[, j])
      log_bin0 <- log1p(-zi[, j]) +
        stats::dbinom(0, size = trials[j], prob = prob[, j], log = TRUE)
      pmax <- pmax(log_zi, log_bin0)
      pmax + log(exp(log_zi - pmax) + exp(log_bin0 - pmax))
    } else {
      log1p(-zi[, j]) +
        stats::dbinom(yj, size = trials[j], prob = prob[, j], log = TRUE)
    }
  })
}

log_lik_zero_inflated_beta <- function(linpred, link, y,
                                       family_pars, trials) {
  mu <- .linkinv(linpred, link)
  zi <- family_pars$zi
  phi <- family_pars$phi
  .apply_log_density(linpred, y, function(yj, j) {
    if (yj == 0) {
      log(zi[, j])
    } else {
      a <- mu[, j] * phi[, j]
      b <- (1 - mu[, j]) * phi[, j]
      log1p(-zi[, j]) + stats::dbeta(yj, shape1 = a, shape2 = b, log = TRUE)
    }
  })
}

# Ordinal cumulative model on a 1D linpred — uses thresholds + disc from
# posterior draws. brms parameterisation: P(Y <= k) = pnorm(thres[k] - eta * disc)
# for probit link, or plogis for logit. Returns [ndraws x nobs] log densities.
log_lik_cumulative <- function(linpred, link, y, family_pars, trials) {
  if (is.null(family_pars$thres) || is.null(family_pars$disc)) {
    stop(insight::format_error(
      "Ordinal log_lik requires {.field thres} and {.field disc} draws."
    ))
  }
  thres <- family_pars$thres  # [ndraws x ncat-1]
  disc <- family_pars$disc    # [ndraws x nobs] (scalar broadcast)
  link_cdf <- switch(link,
    logit = stats::plogis,
    probit = stats::pnorm,
    cloglog = function(x) 1 - exp(-exp(x)),
    stop(insight::format_error(
      cli::format_inline("Unsupported ordinal link: {.val {link}}.")
    ))
  )
  ncat <- ncol(thres) + 1L
  out <- matrix(NA_real_, nrow = nrow(linpred), ncol = ncol(linpred))
  for (j in seq_along(y)) {
    yj <- as.integer(y[j])
    eta_j <- linpred[, j] * disc[, j]
    if (yj == 1L) {
      out[, j] <- log(link_cdf(thres[, 1L] - eta_j))
    } else if (yj == ncat) {
      out[, j] <- log1p(-link_cdf(thres[, ncat - 1L] - eta_j))
    } else {
      lo <- link_cdf(thres[, yj - 1L] - eta_j)
      hi <- link_cdf(thres[, yj] - eta_j)
      out[, j] <- log(hi - lo)
    }
  }
  out
}


#' Log-likelihood of a fitted mvgam model
#'
#' Method for the `stats::logLik` generic so that `AIC()` and `BIC()`
#' dispatch on `mvgam` objects. Returns the posterior mean of the
#' summed pointwise log-likelihood (across observations) as a numeric
#' scalar with the `df` and `nobs` attributes the [stats::AIC()] /
#' [stats::BIC()] machinery expects.
#'
#' @param object A fitted [mvgam][mvgam] object.
#' @param pointwise Logical. When `FALSE` (default) a single scalar
#'   `logLik` value is returned for compatibility with `AIC` and `BIC`.
#'   When `TRUE` the underlying `[ndraws x nobs]` pointwise matrix from
#'   [log_lik.mvgam()] is returned (matches the historical mvgam
#'   return shape).
#' @param ... Additional arguments forwarded to [log_lik.mvgam()]
#'   (e.g. `newdata`, `ndraws`, `process_error`).
#'
#' @return When `pointwise = FALSE`, a length-1 `logLik` object with
#'   `df` (number of sampled parameters, excluding sampler
#'   diagnostics) and `nobs` attributes. When `pointwise = TRUE`, a
#'   `[ndraws x nobs]` numeric matrix of pointwise log densities.
#'
#' @details
#' The scalar returned is the posterior mean of `rowSums(log_lik(object))`
#' across draws. For Bayesian state-space models AIC and BIC are coarse
#' instruments because every latent state inflates the parameter count;
#' [loo()] / [waic()] are usually the better model-selection tools and
#' are recommended in preference.
#'
#' @seealso [log_lik.mvgam()], [loo.mvgam()], [waic.mvgam()].
#'
#' @method logLik mvgam
#' @export
logLik.mvgam <- function(object, pointwise = FALSE, ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_flag(pointwise)

  ll_mat <- log_lik(object, ...)

  if (pointwise) {
    return(ll_mat)
  }

  per_draw_sum <- rowSums(ll_mat)
  ll_scalar <- mean(per_draw_sum)

  diag_pars <- c(
    "lp__", "lprior", "accept_stat__", "stepsize__", "treedepth__",
    "n_leapfrog__", "divergent__", "energy__"
  )
  df <- length(setdiff(variables(object), diag_pars))

  structure(
    ll_scalar,
    df = df,
    nobs = nobs(object),
    class = "logLik"
  )
}
