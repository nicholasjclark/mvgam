#' Posterior Predictive Distribution for mvgam Models
#'
#' @description
#' Generate posterior predictive samples from fitted mvgam models with
#' observation-level noise. Combines expected values with family-specific
#' random draws.
#'
#' @name posterior_predict
NULL


# ============ Truncation Helper Functions ============
#
# These functions reimplement brms' internal truncation sampling logic
# (rcontinuous, rdiscrete, check_discrete_trunc_bounds). Reimplementation
# is necessary because:
#
# 1. brms does NOT export these functions (no @export tag)
# 2. Using brms:::rcontinuous() violates R package best practices
# 3. brms maintainers have indicated these are internal implementation
#    details subject to change without notice
#
# The implementations follow brms' approach:
# - Inverse CDF transformation for continuous distributions
# - Rejection sampling for discrete distributions
# - Warning threshold of 1% for invalid samples
#
# Verified against brms version 2.22.9 as reference implementation.


#' Extract Truncation Bounds for Posterior Prediction
#'
#' Extracts constant lower and upper truncation bounds from fitted mvgam
#' model. Variable bounds (observation-specific) are not yet supported.
#'
#' @param object A fitted mvgam object
#' @param nobs Integer; number of observations for bounds replication
#'
#' @return Named list with two elements: `lb` (numeric vector of length
#'   nobs or NULL if no lower bound) and `ub` (numeric vector of length
#'   nobs or NULL if no upper bound). Both NULL if model has no truncation.
#'
#' @details
#' Truncation bounds are specified via `trunc()` in the model formula,
#' e.g., `y | trunc(lb = 0, ub = 10) ~ x`. Bounds are stored in standata.
#'
#' Constant bounds: All observations share the same lb/ub values.
#' Variable bounds: Observation-specific bounds from data columns.
#' Variable bounds trigger a warning and are not yet supported.
#'
#' @noRd
extract_truncation_bounds <- function(object, nobs) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_int(nobs, lower = 1)

  if (is.null(object$standata)) {
    return(list(lb = NULL, ub = NULL))
  }

  lb <- object$standata$lb
  ub <- object$standata$ub

  if (is.null(lb) && is.null(ub)) {
    return(list(lb = NULL, ub = NULL))
  }

  # Validate extracted bounds are numeric
  if (!is.null(lb)) {
    checkmate::assert_numeric(lb, any.missing = FALSE)
  }

  if (!is.null(ub)) {
    checkmate::assert_numeric(ub, any.missing = FALSE)
  }

  # Validate no mixed finite/infinite within bounds vector
  if (!is.null(lb)) {
    has_finite <- any(is.finite(lb))
    has_infinite <- any(!is.finite(lb))
    if (has_finite && has_infinite) {
      stop(insight::format_error(c(
        "Mixed finite and infinite lower bounds not supported.",
        i = "All lb values must be either finite or all infinite (-Inf)."
      )))
    }
  }
  if (!is.null(ub)) {
    has_finite <- any(is.finite(ub))
    has_infinite <- any(!is.finite(ub))
    if (has_finite && has_infinite) {
      stop(insight::format_error(c(
        "Mixed finite and infinite upper bounds not supported.",
        i = "All ub values must be either finite or all infinite (Inf)."
      )))
    }
  }

  # Fail-fast: validate lb < ub for constant bounds
  if (!is.null(lb) && !is.null(ub)) {
    finite_lb <- lb[is.finite(lb)]
    finite_ub <- ub[is.finite(ub)]
    if (length(finite_lb) > 0 && length(finite_ub) > 0) {
      lb_vals <- unique(finite_lb)
      ub_vals <- unique(finite_ub)
      if (length(lb_vals) == 1 && length(ub_vals) == 1 && lb_vals >= ub_vals) {
        stop(insight::format_error(c(
          cli::format_inline(
            paste0(
              "Invalid truncation bounds: {.field lb} (", lb_vals,
              ") must be less than {.field ub} (", ub_vals, ")."
            )
          ),
          i = "Truncation requires lb < ub to define a valid bounded region."
        )))
      }
    }
  }

  # Process lower bound
  if (!is.null(lb)) {
    finite_lb <- lb[is.finite(lb)]
    if (length(finite_lb) == 0) {
      lb <- NULL
    } else if (length(unique(finite_lb)) == 1) {
      lb <- rep(unique(finite_lb), nobs)
    } else {
      if (!identical(Sys.getenv("TESTTHAT"), "true")) {
        rlang::warn(
          c(
            "Variable truncation bounds not yet supported for predictions.",
            "i" = "Truncation will be ignored for posterior_predict."
          ),
          .frequency = "once",
          .frequency_id = "mvgam_variable_trunc_lb"
        )
      }
      lb <- NULL
    }
  }

  # Process upper bound
  if (!is.null(ub)) {
    finite_ub <- ub[is.finite(ub)]
    if (length(finite_ub) == 0) {
      ub <- NULL
    } else if (length(unique(finite_ub)) == 1) {
      ub <- rep(unique(finite_ub), nobs)
    } else {
      if (!identical(Sys.getenv("TESTTHAT"), "true")) {
        rlang::warn(
          c(
            "Variable truncation bounds not yet supported for predictions.",
            "i" = "Truncation will be ignored for posterior_predict."
          ),
          .frequency = "once",
          .frequency_id = "mvgam_variable_trunc_ub"
        )
      }
      ub <- NULL
    }
  }

  list(lb = lb, ub = ub)
}


#' Determine if Family Uses Integer Values
#'
#' Returns TRUE for count/discrete families that produce integer samples.
#'
#' @param family_name Character; distribution family name
#'
#' @return Logical; TRUE if family produces integer samples
#'
#' @noRd
family_uses_integers <- function(family_name) {
  checkmate::assert_string(family_name)

  integer_families <- c(
    "poisson", "negbinomial", "negbinomial2", "geometric",
    "binomial", "beta_binomial", "bernoulli",
    "zero_inflated_poisson", "zero_inflated_negbinomial",
    "zero_inflated_binomial", "zero_inflated_beta_binomial",
    "hurdle_poisson", "hurdle_negbinomial", "hurdle_cumulative",
    "discrete_weibull", "com_poisson", "beta_nb", "com_binomial"
  )

  family_name %in% integer_families
}


#' Apply Truncation to Samples Matrix
#'
#' Applies truncation bounds to samples by resampling out-of-bounds values
#' or clamping to bounds when resampling fails.
#'
#' @param samples Matrix `\\[ndraws x nobs\\]` or vector of samples
#' @param family_name Character; distribution family name
#' @param lb Numeric; lower bound(s) - scalar or vector of length nobs
#' @param ub Numeric; upper bound(s) - scalar or vector of length nobs
#' @param ntrys Integer; rejection sampling attempts for discrete families
#' @param ndraws Integer; number of draws (needed if samples is vector)
#' @param nobs Integer; number of observations (needed if samples is vector)
#'
#' @return Matrix `\\[ndraws x nobs\\]` of truncated samples
#'
#' @details
#' The function works column-by-column to handle observation-specific bounds.
#' For efficiency, columns with no out-of-bounds samples are skipped.
#'
#' Uses inverse CDF for continuous distributions when possible, falls back
#' to rejection sampling. For discrete distributions, uses rejection sampling.
#'
#' When resampling fails (after ntrys for rejection), samples are clamped
#' to bounds with a warning if >1% of samples were clamped.
#'
#' @noRd
apply_truncation <- function(samples, lb, ub, ntrys, ndraws, nobs,
                             redraw, spec = NULL, discrete = FALSE) {
  checkmate::assert_numeric(lb, null.ok = TRUE)
  checkmate::assert_numeric(ub, null.ok = TRUE)
  checkmate::assert_int(ntrys, lower = 1)
  checkmate::assert_int(ndraws, lower = 1)
  checkmate::assert_int(nobs, lower = 1)
  checkmate::assert_function(redraw)

  # Convert vector to matrix for consistent handling
  is_vector <- !is.matrix(samples)
  if (is_vector) {
    samples <- matrix(samples, nrow = ndraws, ncol = nobs)
  }

  # Handle NULL bounds
  if (is.null(lb) || all(is.infinite(lb) & lb < 0)) {
    lb <- rep(-Inf, nobs)
  }
  if (is.null(ub) || all(is.infinite(ub) & ub > 0)) {
    ub <- rep(Inf, nobs)
  }
  if (length(lb) == 1) lb <- rep(lb, nobs)
  if (length(ub) == 1) ub <- rep(ub, nobs)

  lb_mat <- matrix(lb, nrow = nrow(samples), ncol = nobs, byrow = TRUE)
  ub_mat <- matrix(ub, nrow = nrow(samples), ncol = nobs, byrow = TRUE)
  outside <- function(x) !is.na(x) & (x < lb_mat | x > ub_mat)

  invalid <- outside(samples)
  total_samples <- ndraws * nobs

  # Where the family's own quantile function is known, an
  # out-of-bounds draw is replaced exactly, by inverting the
  # distribution restricted to the bounds. Nothing is rejected and
  # nothing needs clamping.
  #
  # An earlier version resolved the distribution by name and called
  # it with no parameters at all, so a replacement came from the
  # standard member of the family rather than from the observation's
  # own predictive: on a gaussian centred at 50 truncated below at
  # zero, every replaced draw landed inside [0, 2].
  if (any(invalid) && !is.null(spec)) {
    exact <- truncated_dist_draws(spec, invalid, lb, ub, discrete)
    if (!is.null(exact)) {
      usable <- invalid & is.finite(exact)
      samples[usable] <- exact[usable]
      invalid <- invalid & !usable
    }
  }

  # A family with no quantile function, such as a Tweedie or a
  # COM-binomial, is redrawn from itself instead, and whatever will
  # not fall inside the bounds is clamped onto them.
  for (i in seq_len(ntrys)) {
    if (!any(invalid)) break
    fresh <- redraw()
    if (!is.matrix(fresh)) {
      fresh <- matrix(fresh, nrow = nrow(samples), ncol = nobs)
    }
    usable <- invalid & !is.na(fresh) & !outside(fresh)
    samples[usable] <- fresh[usable]
    invalid <- invalid & !usable
  }

  # Clamp whatever no redraw could place inside the bounds.
  n_clamped <- sum(invalid)
  if (n_clamped > 0) {
    too_low <- invalid & samples < lb_mat
    too_high <- invalid & samples > ub_mat
    samples[too_low] <- lb_mat[too_low]
    samples[too_high] <- ub_mat[too_high]
  }

  # Warn if significant clamping occurred (>1% matches brms threshold)
  clamp_frac <- n_clamped / total_samples
  if (clamp_frac > 0.01) {
    if (!identical(Sys.getenv("TESTTHAT"), "true")) {
      rlang::warn(
        c(
          paste0(
            round(clamp_frac * 100, 1), "% of samples (",
            n_clamped, " of ", total_samples, ") were clamped."
          ),
          "i" = paste(
            "This may indicate the truncation region is too narrow",
            "relative to the posterior predictive distribution."
          )
        ),
        .frequency = "once",
        .frequency_id = "mvgam_truncation_clamp"
      )
    }
  }

  # Convert back to vector if input was vector
  if (is_vector) {
    samples <- as.vector(samples)
  }

  samples
}


#' Sample from a Probability Distribution
#'
#' Internal helper that samples from a specified distribution using
#' appropriate parameters. Used to add observation noise to predicted
#' expected values.
#'
#' @param family_name Character name of the distribution (e.g., "poisson")
#' @param ndraws Integer; number of draws per observation
#' @param epred Matrix `\\[ndraws x nobs\\]` of expected values
#' @param sigma Optional matrix `\\[ndraws x nobs\\]` of scale/dispersion.
#'   Required for: gaussian, student, lognormal, hurdle_lognormal,
#'   skew_normal, exgaussian, gen_extreme_value, asym_laplace.
#' @param phi Optional matrix `\\[ndraws x nobs\\]` of precision parameter.
#'   Required for: beta, beta_binomial, zero_inflated_beta,
#'   zero_inflated_beta_binomial, zero_one_inflated_beta.
#' @param shape Optional matrix `\\[ndraws x nobs\\]` of shape parameters.
#'   Required for: gamma, negbinomial, weibull, frechet, inverse.gaussian,
#'   hurdle_negbinomial, hurdle_gamma, discrete_weibull, com_poisson
#' @param nu Optional matrix `\\[ndraws x nobs\\]` of degrees of freedom.
#'   Required for: student
#' @param trials Optional vector of trial counts (length 1 or nobs).
#'   Required for: binomial, beta_binomial, zero_inflated_binomial,
#'   zero_inflated_beta_binomial
#' @param hu Optional matrix `\\[ndraws x nobs\\]` of hurdle probability.
#'   Required for: hurdle_poisson, hurdle_negbinomial, hurdle_gamma,
#'   hurdle_lognormal
#' @param zi Optional matrix `\\[ndraws x nobs\\]` of zero-inflation probability.
#'   Required for: zero_inflated_poisson, zero_inflated_negbinomial,
#'   zero_inflated_binomial, zero_inflated_beta_binomial, zero_inflated_beta,
#'   zero_inflated_asym_laplace
#' @param zoi Optional matrix `\\[ndraws x nobs\\]` of zero-one inflation.
#'   Required for: zero_one_inflated_beta (probability of boundary value)
#' @param coi Optional matrix `\\[ndraws x nobs\\]` of conditional one-inflation.
#'   Required for: zero_one_inflated_beta (P(Y=1 | Y in {0,1}))
#' @param alpha Optional matrix `\\[ndraws x nobs\\]` of skewness parameters.
#'   Required for: skew_normal
#' @param ndt Optional matrix `\\[ndraws x nobs\\]` of non-decision time (>=0).
#'   Required for: shifted_lognormal, exgaussian
#' @param xi Optional matrix `\\[ndraws x nobs\\]` of shape/tail parameters.
#'   Required for: gen_extreme_value
#' @param quantile Optional matrix `\\[ndraws x nobs\\]` of quantile values (0,1).
#'   Required for: asym_laplace, zero_inflated_asym_laplace
#' @param kappa Optional matrix `\\[ndraws x nobs\\]` of concentration (>0).
#'   Required for: von_mises
#' @param beta Optional matrix `\\[ndraws x nobs\\]` of rate parameters (>0).
#'   Required for: exgaussian
#' @param bs Optional matrix `\\[ndraws x nobs\\]` of boundary separation (>0).
#'   Required for: wiener
#' @param bias Optional matrix `\\[ndraws x nobs\\]` of starting point bias (0-1).
#'   Required for: wiener
#' @param disc Optional matrix `\\[ndraws x nobs\\]` of discrimination (>0).
#'   Required for: hurdle_cumulative. Defaults to 1 if NULL.
#' @param thres Optional matrix \\[ndraws x nthres\\] of ordinal thresholds.
#'   Required for: hurdle_cumulative
#' @param link Character; link function for ordinal models.
#'   Required for: hurdle_cumulative. Default "logit".
#'
#' @return Vector of sampled values with length = length(epred). Values are
#'   integers for count families, doubles for continuous. Used internally
#'   by posterior_predict.mvgam() which reshapes to `\\[ndraws x nobs\\]` matrix.
#'
#' @noRd


# Apply the link's inverse CDF to the threshold offset arg = disc *
# (thres - eta). Vectorised over both draws (rows) and thresholds
# (columns) of thres.
ordinal_linkinv <- function(arg, link) {
  switch(link,
    logit = stats::plogis(arg),
    probit = stats::pnorm(arg),
    probit_approx = stats::pnorm(arg),
    cloglog = 1 - exp(-exp(arg)),
    cauchit = stats::pcauchy(arg),
    identity = arg,
    stop(insight::format_error(
      cli::format_inline("Unsupported ordinal link: {.val {link}}.")
    ))
  )
}

# Sample a category for each (draw, obs) under the cumulative family.
# Returns a length(eta) vector of integers in 1..ncat. eta is a
# [ndraws x nobs] matrix of link-scale linear predictors; thres is
# [ndraws x nthres]; disc is a [ndraws x nobs] matrix or scalar.
ordinal_sample <- function(eta, thres, disc = 1, link = "logit") {
  ndraws <- nrow(eta)
  nobs <- ncol(eta)
  ncat <- ncol(thres) + 1L
  out <- integer(length(eta))
  for (j in seq_len(nobs)) {
    disc_j <- if (is.matrix(disc)) disc[, j] else disc
    cdf <- matrix(NA_real_, nrow = ndraws, ncol = ncat)
    for (k in seq_len(ncat - 1L)) {
      cdf[, k] <- ordinal_linkinv(disc_j * (thres[, k] - eta[, j]), link)
    }
    cdf[, ncat] <- 1
    u <- stats::runif(ndraws)
    cats <- rep.int(ncat, ndraws)
    unmatched <- rep.int(TRUE, ndraws)
    for (k in seq_len(ncat - 1L)) {
      hit <- unmatched & u <= cdf[, k]
      cats[hit] <- k
      unmatched[hit] <- FALSE
    }
    out[((j - 1L) * ndraws + 1L):(j * ndraws)] <- cats
  }
  out
}


#' Draw beta-binomial variates in the mean-precision parameterisation
#'
#' `extraDistr::rbbinom()` takes the two Beta shapes, while brms and
#' mvgam both parameterise the family by a per-trial probability and a
#' precision. Converting in one place keeps the two samplers that need
#' it agreeing with `dbeta_binomial()` and with the Stan likelihood.
#'
#' @param n Number of variates
#' @param size Trial counts
#' @param mu Per-trial probability
#' @param phi Precision
#' @return Integer vector of length `n`
#'
#' @noRd
rbeta_binomial_draws <- function(n, size, mu, phi) {
  shapes <- beta_shapes(mu, phi)
  probs <- stats::rbeta(n, shapes$shape1, shapes$shape2)
  stats::rbinom(n, size = size, prob = probs)
}


sample_from_family <- function(family_name, ndraws, epred,
                               sigma = NULL, phi = NULL,
                               shape = NULL, nu = NULL,
                               trials = NULL, hu = NULL, zi = NULL,
                               zoi = NULL, coi = NULL,
                               alpha = NULL, ndt = NULL, xi = NULL,
                               quantile = NULL, kappa = NULL,
                               beta = NULL, bs = NULL, bias = NULL,
                               disc = NULL, thres = NULL,
                               mphi = NULL, mtheta = NULL,
                               mtail = NULL,
                               link = "logit",
                               lb = NULL, ub = NULL, ntrys = 5) {
  checkmate::assert_string(family_name)
  checkmate::assert_int(ndraws, lower = 1)
  checkmate::assert_matrix(epred)
  checkmate::assert_numeric(lb, null.ok = TRUE)
  checkmate::assert_numeric(ub, null.ok = TRUE)
  checkmate::assert_int(ntrys, lower = 1)

  has_truncation <- !is.null(lb) || !is.null(ub)
  nobs <- ncol(epred)

  # Named so truncation can ask the same family for another draw
  # rather than reaching for a second account of what the family is.
  draw_once <- function() switch(
    family_name,

    # ============ Continuous families ============

    "gaussian" = {
      checkmate::assert_matrix(sigma, nrows = ndraws, ncols = ncol(epred))
      stats::rnorm(length(epred), mean = epred, sd = sigma)
    },

    "student" = {
      checkmate::assert_matrix(sigma, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(nu, nrows = ndraws, ncols = ncol(epred))
      extraDistr::rlst(length(epred), df = nu, mu = epred, sigma = sigma)
    },

    "skew_normal" = {
      checkmate::assert_matrix(sigma, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(alpha, nrows = ndraws, ncols = ncol(epred))
      brms::rskew_normal(length(epred), mu = epred, sigma = sigma, alpha = alpha)
    },

    "exponential" = {
      # E[X] = 1/rate, so rate = 1/epred
      stats::rexp(length(epred), rate = 1 / epred)
    },

    "gamma" = {
      checkmate::assert_matrix(shape, nrows = ndraws, ncols = ncol(epred))
      # E[X] = shape * scale, so scale = epred / shape
      scale <- epred / shape
      stats::rgamma(length(epred), shape = shape, scale = scale)
    },

    "weibull" = {
      checkmate::assert_matrix(shape, nrows = ndraws, ncols = ncol(epred))
      # E[X] = scale * gamma(1 + 1/shape)
      # scale = epred / gamma(1 + 1/shape)
      scale <- epred / gamma(1 + 1 / shape)
      stats::rweibull(length(epred), shape = shape, scale = scale)
    },

    "frechet" = {
      checkmate::assert_matrix(shape, nrows = ndraws, ncols = ncol(epred))
      # E[X] = scale * gamma(1 - 1/shape)
      # scale = epred / gamma(1 - 1/shape)
      scale <- epred / gamma(1 - 1 / shape)
      extraDistr::rfrechet(length(epred), scale = scale, shape = shape)
    },

    "gen_extreme_value" = {
      checkmate::assert_matrix(sigma, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(xi, nrows = ndraws, ncols = ncol(epred))
      brms::rgen_extreme_value(length(epred), mu = epred, sigma = sigma, xi = xi)
    },

    "inverse.gaussian" = {
      checkmate::assert_matrix(shape, nrows = ndraws, ncols = ncol(epred))
      statmod::rinvgauss(length(epred), mean = epred, shape = shape)
    },

    "tweedie" = {
      # Compound Poisson-gamma; uses mgcv::rTweedie (already an
      # mvgam hard dependency) so no Suggests check is needed.
      # mphi and mtheta arrive as [ndraws x nobs] matrices from
      # the custom-family dpars.
      checkmate::assert_matrix(mphi, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(mtheta, nrows = ndraws, ncols = ncol(epred))
      mgcv::rTweedie(
        mu = as.numeric(epred),
        p = as.numeric(mtheta),
        phi = as.numeric(mphi)
      )
    },

    "beta_nb" = {
      # Beta negative binomial. `epred` is the mean mu; the Stan side
      # sets the Beta mixing scale to mu * mtail / shape so drawing
      # the success probability from Beta(1 + mtail, that scale) and
      # then a negative binomial reproduces the fitted distribution.
      checkmate::assert_matrix(shape, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(mtail, nrows = ndraws, ncols = ncol(epred))
      rbeta_nb_mvgam(
        n = length(epred),
        mu = as.numeric(epred),
        shape = as.numeric(shape),
        mtail = as.numeric(mtail)
      )
    },

    "com_binomial" = {
      # Conway-Maxwell-Binomial. `epred` here is the probability
      # `p = plogis(linpred)`, populated by `family$linkinv` in
      # the upstream pipeline. Reuses
      # `posterior_predict_com_binomial()` so the CMB drawing
      # logic lives in one place (same kernel as
      # `predict(type = "response")`).
      checkmate::assert_matrix(nu, nrows = ndraws,
                                ncols = ncol(epred))
      checkmate::assert_numeric(trials, lower = 0L,
                                 len = ncol(epred))
      # `epred` is on probability scale (p); rebuild linpred via
      # qlogis so `posterior_predict_com_binomial` (which expects
      # linpred + link) can apply its own logit inversion. Keeps
      # the helper-facing contract identical to
      # `log_lik_com_binomial` rather than introducing a second
      # p-scale entry point.
      linpred_cmb <- stats::qlogis(epred)
      posterior_predict_com_binomial(
        linpred     = linpred_cmb,
        link        = "logit",
        family_pars = list(nu = nu),
        trials      = trials
      )
    },

    "exgaussian" = {
      checkmate::assert_matrix(sigma, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(beta, nrows = ndraws, ncols = ncol(epred))
      brms::rexgaussian(length(epred), mu = epred, sigma = sigma, beta = beta)
    },

    "lognormal" = {
      checkmate::assert_matrix(sigma, nrows = ndraws, ncols = ncol(epred))
      # `epred` here is the inverse link of the predictor, which for a
      # lognormal is `meanlog` itself and not `E[Y]`. brms samples from
      # the same parameter; correcting it to the mean would both shift
      # the draws and produce NaN wherever `meanlog` is not positive.
      stats::rlnorm(length(epred), meanlog = epred, sdlog = sigma)
    },

    "shifted_lognormal" = {
      checkmate::assert_matrix(sigma, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(ndt, nrows = ndraws, ncols = ncol(epred))
      # As for the lognormal, `epred` is `meanlog` rather than `E[Y]`,
      # so it is the shift's own parameter and needs no correction.
      brms::rshifted_lnorm(
        length(epred),
        meanlog = epred,
        sdlog = sigma,
        shift = ndt
      )
    },

    "beta" = {
      checkmate::assert_matrix(phi, nrows = ndraws, ncols = ncol(epred))
      shapes <- beta_shapes(epred, phi)
      shape1 <- shapes$shape1
      shape2 <- shapes$shape2
      stats::rbeta(length(epred), shape1 = shape1, shape2 = shape2)
    },

    "von_mises" = {
      checkmate::assert_matrix(kappa, nrows = ndraws, ncols = ncol(epred))
      brms::rvon_mises(length(epred), mu = epred, kappa = kappa)
    },

    "asym_laplace" = {
      checkmate::assert_matrix(sigma, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(quantile, nrows = ndraws, ncols = ncol(epred))
      brms::rasym_laplace(
        length(epred),
        mu = epred,
        sigma = sigma,
        quantile = quantile
      )
    },

    # ============ Count families (discrete) ============

    "poisson" = {
      stats::rpois(length(epred), lambda = epred)
    },

    "negbinomial" = {
      checkmate::assert_matrix(shape, nrows = ndraws, ncols = ncol(epred))
      stats::rnbinom(length(epred), mu = epred, size = shape)
    },

    "negbinomial2" = {
      checkmate::assert_matrix(sigma, nrows = ndraws, ncols = ncol(epred))
      # sigma is sqrt(1/shape), so shape = 1/sigma^2
      shape <- 1 / (sigma^2)
      stats::rnbinom(length(epred), mu = epred, size = shape)
    },

    "geometric" = {
      # Geometric is negbinomial with shape = 1
      stats::rnbinom(length(epred), mu = epred, size = 1)
    },

    "discrete_weibull" = {
      checkmate::assert_matrix(shape, nrows = ndraws, ncols = ncol(epred))
      rdiscrete_weibull_mvgam(length(epred), mu = epred, shape = shape)
    },

    "com_poisson" = {
      checkmate::assert_matrix(shape, nrows = ndraws, ncols = ncol(epred))
      rcom_poisson_mvgam(length(epred), mu = epred, shape = shape)
    },

    # ============ Binomial families ============

    "binomial" = {
      checkmate::assert_numeric(trials, min.len = 1)
      # epred from linkinv is probability (0-1), use directly as prob
      # (matches brms behavior in posterior_predict_binomial)
      if (length(trials) == 1) {
        stats::rbinom(length(epred), size = trials, prob = epred)
      } else {
        # Different trial counts per observation: sample column-by-column
        nobs <- ncol(epred)
        result <- matrix(NA_integer_, nrow = ndraws, ncol = nobs)
        for (j in seq_len(nobs)) {
          result[, j] <- stats::rbinom(
            ndraws,
            size = trials[j],
            prob = epred[, j]
          )
        }
        result
      }
    },

    "beta_binomial" = {
      checkmate::assert_numeric(trials, min.len = 1)
      checkmate::assert_matrix(phi, nrows = ndraws, ncols = ncol(epred))
      # `epred` is the inverse link of the predictor, so it is already
      # the per-trial probability the sampler wants, as it is for the
      # plain binomial above.
      rbeta_binomial_draws(length(epred), size = trials, mu = epred,
                           phi = phi)
    },

    "bernoulli" = {
      stats::rbinom(length(epred), size = 1, prob = epred)
    },

    # ============ Zero-inflated families ============

    "zero_inflated_poisson" = {
      checkmate::assert_matrix(zi, nrows = ndraws, ncols = ncol(epred))
      tmp <- stats::runif(length(epred))
      ifelse(tmp < zi, 0L, stats::rpois(length(epred), lambda = epred))
    },

    "zero_inflated_negbinomial" = {
      checkmate::assert_matrix(zi, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(shape, nrows = ndraws, ncols = ncol(epred))
      tmp <- stats::runif(length(epred))
      ifelse(tmp < zi, 0L, stats::rnbinom(length(epred), mu = epred, size = shape))
    },

    "zero_inflated_binomial" = {
      checkmate::assert_numeric(trials, min.len = 1)
      checkmate::assert_matrix(zi, nrows = ndraws, ncols = ncol(epred))
      tmp <- stats::runif(length(epred))
      ifelse(tmp < zi, 0L,
             stats::rbinom(length(epred), size = trials, prob = epred))
    },

    "zero_inflated_beta_binomial" = {
      checkmate::assert_numeric(trials, min.len = 1)
      checkmate::assert_matrix(zi, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(phi, nrows = ndraws, ncols = ncol(epred))
      draws <- rbeta_binomial_draws(length(epred), size = trials,
                                    mu = epred, phi = phi)
      tmp <- stats::runif(length(epred))
      draws[tmp < zi] <- 0L
      draws
    },

    "zero_inflated_beta" = {
      checkmate::assert_matrix(zi, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(phi, nrows = ndraws, ncols = ncol(epred))
      shapes <- beta_shapes(epred, phi)
      shape1 <- shapes$shape1
      shape2 <- shapes$shape2
      tmp <- stats::runif(length(epred))
      ifelse(tmp < zi, 0, stats::rbeta(length(epred), shape1 = shape1,
                                        shape2 = shape2))
    },

    "zero_one_inflated_beta" = {
      checkmate::assert_matrix(zoi, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(coi, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(phi, nrows = ndraws, ncols = ncol(epred))
      # zoi = P(Y in {0,1}), coi = P(Y=1 | Y in {0,1})
      tmp <- stats::runif(length(epred))
      one_or_zero <- stats::runif(length(epred))
      shapes <- beta_shapes(epred, phi)
      shape1 <- shapes$shape1
      shape2 <- shapes$shape2
      ifelse(
        tmp < zoi,
        ifelse(one_or_zero < coi, 1, 0),
        stats::rbeta(length(epred), shape1 = shape1, shape2 = shape2)
      )
    },

    "zero_inflated_asym_laplace" = {
      checkmate::assert_matrix(zi, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(sigma, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(quantile, nrows = ndraws, ncols = ncol(epred))
      tmp <- stats::runif(length(epred))
      base <- brms::rasym_laplace(
        length(epred),
        mu = epred,
        sigma = sigma,
        quantile = quantile
      )
      ifelse(tmp < zi, 0, base)
    },

    "wiener" = {
      # Wiener diffusion model: epred is drift rate (delta)
      # bs=boundary separation, ndt=non-decision time, bias=starting point
      checkmate::assert_matrix(bs, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(ndt, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(bias, nrows = ndraws, ncols = ncol(epred))
      brms::rwiener(
        length(epred),
        alpha = bs,
        tau = ndt,
        beta = bias,
        delta = epred
      )
    },

    # ============ Hurdle families ============

    "hurdle_poisson" = {
      checkmate::assert_matrix(hu, nrows = ndraws, ncols = ncol(epred))
      tmp <- stats::runif(length(epred))
      # Sample from truncated Poisson
      t_val <- -log(1 - stats::runif(length(epred)) * (1 - exp(-epred)))
      ifelse(tmp < hu, 0L, stats::rpois(length(epred), lambda = epred - t_val) + 1)
    },

    "hurdle_negbinomial" = {
      checkmate::assert_matrix(hu, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(shape, nrows = ndraws, ncols = ncol(epred))
      tmp <- stats::runif(length(epred))
      t_val <- -log(1 - stats::runif(length(epred)) * (1 - exp(-epred)))
      ifelse(tmp < hu, 0L, stats::rnbinom(length(epred), mu = epred - t_val, size = shape) + 1)
    },

    "hurdle_gamma" = {
      checkmate::assert_matrix(hu, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(shape, nrows = ndraws, ncols = ncol(epred))
      tmp <- stats::runif(length(epred))
      scale <- epred / shape
      ifelse(tmp < hu, 0, stats::rgamma(length(epred), shape = shape, scale = scale))
    },

    "hurdle_lognormal" = {
      checkmate::assert_matrix(hu, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(sigma, nrows = ndraws, ncols = ncol(epred))
      tmp <- stats::runif(length(epred))
      ifelse(tmp < hu, 0,
             stats::rlnorm(length(epred), meanlog = epred, sdlog = sigma))
    },

    "hurdle_cumulative" = {
      checkmate::assert_matrix(hu, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(thres)
      if (is.null(disc)) disc <- 1
      ordinal_samples <- ordinal_sample(eta = epred, thres = thres,
                                        disc = disc, link = link)
      tmp <- stats::runif(length(epred))
      ifelse(tmp < hu, 0L, ordinal_samples)
    },

    "cumulative" = {
      checkmate::assert_matrix(thres)
      if (is.null(disc)) disc <- 1
      ordinal_sample(eta = epred, thres = thres, disc = disc,
                     link = link)
    },

    "sratio" = ,
    "cratio" = ,
    "acat" = stop(insight::format_error(c(
      cli::format_inline(
        paste0("Posterior predictive sampling is unavailable for ",
               "family {.val {family_name}}.")
      ),
      i = cli::format_inline(
        "The supported ordinal family is {.val cumulative}."
      )
    ))),

    # ============ Unsupported families ============

    stop(insight::format_error(c(
      cli::format_inline(
        paste0("Posterior predictive sampling is unavailable for ",
               "family {.val {family_name}}.")
      ),
      i = paste0(
        "See ?mvgam_families for the families mvgam can draw from."
      )
    )))
  )

  samples <- draw_once()

  # Apply truncation if bounds are specified
  if (has_truncation) {
    # `epred` is already on the parameter's own scale, which is what
    # the spec calls `mu`, so the identity link is the honest one to
    # name here.
    samples <- apply_truncation(
      samples = samples,
      lb = lb,
      ub = ub,
      ntrys = ntrys,
      ndraws = ndraws,
      nobs = nobs,
      redraw = draw_once,
      spec = family_dist_spec(
        family_name, "identity", epred,
        list(sigma = sigma, shape = shape, phi = phi, nu = nu,
             mphi = mphi, mtheta = mtheta),
        trials
      ),
      discrete = family_uses_integers(family_name)
    )
  }

  samples
}


#' Get Distributional Parameters Required by Family
#'
#' Returns a character vector of distributional parameter names (dpars) that
#' a given family requires for posterior prediction sampling.
#'
#' @param family_name Character name of the family (e.g., "gaussian", "poisson")
#'
#' @return Character vector of parameter names. Possible values:
#'   \itemize{
#'     \item "sigma" - scale/dispersion (gaussian, student, lognormal, etc.)
#'     \item "shape" - shape parameter (gamma, negbinomial, weibull, etc.)
#'     \item "nu" - degrees of freedom (student-t)
#'     \item "phi" - precision parameter (beta families)
#'     \item "zi" - zero-inflation probability
#'     \item "hu" - hurdle probability
#'     \item "zoi" - zero-one inflation probability
#'     \item "coi" - conditional one-inflation probability
#'     \item "ndt" - non-decision time (shifted_lognormal, wiener, exgaussian)
#'     \item "xi" - tail parameter (gen_extreme_value)
#'     \item "quantile" - quantile parameter (asym_laplace)
#'     \item "kappa" - concentration (von_mises)
#'     \item "bs" - boundary separation (wiener)
#'     \item "bias" - starting point bias (wiener)
#'     \item "beta" - rate of exponential component (exgaussian)
#'   }
#'   Note: "trials" is NOT returned as it comes from data, not posterior.
#'
#' @details
#' Covers the brms families, whether or not
#' \code{sample_from_family()} can draw from them; one it cannot draw
#' from errors at sampling time rather than here. A family absent from
#' the mapping returns an empty character vector.
#'
#' @noRd
get_family_dpars <- function(family_name) {
  checkmate::assert_string(family_name)

  # Map families to required distributional parameters
  # Covers all brms families for future-proofing
  dpar_map <- list(
    # Continuous families
    gaussian = c("sigma"),
    student = c("sigma", "nu"),
    skew_normal = c("sigma", "alpha"),
    lognormal = c("sigma"),
    shifted_lognormal = c("sigma", "ndt"),
    gamma = c("shape"),
    weibull = c("shape"),
    frechet = c("shape"),
    inverse.gaussian = c("shape"),
    exgaussian = c("sigma", "beta"),
    beta = c("phi"),
    gen_extreme_value = c("sigma", "xi"),
    asym_laplace = c("sigma", "quantile"),
    wiener = c("bs", "ndt", "bias"),
    von_mises = c("kappa"),
    exponential = character(0),

    # Count families
    poisson = character(0),
    negbinomial = c("shape"),
    negbinomial2 = c("sigma"),
    geometric = character(0),
    discrete_weibull = c("shape"),
    com_poisson = c("shape"),

    # Binomial families (trials from data, not posterior)
    binomial = character(0),
    beta_binomial = c("phi"),
    bernoulli = character(0),

    # Zero-inflated families
    zero_inflated_poisson = c("zi"),
    zero_inflated_negbinomial = c("zi", "shape"),
    zero_inflated_binomial = c("zi"),
    zero_inflated_beta_binomial = c("zi", "phi"),
    zero_inflated_beta = c("zi", "phi"),
    zero_one_inflated_beta = c("zoi", "coi", "phi"),
    zero_inflated_asym_laplace = c("zi", "sigma", "quantile"),

    # Hurdle families
    hurdle_poisson = c("hu"),
    hurdle_negbinomial = c("hu", "shape"),
    hurdle_gamma = c("hu", "shape"),
    hurdle_lognormal = c("hu", "sigma"),
    hurdle_cumulative = c("hu", "disc"),

    # Custom mvgam families
    tweedie      = c("mphi", "mtheta"),
    beta_nb      = c("shape", "mtail"),
    com_binomial = c("nu"),
    nmix         = c("p"),
    occ          = c("p"),
    diri         = c("phi"),
    multi        = character(0),
    categ        = character(0),
    mvn          = c("Psi"),
    mvt          = c("Psi", "nu")
  )

  dpar_map[[family_name]] %||% character(0)
}


#' Resolve every distributional parameter a fitted family declares
#'
#' brms settles a distributional parameter one of two ways, in
#' `prepare_predictions.brmsframe()`: either the user gave it a formula
#' of its own, in which case it is a linear predictor evaluated per
#' observation, or it was sampled as a scalar (or one scalar per
#' series) and can be read straight off the draws. This does the same,
#' and is the single seam the prediction paths go through so the two
#' cases never have to be told apart at a call site.
#'
#' Names are the bare parameter names throughout. A multivariate fit
#' stores its parameters suffixed with the response, but that suffix is
#' an artefact of how brms writes the posterior, so it is applied on
#' the way in and stripped on the way out.
#'
#' @param object An `mvgam` model object
#' @param dpar_names Bare names of the parameters the family declares
#' @param ndraws Number of draws the prediction covers
#' @param nobs Number of rows the prediction covers
#' @param draw_ids Draw indices to keep, or `NULL` for all
#' @param newdata Data the prediction covers; `object$data` if `NULL`
#' @param resp Response name for a multivariate fit, or `NULL`
#' @return Named list of `[ndraws x nobs]` matrices, keyed by bare
#'   name. A parameter the posterior does not carry is left out.
#'
#' @noRd
resolve_family_pars <- function(object, dpar_names, ndraws, nobs,
                                draw_ids = NULL, newdata = NULL,
                                resp = NULL) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_character(dpar_names, min.len = 0)
  if (length(dpar_names) == 0) {
    return(list())
  }

  # The prediction paths resolve the count before they compute the
  # mean, so `draw_ids` normally arrives already settled. Resolving
  # again here is idempotent and makes the seam safe to call on its
  # own: the two branches below read from different extractors, and a
  # bare count would let each choose its own draws.
  draw_ids <- resolve_draw_ids(object, ndraws, draw_ids)

  predicted <- predicted_dpar_names(object, dpar_names, resp = resp)
  sampled <- setdiff(dpar_names, predicted)

  out <- list()
  for (dpar in predicted) {
    out[[dpar]] <- predicted_dpar_draws(
      object, dpar, nobs = nobs, ndraws = ndraws, draw_ids = draw_ids,
      newdata = newdata, resp = resp
    )
  }

  if (length(sampled) > 0) {
    suffixed <- if (!is.null(resp) && nzchar(resp)) {
      paste0(sampled, "_", resp)
    } else {
      sampled
    }
    found <- extract_dpars_from_stanfit(
      stanfit = object$fit,
      dpar_names = suffixed,
      ndraws = ndraws,
      nobs = nobs,
      draw_ids = draw_ids
    )
    # `extract_dpars_from_stanfit()` drops a parameter it cannot find,
    # so match names back rather than assuming the two lists align.
    names(found) <- sampled[match(names(found), suffixed)]
    out[names(found)] <- found
  }

  out[intersect(dpar_names, names(out))]
}


#' Names of the distributional parameters carrying their own formula
#'
#' A distributional submodel such as `sigma ~ x` or `nu ~ z` is
#' recorded by brms in `pforms` on the model formula. Such a parameter
#' has no scalar counterpart in the posterior: it is a linear predictor
#' evaluated per observation, so it has to be rebuilt from its
#' coefficients rather than read off the draws. Non-linear parameters
#' also live in `pforms`, so the names are intersected with the ones
#' the family actually recognises.
#'
#' @param object An `mvgam` model object
#' @param dpar_names Distributional parameters the family declares
#' @param resp Response name for a multivariate fit, or `NULL`
#' @return Character vector, possibly empty
#'
#' @noRd
predicted_dpar_names <- function(object, dpar_names, resp = NULL) {
  formula <- object$formula
  if (is.null(formula)) {
    return(character())
  }
  if (brms::is.mvbrmsformula(formula)) {
    forms <- formula$forms
    if (!is.null(resp) && nzchar(resp) && !is.null(forms[[resp]])) {
      forms <- forms[resp]
    }
    named <- unlist(lapply(forms, function(f) names(f$pforms)))
  } else {
    named <- names(formula$pforms)
  }
  intersect(dpar_names, named %||% character())
}


#' Draws of a distributional parameter that carries its own formula
#'
#' A parameter given a formula of its own is a linear predictor, not a
#' sampled scalar, so it is rebuilt from its coefficients and then put
#' on the parameter's own scale. `extract_component_linpred()` already
#' composes such a predictor from its parametric, smooth,
#' random-effect and Gaussian-process terms; this adds the inverse
#' link, so the result matches the scale a non-distributional fit
#' would have sampled the scalar on.
#'
#' @param object An `mvgam` model object
#' @param dpar Name of the distributional parameter
#' @param nobs Number of rows the prediction covers; `NULL` skips the
#'   conformity check for callers working at a different grain
#' @param draw_ids Draw indices to keep, or `NULL` for all
#' @param newdata Data the prediction covers; `object$data` if `NULL`
#' @param resp Response name for a multivariate fit, or `NULL`
#' @return A `[ndraws x nobs]` matrix
#'
#' @noRd
predicted_dpar_draws <- function(object, dpar, nobs = NULL,
                                 ndraws = NULL, draw_ids = NULL,
                                 newdata = NULL, resp = NULL) {
  linpred <- extract_component_linpred(
    mvgam_fit = object,
    newdata = newdata %||% mvgam_training_data(object),
    component = dpar,
    draw_ids = draw_ids,
    resp = resp
  )
  if (is.list(linpred) && !is.matrix(linpred)) {
    stop(insight::format_error(c(
      paste0(
        "Distributional parameter '", dpar,
        "' resolved to one predictor per response."
      ),
      i = "Scope the call to a single response with 'resp'."
    )))
  }
  out <- as.matrix(.linkinv(linpred, dpar_link(object$family, dpar)))
  if (!is.null(ndraws) && nrow(out) != ndraws) {
    stop(insight::format_error(c(
      paste0(
        "Distributional parameter '", dpar,
        "' was predicted from a different number of draws than the ",
        "linear predictor."
      ),
      x = paste0("Got ", nrow(out), " rows; expected ", ndraws, ".")
    )))
  }
  if (!is.null(nobs) && ncol(out) != nobs) {
    stop(insight::format_error(c(
      paste0(
        "Distributional parameter '", dpar, "' was predicted for a ",
        "different number of rows than the linear predictor covers."
      ),
      x = paste0("Got ", ncol(out), " columns; expected ", nobs, "."),
      i = paste0(
        "This happens when a covariate in the '", dpar, "' formula is ",
        "missing from the prediction data. Supply it in 'newdata'."
      )
    )))
  }
  out
}


#' Link of one distributional parameter
#'
#' brms keeps the mean's link on `family$link` and every other
#' parameter's on `family$link_<dpar>`. A family built by
#' `stats::gaussian()` or `stats::Gamma()` carries no per-parameter
#' links at all, so brms's own constructor is asked for the default it
#' would have applied. Guessing is not an option here: assuming an
#' identity where a log belongs hands back a parameter still on the
#' link scale, and every value derived from it is then wrong by an
#' exponential, with nothing to show for it. Custom families declare
#' every link when they are built and so never reach the fallback.
#'
#' @param family A family object
#' @param dpar Name of the distributional parameter
#' @return A single link name
#'
#' @noRd
dpar_link <- function(family, dpar) {
  link <- family[[paste0("link_", dpar)]]
  if (!is.null(link)) {
    return(link)
  }
  name <- tolower(resolve_family_name(family))
  link <- brms::brmsfamily(name)[[paste0("link_", dpar)]]
  if (is.null(link)) {
    stop(insight::format_error(c(
      paste0(
        "No link is recorded for distributional parameter '", dpar,
        "' of family '", name, "'."
      ),
      i = paste0(
        "The family needs a 'link_", dpar, "' entry before the ",
        "parameter can be put on its own scale."
      )
    )))
  }
  link
}


#' Extract Distributional Parameters from Stanfit Object
#'
#' Extracts posterior draws for distributional parameters (dpars) required by
#' family-specific sampling. Returns matrices in the format expected by
#' \code{sample_from_family()}.
#'
#' @param stanfit A stanfit or draws object containing posterior samples
#' @param dpar_names Character vector of distributional parameter names to
#'   extract (e.g., c("sigma", "shape", "zi"))
#' @param ndraws Integer; number of draws to extract
#' @param nobs Integer; number of observations for matrix dimensions
#' @param draw_ids Optional integer vector of specific draw indices to use.
#'   If provided, overrides ndraws.
#'
#' @return Named list of matrices, each with dimensions `\\[ndraws x nobs\\]`.
#'   Scalar parameters are broadcast to full `\\[ndraws x nobs\\]` matrices.
#'   Parameters not found in the posterior return NULL.
#'
#' @details
#' This function handles both scalar and observation-indexed parameters:
#' \itemize{
#'   \item Scalar parameters (e.g., "sigma"): broadcast to `\\[ndraws x nobs\\]`
#'   \item Indexed parameters (e.g., "sigma\[1\]", "sigma\[2\]", ...): extracted
#'     as matrix with nobs columns
#' }
#'
#' Example of scalar broadcasting:
#' If posterior contains "sigma" (scalar), and ndraws=100, nobs=50:
#'   - Extracts 100 draws of scalar sigma
#'   - Returns matrix \[100 x 50\] with each row containing the same sigma value
#'
#' Parameter naming follows brms conventions. Some families use different
#' internal names:
#' \itemize{
#'   \item Beta precision: "phi" in family specification
#'   \item Negative binomial shape: "shape" (may be "r" in some
#'     parameterizations)
#' }
#'
#' @seealso `get_family_dpars()` for family-to-dpar mapping,
#'   `sample_from_family()` which consumes these matrices.
#'
#' @noRd
extract_dpars_from_stanfit <- function(stanfit,
                                       dpar_names,
                                       ndraws,
                                       nobs,
                                       draw_ids = NULL,
                                       resp = NULL) {
  # Validate stanfit can be converted to draws
  checkmate::assert_multi_class(
    stanfit,
    c("stanfit", "CmdStanMCMC", "draws", "draws_matrix", "draws_array")
  )
  checkmate::assert_character(dpar_names, min.len = 0)
  checkmate::assert_int(ndraws, lower = 1)
  checkmate::assert_int(nobs, lower = 1)
  checkmate::assert_integerish(draw_ids, lower = 1, null.ok = TRUE)

  # Return empty list if no dpars needed
  if (length(dpar_names) == 0) {
    return(list())
  }

  # Convert stanfit to draws matrix
  draws_mat <- posterior::as_draws_matrix(stanfit)
  all_cols <- colnames(draws_mat)
  total_draws <- nrow(draws_mat)

  # Determine which draw indices to use
  if (!is.null(draw_ids)) {
    if (max(draw_ids) > total_draws) {
      stop(insight::format_error(c(
        cli::format_inline(
          "Requested {.field draw_ids} exceed available draws."
        ),
        x = cli::format_inline(
          "Max requested: {max(draw_ids)}, available: {total_draws}."
        )
      )))
    }
    draw_indices <- draw_ids
    ndraws <- length(draw_ids)
  } else {
    draw_indices <- resolve_draw_indices(total_draws, ndraws, NULL)
    ndraws <- length(draw_indices)
  }

  # Extract each dpar
  dpars_list <- list()

  for (dpar in dpar_names) {
    # Build patterns for scalar and indexed parameters. brms's
    # mvbf suffixes per-arm dpars with `_<resp>` (e.g.
    # `shape_biomass`); for univariate fits no suffix is added.
    suffix <- if (!is.null(resp)) paste0("_", resp) else ""
    scalar_pattern <- paste0("^", dpar, suffix, "$")
    indexed_pattern <- paste0("^", dpar, suffix, "\\[")

    # Find matching columns
    scalar_cols <- grep(scalar_pattern, all_cols, value = TRUE)
    indexed_cols <- grep(indexed_pattern, all_cols, value = TRUE)

    if (length(scalar_cols) > 0) {
      # Scalar parameter - extract and broadcast to [ndraws x nobs]
      scalar_draws <- as.numeric(draws_mat[draw_indices, scalar_cols[1]])
      dpars_list[[dpar]] <- matrix(
        scalar_draws,
        nrow = ndraws,
        ncol = nobs,
        byrow = FALSE
      )
    } else if (length(indexed_cols) > 0) {
      # Indexed parameters - extract and validate dimensions
      # Sort columns by index to ensure correct ordering
      indices <- as.integer(gsub(".*\\[(\\d+)\\].*", "\\1", indexed_cols))

      # Validate index extraction succeeded
      if (any(is.na(indices))) {
        stop(insight::format_error(
          cli::format_inline(
            "Failed to extract numeric indices from parameter names: {.val {indexed_cols[is.na(indices)]}}."
          )
        ))
      }

      indexed_cols <- indexed_cols[order(indices)]

      # Extract as matrix
      dpar_matrix <- as.matrix(
        draws_mat[draw_indices, indexed_cols, drop = FALSE]
      )

      # Handle dimension mismatch when training data has different nobs than
      # prediction data (e.g., newdata in posterior_predict has fewer/more
      # observations than the fitted model's observation-level parameters)
      if (ncol(dpar_matrix) == 1) {
        # Single indexed parameter - broadcast like scalar
        dpars_list[[dpar]] <- matrix(
          dpar_matrix[, 1],
          nrow = ndraws,
          ncol = nobs,
          byrow = FALSE
        )
      } else if (ncol(dpar_matrix) == nobs) {
        # Correct number of columns
        dpars_list[[dpar]] <- dpar_matrix
      } else {
        # Dimension mismatch - use first column with warning
        if (!identical(Sys.getenv("TESTTHAT"), "true")) {
          rlang::warn(
            paste0(
              "Parameter '", dpar, "' has ", ncol(dpar_matrix),
              " columns but ", nobs, " observations. ",
              "Using first column (scalar behavior)."
            ),
            .frequency = "once",
            .frequency_id = paste0("dpar_dim_mismatch_", dpar)
          )
        }
        dpars_list[[dpar]] <- matrix(
          dpar_matrix[, 1],
          nrow = ndraws,
          ncol = nobs,
          byrow = FALSE
        )
      }
    } else {
      # Parameter not found - return NULL (caller handles defaults)
      dpars_list[[dpar]] <- NULL
    }
  }

  dpars_list
}


#' Extract Posterior Predictive Distribution from mvgam Models
#'
#' @description
#' Generate posterior predictive samples from fitted mvgam models. These samples
#' include observation-level noise, making them suitable for posterior predictive
#' checks and predictions on new data.
#'
#' @param object A fitted mvgam object from [mvgam()].
#' @param newdata Optional data frame with covariates for prediction. If
#'   NULL, uses original training data stored in the model object.
#' @inheritParams posterior_epred.mvgam
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
#' @return Matrix with dimensions `\\[ndraws x nobs\\]` containing posterior
#'   predictive samples. Each row is one posterior draw, each column is
#'   one observation. Values are on the response scale (include observation
#'   noise).
#'
#'   For multivariate models with resp = NULL, returns a named list of
#'   matrices (one per response variable).
#'
#' @details
#' The posterior predictive distribution combines:
#' \itemize{
#'   \item Expected values E\[Y|X\]: from observation + trend models
#'   \item Observation noise: family-specific random draws
#' }
#'
#' This differs from `posterior_epred.mvgam()` which returns only E\[Y|X\].
#' Posterior predictive samples have higher variance and are suitable for
#' checking if the model can generate data like the observed data.
#'
#' Semantic split (see also the architecture-decisions document):
#' \itemize{
#'   \item `posterior_predict()` / `posterior_epred()` /
#'     `posterior_linpred()` integrate over the trend's stochastic
#'     dynamics. At newdata times beyond the training grid the
#'     latent state is treated as stationary at its per-series
#'     posterior mean (a `marginaleffects`-style convention; an
#'     informational message fires once per session).
#'   \item \[forecast.mvgam\] / \[hindcast.mvgam\] read the fitted
#'     `lv_trend` posterior draws directly. `hindcast()` returns
#'     them at the training grid; `forecast()` extrapolates them
#'     forward via the kernel for newdata times beyond training.
#'     Use these surfaces when you want state-aware out-of-sample
#'     prediction.
#' }
#'
#' @seealso `posterior_epred.mvgam()` for expected values without
#'   noise, [posterior_linpred.mvgam()] for link-scale predictions,
#'   \[forecast.mvgam\] and \[hindcast.mvgam\] for the deterministic
#'   state-extrapolating prediction surface.
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
#' # Posterior predictive draws on the response scale, marginalising
#' # over the trend's stochastic dynamics. For h-step forecasts that
#' # extrapolate the fitted latent state, see [forecast.mvgam()].
#' pp <- posterior_predict(mod, ndraws = 50L)
#' dim(pp)
#'
#' # A coverage check: how often does the central 80% predictive
#' # interval contain the observed cell? Should land near 0.80 for
#' # a well-calibrated marginal-MC fit.
#' lo <- apply(pp, 2L, quantile, 0.10)
#' hi <- apply(pp, 2L, quantile, 0.90)
#' mean(simdat$data_train$y >= lo & simdat$data_train$y <= hi,
#'       na.rm = TRUE)
#' }
#'
#' @importFrom brms posterior_predict
#' @method posterior_predict mvgam
#' @export
posterior_predict.mvgam <- function(object, newdata = NULL,
                                    process_error = FALSE,
                                    incl_autocor = FALSE,
                                    ndraws = NULL,
                                    draw_ids = NULL,
                                    re_formula = NULL,
                                    allow_new_levels = FALSE,
                                    sample_new_levels = "uncertainty",
                                    resp = NULL,
                                    ...) {
  # Validate all parameters
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_data_frame(newdata, null.ok = TRUE)
  checkmate::assert_logical(process_error, len = 1)
  checkmate::assert_logical(incl_autocor, len = 1,
                            any.missing = FALSE)
  checkmate::assert_int(ndraws, lower = 1, null.ok = TRUE)
  checkmate::assert_integerish(draw_ids, lower = 1L, null.ok = TRUE)
  if (!is.null(ndraws) && !is.null(draw_ids)) {
    stop(insight::format_error(
      "Specify only one of 'ndraws' or 'draw_ids'."
    ))
  }
  checkmate::assert(
    checkmate::check_class(re_formula, "formula"),
    checkmate::check_true(is.na(re_formula)),
    checkmate::check_null(re_formula)
  )
  checkmate::assert_logical(allow_new_levels, len = 1)
  checkmate::assert_choice(
    sample_new_levels,
    choices = c("uncertainty", "gaussian", "old_levels")
  )
  checkmate::assert_string(resp, null.ok = TRUE)

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

  # Closure-unit families intercept upstream because the per-visit
  # sampling step needs joint-over-unit latent state draws (N for
  # nmix, z for occ) plus the rebuilt closure-unit arrays from the
  # current newdata; the generic linpred + sample_from_family path
  # cannot produce that without the unit-level structure.
  if (is_closure_unit_family(object$family)) {
    predict_fn <- dispatch_closure_unit_method(object$family, "predict")
    return(predict_fn(
      object, newdata = newdata, draw_ids = draw_ids
    ))
  }

  # Get ALL draws from linpred (returns list for multivariate without resp).
  # Using linpred + inverse link (not posterior_epred) because for ZI/hurdle
  # families, posterior_epred returns E[Y]=(1-zi)*mu, but sampling requires
  # the raw mu parameter to apply zi/hu during sampling.
  # `posterior_linpred()` is a sibling method, so it takes the
  # user-facing `incl_autocor` rather than the `trend_state` that
  # `get_combined_linpred()` reads.
  linpred_all <- posterior_linpred(
    object,
    newdata = newdata,
    process_error = process_error,
    incl_autocor = incl_autocor,
    ndraws = NULL,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels,
    resp = resp
  )

  # The trend's innovations are already carried on the linear
  # predictor: `posterior_linpred()` composes them through
  # `get_combined_linpred()` under `process_error = TRUE`. Sampling a
  # second, independent set here added the process variance twice
  # before the observation noise was drawn on top.

  # Sample draw_ids ONCE for consistent subsampling across all responses
  if (is.list(linpred_all) && !is.matrix(linpred_all)) {
    total_draws <- nrow(linpred_all[[1]])
  } else {
    total_draws <- nrow(linpred_all)
  }

  # A requested count became indices above, so either the caller named
  # the draws it wants or every draw is used.
  if (is.null(draw_ids)) {
    draw_ids <- seq_len(total_draws)
  }
  if (max(draw_ids) > total_draws) {
    stop(insight::format_error(c(
      "Requested 'draw_ids' exceed available draws.",
      x = paste0(
        "Max requested: ", max(draw_ids),
        ", available: ", total_draws, "."
      )
    )))
  }
  ndraws <- length(draw_ids)

  # Multivariate detection (consistent with posterior_epred.mvgam)
  is_mv <- inherits(object$formula, "mvbrmsformula") &&
    !is.null(object$formula$forms) &&
    length(object$formula$forms) > 1

  if (is_mv && is.null(resp)) {
    # Multivariate without resp: process each response with SAME draw_ids
    resp_names <- names(linpred_all)
    result_list <- lapply(resp_names, function(r) {
      predict_single_response(
        object = object,
        linpred_resp = linpred_all[[r]],
        resp = r,
        draw_ids = draw_ids,
        ndraws = ndraws,
        newdata = newdata,
        is_multivariate = TRUE
      )
    })
    names(result_list) <- resp_names
    return(result_list)
  }

  # Univariate or multivariate with resp specified
  linpred_mat <- if (is.list(linpred_all)) linpred_all[[1]] else linpred_all
  predict_single_response(
    object = object,
    linpred_resp = linpred_mat,
    resp = resp,
    draw_ids = draw_ids,
    ndraws = ndraws,
    newdata = newdata,
    is_multivariate = is_mv
  )
}


#' Predict from a single response (helper for posterior_predict.mvgam)
#'
#' Applies inverse link to linpred, extracts dpars, and samples from family.
#' For ZI/hurdle families, uses raw mu (not deflated expected value) so that
#' zi/hu can be applied during the sampling process itself.
#'
#' @param object mvgam model object
#' @param linpred_resp Matrix \[total_draws x nobs\] of linear predictor
#' @param resp Response name (NULL for univariate)
#' @param draw_ids Integer vector of draw indices to use
#' @param ndraws Number of draws (length of draw_ids)
#' @param newdata Data frame for predictions
#' @param is_multivariate Logical; TRUE if multivariate model
#'
#' @return Matrix `\\[ndraws x nobs\\]` of posterior predictive samples
#'
#' @noRd
predict_single_response <- function(object, linpred_resp, resp, draw_ids,
                                    ndraws, newdata, is_multivariate) {
  # Validate parameters
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_matrix(linpred_resp)
  checkmate::assert_integerish(draw_ids, lower = 1, any.missing = FALSE)
  checkmate::assert_int(ndraws, lower = 1)
  checkmate::assert_data_frame(newdata)
  checkmate::assert_logical(is_multivariate, len = 1)
  checkmate::assert_string(resp, null.ok = TRUE)

  # For multivariate, resp must be specified
  if (is_multivariate) {
    checkmate::assert_string(resp, null.ok = FALSE)
  }

  # Subset linpred by draw_ids and apply inverse link
  linpred <- linpred_resp[draw_ids, , drop = FALSE]
  nobs <- ncol(linpred)

  # `resp` is NULL on a univariate fit, which resolves to the fit's
  # own family.
  family <- get_family_for_resp(object, if (is_multivariate) resp else NULL)

  # Closure-unit families need joint-over-unit sampling: draw
  # the latent state (N for nmix, z for occ) per closure unit,
  # then draw each visit's response conditional on that state.
  # Intercept upstream so the generic per-row sample_from_family
  # dispatch never sees a closure-unit family.
  if (is_closure_unit_family(family)) {
    predict_fn <- dispatch_closure_unit_method(family, "predict")
    return(predict_fn(
      object, newdata = newdata, draw_ids = draw_ids
    ))
  }

  mu <- family$linkinv(linpred)
  family_name <- resolve_family_name(family)

  # Get dpar names for this family
  dpars <- resolve_family_pars(
    object,
    dpar_names = get_family_dpars(family_name),
    ndraws = ndraws,
    nobs = nobs,
    draw_ids = draw_ids,
    newdata = newdata,
    resp = if (is_multivariate) resp else NULL
  )

  # Extract trials for binomial families
  trials <- extract_trials_for_family(object, family, newdata)

  # Extract constant truncation bounds if model has truncation
  trunc_bounds <- extract_truncation_bounds(object, nobs)

  # Ordinal families need thres + disc draws and operate on the
  # link-scale linear predictor rather than the response-scale mu.
  if (family_name %in% ORDINAL_FAMILIES) {
    dpars$thres <- extract_ordinal_thresholds(object, ndraws = ndraws,
                                              draw_ids = draw_ids)
    dpars$disc <- extract_ordinal_disc(object, ndraws = ndraws,
                                       nobs = nobs, draw_ids = draw_ids)
    epred_for_family <- linpred
  } else {
    epred_for_family <- mu
  }

  # Sample from family distribution. Forward whichever distributional
  # parameters the registry produced for this family instead of naming
  # them one at a time; a hand-written list silently drops the
  # parameters of any family added afterwards. Names with no matching
  # argument belong to families that draw through a different path.
  dpar_args <- dpars[intersect(names(dpars),
                               names(formals(sample_from_family)))]
  samples <- do.call(sample_from_family, c(
    list(
      family_name = family_name,
      ndraws = ndraws,
      epred = epred_for_family,
      trials = trials,
      lb = trunc_bounds$lb,
      ub = trunc_bounds$ub
    ),
    dpar_args
  ))

  # Reshape vector to matrix [ndraws x nobs]
  matrix(samples, nrow = ndraws, ncol = nobs, byrow = FALSE)
}
