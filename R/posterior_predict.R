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
      stop(insight::format_error(
        "Mixed finite and infinite lower bounds not supported.",
        "i" = "All lb values must be either finite or all infinite (-Inf)."
      ))
    }
  }
  if (!is.null(ub)) {
    has_finite <- any(is.finite(ub))
    has_infinite <- any(!is.finite(ub))
    if (has_finite && has_infinite) {
      stop(insight::format_error(
        "Mixed finite and infinite upper bounds not supported.",
        "i" = "All ub values must be either finite or all infinite (Inf)."
      ))
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
        stop(insight::format_error(
          paste0(
            "Invalid truncation bounds: {.field lb} (", lb_vals,
            ") must be less than {.field ub} (", ub_vals, ")."
          ),
          "i" = "Truncation requires lb < ub to define a valid bounded region."
        ))
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


#' Sample from Truncated Continuous Distribution
#'
#' Uses inverse CDF transformation for truncated sampling from continuous
#' distributions. Falls back to rejection sampling if CDF/quantile functions
#' are unavailable.
#'
#' @param n Integer; number of samples
#' @param dist Character; distribution name (e.g., "norm", "gamma")
#' @param lb Numeric; lower truncation bound
#' @param ub Numeric; upper truncation bound
#' @param ntrys Integer; rejection sampling attempts if CDF unavailable
#' @param ... Additional arguments passed to distribution functions
#'
#' @return Numeric vector of n samples from the truncated distribution.
#'   NA values indicate samples that could not be generated within bounds.
#'
#' @noRd
sample_continuous_truncated <- function(n, dist, lb = -Inf, ub = Inf,
                                        ntrys = 5, ...) {
  checkmate::assert_int(n, lower = 1)
  checkmate::assert_string(dist)
  checkmate::assert_number(lb)
  checkmate::assert_number(ub)
  checkmate::assert_int(ntrys, lower = 1)

  if (is.finite(lb) && is.finite(ub) && lb >= ub) {
    stop(insight::format_error(
      paste0(
        "Invalid truncation bounds: {.field lb} (", lb,
        ") must be less than {.field ub} (", ub, ")."
      ),
      "i" = "Truncation requires lb < ub to define a valid bounded region."
    ))
  }

  args <- list(...)
  pdist <- paste0("p", dist)
  qdist <- paste0("q", dist)

  if (exists(pdist, mode = "function") && exists(qdist, mode = "function")) {
    # Inverse CDF transformation method
    plb <- do.call(pdist, c(list(lb), args))
    pub <- do.call(pdist, c(list(ub), args))
    u <- stats::runif(n, min = plb, max = pub)
    out <- do.call(qdist, c(list(u), args))
    # Handle numerical imprecision at boundaries
    out[out %in% c(-Inf, Inf)] <- NA_real_
  } else {
    # Fall back to rejection sampling
    out <- sample_truncated_rejection(
      n, dist, lb = lb, ub = ub, ntrys = ntrys, ...
    )
  }

  out
}


#' Sample from Truncated Distribution via Rejection Sampling
#'
#' General-purpose rejection sampler for truncated distributions. Samples
#' ntrys times and selects first valid value within bounds.
#'
#' @param n Integer; number of samples
#' @param dist Character; distribution name (e.g., "pois", "norm")
#' @param lb Numeric; lower truncation bound
#' @param ub Numeric; upper truncation bound
#' @param ntrys Integer; number of rejection sampling attempts
#' @param ... Additional arguments passed to distribution functions
#'
#' @return Numeric vector of n samples. NA_real_ where no valid sample found
#'   after ntrys attempts.
#'
#' @noRd
sample_truncated_rejection <- function(n, dist, lb = -Inf, ub = Inf,
                                       ntrys = 5, ...) {
  checkmate::assert_int(n, lower = 1)
  checkmate::assert_string(dist)
  checkmate::assert_number(lb)
  checkmate::assert_number(ub)
  checkmate::assert_int(ntrys, lower = 1)

  if (is.finite(lb) && is.finite(ub) && lb >= ub) {
    stop(insight::format_error(
      paste0(
        "Invalid truncation bounds: {.field lb} (", lb,
        ") must be less than {.field ub} (", ub, ")."
      ),
      "i" = "Truncation requires lb < ub to define a valid bounded region."
    ))
  }

  args <- list(...)
  rdist <- paste0("r", dist)

  # Sample ntrys times
  samples <- vector("list", ntrys)
  for (i in seq_along(samples)) {
    samples[[i]] <- as.vector(do.call(rdist, c(list(n), args)))
  }
  samples <- do.call(cbind, samples)

  # For each row, find first valid sample within bounds
  out <- apply(samples, 1, function(x) {
    valid <- which(x >= lb & x <= ub)
    if (length(valid) > 0) {
      x[valid[1]]
    } else {
      NA_real_
    }
  })

  out
}


#' Check for Invalid Truncated Samples
#'
#' Checks predicted values against truncation bounds and warns if many samples
#' are invalid. Rounds values for discrete distributions.
#'
#' @param x Matrix of samples [ndraws x nobs]
#' @param lb Lower bounds (vector of length nobs, scalar, or NULL)
#' @param ub Upper bounds (vector of length nobs, scalar, or NULL)
#' @param threshold Numeric in (0,1); fraction of invalid samples triggering
#'   warning. Default 0.01 (1%) matches brms::check_discrete_trunc_bounds().
#'
#' @return Matrix x rounded to integers (for discrete distributions)
#'
#' @noRd
check_truncation_bounds <- function(x, lb = NULL, ub = NULL, threshold = 0.01) {
  checkmate::assert_matrix(x)
  checkmate::assert_numeric(lb, null.ok = TRUE)
  checkmate::assert_numeric(ub, null.ok = TRUE)
  checkmate::assert_number(threshold, lower = 0, upper = 1)

  if (is.null(lb) && is.null(ub)) {
    return(x)
  }

  lb_check <- if (is.null(lb)) -Inf else lb
  ub_check <- if (is.null(ub)) Inf else ub

  # Expand bounds to match matrix dimensions
  if (length(lb_check) == 1) lb_check <- rep(lb_check, ncol(x))
  if (length(ub_check) == 1) ub_check <- rep(ub_check, ncol(x))

  # Flatten for comparison
  y <- as.vector(t(x))
  lb_expanded <- rep(lb_check, each = nrow(x))
  ub_expanded <- rep(ub_check, each = nrow(x))

  # Count invalid: NA or outside bounds
  pct_invalid <- mean(is.na(y) | y < lb_expanded | y > ub_expanded,
                      na.rm = FALSE)

  if (pct_invalid >= threshold) {
    if (!identical(Sys.getenv("TESTTHAT"), "true")) {
      rlang::warn(
        c(
          paste0(
            round(pct_invalid * 100),
            "% of all predicted values were invalid."
          ),
          "i" = "Increasing argument {.arg ntrys} may help."
        ),
        .frequency = "once",
        .frequency_id = "mvgam_trunc_invalid"
      )
    }
  }

  round(x)
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
    "discrete_weibull", "com_poisson"
  )

  family_name %in% integer_families
}


#' Map Family Name to R Distribution Abbreviation
#'
#' Maps brms/mvgam family names to base R distribution abbreviations
#' (e.g., "gaussian" -> "norm", "poisson" -> "pois") for use with
#' truncation sampling functions.
#'
#' @param family_name Character; family name (e.g., "gaussian", "poisson")
#'
#' @return Character; R distribution abbreviation (e.g., "norm", "pois"),
#'   or NA_character_ if family has no simple R distribution mapping.
#'
#' @noRd
family_to_dist <- function(family_name) {
  checkmate::assert_string(family_name)

  dist_map <- c(
    "gaussian" = "norm",
    "student" = "lst",
    "exponential" = "exp",
    "gamma" = "gamma",
    "weibull" = "weibull",
    "lognormal" = "lnorm",
    "beta" = "beta",
    "poisson" = "pois",
    "negbinomial" = "nbinom",
    "negbinomial2" = "nbinom",
    "geometric" = "geom",
    "binomial" = "binom",
    "bernoulli" = "binom"
  )

  if (family_name %in% names(dist_map)) {
    return(dist_map[[family_name]])
  }

  NA_character_
}


#' Apply Truncation to Samples Matrix
#'
#' Applies truncation bounds to samples by resampling out-of-bounds values
#' or clamping to bounds when resampling fails.
#'
#' @param samples Matrix [ndraws x nobs] or vector of samples
#' @param family_name Character; distribution family name
#' @param lb Numeric; lower bound(s) - scalar or vector of length nobs
#' @param ub Numeric; upper bound(s) - scalar or vector of length nobs
#' @param ntrys Integer; rejection sampling attempts for discrete families
#' @param ndraws Integer; number of draws (needed if samples is vector)
#' @param nobs Integer; number of observations (needed if samples is vector)
#'
#' @return Matrix [ndraws x nobs] of truncated samples
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
apply_truncation <- function(samples, family_name, lb, ub, ntrys,
                             ndraws, nobs) {
  checkmate::assert_string(family_name)
  checkmate::assert_numeric(lb, null.ok = TRUE)
  checkmate::assert_numeric(ub, null.ok = TRUE)
  checkmate::assert_int(ntrys, lower = 1)
  checkmate::assert_int(ndraws, lower = 1)

  checkmate::assert_int(nobs, lower = 1)

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

  # Expand scalar bounds to vector
  if (length(lb) == 1) lb <- rep(lb, nobs)
  if (length(ub) == 1) ub <- rep(ub, nobs)

  # Get distribution abbreviation for resampling
  dist <- family_to_dist(family_name)
  is_discrete <- family_uses_integers(family_name)

  # Track clamping for warning
  n_clamped <- 0
  total_samples <- ndraws * nobs

  # Process each column (observation)
  for (j in seq_len(nobs)) {
    col_lb <- lb[j]
    col_ub <- ub[j]

    # Skip if no effective truncation
    if (is.infinite(col_lb) && col_lb < 0 &&
        is.infinite(col_ub) && col_ub > 0) {
      next
    }

    # Find invalid samples
    invalid <- !is.na(samples[, j]) &
      (samples[, j] < col_lb | samples[, j] > col_ub)
    n_invalid <- sum(invalid)

    if (n_invalid == 0) next

    # Attempt resampling if we have a known distribution
    if (!is.na(dist)) {
      if (is_discrete) {
        new_samples <- sample_truncated_rejection(
          n = n_invalid,
          dist = dist,
          lb = col_lb,
          ub = col_ub,
          ntrys = ntrys
        )
      } else {
        new_samples <- sample_continuous_truncated(
          n = n_invalid,
          dist = dist,
          lb = col_lb,
          ub = col_ub,
          ntrys = ntrys
        )
      }

      # Replace valid resampled values
      valid_new <- !is.na(new_samples)
      samples[which(invalid)[valid_new], j] <- new_samples[valid_new]

      # Check for remaining invalid samples after resampling
      still_invalid <- invalid &
        (samples[, j] < col_lb | samples[, j] > col_ub)
      n_still_invalid <- sum(still_invalid)
    } else {
      # No known distribution - just clamp
      still_invalid <- invalid
      n_still_invalid <- n_invalid
    }

    # Clamp remaining out-of-bounds samples
    if (n_still_invalid > 0) {
      n_clamped <- n_clamped + n_still_invalid
      clamp_idx <- which(still_invalid)
      samples[clamp_idx[samples[clamp_idx, j] < col_lb], j] <- col_lb
      samples[clamp_idx[samples[clamp_idx, j] > col_ub], j] <- col_ub
    }
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
#' @param epred Matrix [ndraws x nobs] of expected values
#' @param sigma Optional matrix [ndraws x nobs] of scale/dispersion.
#'   Required for: gaussian, student, lognormal, hurdle_lognormal,
#'   skew_normal, exgaussian, gen_extreme_value, asym_laplace.
#' @param phi Optional matrix [ndraws x nobs] of precision parameter.
#'   Required for: beta, beta_binomial, zero_inflated_beta,
#'   zero_inflated_beta_binomial, zero_one_inflated_beta.
#' @param shape Optional matrix [ndraws x nobs] of shape parameters.
#'   Required for: gamma, negbinomial, weibull, frechet, inverse.gaussian,
#'   hurdle_negbinomial, hurdle_gamma, discrete_weibull, com_poisson
#' @param nu Optional matrix [ndraws x nobs] of degrees of freedom.
#'   Required for: student
#' @param trials Optional vector of trial counts (length 1 or nobs).
#'   Required for: binomial, beta_binomial, zero_inflated_binomial,
#'   zero_inflated_beta_binomial
#' @param hu Optional matrix [ndraws x nobs] of hurdle probability.
#'   Required for: hurdle_poisson, hurdle_negbinomial, hurdle_gamma,
#'   hurdle_lognormal
#' @param zi Optional matrix [ndraws x nobs] of zero-inflation probability.
#'   Required for: zero_inflated_poisson, zero_inflated_negbinomial,
#'   zero_inflated_binomial, zero_inflated_beta_binomial, zero_inflated_beta,
#'   zero_inflated_asym_laplace
#' @param zoi Optional matrix [ndraws x nobs] of zero-one inflation.
#'   Required for: zero_one_inflated_beta (probability of boundary value)
#' @param coi Optional matrix [ndraws x nobs] of conditional one-inflation.
#'   Required for: zero_one_inflated_beta (P(Y=1 | Y in {0,1}))
#' @param alpha Optional matrix [ndraws x nobs] of skewness parameters.
#'   Required for: skew_normal
#' @param ndt Optional matrix [ndraws x nobs] of non-decision time (>=0).
#'   Required for: shifted_lognormal, exgaussian
#' @param xi Optional matrix [ndraws x nobs] of shape/tail parameters.
#'   Required for: gen_extreme_value
#' @param quantile Optional matrix [ndraws x nobs] of quantile values (0,1).
#'   Required for: asym_laplace, zero_inflated_asym_laplace
#' @param kappa Optional matrix [ndraws x nobs] of concentration (>0).
#'   Required for: von_mises
#' @param beta Optional matrix [ndraws x nobs] of rate parameters (>0).
#'   Required for: exgaussian
#' @param bs Optional matrix [ndraws x nobs] of boundary separation (>0).
#'   Required for: wiener
#' @param bias Optional matrix [ndraws x nobs] of starting point bias (0-1).
#'   Required for: wiener
#' @param disc Optional matrix [ndraws x nobs] of discrimination (>0).
#'   Required for: hurdle_cumulative. Defaults to 1 if NULL.
#' @param thres Optional matrix [ndraws x nthres] of ordinal thresholds.
#'   Required for: hurdle_cumulative
#' @param link Character; link function for ordinal models.
#'   Required for: hurdle_cumulative. Default "logit".
#'
#' @return Vector of sampled values with length = length(epred). Values are
#'   integers for count families, doubles for continuous. Used internally
#'   by posterior_predict.mvgam() which reshapes to [ndraws x nobs] matrix.
#'
#' @noRd
sample_from_family <- function(family_name, ndraws, epred,
                               sigma = NULL, phi = NULL,
                               shape = NULL, nu = NULL,
                               trials = NULL, hu = NULL, zi = NULL,
                               zoi = NULL, coi = NULL,
                               alpha = NULL, ndt = NULL, xi = NULL,
                               quantile = NULL, kappa = NULL,
                               beta = NULL, bs = NULL, bias = NULL,
                               disc = NULL, thres = NULL,
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

  samples <- switch(
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

    "exgaussian" = {
      checkmate::assert_matrix(sigma, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(beta, nrows = ndraws, ncols = ncol(epred))
      brms::rexgaussian(length(epred), mu = epred, sigma = sigma, beta = beta)
    },

    "lognormal" = {
      checkmate::assert_matrix(sigma, nrows = ndraws, ncols = ncol(epred))
      # epred = exp(mu + sigma^2/2), so mu = log(epred) - sigma^2/2
      mu <- log(epred) - sigma^2 / 2
      stats::rlnorm(length(epred), meanlog = mu, sdlog = sigma)
    },

    "shifted_lognormal" = {
      checkmate::assert_matrix(sigma, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(ndt, nrows = ndraws, ncols = ncol(epred))
      # Validate epred > ndt (shift must be less than expected value)
      if (any(epred <= ndt)) {
        stop(insight::format_error(
          "For shifted_lognormal, expected values must exceed shift (ndt). ",
          "Found {sum(epred <= ndt)} values where epred <= ndt."
        ))
      }
      # epred = exp(meanlog + sigma^2/2) + ndt
      # So meanlog = log(epred - ndt) - sigma^2/2
      meanlog <- log(epred - ndt) - sigma^2 / 2
      brms::rshifted_lnorm(
        length(epred),
        meanlog = meanlog,
        sdlog = sigma,
        shift = ndt
      )
    },

    "beta" = {
      checkmate::assert_matrix(phi, nrows = ndraws, ncols = ncol(epred))
      # mu and phi (precision) -> shape1, shape2
      shape1 <- epred * phi
      shape2 <- (1 - epred) * phi
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
      brms::rdiscrete_weibull(length(epred), mu = epred, shape = shape)
    },

    "com_poisson" = {
      checkmate::assert_matrix(shape, nrows = ndraws, ncols = ncol(epred))
      brms::rcom_poisson(length(epred), mu = epred, shape = shape)
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
      # epred = mu * trials, mu = epred / trials
      if (length(trials) == 1) {
        mu <- epred / trials
      } else {
        mu <- sweep(epred, 2, trials, `/`)
      }
      extraDistr::rbbinom(length(epred), size = trials, mu = mu, sigma = phi)
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
      if (length(trials) == 1) {
        prob <- epred / trials
      } else {
        prob <- sweep(epred, 2, trials, `/`)
      }
      tmp <- stats::runif(length(epred))
      ifelse(tmp < zi, 0L, stats::rbinom(length(epred), size = trials, prob = prob))
    },

    "zero_inflated_beta_binomial" = {
      checkmate::assert_numeric(trials, min.len = 1)
      checkmate::assert_matrix(zi, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(phi, nrows = ndraws, ncols = ncol(epred))
      if (length(trials) == 1) {
        mu <- epred / trials
      } else {
        mu <- sweep(epred, 2, trials, `/`)
      }
      draws <- extraDistr::rbbinom(length(epred), size = trials, mu = mu,
                                   sigma = phi)
      tmp <- stats::runif(length(epred))
      draws[tmp < zi] <- 0L
      draws
    },

    "zero_inflated_beta" = {
      checkmate::assert_matrix(zi, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(phi, nrows = ndraws, ncols = ncol(epred))
      shape1 <- epred * phi
      shape2 <- (1 - epred) * phi
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
      shape1 <- epred * phi
      shape2 <- (1 - epred) * phi
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
      mu <- log(epred) - sigma^2 / 2
      ifelse(tmp < hu, 0, stats::rlnorm(length(epred), meanlog = mu, sdlog = sigma))
    },

    "hurdle_cumulative" = {
      checkmate::assert_matrix(hu, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(thres)
      if (is.null(disc)) disc <- 1
      nthres <- ncol(thres)
      ncat <- nthres + 1L

      # Compute cumulative probabilities for each category
      # Using brms pordinal helper
      pordinal <- getFromNamespace("pordinal", "brms")
      first_greater <- getFromNamespace("first_greater", "brms")

      # Get category probabilities [ndraws x ncat]
      p <- pordinal(
        q = seq_len(ncat),
        eta = epred,
        disc = disc,
        thres = thres,
        family = "cumulative",
        link = link
      )

      # Sample: 0 if hurdle, else sample from categories 1:ncat
      tmp <- stats::runif(length(epred))
      u <- stats::runif(ndraws)
      ordinal_samples <- first_greater(p, target = u)
      ifelse(tmp < hu, 0L, ordinal_samples)
    },

    # ============ Unsupported families ============

    stop(insight::format_error(
      "Posterior predictive sampling for family {.val {family_name}} ",
      "is not yet implemented."
    ))
  )

  # Apply truncation if bounds are specified
  if (has_truncation) {
    samples <- apply_truncation(
      samples = samples,
      family_name = family_name,
      lb = lb,
      ub = ub,
      ntrys = ntrys,
      ndraws = ndraws,
      nobs = nobs
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
#' Includes all brms families for completeness. Families not yet implemented
#' in \code{sample_from_family()} will error at sampling time, not here.
#' Families not in the mapping return an empty character vector.
#'
#' @noRd
get_family_dpars <- function(family_name) {
  checkmate::assert_string(family_name)

  # Map families to required distributional parameters
  # Complete brms family coverage for future-proofing
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
    hurdle_cumulative = c("hu", "disc")
  )

  dpar_map[[family_name]] %||% character(0)
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
#' @return Named list of matrices, each with dimensions [ndraws x nobs].
#'   Scalar parameters are broadcast to full [ndraws x nobs] matrices.
#'   Parameters not found in the posterior return NULL.
#'
#' @details
#' This function handles both scalar and observation-indexed parameters:
#' \itemize{
#'   \item Scalar parameters (e.g., "sigma"): broadcast to [ndraws x nobs]
#'   \item Indexed parameters (e.g., "sigma[1]", "sigma[2]", ...): extracted
#'     as matrix with nobs columns
#' }
#'
#' Example of scalar broadcasting:
#' If posterior contains "sigma" (scalar), and ndraws=100, nobs=50:
#'   - Extracts 100 draws of scalar sigma
#'   - Returns matrix [100 x 50] with each row containing the same sigma value
#'
#' Parameter naming follows brms conventions. Some families use different
#' internal names:
#' \itemize{
#'   \item Beta precision: "phi" in family specification
#'   \item Negative binomial shape: "shape" (may be "r" in some
#'     parameterizations)
#' }
#'
#' @seealso [get_family_dpars()] for family-to-dpar mapping,
#'   [sample_from_family()] which consumes these matrices.
#'
#' @noRd
extract_dpars_from_stanfit <- function(stanfit,
                                       dpar_names,
                                       ndraws,
                                       nobs,
                                       draw_ids = NULL) {
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
      stop(insight::format_error(
        "Requested {.field draw_ids} exceed available draws.",
        "Max requested: {max(draw_ids)}, available: {total_draws}."
      ))
    }
    draw_indices <- draw_ids
    ndraws <- length(draw_ids)
  } else if (ndraws > total_draws) {
    stop(insight::format_error(
      "Requested {.field ndraws} ({ndraws}) exceeds available draws ",
      "({total_draws})."
    ))
  } else {
    draw_indices <- seq_len(ndraws)
  }

  # Extract each dpar
  dpars_list <- list()

  for (dpar in dpar_names) {
    # Build patterns for scalar and indexed parameters
    scalar_pattern <- paste0("^", dpar, "$")
    indexed_pattern <- paste0("^", dpar, "\\[")

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
          "Failed to extract numeric indices from parameter names: ",
          "{.val {indexed_cols[is.na(indices)]}}."
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
#' @param process_error Logical; if TRUE (default), includes full
#'   draw-by-draw uncertainty from trend parameters. If FALSE, fixes
#'   trend at posterior mean for faster computation.
#' @param ndraws Positive integer specifying number of posterior draws to
#'   use. NULL (default) uses all available draws.
#' @param re_formula Formula for random effects. NULL (default) includes
#'   all random effects, NA excludes all random effects.
#' @param allow_new_levels Logical; if TRUE, allows new factor levels in
#'   random effects grouping variables. Default FALSE.
#' @param sample_new_levels Character specifying how to handle new levels.
#'   Either "uncertainty" (default) or "gaussian".
#' @param resp Character specifying which response variable for
#'   multivariate models. NULL (default) returns predictions for all
#'   responses.
#' @param ... Additional arguments passed to internal methods.
#'
#' @return Matrix with dimensions [ndraws x nobs] containing posterior
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
#'   \item Expected values E[Y|X]: from observation + trend models
#'   \item Observation noise: family-specific random draws
#' }
#'
#' This differs from [posterior_epred.mvgam()] which returns only E[Y|X].
#' Posterior predictive samples have higher variance and are suitable for
#' checking if the model can generate data like the observed data.
#'
#' @seealso [posterior_epred.mvgam()] for expected values without noise,
#'   [posterior_linpred.mvgam()] for link-scale predictions.
#'
#' @examples
#' \dontrun{
#' # Fit a Poisson model
#' fit <- mvgam(
#'   count ~ temperature + s(day),
#'   trend_formula = ~ AR(p = 1),
#'   data = my_data,
#'   family = poisson()
#' )
#'
#' # Generate posterior predictive samples
#' pp <- posterior_predict(fit)
#' dim(pp)  # [ndraws x nobs]
#'
#' # Posterior predictive check: samples should vary more than epred
#' epred <- posterior_epred(fit)
#' var(pp) > var(epred)  # Should be TRUE
#' }
#'
#' @importFrom brms posterior_predict
#' @method posterior_predict mvgam
#' @export
posterior_predict.mvgam <- function(object, newdata = NULL,
                                    process_error = TRUE,
                                    ndraws = NULL,
                                    re_formula = NULL,
                                    allow_new_levels = FALSE,
                                    sample_new_levels = "uncertainty",
                                    resp = NULL,
                                    ...) {
  # Validate all parameters
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_data_frame(newdata, null.ok = TRUE)
  checkmate::assert_logical(process_error, len = 1)
  checkmate::assert_int(ndraws, lower = 1, null.ok = TRUE)
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
        "No training data found in model object. ",
        "Please provide {.field newdata} explicitly."
      ))
    }
    newdata <- object$data
  }

  # Get ALL draws from linpred (returns list for multivariate without resp).
  # Using linpred + inverse link (not posterior_epred) because for ZI/hurdle
  # families, posterior_epred returns E[Y]=(1-zi)*mu, but sampling requires
  # the raw mu parameter to apply zi/hu during sampling.
  linpred_all <- posterior_linpred(
    object,
    newdata = newdata,
    process_error = process_error,
    ndraws = NULL,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels,
    resp = resp
  )

  # Sample draw_ids ONCE for consistent subsampling across all responses
  if (is.list(linpred_all) && !is.matrix(linpred_all)) {
    total_draws <- nrow(linpred_all[[1]])
  } else {
    total_draws <- nrow(linpred_all)
  }

  if (!is.null(ndraws)) {
    if (ndraws > total_draws) {
      stop(insight::format_error(
        "Requested {.field ndraws} ({ndraws}) exceeds available ",
        "draws ({total_draws})."
      ))
    }
    draw_ids <- sample(total_draws, ndraws)
  } else {
    draw_ids <- seq_len(total_draws)
    ndraws <- total_draws
  }

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
#' @param linpred_resp Matrix [total_draws x nobs] of linear predictor
#' @param resp Response name (NULL for univariate)
#' @param draw_ids Integer vector of draw indices to use
#' @param ndraws Number of draws (length of draw_ids)
#' @param newdata Data frame for predictions
#' @param is_multivariate Logical; TRUE if multivariate model
#'
#' @return Matrix [ndraws x nobs] of posterior predictive samples
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

  # Get family for this response
  if (!is_multivariate) {
    family <- object$family
  } else {
    family <- get_family_for_resp(object, resp)
  }

  mu <- family$linkinv(linpred)
  family_name <- family$family

  # Get dpar names for this family
  dpar_names <- get_family_dpars(family_name)

  # For multivariate, dpars are named {dpar}_{resp} in Stan output
  if (is_multivariate) {
    dpar_names_stan <- paste0(dpar_names, "_", resp)
  } else {
    dpar_names_stan <- dpar_names
  }

  # Extract dpars with matching draw_ids
  dpars <- extract_dpars_from_stanfit(
    stanfit = object$fit,
    dpar_names = dpar_names_stan,
    ndraws = ndraws,
    nobs = nobs,
    draw_ids = draw_ids
  )

  # Rename back to standard names for sample_from_family
  if (is_multivariate && length(dpars) > 0) {
    names(dpars) <- dpar_names
  }

  # Extract trials for binomial families
  trials <- extract_trials_for_family(object, family, newdata)

  # Extract constant truncation bounds if model has truncation
  trunc_bounds <- extract_truncation_bounds(object, nobs)

  # Sample from family distribution
  samples <- sample_from_family(
    family_name = family_name,
    ndraws = ndraws,
    epred = mu,
    sigma = dpars$sigma,
    phi = dpars$phi,
    shape = dpars$shape,
    nu = dpars$nu,
    trials = trials,
    hu = dpars$hu,
    zi = dpars$zi,
    zoi = dpars$zoi,
    coi = dpars$coi,
    alpha = dpars$alpha,
    ndt = dpars$ndt,
    xi = dpars$xi,
    quantile = dpars$quantile,
    kappa = dpars$kappa,
    beta = dpars$beta,
    bs = dpars$bs,
    bias = dpars$bias,
    disc = dpars$disc,
    thres = dpars$thres,
    lb = trunc_bounds$lb,
    ub = trunc_bounds$ub
  )

  # Reshape vector to matrix [ndraws x nobs]
  matrix(samples, nrow = ndraws, ncol = nobs, byrow = FALSE)
}
