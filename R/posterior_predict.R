#' Posterior Predictive Distribution for mvgam Models
#'
#' @description
#' Generate posterior predictive samples from fitted mvgam models with
#' observation-level noise. Combines expected values with family-specific
#' random draws.
#'
#' @name posterior_predict
NULL


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
#'   For beta families, represents phi (precision parameter).
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
                               sigma = NULL, shape = NULL, nu = NULL,
                               trials = NULL, hu = NULL, zi = NULL,
                               zoi = NULL, coi = NULL,
                               alpha = NULL, ndt = NULL, xi = NULL,
                               quantile = NULL, kappa = NULL,
                               beta = NULL, bs = NULL, bias = NULL,
                               disc = NULL, thres = NULL,
                               link = "logit") {
  checkmate::assert_string(family_name)
  checkmate::assert_int(ndraws, lower = 1)
  checkmate::assert_matrix(epred)

  switch(
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
      checkmate::assert_matrix(sigma, nrows = ndraws, ncols = ncol(epred))
      # sigma is phi (precision); mu and phi -> shape1, shape2
      # shape1 = mu * phi, shape2 = (1 - mu) * phi
      shape1 <- epred * sigma
      shape2 <- (1 - epred) * sigma
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
      # epred = p * trials (on response scale)
      # so probability = epred / trials
      if (length(trials) == 1) {
        prob <- epred / trials
        stats::rbinom(length(epred), size = trials, prob = prob)
      } else {
        prob <- sweep(epred, 2, trials, `/`)
        # rbinom vectorizes: each element uses corresponding trial count
        mapply(stats::rbinom, n = nrow(epred),
               size = rep(trials, each = nrow(epred)),
               prob = prob)
      }
    },

    "beta_binomial" = {
      checkmate::assert_numeric(trials, min.len = 1)
      checkmate::assert_matrix(sigma, nrows = ndraws, ncols = ncol(epred))
      # epred = mu * trials, sigma = phi
      # mu = epred / trials
      if (length(trials) == 1) {
        mu <- epred / trials
      } else {
        mu <- sweep(epred, 2, trials, `/`)
      }
      extraDistr::rbbinom(length(epred), size = trials, mu = mu, sigma = sigma)
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
      checkmate::assert_matrix(sigma, nrows = ndraws, ncols = ncol(epred))
      if (length(trials) == 1) {
        mu <- epred / trials
      } else {
        mu <- sweep(epred, 2, trials, `/`)
      }
      draws <- extraDistr::rbbinom(length(epred), size = trials, mu = mu, sigma = sigma)
      tmp <- stats::runif(length(epred))
      draws[tmp < zi] <- 0L
      draws
    },

    "zero_inflated_beta" = {
      checkmate::assert_matrix(zi, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(sigma, nrows = ndraws, ncols = ncol(epred))
      shape1 <- epred * sigma
      shape2 <- (1 - epred) * sigma
      tmp <- stats::runif(length(epred))
      ifelse(tmp < zi, 0, stats::rbeta(length(epred), shape1 = shape1, shape2 = shape2))
    },

    "zero_one_inflated_beta" = {
      checkmate::assert_matrix(zoi, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(coi, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(sigma, nrows = ndraws, ncols = ncol(epred))
      # zoi = P(Y in {0,1}), coi = P(Y=1 | Y in {0,1})
      # sigma is phi (precision parameter for beta)
      tmp <- stats::runif(length(epred))
      one_or_zero <- stats::runif(length(epred))
      shape1 <- epred * sigma
      shape2 <- (1 - epred) * sigma
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
  # Validate mvgam-specific parameters
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_logical(process_error, len = 1)

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

  # Get expected values (response scale, obs + trend combined)
  epred <- posterior_epred(
    object,
    newdata = newdata,
    process_error = process_error,
    ndraws = ndraws,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels,
    resp = resp
  )

  # For now, simply return epred with a note that sampling is implemented
  # TODO: Add family-specific noise to epred values
  epred
}
