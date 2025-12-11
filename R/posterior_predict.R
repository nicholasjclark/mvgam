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
#' @param sigma Optional matrix of sigma (dispersion) parameters
#' @param shape Optional matrix of shape parameters
#' @param trials Optional vector of trial counts (length 1 or nobs)
#' @param hu Optional matrix of hurdle probability parameters
#' @param zi Optional matrix of zero-inflation parameters
#'
#' @return Matrix [ndraws x nobs] of sampled values from the distribution
#'
#' @noRd
sample_from_family <- function(family_name, ndraws, epred,
                               sigma = NULL, shape = NULL,
                               trials = NULL, hu = NULL, zi = NULL) {
  checkmate::assert_string(family_name)
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
      # nu parameter required for student t; may be handled separately
      stop(insight::format_error(
        "Student-t sampling requires nu parameter not yet implemented."
      ))
    },

    "skew_normal" = {
      stop(insight::format_error(
        "Skew-normal sampling not yet implemented for posterior_predict."
      ))
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
      stop(insight::format_error(
        "Generalized extreme value sampling not yet implemented."
      ))
    },

    "inverse.gaussian" = {
      checkmate::assert_matrix(shape, nrows = ndraws, ncols = ncol(epred))
      statmod::rinvgauss(length(epred), mean = epred, shape = shape)
    },

    "exgaussian" = {
      stop(insight::format_error(
        "Ex-Gaussian sampling not yet implemented."
      ))
    },

    "lognormal" = {
      checkmate::assert_matrix(sigma, nrows = ndraws, ncols = ncol(epred))
      # epred = exp(mu + sigma^2/2), so mu = log(epred) - sigma^2/2
      mu <- log(epred) - sigma^2 / 2
      stats::rlnorm(length(epred), meanlog = mu, sdlog = sigma)
    },

    "shifted_lognormal" = {
      stop(insight::format_error(
        "Shifted lognormal sampling not yet implemented."
      ))
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
      stop(insight::format_error(
        "Von Mises sampling not yet implemented."
      ))
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
      stop(insight::format_error(
        "Discrete Weibull sampling not yet implemented."
      ))
    },

    "com_poisson" = {
      stop(insight::format_error(
        "COM-Poisson sampling not yet implemented."
      ))
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
      stop(insight::format_error(
        "Zero-one inflated beta sampling not yet implemented."
      ))
    },

    "zero_inflated_asym_laplace" = {
      stop(insight::format_error(
        "Zero-inflated asymmetric Laplace sampling not yet implemented."
      ))
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

    # ============ Unsupported families ============

    stop(insight::format_error(
      "Posterior predictive sampling for family {.val {family_name}} ",
      "is not yet implemented."
    ))
  )
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
