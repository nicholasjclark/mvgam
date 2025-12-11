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
#' @param linpred Matrix [ndraws x nobs] of linear predictor values, or named
#'   list of matrices for multivariate models.
#' @param family Family object with `$family` (name) and `$linkinv` (function).
#'   For multivariate, a named list of family objects keyed by response name.
#' @param sigma Optional matrix [ndraws x nobs] of sigma values. Required for
#'   lognormal family where E[Y] = exp(mu + sigma^2/2). For multivariate, a
#'   named list of matrices keyed by response name.
#' @param trials Optional vector of trial counts for binomial family where
#'   E[Y] = p * trials. Length 1 or ncol(linpred).
#'
#' @return Matrix [ndraws x nobs] of expected values on response scale, or
#'   named list of matrices for multivariate models.
#'
#' @noRd
compute_family_epred <- function(linpred, family,
                                 sigma = NULL, trials = NULL) {
  # Handle multivariate case (list of linpred matrices)
  if (is.list(linpred) && !is.matrix(linpred)) {
    checkmate::assert_list(family, names = "named")

    result <- lapply(names(linpred), function(resp_name) {
      compute_family_epred(
        linpred = linpred[[resp_name]],
        family = family[[resp_name]],
        sigma = if (!is.null(sigma)) sigma[[resp_name]] else NULL,
        trials = trials
      )
    })
    names(result) <- names(linpred)
    return(result)
  }

  # Univariate case: validate inputs
  checkmate::assert_matrix(linpred)

  # Validate family structure
  if (is.null(family$family) || is.null(family$linkinv)) {
    stop(insight::format_error(
      "{.field family} object missing required components: ",
      "{.field $family} and {.field $linkinv}."
    ))
  }

  family_name <- family$family
  checkmate::assert_string(family_name)

  # Validate sigma when provided
  if (!is.null(sigma)) {
    checkmate::assert_matrix(sigma)
    if (nrow(sigma) != nrow(linpred) || ncol(sigma) != ncol(linpred)) {
      stop(insight::format_error(
        "Dimension mismatch: {.field sigma} is ",
        "[{nrow(sigma)} x {ncol(sigma)}] but ",
        "{.field linpred} is [{nrow(linpred)} x {ncol(linpred)}]."
      ))
    }
  }

  # Validate trials when provided
  if (!is.null(trials)) {
    checkmate::assert_numeric(trials, min.len = 1)
    if (length(trials) != 1 && length(trials) != ncol(linpred)) {
      stop(insight::format_error(
        "{.field trials} must be length 1 or match number of ",
        "observations ({ncol(linpred)})."
      ))
    }
  }

  # Dispatch based on family
  epred <- switch(
    family_name,

    # Simple families: E[Y] = g^-1(eta)
    "gaussian" = ,
    "poisson" = ,
    "bernoulli" = ,
    "beta" = ,
    "Gamma" = ,
    "gamma" = ,
    "negbinomial" = ,
    "negative binomial" = ,
    "student" = family$linkinv(linpred),

    # Count families requiring trials: E[Y] = p * trials
    "binomial" = ,
    "beta_binomial" = {
      if (is.null(trials)) {
        stop(insight::format_error(
          "Family {.val {family_name}} requires {.field trials} argument ",
          "for computing expected values."
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

    # Lognormal: E[Y] = exp(mu + sigma^2/2)
    "lognormal" = {
      if (is.null(sigma)) {
        stop(insight::format_error(
          "Family {.val {family_name}} requires {.field sigma} argument ",
          "for computing expected values. E[Y] = exp(mu + sigma^2/2)."
        ))
      }
      exp(linpred + sigma^2 / 2)
    },

    # Unsupported families
    "nmix" = stop(insight::format_error(
      "Family {.val nmix} is not yet supported for {.fn posterior_epred}. ",
      "N-mixture models require specialized expected value computation."
    )),

    "tweedie" = stop(insight::format_error(
      "Family {.val tweedie} is not yet supported for {.fn posterior_epred}."
    )),

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
#' @return Matrix with dimensions [ndraws x nobs] containing expected
#'   values on response scale. Each row is one posterior draw, each column
#'   is one observation from newdata.
#'
#'   For multivariate models with resp = NULL, returns a named list of
#'   matrices (one per response variable).
#'
#' @details
#' Expected values are computed as E[Y|X] by applying the inverse link
#' function to the linear predictor. For most families this is simply
#' `linkinv(eta)`:
#' \itemize{
#'   \item Gaussian, Poisson, Bernoulli, Beta, Gamma, NegBinomial, Student:
#'     E[Y] = linkinv(eta)
#' }
#'
#' Some families require additional parameters:
#' \itemize{
#'   \item Binomial, Beta-binomial: E[Y] = p * trials (requires trials)
#'   \item Lognormal: E[Y] = exp(mu + sigma^2/2) (requires sigma)
#' }
#'
#' Unsupported families:
#' \itemize{
#'   \item N-mixture, Tweedie: require specialized computation
#' }
#'
#' @seealso [posterior_linpred.mvgam()] for link scale predictions,
#'   [posterior_predict.mvgam()] for posterior predictive samples.
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
#' # Extract expected values (response scale)
#' epred <- posterior_epred(fit)
#'
#' # For Poisson: epred = exp(linpred)
#' linpred <- posterior_linpred(fit)
#' all.equal(epred, exp(linpred))
#' }
#'
#' @importFrom brms posterior_epred
#' @method posterior_epred mvgam
#' @export
posterior_epred.mvgam <- function(object, newdata = NULL,
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

  # Get linear predictor (handles obs+trend combination)
  linpred <- get_combined_linpred(
    mvgam_fit = object,
    newdata = newdata,
    process_error = process_error,
    ndraws = ndraws,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels,
    resp = resp
  )

  # Extract family information for transformation
  is_multivariate <- inherits(object$formula, "mvbrmsformula") &&
    !is.null(object$formula$forms) &&
    length(object$formula$forms) > 1

  if (is_multivariate) {
    # Multivariate: extract per-response families
    # Family can be embedded in bf() or shared at top-level object$family
    family <- lapply(names(linpred), function(resp_name) {
      form <- object$formula$forms[[resp_name]]

      # Use response-specific family if embedded in bf(), else fall back
      if (!is.null(form$family)) {
        form$family
      } else if (!is.null(object$family)) {
        object$family
      } else {
        stop(insight::format_error(
          "No family found for response {.val {resp_name}}. ",
          "Family must be specified either in {.fn bf} or at top-level."
        ))
      }
    })
    names(family) <- names(linpred)
  } else {
    # Univariate: single family object
    family <- object$family
  }

  # Transform to response scale
  # compute_family_epred() throws appropriate errors for unsupported families
  compute_family_epred(
    linpred = linpred,
    family = family,
    sigma = NULL,
    trials = NULL
  )
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
        "Length of {.field x} must be 1 or {dim[2]}, not {length(x)}."
      ))
    }
    matrix(x, nrow = dim[1], ncol = dim[2], byrow = TRUE)
  } else {
    # Expand to 3D array for categorical/compositional families
    # dim[1] = ndraws, dim[2:3] = observation dimensions
    if (!(length(x) == 1 || identical(dim(x), as.integer(dim[2:3])))) {
      stop(insight::format_error(
        "Dimension of {.field x} must match dim[2:3]."
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
#' Computes E[Y] for discrete Weibull via truncated series summation.
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
#' Computes E[Y] for Conway-Maxwell-Poisson via series approximation.
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
