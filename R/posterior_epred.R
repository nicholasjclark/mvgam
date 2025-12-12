#' Posterior Expected Values for mvgam Models
#'
#' @description
#' Extract expected values (on response scale) from fitted mvgam models.
#' Combines observation and trend components, then applies family-appropriate
#' transformations to convert from link scale to response scale.
#'
#' @name posterior_epred
NULL


#' Extract Family for Single Response
#'
#' Extracts the family object for a specific response from an mvgam model.
#' Response-specific families embedded in bf() take precedence over the
#' shared top-level family.
#'
#' @param object A fitted mvgam object.
#' @param resp_name Response variable name (character length 1).
#'
#' @return A single family object with `$family` and `$linkinv` components.
#'
#' @noRd
get_family_for_resp <- function(object, resp_name) {
  checkmate::assert_string(resp_name)

  form <- object$formula$forms[[resp_name]]
  if (!is.null(form) && !is.null(form$family)) {
    form$family
  } else if (!is.null(object$family)) {
    object$family
  } else {
    stop(insight::format_error(
      "No family found for response {.val {resp_name}}. ",
      "Family must be specified either in {.fn bf} or at top-level."
    ))
  }
}


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
    "student" = ,
    # Hurdle families: E[Y|Y>0] = g^-1(eta) for the count component
    "hurdle_poisson" = ,
    "hurdle_negbinomial" = ,
    "hurdle_gamma" = ,
    "hurdle_lognormal" = ,
    # Zero-inflated families: E[Y|Y>0] = g^-1(eta) for the count component
    "zero_inflated_poisson" = ,
    "zero_inflated_negbinomial" = ,
    "zero_inflated_binomial" = ,
    "zero_inflated_beta" = family$linkinv(linpred),

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

    # Ordinal families require threshold parameters for category probability
    # computation. Routing in posterior_epred.mvgam() handles these families
    # before compute_family_epred() is called.
    "cumulative" = ,
    "sratio" = ,
    "cratio" = ,
    "acat" = stop(insight::format_error(
      "Family {.val {family_name}} is not yet supported for ",
      "{.fn posterior_epred}. Ordinal models require threshold parameters."
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
  # Use resp argument as single source of truth for response selection
  is_mv <- inherits(object$formula, "mvbrmsformula") &&
    !is.null(object$formula$forms) &&
    length(object$formula$forms) > 1

  if (!is_mv) {
    # Univariate model
    family <- object$family
  } else if (!is.null(resp)) {
    # Multivariate with response filter
    family <- get_family_for_resp(object, resp)
  } else {
    # Multivariate returning all responses
    resp_names <- names(object$formula$forms)
    family <- lapply(resp_names, function(r) get_family_for_resp(object, r))
    names(family) <- resp_names
  }

  # Ordinal families require threshold parameters for probability computation
  # These transform 2D linpred [ndraws x nobs] to 3D category probabilities
  # [ndraws x nobs x ncat] using threshold-based cumulative models
  is_list_family <- is.list(family) &&
    !inherits(family, "family") &&
    !inherits(family, "brmsfamily")

  if (is_list_family) {
    # Multivariate case without resp specified
    if (any(vapply(family, is_ordinal_family, logical(1)))) {
      stop(insight::format_error(
        "Ordinal families in multivariate models require ",
        "{.field resp} parameter to specify which response variable."
      ))
    }
  } else if (is_ordinal_family(family)) {
    # Univariate ordinal or multivariate with resp specified
    ndraws_actual <- nrow(linpred)
    nobs_actual <- ncol(linpred)

    # Extract ordinal-specific parameters
    thres <- extract_ordinal_thresholds(object, ndraws_actual)
    disc <- extract_ordinal_disc(object, ndraws_actual, nobs_actual)

    # Validate extracted dimensions match linear predictor
    if (nrow(thres) != ndraws_actual) {
      stop(insight::format_error(
        "Threshold extraction returned {nrow(thres)} draws but ",
        "expected {ndraws_actual} to match linear predictor."
      ))
    }
    if (nrow(disc) != ndraws_actual || ncol(disc) != nobs_actual) {
      stop(insight::format_error(
        "Discrimination parameter dimensions [{nrow(disc)} x {ncol(disc)}] ",
        "do not match expected [{ndraws_actual} x {nobs_actual}]."
      ))
    }

    # Build prep object expected by posterior_epred_ordinal()
    prep <- list(
      dpars = list(
        mu = linpred,
        thres = thres,
        disc = disc
      ),
      family = family,
      ndraws = ndraws_actual,
      nobs = nobs_actual,
      data = list(
        nthres = rep(ncol(thres), nobs_actual)
      )
    )

    return(posterior_epred_ordinal(prep))
  }

  # Extract trials for binomial families
  trials <- extract_trials_for_family(object, family, newdata)

  # Transform to response scale
  compute_family_epred(
    linpred = linpred,
    family = family,
    sigma = NULL,
    trials = trials
  )
}

#' Extract trials from model object or newdata for binomial families
#'
#' @param object mvgam model object
#' @param family Family object or list of families (for multivariate)
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
  checkmate::assert(
    checkmate::check_class(family, "family"),
    checkmate::check_class(family, "brmsfamily"),
    checkmate::check_list(family, types = c("family", "brmsfamily"))
  )
  checkmate::assert_data_frame(newdata, null.ok = TRUE)

  # Determine if family requires trials
  # For multivariate: family is a named list of family objects
  # For univariate: family is a single family object

  is_mv_family <- is.list(family) &&
    !inherits(family, "family") &&
    !inherits(family, "brmsfamily") &&
    !is.null(names(family))

  family_name <- if (is_mv_family) {
    family[[1]]$family
  } else {
    family$family
  }

  binomial_families <- c("binomial", "beta_binomial",
                         "zero_inflated_binomial",
                         "zero_inflated_beta_binomial")

  if (!family_name %in% binomial_families) {
    return(NULL)
  }

  # Extract trials from standata or newdata
  trials <- NULL

  if (is.null(newdata) || identical(newdata, object$data)) {
    trials <- object$standata$trials %||% object$data$trials
  } else {
    if ("trials" %in% names(newdata)) {
      trials <- newdata$trials
    }
  }

  if (is.null(trials)) {
    stop(insight::format_error(
      "Family {.val {family_name}} requires {.field trials} data.",
      "Ensure {.code trials} is present in your data or use ",
      "{.code y | trials(n) ~ ...} formula syntax."
    ))
  }

  # Validate trials values
  checkmate::assert_numeric(trials, lower = 1, finite = TRUE,
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

# ============================================================================
# Ordinal and Categorical Family Support
# ============================================================================
# Functions for ordinal families (cumulative, sratio, cratio, acat) and
# categorical/compositional families (categorical, multinomial, dirichlet).
#
# Adapted from brms package (Buerkner, 2017) with modifications for mvgam.
# Original author: Paul-Christian Buerkner (brms package)
# ============================================================================

# --- Ordinal Family Detection and Threshold Extraction ---
# Called from posterior_epred.mvgam() when family is ordinal.
# These functions extract threshold parameters from posterior draws to
# build the prep object expected by posterior_epred_ordinal().

#' Check if Family is Ordinal
#'
#' Determines whether a family object represents an ordinal model
#' (cumulative, sratio, cratio, or acat).
#'
#' @param family A family or brmsfamily object
#'
#' @return Logical TRUE if ordinal, FALSE otherwise
#'
#' @noRd
is_ordinal_family <- function(family) {
  checkmate::assert(
    checkmate::check_class(family, "family"),
    checkmate::check_class(family, "brmsfamily"),
    checkmate::check_null(family),
    combine = "or"
  )

  if (is.null(family) || is.null(family$family)) {
    return(FALSE)
  }
  family$family %in% c("cumulative", "sratio", "cratio", "acat")
}


#' Extract Ordinal Threshold Parameters from Posterior Draws
#'
#' Extracts threshold parameters (intercepts) from an mvgam model's
#' posterior draws. For ordinal models, thresholds are stored as
#' `b_Intercept[1]`, `b_Intercept[2]`, etc. in the Stan output.
#'
#' @param object An mvgam model object
#' @param ndraws Optional number of draws to return. If NULL, all draws.
#'
#' @return Matrix [ndraws x nthres] of threshold values
#'
#' @details
#' For an ordinal model with K categories, there are K-1 thresholds.
#' These define cutpoints on the latent scale separating categories.
#' Thresholds are ordered: theta_1 < theta_2 < ... < theta_{K-1}.
#'
#' @noRd
extract_ordinal_thresholds <- function(object, ndraws = NULL) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_int(ndraws, lower = 1, null.ok = TRUE)

  # Get posterior draws from the stanfit stored in object$fit
  draws_mat <- posterior::as_draws_matrix(object$fit)

  # Find threshold parameter columns
  # brms uses Intercept[k] for ordinal thresholds (NOT b_Intercept[k])
  all_cols <- colnames(draws_mat)
  thres_pattern <- "^Intercept\\[\\d+\\]$"
  thres_cols <- grep(thres_pattern, all_cols, value = TRUE)

  if (length(thres_cols) == 0) {
    stop(insight::format_error(
      "No threshold parameters found in model.",
      "Expected parameters matching pattern {.code b_Intercept[k]}.",
      "Is this an ordinal family model?"
    ))
  }

  # Sort columns by index to ensure correct ordering
  indices <- as.integer(gsub(".*\\[(\\d+)\\].*", "\\1", thres_cols))
  thres_cols <- thres_cols[order(indices)]

  # Extract as matrix
  thres_matrix <- as.matrix(draws_mat[, thres_cols, drop = FALSE])

  # Subsample draws if requested (use seq_len for consistency with disc)
  if (!is.null(ndraws) && ndraws < nrow(thres_matrix)) {
    thres_matrix <- thres_matrix[seq_len(ndraws), , drop = FALSE]
  }

  # Remove column names - prep$dpars$thres expects unnamed matrix
  colnames(thres_matrix) <- NULL

  thres_matrix
}


#' Extract Discrimination Parameter from Posterior Draws
#'
#' Extracts the discrimination parameter for ordinal models. If not
#' present (standard ordinal models), returns a matrix of 1s.
#'
#' @param object An mvgam model object
#' @param ndraws Number of draws (must match threshold extraction)
#' @param nobs Number of observations for matrix dimensions
#'
#' @return Matrix [ndraws x nobs] of discrimination values
#'
#' @details
#' The discrimination parameter scales the linear predictor in ordinal
#' models. When disc = 1 (default), the model is standard cumulative.
#' When disc > 1, category boundaries are sharper; disc < 1, smoother.
#'
#' @noRd
extract_ordinal_disc <- function(object, ndraws, nobs) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_int(ndraws, lower = 1)
  checkmate::assert_int(nobs, lower = 1)

  # Get posterior draws from the stanfit stored in object$fit
  draws_mat <- posterior::as_draws_matrix(object$fit)

  # Look for discrimination parameter
  disc_pattern <- "^disc$|^disc\\["
  all_cols <- colnames(draws_mat)
  disc_cols <- grep(disc_pattern, all_cols, value = TRUE)

  if (length(disc_cols) > 0) {
    # Extract discrimination draws
    disc_draws <- as.matrix(draws_mat[, disc_cols[1], drop = FALSE])

    # Validate ndraws doesn't exceed available draws
    if (ndraws > nrow(disc_draws)) {
      stop(insight::format_error(
        "Requested {ndraws} draws but only {nrow(disc_draws)} available."
      ))
    }

    # Subsample if needed (use seq_len for consistency with thresholds)
    disc_draws <- disc_draws[seq_len(ndraws), , drop = FALSE]

    # Expand to [ndraws x nobs] if scalar disc
    if (ncol(disc_draws) == 1) {
      disc_matrix <- matrix(
        disc_draws[, 1],
        nrow = ndraws,
        ncol = nobs,
        byrow = FALSE
      )
    } else {
      disc_matrix <- disc_draws
    }
  } else {
    # Default discrimination = 1 for standard ordinal models
    disc_matrix <- matrix(1.0, nrow = ndraws, ncol = nobs)
  }

  disc_matrix
}


# --- Ordinal Helper Functions ---

#' Extract Column Slice from Matrix
#'
#' Extracts column i from a matrix, preserving the result as a 1-column matrix
#' rather than dropping to a vector.
#'
#' @param x Matrix to slice
#' @param i Column index to extract
#' @return Matrix with single column
#' @noRd
slice_col <- function(x, i) {
  if (is.null(x)) {
    return(NULL)
  }
  checkmate::assert(
    checkmate::check_matrix(x),
    checkmate::check_array(x, d = 3),
    combine = "or"
  )
  checkmate::assert_int(i, lower = 1)

  if (is.matrix(x)) {
    x[, i, drop = FALSE]
  } else if (is.array(x) && length(dim(x)) == 3) {
    x[, i, , drop = FALSE]
  } else {
    x
  }
}

#' Sequence of Column Indices
#'
#' Returns sequence from 1 to number of columns (or 2nd dimension for arrays).
#'
#' @param x Matrix or array
#' @return Integer sequence
#' @noRd
seq_cols <- function(x) {
  checkmate::assert(
    checkmate::check_matrix(x),
    checkmate::check_array(x, d = 3),
    combine = "or"
  )

  if (is.matrix(x)) {
    seq_len(ncol(x))
  } else if (is.array(x) && length(dim(x)) == 3) {
    seq_len(dim(x)[2])
  } else {
    seq_len(length(x))
  }
}

#' Insert Reference Category into Eta Matrix
#'
#' For categorical models, the linear predictor eta has ncat-1 columns (one
#' per non-reference category). This function inserts zeros for the reference
#' category to create a full ncat-column matrix for softmax transformation.
#'
#' @param eta Matrix [ndraws x (ncat-1)] or array [ndraws x nobs x (ncat-1)]
#' @param refcat Integer index of reference category (typically 1)
#' @return Matrix [ndraws x ncat] or array [ndraws x nobs x ncat] with zeros
#'   inserted at reference category position
#' @noRd
insert_refcat <- function(eta, refcat = 1) {
  if (is.null(refcat)) {
    return(eta)
  }
  checkmate::assert(
    checkmate::check_matrix(eta),
    checkmate::check_array(eta, d = 3),
    combine = "or"
  )
  checkmate::assert_int(refcat, lower = 1)

  ndim <- length(dim(eta))
  if (ndim == 2) {
    # Matrix case: [ndraws x (ncat-1)]
    ndraws <- nrow(eta)
    ncat <- ncol(eta) + 1
    zeros <- matrix(0, nrow = ndraws, ncol = 1)

    if (refcat == 1) {
      out <- cbind(zeros, eta)
    } else if (refcat == ncat) {
      out <- cbind(eta, zeros)
    } else {
      out <- cbind(
        eta[, seq_len(refcat - 1), drop = FALSE],
        zeros,
        eta[, refcat:(ncat - 1), drop = FALSE]
      )
    }
  } else if (ndim == 3) {
    # Array case: [ndraws x nobs x (ncat-1)]
    ndraws <- dim(eta)[1]
    nobs <- dim(eta)[2]
    ncat <- dim(eta)[3] + 1
    zeros <- array(0, dim = c(ndraws, nobs, 1))

    if (refcat == 1) {
      out <- abind::abind(zeros, eta, along = 3)
    } else if (refcat == ncat) {
      out <- abind::abind(eta, zeros, along = 3)
    } else {
      out <- abind::abind(
        eta[, , seq_len(refcat - 1), drop = FALSE],
        zeros,
        eta[, , refcat:(ncat - 1), drop = FALSE],
        along = 3
      )
    }
  } else {
    stop(insight::format_error(
      "eta must be a matrix or 3D array, not {class(eta)[1]}."
    ))
  }

  out
}

#' Log-Softmax Transformation
#'
#' Computes log(softmax(x)) in a numerically stable way by subtracting
#' the log-sum-exp from each element.
#'
#' @param x Matrix [ndraws x ncat] or array [ndraws x nobs x ncat]
#' @return Matrix or array of same dimensions with log-softmax applied
#' @noRd
log_softmax <- function(x) {
  checkmate::assert(
    checkmate::check_matrix(x),
    checkmate::check_array(x, d = 3),
    combine = "or"
  )

  ndim <- length(dim(x))
  if (ndim == 2) {
    # Matrix case: apply across columns (categories)
    max_x <- apply(x, 1, max)
    x_centered <- x - max_x
    log_sum_exp <- log(rowSums(exp(x_centered)))
    x_centered - log_sum_exp
  } else if (ndim == 3) {
    # Array case: apply across 3rd dimension (categories)
    max_x <- apply(x, c(1, 2), max)
    x_centered <- x - as.vector(max_x)
    log_sum_exp <- log(apply(exp(x_centered), c(1, 2), sum))
    x_centered - as.vector(log_sum_exp)
  } else {
    stop(insight::format_error(
      "x must be a matrix or 3D array for log_softmax."
    ))
  }
}

#' Generic Inverse Link Function
#'
#' Applies the inverse of common link functions.
#'
#' @param x Numeric matrix or array
#' @param link Character string naming the link function
#' @return Transformed values
#' @noRd
inv_link <- function(x, link) {
  checkmate::assert_numeric(x)
  checkmate::assert_string(link)

  switch(
    link,
    logit = stats::plogis(x),
    probit = stats::pnorm(x),
    probit_approx = stats::pnorm(x),
    cloglog = 1 - exp(-exp(x)),
    cauchit = stats::pcauchy(x),
    identity = x,
    log = exp(x),
    inverse = 1 / x,
    sqrt = x^2,
    stop(insight::format_error("Unknown link function: {.val {link}}."))
  )
}

#' Subset Thresholds for Observation i
#'
#' Extracts threshold parameters for a specific observation, handling both
#' fixed and varying threshold cases.
#'
#' @param prep Prediction preparation object
#' @param i Observation index
#' @return Matrix [ndraws x nthres] of threshold values
#' @noRd
subset_thres <- function(prep, i) {
  checkmate::assert_list(prep)
  checkmate::assert_int(i, lower = 1)

  thres <- prep$dpars$thres
  if (is.null(thres)) {
    return(NULL)
  }

  # If thresholds vary by observation (3D array), extract for observation i
  if (length(dim(thres)) == 3) {
    thres[, i, , drop = TRUE]
  } else {
    # Fixed thresholds across observations
    thres
  }
}

# --- Ordinal Distribution Density Functions ---
# Adapted from brms::distributions.R

#' Cumulative Model Density
#'
#' Computes category probabilities for cumulative ordinal models.
#' P(Y = k) = F(theta_k - eta) - F(theta_{k-1} - eta)
#'
#' @param x Integer vector of category indices
#' @param eta Vector [ndraws] of linear predictor values
#' @param thres Matrix [ndraws x nthres] of threshold values
#' @param disc Vector [ndraws] of discrimination parameter (default 1)
#' @param link Character link function name
#' @return Matrix [ndraws x length(x)] of category probabilities
#' @noRd
dcumulative <- function(x, eta, thres, disc = 1, link = "logit") {
  checkmate::assert_integerish(x, lower = 1)
  checkmate::assert_numeric(eta)
  checkmate::assert_matrix(thres)
  checkmate::assert_numeric(disc)
  checkmate::assert_string(link)

  # Drop dimensions if eta/disc are 1-column matrices (from slice_col)
  eta <- drop(eta)
  disc <- drop(disc)

  eta <- disc * (thres - eta)
  if (link == "identity") {
    out <- eta
  } else {
    out <- inv_link_cumulative(eta, link = link)
  }
  out[, x, drop = FALSE]
}

#' Inverse Link for Cumulative Models
#'
#' Transforms cumulative probabilities to category probabilities.
#'
#' @param x Matrix [ndraws x nthres] of disc * (thres - eta)
#' @param link Character link function name
#' @return Matrix [ndraws x ncat] of category probabilities
#' @noRd
inv_link_cumulative <- function(x, link) {
  checkmate::assert_numeric(x)
  checkmate::assert_string(link)

  x <- inv_link(x, link)
  ndim <- length(dim(x))
  dim_noncat <- dim(x)[-ndim]
  ones_arr <- array(1, dim = c(dim_noncat, 1))
  zeros_arr <- array(0, dim = c(dim_noncat, 1))
  abind::abind(x, ones_arr) - abind::abind(zeros_arr, x)
}

#' Sequential Ratio Model Density
#'
#' Computes category probabilities for stopping ratio ordinal models.
#' P(Y = k | Y >= k) = F(theta_k - eta)
#'
#' @inheritParams dcumulative
#' @return Matrix [ndraws x length(x)] of category probabilities
#' @noRd
dsratio <- function(x, eta, thres, disc = 1, link = "logit") {
  checkmate::assert_integerish(x, lower = 1)
  checkmate::assert_numeric(eta)
  checkmate::assert_matrix(thres)
  checkmate::assert_numeric(disc)
  checkmate::assert_string(link)

  eta <- disc * (thres - eta)
  if (link == "identity") {
    out <- eta
  } else {
    out <- inv_link_sratio(eta, link = link)
  }
  out[, x, drop = FALSE]
}

#' Inverse Link for Sequential Ratio Models
#'
#' @inheritParams inv_link_cumulative
#' @return Matrix [ndraws x ncat] of category probabilities
#' @noRd
inv_link_sratio <- function(x, link) {
  checkmate::assert_numeric(x)
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

#' Continuation Ratio Model Density
#'
#' Computes category probabilities for continuation ratio ordinal models.
#' P(Y = k | Y <= k) = 1 - F(theta_k - eta)
#'
#' @inheritParams dcumulative
#' @return Matrix [ndraws x length(x)] of category probabilities
#' @noRd
dcratio <- function(x, eta, thres, disc = 1, link = "logit") {
  checkmate::assert_integerish(x, lower = 1)
  checkmate::assert_numeric(eta)
  checkmate::assert_matrix(thres)
  checkmate::assert_numeric(disc)
  checkmate::assert_string(link)

  eta <- disc * (eta - thres)
  if (link == "identity") {
    out <- eta
  } else {
    out <- inv_link_cratio(eta, link = link)
  }
  out[, x, drop = FALSE]
}

#' Inverse Link for Continuation Ratio Models
#'
#' @inheritParams inv_link_cumulative
#' @return Matrix [ndraws x ncat] of category probabilities
#' @noRd
inv_link_cratio <- function(x, link) {
  checkmate::assert_numeric(x)
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

#' Adjacent Category Model Density
#'
#' Computes category probabilities for adjacent category ordinal models.
#' log(P(Y=k) / P(Y=k+1)) = theta_k - eta
#'
#' @inheritParams dcumulative
#' @return Matrix [ndraws x length(x)] of category probabilities
#' @noRd
dacat <- function(x, eta, thres, disc = 1, link = "logit") {
  checkmate::assert_integerish(x, lower = 1)
  checkmate::assert_numeric(eta)
  checkmate::assert_matrix(thres)
  checkmate::assert_numeric(disc)
  checkmate::assert_string(link)

  eta <- disc * (eta - thres)
  if (link == "identity") {
    out <- eta
  } else {
    out <- inv_link_acat(eta, link = link)
  }
  out[, x, drop = FALSE]
}

#' Inverse Link for Adjacent Category Models
#'
#' @inheritParams inv_link_cumulative
#' @return Matrix [ndraws x ncat] of category probabilities
#' @noRd
inv_link_acat <- function(x, link) {
  checkmate::assert_numeric(x)
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

# --- Categorical Distribution Functions ---

#' Categorical Distribution Density
#'
#' Computes category probabilities from linear predictor using softmax.
#'
#' @param x Integer vector of category indices
#' @param eta Matrix [ndraws x ncat] of linear predictor values
#' @param log Logical; return log probabilities?
#' @return Matrix [ndraws x length(x)] of category probabilities
#' @noRd
dcategorical <- function(x, eta, log = FALSE) {
  checkmate::assert_integerish(x, lower = 1)
  checkmate::assert_logical(log, len = 1)

  if (is.null(dim(eta))) {
    eta <- matrix(eta, nrow = 1)
  }
  if (length(dim(eta)) != 2L) {
    stop(insight::format_error(
      "eta must be a numeric vector or matrix."
    ))
  }
  out <- inv_link_categorical(eta, log = log, refcat = NULL)
  out[, x, drop = FALSE]
}

#' Inverse Link for Categorical Models
#'
#' Applies softmax transformation to convert linear predictors to
#'   probabilities.
#'
#' @param x Matrix [ndraws x ncat] or array [ndraws x nobs x ncat]
#' @param refcat Integer reference category index (NULL if already complete)
#' @param log Logical; return log probabilities?
#' @return Matrix or array of category probabilities
#' @noRd
inv_link_categorical <- function(x, refcat = 1, log = FALSE) {
  checkmate::assert(
    checkmate::check_matrix(x),
    checkmate::check_array(x, d = 3),
    combine = "or"
  )
  checkmate::assert_int(refcat, lower = 1, null.ok = TRUE)
  checkmate::assert_logical(log, len = 1)

  if (!is.null(refcat)) {
    x <- insert_refcat(x, refcat = refcat)
  }
  out <- log_softmax(x)
  if (!log) {
    out <- exp(out)
  }
  out
}

#' Get Mu Parameter for Categorical Models
#'
#' Extracts the Mu parameter which may be a 3D array for categorical models.
#'
#' @param prep Prediction preparation object
#' @return Array [ndraws x nobs x (ncat-1)] or matrix
#' @noRd
get_Mu <- function(prep) {
  checkmate::assert_list(prep)
  prep$dpars$mu
}

# --- Ordinal posterior_epred Functions ---

#' Posterior Expected Values for Ordinal Models
#'
#' Main helper function for computing category probabilities in ordinal models.
#' Returns a 3D array [ndraws x nobs x ncat] of category probabilities.
#'
#' @param prep Prediction preparation object containing:
#'   - dpars$mu: linear predictor matrix [ndraws x nobs]
#'   - dpars$disc: discrimination parameter (optional)
#'   - dpars$thres: threshold matrix [ndraws x nthres]
#'   - data$nthres: number of thresholds (may vary by observation)
#'   - family$family: family name (cumulative, sratio, cratio, acat)
#'   - family$link: link function name
#' @return Array [ndraws x nobs x ncat] of category probabilities
#' @noRd
posterior_epred_ordinal <- function(prep) {
  checkmate::assert_list(prep)

  # Get density function for specific ordinal family
  dens <- get(paste0("d", prep$family$family), mode = "function")

  # Adjustment for identity link (no threshold transformation)
  adjust <- ifelse(prep$family$link == "identity", 0, 1)

  # Handle varying number of thresholds across observations
  nthres <- prep$data$nthres
  if (is.null(nthres)) {
    # Fixed thresholds: use column count
    nthres <- rep(ncol(prep$dpars$thres), prep$nobs)
  }
  ncat_max <- max(nthres) + adjust
  ncat_min <- min(nthres) + adjust

  # Initialize padding matrix for observations with fewer categories

  init_mat <- matrix(
    ifelse(prep$family$link == "identity", NA, 0),
    nrow = prep$ndraws,
    ncol = ncat_max - ncat_min
  )

  args <- list(link = prep$family$link)
  out <- vector("list", prep$nobs)

  for (i in seq_along(out)) {
    args_i <- args
    args_i$eta <- slice_col(prep$dpars$mu, i)
    args_i$disc <- slice_col(prep$dpars$disc, i)
    args_i$thres <- subset_thres(prep, i)
    ncat_i <- NCOL(args_i$thres) + adjust
    args_i$x <- seq_len(ncat_i)
    out[[i]] <- do.call(dens, args_i)

    # Pad with zeros if fewer categories than maximum
    if (ncat_i < ncat_max) {
      sel <- seq_len(ncat_max - ncat_i)
      out[[i]] <- cbind(out[[i]], init_mat[, sel, drop = FALSE])
    }
  }

  # Combine into 3D array [ndraws x nobs x ncat]
  out <- abind::abind(out, along = 3)
  out <- aperm(out, perm = c(1, 3, 2))
  dimnames(out)[[3]] <- seq_len(ncat_max)
  out
}

#' @noRd
posterior_epred_cumulative <- function(prep) {
  posterior_epred_ordinal(prep)
}

#' @noRd
posterior_epred_sratio <- function(prep) {
  posterior_epred_ordinal(prep)
}

#' @noRd
posterior_epred_cratio <- function(prep) {
  posterior_epred_ordinal(prep)
}

#' @noRd
posterior_epred_acat <- function(prep) {
  posterior_epred_ordinal(prep)
}

# --- Categorical/Compositional posterior_epred Functions ---

#' Posterior Expected Values for Categorical Models
#'
#' Computes category probabilities for categorical response models.
#' Returns a 3D array [ndraws x nobs x ncat] of category probabilities.
#'
#' @param prep Prediction preparation object containing:
#'   - dpars$mu: array [ndraws x nobs x (ncat-1)] of linear predictors
#'   - data$ncat: number of response categories
#'   - refcat: reference category index
#'   - cats: category labels
#' @return Array [ndraws x nobs x ncat] of category probabilities
#' @noRd
posterior_epred_categorical <- function(prep) {
  checkmate::assert_list(prep)

  get_probs <- function(i) {
    eta_i <- insert_refcat(slice_col(eta, i), refcat = prep$refcat)
    dcategorical(cats, eta = eta_i)
  }

  eta <- get_Mu(prep)
  cats <- seq_len(prep$data$ncat)
  out <- abind::abind(lapply(seq_cols(eta), get_probs), along = 3)
  out <- aperm(out, perm = c(1, 3, 2))
  dimnames(out)[[3]] <- prep$cats
  out
}

#' Posterior Expected Values for Multinomial Models
#'
#' Computes expected counts for multinomial response models.
#' Returns a 3D array [ndraws x nobs x ncat] of expected counts.
#'
#' @param prep Prediction preparation object
#' @return Array [ndraws x nobs x ncat] of expected counts
#' @noRd
posterior_epred_multinomial <- function(prep) {
  checkmate::assert_list(prep)

  get_counts <- function(i) {
    eta_i <- insert_refcat(slice_col(eta, i), refcat = prep$refcat)
    dcategorical(cats, eta = eta_i) * trials[i]
  }

  eta <- get_Mu(prep)
  cats <- seq_len(prep$data$ncat)
  trials <- prep$data$trials
  out <- abind::abind(lapply(seq_cols(eta), get_counts), along = 3)
  out <- aperm(out, perm = c(1, 3, 2))
  dimnames(out)[[3]] <- prep$cats
  out
}

#' Posterior Expected Values for Dirichlet-Multinomial Models
#'
#' Mean of dirichlet-multinomial equals multinomial mean.
#' The phi parameter affects variance only.
#'
#' @param prep Prediction preparation object
#' @return Array [ndraws x nobs x ncat] of expected counts
#' @noRd
posterior_epred_dirichlet_multinomial <- function(prep) {
  posterior_epred_multinomial(prep)
}

#' Posterior Expected Values for Dirichlet Models
#'
#' Computes expected proportions for Dirichlet response models.
#' The expected value of a Dirichlet distribution is the normalized
#' concentration parameters, which equals the softmax probabilities.
#' Implementation identical to categorical (see brms source lines 562-606).
#'
#' @param prep Prediction preparation object
#' @return Array [ndraws x nobs x ncat] of expected proportions
#' @noRd
posterior_epred_dirichlet <- function(prep) {
  # Dirichlet expected values use same softmax transformation as categorical
  posterior_epred_categorical(prep)
}

#' Posterior Expected Values for Dirichlet2 Models
#'
#' Dirichlet2 uses direct concentration parameters rather than softmax.
#' E[Y_k] = alpha_k / sum(alpha)
#'
#' @param prep Prediction preparation object
#' @return Array [ndraws x nobs x ncat] of expected proportions
#' @noRd
posterior_epred_dirichlet2 <- function(prep) {
  checkmate::assert_list(prep)

  mu <- get_Mu(prep)
  sums_mu <- apply(mu, 1:2, sum)
  cats <- seq_len(prep$data$ncat)
  for (i in cats) {
    mu[, , i] <- mu[, , i] / sums_mu
  }
  dimnames(mu)[[3]] <- prep$cats
  mu
}

#' Posterior Expected Values for Logistic Normal Models
#'
#' Cannot compute expected values analytically for logistic normal.
#'
#' @param prep Prediction preparation object
#' @noRd
posterior_epred_logistic_normal <- function(prep) {
  checkmate::assert_list(prep)

  stop(insight::format_error(
    "Cannot compute expected values of the posterior predictive ",
    "distribution for family {.val logistic_normal}."
  ))
}
