#' Whether a family's draws are whole numbers
#'
#' @param family_name Character; distribution family name
#'
#' @return Logical scalar
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
    "discrete_weibull", "com_poisson", "beta_nb", "com_binomial",
    # A multinomial's counts and a categorical's one-hot row are both
    # whole numbers. Each observed value is an atom of the law. The
    # quantile residual randomises across that atom's interval.
    "multi", "categ"
  )

  family_name %in% integer_families
}


#' Draws restricted to their truncation bounds
#'
#' A family with a quantile function replaces each out-of-bounds draw
#' exactly, by inverting its distribution restricted to the bounds. A
#' family without one, such as a Tweedie or a COM-binomial, redraws
#' from itself up to `ntrys` times. A draw that never lands inside is
#' clamped onto the nearer bound. A warning reports clamping of more
#' than one percent of the draws.
#'
#' @param samples Matrix `\\[ndraws x nobs\\]` or vector of draws
#' @param lb,ub Numeric bounds, scalar or one per observation, or `NULL`
#' @param ntrys Integer; redraw attempts for a family without a quantile
#'   function
#' @param ndraws,nobs Integer dimensions of the draws
#' @param redraw Function returning fresh draws of the same family
#' @param spec Distribution spec from `family_dist_spec()`, or `NULL`
#' @param discrete Logical. `TRUE` for an integer-valued family
#'
#' @return The draws, in the shape `samples` arrived in
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

  is_vector <- !is.matrix(samples)
  if (is_vector) {
    samples <- matrix(samples, nrow = ndraws, ncol = nobs)
  }
  lb <- rep_len(lb %||% -Inf, nobs)
  ub <- rep_len(ub %||% Inf, nobs)
  lb_mat <- data2draws(lb, dim(samples))
  ub_mat <- data2draws(ub, dim(samples))
  outside <- function(x) !is.na(x) & (x < lb_mat | x > ub_mat)
  invalid <- outside(samples)

  if (any(invalid) && !is.null(spec)) {
    exact <- truncated_dist_draws(spec, invalid, lb, ub, discrete)
    if (!is.null(exact)) {
      usable <- invalid & is.finite(exact)
      samples[usable] <- exact[usable]
      invalid <- invalid & !usable
    }
  }
  for (i in seq_len(ntrys)) {
    if (!any(invalid)) break
    fresh <- matrix(redraw(), nrow = ndraws, ncol = nobs)
    usable <- invalid & !is.na(fresh) & !outside(fresh)
    samples[usable] <- fresh[usable]
    invalid <- invalid & !usable
  }

  n_clamped <- sum(invalid)
  if (n_clamped > 0) {
    samples[invalid] <- pmin(pmax(samples[invalid], lb_mat[invalid]),
                             ub_mat[invalid])
  }
  if (n_clamped > 0.01 * ndraws * nobs) {
    warn_once(
      c(
        paste0(round(100 * n_clamped / (ndraws * nobs), 1),
               " percent of predictive draws (", n_clamped, " of ",
               ndraws * nobs, ") were clamped onto a truncation bound."),
        i = "The model places that share of its draws outside the bounds."
      ),
      "mvgam_truncation_clamp"
    )
  }
  if (is_vector) as.vector(samples) else samples
}


#' Draw beta-binomial variates in the mean-precision parameterisation
#'
#' `extraDistr::rbbinom()` takes the two Beta shapes, while brms and
#' mvgam both parameterise the family by a per-trial probability and a
#' precision. Converting in one place keeps the two samplers that need
#' it agreeing with `dbeta_binomial()` and with the Stan likelihood.
#'
#' @param n Number of variates
#' @param size Trial counts, one per variate or a single count
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


#' Predictive draws of one family at its parameters
#'
#' @param family_name Character name of the distribution (e.g., "poisson")
#' @param ndraws Integer; number of draws per observation
#' @param epred Matrix `\\[ndraws x nobs\\]` of the inverse link of the
#'   predictor, the base distribution's `mu`. A lognormal's `mu` is its
#'   `meanlog`. A hurdle or zero-inflated `mu` excludes the mass at zero.
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
#'   zero_inflated_beta_binomial, com_binomial
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
#'
#' @return Vector of `length(epred)` draws, in the column order of
#'   `epred`
#'
#' @details
#' An ordinal family draws from its category probabilities through
#' `ordinal_draws()`.
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
                               mphi = NULL, mtheta = NULL,
                               mtail = NULL,
                               lb = NULL, ub = NULL, ntrys = 5) {
  checkmate::assert_string(family_name)
  checkmate::assert_int(ndraws, lower = 1)
  checkmate::assert_matrix(epred)
  checkmate::assert_numeric(lb, null.ok = TRUE)
  checkmate::assert_numeric(ub, null.ok = TRUE)
  checkmate::assert_int(ntrys, lower = 1)

  has_truncation <- !is.null(lb) || !is.null(ub)
  nobs <- ncol(epred)
  # `rbinom()` recycles `size` along `epred` flattened by column, one
  # observation's draws after another. A vector of one trial count per
  # observation would pair those draws with other observations' totals.
  # `data2draws()` gives every draw its own observation's count.
  trial_draws <- if (is.null(trials)) NULL else data2draws(trials, dim(epred))

  # A count law restricted to positive values. Inverting its survival
  # function on `(0, S(0))` is exact for any count law. The survival
  # scale keeps a small mean, whose `S(0)` is near zero, accurate.
  positive_draw <- function(pfun, qfun, ...) {
    v <- stats::runif(length(epred), 0, pfun(0, ..., lower.tail = FALSE))
    qfun(v, ..., lower.tail = FALSE)
  }

  # Truncation redraws through the same function.
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
      stats::rexp(length(epred), rate = 1 / epred)
    },

    "gamma" = {
      checkmate::assert_matrix(shape, nrows = ndraws, ncols = ncol(epred))
      scale <- epred / shape
      stats::rgamma(length(epred), shape = shape, scale = scale)
    },

    "weibull" = {
      checkmate::assert_matrix(shape, nrows = ndraws, ncols = ncol(epred))
      scale <- epred / gamma(1 + 1 / shape)
      stats::rweibull(length(epred), shape = shape, scale = scale)
    },

    "frechet" = {
      checkmate::assert_matrix(shape, nrows = ndraws, ncols = ncol(epred))
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
      checkmate::assert_matrix(mphi, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(mtheta, nrows = ndraws, ncols = ncol(epred))
      mgcv::rTweedie(
        mu = as.numeric(epred),
        p = as.numeric(mtheta),
        phi = as.numeric(mphi)
      )
    },

    "beta_nb" = {
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
      # `posterior_predict_com_binomial()` holds the CMB sampler.
      checkmate::assert_matrix(nu, nrows = ndraws,
                                ncols = ncol(epred))
      checkmate::assert_numeric(trials, lower = 0L,
                                 len = ncol(epred))
      # The kernel takes the link-scale predictor.
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
      stats::rlnorm(length(epred), meanlog = epred, sdlog = sigma)
    },

    "shifted_lognormal" = {
      checkmate::assert_matrix(sigma, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(ndt, nrows = ndraws, ncols = ncol(epred))
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
      # `sigma` is `1 / sqrt(shape)`
      stats::rnbinom(length(epred), mu = epred, size = 1 / sigma^2)
    },

    "geometric" = {
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
      stats::rbinom(length(epred), size = trial_draws, prob = epred)
    },

    "beta_binomial" = {
      checkmate::assert_numeric(trials, min.len = 1)
      checkmate::assert_matrix(phi, nrows = ndraws, ncols = ncol(epred))
      rbeta_binomial_draws(length(epred), size = trial_draws, mu = epred,
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
             stats::rbinom(length(epred), size = trial_draws, prob = epred))
    },

    "zero_inflated_beta_binomial" = {
      checkmate::assert_numeric(trials, min.len = 1)
      checkmate::assert_matrix(zi, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(phi, nrows = ndraws, ncols = ncol(epred))
      draws <- rbeta_binomial_draws(length(epred), size = trial_draws,
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
      # `epred` is the drift rate
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
      ifelse(stats::runif(length(epred)) < hu, 0L,
             positive_draw(stats::ppois, stats::qpois, lambda = epred))
    },

    "hurdle_negbinomial" = {
      checkmate::assert_matrix(hu, nrows = ndraws, ncols = ncol(epred))
      checkmate::assert_matrix(shape, nrows = ndraws, ncols = ncol(epred))
      ifelse(stats::runif(length(epred)) < hu, 0L,
             positive_draw(stats::pnbinom, stats::qnbinom, mu = epred,
                           size = shape))
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

    refuse_missing_family_dispatch(family_name, "predictive draws")
  )

  samples <- draw_once()

  if (has_truncation) {
    # `epred` is `mu` on its own scale. The spec takes it under the
    # identity link.
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
             mphi = mphi, mtheta = mtheta, mtail = mtail),
        trials
      ),
      discrete = family_uses_integers(family_name)
    )
  }

  samples
}


#' The distributional parameters a family declares other than `mu`
#'
#' `MVGAM_FAMILY_DPARS` lists them for every brms and mvgam family. A
#' trial count is data and stays out of the list.
#'
#' @param family_name Character name of the family (e.g., "gaussian")
#'
#' @return Character vector of parameter names, empty for a family the
#'   list lacks
#'
#' @noRd
get_family_dpars <- function(family_name) {
  checkmate::assert_string(family_name)

  MVGAM_FAMILY_DPARS[[family_name]] %||% character(0)
}


#' Resolve every distributional parameter a fitted family declares
#'
#' A parameter with a formula of its own is a linear predictor,
#' evaluated per observation. Any other parameter was sampled as a
#' scalar, or one scalar per series. Its values come from the draws. Every
#' prediction path calls this function for both kinds.
#'
#' Names are the bare parameter names throughout. A model with several
#' responses stores each response's parameters under its key.
#' `response_suffix()` adds the key on the way in. The result is keyed
#' by the bare name.
#'
#' @param object An `mvgam` model object
#' @param dpar_names Bare names of the parameters the family declares
#' @param ndraws Number of draws the prediction covers
#' @param nobs Number of rows the prediction covers
#' @param draw_ids Draw indices to keep, or `NULL` for all
#' @param newdata Data the prediction covers; `object$data` if `NULL`
#' @param resp The response's key, or `NULL`
#' @return Named list of `[ndraws x nobs]` matrices, keyed by bare
#'   name. A parameter the posterior does not carry is left out.
#'
#' @noRd
resolve_family_pars <- function(object, dpar_names, ndraws, nobs,
                                draw_ids = NULL, newdata = NULL,
                                resp = NULL,
                                re_formula = NULL,
                                allow_new_levels = FALSE,
                                sample_new_levels = "uncertainty") {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_character(dpar_names, min.len = 0)
  if (length(dpar_names) == 0) {
    return(list())
  }

  # The two branches below call separate extractors. A bare count would
  # let each choose its own draws. Resolving it again is idempotent.
  draw_ids <- resolve_draw_ids(object, ndraws, draw_ids)

  predicted <- predicted_dpar_names(object, dpar_names, resp = resp)
  sampled <- setdiff(dpar_names, predicted)

  out <- list()
  for (dpar in predicted) {
    out[[dpar]] <- predicted_dpar_draws(
      object, dpar, nobs = nobs, ndraws = ndraws, draw_ids = draw_ids,
      newdata = newdata, resp = resp,
      re_formula = re_formula, allow_new_levels = allow_new_levels,
      sample_new_levels = sample_new_levels
    )
  }

  if (length(sampled) > 0) {
    suffixed <- paste0(sampled, response_suffix(object, resp))
    found <- extract_dpars_from_stanfit(
      stanfit = object$fit,
      dpar_names = suffixed,
      ndraws = ndraws,
      nobs = nobs,
      draw_ids = draw_ids
    )
    # `extract_dpars_from_stanfit()` leaves out a parameter it cannot
    # find. Each name it returns is matched back to its bare one.
    names(found) <- sampled[match(names(found), suffixed)]
    out[names(found)] <- found
  }

  out[intersect(dpar_names, names(out))]
}


#' Names of the distributional parameters carrying their own formula
#'
#' A distributional submodel such as `sigma ~ x` or `nu ~ z` is
#' recorded in `pforms` on the model formula. Such a parameter is a
#' linear predictor evaluated per observation. `pforms` also holds
#' non-linear parameters. The function returns the names the family
#' declares.
#'
#' @param object An `mvgam` model object
#' @param dpar_names Distributional parameters the family declares
#' @param resp The response's key, or `NULL` for every response
#' @return Character vector, possibly empty
#'
#' @noRd
predicted_dpar_names <- function(object, dpar_names, resp = NULL) {
  forms <- response_formulas(object)
  if (!is.null(resp)) {
    forms <- forms[resp]
  }
  named <- as.character(unlist(lapply(forms, function(f) names(f$pforms))))
  intersect(dpar_names, named)
}


#' Draws of a distributional parameter with a formula of its own
#'
#' `extract_component_linpred()` composes the parameter's predictor
#' from its parametric, smooth, group-level and Gaussian process terms.
#' This function applies the parameter's inverse link. The result is on
#' the scale a scalar draw of the parameter takes.
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
                                 newdata = NULL, resp = NULL,
                                 re_formula = NULL,
                                 allow_new_levels = FALSE,
                                 sample_new_levels = "uncertainty") {
  linpred <- extract_component_linpred(
    mvgam_fit = object,
    newdata = prediction_frame(object, newdata),
    component = dpar,
    draw_ids = draw_ids,
    resp = resp,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels
  )
  if (is_per_response(linpred)) {
    stop(insight::format_error(c(
      paste0(
        "Distributional parameter '", dpar,
        "' resolved to one predictor per response."
      ),
      i = "Scope the call to a single response with 'resp'."
    )), call. = FALSE)
  }
  # This response's family gives the parameter its link.
  out <- as.matrix(inv_link(linpred,
                            dpar_link(model_families(object, resp), dpar)))
  if (!is.null(ndraws) && nrow(out) != ndraws) {
    stop_mvgam_fault(
      paste0("Distributional parameter '", dpar, "' needs one row per ",
             "draw of the linear predictor."),
      paste0("Got ", nrow(out), " rows, expected ", ndraws, ".")
    )
  }
  if (!is.null(nobs) && ncol(out) != nobs) {
    stop(insight::format_error(c(
      paste0(
        "Distributional parameter '", dpar, "' was predicted for a ",
        "different number of rows than the linear predictor covers."
      ),
      x = paste0("Got ", ncol(out), " columns, expected ", nobs, "."),
      i = paste0("Check 'newdata' for every covariate of the '", dpar,
                 "' formula.")
    )), call. = FALSE)
  }
  out
}


#' Link of one distributional parameter
#'
#' A family object records the mean's link in `family$link` and each
#' other parameter's in `family$link_<dpar>`. A family built by
#' `stats::gaussian()` or `stats::Gamma()` records no per-parameter
#' links. `brms::brmsfamily()` then supplies the default link. An
#' assumed identity link would leave a log-linked parameter on the
#' link scale. Every value derived from it would be wrong by an
#' exponential. Custom families declare every link when built.
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
  name <- resolve_family_name(family)
  link <- brms::brmsfamily(name)[[paste0("link_", dpar)]]
  if (is.null(link)) {
    stop_mvgam_fault(
      paste0("Family '", name, "' lacks a link for parameter '", dpar,
             "'."),
      paste0("The family object and brms::brmsfamily('", name,
             "') both lack 'link_", dpar, "'.")
    )
  }
  link
}


#' Inverse of a brms link
#'
#' Covers every link brms offers. A distributional parameter's link is
#' recorded in `link_<dpar>` as a name alone, with no `linkinv`
#' function.
#'
#' @param x Numeric vector, matrix or array on the link scale
#' @param link Name of the link
#' @return `x` on the parameter's own scale, with its dimensions
#' @noRd
inv_link <- function(x, link) {
  checkmate::assert_numeric(x)
  checkmate::assert_string(link)
  switch(
    link,
    identity = x,
    log = exp(x),
    logm1 = exp(x) + 1,
    log1p = expm1(x),
    inverse = 1 / x,
    sqrt = x^2,
    `1/mu^2` = 1 / sqrt(x),
    tan_half = 2 * atan(x),
    logit = stats::plogis(x),
    probit = stats::pnorm(x),
    probit_approx = stats::pnorm(x),
    cauchit = stats::pcauchy(x),
    cloglog = 1 - exp(-exp(x)),
    softplus = log1p_exp(x),
    squareplus = (x + sqrt(x^2 + 4)) / 2,
    softit = {
      y <- log1p_exp(x)
      y / (1 + y)
    },
    stop(insight::format_error(paste0(
      "Link '", link, "' is not supported."
    )), call. = FALSE)
  )
}


#' Draws of sampled distributional parameters, one column per row
#'
#' A scalar parameter such as `sigma`, or one written with a single
#' index, fills every column. An indexed parameter with `nobs` columns
#' gives one column per row. Any other width belongs to another axis
#' and is refused.
#'
#' @param stanfit A stanfit or draws object
#' @param dpar_names Character vector of parameter names, each carrying
#'   any response suffix
#' @param ndraws Integer; number of draws when `draw_ids` is `NULL`
#' @param nobs Integer; number of prediction rows
#' @param draw_ids Optional integer vector of draw indices
#'
#' @return Named list of `\\[ndraws x nobs\\]` matrices. A parameter the
#'   posterior lacks is left out.
#'
#' @noRd
extract_dpars_from_stanfit <- function(stanfit,
                                       dpar_names,
                                       ndraws,
                                       nobs,
                                       draw_ids = NULL) {
  checkmate::assert_multi_class(
    stanfit,
    c("stanfit", "CmdStanMCMC", "draws", "draws_matrix", "draws_array")
  )
  checkmate::assert_character(dpar_names, min.len = 0)
  checkmate::assert_int(ndraws, lower = 1)
  checkmate::assert_int(nobs, lower = 1)
  checkmate::assert_integerish(draw_ids, lower = 1, null.ok = TRUE)
  if (length(dpar_names) == 0) {
    return(list())
  }

  draws_mat <- subset_draws_rows(fit_draws(stanfit, dpar_names),
                                 ndraws = ndraws, draw_ids = draw_ids)
  all_cols <- colnames(draws_mat)
  out <- list()
  for (dpar in dpar_names) {
    cols <- if (dpar %in% all_cols) {
      dpar
    } else {
      grep(paste0("^", dpar, "\\["), all_cols, value = TRUE)
    }
    if (length(cols) == 0L) next
    if (length(cols) > 1L) {
      one_index <- "^[^[]*\\[(\\d+)\\]$"
      if (!all(grepl(one_index, cols))) {
        stop_mvgam_fault(
          "A per-row parameter must carry one index per column.",
          paste0("Got ", paste(cols[!grepl(one_index, cols)],
                               collapse = ", "), ".")
        )
      }
      cols <- cols[order(as.integer(sub(one_index, "\\1", cols)))]
    }
    values <- as.matrix(draws_mat[, cols, drop = FALSE])
    if (ncol(values) == 1L) {
      out[[dpar]] <- matrix(values[, 1L], nrow = nrow(values), ncol = nobs)
    } else if (ncol(values) == nobs) {
      out[[dpar]] <- unname(values)
    } else {
      stop_mvgam_fault(
        paste0("Parameter '", dpar, "' must have one column per ",
               "prediction row."),
        paste0("Found ", ncol(values), " columns for ", nobs, " rows.")
      )
    }
  }
  out
}


#' Posterior predictive draws of the response
#'
#' @description
#' Draws from the posterior predictive distribution: the family's
#' observation noise around the parameters of each posterior draw.
#' They predict new observations, for the training data or new data.
#' [pp_check.mvgam()] draws them under the fitted state to check a fit
#' against its data.
#'
#' @param object A fitted mvgam object from [mvgam()].
#' @inheritParams posterior_epred.mvgam
#' @param ... Unused. Anything passed here is refused.
#'
#' @return An `[ndraws, nobs]` matrix on the response scale, one row per
#'   draw and one column per row of `newdata`. A multivariate model
#'   with `resp = NULL` gives a named list, one element per response.
#'
#' @details
#' Each family draws from its own distribution at the parameters
#' [posterior_linpred.mvgam()] and [posterior_epred.mvgam()] use. A
#' `trunc()` term restricts the draws to its bounds.
#'
#' [hindcast.mvgam()] draws under the fitted latent states at the
#' training times. [forecast.mvgam()] propagates them to future times.
#'
#' @template prediction-trend-state
#'
#' @seealso [pp_check.mvgam()] compares draws taken under the fitted
#'   state with the data.
#'   [forecast.mvgam()] predicts future times from the fitted state.
#'
#' @examples
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                     n_timepoints = 120L, trend_model = AR())
#' mod <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
#'              data = simdat$data_train, family = poisson(),
#'              chains = 2, silent = 2)
#'
#' # Simulated counts, one row per draw and one column per observation
#' pp <- posterior_predict(mod)
#' dim(pp)
#'
#' # The share of observed counts inside their 90 percent predictive
#' # intervals
#' bounds <- apply(pp, 2, quantile, probs = c(0.05, 0.95))
#' y <- simdat$data_train$y
#' mean(y >= bounds[1, ] & y <= bounds[2, ])
#'
#' # Next: pp_check() plots the draws against the observed counts
#' }
#'
#' @importFrom brms posterior_predict
#' @family predictions
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
  checkmate::assert_class(object, "mvgam")
  require_fitted_model(object, "posterior_predict")
  checkmate::assert_data_frame(newdata, null.ok = TRUE)
  checkmate::assert_flag(process_error)
  checkmate::assert_flag(incl_autocor)
  validate_draw_selectors(ndraws, draw_ids)
  validate_group_level_args(re_formula, allow_new_levels, sample_new_levels)
  checkmate::assert_string(resp, null.ok = TRUE)
  rlang::check_dots_empty()

  newdata <- prediction_frame(object, newdata)
  # The predictor and the distributional parameters come from separate
  # extractions. Both take these indices.
  draw_ids <- resolve_draw_ids(object, ndraws, draw_ids)

  # Sampling starts from the predictor. A hurdle or zero-inflated
  # family draws its mixture from `mu`, which its mean excludes. Under
  # `process_error = TRUE` the predictor already carries the trend's
  # innovations.
  linpred <- posterior_linpred(
    object,
    newdata = newdata,
    process_error = process_error,
    incl_autocor = incl_autocor,
    draw_ids = draw_ids,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels,
    resp = resp
  )

  # A model with several responses returns each response's draws when
  # `resp` is `NULL`, every response at the same iterations.
  if (is_per_response(linpred)) {
    if (is.null(resp)) {
      return(lapply(stats::setNames(nm = names(linpred)), function(r) {
        draw_observations(object, linpred[[r]], newdata, draw_ids,
                          resp = r, re_formula = re_formula,
                          allow_new_levels = allow_new_levels,
                          sample_new_levels = sample_new_levels)
      }))
    }
    linpred <- linpred[[resp]]
  }
  draw_observations(object, linpred, newdata, draw_ids, resp = resp,
                    re_formula = re_formula,
                    allow_new_levels = allow_new_levels,
                    sample_new_levels = sample_new_levels)
}


#' Draw one response's observations at its predictor
#'
#' `posterior_predict()`, `hindcast()` and `forecast()` share this
#' sampler. A closure-unit family draws each unit's latent state and
#' its visits together. Its kernel takes the caller's predictor, which
#' keeps the latent state a forecast carries past the grid. An ordinal
#' family draws a level from `ordinal_category_probs()`. Every other
#' family draws from its own distribution at the inverse link of the
#' predictor. Its distributional parameters, trials and truncation
#' bounds come from the same draws and rows.
#'
#' @param object A fitted `mvgam` object
#' @param linpred `[ndraws x nobs]` link-scale predictor, its rows the
#'   draws `draw_ids` names
#' @param newdata Data the predictor was built for
#' @param draw_ids Draws the predictor was taken at, or `NULL` for every
#'   draw
#' @param resp The response's key, or `NULL`
#' @return `[ndraws x nobs]` matrix of draws
#' @noRd
draw_observations <- function(object, linpred, newdata, draw_ids,
                              resp = NULL,
                              re_formula = NULL,
                              allow_new_levels = FALSE,
                              sample_new_levels = "uncertainty") {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_matrix(linpred)
  checkmate::assert_data_frame(newdata)
  checkmate::assert_integerish(draw_ids, lower = 1, any.missing = FALSE,
                               len = nrow(linpred), null.ok = TRUE)
  family <- model_families(object, resp)
  if (uses_closure_unit_layout(family)) {
    predict_fn <- dispatch_closure_unit_method(family, "predict")
    return(predict_fn(
      object, newdata = newdata, draw_ids = draw_ids, linpred = linpred
    ))
  }
  if (is_ordinal_family(family)) {
    return(ordinal_draws(ordinal_category_probs(
      object, linpred, family, draw_ids = draw_ids, newdata = newdata,
      resp = resp
    )))
  }

  family_name <- resolve_family_name(family)
  ndraws <- nrow(linpred)
  nobs <- ncol(linpred)
  dpars <- resolve_family_pars(
    object,
    dpar_names = get_family_dpars(family_name),
    ndraws = ndraws,
    nobs = nobs,
    draw_ids = draw_ids,
    newdata = newdata,
    resp = resp,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels
  )
  bounds <- response_addition_terms(object, newdata, resp)

  # Every resolved parameter with a matching sampler argument is
  # forwarded. A family added later then keeps all of its parameters.
  dpar_args <- dpars[intersect(names(dpars),
                               names(formals(sample_from_family)))]
  samples <- do.call(sample_from_family, c(
    list(
      family_name = family_name,
      ndraws = ndraws,
      epred = family$linkinv(linpred),
      trials = extract_trials_for_family(object, family, newdata, resp),
      lb = bounds$lb,
      ub = bounds$ub
    ),
    dpar_args
  ))
  matrix(samples, nrow = ndraws, ncol = nobs)
}
