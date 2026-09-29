# Internal helpers for sim_mvgam. Each helper builds one
# kind of observation-side structure that the typed catalog
# composes into a full simulation. Each lives outside sim_mvgam
# to be unit-tested in isolation.


# Internal: Sample a single smooth function on a fine grid by
# fitting a thin-plate spline basis (or user-chosen `bs`) to
# uniformly-spaced training points, drawing random spline
# coefficients, and evaluating the resulting smooth.
#
# Inputs:
#   x       numeric vector of covariate values where the smooth
#           should be evaluated.
#   k       basis dimension (default 8).
#   bs      smooth basis type (default "tp"; "cc" for cyclic).
#   scale   marginal SD of the random spline coefficients.
#
# Returns a list with `f` (the smooth at each `x`) + `basis` (the
# mgcv smooth object, for downstream reconstruction).
#'@noRd
sim_smooth <- function(x, k = 8L, bs = "tp", scale = 1) {
  checkmate::assert_numeric(x, finite = TRUE, min.len = 2L)
  checkmate::assert_int(k, lower = 3L)
  checkmate::assert_string(bs)
  checkmate::assert_number(scale, lower = 0)
  sm_spec <- mgcv::s(x, k = k, bs = bs)
  basis_data <- data.frame(x = x)
  sm <- mgcv::smoothCon(
    sm_spec, data = basis_data,
    knots = NULL, absorb.cons = TRUE
  )[[1L]]
  beta <- stats::rnorm(ncol(sm$X), sd = scale)
  list(f = as.numeric(sm$X %*% beta), basis = sm, coefs = beta)
}


# Internal: Sample a Gaussian process realisation on the supplied
# covariate values via a squared-exponential kernel.
#
# Inputs:
#   x       numeric vector of covariate values.
#   alpha   marginal SD (kernel amplitude).
#   rho     length-scale.
#
# Returns a numeric vector of length `length(x)`.
#'@noRd
sim_gp_cov <- function(x, alpha = 1, rho = NULL) {
  checkmate::assert_numeric(x, finite = TRUE, min.len = 2L)
  checkmate::assert_number(alpha, lower = 0)
  if (is.null(rho)) {
    rho <- diff(range(x)) / 4
  }
  checkmate::assert_number(rho, lower = 0)
  d <- as.matrix(stats::dist(x))
  K <- alpha^2 * exp(-0.5 * (d / rho)^2)
  # Tiny jitter for numerical PSD.
  K <- K + diag(1e-6, nrow = nrow(K))
  as.numeric(rmvn(1L, mu = rep(0, length(x)), Sigma = K))
}


# Internal: Sample random-effect coefficients for a grouping
# factor.
#
# Inputs:
#   grp     factor (or coercible to one) of group memberships.
#   sigma   marginal SD of the random effect.
#
# Returns a list with `levels`, `coefs` (named numeric, length
# = nlevels(grp)), and `values` (the random-effect contribution
# aligned with `grp`, length = length(grp)).
#'@noRd
sim_re <- function(grp, sigma = 1) {
  grp <- as.factor(grp)
  checkmate::assert_number(sigma, lower = 0)
  levs <- levels(grp)
  coefs <- stats::rnorm(length(levs), sd = sigma)
  names(coefs) <- levs
  list(levels = levs, coefs = coefs, values = coefs[as.integer(grp)])
}


# Internal: Family-specific RNG dispatcher. Given an `eta` linear
# predictor (link scale) and any auxiliary parameters in `pars`,
# return a vector of length(eta) of simulated response values
# under the chosen family. Mirrors what brms / mvgam fit on the
# inverse side.
#
# Supported families (v1):
#   gaussian   pars$sigma
#   student    pars$nu, pars$sigma
#   poisson    (none, log link)
#   negbinomial pars$size (brms NB2 parametrisation;
#                          uses extraDistr::rnbinom)
#   binomial   pars$trials (length(eta) or scalar)
#   beta       pars$phi (precision)
#   Gamma      pars$shape
#   tweedie    pars$phi, pars$power
#   com_binomial pars$trials, pars$nu (Conway-Maxwell-Binomial;
#                logit link only; mu = plogis(eta) is the
#                location, nu the dispersion: nu = 1 binomial,
#                nu > 1 under-dispersed, nu < 1 over-dispersed,
#                nu < 0 super-dispersed / bimodal)
#
# `family` is a brms- / stats-style `family` object; the function
# reads `family$family` and `family$link` to decide.
#'@noRd
sim_family_rng <- function(eta, family, pars = list()) {
  checkmate::assert_numeric(eta, finite = TRUE)
  # `resolve_family_name()` returns the user-visible name for both
  # base R / brms families and customfamily objects (where
  # `family$family` is the literal string "custom"). Routing
  # through it lets sim_family_rng dispatch on `tweedie` as well
  # as the brms-native families with one code path.
  fam_name <- resolve_family_name(family)
  link <- family$link
  # `inv_link()` holds every link the prediction surface applies. A
  # five-entry switch here refused probit, cloglog and cauchit at
  # simulation time while the same model predicted through them.
  mu <- inv_link(eta, link)
  # A family on the positive line needs a positive mean. Under the
  # inverse link a negative `eta` gives a negative `mu`, and
  # `rgamma()` and `rpois()` return NaN with a warning a caller can
  # miss. The refusal names the pairing the caller chose.
  positive_mean <- c("poisson", "negbinomial", "gamma", "tweedie",
                     "beta_nb")
  if (fam_name %in% positive_mean && any(mu <= 0)) {
    stop(insight::format_error(c(
      paste0("A '", fam_name, "' mean has to be positive."),
      x = paste0(
        sum(mu <= 0), " of ", length(mu),
        " values are zero or below under the '", link, "' link."
      ),
      i = "Use a link that keeps the mean positive, such as 'log'."
    )), call. = FALSE)
  }
  switch(
    fam_name,
    "gaussian" = stats::rnorm(
      length(eta), mean = mu, sd = pars$sigma %||% 1
    ),
    "student" = mu + (pars$sigma %||% 1) *
      stats::rt(length(eta), df = pars$nu %||% 4),
    "poisson" = stats::rpois(length(eta), lambda = mu),
    "negbinomial" = sim_negbinom(mu, pars$size %||% 5),
    # A Bernoulli is the binomial at one trial, and takes no trial
    # count of its own, so the size is fixed here rather than read
    # from `pars`.
    "bernoulli" = stats::rbinom(length(eta), size = 1L, prob = mu),
    "binomial" = stats::rbinom(
      length(eta), size = pars$trials %||% 1L, prob = mu
    ),
    "beta" = {
      shp <- beta_shapes(mu, pars$phi %||% 5)
      stats::rbeta(length(eta), shp$shape1, shp$shape2)
    },
    "gamma" = stats::rgamma(
      length(eta), shape = pars$shape %||% 2,
      rate = (pars$shape %||% 2) / mu
    ),
    "tweedie" = sim_tweedie(mu, pars$phi %||% 1, pars$power %||% 1.5),
    "beta_nb" = rbeta_nb_mvgam(
      n = length(eta), mu = mu,
      shape = pars$shape %||% 2, mtail = pars$mtail %||% 2
    ),
    "com_binomial" = {
      trials_vec <- if (length(pars$trials %||% 10L) == 1L) {
        rep(as.integer(pars$trials %||% 10L), length(eta))
      } else {
        as.integer(pars$trials)
      }
      nu_vec <- if (length(pars$nu %||% 1) == 1L) {
        rep(pars$nu %||% 1, length(eta))
      } else {
        pars$nu
      }
      rcmb_vec(mu = mu, nu = nu_vec, T = trials_vec)
    },
    stop(insight::format_error(c(
      "Unsupported family in 'sim_family_rng'.",
      x = paste0("Got: '", fam_name, "'."),
      i = paste0(
        "Supported: gaussian, student, poisson, negbinomial, ",
        "bernoulli, binomial, beta, gamma, tweedie, beta_nb, ",
        "com_binomial."
      )
    )))
  )
}

# Internal: Tweedie (compound Poisson-Gamma) sampler for
# 1 < power < 2. Generates N ~ Poisson(lambda) jumps and sums
# Gamma(shape, rate) summands. Avoids the optional `tweedie`
# package dependency; matches Jorgensen's CP parameterisation
# used by `brms::brmsfamily('tweedie')` and mvgam::tweedie().
#'@noRd
sim_tweedie <- function(mu, phi, power) {
  checkmate::assert_numeric(mu, lower = .Machine$double.eps,
                              finite = TRUE)
  checkmate::assert_number(phi, lower = .Machine$double.eps)
  checkmate::assert_number(power, lower = 1 + 1e-6,
                            upper = 2 - 1e-6)
  lambda <- mu^(2 - power) / (phi * (2 - power))
  shape  <- (2 - power) / (power - 1)
  rate   <- mu^(1 - power) / (phi * (power - 1))
  n      <- length(mu)
  N      <- stats::rpois(n, lambda)
  out    <- numeric(n)
  pos    <- N > 0L
  if (any(pos)) {
    out[pos] <- vapply(
      which(pos),
      function(i) sum(stats::rgamma(N[i], shape = shape, rate = rate[i])),
      numeric(1)
    )
  }
  out
}


# Internal: Negbinomial NB2 sampler. brms uses
# `negative_binomial_2(mu, size)`; the equivalent in base R is
# `stats::rnbinom(n, size, mu)` (note: positional arg order is
# size first). `extraDistr::rnbinom` accepts named `mu` + `size`
# directly, which is clearer when extraDistr is available;
# falls back to base R otherwise.
#'@noRd
sim_negbinom <- function(mu, size) {
  stats::rnbinom(length(mu), size = size, mu = mu)
}


# Internal: Build a balanced group factor with `n_levels` groups
# and `n` total observations.
#'@noRd
sim_grp <- function(n, n_levels = 5L,
                     labels = NULL) {
  if (is.null(labels)) {
    labels <- if (n_levels <= 26L) {
      letters[seq_len(n_levels)]
    } else {
      paste0("g", seq_len(n_levels))
    }
  }
  factor(rep_len(labels, n), levels = labels)
}
