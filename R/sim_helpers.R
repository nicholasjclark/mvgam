# Internal helpers for sim_mvgam (Phase D). Each helper builds one
# kind of observation-side structure that the typed catalog
# composes into a full simulation. Kept here (rather than inline in
# sim_mvgam) so each piece can be unit-tested in isolation.
#
# Where the helper exists in mgcv (e.g. mgcv::gamSim's f0..f3 test
# functions), the mvgam version uses the same well-known forms so
# users moving between mgcv and mvgam see familiar generative
# truth.


# Internal: Well-known smooth test functions from mgcv::gamSim()'s
# internals (Gu & Wahba 1991, also used by Wood's mgcv documentation).
# Used by sim_mvgam types 1 / 2 / 3 / 5 where the user benefits from
# a recoverable, recognisable target function. Each takes `x` on
# [0, 1].
#'@noRd
gam_test_f0 <- function(x) 2 * sin(pi * x)
gam_test_f1 <- function(x) exp(2 * x)
gam_test_f2 <- function(x) {
  0.2 * x^11 * (10 * (1 - x))^6 +
    10 * (10 * x)^3 * (1 - x)^10
}
gam_test_f3 <- function(x) 0 * x


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


# Internal: Convert beta-distribution mean / precision (`mu`,
# `phi`) to (`shape1`, `shape2`) for `stats::rbeta`. Matches
# brms's parametrisation.
#'@noRd
beta_shapes <- function(mu, phi) {
  list(shape1 = mu * phi, shape2 = (1 - mu) * phi)
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
#   poisson    (none — log link)
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
  fam_name <- tolower(resolve_family_name(family))
  link <- family$link
  inv_link <- switch(
    link,
    "identity" = identity,
    "log" = exp,
    "logit" = function(x) 1 / (1 + exp(-x)),
    "inverse" = function(x) 1 / x,
    "sqrt" = function(x) x^2,
    stop(insight::format_error(c(
      "Unsupported link in 'sim_family_rng'.",
      x = paste0("Got: '", link, "'.")
    )))
  )
  mu <- inv_link(eta)
  switch(
    fam_name,
    "gaussian" = stats::rnorm(
      length(eta), mean = mu, sd = pars$sigma %||% 1
    ),
    "student" = mu + (pars$sigma %||% 1) *
      stats::rt(length(eta), df = pars$nu %||% 4),
    "poisson" = stats::rpois(length(eta), lambda = mu),
    "negbinomial" = sim_negbinom(mu, pars$size %||% 5),
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
        "binomial, beta, gamma, tweedie, beta_nb, com_binomial."
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
# directly — preferred for clarity if extraDistr is available;
# falls back to base R otherwise.
#'@noRd
sim_negbinom <- function(mu, size) {
  stats::rnbinom(length(mu), size = size, mu = mu)
}


# Internal: Compute the marginal link-scale SD a smooth or
# random-effect contributes, so sim_mvgam can target a chosen
# `prop_trend` variance share when balancing trend vs obs-side
# components.
#'@noRd
component_sd <- function(values) {
  if (length(values) < 2L) return(0)
  stats::sd(as.numeric(values))
}


# Internal: Build a covariate vector of length `n` from a chosen
# distribution. Used by sim_mvgam to populate `x`, `z`, `w`, etc.
#'@noRd
sim_covariate <- function(n, type = c("uniform", "normal", "seq",
                                       "cyclic")) {
  type <- match.arg(type)
  switch(
    type,
    "uniform" = stats::runif(n, -2, 2),
    "normal" = stats::rnorm(n),
    "seq" = seq(-2, 2, length.out = n),
    "cyclic" = rep_len(1:12, n)
  )
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


# Internal: Stationary VAR(p) coefficient matrix via Ansley-Kohn
# (1986). Lets sim_mvgam draw random stable VAR coefficients
# without user input. Returns a `[n_series, n_series, p]` cube.
#'@noRd
stationary_VAR_phi <- function(p = 1L, n_series = 3L, ar_scale = 1) {
  stopifnot(ar_scale > 0)
  Id <- diag(nrow = n_series)
  all_P <- array(dim = c(n_series, n_series, p))
  for (i in seq_len(p)) {
    A <- matrix(
      stats::rnorm(n_series * n_series, sd = ar_scale),
      nrow = n_series
    )
    if (i == 1L) {
      diag(A) <- abs(diag(A))
    }
    B <- t(chol(Id + tcrossprod(A, A)))
    all_P[, , i] <- solve(B, A)
  }

  all_phi <- array(dim = c(n_series, n_series, p, p))
  all_phi_star <- array(dim = c(n_series, n_series, p, p))
  L <- L_star <- Sigma <- Sigma_star <- Gamma <- Id

  for (s in 0:(p - 1L)) {
    all_phi[, , s + 1L, s + 1L] <-
      L %*% all_P[, , s + 1L] %*% solve(L_star)
    all_phi_star[, , s + 1L, s + 1L] <-
      tcrossprod(L_star, all_P[, , s + 1L]) %*% solve(L)
    if (s >= 1L) {
      for (kk in 1:s) {
        all_phi[, , s + 1L, kk] <-
          all_phi[, , s, kk] -
          all_phi[, , s + 1L, s + 1L] %*%
            all_phi_star[, , s, s - kk + 1L]
        all_phi_star[, , s + 1L, kk] <-
          all_phi_star[, , s, kk] -
          all_phi_star[, , s + 1L, s + 1L] %*%
            all_phi[, , s, s - kk + 1L]
      }
    }
    if (s < p - 1L) {
      Sigma_next <- Sigma -
        all_phi[, , s + 1L, s + 1L] %*%
          tcrossprod(Sigma_star, all_phi[, , s + 1L, s + 1L])
      Sigma_star_next <- Sigma_star -
        all_phi_star[, , s + 1L, s + 1L] %*%
          tcrossprod(Sigma, all_phi_star[, , s + 1L, s + 1L])
      L <- t(chol(Sigma_next))
      L_star <- t(chol(Sigma_star_next))
      Sigma <- Sigma_next
      Sigma_star <- Sigma_star_next
    }
  }

  phi_out <- array(dim = c(n_series, n_series, p))
  for (kk in seq_len(p)) {
    phi_out[, , kk] <- all_phi[, , p, kk]
  }
  phi_out
}
