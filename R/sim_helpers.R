# Helpers for sim_mvgam. Each one builds one kind of observation-side
# structure for the typed catalog to compose into a simulation. Each is
# a separate function for unit testing.


#' A random smooth function evaluated at covariate values
#'
#' Builds an mgcv basis on `x`, draws random spline coefficients and
#' evaluates the smooth they define.
#'
#' @param x Numeric vector of covariate values to evaluate the smooth at
#' @param k Integer basis dimension
#' @param bs Character smooth basis type, such as `"tp"` or `"cc"` for a
#'   cyclic smooth
#' @param scale Non-negative marginal SD of the spline coefficients
#' @return List holding the smooth at each `x` in `f`, the mgcv smooth
#'   object in `basis` and the drawn coefficients in `coefs`
#' @noRd
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


#' A Gaussian process draw at covariate values
#'
#' Uses a squared-exponential kernel with a small diagonal jitter to keep
#' the covariance positive definite.
#'
#' @param x Numeric vector of covariate values
#' @param alpha Non-negative marginal SD of the kernel
#' @param rho Positive length-scale, or `NULL` for a quarter of the range
#'   of `x`
#' @return Numeric vector with one value per element of `x`
#' @noRd
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


#' Random-effect coefficients drawn for a grouping factor
#'
#' @param grp Factor of group memberships, or a vector coercible to one
#' @param sigma Non-negative marginal SD of the random effect
#' @return List holding the group `levels`, the named `coefs` with one
#'   value per level and the `values` matched to each element of `grp`
#' @noRd
sim_re <- function(grp, sigma = 1) {
  checkmate::assert_atomic_vector(grp, min.len = 1L)
  grp <- as.factor(grp)
  checkmate::assert_number(sigma, lower = 0)
  levs <- levels(grp)
  coefs <- stats::rnorm(length(levs), sd = sigma)
  names(coefs) <- levs
  list(levels = levs, coefs = coefs, values = coefs[as.integer(grp)])
}


#' Simulated responses from a family given a linear predictor
#'
#' Applies the inverse link of `family` to `eta` and draws one response
#' per element from the parametrisation brms and mvgam fit. The
#' auxiliary parameters each family takes from `pars` are listed here.
#' * gaussian: `sigma`
#' * student: `nu`, `sigma`
#' * negbinomial: `size`, in the NB2 parametrisation
#' * binomial: `trials`, a scalar or one per element of `eta`
#' * beta: `phi`, the precision
#' * Gamma: `shape`
#' * tweedie: `phi`, `power`
#' * com_binomial: `trials`, `nu`, with the logit link only. `nu = 1`
#'   gives the binomial, `nu > 1` under-dispersion, `nu < 1`
#'   over-dispersion and `nu < 0` a bimodal response.
#'
#' @param eta Numeric vector of finite link-scale linear predictors
#' @param family Family object holding `family` and `link`
#' @param pars Named list of auxiliary family parameters
#' @return Numeric vector of simulated responses, one per element of
#'   `eta`
#' @noRd
sim_family_rng <- function(eta, family, pars = list()) {
  checkmate::assert_numeric(eta, finite = TRUE)
  checkmate::assert_list(family)
  checkmate::assert_list(pars)
  # The name of a custom family such as `tweedie()` as well as of a
  # brms family
  fam_name <- resolve_family_name(family)
  link <- family$link
  mu <- inv_link(eta, link)
  # An identity or inverse link can give a family on the positive line
  # a mean at or below zero. `rgamma()` and `rpois()` would return NaN.
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
    "negbinomial" = stats::rnbinom(length(eta), size = pars$size %||% 5,
                                   mu = mu),
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
    "tweedie" = mgcv::rTweedie(mu, p = pars$power %||% 1.5,
                               phi = pars$phi %||% 1),
    "beta_nb" = rbeta_nb_mvgam(
      n = length(eta), mu = mu,
      shape = pars$shape %||% 2, mtail = pars$mtail %||% 2
    ),
    "com_binomial" = rcmb_vec(
      mu = mu,
      nu = rep_len(pars$nu %||% 1, length(eta)),
      T = rep_len(as.integer(pars$trials %||% 10L), length(eta))
    ),
    stop(insight::format_error(c(
      paste0("Simulating the '", fam_name, "' family is not supported."),
      i = paste0(
        "Supported: gaussian, student, poisson, negbinomial, ",
        "bernoulli, binomial, beta, gamma, tweedie, beta_nb, ",
        "com_binomial."
      )
    )), call. = FALSE)
  )
}

#' A balanced grouping factor
#'
#' @param n Integer number of observations
#' @param n_levels Integer number of groups
#' @param labels Character vector of group labels, or `NULL` for letters
#'   when there are 26 groups or fewer and `g1`, `g2` and so on otherwise
#' @return Factor of length `n` cycling through the group labels
#' @noRd
sim_grp <- function(n, n_levels = 5L,
                     labels = NULL) {
  checkmate::assert_int(n, lower = 1L)
  checkmate::assert_int(n_levels, lower = 1L)
  checkmate::assert_character(labels, min.len = 1L, null.ok = TRUE)
  if (is.null(labels)) {
    labels <- if (n_levels <= 26L) {
      letters[seq_len(n_levels)]
    } else {
      paste0("g", seq_len(n_levels))
    }
  }
  factor(rep_len(labels, n), levels = labels)
}
