# Random-number generators for the 'discrete_weibull' and 'beta_nb'
# observation families, called by posterior_predict() for those
# families. brms does not export the equivalent generators. The
# COM-Poisson kernels are in R/dist_com_poisson.R.

# Draw `n` values from the discrete Weibull distribution in the
# (mu, shape) parameterisation, where P(Y >= y) = mu ^ (y ^ shape),
# via inverse-CDF sampling.
#' @importFrom stats runif
rdiscrete_weibull_mvgam <- function(n, mu, shape) {
  u <- runif(n, 0, 1)
  ceiling((log(1 - u) / log(mu))^(1 / shape) - 1)
}

# Beta negative binomial variates under mvgam's mean parameterisation.
# The Stan side computes the Beta mixing scale as mu * mtail / shape and
# shifts the first Beta parameter to 1 + mtail. With that shift the mean
# is exactly mu. Drawing the success probability from that Beta and then
# a negative binomial reproduces the same distribution. Vectorised over
# recycled `mu`, `shape` and `mtail`.
rbeta_nb_mvgam <- function(n, mu, shape, mtail) {
  checkmate::assert_int(n, lower = 0L)
  mu <- rep_len(mu, n)
  shape <- rep_len(shape, n)
  mtail <- rep_len(mtail, n)
  prob <- stats::rbeta(n, 1 + mtail, mu * mtail / shape)
  stats::rnbinom(n, size = shape, prob = prob)
}

# Beta negative binomial mass function under the same
# parameterisation, the density `family_dist_spec()` names for
# `beta_nb()`. Written with `lbeta()` so the Gamma-function ratio stays
# on the log scale for large counts. A count below zero has no mass.
dbeta_nb_mvgam <- function(x, mu, shape, mtail, log = FALSE) {
  alpha <- 1 + mtail
  beta <- mu * mtail / shape
  out <- lgamma(x + shape) - lgamma(x + 1) - lgamma(shape) +
    lbeta(beta + x, alpha + shape) - lbeta(beta, alpha)
  out[!is.na(x) & x < 0] <- -Inf
  if (log) out else exp(out)
}

# Beta negative binomial distribution function under the same
# parameterisation, mirroring the Stan `beta_nb_lcdf`. Accumulates the
# mass function through its term ratio
# p(k+1)/p(k) = (r+k)(beta+k) / ((k+1)(alpha+beta+r+k)),
# which is exact for counts and avoids re-evaluating log-gamma at every
# step. All arguments recycle. Every element steps together up to the
# largest quantile. A quantile below zero has no mass.
pbeta_nb_mvgam <- function(q, mu, shape, mtail, lower.tail = TRUE,
                           log.p = FALSE) {
  n <- max(length(q), length(mu), length(shape), length(mtail))
  q <- floor(rep_len(q, n) + 1e-9)
  mu <- rep_len(mu, n)
  shape <- rep_len(shape, n)
  mtail <- rep_len(mtail, n)
  alpha <- 1 + mtail
  beta <- mu * mtail / shape
  lp <- dbeta_nb_mvgam(0, mu, shape, mtail, log = TRUE)
  acc <- lp
  k_max <- if (all(is.na(q))) 0 else max(q, na.rm = TRUE)
  k <- 0
  while (k < k_max) {
    step <- !is.na(q) & k < q
    lp[step] <- lp[step] + log(shape[step] + k) + log(beta[step] + k) -
      log(k + 1) - log(alpha[step] + beta[step] + shape[step] + k)
    acc[step] <- log_add_exp(acc[step], lp[step])
    k <- k + 1
  }
  acc[!is.na(q) & q < 0] <- -Inf
  acc[is.na(q)] <- NA_real_
  if (!lower.tail) {
    acc <- log1mexp(-acc)
  }
  if (log.p) acc else exp(acc)
}
