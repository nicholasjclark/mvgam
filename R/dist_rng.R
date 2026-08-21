# Random-number generators for the 'com_poisson' and 'discrete_weibull'
# observation families, called by posterior_predict() for those families.
# These routines are reimplemented inside mvgam because the equivalent
# generators are not exported by 'brms'. The algorithms follow brms
# (Buerkner, 2017) and the COM-Poisson normalizing constant of Shmueli
# et al. (2005).

# Elementwise log(1 + exp(x)), guarding against overflow for large x.
log1p_exp_vec <- function(x) {
  ifelse(x > 0, x + log1p(exp(-x)), log1p(exp(x)))
}

# Elementwise log(exp(a) + exp(b)), guarding against overflow.
log_sum_exp_vec <- function(a, b) {
  m <- pmax(a, b)
  m + log(exp(a - m) + exp(b - m))
}

# Log normalizing constant of the COM-Poisson distribution, by direct
# series summation on the log scale. Vectorised over recycled `log_mu`
# and `shape`. `M` caps the number of series terms; `thres` sets the
# per-term convergence tolerance.
log_z_com_poisson <- function(log_mu, shape, M = 10000L, thres = 1e-16) {
  n <- max(length(log_mu), length(shape))
  log_mu <- rep_len(log_mu, n)
  shape <- rep_len(shape, n)
  log_thres <- log(thres)
  # Sum of the k = 0 term (shape * (0 - 0) = 0) and the k = 1 term
  # (shape * (log_mu - log(1!)) = shape * log_mu).
  running <- log1p_exp_vec(shape * log_mu)
  lfac <- 0
  k <- 2L
  converged <- FALSE
  while (!converged && k <= M) {
    lfac <- lfac + log(k)
    term <- shape * (k * log_mu - lfac)
    running <- log_sum_exp_vec(running, term)
    converged <- all(term <= log_thres)
    k <- k + 1L
  }
  running
}

# Draw `n` values from the COM-Poisson distribution via inverse-CDF
# sampling. `mu` and `shape` are recycled to length `n`.
#' @importFrom stats runif
rcom_poisson_mvgam <- function(n, mu, shape, M = 10000L) {
  mu <- rep_len(mu, n)
  shape <- rep_len(shape, n)
  log_mu <- log(mu)
  log_z <- log_z_com_poisson(log_mu, shape, M = M)
  u <- runif(n, 0, 1)
  cdf <- exp(-log_z)
  lfac <- 0
  y <- 0
  out <- rep(0, n)
  not_found <- cdf < u
  while (any(not_found) && y <= M) {
    y <- y + 1
    out[not_found] <- y
    lfac <- lfac + log(y)
    cdf <- cdf + exp(shape * (y * log_mu - lfac) - log_z)
    not_found <- cdf < u
  }
  if (any(not_found)) {
    out[not_found] <- NA_real_
  }
  out
}

# Draw `n` values from the discrete Weibull distribution in the
# (mu, shape) parameterisation, where P(Y >= y) = mu ^ (y ^ shape),
# via inverse-CDF sampling.
#' @importFrom stats runif
rdiscrete_weibull_mvgam <- function(n, mu, shape) {
  u <- runif(n, 0, 1)
  ceiling((log(1 - u) / log(mu))^(1 / shape) - 1)
}

# Beta negative binomial variates under mvgam's mean parameterisation.
# The Stan side computes the Beta mixing scale as mu * mtail / shape,
# with the first Beta parameter shifted to 1 + mtail so that the mean is
# exactly mu; drawing the success probability from that Beta and then a
# negative binomial reproduces the same distribution. Vectorised over
# recycled `mu`, `shape` and `mtail`.
rbeta_nb_mvgam <- function(n, mu, shape, mtail) {
  checkmate::assert_int(n, lower = 0L)
  mu <- rep_len(mu, n)
  shape <- rep_len(shape, n)
  mtail <- rep_len(mtail, n)
  prob <- stats::rbeta(n, 1 + mtail, mu * mtail / shape)
  stats::rnbinom(n, size = shape, prob = prob)
}

# Beta negative binomial log-pmf under the same parameterisation, used
# by `log_lik_beta_nb()`. Written with `lbeta()` so the Gamma-function
# ratio stays on the log scale for large counts.
dbeta_nb_mvgam <- function(y, mu, shape, mtail, log = TRUE) {
  alpha <- 1 + mtail
  beta <- mu * mtail / shape
  out <- lgamma(y + shape) - lgamma(y + 1) - lgamma(shape) +
    lbeta(beta + y, alpha + shape) - lbeta(beta, alpha)
  if (log) out else exp(out)
}

# Beta negative binomial distribution function under the same
# parameterisation, mirroring the Stan `beta_nb_lcdf`. Accumulates the
# mass function through its term ratio
# p(k+1)/p(k) = (r+k)(beta+k) / ((k+1)(alpha+beta+r+k)),
# which is exact for counts and avoids re-evaluating log-gamma at every
# step. Returns -Inf below zero, where the distribution has no mass.
pbeta_nb_mvgam <- function(q, mu, shape, mtail, log.p = TRUE) {
  checkmate::assert_number(mu, lower = 0)
  checkmate::assert_number(shape, lower = 0)
  checkmate::assert_number(mtail, lower = 0)
  alpha <- 1 + mtail
  beta <- mu * mtail / shape
  out <- vapply(q, function(y) {
    if (y < 0) return(-Inf)
    lp <- dbeta_nb_mvgam(0, mu, shape, mtail, log = TRUE)
    acc <- lp
    k <- 0
    while (k < y) {
      lp <- lp + log(shape + k) + log(beta + k) -
        log(k + 1) - log(alpha + beta + shape + k)
      m <- max(acc, lp)
      acc <- m + log(exp(acc - m) + exp(lp - m))
      k <- k + 1
    }
    acc
  }, numeric(1))
  if (log.p) out else exp(out)
}
