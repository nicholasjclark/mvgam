# Random-number generators for the 'com_poisson' and 'discrete_weibull'
# observation families, called by posterior_predict() for those families.
# These routines are reimplemented inside mvgam because the equivalent
# generators are not exported by 'brms'. The algorithms follow brms
# (Buerkner, 2017) and the COM-Poisson normalizing constant of Shmueli
# et al. (2005).

# The two series of the COM-Poisson distribution, summed on the log
# scale: the normalising constant `Z = sum_k w_k` and the first moment
# `sum_k k w_k`, where `log w_k = shape * (k * log_mu - log(k!))`.
# The sampler needs the first and the mean needs both. Vectorised over
# recycled `log_mu` and `shape`. `M` caps the number of series terms.
# `thres` sets the per-term convergence tolerance, and `converged`
# reports whether every element reached it.
com_poisson_log_series <- function(log_mu, shape, M = 10000L,
                                   thres = 1e-16) {
  n <- max(length(log_mu), length(shape))
  log_mu <- rep_len(log_mu, n)
  shape <- rep_len(shape, n)
  log_thres <- log(thres)
  # The k = 0 term is exp(0) = 1 and the k = 1 term is exp(shape *
  # log_mu). Only the k = 1 term enters the first moment.
  log_z <- log1p_exp(shape * log_mu)
  log_moment <- shape * log_mu
  lfac <- 0
  k <- 2L
  converged <- FALSE
  while (!converged && k <= M) {
    lfac <- lfac + log(k)
    term <- shape * (k * log_mu - lfac)
    log_z <- log_add_exp(log_z, term)
    log_moment <- log_add_exp(log_moment, log(k) + term)
    converged <- all(term <= log_thres)
    k <- k + 1L
  }
  list(log_z = log_z, log_moment = log_moment, converged = converged)
}

# Draw `n` values from the COM-Poisson distribution via inverse-CDF
# sampling. `mu` and `shape` are recycled to length `n`.
#' @importFrom stats runif
rcom_poisson_mvgam <- function(n, mu, shape, M = 10000L) {
  mu <- rep_len(mu, n)
  shape <- rep_len(shape, n)
  log_mu <- log(mu)
  log_z <- com_poisson_log_series(log_mu, shape, M = M)$log_z
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
# step. All arguments recycle, and every element steps together up to
# the largest quantile. A quantile below zero has no mass.
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
