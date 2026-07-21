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
