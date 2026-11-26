# The Conway-Maxwell-Poisson distribution in the mode parameterisation
# brms fits: P(Y = k) = w_k / Z with log w_k = shape * (k log(mu) -
# log(k!)). The density, distribution function, mean and sampler share
# one normalising constant `Z`. `com_poisson_normaliser()` takes it from
# an asymptotic expansion where the distribution is broad and from a
# bounded direct sum elsewhere.


#' The default tolerance and work limit of a COM-Poisson sum
#'
#' A tail is truncated once a bound on its omitted mass falls below
#' `tol` times the mass summed so far. `max_terms` caps the terms summed
#' in one direction from a starting count.
#'
#' @return List of `tol` and `max_terms`
#' @noRd
com_poisson_controls <- function() {
  list(tol = 1e-12, max_terms = 100000L)
}


#' Check the COM-Poisson parameters
#'
#' @param mu Numeric modal location
#' @param shape Numeric dispersion
#' @return `NULL`, invisibly
#' @noRd
assert_com_poisson_pars <- function(mu, shape) {
  bad <- c(!is.finite(mu) | mu <= 0, !is.finite(shape) | shape <= 0)
  if (any(bad, na.rm = TRUE)) {
    stop(insight::format_error(c(
      "COM-Poisson needs a finite positive 'mu' and 'shape'.",
      x = paste0("Got 'mu' in [", format(min(mu, na.rm = TRUE)), ", ",
                 format(max(mu, na.rm = TRUE)), "] and 'shape' in [",
                 format(min(shape, na.rm = TRUE)), ", ",
                 format(max(shape, na.rm = TRUE)), "].")
    )), call. = FALSE)
  }
  invisible(NULL)
}


#' The log of the unnormalised COM-Poisson weight `w_k`
#'
#' @param k Counts
#' @param log_mu,shape Parameters, recycled with `k`
#' @return Numeric vector
#' @noRd
com_poisson_log_weight <- function(k, log_mu, shape) {
  shape * (k * log_mu - lgamma(k + 1))
}


#' The log ratio of a COM-Poisson weight to its neighbour
#'
#' `log(w_{k+1} / w_k)` for `direction = 1` and `log(w_{k-1} / w_k)`
#' for `direction = -1`. Either is negative on the outer side of the
#' mode and shrinks with every further step.
#'
#' @param k Counts
#' @param log_mu,shape Parameters, recycled with `k`
#' @param direction `1` or `-1`
#' @return Numeric vector
#' @noRd
com_poisson_log_ratio <- function(k, log_mu, shape, direction) {
  shape * if (direction > 0) log_mu - log(k + 1) else log(k) - log_mu
}


#' A geometric bound on the log mass beyond a term
#'
#' Terms after one of weight `w` whose successive ratios never exceed
#' a ratio below one, `r`, sum to at most `w r / (1 - r)`. A ratio of
#' one or more bounds nothing.
#'
#' @param log_w Log weight of the last term summed
#' @param log_r Log of the ratio to the next term
#' @return Numeric vector, `Inf` where `log_r >= 0`
#' @noRd
geometric_log_tail <- function(log_w, log_r) {
  out <- rep(Inf, length(log_r))
  ok <- log_r < 0
  out[ok] <- log_w[ok] + log_r[ok] - log(-expm1(log_r[ok]))
  out
}


#' The log mass and first moment of one tail of a COM-Poisson sum
#'
#' Sums `w_k` and `k w_k` from `start` outward, upward for
#' `direction = 1` and downward to zero for `direction = -1`. Each
#' `start` lies on the outer side of the mode. The omitted mass then
#' has the geometric bound of `geometric_log_tail()`. Going up, the
#' omitted moment is at most `w_k (k r / (1 - r) + r / (1 - r)^2)`.
#' Going down it is at most `k - 1` times the omitted mass (Shmueli et
#' al. 2005, Appendix B.1). An element stops once both bounds fall
#' below `tol` times what it has summed. A downward tail from a
#' negative count is empty, with log mass and moment `-Inf`.
#'
#' @param start Integer-valued starting counts
#' @param log_mu,shape Parameters, one per element of `start`
#' @param direction `1` or `-1`
#' @param tol,max_terms As `com_poisson_controls()` gives them
#' @return List of `log_mass` and `log_moment`, one per element
#' @noRd
com_poisson_log_tail <- function(start, log_mu, shape, direction,
                                 tol = com_poisson_controls()$tol,
                                 max_terms =
                                   com_poisson_controls()$max_terms) {
  checkmate::assert_choice(direction, c(-1, 1))
  k <- start
  log_w <- com_poisson_log_weight(pmax(k, 0), log_mu, shape)
  log_mass <- log_moment <- rep(-Inf, length(k))
  active <- which(k >= 0)
  log_mass[active] <- log_w[active]
  log_moment[active] <- log(k[active]) + log_w[active]
  log_tol <- log(tol)
  for (step in seq_len(max_terms + 1L)) {
    # A downward tail that has reached zero holds every term
    if (direction < 0) active <- active[k[active] > 0]
    if (!length(active)) break
    j <- active
    log_r <- com_poisson_log_ratio(k[j], log_mu[j], shape[j], direction)
    mass_bound <- geometric_log_tail(log_w[j], log_r)
    moment_bound <- if (direction > 0) {
      r <- exp(log_r)
      log_w[j] + log(k[j] * r / (1 - r) + r / (1 - r)^2)
    } else {
      mass_bound + log(pmax(k[j] - 1, 0))
    }
    moment_bound[log_r >= 0] <- Inf
    open <- mass_bound > log_tol + log_mass[j] |
      moment_bound > log_tol + log_moment[j]
    active <- j[open]
    if (!length(active)) break
    if (step > max_terms) {
      stop(insight::format_error(c(
        "A COM-Poisson sum exceeded its work limit.",
        x = paste0("The sum needed over ", max_terms, " terms at 'mu' = ",
                   format(exp(log_mu[active[1L]])), " and 'shape' = ",
                   format(shape[active[1L]]), "."),
        i = "Such a broad distribution is outside what the sum supports."
      )), call. = FALSE)
    }
    k[active] <- k[active] + direction
    log_w[active] <- log_w[active] + log_r[open]
    log_mass[active] <- log_add_exp(log_mass[active], log_w[active])
    log_moment[active] <- log_add_exp(log_moment[active],
                                      log(k[active]) + log_w[active])
  }
  list(log_mass = log_mass, log_moment = log_moment)
}


#' Whether the asymptotic normaliser serves a COM-Poisson distribution
#'
#' The region where the Gaunt et al. (2019) expansion is accurate to
#' about `1e-8` in log probability: `mu * shape >= 200` with
#' `mu / shape >= 50`. brms 2.23.2 adopts the same region. Its
#' corrections fall in powers of `1 / (mu * shape)` and `shape / mu`.
#'
#' @param log_mu,shape Parameters
#' @return Logical vector
#' @noRd
com_poisson_use_gaunt <- function(log_mu, shape) {
  log_shape <- log(shape)
  log_mu + log_shape >= log(200) & log_mu - log_shape >= log(50)
}


#' The asymptotic COM-Poisson normaliser and mean
#'
#' With `lambda = mu^shape`, Gaunt et al. (2019) expand
#'
#' `log Z = shape mu - (shape - 1) / 2 log(2 pi mu) - log(shape) / 2 +
#'   log(1 + C)`, `C = sum_m c_m (shape mu)^(-m)`,
#'
#' with `c_1 = (s^2 - 1) / 24`, `c_2 = (s^2 - 1)(s^2 + 23) / 1152` and
#' `c_3 = (s^2 - 1)(5 s^4 - 298 s^2 + 11237) / 414720` for
#' `s = shape`. The mean `lambda d log(Z) / d lambda` is
#' `(mu / shape) d log(Z) / d mu`:
#'
#' `mu - (shape - 1) / (2 shape) - sum_m m c_m (shape mu)^(-m) /
#'   (shape (1 + C))`.
#'
#' @param log_mu,shape Parameters
#' @return List of `log_z` and `mean`
#' @noRd
com_poisson_gaunt <- function(log_mu, shape) {
  mu <- exp(log_mu)
  s2 <- shape^2
  x <- 1 / (shape * mu)
  terms <- cbind(
    (s2 - 1) / 24 * x,
    (s2 - 1) * (s2 + 23) / 1152 * x^2,
    (s2 - 1) * (5 * s2^2 - 298 * s2 + 11237) / 414720 * x^3
  )
  C <- rowSums(terms)
  list(
    log_z = shape * mu - (shape - 1) / 2 * (log(2 * pi) + log_mu) -
      log(shape) / 2 + log1p(C),
    mean = mu - (shape - 1) / (2 * shape) -
      as.vector(terms %*% (1:3)) / (shape * (1 + C))
  )
}


#' The COM-Poisson normalising constant and mean
#'
#' `shape = 1` is the Poisson, with `log Z = mu` and mean `mu`. The
#' broad region takes `com_poisson_gaunt()`. Everywhere else the sum
#' runs outward from the mode `floor(mu)`, the largest weight, in both
#' directions.
#'
#' @param mu,shape Parameters, recycled together
#' @param approx Whether the broad region may take the expansion.
#'   `FALSE` sums directly everywhere.
#' @return List of `log_z` and `mean`, each with the dimensions of the
#'   longer argument
#' @noRd
com_poisson_normaliser <- function(mu, shape, approx = TRUE) {
  checkmate::assert_flag(approx)
  assert_com_poisson_pars(mu, shape)
  out_dim <- if (length(shape) > length(mu)) dim(shape) else dim(mu)
  n <- max(length(mu), length(shape))
  log_mu <- log(rep_len(as.vector(mu), n))
  shape <- rep_len(as.vector(shape), n)
  log_z <- mean <- rep(NA_real_, n)

  poisson <- which(shape == 1)
  log_z[poisson] <- mean[poisson] <- exp(log_mu[poisson])

  gaunt <- which(approx & shape != 1 & com_poisson_use_gaunt(log_mu, shape))
  if (length(gaunt)) {
    g <- com_poisson_gaunt(log_mu[gaunt], shape[gaunt])
    log_z[gaunt] <- g$log_z
    mean[gaunt] <- g$mean
  }

  direct <- which(is.na(log_z) & !is.na(log_mu) & !is.na(shape))
  if (length(direct)) {
    lm <- log_mu[direct]
    sh <- shape[direct]
    mode <- floor(exp(lm))
    log_w_mode <- com_poisson_log_weight(mode, lm, sh)
    down <- com_poisson_log_tail(mode - 1, lm, sh, -1)
    up <- com_poisson_log_tail(mode + 1, lm, sh, 1)
    log_z[direct] <- log_add_exp(log_w_mode,
                                 log_add_exp(down$log_mass, up$log_mass))
    log_moment <- log_add_exp(log(mode) + log_w_mode,
                              log_add_exp(down$log_moment, up$log_moment))
    mean[direct] <- exp(log_moment - log_z[direct])
  }
  dim(log_z) <- dim(mean) <- out_dim
  list(log_z = log_z, mean = mean)
}


#' Mean of the COM-Poisson distribution
#'
#' @param mu,shape Parameters, recycled together
#' @param approx Passed to `com_poisson_normaliser()`
#' @return Numeric, with the dimensions of the longer argument
#' @noRd
mean_com_poisson <- function(mu, shape, approx = TRUE) {
  com_poisson_normaliser(mu, shape, approx = approx)$mean
}


#' COM-Poisson probability mass function
#'
#' A value that is negative or not whole has no mass.
#'
#' @param x Numeric values
#' @param mu,shape Parameters, recycled with `x`
#' @param log Whether to return log probabilities
#' @return Numeric vector along the recycled arguments
#' @noRd
dcom_poisson_mvgam <- function(x, mu, shape, log = FALSE) {
  n <- max(length(x), length(mu), length(shape))
  x <- rep_len(x, n)
  mu <- rep_len(mu, n)
  shape <- rep_len(shape, n)
  out <- com_poisson_log_weight(pmax(x, 0), log(mu), shape) -
    com_poisson_normaliser(mu, shape)$log_z
  out[!is.na(x) & (x < 0 | x != floor(x))] <- -Inf
  if (log) out else exp(out)
}


#' COM-Poisson distribution function
#'
#' Sums the tail on the side of `q` away from the mode, which keeps
#' far-tail probabilities accurate. The other tail is its complement.
#' A negative `q` has probability zero below it. An infinite `q` has
#' probability one.
#'
#' @param q Numeric quantiles, floored to counts
#' @param mu,shape Parameters, recycled with `q`
#' @param lower.tail Whether to return `P(Y <= q)`
#' @param log.p Whether to return log probabilities
#' @return Numeric vector along the recycled arguments
#' @noRd
pcom_poisson_mvgam <- function(q, mu, shape, lower.tail = TRUE,
                               log.p = FALSE) {
  checkmate::assert_flag(lower.tail)
  checkmate::assert_flag(log.p)
  n <- max(length(q), length(mu), length(shape))
  q <- floor(rep_len(q, n))
  mu <- rep_len(mu, n)
  shape <- rep_len(shape, n)
  log_lower <- rep(NA_real_, n)
  log_lower[which(q < 0)] <- -Inf
  log_lower[which(q == Inf)] <- 0
  inside <- which(is.finite(q) & q >= 0)
  if (length(inside)) {
    lm <- log(mu[inside])
    sh <- shape[inside]
    qi <- q[inside]
    below <- qi < floor(mu[inside])
    side <- rep(NA_real_, length(inside))
    b <- which(below)
    a <- which(!below)
    side[b] <- com_poisson_log_tail(qi[b], lm[b], sh[b], -1)$log_mass
    side[a] <- com_poisson_log_tail(qi[a] + 1, lm[a], sh[a], 1)$log_mass
    side <- pmin(side - com_poisson_normaliser(mu[inside], sh)$log_z, 0)
    log_lower[inside] <- ifelse(below, side, log1mexp(-side))
  }
  out <- if (lower.tail) log_lower else log1mexp(-log_lower)
  if (log.p) out else exp(out)
}


#' COM-Poisson random variates
#'
#' Inverts the distribution function from the mode outward, which takes
#' about one standard deviation of steps per draw. The mass below the
#' mode comes from `com_poisson_log_tail()`. A uniform above it steps
#' up from the mode until the cumulative probability reaches it. One
#' below it steps down. An upward search also stops once the mass left
#' above the count falls below the summation tolerance. A uniform within
#' that tolerance of one then takes the count reached.
#'
#' @param n Number of draws
#' @param mu,shape Parameters, recycled to `n`
#' @return Numeric vector of `n` counts
#' @noRd
rcom_poisson_mvgam <- function(n, mu, shape) {
  checkmate::assert_count(n)
  mu <- rep_len(mu, n)
  shape <- rep_len(shape, n)
  log_mu <- log(mu)
  log_z <- com_poisson_normaliser(mu, shape)$log_z
  mode <- floor(mu)
  below <- exp(com_poisson_log_tail(mode - 1, log_mu, shape, -1)$log_mass -
                 log_z)
  u <- stats::runif(n)
  out <- mode
  log_p <- com_poisson_log_weight(mode, log_mu, shape) - log_z
  log_tol <- log(com_poisson_controls()$tol)

  # Upward: F(k) grows from F(mode) until it reaches u
  cdf <- below + exp(log_p)
  active <- which(u > below & cdf < u)
  while (length(active)) {
    j <- active
    log_r <- com_poisson_log_ratio(out[j], log_mu[j], shape[j], 1)
    out[j] <- out[j] + 1
    log_p[j] <- log_p[j] + log_r
    cdf[j] <- cdf[j] + exp(log_p[j])
    spent <- geometric_log_tail(log_p[j], com_poisson_log_ratio(
      out[j], log_mu[j], shape[j], 1
    )) <= log_tol
    active <- j[cdf[j] < u[j] & !spent]
  }

  # Downward: F(k - 1) = F(k) - p_k shrinks from F(mode - 1) while it
  # still reaches u
  down <- which(u <= below)
  cdf[down] <- below[down]
  out[down] <- mode[down] - 1
  log_p[down] <- com_poisson_log_weight(out[down], log_mu[down],
                                        shape[down]) - log_z[down]
  active <- down[out[down] > 0 & cdf[down] - exp(log_p[down]) >= u[down]]
  while (length(active)) {
    j <- active
    cdf[j] <- cdf[j] - exp(log_p[j])
    log_r <- com_poisson_log_ratio(out[j], log_mu[j], shape[j], -1)
    out[j] <- out[j] - 1
    log_p[j] <- log_p[j] + log_r
    active <- j[out[j] > 0 & cdf[j] - exp(log_p[j]) >= u[j]]
  }
  out
}
