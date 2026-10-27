# The per-family kernels, the dispatcher and the addition terms,
# checked against closed-form densities. The brms concordance check
# runs on a fitted model in tests/local/test-log-lik-brms-concordance.R.

test_that("the dispatcher returns each family's stats density", {
  set.seed(123)
  linpred <- matrix(rnorm(12, 0.5, 0.2), 3L, 4L)
  mu <- exp(linpred)
  sigma <- matrix(0.5, 3L, 4L)
  shape <- matrix(2, 3L, 4L)
  phi <- matrix(5, 3L, 4L)
  cols <- function(f) vapply(seq_len(4L), f, numeric(3L))
  y_count <- c(0, 2, 5, 1)
  y_unit <- c(0.2, 0.5, 0.7, 0.9)

  expect_equal(
    dispatch_log_lik("gaussian", "identity", linpred, y_unit,
                     list(sigma = sigma), NULL),
    cols(function(j) stats::dnorm(y_unit[j], linpred[, j], 0.5, log = TRUE))
  )
  expect_equal(
    dispatch_log_lik("poisson", "log", linpred, y_count, list(), NULL),
    cols(function(j) stats::dpois(y_count[j], mu[, j], log = TRUE))
  )
  expect_equal(
    dispatch_log_lik("negbinomial", "log", linpred, y_count,
                     list(shape = shape), NULL),
    cols(function(j) {
      stats::dnbinom(y_count[j], mu = mu[, j], size = 2, log = TRUE)
    })
  )
  p <- stats::plogis(linpred)
  expect_equal(
    dispatch_log_lik("beta", "logit", linpred, y_unit, list(phi = phi),
                     NULL),
    cols(function(j) {
      stats::dbeta(y_unit[j], p[, j] * 5, (1 - p[, j]) * 5, log = TRUE)
    })
  )
  # Each row has its own number of trials
  trials <- c(6, 7, 8, 9)
  expect_equal(
    dispatch_log_lik("binomial", "logit", linpred, y_count, list(), trials),
    cols(function(j) {
      stats::dbinom(y_count[j], trials[j], p[, j], log = TRUE)
    })
  )
  # The asymmetric Laplace density at quantile q, with the check
  # function rho_q(u) = u (q - 1[u < 0])
  q <- matrix(0.25, 3L, 4L)
  expect_equal(
    dispatch_log_lik("asym_laplace", "identity", linpred, y_unit,
                     list(sigma = sigma, quantile = q), NULL),
    cols(function(j) {
      u <- (y_unit[j] - linpred[, j]) / 0.5
      log(0.25 * 0.75 / 0.5) - u * (0.25 - (u < 0))
    })
  )
})

test_that("hurdle and zero-inflated kernels match their closed forms", {
  linpred <- log(matrix(c(1.5, 2.5), 2L, 2L))
  mu <- exp(linpred[, 1L])
  mix <- matrix(0.3, 2L, 2L)
  shape <- matrix(2, 2L, 2L)
  y <- c(0, 3)
  kernel <- function(fn, pars, link = "log", yy = y, trials = NULL) {
    fn(linpred = linpred, link = link, y = yy, family_pars = pars,
       trials = trials)
  }

  hp <- kernel(log_lik_hurdle_poisson, list(hu = mix))
  expect_equal(hp[, 1L], log(c(0.3, 0.3)))
  expect_equal(hp[, 2L], log(0.7) + stats::dpois(3, mu, log = TRUE) -
                 log(1 - exp(-mu)))
  hnb <- kernel(log_lik_hurdle_negbinomial, list(hu = mix, shape = shape))
  expect_equal(hnb[, 2L], log(0.7) +
                 stats::dnbinom(3, mu = mu, size = 2, log = TRUE) -
                 log(1 - stats::dnbinom(0, mu = mu, size = 2)))
  hg <- kernel(log_lik_hurdle_gamma, list(hu = mix, shape = shape))
  expect_equal(hg[, 2L], log(0.7) + stats::dgamma(3, 2, 2 / mu, log = TRUE))

  zp <- kernel(log_lik_zero_inflated_poisson, list(zi = mix))
  expect_equal(zp[, 1L], log(0.3 + 0.7 * exp(-mu)))
  expect_equal(zp[, 2L], log(0.7) + stats::dpois(3, mu, log = TRUE))
  znb <- kernel(log_lik_zero_inflated_negbinomial,
                list(zi = mix, shape = shape))
  expect_equal(znb[, 1L],
               log(0.3 + 0.7 * stats::dnbinom(0, mu = mu, size = 2)))
  zb <- kernel(log_lik_zero_inflated_binomial, list(zi = mix),
               link = "logit", trials = c(5, 5))
  prob <- stats::plogis(linpred[, 1L])
  expect_equal(zb[, 1L], log(0.3 + 0.7 * (1 - prob)^5))
  # The beta gives zero no mass, leaving it to the inflation
  zbeta <- kernel(log_lik_zero_inflated_beta,
                  list(zi = mix, phi = matrix(5, 2L, 2L)),
                  link = "logit", yy = c(0, 0.4))
  expect_equal(zbeta[, 1L], log(c(0.3, 0.3)))
  expect_equal(zbeta[, 2L], log(0.7) +
                 stats::dbeta(0.4, prob * 5, (1 - prob) * 5, log = TRUE))
})

test_that("dispatch_log_lik errors clearly for unknown family", {
  refuse <- function() {
    dispatch_log_lik(
      family_name = "definitely_not_a_family",
      link = "identity",
      linpred = matrix(0, 2L, 2L),
      y = c(0, 1),
      family_pars = list(),
      trials = NULL
    )
  }
  # The message names the family and the issue tracker. It reports the
  # fault as mvgam's.
  err <- expect_error(refuse(), regexp = "definitely_not_a_family")
  msg <- conditionMessage(err)
  expect_match(msg, "Please report this mvgam fault")
  expect_match(msg, "mvgam/issues")
  # The reader runs the package. The message gives them no source edit
  expect_false(grepl("Add a ", msg, fixed = TRUE))
  expect_false(grepl("branch", msg, fixed = TRUE))
  expect_false(grepl("R/", msg, fixed = TRUE))
})


# logLik.mvgam, which AIC() and BIC() call
test_that("logLik.mvgam(pointwise = FALSE) returns scalar with df + nobs", {
  ll_mat <- matrix(c(-1, -2, -3, -4, -5, -6), nrow = 2, byrow = TRUE)
  testthat::local_mocked_bindings(
    log_lik.mvgam = function(object, ...) ll_mat,
    variables.mvgam = function(x, ...) c("b_Intercept", "sigma"),
    nobs.mvgam = function(object, ...) 3L,
    .package = "mvgam"
  )
  obj <- structure(list(), class = "mvgam")
  out <- logLik.mvgam(obj)
  expect_s3_class(out, "logLik")
  expect_equal(length(out), 1L)
  # row sums: -6, -15; mean = -10.5
  expect_equal(as.numeric(out), -10.5)
  # df counts the parameters `variables()` lists
  expect_equal(attr(out, "df"), 2L)
  expect_equal(attr(out, "nobs"), 3L)
})


test_that("logLik.mvgam(pointwise = TRUE) returns the [ndraws x nobs] matrix", {
  ll_mat <- matrix(c(-1, -2, -3, -4, -5, -6), nrow = 2, byrow = TRUE)
  testthat::local_mocked_bindings(
    log_lik.mvgam = function(object, ...) ll_mat,
    .package = "mvgam"
  )
  obj <- structure(list(), class = "mvgam")
  out <- logLik.mvgam(obj, pointwise = TRUE)
  expect_identical(out, ll_mat)
})


test_that("logLik.mvgam validates pointwise as a flag", {
  obj <- structure(list(), class = "mvgam")
  expect_error(logLik.mvgam(obj, pointwise = "yes"), "pointwise")
  expect_error(logLik.mvgam(obj, pointwise = NA), "pointwise")
})


# ------------------------------------------------------------------
# Addition terms: weights(), cens() and trunc()
#
# Each changes one row's contribution to the likelihood. The results
# are checked against the reference distribution functions.
# ------------------------------------------------------------------

addition_fixture <- function(S = 6L, N = 5L) {
  set.seed(42)
  linpred <- matrix(rnorm(S * N, 1, 0.3), S, N)
  pars <- list(sigma = matrix(runif(S * N, 0.5, 1.2), S, N))
  spec <- mvgam:::family_dist_spec("gaussian", "identity", linpred,
                                   pars, NULL)
  y <- c(0.4, 1.1, 1.9, 0.2, 1.5)
  list(
    linpred = linpred, pars = pars, spec = spec, y = y,
    ll = mvgam:::dispatch_log_lik("gaussian", "identity", linpred, y,
                                pars, NULL)
  )
}


test_that("case weights scale each column of the log-likelihood", {
  f <- addition_fixture()
  w <- c(1, 2, 0.5, 3, 1)
  out <- mvgam:::apply_case_weights(f$ll, w)
  expect_equal(out, sweep(f$ll, 2L, w, `*`))
  # A weight of one everywhere leaves the matrix untouched.
  expect_identical(mvgam:::apply_case_weights(f$ll, rep(1, 5L)), f$ll)
  expect_identical(mvgam:::apply_case_weights(f$ll, NULL), f$ll)
})


test_that("right censoring contributes the survival probability", {
  f <- addition_fixture()
  cens <- c(0, 1, 0, 0, 0)
  out <- mvgam:::apply_censoring(f$ll, cens, NULL, f$spec, f$linpred, f$y)
  expected <- stats::pnorm(
    f$y[2L], mean = f$linpred[, 2L], sd = f$pars$sigma[, 2L],
    lower.tail = FALSE, log.p = TRUE
  )
  expect_equal(out[, 2L], expected)
  # Uncensored columns are left as the density.
  expect_equal(out[, -2L], f$ll[, -2L])
})


test_that("left censoring contributes the cumulative probability", {
  f <- addition_fixture()
  cens <- c(-1, 0, 0, 0, 0)
  out <- mvgam:::apply_censoring(f$ll, cens, NULL, f$spec, f$linpred, f$y)
  expected <- stats::pnorm(
    f$y[1L], mean = f$linpred[, 1L], sd = f$pars$sigma[, 1L],
    log.p = TRUE
  )
  expect_equal(out[, 1L], expected)
})


test_that("interval censoring contributes the mass between bounds", {
  f <- addition_fixture()
  cens <- c(0, 0, 2, 0, 0)
  rcens <- c(0, 0, 3.5, 0, 0)
  out <- mvgam:::apply_censoring(f$ll, cens, rcens, f$spec, f$linpred, f$y)
  expected <- log(
    stats::pnorm(rcens[3L], f$linpred[, 3L], f$pars$sigma[, 3L]) -
      stats::pnorm(f$y[3L], f$linpred[, 3L], f$pars$sigma[, 3L])
  )
  expect_equal(out[, 3L], expected)
})


test_that("interval censoring without an upper bound errors clearly", {
  f <- addition_fixture()
  expect_error(
    mvgam:::apply_censoring(f$ll, c(0, 0, 2, 0, 0), NULL, f$spec,
                            f$linpred, f$y),
    "need an upper bound"
  )
})


test_that("truncation renormalises over the retained region", {
  f <- addition_fixture()
  lb <- rep(-1, 5L)
  ub <- rep(3, 5L)
  out <- mvgam:::apply_truncation_to_loglik(
    f$ll, lb, ub, f$spec, f$linpred, discrete = FALSE
  )
  denom <- log(
    stats::pnorm(3, f$linpred, f$pars$sigma) -
      stats::pnorm(-1, f$linpred, f$pars$sigma)
  )
  expect_equal(out, f$ll - denom)
})


test_that("an absent bound leaves that side unnormalised", {
  f <- addition_fixture()
  both <- mvgam:::apply_truncation_to_loglik(
    f$ll, NULL, NULL, f$spec, f$linpred, discrete = FALSE
  )
  expect_identical(both, f$ll)
  upper_only <- mvgam:::apply_truncation_to_loglik(
    f$ll, NULL, rep(3, 5L), f$spec, f$linpred, discrete = FALSE
  )
  expect_equal(
    upper_only[, 1L],
    f$ll[, 1L] - stats::pnorm(3, f$linpred[, 1L], f$pars$sigma[, 1L],
                              log.p = TRUE)
  )
})


test_that("a discrete lower bound excludes the step below it", {
  set.seed(3)
  S <- 5L
  N <- 3L
  linpred <- matrix(log(rep(4, S * N)), S, N)
  spec <- mvgam:::family_dist_spec("poisson", "log", linpred, list(), NULL)
  y <- c(2, 5, 7)
  ll <- mvgam:::dispatch_log_lik("poisson", "log", linpred, y, list(),
                                NULL)
  out <- mvgam:::apply_truncation_to_loglik(
    ll, rep(2, N), rep(9, N), spec, linpred, discrete = TRUE
  )
  # brms generates log_diff_exp(lcdf(ub), lcdf(lb - 1)) for a discrete
  # family. The bound itself keeps its mass.
  denom <- log(stats::ppois(9, 4) - stats::ppois(1, 4))
  expect_equal(out[, 1L], ll[, 1L] - denom)
})


test_that("a family with no distribution function is refused", {
  expect_null(
    mvgam:::family_dist_spec("cumulative", "logit",
                             matrix(0, 2L, 2L), list(), NULL)
  )
})


test_that("a family with no quantile function samples by rejection", {
  # extraDistr has no beta-binomial quantile function. The COM-binomial
  # and Tweedie kernels have none either. `truncated_dist_draws()`
  # samples by rejection when `dist_fun()` returns `NULL`.
  for (dist in c("bbinom", "cmb", "tweedie")) {
    expect_type(mvgam:::dist_fun(dist, "d"), "closure")
    expect_type(mvgam:::dist_fun(dist, "p"), "closure")
    expect_null(mvgam:::dist_fun(dist, "q"))
  }
  # A family served by stats carries all three.
  for (dist in c("pois", "norm", "binom")) {
    expect_type(mvgam:::dist_fun(dist, "q"), "closure")
  }
  spec <- list(
    dist = "bbinom",
    args = function(j) list(size = 10, alpha = 2, beta = 2)
  )
  expect_null(
    mvgam:::truncated_dist_draws(spec, matrix(TRUE, 2L, 2L), 0, 10, TRUE)
  )
})


test_that("a package distribution function passes a missing value through", {
  # The stats functions return NA for a missing value. The COM-binomial
  # distribution function and the Tweedie density raised an error
  # instead. One missing response then stopped every residual of a fit.
  args <- list(
    bbinom = list(size = 5, alpha = 2, beta = 2),
    cmb = list(mu = 0.4, nu = 1.5, size = 5),
    beta_nb = list(mu = 2, shape = 2, mtail = 3),
    tweedie = list(power = 1.5, mu = 2, phi = 1)
  )
  for (dist in names(args)) {
    for (kind in c("d", "p")) {
      out <- do.call(mvgam:::dist_fun(dist, kind),
                     c(list(c(NA, 1)), args[[dist]]))
      expect_identical(is.na(out), c(TRUE, FALSE))
    }
  }
})


test_that("the COM-binomial distribution function is the binomial at nu = 1", {
  # The quantiles cover no mass, a missing value, the full support and
  # beyond it. Every tail and scale has to match `pbinom()`.
  q <- c(-1, NA, 0, 2, 5, 7)
  mu <- seq(0.2, 0.8, length.out = length(q))
  for (lower_tail in c(TRUE, FALSE)) {
    for (log_p in c(TRUE, FALSE)) {
      expect_equal(
        mvgam:::pcmb(q, mu, nu = 1, size = 5,
                     lower.tail = lower_tail, log.p = log_p),
        stats::pbinom(q, 5, mu, lower.tail = lower_tail, log.p = log_p)
      )
    }
  }
})


test_that("addition terms are evaluated against the frame being scored", {
  # `standata` holds only the observed rows of the training data. The
  # scored frame carries a missing response in row 2.
  frame <- data.frame(
    y = c(1, NA, 4), w = c(1, 2, 3), cc = c("none", "right", "int"),
    up = c(NA, NA, 6), lo = c(0, 1, 0)
  )
  terms <- mvgam:::addition_term_values(
    brms::bf(y | weights(w) + cens(cc, up) + trunc(lb = lo, ub = 10) ~ 1),
    gaussian(), frame
  )
  expect_equal(terms$weights, c(1, 2, 3))
  # A label matches by prefix, as in brms
  expect_equal(terms$cens, c(0, 1, 2))
  expect_equal(terms$rcens, c(NA, NA, 6))
  expect_equal(terms$lb, c(0, 1, 0))
  expect_equal(terms$ub, rep(10, 3L))

  # `scale = TRUE` rescales the weights to sum to the row count
  scaled <- mvgam:::addition_term_values(
    brms::bf(y | weights(w, scale = TRUE) ~ 1), gaussian(), frame
  )
  expect_equal(scaled$weights, c(1, 2, 3) / 2)

  expect_error(
    mvgam:::addition_term_values(
      brms::bf(y | cens(cc) ~ 1), gaussian(),
      transform(frame, cc = c("none", "above", "none"))
    ),
    "'above'"
  )
})


test_that("quantile residual bounds follow censoring and truncation", {
  f <- addition_fixture()
  cdf <- mvgam:::dist_cdf(f$spec, f$linpred, f$y)
  out <- mvgam:::censored_pit_bounds(
    f$spec, f$linpred, f$y, f$y, list(cens = c(1, -1, 0, 0, 0)),
    discrete = FALSE
  )
  # A right-censored row spans (F(y), 1], a left-censored row [0, F(y)]
  expect_equal(out$lower[, 1:2], cbind(cdf[, 1L], 0))
  expect_equal(out$upper[, 1:2], cbind(1, cdf[, 2L]))
  expect_equal(out$lower[, 3:5], cdf[, 3:5])

  # Truncation below zero renormalises the distribution function
  trunc <- mvgam:::censored_pit_bounds(
    f$spec, f$linpred, f$y, f$y, list(lb = rep(0, 5L), ub = rep(Inf, 5L)),
    discrete = FALSE
  )
  below <- stats::pnorm(0, f$linpred, f$pars$sigma)
  expect_equal(trunc$upper, (cdf - below) / (1 - below))
})


test_that("a missing response leaves its column NA", {
  # The hurdle kernels branch on the response value. `if (NA)` would
  # abort the call.
  linpred <- matrix(log(3), 4L, 3L)
  ll <- log_lik_hurdle_poisson(linpred, "log", c(2, NA, 0),
                               list(hu = matrix(0.2, 4L, 3L)), NULL)
  expect_true(all(is.na(ll[, 2L])))
  expect_equal(ll[, 3L], rep(log(0.2), 4L))
})


test_that("resolve_incl_autocor takes the superseded spelling second", {
  # With only the superseded argument named, its value is used
  expect_false(resolve_incl_autocor(TRUE, legacy = FALSE,
                                     autocor_supplied = FALSE))
  expect_true(resolve_incl_autocor(FALSE, legacy = TRUE,
                                    autocor_supplied = FALSE))
  # With both named, the documented argument's value is used
  expect_false(resolve_incl_autocor(FALSE, legacy = TRUE,
                                     autocor_supplied = TRUE))
  expect_true(resolve_incl_autocor(TRUE, legacy = NULL,
                                    autocor_supplied = FALSE))
})


# ------------------------------------------------------------------
# The multi-response families' distribution functions
#
# Randomised quantile residuals are built from them. A residual on the
# wrong scale is still finite and varies by draw. Each distribution
# function is compared with its closed form.
# ------------------------------------------------------------------

mv_spec_fixture <- function(S = 4L, N = 6L) {
  set.seed(17)
  list(
    S = S, N = N,
    linpred = matrix(rnorm(S * N), S, N),
    Psi = matrix(runif(S * N, 0.3, 1.1), S, N),
    nu = runif(S, 3, 12),
    prob = matrix(runif(S * N, 0.1, 0.6), S, N),
    phi = matrix(runif(S * N, 5, 40), S, N),
    y = seq(-1, 1, length.out = N)
  )
}


test_that("the mvn distribution function is normal on the row scale", {
  f <- mv_spec_fixture()
  spec <- mvgam:::family_dist_spec(
    "mvn", "identity", f$linpred, list(Psi_row = f$Psi), NULL
  )
  got <- mvgam:::dist_cdf(spec, f$linpred, f$y)
  want <- vapply(seq_len(f$N), function(j) {
    stats::pnorm(f$y[j], mean = f$linpred[, j], sd = f$Psi[, j])
  }, numeric(f$S))
  expect_equal(got, want)
})


test_that("the mvt distribution function takes each row's location and scale", {
  f <- mv_spec_fixture()
  spec <- mvgam:::family_dist_spec(
    "mvt", "identity", f$linpred, list(Psi_row = f$Psi, nu = f$nu), NULL
  )
  got <- mvgam:::dist_cdf(spec, f$linpred, f$y)
  want <- vapply(seq_len(f$N), function(j) {
    stats::pt((f$y[j] - f$linpred[, j]) / f$Psi[, j], df = f$nu)
  }, numeric(f$S))
  expect_equal(got, want)
})


test_that("a Dirichlet component's second shape is the complement", {
  f <- mv_spec_fixture()
  y <- seq(0.1, 0.6, length.out = f$N)
  spec <- mvgam:::family_dist_spec(
    "diri", "identity", f$linpred,
    list(prob_row = f$prob, phi = f$phi), NULL
  )
  got <- mvgam:::dist_cdf(spec, f$linpred, y)
  want <- vapply(seq_len(f$N), function(j) {
    stats::pbeta(y[j], f$prob[, j] * f$phi[, j],
                 (1 - f$prob[, j]) * f$phi[, j])
  }, numeric(f$S))
  expect_equal(got, want)
})


test_that("a beta mean at the boundary has no transform to take", {
  # A share of one sets the second shape to zero. Every probability is
  # then a boundary, which `qnorm()` turns into an eight-sigma residual.
  guarded <- mvgam:::beta_shapes(c(0.4, 1, 0, NA), rep(10, 4L),
                                 boundary_na = TRUE)
  expect_equal(guarded$shape1, c(4, NA, NA, NA))
  expect_equal(guarded$shape2, c(6, NA, NA, NA))
  # A simulator can draw a degenerate beta. The guard is off by
  # default.
  bare <- mvgam:::beta_shapes(c(0.4, 1, 0, NA), rep(10, 4L))
  expect_equal(bare$shape1, c(4, 10, 0, NA))
  expect_equal(bare$shape2, c(6, 0, 10, NA))
})
