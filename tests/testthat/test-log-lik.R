# Targeted tests for log_lik.mvgam and the consumers it unblocks.
# These exercise the per-family log-density helpers in isolation and the
# dispatcher, plus signature compatibility for log_lik.mvgam itself.
# Numerical concordance against brms lives in
# tests/local/test-log-lik-brms-concordance.R and runs manually with a
# fitted fixture; this file is fixture-free and CI-safe.

# Helper: synthetic [ndraws x nobs] linpred + dpars + y for a few small
# families. Uses fixed seeds and small shapes to keep assertions stable.
.make_synthetic <- function(n_draws = 50, n_obs = 8, mu = 1.0, sd = 0.2) {
  set.seed(123)
  linpred <- matrix(rnorm(n_draws * n_obs, mean = mu, sd = sd),
                    nrow = n_draws, ncol = n_obs)
  list(
    linpred = linpred,
    sigma = matrix(0.5, nrow = n_draws, ncol = n_obs),
    shape = matrix(2.0, nrow = n_draws, ncol = n_obs),
    phi = matrix(5.0, nrow = n_draws, ncol = n_obs),
    nu = matrix(4.0, nrow = n_draws, ncol = n_obs),
    hu = matrix(0.2, nrow = n_draws, ncol = n_obs),
    zi = matrix(0.3, nrow = n_draws, ncol = n_obs)
  )
}

test_that("log_lik_gaussian returns finite [ndraws x nobs]", {
  s <- .make_synthetic()
  y <- rnorm(8, mean = 1, sd = 0.5)
  out <- log_lik_gaussian(
    linpred = s$linpred, link = "identity", y = y,
    family_pars = list(sigma = s$sigma), trials = NULL
  )
  expect_equal(dim(out), c(50, 8))
  expect_true(all(is.finite(out)))
})

test_that("log_lik_poisson returns finite [ndraws x nobs]", {
  s <- .make_synthetic(mu = 1.5)
  y <- rpois(8, lambda = exp(1.5))
  out <- log_lik_poisson(
    linpred = s$linpred, link = "log", y = y,
    family_pars = list(), trials = NULL
  )
  expect_equal(dim(out), c(50, 8))
  expect_true(all(is.finite(out)))
})

test_that("log_lik_negbinomial returns finite [ndraws x nobs]", {
  s <- .make_synthetic(mu = 1.5)
  y <- rnbinom(8, mu = exp(1.5), size = 2)
  out <- log_lik_negbinomial(
    linpred = s$linpred, link = "log", y = y,
    family_pars = list(shape = s$shape), trials = NULL
  )
  expect_equal(dim(out), c(50, 8))
  expect_true(all(is.finite(out)))
})

test_that("log_lik_binomial uses trials per observation", {
  s <- .make_synthetic(mu = 0)
  y <- rbinom(8, size = 10, prob = 0.5)
  out <- log_lik_binomial(
    linpred = s$linpred, link = "logit", y = y,
    family_pars = list(), trials = rep(10, 8)
  )
  expect_equal(dim(out), c(50, 8))
  expect_true(all(is.finite(out)))
})

test_that("log_lik_beta returns finite densities on (0, 1)", {
  s <- .make_synthetic(mu = 0)
  y <- rbeta(8, shape1 = 2, shape2 = 2)
  out <- log_lik_beta(
    linpred = s$linpred, link = "logit", y = y,
    family_pars = list(phi = s$phi), trials = NULL
  )
  expect_equal(dim(out), c(50, 8))
  expect_true(all(is.finite(out)))
})

test_that("log_lik_hurdle_poisson splits cleanly at y = 0", {
  s <- .make_synthetic(mu = 1.5)
  y <- c(0L, 0L, 1L, 2L, 3L, 0L, 4L, 5L)
  out <- log_lik_hurdle_poisson(
    linpred = s$linpred, link = "log", y = y,
    family_pars = list(hu = s$hu), trials = NULL
  )
  expect_equal(dim(out), c(50, 8))
  expect_true(all(is.finite(out)))
  # At y = 0 the log density equals log(hu); does not depend on mu draws
  expect_equal(out[1, 1], log(0.2), tolerance = 1e-10)
})

test_that("log_lik_zero_inflated_poisson splits cleanly at y = 0", {
  s <- .make_synthetic(mu = 1.5)
  y <- c(0L, 0L, 1L, 2L, 3L, 0L, 4L, 5L)
  out <- log_lik_zero_inflated_poisson(
    linpred = s$linpred, link = "log", y = y,
    family_pars = list(zi = s$zi), trials = NULL
  )
  expect_equal(dim(out), c(50, 8))
  expect_true(all(is.finite(out)))
  # P(Y=0) = zi + (1 - zi) * exp(-mu); must be > log(zi) since the mass at
  # zero from the Poisson is non-negative
  expect_true(all(out[, 1] >= log(0.3) - 1e-10))
})

test_that("dispatch_log_lik errors clearly for unknown family", {
  s <- .make_synthetic()
  expect_error(
    dispatch_log_lik(
      family_name = "definitely_not_a_family",
      link = "identity",
      linpred = s$linpred,
      y = rnorm(8),
      family_pars = list(),
      trials = NULL
    ),
    regexp = "not yet supported"
  )
})

test_that("log_lik.mvgam method is registered as S3", {
  expect_true(
    inherits(
      getS3method("log_lik", "mvgam", optional = TRUE),
      "function"
    )
  )
})

test_that("log_lik.mvgam signature matches brms log_lik.brmsfit", {
  brms_args <- names(formals(getS3method("log_lik", "brmsfit")))
  mvgam_args <- names(formals(getS3method("log_lik", "mvgam")))
  # Both should accept the brms-parity arguments
  parity_args <- c("object", "newdata", "re_formula", "resp",
                   "ndraws", "draw_ids")
  expect_true(all(parity_args %in% mvgam_args))
  # mvgam adds process_error, which brms doesn't have
  expect_true("process_error" %in% mvgam_args)
})

test_that("waic.mvgam method is registered as S3", {
  expect_true(
    inherits(
      getS3method("waic", "mvgam", optional = TRUE),
      "function"
    )
  )
})

test_that("waic.mvgam signature matches brms waic.brmsfit", {
  brms_args <- names(formals(getS3method("waic", "brmsfit")))
  mvgam_args <- names(formals(getS3method("waic", "mvgam")))
  parity_args <- c("x", "compare", "resp", "pointwise", "model_names")
  expect_true(all(parity_args %in% mvgam_args))
})

test_that("loo.mvgam no longer calls removed logLik / extract_family_pars", {
  src <- deparse(getS3method("loo", "mvgam"))
  src <- paste(src, collapse = "\n")
  expect_false(grepl("logLik\\(", src))
  expect_false(grepl("extract_family_pars", src))
  expect_true(grepl("log_lik\\(", src))
})


# logLik.mvgam: stats::logLik S3 method enabling AIC()/BIC().
test_that("logLik.mvgam method is registered as S3", {
  expect_true(
    inherits(
      getS3method("logLik", "mvgam", optional = TRUE),
      "function"
    )
  )
})


test_that("logLik.mvgam(pointwise = FALSE) returns scalar with df + nobs", {
  ll_mat <- matrix(c(-1, -2, -3, -4, -5, -6), nrow = 2, byrow = TRUE)
  testthat::local_mocked_bindings(
    log_lik.mvgam = function(object, ...) ll_mat,
    variables.mvgam = function(x, ...) c("b_Intercept", "sigma", "lp__"),
    nobs.mvgam = function(object, ...) 3L,
    .package = "mvgam"
  )
  obj <- structure(list(), class = "mvgam")
  out <- logLik.mvgam(obj)
  expect_s3_class(out, "logLik")
  expect_equal(length(out), 1L)
  # row sums: -6, -15; mean = -10.5
  expect_equal(as.numeric(out), -10.5)
  # df excludes lp__: counts b_Intercept + sigma = 2
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


test_that("logLik.mvgam strips all known NUTS diagnostic vars from df", {
  diag_pars <- c("lp__", "lprior", "accept_stat__", "stepsize__",
                 "treedepth__", "n_leapfrog__", "divergent__", "energy__")
  ll_mat <- matrix(-1, nrow = 4, ncol = 5)
  testthat::local_mocked_bindings(
    log_lik.mvgam = function(object, ...) ll_mat,
    variables.mvgam = function(x, ...) c("b_a", "b_b", "sigma", diag_pars),
    nobs.mvgam = function(object, ...) 5L,
    .package = "mvgam"
  )
  out <- logLik.mvgam(structure(list(), class = "mvgam"))
  # 3 real pars (b_a, b_b, sigma); the 8 diagnostics are stripped
  expect_equal(attr(out, "df"), 3L)
})


# ------------------------------------------------------------------
# Addition terms: weights(), cens() and trunc()
#
# Each changes what one row contributes to the likelihood, and every
# information criterion reads that matrix, so the arithmetic is
# checked directly against the reference density rather than through
# a fit.
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
    ll = mvgam:::log_lik_gaussian(linpred, "identity", y, pars, NULL)
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
    "no upper bound"
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
    stats::pnorm(3, f$linpred[, 1L], f$pars$sigma[, 1L]) -
      stats::pnorm(-1, f$linpred[, 1L], f$pars$sigma[, 1L])
  )
  expect_equal(out[, 1L], f$ll[, 1L] - denom)
  # Truncation always removes mass, so contributions rise.
  expect_true(all(out >= f$ll))
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
  ll <- mvgam:::log_lik_poisson(linpred, "log", y, list(), NULL)
  out <- mvgam:::apply_truncation_to_loglik(
    ll, rep(2, N), rep(9, N), spec, linpred, discrete = TRUE
  )
  # brms generates log_diff_exp(lcdf(ub), lcdf(lb - 1)) for a discrete
  # family, so the bound itself keeps its mass.
  denom <- log(stats::ppois(9, 4) - stats::ppois(1, 4))
  expect_equal(out[, 1L], ll[, 1L] - denom)
})


test_that("a family with no distribution function is refused", {
  expect_null(
    mvgam:::family_dist_spec("cumulative", "logit",
                             matrix(0, 2L, 2L), list(), NULL)
  )
})


test_that("addition-term keys are read per response", {
  sdata <- list(weights = rep(2, 4L), weights_y1 = rep(5, 4L))
  obj <- structure(list(standata = sdata), class = "mvgam")

  # A univariate fit reads the bare key.
  uni <- mvgam:::addition_term_data(obj, NULL, nobs = 4L)
  expect_equal(uni$weights, rep(2, 4L))

  # A named response reads only its own suffixed key, so one arm of a
  # multivariate fit cannot inherit another's weights.
  y1 <- mvgam:::addition_term_data(obj, "y1", nobs = 4L)
  expect_equal(y1$weights, rep(5, 4L))
  y2 <- mvgam:::addition_term_data(obj, "y2", nobs = 4L)
  expect_null(y2$weights)
})


test_that("a scalar bound is recycled across observations", {
  obj <- structure(list(standata = list(lb = 0)), class = "mvgam")
  out <- mvgam:::addition_term_data(obj, NULL, nobs = 6L)
  expect_equal(out$lb, rep(0, 6L))
})


test_that("terms recorded at the wrong grain are refused", {
  # A closure-unit family scores once per unit while the terms are
  # recorded per visit, so applying them would silently misalign.
  obj <- structure(
    list(standata = list(weights = rep(1.5, 12L))), class = "mvgam"
  )
  expect_error(
    mvgam:::addition_term_data(obj, NULL, nobs = 4L),
    "does not line up with the likelihood"
  )
})


test_that("a missing response yields NA rather than aborting", {
  # The hurdle, zero-inflated and ordinal kernels branch on the
  # response value, so `if (NA)` would abort the whole call. The
  # guard lives in `.apply_log_density()` so every kernel inherits it.
  linpred <- matrix(log(3), 4L, 3L)
  y <- c(2, NA, 5)
  ll <- mvgam:::.apply_log_density(linpred, y, function(yj, j) {
    if (yj == 0) rep(-1, nrow(linpred)) else stats::dpois(
      yj, lambda = exp(linpred[, j]), log = TRUE
    )
  })
  expect_true(all(is.na(ll[, 2L])))
  expect_true(all(is.finite(ll[, -2L])))
})


test_that("resolve_incl_autocor keeps the superseded spelling working", {
  # Nothing supplied but the current argument: it is used as given.
  expect_true(resolve_incl_autocor(TRUE, legacy = NULL,
                                    autocor_supplied = FALSE))
  expect_false(resolve_incl_autocor(FALSE, legacy = NULL,
                                     autocor_supplied = FALSE))
  # Only the superseded argument named: it decides.
  expect_false(resolve_incl_autocor(TRUE, legacy = FALSE,
                                     autocor_supplied = FALSE))
  expect_true(resolve_incl_autocor(FALSE, legacy = TRUE,
                                    autocor_supplied = FALSE))
})


test_that("resolve_incl_autocor prefers the current spelling", {
  # Both named: the documented argument wins and the superseded value
  # is ignored rather than combined with it.
  expect_false(resolve_incl_autocor(FALSE, legacy = TRUE,
                                     autocor_supplied = TRUE))
  expect_true(resolve_incl_autocor(TRUE, legacy = FALSE,
                                    autocor_supplied = TRUE))
})


test_that("resolve_incl_autocor rejects a non-logical", {
  expect_error(
    resolve_incl_autocor("yes", legacy = NULL, autocor_supplied = FALSE),
    "logical"
  )
  expect_error(
    resolve_incl_autocor(TRUE, legacy = "no", autocor_supplied = FALSE),
    "logical"
  )
  expect_error(
    resolve_incl_autocor(NA, legacy = NULL, autocor_supplied = FALSE),
    "missing"
  )
})


# ------------------------------------------------------------------
# The multi-response families' distribution functions.
#
# These three are what a randomised quantile residual is built from,
# and a residual on the wrong scale is not visibly wrong: it is
# finite, it moves with the draw and it fills every column. So each
# spec is pinned here against its closed form, computed from the same
# parameters but written out rather than looked up, and each check is
# one a plausible mistake would fail -- reading `nu` without the
# location and scale, or taking a Dirichlet component's second shape
# to be the whole concentration rather than its complement.
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


test_that("the mvt distribution function carries location and scale", {
  f <- mv_spec_fixture()
  spec <- mvgam:::family_dist_spec(
    "mvt", "identity", f$linpred, list(Psi_row = f$Psi, nu = f$nu), NULL
  )
  got <- mvgam:::dist_cdf(spec, f$linpred, f$y)
  want <- vapply(seq_len(f$N), function(j) {
    stats::pt((f$y[j] - f$linpred[, j]) / f$Psi[, j], df = f$nu)
  }, numeric(f$S))
  expect_equal(got, want)
  # A scale of one everywhere is the only case where dropping the
  # standardisation agrees, so the two must differ here.
  bare <- vapply(seq_len(f$N), function(j) stats::pt(f$y[j], df = f$nu),
                 numeric(f$S))
  expect_false(isTRUE(all.equal(got, bare)))
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
  # Taking alpha_0 itself as the second shape rather than the
  # complement is the mistake this pins, and it is not a small one.
  wrong <- vapply(seq_len(f$N), function(j) {
    stats::pbeta(y[j], f$prob[, j] * f$phi[, j], f$phi[, j])
  }, numeric(f$S))
  expect_gt(max(abs(got - wrong)), 0.05)
})


test_that("a beta mean at the boundary has no transform to take", {
  # A share of one leaves the second shape at zero, where every
  # probability is a boundary and `qnorm` would report the clamp as
  # an eight-sigma residual. NA is the honest answer.
  guarded <- mvgam:::beta_shapes(c(0.4, 1, 0, NA), rep(10, 4L),
                                 boundary_na = TRUE)
  expect_equal(guarded$shape1, c(4, NA, NA, NA))
  expect_equal(guarded$shape2, c(6, NA, NA, NA))
  # A simulator draws a degenerate beta happily, so it keeps the
  # shapes it was given and the guard is off by default.
  bare <- mvgam:::beta_shapes(c(0.4, 1, 0, NA), rep(10, 4L))
  expect_equal(bare$shape1, c(4, 10, 0, NA))
  expect_equal(bare$shape2, c(6, 0, 10, NA))
})
