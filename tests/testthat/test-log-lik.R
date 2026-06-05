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
