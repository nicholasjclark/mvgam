# CI tests for beta_nb(). Covers the constructor contract, the Stan
# function block, stancode and standata, registration in the dpar /
# integer / epred tables, and the R-side density and RNG. Fitting,
# parameter recovery and forecast calibration live in
# `tests/local/test-beta-nb-fitting.R` so CI stays fast and needs no
# Stan installation.

bnb_test_data <- function(n = 60L) {
  set.seed(11)
  data.frame(
    y = rnbinom(n, size = 2, mu = 8),
    x = rnorm(n),
    time = seq_len(n),
    series = factor(rep("series1", n))
  )
}


# ---- constructor ---------------------------------------------------

test_that("beta_nb() returns a customfamily with the documented contract", {
  fam <- beta_nb()
  expect_s3_class(fam, "customfamily")
  expect_identical(fam$name, "beta_nb")
  expect_identical(fam$dpars, c("mu", "shape", "mtail"))
  expect_identical(fam$type, "int")
  expect_identical(fam$link, "log")
  expect_identical(fam$link_shape, "log")
  expect_identical(fam$link_mtail, "log")
  # Both dispersion parameters are strictly positive; mtail = alpha - 1
  # carries the alpha > 1 constraint that makes the mean exist
  expect_identical(unname(unlist(fam$lb)), c(NA, "0", "0"))
})

test_that("beta_nb() attaches linkinv / linkfun for the epred dispatcher", {
  fam <- beta_nb()
  expect_true(is.function(fam$linkinv))
  expect_equal(fam$linkinv(log(7)), 7)
  expect_equal(fam$linkfun(7), log(7))
})

test_that("beta_nb() carries its Stan functions as stanvars", {
  sv <- attr(beta_nb(), "mvgam_stanvars", exact = TRUE)
  expect_s3_class(sv, "stanvars")
  scode <- paste(vapply(sv, function(x) x$scode, character(1)), collapse = "\n")
  expect_true(grepl("real beta_nb_lpmf(int y, real mu, real shape, real mtail)",
                    scode, fixed = TRUE))
  expect_true(grepl("real beta_nb_lcdf(", scode, fixed = TRUE))
  expect_true(grepl("real beta_nb_lccdf(", scode, fixed = TRUE))
})

test_that("the Stan wrapper does not shadow Stan's own beta_neg_binomial", {
  # A function named beta_neg_binomial_lpmf taking three reals would
  # match the built-in signature on Stan >= 2.36 and bind the arguments
  # to the wrong roles, silently changing the likelihood.
  scode <- mvgam:::beta_nb_stan_funs()
  expect_false(grepl("real beta_neg_binomial_lpmf", scode, fixed = TRUE))
  # and the density must be self-contained, so no minimum Stan version
  # is needed and either backend can compile it
  expect_false(grepl("beta_neg_binomial_", scode, fixed = TRUE))
  expect_true(grepl("lgamma(", scode, fixed = TRUE))
  expect_true(grepl("lbeta(", scode, fixed = TRUE))
})


# ---- stancode and standata -----------------------------------------

test_that("beta_nb() emits its functions and bounded dpars into stancode", {
  dat <- bnb_test_data()
  mf <- mvgam_formula(y ~ x)
  sc <- paste(unlist(stancode(mf, data = dat, family = beta_nb(),
                              backend = "cmdstanr")), collapse = "\n")
  expect_true(grepl("beta_nb_lpmf", sc, fixed = TRUE))
  expect_true(grepl("real<lower=0> shape", sc, fixed = TRUE))
  expect_true(grepl("real<lower=0> mtail", sc, fixed = TRUE))
})

test_that("beta_nb() builds identically under either backend", {
  # The density avoids Stan 2.36 built-ins precisely so the family is
  # not restricted to cmdstanr; the two backends must agree byte for
  # byte, otherwise the rstan path is emitting something different.
  dat <- bnb_test_data()
  mf <- mvgam_formula(y ~ x)
  sc_r <- paste(unlist(stancode(mf, data = dat, family = beta_nb(),
                                backend = "rstan")), collapse = "\n")
  sc_c <- paste(unlist(stancode(mf, data = dat, family = beta_nb(),
                                backend = "cmdstanr")), collapse = "\n")
  expect_identical(sc_r, sc_c)
})

test_that("beta_nb() injects its default priors ahead of the brms defaults", {
  # `shape` would otherwise inherit brms's negative binomial
  # gamma(0.01, 0.01), which concentrates mass where the Beta mixing
  # scale mu * mtail / shape diverges
  dat <- bnb_test_data()
  sc <- paste(unlist(stancode(mvgam_formula(y ~ x), data = dat,
                              family = beta_nb(), backend = "cmdstanr")),
              collapse = "\n")
  expect_true(grepl("gamma_lpdf(shape | 2, 0.5)", sc, fixed = TRUE))
  expect_true(grepl("gamma_lpdf(mtail | 2, 0.25)", sc, fixed = TRUE))
  expect_false(grepl("gamma_lpdf(shape | 0.01, 0.01)", sc, fixed = TRUE))
})

test_that("beta_nb() supports distributional regression on mtail", {
  dat <- bnb_test_data()
  dat$site <- factor(rep(c("a", "b"), length.out = nrow(dat)))
  mf <- mvgam_formula(brms::bf(y ~ 1, mtail ~ site))
  sc <- paste(unlist(stancode(mf, data = dat, family = beta_nb(),
                              backend = "cmdstanr")), collapse = "\n")
  expect_true(grepl("b_mtail", sc, fixed = TRUE))
  expect_true(grepl("X_mtail", sc, fixed = TRUE))
  sd <- standata(mf, data = dat, family = beta_nb(), backend = "cmdstanr")
  expect_true(all(c("K_mtail", "X_mtail") %in% names(sd)))
})

test_that("cens() and trunc() reach the distribution function", {
  # These addition terms are the reason the Stan code ships a lcdf and
  # lccdf at all rather than the density alone
  dat <- bnb_test_data()
  dat$ycens <- pmin(dat$y, 12L)
  dat$cn <- ifelse(dat$y > 12, "right", "none")
  sc_cens <- paste(unlist(stancode(
    mvgam_formula(ycens | cens(cn) ~ 1), data = dat,
    family = beta_nb(), backend = "cmdstanr")), collapse = "\n")
  expect_true(grepl("beta_nb_lccdf(", sc_cens, fixed = TRUE))

  sc_trunc <- paste(unlist(stancode(
    mvgam_formula(y | trunc(ub = 200) ~ 1), data = dat,
    family = beta_nb(), backend = "cmdstanr")), collapse = "\n")
  expect_true(grepl("beta_nb_lcdf(", sc_trunc, fixed = TRUE))
})


# ---- registration --------------------------------------------------

test_that("beta_nb() is registered everywhere the dispatchers look", {
  expect_identical(mvgam:::get_family_dpars("beta_nb"), c("shape", "mtail"))
  expect_true(mvgam:::family_uses_integers("beta_nb"))
  expect_true(mvgam:::is_beta_nb_family(beta_nb()))
  expect_false(mvgam:::is_beta_nb_family(poisson()))
  expect_false(mvgam:::is_closure_unit_family(beta_nb()))
  expect_invisible(mvgam:::validate_supported_family(beta_nb()))
  # Both dpars must be reachable as arguments of the sampler, or
  # posterior_predict() errors only once someone fits the model
  expect_true(all(mvgam:::get_family_dpars("beta_nb") %in%
                    names(formals(mvgam:::sample_from_family))))
})

test_that("beta_nb() has a mean-equals-mu epred", {
  linpred <- matrix(log(c(2, 5, 9)), nrow = 1)
  ep <- mvgam:::compute_family_epred(
    linpred = linpred, family = beta_nb()
  )
  # The shifted tail parameter exists so that E[Y] = mu exactly
  expect_equal(as.numeric(ep), c(2, 5, 9), tolerance = 1e-12)
})


# ---- R-side density and RNG ----------------------------------------

test_that("dbeta_nb_mvgam() is a proper probability mass function", {
  for (cfg in list(c(5, 2, 2), c(20, 1.5, 1.2), c(2, 0.7, 0.5))) {
    p <- mvgam:::dbeta_nb_mvgam(0:200000, cfg[1], cfg[2], cfg[3], log = FALSE)
    expect_true(all(p >= 0))
    # The remaining gap is the tail beyond the truncation point, which
    # is the point of the family; it is not numerical error
    expect_equal(sum(p), 1, tolerance = 1e-6)
  }
})

test_that("dbeta_nb_mvgam() puts the mean exactly at mu", {
  # E[Y] = r * beta / (alpha - 1) with alpha = 1 + mtail and
  # beta = mu * mtail / shape, which collapses to mu
  for (cfg in list(c(5, 2, 2), c(12, 3, 4))) {
    mu <- cfg[1]
    k <- 0:100000
    p <- mvgam:::dbeta_nb_mvgam(k, mu, cfg[2], cfg[3], log = FALSE)
    expect_equal(sum(k * p), mu, tolerance = 1e-4)
  }
})

test_that("rbeta_nb_mvgam() draws non-negative integers with the right mean", {
  set.seed(42)
  y <- mvgam:::rbeta_nb_mvgam(2e5, mu = 10, shape = 3, mtail = 4)
  expect_length(y, 2e5)
  expect_true(all(y >= 0))
  expect_true(all(y == floor(y)))
  expect_equal(mean(y), 10, tolerance = 0.5)
})

test_that("log_lik_beta_nb() matches the closed-form density", {
  set.seed(3)
  ndraws <- 5L
  nobs <- 7L
  linpred <- matrix(log(runif(ndraws * nobs, 2, 20)), ndraws, nobs)
  shape <- matrix(runif(ndraws * nobs, 0.5, 4), ndraws, nobs)
  mtail <- matrix(runif(ndraws * nobs, 0.5, 6), ndraws, nobs)
  y <- rpois(nobs, 8)

  ll <- mvgam:::log_lik_beta_nb(
    linpred = linpred, link = "log", y = y,
    family_pars = list(shape = shape, mtail = mtail), trials = NULL
  )
  expect_equal(dim(ll), c(ndraws, nobs))
  expect_true(all(is.finite(ll)))

  expected <- matrix(NA_real_, ndraws, nobs)
  for (i in seq_len(ndraws)) {
    for (j in seq_len(nobs)) {
      expected[i, j] <- mvgam:::dbeta_nb_mvgam(
        y[j], exp(linpred[i, j]), shape[i, j], mtail[i, j], log = TRUE
      )
    }
  }
  expect_equal(ll, expected, tolerance = 1e-12)
})

test_that("log_lik_beta_nb() rejects links other than log", {
  linpred <- matrix(0, 2, 2)
  pars <- list(shape = matrix(1, 2, 2), mtail = matrix(1, 2, 2))
  expect_error(
    mvgam:::log_lik_beta_nb(linpred, "identity", c(1, 2), pars, NULL),
    "log"
  )
})


# ---- simulation ----------------------------------------------------

test_that("sim_mvgam() can generate from beta_nb()", {
  set.seed(8)
  sim <- sim_mvgam(family = beta_nb(), n_series = 2L, n_timepoints = 40L,
                   trend_model = AR(),
                   family_pars = list(shape = 2, mtail = 3))
  y <- sim$data_train$y
  expect_true(all(y >= 0))
  expect_true(all(y == floor(y)))
  expect_true(is.factor(sim$data_train$series))
})


# ---- distribution function -----------------------------------------

test_that("pbeta_nb_mvgam() has no mass below zero", {
  # brms builds the two-sided truncation normaliser as
  # log_diff_exp(lcdf(ub), lcdf(lb - 1)), so `trunc(lb = 0, ...)` calls
  # the distribution function at -1. Returning anything other than -Inf
  # there silently subtracts the zero count from the normalising
  # constant and biases every observation.
  expect_identical(mvgam:::pbeta_nb_mvgam(-1, 5, 2, 2), -Inf)
  expect_identical(mvgam:::pbeta_nb_mvgam(-3, 5, 2, 2), -Inf)
  expect_equal(mvgam:::pbeta_nb_mvgam(-1, 5, 2, 2, log.p = FALSE), 0)
})

test_that("pbeta_nb_mvgam() equals the mass function at zero", {
  for (cfg in list(c(5, 2, 2), c(20, 1.5, 1.2), c(2, 0.7, 0.5))) {
    expect_equal(
      mvgam:::pbeta_nb_mvgam(0, cfg[1], cfg[2], cfg[3]),
      mvgam:::dbeta_nb_mvgam(0, cfg[1], cfg[2], cfg[3], log = TRUE)
    )
  }
})

test_that("pbeta_nb_mvgam() matches a direct cumulative sum of the pmf", {
  for (cfg in list(c(5, 2, 2), c(20, 1.5, 1.2), c(50, 3, 5))) {
    q <- 0:60
    got <- mvgam:::pbeta_nb_mvgam(q, cfg[1], cfg[2], cfg[3])
    want <- log(cumsum(
      mvgam:::dbeta_nb_mvgam(q, cfg[1], cfg[2], cfg[3], log = FALSE)
    ))
    expect_equal(got, want, tolerance = 1e-12)
  }
})

test_that("pbeta_nb_mvgam() is monotone and bounded by one", {
  p <- mvgam:::pbeta_nb_mvgam(0:400, 20, 1.5, 1.2, log.p = FALSE)
  expect_true(all(diff(p) >= 0))
  expect_true(all(p >= 0 & p <= 1))
})

test_that("the Stan distribution function guards against negative counts", {
  # Same contract as the R side, checked on the emitted Stan string so
  # the two implementations cannot drift apart
  scode <- mvgam:::beta_nb_stan_funs()
  expect_true(grepl("if (y < 0) {", scode, fixed = TRUE))
  expect_true(grepl("return negative_infinity();", scode, fixed = TRUE))
})
