
# ---- constructor ---------------------------------------------------

test_that("beta_nb() returns a customfamily with the documented contract", {
  fam <- beta_nb()
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
  expect_equal(fam$linkinv(log(7)), 7)
  expect_equal(fam$linkfun(7), log(7))
})

test_that("beta_nb() carries its Stan functions as stanvars", {
  sv <- attr(beta_nb(), "mvgam_stanvars", exact = TRUE)
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

# ---- registration --------------------------------------------------

test_that("beta_nb() is registered everywhere the dispatchers look", {
  expect_identical(mvgam:::get_family_dpars("beta_nb"), c("shape", "mtail"))
  expect_true(mvgam:::family_uses_integers("beta_nb"))
  expect_true(mvgam:::is_beta_nb_family(beta_nb()))
  expect_false(mvgam:::is_beta_nb_family(poisson()))
  expect_false(mvgam:::uses_closure_unit_layout(beta_nb()))
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


test_that("the beta_nb log density matches the closed-form density", {
  set.seed(3)
  ndraws <- 5L
  nobs <- 7L
  linpred <- matrix(log(runif(ndraws * nobs, 2, 20)), ndraws, nobs)
  shape <- matrix(runif(ndraws * nobs, 0.5, 4), ndraws, nobs)
  mtail <- matrix(runif(ndraws * nobs, 0.5, 6), ndraws, nobs)
  y <- rpois(nobs, 8)

  ll <- mvgam:::dispatch_log_lik(
    "beta_nb", linpred = linpred, link = "log", y = y,
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

# ---- simulation ----------------------------------------------------

test_that("sim_mvgam() can generate from beta_nb()", {
  set.seed(8)
  sim <- sim_mvgam(family = beta_nb(), n_series = 2L, n_timepoints = 40L,
                   trend_model = AR(),
                   family_pars = list(shape = 2, mtail = 3))
  y <- sim$data_train$y
  expect_true(all(y >= 0))
  expect_true(all(y == floor(y)))
})

# ---- distribution function -----------------------------------------

test_that("pbeta_nb_mvgam() has no mass below zero", {
  # brms builds the two-sided truncation normaliser as
  # log_diff_exp(lcdf(ub), lcdf(lb - 1)), so `trunc(lb = 0, ...)` calls
  # the distribution function at -1. Returning anything other than -Inf
  # there silently subtracts the zero count from the normalising
  # constant and biases every observation.
  expect_identical(mvgam:::pbeta_nb_mvgam(-1, 5, 2, 2, log.p = TRUE), -Inf)
  expect_identical(mvgam:::pbeta_nb_mvgam(-3, 5, 2, 2, log.p = TRUE), -Inf)
  expect_identical(mvgam:::pbeta_nb_mvgam(c(-Inf, Inf), 5, 2, 2), c(0, 1))
  expect_identical(
    mvgam:::pbeta_nb_mvgam(c(-Inf, Inf), 5, 2, 2, lower.tail = FALSE,
                           log.p = TRUE),
    c(0, -Inf)
  )
  expect_equal(mvgam:::pbeta_nb_mvgam(-1, 5, 2, 2), 0)
  expect_equal(mvgam:::dbeta_nb_mvgam(c(-1, 1.5, Inf), 5, 2, 2), c(0, 0, 0))
  # Clamping accumulated log-CDF at 0 keeps the upper tail finite in the far right tail
  far <- mvgam:::pbeta_nb_mvgam(5000, 1, 2, 5, lower.tail = FALSE,
                                log.p = TRUE)
  expect_false(is.nan(far))
  expect_true(far <= 0)
})

test_that("pbeta_nb_mvgam() equals the mass function at zero", {
  for (cfg in list(c(5, 2, 2), c(20, 1.5, 1.2), c(2, 0.7, 0.5))) {
    expect_equal(
      mvgam:::pbeta_nb_mvgam(0, cfg[1], cfg[2], cfg[3], log.p = TRUE),
      mvgam:::dbeta_nb_mvgam(0, cfg[1], cfg[2], cfg[3], log = TRUE)
    )
  }
})

test_that("pbeta_nb_mvgam() matches a direct cumulative sum of the pmf", {
  for (cfg in list(c(5, 2, 2), c(20, 1.5, 1.2), c(50, 3, 5))) {
    q <- 0:60
    got <- mvgam:::pbeta_nb_mvgam(q, cfg[1], cfg[2], cfg[3],
                                  log.p = TRUE)
    want <- log(cumsum(mvgam:::dbeta_nb_mvgam(q, cfg[1], cfg[2], cfg[3])))
    expect_equal(got, want, tolerance = 1e-12)
  }
})

test_that("pbeta_nb_mvgam() is monotone and bounded by one", {
  p <- mvgam:::pbeta_nb_mvgam(0:400, 20, 1.5, 1.2)
  expect_true(all(diff(p) >= 0))
  expect_true(all(p >= 0 & p <= 1))
})

test_that("pbeta_nb_mvgam() recycles every argument elementwise", {
  # The spec pairs one scalar quantile with a column of draws. Each
  # parameter arrives as a vector of draws.
  mu <- c(2, 8, 30)
  shape <- c(0.8, 2, 4)
  mtail <- c(1, 3, 6)
  got <- mvgam:::pbeta_nb_mvgam(5, mu, shape, mtail)
  want <- vapply(1:3, function(i) {
    mvgam:::pbeta_nb_mvgam(5, mu[i], shape[i], mtail[i])
  }, numeric(1))
  expect_equal(got, want)
  expect_equal(
    mvgam:::pbeta_nb_mvgam(5, mu, shape, mtail, lower.tail = FALSE),
    1 - want
  )
  expect_equal(
    mvgam:::pbeta_nb_mvgam(5, mu, shape, mtail, lower.tail = FALSE,
                           log.p = TRUE),
    log1p(-want)
  )
})

test_that("cens() and trunc() are applied post-fit through the spec", {
  # Stan scores a censored or truncated beta_nb with `beta_nb_lcdf`.
  # log_lik() needs the same distribution function, or `loo()` on
  # such a fit refuses.
  set.seed(4)
  linpred <- matrix(log(runif(12, 3, 9)), 4, 3)
  pars <- list(shape = matrix(1.5, 4, 3), mtail = matrix(2, 4, 3))
  expect_true(mvgam:::family_has_dist_spec("beta_nb", "log"))
  spec <- mvgam:::family_dist_spec("beta_nb", "log", linpred, pars, NULL)
  y <- c(2, 5, 12)
  got <- mvgam:::dist_cdf(spec, linpred, y)
  want <- sapply(1:3, function(j) {
    mvgam:::pbeta_nb_mvgam(y[j], exp(linpred[, j]), 1.5, 2)
  })
  expect_equal(got, want)
  # Truncating to [2, 20] renormalises by the mass on that interval.
  ll <- mvgam:::dispatch_log_lik("beta_nb", "log", linpred, y, pars,
                                 NULL)
  trunc_ll <- mvgam:::apply_truncation_to_loglik(
    ll, rep(2, 3), rep(20, 3), spec, linpred, discrete = TRUE
  )
  mass <- mvgam:::dist_cdf(spec, linpred, 20) -
    mvgam:::dist_cdf(spec, linpred, 1)
  expect_equal(trunc_ll, ll - log(mass))
  # Lower-only truncation passes ub = Inf through log_interval_mass()
  lower_trunc_ll <- mvgam:::apply_truncation_to_loglik(
    ll, rep(1, 3), NULL, spec, linpred, discrete = TRUE
  )
  lower_mass <- 1 - mvgam:::dist_cdf(spec, linpred, 0)
  expect_equal(lower_trunc_ll, ll - log(lower_mass))
})

test_that("the Stan distribution function guards against negative counts", {
  # Same contract as the R side, checked on the emitted Stan string so
  # the two implementations cannot drift apart
  scode <- mvgam:::beta_nb_stan_funs()
  expect_true(grepl("if (y < 0) {", scode, fixed = TRUE))
  expect_true(grepl("return negative_infinity();", scode, fixed = TRUE))
})
