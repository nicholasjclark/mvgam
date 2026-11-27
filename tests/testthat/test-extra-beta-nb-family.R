# The tests of test-beta-nb-family.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()
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

# ---- stancode and standata -----------------------------------------

test_that("beta_nb() emits its likelihood, bounds and default priors", {
  dat <- bnb_test_data()
  sc <- paste(unlist(stancode(mvgam_formula(y ~ x), data = dat,
                              family = beta_nb(), backend = "cmdstanr",
                              validate = FALSE)),
              collapse = "\n")
  expect_true(grepl("target += beta_nb_lpmf(Y[n] | mu[n], shape, mtail);",
                    sc, fixed = TRUE))
  expect_true(grepl("real<lower=0> shape", sc, fixed = TRUE))
  expect_true(grepl("real<lower=0> mtail", sc, fixed = TRUE))
  # `shape` would otherwise inherit brms's negative binomial
  # gamma(0.01, 0.01), which concentrates mass where the Beta mixing
  # scale mu * mtail / shape diverges
  expect_true(grepl("gamma_lpdf(shape | 2, 0.5)", sc, fixed = TRUE))
  expect_true(grepl("gamma_lpdf(mtail | 2, 0.25)", sc, fixed = TRUE))
})

test_that("beta_nb() builds identically under either backend", {
  # The density avoids Stan 2.36 built-ins precisely so the family is
  # not restricted to cmdstanr; the two backends must agree byte for
  # byte, otherwise the rstan path is emitting something different.
  dat <- bnb_test_data()
  mf <- mvgam_formula(y ~ x)
  sc_r <- paste(unlist(stancode(mf, data = dat, family = beta_nb(),
                                backend = "rstan",
                                validate = FALSE)), collapse = "\n")
  sc_c <- paste(unlist(stancode(mf, data = dat, family = beta_nb(),
                                backend = "cmdstanr",
                                validate = FALSE)), collapse = "\n")
  expect_identical(sc_r, sc_c)
})

test_that("beta_nb() supports distributional regression on mtail", {
  dat <- bnb_test_data()
  dat$site <- factor(rep(c("a", "b"), length.out = nrow(dat)))
  mf <- mvgam_formula(brms::bf(y ~ 1, mtail ~ site))
  sc <- paste(unlist(stancode(mf, data = dat, family = beta_nb(),
                              backend = "cmdstanr",
                              validate = FALSE)), collapse = "\n")
  expect_true(grepl("mtail = exp(mtail);", sc, fixed = TRUE))
  expect_true(grepl("target += beta_nb_lpmf(Y[n] | mu[n], shape, mtail[n]);",
                    sc, fixed = TRUE))
  # The regressed intercept takes the brms intercept default in place
  # of the flat prior brms gives a custom family's dpar.
  expect_true(grepl("lprior += student_t_lpdf(Intercept_mtail | 3, 0, 2.5);",
                    sc, fixed = TRUE))
})

test_that("cens() and trunc() reach the distribution function", {
  # These addition terms are the reason the Stan code ships a lcdf and
  # lccdf at all rather than the density alone
  dat <- bnb_test_data()
  dat$ycens <- pmin(dat$y, 12L)
  dat$cn <- ifelse(dat$y > 12, "right", "none")
  sc_cens <- paste(unlist(stancode(
    mvgam_formula(ycens | cens(cn) ~ 1), data = dat,
    family = beta_nb(), backend = "cmdstanr",
    validate = FALSE)), collapse = "\n")
  expect_true(grepl("beta_nb_lccdf(", sc_cens, fixed = TRUE))

  sc_trunc <- paste(unlist(stancode(
    mvgam_formula(y | trunc(ub = 200) ~ 1), data = dat,
    family = beta_nb(), backend = "cmdstanr",
    validate = FALSE)), collapse = "\n")
  expect_true(grepl("beta_nb_lcdf(", sc_trunc, fixed = TRUE))
})

test_that("rbeta_nb_mvgam() draws non-negative integers with the right mean", {
  set.seed(42)
  y <- mvgam:::rbeta_nb_mvgam(2e5, mu = 10, shape = 3, mtail = 4)
  expect_length(y, 2e5)
  expect_true(all(y >= 0))
  expect_true(all(y == floor(y)))
  # The standard error of the mean is near 0.2% at this size
  expect_equal(mean(y), 10, tolerance = 0.02)
})
