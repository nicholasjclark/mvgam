# The tests of test-tweedie-family.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()
# Tests for the Tweedie custom family. The local suite fits and
# recovers it.

test_that("mvgam(family = tweedie()) auto-injects the Stan function block + M data", {
  set.seed(1L)
  dat <- data.frame(
    y = c(rep(0, 30), rgamma(50, shape = 2, rate = 0.5)),
    x = rnorm(80),
    time = 1:80,
    series = factor(rep("s1", 80))
  )
  mf <- mvgam_formula(y ~ x)
  sc <- paste(unlist(stancode(mf, data = dat, family = tweedie(),
                              validate = FALSE)),
              collapse = "\n")
  # The likelihood takes the response-scale mean and both dpars.
  expect_true(grepl("mu = exp(mu);", sc, fixed = TRUE))
  expect_true(grepl("target += tweedie_lpdf(Y | mu, mphi, mtheta, M);",
                    sc, fixed = TRUE))
  # M data stanvar.
  expect_true(grepl("int<lower=1> M;", sc))
  # mphi parameter with strict lb and no ub.
  expect_true(grepl("<lower=1e-08>\\s*mphi", sc))
  # mtheta parameter restricted to the open (1, 2) interval.
  expect_true(grepl("<lower=1.001,\\s*upper=1.999>\\s*mtheta", sc))
})

test_that("standata under tweedie() carries M with the requested value", {
  set.seed(1L)
  dat <- data.frame(
    y = c(rep(0, 20), rgamma(30, shape = 2, rate = 0.5)),
    time = 1:50,
    series = factor(rep("s1", 50))
  )
  mf <- mvgam_formula(y ~ 1)
  sd_default <- standata(mf, data = dat, family = tweedie())
  sd_custom <- standata(mf, data = dat, family = tweedie(M = 75L))
  expect_equal(sd_default$M, 30L)
  expect_equal(sd_custom$M, 75L)
})

test_that("check_tweedie_truncation requires a tweedie response", {
  # A model is read for every response's family, so a tweedie response
  # of a multivariate model qualifies where the family given beside
  # its formula does not.
  set.seed(8)
  d <- data.frame(time = 1:30, series = factor("a"),
                  y = rpois(30, 4), pos = rgamma(30, 2, 1))
  pf <- mvgam(y ~ 1, data = d, family = poisson(), run_model = FALSE,
              validate = FALSE)
  expect_error(
    check_tweedie_truncation(pf),
    "needs a response with family tweedie()",
    fixed = TRUE
  )
  mv <- mvgam(
    bf(y ~ 1, family = poisson()) + bf(pos ~ 1, family = tweedie()) +
      set_rescor(FALSE),
    data = d, run_model = FALSE, validate = FALSE
  )
  expect_error(
    check_tweedie_truncation(mv, resp = "y"),
    "needs a response with family tweedie()",
    fixed = TRUE
  )
})

test_that("tweedie() supports brms distributional regression on mphi", {
  # `mphi ~ covs` routes a vector mphi into the lpdf's per-element
  # path
  set.seed(1)
  dat <- data.frame(
    y    = c(rep(0, 10), rgamma(40, 2, 0.5)),
    site = factor(rep(c("s1", "s2"), each = 25)),
    time = 1:50,
    series = factor(rep("s1", 50))
  )
  mf <- mvgam_formula(brms::bf(y ~ 1, mphi ~ site))
  sc <- paste(unlist(stancode(mf, data = dat, family = tweedie(),
                              validate = FALSE)),
              collapse = "\n")
  # The dispersion predictor is on the log scale. The program
  # exponentiates it before the likelihood. On an identity link the
  # positivity check in `tweedie_lpdf` rejected every negative value.
  expect_true(grepl("mphi += Intercept_mphi + Xc_mphi * b_mphi;", sc,
                    fixed = TRUE))
  expect_true(grepl("mphi = exp(mphi);", sc, fixed = TRUE))
  expect_true(grepl("target += tweedie_lpdf(Y | mu, mphi, mtheta, M);",
                    sc, fixed = TRUE))
  # brms leaves a custom family's dpar intercept flat. mvgam gives it
  # the intercept default brms uses for its own families.
  expect_true(grepl("lprior += student_t_lpdf(Intercept_mphi | 3, 0, 2.5);",
                    sc, fixed = TRUE))
  # A user prior on that intercept replaces the default.
  own <- paste(unlist(stancode(
    mf, data = dat, family = tweedie(),
    prior = brms::prior(normal(0, 1), class = Intercept, dpar = mphi),
    validate = FALSE
  )), collapse = "\n")
  expect_true(grepl("lprior += normal_lpdf(Intercept_mphi | 0, 1);", own,
                    fixed = TRUE))
  expect_false(grepl("student_t_lpdf(Intercept_mphi", own, fixed = TRUE))

  # The power parameter must stay in (1, 2). An identity link lets a
  # linear predictor leave that range. A sub-formula on it is refused.
  expect_error(
    stancode(mvgam_formula(brms::bf(y ~ 1, mtheta ~ site)), data = dat,
             family = tweedie(), validate = FALSE),
    "'mtheta' must stay in [1.001, 1.999]", fixed = TRUE
  )
})
