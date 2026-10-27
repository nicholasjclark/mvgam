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
  sd_default <- standata(mf, data = dat, family = tweedie(), validate = FALSE)
  sd_custom <- standata(mf, data = dat, family = tweedie(M = 75L),
                        validate = FALSE)
  expect_equal(sd_default$M, 30L)
  expect_equal(sd_custom$M, 75L)
})

test_that("compute_family_epred returns mu directly for tweedie", {
  # E[Y | mu, phi, theta] = mu under the compound Poisson-gamma
  # for all theta in (1, 2); no Jensen correction needed.
  fam <- tweedie()
  set.seed(1L)
  linpred <- matrix(rnorm(20, mean = 0.5, sd = 0.2), nrow = 4)
  epred <- compute_family_epred(linpred = linpred, family = fam)
  expect_equal(epred, exp(linpred), tolerance = 1e-12)
})

test_that("the tweedie log density matches mgcv::ldTweedie row-wise", {
  # Per-observation log density must equal mgcv's reference
  # implementation column by column.
  set.seed(1L)
  ndraws <- 3L
  nobs <- 5L
  y <- c(0, 0.5, 1.2, 0, 2.7)
  mu <- matrix(runif(ndraws * nobs, 0.5, 3), nrow = ndraws)
  linpred <- log(mu)
  mphi <- matrix(runif(ndraws * nobs, 0.3, 0.8), nrow = ndraws)
  mtheta <- matrix(runif(ndraws * nobs, 1.3, 1.7), nrow = ndraws)
  ll <- dispatch_log_lik(
    "tweedie", linpred = linpred,
    link = "log",
    y = y,
    family_pars = list(mphi = mphi, mtheta = mtheta),
    trials = NULL
  )
  for (j in seq_len(nobs)) {
    # Broadcast scalar y to length ndraws so ldTweedie returns
    # one row per draw (it vectorises over y, not mu).
    expected <- mgcv::ldTweedie(
      y = rep(y[j], ndraws),
      mu = mu[, j], p = mtheta[, j], phi = mphi[, j]
    )[, 1L]
    expect_equal(ll[, j], expected, tolerance = 1e-12)
  }
})

test_that("attach_family_stanvars merges tweedie stanvars with user stanvars", {
  user_sv <- brms::stanvar(scode = "// user", block = "functions")
  merged <- attach_family_stanvars(user_sv, list(y = tweedie()))
  # The user's stanvar, tweedie's function block and its `M` data.
  expect_length(merged, 3L)
  scodes <- vapply(merged, function(sv) sv$scode, character(1))
  expect_true(any(grepl("// user", scodes)))
  expect_true(any(grepl("tweedie_lpdf", scodes)))
  expect_true(any(grepl("int<lower=1> M", scodes)))
})

test_that("attach_family_stanvars passes through unchanged for built-in families", {
  fam_gauss <- list(y = stats::gaussian())
  expect_null(attach_family_stanvars(NULL, fam_gauss))
  user_sv <- brms::stanvar(scode = "// user", block = "functions")
  expect_identical(
    attach_family_stanvars(user_sv, fam_gauss),
    user_sv
  )
})

test_that("two responses sharing a family declare its Stan code once", {
  # A custom family named by one response of a multivariate formula
  # needs its functions as much as one given beside the formula, and
  # two responses sharing it share one declaration. Given different
  # arguments they would declare `M` twice.
  shared <- attach_family_stanvars(
    NULL, list(a = tweedie(), b = poisson(), c = tweedie())
  )
  expect_length(shared, 2L)
  expect_error(
    attach_family_stanvars(
      NULL, list(a = tweedie(M = 30L), b = tweedie(M = 40L))
    ),
    "Two responses give 'tweedie()' different arguments",
    fixed = TRUE
  )
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

test_that("tweedie_lpdf has overloaded scalar / vector dpar signatures", {
  # brms passes mphi and mtheta as vectors when each has a formula and
  # as reals otherwise. Each of the four mixes needs an entry point.
  scode <- gsub("\\s+", " ", tweedie_stan_funs())
  for (mphi in c("real", "vector")) {
    for (mtheta in c("real", "vector")) {
      expect_true(grepl(
        paste0(
          "real tweedie_lpdf\\( ?vector y, vector mu, ", mphi,
          " mphi, ", mtheta, " mtheta, int M\\)"
        ),
        scode
      ))
    }
  }
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

# ---- custom-family dpar resolution in the forecast path -------------

test_that("custom families resolve to their constructor name, not 'custom'", {
  # `forecast()` takes the dpars of the constructor name. The stored
  # family field "custom" names none.
  fam <- tweedie()
  expect_identical(
    get_family_dpars(resolve_family_name(fam)),
    c("mphi", "mtheta")
  )
})

test_that("family() names a custom family by its own name", {
  # brms writes the placeholder "custom" into a custom family, and
  # `family()` is the accessor other packages call, so it has to
  # answer with the name mvgam records instead.
  stub <- structure(
    list(family = tweedie(), formula = y ~ x), class = "mvgam"
  )
  expect_identical(family(stub)$family, "tweedie")

  # Built-in families are unaffected
  gauss <- structure(
    list(family = gaussian(), formula = y ~ x), class = "mvgam"
  )
  expect_identical(family(gauss)$family, "gaussian")
})

test_that("sample_from_family() accepts every dpar the registry can emit for it", {
  # The forecast path splices the registry's dpar list into
  # sample_from_family() rather than naming parameters one by one, so a
  # family whose parameters have no matching argument would error at
  # forecast time instead of at test time.
  formals_avail <- names(formals(sample_from_family))
  for (fam_name in c("tweedie", "com_binomial", "gaussian", "negbinomial",
                     "beta", "student")) {
    dpars <- get_family_dpars(fam_name)
    expect_true(all(dpars %in% formals_avail))
  }
})
