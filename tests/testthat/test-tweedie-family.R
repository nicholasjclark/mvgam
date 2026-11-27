
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
  # A Frechet draw has the fitted mean. Its sampler once called
  # arguments its source package does not take. Every draw failed.
  set.seed(4L)
  draws <- sample_from_family("frechet", ndraws = 20000L,
                              epred = matrix(2, 20000L, 1L),
                              shape = matrix(6, 20000L, 1L))
  expect_equal(mean(draws), 2, tolerance = 0.02)
})
