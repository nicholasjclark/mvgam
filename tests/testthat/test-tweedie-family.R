# Tests for the Tweedie custom family.
#
# Covers constructor surface, stanvar auto-injection (functions
# block + M data stanvar), parameter-name registration in
# get_family_dpars, the R-side log_lik / posterior_epred /
# posterior_predict branches, and the link / bound guards.
# End-to-end fit + recovery lives under tests/local/ so CI stays
# fast.

test_that("tweedie() returns a customfamily with mu/mphi/mtheta dpars", {
  fam <- tweedie()
  expect_s3_class(fam, "customfamily")
  expect_s3_class(fam, "brmsfamily")
  expect_identical(fam$name, "tweedie")
  expect_identical(fam$dpars, c("mu", "mphi", "mtheta"))
  expect_identical(fam$link, "log")
  expect_identical(fam$link_mphi, "identity")
  expect_identical(fam$link_mtheta, "identity")
  # Strict positivity / open interval bounds (must avoid the
  # mtheta -> 1 and mtheta -> 2 boundary singularities).
  expect_identical(unname(unlist(fam$lb)), c(NA, "1e-08", "1.001"))
  expect_identical(unname(unlist(fam$ub)), c(NA, NA, "1.999"))
  expect_identical(fam$type, "real")
  expect_false(fam$loop)
  expect_identical(fam$vars, "M")
})

test_that("tweedie() M argument validates and flows through to the M stanvar", {
  expect_error(tweedie(M = 0), "not >= 1")
  expect_error(tweedie(M = -1L), "not >= 1")
  expect_error(tweedie(M = "30"), "integerish")
  fam_default <- tweedie()
  fam_custom <- tweedie(M = 50L)
  sv_default <- attr(fam_default, "mvgam_stanvars")
  sv_custom <- attr(fam_custom, "mvgam_stanvars")
  expect_s3_class(sv_default, "stanvars")
  expect_s3_class(sv_custom, "stanvars")
  expect_equal(sv_default$M$sdata, 30L)
  expect_equal(sv_custom$M$sdata, 50L)
})

test_that("mvgam(family = tweedie()) auto-injects the Stan function block + M data", {
  set.seed(1L)
  dat <- data.frame(
    y = c(rep(0, 30), rgamma(50, shape = 2, rate = 0.5)),
    x = rnorm(80),
    time = 1:80,
    series = factor(rep("s1", 80))
  )
  mf <- mvgam_formula(y ~ x)
  sc <- paste(unlist(stancode(mf, data = dat, family = tweedie())),
              collapse = "\n")
  # Function-block helpers.
  expect_true(grepl("tweedie_lpdf", sc))
  expect_true(grepl("check_tweedie", sc))
  expect_true(grepl("num_non_zero_fun", sc))
  expect_true(grepl("non_zero_index_fun", sc))
  expect_true(grepl("zero_index_fun", sc))
  expect_true(grepl("log_sum_exp", sc))
  # Strict positivity guards (the AIMS reference accepted mu/mphi
  # >= 0; mvgam tightens to strict > 0).
  # check_tweedie now operates per-element (vector dpars) so the
  # guard reads `mphi[n] <= 0`, `mu[n] <= 0`,
  # `mtheta[n] <= 1 || mtheta[n] >= 2`.
  expect_true(grepl("if \\(mphi\\[n\\] <= 0\\)", sc))
  expect_true(grepl("if \\(mu\\[n\\] <= 0\\)", sc))
  expect_true(grepl(
    "mtheta\\[n\\] <= 1 \\|\\| mtheta\\[n\\] >= 2", sc
  ))
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

test_that("get_family_dpars(\"tweedie\") returns mphi + mtheta", {
  # Without this, log_lik.mvgam cannot extract the posterior
  # dpar matrices that log_lik_tweedie expects (mphi, mtheta).
  expect_identical(
    mvgam:::get_family_dpars("tweedie"),
    c("mphi", "mtheta")
  )
})

test_that("compute_family_epred returns mu directly for tweedie", {
  # E[Y | mu, phi, theta] = mu under the compound Poisson-gamma
  # for all theta in (1, 2); no Jensen correction needed.
  fam <- tweedie()
  set.seed(1L)
  linpred <- matrix(rnorm(20, mean = 0.5, sd = 0.2), nrow = 4)
  epred <- mvgam:::compute_family_epred(linpred = linpred, family = fam)
  expect_equal(epred, exp(linpred), tolerance = 1e-12)
})

test_that("log_lik_tweedie returns finite log densities for zero + positive y", {
  fam <- tweedie()
  ndraws <- 5L
  nobs <- 4L
  y <- c(0, 0, 1.5, 3.2)
  linpred <- matrix(log(2), nrow = ndraws, ncol = nobs)
  mphi <- matrix(0.5, nrow = ndraws, ncol = nobs)
  mtheta <- matrix(1.5, nrow = ndraws, ncol = nobs)
  ll <- mvgam:::log_lik_tweedie(
    linpred = linpred,
    link = "log",
    y = y,
    family_pars = list(mphi = mphi, mtheta = mtheta),
    trials = NULL
  )
  expect_true(is.matrix(ll))
  expect_equal(dim(ll), c(ndraws, nobs))
  expect_true(all(is.finite(ll)))
  # Zero observations have non-trivial log probability; positive
  # observations have finite log density.
  expect_true(all(ll[, 1] < 0))
  expect_true(all(ll[, 3] < 0))
})

test_that("log_lik_tweedie matches mgcv::ldTweedie row-wise", {
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
  ll <- mvgam:::log_lik_tweedie(
    linpred = linpred,
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
  merged <- mvgam:::attach_family_stanvars(user_sv, list(y = tweedie()))
  expect_s3_class(merged, "stanvars")
  # The user's stanvar, tweedie's function block and its `M` data.
  expect_length(merged, 3L)
  scodes <- vapply(merged, function(sv) sv$scode, character(1))
  expect_true(any(grepl("// user", scodes)))
  expect_true(any(grepl("tweedie_lpdf", scodes)))
  expect_true(any(grepl("int<lower=1> M", scodes)))
})

test_that("attach_family_stanvars passes through unchanged for built-in families", {
  fam_gauss <- list(y = stats::gaussian())
  expect_null(mvgam:::attach_family_stanvars(NULL, fam_gauss))
  user_sv <- brms::stanvar(scode = "// user", block = "functions")
  expect_identical(
    mvgam:::attach_family_stanvars(user_sv, fam_gauss),
    user_sv
  )
})

test_that("two responses sharing a family declare its Stan code once", {
  # A custom family named by one response of a multivariate formula
  # needs its functions as much as one given beside the formula, and
  # two responses sharing it share one declaration. Given different
  # arguments they would declare `M` twice.
  shared <- mvgam:::attach_family_stanvars(
    NULL, list(a = tweedie(), b = poisson(), c = tweedie())
  )
  expect_length(shared, 2L)
  expect_error(
    mvgam:::attach_family_stanvars(
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
  pf <- mvgam(y ~ 1, data = d, family = poisson(), run_model = FALSE)
  expect_error(
    check_tweedie_truncation(pf),
    "needs a response with family tweedie()",
    fixed = TRUE
  )
  mv <- mvgam(
    bf(y ~ 1, family = poisson()) + bf(pos ~ 1, family = tweedie()) +
      set_rescor(FALSE),
    data = d, run_model = FALSE
  )
  expect_error(
    check_tweedie_truncation(mv, resp = "y"),
    "needs a response with family tweedie()",
    fixed = TRUE
  )
})

test_that("how_to_cite reference_db includes Tweedie entries", {
  db <- mvgam:::reference_db()
  expect_true("jorgensen_tweedie" %in% names(db))
  expect_true("dunn_smyth_tweedie" %in% names(db))
  # Canonical Tweedie reference (Jorgensen 1987) DOI present.
  expect_true(grepl("10\\.1111/j\\.2517-6161",
                    db$jorgensen_tweedie$text))
  # Series-truncation reference (Dunn & Smyth 2005) DOI present.
  expect_true(grepl("10\\.1007/s11222-005-4070-y",
                    db$dunn_smyth_tweedie$text))
})

test_that("tweedie_lpdf has overloaded scalar / vector dpar signatures", {
  # The Stan function block must declare both the all-scalar
  # and all-vector entry points (plus mixed) so brms's
  # distributional regression on mphi / mtheta flows through
  # without a typecheck error.
  scode <- mvgam:::tweedie_stan_funs()
  expect_true(grepl(
    "real tweedie_lpdf\\(vector y, vector mu, vector mphi,\\s*vector mtheta, int M\\)",
    scode
  ))
  expect_true(grepl(
    "real tweedie_lpdf\\(vector y, vector mu, real mphi,\\s*real mtheta, int M\\)",
    scode
  ))
  expect_true(grepl(
    "real tweedie_lpdf\\(vector y, vector mu, vector mphi,\\s*real mtheta, int M\\)",
    scode
  ))
  expect_true(grepl(
    "real tweedie_lpdf\\(vector y, vector mu, real mphi,\\s*vector mtheta, int M\\)",
    scode
  ))
})

test_that("tweedie() supports brms distributional regression on mphi", {
  # Confirms that the user can write `mphi ~ covs` and brms
  # routes a vector mphi into the lpdf via the per-element
  # path. Does NOT fit -- just checks stancode + standata.
  set.seed(1)
  dat <- data.frame(
    y    = c(rep(0, 10), rgamma(40, 2, 0.5)),
    site = factor(rep(c("s1", "s2"), each = 25)),
    time = 1:50,
    series = factor(rep("s1", 50))
  )
  mf <- mvgam_formula(brms::bf(y ~ 1, mphi ~ site))
  sc <- paste(unlist(stancode(mf, data = dat, family = tweedie())),
              collapse = "\n")
  expect_true(grepl("\\bb_mphi\\b", sc))
  expect_true(grepl("X_mphi", sc))
  expect_true(grepl(
    "mphi\\s*\\+=\\s*Intercept_mphi\\s*\\+\\s*Xc_mphi\\s*\\*\\s*b_mphi", sc
  ))
  expect_true(grepl("tweedie_lpdf", sc))
  sd <- standata(mf, data = dat, family = tweedie())
  expect_true(all(c("K_mphi", "X_mphi") %in% names(sd)))
})


# ---- custom-family dpar resolution in the forecast path -------------

test_that("custom families resolve to their constructor name, not 'custom'", {
  # brms records `family$family = "custom"` for every custom family, so
  # any dispatcher reading that field directly looks up a family that
  # has no registered distributional parameters. `forecast()` must
  # resolve the constructor name instead, or it would silently drop
  # mphi / mtheta and leave the Tweedie sampler with no dispersion or
  # power parameter.
  fam <- tweedie()
  expect_identical(fam$family, "custom")
  expect_length(mvgam:::get_family_dpars(tolower(fam$family)), 0L)

  expect_identical(mvgam:::resolve_family_name(fam), "tweedie")
  expect_identical(
    mvgam:::get_family_dpars(mvgam:::resolve_family_name(fam)),
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
  formals_avail <- names(formals(mvgam:::sample_from_family))
  for (fam_name in c("tweedie", "com_binomial", "gaussian", "negbinomial",
                     "beta", "student")) {
    dpars <- mvgam:::get_family_dpars(fam_name)
    expect_true(all(dpars %in% formals_avail))
  }
})
