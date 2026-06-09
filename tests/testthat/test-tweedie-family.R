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
  fam <- tweedie()
  user_sv <- brms::stanvar(scode = "// user", block = "functions")
  merged <- mvgam:::attach_family_stanvars(user_sv, fam)
  expect_s3_class(merged, "stanvars")
  # User stanvar + tweedie's function block + M data = 3 entries.
  expect_gte(length(merged), 3L)
  scodes <- vapply(merged, function(sv) sv$scode, character(1))
  expect_true(any(grepl("// user", scodes)))
  expect_true(any(grepl("tweedie_lpdf", scodes)))
  expect_true(any(grepl("int<lower=1> M", scodes)))
})

test_that("attach_family_stanvars passes through unchanged for built-in families", {
  fam_gauss <- stats::gaussian()
  expect_null(mvgam:::attach_family_stanvars(NULL, fam_gauss))
  user_sv <- brms::stanvar(scode = "// user", block = "functions")
  expect_identical(
    mvgam:::attach_family_stanvars(user_sv, fam_gauss),
    user_sv
  )
})

test_that("check_tweedie_truncation requires a tweedie fit", {
  # Stub a non-Tweedie object so we get the expected guardrail.
  fake <- list(family = stats::gaussian())
  class(fake) <- "mvgam"
  expect_error(
    check_tweedie_truncation(fake),
    "tweedie\\(\\)"
  )
})

test_that("how_to_cite reference_db includes Tweedie + AIMS entries", {
  db <- mvgam:::reference_db()
  expect_true("jorgensen_tweedie" %in% names(db))
  expect_true("dunn_smyth_tweedie" %in% names(db))
  expect_true("aims_tweedie_brms" %in% names(db))
  # AIMS entry must surface the github URL so users can find the
  # upstream implementation.
  expect_true(grepl("github.com/open-AIMS/tweedie",
                    db$aims_tweedie_brms$text))
  # Canonical Tweedie reference (Jorgensen 1987) DOI present.
  expect_true(grepl("10\\.1111/j\\.2517-6161",
                    db$jorgensen_tweedie$text))
  # Series-truncation reference (Dunn & Smyth 2005) DOI present.
  expect_true(grepl("10\\.1007/s11222-005-4070-y",
                    db$dunn_smyth_tweedie$text))
})

test_that("uses_tweedie_family predicate distinguishes the family", {
  fake_tw <- list(family = tweedie())
  class(fake_tw) <- "mvgam"
  fake_po <- list(family = stats::poisson())
  class(fake_po) <- "mvgam"
  expect_true(mvgam:::uses_tweedie_family(fake_tw))
  expect_false(mvgam:::uses_tweedie_family(fake_po))
  # NULL-family safety: a malformed object should return FALSE,
  # not error.
  expect_false(mvgam:::uses_tweedie_family(list()))
})


test_that("tweedie_lpdf has overloaded scalar / vector dpar signatures", {
  # The Stan function block must declare both the all-scalar
  # and all-vector entry points (plus mixed) so brms's
  # distributional regression on mphi / mtheta flows through
  # without a typecheck error. Was discovered via smoke test
  # 2026-06-09 (mvgam(bf(y ~ x, mphi ~ site), family = tweedie())
  # used to fail at Stan compile).
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
