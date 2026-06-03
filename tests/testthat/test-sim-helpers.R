# CI tests for the simulation helpers in `R/sim_helpers.R`. Each
# helper covers one building block sim_mvgam composes.


# ---- sim_smooth ----------------------------------------------------

test_that("sim_smooth returns shape-matched output", {
  set.seed(11L)
  out <- sim_smooth(
    seq(-2, 2, length.out = 60L), k = 8L, bs = "tp", scale = 0.5
  )
  expect_named(out, c("f", "basis", "coefs"))
  expect_length(out$f, 60L)
  expect_true(all(is.finite(out$f)))
  expect_length(out$coefs, ncol(out$basis$X))
})


test_that("sim_smooth accepts cyclic basis 'cc'", {
  set.seed(11L)
  out <- sim_smooth(
    seq(1, 12, length.out = 24L), k = 6L, bs = "cc", scale = 0.5
  )
  expect_length(out$f, 24L)
})


test_that("sim_smooth errors on invalid inputs", {
  expect_error(sim_smooth(1, k = 8L), "length")
  expect_error(
    sim_smooth(seq(-2, 2, length.out = 50L), k = 2L),
    "not >= 3"
  )
})


# ---- sim_gp_cov ----------------------------------------------------

test_that("sim_gp_cov returns a finite numeric vector", {
  set.seed(11L)
  out <- sim_gp_cov(
    seq(0, 1, length.out = 30L), alpha = 1, rho = 0.3
  )
  expect_length(out, 30L)
  expect_true(all(is.finite(out)))
})


test_that("sim_gp_cov defaults rho when NULL", {
  set.seed(11L)
  out <- sim_gp_cov(seq(0, 1, length.out = 20L), alpha = 1)
  expect_length(out, 20L)
})


# ---- sim_re --------------------------------------------------------

test_that("sim_re returns one coef per level and per-obs values", {
  set.seed(11L)
  grp <- rep(letters[1:5], each = 8L)
  out <- sim_re(grp, sigma = 0.5)
  expect_named(out, c("levels", "coefs", "values"))
  expect_length(out$levels, 5L)
  expect_length(out$coefs, 5L)
  expect_length(out$values, 40L)
  expect_identical(names(out$coefs), out$levels)
})


# ---- beta_shapes ---------------------------------------------------

test_that("beta_shapes recovers (mu, phi) parametrisation", {
  shp <- beta_shapes(0.3, 10)
  expect_equal(shp$shape1, 3)
  expect_equal(shp$shape2, 7)
  # Mean of Beta(a, b) is a/(a+b); verify it returns the mu we put in.
  expect_equal(shp$shape1 / (shp$shape1 + shp$shape2), 0.3)
})


# ---- sim_family_rng ------------------------------------------------

test_that("sim_family_rng gaussian centres on inverse link", {
  set.seed(11L)
  y <- sim_family_rng(
    rep(2, 1000L), family = gaussian(), pars = list(sigma = 0.5)
  )
  expect_length(y, 1000L)
  expect_lt(abs(mean(y) - 2), 0.1)
})


test_that("sim_family_rng poisson returns non-negative integers", {
  set.seed(11L)
  y <- sim_family_rng(
    rep(log(5), 200L), family = poisson()
  )
  expect_true(all(y >= 0L))
  expect_true(all(y == round(y)))
})


test_that("sim_family_rng binomial respects trial size", {
  set.seed(11L)
  y <- sim_family_rng(
    rep(0, 200L), family = binomial(),
    pars = list(trials = 10L)
  )
  expect_true(all(y >= 0L))
  expect_true(all(y <= 10L))
})


test_that("sim_family_rng beta returns values in (0, 1)", {
  set.seed(11L)
  y <- sim_family_rng(
    rep(0, 200L),
    family = brms::brmsfamily("beta"),
    pars = list(phi = 10)
  )
  expect_true(all(y > 0))
  expect_true(all(y < 1))
})


test_that("sim_family_rng negbinomial returns non-negative integers", {
  set.seed(11L)
  y <- sim_family_rng(
    rep(log(5), 200L),
    family = brms::brmsfamily("negbinomial"),
    pars = list(size = 5)
  )
  expect_true(all(y >= 0L))
  expect_true(all(y == round(y)))
})


test_that("sim_family_rng Gamma returns positive values", {
  set.seed(11L)
  y <- sim_family_rng(
    rep(log(2), 200L), family = Gamma(link = "log"),
    pars = list(shape = 2)
  )
  expect_true(all(y > 0))
})


test_that("sim_family_rng errors on unsupported family", {
  expect_error(
    sim_family_rng(rep(0, 5L),
                    family = brms::brmsfamily("bernoulli"),
                    pars = list()),
    "Unsupported family"
  )
})


# ---- sim_covariate -------------------------------------------------

test_that("sim_covariate returns length-n vectors for each type", {
  set.seed(11L)
  for (type in c("uniform", "normal", "seq", "cyclic")) {
    expect_length(sim_covariate(50L, type = type), 50L)
  }
})


# ---- sim_grp -------------------------------------------------------

test_that("sim_grp builds a balanced factor", {
  out <- sim_grp(20L, n_levels = 4L)
  expect_s3_class(out, "factor")
  expect_length(out, 20L)
  expect_identical(nlevels(out), 4L)
})


# ---- stationary_VAR_phi --------------------------------------------

test_that("stationary_VAR_phi returns cube of correct shape", {
  set.seed(11L)
  phi <- stationary_VAR_phi(p = 2L, n_series = 3L)
  expect_identical(dim(phi), c(3L, 3L, 2L))
  expect_true(all(is.finite(phi)))
})


# ---- gam_test_fX helpers ------------------------------------------

test_that("gam_test_f0..f3 return finite numeric vectors", {
  xs <- seq(0, 1, length.out = 50L)
  for (fn in list(gam_test_f0, gam_test_f1, gam_test_f2,
                   gam_test_f3)) {
    out <- fn(xs)
    expect_length(out, 50L)
    expect_true(all(is.finite(out)))
  }
})
