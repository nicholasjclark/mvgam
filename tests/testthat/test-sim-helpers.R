# CI tests for the simulation helpers in `R/sim_helpers.R`. Each
# helper covers one building block sim_mvgam composes.


# ---- sim_smooth ----------------------------------------------------

test_that("sim_smooth evaluates its own basis at its own coefficients", {
  set.seed(11L)
  out <- sim_smooth(
    seq(-2, 2, length.out = 60L), k = 8L, bs = "tp", scale = 0.5
  )
  expect_named(out, c("f", "basis", "coefs"))
  expect_length(out$coefs, ncol(out$basis$X))
  # All three pieces are returned and `f` is the product of the other
  # two. Measuring the length of `f` echoed the 60 x values supplied.
  expect_equal(out$f, as.numeric(out$basis$X %*% out$coefs))
})


test_that("sim_smooth accepts cyclic basis 'cc'", {
  set.seed(11L)
  out <- sim_smooth(
    seq(1, 12, length.out = 24L), k = 6L, bs = "cc", scale = 0.5
  )
  expect_length(out$f, 24L)
  # A cyclic basis closes on itself: the two ends of one period meet.
  # The thin-plate basis over the same points parts by 0.24 there,
  # which is what the length check could not separate.
  expect_equal(out$f[1L], out$f[24L], tolerance = 1e-8)
})


test_that("sim_smooth errors on invalid inputs", {
  expect_error(sim_smooth(1, k = 8L), "length")
  expect_error(
    sim_smooth(seq(-2, 2, length.out = 50L), k = 2L),
    "not >= 3"
  )
})


# ---- sim_family_rng ------------------------------------------------

test_that("sim_family_rng draws through the links a fit predicts on", {
  # The link switch here held five entries while `inv_link()` applies
  # seventeen. A probit, cloglog or cauchit model predicted and could
  # not be simulated. The draws are checked as Bernoulli values: a
  # working inverse link and one returning the linear predictor
  # untouched both give a vector of the right length.
  set.seed(3L)
  eta <- rnorm(20L)
  for (lk in c("logit", "probit", "cloglog", "cauchit")) {
    y <- sim_family_rng(eta, stats::binomial(link = lk))
    expect_length(y, 20L)
    expect_true(all(y %in% c(0L, 1L)))
  }
})


test_that("a positive-mean family refuses a link that turns it negative", {
  # `rgamma()` returns NaN for a negative rate and raises a warning.
  # A caller then took NAs for simulated data. The refusal names the
  # family, the link and how many values fell out.
  eta <- c(-1, 1, 2)
  err <- expect_error(
    sim_family_rng(eta, stats::Gamma(link = "inverse")),
    "has to be positive"
  )
  expect_match(conditionMessage(err), "inverse", fixed = TRUE)

  # The same family draws cleanly where the link keeps the mean above
  # zero. The guard turns away the pairing and leaves the family
  # usable.
  y <- sim_family_rng(eta, stats::Gamma(link = "log"))
  expect_length(y, 3L)
  expect_false(anyNA(y))
  expect_true(all(y > 0))
})


# ---- sim_gp_cov ----------------------------------------------------

test_that("sim_gp_cov uses the length-scale its arguments set", {
  x <- seq(0, 1, length.out = 30L)
  set.seed(11L)
  out <- sim_gp_cov(x, alpha = 1, rho = 0.3)
  expect_length(out, 30L)
  expect_true(all(is.finite(out)))
  # The documented default length-scale is a quarter of the x range.
  # Every length-scale returns one value per x, which left the
  # default itself unchecked.
  set.seed(7L)
  default <- sim_gp_cov(x, alpha = 1)
  set.seed(7L)
  expect_identical(
    default, sim_gp_cov(x, alpha = 1, rho = diff(range(x)) / 4)
  )
  set.seed(7L)
  expect_false(isTRUE(all.equal(
    default, sim_gp_cov(x, alpha = 1, rho = 20)
  )))
})


# ---- sim_re --------------------------------------------------------

test_that("sim_re gives each observation its own group's coefficient", {
  set.seed(11L)
  grp <- rep(letters[1:5], each = 8L)
  out <- sim_re(grp, sigma = 0.5)
  expect_named(out, c("levels", "coefs", "values"))
  expect_identical(names(out$coefs), out$levels)
  # The per-observation values are the coefficients looked up by
  # group. Their length accepted any 40 numbers, a copy taken in row
  # order included.
  expect_equal(unname(out$values), unname(out$coefs[grp]))
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


test_that("sim_family_rng poisson draws at the rate the predictor names", {
  set.seed(11L)
  y <- sim_family_rng(
    rep(log(5), 200L), family = poisson()
  )
  expect_true(all(y >= 0L))
  expect_true(all(y == round(y)))
  # The log link puts the mean at 5. Non-negative integers alone
  # passed for any rate, the link applied or skipped.
  expect_lt(abs(mean(y) - 5), 0.6)
})


test_that("sim_family_rng binomial respects trial size", {
  set.seed(11L)
  y <- sim_family_rng(
    rep(0, 200L), family = binomial(),
    pars = list(trials = 10L)
  )
  expect_true(all(y >= 0L))
  expect_true(all(y <= 10L))
  # At eta = 0 the logit link gives p = 0.5. Ten trials then centre
  # the draws at 5. The bounds alone passed a sampler dropping
  # `trials` and drawing one Bernoulli per row.
  expect_lt(abs(mean(y) - 5), 0.5)
  expect_gt(max(y), 1L)
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
  # logit(0) is a mean of 0.5, and phi = 10 puts the spread at
  # sqrt(mu (1 - mu) / (1 + phi)) = 0.151. The open interval admitted
  # any beta at all, one ignoring `phi` included.
  expect_lt(abs(mean(y) - 0.5), 0.05)
  expect_lt(abs(stats::sd(y) - sqrt(0.25 / 11)), 0.03)
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
  # NB2 keeps the mean at the inverse link and puts the extra
  # dispersion in the variance.
  expect_lt(abs(mean(y) - 5), 1)
})


test_that("sim_family_rng Gamma returns positive values", {
  set.seed(11L)
  y <- sim_family_rng(
    rep(log(2), 200L), family = Gamma(link = "log"),
    pars = list(shape = 2)
  )
  expect_true(all(y > 0))
  # The rate is computed from the shape and the mean, which keeps the
  # mean at the inverse link. Positivity held for any gamma.
  expect_lt(abs(mean(y) - 2), 0.4)
})


test_that("sim_family_rng errors on unsupported family", {
  bad <- structure(
    list(family = "lognormal", link = "identity"),
    class = "family"
  )
  expect_error(
    sim_family_rng(rep(0, 5L), family = bad, pars = list()),
    "Unsupported family"
  )
})


# ---- sim_covariate -------------------------------------------------

test_that("sim_covariate builds each type to its own shape", {
  set.seed(11L)
  n <- 50L
  # Every type returns n values. The length alone identifies none of
  # them. Each is pinned by the property defining it.
  expect_length(sim_covariate(n, type = "normal"), n)
  unif <- sim_covariate(n, type = "uniform")
  expect_true(all(unif >= -2 & unif <= 2))
  sq <- sim_covariate(n, type = "seq")
  expect_true(all(diff(sq) > 0))
  expect_equal(range(sq), c(-2, 2))
  cyc <- sim_covariate(n, type = "cyclic")
  expect_identical(cyc[1:12], cyc[13:24])
})


# ---- sim_grp -------------------------------------------------------

test_that("sim_grp builds a balanced factor", {
  out <- sim_grp(20L, n_levels = 4L)
  expect_s3_class(out, "factor")
  expect_length(out, 20L)
  expect_identical(nlevels(out), 4L)
  # Balance is the word in the name, and the one property the level
  # count could not separate from a factor putting 17 rows on one
  # level and one row on each of the rest.
  expect_identical(unname(as.integer(table(out))), rep(5L, 4L))
})


# ---- stationary_VAR_phi --------------------------------------------

test_that("stationary_VAR_phi draws stationary coefficients", {
  set.seed(11L)
  phi <- stationary_VAR_phi(p = 2L, n_series = 3L)
  expect_identical(dim(phi), c(3L, 3L, 2L))
  expect_true(all(is.finite(phi)))
  # Stationarity is the property the helper is named for: every
  # eigenvalue of the VAR(p) companion matrix falls inside the unit
  # circle. Shape and finiteness pass for any draw at all.
  companion <- rbind(
    cbind(phi[, , 1L], phi[, , 2L]),
    cbind(diag(3L), matrix(0, 3L, 3L))
  )
  expect_lt(max(Mod(eigen(companion, only.values = TRUE)$values)), 1)
  # One lag is its own companion matrix.
  set.seed(11L)
  phi1 <- stationary_VAR_phi(p = 1L, n_series = 3L)
  expect_lt(
    max(Mod(eigen(phi1[, , 1L], only.values = TRUE)$values)), 1
  )
})


# ---- gam_test_fX helpers ------------------------------------------

test_that("gam_test_f0..f3 give three signals and one null", {
  xs <- seq(0, 1, length.out = 50L)
  fs <- lapply(
    list(gam_test_f0, gam_test_f1, gam_test_f2, gam_test_f3),
    function(fn) fn(xs)
  )
  for (f in fs) {
    expect_length(f, 50L)
    expect_true(all(is.finite(f)))
  }
  # f3 is the null function a simulation gives a covariate with no
  # effect. The other three have to vary, or a recovery check against
  # them would pass on a flat fit.
  expect_identical(fs[[4L]], rep(0, 50L))
  for (i in 1:3) {
    expect_gt(stats::sd(fs[[i]]), 0.1)
  }
})


# A family reaches `sim_mvgam()` through four separate switches: an
# intercept, a link-scale budget, a parameter list and the sampler
# that finally draws it. Nothing ties them together. A family added
# to the first three and forgotten in the fourth is accepted all the
# way to the draw and then refused, which is how
# `sim_mvgam(family = bernoulli())` came to error.
switch_labels <- function(fn) {
  found <- character(0)
  walk <- function(e) {
    if (!is.call(e)) return(invisible(NULL))
    parts <- as.list(e)
    if (identical(parts[[1L]], as.name("switch"))) {
      nms <- names(parts)[-(1:2)]
      found <<- c(found, nms[!is.na(nms) & nzchar(nms)])
    }
    for (p in parts) walk(p)
    invisible(NULL)
  }
  walk(body(fn))
  unique(found)
}


test_that("every family the simulator parameterises can be drawn from", {
  ns <- asNamespace("mvgam")
  # The three tables that describe a family, against the one that
  # emits it.
  described <- unique(unlist(lapply(
    c("intercept_for_family", "link_scale_budget", "sim_family_pars"),
    function(f) switch_labels(get(f, envir = ns))
  )))
  drawable <- switch_labels(get("sim_family_rng", envir = ns))

  # An empty set on either side would satisfy the comparison while
  # comparing nothing.
  expect_gt(length(described), 8L)
  expect_gt(length(drawable), 8L)
  expect_identical(setdiff(described, drawable), character(0))
})


test_that("a bernoulli response simulates as a one-trial binomial", {
  set.seed(5L)
  eta <- stats::qlogis(rep(c(0.2, 0.8), each = 500L))
  y <- mvgam:::sim_family_rng(eta, bernoulli())
  expect_length(y, length(eta))
  expect_true(all(y %in% c(0L, 1L)))
  # The draws follow the probability the predictor names. A sampler
  # ignoring `eta` fails here, where a shape check would pass it.
  expect_lt(abs(mean(y[1:500]) - 0.2), 0.06)
  expect_lt(abs(mean(y[501:1000]) - 0.8), 0.06)
  # And a trial count offered by mistake cannot turn it binomial.
  y2 <- mvgam:::sim_family_rng(eta, bernoulli(), list(trials = 10L))
  expect_true(all(y2 %in% c(0L, 1L)))
})
