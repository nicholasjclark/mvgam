# CI tests for the simulation helpers in `R/sim_helpers.R`. Each
# helper covers one building block sim_mvgam composes.


# ---- sim_smooth ----------------------------------------------------

test_that("sim_smooth evaluates its own basis at its own coefficients", {
  set.seed(11L)
  out <- sim_smooth(
    seq(-2, 2, length.out = 60L), k = 8L, bs = "tp", scale = 0.5
  )
  expect_equal(out$f, as.numeric(out$basis$X %*% out$coefs))
})


test_that("sim_smooth accepts cyclic basis 'cc'", {
  set.seed(11L)
  out <- sim_smooth(
    seq(1, 12, length.out = 24L), k = 6L, bs = "cc", scale = 0.5
  )
  # A cyclic basis closes on itself: the two ends of one period meet.
  # The thin-plate basis over the same points parts by 0.24 there.
  expect_equal(out$f[1L], out$f[24L], tolerance = 1e-8)
})


# ---- sim_family_rng ------------------------------------------------

test_that("sim_family_rng draws through the links a fit predicts on", {
  # Each inverse link maps the predictor to a probability
  set.seed(3L)
  eta <- rnorm(20L)
  for (lk in c("logit", "probit", "cloglog", "cauchit")) {
    y <- sim_family_rng(eta, stats::binomial(link = lk))
    expect_true(all(y %in% c(0L, 1L)))
  }
})


test_that("a positive-mean family refuses a link that turns it negative", {
  # `rgamma()` returns NaN for a negative rate. The refusal names the
  # link.
  eta <- c(-1, 1, 2)
  err <- expect_error(
    sim_family_rng(eta, stats::Gamma(link = "inverse")),
    "has to be positive"
  )
  expect_match(conditionMessage(err), "inverse", fixed = TRUE)

  # The log link keeps the mean above zero
  y <- sim_family_rng(eta, stats::Gamma(link = "log"))
  expect_true(all(y > 0))
})


# ---- sim_gp_cov ----------------------------------------------------

test_that("sim_gp_cov uses the length-scale its arguments set", {
  x <- seq(0, 1, length.out = 30L)
  set.seed(11L)
  # The default length-scale is a quarter of the x range
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
  expect_identical(names(out$coefs), out$levels)
  # The per-observation values are the coefficients looked up by group
  expect_equal(unname(out$values), unname(out$coefs[grp]))
})


# ---- beta_shapes ---------------------------------------------------

test_that("beta_shapes recovers (mu, phi) parametrisation", {
  shp <- beta_shapes(0.3, 10)
  expect_equal(shp$shape1, 3)
  expect_equal(shp$shape2, 7)
})


# ---- sim_family_rng ------------------------------------------------

test_that("sim_family_rng gaussian centres on inverse link", {
  set.seed(11L)
  y <- sim_family_rng(
    rep(2, 1000L), family = gaussian(), pars = list(sigma = 0.5)
  )
  expect_lt(abs(mean(y) - 2), 0.1)
})


test_that("sim_family_rng poisson draws at the rate the predictor names", {
  set.seed(11L)
  y <- sim_family_rng(
    rep(log(5), 200L), family = poisson()
  )
  # The log link puts the mean at 5
  expect_lt(abs(mean(y) - 5), 0.6)
})


test_that("sim_family_rng binomial respects trial size", {
  set.seed(11L)
  y <- sim_family_rng(
    rep(0, 200L), family = binomial(),
    pars = list(trials = 10L)
  )
  expect_true(all(y <= 10L))
  # At eta = 0 the logit link gives p = 0.5. Ten trials then centre
  # the draws at 5.
  expect_lt(abs(mean(y) - 5), 0.5)
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
  # sqrt(mu (1 - mu) / (1 + phi)) = 0.151
  expect_lt(abs(mean(y) - 0.5), 0.05)
  expect_lt(abs(stats::sd(y) - sqrt(0.25 / 11)), 0.03)
})


test_that("sim_family_rng negbinomial keeps the mean at the link", {
  set.seed(11L)
  y <- sim_family_rng(
    rep(log(5), 200L),
    family = brms::brmsfamily("negbinomial"),
    pars = list(size = 5)
  )
  # NB2 keeps the mean at the inverse link and puts the extra
  # dispersion in the variance
  expect_lt(abs(mean(y) - 5), 1)
  expect_gt(stats::var(y), 5)
})


test_that("sim_family_rng Gamma returns positive values", {
  set.seed(11L)
  y <- sim_family_rng(
    rep(log(2), 200L), family = Gamma(link = "log"),
    pars = list(shape = 2)
  )
  expect_true(all(y > 0))
  # The rate is computed from the shape and the mean
  expect_lt(abs(mean(y) - 2), 0.4)
})


test_that("sim_family_rng errors on unsupported family", {
  bad <- structure(
    list(family = "lognormal", link = "identity"),
    class = "family"
  )
  expect_error(
    sim_family_rng(rep(0, 5L), family = bad, pars = list()),
    "is not supported"
  )
})


# ---- sim_grp -------------------------------------------------------

test_that("sim_grp builds a balanced factor", {
  out <- sim_grp(20L, n_levels = 4L)
  expect_identical(unname(as.integer(table(out))), rep(5L, 4L))
})


# A family reaches `sim_mvgam()` through four switches: an intercept, a
# link-scale budget, a parameter list and the sampler that draws it. A
# family in the first three must also be in the fourth.
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
  y <- sim_family_rng(eta, bernoulli())
  # The draws follow the probability the predictor names
  expect_lt(abs(mean(y[1:500]) - 0.2), 0.06)
  expect_lt(abs(mean(y[501:1000]) - 0.8), 0.06)
  # A trial count offered by mistake leaves it Bernoulli
  y2 <- sim_family_rng(eta, bernoulli(), list(trials = 10L))
  expect_true(all(y2 %in% c(0L, 1L)))
})
