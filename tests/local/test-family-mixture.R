# Hurdle and zero-inflated densities, each checked against its own
# closed form and against the other's.
#
# A mixture likelihood is where `log_lik()` goes wrong in ways a
# finiteness check cannot see. A hurdle density written with a
# zero-inflated branch returns a matrix of the right shape holding
# finite numbers throughout, and is a different distribution, so the
# claims here are against the closed forms rather than the shape.
#
# So the claim here is the density itself. The two families differ
# only in where a zero may come from. Under a hurdle, zeros come from
# the hurdle alone and the count part is truncated at zero:
#
#   y == 0 :  log(hu)
#   y  > 0 :  log(1 - hu) + dpois(y, mu, log) - log(1 - exp(-mu))
#
# Under zero inflation the count part keeps its own zeros, so they
# arrive from two places and nothing is truncated:
#
#   y == 0 :  log(zi + (1 - zi) * exp(-mu))
#   y  > 0 :  log(1 - zi) + dpois(y, mu, log)
#
# Both forms are finite, correctly shaped and plausibly scaled on
# either fit, so each is written out once and applied to both. A fit
# has to match its own form and fail the other, which is what makes
# the check discriminate rather than merely be satisfiable.
#
# The families separate again in what they predict. A hurdle mean is
# rescaled by the truncation, `(1 - hu) * mu / (1 - exp(-mu))`; a
# zero-inflated mean is only thinned, `(1 - zi) * mu`. And the
# probability of a zero is `hu` exactly under a hurdle, while under
# zero inflation it strictly exceeds `zi`, by however much the count
# part contributes. Both are checked from the draws.
#
# One rule makes any of this possible. `log_lik()` conditions on the
# fitted latent state by default and every other prediction method
# marginalises by default, so the two must be asked for the same
# surface before their outputs can be compared. Crossed, they differ
# by 2.4; matched, by zero. That asymmetry is deliberate and
# documented -- a marginal density is not a basis for an ELPD -- and
# it is pinned below because it is the trap anyone rebuilding a
# density from a linear predictor walks into.
#
# Three fits:
#   1. hurdle_poisson, scalar `hu` on the probability scale
#   2. hurdle_poisson, `hu ~ z` on a logit link, one hurdle per row
#   3. zero_inflated_poisson, at a low rate so the count part
#      contributes zeros of its own
#
# Run with:
#   testthat::test_file("tests/local/test-family-mixture.R")

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(posterior)
  library(testthat)
})

# Several blocks below state what the package does not yet do, and
# testthat stops a file after ten failures by default, which would
# leave the blocks after them unrun and looking clean. The limit is
# read when the reporter is built, before this file is sourced, so it
# has to come from the environment:
#   TESTTHAT_MAX_FAILS=1000 Rscript -e "..."

# Resolved from where this file is running rather than from what is
# already on disk. testthat sets the working directory to the test
# file's own, so asking whether `fixtures` exists picks the wrong
# branch on a clean tree and writes tests/local/tests/local/fixtures.
cache_path <- function(name) {
  dir <- if (dir.exists(file.path("tests", "local"))) {
    file.path("tests", "local", "fixtures")
  } else {
    "fixtures"
  }
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(dir, name)
}

set.seed(11L)

n_time <- 80L
hu_true <- 0.3
x <- seq(-1.5, 1.5, length.out = n_time)
latent <- as.numeric(arima.sim(list(ar = 0.6), n_time, sd = 0.3))
mu_true <- exp(1.1 + 0.7 * x + latent)

# A zero-truncated draw is what the positive part of a hurdle family
# is: the zeros come from the hurdle and never from the count.
r_trunc_pois <- function(m) {
  repeat {
    v <- rpois(1, m)
    if (v > 0) return(v)
  }
}
y <- ifelse(
  rbinom(n_time, 1L, hu_true) == 1L, 0L,
  vapply(mu_true, r_trunc_pois, numeric(1))
)

dat <- data.frame(
  y = as.integer(y), x = x,
  time = seq_len(n_time), series = factor("s1")
)
stopifnot(any(dat$y == 0L), any(dat$y > 0L))

obs_zero_rate <- mean(dat$y == 0L)

sim_truth <- list(
  n_time = n_time, hu_true = hu_true, x = x, latent = latent,
  mu_true = mu_true, obs_zero_rate = obs_zero_rate
)


# -- Fit --------------------------------------------------------------

cache <- cache_path("val_mvgam_hurdle_poisson.rds")
if (file.exists(cache)) {
  cat("[cache] Loading hurdle Poisson fit.\n")
  fit <- readRDS(cache)
} else {
  cat("[fit ] mvgam(y ~ x, AR(p = 1), hurdle_poisson())\n")
  fit <- mvgam(
    y ~ x, trend_formula = ~ AR(p = 1), data = dat,
    family = brms::hurdle_poisson(),
    chains = 2L, iter = 800L, warmup = 400L,
    silent = 2, backend = "cmdstanr"
  )
}
if (!identical(attr(fit, "sim_truth"), sim_truth)) {
  attr(fit, "sim_truth") <- sim_truth
  saveRDS(fit, cache)
}

draw_ids <- c(3L, 17L, 55L, 120L, 301L)

# The density this family defines, written out rather than taken from
# the package. Both branches are needed: a form that used one of them
# everywhere still returns a finite matrix of the right shape.
hurdle_poisson_ll <- function(mu, hu, y) {
  out <- matrix(NA_real_, nrow(mu), ncol(mu))
  zero <- y == 0
  for (i in seq_len(nrow(mu))) {
    out[i, zero] <- log(hu[i, zero])
    out[i, !zero] <- log1p(-hu[i, !zero]) +
      dpois(y[!zero], mu[i, !zero], log = TRUE) -
      log1p(-exp(-mu[i, !zero]))
  }
  out
}

# The density a zero-inflated Poisson defines on the same parameters.
# Used only as a contrast, so that agreeing with the form above says
# something about which mixture was computed.
zi_poisson_ll <- function(mu, zi, y) {
  out <- matrix(NA_real_, nrow(mu), ncol(mu))
  zero <- y == 0
  for (i in seq_len(nrow(mu))) {
    out[i, zero] <- log(zi[i, zero] +
                          (1 - zi[i, zero]) * exp(-mu[i, zero]))
    out[i, !zero] <- log1p(-zi[i, !zero]) +
      dpois(y[!zero], mu[i, !zero], log = TRUE)
  }
  out
}


test_that("log_lik is the hurdle density, on either surface", {
  # The identity holds exactly, so this is an equality and not an
  # agreement. It is made on both surfaces because each composes `mu`
  # differently and a form that only matched one of them would be
  # reading the trend contribution wrongly.
  for (cond in c(TRUE, FALSE)) {
    mu <- posterior_linpred(fit, transform = TRUE, draw_ids = draw_ids,
                            incl_autocor = cond)
    hu <- posterior_linpred(fit, dpar = "hu", draw_ids = draw_ids,
                            incl_autocor = cond)
    ll <- log_lik(fit, draw_ids = draw_ids, incl_autocor = cond)
    expect_identical(dim(mu), dim(ll))
    expect_equal(unname(hurdle_poisson_ll(mu, hu, dat$y)), unname(ll),
                 tolerance = 1e-10)
  }
})


test_that("the density is the hurdle one and not the zero-inflated one", {
  # The two mixtures differ only in where the zeros may come from, and
  # both are finite, correctly shaped and plausibly scaled. Without
  # this contrast the check above would pass on either.
  mu <- posterior_linpred(fit, transform = TRUE, draw_ids = draw_ids,
                          incl_autocor = TRUE)
  hu <- posterior_linpred(fit, dpar = "hu", draw_ids = draw_ids,
                          incl_autocor = TRUE)
  ll <- log_lik(fit, draw_ids = draw_ids, incl_autocor = TRUE)
  zi_form <- zi_poisson_ll(mu, hu, dat$y)
  expect_true(all(is.finite(zi_form)))
  expect_gt(max(abs(zi_form - ll)), 0.5)
})


test_that("a density and its predictor must be asked for one surface", {
  # `log_lik()` conditions on the fitted state by default; every other
  # prediction method marginalises by default. Both defaults are
  # deliberate -- `?log_lik.mvgam` says a marginal density is not a
  # basis for an ELPD -- but crossed they describe different models,
  # which is what makes rebuilding a density from a linear predictor
  # go quietly wrong.
  expect_equal(log_lik(fit, draw_ids = draw_ids),
               log_lik(fit, draw_ids = draw_ids, incl_autocor = TRUE))
  expect_equal(
    posterior_linpred(fit, draw_ids = draw_ids),
    posterior_linpred(fit, draw_ids = draw_ids, incl_autocor = FALSE)
  )

  # Crossed, the closed form built on the marginal predictor no longer
  # reproduces the conditional density.
  mu_marg <- posterior_linpred(fit, transform = TRUE,
                               draw_ids = draw_ids, incl_autocor = FALSE)
  hu_marg <- posterior_linpred(fit, dpar = "hu", draw_ids = draw_ids,
                               incl_autocor = FALSE)
  ll_cond <- log_lik(fit, draw_ids = draw_ids, incl_autocor = TRUE)
  expect_gt(max(abs(hurdle_poisson_ll(mu_marg, hu_marg, dat$y) -
                      ll_cond)), 0.5)
})


test_that("the expectation is the hurdle mean", {
  # A hurdle mean is the Poisson rate rescaled by the truncation and
  # thinned by the hurdle. Returning the rate itself, or the rate
  # thinned but not rescaled, is positive, finite and the right shape,
  # so both are contrasted rather than assumed away.
  ids <- 1:200
  mu <- posterior_linpred(fit, transform = TRUE, draw_ids = ids)
  hu <- posterior_linpred(fit, dpar = "hu", draw_ids = ids)
  ep <- posterior_epred(fit, draw_ids = ids)

  expect_equal(unname(ep), unname((1 - hu) * mu / (1 - exp(-mu))),
               tolerance = 1e-10)
  expect_gt(max(abs(ep - mu)), 1)
  expect_gt(max(abs(ep - (1 - hu) * mu)), 0.1)
})


test_that("every zero comes from the hurdle", {
  # The property that separates this family from a zero-inflated one.
  # In a hurdle model the count part cannot produce a zero, so the
  # probability of observing one is `hu` exactly. A zero-inflated
  # model puts `hu + (1 - hu) * exp(-mu)` there, which on this fit is
  # about 0.46 against 0.37, so the two are told apart by the data
  # rather than by the family's name.
  ids <- 1:200
  mu <- posterior_linpred(fit, transform = TRUE, draw_ids = ids)
  hu <- posterior_linpred(fit, dpar = "hu", draw_ids = ids)
  set.seed(5L)
  pp <- posterior_predict(fit, draw_ids = ids)

  expect_true(all(pp >= 0))
  expect_true(all(pp == floor(pp)))
  # The replicated zero rate is the posterior mean of `hu`, and both
  # are the rate the data show.
  expect_equal(mean(pp == 0), mean(hu), tolerance = 0.03)
  expect_equal(mean(hu), obs_zero_rate, tolerance = 0.05)
  # And that is not what a zero-inflated model would have produced.
  zi_rate <- mean(hu + (1 - hu) * exp(-mu))
  expect_gt(zi_rate - mean(pp == 0), 0.03)
})


test_that("hu is one probability, shared by every row", {
  # With no sub-formula, `hu` is a scalar the sampler draws on the
  # probability scale rather than through a link, so asking for it
  # transformed changes nothing. Reading it as a logit would give
  # about 0.59 where the answer is 0.37.
  ids <- 1:200
  hu <- posterior_linpred(fit, dpar = "hu", draw_ids = ids)
  hu_t <- posterior_linpred(fit, dpar = "hu", draw_ids = ids,
                            transform = TRUE)
  expect_equal(unname(hu), unname(hu_t))
  expect_true(all(hu > 0 & hu < 1))
  # One value per draw, repeated across the rows.
  for (j in c(2L, 40L, ncol(hu))) {
    expect_equal(as.numeric(hu[, 1L]), as.numeric(hu[, j]))
  }
  # And it is the parameter the draws carry under that name.
  raw <- as.numeric(posterior::as_draws_matrix(fit)[, "hu"])[ids]
  expect_equal(as.numeric(hu[, 1L]), raw, tolerance = 1e-8)
})


test_that("the prediction types this family answers, and the one it does not", {
  # Each type is pinned to the quantity it claims to be, because
  # checking the shape and finiteness passes just as well when `link`
  # comes back on the response scale or `expected` returns the raw
  # Poisson rate. `draw_ids` rather than `ndraws`, so both sides read
  # the same iterations instead of two random subsamples.
  ids <- 1:400
  lk <- predict(fit, type = "link", draw_ids = ids)[, "Estimate"]
  ex <- predict(fit, type = "expected", draw_ids = ids)[, "Estimate"]
  set.seed(11L)
  rs <- predict(fit, type = "response", draw_ids = ids)[, "Estimate"]
  set.seed(11L)
  pp <- posterior_predict(fit, draw_ids = ids)

  expect_equal(unname(lk),
               unname(colMeans(posterior_linpred(fit, draw_ids = ids))),
               tolerance = 1e-12)
  expect_equal(unname(ex),
               unname(colMeans(posterior_epred(fit, draw_ids = ids))),
               tolerance = 1e-12)
  # Summarised without `robust`, so the estimate is the mean of the
  # sampled outcomes; `robust = TRUE` is the median of the same draws.
  expect_equal(unname(rs), unname(colMeans(pp)), tolerance = 1e-12)
  set.seed(11L)
  expect_equal(
    unname(predict(fit, type = "response", draw_ids = ids,
                   robust = TRUE)[, "Estimate"]),
    unname(apply(pp, 2L, median)), tolerance = 1e-12
  )
  # Unsummarised, the response type is the predictive draws.
  set.seed(11L)
  raw <- predict(fit, type = "response", draw_ids = ids, summary = FALSE)
  expect_equal(unname(raw), unname(pp), tolerance = 1e-12)
  expect_true(all(raw == floor(raw)))

  # And the three are far enough apart that none could stand in for
  # another: the hurdle sits between `link` and `expected`.
  expect_gt(max(abs(lk - log(ex))), 0.1)
  expect_gt(max(abs(rs - ex)), 0.1)
  # A hurdle mixture has no closed-form variance in mvgam's table, and
  # the refusal says so, lists what is supported and names the way
  # round it. A refusal that only said "not implemented" would leave a
  # user nowhere.
  err <- expect_error(predict(fit, type = "variance", ndraws = 50L),
                      "not implemented for family")
  msg <- conditionMessage(err)
  expect_match(msg, "hurdle_poisson", fixed = TRUE)
  expect_match(msg, "poisson", fixed = TRUE)
  # Matched with the wrapping collapsed: `insight::format_error()`
  # breaks lines at `getOption("width")`, so pinning the break pins
  # the console width the suite happens to run at.
  expect_match(gsub("[[:space:]]+", " ", msg),
               "compute the variance empirically", fixed = TRUE)
})


test_that("loo is built from the same density", {
  loo_warnings <- character(0)
  ic <- withCallingHandlers(
    loo(fit),
    warning = function(w) {
      loo_warnings <<- c(loo_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(is.finite(ic$estimates["elpd_loo", "Estimate"]))
  expect_identical(length(ic$diagnostics$pareto_k), nrow(dat))
  # loo picks its warning threshold from the number of draws (about
  # 0.66 here), not from the 0.7 the documentation quotes, so the
  # claim is bracketed either side of it. Pinning 0.7 passes today
  # and fails the moment a k lands between the two with nothing
  # wrong.
  pareto_k <- ic$diagnostics$pareto_k
  expect_true(all(is.finite(pareto_k)))
  expect_true(!any(pareto_k > 0.75) || length(loo_warnings) > 0L)
  expect_true(length(loo_warnings) == 0L || max(pareto_k) > 0.6)
  # Whatever was raised was the Pareto notice, so nothing else is
  # being muted by the handler above.
  expect_true(all(grepl("[Pp]areto[ _]k", loo_warnings)))
  # The pointwise terms loo consumes are the conditional density,
  # which is the surface `log_lik()` answers with by default. Stated
  # as an equality against loo run on that surface, and an inequality
  # against the marginal one, so the claim is read rather than merely
  # asserted to be finite.
  chain_id <- rep(seq_len(nchains(fit)),
                  each = ndraws(fit) / nchains(fit))
  ll_cond <- log_lik(fit, incl_autocor = TRUE)
  ll_marg <- log_lik(fit, incl_autocor = FALSE)
  # These raise the same Pareto notice, and it is collected into the
  # vector asserted on above rather than left to escape the file.
  by_cond <- withCallingHandlers(
    loo::loo(ll_cond,
             r_eff = loo::relative_eff(exp(ll_cond), chain_id = chain_id)),
    warning = function(w) {
      loo_warnings <<- c(loo_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  by_marg <- withCallingHandlers(
    loo::loo(ll_marg,
             r_eff = loo::relative_eff(exp(ll_marg), chain_id = chain_id)),
    warning = function(w) {
      loo_warnings <<- c(loo_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_equal(unname(ic$pointwise[, "elpd_loo"]),
               unname(by_cond$pointwise[, "elpd_loo"]),
               tolerance = 1e-10)
  expect_gt(max(abs(ic$pointwise[, "elpd_loo"] -
                      by_marg$pointwise[, "elpd_loo"])), 0.05)
})


# -- A hurdle with its own linear predictor ---------------------------
#
# Above, `hu` is one number the sampler draws directly on the
# probability scale, so asking for it transformed changes nothing and
# the link is never exercised. Giving it a sub-formula changes that:
# `hu ~ z` puts it on a logit link with a coefficient per row, which
# is the only arrangement where `posterior_linpred(dpar = )` has a
# scale to get wrong. The two fits together say that the identity
# above was a real identity rather than a transform that happened to
# be absent.
#
# It also gives the density a hurdle that varies across rows, so a
# form that applied one hurdle everywhere -- which the first fit
# cannot distinguish -- stops reproducing `log_lik()`.
#
#   truth: hu = plogis(-0.4 + 1.1 * z), z alternating -1 / +1, so the
#          two groups carry hurdles near 0.18 and 0.67

set.seed(23L)

n2 <- 90L
x2 <- seq(-1.5, 1.5, length.out = n2)
z2 <- rep(c(-1, 1), length.out = n2)
latent2 <- as.numeric(arima.sim(list(ar = 0.5), n2, sd = 0.3))
mu2_true <- exp(1.2 + 0.6 * x2 + latent2)
hu2_true <- plogis(-0.4 + 1.1 * z2)
y2 <- ifelse(
  rbinom(n2, 1L, hu2_true) == 1L, 0L,
  vapply(mu2_true, r_trunc_pois, numeric(1))
)

dat2 <- data.frame(
  y = as.integer(y2), x = x2, z = z2,
  time = seq_len(n2), series = factor("s1")
)
# The design is only informative if the two hurdle groups actually
# differ in the data, so that a model ignoring `z` would be visibly
# wrong rather than merely unidentified.
stopifnot(
  mean(dat2$y[z2 == -1] == 0L) < mean(dat2$y[z2 == 1] == 0L) - 0.2
)

cache2 <- cache_path("val_mvgam_hurdle_poisson_dpar.rds")
if (file.exists(cache2)) {
  cat("[cache] Loading hurdle Poisson fit with hu sub-formula.\n")
  fit2 <- readRDS(cache2)
} else {
  cat("[fit ] mvgam(bf(y ~ x, hu ~ z), AR(p = 1), hurdle_poisson())\n")
  fit2 <- mvgam(
    brms::bf(y ~ x, hu ~ z), trend_formula = ~ AR(p = 1), data = dat2,
    family = brms::hurdle_poisson(),
    chains = 2L, iter = 800L, warmup = 400L,
    silent = 2, backend = "cmdstanr"
  )
  saveRDS(fit2, cache2)
}


test_that("dpar = 'hu' is the hurdle's own linear predictor", {
  # Untransformed it is the logit-scale predictor, and it equals the
  # coefficients times the design exactly, so this reads the
  # parameters rather than merely checking a plausible range.
  dm <- posterior::as_draws_matrix(fit2)
  b0 <- as.numeric(dm[, "b_hu_Intercept"])[draw_ids]
  bz <- as.numeric(dm[, "b_hu_z"])[draw_ids]
  manual <- outer(b0, rep(1, n2)) + outer(bz, dat2$z)

  hu_link <- posterior_linpred(fit2, dpar = "hu", draw_ids = draw_ids)
  expect_equal(unname(hu_link), unname(manual), tolerance = 1e-10)

  # Transformed it is that predictor through the family's link for
  # this parameter, again exactly.
  hu_resp <- posterior_linpred(fit2, dpar = "hu", draw_ids = draw_ids,
                               transform = TRUE)
  expect_equal(unname(hu_resp), unname(plogis(manual)),
               tolerance = 1e-10)

  # The two scales are far apart, so returning the wrong one could
  # not pass the checks below by accident.
  expect_gt(max(abs(hu_link - hu_resp)), 1)
  expect_true(all(hu_resp > 0 & hu_resp < 1))
  expect_true(any(hu_link < 0))

  # Two covariate values, so two hurdles and no more.
  expect_identical(length(unique(round(hu_resp[1L, ], 10))), 2L)
})


test_that("the trend enters the mean and not the hurdle", {
  # `mu` carries the latent state, so it moves between the
  # conditional and marginal surfaces. `hu` has no trend term, so it
  # must not move at all -- a hurdle that shifted with the state
  # would mean the trend had been added to the wrong predictor.
  hu_c <- posterior_linpred(fit2, dpar = "hu", draw_ids = draw_ids,
                            transform = TRUE, incl_autocor = TRUE)
  hu_m <- posterior_linpred(fit2, dpar = "hu", draw_ids = draw_ids,
                            transform = TRUE, incl_autocor = FALSE)
  expect_equal(unname(hu_c), unname(hu_m), tolerance = 1e-12)

  mu_c <- posterior_linpred(fit2, transform = TRUE, draw_ids = draw_ids,
                            incl_autocor = TRUE)
  mu_m <- posterior_linpred(fit2, transform = TRUE, draw_ids = draw_ids,
                            incl_autocor = FALSE)
  expect_gt(max(abs(mu_c - mu_m)), 0.1)
})


test_that("the density carries a hurdle per row", {
  for (cond in c(TRUE, FALSE)) {
    mu <- posterior_linpred(fit2, transform = TRUE, draw_ids = draw_ids,
                            incl_autocor = cond)
    hu <- posterior_linpred(fit2, dpar = "hu", draw_ids = draw_ids,
                            transform = TRUE, incl_autocor = cond)
    ll <- log_lik(fit2, draw_ids = draw_ids, incl_autocor = cond)
    expect_equal(unname(hurdle_poisson_ll(mu, hu, dat2$y)), unname(ll),
                 tolerance = 1e-10)

    # A single hurdle for every row reproduces the first fit's
    # density but not this one, so this is the fit that can tell a
    # per-row hurdle from a shared one.
    hu_flat <- hu
    hu_flat[] <- rowMeans(hu)
    expect_gt(max(abs(hurdle_poisson_ll(mu, hu_flat, dat2$y) - ll)), 0.1)
  }
})


test_that("the covariate moves the zeros, and only the zeros", {
  ids <- 1:400
  hu <- posterior_linpred(fit2, dpar = "hu", draw_ids = ids,
                          transform = TRUE)
  set.seed(7L)
  pp <- posterior_predict(fit2, draw_ids = ids)

  # Within each group the replicated zero rate, the fitted hurdle and
  # the observed rate all agree; across groups they separate widely.
  # A model that had dropped `z` from the hurdle would still match
  # the pooled rate, so the comparison is made group by group.
  rates <- vapply(c(-1, 1), function(lev) {
    k <- dat2$z == lev
    c(obs = mean(dat2$y[k] == 0L), rep = mean(pp[, k] == 0),
      hu = mean(hu[, k]))
  }, numeric(3L))

  for (j in 1:2) {
    expect_equal(unname(rates["rep", j]), unname(rates["hu", j]),
                 tolerance = 0.03)
    expect_equal(unname(rates["obs", j]), unname(rates["hu", j]),
                 tolerance = 0.06)
  }
  expect_gt(rates["hu", 2L] - rates["hu", 1L], 0.3)
  expect_gt(rates["rep", 2L] - rates["rep", 1L], 0.3)

  # The positive counts are a truncated Poisson either way, so the
  # covariate must not have moved them into zero territory.
  expect_true(all(pp >= 0))
  expect_true(all(pp == floor(pp)))
})


test_that("newdata is what sets the hurdle for a new row", {
  # Two rows differing only in `z` must come back with the two
  # training hurdles. This is the path a prediction takes, and it is
  # separate from the one the fitted rows take.
  nd <- data.frame(
    y = NA_integer_, x = 0, z = c(-1, 1),
    time = c(n2 + 1L, n2 + 2L),
    series = factor("s1", levels = levels(dat2$series))
  )
  hu_nd <- posterior_linpred(fit2, newdata = nd, dpar = "hu",
                             transform = TRUE, draw_ids = 1:400)
  expect_identical(dim(hu_nd), c(400L, 2L))

  hu_fit <- posterior_linpred(fit2, dpar = "hu", transform = TRUE,
                              draw_ids = 1:400)
  expect_equal(mean(hu_nd[, 1L]), mean(hu_fit[, dat2$z == -1]),
               tolerance = 1e-8)
  expect_equal(mean(hu_nd[, 2L]), mean(hu_fit[, dat2$z == 1]),
               tolerance = 1e-8)
})


test_that("a distributional term is offered by default", {
  # `z` appears only in the hurdle's sub-formula. Enumerating the
  # default terms from the mean's right-hand side alone drops it, and
  # the user is shown a plot of `x` with no sign that a second
  # covariate exists. brms builds its default list from the whole
  # formula, and this is the same list.
  ce <- conditional_effects(fit2)
  expect_setequal(names(ce), c("x", "z"))

  ce_df <- as.data.frame(ce)
  panel <- ce_df[ce_df$effect == "z", ]
  expect_gt(nrow(panel), 1L)

  # The panel is the hurdle mean over `z` at the held values of the
  # other predictors, and its point estimate is the posterior median
  # there. Asserting the value rather than the shape is what
  # distinguishes this from a panel drawn on the wrong predictor.
  nd <- data.frame(
    y = NA_integer_, x = unique(panel$x), z = range(panel$z),
    time = unique(panel$time),
    series = factor("s1", levels = levels(dat2$series))
  )
  ep <- posterior_epred(fit2, newdata = nd)
  ends <- panel$estimate[match(range(panel$z), panel$z)]
  expect_equal(ends, unname(apply(ep, 2L, median)), tolerance = 1e-6)

  # A higher `z` means a higher hurdle, so the curve falls.
  expect_lt(ends[2L], ends[1L])
})


# -- Zero inflation, where the count part keeps its zeros -------------
#
# The rate is deliberately low. At a high rate `exp(-mu)` is near
# zero, the count part contributes almost no zeros, and a
# zero-inflated model becomes numerically hard to tell from a hurdle
# one -- the gap between `zi` and the true probability of a zero
# falls to a few points, which is inside Monte Carlo noise. At
# `exp(0.3 + ...)` the count part supplies zeros of its own, so the
# two quantities separate widely and the contrast below decides
# something.

set.seed(41L)

n3 <- 80L
x3 <- seq(-1.5, 1.5, length.out = n3)
latent3 <- as.numeric(arima.sim(list(ar = 0.6), n3, sd = 0.3))
mu3_true <- exp(0.3 + 0.6 * x3 + latent3)
zi3_true <- 0.35
# Unlike the hurdle draws above, the count part is not truncated: a
# zero here may have come from either source.
y3 <- ifelse(rbinom(n3, 1L, zi3_true) == 1L, 0L, rpois(n3, mu3_true))

dat3 <- data.frame(
  y = as.integer(y3), x = x3,
  time = seq_len(n3), series = factor("s1")
)
# The design only separates the two sources if the count part really
# does produce zeros, so this is checked rather than assumed.
stopifnot(
  mean(y3 == 0L) > zi3_true + 0.08,
  any(y3 > 0L)
)

cache3 <- cache_path("val_mvgam_zero_inflated_poisson.rds")
if (file.exists(cache3)) {
  cat("[cache] Loading zero-inflated Poisson fit.\n")
  fit3 <- readRDS(cache3)
} else {
  cat("[fit ] mvgam(y ~ x, AR(p = 1), zero_inflated_poisson())\n")
  fit3 <- mvgam(
    y ~ x, trend_formula = ~ AR(p = 1), data = dat3,
    family = brms::zero_inflated_poisson(),
    chains = 2L, iter = 800L, warmup = 400L,
    silent = 2, backend = "cmdstanr"
  )
  # Written under a temporary name and moved into place, so an
  # interrupted run cannot leave a truncated file to be read back.
  part <- paste0(cache3, ".part")
  saveRDS(fit3, part)
  file.rename(part, cache3)
}


test_that("log_lik is the zero-inflated density, on either surface", {
  for (cond in c(TRUE, FALSE)) {
    mu <- posterior_linpred(fit3, transform = TRUE, draw_ids = draw_ids,
                            incl_autocor = cond)
    zi <- posterior_linpred(fit3, dpar = "zi", draw_ids = draw_ids,
                            incl_autocor = cond)
    ll <- log_lik(fit3, draw_ids = draw_ids, incl_autocor = cond)
    expect_identical(dim(mu), dim(ll))
    expect_equal(unname(zi_poisson_ll(mu, zi, dat3$y)), unname(ll),
                 tolerance = 1e-10)

    # And not the hurdle density on the same parameters. With the
    # first fit checked the other way round, neither form can be
    # standing in for the other.
    hu_form <- hurdle_poisson_ll(mu, zi, dat3$y)
    expect_true(all(is.finite(hu_form)))
    expect_gt(max(abs(hu_form - ll)), 0.1)
  }
})


test_that("the zero-inflated expectation is thinned, not rescaled", {
  # `(1 - zi) * mu`, with no truncation factor: the count part is an
  # ordinary Poisson here, so nothing has been conditioned away.
  ids <- 1:200
  mu <- posterior_linpred(fit3, transform = TRUE, draw_ids = ids)
  zi <- posterior_linpred(fit3, dpar = "zi", draw_ids = ids)
  ep <- posterior_epred(fit3, draw_ids = ids)

  expect_equal(unname(ep), unname((1 - zi) * mu), tolerance = 1e-10)
  # The hurdle mean is the same expression with the truncation
  # factor, so this says the factor is absent rather than merely
  # small.
  expect_gt(max(abs(ep - (1 - zi) * mu / (1 - exp(-mu)))), 0.05)
})


test_that("zeros arrive from two sources", {
  # The property that separates the families. Under a hurdle the
  # probability of a zero is the hurdle exactly; here the count part
  # adds its own, so the rate must sit strictly above `zi` and at
  # `zi + (1 - zi) * exp(-mu)`.
  ids <- 1:400
  mu <- posterior_linpred(fit3, transform = TRUE, draw_ids = ids)
  zi <- posterior_linpred(fit3, dpar = "zi", draw_ids = ids)
  set.seed(13L)
  pp <- posterior_predict(fit3, draw_ids = ids)

  rep_zero <- mean(pp == 0)
  from_both <- mean(zi + (1 - zi) * exp(-mu))
  from_zi <- mean(zi)

  expect_equal(rep_zero, from_both, tolerance = 0.03)
  # Stated as a comparison as well as a tolerance, so the check does
  # not depend on how wide the design happened to make the gap.
  expect_lt(abs(rep_zero - from_both), abs(rep_zero - from_zi))
  expect_gt(rep_zero - from_zi, 0.05)

  expect_true(all(pp >= 0))
  expect_true(all(pp == floor(pp)))
})


test_that("zi is one probability, shared by every row", {
  ids <- 1:200
  zi <- posterior_linpred(fit3, dpar = "zi", draw_ids = ids)
  zi_t <- posterior_linpred(fit3, dpar = "zi", draw_ids = ids,
                            transform = TRUE)
  # As with `hu` on the first fit, no sub-formula means the sampler
  # draws this on the probability scale directly.
  expect_equal(unname(zi), unname(zi_t))
  expect_true(all(zi > 0 & zi < 1))
  expect_identical(length(unique(round(zi[1L, ], 10))), 1L)
  raw <- as.numeric(posterior::as_draws_matrix(fit3)[, "zi"])[ids]
  expect_equal(as.numeric(zi[, 1L]), raw, tolerance = 1e-8)
})


test_that("the two families disagree about where zeros come from", {
  # Side by side, on the draws alone, with no closed form involved.
  # This is the statement a user relies on when choosing between
  # them, and it is the one a density written with the wrong branch
  # would quietly break.
  ids <- 1:400
  set.seed(21L)

  hu <- posterior_linpred(fit, dpar = "hu", draw_ids = ids)
  pp_h <- posterior_predict(fit, draw_ids = ids)
  # A hurdle puts every zero in one place, so the rate is the
  # parameter itself.
  expect_equal(mean(pp_h == 0), mean(hu), tolerance = 0.03)

  zi <- posterior_linpred(fit3, dpar = "zi", draw_ids = ids)
  pp_z <- posterior_predict(fit3, draw_ids = ids)
  # Zero inflation does not, so the rate is strictly above it.
  expect_gt(mean(pp_z == 0) - mean(zi), 0.05)
})


test_that("loo is built from the zero-inflated density", {
  loo_warnings <- character(0)
  ic <- withCallingHandlers(
    loo(fit3),
    warning = function(w) {
      loo_warnings <<- c(loo_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(is.finite(ic$estimates["elpd_loo", "Estimate"]))
  expect_identical(length(ic$diagnostics$pareto_k), nrow(dat3))
  # loo picks its warning threshold from the number of draws (about
  # 0.66 here), not from the 0.7 the documentation quotes, so the
  # claim is bracketed either side of it. Pinning 0.7 passes today
  # and fails the moment a k lands between the two with nothing
  # wrong.
  pareto_k <- ic$diagnostics$pareto_k
  expect_true(all(is.finite(pareto_k)))
  expect_true(!any(pareto_k > 0.75) || length(loo_warnings) > 0L)
  expect_true(length(loo_warnings) == 0L || max(pareto_k) > 0.6)
  # Whatever was raised was the Pareto notice, so nothing else is
  # being muted by the handler above.
  expect_true(all(grepl("[Pp]areto[ _]k", loo_warnings)))
})


test_that("a forecast keeps the mechanism that made the zeros", {
  # Forecasting runs the trend forward and then draws from the
  # observation family, so it is a second implementation of the same
  # mixture and can disagree with the fitted one silently. Both
  # families are checked by the property that separates them, so a
  # forecast that drew from the wrong branch fails even though its
  # counts stay whole, non-negative and plausibly scaled.
  # Built from the frame each model was given rather than from a
  # field read back off the fit, so a fit that quietly kept different
  # rows fails here instead of being forecast from its own copy.
  future <- function(d, h) {
    data.frame(
      y = NA_integer_, x = 0,
      time = max(d$time) + seq_len(h),
      series = factor("s1", levels = levels(d$series))
    )
  }
  expect_equal(fit$data$y, dat$y)
  expect_equal(fit3$data$y, dat3$y)

  set.seed(4L)
  fc_h <- forecast(fit, newdata = future(dat, 6L), ndraws = 400L,
                   type = "response")
  draws_h <- fc_h$forecasts[[1L]]
  hu <- posterior_linpred(fit, dpar = "hu", draw_ids = 1:400)
  # A hurdle forecast puts every zero in the hurdle, as the fitted
  # model does.
  expect_equal(mean(draws_h == 0), mean(hu), tolerance = 0.04)
  expect_true(all(draws_h >= 0))
  expect_true(all(draws_h == floor(draws_h)))

  set.seed(4L)
  fc_z <- forecast(fit3, newdata = future(dat3, 6L), ndraws = 400L,
                   type = "response")
  draws_z <- fc_z$forecasts[[1L]]
  zi <- posterior_linpred(fit3, dpar = "zi", draw_ids = 1:400)
  mu <- posterior_linpred(fit3, transform = TRUE, draw_ids = 1:400)
  from_both <- mean(zi + (1 - zi) * exp(-mu))
  # And a zero-inflated forecast draws them from both sources, so it
  # lands on the compound rate rather than on `zi`.
  expect_equal(mean(draws_z == 0), from_both, tolerance = 0.04)
  expect_lt(abs(mean(draws_z == 0) - from_both),
            abs(mean(draws_z == 0) - mean(zi)))
  expect_gt(mean(draws_z == 0) - mean(zi), 0.05)
  expect_true(all(draws_z == floor(draws_z)))
})


test_that("fitted is the expectation, averaged", {
  # `fitted()` and `posterior_epred()` are separate entry points to
  # one quantity, and on a mixture the mean is the part most easily
  # composed differently by each.
  for (f in list(fit, fit2, fit3)) {
    expect_equal(unname(fitted(f)[, "Estimate"]),
                 unname(colMeans(posterior_epred(f))),
                 tolerance = 1e-12)
  }
})


test_that("the predictor is the design times the coefficients", {
  # Every density check above feeds mvgam's own `posterior_linpred()`
  # into a hand-written density and compares the result to mvgam's
  # own `log_lik()`. A wrongly composed `mu` moves both sides
  # together and none of them can see it. This one builds the
  # predictor from the sampled coefficients and the sampled state
  # instead, so the composition is read rather than assumed.
  ids <- 1:50
  dm <- posterior::as_draws_matrix(fit)
  raw <- posterior::as_draws_matrix(fit$fit)
  fixed <- outer(as.numeric(dm[, "b_Intercept"])[ids], rep(1, nrow(dat))) +
    outer(as.numeric(dm[, "b_x"])[ids], dat$x)

  # Marginalising over the trend leaves the observation predictor.
  expect_equal(
    unname(posterior_linpred(fit, draw_ids = ids, incl_autocor = FALSE)),
    unname(fixed), tolerance = 1e-10
  )

  # Conditioning adds the latent state of that row's own occasion,
  # so this reads `trend[t, s]` cell by cell. A state offset by one
  # occasion, or transposed, stays finite and fails here.
  # Coerced to a plain matrix: a `draws_matrix` on one side of the
  # comparison and not the other fails on class before it ever
  # reaches the numbers.
  trend <- matrix(
    as.numeric(raw[ids, paste0("trend[", seq_len(nrow(dat)), ",1]")]),
    nrow = length(ids)
  )
  expect_equal(
    unname(posterior_linpred(fit, draw_ids = ids, incl_autocor = TRUE)),
    unname(fixed + trend), tolerance = 1e-10
  )
  # The state is what separates the two surfaces, and it is not zero.
  expect_gt(max(abs(trend)), 0.1)
})


# -- The marginaleffects entry points on a mixture --------------------

test_that("the entry points reach the mixture, not the count alone", {
  # What only a mixture can be asked. That `avg_predictions` averages
  # the draws `predictions` returned is a property of marginaleffects
  # and is stated once, in test-draws-alignment.R; repeating it per
  # family would test the same code again. What is family-specific is
  # which density the estimate came off, and these two families
  # differ from the plain Poisson in opposite directions: a hurdle
  # conditions the count away from zero and a zero-inflated Poisson
  # adds zeros to it.
  library(marginaleffects)
  options("marginaleffects_model_classes" = "mvgam")

  for (m in list(fit, fit3)) {
    pred <- predictions(m, type = "expected")
    expect_identical(nrow(pred), nrow(m$data))
    expect_true(all(pred$estimate >= 0))
    # The estimate is the public method's own answer, summarised the
    # way this backend summarises. A branch that reached the count
    # component alone returns the right shape on the wrong density.
    expect_equal(pred$estimate,
                 unname(apply(t(posterior_epred(m, newdata = m$data)),
                              1L, stats::median)),
                 tolerance = 1e-8)
    # And that density is not the untouched Poisson the linear
    # predictor implies: exp(eta) is what a branch ignoring the
    # mixture would return, and the mixture moves it.
    eta <- posterior_linpred(m, newdata = m$data)
    poisson_mean <- unname(apply(exp(eta), 2L, stats::median))
    expect_false(isTRUE(all.equal(pred$estimate, poisson_mean)))

    slope <- avg_slopes(m, variables = "x", type = "expected")
    comp <- avg_comparisons(m, variables = "x", type = "expected")
    expect_identical(nrow(slope), 1L)
    expect_identical(as.character(slope$term), "x")
    expect_identical(nrow(comp), 1L)
    # `x` enters the count component of both models, and the mixture
    # weight is constant, so its effect survives to the expectation
    # with its sign intact. A derivative and a unit-step contrast
    # agree on that sign; a comparison that lost the covariate
    # returns zero.
    expect_gt(abs(comp$estimate), 1e-8)
    expect_identical(sign(comp$estimate), sign(slope$estimate))
  }
})


test_that("a distributional covariate is offered as a term", {
  # `hu ~ z` puts `z` in the model, and finding 20 recorded the
  # default effect list omitting it. The claim here is the one
  # marginaleffects acts on: `z` has to be reachable as a term with
  # a slope of its own, and that slope has to move the zeros rather
  # than the counts.
  library(marginaleffects)
  options("marginaleffects_model_classes" = "mvgam")
  preds <- insight::find_predictors(fit2)$conditional
  expect_true(all(c("x", "z") %in% preds))

  s_z <- avg_slopes(fit2, variables = "z", type = "expected")
  expect_identical(nrow(s_z), 1L)
  expect_gt(abs(s_z$estimate), 1e-8)
  # `z` enters through the hurdle alone, so a rise in it makes zeros
  # more likely and the expectation smaller. The slope therefore has
  # to carry the opposite sign to the hurdle coefficient, which is a
  # claim about direction rather than about magnitude.
  #
  # Read through the aliased draws rather than the raw stanfit: brms
  # writes the block as `b_hu[1]` and the alias pass is what turns it
  # into `b_hu_z`, which is finding 29's cause reaching this file.
  dm <- posterior::as_draws_matrix(fit2)
  b_hu <- as.numeric(dm[, "b_hu_z"])
  expect_gt(mean(b_hu), 0)
  expect_lt(s_z$estimate, 0)
})


# -- What these two families settle about the residual path ----------

test_that("a mixture's quantile residuals are standard normal", {
  # `quantile_family_specs` holds five entries, all continuous, so
  # every count family falls through to the empirical PIT. Finding 72
  # records a plain poisson reading sd 0.454 on that route, which
  # would make the route itself the fault.
  #
  # These two take the same route and answer correctly, at 0.947 and
  # 0.954 with roughly the 0.27 per cent a standard normal puts
  # beyond three standard deviations. So the route is not sufficient
  # to explain the compression, and this block is the evidence that
  # says so rather than a claim about these fits.
  for (f in list(fit, fit3)) {
    r <- suppressWarnings(
      residuals(f, type = "quantile", summary = FALSE, ndraws = 200L)
    )
    expect_lt(abs(stats::sd(r, na.rm = TRUE) - 1), 0.25)
    # And no column is pinned, which is finding 21's continuous half.
    constant <- apply(r, 2L, function(z) {
      length(unique(z[!is.na(z)])) <= 1L
    })
    expect_false(any(constant))
  }
})


test_that("the fit describes itself through the standard accessors", {
  # `family()` answers correctly on both, because a hurdle and a
  # zero-inflated Poisson are brms families rather than
  # `custom_family()` constructions. That is the control for finding
  # 40, which is about the custom route and not about mixtures.
  expect_identical(family(fit)$family, "hurdle_poisson")
  expect_identical(family(fit3)$family, "zero_inflated_poisson")
  expect_identical(as.character(glance(fit)$family), "hurdle_poisson")

  # `getCall()` is meant to return a call `update()` can re-evaluate.
  # The head position holds the `mvgam` closure, so printing it emits
  # the function's source in place of the call that made the fit.
  expect_identical(class(getCall(fit)[[1L]]), "name")
  # `model.frame()` answers and `terms()` does not, so the pair a
  # caller reaches for is half available. Asserted last.
  expect_identical(nrow(model.frame(fit)), nrow(dat))
  expect_s3_class(terms(fit), "terms")
})


test_that("k-fold partitions a frame with no gaps in it", {
  # 80 consecutive occasions on one series, no missing time and no
  # missing response. A fold takes half the rows, `update()` is
  # handed that subset and rebuilds the trend grid from it, so the
  # holes the split makes are read as irregular spacing. The
  # held-out rows are missing responses rather than missing time,
  # which is the distinction the fit itself already draws.
  expect_identical(sort(unique(diff(sort(dat$time)))), 1L)
  kf <- suppressWarnings(kfold(fit, K = 2L))
  expect_true(is.finite(kf$estimates["elpd_kfold", "Estimate"]))
})


test_that("an argument these methods cannot read is refused", {
  for (m in c("posterior_epred", "residuals", "predict", "summary")) {
    expect_error(do.call(m, list(fit3, zzz_unknown = 1)), "zzz_unknown")
  }
})


cat("\nDone.\n")
