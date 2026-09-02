# Local fitting tests for the closure-unit family infrastructure.
# Covers `nmix()` (PB / RN / PPM variants); `occ()` and the simplex
# multi-response trio are out of scope here. Kept separate from
# tests/testthat/test-closure-unit-families.R because each block
# compiles a Stan model and runs short HMC chains; that is too
# expensive for the CI test suite. The cheap constructor / predicate
# / source-helper / standata round-trip / how_to_cite coverage
# stays in tests/testthat/.
#
# Run with:
#   Rscript -e "devtools::load_all('.'); testthat::test_file('tests/local/closure_unit_family_fitting.R')"
#
# Three blocks:
#   1. PB nmix prediction surface
#   2. Royle-Nichols Stan emission + end-to-end + smooth/RE/state
#   3. Poisson-Poisson Stan emission + end-to-end + smooth-p

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(testthat)
})

# ------------------------------------------------------------
# R-side prediction surface (PB nmix): log_lik,
# posterior_epred, posterior_predict, predict(latent_N),
# predict(detection)
# ------------------------------------------------------------
#
# These tests fit a small nmix() model on simulated data with
# known truth (lambda_intercept = 1, lambda_elev_slope = 0.5,
# p = 0.6) and verify that each surface returns the expected
# shape and recovers the truth within a wide CI.

# Cache one short fit at the top so each test runs fast. The
# truth values are fixed by the seed; recovery is loose because
# the chain is short (300 iter), so the assertions check shape
# + sign + order-of-magnitude rather than tight intervals.
local_nmix_fit <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    set.seed(42)
    n_unit <- 15
    n_visit <- 3
    elev <- rnorm(n_unit)
    log_lambda <- 1 + 0.5 * elev
    N_per <- rpois(n_unit, exp(log_lambda))
    p_true <- 0.6
    y_sim <- as.integer(unlist(lapply(N_per, function(N) {
      rbinom(n_visit, N, p_true)
    })))
    d <- data.frame(
      series = factor(rep(seq_len(n_unit), each = n_visit)),
      time   = rep(1L, n_unit * n_visit),
      y      = y_sim,
      cap    = rep(30L, n_unit * n_visit),
      elev   = rep(elev, each = n_visit)
    )
    # `threads = 2L` enables mvgam's `partial_sum_nmix_*_lpmf` +
    # `reduce_sum` per-closure-unit parallelism. Closure-unit
    # families thread independently of the brms partial-log-lik
    # path, so the brms-native threading gate is inert here.
    fit <- mvgam(y ~ elev,
                 family = nmix(),
                 data = d,
                 chains = 1, iter = 300, warmup = 150,
                 threads = 2L,
                 silent = 2, refresh = 0)
    cached <<- list(fit = fit, data = d, N_per = N_per, p_true = p_true)
    cached
  }
})

test_that("posterior_epred.mvgam returns [S x N_visit] for nmix and recovers lambda * p", {
  bundle <- local_nmix_fit()
  pe <- posterior_epred(bundle$fit)
  expect_equal(dim(pe), c(150L, nrow(bundle$data)))
  # Mean of E[Y] should be in the right ballpark of the data mean.
  expect_lt(abs(mean(pe) - mean(bundle$data$y)), 1.0)
})

test_that("posterior_predict.mvgam returns [S x N_visit] integer counts for nmix", {
  bundle <- local_nmix_fit()
  pp <- posterior_predict(bundle$fit)
  expect_equal(dim(pp), c(150L, nrow(bundle$data)))
  expect_true(all(pp == as.integer(pp)))
  expect_true(all(pp >= 0))
  # Marginal mean within data-mean ballpark.
  expect_lt(abs(mean(pp) - mean(bundle$data$y)), 1.5)
})

test_that("log_lik.mvgam returns [S x N_unit] for nmix (closure-unit grain for LOO)", {
  bundle <- local_nmix_fit()
  ll <- log_lik(bundle$fit)
  n_unit <- length(unique(bundle$data$series))
  expect_equal(dim(ll), c(150L, n_unit))
  expect_true(all(is.finite(ll)))
})

test_that("predict.mvgam(type = 'latent_state') returns [S x N_unit] integer N draws covering truth", {
  bundle <- local_nmix_fit()
  ln <- predict(bundle$fit, type = "latent_state", summary = FALSE)
  expect_equal(dim(ln), c(150L, length(unique(bundle$data$series))))
  expect_true(all(ln == as.integer(ln)))
  # Per-unit mean should land near the simulated N's mean.
  expect_lt(
    abs(mean(colMeans(ln)) - mean(bundle$N_per)),
    1.5
  )
})

test_that("predict.mvgam(type = 'detection') returns [S x N_visit] in (0, 1)", {
  bundle <- local_nmix_fit()
  de <- predict(bundle$fit, type = "detection", summary = FALSE)
  expect_equal(dim(de), c(150L, nrow(bundle$data)))
  expect_true(all(de > 0 & de < 1))
  # Scalar-p case: every visit column should have the same draw.
  expect_true(all(de[, 1L] == de[, 2L]))
  # Posterior mean covers the truth.
  expect_lt(abs(mean(de) - bundle$p_true), 0.2)
})

test_that("predict.mvgam(type = 'latent_N') errors on non-nmix families", {
  set.seed(1)
  d <- data.frame(
    series = factor(rep(1L:3L, each = 4L)),
    time   = 1L:4L,
    y      = rnorm(12L),
    elev   = rnorm(12L)
  )
  fit <- mvgam(y ~ elev, data = d, chains = 1, iter = 100,
               warmup = 50, silent = 2, refresh = 0)
  expect_error(
    predict(fit, type = "latent_state"),
    "not available for this family"
  )
  expect_error(
    predict(fit, type = "detection"),
    "not available for this family"
  )
})

test_that("nmix vector-p R-side prediction recovers the detection-covariate effect", {
  set.seed(31)
  n_unit <- 18; n_visit <- 4
  elev <- rnorm(n_unit)
  tod  <- stats::runif(n_unit * n_visit)
  N_per <- rpois(n_unit, exp(1.2 + 0.6 * elev))
  y_sim <- integer(n_unit * n_visit)
  for (g in seq_len(n_unit)) {
    visit_rows <- ((g - 1) * n_visit + 1):(g * n_visit)
    for (j in visit_rows) {
      y_sim[j] <- rbinom(1, N_per[g], plogis(-0.2 + 1.3 * tod[j]))
    }
  }
  d <- data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_unit * n_visit),
    y      = y_sim, cap = rep(30L, n_unit * n_visit),
    elev   = rep(elev, each = n_visit), tod = tod
  )
  fit <- mvgam(brms::bf(y ~ elev, p ~ tod), family = nmix(),
               data = d, chains = 1, iter = 400, warmup = 200,
               silent = 2, refresh = 0)
  de <- predict(fit, type = "detection", summary = FALSE)
  expect_equal(dim(de), c(200L, nrow(d)))
  # Vector-p must vary across visits (unlike the scalar case).
  expect_gt(stats::sd(apply(de, 2L, median)), 1e-3)
  # Detection covaries strongly with tod by construction.
  expect_gt(stats::cor(apply(de, 2L, median), d$tod), 0.8)
  # All five surfaces should run without error.
  expect_no_error(posterior_epred(fit))
  expect_no_error(posterior_predict(fit))
  expect_no_error(log_lik(fit))
  expect_no_error(predict(fit, type = "latent_state", summary = FALSE))
})

test_that("nmix smooth-in-p recovers a known non-linear effect", {
  set.seed(11)
  n_unit <- 25; n_visit <- 4
  elev <- rnorm(n_unit)
  tod  <- stats::runif(n_unit * n_visit)
  N_per <- rpois(n_unit, exp(1.2 + 0.6 * elev))
  y_sim <- integer(n_unit * n_visit)
  for (g in seq_len(n_unit)) {
    rows <- ((g - 1) * n_visit + 1):(g * n_visit)
    for (j in rows) {
      eta_p <- -0.5 + 2 * sin(2 * pi * tod[j])
      y_sim[j] <- rbinom(1, N_per[g], plogis(eta_p))
    }
  }
  d <- data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_unit * n_visit),
    y      = y_sim, cap = rep(40L, n_unit * n_visit),
    elev   = rep(elev, each = n_visit), tod = tod
  )
  fit <- mvgam(brms::bf(y ~ elev, p ~ s(tod, k = 8)),
               family = nmix(), data = d,
               chains = 1, iter = 400, warmup = 200,
               silent = 2, refresh = 0)
  de <- predict(fit, type = "detection", summary = FALSE)
  expect_equal(dim(de), c(200L, nrow(d)))
  de_med <- apply(de, 2L, median)
  truth <- plogis(-0.5 + 2 * sin(2 * pi * d$tod))
  expect_gt(stats::cor(de_med, truth), 0.9)
})

test_that("nmix random-effects-in-p recovers per-observer detection", {
  set.seed(21)
  n_unit <- 25; n_visit <- 4
  n_obs_total <- n_unit * n_visit
  elev <- rnorm(n_unit)
  N_per <- rpois(n_unit, exp(1.2 + 0.6 * elev))
  observer <- factor(sample(letters[1:5], n_obs_total, replace = TRUE))
  obs_effects <- c(a = -0.5, b = 0.3, c = 1.0, d = -0.2, e = 0.8)
  y_sim <- integer(n_obs_total)
  for (g in seq_len(n_unit)) {
    rows <- ((g - 1) * n_visit + 1):(g * n_visit)
    for (j in rows) {
      y_sim[j] <- rbinom(1, N_per[g],
                          plogis(0 + obs_effects[observer[j]]))
    }
  }
  d <- data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_obs_total),
    y      = y_sim, cap = rep(40L, n_obs_total),
    elev   = rep(elev, each = n_visit), observer = observer
  )
  fit <- mvgam(brms::bf(y ~ elev, p ~ (1 | observer)),
               family = nmix(), data = d,
               chains = 1, iter = 400, warmup = 200,
               silent = 2, refresh = 0)
  de <- predict(fit, type = "detection", summary = FALSE)
  de_med <- apply(de, 2L, median)
  truth <- plogis(0 + obs_effects[d$observer])
  expect_gt(stats::cor(de_med, truth), 0.85)
})

test_that("nmix gp-in-p recovers a non-linear detection effect", {
  set.seed(31)
  n_unit <- 25; n_visit <- 4
  elev <- rnorm(n_unit)
  tod  <- stats::runif(n_unit * n_visit)
  N_per <- rpois(n_unit, exp(1.2 + 0.6 * elev))
  y_sim <- integer(n_unit * n_visit)
  for (g in seq_len(n_unit)) {
    rows <- ((g - 1) * n_visit + 1):(g * n_visit)
    for (j in rows) {
      eta_p <- -0.2 + 1.8 * sin(2 * pi * tod[j])
      y_sim[j] <- rbinom(1, N_per[g], plogis(eta_p))
    }
  }
  d <- data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_unit * n_visit),
    y      = y_sim, cap = rep(40L, n_unit * n_visit),
    elev   = rep(elev, each = n_visit), tod = tod
  )
  fit <- mvgam(brms::bf(y ~ elev, p ~ gp(tod, k = 8, c = 5/4)),
               family = nmix(), data = d,
               chains = 1, iter = 400, warmup = 200,
               silent = 2, refresh = 0)
  de <- predict(fit, type = "detection", summary = FALSE)
  expect_equal(dim(de), c(200L, nrow(d)))
  de_med <- apply(de, 2L, median)
  truth <- plogis(-0.2 + 1.8 * sin(2 * pi * d$tod))
  expect_gt(stats::cor(de_med, truth), 0.85)
})

test_that("nmix predict(type = 'variance') equals predict(type = 'expected')", {
  bundle <- local_nmix_fit()
  v <- predict(bundle$fit, type = "variance", summary = FALSE)
  e <- predict(bundle$fit, type = "expected", summary = FALSE)
  expect_equal(dim(v), dim(e))
  # Var[Y] = lambda * p = E[Y] under the thinned-Poisson property
  # of the Poisson-Binomial mixture.
  expect_equal(v, e)
})

# ------------------------------------------------------------
# nmix("royle_nichols"): Stan emission via chains = 0
# (compile-only), end-to-end fit, smooth-r, random-r,
# smooth-on-state.
# ------------------------------------------------------------

test_that("nmix('royle_nichols') Stan emission carries the RN lpmf and the binary Y_max upper bound", {
  set.seed(7)
  n_unit <- 25L; n_visit <- 3L
  d <- data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(seq_len(n_visit), n_unit),
    y      = rbinom(n_unit * n_visit, 1, 0.4),
    cap    = rep(8L, n_unit * n_visit),
    elev   = rep(rnorm(n_unit), each = n_visit)
  )
  prefit <- mvgam(y ~ elev, family = nmix("royle_nichols"),
                  data = d, algorithm = "sampling", chains = 0)
  sc <- stancode(prefit)
  expect_true(grepl("nmix_royle_nichols_lpmf", sc))
  expect_true(grepl("array\\[N_unit\\] int<lower=0, upper=1> Y_max",
                    sc))
  expect_true(grepl("array\\[N_unit\\] int<lower=1> K_max", sc))
  expect_true(grepl("log1m_exp", sc))
  # The Bollen `+ 1` literal on the occ==0 log-prob must not leak
  # into the emitted lpmf.
  expect_false(grepl("\\+ 1;\\s*//\\s*occ", sc))
})

test_that("nmix('royle_nichols') end-to-end fit returns correct grain for every dispatcher arm", {
  set.seed(101)
  n_unit <- 30L; n_visit <- 4L
  elev <- rnorm(n_unit)
  lambda_true <- exp(1.2 + 0.6 * elev)
  r_true <- 0.3
  N_per <- rpois(n_unit, lambda_true)
  y_sim <- integer(n_unit * n_visit)
  for (g in seq_len(n_unit)) {
    rows <- ((g - 1L) * n_visit + 1L):(g * n_visit)
    p_visit <- 1 - (1 - r_true)^N_per[g]
    y_sim[rows] <- rbinom(n_visit, 1, p_visit)
  }
  d <- data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_unit * n_visit),
    y      = y_sim,
    cap    = rep(15L, n_unit * n_visit),
    elev   = rep(elev, each = n_visit)
  )
  fit <- mvgam(y ~ elev,
               family    = nmix("royle_nichols"),
               data      = d,
               chains    = 1, iter = 300, warmup = 150,
               silent    = 2, refresh = 0)
  n_total <- n_unit * n_visit
  yhat <- posterior_predict(fit)
  expect_equal(dim(yhat), c(150L, n_total))
  expect_true(all(yhat %in% c(0L, 1L)))
  ehat <- posterior_epred(fit)
  expect_equal(dim(ehat), c(150L, n_total))
  expect_true(all(ehat > 0 & ehat < 1))
  ll <- log_lik(fit)
  expect_equal(dim(ll), c(150L, n_unit))
  expect_true(all(is.finite(ll)))
  latent <- predict(fit, type = "latent_state", summary = FALSE)
  expect_equal(dim(latent), c(150L, n_unit))
  expect_true(all(latent >= 0L & latent <= 15L))
  expect_true(all(latent == as.integer(latent)))
  det <- predict(fit, type = "detection", summary = FALSE)
  expect_equal(dim(det), c(150L, n_total))
  expect_true(all(det > 0 & det < 1))
})

test_that("nmix('royle_nichols') smooth-r recovers a known non-linear detection effect", {
  set.seed(202)
  n_unit <- 40L; n_visit <- 5L
  elev <- rnorm(n_unit)
  tod  <- stats::runif(n_unit * n_visit)
  N_per <- rpois(n_unit, exp(1.4 + 0.5 * elev))
  y_sim <- integer(n_unit * n_visit)
  for (g in seq_len(n_unit)) {
    rows <- ((g - 1L) * n_visit + 1L):(g * n_visit)
    for (j in rows) {
      r_j <- plogis(-0.5 + 2 * sin(2 * pi * tod[j]))
      p_visit <- 1 - (1 - r_j)^N_per[g]
      y_sim[j] <- rbinom(1, 1, p_visit)
    }
  }
  d <- data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_unit * n_visit),
    y      = y_sim,
    cap    = rep(40L, n_unit * n_visit),
    elev   = rep(elev, each = n_visit),
    tod    = tod
  )
  fit <- mvgam(brms::bf(y ~ elev, p ~ s(tod, k = 8)),
               family    = nmix("royle_nichols"),
               data      = d,
               chains    = 1, iter = 400, warmup = 200,
               silent    = 2, refresh = 0)
  de <- predict(fit, type = "detection", summary = FALSE)
  expect_equal(dim(de), c(200L, nrow(d)))
  de_med <- apply(de, 2L, median)
  truth <- plogis(-0.5 + 2 * sin(2 * pi * d$tod))
  # RN binary data is less informative than PB counts per visit,
  # so the recovery threshold is looser than the PB equivalent
  # (which targets > 0.9).
  expect_gt(stats::cor(de_med, truth), 0.7)
})

test_that("nmix('royle_nichols') random-effects-r recovers per-observer detection", {
  set.seed(303)
  n_unit <- 40L; n_visit <- 5L
  n_obs_total <- n_unit * n_visit
  elev <- rnorm(n_unit)
  N_per <- rpois(n_unit, exp(1.4 + 0.5 * elev))
  observer <- factor(sample(letters[1:5], n_obs_total, replace = TRUE))
  obs_effects <- c(a = -0.5, b = 0.3, c = 1.0, d = -0.2, e = 0.8)
  y_sim <- integer(n_obs_total)
  for (g in seq_len(n_unit)) {
    rows <- ((g - 1L) * n_visit + 1L):(g * n_visit)
    for (j in rows) {
      r_j <- plogis(0 + obs_effects[observer[j]])
      p_visit <- 1 - (1 - r_j)^N_per[g]
      y_sim[j] <- rbinom(1, 1, p_visit)
    }
  }
  d <- data.frame(
    series   = factor(rep(seq_len(n_unit), each = n_visit)),
    time     = rep(1L, n_obs_total),
    y        = y_sim,
    cap      = rep(40L, n_obs_total),
    elev     = rep(elev, each = n_visit),
    observer = observer
  )
  fit <- mvgam(brms::bf(y ~ elev, p ~ (1 | observer)),
               family    = nmix("royle_nichols"),
               data      = d,
               chains    = 1, iter = 400, warmup = 200,
               silent    = 2, refresh = 0)
  de <- predict(fit, type = "detection", summary = FALSE)
  de_med <- apply(de, 2L, median)
  truth <- plogis(0 + obs_effects[d$observer])
  expect_gt(stats::cor(de_med, truth), 0.6)
})

test_that("nmix('royle_nichols') smooth-on-state recovers a non-linear lambda effect", {
  set.seed(404)
  n_unit <- 50L; n_visit <- 5L
  elev <- stats::runif(n_unit, -2, 2)
  # Non-linear lambda(elev): peaks in the middle of the range
  lambda_true <- exp(1.0 + 1.5 * exp(-elev^2 / 2) - 0.5)
  r_true <- 0.4
  N_per <- rpois(n_unit, lambda_true)
  y_sim <- integer(n_unit * n_visit)
  for (g in seq_len(n_unit)) {
    rows <- ((g - 1L) * n_visit + 1L):(g * n_visit)
    p_visit <- 1 - (1 - r_true)^N_per[g]
    y_sim[rows] <- rbinom(n_visit, 1, p_visit)
  }
  d <- data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_unit * n_visit),
    y      = y_sim,
    cap    = rep(30L, n_unit * n_visit),
    elev   = rep(elev, each = n_visit)
  )
  fit <- mvgam(y ~ s(elev, k = 8),
               family    = nmix("royle_nichols"),
               data      = d,
               chains    = 1, iter = 400, warmup = 200,
               silent    = 2, refresh = 0)
  ehat <- posterior_epred(fit)
  ehat_med <- apply(ehat, 2L, median)
  # Truth at each visit: 1 - exp(-r * lambda(elev))
  truth <- 1 - exp(-r_true * exp(1.0 + 1.5 * exp(-d$elev^2 / 2) - 0.5))
  expect_gt(stats::cor(ehat_med, truth), 0.7)
})

# ------------------------------------------------------------
# nmix("poisson_poisson"): Stan emission via chains = 0,
# end-to-end fit, smooth-p.
# ------------------------------------------------------------

test_that("nmix('poisson_poisson') Stan emission carries the factored Poisson lpmf", {
  set.seed(7)
  n_unit <- 25L; n_visit <- 3L
  d <- data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(seq_len(n_visit), n_unit),
    y      = rpois(n_unit * n_visit, 2),
    cap    = rep(20L, n_unit * n_visit),
    elev   = rep(rnorm(n_unit), each = n_visit)
  )
  prefit <- mvgam(y ~ elev, family = nmix("poisson_poisson"),
                  data = d, algorithm = "sampling", chains = 0)
  sc <- stancode(prefit)
  expect_true(grepl("nmix_poisson_poisson_lpmf", sc))
  expect_true(grepl("array\\[N_unit\\] int<lower=1> K_max", sc))
  # Y_max stays unbounded for PPM (y_t can exceed N).
  expect_true(grepl("array\\[N_unit\\] int<lower=0> Y_max", sc))
  expect_false(grepl("upper=1>\\s+Y_max", sc))
  # Factored constants in the loop body.
  expect_true(grepl("sum_counts", sc))
  expect_true(grepl("sum_y_log_p", sc))
  expect_true(grepl("sum_p_v", sc))
  expect_true(grepl("lgamma_const", sc))
  # No O(n_rep) per-k poisson_log_lpmf(counts | ...) call.
  expect_false(grepl("poisson_log_lpmf\\(counts\\s*\\|\\s*log\\(k\\)",
                     sc))
  # Log link on p (not logit).
  expect_true(grepl("log\\(p\\)", sc))
  expect_false(grepl("logit\\(p\\)", sc))
})

test_that("nmix('poisson_poisson') end-to-end fit returns correct grain for every dispatcher arm", {
  set.seed(202)
  n_unit <- 30L; n_visit <- 4L
  elev <- rnorm(n_unit)
  lambda_true <- exp(1.0 + 0.5 * elev)
  p_true <- 0.4
  N_per <- rpois(n_unit, lambda_true)
  y_sim <- integer(n_unit * n_visit)
  for (g in seq_len(n_unit)) {
    rows <- ((g - 1L) * n_visit + 1L):(g * n_visit)
    y_sim[rows] <- rpois(n_visit, lambda = N_per[g] * p_true)
  }
  d <- data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_unit * n_visit),
    y      = y_sim,
    cap    = rep(30L, n_unit * n_visit),
    elev   = rep(elev, each = n_visit)
  )
  fit <- mvgam(y ~ elev,
               family    = nmix("poisson_poisson"),
               data      = d,
               chains    = 1, iter = 300, warmup = 150,
               silent    = 2, refresh = 0)
  n_total <- n_unit * n_visit
  yhat <- posterior_predict(fit)
  expect_equal(dim(yhat), c(150L, n_total))
  expect_true(all(yhat == as.integer(yhat)))
  expect_true(all(yhat >= 0L))
  ehat <- posterior_epred(fit)
  expect_equal(dim(ehat), c(150L, n_total))
  expect_true(all(ehat > 0))
  ll <- log_lik(fit)
  expect_equal(dim(ll), c(150L, n_unit))
  expect_true(all(is.finite(ll)))
  latent <- predict(fit, type = "latent_state", summary = FALSE)
  expect_equal(dim(latent), c(150L, n_unit))
  expect_true(all(latent >= 0L & latent <= 30L))
  expect_true(all(latent == as.integer(latent)))
  det <- predict(fit, type = "detection", summary = FALSE)
  expect_equal(dim(det), c(150L, n_total))
  expect_true(all(det > 0))
})

test_that("nmix('poisson_poisson') smooth-p recovers a known non-linear encounter-rate effect", {
  set.seed(303)
  n_unit <- 40L; n_visit <- 5L
  elev <- rnorm(n_unit)
  tod  <- stats::runif(n_unit * n_visit)
  N_per <- rpois(n_unit, exp(1.2 + 0.5 * elev))
  y_sim <- integer(n_unit * n_visit)
  for (g in seq_len(n_unit)) {
    rows <- ((g - 1L) * n_visit + 1L):(g * n_visit)
    for (j in rows) {
      p_j <- exp(-1 + 1.2 * sin(2 * pi * tod[j]))
      y_sim[j] <- rpois(1, lambda = N_per[g] * p_j)
    }
  }
  d <- data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_unit * n_visit),
    y      = y_sim,
    cap    = rep(40L, n_unit * n_visit),
    elev   = rep(elev, each = n_visit),
    tod    = tod
  )
  fit <- mvgam(brms::bf(y ~ elev, p ~ s(tod, k = 8)),
               family    = nmix("poisson_poisson"),
               data      = d,
               chains    = 1, iter = 400, warmup = 200,
               silent    = 2, refresh = 0)
  de <- predict(fit, type = "detection", summary = FALSE)
  expect_equal(dim(de), c(200L, nrow(d)))
  de_med <- apply(de, 2L, median)
  truth <- exp(-1 + 1.2 * sin(2 * pi * d$tod))
  # PPM encounter rates are unidentified up to a scale (banana
  # ridge on the `lambda * p` product), so absolute recovery may
  # be off; the shape correlation with the truth is what the
  # smooth identifies once the elev covariate partially constrains
  # `lambda`. Threshold tightened from 0.7 to 0.80 per stats
  # review: a correct factored Poisson precompute on this
  # simulation should recover the shape well above 0.7, so a
  # weaker threshold has no power to catch an implementation
  # error in the precompute terms.
  expect_gt(stats::cor(de_med, truth), 0.80)
})
