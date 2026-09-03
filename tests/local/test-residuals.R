# End-to-end tests for `residuals.mvgam`.
#
# The version this replaces opened with a loop over seven cached
# fixtures -- poisson, gaussian, beta, zero-inflated, hurdle,
# hurdle negbinomial, binomial -- and asked of each only that the
# result was a matrix with four columns of the expected names and no
# infinities. Seven fitted models to check four shapes, and a
# residual computed on the wrong surface, against the wrong
# observations, or summarised from the wrong draws satisfies every
# one of them. The mixture families now have their densities checked
# in `test-mixture-family-density.R` and the continuous-with-atom
# case in `test-tweedie-family.R`, so what is left here is what those
# do not cover: whether the residual is the quantity it claims to be.
#
# Two models carry that. A count fit is the one where an ordinary
# residual can be rebuilt by hand from the predictive draws. A
# gaussian fit with a strong latent signal is the one where reading
# the wrong surface is visible, because leaving the trend in the
# residual saturates the PIT and pins values at the `qnorm` bounds.
#
#   count    : y ~ 1, ~ AR(p = 1), poisson, 30 occasions
#   gaussian : y ~ 1 + x, ~ AR(p = 1), 150 occasions, ar 0.5
#
# Run with:
#   testthat::test_file("tests/local/test-residuals.R")

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(posterior)
  library(testthat)
})

cache_path <- function(name) {
  dir <- if (dir.exists("fixtures")) {
    "fixtures"
  } else {
    file.path("tests", "local", "fixtures")
  }
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(dir, name)
}

# Named for this file rather than shared, so the models asserted on
# are the ones built here and cannot be answered by a differently
# specified fit of the same name.
fit_cached <- function(name, ...) {
  path <- cache_path(paste0("val_resid_", name, ".rds"))
  if (file.exists(path)) {
    cat("[cache]", name, "\n")
    return(readRDS(path))
  }
  cat("[fit  ]", name, "\n")
  fit <- mvgam(
    ..., chains = 2L, iter = 1000L, warmup = 500L,
    silent = 2, backend = "cmdstanr"
  )
  part <- paste0(path, ".part")
  saveRDS(fit, part)
  file.rename(part, path)
  fit
}

sim_ar1 <- function(n, ar, sd) {
  out <- numeric(n)
  out[1] <- rnorm(1, 0, sd / sqrt(1 - ar^2))
  for (t in 2:n) out[t] <- ar * out[t - 1] + rnorm(1, 0, sd)
  out
}

set.seed(42L)
n_count <- 30L
latent <- sim_ar1(n_count, 0.7, 0.5)
z <- seq(-2, 2, length.out = n_count)
dat_count <- data.frame(
  y = rpois(n_count, exp(2 + latent + 0.5 * sin(z * pi))),
  time = seq_len(n_count),
  series = factor("s1")
)

set.seed(7L)
n_gauss <- 150L
gauss_latent <- sim_ar1(n_gauss, 0.5, 0.4)
gauss_x <- rnorm(n_gauss)
dat_gauss <- data.frame(
  y = 1.0 + 1.5 * gauss_x + gauss_latent + rnorm(n_gauss, 0, 0.3),
  x = gauss_x,
  time = seq_len(n_gauss),
  series = factor("s1")
)


# -- Fits -------------------------------------------------------------

fit_count <- fit_cached(
  "count", formula = y ~ 1, trend_formula = ~ AR(p = 1),
  data = dat_count, family = poisson()
)
fit_gauss <- fit_cached(
  "gauss", formula = y ~ 1 + x, trend_formula = ~ AR(p = 1),
  data = dat_gauss, family = gaussian()
)


test_that("the summary is the summary of the draws it reports", {
  # What the shape loop should have asked. Each column is a named
  # statistic of the per-draw matrix, so a summary built from a
  # second, independently drawn set of residuals -- which is what a
  # subsample taken twice produces -- fails here while still having
  # four correctly named columns of finite numbers.
  for (fit in list(fit_count, fit_gauss)) {
    # Seeded on both sides. The empirical PIT randomises each
    # residual between its two bounds, so two calls asking for the
    # same draws still differ, and the summary can only be compared
    # with the matrix it was computed from.
    ids <- 1:300
    set.seed(5L)
    summarised <- residuals(fit, draw_ids = ids)
    set.seed(5L)
    per_draw <- residuals(fit, draw_ids = ids, summary = FALSE)

    expect_true(is.matrix(summarised))
    expect_identical(colnames(summarised),
                     c("Estimate", "Est.Error", "Q2.5", "Q97.5"))
    expect_identical(nrow(summarised), ncol(per_draw))
    expect_identical(dim(per_draw), c(length(ids), nrow(fit$data)))

    expect_equal(unname(summarised[, "Estimate"]),
                 unname(colMeans(per_draw)), tolerance = 1e-12)
    expect_equal(unname(summarised[, "Est.Error"]),
                 unname(apply(per_draw, 2L, sd)), tolerance = 1e-12)
    expect_equal(unname(summarised[, "Q2.5"]),
                 unname(apply(per_draw, 2L, quantile, 0.025)),
                 tolerance = 1e-12)
    expect_equal(unname(summarised[, "Q97.5"]),
                 unname(apply(per_draw, 2L, quantile, 0.975)),
                 tolerance = 1e-12)
    # The interval brackets the estimate, which a column written in
    # the wrong order would not.
    expect_true(all(summarised[, "Q2.5"] <= summarised[, "Estimate"]))
    expect_true(all(summarised[, "Estimate"] <= summarised[, "Q97.5"]))
  }
})


test_that("robust summarises the same draws by their median", {
  ids <- 1:300
  set.seed(5L)
  per_draw <- residuals(fit_count, draw_ids = ids, summary = FALSE)
  set.seed(5L)
  robust <- residuals(fit_count, draw_ids = ids, robust = TRUE)
  expect_equal(unname(robust[, "Estimate"]),
               unname(apply(per_draw, 2L, median)), tolerance = 1e-12)
  # And the median is not the mean here, so the two summaries are
  # distinguishable on this fit.
  set.seed(5L)
  expect_false(isTRUE(all.equal(
    unname(robust[, "Estimate"]),
    unname(residuals(fit_count, draw_ids = ids)[, "Estimate"])
  )))
})


# ---- What in-sample quantile residuals can and cannot show -------
#
# A gaussian quantile residual is `qnorm(pnorm(y, mu, sigma))`, and
# `sigma` is the observation scale alone. In sample the latent state
# is fitted to the same observations it is then measured against, so
# `y - mu` is smaller than `sigma` and the residuals come back
# under-dispersed. That is the same optimism that makes in-sample
# PSIS-LOO unreliable for a state-space fit, not a defect, and no
# threshold makes `sd == 1` a property this quantity has. Calibration
# has to be judged out of sample or against a known truth.
#
# What the residuals do have to satisfy in sample is that they are
# centred, that none is clamped, and that they answer on the state
# the model inferred. The last is the one worth testing: reading the
# marginal surface instead leaves the trend in the residual, the PIT
# saturates, and values pin at the `qnorm` bounds.

test_that("in-sample gaussian residuals are centred and unclamped", {
  est <- residuals(fit_gauss, draw_ids = 1:500)[, "Estimate"]
  est <- est[is.finite(est)]
  expect_gt(length(est), 100L)
  expect_lt(abs(mean(est)), 0.25)
  # Under-dispersed by construction, but a degenerate or exploded
  # scale still says something has gone wrong.
  expect_gt(stats::sd(est), 0.1)
  expect_lt(stats::sd(est), 1)
  # Nothing pinned at the clamping bounds.
  expect_true(all(abs(est) < 5))
})


test_that("residuals read the fitted state, not the marginal trend", {
  # The defect this guards: `posterior_predict()` once dropped
  # `incl_autocor`, so every residual on a family without an analytic
  # quantile spec was PIT-ed against draws that ignored the fitted
  # state. On a strong-AR fit that saturates the PIT and pins values
  # at `qnorm(eps)`, and the autoregressive signal reappears as
  # residual autocorrelation. The two surfaces have to differ, and
  # the conditional one has to be the better behaved.
  set.seed(1L)
  cond <- residuals(fit_gauss, draw_ids = 1:300)[, "Estimate"]
  set.seed(1L)
  marg <- residuals(fit_gauss, draw_ids = 1:300,
                    incl_autocor = FALSE)[, "Estimate"]
  cond <- cond[is.finite(cond)]
  marg <- marg[is.finite(marg)]

  expect_false(isTRUE(all.equal(cond, marg)))
  # Leaving the trend in widens the spread rather than shrinking it.
  expect_gt(stats::sd(marg), stats::sd(cond))
  expect_gt(stats::sd(marg), 1)
  # And it is the marginal surface that clamps, not the conditional.
  expect_true(all(abs(cond) < 5))
  expect_gt(sum(abs(marg) > 5), 0L)
})


test_that("ordinary residuals match y - posterior_predict per draw", {
  set.seed(1L)
  r <- residuals(fit_count, type = "ordinary", draw_ids = 1:50,
                 summary = FALSE)
  # In sample `diagnostic_surface_args()` asks residuals for the
  # conditional surface, so the hand-built comparison has to ask for
  # it too. Reading the default here compares a residual taken
  # against the fitted state with a draw that ignores it.
  set.seed(1L)
  yrep <- posterior_predict(fit_count, draw_ids = 1:50,
                            incl_autocor = TRUE)
  y <- as.numeric(dat_count$y)
  manual <- sweep(yrep, 2L, y, FUN = function(yh, yi) yi - yh)
  expect_equal(unname(r), unname(manual), tolerance = 1e-10)
})


test_that("ordinary residuals do not match the marginal surface", {
  # The pairing above is a real constraint rather than one that
  # holds whatever surface either side reads.
  set.seed(1L)
  r <- residuals(fit_count, type = "ordinary", draw_ids = 1:50,
                 summary = FALSE)
  set.seed(1L)
  yrep <- posterior_predict(fit_count, draw_ids = 1:50,
                            incl_autocor = FALSE)
  y <- as.numeric(dat_count$y)
  marginal <- sweep(yrep, 2L, y, FUN = function(yh, yi) yi - yh)
  expect_false(isTRUE(all.equal(unname(r), unname(marginal))))
})


test_that("an unobserved row produces no residual", {
  fit <- fit_count
  d <- fit$data
  d$y[1L] <- NA
  fit$data <- d
  fit$obs_data <- d
  r <- residuals(fit, draw_ids = 1:100)
  expect_true(all(is.na(r[1L, ])))
  # And only that row: blanking one observation must not blank the
  # rest of the series.
  expect_true(all(is.finite(r[-1L, "Estimate"])))
})


cat("\nDone.\n")
