# The four diagnostic `pp_check()` residual types, and the panel
# that composes them.
#
# The version this replaces read a cached fixture and asked of each
# type that it returned a ggplot with the right title. A panel
# plotting the wrong quantity on either axis satisfies that, which is
# how finding 24 below survived: `resid_vs_fitted` draws a
# conditional residual against a marginal fitted value, and the plot
# looks entirely reasonable.
#
# So each type is checked against the numbers behind it. The plot
# data of these panels is built by mvgam rather than by bayesplot, so
# it is readable: the lag panels carry one row per lag with quantile
# bands, and the scatter carries one row per observation with the two
# quantities it plots.
#
#   model: y ~ 1 + x, trend_formula = ~ AR(p = 1), poisson,
#          30 occasions, a strong AR(1) state
#
# Run with:
#   testthat::test_file("tests/local/test-pp-check-resids.R")

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(posterior)
  library(ggplot2)
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

sim_ar1 <- function(n, ar, sd) {
  out <- numeric(n)
  out[1] <- rnorm(1, 0, sd / sqrt(1 - ar^2))
  for (t in 2:n) out[t] <- ar * out[t - 1] + rnorm(1, 0, sd)
  out
}

set.seed(42L)
n_time <- 30L
latent <- sim_ar1(n_time, 0.7, 0.5)
z <- seq(-2, 2, length.out = n_time)
dat <- data.frame(
  y = rpois(n_time, exp(2 + latent + 0.5 * sin(z * pi))),
  x = rnorm(n_time),
  time = seq_len(n_time),
  series = factor("s1")
)

# The latent state has to carry real signal, or a marginal fitted
# value and a conditional one would agree and finding 24 would be
# invisible on this fit.
stopifnot(diff(range(dat$y)) > 20)

cache <- cache_path("val_ppc_ar1_fx.rds")
if (file.exists(cache)) {
  cat("[cache] Loading pp_check fit.\n")
  fit <- readRDS(cache)
} else {
  cat("[fit ] mvgam(y ~ 1 + x, AR(p = 1), poisson())\n")
  fit <- mvgam(
    y ~ 1 + x, trend_formula = ~ AR(p = 1), data = dat,
    family = poisson(), chains = 2L, iter = 1000L, warmup = 500L,
    silent = 2, backend = "cmdstanr"
  )
  part <- paste0(cache, ".part")
  saveRDS(fit, part)
  file.rename(part, cache)
}

ids <- 1:200
n_obs <- nrow(dat)


test_that("the lag panels carry one ordered band per lag", {
  acf_panel <- pp_check(fit, type = "resid_acf", draw_ids = ids)
  pacf_panel <- pp_check(fit, type = "resid_pacf", draw_ids = ids)
  expect_true(inherits(acf_panel, "ggplot"))
  expect_true(inherits(pacf_panel, "ggplot"))
  expect_identical(acf_panel$labels$title, "ACF")
  expect_identical(pacf_panel$labels$title, "pACF")

  for (panel in list(acf_panel, pacf_panel)) {
    d <- panel$data
    expect_identical(names(d),
                     c("lag", "q025", "q975", "q100", "q900",
                       "q250", "q750"))
    # Consecutive lags from one, so a panel that dropped or repeated
    # a lag is visible.
    expect_identical(as.integer(d$lag), seq_len(nrow(d)))
    expect_gt(nrow(d), 5L)
    # Nested quantile bands. Any pair swapped in the assembly would
    # draw a ribbon inside out while staying finite.
    expect_true(all(d$q025 <= d$q100))
    expect_true(all(d$q100 <= d$q250))
    expect_true(all(d$q250 <= d$q750))
    expect_true(all(d$q750 <= d$q900))
    expect_true(all(d$q900 <= d$q975))
    expect_true(all(abs(unlist(d[, -1L])) <= 1))
  }

  # The two compute different functions of the same residuals, so
  # returning one for the other is a real failure mode.
  expect_false(identical(acf_panel$data, pacf_panel$data))
})


test_that("the Q-Q panel holds every residual draw", {
  panel <- pp_check(fit, type = "resid_qq", draw_ids = ids)
  expect_true(inherits(panel, "ggplot"))
  expect_identical(panel$labels$title, "Normal Q-Q Plot")
  # One value per draw and observation, not one per observation.
  expect_identical(nrow(panel$data), length(ids) * n_obs)
  expect_true(all(is.finite(panel$data$resids)))
})


test_that("resid_vs_fitted plots one point per observation", {
  panel <- pp_check(fit, type = "resid_vs_fitted", draw_ids = ids)
  expect_true(inherits(panel, "ggplot"))
  expect_identical(panel$labels$title, "Resids vs Fitted")
  expect_identical(names(panel$data), c("preds", "resids"))
  expect_identical(nrow(panel$data), n_obs)

  # Pooled, it keeps every draw instead of collapsing them.
  pooled <- pp_check(fit, type = "resid_vs_fitted", draw_ids = ids,
                     per_obs = FALSE)
  expect_identical(nrow(pooled$data), length(ids) * n_obs)
})


test_that("the residual axis is the conditional residual", {
  # The y axis is the median over draws of the conditional residual,
  # exactly. Reading the marginal one instead moves it by about 8 on
  # this fit.
  set.seed(9L)
  panel <- pp_check(fit, type = "resid_vs_fitted", draw_ids = ids)
  set.seed(9L)
  conditional <- residuals(fit, draw_ids = ids, summary = FALSE,
                           incl_autocor = TRUE)
  expect_equal(unname(panel$data$resids),
               unname(apply(conditional, 2L, median)),
               tolerance = 1e-10)
})


test_that("the fitted axis reads the same surface as the residuals", {
  # FAILS TODAY -- finding 24, left standing so the sweep shows it.
  #
  # `pp_check()` swaps a NULL `newdata` for the training frame before
  # the fitted values are drawn, and `diagnostic_surface_args()` only
  # adds `incl_autocor = TRUE` when `newdata` is NULL. So the fitted
  # axis silently keeps `posterior_epred()`'s own default and comes
  # back marginal, while the residuals on the other axis are
  # conditional.
  #
  # The panel still looks reasonable, which is why a title check
  # never found it. What it costs is the diagnostic: the fitted axis
  # collapses onto the marginal mean, so the plot cannot show
  # structure across the range the model actually predicts over.
  panel <- pp_check(fit, type = "resid_vs_fitted", draw_ids = ids)
  conditional <- apply(
    posterior_epred(fit, draw_ids = ids, incl_autocor = TRUE),
    2L, median
  )
  expect_equal(unname(panel$data$preds), unname(conditional),
               tolerance = 1e-8)

  # And the consequence, stated on its own: the fitted axis should
  # cover the range the outcome does, not a narrow band beside it.
  axis_span <- diff(range(panel$data$preds))
  expect_gt(axis_span, 0.5 * diff(range(conditional)))
})


test_that("the panel composes the four types", {
  panel <- mvgam:::mvgam_resid_panel(fit, ndraws = 30L)
  expect_true(inherits(panel, "patchwork"))
  # Four plots, in the documented order.
  expect_identical(length(panel$patches$plots) + 1L, 4L)
  titles <- c(
    vapply(panel$patches$plots, function(p) p$labels$title,
           character(1L)),
    panel$labels$title
  )
  expect_setequal(titles,
                  c("Resids vs Fitted", "Normal Q-Q Plot", "ACF",
                    "pACF"))
})


cat("\nDone.\n")
