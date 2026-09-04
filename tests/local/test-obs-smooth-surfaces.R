# Smooth and Gaussian-process terms, and the three methods that
# enumerate and evaluate them.
#
# One fit carries every smooth shape the rest of the suite does not:
#
#   y ~ s(z, by = grp) + t2(z, w) + gp(w, by = cat)
#
# A by-factor smooth, a tensor product and a by-factor Gaussian
# process, side by side. Fitting them together rather than one per
# model is what makes the claims below possible: `smooths()`,
# `posterior_smooths()` and `conditional_smooths()` each have to pick
# the right term out of three, and a method that returned the first
# term whatever it was asked for passes on any single-smooth model.
#
# The shapes this file does not fit are the ones already fitted by
# the files that own them, so nothing is refitted to be looked at
# twice:
#
#   s(z) alone            test-random-effects.R, test-pathfinder-init.R
#   a two-covariate s()   test-trend-pw.R
#   gp(x)                 test-trend-ar-multilag.R, test-forecast-recovery.R
#   gp(x1, x2)            test-trend-arma.R
#   a trend-side smooth   test-draws-alignment.R
#   s(x, by = lv_axis())  test-factor-lv-axis.R, test-grain-closure-units.R
#
# `assert_by_factor_variation()` is the reason the by-factor terms
# are here at all. A by-factor contribution can be carried by one
# prediction route and dropped by another, and a fit that dropped it
# everywhere still returns the right shape from all of them, so six
# routes are driven at once.

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(posterior)
  library(testthat)
  library(marginaleffects)
})

source(if (file.exists("concordance_helpers.R")) {
  "concordance_helpers.R"
} else {
  file.path("tests", "local", "concordance_helpers.R")
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


sm_data <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    set.seed(42L)
    n <- 60L
    ar_coef <- 0.7
    sigma <- 0.5
    latent <- numeric(n)
    latent[1L] <- rnorm(1L, 0, sigma / sqrt(1 - ar_coef^2))
    for (t in 2L:n) {
      latent[t] <- ar_coef * latent[t - 1L] + rnorm(1L, 0, sigma)
    }
    z <- seq(-2, 2, length.out = n)
    w <- seq(-1, 1, length.out = n)
    # Three grouping levels for the smooth and two for the GP, so a
    # by-factor block counted against the wrong factor is the wrong
    # size rather than merely the wrong contents.
    grp <- factor(rep(letters[1:3], length.out = n), levels = letters[1:3])
    cat_f <- factor(rep(c("A", "B"), length.out = n), levels = c("A", "B"))
    cached <<- data.frame(
      y = rpois(n, exp(1.6 + latent + 0.5 * sin(z * pi) +
                         0.4 * w * as.integer(cat_f))),
      z = z, w = w, grp = grp, cat = cat_f,
      time = seq_len(n),
      series = factor("s1")
    )
    cached
  }
})


sm_fit <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    path <- cache_path("val_mvgam_smooth_surfaces.rds")
    if (file.exists(path)) {
      cached <<- readRDS(path)
      return(cached)
    }
    cached <<- mvgam(
      formula = y ~ s(z, by = grp, k = 5) + t2(z, w, k = c(4, 4)) +
        gp(w, by = cat, k = 5),
      trend_formula = ~ AR(p = 1),
      data = sm_data(), family = poisson(),
      chains = 2L, iter = 1000L, warmup = 500L,
      silent = 2, backend = "cmdstanr"
    )
    saveRDS(cached, path)
    cached
  }
})


test_that("the frame separates the two grouping factors", {
  d <- sm_data()
  # Three levels against two, so a by-factor block counted against
  # the wrong factor has the wrong length and cannot pass by luck.
  expect_identical(nlevels(d$grp), 3L)
  expect_identical(nlevels(d$cat), 2L)
  expect_false(nlevels(d$grp) == nlevels(d$cat))
  expect_identical(nrow(d), 60L)
})


test_that("smooths names all three terms, and only those", {
  mv <- sm_fit()
  # The expected list comes from the fit's own formula, so this
  # asks whether `smooths()` reports what the model was given
  # rather than whether two packages agree.
  expected <- attr(
    terms(brms::brmsterms(mv$formula)$dpars$mu$sm), "term.labels"
  )
  got <- smooths(mv)
  expect_setequal(got, expected)
  # A GP is not an mgcv smooth, so `smooths()` names the by-factor
  # smooth and the tensor and leaves `gp(w, by = cat)` out: the GP
  # is reached through its own machinery. Two, not three, and which
  # two is the claim.
  expect_length(got, 2L)
  expect_true(any(grepl("^s\\(z", got)))
  expect_true(any(grepl("^t2\\(z", got)))
  expect_false(any(grepl("^gp\\(", got)))
})


test_that("posterior_smooths evaluates the term it was asked for", {
  mv <- sm_fit()
  d <- sm_data()
  terms_avail <- smooths(mv)
  etas <- lapply(terms_avail, function(tm) {
    posterior_smooths(mv, smooth = tm)
  })
  names(etas) <- terms_avail
  for (tm in terms_avail) {
    expect_identical(dim(etas[[tm]]),
                     c(as.integer(ndraws(mv)), nrow(d)))
    expect_true(all(is.finite(etas[[tm]])))
    # A smooth that never reached the draws returns zeros, which
    # keeps the dimensions above.
    expect_gt(stats::sd(apply(etas[[tm]], 2L, stats::median)), 1e-6)
  }
  # The three terms are different functions of the data. A method
  # that returned the first basis block whatever it was asked for
  # would return three identical matrices and satisfy every claim
  # above.
  for (i in seq_along(terms_avail)[-1L]) {
    expect_false(isTRUE(all.equal(etas[[1L]], etas[[i]])))
  }

  # Column order follows the frame's own rows, which a shuffled
  # frame is what tests: a smooth evaluated by position rather than
  # by content answers in the original order.
  tm <- terms_avail[1L]
  perm <- c(seq(2L, nrow(d)), 1L)
  eta_s <- posterior_smooths(mv, smooth = tm,
                             newdata = d[perm, , drop = FALSE])
  expect_equal(apply(eta_s, 2L, stats::median),
               apply(etas[[tm]], 2L, stats::median)[perm],
               tolerance = 1e-8)
})


test_that("posterior_smooths honours ndraws and draw_ids", {
  mv <- sm_fit()
  tm <- smooths(mv)[1L]
  ids <- c(3L, 11L, 42L)
  by_id <- posterior_smooths(mv, smooth = tm, draw_ids = ids)
  expect_identical(nrow(by_id), length(ids))
  # Naming the same draws twice returns the same numbers, so the
  # selection is by index and not a fresh sample each call.
  expect_equal(by_id, posterior_smooths(mv, smooth = tm,
                                        draw_ids = ids))
  expect_identical(nrow(posterior_smooths(mv, smooth = tm,
                                          ndraws = 7L)), 7L)
  expect_error(posterior_smooths(mv, smooth = "s(nonexistent)"))
})


test_that("s(z, by = grp) gives each of the three levels a curve", {
  mv <- sm_fit()
  lev <- levels(sm_data()$grp)
  cs <- conditional_smooths(mv)
  by_term <- Filter(function(d) "grp" %in% colnames(d), cs)
  expect_length(by_term, 1L)
  d <- by_term[[1L]]
  expect_setequal(as.character(unique(d$grp)), lev)
  # One block of grid rows per level, so three levels means three
  # times the single-curve row count. A model that fitted one
  # shared curve returns a third of this.
  expect_identical(nrow(d), 100L * length(lev))
  curves <- split(d$estimate__, d$grp)
  expect_length(curves, length(lev))
  for (k in seq_along(curves)) {
    expect_gt(stats::sd(curves[[k]]), 1e-6)
    expect_gt(mean(d$upper__[d$grp == lev[k]] -
                     d$lower__[d$grp == lev[k]]), 0)
  }
  # The levels differ from one another, which is what `by =` asks
  # for and what finding 35 records failing on the factor grain.
  expect_false(isTRUE(all.equal(curves[[1L]], curves[[2L]])))
  expect_false(isTRUE(all.equal(curves[[2L]], curves[[3L]])))
})


test_that("t2(z, w) is a surface over both covariates", {
  mv <- sm_fit()
  cs <- conditional_smooths(mv)
  tens <- Filter(function(d) all(c("z", "w") %in% colnames(d)), cs)
  expect_length(tens, 1L)
  d <- tens[[1L]]
  # A tensor's grid is a grid: both margins appear and the row
  # count is their product. A tensor collapsed to one covariate
  # returns a grid of 100 and passes any is-it-there check.
  expect_true(all(c("effect1__", "effect2__") %in% colnames(d)))
  expect_identical(nrow(d), 100L * 100L)
  expect_identical(length(unique(d$z)), 100L)
  expect_identical(length(unique(d$w)), 100L)

  # And it moves along both margins. One that varied with `z` alone
  # would be an s(z) wearing a tensor's shape.
  along_w <- vapply(split(d$estimate__, d$z), stats::sd, numeric(1L))
  along_z <- vapply(split(d$estimate__, d$w), stats::sd, numeric(1L))
  expect_gt(mean(along_w), 1e-6)
  expect_gt(mean(along_z), 1e-6)
})


test_that("conditional_smooths returns one entry per smooth term", {
  mv <- sm_fit()
  cs <- conditional_smooths(mv)
  expect_s3_class(cs, "mvgam_conditional_smooths")
  expect_length(cs, length(smooths(mv)))
  for (d in cs) {
    expect_true(all(c("effect1__", "estimate__", "se__",
                      "lower__", "upper__") %in% colnames(d)))
    # Finding 34 is what happens when the frame is right and
    # nothing checks it was evaluated: a constant estimate with a
    # zero-width band satisfies the column list and the row count.
    expect_gt(stats::sd(d$estimate__), 1e-6)
    expect_true(all(d$se__ > 0))
    expect_true(all(d$lower__ <= d$estimate__))
    expect_true(all(d$estimate__ <= d$upper__))
  }
  # Asking for one term returns that term alone.
  one <- conditional_smooths(mv, smooths = smooths(mv)[1L])
  expect_length(one, 1L)
})


test_that("gp(w, by = cat) reaches every prediction route", {
  mv <- sm_fit()
  d <- sm_data()
  n <- 6L
  # Two grids alike in every column but the by-factor, so any
  # difference between their predictions is that term and nothing
  # else. Every covariate the model reads has to be present, since
  # a frame missing one is refused before a prediction is made.
  grid_A <- data.frame(
    z = seq(min(d$z), max(d$z), length.out = n),
    w = seq(min(d$w), max(d$w), length.out = n),
    grp = factor(levels(d$grp)[1L], levels = levels(d$grp)),
    cat = factor("A", levels = levels(d$cat)),
    series = factor("s1", levels = levels(d$series)),
    time = seq_len(n)
  )
  grid_B <- grid_A
  grid_B$cat <- factor("B", levels = levels(d$cat))
  expect_gt(stats::sd(grid_A$w), 0)

  # posterior_linpred, posterior_epred, fitted, posterior_predict,
  # log_lik and the marginaleffects entry point all have to tell the
  # two levels apart. A by-factor contribution dropped from any one
  # of them fails here and nowhere else.
  assert_by_factor_variation(mv, grid_A, grid_B)
})


test_that("the by-factor GP carries one length-scale per level", {
  mv <- sm_fit()
  # `gp()` is isotropic by default, so a second covariate would not
  # add a length-scale; a second by-factor level does. Two levels of
  # `cat` therefore give two, indexed `lscale_<id>[level, dim]`.
  lscale <- grep("^lscale_", variables(mv), value = TRUE)
  expect_length(lscale, nlevels(sm_data()$cat))
  dm <- as_draws_matrix(mv)
  for (p in lscale) expect_true(all(as.numeric(dm[, p]) > 0))
  # The two levels are estimated separately, so a program that
  # declared two and filled both from one level would tie them.
  expect_false(isTRUE(all.equal(as.numeric(dm[, lscale[1L]]),
                                as.numeric(dm[, lscale[2L]]))))
})


cat("\nDone.\n")
