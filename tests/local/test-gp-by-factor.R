# Gaussian processes with more than one covariate, and with a
# by-factor.
#
# `gp()` reaches Stan as an approximate basis whose length-scale is
# indexed `lscale_<id>[level, dim]`. The level index is what a
# by-factor multiplies; the dimension index is not what a second
# covariate multiplies, because `gp()` is isotropic by default and
# one length-scale then covers every covariate it spans. Measured on
# these three fixtures:
#
#   gp(z) + gp(w, by = cat)   lscale_1[1,1], lscale_2[1,1], lscale_2[2,1]
#   gp(z, w)                  lscale_1[1,1]
#   gp(z, w, by = cat)        lscale_1[1,1], lscale_1[2,1]
#
# So a two-covariate GP is distinguished from a one-covariate GP by
# the surface it predicts and not by its parameter count, which is
# what the middle block below asserts.
#
# The claim that matters for the by-factor shapes is not that the
# model fits but that every route a user can take to a prediction
# carries the per-level contribution. A GP dropped from one path and
# kept in another returns the right shape everywhere and the right
# number nowhere, which is why `assert_by_factor_variation()` drives
# six of them at once rather than one.
#
# These blocks and the fixtures they read came from a file that
# compared each fit against a matched brms one. The comparison went
# because it needed a second fit per fixture to ask a weaker
# question; the sentinel stayed because nothing else in the suite
# asks it, and because a two-dimensional GP has no other coverage.

source("setup_tests_local.R")
source("concordance_helpers.R")

library(marginaleffects)


# One grid per by-factor level, identical in every column but the
# factor. Any difference between the two predictions is therefore
# attributable to the by-factor term and to nothing else.
by_factor_grids <- function(mv, covars) {
  d <- mv$data
  n <- 6L
  grid_A <- data.frame(
    cat = factor("A", levels = levels(d$cat)),
    series = factor(levels(d$series)[1L], levels = levels(d$series)),
    time = seq_len(n),
    grp = "a"
  )
  # Every covariate the model reads has to be present, including the
  # ones this grid holds fixed: `ar1_gp2_by` carries a second `gp(z)`
  # term beside the by-factor one, and a frame without `z` is refused
  # by brms before any prediction is made.
  for (cv in c("z", "w")) grid_A[[cv]] <- mean(d[[cv]])
  for (cv in covars) {
    grid_A[[cv]] <- seq(min(d[[cv]]), max(d[[cv]]), length.out = n)
  }
  # A GP is evaluated at a covariate value, so a grid holding one
  # value would compare two constants and pass on a model that had
  # dropped the covariate entirely.
  for (cv in covars) {
    expect_gt(stats::sd(grid_A[[cv]]), 0)
  }
  grid_B <- grid_A
  grid_B$cat <- factor("B", levels = levels(d$cat))
  list(A = grid_A, B = grid_B)
}


test_that("gp(w, by = cat) reaches every prediction route", {
  require_fixtures("val_mvgam_ar1_gp2_by.rds")
  mv <- load_mvgam("ar1_gp2_by")
  grids <- by_factor_grids(mv, "w")
  # The two levels have to be told apart by posterior_linpred,
  # posterior_epred, fitted, posterior_predict, log_lik and the
  # marginaleffects entry point. A by-factor contribution silently
  # dropped from any one of them fails here and nowhere else.
  assert_by_factor_variation(mv, grids$A, grids$B)
})


test_that("gp(z, w) spans both covariates under one length-scale", {
  require_fixtures("val_mvgam_ar1_gp2d.rds")
  mv <- load_mvgam("ar1_gp2d")
  # `gp()` is isotropic by default, so two covariates share a single
  # length-scale and there is one level to index it by. Counting
  # parameters therefore cannot tell this apart from `gp(z)`, and
  # the surface is what can.
  lscale <- grep("^lscale_", variables(mv), value = TRUE)
  expect_identical(lscale, "lscale_1[1,1]")
  dm <- posterior::as_draws_matrix(mv)
  expect_true(all(as.numeric(dm[, lscale]) > 0))

  # And the surface moves along both covariates. Holding one fixed
  # while the other varies has to change the prediction, in each
  # direction separately.
  d <- mv$data
  base <- data.frame(
    z = mean(d$z), w = mean(d$w),
    series = factor(levels(d$series)[1L], levels = levels(d$series)),
    time = seq_len(6L), grp = "a"
  )
  along_z <- base
  along_z$z <- seq(min(d$z), max(d$z), length.out = 6L)
  along_w <- base
  along_w$w <- seq(min(d$w), max(d$w), length.out = 6L)
  ep_base <- colMeans(posterior_epred(mv, newdata = base, ndraws = 50L))
  ep_z <- colMeans(posterior_epred(mv, newdata = along_z, ndraws = 50L))
  ep_w <- colMeans(posterior_epred(mv, newdata = along_w, ndraws = 50L))
  expect_gt(max(abs(ep_z - ep_base)), 1e-3)
  expect_gt(max(abs(ep_w - ep_base)), 1e-3)
})


test_that("gp(z, w, by = cat) reaches every prediction route", {
  require_fixtures("val_mvgam_ar1_gp2d_by.rds")
  mv <- load_mvgam("ar1_gp2d_by")
  grids <- by_factor_grids(mv, c("z", "w"))
  # Two covariates and two levels at once. Each half is covered
  # above, and a dimension index that only misbehaves when a level
  # index is also present would pass both and fail here.
  assert_by_factor_variation(mv, grids$A, grids$B)
})
