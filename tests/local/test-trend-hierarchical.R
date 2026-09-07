# Recovery and post-fit coverage for a hierarchical trend, fitted in
# this file.
#
# `gr` and `subgr` make mvgam derive the series identifier itself
# rather than read a column, and that derived value has to agree with
# the levels recorded at fit time. When it did not, every post-fit
# method on a hierarchical fit failed at the level validator while
# `summary()` kept working, so nothing in the suite noticed.
#
#   truth: 2 regions x 3 species, 90 occasions, poisson, one AR
#          coefficient per region and species correlated within a
#          region but not across one
#   model: y ~ 1, trend_formula = ~ AR(gr = region, subgr = species,
#                                      cor = TRUE)
#
# Region and species levels are both declared out of alphabetical
# order and occasions are numbered from 3, so a derived axis rebuilt
# from sorted values differs from the one the frame declares and a
# rank never equals a time.
#
# Run with:
#   testthat::test_file("tests/local/test-trend-hierarchical.R")

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

# This file fits its own model and caches it beside itself, so it
# depends on no shared fixture and no build step.
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

set.seed(4021L)

region_levels <- c("south", "north")
species_levels <- c("sp_c", "sp_a", "sp_b")
stopifnot(!identical(region_levels, sort(region_levels)))
stopifnot(!identical(species_levels, sort(species_levels)))

n_time <- 90L
time_vals <- seq_len(n_time) + 2L

# One persistence per region, so a fit that pooled them, or that
# read a region's coefficient under the other's name, lands
# somewhere else. The two are far apart on purpose.
ar_true <- c(south = 0.30, north = 0.75)

# Species within a region share correlated innovations; species in
# different regions do not. That is what `cor = TRUE` under a
# grouping means, and `group_inds_trend` is the only thing that
# carries it.
#
# The two regions are given different within-region correlations on
# purpose. Stan builds each group's correlation as a shared global
# factor combined with a per-group deviation, so two regions that
# correlate alike would be recovered by a model whose deviation was
# identically zero, and nothing would say so.
rho_true <- c(south = 0.70, north = 0.20)
sigma_true <- 0.40

n_series <- length(region_levels) * length(species_levels)
# The order the grouping declares: a region's species sit together,
# region first. Written out rather than derived, so the assertions
# have a statement of intent that did not come from the package.
series_levels <- as.vector(t(outer(region_levels, species_levels,
                                   paste, sep = "_")))

n_sub <- length(species_levels)
chol_for <- function(rho) {
  R <- matrix(rho, n_sub, n_sub)
  diag(R) <- 1
  chol(sigma_true^2 * R)
}

latent <- matrix(0, nrow = n_time, ncol = n_series)
for (g in seq_along(region_levels)) {
  cols <- (g - 1L) * n_sub + seq_len(n_sub)
  phi <- ar_true[[region_levels[g]]]
  L_g <- chol_for(rho_true[[region_levels[g]]])
  for (t in 2:n_time) {
    innov <- as.numeric(crossprod(L_g, rnorm(n_sub)))
    latent[t, cols] <- phi * latent[t - 1L, cols] + innov
  }
}

# Which region each trend column belongs to, in axis order. Read by
# both recovery blocks below, so the mapping is stated once.
region_of_column <- rep(region_levels, each = n_sub)

grid <- expand.grid(
  time = time_vals,
  species = factor(species_levels, levels = species_levels),
  region = factor(region_levels, levels = region_levels),
  stringsAsFactors = FALSE
)
dat <- data.frame(
  time = grid$time,
  region = grid$region,
  species = grid$species
)
# A series column the grouping supersedes, spelled with a dot and
# ordered species-major while the derived axis runs region-major.
# The two are a permutation of one another, which is the shape that
# once handed each series another's trend column, and it is what
# separates a fault in the forecast recursion from a fault in which
# column is read to reach it.
dat$series <- interaction(dat$region, dat$species, drop = TRUE)
stopifnot(!identical(
  levels(dat$series),
  gsub("_", ".", series_levels, fixed = TRUE)
))
# Column order of `latent` follows `series_levels`, which is region
# first then species, and `expand.grid` varies species inside region,
# so the two line up column for column.
# Counts averaging about thirteen rather than three. A latent
# signal of this size is swamped by Poisson noise on small counts,
# and a fit that cannot see the trend shrinks every coefficient
# toward its prior, which would make the recovery claims below pass
# or fail on the intercept rather than on anything structural.
dat$y <- rpois(nrow(dat), exp(2.6 + as.numeric(latent)))

obs_formula <- y ~ 1
trend_spec <- ~ AR(gr = region, subgr = species, cor = TRUE)

sim_truth <- list(
  n_series = n_series, n_time = n_time,
  region_levels = region_levels, species_levels = species_levels,
  series_levels = series_levels, time_vals = time_vals,
  ar_true = ar_true, rho_true = rho_true,
  sigma_true = sigma_true, latent = latent
)

cache <- cache_path("val_mvgam_hier_trend.rds")
if (file.exists(cache)) {
  cat("[cache] Loading hierarchical AR fit.\n")
  fit <- readRDS(cache)
} else {
  cat("[fit ] mvgam(AR(gr = region, subgr = species, cor = TRUE))\n")
  fit <- mvgam(
    formula = obs_formula, trend_formula = trend_spec,
    data = dat, family = poisson(),
    chains = 2L, iter = 1000L, warmup = 500L,
    silent = 2, backend = "cmdstanr"
  )
}
if (!identical(attr(fit, "sim_truth"), sim_truth)) {
  attr(fit, "sim_truth") <- sim_truth
  saveRDS(fit, cache)
}

dm_all <- posterior::as_draws_matrix(fit$fit)


test_that("the derived axis is the one the frame declares", {
  # The whole point of a grouping is that the series identifier is
  # derived rather than read, so the order it comes out in is the
  # claim. `series_levels` above is written out from the level
  # declarations, not recovered from the fit, so a package that
  # rebuilt the axis from sorted values fails here.
  ax <- mvgam:::mvgam_axes(fit)
  expect_identical(as.character(ax$series$levels), series_levels)
  expect_identical(as.integer(ax$time$values), time_vals)
  expect_identical(as.integer(fit$standata$N_series_trend), n_series)
  expect_identical(as.integer(fit$standata$N_time_trend), n_time)
})


test_that("each series recovers its own region's persistence", {
  # `ar1_trend` is declared `vector[N_lv_trend]`, so it is one
  # coefficient per trend column and not one per group. That makes
  # this an axis check as much as a recovery one: columns 1 to 3 are
  # south's species and 4 to 6 are north's, so an axis that mixed the
  # two puts a 0.30 series inside the 0.75 block. Every coefficient
  # stays inside its declared bounds either way.
  expect_identical(as.integer(fit$standata$N_lv_trend), n_series)
  cols <- grep("^ar1_trend\\[", colnames(dm_all), value = TRUE)
  expect_length(cols, n_series)

  phi <- vapply(seq_len(n_series), function(k) {
    mean(dm_all[, paste0("ar1_trend[", k, "]")])
  }, numeric(1))

  # Which block a coefficient belongs to, rather than how closely a
  # 90-occasion poisson panel pins it. The claim is made on the block
  # means and not series by series: shrinkage pulls the estimates
  # toward one another, so a single series can land near the midpoint
  # of the two truths and belong to neither by that measure, which
  # says nothing about the axis.
  #
  # Moving one series into the other block shifts both means by
  # roughly a third of the gap between them, so a permuted axis fails
  # both claims below while leaving every coefficient inside its
  # declared bounds.
  by_region <- split(phi, region_of_column)
  for (r in region_levels) {
    other <- setdiff(region_levels, r)
    expect_lt(abs(mean(by_region[[r]]) - ar_true[[r]]),
              abs(mean(by_region[[r]]) - ar_true[[other]]))
  }
  expect_gt(mean(by_region[["north"]]) - mean(by_region[["south"]]),
            0.2)
})


test_that("the covariance is one block per region, and the blocks differ", {
  # `Sigma_group_trend` is declared as one covariance per group over
  # subgroups, and the program carries no cross-group term at all. So
  # a correlation between species in different regions is not a small
  # number, it is one this model cannot express, and counting the
  # parameters is what says so. Asserting instead that across-region
  # correlation is merely smaller would pass a model fitting a single
  # full 6 by 6 covariance, which is the structure a grouping exists
  # to avoid.
  cols <- grep("^Sigma_group_trend\\[", colnames(dm_all), value = TRUE)
  expect_length(cols, length(region_levels) * n_sub * n_sub)
  expect_length(grep("^Sigma_trend\\[", colnames(dm_all)), 0L)

  block_cor <- function(g) {
    S <- matrix(NA_real_, n_sub, n_sub)
    for (i in seq_len(n_sub)) {
      for (j in seq_len(n_sub)) {
        S[i, j] <- mean(dm_all[, sprintf("Sigma_group_trend[%d,%d,%d]",
                                         g, i, j)])
      }
    }
    expect_equal(unname(S), unname(t(S)), tolerance = 1e-6)
    expect_true(all(eigen(S, only.values = TRUE)$values > 0))
    stats::cov2cor(S)
  }

  rho_hat <- vapply(seq_along(region_levels), function(g) {
    R <- block_cor(g)
    mean(R[upper.tri(R)])
  }, numeric(1))
  names(rho_hat) <- region_levels

  # Each group's correlation is blended with a shared global one at a
  # weight the data have to move, so the recovered contrast is a
  # heavily shrunk version of the simulated one: this frame puts 0.50
  # between the two groups and about 0.19 comes back. The claim is
  # the sign and the ordering, which is what the per-group deviation
  # exists to produce. A model that fitted one shared correlation and
  # copied it into both groups returns the same number twice and
  # fails.
  expect_gt(rho_hat[["south"]] - rho_hat[["north"]], 0.1)
  for (r in region_levels) {
    expect_true(rho_hat[[r]] > 0 && rho_hat[[r]] < 1)
  }
})


test_that("group_inds_trend puts each series in its own group", {
  # This array is the only place two series are told they share a
  # group, so it is what decides which series are correlated with
  # which. A wrong entry pools a series with another region's and
  # costs no error: every index is in range and the model samples.
  sd <- fit$standata
  axes <- mvgam:::mvgam_axes(fit)
  groups <- as.character(axes$series$groups)
  expect_length(groups, as.integer(sd$N_series_trend))

  # Group 1 is the first group to appear on the axis, so the integer
  # each series carries is the position of its own group label.
  expect_identical(as.integer(sd$group_inds_trend),
                   match(groups, unique(groups)))
  # Series in one region share an index; series in different regions
  # do not.
  by_group <- split(seq_along(groups), groups)
  for (idx in by_group) {
    expect_length(unique(as.integer(sd$group_inds_trend)[idx]), 1L)
  }
  expect_length(unique(as.integer(sd$group_inds_trend)),
                as.integer(sd$N_groups_trend))
})


test_that("the group and subgroup counts are the frame's own", {
  # An unbalanced design that is read as balanced draws a slice of a
  # correlation matrix built for more subgroups than the group has,
  # and every index stays in range.
  sd <- fit$standata
  vars <- fit$trend_metadata$variables
  d <- as.data.frame(fit$data)

  n_gr <- length(unique(d[[vars$gr_var]]))
  n_sub <- length(unique(d[[vars$subgr_var]]))
  expect_identical(as.integer(sd$N_groups_trend), n_gr)
  expect_identical(as.integer(sd$N_subgroups_trend), n_sub)

  # Every group carries every subgroup on this frame, so the series
  # count is the product. A design where it is not has to be refused
  # rather than folded into this shape.
  per_group <- tapply(d[[vars$subgr_var]], d[[vars$gr_var]],
                      function(x) length(unique(x)))
  expect_true(all(per_group == n_sub))
  expect_identical(as.integer(sd$N_series_trend), n_gr * n_sub)
})


test_that("the axis keeps each group's subgroups together", {
  # `group_inds_trend` runs in blocks, and it can only do that while
  # a group's subgroups are adjacent on the axis. An axis ordered
  # subgroup-major interleaves the regions, and anything reading the
  # grouping as contiguous then reads across a boundary.
  groups <- as.character(mvgam:::mvgam_axes(fit)$series$groups)
  expect_identical(groups, rep(unique(groups), each = 3L))
  expect_identical(as.integer(fit$standata$group_inds_trend),
                   rep(seq_along(unique(groups)), each = 3L))
})


test_that("the correlation block is sized by subgroup, not by series", {
  # The pooled correlation is between subgroups within a group, so it
  # is `N_subgroups` square. Built at the series dimension it would
  # be six by six and would correlate species across regions, which
  # is a different model that samples perfectly well.
  sd <- fit$standata
  n_sub <- as.integer(sd$N_subgroups_trend)
  dm <- posterior::as_draws_matrix(fit$fit)

  glob <- grep("^L_Omega_global_trend\\[", colnames(dm), value = TRUE)
  expect_length(glob, n_sub * n_sub)
  grp <- grep("^L_Omega_group_trend\\[", colnames(dm), value = TRUE)
  expect_length(grp, n_sub * n_sub * as.integer(sd$N_groups_trend))
})


test_that("the pooled correlation is a correlation matrix", {
  # A Cholesky factor of a correlation matrix is lower triangular
  # with unit-norm rows. Draws that drift off that are not a
  # correlation at all, and the entries stay finite and plausible
  # while every interval computed from them is wrong.
  n_sub <- as.integer(fit$standata$N_subgroups_trend)
  dm <- posterior::as_draws_matrix(fit$fit)

  # Draw by draw. The average of several Cholesky factors is not a
  # Cholesky factor -- its rows lose unit norm -- so a mean taken
  # first would report a violation that is only the averaging.
  draw_L <- function(k) {
    L <- matrix(0, n_sub, n_sub)
    for (i in seq_len(n_sub)) {
      for (j in seq_len(n_sub)) {
        L[i, j] <- dm[k, paste0("L_Omega_global_trend[", i, ",", j, "]")]
      }
    }
    L
  }
  for (k in unique(round(seq(1, nrow(dm), length.out = 10L)))) {
    L <- draw_L(k)
    # Upper triangle is structurally zero.
    expect_equal(L[upper.tri(L)], rep(0, sum(upper.tri(L))))
    # Unit-norm rows, so `L L'` is a correlation matrix.
    Omega <- tcrossprod(L)
    expect_equal(unname(diag(Omega)), rep(1, n_sub), tolerance = 1e-6)
    expect_true(all(abs(Omega[upper.tri(Omega)]) <= 1 + 1e-8))
    expect_true(all(eigen(Omega, only.values = TRUE)$values > -1e-8))
  }

  # The weight blending each group's correlation toward the pooled
  # one is a proportion.
  alpha <- as.numeric(dm[, "alpha_cor_trend"])
  expect_true(all(alpha >= 0 & alpha <= 1))
})


test_that("a shuffled newdata answers the same, in the new order", {
  # A prediction placing rows by position rather than by content
  # agrees with every check that hands back the training frame in its
  # own order, and disagrees here.
  d <- as.data.frame(fit$data)
  set.seed(21L)
  perm <- sample(nrow(d))
  base <- posterior_epred(fit, newdata = d, draw_ids = 1:10,
                          incl_autocor = TRUE)
  shuf <- posterior_epred(fit, newdata = d[perm, , drop = FALSE],
                          draw_ids = 1:10, incl_autocor = TRUE)
  expect_equal(unname(base[, perm, drop = FALSE]), unname(shuf))
})


test_that("a newdata holding one series reads that series' state", {
  # A hierarchical series is rebuilt from `gr` and `subgr` on
  # whatever rows it is handed, so a one-series frame carries only
  # its own grouping levels. An index taken from those levels
  # numbers it 1 whatever it is, and it reads the first series'
  # latent column. A `series` column would have survived subsetting
  # with its levels intact and hidden this.
  d <- as.data.frame(fit$data)
  vars <- fit$trend_metadata$variables
  stated <- paste(d[[vars$gr_var]], d[[vars$subgr_var]], sep = "_")
  levs <- as.character(mvgam:::mvgam_axes(fit)$series$levels)
  full <- posterior_epred(fit, newdata = d, draw_ids = 1:10,
                          incl_autocor = TRUE)
  for (s in levs) {
    rows <- which(stated == s)
    sub <- d[rows, , drop = FALSE]
    sub[[vars$gr_var]] <- droplevels(factor(sub[[vars$gr_var]]))
    sub[[vars$subgr_var]] <- droplevels(factor(sub[[vars$subgr_var]]))
    got <- posterior_epred(fit, newdata = sub, draw_ids = 1:10,
                           incl_autocor = TRUE)
    expect_equal(unname(got), unname(full[, rows, drop = FALSE]))
  }
})


test_that("a hierarchical fit forecasts on its own axis", {
  # The forecast grid is rebuilt from the frame rather than read from
  # the record. This fit carries a `series` column that the grouping
  # superseded, so the two spellings differ and the rebuild has to
  # use the one the model was fitted on.
  #
  # This fails today, and the failure is the finding: the training
  # tail is cut by the raw column rather than by the derived axis, so
  # nothing matches and an empty frame reaches
  # `get_observation_structure()`. Stripping the superseded column
  # from the fit makes the identical call succeed, which is what
  # isolates it. Recorded as finding 13.
  d <- as.data.frame(fit$data)
  vars <- fit$trend_metadata$variables
  levs <- as.character(mvgam:::mvgam_axes(fit)$series$levels)
  stated <- paste(d[[vars$gr_var]], d[[vars$subgr_var]], sep = "_")
  h <- 3L
  last_t <- max(d[[vars$time_var]])

  future <- do.call(rbind, lapply(levs, function(s) {
    proto <- d[which(stated == s)[1L], , drop = FALSE]
    out <- proto[rep(1L, h), , drop = FALSE]
    out[[vars$time_var]] <- last_t + seq_len(h)
    out
  }))
  expect_identical(nrow(future), length(levs) * h)

  fc <- forecast(fit, newdata = future, ndraws = 10L)
  expect_s3_class(fc, "mvgam_forecast")
  expect_identical(names(fc$forecasts), levs)
  for (s in levs) {
    expect_identical(dim(fc$forecasts[[s]]), c(10L, h))
  }
  # Per-series dynamics mean two series do not forecast identically.
  expect_false(isTRUE(all.equal(fc$forecasts[[1L]], fc$forecasts[[2L]])))
})


test_that("the forecast grid is cut by the axis, not by the column", {
  # The same fit and the same future frame, with the superseded
  # `series` column absent. The grouping is then the only spelling of
  # the axis, so the training tail is cut by the identity the model
  # was fitted on. This separates a fault in the hierarchical
  # forecast recursion from a fault in which column is read to reach
  # it, and it is the only route by which this fixture forecasts.
  d <- as.data.frame(fit$data)
  vars <- fit$trend_metadata$variables
  levs <- as.character(mvgam:::mvgam_axes(fit)$series$levels)
  stated <- paste(d[[vars$gr_var]], d[[vars$subgr_var]], sep = "_")
  h <- 3L
  last_t <- max(d[[vars$time_var]])

  bare <- fit
  bare$data[[vars$series_var]] <- NULL
  if (!is.null(bare$obs_data)) {
    bare$obs_data[[vars$series_var]] <- NULL
  }
  future <- do.call(rbind, lapply(levs, function(s) {
    proto <- d[which(stated == s)[1L], , drop = FALSE]
    out <- proto[rep(1L, h), , drop = FALSE]
    out[[vars$time_var]] <- last_t + seq_len(h)
    out[[vars$series_var]] <- NULL
    out
  }))

  fc <- forecast(bare, newdata = future, ndraws = 20L)
  expect_identical(names(fc$forecasts), levs)
  for (s in levs) {
    expect_identical(dim(fc$forecasts[[s]]), c(20L, h))
    expect_true(all(is.finite(fc$forecasts[[s]])))
  }
  expect_false(isTRUE(all.equal(fc$forecasts[[1L]], fc$forecasts[[2L]])))
})


test_that("the one-step trend forecast follows this fit's own AR", {
  # Shape and keying say the forecast reached the right series; they
  # say nothing about whether the number is right. An AR(1) trend
  # steps as `ar1[s] * trend[T, s]` plus a mean-zero innovation, so
  # averaged over draws the first forecast step has to land on that
  # product. A recursion applied with another series' coefficient,
  # or started from another series' last state, gives a finite
  # trajectory of the right width and fails only here.
  d <- as.data.frame(fit$data)
  vars <- fit$trend_metadata$variables
  levs <- as.character(mvgam:::mvgam_axes(fit)$series$levels)
  stated <- paste(d[[vars$gr_var]], d[[vars$subgr_var]], sep = "_")
  h <- 1L
  last_t <- max(d[[vars$time_var]])
  n_time <- as.integer(fit$standata$N_time_trend)

  bare <- fit
  bare$data[[vars$series_var]] <- NULL
  if (!is.null(bare$obs_data)) {
    bare$obs_data[[vars$series_var]] <- NULL
  }
  future <- do.call(rbind, lapply(levs, function(s) {
    proto <- d[which(stated == s)[1L], , drop = FALSE]
    out <- proto[rep(1L, h), , drop = FALSE]
    out[[vars$time_var]] <- last_t + seq_len(h)
    out[[vars$series_var]] <- NULL
    out
  }))

  n_draw <- 600L
  fc <- forecast(bare, newdata = future, ndraws = n_draw, type = "trend")
  dm <- posterior::as_draws_matrix(fit$fit)
  for (k in seq_along(levs)) {
    ar1 <- as.numeric(dm[, paste0("ar1_trend[", k, "]")])
    last <- as.numeric(dm[, paste0("trend[", n_time, ",", k, "]")])
    expected <- mean(ar1 * last)
    got <- mean(fc$forecasts[[levs[k]]][, 1L])
    # The innovation is mean zero, so the gap is Monte Carlo noise on
    # the innovation scale rather than a systematic offset.
    expect_lt(abs(got - expected), 0.35)
  }
})


test_that("summary, residual_cor and shared_variation name the axis", {
  # Everything a user reads off a hierarchical fit is labelled by the
  # derived identity rather than by the column it superseded. A
  # correct estimate under the wrong label is indistinguishable, to
  # the reader, from a wrong estimate.
  levs <- as.character(mvgam:::mvgam_axes(fit)$series$levels)

  txt <- capture.output(summary(fit))
  expect_gt(length(txt), 10L)
  expect_true(any(grepl(paste0("Series:\\s*", length(levs)), txt)))

  # The pooled correlation is between subgroups within a group, so
  # it is the subgroup set that labels it rather than the six series.
  # A matrix built at the series dimension would correlate species
  # across regions, and the labels are how a reader tells which.
  vars <- fit$trend_metadata$variables
  subgr <- levels(factor(fit$data[[vars$subgr_var]]))
  rc <- residual_cor(fit)
  expect_identical(rownames(rc$cor), subgr)
  expect_identical(colnames(rc$cor), subgr)
  expect_identical(dim(rc$cor),
                   c(length(subgr), length(subgr)))
  expect_equal(unname(diag(rc$cor)), rep(1, length(subgr)))
  expect_equal(unname(rc$cor), unname(t(rc$cor)))

  # This fit has no latent factors, so the shared-variation
  # decomposition has nothing to decompose and says so rather than
  # returning a zero it would be read as a result.
  expect_error(shared_variation(fit), "requires a latent-factor fit")
})


test_that("the tidiers keep this fit's own row order", {
  # A tidier that re-sorts its output pairs each fitted value with
  # another row's observation while every column keeps its length.
  d <- as.data.frame(fit$data)
  vars <- fit$trend_metadata$variables

  resp <- fit$response_names[1L]
  aug <- augment(fit)
  expect_true(is.data.frame(aug))
  expect_identical(nrow(aug), nrow(d))
  expect_equal(as.numeric(aug[[vars$time_var]]),
               as.numeric(d[[vars$time_var]]))
  expect_equal(as.numeric(aug$.observed), as.numeric(d[[resp]]))
  # The grouping columns travel with the row, so a reader can tell
  # which series each fitted value belongs to.
  expect_identical(as.character(aug[[vars$gr_var]]),
                   as.character(d[[vars$gr_var]]))
  expect_identical(as.character(aug[[vars$subgr_var]]),
                   as.character(d[[vars$subgr_var]]))

  expect_true(is.data.frame(tidy(fit)))
  expect_true(is.data.frame(glance(fit)))
  expect_true(any(grepl("^ar1_trend\\[", variables(fit))))
})


test_that("every series gets a panel, and the panels are named", {
  # Six series on a derived axis. The trend plot draws all six and
  # names them, so the frame carries everything the series plot needs.
  p <- plot(fit, type = "series")
  b <- ggplot2::ggplot_build(p)
  lay <- b$layout$layout
  strip_col <- intersect(c("series", "trend"), names(lay))[1L]
  labs <- as.character(lay[[strip_col]])
  expect_identical(nrow(lay), as.integer(n_series))
  expect_false(any(is.na(labs)))
  expect_setequal(labs, series_levels)
})


test_that("the trend panels follow the axis the fit was built on", {
  # Placed beside the series plot, a trend panel has to hold the same
  # series in the same position. The axis is derived here, so its
  # order is the grouping's rather than the alphabet's.
  p <- plot(fit, type = "trend")
  lay <- ggplot2::ggplot_build(p)$layout$layout
  strip_col <- intersect(c("series", "trend"), names(lay))[1L]
  expect_identical(as.character(lay[[strip_col]]), series_levels)
})


test_that("insight reports the terms this model actually has", {
  # The observation formula is `y ~ 1`, so there is no predictor to
  # report. `region` and `species` are the grouping the trend is
  # built on and `time` and `series` are the axis, and a consumer
  # handed any of them will offer a slope over an occasion number or
  # a contrast between two regions.
  preds <- insight::find_predictors(fit)$conditional
  expect_null(preds)

  # The pair a caller reaches for together.
  expect_s3_class(model.frame(fit), "data.frame")
  expect_true(inherits(terms(fit), "terms"))
})


test_that("the plotting methods render for a hierarchical fit", {
  for (ty in c("residuals", "trend", "series")) {
    # `plot()` returns a ggplot, so that is the class asserted. An
    # `is.list()` check would pass on any method returning `list()`.
    p <- plot(fit, type = ty)
    expect_s3_class(p, "ggplot")
  }
  expect_s3_class(mcmc_plot(fit), "ggplot")
})


test_that("conditional_effects has nothing to condition on here", {
  # The observation formula is intercept-only, so there is no
  # predictor to vary and the answer is no effects at all rather
  # than an empty panel. The formula is asserted alongside it, so a
  # fixture that gains a covariate fails here instead of quietly
  # turning this into a check of nothing.
  expect_identical(deparse1(stats::formula(fit$formula)), "y ~ 1")

  ce <- conditional_effects(fit)
  expect_s3_class(ce, "mvgam_conditional_effects")
  expect_length(ce, 0L)
})


test_that("each prediction type answers with the quantity it names", {
  # mvgam separates the two things brms and marginaleffects both
  # spell `"response"`: `"expected"` is the family's mean, and
  # `"response"` samples from the observation family and reports its
  # median, so on this poisson fit it lands on a whole number by
  # contract. Pinning the three together is what makes the check
  # bite, since a type answering with another's quantity satisfies
  # any check made on one type alone.
  withr::local_options(marginaleffects_model_classes = "mvgam")
  d <- as.data.frame(fit$data)
  vars <- fit$trend_metadata$variables
  keyed <- paste(d[[vars$gr_var]], d[[vars$subgr_var]])
  grid <- d[!duplicated(keyed), , drop = FALSE]

  ask <- function(ty) {
    as.numeric(marginaleffects::predictions(
      fit, newdata = grid, type = ty
    )$estimate)
  }
  ep <- colMeans(posterior_epred(fit, newdata = grid, ndraws = 400L))
  lp <- colMeans(posterior_linpred(fit, newdata = grid, ndraws = 400L))
  med <- apply(posterior_predict(fit, newdata = grid, ndraws = 400L),
               2L, stats::median)

  expect_length(ask("expected"), nrow(grid))
  expect_equal(ask("expected"), as.numeric(ep), tolerance = 0.05)
  expect_equal(ask("link"), as.numeric(lp), tolerance = 0.05)
  expect_equal(ask("response"), as.numeric(med), tolerance = 0.05)

  # A count drawn from the observation family is a whole number, and
  # its expectation is not, so the two types cannot be confused.
  expect_true(all(ask("response") == floor(ask("response"))))
  expect_true(any(abs(ask("expected") - round(ask("expected"))) > 1e-8))
  expect_true(all(ask("expected") > 0))
})


test_that("feeding the training data back as newdata is a no-op", {
  # Compared on the deterministic path. With `process_error = TRUE`
  # the expectation is marginalised over the trend by drawing fresh
  # innovations, so two calls differ by construction.
  bare <- posterior_epred(fit, draw_ids = 1:5, process_error = FALSE)
  with_nd <- posterior_epred(
    fit, newdata = fit$data, draw_ids = 1:5, process_error = FALSE
  )
  expect_equal(dim(bare), dim(with_nd))
  expect_equal(bare, with_nd)

  lp <- posterior_linpred(fit, draw_ids = 1:5, process_error = FALSE)
  lp_nd <- posterior_linpred(
    fit, newdata = fit$data, draw_ids = 1:5, process_error = FALSE
  )
  expect_equal(lp, lp_nd)
})


test_that("the marginal expectation redraws innovations each call", {
  # `process_error = TRUE` integrates over the trend by Monte Carlo,
  # so repeating the call on the same draws gives a different answer.
  # Reproducible output needs an explicit seed. The argument has to be
  # passed to get that: the default leaves the trend at its
  # deterministic submodel, and this asserted a difference without it,
  # so it had been comparing one fixed answer against itself.
  a <- posterior_epred(fit, draw_ids = 1:5, process_error = TRUE)
  b <- posterior_epred(fit, draw_ids = 1:5, process_error = TRUE)
  expect_false(isTRUE(all.equal(a, b)))

  # The default is the other half of the contract, and it is the half
  # a user relies on when they call the same surface twice.
  expect_equal(
    posterior_epred(fit, draw_ids = 1:5),
    posterior_epred(fit, draw_ids = 1:5)
  )
})


test_that("kfold refits a fold without rebuilding the time grid", {
  # A fold holds rows out, and a held-out row is a missing response
  # rather than a missing occasion. `mvgam()` already draws that
  # distinction: an `NA` response shrinks the likelihood and leaves
  # `N_time_trend` alone. The refit rebuilds the axis from the subset
  # frame instead, so the trend meets a grid with holes in it and
  # refuses the fold it was asked to fit.
  kf <- kfold(fit, K = 2L)
  expect_true(is.finite(kf$estimates["elpd_kfold", "Estimate"]))
})


test_that("the criticism surface runs on a hierarchical fit", {
  # Warnings are captured and asserted rather than swept away: a
  # Pareto-k notice is the one diagnostic that says whether the loo
  # approximation holds, and this frame has no missing responses, so
  # the plotting calls owe no notice at all.
  loo_warnings <- character(0)
  ic <- withCallingHandlers(
    loo(fit),
    warning = function(w) {
      loo_warnings <<- c(loo_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_s3_class(ic, "loo")
  expect_true(is.finite(ic$estimates["elpd_loo", "Estimate"]))
  pareto_k <- ic$diagnostics$pareto_k
  expect_true(all(is.finite(pareto_k)))
  # A high Pareto-k is what a latent state-space fit is expected to
  # produce: dropping an observation moves the very state it is being
  # scored against, which is the reason `lfo_cv()` exists. So the
  # claim is not that the diagnostic is good, it is that the user is
  # told the truth about it. The notice has to arrive exactly when
  # there is something to report, which fails both on a `loo()` gone
  # silent over bad draws and on one that cries out over good ones.
  expect_identical(
    any(pareto_k > 0.7),
    any(grepl("Pareto k", loo_warnings))
  )
  # Whatever was raised is that notice and nothing else, so an
  # unrelated warning cannot hide among the expected ones.
  expect_true(all(grepl("Pareto k", loo_warnings)))

  # A ggplot comes back whether or not a layer received data, so the
  # object is built and its layers required to hold rows.
  for (p in list(pp_check(fit, ndraws = 10L),
                 pp_check(fit, type = "resid_qq", ndraws = 50L))) {
    expect_s3_class(p, "ggplot")
    layers <- ggplot2::ggplot_build(p)$data
    expect_gt(sum(vapply(layers, nrow, integer(1L))), 0L)
  }

  hc <- hindcast(fit, ndraws = 5L)
  expect_s3_class(hc, "mvgam_forecast")
  expect_equal(length(hc$hindcasts), fit$series_info$n_series)
})


test_that("a grouping combination absent from training is rejected", {
  vars <- fit$trend_metadata$variables
  nd <- fit$data
  # Each level below is known on its own, but this pairing never
  # appeared, so the derived series identifier is new.
  nd[[vars$subgr_var]] <- "sp_unseen"
  expect_error(
    posterior_epred(fit, newdata = nd, ndraws = 5L),
    "Series levels in newdata not found in training data"
  )
})


test_that("the derived identifier follows the declared level order", {
  # The contract is the order each column declares its own levels in,
  # grouping variable first, joined by an underscore, with a group's
  # subgroups adjacent. It is not alphabetical order: a frame whose
  # levels happen to sort that way cannot tell the two apart, and
  # both this frame's columns are declared out of alphabetical order
  # so that it can.
  vars <- fit$trend_metadata$variables
  vals <- mvgam:::hierarchical_series_values(
    fit$data, vars$gr_var, vars$subgr_var
  )
  expect_true(all(grepl("_", levels(vals), fixed = TRUE)))
  expect_identical(levels(vals), series_levels)
  expect_false(identical(levels(vals), sort(levels(vals))))
  # And it is the order the fit recorded, so what a user reads back
  # names the column each series occupies.
  expect_identical(as.character(mvgam:::mvgam_axes(fit)$series$levels),
                   levels(vals))
})


test_that("a superseded series column warns once, and obeys silent", {
  vars <- fit$trend_metadata$variables
  derived <- mvgam:::hierarchical_series_values(
    fit$data, vars$gr_var, vars$subgr_var
  )
  # The warning is silent under TESTTHAT so the suite stays quiet;
  # clear it here to exercise the path a user actually meets.
  withr::local_envvar(TESTTHAT = "")

  count_warnings <- function() {
    n <- 0L
    withCallingHandlers(
      mvgam:::warn_series_superseded(
        fit$data, "series", derived, vars$gr_var, vars$subgr_var
      ),
      warning = function(w) {
        n <<- n + 1L
        invokeRestart("muffleWarning")
      }
    )
    n
  }

  # One notice per event: insight::format_warning() would raise its
  # own on top of the one rlang::warn() emits.
  withr::local_options(mvgam.silent = 0L)
  expect_equal(count_warnings(), 1L)

  rlang::reset_warning_verbosity("mvgam_series_superseded")
  withr::local_options(mvgam.silent = 2L)
  expect_equal(count_warnings(), 0L)
})


test_that("a series column matching the derived one is left alone", {
  vars <- fit$trend_metadata$variables
  derived <- mvgam:::hierarchical_series_values(
    fit$data, vars$gr_var, vars$subgr_var
  )
  dat <- fit$data
  dat$series <- derived
  withr::local_envvar(TESTTHAT = "")
  rlang::reset_warning_verbosity("mvgam_series_superseded")
  expect_silent(
    mvgam:::warn_series_superseded(
      dat, "series", derived, vars$gr_var, vars$subgr_var
    )
  )
})


test_that("the resolved series index is the one the fit sampled with", {
  # `standata$obs_trend_series` is the column index the fit gave each
  # training row, so it settles what `trend[t, s]` means. Deriving
  # that order a second time from the data is how a prediction comes
  # to disagree with it, and on a hierarchical fit the two derivations
  # are a permutation of one another: the rebuilt series sorts
  # region-major, the column it superseded sorts otherwise. Both carry
  # every label, so the mismatch raises nothing and every series reads
  # another series' state. This test reads the record rather than
  # asking two derivations whether they agree with each other.
  d <- as.data.frame(fit$data)
  recorded <- as.integer(fit$standata$obs_trend_series)
  expect_length(recorded, nrow(d))
  expect_gt(length(unique(recorded)), 1L)

  resolved <- as.integer(
    mvgam:::get_observation_structure(fit, newdata = d)$series_int
  )
  expect_identical(resolved, recorded)

  # And the state each row reads is the cell Stan sampled for it.
  dm <- posterior::as_draws_matrix(fit$fit)
  t_rec <- as.integer(fit$standata$obs_trend_time)
  want <- vapply(seq_len(nrow(d)), function(j) {
    mean(dm[, paste0("trend[", t_rec[j], ",", recorded[j], "]")])
  }, numeric(1))
  got <- colMeans(
    mvgam:::extract_trend_latent_states(fit, newdata = d, full_draws = dm)
  )
  expect_equal(unname(got), unname(want))
})


test_that("each series reads its own latent state, not the first one's", {
  # A prediction frame holding one series is the shape every
  # per-series hindcast arm passes down, and it is the shape that
  # exposed the defect: the series index was read off the levels the
  # frame carried rather than the levels the model was fitted on, so
  # a single-series frame numbered its one series 1 whatever it was.
  # A `series` column survives subsetting with its levels intact and
  # so hid this; a hierarchical series, rebuilt from `gr` and `subgr`
  # on whatever rows it is handed, does not.
  d <- as.data.frame(fit$data)
  os <- mvgam:::get_observation_structure(fit, newdata = d)
  levs <- os$series_levels
  expect_gt(length(levs), 1L)

  resolved <- vapply(seq_along(levs), function(k) {
    rows <- which(as.character(os$series) == levs[k])
    unique(mvgam:::get_observation_structure(
      fit, newdata = d[rows, , drop = FALSE]
    )$series_int)
  }, integer(1L))
  expect_identical(resolved, seq_along(levs))
})


test_that("a hierarchical hindcast agrees with the conditional epred", {
  # `hindcast()` and `posterior_epred(incl_autocor = TRUE)` are two
  # routes to one quantity: the mean given the latent state the model
  # inferred. They compose it differently, `hindcast()` per series
  # and `posterior_epred()` across the whole frame, so they agree
  # only when both resolve the same state for the same cell.
  d <- as.data.frame(fit$data)
  time_var <- fit$trend_metadata$variables$time_var

  hc <- hindcast(fit, type = "expected")
  blocks <- hc$hindcasts
  expect_gt(length(blocks), 1L)

  lab <- mvgam:::training_series_labels(fit, d)
  cells <- unlist(lapply(names(blocks), function(s) {
    rows <- which(lab == s)
    rows[order(d[[time_var]][rows])]
  }))
  ep <- posterior_epred(fit, incl_autocor = TRUE)
  expect_equal(
    unname(ep[, cells, drop = FALSE]),
    unname(do.call(cbind, blocks))
  )

  # Every series carrying the first one's state is what the defect
  # looked like, and it agreed on shape throughout. Two series of a
  # fit with per-series dynamics do not hindcast identically.
  expect_false(isTRUE(all.equal(blocks[[1L]], blocks[[2L]])))
})


test_that("the axis maps a hierarchical newdata with no draws at all", {
  # The completeness claim, on the frame that has no series column to
  # fall back to. A `gr` / `subgr` model derives the identifier from
  # two columns, so every draw-free resolver has to rebuild it rather
  # than read it, and each of these was a layer that once refused
  # such a frame outright.
  d <- as.data.frame(fit$data)
  vars <- fit$trend_metadata$variables
  levs <- as.character(mvgam:::mvgam_axes(fit)$series$levels)
  stated <- paste(d[[vars$gr_var]], d[[vars$subgr_var]], sep = "_")

  ids <- mvgam:::axis_row_series(fit, d)
  expect_identical(levels(ids), levs)
  expect_identical(as.character(ids), stated)

  # Row by row, not by position.
  set.seed(61L)
  perm <- sample(nrow(d))
  expect_identical(
    as.character(mvgam:::axis_row_series(fit, d[perm, , drop = FALSE])),
    stated[perm]
  )

  # And with the superseded column gone, which is the only spelling
  # a user of this model needs to supply.
  bare_d <- d
  bare_d[[vars$series_var]] <- NULL
  expect_false(vars$series_var %in% names(bare_d))
  expect_identical(as.character(mvgam:::axis_row_series(fit, bare_d)),
                   stated)

  # The training arms are cut by the derived identity.
  training <- mvgam:::build_training_arms(fit, levs)
  expect_identical(names(training$times), levs)
  user_times <- sort(unique(as.integer(d[[vars$time_var]])))
  for (s in levs) {
    expect_identical(as.integer(training$times[[s]]), user_times)
    expect_length(training$observations[[s]], length(user_times))
  }
  # Each arm holds its own series' observations, in time order.
  resp <- fit$response_names[1L]
  for (s in levs) {
    rows <- which(stated == s)
    rows <- rows[order(d[[vars$time_var]][rows])]
    expect_equal(training$observations[[s]],
                 as.numeric(d[[resp]][rows]))
  }

  # A frame past the training grid yields the new occasions per
  # series; one inside it yields none.
  h <- 4L
  future_times <- max(user_times) + seq_len(h)
  future <- do.call(rbind, lapply(levs, function(s) {
    proto <- d[which(stated == s)[1L], , drop = FALSE]
    out <- proto[rep(1L, h), , drop = FALSE]
    out[[vars$time_var]] <- future_times
    out
  }))
  grid <- mvgam:::resolve_forecast_grid(fit, future, training, levs)
  expect_false(is.null(grid))
  expect_identical(names(grid$times), levs)
  for (s in levs) {
    expect_identical(as.integer(grid$times[[s]]), future_times)
  }
  expect_null(mvgam:::resolve_forecast_grid(fit, d, training, levs))
})


test_that("a newdata holding a subset of series reads each of them", {
  # One series at a time is already covered. A frame carrying some
  # but not all of them is what separates an axis read off the record
  # from one rebuilt out of the groupings present, because here the
  # levels in hand are a proper subset in a different order.
  d <- as.data.frame(fit$data)
  vars <- fit$trend_metadata$variables
  levs <- as.character(mvgam:::mvgam_axes(fit)$series$levels)
  stated <- paste(d[[vars$gr_var]], d[[vars$subgr_var]], sep = "_")
  full <- posterior_epred(fit, newdata = d, draw_ids = 1:10,
                          incl_autocor = TRUE)

  # Two cuts: one within a single group, one spanning both, so a
  # resolver that happens to work block-wise is not let through.
  subsets <- list(levs[c(1L, 2L)], levs[c(5L, 1L, 4L)])
  for (subset in subsets) {
    rows <- which(stated %in% subset)
    sub <- d[rows, , drop = FALSE]
    sub[[vars$gr_var]] <- droplevels(factor(sub[[vars$gr_var]]))
    sub[[vars$subgr_var]] <- droplevels(factor(sub[[vars$subgr_var]]))
    got <- posterior_epred(fit, newdata = sub, draw_ids = 1:10,
                           incl_autocor = TRUE)
    expect_equal(unname(got), unname(full[, rows, drop = FALSE]))
  }
})


test_that("how the grouping columns are typed does not move an answer", {
  # The identifier is rebuilt from `gr` and `subgr`, so the way those
  # two columns happen to be stored is exactly the sort of thing that
  # can leak into the axis. Character columns are what a user gets
  # from a frame built without `stringsAsFactors`; a reversed level
  # order is what a user gets from `factor(levels = ...)`; a level
  # with no rows is what survives subsetting a larger frame.
  d <- as.data.frame(fit$data)
  vars <- fit$trend_metadata$variables
  base <- posterior_epred(fit, newdata = d, draw_ids = 1:10,
                          incl_autocor = TRUE)

  chr <- d
  chr[[vars$gr_var]] <- as.character(chr[[vars$gr_var]])
  chr[[vars$subgr_var]] <- as.character(chr[[vars$subgr_var]])
  expect_type(chr[[vars$gr_var]], "character")
  expect_equal(
    unname(posterior_epred(fit, newdata = chr, draw_ids = 1:10,
                           incl_autocor = TRUE)),
    unname(base)
  )

  rev_nd <- d
  rev_nd[[vars$subgr_var]] <- factor(
    as.character(rev_nd[[vars$subgr_var]]),
    levels = rev(levels(factor(d[[vars$subgr_var]])))
  )
  expect_equal(
    unname(posterior_epred(fit, newdata = rev_nd, draw_ids = 1:10,
                           incl_autocor = TRUE)),
    unname(base)
  )

  # A declared level with no rows is refused here, which is the
  # opposite of what the same shape does on a fit whose series is a
  # column: there an unused level is carried without comment. The
  # refusal is asserted as the contract this path actually has, and
  # it is a good refusal, naming the level and listing the ones that
  # would have worked. Whether the two paths ought to differ is the
  # open question recorded in FINDINGS.md, not something to settle by
  # writing the assertion either way.
  extra <- d
  extra[[vars$subgr_var]] <- factor(
    as.character(extra[[vars$subgr_var]]),
    levels = c(levels(factor(d[[vars$subgr_var]])), "sp_absent")
  )
  expect_identical(sum(extra[[vars$subgr_var]] == "sp_absent"), 0L)
  err <- expect_error(
    posterior_epred(fit, newdata = extra, draw_ids = 1:10,
                    incl_autocor = TRUE),
    "levels not in training data"
  )
  expect_match(conditionMessage(err), "sp_absent", fixed = TRUE)
  for (lv in levels(factor(d[[vars$subgr_var]]))) {
    expect_match(conditionMessage(err), lv, fixed = TRUE)
  }
})


test_that("single and repeated rows read the cell they name", {
  # The smallest frames there are. A mapping that numbers series by
  # order of appearance is right by accident for whichever series
  # comes first and wrong for the rest, and duplicate rows are what
  # every prediction grid is built from.
  d <- as.data.frame(fit$data)
  vars <- fit$trend_metadata$variables
  levs <- as.character(mvgam:::mvgam_axes(fit)$series$levels)
  stated <- paste(d[[vars$gr_var]], d[[vars$subgr_var]], sep = "_")
  full <- posterior_epred(fit, newdata = d, draw_ids = 1:10,
                          incl_autocor = TRUE)

  for (s in levs) {
    j <- which(stated == s)[1L]
    one <- posterior_epred(fit, newdata = d[j, , drop = FALSE],
                           draw_ids = 1:10, incl_autocor = TRUE)
    expect_identical(dim(one), c(10L, 1L))
    expect_equal(unname(one), unname(full[, j, drop = FALSE]))
  }

  j <- which(stated == levs[4L])[1L]
  rep_nd <- d[c(j, j, j), , drop = FALSE]
  got <- posterior_epred(fit, newdata = rep_nd, draw_ids = 1:10,
                         incl_autocor = TRUE)
  expect_identical(dim(got), c(10L, 3L))
  expect_equal(unname(got[, 1L]), unname(got[, 2L]))
  expect_equal(unname(got[, 1L]), unname(full[, j]))
})


test_that("a newdata holding one occasion reads that occasion", {
  # The complement of the one-series cut: hold the series and cut the
  # time axis. An AR trend indexes `trend[t, s]` by both, so a cut
  # that renumbers the occasions from 1 reads the wrong rows.
  d <- as.data.frame(fit$data)
  vars <- fit$trend_metadata$variables
  full <- posterior_epred(fit, newdata = d, draw_ids = 1:10,
                          incl_autocor = TRUE)
  times <- sort(unique(d[[vars$time_var]]))
  for (tv in times[c(1L, 2L, length(times))]) {
    rows <- which(d[[vars$time_var]] == tv)
    expect_gt(length(rows), 0L)
    got <- posterior_epred(fit, newdata = d[rows, , drop = FALSE],
                           draw_ids = 1:10, incl_autocor = TRUE)
    expect_equal(unname(got), unname(full[, rows, drop = FALSE]))
  }
})


test_that("the tidiers agree on the group covariance block", {
  # `Sigma_group_trend` is the covariance a hierarchical trend exists
  # to estimate: one block per region, over the species. Four methods
  # name parameters, three of them report it, and the claim is that
  # the fourth does too.
  sig <- function(x) grep("^Sigma", x, value = TRUE)
  from_vars <- sig(variables(fit))
  # One square block per group, so the count states the shape rather
  # than merely that something is present.
  n_sub <- length(species_levels)
  expect_length(from_vars, length(region_levels) * n_sub^2)
  expect_setequal(sig(rownames(posterior_summary(fit))), from_vars)
  expect_setequal(sig(names(rhat(fit))), from_vars)

  td <- tidy(fit, effects = "all")
  expect_setequal(sig(td$term), from_vars)
  # The tidier does reach the trend block generally, so this is about
  # one parameter family rather than about the trend side being
  # invisible to it.
  expect_true(any(grepl("^sigma_group_trend", td$term)))
  expect_true(any(grepl("^ar1_trend", td$term)))
})


# ----- residual_cor on a hierarchical trend --------------------------
#
# The correlation block a grouping implies is over the subgroups, and
# it is per group. Both facts are invisible to a dimension check on a
# frame whose series count happens to match.

test_that("residual_cor returns the global block by default", {
  res <- residual_cor(fit)
  expect_s3_class(res, "mvgam_residcor")
  expect_true(isTRUE(res$hierarchical))
  expect_identical(res$group_label, "_global")
  # The matrix is over subgroups, not over series: three species, not
  # the six series the grouping produces. A block sized by the series
  # axis would be 6 x 6 and pass any symmetry or bounds check.
  sp <- levels(dat$species)
  expect_identical(dim(res$cor), c(length(sp), length(sp)))
  expect_false(nrow(res$cor) == nlevels(dat$series))
  expect_true(all(diag(res$cor) == 1))
  expect_true(isSymmetric(unname(res$cor), tol = 1e-8))
  # Labelled by the frame's own species levels, in its order.
  expect_identical(rownames(res$cor), sp)
  expect_identical(colnames(res$cor), sp)
})


test_that("residual_cor(by_group = TRUE) returns one block per region", {
  res <- residual_cor(fit, by_group = TRUE)
  regions <- levels(dat$region)
  sp <- levels(dat$species)
  expect_type(res, "list")
  expect_setequal(names(res), c("_global", regions))
  for (nm in names(res)) {
    expect_s3_class(res[[nm]], "mvgam_residcor")
    expect_identical(dim(res[[nm]]$cor), c(length(sp), length(sp)))
    expect_identical(rownames(res[[nm]]$cor), sp)
    expect_identical(colnames(res[[nm]]$cor), sp)
    expect_true(all(diag(res[[nm]]$cor) == 1))
  }
  # The regions were given different within-region correlations, so
  # returning the global block under each region's name would satisfy
  # every claim above and mean no region was distinguished.
  off <- upper.tri(res[[regions[1L]]]$cor)
  expect_false(isTRUE(all.equal(res[[regions[1L]]]$cor[off],
                                res[[regions[2L]]]$cor[off])))
  expect_false(isTRUE(all.equal(res[["_global"]]$cor[off],
                                res[[regions[1L]]]$cor[off])))
})
