# `s(x, by = lv_axis())` puts the trend design on the latent-factor
# axis rather than the series axis, which is the one place the second
# dimension of `times_trend` means something other than a series.
#
# The file builds its own data and its own two fits so it has a
# producer: a `by = lv_axis()` model and the same model without it,
# which is the contrast that says the design moved axis rather than
# merely compiling. Fits are cached in tests/local/fixtures/.
#
#   data: n_time = 60, n_species = 5, n_lv = 2, gaussian
#         obs side  y ~ region + depth, a factor and a slope
#         species named out of alphabetical order, rows shuffled,
#         8 per cent of rows dropped so the species are unbalanced,
#         occasions numbered from 3 so a value is not its own rank
#   by_lv    : trend_formula = ~ s(elev, k = 5, by = lv_axis()) - 1
#              + ZMVN(cor = TRUE), trend_map = matrix(NA, 5, 2)
#   no_by_lv : the same without `by = lv_axis()`
#
# Run with:
#   testthat::test_file("tests/local/test-factor-lv-axis.R")

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

set.seed(700L)

n_time <- 60L
n_species <- 5L
N_lv <- 2L

# The species are named so that no two rival resolvers agree by
# accident. Declared order is neither alphabetical (which would be
# ant, beetle, crane, moth, zebra) nor order of first appearance
# (the frame is shuffled below). A fit taking its axis from
# `sort(unique(...))`, from `factor()`'s own defaults or from the
# order rows happen to arrive in therefore gives three different
# answers, and only the recorded one is right.
#
# Every other fixture uses `sp1..spK`, where all three coincide.
species_levels <- c("zebra", "ant", "moth", "beetle", "crane")
stopifnot(!identical(species_levels, sort(species_levels)))

# `elev` drives both latent factors through different smooth shapes,
# so the two factors are separable and the loadings are identified up
# to the usual rotation.
elev <- as.numeric(scale(sin(seq_len(n_time) / 6) +
                           rnorm(n_time, 0, 0.2)))
lv_true <- cbind(sin(elev), elev^2 - mean(elev^2))
lv_true <- scale(lv_true, center = TRUE,
                 scale = apply(lv_true, 2L, sd))
Z_true <- matrix(rnorm(n_species * N_lv, sd = 0.8),
                 nrow = n_species, ncol = N_lv)
mu_true <- lv_true %*% t(Z_true)

# The occasions are numbered from 3, not from 1, so a raw time value
# never equals its own rank. Every other local fixture is built on
# `time = 1..n`, where the two coincide and a function returning the
# index where it was asked for the value is indistinguishable from a
# correct one.
time_vals <- seq_len(n_time) + 2L

# Observation-side predictors, so the frame is not a bare intercept
# over a trend. `region` is a factor whose levels are again declared
# out of alphabetical order, and `depth` is a second continuous
# covariate. Between them the design matrix carries contrasts and a
# slope, which is what `conditional_effects()` and `marginaleffects`
# have to cut by the right axis.
region_levels <- c("north", "east", "central")
beta_region <- c(north = 0, east = 0.6, central = -0.4)
beta_depth <- 0.35

dat <- data.frame(
  time = rep(time_vals, n_species),
  series = factor(rep(species_levels, each = n_time),
                  levels = species_levels),
  elev = rep(elev, n_species),
  region = factor(
    rep(region_levels[(seq_len(n_time) %% 3L) + 1L], n_species),
    levels = region_levels
  ),
  depth = rep(as.numeric(scale(runif(n_time))), n_species)
)
dat$y <- as.numeric(mu_true) +
  beta_region[as.character(dat$region)] +
  beta_depth * dat$depth +
  rnorm(nrow(dat), 0, 0.3)

# Two things a tidy frame hides. The rows are shuffled, so order of
# first appearance is neither the declared order nor the alphabetical
# one. And a scatter of responses is set to NA, so the species carry
# different numbers of observations over one shared grid.
#
# mvgam requires the frame to hold a row for every (time, series)
# cell and says so, pointing at NA as the way to mark a cell
# unobserved. Imbalance therefore takes that form here instead of
# dropped rows. It is worth having: on a frame that is balanced and
# in tidy order, a mapping that cuts the timeline into one block per
# species produces exactly the right shapes, and nothing downstream
# can tell it from a correct one.
dat <- dat[sample(nrow(dat)), , drop = FALSE]
rownames(dat) <- NULL
dat$y[sample(nrow(dat), size = floor(0.08 * nrow(dat)))] <- NA_real_

observed_per_species <- table(
  factor(as.character(dat$series[!is.na(dat$y)]),
         levels = species_levels)
)
stopifnot(
  # First appearance is neither the declared nor the alphabetical
  # order.
  !identical(as.character(unique(dat$series)), species_levels),
  !identical(as.character(unique(dat$series)), sort(species_levels)),
  # The grid stays rectangular, and complete.
  nrow(dat) == n_time * n_species,
  identical(sort(unique(dat$time)), time_vals),
  # The observations over it do not.
  length(unique(as.integer(observed_per_species))) > 1L,
  all(observed_per_species > 0L)
)

# The rows that reach the likelihood. mvgam wants the frame
# rectangular so the trend grid is complete, and brms then drops the
# unobserved cells from the model frame, so `standata` is shorter
# than `dat`. Anything compared against `standata` is compared
# against these rows.
obs_rows <- which(!is.na(dat$y))
dat_obs <- dat[obs_rows, , drop = FALSE]
stopifnot(length(obs_rows) < nrow(dat))

trend_map <- matrix(NA_real_, nrow = n_species, ncol = N_lv)

sim_truth <- list(
  n_time = n_time, n_species = n_species, N_lv = N_lv,
  species_levels = species_levels, Z_true = Z_true,
  lv_true = lv_true, elev = elev, time_vals = time_vals,
  region_levels = region_levels, beta_region = beta_region,
  beta_depth = beta_depth
)

# The value of `expr` alongside every warning raised computing it.
# This frame deliberately carries unobserved cells, so the notices
# about them are part of the contract a user is owed and are counted
# rather than discarded. Returned as a list so nothing is attached to
# a fitted object that would then be cached.
with_warnings <- function(expr) {
  seen <- character(0)
  value <- withCallingHandlers(expr, warning = function(w) {
    seen <<- c(seen, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = seen)
}

by_lv_formula <- ~ s(elev, k = 5, by = lv_axis()) - 1 + ZMVN(cor = TRUE)
plain_formula <- ~ s(elev, k = 5) - 1 + ZMVN(cor = TRUE)
obs_formula <- y ~ region + depth


# -- Prefit: the structure Stan is handed, before any sampling -------
#
# A `run_model = FALSE` build carries no draws, so what it can settle
# is exactly the thing this design gets wrong: which axis the trend
# design is indexed by. Reaching it costs no sampling, so the whole
# contrast is checked here and the fits below only have to answer for
# the posterior.

built_by_lv <- with_warnings(mvgam(
  formula = obs_formula, trend_formula = by_lv_formula,
  trend_map = trend_map, data = dat, family = gaussian(),
  run_model = FALSE, silent = 2
))
built_plain <- with_warnings(mvgam(
  formula = obs_formula, trend_formula = plain_formula,
  trend_map = trend_map, data = dat, family = gaussian(),
  run_model = FALSE, silent = 2
))
prefit_by_lv <- built_by_lv$value
prefit_plain <- built_plain$value


test_that("the dropped rows are reported once per build", {
  # mvgam refuses a panel whose series cover different occasions and
  # asks for the short ones to be padded with `NA`. brms then drops
  # those rows from the likelihood and says so. The notice is correct
  # and a user needs it, so it is asserted rather than silenced.
  #
  # The count is the claim, not the presence. Assembling a model runs
  # brms's code generator more than once over the same frame, and a
  # notice raised once per internal pass rather than once per call is
  # a defect this package has had before. A notice that stops being
  # raised at all is worse: rows would leave the likelihood in
  # silence.
  na_notice <- function(w) grep("Rows containing NAs", w, value = TRUE)
  expect_length(na_notice(built_by_lv$warnings), 1L)
  expect_length(na_notice(built_plain$warnings), 1L)

  # And nothing else was raised. Without this, a new and unrelated
  # warning would sit unexamined beside the expected one.
  for (built in list(built_by_lv, built_plain)) {
    expect_identical(
      setdiff(built$warnings, na_notice(built$warnings)),
      character(0)
    )
  }
})


test_that("the unobserved cells leave the likelihood, not the grid", {
  # A cell with no response cannot contribute a likelihood term, so
  # the model frame is the observed rows. The latent grid is a
  # different matter: it has to stay complete, or the trend is being
  # asked to skip occasions at which other species were seen.
  sd <- prefit_by_lv$standata
  expect_identical(as.integer(sd$N), length(obs_rows))
  expect_lt(as.integer(sd$N), nrow(dat))
  # The trend still spans every occasion and every species.
  expect_identical(as.integer(sd$N_time_trend), n_time)
  expect_identical(as.integer(sd$N_series_trend), n_species)
  expect_identical(as.integer(sd$N_trend), n_time * N_lv)
})


test_that("the prefit records the factor axis as the design grain", {
  ax <- mvgam:::mvgam_axes(prefit_by_lv)
  expect_identical(ax$grain, "lv")
  expect_identical(as.character(ax$series$levels), species_levels)
  expect_identical(as.integer(ax$series$n), n_species)
  expect_identical(as.integer(ax$factor$n_lv), N_lv)

  # Without `by = lv_axis()` the same model runs on the series axis,
  # which is what makes the record's answer a claim rather than a
  # restatement of the fit's own shape.
  expect_identical(mvgam:::mvgam_axes(prefit_plain)$grain, "series")
})


test_that("the record carries the user's times, not their ranks", {
  # `time` runs 3..62 here, so a function handing back the rank where
  # the value was asked for is visible. On a `1..n` frame the two
  # agree and the confusion is unobservable.
  ax <- mvgam:::mvgam_axes(prefit_by_lv)
  expect_identical(as.integer(ax$time$values), time_vals)
  expect_identical(as.integer(ax$time$n), n_time)
  expect_identical(as.integer(ax$time$step), 1L)

  # The record keeps the values and nothing else: an index is
  # `match()` against them and a gap is `diff()`. So the claim worth
  # asserting is that deriving the index that way reproduces the one
  # Stan was handed, which is what lets a single stored field serve
  # both readings.
  idx <- match(dat_obs$time, ax$time$values)
  expect_identical(idx, match(dat_obs$time, sort(unique(dat$time))))
  expect_identical(as.integer(prefit_by_lv$standata$obs_trend_time),
                   idx)
  expect_identical(as.integer(prefit_by_lv$standata$N_time_trend),
                   n_time)
  # Every occasion is reachable and none is invented.
  expect_identical(sort(unique(idx)), seq_len(n_time))
  expect_identical(as.integer(diff(ax$time$values)),
                   rep(1L, n_time - 1L))
})


test_that("a prefit maps a newdata frame with no posterior at all", {
  # The completeness claim: given the record and a frame, every row
  # reaches a trend cell and a label without the training frame and
  # without draws. The forecast grid is where the raw-column read
  # lived, so it is driven here rather than only behind a fit.
  ids <- mvgam:::axis_row_series(prefit_by_lv, dat)
  expect_identical(levels(ids), species_levels)
  expect_identical(as.character(ids), as.character(dat$series))

  # A shuffled frame is answered row-wise, not by position.
  set.seed(5L)
  perm <- sample(nrow(dat))
  shuffled <- dat[perm, , drop = FALSE]
  expect_identical(
    as.character(mvgam:::axis_row_series(prefit_by_lv, shuffled)),
    as.character(dat$series)[perm]
  )

  # A frame naming a species the model never had is refused, and the
  # message names the offender and the axis it is not on.
  bad <- dat
  bad$series <- as.character(bad$series)
  bad$series[bad$series == species_levels[2L]] <- "sp_ghost"
  err <- expect_error(
    mvgam:::validate_prediction_factor_levels(
      bad, prefit_by_lv$trend_metadata
    )
  )
  expect_match(conditionMessage(err), "sp_ghost", fixed = TRUE)

  # The training arms and the forecast grid, both draw-free.
  levs <- mvgam:::fitted_series_index(prefit_by_lv)
  expect_identical(names(levs), species_levels)
  training <- mvgam:::build_training_arms(prefit_by_lv, species_levels)
  expect_identical(names(training$times), species_levels)
  for (s in species_levels) {
    # Every species spans the shared grid, in the user's numbering,
    # because the frame is rectangular by construction.
    expect_identical(as.integer(training$times[[s]]), time_vals)
    expect_length(training$observations[[s]], n_time)
    # What differs by species is how much of that grid was observed.
    # An arm that quietly dropped or filled the unobserved cells
    # would carry a different count here.
    expect_identical(
      sum(!is.na(training$observations[[s]])),
      as.integer(observed_per_species[[s]])
    )
  }
  # The species really do differ, so the check above is not vacuous.
  expect_gt(length(unique(as.integer(observed_per_species))), 1L)

  # A frame reaching past the training grid yields exactly the new
  # occasions, per series, in the user's numbering.
  h <- 6L
  future_times <- max(time_vals) + seq_len(h)
  future <- data.frame(
    time = rep(future_times, n_species),
    series = factor(rep(species_levels, each = h),
                    levels = species_levels),
    elev = rep(as.numeric(scale(rnorm(h))), n_species)
  )
  grid <- mvgam:::resolve_forecast_grid(
    prefit_by_lv, future, training, species_levels
  )
  expect_false(is.null(grid))
  expect_identical(names(grid$times), species_levels)
  for (s in species_levels) {
    expect_identical(as.integer(grid$times[[s]]), future_times)
  }
  # A frame entirely inside the training grid extends nothing, and
  # that is refused rather than answered. Returning an empty grid gave
  # back a class-correct forecast holding no draws, so a caller
  # looping over the arms saw a result and read no numbers out of it.
  # The refusal names the occasions supplied and the last one observed,
  # which is what a caller needs to correct the call.
  expect_error(
    mvgam:::resolve_forecast_grid(prefit_by_lv, dat, training,
                                  species_levels),
    "names no occasion beyond the training grid"
  )
  # The last observed occasion is named in the user's own numbering,
  # so a rank standing in for a value would not satisfy this.
  expect_error(
    mvgam:::resolve_forecast_grid(prefit_by_lv, dat, training,
                                  species_levels),
    as.character(max(time_vals)),
    fixed = TRUE
  )
})


test_that("the prefit sizes the trend design by factor, not by series", {
  sd <- prefit_by_lv$standata
  expect_identical(as.integer(sd$N_time_trend), n_time)
  expect_identical(as.integer(sd$N_lv_trend), N_lv)
  expect_identical(as.integer(sd$N_series_trend), n_species)
  # One design row per (time, factor).
  expect_identical(as.integer(sd$N_trend), n_time * N_lv)
  expect_identical(dim(sd$times_trend), c(n_time, N_lv))

  # The same model without `by = lv_axis()` has one row per
  # (time, series) instead.
  sp <- prefit_plain$standata
  expect_identical(as.integer(sp$N_trend), n_time * n_species)
  expect_identical(dim(sp$times_trend), c(n_time, n_species))
})


test_that("times_trend numbers the design factor-fastest", {
  # The map from a (time, factor) cell to a design row. Numbered
  # time-fastest instead, every factor would read the design row of
  # another occasion: the indices stay in range, the model samples,
  # and the smooth is evaluated at the wrong covariate value.
  sd <- prefit_by_lv$standata
  tt <- sd$times_trend
  expect_identical(as.integer(tt[1L, ]), c(1L, 2L))
  expect_identical(as.integer(tt[2L, ]), c(3L, 4L))
  expect_identical(as.integer(tt[n_time, ]),
                   c(n_time * N_lv - 1L, n_time * N_lv))
  # Every design row is used exactly once.
  expect_identical(sort(as.integer(tt)), seq_len(n_time * N_lv))
})


test_that("the trend design is split by factor, on one shared basis", {
  # The fingerprint of a factor-axis design, read off the data Stan
  # is handed rather than off the program text. `by = lv_axis()`
  # gives each factor its own coefficients on a basis both evaluate
  # at the same covariate, so the design is block-complementary: a
  # factor's rows carry its own columns and zero in the other's.
  #
  # A design built on the series axis and merely relabelled cannot
  # produce this. Nor can one that gave the two factors separate
  # bases, which would fit but would mean the factors were no longer
  # comparable through `Z`.
  sd <- prefit_by_lv$standata
  expect_identical(nrow(sd$Xs_trend), n_time * N_lv)
  r1 <- as.integer(sd$times_trend[, 1L])
  r2 <- as.integer(sd$times_trend[, 2L])
  expect_length(intersect(r1, r2), 0L)

  X <- sd$Xs_trend
  expect_identical(colnames(X),
                   c("selev:.trend1_1", "selev:.trend2_1"))
  # Each factor's rows are zero in the other factor's column ...
  expect_true(all(X[r1, 2L] == 0))
  expect_true(all(X[r2, 1L] == 0))
  # ... and carry the identical covariate value in their own, which
  # is what makes it one smooth split two ways rather than two.
  expect_equal(unname(X[r1, 1L]), unname(X[r2, 2L]))
  expect_gt(max(abs(X[r1, 1L])), 0)

  # The same holds of the penalised basis, and the two blocks agree
  # exactly on their own rows.
  zs <- grep("^Zs_[0-9]+_[0-9]+_trend$", names(sd), value = TRUE)
  expect_length(zs, N_lv)
  Z1 <- sd$Zs_1_1_trend
  Z2 <- sd$Zs_2_1_trend
  expect_identical(nrow(Z1), n_time * N_lv)
  expect_identical(dim(Z1), dim(Z2))
  expect_true(all(Z1[r2, ] == 0))
  expect_true(all(Z2[r1, ] == 0))
  expect_true(any(Z1[r1, ] != 0))
  expect_equal(unname(Z1[r1, ]), unname(Z2[r2, ]))
  # One basis means one dimension and one knot count.
  expect_equal(as.integer(sd$nb_1_trend), as.integer(sd$nb_2_trend))
  expect_equal(as.integer(sd$knots_1_trend),
               as.integer(sd$knots_2_trend))

  # The plain model emits one block, over the series grain.
  zs_plain <- grep("^Zs_[0-9]+_[0-9]+_trend$",
                   names(prefit_plain$standata), value = TRUE)
  expect_length(zs_plain, 1L)
  expect_identical(nrow(prefit_plain$standata$Xs_trend),
                   n_time * n_species)
})


test_that("the smooth record names the rows Stan emitted", {
  # The design above is right, and the block before it proves so.
  # What broke was the record `posterior_smooths()` and
  # `conditional_smooths()` walk to find a smooth's coefficients:
  # it indexed the trend side against the observation frame, where
  # `.trend` does not exist, so the term was never expanded per
  # by-level. Only the first level was ever evaluated and the
  # second came back a flat curve with no posterior width.
  #
  # Stan emits one `Zs_<row>_<term>_trend` block per by-level, so
  # the record is checked against those rather than against a
  # count written here.
  hits <- mvgam:::mvgam_smooth_terms(prefit_by_lv)
  expect_length(hits, 1L)
  expect_identical(hits[[1L]]$side, "trend")
  expect_identical(hits[[1L]]$by_var, ".trend")

  zs <- grep("^Zs_[0-9]+_1_trend$", names(prefit_by_lv$standata),
             value = TRUE)
  expect_length(zs, N_lv)
  expect_identical(as.integer(hits[[1L]]$rows), seq_along(zs))

  # The control: the plain model's smooth carries no `by`, emits one
  # block, and stays one row. Without it the check above passes on a
  # record that expanded everything.
  plain <- mvgam:::mvgam_smooth_terms(prefit_plain)
  expect_length(plain, 1L)
  expect_true(is.na(plain[[1L]]$by_var))
  expect_identical(as.integer(plain[[1L]]$rows), 1L)
})


test_that("an observation still reads a series cell, not a factor", {
  # The distinction the whole design turns on. `times_trend` moved to
  # the factor axis, but `trend[t, s]` is still series-grained
  # because the program folds `mu_factor` through `Z`. So
  # `obs_trend_series` must run over the five species, not the two
  # factors. Running it over factors keeps every index in range,
  # samples cleanly, and silently gives five species two states.
  sd <- prefit_by_lv$standata
  s_rec <- as.integer(sd$obs_trend_series)
  expect_identical(range(s_rec), c(1L, n_species))
  expect_identical(sort(unique(s_rec)), seq_len(n_species))
  # Each species claims its own rows, and they are the frame's rows
  # for that species. The frame is unbalanced, so the counts differ
  # by species and have to be read off the frame rather than assumed.
  expect_identical(s_rec, match(as.character(dat_obs$series),
                                species_levels))
  expect_identical(
    as.integer(table(factor(s_rec, levels = seq_len(n_species)))),
    as.integer(observed_per_species)
  )
})


test_that("an all-NA trend_map leaves the loadings free", {
  # `trend_map` is the only route by which `Z` becomes data. A map of
  # all `NA` marks every loading free, so `Z` must stay a parameter:
  # were it passed as data, every species would be pinned to whatever
  # the map's `NA`s were coerced to.
  expect_false("Z" %in% names(prefit_by_lv$standata))
  ax <- mvgam:::mvgam_axes(prefit_by_lv)
  expect_identical(as.integer(ax$factor$n_lv), N_lv)
  expect_null(ax$factor$Z)
})


test_that("the record and the metadata tell one story about the axis", {
  # The series axis is written twice, on the record and on
  # `trend_metadata`. Both are still read, so the two accounts
  # agreeing is a live claim rather than a tautology: they are built
  # by different routes and have disagreed before.
  ax <- mvgam:::mvgam_axes(prefit_by_lv)
  meta <- prefit_by_lv$trend_metadata
  expect_identical(as.character(meta$levels$series),
                   as.character(ax$series$levels))
  expect_identical(meta$series_source, ax$series$source)
  expect_identical(ax$series$source, "explicit")
  # No grouping is in play, so the axis is the series column itself.
  expect_null(ax$series$groups)
  expect_identical(ax$vars$time_var, "time")
  expect_identical(ax$vars$series_var, "series")
  expect_null(ax$vars$gr_var)
  expect_null(ax$vars$subgr_var)
  expect_identical(ax$vars$response_vars, "y")
})


test_that("the by-lv program folds mu_factor into the latent states", {
  sc <- as.character(stancode(prefit_by_lv))
  expect_true(grepl("row_vector[N_lv_trend] mu_factor", sc,
                    fixed = TRUE))
  expect_match(sc, "lv_trend\\[i,\\s*:\\s*\\]\\s*\\+\\s*mu_factor")
  expect_true(grepl("array[N_time_trend, N_lv_trend] int times_trend",
                    sc, fixed = TRUE))
  # A free `Z` alongside a factor-axis design would need the QR
  # identification; the by-lv path does not carry it.
  expect_false(grepl("qr_thin_R", sc, fixed = TRUE))

  # The plain path indexes the design by series and folds nothing.
  sp <- as.character(stancode(prefit_plain))
  expect_true(grepl("array[N_time_trend, N_series_trend] int times_trend",
                    sp, fixed = TRUE))
  expect_false(grepl("mu_factor", sp, fixed = TRUE))
})


# -- Fits -------------------------------------------------------------

cache_by_lv <- cache_path("val_mvgam_by_lv_axis.rds")
if (file.exists(cache_by_lv)) {
  cat("[cache] Loading by_lv_axis fit.\n")
  fit <- readRDS(cache_by_lv)
} else {
  cat("[fit ] mvgam(s(elev, by = lv_axis()), ZMVN, n_lv = 2)\n")
  # Captured, not asserted: this call runs only on a cache miss, so a
  # count here would be a claim the file makes on some runs and not
  # others. The identical claim is made unconditionally above, on a
  # build of the same frame and the same formula.
  fit <- with_warnings(mvgam(
    formula = obs_formula, trend_formula = by_lv_formula,
    trend_map = trend_map, data = dat, family = gaussian(),
    chains = 2L, iter = 1000L, warmup = 500L,
    silent = 2, backend = "cmdstanr"
  ))$value
}
if (!identical(attr(fit, "sim_truth"), sim_truth)) {
  attr(fit, "sim_truth") <- sim_truth
  saveRDS(fit, cache_by_lv)
}


test_that("the fitted object keeps the factor-axis structure", {
  ax <- mvgam:::mvgam_axes(fit)
  expect_identical(ax$grain, "lv")
  expect_identical(as.character(ax$series$levels), species_levels)
  expect_identical(as.integer(fit$standata$N_lv_trend), N_lv)
  expect_identical(as.integer(fit$standata$N_series_trend), n_species)
  expect_identical(as.integer(fit$standata$N_trend), n_time * N_lv)
  expect_identical(dim(fit$standata$times_trend), c(n_time, N_lv))
})


test_that("every prediction surface answers for every row", {
  n_obs <- nrow(dat)
  ep <- posterior_epred(fit, ndraws = 20L)
  pp <- posterior_predict(fit, ndraws = 20L)
  expect_identical(dim(ep), c(20L, n_obs))
  expect_identical(dim(pp), c(20L, n_obs))
  expect_true(all(is.finite(ep)))
  expect_true(all(is.finite(pp)))
  expect_identical(nrow(fitted(fit, ndraws = 20L)), n_obs)
  expect_identical(nrow(residuals(fit, ndraws = 20L)), n_obs)

  # Column j is row j of the frame.
  os <- mvgam:::get_observation_structure(fit, newdata = dat)
  expect_identical(as.character(os$series), as.character(dat$series))
  expect_identical(os$series_levels, species_levels)
  expect_identical(as.integer(os$time),
                   match(dat$time, sort(unique(dat$time))))
})


test_that("each row reads the latent cell the sampler drew for it", {
  # `trend[t, s]` is at the series grain even though the design runs
  # on the factor axis, because the program folds through Z. A row
  # reading another series' cell returns a real state of the right
  # shape, so only this comparison sees it.
  # Over the rows that reached the likelihood: `obs_trend_*` is
  # recorded for those, so the frame compared against is `dat_obs`.
  dm <- posterior::as_draws_matrix(fit$fit)
  t_rec <- as.integer(fit$standata$obs_trend_time)
  s_rec <- as.integer(fit$standata$obs_trend_series)
  expect_length(t_rec, nrow(dat_obs))
  want <- vapply(paste0("trend[", t_rec, ",", s_rec, "]"),
                 function(k) mean(dm[, k]), numeric(1))
  got <- colMeans(
    mvgam:::extract_trend_latent_states(fit, newdata = dat_obs,
                                        full_draws = dm)
  )
  expect_equal(unname(got), unname(want))
})


test_that("the trend linpred comes back at the series grain", {
  # `mu_factor` is per latent axis and the program folds it through
  # `Z`, so what a caller receives is one value per observation like
  # any other trend formula rather than one per factor.
  lp_full <- extract_component_linpred(
    mvgam_fit = fit, newdata = dat, component = "trend",
    draw_ids = 1:20, incl_latent_state = TRUE
  )
  lp_det <- extract_component_linpred(
    mvgam_fit = fit, newdata = dat, component = "trend",
    draw_ids = 1:20, incl_latent_state = FALSE
  )
  expect_identical(dim(lp_full), c(20L, nrow(dat)))
  expect_identical(dim(lp_det), c(20L, nrow(dat)))
  expect_false(anyNA(lp_full))
  expect_false(anyNA(lp_det))

  # `predict_*` is time-agnostic: it composes the deterministic
  # submodel and leaves the latent state to arrive as a marginal
  # envelope. So `incl_latent_state` does not move this result, and
  # the conditional state is a different quantity reached elsewhere.
  # Asserting a difference here would be asking a marginal surface to
  # behave like a conditional one.
  expect_equal(unname(lp_full), unname(lp_det))

  # And the conditional state really is available, by the route that
  # owns it, so the agreement above is not the state going missing.
  dm <- posterior::as_draws_matrix(fit$fit)
  states <- mvgam:::extract_trend_latent_states(
    fit, newdata = dat_obs, full_draws = dm
  )
  expect_identical(ncol(states), nrow(dat_obs))
  expect_gt(stats::sd(colMeans(states)), 0)

  # Reshaped to the grid it is (time, series), and the species differ
  # because their rows of `Z` do.
  obs <- mvgam:::get_observation_structure(fit, newdata = dat)
  grid <- mvgam:::reshape_linpred_to_grid(lp_det[1L, ], obs)
  expect_identical(dim(grid), c(n_time, n_species))
  same <- character(0)
  for (i in seq_len(n_species)) {
    for (j in seq_len(n_species)) {
      if (j <= i) next
      if (isTRUE(all.equal(grid[, i], grid[, j]))) {
        same <- c(same, paste(species_levels[i], species_levels[j],
                              sep = "="))
      }
    }
  }
  expect_identical(same, character(0))
})


test_that("hindcast arms are the species, in order, and distinct", {
  arms <- hindcast(fit, ndraws = 20L)$hindcasts
  expect_identical(names(arms), species_levels)
  same <- character(0)
  for (i in seq_along(arms)) {
    for (j in seq_along(arms)) {
      if (j <= i) next
      if (isTRUE(all.equal(arms[[i]], arms[[j]]))) {
        same <- c(same, paste(names(arms)[i], names(arms)[j], sep = "="))
      }
    }
  }
  expect_identical(same, character(0))
})


# -- The newdata battery ---------------------------------------------
#
# Every check below compares a rearranged, cut or relabelled frame
# against the same rows of the answer for the full frame. A
# prediction that places rows by position rather than by content
# agrees with the training-order call and disagrees with all of
# these; so does one that takes its species axis from whatever the
# frame in hand happens to carry.

# Computed once. `epred` on the training frame, in the frame's own
# order, is the reference every rearrangement is judged against.
ref_epred <- posterior_epred(fit, newdata = dat, draw_ids = 1:10,
                             incl_autocor = TRUE)

test_that("the reference the battery compares against actually varies", {
  # Every check below compares a rearranged or cut frame against the
  # same columns of `ref_epred`. That is a comparison of the
  # prediction with itself, so a predictor returning one constant for
  # every row satisfies the whole battery: the shuffle, the subsets,
  # the relabelling, the single rows and the duplicates all agree
  # trivially. This is the guard that makes the rest of them mean
  # something.
  colm <- colMeans(ref_epred)
  expect_gt(stats::sd(colm), 0)

  # It varies between series, so a cut by series is a real cut.
  by_series <- tapply(colm, as.character(dat$series), mean)
  expect_length(by_series, n_species)
  expect_gt(stats::sd(as.numeric(by_series)), 0)

  # And within a series over time, so a cut by occasion is one too.
  for (s in species_levels) {
    rows <- which(as.character(dat$series) == s)
    expect_gt(stats::sd(colm[rows]), 0)
  }

  # No two rows of the reference are the same everywhere, which is
  # what a collapsed axis would produce while keeping every shape.
  expect_gt(length(unique(round(colm, 8))), 1L)
})



test_that("a shuffled newdata answers the same, in the new order", {
  set.seed(23L)
  perm <- sample(nrow(dat))
  shuf <- posterior_epred(fit, newdata = dat[perm, , drop = FALSE],
                          draw_ids = 1:10, incl_autocor = TRUE)
  expect_equal(unname(ref_epred[, perm, drop = FALSE]), unname(shuf))
})


test_that("a newdata sorted by species answers the same", {
  # The tidy order every other fixture is built in. If anything
  # downstream quietly assumes rows arrive grouped by species, this
  # is the frame that satisfies it and the shuffled one above does
  # not; the two must still agree row for row.
  ord <- order(as.character(dat$series), dat$time)
  got <- posterior_epred(fit, newdata = dat[ord, , drop = FALSE],
                         draw_ids = 1:10, incl_autocor = TRUE)
  expect_equal(unname(ref_epred[, ord, drop = FALSE]), unname(got))
})


test_that("a newdata holding one species reads that species' state", {
  for (s in species_levels) {
    rows <- which(as.character(dat$series) == s)
    sub <- dat[rows, , drop = FALSE]
    sub$series <- droplevels(sub$series)
    expect_identical(levels(sub$series), s)
    got <- posterior_epred(fit, newdata = sub, draw_ids = 1:10,
                           incl_autocor = TRUE)
    expect_equal(unname(got), unname(ref_epred[, rows, drop = FALSE]))
  }
})


test_that("a newdata holding a subset of species reads each of them", {
  # One species at a time is the arm every per-series method builds.
  # A frame carrying some but not all of them is the case that
  # separates an axis read off the record from one rebuilt out of the
  # levels present, because here the frame's own levels are a proper
  # subset in a different order.
  for (subset in list(species_levels[c(2L, 4L)],
                      species_levels[c(5L, 1L, 3L)],
                      rev(species_levels[-1L]))) {
    rows <- which(as.character(dat$series) %in% subset)
    sub <- dat[rows, , drop = FALSE]
    sub$series <- factor(as.character(sub$series), levels = subset)
    expect_setequal(levels(sub$series), subset)
    got <- posterior_epred(fit, newdata = sub, draw_ids = 1:10,
                           incl_autocor = TRUE)
    expect_equal(unname(got), unname(ref_epred[, rows, drop = FALSE]))
  }
})


test_that("a newdata declaring its levels in another order maps right", {
  # Mapping is by label, so redeclaring the same species in reverse
  # must not move a single answer. Taking the axis from the frame's
  # own `levels()` makes every answer move.
  nd <- dat
  nd$series <- factor(as.character(nd$series),
                      levels = rev(species_levels))
  got <- posterior_epred(fit, newdata = nd, draw_ids = 1:10,
                         incl_autocor = TRUE)
  expect_equal(unname(got), unname(ref_epred))
})


test_that("a newdata carrying an unused extra level maps right", {
  # A user subsetting a larger frame keeps the parent's levels, so a
  # declared level with no rows is ordinary. It must not shift the
  # axis, and it must not be mistaken for an unknown species: there
  # are no rows naming it.
  nd <- dat
  nd$series <- factor(as.character(nd$series),
                      levels = c(species_levels, "unobserved"))
  expect_identical(nlevels(nd$series), n_species + 1L)
  expect_identical(sum(nd$series == "unobserved"), 0L)
  got <- posterior_epred(fit, newdata = nd, draw_ids = 1:10,
                         incl_autocor = TRUE)
  expect_equal(unname(got), unname(ref_epred))
})


test_that("a newdata whose species is a character column maps right", {
  # `series` reaches the package as a character vector whenever a
  # user builds a frame without `stringsAsFactors`. The axis is a
  # property of the model, so the answers must not depend on how the
  # column happens to be typed.
  nd <- dat
  nd$series <- as.character(nd$series)
  expect_type(nd$series, "character")
  got <- posterior_epred(fit, newdata = nd, draw_ids = 1:10,
                         incl_autocor = TRUE)
  expect_equal(unname(got), unname(ref_epred))
})


test_that("a one-row newdata reads that row's cell", {
  # The smallest frame there is, and the one where a mapping that
  # numbers the species by their order of appearance is always
  # right by accident for the first species and always wrong for the
  # rest. One row of each species is checked.
  for (s in species_levels) {
    j <- which(as.character(dat$series) == s)[1L]
    got <- posterior_epred(fit, newdata = dat[j, , drop = FALSE],
                           draw_ids = 1:10, incl_autocor = TRUE)
    expect_identical(dim(got), c(10L, 1L))
    expect_equal(unname(got), unname(ref_epred[, j, drop = FALSE]))
  }
})


test_that("a newdata repeating a row answers the same for each copy", {
  # Duplicate rows are what a prediction grid is made of. Both copies
  # must read the same cell, which a mapping keyed on row position
  # inside the frame gets wrong.
  j <- which(as.character(dat$series) == species_levels[3L])[1L]
  nd <- dat[c(j, j, j), , drop = FALSE]
  got <- posterior_epred(fit, newdata = nd, draw_ids = 1:10,
                         incl_autocor = TRUE)
  expect_identical(dim(got), c(10L, 3L))
  expect_equal(unname(got[, 1L]), unname(got[, 2L]))
  expect_equal(unname(got[, 1L]), unname(got[, 3L]))
  expect_equal(unname(got[, 1L]), unname(ref_epred[, j]))
})


test_that("a newdata holding one occasion reads that occasion", {
  # The complement of the one-species cut: hold the species and cut
  # the time axis instead. An occasion is addressed by its value, and
  # the values here start at 3, so a cut that renumbers from 1 reads
  # the wrong rows of the trend.
  for (tv in time_vals[c(1L, 2L, 30L, n_time)]) {
    rows <- which(dat$time == tv)
    expect_gt(length(rows), 0L)
    sub <- dat[rows, , drop = FALSE]
    got <- posterior_epred(fit, newdata = sub, draw_ids = 1:10,
                           incl_autocor = TRUE)
    expect_equal(unname(got), unname(ref_epred[, rows, drop = FALSE]))
  }
})


test_that("the other prediction methods agree with epred on newdata", {
  # `epred` is what every check above is written against, so the
  # remaining methods are pinned to it on a rearranged frame. One
  # that resolves the axis by its own route agrees on the training
  # frame and parts company here.
  set.seed(77L)
  perm <- sample(nrow(dat))
  nd <- dat[perm, , drop = FALSE]

  lp <- posterior_linpred(fit, newdata = nd, draw_ids = 1:10,
                          incl_autocor = TRUE)
  expect_identical(dim(lp), c(10L, nrow(dat)))
  # Gaussian with an identity link, so the two coincide.
  expect_equal(unname(lp), unname(ref_epred[, perm, drop = FALSE]))

  ft <- fitted(fit, newdata = nd, ndraws = 10L)
  expect_identical(nrow(ft), nrow(dat))
  pp <- posterior_predict(fit, newdata = nd, draw_ids = 1:10)
  expect_identical(dim(pp), c(10L, nrow(dat)))
  expect_true(all(is.finite(pp)))
})


test_that("a newdata naming an unknown species is refused", {
  nd <- dat
  nd$series <- factor(
    ifelse(seq_len(nrow(nd)) == 1L, "y_unseen",
           as.character(nd$series)),
    levels = c(species_levels, "y_unseen")
  )
  err <- expect_error(
    posterior_epred(fit, newdata = nd, draw_ids = 1:5),
    "Series levels in newdata not found in training data"
  )
  # A refusal that does not name the offending level, or list the
  # ones that would have worked, leaves the user to find which of
  # their species the model has never seen.
  expect_match(conditionMessage(err), "y_unseen", fixed = TRUE)
  for (s in species_levels) {
    expect_match(conditionMessage(err), s, fixed = TRUE)
  }
})


test_that("a newdata naming an unknown region is refused", {
  # The same question on an observation-side factor rather than the
  # series axis. A contrast has no column for a level the design was
  # never built with, so predicting one silently would put the row on
  # whichever level happens to sit first.
  nd <- dat
  nd$region <- factor(
    ifelse(seq_len(nrow(nd)) == 1L, "atlantis",
           as.character(nd$region)),
    levels = c(region_levels, "atlantis")
  )
  err <- expect_error(posterior_epred(fit, newdata = nd,
                                      draw_ids = 1:5))
  expect_match(conditionMessage(err), "atlantis", fixed = TRUE)
})


test_that("a newdata missing a required column is refused", {
  # `depth` carries a slope, so a frame without it cannot be
  # predicted from. The refusal has to name the column rather than
  # failing further down on a dimension mismatch.
  nd <- dat
  nd$depth <- NULL
  err <- expect_error(posterior_epred(fit, newdata = nd,
                                      draw_ids = 1:5))
  expect_match(conditionMessage(err), "depth", fixed = TRUE)
})


test_that("the factor methods report two factors over five species", {
  af <- active_factors(fit)
  expect_s3_class(af, "mvgam_active_factors")
  expect_identical(as.integer(af$n_lv), N_lv)
  expect_identical(nrow(af$per_factor), N_lv)
  # The object has a plot method, and it is the one a user reaches
  # for after reading the table.
  expect_s3_class(plot(af), "ggplot")

  sv <- shared_variation(fit)
  expect_identical(as.character(sv$series_names), species_levels)
  expect_identical(as.integer(sv$n_series), n_species)
  expect_identical(as.integer(sv$n_lv), N_lv)

  rc <- residual_cor(fit)
  expect_identical(rownames(rc$cor), species_levels)
  expect_equal(unname(diag(rc$cor)), rep(1, n_species))
  expect_equal(unname(rc$cor), unname(t(rc$cor)))

  Z_arr <- mvgam:::extract_Z_loadings(
    posterior::as_draws_matrix(fit$fit),
    n_obs_series = n_species, n_lv = N_lv
  )
  expect_identical(dim(Z_arr)[2:3], c(n_species, N_lv))
})


test_that("the factor-axis smooth is drawn once per latent factor", {
  # `s(elev, by = lv_axis())` is the only smooth this model has, and
  # it is split by latent factor rather than by species. Checking
  # merely that the call returns leaves the two ways it can be wrong
  # untouched: an empty grid, which returns a correctly named result
  # with no rows in it, and a grid built over the observation frame,
  # which has no `.trend` column and so cannot separate the factors.
  sm <- smooths(fit)
  expect_length(sm, 1L)
  expect_match(sm[1L], ".trend", fixed = TRUE)

  # The smooth is evaluated over the trend grid, which is one row per
  # (time, factor), not one per observation.
  ps <- posterior_smooths(fit, smooth = sm[1L], ndraws = 20L)
  expect_identical(dim(ps), c(20L, n_time * N_lv))
  expect_true(all(is.finite(ps)))

  cs <- conditional_smooths(fit)
  expect_length(cs, 1L)
  # `conditional_smooths()` hands back the frame itself rather than
  # nesting it under `$data`, which `conditional_effects()` does.
  d <- cs[[1L]]
  expect_s3_class(d, "data.frame")
  expect_gt(nrow(d), 0L)
  expect_true(all(c("effect1__", "cond__", "estimate__",
                    "lower__", "upper__") %in% names(d)))
  expect_true(all(is.finite(d$estimate__)))
  expect_true(all(d$lower__ <= d$estimate__))
  expect_true(all(d$estimate__ <= d$upper__))

  # One curve per latent factor, and the two are not the same curve:
  # a design that collapsed the factor axis draws one shape twice.
  curves <- split(d$estimate__, d$cond__)
  expect_length(curves, N_lv)
  expect_false(isTRUE(all.equal(curves[[1L]], curves[[2L]])))

  # The curve is drawn over the covariate the model saw, so a grid
  # built from the wrong frame lands outside this range.
  trend_elev <- fit$trend_model$data$elev
  expect_gte(min(d$effect1__), min(trend_elev) - 1e-8)
  expect_lte(max(d$effect1__), max(trend_elev) + 1e-8)
})


test_that("summary and the criticism methods run on this fit", {
  txt <- capture.output(summary(fit))
  expect_gt(length(txt), 10L)
  expect_true(any(grepl(paste0("Series:\\s*", n_species), txt)))
  # `log_lik` answers per frame row, not per likelihood term, so it
  # spans all 300 rows while only 276 reached the model. A cell with
  # no response contributes no density, and it comes back non-finite
  # rather than as a silent zero, which would flatter every
  # information criterion computed from it. The claim is that the two
  # sets line up exactly: finite where observed, non-finite where not.
  ll <- log_lik(fit, ndraws = 20L)
  expect_identical(dim(ll), c(20L, nrow(dat)))
  bad_cols <- which(apply(ll, 2L, function(col) any(!is.finite(col))))
  expect_identical(unname(bad_cols), setdiff(seq_len(nrow(dat)),
                                             obs_rows))
  expect_true(all(is.finite(ll[, obs_rows, drop = FALSE])))

  # `loo()` warns when a Pareto-k exceeds its threshold, which is a
  # statement about this fit rather than noise. Suppressing it throws
  # away the one diagnostic that says whether the approximation can
  # be trusted, so it is captured and turned into claims: the
  # estimate is finite, no k reaches the point where the
  # approximation breaks, and the warning that arrived, if any, is
  # the k notice those numbers already account for rather than
  # something else that slipped through.
  loo_warnings <- character(0)
  ic <- withCallingHandlers(
    loo(fit),
    warning = function(w) {
      loo_warnings <<- c(loo_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
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
  # A `loo` built on non-finite terms would be quietly wrong, so the
  # number of observations it kept is pinned to the observed rows.
  expect_identical(length(ic$diagnostics$pareto_k), length(obs_rows))
})



test_that("every panel draws the occasions the frame supplied", {
  # The frame numbers its occasions from 3, so a rank and a time are
  # different vectors and a plot drawing one where it means the other
  # is visible. The series and trend panels draw the times; the
  # factor panel is the one that draws ranks, and its axis is
  # labelled the same as the others.
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  drawn_x <- function(p) {
    b <- ggplot2::ggplot_build(p)
    xs <- unlist(lapply(
      b$data, function(l) if ("x" %in% names(l)) l$x else NULL
    ))
    xs <- xs[is.finite(xs)]
    expect_gt(length(xs), 0L)
    range(xs)
  }
  want <- as.numeric(range(time_vals))
  for (ty in c("series", "trend", "factors")) {
    expect_equal(drawn_x(plot(fit, type = ty)), want)
  }
})


test_that("a narrowed likelihood is paired with the rows it kept", {
  # The frame is unbalanced on purpose, so some cells carry no
  # response. A missing response is a row the density cannot be
  # evaluated at rather than a row that leaves the model, so
  # `log_lik()` answers at the full width and empties those columns,
  # and `clean_ll()` drops them and records which survived.
  #
  # The claim is that a consumer narrows to the same columns.
  # `clean_ll()` sets `scored_columns` for exactly that, and its own
  # roxygen says a matrix paired with the frame afterwards has to be
  # narrowed with it.
  d <- mvgam:::mvgam_training_data(fit)
  y <- insight::find_response(fit)
  n_missing <- sum(is.na(d[[y]]))
  expect_gt(n_missing, 0L)

  ll <- log_lik(fit, ndraws = 20L)
  expect_identical(ncol(ll), nrow(d))
  expect_identical(sum(apply(ll, 2L, function(z) all(is.na(z)))),
                   n_missing)
  cleaned <- mvgam:::clean_ll(fit, ll)
  expect_identical(ncol(cleaned), nrow(d) - n_missing)
  expect_length(attr(cleaned, "scored_columns"), ncol(cleaned))

  # `loo()` and `waic()` read the narrowed matrix and answer, which
  # is what places the fault below in the pairing rather than in the
  # narrowing. Asserted first so that they run.
  expect_true(is.finite(
    suppressWarnings(loo(fit))$estimates["elpd_loo", "Estimate"]
  ))
  expect_true(is.finite(
    suppressWarnings(waic(fit))$estimates["elpd_waic", "Estimate"]
  ))

})


# The two consumers that pair the narrowed matrix against the whole
# frame sit in blocks of their own. Both raise rather than fail, and
# an error ends the block it is in, so together in one block the
# first would hide the second.

test_that("loo splits the likelihood it was given", {
  by_series <- suppressWarnings(loo(fit, by_series = TRUE))
  expect_s3_class(by_series, "data.frame")
  expect_true(all(is.finite(by_series$elpd_loo)))
})


test_that("a refit rebuilds the model that was fitted", {
  # Every cross-validation method here refits through `update()`, so
  # what `update()` rebuilds is what `kfold()` and `lfo_cv()` score
  # against. This fit's factor count came from a top-level
  # `trend_map`, which the trend formula does not name: re-evaluating
  # that formula alone loses it, `by = lv_axis()` then reads the
  # series axis instead of the factor axis, and the refit carries one
  # smooth per series where the fit has one per factor.
  #
  # Asserted on the program rather than on the call, because a call
  # that names `n_lv` is not the claim; a refit that builds the same
  # model is.
  refit <- suppressWarnings(update(
    fit, newdata = mvgam:::mvgam_training_data(fit),
    chains = 1L, iter = 2L, silent = 2L, refresh = 0
  ))
  parent_sd <- standata(fit)
  refit_sd <- standata(refit)
  # Compared by value: the two paths agree on the count while
  # differing in whether they store it as integer or double, and the
  # claim here is the count.
  expect_equal(as.integer(refit_sd$N_lv_trend),
               as.integer(parent_sd$N_lv_trend))
  expect_equal(as.integer(refit_sd$N_trend),
               as.integer(parent_sd$N_trend))
  # The factor axis is narrower than the series axis here, so a fit
  # that fell back to the series would be caught by the line above.
  expect_lt(as.integer(parent_sd$N_lv_trend),
            as.integer(parent_sd$N_series_trend))
  # The whole program, so a difference in any design block bites and
  # not only in the two counts named above.
  expect_identical(
    mvgam:::mvgam_normalise_stancode(stancode(refit)),
    mvgam:::mvgam_normalise_stancode(stancode(fit))
  )
})


test_that("kfold partitions the rows the likelihood scored", {
  kf <- suppressWarnings(kfold(fit, K = 2L))
  expect_true(is.finite(kf$estimates["elpd_kfold", "Estimate"]))
})


test_that("pp_check and the plotting methods render", {
  # `plot()` returns a ggplot, so that is the class asserted. An
  # `is.list()` check would pass on any method returning `list()`.
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  drawn <- list(pp_check = with_warnings(pp_check(fit, ndraws = 20L)))
  for (ty in c("residuals", "trend", "factors")) {
    drawn[[ty]] <- with_warnings(plot(fit, type = ty))
  }
  drawn$mcmc <- with_warnings(mcmc_plot(fit))
  for (nm in names(drawn)) {
    expect_s3_class(drawn[[nm]]$value, "ggplot")
  }

  # A plot of observed against fitted has rows it cannot draw, and
  # says so. Counted rather than silenced, and counted per call: the
  # residual panel runs the same check once per panel, so reporting
  # it once for the grid is the contract and reporting it four times
  # is the defect.
  miss <- function(w) grep("missing response", w, value = TRUE)
  expect_length(miss(drawn$pp_check$warnings), 1L)
  expect_length(miss(drawn$residuals$warnings), 1L)

  # A plot that draws no observations owes no such notice, so this
  # separates one raised where it belongs from one raised on every
  # plot the package makes.
  expect_identical(miss(drawn$trend$warnings), character(0))
  expect_identical(miss(drawn$factors$warnings), character(0))
  expect_identical(miss(drawn$mcmc$warnings), character(0))

  # And nothing unrelated was raised anywhere in the set.
  for (nm in names(drawn)) {
    expect_identical(
      setdiff(drawn[[nm]]$warnings, miss(drawn[[nm]]$warnings)),
      character(0)
    )
  }
})

cat("\nDone.\n")
