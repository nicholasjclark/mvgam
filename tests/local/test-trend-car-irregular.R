# `CAR()` is the one trend whose dynamics read the time axis as a
# quantity rather than as an index. Every other trend steps once per
# occasion and cares only about the order; a continuous-time
# autoregression raises its damping to the power of the gap, so it
# needs the size of each gap and gets a different answer when the gap
# is wrong.
#
# That makes it the trend where a rank standing in for a value is
# visible, and no other local fixture can see the difference: they are
# all built on `time = 1..n`, where every gap is 1 and a rank and a
# value coincide. The grid here is deliberately irregular, so
# `c(1, diff(times))` is not a vector of ones and a derivation that
# counted occasions instead of measuring them fails.
#
# The file also pins the two structures `CAR()` refuses. A
# continuous-time trend has no factor decomposition and no pooled
# correlation, so `by = lv_axis()` and a `gr` / `subgr` grouping are
# both errors rather than quietly ignored arguments.
#
#   data: 9 series x 26 irregular occasions, poisson
#   times: 3, 4, 5, 7, 10, 11, 15, ... (gaps of 1, 2, 3, 4)
#
# Run with:
#   testthat::test_file("tests/local/test-trend-car-irregular.R")

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

set.seed(808L)

n_series <- 3L
series_levels <- c("delta", "alpha", "charlie")
stopifnot(!identical(series_levels, sort(series_levels)))

# An irregular grid, starting away from 1. Gaps of several sizes, so
# a single wrong gap moves one step and not the whole trajectory, and
# the first value is 3 so no occasion equals its own rank.
time_vals <- as.integer(c(3, 4, 5, 7, 10, 11, 15, 16, 17, 20,
                          24, 25, 26, 30, 31, 35, 36, 40, 41, 45,
                          46, 50, 54, 55, 60, 61))
n_time <- length(time_vals)
gaps <- diff(time_vals)
stopifnot(
  # The grid has to be irregular, or this file tests nothing the
  # other fixtures do not.
  length(unique(gaps)) > 1L,
  !any(time_vals == seq_along(time_vals))
)

# A continuous-time AR: the damping over a gap of `d` is `phi^d`, so
# the simulation has to use the gap and not the step count.
phi_true <- 0.75
sigma_true <- 0.35
latent <- matrix(0, nrow = n_time, ncol = n_series)
for (s in seq_len(n_series)) {
  for (t in 2:n_time) {
    d <- gaps[t - 1L]
    latent[t, s] <- phi_true^d * latent[t - 1L, s] +
      rnorm(1L, 0, sigma_true * sqrt(d))
  }
}

dat <- data.frame(
  time = rep(time_vals, times = n_series),
  series = factor(rep(series_levels, each = n_time),
                  levels = series_levels),
  temp = rep(as.numeric(scale(sin(seq_len(n_time) / 4))), n_series)
)
dat$y <- rpois(nrow(dat), exp(1.2 + 0.3 * dat$temp +
                                as.vector(latent)))

sim_truth <- list(
  n_series = n_series, n_time = n_time,
  series_levels = series_levels, time_vals = time_vals,
  gaps = gaps, phi_true = phi_true, sigma_true = sigma_true,
  latent = latent
)


# -- Prefit: what the gaps become, before any sampling ---------------

prefit <- mvgam(
  formula = y ~ temp, trend_formula = ~ CAR(),
  data = dat, family = poisson(), run_model = FALSE, silent = 2
)


test_that("the record keeps the irregular grid the user supplied", {
  ax <- mvgam:::mvgam_axes(prefit)
  expect_identical(as.integer(ax$time$values), time_vals)
  expect_identical(as.integer(ax$time$n), n_time)
  expect_identical(as.character(ax$series$levels), series_levels)
  # An index is `match()` against the values, and it is the rank, so
  # the two readings differ here where elsewhere they cannot.
  idx <- match(dat$time, ax$time$values)
  expect_identical(as.integer(prefit$standata$obs_trend_time), idx)
  expect_false(identical(as.integer(ax$time$values), idx))
})


test_that("time_dis is the measured gap, not the step count", {
  # The array the CAR recursion raises its damping to the power of.
  # A derivation that counted occasions returns a column of ones and
  # keeps every dimension, every index and every prior intact, so a
  # shape check cannot see it and only the values can.
  sd <- prefit$standata
  td <- sd$time_dis
  expect_identical(dim(td), c(n_time, n_series))

  # Step `t` carries the distance from `t - 1`; the first has no
  # predecessor and takes 1 rather than a gap, because Stan raises
  # the damping to this power.
  expected <- c(1, gaps)
  expect_equal(as.numeric(td[, 1L]), as.numeric(expected))
  expect_identical(as.numeric(td[1L, 1L]), 1)

  # This is the assertion the regular fixtures cannot make: the
  # column is not all ones.
  expect_gt(length(unique(as.numeric(td[, 1L]))), 1L)
  expect_gt(max(as.numeric(td[, 1L])), 1)

  # Every series shares one grid, so every column is the same.
  for (s in seq_len(n_series)) {
    expect_equal(as.numeric(td[, s]), as.numeric(expected))
  }
  # And the gaps are the user's own, so rescaling the time column
  # would be visible.
  expect_equal(sum(as.numeric(td[-1L, 1L])),
               as.numeric(max(time_vals) - min(time_vals)))
})


test_that("the CAR trend is sized by series, not by factor", {
  sd <- prefit$standata
  expect_identical(as.integer(sd$N_time_trend), n_time)
  expect_identical(as.integer(sd$N_series_trend), n_series)
  expect_identical(dim(sd$times_trend), c(n_time, n_series))
  # A continuous-time trend has no factor decomposition, so the grain
  # is the series and every design row is used once.
  expect_identical(mvgam:::mvgam_axes(prefit)$grain, "series")
  expect_identical(sort(as.integer(sd$times_trend)),
                   seq_len(n_time * n_series))

  # Each row reads the cell for its own (time, series).
  s_rec <- as.integer(sd$obs_trend_series)
  expect_identical(s_rec, match(as.character(dat$series),
                                series_levels))
})


test_that("a multivariate CAR refuses every trend covariate", {
  # The rule is broader than it first looks, and worth pinning
  # because it decides which of these models can exist at all: a CAR
  # over more than one series takes no trend covariate of any kind.
  # A plain smooth is refused for the same reason a factor-split one
  # is, so `by = lv_axis()` never reaches the factor machinery here.
  # That answers, structurally, why there is no multivariate
  # CAR-by-factor cell to test: the covariate is refused first.
  plain <- expect_error(
    mvgam(
      formula = y ~ temp,
      trend_formula = ~ s(temp, k = 4) + CAR(),
      data = dat, family = poisson(), run_model = FALSE, silent = 2
    ),
    "cannot include trend covariates"
  )
  expect_match(conditionMessage(plain), "trend_formula", fixed = TRUE)

  by_lv <- expect_error(
    mvgam(
      formula = y ~ temp,
      trend_formula = ~ s(temp, k = 4, by = lv_axis()) - 1 + CAR(),
      data = dat, family = poisson(), run_model = FALSE, silent = 2
    ),
    "cannot include trend covariates"
  )
  # The two are refused by one rule, so they carry one message. A
  # separate wording for the factor case would mean a second layer
  # had formed its own opinion about the same frame.
  expect_identical(conditionMessage(by_lv), conditionMessage(plain))

  # The message tells the user what does work, and that claim is
  # checked rather than taken on trust: a single-series CAR accepts
  # the covariate the multivariate one refuses.
  one <- dat[as.character(dat$series) == series_levels[1L], ,
             drop = FALSE]
  one$series <- droplevels(one$series)
  uni <- mvgam(
    formula = y ~ 1, trend_formula = ~ s(temp, k = 4) + CAR(),
    data = one, family = poisson(), run_model = FALSE, silent = 2
  )
  expect_identical(as.integer(uni$standata$N_series_trend), 1L)
  expect_equal(as.numeric(uni$standata$time_dis[, 1L]),
               as.numeric(c(1, gaps)))
})


test_that("CAR refuses a factor decomposition, by both routes", {
  # A continuous-time trend evolves per series, so it has no factor
  # decomposition to offer. The refusal is registered against the
  # trend type itself, and both routes a user can ask by have to
  # reach it: a `trend_map` naming fewer factors than series, and
  # `jsdgam()`, which is a factor model by construction.
  map_err <- expect_error(
    mvgam(
      formula = y ~ temp, trend_formula = ~ CAR(),
      trend_map = matrix(NA_real_, nrow = n_series, ncol = 2L),
      data = dat, family = poisson(), run_model = FALSE, silent = 2
    ),
    "do not support factor models"
  )
  expect_match(conditionMessage(map_err), "n_lv < n_series",
               fixed = TRUE)

  jsd_err <- expect_error(
    jsdgam(
      formula = y ~ 1, factor_formula = ~ -1 + CAR(),
      data = dat, unit = time, species = series,
      family = poisson(), n_lv = 2L, run_model = FALSE, silent = 2
    ),
    "do not support factor models"
  )
  # One rule, so one message, whichever route asked.
  expect_identical(conditionMessage(jsd_err),
                   conditionMessage(map_err))
})


test_that("n_lv is the third route to a factor CAR, and refuses too", {
  # The same request as the two above, written the third way a user
  # can write it. It has to meet the same refusal: `n_lv` below the
  # series count asks for a factor decomposition, and CAR has none to
  # give.
  #
  # What happens instead is that the model is accepted and `n_lv` is
  # raised to the series count, so a user who asked for two factors
  # is handed a saturated trend with nothing in the output saying the
  # request was not honoured. Reading `N_lv_trend` back is the only
  # way to notice.
  # One expectation, so the failure reads as the single fact it is.
  # A follow-up check on the message would raise a second, confusing
  # error of its own whenever no error was thrown at all.
  expect_error(
    mvgam(
      formula = y ~ temp, trend_formula = ~ CAR(), n_lv = 2L,
      data = dat, family = poisson(), run_model = FALSE, silent = 2
    ),
    "do not support factor models"
  )
})


test_that("df reaches the program as the value it was given", {
  # `df` is the one CAR argument that changes the innovation
  # distribution, and an argument accepted and dropped is a mistake
  # this package makes elsewhere. The contrast is the claim: the
  # default draws gaussian innovations, a finite `df` draws
  # student-t ones, and the number written into the program is the
  # number asked for rather than a fixed one.
  code_of <- function(...) {
    as.character(stancode(mvgam(
      formula = y ~ temp, data = dat, family = poisson(),
      run_model = FALSE, silent = 2, ...
    )))
  }
  gauss <- code_of(trend_formula = ~ CAR())
  t4 <- code_of(trend_formula = ~ CAR(df = 4))
  t8 <- code_of(trend_formula = ~ CAR(df = 8))

  expect_match(gauss, "std_normal_lpdf(to_vector(innovations_trend))",
               fixed = TRUE)
  expect_false(grepl("multi_student_t", gauss, fixed = TRUE))
  # The value itself, so a df read and then defaulted fails here.
  expect_match(t4, "multi_student_t_cholesky_lpdf", fixed = TRUE)
  expect_match(t4, "innovations_trend[t_inn]' | 4", fixed = TRUE)
  expect_match(t8, "innovations_trend[t_inn]' | 8", fixed = TRUE)

  # Below 3 the innovations have no finite variance, so the
  # stationary initialisation is undefined and the refusal says so.
  err <- expect_error(code_of(trend_formula = ~ CAR(df = 2)),
                      "must be greater than 2")
  expect_match(conditionMessage(err), "finite variance", fixed = TRUE)
})


test_that("CAR takes no grouping arguments at all", {
  # `gr` and `subgr` are how every other trend is made hierarchical,
  # so a user will try them here. `CAR()` does not define them, and
  # what comes back is R's own "unused arguments" rather than a
  # message naming the constraint. It is recorded as the current
  # behaviour, since an argument that is silently accepted and
  # ignored would be the worse outcome by far.
  err <- expect_error(
    mvgam(
      formula = y ~ temp,
      trend_formula = ~ CAR(gr = series, subgr = series),
      data = dat, family = poisson(), run_model = FALSE, silent = 2
    ),
    "unused argument"
  )
  expect_match(conditionMessage(err), "gr", fixed = TRUE)
})


test_that("a repeated timestamp cannot collapse the damping", {
  # Two rows at one occasion give a gap of zero, and `phi^0` is 1:
  # the trend would carry forward undamped and the series would look
  # perfectly autocorrelated across that step. The gap is floored
  # instead, so the answer stays a gap rather than becoming an
  # identity.
  dup_times <- time_vals
  dup_times[5L] <- dup_times[4L]
  dup <- dat
  dup$time <- rep(dup_times, times = n_series)
  info <- list(
    data = dup, time_var = "time", series_var = "series",
    n_series = n_series
  )
  td <- mvgam:::calculate_car_time_distances(info)
  expect_true(all(as.numeric(td) > 0))
  expect_true(all(is.finite(as.numeric(td))))
})


test_that("the refusals carry messages a user can act on", {
  # A refusal is only useful if it names what was wrong and what to
  # do instead. Each of these is matched on its own wording rather
  # than on "some error happened", because an alternation over
  # several patterns passes on whichever one happens to match and
  # stops telling apart the conditions it was written to separate.

  # A series missing an occasion the others have. This is the
  # refusal most particular to CAR, and its wording says so: gaps
  # within a series are the whole point of the trend, but the
  # series still have to align on one shared grid, since `time_dis`
  # is one column per series over a common set of occasions.
  ragged <- dat[!(as.character(dat$series) == series_levels[2L] &
                    dat$time == time_vals[5L]), , drop = FALSE]
  expect_identical(nrow(ragged), nrow(dat) - 1L)
  ragged_err <- expect_error(
    mvgam(
      formula = y ~ temp, trend_formula = ~ CAR(),
      data = ragged, family = poisson(), run_model = FALSE,
      silent = 2
    ),
    "do not share the same time grid"
  )
  msg <- conditionMessage(ragged_err)
  # It counts what it got against what it wanted, so the user can
  # see which side is short.
  expect_match(msg, as.character(nrow(ragged)), fixed = TRUE)
  expect_match(msg, as.character(n_time), fixed = TRUE)
  expect_match(msg, as.character(n_series), fixed = TRUE)
  # It names the remedy, and it names this trend, because a reader
  # who chose CAR for irregular sampling needs telling that within
  # a series is where the irregularity is allowed to live.
  expect_match(msg, "NA", fixed = TRUE)
  expect_match(msg, "CAR()", fixed = TRUE)

  # A prediction frame with no time column. The time is the one
  # thing a forecast grid cannot do without, and the message names
  # the column it wanted rather than failing later on a dimension.
  training <- mvgam:::build_training_arms(prefit, series_levels)
  no_time <- dat
  no_time$time <- NULL
  time_err <- expect_error(
    mvgam:::resolve_forecast_grid(prefit, no_time, training,
                                  series_levels),
    "must contain the time column"
  )
  expect_match(conditionMessage(time_err), "time", fixed = TRUE)
  # And it lists what the frame did carry, so the user can see the
  # spelling they used.
  expect_match(conditionMessage(time_err), "series", fixed = TRUE)
})


# -- Fit --------------------------------------------------------------

cache <- cache_path("val_mvgam_car_irregular.rds")
if (file.exists(cache)) {
  cat("[cache] Loading CAR irregular-time fit.\n")
  fit <- readRDS(cache)
} else {
  cat("[fit ] mvgam(CAR(), 3 series x 26 irregular occasions)\n")
  fit <- mvgam(
    formula = y ~ temp, trend_formula = ~ CAR(),
    data = dat, family = poisson(),
    chains = 2L, iter = 1000L, warmup = 500L,
    silent = 2, backend = "cmdstanr"
  )
}
if (!identical(attr(fit, "sim_truth"), sim_truth)) {
  attr(fit, "sim_truth") <- sim_truth
  saveRDS(fit, cache)
}


test_that("the fitted CAR keeps the irregular grid", {
  expect_identical(as.integer(mvgam:::mvgam_axes(fit)$time$values),
                   time_vals)
  expect_equal(as.numeric(fit$standata$time_dis[, 1L]),
               as.numeric(c(1, gaps)))
})


test_that("the damping recovers the simulated one", {
  # A recursion that stepped once per occasion rather than over the
  # gap would need a much smaller per-step damping to fit the same
  # data, so this number moves if the gaps are being ignored.
  dm <- posterior::as_draws_matrix(fit$fit)
  ar_cols <- grep("^ar1_trend\\[", colnames(dm), value = TRUE)
  expect_length(ar_cols, n_series)
  ar_mean <- mean(colMeans(dm[, ar_cols, drop = FALSE]))
  expect_gt(ar_mean, 0)
  expect_lt(ar_mean, 1)
  expect_lt(abs(ar_mean - phi_true), 0.3)
})


test_that("every prediction surface answers for every row", {
  n_obs <- nrow(dat)
  ep <- posterior_epred(fit, ndraws = 20L)
  pp <- posterior_predict(fit, ndraws = 20L)
  expect_identical(dim(ep), c(20L, n_obs))
  expect_identical(dim(pp), c(20L, n_obs))
  expect_true(all(is.finite(ep)))
  expect_true(all(pp >= 0))
  expect_true(all(pp == floor(pp)))
  expect_identical(nrow(fitted(fit, ndraws = 20L)), n_obs)
  expect_identical(nrow(residuals(fit, ndraws = 20L)), n_obs)

  os <- mvgam:::get_observation_structure(fit, newdata = dat)
  expect_identical(as.character(os$series), as.character(dat$series))
  expect_identical(os$series_levels, series_levels)
  # The time index is the rank of the user's value, which on this
  # grid is a different vector from the value.
  expect_identical(as.integer(os$time),
                   match(dat$time, time_vals))
})


test_that("each row reads the latent cell the sampler drew for it", {
  dm <- posterior::as_draws_matrix(fit$fit)
  t_rec <- as.integer(fit$standata$obs_trend_time)
  s_rec <- as.integer(fit$standata$obs_trend_series)
  expect_length(t_rec, nrow(dat))
  want <- vapply(paste0("trend[", t_rec, ",", s_rec, "]"),
                 function(k) mean(dm[, k]), numeric(1))
  got <- colMeans(
    mvgam:::extract_trend_latent_states(fit, newdata = dat,
                                        full_draws = dm)
  )
  expect_equal(unname(got), unname(want))
})


test_that("the draw-free resolvers answer on this grid", {
  ids <- mvgam:::axis_row_series(fit, dat)
  expect_identical(levels(ids), series_levels)
  expect_identical(as.character(ids), as.character(dat$series))

  training <- mvgam:::build_training_arms(fit, series_levels)
  expect_identical(names(training$times), series_levels)
  for (s in series_levels) {
    # The user's own occasions, not 1..26.
    expect_identical(as.integer(training$times[[s]]), time_vals)
  }

  # A forecast frame continues the grid in the user's numbering, and
  # its gaps are the ones the caller chose rather than ones.
  h <- 4L
  future_times <- max(time_vals) + as.integer(c(3, 5, 10, 11))
  future <- data.frame(
    time = rep(future_times, times = n_series),
    series = factor(rep(series_levels, each = h),
                    levels = series_levels),
    temp = rep(as.numeric(scale(rnorm(h))), n_series)
  )
  grid <- mvgam:::resolve_forecast_grid(fit, future, training,
                                        series_levels)
  expect_false(is.null(grid))
  for (s in series_levels) {
    expect_identical(as.integer(grid$times[[s]]), future_times)
  }
  expect_null(
    mvgam:::resolve_forecast_grid(fit, dat, training, series_levels)
  )
})


test_that("the newdata battery holds on an irregular grid", {
  full <- posterior_epred(fit, newdata = dat, draw_ids = 1:10,
                          incl_autocor = TRUE)

  # Everything below compares a rearranged or cut frame against the
  # same columns of `full`, which is the prediction compared with
  # itself. A predictor returning one constant for every row would
  # satisfy all of it, so the reference is checked for variation
  # first: across rows, between series, and within a series over
  # time.
  colm <- colMeans(full)
  expect_gt(stats::sd(colm), 0)
  by_series <- tapply(colm, as.character(dat$series), mean)
  expect_length(by_series, n_series)
  expect_gt(stats::sd(as.numeric(by_series)), 0)
  for (s in series_levels) {
    expect_gt(stats::sd(colm[which(as.character(dat$series) == s)]), 0)
  }

  set.seed(29L)
  perm <- sample(nrow(dat))
  expect_equal(
    unname(posterior_epred(fit, newdata = dat[perm, , drop = FALSE],
                           draw_ids = 1:10, incl_autocor = TRUE)),
    unname(full[, perm, drop = FALSE])
  )

  for (s in series_levels) {
    rows <- which(as.character(dat$series) == s)
    sub <- dat[rows, , drop = FALSE]
    sub$series <- droplevels(sub$series)
    expect_identical(levels(sub$series), s)
    expect_equal(
      unname(posterior_epred(fit, newdata = sub, draw_ids = 1:10,
                             incl_autocor = TRUE)),
      unname(full[, rows, drop = FALSE])
    )
  }

  # One occasion at a time, addressed by its value. On this grid a
  # cut that renumbered from 1 would read a different occasion, and
  # on a `1..n` grid it would read the right one by accident.
  for (tv in time_vals[c(1L, 4L, 7L, n_time)]) {
    rows <- which(dat$time == tv)
    expect_gt(length(rows), 0L)
    expect_equal(
      unname(posterior_epred(fit, newdata = dat[rows, , drop = FALSE],
                             draw_ids = 1:10, incl_autocor = TRUE)),
      unname(full[, rows, drop = FALSE])
    )
  }

  # Declared in another order, and handed over as characters.
  rev_nd <- dat
  rev_nd$series <- factor(as.character(rev_nd$series),
                          levels = rev(series_levels))
  expect_equal(
    unname(posterior_epred(fit, newdata = rev_nd, draw_ids = 1:10,
                           incl_autocor = TRUE)),
    unname(full)
  )
  chr_nd <- dat
  chr_nd$series <- as.character(chr_nd$series)
  expect_equal(
    unname(posterior_epred(fit, newdata = chr_nd, draw_ids = 1:10,
                           incl_autocor = TRUE)),
    unname(full)
  )
})


test_that("a newdata missing a model covariate is refused by name", {
  # `temp` carries a slope on the observation side, so a frame
  # without it cannot be predicted from. The refusal has to name the
  # column rather than failing further down on a dimension mismatch,
  # which is what a user needs in order to fix their frame.
  no_cov <- dat
  no_cov$temp <- NULL
  err <- expect_error(
    posterior_epred(fit, newdata = no_cov, draw_ids = 1:5)
  )
  expect_match(conditionMessage(err), "temp", fixed = TRUE)
})


test_that("an unknown series is refused, and named", {
  nd <- dat
  nd$series <- factor(
    ifelse(seq_len(nrow(nd)) == 1L, "echo", as.character(nd$series)),
    levels = c(series_levels, "echo")
  )
  err <- expect_error(
    posterior_epred(fit, newdata = nd, draw_ids = 1:5),
    "Series levels in newdata not found in training data"
  )
  expect_match(conditionMessage(err), "echo", fixed = TRUE)
  for (s in series_levels) {
    expect_match(conditionMessage(err), s, fixed = TRUE)
  }
})


test_that("a CAR forecast continues the grid it was given", {
  # The horizon is irregular too, so a forecast that stepped once per
  # row rather than over each gap damps by the wrong amount. Keying
  # and width are checked first, then the value.
  h <- 4L
  future_times <- max(time_vals) + as.integer(c(3, 5, 10, 11))
  future <- data.frame(
    time = rep(future_times, times = n_series),
    series = factor(rep(series_levels, each = h),
                    levels = series_levels),
    temp = rep(rep(0, h), n_series),
    y = NA_integer_
  )
  for (ty in c("link", "trend")) {
    fc <- forecast(fit, newdata = future, ndraws = 50L, type = ty)
    expect_s3_class(fc, "mvgam_forecast")
    expect_identical(names(fc$forecasts), series_levels)
    for (s in series_levels) {
      expect_identical(dim(fc$forecasts[[s]]), c(50L, h))
      expect_true(all(is.finite(fc$forecasts[[s]])))
    }
  }

  # The first forecast step is a gap of 3 from the last observed
  # occasion, so the trend decays by `ar1^3` rather than by `ar1`.
  # Averaged over draws it lands on that product, and a recursion
  # ignoring the gap lands on the much larger `ar1 * last`.
  dm <- posterior::as_draws_matrix(fit$fit)
  fc <- forecast(fit, newdata = future, ndraws = 600L, type = "trend")
  first_gap <- future_times[1L] - max(time_vals)
  for (k in seq_along(series_levels)) {
    ar1 <- as.numeric(dm[, paste0("ar1_trend[", k, "]")])
    last <- as.numeric(dm[, paste0("trend[", n_time, ",", k, "]")])
    with_gap <- mean(ar1^first_gap * last)
    got <- mean(fc$forecasts[[series_levels[k]]][, 1L])
    expect_lt(abs(got - with_gap), 0.4)
  }
})


test_that("hindcast arms are the series, in order, and distinct", {
  arms <- hindcast(fit, ndraws = 20L)$hindcasts
  expect_identical(names(arms), series_levels)
  same <- character(0)
  for (i in seq_along(arms)) {
    for (j in seq_along(arms)) {
      if (j <= i) next
      if (isTRUE(all.equal(arms[[i]], arms[[j]]))) {
        same <- c(same, paste(names(arms)[i], names(arms)[j],
                              sep = "="))
      }
    }
  }
  expect_identical(same, character(0))
})


test_that("summary, tidiers and criticism run on a CAR fit", {
  txt <- capture.output(summary(fit))
  expect_gt(length(txt), 10L)
  expect_true(any(grepl(paste0("Series:\\s*", n_series), txt)))

  ll <- log_lik(fit, ndraws = 20L)
  expect_identical(dim(ll), c(20L, nrow(dat)))
  expect_true(all(is.finite(ll)))
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

  aug <- augment(fit)
  expect_identical(nrow(aug), nrow(dat))
  # The tidier keeps the frame's own row order and its own times.
  expect_equal(as.numeric(aug$time), as.numeric(dat$time))
  expect_equal(as.numeric(aug$.observed), as.numeric(dat$y))
  expect_true(is.data.frame(tidy(fit)))
  expect_true(any(grepl("^ar1_trend\\[", variables(fit))))
})


test_that("the plots and conditional effects render for CAR", {
  expect_s3_class(pp_check(fit, ndraws = 20L), "ggplot")
  for (ty in c("residuals", "trend", "series")) {
    # `plot()` returns a ggplot, so that is the class asserted. An
    # `is.list()` check would pass on any method returning `list()`.
    p <- plot(fit, type = ty)
    expect_s3_class(p, "ggplot")
  }
  expect_s3_class(mcmc_plot(fit), "ggplot")

  ce <- conditional_effects(fit)
  expect_s3_class(ce, "mvgam_conditional_effects")
  expect_gt(length(ce), 0L)
  for (eff in names(ce)) {
    dd <- ce[[eff]]$data
    expect_true(all(is.finite(dd$estimate)))
    expect_true(all(dd$conf.low <= dd$estimate))
    expect_true(all(dd$estimate <= dd$conf.high))
  }
})

test_that("forecast splits a frame by what lies beyond the grid", {
  # `newdata` may hold occasions the fit already saw and occasions
  # past its end. Only the second kind is a forecast, so the frame is
  # split rather than refused, and the hindcast arms still cover the
  # whole training grid.
  mk <- function(times) data.frame(
    time = rep(times, times = n_series),
    series = factor(rep(series_levels, each = length(times)),
                    levels = series_levels),
    temp = 0, y = NA_integer_
  )
  last_t <- max(time_vals)

  beyond <- forecast(fit, newdata = mk(c(65L, 70L)), ndraws = 20L)
  expect_identical(names(beyond$forecasts), series_levels)
  expect_identical(ncol(beyond$forecasts[[1L]]), 2L)
  expect_identical(ncol(beyond$hindcasts[[1L]]), n_time)

  # An occasion the fit already holds contributes a hindcast and not
  # a horizon, so the mixed frame forecasts the two future occasions
  # alone.
  mixed <- forecast(fit, newdata = mk(c(60L, 61L, 65L, 70L)),
                    ndraws = 20L)
  expect_identical(ncol(mixed$forecasts[[1L]]), 2L)
  expect_identical(as.integer(mixed$test_times[[1L]]), c(65L, 70L))
  expect_identical(ncol(mixed$hindcasts[[1L]]), n_time)

  # The horizon is measured in the user's own time, not in steps, so
  # a single occasion nine units out is one column and damps by nine.
  far <- forecast(fit, newdata = mk(70L), ndraws = 20L)
  expect_identical(ncol(far$forecasts[[1L]]), 1L)
  expect_identical(as.integer(far$test_times[[1L]]), 70L)
})


test_that("an occasion inside the grid is not a forecast horizon", {
  # `?forecast.mvgam` says rows beyond the training grid drive the
  # horizon. An occasion the fit never observed but which lies inside
  # the grid is not beyond it, so it cannot be a horizon: 6 sits
  # between the observed 5 and 7. The horizon it produces is
  # negative, and the refusal names 'eta' or 'time', neither of which
  # the caller supplied.
  mk <- function(times) data.frame(
    time = rep(times, times = n_series),
    series = factor(rep(series_levels, each = length(times)),
                    levels = series_levels),
    temp = 0, y = NA_integer_
  )
  interior <- 6L
  expect_false(interior %in% time_vals)
  expect_gt(max(time_vals), interior)

  err <- expect_error(forecast(fit, newdata = mk(interior),
                               ndraws = 20L))
  msg <- conditionMessage(err)
  # The message has to name the user's own column and the grid it
  # was measured against, the way the gapped-frame refusal does.
  expect_match(msg, "time", fixed = TRUE)
  expect_false(grepl("eta", msg, fixed = TRUE))
  expect_match(msg, as.character(max(time_vals)), fixed = TRUE)
})


test_that("a frame wholly inside the grid is refused, not emptied", {
  # Every occasion here is one the fit already holds, so there is no
  # horizon to forecast. An object carrying named series, a type and
  # a full set of hindcasts reads as though it forecast something,
  # and any claim written as a loop over the arms passes on it.
  mk <- function(times) data.frame(
    time = rep(times, times = n_series),
    series = factor(rep(series_levels, each = length(times)),
                    levels = series_levels),
    temp = 0, y = NA_integer_
  )
  fc <- forecast(fit, newdata = mk(c(30L, 31L)), ndraws = 20L)
  # `?forecast.mvgam` documents the empty case as NULL slots.
  expect_null(fc$forecasts)
  expect_null(fc$test_times)
})


test_that("the fit answers the standard model accessors", {
  # `terms()` is how a caller discovers a model's structure without
  # knowing its class, and it is the one member of this group that
  # has no method.
  expect_s3_class(model.frame(fit), "data.frame")
  expect_s3_class(formula(fit), "formula")
  expect_s3_class(insight::get_data(fit), "data.frame")
  expect_true(inherits(terms(fit), "terms"))
})


test_that("the index columns are not reported as model predictors", {
  # insight builds the term list every downstream package reads.
  # `temp` is the only predictor this model has; `time` and `series`
  # are the axis it is indexed by, and a consumer offered them will
  # take a slope over an occasion number.
  preds <- insight::find_predictors(fit)$conditional
  expect_true("temp" %in% preds)
  expect_false("time" %in% preds)
  expect_false("series" %in% preds)
})


test_that("hypothesis reaches every name variables() lists", {
  # `hypothesis()` reads the stanfit directly, so a name mvgam's
  # alias pass created is unreachable while its unaliased neighbours
  # are not. Both are listed by `variables()`.
  vars <- variables(fit)
  expect_true("b_temp" %in% vars)
  expect_true(any(grepl("^ar1_trend\\[", vars)))

  expect_s3_class(hypothesis(fit, "ar1_trend[1] > 0"), "brmshypothesis")
  expect_s3_class(hypothesis(fit, "b_temp > 0"), "brmshypothesis")
})


# ----------------------------------------------------------------------
# A grid that is continuous, not merely irregular
# ----------------------------------------------------------------------
#
# The grid above is irregular but whole-numbered, so a value and its
# truncation coincide and a derivation that floored the times would
# still answer correctly. `CAR()` places no such restriction: the
# damping is raised to the elapsed gap, and the gap may be any
# positive real. `?CAR` fits its own example on exactly this shape,
# `sim_mvgam(type = 6)` drawing U(1, 6) gaps.
#
# So this is the grid CAR exists for, and the one where a time read
# as an integer is a different occasion rather than the same one.

cont_sim <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    set.seed(717L)
    series_names <- c("north", "east")
    n_s <- length(series_names)
    gaps <- round(stats::runif(29L, 1.2, 4.8), 3L)
    tv <- cumsum(c(0.5, gaps))
    n_t <- length(tv)
    phi <- 0.8
    lat <- matrix(0, n_t, n_s)
    for (s in seq_len(n_s)) {
      for (t in 2:n_t) {
        d <- tv[t] - tv[t - 1L]
        lat[t, s] <- phi^d * lat[t - 1L, s] +
          stats::rnorm(1L, 0, 0.3 * sqrt(d))
      }
    }
    d <- data.frame(
      time = rep(tv, times = n_s),
      series = factor(rep(series_names, each = n_t),
                      levels = series_names),
      temp = rep(as.numeric(scale(cos(seq_len(n_t) / 3))), n_s)
    )
    d$y <- stats::rpois(nrow(d), exp(1.1 + 0.4 * d$temp +
                                       as.vector(lat)))
    cached <<- list(data = d, times = tv, series_names = series_names,
                    n_series = n_s, phi = phi)
    cached
  }
})

cont_fit <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    path <- cache_path("val_mvgam_car_continuous.rds")
    if (file.exists(path)) {
      cached <<- readRDS(path)
      return(cached)
    }
    cat("[fit ] mvgam(CAR(), continuous time grid)\n")
    cached <<- mvgam(
      formula = y ~ temp, trend_formula = ~ CAR(),
      data = cont_sim()$data, family = poisson(),
      chains = 2L, iter = 1000L, warmup = 500L,
      silent = 2, backend = "cmdstanr"
    )
    saveRDS(cached, path)
    cached
  }
})


test_that("the continuous grid reaches the record as the user gave it", {
  sim <- cont_sim()
  # The premise. Not one occasion is a whole number, so anything
  # reading these times as integers reads a different grid.
  expect_false(any(sim$times == floor(sim$times)))
  expect_gt(length(unique(round(diff(sim$times), 3L))), 1L)

  fit_c <- cont_fit()
  expect_equal(as.numeric(mvgam_axes(fit_c)$time$values),
               as.numeric(sim$times))
  # The damping is raised to these, so they are the measured gaps.
  expect_equal(as.numeric(fit_c$standata$time_dis[, 1L]),
               as.numeric(c(1, diff(sim$times))))
})


test_that("the training arms keep the times the fit was given", {
  # `build_training_arms()` is what every forecast arm is cut
  # against. It records the training times as integers while the
  # frame keeps the values the user supplied, so on this grid the
  # two describe different occasions and nothing downstream can
  # match a row against them.
  sim <- cont_sim()
  fit_c <- cont_fit()
  training <- mvgam:::build_training_arms(fit_c, sim$series_names)
  for (s in sim$series_names) {
    expect_equal(as.numeric(training$times[[s]]),
                 as.numeric(sim$times))
  }
})


test_that("a continuous-time CAR forecasts its own grid", {
  # `?forecast.mvgam` exempts CAR from the contiguity every other
  # trend obeys, because it carries the elapsed gap into its kernel.
  # This is the grid that exemption exists for.
  sim <- cont_sim()
  fit_c <- cont_fit()
  h <- 4L
  future_times <- max(sim$times) + cumsum(c(2.3, 3.1, 1.7, 4.2))
  future <- data.frame(
    time = rep(future_times, times = sim$n_series),
    series = factor(rep(sim$series_names, each = h),
                    levels = sim$series_names),
    temp = 0, y = NA_integer_
  )
  fc <- forecast(fit_c, newdata = future, ndraws = 50L)
  expect_s3_class(fc, "mvgam_forecast")
  expect_identical(names(fc$forecasts), sim$series_names)
  for (s in sim$series_names) {
    expect_identical(dim(fc$forecasts[[s]]), c(50L, h))
    expect_true(all(is.finite(fc$forecasts[[s]])))
  }
})


test_that("lfo_cv admits the occasions the fit was given", {
  # The window is named by a time, so the times the fit holds are the
  # values that can name one. Measured here, the accepted set is
  # their truncation instead: an occasion the user never observed is
  # taken, and every occasion they did observe is refused for not
  # being whole.
  sim <- cont_sim()
  fit_c <- cont_fit()
  observed <- sim$times[20L]
  fabricated <- 50L

  expect_false(fabricated %in% sim$times)
  expect_true(fabricated %in% floor(sim$times))

  # An occasion the fit holds is a legitimate window boundary.
  expect_s3_class(lfo_cv(fit_c, min_t = observed), "mvgam_lfo")
  # One it does not hold is not, whatever its truncation matches.
  expect_error(lfo_cv(fit_c, min_t = fabricated),
               "not an observed time")
})


test_that("lfo_cv reports the occasions it scored at", {
  # The default call raises nothing, so the labels are the only place
  # the grid it used is visible. `eval_timepoints` is what a reader
  # consults to learn where the model was scored, and each entry has
  # to name an occasion the fit holds.
  sim <- cont_sim()
  fit_c <- cont_fit()
  seen <- character(0)
  lfo <- withCallingHandlers(
    lfo_cv(fit_c),
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  ev <- lfo$eval_timepoints
  expect_gt(length(ev), 0L)
  expect_true(all(ev %in% sim$times))
})


test_that("every panel draws the occasions the frame supplied", {
  # The series panel draws the user's times. The trend panel and the
  # hindcast draw their truncation, so the three cannot be read
  # against one another and each is labelled `Time`.
  sim <- cont_sim()
  fit_c <- cont_fit()
  want <- range(sim$times)
  drawn_x <- function(p) {
    xs <- unlist(lapply(
      ggplot2::ggplot_build(p)$data,
      function(l) if ("x" %in% names(l)) l$x else NULL
    ))
    xs <- xs[is.finite(xs)]
    expect_gt(length(xs), 0L)
    range(xs)
  }
  for (ty in c("series", "trend")) {
    expect_equal(drawn_x(plot(fit_c, type = ty)), as.numeric(want))
  }
  expect_equal(drawn_x(plot(hindcast(fit_c, ndraws = 50L), series = 1)),
               as.numeric(want))
})


test_that("the trend panels follow the model's series order", {
  # Declared north, east. Every other per-series surface keeps that
  # order, so a trend panel placed beside a series panel has to hold
  # the same series in the same position.
  sim <- cont_sim()
  fit_c <- cont_fit()
  panel_order <- function(p) {
    lay <- ggplot2::ggplot_build(p)$layout$layout
    col <- intersect(c("series", "trend"), names(lay))
    as.character(lay[[if (length(col)) col[1L] else 1L]])
  }
  expect_identical(panel_order(plot(fit_c, type = "series")),
                   sim$series_names)
  expect_identical(panel_order(plot(fit_c, type = "trend")),
                   sim$series_names)
})


# ----------------------------------------------------------------------
# The trend that must ignore the gaps
# ----------------------------------------------------------------------
#
# `ZMVN()` is the other side of the claim this file opens with. It is
# `MVN(0, Sigma)` with the covariance indexed by series alone, so the
# spacing of the occasions never enters the likelihood at all, and it
# takes no `requires_regular_intervals` rule. That is what makes the
# pair worth keeping on one grid: `CAR()` has to read the gaps and
# `ZMVN()` has to be unmoved by them.

zmvn_sim <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    set.seed(2026L)
    n_series <- 3L
    series_names <- paste0("s", seq_len(n_series))
    # Drop 3, 7 and 11 from 1:15, giving gaps of 1 and 2.
    unique_times <- setdiff(seq_len(15L), c(3L, 7L, 11L))
    Sigma_true <- matrix(c(
      1.0, 0.6, -0.3,
      0.6, 1.0, 0.2,
      -0.3, 0.2, 1.0
    ), nrow = 3L, byrow = TRUE)
    L_true <- chol(Sigma_true)

    rows <- list()
    for (t in unique_times) {
      eta <- t(L_true) %*% rnorm(n_series)
      for (s in seq_len(n_series)) {
        rows[[length(rows) + 1L]] <- data.frame(
          series = series_names[s], time = t,
          y = 0.5 + eta[s] + rnorm(1L, sd = 0.4)
        )
      }
    }
    d <- do.call(rbind, rows)
    d$series <- factor(d$series, levels = series_names)
    cached <<- list(
      data = d, n_series = n_series, series_names = series_names,
      unique_times = unique_times, Sigma_true = Sigma_true,
      cor_true = stats::cov2cor(Sigma_true)
    )
    cached
  }
})

zmvn_fit <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    path <- cache_path("val_mvgam_zmvn_irregular.rds")
    if (file.exists(path)) {
      cached <<- readRDS(path)
      return(cached)
    }
    cached <<- mvgam(
      formula = y ~ 1, trend_formula = ~ ZMVN(cor = TRUE),
      data = zmvn_sim()$data, family = gaussian(),
      chains = 2L, burnin = 300L, samples = 300L,
      silent = 2, refresh = 0
    )
    saveRDS(cached, path)
    cached
  }
})


test_that("ZMVN accepts the grid CAR needs the gaps of", {
  sim <- zmvn_sim()
  # The grid is irregular, or this file is testing a regular one
  # twice.
  gaps <- diff(sort(sim$unique_times))
  expect_gt(length(unique(gaps)), 1L)
  fit <- zmvn_fit()
  expect_s3_class(fit, "mvgam")
  # The occasions the fit kept are the ones the frame supplied, in
  # their own values rather than their ranks.
  expect_identical(
    as.integer(mvgam:::mvgam_axes(fit)$time$values),
    as.integer(sim$unique_times)
  )
})


test_that("ZMVN recovers the cross-series correlation", {
  # The headline surface for this trend. Recovering the off-diagonals
  # as a set is not enough on its own, so the labelled ordering is
  # checked with it: a permuted axis leaves the same three numbers in
  # a different arrangement.
  sim <- zmvn_sim()
  rc <- residual_cor(zmvn_fit())
  expect_identical(rownames(rc$cor), sim$series_names)
  expect_identical(colnames(rc$cor), sim$series_names)
  expect_equal(unname(diag(rc$cor)), rep(1, sim$n_series))

  off <- upper.tri(rc$cor)
  # Three series give three off-diagonals, and a correlation between
  # three numbers carries almost no information: measured here, the
  # correct ordering scores 0.38 against the truth while permuting
  # the axis scores 0.61. A threshold on that statistic would have
  # preferred the wrong answer, so it is not the claim.
  #
  # What does separate them is the sign of each pair. The simulation
  # runs -0.3, 0.2 and 0.6, so one pair is negative and two are
  # positive; the fit reproduces that pattern and a permuted axis
  # does not.
  expect_identical(sign(rc$cor[off]) > 0, sim$cor_true[off] > 0)
  for (perm in list(c(2L, 3L, 1L), c(3L, 1L, 2L))) {
    shuffled <- rc$cor[perm, perm]
    expect_false(identical(sign(shuffled[off]) > 0,
                           sim$cor_true[off] > 0))
  }
  # Twelve occasions over three series is too little to pin the
  # magnitudes, so they are bounded rather than matched: every entry
  # stays a correlation and the matrix is not the identity.
  expect_true(all(rc$cor[off] > -1 & rc$cor[off] < 1))
  expect_gt(max(abs(rc$cor[off])), 0.05)
})


test_that("every post-fit method answers on the irregular ZMVN fit", {
  fit <- zmvn_fit()
  sim <- zmvn_sim()
  n_obs <- nrow(sim$data)
  n_time <- length(sim$unique_times)

  txt <- capture.output(summary(fit))
  expect_true(any(grepl(paste0("Series:\\s*", sim$n_series), txt)))
  expect_gt(length(capture.output(print(fit))), 5L)

  ep <- posterior_epred(fit, draw_ids = 1:30)
  pp <- posterior_predict(fit, draw_ids = 1:30)
  expect_identical(dim(ep), c(30L, n_obs))
  expect_identical(dim(pp), c(30L, n_obs))
  expect_true(all(is.finite(ep)))
  # A gaussian draw is wider than its own expectation.
  expect_gt(stats::sd(as.numeric(pp)), stats::sd(as.numeric(ep)))

  expect_identical(nrow(predict(fit, type = "link",
                                summary = FALSE)),
                   as.integer(ndraws(fit)))
  ll <- log_lik(fit, draw_ids = 1:30)
  expect_identical(dim(ll), c(30L, n_obs))
  expect_true(all(is.finite(ll)))

  seen <- character(0)
  ic <- withCallingHandlers(loo(fit), warning = function(w) {
    seen <<- c(seen, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  expect_true(is.finite(ic$estimates["elpd_loo", "Estimate"]))
  expect_true(all(grepl("Pareto", seen)))

  # A hindcast covers the occasions the frame supplied, per series.
  hc <- hindcast(fit)
  expect_identical(names(hc$hindcasts), sim$series_names)
  for (s in sim$series_names) {
    expect_identical(ncol(hc$hindcasts[[s]]), n_time)
  }

  rs <- residuals(fit)
  expect_identical(nrow(rs), n_obs)
  aug <- augment(fit)
  expect_identical(nrow(aug), n_obs)
  expect_equal(as.numeric(aug$.observed), as.numeric(sim$data$y))
  expect_identical(nrow(glance(fit)), 1L)
  expect_gt(nrow(tidy(fit)), 0L)
})


test_that("the ZMVN panels draw the occasions the frame supplied", {
  # This fit carries no smooth, so `plot(type = "smooths")` has to
  # refuse. An empty panel would satisfy any class check while
  # telling the reader nothing.
  fit <- zmvn_fit()
  sim <- zmvn_sim()
  rng <- range(sim$unique_times)
  for (ty in c("trend", "series")) {
    p <- plot(fit, type = ty)
    expect_s3_class(p, "ggplot")
    xs <- unlist(lapply(
      ggplot2::ggplot_build(p)$data,
      function(l) if ("x" %in% names(l)) l$x else NULL
    ))
    xs <- xs[is.finite(xs)]
    expect_gt(length(xs), 0L)
    expect_equal(range(xs), as.numeric(rng))
  }
  expect_error(plot(fit, type = "smooths"), "no smooth terms")

  pc <- pp_check(fit, ndraws = 30L)
  expect_s3_class(pc, "ggplot")
  expect_gt(sum(vapply(ggplot2::ggplot_build(pc)$data, nrow,
                       integer(1L))), 0L)

  # The observation formula names no predictor, so there is nothing
  # to condition on and the answer is no effects rather than an
  # empty panel.
  ce <- conditional_effects(fit)
  expect_s3_class(ce, "mvgam_conditional_effects")
  expect_length(ce, 0L)
})


cat("\nDone.\n")
