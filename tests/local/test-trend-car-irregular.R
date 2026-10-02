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

# A continuous-time AR in the unit the model measures gaps in, the
# median gap. The damping over a gap of `d` is `phi^d`, the innovation
# scales with it, and the first state comes from the stationary law.
time_scale <- stats::median(gaps)
model_gaps <- gaps / time_scale
phi_true <- 0.75
sigma_true <- 0.35
car_step_sd <- function(phi, sigma, d) {
  sigma * sqrt((1 - phi^(2 * d)) / (1 - phi^2))
}
latent <- matrix(0, nrow = n_time, ncol = n_series)
for (s in seq_len(n_series)) {
  latent[1L, s] <- rnorm(1L, 0, sigma_true / sqrt(1 - phi_true^2))
  for (t in 2:n_time) {
    d <- model_gaps[t - 1L]
    latent[t, s] <- phi_true^d * latent[t - 1L, s] +
      rnorm(1L, 0, car_step_sd(phi_true, sigma_true, d))
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
  gaps = gaps, time_scale = time_scale, phi_true = phi_true,
  sigma_true = sigma_true, latent = latent
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


test_that("time_dis is the measured gap in units of the median gap", {
  # The array the CAR recursion raises its damping to the power of.
  # A derivation that counted occasions returns a column of ones and
  # keeps every dimension, every index and every prior intact, so a
  # shape check cannot see it and only the values can.
  sd <- prefit$standata
  td <- sd$time_dis
  expect_identical(dim(td), c(n_time, n_series))

  # Step `t` holds the distance from `t - 1`. The first has no
  # predecessor and takes 1, because Stan raises the damping to this
  # power.
  expected <- c(1, model_gaps)
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
  # The gaps span the user's own grid
  expect_equal(sum(as.numeric(td[-1L, 1L])) * time_scale,
               as.numeric(max(time_vals) - min(time_vals)))
  # The same grid in another time unit gives the same gaps
  dat_scaled <- transform(dat, time = time * 7.3)
  scaled <- mvgam(
    formula = y ~ temp, trend_formula = ~ CAR(),
    data = dat_scaled, family = poisson(), run_model = FALSE, silent = 2
  )
  expect_equal(scaled$standata$time_dis, td)
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
    "on a single series only"
  )
  expect_match(conditionMessage(plain), "trend_formula", fixed = TRUE)

  by_lv <- expect_error(
    mvgam(
      formula = y ~ temp,
      trend_formula = ~ s(temp, k = 4, by = lv_axis()) - 1 + CAR(),
      data = dat, family = poisson(), run_model = FALSE, silent = 2
    ),
    "on a single series only"
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
               as.numeric(c(1, model_gaps)))
})


test_that("CAR refuses a factor decomposition, by every route", {
  # A continuous-time trend evolves per series and has no factor
  # decomposition. The refusal is registered against the trend type
  # itself, for every route a user can ask by: a `trend_map` naming
  # fewer factors than series, `jsdgam()`, which is a factor model by
  # construction, and `n_lv` on the constructor.
  map_err <- expect_error(
    mvgam(
      formula = y ~ temp, trend_formula = ~ CAR(),
      trend_map = matrix(NA_real_, nrow = n_series, ncol = 2L),
      data = dat, family = poisson(), run_model = FALSE, silent = 2
    ),
    "Factor models are not supported for CAR trends"
  )
  # The refusal carries the reason the registry records against CAR,
  # and names trends that do decompose, so the user has somewhere to
  # go rather than only a closed door.
  expect_match(conditionMessage(map_err), "irregular time gaps",
               fixed = TRUE)
  expect_match(conditionMessage(map_err), "use a trend with a factor form",
               fixed = TRUE)

  jsd_err <- expect_error(
    jsdgam(
      formula = y ~ 1, factor_formula = ~ -1 + CAR(),
      data = dat, unit = time, species = series,
      family = poisson(), n_lv = 2L, run_model = FALSE, silent = 2
    ),
    "Factor models are not supported for CAR trends"
  )
  # One rule, one message, whichever route asked. The constructor is
  # the third spelling, and it composes from the same registry entry.
  expect_identical(conditionMessage(jsd_err),
                   conditionMessage(map_err))
  expect_identical(conditionMessage(expect_error(CAR(n_lv = 2))),
                   conditionMessage(map_err))
})


test_that("n_lv is the third route to a factor CAR, and refuses too", {
  # The same request as the two above, written the third way a user
  # can write it. It was accepted and `n_lv` raised to the series
  # count, so a user who asked for two factors was handed a
  # saturated trend and reading `N_lv_trend` back was the only way
  # to notice.
  #
  # This route is refused on where the argument was written rather
  # than on the trend: `n_lv` passed to `mvgam()` reaches the
  # arguments forwarded to brms and Stan, which read no factor
  # count, so it is misplaced whatever trend it was paired with.
  # The two routes above carry the request as far as a trend and are
  # answered by the trend registry.
  expect_error(
    mvgam(
      formula = y ~ temp, trend_formula = ~ CAR(), n_lv = 2L,
      data = dat, family = poisson(), run_model = FALSE, silent = 2
    ),
    "takes 'n_lv' on the trend constructor"
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
  info <- list(time_axis = list(values = as.numeric(dup_times)),
               n_series = n_series)
  td <- mvgam:::calculate_car_time_distances(info)$time_dis
  expect_true(all(as.numeric(td) > 0))
  expect_true(all(is.finite(as.numeric(td))))
})


test_that("the refusals carry messages a user can act on", {
  # A refusal is only useful if it names what was wrong and what to
  # do instead. Each of these is matched on its own wording rather
  # than on "some error happened", because an alternation over
  # several patterns passes on whichever one happens to match and
  # stops telling apart the conditions it was written to separate.

  # A series missing an occasion the others have. `CAR()` holds a
  # state for that series there, and the frame is the model its
  # NA-padded form gives. A trend that steps by position refuses it
  # and names the trend that takes it.
  ragged <- dat[!(as.character(dat$series) == series_levels[2L] &
                    dat$time == time_vals[5L]), , drop = FALSE]
  expect_identical(nrow(ragged), nrow(dat) - 1L)
  completed <- mvgam(
    formula = y ~ temp, trend_formula = ~ CAR(),
    data = ragged, family = poisson(), run_model = FALSE, silent = 2
  )
  expect_identical(as.integer(completed$standata$N_trend),
                   n_time * n_series)
  expect_identical(as.integer(completed$standata$N), nrow(ragged))
  expect_equal(completed$standata$time_dis, prefit$standata$time_dis)
  zmvn_err <- expect_error(
    mvgam(
      formula = y ~ temp, trend_formula = ~ ZMVN(),
      data = ragged, family = poisson(), run_model = FALSE,
      silent = 2
    ),
    "must share one time grid"
  )
  msg <- conditionMessage(zmvn_err)
  # The message gives the cells supplied and the cells a full grid has
  expect_match(msg, as.character(nrow(ragged)), fixed = TRUE)
  expect_match(msg, as.character(n_time * n_series), fixed = TRUE)
  # It names the remedy and the trend that needs none
  expect_match(msg, "NA", fixed = TRUE)
  expect_match(msg, "'CAR()' takes series observed at their own times",
               fixed = TRUE)

  # A prediction frame with no time column. The time is the one
  # thing a forecast grid cannot do without, and the message names
  # the column it wanted rather than failing later on a dimension.
  training <- mvgam:::build_training_arms(prefit, series_levels)
  no_time <- dat
  no_time$time <- NULL
  time_err <- expect_error(
    mvgam:::resolve_forecast_grid(prefit, no_time, training,
                                  series_levels),
    "Column 'time' is absent"
  )
  expect_match(conditionMessage(time_err), "time", fixed = TRUE)
  # And it lists what the frame did carry, so the user can see the
  # spelling they used.
  expect_match(conditionMessage(time_err), "series", fixed = TRUE)
})


# -- Fit --------------------------------------------------------------

fit <- cached_fit(
  "val_mvgam_car_irregular.rds",
  function() {
    mvgam(
      formula = y ~ temp, trend_formula = ~ CAR(),
      data = dat, family = poisson(),
      chains = 2L, iter = 1000L, warmup = 500L,
      silent = 2, backend = "cmdstanr"
    )
  },
  key = sim_truth
)


test_that("the fitted CAR keeps the irregular grid", {
  expect_identical(as.integer(mvgam:::mvgam_axes(fit)$time$values),
                   time_vals)
  expect_equal(as.numeric(fit$standata$time_dis[, 1L]),
               as.numeric(c(1, model_gaps)))
})


test_that("the damping recovers the simulated one", {
  # A recursion that stepped once per occasion rather than over the
  # gap would need a much smaller per-step damping to fit the same
  # data, so this number moves if the gaps are being ignored.
  dm <- posterior::as_draws_matrix(fit$fit)
  ar_cols <- grep("^ar1_trend\\[", colnames(dm), value = TRUE)
  expect_length(ar_cols, n_series)
  # Twenty-six occasions leave the prior much of its weight, and the
  # claim is that each interval holds the simulated value.
  for (col in ar_cols) {
    bounds <- stats::quantile(as.numeric(dm[, col]), c(0.025, 0.975))
    expect_lt(bounds[[1L]], phi_true)
    expect_gt(bounds[[2L]], phi_true)
  }
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
  # The training frame itself names no occasion beyond the grid, so
  # there is no horizon in it to resolve.
  expect_error(
    mvgam:::resolve_forecast_grid(fit, dat, training, series_levels),
    "ends at or before the last training time"
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


test_that("the CAR innovation scales with the gap it spans", {
  # `car1_recursC()` steps the state as
  #   phi^dt * prev + sigma * sqrt((1 - phi^(2 dt)) / (1 - phi^2)) * z
  # The sampled program describes that same process. Ground truth
  # here is that formula, taken from the stationary continuous-time
  # AR(1). Building it from the generated program would test the
  # program against itself. The first state comes from the
  # stationary marginal, sigma / sqrt(1 - phi^2). Stored draws keep
  # about nine significant digits.
  dm <- posterior::as_draws_matrix(fit$fit)
  td <- fit$standata$time_dis
  for (s in seq_along(series_levels)) {
    phi <- as.numeric(dm[, paste0("ar1_trend[", s, "]")])
    sg <- as.numeric(dm[, paste0("sigma_trend[", s, "]")])
    lv1 <- as.numeric(dm[, paste0("lv_trend[1,", s, "]")])
    z1 <- as.numeric(dm[, paste0("innovations_trend[1,", s, "]")])
    expect_equal(lv1, sg * z1 / sqrt(1 - phi^2), tolerance = 1e-6)
    worst <- 0
    for (i in 2:n_time) {
      lv_i <- as.numeric(dm[, paste0("lv_trend[", i, ",", s, "]")])
      lv_p <- as.numeric(dm[, paste0("lv_trend[", i - 1L, ",", s, "]")])
      z_i <- as.numeric(dm[, paste0("innovations_trend[", i, ",", s, "]")])
      dt <- as.numeric(td[i, s])
      step <- phi^dt * lv_p +
        sg * sqrt((1 - phi^(2 * dt)) / (1 - phi^2)) * z_i
      worst <- max(worst, max(abs(lv_i - step)))
    }
    expect_lt(worst, 1e-6)
  }
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
    "Series in 'newdata' has levels absent from the training data"
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
  first_gap <- (future_times[1L] - max(time_vals)) / time_scale
  for (k in seq_along(series_levels)) {
    ar1 <- as.numeric(dm[, paste0("ar1_trend[", k, "]")])
    last <- as.numeric(dm[, paste0("trend[", n_time, ",", k, "]")])
    with_gap <- mean(ar1^first_gap * last)
    got <- mean(fc$forecasts[[series_levels[k]]][, 1L])
    expect_lt(abs(got - with_gap), 0.4)
  }
})


test_that("the criticism surface refuses an argument nothing reads", {
  # Same claim as the prediction methods, on the ones a reader
  # reaches for after them.
  expect_error(fitted(fit, zzz_unknown = 1))
  expect_error(log_lik(fit, zzz_unknown = 1))
  expect_error(plot(fit, zzz_unknown = 1))
  expect_error(posterior_linpred(fit, zzz_unknown = 1))
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
  caught <- with_warnings(loo(fit))
  ic <- caught$value
  loo_warnings <- caught$warnings
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
  # The refusal names the occasions supplied and the last one each
  # series was seen at, so the caller can tell which of the two they
  # got wrong.
  expect_error(
    forecast(fit, newdata = mk(c(30L, 31L)), ndraws = 20L),
    "ends at or before the last training time"
  )
  msg <- tryCatch(
    forecast(fit, newdata = mk(c(30L, 31L)), ndraws = 20L),
    error = conditionMessage
  )
  expect_true(grepl("30", msg, fixed = TRUE))
  expect_true(grepl(as.character(max(time_vals)), msg, fixed = TRUE))
})


test_that("the fit answers the standard model accessors", {
  expect_s3_class(model.frame(fit), "data.frame")
  expect_s3_class(formula(fit), "formula")
  expect_s3_class(insight::get_data(fit), "data.frame")
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
  # With `class = NULL`, `hypothesis()` takes a parameter by the name
  # `variables()` lists
  expect_s3_class(hypothesis(fit, "ar1_trend[1] > 0", class = NULL),
                  "brmshypothesis")
  expect_s3_class(hypothesis(fit, "temp > 0"), "brmshypothesis")
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
                    n_series = n_s, phi = phi,
                    time_scale = stats::median(diff(tv)))
    cached
  }
})

cont_fit <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    cached <<- cached_fit("val_mvgam_car_continuous.rds", function() {
      mvgam(
        formula = y ~ temp, trend_formula = ~ CAR(),
        data = cont_sim()$data, family = poisson(),
        chains = 2L, iter = 1000L, warmup = 500L,
        silent = 2, backend = "cmdstanr"
      )
    }, key = cont_sim()$time_scale)
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
  # The damping is raised to the measured gaps, in median-gap units
  expect_equal(as.numeric(fit_c$standata$time_dis[, 1L]),
               as.numeric(c(1, diff(sim$times) / sim$time_scale)))
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
               "must be an observed time")
})


test_that("lfo_cv reports the occasions it scored at", {
  # The default call raises nothing, so the labels are the only place
  # the grid it used is visible. `eval_timepoints` is what a reader
  # consults to learn where the model was scored, and each entry has
  # to name an occasion the fit holds.
  sim <- cont_sim()
  fit_c <- cont_fit()
  seen <- character(0)
  caught <- with_warnings(lfo_cv(fit_c))
  lfo <- caught$value
  seen <- c(seen, caught$warnings)
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
  expect_identical(panel_order(plot(fit_c, type = "series")),
                   sim$series_names)
  expect_identical(panel_order(plot(fit_c, type = "trend")),
                   sim$series_names)
})


# ----------------------------------------------------------------------
# Correlated series on the continuous grid
# ----------------------------------------------------------------------
#
# `CAR(cor = TRUE)` correlates the shocks of the series. The series
# here damp at different rates under a strong correlation, and some
# gaps are below one, where a correlation placed on the unit-gap
# innovations fails to give a positive definite covariance. One series
# has no response at the last two occasions, and its forecast starts
# at the end of the grid with the others.

cor_sim <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    set.seed(4242L)
    series_names <- c("west", "north", "east")
    k <- length(series_names)
    tv <- cumsum(c(0.5, round(stats::runif(79L, 0.3, 4.8), 3L)))
    n_t <- length(tv)
    scale <- stats::median(diff(tv))
    gaps <- diff(tv) / scale
    phi <- c(0.9, 0.3, 0.6)
    sigma <- c(0.5, 0.6, 0.4)
    rate <- -log(phi)
    cor_instant <- matrix(c(1, 0.8, -0.5, 0.8, 1, -0.3, -0.5, -0.3, 1), k)
    gain <- sqrt(2 * rate / (1 - phi^2))
    Sigma <- cor_instant * tcrossprod(sigma * gain) *
      (1 - tcrossprod(phi)) / outer(rate, rate, "+")
    gamma <- Sigma / (1 - tcrossprod(phi))
    lat <- matrix(0, n_t, k)
    lat[1L, ] <- t(chol(gamma)) %*% stats::rnorm(k)
    for (t in 2:n_t) {
      step_cov <- gamma * (1 - tcrossprod(phi)^gaps[t - 1L])
      lat[t, ] <- phi^gaps[t - 1L] * lat[t - 1L, ] +
        t(chol(step_cov)) %*% stats::rnorm(k)
    }
    d <- data.frame(
      time = rep(tv, times = k),
      series = factor(rep(series_names, each = n_t), levels = series_names)
    )
    d$y <- stats::rpois(nrow(d), exp(1.5 + as.vector(lat)))
    d$y[d$series == "north" & d$time >= tv[n_t - 1L]] <- NA
    cached <<- list(
      data = d, times = tv, series_names = series_names, n_series = k,
      time_scale = scale, gaps = gaps, phi = phi, sigma = sigma,
      cor_instant = cor_instant
    )
    cached
  }
})

cor_fit <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    cached <<- cached_fit("val_mvgam_car_correlated.rds", function() {
      # The frame holds two missing responses, and the fit says so
      caught <- with_warnings(mvgam(
        formula = y ~ 1, trend_formula = ~ CAR(cor = TRUE),
        data = cor_sim()$data, family = poisson(),
        chains = 2L, iter = 1000L, warmup = 500L,
        silent = 2, backend = "cmdstanr"
      ))
      stopifnot(all(grepl("Rows containing NAs", caught$warnings)))
      caught$value
    }, key = cor_sim()[c("times", "phi", "sigma", "cor_instant")])
    cached
  }
})


test_that("the correlated CAR program is the exact continuous-time law", {
  sim <- cor_sim()
  fit_k <- cor_fit()
  k <- sim$n_series
  n_t <- length(sim$times)
  sd <- fit_k$standata
  # One gap entry per distinct gap, in median-gap units, some below one
  expect_equal(as.numeric(sd$gap_trend[sd$gap_index_trend][-1L]),
               sim$gaps, tolerance = 1e-8)
  expect_lt(min(sim$gaps), 1)

  # Ground truth is the process itself: the stored states are rebuilt
  # from the sampled damping, scales, instantaneous correlation and
  # innovations. Stored draws keep about six significant digits.
  dm <- posterior::as_draws_matrix(fit_k$fit)
  cells <- function(name, nr, nc) {
    paste0(name, "[", rep(seq_len(nr), nc), ",",
           rep(seq_len(nc), each = nr), "]")
  }
  worst <- c(Sigma = 0, state = 0)
  for (r in c(1L, 250L, 500L, 750L, 1000L)) {
    phi <- as.numeric(dm[r, paste0("ar1_trend[", seq_len(k), "]")])
    sig <- as.numeric(dm[r, paste0("sigma_trend[", seq_len(k), "]")])
    L <- matrix(as.numeric(dm[r, cells("L_Omega_trend", k, k)]), k, k)
    z <- matrix(as.numeric(dm[r, cells("innovations_trend", n_t, k)]),
                n_t, k)
    rate <- -log(phi)
    gain <- sqrt(2 * rate / (1 - phi^2))
    Sigma <- tcrossprod(L) * tcrossprod(sig * gain) *
      (1 - tcrossprod(phi)) / outer(rate, rate, "+")
    diag(Sigma) <- sig^2
    worst["Sigma"] <- max(worst["Sigma"], max(abs(
      Sigma - matrix(as.numeric(dm[r, cells("Sigma_trend", k, k)]), k, k)
    )))
    gamma <- Sigma / (1 - tcrossprod(phi))
    x <- matrix(NA_real_, n_t, k)
    x[1L, ] <- t(chol(gamma)) %*% z[1L, ]
    for (i in 2:n_t) {
      d <- sim$gaps[i - 1L]
      x[i, ] <- phi^d * x[i - 1L, ] +
        t(chol(gamma * (1 - tcrossprod(phi)^d))) %*% z[i, ]
    }
    worst["state"] <- max(worst["state"], max(abs(
      x - matrix(as.numeric(dm[r, cells("trend", n_t, k)]), n_t, k)
    )))
  }
  expect_lt(worst[["Sigma"]], 1e-6)
  expect_lt(worst[["state"]], 1e-5)
})


test_that("a correlated CAR forecast is the exact conditional law", {
  sim <- cor_sim()
  fit_k <- cor_fit()
  k <- sim$n_series
  n_t <- length(sim$times)
  # The series without responses at the end of the grid is recorded as
  # last observed earlier, and every series is stepped from the grid end
  expect_lt(mvgam:::mvgam_axes(fit_k)$series$last_time[2L], max(sim$times))
  leads <- c(0.05, 0.7, 2.5, 40) * sim$time_scale
  future <- data.frame(
    time = rep(max(sim$times) + leads, times = k),
    series = factor(rep(sim$series_names, each = length(leads)),
                    levels = sim$series_names),
    y = NA_integer_
  )
  fc <- forecast(fit_k, newdata = future, type = "trend")
  draws <- simplify2array(fc$forecasts)
  dm <- posterior::as_draws_matrix(fit_k$fit)
  phi <- unclass(dm[, paste0("ar1_trend[", seq_len(k), "]")])
  last <- unclass(dm[, paste0("trend[", n_t, ",", seq_len(k), "]")])
  dim(phi) <- dim(last) <- c(nrow(dm), k)
  for (h in seq_along(leads)) {
    gap <- leads[h] / sim$time_scale
    cond_mean <- phi^gap * last
    cond_cov <- stats::cov(cond_mean)
    for (r in seq_len(nrow(dm))) {
      p <- as.numeric(phi[r, ])
      Sigma <- matrix(as.numeric(dm[r, paste0(
        "Sigma_trend[", rep(seq_len(k), k), ",",
        rep(seq_len(k), each = k), "]"
      )]), k, k)
      cond_cov <- cond_cov + Sigma * (1 - tcrossprod(p)^gap) /
        (1 - tcrossprod(p)) / nrow(dm)
    }
    got <- stats::cov(draws[, h, ])
    # Spread within 10 percent and correlation within 0.1 at every
    # lead, the shortest a twentieth of the median gap
    expect_lt(max(abs(sqrt(diag(got) / diag(cond_cov)) - 1)), 0.1)
    expect_lt(max(abs(stats::cov2cor(got) - stats::cov2cor(cond_cov))), 0.1)
    expect_lt(max(abs(colMeans(draws[, h, ]) - colMeans(cond_mean))), 0.1)
  }

  # A time the model was fitted on is a hindcast
  inside <- future
  inside$time <- rep(c(sim$times[n_t], max(sim$times) + leads[-1L]), k)
  expect_error(forecast(fit_k, newdata = inside),
               "begin after the last fitted time")
})


test_that("the correlated CAR reports the correlation of its trends", {
  sim <- cor_sim()
  fit_k <- cor_fit()
  rc <- residual_cor(fit_k)
  expect_identical(rownames(rc$cor), sim$series_names)
  # The stationary correlation, from the stored covariance and damping
  dm <- posterior::as_draws_matrix(fit_k$fit)
  k <- sim$n_series
  want <- matrix(0, k, k)
  for (r in seq_len(nrow(dm))) {
    p <- as.numeric(dm[r, paste0("ar1_trend[", seq_len(k), "]")])
    Sigma <- matrix(as.numeric(dm[r, paste0(
      "Sigma_trend[", rep(seq_len(k), k), ",", rep(seq_len(k), each = k), "]"
    )]), k, k)
    want <- want + stats::cov2cor(Sigma / (1 - tcrossprod(p))) / nrow(dm)
  }
  expect_equal(unname(rc$cor), want, tolerance = 0.02)
  # The simulated signs: west and north together, east against both
  off <- upper.tri(rc$cor)
  expect_identical(sign(rc$cor[off]), sign(sim$cor_instant[off]))

  expect_s3_class(lfo_cv(fit_k, min_t = sim$times[length(sim$times) - 3L]),
                  "mvgam_lfo")
  for (ty in c("trend", "series")) {
    expect_equal(drawn_x(plot(fit_k, type = ty)), range(sim$times))
  }
})



# ----------------------------------------------------------------------
# Series observed at their own times
# ----------------------------------------------------------------------
#
# The correlated frame above with each series keeping its own share of
# the times. The trend holds a state for every series at each time any
# series was observed, and a series enters the likelihood where it has
# a row.

own_sim <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    sim <- cor_sim()
    set.seed(5150L)
    d <- sim$data[!is.na(sim$data$y), ]
    d <- d[stats::runif(nrow(d)) < 0.6, ]
    d <- d[sample(nrow(d)), ]
    union_times <- sort(unique(d$time))
    padded <- merge(
      expand.grid(time = union_times,
                  series = factor(sim$series_names,
                                  levels = sim$series_names)),
      d, all.x = TRUE
    )
    cached <<- list(data = d, padded = padded, union_times = union_times,
                    series_names = sim$series_names, n_series = sim$n_series)
    cached
  }
})

own_fit <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    cached <<- cached_fit("val_mvgam_car_own_times.rds", function() {
      mvgam(
        formula = y ~ 1, trend_formula = ~ CAR(cor = TRUE),
        data = own_sim()$data, family = poisson(),
        chains = 2L, iter = 1000L, warmup = 500L,
        silent = 2, backend = "cmdstanr"
      )
    }, key = own_sim()$data)
    cached
  }
})


test_that("series at their own times are the NA-padded model", {
  sim <- own_sim()
  # The premise: the series miss different times
  per_series <- table(sim$data$series)
  expect_true(all(per_series < length(sim$union_times)))

  car <- mvgam_formula(y ~ 1, trend_formula = ~ CAR(cor = TRUE))
  sd_own <- standata(car, data = sim$data, family = poisson())
  caught <- with_warnings(standata(car, data = sim$padded,
                                   family = poisson()))
  expect_true(all(grepl("Rows containing NAs", caught$warnings)))
  sd_pad <- caught$value
  # One program, one trend grid and one time unit. The observations
  # are the same cells in the order each frame lists them, and the
  # observation design names its rows by that order.
  shared <- setdiff(names(sd_own),
                    c("Y", "X", "obs_trend_time", "obs_trend_series"))
  expect_identical(sd_own[shared], sd_pad[shared])
  cell <- function(s) paste(s$obs_trend_time, s$obs_trend_series)
  expect_identical(
    as.numeric(sd_own$Y)[order(cell(sd_own))],
    as.numeric(sd_pad$Y)[order(cell(sd_pad))]
  )
  expect_identical(
    as.integer(sd_own$N_trend),
    length(sim$union_times) * sim$n_series
  )
  # The unit is the gap between one series' observations, which
  # exceeds the gap between the times of the union
  axis_time <- mvgam:::mvgam_axes(own_fit())$time
  expect_gt(axis_time$observation_gap,
            stats::median(diff(sim$union_times)))
})


test_that("each surface reports a series at its own times", {
  sim <- own_sim()
  fit_o <- own_fit()
  dm <- posterior::as_draws_matrix(fit_o$fit)
  cell_state <- function(time, series) {
    as.numeric(dm[, paste0(
      "trend[", match(time, sim$union_times), ",",
      match(series, sim$series_names), "]"
    )])
  }
  intercept <- as.numeric(dm[, "b_Intercept"])

  hc <- hindcast(fit_o, type = "expected")
  for (s in sim$series_names) {
    own <- sort(sim$data$time[sim$data$series == s])
    expect_equal(as.numeric(hc$train_times[[s]]), own)
    expect_identical(ncol(hc$hindcasts[[s]]), length(own))
    expect_equal(hc$hindcasts[[s]][, 3L],
                 exp(intercept + cell_state(own[3L], s)))
  }
  ep <- posterior_epred(fit_o, incl_autocor = TRUE)
  for (i in c(1L, 40L, nrow(sim$data))) {
    expect_equal(
      ep[, i],
      exp(intercept + cell_state(sim$data$time[i],
                                 as.character(sim$data$series[i])))
    )
  }
  # A time inside the grid at which a series has no row takes the
  # state the model holds there
  first <- sim$series_names[1L]
  absent <- setdiff(sim$union_times,
                    sim$data$time[sim$data$series == first])[2L]
  at_gap <- posterior_linpred(
    fit_o, incl_autocor = TRUE,
    newdata = data.frame(time = absent,
                         series = factor(first, levels = sim$series_names),
                         y = NA_integer_)
  )
  expect_equal(at_gap[, 1L], intercept + cell_state(absent, first))

  for (ty in c("trend", "series")) {
    expect_equal(drawn_x(plot(fit_o, type = ty)), range(sim$union_times))
  }
})


test_that("forecast and its scores take each series at its own times", {
  sim <- own_sim()
  fit_o <- own_fit()
  end <- max(sim$union_times)
  future <- data.frame(
    time = end + c(1, 4, 9, 2, 4, 30),
    series = factor(rep(sim$series_names[1:2], each = 3L),
                    levels = sim$series_names),
    y = c(3L, 5L, 4L, 6L, 2L, 7L)
  )
  fc <- forecast(fit_o, newdata = future, type = "trend")
  expect_equal(as.numeric(fc$test_times[[1L]]), end + c(1, 4, 9))
  expect_equal(as.numeric(fc$test_times[[2L]]), end + c(2, 4, 30))
  expect_identical(ncol(fc$forecasts[[3L]]), 0L)

  # Every series steps from its state at the end of the grid, over
  # the gap to its own forecast time in the fit's unit
  dm <- posterior::as_draws_matrix(fit_o$fit)
  unit <- mvgam:::mvgam_axes(fit_o)$time$observation_gap
  n_t <- length(sim$union_times)
  for (s in 1:2) {
    phi <- as.numeric(dm[, paste0("ar1_trend[", s, "]")])
    last <- as.numeric(dm[, paste0("trend[", n_t, ",", s, "]")])
    scale2 <- as.numeric(dm[, paste0("Sigma_trend[", s, ",", s, "]")])
    leads <- (as.numeric(fc$test_times[[s]]) - end) / unit
    for (h in seq_along(leads)) {
      draws <- fc$forecasts[[s]][, h]
      cond_var <- mean(scale2 * (1 - phi^(2 * leads[h])) / (1 - phi^2)) +
        stats::var(phi^leads[h] * last)
      expect_lt(abs(mean(draws) - mean(phi^leads[h] * last)), 0.15)
      expect_lt(abs(stats::sd(draws) / sqrt(cond_var) - 1), 0.1)
    }
  }

  # The scores of the two series are summed at the one time they share
  sc <- score(forecast(fit_o, newdata = future), "crps")
  expect_identical(nrow(sc$all_series), 5L)
  expect_equal(
    sc$all_series$score[3L],
    sc[[sim$series_names[1L]]]$score[2L] +
      sc[[sim$series_names[2L]]]$score[2L]
  )
  expect_error(score(forecast(fit_o, newdata = future), "energy"),
               "shared forecast horizon")

  lfo <- lfo_cv(fit_o,
                min_t = sim$union_times[length(sim$union_times) - 3L])
  rows_at <- vapply(lfo$eval_timepoints, function(t) {
    sum(sim$data$time == t)
  }, integer(1L))
  expect_identical(lfo$n_obs, rows_at)
  expect_true(all(is.finite(lfo$elpds)))
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
    cached <<- cached_fit("val_mvgam_zmvn_irregular.rds", function() {
      mvgam(
        formula = y ~ 1, trend_formula = ~ ZMVN(cor = TRUE),
        data = zmvn_sim()$data, family = gaussian(),
        chains = 2L, burnin = 300L, samples = 300L,
        silent = 2, refresh = 0
      )
    })
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
  caught <- with_warnings(loo(fit))
  ic <- caught$value
  seen <- c(seen, caught$warnings)
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
  expect_drawn(pc)

  # The observation formula names no predictor, so there is nothing
  # to condition on and the answer is no effects rather than an
  # empty panel.
  ce <- conditional_effects(fit)
  expect_s3_class(ce, "mvgam_conditional_effects")
  expect_length(ce, 0L)
})


cat("\nDone.\n")
