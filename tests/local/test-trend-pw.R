# Recovery and post-fit coverage for `PW()`, fitted in this file.
#
# A piecewise trend is a rate that changes at a fixed set of
# changepoints. What decides the fit is therefore where those
# changepoints sit and how the rate adjustments attach to them, so the
# claims here are about `t_change_trend` and `delta_trend` rather than
# about the fitted line as a whole.
#
# The observation side carries an offset and a two-dimensional
# smooth, neither of which any other local file exercises:
#
#   model: y ~ -1 + offset(log_effort) + s(x1, x2, k = 12),
#          trend_formula = ~ PW(n_changepoints = 8)
#
# The `-1` is deliberate. PW carries its own intercept (`m_trend`),
# and an observation intercept competes with it for the same constant;
# the package warns about exactly that, so the formula is written the
# way the warning asks for.
#
# An offset is a covariate whose coefficient is fixed at one, so it
# has to reach Stan as data in row order. A two-dimensional smooth is
# one basis over two covariates rather than two bases, and a design
# can get that wrong while keeping every dimension intact.
#
# Series are named out of alphabetical order and occasions numbered
# from 3.
#
# Cached at tests/local/fixtures/val_mvgam_pw_trend.rds.
#
# Run with:
#   testthat::test_file("tests/local/test-trend-pw.R")

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

set.seed(1301L)

n_series <- 2L
n_time <- 60L
n_change <- 8L
series_levels <- c("delta", "bravo")
stopifnot(!identical(series_levels, sort(series_levels)))

time_vals <- seq_len(n_time) + 2L

# A piecewise-linear rate: a base slope with three real breaks in it,
# so the changepoint machinery has something to find.
k_true <- c(delta = 0.05, bravo = -0.03)
break_at <- c(15L, 32L, 45L)
break_delta <- c(0.09, -0.14, 0.06)

latent <- matrix(0, nrow = n_time, ncol = n_series)
for (s in seq_len(n_series)) {
  rate <- k_true[s]
  level <- 0
  for (t in seq_len(n_time)) {
    if (t %in% break_at) {
      rate <- rate + break_delta[match(t, break_at)]
    }
    level <- level + rate
    latent[t, s] <- level
  }
}

# `log_effort` varies by row, so an offset applied in the wrong row
# order changes the answer. Exposure between 1 and 4.
effort <- runif(n_time * n_series, 1, 4)

x1 <- rep(as.numeric(scale(rnorm(n_time))), times = n_series)
x2 <- rep(as.numeric(scale(rnorm(n_time))), times = n_series)

dat <- data.frame(
  time = rep(time_vals, times = n_series),
  series = factor(rep(series_levels, each = n_time),
                  levels = series_levels),
  x1 = x1,
  x2 = x2,
  log_effort = log(effort)
)
# A surface that varies in both margins at once, so the smooth has
# something to find that neither margin alone explains.
surface <- 0.4 * sin(dat$x1 * 1.5) * cos(dat$x2 * 1.5)
dat$y <- rpois(
  nrow(dat),
  exp(1.1 + as.numeric(latent) + surface + dat$log_effort)
)

obs_formula <- y ~ -1 + offset(log_effort) + s(x1, x2, k = 12)

sim_truth <- list(
  n_series = n_series, n_time = n_time, n_change = n_change,
  series_levels = series_levels, time_vals = time_vals,
  k_true = k_true, break_at = break_at, break_delta = break_delta,
  latent = latent, effort = effort
)

make_future <- function(h) {
  ft <- max(time_vals) + seq_len(h)
  data.frame(
    time = rep(ft, times = n_series),
    series = factor(rep(series_levels, each = h),
                    levels = series_levels),
    x1 = 0, x2 = 0,
    log_effort = log(2),
    y = NA_integer_
  )
}


# -- Prefit: the changepoints, the offset and the surface ------------

prefit <- mvgam(
  formula = obs_formula,
  trend_formula = ~ PW(n_changepoints = n_change),
  data = dat, family = poisson(), run_model = FALSE, silent = 2
)


test_that("the changepoints are ordered and in the user's time units", {
  # `t_change_trend` is where the rate is allowed to break. Order
  # matters because the changepoint matrix is built by comparing each
  # occasion against these in sequence, and units matter because the
  # comparison is against the time column: values on a `1..n` scale
  # would all fall before the first occasion here, which starts at 3.
  sd <- prefit$standata
  expect_identical(as.integer(sd$N_change_trend), n_change)
  tc <- as.numeric(sd$t_change_trend)
  expect_length(tc, n_change)
  expect_false(is.unsorted(tc))
  expect_true(all(is.finite(tc)))

  # Inside the observed span, and expressed on it.
  expect_gte(min(tc), min(time_vals))
  expect_lte(max(tc), max(time_vals))
  # They are not simply 1..n_change, which is what a rank-based
  # placement would produce.
  expect_false(isTRUE(all.equal(tc, as.numeric(seq_len(n_change)))))
  # Distinct, or two breaks would sit on one occasion and the model
  # would be over-parameterised in a way nothing else reports.
  expect_identical(length(unique(tc)), n_change)

  # The changepoints stop short of the end of the series, which is
  # what `changepoint_range` is for: a break in the final occasions
  # has almost no data after it to identify the new rate.
  expect_lt(max(tc), max(time_vals))
})


test_that("the changepoints ignore occasions carrying no response", {
  # A changepoint grid is a proportion of the *history*, which `?PW`
  # says and which the placement did not do: it was
  # `floor(N_time_trend * changepoint_range)`, and the trend grid
  # reaches past the last response whenever a frame is padded with
  # unobserved rows or a `newdata` extends it. On 30 observed
  # occasions the breaks sat at 6, 10, 15, 19 and 24 with one further
  # row in the frame and at 12, 23, 34, 45 and 56 with forty, so the
  # model a user got depended on how much future they happened to
  # carry alongside it.
  #
  # Padding is the shape that shows it, because mvgam asks for a
  # ragged panel to be padded and because a `newdata` does the same
  # thing to the grid.
  baseline <- as.numeric(prefit$standata$t_change_trend)

  for (h in c(1L, 5L, 25L)) {
    padded <- rbind(dat, make_future(h))
    # brms reports the padding rows it drops from the observation
    # likelihood, which is the frame doing what it was built to do.
    # Asserted rather than muffled, so the day it stops arriving is
    # the day this block says so.
    expect_warning(
      grown <- mvgam(
        formula = obs_formula,
        trend_formula = ~ PW(n_changepoints = n_change),
        data = padded, family = poisson(), run_model = FALSE,
        silent = 2
      ),
      "Rows containing NAs"
    )
    sd_grown <- grown$standata
    # The grid did grow, so the two frames differ where it counts.
    expect_gt(as.integer(sd_grown$N_time_trend), n_time)
    # And the breaks did not move with it.
    expect_equal(as.numeric(sd_grown$t_change_trend), baseline)
    expect_identical(as.integer(sd_grown$N_change_trend), n_change)
    # Still inside the observed span rather than out among the
    # occasions that carry no response.
    expect_lte(max(as.numeric(sd_grown$t_change_trend)),
               max(time_vals))
  }
})


test_that("the trend runs on the series axis, one rate per series", {
  sd <- prefit$standata
  ax <- mvgam:::mvgam_axes(prefit)
  expect_identical(as.character(ax$series$levels), series_levels)
  expect_identical(as.integer(ax$time$values), time_vals)
  expect_identical(ax$grain, "series")
  expect_identical(as.integer(sd$N_time_trend), n_time)
  expect_identical(as.integer(sd$N_series_trend), n_series)
  expect_identical(dim(sd$times_trend), c(n_time, n_series))
  expect_identical(as.integer(sd$obs_trend_series),
                   match(as.character(dat$series), series_levels))

  # The program gives every series its own base rate, offset and set
  # of adjustments. A `delta` shared across series would fit a
  # single break pattern to both and lose the per-series dynamics.
  sc <- as.character(stancode(prefit))
  expect_true(grepl("vector[N_lv_trend] k_trend", sc, fixed = TRUE))
  expect_true(grepl("vector[N_lv_trend] m_trend", sc, fixed = TRUE))
  expect_true(grepl(
    "matrix[N_change_trend, N_lv_trend] delta_trend", sc,
    fixed = TRUE
  ))
  # Linear growth by default. Both helper functions are always
  # defined in the program, so which one is *called* is the thing
  # that distinguishes the two growth forms; testing for the
  # definition passes on either.
  expect_true(grepl("= linear_trend(", sc, fixed = TRUE))
  expect_false(grepl("= logistic_trend(", sc, fixed = TRUE))
})


test_that("the offset reaches Stan as data, in the frame's row order", {
  # An offset is a covariate with its coefficient fixed at one, so it
  # is passed as data rather than estimated. Row order is the whole
  # of its correctness: a permuted offset gives every observation
  # another's exposure, changes every fitted value, and leaves the
  # dimensions untouched.
  sd <- prefit$standata
  expect_true("offsets" %in% names(sd))
  expect_length(as.numeric(sd$offsets), nrow(dat))
  expect_equal(as.numeric(sd$offsets), as.numeric(dat$log_effort))
  # It varies by row, so the check above is not satisfied by any
  # constant vector.
  expect_gt(stats::sd(as.numeric(sd$offsets)), 0)
  # And no coefficient is estimated for it: `-1` leaves the design
  # with no population-level columns at all.
  expect_identical(as.integer(sd$K), 0L)
})


test_that("the two-dimensional smooth is one basis over two margins", {
  # `s(x1, x2)` is a single isotropic basis over the pair, not two
  # additive bases. Emitted as two, the model still fits and still
  # has smooth terms, but it can no longer represent an interaction
  # between the margins, which is what a 2-D smooth exists for.
  sd <- prefit$standata
  zs <- grep("^Zs_[0-9]+_[0-9]+$", names(sd), value = TRUE)
  expect_length(zs, 1L)
  expect_identical(nrow(sd$Zs_1_1), nrow(dat))
  # One penalised block and one unpenalised column set.
  expect_identical(nrow(sd$Xs), nrow(dat))

  # The smooth is named for both covariates, so a reader can tell it
  # apart from a pair of one-dimensional terms.
  sm <- smooths(prefit)
  expect_length(sm, 1L)
  expect_match(sm[1L], "x1", fixed = TRUE)
  expect_match(sm[1L], "x2", fixed = TRUE)
})


# -- Fit --------------------------------------------------------------

cache <- cache_path("val_mvgam_pw_trend.rds")
if (file.exists(cache)) {
  cat("[cache] Loading PW fit.\n")
  fit <- readRDS(cache)
} else {
  cat("[fit ] mvgam(PW(n_changepoints = 8), offset + s(x1, x2))\n")
  fit <- mvgam(
    formula = obs_formula,
    trend_formula = ~ PW(n_changepoints = n_change),
    data = dat, family = poisson(),
    chains = 2L, iter = 1000L, warmup = 500L,
    silent = 2, backend = "cmdstanr"
  )
}
if (!identical(attr(fit, "sim_truth"), sim_truth)) {
  attr(fit, "sim_truth") <- sim_truth
  saveRDS(fit, cache)
}

dm <- posterior::as_draws_matrix(fit$fit)


test_that("the rate adjustments are one per changepoint per series", {
  # `delta_trend[c, s]` is the change in slope at changepoint `c` for
  # series `s`. Read at the wrong grain this is a single shared
  # vector, which fits both series with one break pattern.
  cols <- grep("^delta_trend\\[", colnames(dm), value = TRUE)
  expect_length(cols, n_change * n_series)
  for (c_i in seq_len(n_change)) {
    for (s_i in seq_len(n_series)) {
      expect_true(paste0("delta_trend[", c_i, ",", s_i, "]") %in% cols)
    }
  }
  # A base rate and an offset per series, not per row or per
  # changepoint.
  expect_length(grep("^k_trend\\[", colnames(dm), value = TRUE),
                n_series)
  expect_length(grep("^m_trend\\[", colnames(dm), value = TRUE),
                n_series)
  # The adjustments are shrunk toward zero by a double-exponential
  # prior, so most are small and a few are not: a vector that is
  # entirely zero means no break was found anywhere.
  d_hat <- vapply(cols, function(k) mean(dm[, k]), numeric(1))
  expect_true(all(is.finite(d_hat)))
  expect_gt(max(abs(d_hat)), 0)
})


test_that("the latent trend tracks the simulated piecewise path", {
  # The trend is identified only up to a level here, since the
  # observation formula has no intercept and `m_trend` carries it, so
  # the comparison is on correlation with the simulated path rather
  # than on the values.
  trend_hat <- matrix(NA_real_, n_time, n_series)
  for (t in seq_len(n_time)) {
    for (s in seq_len(n_series)) {
      trend_hat[t, s] <- mean(dm[, paste0("trend[", t, ",", s, "]")])
    }
  }
  for (s in seq_len(n_series)) {
    expect_gt(stats::cor(trend_hat[, s], latent[, s]), 0.7)
  }
  # The two series broke in the same places but from different base
  # rates, so their trends are not the same trajectory.
  expect_false(isTRUE(all.equal(trend_hat[, 1L], trend_hat[, 2L])))
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
  expect_identical(as.integer(os$time), match(dat$time, time_vals))
})


test_that("the offset moves the prediction, one for one on the log scale", {
  # The defining property of an offset: its coefficient is one. So
  # doubling the exposure multiplies the expected count by two, and
  # a fitted coefficient, or an ignored offset, breaks that exactly.
  base <- posterior_epred(fit, newdata = dat, draw_ids = 1:20,
                          incl_autocor = TRUE)
  doubled <- dat
  doubled$log_effort <- dat$log_effort + log(2)
  got <- posterior_epred(fit, newdata = doubled, draw_ids = 1:20,
                         incl_autocor = TRUE)
  expect_equal(unname(got), unname(base * 2))
})


test_that("each row reads the latent cell the sampler drew for it", {
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


test_that("fitted summarises the same draws epred returns", {
  ep <- posterior_epred(fit, ndraws = NULL, process_error = FALSE)
  ft <- fitted(fit, process_error = FALSE)
  expect_identical(nrow(ft), nrow(dat))
  expect_identical(colnames(ft),
                   c("Estimate", "Est.Error", "Q2.5", "Q97.5"))
  expect_equal(as.numeric(ft[, "Estimate"]), unname(colMeans(ep)))
  expect_true(all(ft[, "Q2.5"] <= ft[, "Estimate"]))
  expect_true(all(ft[, "Estimate"] <= ft[, "Q97.5"]))
})


test_that("the forecast object is keyed, ordered and in user units", {
  h <- 5L
  future_times <- max(time_vals) + seq_len(h)
  fc <- forecast(fit, newdata = make_future(h), ndraws = 20L,
                 type = "link")
  expect_identical(names(fc$forecasts), series_levels)
  expect_identical(as.character(fc$series_names), series_levels)
  for (s in series_levels) {
    expect_identical(as.integer(fc$test_times[[s]]), future_times)
    expect_identical(as.integer(fc$train_times[[s]]), time_vals)
    expect_identical(ncol(fc$forecasts[[s]]),
                     length(fc$test_times[[s]]))
    rows <- which(as.character(dat$series) == s)
    rows <- rows[order(dat$time[rows])]
    expect_equal(as.numeric(fc$train_observations[[s]]),
                 as.numeric(dat$y[rows]))
  }
})


test_that("the hindcast reads the same cells as the conditional epred", {
  hc <- hindcast(fit, type = "expected")
  blocks <- hc$hindcasts
  expect_identical(names(blocks), series_levels)
  cells <- unlist(lapply(names(blocks), function(s) {
    rows <- which(as.character(dat$series) == s)
    rows[order(dat$time[rows])]
  }))
  ep <- posterior_epred(fit, incl_autocor = TRUE)
  expect_equal(unname(ep[, cells, drop = FALSE]),
               unname(do.call(cbind, blocks)))
})


# -- The newdata battery ----------------------------------------------

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
  expect_length(by_series, n_series)
  expect_gt(stats::sd(as.numeric(by_series)), 0)

  # And within a series over time, so a cut by occasion is one too.
  for (s in series_levels) {
    rows <- which(as.character(dat$series) == s)
    expect_gt(stats::sd(colm[rows]), 0)
  }

  # No two rows of the reference are the same everywhere, which is
  # what a collapsed axis would produce while keeping every shape.
  expect_gt(length(unique(round(colm, 8))), 1L)
})



test_that("rearranged and cut newdata frames read the same cells", {
  set.seed(53L)
  perm <- sample(nrow(dat))
  expect_equal(
    unname(posterior_epred(fit, newdata = dat[perm, , drop = FALSE],
                           draw_ids = 1:10, incl_autocor = TRUE)),
    unname(ref_epred[, perm, drop = FALSE])
  )

  for (s in series_levels) {
    rows <- which(as.character(dat$series) == s)
    sub <- dat[rows, , drop = FALSE]
    sub$series <- droplevels(sub$series)
    expect_identical(levels(sub$series), s)
    expect_equal(
      unname(posterior_epred(fit, newdata = sub, draw_ids = 1:10,
                             incl_autocor = TRUE)),
      unname(ref_epred[, rows, drop = FALSE])
    )
  }

  for (tv in time_vals[c(1L, 20L, n_time)]) {
    rows <- which(dat$time == tv)
    expect_equal(
      unname(posterior_epred(fit, newdata = dat[rows, , drop = FALSE],
                             draw_ids = 1:10, incl_autocor = TRUE)),
      unname(ref_epred[, rows, drop = FALSE])
    )
  }

  rev_nd <- dat
  rev_nd$series <- factor(as.character(rev_nd$series),
                          levels = rev(series_levels))
  expect_equal(
    unname(posterior_epred(fit, newdata = rev_nd, draw_ids = 1:10,
                           incl_autocor = TRUE)),
    unname(ref_epred)
  )
})


test_that("a newdata missing the offset column is refused by name", {
  nd <- dat
  nd$log_effort <- NULL
  err <- expect_error(
    posterior_epred(fit, newdata = nd, draw_ids = 1:5)
  )
  expect_match(conditionMessage(err), "log_effort", fixed = TRUE)
})


test_that("an unknown series is refused, and named", {
  nd <- dat
  nd$series <- factor(
    ifelse(seq_len(nrow(nd)) == 1L, "charlie",
           as.character(nd$series)),
    levels = c(series_levels, "charlie")
  )
  err <- expect_error(
    posterior_epred(fit, newdata = nd, draw_ids = 1:5),
    "Series levels in newdata not found in training data"
  )
  expect_match(conditionMessage(err), "charlie", fixed = TRUE)
  for (s in series_levels) {
    expect_match(conditionMessage(err), s, fixed = TRUE)
  }
})


test_that("a factor model is refused on every route PW offers", {
  # A piecewise trend cannot run a factor model, because each series
  # carries its own changepoints. The registry records that against
  # the trend type, as `supports_factors` and the reason beside it,
  # and every route a factor can be asked for is answered from that
  # one record. Each constructor used to restate the rule instead,
  # so the rule held only where a constructor stood: asked four
  # ways, one route refused and three built a factor model.
  build <- function(...) {
    mvgam(obs_formula, data = dat, family = poisson(),
          run_model = FALSE, silent = 2, ...)
  }
  by_arg <- expect_error(
    build(trend_formula = ~ PW(n_changepoints = 5, n_lv = 1L)),
    "Factor models are not supported for PW trends"
  )
  by_map <- expect_error(
    build(trend_formula = ~ PW(n_changepoints = 5),
          trend_map = matrix(NA_real_, n_series, 1L)),
    "Factor models are not supported for PW trends"
  )
  by_jsdgam <- expect_error(
    jsdgam(y ~ 1, factor_formula = ~ -1 + PW(n_changepoints = 5),
           data = dat, unit = time, species = series,
           family = poisson(), n_lv = 1L, run_model = FALSE,
           silent = 2),
    "Factor models are not supported for PW trends"
  )
  # One rule, so one message, whichever route asked.
  expect_identical(conditionMessage(by_map), conditionMessage(by_arg))
  expect_identical(conditionMessage(by_jsdgam), conditionMessage(by_arg))
  # The fourth route never reached a trend at all. `n_lv` on
  # `mvgam()` lands among the arguments forwarded to brms and Stan,
  # where no factor count is read, so the request was dropped and
  # the fit came back with one latent state per series. It is
  # refused on where it was written rather than on the trend, since
  # it is misplaced whatever the trend turns out to be.
  err <- expect_error(
    build(trend_formula = ~ PW(n_changepoints = 5), n_lv = 1L),
    "not read by 'mvgam\\(\\)'"
  )
  expect_match(conditionMessage(err), "n_lv = 1", fixed = TRUE)
})


test_that("a factor-capable trend still takes every factor route", {
  # The control for the block above. A refusal read off the registry
  # has to leave the trends that do decompose alone, or it would
  # trade one silent wrong answer for a loud wrong refusal.
  build <- function(...) {
    mvgam(obs_formula, data = dat, family = poisson(),
          run_model = FALSE, silent = 2, ...)
  }
  on_constructor <- build(trend_formula = ~ AR(p = 1, n_lv = 1L))
  expect_identical(
    as.integer(on_constructor$standata$N_lv_trend), 1L
  )
  by_trend_map <- build(
    trend_formula = ~ AR(p = 1),
    trend_map = matrix(NA_real_, n_series, 1L)
  )
  expect_identical(as.integer(by_trend_map$standata$N_lv_trend), 1L)
  # And a piecewise trend with no factor asked for is untouched: it
  # keeps one state per series, which is what it has always had.
  plain_pw <- build(trend_formula = ~ PW(n_changepoints = 5))
  expect_identical(as.integer(plain_pw$standata$N_lv_trend),
                   as.integer(n_series))
})


test_that("the changepoint arguments reach Stan as asked", {
  # Three arguments that decide where a piecewise trend can bend and
  # how far. Each is the kind that can be read and dropped, leaving a
  # model that fits and answers with someone else's changepoints.
  build <- function(...) {
    mvgam(obs_formula, data = dat, family = poisson(),
          run_model = FALSE, silent = 2,
          trend_formula = ~ PW(...))
  }
  for (k in c(3L, 8L, 15L)) {
    sd_k <- build(n_changepoints = k)$standata
    expect_length(sd_k$t_change_trend, k)
  }
  # `changepoint_range` confines them to the first share of the grid,
  # so a larger range puts the last changepoint later.
  last_at <- vapply(c(0.4, 0.8, 1.0), function(r) {
    max(build(n_changepoints = 6L, changepoint_range = r)$standata$t_change_trend)
  }, numeric(1))
  expect_true(all(diff(last_at) > 0))
  # `changepoint_scale` is the prior width on the rate adjustments and
  # reaches the program as the double exponential's scale.
  for (sc in c(0.05, 5)) {
    code <- as.character(stancode(build(n_changepoints = 6L,
                                        changepoint_scale = sc)))
    expect_match(
      code,
      paste0("double_exponential_lpdf\\(to_vector\\(delta_trend\\) \\| 0, ",
             sc, "\\)")
    )
  }
})


test_that("logistic growth is refused without a cap, and says so", {
  # The other growth form this trend offers, and it needs a carrying
  # capacity. Whether the frame carries one is a question about the
  # data, so it is answered where the data is in hand rather than in
  # the constructor, which is evaluated while the trend formula is
  # parsed and cannot see it. The refusal names the column it looked
  # for and the growth form that would not need it.
  err <- expect_error(
    mvgam(
      formula = obs_formula,
      trend_formula = ~ PW(growth = "logistic"),
      data = dat, family = poisson(), run_model = FALSE, silent = 2
    ),
    "requires a 'cap' column"
  )
  msg <- conditionMessage(err)
  expect_match(msg, "cap", fixed = TRUE)
  expect_match(msg, "growth = 'linear'", fixed = TRUE)
})


test_that("the cap the refusal names as sufficient is sufficient", {
  # The message above offers two ways out: pass `cap`, or "ensure
  # 'cap' column exists in data". The second is asserted here because
  # a user reading that sentence will do exactly this, and a remedy a
  # message names has to work.
  capped <- dat
  capped$cap <- max(dat$y) * 3L
  expect_true("cap" %in% names(capped))
  built <- mvgam(
    formula = obs_formula,
    trend_formula = ~ PW(growth = "logistic"),
    data = capped, family = poisson(), run_model = FALSE, silent = 2
  )
  expect_true(grepl("= logistic_trend(",
                    as.character(stancode(built)), fixed = TRUE))
})


test_that("naming the cap column explicitly builds the logistic form", {
  # The route that does work today, kept alongside so the two are
  # compared rather than one standing in for the other. The column
  # need not be called `cap`, which is what the argument is for.
  capped <- dat
  capped$carrying <- max(dat$y) * 3L
  built <- mvgam(
    formula = obs_formula,
    trend_formula = ~ PW(growth = "logistic", cap = carrying),
    data = capped, family = poisson(), run_model = FALSE, silent = 2
  )
  sc <- as.character(stancode(built))
  expect_true(grepl("= logistic_trend(", sc, fixed = TRUE))
  expect_false(grepl("= linear_trend(", sc, fixed = TRUE))
})


test_that("the smooth is reachable and drawn over both margins", {
  sm <- smooths(fit)
  expect_length(sm, 1L)
  ps <- posterior_smooths(fit, smooth = sm[1L], ndraws = 20L)
  expect_identical(nrow(ps), 20L)
  expect_true(all(is.finite(ps)))

  cs <- conditional_smooths(fit)
  expect_length(cs, 1L)
  d <- cs[[1L]]
  expect_s3_class(d, "data.frame")
  expect_gt(nrow(d), 0L)
  # A two-dimensional smooth is drawn as a surface, so both margins
  # appear as effects rather than one.
  expect_true(all(c("effect1__", "effect2__") %in% names(d)))
  expect_true(all(is.finite(d$estimate__)))
  expect_gt(length(unique(d$effect1__)), 1L)
  expect_gt(length(unique(d$effect2__)), 1L)
})


test_that("summary, tidiers and criticism run on a PW fit", {
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
  expect_equal(as.numeric(aug$time), as.numeric(dat$time))
  expect_equal(as.numeric(aug$.observed), as.numeric(dat$y))
  expect_identical(as.character(aug$series), as.character(dat$series))
  expect_true(any(grepl("^delta_trend\\[", variables(fit))))
})


test_that("every per-series plot panels in the model's own order", {
  # Series declared out of alphabetical order, so a panel order taken
  # from a sort differs from the model's. `plot(type = "series")` and
  # the hindcast arms use the model's order; `plot(type = "trend")`
  # sorts, so the first panel of one is a different series from the
  # first panel of the other while every label is right on its own.
  panel_order <- function(ty) {
    b <- ggplot2::ggplot_build(plot(fit, type = ty))
    lay <- b$layout$layout
    fc <- setdiff(names(lay),
                  c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y"))
    if (!length(fc)) return(character(0))
    as.character(lay[[fc[1L]]])
  }
  expect_identical(panel_order("series"), series_levels)
  expect_identical(panel_order("trend"), series_levels)
  expect_identical(names(hindcast(fit)$hindcasts), series_levels)
})


test_that("print describes the model without printing a pointer", {
  # The formula environments are emitted on every fit, two lines of
  # address that change between sessions and say nothing about the
  # model.
  out <- capture.output(print(fit))
  expect_identical(grep("<environment: 0x", out, value = TRUE),
                   character(0))
})


test_that("the tidy table carries the smooth this model is built on", {
  # The two-dimensional smooth's basis coefficients are reported by
  # `variables()` and by `posterior_summary()` and are absent from
  # `tidy()`, so the table describes a model with no `s(x1, x2)` in
  # it while carrying the changepoint block in full.
  vars <- variables(fit)
  ps <- rownames(posterior_summary(fit))
  td <- tidy(fit, effects = "all")
  expect_gt(sum(grepl("^bs_", vars)), 0L)
  expect_identical(sum(grepl("^bs_", ps)), sum(grepl("^bs_", vars)))
  expect_identical(sum(grepl("^bs_", td$term)), sum(grepl("^bs_", vars)))
  # The changepoint adjustments are carried, which is what makes the
  # omission specific rather than the tidier failing on this fit.
  expect_identical(sum(grepl("^delta_trend", td$term)),
                   sum(grepl("^delta_trend", vars)))
})


test_that("pp_check and the plotting methods draw something for PW", {
  # A ggplot is returned whether or not a layer received data, so the
  # class alone passes on an empty panel.
  drawn <- function(p) {
    expect_s3_class(p, "ggplot")
    layers <- ggplot2::ggplot_build(p)$data
    expect_gt(sum(vapply(layers, nrow, integer(1L))), 0L)
  }
  drawn(pp_check(fit, ndraws = 20L))
  for (ty in c("residuals", "trend", "series")) {
    drawn(plot(fit, type = ty))
  }
  drawn(mcmc_plot(fit))
})

cat("\nDone.\n")
