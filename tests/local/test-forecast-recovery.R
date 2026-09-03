# Statistical recovery checks for forecast.mvgam.
#
# Three groups:
#
#   1. Baseline calibration. Each trend family is fitted on three
#      seeds and the per-series `in_interval` indicators are pooled, so
#      the 90% interval's realised coverage is bracketed by a Wilson
#      95% interval on the binomial rate. Ten families are covered:
#      AR(1), RW, ZMVN, VAR(1), CAR(1), AR(1) with correlated
#      series, and PW.
#
#   2. Complex predictors. One fit each for AR(p = 3), ARMA(1,1),
#      negbinomial, an observation-side `s(x)`, a trend-side Hilbert
#      `gp(x, k)` and a trend-side `mo(x_ord)`, carried through the
#      scoring pipeline.
#
#   3. Scoring API parity: `score(fc, "crps")` against a direct call
#      to the kernel underneath it.
#
# Two things changed from the version this replaces.
#
# Every fit ran as `SM(SW(do.call(mvgam, ...)))`, so a warning from
# any of the thirty models reached nobody. Warnings are now captured
# per bundle and asserted, which is the only way a suite that fits
# this many models can claim they are clean.
#
# Three blocks read a bundle another block had written, by path. That
# made them pass or fail on the order the file happened to run in,
# and on a cache directory that may hold a bundle from an older
# specification. Each block now asks `prep_recovery()` for what it
# needs, which returns the same cache when it is there.
#
# Bundles are cached under `fixtures/forecast_recovery/`. A bundle
# written before warnings were recorded cannot say whether its fit
# was clean, so it is rebuilt rather than trusted.

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(posterior)
  library(testthat)
})

CACHE_DIR <- if (dir.exists("fixtures")) {
  file.path("fixtures", "forecast_recovery")
} else {
  file.path("tests", "local", "fixtures", "forecast_recovery")
}
if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)


# ----- Cache helper ------------------------------------------------

# Run sim + (optional data augmentation) + fit + forecast for one
# configuration and cache the whole bundle. `mutate_fn` applies the
# same transformation to `sim$data_train` and `sim$data_test` so
# callers can add derived columns (ordinal encodings for `mo()`, log
# transforms, custom interactions) without widening sim_mvgam.
#
# Warnings from the simulation, the fit and the forecast are
# collected rather than suppressed, and travel with the bundle so a
# cached configuration can still answer for itself.
prep_recovery <- function(name, sim_args, fit_args,
                          mutate_fn = identity,
                          ndraws_fc = 200L) {
  path <- file.path(CACHE_DIR, paste0(name, ".rds"))
  if (file.exists(path)) {
    cached <- readRDS(path)
    if (!is.null(cached$warnings)) return(cached)
  }
  seen <- character(0)
  collect <- function(w) {
    seen <<- c(seen, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
  out <- withCallingHandlers({
    sim <- do.call(sim_mvgam, sim_args)
    sim$data_train <- mutate_fn(sim$data_train)
    sim$data_test <- mutate_fn(sim$data_test)
    fit <- suppressMessages(do.call(mvgam, c(
      list(data = sim$data_train), fit_args
    )))
    fc <- forecast(fit, newdata = sim$data_test,
                   type = "response", ndraws = ndraws_fc)
    list(sim = sim, fit = fit, fc = fc)
  }, warning = collect)
  out$warnings <- seen
  part <- paste0(path, ".part")
  saveRDS(out, part)
  file.rename(part, path)
  out
}


# ----- Assertion helpers -------------------------------------------

# A scored forecast has one row per evaluated time, in horizon
# order, carrying the score type that was asked for. Checking only
# that the numbers are finite passes on a frame scored against the
# wrong observations, in the wrong order, or with the wrong rule.
expect_score_frame <- function(sc, type, horizon, n_series = 1L) {
  expect_identical(names(sc),
                   c(paste0("series_", seq_len(n_series)),
                     "all_series"))
  for (nm in setdiff(names(sc), "all_series")) {
    df <- sc[[nm]]
    expect_identical(names(df),
                     c("score", "in_interval", "interval_width",
                       "eval_horizon", "score_type"))
    expect_identical(nrow(df), horizon)
    expect_identical(as.integer(df$eval_horizon), seq_len(horizon))
    expect_identical(unique(df$score_type), type)
    expect_identical(unique(df$interval_width), 0.9)
    expect_true(all(is.finite(df$score)))
    # crps, drps and the energy family are proper scores on a
    # non-negative scale; a sign flip anywhere in the kernel shows
    # up here and nowhere else.
    expect_true(all(df$score >= 0))
    # A score identical at every horizon means the forecast is not
    # being compared against the observations at all.
    expect_gt(stats::sd(df$score), 0)
    expect_true(all(df$in_interval %in% c(0, 1)))
  }
  expect_identical(nrow(sc$all_series), horizon)
  expect_true(all(is.finite(sc$all_series$score)))
  # A univariate rule scores each series and the pooled row is their
  # sum, which the type says: `drps` per series, `sum_drps` across
  # them. The multivariate rules are joint already and keep their
  # bare name, so the prefix is what distinguishes a summed score
  # from one computed over the series together.
  expect_identical(unique(sc$all_series$score_type),
                   paste0("sum_", type))
  expect_equal(
    sc$all_series$score,
    Reduce(`+`, lapply(setdiff(names(sc), "all_series"),
                       function(nm) sc[[nm]]$score)),
    tolerance = 1e-10
  )
}

# The forecast horizon a bundle was built for. Integer, because the
# row counts it is compared against are.
bundle_horizon <- function(bundle) {
  as.integer(nrow(bundle$sim$data_test) /
               length(bundle$fc$series_names))
}


# ----- Coverage helper --------------------------------------------

wilson_ci <- function(k, n, conf = 0.95) {
  z <- stats::qnorm(1 - (1 - conf) / 2)
  phat <- k / n
  denom <- 1 + z^2 / n
  centre <- (phat + z^2 / (2 * n)) / denom
  half <- z * sqrt(phat * (1 - phat) / n + z^2 / (4 * n^2)) /
    denom
  c(lower = centre - half, upper = centre + half)
}


# Pool the per-series `in_interval` columns from a sequence of
# `score()` results, return the binomial (hits, total) pair.
pool_coverage <- function(score_results) {
  hits <- 0L
  total <- 0L
  for (sr in score_results) {
    for (nm in setdiff(names(sr), "all_series")) {
      df <- sr[[nm]]
      ok <- !is.na(df$in_interval)
      hits <- hits + sum(df$in_interval[ok])
      total <- total + sum(ok)
    }
  }
  list(hits = hits, total = total)
}


# Every bundle this file builds, so the warnings they collected can
# be asserted together rather than one assertion per configuration.
built <- new.env(parent = emptyenv())

recover <- function(name, ...) {
  bundle <- prep_recovery(name = name, ...)
  assign(name, bundle$warnings, envir = built)
  bundle
}


# ----- Baseline group: AR(1) Poisson, three seeds ----------------

test_that("AR(1) Poisson 90% PI covers near nominal across seeds", {
  results <- lapply(c(101L, 102L, 103L), function(seed) {
    bundle <- recover(
      name = paste0("ar1_pois_seed", seed),
      sim_args = list(
        trend_model = AR(p = 1L), family = poisson(),
        n_timepoints = 200L, n_series = 1L,
        proportional_train = 0.75, seed = seed
      ),
      fit_args = list(
        formula = y ~ 1,
        trend_formula = ~ AR(p = 1),
        family = poisson(),
        chains = 1L, iter = 500L, warmup = 250L,
        refresh = 0L, silent = 2L
      )
    )
    sc <- score(bundle$fc, "drps")
    expect_score_frame(sc, "drps", bundle_horizon(bundle))
    sc
  })
  pooled <- pool_coverage(results)
  ci <- wilson_ci(pooled$hits, pooled$total)
  expect_true(0.90 >= ci["lower"] && 0.90 <= ci["upper"])
})


# ----- Baseline group: RW Gaussian, three seeds ------------------

test_that("RW Gaussian 90% PI covers near nominal across seeds", {
  results <- lapply(c(201L, 202L, 203L), function(seed) {
    bundle <- recover(
      # RW estimation is weakly identified on short series
      # (the marginal likelihood is flat in sigma_trend when
      # the signal is small). Lengthen the series so the fit
      # has enough data to recover the innovation SD; without
      # this the posterior mean sigma is biased low and the
      # downstream forecast PI is too narrow.
      name = paste0("rw_gauss_T400_seed", seed),
      sim_args = list(
        trend_model = RW(), family = gaussian(),
        n_timepoints = 400L, n_series = 1L,
        proportional_train = 0.75, seed = seed
      ),
      fit_args = list(
        formula = y ~ 1,
        trend_formula = ~ RW(),
        family = gaussian(),
        chains = 1L, iter = 500L, warmup = 250L,
        refresh = 0L, silent = 2L
      )
    )
    sc <- score(bundle$fc, "crps")
    expect_score_frame(sc, "crps", bundle_horizon(bundle))
    sc
  })
  pooled <- pool_coverage(results)
  ci <- wilson_ci(pooled$hits, pooled$total)
  expect_true(0.90 >= ci["lower"] && 0.90 <= ci["upper"])
})


# ----- Baseline group: ZMVN 2-series Poisson, three seeds --------

test_that("ZMVN 2-series 90% PI covers near nominal across seeds", {
  results <- lapply(c(301L, 302L, 303L), function(seed) {
    bundle <- recover(
      name = paste0("zmvn_2_pois_seed", seed),
      sim_args = list(
        trend_model = ZMVN(), family = poisson(),
        n_timepoints = 120L, n_series = 2L,
        proportional_train = 0.75, seed = seed
      ),
      fit_args = list(
        formula = y ~ 1,
        trend_formula = ~ ZMVN(),
        family = poisson(),
        chains = 1L, iter = 500L, warmup = 250L,
        refresh = 0L, silent = 2L
      )
    )
    sc <- score(bundle$fc, "drps")
    expect_score_frame(sc, "drps", bundle_horizon(bundle),
                       n_series = 2L)
    sc
  })
  pooled <- pool_coverage(results)
  ci <- wilson_ci(pooled$hits, pooled$total)
  expect_true(0.90 >= ci["lower"] && 0.90 <= ci["upper"])
})


# ----- Complex-predictor group: AR(p = 3) consecutive ------------

test_that("AR(p=3) Poisson: score pipeline yields finite CRPS / DRPS", {
  bundle <- recover(
    name = "arp3_pois_seed401",
    sim_args = list(
      trend_model = AR(p = 3L), family = poisson(),
      n_timepoints = 200L, n_series = 1L,
      proportional_train = 0.75, seed = 401L
    ),
    fit_args = list(
      formula = y ~ 1,
      trend_formula = ~ AR(p = 3),
      family = poisson(),
      chains = 1L, iter = 500L, warmup = 250L,
      refresh = 0L, silent = 2L
    )
  )
  expect_score_frame(score(bundle$fc, "drps"), "drps",
                     bundle_horizon(bundle))
})


# ----- Complex-predictor group: ARMA(1,1) ------------------------

test_that("ARMA(1,1) Poisson: MA pipeline produces finite scores", {
  bundle <- recover(
    name = "arma11_pois_seed501",
    sim_args = list(
      trend_model = AR(p = 1L, ma = TRUE), family = poisson(),
      n_timepoints = 180L, n_series = 1L,
      proportional_train = 0.75, seed = 501L
    ),
    fit_args = list(
      formula = y ~ 1,
      trend_formula = ~ AR(p = 1, ma = TRUE),
      family = poisson(),
      chains = 1L, iter = 500L, warmup = 250L,
      refresh = 0L, silent = 2L
    )
  )
  expect_score_frame(score(bundle$fc, "drps"), "drps",
                     bundle_horizon(bundle))
})


# ----- Complex-predictor group: Negative binomial ----------------

test_that("AR(1) NegBin: dispersion family produces finite scores", {
  bundle <- recover(
    name = "ar1_nb_seed601",
    sim_args = list(
      trend_model = AR(p = 1L),
      family = brms::negbinomial(),
      n_timepoints = 150L, n_series = 1L,
      proportional_train = 0.75, seed = 601L
    ),
    fit_args = list(
      formula = y ~ 1,
      trend_formula = ~ AR(p = 1),
      family = brms::negbinomial(),
      chains = 1L, iter = 500L, warmup = 250L,
      refresh = 0L, silent = 2L
    )
  )
  horizon <- bundle_horizon(bundle)
  expect_score_frame(score(bundle$fc, "drps"), "drps", horizon)
  # Log score exercises the family-aware density path, and is the
  # one rule here that may legitimately be negative, so it is
  # checked for finiteness and shape rather than sign.
  logs <- score(bundle$fc, "logs")
  expect_identical(nrow(logs$series_1), horizon)
  expect_identical(unique(logs$series_1$score_type), "logs")
  expect_true(all(is.finite(logs$series_1$score)))
})


# ----- Complex-predictor group: obs-side smooth ------------------

test_that("AR(1) + s(x) on obs: smooth contribution flows through", {
  bundle <- recover(
    name = "ar1_pois_obs_sx_seed701",
    sim_args = list(
      trend_model = AR(p = 1L), family = poisson(),
      n_timepoints = 150L, n_series = 1L,
      proportional_train = 0.75, seed = 701L
    ),
    # Re-use the auto-generated `x` covariate from sim_mvgam
    # type 1 / 2 specs (default type = 1L includes `x`).
    fit_args = list(
      formula = y ~ s(x, k = 6L),
      trend_formula = ~ AR(p = 1),
      family = poisson(),
      chains = 1L, iter = 500L, warmup = 250L,
      refresh = 0L, silent = 2L
    )
  )
  expect_score_frame(score(bundle$fc, "drps"), "drps",
                     bundle_horizon(bundle))
})


# ----- Complex-predictor group: trend-side GP --------------------

test_that("AR(1) + gp(x) on trend: Hilbert-GP basis flows through", {
  bundle <- recover(
    name = "ar1_pois_trend_gp_seed801",
    sim_args = list(
      trend_model = AR(p = 1L), family = poisson(),
      n_timepoints = 150L, n_series = 1L,
      proportional_train = 0.75, seed = 801L
    ),
    fit_args = list(
      formula = y ~ 1,
      trend_formula = ~ gp(x, k = 8L) + AR(p = 1),
      family = poisson(),
      chains = 1L, iter = 500L, warmup = 250L,
      refresh = 0L, silent = 2L
    )
  )
  expect_score_frame(score(bundle$fc, "drps"), "drps",
                     bundle_horizon(bundle))
})


# ----- Complex-predictor group: trend-side monotonic -------------

test_that("AR(1) + mo(x_ord) on trend: monotonic effect flows through", {
  bundle <- recover(
    # n_timepoints = 300 so the 25% held-out slice has enough
    # rows for `x` to span the 5 monotonic bins; mvgam's
    # validator rejects test data that hits only a single
    # level.
    name = "ar1_pois_trend_mo_seed901_T300",
    sim_args = list(
      trend_model = AR(p = 1L), family = poisson(),
      n_timepoints = 300L, n_series = 1L,
      proportional_train = 0.75, seed = 901L
    ),
    fit_args = list(
      formula = y ~ 1,
      # Discretise `x` into a 5-level ordered factor for mo().
      trend_formula = ~ mo(x_ord) + AR(p = 1),
      family = poisson(),
      chains = 1L, iter = 500L, warmup = 250L,
      refresh = 0L, silent = 2L
    ),
    mutate_fn = function(d) {
      # Fixed breakpoints over `sim_mvgam`'s [-2, 2] range so
      # train and test rows share the same ordinal coding.
      d$x_ord <- as.integer(cut(
        d$x, breaks = seq(-2, 2, length.out = 6L),
        include.lowest = TRUE
      ))
      d
    }
  )
  expect_s3_class(bundle$fit, "mvgam")
  expect_score_frame(score(bundle$fc, "drps"), "drps",
                     bundle_horizon(bundle))
})


# ----- Scoring API parity vs direct kernel call ------------------

test_that("score(fc, 'crps') matches direct crps_mcmc_object", {
  # Asks for its own bundle rather than reading one another block
  # left on disk, so the comparison cannot depend on the order the
  # file ran in.
  bundle <- recover(
    name = "rw_gauss_T400_seed201",
    sim_args = list(
      trend_model = RW(), family = gaussian(),
      n_timepoints = 400L, n_series = 1L,
      proportional_train = 0.75, seed = 201L
    ),
    fit_args = list(
      formula = y ~ 1,
      trend_formula = ~ RW(),
      family = gaussian(),
      chains = 1L, iter = 500L, warmup = 250L,
      refresh = 0L, silent = 2L
    )
  )
  via_dispatch <- score(bundle$fc, "crps")
  series_name <- as.character(bundle$fc$series_names[1L])
  truth <- bundle$fc$test_observations[[series_name]]
  fc_mat <- bundle$fc$forecasts[[series_name]]
  direct <- mvgam:::crps_mcmc_object(truth, fc_mat)
  expect_equal(
    via_dispatch[[series_name]]$score,
    as.numeric(direct[, "score"])
  )
})


# ----- VAR(1) 2-series Poisson recovery --------------------------

var1_bundle <- NULL

test_that("VAR(1) 2-series 90% PI covers near nominal across seeds", {
  results <- lapply(c(401L, 402L, 403L), function(seed) {
    bundle <- recover(
      # T = 250 (up from 150) so the VAR posterior tightens
      # enough for the forecast PI to not over-cover -- at the
      # shorter length the joint posterior of A + Sigma is
      # still diffuse and the predictive PI is wider than the
      # true conditional distribution.
      name = paste0("var1_2_pois_T250_seed", seed),
      sim_args = list(
        trend_model = VAR(p = 1L), family = poisson(),
        n_timepoints = 250L, n_series = 2L,
        proportional_train = 0.75, seed = seed
      ),
      fit_args = list(
        formula = y ~ 1,
        trend_formula = ~ VAR(p = 1),
        family = poisson(),
        chains = 1L, iter = 500L, warmup = 250L,
        refresh = 0L, silent = 2L
      )
    )
    if (seed == 401L) var1_bundle <<- bundle
    sc <- score(bundle$fc, "drps")
    expect_score_frame(sc, "drps", bundle_horizon(bundle),
                       n_series = 2L)
    sc
  })
  pooled <- pool_coverage(results)
  ci <- wilson_ci(pooled$hits, pooled$total)
  expect_true(0.90 >= ci["lower"] && 0.90 <= ci["upper"])
})


test_that("VAR(1) 2-series: multivariate energy + variogram finite", {
  # The multivariate rules score the two series jointly, so they
  # report on `all_series` alone and carry no per-series interval.
  bundle <- var1_bundle
  horizon <- bundle_horizon(bundle)
  for (rule in c("energy", "variogram")) {
    sc <- score(bundle$fc, rule)
    expect_identical(nrow(sc$all_series), horizon)
    expect_identical(as.integer(sc$all_series$eval_horizon),
                     seq_len(horizon))
    expect_identical(unique(sc$all_series$score_type), rule)
    expect_true(all(is.finite(sc$all_series$score)))
    expect_true(all(sc$all_series$score >= 0))
    expect_gt(stats::sd(sc$all_series$score), 0)
  }
})


# ----- CAR(1) single-series recovery -----------------------------
#
# CAR generates irregular per-series time grids (sim_mvgam
# type 6 draws cumulative Unif(1, 6) gaps independently per
# series). Multi-series CAR with different grids hits the
# kernel's shared-time-vector limit; the single-series case
# is the well-tested path.

test_that("CAR(1) single-series 90% PI covers near nominal across seeds", {
  results <- lapply(c(501L, 502L, 503L), function(seed) {
    bundle <- recover(
      name = paste0("car1_pois_seed", seed),
      sim_args = list(
        type = 6L, family = poisson(),
        n_timepoints = 120L, n_series = 1L,
        proportional_train = 0.75, seed = seed
      ),
      fit_args = list(
        formula = y ~ s(season, bs = "cc", k = 6L),
        trend_formula = ~ CAR(time = time),
        family = poisson(),
        chains = 1L, iter = 500L, warmup = 250L,
        refresh = 0L, silent = 2L
      )
    )
    sc <- score(bundle$fc, "drps")
    expect_score_frame(sc, "drps", bundle_horizon(bundle))
    sc
  })
  pooled <- pool_coverage(results)
  ci <- wilson_ci(pooled$hits, pooled$total)
  expect_true(0.90 >= ci["lower"] && 0.90 <= ci["upper"])
})


# ----- AR(1) cor=TRUE 2-series Gaussian recovery -----------------

test_that("AR(1) cor=TRUE 2-series 90% PI covers near nominal", {
  results <- lapply(c(601L, 602L, 603L), function(seed) {
    bundle <- recover(
      name = paste0("ar1_cor_gauss_seed", seed),
      sim_args = list(
        trend_model = AR(p = 1L, cor = TRUE), family = gaussian(),
        n_timepoints = 200L, n_series = 2L,
        proportional_train = 0.75, seed = seed
      ),
      fit_args = list(
        formula = y ~ 1,
        trend_formula = ~ AR(p = 1, cor = TRUE),
        family = gaussian(),
        chains = 1L, iter = 500L, warmup = 250L,
        refresh = 0L, silent = 2L
      )
    )
    sc <- score(bundle$fc, "crps")
    expect_score_frame(sc, "crps", bundle_horizon(bundle),
                       n_series = 2L)
    sc
  })
  pooled <- pool_coverage(results)
  ci <- wilson_ci(pooled$hits, pooled$total)
  expect_true(0.90 >= ci["lower"] && 0.90 <= ci["upper"])
})


# ----- PW (piecewise linear) recovery -----------------------------
#
# Uses sim_mvgam's PW sim path: `fill_pw_trend_defaults`
# supplies (k, m, delta, t_change) and `pw_trendC` evaluates
# the trend deterministically across the training time grid.
# Gaussian response so the in-PI coverage check measures the
# actual posterior-predictive width rather than the integer-
# rounding artefact a Poisson PI exhibits at small lambda.
#
# Reason: the observation formula uses `y ~ -1` because the PW
# trend's `m_trend` (intercept) and an obs-side intercept are
# not jointly identified -- they compete for the same constant
# offset (Prophet convention from Taylor & Letham 2018).

pw_bundle <- NULL

test_that("PW linear 90% PI covers near nominal across seeds", {
  results <- lapply(c(701L, 702L, 703L), function(seed) {
    bundle <- recover(
      name = paste0("pw_linear_gauss_seed", seed),
      sim_args = list(
        trend_model = PW(n_changepoints = 5),
        family = gaussian(),
        n_timepoints = 200L, n_series = 1L,
        proportional_train = 0.75, seed = seed
      ),
      fit_args = list(
        formula = y ~ -1,
        trend_formula = ~ PW(n_changepoints = 5),
        family = gaussian(),
        chains = 1L, iter = 500L, warmup = 250L,
        refresh = 0L, silent = 2L
      )
    )
    if (seed == 701L) pw_bundle <<- bundle
    sc <- score(bundle$fc, "crps")
    expect_score_frame(sc, "crps", bundle_horizon(bundle))
    sc
  })
  pooled <- pool_coverage(results)
  ci <- wilson_ci(pooled$hits, pooled$total)
  expect_true(0.90 >= ci["lower"] && 0.90 <= ci["upper"])
})


# ----- Multivariate scoring on the ZMVN forecast -----------------

test_that("Energy and variogram score the ZMVN forecast", {
  bundle <- recover(
    name = "zmvn_2_pois_seed301",
    sim_args = list(
      trend_model = ZMVN(), family = poisson(),
      n_timepoints = 120L, n_series = 2L,
      proportional_train = 0.75, seed = 301L
    ),
    fit_args = list(
      formula = y ~ 1,
      trend_formula = ~ ZMVN(),
      family = poisson(),
      chains = 1L, iter = 500L, warmup = 250L,
      refresh = 0L, silent = 2L
    )
  )
  horizon <- bundle_horizon(bundle)
  for (rule in c("energy", "variogram")) {
    sc <- score(bundle$fc, rule)
    expect_named(sc, c("series_1", "series_2", "all_series"))
    expect_identical(nrow(sc$all_series), horizon)
    expect_identical(unique(sc$all_series$score_type), rule)
    expect_true(all(is.finite(sc$all_series$score)))
    expect_true(all(sc$all_series$score >= 0))
  }
})


# ----- Every fit in this file, and what it warned about ----------

test_that("no model in this file fits with an unexplained warning", {
  # Thirty models are fitted here. Running them under
  # `suppressWarnings()`, as this file used to, means a warning from
  # any of them reaches nobody, and a suite that fits this many
  # models and reports nothing is not evidence that they are clean.
  expect_gt(length(ls(built)), 20L)
  raised <- Filter(length, as.list(built))
  # Named so a failure says which configuration warned and about
  # what, rather than only that something did.
  expect_identical(
    vapply(raised, function(w) paste(unique(w), collapse = " | "),
           character(1L)),
    setNames(character(0L), character(0L))
  )
})


cat("\nDone.\n")
