# Statistical recovery checks for forecast.mvgam.
#
# Three groups:
#
#   1. Baseline calibration tests. Three trend families pooled
#      across three seeds each (AR(1) Poisson, RW Gaussian,
#      ZMVN 2-series Poisson). For each pooled set we assert
#      the 90% PI coverage is consistent with nominal via a
#      Wilson 95% CI bracket on the binomial in_interval rate.
#
#   2. Complex-predictor end-to-end tests. Each fits one model
#      with a non-trivial predictor (AR(p = 3) consecutive,
#      ARMA(1,1), negbinomial dispersion family, obs-side
#      `s(x)`, trend-side Hilbert `gp(x, k)`, trend-side
#      `mo(x_ord)`) and asserts the score pipeline produces a
#      finite, sensible CRPS / DRPS / coverage indicator on the
#      held-out test slice.
#
#   3. Scoring-API parity checks. Confirm that
#      `score(fc, "crps")$s$score` matches a direct call to the
#      underlying kernel on a real fit's posterior.
#
# Every fit is cached under
# `tests/local/fixtures/forecast_recovery/<name>.rds` so reruns
# are fast (load) after the initial sweep (fit).

source("setup_tests_local.R")

CACHE_DIR <- "fixtures/forecast_recovery"


# ----- Cache helper ------------------------------------------------

# Run sim + (optional data augmentation) + fit + forecast for
# one configuration and cache the whole bundle. Subsequent
# calls load from disk. `mutate_fn` applies the same
# transformation to `sim$data_train` and `sim$data_test` so
# users can add derived columns (ordinal encodings for `mo()`,
# log transforms, custom interactions, etc.) without inflating
# sim_mvgam's surface.
prep_recovery <- function(name, sim_args, fit_args,
                            mutate_fn = identity,
                            ndraws_fc = 200L) {
  path <- file.path(CACHE_DIR, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  sim <- do.call(sim_mvgam, sim_args)
  sim$data_train <- mutate_fn(sim$data_train)
  sim$data_test <- mutate_fn(sim$data_test)
  fit <- SM(SW(do.call(mvgam, c(
    list(data = sim$data_train), fit_args
  ))))
  fc <- forecast(fit, newdata = sim$data_test,
                   type = "response", ndraws = ndraws_fc)
  out <- list(sim = sim, fit = fit, fc = fc)
  saveRDS(out, path)
  out
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


# ----- Baseline group: AR(1) Poisson, three seeds ----------------

test_that("AR(1) Poisson 90% PI covers near nominal across seeds", {
  results <- lapply(c(101L, 102L, 103L), function(seed) {
    bundle <- prep_recovery(
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
    score(bundle$fc, "drps")
  })
  pooled <- pool_coverage(results)
  ci <- wilson_ci(pooled$hits, pooled$total)
  expect_true(0.90 >= ci["lower"] && 0.90 <= ci["upper"])
})


# ----- Baseline group: RW Gaussian, three seeds ------------------

test_that("RW Gaussian 90% PI covers near nominal across seeds", {
  results <- lapply(c(201L, 202L, 203L), function(seed) {
    bundle <- prep_recovery(
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
    score(bundle$fc, "crps")
  })
  pooled <- pool_coverage(results)
  ci <- wilson_ci(pooled$hits, pooled$total)
  expect_true(0.90 >= ci["lower"] && 0.90 <= ci["upper"])
})


# ----- Baseline group: ZMVN 2-series Poisson, three seeds --------

test_that("ZMVN 2-series 90% PI covers near nominal across seeds", {
  results <- lapply(c(301L, 302L, 303L), function(seed) {
    bundle <- prep_recovery(
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
    score(bundle$fc, "drps")
  })
  pooled <- pool_coverage(results)
  ci <- wilson_ci(pooled$hits, pooled$total)
  expect_true(0.90 >= ci["lower"] && 0.90 <= ci["upper"])
})


# ----- Complex-predictor group: AR(p = 3) consecutive ------------

test_that("AR(p=3) Poisson: score pipeline yields finite CRPS / DRPS", {
  bundle <- prep_recovery(
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
  sc_drps <- score(bundle$fc, "drps")
  expect_true(all(is.finite(sc_drps$series_1$score)))
  expect_true(all(is.finite(sc_drps$all_series$score)))
  expect_identical(unique(sc_drps$series_1$score_type), "drps")
  expect_true(all(sc_drps$series_1$in_interval %in% c(0, 1)))
})


# ----- Complex-predictor group: ARMA(1,1) ------------------------

test_that("ARMA(1,1) Poisson: MA pipeline produces finite scores", {
  bundle <- prep_recovery(
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
  sc <- score(bundle$fc, "drps")
  expect_true(all(is.finite(sc$series_1$score)))
  expect_true(all(is.finite(sc$all_series$score)))
})


# ----- Complex-predictor group: Negative binomial ----------------

test_that("AR(1) NegBin: dispersion family produces finite scores", {
  bundle <- prep_recovery(
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
  sc <- score(bundle$fc, "drps")
  expect_true(all(is.finite(sc$series_1$score)))
  # Log score exercises the family-aware density path.
  sc_logs <- score(bundle$fc, "logs")
  expect_true(all(is.finite(sc_logs$series_1$score)))
})


# ----- Complex-predictor group: obs-side smooth ------------------

test_that("AR(1) + s(x) on obs: smooth contribution flows through", {
  bundle <- prep_recovery(
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
  sc <- score(bundle$fc, "drps")
  expect_true(all(is.finite(sc$series_1$score)))
})


# ----- Complex-predictor group: trend-side GP --------------------

test_that("AR(1) + gp(x) on trend: Hilbert-GP basis flows through", {
  bundle <- prep_recovery(
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
  sc <- score(bundle$fc, "drps")
  expect_true(all(is.finite(sc$series_1$score)))
})


# ----- Complex-predictor group: trend-side monotonic -------------

test_that("AR(1) + mo(x_ord) on trend: monotonic effect flows through", {
  bundle <- prep_recovery(
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
  sc <- score(bundle$fc, "drps")
  expect_true(all(is.finite(sc$series_1$score)))
})


# ----- Scoring API parity vs direct kernel call ------------------

test_that("score(fc, 'crps') matches direct crps_mcmc_object", {
  bundle <- readRDS(
    file.path(CACHE_DIR, "rw_gauss_T400_seed201.rds")
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

test_that("VAR(1) 2-series 90% PI covers near nominal across seeds", {
  results <- lapply(c(401L, 402L, 403L), function(seed) {
    bundle <- prep_recovery(
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
    score(bundle$fc, "drps")
  })
  pooled <- pool_coverage(results)
  ci <- wilson_ci(pooled$hits, pooled$total)
  expect_true(0.90 >= ci["lower"] && 0.90 <= ci["upper"])
})


test_that("VAR(1) 2-series: multivariate energy + variogram finite", {
  bundle <- readRDS(
    file.path(CACHE_DIR, "var1_2_pois_T250_seed401.rds")
  )
  e <- score(bundle$fc, "energy")
  v <- score(bundle$fc, "variogram")
  expect_true(all(is.finite(e$all_series$score)))
  expect_true(all(is.finite(v$all_series$score)))
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
    bundle <- prep_recovery(
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
    score(bundle$fc, "drps")
  })
  pooled <- pool_coverage(results)
  ci <- wilson_ci(pooled$hits, pooled$total)
  expect_true(0.90 >= ci["lower"] && 0.90 <= ci["upper"])
})


# ----- AR(1) cor=TRUE 2-series Gaussian recovery -----------------

test_that("AR(1) cor=TRUE 2-series 90% PI covers near nominal", {
  results <- lapply(c(601L, 602L, 603L), function(seed) {
    bundle <- prep_recovery(
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
    score(bundle$fc, "crps")
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

test_that("PW linear 90% PI covers near nominal across seeds", {
  results <- lapply(c(701L, 702L, 703L), function(seed) {
    bundle <- prep_recovery(
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
    score(bundle$fc, "crps")
  })
  pooled <- pool_coverage(results)
  ci <- wilson_ci(pooled$hits, pooled$total)
  expect_true(0.90 >= ci["lower"] && 0.90 <= ci["upper"])
})


test_that("PW linear: score pipeline yields finite CRPS", {
  bundle <- readRDS(
    file.path(CACHE_DIR, "pw_linear_gauss_seed701.rds")
  )
  sc <- score(bundle$fc, "crps")
  expect_true(all(is.finite(sc$series_1$score)))
  expect_true(all(is.finite(sc$all_series$score)))
  expect_identical(unique(sc$series_1$score_type), "crps")
})


# ----- Multivariate scoring on cached ZMVN -----------------------

test_that("Energy / variogram score the cached ZMVN forecast", {
  bundle <- readRDS(
    file.path(CACHE_DIR, "zmvn_2_pois_seed301.rds")
  )
  e <- score(bundle$fc, "energy")
  v <- score(bundle$fc, "variogram")
  expect_named(e, c("series_1", "series_2", "all_series"))
  expect_named(v, c("series_1", "series_2", "all_series"))
  expect_true(all(is.finite(e$all_series$score)))
  expect_true(all(is.finite(v$all_series$score)))
  expect_true(all(e$all_series$score_type == "energy"))
  expect_true(all(v$all_series$score_type == "variogram"))
})
