# Statistical recovery checks for forecast.mvgam.
#
# Each test simulates from the trend kernel via sim_mvgam,
# fits, forecasts on the held-out test slice, and asserts the
# observed 90% PI coverage is consistent with the nominal rate.
# Coverage is pooled across multiple seeds so the binomial
# Wilson CI bracket is tight enough to detect ~10 pp coverage
# shortfalls.

source("setup_tests_local.R")


# --- Coverage helper ----------------------------------------------

# Wilson 95% confidence interval for a Binomial(n, p_hat) given
# k successes out of n. The interval is centred on the
# Wilson-adjusted estimate `(p_hat + z^2/(2n)) / (1 + z^2/n)`,
# not on `p_hat`. Used here to bracket the acceptable range of
# observed coverage around the nominal probability.
wilson_ci <- function(k, n, conf = 0.95) {
  z <- stats::qnorm(1 - (1 - conf) / 2)
  phat <- k / n
  denom <- 1 + z^2 / n
  centre <- (phat + z^2 / (2 * n)) / denom
  half <- z * sqrt(phat * (1 - phat) / n + z^2 / (4 * n^2)) /
    denom
  c(lower = centre - half, upper = centre + half)
}


# Assert that the observed in-PI count is consistent with the
# `nominal` true coverage at the `conf` confidence level. The
# Wilson CI is the 95% interval around the count's MLE; we
# require `nominal` to fall inside it.
assert_pi_coverage <- function(observed_in_pi, total, nominal,
                                conf = 0.95) {
  ci <- wilson_ci(observed_in_pi, total, conf = conf)
  testthat::expect_true(
    nominal >= ci["lower"] && nominal <= ci["upper"]
  )
}


# Per-series in-PI count given a forecast matrix `[ndraws, h]`,
# truth vector, and central PI probability.
count_in_pi <- function(forecast_mat, truth, prob = 0.90) {
  alpha <- (1 - prob) / 2
  lo <- apply(forecast_mat, 2L, stats::quantile, probs = alpha)
  hi <- apply(forecast_mat, 2L, stats::quantile,
              probs = 1 - alpha)
  sum(truth >= lo & truth <= hi)
}


# Run one forecast simulation: sim -> fit -> forecast -> count
# in-PI observations across all series.
run_one_recovery <- function(seed, sim_args, fit_args) {
  set.seed(seed)
  sim <- do.call(sim_mvgam, c(sim_args, list(seed = seed)))
  fit <- SM(SW(do.call(mvgam, c(
    list(data = sim$data_train), fit_args
  ))))
  fc <- forecast(fit, newdata = sim$data_test,
                   type = "response", ndraws = 200L)
  in_pi <- 0L
  truth_total <- 0L
  for (lv in levels(sim$data_test$series)) {
    truth_s <- as.numeric(sim$data_test$y[
      sim$data_test$series == lv
    ])
    fmat_s <- fc$forecasts[[lv]]
    in_pi <- in_pi + count_in_pi(fmat_s, truth_s, prob = 0.90)
    truth_total <- truth_total + length(truth_s)
  }
  list(in_pi = in_pi, total = truth_total)
}


# Pool counts across `seeds` runs of the same simulation /
# fit / forecast pipeline. Aggregation tightens the Wilson CI
# so 10 pp coverage shortfalls can be detected.
pooled_recovery <- function(seeds, sim_args, fit_args) {
  in_pi <- 0L; total <- 0L
  for (s in seeds) {
    r <- run_one_recovery(s, sim_args, fit_args)
    in_pi <- in_pi + r$in_pi
    total <- total + r$total
  }
  list(in_pi = in_pi, total = total)
}


# --- AR(1) Poisson recovery (3 seeds, ~150 pooled obs) ------------

test_that("AR(1) Poisson 90% PI covers near nominal across seeds", {
  res <- pooled_recovery(
    seeds = c(101L, 102L, 103L),
    sim_args = list(
      trend_model = AR(p = 1L),
      family = poisson(),
      n_timepoints = 200L,
      n_series = 1L,
      proportional_train = 0.75
    ),
    fit_args = list(
      formula = y ~ 1,
      trend_formula = ~ AR(p = 1),
      family = poisson(),
      chains = 1L, iter = 500L, warmup = 250L,
      refresh = 0L, silent = 2L
    )
  )
  assert_pi_coverage(res$in_pi, res$total, nominal = 0.90)
})


# --- RW Gaussian recovery (3 seeds, ~150 pooled obs) -------------

test_that("RW Gaussian 90% PI covers near nominal across seeds", {
  res <- pooled_recovery(
    seeds = c(201L, 202L, 203L),
    sim_args = list(
      trend_model = RW(),
      family = gaussian(),
      n_timepoints = 200L,
      n_series = 1L,
      proportional_train = 0.75
    ),
    fit_args = list(
      formula = y ~ 1,
      trend_formula = ~ RW(),
      family = gaussian(),
      chains = 1L, iter = 500L, warmup = 250L,
      refresh = 0L, silent = 2L
    )
  )
  assert_pi_coverage(res$in_pi, res$total, nominal = 0.90)
})


# --- ZMVN 2-series Poisson recovery (3 seeds, ~180 pooled obs) ----

test_that("ZMVN 2-series 90% PI covers near nominal across seeds", {
  res <- pooled_recovery(
    seeds = c(301L, 302L, 303L),
    sim_args = list(
      trend_model = ZMVN(),
      family = poisson(),
      n_timepoints = 120L,
      n_series = 2L,
      proportional_train = 0.75
    ),
    fit_args = list(
      formula = y ~ 1,
      trend_formula = ~ ZMVN(),
      family = poisson(),
      chains = 1L, iter = 500L, warmup = 250L,
      refresh = 0L, silent = 2L
    )
  )
  assert_pi_coverage(res$in_pi, res$total, nominal = 0.90)
})
