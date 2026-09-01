# The trend-formula linear predictor in the forecast window.
#
# Every generated program builds the series-scale trend the same
# way, whatever the trend type:
#
#   trend[t, s] = Z[s, ] . lv_trend[t, ] + mu_trend[t, s]
#
# so the latent dynamics are zero-mean and the trend linear
# predictor enters once, at series scale, after the loadings
# projection. Forecasting has to reproduce that. Shifting a
# trend-side covariate by a known amount must therefore shift the
# link-scale forecast by exactly the covariate's coefficient times
# that amount, on every draw: the latent innovations cancel
# because both forecasts are drawn from the same seed, and the
# observation formula does not mention the covariate.
#
# The identity is exact rather than statistical, which makes it a
# sharp test. A path that drops `mu_trend` returns a difference of
# exactly zero.
#
# Fits are cached under
# `tests/local/fixtures/forecast_recovery/fc_lp_<name>.rds` so
# reruns load rather than sample.

source("setup_tests_local.R")
if (file.exists("tests/local/concordance_helpers.R")) {
  source("tests/local/concordance_helpers.R")
} else if (file.exists("concordance_helpers.R")) {
  source("concordance_helpers.R")
}

CACHE_DIR <- "fixtures/forecast_recovery"

B_ENV <- 0.8


# ----- Data and cache helpers -------------------------------------

# Data whose latent trend genuinely depends on a trend-side
# covariate. `env` is deliberately absent from the observation
# formula, so any forecast response to it can only arrive through
# `mu_trend`.
sim_trend_env <- function(seed, n_time = 40L, n_series = 4L,
                            n_lv = 2L, h = 6L) {
  set.seed(seed)
  total_t <- n_time + h
  env <- as.numeric(scale(rnorm(total_t)))
  Z <- matrix(rnorm(n_series * n_lv, 0, 0.6), n_series, n_lv)
  lv <- matrix(rnorm(total_t * n_lv, 0, 0.4), total_t, n_lv)
  trend <- lv %*% t(Z) + B_ENV * env
  y <- matrix(rpois(length(trend), exp(1.4 + trend)),
                nrow = total_t)
  d <- data.frame(
    y = as.numeric(y),
    env = rep(env, times = n_series),
    time = rep(seq_len(total_t), times = n_series),
    series = factor(rep(paste0("s", seq_len(n_series)),
                          each = total_t))
  )
  list(train = d[d$time <= n_time, ], test = d[d$time > n_time, ])
}


prep_lp_fit <- function(name, trend_formula, sim) {
  path <- file.path(CACHE_DIR, paste0("fc_lp_", name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  fit <- SM(SW(mvgam(
    y ~ 1, trend_formula = trend_formula, data = sim$train,
    family = poisson(), chains = 2, iter = 1000, warmup = 500,
    silent = 2, refresh = 0, backend = "cmdstanr"
  )))
  out <- list(sim = sim, fit = fit)
  saveRDS(out, path)
  out
}


# Forecast twice from one fit on newdata differing only in `env`,
# and return the per-draw, per-cell difference on the link scale
# alongside the posterior draws of the covariate's coefficient.
env_shift_effect <- function(bundle, shift = 1) {
  fit <- bundle$fit
  nd_base <- bundle$sim$test
  nd_base$env <- 0
  nd_shift <- bundle$sim$test
  nd_shift$env <- shift
  set.seed(7)
  fc_base <- forecast(fit, newdata = nd_base, type = "link")
  set.seed(7)
  fc_shift <- forecast(fit, newdata = nd_shift, type = "link")
  dm <- posterior::as_draws_matrix(fit)
  b_name <- grep("^b_env_trend$", colnames(dm), value = TRUE)
  list(
    diffs = Map(function(a, b) b - a,
                  fc_base$forecasts, fc_shift$forecasts),
    b_draws = as.numeric(dm[, b_name]),
    shift = shift
  )
}


# The shift must equal `b * shift` on every draw and every
# forecast time, and must be materially non-zero so that a path
# silently dropping `mu_trend` cannot pass.
expect_carries_trend_linpred <- function(eff) {
  expect_gt(abs(mean(eff$b_draws)), 0.1)
  for (nm in names(eff$diffs)) {
    d <- eff$diffs[[nm]]
    # Pairing rows with coefficient draws only holds while the
    # forecast uses every draw in posterior order.
    expect_equal(nrow(d), length(eff$b_draws))
    expected <- eff$b_draws[seq_len(nrow(d))] * eff$shift
    expect_lt(max(abs(sweep(d, 1, expected, "-"))), 1e-8)
    expect_gt(mean(abs(d)), 0.1)
  }
}


sim_mv <- sim_trend_env(seed = 401L)
sim_uni <- sim_trend_env(seed = 402L, n_series = 1L, n_lv = 1L)


# ----- One convention, every trend type ---------------------------

test_that("a ZMVN factor forecast carries the trend linpred", {
  eff <- env_shift_effect(
    prep_lp_fit("zmvn_lv", ~ env + ZMVN(n_lv = 2), sim_mv)
  )
  expect_carries_trend_linpred(eff)
})


test_that("an RW factor forecast carries the trend linpred", {
  eff <- env_shift_effect(
    prep_lp_fit("rw_lv", ~ env + RW(n_lv = 2), sim_mv)
  )
  expect_carries_trend_linpred(eff)
})


test_that("a VAR factor forecast carries the trend linpred", {
  eff <- env_shift_effect(
    prep_lp_fit("var_lv", ~ env + VAR(n_lv = 2), sim_mv)
  )
  expect_carries_trend_linpred(eff)
})


test_that("a univariate CAR forecast carries the trend linpred", {
  # Multivariate CAR refuses trend covariates outright, because
  # its time intervals vary by series, so the univariate fit is
  # the whole of the CAR surface here.
  eff <- env_shift_effect(
    prep_lp_fit("car", ~ env + CAR(), sim_uni)
  )
  expect_carries_trend_linpred(eff)
})


test_that("a PW forecast carries the trend linpred", {
  eff <- env_shift_effect(
    prep_lp_fit("pw", ~ env + PW(), sim_mv)
  )
  expect_carries_trend_linpred(eff)
})


test_that("ragged per-series horizons keep the identity", {
  # `lp_forecast` is padded to the longest horizon and its final
  # row repeats, so a series that stops early must not pick up a
  # padded cell. Horizons here run 6, 4, 2, 6.
  bundle <- prep_lp_fit("zmvn_lv", ~ env + ZMVN(n_lv = 2), sim_mv)
  keep <- c(s1 = 6L, s2 = 4L, s3 = 2L, s4 = 6L)
  ragged <- do.call(rbind, lapply(names(keep), function(s) {
    d <- bundle$sim$test[bundle$sim$test$series == s, ]
    d[order(d$time), ][seq_len(keep[[s]]), ]
  }))
  ragged_bundle <- list(fit = bundle$fit,
                          sim = list(test = ragged))
  eff <- env_shift_effect(ragged_bundle)
  expect_equal(unname(vapply(eff$diffs, ncol, integer(1L))),
                 unname(keep))
  expect_carries_trend_linpred(eff)
})


test_that("a series-grain AR forecast carries the trend linpred", {
  # The one path that was already correct, kept here so the shared
  # convention is pinned for series-grain fits too.
  require_fixtures("val_mvgam_ar1_fx_trend.rds")
  fit <- load_mvgam("ar1_fx_trend")
  nd <- utils::tail(fit$data[order(fit$data$time), ], 5L)
  nd$time <- seq.int(max(fit$data$time) + 1L,
                       max(fit$data$time) + 5L)
  nd$y <- NA
  nd_base <- nd
  nd_base$x <- 0
  nd_shift <- nd
  nd_shift$x <- 1
  set.seed(7)
  fc_base <- forecast(fit, newdata = nd_base, type = "link")
  set.seed(7)
  fc_shift <- forecast(fit, newdata = nd_shift, type = "link")
  dm <- posterior::as_draws_matrix(fit)
  b_draws <- as.numeric(dm[, "b_x_trend"])
  d <- fc_shift$forecasts[[1L]] - fc_base$forecasts[[1L]]
  expect_lt(max(abs(sweep(d, 1, b_draws[seq_len(nrow(d))], "-"))),
              1e-8)
})
