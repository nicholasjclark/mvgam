# Recovery and post-fit coverage for a correlated AR(2), fitted in this
# file.
#
# Every other `AR()` fit in this directory gives each series its own
# independent innovation. The trend covariance is diagonal there and
# `residual_cor()` refuses for want of correlations. Combining an
# order above one with a correlation between series makes three claims
# answerable here:
#
#   the first `p` states are drawn from the joint stationary
#   distribution of `(lv_t, lv_{t-1})`, which needs a Lyapunov solve
#   on the companion form and reduces to no elementwise formula,
#
#   the covariance a marginal prediction integrates over is the
#   leading block of that solve, whose off-diagonal a per-series
#   variance multiplier cannot reach, and
#
#   `residual_cor()` reports the correlation the states settle at,
#   which its own documentation promises and which differs from the
#   correlation of one innovation.
#
# The coefficients are spread widely across the three series on
# purpose. A stationary cross-covariance takes
# `1 / (1 - ar_i * ar_j)`, and the wider the spread the further that
# value departs from the geometric mean of each series' own factor.
# Measured on this posterior the two correlations differ by 0.099 at
# the median and 0.181 at the widest. An assertion on the correlation
# separates them; one confined to the variances would pass under
# either.
#
#   truth: 3 series, 150 occasions, gaussian, latent AR(2) with
#          correlated innovations
#   model: y ~ 1, trend_formula = ~ AR(p = 2, cor = TRUE)
#
# Series named out of alphabetical order, occasions numbered from 5.
#
# Ground truth for every stationary quantity is a Neumann sum written
# from the definition, `neumann_omega()` in helper-local.R. It
# calls nothing from the package, and every assertion below tests the
# package against that identity.
#
# This posterior reports an E-BFMI below 0.3, which is ordinary for a
# state-space model separating an observation variance from an
# innovation variance. The blocks that matter here are per-draw
# algebraic identities and hold whatever the chains explored. Only the
# recovery block depends on mixing, and it is loose by design.
#
# Cached at tests/local/fixtures/val_mvgam_ar2_correlated.rds. Delete
# to refit.
#
# Run with:
#   testthat::test_file("tests/local/test-trend-ar-correlated.R")

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(posterior)
  library(testthat)
})

# This file fits its own model and caches it in the fixtures
# directory. It depends on no shared fixture and no build step.

set.seed(4021L)

n_series <- 3L
n_time <- 150L
p_order <- 2L
series_levels <- c("delta", "beta", "alpha")
stopifnot(!identical(series_levels, sort(series_levels)))

# A wide spread of coefficients across series is what separates the
# joint cross-covariance from the geometric mean of each series' own
# factor.
phi1_true <- c(0.60, 0.20, -0.30)
phi2_true <- c(0.20, 0.35, 0.25)
sigma_true <- c(0.40, 0.50, 0.30)
R_true <- matrix(c(1.0, 0.60, 0.30,
                   0.60, 1.0, -0.20,
                   0.30, -0.20, 1.0), 3, 3)
stopifnot(min(eigen(R_true, symmetric = TRUE)$values) > 0.05)
L_true <- t(chol(R_true))
Sigma_true <- diag(sigma_true) %*% R_true %*% diag(sigma_true)

# Burn in past the initial condition, which draws the simulated latent
# state from the stationary distribution the model starts at.
burn <- 500L
nt_sim <- n_time + burn
E <- matrix(rnorm(nt_sim * n_series), nt_sim, n_series) %*% t(L_true)
E <- sweep(E, 2L, sigma_true, "*")
lat <- matrix(0, nt_sim, n_series)
for (t in 3:nt_sim) {
  lat[t, ] <- phi1_true * lat[t - 1L, ] + phi2_true * lat[t - 2L, ] + E[t, ]
}
latent <- lat[(burn + 1L):nt_sim, , drop = FALSE]

time_vals <- seq_len(n_time) + 4L
obs_sd <- 0.15
dat <- data.frame(
  y = as.numeric(latent) + rnorm(n_time * n_series, 0, obs_sd),
  time = rep(time_vals, times = n_series),
  series = factor(rep(series_levels, each = n_time), levels = series_levels)
)

sim_truth <- list(
  phi1 = phi1_true, phi2 = phi2_true, sigma = sigma_true,
  R = R_true, Sigma = Sigma_true, obs_sd = obs_sd,
  latent = latent, series_levels = series_levels, time = time_vals
)

trend_rhs <- ~ AR(p = 2, cor = TRUE)


# -- Ground truth, written from the definition ------------------------

# The innovation covariance of one draw, rebuilt from the scale and the
# correlation factor the posterior stores.
draw_sigma <- function(dm, k, m) {
  sg <- vapply(seq_len(m), function(s) {
    as.numeric(dm[k, sprintf("sigma_trend[%d]", s)])
  }, numeric(1))
  Lw <- matrix(0, m, m)
  for (a in seq_len(m)) {
    for (b in seq_len(m)) {
      Lw[a, b] <- as.numeric(dm[k, sprintf("L_Omega_trend[%d,%d]", a, b)])
    }
  }
  diag(sg) %*% tcrossprod(Lw) %*% diag(sg)
}

draw_coefs <- function(dm, k, m, lag) {
  vapply(seq_len(m), function(s) {
    as.numeric(dm[k, sprintf("ar%d_trend[%d]", lag, s)])
  }, numeric(1))
}


# -- Fit --------------------------------------------------------------

fit <- cached_fit(
  "val_mvgam_ar2_correlated.rds",
  function() {
    mvgam(
      y ~ 1, trend_formula = trend_rhs, data = dat,
      family = gaussian(), chains = 2L, iter = 2000L, warmup = 1000L,
      control = list(adapt_delta = 0.95, max_treedepth = 12),
      silent = 2, backend = "cmdstanr"
    )
  },
  key = sim_truth
)

dm <- posterior::as_draws_matrix(fit$fit)
check_draws <- unique(round(seq(1, nrow(dm), length.out = 25L)))



test_that("the joint solve supplies the first two latent states", {
  # The claim the Stan half of this work exists for, and the sharpest
  # one available. `lv_trend[1:2, ]` is a deterministic function of
  # `innovations_trend[1:2, ]` within each draw. The identity holds to
  # stored precision or fails outright.
  #
  # Measured against these same draws, three implementations that each
  # keep every value finite and every dimension right land between
  # 0.73 and 0.90 away: scaling the innovations by `Sigma` alone, which
  # is what the program did before this work; applying the one-lag
  # closed form `Sigma / (1 - ar_i ar_j)` to a model of two lags; and
  # filling the two states in the opposite time order.
  for (k in check_draws) {
    Sig <- draw_sigma(dm, k, n_series)
    phi1 <- draw_coefs(dm, k, n_series, 1L)
    phi2 <- draw_coefs(dm, k, n_series, 2L)
    z <- matrix(NA_real_, p_order, n_series)
    lv <- matrix(NA_real_, p_order, n_series)
    for (i in seq_len(p_order)) {
      for (j in seq_len(n_series)) {
        z[i, j] <- as.numeric(dm[k, sprintf("innovations_trend[%d,%d]", i, j)])
        lv[i, j] <- as.numeric(dm[k, sprintf("lv_trend[%d,%d]", i, j)])
      }
    }
    state <- as.numeric(t(chol(neumann_omega(Sig, list(phi1, phi2)))) %*%
                          as.numeric(t(z)))
    block <- function(i) state[((i - 1L) * n_series + 1L):(i * n_series)]
    # The companion state runs most recent block first. Block one is
    # the later of the two occasions.
    expect_equal(as.numeric(lv[2L, ]), block(1L), tolerance = 1e-6)
    expect_equal(as.numeric(lv[1L, ]), block(2L), tolerance = 1e-6)
  }
})


test_that("the two initial states take a fixed time order", {
  # The gate that shows the block above can fail. Filling the states
  # in the opposite time order is the mistake the companion layout
  # invites, and the draws separate the two orders by order one.
  k <- check_draws[1L]
  Sig <- draw_sigma(dm, k, n_series)
  phi1 <- draw_coefs(dm, k, n_series, 1L)
  phi2 <- draw_coefs(dm, k, n_series, 2L)
  z <- matrix(NA_real_, p_order, n_series)
  for (i in seq_len(p_order)) {
    for (j in seq_len(n_series)) {
      z[i, j] <- as.numeric(dm[k, sprintf("innovations_trend[%d,%d]", i, j)])
    }
  }
  state <- as.numeric(t(chol(neumann_omega(Sig, list(phi1, phi2)))) %*%
                        as.numeric(t(z)))
  lv1 <- as.numeric(dm[k, sprintf("lv_trend[1,%d]", seq_len(n_series))])
  reversed <- state[seq_len(n_series)]
  expect_gt(max(abs(lv1 - reversed)), 1e-3)
})


test_that("the marginal covariance is the solve's leading block", {
  # What a marginal prediction integrates over. `draw_ids` and not
  # `ndraws`, because `ndraws` takes a random subset, which would pair
  # each covariance with another draw's coefficients.
  ids <- seq_len(30L)
  cs <- get_trend_covariance_structure(fit, draw_ids = ids)
  for (k in ids) {
    Sig <- draw_sigma(dm, k, n_series)
    phi1 <- draw_coefs(dm, k, n_series, 1L)
    phi2 <- draw_coefs(dm, k, n_series, 2L)
    want <- neumann_omega(Sig, list(phi1, phi2))[seq_len(n_series),
                                         seq_len(n_series)]
    s_k <- cs$params$sigma_trend[k, ]
    L_k <- matrix(cs$params$L_Omega_trend[k, , ], n_series, n_series)
    got <- diag(s_k) %*% tcrossprod(L_k) %*% diag(s_k)
    expect_equal(unname(got), unname(want), tolerance = 1e-8)
  }
})


test_that("the cross-covariance needs the joint solve", {
  # Why the scalar factor each series would take alone is
  # insufficient. The variances agree by construction. The
  # off-diagonals do not, and this block records by how much on this
  # posterior. An assertion confined to the variances would pass under
  # the multiplier and state nothing.
  gaps <- vapply(check_draws, function(k) {
    Sig <- draw_sigma(dm, k, n_series)
    phi1 <- draw_coefs(dm, k, n_series, 1L)
    phi2 <- draw_coefs(dm, k, n_series, 2L)
    G0 <- neumann_omega(Sig, list(phi1, phi2))[seq_len(n_series),
                                       seq_len(n_series)]
    # A per-series multiplier rescales each variance and leaves the
    # correlation where the innovations put it.
    max(abs(stats::cov2cor(G0) - stats::cov2cor(Sig)))
  }, numeric(1))
  expect_gt(median(gaps), 0.05)
})


test_that("residual_cor reports the correlation the states settle at", {
  # The documented contract: the covariance summarised is the one the
  # latent states settle at, which for `AR(cor = TRUE)` is the
  # stationary covariance. Both halves are asserted. Matching the
  # stationary correlation states something only once the innovation
  # correlation is known to differ, which the block above establishes.
  cors <- residual_cor(fit, summary = FALSE)$cor_draws
  expect_identical(dim(cors)[2:3], c(n_series, n_series))
  for (k in check_draws) {
    Sig <- draw_sigma(dm, k, n_series)
    phi1 <- draw_coefs(dm, k, n_series, 1L)
    phi2 <- draw_coefs(dm, k, n_series, 2L)
    G0 <- neumann_omega(Sig, list(phi1, phi2))[seq_len(n_series),
                                       seq_len(n_series)]
    got <- matrix(cors[k, , ], n_series, n_series)
    expect_equal(unname(got), unname(stats::cov2cor(G0)),
                 tolerance = 1e-8)
  }
  # The summarised matrix keeps the series the sampler was given.
  rc <- residual_cor(fit)
  expect_identical(rownames(rc$cor), series_levels)
})


test_that("the identified quantities recover the simulated truth", {
  # Recovery establishes that the fit is worth asserting against. This
  # posterior reports a low E-BFMI. The bounds are loose here, and the
  # per-draw identities above are what this file tests.
  post <- posterior_summary(fit)
  for (s in seq_len(n_series)) {
    expect_equal(unname(post[sprintf("sigma_trend[%d]", s), "Estimate"]),
                 sigma_true[s], tolerance = 0.25)
  }
  expect_equal(unname(post["sigma", "Estimate"]), obs_sd, tolerance = 0.1)
  # The latent path is what the trend is for, and it is identified
  # without reference to the coefficients that generated it.
  for (s in seq_len(n_series)) {
    lv_hat <- vapply(seq_len(n_time), function(t) {
      mean(as.numeric(dm[, sprintf("lv_trend[%d,%d]", t, s)]))
    }, numeric(1))
    expect_gt(cor(lv_hat, latent[, s]), 0.85)
  }
})


test_that("every prediction surface returns a value for every row", {
  n_obs <- nrow(dat)
  ep <- posterior_epred(fit, draw_ids = 1:20)
  pp <- posterior_predict(fit, draw_ids = 1:20)
  expect_identical(dim(ep), c(20L, n_obs))
  expect_identical(dim(pp), c(20L, n_obs))
  expect_true(all(is.finite(ep)))
  expect_identical(nrow(fitted(fit, draw_ids = 1:20)), n_obs)
  expect_identical(nrow(residuals(fit, draw_ids = 1:20)), n_obs)
  expect_identical(nrow(augment(fit)), n_obs)
})


test_that("the marginal envelope spans the stationary variance", {
  # The user-facing consequence of the lift. `incl_autocor = FALSE`
  # alone drops the latent state entirely and leaves the observation
  # noise as the whole spread. Adding the envelope takes
  # `process_error = TRUE`.
  #
  # Each series' marginal predictive variance is then its stationary
  # trend variance plus the observation variance. Measured here the
  # ratio falls within 0.02 of one for each series. Against the
  # innovation variance the same ratios reach 2.4, 1.2 and 1.06. The
  # most persistent series separates the two accounts by more than a
  # factor of two.
  ids <- seq_len(100L)
  marg <- posterior_predict(fit, draw_ids = ids, incl_autocor = FALSE,
                            process_error = TRUE)
  bare <- posterior_predict(fit, draw_ids = ids, incl_autocor = FALSE)
  expect_gt(sd(as.numeric(marg)), 2 * sd(as.numeric(bare)))

  obs_var <- mean(as.numeric(dm[ids, "sigma"])^2)
  stat_var <- innov_var <- matrix(NA_real_, length(ids), n_series)
  for (i in seq_along(ids)) {
    k <- ids[i]
    Sig <- draw_sigma(dm, k, n_series)
    phi1 <- draw_coefs(dm, k, n_series, 1L)
    phi2 <- draw_coefs(dm, k, n_series, 2L)
    G0 <- neumann_omega(Sig, list(phi1, phi2))[seq_len(n_series),
                                       seq_len(n_series)]
    stat_var[i, ] <- diag(G0)
    innov_var[i, ] <- diag(Sig)
  }
  ratios <- vapply(seq_along(series_levels), function(si) {
    cols <- which(as.character(dat$series) == series_levels[si])
    got <- var(as.numeric(marg[, cols]))
    c(stationary = got / (mean(stat_var[, si]) + obs_var),
      innovation = got / (mean(innov_var[, si]) + obs_var))
  }, numeric(2))
  expect_equal(unname(ratios["stationary", ]), rep(1, n_series),
               tolerance = 0.1)
  # The innovation account is rejected where the lift is largest.
  expect_gt(max(ratios["innovation", ]), 1.8)
})


test_that("the hindcast and forecast blocks keep the model's series", {
  hc <- hindcast(fit, type = "expected")
  expect_identical(names(hc$hindcasts), series_levels)
  fut <- data.frame(
    time = rep(max(time_vals) + seq_len(4L), times = n_series),
    series = factor(rep(series_levels, each = 4L), levels = series_levels),
    y = NA_real_
  )
  fc <- forecast(fit, newdata = fut, ndraws = 50L, type = "trend")
  expect_identical(names(fc$forecasts), series_levels)
  for (s in series_levels) {
    expect_identical(dim(fc$forecasts[[s]])[2L], 4L)
    expect_true(all(is.finite(fc$forecasts[[s]])))
    expect_identical(as.integer(fc$train_times[[s]]), time_vals)
  }
})


test_that("the reporting methods return on a correlated AR(2)", {
  expect_s3_class(summary(fit), "mvgam_summary")
  expect_identical(nobs(fit), nrow(dat))
  expect_gt(nrow(tidy(fit)), 0L)
  expect_identical(nrow(glance(fit)), 1L)
  expect_true(all(c("sigma_trend[1]", "ar1_trend[1]", "ar2_trend[1]") %in%
                    rownames(posterior_summary(fit))))
})


test_that("the factor plot refuses a fit whose latents are its series", {
  # `n_lv` equals the series count here, which makes each latent state
  # one series. The refusal names the argument that would reduce them.
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_error(plot(fit, type = "factors"),
               "has no latent factors to plot")
  for (ty in c("trend", "series", "residuals")) {
    expect_s3_class(plot(fit, type = ty), "ggplot")
  }
  expect_s3_class(pp_check(fit, ndraws = 20L), "ggplot")
})


test_that("every plot draws the occasions the user supplied", {
  # The frame is numbered from five. An axis drawn on the rank runs
  # 1..150 where the user gave 5..154.
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  want <- as.numeric(range(time_vals))
  for (ty in c("trend", "series")) {
    expect_equal(drawn_x(plot(fit, type = ty)), want, tolerance = 0.02)
  }
})


cat("\nDone.\n")
