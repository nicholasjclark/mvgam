# CI tests for the trend-propagation dispatcher in
# `R/trend_propagation.R`. Covers every supported trend type
# plus the sparse-lag, MA, and cross-series-correlation paths.

# ---- Dispatch -----------------------------------------------------

test_that("every trend propagates an h by n_series matrix", {
  set.seed(5L)
  cases <- list(
    RW = list(RW(), list(sigma = 0.5), 1L),
    sparse_AR = list(AR(p = c(1L, 3L, 12L)),
                     list(ar = c(0.4, 0.2, 0.1), sigma = 0.5), 1L),
    shared_AR = list(AR(p = 2L),
                     list(ar = c(0.5, 0.2), sigma = rep(0.4, 4L)), 4L)
  )
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    out <- propagate_trend(cs[[1]], params = cs[[2]], h = 30L,
                           n_series = cs[[3]])
    expect_identical(dim(out), c(30L, cs[[3]]), label = nm)
    expect_true(all(is.finite(out)), label = nm)
  }
  expect_true(all(propagate_trend("None", h = 20L, n_series = 3L) == 0))
  # A CAR takes irregular gaps. An AR takes a supplied state.
  car <- propagate_trend(CAR(), params = list(phi = 0.7, sigma = 0.5),
                         h = 40L, n_series = 2L,
                         time = stats::runif(40L, 1, 6))
  expect_identical(dim(car), c(40L, 2L))
  expect_true(all(is.finite(car)))
  ls <- list(trends = matrix(0.5, nrow = 3L, ncol = 2L),
             errors = matrix(0, nrow = 1L, ncol = 2L), time = NULL)
  out <- propagate_trend(
    AR(p = 3L), params = list(ar = c(0.5, 0.2, 0.1), sigma = rep(0.4, 2L)),
    h = 10L, n_series = 2L, last_state = ls
  )
  expect_identical(dim(out), c(10L, 2L))
})

# ---- ZMVN ----------------------------------------------------------

test_that("propagate_trend(ZMVN()) returns IID MVN draws", {
  set.seed(11L)
  Sigma <- matrix(c(1, 0.5, 0.5, 1), 2L, 2L)
  out <- propagate_trend(ZMVN(), params = list(Sigma = Sigma),
                          h = 200L, n_series = 2L)
  expect_identical(dim(out), c(200L, 2L))
  # Sample covariance should approximate Sigma; check off-diag.
  expect_lt(abs(stats::cor(out[, 1L], out[, 2L]) - 0.5), 0.2)
})

# ---- AR and LLT -------------------------------------------------

test_that("the forecast kernel runs the recursion the Stan program fits", {
  # The generated Stan forms `u_t = e_t + theta * e_{t-1}` and takes
  # `x_t = phi * x_{t-1} + u_t`. The kernel multiplies its
  # coefficient by a past raw innovation. Writing both out here and
  # running them on one innovation sequence is the comparison a
  # string assertion on the generated program cannot make. Weighting
  # the previous formed value in Stan leaves these equal at the first
  # horizon and apart at every later one.
  phi <- 0.6; theta <- 0.5; h <- 8L
  set.seed(7L)
  e_hist <- rnorm(30L)
  x <- numeric(30L)
  x[1L] <- e_hist[1L]
  for (t in 2:30L) {
    x[t] <- phi * x[t - 1L] + e_hist[t] + theta * e_hist[t - 1L]
  }
  e_fut <- rnorm(h)
  fitted_fwd <- numeric(h)
  prev_x <- x[30L]
  prev_e <- e_hist[30L]
  for (k in seq_len(h)) {
    fitted_fwd[k] <- phi * prev_x + e_fut[k] + theta * prev_e
    prev_x <- fitted_fwd[k]
    prev_e <- e_fut[k]
  }
  kernel <- trend_arma_recursC(
    ar_lags = 1L, ma_lags = 1L, drift = 0,
    A = array(phi, dim = c(1L, 1L, 1L)),
    B = array(theta, dim = c(1L, 1L, 1L)),
    innovations = matrix(c(e_hist[30L], e_fut), ncol = 1L),
    linpreds = matrix(0, nrow = h + 1L, ncol = 1L),
    last_trends = matrix(x[30L], nrow = 1L, ncol = 1L),
    h = h
  )
  expect_equal(as.numeric(kernel[, 1L]), fitted_fwd, tolerance = 1e-12)
})

test_that("an ARMA(2, 1) step from a fitted state carries theta * e_T", {
  # A fitted state holds two states and one innovation. The first
  # forecast step is `ar1 y_T + ar2 y_{T-1} + theta e_T` plus a fresh
  # innovation, which a negligible scale removes. The history was
  # copied only when it filled both lag rows, which zeroed `e_T` here.
  ar <- c(0.5, 0.2)
  theta <- 0.6
  y <- c(-0.4, 1.3)
  e_T <- 0.9
  out <- propagate_trend(
    AR(p = 2L, ma = TRUE),
    params = list(ar = ar, theta = theta, sigma = 1e-10),
    h = 1L, n_series = 1L,
    last_state = list(trends = matrix(y, ncol = 1L),
                      errors = matrix(e_T, 1L, 1L))
  )
  expect_equal(as.numeric(out),
               ar[1L] * y[2L] + ar[2L] * y[1L] + theta * e_T,
               tolerance = 1e-8)
})

test_that("a burn-in hands on its final states and innovations", {
  # With no fitted state, propagation runs 200 steps from zero and
  # continues from the last of them. Known innovations replace the
  # draws, and a recursion written from the definition,
  #   y_t = ar1 y_{t-1} + ar2 y_{t-2} + e_t + theta e_{t-1},
  # gives the states the hand-over must carry. A hand-over one row out
  # of step pairs `e_{T-1}` with `y_T` and fails here, where a variance
  # check moves by 2%.
  ar <- c(0.5, 0.2)
  theta <- 0.6
  burn_in <- 200L
  max_lag <- 2L
  e <- sin(seq_len(burn_in + max_lag))
  local_mocked_bindings(
    rmvn = function(n, mu, Sigma, df = Inf) {
      matrix(e[seq_len(n)], n, length(mu))
    }
  )
  state <- run_burnin(
    ar_lags = 1:2, ma_lags = 1L, drift = 0,
    A_cube = array(ar, c(1L, 1L, 2L)), B_cube = array(theta, c(1L, 1L, 1L)),
    Sigma = matrix(1), n_series = 1L, burn_in = burn_in, max_lag = max_lag
  )
  # Rows 1 to `max_lag` of the innovations are the history before the
  # first step, and the steps take the rest.
  y <- numeric(burn_in + max_lag)
  for (t in max_lag + seq_len(burn_in)) {
    y[t] <- ar[1L] * y[t - 1L] + ar[2L] * y[t - 2L] + e[t] +
      theta * e[t - 1L]
  }
  expect_equal(as.numeric(state$trends), tail(y, max_lag),
               tolerance = 1e-12)
  expect_equal(as.numeric(state$errors), tail(e, max_lag))
})

test_that("an LLT forecast adds the slope to the level, damped by phi", {
  # The reference replays the recursion on the propagator's own draws:
  # level shocks first, then slope shocks. The slope path, row t the
  # slope after step t, rides along as the `slope` component.
  state <- list(trends = matrix(c(1, 2), 1L), errors = NULL,
                slope = c(0.5, -0.2))
  params <- list(sigma = c(0.3, 0.2), sigma_slope = c(0.1, 0.05),
                 phi_slope = c(0.8, 0.6))
  set.seed(9)
  out <- propagate_trend(LLT(damped = TRUE), params, h = 6L, n_series = 2L,
                         last_state = state)
  set.seed(9)
  eta <- rmvn(6L, c(0, 0), diag(c(0.09, 0.04)))
  zeta <- rmvn(6L, c(0, 0), diag(c(0.01, 0.0025)))
  level <- c(1, 2)
  slope <- c(0.5, -0.2)
  ref <- slopes <- matrix(0, 6L, 2L)
  for (t in 1:6) {
    level <- level + slope + eta[t, ]
    slope <- c(0.8, 0.6) * slope + zeta[t, ]
    ref[t, ] <- level
    slopes[t, ] <- slope
  }
  expect_equal(attr(out, "components"), list(level = ref, slope = slopes))
  attr(out, "components") <- NULL
  expect_equal(out, ref)
})

test_that("an LLT forecast turns each seasonal pair by its frequency", {
  # The fitted trend holds the level plus the season. The reference
  # replays the propagator's draws: level and slope shocks first, then
  # the shocks of the first and second states at each step.
  lam <- 2 * pi * (1:2) / 12
  season0 <- matrix(c(0.3, -0.1, 0.2, 0.05, -0.2, 0.4, 0.1, -0.3), 4L)
  state <- list(trends = matrix(c(1 + sum(season0[1:2, 1]),
                                  2 + sum(season0[1:2, 2])), 1L),
                errors = NULL, slope = c(0.1, -0.1), season = season0)
  params <- list(sigma = c(0.3, 0.2), sigma_slope = c(0.1, 0.05),
                 sigma_season = c(0.02, 0.04))
  spec <- LLT(season = 12, harmonics = 2)
  set.seed(5)
  out <- propagate_trend(spec, params, h = 4L, n_series = 2L,
                         last_state = state)
  set.seed(5)
  eta <- rmvn(4L, c(0, 0), diag(c(0.09, 0.04)))
  zeta <- rmvn(4L, c(0, 0), diag(c(0.01, 0.0025)))
  level <- c(1, 2)
  slope <- c(0.1, -0.1)
  g <- season0[1:2, ]
  g_star <- season0[3:4, ]
  ref <- slopes <- seasons <- matrix(0, 4L, 2L)
  for (t in 1:4) {
    level <- level + slope + eta[t, ]
    slope <- slope + zeta[t, ]
    w <- matrix(rnorm(4L, 0, rep(c(0.02, 0.04), each = 2L)), 2L)
    w_star <- matrix(rnorm(4L, 0, rep(c(0.02, 0.04), each = 2L)), 2L)
    g_last <- g
    g <- cos(lam) * g_last + sin(lam) * g_star + w
    g_star <- cos(lam) * g_star - sin(lam) * g_last + w_star
    seasons[t, ] <- colSums(g)
    slopes[t, ] <- slope
    ref[t, ] <- level + seasons[t, ]
  }
  expect_equal(attr(out, "components"),
               list(level = ref, slope = slopes, season = seasons))
  attr(out, "components") <- NULL
  expect_equal(out, ref)

  # Without shocks the season repeats every period and sums to zero
  # over one
  params$sigma_season <- c(0, 0)
  fixed <- attr(propagate_trend(spec, params, h = 24L, n_series = 2L,
                                last_state = state), "components")$season
  expect_equal(fixed[1:12, ], fixed[13:24, ])
  expect_equal(colSums(fixed[1:12, ]), c(0, 0))
})

test_that("an LLT forecast with two periods parts the season by period", {
  # Without shocks each period repeats on its own length
  spec <- LLT(season = c(4, 12), harmonics = c(1L, 2L))
  season0 <- matrix(c(0.3, -0.1, 0.2, 0.05, -0.2, 0.4), 6L)
  state <- list(trends = matrix(1 + sum(season0[1:3]), 1L), errors = NULL,
                slope = 0, season = season0)
  params <- list(sigma = 0.3, sigma_slope = 0.1, sigma_season = 0)
  paths <- attr(propagate_trend(spec, params, h = 24L, n_series = 1L,
                                last_state = state), "components")
  expect_equal(paths$season_4 + paths$season_12, paths$season)
  expect_equal(paths$season_4[1:20, ], paths$season_4[5:24, ])
  expect_equal(paths$season_12[1:12, ], paths$season_12[13:24, ])
})

# ---- VAR ----------------------------------------------------------

test_that("the VAR kernel multiplies each lag's matrix into its state", {
  # The Stan program steps as `Phi[1] x[t - 1] + Phi[2] x[t - 2]`.
  # Every coefficient off the diagonal differs from its transpose, so
  # a kernel applying `t(Phi)` or dropping a cross term lands
  # elsewhere.
  A <- array(c(0.5, 0.2, -0.1, 0.3, 0.4, 0.05, -0.2, 0.1, 0.3,
               0.1, -0.05, 0.02, 0.0, 0.15, -0.1, 0.08, 0.0, -0.12),
             dim = c(3L, 3L, 2L))
  h <- 6L
  set.seed(3L)
  e <- matrix(rnorm((h + 2L) * 3L), ncol = 3L)
  last <- matrix(c(0.4, -0.7, 1.1, -0.2, 0.9, 0.3), nrow = 2L,
                 byrow = TRUE)
  kernel <- trend_arma_recursC(
    ar_lags = 1:2, ma_lags = integer(0L), drift = rep(0, 3L),
    A = A, B = array(0, dim = c(3L, 3L, 0L)), innovations = e,
    linpreds = matrix(0, nrow = h + 2L, ncol = 3L),
    last_trends = last, h = h
  )
  x <- rbind(last, matrix(NA_real_, h, 3L))
  for (t in 3:(h + 2L)) {
    x[t, ] <- A[, , 1L] %*% x[t - 1L, ] + A[, , 2L] %*% x[t - 2L, ] + e[t, ]
  }
  expect_equal(kernel, x[3:(h + 2L), ], tolerance = 1e-12)
})

test_that("propagate_trend(VAR()) errors when A is missing", {
  expect_error(
    propagate_trend(
      VAR(p = 1L), params = list(Sigma = diag(3L)),
      h = 20L, n_series = 3L
    ),
    "VAR propagation requires 'A'"
  )
})

# ---- CAR ----------------------------------------------------------

test_that("correlated CAR steps by the exact continuous-time law", {
  # Series that damp at very different rates under a strong
  # correlation, across gaps below and above one.
  phi <- c(0.9, 0.1, 0.6)
  sigma <- c(0.5, 0.9, 0.3)
  rate <- -log(phi)
  cor_instant <- matrix(c(1, 0.9, -0.6, 0.9, 1, -0.4, -0.6, -0.4, 1), 3L)
  gain <- sqrt(2 * rate / (1 - phi^2))
  Sigma <- cor_instant * tcrossprod(sigma * gain) * (1 - tcrossprod(phi)) /
    outer(rate, rate, "+")
  gamma <- car_stationary_cov(phi, Sigma)

  # A gap of one returns the unit covariance, and two gaps compose to
  # their sum, which is what leaves the model unchanged when a time
  # is added between two others.
  expect_equal(car_gap_cov(phi, gamma, 1), Sigma)
  d1 <- 0.3
  d2 <- 1.7
  expect_equal(
    car_gap_cov(phi, gamma, d1 + d2),
    diag(phi^d2) %*% car_gap_cov(phi, gamma, d1) %*% diag(phi^d2) +
      car_gap_cov(phi, gamma, d2)
  )

  gaps <- c(0.004, 0.5, 1, 2.7)
  z <- matrix(c(0.3, -1.2, 0.8, 1.1, 0.2, -0.5,
                -0.7, 0.9, 1.4, -0.1, 0.6, -1.3), 4L, 3L)
  start <- c(0.4, -1.1, 0.2)
  seen_df <- NULL
  out <- with_mocked_bindings(
    propagate_trend(
      CAR(cor = TRUE, df = 5),
      params = list(phi = phi, sigma = sigma, Sigma = Sigma),
      h = 4L, n_series = 3L,
      last_state = list(trends = matrix(start, 1L)), time = gaps
    ),
    draw_trend_innovations = function(h, n_series, df) {
      seen_df <<- df
      z
    }
  )
  expected <- matrix(NA_real_, 4L, 3L)
  state <- start
  for (i in seq_along(gaps)) {
    step_cov <- gamma * (1 - tcrossprod(phi)^gaps[i])
    state <- phi^gaps[i] * state + as.numeric(t(chol(step_cov)) %*% z[i, ])
    expected[i, ] <- state
  }
  expect_equal(out, expected)
  # The constructor's degrees of freedom reach the innovations
  expect_identical(seen_df, 5)
})

test_that("propagate_trend(CAR()) errors when time is missing", {
  expect_error(
    propagate_trend(
      CAR(), params = list(phi = 0.7, sigma = 0.5),
      h = 10L, n_series = 1L
    ),
    "needs the forecast time gaps"
  )
})

test_that("CAR(1) phi <= 0 / >= 1 guard fires through dispatcher", {
  expect_error(
    propagate_trend(
      CAR(), params = list(phi = 1.0, sigma = 0.5),
      h = 5L, n_series = 1L, time = rep(1, 5L)
    ),
    "phi"
  )
  expect_error(
    propagate_trend(
      CAR(), params = list(phi = -0.5, sigma = 0.5),
      h = 5L, n_series = 1L, time = rep(1, 5L)
    ),
    "phi"
  )
})

# ---- Unsupported trend type ---------------------------------------

test_that("an unregistered trend type reaching the propagator is a fault", {
  # Registration requires every trend's propagator. Only a hand-built
  # object reaches the fault, which points the user at the issue
  # tracker.
  fake_trend <- structure(
    list(trend = "UNKNOWN"), class = "mvgam_trend"
  )
  expect_error(
    propagate_trend(fake_trend, list(), h = 10L, n_series = 1L),
    "lacks 'propagate_unknown\\(\\)'"
  )
})

# ----- PW: horizon-changepoint sampling reproducibility -----------

test_that("propagate_pw is deterministic under set.seed", {
  # Same params + same fc_times + same seed must produce the
  # same forecast trajectory (horizon changepoints sampled in
  # R-level RNG; once the seed is fixed, the result should be
  # bit-identical across calls).
  params <- list(
    k = 0.05, m = 0.5,
    delta = matrix(c(-0.1, 0.05, -0.08), ncol = 1L),
    t_change = c(20, 40, 60)
  )
  fc_times <- 81:100
  training_times <- 1:80
  spec <- structure(
    list(trend = "PW", growth = "linear"),
    class = "mvgam_trend"
  )
  set.seed(42L)
  a <- propagate_trend(
    spec, params, h = length(fc_times), n_series = 1L,
    fc_times = fc_times, training_times = training_times
  )
  set.seed(42L)
  b <- propagate_trend(
    spec, params, h = length(fc_times), n_series = 1L,
    fc_times = fc_times, training_times = training_times
  )
  expect_equal(a, b)
})

test_that("propagate_pw errors on mismatched k / m lengths", {
  spec <- structure(
    list(trend = "PW", growth = "linear"),
    class = "mvgam_trend"
  )
  params <- list(
    k = c(0.05, 0.1),  # length 2, but n_series = 1 below
    m = 0.5,
    delta = matrix(0.1, ncol = 1L),
    t_change = 10
  )
  expect_error(
    propagate_trend(
      spec, params, h = 5L, n_series = 1L,
      fc_times = 11:15, training_times = 1:10
    ),
    "params"
  )
})

test_that("propagate_pw needs a cap and takes one below zero", {
  spec <- structure(
    list(trend = "PW", growth = "logistic"),
    class = "mvgam_trend"
  )
  params <- list(
    k = 0.05, m = 0.5,
    delta = matrix(0.1, ncol = 1L),
    t_change = 10
  )
  expect_error(
    propagate_trend(
      spec, params, h = 5L, n_series = 1L,
      fc_times = 11:15, training_times = 1:10
    ),
    "cap"
  )
  # A log link puts a response-scale cap below one at a negative
  # link-scale value, which propagates to a negative trend
  out <- propagate_trend(
    spec, params, h = 5L, n_series = 1L,
    fc_times = 11:15, training_times = 1:10,
    cap = matrix(log(0.5), 5L, 1L)
  )
  expect_true(all(out < 0))
})
