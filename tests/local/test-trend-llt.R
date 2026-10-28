# A local linear trend, fitted in this file.
#
# The level adds the last slope and a shock, and the slope its own
# shock, damped by `ar1_slope_trend`. The stored states are rebuilt
# from the raw draws by that recursion, which pins the generated
# program against its definition. The fitted slope scale is held
# against the simulated one, which pins the default prior's scale.
#
# A damped slope is the reason the trend exists: a series still
# growing when the data end is forecast to keep growing at a falling
# rate and level off. A random walk holds its last level, a piecewise
# trend keeps its last rate and an AR returns to its mean. Every
# forecast is held against the closed form of its lead's mean and
# variance, and C and D compare the damped and undamped forecasts of
# one series.
#
#   A: 3 series, poisson, correlated shocks, damped slope
#   B: 5 series on 2 factors, poisson, undamped slope
#   C: 1 series, poisson with an offset, damped slope, still growing
#      when the data end
#   D: C's data under an undamped slope
#   E: 4 series in 2 regions, poisson, damped slope, level and slope
#      shocks correlated within each region (0.7 and -0.4)
#   F: 2 series, poisson, a twelve-month season of two harmonics whose
#      shape drifts
#
# Run with:
#   testthat::test_file("tests/local/test-trend-llt.R")

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(posterior)
  library(testthat)
})

sim_llt <- function(n_t, K, sigma, sigma_s, phi, R = diag(K), slope0) {
  Lc <- t(chol(R))
  level <- matrix(0, n_t, K)
  slope <- matrix(0, n_t, K)
  slope[1L, ] <- slope0
  level[1L, ] <- sigma * as.numeric(Lc %*% rnorm(K))
  for (t in 2:n_t) {
    level[t, ] <- level[t - 1L, ] + slope[t - 1L, ] +
      sigma * as.numeric(Lc %*% rnorm(K))
    slope[t, ] <- phi * slope[t - 1L, ] + sigma_s * as.numeric(Lc %*% rnorm(K))
  }
  list(level = level, slope = slope)
}

long_frame <- function(eta, names) {
  data.frame(time = rep(seq_len(nrow(eta)), ncol(eta)),
             series = factor(rep(names, each = nrow(eta)), levels = names),
             eta = as.vector(eta))
}

set.seed(22L)
R_true <- matrix(c(1, 0.6, 0.3, 0.6, 1, 0.5, 0.3, 0.5, 1), 3L)
truth_a <- sim_llt(80L, 3L, 0.1, 0.03, 0.85, R = R_true,
                   slope0 = c(0.05, -0.03, 0.02))
dat_a <- long_frame(truth_a$level, c("c", "a", "b"))
dat_a$y <- rpois(nrow(dat_a), exp(1.5 + dat_a$eta))

set.seed(24L)
truth_b <- sim_llt(60L, 2L, 1, 0.03, 1, slope0 = c(0.1, -0.05))
Z_true <- matrix(c(0.15, 0.1, 0.05, -0.05, 0, 0, 0.05, 0.12, 0.1, -0.1), 5L)
dat_b <- long_frame(truth_b$level %*% t(Z_true), paste0("s", 1:5))
dat_b$y <- rpois(nrow(dat_b), exp(1 + dat_b$eta))

fit_a <- cached_fit(
  "val_mvgam_llt_damped_cor.rds",
  function() {
    mvgam(y ~ 1, trend_formula = ~ LLT(cor = TRUE, damped = TRUE),
          data = dat_a, family = poisson(), chains = 2L, iter = 1000L,
          warmup = 500L, silent = 2, backend = "cmdstanr")
  },
  key = truth_a
)
fit_b <- cached_fit(
  "val_mvgam_llt_factor.rds",
  function() {
    mvgam(y ~ 1, trend_formula = ~ LLT(n_lv = 2), data = dat_b,
          family = poisson(), chains = 2L, iter = 1000L, warmup = 500L,
          silent = 2, backend = "cmdstanr")
  },
  key = truth_b
)

# Growth that slows: the slope starts high and decays, and is still
# positive when the 45 fitted occasions end. The 40 occasions after
# them hold the truth the forecast is checked against.
set.seed(26L)
truth_c <- sim_llt(85L, 1L, 0.05, 0.004, 0.93, slope0 = 0.25)
dat_c <- long_frame(truth_c$level, "s1")
dat_c$expo <- round(runif(nrow(dat_c), 0.5, 2), 3)
dat_c$y <- rpois(nrow(dat_c), dat_c$expo * exp(0.5 + dat_c$eta))
train_c <- dat_c[dat_c$time <= 45L, ]
fit_c <- cached_fit(
  "val_mvgam_llt_damped_growth.rds",
  function() {
    mvgam(y ~ offset(log(expo)), trend_formula = ~ LLT(damped = TRUE),
          data = train_c, family = poisson(), chains = 2L, iter = 1000L,
          warmup = 500L, silent = 2, backend = "cmdstanr")
  },
  key = truth_c
)
fit_d <- cached_fit(
  "val_mvgam_llt_undamped_growth.rds",
  function() {
    mvgam(y ~ offset(log(expo)), trend_formula = ~ LLT(),
          data = train_c, family = poisson(), chains = 2L, iter = 1000L,
          warmup = 500L, silent = 2, backend = "cmdstanr")
  },
  key = truth_c
)

# Two regions whose shocks correlate with opposite signs, strong
# enough for the pooled correlations to separate. Counts in the
# thousands leave a chain started at random stalled far from the
# posterior in about one fit in four, and Pathfinder starts each
# chain near it.
set.seed(52L)
cor_e <- list(matrix(c(1, 0.7, 0.7, 1), 2L), matrix(c(1, -0.4, -0.4, 1), 2L))
level_e <- slope_e <- matrix(0, 120L, 4L)
for (t in 2:120) {
  e <- c(t(chol(cor_e[[1L]])) %*% rnorm(2L), t(chol(cor_e[[2L]])) %*% rnorm(2L))
  z <- c(t(chol(cor_e[[1L]])) %*% rnorm(2L), t(chol(cor_e[[2L]])) %*% rnorm(2L))
  level_e[t, ] <- level_e[t - 1L, ] + slope_e[t - 1L, ] + 0.3 * e
  slope_e[t, ] <- 0.8 * slope_e[t - 1L, ] + 0.02 * z
}
dat_e <- expand.grid(time = 1:120, species = factor(c("sp1", "sp2")),
                     region = factor(c("r1", "r2")))
dat_e$y <- rpois(nrow(dat_e), exp(3 + as.vector(level_e) -
                                    rep(colMeans(level_e), each = 120L)))
train_e <- dat_e[dat_e$time <= 114L, ]
fit_e <- cached_fit(
  "val_mvgam_llt_grouped.rds",
  function() {
    mvgam(y ~ 1, trend_formula = ~ LLT(damped = TRUE, gr = region,
                                       subgr = species),
          data = train_e, family = poisson(), chains = 2L, iter = 1000L,
          warmup = 500L, silent = 2, backend = "cmdstanr",
          init = "pathfinder")
  },
  key = level_e
)

# The trigonometric season: harmonic j of period s turns its state pair
# by 2 pi j / s at each step and adds shocks of scale `sigma`
sim_season <- function(n_t, K, s, n_harm, sigma, amp) {
  freq <- 2 * pi * seq_len(n_harm) / s
  g <- matrix(rnorm(n_harm * K, 0, amp), n_harm, K)
  g_star <- matrix(rnorm(n_harm * K, 0, amp), n_harm, K)
  season <- matrix(0, n_t, K)
  season[1L, ] <- colSums(g)
  for (t in 2:n_t) {
    g_last <- g
    g <- cos(freq) * g_last + sin(freq) * g_star + rnorm(n_harm * K, 0, sigma)
    g_star <- cos(freq) * g_star - sin(freq) * g_last +
      rnorm(n_harm * K, 0, sigma)
    season[t, ] <- colSums(g)
  }
  season
}

# Nine years of monthly counts on two series, a drifting level and a
# twelve-month season of two harmonics whose shape moves slowly. The
# last year holds the truth the forecast is checked against.
set.seed(61L)
truth_f <- list(
  level = sim_llt(108L, 2L, 0.04, 0.003, 1, slope0 = c(0.01, -0.01))$level,
  season = sim_season(108L, 2L, 12, 2L, 0.02, 0.4)
)
dat_f <- long_frame(truth_f$level + truth_f$season, c("s1", "s2"))
dat_f$y <- rpois(nrow(dat_f), exp(2 + dat_f$eta))
train_f <- dat_f[dat_f$time <= 96L, ]
fit_f <- cached_fit(
  "val_mvgam_llt_season.rds",
  function() {
    mvgam(y ~ 1, trend_formula = ~ LLT(season = 12, harmonics = 2),
          data = train_f, family = poisson(), chains = 2L, iter = 1000L,
          warmup = 500L, silent = 2, backend = "cmdstanr")
  },
  key = truth_f
)

cells <- function(name, nr, nc) {
  sprintf("%s[%d,%d]", name, rep(seq_len(nr), nc), rep(seq_len(nc), each = nr))
}
block <- function(dm, r, name, nr, nc) {
  matrix(as.numeric(dm[r, cells(name, nr, nc)]), nr, nc)
}

test_that("the correlated damped states follow the recursion", {
  dm <- as_draws_matrix(fit_a$fit)
  worst <- 0
  for (r in round(seq(1, nrow(dm), length.out = 6))) {
    sig <- as.numeric(dm[r, sprintf("sigma_trend[%d]", 1:3)])
    sds <- as.numeric(dm[r, sprintf("sigma_slope_trend[%d]", 1:3)])
    phi <- as.numeric(dm[r, sprintf("ar1_slope_trend[%d]", 1:3)])
    L <- block(dm, r, "L_Omega_trend", 3L, 3L)
    eps <- block(dm, r, "innovations_trend", 80L, 3L) %*% t(diag(sig) %*% L)
    shocks <- block(dm, r, "innovations_slope_trend", 79L, 3L) %*%
      t(diag(sds) %*% L)
    slope <- matrix(0, 80L, 3L)
    level <- matrix(0, 80L, 3L)
    # The damped slope starts at its free initial value
    slope[1L, ] <- as.numeric(dm[r, sprintf("init_slope_trend[%d]", 1:3)])
    level[1L, ] <- eps[1L, ]
    for (t in 2:80) {
      level[t, ] <- level[t - 1L, ] + slope[t - 1L, ] + eps[t, ]
      slope[t, ] <- phi * slope[t - 1L, ] + shocks[t - 1L, ]
    }
    worst <- max(worst, abs(level - block(dm, r, "lv_trend", 80L, 3L)),
                 abs(slope - block(dm, r, "slope_trend", 80L, 3L)))
  }
  expect_lt(worst, 1e-5)
})

test_that("the slope scale covers the simulated one", {
  # A default prior that dominates holds the posterior near its own
  # scale. The first default kept every 95% interval below 0.006,
  # against a simulated 0.03.
  dm <- as_draws_matrix(fit_a$fit)
  for (k in 1:3) {
    q <- quantile(as.numeric(dm[, sprintf("sigma_slope_trend[%d]", k)]),
                  c(0.025, 0.975))
    expect_true(q[[1L]] < 0.03 && 0.03 < q[[2L]])
  }
})

test_that("the factors share one slope scale and keep their rotation", {
  dm <- as_draws_matrix(fit_b$fit)
  expect_identical(fit_factor_identification(fit_b), "rotation")
  expect_true("sigma_slope_trend[1]" %in% colnames(dm))
  expect_false("sigma_slope_trend[2]" %in% colnames(dm))
  # Unit level shocks and one scalar slope scale
  worst <- 0
  for (r in round(seq(1, nrow(dm), length.out = 6))) {
    sds <- as.numeric(dm[r, "sigma_slope_trend[1]"])
    eps <- block(dm, r, "innovations_trend", 60L, 2L)
    shocks <- block(dm, r, "innovations_slope_trend", 59L, 2L) * sds
    slope <- matrix(0, 60L, 2L)
    level <- matrix(0, 60L, 2L)
    slope[1L, ] <- as.numeric(dm[r, c("init_slope_trend[1]",
                                      "init_slope_trend[2]")])
    level[1L, ] <- eps[1L, ]
    for (t in 2:60) {
      level[t, ] <- level[t - 1L, ] + slope[t - 1L, ] + eps[t, ]
      slope[t, ] <- slope[t - 1L, ] + shocks[t - 1L, ]
    }
    worst <- max(worst, abs(level - block(dm, r, "lv_trend", 60L, 2L)))
  }
  expect_lt(worst, 1e-5)
})

test_that("the slope rotates with the loadings", {
  # The rotated slope reproduces each series' slope, Z times the factor
  # slopes, in every draw. The summary reports the rotated initial
  # slope, which a rotation of the factors leaves unchanged.
  dm <- as_draws_matrix(fit_b$fit)
  worst <- 0
  for (r in round(seq(1, nrow(dm), length.out = 6))) {
    raw <- block(dm, r, "Z", 5L, 2L) %*% t(block(dm, r, "slope_trend", 60L, 2L))
    rot <- block(dm, r, "Z_tilde", 5L, 2L) %*%
      t(block(dm, r, "slope_trend_tilde", 60L, 2L))
    worst <- max(worst, abs(raw - rot))
  }
  expect_lt(worst, 1e-6)
  shown <- rownames(summary(fit_b)$trend_spec)
  expect_true(any(startsWith(shown, "init_slope_trend_tilde[")))
  expect_false(any(grepl("^init_slope_trend\\[", shown)))
})

# Per-series parameters as a draws x K matrix. A shared scalar, as
# under factors, is repeated. An absent damping is one.
per_series <- function(dm, name, K) {
  cols <- grep(paste0("^", name, "\\["), colnames(dm), value = TRUE)
  if (!length(cols)) return(matrix(1, nrow(dm), K))
  m <- matrix(as.numeric(dm[, cols]), nrow(dm))
  m[, rep_len(seq_len(ncol(m)), K), drop = FALSE]
}

# The lead-h trend of one latent process, given the last level and
# slope, has mean `level + slope S_h` and variance
# `h sigma^2 + sigma_slope^2 (S_1^2 + ... + S_{h-1}^2)`, where
# `S_m = 1 + phi + ... + phi^(m - 1)`. The shock at lead i reaches
# the level through the slopes after it.
lead_moments <- function(dm, n_t, K, h, unit_level = FALSE) {
  phi <- per_series(dm, "ar1_slope_trend", K)
  sd_s <- per_series(dm, "sigma_slope_trend", K)
  sig <- if (unit_level) matrix(1, nrow(dm), K) else
    per_series(dm, "sigma_trend", K)
  S <- function(m) if (m == 0) 0 * phi else
    Reduce(`+`, lapply(0:(m - 1), function(j) phi^j))
  last <- function(name) {
    sapply(seq_len(K), function(k) {
      as.numeric(dm[, sprintf("%s[%d,%d]", name, n_t, k)])
    })
  }
  shocks <- Reduce(`+`, lapply(seq_len(h), function(i) S(h - i)^2))
  growth <- matrix(last("slope_trend"), ncol = K) * S(h)
  list(mean = matrix(last("lv_trend"), ncol = K) + growth,
       growth = growth, var = h * sig^2 + sd_s^2 * shocks)
}

# The forecast of every draw, standardised by its own lead moments,
# is a standard normal draw. When the last slopes take both signs, a
# wrong damping shifts the draws in opposite directions and leaves
# their mean near zero. It shows as a correlation between the
# standardised forecast and the slope's growth.
expect_lead_law <- function(fc, mom, Z = NULL) {
  for (s in seq_along(fc$forecasts)) {
    if (is.null(Z)) {
      mu <- mom$mean[, s]
      growth <- mom$growth[, s]
      v <- mom$var[, s]
    } else {
      mu <- rowSums(Z[[s]] * mom$mean)
      growth <- rowSums(Z[[s]] * mom$growth)
      v <- rowSums(Z[[s]]^2 * mom$var)
    }
    z <- (fc$forecasts[[s]][, mom$h] - mu) / sqrt(v)
    expect_lt(abs(mean(z)), 5 / sqrt(length(z)))
    expect_lt(abs(stats::sd(z) - 1), 0.08)
    expect_lt(abs(stats::cor(z, growth)), 5 / sqrt(length(z)))
  }
}

future_frame <- function(train, h, extra = NULL) {
  times <- max(train$time) + seq_len(h)
  out <- expand.grid(time = times, series = levels(train$series))
  out$series <- factor(out$series, levels = levels(train$series))
  out$y <- NA_real_
  if (!is.null(extra)) out <- merge(out, extra, by = c("time", "series"))
  out[order(out$series, out$time), ]
}

# A forecast frame for a grouped trend, which builds its series from
# the grouping columns
future_frame_grouped <- function(train, h) {
  out <- unique(train[, c("species", "region")])
  out <- out[rep(seq_len(nrow(out)), each = h), ]
  out$time <- max(train$time) + rep(seq_len(h), nrow(out) / h)
  out$y <- NA_real_
  out
}

test_that("each forecast lead follows the trend's closed form", {
  # A: correlated damped shocks, each series judged on its marginal
  dm <- as_draws_matrix(fit_a$fit)
  fc <- forecast(fit_a, newdata = future_frame(dat_a, 10L), type = "trend",
                 draw_ids = seq_len(nrow(dm)))
  for (h in c(1L, 10L)) {
    expect_lead_law(fc, c(lead_moments(dm, 80L, 3L, h), h = h))
  }

  # B: each series sums its factors through the loadings
  dm <- as_draws_matrix(fit_b$fit)
  fc <- forecast(fit_b, newdata = future_frame(dat_b, 5L), type = "trend",
                 draw_ids = seq_len(nrow(dm)))
  Z <- lapply(1:5, function(s) {
    sapply(1:2, function(k) as.numeric(dm[, sprintf("Z[%d,%d]", s, k)]))
  })
  for (h in c(1L, 5L)) {
    expect_lead_law(fc, c(lead_moments(dm, 60L, 2L, h, unit_level = TRUE),
                          h = h), Z = Z)
  }

  # C and D: one series, out to lead 40
  for (fit in list(fit_c, fit_d)) {
    dm <- as_draws_matrix(fit$fit)
    fc <- forecast(fit, newdata = future_frame(train_c, 40L, dat_c[, c(
      "time", "series", "expo")]), type = "trend",
      draw_ids = seq_len(nrow(dm)))
    for (h in c(1L, 10L, 40L)) {
      expect_lead_law(fc, c(lead_moments(dm, 45L, 1L, h), h = h))
    }
  }
})


test_that("a damped slope levels the forecast off", {
  # Both fits see a series still growing when its data end, and the
  # truth settles after it. The undamped slope carries the last growth
  # rate on, and the damped one lets it fade. The comparison is on the
  # link scale less the offset, where the simulated intercept is 0.5.
  future <- future_frame(train_c, 40L, dat_c[, c("time", "series", "expo")])
  path <- function(fit) {
    fc <- forecast(fit, newdata = future, type = "link")$forecasts$s1
    sweep(fc, 2L, log(future$expo))
  }
  truth <- 0.5 + truth_c$level[46:85, 1L]
  truth_rise <- truth[40L] - truth[20L]
  damped <- path(fit_c)
  undamped <- path(fit_d)

  # The rise from lead 20 to lead 40
  expect_lt(abs(stats::median(damped[, 40L] - damped[, 20L]) - truth_rise),
            0.1)
  expect_gt(stats::median(undamped[, 40L] - undamped[, 20L]) - truth_rise,
            0.2)

  # The truth at lead 40 lies inside the damped forecast's 90%
  # interval, and below the undamped forecast's median
  q <- stats::quantile(damped[, 40L], c(0.05, 0.95))
  expect_true(q[[1L]] < truth[40L] && truth[40L] < q[[2L]])
  expect_gt(stats::median(undamped[, 40L]), truth[40L])

  # The damping covers the simulated 0.93
  q <- stats::quantile(as.numeric(as_draws_matrix(fit_c$fit)[,
    "ar1_slope_trend[1]"]), c(0.025, 0.975))
  expect_true(q[[1L]] < 0.93 && 0.93 < q[[2L]])
})


test_that("a grouped trend correlates its shocks within each region", {
  dm <- as_draws_matrix(fit_e$fit)
  members <- fit_e$standata$group_members_trend
  group_cor <- function(r, g) {
    matrix(as.numeric(dm[r, sprintf("L_Omega_group_trend[%d,%d,%d]", g,
                                     c(1, 2, 1, 2), c(1, 1, 2, 2))]), 2L)
  }
  group_sd <- function(r, g) {
    as.numeric(dm[r, sprintf("sigma_group_trend[%d,%d]", g, 1:2)])
  }

  # The states follow the recursion, each region's shocks taking its
  # own correlation and the slope shocks their own scales
  worst <- 0
  for (r in round(seq(1, nrow(dm), length.out = 6))) {
    z <- block(dm, r, "innovations_trend", 114L, 4L)
    zs <- block(dm, r, "innovations_slope_trend", 113L, 4L)
    sds <- as.numeric(dm[r, sprintf("sigma_slope_trend[%d]", 1:4)])
    phi <- as.numeric(dm[r, sprintf("ar1_slope_trend[%d]", 1:4)])
    eps <- matrix(0, 114L, 4L)
    shocks <- matrix(0, 113L, 4L)
    for (g in 1:2) {
      m <- members[g, ]
      eps[, m] <- z[, m] %*% t(diag(group_sd(r, g)) %*% group_cor(r, g))
      shocks[, m] <- zs[, m] %*% t(diag(sds[m]) %*% group_cor(r, g))
    }
    level <- slope <- matrix(0, 114L, 4L)
    slope[1L, ] <- as.numeric(dm[r, sprintf("init_slope_trend[%d]", 1:4)])
    level[1L, ] <- eps[1L, ]
    for (t in 2:114) {
      level[t, ] <- level[t - 1L, ] + slope[t - 1L, ] + eps[t, ]
      slope[t, ] <- phi * slope[t - 1L, ] + shocks[t - 1L, ]
    }
    worst <- max(worst, abs(level - block(dm, r, "lv_trend", 114L, 4L)),
                 abs(slope - block(dm, r, "slope_trend", 114L, 4L)))
  }
  expect_lt(worst, 1e-5)

  # Each region's correlation covers its simulated value
  for (g in 1:2) {
    rho <- vapply(seq_len(nrow(dm)), function(r) {
      tcrossprod(group_cor(r, g))[1L, 2L]
    }, numeric(1L))
    q <- stats::quantile(rho, c(0.05, 0.95))
    expect_true(q[[1L]] < cor_e[[g]][1L, 2L] && cor_e[[g]][1L, 2L] < q[[2L]])
  }

  # At lead 6, each series' forecast follows its closed form, a pair in
  # one region correlates as the two shock laws imply, and a pair
  # across regions does not correlate
  fc <- forecast(fit_e, newdata = future_frame_grouped(train_e, 6L),
                 type = "trend", draw_ids = seq_len(nrow(dm)))
  h <- 6L
  n <- nrow(dm)
  S <- function(m, p) if (m == 0) 0 * p else (1 - p^m) / (1 - p)
  phi <- sapply(1:4, function(k) as.numeric(dm[, sprintf("ar1_slope_trend[%d]", k)]))
  sds <- sapply(1:4, function(k) as.numeric(dm[, sprintf("sigma_slope_trend[%d]", k)]))
  sig <- matrix(0, n, 4L)
  for (g in 1:2) {
    for (j in 1:2) {
      sig[, members[g, j]] <- as.numeric(dm[, sprintf("sigma_group_trend[%d,%d]", g, j)])
    }
  }
  zmat <- v <- matrix(0, n, 4L)
  for (k in 1:4) {
    mu <- as.numeric(dm[, sprintf("lv_trend[114,%d]", k)]) +
      as.numeric(dm[, sprintf("slope_trend[114,%d]", k)]) * S(h, phi[, k])
    v[, k] <- h * sig[, k]^2 +
      sds[, k]^2 * Reduce(`+`, lapply(1:h, function(i) S(h - i, phi[, k])^2))
    zmat[, k] <- (fc$forecasts[[k]][, h] - mu) / sqrt(v[, k])
    expect_lt(abs(mean(zmat[, k])), 5 / sqrt(n))
    expect_lt(abs(stats::sd(zmat[, k]) - 1), 0.08)
  }
  for (g in 1:2) {
    a <- members[g, 1L]
    b <- members[g, 2L]
    om <- vapply(seq_len(n), function(r) {
      tcrossprod(group_cor(r, g))[1L, 2L]
    }, numeric(1L))
    implied <- mean((h * sig[, a] * sig[, b] * om + sds[, a] * sds[, b] * om *
                       Reduce(`+`, lapply(1:h, function(i) {
                         S(h - i, phi[, a]) * S(h - i, phi[, b])
                       }))) / sqrt(v[, a] * v[, b]))
    expect_lt(abs(mean(zmat[, a] * zmat[, b]) - implied), 5 / sqrt(n))
  }
  expect_lt(abs(mean(zmat[, members[1L, 1L]] * zmat[, members[2L, 1L]])),
            5 / sqrt(n))
})


test_that("the hindcast components are the fitted level and slope", {
  ids <- 1:40
  dm <- as_draws_matrix(fit_a$fit)[ids, ]
  levs <- levels(dat_a$series)
  slope <- hindcast(fit_a, type = "trend", component = "slope",
                    draw_ids = ids)
  for (k in seq_along(levs)) {
    cols <- sprintf("slope_trend[%d,%d]", 1:80, k)
    expect_equal(unname(slope$hindcasts[[levs[k]]]),
                 matrix(as.numeric(dm[, cols]), nrow = length(ids)))
  }
  # Without a season or trend covariates the level is the summed trend
  expect_identical(
    hindcast(fit_a, type = "trend", component = "level",
             draw_ids = ids)$hindcasts,
    hindcast(fit_a, type = "trend", draw_ids = ids)$hindcasts
  )

  # A factor fit reports each series' slope: the factor slopes through
  # the sampled loadings, in every draw
  dm <- as_draws_matrix(fit_b$fit)[ids, ]
  slope <- hindcast(fit_b, type = "trend", component = "slope",
                    draw_ids = ids)
  worst <- 0
  for (r in seq_along(ids)) {
    want <- block(dm, r, "slope_trend", 60L, 2L) %*%
      t(block(dm, r, "Z", 5L, 2L))
    got <- sapply(1:5, function(s) slope$hindcasts[[paste0("s", s)]][r, ])
    worst <- max(worst, abs(got - want))
  }
  expect_lt(worst, 1e-10)
})


test_that("the forecast components step the fitted level and slope", {
  future <- future_frame(train_c, 40L, dat_c[, c("time", "series", "expo")])
  for (fit in list(fit_c, fit_d)) {
    dm <- as_draws_matrix(fit$fit)
    ids <- seq_len(nrow(dm))
    # With no trend covariates the level is the summed trend, draw for
    # draw under one seed
    expect_identical(
      withr::with_seed(3L, forecast(fit, newdata = future, type = "trend",
                                    component = "level",
                                    draw_ids = ids))$forecasts,
      withr::with_seed(3L, forecast(fit, newdata = future, type = "trend",
                                    draw_ids = ids))$forecasts
    )
    # The lead-h slope of a draw is `phi^h` times its last slope plus
    # shocks of variance `sigma_slope^2 (1 + phi^2 + ... + phi^(2h - 2))`.
    # The undamped fit has `phi = 1`.
    fc <- forecast(fit, newdata = future, type = "trend",
                   component = "slope", draw_ids = ids)$forecasts$s1
    phi <- per_series(dm, "ar1_slope_trend", 1L)[, 1L]
    sd_s <- per_series(dm, "sigma_slope_trend", 1L)[, 1L]
    last <- as.numeric(dm[, "slope_trend[45,1]"])
    for (h in c(1L, 10L, 40L)) {
      v <- sd_s^2 * Reduce(`+`, lapply(0:(h - 1L), function(j) phi^(2 * j)))
      z <- (fc[, h] - phi^h * last) / sqrt(v)
      expect_lt(abs(mean(z)), 5 / sqrt(length(z)))
      expect_lt(abs(stats::sd(z) - 1), 0.08)
    }
  }
})


test_that("a component plots on its own axis and needs the trend scale", {
  p <- plot(fit_c, type = "trend", component = "slope")
  expect_drawn(p)
  expect_identical(p$labels$y, "Latent slope")
  expect_error(hindcast(fit_c, type = "link", component = "slope"),
               "requires type = 'trend'")
  expect_error(plot(fit_c, type = "residuals", component = "slope"),
               "requires type = 'trend'")
  expect_error(hindcast(fit_c, type = "trend", component = "slope",
                        process_error = TRUE),
               "requires 'process_error = FALSE'")
})


test_that("the seasonal states follow the rotation of each harmonic", {
  # The program sums rotated shocks in closed form. The reference steps
  # each pair one turn at a time.
  dm <- as_draws_matrix(fit_f$fit)
  lam <- 2 * pi * (1:2) / 12
  worst <- 0
  for (r in round(seq(1, nrow(dm), length.out = 6))) {
    sig <- as.numeric(dm[r, sprintf("sigma_season_trend[%d]", 1:2)])
    init <- block(dm, r, "init_season_trend", 4L, 2L)
    shocks <- block(dm, r, "innovations_season_trend", 95L, 8L)
    g <- init[1:2, ]
    g_star <- init[3:4, ]
    season <- matrix(0, 96L, 2L)
    season[1L, ] <- colSums(g)
    for (t in 2:96) {
      w <- matrix(shocks[t - 1L, 1:4], 2L) * rep(sig, each = 2L)
      w_star <- matrix(shocks[t - 1L, 5:8], 2L) * rep(sig, each = 2L)
      g_last <- g
      g <- cos(lam) * g_last + sin(lam) * g_star + w
      g_star <- cos(lam) * g_star - sin(lam) * g_last + w_star
      season[t, ] <- colSums(g)
    }
    worst <- max(worst,
                 abs(season - block(dm, r, "season_trend", 96L, 2L)),
                 abs(rbind(g, g_star) -
                       block(dm, r, "season_state_trend", 4L, 2L)))
  }
  expect_lt(worst, 1e-5)
  for (k in 1:2) {
    q <- quantile(as.numeric(dm[, sprintf("sigma_season_trend[%d]", k)]),
                  c(0.025, 0.975))
    expect_true(q[[1L]] < 0.02 && 0.02 < q[[2L]])
  }
})


test_that("the level and the season partition the seasonal trend", {
  ids <- 1:40
  dm <- as_draws_matrix(fit_f$fit)[ids, ]
  parts <- lapply(stats::setNames(nm = c("level", "season")), function(cmp) {
    hindcast(fit_f, type = "trend", component = cmp,
             draw_ids = ids)$hindcasts
  })
  total <- hindcast(fit_f, type = "trend", draw_ids = ids)$hindcasts
  for (k in 1:2) {
    cols <- sprintf("season_trend[%d,%d]", 1:96, k)
    expect_equal(unname(parts$season[[k]]),
                 matrix(as.numeric(dm[, cols]), nrow = length(ids)))
    expect_equal(parts$level[[k]] + parts$season[[k]], total[[k]])
  }
})


test_that("the season's forecast turns its last pairs forward", {
  # Lead h turns each pair by h lambda_j. Each step adds shocks of
  # variance sigma^2 to both states. A rotation keeps that variance.
  # The season's variance at lead h is then K h sigma^2.
  dm <- as_draws_matrix(fit_f$fit)
  lam <- 2 * pi * (1:2) / 12
  fc <- forecast(fit_f, newdata = future_frame(train_f, 12L), type = "trend",
                 component = "season", draw_ids = seq_len(nrow(dm)))
  for (k in 1:2) {
    pairs <- function(row) {
      sapply(1:2, function(j) {
        as.numeric(dm[, sprintf("season_state_trend[%d,%d]", row + j - 1L, k)])
      })
    }
    g <- pairs(1L)
    g_star <- pairs(3L)
    sig <- as.numeric(dm[, sprintf("sigma_season_trend[%d]", k)])
    for (h in c(1L, 5L, 12L)) {
      mu <- g %*% cos(h * lam) + g_star %*% sin(h * lam)
      z <- (fc$forecasts[[k]][, h] - mu) / sqrt(2 * h * sig^2)
      expect_lt(abs(mean(z)), 5 / sqrt(length(z)))
      expect_lt(abs(stats::sd(z) - 1), 0.08)
    }
  }
  p <- plot(fit_f, type = "trend", component = "season")
  expect_identical(p$labels$y, "Latent season")
  expect_true("delivera_trig" %in% names(how_to_cite(fit_f)$bibtex))
})


test_that("the write-up and citations name the local linear trend", {
  cite <- how_to_cite(fit_a)
  expect_match(paste(format(cite), collapse = "\n"), "Harvey", fixed = TRUE)
  expect_match(paste(format(cite), collapse = "\n"), "Gardner", fixed = TRUE)
  md <- paste(methods_md(fit_a), collapse = "\n")
  expect_match(md, "\\beta_{i,t}", fixed = TRUE)
})

cat("\nDone.\n")
