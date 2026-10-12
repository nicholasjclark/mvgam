# Post-fit coverage for a factor VAR, fitted in this file.
#
# The other VAR fixtures give the transition matrix the series axis,
# where `n_lv` equals `n_series`. Here four series load two latent
# factors and the VAR runs on the factors. The observations, the
# dynamics and the loadings then take separate axes. This file checks
# that each post-fit surface keys on the axis it describes:
# `irf()`, `fevd()` and `posterior_transition_matrix()` on the
# factors, `residual_cor()`, `hindcast()` and `forecast()` on the
# series. Processes no series names take index labels.
#
# `Z` is a sampled parameter. The tests check
# `trend[t, s] = Z[s, ] lv_trend[t, ]` on the draws.
#
#   truth: 4 series on 100 occasions loading 2 latent factors that
#          follow a correlated VAR(1), gaussian, with `y ~ x`
#   model: y ~ x, trend_formula = ~ -1 + VAR(cor = TRUE, n_lv = 2)
#
# `y ~ x` carries an intercept. A trend intercept added to it would be
# unidentified. The trend formula drops it with `-1`. `mu_trend` is
# then zero and the loadings identity holds exactly.
#
# Cached at tests/local/fixtures/val_mvgam_factor_var.rds. Delete to
# refit.
#
# Run with:
#   testthat::test_file("tests/local/test-factor-var.R")

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(posterior)
  library(testthat)
})

# This file fits its own model and caches it in the fixtures folder.

set.seed(2024L)

n_series <- 4L
n_time <- 100L
N_lv <- 2L
series_levels <- c("kestrel", "harrier", "merlin", "hobby")
time_vals <- seq_len(n_time) + 2L

# Asymmetric and stationary: a transposed `A` is a different model.
A_true <- matrix(c(0.60, 0.28,
                   0.00, 0.48), nrow = N_lv, byrow = TRUE)
stopifnot(max(Mod(eigen(A_true)$values)) < 1)

sigma_lv <- c(0.40, 0.35)
R_true <- matrix(c(1, 0.35, 0.35, 1), N_lv)
Sigma_true <- diag(sigma_lv) %*% R_true %*% diag(sigma_lv)
L_true <- chol(Sigma_true)

lv_true <- matrix(0, n_time, N_lv)
for (t in 2:n_time) {
  lv_true[t, ] <- as.numeric(A_true %*% lv_true[t - 1L, ]) +
    as.numeric(crossprod(L_true, rnorm(N_lv)))
}

# Four distinct rows of loadings give four distinct series trends.
Z_true <- matrix(c( 1.1,  0.1,
                    0.9, -0.3,
                    0.2,  1.2,
                   -0.4,  1.0),
                 nrow = n_series, ncol = N_lv, byrow = TRUE)
stopifnot(nrow(unique(Z_true)) == n_series)

trend_true <- lv_true %*% t(Z_true)
beta_x <- 0.45
x <- as.numeric(scale(rnorm(n_time)))

dat <- data.frame(
  time = rep(time_vals, times = n_series),
  series = factor(rep(series_levels, each = n_time),
                  levels = series_levels),
  x = rep(x, times = n_series)
)
dat$y <- as.numeric(trend_true) + beta_x * dat$x +
  rnorm(nrow(dat), 0, 0.25)

obs_formula <- y ~ x
trend_rhs <- ~ -1 + VAR(cor = TRUE, n_lv = 2)

sim_truth <- list(
  n_series = n_series, n_time = n_time, N_lv = N_lv,
  series_levels = series_levels, time_vals = time_vals,
  A_true = A_true, Sigma_true = Sigma_true, Z_true = Z_true,
  lv_true = lv_true, trend_true = trend_true, beta_x = beta_x, x = x
)

make_future <- function(h) {
  data.frame(
    time = rep(max(time_vals) + seq_len(h), times = n_series),
    series = factor(rep(series_levels, each = h),
                    levels = series_levels),
    x = rep(rnorm(h), times = n_series),
    y = NA_real_
  )
}


# -- Prefit: the VAR is sized by the factors -------------------------

prefit <- mvgam(
  formula = obs_formula, trend_formula = trend_rhs,
  data = dat, family = gaussian(), run_model = FALSE, silent = 2
)


test_that("the transition matrix is sized by the factor axis", {
  # Two factors give a 2 x 2 transition matrix. The series axis would
  # give 4 x 4.
  expect_identical(as.integer(prefit$standata$N_lv_trend), N_lv)
})


test_that("the loadings are sampled parameters", {
  # `trend_map` puts fixed loadings in the data block. `n_lv` declares
  # `Z` as a parameter.
  code <- paste(as.character(stancode(prefit)), collapse = "\n")
  expect_true(grepl("matrix[N_series_trend, N_lv_trend] Z;", code,
                    fixed = TRUE))
  expect_null(prefit$standata$Z)
  expect_null(prefit$standata$Z_template)
})


test_that("dropping the trend intercept removes it from the program", {
  # The loadings identity below requires `mu_trend` at zero. The `-1`
  # removes the trend intercept that would set it.
  code <- paste(as.character(stancode(prefit)), collapse = "\n")
  expect_false(grepl("Intercept_trend", code, fixed = TRUE))
})


test_that("every row maps to the trend cell its own labels name", {
  # The mapping arrays in the Stan data, checked against the frame
  sd <- prefit$standata
  expect_identical(as.integer(sd$obs_trend_time),
                   match(dat$time, sort(unique(dat$time))))
  expect_identical(as.integer(sd$obs_trend_series),
                   as.integer(dat$series))
})


# -- Fit --------------------------------------------------------------

fit <- cached_fit(
  "val_mvgam_factor_var.rds",
  function() {
    mvgam(
      formula = obs_formula, trend_formula = trend_rhs,
      data = dat, family = gaussian(),
      chains = 2L, iter = 2000L, warmup = 1000L,
      control = list(adapt_delta = 0.95, max_treedepth = 12),
      silent = 2, backend = "cmdstanr"
    )
  },
  key = sim_truth
)

dm <- posterior::as_draws_matrix(fit$fit)



test_that("the trend is the loadings times the factors, draw by draw", {
  # Both sides of `trend[t, s] = Z[s, ] lv_trend[t, ]` come from one
  # draw. A transposed `Z`, or factors indexed by series, breaks the
  # identity with every dimension intact.
  pull <- function(k, pat, nr, nc) {
    m <- matrix(NA_real_, nr, nc)
    for (a in seq_len(nr)) {
      for (b in seq_len(nc)) {
        m[a, b] <- dm[k, sprintf(pat, a, b)]
      }
    }
    m
  }
  for (k in unique(round(seq(1, nrow(dm), length.out = 5L)))) {
    lv <- pull(k, "lv_trend[%d,%d]", n_time, N_lv)
    tr <- pull(k, "trend[%d,%d]", n_time, n_series)
    Zk <- pull(k, "Z[%d,%d]", n_series, N_lv)
    # Stored draws carry six significant digits. Each defect above
    # moves entries by order one.
    expect_equal(unname(tr), unname(lv %*% t(Zk)), tolerance = 1e-6)
  }
})


test_that("a factor is labelled by its index", {
  # Two factors carry four series. A factor has no series name of its
  # own and takes its index as a label.
  expect_identical(var_process_labels(fit, N_lv), c("Process_1", "Process_2"))
})


test_that("VAR summaries key on the factors and the others on the series", {
  # `irf()` and `fevd()` describe the dynamics on the factor axis.
  # `residual_cor()` describes the series. A surface on the wrong axis
  # returns a square table with the wrong labels.
  procs <- c("Process_1", "Process_2")
  for (surface in list(irf(fit, h = 4L), fevd(fit, h = 4L))) {
    sides <- strsplit(unique(surface$shock), " -> ", fixed = TRUE)
    expect_setequal(unlist(sides), procs)
  }
  expect_identical(rownames(posterior_transition_matrix(fit)$A), procs)
  expect_identical(rownames(residual_cor(fit)$cor), series_levels)
})


test_that("two factors give the series correlation rank two", {
  # A factor model's residual correlation has rank `n_lv`
  rc <- residual_cor(fit)
  ev <- eigen(rc$cor, only.values = TRUE)$values
  expect_true(sum(ev > 1e-6) <= N_lv)
})


test_that("the factor summaries report both axes", {
  af <- active_factors(fit)
  # Every series loads on both columns of `Z`. Both factors are
  # active.
  expect_true(all(af$per_factor$is_active))
  sv <- shared_variation(fit)
  expect_identical(as.character(sv$series_names), series_levels)
})


test_that("the hindcast has one element per series", {
  # The dynamics run on two factors. The hindcast holds the four
  # series, each matching the conditional expectation of its rows.
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


test_that("the forecast steps in factor space and reaches four series", {
  # In each draw the first step of series k is `Z[k, ] Phi lv[T, ]`
  # plus the loadings times a factor innovation. Over every draw the
  # forecast mean equals the mean of that product within Monte Carlo
  # error. A step taken on the series axis, or with `Z` transposed,
  # lands elsewhere.
  ids <- seq_len(nrow(dm))
  set.seed(5L)
  fc <- forecast(fit, newdata = make_future(1L), draw_ids = ids,
                 type = "trend")
  pull <- function(k, pat, nr, nc) {
    matrix(as.numeric(dm[k, sprintf(pat, rep(seq_len(nr), nc),
                                     rep(seq_len(nc), each = nr))]), nr)
  }
  step <- t(vapply(ids, function(k) {
    as.numeric(pull(k, "Z[%d,%d]", n_series, N_lv) %*%
                 pull(k, "Phi_trend[1,%d,%d]", N_lv, N_lv) %*%
                 as.numeric(dm[k, sprintf("lv_trend[%d,%d]", n_time,
                                          seq_len(N_lv))]))
  }, numeric(n_series)))
  # Unit-scale factors: the innovation SD of series k is the norm of
  # its loadings
  innov_sd <- sqrt(colMeans(t(vapply(ids, function(k) {
    rowSums(pull(k, "Z[%d,%d]", n_series, N_lv)^2)
  }, numeric(n_series)))))
  for (k in seq_len(n_series)) {
    got <- mean(fc$forecasts[[series_levels[k]]][, 1L])
    expect_lt(abs(got - mean(step[, k])),
              4 * innov_sd[k] / sqrt(length(ids)))
  }
})


test_that("the forecast object is keyed, ordered and in user units", {
  h <- 4L
  future_times <- max(time_vals) + seq_len(h)
  fc <- forecast(fit, newdata = make_future(h), ndraws = 20L,
                 type = "link")
  expect_identical(as.character(fc$series_names), series_levels)
  for (s in series_levels) {
    expect_identical(as.integer(fc$test_times[[s]]), future_times)
    expect_identical(as.integer(fc$train_times[[s]]), time_vals)
  }
})


test_that("every plot draws with per-series panels in series order", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_s3_class(plot(irf(fit, h = 4L), series = 1L), "ggplot")
  expect_s3_class(plot(fevd(fit, h = 4L)), "ggplot")
  for (ty in c("trend", "series", "factors")) {
    expect_s3_class(plot(fit, type = ty), "ggplot")
  }
  expect_s3_class(pp_check(fit, ndraws = 20L), "ggplot")

  expect_identical(panel_order(plot(fit, type = "series")), series_levels)
  expect_identical(panel_order(plot(fit, type = "trend")), series_levels)
})


test_that("every plot draws the occasions the user supplied", {
  # The frame is numbered from three. An axis drawn on the rank runs
  # 1..100 where the user gave 3..102.
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  want <- as.numeric(range(time_vals))
  for (ty in c("trend", "series")) {
    expect_equal(drawn_x(plot(fit, type = ty)), want, tolerance = 0.02)
  }
})


cat("\nDone.\n")
