# Recovery and post-fit coverage for a factor VAR, fitted in this file.
#
# Every other VAR in this directory gives the transition matrix the
# series axis, where `n_lv` and `n_series` coincide and the two cannot
# be told apart. Here four series load two latent factors and the VAR runs
# on the factors, which separates three axes that are otherwise one:
# the series carrying the observations, the factors carrying the
# dynamics, and the loadings joining them.
#
# That separation is what makes two claims answerable here and nowhere
# else:
#
#   `irf()`, `fevd()` and `posterior_transition_matrix()` key on the
#   factor axis, carrying four shock pairs and not sixteen, while
#   `residual_cor()` and the forecast arms stay keyed by the series,
#   and
#
#   a process that no series names is labelled by its index, which is
#   the branch of the label resolver that a series-axis VAR never
#   reaches.
#
# `trend[t, s] = Z[s, ] . lv_trend[t, ]` is checked on values. Unlike
# `trend_map`, `Z` is a sampled parameter here. Both sides come out of
# the draws, and a transpose or a crossed axis breaks the identity
# while leaving every dimension right.
#
#   truth: 4 series on 100 occasions loading 2 latent factors that
#          follow a correlated VAR(1), gaussian, with `y ~ x`
#   model: y ~ x, trend_formula = ~ -1 + VAR(cor = TRUE, n_lv = 2)
#
# The trend intercept is dropped because `y ~ x` already carries one
# and the two sit on an exact ridge otherwise. With it gone, `mu_trend`
# is zero and the loadings identity above holds exactly.
#
# A factor model fixes its loadings only up to rotation. `A` and `Z`
# are not identified entry by entry and are not asserted that way. What
# the rotation leaves alone is asserted instead: the series-level
# trend, `Z Z'` and the observation coefficients.
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

# Four distinct rows: no two series read the factors alike and the
# series-level trends cannot coincide.
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
    x = rep(as.numeric(scale(rnorm(h))), times = n_series),
    y = NA_real_
  )
}


# -- Prefit: the VAR is sized by the factors, not the series ----------

prefit <- mvgam(
  formula = obs_formula, trend_formula = trend_rhs,
  data = dat, family = gaussian(), run_model = FALSE, silent = 2
)


test_that("the transition matrix is sized by the factor axis", {
  # The claim this file exists to make. A VAR given the series axis
  # carries sixteen coefficients here rather than four, fits at least
  # as well, and differs from this model in nothing else a shape check
  # would notice.
  sd <- prefit$standata
  expect_identical(as.integer(sd$N_lv_trend), N_lv)
  expect_identical(as.integer(sd$N_series_trend), n_series)
  expect_false(N_lv == n_series)

  code <- paste(as.character(stancode(prefit)), collapse = "\n")
  expect_true(grepl(
    "array[1] matrix[N_lv_trend, N_lv_trend] A_trend;",
    code, fixed = TRUE
  ))
  expect_true(grepl(
    "matrix[N_time_trend, N_lv_trend] lv_trend;", code, fixed = TRUE
  ))
  # The series reach the factors through `Z` and nowhere else.
  expect_true(grepl(
    "dot_product(Z[s,  : ], lv_trend[i,  : ])", code, fixed = TRUE
  ))
})


test_that("the loadings are sampled here, not supplied", {
  # `trend_map` is the other route to a factor model and puts `Z` in
  # the data block. Both spellings produce the same identity, and only
  # this one leaves the loadings to the posterior.
  code <- paste(as.character(stancode(prefit)), collapse = "\n")
  expect_true(grepl("matrix[N_series_trend, N_lv_trend] Z;", code,
                    fixed = TRUE))
  expect_null(prefit$standata$Z)
  expect_null(prefit$standata$Z_template)
})


test_that("dropping the trend intercept leaves none in the program", {
  # `y ~ x` carries an intercept and a trend intercept would sit on an
  # exact ridge with it. `-1` is what keeps `mu_trend` at zero, which
  # the loadings identity below depends on.
  code <- paste(as.character(stancode(prefit)), collapse = "\n")
  expect_false(grepl("Intercept_trend", code, fixed = TRUE))
})


test_that("every row maps to the trend cell its own labels name", {
  # The axis chain on the observation side, answered from the record
  # rather than from anything the prediction path derives.
  sd <- prefit$standata
  expect_identical(as.integer(sd$obs_trend_time),
                   match(dat$time, sort(unique(dat$time))))
  expect_identical(as.integer(sd$obs_trend_series),
                   as.integer(dat$series))
  expect_identical(dim(sd$times_trend), c(n_time, n_series))
})


# -- Fit --------------------------------------------------------------

cache <- cache_path("val_mvgam_factor_var.rds")
if (file.exists(cache)) {
  cat("[cache] Loading factor VAR fit.\n")
  fit <- readRDS(cache)
} else {
  cat("[fit ] mvgam(VAR(cor = TRUE, n_lv = 2), 4 series on 2 factors)\n")
  fit <- mvgam(
    formula = obs_formula, trend_formula = trend_rhs,
    data = dat, family = gaussian(),
    chains = 2L, iter = 2000L, warmup = 1000L,
    control = list(adapt_delta = 0.95, max_treedepth = 12),
    silent = 2, backend = "cmdstanr"
  )
}
if (!identical(attr(fit, "sim_truth"), sim_truth)) {
  attr(fit, "sim_truth") <- sim_truth
  saveRDS(fit, cache)
}

dm <- posterior::as_draws_matrix(fit$fit)


test_that("the trend is the loadings times the factors, draw by draw", {
  # `Z` is a parameter on this route, so both sides of
  # `trend[t, s] = Z[s, ] . lv_trend[t, ]` come out of the same draw
  # and the identity holds exactly or not at all. A transposed `Z`, a
  # column read as a row, or the factor axis indexed against the series
  # axis each keep every value finite and every dimension right, and
  # each breaks this.
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
    # The draws are stored to finite precision, so the identity is read
    # to the precision they carry. Every violation this exists for
    # moves entries by order one.
    expect_equal(unname(tr), unname(lv %*% t(Zk)), tolerance = 1e-6)
  }
})


test_that("the posterior holds one coefficient per factor pair", {
  # Four transition coefficients and not sixteen, two innovation
  # scales and not four. A model that gave every series its own process
  # differs from this one here and nowhere a shape check would look.
  expect_length(grep("^A_trend\\[", colnames(dm)), N_lv * N_lv)
  expect_length(grep("^sigma_trend", colnames(dm)), N_lv)
  expect_length(grep("^Z\\[", colnames(dm)), n_series * N_lv)
  expect_length(grep("^lv_trend\\[", colnames(dm)), n_time * N_lv)
  expect_length(grep("^trend\\[", colnames(dm)), n_time * n_series)
})


test_that("a process no series names is labelled by its index", {
  # The branch of the label resolver that a series-axis VAR cannot
  # reach. Two latent factors carry four series, so no series names a
  # process and the index is the only honest answer. Naming them for
  # the series would put four labels on two processes.
  expect_identical(as.integer(detect_factor_n_lv(fit)), N_lv)
  labs <- var_process_labels(fit, N_lv)
  expect_length(labs, N_lv)
  expect_identical(labs, c("Process_1", "Process_2"))
  expect_false(any(labs %in% series_levels))
})


test_that("the VAR surfaces key on the factors, the rest on the series", {
  # The two axes read side by side. `irf()` and `fevd()` describe the
  # dynamics, which live on the factor axis, so they carry four pairs.
  # `residual_cor()` describes the series, so it carries four names.
  # A surface that took the wrong axis returns a square table either
  # way and only the size and the labels say which.
  ir <- irf(fit, h = 4L)
  pairs <- unique(ir$shock)
  expect_length(pairs, N_lv * N_lv)
  sides <- strsplit(pairs, " -> ", fixed = TRUE)
  expect_setequal(unlist(sides), c("Process_1", "Process_2"))

  fe <- fevd(fit, h = 4L)
  expect_length(unique(fe$shock), N_lv * N_lv)

  ptm <- posterior_transition_matrix(fit)
  expect_identical(dim(ptm$A), c(N_lv, N_lv))
  expect_identical(rownames(ptm$A), c("Process_1", "Process_2"))
  expect_identical(as.character(ptm$series_names),
                   c("Process_1", "Process_2"))

  rc <- residual_cor(fit)
  expect_identical(rownames(rc$cor), series_levels)
  expect_identical(dim(rc$cor), c(n_series, n_series))
})


test_that("two factors behind four series leave the correlation short", {
  # A factor model's residual correlation is rank `n_lv` by
  # construction, so this is where the narrower axis shows up in a
  # series-shaped object.
  rc <- residual_cor(fit)
  ev <- eigen(rc$cor, only.values = TRUE)$values
  expect_true(sum(ev > 1e-6) <= N_lv)
})


test_that("the factor summaries report both axes", {
  af <- active_factors(fit)
  expect_identical(as.integer(af$n_lv), N_lv)
  expect_identical(nrow(af$per_factor), N_lv)
  # Every series loads on both columns of `Z`, so neither is redundant.
  expect_true(all(af$per_factor$is_active))

  sv <- shared_variation(fit)
  expect_identical(as.integer(sv$n_lv), N_lv)
  expect_identical(as.integer(sv$n_series), n_series)
  expect_identical(as.character(sv$series_names), series_levels)
})


test_that("the identified quantities recover the simulated truth", {
  # A factor model fixes its loadings only up to rotation, so `A` and
  # `Z` are not identified entry by entry and are not read that way.
  # The series-level trend is invariant to the rotation, and so is
  # `Z Z'`, so those are what the truth is asserted against.
  th <- matrix(NA_real_, n_time, n_series)
  for (t in seq_len(n_time)) {
    for (s in seq_len(n_series)) {
      th[t, s] <- mean(dm[, sprintf("trend[%d,%d]", t, s)])
    }
  }
  for (s in seq_len(n_series)) {
    expect_gt(cor(th[, s], trend_true[, s]), 0.85)
  }

  Zh <- matrix(NA_real_, n_series, N_lv)
  for (i in seq_len(n_series)) {
    for (j in seq_len(N_lv)) {
      Zh[i, j] <- mean(dm[, sprintf("Z[%d,%d]", i, j)])
    }
  }
  expect_gt(cor(as.numeric(Zh %*% t(Zh)),
                as.numeric(Z_true %*% t(Z_true))), 0.7)

  # The observation side carries no rotation and is held tightly.
  post <- posterior_summary(fit)
  expect_equal(unname(post["b_x", "Estimate"]), beta_x,
               tolerance = 0.05)
})


test_that("every prediction surface answers for every row", {
  n_obs <- nrow(dat)
  ep <- posterior_epred(fit, ndraws = 20L)
  pp <- posterior_predict(fit, ndraws = 20L)
  expect_identical(dim(ep), c(20L, n_obs))
  expect_identical(dim(pp), c(20L, n_obs))
  expect_true(all(is.finite(ep)))
  expect_identical(nrow(fitted(fit, ndraws = 20L)), n_obs)
  expect_identical(nrow(residuals(fit, ndraws = 20L)), n_obs)
})


test_that("the hindcast fans out over the series, not the factors", {
  # The dynamics run on two processes and a user receives four series.
  # A fan-out keyed on the factor axis returns two arms, each finite
  # and correctly shaped.
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
  h <- 5L
  fc <- forecast(fit, newdata = make_future(h), ndraws = 100L,
                 type = "trend")
  expect_identical(names(fc$forecasts), series_levels)
  for (s in series_levels) {
    expect_identical(dim(fc$forecasts[[s]])[2L], h)
    expect_true(all(is.finite(fc$forecasts[[s]])))
  }
  # Four series driven by two factors cannot move independently: the
  # loadings fix every series' step as one combination of two.
  first <- vapply(series_levels, function(s) {
    mean(fc$forecasts[[s]][, 1L])
  }, numeric(1))
  expect_length(unique(round(first, 8)), n_series)
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


test_that("every plot draws, and the per-series panels keep their order", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_s3_class(plot(irf(fit, h = 4L), series = 1L), "ggplot")
  expect_s3_class(plot(fevd(fit, h = 4L)), "ggplot")
  for (ty in c("trend", "series", "factors")) {
    expect_s3_class(plot(fit, type = ty), "ggplot")
  }
  expect_s3_class(pp_check(fit, ndraws = 20L), "ggplot")

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
})


test_that("every plot draws the occasions the user supplied", {
  # The frame is numbered from three, so an axis drawn on the rank runs
  # 1..100 where the user gave 3..102.
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  drawn_x <- function(p) {
    b <- ggplot2::ggplot_build(p)
    xs <- unlist(lapply(b$data, function(d) if ("x" %in% names(d)) d$x))
    range(xs, na.rm = TRUE)
  }
  want <- as.numeric(range(time_vals))
  for (ty in c("trend", "series")) {
    expect_equal(drawn_x(plot(fit, type = ty)), want, tolerance = 0.02)
  }
})


cat("\nDone.\n")
