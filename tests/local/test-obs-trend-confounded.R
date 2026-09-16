# What the observation and trend designs share, and what the sharing
# costs.
#
# `y ~ 1` with `~ series + AR(p = 1)` is the plain way to ask for a
# per-series latent level. The observation intercept and the series
# columns of the trend design span one direction twice: stacked, the
# two designs hold four columns of rank three. mvgam names that
# overlap and continues, and this file is the evidence for a notice
# in place of a refusal. The model samples, every fitted value is
# sound, and one coefficient alone carries no meaning.
#
# The remedy the notice names is dropping the repeated term from one
# formula. Writing `y ~ -1` is a different remedy and not one this
# file can use: an empty observation formula reaches Stan as the
# placeholder column `.mvgam_empty_obs`, which the trend's series
# columns span in turn.
#
# Series are named out of alphabetical order. A rank never equals a
# value there, and no rival resolver agrees by accident.
#
# Cached at tests/local/fixtures/val_mvgam_confound_conf.rds. Delete
# to refit.
#
# Run with:
#   testthat::test_file("tests/local/test-obs-trend-confounded.R")

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(posterior)
  library(testthat)
})

cache_path <- function(name) {
  dir <- if (dir.exists(file.path("tests", "local"))) {
    file.path("tests", "local", "fixtures")
  } else {
    "fixtures"
  }
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(dir, name)
}

SM <- suppressMessages

set.seed(413L)

series_levels <- c("tern", "auk", "skua")
stopifnot(!identical(series_levels, sort(series_levels)))
n_time <- 40L

dat <- expand.grid(
  time = seq_len(n_time) + 4L,
  series = factor(series_levels, levels = series_levels),
  stringsAsFactors = TRUE
)

# One AR(1) path per series, each around its own level. The level is
# what the two designs both claim.
level_true <- c(tern = 1.4, auk = -0.6, skua = 0.3)
ar_true <- 0.5
sigma_true <- 0.35
dat$y <- NA_real_
for (s in series_levels) {
  rows <- which(dat$series == s)
  state <- numeric(n_time)
  state[1L] <- stats::rnorm(1L, 0, sigma_true / sqrt(1 - ar_true^2))
  for (t in 2:n_time) {
    state[t] <- ar_true * state[t - 1L] + stats::rnorm(1L, 0, sigma_true)
  }
  dat$y[rows] <- level_true[[s]] + state + stats::rnorm(n_time, 0, 0.2)
}

path <- cache_path("val_mvgam_confound_conf.rds")
conf <- if (file.exists(path)) {
  message("[cache] Loading confounded fit.")
  readRDS(path)
} else {
  fit <- SM(mvgam(
    y ~ 1,
    trend_formula = ~ series + AR(p = 1),
    data = dat,
    family = gaussian(),
    chains = 2L,
    cores = 1L,
    silent = 2L
  ))
  saveRDS(fit, path)
  fit
}

# The user-facing projection, where the trend's series coefficients
# carry the names the axis gives them. The raw Stan draws name them
# `b_trend[k]`.
draws <- as.data.frame(conf)
series_betas <- grep("^b_series.*_trend$", names(draws), value = TRUE)


test_that("the rank check names the pairing this file fits", {
  m <- mvgam:::stacked_design_matrix(standata(conf))
  expect_false(is.null(m))
  norms <- sqrt(colSums(m^2))
  norms[norms == 0] <- 1
  q <- qr(sweep(m, 2L, norms, "/"))
  expect_lt(q$rank, ncol(m))
  dependent <- colnames(m)[q$pivot[seq.int(q$rank + 1L, ncol(m))]]
  expect_match(dependent, "series")
})


test_that("the intercept and a series level lie on one ridge", {
  # An observation intercept and a per-series trend level move the
  # fitted values together. The posterior shows a near perfect
  # correlation between them.
  expect_length(series_betas, 3L)
  worst <- max(vapply(series_betas, function(nm) {
    abs(stats::cor(draws$b_Intercept, draws[[nm]]))
  }, numeric(1L)))
  expect_gt(worst, 0.8)
})


test_that("the data pins the sum and the priors pin the split", {
  # The mechanism producing the ridge, stated on the posterior
  # itself. Each coefficient alone wanders across a wide range. Their
  # sum is the quantity the likelihood constrains, and it is far
  # tighter.
  spread_alone <- stats::sd(draws$b_Intercept)
  spread_sum <- min(vapply(series_betas, function(nm) {
    stats::sd(draws$b_Intercept + draws[[nm]])
  }, numeric(1L)))
  expect_lt(spread_sum, spread_alone / 2)
})


test_that("the fit itself is sound", {
  # The notice continues, and this is the reason: the model samples
  # and its fitted values are usable. What a caller loses is one
  # coefficient's meaning on its own.
  ep <- posterior_epred(conf, ndraws = 100L)
  expect_false(anyNA(ep))
  expect_identical(ncol(ep), nrow(dat))
})
