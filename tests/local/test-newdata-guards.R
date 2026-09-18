# What a prediction frame has to carry.
#
# A prediction places every row on the trend's two axes. An occasion
# has no substitute: a frame omitting its time column was handed
# predictions at whatever position the observation structure fell back
# to, of the right shape and finite throughout, and indistinguishable
# from a correct result. The guard that owns this is
# `validate_newdata_complete()`.
#
# The series axis is the half that sometimes has a substitute, and
# `R/validations.R` states the rule: the column is demanded where the
# axis source is `explicit`, the fit has several series, and no
# grouping derives them. Where the fit has one series, every row
# belongs to it and the column adds nothing. Both cases are asserted
# here, on a fit of each shape.
#
# This file fits its own models and caches them under `fixtures`. It
# depends on no shared fixture and no build step.
#
# Run with:
#   testthat::test_file("tests/local/test-newdata-guards.R")

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
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

set.seed(419L)

n_time <- 30L
series_levels <- c("delta", "bravo")

make_frame <- function(levels) {
  d <- data.frame(
    time = rep(seq_len(n_time), times = length(levels)),
    series = factor(rep(levels, each = n_time), levels = levels),
    x = rnorm(n_time * length(levels))
  )
  d$y <- rpois(nrow(d), exp(1.2 + 0.3 * d$x))
  d
}

dat_multi <- make_frame(series_levels)
dat_single <- make_frame(series_levels[1L])

fit_cached <- function(name, data) {
  path <- cache_path(name)
  if (file.exists(path)) {
    cat("[cache] Loading", name, "\n")
    return(readRDS(path))
  }
  cat("[fit ] mvgam(AR(p = 1)) for", name, "\n")
  f <- mvgam(
    formula = y ~ x, trend_formula = ~ AR(p = 1),
    data = data, family = poisson(),
    chains = 2L, iter = 1000L, warmup = 500L,
    silent = 2, backend = "cmdstanr"
  )
  saveRDS(f, path)
  f
}

fit <- fit_cached("val_newdata_guards_multi.rds", dat_multi)
fit_one <- fit_cached("val_newdata_guards_single.rds", dat_single)

time_var <- mvgam:::axis_vars(fit)$time_var
series_var <- mvgam:::axis_vars(fit)$series_var
train <- mvgam:::mvgam_training_data(fit)


test_that("a complete frame predicts", {
  ep <- posterior_epred(fit, newdata = train)
  expect_true(is.matrix(ep))
  expect_identical(ncol(ep), nrow(train))
  expect_false(anyNA(ep))
})


test_that("a frame omitting the time column is refused", {
  nd <- train
  nd[[time_var]] <- NULL
  expect_error(
    posterior_epred(fit, newdata = nd),
    "Absent"
  )
  # The refusal names the column the frame left out. The caller then
  # knows which one to supply.
  expect_error(posterior_epred(fit, newdata = nd), time_var)
})


test_that("a missing time value is refused and its row named", {
  nd <- train
  nd[[time_var]][3L] <- NA
  expect_error(
    posterior_epred(fit, newdata = nd),
    "first at row 3"
  )

  all_na <- train
  all_na[[time_var]] <- NA
  expect_error(posterior_epred(fit, newdata = all_na), time_var)
})


test_that("a multi-series frame omitting the series column is refused", {
  # Several series, named by the column itself, with no grouping to
  # derive them. A row's series comes from that column alone, which
  # makes its absence the same gap as an absent occasion.
  nd <- train
  nd[[series_var]] <- NULL
  expect_error(posterior_epred(fit, newdata = nd), "Absent")
  expect_error(posterior_epred(fit, newdata = nd), series_var)
})


test_that("a single-series frame predicts when the column is dropped", {
  # The case where the axis has a substitute. One series means every
  # row belongs to it, and the record places rows the frame never
  # keyed.
  train_one <- mvgam:::mvgam_training_data(fit_one)
  nd <- train_one
  nd[[mvgam:::axis_vars(fit_one)$series_var]] <- NULL
  ep <- posterior_epred(fit_one, newdata = nd)
  expect_identical(ncol(ep), nrow(train_one))
  expect_false(anyNA(ep))
})


test_that("a series level absent from the fit is refused", {
  nd <- train
  nd[[series_var]] <- as.character(nd[[series_var]])
  nd[[series_var]][1L] <- "ghost"
  expect_error(posterior_epred(fit, newdata = nd), "ghost")
})
