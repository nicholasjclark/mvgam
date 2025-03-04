# Setup models for tests
library("testthat")
library("mvgam")

expect_match2 <- function(object, regexp) {
  any(grepl(regexp, object, fixed = TRUE))
}

expect_character <- function(object, ...) {
  testthat::expect_true(is(object, "character"), ...)
}

expect_list <- function(object, ...) {
  testthat::expect_true(is(object, "list"), ...)
}

expect_ggplot <- function(object, ...) {
  testthat::expect_true(is(object, "ggplot"), ...)
}

expect_loo <- function(object, ...) {
  testthat::expect_true(is(object, "psis_loo"), ...)
}

expect_range <- function(object, lower = -Inf, upper = Inf, ...) {
  testthat::expect_true(all(object >= lower & object <= upper), ...)
}

SM <- suppressMessages
SW <- suppressWarnings

set.seed(100)
beta_data <- sim_mvgam(
  family = betar(),
  trend_model = 'GP',
  trend_rel = 0.5,
  T = 60
)
gaus_data <- sim_mvgam(
  family = gaussian(),
  T = 60,
  trend_model = 'AR1',
  seasonality = 'shared',
  mu = c(-1, 0, 1),
  trend_rel = 0.5,
  prop_missing = 0.2
)
