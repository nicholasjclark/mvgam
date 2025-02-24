# Setup models for tests locally
library("testthat")
library("mvgam")
set.seed(123)

expect_match2 <- function(object, regexp) {
  any(grepl(regexp, object, fixed = TRUE))
}

expect_range <- function(object, lower = -Inf, upper = Inf, ...) {
  testthat::expect_true(all(object >= lower & object <= upper), ...)
}

expect_ggplot <- function(object, ...) {
  testthat::expect_true(is(object, "ggplot"), ...)
}


SM <- suppressMessages
SW <- suppressWarnings

context("local tests")
