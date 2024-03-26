# Setup models for tests locally
library("testthat")
library("mvgam")
set.seed(100)

expect_match2 <- function(object, regexp) {
  any(grepl(regexp, object, fixed = TRUE))
}

context("local tests")

