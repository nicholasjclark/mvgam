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

# Build both Stan code and Stan data from one
# `generate_stan_components_mvgam_formula()` call. Tests that need
# both surfaces should use this instead of calling `stancode()` and
# `standata()` separately, since each public dispatcher re-runs the
# full pipeline (and triggers a fresh V8 isolate for the Stan code
# polish step) on its own.
mvgam_stan_setup <- function(formula, data, family = gaussian(), ...) {
  cc <- mvgam:::generate_stan_components_mvgam_formula(
    formula = formula, data = data, family = family, ...
  )
  code <- cc$combined_components$stancode
  class(code) <- c("mvgamstancode", "stancode", "character")
  list(code = code, data = cc$combined_components$standata)
}
