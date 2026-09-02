# Setup models for tests
library("testthat")
library("mvgam")

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


# The statement an emitter writes for one prior. Assertions build
# their expected text through the package's own writer, so a test
# cannot drift from the form the program actually carries. The
# writer itself is pinned directly in `test-stancode-standata.R`,
# and by the generated programs `stanc` compiles, so the agreement
# is not circular.
stan_prior_line <- function(param, dist, normalize = TRUE) {
  mvgam:::stan_prior_statement(param, dist, normalize = normalize)
}


# The prior an assembled program places on one parameter, read out
# of the program rather than matched against a spelling of it. A
# normalised program writes `target += dist_lpdf(x | args);` where
# an emitter wrote `x ~ dist(args);`, so a negative assertion built
# on the tilde passes whether or not the prior is there. Returns an
# empty vector when the parameter carries no prior, which is the
# answer for a brms-owned coefficient left at its flat default.
stan_prior_on <- function(code, param) {
  rows <- mvgam:::mvgam_stancode_prior_rows(
    paste(as.character(code), collapse = "\n")
  )
  keep <- vapply(
    rows, function(r) identical(as.character(r$class), param),
    logical(1L)
  )
  vapply(rows[keep], function(r) as.character(r$prior), character(1L))
}
