# `run_model = FALSE` is a supported mode: it returns a prefit so a
# user can read `stancode()` and `standata()` before committing to a
# sampling run. The object carries its own `mvgam_prefit` class and an
# empty `$fit`.
#
# Asking such an object for anything that needs a posterior has to be
# refused, and the package already knows how. `summary()` says:
#
#   No fitted model found in mvgam object.
#   summary() requires a fitted Stan model and an unfitted stub was
#   supplied (`run_model = FALSE`).
#   Use `stancode()` ...
#
# That message names the state, names the argument that produced it
# and points at what does work. It is the standard every other post-fit
# method is held to here, because a user who reaches one of them is in
# exactly the same situation and needs the same three things.
#
# The file needs no sampling, so it is cheap to run.
#
# Run with:
#   testthat::test_file("tests/local/test-prefit-guard.R")

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(testthat)
})

set.seed(414L)

dat <- data.frame(
  time = rep(seq_len(20L), 2L),
  series = factor(rep(c("a", "b"), each = 20L)),
  x = rnorm(40L)
)
dat$y <- rpois(40L, 3L)

prefit <- mvgam(
  formula = y ~ x, trend_formula = ~ AR(),
  data = dat, family = poisson(), run_model = FALSE, silent = 2
)


test_that("a prefit is a distinct object carrying no posterior", {
  expect_s3_class(prefit, "mvgam_prefit")
  expect_s3_class(prefit, "mvgam")
  expect_null(prefit$fit)
})


test_that("the two methods a prefit exists for still work", {
  # These are the whole point of `run_model = FALSE`, so they answer
  # rather than refuse. If this ever fails the mode has no purpose.
  expect_true(nzchar(as.character(stancode(prefit))))
  sd <- standata(prefit)
  expect_true(is.list(sd))
  expect_true(length(sd) > 0L)
})


test_that("summary refuses a prefit and says why", {
  # The reference message. Everything below is held to this.
  err <- expect_error(summary(prefit), "No fitted model found")
  msg <- conditionMessage(err)
  expect_match(msg, "run_model", fixed = TRUE)
  expect_match(msg, "stancode", fixed = TRUE)
})


test_that("every method needing a posterior refuses a prefit clearly", {
  # One guard, stated once, for a condition the object knows about
  # itself. What a user meets instead on all but `summary()` is
  # `posterior::as_draws_matrix()` failing to convert the empty `$fit`
  # slot:
  #
  #   Don't know how to transform an object of class 'NULL' to any
  #   supported draws format.
  #
  # That names an internal conversion rather than the situation, says
  # nothing about `run_model = FALSE`, and gives no way forward. It is
  # also identical for every one of these calls, so a user cannot tell
  # which requirement they missed.
  needs_posterior <- list(
    posterior_epred = function(x) posterior_epred(x, ndraws = 2L),
    posterior_predict = function(x) posterior_predict(x, ndraws = 2L),
    posterior_linpred = function(x) posterior_linpred(x, ndraws = 2L),
    predict = function(x) predict(x, ndraws = 2L),
    fitted = function(x) fitted(x, ndraws = 2L),
    residuals = function(x) residuals(x, ndraws = 2L),
    log_lik = function(x) log_lik(x, ndraws = 2L),
    hindcast = function(x) hindcast(x, ndraws = 2L),
    forecast = function(x) forecast(x, ndraws = 2L),
    loo = function(x) loo(x),
    variables = function(x) variables(x),
    tidy = function(x) tidy(x),
    augment = function(x) augment(x),
    plot = function(x) plot(x, type = "trend"),
    pp_check = function(x) pp_check(x, ndraws = 2L),
    mcmc_plot = function(x) mcmc_plot(x),
    conditional_effects = function(x) conditional_effects(x)
  )

  # Collected rather than asserted one at a time, so a single failure
  # names every method that falls short instead of stopping at
  # whichever comes first alphabetically.
  # `expect_error()` hands back the condition, so each call is both
  # the check that it refuses at all and the source of the message
  # the rest of the block judges.
  msgs <- vapply(needs_posterior, function(f) {
    err <- expect_error(f(prefit))
    if (is.null(err)) NA_character_ else conditionMessage(err)
  }, character(1L))

  # Held to the standard `summary()` already meets: name the state,
  # and name the argument that produced it.
  # Compared as one joined string so the failure prints the offending
  # method names rather than only how many there were.
  unclear <- names(msgs)[!grepl("fitted|run_model", msgs)]
  expect_identical(paste(unclear, collapse = ", "), "")

  # And not the internal draws conversion, which is what stands in
  # for a refusal today.
  internal <- names(msgs)[
    grepl("supported draws format", msgs, fixed = TRUE)
  ]
  expect_identical(paste(internal, collapse = ", "), "")
})

cat("\nDone.\n")
