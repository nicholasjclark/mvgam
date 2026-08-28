# `update()` on a fit whose trend constructor took an argument from a
# variable.
#
# A formula stores the expression and the environment it was written
# in, not the values it names. `~ AR(p = 1, trend_map = Z)` written
# against a local `Z`, then saved and read back in a fresh session,
# names something no longer in scope, so rebuilding the call for a
# refit failed with `object 'Z' not found`. The `recompile = FALSE`
# guard runs the same rebuild, so a user could not even ask whether a
# refit needed recompiling.
#
# The values are on the fit already, under `trend_metadata`. These
# fixtures are the two that carry such an argument: `trend_map_fx`
# passes a loading matrix and `loadings_prior` a factor count.
#
# Run with:
#   Rscript -e "devtools::load_all('.'); testthat::test_file('tests/local/test-update-trend-args.R')"

source("setup_tests_local.R")

fixture <- function(name) {
  readRDS(file.path("fixtures", paste0(name, ".rds")))
}


test_that("a trend call naming a variable rebuilds for a refit", {
  # Whether a given fixture still holds an out-of-scope name depends
  # on how its builder captured the argument, so the mechanism is unit
  # tested in tests/testthat/test-update.R and what is asserted here
  # is the contract: the call rebuilds, whatever it names.
  for (name in c("val_mvgam_trend_map_fx", "val_mvgam_loadings_prior")) {
    fit <- fixture(name)
    expect_true(length(all.vars(fit$trend_call)) > 0L)
    call_args <- mvgam:::mvgam_update_call(fit, NULL, NULL, list())
    code <- as.character(mvgam:::mvgam_dry_stancode(call_args))
    expect_true(any(grepl("trend", code)))
  }
})


test_that("the rebuilt code matches what the fit was built from", {
  # Putting the value back must reproduce the model, not merely get
  # past the error: the same specification has to emit the same Stan.
  for (name in c("val_mvgam_trend_map_fx", "val_mvgam_loadings_prior")) {
    fit <- fixture(name)
    call_args <- mvgam:::mvgam_update_call(fit, NULL, NULL, list())
    rebuilt <- mvgam:::mvgam_normalise_stancode(
      mvgam:::mvgam_dry_stancode(call_args)
    )
    stored <- mvgam:::mvgam_normalise_stancode(fit$stancode)
    expect_equal(rebuilt, stored)
  }
})


test_that("update(recompile = FALSE) is answerable on such a fit", {
  # The guard compares rebuilt against stored stancode, so it could
  # not run at all while the rebuild errored.
  fit <- fixture("val_mvgam_trend_map_fx")
  refit <- suppressWarnings(suppressMessages(
    update(fit, recompile = FALSE, iter = 200, warmup = 100,
            chains = 1, silent = 2, refresh = 0)
  ))
  expect_s3_class(refit, "mvgam")
})
