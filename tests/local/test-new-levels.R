# Prediction for a grouping level the model was not fitted to.
#
# brms refuses such a level and tells the caller to set
# `allow_new_levels = TRUE`. Doing so would give `subscript out of
# bounds`: brms extends the grouping index to cover the new level,
# while the posterior holds a coefficient only for each level the
# model saw, so the index runs past the end of the draws. Drawing a
# coefficient for a level the model never saw is brms's own
# `get_new_rdraws()`, which mvgam does not reproduce, so the
# limitation is named instead.
#
# Needs a fit carrying a random effect. `population_random_pred()`'s
# guard is unit tested without one in
# tests/testthat/test-posterior-linpred.R.
#
# Run with:
#   Rscript -e "devtools::load_all('.'); testthat::test_file('tests/local/test-new-levels.R')"

source("setup_tests_local.R")

fixture <- file.path("fixtures", "val_mvgam_ar1_re.rds")
fit <- readRDS(fixture)
training <- fit$data
unseen <- training
unseen$grp <- factor("unseen_level")


test_that("a known grouping level predicts on every surface", {
  # The guard must not narrow what already worked.
  expect_true(is.matrix(posterior_epred(fit, newdata = training,
                                         ndraws = 20)))
  expect_true(is.matrix(posterior_predict(fit, newdata = training,
                                           ndraws = 20)))
  expect_true(is.matrix(posterior_linpred(fit, newdata = training,
                                           ndraws = 20)))
  # A subset of the fitted levels is still a known level set.
  subset_levels <- training[training$grp %in% c("a", "b"), , drop = FALSE]
  expect_equal(
    ncol(posterior_epred(fit, newdata = subset_levels, ndraws = 20)),
    nrow(subset_levels)
  )
})


test_that("an unseen level is refused by name on every surface", {
  for (method in list(posterior_epred, posterior_predict,
                       posterior_linpred)) {
    expect_error(
      method(fit, newdata = unseen, allow_new_levels = TRUE,
             ndraws = 20),
      "grouping level the model never saw"
    )
  }
  # The message names the factor, both counts and the way out.
  msg <- tryCatch(
    posterior_epred(fit, newdata = unseen, allow_new_levels = TRUE,
                    ndraws = 20),
    error = function(e) conditionMessage(e)
  )
  expect_true(grepl("grp", msg))
  expect_true(grepl("re_formula = NA", msg))
  # Left at its default, brms refuses first and says how to proceed.
  expect_error(
    posterior_epred(fit, newdata = unseen, ndraws = 20),
    "cannot be found in the fitted model"
  )
})


test_that("every documented sample_new_levels value reaches the guard", {
  # All three are brms's `snl_options` and mvgam's entry points offer
  # all three. Two validators underneath accepted only the first two,
  # so `old_levels` failed on an assertion raised from inside rather
  # than on the real limitation.
  for (snl in c("uncertainty", "gaussian", "old_levels")) {
    expect_error(
      posterior_epred(fit, newdata = unseen, allow_new_levels = TRUE,
                      sample_new_levels = snl, ndraws = 20),
      "grouping level the model never saw"
    )
  }
})


test_that("re_formula = NA predicts an unseen level from the population", {
  # The way out the error recommends has to work.
  out <- posterior_epred(fit, newdata = unseen, re_formula = NA,
                          ndraws = 20)
  expect_equal(dim(out), c(20L, nrow(unseen)))
  expect_true(all(is.finite(out)))
  # Dropping the group term makes the level irrelevant, so an unseen
  # level and a known one give the same answer for the same rows.
  known <- posterior_epred(fit, newdata = training, re_formula = NA,
                            ndraws = 20)
  expect_equal(dim(known), dim(out))
})
