# End-to-end post-fit coverage for hierarchical trends.
#
# `gr` and `subgr` make mvgam derive the series identifier itself
# rather than read a column, and that derived value has to agree with
# the levels recorded at fit time. When it did not, every post-fit
# method on a hierarchical fit failed at the level validator while
# `summary()` kept working, so nothing in the suite noticed. These
# tests drive the whole surface against the cached fit.
#
# Run with:
#   testthat::test_file("tests/local/test-hierarchical-trends.R")

source("setup_tests_local.R")
source("concordance_helpers.R")


test_that("the fitted series levels match the trend metadata", {
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  vars <- fit$trend_metadata$variables
  derived <- levels(droplevels(mvgam:::hierarchical_series_values(
    fit$data, vars$gr_var, vars$subgr_var
  )))
  expect_setequal(derived, fit$trend_metadata$levels$series)
})


test_that("the prediction stack runs on a hierarchical fit", {
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  n_obs <- nrow(fit$data)

  expect_equal(dim(posterior_epred(fit, ndraws = 5L)), c(5L, n_obs))
  expect_equal(dim(posterior_predict(fit, ndraws = 5L)), c(5L, n_obs))
  expect_equal(dim(posterior_linpred(fit, ndraws = 5L)), c(5L, n_obs))
  expect_equal(dim(log_lik(fit, ndraws = 5L)), c(5L, n_obs))
  expect_equal(nrow(predict(fit, ndraws = 5L)), n_obs)
  expect_equal(nrow(fitted(fit, ndraws = 5L)), n_obs)
  expect_equal(nrow(residuals(fit, ndraws = 5L)), n_obs)
})


test_that("feeding the training data back as newdata is a no-op", {
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  # Compared on the deterministic path. With `process_error = TRUE`
  # the expectation is marginalised over the trend by drawing fresh
  # innovations, so two calls differ by construction.
  bare <- posterior_epred(fit, draw_ids = 1:5, process_error = FALSE)
  with_nd <- posterior_epred(
    fit, newdata = fit$data, draw_ids = 1:5, process_error = FALSE
  )
  expect_equal(dim(bare), dim(with_nd))
  expect_equal(bare, with_nd)

  lp <- posterior_linpred(fit, draw_ids = 1:5, process_error = FALSE)
  lp_nd <- posterior_linpred(
    fit, newdata = fit$data, draw_ids = 1:5, process_error = FALSE
  )
  expect_equal(lp, lp_nd)
})


test_that("the marginal expectation redraws innovations each call", {
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  # `process_error = TRUE` integrates over the trend by Monte Carlo,
  # so repeating the call on the same draws gives a different answer.
  # Reproducible output needs an explicit seed.
  a <- posterior_epred(fit, draw_ids = 1:5)
  b <- posterior_epred(fit, draw_ids = 1:5)
  expect_false(isTRUE(all.equal(a, b)))
})


test_that("the criticism surface runs on a hierarchical fit", {
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  ic <- SW(loo(fit))
  expect_s3_class(ic, "loo")
  expect_true(is.finite(ic$estimates["elpd_loo", "Estimate"]))
  expect_ggplot(SW(pp_check(fit, ndraws = 10L)))
  expect_ggplot(SW(pp_check(fit, type = "resid_qq", ndraws = 50L)))
  hc <- hindcast(fit, ndraws = 5L)
  expect_s3_class(hc, "mvgam_forecast")
  expect_equal(length(hc$hindcasts), fit$series_info$n_series)
})


test_that("a grouping combination absent from training is rejected", {
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  vars <- fit$trend_metadata$variables
  nd <- fit$data
  # Each level below is known on its own, but this pairing never
  # appeared, so the derived series identifier is new.
  nd[[vars$subgr_var]] <- "sp_unseen"
  expect_error(
    posterior_epred(fit, newdata = nd, ndraws = 5L),
    "Series levels in newdata not found in training data"
  )
})


test_that("the derived series identifier is stable and lexical", {
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  vars <- fit$trend_metadata$variables
  vals <- mvgam:::hierarchical_series_values(
    fit$data, vars$gr_var, vars$subgr_var
  )
  # Underscore-joined, grouping variable first, lexically ordered, so
  # the labels sort predictably in post-fit output.
  expect_true(all(grepl("_", levels(vals), fixed = TRUE)))
  expect_equal(levels(vals), sort(levels(vals)))
})


test_that("a superseded series column warns once, and obeys silent", {
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  vars <- fit$trend_metadata$variables
  derived <- mvgam:::hierarchical_series_values(
    fit$data, vars$gr_var, vars$subgr_var
  )
  # The warning is silent under TESTTHAT so the suite stays quiet;
  # clear it here to exercise the path a user actually meets.
  withr::local_envvar(TESTTHAT = "")

  count_warnings <- function() {
    n <- 0L
    withCallingHandlers(
      mvgam:::warn_series_superseded(
        fit$data, "series", derived, vars$gr_var, vars$subgr_var
      ),
      warning = function(w) {
        n <<- n + 1L
        invokeRestart("muffleWarning")
      }
    )
    n
  }

  # One notice per event: insight::format_warning() would raise its
  # own on top of the one rlang::warn() emits.
  withr::local_options(mvgam.silent = 0L)
  expect_equal(count_warnings(), 1L)

  rlang::reset_warning_verbosity("mvgam_series_superseded")
  withr::local_options(mvgam.silent = 2L)
  expect_equal(count_warnings(), 0L)
})


test_that("a series column matching the derived one is not flagged", {
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  vars <- fit$trend_metadata$variables
  derived <- mvgam:::hierarchical_series_values(
    fit$data, vars$gr_var, vars$subgr_var
  )
  dat <- fit$data
  dat$series <- derived
  withr::local_envvar(TESTTHAT = "")
  rlang::reset_warning_verbosity("mvgam_series_superseded")
  expect_silent(
    mvgam:::warn_series_superseded(
      dat, "series", derived, vars$gr_var, vars$subgr_var
    )
  )
})
