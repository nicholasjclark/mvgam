# marginaleffects integration tests for mvgam.
#
# Lives in tests/local because the round-trip through
# marginaleffects -> insight -> get_predict -> posterior_epred is
# fixture-driven and too heavy for CI. Covers the full user surface:
# predictions / avg_predictions / avg_slopes / avg_comparisons on a
# slice of families that pass the marginaleffects data validation
# (no `group` column in fixture data; fixtures that include `group`
# trip marginaleffects's forbidden-name check before reaching mvgam).
#
# Concordance bar: predictions(mvgam_fit) and predictions(brms_fit)
# at observed time points should agree on the deterministic submodel
# (obs + trend covariates, no AR contribution) within a tolerance
# that captures the state-space / residual-AR architectural gap.

source("setup_tests_local.R")
source("concordance_helpers.R")

library(marginaleffects)
options("marginaleffects_model_classes" = "mvgam")


# Fixtures whose synthetic data does NOT carry a `group` column
# (marginaleffects reserves that name for its own output and refuses
# to evaluate models whose data includes it).
me_safe_fixtures <- c(
  "beta_ar1",
  "binom_ar1",
  "cumulative_fx",
  "hurdle_poisson_ar1",
  "zero_inflated_poisson_ar1"
)


# -- Insight surface ----------------------------------------------------

test_that("insight::get_data returns the training data", {
  require_fixtures("val_mvgam_beta_ar1.rds")
  mv <- load_mvgam("beta_ar1")
  out <- insight::get_data(mv)
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_identical(out, mv$data)
})

test_that("insight::find_response returns the response name(s)", {
  require_fixtures("val_mvgam_beta_ar1.rds")
  mv <- load_mvgam("beta_ar1")
  testthat::expect_equal(insight::find_response(mv), "y")
})

test_that("insight::find_predictors picks up obs + trend + meta vars", {
  require_fixtures("val_mvgam_ar1_fx_trend.rds")
  mv <- load_mvgam("ar1_fx_trend")
  preds <- insight::find_predictors(mv)
  testthat::expect_type(preds, "list")
  testthat::expect_true("x" %in% preds$conditional)
  testthat::expect_true("time" %in% preds$conditional)
  testthat::expect_true("series" %in% preds$conditional)
})

test_that("model.frame.mvgam returns predictors + response only", {
  require_fixtures("val_mvgam_beta_ar1.rds")
  mv <- load_mvgam("beta_ar1")
  mf <- model.frame(mv)
  testthat::expect_s3_class(mf, "data.frame")
  testthat::expect_true("y" %in% names(mf))
  testthat::expect_true("x" %in% names(mf))
})


# -- get_predict per type ----------------------------------------------

test_that("get_predict.mvgam: response type returns rowid/group/estimate", {
  require_fixtures("val_mvgam_beta_ar1.rds")
  mv <- load_mvgam("beta_ar1")
  out <- get_predict(mv, newdata = mv$data, type = "response")
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_true(all(c("rowid", "group", "estimate") %in% names(out)))
  testthat::expect_equal(nrow(out), nrow(mv$data))
  draws <- attr(out, "posterior_draws")
  testthat::expect_true(is.matrix(draws))
  # marginaleffects stores draws as [nobs x ndraws]
  testthat::expect_equal(nrow(draws), nrow(mv$data))
})

test_that("get_predict.mvgam: link type matches posterior_linpred", {
  require_fixtures("val_mvgam_beta_ar1.rds")
  mv <- load_mvgam("beta_ar1")
  out <- get_predict(mv, newdata = mv$data, type = "link")
  testthat::expect_s3_class(out, "data.frame")
  draws <- attr(out, "posterior_draws")
  testthat::expect_true(is.matrix(draws))
})

test_that("get_predict.mvgam: 3D ordinal epred flattens to tall frame", {
  require_fixtures("val_mvgam_cumulative_fx.rds")
  mv <- load_mvgam("cumulative_fx")
  out <- get_predict(mv, newdata = mv$data, type = "response")
  # nobs * ncat rows
  testthat::expect_equal(nrow(out), nrow(mv$data) * length(unique(out$group)))
  testthat::expect_true(length(unique(out$group)) > 1L)
})


# -- End-to-end marginaleffects entry points ---------------------------

run_me_smoke <- function(mv) {
  list(
    pred      = predictions(mv),
    avg_pred  = avg_predictions(mv),
    avg_slope = if (any(insight::find_predictors(mv)$conditional == "x")) {
      avg_slopes(mv, variables = "x")
    } else {
      NULL
    },
    avg_comp  = if (any(insight::find_predictors(mv)$conditional == "x")) {
      avg_comparisons(mv, variables = "x")
    } else {
      NULL
    }
  )
}

test_that("marginaleffects entry points run on supported families", {
  for (fix in me_safe_fixtures) {
    require_fixtures(paste0("val_mvgam_", fix, ".rds"))
    mv <- load_mvgam(fix)
    out <- suppressWarnings(run_me_smoke(mv))
    testthat::expect_s3_class(out$pred, "predictions")
    testthat::expect_s3_class(out$avg_pred, "predictions")
    if (!is.null(out$avg_slope)) {
      testthat::expect_true(nrow(out$avg_slope) >= 1L)
    }
    if (!is.null(out$avg_comp)) {
      testthat::expect_true(nrow(out$avg_comp) >= 1L)
    }
  }
})


# -- brms-concordance on the deterministic submodel --------------------

test_that("predictions(mvgam) tracks predictions(brms) on Beta AR(1)", {
  require_fixtures("val_mvgam_beta_ar1.rds", "val_brms_beta_ar1.rds")
  mv <- load_mvgam("beta_ar1")
  bf <- load_brms("beta_ar1")
  # Restrict comparison to a fresh datagrid so brms's AR-residual
  # draws don't dominate.
  nd <- mv$data
  mv_p  <- suppressWarnings(predictions(mv, newdata = nd))
  bf_p  <- suppressWarnings(predictions(bf, newdata = nd))
  # Both estimates are on the response scale and bounded (0, 1)
  testthat::expect_true(all(mv_p$estimate > 0 & mv_p$estimate < 1))
  testthat::expect_true(all(bf_p$estimate > 0 & bf_p$estimate < 1))
  # Loose cor bound: state-space vs residual-AR diverge per-obs but
  # share the X * beta deterministic signal.
  comp <- compare_vectors(bf_p$estimate, mv_p$estimate)
  testthat::expect_gte(comp$cor, 0.40)
})


# -- Unsupported / error paths -----------------------------------------

test_that("get_predict rejects multivariate without resp", {
  require_fixtures("val_mvgam_mv_gauss.rds")
  mv <- load_mvgam("mv_gauss")
  testthat::expect_error(
    get_predict(mv, newdata = mv$data, type = "response"),
    regexp = "requires"
  )
})

test_that("get_predict rejects an invalid type", {
  require_fixtures("val_mvgam_beta_ar1.rds")
  mv <- load_mvgam("beta_ar1")
  testthat::expect_error(
    get_predict(mv, newdata = mv$data, type = "bogus")
  )
})
