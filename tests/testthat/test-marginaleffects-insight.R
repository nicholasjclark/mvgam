# Fast CI-safe tests for the insight + marginaleffects S3 surface.
# Signature parity, NAMESPACE wiring, validation paths — no fitted
# objects required.

test_that("insight S3 methods are registered on mvgam", {
  for (g in c("find_formula", "find_response", "find_predictors",
              "get_data", "model_info")) {
    expect_true(
      !is.null(getS3method(g, "mvgam", optional = TRUE)),
      label = paste("insight::", g, ".mvgam registered")
    )
  }
})

test_that("marginaleffects S3 methods are registered on mvgam", {
  for (g in c("get_predict", "get_coef", "get_vcov", "set_coef")) {
    expect_true(
      !is.null(getS3method(g, "mvgam", optional = TRUE)),
      label = paste("marginaleffects::", g, ".mvgam registered")
    )
  }
})

test_that(".onAttach sets marginaleffects_model_classes", {
  # `loadNamespace` does not always trigger .onAttach; check the source
  # directly so this test does not depend on load order.
  zzz <- readLines(system.file("..", "R", "zzz.R", package = "mvgam") %||%
                   "R/zzz.R")
  expect_true(any(grepl("marginaleffects_model_classes", zzz)))
})

test_that("model.frame.mvgam signature has trend_effects after formula", {
  fmls <- names(formals(getS3method("model.frame", "mvgam")))
  expect_equal(fmls[1L], "formula")
  expect_true("trend_effects" %in% fmls)
})

test_that("get_predict.mvgam validates type via checkmate::assert_choice", {
  stub <- structure(list(), class = "mvgam")
  # Invalid type triggers checkmate before any model machinery runs.
  expect_error(
    marginaleffects::get_predict(
      stub,
      newdata = data.frame(x = 1),
      type = "bogus"
    ),
    regexp = "type"
  )
})

test_that("get_coef.mvgam errors on trend_effects when no trend formula", {
  stub <- structure(
    list(trend_formula = NULL),
    class = "mvgam"
  )
  expect_error(
    marginaleffects::get_coef(stub, trend_effects = TRUE),
    regexp = "trend"
  )
})

test_that("get_vcov.mvgam returns NULL", {
  stub <- structure(list(), class = "mvgam")
  expect_null(marginaleffects::get_vcov(stub))
})

test_that("set_coef.mvgam is a no-op pass-through", {
  stub <- structure(list(marker = 42L), class = "mvgam")
  out <- marginaleffects::set_coef(stub, coefs = c(a = 1))
  expect_identical(out$marker, 42L)
})

test_that("find_predictors.mvgam pulls obs + trend + meta vars", {
  stub <- structure(
    list(
      formula = y ~ x1,
      trend_formula = trend_y ~ x2 - 1,
      trend_metadata = list(variables = list(time_var = "time",
                                              series_var = "series",
                                              gr_var = NA_character_,
                                              subgr_var = NA_character_))
    ),
    class = "mvgam"
  )
  preds <- insight::find_predictors(stub)$conditional
  expect_true(all(c("x1", "x2", "time", "series") %in% preds))
})

test_that("conditional_effects.mvgam is registered and re-exports the generic", {
  expect_true(
    !is.null(getS3method("conditional_effects", "mvgam", optional = TRUE))
  )
  exports <- getNamespaceExports("mvgam")
  expect_true("conditional_effects" %in% exports)
})

test_that("plot/print methods on mvgam_conditional_effects are registered", {
  expect_true(
    !is.null(getS3method("plot", "mvgam_conditional_effects",
                         optional = TRUE))
  )
  expect_true(
    !is.null(getS3method("print", "mvgam_conditional_effects",
                         optional = TRUE))
  )
})

test_that("conditional_effects.mvgam signature has expected args", {
  fmls <- names(formals(getS3method("conditional_effects", "mvgam")))
  expect_true("x" %in% fmls)
  expect_true("effects" %in% fmls)
  expect_true("type" %in% fmls)
  expect_true("process_error" %in% fmls)
})

test_that("re-exports of marginaleffects entry points are wired", {
  exports <- getNamespaceExports("mvgam")
  for (nm in c("predictions", "avg_predictions", "slopes", "avg_slopes",
               "comparisons", "avg_comparisons", "datagrid",
               "hypotheses", "plot_predictions",
               "plot_slopes", "plot_comparisons")) {
    expect_true(
      nm %in% exports,
      label = paste0("mvgam re-exports ", nm)
    )
  }
})
