#' Unit tests for the combination logic in get_combined_linpred()
#'
#' get_combined_linpred() takes the obs and trend linear predictors and
#' adds them on the link scale. It must handle three shapes that the
#' upstream extract_component_linpred() can return:
#'
#'   - both obs and trend as single matrix (univariate model)
#'   - obs as named list, trend as single matrix (multivariate obs +
#'     shared trend across responses; the most common multivariate
#'     case in mvgam, exercised by tests/local fit2)
#'   - obs as named list, trend as named list (multivariate obs with
#'     per-response trend output; not produced by any current mvgam
#'     codegen path, but the combination branch must still be correct
#'     because the prediction-system contract supports it)
#'
#' Tests use testthat::local_mocked_bindings() to feed deterministic
#' inputs through the combination, avoiding the need for fitted Stan
#' models in fast unit tests.

stub_obj <- function(has_trend = TRUE) {
  out <- structure(list(), class = "mvgam")
  if (has_trend) {
    out$trend_model <- list(formula = ~ 1)
  }
  out
}


test_that("get_combined_linpred adds obs and trend in univariate case", {
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata, component, ...) {
      if (component == "obs") {
        matrix(1, nrow = 4, ncol = 3)
      } else {
        matrix(0.5, nrow = 4, ncol = 3)
      }
    },
    has_stochastic_trend = function(object) FALSE,
    .package = "mvgam"
  )
  out <- get_combined_linpred(stub_obj(), newdata = NULL,
                              process_error = TRUE)
  expect_true(is.matrix(out))
  expect_equal(dim(out), c(4L, 3L))
  expect_equal(out, matrix(1.5, 4, 3))
})


test_that("get_combined_linpred handles list obs + matrix shared trend", {
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata, component, ...) {
      if (component == "obs") {
        list(
          y1 = matrix(1, nrow = 5, ncol = 4),
          y2 = matrix(10, nrow = 5, ncol = 4)
        )
      } else {
        # Shared trend: single matrix applies to both responses
        matrix(0.25, nrow = 5, ncol = 4)
      }
    },
    has_stochastic_trend = function(object) FALSE,
    .package = "mvgam"
  )
  out <- get_combined_linpred(stub_obj(), newdata = NULL,
                              process_error = TRUE)
  expect_type(out, "list")
  expect_named(out, c("y1", "y2"))
  expect_equal(out$y1, matrix(1.25, 5, 4))
  expect_equal(out$y2, matrix(10.25, 5, 4))
})


test_that("get_combined_linpred handles list obs + list per-response trend", {
  # The per-response trend list shape isn't produced by current mvgam
  # codegen (single trend type per fit; see architecture decisions),
  # but the combination logic supports it for completeness.
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata, component, ...) {
      if (component == "obs") {
        list(
          y1 = matrix(1, nrow = 5, ncol = 4),
          y2 = matrix(10, nrow = 5, ncol = 4)
        )
      } else {
        list(
          y1 = matrix(0.1, nrow = 5, ncol = 4),
          y2 = matrix(2.0, nrow = 5, ncol = 4)
        )
      }
    },
    has_stochastic_trend = function(object) FALSE,
    .package = "mvgam"
  )
  out <- get_combined_linpred(stub_obj(), newdata = NULL,
                              process_error = TRUE)
  expect_type(out, "list")
  expect_named(out, c("y1", "y2"))
  expect_equal(out$y1, matrix(1.1, 5, 4))
  expect_equal(out$y2, matrix(12.0, 5, 4))
})


test_that("get_combined_linpred process_error=FALSE collapses trend to mean", {
  trend_draws <- matrix(c(0, 0.4, 0.8, 1.2,
                           0, 0.4, 0.8, 1.2,
                           0, 0.4, 0.8, 1.2), 3, 4, byrow = TRUE)
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata, component, ...) {
      if (component == "obs") {
        matrix(0, 3, 4)
      } else {
        # Trend draws differ across draws so the column-mean differs
        # from any single draw
        rbind(c(0, 0, 0, 0),
              c(1, 1, 1, 1),
              c(2, 2, 2, 2))
      }
    },
    has_stochastic_trend = function(object) FALSE,
    .package = "mvgam"
  )
  out <- get_combined_linpred(stub_obj(), newdata = NULL,
                              process_error = FALSE)
  # Each column collapsed to its mean (= 1) and broadcast to all rows
  expect_equal(out, matrix(1, 3, 4))
})


test_that("get_combined_linpred errors on dimension mismatch (univariate)", {
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata, component, ...) {
      if (component == "obs") {
        matrix(0, 3, 4)
      } else {
        matrix(0, 3, 5)  # mismatched ncols
      }
    },
    has_stochastic_trend = function(object) FALSE,
    .package = "mvgam"
  )
  expect_error(
    get_combined_linpred(stub_obj(), newdata = NULL,
                         process_error = TRUE),
    "Dimension mismatch"
  )
})


test_that("get_combined_linpred errors on dim mismatch (per-response)", {
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata, component, ...) {
      if (component == "obs") {
        list(y1 = matrix(0, 3, 4), y2 = matrix(0, 3, 4))
      } else {
        list(y1 = matrix(0, 3, 4), y2 = matrix(0, 3, 5))  # y2 mismatch
      }
    },
    has_stochastic_trend = function(object) FALSE,
    .package = "mvgam"
  )
  expect_error(
    get_combined_linpred(stub_obj(), newdata = NULL,
                         process_error = TRUE),
    "Dimension mismatch"
  )
})


test_that("get_combined_linpred returns obs only when no trend model", {
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata, component, ...) {
      stopifnot(component == "obs")
      matrix(7, 3, 2)
    },
    .package = "mvgam"
  )
  out <- get_combined_linpred(stub_obj(has_trend = FALSE),
                              newdata = NULL, process_error = TRUE)
  expect_equal(out, matrix(7, 3, 2))
})
