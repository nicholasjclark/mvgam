#' Unit tests for fitted.mvgam
#'
#' Mirrors the brms fitted.brmsfit interface. Heavy lifting is done by
#' posterior_epred / posterior_linpred / summarize_predictions, which
#' have their own tests; here we pin the dispatch and summary contract
#' by mocking the inner posterior calls.

stub_object <- function() structure(list(), class = "mvgam")

univariate_draws <- function(...) {
  matrix(rep(c(1, 2, 3), each = 4), 4, 3,
         dimnames = list(NULL, paste0("o", 1:3)))
}
linear_draws <- function(...) {
  matrix(rep(c(0.1, 0.2, 0.3), each = 4), 4, 3,
         dimnames = list(NULL, paste0("o", 1:3)))
}
mv_epred_draws <- function(...) {
  list(y1 = matrix(1, 5, 4), y2 = matrix(10, 5, 4))
}
mv_linpred_draws <- function(...) {
  list(y1 = matrix(0.1, 5, 4), y2 = matrix(2.3, 5, 4))
}


test_that("fitted.mvgam dispatches to posterior_epred for response scale", {
  testthat::local_mocked_bindings(
    posterior_epred = univariate_draws,
    posterior_linpred = function(...) stop("should not be called"),
    .package = "mvgam"
  )
  out <- fitted(stub_object(), summary = FALSE)
  expect_true(is.matrix(out))
  expect_equal(dim(out), c(4L, 3L))
  expect_equal(out[1, ], c(o1 = 1, o2 = 2, o3 = 3))
})


test_that("fitted.mvgam dispatches to posterior_linpred for linear scale", {
  testthat::local_mocked_bindings(
    posterior_linpred = linear_draws,
    posterior_epred = function(...) stop("should not be called"),
    .package = "mvgam"
  )
  out <- fitted(stub_object(), scale = "linear", summary = FALSE)
  expect_equal(dim(out), c(4L, 3L))
  expect_equal(out[1, ], c(o1 = 0.1, o2 = 0.2, o3 = 0.3))
})


test_that("fitted.mvgam summary returns brms-style columns", {
  testthat::local_mocked_bindings(
    posterior_epred = univariate_draws,
    .package = "mvgam"
  )
  s <- fitted(stub_object(), summary = TRUE)
  expect_true(is.matrix(s))
  expect_equal(rownames(s), c("o1", "o2", "o3"))
  expect_true(all(c("Estimate", "Est.Error", "Q2.5", "Q97.5") %in%
                  colnames(s)))
  expect_equal(unname(s[, "Estimate"]), c(1, 2, 3))
  expect_equal(unname(s[, "Est.Error"]), c(0, 0, 0))
})


test_that("fitted.mvgam summary respects probs and robust args", {
  testthat::local_mocked_bindings(
    posterior_epred = univariate_draws,
    .package = "mvgam"
  )
  s <- fitted(stub_object(), summary = TRUE,
              probs = c(0.1, 0.5, 0.9), robust = TRUE)
  expect_true(all(c("Q10", "Q50", "Q90") %in% colnames(s)))
  expect_equal(unname(s[, "Estimate"]), c(1, 2, 3))
})


test_that("fitted.mvgam summary handles multivariate list correctly", {
  testthat::local_mocked_bindings(
    posterior_epred = mv_epred_draws,
    .package = "mvgam"
  )
  out <- fitted(stub_object(), summary = TRUE)
  expect_type(out, "list")
  expect_equal(names(out), c("y1", "y2"))
  expect_true(all(sapply(out, is.matrix)))
  expect_equal(unname(out$y1[, "Estimate"]), rep(1, 4))
  expect_equal(unname(out$y2[, "Estimate"]), rep(10, 4))
})


test_that("fitted.mvgam returns raw list when summary = FALSE for mv", {
  testthat::local_mocked_bindings(
    posterior_epred = mv_epred_draws,
    .package = "mvgam"
  )
  out <- fitted(stub_object(), summary = FALSE)
  expect_type(out, "list")
  expect_equal(dim(out$y1), c(5L, 4L))
  expect_equal(dim(out$y2), c(5L, 4L))
})


test_that("fitted.mvgam validates scale argument", {
  expect_error(fitted(stub_object(), scale = "bogus"))
})


test_that("fitted.mvgam validates probs and other inputs", {
  expect_error(fitted(stub_object(), probs = c(-0.1, 0.5)), "probs")
  expect_error(fitted(stub_object(), robust = "yes"), "robust")
  expect_error(fitted(stub_object(), summary = NA), "summary")
})


# ---------------------------------------------------------------
# components + unit_level routing (task #308): delegation pattern
# verified through stubbed posterior_epred / predict; the heavy
# work belongs to predict.mvgam and aggregate_closure_unit_visits,
# both of which have their own tests.
# ---------------------------------------------------------------

# Family stub that satisfies is_closure_unit_family() and
# needs_closure_unit_aggregation() so the dispatch reaches the
# aggregator branch without needing a real fit.
make_occ_family_stub <- function() {
  structure(
    list(family = "occ", name = "occ"),
    class = "customfamily",
    mvgam_closure_unit  = TRUE,
    mvgam_unit_grouping = c("series", "time"),
    mvgam_default_cap   = 1L
  )
}

stub_closure_object <- function() {
  structure(
    list(
      family  = make_occ_family_stub(),
      formula = y ~ 1,
      data    = data.frame(
        series = factor(rep(1:2, each = 3L)),
        time   = rep(1:3, 2L),
        y      = c(0L, 1L, 0L, 1L, 1L, 0L)
      )
    ),
    class = "mvgam"
  )
}

test_that("fitted.mvgam validates components arg", {
  expect_error(fitted(stub_object(), components = "bogus"))
})

test_that("fitted.mvgam validates unit_level arg", {
  expect_error(fitted(stub_object(), unit_level = "yes"))
})

test_that("fitted.mvgam(components = 'latent_state') delegates to predict()", {
  testthat::local_mocked_bindings(
    predict.mvgam = function(object, type, ...) {
      structure(matrix(0.5, 4L, 2L), called_with_type = type)
    },
    posterior_epred = function(...) stop("should not be called"),
    .package = "mvgam"
  )
  out <- fitted(stub_object(), components = "latent_state",
                summary = FALSE)
  expect_true(is.matrix(out))
  expect_identical(attr(out, "called_with_type"), "latent_state")
})

test_that("fitted.mvgam(components = 'detection') delegates to predict()", {
  testthat::local_mocked_bindings(
    predict.mvgam = function(object, type, ...) {
      structure(matrix(0.7, 4L, 6L), called_with_type = type)
    },
    .package = "mvgam"
  )
  out <- fitted(stub_object(), components = "detection",
                summary = FALSE)
  expect_identical(attr(out, "called_with_type"), "detection")
})

test_that("fitted.mvgam(components != 'response') warns on unit_level", {
  testthat::local_mocked_bindings(
    predict.mvgam = function(object, type, ...) {
      matrix(0.5, 4L, 2L)
    },
    .package = "mvgam"
  )
  expect_warning(
    fitted(stub_object(), components = "latent_state",
           unit_level = TRUE, summary = FALSE),
    "'unit_level' ignored"
  )
})

test_that("fitted.mvgam(unit_level = TRUE) aggregates via the visit summer", {
  visit_draws <- function(...) {
    matrix(seq_len(4L * 6L), nrow = 4L, ncol = 6L)
  }
  agg_called <- FALSE
  testthat::local_mocked_bindings(
    posterior_epred = visit_draws,
    aggregate_closure_unit_visits = function(object, newdata, yrep_visit) {
      agg_called <<- TRUE
      list(
        y_unit    = c(1, 1),
        yrep_unit = yrep_visit[, 1:2, drop = FALSE],
        arrays    = list()
      )
    },
    .package = "mvgam"
  )
  out <- fitted(stub_closure_object(), unit_level = TRUE,
                summary = FALSE)
  expect_true(agg_called)
  expect_equal(dim(out), c(4L, 2L))
})

test_that("fitted.mvgam(unit_level = TRUE) warns on non-aggregating family", {
  testthat::local_mocked_bindings(
    posterior_epred = univariate_draws,
    .package = "mvgam"
  )
  obj <- stub_object()
  obj$family <- gaussian()
  expect_warning(
    fitted(obj, unit_level = TRUE, summary = FALSE),
    "'unit_level = TRUE' ignored"
  )
})

test_that("fitted.mvgam(unit_level = TRUE) errors on multi-response fit", {
  testthat::local_mocked_bindings(
    posterior_epred = mv_epred_draws,
    .package = "mvgam"
  )
  out <- expect_error(
    fitted(stub_closure_object(), unit_level = TRUE, summary = FALSE),
    "multi-response"
  )
})
