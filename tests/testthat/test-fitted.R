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
