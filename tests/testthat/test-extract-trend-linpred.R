# Unit tests for `extract_trend_linpred()` in
# `R/extract_trend_linpred.R`. The function is a thin wrapper
# around `extract_component_linpred(..., component = "trend",
# incl_latent_state = FALSE)` plus a reshape into a `[n_time,
# n_series]` matrix; the brms-side machinery is exercised by
# other tests (test-component-linpred.R), so these tests use
# `local_mocked_bindings()` to stub the underlying primitives
# and focus on the slice / reshape / NULL-trend-formula / error
# paths the F2 wrapper owns.


# Build a mock mvgam fit with a chosen `trend_model`
# placeholder. The wrapper only checks `is.null(fit$trend_model)`
# before deferring to the stubbed `extract_component_linpred`,
# so the placeholder doesn't need to satisfy the brms interface.
make_mock_trend_fit <- function(trend_model = list()) {
  fit <- list(trend_model = trend_model)
  class(fit) <- "mvgam"
  fit
}


# Helper: construct a stub obs_struct list with the same fields
# `get_observation_structure` produces.
make_obs_struct <- function(time, series_int) {
  unique_times <- sort(unique(time))
  list(
    time = time,
    series_int = as.integer(series_int),
    unique_times = unique_times,
    n_obs = length(time),
    n_times = length(unique_times),
    n_series = length(unique(series_int))
  )
}


# ----- NULL trend_formula path ------------------------------------

test_that("Fit without trend_formula returns NULL", {
  fit <- make_mock_trend_fit(trend_model = NULL)
  newdata <- data.frame(time = 1:3, series = factor(rep("a", 3L)))
  expect_null(extract_trend_linpred(fit, 1L, newdata))
})


# ----- Basic slice + reshape --------------------------------------

test_that("Single-series fit returns [n_time, 1] matrix for one draw", {
  fit <- make_mock_trend_fit()
  newdata <- data.frame(time = c(1, 2, 3),
                         series = factor(rep("a", 3L)))
  # Pretend extract_component_linpred returns a [2 draws, 3 obs]
  # matrix; obs_struct maps the 3 obs to (time = 1:3, series = 1).
  full_mat <- matrix(c(10, 20, 30,
                        40, 50, 60),
                      nrow = 2L, byrow = TRUE)
  obs_struct <- make_obs_struct(time = c(1, 2, 3),
                                  series_int = c(1, 1, 1))

  testthat::local_mocked_bindings(
    extract_component_linpred = function(...) full_mat,
    get_observation_structure = function(...) obs_struct
  )

  res <- extract_trend_linpred(fit, 1L, newdata)
  expect_identical(dim(res), c(3L, 1L))
  expect_equal(as.numeric(res[, 1]), c(10, 20, 30))

  res2 <- extract_trend_linpred(fit, 2L, newdata)
  expect_equal(as.numeric(res2[, 1]), c(40, 50, 60))
})


test_that("Multi-series fit reshapes by (time, series_int) correctly", {
  fit <- make_mock_trend_fit()
  # Two series, three times each, interleaved in newdata row order.
  newdata <- data.frame(
    time = c(1, 1, 2, 2, 3, 3),
    series = factor(rep(c("a", "b"), times = 3L))
  )
  # One draw, 6 obs, ordered (t=1,s=1), (t=1,s=2), (t=2,s=1),
  # (t=2,s=2), (t=3,s=1), (t=3,s=2).
  full_mat <- matrix(c(11, 12, 21, 22, 31, 32),
                      nrow = 1L)
  obs_struct <- make_obs_struct(time = c(1, 1, 2, 2, 3, 3),
                                  series_int = c(1, 2, 1, 2, 1, 2))

  testthat::local_mocked_bindings(
    extract_component_linpred = function(...) full_mat,
    get_observation_structure = function(...) obs_struct
  )

  res <- extract_trend_linpred(fit, 1L, newdata)
  expect_identical(dim(res), c(3L, 2L))
  # Series 1 column: t = 1, 2, 3 -> 11, 21, 31.
  expect_equal(as.numeric(res[, 1]), c(11, 21, 31))
  # Series 2 column: t = 1, 2, 3 -> 12, 22, 32.
  expect_equal(as.numeric(res[, 2]), c(12, 22, 32))
})


test_that("Non-contiguous newdata rows still reshape by grid", {
  fit <- make_mock_trend_fit()
  # newdata rows are NOT pre-sorted by (time, series); the
  # reshape must rebuild the grid from `obs_struct` regardless.
  newdata <- data.frame(
    time = c(3, 1, 2, 3, 1, 2),
    series = factor(c("b", "a", "b", "a", "b", "a"))
  )
  full_mat <- matrix(c(32, 11, 22, 31, 12, 21),
                      nrow = 1L)
  obs_struct <- make_obs_struct(
    time = c(3, 1, 2, 3, 1, 2),
    series_int = c(2, 1, 2, 1, 2, 1)
  )

  testthat::local_mocked_bindings(
    extract_component_linpred = function(...) full_mat,
    get_observation_structure = function(...) obs_struct
  )

  res <- extract_trend_linpred(fit, 1L, newdata)
  expect_identical(dim(res), c(3L, 2L))
  expect_equal(as.numeric(res[, 1]), c(11, 21, 31))
  expect_equal(as.numeric(res[, 2]), c(12, 22, 32))
})


# ----- Multivariate-response error path ---------------------------

test_that("Per-response list return errors with a clear message", {
  fit <- make_mock_trend_fit()
  newdata <- data.frame(time = 1:2, series = factor(rep("a", 2L)))
  # extract_component_linpred returns a NAMED LIST of matrices
  # for multivariate response models. The wrapper rejects this
  # explicitly until forecasting supports mv responses.
  full_list <- list(
    y1 = matrix(c(1, 2), nrow = 1L),
    y2 = matrix(c(3, 4), nrow = 1L)
  )
  obs_struct <- make_obs_struct(time = 1:2, series_int = c(1L, 1L))
  testthat::local_mocked_bindings(
    extract_component_linpred = function(...) full_list,
    get_observation_structure = function(...) obs_struct
  )
  expect_error(
    extract_trend_linpred(fit, 1L, newdata),
    "Multivariate-response trend formulas"
  )
})


# ----- draw_id out of range ---------------------------------------

test_that("draw_id beyond available draws errors informatively", {
  fit <- make_mock_trend_fit()
  newdata <- data.frame(time = 1:2, series = factor(rep("a", 2L)))
  full_mat <- matrix(c(1, 2), nrow = 1L)
  obs_struct <- make_obs_struct(time = 1:2, series_int = c(1L, 1L))
  testthat::local_mocked_bindings(
    extract_component_linpred = function(...) full_mat,
    get_observation_structure = function(...) obs_struct
  )
  expect_error(
    extract_trend_linpred(fit, 5L, newdata),
    "exceeds the number of posterior draws"
  )
})


# ----- Internal reshape guard rails -------------------------------

test_that("Mismatched lp_vec length errors crisply", {
  # `reshape_linpred_to_grid` flags the mismatch rather than
  # letting a downstream subscript error surface.
  obs_struct <- make_obs_struct(time = c(1, 2, 3),
                                  series_int = c(1, 1, 1))
  expect_error(
    reshape_linpred_to_grid(c(10, 20), obs_struct),
    "does not match observation count"
  )
})


test_that("Series index outside [1, n_series] errors crisply", {
  obs_struct <- make_obs_struct(time = c(1, 2),
                                  series_int = c(1, 1))
  obs_struct$series_int <- c(1L, 5L)  # 5 > n_series = 1
  expect_error(
    reshape_linpred_to_grid(c(10, 20), obs_struct),
    "outside the \\[1, n_series\\] range"
  )
})
