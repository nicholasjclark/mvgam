# Unit tests for the reshape in `R/linpred_grid.R`.
#
# `reshape_linpred_to_grid()` turns the ordered length-`nobs`
# vector `extract_component_linpred()` returns into the
# `[n_time, n_series]` grid the forecast path adds at series
# scale. Its guards are what stop a mismatched vector reaching
# the caller as a silent misalignment, so they are asserted
# directly rather than through a wrapper.


# A stub obs_struct carrying the fields
# `get_observation_structure()` produces.
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


test_that("a single series reshapes to one column", {
  obs <- make_obs_struct(time = c(1, 2, 3), series_int = c(1, 1, 1))
  grid <- reshape_linpred_to_grid(c(0.1, 0.2, 0.3), obs)
  expect_identical(dim(grid), c(3L, 1L))
  expect_equal(as.numeric(grid), c(0.1, 0.2, 0.3))
})


test_that("a multi-series vector reshapes by (time, series)", {
  # Rows arrive interleaved, as brms orders them.
  obs <- make_obs_struct(
    time = c(1, 1, 2, 2, 3, 3),
    series_int = c(1, 2, 1, 2, 1, 2)
  )
  grid <- reshape_linpred_to_grid(seq_len(6L) / 10, obs)
  expect_identical(dim(grid), c(3L, 2L))
  expect_equal(grid[, 1L], c(0.1, 0.3, 0.5))
  expect_equal(grid[, 2L], c(0.2, 0.4, 0.6))
})


test_that("rows in any order land on their own cell", {
  # The grid is addressed by value, not by position, so a shuffled
  # newdata gives the same answer as an ordered one.
  obs <- make_obs_struct(
    time = c(3, 1, 2, 1, 3, 2),
    series_int = c(2, 1, 2, 2, 1, 1)
  )
  grid <- reshape_linpred_to_grid(c(32, 11, 22, 12, 31, 21), obs)
  expect_equal(grid[, 1L], c(11, 21, 31))
  expect_equal(grid[, 2L], c(12, 22, 32))
})


test_that("times outside the grid are refused", {
  obs <- make_obs_struct(time = c(1, 2), series_int = c(1, 1))
  obs$unique_times <- c(5, 6)
  expect_error(reshape_linpred_to_grid(c(0.1, 0.2), obs),
                 "missing from the unique-time grid")
})


test_that("a mismatched vector length is refused", {
  obs <- make_obs_struct(time = c(1, 2, 3), series_int = c(1, 1, 1))
  expect_error(reshape_linpred_to_grid(c(0.1, 0.2), obs),
                 "does not match observation count")
})


test_that("a series index outside the range is refused", {
  obs <- make_obs_struct(time = c(1, 2), series_int = c(1, 2))
  obs$n_series <- 1L
  expect_error(reshape_linpred_to_grid(c(0.1, 0.2), obs),
                 "outside the \\[1, n_series\\] range")
})
