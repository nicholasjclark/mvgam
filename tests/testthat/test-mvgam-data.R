# Tests for the pre-fit `mvgam_data()` data inspector. Covers the
# happy path, the closure-unit dispatch, and the response-vs-family
# / structural errors users hit before paying for a Stan compile.


# ---- Happy path --------------------------------------------------

test_that("mvgam_data returns an mvgam_data list on a clean Poisson sim", {
  set.seed(1L)
  simdat <- sim_mvgam(family = poisson(), n_series = 3L,
                       n_timepoints = 20L)
  res <- suppressMessages(
    mvgam_data(simdat$data_train, y = "y", family = poisson(),
                plot = FALSE)
  )
  expect_s3_class(res, "mvgam_data")
  expect_identical(res$n_series, 3L)
  expect_identical(res$series_levels, levels(simdat$data_train$series))
  expect_equal(res$time_range,
               range(simdat$data_train$time, na.rm = TRUE))
})


test_that("mvgam_data validates closure-unit data via the shared validator", {
  cu <- sim_closure_unit_data(family = occ(), type = 1L)
  res <- suppressMessages(
    mvgam_data(cu$data_train, y = "y", family = occ(), plot = FALSE)
  )
  expect_s3_class(res, "mvgam_data")
})


test_that("mvgam_data synthesises a series factor when missing", {
  set.seed(2L)
  df <- data.frame(time = seq_len(20L),
                   y    = rpois(20L, lambda = 3))
  res <- suppressMessages(
    mvgam_data(df, y = "y", family = poisson(), plot = FALSE)
  )
  expect_identical(res$n_series, 1L)
})


# ---- Response-vs-family hard errors ------------------------------

test_that("mvgam_data errors on negative y with Poisson", {
  set.seed(3L)
  simdat <- sim_mvgam(family = poisson(), n_series = 2L,
                       n_timepoints = 15L)
  bad <- simdat$data_train
  bad$y[1L] <- -1L
  expect_error(
    suppressMessages(
      mvgam_data(bad, y = "y", family = poisson(), plot = FALSE)
    ),
    "negative values"
  )
})


test_that("mvgam_data errors on non-binary y with Bernoulli", {
  set.seed(4L)
  df <- data.frame(time   = seq_len(12L),
                   series = factor(rep("s1", 12L)),
                   y      = c(rep(0L, 6L), rep(2L, 6L)))
  expect_error(
    suppressMessages(
      mvgam_data(df, y = "y", family = bernoulli(), plot = FALSE)
    ),
    "0/1"
  )
})


test_that("mvgam_data errors on non-positive y with Gamma", {
  df <- data.frame(time   = seq_len(10L),
                   series = factor(rep("s1", 10L)),
                   y      = c(0, runif(9L)))
  expect_error(
    suppressMessages(
      mvgam_data(df, y = "y", family = Gamma(link = "log"),
                  plot = FALSE)
    ),
    "strictly positive"
  )
})


# ---- Structural errors -------------------------------------------

test_that("mvgam_data refuses multi-response families", {
  set.seed(5L)
  simdat <- sim_mvgam(family = poisson(), n_series = 2L,
                       n_timepoints = 10L)
  expect_error(
    suppressMessages(
      mvgam_data(simdat$data_train, y = "y", family = mvn(),
                  plot = FALSE)
    ),
    "Multi-response families"
  )
})


test_that("mvgam_data errors when 'series' is not a factor", {
  set.seed(6L)
  df <- data.frame(time   = seq_len(10L),
                   series = rep("s1", 10L),
                   y      = rpois(10L, lambda = 2))
  expect_error(
    suppressMessages(
      mvgam_data(df, y = "y", family = poisson(), plot = FALSE)
    ),
    "factor"
  )
})


# ---- Time regularity / CAR dispensation --------------------------

test_that("mvgam_data enforces regular time intervals by default", {
  set.seed(7L)
  df <- data.frame(time   = c(1L, 2L, 4L, 7L),
                   series = factor(rep("s1", 4L)),
                   y      = rpois(4L, lambda = 3))
  expect_error(
    suppressMessages(
      mvgam_data(df, y = "y", family = poisson(), plot = FALSE)
    ),
    "regular|Irregular"
  )
})


test_that("mvgam_data skips time-regularity check under CAR()", {
  set.seed(8L)
  df <- data.frame(time   = c(1L, 2L, 4L, 7L),
                   series = factor(rep("s1", 4L)),
                   y      = rpois(4L, lambda = 3))
  expect_no_error(
    suppressMessages(
      mvgam_data(df, y = "y", family = poisson(),
                  trend_model = CAR(), plot = FALSE)
    )
  )
})
