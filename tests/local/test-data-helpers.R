# Data-helper checks that depend on external example datasets. These
# live under tests/local/ because spOccupancy is not a package
# dependency and is unavailable in the CI environment.

test_that("real spOccupancy `hbef2015` pivots cleanly + closure-unit validates", {
  testthat::skip_if_not_installed("spOccupancy")
  env <- new.env()
  data("hbef2015", package = "spOccupancy", envir = env)
  hbef <- env$hbef2015
  long <- pivot_detection_array(
    y         = hbef$y,
    site_covs = as.data.frame(hbef$occ.covs),
    obs_covs  = hbef$det.covs
  )
  N <- dim(hbef$y)[1L]
  J <- dim(hbef$y)[2L]
  K <- dim(hbef$y)[3L]
  expect_equal(nrow(long), N * J * K - sum(is.na(hbef$y)))
  expect_equal(levels(long$series), dimnames(hbef$y)[[1L]])
  expect_true(all(c("Elevation", "day", "tod") %in% colnames(long)))

  arrays <- mvgam:::build_closure_unit_arrays(
    long, response_var = "y", compute_y_max = FALSE,
    unit_grouping_vars = c("series", "time")
  )
  expect_equal(arrays$N_unit, N * J)
  expect_true(all(arrays$n_rep <= K))
  expect_true(all(arrays$n_rep >= 1L))
})
