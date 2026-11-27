# The tests of test-data-helpers.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()

test_that("a tsibble fills its gaps and keeps keys and covariates", {
  skip_if_not_installed("tsibble")
  month <- tsibble::yearmonth("2020 Nov") + c(0:4, 7:9)
  x <- tsibble::tsibble(
    month = rep(month, 2L),
    region = rep(c("north", "south"), each = 8L),
    cases = 1:16, temp = 16:1,
    key = "region", index = "month"
  )
  out <- series_to_mvgam(x, response = "cases")
  # Two missing months per region become rows with a missing response
  expect_identical(nrow(out), 20L)
  expect_identical(out$time[is.na(out$cases)], c(6L, 6L, 7L, 7L))
  expect_identical(
    out$season, as.integer(format(as.Date(out$month), "%m"))
  )
  expect_identical(out$temp[out$time == 1L], c(16L, 8L))
  expect_identical(levels(out$series), c("north", "south"))
})

test_that("2D single-species pivot composes with mvgam(family = occ())", {
  set.seed(12L)
  J <- 12L; K <- 3L
  y <- matrix(rbinom(J * K, 1L, 0.5), J, K)
  sc <- data.frame(env = rnorm(J))
  long <- pivot_detection_array(y, site_covs = sc)
  # `time` is the closure-unit identifier the helper writes itself.
  expect_identical(names(long),
                   c("site", "visit", "time", "y", "env"))
  # The helper drops `series` for single-species input, and mvgam
  # adds it as a constant level when fitting. A single series is
  # added here because the codegen path requires one.
  long$series <- factor("only")
  mf <- mvgam_formula(y ~ 1)
  # Codegen alone, with no fit, is enough to confirm the frame is
  # accepted.
  expect_no_error(
    stancode(
      mf, data = long, family = occ(), validate = FALSE
    )
  )
})
