# Tests for setup_brms_lightweight() and the trend parse it serves

setup_frame <- function() {
  data.frame(
    y = rnorm(20),
    x = rnorm(20),
    time = 1:20,
    temperature = rnorm(20),
    habitat = factor(rep(c("A", "B"), 10)),
    series = factor(rep(1:2, each = 10))
  )
}


test_that("setup_brms_lightweight returns the mock fit, its program and data", {
  data <- setup_frame()
  setup <- setup_brms_lightweight(formula = y ~ x, data = data)

  expect_true(inherits(setup$brmsfit, "brmsfit"))
  expect_true(inherits(setup$prior, "brmsprior"))
  # The mock backend emits a whole Stan program, with each block
  # opening once
  for (blk in c("functions", STAN_BLOCKS)) {
    expect_identical(stan_block_count(setup$stancode, blk), 1L)
  }
  # Stan data carries the response and the row count brms took from
  # the frame
  expect_identical(setup$standata$N, nrow(data))
  expect_identical(as.numeric(setup$standata$Y), data$y)
})


test_that("setup_brms_lightweight refuses a malformed formula or frame", {
  expect_error(
    setup_brms_lightweight(formula = "not a formula",
                           data = data.frame(y = 1, x = 1))
  )
  expect_error(setup_brms_lightweight(formula = y ~ x,
                                      data = "not a data frame"))
  expect_error(setup_brms_lightweight(formula = y ~ x,
                                      data = data.frame()))
})


test_that("a trend formula parses to its constructor and brms terms", {
  # A trend formula naming no constructor defaults to ZMVN
  for (tf in list(~ 1, ~ gp(time, k = 6))) {
    parsed <- parse_multivariate_trends(y ~ x, tf)
    expect_identical(parsed$trend_specs$trend, "ZMVN")
  }
  # brms cannot evaluate a trend constructor. The base formula drops it
  # and keeps the covariates around it.
  parsed <- parse_multivariate_trends(
    y ~ x, ~ temperature + RW(time = time, series = series) + habitat
  )
  expect_identical(parsed$trend_specs$trend, "RW")
  expect_identical(all.vars(parsed$base_formula),
                   c("temperature", "habitat"))
})
