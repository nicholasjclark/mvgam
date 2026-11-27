
test_that("invalid series name errors with the available list", {
  fc <- .make_fc(series = c("a", "b"))
  expect_error(plot(fc, series = "zzz"), "Unknown series name")
})

test_that("both arms FALSE errors clearly", {
  fc <- .make_fc()
  expect_error(
    plot(fc, hindcast = FALSE, forecast = FALSE),
    "At least one"
  )
})

test_that("a jsdgam forecast refuses a time axis", {
  # The units of a jsdgam() fit are sites in arbitrary order. A line
  # through them would draw dynamics the model does not have.
  fc <- .make_fc()
  fc$site_units <- TRUE
  expect_error(plot(fc), "Plotting a jsdgam() fit along time",
               fixed = TRUE)
})
