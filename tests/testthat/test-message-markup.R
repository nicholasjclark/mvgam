
test_that("the markup check finds markup a format call would print", {
  # The check is only worth having if it fails on the shape it
  # exists to catch, so both spellings of the real defect are put to
  # it here alongside the wrapped form that is correct.
  bare <- function() {
    stop(insight::format_error("Needs {.field resp}."))
  }
  in_c <- function() {
    stop(insight::format_error(c(
      "Header.", i = "Pass {.val {nm}}."
    )))
  }
  wrapped <- function() {
    stop(insight::format_error(
      cli::format_inline("Needs {.field resp}.")
    ))
  }
  plain <- function() {
    stop(insight::format_error("Needs a response name."))
  }
  expect_length(unrendered_markup(bare), 1L)
  expect_length(unrendered_markup(in_c), 1L)
  expect_length(unrendered_markup(wrapped), 0L)
  expect_length(unrendered_markup(plain), 0L)
})
