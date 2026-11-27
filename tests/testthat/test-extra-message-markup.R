# The tests of test-message-markup.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()

test_that("no user-facing message ships unrendered cli markup", {
  ns <- asNamespace("mvgam")
  fns <- Filter(function(nm) is.function(get(nm, envir = ns)),
                ls(ns, all.names = TRUE))
  # An empty set would satisfy the loop without checking anything.
  expect_gt(length(fns), 200L)

  offenders <- character(0)
  for (nm in fns) {
    f <- get(nm, envir = ns)
    if (is.null(body(f))) next
    hits <- unrendered_markup(f)
    if (length(hits)) {
      offenders <- c(offenders, paste0(nm, ": ", hits[1L]))
    }
  }
  expect_identical(offenders, character(0))
})
