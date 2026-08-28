# Multiple imputation forwarding.
#
# `mvgam()` hands a list of imputed frames to `mvgam_multiple()`. Its
# model-defining arguments are formals rather than dots, so they reach
# that call only by being named there. `family` was not, and every
# imputation was fitted under the default gaussian while the user had
# asked for something else. Nothing in the output said so.


test_that("the imputation path forwards every formal a fit needs", {
  # The guard against the same thing happening to the next argument
  # someone adds: the forwarded set and `mvgam()`'s own formals have
  # to agree, minus the two the call supplies itself.
  supplied_by_the_call <- c("data", "combine")
  needed <- setdiff(
    names(formals(mvgam)), c(supplied_by_the_call, "...")
  )
  expect_setequal(mvgam_imputation_forwarded, needed)
})


test_that("a requested family reaches the imputation fits", {
  captured <- NULL
  testthat::local_mocked_bindings(
    mvgam_multiple = function(...) {
      captured <<- list(...)
      structure(list(), class = "mvgam_stub")
    },
    .package = "mvgam"
  )
  d <- data.frame(
    y = c(1L, 3L, 2L, 5L, 4L, 6L), x = seq(-1, 1, length.out = 6L),
    time = 1:6, series = factor("s1")
  )
  out <- mvgam(y ~ x, data = list(d, d), family = poisson())
  expect_s3_class(out, "mvgam_stub")
  expect_equal(captured$family$family, "poisson")
  # The list itself and the combine setting come from the call.
  expect_length(captured$data_list, 2L)
  expect_true(captured$combine)
})


test_that("combine = FALSE reaches the imputation fits", {
  captured <- NULL
  testthat::local_mocked_bindings(
    mvgam_multiple = function(...) {
      captured <<- list(...)
      structure(list(), class = "mvgam_stub")
    },
    .package = "mvgam"
  )
  d <- data.frame(
    y = c(1.2, 0.4, -0.3, 0.9, 1.1, 0.2), x = seq(-1, 1, length.out = 6L),
    time = 1:6, series = factor("s1")
  )
  mvgam(y ~ x, data = list(d, d), combine = FALSE)
  expect_false(captured$combine)
})


test_that("threads travels only when it was set", {
  # `threads = NULL` trips an integer assertion downstream, so the
  # default must not be forwarded as an explicit NULL.
  captured <- NULL
  testthat::local_mocked_bindings(
    mvgam_multiple = function(...) {
      captured <<- list(...)
      structure(list(), class = "mvgam_stub")
    },
    .package = "mvgam"
  )
  d <- data.frame(
    y = c(1.2, 0.4, -0.3, 0.9, 1.1, 0.2), x = seq(-1, 1, length.out = 6L),
    time = 1:6, series = factor("s1")
  )
  mvgam(y ~ x, data = list(d, d))
  expect_false("threads" %in% names(captured))

  mvgam(y ~ x, data = list(d, d), threads = 2L)
  expect_equal(captured$threads, 2L)
})
