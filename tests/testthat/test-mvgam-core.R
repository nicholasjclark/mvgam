# `mvgam()` hands a list of imputed frames to `mvgam_multiple()`. Its
# model-defining arguments are formals, which reach that call only by
# name.


test_that("the imputation path forwards every formal a fit needs", {
  # The forwarded set is `mvgam()`'s formals, less the two the call
  # supplies itself. A formal added later must join it.
  supplied_by_the_call <- c("data", "combine")
  needed <- setdiff(
    names(formals(mvgam)), c(supplied_by_the_call, "...")
  )
  expect_setequal(mvgam_imputation_forwarded, needed)
})


test_that("a requested family and combine reach the imputation fits", {
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
  mvgam(y ~ x, data = list(d, d), family = poisson(), combine = FALSE)
  expect_equal(captured$family$family, "poisson")
  expect_false(captured$combine)
})


test_that("every form of threads gives a thread count", {
  # `NULL` reaches the code generator from `mvgam()` and `jsdgam()`
  expect_identical(mvgam:::thread_count(NULL), 1L)
  expect_identical(mvgam:::thread_count(2), 2L)
  expect_identical(mvgam:::thread_count(brms::threading(3)), 3L)
  expect_error(mvgam:::thread_count("two"), "threads")
})


test_that("a refusal names the caller's own value", {
  # `as_one_logical()` names the value with
  # `deparse0(s, max_char = 100L)`. `base::deparse()` takes no
  # `max_char`, which `deparse0()` must not pass on.
  expect_true(mvgam:::as_one_logical(TRUE))
  err <- expect_error(mvgam:::as_one_logical(c(TRUE, FALSE)))
  expect_false(
    grepl("unused argument", conditionMessage(err), fixed = TRUE)
  )

  # The truncation the caller asks for.
  long <- str2lang(paste(rep("aaaaaaaaaa", 30L), collapse = " + "))
  full <- mvgam:::deparse0(long)
  cut <- mvgam:::deparse0(long, max_char = 50L)
  expect_equal(nchar(cut), 53L)
  expect_true(endsWith(cut, "..."))
  expect_equal(substr(cut, 1L, 50L), substr(full, 1L, 50L))
  expect_equal(mvgam:::deparse0(quote(x + y)), "x + y")
})


test_that("a misspelt argument is refused before the model is built", {
  d <- data.frame(y = rpois(20, 3), time = 1:20, series = factor("a"))
  # The sampler would refuse `familly` only after the model compiled
  expect_error(
    mvgam(y ~ 1, trend_formula = ~ AR(), data = d, familly = poisson(),
          run_model = FALSE),
    "Did you mean 'family'?"
  )
  # cmdstanr's own `step_size` is close to `stepsize` and passes
  expect_s3_class(
    mvgam(y ~ 1, trend_formula = ~ AR(), data = d, family = poisson(),
          run_model = FALSE, silent = 2, validate = FALSE,
          refresh = 0, step_size = 0.1),
    "mvgam_prefit"
  )
})


test_that("imputations are pooled only when they share an axis", {
  # The pooled draws are indexed by the fits' own axes, whatever
  # columns those come from. The axes are compared.
  stub <- function(times) {
    axes <- list(
      series = list(levels = "s1", source = "explicit", n = 1L,
                    groups = NULL),
      time = list(values = times, n = length(times))
    )
    structure(list(trend_metadata = list(axes = axes)), class = "mvgam")
  }
  expect_error(
    mvgam:::pool_mvgam_fits(list(stub(1:5), stub(c(1:4, 6L)))),
    "different time or series axis"
  )
})
