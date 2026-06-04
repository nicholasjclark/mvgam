# Contract tests for trend Stan generators. Locks the
# scalar-vs-vector `p` semantics in AR and VAR.
#
# Semantics (intended):
#   AR(p = 3)        -> CONSECUTIVE lags 1, 2, 3
#                       (declares ar1_trend, ar2_trend, ar3_trend)
#   AR(p = c(2, 4))  -> SPARSE: only lags 2 and 4
#                       (declares ar2_trend, ar4_trend; no ar1)
#   VAR(p = 3)       -> CONSECUTIVE lags 1, 2, 3
#                       (declares array[3] A_raw_trend matrices)
#   VAR(p = c(2, 4)) -> SPARSE: only lags 2 and 4
#                       (declares array[2] A_raw_trend matrices
#                        plus an `active_lags_trend` data array)
#
# Tests build an `mvgam_formula` and call `stancode()` with
# `validate = FALSE` so no Stan compilation runs. Assertions
# use whitespace-tolerant literal substring matches against the
# generated Stan source.


# Whitespace-tolerant literal substring matcher. Tests assert
# that a specific Stan declaration or dynamics expression is
# present; brackets / angle brackets / parentheses come along
# verbatim. Stripping whitespace from both sides handles
# formatter differences; `fixed = TRUE` sidesteps regex
# escaping entirely.
stan_has <- function(code, snippet) {
  remove_ws <- function(text) gsub("\\s+", "", text)
  grepl(remove_ws(snippet), remove_ws(code), fixed = TRUE)
}


build_test_data <- function() {
  set.seed(42L)
  data.frame(
    time = 1:24,
    series = factor(rep("series1", 24)),
    y = stats::rpois(24, lambda = 5),
    x = stats::rnorm(24)
  )
}


get_trend_stancode <- function(form, dat = NULL,
                                family = poisson()) {
  if (is.null(dat)) dat <- build_test_data()
  mf <- mvgam_formula(y ~ x, trend_formula = form)
  stancode(mf, data = dat, family = family, validate = FALSE)
}


# ----- AR scalar p (intended: CONSECUTIVE lags 1..p) ------------

test_that("AR(p = 1) declares ar1_trend only", {
  code <- get_trend_stancode(~ AR(p = 1))
  expect_true(stan_has(code, "[N_lv_trend] ar1_trend;"))
  expect_false(stan_has(code, "ar2_trend"))
  expect_false(stan_has(code, "ar3_trend"))
})

test_that("AR(p = 2) declares ar1_trend AND ar2_trend", {
  code <- get_trend_stancode(~ AR(p = 2))
  expect_true(stan_has(code, "[N_lv_trend] ar1_trend;"))
  expect_true(stan_has(code, "[N_lv_trend] ar2_trend;"))
  expect_false(stan_has(code, "ar3_trend"))
  # Dynamics must reference both lag-1 AND lag-2 of lv_trend.
  expect_true(stan_has(code, "lv_trend[i - 1, j]"))
  expect_true(stan_has(code, "lv_trend[i - 2, j]"))
})

test_that("AR(p = 3) declares ar1_trend, ar2_trend, ar3_trend", {
  code <- get_trend_stancode(~ AR(p = 3))
  expect_true(stan_has(code, "[N_lv_trend] ar1_trend;"))
  expect_true(stan_has(code, "[N_lv_trend] ar2_trend;"))
  expect_true(stan_has(code, "[N_lv_trend] ar3_trend;"))
  expect_true(stan_has(code, "lv_trend[i - 3, j]"))
})


# ----- AR sparse-lag p (intended: ONLY listed lags) -------------

test_that("AR(p = c(2, 4)) declares ar2_trend and ar4_trend only", {
  code <- get_trend_stancode(~ AR(p = c(2, 4)))
  expect_true(stan_has(code, "[N_lv_trend] ar2_trend;"))
  expect_true(stan_has(code, "[N_lv_trend] ar4_trend;"))
  expect_false(stan_has(code, "ar1_trend"))
  expect_false(stan_has(code, "ar3_trend"))
  expect_true(stan_has(code, "lv_trend[i - 2, j]"))
  expect_true(stan_has(code, "lv_trend[i - 4, j]"))
})


# ----- initial_joint_var() out-of-bounds guard ------------------

test_that("initial_joint_var guards the q=0 (pure VAR) case", {
  # Pure VAR (no MA) has q = 0. The companion_var matrix is then
  # sized (p + q) * m = p * m, so the MA-block writes at indices
  # (p * m + 1):((p + 1) * m) are out of bounds. The Stan
  # function must wrap those writes in `if (q > 0)`. Without this
  # guard every pure VAR fit crashed at runtime with an
  # 'accessing element out of range' exception.
  code <- get_trend_stancode(~ VAR(p = 1))
  expect_true(stan_has(code, "if (q > 0)"))
  expect_true(stan_has(code, "companion_var[1:m, 1:m] = Sigma;"))
})


# ----- VAR scalar p (intended: CONSECUTIVE lags 1..p) ------------

test_that("VAR(p = 1) declares array[1] A_raw_trend", {
  code <- get_trend_stancode(~ VAR(p = 1))
  expect_true(stan_has(
    code, "array[1] matrix[N_lv_trend, N_lv_trend] A_raw_trend"
  ))
  expect_true(stan_has(code, "[N_lv_trend] sigma_trend;"))
})

test_that("VAR(p = 2) declares array[2] A_raw_trend", {
  code <- get_trend_stancode(~ VAR(p = 2))
  expect_true(stan_has(
    code, "array[2] matrix[N_lv_trend, N_lv_trend] A_raw_trend"
  ))
})


# ----- VAR sparse-lag p (currently rejected) --------------------

test_that("VAR(p = c(2, 4)) errors with an informative message", {
  # Sparse-lag VAR is not yet supported: the Heaps-2022
  # stationary joint-distribution initialisation assumes a
  # consecutive companion-form structure, and deriving the
  # sparse-companion stationary covariance is a separate piece
  # of work. The constructor rejects vector 'p' with a clear
  # message that points users at AR(p = c(...)) for the
  # univariate sparse-lag case.
  expect_error(
    VAR(p = c(2, 4)),
    "Sparse-lag VAR"
  )
})
