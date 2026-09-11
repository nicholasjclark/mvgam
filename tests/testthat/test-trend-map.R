# Unit tests for `normalise_trend_map()`: the single entry
# point that converts every accepted `trend_map` input shape
# (matrix, data.frame, character code) to a canonical numeric
# `Z` matrix.

.make_data <- function(n_series = 3L) {
  data.frame(
    y = rnorm(n_series * 10L),
    time = rep(seq_len(10L), n_series),
    series = factor(
      rep(paste0("s", seq_len(n_series)), each = 10L),
      levels = paste0("s", seq_len(n_series))
    )
  )
}


# ---- NULL passthrough ------------------------------------

test_that("NULL input returns NULL fast", {
  expect_null(mvgam:::normalise_trend_map(NULL, .make_data()))
})


# ---- Character codes -------------------------------------

test_that("character 'identity' yields the identity matrix", {
  out <- mvgam:::normalise_trend_map("identity", .make_data(3L))
  expect_equal(out$n_lv, 3L)
  expect_equal(unname(out$Z), diag(1, 3L))
  expect_equal(rownames(out$Z), c("s1", "s2", "s3"))
})

test_that("character 'shared' yields a single-column ones matrix", {
  out <- mvgam:::normalise_trend_map("shared", .make_data(4L))
  expect_equal(out$n_lv, 1L)
  expect_equal(unname(out$Z), matrix(1, nrow = 4L, ncol = 1L))
})

test_that("unknown character code errors", {
  expect_error(
    mvgam:::normalise_trend_map("foo", .make_data()),
    "Unknown 'trend_map' code"
  )
})

test_that("character vector input errors", {
  expect_error(
    mvgam:::normalise_trend_map(c("identity", "shared"), .make_data()),
    "single string"
  )
})


# ---- Numeric matrix --------------------------------------

test_that("numeric matrix passes through with row / column names", {
  Z_in <- matrix(c(1, 0, 0.25, 0.75, 1, 0),
                 nrow = 3L, ncol = 2L, byrow = TRUE)
  out <- mvgam:::normalise_trend_map(Z_in, .make_data(3L))
  expect_equal(out$n_lv, 2L)
  expect_equal(unname(out$Z), Z_in)
  expect_equal(rownames(out$Z), c("s1", "s2", "s3"))
  expect_equal(colnames(out$Z), c("trend_1", "trend_2"))
})

test_that("matrix with wrong row count errors", {
  expect_error(
    mvgam:::normalise_trend_map(matrix(1, nrow = 5L, ncol = 1L),
                                .make_data(3L)),
    "wrong number of rows"
  )
})

test_that("matrix with Inf entries errors", {
  Z_in <- matrix(c(1, 0, Inf, 0.5, 1, 0),
                 nrow = 3L, ncol = 2L, byrow = TRUE)
  expect_error(
    mvgam:::normalise_trend_map(Z_in, .make_data(3L)),
    "Inf or NaN"
  )
})

test_that("matrix with NA entries is accepted (partial Z)", {
  # NA marks a free entry in the partial-Z surface. Each row
  # still has at least one fixed non-zero (or any NA), so the
  # zero-row guard is satisfied.
  Z_in <- matrix(c(1, 0, NA, 0.5, 1, 0),
                 nrow = 3L, ncol = 2L, byrow = TRUE)
  out <- mvgam:::normalise_trend_map(Z_in, .make_data(3L))
  expect_equal(out$n_lv, 2L)
  expect_true(anyNA(out$Z))
  # byrow = TRUE layout: row 2 = (NA, 0.5) so NA at Z[2, 1].
  expect_true(is.na(out$Z[2L, 1L]))
  expect_equal(out$Z[2L, 2L], 0.5)
})

test_that("matrix with all-zero rows errors", {
  Z_in <- matrix(c(1, 0, 0, 0, 1, 0),
                 nrow = 3L, ncol = 2L, byrow = TRUE)
  expect_error(
    mvgam:::normalise_trend_map(Z_in, .make_data(3L)),
    "zero-loading"
  )
})

test_that("matrix with all-NA row passes (every entry free)", {
  Z_in <- matrix(c(NA, NA, 1, 0, 0, 1),
                 nrow = 3L, ncol = 2L, byrow = TRUE)
  out <- mvgam:::normalise_trend_map(Z_in, .make_data(3L))
  expect_true(all(is.na(out$Z[1L, ])))
})

test_that("fully-free Z passes validation (Heaps QR identifies it)", {
  # All-NA matrix must be explicitly numeric (NA defaults to
  # logical, which the matrix branch rejects). Fully-free
  # columns are identified up to sign by the post-hoc QR
  # rotation emitted in generate_factor_model(); no warning
  # about unfixed columns is raised at validation time.
  Z_in <- matrix(NA_real_, nrow = 3L, ncol = 2L)
  expect_no_warning(
    out <- mvgam:::normalise_trend_map(Z_in, .make_data(3L))
  )
  expect_equal(sum(is.na(out$Z)), 6L)
})


# ---- data.frame mapping ----------------------------------

test_that("data.frame mapping builds binary Z aligned to series_levels", {
  tm <- data.frame(
    series = c("s2", "s1", "s3"),
    trend = c(2, 1, 2)
  )
  out <- mvgam:::normalise_trend_map(tm, .make_data(3L))
  expect_equal(out$n_lv, 2L)
  expected <- matrix(c(1, 0,
                       0, 1,
                       0, 1),
                     nrow = 3L, ncol = 2L, byrow = TRUE,
                     dimnames = list(c("s1", "s2", "s3"),
                                     c("trend_1", "trend_2")))
  expect_equal(out$Z, expected)
})

test_that("data.frame missing the trend column errors", {
  tm <- data.frame(series = c("s1", "s2", "s3"))
  expect_error(
    mvgam:::normalise_trend_map(tm, .make_data(3L)),
    "not found"
  )
})

test_that("data.frame with factor-class series column works", {
  tm <- data.frame(
    series = factor(c("s2", "s1", "s3"),
                    levels = c("s1", "s2", "s3")),
    trend = c(2L, 1L, 2L)
  )
  out <- mvgam:::normalise_trend_map(tm, .make_data(3L))
  expect_equal(out$n_lv, 2L)
  expected <- matrix(c(1, 0,
                       0, 1,
                       0, 1),
                     nrow = 3L, ncol = 2L, byrow = TRUE)
  expect_equal(unname(out$Z), expected)
})

test_that("data.frame with character trend column errors clearly", {
  tm <- data.frame(
    series = c("s1", "s2", "s3"),
    trend = c("a", "b", "a")
  )
  expect_error(
    mvgam:::normalise_trend_map(tm, .make_data(3L)),
    "numeric or integer column"
  )
})

test_that("data.frame missing a series errors", {
  tm <- data.frame(series = c("s1", "s2"), trend = c(1, 2))
  expect_error(
    mvgam:::normalise_trend_map(tm, .make_data(3L)),
    "every training series"
  )
})

test_that("data.frame with duplicate series errors", {
  tm <- data.frame(
    series = c("s1", "s1", "s2", "s3"),
    trend = c(1, 2, 1, 2)
  )
  expect_error(
    mvgam:::normalise_trend_map(tm, .make_data(3L)),
    "duplicate"
  )
})

test_that("data.frame with unknown series errors", {
  tm <- data.frame(
    series = c("s1", "s2", "s_unknown"),
    trend = c(1, 1, 2)
  )
  expect_error(
    mvgam:::normalise_trend_map(tm, .make_data(3L)),
    "every training series"
  )
})

test_that("data.frame with non-contiguous trend integers errors", {
  tm <- data.frame(
    series = c("s1", "s2", "s3"),
    trend = c(1, 3, 1)
  )
  expect_error(
    mvgam:::normalise_trend_map(tm, .make_data(3L)),
    "contiguous"
  )
})

test_that("data.frame with negative trend integers errors", {
  tm <- data.frame(
    series = c("s1", "s2", "s3"),
    trend = c(1, -1, 2)
  )
  expect_error(
    mvgam:::normalise_trend_map(tm, .make_data(3L)),
    "positive integers"
  )
})

test_that("data.frame with max(trend) > n_series errors", {
  tm <- data.frame(
    series = c("s1", "s2", "s3"),
    trend = c(1, 2, 4)
  )
  expect_error(
    mvgam:::normalise_trend_map(tm, .make_data(3L)),
    "contiguous"
  )
})


# ---- Non-supported input shapes --------------------------

test_that("integer-vector input is rejected (no implicit ordering)", {
  expect_error(
    mvgam:::normalise_trend_map(c(1L, 1L, 2L), .make_data(3L)),
    "matrix, data.frame or character"
  )
})

test_that("list input is rejected", {
  expect_error(
    mvgam:::normalise_trend_map(list(trend1 = "s1", trend2 = "s2"),
                                .make_data(3L)),
    "matrix, data.frame or character"
  )
})


# ---- Data without a series column ------------------------

test_that("missing series column on data errors", {
  d <- data.frame(y = 1:3, time = 1:3)
  expect_error(
    mvgam:::normalise_trend_map("identity", d),
    "requires a 'series'"
  )
})


# ---- Constructor surface (step 2): arg acceptance + stash ----

test_that("AR / VAR / RW / ZMVN accept trend_map and stash on spec", {
  tm <- data.frame(series = c("s1", "s2"), trend = c(1, 1))
  Zmat <- matrix(c(1, 0.5, 0.5, 1), nrow = 2L, ncol = 2L)
  for (ctor in list(
    function() AR(trend_map = tm),
    function() VAR(trend_map = "shared"),
    function() RW(trend_map = Zmat),
    function() ZMVN(trend_map = "identity")
  )) {
    spec <- ctor()
    expect_true(inherits(spec, "mvgam_trend"))
    expect_true("trend_map" %in% names(spec))
    expect_false(is.null(spec$trend_map))
  }
})

test_that("trend_map = NULL stashes NULL (default)", {
  spec <- AR()
  expect_true("trend_map" %in% names(spec))
  expect_null(spec$trend_map)
})

test_that("PW refuses a factor request by either argument", {
  # Both arguments raise one request, so both meet one refusal,
  # composed from the reason the registry records against PW.
  by_map <- expect_error(
    PW(trend_map = "identity"),
    "Factor models are not supported for PW trends"
  )
  by_n_lv <- expect_error(
    PW(n_lv = 2),
    "Factor models are not supported for PW trends"
  )
  expect_identical(conditionMessage(by_map), conditionMessage(by_n_lv))
  expect_match(conditionMessage(by_n_lv), "changepoint modeling",
               fixed = TRUE)
})

test_that("constructors fail-fast on malformed trend_map shapes", {
  # Numeric scalar (not matrix / df / single string) is rejected
  # at constructor time, not deferred to fit time.
  expect_error(AR(trend_map = 123), "trend_map")
  expect_error(RW(trend_map = list(1)), "trend_map")
  expect_error(VAR(trend_map = TRUE), "trend_map")
  expect_error(ZMVN(trend_map = 1:3), "trend_map")
})


# ---- Top-level alias on mvgam() ------------------------

test_that("mvgam() exposes trend_map in its signature", {
  args <- formals(mvgam)
  expect_true("trend_map" %in% names(args))
})

test_that("apply_trend_map_alias is a no-op when alias is NULL", {
  spec <- AR()
  out <- mvgam:::apply_trend_map_alias(spec, NULL)
  expect_null(out$trend_map)
})

test_that("apply_trend_map_alias grafts onto a single spec", {
  spec <- AR()  # constructor-level NULL
  out <- mvgam:::apply_trend_map_alias(spec, "identity")
  expect_equal(out$trend_map, "identity")
})

test_that("apply_trend_map_alias errors on collision", {
  spec <- AR(trend_map = "shared")  # constructor-level set
  expect_error(
    mvgam:::apply_trend_map_alias(spec, "identity"),
    "both"
  )
})

test_that("apply_trend_map_alias preserves multivariate structure", {
  specs <- list(
    y1 = AR(),
    y2 = RW()
  )
  out <- mvgam:::apply_trend_map_alias(specs, "shared")
  expect_equal(names(out), c("y1", "y2"))
  expect_equal(out$y1$trend_map, "shared")
  expect_equal(out$y2$trend_map, "shared")
  expect_s3_class(out$y1, "mvgam_trend")
  expect_s3_class(out$y2, "mvgam_trend")
})

test_that("normalise_trend_map_on_specs rejects mismatched multivariate Zs", {
  # mvgam uses one shared trend component, so multivariate specs
  # must agree on trend_map. Different values across responses
  # would otherwise be silently dropped downstream.
  data <- data.frame(
    y1 = 1:8, y2 = 1:8,
    time = rep(1:4, 2L),
    series = factor(rep(c("s1", "s2"), each = 4L))
  )
  specs <- list(
    y1 = AR(trend_map = "identity"),
    y2 = AR(trend_map = "shared")
  )
  expect_error(
    mvgam:::normalise_trend_map_on_specs(specs, data),
    "differs across multivariate"
  )
})

test_that("normalise_trend_map_on_specs accepts matching multivariate Zs", {
  data <- data.frame(
    y1 = 1:8, y2 = 1:8,
    time = rep(1:4, 2L),
    series = factor(rep(c("s1", "s2"), each = 4L))
  )
  specs <- list(
    y1 = AR(trend_map = "shared"),
    y2 = AR(trend_map = "shared")
  )
  out <- mvgam:::normalise_trend_map_on_specs(specs, data)
  expect_false(is.null(out$y1$fixed_Z))
  expect_false(is.null(out$y2$fixed_Z))
  expect_equal(out$y1$fixed_Z, out$y2$fixed_Z)
})

test_that("apply_trend_map_alias multivariate collision names the spec", {
  specs <- list(
    y1 = AR(trend_map = "shared"),
    y2 = RW()
  )
  expect_error(
    mvgam:::apply_trend_map_alias(specs, "identity"),
    "y1"
  )
})

test_that("mvgam() forwards trend_map through to generate_stan_components", {
  # Verify wiring without invoking the Stan compile path. The
  # mock captures whatever value `mvgam_single` receives so we can
  # confirm the alias survives the call chain.
  data_train <- data.frame(
    y = 1:6,
    time = rep(1:3, 2L),
    series = factor(rep(c("s1", "s2"), each = 3L))
  )
  captured <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    mvgam_single = function(formula, trend_formula, data, backend,
                            family, data_name = NULL,
                            newdata = NULL, trend_map = NULL,
                            ...) {
      captured$trend_map <- trend_map
      structure(list(data = data),
                 class = c("mvgam", "brmsfit"))
    }
  )
  mvgam(
    y ~ 1,
    data = data_train,
    trend_map = "shared",
    family = gaussian()
  )
  expect_equal(captured$trend_map, "shared")
})
