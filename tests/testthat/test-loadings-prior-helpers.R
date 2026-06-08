test_that("encode_loadings_features handles numeric column", {
  feats <- data.frame(
    series = c("a", "b", "c"),
    x = c(1, 2, 3)
  )
  out <- encode_loadings_features(feats, c("a", "b", "c"))
  expect_equal(rownames(out), c("a", "b", "c"))
  expect_equal(colnames(out), "x")
  expected <- (c(1, 2, 3) - 2) / sd(c(1, 2, 3))
  expect_equal(as.numeric(out[, "x"]), expected)
})

test_that("encode_loadings_features one-hots an unordered factor with all levels", {
  feats <- data.frame(
    series = c("a", "b", "c"),
    grp = factor(c("x", "y", "z"))
  )
  out <- encode_loadings_features(feats, c("a", "b", "c"))
  expect_equal(colnames(out), c("grp.x", "grp.y", "grp.z"))
  expect_equal(unname(out[, "grp.x"]), c(1, 0, 0))
  expect_equal(unname(out[, "grp.y"]), c(0, 1, 0))
  expect_equal(unname(out[, "grp.z"]), c(0, 0, 1))
})

test_that("encode_loadings_features z-scores ordered factors", {
  feats <- data.frame(
    series = c("a", "b", "c"),
    rank = ordered(c("lo", "med", "hi"), levels = c("lo", "med", "hi"))
  )
  out <- encode_loadings_features(feats, c("a", "b", "c"))
  expected <- (c(1, 2, 3) - 2) / sd(c(1, 2, 3))
  expect_equal(as.numeric(out[, "rank"]), expected)
})

test_that("encode_loadings_features coerces character to factor", {
  feats <- data.frame(
    series = c("a", "b", "c"),
    grp = c("x", "y", "x"),
    stringsAsFactors = FALSE
  )
  out <- encode_loadings_features(feats, c("a", "b", "c"))
  expect_equal(colnames(out), c("grp.x", "grp.y"))
})

test_that("encode_loadings_features reorders by series column", {
  feats <- data.frame(
    series = c("c", "a", "b"),
    x = c(3, 1, 2)
  )
  out <- encode_loadings_features(feats, c("a", "b", "c"))
  expect_equal(rownames(out), c("a", "b", "c"))
  expect_equal(unname(out[, "x"]), (c(1, 2, 3) - 2) / sd(c(1, 2, 3)))
})

test_that("encode_loadings_features reorders by rownames", {
  feats <- matrix(
    c(3, 1, 2), ncol = 1L,
    dimnames = list(c("c", "a", "b"), "x")
  )
  out <- encode_loadings_features(feats, c("a", "b", "c"))
  expect_equal(rownames(out), c("a", "b", "c"))
  expect_equal(unname(out[, "x"]), (c(1, 2, 3) - 2) / sd(c(1, 2, 3)))
})

test_that("encode_loadings_features errors on mismatched series labels", {
  feats <- data.frame(
    series = c("a", "b", "z"),
    x = c(1, 2, 3)
  )
  expect_error(
    encode_loadings_features(feats, c("a", "b", "c")),
    "must list every training series"
  )
})

test_that("encode_loadings_features errors on duplicate series", {
  feats <- data.frame(
    series = c("a", "a", "b"),
    x = c(1, 2, 3)
  )
  expect_error(
    encode_loadings_features(feats, c("a", "b")),
    "must list every training series exactly once|duplicate"
  )
})

test_that("encode_loadings_features errors on wrong row count without alignment hints", {
  feats <- data.frame(x = c(1, 2))
  expect_error(
    encode_loadings_features(feats, c("a", "b", "c")),
    "Cannot align"
  )
})

test_that("encode_loadings_features errors on all-NA column", {
  feats <- data.frame(
    series = c("a", "b", "c"),
    x = c(NA_real_, NA_real_, NA_real_)
  )
  expect_error(
    encode_loadings_features(feats, c("a", "b", "c")),
    "entirely NA"
  )
})

test_that("encode_loadings_features errors on partial NA after encoding", {
  feats <- data.frame(
    series = c("a", "b", "c"),
    x = c(1, NA, 3)
  )
  expect_error(
    encode_loadings_features(feats, c("a", "b", "c")),
    "contains NA values"
  )
})

test_that("encode_loadings_features keeps zero-SD numeric as zero column", {
  feats <- data.frame(
    series = c("a", "b", "c"),
    flat = c(2, 2, 2)
  )
  out <- encode_loadings_features(feats, c("a", "b", "c"))
  expect_equal(as.numeric(out[, "flat"]), c(0, 0, 0))
})

test_that("encode_loadings_features errors on unsupported column type", {
  feats <- data.frame(
    series = c("a", "b"),
    ts = as.POSIXct(c("2024-01-01", "2024-01-02"))
  )
  expect_error(
    encode_loadings_features(feats, c("a", "b")),
    "unsupported type"
  )
})

test_that("validate_pairwise_distance accepts a valid matrix", {
  d <- matrix(c(0, 1, 2, 1, 0, 3, 2, 3, 0), nrow = 3L)
  out <- validate_pairwise_distance(d, n_series = 3L, name = "phylo",
                                    standardise = FALSE)
  expect_equal(out, d)
})

test_that("validate_pairwise_distance rejects wrong shape", {
  d <- matrix(0, nrow = 2L, ncol = 3L)
  expect_error(
    validate_pairwise_distance(d, n_series = 3L, name = "phylo"),
    "wrong shape"
  )
})

test_that("validate_pairwise_distance rejects non-zero diagonal", {
  d <- matrix(c(1, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 3L)
  expect_error(
    validate_pairwise_distance(d, n_series = 3L, name = "phylo"),
    "non-zero diagonal"
  )
})

test_that("validate_pairwise_distance rejects asymmetric matrix", {
  d <- matrix(c(0, 1, 2, 5, 0, 3, 2, 3, 0), nrow = 3L)
  expect_error(
    validate_pairwise_distance(d, n_series = 3L, name = "phylo"),
    "not symmetric"
  )
})

test_that("validate_pairwise_distance rejects negative entries", {
  d <- matrix(c(0, -1, 2, -1, 0, 3, 2, 3, 0), nrow = 3L)
  expect_error(
    validate_pairwise_distance(d, n_series = 3L, name = "phylo"),
    "negative entries"
  )
})

test_that("validate_pairwise_distance rejects NA entries", {
  d <- matrix(c(0, NA, 2, NA, 0, 3, 2, 3, 0), nrow = 3L)
  expect_error(
    validate_pairwise_distance(d, n_series = 3L, name = "phylo"),
    "NA"
  )
})

test_that("validate_pairwise_distance reorders by rownames", {
  d <- matrix(c(0, 1, 2, 1, 0, 3, 2, 3, 0), nrow = 3L)
  rownames(d) <- colnames(d) <- c("c", "a", "b")
  out <- validate_pairwise_distance(
    d, n_series = 3L, name = "phylo",
    series_levels = c("a", "b", "c"), standardise = FALSE
  )
  expect_equal(rownames(out), c("a", "b", "c"))
  expect_equal(colnames(out), c("a", "b", "c"))
  expect_equal(out["a", "c"], 1)
  expect_equal(out["b", "c"], 2)
})

test_that("validate_pairwise_distance standardises to max-distance one by default", {
  d <- matrix(c(0, 2, 4, 2, 0, 6, 4, 6, 0), nrow = 3L)
  out <- validate_pairwise_distance(d, n_series = 3L, name = "phylo")
  expect_equal(max(out), 1)
  expect_equal(out, d / max(d))
})

test_that("validate_pairwise_distance respects standardise = FALSE", {
  d <- matrix(c(0, 2, 4, 2, 0, 6, 4, 6, 0), nrow = 3L)
  out <- validate_pairwise_distance(d, n_series = 3L, name = "phylo",
                                    standardise = FALSE)
  expect_equal(max(out), 6)
})

test_that("length_scale_collinearity_warning flags identical distances", {
  features_mat <- cbind(x = c(1, 2, 3, 4))
  d <- as.matrix(dist(c(1, 2, 3, 4)))
  Sys.unsetenv("TESTTHAT")
  on.exit(Sys.setenv(TESTTHAT = "true"), add = TRUE)
  expect_warning(
    length_scale_collinearity_warning(features_mat, list(phylo = d)),
    "nearly colinear"
  )
})

test_that("length_scale_collinearity_warning is silent under TESTTHAT", {
  features_mat <- cbind(x = c(1, 2, 3, 4))
  d <- as.matrix(dist(c(1, 2, 3, 4)))
  expect_silent(
    length_scale_collinearity_warning(features_mat, list(phylo = d))
  )
})

test_that("length_scale_collinearity_warning is silent without high correlation", {
  features_mat <- cbind(x = c(1, 5, 2, 9))
  d <- as.matrix(dist(c(1, 1, 1, 2)))
  Sys.unsetenv("TESTTHAT")
  on.exit(Sys.setenv(TESTTHAT = "true"), add = TRUE)
  expect_silent(
    length_scale_collinearity_warning(features_mat, list(phylo = d))
  )
})

test_that("imbalance_warning flags dominant one-hot", {
  mat <- cbind(rare = c(rep(0, 19), 1), other = c(rep(1, 19), 0))
  Sys.unsetenv("TESTTHAT")
  on.exit(Sys.setenv(TESTTHAT = "true"), add = TRUE)
  expect_warning(
    imbalance_warning(mat, threshold = 0.9),
    "dominated by a single level"
  )
})

test_that("imbalance_warning ignores non-binary columns", {
  mat <- cbind(numeric_col = (1:10) / 10)
  Sys.unsetenv("TESTTHAT")
  on.exit(Sys.setenv(TESTTHAT = "true"), add = TRUE)
  expect_silent(imbalance_warning(mat))
})
