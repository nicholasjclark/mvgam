make_test_data <- function(n_series = 4L) {
  series_levels <- paste0("s", seq_len(n_series))
  data.frame(
    series = factor(rep(series_levels, each = 10L)),
    time = rep(1:10, n_series),
    y = rpois(n_series * 10L, 1)
  )
}

test_that("normalise_loadings_prior returns NULL when input is NULL", {
  data <- make_test_data()
  expect_null(normalise_loadings_prior(NULL, data2 = NULL, data = data))
})

test_that("normalise_loadings_prior resolves features from data2", {
  data <- make_test_data(3L)
  features <- data.frame(series = c("s1", "s2", "s3"), x = c(1, 2, 3))
  spec <- normalise_loadings_prior(
    list(features = "row_features"),
    data2 = list(row_features = features),
    data = data
  )
  expect_equal(spec$N_features_trend, 1L)
  expect_equal(spec$n_distances, 0L)
  expect_equal(rownames(spec$features_mat), c("s1", "s2", "s3"))
})

test_that("normalise_loadings_prior accepts inline matrix for features", {
  data <- make_test_data(3L)
  mat <- matrix(c(1, 2, 3), ncol = 1L,
                dimnames = list(c("s1", "s2", "s3"), "x"))
  spec <- normalise_loadings_prior(
    list(features = mat), data2 = NULL, data = data
  )
  expect_equal(spec$N_features_trend, 1L)
})

test_that("normalise_loadings_prior errors on missing data2 lookup", {
  data <- make_test_data(3L)
  expect_error(
    normalise_loadings_prior(
      list(features = "nope"), data2 = list(other = 1), data = data
    ),
    "not found in 'data2'"
  )
})

test_that("normalise_loadings_prior handles a single distance matrix inline", {
  data <- make_test_data(3L)
  d <- matrix(c(0, 1, 2, 1, 0, 3, 2, 3, 0), nrow = 3L)
  spec <- normalise_loadings_prior(
    list(distances = d), data2 = NULL, data = data
  )
  expect_equal(spec$n_distances, 1L)
  expect_equal(names(spec$distance_mats), "dist_1")
  expect_equal(max(spec$distance_mats[["dist_1"]]), 1)
})

test_that("normalise_loadings_prior handles a named list of distances", {
  data <- make_test_data(3L)
  d1 <- matrix(c(0, 1, 2, 1, 0, 3, 2, 3, 0), nrow = 3L)
  d2 <- matrix(c(0, 5, 6, 5, 0, 7, 6, 7, 0), nrow = 3L)
  spec <- normalise_loadings_prior(
    list(distances = list(phylo = d1, geo = d2)),
    data2 = NULL, data = data
  )
  expect_equal(spec$n_distances, 2L)
  expect_equal(names(spec$distance_mats), c("phylo", "geo"))
})

test_that("normalise_loadings_prior resolves distance names in data2", {
  data <- make_test_data(3L)
  d1 <- matrix(c(0, 1, 2, 1, 0, 3, 2, 3, 0), nrow = 3L)
  d2 <- matrix(c(0, 5, 6, 5, 0, 7, 6, 7, 0), nrow = 3L)
  spec <- normalise_loadings_prior(
    list(distances = c("phylo", "geo")),
    data2 = list(phylo = d1, geo = d2),
    data = data
  )
  expect_equal(spec$n_distances, 2L)
})

test_that("normalise_loadings_prior errors on unknown spec fields", {
  data <- make_test_data(3L)
  expect_error(
    normalise_loadings_prior(
      list(typo = 1, features = matrix(1:3, ncol = 1L)),
      data2 = NULL, data = data
    ),
    "Unrecognised: 'typo'"
  )
})

test_that("normalise_loadings_prior refuses an empty spec", {
  data <- make_test_data(3L)
  expect_error(
    normalise_loadings_prior(
      list(), data2 = NULL, data = data
    ),
    "at least one of"
  )
})

test_that("normalise_loadings_prior accepts pure MGP, also as a string", {
  data <- make_test_data(4L)
  spec <- normalise_loadings_prior(
    list(column_shrinkage = "mgp"), data2 = NULL, data = data
  )
  expect_null(spec$features_mat)
  expect_identical(spec$N_features_trend, 0L)
  expect_identical(spec$n_distances, 0L)
  expect_equal(spec$mgp_a1, 2)
  expect_equal(spec$mgp_a2, 4)
  expect_identical(normalise_loadings_prior("mgp", data2 = NULL, data = data),
                   spec)
})


test_that("normalise_loadings_prior respects user-supplied mgp_a1/a2", {
  data <- make_test_data(3L)
  d <- matrix(c(0, 1, 2, 1, 0, 3, 2, 3, 0), nrow = 3L)
  spec <- normalise_loadings_prior(
    list(distances = d, column_shrinkage = "mgp",
         mgp_a1 = 1, mgp_a2 = 5),
    data2 = NULL, data = data
  )
  expect_equal(spec$mgp_a1, 1)
  expect_equal(spec$mgp_a2, 5)
})

test_that("MGP hyperparameters require MGP column shrinkage", {
  data <- make_test_data(3L)
  d <- matrix(c(0, 1, 2, 1, 0, 3, 2, 3, 0), nrow = 3L)
  expect_error(
    normalise_loadings_prior(
      list(distances = d, mgp_a1 = 1),
      data2 = NULL, data = data
    ),
    "'mgp_a1' / 'mgp_a2' supplied"
  )
})

test_that("normalise_loadings_prior errors on invalid column_shrinkage", {
  data <- make_test_data(3L)
  d <- matrix(c(0, 1, 2, 1, 0, 3, 2, 3, 0), nrow = 3L)
  expect_error(
    normalise_loadings_prior(
      list(distances = d, column_shrinkage = "horseshoe"),
      data2 = NULL, data = data
    )
  )
})

test_that("normalise_loadings_prior refuses a misshapen distance matrix", {
  data <- make_test_data(3L)
  d <- matrix(0, nrow = 2L, ncol = 2L)
  expect_error(
    normalise_loadings_prior(
      list(distances = d), data2 = NULL, data = data
    ),
    "wrong shape"
  )
})

test_that("normalise_loadings_prior returns the expected struct shape", {
  data <- make_test_data(3L)
  feats <- data.frame(series = c("s1", "s2", "s3"), x = c(1, 2, 3))
  d <- matrix(c(0, 1, 2, 1, 0, 3, 2, 3, 0), nrow = 3L)
  spec <- normalise_loadings_prior(
    list(features = feats, distances = list(phylo = d)),
    data2 = NULL, data = data
  )
  expect_named(spec, c("features_mat", "distance_mats",
                       "column_shrinkage", "mgp_a1", "mgp_a2",
                       "n_series", "N_features_trend", "n_distances"))
  expect_equal(spec$n_series, 3L)
  expect_equal(spec$N_features_trend, 1L)
  expect_equal(spec$n_distances, 1L)
  expect_equal(spec$column_shrinkage, "iid")
})

test_that("assert_loadings_prior_compatible accepts NULL on either side", {
  expect_null(assert_loadings_prior_compatible(NULL, NULL))
  expect_null(assert_loadings_prior_compatible(list(), NULL))
  expect_null(assert_loadings_prior_compatible(
    NULL, matrix(NA_real_, 3L, 2L)
  ))
})

test_that("assert_loadings_prior_compatible errors on partial trend_map", {
  expect_error(
    assert_loadings_prior_compatible(
      list(features_mat = matrix(0, 3L, 1L)),
      matrix(c(1, NA, 0, NA, 1, 0), nrow = 3L)
    ),
    "fixes 4 of 6 entries"
  )
})

test_that("assert_loadings_prior_compatible errors on fully-fixed trend_map", {
  expect_error(
    assert_loadings_prior_compatible(
      list(features_mat = matrix(0, 3L, 1L)),
      matrix(c(1, 0, 0, 0, 1, 0), nrow = 3L)
    ),
    "fixes 6 of 6 entries"
  )
})

test_that("assert_loadings_prior_spec_consistent rejects a corrupt spec", {
  expect_error(
    assert_loadings_prior_spec_consistent(list(features_mat = NULL)),
    "lacks required fields"
  )
  spec <- list(
    features_mat = matrix(0, 4L, 2L), distance_mats = list(),
    column_shrinkage = "iid", mgp_a1 = NA, mgp_a2 = NA,
    n_series = 4L, N_features_trend = 5L, n_distances = 0L
  )
  expect_error(assert_loadings_prior_spec_consistent(spec),
               "feature matrix has the wrong shape")
  spec$N_features_trend <- 2L
  spec$n_distances <- 1L
  expect_error(assert_loadings_prior_spec_consistent(spec),
               "distance list has the wrong shape")
})

test_that("assert_distance_names_unreserved accepts unreserved names", {
  expect_null(
    assert_distance_names_unreserved(c("phylo", "geo", "habitat"))
  )
})

test_that("normalise_loadings_prior errors on reserved 'dist_' name", {
  data <- make_test_data(3L)
  d <- matrix(c(0, 1, 2, 1, 0, 3, 2, 3, 0), nrow = 3L)
  expect_error(
    normalise_loadings_prior(
      list(distances = list(dist_phylo = d)),
      data2 = NULL, data = data
    ),
    "cannot start with 'dist_'"
  )
})
