# Unit tests for the trait-overlay helpers added to ordinate(). These
# do not fit a Stan model; the helpers are pure linear algebra on the
# rotated species loadings, so a synthetic Z matrix plus a per-species
# trait frame is enough to lock the contract.

test_that("ordinate_trait_arrows() returns NULL when traits is NULL", {
  Z <- matrix(rnorm(10L), nrow = 5L, ncol = 2L)
  out <- mvgam:::ordinate_trait_arrows(
    traits = NULL, loadings_2d = Z,
    species_names = paste0("sp", 1:5),
    arrow_scale = 1
  )
  expect_null(out)
})

test_that("ordinate_trait_arrows() returns NULL when no numeric traits remain", {
  Z <- matrix(rnorm(10L), nrow = 5L, ncol = 2L)
  traits <- data.frame(
    diet = factor(c("herb", "carn", "herb", "omni", "carn"))
  )
  out <- mvgam:::ordinate_trait_arrows(
    traits = traits, loadings_2d = Z,
    species_names = paste0("sp", 1:5),
    arrow_scale = 1
  )
  expect_null(out)
})

test_that("ordinate_trait_arrows() returns one row per numeric trait", {
  set.seed(7L)
  Z <- matrix(rnorm(10L), nrow = 5L, ncol = 2L)
  traits <- data.frame(
    body_size = c(1.0, 2.5, 4.0, 3.0, 1.5),
    fecundity = c(50, 30, 10, 25, 60)
  )
  out <- mvgam:::ordinate_trait_arrows(
    traits = traits, loadings_2d = Z,
    species_names = paste0("sp", 1:5),
    arrow_scale = 1
  )
  expect_s3_class(out, "data.frame")
  expect_equal(NROW(out), 2L)
  expect_named(out, c("x", "y", "trait_name"))
  expect_setequal(out$trait_name, c("body_size", "fecundity"))
})

test_that("ordinate_trait_arrows() aligns rows via rownames when set", {
  set.seed(11L)
  sp <- c("rare", "common", "intermediate", "edge", "core")
  Z <- matrix(rnorm(10L), nrow = 5L, ncol = 2L)
  rownames(Z) <- sp
  # Traits supplied in a deliberately scrambled order.
  traits <- data.frame(
    body_size = c(5, 2, 4, 3, 1)
  )
  rownames(traits) <- c("core", "rare", "edge", "intermediate", "common")
  out <- mvgam:::ordinate_trait_arrows(
    traits = traits, loadings_2d = Z,
    species_names = sp, arrow_scale = 1
  )
  # Caller's species_names order is what gets used; the helper aligns
  # internally. Arrow should be non-NULL with one row for body_size.
  expect_equal(NROW(out), 1L)
  expect_equal(out$trait_name, "body_size")
})

test_that("ordinate_trait_arrows() errors when row count mismatches", {
  Z <- matrix(rnorm(10L), nrow = 5L, ncol = 2L)
  traits <- data.frame(body_size = c(1, 2, 3))  # 3 != 5
  expect_error(
    mvgam:::ordinate_trait_arrows(
      traits = traits, loadings_2d = Z,
      species_names = paste0("sp", 1:5),
      arrow_scale = 1
    ),
    "Trait row count does not match species count"
  )
})

test_that("ordinate_trait_arrows() errors when some species are missing", {
  Z <- matrix(rnorm(10L), nrow = 5L, ncol = 2L)
  traits <- data.frame(body_size = c(1, 2, 3, 4, 5))
  rownames(traits) <- paste0("sp", 1:5)
  expect_error(
    mvgam:::ordinate_trait_arrows(
      traits = traits, loadings_2d = Z,
      species_names = c("sp1", "sp2", "sp3", "missing", "sp5"),
      arrow_scale = 1
    ),
    "Some species are missing from 'traits' rownames"
  )
})

test_that("ordinate_trait_arrows() errors on non-data input", {
  Z <- matrix(rnorm(10L), nrow = 5L, ncol = 2L)
  expect_error(
    mvgam:::ordinate_trait_arrows(
      traits = "not a data frame", loadings_2d = Z,
      species_names = paste0("sp", 1:5),
      arrow_scale = 1
    ),
    "must be a data.frame or matrix"
  )
})

test_that("ordinate_trait_arrows() respects trait_arrow_scale", {
  set.seed(3L)
  Z <- matrix(rnorm(10L), nrow = 5L, ncol = 2L)
  traits <- data.frame(body_size = c(1, 5, 2, 4, 3))
  out_one <- mvgam:::ordinate_trait_arrows(
    traits = traits, loadings_2d = Z,
    species_names = paste0("sp", 1:5),
    arrow_scale = 1
  )
  out_two <- mvgam:::ordinate_trait_arrows(
    traits = traits, loadings_2d = Z,
    species_names = paste0("sp", 1:5),
    arrow_scale = 2
  )
  r_one <- sqrt(out_one$x^2 + out_one$y^2)
  r_two <- sqrt(out_two$x^2 + out_two$y^2)
  expect_equal(r_two / r_one, rep(2, length(r_one)), tolerance = 1e-8)
})

test_that("ordinate_trait_layers() returns NULL when trait_dat is NULL", {
  expect_null(mvgam:::ordinate_trait_layers(NULL))
})

test_that("ordinate_trait_layers() returns layer list when trait_dat supplied", {
  trait_dat <- data.frame(x = 0.5, y = -0.3, trait_name = "body_size")
  layers <- mvgam:::ordinate_trait_layers(trait_dat)
  expect_type(layers, "list")
  # geom_segment + repel label layer.
  expect_true(length(layers) >= 2L)
})

test_that("ordinate_extract_fit_traits() finds traits on a single-trend fit", {
  features <- data.frame(body_size = c(1, 2, 3), row.names = paste0("sp", 1:3))
  spec <- structure(
    list(loadings_prior = list(features = features)),
    class = "mvgam_trend"
  )
  stub_fit <- list(mv_spec = list(trend_specs = spec))
  out <- mvgam:::ordinate_extract_fit_traits(stub_fit)
  expect_equal(out, features)
})

test_that("ordinate_extract_fit_traits() finds traits on a multi-trend fit", {
  features <- data.frame(body_size = c(1, 2, 3), row.names = paste0("sp", 1:3))
  spec1 <- structure(list(), class = "mvgam_trend")
  spec2 <- structure(
    list(loadings_prior = list(features = features)),
    class = "mvgam_trend"
  )
  stub_fit <- list(mv_spec = list(trend_specs = list(spec1, spec2)))
  out <- mvgam:::ordinate_extract_fit_traits(stub_fit)
  expect_equal(out, features)
})

test_that("ordinate_extract_fit_traits() returns NULL when no traits", {
  spec <- structure(list(), class = "mvgam_trend")
  stub_fit <- list(mv_spec = list(trend_specs = spec))
  expect_null(mvgam:::ordinate_extract_fit_traits(stub_fit))
})

test_that("resolve_auto_traits() passes NULL through", {
  stub_fit <- list()
  expect_null(mvgam:::resolve_auto_traits(NULL, stub_fit))
})

test_that("resolve_auto_traits() passes data.frame through unchanged", {
  traits <- data.frame(body_size = c(1, 2, 3))
  stub_fit <- list()
  expect_identical(mvgam:::resolve_auto_traits(traits, stub_fit), traits)
})

test_that("resolve_auto_traits('auto') pulls from a trait-informed fit", {
  features <- data.frame(body_size = c(1, 2), row.names = c("sp1", "sp2"))
  spec <- structure(
    list(loadings_prior = list(features = features)),
    class = "mvgam_trend"
  )
  stub_fit <- list(mv_spec = list(trend_specs = spec))
  out <- mvgam:::resolve_auto_traits("auto", stub_fit)
  expect_equal(out, features)
})

test_that("resolve_auto_traits('auto') returns NULL on naive fit", {
  withr::local_envvar(TESTTHAT = "true")
  spec <- structure(list(), class = "mvgam_trend")
  stub_fit <- list(mv_spec = list(trend_specs = spec))
  expect_null(mvgam:::resolve_auto_traits("auto", stub_fit))
})

test_that("resolve_auto_traits() errors on unknown string", {
  stub_fit <- list()
  expect_error(
    mvgam:::resolve_auto_traits("nonsense", stub_fit),
    "string value must be 'auto'"
  )
})
