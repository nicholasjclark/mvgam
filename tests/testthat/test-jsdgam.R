# Tests for the `jsdgam()` wrapper (chunk 1: skeleton + legacy
# signature port + class + slot plumbing). Three layers:
#   1. Argument validation: 'unit' / 'species' / 'n_lv' / 'data'.
#   2. Class + slot plumbing: c("mvgam", "jsdgam"); is_jsdgam flag;
#      prepped_trend_model attribute populated correctly.
#   3. Forward-compat smoke test: a minimal jsdgam call composes
#      cleanly with the brms-integration mvgam() pipeline.

# Utility -----------------------------------------------------------------

build_jsdgam_toy <- function(n_time = 24L, n_species = 4L, seed = 42L) {
  set.seed(seed)
  dat <- expand.grid(
    time = seq_len(n_time),
    species = paste0("sp", seq_len(n_species))
  )
  dat$species <- factor(dat$species)
  dat$elev <- rep(rnorm(n_time), times = n_species)
  dat$y <- rpois(nrow(dat), lambda = 2)
  dat
}

# 1. Validation ------------------------------------------------------------

test_that("jsdgam errors when 'species' column is missing", {
  dat <- build_jsdgam_toy()
  # Default `species = series` but the data only has `species`.
  expect_error(
    jsdgam(
      formula = y ~ 1, factor_formula = ~ -1,
      data = dat, family = poisson(), n_lv = 2L,
      run_model = FALSE, silent = 2
    ),
    "must.+include"
  )
})

test_that("jsdgam rejects n_lv >= number of species", {
  dat <- build_jsdgam_toy()
  expect_error(
    jsdgam(
      formula = y ~ 1, factor_formula = ~ -1,
      data = dat, species = species,
      family = poisson(), n_lv = 4L,
      run_model = FALSE, silent = 2
    ),
    "strictly less than the number of species"
  )
})

test_that("jsdgam rejects n_lv = 0", {
  dat <- build_jsdgam_toy()
  expect_error(
    jsdgam(
      formula = y ~ 1, factor_formula = ~ -1,
      data = dat, species = species,
      family = poisson(), n_lv = 0L,
      run_model = FALSE, silent = 2
    ),
    "not >= 1"
  )
})

test_that("jsdgam rejects non-numeric unit column", {
  dat <- build_jsdgam_toy()
  dat$site_name <- paste0("site", dat$time)
  expect_error(
    jsdgam(
      formula = y ~ 1, factor_formula = ~ -1,
      data = dat, unit = site_name, species = species,
      family = poisson(), n_lv = 2L,
      run_model = FALSE, silent = 2
    ),
    "numeric or integer"
  )
})

test_that("jsdgam rejects single-species data", {
  dat <- build_jsdgam_toy(n_species = 1L)
  expect_error(
    jsdgam(
      formula = y ~ 1, factor_formula = ~ -1,
      data = dat, species = species,
      family = poisson(), n_lv = 2L,
      run_model = FALSE, silent = 2
    ),
    "requires at least 2 species"
  )
})

test_that("jsdgam refuses to overwrite existing 'time' column", {
  dat <- build_jsdgam_toy()
  dat$site <- dat$time
  # User asks `unit = site` but `time` is already present.
  expect_error(
    jsdgam(
      formula = y ~ 1, factor_formula = ~ -1,
      data = dat, unit = site, species = species,
      family = poisson(), n_lv = 2L,
      run_model = FALSE, silent = 2
    ),
    "already contains a 'time' column"
  )
})

test_that("jsdgam refuses to overwrite existing 'series' column", {
  dat <- build_jsdgam_toy()
  dat$series <- dat$species  # user has both `series` and `species`
  expect_error(
    jsdgam(
      formula = y ~ 1, factor_formula = ~ -1,
      data = dat, unit = time, species = species,
      family = poisson(), n_lv = 2L,
      run_model = FALSE, silent = 2
    ),
    "already contains a 'series' column"
  )
})

# 2. Class + slot plumbing -------------------------------------------------

test_that("jsdgam returns c('mvgam', 'jsdgam') and the metadata slots", {
  dat <- build_jsdgam_toy()
  mod <- jsdgam(
    formula = y ~ 1, factor_formula = ~ -1,
    data = dat, unit = time, species = species,
    family = poisson(), n_lv = 2L,
    run_model = FALSE, silent = 2
  )
  expect_s3_class(mod, "jsdgam")
  expect_s3_class(mod, "mvgam")
  expect_identical(class(mod)[1L:2L], c("mvgam", "jsdgam"))
  expect_true(isTRUE(mod$model_spec$is_jsdgam))

  prepped <- attr(mod$model_data, "prepped_trend_model")
  expect_type(prepped, "list")
  expect_identical(prepped$unit, "time")
  expect_identical(prepped$species, "species")

  expect_true(!is.null(mod$obs_data))
  expect_true(!is.null(mod$model_data))
})

test_that("jsdgam preserves the unit column name on prepped_trend_model", {
  dat <- build_jsdgam_toy()
  dat$site <- dat$time
  dat$time <- NULL
  mod <- jsdgam(
    formula = y ~ 1, factor_formula = ~ -1,
    data = dat, unit = site, species = species,
    family = poisson(), n_lv = 2L,
    run_model = FALSE, silent = 2
  )
  expect_identical(
    attr(mod$model_data, "prepped_trend_model")$unit,
    "site"
  )
  expect_true("time" %in% names(mod$model_data))
})

# 3. Forward-compat composition --------------------------------------------

test_that("jsdgam composes with the standard mvgam pipeline", {
  dat <- build_jsdgam_toy()
  mod <- jsdgam(
    formula = y ~ 1, factor_formula = ~ -1,
    data = dat, unit = time, species = species,
    family = poisson(), n_lv = 2L,
    run_model = FALSE, silent = 2
  )
  expect_true(!is.null(mod$fit))
  expect_true(!is.null(mod$standata))
  expect_true(!is.null(mod$stancode))
  expect_equal(mod$standata$N_lv_trend, 2L)
  expect_equal(mod$standata$N_series_trend, 4L)
})

test_that("jsdgam with by = lv_axis() composes the per-factor smooth path", {
  dat <- build_jsdgam_toy(n_time = 60L)
  mod <- jsdgam(
    formula = y ~ 1,
    factor_formula = ~ s(elev, k = 5L, by = lv_axis()) - 1,
    data = dat, unit = time, species = species,
    family = poisson(), n_lv = 2L,
    run_model = FALSE, silent = 2
  )
  expect_true(isTRUE(mod$trend_metadata$has_by_lv))
  expect_equal(mod$trend_metadata$n_lv_for_grain, 2L)
  expect_equal(mod$standata$N_lv_trend, 2L)
})

test_that("jsdgam default family is binomial", {
  # Inspect formals rather than fit so the test does not need to wire
  # a trials() addition term into the formula just to exercise the
  # default. The end-to-end smoke for binomial fitting lives in the
  # full-suite recovery fixtures and the user-facing examples.
  expect_identical(
    eval(formals(jsdgam)$family)$family,
    "binomial"
  )
})
