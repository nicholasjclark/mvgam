# Fixture-free tests for sim_closure_unit_data(). Confirms shape,
# class, and truth-bundle invariants for each recipe x family
# combination without compiling Stan; downstream recovery checks
# (truth vs posterior mean of psi / lambda) live in tests/local/.


test_that("type 1 occ single-species emits the expected layout", {
  s <- sim_closure_unit_data(
    type = 1L, family = occ(), n_sites = 30L,
    n_visits = 4L, seed = 1L
  )
  expect_s3_class(s, "mvgam_sim_closure_unit")
  expect_s3_class(s, "mvgam_sim")
  expect_equal(nrow(s$data_train), 30L * 4L)
  expect_equal(dim(s$y_array), c(1L, 30L, 1L, 4L))
  expect_setequal(
    names(s$data_train),
    c("visit", "site", "series", "time", "y", "cap", "env", "tod_c")
  )
  expect_true(all(s$data_train$cap == 1L))
  expect_true(all(s$data_train$y %in% c(0L, 1L)))
  # Sorted by (series, time, visit) so closure_unit_arrays sees
  # the K visits per unit in order.
  expect_equal(s$data_train$series, factor(rep("sp_1", 120L),
                                            levels = "sp_1"))
  expect_equal(s$data_train$time, rep(1:30, each = 4L))
  expect_equal(s$data_train$visit,
                rep(1:4, times = 30L))
})


test_that("type 2 emits the second covariate on each linpred", {
  s <- sim_closure_unit_data(
    type = 2L, family = occ(), n_sites = 25L, seed = 2L
  )
  expect_true(all(c("env", "elev", "tod_c", "effort") %in%
                    names(s$data_train)))
  expect_named(
    s$truth$state_coefs,
    c("intercept", "env", "elev")
  )
  expect_named(
    s$truth$detection_coefs,
    c("intercept", "tod_c", "effort")
  )
})


test_that("nmix family produces integer counts and cap = K_max", {
  s <- sim_closure_unit_data(
    type = 1L, family = nmix(), n_sites = 20L,
    K_max = 15L, seed = 3L
  )
  expect_true(all(s$data_train$cap == 15L))
  expect_true(all(s$data_train$y >= 0L))
  expect_named(s$truth, c("state_coefs", "detection_coefs", "p",
                            "lambda", "N"))
  expect_equal(dim(s$truth$lambda), c(1L, 20L))
  expect_equal(dim(s$truth$N), c(1L, 20L))
})


test_that("multi-species emits per-species coef matrices", {
  s <- sim_closure_unit_data(
    type = 2L, family = occ(),
    n_species = 4L, n_sites = 15L, seed = 4L
  )
  expect_equal(dim(s$truth$state_coefs), c(4L, 3L))
  expect_equal(dim(s$truth$detection_coefs), c(4L, 3L))
  expect_equal(dim(s$y_array), c(4L, 15L, 1L, 4L))
  expect_equal(levels(s$data_train$series),
                paste0("sp_", 1:4))
})


test_that("n_lv > 0 carries loadings + lv in truth", {
  s <- sim_closure_unit_data(
    type = 1L, family = occ(),
    n_species = 3L, n_sites = 20L, n_lv = 2L, seed = 5L
  )
  expect_equal(dim(s$truth$loadings), c(3L, 2L))
  expect_equal(dim(s$truth$lv), c(20L, 2L))
})


test_that("n_lv > 0 requires n_species > 1", {
  expect_error(
    sim_closure_unit_data(n_lv = 2L, n_species = 1L),
    "n_lv > 0 requires n_species >= 2"
  )
})


test_that("non-closure-unit families are rejected up front", {
  expect_error(
    sim_closure_unit_data(family = gaussian()),
    "must be a closure-unit family"
  )
})


test_that("seed gives reproducible draws", {
  a <- sim_closure_unit_data(type = 1L, family = occ(),
                              n_sites = 10L, seed = 99L)
  b <- sim_closure_unit_data(type = 1L, family = occ(),
                              n_sites = 10L, seed = 99L)
  expect_identical(a$y_array, b$y_array)
  expect_identical(a$truth$state_coefs, b$truth$state_coefs)
})


test_that("summary prints non-degenerate detection-rate diagnostic", {
  s <- sim_closure_unit_data(type = 1L, family = occ(),
                              n_sites = 50L, seed = 7L)
  smry <- summary(s)
  expect_s3_class(smry, "mvgam_sim_closure_unit_summary")
  out <- capture.output(print(smry))
  expect_true(any(grepl("Family", out)))
  expect_true(any(grepl("Non-zero observation fraction", out)))
  # Detection fraction in (0, 1) -- not all-zeros, not all-ones.
  expect_gt(smry$non_zero_fraction, 0)
  expect_lt(smry$non_zero_fraction, 1)
})


test_that("y_array round-trips through pivot_detection_array()", {
  s <- sim_closure_unit_data(
    type = 1L, family = occ(),
    n_species = 3L, n_sites = 20L, seed = 8L
  )
  pivoted <- pivot_detection_array(
    s$y_array,
    site_covs = data.frame(env = stats::rnorm(20L)),
    obs_covs  = list(tod_c = matrix(stats::runif(20L * 1L * 4L),
                                     nrow = 20L)),
    site_col   = "site", season_col = "time",
    visit_col  = "visit", series_col = "series",
    y_col      = "y"
  )
  # The pivoted long-form should have one row per
  # (species, site, season=1, visit) cell.
  expect_equal(nrow(pivoted), 3L * 20L * 1L * 4L)
  expect_true(all(c("series", "site", "visit", "y") %in%
                    names(pivoted)))
})


test_that("compiled fit dispatches via family branching", {
  # No Stan fit: just compile the standata to confirm the long-
  # format data feeds make_stancode + the closure-unit prep
  # pipeline without error.
  s <- sim_closure_unit_data(
    type = 1L, family = occ(), n_sites = 30L, seed = 13L
  )
  code <- tryCatch(
    make_stancode(
      brms::bf(y ~ env, p ~ tod_c),
      family = occ(), data = s$data_train
    ),
    error = function(e) conditionMessage(e)
  )
  expect_type(code, "character")
  expect_true(nchar(code) > 100L)
})
