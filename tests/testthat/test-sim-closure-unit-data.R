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


test_that("every nmix variant holds the abundance to K_max", {
  # `K_max` is documented as the cap on the latent abundance for
  # every `nmix()` variant, and each variant's likelihood integrates
  # the abundance up to it. The Royle-Nichols and Poisson-Poisson
  # draws ignored it.
  for (type in c("poisson_binomial", "royle_nichols", "poisson_poisson")) {
    s <- sim_closure_unit_data(type = 1L, family = nmix(type),
                               n_sites = 60L, K_max = 2L, seed = 21L)
    expect_lte(max(s$truth$N), 2L)
    expect_gt(max(s$truth$N), 0L)
  }
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


test_that("the simulated frame builds the family's own likelihood", {
  # No Stan fit. The long frame goes through the closure-unit
  # preparation, and the program scores it with the occupancy
  # likelihood over the unit arrays that preparation builds.
  s <- sim_closure_unit_data(
    type = 1L, family = occ(), n_sites = 30L, seed = 13L
  )
  code <- stancode(
    mvgam_formula(brms::bf(y ~ env, p ~ tod_c)),
    data = s$data_train, family = occ()
  )
  expect_match(
    code, "target += occ_lpmf(Y | mu, p, N_unit, n_rep, Y_max, visit_idx);",
    fixed = TRUE
  )
})


test_that("detection is simulated on the scale of the family's link", {
  # `?sim_closure_unit_data` put the Royle-Nichols detection predictor
  # on the log scale. The family declares a logit link and the
  # simulator applies it, so a reader setting an intercept from the
  # page got another probability. The recorded detection probability
  # is rebuilt here from the predictor through each family's own
  # inverse link.
  families <- list(occ(), nmix("poisson_binomial"),
                   nmix("royle_nichols"), nmix("poisson_poisson"))
  for (fam in families) {
    s <- sim_closure_unit_data(type = 1L, family = fam, n_sites = 12L,
                               n_visits = 3L, seed = 11L)
    d <- s$data_train
    coefs <- s$truth$detection_coefs
    eta <- coefs[["intercept"]] + coefs[["tod_c"]] * d$tod_c
    r <- stats::make.link(fam$link_p)$linkinv(eta)
    # Royle-Nichols detects a unit when any of its N individuals is
    # detected; the other three read the probability directly.
    expected <- if (identical(fam$name, "nmix_royle_nichols")) {
      1 - (1 - r)^s$truth$N[1L, d$site]
    } else {
      r
    }
    # `truth$p` is `[species, site, visit]`; the frame runs visits
    # within sites.
    expect_equal(as.vector(t(s$truth$p[1L, , ])), expected)
  }
})


test_that("draw_recipe_coefs produces community-mean structure", {
  # The simulator draws one community mean per covariate, then
  # per-species slopes around it with sd 0.5. Across 50 species the
  # empirical sd of each covariate's slopes sits near 0.5. The
  # community mean can be large or small; the spread is what is
  # pinned.
  set.seed(2024)
  s <- sim_closure_unit_data(
    type = 2L, family = occ(),
    n_species = 50L, n_sites = 5L, n_visits = 2L
  )
  state <- s$truth$state_coefs
  expect_true(all(c("env", "elev") %in% colnames(state)))
  # Per-covariate spread across species (sd of slopes).
  env_sd  <- stats::sd(state[, "env"])
  elev_sd <- stats::sd(state[, "elev"])
  # A tolerance of 0.25 separates sd 0.5 from an iid N(0, 0.75)
  # draw, whose empirical sd lands near 0.75.
  expect_lt(abs(env_sd  - 0.5), 0.25)
  expect_lt(abs(elev_sd - 0.5), 0.25)
})
