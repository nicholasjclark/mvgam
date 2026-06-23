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

test_that("jsdgam accepts brmsformula (bf) in addition to plain formula", {
  dat <- build_jsdgam_toy()
  # Relaxed assertion allows `brms::bf()` so downstream dpar
  # sub-formulas (`phi ~ env` for diri(), `p ~ visit_cov` for
  # occ() / nmix()) compose with `jsdgam()`. Plain `bf(y ~ x)`
  # round-trips identically to a plain `formula`.
  suppressWarnings(expect_no_error(
    jsdgam(
      formula = brms::bf(y ~ elev),
      factor_formula = ~ -1,
      data = dat, species = species,
      family = poisson(), n_lv = 2L,
      run_model = FALSE, silent = 2
    )
  ))
})

test_that("jsdgam rejects non-formula non-brmsformula 'formula' arg", {
  dat <- build_jsdgam_toy()
  suppressWarnings(expect_error(
    jsdgam(
      formula = "y ~ elev",  # bare string, not parsed
      factor_formula = ~ -1,
      data = dat, species = species,
      family = poisson(), n_lv = 2L,
      run_model = FALSE, silent = 2
    ),
    "Must inherit from class"
  ))
})

test_that("jsdgam accepts n_lv = n_species under iid prior (user owns ID)", {
  # Reason: n_lv = n_species is a full-rank factor model; the iid
  # loadings prior leaves Z rotationally unidentified, but the user
  # owns that decision. A post-fit advisor warns the user when
  # convergence diagnostics confirm a funnel (see
  # flag_by_lv_full_rank_funnel). validate_n_lv_ceiling() only
  # rejects n_lv > n_species.
  dat <- build_jsdgam_toy()
  suppressWarnings(expect_no_error(
    jsdgam(
      formula = y ~ 1, factor_formula = ~ -1,
      data = dat, species = species,
      family = poisson(), n_lv = 4L,
      run_model = FALSE, silent = 2
    )
  ))
})

test_that("jsdgam rejects n_lv > n_species under iid prior", {
  dat <- build_jsdgam_toy()
  suppressWarnings(expect_error(
    jsdgam(
      formula = y ~ 1, factor_formula = ~ -1,
      data = dat, species = species,
      family = poisson(), n_lv = 5L,
      run_model = FALSE, silent = 2
    ),
    "cannot exceed"
  ))
})

test_that("jsdgam allows n_lv = n_species under MGP loadings_prior", {
  dat <- build_jsdgam_toy()
  # 4 species, n_lv = 4, admissible only under MGP shrinkage. The
  # `run_model = FALSE` deprecation warning fires here; suppress so
  # it doesn't leak into the testthat summary.
  suppressWarnings(expect_no_error(
    jsdgam(
      formula = y ~ 1, factor_formula = ~ -1,
      data = dat, species = species,
      family = poisson(), n_lv = 4L,
      loadings_prior = "mgp",
      run_model = FALSE, silent = 2
    )
  ))
})

test_that("jsdgam rejects n_lv > n_species even under MGP", {
  dat <- build_jsdgam_toy()
  suppressWarnings(expect_error(
    jsdgam(
      formula = y ~ 1, factor_formula = ~ -1,
      data = dat, species = species,
      family = poisson(), n_lv = 5L,
      loadings_prior = "mgp",
      run_model = FALSE, silent = 2
    ),
    "cannot exceed the number of species"
  ))
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
  mod <- suppressWarnings(jsdgam(
    formula = y ~ 1, factor_formula = ~ -1,
    data = dat, unit = time, species = species,
    family = poisson(), n_lv = 2L,
    run_model = FALSE, silent = 2
  ))
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
  mod <- suppressWarnings(jsdgam(
    formula = y ~ 1, factor_formula = ~ -1,
    data = dat, unit = site, species = species,
    family = poisson(), n_lv = 2L,
    run_model = FALSE, silent = 2
  ))
  expect_identical(
    attr(mod$model_data, "prepped_trend_model")$unit,
    "site"
  )
  expect_true("time" %in% names(mod$model_data))
})

# 3. Forward-compat composition --------------------------------------------

test_that("jsdgam composes with the standard mvgam pipeline", {
  dat <- build_jsdgam_toy()
  mod <- suppressWarnings(jsdgam(
    formula = y ~ 1, factor_formula = ~ -1,
    data = dat, unit = time, species = species,
    family = poisson(), n_lv = 2L,
    run_model = FALSE, silent = 2
  ))
  # `run_model = FALSE` returns the stub mvgam shape with stancode +
  # standata populated and a NULL $fit; the end-to-end smoke fit
  # lives in tests/local/.
  expect_null(mod$fit)
  expect_true(!is.null(mod$standata))
  expect_true(!is.null(mod$stancode))
  expect_equal(mod$standata$N_lv_trend, 2L)
  expect_equal(mod$standata$N_series_trend, 4L)
})

test_that("jsdgam with by = lv_axis() composes the per-factor smooth path", {
  dat <- build_jsdgam_toy(n_time = 60L)
  mod <- suppressWarnings(jsdgam(
    formula = y ~ 1,
    factor_formula = ~ s(elev, k = 5L, by = lv_axis()) - 1,
    data = dat, unit = time, species = species,
    family = poisson(), n_lv = 2L,
    run_model = FALSE, silent = 2
  ))
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

# 4. traits + phylo aliases -----------------------------------------------
#
# Unit-test the alias compiler directly and exercise the downstream
# Stan emission via standata.mvgam_formula() (no jsdgam()/mvgam() fit
# pipeline). Goes straight to the brms-style helper so every test is
# bounded by a single make_standata() call rather than full wrapper
# validation + slot plumbing.

species_4 <- function() paste0("sp", 1:4)

# 4a. build_jsdgam_loadings_prior helper

test_that("build_jsdgam_loadings_prior returns NULL when all aliases are NULL", {
  spec <- mvgam:::build_jsdgam_loadings_prior(
    traits = NULL, phylo = NULL, loadings_prior = NULL,
    species_levels = species_4()
  )
  expect_null(spec)
})

test_that("build_jsdgam_loadings_prior errors on alias + explicit conflict", {
  expect_error(
    mvgam:::build_jsdgam_loadings_prior(
      traits = data.frame(series = species_4(), x = 1:4),
      phylo = NULL,
      loadings_prior = list(features = data.frame(x = 1:4)),
      species_levels = species_4()
    ),
    "EITHER an explicit"
  )
})

test_that("build_jsdgam_loadings_prior threads traits into features", {
  traits <- data.frame(series = species_4(), body_mass = c(0.2, 0.6, 1.1, 1.5))
  spec <- mvgam:::build_jsdgam_loadings_prior(
    traits = traits, phylo = NULL, loadings_prior = NULL,
    species_levels = species_4()
  )
  expect_identical(spec$features, traits)
  expect_null(spec$distances)
})

test_that("build_jsdgam_loadings_prior threads phylo into distances$phylo", {
  d <- matrix(c(0, 1, 2, 3, 1, 0, 1, 2, 2, 1, 0, 1, 3, 2, 1, 0), 4, 4)
  rownames(d) <- colnames(d) <- species_4()
  spec <- mvgam:::build_jsdgam_loadings_prior(
    traits = NULL, phylo = d, loadings_prior = NULL,
    species_levels = species_4()
  )
  expect_true("phylo" %in% names(spec$distances))
  expect_equal(dim(spec$distances$phylo), c(4L, 4L))
})

test_that("build_jsdgam_loadings_prior forwards explicit loadings_prior", {
  lp <- list(features = data.frame(series = species_4(), x = 1:4))
  spec <- mvgam:::build_jsdgam_loadings_prior(
    traits = NULL, phylo = NULL, loadings_prior = lp,
    species_levels = species_4()
  )
  expect_identical(spec, lp)
})

# 4b. jsdgam_phylo_to_dist helper

test_that("jsdgam_phylo_to_dist passes through a numeric distance matrix", {
  d <- matrix(c(0, 1, 2, 3, 1, 0, 1, 2, 2, 1, 0, 1, 3, 2, 1, 0), 4, 4)
  rownames(d) <- colnames(d) <- species_4()
  out <- mvgam:::jsdgam_phylo_to_dist(d, species_4())
  expect_equal(out, d[species_4(), species_4()])
})

test_that("jsdgam_phylo_to_dist reorders rows / cols to species_levels", {
  d <- matrix(c(0, 1, 2, 3, 1, 0, 1, 2, 2, 1, 0, 1, 3, 2, 1, 0), 4, 4)
  reordered <- rev(species_4())
  rownames(d) <- colnames(d) <- reordered
  out <- mvgam:::jsdgam_phylo_to_dist(d, species_4())
  expect_equal(rownames(out), species_4())
})

test_that("jsdgam_phylo_to_dist errors when distance matrix lacks names", {
  d <- matrix(0, 4L, 4L)
  expect_error(
    mvgam:::jsdgam_phylo_to_dist(d, species_4()),
    "row and column names"
  )
})

test_that("jsdgam_phylo_to_dist errors when species missing from matrix", {
  d <- matrix(0, 3L, 3L)
  rownames(d) <- colnames(d) <- species_4()[1:3]
  expect_error(
    mvgam:::jsdgam_phylo_to_dist(d, species_4()),
    "missing one or more species levels"
  )
})

test_that("jsdgam_phylo_to_dist accepts ape::phylo and returns named matrix", {
  testthat::skip_if_not_installed("ape")
  set.seed(7L)
  tree <- ape::rcoal(n = 4L, tip.label = species_4())
  out <- mvgam:::jsdgam_phylo_to_dist(tree, species_4())
  expect_equal(dim(out), c(4L, 4L))
  expect_equal(rownames(out), species_4())
})

test_that("jsdgam_phylo_to_dist rejects bad object types", {
  expect_error(
    mvgam:::jsdgam_phylo_to_dist("not_a_phylo", species_4()),
    "must be an 'ape::phylo' object or a numeric"
  )
})

# 4c. End-to-end Stan emission via standata.mvgam_formula().
# Single standata() call rather than per-test mvgam compiles.

jsdgam_loadings_fixture <- function() {
  set.seed(2L)
  n_species <- 4L
  series_levels <- species_4()
  data <- data.frame(
    series = factor(rep(series_levels, each = 6L), levels = series_levels),
    time = rep(seq_len(6L), n_species),
    y = rpois(24L, 2)
  )
  traits <- data.frame(
    series = series_levels,
    body_mass = c(0.2, 0.6, 1.1, 1.5),
    diet = factor(c("plant", "insect", "fish", "fish"))
  )
  d_mat <- as.matrix(stats::dist(seq_len(n_species)))
  rownames(d_mat) <- colnames(d_mat) <- series_levels
  mf <- mvgam_formula(
    y ~ 1,
    trend_formula = ~ ZMVN(cor = TRUE, subgr = series)
  )
  tm <- matrix(NA_real_, n_species, 2L)
  rownames(tm) <- series_levels
  list(
    data = data, traits = traits, d_mat = d_mat,
    mf = mf, trend_map = tm
  )
}

test_that("traits + phylo route through standata.mvgam_formula cleanly", {
  fx <- jsdgam_loadings_fixture()
  sd <- suppressWarnings(standata(
    fx$mf, data = fx$data, family = poisson(),
    trend_map = fx$trend_map,
    loadings_prior = list(
      features = fx$traits,
      distances = list(phylo = fx$d_mat)
    )
  ))
  # Feature matrix: 1 numeric + 3 one-hot diet columns = 4 features
  expect_true(!is.null(sd$row_features))
  expect_equal(nrow(sd$row_features), 4L)
  expect_equal(ncol(sd$row_features), 4L)
  expect_true("dist_phylo" %in% names(sd))
  expect_equal(dim(sd$dist_phylo), c(4L, 4L))
  # validate_pairwise_distance() rescales to max = 1.
  expect_equal(max(sd$dist_phylo), 1)
})

test_that("default (no aliases) yields no row_features / dist_* slots", {
  fx <- jsdgam_loadings_fixture()
  sd <- suppressWarnings(standata(
    fx$mf, data = fx$data, family = poisson(),
    trend_map = fx$trend_map
  ))
  expect_false("dist_phylo" %in% names(sd))
  expect_false("row_features" %in% names(sd))
})

