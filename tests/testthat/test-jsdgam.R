
# 1. Validation ------------------------------------------------------------

test_that("jsdgam errors when 'species' column is missing", {
  dat <- build_jsdgam_toy()
  # Default `species = series` but the data only has `species`.
  expect_error(
    jsdgam(
      formula = y ~ 1, factor_formula = ~ -1,
      data = dat, family = poisson(), n_lv = 2L,
      run_model = FALSE, silent = 2, validate = FALSE
    ),
    "must.+include"
  )
})

test_that("jsdgam rejects non-formula non-brmsformula 'formula' arg", {
  dat <- build_jsdgam_toy()
  expect_error(
    jsdgam(
      formula = "y ~ elev",  # bare string, not parsed
      factor_formula = ~ -1,
      data = dat, species = species,
      family = poisson(), n_lv = 2L,
      run_model = FALSE, silent = 2, validate = FALSE
    ),
    "Must inherit from class"
  )
})

test_that("jsdgam rejects n_lv > n_species under iid prior", {
  dat <- build_jsdgam_toy()
  expect_error(
    jsdgam(
      formula = y ~ 1, factor_formula = ~ -1,
      data = dat, species = species,
      family = poisson(), n_lv = 5L,
      run_model = FALSE, silent = 2, validate = FALSE
    ),
    "Got n_lv = 5, n_species = 4"
  )
})

test_that("jsdgam rejects n_lv > n_species even under MGP", {
  dat <- build_jsdgam_toy()
  expect_error(
    jsdgam(
      formula = y ~ 1, factor_formula = ~ -1,
      data = dat, species = species,
      family = poisson(), n_lv = 5L,
      loadings_prior = "mgp",
      run_model = FALSE, silent = 2, validate = FALSE
    ),
    "at most the number of species"
  )
})

test_that("jsdgam rejects n_lv = 0", {
  dat <- build_jsdgam_toy()
  expect_error(
    jsdgam(
      formula = y ~ 1, factor_formula = ~ -1,
      data = dat, species = species,
      family = poisson(), n_lv = 0L,
      run_model = FALSE, silent = 2, validate = FALSE
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
      run_model = FALSE, silent = 2, validate = FALSE
    ),
    "must be numeric"
  )
})

test_that("jsdgam rejects single-species data", {
  dat <- build_jsdgam_toy(n_species = 1L)
  expect_error(
    jsdgam(
      formula = y ~ 1, factor_formula = ~ -1,
      data = dat, species = species,
      family = poisson(), n_lv = 2L,
      run_model = FALSE, silent = 2, validate = FALSE
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
      run_model = FALSE, silent = 2, validate = FALSE
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
      run_model = FALSE, silent = 2, validate = FALSE
    ),
    "already contains a 'series' column"
  )
})

# 3a. build_jsdgam_loadings_prior helper

test_that("build_jsdgam_loadings_prior returns NULL by default", {
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
    "aliases were both supplied"
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
  expect_identical(names(spec), "distances")
  expect_equal(spec$distances$phylo,
               mvgam:::jsdgam_phylo_to_dist(d, species_4()))
})

test_that("build_jsdgam_loadings_prior forwards explicit loadings_prior", {
  lp <- list(features = data.frame(series = species_4(), x = 1:4))
  spec <- mvgam:::build_jsdgam_loadings_prior(
    traits = NULL, phylo = NULL, loadings_prior = lp,
    species_levels = species_4()
  )
  expect_identical(spec, lp)
})

# 3b. jsdgam_phylo_to_dist helper

test_that("jsdgam_phylo_to_dist orders a distance matrix by species", {
  d <- matrix(c(0, 1, 2, 3, 1, 0, 1, 2, 2, 1, 0, 1, 3, 2, 1, 0), 4, 4)
  rownames(d) <- colnames(d) <- species_4()
  expect_equal(mvgam:::jsdgam_phylo_to_dist(d, species_4()), d)
  rev_d <- d[4:1, 4:1]
  expect_equal(mvgam:::jsdgam_phylo_to_dist(rev_d, species_4()), d)
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

test_that("jsdgam_phylo_to_dist reads a phylo and returns named distances", {
  skip_if_not_installed("ape")
  # A tree built by hand needs no 'ape' and has known distances.
  # `((a,b),(c,d))` with every branch 0.5 puts each pair within a clade
  # at 1 and each pair across the clades at 2.
  sp <- species_4()
  tree <- structure(
    list(
      edge = matrix(
        c(5L, 6L, 6L, 1L, 6L, 2L, 5L, 7L, 7L, 3L, 7L, 4L),
        ncol = 2L, byrow = TRUE
      ),
      edge.length = rep(0.5, 6L),
      tip.label = sp,
      Nnode = 3L
    ),
    class = "phylo"
  )
  out <- mvgam:::jsdgam_phylo_to_dist(tree, sp)
  expect_equal(rownames(out), sp)
  expect_equal(unname(diag(out)), rep(0, 4L))
  # Within a clade, then across. Indexed by position, since the
  # labels come from `species_4()`.
  expect_equal(unname(out[sp[1L], sp[2L]]), 1)
  expect_equal(unname(out[sp[3L], sp[4L]]), 1)
  expect_equal(unname(out[sp[1L], sp[3L]]), 2)
})

test_that("jsdgam_phylo_to_dist rejects bad object types", {
  skip_if_not_installed("ape")
  expect_error(
    mvgam:::jsdgam_phylo_to_dist("not_a_phylo", species_4()),
    "must be an 'ape::phylo' object or a numeric"
  )
})

# 4. Arguments the wrapper refuses ----------------------------------------

test_that("mvgam refuses the arguments 1.x took and 2.0 does not", {
  dat <- build_jsdgam_toy()
  # Each of these would otherwise reach `...`, where brms drops what
  # it does not recognise, silently returning a model different from
  # the one the call described.
  expect_error(
    mvgam(y ~ 1, trend_formula = ~ AR(p = 1), data = dat,
          share_obs_params = TRUE),
    "share_obs_params"
  )
  expect_error(
    mvgam(y ~ 1, data = dat, trend_model = AR(p = 1)),
    "trend_formula"
  )
  expect_error(
    mvgam(y ~ 1, data = dat, use_lv = TRUE),
    "n_lv"
  )
  # And through the wrapper, whose own formals no longer swallow them.
  expect_error(
    jsdgam(formula = y ~ 1, factor_formula = ~ -1, data = dat,
           species = species, family = poisson(), n_lv = 2L,
           share_obs_params = TRUE, run_model = FALSE, silent = 2,
           validate = FALSE),
    "share_obs_params"
  )
})

test_that("every removed argument names its replacement", {
  # The table is what the message is built from, so an entry added
  # without a reason would say nothing useful.
  # A name that is still a formal would make the refusal unreachable
  # and the message wrong.
  expect_length(
    intersect(names(mvgam:::mvgam_removed_args), names(formals(mvgam))),
    0L
  )
  expect_length(
    intersect(names(mvgam:::mvgam_removed_args), names(formals(jsdgam))),
    0L
  )
})

test_that("a frame takes the series and time a jsdgam fit uses", {
  lev <- c("ant", "bee", "cod")
  train <- data.frame(
    site = rep(1:2, times = 3L),
    sp = factor(rep(lev, each = 2L), levels = lev)
  )
  train$time <- train$site
  train$series <- train$sp
  fit <- list(data = train,
              jsdgam_args = list(unit = "site", species = "sp"))
  # A frame naming only the user's columns takes both copies.
  filled <- jsdgam_prediction_columns(fit, train[c("site", "sp")])
  expect_identical(filled$time, train$site)
  expect_identical(filled$series, train$sp)
  # A grid varies one name of the species and holds the other at one
  # level. Both then take the varied value.
  held <- factor("ant", levels = lev)
  for (grid in list(
    data.frame(sp = factor(lev, levels = lev), series = held, time = 1L),
    data.frame(sp = held, series = factor(lev, levels = lev), time = 1L)
  )) {
    aligned <- jsdgam_prediction_columns(fit, grid)
    expect_identical(as.character(aligned$sp), lev)
    expect_identical(aligned$series, aligned$sp)
  }
})
