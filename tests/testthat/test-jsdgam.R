# Tests for the `jsdgam()` wrapper:
#   1. Argument validation: 'unit' / 'species' / 'n_lv' / 'data'.
#   2. What a prefit carries: class, unit and species columns, program.
#   3. The traits and phylo aliases for the loadings prior.
#   4. Arguments the wrapper refuses.

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
    , validate = FALSE),
    "must.+include"
  )
})

test_that("jsdgam accepts brmsformula (bf) in addition to plain formula", {
  dat <- build_jsdgam_toy()
  # A `bf()` carries the dpar sub-formulas of diri(), occ() and
  # nmix(), as in `p ~ visit_cov`.
  expect_no_error(
    jsdgam(
      formula = brms::bf(y ~ elev),
      factor_formula = ~ -1,
      data = dat, species = species,
      family = poisson(), n_lv = 2L,
      run_model = FALSE, silent = 2
    , validate = FALSE)
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
      run_model = FALSE, silent = 2
    , validate = FALSE),
    "Must inherit from class"
  )
})

test_that("jsdgam accepts n_lv = n_species under iid prior (user owns ID)", {
  # Reason: n_lv = n_species is a full-rank factor model; the iid
  # loadings prior leaves Z rotationally unidentified, but the user
  # owns that decision. A post-fit advisor warns the user when
  # convergence diagnostics confirm a funnel (see
  # flag_by_lv_full_rank_funnel). validate_n_lv_ceiling() only
  # rejects n_lv > n_species.
  dat <- build_jsdgam_toy()
  expect_no_error(
    jsdgam(
      formula = y ~ 1, factor_formula = ~ -1,
      data = dat, species = species,
      family = poisson(), n_lv = 4L,
      run_model = FALSE, silent = 2
    , validate = FALSE)
  )
})

test_that("jsdgam rejects n_lv > n_species under iid prior", {
  dat <- build_jsdgam_toy()
  expect_error(
    jsdgam(
      formula = y ~ 1, factor_formula = ~ -1,
      data = dat, species = species,
      family = poisson(), n_lv = 5L,
      run_model = FALSE, silent = 2
    , validate = FALSE),
    "Got n_lv = 5, n_species = 4"
  )
})

test_that("jsdgam allows n_lv = n_species under MGP loadings_prior", {
  dat <- build_jsdgam_toy()
  # 4 species, n_lv = 4, admissible only under MGP shrinkage.
  expect_no_error(
    jsdgam(
      formula = y ~ 1, factor_formula = ~ -1,
      data = dat, species = species,
      family = poisson(), n_lv = 4L,
      loadings_prior = "mgp",
      run_model = FALSE, silent = 2
    , validate = FALSE)
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
      run_model = FALSE, silent = 2
    , validate = FALSE),
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
      run_model = FALSE, silent = 2
    , validate = FALSE),
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
    , validate = FALSE),
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
      run_model = FALSE, silent = 2
    , validate = FALSE),
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
    , validate = FALSE),
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
    , validate = FALSE),
    "already contains a 'series' column"
  )
})

# 2. Class + slot plumbing -------------------------------------------------

test_that("a jsdgam prefit carries its class, axes, trend and program", {
  dat <- build_jsdgam_toy()
  mod <- jsdgam(
    formula = y ~ 1, factor_formula = ~ -1,
    data = dat, unit = time, species = species,
    family = poisson(), n_lv = 2L,
    run_model = FALSE, silent = 2
  , validate = FALSE)
  expect_identical(class(mod), c("mvgam", "jsdgam", "mvgam_prefit"))
  expect_identical(mvgam:::jsdgam_axis_names(mod),
                   list(unit = "time", species = "species"))
  expect_equal(mod$standata$N_lv_trend, 2L)
  expect_equal(mod$standata$N_series_trend, 4L)
  # The default trend is the correlated latent prior. It writes no
  # `subgr`: a subgroup without a group is refused.
  spec <- mod$mv_spec$trend_specs
  expect_identical(spec$trend, "ZMVN")
  expect_true(isTRUE(spec$cor))
  expect_identical(spec$gr, "NA")
})

test_that("jsdgam records the unit column name the user gave", {
  dat <- build_jsdgam_toy()
  dat$site <- dat$time
  dat$time <- NULL
  mod <- jsdgam(
    formula = y ~ 1, factor_formula = ~ -1,
    data = dat, unit = site, species = species,
    family = poisson(), n_lv = 2L,
    run_model = FALSE, silent = 2
  , validate = FALSE)
  expect_identical(mvgam:::jsdgam_axis_names(mod)$unit, "site")
  expect_true("time" %in% names(mod$data))
})

test_that("jsdgam with by = lv_axis() composes the per-factor smooth path", {
  dat <- build_jsdgam_toy(n_time = 60L)
  mod <- jsdgam(
    formula = y ~ 1,
    factor_formula = ~ s(elev, k = 5L, by = lv_axis()) - 1,
    data = dat, unit = time, species = species,
    family = poisson(), n_lv = 2L,
    run_model = FALSE, silent = 2
  , validate = FALSE)
  expect_true(isTRUE(mod$trend_metadata$has_by_lv))
  expect_equal(mod$trend_metadata$n_lv_for_grain, 2L)
  expect_equal(mod$standata$N_lv_trend, 2L)
})

test_that("jsdgam default family is binomial", {
  # The test inspects the formals. A binomial prefit needs a trials() term.
  expect_identical(
    eval(formals(jsdgam)$family)$family,
    "binomial"
  )
})

# 3. traits + phylo aliases -----------------------------------------------
#
# The alias compiler, then the Stan data it gives through
# standata.mvgam_formula().

species_4 <- function() paste0("sp", 1:4)

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
  expect_error(
    mvgam:::jsdgam_phylo_to_dist("not_a_phylo", species_4()),
    "must be an 'ape::phylo' object or a numeric"
  )
})

# 3c. Stan data via standata.mvgam_formula()

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
  sd <- standata(
    fx$mf, data = fx$data, family = poisson(),
    trend_map = fx$trend_map,
    loadings_prior = list(
      features = fx$traits,
      distances = list(phylo = fx$d_mat)
    )
  )
  # Feature matrix: 1 numeric + 3 one-hot diet columns = 4 features
  expect_equal(dim(sd$row_features), c(4L, 4L))
  expect_equal(dim(sd$dist_phylo), c(4L, 4L))
  # validate_pairwise_distance() rescales to max = 1.
  expect_equal(max(sd$dist_phylo), 1)
})

test_that("default (no aliases) yields no row_features / dist_* slots", {
  fx <- jsdgam_loadings_fixture()
  sd <- standata(
    fx$mf, data = fx$data, family = poisson(),
    trend_map = fx$trend_map
  )
  expect_false("dist_phylo" %in% names(sd))
  expect_false("row_features" %in% names(sd))
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
  expect_true(all(nzchar(mvgam:::mvgam_removed_args)))
  expect_true(all(nzchar(names(mvgam:::mvgam_removed_args))))
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

test_that("jsdgam accepts the formulas mvgam accepts", {
  # A multivariate formula gives each species its own response and its
  # own observation family, which is the only way to fit a joint
  # species model over mixed data types. `jsdgam()` forwards to
  # `mvgam()`. A formula `mvgam()` accepts and the wrapper refused
  # would be a fault of the wrapper.
  set.seed(1L)
  n_site <- 24L
  d <- data.frame(
    site = seq_len(n_site),
    env = rnorm(n_site),
    cnt = rpois(n_site, 3),
    pa = rbinom(n_site, 1, 0.5)
  )
  mvf <- brms::bf(cnt ~ env, family = poisson()) +
    brms::bf(pa ~ env, family = bernoulli())

  m <- SW(jsdgam(
    formula = mvf, factor_formula = ~ -1, data = d,
    unit = site, species = species, n_lv = 1L, run_model = FALSE
  , validate = FALSE))
  expect_s3_class(m, "jsdgam")
  expect_identical(names(response_columns(m)), c("cnt", "pa"))

  # Each response keeps the family it named.
  code <- m$stancode
  expect_true(grepl("poisson_log_glm_lpmf(Y_cnt", code, fixed = TRUE))
  expect_true(grepl("bernoulli_logit_glm_lpmf(Y_pa", code, fixed = TRUE))

  # The loadings run across the responses, the species axis on this
  # layout. One latent state per response would ignore 'n_lv'.
  expect_identical(as.integer(m$standata$N_series_trend), 2L)
  expect_identical(as.integer(m$standata$N_lv_trend), 1L)
  expect_true(grepl("matrix[N_series_trend, N_lv_trend] Z", code,
                    fixed = TRUE))
})

test_that("a species column is required only when the formula needs one", {
  # The responses name the species on a multivariate formula, so
  # demanding a species column there asks for something the layout
  # cannot carry. The long layout still requires it.
  set.seed(2L)
  d <- data.frame(
    site = rep(seq_len(10L), times = 2L),
    species = factor(rep(c("a", "b"), each = 10L)),
    env = rnorm(20L)
  )
  d$y <- rpois(20L, 2)
  expect_error(
    SW(jsdgam(formula = y ~ env, factor_formula = ~ -1,
              data = d[, setdiff(names(d), "species")],
              unit = site, species = species, n_lv = 1L,
              family = poisson(), run_model = FALSE, validate = FALSE)),
    "species"
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
  filled <- jsdgam_axis_columns(fit, train[c("site", "sp")])
  expect_identical(filled$time, train$site)
  expect_identical(filled$series, train$sp)
  # A grid varies one name of the species and holds the other at one
  # level. Both then take the varied value.
  held <- factor("ant", levels = lev)
  for (grid in list(
    data.frame(sp = factor(lev, levels = lev), series = held, time = 1L),
    data.frame(sp = held, series = factor(lev, levels = lev), time = 1L)
  )) {
    aligned <- jsdgam_axis_columns(fit, grid)
    expect_identical(as.character(aligned$sp), lev)
    expect_identical(aligned$series, aligned$sp)
  }
})
