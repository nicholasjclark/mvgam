# The tests of test-jsdgam.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()

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
      run_model = FALSE, silent = 2, validate = FALSE
    )
  )
})

# 2. Class + slot plumbing -------------------------------------------------

test_that("a jsdgam prefit carries its class, axes, trend and program", {
  dat <- build_jsdgam_toy()
  mod <- jsdgam(
    formula = y ~ 1, factor_formula = ~ -1,
    data = dat, unit = time, species = species,
    family = poisson(), n_lv = 2L,
    run_model = FALSE, silent = 2, validate = FALSE
  )
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
    run_model = FALSE, silent = 2, validate = FALSE
  )
  expect_identical(mvgam:::jsdgam_axis_names(mod)$unit, "site")
  expect_true("time" %in% names(mod$data))
})

test_that("newdata takes the columns data takes, derived ones too", {
  dat <- build_jsdgam_toy()
  names(dat)[names(dat) == "time"] <- "site"
  dat$hab <- factor(ifelse(dat$site %% 2 == 0, "a", "b"))
  dat$size <- as.numeric(dat$species)
  mod <- jsdgam(
    formula = y ~ s(elev, k = 4, by = hab), trait_slopes = ~ size,
    data = dat[dat$site <= 20, ], newdata = dat[dat$site > 20, ],
    unit = site, species = species, family = poisson(), n_lv = 1L,
    run_model = FALSE, silent = 2, validate = FALSE
  )
  test <- mod$test_data
  # The held-out frame takes the `time` and `series` copies
  expect_identical(test$time, test$site)
  expect_identical(as.character(test$series), as.character(test$species))
  # The factor `by` crosses the trait level by level
  expect_identical(test$habb_size, (test$hab == "b") * test$size)
  expect_identical(as.character(test$hab_series),
                   paste(test$hab, test$species, sep = "_"))
  # A frame naming the user's columns alone gets every derived column
  raw <- dat[dat$site > 20, c("site", "species", "elev", "hab")]
  expect_identical(
    mvgam:::jsdgam_prediction_columns(mod, raw)$habb_size,
    test$habb_size
  )
  # A refit passes both frames as the user gave them. jsdgam() derives
  # the columns again under the same names.
  refit <- update(mod, n_lv = 2L, run_model = FALSE)
  expect_identical(names(refit$trait_columns$derived),
                   names(mod$trait_columns$derived))
  expect_identical(refit$test_data$habb_size, test$habb_size)
  # kfold() and lfo_cv() refit on frames of the fit's own data
  args <- mvgam:::jsdgam_update_call(mod, NULL, mod$data, list())
  expect_setequal(names(args$data), names(dat))
})

test_that("trait_slopes priors scale by column and give way to the user's", {
  dat <- build_jsdgam_toy()
  dat$size <- as.numeric(dat$species)
  prefit <- function(...) {
    jsdgam(y ~ elev, trait_slopes = ~ size, data = dat, unit = time,
           species = species, family = poisson(), n_lv = 1L,
           run_model = FALSE, silent = 2, validate = FALSE, ...)
  }
  rows <- function(fit, coef) {
    tab <- get_prior(fit)
    tab[tab$class == "b" & tab$coef == coef, c("prior", "source")]
  }
  # get_prior() shows each scaled default, labelled as a default
  expect_identical(
    unname(unlist(rows(prefit(), "elev:size"))),
    c(sprintf("normal(0, %s)", signif(1 / sd(dat$elev * dat$size), 3)),
      "default")
  )
  # A user prior on the class reaches every coefficient
  fit <- prefit(priors = brms::prior(normal(0, 3), class = b))
  tab <- get_prior(fit)
  b_rows <- tab[tab$class == "b", ]
  expect_identical(b_rows$prior[!nzchar(b_rows$coef)], "normal(0, 3)")
  expect_false(any(grepl("normal(0, 0", b_rows$prior, fixed = TRUE)))
  # A user prior on one coefficient replaces that default alone
  fit <- prefit(priors = brms::prior(normal(0, 9), class = b,
                                     coef = "elev:size"))
  expect_identical(rows(fit, "elev:size")$prior, "normal(0, 9)")
  expect_identical(rows(fit, "elev")$source, "default")
})

test_that("jsdgam with by = lv_axis() composes the per-factor smooth path", {
  dat <- build_jsdgam_toy(n_time = 60L)
  mod <- jsdgam(
    formula = y ~ 1,
    factor_formula = ~ s(elev, k = 5L, by = lv_axis()) - 1,
    data = dat, unit = time, species = species,
    family = poisson(), n_lv = 2L,
    run_model = FALSE, silent = 2, validate = FALSE
  )
  expect_true(isTRUE(mod$trend_metadata$has_by_lv))
  expect_equal(mod$trend_metadata$n_lv_for_grain, 2L)
  expect_equal(mod$standata$N_lv_trend, 2L)
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
    trend_formula = ~ ZMVN(cor = TRUE, subgr = series, n_lv = 2L)
  )
  list(data = data, traits = traits, d_mat = d_mat, mf = mf)
}

test_that("traits + phylo route through standata.mvgam_formula cleanly", {
  fx <- jsdgam_loadings_fixture()
  sd <- standata(
    fx$mf, data = fx$data, family = poisson(),
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
    fx$mf, data = fx$data, family = poisson()
  )
  expect_false("dist_phylo" %in% names(sd))
  expect_false("row_features" %in% names(sd))
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
    unit = site, species = species, n_lv = 1L, run_model = FALSE,
    validate = FALSE
  ))
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
