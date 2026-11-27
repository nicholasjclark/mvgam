# The tests of test-loadings-prior.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()

test_that("MGP shrinkage scales the unit-scale loadings by column", {
  # The program samples `Z_raw` at unit scale and builds
  # `Z = diag_post_multiply(Z_raw, sqrt(Psi_diag))`. The factor
  # innovations stay at unit scale, as under every other prior on
  # sampled loadings. A scale on both would leave only their product
  # identified.
  set.seed(1L)
  d <- sim_mvgam(family = poisson(), n_series = 6L,
                  n_timepoints = 20L)$data_train
  m <- mvgam(
    y ~ 1, trend_formula = ~ AR(n_lv = 3), data = d, family = poisson(),
    loadings_prior = list(column_shrinkage = "mgp"), run_model = FALSE,
    validate = FALSE
  )
  code <- gsub("\\s+", " ", paste(as.character(m$model_file %||%
                                                  m$stancode),
                                   collapse = " "))
  expect_match(code, "Z = diag_post_multiply(Z_raw, sqrt(Psi_diag));",
               fixed = TRUE)
  expect_match(code, "std_normal_lpdf(to_vector(Z_raw))", fixed = TRUE)
  expect_match(code, "scaled_innovations_trend = innovations_trend;",
               fixed = TRUE)
  expect_match(code, "sigma_trend = rep_vector(1.0, N_lv_trend);",
               fixed = TRUE)

  # A user prior on `Z` reaches the unit-scale loadings
  m_user <- mvgam(
    y ~ 1, trend_formula = ~ AR(n_lv = 3), data = d, family = poisson(),
    loadings_prior = "mgp", prior = prior(normal(0, 2), class = Z),
    run_model = FALSE, validate = FALSE
  )
  expect_match(paste(as.character(m_user$model_file %||% m_user$stancode),
                     collapse = "\n"),
               "normal_lpdf(to_vector(Z_raw) | 0, 2)", fixed = TRUE)

  # A kernel fixes the loadings' prior and refuses one on `Z`
  traits <- data.frame(series = factor(paste0("series_", 1:6)),
                       t1 = rnorm(6))
  expect_error(
    mvgam(y ~ 1, trend_formula = ~ AR(n_lv = 3), data = d,
          family = poisson(), loadings_prior = list(features = traits),
          prior = prior(normal(0, 2), class = Z), run_model = FALSE,
          validate = FALSE),
    "A prior on 'Z' is not supported with a loadings kernel"
  )
})

test_that("MGP shrinkage is refused on trends that cannot carry it", {
  # The column scale reaches the model through `sigma_trend`, which
  # only the shared-innovation path builds. VAR() constructs its own
  # innovation covariance. The shrinkage would be declared and never
  # applied.
  set.seed(1L)
  d <- sim_mvgam(family = poisson(), n_series = 6L,
                  n_timepoints = 20L)$data_train
  expect_error(
    mvgam(y ~ 1, trend_formula = ~ VAR(n_lv = 3), data = d,
          family = poisson(),
          loadings_prior = list(column_shrinkage = "mgp"),
          run_model = FALSE, validate = FALSE),
    "Got 'VAR\\(\\)'"
  )
  # The other factor trends carry it. AR() is built above.
  for (tf in list(~ RW(n_lv = 3), ~ ZMVN(n_lv = 3))) {
    expect_no_error(mvgam(
      y ~ 1, trend_formula = tf, data = d, family = poisson(),
      loadings_prior = list(column_shrinkage = "mgp"), run_model = FALSE,
      validate = FALSE
    ))
  }
})

test_that("loadings_prior requires a trend to carry it", {
  # The structured prior replaces the iid prior on `Z`, which exists
  # only on a trend with latent factors. Both refusals name what the
  # prior needs.
  set.seed(1L)
  d <- sim_mvgam(family = poisson(), n_series = 6L,
                 n_timepoints = 20L)$data_train
  err <- expect_error(
    mvgam(y ~ 1, data = d, family = poisson(),
          loadings_prior = list(column_shrinkage = "mgp"),
          run_model = FALSE, validate = FALSE),
    "loadings_prior"
  )
  expect_match(conditionMessage(err), "trend_formula", fixed = TRUE)
  expect_error(
    mvgam(y ~ 1, trend_formula = ~ AR(), data = d, family = poisson(),
          loadings_prior = list(column_shrinkage = "mgp"),
          run_model = FALSE, validate = FALSE),
    "requires a factor trend"
  )
})

test_that("each kernel length-scale can be seen and set", {
  # The length-scales were emitted as literals with no registry
  # entry, so `prior_summary()` reported a row `get_prior()` never
  # offered and nothing could change. For a phylogenetic kernel the
  # length-scale sets how fast covariance decays with relatedness,
  # which is a modelling choice rather than a nuisance.
  dat <- data.frame(
    y = rpois(40, 5), time = rep(1:20, 2),
    series = factor(rep(c("a", "b"), each = 20))
  )
  feat <- matrix(rnorm(6), 2, 3, dimnames = list(c("a", "b"), NULL))
  dm <- matrix(c(0, .5, .5, 0), 2, 2,
               dimnames = list(c("a", "b"), c("a", "b")))
  lp <- list(features = feat, distances = list(phy = dm))
  mf <- mvgam_formula(y ~ 1, ~ ZMVN(n_lv = 2))

  tab <- get_prior(mf, data = dat, family = poisson(), loadings_prior = lp)
  scales <- tab[is_loadings_length_scale(tab$class), ]
  expect_setequal(scales$class, c("theta_features", "theta_dist_phy"))
  # A length-scale is positive, and the table has to say so.
  expect_true(all(scales$lb == "0"))

  # What is reported is what is emitted.
  sc <- stancode(mf, data = dat, family = poisson(), loadings_prior = lp,
                 validate = FALSE)
  for (i in seq_len(nrow(scales))) {
    expect_true(grepl(
      stan_prior_line(scales$class[i], scales$prior[i]), sc,
      fixed = TRUE
    ))
  }

  # An override reaches the program, per distance source.
  user <- brms::prior_string("inv_gamma(3, 2)", class = "theta_features") +
    brms::prior_string("lognormal(1, 0.5)", class = "theta_dist_phy")
  sc2 <- stancode(mf, data = dat, family = poisson(),
                  loadings_prior = lp, prior = user, validate = FALSE)
  expect_true(grepl(stan_prior_line("theta_features", "inv_gamma(3, 2)"),
                      sc2, fixed = TRUE))
  expect_true(grepl(stan_prior_line("theta_dist_phy", "lognormal(1, 0.5)"),
                      sc2, fixed = TRUE))

  # A model with no kernel declares none, so none is reported.
  plain <- get_prior(mf, data = dat, family = poisson())
  expect_false(any(is_loadings_length_scale(plain$class)))
})
