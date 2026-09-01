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

test_that("normalise_loadings_prior resolves multiple distance names from data2", {
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

test_that("normalise_loadings_prior errors when features, distances, and mgp are all absent", {
  data <- make_test_data(3L)
  expect_error(
    normalise_loadings_prior(
      list(), data2 = NULL, data = data
    ),
    "at least one of"
  )
})

test_that("normalise_loadings_prior accepts pure MGP (no features / distances)", {
  data <- make_test_data(4L)
  spec <- normalise_loadings_prior(
    list(column_shrinkage = "mgp"), data2 = NULL, data = data
  )
  expect_identical(spec$column_shrinkage, "mgp")
  expect_null(spec$features_mat)
  expect_identical(spec$N_features_trend, 0L)
  expect_identical(spec$n_distances, 0L)
  expect_equal(spec$mgp_a1, 2)
  expect_equal(spec$mgp_a2, 4)
})

test_that("normalise_loadings_prior accepts string shorthand 'mgp'", {
  data <- make_test_data(4L)
  spec <- normalise_loadings_prior(
    "mgp", data2 = NULL, data = data
  )
  expect_identical(spec$column_shrinkage, "mgp")
  expect_null(spec$features_mat)
  expect_equal(spec$mgp_a1, 2)
  expect_equal(spec$mgp_a2, 4)
})

test_that("make_loadings_prior_stanvars leaves the MGP column scale to the innovations", {
  data <- make_test_data(4L)
  spec <- normalise_loadings_prior(
    list(column_shrinkage = "mgp"), data2 = NULL, data = data
  )
  sv <- make_loadings_prior_stanvars(spec)
  sc <- paste(vapply(sv, function(s) s$scode, character(1L)),
              collapse = "\n")
  # No Phi / Cholesky construction in the pure-MGP path.
  expect_false(grepl("Phi_loadings", sc, fixed = TRUE))
  expect_false(grepl("L_Phi_loadings", sc, fixed = TRUE))
  # Z is drawn at unit scale. Scaling its prior by sqrt(Psi_diag)
  # here would give the column a second scale on top of
  # `sigma_trend`, leaving only their product identified, and would
  # put that scale on a centred parameter.
  expect_match(sc, "to_vector(Z) ~ std_normal();", fixed = TRUE)
  expect_false(grepl("sqrt(Psi_diag", sc, fixed = TRUE))
  # The MGP parameters are still declared here; the cumulative
  # product itself is written by whichever block consumes it.
  expect_match(sc, "varrho_inv", fixed = TRUE)
  expect_match(sc, "inv_gamma(mgp_a1, 1)", fixed = TRUE)
})


test_that("the MGP column scale reaches the trend through sigma_trend", {
  # The end-to-end contract the split above serves: exactly one
  # declaration of the cumulative product, and `sigma_trend` derived
  # from it rather than sampled alongside it.
  set.seed(1L)
  d <- sim_mvgam(family = poisson(), n_series = 6L,
                  n_timepoints = 20L)$data_train
  m <- suppressWarnings(mvgam(
    y ~ 1, trend_formula = ~ AR(n_lv = 3), data = d, family = poisson(),
    loadings_prior = list(column_shrinkage = "mgp"), run_model = FALSE
  ))
  ln <- strsplit(paste(as.character(m$model_file %||% m$stancode),
                        collapse = "\n"), "\n")[[1]]
  expect_equal(sum(grepl("Psi_diag = exp(cumulative_sum", ln,
                          fixed = TRUE)), 1L)
  expect_true(any(grepl("sigma_trend = sqrt(Psi_diag)", ln, fixed = TRUE)))
  # It is derived, so it is not also a sampled parameter. Both
  # spellings of a sampling statement are named, because a
  # normalised program writes the density call rather than a tilde
  # and an assertion against one spelling alone passes vacuously.
  expect_false(any(grepl("] sigma_trend;", ln, fixed = TRUE)))
  expect_false(any(grepl("sigma_trend ~", ln, fixed = TRUE)))
  expect_false(any(grepl("_lpdf(sigma_trend", ln, fixed = TRUE)))
})

test_that("normalise_loadings_prior accepts column_shrinkage = 'mgp' with defaults", {
  data <- make_test_data(3L)
  d <- matrix(c(0, 1, 2, 1, 0, 3, 2, 3, 0), nrow = 3L)
  spec <- normalise_loadings_prior(
    list(distances = d, column_shrinkage = "mgp"),
    data2 = NULL, data = data
  )
  expect_equal(spec$column_shrinkage, "mgp")
  expect_equal(spec$mgp_a1, 2)
  expect_equal(spec$mgp_a2, 4)
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

test_that("normalise_loadings_prior errors on mgp hyperparams without mgp shrinkage", {
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

test_that("normalise_loadings_prior errors when distance matrix has wrong dim", {
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
    "cannot combine with a partial 'trend_map'"
  )
})

test_that("assert_loadings_prior_compatible errors on fully-fixed trend_map", {
  expect_error(
    assert_loadings_prior_compatible(
      list(features_mat = matrix(0, 3L, 1L)),
      matrix(c(1, 0, 0, 0, 1, 0), nrow = 3L)
    ),
    "cannot combine with a fully-fixed 'trend_map'"
  )
})

test_that("assert_loadings_prior_spec_consistent rejects missing fields", {
  expect_error(
    assert_loadings_prior_spec_consistent(list(features_mat = NULL)),
    "missing required fields"
  )
})

test_that("assert_loadings_prior_spec_consistent rejects bad feature dim", {
  bad <- list(
    features_mat = matrix(0, 4L, 2L),
    distance_mats = list(),
    column_shrinkage = "iid",
    mgp_a1 = NA, mgp_a2 = NA,
    n_series = 4L, N_features_trend = 5L, n_distances = 0L
  )
  expect_error(
    assert_loadings_prior_spec_consistent(bad),
    "inconsistent feature dimensions"
  )
})

test_that("assert_loadings_prior_spec_consistent rejects bad distance count", {
  bad <- list(
    features_mat = NULL,
    distance_mats = list(a = diag(0, 3L), b = diag(0, 3L)),
    column_shrinkage = "iid",
    mgp_a1 = NA, mgp_a2 = NA,
    n_series = 3L, N_features_trend = 0L, n_distances = 5L
  )
  expect_error(
    assert_loadings_prior_spec_consistent(bad),
    "inconsistent distance counts"
  )
})

test_that("assert_distance_names_unreserved rejects 'dist_' prefix", {
  expect_error(
    assert_distance_names_unreserved(c("phylo", "dist_geo")),
    "cannot start with 'dist_'"
  )
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


test_that("MGP shrinkage is refused on trends that cannot carry it", {
  # The column scale reaches the model through `sigma_trend`, which
  # only the shared-innovation path builds. VAR() constructs its own
  # innovation covariance instead, so the shrinkage would be declared
  # and never applied. Refusing beats emitting a model whose prior
  # does nothing.
  set.seed(1L)
  d <- sim_mvgam(family = poisson(), n_series = 6L,
                  n_timepoints = 20L)$data_train
  expect_error(
    mvgam(y ~ 1, trend_formula = ~ VAR(n_lv = 3), data = d,
          family = poisson(),
          loadings_prior = list(column_shrinkage = "mgp"),
          run_model = FALSE),
    "not available for 'VAR\\(\\)'"
  )
  # The trends that do carry it are unaffected.
  for (tf in list(~ AR(n_lv = 3), ~ RW(n_lv = 3), ~ ZMVN(n_lv = 3))) {
    expect_no_error(suppressWarnings(mvgam(
      y ~ 1, trend_formula = tf, data = d, family = poisson(),
      loadings_prior = list(column_shrinkage = "mgp"), run_model = FALSE
    )))
  }
  # The refusal is specific to the MGP column scale. It fires
  # during spec validation, before code generation, so it does not
  # depend on whether a VAR factor model assembles.
  expect_error(
    mvgam(y ~ 1, trend_formula = ~ VAR(n_lv = 3), data = d,
          family = poisson(),
          loadings_prior = list(column_shrinkage = "mgp"),
          run_model = FALSE),
    "shared innovation scale"
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
  sc <- stancode(mf, data = dat, family = poisson(), loadings_prior = lp)
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
                  loadings_prior = lp, prior = user)
  expect_true(grepl(stan_prior_line("theta_features", "inv_gamma(3, 2)"),
                      sc2, fixed = TRUE))
  expect_true(grepl(stan_prior_line("theta_dist_phy", "lognormal(1, 0.5)"),
                      sc2, fixed = TRUE))

  # A model with no kernel declares none, so none is reported.
  plain <- get_prior(mf, data = dat, family = poisson())
  expect_false(any(is_loadings_length_scale(plain$class)))
})
