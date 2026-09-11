test_that("a monotonic variable is read as brms codes it", {
  # brms codes a variable with D + 1 levels as 0..D in every frame. A
  # frame holding only the upper levels starts at 1, and its values
  # are read as they stand: shifting them to start at 0 would put
  # every row a level low.
  expect_identical(
    validate_monotonic_indices(c(0, 1, 2, 1, 0), "Xmo_1", 2L, 5L),
    c(0L, 1L, 2L, 1L, 0L)
  )
  expect_identical(
    validate_monotonic_indices(c(2, 2, 1), "Xmo_1", 2L, 3L),
    c(2L, 2L, 1L)
  )
  expect_error(
    validate_monotonic_indices(c(0, 3), "Xmo_1", 2L, 2L),
    "outside its coding"
  )
  expect_error(
    validate_monotonic_indices(c(0, 1), "Xmo_1", 2L, 5L),
    "does not cover"
  )
})

test_that("spd_gp_exp_quad computes spectral density correctly", {
  # brms's anisotropic `spd_gp_exp_quad()`: sdgp^2 * sqrt(2 pi)^D *
  # prod(l) * exp(-0.5 * sum(l^2 * x^2)), one row per draw.
  slambda <- array(c(1, 2, 3, 4), dim = c(2, 2))
  sdgp <- c(0.5, 0.8)
  lscale <- matrix(c(1.0, 1.5, 2.0, 2.5), nrow = 2, ncol = 2)

  result <- spd_gp_exp_quad(slambda, sdgp, lscale)
  expected <- outer(seq_len(2), seq_len(2), Vectorize(function(d, m) {
    sdgp[d]^2 * sqrt(2 * pi)^2 * prod(lscale[d, ]) *
      exp(-0.5 * sum(lscale[d, ]^2 * slambda[m, ]^2))
  }))
  expect_equal(result, expected)

  # Test input validation
  expect_error(
    spd_gp_exp_quad("not_array", sdgp, lscale),
    "Must be of type 'array'"
  )

  expect_error(
    spd_gp_exp_quad(slambda, "not_numeric", lscale),
    "Must be of type 'numeric'"
  )
})

test_that("each draw's spectral density is its own", {
  # An isotropic kernel's density at a draw depends on that draw's
  # length scale alone. The Matern kernels summed the length scale over
  # every draw, which no single-draw call can show.
  slambda <- array(c(0.5, 1, 1.5), dim = c(3, 1))
  sdgp <- c(0.5, 2)
  lscale <- matrix(c(1, 3), nrow = 2)
  for (kernel in c("exp_quad", "matern32", "matern52")) {
    both <- compute_spd_vectorized(slambda, sdgp, lscale, kernel)
    alone <- rbind(
      compute_spd_vectorized(slambda, sdgp[1], lscale[1, , drop = FALSE],
                             kernel),
      compute_spd_vectorized(slambda, sdgp[2], lscale[2, , drop = FALSE],
                             kernel)
    )
    expect_equal(both, alone)
  }
})


test_that("the Matern 3/2 density is the one brms writes in Stan", {
  # brms's `spd_gp_matern32()` for one dimension:
  # sdgp^2 * 2 * sqrt(pi) * gamma(2) * 3^1.5 / (0.5 * sqrt(pi)) * l *
  # (3 + l^2 * x^2)^-2.
  x <- c(0.5, 1, 1.5)
  sdgp <- 2
  l <- 3
  expected <- sdgp^2 * 4 * 3^1.5 * l * (3 + l^2 * x^2)^-2
  got <- compute_spd_vectorized(array(x, dim = c(3, 1)), sdgp,
                                matrix(l), "matern32")
  expect_equal(as.vector(got), sqrt(expected))
  expect_error(
    compute_spd_vectorized(array(x, dim = c(3, 1)), sdgp, matrix(l),
                           "periodic"),
    "Must be element of set"
  )
})

test_that("categorize_mvgam_parameters validates input", {
  # Test with invalid input
  expect_error(
    categorize_mvgam_parameters("not_mvgam"),
    "Must inherit from class 'mvgam'"
  )

  expect_error(
    categorize_mvgam_parameters(list(a = 1)),
    "Must inherit from class 'mvgam'"
  )
})

test_that("side_parameters() takes a fit and one of its two sides", {
  expect_error(side_parameters("not_mvgam", "obs"),
               "Must inherit from class 'mvgam'")
  expect_error(side_parameters(structure(list(), class = "mvgam"),
                               "observation"),
               "Must be element of set")
})

# ==============================================================================
# validate_prediction_factor_levels tests
# ==============================================================================

test_that("validate_prediction_factor_levels catches invalid series levels", {
  # Create metadata matching training data
  metadata <- list(
    levels = list(series = c("s1", "s2", "s3")),
    variables = list(series_var = "series")
  )

  # Valid newdata - should pass silently
  valid_newdata <- data.frame(
    time = 1:3,
    series = factor(c("s1", "s2", "s1"), levels = c("s1", "s2", "s3"))
  )
  expect_silent(validate_prediction_factor_levels(valid_newdata, metadata))

  # Invalid newdata - series level not in training
  invalid_newdata <- data.frame(
    time = 1:3,
    series = factor(c("s1", "s4", "s1"))
  )
  expect_error(
    validate_prediction_factor_levels(invalid_newdata, metadata),
    "Series levels in newdata not found in training data"
  )
})

test_that("validate_prediction_factor_levels catches invalid gr levels", {
  # Create metadata for hierarchical model
  metadata <- list(
    levels = list(
      series = c("s1"),
      gr = c("group_a", "group_b", "group_c")
    ),
    variables = list(
      series_var = "series",
      gr_var = "group"
    )
  )

  # Valid newdata - should pass silently
  valid_newdata <- data.frame(
    time = 1:3,
    series = factor("s1"),
    group = factor(c("group_a", "group_b", "group_a"))
  )
  expect_silent(validate_prediction_factor_levels(valid_newdata, metadata))

  # Invalid newdata - gr level not in training
  invalid_newdata <- data.frame(
    time = 1:3,
    series = factor("s1"),
    group = factor(c("group_a", "group_d", "group_a"))
  )
  expect_error(
    validate_prediction_factor_levels(invalid_newdata, metadata),
    "Grouping variable.*has levels not in"
  )
})

test_that("validate_prediction_factor_levels handles missing metadata gracefully", {
  newdata <- data.frame(
    time = 1:3,
    series = factor(c("s1", "s2", "s1"))
  )

  # Empty metadata - should pass silently
  expect_silent(validate_prediction_factor_levels(newdata, list()))

  # Metadata without levels - should pass silently
  expect_silent(validate_prediction_factor_levels(newdata, list(other = "stuff")))

  # Metadata with levels but no variables - should pass silently
  expect_silent(validate_prediction_factor_levels(
    newdata,
    list(levels = list(series = c("s1", "s2")))
  ))
})

test_that("validate_prediction_factor_levels validates input types", {
  metadata <- list(
    levels = list(series = c("s1", "s2")),
    variables = list(series_var = "series")
  )

  # Invalid data argument

  expect_error(
    validate_prediction_factor_levels("not_a_dataframe", metadata),
    "Must be of type 'data.frame'"
  )

  # Invalid metadata argument
  expect_error(
    validate_prediction_factor_levels(data.frame(x = 1), "not_a_list"),
    "Must be of type 'list'"
  )
})

test_that("validate_prediction_factor_levels handles character columns", {
  # Create metadata matching training data
  metadata <- list(
    levels = list(series = c("s1", "s2", "s3")),
    variables = list(series_var = "series")
  )

  # Character column (not factor) with valid values
  valid_newdata <- data.frame(
    time = 1:3,
    series = c("s1", "s2", "s1"),
    stringsAsFactors = FALSE
  )
  expect_silent(validate_prediction_factor_levels(valid_newdata, metadata))

  # Character column with invalid values
  invalid_newdata <- data.frame(
    time = 1:3,
    series = c("s1", "s4", "s1"),
    stringsAsFactors = FALSE
  )
  expect_error(
    validate_prediction_factor_levels(invalid_newdata, metadata),
    "Series levels in newdata not found in training data"
  )
})

# ==============================================================================
# compute_family_epred tests
# ==============================================================================

test_that("compute_family_epred handles simple families correctly", {
  # Create test linpred matrix
  set.seed(123)
  linpred <- matrix(rnorm(20), nrow = 4, ncol = 5)

  # Poisson family (log link): epred = exp(linpred)
  poisson_family <- list(
    family = "poisson",
    linkinv = exp
  )
  epred_poisson <- compute_family_epred(linpred, poisson_family)
  expect_equal(epred_poisson, exp(linpred))
  expect_true(all(epred_poisson >= 0))

  # Gaussian family (identity link): epred = linpred
  gaussian_family <- list(
    family = "gaussian",
    linkinv = identity
  )
  epred_gaussian <- compute_family_epred(linpred, gaussian_family)
  expect_equal(epred_gaussian, linpred)

  # Bernoulli family (logit link): epred = plogis(linpred)
  bernoulli_family <- list(
    family = "bernoulli",
    linkinv = plogis
  )
  epred_bernoulli <- compute_family_epred(linpred, bernoulli_family)
  expect_equal(epred_bernoulli, plogis(linpred))
  expect_true(all(epred_bernoulli >= 0 & epred_bernoulli <= 1))
})

test_that("compute_family_epred handles binomial with trials", {
  set.seed(123)
  linpred <- matrix(rnorm(20), nrow = 4, ncol = 5)

  binomial_family <- list(
    family = "binomial",
    linkinv = plogis
  )

  # Requires trials argument
  expect_error(
    compute_family_epred(linpred, binomial_family),
    "requires.*trials"
  )

  # With trials: epred = p * trials (column-wise via R's recycling)
  # linpred is [ndraws x nobs], trials is per-observation (per-column)
  trials <- c(10, 20, 15, 25, 30)
  epred <- compute_family_epred(linpred, binomial_family, trials = trials)

  # R recycles trials across columns (each column multiplied by its trial count)
  prob <- plogis(linpred)
  expected <- sweep(prob, 2, trials, `*`)
  expect_equal(epred, expected)
})

test_that("compute_family_epred handles lognormal with sigma", {
  set.seed(123)
  linpred <- matrix(rnorm(20, mean = 1), nrow = 4, ncol = 5)
  sigma <- matrix(abs(rnorm(20, mean = 0.5)), nrow = 4, ncol = 5)
  lognormal_family <- brms::lognormal()

  # The Jensen correction needs sigma, so the mean is refused without it
  expect_error(
    compute_family_epred(linpred, lognormal_family),
    "sigma"
  )

  # With sigma: E[Y] = exp(mu + sigma^2/2)
  epred <- compute_family_epred(linpred, lognormal_family,
                                family_pars = list(sigma = sigma))
  expected <- exp(linpred + sigma^2 / 2)
  expect_equal(epred, expected)
  expect_true(all(epred > 0))
})

test_that("compute_family_epred rejects unsupported families", {
  linpred <- matrix(rnorm(20), nrow = 4, ncol = 5)

  nmix_family <- list(
    family = "nmix",
    linkinv = exp
  )
  # The refusal names the family the dispatch was asked about, so a
  # caller can tell which one it reached this branch with.
  expect_error(
    compute_family_epred(linpred, nmix_family),
    "no mean this dispatch can compute"
  )
  expect_error(compute_family_epred(linpred, nmix_family), "'nmix'")

  # `tweedie` has an analytic epred branch (linkinv(linpred)), so
  # it routes through compute_family_epred without erroring; only
  # families whose mean reads a unit's other visits raise here.
})

test_that("compute_family_epred validates inputs", {
  linpred <- matrix(rnorm(20), nrow = 4, ncol = 5)

  # Invalid family (missing components)
  expect_error(
    compute_family_epred(linpred, list(family = "poisson")),
    "missing required components"
  )

  expect_error(
    compute_family_epred(linpred, list(linkinv = exp)),
    "missing required components"
  )

  # Invalid linpred type
  expect_error(
    compute_family_epred("not_a_matrix", list(family = "poisson", linkinv = exp)),
    "Must be of type 'matrix'"
  )

  # A distributional parameter that does not match the predictor
  wrong_sigma <- matrix(1, nrow = 2, ncol = 3)
  expect_error(
    compute_family_epred(linpred, brms::lognormal(),
                         family_pars = list(sigma = wrong_sigma)),
    "Dimension mismatch"
  )
})

test_that("a response's mean reads its own family and parameters", {
  set.seed(123)
  linpred <- list(
    count = matrix(rnorm(20), nrow = 4, ncol = 5),
    biomass = matrix(rnorm(20, mean = 1), nrow = 4, ncol = 5)
  )
  # The family beside the formula is neither response's.
  obj <- structure(list(
    formula = brms::bf(count ~ 1, family = poisson()) +
      brms::bf(biomass ~ 1, family = brms::lognormal()),
    family = gaussian(),
    data = data.frame(count = 1:5, biomass = 1:5)
  ), class = "mvgam")

  # The lognormal mean needs `sigma_biomass`, the name brms gives the
  # response's own scale. A plain `sigma` belongs to no response, and
  # asking for it was how a response named by `resp` lost its scale.
  testthat::local_mocked_bindings(
    extract_dpars_from_stanfit = function(stanfit, dpar_names, ndraws,
                                          nobs, draw_ids = NULL) {
      expect_identical(dpar_names, "sigma_biomass")
      list(sigma_biomass = matrix(0.5, nrow = ndraws, ncol = nobs))
    },
    get_combined_linpred = function(..., resp = NULL) {
      if (is.null(resp)) linpred else linpred[[resp]]
    }
  )

  one <- posterior_epred(obj, draw_ids = 1:4, resp = "biomass")
  expect_equal(one, exp(linpred$biomass + 0.5^2 / 2))

  both <- posterior_epred(obj, draw_ids = 1:4)
  expect_named(both, c("count", "biomass"))
  expect_equal(both$count, exp(linpred$count))
  expect_equal(both$biomass, one)
})

# ==============================================================================
# data2draws helper function tests
# ==============================================================================

test_that("data2draws expands vector to 2D matrix correctly", {
  # Single value expansion
  result_single <- data2draws(5, dim = c(4, 3))
  expect_true(is.matrix(result_single))
  expect_equal(dim(result_single), c(4, 3))
  expect_true(all(result_single == 5))

  # Vector expansion: each row gets the same values
  vec <- c(1, 2, 3)
  result_vec <- data2draws(vec, dim = c(4, 3))
  expect_equal(dim(result_vec), c(4, 3))
  # Each row should be c(1, 2, 3)
  for (i in 1:4) {
    expect_equal(result_vec[i, ], vec)
  }
})

test_that("data2draws expands to 3D array correctly", {
  # For categorical models with dim = c(ndraws, nobs, ncats)
  x_mat <- matrix(1:6, nrow = 2, ncol = 3)
  result <- data2draws(x_mat, dim = c(4, 2, 3))
  expect_true(is.array(result))
  expect_equal(dim(result), c(4, 2, 3))
  # Each draw slice should contain the original matrix
  for (d in 1:4) {
    expect_equal(result[d, , ], x_mat)
  }
})

test_that("data2draws validates input length for 2D", {
  # Wrong length should error
  expect_error(
    data2draws(c(1, 2), dim = c(4, 3)),
    "Length of"
  )
})

test_that("data2draws validates dimensions for 3D", {
  # Wrong dimensions for 3D input
  wrong_mat <- matrix(1:4, nrow = 2, ncol = 2)
  expect_error(
    data2draws(wrong_mat, dim = c(4, 3, 5)),
    "Dimension of.*must match"
  )
})

test_that("data2draws validates dim parameter", {
  # dim must have length 2 or 3
  expect_error(
    data2draws(5, dim = c(4)),
    "length >= 2"
  )
  expect_error(
    data2draws(5, dim = c(4, 3, 2, 1)),
    "length <= 3"
  )
})

# ==============================================================================
# dim_mu helper function tests
# ==============================================================================

test_that("dim_mu extracts correct dimensions", {
  prep <- list(ndraws = 100, nobs = 50)
  result <- dim_mu(prep)
  expect_equal(result, c(100, 50))
})

test_that("dim_mu validates prep structure", {
  expect_error(dim_mu(list(ndraws = 100)), "nobs")
  expect_error(dim_mu(list(nobs = 50)), "ndraws")
  # ndraws must be >= 1
  expect_error(dim_mu(list(ndraws = 0, nobs = 50)), "not >= 1")
  expect_error(dim_mu("not_a_list"), "Must be of type 'list'")
})

# ==============================================================================
# multiply_dpar_rate_denom helper function tests
# ==============================================================================

test_that("multiply_dpar_rate_denom returns unchanged when no rate_denom", {
  dpar <- matrix(1:12, nrow = 3, ncol = 4)
  prep <- list(data = list())
  result <- multiply_dpar_rate_denom(dpar, prep)
  expect_equal(result, dpar)
})
test_that("multiply_dpar_rate_denom applies single rate_denom", {
  dpar <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
  prep <- list(data = list(rate_denom = 10))
  result <- multiply_dpar_rate_denom(dpar, prep)
  expect_equal(result, dpar * 10)
})

test_that("multiply_dpar_rate_denom applies vector rate_denom", {
  dpar <- matrix(1:6, nrow = 2, ncol = 3)
  rate_denom <- c(10, 20, 30)
  prep <- list(data = list(rate_denom = rate_denom))
  result <- multiply_dpar_rate_denom(dpar, prep)
  # Each column multiplied by corresponding rate_denom
  expected <- dpar
  for (j in 1:3) {
    expected[, j] <- dpar[, j] * rate_denom[j]
  }
  expect_equal(result, expected)
})

# ==============================================================================
# Family-specific posterior_epred_* function tests
# ==============================================================================

test_that("posterior_epred_gaussian returns mu directly", {
  prep <- list(dpars = list(mu = matrix(1:6, nrow = 2, ncol = 3)))
  result <- posterior_epred_gaussian(prep)
  expect_equal(result, prep$dpars$mu)
})

test_that("posterior_epred_student returns mu directly", {
  prep <- list(dpars = list(mu = matrix(1:6, nrow = 2, ncol = 3)))
  result <- posterior_epred_student(prep)
  expect_equal(result, prep$dpars$mu)
})

test_that("posterior_epred_beta returns mu directly", {
  prep <- list(dpars = list(mu = matrix(runif(6), nrow = 2, ncol = 3)))
  result <- posterior_epred_beta(prep)
  expect_equal(result, prep$dpars$mu)
})

test_that("posterior_epred_bernoulli returns mu directly", {
  prep <- list(dpars = list(mu = matrix(runif(6), nrow = 2, ncol = 3)))
  result <- posterior_epred_bernoulli(prep)
  expect_equal(result, prep$dpars$mu)
})

test_that("posterior_epred_poisson applies rate_denom", {
  mu <- matrix(c(5, 10, 15, 20), nrow = 2, ncol = 2)
  prep <- list(
    dpars = list(mu = mu),
    data = list(rate_denom = c(2, 3))
  )
  result <- posterior_epred_poisson(prep)
  # Column 1 * 2, column 2 * 3
  expected <- mu
  expected[, 1] <- mu[, 1] * 2
  expected[, 2] <- mu[, 2] * 3
  expect_equal(result, expected)
})

test_that("posterior_epred_poisson returns mu when no rate_denom", {
  mu <- matrix(c(5, 10, 15, 20), nrow = 2, ncol = 2)
  prep <- list(
    dpars = list(mu = mu),
    data = list()
  )
  result <- posterior_epred_poisson(prep)
  expect_equal(result, mu)
})

test_that("posterior_epred_negbinomial applies rate_denom", {
  mu <- matrix(c(5, 10), nrow = 1, ncol = 2)
  prep <- list(
    dpars = list(mu = mu),
    data = list(rate_denom = 10)
  )
  result <- posterior_epred_negbinomial(prep)
  expect_equal(result, mu * 10)
})

test_that("posterior_epred_lognormal computes E[Y] = exp(mu + sigma^2/2)", {
  mu <- matrix(c(0, 1, 2, 3), nrow = 2, ncol = 2)
  sigma <- matrix(c(0.5, 0.5, 1, 1), nrow = 2, ncol = 2)
  prep <- list(dpars = list(mu = mu, sigma = sigma))
  result <- posterior_epred_lognormal(prep)
  expected <- exp(mu + sigma^2 / 2)
  expect_equal(result, expected)
})

test_that("posterior_epred_shifted_lognormal adds ndt shift", {
  mu <- matrix(c(0, 1), nrow = 1, ncol = 2)
  sigma <- matrix(c(0.5, 0.5), nrow = 1, ncol = 2)
  ndt <- matrix(c(0.1, 0.2), nrow = 1, ncol = 2)
  prep <- list(dpars = list(mu = mu, sigma = sigma, ndt = ndt))
  result <- posterior_epred_shifted_lognormal(prep)
  expected <- exp(mu + sigma^2 / 2) + ndt
  expect_equal(result, expected)
})

test_that("posterior_epred_binomial multiplies mu by trials", {
  mu <- matrix(c(0.2, 0.5, 0.8, 0.3), nrow = 2, ncol = 2)
  trials <- c(10, 20)
  prep <- list(
    dpars = list(mu = mu),
    data = list(trials = trials),
    ndraws = 2,
    nobs = 2
  )
  result <- posterior_epred_binomial(prep)
  # Each column multiplied by corresponding trial count
  expected <- mu
  expected[, 1] <- mu[, 1] * 10
  expected[, 2] <- mu[, 2] * 20
  expect_equal(result, expected)
})

test_that("posterior_epred_beta_binomial multiplies mu by trials", {
  mu <- matrix(c(0.2, 0.5), nrow = 1, ncol = 2)
  trials <- c(10, 20)
  prep <- list(
    dpars = list(mu = mu),
    data = list(trials = trials),
    ndraws = 1,
    nobs = 2
  )
  result <- posterior_epred_beta_binomial(prep)
  expect_equal(result[1, 1], 0.2 * 10)
  expect_equal(result[1, 2], 0.5 * 20)
})

# ==============================================================================
# Zero-inflated family tests
# ==============================================================================

test_that("posterior_epred_zero_inflated_poisson applies zi correction", {
  mu <- matrix(c(5, 10, 15, 20), nrow = 2, ncol = 2)
  zi <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2, ncol = 2)
  prep <- list(dpars = list(mu = mu, zi = zi))
  result <- posterior_epred_zero_inflated_poisson(prep)
  expected <- mu * (1 - zi)
  expect_equal(result, expected)
})

test_that("posterior_epred_zero_inflated_negbinomial applies zi correction", {
  mu <- matrix(c(5, 10), nrow = 1, ncol = 2)
  zi <- matrix(c(0.2, 0.3), nrow = 1, ncol = 2)
  prep <- list(dpars = list(mu = mu, zi = zi))
  result <- posterior_epred_zero_inflated_negbinomial(prep)
  expected <- mu * (1 - zi)
  expect_equal(result, expected)
})

test_that("posterior_epred_zero_inflated_binomial applies zi and trials", {
  mu <- matrix(c(0.5, 0.8), nrow = 1, ncol = 2)
  zi <- matrix(c(0.1, 0.2), nrow = 1, ncol = 2)
  trials <- c(10, 20)
  prep <- list(
    dpars = list(mu = mu, zi = zi),
    data = list(trials = trials),
    ndraws = 1,
    nobs = 2
  )
  result <- posterior_epred_zero_inflated_binomial(prep)
  # E[Y] = mu * trials * (1 - zi)
  expect_equal(result[1, 1], 0.5 * 10 * (1 - 0.1))
  expect_equal(result[1, 2], 0.8 * 20 * (1 - 0.2))
})

test_that("posterior_epred_zero_inflated_beta applies zi correction", {
  mu <- matrix(c(0.3, 0.6), nrow = 1, ncol = 2)
  zi <- matrix(c(0.1, 0.2), nrow = 1, ncol = 2)
  prep <- list(dpars = list(mu = mu, zi = zi))
  result <- posterior_epred_zero_inflated_beta(prep)
  expected <- mu * (1 - zi)
  expect_equal(result, expected)
})

test_that("posterior_epred_zero_one_inflated_beta computes correctly", {
  mu <- matrix(c(0.5, 0.6), nrow = 1, ncol = 2)
  zoi <- matrix(c(0.2, 0.3), nrow = 1, ncol = 2)
  coi <- matrix(c(0.4, 0.5), nrow = 1, ncol = 2)
  prep <- list(dpars = list(mu = mu, zoi = zoi, coi = coi))
  result <- posterior_epred_zero_one_inflated_beta(prep)
  # E[Y] = zoi * coi + mu * (1 - zoi)
  expected <- zoi * coi + mu * (1 - zoi)
  expect_equal(result, expected)
})

# ==============================================================================
# Hurdle family tests
# ==============================================================================

test_that("posterior_epred_hurdle_poisson computes E[Y|Y>0] * P(Y>0)", {
  mu <- matrix(c(2, 5), nrow = 1, ncol = 2)
  hu <- matrix(c(0.2, 0.3), nrow = 1, ncol = 2)
  prep <- list(dpars = list(mu = mu, hu = hu))
  result <- posterior_epred_hurdle_poisson(prep)
  # E[Y] = mu / (1 - exp(-mu)) * (1 - hu)
  expected <- mu / (1 - exp(-mu)) * (1 - hu)
  expect_equal(result, expected)
})

test_that("posterior_epred_hurdle_negbinomial uses shape parameter", {
  mu <- matrix(c(5, 10), nrow = 1, ncol = 2)
  hu <- matrix(c(0.2, 0.3), nrow = 1, ncol = 2)
  shape <- matrix(c(2, 3), nrow = 1, ncol = 2)
  prep <- list(dpars = list(mu = mu, hu = hu, shape = shape))
  result <- posterior_epred_hurdle_negbinomial(prep)
  # E[Y] = mu / (1 - (shape/(mu+shape))^shape) * (1 - hu)
  expected <- mu / (1 - (shape / (mu + shape))^shape) * (1 - hu)
  expect_equal(result, expected)
})

test_that("posterior_epred_hurdle_gamma applies hu correction", {
  mu <- matrix(c(5, 10), nrow = 1, ncol = 2)
  hu <- matrix(c(0.2, 0.3), nrow = 1, ncol = 2)
  prep <- list(dpars = list(mu = mu, hu = hu))
  result <- posterior_epred_hurdle_gamma(prep)
  expected <- mu * (1 - hu)
  expect_equal(result, expected)
})

test_that("posterior_epred_hurdle_lognormal combines lognormal E[Y] with hu", {
  mu <- matrix(c(1, 2), nrow = 1, ncol = 2)
  sigma <- matrix(c(0.5, 0.5), nrow = 1, ncol = 2)
  hu <- matrix(c(0.2, 0.3), nrow = 1, ncol = 2)
  prep <- list(dpars = list(mu = mu, sigma = sigma, hu = hu))
  result <- posterior_epred_hurdle_lognormal(prep)
  # E[Y] = exp(mu + sigma^2/2) * (1 - hu)
  expected <- exp(mu + sigma^2 / 2) * (1 - hu)
  expect_equal(result, expected)
})

# ==============================================================================
# Complex distribution mean helper tests
# ==============================================================================

test_that("mean_discrete_weibull computes series approximation", {
  # Test with simple inputs where series converges quickly
  mu <- matrix(c(0.3, 0.5), nrow = 1, ncol = 2)
  shape <- matrix(c(1, 1.5), nrow = 1, ncol = 2)
  result <- mean_discrete_weibull(mu, shape)
  expect_true(is.matrix(result))
  expect_equal(dim(result), c(1, 2))
  # Result should be positive for valid inputs
  expect_true(all(result >= 0))
})

test_that("mean_com_poisson returns mu when shape = 1 (Poisson)", {
  mu <- matrix(c(3, 5, 7), nrow = 1, ncol = 3)
  shape <- 1
  result <- mean_com_poisson(mu, shape)
  # When shape = 1, COM-Poisson reduces to Poisson with E[Y] = mu
  # Result preserves mu's dimensions so compare values ignoring attributes
  expect_equal(as.numeric(result), as.numeric(mu))
})

test_that("mean_com_poisson uses approximation for large mu", {
  # Large mu values should trigger closed-form approximation
  mu <- matrix(c(10, 20), nrow = 1, ncol = 2)
  shape <- matrix(c(2, 2), nrow = 1, ncol = 2)
  result <- mean_com_poisson(mu, shape)
  expect_true(all(is.finite(result)))
  expect_true(all(result > 0))
})

test_that("mean_com_poisson validates shape parameter", {
  mu <- matrix(c(3, 5), nrow = 1, ncol = 2)
  expect_error(
    mean_com_poisson(mu, shape = 0),
    "shape must be positive"
  )
  expect_error(
    mean_com_poisson(mu, shape = Inf),
    "shape must be finite"
  )
})

# ==============================================================================
# Additional family edge cases
# ==============================================================================

test_that("posterior_epred_gen_extreme_value uses xi parameter", {
  mu <- matrix(c(0, 1), nrow = 1, ncol = 2)
  sigma <- matrix(c(1, 1), nrow = 1, ncol = 2)
  xi <- matrix(c(0.1, 0.2), nrow = 1, ncol = 2)
  prep <- list(dpars = list(mu = mu, sigma = sigma, xi = xi))
  result <- posterior_epred_gen_extreme_value(prep)
  expected <- mu + sigma * (gamma(1 - xi) - 1) / xi
  expect_equal(result, expected)
})

test_that("posterior_epred_asym_laplace uses quantile parameter", {
  mu <- matrix(c(0, 1), nrow = 1, ncol = 2)
  sigma <- matrix(c(1, 1), nrow = 1, ncol = 2)
  quantile <- matrix(c(0.25, 0.75), nrow = 1, ncol = 2)
  prep <- list(dpars = list(mu = mu, sigma = sigma, quantile = quantile))
  result <- posterior_epred_asym_laplace(prep)
  expected <- mu + sigma * (1 - 2 * quantile) / (quantile * (1 - quantile))
  expect_equal(result, expected)
})

# ==============================================================================
# extract_dpars_from_stanfit tests
# ==============================================================================

test_that("extract_dpars_from_stanfit extracts and broadcasts scalar params", {
  # Create mock draws with scalar sigma parameter
  set.seed(123)
  draws_mat <- matrix(
    c(rnorm(10), abs(rnorm(10, mean = 1))),
    nrow = 10, ncol = 2
  )
  colnames(draws_mat) <- c("b_Intercept", "sigma")
  mock_draws <- posterior::as_draws_matrix(draws_mat)

  # Extract sigma with nobs = 5 (should broadcast to [10 x 5])
  result <- extract_dpars_from_stanfit(
    mock_draws,
    dpar_names = "sigma",
    ndraws = 10,
    nobs = 5
  )

  expect_true(is.list(result))
  expect_named(result, "sigma")
  expect_true(is.matrix(result$sigma))
  expect_equal(dim(result$sigma), c(10, 5))

  # Broadcasting with byrow=FALSE replicates scalar across columns
  for (col in 1:5) {
    expect_equal(result$sigma[, col], draws_mat[, "sigma"])
  }

  # Also test single indexed param broadcasts like scalar
  draws_single_idx <- matrix(abs(rnorm(10, mean = 1)), nrow = 10, ncol = 1)
  colnames(draws_single_idx) <- "zi[1]"
  mock_single <- posterior::as_draws_matrix(draws_single_idx)

  result_single <- extract_dpars_from_stanfit(
    mock_single,
    dpar_names = "zi",
    ndraws = 10,
    nobs = 4
  )

  expect_equal(dim(result_single$zi), c(10, 4))
  # All columns should be identical (broadcasted from single indexed value)
  for (col in 2:4) {
    expect_equal(result_single$zi[, col], result_single$zi[, 1])
  }
})

test_that("extract_dpars_from_stanfit handles indexed and missing params", {
  # Create mock draws with indexed zi parameter and scalar sigma
  set.seed(456)
  ndraws <- 8
  nobs <- 3
  zi_vals <- matrix(runif(ndraws * nobs, 0, 0.5), nrow = ndraws, ncol = nobs)

  draws_mat <- cbind(
    matrix(rnorm(ndraws), ncol = 1),
    abs(rnorm(ndraws, mean = 1)),
    zi_vals
  )
  colnames(draws_mat) <- c("b_Intercept", "sigma", "zi[1]", "zi[2]", "zi[3]")
  mock_draws <- posterior::as_draws_matrix(draws_mat)

  # Extract both existing params
  result <- extract_dpars_from_stanfit(
    mock_draws,
    dpar_names = c("sigma", "zi"),
    ndraws = ndraws,
    nobs = nobs
  )

  expect_true(is.list(result))
  expect_named(result, c("sigma", "zi"))

  # sigma is scalar, should be broadcasted
  expect_equal(dim(result$sigma), c(ndraws, nobs))

  # zi is indexed, should match original values
  expect_equal(dim(result$zi), c(ndraws, nobs))
  expect_equal(as.numeric(result$zi), as.numeric(zi_vals))

  # Request shape (missing) - should return NULL
  result_missing <- extract_dpars_from_stanfit(
    mock_draws,
    dpar_names = c("sigma", "shape"),
    ndraws = ndraws,
    nobs = nobs
  )
  expect_true(is.matrix(result_missing$sigma))
  expect_null(result_missing$shape)

  # Empty dpar_names should return empty list
  result_empty <- extract_dpars_from_stanfit(
    mock_draws,
    dpar_names = character(0),
    ndraws = ndraws,
    nobs = nobs
  )
  expect_equal(result_empty, list())
})

test_that("an indexed parameter on another axis is not read per row", {
  # Two columns against three rows: a per-factor scale, say. Taking the
  # first column put that scale on every row.
  draws <- posterior::as_draws_matrix(
    cbind(`sigma[1]` = c(0.2, 0.3), `sigma[2]` = c(0.4, 0.5))
  )
  expect_error(
    extract_dpars_from_stanfit(draws, "sigma", ndraws = 2, nobs = 3),
    "cannot be read one value per row"
  )
})

test_that("extract_dpars_from_stanfit validates inputs correctly", {
  # Create valid mock draws for testing
  set.seed(789)
  draws_mat <- matrix(abs(rnorm(20, mean = 1)), nrow = 10, ncol = 2)
  colnames(draws_mat) <- c("b_Intercept", "sigma")
  valid_draws <- posterior::as_draws_matrix(draws_mat)

  # Invalid stanfit type
  expect_error(
    extract_dpars_from_stanfit("not_draws", "sigma", 10, 5),
    "Must inherit from class"
  )

  # ndraws must be >= 1
  expect_error(
    extract_dpars_from_stanfit(valid_draws, "sigma", 0, 5),
    "not >= 1"
  )

  # nobs must be >= 1
  expect_error(
    extract_dpars_from_stanfit(valid_draws, "sigma", 10, 0),
    "not >= 1"
  )

  # ndraws exceeds available draws
  expect_error(
    extract_dpars_from_stanfit(valid_draws, "sigma", 100, 5),
    "more draws than the posterior holds"
  )

  # draw_ids exceeds available draws
  expect_error(
    extract_dpars_from_stanfit(valid_draws, "sigma", 10, 5, draw_ids = 1:100),
    "exceed available draws"
  )

  # draw_ids with valid subset should work
  result <- extract_dpars_from_stanfit(
    valid_draws,
    dpar_names = "sigma",
    ndraws = 10,
    nobs = 3,
    draw_ids = c(1, 3, 5)
  )
  expect_equal(dim(result$sigma), c(3, 3))
})

# ==============================================================================
# Truncation helper function tests
# ==============================================================================

test_that("extract_truncation_bounds handles NULL and constant bounds", {
  # Mock mvgam object with no truncation bounds in standata
  mock_no_trunc <- structure(
    list(standata = list(N = 10)),
    class = "mvgam"
  )
  result_none <- extract_truncation_bounds(mock_no_trunc, nobs = 10)
  expect_null(result_none$lb)
  expect_null(result_none$ub)

  # Mock mvgam object with constant lb=0, ub=100 in standata
  mock_trunc <- structure(
    list(standata = list(N = 10, lb = 0, ub = 100)),
    class = "mvgam"
  )
  result_const <- extract_truncation_bounds(mock_trunc, nobs = 5)
  expect_equal(result_const$lb, rep(0, 5))
  expect_equal(result_const$ub, rep(100, 5))
})

test_that("apply_truncation clamps samples to specified bounds", {
  set.seed(42)
  ndraws <- 50
  nobs <- 4

  # Normal samples centered at 5 with sd=3 will have values outside [0, 10]
  samples <- matrix(
    rnorm(ndraws * nobs, mean = 5, sd = 3),
    nrow = ndraws,
    ncol = nobs
  )

  result <- apply_truncation(
    samples = samples,
    lb = 0,
    ub = 10,
    ntrys = 5,
    ndraws = ndraws,
    nobs = nobs,
    redraw = function() matrix(rnorm(ndraws * nobs, mean = 5, sd = 3),
                                nrow = ndraws, ncol = nobs)
  )

  # All values should be within [0, 10] after truncation
  expect_true(all(result >= 0))
  expect_true(all(result <= 10))
  expect_equal(dim(result), c(ndraws, nobs))
})

test_that("truncation reaches sample_from_family without erroring", {
  # The unit tests below drive `apply_truncation()` directly, which
  # left the wiring between it and the family sampler uncovered: a
  # signature change to one passed every test while any truncated
  # model errored. This drives the entry point instead.
  set.seed(3)
  ndraws <- 40L
  epred <- matrix(5, nrow = ndraws, ncol = 3L)
  out <- sample_from_family(
    family_name = "gaussian", ndraws = ndraws, epred = epred,
    sigma = matrix(3, nrow = ndraws, ncol = 3L),
    lb = 0, ub = 10, ntrys = 5
  )
  # The family samplers answer with a vector the callers reshape, so
  # the length is what this checks rather than a dimension.
  expect_length(out, ndraws * 3L)
  expect_true(all(out >= 0 & out <= 10))

  # A count family takes the same route and must keep the lower bound
  # attainable rather than excluding it.
  counts <- sample_from_family(
    family_name = "poisson", ndraws = ndraws,
    epred = matrix(2, nrow = ndraws, ncol = 3L),
    lb = 1, ub = Inf, ntrys = 5
  )
  expect_true(all(counts >= 1))
  expect_true(any(counts == 1))
})


test_that("a truncated draw follows the truncated distribution", {
  # Replacing an out-of-bounds draw by inverting the distribution
  # restricted to the bounds is exact, so the result is the truncated
  # law itself rather than something merely inside the bounds. The
  # earlier rejection sampler could not place every draw and clamped
  # the remainder onto the bound, which piles mass there.
  set.seed(8)
  ndraws <- 20000L
  nobs <- 2L
  mu <- matrix(5, ndraws, nobs)
  spec <- family_dist_spec("gaussian", "identity", mu,
                           list(sigma = matrix(3, ndraws, nobs)), NULL)
  draw <- function() matrix(rnorm(ndraws * nobs, 5, 3), nrow = ndraws)
  out <- apply_truncation(
    draw(), lb = 0, ub = Inf, ntrys = 5, ndraws = ndraws,
    nobs = nobs, redraw = draw, spec = spec, discrete = FALSE
  )
  v <- as.numeric(out[, 1L])
  expect_true(all(v >= 0))
  # Nothing sits exactly on the bound, which is the clamping tell.
  expect_identical(sum(v == 0), 0L)
  probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  p0 <- stats::pnorm(0, 5, 3)
  truth <- stats::qnorm(p0 + probs * (1 - p0), 5, 3)
  expect_lt(max(abs(stats::quantile(v, probs) - truth)), 0.15)
})


test_that("a replaced draw comes from the fit's own predictive", {
  # The replacement for an out-of-bounds draw has to be drawn from
  # the observation's own predictive. Resolving the distribution by
  # name and calling it separately meant calling it with no
  # parameters, so every replacement came from the standard member of
  # the family. Here the predictive sits near 50 and the bound is at
  # zero, so a replacement from a standard normal is unmissable: it
  # lands within a few units of zero rather than near 50.
  set.seed(11)
  ndraws <- 200
  nobs <- 3
  centre <- 50
  samples <- matrix(rnorm(ndraws * nobs, mean = centre, sd = 5),
                    nrow = ndraws, ncol = nobs)
  planted <- 1:20
  samples[planted, ] <- -3

  result <- apply_truncation(
    samples = samples,
    lb = 0,
    ub = Inf,
    ntrys = 5,
    ndraws = ndraws,
    nobs = nobs,
    redraw = function() matrix(rnorm(ndraws * nobs, mean = centre,
                                      sd = 5),
                                nrow = ndraws, ncol = nobs)
  )

  replaced <- result[planted, ]
  expect_true(all(replaced >= 0))
  # Six standard deviations of the predictive still leaves a wide
  # gap between it and the standard normal the fault drew from.
  expect_true(all(replaced > centre - 30))
  expect_gt(min(replaced), 5)

  # The draws that were already inside the bounds are untouched.
  kept <- setdiff(seq_len(ndraws), planted)
  expect_identical(result[kept, ], samples[kept, ])
})

# Tests for summarize_predictions() ------------------------------------------

test_that("summarize_predictions computes correct statistics", {
  set.seed(123)
  draws <- matrix(rnorm(100 * 5, mean = 5, sd = 2), nrow = 100)

  # Non-robust summary (mean/sd)
  result <- summarize_predictions(draws, probs = c(0.025, 0.975), robust = FALSE)

  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 4)
  expect_equal(colnames(result), c("Estimate", "Est.Error", "Q2.5", "Q97.5"))
  expect_equal(result[, "Estimate"], colMeans(draws), tolerance = 1e-10)
  expect_equal(result[, "Est.Error"], apply(draws, 2, sd), tolerance = 1e-10)
})

test_that("summarize_predictions handles robust = TRUE", {
  set.seed(456)
  draws <- matrix(rnorm(100 * 3, mean = 10, sd = 3), nrow = 100)

  result <- summarize_predictions(draws, probs = c(0.1, 0.9), robust = TRUE)

  expect_equal(colnames(result), c("Estimate", "Est.Error", "Q10", "Q90"))
  expect_equal(result[, "Estimate"], apply(draws, 2, median), tolerance = 1e-10)
  expect_equal(result[, "Est.Error"], apply(draws, 2, mad), tolerance = 1e-10)
})

test_that("summarize_predictions handles single and multiple quantiles", {
  draws <- matrix(rnorm(50 * 4), nrow = 50)

  # Single quantile
  result1 <- summarize_predictions(draws, probs = 0.5, robust = FALSE)
  expect_equal(ncol(result1), 3)
  expect_equal(colnames(result1), c("Estimate", "Est.Error", "Q50"))

  # Multiple quantiles
  result2 <- summarize_predictions(
    draws,
    probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
    robust = FALSE
  )
  expect_equal(ncol(result2), 7)
})


# predict.mvgam type= dispatch. Each type routes to a different
# posterior_* method (or, for type = "variance", to predict_variance()).
# Tests use mocked S3 methods so the dispatcher can be exercised
# without a real Stan fit.

predict_stub_obj <- function() structure(list(), class = "mvgam")

test_that("predict.mvgam(type = 'response') routes to posterior_predict", {
  called <- list()
  sentinel <- matrix(rnorm(4 * 3), 4, 3)
  testthat::local_mocked_bindings(
    posterior_predict.mvgam = function(object, ...) {
      called$response <<- TRUE
      sentinel
    },
    .package = "mvgam"
  )
  out <- predict.mvgam(predict_stub_obj(), summary = FALSE)
  expect_true(isTRUE(called$response))
  expect_identical(out, sentinel)
})


test_that("predict.mvgam(type = 'link') routes to posterior_linpred", {
  called <- list()
  sentinel <- matrix(rnorm(4 * 3), 4, 3)
  testthat::local_mocked_bindings(
    posterior_linpred.mvgam = function(object, ...) {
      called$link <<- TRUE
      called$dots <<- list(...)
      sentinel
    },
    .package = "mvgam"
  )
  out <- predict.mvgam(predict_stub_obj(), type = "link", summary = FALSE)
  expect_true(isTRUE(called$link))
  expect_identical(out, sentinel)
  # transform should be hard-coded FALSE for the link route
  expect_identical(called$dots$transform, FALSE)
})


test_that("predict.mvgam(type = 'expected') routes to posterior_epred", {
  called <- list()
  sentinel <- matrix(rnorm(4 * 3), 4, 3)
  testthat::local_mocked_bindings(
    posterior_epred.mvgam = function(object, ...) {
      called$expected <<- TRUE
      sentinel
    },
    .package = "mvgam"
  )
  out <- predict.mvgam(predict_stub_obj(), type = "expected", summary = FALSE)
  expect_true(isTRUE(called$expected))
  expect_identical(out, sentinel)
})


test_that("predict.mvgam(type = 'terms') errors with a redirect", {
  expect_error(
    predict.mvgam(predict_stub_obj(), type = "terms"),
    "no single equivalent"
  )
  expect_error(
    predict.mvgam(predict_stub_obj(), type = "terms"),
    "posterior_smooths"
  )
})


test_that("predict.mvgam(type = 'latent_state'/'detection') errors clearly", {
  expect_error(
    predict.mvgam(predict_stub_obj(), type = "latent_state"),
    "not available for this family"
  )
  expect_error(
    predict.mvgam(predict_stub_obj(), type = "detection"),
    "not available for this family"
  )
})


# compute_family_variance: family-by-family variance formulas.

test_that("compute_family_variance: gaussian returns sigma^2", {
  mu <- matrix(rnorm(6), 2, 3)
  sigma <- matrix(c(1, 2, 3, 4, 5, 6), 2, 3)
  v <- compute_family_variance(
    mu = mu, family = gaussian(), sigma = sigma
  )
  expect_equal(v, sigma^2)
})


test_that("compute_family_variance: poisson returns mu", {
  mu <- matrix(c(1, 2, 3, 4, 5, 6), 2, 3)
  v <- compute_family_variance(mu = mu, family = poisson())
  expect_equal(v, mu)
})


test_that("compute_family_variance: bernoulli returns mu*(1-mu)", {
  mu <- matrix(c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6), 2, 3)
  v <- compute_family_variance(
    mu = mu,
    family = list(family = "bernoulli", linkinv = plogis)
  )
  expect_equal(v, mu * (1 - mu))
})


test_that("compute_family_variance: binomial uses trials per obs", {
  mu <- matrix(c(2, 4, 6), 1, 3)
  trials <- c(10, 10, 10)
  v <- compute_family_variance(
    mu = mu,
    family = list(family = "binomial", linkinv = plogis),
    trials = trials
  )
  # p = mu/trials; Var = trials*p*(1-p)
  p <- mu / 10
  expect_equal(v, p * (1 - p) * 10)
})


test_that("compute_family_variance: negbinomial uses mu + mu^2/shape", {
  mu <- matrix(c(1, 2, 3, 4), 2, 2)
  shape <- matrix(c(2, 4, 6, 8), 2, 2)
  v <- compute_family_variance(
    mu = mu,
    family = list(family = "negbinomial", linkinv = exp),
    shape = shape
  )
  expect_equal(v, mu + mu^2 / shape)
})


test_that("compute_family_variance: gamma uses mu^2/shape", {
  mu <- matrix(c(2, 3, 4, 5), 2, 2)
  shape <- matrix(c(1, 2, 3, 4), 2, 2)
  v <- compute_family_variance(
    mu = mu,
    family = list(family = "gamma", linkinv = exp),
    shape = shape
  )
  expect_equal(v, mu^2 / shape)
})


test_that("compute_family_variance: beta uses mu(1-mu)/(1+phi)", {
  mu <- matrix(c(0.1, 0.2, 0.3, 0.4), 2, 2)
  phi <- matrix(c(2, 4, 6, 8), 2, 2)
  v <- compute_family_variance(
    mu = mu,
    family = list(family = "beta", linkinv = plogis),
    phi = phi
  )
  expect_equal(v, mu * (1 - mu) / (1 + phi))
})


test_that("compute_family_variance: student uses sigma^2 * nu/(nu-2)", {
  mu <- matrix(c(1, 2, 3, 4), 2, 2)
  sigma <- matrix(c(1, 2, 1, 2), 2, 2)
  nu <- matrix(c(5, 10, 5, 10), 2, 2)
  v <- compute_family_variance(
    mu = mu,
    family = list(family = "student", linkinv = identity),
    sigma = sigma, nu = nu
  )
  expect_equal(v, sigma^2 * nu / (nu - 2))
})


test_that("compute_family_variance: student returns Inf when nu <= 2", {
  mu <- matrix(1, 2, 2)
  sigma <- matrix(1, 2, 2)
  # Column-major fill: nu[1,1]=1.5, nu[2,1]=2.0, nu[1,2]=5.0, nu[2,2]=10.
  nu <- matrix(c(1.5, 2.0, 5.0, 10.0), 2, 2)
  v <- compute_family_variance(
    mu = mu,
    family = list(family = "student", linkinv = identity),
    sigma = sigma, nu = nu
  )
  expect_true(is.infinite(v[1, 1]))  # nu = 1.5
  expect_true(is.infinite(v[2, 1]))  # nu = 2.0 (boundary)
  expect_true(is.finite(v[1, 2]))    # nu = 5.0
  expect_true(is.finite(v[2, 2]))    # nu = 10.0
})


test_that("compute_family_variance: lognormal uses mu^2 * (exp(sigma^2) - 1)", {
  # mu here is the response-scale mean E[Y] = exp(meanlog + sdlog^2/2).
  mu <- matrix(c(2, 4, 6, 8), 2, 2)
  sigma <- matrix(c(0.5, 1, 0.5, 1), 2, 2)
  v <- compute_family_variance(
    mu = mu,
    family = list(family = "lognormal", linkinv = identity),
    sigma = sigma
  )
  expect_equal(v, mu^2 * (exp(sigma^2) - 1))
})


test_that("compute_family_variance: errors for unsupported family", {
  mu <- matrix(1, 2, 2)
  expect_error(
    compute_family_variance(
      mu = mu,
      family = list(family = "tweedie", linkinv = exp)
    ),
    "not implemented"
  )
})


test_that("compute_family_variance: errors with dim mismatch on dpar", {
  mu <- matrix(1, 2, 3)
  bad_sigma <- matrix(1, 2, 2)
  expect_error(
    compute_family_variance(
      mu = mu, family = gaussian(), sigma = bad_sigma
    ),
    "Dimension mismatch"
  )
})


test_that("compute_family_variance: errors when required dpar missing", {
  mu <- matrix(1, 2, 2)
  expect_error(
    compute_family_variance(
      mu = mu,
      family = list(family = "negbinomial", linkinv = exp)
    ),
    "shape"
  )
})


test_that("a denominator of zero is a legal binomial observation", {
  # A row whose response was never observed has no trials behind it,
  # and zero is the natural padding: brms accepts it when fitting, so
  # prediction has to accept it coming back out. Requiring at least
  # one trial made posterior_epred(), loo(), pp_check() and plot()
  # fail together on any fit padded that way.
  stub <- structure(
    list(
      formula = brms::bf(y | trials(trials) ~ x),
      data = data.frame(
        y = c(1L, 2L, NA_integer_), trials = c(5L, 5L, 0L),
        x = rnorm(3)
      ),
      standata = list(trials = c(5L, 5L))
    ),
    class = c("mvgam", "brmsfit")
  )
  expect_equal(
    extract_trials_for_family(stub, binomial(), newdata = NULL),
    c(5, 5, 0)
  )
  # com_binomial() reaches the same extractor.
  expect_equal(
    extract_trials_for_family(stub, com_binomial(), newdata = NULL),
    c(5, 5, 0)
  )
  # A negative denominator is still refused, matching brms.
  stub$data$trials <- c(5L, 5L, -1L)
  expect_error(
    extract_trials_for_family(stub, binomial(), newdata = NULL),
    "trials"
  )
})

# A family whose mean is not the inverse link of its predictor has that
# mean written twice: once in the per-family kernel copied from brms,
# and once in the dispatch `compute_family_epred()` runs. The two are
# free to disagree, and did: eight families returned the base
# distribution's parameter as E[Y] while their kernels, tested on their
# own, computed the right thing. This drives both and requires them to
# agree, so a family cannot be added to one and not the other.
epred_kernel_for <- function(family_name) {
  tryCatch(
    get(paste0("posterior_epred_", family_name),
        envir = asNamespace("mvgam"), mode = "function"),
    error = function(e) NULL
  )
}

test_that("compute_family_epred agrees with every family's own mean kernel", {
  dpar_value <- function(dpar) {
    switch(dpar,
      sigma = 0.7, shape = 2.5, phi = 4, nu = 6, hu = 0.3, zi = 0.25,
      zoi = 0.2, coi = 0.6, beta = 1.3, alpha = 0.5, kappa = 2,
      xi = 0.1, quantile = 0.5, ndt = 0.2, 0.5)
  }
  families <- c(
    "gaussian", "poisson", "binomial", "bernoulli", "negbinomial",
    "student", "lognormal", "beta", "beta_binomial", "exponential",
    "weibull", "geometric", "skew_normal", "exgaussian",
    "hurdle_poisson", "hurdle_negbinomial", "hurdle_gamma",
    "hurdle_lognormal", "zero_inflated_poisson",
    "zero_inflated_negbinomial", "zero_inflated_binomial",
    "zero_inflated_beta", "zero_inflated_beta_binomial",
    "zero_one_inflated_beta", "von_mises", "frechet",
    "gen_extreme_value", "asym_laplace", "shifted_lognormal"
  )
  set.seed(19)
  ndraws <- 3L
  nobs <- 4L
  trials <- rep(10, nobs)
  compared <- 0L

  for (family_name in families) {
    kernel <- epred_kernel_for(family_name)
    if (is.null(kernel)) next
    family <- brms::brmsfamily(family_name)
    eta <- matrix(stats::rnorm(ndraws * nobs, 0.3, 0.2), ndraws, nobs)
    extra <- setdiff(family$dpars, "mu")
    dpars <- c(
      list(mu = family$linkinv(eta)),
      stats::setNames(
        lapply(extra, function(d) matrix(dpar_value(d), ndraws, nobs)),
        extra
      )
    )
    from_kernel <- kernel(list(
      dpars = dpars, ndraws = ndraws, nobs = nobs,
      data = list(trials = trials)
    ))
    needed <- epred_extra_dpars(family)
    from_dispatch <- compute_family_epred(
      linpred = eta, family = family, trials = trials,
      family_pars = if (length(needed)) dpars[needed] else NULL
    )
    expect_equal(as.numeric(from_dispatch), as.numeric(from_kernel))
    compared <- compared + 1L
  }
  # Guard against the loop silently comparing nothing.
  expect_gt(compared, 25L)
})

test_that("epred_extra_dpars names what a family's mean needs beyond mu", {
  expect_equal(epred_extra_dpars(brms::hurdle_poisson()), "hu")
  expect_equal(epred_extra_dpars(brms::zero_inflated_poisson()), "zi")
  expect_equal(epred_extra_dpars(brms::lognormal()), "sigma")
  expect_setequal(epred_extra_dpars(brms::hurdle_negbinomial()),
                  c("hu", "shape"))
  expect_setequal(epred_extra_dpars(brms::zero_one_inflated_beta()),
                  c("zoi", "coi"))
  # A family whose mean is the inverse link needs nothing extra.
  expect_equal(epred_extra_dpars(poisson()), character(0))
  expect_equal(epred_extra_dpars(NULL), character(0))
})

test_that("a family's mean kernel refuses to run without its parameters", {
  mu <- matrix(2, 3L, 4L)
  expect_error(
    family_mean_from_kernel("hurdle_poisson", mu, family_pars = list()),
    "hu"
  )
  expect_error(
    family_mean_from_kernel("nosuchfamily", mu, family_pars = list()),
    "No mean kernel"
  )
})

# `sample_from_family()` and `compute_family_epred()` are handed the same
# quantity: the inverse link of the linear predictor. Six samplers instead
# treated it as E[Y] and transformed it back, which shifted the draws and,
# for the lognormal families, produced NaN wherever that parameter was not
# positive. Averaging the draws and comparing against the mean ties the two
# layers together, so neither can change its mind about what it was given.
test_that("sampled draws average to the mean the same predictor implies", {
  dpar_value <- function(dpar) {
    switch(dpar,
      sigma = 0.4, shape = 6, phi = 8, nu = 8, hu = 0.3, zi = 0.25,
      zoi = 0.2, coi = 0.6, beta = 1.3, ndt = 0.2, 0.5)
  }
  # `hurdle_negbinomial` is left out deliberately. Its sampler is the
  # one brms uses, which reaches a zero-truncated negative binomial by
  # the tilt that is exact only for the Poisson, and so averages about
  # five percent below the analytic mean. mvgam reproduces brms rather
  # than diverging from it, so the draws are right and the comparison
  # is not one this test can make.
  families <- c(
    "poisson", "negbinomial", "binomial", "bernoulli", "beta",
    "beta_binomial", "lognormal", "hurdle_poisson",
    "hurdle_lognormal", "zero_inflated_poisson",
    "zero_inflated_negbinomial", "zero_inflated_binomial",
    "zero_inflated_beta", "zero_inflated_beta_binomial"
  )
  set.seed(2024)
  ndraws <- 40000L
  trials <- 12
  checked <- 0L

  for (family_name in families) {
    family <- brms::brmsfamily(family_name)
    eta <- matrix(0.2, ndraws, 1L)
    mu <- family$linkinv(eta)
    extra <- setdiff(family$dpars, "mu")
    dpars <- stats::setNames(
      lapply(extra, function(d) matrix(dpar_value(d), ndraws, 1L)), extra
    )
    needed <- epred_extra_dpars(family)
    expected <- compute_family_epred(
      linpred = eta, family = family, trials = trials,
      family_pars = if (length(needed)) dpars[needed] else NULL
    )[1L, 1L]

    args <- dpars[intersect(names(dpars), names(formals(sample_from_family)))]
    drawn <- do.call(sample_from_family, c(
      list(family_name = family_name, ndraws = ndraws, epred = mu,
           trials = trials, lb = NULL, ub = NULL),
      args
    ))
    expect_false(anyNA(drawn))
    # Monte Carlo error over 40k draws. The defects this guards against
    # were off by a factor, not by a few percent.
    expect_equal(mean(drawn), expected, tolerance = 0.05)
    checked <- checked + 1L
  }
  expect_equal(checked, length(families))
})
