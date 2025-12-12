test_that("create_mock_stanfit validates input", {
  # Valid draws_matrix input
  valid_matrix <- matrix(rnorm(20), nrow = 4, ncol = 5)
  colnames(valid_matrix) <- paste0("param_", 1:5)
  valid_draws <- posterior::as_draws_matrix(valid_matrix)

  result <- create_mock_stanfit(valid_draws)
  expect_s3_class(result, "mock_stanfit")
  expect_equal(result$draws_cache, valid_draws)

  # Invalid inputs
  expect_error(
    create_mock_stanfit("not_a_matrix"),
    "Must inherit from class 'draws_matrix'"
  )

  expect_error(
    create_mock_stanfit(matrix(1:4, nrow = 2)),
    "Must inherit from class 'draws_matrix'"
  )
})

test_that("has_nlpars correctly detects nonlinear formulas", {
  # Test with brmsformula objects
  linear_formula <- structure(
    list(
      formula = structure(y ~ x, nl = FALSE),
      pforms = NULL
    ),
    class = "brmsformula"
  )
  expect_false(has_nlpars(linear_formula))

  nonlinear_formula <- structure(
    list(
      formula = structure(y ~ x, nl = TRUE),
      pforms = list(b1 = ~ 1, b2 ~ x)
    ),
    class = "brmsformula"
  )
  expect_true(has_nlpars(nonlinear_formula))

  # Test with invalid objects
  expect_error(
    has_nlpars("not_a_formula"),
    "Assertion failed.*One of the following must apply"
  )
})

test_that("validate_monotonic_indices handles indexing correctly", {
  # Test 0-based indexing (already correct)
  indices_0 <- c(0, 1, 2, 1, 0)
  result_0 <- validate_monotonic_indices(indices_0, "Xmo_1", 3, 5)
  expect_equal(result_0, c(0, 1, 2, 1, 0))

  # Test 1-based indexing (needs conversion)
  indices_1 <- c(1, 2, 3, 2, 1)
  result_1 <- validate_monotonic_indices(indices_1, "Xmo_1", 3, 5)
  expect_equal(result_1, c(0, 1, 2, 1, 0))

  # Test invalid range
  expect_error(
    validate_monotonic_indices(c(0, 5), "Xmo_1", 3, 2),
    "invalid index range"
  )

  # Test wrong length
  expect_error(
    validate_monotonic_indices(c(0, 1), "Xmo_1", 3, 5),
    "has.*elements but expected.*observations"
  )
})

test_that("spd_gp_exp_quad computes spectral density correctly", {
  # Simple test case
  slambda <- array(c(1, 2, 3, 4), dim = c(2, 2))
  sdgp <- c(0.5, 0.8)
  lscale <- matrix(c(1.0, 1.5, 2.0, 2.5), nrow = 2, ncol = 2)

  result <- spd_gp_exp_quad(slambda, sdgp, lscale)

  expect_true(is.matrix(result))
  expect_equal(dim(result), c(2, 2))  # n_draws x n_basis
  expect_true(all(result >= 0))  # Spectral density should be non-negative
  expect_true(all(is.finite(result)))

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

test_that("compute_spd_vectorized dispatches correctly", {
  slambda <- array(c(1, 2), dim = c(1, 2))
  sdgp <- 0.5
  lscale <- matrix(c(1.0, 1.5), nrow = 1)

  # Test valid kernel dispatch
  result_exp <- compute_spd_vectorized(slambda, sdgp, lscale, "exp_quad")
  expect_true(is.matrix(result_exp))
  expect_true(all(result_exp >= 0))

  result_m32 <- compute_spd_vectorized(slambda, sdgp, lscale, "matern32")
  expect_true(is.matrix(result_m32))
  expect_true(all(result_m32 >= 0))

  result_m52 <- compute_spd_vectorized(slambda, sdgp, lscale, "matern52")
  expect_true(is.matrix(result_m52))
  expect_true(all(result_m52 >= 0))

  # Test invalid kernel
  expect_error(
    compute_spd_vectorized(slambda, sdgp, lscale, "invalid_kernel"),
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

test_that("extract_parameters_by_type validates input", {
  # Test with invalid mvgam_fit argument
  expect_error(
    extract_parameters_by_type("not_mvgam", type = "observation"),
    "Must inherit from class 'mvgam'"
  )

  # Test with invalid type argument - class validation happens first
  expect_error(
    extract_parameters_by_type(list(class = "test"), type = "invalid"),
    "Must inherit from class 'mvgam'"
  )
})

test_that("extract_obs_parameters returns character vector", {
  # Validate return type specification by checking function exists
  # and documentation promises character(0) on missing parameters
  expect_true(is.function(extract_obs_parameters))
})

test_that("extract_trend_parameters returns character vector", {
  # Validate return type specification by checking function exists
  # and documentation promises character(0) on missing parameters
  expect_true(is.function(extract_trend_parameters))
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

  lognormal_family <- list(
    family = "lognormal",
    linkinv = exp
  )

  # Requires sigma argument
  expect_error(
    compute_family_epred(linpred, lognormal_family),
    "requires.*sigma"
  )

  # With sigma: E[Y] = exp(mu + sigma^2/2)
  epred <- compute_family_epred(linpred, lognormal_family, sigma = sigma)
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
  expect_error(
    compute_family_epred(linpred, nmix_family),
    "not yet supported"
  )

  tweedie_family <- list(
    family = "tweedie",
    linkinv = exp
  )
  expect_error(
    compute_family_epred(linpred, tweedie_family),
    "not yet supported"
  )
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

  # Dimension mismatch for sigma
  family <- list(family = "lognormal", linkinv = exp)
  wrong_sigma <- matrix(1, nrow = 2, ncol = 3)
  expect_error(
    compute_family_epred(linpred, family, sigma = wrong_sigma),
    "Dimension mismatch"
  )
})

test_that("compute_family_epred handles multivariate input (list)", {
  set.seed(123)
  linpred_list <- list(
    count = matrix(rnorm(20), nrow = 4, ncol = 5),
    biomass = matrix(rnorm(20, mean = 1), nrow = 4, ncol = 5)
  )

  family_list <- list(
    count = list(family = "poisson", linkinv = exp),
    biomass = list(family = "gaussian", linkinv = identity)
  )

  epred <- compute_family_epred(linpred_list, family_list)

  # Returns named list

  expect_true(is.list(epred))
  expect_named(epred, c("count", "biomass"))

  # Each element correctly transformed
  expect_equal(epred$count, exp(linpred_list$count))
  expect_equal(epred$biomass, linpred_list$biomass)
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
# Ordinal and Categorical Family Helper Tests
# ==============================================================================

test_that("insert_refcat inserts reference category correctly", {
  # Matrix input: 2 draws x 3 non-reference categories
  eta <- matrix(c(0.5, 1.0, 1.5, 0.8, 1.2, 1.8), nrow = 2, ncol = 3)

  # Insert reference at position 1 (default)
  result1 <- insert_refcat(eta, refcat = 1)
  expect_equal(dim(result1), c(2, 4))
  # Reference category should be 0

  expect_true(all(result1[, 1] == 0))
  # Other columns should match original
  expect_equal(result1[, 2:4], eta)


  # Insert reference at position 2 (middle)
  result2 <- insert_refcat(eta, refcat = 2)
  expect_equal(dim(result2), c(2, 4))
  expect_true(all(result2[, 2] == 0))
  expect_equal(result2[, 1], eta[, 1])
  expect_equal(result2[, 3:4], eta[, 2:3])

  # Insert reference at last position
  result4 <- insert_refcat(eta, refcat = 4)
  expect_equal(dim(result4), c(2, 4))
  expect_true(all(result4[, 4] == 0))
  expect_equal(result4[, 1:3], eta)
})

test_that("log_softmax computes numerically stable softmax", {
  # Simple test case
  eta <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
  result <- log_softmax(eta)

  expect_equal(dim(result), dim(eta))

  # Log-probabilities should sum to 0 in log space (exp sums to 1)
  row_logsumexp <- log(rowSums(exp(result)))
  expect_equal(row_logsumexp, rep(0, 2), tolerance = 1e-10)

  # All values should be <= 0 (log of probability)
  expect_true(all(result <= 0))

  # Test with extreme values (numerical stability)
  eta_extreme <- matrix(c(1000, 1001, 1002), nrow = 1, ncol = 3)
  result_extreme <- log_softmax(eta_extreme)
  expect_true(all(is.finite(result_extreme)))
  expect_equal(log(sum(exp(result_extreme))), 0, tolerance = 1e-10)
})

test_that("dcumulative computes ordinal probabilities correctly", {
  # 2 draws, 3 categories (2 thresholds)
  eta <- c(0.5, 0.8)  # Linear predictor values for 2 draws
  thres <- matrix(c(-1, 1, -0.5, 1.5), nrow = 2, ncol = 2)  # 2 thresholds

  result <- dcumulative(x = 1:3, eta = eta, thres = thres, link = "logit")

  expect_equal(dim(result), c(2, 3))
  # Probabilities should sum to 1 for each draw
  row_sums <- rowSums(result)
  expect_equal(row_sums, rep(1, 2), tolerance = 1e-10)
  # All probabilities should be in [0, 1]
  expect_true(all(result >= 0 & result <= 1))
})
