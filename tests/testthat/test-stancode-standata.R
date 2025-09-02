# stancode and standata Tests
#
# Comprehensive integrated tests for stancode.mvgam_formula() and
# standata.mvgam_formula() following brms testing patterns.
# Tests cover model specification validation, Stan code generation,
# data preparation, class assignment, and integration with trend systems.

# Test Data Setup ----

#' Create standardized test datasets for stancode/standata function testing
#' @noRd
setup_stan_test_data <- function() {
  set.seed(42)  # Reproducible tests
  n_time <- 24
  n_series <- 3

  # Simple univariate dataset
  univariate <- data.frame(
    time = 1:n_time,
    series = factor(rep("series1", n_time)),
    y = rpois(n_time, lambda = 5),
    x = rnorm(n_time),
    temperature = rnorm(n_time, mean = 15, sd = 3)
  )

  # Multivariate dataset with balanced design
  multivariate <- data.frame(
    time = rep(1:n_time, n_series),
    series = factor(rep(paste0("series", 1:n_series), each = n_time)),
    count = rpois(n_time * n_series, lambda = 4),
    biomass = rlnorm(n_time * n_series, meanlog = 1, sdlog = 0.5),
    presence = rbinom(n_time * n_series, size = 1, prob = 0.7),
    x = rnorm(n_time * n_series),
    habitat = factor(sample(c("forest", "grassland"), n_time * n_series, replace = TRUE))
  )

  # Dataset with missing values
  with_missings <- univariate
  with_missings$y[c(3, 7, 15)] <- NA

  list(
    univariate = univariate,
    multivariate = multivariate,
    with_missings = with_missings
  )
}

# Helper function for string pattern matching
expect_match2 <- function(object, regexp, ...) {
  expect_true(grepl(regexp, object, ...))
}

# stancode Tests ----

test_that("stancode.mvgam_formula returns correct class structure", {
  data <- setup_stan_test_data()$univariate

  # Simple observation-only model
  mf_obs_only <- mvgam_formula(y ~ x)
  code_obs_only <- stancode(mf_obs_only, data = data, family = poisson(), validate = FALSE)

  # Check class structure follows mvgam convention with brms compatibility
  expect_s3_class(code_obs_only, "mvgamstancode")
  expect_s3_class(code_obs_only, "stancode")
  expect_s3_class(code_obs_only, "character")
  expect_equal(class(code_obs_only), c("mvgamstancode", "stancode", "character"))

  # Model with trends - generate without validation first
  mf_with_trend <- mvgam_formula(y ~ x, trend_formula = ~ RW())
  code_with_trend <- stancode(mf_with_trend, data = data, family = poisson(), validate = FALSE)

  # Should have same class structure
  expect_s3_class(code_with_trend, "mvgamstancode")
  expect_s3_class(code_with_trend, "stancode")
  expect_equal(class(code_with_trend), c("mvgamstancode", "stancode", "character"))

  # Should be longer than observation-only model
  expect_gt(nchar(code_with_trend), nchar(code_obs_only))

  # Trend formula ~ RW() has no predictors, so should NOT contain design matrix variables
  expect_false(grepl("int K_trend;", code_with_trend, fixed = TRUE))
  expect_false(grepl("real Kc_trend;", code_with_trend, fixed = TRUE))
  expect_false(grepl("X_trend", code_with_trend, fixed = TRUE))
  expect_false(grepl("Xc_trend", code_with_trend, fixed = TRUE))
  expect_false(grepl("matrix.*K.*X_trend", code_with_trend))

  # mu needs to be created explicitly because of glm observation likelihood
  expect_true(grepl("vector[N] mu = Xc * b;", code_with_trend, fixed = TRUE))

  # brms-generated dispersion parameter should be excluded
  expect_false(grepl("real<lower=0> sigma;", code_with_trend, fixed = TRUE))

  # GLM function usage for Poisson family
  expect_true(grepl("poisson_log_glm_lpmf", code_with_trend, fixed = TRUE))
  expect_true(grepl("vector[1] mu_ones;", code_with_trend, fixed = TRUE))

  # Essential trend dimensions in data block
  expect_true(grepl("int<lower=1> n_trend;", code_with_trend, fixed = TRUE))
  expect_true(grepl("int<lower=1> n_series_trend;", code_with_trend, fixed = TRUE))
  expect_true(grepl("int<lower=1> n_lv_trend;", code_with_trend, fixed = TRUE))

  # Critical times_trend matrix structure
  expect_true(grepl("matrix times_trend\\[n_trend, n_series_trend\\];", code_with_trend, fixed = TRUE))

  # Factor loading matrix for non-factor models
  expect_true(grepl("matrix\\[n_series_trend, n_lv_trend\\] Z = diag_matrix", code_with_trend))

  # RW-specific innovation structure
  expect_true(grepl("matrix\\[n_trend, n_lv_trend\\] innovations_trend;", code_with_trend))
  expect_true(grepl("vector<lower=0>\\[1\\] sigma_trend;", code_with_trend))
  expect_true(grepl("scaled_innovations_trend = innovations_trend \\* diag_matrix\\(sigma_trend\\)", code_with_trend))

  # 6. RW Dynamics Implementation
  # Random walk state evolution
  expect_true(grepl("lv_trend\\[1, :\\] = scaled_innovations_trend\\[1, :\\]", code_with_trend))
  expect_true(grepl("lv_trend\\[i, :\\] = lv_trend\\[i-1, :\\] \\+ scaled_innovations_trend\\[i, :\\]", code_with_trend))

  # Critical universal pattern with dot_product
  expect_true(grepl("trend\\[i, s\\] = dot_product\\(Z\\[s, :\\], lv_trend\\[i, :\\]\\) \\+ mu_trend\\[times_trend\\[i, s\\]\\]", code_with_trend))

  # mu construction and trend addition in transformed parameters
  expect_true(grepl("mu\\[n\\] \\+= Intercept \\+ trend\\[obs_trend_time\\[n\\], obs_trend_series\\[n\\]\\];", code_with_trend))

  # Check for no duplicated Stan blocks
  expect_equal(length(gregexpr("^\\s*data\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*transformed data\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*parameters\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*transformed parameters\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*model\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*generated quantities\\s*\\{", code_with_trend)[[1]]), 1)

  # Final validation: ensure both models compile correctly
  expect_no_error(stancode(mf_obs_only, data = data, family = poisson(), validate = TRUE))
  expect_no_error(stancode(mf_with_trend, data = data, family = poisson(), validate = TRUE))
})

test_that("stancode generates correct AR(p = c(1, 12)) seasonal model structure", {
  data <- setup_stan_test_data()$univariate
  mf_with_trend <- mvgam_formula(
    y ~ x,
    trend_formula = ~ AR(p = c(1, 12))
  )
  code_with_trend <- stancode(
    mf_with_trend, data = data,
    family = poisson(),
    validate = FALSE
  )

  # Basic structure checks
  expect_s3_class(code_with_trend, "mvgamstancode")
  expect_s3_class(code_with_trend, "stancode")

  # AR-specific parameter declarations
  # Should have ar1_trend and ar12_trend, NOT ar2_trend through ar11_trend
  expect_true(grepl("vector<lower=-1,upper=1>\\[n_lv_trend\\] ar1_trend;", code_with_trend))
  expect_true(grepl("vector<lower=-1,upper=1>\\[n_lv_trend\\] ar12_trend;", code_with_trend))

  # Should NOT have intermediate lags
  expect_false(grepl("ar2_trend", code_with_trend, fixed = TRUE))
  expect_false(grepl("ar3_trend", code_with_trend, fixed = TRUE))
  expect_false(grepl("ar11_trend", code_with_trend, fixed = TRUE))

  # Initialization: First 12 time points should be pure innovations
  expect_true(grepl("// Initialize first 12 time points", code_with_trend, fixed = TRUE))
  expect_true(grepl("for \\(i in 1:12\\)", code_with_trend))
  expect_true(grepl("lv_trend\\[i, :\\] = scaled_innovations_trend\\[i, :\\];", code_with_trend))

  # AR dynamics: Should start from time point 13
  expect_true(grepl("// Apply AR dynamics", code_with_trend, fixed = TRUE))
  expect_true(grepl("for \\(i in 13:n_trend\\)", code_with_trend))

  # AR dynamics equation: Should use both ar1_trend and ar12_trend
  expect_true(grepl("ar1_trend\\[j\\] \\* lv_trend\\[i-1, j\\]", code_with_trend))
  expect_true(grepl("ar12_trend\\[j\\] \\* lv_trend\\[i-12, j\\]", code_with_trend))

  # Combined AR equation pattern (looking for the sum of AR terms)
  expect_true(grepl("lv_trend\\[i, j\\] = ar1_trend\\[j\\] \\* lv_trend\\[i-1, j\\] \\+ ar12_trend\\[j\\] \\* lv_trend\\[i-12, j\\] \\+ scaled_innovations_trend\\[i, j\\]", code_with_trend))

  # Priors for AR coefficients in model block (just check they exist, not specific values)
  expect_true(grepl("ar1_trend ~ normal", code_with_trend))
  expect_true(grepl("ar12_trend ~ normal", code_with_trend))

  # Should still have standard trend components
  expect_true(grepl("matrix\\[n_trend, n_lv_trend\\] innovations_trend;", code_with_trend))
  expect_true(grepl("vector<lower=0>\\[1\\] sigma_trend;", code_with_trend))
  expect_true(grepl("matrix\\[n_trend, n_lv_trend\\] lv_trend;", code_with_trend))

  # mu construction and trend addition in transformed parameters
  expect_true(grepl("mu\\[n\\] \\+= Intercept \\+ trend\\[obs_trend_time\\[n\\], obs_trend_series\\[n\\]\\];", code_with_trend))

  # Universal trend computation pattern should still be present
  expect_true(grepl("trend\\[i, s\\] = dot_product\\(Z\\[s, :\\], lv_trend\\[i, :\\]\\) \\+ mu_trend\\[times_trend\\[i, s\\]\\]", code_with_trend))

  # GLM optimization should still be present
  expect_true(grepl("poisson_log_glm_lpmf", code_with_trend, fixed = TRUE))
  expect_true(grepl("vector\\[1\\] mu_ones", code_with_trend))

  # Mapping arrays should still be present
  expect_true(grepl("array\\[N\\] int obs_trend_time", code_with_trend))
  expect_true(grepl("array\\[N\\] int obs_trend_series", code_with_trend))

  # Check for no duplicated Stan blocks
  expect_equal(length(gregexpr("^\\s*data\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*transformed data\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*parameters\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*transformed parameters\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*model\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*generated quantities\\s*\\{", code_with_trend)[[1]]), 1)

  # Final validation: ensure model compiles correctly
  expect_no_error(stancode(mf_with_trend, data = data, family = poisson(), validate = TRUE))
})

test_that("stancode generates correct AR(p = c(2, 4), ma = TRUE) ARMA model structure", {
  data <- setup_stan_test_data()$univariate
  mf_with_trend <- mvgam_formula(
    y ~ x,
    trend_formula = ~ AR(p = c(2, 4), ma = TRUE)
  )
  code_with_trend <- stancode(
    mf_with_trend, data = data,
    family = poisson(),
    validate = FALSE
  )

  # Basic structure checks
  expect_s3_class(code_with_trend, "mvgamstancode")
  expect_s3_class(code_with_trend, "stancode")

  # AR-specific parameter declarations
  # Should have ar2_trend and ar4_trend, NOT ar1_trend or ar3_trend
  expect_true(grepl("vector<lower=-1,upper=1>\\[n_lv_trend\\] ar2_trend;", code_with_trend))
  expect_true(grepl("vector<lower=-1,upper=1>\\[n_lv_trend\\] ar4_trend;", code_with_trend))

  # Should NOT have other AR lags
  expect_false(grepl("ar1_trend", code_with_trend, fixed = TRUE))
  expect_false(grepl("ar3_trend", code_with_trend, fixed = TRUE))
  expect_false(grepl("ar5_trend", code_with_trend, fixed = TRUE))

  # MA parameter declaration
  expect_true(grepl("vector<lower=-1,upper=1>\\[n_lv_trend\\] theta1_trend;", code_with_trend))

  # MA innovations matrix should be created from scaled innovations
  expect_true(grepl("matrix\\[n_trend, n_lv_trend\\] ma_innovations_trend = scaled_innovations_trend;", code_with_trend))

  # MA transformation should be applied to the entire matrix first
  expect_true(grepl("// Apply MA\\(1\\) transformation", code_with_trend))
  expect_true(grepl("for \\(i in 2:n_trend\\)", code_with_trend))
  expect_true(grepl("ma_innovations_trend\\[i, j\\] \\+= theta1_trend\\[j\\] \\* ma_innovations_trend\\[i-1, j\\];", code_with_trend))

  # Initialization: First 4 time points should use MA innovations
  expect_true(grepl("// Initialize first 4 time points", code_with_trend, fixed = TRUE))
  expect_true(grepl("for \\(i in 1:4\\)", code_with_trend))
  expect_true(grepl("lv_trend\\[i, :\\] = ma_innovations_trend\\[i, :\\];", code_with_trend))

  # AR dynamics: Should start from time point 5 and use ma_innovations_trend
  expect_true(grepl("// Apply AR dynamics", code_with_trend, fixed = TRUE))
  expect_true(grepl("for \\(i in 5:n_trend\\)", code_with_trend))

  # AR dynamics equation: Should use both ar2_trend and ar4_trend with MA innovations
  expect_true(grepl("ar2_trend\\[j\\] \\* lv_trend\\[i-2, j\\]", code_with_trend))
  expect_true(grepl("ar4_trend\\[j\\] \\* lv_trend\\[i-4, j\\]", code_with_trend))
  expect_true(grepl("ma_innovations_trend\\[i, j\\]", code_with_trend))

  # Combined ARMA equation pattern - should be addition of AR lags plus MA innovation
  expect_true(grepl("lv_trend\\[i, j\\] = ar2_trend\\[j\\] \\* lv_trend\\[i-2, j\\] \\+ ar4_trend\\[j\\] \\* lv_trend\\[i-4, j\\] \\+ ma_innovations_trend\\[i, j\\]", code_with_trend))

  # Priors for AR and MA coefficients in model block
  expect_true(grepl("ar2_trend ~ normal", code_with_trend))
  expect_true(grepl("ar4_trend ~ normal", code_with_trend))
  expect_true(grepl("theta1_trend ~ normal", code_with_trend))

  # Should still have standard trend components
  expect_true(grepl("matrix\\[n_trend, n_lv_trend\\] innovations_trend;", code_with_trend))
  expect_true(grepl("vector<lower=0>\\[1\\] sigma_trend;", code_with_trend))
  expect_true(grepl("matrix\\[n_trend, n_lv_trend\\] lv_trend;", code_with_trend))

  # Universal trend computation pattern should still be present
  expect_true(grepl("trend\\[i, s\\] = dot_product\\(Z\\[s, :\\], lv_trend\\[i, :\\]\\) \\+ mu_trend\\[times_trend\\[i, s\\]\\]", code_with_trend))

  # GLM optimization should still be present
  expect_true(grepl("poisson_log_glm_lpmf", code_with_trend, fixed = TRUE))
  expect_true(grepl("vector\\[1\\] mu_ones", code_with_trend))

  # Mapping arrays should still be present
  expect_true(grepl("array\\[N\\] int obs_trend_time", code_with_trend))
  expect_true(grepl("array\\[N\\] int obs_trend_series", code_with_trend))

  # Check for no duplicated Stan blocks
  expect_equal(length(gregexpr("^\\s*data\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*transformed data\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*parameters\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*transformed parameters\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*model\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*generated quantities\\s*\\{", code_with_trend)[[1]]), 1)

  # Final validation: ensure model compiles correctly
  expect_no_error(stancode(mf_with_trend, data = data, family = poisson(), validate = TRUE))
})

test_that("stancode generates correct ZMVN(n_lv = 2) factor model with trend covariate", {
  data <- setup_stan_test_data()$multivariate
  mf_with_trend <- mvgam_formula(
    biomass ~ 1,
    trend_formula = ~ x + ZMVN(n_lv = 2)
  )
  code_with_trend <- stancode(
    mf_with_trend,
    data = data,
    family = lognormal(),
    validate = FALSE
  )

  # Basic structure checks
  expect_s3_class(code_with_trend, "mvgamstancode")
  expect_s3_class(code_with_trend, "stancode")

  # Response variable should be continuous (vector, not array)
  expect_true(grepl("vector\\[N\\] Y;", code_with_trend))
  expect_false(grepl("array\\[N\\] int Y;", code_with_trend))

  # Observation model: Only intercept (no covariates)
  expect_false(grepl("int.*K;", code_with_trend))  # No K for observation model
  expect_false(grepl("matrix\\[N,.*X;", code_with_trend))  # No X design matrix for obs
  expect_false(grepl("vector\\[.*\\] b;", code_with_trend))  # No b coefficients for obs

  # Observation model dispersion parameter
  expect_true(grepl("real<lower=0> sigma;", code_with_trend))

  # Trend design matrix and coefficients
  expect_true(grepl("int<lower=1> K_trend;", code_with_trend))
  expect_true(grepl("int<lower=1> Kc_trend;", code_with_trend))
  expect_true(grepl("matrix\\[n_trend, K_trend\\] X_trend;", code_with_trend))
  expect_true(grepl("matrix\\[n_trend, Kc_trend\\] Xc_trend;", code_with_trend))
  expect_true(grepl("vector\\[Kc_trend\\] b_trend;", code_with_trend))
  expect_true(grepl("vector\\[Kc_trend\\] means_X_trend;", code_with_trend))

  # Factor model parameters
  expect_true(grepl("vector<lower=0>\\[n_lv_trend\\] sigma_trend;", code_with_trend))
  expect_true(grepl("vector\\[n_series_trend \\* n_lv_trend\\] Z_raw;", code_with_trend))

  # Factor loading constraints in transformed parameters
  expect_true(grepl("matrix\\[n_series_trend, n_lv_trend\\] Z = rep_matrix\\(0, n_series_trend, n_lv_trend\\);", code_with_trend))
  expect_true(grepl("// constraints allow identifiability of loadings", code_with_trend))
  expect_true(grepl("for \\(j in 1 : n_lv_trend\\)", code_with_trend))
  expect_true(grepl("for \\(i in j : n_series_trend\\)", code_with_trend))
  expect_true(grepl("Z\\[i, j\\] = Z_raw\\[index\\];", code_with_trend))

  # ZMVN dynamics (just scaled innovations, no complex dynamics)
  expect_true(grepl("lv_trend = scaled_innovations_trend;", code_with_trend))

  # Trend mean with covariate effects
  expect_true(grepl("vector\\[n_trend\\] mu_trend = Xc_trend \\* b_trend \\+ Intercept_trend;", code_with_trend))

  # Universal trend computation pattern should still be present
  expect_true(grepl("trend\\[i, s\\] = dot_product\\(Z\\[s, :\\], lv_trend\\[i, :\\]\\) \\+ mu_trend\\[times_trend\\[i, s\\]\\]", code_with_trend))

  # Observation model priors (brms pattern)
  expect_true(grepl("lprior \\+= student_t_lpdf\\(Intercept \\| 3, 0\\.9, 2\\.5\\);", code_with_trend))
  expect_true(grepl("lprior \\+= student_t_lpdf\\(sigma \\| 3, 0, 2\\.5\\)", code_with_trend))
  expect_true(grepl("- 1 \\* student_t_lccdf\\(0 \\| 3, 0, 2\\.5\\);", code_with_trend))

  # Trend parameter priors
  expect_true(grepl("sigma_trend ~ exponential\\(2\\);", code_with_trend))
  expect_true(grepl("Z_raw ~ student_t\\(3, 0, 1\\);", code_with_trend))

  # No prior for b_trend (brms default flat prior)
  expect_false(grepl("b_trend ~", code_with_trend))

  # Lognormal likelihood (not GLM optimized)
  expect_true(grepl("target \\+= lognormal_lpdf\\(Y \\| mu, sigma\\);", code_with_trend))
  expect_false(grepl("lognormal.*glm", code_with_trend))
  expect_false(grepl("mu_ones", code_with_trend))

  # Observation model likelihood structure (brms pattern)
  expect_true(grepl("vector\\[N\\] mu = rep_vector\\(0\\.0, N\\);", code_with_trend))
  expect_true(grepl("mu \\+= Intercept;", code_with_trend))
  expect_true(grepl("mu\\[n\\] \\+= trend\\[obs_trend_time\\[n\\], obs_trend_series\\[n\\]\\];", code_with_trend))

  # Generated quantities
  expect_true(grepl("real b_Intercept = Intercept;", code_with_trend))
  expect_true(grepl("real b_Intercept_trend = Intercept_trend - dot_product\\(means_X_trend, b_trend\\);", code_with_trend))

  # Mapping arrays should still be present
  expect_true(grepl("array\\[N\\] int obs_trend_time", code_with_trend))
  expect_true(grepl("array\\[N\\] int obs_trend_series", code_with_trend))

  # Check for no duplicated Stan blocks
  expect_equal(length(gregexpr("^\\s*data\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*transformed data\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*parameters\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*transformed parameters\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*model\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*generated quantities\\s*\\{", code_with_trend)[[1]]), 1)

  # Final validation: ensure model compiles correctly
  expect_no_error(stancode(mf_with_trend, data = data, family = lognormal(), validate = TRUE))
})

test_that("stancode generates correct hierarchical ZMVN(gr = habitat) model with custom prior", {
  data <- setup_stan_test_data()$multivariate
  mf_with_trend <- mvgam_formula(
    biomass ~ 1,
    trend_formula = ~ x + ZMVN(gr = habitat)
  )

  # Custom prior for hierarchical mixing parameter
  custom_prior <- brms::prior("beta(5, 5)", class = "alpha_cor_trend")

  code_with_trend <- stancode(
    mf_with_trend,
    data = data,
    family = lognormal(),
    prior = custom_prior,
    validate = FALSE
  )

  # Basic structure checks
  expect_s3_class(code_with_trend, "mvgamstancode")
  expect_s3_class(code_with_trend, "stancode")

  # Response variable should be continuous (vector, not array)
  expect_true(grepl("vector\\[N\\] Y;", code_with_trend))
  expect_false(grepl("array\\[N\\] int Y;", code_with_trend))

  # Observation model: Only intercept (no covariates)
  expect_false(grepl("int.*K;", code_with_trend))  # No K for observation model
  expect_false(grepl("matrix\\[N,.*X;", code_with_trend))  # No X design matrix for obs
  expect_false(grepl("vector\\[.*\\] b;", code_with_trend))  # No b coefficients for obs

  # Observation model dispersion parameter
  expect_true(grepl("real<lower=0> sigma;", code_with_trend))

  # Trend design matrix and coefficients
  expect_true(grepl("int<lower=1> K_trend;", code_with_trend))
  expect_true(grepl("int<lower=1> Kc_trend;", code_with_trend))
  expect_true(grepl("matrix\\[n_trend, K_trend\\] X_trend;", code_with_trend))
  expect_true(grepl("matrix\\[n_trend, Kc_trend\\] Xc_trend;", code_with_trend))
  expect_true(grepl("vector\\[Kc_trend\\] b_trend;", code_with_trend))
  expect_true(grepl("vector\\[Kc_trend\\] means_X_trend;", code_with_trend))

  # Hierarchical grouping data structures
  expect_true(grepl("int<lower=1> n_groups_trend;", code_with_trend))
  expect_true(grepl("array\\[n_series_trend\\] int.*group_inds_trend;", code_with_trend))

  # Hierarchical correlation functions
  expect_true(grepl("matrix combine_cholesky\\(", code_with_trend))
  expect_true(grepl("real alpha", code_with_trend))

  # Hierarchical correlation parameters with _trend suffix
  expect_true(grepl("cholesky_factor_corr\\[n_lv_trend\\] L_Omega_global_trend;", code_with_trend))
  expect_true(grepl("array\\[n_groups_trend\\] cholesky_factor_corr\\[n_lv_trend\\] L_deviation_group_trend;", code_with_trend))
  expect_true(grepl("real<lower=0, upper=1> alpha_cor_trend;", code_with_trend))

  # Group-specific correlation matrices computation
  expect_true(grepl("array\\[n_groups_trend\\] cov_matrix\\[n_lv_trend\\] Sigma_group_trend;", code_with_trend))
  expect_true(grepl("L_Omega_group_trend = combine_cholesky\\(L_Omega_global_trend, L_deviation_group_trend\\[g\\], alpha_cor_trend\\)", code_with_trend))

  # Group-specific innovation scaling
  expect_true(grepl("int group_idx = group_inds_trend\\[s\\];", code_with_trend))
  expect_true(grepl("L_group_trend = cholesky_decompose\\(Sigma_group_trend\\[group_idx\\]\\)", code_with_trend))

  # ZMVN dynamics (just scaled innovations)
  expect_true(grepl("lv_trend = scaled_innovations_trend;", code_with_trend))

  # Factor loading matrix (diagonal for non-factor ZMVN)
  expect_true(grepl("matrix\\[n_series_trend, n_lv_trend\\] Z = diag_matrix\\(rep_vector\\(1\\.0, n_lv_trend\\)\\);", code_with_trend))

  # Trend mean with covariate effects
  expect_true(grepl("vector\\[n_trend\\] mu_trend = Xc_trend \\* b_trend \\+ Intercept_trend;", code_with_trend))

  # Universal trend computation pattern should still be present
  expect_true(grepl("trend\\[i, s\\] = dot_product\\(Z\\[s, :\\], lv_trend\\[i, :\\]\\) \\+ mu_trend\\[times_trend\\[i, s\\]\\]", code_with_trend))

  # Hierarchical correlation priors with _trend suffix
  expect_true(grepl("L_Omega_global_trend ~ lkj_corr_cholesky\\(1\\);", code_with_trend))
  expect_true(grepl("L_deviation_group_trend\\[g\\] ~ lkj_corr_cholesky\\(6\\);", code_with_trend))

  # Custom alpha_cor_trend prior should be applied
  expect_true(grepl("alpha_cor_trend ~ beta\\(5, 5\\);", code_with_trend))

  # Standard trend parameter priors
  expect_true(grepl("sigma_trend ~ exponential\\(2\\);", code_with_trend))
  expect_true(grepl("to_vector\\(innovations_trend\\) ~ std_normal\\(\\);", code_with_trend))

  # No prior for b_trend (brms default flat prior)
  expect_false(grepl("b_trend ~", code_with_trend))

  # Observation model priors (brms pattern)
  expect_true(grepl("lprior \\+= student_t_lpdf\\(Intercept \\| 3, 0\\.9, 2\\.5\\);", code_with_trend))
  expect_true(grepl("lprior \\+= student_t_lpdf\\(sigma \\| 3, 0, 2\\.5\\)", code_with_trend))
  expect_true(grepl("- 1 \\* student_t_lccdf\\(0 \\| 3, 0, 2\\.5\\);", code_with_trend))

  # Lognormal likelihood (not GLM optimized)
  expect_true(grepl("target \\+= lognormal_lpdf\\(Y \\| mu, sigma\\);", code_with_trend))
  expect_false(grepl("lognormal.*glm", code_with_trend))
  expect_false(grepl("mu_ones", code_with_trend))

  # Observation model likelihood structure (brms pattern)
  expect_true(grepl("vector\\[N\\] mu = rep_vector\\(0\\.0, N\\);", code_with_trend))
  expect_true(grepl("mu \\+= Intercept;", code_with_trend))
  expect_true(grepl("mu\\[n\\] \\+= trend\\[obs_trend_time\\[n\\], obs_trend_series\\[n\\]\\];", code_with_trend))

  # Generated quantities
  expect_true(grepl("real b_Intercept = Intercept;", code_with_trend))
  expect_true(grepl("real b_Intercept_trend = Intercept_trend - dot_product\\(means_X_trend, b_trend\\);", code_with_trend))

  # Mapping arrays should still be present
  expect_true(grepl("array\\[N\\] int obs_trend_time", code_with_trend))
  expect_true(grepl("array\\[N\\] int obs_trend_series", code_with_trend))

  # Check for no duplicated Stan blocks
  expect_equal(length(gregexpr("^\\s*data\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*transformed data\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*parameters\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*transformed parameters\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*model\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*generated quantities\\s*\\{", code_with_trend)[[1]]), 1)

  # Final validation: ensure model compiles correctly
  expect_no_error(stancode(mf_with_trend, data = data, family = lognormal(), prior = custom_prior, validate = TRUE))
})

test_that("stancode handles different observation families", {
  data <- setup_stan_test_data()$univariate
  mf <- mvgam_formula(y ~ x, trend_formula = ~ RW())

  # Test major families
  families_to_test <- list(
    poisson = poisson(),
    gaussian = gaussian(),
    bernoulli = bernoulli()
  )

  for (fam_name in names(families_to_test)) {
    family <- families_to_test[[fam_name]]

    # Adjust data for family
    test_data <- data
    if (fam_name == "bernoulli") {
      test_data$y <- rbinom(nrow(data), size = 1, prob = 0.3)
    } else if (fam_name == "gaussian") {
      test_data$y <- rnorm(nrow(data))
    }

    code <- stancode(mf, data = test_data, family = family)

    # Basic validation
    expect_s3_class(code, "mvgamstancode")
    expect_s3_class(code, "stancode")
    expect_gt(nchar(code), 200)

    # Should contain family-specific elements
    if (fam_name == "poisson") {
      expect_match2(code, "poisson")
    } else if (fam_name == "gaussian") {
      expect_match2(code, "normal")
    } else if (fam_name == "bernoulli") {
      expect_match2(code, "bernoulli")
    }
  }
})

test_that("stancode generates correct Stan blocks", {
  data <- setup_stan_test_data()$univariate
  mf <- mvgam_formula(y ~ s(x), trend_formula = ~ AR(p = 1))

  # Generate Stan code without validation for structure inspection
  code <- stancode(mf, data = data, family = poisson(), validate = FALSE)

  # Each Stan block should appear exactly once
  expect_equal(length(gregexpr("^\\s*data\\s*\\{", code)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*parameters\\s*\\{", code)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*transformed parameters\\s*\\{", code)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*model\\s*\\{", code)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*generated quantities\\s*\\{", code)[[1]]), 1)

  # Stan blocks should be in correct order
  data_pos <- regexpr("data\\s*\\{", code)
  param_pos <- regexpr("parameters\\s*\\{", code)
  tp_pos <- regexpr("transformed parameters\\s*\\{", code)
  model_pos <- regexpr("model\\s*\\{", code)
  gq_pos <- regexpr("generated quantities\\s*\\{", code)

  expect_true(data_pos < param_pos)
  expect_true(param_pos < tp_pos)
  expect_true(tp_pos < model_pos)
  expect_true(model_pos < gq_pos)

  # Should have exactly one lprior declaration (not duplicated)
  lprior_decls <- gregexpr("real\\s+lprior\\s*=\\s*0;", code)[[1]]
  expect_equal(length(lprior_decls), 1)

  # Required variable declarations should be present
  expect_match2(code, "vector\\[N\\]\\s+mu")
  expect_match2(code, "vector\\[.*\\]\\s+mu_trend")
  expect_match2(code, "matrix\\[.*\\]\\s+trend;")

  # Trend parameters should be declared in parameters block
  expect_match2(code, "real.*ar1_trend")
  expect_match2(code, "vector<lower=0>\\[.*\\]\\s+sigma_trend")

  # Data block should contain mapping arrays
  expect_match2(code, "array\\[N\\]\\s+int\\s+obs_trend_time")
  expect_match2(code, "array\\[N\\]\\s+int\\s+obs_trend_series")

  # Trend injection should use correct pattern
  expect_match2(code, "mu\\[n\\]\\s*\\+=\\s*trend\\[obs_trend_time\\[n\\],\\s*obs_trend_series\\[n\\]\\]")
  expect_false(grepl("obs_ind", code))

  # Universal trend computation pattern should be present
  expect_match2(code, "for\\s*\\(\\s*i\\s+in\\s+1:n_trend\\s*\\)")
  expect_match2(code, "trend\\[i,\\s*s\\]\\s*=.*dot_product")

  # Should contain sigma_trend prior but not duplicate sigma prior
  expect_match2(code, "sigma_trend\\s*~")

  # All braces should be properly matched
  open_braces <- length(gregexpr("\\{", code)[[1]])
  close_braces <- length(gregexpr("\\}", code)[[1]])
  expect_equal(open_braces, close_braces)

  # Split code into lines to check positioning
  code_lines <- strsplit(code, "\n", fixed = TRUE)[[1]]

  # Find model block boundaries
  model_start <- which(grepl("^\\s*model\\s*\\{", code_lines))[1]
  expect_false(is.na(model_start))  # Model block should be found

  # Find model block end by counting braces
  brace_count <- 0
  model_end <- model_start
  for (i in model_start:length(code_lines)) {
    line <- code_lines[i]
    open_braces <- lengths(regmatches(line, gregexpr("\\{", line, perl = TRUE)))
    close_braces <- lengths(regmatches(line, gregexpr("\\}", line, perl = TRUE)))
    brace_count <- brace_count + open_braces - close_braces
    if (i > model_start && brace_count == 0) {
      model_end <- i
      break
    }
  }

  # Find positions within model block
  model_lines <- code_lines[model_start:model_end]
  mu_plus_lines <- which(grepl("\\s*mu\\s*\\+=", model_lines))
  trend_injection_lines <- which(grepl("Add trend effects using mapping arrays|mu\\[n\\]\\s*\\+=\\s*trend\\[", model_lines))
  likelihood_lines <- which(grepl("target\\s*\\+=.*lpmf\\s*\\(|target\\s*\\+=.*lpdf\\s*\\(", model_lines))

  # Trend injection should be in model block, not transformed parameters
  tp_start <- which(grepl("^\\s*transformed parameters\\s*\\{", code_lines))[1]
  if (!is.na(tp_start)) {
    tp_end <- tp_start
    brace_count <- 0
    for (i in tp_start:length(code_lines)) {
      line <- code_lines[i]
      open_braces <- lengths(regmatches(line, gregexpr("\\{", line, perl = TRUE)))
      close_braces <- lengths(regmatches(line, gregexpr("\\}", line, perl = TRUE)))
      brace_count <- brace_count + open_braces - close_braces
      if (i > tp_start && brace_count == 0) {
        tp_end <- i
        break
      }
    }
    tp_lines <- code_lines[tp_start:tp_end]

    # Trend injection should NOT be in transformed parameters block
    expect_false(any(grepl("Add trend effects using mapping arrays", tp_lines)))
    expect_false(any(grepl("mu\\[n\\]\\s*\\+=\\s*trend\\[", tp_lines)))
  }

  # Trend injection should be AFTER last mu += line
  if (length(mu_plus_lines) > 0 && length(trend_injection_lines) > 0) {
    last_mu_plus <- max(mu_plus_lines)
    first_trend_injection <- min(trend_injection_lines)
    expect_true(first_trend_injection > last_mu_plus)
  }

  # Trend injection should be BEFORE likelihood statement
  if (length(trend_injection_lines) > 0 && length(likelihood_lines) > 0) {
    last_trend_injection <- max(trend_injection_lines)
    first_likelihood <- min(likelihood_lines)
    expect_true(last_trend_injection < first_likelihood)
  }

  # Verify trend injection pattern is in model block
  expect_true(any(grepl("Add trend effects using mapping arrays", model_lines)))
  expect_true(any(grepl("mu\\[n\\]\\s*\\+=\\s*trend\\[obs_trend_time\\[n\\],\\s*obs_trend_series\\[n\\]\\]", model_lines)))

  # Generated Stan code should compile without errors
  expect_no_error(stancode(mf, data = data, family = poisson(), validate = TRUE))
})

test_that("stancode handles multivariate specifications with shared RW trend", {
  data <- setup_stan_test_data()$multivariate

  # Multivariate with shared trend (explicitly set rescor = FALSE
  # to avoid brms deprecation warnings)
  mf_shared <- mvgam_formula(
    bf(mvbind(count, biomass) ~ x) + set_rescor(FALSE),
    trend_formula = ~ RW(cor = TRUE)
  )

  # Generate without validation first for structure inspection
  code_shared <- stancode(mf_shared, data = data, validate = FALSE)

  expect_s3_class(code_shared, "stancode")
  expect_gt(nchar(code_shared), 500)

  # ========== BLOCK STRUCTURE TESTS ==========
  # Should have exactly one of each Stan block (no duplicates)
  expect_equal(length(gregexpr("^\\s*data\\s*\\{", code_shared)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*parameters\\s*\\{", code_shared)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*transformed parameters\\s*\\{", code_shared)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*model\\s*\\{", code_shared)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*generated quantities\\s*\\{", code_shared)[[1]]), 1)

  # Check proper block ordering
  data_pos <- regexpr("data\\s*\\{", code_shared)
  tdata_pos <- regexpr("transformed data\\s*\\{", code_shared)
  params_pos <- regexpr("parameters\\s*\\{", code_shared)
  tp_pos <- regexpr("transformed parameters\\s*\\{", code_shared)
  model_pos <- regexpr("model\\s*\\{", code_shared)
  gq_pos <- regexpr("generated quantities\\s*\\{", code_shared)
  
  # Data should come before transformed data
  expect_true(data_pos < tdata_pos)
  # Transformed data should come before parameters
  expect_true(tdata_pos < params_pos)
  # Parameters should come before transformed parameters
  expect_true(params_pos < tp_pos)
  # Transformed parameters should come before model
  expect_true(tp_pos < model_pos)
  # Model should come before generated quantities
  expect_true(model_pos < gq_pos)

  # ========== DATA BLOCK TESTS ==========
  # brms observation data declarations
  # Should declare N_count with comment
  expect_match2(code_shared, "int<lower=1> N_count;\\s*//\\s*number of observations")
  # Should declare Y_count as vector
  expect_match2(code_shared, "vector\\[N_count\\] Y_count;\\s*//\\s*response variable")
  # Should declare X_count design matrix
  expect_match2(code_shared, "matrix\\[N_count, K_count\\] X_count;")
  # Should declare N_biomass with comment
  expect_match2(code_shared, "int<lower=1> N_biomass;\\s*//\\s*number of observations")
  # Should declare Y_biomass as vector
  expect_match2(code_shared, "vector\\[N_biomass\\] Y_biomass;\\s*//\\s*response variable")
  
  # Trend dimensions
  # Should declare n_trend
  expect_match2(code_shared, "int<lower=1> n_trend;\\s*//\\s*number of time points")
  # Should declare n_series_trend
  expect_match2(code_shared, "int<lower=1> n_series_trend;\\s*//\\s*number of series")
  # Should declare n_lv_trend
  expect_match2(code_shared, "int<lower=1> n_lv_trend;\\s*//\\s*latent variables")
  
  # Observation-to-trend mapping arrays
  # Should declare obs_trend_time_count array
  expect_match2(code_shared, "array\\[N_count\\] int obs_trend_time_count;")
  # Should declare obs_trend_series_count array
  expect_match2(code_shared, "array\\[N_count\\] int obs_trend_series_count;")
  # Should declare obs_trend_time_biomass array
  expect_match2(code_shared, "array\\[N_biomass\\] int obs_trend_time_biomass;")
  # Should declare obs_trend_series_biomass array
  expect_match2(code_shared, "array\\[N_biomass\\] int obs_trend_series_biomass;")
  
  # Times trend matrix - Should declare times_trend 2D array
  expect_match2(code_shared, "array\\[n_trend, n_series_trend\\] int times_trend;")
  
  # GLM compatibility vectors
  # Should declare mu_ones_count for GLM
  expect_match2(code_shared, "vector\\[1\\] mu_ones_count;\\s*//.*count")
  # Should declare mu_ones_biomass for GLM
  expect_match2(code_shared, "vector\\[1\\] mu_ones_biomass;\\s*//.*biomass")

  # ========== TRANSFORMED DATA BLOCK TESTS ==========
  # Should create identity matrix Z for non-factor model
  expect_match2(code_shared, "matrix\\[n_series_trend, n_lv_trend\\] Z = diag_matrix\\(rep_vector\\(1\\.0, n_lv_trend\\)\\);")

  # ========== PARAMETERS BLOCK TESTS ==========
  # Observation model parameters
  # Should declare b_count coefficients
  expect_match2(code_shared, "vector\\[Kc_count\\] b_count;\\s*//\\s*regression coefficients")
  # Should declare Intercept_count
  expect_match2(code_shared, "real Intercept_count;\\s*//\\s*temporary intercept")
  # Should declare sigma_count with lower bound
  expect_match2(code_shared, "real<lower=0> sigma_count;\\s*//\\s*dispersion parameter")
  
  # Trend parameters with _trend suffix
  # Should declare Intercept_trend (not vector mu_trend)
  expect_match2(code_shared, "real Intercept_trend;\\s*//\\s*trend intercept")
  # Should declare sigma_trend vector
  expect_match2(code_shared, "vector<lower=0>\\[n_lv_trend\\] sigma_trend;\\s*//\\s*innovation SDs")
  # Should declare L_Omega_trend for correlation
  expect_match2(code_shared, "cholesky_factor_corr\\[n_lv_trend\\] L_Omega_trend;\\s*//\\s*correlation Cholesky")
  # Should declare innovations_trend matrix
  expect_match2(code_shared, "matrix\\[n_trend, n_lv_trend\\] innovations_trend;\\s*//\\s*raw innovations")

  # ========== TRANSFORMED PARAMETERS BLOCK TESTS ==========
  # Should initialize lprior
  expect_match2(code_shared, "real lprior = 0;\\s*//\\s*prior contributions")
  
  # Should include Intercept_trend in lprior
  expect_match2(code_shared, "lprior \\+= student_t_lpdf\\(Intercept_trend \\| 3, 0, 2\\.5\\);")
  
  # Should create mu_trend from Intercept_trend using rep_vector
  expect_match2(code_shared, "vector\\[n_trend\\] mu_trend = rep_vector\\(Intercept_trend, n_trend\\);")
  
  # Should construct Sigma_trend covariance matrix
  expect_match2(code_shared, "matrix\\[n_lv_trend, n_lv_trend\\] Sigma_trend\\s*=\\s*diag_pre_multiply\\(sigma_trend, L_Omega_trend\\);")
  
  # RW latent variables
  # Should declare lv_trend matrix for latent variables (with _trend suffix)
  expect_match2(code_shared, "matrix\\[n_trend, n_lv_trend\\] lv_trend;")
  # Should declare L_Sigma_trend for scaling
  expect_match2(code_shared, "matrix\\[n_lv_trend, n_lv_trend\\] L_Sigma_trend\\s*=")
  # Should declare scaled_innovations_trend
  expect_match2(code_shared, "matrix\\[n_trend, n_lv_trend\\] scaled_innovations_trend\\s*=")
  # Should initialize first lv_trend from scaled innovations
  expect_match2(code_shared, "lv_trend\\[1, :\\] = scaled_innovations_trend\\[1, :\\];")
  # Should implement RW cumulative sum
  expect_match2(code_shared, "lv_trend\\[i, :\\] = lv_trend\\[i-1, :\\] \\+ scaled_innovations_trend\\[i, :\\];")
  
  # Trend matrix computation (shared, not response-specific)
  # Should declare shared trend matrix (not trend_count/trend_biomass)
  expect_match2(code_shared, "matrix\\[n_trend, n_series_trend\\] trend;")
  # Should compute trend using universal formula with lv_trend
  expect_match2(code_shared, "trend\\[i, s\\] = dot_product\\(Z\\[s, :\\], lv_trend\\[i, :\\]\\)\\s*\\+\\s*mu_trend\\[times_trend\\[i, s\\]\\];")
  
  # Linear predictor construction with trend injection
  # Should initialize mu_count from design matrix
  expect_match2(code_shared, "vector\\[N_count\\] mu_count = Xc_count \\* b_count;")
  # Should initialize mu_biomass from design matrix
  expect_match2(code_shared, "vector\\[N_biomass\\] mu_biomass = Xc_biomass \\* b_biomass;")
  # Should inject trend into mu_count using mapping arrays
  expect_match2(code_shared, "mu_count\\[n\\] \\+= Intercept_count \\+ trend\\[obs_trend_time_count\\[n\\], obs_trend_series_count\\[n\\]\\];")
  # Should inject trend into mu_biomass using mapping arrays
  expect_match2(code_shared, "mu_biomass\\[n\\] \\+= Intercept_biomass \\+ trend\\[obs_trend_time_biomass\\[n\\],\\s*obs_trend_series_biomass\\[n\\]\\];")

  # ========== MODEL BLOCK TESTS ==========
  # Should use GLM function with to_matrix(mu_count) and mu_ones_count
  expect_match2(code_shared, "normal_id_glm_lpdf\\(Y_count \\| to_matrix\\(mu_count\\), 0\\.0, mu_ones_count, sigma_count\\)")
  # Should use GLM function with to_matrix(mu_biomass) and mu_ones_biomass
  expect_match2(code_shared, "normal_id_glm_lpdf\\(Y_biomass \\| to_matrix\\(mu_biomass\\), 0\\.0, mu_ones_biomass,\\s*sigma_biomass\\)")
  
  # Trend priors in model block
  # Should NOT have prior for Intercept_trend in model block (it's in lprior)
  expect_false(grepl("Intercept_trend\\s*~", code_shared))
  # Should have some prior for sigma_trend (distribution may vary)
  expect_match2(code_shared, "sigma_trend ~ ")
  # Should have LKJ prior for correlation
  expect_match2(code_shared, "L_Omega_trend ~ lkj_corr_cholesky")
  # Should have standard normal prior for innovations
  expect_match2(code_shared, "to_vector\\(innovations_trend\\) ~ std_normal\\(\\);")

  # ========== GENERATED QUANTITIES BLOCK TESTS ==========
  # Should compute actual intercept for count
  expect_match2(code_shared, "real b_count_Intercept = Intercept_count - dot_product\\(means_X_count, b_count\\);")
  # Should compute actual intercept for biomass
  expect_match2(code_shared, "real b_biomass_Intercept = Intercept_biomass - dot_product\\(means_X_biomass, b_biomass\\);")

  # ========== NEGATIVE TESTS - Things that should NOT appear ==========
  # Should NOT have response-specific trend_count matrix
  expect_false(grepl("matrix.*trend_count", code_shared))
  # Should NOT have response-specific trend_biomass matrix
  expect_false(grepl("matrix.*trend_biomass", code_shared))
  
  # Should NOT declare mu_trend as parameter vector
  expect_false(grepl("parameters\\s*\\{[^}]*vector\\[[^]]*\\]\\s+mu_trend", code_shared))
  
  # Should NOT have duplicate model blocks
  expect_false(grepl("model\\s*\\{.*model\\s*\\{", code_shared))
  
  # Should NOT use response-specific trend_count in injection
  expect_false(grepl("mu_count\\[n\\] \\+= trend_count\\[", code_shared))
  
  # Should NOT have unsuffixed mu_ones
  expect_false(grepl("vector\\[1\\] mu_ones;", code_shared))

  # ========== SYNTAX AND STRUCTURE TESTS ==========
  # All braces should be properly matched
  open_braces <- length(gregexpr("\\{", code_shared)[[1]])
  close_braces <- length(gregexpr("\\}", code_shared)[[1]])
  expect_equal(open_braces, close_braces)
  
  # Parameters block should have multiple statements
  params_block <- sub(".*parameters\\s*\\{([^}]*)\\}.*", "\\1", code_shared)
  expect_gt(length(gregexpr(";", params_block)[[1]]), 5)

  # Final validation: ensure multivariate model compiles correctly
  expect_no_error(stancode(mf_shared, data = data, validate = TRUE))
})

test_that("stancode integrates custom priors correctly", {
  data <- setup_stan_test_data()$univariate
  mf <- mvgam_formula(y ~ x, trend_formula = ~ AR(p = 1))

  # Custom priors using brms syntax
  custom_priors <- brms::prior("normal(0, 0.75)", class = "ar1_trend") +
                   brms::prior("exponential(7)", class = "sigma_trend")

  code_with_priors <- stancode(mf, data = data, family = poisson(),
                               prior = custom_priors)
  code_default <- stancode(mf, data = data, family = poisson())

  expect_s3_class(code_with_priors, "stancode")
  expect_s3_class(code_default, "stancode")

  # Custom priors should be reflected in code
  expect_match2(code_with_priors, "normal\\(0,\\s*0\\.75\\)")
  expect_match2(code_with_priors, "exponential\\(7\\)")
})

test_that("stancode validates input parameters", {
  data <- setup_stan_test_data()$univariate

  # Invalid formula object
  expect_error(
    stancode("y ~ x", data = data),
    "object.*mvgam_formula"
  )

  # Invalid data
  mf <- mvgam_formula(y ~ x)
  expect_error(
    stancode(mf, data = "not_a_dataframe")
  )

  # Missing data parameter
  expect_error(
    stancode(mf, family = poisson()),
    "data"
  )
})

# standata Tests ----

test_that("standata.mvgam_formula returns proper list structure", {
  data <- setup_stan_test_data()$univariate
  mf <- mvgam_formula(y ~ x, trend_formula = ~ RW())

  standata_result <- standata(mf, data = data, family = poisson())

  # Should be a named list
  expect_type(standata_result, "list")
  expect_gt(length(standata_result), 0)
  expect_true(all(names(standata_result) != ""))

  # Should contain key data elements
  expect_true("N" %in% names(standata_result))  # Number of observations
  expect_true("Y" %in% names(standata_result))  # Response variable

  # Should contain trend-related data
  expect_true(any(grepl("trend", names(standata_result))))

  # Mapping functionality: should contain observation-to-trend mapping arrays
  expect_true("obs_trend_time" %in% names(standata_result))
  expect_true("obs_trend_series" %in% names(standata_result))

  # Mapping arrays should have correct dimensions
  expect_equal(length(standata_result$obs_trend_time), standata_result$N)
  expect_equal(length(standata_result$obs_trend_series), standata_result$N)

  # Mapping arrays should contain valid indices
  expect_true(all(standata_result$obs_trend_time >= 1))
  expect_true(all(standata_result$obs_trend_series >= 1))
})

test_that("standata handles different data structures", {
  test_data <- setup_stan_test_data()

  # Test with different datasets
  for (data_name in names(test_data)) {
    data <- test_data[[data_name]]
    mf <- mvgam_formula(y ~ 1, trend_formula = ~ RW())

    # Skip multivariate data for simple formula
    if (data_name == "multivariate") next

    standata_result <- standata(mf, data = data, family = poisson())

    expect_type(standata_result, "list")
    expect_true("N" %in% names(standata_result))
    expect_equal(standata_result$N, nrow(data))
  }
})

test_that("standata processes missing data correctly", {
  data <- setup_stan_test_data()$with_missings
  mf <- mvgam_formula(y ~ x, trend_formula = ~ RW())

  standata_result <- standata(mf, data = data, family = poisson())

  expect_type(standata_result, "list")

  # Should handle missing values appropriately
  # Exact behavior depends on mvgam's missing data handling
  expect_true("N" %in% names(standata_result))
  expect_gte(standata_result$N, 0)  # Should have some observations

  # Missing data mapping: mapping arrays should match reduced observation count
  expect_true("obs_trend_time" %in% names(standata_result))
  expect_true("obs_trend_series" %in% names(standata_result))

  # Mapping arrays should align with non-missing observations
  expect_equal(length(standata_result$obs_trend_time), standata_result$N)
  expect_equal(length(standata_result$obs_trend_series), standata_result$N)

  # Should have fewer observations than original data due to missings
  original_data <- setup_stan_test_data()$univariate
  expect_lt(standata_result$N, nrow(original_data))
})

test_that("standata integrates with trend systems", {
  data <- setup_stan_test_data()$univariate

  # Test different trend types
  trend_specs <- list(
    rw = ~ RW(),
    ar = ~ AR(p = 1),
    var = ~ VAR(p = 1, cor = TRUE)
  )

  for (trend_name in names(trend_specs)) {
    mf <- mvgam_formula(y ~ x, trend_formula = trend_specs[[trend_name]])
    standata_result <- standata(mf, data = data, family = poisson())

    expect_type(standata_result, "list")
    expect_gt(length(standata_result), 5)  # Should have multiple data components

    # Should contain trend-specific elements
    expect_true(any(grepl("trend", names(standata_result))))
  }
})

test_that("standata validates input consistency", {
  data <- setup_stan_test_data()$univariate

  # Data validation should occur
  mf <- mvgam_formula(y ~ x, trend_formula = ~ RW())

  # Valid call should work
  expect_silent({
    standata_result <- standata(mf, data = data, family = poisson())
  })

  # Invalid time series structure should error
  bad_data <- data
  bad_data$time <- c(1, 3, 5, 7:nrow(data))  # Irregular intervals

  expect_error(
    standata(mf, data = bad_data, family = poisson())
  )
})

# Integration Tests ----

test_that("stancode and standata are consistent", {
  data <- setup_stan_test_data()$univariate
  mf <- mvgam_formula(y ~ s(x), trend_formula = ~ AR(p = 1))

  code <- stancode(mf, data = data, family = poisson())
  standata_result <- standata(mf, data = data, family = poisson())

  # Both should succeed
  expect_s3_class(code, "stancode")
  expect_type(standata_result, "list")

  # Code should reference data elements that exist in standata
  # This is a simplified check - real validation would parse Stan code
  if ("N" %in% names(standata_result)) {
    expect_match2(code, "int.*N")
  }
})

test_that("stan functions work with complex model specifications", {
  data <- setup_stan_test_data()$multivariate

  # Complex multivariate model with priors
  mf <- mvgam_formula(
    mvbind(count, biomass) ~ s(x, by = habitat) + habitat,
    trend_formula = ~ AR(p = 1, cor = TRUE, n_lv = 2)
  )

  priors <- brms::prior("normal(0, 1)", class = "ar1_trend")

  # Should handle complex specifications
  expect_silent({
    code <- stancode(mf, data = data, prior = priors)
    standata_result <- standata(mf, data = data, prior = priors)
  })

  expect_s3_class(code, "stancode")
  expect_type(standata_result, "list")

  # Should contain complex model elements
  expect_match2(code, "count")
  expect_match2(code, "biomass")
  expect_match2(code, "ar1_trend")
})

test_that("stan functions preserve object attributes and metadata", {
  data <- setup_stan_test_data()$univariate
  mf <- mvgam_formula(y ~ x, trend_formula = ~ RW())

  code <- stancode(mf, data = data, family = poisson())
  standata_result <- standata(mf, data = data, family = poisson())

  # stancode should have correct class
  expect_equal(class(code), c("mvgamstancode", "stancode", "character"))

  # standata should preserve key information
  expect_type(standata_result, "list")
  expect_true(length(names(standata_result)) > 0)  # Should have named elements
})

# Edge Cases and Error Handling ----

test_that("stan functions handle edge cases gracefully", {
  # Very small dataset
  small_data <- data.frame(
    time = 1:3,
    series = factor(rep("s1", 3)),
    y = c(1, 2, 1),
    x = c(0.1, 0.2, 0.3)
  )

  mf <- mvgam_formula(y ~ 1, trend_formula = ~ RW())

  # Should handle small datasets
  expect_silent({
    code <- stancode(mf, data = small_data, family = poisson())
    standata_result <- standata(mf, data = small_data, family = poisson())
  })

  expect_s3_class(code, "stancode")
  expect_type(standata_result, "list")
})

test_that("stan functions provide informative error messages", {
  data <- setup_stan_test_data()$univariate

  # Missing required data components
  bad_data <- data[, c("y", "x")]  # Missing time and series
  mf <- mvgam_formula(y ~ x, trend_formula = ~ RW())

  # Should provide informative errors
  expect_error(
    stancode(mf, data = bad_data, family = poisson()),
    class = c("rlang_error", "error")
  )

  expect_error(
    standata(mf, data = bad_data, family = poisson()),
    class = c("rlang_error", "error")
  )
})
