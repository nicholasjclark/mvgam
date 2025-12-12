# stancode and standata Tests
#
# Integrated tests for stancode.mvgam_formula() and
# standata.mvgam_formula()

# Test Helper Functions ----

#' Robust pattern matching for Stan code with whitespace tolerance
#'
#' This function wraps grepl() to provide more robust pattern matching for
#' generated Stan code by trimming whitespace from both pattern and input.
#' This makes tests resilient to formatting changes in code generation.
#'
#' @param pattern Character string containing the regular expression pattern
#' @param x Character vector where matches are sought
#' @param fixed Logical. If TRUE, pattern is a literal string (default FALSE)
#' @param ignore.case Logical. If TRUE, case is ignored (default FALSE)
#' @param ... Additional arguments passed to grepl()
#'
#' @return Logical vector indicating matches
#' @noRd
stan_pattern <- function(pattern, x, ignore.case = FALSE, ...) {
  # Function to remove ALL whitespace for robust matching
  remove_whitespace <- function(text) {
    gsub("\\s+", "", text)
  }

  # Remove all whitespace from both pattern and input
  x_no_space <- remove_whitespace(x)

  # Check if pattern contains regex escapes (\\[ \\] \\( \\) etc.)
  # If so, remove whitespace but don't double-escape
  # If not, remove whitespace and escape for literal matching
  if (grepl("\\\\[\\[\\]()\\{\\}^$\\*\\+\\?\\.|]", pattern)) {
    # Pattern is already regex-escaped, just remove whitespace
    pattern_final <- remove_whitespace(pattern)
  } else {
    # Pattern is literal Stan code, remove whitespace then escape metacharacters
    pattern_no_space <- remove_whitespace(pattern)
    pattern_final <- gsub("([\\[\\]()\\{\\}^$\\*\\+\\?\\.|\\\\])", "\\\\\\1", pattern_no_space)
  }

  # Apply grepl with processed pattern and whitespace-free input
  grepl(pattern_final, x_no_space, ignore.case = ignore.case, ...)
}

# Test Data Setup ----

#' Create standardized test datasets for stancode/standata function testing
#' @noRd
setup_stan_test_data <- function() {
  set.seed(42)
  n_time <- 24
  n_series <- 3

  # Simple univariate dataset
  univariate <- data.frame(
    time = 1:n_time,
    series = factor(rep("series1", n_time)),
    y = rpois(n_time, lambda = 5),
    x = rnorm(n_time),
    temperature = rnorm(n_time, mean = 15, sd = 3),
    site = factor(rep(c("A", "B", "C"), length.out = n_time))
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

  # brms-generated dispersion parameter should be excluded
  expect_false(grepl("real<lower=0> sigma;", code_with_trend, fixed = TRUE))

  # GLM optimization enabled (fixed effects present)
  expect_true(stan_pattern("poisson_log_glm_lpmf", code_with_trend, fixed = TRUE))
  expect_true(stan_pattern("vector[1] mu_ones;", code_with_trend, fixed = TRUE))

  # Essential trend dimensions in data block
  expect_true(stan_pattern("int<lower=1> N_trend;", code_with_trend, fixed = TRUE))
  expect_true(stan_pattern("int<lower=1> N_series_trend;", code_with_trend, fixed = TRUE))
  expect_true(stan_pattern("int<lower=1> N_lv_trend;", code_with_trend, fixed = TRUE))

  # Critical times_trend array structure
  expect_true(stan_pattern("array\\[N_trend, N_series_trend\\] int times_trend;", code_with_trend))

  # Factor loading matrix for non-factor models
  expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z = diag_matrix", code_with_trend))

  # RW-specific innovation structure
  expect_true(stan_pattern("matrix\\[N_trend, N_lv_trend\\] innovations_trend;", code_with_trend))
  expect_true(stan_pattern("vector<lower=0>\\[N_lv_trend\\] sigma_trend;", code_with_trend))
  expect_true(stan_pattern("scaled_innovations_trend = innovations_trend \\* diag_matrix\\(sigma_trend\\)", code_with_trend))

  # 6. RW Dynamics Implementation
  # Random walk state evolution
  expect_true(stan_pattern("lv_trend\\[1,\\s*:\\s*\\] = scaled_innovations_trend\\[1,\\s*:\\s*\\]", code_with_trend))
  expect_true(stan_pattern("lv_trend\\[i, : \\] = lv_trend\\[i - 1, : \\] \\+ scaled_innovations_trend\\[i, : \\]", code_with_trend))

  # Critical universal pattern with dot_product
  expect_true(stan_pattern("trend\\[i, s\\] = dot_product\\(Z\\[s, :\\], lv_trend\\[i, :\\]\\) \\+ mu_trend\\[times_trend\\[i, s\\]\\]", code_with_trend))

  # Verify observation-to-trend mappings exist in data block
  expect_true(stan_pattern("array\\[N\\] int obs_trend_time;", code_with_trend))
  expect_true(stan_pattern("array\\[N\\] int obs_trend_series;", code_with_trend))

  # mu_trend should be just zeros for RW model (no trend predictors)
  expect_true(stan_pattern("vector\\[N_trend\\] mu_trend = rep_vector\\(0\\.0, N_trend\\);", code_with_trend))
  expect_false(grepl("mu_trend \\+= Intercept_trend", code_with_trend))

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

test_that("stancode uses GLM optimization with fixed effects + random effects", {
  data <- setup_stan_test_data()$univariate

  # Fixed effect + random effect - should use GLM optimization
  mf_fixed_plus_re <- mvgam_formula(y ~ x + (1 | site))
  code_fixed_plus_re <- stancode(mf_fixed_plus_re, data = data, family = poisson(), validate = FALSE)

  # Should use GLM optimization despite random effects presence
  expect_true(stan_pattern("poisson_log_glm_lpmf", code_fixed_plus_re))
  expect_true(stan_pattern("poisson_log_glm_lpmf\\(Y \\| Xc, mu, b\\);", code_fixed_plus_re))

  # Should still have random effects structure
  expect_true(stan_pattern("int<lower=1> N_1;", code_fixed_plus_re))
  expect_true(stan_pattern("vector<lower=0>\\[M_1\\] sd_1;", code_fixed_plus_re))
  expect_true(stan_pattern("r_1_1 = \\(sd_1\\[1\\] \\* \\(z_1\\[1\\]\\)\\);", code_fixed_plus_re))

  # Verify mu construction preserves both fixed and random effects
  expect_true(stan_pattern("mu \\+= Intercept;", code_fixed_plus_re))
  expect_true(stan_pattern("mu\\[n\\] \\+= r_1_1\\[J_1\\[n\\]\\] \\* Z_1_1\\[n\\];", code_fixed_plus_re))
})

test_that("stancode generates correct AR(p = c(1, 12)) seasonal model with negative binomial family", {
  data <- setup_stan_test_data()$univariate
  mf_with_trend <- mvgam_formula(
    y ~ x,
    trend_formula = ~ AR(p = c(1, 12))
  )
  code_with_trend <- stancode(
    mf_with_trend, data = data,
    family = negbinomial(),
    validate = FALSE
  )

  # Basic structure checks
  expect_s3_class(code_with_trend, "mvgamstancode")
  expect_s3_class(code_with_trend, "stancode")

  # AR-specific parameter declarations
  # Should have ar1_trend and ar12_trend, NOT ar2_trend through ar11_trend
  expect_true(stan_pattern("vector<lower=-1,upper=1>\\[N_lv_trend\\] ar1_trend;", code_with_trend))
  expect_true(stan_pattern("vector<lower=-1,upper=1>\\[N_lv_trend\\] ar12_trend;", code_with_trend))

  # Check no template placeholders
  expect_false(grepl("\\{max_lag\\}", code_with_trend))
  expect_false(grepl("\\{lags\\}", code_with_trend))

  # Should NOT have intermediate lags
  expect_false(grepl("ar2_trend", code_with_trend, fixed = TRUE))
  expect_false(grepl("ar3_trend", code_with_trend, fixed = TRUE))
  expect_false(grepl("ar11_trend", code_with_trend, fixed = TRUE))

  # Initialization: First 12 time points should be pure innovations
  expect_true(stan_pattern("for \\(i in 1:12\\)", code_with_trend))
  expect_true(stan_pattern("lv_trend\\[i, :\\] = scaled_innovations_trend\\[i, :\\];", code_with_trend))

  # AR dynamics: Should start from time point 13
  expect_true(stan_pattern("for \\(i in 13:N_trend\\)", code_with_trend))

  # AR dynamics equation: Should use both ar1_trend and ar12_trend
  expect_true(stan_pattern("ar1_trend\\[j\\] \\* lv_trend\\[i-1,j\\]", code_with_trend))
  expect_true(stan_pattern("ar12_trend\\[j\\] \\* lv_trend\\[i-12, j\\]", code_with_trend))

  # Combined AR equation pattern (looking for the sum of AR terms)
  expect_true(stan_pattern("lv_trend\\[i,j\\] = ar1_trend\\[j\\] \\* lv_trend\\[i-1,j\\] \\+ ar12_trend\\[j\\] \\* lv_trend\\[i-12,j\\] \\+ scaled_innovations_trend\\[i,j\\]", code_with_trend))

  # Priors for AR coefficients in model block (just check they exist, not specific values)
  expect_true(stan_pattern("ar1_trend ~ normal", code_with_trend))
  expect_true(stan_pattern("ar12_trend ~ normal", code_with_trend))

  # Negative binomial family-specific structure
  # Shape parameter for overdispersion
  expect_true(stan_pattern("real<lower=0> shape;", code_with_trend))

  # Negative binomial likelihood (not Poisson)
  expect_true(stan_pattern("neg_binomial_2_log_glm_lpmf", code_with_trend))
  expect_false(grepl("poisson_log_glm_lpmf", code_with_trend))

  # Should still have standard trend components
  expect_true(stan_pattern("matrix\\[N_trend, N_lv_trend\\] innovations_trend;", code_with_trend))
  expect_true(stan_pattern("vector<lower=0>\\[N_lv_trend\\] sigma_trend;", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_trend, N_lv_trend\\] lv_trend;", code_with_trend))

  # mu construction and trend addition in transformed parameters
  expect_true(stan_pattern("mu\\[n\\] \\+= trend\\[obs_trend_time\\[n\\], obs_trend_series\\[n\\]\\];", code_with_trend))

  # Universal trend computation pattern should still be present
  expect_true(stan_pattern("trend\\[i, s\\] = dot_product\\(Z\\[s, :\\], lv_trend\\[i, :\\]\\) \\+ mu_trend\\[times_trend\\[i, s\\]\\]", code_with_trend))

  # Mapping arrays should still be present
  expect_true(stan_pattern("array\\[N\\] int obs_trend_time", code_with_trend))
  expect_true(stan_pattern("array\\[N\\] int obs_trend_series", code_with_trend))

  # Check for no duplicated Stan blocks
  expect_equal(length(gregexpr("^\\s*data\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*transformed data\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*parameters\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*transformed parameters\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*model\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*generated quantities\\s*\\{", code_with_trend)[[1]]), 1)

  # Final validation: ensure model compiles correctly
  expect_no_error(stancode(mf_with_trend, data = data, family = negbinomial(), validate = TRUE))
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
  expect_true(stan_pattern("vector<lower=-1,upper=1>\\[N_lv_trend\\] ar2_trend;", code_with_trend))
  expect_true(stan_pattern("vector<lower=-1,upper=1>\\[N_lv_trend\\] ar4_trend;", code_with_trend))

  # Should NOT have other AR lags
  expect_false(grepl("ar1_trend", code_with_trend, fixed = TRUE))
  expect_false(grepl("ar3_trend", code_with_trend, fixed = TRUE))
  expect_false(grepl("ar5_trend", code_with_trend, fixed = TRUE))

  # MA parameter declaration
  expect_true(stan_pattern("vector<lower=-1,upper=1>\\[N_lv_trend\\] theta1_trend;", code_with_trend))

  # MA innovations matrix should be created from scaled innovations
  expect_true(stan_pattern("matrix\\[N_trend, N_lv_trend\\] ma_innovations_trend = scaled_innovations_trend;", code_with_trend))

  # MA transformation should be applied to the entire matrix first
  expect_true(stan_pattern("for \\(i in 2:N_trend\\)", code_with_trend))
  expect_true(stan_pattern("ma_innovations_trend\\[i, j\\] \\+= theta1_trend\\[j\\] \\* ma_innovations_trend\\[i-1, j\\];", code_with_trend))

  # Initialization: First 4 time points should use MA innovations
  expect_true(stan_pattern("for \\(i in 1:4\\)", code_with_trend))
  expect_true(stan_pattern("lv_trend\\[i, :\\] = ma_innovations_trend\\[i, :\\];", code_with_trend))

  # AR dynamics: Should start from time point 5 and use ma_innovations_trend
  expect_true(stan_pattern("for \\(i in 5:N_trend\\)", code_with_trend))

  # AR dynamics equation: Should use both ar2_trend and ar4_trend with MA innovations
  expect_true(stan_pattern("ar2_trend\\[j\\] \\* lv_trend\\[i-2, j\\]", code_with_trend))
  expect_true(stan_pattern("ar4_trend\\[j\\] \\* lv_trend\\[i-4, j\\]", code_with_trend))
  expect_true(stan_pattern("ma_innovations_trend\\[i, j\\]", code_with_trend))

  # Combined ARMA equation pattern - should be addition of AR lags plus MA innovation
  expect_true(stan_pattern("lv_trend\\[i, j\\] = ar2_trend\\[j\\] \\* lv_trend\\[i-2, j\\] \\+ ar4_trend\\[j\\] \\* lv_trend\\[i-4, j\\] \\+ ma_innovations_trend\\[i, j\\]", code_with_trend))

  # Priors for AR and MA coefficients in model block
  expect_true(stan_pattern("ar2_trend ~ normal", code_with_trend))
  expect_true(stan_pattern("ar4_trend ~ normal", code_with_trend))
  expect_true(stan_pattern("theta1_trend ~ normal", code_with_trend))

  # Should still have standard trend components
  expect_true(stan_pattern("matrix\\[N_trend, N_lv_trend\\] innovations_trend;", code_with_trend))
  expect_true(stan_pattern("vector<lower=0>\\[N_lv_trend\\] sigma_trend;", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_trend, N_lv_trend\\] lv_trend;", code_with_trend))

  # Universal trend computation pattern should still be present
  expect_true(stan_pattern("trend\\[i, s\\] = dot_product\\(Z\\[s, :\\], lv_trend\\[i, :\\]\\) \\+ mu_trend\\[times_trend\\[i, s\\]\\]", code_with_trend))

  # GLM optimization should still be present
  expect_true(stan_pattern("poisson_log_glm_lpmf", code_with_trend, fixed = TRUE))
  expect_true(stan_pattern("vector\\[1\\] mu_ones", code_with_trend))

  # Mapping arrays should still be present
  expect_true(stan_pattern("array\\[N\\] int obs_trend_time", code_with_trend))
  expect_true(stan_pattern("array\\[N\\] int obs_trend_series", code_with_trend))

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

test_that("stancode generates correct VAR(p = 2, ma = TRUE) VARMA model with tensor product smooths and presence covariate", {
  data <- setup_stan_test_data()$multivariate
  mf_with_trend <- mvgam_formula(
    bf(mvbind(count, biomass) ~ t2(x, time)) + set_rescor(FALSE),
    trend_formula = ~ presence + VAR(p = 2, ma = TRUE)
  )
  code_with_trend <- stancode(
    mf_with_trend, data = data,
    validate = FALSE
  )

  # Basic structure checks
  expect_s3_class(code_with_trend, "mvgamstancode")
  expect_s3_class(code_with_trend, "stancode")

  # Advanced mathematical functions for VARMA stationarity (Heaps 2022)
  expect_true(stan_pattern("matrix sqrtm\\(matrix A\\)", code_with_trend))
  expect_true(stan_pattern("matrix AtoP\\(matrix P_real\\)", code_with_trend))
  expect_true(stan_pattern("matrix initial_joint_var\\(", code_with_trend))

  # Multivariate observation data
  expect_true(stan_pattern("int<lower=1> N_count;", code_with_trend))
  expect_true(stan_pattern("vector\\[N_count\\] Y_count;", code_with_trend))
  expect_true(stan_pattern("int<lower=1> N_biomass;", code_with_trend))
  expect_true(stan_pattern("vector\\[N_biomass\\] Y_biomass;", code_with_trend))

  # Spline data structures
  expect_true(stan_pattern("int Ks_count;", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_count, Ks_count\\] Xs_count;", code_with_trend))
  expect_true(stan_pattern("int nb_count_1;", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_count, knots_count_1\\[1\\]\\] Zs_count_1_1;", code_with_trend))

  # Trend dimensions
  expect_true(stan_pattern("int<lower=1> N_trend;", code_with_trend))
  expect_true(stan_pattern("int<lower=1> N_series_trend;", code_with_trend))
  expect_true(stan_pattern("int<lower=1> N_lv_trend;", code_with_trend))

  # Trend formula data (presence covariate)
  expect_true(stan_pattern("int<lower=1> K_trend;", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_trend, K_trend\\] X_trend;", code_with_trend))
  # No centering variables since trend_formula has no explicit intercept

  # Mapping arrays with response-specific suffixes
  expect_true(stan_pattern("array\\[N_count\\] int obs_trend_time_count;", code_with_trend))
  expect_true(stan_pattern("array\\[N_count\\] int obs_trend_series_count;", code_with_trend))
  expect_true(stan_pattern("array\\[N_biomass\\] int obs_trend_time_biomass;", code_with_trend))
  expect_true(stan_pattern("array\\[N_biomass\\] int obs_trend_series_biomass;", code_with_trend))

  # Times trend matrix (2D integer array)
  expect_true(stan_pattern("array\\[N_trend, N_series_trend\\] int times_trend;", code_with_trend))

  # Trend formula design matrix variables (presence covariate, no intercept)
  expect_true(stan_pattern("int<lower=1> K_trend;", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_trend, K_trend\\] X_trend;", code_with_trend))
  # No centering variables (Kc_trend, Xc_trend, means_X_trend) since no explicit intercept

  # VAR initialization constants in transformed data
  expect_true(stan_pattern("vector\\[N_lv_trend\\] trend_zeros = rep_vector\\(0\\.0, N_lv_trend\\);", code_with_trend))

  # Factor loading matrix (identity for non-factor VAR)
  expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z = diag_matrix\\(rep_vector\\(1\\.0, N_lv_trend\\)\\);", code_with_trend))

  # Observation model parameters (multivariate with splines)
  expect_true(stan_pattern("real Intercept_count;", code_with_trend))
  expect_true(stan_pattern("vector\\[Ks_count\\] bs_count;", code_with_trend))
  expect_true(stan_pattern("vector\\[knots_count_1\\[1\\]\\] zs_count_1_1;", code_with_trend))
  expect_true(stan_pattern("vector<lower=0>\\[nb_count_1\\] sds_count_1;", code_with_trend))
  expect_true(stan_pattern("real<lower=0> sigma_count;", code_with_trend))

  # Trend parameters (no intercept since formula is ~ presence + VAR, not ~ 1 + presence + VAR)
  expect_true(stan_pattern("vector\\[K_trend\\] b_trend;", code_with_trend))

  # VAR coefficient matrices (raw/unconstrained for stationarity)
  expect_true(stan_pattern("array\\[2\\] matrix\\[N_lv_trend, N_lv_trend\\] A_raw_trend;", code_with_trend))

  # MA coefficient matrices
  expect_true(stan_pattern("array\\[1\\] matrix\\[N_lv_trend, N_lv_trend\\] D_raw_trend;", code_with_trend))

  # Hierarchical hyperparameters (Heaps 2022 methodology)
  expect_true(stan_pattern("array\\[2\\] vector\\[2\\] Amu_trend;", code_with_trend))
  expect_true(stan_pattern("array\\[2\\] vector<lower=0>\\[2\\] Aomega_trend;", code_with_trend))
  expect_true(stan_pattern("array\\[2\\] vector\\[1\\] Dmu_trend;", code_with_trend))
  expect_true(stan_pattern("array\\[2\\] vector<lower=0>\\[1\\] Domega_trend;", code_with_trend))

  # Innovation parameters
  expect_true(stan_pattern("vector<lower=0>\\[N_lv_trend\\] sigma_trend;", code_with_trend))
  expect_true(stan_pattern("cholesky_factor_corr\\[N_lv_trend\\] L_Omega_trend;", code_with_trend))

  # Standard latent variables
  expect_true(stan_pattern("matrix\\[N_trend, N_lv_trend\\] lv_trend;", code_with_trend))

  # Spline coefficient computations in transformed parameters
  expect_true(stan_pattern("s_count_1_1 = sds_count_1\\[1\\] \\* zs_count_1_1;", code_with_trend))
  expect_true(stan_pattern("s_biomass_1_1 = sds_biomass_1\\[1\\] \\* zs_biomass_1_1;", code_with_trend))

  # lprior initialization and accumulation
  expect_true(stan_pattern("real lprior = 0;", code_with_trend))

  # Trend linear predictor with presence covariate (no intercept since ~ presence + VAR, not ~ 1 + presence + VAR)
  expect_true(stan_pattern("vector\\[N_trend\\] mu_trend = rep_vector\\(0\\.0, N_trend\\);", code_with_trend))
  expect_true(stan_pattern("mu_trend \\+= X_trend \\* b_trend;", code_with_trend))

  # Innovation covariance construction
  expect_true(stan_pattern("matrix\\[N_lv_trend, N_lv_trend\\] L_Sigma_trend = diag_pre_multiply\\(sigma_trend, L_Omega_trend\\);", code_with_trend))
  expect_true(stan_pattern("cov_matrix\\[N_lv_trend\\] Sigma_trend = multiply_lower_tri_self_transpose\\(L_Sigma_trend\\);", code_with_trend))

  # Stationarity transformations (Heaps 2022)
  expect_true(stan_pattern("array\\[2\\] matrix\\[N_lv_trend, N_lv_trend\\] A_trend;", code_with_trend))
  expect_true(stan_pattern("array\\[1\\] matrix\\[N_lv_trend, N_lv_trend\\] D_trend;", code_with_trend))
  expect_true(stan_pattern("P_var\\[i\\] = AtoP\\(A_raw_trend\\[i\\]\\);", code_with_trend))
  expect_true(stan_pattern("result_var = rev_mapping\\(P_var, Sigma_trend\\);", code_with_trend))

  # Initial joint covariance matrix
  expect_true(stan_pattern("Omega_trend = initial_joint_var\\(Sigma_trend, A_trend, D_trend\\);", code_with_trend))

  # Universal trend computation pattern
  expect_true(stan_pattern("trend\\[i, s\\] = dot_product\\(Z\\[s, :\\], lv_trend\\[i, :\\]\\) \\+ mu_trend\\[times_trend\\[i, s\\]\\];", code_with_trend))

  # Multivariate linear predictors with splines
  expect_true(stan_pattern("vector\\[N_count\\] mu_count = rep_vector\\(0\\.0, N_count\\);", code_with_trend))

  # 2D tensor product smooth: t2(x, time) creates multiple marginal smooth components
  expect_true(stan_pattern("Zs_count_1_1", code_with_trend))
  expect_true(stan_pattern("Zs_count_1_2", code_with_trend))
  expect_true(stan_pattern("Zs_count_1_3", code_with_trend))

  # Same pattern for biomass response
  expect_true(stan_pattern("Zs_biomass_1_1", code_with_trend))
  expect_true(stan_pattern("Zs_biomass_1_2", code_with_trend))
  expect_true(stan_pattern("Zs_biomass_1_3", code_with_trend))

  # Trend injection using response-specific mapping arrays
  expect_true(stan_pattern("mu_count\\[n\\] \\+= trend\\[obs_trend_time_count\\[n\\], obs_trend_series_count\\[n\\]\\];", code_with_trend))
  expect_true(stan_pattern("mu_biomass\\[n\\] \\+= trend\\[obs_trend_time_biomass\\[n\\], obs_trend_series_biomass\\[n\\]\\];", code_with_trend))

  expect_true(stan_pattern("array\\[N_count\\] int obs_trend_time_count;", code_with_trend))
  expect_true(stan_pattern("array\\[N_count\\] int obs_trend_series_count;", code_with_trend))
  expect_true(stan_pattern("array\\[N_biomass\\] int obs_trend_time_biomass;", code_with_trend))
  expect_true(stan_pattern("array\\[N_biomass\\] int obs_trend_series_biomass;", code_with_trend))

  # Verify mu_trend is computed from trend formula (no intercept)
  expect_true(stan_pattern("mu_trend \\+= X_trend \\* b_trend;", code_with_trend))

  # Check no template placeholders remain
  expect_false(grepl("\\{lags\\}", code_with_trend))
  expect_false(grepl("\\{n_lags\\}", code_with_trend))
  expect_false(grepl("\\{response\\}", code_with_trend))

  # Check for duplicate parameter declarations
  sigma_trend_count <- length(gregexpr("vector<lower=0>\\[N_lv_trend\\] sigma_trend;", code_with_trend)[[1]])
  expect_equal(sigma_trend_count, 1)

  L_Omega_count <- length(gregexpr("cholesky_factor_corr\\[N_lv_trend\\] L_Omega_trend;", code_with_trend)[[1]])
  expect_equal(L_Omega_count, 1)

  # Verify trend injection happens in a loop
  expect_true(stan_pattern("for \\(n in 1:N_count\\) \\{[^}]*mu_count\\[n\\] \\+= trend", code_with_trend))
  expect_true(stan_pattern("for \\(n in 1:N_biomass\\) \\{[^}]*mu_biomass\\[n\\] \\+= trend", code_with_trend))

  # Standard observation likelihoods (not GLM optimized for splines)
  expect_true(stan_pattern("target \\+= normal_lpdf\\(Y_count \\| mu_count, sigma_count\\);", code_with_trend))
  expect_true(stan_pattern("target \\+= normal_lpdf\\(Y_biomass \\| mu_biomass, sigma_biomass\\);", code_with_trend))

  # Prior accumulation
  expect_true(stan_pattern("target \\+= lprior;", code_with_trend))
  expect_true(stan_pattern("target \\+= std_normal_lpdf\\(zs_count_1_1\\);", code_with_trend))

  # Initial joint distribution
  expect_true(stan_pattern("mu_init_trend = rep_vector\\(0\\.0, \\(2 \\+ 1\\) \\* N_lv_trend\\);", code_with_trend))
  expect_true(stan_pattern("init_trend ~ multi_normal\\(mu_init_trend, Omega_trend\\);", code_with_trend))

  # VARMA dynamics implementation
  expect_true(stan_pattern("vector\\[N_lv_trend\\] ma_init_trend;", code_with_trend))

  # VAR component with initialization handling
  expect_true(stan_pattern("for \\(i in 1:2\\)", code_with_trend))
  expect_true(stan_pattern("if \\(t - i <= 0\\)", code_with_trend))
  expect_true(stan_pattern("mu_t_trend\\[t\\] \\+= A_trend\\[i\\] \\* lv_trend\\[t - i, :\\]';", code_with_trend))

  # MA component
  expect_true(stan_pattern("if \\(t - 1 <= 0\\)", code_with_trend))
  expect_true(stan_pattern("mu_t_trend\\[t\\] \\+= D_trend\\[1\\] \\* ma_init_trend;", code_with_trend))
  expect_true(stan_pattern("} else \\{", code_with_trend))

  # MA specific parameters must exist for VARMA model
  expect_true(stan_pattern("array\\[1\\] matrix\\[N_lv_trend, N_lv_trend\\] D_raw_trend;", code_with_trend))
  expect_true(stan_pattern("array\\[1\\] matrix\\[N_lv_trend, N_lv_trend\\] D_trend;", code_with_trend))
  expect_true(stan_pattern("vector\\[N_lv_trend\\] ma_init_trend", code_with_trend))

  # Latent variable likelihood
  expect_true(stan_pattern("lv_trend\\[t, :\\]' ~ multi_normal\\(mu_t_trend\\[t\\], Sigma_trend\\);", code_with_trend))

  # Prior structure validation (existence, not specific distributions)
  # Hierarchical priors should exist
  expect_true(stan_pattern("Amu_trend\\[", code_with_trend))
  expect_true(stan_pattern("Aomega_trend\\[", code_with_trend))
  expect_true(stan_pattern("Dmu_trend\\[", code_with_trend))
  expect_true(stan_pattern("Domega_trend\\[", code_with_trend))

  # Structured priors for raw coefficients should exist
  expect_true(stan_pattern("A_raw_trend\\[.*\\] ~ ", code_with_trend))
  expect_true(stan_pattern("D_raw_trend\\[.*\\] ~ ", code_with_trend))

  # Basic parameter priors should exist
  expect_true(stan_pattern("sigma_trend ~", code_with_trend))
  expect_true(stan_pattern("L_Omega_trend ~", code_with_trend))

  # Intercept transformations in generated quantities
  expect_true(stan_pattern("real b_count_Intercept = Intercept_count;", code_with_trend))
  expect_true(stan_pattern("real b_biomass_Intercept = Intercept_biomass;", code_with_trend))
  # No b_trend_Intercept since trend formula has no explicit intercept

  # Anti-patterns: Should NOT have simple AR parameters (this is VAR, not AR)
  expect_false(grepl("vector.*ar1_trend", code_with_trend))
  expect_false(grepl("vector.*ar2_trend", code_with_trend))

  # Should NOT have simple MA parameters (this uses structured coefficients)
  expect_false(grepl("vector.*theta1_trend", code_with_trend))
  expect_false(grepl("matrix.*ma_innovations_trend", code_with_trend))

  # Should NOT have GLM optimization (splines prevent this)
  expect_false(grepl("_glm_lpdf", code_with_trend))
  expect_false(grepl("_glm_lpmf", code_with_trend))
  expect_false(grepl("mu_ones", code_with_trend))

  # Should NOT have simple RW dynamics
  expect_false(grepl("lv_trend\\[i, :\\] = lv_trend\\[i-1, :\\] \\+ scaled_innovations_trend", code_with_trend))

  # Should NOT have factor loading estimation (non-factor VAR model)
  expect_false(grepl("vector.*Z_raw", code_with_trend))
  expect_false(grepl("vector\\[N_series_trend \\* N_lv_trend\\] Z_raw", code_with_trend))
  expect_false(grepl("Z_raw\\[index\\]", code_with_trend))
  expect_false(grepl("Z\\[i, j\\] = Z_raw\\[index\\]", code_with_trend))
  expect_false(grepl("for \\(i in j : N_series_trend\\)", code_with_trend))

  # Should NOT have unsuffixed parameter names (could conflict with observation model)
  expect_false(grepl("real sigma;", code_with_trend))
  expect_false(grepl("vector.*b;", code_with_trend))

  # Should NOT have incorrect VARMA structure
  expect_false(grepl("for \\(i in 1:1\\)", code_with_trend)) # Should be 1:2 for VAR(2)
  expect_false(grepl("ar_dynamics", code_with_trend)) # Should use A_trend matrices

  # Tensor product smooth structure (t2(x, time))
  # brms decomposes tensor products into multiple indexed marginal components
  expect_true(stan_pattern("array\\[nb_count_1\\] int knots_count_1;", code_with_trend))
  expect_true(stan_pattern("array\\[nb_biomass_1\\] int knots_biomass_1;", code_with_trend))

  # Multiple knot components for tensor product (marginal smooth decomposition)
  expect_true(stan_pattern("knots_count_1\\[1\\]", code_with_trend))
  expect_true(stan_pattern("knots_count_1\\[2\\]", code_with_trend))
  expect_true(stan_pattern("knots_count_1\\[3\\]", code_with_trend))

  # Multiple Z matrices for tensor product components
  expect_true(stan_pattern("matrix\\[N_count, knots_count_1\\[1\\]\\] Zs_count_1_1;", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_count, knots_count_1\\[2\\]\\] Zs_count_1_2;", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_count, knots_count_1\\[3\\]\\] Zs_count_1_3;", code_with_trend))

  # Multiple coefficient vectors for tensor product components
  expect_true(stan_pattern("vector\\[knots_count_1\\[1\\]\\] zs_count_1_1;", code_with_trend))
  expect_true(stan_pattern("vector\\[knots_count_1\\[2\\]\\] zs_count_1_2;", code_with_trend))
  expect_true(stan_pattern("vector\\[knots_count_1\\[3\\]\\] zs_count_1_3;", code_with_trend))

  # Check for no duplicated Stan blocks
  expect_equal(length(gregexpr("^\\s*functions\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*data\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*transformed data\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*parameters\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*transformed parameters\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*model\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*generated quantities\\s*\\{", code_with_trend)[[1]]), 1)

  # Final validation: ensure model compiles correctly
  expect_no_error(stancode(mf_with_trend, data = data, validate = TRUE))
})

test_that("stancode generates correct multivariate factor AR(p = 1, n_lv = 2, cor =
  TRUE) model with three families", {
    data <- setup_stan_test_data()$multivariate
    mf_with_trend <- mvgam_formula(
      formula = bf(count ~ x, family = poisson()) +
        bf(presence ~ x, family = bernoulli()) +
        bf(biomass ~ x, family = Gamma()),
      trend_formula = ~ -1 + AR(p = 1, n_lv = 2, cor = TRUE)
    )
    code_with_trend <- stancode(
      mf_with_trend, data = data,
      validate = FALSE
    )

    # Basic structure checks
    expect_s3_class(code_with_trend, "mvgamstancode")
    expect_s3_class(code_with_trend, "stancode")

    # Empty functions block (no custom functions needed for AR factor model)
    expect_true(stan_pattern("functions \\{\\s*\\}", code_with_trend))

    # Three-family multivariate observation data
    expect_true(stan_pattern("int<lower=1> N_count;",
                      code_with_trend))
    expect_true(stan_pattern("array\\[N_count\\] int Y_count;",
                      code_with_trend))
    expect_true(stan_pattern("int<lower=1> N_presence;",
                      code_with_trend))
    expect_true(stan_pattern("array\\[N_presence\\] int Y_presence;",
                      code_with_trend))
    expect_true(stan_pattern("int<lower=1> N_biomass;",
                      code_with_trend))
    expect_true(stan_pattern("vector\\[N_biomass\\] Y_biomass;",
                      code_with_trend))

    # Population-level design matrices for all three families
    expect_true(stan_pattern("matrix\\[N_count, K_count\\] X_count;", code_with_trend))
    expect_true(stan_pattern("matrix\\[N_presence, K_presence\\] X_presence;", code_with_trend))
    expect_true(stan_pattern("matrix\\[N_biomass, K_biomass\\] X_biomass;", code_with_trend))

    # Trend dimensions
    expect_true(stan_pattern("int<lower=1> N_trend;", code_with_trend))
    expect_true(stan_pattern("int<lower=1> N_series_trend;",
                      code_with_trend))
    expect_true(stan_pattern("int<lower=1> N_lv_trend;", code_with_trend))

    # Observation-to-trend mappings for all three families
    expect_true(stan_pattern("array\\[N_count\\] int obs_trend_time_count;", code_with_trend))
    expect_true(stan_pattern("array\\[N_count\\] int obs_trend_series_count;",
                      code_with_trend))
    expect_true(stan_pattern("array\\[N_presence\\] int obs_trend_time_presence;",
                      code_with_trend))
    expect_true(stan_pattern("array\\[N_presence\\] int obs_trend_series_presence;",
                      code_with_trend))
    expect_true(stan_pattern("array\\[N_biomass\\] int obs_trend_time_biomass;",
                      code_with_trend))
    expect_true(stan_pattern("array\\[N_biomass\\] int obs_trend_series_biomass;",
                      code_with_trend))

    # Times trend matrix
    expect_true(stan_pattern("array\\[N_trend, N_series_trend\\] int times_trend;",
                      code_with_trend))

    # GLM compatibility vectors for discrete families only
    expect_true(stan_pattern("vector\\[1\\] mu_ones_count;", code_with_trend))
    expect_true(stan_pattern("vector\\[1\\] mu_ones_presence;",
                      code_with_trend))

    # Centered design matrices in transformed data
    expect_true(stan_pattern("matrix\\[N_count, Kc_count\\] Xc_count;",
                      code_with_trend))
    expect_true(stan_pattern("matrix\\[N_presence, Kc_presence\\] Xc_presence;", code_with_trend))
    expect_true(stan_pattern("matrix\\[N_biomass, Kc_biomass\\] Xc_biomass;", code_with_trend))

    # Centering loops for all three families
    expect_true(stan_pattern("for \\(i in 2:K_count\\)", code_with_trend))
    expect_true(stan_pattern("means_X_count\\[i - 1\\] = mean\\(X_count\\[ : , i\\]\\);",
                      code_with_trend))
    expect_true(stan_pattern("for \\(i in 2:K_presence\\)", code_with_trend))
    expect_true(stan_pattern("for \\(i in 2:K_biomass\\)", code_with_trend))

    # Observation model parameters for all three families
    expect_true(stan_pattern("vector\\[Kc_count\\] b_count;",
                      code_with_trend))
    expect_true(stan_pattern("real Intercept_count;", code_with_trend))
    expect_true(stan_pattern("vector\\[Kc_presence\\] b_presence;",
                      code_with_trend))
    expect_true(stan_pattern("real Intercept_presence;",
                      code_with_trend))
    expect_true(stan_pattern("vector\\[Kc_biomass\\] b_biomass;",
                      code_with_trend))
    expect_true(stan_pattern("real Intercept_biomass;", code_with_trend))
    expect_true(stan_pattern("real<lower=0> shape_biomass;",
                      code_with_trend))

    # Factor AR(1) trend parameters
    expect_true(stan_pattern("vector<lower=-1,upper=1>\\[N_lv_trend\\] ar1_trend;", code_with_trend))
    expect_true(stan_pattern("vector<lower=0>\\[N_lv_trend\\] sigma_trend;",
                      code_with_trend))
    expect_true(stan_pattern("cholesky_factor_corr\\[N_lv_trend\\] L_Omega_trend;", code_with_trend))

    # Factor loading parameters (estimated for n_lv = 2)
    expect_true(stan_pattern("vector\\[N_series_trend \\* N_lv_trend\\] Z_raw;", code_with_trend))

    # Innovation matrix
    expect_true(stan_pattern("matrix\\[N_trend, N_lv_trend\\] innovations_trend;",
                      code_with_trend))

    # lprior initialization with family-specific priors
    expect_true(stan_pattern("real lprior = 0;", code_with_trend))

    # Factor loading matrix construction with identifiability constraints
    expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z = rep_matrix\\(0,
  N_series_trend, N_lv_trend\\);", code_with_trend))
    expect_true(stan_pattern("int index = 1;", code_with_trend))
    expect_true(stan_pattern("for \\(j in 1 : N_lv_trend\\)", code_with_trend))
    expect_true(stan_pattern("for \\(i in j : N_series_trend\\)", code_with_trend))
    expect_true(stan_pattern("Z\\[i, j\\] = Z_raw\\[index\\];", code_with_trend))

    # Innovation covariance construction
    expect_true(stan_pattern("matrix\\[N_lv_trend, N_lv_trend\\] L_Sigma_trend =
  diag_pre_multiply\\(sigma_trend, L_Omega_trend\\);", code_with_trend))
    expect_true(stan_pattern("scaled_innovations_trend = innovations_trend \\* L_Sigma_trend';", code_with_trend))

    # AR(1) latent variable dynamics
    expect_true(stan_pattern("matrix\\[N_trend, N_lv_trend\\] lv_trend;", code_with_trend))
    expect_true(stan_pattern("lv_trend\\[i,:\\] = scaled_innovations_trend\\[i,:\\];",
                      code_with_trend))
    expect_true(stan_pattern("for \\(i in 2:N_trend\\)", code_with_trend))
    expect_true(stan_pattern("for \\(j in 1:N_lv_trend\\)", code_with_trend))
    expect_true(stan_pattern("lv_trend\\[i,j\\] = ar1_trend\\[j\\] \\* lv_trend\\[i-1,j\\] \\+ scaled_innovations_trend\\[i,j\\];", code_with_trend))

    # Zero trend mean vector (for ~ -1 specification)
    expect_true(stan_pattern("vector\\[N_trend\\] mu_trend = rep_vector\\(0\\.0, N_trend\\);",
                      code_with_trend))

    # Universal trend computation pattern
    expect_true(stan_pattern("matrix\\[N_trend, N_series_trend\\] trend;", code_with_trend))
    expect_true(stan_pattern("trend\\[i, s\\] = dot_product\\(Z\\[s, :\\], lv_trend\\[i,
  :\\]\\) \\+ mu_trend\\[times_trend\\[i, s\\]\\];", code_with_trend))

    # Family-specific linear predictors
    expect_true(stan_pattern("vector\\[N_count\\] mu_count = Xc_count \\* b_count;",
                      code_with_trend))
    expect_true(stan_pattern("vector\\[N_presence\\] mu_presence = Xc_presence \\*
  b_presence;", code_with_trend))
    expect_true(stan_pattern("vector\\[N_biomass\\] mu_biomass = rep_vector\\(0\\.0,
  N_biomass\\);", code_with_trend))
    expect_true(stan_pattern("mu_biomass \\+= Intercept_biomass \\+ Xc_biomass \\*
  b_biomass;", code_with_trend))

    # Trend injection for all three families
    expect_true(stan_pattern("for \\(n in 1:N_count\\)", code_with_trend))
    expect_true(stan_pattern("mu_count\\[n\\] \\+= Intercept_count \\+
  trend\\[obs_trend_time_count\\[n\\], obs_trend_series_count\\[n\\]\\];",
                      code_with_trend))
    expect_true(stan_pattern("for \\(n in 1:N_presence\\)", code_with_trend))
    expect_true(stan_pattern("mu_presence\\[n\\] \\+= Intercept_presence \\+
  trend\\[obs_trend_time_presence\\[n\\], obs_trend_series_presence\\[n\\]\\];",
                      code_with_trend))
    expect_true(stan_pattern("for \\(n in 1:N_biomass\\)", code_with_trend))
    expect_true(stan_pattern("mu_biomass\\[n\\] \\+= trend\\[obs_trend_time_biomass\\[n\\],
  obs_trend_series_biomass\\[n\\]\\];", code_with_trend))

    # Gamma inverse link transformation
    expect_true(stan_pattern("mu_biomass = inv\\(mu_biomass\\);", code_with_trend))

    # Three-family likelihoods with GLM optimization for discrete families
    expect_true(stan_pattern("target \\+= poisson_log_glm_lpmf\\(Y_count \\|
  to_matrix\\(mu_count\\), 0\\.0, mu_ones_count\\);", code_with_trend))
    expect_true(stan_pattern("target \\+= bernoulli_logit_glm_lpmf\\(Y_presence \\|
  to_matrix\\(mu_presence\\), 0\\.0, mu_ones_presence\\);", code_with_trend))
    expect_true(stan_pattern("target \\+= gamma_lpdf\\(Y_biomass \\| shape_biomass", code_with_trend))

    # Prior accumulation
    expect_true(stan_pattern("target \\+= lprior;", code_with_trend))

    # Trend parameter priors (existence, not specific distributions)
    expect_true(stan_pattern("ar1_trend ~", code_with_trend))
    expect_true(stan_pattern("sigma_trend ~", code_with_trend))
    expect_true(stan_pattern("L_Omega_trend ~", code_with_trend))
    expect_true(stan_pattern("Z_raw ~", code_with_trend))
    expect_true(stan_pattern("to_vector\\(innovations_trend\\) ~", code_with_trend))

    # Generated quantities for all three families
    expect_true(stan_pattern("real b_count_Intercept = Intercept_count -
  dot_product\\(means_X_count, b_count\\);", code_with_trend))
    expect_true(stan_pattern("real b_presence_Intercept = Intercept_presence -
  dot_product\\(means_X_presence, b_presence\\);", code_with_trend))
    expect_true(stan_pattern("real b_biomass_Intercept = Intercept_biomass -
  dot_product\\(means_X_biomass, b_biomass\\);", code_with_trend))

    # Anti-patterns: Should NOT have trend intercept parameters (~ -1 specification)
    expect_false(grepl("real Intercept_trend;", code_with_trend))
    expect_false(grepl("vector.*b_trend;", code_with_trend))

    # Should NOT have design matrices for trend formula (~ -1 has no covariates)
    expect_false(grepl("matrix.*X_trend;", code_with_trend))
    expect_false(grepl("matrix.*Xc_trend;", code_with_trend))

    # Should NOT have simple AR structure (this is factor model)
    expect_false(grepl("matrix.*scaled_innovations_trend.*diag_matrix",
                       code_with_trend))

    # Should NOT have correlation parameter without structure (uses Cholesky factor)
    expect_false(grepl("corr_matrix.*Omega_trend", code_with_trend))

    # Should NOT have mu_ones for biomass (Gamma doesn't use GLM optimization)
    expect_false(grepl("mu_ones_biomass", code_with_trend))

    # Should NOT have vector ar1 coefficients without bounds
    expect_false(grepl("vector\\[N_lv_trend\\] ar1_trend;", code_with_trend))

    # Check for no duplicated Stan blocks
    expect_equal(length(gregexpr("^\\s*functions\\s*\\{", code_with_trend)[[1]]), 1)
    expect_equal(length(gregexpr("^\\s*data\\s*\\{", code_with_trend)[[1]]), 1)
    expect_equal(length(gregexpr("^\\s*transformed data\\s*\\{",
                                 code_with_trend)[[1]]), 1)
    expect_equal(length(gregexpr("^\\s*parameters\\s*\\{", code_with_trend)[[1]]), 1)
    expect_equal(length(gregexpr("^\\s*transformed parameters\\s*\\{",
                                 code_with_trend)[[1]]), 1)
    expect_equal(length(gregexpr("^\\s*model\\s*\\{", code_with_trend)[[1]]), 1)
    expect_equal(length(gregexpr("^\\s*generated quantities\\s*\\{",
                                 code_with_trend)[[1]]), 1)

    # Final validation: ensure model compiles correctly
    expect_no_error(stancode(mf_with_trend, data = data, validate = TRUE))
  })

test_that("stancode generates correct multivariate factor AR(p = 1, n_lv = 2, cor = TRUE) model with three families", {
  data <- setup_stan_test_data()$multivariate
  mf_with_trend <- mvgam_formula(
    formula = bf(count ~ x, family = poisson()) +
      bf(presence ~ x, family = bernoulli()) +
      bf(biomass ~ x, family = Gamma()),
    trend_formula = ~ -1 + AR(p = 1, n_lv = 2, cor = TRUE)
  )
  code_with_trend <- stancode(
    mf_with_trend, data = data,
    validate = FALSE
  )

  # Basic structure checks
  expect_s3_class(code_with_trend, "mvgamstancode")
  expect_s3_class(code_with_trend, "stancode")

  # Empty functions block (no custom functions needed for AR factor model)
  expect_true(stan_pattern("functions \\{\\s*\\}", code_with_trend))

  # Three-family multivariate observation data
  expect_true(stan_pattern("int<lower=1> N_count;", code_with_trend))
  expect_true(stan_pattern("array\\[N_count\\] int Y_count;", code_with_trend))
  expect_true(stan_pattern("int<lower=1> N_presence;", code_with_trend))
  expect_true(stan_pattern("array\\[N_presence\\] int Y_presence;", code_with_trend))
  expect_true(stan_pattern("int<lower=1> N_biomass;", code_with_trend))
  expect_true(stan_pattern("vector\\[N_biomass\\] Y_biomass;", code_with_trend))

  # Population-level design matrices for all three families
  expect_true(stan_pattern("matrix\\[N_count, K_count\\] X_count;", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_presence, K_presence\\] X_presence;", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_biomass, K_biomass\\] X_biomass;", code_with_trend))

  # Trend dimensions
  expect_true(stan_pattern("int<lower=1> N_trend;", code_with_trend))
  expect_true(stan_pattern("int<lower=1> N_series_trend;", code_with_trend))
  expect_true(stan_pattern("int<lower=1> N_lv_trend;", code_with_trend))

  # Observation-to-trend mappings for all three families
  expect_true(stan_pattern("array\\[N_count\\] int obs_trend_time_count;", code_with_trend))
  expect_true(stan_pattern("array\\[N_count\\] int obs_trend_series_count;", code_with_trend))
  expect_true(stan_pattern("array\\[N_presence\\] int obs_trend_time_presence;", code_with_trend))
  expect_true(stan_pattern("array\\[N_presence\\] int obs_trend_series_presence;", code_with_trend))
  expect_true(stan_pattern("array\\[N_biomass\\] int obs_trend_time_biomass;", code_with_trend))
  expect_true(stan_pattern("array\\[N_biomass\\] int obs_trend_series_biomass;", code_with_trend))

  # Times trend matrix
  expect_true(stan_pattern("array\\[N_trend, N_series_trend\\] int times_trend;", code_with_trend))

  # GLM compatibility vectors for discrete families only
  expect_true(stan_pattern("vector\\[1\\] mu_ones_count;", code_with_trend))
  expect_true(stan_pattern("vector\\[1\\] mu_ones_presence;", code_with_trend))

  # Centered design matrices in transformed data
  expect_true(stan_pattern("matrix\\[N_count, Kc_count\\] Xc_count;", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_presence, Kc_presence\\] Xc_presence;", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_biomass, Kc_biomass\\] Xc_biomass;", code_with_trend))

  # Centering loops for all three families
  expect_true(stan_pattern("for \\(i in 2:K_count\\)", code_with_trend))
  expect_true(stan_pattern("means_X_count\\[i - 1\\] = mean\\(X_count\\[ : , i\\]\\);", code_with_trend))
  expect_true(stan_pattern("for \\(i in 2:K_presence\\)", code_with_trend))
  expect_true(stan_pattern("for \\(i in 2:K_biomass\\)", code_with_trend))

  # Observation model parameters for all three families
  expect_true(stan_pattern("vector\\[Kc_count\\] b_count;", code_with_trend))
  expect_true(stan_pattern("real Intercept_count;", code_with_trend))
  expect_true(stan_pattern("vector\\[Kc_presence\\] b_presence;", code_with_trend))
  expect_true(stan_pattern("real Intercept_presence;", code_with_trend))
  expect_true(stan_pattern("vector\\[Kc_biomass\\] b_biomass;", code_with_trend))
  expect_true(stan_pattern("real Intercept_biomass;", code_with_trend))
  expect_true(stan_pattern("real<lower=0> shape_biomass;", code_with_trend))

  # Factor AR(1) trend parameters
  expect_true(stan_pattern("vector<lower=-1,upper=1>\\[N_lv_trend\\] ar1_trend;", code_with_trend))
  expect_true(stan_pattern("vector<lower=0>\\[N_lv_trend\\] sigma_trend;", code_with_trend))
  expect_true(stan_pattern("cholesky_factor_corr\\[N_lv_trend\\] L_Omega_trend;", code_with_trend))

  # Factor loading parameters (estimated for n_lv = 2)
  expect_true(stan_pattern("vector\\[N_series_trend \\* N_lv_trend\\] Z_raw;", code_with_trend))

  # Innovation matrix
  expect_true(stan_pattern("matrix\\[N_trend, N_lv_trend\\] innovations_trend;", code_with_trend))

  # lprior initialization with family-specific priors
  expect_true(stan_pattern("real lprior = 0;", code_with_trend))

  # Factor loading matrix construction with identifiability constraints
  expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z = rep_matrix\\(0, N_series_trend, N_lv_trend\\);", code_with_trend))
  expect_true(stan_pattern("int index = 1;", code_with_trend))
  expect_true(stan_pattern("for \\(j in 1 : N_lv_trend\\)", code_with_trend))
  expect_true(stan_pattern("for \\(i in j : N_series_trend\\)", code_with_trend))
  expect_true(stan_pattern("Z\\[i, j\\] = Z_raw\\[index\\];", code_with_trend))

  # Innovation covariance construction
  expect_true(stan_pattern("matrix\\[N_lv_trend, N_lv_trend\\] L_Sigma_trend = diag_pre_multiply\\(sigma_trend, L_Omega_trend\\);", code_with_trend))
  expect_true(stan_pattern("scaled_innovations_trend = innovations_trend \\* L_Sigma_trend';", code_with_trend))

  # AR(1) latent variable dynamics
  expect_true(stan_pattern("matrix\\[N_trend, N_lv_trend\\] lv_trend;", code_with_trend))
  expect_true(stan_pattern("lv_trend\\[i,:\\] = scaled_innovations_trend\\[i,:\\];", code_with_trend))
  expect_true(stan_pattern("for \\(i in 2:N_trend\\)", code_with_trend))
  expect_true(stan_pattern("for \\(j in 1:N_lv_trend\\)", code_with_trend))
  expect_true(stan_pattern("lv_trend\\[i,j\\] = ar1_trend\\[j\\] \\* lv_trend\\[i-1,j\\] \\+ scaled_innovations_trend\\[i,j\\];", code_with_trend))

  # Zero trend mean vector (for ~ -1 specification)
  expect_true(stan_pattern("vector\\[N_trend\\] mu_trend = rep_vector\\(0\\.0, N_trend\\);", code_with_trend))

  # Universal trend computation pattern
  expect_true(stan_pattern("matrix\\[N_trend, N_series_trend\\] trend;", code_with_trend))
  expect_true(stan_pattern("trend\\[i, s\\] = dot_product\\(Z\\[s, :\\], lv_trend\\[i, :\\]\\) \\+ mu_trend\\[times_trend\\[i, s\\]\\];", code_with_trend))

  # Family-specific linear predictors
  expect_true(stan_pattern("vector\\[N_count\\] mu_count = Xc_count \\* b_count;", code_with_trend))
  expect_true(stan_pattern("vector\\[N_presence\\] mu_presence = Xc_presence \\* b_presence;", code_with_trend))
  expect_true(stan_pattern("vector\\[N_biomass\\] mu_biomass = rep_vector\\(0\\.0, N_biomass\\);", code_with_trend))
  expect_true(stan_pattern("mu_biomass \\+= Intercept_biomass \\+ Xc_biomass \\* b_biomass;", code_with_trend))

  # Trend injection for all three families
  expect_true(stan_pattern("for \\(n in 1:N_count\\)", code_with_trend))
  expect_true(stan_pattern("mu_count\\[n\\] \\+= Intercept_count \\+ trend\\[obs_trend_time_count\\[n\\], obs_trend_series_count\\[n\\]\\];", code_with_trend))
  expect_true(stan_pattern("for \\(n in 1:N_presence\\)", code_with_trend))
  expect_true(stan_pattern("mu_presence\\[n\\] \\+= Intercept_presence \\+ trend\\[obs_trend_time_presence\\[n\\], obs_trend_series_presence\\[n\\]\\];", code_with_trend))
  expect_true(stan_pattern("for \\(n in 1:N_biomass\\)", code_with_trend))
  expect_true(stan_pattern("mu_biomass\\[n\\] \\+= trend\\[obs_trend_time_biomass\\[n\\], obs_trend_series_biomass\\[n\\]\\];", code_with_trend))

  # Gamma inverse link transformation
  expect_true(stan_pattern("mu_biomass = inv\\(mu_biomass\\);", code_with_trend))

  # Three-family likelihoods with GLM optimization for discrete families
  expect_true(stan_pattern("target \\+= poisson_log_glm_lpmf\\(Y_count \\| to_matrix\\(mu_count\\), 0\\.0, mu_ones_count\\);", code_with_trend))
  expect_true(stan_pattern("target \\+= bernoulli_logit_glm_lpmf\\(Y_presence \\| to_matrix\\(mu_presence\\), 0\\.0, mu_ones_presence\\);", code_with_trend))
  expect_true(stan_pattern("target \\+= gamma_lpdf\\(Y_biomass \\| shape_biomass, shape_biomass \\./", code_with_trend))

  # Prior accumulation
  expect_true(stan_pattern("target \\+= lprior;", code_with_trend))

  # Trend parameter priors (existence, not specific distributions)
  expect_true(stan_pattern("ar1_trend ~", code_with_trend))
  expect_true(stan_pattern("sigma_trend ~", code_with_trend))
  expect_true(stan_pattern("L_Omega_trend ~", code_with_trend))
  expect_true(stan_pattern("Z_raw ~", code_with_trend))
  expect_true(stan_pattern("to_vector\\(innovations_trend\\) ~", code_with_trend))

  # Generated quantities for all three families
  expect_true(stan_pattern("real b_count_Intercept = Intercept_count - dot_product\\(means_X_count, b_count\\);", code_with_trend))
  expect_true(stan_pattern("real b_presence_Intercept = Intercept_presence - dot_product\\(means_X_presence, b_presence\\);", code_with_trend))
  expect_true(stan_pattern("real b_biomass_Intercept = Intercept_biomass - dot_product\\(means_X_biomass, b_biomass\\);", code_with_trend))

  # Anti-patterns: Should NOT have trend intercept parameters (~ -1 specification)
  expect_false(grepl("real Intercept_trend;", code_with_trend))
  expect_false(grepl("vector.*b_trend;", code_with_trend))

  # Should NOT have design matrices for trend formula (~ -1 has no covariates)
  expect_false(grepl("matrix.*X_trend;", code_with_trend))
  expect_false(grepl("matrix.*Xc_trend;", code_with_trend))

  # Should NOT have simple AR structure (this is factor model)
  expect_false(grepl("matrix.*scaled_innovations_trend.*diag_matrix", code_with_trend))

  # Should NOT have correlation parameter without structure (uses Cholesky factor)
  expect_false(grepl("corr_matrix.*Omega_trend", code_with_trend))

  # Should NOT have mu_ones for biomass (Gamma doesn't use GLM optimization)
  expect_false(grepl("mu_ones_biomass", code_with_trend))

  # Should NOT have vector ar1 coefficients without bounds
  expect_false(grepl("vector\\[N_lv_trend\\] ar1_trend;", code_with_trend))

  # Check for no duplicated Stan blocks
  expect_equal(length(gregexpr("^\\s*functions\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*data\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*transformed data\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*parameters\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*transformed parameters\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*model\\s*\\{", code_with_trend)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*generated quantities\\s*\\{", code_with_trend)[[1]]), 1)

  # Final validation: ensure model compiles correctly
  expect_no_error(stancode(mf_with_trend, data = data, validate = TRUE))
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
  expect_true(stan_pattern("vector\\[N\\] Y;", code_with_trend))
  expect_false(grepl("array\\[N\\] int Y;", code_with_trend))

  # Observation model: Only intercept (no covariates)
  expect_false(grepl("int.*K;", code_with_trend))  # No K for observation model
  expect_false(grepl("matrix\\[N,.*X;", code_with_trend))  # No X design matrix for obs
  expect_false(grepl("vector\\[.*\\] b;", code_with_trend))  # No b coefficients for obs

  # Observation model dispersion parameter
  expect_true(stan_pattern("real<lower=0> sigma;", code_with_trend))

  # Trend design matrix and coefficients
  expect_true(stan_pattern("int<lower=1> K_trend;", code_with_trend))
  expect_false(stan_pattern("int<lower=1> Kc_trend;", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_trend, K_trend\\] X_trend;", code_with_trend))
  expect_false(stan_pattern("matrix\\[N_trend, Kc_trend\\] Xc_trend;", code_with_trend))
  expect_true(stan_pattern("vector\\[K_trend\\] b_trend;", code_with_trend))
  expect_false(stan_pattern("vector\\[Kc_trend\\] means_X_trend;", code_with_trend))

  # Factor model parameters
  expect_true(stan_pattern("vector<lower=0>\\[N_lv_trend\\] sigma_trend;", code_with_trend))
  expect_true(stan_pattern("vector\\[N_series_trend \\* N_lv_trend\\] Z_raw;", code_with_trend))

  # Factor loading constraints in transformed parameters
  expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z = rep_matrix\\(0, N_series_trend, N_lv_trend\\);", code_with_trend))
  expect_true(stan_pattern("for \\(j in 1 : N_lv_trend\\)", code_with_trend))
  expect_true(stan_pattern("for \\(i in j : N_series_trend\\)", code_with_trend))
  expect_true(stan_pattern("Z\\[i, j\\] = Z_raw\\[index\\];", code_with_trend))

  # ZMVN dynamics (just scaled innovations, no complex dynamics)
  expect_true(stan_pattern("lv_trend = scaled_innovations_trend;", code_with_trend))

  # Trend mean with covariate effects
  expect_true(stan_pattern("mu_trend \\+= X_trend \\* b_trend;", code_with_trend))

  # Universal trend computation pattern should still be present
  expect_true(stan_pattern("trend\\[i, s\\] = dot_product\\(Z\\[s, :\\], lv_trend\\[i, :\\]\\) \\+ mu_trend\\[times_trend\\[i, s\\]\\]", code_with_trend))

  # Observation model priors (brms pattern)
  expect_true(stan_pattern("lprior \\+= student_t_lpdf\\(Intercept \\| 3, 0\\.9, 2\\.5\\);", code_with_trend))
  expect_true(stan_pattern("lprior \\+= student_t_lpdf\\(sigma \\| 3, 0, 2\\.5\\)", code_with_trend))
  expect_true(stan_pattern("- 1 \\* student_t_lccdf\\(0 \\| 3, 0, 2\\.5\\);", code_with_trend))

  # Trend parameter priors
  expect_true(stan_pattern("sigma_trend ~ exponential\\(2\\);", code_with_trend))
  expect_true(stan_pattern("Z_raw ~ student_t\\(3, 0, 1\\);", code_with_trend))

  # No prior for b_trend (brms default flat prior)
  expect_false(grepl("b_trend ~", code_with_trend))

  # Lognormal likelihood (not GLM optimized)
  expect_true(stan_pattern("target \\+= lognormal_lpdf\\(Y \\| mu, sigma\\);", code_with_trend))
  expect_false(grepl("lognormal.*glm", code_with_trend))
  expect_false(grepl("mu_ones", code_with_trend))

  # Observation model likelihood structure (brms pattern)
  expect_true(stan_pattern("vector\\[N\\] mu = rep_vector\\(0\\.0, N\\);", code_with_trend))
  expect_true(stan_pattern("mu \\+= Intercept;", code_with_trend))
  expect_true(stan_pattern("mu\\[n\\] \\+= trend\\[obs_trend_time\\[n\\], obs_trend_series\\[n\\]\\];", code_with_trend))

  # Generated quantities
  expect_true(stan_pattern("real b_Intercept = Intercept;", code_with_trend))

  # Mapping arrays should still be present
  expect_true(stan_pattern("array\\[N\\] int obs_trend_time", code_with_trend))
  expect_true(stan_pattern("array\\[N\\] int obs_trend_series", code_with_trend))

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

test_that("stancode generates correct hierarchical ZMVN(gr = habitat) model with correlated RE and custom prior", {
  data <- setup_stan_test_data()$multivariate
  mf_with_trend <- mvgam_formula(
    biomass ~ 1,
    trend_formula = ~ x + (x | habitat) + ZMVN(gr = habitat)
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
  expect_true(stan_pattern("vector\\[N\\] Y;", code_with_trend))
  expect_false(grepl("array\\[N\\] int Y;", code_with_trend))

  # Observation model: Only intercept (no covariates)
  expect_false(grepl("int.*K;", code_with_trend))  # No K for observation model
  expect_false(grepl("matrix\\[N,.*X;", code_with_trend))  # No X design matrix for obs
  expect_false(grepl("vector\\[.*\\] b;", code_with_trend))  # No b coefficients for obs

  # Observation model dispersion parameter
  expect_true(stan_pattern("real<lower=0> sigma;", code_with_trend))

  # Trend design matrix and coefficients (uses direct K_trend, not centering)
  expect_true(stan_pattern("int<lower=1> K_trend;", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_trend, K_trend\\] X_trend;", code_with_trend))
  expect_true(stan_pattern("vector\\[K_trend\\] b_trend;", code_with_trend))

  # Correlated random effects structure for (x | habitat) in trend
  expect_true(stan_pattern("int<lower=1> N_1_trend;", code_with_trend))  # Number of grouping factor levels
  expect_true(stan_pattern("vector<lower=0>\\[M_1_trend\\] sd_1_trend;", code_with_trend))  # Random effects SDs
  expect_true(stan_pattern("matrix\\[M_1_trend, N_1_trend\\] z_1_trend;", code_with_trend))  # Z-scores matrix
  expect_true(stan_pattern("cholesky_factor_corr\\[M_1_trend\\] L_1_trend;", code_with_trend))  # Correlation matrix

  # Hierarchical grouping data structures
  expect_true(stan_pattern("int<lower=1> N_groups_trend;", code_with_trend))
  expect_true(stan_pattern("int<lower=1> N_subgroups_trend;", code_with_trend))
  expect_true(stan_pattern("array\\[N_series_trend\\] int<lower=1> group_inds_trend;", code_with_trend))

  # Hierarchical correlation functions
  expect_true(stan_pattern("matrix combine_cholesky\\(", code_with_trend))
  expect_true(stan_pattern("real alpha", code_with_trend))

  # Hierarchical correlation parameters with _trend suffix
  expect_true(stan_pattern("cholesky_factor_corr\\[N_subgroups_trend\\] L_Omega_global_trend;", code_with_trend))
  expect_true(stan_pattern("array\\[N_groups_trend\\] cholesky_factor_corr\\[N_subgroups_trend\\] L_deviation_group_trend;", code_with_trend))
  expect_true(stan_pattern("real<lower=0, upper=1> alpha_cor_trend;", code_with_trend))

  # Group-specific correlation matrices computation
  expect_true(stan_pattern("array\\[N_groups_trend\\] cov_matrix\\[N_subgroups_trend\\] Sigma_group_trend;", code_with_trend))
  expect_true(stan_pattern("L_Omega_group_trend\\[g_idx\\] = combine_cholesky\\(L_Omega_global_trend, L_deviation_group_trend\\[g_idx\\], alpha_cor_trend\\);", code_with_trend))

  # Group-specific sigma parameters (array of vectors for each group)
  expect_true(stan_pattern("array\\[N_groups_trend\\] vector<lower=0>\\[N_subgroups_trend\\] sigma_group_trend;", code_with_trend))

  # Group-specific Cholesky and covariance computation
  expect_true(stan_pattern("L_group_trend\\[g_idx\\] = diag_pre_multiply\\(sigma_group_trend\\[g_idx\\], L_Omega_group_trend\\[g_idx\\]\\);", code_with_trend))
  expect_true(stan_pattern("Sigma_group_trend\\[g_idx\\] = multiply_lower_tri_self_transpose\\(L_group_trend\\[g_idx\\]\\);", code_with_trend))

  # ZMVN dynamics (just scaled innovations)
  expect_true(stan_pattern("lv_trend = scaled_innovations_trend;", code_with_trend))

  # Factor loading matrix (diagonal for non-factor ZMVN)
  expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z = diag_matrix\\(rep_vector\\(1\\.0, N_lv_trend\\)\\);", code_with_trend))

  # Trend mean with fixed effects and random effects computation
  expect_true(stan_pattern("r_1_trend = scale_r_cor\\(z_1_trend, sd_1_trend, L_1_trend\\);", code_with_trend))
  expect_true(stan_pattern("r_1_1_trend = r_1_trend\\[\\s*:\\s*, 1\\];", code_with_trend))  # Random intercepts
  expect_true(stan_pattern("r_1_2_trend = r_1_trend\\[\\s*:\\s*, 2\\];", code_with_trend))  # Random slopes

  # Universal trend computation pattern should still be present
  expect_true(stan_pattern("trend\\[i, s\\] = dot_product\\(Z\\[s, :\\], lv_trend\\[i, :\\]\\) \\+ mu_trend\\[times_trend\\[i, s\\]\\]", code_with_trend))

  # mu_trend must include fixed effects (X_trend * b_trend) when trend_formula
  # has both fixed effects and random effects. This validates the fix for
  # GLM-hidden fixed effects where brms passes Xc*b to normal_id_glm_lpdf.
  expect_true(stan_pattern("mu_trend \\+= X_trend \\* b_trend", code_with_trend))

  # Hierarchical correlation priors with _trend suffix
  expect_true(stan_pattern("L_Omega_global_trend ~ lkj_corr_cholesky\\(1\\);", code_with_trend))
  expect_true(stan_pattern("for \\(g_idx in 1:N_groups_trend\\) \\{ L_deviation_group_trend\\[g_idx\\] ~ lkj_corr_cholesky\\(6\\); \\}", code_with_trend))

  # Custom alpha_cor_trend prior should be applied
  expect_true(stan_pattern("alpha_cor_trend ~ beta\\(5, 5\\);", code_with_trend))

  # Group-specific sigma priors and innovation priors
  expect_true(stan_pattern("to_vector\\(sigma_group_trend\\[g_idx\\]\\) ~ exponential\\(2\\);", code_with_trend))
  expect_true(stan_pattern("to_vector\\(innovations_trend\\) ~ std_normal\\(\\);", code_with_trend))

  # No prior for b_trend (brms default flat prior)
  expect_false(grepl("b_trend ~", code_with_trend))

  # Observation model priors (brms pattern)
  expect_true(stan_pattern("lprior \\+= student_t_lpdf\\(Intercept \\| 3, 0\\.9, 2\\.5\\);", code_with_trend))
  expect_true(stan_pattern("lprior \\+= student_t_lpdf\\(sigma \\| 3, 0, 2\\.5\\)", code_with_trend))
  expect_true(stan_pattern("- 1 \\* student_t_lccdf\\(0 \\| 3, 0, 2\\.5\\);", code_with_trend))

  # Lognormal likelihood (not GLM optimized)
  expect_true(stan_pattern("target \\+= lognormal_lpdf\\(Y \\| mu, sigma\\);", code_with_trend))
  expect_false(grepl("lognormal.*glm", code_with_trend))
  expect_false(grepl("mu_ones", code_with_trend))

  # Observation model likelihood structure (brms pattern)
  expect_true(stan_pattern("vector\\[N\\] mu = rep_vector\\(0\\.0, N\\);", code_with_trend))
  expect_true(stan_pattern("mu \\+= Intercept;", code_with_trend))
  expect_true(stan_pattern("mu\\[n\\] \\+= trend\\[obs_trend_time\\[n\\], obs_trend_series\\[n\\]\\];", code_with_trend))

  # Generated quantities (no trend intercept centering in this model)
  expect_true(stan_pattern("real b_Intercept = Intercept;", code_with_trend))

  # Mapping arrays should still be present
  expect_true(stan_pattern("array\\[N\\] int obs_trend_time", code_with_trend))
  expect_true(stan_pattern("array\\[N\\] int obs_trend_series", code_with_trend))

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

test_that("stancode generates correct hierarchical VAR(gr = habitat) model with proper coefficient matrices", {
  data <- setup_stan_test_data()$multivariate
  mf_with_trend <- mvgam_formula(
    count ~ 1 + x,
    trend_formula = ~ 1 + VAR(p = 1, gr = habitat)
  )

  code_with_trend <- stancode(
    mf_with_trend,
    data = data,
    family = poisson(),
    validate = FALSE
  )

  # Basic structure checks
  expect_s3_class(code_with_trend, "mvgamstancode")
  expect_s3_class(code_with_trend, "stancode")

  # Response variable should be integer array for Poisson
  expect_true(stan_pattern("array\\[N\\] int Y;", code_with_trend))
  expect_false(grepl("vector\\[N\\] Y;", code_with_trend))

  # Hierarchical data structures - VAR should use shared system
  expect_true(stan_pattern("int<lower=1> N_groups_trend;", code_with_trend))
  expect_true(stan_pattern("array\\[N_series_trend\\] int<lower=1> group_inds_trend;", code_with_trend))

  # VAR-specific data structures
  expect_true(stan_pattern("int<lower=1> N_lags_trend;", code_with_trend))

  # Hierarchical correlation parameters (shared with ZMVN)
  expect_true(stan_pattern("cholesky_factor_corr\\[N_subgroups_trend\\] L_Omega_global_trend;", code_with_trend))
  expect_true(stan_pattern("array\\[N_groups_trend\\] cholesky_factor_corr\\[N_subgroups_trend\\] L_deviation_group_trend;", code_with_trend))
  expect_true(stan_pattern("real<lower=0, upper=1> alpha_cor_trend;", code_with_trend))

  # VAR-specific hierarchical coefficient parameters
  expect_true(stan_pattern("array\\[N_groups_trend, 1\\] matrix\\[N_subgroups_trend, N_subgroups_trend\\] A_raw_group_trend;", code_with_trend))

  # Group-specific sigma parameters for VAR
  expect_true(stan_pattern("array\\[N_groups_trend\\] vector<lower=0>\\[N_subgroups_trend\\] sigma_group_trend;", code_with_trend))

  # Hierarchical correlation functions
  expect_true(stan_pattern("matrix combine_cholesky\\(", code_with_trend))

  # Group-specific coefficient matrices in transformed parameters
  expect_true(stan_pattern("array\\[N_groups_trend, 1\\] matrix\\[N_subgroups_trend, N_subgroups_trend\\] A_group_trend;", code_with_trend))
  expect_true(stan_pattern("array\\[N_groups_trend\\] cov_matrix\\[N_subgroups_trend\\] Sigma_group_trend;", code_with_trend))

  # Hierarchical correlation computation with g_idx loop indices
  expect_true(stan_pattern("for \\(g_idx in 1:N_groups_trend\\) \\{", code_with_trend))
  expect_true(stan_pattern("matrix\\[.*\\] L_Omega_group_trend = combine_cholesky\\(L_Omega_global_trend,.*L_deviation_group_trend\\[g_idx\\],.*alpha_cor_trend\\);", code_with_trend))
  expect_true(stan_pattern("Sigma_group_trend\\[g_idx\\] = multiply_lower_tri_self_transpose\\(", code_with_trend))

  # Block-diagonal assembly of full system matrices
  expect_true(stan_pattern("cov_matrix\\[N_lv_trend\\] Sigma_trend = rep_matrix\\(0, N_lv_trend, N_lv_trend\\);", code_with_trend))
  expect_true(stan_pattern("array\\[N_lags_trend\\] matrix\\[N_lv_trend, N_lv_trend\\] A_trend;", code_with_trend))

  # Heaps transformation for stationarity (VAR-specific)
  expect_true(stan_pattern("array\\[1\\] matrix\\[N_subgroups_trend, N_subgroups_trend\\] P_group;", code_with_trend))
  expect_true(stan_pattern("P_group\\[1\\] = AtoP\\(A_raw_group_trend\\[g_idx, lag\\]\\);", code_with_trend))

  # Default prior application (VAR should use default beta(3, 2))
  expect_true(stan_pattern("alpha_cor_trend ~ beta\\(3, 2\\);", code_with_trend))

  # VAR coefficient and sigma priors (uses g_idx consistently)
  expect_true(stan_pattern("diagonal\\(A_raw_group_trend\\[g_idx, lag\\]\\) ~ normal\\(", code_with_trend))
  expect_true(stan_pattern("to_vector\\(sigma_group_trend\\[g_idx\\]\\) ~ exponential\\(2\\);", code_with_trend))

  # Innovation correlation priors (shared pattern) with g_idx
  expect_true(stan_pattern("L_Omega_global_trend ~ lkj_corr_cholesky\\(1\\);", code_with_trend))
  expect_true(stan_pattern("for \\(g_idx in 1:N_groups_trend\\) \\{", code_with_trend))
  expect_true(stan_pattern("L_deviation_group_trend\\[g_idx\\] ~ lkj_corr_cholesky\\(6\\);", code_with_trend))

  # Poisson likelihood (GLM optimized for count data)
  expect_true(stan_pattern("target \\+= poisson_log_glm_lpmf\\(Y \\| to_matrix\\(mu\\), 0\\.0, mu_ones\\);", code_with_trend))


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

test_that("stancode generates correct CAR() continuous autoregressive trend with nested RE and monotonic effects", {
    # Create test data with irregular time intervals (CAR's specialty)
    # NOTE: Using univariate data (single series) so CAR can include trend covariates
    set.seed(42)
    n_time <- 20

    data <- data.frame(
      time = cumsum(c(1, rexp(n_time - 1, rate = 0.8))),  # Irregular intervals
      series = factor(rep("series1", n_time)),  # Single series for univariate CAR
      y = rpois(n_time, lambda = 3),
      income = ordered(sample(1:5, n_time, replace = TRUE)),  # Ordered factor for monotonic
      site = factor(rep(c("A", "B", "C"), length.out = n_time)),  # For nested RE
      plot = factor(paste0(rep(c("A", "B", "C"), length.out = n_time), "_", rep(1:2, length.out = n_time)))  # Nested within site
    )

    mf_with_trend <- mvgam_formula(
      y ~ (1 | site) + (1 | plot),
      trend_formula = ~ mo(income) + CAR()
    )
    code_with_trend <- stancode(
      mf_with_trend, data = data,
      family = poisson(),
      validate = FALSE
    )

    # Basic structure checks
    expect_s3_class(code_with_trend, "mvgamstancode")
    expect_s3_class(code_with_trend, "stancode")

    # 1. Functions Block - Should contain monotonic effects function
    expect_true(stan_pattern("real mo\\(vector scale, int i\\)", code_with_trend))
    expect_true(stan_pattern("compute monotonic effects", code_with_trend))

    # 2. Data Block - Monotonic effects and CAR trend data
    # Monotonic effects data structures
    expect_true(stan_pattern("array\\[N_trend\\] int Xmo_1_trend", code_with_trend))
    expect_true(stan_pattern("array\\[Imo_trend\\] int<lower=1> Jmo_trend", code_with_trend))
    expect_true(stan_pattern("vector\\[Jmo_trend\\[1\\]\\] con_simo_1_trend", code_with_trend))

    # Nested random effects data structures for (1 | site) + (1 | plot)
    expect_true(stan_pattern("int<lower=1> N_1;", code_with_trend))  # Number of site levels
    expect_true(stan_pattern("int<lower=1> M_1;", code_with_trend))  # Number of site RE parameters
    expect_true(stan_pattern("int<lower=1> N_2;", code_with_trend))  # Number of plot levels
    expect_true(stan_pattern("int<lower=1> M_2;", code_with_trend))  # Number of plot RE parameters
    expect_true(stan_pattern("array\\[N\\] int<lower=1> J_1;", code_with_trend))  # Site indices
    expect_true(stan_pattern("array\\[N\\] int<lower=1> J_2;", code_with_trend))  # Plot indices
    expect_true(stan_pattern("vector\\[N\\] Z_1_1;", code_with_trend))  # Site design vector
    expect_true(stan_pattern("vector\\[N\\] Z_2_1;", code_with_trend))  # Plot design vector

    # CAR trend dimensions
    expect_true(stan_pattern("int<lower=1> N_trend;", code_with_trend))
    expect_true(stan_pattern("int<lower=1> N_series_trend;",
                      code_with_trend))
    expect_true(stan_pattern("int<lower=1> N_lv_trend;", code_with_trend))

    # CAR-specific time distance array for irregular intervals
    expect_true(stan_pattern("array\\[N_trend, N_series_trend\\] real<lower=0> time_dis", code_with_trend))

    # Standard trend mapping arrays
    expect_true(stan_pattern("array\\[N_trend, N_series_trend\\] int times_trend;", code_with_trend))
    expect_true(stan_pattern("array\\[N\\] int obs_trend_time;", code_with_trend))
    expect_true(stan_pattern("array\\[N\\] int obs_trend_series;", code_with_trend))

    # GLM optimization should not be present for models with only random effects
    expect_false(stan_pattern("poisson_log_glm_lpmf", code_with_trend))

    # 3. Transformed Data Block - Factor loading matrix
    expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z =
  diag_matrix\\(rep_vector\\(1\\.0, N_lv_trend\\)\\);", code_with_trend))

    # 4. Parameters Block - Complex observation model + CAR trend parameters
    # Monotonic effects parameters
    expect_true(stan_pattern("simplex\\[Jmo_trend\\[1\\]\\] simo_1_trend;", code_with_trend))
    expect_true(stan_pattern("vector\\[Ksp_trend\\] bsp_trend;", code_with_trend))

    # Nested random effects parameters for (1 | site) + (1 | plot)
    expect_true(stan_pattern("vector<lower=0>\\[M_1\\] sd_1;", code_with_trend))  # Site SDs
    expect_true(stan_pattern("array\\[M_1\\] vector\\[N_1\\] z_1;", code_with_trend))  # Site z-scores
    expect_true(stan_pattern("vector<lower=0>\\[M_2\\] sd_2;", code_with_trend))  # Plot SDs
    expect_true(stan_pattern("array\\[M_2\\] vector\\[N_2\\] z_2;", code_with_trend))  # Plot z-scores

    # CAR trend parameters
    expect_false(stan_pattern("real Intercept_trend;", code_with_trend))
    expect_true(stan_pattern("vector<lower=-1,upper=1>\\[N_lv_trend\\] ar1_trend;", code_with_trend))
    expect_true(stan_pattern("vector<lower=0>\\[N_lv_trend\\] sigma_trend;",
                      code_with_trend))
    expect_true(stan_pattern("matrix\\[N_trend, N_lv_trend\\] innovations_trend;",
                      code_with_trend))

    # 5. Transformed Parameters Block - Complex computations
    # GP and nested random effects computations (from brms)
    expect_true(stan_pattern("vector\\[N_1\\] r_1_1;", code_with_trend))  # Site random effects
    expect_true(stan_pattern("vector\\[N_2\\] r_2_1;", code_with_trend))  # Plot random effects
    expect_true(stan_pattern("r_1_1 = \\(sd_1\\[1\\] \\* \\(z_1\\[1\\]\\)\\);", code_with_trend))  # Site computation
    expect_true(stan_pattern("r_2_1 = \\(sd_2\\[1\\] \\* \\(z_2\\[1\\]\\)\\);", code_with_trend))  # Plot computation

    # Prior accumulation - Model block priors
    expect_true(stan_pattern("real lprior = 0;", code_with_trend, fixed = TRUE))
    expect_true(stan_pattern("lprior \\+= student_t_lpdf\\(Intercept \\|", code_with_trend))
    expect_true(stan_pattern("lprior \\+= dirichlet_lpdf\\(simo_1_trend \\|", code_with_trend))  # Monotonic prior
    expect_true(stan_pattern("lprior \\+= student_t_lpdf\\(sd_1 \\|", code_with_trend))  # Site SD priors
    expect_true(stan_pattern("lprior \\+= student_t_lpdf\\(sd_2 \\|", code_with_trend))  # Plot SD priors

    # CAR-specific trend computation
    expect_true(stan_pattern("matrix\\[N_trend, N_lv_trend\\] scaled_innovations_trend;",
                      code_with_trend))
    expect_true(stan_pattern("scaled_innovations_trend = innovations_trend \\*
  diag_matrix\\(sigma_trend\\);", code_with_trend))

    # CAR latent variable evolution
    expect_true(stan_pattern("matrix\\[N_trend, N_lv_trend\\] lv_trend;", code_with_trend))

    # CAR initialization (first time point)
    expect_true(stan_pattern("for \\(j in 1:N_lv_trend\\)", code_with_trend))
    expect_true(stan_pattern("lv_trend\\[1, j\\] = scaled_innovations_trend\\[1, j\\];",
                      code_with_trend))

    # CAR continuous-time evolution (key differentiator)
    expect_true(stan_pattern("for \\(j in 1:N_lv_trend\\)", code_with_trend))
    expect_true(stan_pattern("for \\(i in 2:N_trend\\)", code_with_trend))
    expect_true(stan_pattern("lv_trend\\[i, j\\] = pow\\(ar1_trend\\[j\\], time_dis\\[i, j\\]\\) \\*
  lv_trend\\[i - 1, j\\]", code_with_trend))
    expect_true(stan_pattern("\\+ scaled_innovations_trend\\[i, j\\];", code_with_trend))

    # Universal trend computation pattern
    expect_true(stan_pattern("matrix\\[N_trend, N_series_trend\\] trend;", code_with_trend))
    expect_true(stan_pattern("vector\\[N_trend\\] mu_trend = rep_vector\\(0.0,
  N_trend\\);", code_with_trend))
    expect_true(stan_pattern("for \\(i in 1:N_trend\\)", code_with_trend))
    expect_true(stan_pattern("for \\(s in 1:N_series_trend\\)", code_with_trend))
    expect_true(stan_pattern("trend\\[i, s\\] = dot_product\\(Z\\[s, :\\], lv_trend\\[i, :\\]\\) \\+
  mu_trend\\[times_trend\\[i, s\\]\\];", code_with_trend))

    # 6. Model Block - Monotonic effects in trend computation
    # Monotonic effects computation
    expect_true(stan_pattern("mu_trend\\[n\\] \\+= \\(bsp_trend\\[1\\]\\) \\* mo\\(simo_1_trend, Xmo_1_trend\\[n\\]\\)", code_with_trend))

    # Complex linear predictor construction with nested random effects
    expect_true(stan_pattern("vector\\[N\\] mu = rep_vector\\(0\\.0, N\\);", code_with_trend))
    expect_true(stan_pattern("mu \\+= Intercept;", code_with_trend))

    # Multi-component trend injection (random effects and trend)
    expect_true(stan_pattern("for \\(n in 1:N\\)", code_with_trend))
    expect_true(stan_pattern("mu\\[n\\] \\+= r_1_1\\[J_1\\[n\\]\\] \\* Z_1_1\\[n\\] \\+ r_2_1\\[J_2\\[n\\]\\] \\* Z_2_1\\[n\\]", code_with_trend))
    expect_true(stan_pattern("mu\\[n\\] \\+= trend\\[obs_trend_time\\[n\\], obs_trend_series\\[n\\]\\]", code_with_trend))


    # Prior contributions
    expect_true(stan_pattern("target \\+= lprior;", code_with_trend))
    expect_true(stan_pattern("target \\+= std_normal_lpdf\\(z_1\\[1\\]\\);", code_with_trend))

    # CAR trend priors (check existence, not specific distributions)
    expect_true(stan_pattern("ar1_trend ~", code_with_trend))
    expect_true(stan_pattern("sigma_trend ~", code_with_trend))
    expect_true(stan_pattern("to_vector\\(innovations_trend\\) ~", code_with_trend))

    # 7. Generated Quantities Block
    expect_true(stan_pattern("real b_Intercept = Intercept;", code_with_trend))

    # 8. CAR-Specific Anti-patterns - Things that should NOT be present
    # Should NOT have discrete AR initialization patterns
    expect_false(grepl("lv_trend\\[i, :\\] = lv_trend\\[i-1, :\\] \\+", code_with_trend))

    # Should NOT have simple AR coefficient without bounds
    expect_false(grepl("vector\\[N_lv_trend\\] ar1_trend;", code_with_trend))

    # Should NOT have factor model parameters (CAR doesn't support factors)
    expect_false(grepl("Z_raw", code_with_trend, fixed = TRUE))
    expect_false(grepl("matrix\\[N_series_trend, N_lv_trend\\] Z;", code_with_trend))

    # Should NOT have correlation parameters (CAR doesn't support correlated trends)
    expect_false(grepl("L_Omega_trend", code_with_trend, fixed = TRUE))
    expect_false(grepl("Sigma_trend", code_with_trend, fixed = TRUE))

    # Should NOT have changepoint parameters (that's PW, not CAR)
    expect_false(grepl("delta_trend", code_with_trend, fixed = TRUE))
    expect_false(grepl("k_trend", code_with_trend, fixed = TRUE))
    expect_false(grepl("m_trend", code_with_trend, fixed = TRUE))

    # Should NOT have MA parameters (CAR is pure AR)
    expect_false(grepl("theta[0-9]+_trend", code_with_trend))
    expect_false(grepl("ma_innovations", code_with_trend, fixed = TRUE))

    # Should NOT have VAR parameters
    expect_false(grepl("A[0-9]+_trend", code_with_trend))

    # Monotonic effects structure (mo(income))
    # Monotonic function in functions block
    expect_true(stan_pattern("real mo\\(vector scale, int i\\)", code_with_trend))
    expect_true(stan_pattern("compute monotonic effects", code_with_trend))

    # Monotonic data arrays
    expect_true(stan_pattern("array\\[N_trend\\] int Xmo_1_trend;", code_with_trend))

    # Monotonic parameters (simplex for ordered constraint)
    expect_true(stan_pattern("simplex\\[Jmo_trend\\[1\\]\\] simo_1_trend;", code_with_trend))

    # Monotonic prior (Dirichlet)
    expect_true(stan_pattern("dirichlet_lpdf\\(simo_1_trend \\| con_simo_1_trend\\);", code_with_trend))

    # Monotonic effect usage in trend construction
    expect_true(stan_pattern("mo\\(simo_1_trend, Xmo_1_trend\\[n\\]\\)", code_with_trend))

    # Check for no duplicated Stan blocks
    expect_equal(length(gregexpr("^\\s*functions\\s*\\{", code_with_trend)[[1]]), 1)
    expect_equal(length(gregexpr("^\\s*data\\s*\\{", code_with_trend)[[1]]), 1)
    expect_equal(length(gregexpr("^\\s*transformed data\\s*\\{", code_with_trend)[[1]]), 1)
    expect_equal(length(gregexpr("^\\s*parameters\\s*\\{", code_with_trend)[[1]]), 1)
    expect_equal(length(gregexpr("^\\s*transformed parameters\\s*\\{", code_with_trend)[[1]]),
                 1)
    expect_equal(length(gregexpr("^\\s*model\\s*\\{", code_with_trend)[[1]]), 1)
    expect_equal(length(gregexpr("^\\s*generated quantities\\s*\\{", code_with_trend)[[1]]), 1)

    # Final validation: ensure model compiles correctly
    expect_no_error(stancode(mf_with_trend, data = data, family = poisson(), validate = TRUE))
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
  expect_true(stan_pattern("for\\(iin1:N_trend\\)", code))
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
  expect_true(any(grepl("mu\\[n\\]\\s*\\+=\\s*trend\\[obs_trend_time\\[n\\],\\s*obs_trend_series\\[n\\]\\]", model_lines)))
  expect_true(any(grepl("mu\\[n\\]\\s*\\+=\\s*trend\\[obs_trend_time\\[n\\],\\s*obs_trend_series\\[n\\]\\]", model_lines)))

  # Generated Stan code should compile without errors
  expect_no_error(stancode(mf, data = data, family = poisson(), validate = TRUE))
})

test_that("stancode handles smooth terms in trend_formula with correct declaration order", {
  # Verifies fix for declaration ordering bug where knots_1_trend was used

  # before declaration, causing: "Identifier 'knots_1_trend' not in scope"
  data <- setup_stan_test_data()$univariate
  mf <- mvgam_formula(y ~ 1, trend_formula = ~ s(x, k = 5) + AR(p = 1))

  # Should compile without "Identifier not in scope" errors
  code <- stancode(mf, data = data, family = poisson(), validate = TRUE)

  # Verify smooth-related declarations are present
  expect_true(stan_pattern("int nb_1_trend", code))
  expect_true(stan_pattern("array\\[nb_1_trend\\] int knots_1_trend", code))
  expect_true(stan_pattern("Zs_1_1_trend", code))

  # Verify declaration order: knots must appear before Zs (which uses it)
  code_lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  knots_line <- which(grepl("knots_1_trend", code_lines))[1]
  zs_line <- which(grepl("Zs_1_1_trend", code_lines))[1]
  expect_true(knots_line < zs_line)
})

test_that("stancode handles multivariate specifications with shared RW trend and offset", {
  data <- setup_stan_test_data()$multivariate

  # Add offset variables to multivariate data
  data$log_baseline_count <- log(runif(nrow(data), min = 2, max = 5))
  data$log_baseline_biomass <- log(runif(nrow(data), min = 1, max = 3))

  # Multivariate with shared trend and offsets (explicitly set rescor = FALSE
  # to avoid brms deprecation warnings)
  mf_shared <- mvgam_formula(
    bf(mvbind(count, biomass) ~ x + offset(log_baseline_count) + offset(log_baseline_biomass)) + set_rescor(FALSE),
    trend_formula = ~ RW(cor = TRUE)
  )

  # Generate without validation first for structure inspection
  code_shared <- stancode(mf_shared, data = data, validate = FALSE)

  expect_s3_class(code_shared, "stancode")
  expect_gt(nchar(code_shared), 500)

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

  # brms observation data declarations
  # Should declare N_count with comment
  expect_match2(code_shared, "int<lower=1> N_count;")
  # Should declare Y_count as vector
  expect_match2(code_shared, "vector\\[N_count\\] Y_count;")
  # Should declare X_count design matrix
  expect_match2(code_shared, "matrix\\[N_count, K_count\\] X_count;")
  # Should declare N_biomass with comment
  expect_match2(code_shared, "int<lower=1> N_biomass;")
  # Should declare Y_biomass as vector
  expect_match2(code_shared, "vector\\[N_biomass\\] Y_biomass;")

  # Trend dimensions
  # Should declare N_trend
  expect_match2(code_shared, "int<lower=1> N_trend;")
  # Should declare N_series_trend
  expect_match2(code_shared, "int<lower=1> N_series_trend;")
  # Should declare N_lv_trend
  expect_match2(code_shared, "int<lower=1> N_lv_trend;")

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
  expect_match2(code_shared, "array\\[N_trend, N_series_trend\\] int times_trend;")

  # Offset data structures for each response (brms consolidates them)
  expect_match2(code_shared, "vector\\[N_count\\] offsets_count;")  # Count offsets
  expect_match2(code_shared, "vector\\[N_biomass\\] offsets_biomass;")  # Biomass offsets

  # GLM compatibility vectors
  # Should declare mu_ones_count for GLM
  expect_match2(code_shared, "vector\\[1\\] mu_ones_count;")
  # Should declare mu_ones_biomass for GLM
  expect_match2(code_shared, "vector\\[1\\] mu_ones_biomass;")

  # Should create identity matrix Z for non-factor model
  expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z = diag_matrix\\(rep_vector\\(1.0, N_lv_trend\\)\\);", code_shared))

  # Observation model parameters
  # Should declare b_count coefficients
  expect_match2(code_shared, "vector\\[Kc_count\\] b_count;")
  # Should declare Intercept_count
  expect_match2(code_shared, "real Intercept_count;")
  # Should declare sigma_count with lower bound
  expect_match2(code_shared, "real<lower=0> sigma_count;")

  # Trend parameters with _trend suffix
  # Should declare sigma_trend vector
  expect_match2(code_shared, "vector<lower=0>\\[N_lv_trend\\] sigma_trend;")
  # Should declare L_Omega_trend for correlation
  expect_match2(code_shared, "cholesky_factor_corr\\[N_lv_trend\\] L_Omega_trend;")
  # Should declare innovations_trend matrix
  expect_match2(code_shared, "matrix\\[N_trend, N_lv_trend\\] innovations_trend;")

  # Should initialize lprior
  expect_match2(code_shared, "real lprior = 0;")

  # Should create mu_trend from Intercept_trend using rep_vector
  expect_true(stan_pattern("vector\\[N_trend\\] mu_trend = rep_vector\\(0.0, N_trend\\);", code_shared))

  # Should construct Sigma_trend covariance matrix
  expect_true(stan_pattern("matrix\\[N_lv_trend, N_lv_trend\\] Sigma_trend = diag_pre_multiply\\(sigma_trend, L_Omega_trend\\);", code_shared))

  # RW latent variables
  # Should declare lv_trend matrix for latent variables (with _trend suffix)
  expect_match2(code_shared, "matrix\\[N_trend, N_lv_trend\\] lv_trend;")
  # Should declare L_Sigma_trend for scaling
  expect_true(stan_pattern("matrix\\[N_lv_trend, N_lv_trend\\] L_Sigma_trend =", code_shared))
  # Should declare scaled_innovations_trend
  expect_true(stan_pattern("matrix\\[N_trend, N_lv_trend\\] scaled_innovations_trend", code_shared))
  # Should initialize first lv_trend from scaled innovations
  expect_true(stan_pattern("lv_trend\\[1,\\s*:\\s*\\] = scaled_innovations_trend\\[1,\\s*:\\s*\\]", code_shared))
  # Should implement RW cumulative sum
  expect_true(stan_pattern("lv_trend\\[i,\\s*:\\s*\\] = lv_trend\\[i - 1,\\s*:\\s*\\].*\\+.*scaled_innovations_trend\\[i,\\s*:\\s*\\];", code_shared))

  # Trend matrix computation (shared, not response-specific)
  # Should declare shared trend matrix (not trend_count/trend_biomass)
  expect_match2(code_shared, "matrix\\[N_trend, N_series_trend\\] trend;")

  # Linear predictor construction with trend injection
  # Should initialize mu vectors
  expect_true(stan_pattern("vector\\[N_count\\] mu_count = rep_vector\\(0\\.0, N_count\\);", code_shared))
  expect_true(stan_pattern("vector\\[N_biomass\\] mu_biomass = rep_vector\\(0\\.0, N_biomass\\);", code_shared))
  # Should inject trend into mu using for loops
  expect_true(stan_pattern("mu_count\\[i\\] \\+= trend\\[obs_trend_time_count\\[i\\], obs_trend_series_count\\[i\\]\\];", code_shared))
  expect_true(stan_pattern("mu_biomass\\[i\\] \\+= trend\\[obs_trend_time_biomass\\[i\\], obs_trend_series_biomass\\[i\\]\\];", code_shared))

  # Offset handling in linear predictor construction (brms consolidates offsets)
  expect_true(stan_pattern("mu_count \\+= Intercept_count \\+ offsets_count;", code_shared))  # Count offset injection
  expect_true(stan_pattern("mu_biomass \\+= Intercept_biomass \\+ offsets_biomass;", code_shared))  # Biomass offset injection

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

  # Should NOT have response-specific trend_count matrix
  expect_false(grepl("matrix.*trend_count", code_shared))
  # Should NOT have response-specific trend_biomass matrix
  expect_false(grepl("matrix.*trend_biomass", code_shared))

  # Should NOT have duplicate model blocks
  expect_false(grepl("model\\s*\\{.*model\\s*\\{", code_shared))

  # Should NOT use response-specific trend_count in injection
  expect_false(grepl("mu_count\\[n\\] \\+= trend_count\\[", code_shared))

  # Should NOT have unsuffixed mu_ones
  expect_false(grepl("vector\\[1\\] mu_ones;", code_shared))

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
    stancode("banana", data = data),
    "invalid formula.*not a call"
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

    # Skip multivariate data for simple formula and missing data (has dedicated test)
    if (data_name %in% c("multivariate", "with_missings")) next

    standata_result <- SW(standata(mf, data = data, family = poisson()))

    expect_type(standata_result, "list")
    expect_true("N" %in% names(standata_result))
    expect_equal(standata_result$N, nrow(data))
  }
})

test_that("standata processes missing data correctly", {
  data <- setup_stan_test_data()$with_missings
  mf <- mvgam_formula(y ~ x, trend_formula = ~ RW())

  standata_result <- SW(standata(mf, data = data, family = poisson()))

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
    var = ~ VAR(p = 1)
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
  standata_result <- standata(mf, data = data, family = poisson())

  # Invalid time series structure should error
  bad_data <- data
  bad_data$time <- c(0, 1, 3, 4:nrow(data))  # Irregular intervals

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
  code <- SW(stancode(mf, data = data, prior = priors))
  standata_result <- SW(standata(mf, data = data, prior = priors))

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
  code <- stancode(mf, data = small_data, family = poisson())
  standata_result <- standata(mf, data = small_data, family = poisson())

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

test_that("stancode generates correct PW(n_changepoints = 10) piecewise trend structure", {
  data <- setup_stan_test_data()$univariate
  # Add cap column for potential logistic growth (though linear is default)
  data$cap <- 16

  mf_with_trend <- mvgam_formula(
    y ~ x,
    trend_formula = ~ PW(n_changepoints = 10)
  )
  code_with_trend <- stancode(
    mf_with_trend, data = data,
    family = poisson(),
    validate = FALSE
  )

  # Basic structure checks
  expect_s3_class(code_with_trend, "mvgamstancode")
  expect_s3_class(code_with_trend, "stancode")

  # 1. Functions Block - Prophet-style piecewise functions
  # Check for changepoint matrix function
  expect_true(stan_pattern("matrix get_changepoint_matrix\\(vector t, vector t_change_trend, int T, int S\\)", code_with_trend))
  expect_true(stan_pattern("Function to sort changepoints", code_with_trend, fixed = TRUE))
  expect_true(stan_pattern("credit goes to the Prophet development team", code_with_trend, fixed = TRUE))

  # Check changepoint matrix implementation details
  expect_true(stan_pattern("matrix\\[T, S\\] Kappa;", code_with_trend))
  expect_true(stan_pattern("row_vector\\[S\\] a_row;", code_with_trend))
  expect_true(stan_pattern("while \\(\\(cp_idx <= S\\) && \\(t\\[i\\] >= t_change_trend\\[cp_idx\\]\\)\\)", code_with_trend))

  # Check for linear trend function
  expect_true(stan_pattern("vector linear_trend\\(real k, real m, vector delta, vector t, matrix Kappa_trend,", code_with_trend))
  expect_true(stan_pattern("Function to compute a linear trend with changepoints", code_with_trend, fixed = TRUE))
  expect_true(stan_pattern("return \\(k \\+ Kappa_trend \\* delta\\) \\.\\* t \\+ \\(m \\+ Kappa_trend \\* \\(-t_change_trend \\.\\* delta\\)\\);", code_with_trend))

  # 2. Data Block - Piecewise-specific data structures
  # Standard trend dimensions
  expect_true(stan_pattern("int<lower=1> N_trend;", code_with_trend, fixed = TRUE))
  expect_true(stan_pattern("int<lower=1> N_series_trend;", code_with_trend, fixed = TRUE))
  expect_true(stan_pattern("int<lower=1> N_lv_trend;", code_with_trend, fixed = TRUE))

  # Piecewise-specific data
  expect_true(stan_pattern("int<lower=0> n_change_trend;", code_with_trend))
  expect_true(stan_pattern("vector\\[n_change_trend\\] t_change_trend;", code_with_trend))
  expect_true(stan_pattern("real<lower=0> change_scale_trend;", code_with_trend))

  # GLM optimization components
  expect_true(stan_pattern("vector\\[1\\] mu_ones;", code_with_trend))

  # Observation-to-trend mapping arrays
  expect_true(stan_pattern("array\\[N\\] int obs_trend_time;", code_with_trend))
  expect_true(stan_pattern("array\\[N\\] int obs_trend_series;", code_with_trend))

  # Times trend matrix
  expect_true(stan_pattern("array\\[N_trend, N_series_trend\\] int times_trend;", code_with_trend))

  # 3. Transformed Data Block - Time vector and changepoint matrix
  # Factor loading matrix (diagonal for PW - no factor model support)
  expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z = diag_matrix\\(rep_vector\\(1\\.0, N_lv_trend\\)\\);", code_with_trend))

  # Time vector creation (integer sequence)
  expect_true(stan_pattern("vector\\[N_trend\\] time_trend;", code_with_trend))
  expect_true(stan_pattern("for \\(i in 1:N_trend\\) time_trend\\[i\\] = i;", code_with_trend))

  # Changepoint matrix computation
  expect_true(stan_pattern("matrix\\[N_trend, n_change_trend\\] Kappa_trend = get_changepoint_matrix\\(time_trend, t_change_trend, N_trend, n_change_trend\\);", code_with_trend))

  # 4. Parameters Block - PW-specific parameters
  # Base trend parameters
  expect_true(stan_pattern("vector\\[N_lv_trend\\] k_trend;", code_with_trend))
  expect_true(stan_pattern("vector\\[N_lv_trend\\] m_trend;", code_with_trend))
  expect_true(stan_pattern("matrix\\[n_change_trend, N_lv_trend\\] delta_trend;", code_with_trend))

  # Standard observation model parameters
  expect_true(stan_pattern("vector\\[Kc\\] b;", code_with_trend))
  expect_true(stan_pattern("real Intercept;", code_with_trend))
  expect_false(stan_pattern("real Intercept_trend;", code_with_trend))

  # Should NOT have innovation/sigma parameters (PW doesn't use them)
  expect_false(grepl("innovations_trend", code_with_trend, fixed = TRUE))
  expect_false(grepl("sigma_trend", code_with_trend, fixed = TRUE))

  # 5. Transformed Parameters Block - Trend computation
  # Prior accumulation
  expect_true(stan_pattern("real lprior = 0;", code_with_trend, fixed = TRUE))
  expect_true(stan_pattern("lprior \\+= student_t_lpdf\\(Intercept \\|", code_with_trend))
  expect_false(stan_pattern("lprior \\+= student_t_lpdf\\(Intercept_trend \\|", code_with_trend))

  # Latent trend matrix declaration
  expect_true(stan_pattern("matrix\\[N_trend, N_lv_trend\\] lv_trend;", code_with_trend))

  # Linear trend computation
  expect_true(stan_pattern("for \\(s in 1 : N_lv_trend\\)", code_with_trend))
  expect_true(stan_pattern("lv_trend\\[1 : N_trend, s\\] = linear_trend\\(k_trend\\[s\\], m_trend\\[s\\],", code_with_trend))
  expect_true(stan_pattern("to_vector\\(delta_trend\\[ : , s\\]\\), time_trend,", code_with_trend))
  expect_true(stan_pattern("Kappa_trend,", code_with_trend))
  expect_true(stan_pattern("t_change_trend\\);", code_with_trend))

  # Universal trend computation pattern
  expect_true(stan_pattern("matrix\\[N_trend, N_series_trend\\] trend;", code_with_trend))
  expect_false(stan_pattern("vector\\[N_trend\\] mu_trend = rep_vector\\(Intercept_trend, N_trend\\);", code_with_trend))
  expect_true(stan_pattern("for \\(i in 1:N_trend\\)", code_with_trend))
  expect_true(stan_pattern("for \\(s in 1:N_series_trend\\)", code_with_trend))
  expect_true(stan_pattern("trend\\[i, s\\] = dot_product\\(Z\\[s, :\\], lv_trend\\[i, :\\]\\) \\+ mu_trend\\[times_trend\\[i, s\\]\\];", code_with_trend))

  # GLM-compatible mu construction and trend injection
  expect_true(stan_pattern("mu \\+= Xc \\* b;", code_with_trend))
  expect_true(stan_pattern("for \\(n in 1:N\\)", code_with_trend))
  expect_true(stan_pattern("mu\\[n\\] \\+= trend\\[obs_trend_time\\[n\\], obs_trend_series\\[n\\]\\];", code_with_trend))

  # 6. Model Block - Priors and likelihood
  # PW-specific priors (check existence, not specific distributions)
  expect_true(stan_pattern("m_trend ~", code_with_trend))
  expect_true(stan_pattern("k_trend ~", code_with_trend))
  expect_true(stan_pattern("to_vector\\(delta_trend\\) ~", code_with_trend))

  # Should use double exponential for sparsity
  expect_true(stan_pattern("double_exponential", code_with_trend))
  expect_true(stan_pattern("change_scale_trend", code_with_trend))

  # GLM likelihood
  expect_true(stan_pattern("if \\(!prior_only\\)", code_with_trend))
  expect_true(stan_pattern("target \\+= poisson_log_glm_lpmf\\(Y \\| to_matrix\\(mu\\), 0\\.0, mu_ones\\);", code_with_trend))

  # Prior contributions
  expect_true(stan_pattern("target \\+= lprior;", code_with_trend))

  # 7. Generated Quantities Block
  expect_true(stan_pattern("real b_Intercept = Intercept - dot_product\\(means_X, b\\);", code_with_trend))

  # 8. Anti-patterns - Things that should NOT be present
  # Should NOT have AR/MA/VAR parameters
  expect_false(grepl("ar[0-9]+_trend", code_with_trend))
  expect_false(grepl("theta[0-9]+_trend", code_with_trend))
  expect_false(grepl("A[0-9]+_trend", code_with_trend))

  # Should NOT have correlation parameters (PW doesn't support correlated trends)
  expect_false(grepl("L_Omega_trend", code_with_trend, fixed = TRUE))
  expect_false(grepl("Sigma_trend", code_with_trend, fixed = TRUE))

  # Should NOT have factor model parameters (PW doesn't support factors)
  expect_false(grepl("Z_raw", code_with_trend, fixed = TRUE))
  expect_false(grepl("matrix\\[N_series_trend, N_lv_trend\\] Z;", code_with_trend))

  # Should NOT have RW/AR initialization patterns
  expect_false(grepl("lv_trend\\[1, :\\] = scaled_innovations", code_with_trend))
  expect_false(grepl("lv_trend\\[i, :\\] = lv_trend\\[i-1", code_with_trend))

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

test_that("stancode handles distributional regression models correctly", {
  data <- setup_stan_test_data()$univariate

  # Distributional model: single response with distributional parameter
  mf_distributional <- mvgam_formula(
    bf(y ~ x, sigma ~ temperature),
    trend_formula = ~ RW()
  )

  code_distributional <- stancode(
    mf_distributional, data = data,
    family = gaussian(),
    validate = FALSE
  )

  # Basic structure checks
  expect_s3_class(code_distributional, "mvgamstancode")
  expect_s3_class(code_distributional, "stancode")

  # 1. Classification check - should be univariate (not multivariate)
  # Key indicator: N_trend should appear (not N_trend_y)
  expect_true(stan_pattern("int<lower=1> N_trend;", code_distributional))
  expect_false(grepl("N_trend_y", code_distributional))

  # 2. Data Block - univariate trend structure (not response-specific)
  expect_true(stan_pattern("int<lower=1> N_series_trend;", code_distributional))
  expect_true(stan_pattern("int<lower=1> N_lv_trend;", code_distributional))
  expect_true(stan_pattern("array\\[N_trend, N_series_trend\\] int times_trend;", code_distributional))
  expect_true(stan_pattern("array\\[N\\] int obs_trend_time;", code_distributional))
  expect_true(stan_pattern("array\\[N\\] int obs_trend_series;", code_distributional))

  # Should NOT have response-specific trend arrays
  expect_false(grepl("obs_trend_time_y", code_distributional))
  expect_false(grepl("obs_trend_series_y", code_distributional))

  # 3. Distributional parameter structure for sigma
  expect_true(stan_pattern("real Intercept_sigma;", code_distributional))
  expect_true(stan_pattern("matrix\\[N, K_sigma\\] X_sigma;", code_distributional))
  expect_true(stan_pattern("vector\\[Kc_sigma\\] b_sigma;", code_distributional))

  # 4. Transformed Data - factor loading matrix (diagonal for univariate)
  expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z = diag_matrix\\(rep_vector\\(1\\.0, N_lv_trend\\)\\);", code_distributional))

  # 5. Parameters Block - RW trend parameters
  expect_true(stan_pattern("vector<lower=0>\\[N_lv_trend\\] sigma_trend;", code_distributional))
  expect_true(stan_pattern("matrix\\[N_trend, N_lv_trend\\] innovations_trend;", code_distributional))

  # 6. Transformed Parameters - RW dynamics
  expect_true(stan_pattern("vector\\[N_trend\\] mu_trend = rep_vector\\(0\\.0, N_trend\\);", code_distributional))
  expect_true(stan_pattern("matrix\\[N_trend, N_lv_trend\\] scaled_innovations_trend;", code_distributional))
  expect_true(stan_pattern("scaled_innovations_trend = innovations_trend \\* diag_matrix\\(sigma_trend\\);", code_distributional))

  # RW state evolution
  expect_true(stan_pattern("lv_trend\\[1, :\\] = scaled_innovations_trend\\[1, :\\];", code_distributional))
  expect_true(stan_pattern("lv_trend\\[i, :\\] = lv_trend\\[i - 1, :\\] \\+ scaled_innovations_trend\\[i, :\\];", code_distributional))

  # Final trend computation
  expect_true(stan_pattern("matrix\\[N_trend, N_series_trend\\] trend;", code_distributional))
  expect_true(stan_pattern("trend\\[i, s\\] = dot_product\\(Z\\[s, :\\], lv_trend\\[i, :\\]\\) \\+ mu_trend\\[times_trend\\[i, s\\]\\];", code_distributional))

  # 7. Model Block - trend injection into mu only
  expect_true(stan_pattern("vector\\[N\\] mu = rep_vector\\(0\\.0, N\\);", code_distributional))
  expect_true(stan_pattern("vector\\[N\\] sigma = rep_vector\\(0\\.0, N\\);", code_distributional))

  # Trend should be injected into mu linear predictor
  expect_true(stan_pattern("mu\\[n\\] \\+= trend\\[obs_trend_time\\[n\\], obs_trend_series\\[n\\]\\];", code_distributional))

  # Trend should NOT be injected into sigma
  expect_false(grepl("sigma\\[n\\] \\+= trend", code_distributional))

  # Sigma construction from distributional parameters
  expect_true(stan_pattern("sigma \\+= Intercept_sigma \\+ Xc_sigma \\* b_sigma;", code_distributional))
  expect_true(stan_pattern("sigma = exp\\(sigma\\);", code_distributional))

  # 8. Likelihood - normal with both mu and sigma
  expect_true(stan_pattern("target \\+= normal_lpdf\\(Y \\| mu, sigma\\);", code_distributional))

  # 9. Prior structure
  expect_true(stan_pattern("lprior \\+= student_t_lpdf\\(Intercept \\|", code_distributional))
  expect_true(stan_pattern("lprior \\+= student_t_lpdf\\(Intercept_sigma \\|", code_distributional))
  expect_true(stan_pattern("sigma_trend ~ exponential\\(2\\);", code_distributional))
  expect_true(stan_pattern("to_vector\\(innovations_trend\\) ~ std_normal\\(\\);", code_distributional))

  # 10. Anti-patterns - should NOT have multivariate structure
  # No response-specific trend dimensions
  expect_false(grepl("N_trend_y", code_distributional))
  expect_false(grepl("N_y", code_distributional))

  # No multivariate correlation structure
  expect_false(grepl("L_Omega_trend", code_distributional))
  expect_false(grepl("cholesky_factor_corr", code_distributional))

  # No response-specific trend parameters
  expect_false(grepl("innovations_trend_y", code_distributional))
  expect_false(grepl("sigma_trend_y", code_distributional))

  # Check for no duplicated Stan blocks
  expect_equal(length(gregexpr("^\\s*data\\s*\\{", code_distributional)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*transformed data\\s*\\{", code_distributional)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*parameters\\s*\\{", code_distributional)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*transformed parameters\\s*\\{", code_distributional)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*model\\s*\\{", code_distributional)[[1]]), 1)
  expect_equal(length(gregexpr("^\\s*generated quantities\\s*\\{", code_distributional)[[1]]), 1)

  # Final validation: ensure model compiles correctly
  expect_no_error(stancode(mf_distributional, data = data, family = gaussian(), validate = TRUE))
})

# Unsupported Family Validation Tests ----

test_that("multi-category families are blocked with informative error", {
  data <- setup_stan_test_data()$univariate

  # Create a simple formula for testing
mf <- mvgam_formula(y ~ x)

  # Test categorical family is blocked
  expect_error(
    stancode(mf, data = data, family = categorical()),
    regexp = "categorical.*not supported",
    ignore.case = TRUE
  )

  # Test multinomial family is blocked
  expect_error(
    stancode(mf, data = data, family = multinomial()),
    regexp = "multinomial.*not supported",
    ignore.case = TRUE
  )

  # Test dirichlet family is blocked
  expect_error(
    stancode(mf, data = data, family = dirichlet()),
    regexp = "dirichlet.*not supported",
    ignore.case = TRUE
  )

  # Test logistic_normal family is blocked
  expect_error(
    stancode(mf, data = data, family = logistic_normal()),
    regexp = "logistic_normal.*not supported",
    ignore.case = TRUE
  )
})

test_that("error message directs users to brms for multi-category families", {
  data <- setup_stan_test_data()$univariate
  mf <- mvgam_formula(y ~ x)

  # Verify error message mentions brms as alternative
  expect_error(
    stancode(mf, data = data, family = categorical()),
    regexp = "brms",
    ignore.case = TRUE
  )
})

test_that("ordinal families remain supported", {
  data <- setup_stan_test_data()$univariate

  # Create ordered factor response for ordinal models
  data$y_ord <- ordered(cut(data$y, breaks = 4, labels = c("low", "med", "high", "vhigh")))
  mf <- mvgam_formula(y_ord ~ x)

  # Cumulative family should work (ordinal uses 2D linpred)
  expect_no_error(
    stancode(mf, data = data, family = cumulative(), validate = FALSE)
  )

  # Sequential ratio family should work
  expect_no_error(
    stancode(mf, data = data, family = sratio(), validate = FALSE)
  )

  # Continuation ratio family should work
  expect_no_error(
    stancode(mf, data = data, family = cratio(), validate = FALSE)
  )

  # Adjacent category family should work
  expect_no_error(
    stancode(mf, data = data, family = acat(), validate = FALSE)
  )
})

test_that("hurdle_poisson stancode has correct structure", {
  data <- setup_stan_test_data()$univariate
  mf <- mvgam_formula(y ~ x, trend_formula = ~ AR())

  code <- stancode(mf, data = data, family = hurdle_poisson(), validate = FALSE)

  # Hurdle function definitions should exist in functions block
  expect_true(stan_pattern("real hurdle_poisson_lpmf", code))
  expect_true(stan_pattern("real hurdle_poisson_log_lpmf", code))
  expect_true(stan_pattern("real hurdle_poisson_logit_lpmf", code))

  # Hurdle probability parameter should be declared
  expect_true(stan_pattern("real<lower=0,upper=1> hu;", code))

  # Prior for hu should exist
  expect_true(stan_pattern("beta_lpdf\\(hu \\|", code))

  # Likelihood must be INSIDE the for loop (this was the bug fix)
  # The pattern ensures hurdle_poisson_log_lpmf appears after for(n in 1:N){
  expect_true(stan_pattern(
    "for\\(n in 1:N\\)\\{[^}]*hurdle_poisson_log_lpmf\\(Y\\[n\\]\\|mu\\[n\\],hu\\)",
    code
  ))

  # Also verify mu is computed before the likelihood loop
  expect_true(stan_pattern("mu\\[n\\]\\+=trend", code))

  # Should pass stanc validation
  expect_no_error(
    stancode(
      mf, data = data, family = hurdle_poisson(), validate = TRUE
    )
  )
})
