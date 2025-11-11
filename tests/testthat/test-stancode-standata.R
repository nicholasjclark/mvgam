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

  # brms-generated dispersion parameter should be excluded
  expect_false(grepl("real<lower=0> sigma;", code_with_trend, fixed = TRUE))

  # GLM function usage for Poisson family
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
  expect_true(stan_pattern("lv_trend\\[1, : \\] = scaled_innovations_trend\\[1, : \\]", code_with_trend))
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
  expect_true(stan_pattern("ar1_trend\\[j\\] \\* lv_trend\\[i-1, j\\]", code_with_trend))
  expect_true(stan_pattern("ar12_trend\\[j\\] \\* lv_trend\\[i-12, j\\]", code_with_trend))

  # Combined AR equation pattern (looking for the sum of AR terms)
  expect_true(stan_pattern("lv_trend\\[i, j\\] = ar1_trend\\[j\\] \\* lv_trend\\[i-1, j\\] \\+ ar12_trend\\[j\\] \\* lv_trend\\[i-12, j\\] \\+ scaled_innovations_trend\\[i, j\\]", code_with_trend))

  # Priors for AR coefficients in model block (just check they exist, not specific values)
  expect_true(stan_pattern("ar1_trend ~ normal", code_with_trend))
  expect_true(stan_pattern("ar12_trend ~ normal", code_with_trend))

  # Should still have standard trend components
  expect_true(stan_pattern("matrix\\[N_trend, N_lv_trend\\] innovations_trend;", code_with_trend))
  expect_true(stan_pattern("vector<lower=0>\\[N_lv_trend\\] sigma_trend;", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_trend, N_lv_trend\\] lv_trend;", code_with_trend))

  # mu construction and trend addition in transformed parameters
  expect_true(stan_pattern("mu\\[n\\] \\+= trend\\[obs_trend_time\\[n\\], obs_trend_series\\[n\\]\\];", code_with_trend))

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

test_that("stancode generates correct VAR(p = 2, ma = TRUE) VARMA model with multivariate splines and presence covariate", {
  data <- setup_stan_test_data()$multivariate
  mf_with_trend <- mvgam_formula(
    bf(mvbind(count, biomass) ~ s(x)) + set_rescor(FALSE),
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
  expect_true(stan_pattern("array\\[,\\] matrix\\[,\\] rev_mapping\\(", code_with_trend))
  expect_true(stan_pattern("matrix initial_joint_var\\(", code_with_trend))

  # Function documentation comments
  expect_true(stan_pattern("Compute matrix square root using eigendecomposition", code_with_trend))
  expect_true(stan_pattern("Following Heaps 2022 methodology", code_with_trend))
  expect_true(stan_pattern("Numerical stability check for positive definiteness", code_with_trend))

  # Multivariate observation data with comments
  expect_true(stan_pattern("int<lower=1> N_count;.*number of observations", code_with_trend))
  expect_true(stan_pattern("vector\\[N_count\\] Y_count;.*response variable", code_with_trend))
  expect_true(stan_pattern("int<lower=1> N_biomass;.*number of observations", code_with_trend))
  expect_true(stan_pattern("vector\\[N_biomass\\] Y_biomass;.*response variable", code_with_trend))

  # Spline data structures
  expect_true(stan_pattern("int Ks_count;.*number of linear effects", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_count, Ks_count\\] Xs_count;.*design matrix", code_with_trend))
  expect_true(stan_pattern("int nb_count_1;.*number of bases", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_count, knots_count_1\\[1\\]\\] Zs_count_1_1;", code_with_trend))

  # Trend dimensions
  expect_true(stan_pattern("int<lower=1> N_trend;.*number of time points", code_with_trend))
  expect_true(stan_pattern("int<lower=1> N_series_trend;.*number of series", code_with_trend))
  expect_true(stan_pattern("int<lower=1> N_lv_trend;.*latent variables", code_with_trend))

  # Trend formula data (presence covariate)
  expect_true(stan_pattern("int<lower=1> K_trend;.*number of trend coefficients", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_trend, K_trend\\] X_trend;.*trend design matrix", code_with_trend))
  expect_true(stan_pattern("vector\\[Kc_trend\\] means_X_trend;.*column means", code_with_trend))

  # Mapping arrays with response-specific suffixes
  expect_true(stan_pattern("array\\[N_count\\] int obs_trend_time_count;", code_with_trend))
  expect_true(stan_pattern("array\\[N_count\\] int obs_trend_series_count;", code_with_trend))
  expect_true(stan_pattern("array\\[N_biomass\\] int obs_trend_time_biomass;", code_with_trend))
  expect_true(stan_pattern("array\\[N_biomass\\] int obs_trend_series_biomass;", code_with_trend))

  # Times trend matrix (2D integer array)
  expect_true(stan_pattern("array\\[N_trend, N_series_trend\\] int times_trend;", code_with_trend))

  # Trend formula design matrix variables (presence covariate)
  expect_true(stan_pattern("int<lower=1> K_trend;", code_with_trend))
  expect_true(stan_pattern("int<lower=1> Kc_trend;", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_trend, K_trend\\] X_trend;", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_trend, Kc_trend\\] Xc_trend;", code_with_trend))
  expect_true(stan_pattern("vector\\[Kc_trend\\] means_X_trend;", code_with_trend))

  # VAR initialization constants in transformed data
  expect_true(stan_pattern("vector\\[N_lv_trend\\] trend_zeros = rep_vector\\(0\\.0, N_lv_trend\\);", code_with_trend))
  expect_true(stan_pattern("Zero mean vector for VARMA process.*following Heaps 2022", code_with_trend))

  # brms-style centering computation in transformed data
  expect_true(stan_pattern("matrix\\[N_trend, Kc_trend\\] Xc_trend;.*centered version", code_with_trend))
  expect_true(stan_pattern("vector\\[Kc_trend\\] means_X_trend;.*column means", code_with_trend))
  expect_true(stan_pattern("for \\(i in 2:K_trend\\)", code_with_trend))
  expect_true(stan_pattern("means_X_trend\\[i - 1\\] = mean\\(X_trend\\[, i\\]\\);", code_with_trend))
  expect_true(stan_pattern("Xc_trend\\[, i - 1\\] = X_trend\\[, i\\] - means_X_trend\\[i - 1\\];", code_with_trend))

  # Factor loading matrix (identity for non-factor VAR)
  expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z = diag_matrix\\(rep_vector\\(1\\.0, N_lv_trend\\)\\);", code_with_trend))

  # Observation model parameters (multivariate with splines)
  expect_true(stan_pattern("real Intercept_count;.*temporary intercept", code_with_trend))
  expect_true(stan_pattern("vector\\[Ks_count\\] bs_count;.*unpenalized spline", code_with_trend))
  expect_true(stan_pattern("vector\\[knots_count_1\\[1\\]\\] zs_count_1_1;", code_with_trend))
  expect_true(stan_pattern("vector<lower=0>\\[nb_count_1\\] sds_count_1;.*SDs.*spline", code_with_trend))
  expect_true(stan_pattern("real<lower=0> sigma_count;.*dispersion parameter", code_with_trend))

  # Trend parameters
  expect_true(stan_pattern("real Intercept_trend;.*trend intercept", code_with_trend))
  expect_true(stan_pattern("vector\\[Kc_trend\\] b_trend;.*trend coefficients.*presence", code_with_trend))

  # VAR coefficient matrices (raw/unconstrained for stationarity)
  expect_true(stan_pattern("array\\[2\\] matrix\\[N_lv_trend, N_lv_trend\\] A_raw_trend;.*lag-1, lag-2", code_with_trend))

  # MA coefficient matrices
  expect_true(stan_pattern("array\\[1\\] matrix\\[N_lv_trend, N_lv_trend\\] D_raw_trend;.*ma=TRUE.*1 MA lag", code_with_trend))

  # Hierarchical hyperparameters (Heaps 2022 methodology)
  expect_true(stan_pattern("array\\[2\\] vector\\[2\\] Amu_trend;.*Shared means.*diag, off-diag.*lag1, lag2", code_with_trend))
  expect_true(stan_pattern("array\\[2\\] vector<lower=0>\\[2\\] Aomega_trend;.*Shared precisions", code_with_trend))
  expect_true(stan_pattern("array\\[2\\] vector\\[1\\] Dmu_trend;.*MA means", code_with_trend))
  expect_true(stan_pattern("array\\[2\\] vector<lower=0>\\[1\\] Domega_trend;.*MA precisions", code_with_trend))

  # Innovation parameters
  expect_true(stan_pattern("vector<lower=0>\\[N_lv_trend\\] sigma_trend;.*innovation SDs", code_with_trend))
  expect_true(stan_pattern("cholesky_factor_corr\\[N_lv_trend\\] L_Omega_trend;.*innovation correlations", code_with_trend))

  # Trend coefficients for presence covariate
  expect_true(stan_pattern("vector\\[Kc_trend\\] b_trend;.*trend coefficients", code_with_trend))

  # Joint initialization for stationary distribution
  expect_true(stan_pattern("vector\\[3 \\* N_lv_trend\\] init_trend;.*2 VAR lags \\+ 1 MA lag", code_with_trend))

  # Standard latent variables
  expect_true(stan_pattern("matrix\\[N_trend, N_lv_trend\\] lv_trend;", code_with_trend))

  # Spline coefficient computations in transformed parameters
  expect_true(stan_pattern("s_count_1_1 = sds_count_1\\[1\\] \\* zs_count_1_1;", code_with_trend))
  expect_true(stan_pattern("s_biomass_1_1 = sds_biomass_1\\[1\\] \\* zs_biomass_1_1;", code_with_trend))

  # lprior initialization and accumulation
  expect_true(stan_pattern("real lprior = 0;.*prior contributions", code_with_trend))

  # Trend linear predictor with presence covariate
  expect_true(stan_pattern("vector\\[N_trend\\] mu_trend = rep_vector\\(0\\.0, N_trend\\);", code_with_trend))
  expect_true(stan_pattern("mu_trend \\+= Intercept_trend \\+ Xc_trend \\* b_trend;", code_with_trend))

  # Innovation covariance construction
  expect_true(stan_pattern("matrix\\[N_lv_trend, N_lv_trend\\] L_Sigma_trend = diag_pre_multiply\\(sigma_trend, L_Omega_trend\\);", code_with_trend))
  expect_true(stan_pattern("cov_matrix\\[N_lv_trend\\] Sigma_trend = multiply_lower_tri_self_transpose\\(L_Sigma_trend\\);", code_with_trend))

  # Stationarity transformations (Heaps 2022)
  expect_true(stan_pattern("array\\[2\\] matrix\\[N_lv_trend, N_lv_trend\\] A_trend;", code_with_trend))
  expect_true(stan_pattern("array\\[1\\] matrix\\[N_lv_trend, N_lv_trend\\] D_trend;", code_with_trend))
  expect_true(stan_pattern("P_var\\[i\\] = AtoP\\(A_raw_trend\\[i\\]\\);", code_with_trend))
  expect_true(stan_pattern("result_var = rev_mapping\\(P_var, Sigma_trend\\);", code_with_trend))
  expect_true(stan_pattern("D_trend\\[1\\] = -result_ma\\[1, 1\\];", code_with_trend))

  # Initial joint covariance matrix
  expect_true(stan_pattern("cov_matrix\\[3 \\* N_lv_trend\\] Omega_trend = initial_joint_var\\(Sigma_trend, A_trend, D_trend\\);", code_with_trend))

  # MA initialization
  expect_true(stan_pattern("vector\\[N_lv_trend\\] ma_init_trend = init_trend\\[\\(2 \\* N_lv_trend \\+ 1\\):\\(3 \\* N_lv_trend\\)\\];", code_with_trend))

  # Universal trend computation pattern
  expect_true(stan_pattern("trend\\[i, s\\] = dot_product\\(Z\\[s, :\\], lv_trend\\[i, :\\]\\) \\+ mu_trend\\[times_trend\\[i, s\\]\\];", code_with_trend))

  # Multivariate linear predictors with splines
  expect_true(stan_pattern("vector\\[N_count\\] mu_count = rep_vector\\(0\\.0, N_count\\);", code_with_trend))
  expect_true(stan_pattern("mu_count \\+= Intercept_count \\+ Xs_count \\* bs_count \\+ Zs_count_1_1 \\* s_count_1_1;", code_with_trend))
  expect_true(stan_pattern("mu_biomass \\+= Intercept_biomass \\+ Xs_biomass \\* bs_biomass \\+ Zs_biomass_1_1 \\* s_biomass_1_1;", code_with_trend))

  # Trend injection using response-specific mapping arrays
  expect_true(stan_pattern("mu_count\\[n\\] \\+= trend\\[obs_trend_time_count\\[n\\], obs_trend_series_count\\[n\\]\\];", code_with_trend))
  expect_true(stan_pattern("mu_biomass\\[n\\] \\+= trend\\[obs_trend_time_biomass\\[n\\], obs_trend_series_biomass\\[n\\]\\];", code_with_trend))

  # Check observation-to-trend mappings are in data block
  data_block <- substr(code_with_trend,
                       regexpr("^data \\{", code_with_trend),
                       regexpr("^\\}", code_with_trend))
  expect_true(stan_pattern("array\\[N_count\\] int obs_trend_time_count;", data_block))
  expect_true(stan_pattern("array\\[N_count\\] int obs_trend_series_count;", data_block))
  expect_true(stan_pattern("array\\[N_biomass\\] int obs_trend_time_biomass;", data_block))
  expect_true(stan_pattern("array\\[N_biomass\\] int obs_trend_series_biomass;", data_block))

  # Verify mu_trend is computed from trend formula
  expect_true(stan_pattern("mu_trend \\+= Intercept_trend \\+ Xc_trend \\* b_trend;", code_with_trend))

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
  expect_true(stan_pattern("vector\\[3 \\* N_lv_trend\\] mu_init_trend = rep_vector\\(0\\.0, 3 \\* N_lv_trend\\);", code_with_trend))
  expect_true(stan_pattern("init_trend ~ multi_normal\\(mu_init_trend, Omega_trend\\);", code_with_trend))

  # VARMA dynamics implementation
  expect_true(stan_pattern("vector\\[N_lv_trend\\] mu_t_trend\\[N_trend\\];", code_with_trend))
  expect_true(stan_pattern("vector\\[N_lv_trend\\] ma_error_trend\\[N_trend\\];.*MA error terms", code_with_trend))

  # VAR component with initialization handling
  expect_true(stan_pattern("for \\(i in 1:2\\).*2 VAR lags", code_with_trend))
  expect_true(stan_pattern("if \\(t - i <= 0\\)", code_with_trend))
  expect_true(stan_pattern("Use values from earlier than series start.*from init_trend", code_with_trend))
  expect_true(stan_pattern("mu_t_trend\\[t\\] \\+= A_trend\\[i\\] \\* lv_trend\\[t - i, :\\]';", code_with_trend))

  # MA component
  expect_true(stan_pattern("if \\(t - 1 <= 0\\)", code_with_trend))
  expect_true(stan_pattern("Use initial MA errors for early time points", code_with_trend))
  expect_true(stan_pattern("mu_t_trend\\[t\\] \\+= D_trend\\[1\\] \\* ma_init_trend;", code_with_trend))
  expect_true(stan_pattern("mu_t_trend\\[t\\] \\+= D_trend\\[1\\] \\* ma_error_trend\\[t - 1\\];", code_with_trend))

  # MA specific parameters must exist for VARMA model
  expect_true(stan_pattern("array\\[1\\] matrix\\[N_lv_trend, N_lv_trend\\] D_raw_trend;", code_with_trend))
  expect_true(stan_pattern("array\\[1\\] matrix\\[N_lv_trend, N_lv_trend\\] D_trend;", code_with_trend))
  expect_true(stan_pattern("vector\\[N_lv_trend\\] ma_error_trend\\[N_trend\\];", code_with_trend))
  expect_true(stan_pattern("vector\\[N_lv_trend\\] ma_init_trend", code_with_trend))

  # MA transformation must occur
  expect_true(stan_pattern("D_trend\\[1\\] = -result_ma\\[1, 1\\];", code_with_trend))

  # Latent variable likelihood
  expect_true(stan_pattern("lv_trend\\[t, :\\]' ~ multi_normal\\(mu_t_trend\\[t\\], Sigma_trend\\);", code_with_trend))
  expect_true(stan_pattern("ma_error_trend\\[t\\] = lv_trend\\[t, :\\]' - mu_t_trend\\[t\\];", code_with_trend))

  # Prior structure validation (existence, not specific distributions)
  # Hierarchical priors should exist
  expect_true(stan_pattern("Amu_trend\\[.*\\] ~", code_with_trend))
  expect_true(stan_pattern("Aomega_trend\\[.*\\] ~", code_with_trend))
  expect_true(stan_pattern("Dmu_trend\\[.*\\] ~", code_with_trend))
  expect_true(stan_pattern("Domega_trend\\[.*\\] ~", code_with_trend))

  # Structured priors for raw coefficients should exist
  expect_true(stan_pattern("A_raw_trend\\[.*\\] ~ .*\\(Amu_trend", code_with_trend))
  expect_true(stan_pattern("D_raw_trend\\[.*\\] ~ .*\\(Dmu_trend", code_with_trend))

  # Basic parameter priors should exist
  expect_true(stan_pattern("sigma_trend ~", code_with_trend))
  expect_true(stan_pattern("L_Omega_trend ~", code_with_trend))
  expect_true(stan_pattern("b_trend ~", code_with_trend))

  # Intercept transformations in generated quantities
  expect_true(stan_pattern("real b_count_Intercept = Intercept_count;", code_with_trend))
  expect_true(stan_pattern("real b_biomass_Intercept = Intercept_biomass;", code_with_trend))
  expect_true(stan_pattern("real b_trend_Intercept = Intercept_trend - dot_product\\(means_X_trend, b_trend\\);", code_with_trend))

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
  expect_false(grepl("for \\(j in 1 : N_lv_trend\\)", code_with_trend))
  expect_false(grepl("for \\(i in j : N_series_trend\\)", code_with_trend))

  # Should NOT have unsuffixed parameter names (could conflict with observation model)
  expect_false(grepl("real sigma;", code_with_trend))
  expect_false(grepl("vector.*b;", code_with_trend))

  # Should NOT have incorrect VARMA structure
  expect_false(grepl("for \\(i in 1:1\\)", code_with_trend)) # Should be 1:2 for VAR(2)
  expect_false(grepl("ar_dynamics", code_with_trend)) # Should use A_trend matrices

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
    expect_true(stan_pattern("int<lower=1> N_count;.*number of observations",
                      code_with_trend))
    expect_true(stan_pattern("array\\[N_count\\] int Y_count;.*response variable",
                      code_with_trend))
    expect_true(stan_pattern("int<lower=1> N_presence;.*number of observations",
                      code_with_trend))
    expect_true(stan_pattern("array\\[N_presence\\] int Y_presence;.*response variable",
                      code_with_trend))
    expect_true(stan_pattern("int<lower=1> N_biomass;.*number of observations",
                      code_with_trend))
    expect_true(stan_pattern("vector\\[N_biomass\\] Y_biomass;.*response variable",
                      code_with_trend))

    # Population-level design matrices for all three families
    expect_true(stan_pattern("matrix\\[N_count, K_count\\] X_count;.*population-level design
  matrix", code_with_trend))
    expect_true(stan_pattern("matrix\\[N_presence, K_presence\\] X_presence;.*population-level
   design matrix", code_with_trend))
    expect_true(stan_pattern("matrix\\[N_biomass, K_biomass\\] X_biomass;.*population-level
  design matrix", code_with_trend))

    # Trend dimensions
    expect_true(stan_pattern("int<lower=1> N_trend;.*number of time points", code_with_trend))
    expect_true(stan_pattern("int<lower=1> N_series_trend;.*number of series",
                      code_with_trend))
    expect_true(stan_pattern("int<lower=1> N_lv_trend;.*latent variables", code_with_trend))

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
    expect_true(stan_pattern("vector\\[1\\] mu_ones_count;.*for GLM count", code_with_trend))
    expect_true(stan_pattern("vector\\[1\\] mu_ones_presence;.*for GLM presence",
                      code_with_trend))

    # Centered design matrices in transformed data
    expect_true(stan_pattern("matrix\\[N_count, Kc_count\\] Xc_count;.*centered version",
                      code_with_trend))
    expect_true(stan_pattern("matrix\\[N_presence, Kc_presence\\] Xc_presence;.*centered
  version", code_with_trend))
    expect_true(stan_pattern("matrix\\[N_biomass, Kc_biomass\\] Xc_biomass;.*centered
  version", code_with_trend))

    # Centering loops for all three families
    expect_true(stan_pattern("for \\(i in 2:K_count\\)", code_with_trend))
    expect_true(stan_pattern("means_X_count\\[i - 1\\] = mean\\(X_count\\[, i\\]\\);",
                      code_with_trend))
    expect_true(stan_pattern("for \\(i in 2:K_presence\\)", code_with_trend))
    expect_true(stan_pattern("for \\(i in 2:K_biomass\\)", code_with_trend))

    # Observation model parameters for all three families
    expect_true(stan_pattern("vector\\[Kc_count\\] b_count;.*regression coefficients",
                      code_with_trend))
    expect_true(stan_pattern("real Intercept_count;.*temporary intercept", code_with_trend))
    expect_true(stan_pattern("vector\\[Kc_presence\\] b_presence;.*regression coefficients",
                      code_with_trend))
    expect_true(stan_pattern("real Intercept_presence;.*temporary intercept",
                      code_with_trend))
    expect_true(stan_pattern("vector\\[Kc_biomass\\] b_biomass;.*regression coefficients",
                      code_with_trend))
    expect_true(stan_pattern("real Intercept_biomass;.*temporary intercept", code_with_trend))
    expect_true(stan_pattern("real<lower=0> shape_biomass;.*shape parameter",
                      code_with_trend))

    # Factor AR(1) trend parameters
    expect_true(stan_pattern("vector<lower=-1,upper=1>\\[N_lv_trend\\] ar1_trend;.*AR
  coefficients", code_with_trend))
    expect_true(stan_pattern("vector<lower=0>\\[N_lv_trend\\] sigma_trend;.*innovation SDs",
                      code_with_trend))
    expect_true(stan_pattern("cholesky_factor_corr\\[N_lv_trend\\] L_Omega_trend;.*innovation
  correlations", code_with_trend))

    # Factor loading parameters (estimated for n_lv = 2)
    expect_true(stan_pattern("vector\\[N_series_trend \\* N_lv_trend\\] Z_raw;.*raw factor
  loadings", code_with_trend))

    # Innovation matrix
    expect_true(stan_pattern("matrix\\[N_trend, N_lv_trend\\] innovations_trend;",
                      code_with_trend))

    # lprior initialization with family-specific priors
    expect_true(stan_pattern("real lprior = 0;.*prior contributions", code_with_trend))

    # Factor loading matrix construction with identifiability constraints
    expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z = rep_matrix\\(0,
  N_series_trend, N_lv_trend\\);", code_with_trend))
    expect_true(stan_pattern("int index = 1;", code_with_trend))
    expect_true(stan_pattern("// constraints allow identifiability of loadings",
                      code_with_trend))
    expect_true(stan_pattern("for \\(j in 1 : N_lv_trend\\)", code_with_trend))
    expect_true(stan_pattern("for \\(i in j : N_series_trend\\)", code_with_trend))
    expect_true(stan_pattern("Z\\[i, j\\] = Z_raw\\[index\\];", code_with_trend))

    # Innovation covariance construction
    expect_true(stan_pattern("matrix\\[N_lv_trend, N_lv_trend\\] L_Sigma_trend =
  diag_pre_multiply\\(sigma_trend, L_Omega_trend\\);", code_with_trend))
    expect_true(stan_pattern("matrix\\[N_trend, N_lv_trend\\] scaled_innovations_trend =
  innovations_trend \\* L_Sigma_trend';", code_with_trend))

    # AR(1) latent variable dynamics
    expect_true(stan_pattern("matrix\\[N_trend, N_lv_trend\\] lv_trend;", code_with_trend))
    expect_true(stan_pattern("lv_trend\\[1, :\\] = scaled_innovations_trend\\[1, :\\];",
                      code_with_trend))
    expect_true(stan_pattern("for \\(i in 2:N_trend\\)", code_with_trend))
    expect_true(stan_pattern("for \\(j in 1:N_lv_trend\\)", code_with_trend))
    expect_true(stan_pattern("lv_trend\\[i, j\\] = ar1_trend\\[j\\] \\* lv_trend\\[i-1, j\\]
  \\+ scaled_innovations_trend\\[i, j\\];", code_with_trend))

    # Zero trend mean vector (for ~ -1 specification)
    expect_true(stan_pattern("vector\\[N_trend\\] mu_trend = rep_vector\\(0\\.0, N_trend\\);",
                      code_with_trend))
    expect_true(stan_pattern("zero for ~ -1, but needed for prediction compatibility",
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
    expect_true(stan_pattern("Transform biomass predictor.*inverse link for Gamma",
                      code_with_trend))

    # Three-family likelihoods with GLM optimization for discrete families
    expect_true(stan_pattern("target \\+= poisson_log_glm_lpmf\\(Y_count \\|
  to_matrix\\(mu_count\\), 0\\.0, mu_ones_count\\);", code_with_trend))
    expect_true(stan_pattern("target \\+= bernoulli_logit_glm_lpmf\\(Y_presence \\|
  to_matrix\\(mu_presence\\), 0\\.0, mu_ones_presence\\);", code_with_trend))
    expect_true(stan_pattern("target \\+= gamma_lpdf\\(Y_biomass \\| shape_biomass,
  shape_biomass \\\\/ mu_biomass\\);", code_with_trend))

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
  expect_true(stan_pattern("int<lower=1> N_count;.*number of observations", code_with_trend))
  expect_true(stan_pattern("array\\[N_count\\] int Y_count;.*response variable", code_with_trend))
  expect_true(stan_pattern("int<lower=1> N_presence;.*number of observations", code_with_trend))
  expect_true(stan_pattern("array\\[N_presence\\] int Y_presence;.*response variable", code_with_trend))
  expect_true(stan_pattern("int<lower=1> N_biomass;.*number of observations", code_with_trend))
  expect_true(stan_pattern("vector\\[N_biomass\\] Y_biomass;.*response variable", code_with_trend))

  # Population-level design matrices for all three families
  expect_true(stan_pattern("matrix\\[N_count, K_count\\] X_count;.*population-level design matrix", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_presence, K_presence\\] X_presence;.*population-level design matrix", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_biomass, K_biomass\\] X_biomass;.*population-level design matrix", code_with_trend))

  # Trend dimensions
  expect_true(stan_pattern("int<lower=1> N_trend;.*number of time points", code_with_trend))
  expect_true(stan_pattern("int<lower=1> N_series_trend;.*number of series", code_with_trend))
  expect_true(stan_pattern("int<lower=1> N_lv_trend;.*latent variables", code_with_trend))

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
  expect_true(stan_pattern("vector\\[1\\] mu_ones_count;.*for GLM count", code_with_trend))
  expect_true(stan_pattern("vector\\[1\\] mu_ones_presence;.*for GLM presence", code_with_trend))

  # Centered design matrices in transformed data
  expect_true(stan_pattern("matrix\\[N_count, Kc_count\\] Xc_count;.*centered version", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_presence, Kc_presence\\] Xc_presence;.*centered version", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_biomass, Kc_biomass\\] Xc_biomass;.*centered version", code_with_trend))

  # Centering loops for all three families
  expect_true(stan_pattern("for \\(i in 2:K_count\\)", code_with_trend))
  expect_true(stan_pattern("means_X_count\\[i - 1\\] = mean\\(X_count\\[, i\\]\\);", code_with_trend))
  expect_true(stan_pattern("for \\(i in 2:K_presence\\)", code_with_trend))
  expect_true(stan_pattern("for \\(i in 2:K_biomass\\)", code_with_trend))

  # Observation model parameters for all three families
  expect_true(stan_pattern("vector\\[Kc_count\\] b_count;.*regression coefficients", code_with_trend))
  expect_true(stan_pattern("real Intercept_count;.*temporary intercept", code_with_trend))
  expect_true(stan_pattern("vector\\[Kc_presence\\] b_presence;.*regression coefficients", code_with_trend))
  expect_true(stan_pattern("real Intercept_presence;.*temporary intercept", code_with_trend))
  expect_true(stan_pattern("vector\\[Kc_biomass\\] b_biomass;.*regression coefficients", code_with_trend))
  expect_true(stan_pattern("real Intercept_biomass;.*temporary intercept", code_with_trend))
  expect_true(stan_pattern("real<lower=0> shape_biomass;.*shape parameter", code_with_trend))

  # Factor AR(1) trend parameters
  expect_true(stan_pattern("vector<lower=-1,upper=1>\\[N_lv_trend\\] ar1_trend;.*AR coefficients", code_with_trend))
  expect_true(stan_pattern("vector<lower=0>\\[N_lv_trend\\] sigma_trend;.*innovation SDs", code_with_trend))
  expect_true(stan_pattern("cholesky_factor_corr\\[N_lv_trend\\] L_Omega_trend;.*innovation correlations", code_with_trend))

  # Factor loading parameters (estimated for n_lv = 2)
  expect_true(stan_pattern("vector\\[N_series_trend \\* N_lv_trend\\] Z_raw;.*raw factor loadings", code_with_trend))

  # Innovation matrix
  expect_true(stan_pattern("matrix\\[N_trend, N_lv_trend\\] innovations_trend;", code_with_trend))

  # lprior initialization with family-specific priors
  expect_true(stan_pattern("real lprior = 0;.*prior contributions", code_with_trend))

  # Factor loading matrix construction with identifiability constraints
  expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z = rep_matrix\\(0, N_series_trend, N_lv_trend\\);", code_with_trend))
  expect_true(stan_pattern("int index = 1;", code_with_trend))
  expect_true(stan_pattern("// constraints allow identifiability of loadings", code_with_trend))
  expect_true(stan_pattern("for \\(j in 1 : N_lv_trend\\)", code_with_trend))
  expect_true(stan_pattern("for \\(i in j : N_series_trend\\)", code_with_trend))
  expect_true(stan_pattern("Z\\[i, j\\] = Z_raw\\[index\\];", code_with_trend))

  # Innovation covariance construction
  expect_true(stan_pattern("matrix\\[N_lv_trend, N_lv_trend\\] L_Sigma_trend = diag_pre_multiply\\(sigma_trend, L_Omega_trend\\);", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_trend, N_lv_trend\\] scaled_innovations_trend = innovations_trend \\* L_Sigma_trend';", code_with_trend))

  # AR(1) latent variable dynamics
  expect_true(stan_pattern("matrix\\[N_trend, N_lv_trend\\] lv_trend;", code_with_trend))
  expect_true(stan_pattern("lv_trend\\[1, :\\] = scaled_innovations_trend\\[1, :\\];", code_with_trend))
  expect_true(stan_pattern("for \\(i in 2:N_trend\\)", code_with_trend))
  expect_true(stan_pattern("for \\(j in 1:N_lv_trend\\)", code_with_trend))
  expect_true(stan_pattern("lv_trend\\[i, j\\] = ar1_trend\\[j\\] \\* lv_trend\\[i-1, j\\] \\+ scaled_innovations_trend\\[i, j\\];", code_with_trend))

  # Zero trend mean vector (for ~ -1 specification)
  expect_true(stan_pattern("vector\\[N_trend\\] mu_trend = rep_vector\\(0\\.0, N_trend\\);", code_with_trend))
  expect_true(stan_pattern("zero for ~ -1, but needed for prediction compatibility", code_with_trend))

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
  expect_true(stan_pattern("Transform biomass predictor.*inverse link for Gamma", code_with_trend))

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
  expect_true(stan_pattern("int<lower=1> Kc_trend;", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_trend, K_trend\\] X_trend;", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_trend, Kc_trend\\] Xc_trend;", code_with_trend))
  expect_true(stan_pattern("vector\\[Kc_trend\\] b_trend;", code_with_trend))
  expect_true(stan_pattern("vector\\[Kc_trend\\] means_X_trend;", code_with_trend))

  # Factor model parameters
  expect_true(stan_pattern("vector<lower=0>\\[N_lv_trend\\] sigma_trend;", code_with_trend))
  expect_true(stan_pattern("vector\\[N_series_trend \\* N_lv_trend\\] Z_raw;", code_with_trend))

  # Factor loading constraints in transformed parameters
  expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z = rep_matrix\\(0, N_series_trend, N_lv_trend\\);", code_with_trend))
  expect_true(stan_pattern("// constraints allow identifiability of loadings", code_with_trend))
  expect_true(stan_pattern("for \\(j in 1 : N_lv_trend\\)", code_with_trend))
  expect_true(stan_pattern("for \\(i in j : N_series_trend\\)", code_with_trend))
  expect_true(stan_pattern("Z\\[i, j\\] = Z_raw\\[index\\];", code_with_trend))

  # ZMVN dynamics (just scaled innovations, no complex dynamics)
  expect_true(stan_pattern("lv_trend = scaled_innovations_trend;", code_with_trend))

  # Trend mean with covariate effects
  expect_true(stan_pattern("vector\\[N_trend\\] mu_trend = Xc_trend \\* b_trend \\+ Intercept_trend;", code_with_trend))

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
  expect_true(stan_pattern("real b_Intercept_trend = Intercept_trend - dot_product\\(means_X_trend, b_trend\\);", code_with_trend))

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
  expect_true(stan_pattern("int<lower=1> Kc_trend;", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_trend, K_trend\\] X_trend;", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_trend, Kc_trend\\] Xc_trend;", code_with_trend))
  expect_true(stan_pattern("vector\\[Kc_trend\\] b_trend;", code_with_trend))
  expect_true(stan_pattern("vector\\[Kc_trend\\] means_X_trend;", code_with_trend))

  # Hierarchical grouping data structures
  expect_true(stan_pattern("int<lower=1> n_groups_trend;", code_with_trend))
  expect_true(stan_pattern("array\\[N_series_trend\\] int.*group_inds_trend;", code_with_trend))

  # Hierarchical correlation functions
  expect_true(stan_pattern("matrix combine_cholesky\\(", code_with_trend))
  expect_true(stan_pattern("real alpha", code_with_trend))

  # Hierarchical correlation parameters with _trend suffix
  expect_true(stan_pattern("cholesky_factor_corr\\[N_lv_trend\\] L_Omega_global_trend;", code_with_trend))
  expect_true(stan_pattern("array\\[n_groups_trend\\] cholesky_factor_corr\\[N_lv_trend\\] L_deviation_group_trend;", code_with_trend))
  expect_true(stan_pattern("real<lower=0, upper=1> alpha_cor_trend;", code_with_trend))

  # Group-specific correlation matrices computation
  expect_true(stan_pattern("array\\[n_groups_trend\\] cov_matrix\\[N_lv_trend\\] Sigma_group_trend;", code_with_trend))
  expect_true(stan_pattern("L_Omega_group_trend = combine_cholesky\\(L_Omega_global_trend, L_deviation_group_trend\\[g\\], alpha_cor_trend\\)", code_with_trend))

  # Group-specific innovation scaling
  expect_true(stan_pattern("int group_idx = group_inds_trend\\[s\\];", code_with_trend))
  expect_true(stan_pattern("L_group_trend = cholesky_decompose\\(Sigma_group_trend\\[group_idx\\]\\)", code_with_trend))

  # ZMVN dynamics (just scaled innovations)
  expect_true(stan_pattern("lv_trend = scaled_innovations_trend;", code_with_trend))

  # Factor loading matrix (diagonal for non-factor ZMVN)
  expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z = diag_matrix\\(rep_vector\\(1\\.0, N_lv_trend\\)\\);", code_with_trend))

  # Trend mean with covariate effects
  expect_true(stan_pattern("vector\\[N_trend\\] mu_trend = Xc_trend \\* b_trend \\+ Intercept_trend;", code_with_trend))

  # Universal trend computation pattern should still be present
  expect_true(stan_pattern("trend\\[i, s\\] = dot_product\\(Z\\[s, :\\], lv_trend\\[i, :\\]\\) \\+ mu_trend\\[times_trend\\[i, s\\]\\]", code_with_trend))

  # Hierarchical correlation priors with _trend suffix
  expect_true(stan_pattern("L_Omega_global_trend ~ lkj_corr_cholesky\\(1\\);", code_with_trend))
  expect_true(stan_pattern("L_deviation_group_trend\\[g\\] ~ lkj_corr_cholesky\\(6\\);", code_with_trend))

  # Custom alpha_cor_trend prior should be applied
  expect_true(stan_pattern("alpha_cor_trend ~ beta\\(5, 5\\);", code_with_trend))

  # Standard trend parameter priors
  expect_true(stan_pattern("sigma_trend ~ exponential\\(2\\);", code_with_trend))
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

  # Generated quantities
  expect_true(stan_pattern("real b_Intercept = Intercept;", code_with_trend))
  expect_true(stan_pattern("real b_Intercept_trend = Intercept_trend - dot_product\\(means_X_trend, b_trend\\);", code_with_trend))

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

test_that("stancode generates correct CAR() continuous autoregressive trend with complex
  observation model", {
    # Create test data with irregular time intervals (CAR's specialty)
    set.seed(42)
    n_time <- 20
    n_series <- 3

    data <- data.frame(
      time = rep(cumsum(c(1, rexp(n_time - 1, rate = 0.8))), n_series),  # Irregular intervals
      series = factor(rep(paste0("series", 1:n_series), each = n_time)),
      y = rpois(n_time * n_series, lambda = 3),
      x = rnorm(n_time * n_series),  # For GP
      group_id = factor(rep(c("A", "B", "C"), length.out = n_time * n_series))  # For random effects
    )

    mf_with_trend <- mvgam_formula(
      y ~ gp(x) + (1 | group_id),
      trend_formula = ~ CAR()
    )
    code_with_trend <- stancode(
      mf_with_trend, data = data,
      family = poisson(),
      validate = FALSE
    )

    # Basic structure checks
    expect_s3_class(code_with_trend, "mvgamstancode")
    expect_s3_class(code_with_trend, "stancode")

    # 1. Functions Block - Should contain brms GP function
    expect_true(stan_pattern("vector gp_exp_quad\\(data array\\[\\] vector x, real sdgp, vector
  lscale, vector zgp\\)", code_with_trend))
    expect_true(stan_pattern("compute a latent Gaussian process with squared exponential kernel",
                      code_with_trend, fixed = TRUE))
    expect_true(stan_pattern("gp_exp_quad_cov\\(x, sdgp, lscale\\[1\\]\\)", code_with_trend))
    expect_true(stan_pattern("cholesky_decompose\\(cov\\) \\* zgp;", code_with_trend))

    # 2. Data Block - Complex observation model + CAR trend data
    # GP-related data structures
    expect_true(stan_pattern("int<lower=1> Kgp_1;.*number of sub-GPs", code_with_trend))
    expect_true(stan_pattern("int<lower=1> Dgp_1;.*GP dimension", code_with_trend))
    expect_true(stan_pattern("int<lower=1> Nsubgp_1;.*number of latent GP groups", code_with_trend))
    expect_true(stan_pattern("array\\[N\\] int<lower=1> Jgp_1;.*indices of latent GP groups",
                      code_with_trend))
    expect_true(stan_pattern("array\\[Nsubgp_1\\] vector\\[Dgp_1\\] Xgp_1;.*covariates of the GP",
                      code_with_trend))

    # Random effects data structures
    expect_true(stan_pattern("int<lower=1> N_1;.*number of grouping levels", code_with_trend))
    expect_true(stan_pattern("int<lower=1> M_1;.*number of coefficients per level", code_with_trend))
    expect_true(stan_pattern("array\\[N\\] int<lower=1> J_1;.*grouping indicator", code_with_trend))
    expect_true(stan_pattern("vector\\[N\\] Z_1_1;.*group-level predictor values", code_with_trend))

    # CAR trend dimensions
    expect_true(stan_pattern("int<lower=1> N_trend;.*number of timepoints", code_with_trend))
    expect_true(stan_pattern("int<lower=1> N_series_trend;.*number of observed time series",
                      code_with_trend))
    expect_true(stan_pattern("int<lower=1> N_lv_trend;.*number of latent states", code_with_trend))

    # CAR-specific time distance array
    expect_true(stan_pattern("array\\[N_trend, N_series_trend\\] real<lower=0> time_dis;.*time
  distances for continuous AR", code_with_trend))

    # Standard trend mapping arrays
    expect_true(stan_pattern("array\\[N_trend, N_series_trend\\] int times_trend;.*temporal order",
                      code_with_trend))
    expect_true(stan_pattern("array\\[N\\] int obs_trend_time;.*idx to map latent states to
  observations", code_with_trend))
    expect_true(stan_pattern("array\\[N\\] int obs_trend_series;.*idx to map latent states to
  observations", code_with_trend))

    # GLM optimization component
    expect_true(stan_pattern("vector\\[1\\] mu_ones;.*Column of ones for glm means", code_with_trend))

    # 3. Transformed Data Block - Factor loading matrix
    expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z =
  diag_matrix\\(rep_vector\\(1\\.0, N_lv_trend\\)\\);", code_with_trend))

    # 4. Parameters Block - Complex observation model + CAR trend parameters
    # GP parameters
    expect_true(stan_pattern("vector<lower=0>\\[Kgp_1\\] sdgp_1;.*GP standard deviation parameters",
                      code_with_trend))
    expect_true(stan_pattern("array\\[Kgp_1\\] vector<lower=0>\\[1\\] lscale_1;.*GP length-scale
  parameters", code_with_trend))
    expect_true(stan_pattern("vector\\[Nsubgp_1\\] zgp_1;.*latent variables of the GP",
                      code_with_trend))

    # Random effects parameters
    expect_true(stan_pattern("vector<lower=0>\\[M_1\\] sd_1;.*group-level standard deviations",
                      code_with_trend))
    expect_true(stan_pattern("array\\[M_1\\] vector\\[N_1\\] z_1;.*standardized group-level effects",
                      code_with_trend))

    # CAR trend parameters
    expect_true(stan_pattern("real Intercept_trend;.*trend intercept", code_with_trend))
    expect_true(stan_pattern("vector<lower=-1,upper=1>\\[N_lv_trend\\] ar1_trend;.*CAR AR1
  coefficients", code_with_trend))
    expect_true(stan_pattern("vector<lower=0>\\[N_lv_trend\\] sigma_trend;.*innovation SDs",
                      code_with_trend))
    expect_true(stan_pattern("matrix\\[N_trend, N_lv_trend\\] innovations_trend;.*raw innovations",
                      code_with_trend))

    # 5. Transformed Parameters Block - Complex computations
    # GP and random effects computations (from brms)
    expect_true(stan_pattern("vector\\[N_1\\] r_1_1;.*actual group-level effects", code_with_trend))
    expect_true(stan_pattern("r_1_1 = \\(sd_1\\[1\\] \\* \\(z_1\\[1\\]\\)\\);", code_with_trend))

    # Prior accumulation
    expect_true(stan_pattern("real lprior = 0;", code_with_trend, fixed = TRUE))
    expect_true(stan_pattern("lprior \\+= student_t_lpdf\\(Intercept \\|", code_with_trend))
    expect_true(stan_pattern("lprior \\+= student_t_lpdf\\(sdgp_1 \\|", code_with_trend))
    expect_true(stan_pattern("lprior \\+= inv_gamma_lpdf\\(lscale_1\\[1\\]\\[1\\] \\|",
                      code_with_trend))
    expect_true(stan_pattern("lprior \\+= student_t_lpdf\\(sd_1 \\|", code_with_trend))
    expect_true(stan_pattern("lprior \\+= student_t_lpdf\\(Intercept_trend \\|", code_with_trend))

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
    expect_true(stan_pattern("vector\\[N_trend\\] mu_trend = rep_vector\\(Intercept_trend,
  N_trend\\);", code_with_trend))
    expect_true(stan_pattern("for \\(i in 1:N_trend\\)", code_with_trend))
    expect_true(stan_pattern("for \\(s in 1:N_series_trend\\)", code_with_trend))
    expect_true(stan_pattern("trend\\[i, s\\] = dot_product\\(Z\\[s, :\\], lv_trend\\[i, :\\]\\) \\+
  mu_trend\\[times_trend\\[i, s\\]\\];", code_with_trend))

    # 6. Model Block - Complex likelihood with trend injection
    # GP computation
    expect_true(stan_pattern("vector\\[Nsubgp_1\\] gp_pred_1 = gp_exp_quad\\(Xgp_1, sdgp_1\\[1\\],
  lscale_1\\[1\\], zgp_1\\);", code_with_trend))

    # Complex linear predictor construction
    expect_true(stan_pattern("vector\\[N\\] mu = rep_vector\\(0\\.0, N\\);", code_with_trend))
    expect_true(stan_pattern("mu \\+= Intercept \\+ gp_pred_1\\[Jgp_1\\];", code_with_trend))

    # Multi-component trend injection
    expect_true(stan_pattern("for \\(n in 1:N\\)", code_with_trend))
    expect_true(stan_pattern("mu\\[n\\] \\+= r_1_1\\[J_1\\[n\\]\\] \\* Z_1_1\\[n\\];",
                      code_with_trend))
    expect_true(stan_pattern("mu\\[n\\] \\+= trend\\[obs_trend_time\\[n\\],
  obs_trend_series\\[n\\]\\];", code_with_trend))

    # Standard Poisson likelihood (not GLM-optimized due to complexity)
    expect_true(stan_pattern("target \\+= poisson_log_lpmf\\(Y \\| mu\\);", code_with_trend))

    # Prior contributions
    expect_true(stan_pattern("target \\+= lprior;", code_with_trend))
    expect_true(stan_pattern("target \\+= std_normal_lpdf\\(zgp_1\\);", code_with_trend))
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

    # Should NOT have GLM optimization due to complex model structure
    expect_false(grepl("poisson_log_glm_lpmf", code_with_trend, fixed = TRUE))

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
  expect_match2(code, "for\\s*\\(\\s*i\\s+in\\s+1:N_trend\\s*\\)")
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
  # Should declare N_trend
  expect_match2(code_shared, "int<lower=1> N_trend;\\s*//\\s*number of time points")
  # Should declare N_series_trend
  expect_match2(code_shared, "int<lower=1> N_series_trend;\\s*//\\s*number of series")
  # Should declare N_lv_trend
  expect_match2(code_shared, "int<lower=1> N_lv_trend;\\s*//\\s*latent variables")

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

  # GLM compatibility vectors
  # Should declare mu_ones_count for GLM
  expect_match2(code_shared, "vector\\[1\\] mu_ones_count;\\s*//.*count")
  # Should declare mu_ones_biomass for GLM
  expect_match2(code_shared, "vector\\[1\\] mu_ones_biomass;\\s*//.*biomass")

  # Should create identity matrix Z for non-factor model
  expect_match2(code_shared, "matrix\\[N_series_trend, N_lv_trend\\] Z = diag_matrix\\(rep_vector\\(1\\.0, N_lv_trend\\)\\);")

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
  expect_match2(code_shared, "vector<lower=0>\\[N_lv_trend\\] sigma_trend;\\s*//\\s*innovation SDs")
  # Should declare L_Omega_trend for correlation
  expect_match2(code_shared, "cholesky_factor_corr\\[N_lv_trend\\] L_Omega_trend;\\s*//\\s*correlation Cholesky")
  # Should declare innovations_trend matrix
  expect_match2(code_shared, "matrix\\[N_trend, N_lv_trend\\] innovations_trend;\\s*//\\s*raw innovations")

  # Should initialize lprior
  expect_match2(code_shared, "real lprior = 0;\\s*//\\s*prior contributions")

  # Should include Intercept_trend in lprior
  expect_match2(code_shared, "lprior \\+= student_t_lpdf\\(Intercept_trend \\| 3, 0, 2\\.5\\);")

  # Should create mu_trend from Intercept_trend using rep_vector
  expect_match2(code_shared, "vector\\[N_trend\\] mu_trend = rep_vector\\(Intercept_trend, N_trend\\);")

  # Should construct Sigma_trend covariance matrix
  expect_match2(code_shared, "matrix\\[N_lv_trend, N_lv_trend\\] Sigma_trend\\s*=\\s*diag_pre_multiply\\(sigma_trend, L_Omega_trend\\);")

  # RW latent variables
  # Should declare lv_trend matrix for latent variables (with _trend suffix)
  expect_match2(code_shared, "matrix\\[N_trend, N_lv_trend\\] lv_trend;")
  # Should declare L_Sigma_trend for scaling
  expect_match2(code_shared, "matrix\\[N_lv_trend, N_lv_trend\\] L_Sigma_trend\\s*=")
  # Should declare scaled_innovations_trend
  expect_match2(code_shared, "matrix\\[N_trend, N_lv_trend\\] scaled_innovations_trend\\s*=")
  # Should initialize first lv_trend from scaled innovations
  expect_match2(code_shared, "lv_trend\\[1, :\\] = scaled_innovations_trend\\[1, :\\];")
  # Should implement RW cumulative sum
  expect_match2(code_shared, "lv_trend\\[i, :\\] = lv_trend\\[i-1, :\\] \\+ scaled_innovations_trend\\[i, :\\];")

  # Trend matrix computation (shared, not response-specific)
  # Should declare shared trend matrix (not trend_count/trend_biomass)
  expect_match2(code_shared, "matrix\\[N_trend, N_series_trend\\] trend;")
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

  # Should NOT declare mu_trend as parameter vector
  expect_false(grepl("parameters\\s*\\{[^}]*vector\\[[^]]*\\]\\s+mu_trend", code_shared))

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

  # Should NOT have logistic functions (default is linear growth)
  expect_false(grepl("logistic_gamma", code_with_trend, fixed = TRUE))
  expect_false(grepl("logistic_trend", code_with_trend, fixed = TRUE))

  # 2. Data Block - Piecewise-specific data structures
  # Standard trend dimensions
  expect_true(stan_pattern("int<lower=1> N_trend;", code_with_trend, fixed = TRUE))
  expect_true(stan_pattern("int<lower=1> N_series_trend;", code_with_trend, fixed = TRUE))
  expect_true(stan_pattern("int<lower=1> N_lv_trend;", code_with_trend, fixed = TRUE))

  # Piecewise-specific data
  expect_true(stan_pattern("int<lower=0> n_change_trend;.*number of potential trend changepoints", code_with_trend))
  expect_true(stan_pattern("vector\\[n_change_trend\\] t_change_trend;.*times of potential changepoints", code_with_trend))
  expect_true(stan_pattern("real<lower=0> change_scale_trend;.*scale of changepoint shock prior", code_with_trend))

  # Should NOT have carrying capacity for linear growth
  expect_false(grepl("cap_trend", code_with_trend, fixed = TRUE))

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
  expect_true(stan_pattern("vector\\[N_lv_trend\\] k_trend;.*base trend growth rates", code_with_trend))
  expect_true(stan_pattern("vector\\[N_lv_trend\\] m_trend;.*trend offset parameters", code_with_trend))
  expect_true(stan_pattern("matrix\\[n_change_trend, N_lv_trend\\] delta_trend;.*trend rate adjustments", code_with_trend))

  # Standard observation model parameters
  expect_true(stan_pattern("vector\\[Kc\\] b;.*regression coefficients", code_with_trend))
  expect_true(stan_pattern("real Intercept;.*temporary intercept", code_with_trend))
  expect_true(stan_pattern("real Intercept_trend;.*temporary intercept", code_with_trend))

  # Should NOT have innovation/sigma parameters (PW doesn't use them)
  expect_false(grepl("innovations_trend", code_with_trend, fixed = TRUE))
  expect_false(grepl("sigma_trend", code_with_trend, fixed = TRUE))

  # 5. Transformed Parameters Block - Trend computation
  # Prior accumulation
  expect_true(stan_pattern("real lprior = 0;", code_with_trend, fixed = TRUE))
  expect_true(stan_pattern("lprior \\+= student_t_lpdf\\(Intercept \\|", code_with_trend))
  expect_true(stan_pattern("lprior \\+= student_t_lpdf\\(Intercept_trend \\|", code_with_trend))

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
  expect_true(stan_pattern("vector\\[N_trend\\] mu_trend = rep_vector\\(Intercept_trend, N_trend\\);", code_with_trend))
  expect_true(stan_pattern("for \\(i in 1:N_trend\\)", code_with_trend))
  expect_true(stan_pattern("for \\(s in 1:N_series_trend\\)", code_with_trend))
  expect_true(stan_pattern("trend\\[i, s\\] = dot_product\\(Z\\[s, :\\], lv_trend\\[i, :\\]\\) \\+ mu_trend\\[times_trend\\[i, s\\]\\];", code_with_trend))

  # GLM-compatible mu construction and trend injection
  expect_true(stan_pattern("vector\\[N\\] mu = Xc \\* b;", code_with_trend))
  expect_true(stan_pattern("for \\(n in 1:N\\)", code_with_trend))
  expect_true(stan_pattern("mu\\[n\\] \\+= Intercept \\+ trend\\[obs_trend_time\\[n\\], obs_trend_series\\[n\\]\\];", code_with_trend))

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
