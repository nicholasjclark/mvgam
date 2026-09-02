# stancode and standata Tests
#
# Integrated tests for stancode.mvgam_formula() and
# standata.mvgam_formula()

# Test Helper Functions ----

#' Match a pattern against generated Stan code
#'
#' Strips whitespace from both the pattern and the program before
#' matching, so an assertion names the tokens a generator emits
#' rather than the layout the polish step gives them. The two
#' spellings of a sampling statement are accepted interchangeably.
#'
#' @param pattern Regular expression, or a literal when the caller
#'   passes `fixed = TRUE`. Must escape its own `(`, `[` and `|`
#'   otherwise.
#' @param x Character vector where matches are sought
#' @param ignore.case Logical. If TRUE, case is ignored (default FALSE)
#' @param ... Additional arguments passed to grepl(), `fixed = TRUE`
#'   among them
#'
#' @return Logical vector indicating matches
#' @noRd
stan_pattern <- function(pattern, x, ignore.case = FALSE, ...) {
  remove_whitespace <- function(text) {
    gsub("\\s+", "", text)
  }

  # Whitespace goes from both sides, so a pattern is written against
  # the program's tokens rather than its layout and survives a
  # reflow of the generated code. `\\s*` between two tokens is
  # therefore redundant and `\\s+` can never match.
  x_no_space <- remove_whitespace(x)

  # The pattern is a regular expression, and one naming Stan syntax
  # has to escape its own metacharacters. Whether a caller meant a
  # literal cannot be told from the string itself, and a wrong guess
  # either drops an escape or doubles it, so a caller wanting a
  # literal says so with `fixed = TRUE` and the contract is stated
  # rather than inferred.
  pattern_final <- remove_whitespace(pattern)

  # A sampling statement has two spellings. `lhs ~ dist(args);` is
  # what an emitter writes, and a normalised program carries
  # `target += dist_lpdf(lhs | args);` instead. A test naming one
  # should accept the other, so the assertions stay about which
  # prior reached which parameter rather than about which form the
  # program happens to use.
  #
  # The alternation is a regular expression, so it is only built
  # when one will be read as such. Under `fixed = TRUE` grepl would
  # search for the alternation's own punctuation and match nothing.
  dots <- list(...)
  if (!isTRUE(dots$fixed)) {
    alt <- tilde_pattern_as_lpdf(pattern_final)
    if (!is.na(alt)) {
      pattern_final <- paste0("(", pattern_final, ")|(", alt, ")")
    }
  }

  # Apply grepl with processed pattern and whitespace-free input
  grepl(pattern_final, x_no_space, ignore.case = ignore.case, ...)
}


# Translate a whitespace-free `lhs~dist(args)` pattern into the
# density-call spelling. Returns NA when the pattern is not a
# sampling statement, so the caller leaves it alone.
tilde_pattern_as_lpdf <- function(pattern) {
  if (!grepl("~", pattern, fixed = TRUE)) return(NA_character_)
  # `strsplit()` drops a trailing empty field, so the two sides are
  # taken directly rather than by splitting.
  lhs <- sub("~.*$", "", pattern)
  rhs <- sub("^[^~]*~", "", pattern)
  if (!nzchar(lhs)) return(NA_character_)
  # A pattern naming no distribution asserts only that the
  # parameter is sampled. The density call and any container
  # wrapping the parameter, such as `diagonal(...)`, are left open.
  if (!nzchar(rhs)) {
    # The parameter has to be the density's first operand. Allowing
    # anything before it would match the trend equation, where the
    # same name appears among the arguments, and the assertion would
    # pass with the prior deleted.
    return(paste0("_lpdf\\((?:[a-z_]*\\()?", lhs, "[|)]"))
  }
  open <- regexpr("\\\\?\\(", rhs)
  if (open > 0L) {
    dist <- substr(rhs, 1L, open - 1L)
    args <- substr(rhs, open, nchar(rhs))
    args <- sub("^\\\\?\\(", "", args)
    args <- sub("\\\\?\\);?$", "", args)
    if (!nzchar(args)) {
      return(paste0("target\\+=", dist, "_lpdf\\(", lhs, "\\)"))
    }
    paste0("target\\+=", dist, "_lpdf\\(", lhs, "\\|", args, "\\)")
  } else {
    paste0("target\\+=", rhs, "_lpdf\\(", lhs, "\\|")
  }
}

# Test Data Setup ----

#' Create standardized test datasets for stancode/standata function testing
#' @noRd
setup_stan_test_data <- function() {
  set.seed(42)
  n_time <- 24
  n_series <- 4

  # Simple univariate dataset
  univariate <- data.frame(
    time = 1:n_time,
    series = factor(rep("series1", n_time)),
    y = rpois(n_time, lambda = 5),
    x = rnorm(n_time),
    temperature = rnorm(n_time, mean = 15, sd = 3),
    site = factor(rep(c("A", "B", "C"), length.out = n_time))
  )

  # Multivariate dataset with balanced design. habitat is assigned
  # deterministically per series so it can be used as a hierarchical
  # grouping variable (constant within each series). Trend-level
  # covariates (x, presence) are generated per (time, habitat) so they
  # satisfy the trend invariance check used by hierarchical trends.
  series_levels <- paste0("series", 1:n_series)
  series_to_habitat <- setNames(
    rep(c("forest", "grassland"), length.out = n_series),
    series_levels
  )
  series_col <- factor(rep(series_levels, each = n_time),
                        levels = series_levels)
  habitat_col <- factor(series_to_habitat[as.character(series_col)],
                         levels = c("forest", "grassland"))
  th_grid <- expand.grid(
    time = seq_len(n_time),
    habitat = factor(c("forest", "grassland"))
  )
  th_grid$x <- rnorm(nrow(th_grid))
  th_grid$presence <- rbinom(nrow(th_grid), 1, 0.7)
  th_lookup <- setNames(
    seq_len(nrow(th_grid)),
    paste(th_grid$time, th_grid$habitat, sep = "_")
  )
  obs_keys <- paste(rep(seq_len(n_time), n_series),
                     as.character(habitat_col), sep = "_")
  obs_idx <- th_lookup[obs_keys]
  multivariate <- data.frame(
    time = rep(seq_len(n_time), n_series),
    series = series_col,
    count = rpois(n_time * n_series, lambda = 4),
    biomass = rlnorm(n_time * n_series, meanlog = 1, sdlog = 0.5),
    presence = th_grid$presence[obs_idx],
    x = th_grid$x[obs_idx],
    habitat = habitat_col
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
  code_obs_only <- stancode(mf_obs_only, data = data, family = poisson(), validate = TRUE)

  # Check class structure follows mvgam convention with brms compatibility
  expect_s3_class(code_obs_only, "mvgamstancode")
  expect_s3_class(code_obs_only, "stancode")
  expect_s3_class(code_obs_only, "character")
  expect_equal(class(code_obs_only), c("mvgamstancode", "stancode", "character"))

  # Model with trends - generate without validation first
  mf_with_trend <- mvgam_formula(y ~ x, trend_formula = ~ RW())
  code_with_trend <- stancode(mf_with_trend, data = data, family = poisson(), validate = TRUE)

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
  expect_true(stan_pattern("array\\[N_time_trend, N_series_trend\\] int times_trend;", code_with_trend))

  # Factor loading matrix for non-factor models
  expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z = diag_matrix", code_with_trend))

  # RW-specific innovation structure
  expect_true(stan_pattern("matrix\\[N_time_trend, N_lv_trend\\] innovations_trend;", code_with_trend))
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
    validate = TRUE
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
  expect_true(stan_pattern("for \\(i in 13:N_time_trend\\)", code_with_trend))

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
  expect_true(stan_pattern("matrix\\[N_time_trend, N_lv_trend\\] innovations_trend;", code_with_trend))
  expect_true(stan_pattern("vector<lower=0>\\[N_lv_trend\\] sigma_trend;", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_time_trend, N_lv_trend\\] lv_trend;", code_with_trend))

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
    validate = TRUE
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
  expect_true(stan_pattern("matrix\\[N_time_trend, N_lv_trend\\] ma_innovations_trend = scaled_innovations_trend;", code_with_trend))

  # MA transformation should be applied to the entire matrix first
  expect_true(stan_pattern("for \\(i in 2:N_time_trend\\)", code_with_trend))
  expect_true(stan_pattern("ma_innovations_trend\\[i, j\\] \\+= theta1_trend\\[j\\] \\* ma_innovations_trend\\[i-1, j\\];", code_with_trend))

  # Initialization: First 4 time points should use MA innovations
  expect_true(stan_pattern("for \\(i in 1:4\\)", code_with_trend))
  expect_true(stan_pattern("lv_trend\\[i, :\\] = ma_innovations_trend\\[i, :\\];", code_with_trend))

  # AR dynamics: Should start from time point 5 and use ma_innovations_trend
  expect_true(stan_pattern("for \\(i in 5:N_time_trend\\)", code_with_trend))

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
  expect_true(stan_pattern("matrix\\[N_time_trend, N_lv_trend\\] innovations_trend;", code_with_trend))
  expect_true(stan_pattern("vector<lower=0>\\[N_lv_trend\\] sigma_trend;", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_time_trend, N_lv_trend\\] lv_trend;", code_with_trend))

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

})

test_that("stancode generates correct VAR(p = 2, ma = TRUE) VARMA model with tensor product smooths and presence covariate", {
  data <- setup_stan_test_data()$multivariate
  mf_with_trend <- mvgam_formula(
    bf(mvbind(count, biomass) ~ t2(x, time)) + set_rescor(FALSE),
    trend_formula = ~ presence + VAR(p = 2, ma = TRUE)
  )
  code_with_trend <- stancode(
    mf_with_trend, data = data,
    validate = TRUE
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
  expect_true(stan_pattern("array\\[N_time_trend, N_series_trend\\] int times_trend;", code_with_trend))

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
  expect_true(stan_pattern("matrix\\[N_time_trend, N_lv_trend\\] lv_trend;", code_with_trend))

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

  # `mu_t_trend` must be declared with Stan >= 2.32 array syntax.
  # Pre-2.32 form `vector[N_lv_trend] mu_t_trend[N_time_trend];`
  # compiles via the stancode polisher but breaks when the same
  # code path is sent to brms/cmdstanr from `mvgam()` directly.
  expect_true(stan_pattern(
    "array\\[N_time_trend\\] vector\\[N_lv_trend\\] mu_t_trend;",
    code_with_trend
  ))
  expect_false(stan_pattern(
    "vector\\[N_lv_trend\\] mu_t_trend\\[N_time_trend\\];",
    code_with_trend
  ))

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
      validate = TRUE
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
    expect_true(stan_pattern("array\\[N_time_trend, N_series_trend\\] int times_trend;",
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

    # Factor loading parameter: unconstrained Z matrix sampled
    # directly; QR identification handled in generated quantities.
    expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z;",
                      code_with_trend))

    # Innovation matrix
    expect_true(stan_pattern("matrix\\[N_time_trend, N_lv_trend\\] innovations_trend;",
                      code_with_trend))

    # lprior initialization with family-specific priors
    expect_true(stan_pattern("real lprior = 0;", code_with_trend))

    # Innovation covariance construction
    expect_true(stan_pattern("matrix\\[N_lv_trend, N_lv_trend\\] L_Sigma_trend =
  diag_pre_multiply\\(sigma_trend, L_Omega_trend\\);", code_with_trend))
    expect_true(stan_pattern("scaled_innovations_trend = innovations_trend \\* L_Sigma_trend';", code_with_trend))

    # AR(1) latent variable dynamics
    expect_true(stan_pattern("matrix\\[N_time_trend, N_lv_trend\\] lv_trend;", code_with_trend))
    expect_true(stan_pattern("lv_trend\\[i,:\\] = scaled_innovations_trend\\[i,:\\];",
                      code_with_trend))
    expect_true(stan_pattern("for \\(i in 2:N_time_trend\\)", code_with_trend))
    expect_true(stan_pattern("for \\(j in 1:N_lv_trend\\)", code_with_trend))
    expect_true(stan_pattern("lv_trend\\[i,j\\] = ar1_trend\\[j\\] \\* lv_trend\\[i-1,j\\] \\+ scaled_innovations_trend\\[i,j\\];", code_with_trend))

    # Zero trend mean vector (for ~ -1 specification)
    expect_true(stan_pattern("vector\\[N_trend\\] mu_trend = rep_vector\\(0\\.0, N_trend\\);",
                      code_with_trend))

    # Universal trend computation pattern
    expect_true(stan_pattern("matrix\\[N_time_trend, N_series_trend\\] trend;", code_with_trend))
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
    expect_true(stan_pattern("to_vector\\(Z\\) ~ student_t\\(3, 0, 0.5\\);",
                      code_with_trend))
    expect_true(stan_pattern("to_vector\\(innovations_trend\\) ~", code_with_trend))

    # Post-hoc QR identification in generated quantities
    expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z_tilde = qr_thin_R\\(Z'\\)';",
                      code_with_trend))
    expect_true(stan_pattern("matrix\\[N_lv_trend, N_lv_trend\\] Q_tilde = qr_thin_Q\\(Z'\\)';",
                      code_with_trend))
    expect_true(stan_pattern("matrix\\[N_time_trend, N_lv_trend\\] lv_trend_tilde = lv_trend \\* Q_tilde';",
                      code_with_trend))

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
    validate = TRUE
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
  expect_true(stan_pattern("array\\[N_time_trend, N_series_trend\\] int times_trend;", code_with_trend))

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

  # Factor loading parameter: unconstrained Z matrix sampled
  # directly; QR identification handled in generated quantities.
  expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z;", code_with_trend))

  # Innovation matrix
  expect_true(stan_pattern("matrix\\[N_time_trend, N_lv_trend\\] innovations_trend;", code_with_trend))

  # lprior initialization with family-specific priors
  expect_true(stan_pattern("real lprior = 0;", code_with_trend))

  # Innovation covariance construction
  expect_true(stan_pattern("matrix\\[N_lv_trend, N_lv_trend\\] L_Sigma_trend = diag_pre_multiply\\(sigma_trend, L_Omega_trend\\);", code_with_trend))
  expect_true(stan_pattern("scaled_innovations_trend = innovations_trend \\* L_Sigma_trend';", code_with_trend))

  # AR(1) latent variable dynamics
  expect_true(stan_pattern("matrix\\[N_time_trend, N_lv_trend\\] lv_trend;", code_with_trend))
  expect_true(stan_pattern("lv_trend\\[i,:\\] = scaled_innovations_trend\\[i,:\\];", code_with_trend))
  expect_true(stan_pattern("for \\(i in 2:N_time_trend\\)", code_with_trend))
  expect_true(stan_pattern("for \\(j in 1:N_lv_trend\\)", code_with_trend))
  expect_true(stan_pattern("lv_trend\\[i,j\\] = ar1_trend\\[j\\] \\* lv_trend\\[i-1,j\\] \\+ scaled_innovations_trend\\[i,j\\];", code_with_trend))

  # Zero trend mean vector (for ~ -1 specification)
  expect_true(stan_pattern("vector\\[N_trend\\] mu_trend = rep_vector\\(0\\.0, N_trend\\);", code_with_trend))

  # Universal trend computation pattern
  expect_true(stan_pattern("matrix\\[N_time_trend, N_series_trend\\] trend;", code_with_trend))
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
  expect_true(stan_pattern("to_vector\\(Z\\) ~ student_t\\(3, 0, 0.5\\);", code_with_trend))
  expect_true(stan_pattern("to_vector\\(innovations_trend\\) ~", code_with_trend))

  # Post-hoc QR identification in generated quantities
  expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z_tilde = qr_thin_R\\(Z'\\)';", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_lv_trend, N_lv_trend\\] Q_tilde = qr_thin_Q\\(Z'\\)';", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_time_trend, N_lv_trend\\] lv_trend_tilde = lv_trend \\* Q_tilde';", code_with_trend))

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
    validate = TRUE
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
  # Unconstrained Z matrix; QR identification in generated quantities.
  expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z;", code_with_trend))

  # Post-hoc QR identification in generated quantities
  expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z_tilde = qr_thin_R\\(Z'\\)';", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_lv_trend, N_lv_trend\\] Q_tilde = qr_thin_Q\\(Z'\\)';", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_time_trend, N_lv_trend\\] lv_trend_tilde = lv_trend \\* Q_tilde';", code_with_trend))

  # ZMVN dynamics (just scaled innovations, no complex dynamics)
  expect_true(stan_pattern("lv_trend = scaled_innovations_trend;", code_with_trend))

  # Trend mean with covariate effects
  expect_true(stan_pattern("mu_trend \\+= X_trend \\* b_trend;", code_with_trend))

  # Universal trend computation pattern should still be present
  expect_true(stan_pattern("trend\\[i, s\\] = dot_product\\(Z\\[s, :\\], lv_trend\\[i, :\\]\\) \\+ mu_trend\\[times_trend\\[i, s\\]\\]", code_with_trend))

  # Observation model priors (brms pattern). The intercept prior
  # location is data-driven (median of the response, computed by brms),
  # so we don't pin it.
  expect_true(stan_pattern("lprior \\+= student_t_lpdf\\(Intercept \\| 3, [^,]+, 2\\.5\\);", code_with_trend))
  expect_true(stan_pattern("lprior \\+= student_t_lpdf\\(sigma \\| 3, 0, 2\\.5\\)", code_with_trend))
  expect_true(stan_pattern("- 1 \\* student_t_lccdf\\(0 \\| 3, 0, 2\\.5\\);", code_with_trend))

  # Trend parameter priors
  expect_true(stan_pattern("sigma_trend ~ exponential\\(2\\);", code_with_trend))
  expect_true(stan_pattern("to_vector\\(Z\\) ~ student_t\\(3, 0, 0.5\\);", code_with_trend))

  # `b_trend` keeps the flat brms default, so the program places no
  # prior on it in either spelling. Reading the program's priors by
  # parameter keeps that true of a normalised program too, where
  # nothing is written with a tilde.
  expect_length(stan_prior_on(code_with_trend, "b_trend"), 0L)

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
    validate = TRUE
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
  # has both fixed effects and random effects, even when brms hides the
  # fixed effects inside normal_id_glm_lpdf's Xc*b argument.
  expect_true(stan_pattern("mu_trend \\+= X_trend \\* b_trend", code_with_trend))

  # Hierarchical correlation priors with _trend suffix
  expect_true(stan_pattern("L_Omega_global_trend ~ lkj_corr_cholesky\\(1\\);", code_with_trend))
  expect_true(stan_pattern(paste0(
    "for \\(g_idx in 1:N_groups_trend\\) \\{",
    "target \\+= lkj_corr_cholesky_lpdf\\(",
    "L_deviation_group_trend\\[g_idx\\] \\| 6\\); \\}"
  ), code_with_trend))

  # Custom alpha_cor_trend prior should be applied
  expect_true(stan_pattern("alpha_cor_trend ~ beta\\(5, 5\\);", code_with_trend))

  # Group-specific sigma priors and innovation priors
  expect_true(stan_pattern(paste0(
    "exponential_lpdf\\(to_vector\\(sigma_group_trend",
    "\\[g_idx\\]\\) \\| 2\\);"
  ), code_with_trend))
  expect_true(stan_pattern("to_vector\\(innovations_trend\\) ~ std_normal\\(\\);", code_with_trend))

  # `b_trend` keeps the flat brms default, so the program places no
  # prior on it in either spelling. Reading the program's priors by
  # parameter keeps that true of a normalised program too, where
  # nothing is written with a tilde.
  expect_length(stan_prior_on(code_with_trend, "b_trend"), 0L)

  # Observation model priors (brms pattern)
  # Intercept prior location is data-driven; don't pin the value.
  expect_true(stan_pattern("lprior \\+= student_t_lpdf\\(Intercept \\| 3, [^,]+, 2\\.5\\);", code_with_trend))
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

})

test_that("RW(gr = habitat) emits hierarchical Stan and wires scaled_innovations_trend assignment", {
  # generate_rw_trend_stanvars() must call add_hierarchical_support(),
  # the same call that AR/ZMVN use, so that when a user supplies gr=
  # the hierarchical correlation system fills
  # scaled_innovations_trend[t, s] in the tparameters block before the
  # RW recurrence consumes it. Without that call, the shared
  # innovation system would emit a declaration-only branch and the RW
  # recurrence would read scaled_innovations_trend with no upstream
  # assignment, producing NaN at Stan init.
  data <- setup_stan_test_data()$multivariate
  mf <- mvgam_formula(count ~ 1, trend_formula = ~ RW(gr = habitat))

  code <- stancode(mf, data = data, family = poisson(), validate = FALSE)

  # Hierarchical data structures wired in
  expect_true(stan_pattern("int<lower=1> N_groups_trend;", code, fixed = TRUE))
  expect_true(stan_pattern("int<lower=1> N_subgroups_trend;", code, fixed = TRUE))
  expect_true(stan_pattern(
    "array\\[N_series_trend\\] int<lower=1> group_inds_trend;", code))

  # Hierarchical parameters present (the same ones AR/ZMVN gr= emit)
  expect_true(stan_pattern(
    "cholesky_factor_corr\\[N_subgroups_trend\\] L_Omega_global_trend;", code))
  expect_true(stan_pattern(
    "array\\[N_groups_trend\\] vector<lower=0>\\[N_subgroups_trend\\] sigma_group_trend;",
    code))
  expect_true(stan_pattern("real<lower=0, upper=1> alpha_cor_trend;", code))

  # The hierarchical assignment loop must populate
  # scaled_innovations_trend before the RW recurrence reads it.
  expect_true(stan_pattern(
    "scaled_innovations_trend\\[t, s\\] = scaled\\[k\\];", code))

  # The flat direct-assignment branch must NOT fire for the hierarchical
  # path (mutually exclusive with the hierarchical assignment).
  expect_false(grepl(
    "scaled_innovations_trend = innovations_trend \\* diag_matrix\\(sigma_trend\\)",
    code))

  # RW recurrence reads scaled_innovations_trend
  expect_true(stan_pattern(
    "lv_trend\\[1,\\s*:\\s*\\] = scaled_innovations_trend\\[1,\\s*:\\s*\\]",
    code))
  expect_true(stan_pattern(
    "lv_trend\\[i,\\s*:\\s*\\] = lv_trend\\[i - 1,\\s*:\\s*\\]\\s*\\+ scaled_innovations_trend\\[i,\\s*:\\s*\\]",
    code))
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
    validate = TRUE
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
  # Group-series index buffer must use modern Stan array syntax;
  # the legacy `int group_series[N];` form parses under rstan but
  # is a hard error under cmdstanr (Stan >= 2.32).
  expect_true(stan_pattern(
    "array\\[N_subgroups_trend\\] int group_series;",
    code_with_trend
  ))
  expect_false(grepl(
    "int group_series\\[N_subgroups_trend\\];",
    code_with_trend,
    fixed = FALSE
  ))

  # Heaps transformation for stationarity (VAR-specific)
  expect_true(stan_pattern("array\\[1\\] matrix\\[N_subgroups_trend, N_subgroups_trend\\] P_group;", code_with_trend))
  expect_true(stan_pattern("P_group\\[1\\] = AtoP\\(A_raw_group_trend\\[g_idx, lag\\]\\);", code_with_trend))

  # Default prior application (VAR should use default beta(3, 2))
  expect_true(stan_pattern("alpha_cor_trend ~ beta\\(3, 2\\);", code_with_trend))

  # VAR coefficient and sigma priors (uses g_idx consistently)
  expect_true(stan_pattern(paste0(
    "normal_lpdf\\(diagonal\\(A_raw_group_trend",
    "\\[g_idx, lag\\]\\) \\|"
  ), code_with_trend))
  expect_true(stan_pattern(paste0(
    "exponential_lpdf\\(to_vector\\(sigma_group_trend",
    "\\[g_idx\\]\\) \\| 2\\);"
  ), code_with_trend))

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

})


test_that("trend codegen emits modern Stan array syntax across every branch", {
  # Regression guard: Stan >= 2.32 (cmdstanr default) rejects the
  # legacy `int name[N];` form, but `rstan::stanc()` still accepts
  # it with only a deprecation warning. That means
  # `validate_stan_code(backend = "rstan")` (used by the rest of
  # this file) can pass stancode that fails at sample time. Sweep
  # the generated Stan for every trend-family / cor combination
  # mvgam ships and assert that no declaration uses the legacy
  # form. Comment lines (`//`) are excluded so prose like
  # `int_pattern[1]` in roxygen-style comments cannot trip the
  # check.
  data <- setup_stan_test_data()$multivariate
  trend_combos <- list(
    list(label = "VAR p=1 cor",
         tf = ~ 1 + VAR(p = 1, cor = TRUE)),
    list(label = "VAR p=1 gr cor",
         tf = ~ 1 + VAR(p = 1, gr = habitat, cor = TRUE)),
    list(label = "AR p=1 cor",
         tf = ~ 1 + AR(p = 1, cor = TRUE)),
    list(label = "AR p=1 gr cor",
         tf = ~ 1 + AR(p = 1, gr = habitat, cor = TRUE)),
    list(label = "RW cor",
         tf = ~ 1 + RW(cor = TRUE)),
    list(label = "ZMVN",
         tf = ~ 1 + ZMVN())
  )
  legacy_decl <- paste0(
    "(int|real|matrix|vector|cov_matrix|corr_matrix|",
    "cholesky_factor_corr|cholesky_factor_cov|simplex|",
    "unit_vector|ordered|positive_ordered)\\s+",
    "[A-Za-z_][A-Za-z0-9_]*\\["
  )
  for (combo in trend_combos) {
    mf <- mvgam_formula(count ~ 1 + x, trend_formula = combo$tf)
    code <- as.character(stancode(
      mf, data = data, family = poisson(), validate = TRUE
    ))
    lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
    code_only <- lines[!grepl("^\\s*//", lines)]
    offenders <- grep(legacy_decl, code_only, value = TRUE)
    expect_equal(
      length(offenders), 0L,
      label = paste0("trend = ", combo$label, " emits legacy array form")
    )
  }
})


test_that("user priors on array-shaped VAR hyperparameters emit per-lag", {
  # Guards user-prior routing through `generate_var_trend_stanvars()`:
  #   * `Amu_trend` / `Aomega_trend` are declared `array[2] vector[lags]`;
  #     a user override must be emitted inside the `for (lag in 1:2)` loop
  #     and must NOT also appear at top level via the centralized
  #     `var_centralized_priors` block (which would emit a scalar
  #     `Amu_trend ~ ...;` Stan rejects).
  #   * `L_Omega_global_trend` and `L_deviation_group_trend` must route
  #     any user override through `generate_hierarchical_correlation_model`
  #     rather than a hardcoded LKJ prior that would discard it.
  data <- setup_stan_test_data()$multivariate
  mf <- mvgam_formula(
    count ~ 1 + x,
    trend_formula = ~ VAR(p = 1, gr = habitat, cor = TRUE)
  )
  custom <- c(
    brms::prior(normal(0, 0.3), class = "Amu_trend"),
    brms::prior(gamma(2, 0.5), class = "Aomega_trend"),
    brms::prior(lkj_corr_cholesky(2), class = "L_Omega_global_trend"),
    brms::prior(lkj_corr_cholesky(4), class = "L_deviation_group_trend")
  )
  code <- as.character(stancode(
    mf, data = data, family = poisson(), prior = custom, validate = TRUE
  ))
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  code_only <- lines[!grepl("^\\s*//", lines)]

  amu_lines <- grep("Amu_trend\\[lag\\]\\s*[~|]", code_only, value = TRUE)
  expect_equal(length(amu_lines), 1L)
  expect_match(amu_lines, "Amu_trend\\[lag\\]\\s*[~|]\\s*(normal\\()?0,\\s*0.3")

  aomega_lines <- grep("Aomega_trend\\[lag\\]\\s*[~|]", code_only, value = TRUE)
  expect_equal(length(aomega_lines), 1L)
  expect_match(aomega_lines, "Aomega_trend\\[lag\\]\\s*[~|]\\s*(gamma\\()?2,\\s*0.5")

  l_global_lines <- grep("L_Omega_global_trend\\s*[~|]", code_only, value = TRUE)
  expect_equal(length(l_global_lines), 1L)
  expect_match(l_global_lines,
                 "lkj_corr_cholesky\\(2\\)|lkj_corr_cholesky_lpdf\\(.*\\| ?2\\)")

  l_dev_lines <- grep("L_deviation_group_trend\\[g_idx\\]\\s*[~|]",
                      code_only, value = TRUE)
  expect_equal(length(l_dev_lines), 1L)
  expect_match(l_dev_lines,
                 "lkj_corr_cholesky\\(4\\)|lkj_corr_cholesky_lpdf\\(.*\\| ?4\\)")
})


test_that("user priors on array-shaped VARMA MA hyperparameters emit", {
  # Regression guard for user-supplied `Dmu_trend` / `Domega_trend`
  # priors on VARMA(p, q) fits. `Dmu_trend` / `Domega_trend` are
  # declared `array[2] vector[ma_lags]`; the user override must reach
  # the emitter and end up on the two scalar target rows
  # (`Dmu_trend[1, 1] ~ ...`, `Dmu_trend[2, 1] ~ ...`) so higher-order
  # ma_lags stay ready to plug in once the current ma_lags = 1 cap
  # lifts. Compile is verified via stancode `validate = TRUE`.
  data <- setup_stan_test_data()$multivariate
  mf <- mvgam_formula(
    count ~ 1 + x,
    trend_formula = ~ VAR(p = 1, ma = TRUE)
  )
  custom <- c(
    brms::prior(normal(0, 0.4), class = "Dmu_trend"),
    brms::prior(gamma(3, 0.75), class = "Domega_trend")
  )
  code <- as.character(stancode(
    mf, data = data, family = poisson(),
    prior = custom, validate = TRUE
  ))
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  code_only <- lines[!grepl("^\\s*//", lines)]

  # Either spelling: `Dmu_trend[1, 1] ~ dist(...)` or the density
  # call `dist_lpdf(Dmu_trend[1, 1] | ...)`.
  dmu_lines <- grep("Dmu_trend\\[[12], ?1\\]\\s*[~|]",
                     code_only, value = TRUE)
  expect_equal(length(dmu_lines), 2L)
  expect_true(all(grepl("normal(\\(|_lpdf\\(.*\\| ?)0,\\s*0.4",
                          dmu_lines)))

  dom_lines <- grep("Domega_trend\\[[12], ?1\\]\\s*[~|]",
                     code_only, value = TRUE)
  expect_equal(length(dom_lines), 2L)
  expect_true(all(grepl("gamma(\\(|_lpdf\\(.*\\| ?)3,\\s*0.75",
                          dom_lines)))
})


test_that("default priors on array-shaped VAR hyperparameters still emit per-lag", {
  data <- setup_stan_test_data()$multivariate
  mf <- mvgam_formula(
    count ~ 1 + x,
    trend_formula = ~ VAR(p = 1, gr = habitat, cor = TRUE)
  )
  code <- as.character(stancode(
    mf, data = data, family = poisson(), validate = TRUE
  ))
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  code_only <- lines[!grepl("^\\s*//", lines)]

  # Without overrides the package defaults must still surface, under the
  # same per-lag indexing, and only once.
  amu_lines <- grep("Amu_trend\\[lag\\]\\s*[~|]", code_only, value = TRUE)
  expect_equal(length(amu_lines), 1L)
  expect_match(amu_lines, paste0(
    "Amu_trend\\[lag\\]\\s*[~|]\\s*(normal\\()?0,",
    "\\s*sqrt\\(0.455\\)"
  ))

  aomega_lines <- grep("Aomega_trend\\[lag\\]\\s*[~|]", code_only, value = TRUE)
  expect_equal(length(aomega_lines), 1L)
  expect_match(aomega_lines, paste0(
    "Aomega_trend\\[lag\\]\\s*[~|]\\s*(gamma\\()?1.365,",
    "\\s*0.071175"
  ))

  l_global_lines <- grep("L_Omega_global_trend\\s*[~|]", code_only, value = TRUE)
  expect_equal(length(l_global_lines), 1L)
  expect_match(l_global_lines,
                 "lkj_corr_cholesky\\(1\\)|lkj_corr_cholesky_lpdf\\(.*\\| ?1\\)")

  l_dev_lines <- grep("L_deviation_group_trend\\[g_idx\\]\\s*[~|]",
                      code_only, value = TRUE)
  expect_equal(length(l_dev_lines), 1L)
  expect_match(l_dev_lines,
                 "lkj_corr_cholesky\\(6\\)|lkj_corr_cholesky_lpdf\\(.*\\| ?6\\)")
})


test_that("AR(p=1) latent state at t=1 uses stationary marginal init", {
  # The implied prior on lv_trend[1, j] should be
  # Normal(0, sigma_trend[j] / sqrt(1 - ar1_trend[j]^2)) (AR(1)
  # stationary marginal), NOT Normal(0, sigma_trend[j]) (a single
  # innovation). Dividing the t=1 scaled innovation by
  # sqrt(1 - square(ar1_trend[j])) makes the marginal variance at
  # the first time point match the stationary variance.
  data <- setup_stan_test_data()$multivariate
  mf <- mvgam_formula(count ~ 1 + x, trend_formula = ~ AR(p = 1))
  code <- as.character(stancode(
    mf, data = data, family = poisson(), validate = TRUE
  ))
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  init_lines <- grep("lv_trend\\[1, j\\]", lines, value = TRUE)
  expect_true(any(grepl(
    "lv_trend\\[1, j\\]\\s*=\\s*scaled_innovations_trend\\[1, j\\]",
    init_lines
  )))
  expect_true(any(grepl(
    "sqrt\\(1\\s*-\\s*square\\(ar1_trend\\[j\\]\\)\\)", lines
  )))
})


test_that("AR(p>1) / AR with MA / AR with cor keep innovation-only init", {
  # The AR(p=1) stationary marginal correction in
  # `generate_ar_trend_stanvars()` applies only to AR(p=1) WITHOUT
  # MA and WITHOUT cross-series correlation. The three families
  # outside that scope would each need a different stationary
  # covariance:
  #
  #   * AR(p>1)            : Yule-Walker for AR(p) (only in VAR
  #                          generator today via `initial_joint_var`)
  #   * AR(p=1, ma=TRUE)   : ARMA(1, 1) stationary variance
  #                          involves the MA coefficient
  #   * AR(p=1, cor=TRUE)  : joint stationary covariance with
  #                          off-diagonal
  #                          sigma[i]*sigma[j]*Omega[i, j] /
  #                          (1 - ar1[i]*ar1[j])
  #
  # All three must keep the per-lag innovation-only initialisation
  # rather than accept a partial AR(1)-style correction.
  data <- setup_stan_test_data()$multivariate
  for (tf in list(
    ~ AR(p = 2),
    ~ AR(p = 1, ma = TRUE),
    ~ AR(p = 1, cor = TRUE)
  )) {
    mf <- mvgam_formula(count ~ 1 + x, trend_formula = tf)
    code <- as.character(stancode(
      mf, data = data, family = poisson(), validate = TRUE
    ))
    lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
    expect_false(any(grepl(
      "sqrt\\(1\\s*-\\s*square\\(ar1_trend", lines
    )))
    expect_true(any(grepl(
      "lv_trend\\[i,\\s*:\\s*\\]\\s*=\\s*(scaled_|ma_)innovations_trend",
      lines
    )))
  }
})


test_that("threads + trend + brms-native family compiles serially with one warning", {
  # brms threading moves both the `mu` declaration and every
  # `mu += ...` / `mu[n] = ...` assignment into
  # `partial_log_lik_lpmf` inside `functions {}`; mvgam's obs-side
  # trend injector at R/stan_assembly.R:1346 / :1571 only searches
  # `model {}` and so cannot find the assignment it needs to splice
  # the trend addition into. The combination must therefore compile
  # serially and emit a one-time warning.
  data <- setup_stan_test_data()$multivariate
  mf <- mvgam_formula(count ~ 1 + x, trend_formula = ~ AR(p = 1))

  # The TESTTHAT env-var suppresses mvgam soft-warnings by design,
  # so flip it off locally to surface the warning class.
  withr::local_envvar(TESTTHAT = "")
  expect_warning(
    code <- as.character(stancode(
      mf, data = data, family = poisson(),
      threads = 2L, validate = FALSE
    )),
    class = "mvgam_threads_trend_brms_native"
  )
  expect_equal(
    sum(grepl("reduce_sum", strsplit(code, "\n", fixed = TRUE)[[1]])),
    0L
  )
})


test_that("threads + brms-native + no trend still emits reduce_sum", {
  data <- setup_stan_test_data()$univariate
  mf <- mvgam_formula(y ~ x)
  code <- as.character(stancode(
    mf, data = data, family = gaussian(),
    threads = 2L, validate = FALSE
  ))
  expect_true(any(grepl(
    "reduce_sum\\(partial_log_lik_lpmf,",
    strsplit(code, "\n", fixed = TRUE)[[1]]
  )))
})


test_that("closure-unit + threads still emits mvgam reduce_sum, no brms wrapper", {
  # Regression guard for the second threading bug: brms's
  # `partial_log_lik_lpmf` cannot see the transformed-data args
  # mvgam's closure-unit lpmfs need (`visit_idx`, `log_n_lookup`),
  # so we must suppress brms's wrapper for closure-unit families
  # while preserving mvgam's own `partial_sum_<family>_lpmf` +
  # `reduce_sum` call. `cpp_options$stan_threads = TRUE` is enabled
  # by `mvgam_single()` on a separate code path, so the inner
  # `reduce_sum` still parallelises at fit time.
  set.seed(1)
  d <- data.frame(
    series = factor(rep(seq_len(6), each = 3)),
    time   = rep(1L, 18),
    visit  = rep(1:3, 6),
    y      = pmin(rpois(18, 5), 20L),
    cap    = rep(20L, 18),
    elev   = rep(rnorm(6), each = 3)
  )
  mf <- mvgam_formula(y ~ elev)
  code <- as.character(stancode(
    mf, data = d, family = nmix("poisson_binomial"),
    threads = 2L, validate = TRUE
  ))
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  expect_equal(sum(grepl("partial_log_lik_lpmf", lines)), 0L)
  expect_true(any(grepl("partial_sum_nmix_lpmf", lines)))
  expect_true(any(grepl(
    "reduce_sum\\(\\s*partial_sum_nmix_lpmf,", lines
  )))
})


test_that("suppress_brms_threading predicate covers all three triggers", {
  mv_spec_with_trends <- list(has_trends = TRUE)
  mv_spec_no_trends   <- list(has_trends = FALSE)

  # Trigger 1: brms-native + trend_formula.
  expect_true(suppress_brms_threading(
    2L, gaussian(), mv_spec_with_trends
  ))

  # Trigger 2: closure-unit (with or without trend).
  expect_true(suppress_brms_threading(
    2L, nmix("poisson_binomial"), mv_spec_with_trends
  ))
  expect_true(suppress_brms_threading(
    2L, occ(), mv_spec_no_trends
  ))

  # Trigger 3: multi-response (with or without trend).
  expect_true(suppress_brms_threading(
    2L, mvn(), mv_spec_no_trends
  ))

  # Non-trigger: brms-native + no trend -> brms threading proceeds.
  expect_false(suppress_brms_threading(
    2L, gaussian(), mv_spec_no_trends
  ))

  # Non-trigger: threads = 1 -> nothing to suppress.
  expect_false(suppress_brms_threading(
    1L, gaussian(), mv_spec_with_trends
  ))
  expect_false(suppress_brms_threading(
    1L, nmix("poisson_binomial"), mv_spec_with_trends
  ))
})


test_that("brms-native gate does not fire on closure-unit / multi-response", {
  # Closure-unit families (`nmix`, `occ`) and multi-response families
  # (`diri`, `mvn`, `mvt`, `multinomial`, `categorical`) emit their
  # own `partial_sum_<family>_lpmf` + `reduce_sum` via mvgam-side
  # stanvars and must remain unaffected by the brms-native gate.
  # Exercise the predicate directly to avoid coupling the test to
  # any one family's full codegen path.
  mv_spec_with_trends <- list(has_trends = TRUE)

  # closure-unit branch
  expect_false(threads_no_op_for_trend_brms_native(
    2L, nmix("poisson_binomial"), mv_spec_with_trends
  ))
  expect_false(threads_no_op_for_trend_brms_native(
    2L, occ(), mv_spec_with_trends
  ))

  # multi-response branch (mvn() is the smallest constructor without
  # extra arg requirements; the other simplex / multivariate families
  # share the `mvgam_multi_response` attribute predicate).
  expect_false(threads_no_op_for_trend_brms_native(
    2L, mvn(), mv_spec_with_trends
  ))

  # brms-native + trend -> predicate returns TRUE
  expect_true(threads_no_op_for_trend_brms_native(
    2L, gaussian(), mv_spec_with_trends
  ))

  # brms-native + no trend -> predicate returns FALSE
  expect_false(threads_no_op_for_trend_brms_native(
    2L, gaussian(), list(has_trends = FALSE)
  ))

  # threads = 1 -> predicate returns FALSE
  expect_false(threads_no_op_for_trend_brms_native(
    1L, gaussian(), mv_spec_with_trends
  ))
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
      validate = TRUE
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
    expect_true(stan_pattern("array\\[N_time_trend, N_series_trend\\] real<lower=0> time_dis", code_with_trend))

    # Standard trend mapping arrays
    expect_true(stan_pattern("array\\[N_time_trend, N_series_trend\\] int times_trend;", code_with_trend))
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
    expect_true(stan_pattern("vector<lower=0.001,upper=0.999>\\[N_lv_trend\\] ar1_trend;", code_with_trend))
    expect_true(stan_pattern("vector<lower=0>\\[N_lv_trend\\] sigma_trend;",
                      code_with_trend))
    expect_true(stan_pattern("matrix\\[N_time_trend, N_lv_trend\\] innovations_trend;",
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
    expect_true(stan_pattern("matrix\\[N_time_trend, N_lv_trend\\] scaled_innovations_trend;",
                      code_with_trend))
    expect_true(stan_pattern("scaled_innovations_trend = innovations_trend \\*
  diag_matrix\\(sigma_trend\\);", code_with_trend))

    # CAR latent variable evolution
    expect_true(stan_pattern("matrix\\[N_time_trend, N_lv_trend\\] lv_trend;", code_with_trend))

    # CAR initialization (first time point)
    expect_true(stan_pattern("for \\(j in 1:N_lv_trend\\)", code_with_trend))
    expect_true(stan_pattern("lv_trend\\[1, j\\] = scaled_innovations_trend\\[1, j\\];",
                      code_with_trend))

    # CAR continuous-time evolution (key differentiator)
    expect_true(stan_pattern("for \\(j in 1:N_lv_trend\\)", code_with_trend))
    expect_true(stan_pattern("for \\(i in 2:N_time_trend\\)", code_with_trend))
    expect_true(stan_pattern("lv_trend\\[i, j\\] = pow\\(ar1_trend\\[j\\], time_dis\\[i, j\\]\\) \\*
  lv_trend\\[i - 1, j\\]", code_with_trend))
    expect_true(stan_pattern("\\+ scaled_innovations_trend\\[i, j\\];", code_with_trend))

    # Universal trend computation pattern
    expect_true(stan_pattern("matrix\\[N_time_trend, N_series_trend\\] trend;", code_with_trend))
    expect_true(stan_pattern("vector\\[N_trend\\] mu_trend = rep_vector\\(0.0,
  N_trend\\);", code_with_trend))
    expect_true(stan_pattern("for \\(i in 1:N_time_trend\\)", code_with_trend))
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
      expect_true(stan_pattern("poisson", code))
    } else if (fam_name == "gaussian") {
      expect_true(stan_pattern("normal", code))
    } else if (fam_name == "bernoulli") {
      expect_true(stan_pattern("bernoulli", code))
    }
  }
})

test_that("stancode generates correct Stan blocks", {
  data <- setup_stan_test_data()$univariate
  mf <- mvgam_formula(y ~ s(x), trend_formula = ~ AR(p = 1))

  # Generate Stan code without validation for structure inspection
  code <- stancode(mf, data = data, family = poisson(), validate = TRUE)

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
  expect_true(stan_pattern("vector\\[N\\]\\s*mu", code))
  expect_true(stan_pattern("vector\\[.*\\]\\s*mu_trend", code))
  expect_true(stan_pattern("matrix\\[.*\\]\\s*trend;", code))

  # Trend parameters should be declared in parameters block
  expect_true(stan_pattern("real.*ar1_trend", code))
  expect_true(stan_pattern("vector<lower=0>\\[.*\\]\\s*sigma_trend", code))

  # Data block should contain mapping arrays
  expect_true(stan_pattern("array\\[N\\]\\s*int\\s*obs_trend_time", code))
  expect_true(stan_pattern("array\\[N\\]\\s*int\\s*obs_trend_series", code))

  # Trend injection should use correct pattern
  expect_true(stan_pattern(paste0(
    "mu\\[n\\]\\s*\\+=\\s*trend\\[obs_trend_time\\[n\\],",
    "\\s*obs_trend_series\\[n\\]\\]"
  ), code))
  expect_false(grepl("obs_ind", code))

  # Universal trend computation pattern should be present
  expect_true(stan_pattern("for\\(iin1:N_time_trend\\)", code))
  expect_true(stan_pattern("trend\\[i,\\s*s\\]\\s*=.*dot_product", code))

  # Should contain sigma_trend prior but not duplicate sigma prior
  expect_true(stan_pattern("sigma_trend\\s*~", code))

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
  code_shared <- stancode(mf_shared, data = data, validate = TRUE)

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
  expect_true(stan_pattern("int<lower=1> N_count;", code_shared))
  # Should declare Y_count as vector
  expect_true(stan_pattern("vector\\[N_count\\] Y_count;", code_shared))
  # Should declare X_count design matrix
  expect_true(stan_pattern(
    "matrix\\[N_count, K_count\\] X_count;", code_shared
  ))
  # Should declare N_biomass with comment
  expect_true(stan_pattern("int<lower=1> N_biomass;", code_shared))
  # Should declare Y_biomass as vector
  expect_true(stan_pattern("vector\\[N_biomass\\] Y_biomass;", code_shared))

  # Trend dimensions
  # Should declare N_trend
  expect_true(stan_pattern("int<lower=1> N_trend;", code_shared))
  # Should declare N_series_trend
  expect_true(stan_pattern("int<lower=1> N_series_trend;", code_shared))
  # Should declare N_lv_trend
  expect_true(stan_pattern("int<lower=1> N_lv_trend;", code_shared))

  # Observation-to-trend mapping arrays
  # Should declare obs_trend_time_count array
  expect_true(stan_pattern(
    "array\\[N_count\\] int obs_trend_time_count;", code_shared
  ))
  # Should declare obs_trend_series_count array
  expect_true(stan_pattern(
    "array\\[N_count\\] int obs_trend_series_count;", code_shared
  ))
  # Should declare obs_trend_time_biomass array
  expect_true(stan_pattern(
    "array\\[N_biomass\\] int obs_trend_time_biomass;", code_shared
  ))
  # Should declare obs_trend_series_biomass array
  expect_true(stan_pattern(
    "array\\[N_biomass\\] int obs_trend_series_biomass;", code_shared
  ))

  # Times trend matrix - Should declare times_trend 2D array
  expect_true(stan_pattern(
    "array\\[N_time_trend, N_series_trend\\] int times_trend;", code_shared
  ))

  # Offset data structures for each response (brms consolidates them)
  expect_true(stan_pattern(
    "vector\\[N_count\\] offsets_count;", code_shared
  ))  # Count offsets
  expect_true(stan_pattern(
    "vector\\[N_biomass\\] offsets_biomass;", code_shared
  ))  # Biomass offsets

  # GLM compatibility vectors
  # Should declare mu_ones_count for GLM
  expect_true(stan_pattern("vector\\[1\\] mu_ones_count;", code_shared))
  # Should declare mu_ones_biomass for GLM
  expect_true(stan_pattern("vector\\[1\\] mu_ones_biomass;", code_shared))

  # Should create identity matrix Z for non-factor model
  expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z = diag_matrix\\(rep_vector\\(1.0, N_lv_trend\\)\\);", code_shared))

  # Observation model parameters
  # Should declare b_count coefficients
  expect_true(stan_pattern("vector\\[Kc_count\\] b_count;", code_shared))
  # Should declare Intercept_count
  expect_true(stan_pattern("real Intercept_count;", code_shared))
  # Should declare sigma_count with lower bound
  expect_true(stan_pattern("real<lower=0> sigma_count;", code_shared))

  # Trend parameters with _trend suffix
  # Should declare sigma_trend vector
  expect_true(stan_pattern(
    "vector<lower=0>\\[N_lv_trend\\] sigma_trend;", code_shared
  ))
  # Should declare L_Omega_trend for correlation
  expect_true(stan_pattern(
    "cholesky_factor_corr\\[N_lv_trend\\] L_Omega_trend;", code_shared
  ))
  # Should declare innovations_trend matrix
  expect_true(stan_pattern(
    "matrix\\[N_time_trend, N_lv_trend\\] innovations_trend;", code_shared
  ))

  # Should initialize lprior
  expect_true(stan_pattern("real lprior = 0;", code_shared))

  # Should create mu_trend from Intercept_trend using rep_vector
  expect_true(stan_pattern("vector\\[N_trend\\] mu_trend = rep_vector\\(0.0, N_trend\\);", code_shared))

  # Should report Sigma_trend as a covariance, not as the scaled
  # Cholesky factor it is built from
  expect_true(stan_pattern("cov_matrix\\[N_lv_trend\\] Sigma_trend = multiply_lower_tri_self_transpose\\(", code_shared))
  expect_true(stan_pattern("diag_pre_multiply\\(\\s*sigma_trend,\\s*L_Omega_trend\\)\\);", code_shared))

  # RW latent variables
  # Should declare lv_trend matrix for latent variables (with _trend suffix)
  expect_true(stan_pattern(
    "matrix\\[N_time_trend, N_lv_trend\\] lv_trend;", code_shared
  ))
  # Should declare L_Sigma_trend for scaling
  expect_true(stan_pattern("matrix\\[N_lv_trend, N_lv_trend\\] L_Sigma_trend =", code_shared))
  # Should declare scaled_innovations_trend
  expect_true(stan_pattern("matrix\\[N_time_trend, N_lv_trend\\] scaled_innovations_trend", code_shared))
  # Should initialize first lv_trend from scaled innovations
  expect_true(stan_pattern("lv_trend\\[1,\\s*:\\s*\\] = scaled_innovations_trend\\[1,\\s*:\\s*\\]", code_shared))
  # Should implement RW cumulative sum
  expect_true(stan_pattern("lv_trend\\[i,\\s*:\\s*\\] = lv_trend\\[i - 1,\\s*:\\s*\\].*\\+.*scaled_innovations_trend\\[i,\\s*:\\s*\\];", code_shared))

  # Trend matrix computation (shared, not response-specific)
  # Should declare shared trend matrix (not trend_count/trend_biomass)
  expect_true(stan_pattern(
    "matrix\\[N_time_trend, N_series_trend\\] trend;", code_shared
  ))

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
  expect_true(stan_pattern(paste0(
    "normal_id_glm_lpdf\\(Y_count \\| to_matrix\\(mu_count\\), ",
    "0\\.0, mu_ones_count, sigma_count\\)"
  ), code_shared))
  # Should use GLM function with to_matrix(mu_biomass) and mu_ones_biomass
  expect_true(stan_pattern(paste0(
    "normal_id_glm_lpdf\\(Y_biomass \\| to_matrix\\(mu_biomass\\), ",
    "0\\.0, mu_ones_biomass,\\s*sigma_biomass\\)"
  ), code_shared))

  # Trend priors in model block
  # `~ RW(cor = TRUE)` contributes no population-level term, so the
  # trend carries no intercept at all and `mu_trend` stays at zero.
  # Naming the parameter says that; naming a sampling statement it
  # was never going to have does not.
  expect_false(grepl("Intercept_trend", code_shared, fixed = TRUE))
  # Should have some prior for sigma_trend (distribution may vary)
  expect_true(stan_pattern("sigma_trend ~ ", code_shared))
  # Should have LKJ prior for correlation
  expect_true(stan_pattern("lkj_corr_cholesky_lpdf(L_Omega_trend",
                             code_shared, fixed = TRUE))
  # Should have standard normal prior for innovations
  # `stan_prior_line()` returns the statement verbatim, so it is
  # matched as a literal rather than read as a pattern.
  expect_true(stan_pattern(
    stan_prior_line("to_vector(innovations_trend)", "std_normal()"),
    code_shared, fixed = TRUE
  ))

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

  # Each custom prior reaches the parameter it names. Asserting the
  # distribution text alone would pass on a program that put it on
  # the wrong parameter, and would miss it entirely once the
  # statement is written as a density call rather than a tilde.
  expect_equal(stan_prior_on(code_with_priors, "ar1_trend"),
                 "normal(0, 0.75)")
  expect_equal(stan_prior_on(code_with_priors, "sigma_trend"),
                 "exponential(7)")
  # The defaults they displaced are gone.
  expect_equal(stan_prior_on(code_default, "ar1_trend"),
                 "normal(0, 0.5)")
  expect_equal(stan_prior_on(code_default, "sigma_trend"),
                 "exponential(2)")
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

  out <- mvgam_stan_setup(mf, data = data, family = poisson())

  # Both should succeed
  expect_s3_class(out$code, "stancode")
  expect_type(out$data, "list")

  # Code should reference data elements that exist in standata
  # This is a simplified check - real validation would parse Stan code
  if ("N" %in% names(out$data)) {
    expect_true(stan_pattern("int.*N", out$code))
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
  out <- SW(mvgam_stan_setup(mf, data = data, prior = priors))

  expect_s3_class(out$code, "stancode")
  expect_type(out$data, "list")

  # Should contain complex model elements
  expect_true(stan_pattern("count", out$code))
  expect_true(stan_pattern("biomass", out$code))
  expect_true(stan_pattern("ar1_trend", out$code))
})

test_that("stan functions preserve object attributes and metadata", {
  data <- setup_stan_test_data()$univariate
  mf <- mvgam_formula(y ~ x, trend_formula = ~ RW())

  out <- mvgam_stan_setup(mf, data = data, family = poisson())

  # stancode should have correct class
  expect_equal(class(out$code), c("mvgamstancode", "stancode", "character"))

  # standata should preserve key information
  expect_type(out$data, "list")
  expect_true(length(names(out$data)) > 0)  # Should have named elements
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
  out <- mvgam_stan_setup(mf, data = small_data, family = poisson())

  expect_s3_class(out$code, "stancode")
  expect_type(out$data, "list")
})

test_that("stan functions provide informative error messages", {
  data <- setup_stan_test_data()$univariate

  # Missing required data components
  bad_data <- data[, c("y", "x")]  # Missing time and series
  mf <- mvgam_formula(y ~ x, trend_formula = ~ RW())

  # Should provide informative errors
  expect_error(stancode(mf, data = bad_data, family = poisson()))
  expect_error(standata(mf, data = bad_data, family = poisson()))
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
    validate = TRUE
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
  expect_true(stan_pattern("int<lower=0> N_change_trend;", code_with_trend))
  expect_true(stan_pattern("vector\\[N_change_trend\\] t_change_trend;", code_with_trend))
  expect_true(stan_pattern("real<lower=0> change_scale_trend;", code_with_trend))

  # GLM optimization components
  expect_true(stan_pattern("vector\\[1\\] mu_ones;", code_with_trend))

  # Observation-to-trend mapping arrays
  expect_true(stan_pattern("array\\[N\\] int obs_trend_time;", code_with_trend))
  expect_true(stan_pattern("array\\[N\\] int obs_trend_series;", code_with_trend))

  # Times trend matrix
  expect_true(stan_pattern("array\\[N_time_trend, N_series_trend\\] int times_trend;", code_with_trend))

  # 3. Transformed Data Block - Time vector and changepoint matrix
  # Factor loading matrix (diagonal for PW - no factor model support)
  expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z = diag_matrix\\(rep_vector\\(1\\.0, N_lv_trend\\)\\);", code_with_trend))

  # Time vector creation (integer sequence)
  expect_true(stan_pattern("vector\\[N_time_trend\\] time_trend;", code_with_trend))
  expect_true(stan_pattern("for \\(i in 1:N_time_trend\\) time_trend\\[i\\] = i;", code_with_trend))

  # Changepoint matrix computation
  expect_true(stan_pattern("matrix\\[N_time_trend, N_change_trend\\] Kappa_trend = get_changepoint_matrix\\(time_trend, t_change_trend, N_time_trend, N_change_trend\\);", code_with_trend))

  # 4. Parameters Block - PW-specific parameters
  # Base trend parameters
  expect_true(stan_pattern("vector\\[N_lv_trend\\] k_trend;", code_with_trend))
  expect_true(stan_pattern("vector\\[N_lv_trend\\] m_trend;", code_with_trend))
  expect_true(stan_pattern("matrix\\[N_change_trend, N_lv_trend\\] delta_trend;", code_with_trend))

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
  expect_true(stan_pattern("matrix\\[N_time_trend, N_lv_trend\\] lv_trend;", code_with_trend))

  # Linear trend computation
  expect_true(stan_pattern("for \\(s in 1 : N_lv_trend\\)", code_with_trend))
  expect_true(stan_pattern("lv_trend\\[1 : N_time_trend, s\\] = linear_trend\\(k_trend\\[s\\], m_trend\\[s\\],", code_with_trend))
  expect_true(stan_pattern("to_vector\\(delta_trend\\[ : , s\\]\\), time_trend,", code_with_trend))
  expect_true(stan_pattern("Kappa_trend,", code_with_trend))
  expect_true(stan_pattern("t_change_trend\\);", code_with_trend))

  # Universal trend computation pattern
  expect_true(stan_pattern("matrix\\[N_time_trend, N_series_trend\\] trend;", code_with_trend))
  expect_false(stan_pattern("vector\\[N_trend\\] mu_trend = rep_vector\\(Intercept_trend, N_trend\\);", code_with_trend))
  expect_true(stan_pattern("for \\(i in 1:N_time_trend\\)", code_with_trend))
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
    validate = TRUE
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
  expect_true(stan_pattern("array\\[N_time_trend, N_series_trend\\] int times_trend;", code_distributional))
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
  expect_true(stan_pattern("matrix\\[N_time_trend, N_lv_trend\\] innovations_trend;", code_distributional))

  # 6. Transformed Parameters - RW dynamics
  expect_true(stan_pattern("vector\\[N_trend\\] mu_trend = rep_vector\\(0\\.0, N_trend\\);", code_distributional))
  expect_true(stan_pattern("matrix\\[N_time_trend, N_lv_trend\\] scaled_innovations_trend;", code_distributional))
  expect_true(stan_pattern("scaled_innovations_trend = innovations_trend \\* diag_matrix\\(sigma_trend\\);", code_distributional))

  # RW state evolution
  expect_true(stan_pattern("lv_trend\\[1, :\\] = scaled_innovations_trend\\[1, :\\];", code_distributional))
  expect_true(stan_pattern("lv_trend\\[i, :\\] = lv_trend\\[i - 1, :\\] \\+ scaled_innovations_trend\\[i, :\\];", code_distributional))

  # Final trend computation
  expect_true(stan_pattern("matrix\\[N_time_trend, N_series_trend\\] trend;", code_distributional))
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

test_that("error message directs users to the mvgam multi-response wrapper", {
  data <- setup_stan_test_data()$univariate
  mf <- mvgam_formula(y ~ x)

  # Naked `categorical()` now redirects users to the mvgam wrapper
  # `categ()` rather than pointing back at brms; the multi-response
  # families ship via the long-format closure-unit pattern, not the
  # brms-native cbind() LHS.
  expect_error(
    stancode(mf, data = data, family = categorical()),
    regexp = "categ\\(\\)",
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

  # Likelihood must be INSIDE the for loop. The pattern ensures
  # hurdle_poisson_log_lpmf appears after for(n in 1:N){
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

test_that("trend formula with covariate on multi-series univariate produces X_trend matching N_trend", {
  # Regression test for the N_trend / N_time_trend split (closes the
  # silent X_trend dim mismatch where trend_data had n_time * n_series
  # rows but N_trend was forced to n_time).
  set.seed(7)
  n_time <- 6
  n_series <- 4
  d <- data.frame(
    time = rep(seq_len(n_time), n_series),
    series = factor(rep(paste0("s", seq_len(n_series)), each = n_time)),
    x = stats::rnorm(n_time * n_series),
    y = stats::rpois(n_time * n_series, lambda = 5)
  )

  sd <- standata(
    mvgam_formula(y ~ 1, trend_formula = ~ x + AR(p = 1)),
    data = d, family = poisson()
  )

  # N_trend keeps brms's natural nrow(trend_data) so the trend-level
  # design matrix has one row per (time, series). N_time_trend is the
  # independent time axis used by the latent dynamics.
  expect_equal(sd$N_trend, n_time * n_series)
  expect_equal(sd$N_time_trend, n_time)
  expect_equal(sd$N_series_trend, n_series)

  # X_trend dims must match N_trend. Before the layout split this was
  # 20 vs 5, causing "dims declared=(5,1); dims found=(20,1)" at Stan
  # init.
  expect_true(is.matrix(sd$X_trend))
  expect_equal(nrow(sd$X_trend), sd$N_trend)
  expect_equal(ncol(sd$X_trend), 1L)

  # times_trend maps each (time index i, series index s) to a unique
  # row of mu_trend; per-series case fills (i - 1) * n_series + s.
  expect_equal(dim(sd$times_trend), c(n_time, n_series))
  expect_equal(min(sd$times_trend), 1L)
  expect_equal(max(sd$times_trend), sd$N_trend)
  expect_equal(length(unique(as.integer(sd$times_trend))), sd$N_trend)
  expect_equal(sd$times_trend[1, ], seq_len(n_series))
  expect_equal(sd$times_trend[2, ], n_series + seq_len(n_series))

  # Stancode must declare both dims and the trend-formula design matrix
  # at the trend-data row count.
  sc <- stancode(
    mvgam_formula(y ~ 1, trend_formula = ~ x + AR(p = 1)),
    data = d, family = poisson(), validate = FALSE
  )
  expect_true(stan_pattern("int<lower=1> N_trend;", sc, fixed = TRUE))
  expect_true(stan_pattern("int<lower=1> N_time_trend;", sc, fixed = TRUE))
  expect_true(stan_pattern("matrix\\[N_trend, K_trend\\] X_trend;", sc))
  expect_true(stan_pattern(
    "vector\\[N_trend\\] mu_trend = rep_vector\\(0\\.0, N_trend\\);", sc
  ))
  expect_true(stan_pattern("matrix\\[N_time_trend, N_lv_trend\\] lv_trend;", sc))
  expect_true(stan_pattern("for \\(i in 2 : N_time_trend\\)", sc))
  expect_true(stan_pattern("matrix\\[N_time_trend, N_series_trend\\] trend;", sc))
  expect_true(stan_pattern(
    "array\\[N_time_trend, N_series_trend\\] int times_trend;", sc
  ))

  # Final validation: stanc should accept the assembled program.
  expect_no_error(
    stancode(
      mvgam_formula(y ~ 1, trend_formula = ~ x + AR(p = 1)),
      data = d, family = poisson(), validate = TRUE
    )
  )
})

test_that("ZMVN(gr = X) without explicit subgr auto-fills subgr to series", {
  # The codegen path always treated `series` as the implicit subgr
  # when only `gr` was supplied; validate_grouping_arguments now
  # mirrors that by auto-filling subgr = "series". Smoke-checks the
  # resulting Stan code carries the hierarchical structure.
  data <- setup_stan_test_data()$multivariate
  mf <- mvgam_formula(
    biomass ~ 1,
    trend_formula = ~ x + (x | habitat) + ZMVN(gr = habitat)
  )
  sd <- standata(mf, data = data, family = lognormal())
  expect_true(!is.null(sd$N_groups_trend))
  expect_true(!is.null(sd$N_subgroups_trend))
  expect_gt(sd$N_groups_trend, 0)
  expect_gt(sd$N_subgroups_trend, 0)

  sc <- stancode(mf, data = data, family = lognormal(), validate = FALSE)
  expect_true(stan_pattern("int<lower=1> N_groups_trend;", sc, fixed = TRUE))
  expect_true(stan_pattern("int<lower=1> N_subgroups_trend;", sc, fixed = TRUE))
})

test_that("subgr without gr is rejected with clear error", {
  # The auto-fill is one-directional: subgr cannot be supplied
  # without gr because we have no inferred main grouping variable.
  data <- setup_stan_test_data()$multivariate
  expect_error(
    standata(
      mvgam_formula(biomass ~ 1,
                    trend_formula = ~ ZMVN(subgr = habitat)),
      data = data, family = lognormal()
    ),
    regexp = "Subgrouping requires main grouping variable"
  )
})


# ---- Fixed-Z (trend_map) Stan emission contract -------------------

test_that("trend_map matrix moves Z into the data block; no Z_raw", {
  # Dense user-supplied Z (4 series, 2 factors, custom weights).
  # Constructor-level path: the trend formula's environment is
  # captured so `Z_user` resolves at evaluation time.
  data <- setup_stan_test_data()$multivariate
  Z_user <- matrix(
    c(1.0, 0.0,
      0.7, 0.3,
      0.0, 1.0,
      0.5, 0.5),
    nrow = 4L, ncol = 2L, byrow = TRUE
  )
  mf <- mvgam_formula(
    count ~ x,
    trend_formula = ~ -1 + AR(p = 1, trend_map = Z_user, cor = TRUE)
  )
  out <- mvgam_stan_setup(mf, data = data, family = poisson(),
                           validate = FALSE)
  code <- out$code
  sd <- out$data

  # Z declared in the data block (no Z_raw / construction / prior).
  expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z;", code))
  expect_false(grepl("Z_raw", code, fixed = TRUE))

  # Standata carries the matrix verbatim.
  expect_equal(dim(sd$Z), c(4L, 2L))
  expect_equal(unname(sd$Z), unname(Z_user))

  # Innovations are still factor-sized (n_lv = 2 < n_series = 4).
  expect_true(stan_pattern(
    "matrix\\[N_time_trend, N_lv_trend\\] innovations_trend;",
    code
  ))
  expect_equal(sd$N_lv_trend, 2L)
})

test_that("trend_map 'shared' yields single-column Z in data", {
  data <- setup_stan_test_data()$multivariate
  mf <- mvgam_formula(
    count ~ 1,
    trend_formula = ~ -1 + RW(trend_map = "shared")
  )
  out <- mvgam_stan_setup(mf, data = data, family = poisson(),
                           validate = FALSE)
  code <- out$code
  sd <- out$data

  expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z;", code))
  expect_false(grepl("Z_raw", code, fixed = TRUE))
  expect_equal(sd$N_lv_trend, 1L)
  expect_equal(dim(sd$Z), c(4L, 1L))
  expect_true(all(sd$Z == 1))
})

test_that("trend_map data.frame builds binary Z aligned to series", {
  data <- setup_stan_test_data()$multivariate
  tm <- data.frame(
    series = paste0("series", 1:4),
    trend = c(1, 1, 2, 2)
  )
  mf <- mvgam_formula(
    count ~ 1,
    trend_formula = ~ -1 + AR(p = 1, trend_map = tm)
  )
  out <- mvgam_stan_setup(mf, data = data, family = poisson(),
                           validate = FALSE)
  code <- out$code
  sd <- out$data

  expected_Z <- matrix(c(1, 0,
                         1, 0,
                         0, 1,
                         0, 1),
                       nrow = 4L, ncol = 2L, byrow = TRUE)
  expect_equal(unname(sd$Z), expected_Z)
  expect_false(grepl("Z_raw", code, fixed = TRUE))
})

test_that("trend_map 'identity' emits Z as data (not tdata default)", {
  # Even when n_lv == n_series, an explicit trend_map keeps Z in
  # the data block, so the loadings every post-fit surface reads
  # are the ones the user wrote rather than a sampled rotation of
  # them.
  data <- setup_stan_test_data()$multivariate
  mf <- mvgam_formula(
    count ~ 1,
    trend_formula = ~ -1 + RW(trend_map = "identity")
  )
  out <- mvgam_stan_setup(mf, data = data, family = poisson(),
                           validate = FALSE)
  code <- out$code
  sd <- out$data

  expect_true(stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z;", code))
  # No default diagonal Z assignment in tdata.
  expect_false(grepl(
    "Z = diag_matrix\\(rep_vector\\(1.0, N_lv_trend\\)\\)", code
  ))
  expect_equal(unname(sd$Z), diag(1, 4L))
})

test_that("trend_map conflict with explicit n_lv errors at fit time", {
  data <- setup_stan_test_data()$multivariate
  Z_user <- matrix(c(1, 0,
                     1, 1,
                     0, 1,
                     1, 0),
                   nrow = 4L, ncol = 2L, byrow = TRUE)
  mf <- mvgam_formula(
    count ~ 1,
    trend_formula = ~ -1 + AR(p = 1, n_lv = 3, trend_map = Z_user)
  )
  expect_error(
    stancode(mf, data = data, family = poisson(), validate = FALSE),
    "shape conflicts"
  )
})


# ---- Partial Z (NA = sampled) Stan emission contract ----------

test_that("trend_map with NA emits Z_template + Z_is_free + Z_free_vec", {
  # Partial Z: some entries fixed, some sampled (NA in matrix).
  data <- setup_stan_test_data()$multivariate
  Z_user <- matrix(
    c(1.0, 0.0,
      NA_real_, NA_real_,
      0.0, 1.0,
      NA_real_, NA_real_),
    nrow = 4L, ncol = 2L, byrow = TRUE
  )
  mf <- mvgam_formula(
    count ~ x,
    trend_formula = ~ -1 + AR(p = 1, trend_map = Z_user, cor = TRUE)
  )
  out <- mvgam_stan_setup(mf, data = data, family = poisson(),
                           validate = FALSE)
  code <- out$code
  sd <- out$data

  # Z_template in data with NAs replaced by 0.
  expect_true(stan_pattern(
    "matrix\\[N_series_trend, N_lv_trend\\] Z_template;", code
  ))
  # Z_is_free integer mask in data.
  expect_true(stan_pattern(
    paste0(
      "array\\[N_series_trend, N_lv_trend\\] ",
      "int<lower=0, upper=1> Z_is_free;"
    ),
    code
  ))
  # Free-entry vector parameter.
  expect_true(stan_pattern("vector\\[N_free_Z\\] Z_free_vec;", code))
  # Assembly loop in transformed parameters.
  expect_true(stan_pattern("Z\\[i, j\\] = Z_free_vec\\[idx\\];", code))
  expect_true(stan_pattern("Z\\[i, j\\] = Z_template\\[i, j\\];", code))
  # Prior on the free vector only. The statement is compared
  # literally rather than through `stan_pattern()`, which escapes
  # a pattern and would also try to read this as a tilde form.
  expect_true(grepl(
    stan_prior_line("Z_free_vec", "student_t(3, 0, 1)"),
    paste(as.character(code), collapse = "\n"), fixed = TRUE
  ))
  # User-supplied loadings bypass the QR identification path.
  expect_false(grepl("Z_tilde", code, fixed = TRUE))
  expect_false(grepl("Q_tilde", code, fixed = TRUE))
  expect_false(grepl("qr_thin_R", code, fixed = TRUE))
  # Standata carries the template (NAs -> 0), mask, and count.
  expect_equal(dim(sd$Z_template), c(4L, 2L))
  expect_equal(sum(sd$Z_template == 0), 6L)  # 4 NAs + 2 fixed-0
  expect_equal(dim(sd$Z_is_free), c(4L, 2L))
  expect_equal(sd$N_free_Z, 4L)
})

test_that("fully-fixed Z is preserved (no partial-Z stanvars emitted)", {
  # When trend_map has no NAs, the fully-fixed code path stays in
  # effect, emitting no Z_template or Z_free_vec.
  data <- setup_stan_test_data()$multivariate
  Z_user <- matrix(c(1, 0, 0.5, 0.5, 0, 1, 0.3, 0.7),
                    nrow = 4L, ncol = 2L, byrow = TRUE)
  mf <- mvgam_formula(
    count ~ 1,
    trend_formula = ~ -1 + AR(p = 1, trend_map = Z_user)
  )
  code <- stancode(mf, data = data, family = poisson(),
                   validate = FALSE)
  expect_false(grepl("Z_template", code, fixed = TRUE))
  expect_false(grepl("Z_is_free", code, fixed = TRUE))
  expect_false(grepl("Z_free_vec", code, fixed = TRUE))
  # User-supplied loadings bypass the QR identification path.
  expect_false(grepl("Z_tilde", code, fixed = TRUE))
  expect_false(grepl("Q_tilde", code, fixed = TRUE))
  expect_false(grepl("qr_thin_R", code, fixed = TRUE))
})


# Helper: 4-series factor-model fixture used by the
# loadings_prior contract tests below. Returns a list with the
# fixed pieces (data, mvgam_formula, features, distance matrix)
# every contract test re-uses.
loadings_prior_fixture <- function() {
  set.seed(1L)
  n_series <- 4L
  series_levels <- paste0("s", seq_len(n_series))
  data <- data.frame(
    series = factor(rep(series_levels, each = 10L)),
    time = rep(1:10, n_series),
    y = rpois(40L, 2)
  )
  features <- data.frame(
    series = series_levels,
    trait = rnorm(n_series)
  )
  d_phylo <- as.matrix(stats::dist(rnorm(n_series)))
  mf <- mvgam_formula(
    y ~ 1, trend_formula = ~ AR(p = 1, n_lv = 2)
  )
  list(
    data = data, features = features, d_phylo = d_phylo, mf = mf,
    series_levels = series_levels
  )
}


test_that("loadings_prior with features only emits ARD prior on Z", {
  fx <- loadings_prior_fixture()
  code <- suppressWarnings(stancode(
    fx$mf, data = fx$data, family = poisson(),
    data2 = list(features = fx$features),
    loadings_prior = list(features = "features")
  ))
  sc <- as.character(code)
  expect_match(sc, "int<lower=1>\\s+N_features_trend\\s*;")
  expect_match(
    sc,
    "array\\[N_features_trend\\]\\s+real<lower=0>\\s+theta_features\\s*;"
  )
  expect_match(sc, "gp_exponential_cov", fixed = TRUE)
  expect_match(sc, "cholesky_decompose", fixed = TRUE)
  expect_match(sc, "multi_normal_cholesky", fixed = TRUE)
  # The length-scale is a settable prior, so it is written as a
  # sampling statement like every other one mvgam emits.
  expect_true(stan_pattern("theta_features ~ lognormal(0, 1)", sc))
  # The structured prior replaces the default iid one, so nothing
  # student_t reaches `Z`. The default program carries that row, so
  # the emptiness here is the structured prior displacing it rather
  # than a pattern that no longer matches anything.
  expect_length(stan_prior_on(sc, "Z"), 0L)
  expect_match(sc, "qr_thin_R", fixed = TRUE)
})


test_that("loadings_prior with distances only emits exponential decay prior", {
  fx <- loadings_prior_fixture()
  code <- suppressWarnings(stancode(
    fx$mf, data = fx$data, family = poisson(),
    data2 = list(phylo = fx$d_phylo),
    loadings_prior = list(distances = "phylo")
  ))
  sc <- as.character(code)
  expect_match(
    sc,
    "matrix<lower=0>\\[N_series_trend,\\s*N_series_trend\\]\\s+dist_phylo\\s*;"
  )
  expect_match(sc, "real<lower=0>\\s+theta_dist_phylo\\s*;")
  expect_match(sc, "exp\\(-dist_phylo")
  expect_false(grepl("gp_exponential_cov", sc, fixed = TRUE))
  expect_false(grepl("row_features", sc, fixed = TRUE))
})


test_that("loadings_prior combines features and distances multiplicatively, threads standata", {
  fx <- loadings_prior_fixture()
  out <- suppressWarnings(mvgam_stan_setup(
    fx$mf, data = fx$data, family = poisson(),
    data2 = list(features = fx$features, phylo = fx$d_phylo),
    loadings_prior = list(
      features = "features", distances = "phylo"
    )
  ))
  sc <- as.character(out$code)
  expect_match(sc, "gp_exponential_cov", fixed = TRUE)
  expect_match(sc, "dist_phylo", fixed = TRUE)
  # Multiplicative combination uses Stan's elementwise `.*`.
  expect_match(sc, ".*", fixed = TRUE)
  # Standata carries both feature and distance threads.
  expect_equal(out$data$N_features_trend, 1L)
  expect_equal(dim(out$data$row_features), c(4L, 1L))
  expect_equal(dim(out$data$dist_phylo), c(4L, 4L))
  expect_equal(max(out$data$dist_phylo), 1)
})


test_that("loadings_prior with column_shrinkage = 'mgp' emits MGP machinery", {
  fx <- loadings_prior_fixture()
  code <- suppressWarnings(stancode(
    fx$mf, data = fx$data, family = poisson(),
    data2 = list(phylo = fx$d_phylo),
    loadings_prior = list(
      distances = "phylo",
      column_shrinkage = "mgp",
      mgp_a1 = 2, mgp_a2 = 6
    )
  ))
  sc <- as.character(code)
  expect_match(sc, "varrho_inv", fixed = TRUE)
  expect_match(sc, "Psi_diag", fixed = TRUE)
  expect_match(sc, "inv_gamma\\(mgp_a1|inv_gamma_lpdf\\([^|]*\\| ?mgp_a1")
  expect_match(sc, "sqrt\\(Psi_diag")
})


test_that("default factor model still emits the iid student_t default", {
  fx <- loadings_prior_fixture()
  code <- stancode(fx$mf, data = fx$data, family = poisson())
  sc <- as.character(code)
  expect_match(sc, paste0("to_vector\\(Z\\)\\s*~\\s*student_t",
                            "|student_t_lpdf\\(to_vector\\(Z\\)"))
  expect_false(grepl("Phi_loadings", sc, fixed = TRUE))
  expect_false(grepl("gp_exponential_cov", sc, fixed = TRUE))
  expect_match(sc, "qr_thin_R", fixed = TRUE)
})


test_that("loadings_prior errors when combined with a partial trend_map", {
  fx <- loadings_prior_fixture()
  # Each row carries at least one finite entry and at least one
  # NA so the partial-Z validator accepts the matrix and the
  # loadings-prior compatibility check is what fires.
  Z_partial <- matrix(c(1, NA, NA, 1, 1, NA, NA, 1),
                       nrow = 4L, ncol = 2L, byrow = TRUE)
  mf <- mvgam_formula(
    y ~ 1,
    trend_formula = ~ AR(p = 1, trend_map = Z_partial)
  )
  expect_error(
    suppressWarnings(stancode(
      mf, data = fx$data, family = poisson(),
      data2 = list(phylo = fx$d_phylo),
      loadings_prior = list(distances = "phylo")
    )),
    "cannot combine with a partial 'trend_map'"
  )
})


test_that("loadings_prior errors when combined with a fully-fixed trend_map", {
  fx <- loadings_prior_fixture()
  Z_fixed <- matrix(c(1, 0, 0.5, 0.5, 0, 1, 0.3, 0.7),
                     nrow = 4L, ncol = 2L, byrow = TRUE)
  mf <- mvgam_formula(
    y ~ 1,
    trend_formula = ~ AR(p = 1, trend_map = Z_fixed)
  )
  expect_error(
    suppressWarnings(stancode(
      mf, data = fx$data, family = poisson(),
      data2 = list(phylo = fx$d_phylo),
      loadings_prior = list(distances = "phylo")
    )),
    "cannot combine with a fully-fixed 'trend_map'"
  )
})


# ---- AR() prior + coef_sharing surface (Step 3) -------------------

# Shared fixture for the four valid combinations + the
# constructor-side rejections. Small panel because we only
# inspect the emitted stancode, not the posterior.
ar_step3_fixture <- function() {
  set.seed(1L)
  data.frame(
    y = rnorm(80L),
    time = rep(1:20L, 4L),
    series = factor(rep(paste0("s", 1:4), each = 20L))
  )
}

ar_step3_stancode <- function(tf) {
  dat <- ar_step3_fixture()
  mf <- mvgam_formula(y ~ 1, trend_formula = tf)
  paste(unlist(stancode(mf, data = dat)), collapse = "\n")
}

# brms reformats `<lower=a,upper=b>` to `<lower=a, upper=b>`
# in its emitted Stan source. The regex patterns below use
# `\\s*` between comma-separated arguments so they survive
# either spacing.

test_that("AR(default + none) keeps the historical ar{lag}_trend declaration", {
  sc <- ar_step3_stancode(~ AR(p = 2))
  expect_true(grepl(
    "vector<lower=-1,\\s*upper=1>\\[N_lv_trend\\] ar1_trend;", sc
  ))
  expect_true(grepl(
    "vector<lower=-1,\\s*upper=1>\\[N_lv_trend\\] ar2_trend;", sc
  ))
  expect_false(grepl("ar1_shared\\b", sc))
  expect_false(grepl("mu_ar1_trend\\b", sc))
})

test_that("AR(default + shared) emits ar{lag}_shared and broadcasts to ar{lag}_trend", {
  sc <- ar_step3_stancode(~ AR(p = 2, coef_sharing = "shared"))
  expect_true(grepl(
    "vector<lower=-1,\\s*upper=1>\\[1\\] ar1_shared;", sc
  ))
  expect_true(grepl(
    "vector<lower=-1,\\s*upper=1>\\[1\\] ar2_shared;", sc
  ))
  expect_true(grepl(
    "ar1_trend = rep_vector\\(ar1_shared\\[1\\], N_lv_trend\\);", sc
  ))
  expect_true(grepl(stan_prior_line("ar1_shared", "normal(0, 0.5)"), sc,
                      fixed = TRUE))
  expect_false(grepl(
    "vector<lower=-1,\\s*upper=1>\\[N_lv_trend\\] ar1_trend;", sc
  ))
})

test_that("AR(default + hierarchical) emits mu/sigma hyperparams and pooled prior", {
  sc <- ar_step3_stancode(~ AR(p = 2, coef_sharing = "hierarchical"))
  expect_true(grepl(
    "real<lower=-1,\\s*upper=1> mu_ar1_trend;", sc
  ))
  expect_true(grepl("real<lower=0> sigma_ar1_trend;", sc))
  expect_true(grepl(
    "vector<lower=-1,\\s*upper=1>\\[N_lv_trend\\] ar1_trend;", sc
  ))
  expect_true(grepl(stan_prior_line("mu_ar1_trend", "normal(0, 0.5)"), sc,
                      fixed = TRUE))
  expect_true(grepl(stan_prior_line("sigma_ar1_trend", "exponential(2)"),
                      sc, fixed = TRUE))
  expect_true(grepl(
    stan_prior_line("ar1_trend",
                      "normal(mu_ar1_trend, sigma_ar1_trend)"), sc,
    fixed = TRUE
  ))
})


test_that("get_prior() surfaces mu_/sigma_ rows under coef_sharing = \"hierarchical\"", {
  dat <- ar_step3_fixture()
  gp <- get_prior(
    mvgam_formula(
      y ~ 1, trend_formula = ~ AR(p = 2, coef_sharing = "hierarchical")
    ),
    data = dat
  )
  classes <- gp$class
  expect_true("mu_ar1_trend" %in% classes)
  expect_true("mu_ar2_trend" %in% classes)
  expect_true("sigma_ar1_trend" %in% classes)
  expect_true("sigma_ar2_trend" %in% classes)
  # The per-series coefficients are drawn from the population
  # distribution those hyperparameters describe, so they are the
  # model's structure rather than a prior anyone can set. Offering a
  # row for them promised an override that silently did nothing.
  expect_false("ar1_trend" %in% classes)
  expect_false("ar2_trend" %in% classes)
})


# ============================================================
# brms helper-parity exports
# ============================================================
# Users can call stancode / standata / make_stancode /
# make_standata / default_prior / get_prior from the mvgam
# namespace directly. The brms wrappers (make_stancode,
# make_standata) work via the mvgam_formula methods; default_prior
# gets its own dispatch method that delegates to get_prior, and
# standata gains a fitted-model method.

test_that("brms helper generics are exported by mvgam", {
  ns_exports <- getNamespaceExports("mvgam")
  for (name in c("stancode", "standata", "make_stancode",
                 "make_standata", "default_prior", "get_prior",
                 "prior_summary")) {
    expect_true(
      name %in% ns_exports,
      label = paste0("mvgam exports `", name, "`")
    )
  }
})

test_that("make_stancode / make_standata dispatch on mvgam_formula without brms::", {
  dat <- ar_step3_fixture()
  mf <- mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 2))
  sc <- make_stancode(mf, data = dat)
  expect_s3_class(sc, "mvgamstancode")
  expect_s3_class(sc, "stancode")
  expect_true(grepl("ar1_trend", as.character(sc)))

  sd <- make_standata(mf, data = dat)
  expect_type(sd, "list")
  expect_true(all(c("N", "Y", "N_trend") %in% names(sd)))
})

test_that("default_prior dispatches on mvgam_formula (was broken in brms default)", {
  dat <- ar_step3_fixture()
  mf <- mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1))
  # Sanity: the brms default dispatcher errors with "Nested
  # formulas are not allowed" on an mvgam_formula; the mvgam
  # method routes through get_prior.mvgam_formula and returns
  # the standard brmsprior frame.
  dp <- default_prior(mf, data = dat)
  expect_s3_class(dp, "brmsprior")
  expect_true("Intercept" %in% dp$class)
  # Must match get_prior() output exactly, so a user reading the
  # priors through either entry point is told the same thing.
  gp <- get_prior(mf, data = dat)
  expect_identical(default_prior(mf, data = dat),
                   get_prior(mf, data = dat))
})

test_that("standata.mvgam and default_prior.mvgam read from the fitted slot", {
  # Stub a fitted-mvgam shape; the methods only need
  # `$standata` / `$prior` and the class tag.
  stub <- structure(
    list(
      standata = list(N = 10L, Y = rnorm(10L)),
      prior = structure(
        data.frame(prior = "normal(0, 1)", class = "Intercept",
                   coef = "", group = "", resp = "", dpar = "",
                   nlpar = "", lb = NA_character_, ub = NA_character_,
                   source = "user", stringsAsFactors = FALSE),
        class = c("brmsprior", "data.frame")
      )
    ),
    class = "mvgam"
  )
  sd <- standata(stub)
  expect_identical(sd, stub$standata)
  dp <- default_prior(stub)
  expect_identical(dp, stub$prior)
  # Errors when the slot is empty so callers learn about stale
  # fits rather than silently getting NULL.
  empty <- structure(list(), class = "mvgam")
  expect_error(standata(empty), "Stan data not found")
  expect_error(default_prior(empty),
               "Fit was not stored with a prior table")
})


test_that("brms helper batch re-exports work via mvgam without library(brms)", {
  # set_prior + prior + prior_string + empty_prior return brmsprior frames
  expect_s3_class(set_prior("normal(0,1)", class = "b"), "brmsprior")
  expect_s3_class(prior(normal(0, 1), class = "b"), "brmsprior")
  expect_s3_class(prior_string("exponential(2)", class = "sigma"),
                  "brmsprior")
  expect_s3_class(empty_prior(), "brmsprior")
  # Coercion + predicate
  p <- set_prior("normal(0,1)", class = "b")
  expect_true(is.brmsprior(p))
  expect_s3_class(as.brmsprior(p), "brmsprior")
  # Family + formula constructors
  expect_s3_class(brmsfamily("gaussian"), "brmsfamily")
  expect_s3_class(bf(y ~ x), "brmsformula")
  expect_s3_class(brmsformula(y ~ x), "brmsformula")
  expect_s3_class(mvbrmsformula(bf(y ~ x)), "mvbrmsformula")
  expect_true(is.brmsformula(bf(y ~ x)))
  expect_true(is.mvbrmsformula(mvbrmsformula(bf(y ~ x))))
  # Stanvar + constant
  expect_s3_class(
    stanvar(x = 5L, name = "K", scode = "int K;"),
    "stanvars"
  )
  expect_true(is.list(constant(1)) || is.function(constant))
})

test_that("control_params.mvgam / inits.mvgam read from the stanfit slot", {
  # Stub a minimal mvgam-shaped object with the @stan_args slot
  # populated. Avoids the cost of a real fit while still
  # exercising the dispatch and extraction.
  stub_stan_args <- list(
    list(control = list(adapt_delta = 0.95, max_treedepth = 12),
         init = "random"),
    list(control = list(adapt_delta = 0.95, max_treedepth = 12),
         init = "random")
  )
  stub_stanfit <- new(
    "stanfit",
    stan_args = stub_stan_args
  )
  stub <- structure(
    list(fit = stub_stanfit),
    class = "mvgam"
  )
  cp <- control_params(stub)
  expect_type(cp, "list")
  expect_identical(cp$adapt_delta, 0.95)
  expect_identical(cp$max_treedepth, 12)
  ii <- inits(stub)
  expect_length(ii, 2L)
  expect_identical(ii[[1L]], "random")
  expect_identical(ii[[2L]], "random")
})

# ---------------------------------------------------------------------------
# Deprecated `run_model = FALSE` short-circuit. Skips Stan parse /
# compile / sampling and returns a stub mvgam object whose stancode +
# standata slots are populated but $fit is NULL. The deprecation
# pointer goes to stancode() / standata() on an mvgam_formula(),
# which is what these tests exercise alongside.
# ---------------------------------------------------------------------------

run_model_test_data <- function(n = 24L) {
  set.seed(42L)
  data.frame(
    time = seq_len(n),
    series = factor(rep("series1", n)),
    y = rpois(n, lambda = 5),
    x = rnorm(n)
  )
}

test_that("run_model = FALSE warns once and returns NULL $fit", {
  data <- run_model_test_data()
  rlang::reset_warning_verbosity("mvgam_run_model_false_deprecated")
  expect_warning(
    mod <- mvgam(y ~ x, data = data, family = poisson(),
                 run_model = FALSE),
    "run_model = FALSE.*deprecated"
  )
  expect_s3_class(mod, "mvgam")
  expect_null(mod$fit)
})

test_that("run_model = FALSE populates stancode + standata slots", {
  data <- run_model_test_data()
  mod <- suppressWarnings(mvgam(
    y ~ x, data = data, family = poisson(), run_model = FALSE
  ))
  expect_true(!is.null(mod$stancode))
  expect_true(!is.null(mod$standata))
  # standata should carry the brms-flavoured slots from codegen.
  expect_true(is.list(mod$standata))
  expect_true("N" %in% names(mod$standata))
  expect_equal(mod$standata$N, nrow(data))
})

test_that("run_model = FALSE stub matches stancode() on the same formula", {
  data <- run_model_test_data()
  mf <- mvgam_formula(y ~ x)
  code_direct <- stancode(mf, data = data, family = poisson(),
                          validate = FALSE)
  mod <- suppressWarnings(mvgam(
    y ~ x, data = data, family = poisson(), run_model = FALSE
  ))
  # The deprecated path should produce IDENTICAL code to the helper
  # it points users toward; otherwise the deprecation message is
  # misleading. Strip whitespace + trailing newlines for the compare.
  norm <- function(x) gsub("\\s+", "", as.character(x))
  expect_equal(norm(mod$stancode), norm(code_direct))
})

test_that("run_model = FALSE threads through jsdgam() too", {
  set.seed(11L)
  dat <- expand.grid(
    time = seq_len(12L),
    species = factor(paste0("sp", 1:3))
  )
  dat$y <- rpois(nrow(dat), 2)
  # Reset rlang's per-session rate limit so we observe the warning
  # even when an earlier test already triggered it.
  rlang::reset_warning_verbosity("mvgam_run_model_false_deprecated")
  expect_warning(
    mod <- jsdgam(
      formula = y ~ 1, factor_formula = ~ -1,
      data = dat, unit = time, species = species,
      family = poisson(), n_lv = 2L,
      run_model = FALSE, silent = 2
    ),
    "run_model = FALSE.*deprecated"
  )
  expect_s3_class(mod, "jsdgam")
  expect_s3_class(mod, "mvgam")
  expect_null(mod$fit)
  expect_true(!is.null(mod$standata))
  expect_equal(mod$standata$N_lv_trend, 2L)
})

test_that("run_model = FALSE leaves trend_metadata populated for forecast / predict context", {
  data <- run_model_test_data()
  mod <- suppressWarnings(mvgam(
    y ~ x, trend_formula = ~ AR(p = 1L),
    data = data, family = poisson(), run_model = FALSE
  ))
  expect_true(!is.null(mod$trend_metadata))
  # Enrichment fields the forecast / predict surfaces read.
  expect_true("trend_type" %in% names(mod$trend_metadata) ||
              "max_lag" %in% names(mod$trend_metadata))
})

test_that("run_model = FALSE deprecation fires only once per session via rlang", {
  data <- run_model_test_data()
  # Frequency-controlled rlang warnings re-fire after a reset; reset
  # so we observe the same warning in both calls below.
  rlang::reset_warning_verbosity("mvgam_run_model_false_deprecated")
  expect_warning(
    suppressMessages(mvgam(y ~ x, data = data, family = poisson(),
                           run_model = FALSE)),
    "run_model = FALSE"
  )
  # Second call within the same session should NOT re-warn (frequency
  # = "regularly" suppresses repeat fires within the rate limit).
  expect_no_warning(
    suppressMessages(mvgam(y ~ x, data = data, family = poisson(),
                           run_model = FALSE))
  )
})


test_that("threads is a first-class named arg on mvgam() and jsdgam()", {
  # threads is a named arg rather than something captured in `...`
  # and pulled from `dots$threads` inside mvgam_single, so it gives
  # users autocomplete plus a checkmate-style early-fail on a bad
  # value, before the expensive Stan compile.
  expect_true("threads" %in% formalArgs(mvgam))
  expect_true("threads" %in% formalArgs(jsdgam))
  # Default is NULL (no threading). validate_threads(NULL) returns
  # `brms::threading()`, the "no threading" sentinel.
  fmls <- formals(mvgam)
  expect_null(eval(fmls$threads))
})

test_that("mvgam() rejects an invalid threads argument before compile", {
  # validate_threads errors on anything that is not NULL, numeric,
  # or a `brms::threading()` object. The outer call runs the check
  # before any compile so the error path is fast.
  expect_error(
    mvgam(y ~ 1, data = data.frame(y = 1:5), threads = "many"),
    "threads"
  )
})

test_that("threads = N forwards brms partial_log_lik into mvgam stancode", {
  # Threading was being dropped at the mvgam mock-fit setup; only
  # closure-unit families threaded (via mvgam's own partial_sum
  # stanvars). The plumbing fix forwards `threads = N` (when N > 1)
  # so brms emits its partial_log_lik_lpmf for brms-native fits.
  set.seed(1L)
  n <- 50L
  dat <- data.frame(
    y      = rnorm(n),
    x      = rnorm(n),
    grp    = factor(sample(letters[1:6], n, replace = TRUE)),
    time   = seq_len(n),
    series = factor(rep("s1", n))
  )
  mf <- mvgam_formula(y ~ x + (1 | grp))
  sc_default <- SW(stancode(mf, data = dat, family = gaussian()))
  sc_one     <- SW(stancode(mf, data = dat, family = gaussian(),
                              threads = 1L))
  sc_four    <- SW(stancode(mf, data = dat, family = gaussian(),
                              threads = 4L))
  expect_false(grepl("partial_log_lik_lpmf", sc_default))
  expect_false(grepl("partial_log_lik_lpmf", sc_one))
  expect_true(grepl("partial_log_lik_lpmf", sc_four))
  expect_true(grepl("reduce_sum", sc_four))
})


# ---- Conway-Maxwell-Binomial family contract --------------------

test_that("com_binomial() emits expected Stan lpmf + lookup table", {
  # Asserts that:
  # 1. The custom lpmf function is declared in the Stan functions
  #    block with the (int y, real mu, real nu, int T, data vector
  #    lfact) signature
  # 2. `trials[n]` (the denominator) is referenced in the model
  #    block
  # 3. The transformed-data log-factorial table is precomputed once
  #    per fit, as a vector rather than the square array of binomial
  #    coefficients an earlier implementation built
  # 4. The lpmf reads that table rather than calling `lchoose()` per
  #    row, and bounds the normalising sum instead of enumerating
  #    every one of the T + 1 outcomes
  set.seed(0)
  dat <- data.frame(
    y = rbinom(30, size = 5L, prob = 0.4),
    trials = 5L,
    x = rnorm(30),
    series = factor("s1"),
    time = 1:30
  )
  mf <- mvgam_formula(bf(y | trials(trials) ~ x))
  sc <- as.character(stancode(
    mf, data = dat, family = com_binomial(), validate = TRUE
  ))
  expect_true(grepl(
    "real com_binomial_lpmf\\(int y, real mu, real nu, int T",
    sc
  ))
  expect_true(grepl("trials\\[n\\]", sc))
  expect_true(grepl("data vector lfact", sc))
  expect_true(grepl(
    "vector\\[max_com_binomial_T \\+ 1\\] lfact_com_binomial", sc
  ))
  expect_false(grepl("lchoose", sc))
  # The normaliser is bounded rather than enumerated, so the helper
  # that locates the mode and the weight function are both emitted.
  expect_true(grepl("com_binomial_mode\\(T, nu, theta\\)", sc))
  expect_true(grepl("com_binomial_lw\\(", sc))
})


test_that("com_binomial() default prior on nu is normal(1, 1)", {
  # `nu` is the natural parameter of the COM-Binomial exponential
  # family, so a normal prior on it regularises as one on a
  # regression coefficient does. It is centred on independence
  # (`nu = 1`) and scaled to the range the data can resolve, the
  # response saturating outside roughly `nu` in `(-1, 4)`. Confirm
  # the injection emits this prior on the nu class along with the
  # truncation correction for the `lb = -5` lower bound.
  set.seed(0)
  dat <- data.frame(
    y = rbinom(30, size = 5L, prob = 0.4),
    trials = 5L,
    x = rnorm(30),
    series = factor("s1"),
    time = 1:30
  )
  mf <- mvgam_formula(bf(y | trials(trials) ~ x))
  sc <- as.character(stancode(
    mf, data = dat, family = com_binomial(), validate = FALSE
  ))
  expect_true(grepl(
    "lprior \\+= normal_lpdf\\(nu \\| 1, 1\\)", sc
  ))
  expect_true(grepl("real<lower=-5> nu", sc))
})


test_that("com_binomial() refuses non-logit links", {
  expect_error(com_binomial(link = "probit"))
  expect_error(com_binomial(link = "log"))
})


test_that("com_binomial() routes through the row-wise family kind", {
  # CMB is a row-wise custom family (one observation per row, no
  # per-unit aggregation, no multi-response). Confirm the kind
  # predicates classify it correctly so the threading gate and
  # other dispatch tables route as intended.
  fam <- com_binomial()
  expect_false(is_closure_unit_family(fam))
  expect_false(is_multi_response_family(fam))
  expect_false(is_simplex_response_family(fam))
  expect_true(is_com_binomial_family(fam))
  expect_identical(resolve_family_name(fam), "com_binomial")
})


test_that("com_binomial() reports `nu` as the only auxiliary dpar", {
  # `mu` lives on `linpred` (driven by the formula); `nu` is the
  # only posterior dpar to extract for the count-scale `E[Y]`.
  expect_identical(
    get_family_dpars("com_binomial"), "nu"
  )
})


test_that("com_binomial() requires a `trials()` addition term", {
  # The lpmf references `trials[n]`, which brms emits only for a
  # response carrying `trials()`. Without it stanc would fail on an
  # undefined variable, so the omission is named up front.
  dat <- data.frame(
    y = rbinom(30, size = 5L, prob = 0.4),
    trials = 5L,
    x = rnorm(30),
    series = factor("s1"),
    time = 1:30
  )
  expect_error(
    stancode(mvgam_formula(bf(y ~ x)), data = dat,
             family = com_binomial(), validate = FALSE),
    "trials"
  )
  # With the term present, brms sizes `trials` to the rows it kept
  # rather than to the raw frame, which is what makes NA responses
  # usable with a latent trend.
  dat$y[1:5] <- NA
  # brms announces the dropped rows; the point of the test is that
  # `trials` follows them instead of keeping the raw frame's length.
  expect_warning(
    sd <- standata(mvgam_formula(bf(y | trials(trials) ~ x)), data = dat,
                   family = com_binomial()),
    "Rows containing NAs"
  )
  expect_identical(length(sd$trials), sd$N)
  expect_identical(sd$N, 25L)
})


test_that("multi-response fits with NA preserve per-response valid rows", {
  # brms's mvbf listwise-deletes rows with NA in ANY response.
  # mvgam overrides this so each
  # response keeps its own non-NA rows and contributes to the
  # shared latent state at every time point it was observed. The
  # `expand_per_response_standata` helper substitutes per-arm
  # data arrays into the combined standata.
  set.seed(1)
  T_ <- 20L
  dat <- data.frame(
    time   = seq_len(T_),
    series = factor(rep("a", T_)),
    count   = rpois(T_, 5),
    biomass = rgamma(T_, 2, 0.5),
    camera  = rbinom(T_, 1, 0.4)
  )
  dat$count[c(5L, 11L)]              <- NA_integer_
  dat$biomass[c(8L, 14L)]            <- NA_real_
  dat$camera[c(3L, 7L, 12L, 18L)]    <- NA_integer_

  f <- bf(count   ~ 1, family = poisson()) +
       bf(biomass ~ 1, family = Gamma(link = "log")) +
       bf(camera  ~ 1, family = bernoulli(link = "logit"))

  mf <- mvgam_formula(f, trend_formula = ~ AR(p = 1))
  # brms emits one "Rows containing NAs were excluded" warning
  # per bf() arm; mvgam's per-arm expansion call emits more.
  # All are expected.
  sd <- withCallingHandlers(
    standata(mf, data = dat),
    warning = function(w) {
      if (grepl("Rows containing NAs", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )

  # Per-response N_<resp> should equal that response's own
  # non-NA row count, NOT the listwise intersection.
  for (r in c("count", "biomass", "camera")) {
    expected <- sum(!is.na(dat[[r]]))
    expect_identical(as.integer(sd[[paste0("N_", r)]]),
                     as.integer(expected))
    expect_identical(length(sd[[paste0("Y_", r)]]), expected)
    expect_identical(length(sd[[paste0("obs_trend_time_", r)]]),
                     expected)
    expect_identical(length(sd[[paste0("obs_trend_series_", r)]]),
                     expected)
  }
  # The per-response N values should differ because each response
  # has different NA rows.
  expect_false(sd$N_count == sd$N_biomass &&
                 sd$N_biomass == sd$N_camera)
})


test_that("multi-response fits with no NAs hit the fast no-op path", {
  # When no response has NAs, brms's listwise-deleted standata is
  # already per-response-correct and `expand_per_response_standata`
  # returns it unchanged after a fast no-op check.
  set.seed(1)
  T_ <- 20L
  dat <- data.frame(
    time   = seq_len(T_),
    series = factor(rep("a", T_)),
    count   = rpois(T_, 5),
    biomass = rgamma(T_, 2, 0.5)
  )

  mf <- mvgam_formula(
    bf(count ~ 1, family = poisson()) +
      bf(biomass ~ 1, family = Gamma(link = "log")),
    trend_formula = ~ AR(p = 1)
  )
  sd <- withCallingHandlers(
    standata(mf, data = dat),
    warning = function(w) {
      if (grepl("Rows containing NAs", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
  expect_identical(as.integer(sd$N_count),   T_)
  expect_identical(as.integer(sd$N_biomass), T_)
})


test_that("per-arm standata expansion handles smooths, ranef and dpar", {
  # Verifies the trailing-then-mid key stripper in
  # `map_combined_key_to_single` correctly recovers per-arm values
  # under every per-key naming pattern brms emits.
  set.seed(1)
  T_ <- 30L
  dat <- data.frame(
    time     = seq_len(T_),
    series   = factor(rep("a", T_)),
    g        = factor(rep(c("g1", "g2", "g3"), each = T_ / 3L)),
    x        = rnorm(T_),
    z        = rnorm(T_),
    m        = rep(1:12, length.out = T_),
    count    = rpois(T_, 5),
    biomass  = rgamma(T_, 2, 0.5),
    camera   = rbinom(T_, 1, 0.4)
  )
  dat$count[c(2L, 5L)]            <- NA_integer_
  dat$biomass[c(8L, 11L, 17L)]    <- NA_real_
  dat$camera[c(3L, 7L, 12L, 18L)] <- NA_integer_

  mf <- mvgam_formula(
    bf(count   ~ x + (1|g),               family = poisson()) +
      bf(biomass ~ s(m, k = 5), shape ~ z, family = Gamma(link = "log")) +
      bf(camera  ~ x,                       family = bernoulli(link = "logit")),
    trend_formula = ~ AR(p = 1)
  )
  sd <- withCallingHandlers(
    standata(mf, data = dat),
    warning = function(w) {
      if (grepl("Rows containing NAs", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
  # Every per-response array reaches its expected per-arm length.
  for (r in c("count", "biomass", "camera")) {
    expected <- sum(!is.na(dat[[r]]))
    expect_identical(as.integer(sd[[paste0("N_", r)]]),
                     as.integer(expected))
    expect_identical(length(sd[[paste0("Y_", r)]]), expected)
  }
  # Ranef Z and J arrays for count (mid-position then trailing
  # suffix patterns) should match N_count.
  expect_identical(length(sd$J_1_count),   as.integer(sd$N_count))
  expect_identical(nrow(sd$Z_1_count_1),   as.integer(sd$N_count))
  # Smooth Zs array for biomass (mid-position suffix).
  expect_identical(nrow(sd$Zs_biomass_1_1),
                   as.integer(sd$N_biomass))
  # Dpar `shape ~ z` design matrix for biomass has trailing
  # `_biomass` suffix after the dpar prefix `_shape`.
  expect_identical(nrow(sd$X_shape_biomass),
                   as.integer(sd$N_biomass))
})


test_that("a modelled dpar does not collide with mvgam's injected default prior", {
  # mvgam injects a class-level default for `nu` (com_binomial) and for
  # `shape` / `mtail` (beta_nb). When the user gives that parameter its
  # own sub-formula, brms replaces the scalar with a design matrix, so
  # a prior aimed at the scalar class matches nothing and brms rejects
  # the whole prior set. The injected row has to be dropped first.
  set.seed(4)
  dat <- data.frame(
    y = rbinom(40, size = 10, prob = 0.5),
    trials = 10L,
    site = factor(rep(c("a", "b"), each = 20)),
    time = 1:40,
    series = factor(rep("series1", 40))
  )
  sc <- paste(unlist(stancode(
    mvgam_formula(brms::bf(y | trials(trials) ~ 1, nu ~ site)),
    data = dat, family = com_binomial(), backend = "cmdstanr"
  )), collapse = "\n")
  expect_true(grepl("b_nu", sc, fixed = TRUE))
  # nu is identity-linked, so its default belongs on the intercept once
  # nu is modelled. Without the move brms falls back to a positive-only
  # gamma on a parameter that may go negative, and warns.
  expect_true(grepl("normal_lpdf(Intercept_nu | 1, 1)", sc, fixed = TRUE))
  expect_false(grepl("gamma_lpdf(Intercept_nu", sc, fixed = TRUE))

  # and the scalar default is still emitted when nu is not modelled
  sc_plain <- paste(unlist(stancode(
    mvgam_formula(y | trials(trials) ~ 1),
    data = dat, family = com_binomial(), backend = "cmdstanr"
  )), collapse = "\n")
  expect_true(grepl("normal_lpdf(nu | 1, 1)", sc_plain, fixed = TRUE))
})


# ---- time as a covariate inside trend_formula ----------------------

test_that("trend_covariate_names() drops the grouping columns", {
  # `time` and `series` are added by the caller and grouped on before
  # the summarise, and dplyr omits grouping columns from `across()`.
  # Naming them in the selection fails with "Element `time` doesn't
  # exist", which is what broke every trend_formula referring to time.
  expect_identical(mvgam:::trend_covariate_names(c("time", "x")), "x")
  expect_identical(mvgam:::trend_covariate_names(c("series", "x")), "x")
  expect_identical(mvgam:::trend_covariate_names(c("time", "series")),
                   character(0))
  expect_identical(mvgam:::trend_covariate_names(c("x", "z")), c("x", "z"))
  expect_identical(mvgam:::trend_covariate_names(character(0)), character(0))
})

test_that("trend_formula accepts the time variable as a covariate", {
  # `~ s(time)` is the most natural latent trend to write in a dynamic
  # GAM, and a plain `~ time` is a linear trend. Both must reach code
  # generation rather than error earlier.
  set.seed(7)
  dat <- data.frame(
    y = rpois(120, 5), x = rnorm(120), time = 1:120,
    series = factor(rep("series1", 120))
  )

  sc_lin <- paste(unlist(stancode(
    mvgam_formula(y ~ 1, trend_formula = ~ time + AR(p = 1)),
    data = dat, family = poisson(), backend = "cmdstanr")), collapse = "\n")
  expect_true(grepl("b_trend", sc_lin, fixed = TRUE))

  sc_s <- paste(unlist(stancode(
    mvgam_formula(y ~ 1, trend_formula = ~ s(time) + AR(p = 1)),
    data = dat, family = poisson(), backend = "cmdstanr")), collapse = "\n")
  expect_true(grepl("sds_", sc_s, fixed = TRUE))
  expect_true(grepl("Zs_", sc_s, fixed = TRUE))

  sd_gp <- standata(
    mvgam_formula(y ~ 1, trend_formula = ~ gp(time, k = 20) + AR(p = 1)),
    data = dat, family = poisson(), backend = "cmdstanr")
  expect_true(any(grepl("^Xgp_.*_trend$", names(sd_gp))))
  expect_true(any(grepl("^slambda_.*_trend$", names(sd_gp))))
})

test_that("trend_formula still builds without any time covariate", {
  # Stripping the grouping columns must not remove the design matrix
  # for ordinary covariates, nor invent one for a trend that has no
  # covariates at all.
  set.seed(7)
  dat <- data.frame(
    y = rpois(120, 5), x = rnorm(120), time = 1:120,
    series = factor(rep("series1", 120))
  )
  sc_x <- paste(unlist(stancode(
    mvgam_formula(y ~ 1, trend_formula = ~ s(x) + AR(p = 1)),
    data = dat, family = poisson(), backend = "cmdstanr")), collapse = "\n")
  expect_true(grepl("sds_", sc_x, fixed = TRUE))

  sc_bare <- paste(unlist(stancode(
    mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1)),
    data = dat, family = poisson(), backend = "cmdstanr")), collapse = "\n")
  expect_false(grepl("sds_", sc_bare, fixed = TRUE))
  expect_false(grepl("b_trend", sc_bare, fixed = TRUE))
})


test_that("trend_covariate_names() rejects a non-character selection", {
  expect_error(mvgam:::trend_covariate_names(1:3), "character")
  expect_error(mvgam:::trend_covariate_names(c("x", NA)), "missing")
})

test_that("time works as a trend covariate on the by = lv_axis() path", {
  # `collapse_to_time_level()` is only reached when `has_by_lv` is
  # TRUE. It collapses twice, once to (time, series) and once to
  # time.
  set.seed(5)
  dat <- expand.grid(time = 1:60, series = factor(paste0("s", 1:4)))
  dat$y <- rpois(nrow(dat), 5)
  dat$env <- rnorm(nrow(dat))

  sc_time <- paste(unlist(stancode(
    mvgam_formula(y ~ 1,
                  trend_formula = ~ s(time, by = lv_axis()) + AR(p = 1, n_lv = 2)),
    data = dat, family = poisson(), backend = "cmdstanr")), collapse = "\n")
  expect_true(grepl("sds_", sc_time, fixed = TRUE))

  # a non-time covariate on the same path is unaffected
  sc_env <- paste(unlist(stancode(
    mvgam_formula(y ~ 1,
                  trend_formula = ~ s(env, by = lv_axis()) + AR(p = 1, n_lv = 2)),
    data = dat, family = poisson(), backend = "cmdstanr")), collapse = "\n")
  expect_true(grepl("sds_", sc_env, fixed = TRUE))
})

test_that("time works as a trend covariate when the columns are named otherwise", {
  # The helper strips the literal names "time" and "series" because the
  # caller creates columns with exactly those names before grouping,
  # whatever the user's own columns are called. A user column named
  # `year` is therefore an ordinary covariate and must survive.
  set.seed(3)
  dat <- data.frame(
    y = rpois(120, 5), x = rnorm(120), year = 1:120,
    site = factor(rep("a", 120))
  )
  sc <- paste(unlist(stancode(
    mvgam_formula(y ~ 1,
                  trend_formula = ~ s(year) + AR(p = 1, time = year, series = site)),
    data = dat, family = poisson(), backend = "cmdstanr")), collapse = "\n")
  expect_true(grepl("sds_", sc, fixed = TRUE))

  sc_lin <- paste(unlist(stancode(
    mvgam_formula(y ~ 1,
                  trend_formula = ~ year + AR(p = 1, time = year, series = site)),
    data = dat, family = poisson(), backend = "cmdstanr")), collapse = "\n")
  expect_true(grepl("b_trend", sc_lin, fixed = TRUE))
})

test_that("time works as a trend covariate for multivariate responses", {
  set.seed(9)
  dat <- expand.grid(time = 1:50, series = factor(paste0("s", 1:3)))
  dat$y1 <- rpois(nrow(dat), 5)
  dat$y2 <- rpois(nrow(dat), 8)
  sc <- paste(unlist(stancode(
    mvgam_formula(brms::mvbind(y1, y2) ~ 1,
                  trend_formula = ~ s(time) + AR(p = 1)),
    data = dat, family = poisson(), backend = "cmdstanr")), collapse = "\n")
  expect_true(grepl("sds_", sc, fixed = TRUE))
})


test_that("NA responses shrink the likelihood but not the trend grid", {
  # mvgam keeps the latent process on the full time grid and lets
  # the likelihood run over observed rows only. Anything carried
  # per observation must therefore follow brms's retained rows.
  # `com_binomial()` once packed its denominator from the raw
  # frame instead, so Stan received more values than there were
  # observations and every chain died at data-init.
  #
  # Three series over twenty times means the raw row count (60)
  # matches neither `N` nor the grid, so a leftover raw-length
  # array cannot hide behind a coincidence.
  set.seed(2)
  n_time <- 20L
  dat <- expand.grid(time = seq_len(n_time),
                     series = factor(paste0("s", 1:3)))
  dat$x <- rnorm(nrow(dat))
  dat$trials <- pmax(rpois(nrow(dat), 20L), 1L)

  cases <- list(
    list(tag = "poisson", family = poisson(),
         y = rpois(nrow(dat), 5), formula = y ~ x),
    list(tag = "tweedie", family = tweedie(),
         y = rgamma(nrow(dat), 2, 1), formula = y ~ x),
    list(tag = "beta_nb", family = beta_nb(),
         y = rnbinom(nrow(dat), mu = 5, size = 2), formula = y ~ x),
    list(tag = "com_binomial", family = com_binomial(),
         y = rbinom(nrow(dat), dat$trials, 0.5),
         formula = bf(y | trials(trials) ~ x))
  )

  for (case in cases) {
    d <- dat
    d$y <- case$y
    d$y[c(3L, 17L, 28L, 41L, 55L)] <- NA
    mf <- mvgam_formula(case$formula, trend_formula = ~ AR(p = 1))
    # brms reports the rows it dropped, once per internal pass.
    sd <- withCallingHandlers(
      standata(mf, data = d, family = case$family, backend = "cmdstanr"),
      warning = function(w) {
        if (grepl("Rows containing NAs", conditionMessage(w))) {
          invokeRestart("muffleWarning")
        }
      }
    )
    # The likelihood sees only observed rows ...
    expect_identical(sd$N, sum(!is.na(d$y)))
    # ... while the trend still spans every timepoint.
    expect_identical(sd$N_time_trend, n_time)
    # No data array may still be sized to the raw frame.
    raw_length <- vapply(
      sd,
      function(v) is.atomic(v) && is.null(dim(v)) && length(v) == nrow(d),
      logical(1)
    )
    expect_identical(names(sd)[raw_length], character(0))
  }
})

test_that("closure-unit families keep their unit arrays over observed visits", {
  # A missing response is a visit that did not happen, which is the
  # normal case in repeat-visit survey designs. The per-unit arrays
  # index brms's retained rows, so they must cover the observed
  # visits exactly: `sum(n_rep)` is the likelihood's row count and
  # no `visit_idx` entry may point past it.
  sim <- sim_closure_unit_data(family = occ(), type = 1L,
                               n_sites = 8L, n_visits = 4L, seed = 3L)
  dat <- sim$data_train
  dat$y[seq(2L, nrow(dat), by = 7L)] <- NA_integer_
  sd <- expect_warning(
    standata(mvgam_formula(y ~ 1), data = dat, family = occ()),
    "Rows containing NAs"
  )
  expect_identical(as.integer(sd$N), sum(!is.na(dat$y)))
  expect_identical(sum(sd$n_rep), as.integer(sd$N))
  expect_true(max(sd$visit_idx) <= sd$N)
  expect_true(all(sd$Y_max <= sd$K_max))

  # A unit whose visits are all missing carries nothing about
  # detection and is dropped rather than left to index an empty
  # visit list in the Stan loop.
  unit <- paste(dat$series, dat$time)
  blanked <- dat
  blanked$y[unit == unique(unit)[2L]] <- NA_integer_
  sd_dropped <- expect_warning(
    standata(mvgam_formula(y ~ 1), data = blanked, family = occ()),
    "Rows containing NAs"
  )
  expect_identical(as.integer(sd_dropped$N_unit),
                   as.integer(sd$N_unit) - 1L)
  expect_identical(sum(sd_dropped$n_rep), as.integer(sd_dropped$N))
})

test_that("com_binomial() accepts a constant denominator", {
  # `trials(30)` is the brms spelling for a fixed number of trials.
  # Taking the denominator from the addition term rather than a
  # hardcoded column means no `trials` column is needed.
  set.seed(7)
  dat <- data.frame(
    y = rbinom(30, size = 30L, prob = 0.5),
    x = rnorm(30), series = factor("s1"), time = 1:30
  )
  expect_false("trials" %in% names(dat))
  sd <- standata(mvgam_formula(bf(y | trials(30) ~ x)), data = dat,
                 family = com_binomial())
  expect_true(all(sd$trials == 30L))
  expect_identical(length(sd$trials), sd$N)
})


# ---- brms code-generation options ---------------------------------
# `knots`, `sample_prior`, `drop_unused_levels` and `normalize` are
# documented arguments that reach brms three times over: once when the
# mock fit is built, once when the Stan program is regenerated and once
# when the Stan data is. Each of these pins one of those journeys.

codegen_test_data <- function(n_time = 24L, n_series = 2L) {
  set.seed(11)
  out <- data.frame(
    time = rep(seq_len(n_time), n_series),
    series = factor(rep(paste0("s", seq_len(n_series)), each = n_time)),
    elev = runif(n_time * n_series, 0, 100)
  )
  out$y <- rpois(nrow(out), 5)
  out
}

test_that("mvgam_codegen_options() refuses malformed settings", {
  expect_error(mvgam:::mvgam_codegen_options(knots = list(1, 2)),
               "names")
  expect_error(mvgam:::mvgam_codegen_options(sample_prior = "maybe"),
               "sample_prior")
  expect_error(mvgam:::mvgam_codegen_options(normalize = NA),
               "normalize")
})

test_that("codegen_args_for() hands each generator what it declares", {
  opts <- mvgam:::mvgam_codegen_options(normalize = FALSE)
  expect_named(
    mvgam:::codegen_args_for(opts, mvgam:::mvgam_codegen_stancode_options),
    c("knots", "sample_prior", "drop_unused_levels", "normalize")
  )
  # `brms::standata()` declares no `normalize`, so it never sees one.
  expect_named(
    mvgam:::codegen_args_for(opts, mvgam:::mvgam_codegen_standata_options),
    c("knots", "sample_prior", "drop_unused_levels")
  )
  expect_identical(
    mvgam:::codegen_args_for(NULL, mvgam:::mvgam_codegen_stancode_options),
    list()
  )
})

test_that("knots reach the basis on both formulas", {
  dat <- codegen_test_data()
  kn <- list(elev = seq(0, 100, length.out = 5))

  obs <- mvgam_formula(y ~ s(elev, bs = "cr", k = 5),
                       trend_formula = ~ AR(p = 1))
  sd_default <- standata(obs, data = dat, family = poisson())
  sd_knots <- standata(obs, data = dat, family = poisson(), knots = kn)
  expect_false(isTRUE(all.equal(sd_default$Xs, sd_knots$Xs)))
  expect_false(isTRUE(all.equal(sd_default$Zs_1_1, sd_knots$Zs_1_1)))

  # The basis has to match what brms builds from the same knots, not
  # merely differ from the default.
  brms_knots <- brms::make_standata(y ~ s(elev, bs = "cr", k = 5),
                                    data = dat, family = poisson(),
                                    knots = kn)
  expect_equal(sd_knots$Xs, brms_knots$Xs, ignore_attr = TRUE)

  trend <- mvgam_formula(y ~ 1,
                         trend_formula = ~ s(elev, bs = "cr", k = 5) +
                           AR(p = 1))
  td_default <- standata(trend, data = dat, family = poisson())
  td_knots <- standata(trend, data = dat, family = poisson(), knots = kn)
  expect_false(isTRUE(all.equal(td_default$Xs_trend, td_knots$Xs_trend)))
})

test_that("a knot count that disagrees with k is refused", {
  # mgcv raises this, which is only reachable once the knots arrive.
  dat <- codegen_test_data()
  expect_error(
    standata(mvgam_formula(y ~ s(elev, bs = "cr", k = 5),
                           trend_formula = ~ AR(p = 1)),
              data = dat, family = poisson(),
              knots = list(elev = seq(0, 100, length.out = 6))),
    "knots"
  )
})

test_that("drop_unused_levels decides whether an empty level is priced", {
  dat <- codegen_test_data()
  dat$grp <- factor(rep(c("a", "b"), length.out = nrow(dat)),
                    levels = c("a", "b", "c"))
  mf <- mvgam_formula(y ~ grp, trend_formula = ~ AR(p = 1))

  expect_identical(
    as.integer(standata(mf, data = dat, family = poisson())$K), 2L
  )
  # The prior table is keyed by the same coefficients, so it has to be
  # merged under the same setting: merging under the default leaves no
  # row for `b_grpc` and brms rejects the whole table.
  kept <- standata(mf, data = dat, family = poisson(),
                   drop_unused_levels = FALSE)
  expect_identical(as.integer(kept$K), 3L)
})

test_that("sample_prior moves the data, not the program", {
  dat <- codegen_test_data()
  mf <- mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1))

  # brms gates the likelihood on `prior_only`, which is Stan data, so
  # "only" changes the standata and leaves the program untouched.
  expect_identical(
    as.integer(standata(mf, data = dat, family = poisson())$prior_only), 0L
  )
  expect_identical(
    as.integer(standata(mf, data = dat, family = poisson(),
                        sample_prior = "only")$prior_only), 1L
  )
  expect_identical(
    stancode(mf, data = dat, family = poisson(), silent = 2L),
    stancode(mf, data = dat, family = poisson(), sample_prior = "only",
             silent = 2L)
  )

  # "yes" keeps the likelihood and adds draws from the prior alongside.
  expect_true(grepl(
    "prior_Intercept",
    stancode(mf, data = dat, family = poisson(), sample_prior = "yes",
             silent = 2L)
  ))
})

test_that("normalize = FALSE emits the unnormalised densities", {
  dat <- codegen_test_data()
  mf <- mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1))
  unnormalised <- stancode(mf, data = dat, family = poisson(),
                           normalize = FALSE, silent = 2L)
  expect_true(grepl("_lup[dm]f", unnormalised))
  expect_false(identical(
    unnormalised,
    stancode(mf, data = dat, family = poisson(), silent = 2L)
  ))
  # The trend's own `sigma` prior is mvgam's to set, so brms's copy is
  # dropped under either spelling. Leaving the unnormalised spelling
  # unrecognised left `student_t_lupdf(sigma | ...)` in the program,
  # where the renamer then suffixed the function instead of the
  # parameter and Stan rejected it.
  expect_false(grepl("student_t_lupdf_trend", unnormalised))
})

test_that("the reserved-word list carries both density spellings", {
  reserved <- mvgam:::get_stan_reserved_words()
  normalised <- grep("_lp[dm]f$", reserved, value = TRUE)
  expect_true(length(normalised) > 0)
  expect_true(all(
    sub("_lp([dm])f$", "_lup\\1f", normalised) %in% reserved
  ))
})

test_that("save_model writes the assembled program", {
  dat <- codegen_test_data()
  mf <- mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1))
  path <- withr::local_tempfile(fileext = ".stan")

  code <- stancode(mf, data = dat, family = poisson(), save_model = path,
                   silent = 2L)
  expect_true(file.exists(path))
  # What lands on disk is mvgam's program, not the brms one it starts
  # from, so it names the trend.
  written <- paste(readLines(path), collapse = "\n")
  expect_identical(written, as.character(code))
  expect_true(grepl("trend", written))
})

test_that("save_model refuses a directory that does not exist", {
  dat <- codegen_test_data()
  expect_error(
    stancode(mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1)),
              data = dat, family = poisson(),
              save_model = file.path(tempdir(), "no_such_dir", "m.stan")),
    "not exist"
  )
})

test_that("brms-deprecated arguments are named rather than dropped", {
  dat <- codegen_test_data()
  mf <- mvgam_formula(y ~ elev, trend_formula = ~ AR(p = 1))

  # brms moved both of these off its own top level, so mvgam points at
  # where brms now reads them rather than resurrecting the argument.
  expect_error(
    stancode(mf, data = dat, family = poisson(), sparse = TRUE),
    "sparse = TRUE"
  )
  expect_error(
    standata(mf, data = dat, family = poisson(),
             stan_funs = "real f(real x) { return x; }"),
    "stanvars"
  )

  # The supported spelling still works.
  expect_true(grepl(
    "csr_matrix_times_vector",
    stancode(mvgam_formula(bf(y ~ elev, sparse = TRUE),
                           trend_formula = ~ AR(p = 1)),
              data = dat, family = poisson(), silent = 2L)
  ))
})

test_that("a fit records the options its program was generated under", {
  # `update()` reads all four off one slot, so a fit made before the
  # slot existed inherits nothing and falls back to the defaults.
  opts <- names(mvgam:::mvgam_codegen_options())
  expect_true(all(opts %in% names(mvgam:::update_inheritance_table())))
  expect_false(any(opts %in% names(mvgam:::mvgam_update_uninherited)))

  stored <- list(codegen = mvgam:::mvgam_codegen_options(
    knots = list(elev = 1:5), normalize = FALSE
  ))
  getters <- mvgam:::update_inheritance_table()
  expect_identical(getters$knots$getter(stored), list(elev = 1:5))
  expect_false(getters$normalize$getter(stored))
  expect_null(getters$knots$getter(list()))
})

test_that("the options reach a fit with no trend formula", {
  # A trend-free fit takes its program and data straight off the mock
  # brms fit, never through the assembly stage, so it is a separate
  # journey for the options to make.
  dat <- codegen_test_data()
  kn <- list(elev = seq(0, 100, length.out = 5))
  mf <- mvgam_formula(y ~ s(elev, bs = "cr", k = 5))

  sd_knots <- standata(mf, data = dat, family = poisson(), knots = kn)
  expect_false(isTRUE(all.equal(
    standata(mf, data = dat, family = poisson())$Xs, sd_knots$Xs
  )))
  expect_equal(
    sd_knots$Xs,
    brms::make_standata(y ~ s(elev, bs = "cr", k = 5), data = dat,
                        family = poisson(), knots = kn)$Xs,
    ignore_attr = TRUE
  )
  expect_true(grepl(
    "_lup[dm]f",
    stancode(mf, data = dat, family = poisson(), normalize = FALSE,
             silent = 2L)
  ))
})

test_that("a knots fit carries its basis into newdata predictions", {
  # `posterior_smooths()` and `conditional_smooths()` rebuild their
  # design matrices with `brms::standata()` on the stored brms fit. If
  # the knots never reached that fit, a prediction grid would be built
  # on a default basis and the smooth would be read off the wrong
  # coefficients without anything erroring.
  dat <- codegen_test_data()
  kn <- list(elev = seq(0, 100, length.out = 5))
  form <- mvgam_formula(y ~ s(elev, bs = "cr", k = 5),
                        trend_formula = ~ AR(p = 1))

  with_knots <- mvgam:::build_stan_components(
    formula = form, data = dat, family = poisson(), knots = kn,
    validate = FALSE, silent = 2L
  )
  without <- mvgam:::build_stan_components(
    formula = form, data = dat, family = poisson(),
    validate = FALSE, silent = 2L
  )
  expect_identical(with_knots$obs_setup$codegen$knots, kn)
  expect_identical(with_knots$trend_setup$codegen,
                   with_knots$obs_setup$codegen)

  grid <- data.frame(
    elev = seq(5, 95, length.out = 12), time = 1:12,
    series = factor("s1", levels = levels(dat$series))
  )
  regen <- function(components) {
    brms::standata(components$obs_setup$brmsfit, newdata = grid,
                   check_response = FALSE, internal = TRUE)$Xs
  }
  expect_false(isTRUE(all.equal(regen(with_knots), regen(without))))
})

test_that("normalize = FALSE survives the GLM path", {
  # brms writes a `*_glm_lupmf` call under `normalize = FALSE`, and
  # mvgam's GLM detection recognised only the normalised spelling, so
  # the trend injector could not find the likelihood and the assembly
  # aborted. An intercept-only formula hides this: brms emits a plain
  # `poisson_log_lpmf` and never reaches the GLM path at all, so the
  # fixture needs a predictor.
  dat <- codegen_test_data()
  dat$z <- rnorm(nrow(dat))
  dat$bin <- rbinom(nrow(dat), 1L, 0.5)
  dat$cont <- rnorm(nrow(dat))

  cases <- list(
    list(resp = "y", family = poisson()),
    list(resp = "y", family = brms::negbinomial()),
    list(resp = "cont", family = gaussian()),
    list(resp = "bin", family = brms::bernoulli())
  )
  for (case in cases) {
    mf <- mvgam_formula(
      stats::as.formula(paste0(case$resp, " ~ elev + z")),
      trend_formula = ~ AR(p = 1)
    )
    code <- stancode(mf, data = dat, family = case$family,
                     normalize = FALSE, silent = 2L)
    expect_true(grepl("_glm_lu?p[dm]f", code),
                label = paste("GLM call kept for", case$family$family))
    # The rewritten call keeps the spelling it replaced, so turning the
    # constants off is not quietly undone.
    expect_true(grepl("_glm_lup[dm]f", code),
                label = paste("unnormalised kept for", case$family$family))
  }
})

test_that("stan_density_suffix reports the spelling a program used", {
  expect_identical(
    mvgam:::stan_density_suffix("target += poisson_log_glm_lpmf(Y | Xc);",
                                "poisson_log_glm"),
    "_lpmf"
  )
  expect_identical(
    mvgam:::stan_density_suffix("target += normal_id_glm_lupdf(Y | Xc);",
                                "normal_id_glm"),
    "_lupdf"
  )
  expect_null(
    mvgam:::stan_density_suffix("target += normal_lpdf(y | 0, 1);",
                                "poisson_log_glm")
  )
})

test_that("the generator option sets stay pinned to the options list", {
  # A fifth option added to `mvgam_codegen_options()` and forgotten in
  # either set would be dropped on its way to brms without a word, so
  # both sets are derived rather than written out again.
  expect_identical(
    mvgam:::mvgam_codegen_stancode_options,
    names(mvgam:::mvgam_codegen_options())
  )
  expect_identical(
    mvgam:::mvgam_codegen_standata_options,
    setdiff(names(mvgam:::mvgam_codegen_options()), "normalize")
  )
})

test_that("codegen_from_dots reads the options a call carries", {
  opts <- mvgam:::codegen_from_dots(
    list(knots = list(x = 1:5), normalize = FALSE, chains = 4, iter = 100)
  )
  expect_identical(opts$knots, list(x = 1:5))
  expect_false(opts$normalize)
  # Sampler arguments are not code-generation options.
  expect_identical(names(opts), names(mvgam:::mvgam_codegen_options()))
})

test_that("get_prior reports the trend model that gets fitted", {
  # The observation side takes these through the dots brms reads; the
  # trend side is assembled by mvgam and has to be given them too, or a
  # user cannot set a prior on a coefficient the fit will estimate.
  dat <- codegen_test_data()
  dat$grp <- factor(rep(c("a", "b"), length.out = nrow(dat)),
                    levels = c("a", "b", "c"))
  mf <- mvgam_formula(y ~ 1, trend_formula = ~ grp + AR(p = 1))

  trend_coefs <- function(...) {
    tbl <- get_prior(mf, data = dat, family = poisson(), ...)
    sort(tbl$coef[tbl$class == "b_trend" & nzchar(tbl$coef)])
  }
  # The trend formula carries no intercept, so every observed level
  # earns a coefficient and the unused one appears only when it is kept.
  expect_identical(trend_coefs(), c("grpa", "grpb"))
  expect_identical(trend_coefs(drop_unused_levels = FALSE),
                   c("grpa", "grpb", "grpc"))
})

test_that("a shrinkage prior gets the Stan data it declares", {
  # `horseshoe()` declares data of its own. The program was generated
  # with the prior and the data without it, so the fit declared six
  # variables nothing supplied.
  dat <- codegen_test_data()
  dat$x1 <- rnorm(nrow(dat))
  dat$x2 <- rnorm(nrow(dat))
  mf <- mvgam_formula(y ~ x1 + x2, trend_formula = ~ AR(p = 1))
  hs <- brms::prior(horseshoe(1), class = "b")

  code <- stancode(mf, data = dat, family = poisson(), prior = hs,
                   silent = 2L)
  sdata <- standata(mf, data = dat, family = poisson(), prior = hs,
                    silent = 2L)
  declared <- unique(unlist(regmatches(
    code, gregexpr("\\b(hs_[a-z_]+|Kscales)\\b", code)
  )))
  supplied <- names(sdata)
  # Every name the data block reads has to be a name the data carries.
  expect_true(all(
    intersect(declared, c("hs_df", "hs_df_global", "hs_df_slab",
                          "hs_scale_global", "hs_scale_slab", "Kscales")) %in%
      supplied
  ))
})

test_that("normalize = FALSE still injects the trend into the GLM call", {
  # The GLM path recognised only the normalised `_lpmf` / `_lpdf`
  # spelling when deciding which family a likelihood line called, so
  # under `normalize = FALSE` it identified none, returned the line
  # untouched, and the emitted program computed the latent states and
  # then never used them. The model fitted as though it had no trend,
  # and it compiled and sampled, so nothing complained.
  dat <- codegen_test_data()
  dat$z <- rnorm(nrow(dat))
  dat$cont <- rnorm(nrow(dat))

  cases <- list(
    list(resp = "y", family = poisson(), density = "poisson_log_glm_lupmf"),
    list(resp = "cont", family = gaussian(),
         density = "normal_id_glm_lupdf")
  )
  for (case in cases) {
    mf <- mvgam_formula(
      stats::as.formula(paste0(case$resp, " ~ elev + z")),
      trend_formula = ~ AR(p = 1)
    )
    code <- stancode(mf, data = dat, family = case$family,
                     normalize = FALSE, silent = 2L)
    lab <- case$family$family

    # The trend has to reach mu, and mu has to reach the likelihood.
    expect_true(grepl("mu\\[n\\] \\+= trend\\[", code),
                label = paste("trend added to mu for", lab))
    expect_true(grepl("to_matrix\\(mu\\)", code),
                label = paste("likelihood reads mu for", lab))
    # And the unnormalised spelling survives the rewrite, or turning
    # the constants off would be quietly undone.
    expect_true(grepl(case$density, code, fixed = TRUE),
                label = paste("unnormalised density kept for", lab))
  }
})

test_that("the GLM family list is stated once", {
  # Detection, the transformation gate and the per-line type lookup all
  # read the same vector, so a seventh form cannot reach one and miss
  # the others.
  fams <- mvgam:::mvgam_glm_families
  expect_length(fams, 6L)
  expect_true(all(grepl("_glm$", fams)))

  present <- mvgam:::glm_calls_present(
    "target += poisson_log_glm_lupmf(Y | Xc, Intercept, b);"
  )
  expect_named(present, fams)
  expect_true(present[["poisson_log_glm"]])
  expect_false(present[["normal_id_glm"]])
})

test_that("an unidentifiable GLM family is refused, not skipped", {
  # Returning the line untouched is what let `normalize = FALSE` emit a
  # program that computed the trend and never used it. A family added
  # to the list without a transformation must fail loudly instead.
  block <- list(start_idx = 1L, end_idx = 2L)
  lines <- c("model {", "  target += mystery_glm_lpmf(Y | Xc, a, b);")
  expect_error(
    mvgam:::inject_trends_into_glm_calls(lines, block, "  // trend"),
    "family could not be identified"
  )

  # A model block with no GLM call is the ordinary case and passes
  # through unchanged.
  plain <- c("model {", "  target += poisson_log_lpmf(Y | mu);")
  expect_identical(
    mvgam:::inject_trends_into_glm_calls(plain, block, "  // trend"),
    plain
  )
})

test_that("the two code-generation entry points agree", {
  # `stancode()` and `standata()` reach the same generator, so an
  # argument on one and not the other means the pair describes
  # different models. `standata()` accepted five of these through its
  # dots while advertising none of them, and restated defaults drift
  # apart silently because nothing compares them.
  sc <- formals(mvgam:::stancode.mvgam_formula)
  sd <- formals(mvgam:::standata.mvgam_formula)
  expect_setequal(names(sc), names(sd))

  shared <- intersect(names(sc), names(sd))
  disagreeing <- shared[!vapply(
    shared, function(n) identical(sc[[n]], sd[[n]]), logical(1L)
  )]
  expect_identical(disagreeing, character(0))

  # Every argument they declare has to be one the generator takes, or
  # it is quietly discarded on the way down.
  generator <- names(formals(mvgam:::build_stan_components))
  expect_true(all(setdiff(names(sc), c("object", "...")) %in% generator))
})
