# Numerical Validation: mvgam extraction vs brms baseline
#
# Compares prediction SUMMARY STATISTICS between comparable brms and mvgam models.
# Uses caching to avoid refitting models when adding new tests.

devtools::load_all()
library(brms)
library(posterior)

# =============================================================================
# CONFIGURATION
# =============================================================================

CHAINS <- 2
ITER <- 1000
WARMUP <- 500
REFRESH <- 100
FIXTURE_DIR <- "tasks/fixtures"

if (!dir.exists(FIXTURE_DIR)) dir.create(FIXTURE_DIR, recursive = TRUE)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Load or fit a brms model with caching
fit_brms_cached <- function(name, formula, data, family, ...) {

  path <- file.path(FIXTURE_DIR, paste0("val_brms_", name, ".rds"))
  if (file.exists(path)) {
    cat("  Loading cached brms model:", name, "\n")
    return(readRDS(path))
  }
  cat("  Fitting brms model:", name, "\n")
  fit <- brm(
    formula = formula, data = data, family = family,
    chains = CHAINS, iter = ITER, warmup = WARMUP,
    refresh = REFRESH, silent = 2, backend = "cmdstanr", ...
  )
  saveRDS(fit, path)
  return(fit)
}

#' Load or fit an mvgam model with caching
fit_mvgam_cached <- function(name, formula, trend_formula, data, family, ...) {
  path <- file.path(FIXTURE_DIR, paste0("val_mvgam_", name, ".rds"))
  if (file.exists(path)) {
    cat("  Loading cached mvgam model:", name, "\n")
    return(readRDS(path))
  }
  cat("  Fitting mvgam model:", name, "\n")
  fit <- mvgam(
    formula = formula, trend_formula = trend_formula,
    data = data, family = family,
    chains = CHAINS, iter = ITER, warmup = WARMUP,
    refresh = REFRESH, silent = 2, backend = "cmdstanr", ...
  )
  saveRDS(fit, path)
  return(fit)
}

#' Extract mvgam observation-level predictions using our method
extract_mvgam_obs_pred <- function(mvgam_fit, newdata) {
  obs_params <- extract_obs_parameters(mvgam_fit)
  full_draws <- posterior::as_draws_matrix(mvgam_fit$fit)
  obs_draws <- full_draws[, obs_params, drop = FALSE]
  mock_fit <- create_mock_stanfit(obs_draws)
  prep <- prepare_predictions.mock_stanfit(
    object = mock_fit,
    brmsfit = mvgam_fit$obs_model,
    newdata = newdata
  )
  extract_linpred_from_prep(prep)
}

#' Extract mvgam trend-level predictions using our method
#' Note: Must strip _trend suffix from parameter names because:
#'   - Combined fit stores: b_trend[1], sd_1_trend[1], r_1_1_trend[1], etc.
#'   - But trend_model brmsfit expects: b[1], sd_1[1], r_1_1[1], etc.
extract_mvgam_trend_pred <- function(mvgam_fit, newdata) {
  trend_params <- extract_trend_parameters(mvgam_fit)
  full_draws <- posterior::as_draws_matrix(mvgam_fit$fit)
  trend_draws <- full_draws[, trend_params, drop = FALSE]

  # Strip _trend suffix so parameters match trend_model brmsfit expectations
  colnames(trend_draws) <- gsub("_trend", "", colnames(trend_draws))

  mock_fit <- create_mock_stanfit(trend_draws)
  prep <- prepare_predictions.mock_stanfit(
    object = mock_fit,
    brmsfit = mvgam_fit$trend_model,
    newdata = newdata
  )
  extract_linpred_from_prep(prep)
}

#' Extract combined obs + trend predictions (additive on link scale)
extract_mvgam_combined_pred <- function(mvgam_fit, newdata) {
  obs_pred <- extract_mvgam_obs_pred(mvgam_fit, newdata)
  trend_pred <- extract_mvgam_trend_pred(mvgam_fit, newdata)
  obs_pred + trend_pred
}

#' Compute summary statistics per observation
summarize_pred <- function(pred_matrix) {
  list(
    mean = colMeans(pred_matrix),
    median = apply(pred_matrix, 2, median),
    q025 = apply(pred_matrix, 2, quantile, probs = 0.025),
    q975 = apply(pred_matrix, 2, quantile, probs = 0.975),
    sd = apply(pred_matrix, 2, sd)
  )
}

#' Compare two vectors of summary statistics
compare_vectors <- function(v1, v2) {
  sd1 <- sd(v1)
  sd2 <- sd(v2)

  # Handle constant vectors
  if (sd1 < 1e-10 && sd2 < 1e-10) {
    return(list(cor = NA, rmse = abs(mean(v1) - mean(v2)), constant = TRUE))
  }
  if (sd1 < 1e-10 || sd2 < 1e-10) {
    return(list(cor = NA, rmse = NA, constant = FALSE, mismatch = TRUE))
  }

  list(
    cor = cor(v1, v2),
    rmse = sqrt(mean((v1 - v2)^2)),
    constant = FALSE
  )
}

#' Extract parameter summary (mean, sd, quantiles) from draws
param_summary <- function(draws, param_name) {
  if (!param_name %in% colnames(draws)) return(NULL)
  x <- draws[, param_name]
  c(
    mean = mean(x),
    sd = sd(x),
    q025 = unname(quantile(x, 0.025)),
    q500 = unname(quantile(x, 0.5)),
    q975 = unname(quantile(x, 0.975))
  )
}

#' Compare parameter estimates between two models
compare_params <- function(brms_draws, mvgam_draws, param_pairs) {
  cat("\n  Parameter Estimates (mean [q025, q975]):\n")
  cat(sprintf("  %-15s %-25s %-25s\n", "Parameter", "brms", "mvgam"))
  cat(sprintf("  %-15s %-25s %-25s\n", "---------", "----", "-----"))

  for (pair in param_pairs) {
    brms_name <- pair[1]
    mvgam_name <- pair[2]
    display <- pair[3]

    brms_s <- param_summary(brms_draws, brms_name)
    mvgam_s <- param_summary(mvgam_draws, mvgam_name)

    if (is.null(brms_s)) {
      brms_str <- "not found"
    } else {
      brms_str <- sprintf("%.3f [%.3f, %.3f]", brms_s["mean"], brms_s["q025"], brms_s["q975"])
    }

    if (is.null(mvgam_s)) {
      mvgam_str <- "not found"
    } else {
      mvgam_str <- sprintf("%.3f [%.3f, %.3f]", mvgam_s["mean"], mvgam_s["q025"], mvgam_s["q975"])
    }

    cat(sprintf("  %-15s %-25s %-25s\n", display, brms_str, mvgam_str))
  }
}

#' Run a complete validation test
#' @param extract_fn Function to extract mvgam predictions (default: obs only)
run_validation <- function(test_name, brms_fit, mvgam_fit, newdata,
                           param_pairs, incl_autocor = FALSE,
                           extract_fn = extract_mvgam_obs_pred) {
  cat("\n--- Validation:", test_name, "---\n")

  # Compare parameter estimates
  brms_draws <- as_draws_matrix(brms_fit)
  mvgam_draws <- as_draws_matrix(mvgam_fit$fit)
  compare_params(brms_draws, mvgam_draws, param_pairs)

  # Get predictions
  brms_pred <- posterior_linpred(brms_fit, newdata = newdata,
                                  incl_autocor = incl_autocor)
  mvgam_pred <- extract_fn(mvgam_fit, newdata)

  # Check dimensions
  if (!identical(dim(brms_pred), dim(mvgam_pred))) {
    cat("  FAIL: Dimension mismatch\n")
    cat("    brms:", dim(brms_pred), "mvgam:", dim(mvgam_pred), "\n")
    return(list(name = test_name, passed = FALSE, reason = "dim_mismatch"))
  }

  # Compare per-observation prediction summaries
  cat("\n  Per-Observation Predictions (cor, rmse):\n")
  brms_summ <- summarize_pred(brms_pred)
  mvgam_summ <- summarize_pred(mvgam_pred)

  stats <- c("mean", "median", "q025", "q975", "sd")
  results <- lapply(stats, function(s) {
    compare_vectors(brms_summ[[s]], mvgam_summ[[s]])
  })
  names(results) <- stats

  # Report per-observation stats
  for (s in stats) {
    r <- results[[s]]
    if (isTRUE(r$mismatch)) {
      cat(sprintf("  %s: MISMATCH\n", s))
    } else if (r$constant) {
      cat(sprintf("  %s: constant, diff = %.4f\n", s, r$rmse))
    } else {
      cat(sprintf("  %s: cor=%.4f, rmse=%.4f\n", s, r$cor, r$rmse))
    }
  }

  # Pass/fail based on MEAN correlation >= 0.95 (key metric)
  mean_r <- results$mean
  if (isTRUE(mean_r$mismatch)) {
    passed <- FALSE
  } else if (mean_r$constant) {
    passed <- TRUE  # Intercept-only is fine
  } else {
    passed <- mean_r$cor >= 0.95
  }

  status <- if (passed) "PASSED" else "FAILED"
  cat(sprintf("\n  Result: %s (mean cor >= 0.95)\n", status))

  list(name = test_name, passed = passed, stats = results)
}

# =============================================================================
# TEST DATA
# =============================================================================

cat("=== NUMERICAL VALIDATION: mvgam vs brms ===\n\n")

set.seed(42)
n_time <- 30

# Generate AR(1) latent process
ar_coef <- 0.7
sigma <- 0.5
latent <- numeric(n_time)
latent[1] <- rnorm(1, 0, sigma / sqrt(1 - ar_coef^2))
for (t in 2:n_time) {
  latent[t] <- ar_coef * latent[t - 1] + rnorm(1, 0, sigma)
}

# Create covariate with nonlinear effect
z <- seq(-2, 2, length.out = n_time)
z_effect <- 0.5 * sin(z * pi)  # Nonlinear sine effect

# Create base data with grouping
test_data <- data.frame(
  y = rpois(n_time, exp(2 + latent + z_effect)),
  x = rnorm(n_time),
  z = z,
  time = 1:n_time,
  series = factor("s1"),
  group = factor(rep(letters[1:6], each = 5))
)

cat("Test data: n =", n_time, "\n")

# =============================================================================
# VALIDATION TESTS
# =============================================================================

results <- list()

# -----------------------------------------------------------------------------
# Test 1: Intercept-only AR(1)
# -----------------------------------------------------------------------------

cat("\n=== Test 1: Intercept-only AR(1) ===\n")

brms_1 <- fit_brms_cached(

  "ar1_int",
  y ~ 1 + ar(time = time, p = 1, cov = TRUE),
  test_data, poisson()
)

mvgam_1 <- fit_mvgam_cached(

  "ar1_int",
  y ~ 1, ~ AR(p = 1),
  test_data, poisson()
)

results$test1 <- run_validation(
  "Intercept-only AR(1)",
  brms_1, mvgam_1, test_data,
  param_pairs = list(
    c("b_Intercept", "b_Intercept", "Intercept"),
    c("ar[1]", "ar1_trend[1]", "AR(1)"),
    c("sderr", "sigma_trend[1]", "Sigma")
  )
)

# -----------------------------------------------------------------------------
# Test 2: AR(1) with fixed effect
# -----------------------------------------------------------------------------

cat("\n=== Test 2: AR(1) with fixed effect ===\n")

brms_2 <- fit_brms_cached(
 "ar1_fx",
  y ~ 1 + x + ar(time = time, p = 1, cov = TRUE),
  test_data, poisson()
)

mvgam_2 <- fit_mvgam_cached(
  "ar1_fx",
  y ~ 1 + x, ~ AR(p = 1),
  test_data, poisson()
)

results$test2 <- run_validation(
  "AR(1) with fixed effect",
  brms_2, mvgam_2, test_data,
  param_pairs = list(
    c("b_Intercept", "b_Intercept", "Intercept"),
    c("b_x", "b[1]", "b_x"),
    c("ar[1]", "ar1_trend[1]", "AR(1)"),
    c("sderr", "sigma_trend[1]", "Sigma")
  )
)

# -----------------------------------------------------------------------------
# Test 3: AR(1) with random intercept
# -----------------------------------------------------------------------------

cat("\n=== Test 3: AR(1) with random intercept ===\n")

brms_3 <- fit_brms_cached(
  "ar1_re",
  y ~ 1 + x + (1 | group) + ar(time = time, p = 1, cov = TRUE),
  test_data, poisson()
)

mvgam_3 <- fit_mvgam_cached(
  "ar1_re",
  y ~ 1 + x + (1 | group), ~ AR(p = 1),
  test_data, poisson()
)

results$test3 <- run_validation(
  "AR(1) with random intercept",
  brms_3, mvgam_3, test_data,
  param_pairs = list(
    c("b_Intercept", "b_Intercept", "Intercept"),
    c("b_x", "b[1]", "b_x"),
    c("sd_group__Intercept", "sd_1[1]", "sd_group"),
    c("ar[1]", "ar1_trend[1]", "AR(1)"),
    c("sderr", "sigma_trend[1]", "Sigma")
  )
)

# -----------------------------------------------------------------------------
# Test 4: AR(1) with fixed effect, random intercept, and smooth
# -----------------------------------------------------------------------------

cat("\n=== Test 4: AR(1) + fixed + random + smooth ===\n")

brms_4 <- fit_brms_cached(
  "ar1_re_smooth",
  y ~ 1 + x + s(z, k = 5) + (1 | group) + ar(time = time, p = 1, cov = TRUE),
  test_data, poisson()
)

mvgam_4 <- fit_mvgam_cached(
  "ar1_re_smooth",
  y ~ 1 + x + s(z, k = 5) + (1 | group), ~ AR(p = 1),
  test_data, poisson()
)

results$test4 <- run_validation(
  "AR(1) + fixed + random + smooth",
  brms_4, mvgam_4, test_data,
  param_pairs = list(
    c("b_Intercept", "b_Intercept", "Intercept"),
    c("b_x", "b[1]", "b_x"),
    c("sd_group__Intercept", "sd_1[1]", "sd_group"),
    c("sds_sz_1", "sds_1[1]", "sds_smooth"),
    c("ar[1]", "ar1_trend[1]", "AR(1)"),
    c("sderr", "sigma_trend[1]", "Sigma")
  )
)

# -----------------------------------------------------------------------------
# Test 5: t2() tensor product smooth (NO INTERCEPT)
# -----------------------------------------------------------------------------

cat("\n=== Test 5: t2() tensor product smooth (no intercept) ===\n")

# Create data with two continuous predictors for tensor product
test_data_t2 <- test_data
test_data_t2$w <- seq(-1, 1, length.out = n_time)

brms_5 <- fit_brms_cached(
  "ar1_t2_noint",
  y ~ 0 + t2(z, w, k = c(4, 4)) + ar(time = time, p = 1, cov = TRUE),
  test_data_t2, poisson()
)

mvgam_5 <- fit_mvgam_cached(
  "ar1_t2_noint",
  y ~ 0 + t2(z, w, k = c(4, 4)), ~ AR(p = 1),
  test_data_t2, poisson()
)

results$test5 <- run_validation(
  "AR(1) + t2() tensor smooth (no intercept)",
  brms_5, mvgam_5, test_data_t2,
  param_pairs = list(
    c("sds_t2zw_1", "sds_1[1]", "sds_t2_1"),
    c("sds_t2zw_2", "sds_1[2]", "sds_t2_2"),
    c("sds_t2zw_3", "sds_1[3]", "sds_t2_3"),
    c("ar[1]", "ar1_trend[1]", "AR(1)"),
    c("sderr", "sigma_trend[1]", "Sigma")
  )
)

# -----------------------------------------------------------------------------
# Test 6: Monotonic effect (mo())
# -----------------------------------------------------------------------------

cat("\n=== Test 6: Monotonic effect (mo()) ===\n")

# Create ordered factor for monotonic effect
test_data_mo <- test_data
test_data_mo$ord_factor <- ordered(cut(test_data_mo$z, 4))

brms_6 <- fit_brms_cached(
  "ar1_mo",
  y ~ 1 + mo(ord_factor) + ar(time = time, p = 1, cov = TRUE),
  test_data_mo, poisson()
)

mvgam_6 <- fit_mvgam_cached(
  "ar1_mo",
  y ~ 1 + mo(ord_factor), ~ AR(p = 1),
  test_data_mo, poisson()
)

# For monotonic validation, use subset of training data to avoid factor level issues
# Both brms and mvgam correctly reject new factor levels for monotonic effects
validation_data_mo <- brms_6$data[1:15, ]  # Use subset of training data
validation_data_mo$time <- 1:15  # Update time sequence

results$test6 <- run_validation(
  "AR(1) + monotonic effect",
  brms_6, mvgam_6, validation_data_mo,
  param_pairs = list(
    c("b_Intercept", "b_Intercept", "Intercept"),
    c("bsp_moord_factor", "bsp[1]", "bsp_mo"),
    c("ar[1]", "ar1_trend[1]", "AR(1)"),
    c("sderr", "sigma_trend[1]", "Sigma")
  )
)

# -----------------------------------------------------------------------------
# Test 7: Correlated random effects ((x|group))
# -----------------------------------------------------------------------------

cat("\n=== Test 7: Correlated random effects ((x|group)) ===\n")

brms_7 <- fit_brms_cached(
  "ar1_cor_re",
  y ~ 1 + x + (1 + x | group) + ar(time = time, p = 1, cov = TRUE),
  test_data, poisson()
)

mvgam_7 <- fit_mvgam_cached(
  "ar1_cor_re",
  y ~ 1 + x + (1 + x | group), ~ AR(p = 1),
  test_data, poisson()
)

results$test7 <- run_validation(
  "AR(1) + correlated random effects",
  brms_7, mvgam_7, test_data,
  param_pairs = list(
    c("b_Intercept", "b_Intercept", "Intercept"),
    c("b_x", "b[1]", "b_x"),
    c("sd_group__Intercept", "sd_1[1]", "sd_group_Int"),
    c("sd_group__x", "sd_1[2]", "sd_group_x"),
    c("cor_group__Intercept__x", "Lcorr_1[2,1]", "cor_group"),
    c("ar[1]", "ar1_trend[1]", "AR(1)"),
    c("sderr", "sigma_trend[1]", "Sigma")
  )
)

# -----------------------------------------------------------------------------
# Test 8: Gaussian Process (gp())
# -----------------------------------------------------------------------------

cat("\n=== Test 8: Gaussian Process (gp()) ===\n")

brms_8 <- fit_brms_cached(
  "ar1_gp",
  y ~ 1 + gp(z, k = 5) + ar(time = time, p = 1, cov = TRUE),
  test_data, poisson()
)

mvgam_8 <- fit_mvgam_cached(
  "ar1_gp",
  y ~ 1 + gp(z, k = 5), ~ AR(p = 1),
  test_data, poisson()
)

results$test8 <- run_validation(
  "AR(1) + Gaussian Process",
  brms_8, mvgam_8, test_data,
  param_pairs = list(
    c("b_Intercept", "b_Intercept", "Intercept"),
    c("sdgp_gpz", "sdgp_1[1]", "sdgp"),
    c("lscale_gpz", "lscale_1[1]", "lscale"),
    c("ar[1]", "ar1_trend[1]", "AR(1)"),
    c("sderr", "sigma_trend[1]", "Sigma")
  )
)

# =============================================================================
# TREND FORMULA TESTS
# Effects in trend_formula instead of observation formula
# These test combined obs + trend extraction
# =============================================================================

cat("\n\n")
cat(rep("=", 60), "\n", sep = "")
cat("TREND FORMULA TESTS (effects in trend, intercept in obs)\n")
cat(rep("=", 60), "\n", sep = "")

# -----------------------------------------------------------------------------
# Test 2T: Fixed effect in trend formula
# -----------------------------------------------------------------------------

cat("\n=== Test 2T: AR(1) with fixed effect in TREND ===\n")

mvgam_2t <- fit_mvgam_cached(
  "ar1_fx_trend",
  y ~ 1, ~ x + AR(p = 1),
  test_data, poisson()
)

results$test2t <- run_validation(
  "AR(1) + fixed (in trend)",
  brms_2, mvgam_2t, test_data,
  param_pairs = list(
    c("b_Intercept", "b_Intercept", "Intercept"),
    c("b_x", "b_trend[1]", "b_x"),
    c("ar[1]", "ar1_trend[1]", "AR(1)"),
    c("sderr", "sigma_trend[1]", "Sigma")
  ),
  extract_fn = extract_mvgam_combined_pred
)

# -----------------------------------------------------------------------------
# Test 3T: Fixed + random in trend formula
# -----------------------------------------------------------------------------

cat("\n=== Test 3T: AR(1) with fixed + random in TREND ===\n")

mvgam_3t <- fit_mvgam_cached(
  "ar1_re_trend",
  y ~ 1, ~ x + (1 | group) + AR(p = 1),
  test_data, poisson()
)

results$test3t <- run_validation(
  "AR(1) + fixed + random (in trend)",
  brms_3, mvgam_3t, test_data,
  param_pairs = list(
    c("b_Intercept", "b_Intercept", "Intercept"),
    c("b_x", "b_trend[1]", "b_x"),
    c("sd_group__Intercept", "sd_1_trend[1]", "sd_group"),
    c("ar[1]", "ar1_trend[1]", "AR(1)"),
    c("sderr", "sigma_trend[1]", "Sigma")
  ),
  extract_fn = extract_mvgam_combined_pred
)

# -----------------------------------------------------------------------------
# Test 4T: Fixed + random + smooth in trend formula
# -----------------------------------------------------------------------------

cat("\n=== Test 4T: AR(1) + fixed + random + smooth in TREND ===\n")

mvgam_4t <- fit_mvgam_cached(
  "ar1_re_smooth_trend",
  y ~ 1, ~ x + s(z, k = 5) + (1 | group) + AR(p = 1),
  test_data, poisson()
)

results$test4t <- run_validation(
  "AR(1) + fixed + random + smooth (in trend)",
  brms_4, mvgam_4t, test_data,
  param_pairs = list(
    c("b_Intercept", "b_Intercept", "Intercept"),
    c("b_x", "b_trend[1]", "b_x"),
    c("sd_group__Intercept", "sd_1_trend[1]", "sd_group"),
    c("sds_sz_1", "sds_1_trend[1]", "sds_smooth"),
    c("ar[1]", "ar1_trend[1]", "AR(1)"),
    c("sderr", "sigma_trend[1]", "Sigma")
  ),
  extract_fn = extract_mvgam_combined_pred
)

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n=== VALIDATION SUMMARY ===\n")
n_passed <- sum(sapply(results, function(x) x$passed))
n_total <- length(results)
cat(sprintf("Passed: %d / %d\n", n_passed, n_total))

for (r in results) {
  status <- if (r$passed) "PASS" else "FAIL"
  cat(sprintf("  [%s] %s\n", status, r$name))
}

cat("\n=== COMPLETE ===\n")
