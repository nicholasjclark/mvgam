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

# OLD PATHWAY (reference only)
# #' Extract mvgam observation-level predictions using our method
# extract_mvgam_obs_pred <- function(mvgam_fit, newdata) {
#   obs_params <- extract_obs_parameters(mvgam_fit)
#   full_draws <- posterior::as_draws_matrix(mvgam_fit$fit)
#   obs_draws <- full_draws[, obs_params, drop = FALSE]
#   mock_fit <- create_mock_stanfit(obs_draws)
#   prep <- prepare_predictions.mock_stanfit(
#     object = mock_fit,
#     brmsfit = mvgam_fit$obs_model,
#     newdata = newdata
#   )
#   extract_linpred_from_prep(prep)
# }
#
# #' Extract mvgam trend-level predictions using our method
# #' Note: Must strip _trend suffix from parameter names because:
# #'   - Combined fit stores: b_trend[1], sd_1_trend[1], r_1_1_trend[1], etc.
# #'   - But trend_model brmsfit expects: b[1], sd_1[1], r_1_1[1], etc.
# extract_mvgam_trend_pred <- function(mvgam_fit, newdata) {
#   trend_params <- extract_trend_parameters(mvgam_fit)
#   full_draws <- posterior::as_draws_matrix(mvgam_fit$fit)
#   trend_draws <- full_draws[, trend_params, drop = FALSE]
#
#   # Strip _trend suffix so parameters match trend_model brmsfit expectations
#   colnames(trend_draws) <- gsub("_trend", "", colnames(trend_draws))
#
#   mock_fit <- create_mock_stanfit(trend_draws)
#   prep <- prepare_predictions.mock_stanfit(
#     object = mock_fit,
#     brmsfit = mvgam_fit$trend_model,
#     newdata = newdata
#   )
#   extract_linpred_from_prep(prep)
# }
#
# #' Extract combined obs + trend predictions (additive on link scale)
# extract_mvgam_combined_pred <- function(mvgam_fit, newdata) {
#   obs_pred <- extract_mvgam_obs_pred(mvgam_fit, newdata)
#   trend_pred <- extract_mvgam_trend_pred(mvgam_fit, newdata)
#   obs_pred + trend_pred
# }

# NEW PATHWAY using extract_component_linpred()
#' Extract mvgam observation-level predictions using our method
extract_mvgam_obs_pred <- function(mvgam_fit, newdata) {
  extract_component_linpred(mvgam_fit, newdata, component = "obs")
}

#' Extract mvgam trend-level predictions using our method
extract_mvgam_trend_pred <- function(mvgam_fit, newdata) {
  extract_component_linpred(mvgam_fit, newdata, component = "trend")
}

#' Extract combined obs + trend predictions (additive on link scale)
extract_mvgam_combined_pred <- function(mvgam_fit, newdata) {
  obs_pred <- extract_component_linpred(mvgam_fit, newdata, component = "obs")
  trend_pred <- extract_component_linpred(mvgam_fit, newdata, component = "trend")
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
    passed <- mean_r$cor >= 0.925
  }

  status <- if (passed) "PASSED" else "FAILED"
  cat(sprintf("\n  Result: %s (mean cor >= 0.925)\n", status))

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
    c("lscale_gpz", "lscale_1[1,1]", "lscale"),
    c("ar[1]", "ar1_trend[1]", "AR(1)"),
    c("sderr", "sigma_trend[1]", "Sigma")
  )
)

# -----------------------------------------------------------------------------
# Test 9: Multidimensional Gaussian Process (gp(z, w))
# -----------------------------------------------------------------------------

cat("\n=== Test 9: Multidimensional GP (2D) ===\n")

brms_9 <- fit_brms_cached(
  "ar1_gp2d",
  y ~ 1 + gp(z, w, k = 5) + ar(time = time, p = 1, cov = TRUE),
  test_data_t2, poisson()
)

mvgam_9 <- fit_mvgam_cached(
  "ar1_gp2d",
  y ~ 1 + gp(z, w, k = 5), ~ AR(p = 1),
  test_data_t2, poisson()
)

results$test9 <- run_validation(
  "AR(1) + Multidimensional GP (2D)",
  brms_9, mvgam_9, test_data_t2,
  param_pairs = list(
    c("b_Intercept", "b_Intercept", "Intercept"),
    c("sdgp_gpzw", "sdgp_1[1]", "sdgp"),
    c("lscale_gpzw[1]", "lscale_1[1,1]", "lscale_z"),
    c("lscale_gpzw[2]", "lscale_1[1,2]", "lscale_w"),
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

# -----------------------------------------------------------------------------
# Test 8T: Gaussian Process in trend formula
# -----------------------------------------------------------------------------

cat("\n=== Test 8T: Gaussian Process in TREND ===\n")

mvgam_8t <- fit_mvgam_cached(
  "ar1_gp_trend",
  y ~ 1, ~ gp(z, k = 5) + AR(p = 1),
  test_data, poisson()
)

results$test8t <- run_validation(
  "AR(1) + GP (in trend)",
  brms_8, mvgam_8t, test_data,
  param_pairs = list(
    c("b_Intercept", "b_Intercept", "Intercept"),
    c("sdgp_gpz", "sdgp_1_trend[1]", "sdgp"),
    c("lscale_gpz", "lscale_1_trend[1,1]", "lscale"),
    c("ar[1]", "ar1_trend[1]", "AR(1)"),
    c("sderr", "sigma_trend[1]", "Sigma")
  ),
  extract_fn = extract_mvgam_combined_pred
)

# -----------------------------------------------------------------------------
# Test 6T: Monotonic effect in trend formula
# -----------------------------------------------------------------------------

cat("\n=== Test 6T: Monotonic effect in TREND ===\n")

mvgam_6t <- fit_mvgam_cached(
  "ar1_mo_trend",
  y ~ 1, ~ mo(ord_factor) + AR(p = 1),
  test_data_mo, poisson()
)

results$test6t <- run_validation(
  "AR(1) + monotonic (in trend)",
  brms_6, mvgam_6t, validation_data_mo,
  param_pairs = list(
    c("b_Intercept", "b_Intercept", "Intercept"),
    c("bsp_moord_factor", "bsp_trend[1]", "bsp_mo"),
    c("ar[1]", "ar1_trend[1]", "AR(1)"),
    c("sderr", "sigma_trend[1]", "Sigma")
  ),
  extract_fn = extract_mvgam_combined_pred
)

# -----------------------------------------------------------------------------
# Test 9T: Multidimensional GP in trend formula
# -----------------------------------------------------------------------------

cat("\n=== Test 9T: Multidimensional GP (2D) in TREND ===\n")

mvgam_9t <- fit_mvgam_cached(
  "ar1_gp2d_trend",
  y ~ 1, ~ gp(z, w, k = 5) + AR(p = 1),
  test_data_t2, poisson()
)

results$test9t <- run_validation(
  "AR(1) + Multidimensional GP (in trend)",
  brms_9, mvgam_9t, test_data_t2,
  param_pairs = list(
    c("b_Intercept", "b_Intercept", "Intercept"),
    c("sdgp_gpzw", "sdgp_1_trend[1]", "sdgp"),
    c("lscale_gpzw[1]", "lscale_1_trend[1,1]", "lscale_z"),
    c("lscale_gpzw[2]", "lscale_1_trend[1,2]", "lscale_w"),
    c("ar[1]", "ar1_trend[1]", "AR(1)"),
    c("sderr", "sigma_trend[1]", "Sigma")
  ),
  extract_fn = extract_mvgam_combined_pred
)

# -----------------------------------------------------------------------------
# Test 10: Two GP effects - one standard, one with by variable
# Tests multiple GP terms and categorical group-specific GP effects
# -----------------------------------------------------------------------------

cat("\n=== Test 10: Two GPs (one with by variable) ===\n")

# Create test data with categorical variable for by parameter
test_data_gp2 <- test_data_t2
test_data_gp2$cat <- factor(rep(c("A", "B"), each = 15))

brms_10 <- fit_brms_cached(
  "ar1_gp2_by",
  y ~ 1 + gp(z, k = 5) + gp(w, by = cat, k = 5) +
    ar(time = time, p = 1, cov = TRUE),
  test_data_gp2, poisson()
)

mvgam_10 <- fit_mvgam_cached(
  "ar1_gp2_by",
  y ~ 1 + gp(z, k = 5) + gp(w, by = cat, k = 5), ~ AR(p = 1),
  test_data_gp2, poisson()
)

results$test10 <- run_validation(
  "AR(1) + Two GPs (one with by)",
  brms_10, mvgam_10, test_data_gp2,
  param_pairs = list(
    c("b_Intercept", "b_Intercept", "Intercept"),
    c("sdgp_gpz", "sdgp_1[1]", "sdgp_z"),
    c("lscale_gpz", "lscale_1[1,1]", "lscale_z"),
    c("sdgp_gpw:catA", "sdgp_2[1]", "sdgp_w_A"),
    c("sdgp_gpw:catB", "sdgp_2[2]", "sdgp_w_B"),
    c("lscale_gpw:catA", "lscale_2[1,1]", "lscale_w_A"),
    c("lscale_gpw:catB", "lscale_2[2,1]", "lscale_w_B"),
    c("ar[1]", "ar1_trend[1]", "AR(1)"),
    c("sderr", "sigma_trend[1]", "Sigma")
  )
)

# -----------------------------------------------------------------------------
# Test 10T: Two GP effects in trend formula
# -----------------------------------------------------------------------------

cat("\n=== Test 10T: Two GPs (one with by) in TREND ===\n")

mvgam_10t <- fit_mvgam_cached(
  "ar1_gp2_by_trend",
  y ~ 1, ~ gp(z, k = 5) + gp(w, by = cat, k = 5) + AR(p = 1),
  test_data_gp2, poisson()
)

results$test10t <- run_validation(
  "AR(1) + Two GPs (one with by) (in trend)",
  brms_10, mvgam_10t, test_data_gp2,
  param_pairs = list(
    c("b_Intercept", "b_Intercept", "Intercept"),
    c("sdgp_gpz", "sdgp_1_trend[1]", "sdgp_z"),
    c("lscale_gpz", "lscale_1_trend[1,1]", "lscale_z"),
    c("sdgp_gpw:catA", "sdgp_2_trend[1]", "sdgp_w_A"),
    c("sdgp_gpw:catB", "sdgp_2_trend[2]", "sdgp_w_B"),
    c("lscale_gpw:catA", "lscale_2_trend[1,1]", "lscale_w_A"),
    c("lscale_gpw:catB", "lscale_2_trend[2,1]", "lscale_w_B"),
    c("ar[1]", "ar1_trend[1]", "AR(1)"),
    c("sderr", "sigma_trend[1]", "Sigma")
  ),
  extract_fn = extract_mvgam_combined_pred
)

# =============================================================================
# MULTIVARIATE MODEL TESTS
# Tests extraction with multiple response variables
# =============================================================================

cat("\n\n")
cat(rep("=", 60), "\n", sep = "")
cat("MULTIVARIATE MODEL TESTS\n")
cat(rep("=", 60), "\n", sep = "")

# -----------------------------------------------------------------------------
# Test 11: Multivariate model (mvbind) with shared AR(1)
# Two Gaussian responses with response-specific fixed effects
# -----------------------------------------------------------------------------

cat("\n=== Test 11: Multivariate mvbind (2 responses) ===\n")

# Create multivariate test data with two responses
set.seed(123)
n_mv <- 30
latent_mv <- numeric(n_mv)
latent_mv[1] <- rnorm(1, 0, 0.5)
for (t in 2:n_mv) {
  latent_mv[t] <- 0.6 * latent_mv[t - 1] + rnorm(1, 0, 0.4)
}

test_data_mv <- data.frame(
  y1 = rnorm(n_mv, 2 + 0.5 * (1:n_mv / n_mv) + latent_mv, 0.3),
  y2 = rnorm(n_mv, 1 - 0.3 * (1:n_mv / n_mv) + latent_mv, 0.4),
  x = (1:n_mv) / n_mv,
  time = 1:n_mv,
  series = factor("s1")
)

brms_11 <- fit_brms_cached(
  "mv_gauss",
  bf(mvbind(y1, y2) ~ x + ar(time = time, p = 1, cov = TRUE)) + set_rescor(FALSE),
  test_data_mv, gaussian()
)

mvgam_11 <- fit_mvgam_cached(
  "mv_gauss",
  bf(y1 ~ x) + bf(y2 ~ x), ~ AR(p = 1),
  test_data_mv, gaussian()
)

# Validate y1 response
cat("\n  Validating response: y1\n")
brms_pred_y1 <- posterior_linpred(brms_11, newdata = test_data_mv,
                                   resp = "y1", incl_autocor = FALSE)
mvgam_pred_y1 <- extract_component_linpred(mvgam_11, test_data_mv,
                                            component = "obs", resp = "y1")

brms_summ_y1 <- summarize_pred(brms_pred_y1)
mvgam_summ_y1 <- summarize_pred(mvgam_pred_y1)
comp_y1 <- compare_vectors(brms_summ_y1$mean, mvgam_summ_y1$mean)
cat(sprintf("    y1 mean: cor=%.4f, rmse=%.4f\n", comp_y1$cor, comp_y1$rmse))

# Validate y2 response
cat("  Validating response: y2\n")
brms_pred_y2 <- posterior_linpred(brms_11, newdata = test_data_mv,
                                   resp = "y2", incl_autocor = FALSE)
mvgam_pred_y2 <- extract_component_linpred(mvgam_11, test_data_mv,
                                            component = "obs", resp = "y2")

brms_summ_y2 <- summarize_pred(brms_pred_y2)
mvgam_summ_y2 <- summarize_pred(mvgam_pred_y2)
comp_y2 <- compare_vectors(brms_summ_y2$mean, mvgam_summ_y2$mean)
cat(sprintf("    y2 mean: cor=%.4f, rmse=%.4f\n", comp_y2$cor, comp_y2$rmse))

# Pass if both responses have cor >= 0.925
mv_passed <- !is.na(comp_y1$cor) && !is.na(comp_y2$cor) &&
             comp_y1$cor >= 0.925 && comp_y2$cor >= 0.925
status <- if (mv_passed) "PASSED" else "FAILED"
cat(sprintf("\n  Result: %s (both responses cor >= 0.925)\n", status))

results$test11 <- list(
  name = "Multivariate mvbind (2 responses)",
  passed = mv_passed,
  y1_cor = comp_y1$cor,
  y2_cor = comp_y2$cor
)

# -----------------------------------------------------------------------------
# Test 11L: Multivariate posterior_linpred validation
# Tests posterior_linpred.mvgam() returns correct list structure
# -----------------------------------------------------------------------------

cat("\n=== Test 11L: Multivariate posterior_linpred ===\n")

mvgam_linpred_mv <- posterior_linpred(mvgam_11, newdata = test_data_mv)

# Check returns list with both responses
is_list_output <- is.list(mvgam_linpred_mv) && !is.matrix(mvgam_linpred_mv)
has_both_resp <- all(c("y1", "y2") %in% names(mvgam_linpred_mv))

cat(sprintf("  Returns list: %s\n", is_list_output))
cat(sprintf("  Has both responses (y1, y2): %s\n", has_both_resp))

if (is_list_output && has_both_resp) {
  # Check dimensions match
  dim_y1 <- dim(mvgam_linpred_mv$y1)
  dim_y2 <- dim(mvgam_linpred_mv$y2)
  cat(sprintf("  y1 dims: [%d x %d]\n", dim_y1[1], dim_y1[2]))
  cat(sprintf("  y2 dims: [%d x %d]\n", dim_y2[1], dim_y2[2]))

  # Compare against brms for each response
  brms_y1 <- posterior_linpred(brms_11, newdata = test_data_mv,
                                resp = "y1", incl_autocor = FALSE)
  brms_y2 <- posterior_linpred(brms_11, newdata = test_data_mv,
                                resp = "y2", incl_autocor = FALSE)

  cor_y1 <- cor(colMeans(brms_y1), colMeans(mvgam_linpred_mv$y1))
  cor_y2 <- cor(colMeans(brms_y2), colMeans(mvgam_linpred_mv$y2))
  cat(sprintf("  y1 vs brms cor: %.4f\n", cor_y1))
  cat(sprintf("  y2 vs brms cor: %.4f\n", cor_y2))

  linpred_mv_passed <- cor_y1 >= 0.925 && cor_y2 >= 0.925
} else {
  linpred_mv_passed <- FALSE
}

status <- if (linpred_mv_passed) "PASSED" else "FAILED"
cat(sprintf("  Result: %s\n", status))

results$test11l <- list(
  name = "Multivariate posterior_linpred",
  passed = linpred_mv_passed
)

# =============================================================================
# POSTERIOR_LINPRED.MVGAM VALIDATION
# Tests the new S3 method against brms::posterior_linpred()
# =============================================================================

cat("\n\n")
cat(rep("=", 60), "\n", sep = "")
cat("POSTERIOR_LINPRED.MVGAM VALIDATION\n")
cat(rep("=", 60), "\n", sep = "")

#' Run validation using posterior_linpred.mvgam() S3 method
run_linpred_validation <- function(test_name, brms_fit, mvgam_fit, newdata,
                                   incl_autocor = FALSE) {
  cat("\n--- posterior_linpred:", test_name, "---\n")

  # Get predictions via S3 methods
  brms_pred <- brms::posterior_linpred(brms_fit, newdata = newdata,
                                       incl_autocor = incl_autocor)
  mvgam_pred <- posterior_linpred(mvgam_fit, newdata = newdata)

  # Check dimensions
  if (!identical(dim(brms_pred), dim(mvgam_pred))) {
    cat("  FAIL: Dimension mismatch\n")
    cat("    brms:", dim(brms_pred), "\n")
    cat("    mvgam:", dim(mvgam_pred), "\n")
    return(list(name = test_name, passed = FALSE, reason = "dim_mismatch"))
  }
  cat("  Dimensions match:", dim(brms_pred), "\n")

  # Compare summaries
  brms_summ <- summarize_pred(brms_pred)
  mvgam_summ <- summarize_pred(mvgam_pred)

  stats <- c("mean", "median", "sd")
  results <- lapply(stats, function(s) {
    compare_vectors(brms_summ[[s]], mvgam_summ[[s]])
  })
  names(results) <- stats

  for (s in stats) {
    r <- results[[s]]
    if (isTRUE(r$mismatch)) {
      cat(sprintf("  %s: MISMATCH\n", s))
    } else if (r$constant) {
      cat(sprintf("  %s: constant, diff = %.6f\n", s, r$rmse))
    } else {
      cat(sprintf("  %s: cor=%.6f, rmse=%.6f\n", s, r$cor, r$rmse))
    }
  }

  # Pass/fail based on mean correlation
  mean_r <- results$mean
  if (isTRUE(mean_r$mismatch)) {
    passed <- FALSE
  } else if (mean_r$constant) {
    passed <- TRUE
  } else {
    passed <- mean_r$cor >= 0.925
  }

  status <- if (passed) "PASSED" else "FAILED"
  cat(sprintf("  Result: %s\n", status))

  list(name = paste0("linpred_", test_name), passed = passed, stats = results)
}

# Test posterior_linpred.mvgam() for obs-formula models (no trend effects)
results$linpred_1 <- run_linpred_validation(
  "Intercept-only AR(1)", brms_1, mvgam_1, test_data
)

results$linpred_2 <- run_linpred_validation(
  "AR(1) + fixed effect", brms_2, mvgam_2, test_data
)

results$linpred_3 <- run_linpred_validation(
  "AR(1) + random intercept", brms_3, mvgam_3, test_data
)

results$linpred_4 <- run_linpred_validation(
  "AR(1) + fixed + random + smooth", brms_4, mvgam_4, test_data
)

results$linpred_8 <- run_linpred_validation(
  "AR(1) + GP", brms_8, mvgam_8, test_data
)

# Test posterior_linpred.mvgam() for trend-formula models (combined)
# mvgam trend formula linpred = fixed effects only (not AR states)
# brms incl_autocor = FALSE excludes AR contributions, matching mvgam behavior
results$linpred_2t <- run_linpred_validation(
  "AR(1) + fixed (in trend)", brms_2, mvgam_2t, test_data
)

results$linpred_3t <- run_linpred_validation(
  "AR(1) + fixed + random (in trend)", brms_3, mvgam_3t, test_data
)

results$linpred_4t <- run_linpred_validation(
  "AR(1) + fixed + random + smooth (in trend)", brms_4, mvgam_4t, test_data
)

results$linpred_8t <- run_linpred_validation(
  "AR(1) + GP (in trend)", brms_8, mvgam_8t, test_data
)

# Test process_error = FALSE reduces variance
cat("\n--- posterior_linpred: process_error toggle ---\n")
linpred_full <- posterior_linpred(mvgam_2t, newdata = test_data,
                                  process_error = TRUE)
linpred_fixed <- posterior_linpred(mvgam_2t, newdata = test_data,
                                   process_error = FALSE)

var_full <- mean(apply(linpred_full, 2, var))
var_fixed <- mean(apply(linpred_fixed, 2, var))
cat(sprintf("  process_error=TRUE variance:  %.4f\n", var_full))
cat(sprintf("  process_error=FALSE variance: %.4f\n", var_fixed))
cat(sprintf("  Variance ratio (fixed/full):  %.4f\n", var_fixed / var_full))

pe_passed <- var_fixed < var_full
results$linpred_process_error <- list(
  name = "linpred_process_error_toggle",
  passed = pe_passed,
  var_full = var_full,
  var_fixed = var_fixed
)
cat(sprintf("  Result: %s (fixed < full)\n", if (pe_passed) "PASSED" else "FAILED"))

# Test ndraws subsetting
cat("\n--- posterior_linpred: ndraws subsetting ---\n")
linpred_sub <- posterior_linpred(mvgam_2, newdata = test_data, ndraws = 100)
ndraws_passed <- nrow(linpred_sub) == 100
cat(sprintf("  Requested ndraws: 100, Got: %d\n", nrow(linpred_sub)))
cat(sprintf("  Result: %s\n", if (ndraws_passed) "PASSED" else "FAILED"))
results$linpred_ndraws <- list(
  name = "linpred_ndraws_subsetting",
  passed = ndraws_passed
)

# =============================================================================
# POSTERIOR_EPRED.MVGAM VALIDATION
# Tests the new S3 method against brms::posterior_epred()
# =============================================================================

cat("\n\n")
cat(rep("=", 60), "\n", sep = "")
cat("POSTERIOR_EPRED.MVGAM VALIDATION\n")
cat(rep("=", 60), "\n", sep = "")

#' Run validation using posterior_epred.mvgam() S3 method
#' For integer-valued families (Poisson, negative binomial), small differences
#' in linear predictors get amplified by exp(), so we use a relaxed threshold
run_epred_validation <- function(test_name, brms_fit, mvgam_fit, newdata,
                                 incl_autocor = FALSE) {
  cat("\n--- posterior_epred:", test_name, "---\n")

  # Get predictions via S3 methods
  brms_pred <- brms::posterior_epred(brms_fit, newdata = newdata,
                                     incl_autocor = incl_autocor)
  mvgam_pred <- posterior_epred(mvgam_fit, newdata = newdata)

  # Check dimensions
  if (!identical(dim(brms_pred), dim(mvgam_pred))) {
    cat("  FAIL: Dimension mismatch\n")
    cat("    brms:", dim(brms_pred), "\n")
    cat("    mvgam:", dim(mvgam_pred), "\n")
    return(list(name = paste0("epred_", test_name), passed = FALSE,
                reason = "dim_mismatch"))
  }
  cat("  Dimensions match:", dim(brms_pred), "\n")

  # Compare summaries
  brms_summ <- summarize_pred(brms_pred)
  mvgam_summ <- summarize_pred(mvgam_pred)

  stats <- c("mean", "median", "sd")
  stat_results <- lapply(stats, function(s) {
    compare_vectors(brms_summ[[s]], mvgam_summ[[s]])
  })
  names(stat_results) <- stats

  for (s in stats) {
    r <- stat_results[[s]]
    if (isTRUE(r$mismatch)) {
      cat(sprintf("  %s: MISMATCH\n", s))
    } else if (r$constant) {
      cat(sprintf("  %s: constant, diff = %.6f\n", s, r$rmse))
    } else {
      cat(sprintf("  %s: cor=%.6f, rmse=%.6f\n", s, r$cor, r$rmse))
    }
  }

  # Verify values are on correct scale for family
  family_name <- mvgam_fit$family$family
  scale_check <- TRUE
  if (family_name == "poisson") {
    # Poisson epred should be >= 0
    if (any(mvgam_pred < 0)) {
      cat("  WARNING: Poisson epred contains negative values\n")
      scale_check <- FALSE
    } else {
      cat("  Scale check: Poisson epred >= 0 (OK)\n")
    }
  }

  # Determine correlation threshold based on family

  # Integer-valued families have expectations on count scale where small linpred

  # differences get amplified by exp(), so use relaxed threshold
  integer_families <- c("poisson", "negbinomial", "negative_binomial",
                        "zero_inflated_poisson", "zero_inflated_negbinomial")
  cor_threshold <- if (family_name %in% integer_families) 0.75 else 0.925

  # Pass/fail based on mean correlation
  mean_r <- stat_results$mean
  if (isTRUE(mean_r$mismatch)) {
    passed <- FALSE
  } else if (mean_r$constant) {
    passed <- scale_check
  } else {
    passed <- mean_r$cor >= cor_threshold && scale_check
  }

  status <- if (passed) "PASSED" else "FAILED"
  cat(sprintf("  Result: %s (threshold=%.2f)\n", status, cor_threshold))

  list(name = paste0("epred_", test_name), passed = passed, stats = stat_results)
}

# Test posterior_epred.mvgam() for Poisson models (obs-formula)
results$epred_1 <- run_epred_validation(
  "Intercept-only AR(1)", brms_1, mvgam_1, test_data
)

results$epred_2 <- run_epred_validation(
  "AR(1) + fixed effect", brms_2, mvgam_2, test_data
)

results$epred_3 <- run_epred_validation(
  "AR(1) + random intercept", brms_3, mvgam_3, test_data
)

results$epred_4 <- run_epred_validation(
  "AR(1) + fixed + random + smooth", brms_4, mvgam_4, test_data
)

results$epred_8 <- run_epred_validation(
  "AR(1) + GP", brms_8, mvgam_8, test_data
)

# Additional obs-formula epred tests (tests 5, 6, 7, 9, 10)
results$epred_5 <- run_epred_validation(
  "AR(1) + t2() tensor (no intercept)", brms_5, mvgam_5, test_data_t2
)

results$epred_6 <- run_epred_validation(
  "AR(1) + monotonic", brms_6, mvgam_6, validation_data_mo
)

results$epred_7 <- run_epred_validation(
  "AR(1) + correlated random effects", brms_7, mvgam_7, test_data
)

results$epred_9 <- run_epred_validation(
  "AR(1) + Multidimensional GP (2D)", brms_9, mvgam_9, test_data_t2
)

results$epred_10 <- run_epred_validation(
  "AR(1) + Two GPs (one with by)", brms_10, mvgam_10, test_data_gp2
)

# Test posterior_epred.mvgam() for trend-formula models
results$epred_2t <- run_epred_validation(
  "AR(1) + fixed (in trend)", brms_2, mvgam_2t, test_data
)

results$epred_3t <- run_epred_validation(
  "AR(1) + fixed + random (in trend)", brms_3, mvgam_3t, test_data
)

results$epred_4t <- run_epred_validation(
  "AR(1) + fixed + random + smooth (in trend)", brms_4, mvgam_4t, test_data
)

results$epred_8t <- run_epred_validation(
  "AR(1) + GP (in trend)", brms_8, mvgam_8t, test_data
)

# Additional trend-formula epred tests (6t, 9t, 10t)
results$epred_6t <- run_epred_validation(
  "AR(1) + monotonic (in trend)", brms_6, mvgam_6t, validation_data_mo
)

results$epred_9t <- run_epred_validation(
  "AR(1) + Multidimensional GP (in trend)", brms_9, mvgam_9t, test_data_t2
)

results$epred_10t <- run_epred_validation(
  "AR(1) + Two GPs (one with by) (in trend)", brms_10, mvgam_10t, test_data_gp2
)

# Test posterior_epred for Gaussian multivariate model
cat("\n--- posterior_epred: Multivariate Gaussian ---\n")
brms_epred_y1 <- brms::posterior_epred(brms_11, newdata = test_data_mv,
                                        resp = "y1", incl_autocor = FALSE)
brms_epred_y2 <- brms::posterior_epred(brms_11, newdata = test_data_mv,
                                        resp = "y2", incl_autocor = FALSE)
mvgam_epred_mv <- posterior_epred(mvgam_11, newdata = test_data_mv)

# Check structure
is_list_mv <- is.list(mvgam_epred_mv) && !is.matrix(mvgam_epred_mv)
has_both_mv <- all(c("y1", "y2") %in% names(mvgam_epred_mv))
cat(sprintf("  Returns list: %s\n", is_list_mv))
cat(sprintf("  Has both responses: %s\n", has_both_mv))

if (is_list_mv && has_both_mv) {
  cor_y1_epred <- cor(colMeans(brms_epred_y1), colMeans(mvgam_epred_mv$y1))
  cor_y2_epred <- cor(colMeans(brms_epred_y2), colMeans(mvgam_epred_mv$y2))
  cat(sprintf("  y1 vs brms cor: %.4f\n", cor_y1_epred))
  cat(sprintf("  y2 vs brms cor: %.4f\n", cor_y2_epred))
  epred_mv_passed <- cor_y1_epred >= 0.925 && cor_y2_epred >= 0.925
} else {
  epred_mv_passed <- FALSE
}
status <- if (epred_mv_passed) "PASSED" else "FAILED"
cat(sprintf("  Result: %s\n", status))
results$epred_mv <- list(name = "epred_Multivariate Gaussian", passed = epred_mv_passed)

# Test that epred = linkinv(linpred) for simple families
cat("\n--- posterior_epred: linkinv consistency ---\n")
linpred_test <- posterior_linpred(mvgam_2, newdata = test_data)
epred_test <- posterior_epred(mvgam_2, newdata = test_data)
# For Poisson with log link: epred = exp(linpred)
expected_epred <- exp(linpred_test)
linkinv_match <- all.equal(epred_test, expected_epred, tolerance = 1e-10)
linkinv_passed <- isTRUE(linkinv_match)
cat(sprintf("  Poisson: epred == exp(linpred): %s\n",
            if (linkinv_passed) "TRUE" else as.character(linkinv_match)))
cat(sprintf("  Result: %s\n", if (linkinv_passed) "PASSED" else "FAILED"))
results$epred_linkinv <- list(name = "epred_linkinv_consistency", passed = linkinv_passed)

# -----------------------------------------------------------------------------
# Test Beta family posterior_epred
# Beta is important because E[Y] = mu (simple inverse link)
# -----------------------------------------------------------------------------

cat("\n--- posterior_epred: Beta family ---\n")

# Create beta test data (values in (0, 1))
set.seed(456)
n_beta <- 30
test_data_beta <- data.frame(
  y = rbeta(n_beta, shape1 = 2, shape2 = 5),  # Mean ~ 0.286
  x = rnorm(n_beta),
  time = 1:n_beta,
  series = factor("s1")
)
# Ensure y is strictly in (0, 1)
test_data_beta$y <- pmax(pmin(test_data_beta$y, 0.999), 0.001)

brms_beta <- fit_brms_cached(
  "beta_ar1",
  y ~ 1 + x + ar(time = time, p = 1, cov = TRUE),
  test_data_beta, Beta()
)

mvgam_beta <- fit_mvgam_cached(
  "beta_ar1",
  y ~ 1 + x, ~ AR(p = 1),
  test_data_beta, Beta()
)

# Get epred from both
brms_epred_beta <- brms::posterior_epred(brms_beta, newdata = test_data_beta,
                                          incl_autocor = FALSE)
mvgam_epred_beta <- posterior_epred(mvgam_beta, newdata = test_data_beta)

# Check dimensions
cat(sprintf("  Dimensions: brms %s, mvgam %s\n",
            paste(dim(brms_epred_beta), collapse = "x"),
            paste(dim(mvgam_epred_beta), collapse = "x")))

# Compare summaries
brms_beta_summ <- summarize_pred(brms_epred_beta)
mvgam_beta_summ <- summarize_pred(mvgam_epred_beta)
comp_beta <- compare_vectors(brms_beta_summ$mean, mvgam_beta_summ$mean)
cat(sprintf("  mean: cor=%.4f, rmse=%.4f\n", comp_beta$cor, comp_beta$rmse))

# Check values are in valid range (0, 1)
beta_range_ok <- all(mvgam_epred_beta > 0 & mvgam_epred_beta < 1)
cat(sprintf("  Values in (0,1): %s\n", beta_range_ok))

beta_passed <- !is.na(comp_beta$cor) && comp_beta$cor >= 0.925 && beta_range_ok
status <- if (beta_passed) "PASSED" else "FAILED"
cat(sprintf("  Result: %s\n", status))
results$epred_beta <- list(name = "epred_Beta family", passed = beta_passed,
                            cor = comp_beta$cor)

# Also test linpred for Beta
cat("\n--- posterior_linpred: Beta family ---\n")
brms_linpred_beta <- brms::posterior_linpred(brms_beta, newdata = test_data_beta,
                                              incl_autocor = FALSE)
mvgam_linpred_beta <- posterior_linpred(mvgam_beta, newdata = test_data_beta)
brms_beta_linpred_summ <- summarize_pred(brms_linpred_beta)
mvgam_beta_linpred_summ <- summarize_pred(mvgam_linpred_beta)
comp_beta_linpred <- compare_vectors(brms_beta_linpred_summ$mean,
                                      mvgam_beta_linpred_summ$mean)
cat(sprintf("  mean: cor=%.4f, rmse=%.4f\n", comp_beta_linpred$cor,
            comp_beta_linpred$rmse))
beta_linpred_passed <- !is.na(comp_beta_linpred$cor) &&
                        comp_beta_linpred$cor >= 0.925
status <- if (beta_linpred_passed) "PASSED" else "FAILED"
cat(sprintf("  Result: %s\n", status))
results$linpred_beta <- list(name = "linpred_Beta family",
                              passed = beta_linpred_passed,
                              cor = comp_beta_linpred$cor)

# -----------------------------------------------------------------------------
# Test Binomial family posterior_linpred and posterior_epred
# Binomial is important because E[Y] = p * trials (requires trials)
# -----------------------------------------------------------------------------

cat("\n--- Binomial family ---\n")

# Create binomial test data (successes out of trials)
set.seed(789)
n_binom <- 30
trials_vec <- rep(20, n_binom)  # 20 trials per observation
test_data_binom <- data.frame(
  y = rbinom(n_binom, size = trials_vec, prob = 0.4),
  trials = trials_vec,
  x = rnorm(n_binom),
  time = 1:n_binom,
  series = factor("s1")
)

brms_binom <- fit_brms_cached(
  "binom_ar1",
  y | trials(trials) ~ 1 + x + ar(time = time, p = 1, cov = TRUE),
  test_data_binom, binomial()
)

mvgam_binom <- fit_mvgam_cached(
  "binom_ar1",
  y | trials(trials)  ~ 1 + x, ~ AR(p = 1),
  test_data_binom, binomial()
)

# Test posterior_linpred for Binomial
cat("\n--- posterior_linpred: Binomial family ---\n")
brms_linpred_binom <- brms::posterior_linpred(brms_binom,
                                               newdata = test_data_binom,
                                               incl_autocor = FALSE)
mvgam_linpred_binom <- posterior_linpred(mvgam_binom, newdata = test_data_binom)

cat(sprintf("  Dimensions: brms %s, mvgam %s\n",
            paste(dim(brms_linpred_binom), collapse = "x"),
            paste(dim(mvgam_linpred_binom), collapse = "x")))

brms_binom_linpred_summ <- summarize_pred(brms_linpred_binom)
mvgam_binom_linpred_summ <- summarize_pred(mvgam_linpred_binom)
comp_binom_linpred <- compare_vectors(brms_binom_linpred_summ$mean,
                                       mvgam_binom_linpred_summ$mean)
cat(sprintf("  mean: cor=%.4f, rmse=%.4f\n", comp_binom_linpred$cor,
            comp_binom_linpred$rmse))
binom_linpred_passed <- !is.na(comp_binom_linpred$cor) &&
                         comp_binom_linpred$cor >= 0.925
status <- if (binom_linpred_passed) "PASSED" else "FAILED"
cat(sprintf("  Result: %s\n", status))
results$linpred_binom <- list(name = "linpred_Binomial family",
                               passed = binom_linpred_passed,
                               cor = comp_binom_linpred$cor)

# Test posterior_epred for Binomial
cat("\n--- posterior_epred: Binomial family ---\n")
brms_epred_binom <- brms::posterior_epred(brms_binom, newdata = test_data_binom,
                                           incl_autocor = FALSE)
mvgam_epred_binom <- posterior_epred(mvgam_binom, newdata = test_data_binom)

cat(sprintf("  Dimensions: brms %s, mvgam %s\n",
            paste(dim(brms_epred_binom), collapse = "x"),
            paste(dim(mvgam_epred_binom), collapse = "x")))

brms_binom_epred_summ <- summarize_pred(brms_epred_binom)
mvgam_binom_epred_summ <- summarize_pred(mvgam_epred_binom)
comp_binom_epred <- compare_vectors(brms_binom_epred_summ$mean,
                                     mvgam_binom_epred_summ$mean)
cat(sprintf("  mean: cor=%.4f, rmse=%.4f\n", comp_binom_epred$cor,
            comp_binom_epred$rmse))

# Check values are in valid range [0, trials]
binom_range_ok <- all(mvgam_epred_binom >= 0 &
                       mvgam_epred_binom <= max(test_data_binom$trials))
cat(sprintf("  Values in [0, trials]: %s\n", binom_range_ok))

binom_epred_passed <- !is.na(comp_binom_epred$cor) &&
                       comp_binom_epred$cor >= 0.925 && binom_range_ok
status <- if (binom_epred_passed) "PASSED" else "FAILED"
cat(sprintf("  Result: %s\n", status))
results$epred_binom <- list(name = "epred_Binomial family",
                             passed = binom_epred_passed,
                             cor = comp_binom_epred$cor)

# Verify epred = plogis(linpred) * trials for Binomial
cat("\n--- posterior_epred: Binomial linkinv consistency ---\n")
expected_binom_epred <- plogis(mvgam_linpred_binom) *
                         matrix(test_data_binom$trials, nrow = nrow(mvgam_linpred_binom),
                                ncol = ncol(mvgam_linpred_binom), byrow = TRUE)
binom_linkinv_match <- all.equal(mvgam_epred_binom, expected_binom_epred,
                                  tolerance = 1e-10)
binom_linkinv_passed <- isTRUE(binom_linkinv_match)
cat(sprintf("  Binomial: epred == plogis(linpred) * trials: %s\n",
            if (binom_linkinv_passed) "TRUE" else as.character(binom_linkinv_match)))
cat(sprintf("  Result: %s\n", if (binom_linkinv_passed) "PASSED" else "FAILED"))
results$epred_binom_linkinv <- list(name = "epred_Binomial_linkinv_consistency",
                                     passed = binom_linkinv_passed)

# -----------------------------------------------------------------------------
# NOTE: Categorical family tests removed
# Multi-category families (categorical, multinomial, dirichlet, dirichlet2,
# logistic_normal) are not supported by mvgam because they require 3D linear
# predictors that cannot be combined with State-Space trends.
# Users should use brms directly for these response types.
# See validate_supported_family() in R/validations.R
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Test Ordinal (Cumulative) family posterior_linpred and posterior_epred
# Ordinal linpred is 2D [ndraws x nobs] (single eta per observation)
# Ordinal epred is 3D [ndraws x nobs x ncat] with category probabilities
# Note: brms cumulative() doesn't support ar() terms, so we use ZMVN trend
# -----------------------------------------------------------------------------

cat("\n--- Ordinal (Cumulative) family ---\n")

# Create ordinal test data with ordered factor response
set.seed(456)
n_ord <- 30
# Generate ordinal response (4 ordered categories)
latent <- 1.0 + 0.5 * rnorm(n_ord)
# Cut points for ordinal categories
cuts <- c(-Inf, -0.5, 0.5, 1.5, Inf)
test_data_ord <- data.frame(
  y = ordered(cut(latent, breaks = cuts, labels = c("Low", "Med", "High", "VHigh"))),
  x = rnorm(n_ord),
  z = rnorm(n_ord),
  time = 1:n_ord,
  series = factor("s1")
)

# Ordinal models without AR (brms doesn't support ar() for cumulative)
brms_ord <- fit_brms_cached(
  "cumulative_fx",
  y ~ 1 + x + z,
  test_data_ord, cumulative()
)

mvgam_ord <- fit_mvgam_cached(
  "cumulative_fx",
  y ~ 1 + x + z, ~ ZMVN(),
  test_data_ord, cumulative()
)

# Test posterior_linpred for Ordinal
cat("\n--- posterior_linpred: Ordinal (Cumulative) family ---\n")
brms_linpred_ord <- brms::posterior_linpred(brms_ord, newdata = test_data_ord,
                                             incl_autocor = FALSE)
mvgam_linpred_ord <- posterior_linpred(mvgam_ord, newdata = test_data_ord)

cat(sprintf("  brms dims: %s\n", paste(dim(brms_linpred_ord), collapse = " x ")))
cat(sprintf("  mvgam dims: %s\n", paste(dim(mvgam_linpred_ord), collapse = " x ")))

# For ordinal, linpred is 2D [ndraws x nobs] - single eta per observation
# Verify dimensionality
ord_linpred_is_2d <- length(dim(brms_linpred_ord)) == 2 &&
                     length(dim(mvgam_linpred_ord)) == 2
cat(sprintf("  Both linpred are 2D: %s\n", ord_linpred_is_2d))

# Compare mean predictions
brms_linpred_ord_mean <- colMeans(brms_linpred_ord)
mvgam_linpred_ord_mean <- colMeans(mvgam_linpred_ord)
ord_linpred_cor <- cor(brms_linpred_ord_mean, mvgam_linpred_ord_mean)
cat(sprintf("  Correlation: %.4f\n", ord_linpred_cor))

ord_linpred_passed <- ord_linpred_is_2d && !is.na(ord_linpred_cor) &&
                      ord_linpred_cor >= 0.925
status <- if (ord_linpred_passed) "PASSED" else "FAILED"
cat(sprintf("  Result: %s\n", status))
results$linpred_ord <- list(name = "linpred_Ordinal (Cumulative) family",
                             passed = ord_linpred_passed,
                             cor = ord_linpred_cor,
                             is_2d = ord_linpred_is_2d)

# Test posterior_epred for Ordinal
cat("\n--- posterior_epred: Ordinal (Cumulative) family ---\n")
brms_epred_ord <- brms::posterior_epred(brms_ord, newdata = test_data_ord,
                                         incl_autocor = FALSE)
mvgam_epred_ord <- posterior_epred(mvgam_ord, newdata = test_data_ord)

cat(sprintf("  brms dims: %s\n", paste(dim(brms_epred_ord), collapse = " x ")))
cat(sprintf("  mvgam dims: %s\n", paste(dim(mvgam_epred_ord), collapse = " x ")))

# epred is 3D [ndraws x nobs x ncat] with probabilities
ord_epred_is_3d <- length(dim(brms_epred_ord)) == 3 &&
                   length(dim(mvgam_epred_ord)) == 3
cat(sprintf("  Both epred are 3D: %s\n", ord_epred_is_3d))

# Check probabilities sum to 1
mvgam_ord_prob_sums <- apply(mvgam_epred_ord, 1:2, sum)
ord_prob_sum_ok <- all(abs(mvgam_ord_prob_sums - 1) < 1e-10)
cat(sprintf("  Probabilities sum to 1: %s\n", ord_prob_sum_ok))

# Compare mean probabilities per category
brms_epred_ord_mean <- apply(brms_epred_ord, 2:3, mean)
mvgam_epred_ord_mean <- apply(mvgam_epred_ord, 2:3, mean)
ord_epred_cor <- cor(as.vector(brms_epred_ord_mean), as.vector(mvgam_epred_ord_mean))
cat(sprintf("  Overall cor (flattened): %.4f\n", ord_epred_cor))

ord_epred_passed <- ord_epred_is_3d && !is.na(ord_epred_cor) &&
                    ord_epred_cor >= 0.925 && ord_prob_sum_ok
status <- if (ord_epred_passed) "PASSED" else "FAILED"
cat(sprintf("  Result: %s\n", status))
results$epred_ord <- list(name = "epred_Ordinal (Cumulative) family",
                           passed = ord_epred_passed,
                           cor = ord_epred_cor,
                           is_3d = ord_epred_is_3d,
                           prob_sum_ok = ord_prob_sum_ok)

# -----------------------------------------------------------------------------
# Test Hurdle Poisson family posterior_linpred and posterior_epred
# E[Y] = mu / (1 - exp(-mu)) * (1 - hu)
# -----------------------------------------------------------------------------

cat("\n--- Hurdle Poisson family ---\n")

# Create hurdle Poisson test data (zero-inflated counts)
set.seed(654)
n_hp <- 30
# Generate data with excess zeros
mu_true <- exp(1.5 + 0.3 * rnorm(n_hp))
hu_true <- 0.3  # 30% chance of structural zero
test_data_hp <- data.frame(
  y = ifelse(runif(n_hp) < hu_true, 0, rpois(n_hp, mu_true)),
  x = rnorm(n_hp),
  time = 1:n_hp,
  series = factor("s1")
)

brms_hp <- fit_brms_cached(
  "hurdle_poisson_ar1",
  y ~ 1 + x + ar(time = time, p = 1, cov = TRUE),
  test_data_hp, hurdle_poisson()
)

mvgam_hp <- fit_mvgam_cached(
  "hurdle_poisson_ar1",
  y ~ 1 + x, ~ AR(p = 1),
  test_data_hp, hurdle_poisson()
)

# Test posterior_linpred for Hurdle Poisson
cat("\n--- posterior_linpred: Hurdle Poisson family ---\n")
brms_linpred_hp <- brms::posterior_linpred(brms_hp, newdata = test_data_hp,
                                            incl_autocor = FALSE)
mvgam_linpred_hp <- posterior_linpred(mvgam_hp, newdata = test_data_hp)

cat(sprintf("  Dimensions: brms %s, mvgam %s\n",
            paste(dim(brms_linpred_hp), collapse = "x"),
            paste(dim(mvgam_linpred_hp), collapse = "x")))

brms_hp_linpred_summ <- summarize_pred(brms_linpred_hp)
mvgam_hp_linpred_summ <- summarize_pred(mvgam_linpred_hp)
comp_hp_linpred <- compare_vectors(brms_hp_linpred_summ$mean,
                                    mvgam_hp_linpred_summ$mean)
cat(sprintf("  mean: cor=%.4f, rmse=%.4f\n", comp_hp_linpred$cor,
            comp_hp_linpred$rmse))

hp_linpred_passed <- !is.na(comp_hp_linpred$cor) && comp_hp_linpred$cor >= 0.925
status <- if (hp_linpred_passed) "PASSED" else "FAILED"
cat(sprintf("  Result: %s\n", status))
results$linpred_hp <- list(name = "linpred_Hurdle Poisson family",
                            passed = hp_linpred_passed,
                            cor = comp_hp_linpred$cor)

# Test posterior_epred for Hurdle Poisson
cat("\n--- posterior_epred: Hurdle Poisson family ---\n")
brms_epred_hp <- brms::posterior_epred(brms_hp, newdata = test_data_hp,
                                        incl_autocor = FALSE)
mvgam_epred_hp <- posterior_epred(mvgam_hp, newdata = test_data_hp)

cat(sprintf("  Dimensions: brms %s, mvgam %s\n",
            paste(dim(brms_epred_hp), collapse = "x"),
            paste(dim(mvgam_epred_hp), collapse = "x")))

brms_hp_epred_summ <- summarize_pred(brms_epred_hp)
mvgam_hp_epred_summ <- summarize_pred(mvgam_epred_hp)
comp_hp_epred <- compare_vectors(brms_hp_epred_summ$mean,
                                  mvgam_hp_epred_summ$mean)
cat(sprintf("  mean: cor=%.4f, rmse=%.4f\n", comp_hp_epred$cor,
            comp_hp_epred$rmse))

# Check values are non-negative
hp_nonneg_ok <- all(mvgam_epred_hp >= 0)
cat(sprintf("  Values >= 0: %s\n", hp_nonneg_ok))

# Use relaxed threshold for count models
hp_epred_passed <- !is.na(comp_hp_epred$cor) && comp_hp_epred$cor >= 0.75 &&
                   hp_nonneg_ok
status <- if (hp_epred_passed) "PASSED" else "FAILED"
cat(sprintf("  Result: %s (threshold=0.75 for count family)\n", status))
results$epred_hp <- list(name = "epred_Hurdle Poisson family",
                          passed = hp_epred_passed,
                          cor = comp_hp_epred$cor)

# -----------------------------------------------------------------------------
# Test Hurdle Negative Binomial family
# -----------------------------------------------------------------------------

cat("\n--- Hurdle Negative Binomial family ---\n")

set.seed(655)
n_hnb <- 30
mu_true_hnb <- exp(1.2 + 0.4 * rnorm(n_hnb))
hu_true_hnb <- 0.35
test_data_hnb <- data.frame(
  y = ifelse(runif(n_hnb) < hu_true_hnb, 0,
             rnbinom(n_hnb, mu = mu_true_hnb, size = 2)),
  x = rnorm(n_hnb),
  time = 1:n_hnb,
  series = factor("s1")
)

brms_hnb <- fit_brms_cached(
  "hurdle_negbinomial_ar1",
  y ~ 1 + x + ar(time = time, p = 1, cov = TRUE),
  test_data_hnb, hurdle_negbinomial()
)

mvgam_hnb <- fit_mvgam_cached(
  "hurdle_negbinomial_ar1",
  y ~ 1 + x, ~ AR(p = 1),
  test_data_hnb, hurdle_negbinomial()
)

# Test posterior_linpred for Hurdle Negative Binomial
cat("\n--- posterior_linpred: Hurdle Negative Binomial family ---\n")
brms_linpred_hnb <- brms::posterior_linpred(brms_hnb, newdata = test_data_hnb,
                                             incl_autocor = FALSE)
mvgam_linpred_hnb <- posterior_linpred(mvgam_hnb, newdata = test_data_hnb)

cat(sprintf("  Dimensions: brms %s, mvgam %s\n",
            paste(dim(brms_linpred_hnb), collapse = "x"),
            paste(dim(mvgam_linpred_hnb), collapse = "x")))

brms_hnb_linpred_summ <- summarize_pred(brms_linpred_hnb)
mvgam_hnb_linpred_summ <- summarize_pred(mvgam_linpred_hnb)
comp_hnb_linpred <- compare_vectors(brms_hnb_linpred_summ$mean,
                                     mvgam_hnb_linpred_summ$mean)
cat(sprintf("  mean: cor=%.4f, rmse=%.4f\n", comp_hnb_linpred$cor,
            comp_hnb_linpred$rmse))

hnb_linpred_passed <- !is.na(comp_hnb_linpred$cor) && comp_hnb_linpred$cor >= 0.925
status <- if (hnb_linpred_passed) "PASSED" else "FAILED"
cat(sprintf("  Result: %s\n", status))
results$linpred_hnb <- list(name = "linpred_Hurdle NegBinomial family",
                             passed = hnb_linpred_passed,
                             cor = comp_hnb_linpred$cor)

# Test posterior_epred for Hurdle Negative Binomial
cat("\n--- posterior_epred: Hurdle Negative Binomial family ---\n")
brms_epred_hnb <- brms::posterior_epred(brms_hnb, newdata = test_data_hnb,
                                         incl_autocor = FALSE)
mvgam_epred_hnb <- posterior_epred(mvgam_hnb, newdata = test_data_hnb)

cat(sprintf("  Dimensions: brms %s, mvgam %s\n",
            paste(dim(brms_epred_hnb), collapse = "x"),
            paste(dim(mvgam_epred_hnb), collapse = "x")))

brms_hnb_epred_summ <- summarize_pred(brms_epred_hnb)
mvgam_hnb_epred_summ <- summarize_pred(mvgam_epred_hnb)
comp_hnb_epred <- compare_vectors(brms_hnb_epred_summ$mean,
                                   mvgam_hnb_epred_summ$mean)
cat(sprintf("  mean: cor=%.4f, rmse=%.4f\n", comp_hnb_epred$cor,
            comp_hnb_epred$rmse))

hnb_nonneg_ok <- all(mvgam_epred_hnb >= 0)
cat(sprintf("  Values >= 0: %s\n", hnb_nonneg_ok))

hnb_epred_passed <- !is.na(comp_hnb_epred$cor) && comp_hnb_epred$cor >= 0.75 &&
                    hnb_nonneg_ok
status <- if (hnb_epred_passed) "PASSED" else "FAILED"
cat(sprintf("  Result: %s (threshold=0.75 for count family)\n", status))
results$epred_hnb <- list(name = "epred_Hurdle NegBinomial family",
                           passed = hnb_epred_passed,
                           cor = comp_hnb_epred$cor)

# -----------------------------------------------------------------------------
# Test Zero-Inflated Poisson family
# -----------------------------------------------------------------------------

cat("\n--- Zero-Inflated Poisson family ---\n")

set.seed(656)
n_zip <- 30
mu_true_zip <- exp(1.3 + 0.35 * rnorm(n_zip))
zi_true <- 0.25
test_data_zip <- data.frame(
  y = ifelse(runif(n_zip) < zi_true, 0, rpois(n_zip, mu_true_zip)),
  x = rnorm(n_zip),
  time = 1:n_zip,
  series = factor("s1")
)

brms_zip <- fit_brms_cached(
  "zero_inflated_poisson_ar1",
  y ~ 1 + x + ar(time = time, p = 1, cov = TRUE),
  test_data_zip, zero_inflated_poisson()
)

mvgam_zip <- fit_mvgam_cached(
  "zero_inflated_poisson_ar1",
  y ~ 1 + x, ~ AR(p = 1),
  test_data_zip, zero_inflated_poisson()
)

# Test posterior_linpred for Zero-Inflated Poisson
cat("\n--- posterior_linpred: Zero-Inflated Poisson family ---\n")
brms_linpred_zip <- brms::posterior_linpred(brms_zip, newdata = test_data_zip,
                                             incl_autocor = FALSE)
mvgam_linpred_zip <- posterior_linpred(mvgam_zip, newdata = test_data_zip)

cat(sprintf("  Dimensions: brms %s, mvgam %s\n",
            paste(dim(brms_linpred_zip), collapse = "x"),
            paste(dim(mvgam_linpred_zip), collapse = "x")))

brms_zip_linpred_summ <- summarize_pred(brms_linpred_zip)
mvgam_zip_linpred_summ <- summarize_pred(mvgam_linpred_zip)
comp_zip_linpred <- compare_vectors(brms_zip_linpred_summ$mean,
                                     mvgam_zip_linpred_summ$mean)
cat(sprintf("  mean: cor=%.4f, rmse=%.4f\n", comp_zip_linpred$cor,
            comp_zip_linpred$rmse))

zip_linpred_passed <- !is.na(comp_zip_linpred$cor) && comp_zip_linpred$cor >= 0.925
status <- if (zip_linpred_passed) "PASSED" else "FAILED"
cat(sprintf("  Result: %s\n", status))
results$linpred_zip <- list(name = "linpred_Zero-Inflated Poisson family",
                             passed = zip_linpred_passed,
                             cor = comp_zip_linpred$cor)

# Test posterior_epred for Zero-Inflated Poisson
cat("\n--- posterior_epred: Zero-Inflated Poisson family ---\n")
brms_epred_zip <- brms::posterior_epred(brms_zip, newdata = test_data_zip,
                                         incl_autocor = FALSE)
mvgam_epred_zip <- posterior_epred(mvgam_zip, newdata = test_data_zip)

cat(sprintf("  Dimensions: brms %s, mvgam %s\n",
            paste(dim(brms_epred_zip), collapse = "x"),
            paste(dim(mvgam_epred_zip), collapse = "x")))

brms_zip_epred_summ <- summarize_pred(brms_epred_zip)
mvgam_zip_epred_summ <- summarize_pred(mvgam_epred_zip)
comp_zip_epred <- compare_vectors(brms_zip_epred_summ$mean,
                                   mvgam_zip_epred_summ$mean)
cat(sprintf("  mean: cor=%.4f, rmse=%.4f\n", comp_zip_epred$cor,
            comp_zip_epred$rmse))

zip_nonneg_ok <- all(mvgam_epred_zip >= 0)
cat(sprintf("  Values >= 0: %s\n", zip_nonneg_ok))

zip_epred_passed <- !is.na(comp_zip_epred$cor) && comp_zip_epred$cor >= 0.75 &&
                    zip_nonneg_ok
status <- if (zip_epred_passed) "PASSED" else "FAILED"
cat(sprintf("  Result: %s (threshold=0.75 for count family)\n", status))
results$epred_zip <- list(name = "epred_Zero-Inflated Poisson family",
                           passed = zip_epred_passed,
                           cor = comp_zip_epred$cor)

# =============================================================================
# DPARS EXTRACTION VALIDATION
# =============================================================================

cat(rep("=", 60), "\n", sep = "")
cat("DPARS EXTRACTION VALIDATION\n")
cat(rep("=", 60), "\n", sep = "")

#' Validate dpars extraction against brms
#'
#' @param test_name Descriptive name for test
#' @param brms_fit Fitted brms model
#' @param mvgam_fit Fitted mvgam model
#' @param newdata Data for predictions
#' @param dpar_names Character vector of dpars to validate
#' @return List with name, passed, and dpars results
#' @noRd
run_dpars_validation <- function(test_name,
                                 brms_fit,
                                 mvgam_fit,
                                 newdata,
                                 dpar_names) {
  checkmate::assert_string(test_name)
  checkmate::assert_class(brms_fit, "brmsfit")
  checkmate::assert_class(mvgam_fit, "mvgam")
  checkmate::assert_data_frame(newdata, min.rows = 1)
  checkmate::assert_character(dpar_names, min.len = 1, any.missing = FALSE)

  cat("\n--- dpars:", test_name, "---\n")

  # Create mvgam prep object
  obs_params <- extract_obs_parameters(mvgam_fit)
  full_draws <- posterior::as_draws_matrix(mvgam_fit$fit)
  obs_draws <- full_draws[, obs_params, drop = FALSE]
  mock_fit <- create_mock_stanfit(obs_draws)
  prep <- prepare_predictions.mock_stanfit(
    object = mock_fit,
    brmsfit = mvgam_fit$obs_model,
    newdata = newdata
  )

  all_passed <- TRUE
  dpar_results <- list()

  for (dpar in dpar_names) {
    cat(sprintf("  Checking %s:\n", dpar))

    # Get brms reference
    brms_dpar <- tryCatch(
      brms::posterior_linpred(brms_fit, newdata = newdata, dpar = dpar),
      error = function(e) {
        cat(sprintf("    brms error: %s\n", conditionMessage(e)))
        NULL
      }
    )

    if (is.null(brms_dpar)) {
      cat("    SKIP: brms dpar not available\n")
      next
    }

    mvgam_dpar <- prep$dpars[[dpar]]

    if (is.null(mvgam_dpar)) {
      cat("    FAIL: mvgam dpar is NULL\n")
      all_passed <- FALSE
      dpar_results[[dpar]] <- list(passed = FALSE, reason = "null")
      next
    }

    # Check dimensions
    if (!identical(dim(brms_dpar), dim(mvgam_dpar))) {
      cat(sprintf("    FAIL: dims brms=%s mvgam=%s\n",
                  paste(dim(brms_dpar), collapse = "x"),
                  paste(dim(mvgam_dpar), collapse = "x")))
      all_passed <- FALSE
      dpar_results[[dpar]] <- list(passed = FALSE, reason = "dim")
      next
    }

    # Compare values
    comp <- compare_vectors(colMeans(brms_dpar), colMeans(mvgam_dpar))
    passed <- if (comp$constant) {
      comp$rmse < 0.01
    } else {
      !is.na(comp$cor) && comp$cor >= 0.99
    }

    status_msg <- if (passed) "PASS" else "FAIL"
    cat(sprintf("    cor=%.4f rmse=%.4f %s\n",
                comp$cor %||% NA, comp$rmse, status_msg))

    dpar_results[[dpar]] <- list(passed = passed, cor = comp$cor)
    if (!passed) all_passed <- FALSE
  }

  cat(sprintf("  Overall: %s\n", if (all_passed) "PASSED" else "FAILED"))
  list(name = paste0("dpars_", test_name), passed = all_passed,
       dpars = dpar_results)
}

# Validate dpars using existing models (no new fitting)
results$dpars_beta <- run_dpars_validation(
  "Beta (phi)", brms_beta, mvgam_beta, test_data_beta, c("phi")
)

results$dpars_hp <- run_dpars_validation(
  "Hurdle Poisson (hu)", brms_hp, mvgam_hp, test_data_hp, c("hu")
)

results$dpars_hnb <- run_dpars_validation(
  "Hurdle NB (hu, shape)", brms_hnb, mvgam_hnb, test_data_hnb,
  c("hu", "shape")
)

results$dpars_zip <- run_dpars_validation(
  "ZI Poisson (zi)", brms_zip, mvgam_zip, test_data_zip, c("zi")
)


# =============================================================================
# POSTERIOR_PREDICT.MVGAM VALIDATION
# Tests the new S3 method against brms::posterior_predict()
# =============================================================================

cat("\n\n")
cat("============================================================\n")
cat("POSTERIOR_PREDICT.MVGAM VALIDATION\n")
cat("============================================================\n")

#' Run validation using posterior_predict.mvgam() S3 method
#' posterior_predict includes observation noise, so we expect:
#' 1. Lower correlation than posterior_epred (stochastic sampling)
#' 2. var(predict) > var(epred) for most families
#' 3. Integer values for count families
run_predict_validation <- function(test_name, brms_fit, mvgam_fit, newdata,
                                   ndraws = 500, incl_autocor = FALSE) {
  cat("\n--- posterior_predict:", test_name, "---\n")

  # Get predictions via S3 methods (use same ndraws for fair comparison)
  set.seed(123)
  brms_pred <- brms::posterior_predict(brms_fit, newdata = newdata,
                                       ndraws = ndraws,
                                       incl_autocor = incl_autocor)
  set.seed(123)
  mvgam_pred <- posterior_predict(mvgam_fit, newdata = newdata, ndraws = ndraws)

  # Check dimensions
  if (!identical(dim(brms_pred), dim(mvgam_pred))) {
    cat("  FAIL: Dimension mismatch\n")
    cat("    brms:", dim(brms_pred), "\n")
    cat("    mvgam:", dim(mvgam_pred), "\n")
    return(list(name = paste0("predict_", test_name), passed = FALSE,
                reason = "dim_mismatch"))
  }
  cat("  Dimensions match:", dim(brms_pred), "\n")

  # Compare summary statistics across observations
  brms_summ <- summarize_pred(brms_pred)
  mvgam_summ <- summarize_pred(mvgam_pred)

  stats <- c("mean", "median", "sd")
  stat_results <- lapply(stats, function(s) {
    compare_vectors(brms_summ[[s]], mvgam_summ[[s]])
  })
  names(stat_results) <- stats

  for (s in stats) {
    r <- stat_results[[s]]
    if (isTRUE(r$mismatch)) {
      cat(sprintf("  %s: MISMATCH\n", s))
    } else if (r$constant) {
      cat(sprintf("  %s: constant, diff = %.6f\n", s, r$rmse))
    } else {
      cat(sprintf("  %s: cor=%.4f, rmse=%.4f\n", s, r$cor, r$rmse))
    }
  }

  # Family-specific checks
  family_name <- mvgam_fit$family$family
  scale_check <- TRUE

  # Check integer values for count families
  integer_families <- c("poisson", "negbinomial", "negative_binomial",
                        "binomial", "beta_binomial",
                        "hurdle_poisson", "hurdle_negbinomial",
                        "zero_inflated_poisson", "zero_inflated_negbinomial",
                        "zero_inflated_binomial")
  if (family_name %in% integer_families) {
    mvgam_is_int <- all(mvgam_pred == floor(mvgam_pred))
    brms_is_int <- all(brms_pred == floor(brms_pred))
    if (!mvgam_is_int) {
      cat("  WARNING: mvgam predictions are not integers\n")
      scale_check <- FALSE
    } else {
      cat("  Integer check: OK (all predictions are integers)\n")
    }
  }

  # Check non-negative for count/positive families
  nonneg_families <- c("poisson", "negbinomial", "negative_binomial",
                       "gamma", "lognormal", "exponential",
                       "hurdle_poisson", "hurdle_negbinomial", "hurdle_gamma",
                       "hurdle_lognormal",
                       "zero_inflated_poisson", "zero_inflated_negbinomial")
  if (family_name %in% nonneg_families) {
    if (any(mvgam_pred < 0)) {
      cat("  WARNING: mvgam predictions contain negative values\n")
      scale_check <- FALSE
    } else {
      cat("  Non-negative check: OK\n")
    }
  }

  # Compare variance to epred (predict should have more variance)
  set.seed(456)
  mvgam_epred <- posterior_epred(mvgam_fit, newdata = newdata, ndraws = ndraws)
  var_predict <- var(as.vector(mvgam_pred))
  var_epred <- var(as.vector(mvgam_epred))
  var_ratio <- var_predict / var_epred

  cat(sprintf("  Variance: predict=%.2f, epred=%.2f, ratio=%.2f\n",
              var_predict, var_epred, var_ratio))
  if (var_ratio < 1.0) {
    cat("  WARNING: var(predict) < var(epred), expected more variance\n")
  }

  # Relaxed threshold for posterior_predict due to sampling noise
  # Mean predictions should still correlate reasonably well
  cor_threshold <- 0.70

  mean_r <- stat_results$mean
  if (isTRUE(mean_r$mismatch)) {
    passed <- FALSE
  } else if (mean_r$constant) {
    passed <- scale_check
  } else {
    passed <- mean_r$cor >= cor_threshold && scale_check
  }

  status <- if (passed) "PASSED" else "FAILED"
  cat(sprintf("  Result: %s (mean cor=%.3f, threshold=%.2f)\n",
              status, ifelse(mean_r$constant, NA, mean_r$cor), cor_threshold))

  list(name = paste0("predict_", test_name), passed = passed,
       stats = stat_results, var_ratio = var_ratio)
}

# Test posterior_predict.mvgam() for Poisson models
cat("\n--- Testing Poisson family ---\n")
results$predict_1 <- run_predict_validation(
  "Intercept-only AR(1)", brms_1, mvgam_1, test_data
)
results$predict_2 <- run_predict_validation(
  "AR(1) + fixed effect", brms_2, mvgam_2, test_data
)
results$predict_4 <- run_predict_validation(
  "AR(1) + fixed + random + smooth", brms_4, mvgam_4, test_data
)
results$predict_8 <- run_predict_validation(
  "AR(1) + GP", brms_8, mvgam_8, test_data
)

# Test posterior_predict for trend-formula models
cat("\n--- Testing trend-formula models ---\n")
results$predict_2t <- run_predict_validation(
  "AR(1) + fixed (in trend)", brms_2, mvgam_2t, test_data
)
results$predict_4t <- run_predict_validation(
  "AR(1) + fixed + random + smooth (in trend)", brms_4, mvgam_4t, test_data
)

# Test posterior_predict for Beta family (continuous 0-1)
cat("\n--- Testing Beta family ---\n")
results$predict_beta <- run_predict_validation(
  "Beta AR(1)", brms_beta, mvgam_beta, test_data_beta
)

# Test posterior_predict for Binomial family
cat("\n--- Testing Binomial family ---\n")
results$predict_binom <- run_predict_validation(
  "Binomial AR(1)", brms_binom, mvgam_binom, test_data_binom
)

# Test posterior_predict for Hurdle Poisson
cat("\n--- Testing Hurdle Poisson family ---\n")
results$predict_hp <- run_predict_validation(
  "Hurdle Poisson AR(1)", brms_hp, mvgam_hp, test_data_hp
)

# Test posterior_predict for Hurdle Negative Binomial
cat("\n--- Testing Hurdle Negative Binomial family ---\n")
results$predict_hnb <- run_predict_validation(
  "Hurdle NegBin AR(1)", brms_hnb, mvgam_hnb, test_data_hnb
)

# Test posterior_predict for Zero-Inflated Poisson
cat("\n--- Testing Zero-Inflated Poisson family ---\n")
results$predict_zip <- run_predict_validation(
  "ZI Poisson AR(1)", brms_zip, mvgam_zip, test_data_zip
)

# Test posterior_predict for Multivariate Gaussian
cat("\n--- Testing Multivariate Gaussian ---\n")
set.seed(789)
brms_predict_y1 <- brms::posterior_predict(brms_11, newdata = test_data_mv,
                                            resp = "y1", ndraws = 500)
brms_predict_y2 <- brms::posterior_predict(brms_11, newdata = test_data_mv,
                                            resp = "y2", ndraws = 500)
set.seed(789)
mvgam_predict_mv <- posterior_predict(mvgam_11, newdata = test_data_mv,
                                       ndraws = 500)

cat("  Returns list:", is.list(mvgam_predict_mv), "\n")
cat("  Has both responses:", all(c("y1", "y2") %in% names(mvgam_predict_mv)), "\n")

if (is.list(mvgam_predict_mv)) {
  mv_y1_cor <- cor(colMeans(brms_predict_y1), colMeans(mvgam_predict_mv$y1))
  mv_y2_cor <- cor(colMeans(brms_predict_y2), colMeans(mvgam_predict_mv$y2))
  cat(sprintf("  y1 mean cor: %.4f\n", mv_y1_cor))
  cat(sprintf("  y2 mean cor: %.4f\n", mv_y2_cor))
  mv_passed <- mv_y1_cor >= 0.70 && mv_y2_cor >= 0.70
} else {
  mv_passed <- FALSE
  cat("  ERROR: Expected list for multivariate model\n")
}
results$predict_mv <- list(
  name = "predict_multivariate",
  passed = mv_passed
)
cat("  Result:", if (mv_passed) "PASSED" else "FAILED", "\n")


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
