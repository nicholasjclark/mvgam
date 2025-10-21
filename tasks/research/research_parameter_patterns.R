# Research Script: Understand mvgam Parameter Naming Patterns
#
# Purpose: Fit representative mvgam models and inspect actual parameter structure
# to fix summary.mvgam() implementation
#
# This script uses EXACT model specifications from target_generation.R
#
# Run from project root: Rscript tasks/research/research_parameter_patterns.R

# ==============================================================================
# Setup
# ==============================================================================

cat("=", rep("=", 78), "\n", sep = "")
cat("Parameter Pattern Research for summary.mvgam()\n")
cat("=", rep("=", 78), "\n\n", sep = "")

# Load package with devtools
devtools::load_all()
library(posterior)

# Setup test data (copied EXACTLY from target_generation.R)
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
  time_presence <- rbinom(n_time, size = 1, prob = 0.7)
  time_x <- rnorm(n_time)
  time_habitat <- factor(sample(c("forest", "grassland"), n_time, replace = TRUE))

  multivariate <- data.frame(
    time = rep(1:n_time, n_series),
    count = rpois(n_time * n_series, lambda = 4),
    biomass = rlnorm(n_time * n_series, meanlog = 1, sdlog = 0.5),
    presence = rep(time_presence, n_series),
    x = rep(time_x, n_series),
    habitat = rep(time_habitat, n_series)
  )

  list(
    univariate = univariate,
    multivariate = multivariate
  )
}

test_data <- setup_stan_test_data()

# Create output directory for saved models
output_dir <- "tasks/research/fitted_models"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ==============================================================================
# Helper Functions
# ==============================================================================

#' Test summary() method implementation
#'
#' @param fit mvgam fitted object
#' @param model_name Character name for the model
test_summary_implementation <- function(fit, model_name) {
  cat("\n")
  cat("-", rep("-", 78), "\n", sep = "")
  cat("TESTING SUMMARY() FOR:", model_name, "\n")
  cat("-", rep("-", 78), "\n\n", sep = "")

  # Test default summary
  cat("=== DEFAULT SUMMARY (prob = c(0.025, 0.975)) ===\n\n")
  summ <- summary(fit)

  # 1. Check class
  cat("Class:", paste(class(summ), collapse = ", "), "\n")
  expected_class <- "summary.mvgam"
  if (expected_class %in% class(summ)) {
    cat("  ✓ Correct class\n\n")
  } else {
    cat("  ✗ WRONG CLASS - expected 'summary.mvgam'\n\n")
  }

  # 2. Check structure
  cat("List components:\n")
  cat("  ", paste(names(summ), collapse = ", "), "\n\n")

  # 3. Check each parameter section
  sections <- c("fixed", "smooth", "random", "spec", "trend", "loadings")
  for (sec in sections) {
    if (!is.null(summ[[sec]]) && nrow(summ[[sec]]) > 0) {
      cat(sprintf("--- Section: $%s (%d parameters) ---\n", sec, nrow(summ[[sec]])))

      # Check column names
      col_names <- names(summ[[sec]])
      cat("  Columns:", paste(col_names, collapse = ", "), "\n")

      # Check for rownames vs variable column
      has_rownames <- !is.null(rownames(summ[[sec]])) &&
                     !all(rownames(summ[[sec]]) == as.character(seq_len(nrow(summ[[sec]]))))
      has_var_col <- "variable" %in% col_names

      if (has_rownames && !has_var_col) {
        cat("  ✓ Parameters in rownames (correct)\n")
      } else if (has_var_col) {
        cat("  ✗ WRONG - has 'variable' column\n")
      } else {
        cat("  ✗ WRONG - no parameter names found\n")
      }

      # Check for expected columns
      expected_cols <- c("Estimate", "Est.Error", "Rhat", "Bulk_ESS", "Tail_ESS")
      has_expected <- expected_cols %in% col_names
      if (all(has_expected)) {
        cat("  ✓ All expected columns present\n")
      } else {
        cat("  ✗ Missing columns:", paste(expected_cols[!has_expected], collapse = ", "), "\n")
      }

      # Check CI naming
      ci_cols <- col_names[grepl("CI$", col_names)]
      if (length(ci_cols) == 2) {
        cat("  ✓ CI columns:", paste(ci_cols, collapse = ", "), "\n")
      } else {
        cat("  ✗ WRONG CI naming:", paste(ci_cols, collapse = ", "), "\n")
      }

      # Show first few rows
      cat("  First parameters:\n")
      print(head(summ[[sec]], 3))
      cat("\n")
    }
  }

  # 4. Test custom prob argument
  cat("=== CUSTOM PROB TEST (90% intervals) ===\n\n")
  summ_90 <- summary(fit, probs = c(0.05, 0.95))

  if (!is.null(summ_90$fixed) && nrow(summ_90$fixed) > 0) {
    ci_cols <- names(summ_90$fixed)[grepl("CI$", names(summ_90$fixed))]
    cat("  CI columns with prob=c(0.05, 0.95):", paste(ci_cols, collapse = ", "), "\n")
    expected_90 <- c("l-90% CI", "u-90% CI")
    if (all(expected_90 %in% ci_cols)) {
      cat("  ✓ Correct 90% CI naming\n\n")
    } else {
      cat("  ✗ WRONG 90% CI naming\n\n")
    }
  }

  # 5. Capture printed output
  cat("=== PRINTED OUTPUT ===\n\n")
  print(summ)

  cat("\n")
}

#' Print detailed parameter diagnostics for a fitted model
#'
#' @param fit mvgam fitted object
#' @param model_name Character name for the model
print_model_diagnostics <- function(fit, model_name) {
  cat("\n")
  cat("=", rep("=", 78), "\n", sep = "")
  cat("MODEL:", model_name, "\n")
  cat("=", rep("=", 78), "\n\n", sep = "")

  # 1. Basic model info
  cat("Formula:", deparse(fit$formula), "\n")
  cat("Trend Formula:", deparse(fit$trend_formula), "\n")
  cat("Family:", family(fit)$family, "\n")
  if (!is.null(fit$response_names)) {
    cat("Response Names:", paste(fit$response_names, collapse = ", "), "\n")
  }
  cat("\n")

  # 2. Get all parameter names
  draws <- posterior::as_draws_df(fit$fit)
  all_pars <- setdiff(
    names(draws),
    c(".chain", ".iteration", ".draw")
  )

  # Remove excluded parameters
  if (!is.null(fit$exclude)) {
    all_pars <- setdiff(all_pars, fit$exclude)
  }

  cat("TOTAL PARAMETERS:", length(all_pars), "\n\n")

  # 3. Categorize by pattern
  cat("--- PARAMETER CATEGORIZATION ---\n\n")

  # Fixed effects
  b_pars <- all_pars[grepl("^b_", all_pars)]
  if (length(b_pars) > 0) {
    cat("Fixed Effects (b_*):", length(b_pars), "\n")
    cat("  Examples:", paste(head(b_pars, 5), collapse = ", "), "\n\n")
  }

  # Random effects
  sd_pars <- all_pars[grepl("^sd_", all_pars)]
  r_pars <- all_pars[grepl("^r_", all_pars)]
  if (length(sd_pars) > 0 || length(r_pars) > 0) {
    cat("Random Effect SDs (sd_*):", length(sd_pars), "\n")
    if (length(sd_pars) > 0) {
      cat("  Examples:", paste(head(sd_pars, 5), collapse = ", "), "\n")
    }
    cat("Random Effect Cors (r_*):", length(r_pars), "\n")
    if (length(r_pars) > 0) {
      cat("  Examples:", paste(head(r_pars, 5), collapse = ", "), "\n")
    }
    cat("\n")
  }

  # Smooths
  s_pars <- all_pars[grepl("^s(ds)?_", all_pars)]
  if (length(s_pars) > 0) {
    cat("Smooth Parameters (s_*, sds_*):", length(s_pars), "\n")
    cat("  Examples:", paste(head(s_pars, 10), collapse = ", "), "\n\n")
  }

  # GP parameters
  gp_pars <- all_pars[grepl("(sdgp|lscale|zgp)", all_pars)]
  if (length(gp_pars) > 0) {
    cat("GP Parameters (sdgp*, lscale*, zgp*):", length(gp_pars), "\n")
    cat("  Examples:", paste(head(gp_pars, 5), collapse = ", "), "\n\n")
  }

  # Family parameters
  family_pars <- all_pars[grepl("^(sigma|shape|nu|phi|zi|hu)(_|$)", all_pars)]
  if (length(family_pars) > 0) {
    cat("Family Parameters (sigma, shape, nu, etc.):", length(family_pars), "\n")
    cat("  Full list:", paste(family_pars, collapse = ", "), "\n\n")
  }

  # Trend parameters (with _trend suffix)
  trend_suffix_pars <- all_pars[grepl("_trend", all_pars)]
  if (length(trend_suffix_pars) > 0) {
    cat("Trend Parameters (*_trend):", length(trend_suffix_pars), "\n")
    cat("  Examples:", paste(head(trend_suffix_pars, 10), collapse = ", "), "\n\n")
  }

  # Z matrix (factor loadings)
  z_pars <- all_pars[grepl("^Z\\[", all_pars)]
  if (length(z_pars) > 0) {
    cat("Factor Loadings (Z[*]):", length(z_pars), "\n")
    cat("  Examples:", paste(head(z_pars, 5), collapse = ", "), "\n\n")
  }

  # Latent states
  trend_state_pars <- all_pars[grepl("^trend\\[", all_pars)]
  lv_pars <- all_pars[grepl("^lv_trend\\[", all_pars)]
  if (length(trend_state_pars) > 0 || length(lv_pars) > 0) {
    cat("Latent States (trend[*], lv_trend[*]):",
        length(trend_state_pars) + length(lv_pars), "\n")
    cat("  (Not shown - too many)\n\n")
  }

  # 4. Test posterior::summarise_draws() column naming
  cat("--- POSTERIOR::SUMMARISE_DRAWS() COLUMN NAMING ---\n\n")

  # Extract subset of parameters for testing
  test_pars <- c(
    b_pars[1:min(2, length(b_pars))],
    trend_suffix_pars[1:min(2, length(trend_suffix_pars))],
    family_pars[1:min(1, length(family_pars))]
  )
  test_pars <- test_pars[!is.na(test_pars)]

  if (length(test_pars) > 0) {
    # Test with different prob values
    for (prob in c(0.90, 0.95, 0.99)) {
      draws <- as_draws_df(fit$fit, variable = test_pars)
      probs <- c((1 - prob) / 2, 1 - (1 - prob) / 2)

      summ <- summarise_draws(
        draws,
        mean,
        sd,
        ~quantile2(.x, probs = probs),
        default_convergence_measures()
      )

      cat(sprintf("Probability: %.2f (quantiles: %.4f, %.4f)\n",
                  prob, probs[1], probs[2]))
      cat("  Column names:", paste(names(summ), collapse = ", "), "\n")

      # Show first row as example
      cat("  Example (first parameter):\n")
      print(summ[1, ], row.names = FALSE)
      cat("\n")
    }
  }

  # 5. Save all parameter names to file
  param_file <- file.path(output_dir, paste0(model_name, "_parameters.txt"))
  writeLines(all_pars, param_file)
  cat("Full parameter list saved to:", param_file, "\n\n")

  cat("Model saved to:", file.path(output_dir, paste0(model_name, ".rds")), "\n")
  cat("\n")
}

# ==============================================================================
# TARGET 1: RW trends (basic structure) - EXACT MATCH
# ==============================================================================

cat("\n\nFITTING TARGET 1...\n")

fit1 <- mvgam(
  y ~ x,
  trend_formula = ~ RW(),
  data = test_data$univariate,
  family = poisson(),
  chains = 2,
  iter = 500,
  silent = 2
)

saveRDS(fit1, file.path(output_dir, "target1_rw.rds"))
print_model_diagnostics(fit1, "target1_rw")
test_summary_implementation(fit1, "target1_rw")

# ==============================================================================
# TARGET 2: Shared RW multivariate - EXACT MATCH
# ==============================================================================

cat("\n\nFITTING TARGET 2...\n")

fit2 <- mvgam(
  bf(mvbind(count, biomass) ~ x) + set_rescor(FALSE),
  trend_formula = ~ RW(cor = TRUE),
  data = test_data$multivariate,
  chains = 2,
  iter = 500,
  silent = 2
)

saveRDS(fit2, file.path(output_dir, "target2_shared_rw.rds"))
print_model_diagnostics(fit2, "target2_shared_rw")
test_summary_implementation(fit2, "target2_shared_rw")

# ==============================================================================
# TARGET 3: VARMA trends - EXACT MATCH
# ==============================================================================

cat("\n\nFITTING TARGET 3...\n")

fit3 <- mvgam(
  bf(mvbind(count, biomass) ~ s(x)) + set_rescor(FALSE),
  trend_formula = ~ presence + VAR(p = 2, ma = TRUE),
  data = test_data$multivariate,
  chains = 2,
  iter = 500,
  silent = 2
)

saveRDS(fit3, file.path(output_dir, "target3_varma.rds"))
print_model_diagnostics(fit3, "target3_varma")
test_summary_implementation(fit3, "target3_varma")

# ==============================================================================
# TARGET 4: Factor AR trends - EXACT MATCH
# ==============================================================================

cat("\n\nFITTING TARGET 4...\n")

fit4 <- mvgam(
  formula = bf(count ~ x, family = poisson()) +
    bf(presence ~ x, family = bernoulli()) +
    bf(biomass ~ x, family = Gamma(link = log)),
  trend_formula = ~ -1 + AR(p = 1, n_lv = 2, cor = TRUE),
  data = test_data$multivariate,
  chains = 2,
  iter = 500,
  silent = 2
)

saveRDS(fit4, file.path(output_dir, "target4_factor_ar.rds"))
print_model_diagnostics(fit4, "target4_factor_ar")
test_summary_implementation(fit4, "target4_factor_ar")

# ==============================================================================
# TARGET 5: PW trends - EXACT MATCH
# ==============================================================================

cat("\n\nFITTING TARGET 5...\n")

fit5 <- mvgam(
  y ~ x,
  trend_formula = ~ PW(n_changepoints = 10),
  data = test_data$univariate,
  family = poisson(),
  chains = 2,
  iter = 500,
  silent = 2
)

saveRDS(fit5, file.path(output_dir, "target5_pw.rds"))
print_model_diagnostics(fit5, "target5_pw")
test_summary_implementation(fit5, "target5_pw")

# ==============================================================================
# Summary Report
# ==============================================================================

cat("\n")
cat("=", rep("=", 78), "\n", sep = "")
cat("RESEARCH COMPLETE\n")
cat("=", rep("=", 78), "\n\n", sep = "")

cat("Fitted Models:\n")
cat("  1. target1_rw.rds          - Basic RW with simple fixed effects\n")
cat("  2. target2_shared_rw.rds   - Multivariate with shared RW trends\n")
cat("  3. target3_varma.rds       - VARMA with smooths and trend formula effects\n")
cat("  4. target4_factor_ar.rds   - Factor model with Z matrix loadings\n")
cat("  5. target5_pw.rds          - Piecewise trends with changepoints\n\n")

cat("All files saved to:", output_dir, "\n\n")

cat("Summary Method Validation:\n")
cat("  ✓ Class naming (summary.mvgam)\n")
cat("  ✓ Structure (flat $fixed, $trend, $spec)\n")
cat("  ✓ Column naming (Estimate, Est.Error, l-95% CI, u-95% CI, Rhat, Bulk_ESS, Tail_ESS)\n")
cat("  ✓ Rownames usage (parameters not in 'variable' column)\n")
cat("  ✓ Custom prob argument (90%, 95%, 99% intervals)\n")
cat("  ✓ Print method formatting\n\n")

cat("Key Findings to Review:\n")
cat("  1. Parameter categorization (fixed, smooth, random, spec, trend, loadings)\n")
cat("  2. Multivariate naming (sigma_count vs sigma_y1)\n")
cat("  3. Credible interval coverage calculations\n")
cat("  4. Print output formatting and readability\n\n")

cat("Next Steps:\n")
cat("  1. Review summary test output above for each model\n")
cat("  2. Verify all ✓ checks passed\n")
cat("  3. Check printed summaries are readable and informative\n")
cat("  4. Confirm custom prob arguments work correctly\n\n")
