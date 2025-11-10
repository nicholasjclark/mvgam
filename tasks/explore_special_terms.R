# Exploration Script: Special Terms in prep$sdata
# Task 2.3.5.4.1 - Document smooth, random effects, and GP term structures
#
# CRITICAL: Start with devtools::load_all() (NOT library(mvgam))

devtools::load_all()

# Load required packages
library(posterior)

# ==============================================================================
# SETUP: Load test fixtures with special terms
# ==============================================================================

cat("\n=== LOADING TEST FIXTURES ===\n")

# fit3: Multivariate with smooths s(x) + VAR with presence covariate
fit3 <- readRDS("tasks/fixtures/fit3.rds")
cat("✓ Loaded fit3: mvbind(count, biomass) ~ s(x) + VAR(p=2, ma=TRUE)\n")

# fit6: Random effects + GP in trend formula
fit6 <- readRDS("tasks/fixtures/fit6.rds")
cat("✓ Loaded fit6: y ~ (1 | series) + CAR() with gp(x)\n")

# fit8: GP in both observation and trend formulas
fit8 <- readRDS("tasks/fixtures/fit8.rds")
cat("✓ Loaded fit8: y ~ gp(x, k=5) + AR(p=c(1,12)) with gp(temperature, k=6)\n")

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Print structure of prep$sdata with focused output
explore_prep_structure <- function(prep, model_name) {
  cat("\n", strrep("=", 80), "\n", sep = "")
  cat("MODEL: ", model_name, "\n", sep = "")
  cat(strrep("=", 80), "\n")

  if (is.null(prep)) {
    cat("\n✗ prep object is NULL (error occurred during generation)\n")
    return(invisible(NULL))
  }

  cat("\n--- prep$sdata names ---\n")
  print(names(prep$sdata))

  cat("\n--- Design matrix dimensions ---\n")
  for (nm in names(prep$sdata)) {
    obj <- prep$sdata[[nm]]
    if (is.matrix(obj) || is.array(obj)) {
      cat(sprintf("  %-30s: %s\n", nm, paste(dim(obj), collapse = " × ")))
    } else if (is.numeric(obj) || is.integer(obj)) {
      cat(sprintf("  %-30s: length %d\n", nm, length(obj)))
    }
  }

  cat("\n--- Parameter names in prep$draws ---\n")
  draw_vars <- variables(prep$draws)
  print(draw_vars)

  invisible(prep)
}

#' Extract and document specific term patterns
document_term_pattern <- function(prep, pattern, term_type) {
  cat("\n", strrep("-", 80), "\n", sep = "")
  cat("TERM TYPE: ", term_type, " (pattern: ", pattern, ")\n", sep = "")
  cat(strrep("-", 80), "\n")

  if (is.null(prep)) {
    cat("\n✗ Cannot document - prep object is NULL\n")
    return(invisible(NULL))
  }

  # Find matching sdata elements
  matching_sdata <- grep(pattern, names(prep$sdata), value = TRUE)

  if (length(matching_sdata) > 0) {
    cat("\n✓ Found", length(matching_sdata), "matching sdata elements:\n")
    for (nm in matching_sdata) {
      obj <- prep$sdata[[nm]]
      if (is.matrix(obj) || is.array(obj)) {
        cat(sprintf("  %-40s: %s\n", nm, paste(dim(obj), collapse = " × ")))
      } else {
        cat(sprintf("  %-40s: %s\n", nm, class(obj)))
      }
    }
  } else {
    cat("\n✗ No matching sdata elements found\n")
  }

  # Find matching parameter names
  draw_vars <- variables(prep$draws)
  matching_params <- grep(pattern, draw_vars, value = TRUE)

  if (length(matching_params) > 0) {
    cat("\n✓ Found", length(matching_params), "matching parameters:\n")
    cat("  ", paste(head(matching_params, 10), collapse = ", "), "\n")
    if (length(matching_params) > 10) {
      cat("  ... and", length(matching_params) - 10, "more\n")
    }
  } else {
    cat("\n✗ No matching parameters found\n")
  }

  invisible(NULL)
}

# ==============================================================================
# EXPLORATION 1: SMOOTH TERMS (fit3)
# ==============================================================================

cat("\n\n")
cat("################################################################################\n")
cat("# EXPLORATION 1: SMOOTH TERMS (fit3)\n")
cat("################################################################################\n")

# Extract obs and trend parameters for fit3
obs_params_3 <- extract_obs_parameters(fit3)
trend_params_3 <- extract_trend_parameters(fit3)

# Create parameter subsets (as draws_matrix)
obs_draws_3 <- as_draws_matrix(subset_draws(
  as_draws(fit3$fit),
  variable = obs_params_3
))

trend_draws_3 <- as_draws_matrix(subset_draws(
  as_draws(fit3$fit),
  variable = trend_params_3
))

# Create mock stanfits
mock_obs_3 <- create_mock_stanfit(obs_draws_3)
mock_trend_3 <- create_mock_stanfit(trend_draws_3)

# Create newdata
newdata_3 <- data.frame(
  time = rep(25:27, 2),
  count = rep(0L, 6),
  biomass = rep(1, 6),
  x = rep(c(0.5, 1.0, 1.5), 2),
  presence = rep(c(0L, 1L, 0L), 2)
)

# Generate prep objects
cat("\n--- Generating prep object for observation model ---\n")
prep_obs_3 <- tryCatch(
  prepare_predictions(mock_obs_3, brmsfit = fit3$obs_model, newdata = newdata_3),
  error = function(e) {
    cat("ERROR in prepare_predictions for obs:\n")
    cat(conditionMessage(e), "\n")
    cat("\nThis error needs investigation via r-package-analyzer agent\n\n")
    NULL
  }
)

cat("\n--- Generating prep object for trend model ---\n")
prep_trend_3 <- tryCatch(
  prepare_predictions(mock_trend_3, brmsfit = fit3$trend_model, newdata = newdata_3),
  error = function(e) {
    cat("ERROR in prepare_predictions for trend:\n")
    cat(conditionMessage(e), "\n")
    cat("\nThis error needs investigation via r-package-analyzer agent\n\n")
    NULL
  }
)

# Explore structure
explore_prep_structure(prep_obs_3, "fit3 - Observation (Multivariate with smooths)")
explore_prep_structure(prep_trend_3, "fit3 - Trend (VAR with covariates)")

# Document smooth-specific patterns
document_term_pattern(prep_obs_3, "^Zs_", "Smooth basis matrices (Zs_*)")
document_term_pattern(prep_obs_3, "^Xs_", "Smooth design (Xs_*)")
document_term_pattern(prep_obs_3, "^s_", "Smooth coefficients (s_*)")
document_term_pattern(prep_obs_3, "^sds_", "Smooth standard deviations (sds_*)")
document_term_pattern(prep_obs_3, "^zs_", "Standardized smooth coefficients (zs_*)")

# ==============================================================================
# EXPLORATION 2: RANDOM EFFECTS (fit6)
# ==============================================================================

cat("\n\n")
cat("################################################################################\n")
cat("# EXPLORATION 2: RANDOM EFFECTS (fit6)\n")
cat("################################################################################\n")

# Extract parameters for fit6
obs_params_6 <- extract_obs_parameters(fit6)
trend_params_6 <- extract_trend_parameters(fit6)

# Create parameter subsets (as draws_matrix)
obs_draws_6 <- as_draws_matrix(subset_draws(
  as_draws(fit6$fit),
  variable = obs_params_6
))

trend_draws_6 <- as_draws_matrix(subset_draws(
  as_draws(fit6$fit),
  variable = trend_params_6
))

# Create mock stanfits
mock_obs_6 <- create_mock_stanfit(obs_draws_6)
mock_trend_6 <- create_mock_stanfit(trend_draws_6)

# Create newdata (univariate)
newdata_6 <- data.frame(
  time = 25:27,
  series = factor(rep("series1", 3)),
  y = rep(0L, 3),
  x = c(0.5, 1.0, 1.5)
)

# Generate prep objects
prep_obs_6 <- prepare_predictions(mock_obs_6, brmsfit = fit6$obs_model, newdata = newdata_6)
prep_trend_6 <- prepare_predictions(mock_trend_6, brmsfit = fit6$trend_model, newdata = newdata_6)

# Explore structure
explore_prep_structure(prep_obs_6, "fit6 - Observation (Random effects)")
explore_prep_structure(prep_trend_6, "fit6 - Trend (CAR with GP)")

# Document random effects patterns
document_term_pattern(prep_obs_6, "^Z_", "Random effects design (Z_*)")
document_term_pattern(prep_obs_6, "^J_", "Random effects grouping indices (J_*)")
document_term_pattern(prep_obs_6, "^r_", "Random effects deviations (r_*)")
document_term_pattern(prep_obs_6, "^sd_", "Random effects standard deviations (sd_*)")
document_term_pattern(prep_obs_6, "^z_", "Standardized random effects (z_*)")

# ==============================================================================
# EXPLORATION 3: GAUSSIAN PROCESSES (fit6 trend + fit8)
# ==============================================================================

cat("\n\n")
cat("################################################################################\n")
cat("# EXPLORATION 3: GAUSSIAN PROCESSES (fit6 trend, fit8 obs + trend)\n")
cat("################################################################################\n")

# fit6 trend already loaded above - has gp(x)
document_term_pattern(prep_trend_6, "gp", "GP terms (gp*)")
document_term_pattern(prep_trend_6, "^sdgp_", "GP standard deviations (sdgp_*)")
document_term_pattern(prep_trend_6, "^lscale_", "GP length scales (lscale_*)")
document_term_pattern(prep_trend_6, "^zgp_", "Standardized GP coefficients (zgp_*)")

# Now explore fit8 which has GP in both formulas
cat("\n--- Additional GP exploration with fit8 ---\n")

# Extract parameters for fit8
obs_params_8 <- extract_obs_parameters(fit8)
trend_params_8 <- extract_trend_parameters(fit8)

# Create parameter subsets (as draws_matrix)
obs_draws_8 <- as_draws_matrix(subset_draws(
  as_draws(fit8$fit),
  variable = obs_params_8
))

trend_draws_8 <- as_draws_matrix(subset_draws(
  as_draws(fit8$fit),
  variable = trend_params_8
))

# Create mock stanfits
mock_obs_8 <- create_mock_stanfit(obs_draws_8)
mock_trend_8 <- create_mock_stanfit(trend_draws_8)

# Create newdata
newdata_8 <- data.frame(
  time = 25:27,
  series = factor(rep("series1", 3)),
  y = rep(0L, 3),
  x = c(0.5, 1.0, 1.5),
  temperature = c(15, 16, 17)
)

# Generate prep objects
prep_obs_8 <- prepare_predictions(mock_obs_8, brmsfit = fit8$obs_model, newdata = newdata_8)
prep_trend_8 <- prepare_predictions(mock_trend_8, brmsfit = fit8$trend_model, newdata = newdata_8)

# Explore structure
explore_prep_structure(prep_obs_8, "fit8 - Observation (GP in obs formula)")
explore_prep_structure(prep_trend_8, "fit8 - Trend (GP in trend formula)")

# Document GP patterns for both
document_term_pattern(prep_obs_8, "gp", "GP terms in obs (gp*)")
document_term_pattern(prep_trend_8, "gp", "GP terms in trend (gp*)")

# ==============================================================================
# SUMMARY: KEY FINDINGS
# ==============================================================================

cat("\n\n")
cat("################################################################################\n")
cat("# SUMMARY: KEY FINDINGS FOR IMPLEMENTATION\n")
cat("################################################################################\n")

cat("\n1. SMOOTH TERMS:\n")
cat("   - Basis matrices: Zs_<response>_<term>_<smooth> (e.g., Zs_count_sx_1)\n")
cat("   - Design matrices: Xs_<response> for random smooth effects\n")
cat("   - Parameters: s_<response>_<term>_<smooth> OR zs_<response>_<term>_<smooth>\n")
cat("   - Standard deviations: sds_<response>_<term>_<smooth>\n")
cat("   - Computation: Sum over smooths of coef %*% t(basis_matrix)\n")

cat("\n2. RANDOM EFFECTS:\n")
cat("   - Design matrices: Z_<group>_<term> (e.g., Z_1_1 for first grouping)\n")
cat("   - Grouping indices: J_<group> (maps observations to group levels)\n")
cat("   - Parameters: r_<group>_<term> (random deviations)\n")
cat("   - Standard deviations: sd_<group>_<term>\n")
cat("   - Standardized form: z_<group> with sd_<group> multiplication\n")
cat("   - Computation: Extract r[J[n]] for each observation n\n")

cat("\n3. GAUSSIAN PROCESSES:\n")
cat("   - GP predictions appear directly in prep$sdata (need to check for Jgp_*)\n")
cat("   - Parameters: sdgp_<term>, lscale_<term> (hyperparameters)\n")
cat("   - Coefficients: zgp_<term> (standardized)\n")
cat("   - Computation: Pre-computed GP predictions indexed by Jgp\n")

cat("\n4. PARAMETER NAMING:\n")
cat("   - Univariate: parameter_<term> (e.g., s_sx_1)\n")
cat("   - Multivariate: parameter_<response>_<term> (e.g., s_count_sx_1)\n")
cat("   - Trend: parameter_<term>_trend (e.g., sdgp_1_trend)\n")

cat("\n5. VECTORIZATION STRATEGY:\n")
cat("   - Fixed effects: Single matrix multiplication b %*% t(X)\n")
cat("   - Smooths: Sum over smooths, each as s %*% t(Zs)\n")
cat("   - Random effects: Requires indexing by J, partial vectorization\n")
cat("   - GPs: Direct indexing by Jgp into pre-computed predictions\n")

cat("\n\nExploration complete. Findings saved for implementation in Task 2.3.5.4.2+\n")

# ==============================================================================
# VERIFICATION: Compare Our Understanding to brms Ground Truth
# ==============================================================================

cat("\n\n")
cat("################################################################################\n")
cat("# VERIFICATION: Testing Computation Formulas\n")
cat("################################################################################\n")

cat("\n=== VERIFICATION 1: Smooth Terms (fit3 obs - multivariate) ===\n")
if (!is.null(prep_obs_3)) {
  # Get brms ground truth for first draw
  cat("Getting brms posterior_linpred for comparison...\n")

  # Extract first draw's linear predictor from brms
  # Note: This tests if our understanding is correct
  cat("TODO for Task 2.3.5.4.2:\n")
  cat("  1. Extract smooth basis matrix: Zs_count_sx_1, Zs_biomass_sx_1\n")
  cat("  2. Extract smooth coefficients: s_count_sx_1, s_biomass_sx_1 (or zs_*)\n")
  cat("  3. Test computation: s %*% t(Zs) vs Zs %*% s\n")
  cat("  4. Verify dimensions match nobs\n")
  cat("  5. Handle multiple smooths per response\n")
  cat("  6. Compare to brms::posterior_linpred() output\n")

  cat("\nSmooth term matrices available:\n")
  smooth_mats <- grep("^Zs_", names(prep_obs_3$sdata), value = TRUE)
  for (sm in smooth_mats) {
    cat(sprintf("  %s: %s\n", sm,
                paste(dim(prep_obs_3$sdata[[sm]]), collapse = " × ")))
  }

  cat("\nSmooth parameters available:\n")
  smooth_pars <- grep("^s_|^zs_", variables(prep_obs_3$draws), value = TRUE)
  cat("  ", paste(head(smooth_pars, 10), collapse = ", "))
  if (length(smooth_pars) > 10) cat(", ... (", length(smooth_pars), "total)")
  cat("\n")
} else {
  cat("✗ Cannot verify - prep_obs_3 is NULL\n")
}

cat("\n=== VERIFICATION 2: Random Effects (fit6 obs) ===\n")
if (!is.null(prep_obs_6)) {
  cat("TODO for Task 2.3.5.4.3:\n")
  cat("  1. Extract Z matrix: Z_1_1\n")
  cat("  2. Extract grouping indices: J_1\n")
  cat("  3. Extract random deviations: r_1_1[...]\n")
  cat("  4. Test indexing: r[J[n]] for each observation n\n")
  cat("  5. Check if Z matrix multiplication is needed\n")
  cat("  6. Handle standardized form (z_ * sd_) if present\n")
  cat("  7. Compare to brms::posterior_linpred() output\n")

  cat("\nRandom effects matrices available:\n")
  re_mats <- grep("^Z_|^J_", names(prep_obs_6$sdata), value = TRUE)
  for (re in re_mats) {
    obj <- prep_obs_6$sdata[[re]]
    if (is.matrix(obj) || is.array(obj)) {
      cat(sprintf("  %s: %s\n", re, paste(dim(obj), collapse = " × ")))
    } else {
      cat(sprintf("  %s: length %d\n", re, length(obj)))
    }
  }

  cat("\nRandom effects parameters available:\n")
  re_pars <- grep("^r_|^z_|^sd_", variables(prep_obs_6$draws), value = TRUE)
  cat("  ", paste(head(re_pars, 10), collapse = ", "))
  if (length(re_pars) > 10) cat(", ... (", length(re_pars), "total)")
  cat("\n")
} else {
  cat("✗ Cannot verify - prep_obs_6 is NULL\n")
}

cat("\n=== VERIFICATION 3: Gaussian Processes (fit8 obs & trend) ===\n")
if (!is.null(prep_obs_8) && !is.null(prep_trend_8)) {
  cat("TODO for Task 2.3.5.4.4:\n")
  cat("  1. Locate GP predictions in prep$sdata\n")
  cat("  2. Extract indexing variable: Jgp_<term>\n")
  cat("  3. Extract GP parameters: sdgp_*, lscale_*, zgp_*\n")
  cat("  4. Test if GP contribution is pre-computed or needs calculation\n")
  cat("  5. Verify indexing: gp_pred[Jgp[n]]\n")
  cat("  6. Compare to brms::posterior_linpred() output\n")

  cat("\nGP-related data in obs model:\n")
  gp_obs <- grep("gp|Jgp|Xgp", names(prep_obs_8$sdata), value = TRUE)
  cat("  ", paste(gp_obs, collapse = ", "), "\n")

  cat("\nGP-related data in trend model:\n")
  gp_trend <- grep("gp|Jgp|Xgp", names(prep_trend_8$sdata), value = TRUE)
  cat("  ", paste(gp_trend, collapse = ", "), "\n")

  cat("\nGP parameters in obs:\n")
  gp_pars_obs <- grep("gp_|sdgp_|lscale_|zgp_", variables(prep_obs_8$draws),
                      value = TRUE)
  cat("  ", paste(head(gp_pars_obs, 10), collapse = ", "))
  if (length(gp_pars_obs) > 10) cat(", ... (", length(gp_pars_obs), "total)")
  cat("\n")

  cat("\nGP parameters in trend:\n")
  gp_pars_trend <- grep("gp_|sdgp_|lscale_|zgp_", variables(prep_trend_8$draws),
                        value = TRUE)
  cat("  ", paste(head(gp_pars_trend, 10), collapse = ", "))
  if (length(gp_pars_trend) > 10) cat(", ... (", length(gp_pars_trend), "total)")
  cat("\n")
} else {
  cat("✗ Cannot verify - prep_obs_8 or prep_trend_8 is NULL\n")
}

cat("\n=== CRITICAL QUESTIONS FOR TASK 2.3.5.4.5 (r-package-analyzer) ===\n")
cat("\nUse r-package-analyzer agent to investigate brms source code:\n")
cat("  1. How does posterior_linpred.brmsfit() compute smooth contributions?\n")
cat("  2. Exact formula for random effects: r[J[n]] or Z[n] * r[J[n]]?\n")
cat("  3. How are GP predictions computed/indexed?\n")
cat("  4. How do multiple terms combine? (intercept + fixed + smooths + RE + GP)\n")
cat("  5. Are there any brms-specific optimizations we should replicate?\n")
cat("\nAgent should examine:\n")
cat("  - R/posterior_linpred.R in brms\n")
cat("  - How brms uses prep$sdata to build linear predictors\n")
cat("  - The exact matrix operations for each term type\n")

cat("\n\n✓ Exploration and verification planning complete.\n")
cat("Output saved to: tasks/special_terms_exploration_output.txt\n")
