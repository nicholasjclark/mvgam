# Debug Intercept-Only Models: Minimal Case for Extraction Bug Isolation
# This script focuses on the simplest possible case: intercept-only models
# If we can't get this right, nothing else will work.

devtools::load_all()
library(brms)
library(posterior)
library(checkmate)

cat(rep("=", 70), "\n")
cat("DEBUGGING: mvgam AR vs brms intercept-only\n") 
cat(rep("=", 70), "\n")

# Create time series test data with covariate and grouping variable
set.seed(12345)
temperature <- rnorm(12, mean = 20, sd = 3)  # Temperature covariate
test_data_intercept <- data.frame(
  y = rpois(12, lambda = exp(1.2 + 0.1 * temperature/10 + 0.05 * sin(1:12/3))),
  temperature = temperature,
  time = 1:12,
  series = factor(rep("A", 12)),  # Single series for AR models
  site = factor(rep(c("Site1", "Site2", "Site3"), each = 4))  # 3 sites, 4 obs each for random effects
)

cat("Test data:\n")
print(test_data_intercept)

# PART 1: Fit and save intercept-only brms model
# ============================================================================

brms_intercept_file <- "tasks/fixtures/debug_brms_intercept.rds"
if (file.exists(brms_intercept_file)) {
  cat("\nLoading saved brms intercept-only model...\n")
  brms_intercept <- readRDS(brms_intercept_file)
} else {
  cat("\nFitting brms model with AR(1) + random effects: y ~ 1 + temperature + (1|site) + ar(p = 1)...\n")
  brms_intercept <- brm(
    y ~ 1 + temperature + (1|site) + ar(p = 1),
    data = test_data_intercept,
    family = poisson(),
    chains = 2,
    iter = 600,
    refresh = 0,
    silent = 2,
    backend = "cmdstanr"
  )
  
  if (!dir.exists("tasks/fixtures")) dir.create("tasks/fixtures", recursive = TRUE)
  saveRDS(brms_intercept, brms_intercept_file)
  cat("Saved brms model to", brms_intercept_file, "\n")
}

# PART 2: Fit and save equivalent mvgam model 
# ============================================================================

mvgam_intercept_file <- "tasks/fixtures/debug_mvgam_intercept.rds" 
if (file.exists(mvgam_intercept_file)) {
  cat("\nLoading saved mvgam intercept-only model...\n")
  mvgam_intercept <- readRDS(mvgam_intercept_file)
} else {
  cat("\nFitting equivalent mvgam model: y ~ 1 + temperature + (1|site), trend_formula = ~ AR(p = 1)...\n")
  cat("NOTE: Using AR trend to trigger GLM-to-standard conversion debugging\n")
  mvgam_intercept <- mvgam(
    formula = y ~ 1 + temperature + (1|site),  # Same fixed + random effects as brms
    trend_formula = ~ AR(p = 1),               # AR trend to trigger conversion
    data = test_data_intercept,
    family = poisson(),
    chains = 2,
    iter = 600,
    refresh = 0,
    silent = 2,
    backend = "cmdstanr"
  )
  
  saveRDS(mvgam_intercept, mvgam_intercept_file)
  cat("Saved mvgam model to", mvgam_intercept_file, "\n")
}

# PART 3: Create prediction data and get brms baseline
# ============================================================================

# Use training data for fitted values (in-sample, avoids AR prediction issues)
# Include all sites in prediction data
newdata_intercept <- test_data_intercept[c(1, 5, 9), ]  # One obs from each site (Site1, Site2, Site3)

cat("\nNewdata for predictions:\n")
print(newdata_intercept)

# Try brms baseline using posterior_linpred with incl_autocor = FALSE
cat("\nAttempting brms baseline using posterior_linpred with incl_autocor = FALSE...\n")
tryCatch({
  brms_linpred <- posterior_linpred(
    brms_intercept,
    newdata = newdata_intercept,
    ndraws = 50,
    summary = FALSE,
    incl_autocor = FALSE  # Exclude AR component, get only fixed effects
  )
  cat("✅ brms posterior_linpred succeeded with incl_autocor = FALSE!\n")
}, error = function(e) {
  cat("❌ brms posterior_linpred failed with error:\n")
  cat("Error message:", e$message, "\n")
  cat("\nTrying alternative: manual computation from brms parameters...\n")
  
  # Manual computation from brms parameters
  brms_draws_all <- as_draws_matrix(brms_intercept)
  brms_beta_names <- c("b_Intercept", "b_temperature")
  
  # Check what parameters are actually available
  available_params <- colnames(brms_draws_all)
  cat("Available brms parameters:", paste(head(available_params, 10), collapse = ", "), "...\n")
  
  # Try to find temperature parameter
  temp_param <- grep("temperature|temp", available_params, value = TRUE)[1]
  if (is.na(temp_param)) {
    cat("No temperature parameter found, checking for coefficient patterns...\n")
    coef_params <- grep("^b_", available_params, value = TRUE)
    cat("Available coefficients:", paste(coef_params, collapse = ", "), "\n")
    
    if (length(coef_params) >= 2) {
      temp_param <- coef_params[2]  # Second coefficient should be temperature
      cat("Using", temp_param, "as temperature parameter\n")
    }
  }
  
  if (!is.na(temp_param)) {
    beta_names_actual <- c("b_Intercept", temp_param)
    brms_beta_subset <- brms_draws_all[1:50, beta_names_actual, drop = FALSE]
    
    # Create design matrix manually (prep_intercept not available yet)
    X_design <- model.matrix(~ temperature, data = newdata_intercept)
    
    # Manual computation
    brms_linpred_manual <- brms_beta_subset %*% t(X_design)
    
    # Store as global variable for comparison
    brms_linpred <<- brms_linpred_manual
    cat("✅ Manual brms computation succeeded!\n")
    cat("Manual brms dimensions:", dim(brms_linpred), "\n")
  } else {
    cat("❌ Could not identify temperature parameter for manual computation\n")
    brms_linpred <<- NULL
  }
})

cat("brms baseline dimensions:", dim(brms_linpred), "\n")
cat("brms baseline summary:\n")
cat("  Mean:", mean(brms_linpred), "\n")
cat("  SD:", sd(brms_linpred), "\n")
cat("  Range:", range(brms_linpred), "\n")

# PART 4: Step-by-step debugging of our extraction method
# ============================================================================

cat("\n", rep("-", 50), "\n")
cat("STEP-BY-STEP DEBUGGING OUR METHOD\n")
cat(rep("=", 50), "\n")

# Step 4.1: Extract parameters
cat("\nStep 1: Extracting observation parameters...\n")
obs_params <- extract_obs_parameters(mvgam_intercept)
cat("Observation parameters found:", length(obs_params), "\n")
cat("Parameter names:", paste(obs_params, collapse = ", "), "\n")

# Step 4.2: Get parameter draws
cat("\nStep 2: Extracting parameter draws...\n")
param_draws <- as_draws_matrix(mvgam_intercept$fit, variable = obs_params)
param_subset <- posterior::subset_draws(param_draws, draw = 1:50)
cat("Parameter draws dimensions:", dim(param_subset), "\n")

# Step 4.3: Inspect parameter values 
cat("\nStep 3: Inspecting observation parameter values...\n")
obs_param_names <- c("b_Intercept", "b_temperature")
for (param_name in obs_param_names) {
  if (param_name %in% colnames(param_subset)) {
    param_vals <- param_subset[, param_name]
    cat(param_name, "summary:\n")
    cat("  Mean:", mean(param_vals), "\n")
    cat("  SD:", sd(param_vals), "\n")
    cat("  Range:", range(param_vals), "\n")
  } else {
    cat("Note:", param_name, "not found in parameter draws\n")
  }
}

# Step 4.4: Create mock object
cat("\nStep 4: Creating mock stanfit...\n")
mock_fit <- create_mock_stanfit(param_subset)
cat("Mock stanfit created successfully:", !is.null(mock_fit), "\n")

# Step 4.5: Create prep object
cat("\nStep 5: Creating prep object...\n")
prep_intercept <- prepare_predictions(
  mock_fit, 
  brmsfit = mvgam_intercept$obs_model, 
  newdata = newdata_intercept
)
cat("Prep object created successfully:", !is.null(prep_intercept), "\n")
cat("Prep object class:", class(prep_intercept), "\n")

# Step 4.6: Inspect prep object structure
cat("\nStep 6: Inspecting prep object structure...\n")
cat("Prep components:", names(prep_intercept), "\n")
if ("sdata" %in% names(prep_intercept)) {
  cat("sdata components:", names(prep_intercept$sdata), "\n")
  if ("X" %in% names(prep_intercept$sdata)) {
    X <- prep_intercept$sdata$X
    cat("Design matrix X dimensions:", dim(X), "\n")
    cat("Design matrix X:\n")
    print(X)
  }
}

# Step 4.7: Inspect draws in prep
cat("\nStep 7: Inspecting draws in prep object...\n")
if ("draws" %in% names(prep_intercept)) {
  draws_mat <- as_draws_matrix(prep_intercept$draws)
  cat("Draws matrix dimensions:", dim(draws_mat), "\n")
  cat("Available parameters:", colnames(draws_mat), "\n")
  
  if ("b_Intercept" %in% colnames(draws_mat)) {
    prep_intercept_vals <- draws_mat[, "b_Intercept"]
    cat("Prep b_Intercept summary:\n")
    cat("  Mean:", mean(prep_intercept_vals), "\n")
    cat("  SD:", sd(prep_intercept_vals), "\n")
    cat("  Range:", range(prep_intercept_vals), "\n")
    
    # Critical check: Are the parameter values the same?
    if (exists("intercept_vals")) {
      values_match <- all.equal(as.numeric(intercept_vals), as.numeric(prep_intercept_vals))
      cat("Parameter values match between input and prep:", isTRUE(values_match), "\n")
      if (!isTRUE(values_match)) {
        cat("Mismatch details:", values_match, "\n")
      }
    }
  }
}

# Step 4.8: Manual computation (GROUND TRUTH)
cat("\nStep 8: Manual computation of linear predictor...\n")
if ("b_Intercept" %in% colnames(draws_mat)) {
  n_draws <- nrow(draws_mat)
  n_obs <- nrow(newdata_intercept)
  
  # Get design matrix X from prep
  X <- prep_intercept$sdata$X
  cat("Design matrix X:\n")
  print(X)
  
  # Identify coefficient parameters dynamically
  # Check for both naming conventions
  if ("b_temperature" %in% colnames(draws_mat)) {
    beta_names <- c("b_Intercept", "b_temperature")
  } else if ("b[1]" %in% colnames(draws_mat)) {
    beta_names <- c("b_Intercept", "b[1]")  # mvgam uses b[1] for temperature
  } else {
    # Find all b_ parameters
    b_params <- grep("^b_|^b\\[", colnames(draws_mat), value = TRUE)
    cat("Available b parameters:", paste(b_params, collapse = ", "), "\n")
    beta_names <- c("b_Intercept", b_params[b_params != "b_Intercept"][1])
  }
  
  # Manual computation: mu = X %*% beta
  # Extract coefficients (intercept + temperature)
  beta_matrix <- draws_mat[, beta_names, drop = FALSE]
  
  cat("Beta coefficients dimensions:", dim(beta_matrix), "\n")
  cat("Beta coefficient names:", colnames(beta_matrix), "\n")
  
  # Manual linear predictor: beta %*% t(X) gives [draws x obs]
  manual_linpred <- beta_matrix %*% t(X)
  
  cat("Manual computation dimensions:", dim(manual_linpred), "\n")
  cat("Manual computation summary:\n")
  cat("  Mean:", mean(manual_linpred), "\n")
  cat("  SD:", sd(manual_linpred), "\n")
  cat("  Range:", range(manual_linpred), "\n")
} else {
  manual_linpred <- NULL
  cat("Cannot perform manual computation - b_Intercept missing\n")
}

# Step 4.9: Our extraction method
cat("\nStep 9: Testing our extract_linpred_from_prep()...\n")
tryCatch({
  mvgam_linpred <- extract_linpred_from_prep(prep_intercept)
  cat("Our method succeeded!\n")
  cat("Our method dimensions:", dim(mvgam_linpred), "\n")
  cat("Our method summary:\n")
  cat("  Mean:", mean(mvgam_linpred), "\n")
  cat("  SD:", sd(mvgam_linpred), "\n")
  cat("  Range:", range(mvgam_linpred), "\n")
}, error = function(e) {
  cat("ERROR in extract_linpred_from_prep():", e$message, "\n")
  mvgam_linpred <<- NULL
})

# PART 5: Critical comparisons
# ============================================================================

cat("\n", rep("-", 50), "\n")
cat("CRITICAL COMPARISONS\n")
cat(rep("=", 50), "\n")

if (!is.null(manual_linpred) && !is.null(mvgam_linpred)) {
  
  cat("\nComparison 1: Our method vs Manual computation (should be IDENTICAL)\n")
  manual_vs_ours <- all.equal(manual_linpred, mvgam_linpred)
  cat("Results match:", isTRUE(manual_vs_ours), "\n")
  if (!isTRUE(manual_vs_ours)) {
    cat("Mismatch details:", manual_vs_ours, "\n")
    cat("Max difference:", max(abs(manual_linpred - mvgam_linpred)), "\n")
  }
  
  cat("\nComparison 2: Proper Comparison - Mean Estimates per Data Point\n")
  
  # Get mean estimates per data point for proper correlation
  mvgam_means <- colMeans(mvgam_linpred)  # Mean across draws for each observation
  brms_means <- colMeans(brms_linpred)    # Mean across draws for each observation
  
  cat("Mean estimates per data point:\n")
  cat("Data point 1 - mvgam:", mvgam_means[1], "vs brms:", brms_means[1], "\n")
  cat("Data point 2 - mvgam:", mvgam_means[2], "vs brms:", brms_means[2], "\n") 
  cat("Data point 3 - mvgam:", mvgam_means[3], "vs brms:", brms_means[3], "\n")
  
  # Correlation across data points (this is the meaningful comparison)
  point_correlation <- cor(mvgam_means, brms_means)
  cat("\nCorrelation across data points:", point_correlation, "\n")
  
  cat("\nComparison 3: Coefficient Comparison (including AR terms)\n")
  
  # Extract all relevant parameters from both models
  brms_draws_all <- as_draws_matrix(brms_intercept)
  mvgam_draws_all <- as_draws_matrix(mvgam_intercept$fit)
  
  # Compare fixed effects coefficients
  cat("\nFixed Effects Coefficients:\n")
  cat("b_Intercept:\n")
  cat("  brms - Mean:", mean(brms_draws_all[1:50, "b_Intercept"]), 
      "SD:", sd(brms_draws_all[1:50, "b_Intercept"]), "\n")
  cat("  mvgam - Mean:", mean(mvgam_draws_all[1:50, "b_Intercept"]), 
      "SD:", sd(mvgam_draws_all[1:50, "b_Intercept"]), "\n")
  
  # Temperature coefficient (check naming)
  brms_temp_param <- "b_temperature"
  mvgam_temp_param <- if ("b_temperature" %in% colnames(mvgam_draws_all)) "b_temperature" else "b[1]"
  
  if (brms_temp_param %in% colnames(brms_draws_all) && mvgam_temp_param %in% colnames(mvgam_draws_all)) {
    cat("Temperature coefficient:\n")
    cat("  brms - Mean:", mean(brms_draws_all[1:50, brms_temp_param]), 
        "SD:", sd(brms_draws_all[1:50, brms_temp_param]), "\n")
    cat("  mvgam - Mean:", mean(mvgam_draws_all[1:50, mvgam_temp_param]), 
        "SD:", sd(mvgam_draws_all[1:50, mvgam_temp_param]), "\n")
  }
  
  # Random effects standard deviation
  cat("\nRandom Effects (site):\n")
  if ("sd_site__Intercept" %in% colnames(brms_draws_all)) {
    cat("  brms sd_site__Intercept - Mean:", mean(brms_draws_all[1:50, "sd_site__Intercept"]), 
        "SD:", sd(brms_draws_all[1:50, "sd_site__Intercept"]), "\n")
  }
  if ("sigma_site" %in% colnames(mvgam_draws_all)) {
    cat("  mvgam sigma_site - Mean:", mean(mvgam_draws_all[1:50, "sigma_site"]), 
        "SD:", sd(mvgam_draws_all[1:50, "sigma_site"]), "\n")
  } else if ("sd_site__Intercept" %in% colnames(mvgam_draws_all)) {
    cat("  mvgam sd_site__Intercept - Mean:", mean(mvgam_draws_all[1:50, "sd_site__Intercept"]), 
        "SD:", sd(mvgam_draws_all[1:50, "sd_site__Intercept"]), "\n")
  }
  
  # Random intercepts by site
  site_params_brms <- grep("r_site\\[.*,Intercept\\]", colnames(brms_draws_all), value = TRUE)
  site_params_mvgam <- grep("r_site\\[.*\\]", colnames(mvgam_draws_all), value = TRUE)
  
  if (length(site_params_brms) > 0) {
    cat("  brms random intercepts:\n")
    for (param in site_params_brms[1:min(3, length(site_params_brms))]) {
      cat("    ", param, "- Mean:", mean(brms_draws_all[1:50, param]), 
          "SD:", sd(brms_draws_all[1:50, param]), "\n")
    }
  }
  
  if (length(site_params_mvgam) > 0) {
    cat("  mvgam random intercepts:\n")
    for (param in site_params_mvgam[1:min(3, length(site_params_mvgam))]) {
      cat("    ", param, "- Mean:", mean(mvgam_draws_all[1:50, param]), 
          "SD:", sd(mvgam_draws_all[1:50, param]), "\n")
    }
  }
  
  # Compare AR terms
  cat("\nAR Terms:\n")
  if ("ar[1]" %in% colnames(brms_draws_all)) {
    cat("  brms ar[1] - Mean:", mean(brms_draws_all[1:50, "ar[1]"]), 
        "SD:", sd(brms_draws_all[1:50, "ar[1]"]), "\n")
  }
  if ("ar1_trend[1]" %in% colnames(mvgam_draws_all)) {
    cat("  mvgam ar1_trend[1] - Mean:", mean(mvgam_draws_all[1:50, "ar1_trend[1]"]), 
        "SD:", sd(mvgam_draws_all[1:50, "ar1_trend[1]"]), "\n")
  }
  
  # Assessment
  if (point_correlation > 0.95) {
    cat("\n✅ Excellent correlation across data points (> 0.95)\n")
  } else if (point_correlation > 0.8) {
    cat("\n✅ Good correlation across data points (> 0.8)\n") 
  } else if (point_correlation > 0.5) {
    cat("\n⚠️ Moderate correlation across data points (> 0.5)\n")
  } else {
    cat("\n❌ Poor correlation across data points (< 0.5)\n")
  }
  
  cat("\nComparison 3: Manual vs brms baseline (should be IDENTICAL)\n")
  manual_vs_brms <- all.equal(manual_linpred, brms_linpred)
  cat("Results match:", isTRUE(manual_vs_brms), "\n")
  if (!isTRUE(manual_vs_brms)) {
    cat("Mismatch details:", manual_vs_brms, "\n")
    cat("Max difference:", max(abs(manual_linpred - brms_linpred)), "\n")
  }
  
} else {
  cat("Cannot perform comparisons - computation failed\n")
}

# PART 6: Summary and diagnosis
# ============================================================================

cat("\n", rep("=", 70), "\n")
cat("DIAGNOSIS SUMMARY\n")
cat(rep("=", 70), "\n")

cat("Test case: Comparable AR(1) models with covariate and random effects\n")
cat("- brms: y ~ 1 + temperature + (1|site) + ar(p = 1) (fixed + random effects + AR autocorrelation)\n")
cat("- mvgam: y ~ 1 + temperature + (1|site), trend_formula = ~ AR(p = 1) (fixed + random effects + AR trend)\n") 
cat("Data size: 12 time points\n") 
cat("Prediction size: 3 future time points\n")
cat("Number of draws: 50\n\n")

if (!is.null(mvgam_linpred)) {
  cat("✅ Our extraction method completed without errors\n")
  
  if (!is.null(manual_linpred)) {
    if (isTRUE(all.equal(manual_linpred, mvgam_linpred))) {
      cat("✅ Our method matches manual computation exactly\n")
    } else {
      cat("❌ Our method does NOT match manual computation\n")
    }
  }
  
  if (exists("brms_linpred")) {
    # Check correlation across data points (proper comparison)
    if (exists("point_correlation")) {
      cat("Data point correlation analysis:\n")
      cat("   Correlation across observations:", point_correlation, "\n")
      
      if (point_correlation > 0.95) {
        cat("✅ Excellent data point correlation (> 0.95)\n")
        cat("🎉 OBSERVATION EXTRACTION IS WORKING EXCELLENTLY!\n")
        cat("📝 Ready for trend component integration\n")
      } else if (point_correlation > 0.8) {
        cat("✅ Good data point correlation (> 0.8)\n")
        cat("🎉 OBSERVATION EXTRACTION IS WORKING WELL!\n") 
        cat("📝 Ready for trend component integration\n")
      } else if (point_correlation > 0.5) {
        cat("⚠️ Moderate data point correlation (> 0.5)\n")
        cat("🔍 Models have different structure but extraction appears functional\n")
      } else {
        cat("❌ Poor data point correlation (< 0.5)\n")
        cat("🔍 May indicate extraction issues or very different model structures\n")
      }
    } else {
      cat("⚠️ Cannot assess data point correlation\n")
    }
  }
  
} else {
  cat("❌ Our extraction method failed with errors\n")
  cat("   Next step: Fix the error before comparing results\n")
}

cat("\nDEBUGGING COMPLETE\n")