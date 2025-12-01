# Debug script to reproduce hierarchical ZMVN test failure
# Based on "stancode generates correct hierarchical ZMVN(gr = habitat) model with custom prior"

# Load all functions (required by CLAUDE.md guidelines)
devtools::load_all()

# Set up test data exactly as in the test
setup_stan_test_data <- function() {
  set.seed(42)
  n_time <- 24
  n_series <- 3

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

  list(
    multivariate = multivariate
  )
}

cat("Setting up test data...\n")
data <- setup_stan_test_data()$multivariate

# Display data structure for verification
cat("\nData structure:\n")
cat("  Dimensions:", nrow(data), "rows x", ncol(data), "columns\n")
cat("  Time points:", length(unique(data$time)), "\n")
cat("  Series:", length(unique(data$series)), "\n")
cat("  Habitat groups:", paste(unique(data$habitat), collapse=", "), "\n")
cat("  Number of habitat groups:", length(unique(data$habitat)), "\n\n")

cat("Creating mvgam formula with hierarchical ZMVN trend...\n")
mf_with_trend <- mvgam_formula(
  biomass ~ 1,
  trend_formula = ~ 1 + x + ZMVN(gr = habitat)
)

cat("Setting up custom prior for alpha_cor_trend (testing prior integration)...\n")
custom_prior <- brms::prior("beta(5, 5)", class = "alpha_cor_trend")

cat("Generating Stan code...\n")
tryCatch({
  code_with_trend <- stancode(
    mf_with_trend,
    data = data,
    family = lognormal(),
    prior = custom_prior,
    validate = TRUE
  )
  
  cat("SUCCESS: Stan code generated successfully!\n")
  cat("Writing Stan code to file for analysis...\n")
  
  # Write the generated Stan code to a file for examination
  writeLines(code_with_trend, "tasks/validated_hierarchical_zmvn_model.stan")
  cat("Stan code written to: tasks/validated_hierarchical_zmvn_model.stan\n")
  
  # Check for some key patterns that the test expects
  cat("\n=== Checking for expected patterns ===\n")
  
  # Helper function to check patterns
  stan_pattern <- function(pattern, code) {
    grepl(pattern, code, perl = TRUE)
  }
  
  cat("\n--- DATA BLOCK STRUCTURES ---\n")
  # Critical variable naming issue: n_groups_trend vs N_groups_trend
  cat("✓ n_groups_trend declaration:", stan_pattern("int<lower=1> n_groups_trend;", code_with_trend), "\n")
  cat("✗ N_groups_trend (test expects):", stan_pattern("int<lower=1> N_groups_trend;", code_with_trend), "\n")
  
  # Check group indices array - tests expect N_groups_trend but code might have n_groups_trend
  cat("✓ group_inds_trend with n_groups:", stan_pattern("array\\[n_groups_trend\\] int", code_with_trend), "\n")
  cat("✗ group_inds_trend with N_groups (test expects):", stan_pattern("array\\[N_groups_trend\\] int", code_with_trend), "\n")
  
  # Check dimension variables
  cat("✓ N_series_trend:", stan_pattern("int<lower=1> N_series_trend;", code_with_trend), "\n")
  cat("✓ N_lv_trend:", stan_pattern("int<lower=1> N_lv_trend;", code_with_trend), "\n")
  cat("✓ n_time_trend:", stan_pattern("int<lower=1> n_time_trend;", code_with_trend), "\n")
  
  # Check array declarations using these dimensions
  cat("✓ times_trend array:", stan_pattern("array\\[n_time_trend, N_series_trend\\] int times_trend;", code_with_trend), "\n")
  
  cat("\n--- PARAMETERS BLOCK STRUCTURES ---\n")
  # Check hierarchical correlation parameters with _trend suffix
  cat("✓ L_Omega_global_trend:", stan_pattern("cholesky_factor_corr\\[N_lv_trend\\] L_Omega_global_trend;", code_with_trend), "\n")
  cat("✓ L_deviation_group_trend:", stan_pattern("array\\[n_groups_trend\\] cholesky_factor_corr\\[N_lv_trend\\] L_deviation_group_trend;", code_with_trend), "\n")
  cat("✓ alpha_cor_trend:", stan_pattern("real<lower=0, upper=1> alpha_cor_trend;", code_with_trend), "\n")
  
  # Check trend variance/innovation parameters
  cat("✓ sigma_trend:", stan_pattern("vector<lower=0>\\[N_lv_trend\\] sigma_trend;", code_with_trend), "\n")
  cat("✓ Z loading matrix:", stan_pattern("matrix\\[N_series_trend, N_lv_trend\\] Z;", code_with_trend), "\n")
  
  cat("\n--- TRANSFORMED PARAMETERS BLOCK ---\n")
  # Check trend computation
  cat("✓ trend matrix declaration:", stan_pattern("matrix\\[n_time_trend, N_series_trend\\] trend;", code_with_trend), "\n")
  cat("✓ LV_trend declaration:", stan_pattern("matrix\\[n_time_trend, N_lv_trend\\] LV_trend;", code_with_trend), "\n")
  cat("✓ mu_trend declaration:", stan_pattern("vector\\[N_trend\\] mu_trend;", code_with_trend), "\n")
  
  cat("\n--- FUNCTIONS BLOCK ---\n")
  # Check hierarchical correlation functions
  cat("✓ combine_cholesky function:", stan_pattern("matrix combine_cholesky\\(", code_with_trend), "\n")
  
  cat("\n--- MODEL BLOCK ---\n")
  # Check group-specific correlation computation
  cat("✓ L_Omega_group_trend computation:", stan_pattern("L_Omega_group_trend = combine_cholesky\\(L_Omega_global_trend, L_deviation_group_trend\\[g\\], alpha_cor_trend\\)", code_with_trend), "\n")
  
  # Check custom prior application
  cat("✓ Custom alpha_cor_trend prior:", stan_pattern("alpha_cor_trend ~ beta\\(5, 5\\);", code_with_trend), "\n")
  
  # Check hierarchical structure loop
  cat("✓ Group loop (for g in 1:n_groups_trend):", stan_pattern("for \\(g in 1:n_groups_trend\\)", code_with_trend), "\n")
  cat("✗ Group loop (for g in 1:N_groups_trend):", stan_pattern("for \\(g in 1:N_groups_trend\\)", code_with_trend), "\n")
  
  cat("\n--- CRITICAL ISSUES SUMMARY ---\n")
  # Summarize variable naming inconsistencies
  n_groups_uses <- length(gregexpr("n_groups_trend", code_with_trend, fixed = TRUE)[[1]])
  N_groups_uses <- length(gregexpr("N_groups_trend", code_with_trend, fixed = TRUE)[[1]])
  
  if (N_groups_uses == -1) N_groups_uses <- 0
  if (n_groups_uses == -1) n_groups_uses <- 0
  
  cat("Variable naming analysis:\n")
  cat("  'n_groups_trend' appears:", n_groups_uses, "times\n")
  cat("  'N_groups_trend' appears:", N_groups_uses, "times\n")
  
  if (n_groups_uses > 0 && N_groups_uses == 0) {
    cat("  ⚠️ WARNING: Using 'n_groups_trend' but test expects 'N_groups_trend'\n")
  }
  
  # Check for intercept and coefficient patterns
  cat("\nTrend formula components:\n")
  cat("  Intercept_trend:", stan_pattern("Intercept_trend", code_with_trend), "\n")
  cat("  b_trend coefficients:", stan_pattern("b_trend", code_with_trend), "\n")
  cat("  X_trend matrix:", stan_pattern("matrix\\[N_trend, K_trend\\] X_trend;", code_with_trend), "\n")
  
  # Check for centering variables (should be present since we have ~ 1 + x)
  cat("\nCentering variables (should exist with explicit intercept):\n")
  cat("  Xc_trend:", stan_pattern("Xc_trend", code_with_trend), "\n")
  cat("  means_X_trend:", stan_pattern("means_X_trend", code_with_trend), "\n")
  
  cat("\n=== Analysis complete ===\n")
  cat("Check tasks/validated_hierarchical_zmvn_model.stan for detailed Stan code inspection.\n")
  
  # Also generate Stan data to check what's being passed
  cat("\n=== Generating Stan data ===\n")
  tryCatch({
    stan_data <- standata(
      mf_with_trend,
      data = data,
      family = lognormal(),
      prior = custom_prior
    )
    
    cat("Stan data generated successfully!\n")
    cat("\nKey Stan data dimensions:\n")
    cat("  N (observations):", stan_data$N, "\n")
    if ("N_trend" %in% names(stan_data)) cat("  N_trend:", stan_data$N_trend, "\n")
    if ("N_series_trend" %in% names(stan_data)) cat("  N_series_trend:", stan_data$N_series_trend, "\n")
    if ("N_lv_trend" %in% names(stan_data)) cat("  N_lv_trend:", stan_data$N_lv_trend, "\n")
    if ("n_time_trend" %in% names(stan_data)) cat("  n_time_trend:", stan_data$n_time_trend, "\n")
    if ("n_groups_trend" %in% names(stan_data)) cat("  n_groups_trend:", stan_data$n_groups_trend, "\n")
    if ("N_groups_trend" %in% names(stan_data)) cat("  N_groups_trend:", stan_data$N_groups_trend, "\n")
    
    # Check group indices
    if ("group_inds_trend" %in% names(stan_data)) {
      cat("\nGroup indices mapping:\n")
      cat("  group_inds_trend:", paste(unique(stan_data$group_inds_trend), collapse=", "), "\n")
      cat("  Length of group_inds_trend:", length(stan_data$group_inds_trend), "\n")
    }
    
    # Save Stan data for inspection
    saveRDS(stan_data, "hierarchical_zmvn_standata.rds")
    cat("\nStan data saved to: hierarchical_zmvn_standata.rds\n")
    
  }, error = function(e2) {
    cat("ERROR occurred during Stan data generation:\n")
    cat("Message:", conditionMessage(e2), "\n")
  })
  
}, error = function(e) {
  cat("ERROR occurred during Stan code generation:\n")
  cat("Message:", conditionMessage(e), "\n")
  cat("Call:", deparse(conditionCall(e)), "\n")
  
  if (exists("e$trace")) {
    cat("Traceback:\n")
    print(e$trace)
  }
})

# Generate all 6 hierarchical models for comprehensive analysis
# Task 6.5: Generate AR, VAR, ZMVN × (factor, non-factor)

models <- list(
  # Non-factor models with CUSTOM PRIORS (testing prior integration)
  ar_hier = list(
    formula = mvgam_formula(count ~ 1 + x, trend_formula = ~ 1 + AR(gr = habitat)),
    family = poisson(),
    filename = "tasks/validated_ar_hier.stan",
    description = "AR hierarchical (non-factor) with custom alpha_cor_trend ~ uniform(0, 1)",
    prior = brms::prior("uniform(0, 1)", class = "alpha_cor_trend")
  ),
  var_hier = list(
    formula = mvgam_formula(count ~ 1 + x, trend_formula = ~ 1 + VAR(p = 1, gr = habitat)),
    family = poisson(),
    filename = "tasks/validated_var_hier.stan", 
    description = "VAR hierarchical (non-factor) with custom alpha_cor_trend ~ beta(2, 8)",
    prior = brms::prior("beta(2, 8)", class = "alpha_cor_trend")
  ),
  zmvn_hier = list(
    formula = mvgam_formula(biomass ~ 1, trend_formula = ~ 1 + x + ZMVN(gr = habitat)),
    family = lognormal(),
    filename = "tasks/validated_zmvn_hier.stan",
    description = "ZMVN hierarchical (non-factor) with DEFAULT alpha_cor_trend prior (should be beta(3, 2))",
    prior = NULL  # No custom prior - should use common_trend_priors default
  ),
  
  # Factor models should FAIL with validation error (hierarchical + factor incompatible)
  ar_hier_factor = list(
    formula = mvgam_formula(count ~ 1 + x, trend_formula = ~ 1 + AR(gr = habitat, n_lv = 2)),
    family = poisson(),
    filename = "tasks/validated_ar_hier_factor.stan",
    description = "AR hierarchical with factor structure (n_lv = 2) - SHOULD FAIL",
    prior = NULL,
    expect_error = TRUE
  ),
  var_hier_factor = list(
    formula = mvgam_formula(count ~ 1 + x, trend_formula = ~ 1 + VAR(p = 1, gr = habitat, n_lv = 2)),
    family = poisson(),
    filename = "tasks/validated_var_hier_factor.stan",
    description = "VAR hierarchical with factor structure (n_lv = 2) - SHOULD FAIL",
    prior = NULL,
    expect_error = TRUE
  ),
  zmvn_hier_factor = list(
    formula = mvgam_formula(biomass ~ 1, trend_formula = ~ 1 + x + ZMVN(gr = habitat, n_lv = 2)),
    family = lognormal(),
    filename = "tasks/validated_zmvn_hier_factor.stan", 
    description = "ZMVN hierarchical with factor structure (n_lv = 2) - SHOULD FAIL",
    prior = NULL,
    expect_error = TRUE
  )
)

cat("\n", paste0(rep("=", 80), collapse=""), "\n")
cat("TESTING HIERARCHICAL MODELS WITH CUSTOM ALPHA_COR_TREND PRIORS\n") 
cat("Task 6.6 Validation: Prior Integration System\n")
cat(paste0(rep("=", 80), collapse=""), "\n")
cat("EXPECTED RESULTS:\n")
cat("✅ AR model: custom uniform(0, 1) prior\n")
cat("✅ VAR model: custom beta(2, 8) prior\n") 
cat("✅ ZMVN model: default beta(3, 2) prior (no custom prior specified)\n")
cat("❌ Factor models: should FAIL with validation error (hierarchical + factor incompatible)\n")
cat(paste0(rep("-", 80), collapse=""), "\n")

generated_files <- character(0)
errors_encountered <- character(0)

for (model_name in names(models)) {
  model <- models[[model_name]]
  
  cat("\n--- Generating", model$description, "---\n")
  cat("Formula:", deparse(model$formula$formula), "\n")
  cat("Trend formula:", deparse(model$formula$trend_formula), "\n")
  cat("Family:", deparse(substitute(model$family)), "\n")
  
  tryCatch({
    # Handle prior parameter correctly
    stan_code <- if (!is.null(model$prior)) {
      stancode(
        model$formula,
        data = data,
        family = model$family,
        prior = model$prior,
        validate = TRUE
      )
    } else {
      stancode(
        model$formula,
        data = data,
        family = model$family,
        validate = TRUE
      )
    }
    
    # Check if this model was expected to fail
    if (isTRUE(model$expect_error)) {
      cat("⚠️ UNEXPECTED SUCCESS:", model$filename, "(expected to fail due to hierarchical + factor incompatibility)\n")
      writeLines(stan_code, model$filename)
      generated_files <- c(generated_files, model$filename)
    } else {
      cat("✅ SUCCESS: Generated", model$filename, "\n")
      
      # Check for expected prior in Stan code
      if (!is.null(model$prior)) {
        prior_class <- model$prior$class
        prior_string <- model$prior$prior
        if (grepl(prior_string, stan_code, fixed = TRUE)) {
          cat("   ✓ Custom prior", prior_string, "correctly applied\n")
        } else {
          cat("   ⚠️ Custom prior", prior_string, "NOT found in Stan code\n")
        }
      } else {
        # Check for default beta(3, 2) prior
        if (grepl("alpha_cor_trend ~ beta(3, 2)", stan_code, fixed = TRUE)) {
          cat("   ✓ Default beta(3, 2) prior correctly applied\n")
        } else {
          cat("   ⚠️ Default beta(3, 2) prior NOT found in Stan code\n")
        }
      }
      
      writeLines(stan_code, model$filename)
      generated_files <- c(generated_files, model$filename)
    }
    
  }, error = function(e) {
    # Check if this error was expected
    if (isTRUE(model$expect_error)) {
      cat("✅ EXPECTED ERROR for", model$filename, "(hierarchical + factor incompatibility):\n")
      cat("   Message:", conditionMessage(e), "\n")
      # This is expected behavior, not a real error
    } else {
      cat("❌ UNEXPECTED ERROR generating", model$filename, ":\n")
      cat("   Message:", conditionMessage(e), "\n") 
      errors_encountered <- c(errors_encountered, paste(model$filename, ":", conditionMessage(e)))
    }
  })
}

cat("\n", paste0(rep("=", 80), collapse=""), "\n")
cat("GENERATION SUMMARY\n")
cat(paste0(rep("=", 80), collapse=""), "\n")

cat("✅ Successfully generated files:\n")
for (file in generated_files) {
  cat("  -", file, "\n")
}

if (length(errors_encountered) > 0) {
  cat("\n❌ Errors encountered:\n")
  for (error in errors_encountered) {
    cat("  -", error, "\n")
  }
}

cat("\nTotal files generated:", length(generated_files), "out of 6\n")

# Preserve original ZMVN analysis for reference
if ("tasks/validated_zmvn_hier.stan" %in% generated_files) {
  cat("\n--- Original ZMVN analysis ---\n")
  cat("(ZMVN model completed as part of 6-model generation)\n")
  cat("File written to: tasks/validated_zmvn_hier.stan\n")
}

# Validate all generated Stan models with stanc() compilation check
cat("\n================================================================================ \n")
cat("STAN COMPILATION VALIDATION\n")
cat("Task 6.8c Validation: stanc() Compilation Check for All Generated Models\n")
cat("================================================================================ \n")

if (length(generated_files) > 0) {
  compilation_results <- list()
  
  for (file_path in generated_files) {
    cat("--- Validating Stan compilation:", basename(file_path), "---\n")
    
    # Read Stan code from file
    stan_code <- paste(readLines(file_path), collapse = "\n")
    
    # Test Stan compilation using stanc()
    compilation_result <- tryCatch({
      # Use rstan::stanc() for compilation validation
      rstan::stanc(model_code = stan_code, verbose = FALSE)
      list(success = TRUE, error = NULL)
    }, error = function(e) {
      list(success = FALSE, error = conditionMessage(e))
    })
    
    compilation_results[[basename(file_path)]] <- compilation_result
    
    if (compilation_result$success) {
      cat("✅ COMPILATION SUCCESS:", basename(file_path), "compiles without errors\n")
    } else {
      cat("❌ COMPILATION FAILURE:", basename(file_path), "\n")
      cat("   Error message:", compilation_result$error, "\n")
    }
    cat("\n")
  }
  
  # Summary of compilation results
  cat("================================================================================ \n")
  cat("COMPILATION SUMMARY\n")
  cat("================================================================================ \n")
  
  success_count <- sum(sapply(compilation_results, function(x) x$success))
  total_count <- length(compilation_results)
  
  cat("Models that compile successfully:", success_count, "/", total_count, "\n")
  
  if (success_count < total_count) {
    cat("\n❌ MODELS WITH COMPILATION ERRORS:\n")
    for (model_name in names(compilation_results)) {
      if (!compilation_results[[model_name]]$success) {
        cat("  -", model_name, "\n")
      }
    }
    
    cat("\n🔧 RECOMMENDED ACTIONS:\n")
    cat("1. Fix compilation errors using Task 6.8c subtasks\n")
    cat("2. Re-run debug script to validate fixes\n")
    cat("3. Proceed to Task 6.9 only when all models compile\n")
  } else {
    cat("\n🎉 ALL MODELS COMPILE SUCCESSFULLY!\n")
    cat("✅ Ready to proceed with Task 6.9: Update test expectations\n")
  }
} else {
  cat("No Stan models were generated to validate.\n")
}

cat("\nTask 6.5 generation complete. Ready for stan-code-expert analysis.\n")