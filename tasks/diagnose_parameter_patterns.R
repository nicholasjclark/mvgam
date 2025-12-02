# Diagnostic script to identify exact parameter pattern mismatches
# For all 5 failing models: fit3, fit6, fit7, fit9, fit13
# Goal: Document exactly what parameters exist vs what extraction expects

devtools::load_all()
library(posterior)

cat("=== PARAMETER PATTERN DIAGNOSTICS ===\n")
cat("Analyzing parameter mismatches in failing models\n")
cat("=" , rep("=", 50), "\n\n", sep = "")

# Define failing models
failing_models <- list(
  fit3 = "tasks/fixtures/fit3.rds",  # Smooths + VAR - missing sds_* 
  fit6 = "tasks/fixtures/fit6.rds",  # Random effects - missing RE params
  fit7 = "tasks/fixtures/fit7.rds",  # Random effects - missing RE params  
  fit9 = "tasks/fixtures/fit9.rds",  # Nonlinear - missing dpars
  fit13 = "tasks/fixtures/fit13.rds" # Hierarchical - missing sds_*
)

# Function to diagnose smooth parameter patterns
diagnose_smooths <- function(model, prep) {
  cat("\n--- SMOOTH TERMS DIAGNOSIS ---\n")
  
  # Get all parameters - use mvgam's variables method
  all_params <- variables(model)
  
  # Find zs_* parameters (standardized smooth coefficients)
  zs_params <- grep("^zs_", all_params, value = TRUE)
  cat("Found zs_* parameters:\n")
  if (length(zs_params) > 0) {
    for (p in head(zs_params, 10)) cat("  ", p, "\n")
    if (length(zs_params) > 10) cat("  ... and", length(zs_params) - 10, "more\n")
  } else {
    cat("  NONE\n")
  }
  
  # Find sds_* parameters (standard deviations)
  sds_params <- grep("^sds_", all_params, value = TRUE)
  cat("\nFound sds_* parameters:\n")
  if (length(sds_params) > 0) {
    for (p in sds_params) cat("  ", p, "\n")
  } else {
    cat("  NONE\n")
  }
  
  # Analyze smooth label patterns
  if (length(zs_params) > 0) {
    cat("\nSmooth label analysis:\n")
    # Extract smooth labels from zs_ parameters
    smooth_labels <- unique(gsub("^zs_|\\[.*$", "", zs_params))
    for (label in smooth_labels) {
      cat("\nSmooth label:", label, "\n")
      
      # What extract_smooth_coef expects (BROKEN)
      expected_sds <- paste0("sds_", label, "[")
      cat("  EXPECTED sds pattern:", expected_sds, "\n")
      
      # What actually exists
      actual_sds <- grep(paste0("^sds_.*", gsub("_\\d+$", "", label)), 
                        all_params, value = TRUE)
      if (length(actual_sds) > 0) {
        cat("  ACTUAL sds param:", actual_sds[1], "\n")
      } else {
        cat("  ACTUAL sds param: NOT FOUND\n")
      }
      
      # Show the fix
      base_label <- gsub("_\\d+$", "", label)
      cat("  FIX: Extract base label '", base_label, "' from '", label, "'\n", sep = "")
    }
  }
}

# Function to diagnose random effects patterns  
diagnose_random_effects <- function(model, prep) {
  cat("\n--- RANDOM EFFECTS DIAGNOSIS ---\n")
  
  # Get all parameters - use mvgam's variables method
  all_params <- variables(model)
  
  # Find RE-related parameters
  re_params <- list(
    r_ = grep("^r_", all_params, value = TRUE),
    z_ = grep("^z_", all_params, value = TRUE),
    sd_ = grep("^sd_", all_params, value = TRUE),
    cor_ = grep("^cor_", all_params, value = TRUE),
    L_ = grep("^L_", all_params, value = TRUE)
  )
  
  for (type in names(re_params)) {
    params <- re_params[[type]]
    cat("\n", type, "parameters (", length(params), "):\n", sep = "")
    if (length(params) > 0) {
      for (p in head(params, 5)) cat("  ", p, "\n")
      if (length(params) > 5) cat("  ... and", length(params) - 5, "more\n")
    } else {
      cat("  NONE\n")
    }
  }
  
  # Check what prep expects
  if (!is.null(prep$sdata)) {
    cat("\nRE structures in prep$sdata:\n")
    re_matrices <- grep("^(Z_|J_)", names(prep$sdata), value = TRUE)
    for (mat in re_matrices) {
      dims <- dim(prep$sdata[[mat]])
      if (is.null(dims)) dims <- length(prep$sdata[[mat]])
      cat("  ", mat, ": ", paste(dims, collapse = " × "), "\n", sep = "")
    }
  }
  
  # Analyze mismatch
  cat("\nPotential issues:\n")
  if (length(re_params$r_) > 0 && length(re_matrices) > 0) {
    # Extract group names from r_ parameters
    r_groups <- unique(gsub("^r_|\\[.*$", "", re_params$r_))
    cat("  Groups from r_ params:", paste(r_groups, collapse = ", "), "\n")
    
    # Extract groups from Z_ matrices  
    z_groups <- unique(gsub("^Z_|_\\d+$", "", re_matrices))
    cat("  Groups from Z_ matrices:", paste(z_groups, collapse = ", "), "\n")
    
    if (!setequal(r_groups, z_groups)) {
      cat("  MISMATCH: Groups don't align between parameters and matrices\n")
    }
  }
}

# Function to diagnose nonlinear formula issues
diagnose_nonlinear <- function(model, prep) {
  cat("\n--- NONLINEAR FORMULA DIAGNOSIS ---\n")
  
  # Check formula for nl attribute
  has_nl <- !is.null(attr(model$formula$formula, "nl")) && 
            attr(model$formula$formula, "nl") == TRUE
  cat("Formula has nl attribute:", has_nl, "\n")
  
  # Check for dpars in prep
  has_dpars <- "dpars" %in% names(prep)
  cat("Prep has dpars component:", has_dpars, "\n")
  
  if (has_dpars) {
    cat("dpars contents:", names(prep$dpars), "\n")
    if ("mu" %in% names(prep$dpars)) {
      cat("  mu dimensions:", dim(prep$dpars$mu), "\n")
    }
  }
  
  # Check for nlpars
  if (!is.null(model$formula$pforms)) {
    cat("Model has pforms (predictor forms):", 
        length(model$formula$pforms), "components\n")
  }
  
  cat("\nIssue: prepare_predictions.mock_stanfit() doesn't generate dpars\n")
  cat("Fix needed: Check nl attribute and generate dpars$mu for nl models\n")
}

# Main diagnostic loop
for (model_name in names(failing_models)) {
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("DIAGNOSING:", model_name, "\n")
  cat(rep("=", 60), "\n", sep = "")
  
  # Load model
  model_path <- failing_models[[model_name]]
  if (!file.exists(model_path)) {
    cat("ERROR: Model file not found\n")
    next
  }
  
  model <- readRDS(model_path)
  cat("Model loaded successfully\n")
  
  # Basic info
  is_mv <- brms::is.mvbrmsformula(model$formula)
  cat("Multivariate:", is_mv, "\n")
  
  # Create minimal newdata
  newdata <- model$data[1:3, ]
  
  # Extract parameters
  obs_params <- extract_obs_parameters(model)
  cat("Observation parameters found:", length(obs_params), "\n")
  
  # Create prep object to analyze  
  tryCatch({
    full_draws <- posterior::as_draws_matrix(model$fit)
    obs_draws <- full_draws[, obs_params, drop = FALSE]
    mock_fit <- create_mock_stanfit(obs_draws)
    
    prep <- prepare_predictions.mock_stanfit(
      object = mock_fit,
      brmsfit = model$obs_model,
      newdata = newdata
    )
    cat("Prep object created successfully\n")
    
    # Run specific diagnostics based on model
    if (model_name %in% c("fit3", "fit13")) {
      diagnose_smooths(model, prep)
    } else if (model_name %in% c("fit6", "fit7")) {
      diagnose_random_effects(model, prep)
    } else if (model_name == "fit9") {
      diagnose_nonlinear(model, prep)
    }
    
  }, error = function(e) {
    cat("ERROR during prep creation:", e$message, "\n")
    
    # Still try to diagnose parameters
    cat("\nAttempting parameter analysis without prep...\n")
    all_params <- variables(model)
    cat("Total parameters in model:", length(all_params), "\n")
    
    if (model_name %in% c("fit3", "fit13")) {
      # Manual smooth diagnosis
      zs_params <- grep("^zs_", all_params, value = TRUE)
      sds_params <- grep("^sds_", all_params, value = TRUE)
      cat("Found", length(zs_params), "zs_* and", 
          length(sds_params), "sds_* parameters\n")
      
      if (length(zs_params) > 0 && length(sds_params) > 0) {
        cat("\nExample patterns:\n")
        cat("  zs param:", zs_params[1], "\n")
        cat("  sds param:", sds_params[1], "\n")
      }
    }
  })
}

cat("\n", rep("=", 60), "\n", sep = "")
cat("SUMMARY OF FIXES NEEDED\n")
cat(rep("=", 60), "\n\n", sep = "")

cat("1. SMOOTH TERMS (fit3, fit13):\n")
cat("   Problem: Looking for 'sds_count_1_1[' but actual is 'sds_count_1['\n")
cat("   Fix: In extract_smooth_coef(), extract base smooth name\n")
cat("   Code: gsub('_\\\\d+$', '', smooth_label)\n\n")

cat("2. RANDOM EFFECTS (fit6, fit7):\n")  
cat("   Problem: Parameter lookup patterns may not match actual structure\n")
cat("   Fix: Debug extract_random_effects_contribution() patterns\n")
cat("   Need: Detailed trace of which params are expected vs found\n\n")

cat("3. NONLINEAR (fit9):\n")
cat("   Problem: dpars$mu not generated for nl models\n")
cat("   Fix: In prepare_predictions.mock_stanfit(), check nl attribute\n") 
cat("   Code: Generate dpars$mu when nl = TRUE\n\n")

cat("Expected progression after fixes:\n")
cat("  Current: 8/13 models (61.5%)\n")
cat("  After smooth fix: 10/13 (76.9%)\n")
cat("  After RE fix: 11/13 (84.6%)\n")
cat("  After nl fix: 12/13 (92.3%)\n")

cat("\n=== DIAGNOSTICS COMPLETE ===\n")