# Test script to investigate dpars generation for nonlinear models
# Goal: Understand how to replicate brms's predictor.bprepnl() logic
# Reference: nlpar_brms_internals.R contains the brms implementation

devtools::load_all()
library(posterior)

cat("=== INVESTIGATING NONLINEAR PREDICTOR COMPUTATION ===\n\n")
cat("Based on brms internals in nlpar_brms_internals.R\n")
cat("Key function: predictor.bprepnl()\n\n")

# Load fit9 (nonlinear model)
fit9_path <- "tasks/fixtures/fit9.rds"
if (!file.exists(fit9_path)) {
  stop("fit9.rds not found. Run fit_and_save_models.R first.")
}

fit9 <- readRDS(fit9_path)

cat("Model loaded: fit9\n")
cat("Formula:", deparse(fit9$formula$formula), "\n")
cat("Has nl attribute:",
    !is.null(attr(fit9$obs_model$formula$formula, "nl")) &&
    isTRUE(attr(fit9$obs_model$formula$formula, "nl")), "\n")
cat("has_nlpars(fit9$obs_model):", has_nlpars(fit9$obs_model), "\n\n")

# Prepare newdata
newdata <- fit9$data[1:5, ]
cat("Using", nrow(newdata), "rows of newdata\n\n")

# Extract obs parameters
obs_params <- extract_obs_parameters(fit9)
cat("Observation parameters (", length(obs_params), "):\n", sep = "")
print(obs_params)

# Create mock stanfit
full_draws <- posterior::as_draws_matrix(fit9$fit)
obs_draws <- full_draws[, obs_params, drop = FALSE]
mock_fit <- create_mock_stanfit(obs_draws)
cat("\nMock stanfit created with", nrow(obs_draws), "draws\n\n")

# Create prep object with our current method
prep <- prepare_predictions.mock_stanfit(
  object = mock_fit,
  brmsfit = fit9$obs_model,
  newdata = newdata
)

cat("=== STEP 1: Examine Formula Structure ===\n")
cat("Formula class:", paste(class(fit9$obs_model$formula), collapse = ", "),
    "\n")
cat("Formula components:\n")
print(names(fit9$obs_model$formula))

# Extract the formula object
form <- fit9$obs_model$formula

# Check for pforms (parameter formulas - these are the nlpars)
if (!is.null(form$pforms)) {
  cat("\n✓ pforms exist (parameter formulas for nlpars)\n")
  cat("Number of nlpars:", length(form$pforms), "\n")
  cat("nlpar names:", paste(names(form$pforms), collapse = ", "), "\n\n")

  # Show each nlpar formula
  for (nlpar_name in names(form$pforms)) {
    cat("  ", nlpar_name, ":", deparse(form$pforms[[nlpar_name]]$formula),
        "\n")
  }
} else {
  cat("\n✗ No pforms found\n")
}

# Check for nlform (the main nonlinear expression)
cat("\nMain formula (nlform):\n")
cat("  LHS:", deparse(form$formula[[2]]), "\n")
cat("  RHS:", deparse(form$formula[[3]]), "\n")
nlform_expr <- form$formula[[3]]

cat("\n=== STEP 2: Extract Design Matrices for nlpars ===\n")
cat("From prep$sdata, look for X_<nlpar> matrices\n\n")

if (!is.null(form$pforms)) {
  nlpar_names <- names(form$pforms)

  for (nlp in nlpar_names) {
    # Check for design matrix
    x_name <- paste0("X_", nlp)
    if (x_name %in% names(prep$sdata)) {
      cat("✓ Found design matrix:", x_name, "\n")
      cat("  Dimensions:",
          paste(dim(prep$sdata[[x_name]]), collapse = " × "), "\n")
    } else {
      cat("✗ Missing design matrix:", x_name, "\n")
    }

    # Check for intercept
    intercept_name <- paste0("Intercept_", nlp)
    if (intercept_name %in% names(prep$sdata)) {
      cat("✓ Found intercept:", intercept_name, "=",
          prep$sdata[[intercept_name]], "\n")
    }
  }
}

cat("\n=== STEP 3: Extract Parameter Draws for nlpars ===\n")
cat("From prep$draws, look for b_<nlpar> coefficients\n")
cat("NOTE: Nonlinear models use array notation: b_nlpar[index]\n\n")

if (!is.null(form$pforms)) {
  nlpar_names <- names(form$pforms)

  for (nlp in nlpar_names) {
    # Pattern for nonlinear coefficients (array notation)
    # Try both patterns: b_nlpar[index] and b_nlpar_coef
    array_pattern <- paste0("^b_", nlp, "\\[")
    underscore_pattern <- paste0("^b_", nlp, "_")

    array_params <- grep(array_pattern, colnames(prep$draws), value = TRUE)
    underscore_params <- grep(underscore_pattern, colnames(prep$draws),
                              value = TRUE)

    matching_params <- c(array_params, underscore_params)

    if (length(matching_params) > 0) {
      cat("✓ Found", length(matching_params), "coefficients for", nlp, ":\n")
      print(matching_params)

      if (length(array_params) > 0) {
        cat("  Using array notation pattern\n")
      }
      if (length(underscore_params) > 0) {
        cat("  Using underscore pattern\n")
      }
    } else {
      cat("✗ No coefficients found for", nlp, "\n")
      cat("  Tried patterns:", array_pattern, "and", underscore_pattern, "\n")
    }

    cat("\n")
  }
}

cat("=== STEP 4: Compute Linear Predictors for Each nlpar ===\n")
cat("Replicate brms get_nlpar() logic\n\n")

if (!is.null(form$pforms)) {
  nlpar_names <- names(form$pforms)
  eta_nlpars <- list()

  for (nlp in nlpar_names) {
    cat("Computing eta for nlpar:", nlp, "\n")

    # Get design matrix
    x_name <- paste0("X_", nlp)
    if (!x_name %in% names(prep$sdata)) {
      cat("  ✗ Skipping - no design matrix\n\n")
      next
    }

    X_nlpar <- prep$sdata[[x_name]]
    cat("  Design matrix:", paste(dim(X_nlpar), collapse = " × "), "\n")

    # Get coefficient parameters (try both array and underscore patterns)
    array_pattern <- paste0("^b_", nlp, "\\[")
    underscore_pattern <- paste0("^b_", nlp, "_")

    array_params <- grep(array_pattern, colnames(prep$draws), value = TRUE)
    underscore_params <- grep(underscore_pattern, colnames(prep$draws),
                              value = TRUE)

    matching_params <- c(array_params, underscore_params)

    if (length(matching_params) == 0) {
      cat("  ✗ Skipping - no coefficients\n\n")
      next
    }

    b_nlpar <- prep$draws[, matching_params, drop = FALSE]
    cat("  Coefficients:", paste(dim(b_nlpar), collapse = " × "), "\n")

    # Check dimensions match
    if (ncol(b_nlpar) != ncol(X_nlpar)) {
      cat("  ✗ Dimension mismatch! X has", ncol(X_nlpar),
          "columns but b has", ncol(b_nlpar), "\n\n")
      next
    }

    # Compute linear predictor: b %*% t(X)
    # Result should be [ndraws × nobs]
    eta <- b_nlpar %*% t(X_nlpar)
    cat("  Linear predictor:", paste(dim(eta), collapse = " × "), "\n")
    cat("  First values:", head(eta[1, ], 3), "...\n")

    eta_nlpars[[nlp]] <- eta
    cat("  ✓ Success\n\n")
  }

  cat("Successfully computed", length(eta_nlpars), "/", length(nlpar_names),
      "nlpar predictors\n")
}

cat("\n=== STEP 5: Extract Covariates ===\n")
cat("Check for additional covariates in prep$C or prep$sdata\n\n")

# Extract covariate information
covariates <- list()

# Look for covariates in prep (brms stores in prep$C)
if ("C" %in% names(prep) && !is.null(prep$C)) {
  cat("✓ prep$C exists\n")
  cat("Covariates:", paste(names(prep$C), collapse = ", "), "\n")
  for (cov_name in names(prep$C)) {
    cat("  ", cov_name, "dimensions:",
        paste(dim(prep$C[[cov_name]]), collapse = " × "), "\n")
    covariates[[cov_name]] <- prep$C[[cov_name]]
  }
} else {
  cat("✗ No prep$C component\n")
  cat("Note: Covariates might be in prep$sdata as C_* entries\n\n")

  # Check for C_* in sdata
  c_vars <- grep("^C_", names(prep$sdata), value = TRUE)
  if (length(c_vars) > 0) {
    cat("Found", length(c_vars), "covariate matrices in prep$sdata:\n")
    for (c_var in c_vars) {
      cat("  ", c_var, ":", class(prep$sdata[[c_var]]), "\n")
      cat("    dimensions:", paste(dim(prep$sdata[[c_var]]), collapse = " × "),
          "\n")
    }

    # Extract covariate variable names from formula
    # The nonlinear formula references variable names (e.g., "x")
    # We need to map C_1, C_2, etc. to their actual variable names
    cat("\nAttempting to map C_* to variable names...\n")

    # Extract all variable names from the formula
    all_vars <- all.vars(form$formula)
    cat("Variables in formula:", paste(all_vars, collapse = ", "), "\n")

    # Remove response and nlpar names to get covariate names
    response_var <- as.character(form$formula[[2]])
    nlpar_names <- names(form$pforms)
    covariate_names <- setdiff(all_vars, c(response_var, nlpar_names))
    cat("Covariate variable names:", paste(covariate_names, collapse = ", "),
        "\n\n")

    # Map C_1, C_2, etc. to actual variable names
    # brms uses C_1, C_2 in order of appearance
    if (length(covariate_names) == length(c_vars)) {
      for (i in seq_along(c_vars)) {
        cov_data <- prep$sdata[[c_vars[i]]]
        cov_name <- covariate_names[i]

        cat("Mapping", c_vars[i], "to", cov_name, "\n")

        # Broadcast covariate across draws if needed
        if (is.array(cov_data) && length(dim(cov_data)) == 1) {
          # Vector - broadcast to matrix [ndraws × nobs]
          n_draws <- nrow(prep$draws)
          covariates[[cov_name]] <- matrix(
            cov_data,
            nrow = n_draws,
            ncol = length(cov_data),
            byrow = TRUE
          )
          cat("  Broadcasted vector to", paste(dim(covariates[[cov_name]]),
                                                collapse = " × "), "\n")
        } else {
          covariates[[cov_name]] <- cov_data
          cat("  Using as-is\n")
        }
      }
    } else {
      cat("✗ Mismatch: Found", length(c_vars), "C_* entries but",
          length(covariate_names), "covariate names\n")
    }
  }
}

cat("\n=== STEP 6: Evaluate Nonlinear Formula ===\n")
cat("Replicate brms predictor.bprepnl() evaluation logic\n\n")

if (exists("eta_nlpars") && length(eta_nlpars) > 0) {
  cat("Nonlinear formula expression:\n")
  cat("  ", deparse(nlform_expr), "\n\n")

  # Build args list (nlpars + covariates)
  args <- eta_nlpars  # Start with computed nlpars

  # Add any extracted covariates
  if (exists("covariates") && length(covariates) > 0) {
    for (cov_name in names(covariates)) {
      args[[cov_name]] <- covariates[[cov_name]]
    }
  }

  cat("Arguments for evaluation:\n")
  for (arg_name in names(args)) {
    cat("  ", arg_name, ":", paste(dim(args[[arg_name]]), collapse = " × "),
        "\n")
  }

  cat("\nAttempting to evaluate formula...\n")

  tryCatch({
    # Try vectorized evaluation (brms uses this when prep$loop = TRUE)
    mu <- eval(nlform_expr, envir = args)

    cat("✓ Evaluation succeeded!\n")
    cat("Result class:", class(mu), "\n")
    cat("Result dimensions:", paste(dim(mu), collapse = " × "), "\n")
    cat("First few values:\n")
    print(mu[1:min(3, nrow(mu)), 1:min(3, ncol(mu))])

    cat("\n=== SUCCESS ===\n")
    cat("We can replicate brms nonlinear predictor computation!\n")
    cat("This mu matrix is what should go in prep$dpars$mu\n")

  }, error = function(e) {
    cat("✗ Evaluation failed:", e$message, "\n")
    cat("\nThis might mean:\n")
    cat("1. Missing covariates in args\n")
    cat("2. Formula uses functions not available in R environment\n")
    cat("3. Dimension mismatches between nlpars\n")
    cat("\nDebug info:\n")
    cat("Available args:", paste(names(args), collapse = ", "), "\n")
    cat("Formula variables:", paste(all.vars(nlform_expr), collapse = ", "),
        "\n")
  })
}

cat("\n=== STEP 7: Check What prep Components Are Missing ===\n")
cat("Compare our prep to what brms would create\n\n")

cat("Current prep components:\n")
print(names(prep))

cat("\nComponents we need to add for nl models:\n")
cat("- dpars$mu: matrix [ndraws × nobs] of computed nonlinear predictors\n")
cat("- used_nlpars: character vector of nlpar names (from pforms)\n")
cat("- nlform: the nonlinear formula expression (from formula[[3]])\n")
cat("- C: list of covariate matrices (if any)\n")
cat("- loop: logical indicating if formula is vectorized (default TRUE)\n")
cat("- env: environment for evaluation (use parent.frame())\n")

cat("\n=== IMPLEMENTATION CHECKLIST ===\n")
cat("[x] Extract nlpar names from formula$pforms\n")
cat("[x] Extract design matrices X_<nlpar> from standata\n")
cat("[x] Extract coefficient parameters b_<nlpar>_* from draws\n")
cat("[x] Compute linear predictors for each nlpar\n")
cat("[x] Extract covariates (if any)\n")
cat("[x] Evaluate nonlinear formula expression\n")
cat("[ ] Integrate into prepare_predictions.mock_stanfit()\n")
cat("[ ] Add dpars component to prep object for nl models\n")
cat("[ ] Test with fit9\n")
cat("[ ] Verify extract_linpred_from_prep() can use dpars$mu\n")

cat("\n=== NEXT STEPS ===\n")
cat("1. Update prepare_predictions.mock_stanfit() to detect nl models\n")
cat("2. When has_nlpars(brmsfit) is TRUE:\n")
cat("   - Extract nlpar names from formula$pforms\n")
cat("   - For each nlpar, compute eta using design matrices and parameters\n")
cat("   - Evaluate nlform expression with nlpar values\n")
cat("   - Add dpars$mu to prep object\n")
cat("3. Test the implementation with this script\n")
cat("4. Verify fit9 works in test_extract_linpred_all_models.R\n")
