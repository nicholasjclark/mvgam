# Validate GP Predictions Against brms
# Tests mvgam GP infrastructure against multiple brms GP configurations
#
# CRITICAL: Run fit_brms_gp_models.R first to generate fixtures

devtools::load_all()

library(brms)
library(posterior)

cat("\n")
cat(strrep("=", 78), "\n")
cat("GP PREDICTION VALIDATION SUITE\n")
cat(strrep("=", 78), "\n\n")

# Check that fixtures exist
fixture_dir <- "tasks/fixtures/brms_gp"
if (!dir.exists(fixture_dir)) {
  stop("Fixture directory not found. Run fit_brms_gp_models.R first.")
}

# Define test models
test_models <- list(
  list(
    name = "Simple GP (k=5)",
    file = "fit1_simple_gp.rds",
    desc = "Basic approximate GP with 5 basis functions"
  ),
  list(
    name = "Grouped GP (by=group)",
    file = "fit2_grouped_gp.rds",
    desc = "GP with categorical grouping factor"
  ),
  list(
    name = "High-dimensional GP (k=10)",
    file = "fit3_gp_k10.rds",
    desc = "GP with 10 basis functions"
  ),
  list(
    name = "Scaled GP (scale='c')",
    file = "fit4_gp_scale_c.rds",
    desc = "GP with continuous scaling"
  )
)

# Results storage
results <- vector("list", length(test_models))
names(results) <- sapply(test_models, function(x) x$name)

# Test each model
for (i in seq_along(test_models)) {
  model_info <- test_models[[i]]

  cat(sprintf("\n[%d/%d] Testing: %s\n", i, length(test_models), model_info$name))
  cat(sprintf("      %s\n", model_info$desc))

  # Load fitted model
  fit_path <- file.path(fixture_dir, model_info$file)
  if (!file.exists(fit_path)) {
    cat(sprintf("      ✗ SKIP: File not found: %s\n", fit_path))
    results[[i]] <- list(status = "SKIP", reason = "File not found")
    next
  }

  fit <- readRDS(fit_path)
  cat("      ✓ Model loaded\n")

  # Create newdata (use first 6 observations)
  newdata <- fit$data[1:6, ]

  # Get brms predictions
  tryCatch({
    brms_pred <- posterior_linpred(fit, newdata = newdata, summary = FALSE)
    cat(sprintf("      ✓ brms: %d draws × %d obs\n",
                nrow(brms_pred), ncol(brms_pred)))
  }, error = function(e) {
    cat(sprintf("      ✗ brms prediction failed: %s\n", e$message))
    results[[i]] <<- list(status = "FAIL", reason = "brms error",
                          error = e$message)
    return(NULL)
  })

  # Get mvgam predictions using our infrastructure
  tryCatch({
    prep <- brms::prepare_predictions(fit, newdata = newdata)

    # Check for GP terms
    gp_info <- mvgam:::detect_gp_terms(prep)
    if (is.null(gp_info)) {
      cat("      ✗ No GP terms detected\n")
      results[[i]] <- list(status = "FAIL", reason = "No GP detected")
      next
    }

    cat(sprintf("      ✓ Detected %d GP term(s): %s\n",
                gp_info$n_terms,
                paste(gp_info$suffixes, collapse = ", ")))

    # Compute predictions
    mvgam_pred <- mvgam:::extract_linpred_from_prep(prep)
    cat(sprintf("      ✓ mvgam: %d draws × %d obs\n",
                nrow(mvgam_pred), ncol(mvgam_pred)))

  }, error = function(e) {
    cat(sprintf("      ✗ mvgam prediction failed: %s\n", e$message))
    results[[i]] <<- list(status = "FAIL", reason = "mvgam error",
                          error = e$message)
    return(NULL)
  })

  # Compare predictions
  if (exists("brms_pred") && exists("mvgam_pred")) {
    # Check dimensions
    if (!all(dim(mvgam_pred) == dim(brms_pred))) {
      cat(sprintf("      ✗ Dimension mismatch: mvgam %d×%d vs brms %d×%d\n",
                  nrow(mvgam_pred), ncol(mvgam_pred),
                  nrow(brms_pred), ncol(brms_pred)))
      results[[i]] <- list(status = "FAIL", reason = "Dimension mismatch")
      next
    }

    # Calculate differences
    abs_diff <- abs(mvgam_pred - brms_pred)
    max_abs <- max(abs_diff)
    mean_abs <- mean(abs_diff)

    # Test thresholds
    pass_1e6 <- max_abs < 1e-6
    pass_1e8 <- max_abs < 1e-8
    pass_1e10 <- max_abs < 1e-10

    cat(sprintf("      Max abs diff: %.2e", max_abs))
    if (pass_1e10) {
      cat(" (< 1e-10) ✓✓✓\n")
      precision <- "excellent"
    } else if (pass_1e8) {
      cat(" (< 1e-8) ✓✓\n")
      precision <- "very good"
    } else if (pass_1e6) {
      cat(" (< 1e-6) ✓\n")
      precision <- "good"
    } else {
      cat(" (>= 1e-6) ✗\n")
      precision <- "poor"
    }

    # Store results
    results[[i]] <- list(
      status = if (pass_1e6) "PASS" else "FAIL",
      max_abs_diff = max_abs,
      mean_abs_diff = mean_abs,
      precision = precision,
      n_gp_terms = gp_info$n_terms,
      gp_suffixes = gp_info$suffixes
    )
  }

  # Clean up for next iteration
  rm(list = c("brms_pred", "mvgam_pred", "prep", "gp_info", "fit"),
     envir = environment())
}

# Summary report
cat("\n")
cat(strrep("=", 78), "\n")
cat("VALIDATION SUMMARY\n")
cat(strrep("=", 78), "\n\n")

passed <- sum(sapply(results, function(x) x$status == "PASS"))
failed <- sum(sapply(results, function(x) x$status == "FAIL"))
skipped <- sum(sapply(results, function(x) x$status == "SKIP"))

cat(sprintf("Total tests:  %d\n", length(results)))
cat(sprintf("Passed:       %d ✓\n", passed))
cat(sprintf("Failed:       %d ✗\n", failed))
cat(sprintf("Skipped:      %d\n\n", skipped))

# Detailed results
cat("Detailed Results:\n")
cat(strrep("-", 78), "\n")
for (name in names(results)) {
  res <- results[[name]]
  status_sym <- switch(res$status,
                       "PASS" = "✓",
                       "FAIL" = "✗",
                       "SKIP" = "⊘")

  cat(sprintf("\n%s %s\n", status_sym, name))

  if (res$status == "PASS") {
    cat(sprintf("  Max abs diff: %.2e (%s precision)\n",
                res$max_abs_diff, res$precision))
    cat(sprintf("  GP terms: %d (%s)\n",
                res$n_gp_terms,
                paste(res$gp_suffixes, collapse = ", ")))
  } else if (res$status == "FAIL") {
    cat(sprintf("  Reason: %s\n", res$reason))
    if (!is.null(res$error)) {
      cat(sprintf("  Error: %s\n", res$error))
    }
  } else if (res$status == "SKIP") {
    cat(sprintf("  Reason: %s\n", res$reason))
  }
}

cat("\n")
cat(strrep("=", 78), "\n")

if (failed == 0 && skipped == 0) {
  cat("✓ ALL TESTS PASSED\n")
  cat(strrep("=", 78), "\n\n")
  cat("GP prediction implementation validated successfully!\n\n")
} else if (failed > 0) {
  cat("✗ SOME TESTS FAILED\n")
  cat(strrep("=", 78), "\n\n")
  stop(sprintf("%d test(s) failed", failed))
} else {
  cat("⚠ SOME TESTS SKIPPED\n")
  cat(strrep("=", 78), "\n\n")
  cat("Some tests were skipped. Check fixture files.\n\n")
}
