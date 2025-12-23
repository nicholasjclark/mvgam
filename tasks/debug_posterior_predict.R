# Debug script for posterior_predict failures
# Run with: Rscript -e "devtools::load_all(); source('tasks/debug_posterior_predict.R')"

library(posterior)

cat("\n========== DEBUGGING POSTERIOR_PREDICT FAILURES ==========\n\n")

fixtures_dir <- "tasks/fixtures"

# 1. Check MVGAM model parameter naming (these are the ones failing)
cat("=== 1. PARAMETER NAMING IN MVGAM MODELS ===\n\n")

mvgam_models <- c(
  "val_mvgam_beta_ar1.rds",           # phi parameter
  "val_mvgam_hurdle_poisson_ar1.rds", # hu parameter
  "val_mvgam_hurdle_negbinomial_ar1.rds", # hu, shape parameters
  "val_mvgam_zero_inflated_poisson_ar1.rds", # zi parameter
  "val_mvgam_ar1_int.rds"             # Simple intercept-only (negative cor)
)

for (model_file in mvgam_models) {
  path <- file.path(fixtures_dir, model_file)
  if (!file.exists(path)) {
    cat("MISSING:", model_file, "\n")
    next
  }

  cat("\n--- ", model_file, " ---\n")
  fit <- readRDS(path)

  # Get all parameter names from the fit slot
  draws_mat <- as_draws_matrix(fit$fit)
  all_names <- colnames(draws_mat)

  # Show family
  cat("Family:", fit$family$family, "\n")
  cat("Class:", paste(class(fit), collapse = ", "), "\n")

  # Look for dpars with various patterns
  dpar_patterns <- c(
    "^phi$", "^phi\\[", "_phi$", "_phi\\[",
    "^shape$", "^shape\\[", "_shape$", "_shape\\[",
    "^zi$", "^zi\\[", "_zi$", "_zi\\[",
    "^hu$", "^hu\\[", "_hu$", "_hu\\[",
    "^sigma$", "^sigma\\[", "_sigma$", "_sigma\\["
  )

  cat("Dpar-related parameters found:\n")
  found_any <- FALSE
  for (pat in dpar_patterns) {
    matches <- grep(pat, all_names, value = TRUE)
    if (length(matches) > 0) {
      found_any <- TRUE
      cat("  Pattern '", pat, "': ", paste(head(matches, 3), collapse = ", "),
          if(length(matches) > 3) "..." else "", "\n", sep = "")
    }
  }
  if (!found_any) cat("  NONE FOUND!\n")

  # Show first 30 parameter names to see what's there
  cat("First 30 params:\n  ", paste(head(all_names, 30), collapse = ", "), "\n")
}

# 2. Test extract_dpars_from_stanfit on MVGAM models
cat("\n\n=== 2. TEST DPAR EXTRACTION ON MVGAM ===\n\n")

beta_path <- file.path(fixtures_dir, "val_mvgam_beta_ar1.rds")
if (file.exists(beta_path)) {
  fit <- readRDS(beta_path)
  cat("Testing dpar extraction on MVGAM Beta model:\n")
  cat("Family:", fit$family$family, "\n")

  dpars_needed <- get_family_dpars(fit$family$family)
  cat("Dpars needed:", paste(dpars_needed, collapse = ", "), "\n")

  result <- extract_dpars_from_stanfit(
    stanfit = fit$fit,
    dpar_names = dpars_needed,
    ndraws = 10,
    nobs = 30,
    draw_ids = NULL
  )

  cat("Extraction result:\n")
  for (nm in names(result)) {
    if (is.null(result[[nm]])) {
      cat("  ", nm, ": NULL <-- PROBLEM!\n", sep = "")
    } else {
      cat("  ", nm, ": ", nrow(result[[nm]]), " x ", ncol(result[[nm]]), "\n", sep = "")
    }
  }
}

# 3. Test posterior_predict on MVGAM intercept-only model
cat("\n\n=== 3. TEST POSTERIOR_PREDICT ON MVGAM SIMPLE MODEL ===\n\n")

int_path <- file.path(fixtures_dir, "val_mvgam_ar1_int.rds")
if (file.exists(int_path)) {
  fit <- readRDS(int_path)
  cat("Testing posterior_predict on MVGAM intercept-only:\n")
  cat("Family:", fit$family$family, "\n")
  cat("Data rows:", nrow(fit$data), "\n")

  linpred <- posterior_linpred(fit, ndraws = 5)
  cat("Linpred dims:", dim(linpred), "\n")

  cat("\nCalling posterior_predict with ndraws=5...\n")
  tryCatch({
    pp <- posterior_predict(fit, ndraws = 5)
    cat("SUCCESS! posterior_predict dims:", dim(pp), "\n")
  }, error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
  }, warning = function(w) {
    cat("WARNING:", conditionMessage(w), "\n")
  })
}

# 4. TRACE THE LINPRED ISSUE
cat("\n\n=== 4. TRACE LINPRED VALUES ===\n\n")

mvgam_path <- file.path(fixtures_dir, "val_mvgam_ar1_int.rds")
brms_path <- file.path(fixtures_dir, "val_brms_ar1_int.rds")

if (file.exists(mvgam_path) && file.exists(brms_path)) {
  mvgam_fit <- readRDS(mvgam_path)
  brms_fit <- readRDS(brms_path)

  ndraws <- 10

  # Get linpred from both
  cat("Getting posterior_linpred...\n")
  lp_mvgam <- posterior_linpred(mvgam_fit, ndraws = ndraws)
  lp_brms <- posterior_linpred(brms_fit, ndraws = ndraws)

  cat("Linpred dims - mvgam:", dim(lp_mvgam), " brms:", dim(lp_brms), "\n")

  cat("\nLinpred mean per obs (first 5):\n")
  cat("  mvgam:", round(colMeans(lp_mvgam)[1:5], 3), "\n")
  cat("  brms: ", round(colMeans(lp_brms)[1:5], 3), "\n")

  # Now test posterior_predict directly (matching validation script: incl_autocor = FALSE)
  cat("\n--- POSTERIOR_PREDICT comparison (incl_autocor = FALSE) ---\n")
  set.seed(123)
  pp_brms <- brms::posterior_predict(brms_fit, newdata = brms_fit$data,
                                      ndraws = ndraws, incl_autocor = FALSE)
  set.seed(123)
  pp_mvgam <- posterior_predict(mvgam_fit, newdata = mvgam_fit$data, ndraws = ndraws)

  cat("Dims - brms:", dim(pp_brms), " mvgam:", dim(pp_mvgam), "\n")

  # Mean prediction per observation
  mean_brms <- colMeans(pp_brms)
  mean_mvgam <- colMeans(pp_mvgam)

  cat("Mean predictions per obs (first 5):\n")
  cat("  brms: ", round(head(mean_brms, 5), 2), "\n")
  cat("  mvgam:", round(head(mean_mvgam, 5), 2), "\n")

  cat("Correlation of mean predictions:", round(cor(mean_brms, mean_mvgam), 4), "\n")

  # Check raw samples for first observation
  cat("\nRaw samples for obs 1 (first 5 draws):\n")
  cat("  brms: ", head(pp_brms[, 1], 5), "\n")
  cat("  mvgam:", head(pp_mvgam[, 1], 5), "\n")

  cat("\nexp(linpred) mean per obs (first 5):\n")
  cat("  mvgam:", round(colMeans(exp(lp_mvgam))[1:5], 2), "\n")
  cat("  brms: ", round(colMeans(exp(lp_brms))[1:5], 2), "\n")

  cat("\nObserved y (first 5):", head(mvgam_fit$data$y, 5), "\n")

  # Check if brms uses AR(1) error structure
  cat("\nModel structures:\n")
  cat("  brms formula:", deparse(brms_fit$formula$formula), "\n")
  cat("  mvgam formula:", deparse(mvgam_fit$formula$formula), "\n")

  # Check brms autocor
  cat("  brms autocor:", class(brms_fit$autocor), "\n")
  if (!is.null(brms_fit$autocor)) {
    cat("  brms autocor details:", capture.output(brms_fit$autocor), "\n")
  }
}

# 5. BINOMIAL INVESTIGATION - negative correlation in self-consistency
cat("\n\n=== 5. BINOMIAL INVESTIGATION ===\n\n")

binom_mvgam <- file.path(fixtures_dir, "val_mvgam_binom_ar1.rds")

if (file.exists(binom_mvgam)) {
  mvgam_fit <- readRDS(binom_mvgam)
  cat("--- Binomial model debug ---\n")
  cat("Family:", mvgam_fit$family$family, "\n")

  ndraws <- 100
  set.seed(123)

  # Get epred and predict
  epred <- posterior_epred(mvgam_fit, ndraws = ndraws)
  cat("epred dims:", dim(epred), "\n")
  cat("epred mean (first 5 obs):", round(colMeans(epred)[1:5], 4), "\n")
  cat("epred range:", round(range(epred), 4), "\n")

  set.seed(123)
  pp <- posterior_predict(mvgam_fit, ndraws = ndraws)
  cat("\npredict dims:", dim(pp), "\n")
  cat("predict mean (first 5 obs):", round(colMeans(pp)[1:5], 4), "\n")
  cat("predict range:", round(range(pp), 2), "\n")

  # Check correlation
  pred_mean <- colMeans(pp)
  epred_mean <- colMeans(epred)
  cat("\nCorrelation(predict_mean, epred_mean):", round(cor(pred_mean, epred_mean), 4), "\n")

  # Check if trials are being extracted correctly
  cat("\n--- Trials check ---\n")
  if ("trials" %in% names(mvgam_fit$data)) {
    cat("trials in data:", head(mvgam_fit$data$trials), "\n")
  } else {
    cat("No 'trials' column in data\n")
  }

  # Check the observed data
  cat("Observed y (first 5):", head(mvgam_fit$data$y, 5), "\n")

  # For binomial: epred should be count (prob * trials), predict should be integers
  cat("\nAre predictions integers?", all(pp == floor(pp)), "\n")

  # Check individual draws to see pattern
  cat("\nDraw 1 - epred vs predict (first 5 obs):\n")
  cat("  epred: ", round(epred[1, 1:5], 3), "\n")
  cat("  predict:", pp[1, 1:5], "\n")

  cat("\nDraw 2 - epred vs predict (first 5 obs):\n")
  cat("  epred: ", round(epred[2, 1:5], 3), "\n")
  cat("  predict:", pp[2, 1:5], "\n")
}

# 6. BETA SELF-CONSISTENCY (keep for reference)
cat("\n\n=== 6. BETA SELF-CONSISTENCY ===\n\n")

beta_mvgam <- file.path(fixtures_dir, "val_mvgam_beta_ar1.rds")

if (file.exists(beta_mvgam)) {
  mvgam_fit <- readRDS(beta_mvgam)
  cat("--- Beta model self-consistency ---\n")

  ndraws <- 500  # More draws for stable estimates
  set.seed(42)

  # Get our model's estimates
  epred <- posterior_epred(mvgam_fit, ndraws = ndraws)  # mu on response scale
  cat("epred (mu) dims:", dim(epred), "\n")
  cat("epred mean (first 5 obs):", round(colMeans(epred)[1:5], 4), "\n")

  # Get phi from stanfit
  draws_mat <- posterior::as_draws_matrix(mvgam_fit$fit)
  phi_draws <- as.numeric(draws_mat[1:ndraws, "phi"])
  cat("phi draws (first 5):", round(phi_draws[1:5], 3), "\n")

  # Manual sampling: Beta(mu*phi, (1-mu)*phi)
  set.seed(123)
  manual_samples <- matrix(NA, nrow = ndraws, ncol = ncol(epred))
  for (i in 1:ndraws) {
    mu_i <- epred[i, ]
    phi_i <- phi_draws[i]
    shape1 <- mu_i * phi_i
    shape2 <- (1 - mu_i) * phi_i
    manual_samples[i, ] <- rbeta(length(mu_i), shape1, shape2)
  }

  # Get posterior_predict output
  set.seed(123)
  pp <- posterior_predict(mvgam_fit, ndraws = ndraws)

  # Check moment conditions: For Beta, E[Y] = mu, Var[Y] = mu(1-mu)/(1+phi)
  cat("\nMoment-based self-consistency check:\n")

  # Expected moments from model parameters
  phi_mean <- mean(phi_draws)
  mu_mean <- colMeans(epred)
  expected_mean <- mu_mean
  expected_var <- mu_mean * (1 - mu_mean) / (1 + phi_mean)

  # Observed moments from posterior_predict
  pp_mean <- colMeans(pp)
  pp_var <- apply(pp, 2, var)

  cat("  Mean comparison (first 5 obs):\n")
  cat("    Expected (mu):  ", round(expected_mean[1:5], 4), "\n")
  cat("    Observed (pp):  ", round(pp_mean[1:5], 4), "\n")

  cat("  Variance comparison (first 5 obs):\n")
  cat("    Expected:       ", round(expected_var[1:5], 4), "\n")
  cat("    Observed (pp):  ", round(pp_var[1:5], 4), "\n")

  # Overall check: means should be highly correlated
  mean_cor <- cor(expected_mean, pp_mean)
  var_cor <- cor(expected_var, pp_var)
  cat("\n  Correlation of expected vs observed means:", round(mean_cor, 4), "\n")
  cat("  Correlation of expected vs observed vars: ", round(var_cor, 4), "\n")

  # Relative error in overall mean
  rel_mean_err <- abs(mean(pp_mean) - mean(expected_mean)) / mean(expected_mean)
  cat("  Relative error in overall mean:", round(rel_mean_err, 4), "\n")

  passed <- mean_cor > 0.95 && rel_mean_err < 0.1
  cat(sprintf("  SELF-CONSISTENCY: %s\n", if (passed) "PASSED" else "FAILED"))
}

cat("\n========== DEBUG COMPLETE ==========\n")
