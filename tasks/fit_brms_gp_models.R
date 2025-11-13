# Fit brms GP Models for Validation Testing
# Fits multiple brms models with different GP configurations
# Saves as .rds fixtures for repeated testing

devtools::load_all()

library(brms)

cat("\n")
cat(strrep("=", 78), "\n")
cat("FITTING BRMS GP MODELS FOR VALIDATION\n")
cat(strrep("=", 78), "\n\n")

# Create output directory if needed
if (!dir.exists("tasks/fixtures/brms_gp")) {
  dir.create("tasks/fixtures/brms_gp", recursive = TRUE)
}

# Create test datasets
set.seed(123)
n <- 40

# Dataset 1: Simple continuous predictor
data_simple <- data.frame(
  y = rnorm(n, mean = 5 + 0.5 * seq(0, 10, length.out = n), sd = 1),
  x = seq(0, 10, length.out = n)
)

# Dataset 2: With grouping factor (2 groups)
data_grouped <- data.frame(
  y = rnorm(n, mean = 5, sd = 2),
  x = seq(0, 10, length.out = n),
  group = rep(c("A", "B"), each = n/2)
)

# Fitting settings (fast but sufficient for testing)
fit_args <- list(
  backend = "cmdstanr",
  chains = 2,
  iter = 600,
  warmup = 300,
  refresh = 0,
  silent = 2
)

# Model 1: Simple GP with default kernel (exponentiated quadratic)
cat("[1/4] Fitting Model 1: Simple GP (gp(x, k=5))...\n")
cat("      Kernel: exponentiated quadratic (default)\n")
fit1 <- do.call(brm, c(
  list(
    formula = y ~ gp(x, k = 5),
    data = data_simple,
    family = gaussian()
  ),
  fit_args
))
saveRDS(fit1, "tasks/fixtures/brms_gp/fit1_simple_gp.rds")
cat("      ✓ Saved to tasks/fixtures/brms_gp/fit1_simple_gp.rds\n\n")

# Model 2: GP with grouping (by factor)
cat("[2/4] Fitting Model 2: GP with grouping (gp(x, by=group, k=5))...\n")
cat("      Tests response-specific GP terms\n")
fit2 <- do.call(brm, c(
  list(
    formula = y ~ gp(x, by = group, k = 5),
    data = data_grouped,
    family = gaussian()
  ),
  fit_args
))
saveRDS(fit2, "tasks/fixtures/brms_gp/fit2_grouped_gp.rds")
cat("      ✓ Saved to tasks/fixtures/brms_gp/fit2_grouped_gp.rds\n\n")

# Model 3: GP with higher k (more basis functions)
cat("[3/4] Fitting Model 3: GP with k=10 basis functions...\n")
cat("      Tests higher-dimensional approximation\n")
fit3 <- do.call(brm, c(
  list(
    formula = y ~ gp(x, k = 10),
    data = data_simple,
    family = gaussian()
  ),
  fit_args
))
saveRDS(fit3, "tasks/fixtures/brms_gp/fit3_gp_k10.rds")
cat("      ✓ Saved to tasks/fixtures/brms_gp/fit3_gp_k10.rds\n\n")

# Model 4: GP with continuous scale parameter
cat("[4/4] Fitting Model 4: GP with scale='c' (continuous scaling)...\n")
cat("      Tests continuous scale (alternative to categorical 'by')\n")
fit4 <- do.call(brm, c(
  list(
    formula = y ~ gp(x, scale = "c", k = 5),
    data = data_simple,
    family = gaussian()
  ),
  fit_args
))
saveRDS(fit4, "tasks/fixtures/brms_gp/fit4_gp_scale_c.rds")
cat("      ✓ Saved to tasks/fixtures/brms_gp/fit4_gp_scale_c.rds\n\n")

# Summary
cat(strrep("=", 78), "\n")
cat("FITTING COMPLETE\n")
cat(strrep("=", 78), "\n\n")

cat("Fitted models saved:\n")
cat("  1. fit1_simple_gp.rds    - Basic GP (k=5)\n")
cat("  2. fit2_grouped_gp.rds   - GP with by=group\n")
cat("  3. fit3_gp_k10.rds       - GP with k=10\n")
cat("  4. fit4_gp_scale_c.rds   - GP with scale='c'\n\n")

cat("Note: brms approximate GP uses Hilbert space approximation with\n")
cat("      exponentiated quadratic (squared exponential) kernel.\n")
cat("      Other kernel types (Matern, etc.) not supported in approximate GP.\n\n")

# Test that models load correctly
cat("Verification: Testing all models load...\n")
models <- list(
  fit1 = readRDS("tasks/fixtures/brms_gp/fit1_simple_gp.rds"),
  fit2 = readRDS("tasks/fixtures/brms_gp/fit2_grouped_gp.rds"),
  fit3 = readRDS("tasks/fixtures/brms_gp/fit3_gp_k10.rds"),
  fit4 = readRDS("tasks/fixtures/brms_gp/fit4_gp_scale_c.rds")
)

for (i in seq_along(models)) {
  cat(sprintf("  ✓ Model %d loads successfully\n", i))
}

cat("\nAll models ready for validation testing!\n\n")
