# Debug script for hurdle-poisson Stan code generation issue
# Error: Identifier 'n' not in scope in likelihood section
#
# The error occurs at:
#   target += hurdle_poisson_log_lpmf(Y[n] | mu[n], hu);
# where 'n' is used outside its loop scope.

devtools::load_all()
library(brms)

# =============================================================================
# 1. Load the brms hurdle-poisson fixture
# =============================================================================
cat("=== Loading brms hurdle-poisson fixture ===\n\n")

brms_hp <- readRDS("tasks/fixtures/val_brms_hurdle_poisson_ar1.rds")
cat("brms model class:", paste(class(brms_hp), collapse = ", "), "\n")
cat("brms family:", brms_hp$family$family, "\n\n")

# Extract brms Stan code for reference
brms_stancode <- brms::stancode(brms_hp)
cat("brms Stan code (model block excerpt):\n")
cat("----------------------------------------\n")
# Extract model block
model_start <- regexpr("model \\{", brms_stancode)
model_section <- substr(brms_stancode, model_start, model_start + 1500)
cat(model_section, "\n\n")

# =============================================================================
# 2. Create test data for hurdle-poisson mvgam model
# =============================================================================
cat("=== Creating test data ===\n\n")

set.seed(123)
n_time <- 30
n_series <- 1

# Create hurdle Poisson data (zero-inflated counts)
test_data <- data.frame(
  time = rep(1:n_time, n_series),
  series = factor(rep(1:n_series, each = n_time)),
  x1 = rnorm(n_time * n_series)
)

# Generate hurdle Poisson response
# hu probability ~ 0.3, lambda ~ exp(1 + 0.5*x1)
hu_prob <- 0.3
lambda <- exp(1 + 0.5 * test_data$x1)
test_data$y <- ifelse(

  runif(n_time * n_series) < hu_prob,
  0,
  rpois(n_time * n_series, lambda)
)

cat("Test data dimensions:", nrow(test_data), "rows\n")
cat("Response summary:\n")
print(summary(test_data$y))
cat("Proportion zeros:", mean(test_data$y == 0), "\n\n")

# =============================================================================
# 3. Generate mvgam Stan code (skip validation to see full code)
# =============================================================================
cat("=== Generating mvgam stancode (validate = FALSE) ===\n\n")

mf <- mvgam_formula(y ~ x1, trend_formula = ~ AR(p = 1))

mvgam_code <- stancode(mf, data = test_data, family = hurdle_poisson(),
                       validate = FALSE)
cat("Stan code length:", nchar(mvgam_code), "characters\n\n")

# Save full code for inspection
writeLines(mvgam_code, "tasks/debug_mvgam_hurdle_stancode.txt")
cat("Full mvgam Stan code saved to: tasks/debug_mvgam_hurdle_stancode.txt\n\n")

# Show model block
cat("mvgam Stan code (model block):\n")
cat("----------------------------------------\n")
model_start <- regexpr("model \\{", mvgam_code)
gen_quant_start <- regexpr("generated quantities", mvgam_code)
model_block <- substr(mvgam_code, model_start, gen_quant_start - 1)
cat(model_block, "\n")

# =============================================================================
# 4. Compare brms model block
# =============================================================================
cat("\n=== brms model block ===\n")
cat("----------------------------------------\n")
brms_model_start <- regexpr("model \\{", brms_stancode)
brms_gen_quant_start <- regexpr("generated quantities", brms_stancode)
brms_model_block <- substr(brms_stancode, brms_model_start, brms_gen_quant_start - 1)
cat(brms_model_block, "\n")

# Save full brms code
writeLines(brms_stancode, "tasks/debug_brms_hurdle_stancode.txt")
cat("\nFull brms Stan code saved to: tasks/debug_brms_hurdle_stancode.txt\n")

# =============================================================================
# 5. Key difference analysis
# =============================================================================
cat("\n=== Key differences ===\n\n")

cat("brms likelihood pattern:\n")
cat("  for (n in 1:N) {\n")
cat("    target += hurdle_poisson_log_lpmf(Y[n] | mu[n], hu);\n")
cat("  }\n\n")

cat("mvgam issue: likelihood is OUTSIDE the loop:\n")
cat("  for (n in 1:N) {\n")
cat("  }  // empty loop\n")
cat("  // Likelihood calculations\n")
cat("    target += hurdle_poisson_log_lpmf(Y[n] | mu[n], hu);  // n not in scope!\n")
