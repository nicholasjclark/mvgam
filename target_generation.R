# Target Stan Code Generation Script
#
# This script generates current Stan code for comparison against validated targets
# Run this script to see where the current implementation differs from targets
#
# Usage: source("target_generation.R")
# Output: Creates current_stancode_*.stan files for comparison

devtools::load_all()
library(brms)

# Setup test data function (copied exactly from test-stancode-standata.R)
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
  multivariate <- data.frame(
    time = rep(1:n_time, n_series),
    series = factor(rep(paste0("series", 1:n_series), each = n_time)),
    count = rpois(n_time * n_series, lambda = 4),
    biomass = rlnorm(n_time * n_series, meanlog = 1, sdlog = 0.5),
    presence = rbinom(n_time * n_series, size = 1, prob = 0.7),
    x = rnorm(n_time * n_series),
    habitat = factor(sample(c("forest", "grassland"), n_time * n_series, replace = TRUE))
  )

  # Dataset with missing values
  with_missings <- univariate
  with_missings$y[c(3, 7, 15)] <- NA

  list(
    univariate = univariate,
    multivariate = multivariate,
    with_missings = with_missings
  )
}

# Get test data
test_data <- setup_stan_test_data()

cat("Generating current Stan code for target comparison...\n")

# ============================================================================
# TARGET 1: RW trends (basic structure)
# ============================================================================
cat("Generating current_stancode_1.stan (RW trends)...\n")
tryCatch({
  mf1 <- mvgam_formula(y ~ x, trend_formula = ~ RW())
  code1 <- stancode(mf1, data = test_data$univariate, family = poisson(), validate = FALSE)
  writeLines(code1, 'tasks/current_stancode_1.stan')
  cat("✓ current_stancode_1.stan created\n")
}, error = function(e) {
  cat("✗ Error generating target 1:", conditionMessage(e), "\n")
})

# ============================================================================
# TARGET 2: Shared RW trends (multivariate)
# ============================================================================
cat("Generating current_stancode_2.stan (Shared RW trends)...\n")
tryCatch({
  mf2 <- mvgam_formula(
    bf(mvbind(count, biomass) ~ x) + set_rescor(FALSE),
    trend_formula = ~ RW(cor = TRUE)
  )
  code2 <- stancode(mf2, data = test_data$multivariate, validate = FALSE)
  writeLines(code2, 'tasks/current_stancode_2.stan')
  cat("✓ current_stancode_2.stan created\n")
}, error = function(e) {
  cat("✗ Error generating target 2:", conditionMessage(e), "\n")
})

# ============================================================================
# TARGET 3: VARMA trends (complex functions)
# ============================================================================
cat("Generating current_stancode_3.stan (VARMA trends)...\n")
tryCatch({
  mf3 <- mvgam_formula(
    bf(mvbind(count, biomass) ~ s(x)) + set_rescor(FALSE),
    trend_formula = ~ presence + VAR(p = 2, ma = TRUE)
  )
  code3 <- stancode(mf3, data = test_data$multivariate, validate = FALSE)
  writeLines(code3, 'tasks/current_stancode_3.stan')
  cat("✓ current_stancode_3.stan created\n")
}, error = function(e) {
  cat("✗ Error generating target 3:", conditionMessage(e), "\n")
})

# ============================================================================
# TARGET 4: Factor AR trends (Z matrix patterns)
# ============================================================================
cat("Generating current_stancode_4.stan (Factor AR trends)...\n")
tryCatch({

  mf4 <- mvgam_formula(
    formula = bf(count ~ x, family = poisson()) +
      bf(presence ~ x, family = bernoulli()) +
      bf(biomass ~ x, family = Gamma()),
    trend_formula = ~ -1 + AR(p = 1, n_lv = 2, cor = TRUE)
  )
  code4 <- stancode(mf4, data = test_data$multivariate, validate = FALSE)
  writeLines(code4, 'tasks/current_stancode_4.stan')
  cat("✓ current_stancode_4.stan created\n")
}, error = function(e) {
  cat("✗ Error generating target 4:", conditionMessage(e), "\n")
})

# ============================================================================
# TARGET 5: PW trends (Prophet functions)
# ============================================================================
cat("Generating current_stancode_5.stan (PW trends)...\n")
tryCatch({
  mf5 <- mvgam_formula(
    y ~ x,
    trend_formula = ~ PW(n_changepoints = 10)
  )
  code5 <- stancode(mf5, data = test_data$univariate, family = poisson(), validate = FALSE)
  writeLines(code5, 'tasks/current_stancode_5.stan')
  cat("✓ current_stancode_5.stan created\n")
}, error = function(e) {
  cat("✗ Error generating target 5:", conditionMessage(e), "\n")
})

# ============================================================================
# TARGET 6: CAR trends (GP + irregular time)
# ============================================================================
cat("Generating current_stancode_6.stan (CAR trends)...\n")
tryCatch({

  mf6 <- mvgam_formula(y ~ (1 | series), trend_formula = ~ gp(x) + CAR())
  code6 <- stancode(mf6, data = test_data$univariate, family = poisson(), validate = FALSE)
  writeLines(code6, 'tasks/current_stancode_6.stan')
  cat("✓ current_stancode_6.stan created\n")
}, error = function(e) {
  cat("✗ Error generating target 6:", conditionMessage(e), "\n")
})

# ============================================================================
# Summary and Comparison Instructions
# ============================================================================
cat("\n", strrep("=", 70), "\n")
cat("TARGET GENERATION COMPLETE\n")
cat(strrep("=", 70), "\n")

cat("\nGenerated files for comparison:\n")
generated_files <- paste0("tasks/current_stancode_", 1:6, ".stan")
for (file in generated_files) {
  if (file.exists(file)) {
    cat("✓", file, "\n")
  } else {
    cat("✗", file, "(failed to generate)\n")
  }
}

cat("\nCOMPARISON INSTRUCTIONS:\n")
cat("1. Compare each current_stancode_*.stan with tasks/target_stancode_*.stan\n")
cat("2. Use diff tools like:\n")
cat("   - diff current_stancode_3.stan tasks/target_stancode_3.stan\n")
cat("   - Or use VS Code/other editors with side-by-side comparison\n")
cat("3. Focus on structural differences:\n")
cat("   - Missing data block variables (obs_trend_time_*, obs_trend_series_*)\n")
cat("   - Template placeholders not replaced ({lags}, {response})\n")
cat("   - Duplicate parameter declarations\n")
cat("   - Incorrect mu_trend computation\n")
cat("   - Missing MA components for VARMA models\n")
cat("   - Factor model code in non-factor models\n")

cat("\nEXAMPLE COMPARISON:\n")
cat("# Quick check for major issues in target 3 (VARMA):\n")
cat("grep -n 'obs_trend_time_count' current_stancode_3.stan\n")
cat("grep -n '{lags}' current_stancode_3.stan\n")
cat("grep -n 'mu_trend +=' current_stancode_3.stan\n")
cat("grep -n 'D_raw_trend' current_stancode_3.stan\n")

cat("\nRUN VALIDATION:\n")
cat("# Check Stan syntax (optional):\n")
cat("library(rstan)\n")
cat("stanc('current_stancode_3.stan')  # Should compile without errors\n")

cat("\n", strrep("=", 70), "\n")
