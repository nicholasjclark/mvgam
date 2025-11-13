# Fit only the new models (fit12 and fit13)
# This avoids refitting all existing models

# Source setup (contains helper functions and test context setup)
source("tests/local/setup_tests_local.R")

# Create fixtures directory if it doesn't exist
if (!dir.exists("tasks/fixtures")) {
  dir.create("tasks/fixtures", recursive = TRUE)
  cat("Created tasks/fixtures/ directory\n")
}

# Setup test data function
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

  list(univariate = univariate)
}

# Get test data
cat("Setting up test data...\n")
test_data <- setup_stan_test_data()

# Track total fitting time
total_start <- Sys.time()

# ==============================================================================
# FIT 12: Model with offset term
# ==============================================================================
cat("\n=== Fitting Model 12: RW with offset ===\n")
start_time <- Sys.time()

# Add offset variable to univariate data
test_data$univariate$log_baseline <- log(runif(
  nrow(test_data$univariate),
  min = 3,
  max = 7
))

tryCatch({
  fit12 <- SW(SM(mvgam(
    y ~ x + offset(log_baseline),
    trend_formula = ~ RW(),
    data = test_data$univariate,
    family = poisson(),
    chains = 2,
    iter = 500,
    silent = 2
  )))

  elapsed <- difftime(Sys.time(), start_time, units = "secs")
  cat(sprintf("Fit 12 completed in %.1f seconds\n", elapsed))
  print(fit12)

  saveRDS(fit12, "tasks/fixtures/fit12.rds")
  cat("Saved to tasks/fixtures/fit12.rds\n")
}, error = function(e) {
  cat(sprintf("ERROR in Fit 12: %s\n", e$message))
})

# ==============================================================================
# FIT 13: Distributional regression (sigma ~ predictor)
# ==============================================================================
cat("\n=== Fitting Model 13: Distributional regression ===\n")
start_time <- Sys.time()

# Create data with heteroskedastic errors
set.seed(43)
n_dist <- 50
test_data$distributional <- data.frame(
  time = 1:n_dist,
  series = factor(rep("series1", n_dist)),
  y = rnorm(n_dist, mean = 10, sd = 1 + 0.5 * (1:n_dist) / n_dist),
  x = rnorm(n_dist),
  temperature = rnorm(n_dist, mean = 15, sd = 3)
)

tryCatch({
  fit13 <- SW(SM(mvgam(
    bf(y ~ s(x), sigma ~ s(temperature)),
    trend_formula = ~ RW(),
    data = test_data$distributional,
    family = gaussian(),
    chains = 2,
    iter = 500,
    silent = 2
  )))

  elapsed <- difftime(Sys.time(), start_time, units = "secs")
  cat(sprintf("Fit 13 completed in %.1f seconds\n", elapsed))
  print(fit13)

  saveRDS(fit13, "tasks/fixtures/fit13.rds")
  cat("Saved to tasks/fixtures/fit13.rds\n")
}, error = function(e) {
  cat(sprintf("ERROR in Fit 13: %s\n", e$message))
})

# ==============================================================================
# Summary
# ==============================================================================
total_elapsed <- difftime(Sys.time(), total_start, units = "mins")
cat(sprintf("\n=== NEW MODELS FITTING COMPLETE ===\n"))
cat(sprintf("Total time: %.1f minutes\n", total_elapsed))

# Verify files exist
cat("\nNew fixture files:\n")
for (i in 12:13) {
  file_path <- sprintf("tasks/fixtures/fit%d.rds", i)
  if (file.exists(file_path)) {
    size_mb <- file.size(file_path) / 1024^2
    cat(sprintf("  fit%d.rds: %.2f MB ✓\n", i, size_mb))
  } else {
    cat(sprintf("  fit%d.rds: MISSING ✗\n", i))
  }
}
