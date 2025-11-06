# Fit and save test model fixtures for prediction system development
# This script sources the setup file and fits all 9 models from
# tests/local/test-models-single.R, saving each to tasks/fixtures/

# Source setup (contains helper functions and test context setup)
source("tests/local/setup_tests_local.R")

# Create fixtures directory if it doesn't exist
if (!dir.exists("tasks/fixtures")) {
  dir.create("tasks/fixtures", recursive = TRUE)
  cat("Created tasks/fixtures/ directory\n")
}

# Setup test data function (from test-models-single.R)
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
  time_presence <- rbinom(n_time, size = 1, prob = 0.7)
  time_x <- rnorm(n_time)
  time_habitat <- factor(
    sample(c("forest", "grassland"), n_time, replace = TRUE)
  )

  multivariate <- data.frame(
    time = rep(1:n_time, n_series),
    count = rpois(n_time * n_series, lambda = 4),
    biomass = rlnorm(n_time * n_series, meanlog = 1, sdlog = 0.5),
    presence = rep(time_presence, n_series),
    x = rep(time_x, n_series),
    habitat = rep(time_habitat, n_series)
  )

  list(
    univariate = univariate,
    multivariate = multivariate
  )
}

# Get test data
cat("Setting up test data...\n")
test_data <- setup_stan_test_data()

# Track total fitting time
total_start <- Sys.time()

# ==============================================================================
# FIT 1: Basic RW model
# ==============================================================================
cat("\n=== Fitting Model 1: Basic RW ===\n")
start_time <- Sys.time()

fit1 <- SW(SM(mvgam(
  y ~ x,
  trend_formula = ~ RW(),
  data = test_data$univariate,
  family = poisson(),
  chains = 2,
  iter = 500,
  silent = 2
)))

elapsed <- difftime(Sys.time(), start_time, units = "secs")
cat(sprintf("Fit 1 completed in %.1f seconds\n", elapsed))
print(fit1)
saveRDS(fit1, "tasks/fixtures/fit1.rds")
cat("Saved to tasks/fixtures/fit1.rds\n")

# ==============================================================================
# FIT 2: Multivariate shared RW model
# ==============================================================================
cat("\n=== Fitting Model 2: Multivariate shared RW ===\n")
start_time <- Sys.time()

fit2 <- SW(SM(mvgam(
  bf(mvbind(count, biomass) ~ x) + set_rescor(FALSE),
  trend_formula = ~ RW(cor = TRUE),
  data = test_data$multivariate,
  chains = 2,
  iter = 500,
  silent = 2
)))

elapsed <- difftime(Sys.time(), start_time, units = "secs")
cat(sprintf("Fit 2 completed in %.1f seconds\n", elapsed))
print(fit2)
saveRDS(fit2, "tasks/fixtures/fit2.rds")
cat("Saved to tasks/fixtures/fit2.rds\n")

# ==============================================================================
# FIT 3: VARMA with smooths
# ==============================================================================
cat("\n=== Fitting Model 3: VARMA with smooths ===\n")
start_time <- Sys.time()

fit3 <- SW(SM(mvgam(
  bf(mvbind(count, biomass) ~ s(x)) + set_rescor(FALSE),
  trend_formula = ~ presence + VAR(p = 2, ma = TRUE),
  data = test_data$multivariate,
  chains = 2,
  iter = 500,
  silent = 2
)))

elapsed <- difftime(Sys.time(), start_time, units = "secs")
cat(sprintf("Fit 3 completed in %.1f seconds\n", elapsed))
print(fit3)
saveRDS(fit3, "tasks/fixtures/fit3.rds")
cat("Saved to tasks/fixtures/fit3.rds\n")

# ==============================================================================
# FIT 4: Factor AR trends
# ==============================================================================
cat("\n=== Fitting Model 4: Factor AR ===\n")
start_time <- Sys.time()

fit4 <- SW(SM(mvgam(
  formula = bf(count ~ x, family = poisson()) +
    bf(presence ~ x, family = bernoulli()) +
    bf(biomass ~ x, family = Gamma(link = log)),
  trend_formula = ~ -1 + AR(p = 1, n_lv = 2, cor = TRUE),
  data = test_data$multivariate,
  chains = 2,
  iter = 500,
  silent = 2
)))

elapsed <- difftime(Sys.time(), start_time, units = "secs")
cat(sprintf("Fit 4 completed in %.1f seconds\n", elapsed))
print(fit4)
saveRDS(fit4, "tasks/fixtures/fit4.rds")
cat("Saved to tasks/fixtures/fit4.rds\n")

# ==============================================================================
# FIT 5: PW trends
# ==============================================================================
cat("\n=== Fitting Model 5: Piecewise trends ===\n")
start_time <- Sys.time()

fit5 <- SW(SM(mvgam(
  y ~ x,
  trend_formula = ~ PW(n_changepoints = 10),
  data = test_data$univariate,
  family = poisson(),
  chains = 2,
  iter = 500,
  silent = 2
)))

elapsed <- difftime(Sys.time(), start_time, units = "secs")
cat(sprintf("Fit 5 completed in %.1f seconds\n", elapsed))
print(fit5)
saveRDS(fit5, "tasks/fixtures/fit5.rds")
cat("Saved to tasks/fixtures/fit5.rds\n")

# ==============================================================================
# FIT 6: CAR trends with GP
# ==============================================================================
cat("\n=== Fitting Model 6: CAR with GP ===\n")
start_time <- Sys.time()

fit6 <- SW(SM(mvgam(
  y ~ (1 | series),
  trend_formula = ~ gp(x) + CAR(),
  data = test_data$univariate,
  family = poisson(),
  chains = 2,
  iter = 500,
  silent = 2
)))

elapsed <- difftime(Sys.time(), start_time, units = "secs")
cat(sprintf("Fit 6 completed in %.1f seconds\n", elapsed))
print(fit6)
saveRDS(fit6, "tasks/fixtures/fit6.rds")
cat("Saved to tasks/fixtures/fit6.rds\n")

# ==============================================================================
# FIT 7: CAR trends with monotonic
# ==============================================================================
cat("\n=== Fitting Model 7: CAR with monotonic ===\n")
start_time <- Sys.time()

# Add ordered factor to data
income_options <- c("below_20", "20_to_40", "40_to_100", "greater_100")
test_data$univariate$income <- factor(
  sample(income_options, nrow(test_data$univariate), TRUE),
  levels = income_options,
  ordered = TRUE
)

fit7 <- SW(SM(mvgam(
  y ~ (1 | series),
  trend_formula = ~ mo(income) + CAR(),
  data = test_data$univariate,
  family = poisson(),
  chains = 2,
  iter = 500,
  silent = 2
)))

elapsed <- difftime(Sys.time(), start_time, units = "secs")
cat(sprintf("Fit 7 completed in %.1f seconds\n", elapsed))
print(fit7)
saveRDS(fit7, "tasks/fixtures/fit7.rds")
cat("Saved to tasks/fixtures/fit7.rds\n")

# ==============================================================================
# FIT 8: Seasonal AR trends
# ==============================================================================
cat("\n=== Fitting Model 8: Seasonal AR ===\n")
start_time <- Sys.time()

fit8 <- SW(SM(mvgam(
  y ~ gp(x, k = 5),
  trend_formula = ~ -1 + gp(temperature, k = 6) + AR(p = c(1, 12)),
  data = test_data$univariate,
  family = poisson(),
  chains = 2,
  iter = 500,
  silent = 2
)))

elapsed <- difftime(Sys.time(), start_time, units = "secs")
cat(sprintf("Fit 8 completed in %.1f seconds\n", elapsed))
print(fit8)
saveRDS(fit8, "tasks/fixtures/fit8.rds")
cat("Saved to tasks/fixtures/fit8.rds\n")

# ==============================================================================
# FIT 9: Nonlinear with AR trends
# ==============================================================================
cat("\n=== Fitting Model 9: Nonlinear with AR ===\n")
start_time <- Sys.time()

prior9 <- prior(normal(1, 2), nlpar = "b1") +
  prior(normal(0, 2), nlpar = "b2")

fit9 <- SW(SM(mvgam(
  bf(y ~ b1 * exp(b2 * x), b1 + b2 ~ 1, nl = TRUE),
  trend_formula = ~ AR(),
  data = test_data$univariate,
  family = poisson(),
  prior = prior9,
  chains = 2,
  iter = 500,
  silent = 2
)))

elapsed <- difftime(Sys.time(), start_time, units = "secs")
cat(sprintf("Fit 9 completed in %.1f seconds\n", elapsed))
print(fit9)
saveRDS(fit9, "tasks/fixtures/fit9.rds")
cat("Saved to tasks/fixtures/fit9.rds\n")

# ==============================================================================
# Summary
# ==============================================================================
total_elapsed <- difftime(Sys.time(), total_start, units = "mins")
cat(sprintf("\n=== ALL MODELS FITTED ===\n"))
cat(sprintf("Total time: %.1f minutes\n", total_elapsed))

# Verify all files exist and report sizes
cat("\nFixture file sizes:\n")
for (i in 1:9) {
  file_path <- sprintf("tasks/fixtures/fit%d.rds", i)
  if (file.exists(file_path)) {
    size_mb <- file.size(file_path) / 1024^2
    cat(sprintf("  fit%d.rds: %.2f MB\n", i, size_mb))
  } else {
    cat(sprintf("  fit%d.rds: MISSING\n", i))
  }
}

cat("\nAll fixtures saved to tasks/fixtures/\n")
cat("Run readRDS('tasks/fixtures/fitN.rds') to load individual models\n")
