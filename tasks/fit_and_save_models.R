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

tryCatch({
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
  
  # Print stancode
  cat("\n--- STANCODE for Fit 1 ---\n")
  cat(fit1$stancode)
  cat("\n--- END STANCODE ---\n\n")
  
  saveRDS(fit1, "tasks/fixtures/fit1.rds")
  cat("Saved to tasks/fixtures/fit1.rds\n")
}, error = function(e) {
  cat(sprintf("ERROR in Fit 1: %s\n", e$message))
  cat("Continuing with next model...\n")
})

# ==============================================================================
# FIT 2: Multivariate shared RW model
# ==============================================================================
cat("\n=== Fitting Model 2: Multivariate shared RW ===\n")
start_time <- Sys.time()

tryCatch({
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
  
  # Print stancode
  cat("\n--- STANCODE for Fit 2 ---\n")
  cat(fit2$stancode)
  cat("\n--- END STANCODE ---\n\n")
  
  saveRDS(fit2, "tasks/fixtures/fit2.rds")
  cat("Saved to tasks/fixtures/fit2.rds\n")
}, error = function(e) {
  cat(sprintf("ERROR in Fit 2: %s\n", e$message))
  cat("Continuing with next model...\n")
})

# ==============================================================================
# FIT 3: VARMA with smooths
# ==============================================================================
cat("\n=== Fitting Model 3: VARMA with smooths ===\n")
start_time <- Sys.time()

tryCatch({
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
  
  # Print stancode
  cat("\n--- STANCODE for Fit 3 ---\n")
  cat(fit3$stancode)
  cat("\n--- END STANCODE ---\n\n")
  
  saveRDS(fit3, "tasks/fixtures/fit3.rds")
  cat("Saved to tasks/fixtures/fit3.rds\n")
}, error = function(e) {
  cat(sprintf("ERROR in Fit 3: %s\n", e$message))
  cat("Continuing with next model...\n")
})

# ==============================================================================
# FIT 4: Factor AR trends
# ==============================================================================
cat("\n=== Fitting Model 4: Factor AR ===\n")
start_time <- Sys.time()

tryCatch({
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
  
  # Print stancode
  cat("\n--- STANCODE for Fit 4 ---\n")
  cat(fit4$stancode)
  cat("\n--- END STANCODE ---\n\n")
  
  saveRDS(fit4, "tasks/fixtures/fit4.rds")
  cat("Saved to tasks/fixtures/fit4.rds\n")
}, error = function(e) {
  cat(sprintf("ERROR in Fit 4: %s\n", e$message))
  cat("Continuing with next model...\n")
})

# ==============================================================================
# FIT 5: PW trends
# ==============================================================================
cat("\n=== Fitting Model 5: Piecewise trends ===\n")
start_time <- Sys.time()

tryCatch({
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
  
  # Print stancode
  cat("\n--- STANCODE for Fit 5 ---\n")
  cat(fit5$stancode)
  cat("\n--- END STANCODE ---\n\n")
  
  saveRDS(fit5, "tasks/fixtures/fit5.rds")
  cat("Saved to tasks/fixtures/fit5.rds\n")
}, error = function(e) {
  cat(sprintf("ERROR in Fit 5: %s\n", e$message))
  cat("Continuing with next model...\n")
})

# ==============================================================================
# FIT 6: CAR trends with GP
# ==============================================================================
cat("\n=== Fitting Model 6: CAR with GP ===\n")
start_time <- Sys.time()

tryCatch({
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
  
  # Print stancode
  cat("\n--- STANCODE for Fit 6 ---\n")
  cat(fit6$stancode)
  cat("\n--- END STANCODE ---\n\n")
  
  saveRDS(fit6, "tasks/fixtures/fit6.rds")
  cat("Saved to tasks/fixtures/fit6.rds\n")
}, error = function(e) {
  cat(sprintf("ERROR in Fit 6: %s\n", e$message))
  cat("Continuing with next model...\n")
})

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

tryCatch({
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
  
  # Print stancode
  cat("\n--- STANCODE for Fit 7 ---\n")
  cat(fit7$stancode)
  cat("\n--- END STANCODE ---\n\n")
  
  saveRDS(fit7, "tasks/fixtures/fit7.rds")
  cat("Saved to tasks/fixtures/fit7.rds\n")
}, error = function(e) {
  cat(sprintf("ERROR in Fit 7: %s\n", e$message))
  cat("Continuing with next model...\n")
})

# ==============================================================================
# FIT 8: Seasonal AR trends
# ==============================================================================
cat("\n=== Fitting Model 8: Seasonal AR ===\n")
start_time <- Sys.time()

tryCatch({
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
  
  # Print stancode
  cat("\n--- STANCODE for Fit 8 ---\n")
  cat(fit8$stancode)
  cat("\n--- END STANCODE ---\n\n")
  
  saveRDS(fit8, "tasks/fixtures/fit8.rds")
  cat("Saved to tasks/fixtures/fit8.rds\n")
}, error = function(e) {
  cat(sprintf("ERROR in Fit 8: %s\n", e$message))
  cat("Continuing with next model...\n")
})

# ==============================================================================
# FIT 9: Nonlinear with AR trends
# ==============================================================================
cat("\n=== Fitting Model 9: Nonlinear with AR ===\n")
start_time <- Sys.time()

prior9 <- prior(normal(1, 2), nlpar = "b1") +
  prior(normal(0, 2), nlpar = "b2")

tryCatch({
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
  
  # Print stancode
  cat("\n--- STANCODE for Fit 9 ---\n")
  cat(fit9$stancode)
  cat("\n--- END STANCODE ---\n\n")
  
  saveRDS(fit9, "tasks/fixtures/fit9.rds")
  cat("Saved to tasks/fixtures/fit9.rds\n")
}, error = function(e) {
  cat(sprintf("ERROR in Fit 9: %s\n", e$message))
  cat("Continuing with next model...\n")
})

# ==============================================================================
# FIT 10: Nested random effects in observation formula
# ==============================================================================
cat("\n=== Fitting Model 10: Nested RE in observation ===\n")
start_time <- Sys.time()

# Extend multivariate data with nested grouping structure
test_data$multivariate$site <- factor(rep(c("A", "B", "C"), each = nrow(test_data$multivariate)/3))
test_data$multivariate$plot <- factor(paste0(test_data$multivariate$site, "_", rep(1:2, length.out = nrow(test_data$multivariate))))

tryCatch({
  fit10 <- SW(SM(mvgam(
    bf(mvbind(count, biomass) ~ x + (1 | site) + (1 | plot)) + set_rescor(FALSE),
    trend_formula = ~ RW(cor = TRUE),
    data = test_data$multivariate,
    chains = 2,
    iter = 500,
    silent = 2
  )))
  
  elapsed <- difftime(Sys.time(), start_time, units = "secs")
  cat(sprintf("Fit 10 completed in %.1f seconds\n", elapsed))
  print(fit10)
  
  # Print stancode
  cat("\n--- STANCODE for Fit 10 ---\n")
  cat(fit10$stancode)
  cat("\n--- END STANCODE ---\n\n")
  
  saveRDS(fit10, "tasks/fixtures/fit10.rds")
  cat("Saved to tasks/fixtures/fit10.rds\n")
}, error = function(e) {
  cat(sprintf("ERROR in Fit 10: %s\n", e$message))
  cat("Continuing with next model...\n")
})

# ==============================================================================
# FIT 11: Correlated random effects in trend formula
# ==============================================================================
cat("\n=== Fitting Model 11: Correlated RE in trend ===\n")
start_time <- Sys.time()

# Add site variable to univariate data for RE in trend formula
test_data$univariate$site <- factor(rep(c("A", "B", "C"), each = nrow(test_data$univariate)/3))

tryCatch({
  fit11 <- SW(SM(mvgam(
    y ~ x,
    trend_formula = ~ x + (x | site) + AR(p = 1),
    data = test_data$univariate,
    family = poisson(),
    chains = 2,
    iter = 500,
    silent = 2
  )))
  
  elapsed <- difftime(Sys.time(), start_time, units = "secs")
  cat(sprintf("Fit 11 completed in %.1f seconds\n", elapsed))
  print(fit11)
  
  # Print stancode
  cat("\n--- STANCODE for Fit 11 ---\n")
  cat(fit11$stancode)
  cat("\n--- END STANCODE ---\n\n")
  
  saveRDS(fit11, "tasks/fixtures/fit11.rds")
  cat("Saved to tasks/fixtures/fit11.rds\n")
}, error = function(e) {
  cat(sprintf("ERROR in Fit 11: %s\n", e$message))
  cat("Continuing with next model...\n")
})

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

  # Print stancode
  cat("\n--- STANCODE for Fit 12 ---\n")
  cat(fit12$stancode)
  cat("\n--- END STANCODE ---\n\n")

  saveRDS(fit12, "tasks/fixtures/fit12.rds")
  cat("Saved to tasks/fixtures/fit12.rds\n")
}, error = function(e) {
  cat(sprintf("ERROR in Fit 12: %s\n", e$message))
  cat("Continuing with next model...\n")
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

  # Print stancode
  cat("\n--- STANCODE for Fit 13 ---\n")
  cat(fit13$stancode)
  cat("\n--- END STANCODE ---\n\n")

  saveRDS(fit13, "tasks/fixtures/fit13.rds")
  cat("Saved to tasks/fixtures/fit13.rds\n")
}, error = function(e) {
  cat(sprintf("ERROR in Fit 13: %s\n", e$message))
  cat("Continuing with next model...\n")
})

# ==============================================================================
# Summary
# ==============================================================================
total_elapsed <- difftime(Sys.time(), total_start, units = "mins")
cat(sprintf("\n=== ALL MODELS ATTEMPTED ===\n"))
cat(sprintf("Total time: %.1f minutes\n", total_elapsed))

# Verify all files exist and report sizes
cat("\nFixture file sizes:\n")
successful_models <- 0
failed_models <- 0

for (i in 1:13) {
  file_path <- sprintf("tasks/fixtures/fit%d.rds", i)
  if (file.exists(file_path)) {
    size_mb <- file.size(file_path) / 1024^2
    cat(sprintf("  fit%d.rds: %.2f MB\n", i, size_mb))
    successful_models <- successful_models + 1
  } else {
    cat(sprintf("  fit%d.rds: MISSING (likely failed)\n", i))
    failed_models <- failed_models + 1
  }
}

cat(sprintf("\nSuccessfully fitted: %d models\n", successful_models))
cat(sprintf("Failed: %d models\n", failed_models))

cat("\nAll available fixtures saved to tasks/fixtures/\n")
cat("Run readRDS('tasks/fixtures/fitN.rds') to load individual models\n")