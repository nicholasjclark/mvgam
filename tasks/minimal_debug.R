#!/usr/bin/env Rscript

# Minimal debugging script for Stan code generation
devtools::load_all()
library(brms)

# Create simple test data
set.seed(12345)
test_data <- data.frame(
  y = rpois(8, lambda = exp(1.2)),
  temperature = rnorm(8, mean = 20, sd = 2),
  time = 1:8,
  series = factor(rep("A", 8)),
  site = factor(rep(c("Site1", "Site2"), each = 4))
)

cat("Starting mvgam fitting with debug output...\n")

# Fit mvgam model to trigger Stan code generation
mvgam_model <- mvgam(
  formula = y ~ 1 + temperature + (1|site),
  trend_formula = ~ AR(p = 1),
  data = test_data,
  family = poisson(),
  chains = 1,
  iter = 50,
  refresh = 0,
  silent = 2,
  backend = "cmdstanr"
)

cat("mvgam fitting completed\n")
cat("Final Stan code check:\n")
cat("Contains 'mu +=':", grepl("mu \\+=", mvgam_model$stancode), "\n")
cat("Contains 'poisson_log_glm':", grepl("poisson_log_glm", mvgam_model$stancode), "\n")