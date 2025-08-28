# Script to capture the malformed Stan code before validation
library(testthat)
library(rstan)

# Load the package
devtools::load_all()

# Create test data
set.seed(42)
n_time <- 24
data <- data.frame(
  time = 1:n_time,
  series = factor(rep("series1", n_time)),
  y = rpois(n_time, lambda = 5),
  x = rnorm(n_time),
  temperature = rnorm(n_time, mean = 15, sd = 3)
)

# Monkey-patch the stanc function to capture the code before validation
bad_stan_code <- NULL

# Override the function temporarily
original_stanc <- rstan::stanc
assignInNamespace("stanc", function(model_code = "", verbose = FALSE, ...) {
  cat("=== CAPTURING MALFORMED STAN CODE ===\n")
  bad_stan_code <<- model_code
  cat("Stan code length:", nchar(model_code), "\n")
  
  # Now call original to get the error
  tryCatch({
    original_stanc(model_code = model_code, verbose = verbose, ...)
  }, error = function(e) {
    cat("Error caught, writing full Stan code to file and console:\n")
    writeLines(model_code, "malformed_stan_code.stan")
    cat("=====================================\n")
    cat(model_code)
    cat("\n=====================================\n")
    stop(e)
  })
}, ns = "rstan")

# Now run the failing code
mf_with_trend <- mvgam_formula(y ~ x, trend_formula = ~ RW())

tryCatch({
  code_with_trend <- stancode(mf_with_trend, data = data, family = poisson())
}, error = function(e) {
  cat("Final error:", e$message, "\n")
})

# Restore original function
assignInNamespace("stanc", original_stanc, ns = "rstan")