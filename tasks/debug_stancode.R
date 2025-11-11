# Debug Stan Code Generation Bug
devtools::load_all()

# Simple test data
test_data <- data.frame(
  y = c(1, 2, 3, 4, 5, 6),
  x = rnorm(6),
  site = factor(c(1, 1, 1, 2, 2, 2)),
  time = 1:6,
  series = factor(c(1, 1, 1, 1, 1, 1))
)

cat("=== TESTING WORKING CASE (no trend) ===\n")
mf1 <- mvgam_formula(y ~ x + (1 | site))
code1 <- stancode(mf1, data = test_data, family = poisson(), validate = FALSE)
cat("SUCCESS: Generated", nchar(paste(code1, collapse="")), "characters\n")

cat("\n=== TESTING FAILING CASE (with trend) ===\n")
mf2 <- mvgam_formula(y ~ x + (1 | site), trend_formula = ~ RW())
tryCatch({
  code2 <- stancode(mf2, data = test_data, family = poisson(), validate = FALSE)
  cat("SUCCESS: Generated", nchar(paste(code2, collapse="")), "characters\n")
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  
  # Extract the problematic line from the error
  if (grepl("mu\\[n\\] \\+= mu \\+", e$message)) {
    cat("\nCRITICAL BUG IDENTIFIED:\n")
    cat("- Line: mu[n] += mu + trend[obs_trend_time[n], obs_trend_series[n]];\n") 
    cat("- Problem: mu[n] is scalar, 'mu' is vector name\n")
    cat("- Should be: mu[n] += Intercept + trend[...] (or similar)\n")
    cat("\nThis is in inject_trend_into_glm_predictor() function\n")
    cat("File: R/stan_assembly.R around line 897\n")
  }
})