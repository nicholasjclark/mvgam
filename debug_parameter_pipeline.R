# ULTRA-SYSTEMATIC Parameter Pipeline Investigation
# =================================================
# Traces the complete pipeline from trend constructor to final prior parameters

# Load development functions
devtools::load_all()

cat("=== ULTRA-SYSTEMATIC PARAMETER PIPELINE INVESTIGATION ===\n\n")

# Setup test data
test_data <- data.frame(
  y = rpois(40, 5), 
  x = rnorm(40),
  time = rep(1:20, 2),
  series = factor(rep(c("A", "B"), each = 20))
)

# INVESTIGATION 1: AR(p=2) Complete Pipeline Trace
cat("1. AR(p=2) COMPLETE PIPELINE TRACE\n")
cat("==================================\n")

# Step 1A: Check if AR constructor works
cat("Step 1A: AR Constructor\n")
cat("-----------------------\n")
tryCatch({
  ar_obj <- AR(p = 2)
  cat("✓ AR(p=2) constructor succeeded\n")
  cat("  trend type:", ar_obj$trend, "\n")
  cat("  p parameter:", ar_obj$p, "\n")
  cat("  has monitor_params?", !is.null(ar_obj$monitor_params), "\n")
  if (!is.null(ar_obj$monitor_params)) {
    cat("  monitor_params:", paste(ar_obj$monitor_params, collapse = ", "), "\n")
  }
}, error = function(e) {
  cat("✗ AR constructor failed:", e$message, "\n")
})

# Step 1B: Check monitor_params generation manually
cat("\nStep 1B: Monitor Params Generation\n")
cat("----------------------------------\n")
tryCatch({
  # Try to access the monitor params generation function
  ar_spec <- list(trend = "AR", p = 2, cor = FALSE)
  monitor_params <- mvgam:::generate_monitor_params(ar_spec)
  cat("✓ Monitor params generation succeeded\n")
  cat("  Generated params:", paste(monitor_params, collapse = ", "), "\n")
}, error = function(e) {
  cat("✗ Monitor params generation failed:", e$message, "\n")
})

# Step 1C: Check full get_prior pipeline
cat("\nStep 1C: Full get_prior Pipeline\n")
cat("--------------------------------\n")
tryCatch({
  mf_ar2 <- mvgam_formula(y ~ x, trend_formula = ~ AR(p = 2))
  cat("✓ mvgam_formula construction succeeded\n")
  
  priors_ar2 <- get_prior(mf_ar2, data = test_data, family = poisson())
  cat("✓ get_prior succeeded\n")
  
  trend_priors <- priors_ar2[priors_ar2$trend_component == "trend", ]
  actual_classes <- sort(unique(trend_priors$class))
  cat("  Final trend parameters:", paste(actual_classes, collapse = ", "), "\n")
  
  # Check specific expectations
  expected <- c("ar1_trend", "ar2_trend", "sigma_trend") 
  missing <- setdiff(expected, actual_classes)
  extra <- setdiff(actual_classes, expected)
  cat("  Missing from expected:", paste(missing, collapse = ", "), "\n")
  cat("  Extra beyond expected:", paste(extra, collapse = ", "), "\n")
  
}, error = function(e) {
  cat("✗ get_prior pipeline failed:", e$message, "\n")
})

cat("\n" , rep("=", 60), "\n\n")

# INVESTIGATION 2: CAR Pipeline Analysis  
cat("2. CAR COMPLETE PIPELINE TRACE\n")
cat("==============================\n")

test_data_uv <- data.frame(y = rpois(20, 5), x = rnorm(20), time = 1:20, series = factor("A"))

# Step 2A: CAR Constructor
cat("Step 2A: CAR Constructor\n")
cat("-----------------------\n")
tryCatch({
  car_obj <- CAR()
  cat("✓ CAR() constructor succeeded\n")
  cat("  trend type:", car_obj$trend, "\n")
  cat("  has monitor_params?", !is.null(car_obj$monitor_params), "\n")
  if (!is.null(car_obj$monitor_params)) {
    cat("  monitor_params:", paste(car_obj$monitor_params, collapse = ", "), "\n")
  }
}, error = function(e) {
  cat("✗ CAR constructor failed:", e$message, "\n")
})

# Step 2B: CAR Monitor Params  
cat("\nStep 2B: CAR Monitor Params Generation\n")
cat("-------------------------------------\n")
tryCatch({
  car_spec <- list(trend = "CAR", p = 1, cor = FALSE)
  monitor_params_car <- mvgam:::generate_monitor_params(car_spec)
  cat("✓ CAR monitor params generation succeeded\n")
  cat("  Generated params:", paste(monitor_params_car, collapse = ", "), "\n")
  
  # Check if base params are being added
  base_params <- c("sigma_trend")
  car_specific <- mvgam:::generate_car_monitor_params(car_spec)
  cat("  Base params (sigma_trend):", paste(base_params, collapse = ", "), "\n") 
  cat("  CAR specific params:", paste(car_specific, collapse = ", "), "\n")
  cat("  Combined should be:", paste(unique(c(base_params, car_specific)), collapse = ", "), "\n")
}, error = function(e) {
  cat("✗ CAR monitor params failed:", e$message, "\n")
})

# Step 2C: CAR Full Pipeline
cat("\nStep 2C: CAR Full get_prior Pipeline\n")
cat("-----------------------------------\n")
tryCatch({
  mf_car <- mvgam_formula(y ~ x, trend_formula = ~ CAR())
  priors_car <- get_prior(mf_car, data = test_data_uv, family = poisson())
  trend_priors_car <- priors_car[priors_car$trend_component == "trend", ]
  actual_car <- sort(unique(trend_priors_car$class))
  cat("  Final CAR parameters:", paste(actual_car, collapse = ", "), "\n")
  
  # Analysis: Should CAR have sigma_trend?
  # Based on the monitor_params code, CAR SHOULD get sigma_trend (base param) + ar1
  expected_car <- c("ar1", "sigma_trend")
  cat("  Expected based on code analysis:", paste(expected_car, collapse = ", "), "\n")
  cat("  My test expected (NO sigma_trend):", "ar1", "\n")
  cat("  → Test expectation appears WRONG\n")
  
}, error = function(e) {
  cat("✗ CAR get_prior failed:", e$message, "\n") 
})

cat("\n" , rep("=", 60), "\n\n")

# INVESTIGATION 3: Deep Dive into monitor_params vs prior generation
cat("3. MONITOR_PARAMS VS PRIOR GENERATION ANALYSIS\n")
cat("==============================================\n")

# Check if there's a difference between what monitor_params generates
# and what actually gets turned into priors
tryCatch({
  cat("Testing ZMVN to understand the disconnect...\n")
  
  # Direct monitor_params generation
  zmvn_spec <- list(trend = "ZMVN", cor = FALSE)
  zmvn_monitor <- mvgam:::generate_monitor_params(zmvn_spec)
  cat("ZMVN monitor_params:", paste(zmvn_monitor, collapse = ", "), "\n")
  
  # Actual prior generation
  mf_zmvn <- mvgam_formula(y ~ x, trend_formula = ~ ZMVN())
  priors_zmvn <- get_prior(mf_zmvn, data = test_data, family = poisson())
  trend_priors_zmvn <- priors_zmvn[priors_zmvn$trend_component == "trend", ]
  actual_zmvn <- sort(unique(trend_priors_zmvn$class))
  cat("ZMVN actual priors:", paste(actual_zmvn, collapse = ", "), "\n")
  
  # Analysis
  if (length(zmvn_monitor) != length(actual_zmvn) || !all(sort(zmvn_monitor) == sort(actual_zmvn))) {
    cat("🚨 DISCREPANCY FOUND between monitor_params and actual priors!\n")
    cat("  This suggests the issue is in the prior generation process\n")
  } else {
    cat("✓ monitor_params matches actual priors for ZMVN\n")
  }
  
}, error = function(e) {
  cat("✗ ZMVN analysis failed:", e$message, "\n")
})

cat("\n=== INVESTIGATION CONCLUSIONS ===\n")
cat("This investigation will reveal:\n")
cat("1. Which step in the pipeline is causing discrepancies\n") 
cat("2. Whether my test expectations are wrong\n")
cat("3. Whether there are bugs in parameter generation\n")
cat("4. The root cause of each failing test\n")