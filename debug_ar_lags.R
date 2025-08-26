# AR Lag Parameter Investigation
# ==============================
# Understanding how AR parameters should work for different lag specifications

devtools::load_all()

cat("=== AR LAG PARAMETER INVESTIGATION ===\n\n")

# Test different AR lag specifications
test_cases <- list(
  "AR(p=1)" = list(call = "AR(p = 1)", expected = c("ar1_trend")),
  "AR(p=2)" = list(call = "AR(p = 2)", expected = c("ar1_trend", "ar2_trend")), 
  "AR(c(1,3))" = list(call = "AR(p = c(1, 3))", expected = c("ar1_trend", "ar3_trend")),
  "AR(c(1,12,24))" = list(call = "AR(p = c(1, 12, 24))", expected = c("ar1_trend", "ar12_trend", "ar24_trend"))
)

for (test_name in names(test_cases)) {
  test_case <- test_cases[[test_name]]
  
  cat(sprintf("%s\n%s\n", test_name, paste(rep("-", nchar(test_name)), collapse = "")))
  
  # Parse the AR call to extract parameters
  if (test_name == "AR(p=1)") {
    ar_obj <- AR(p = 1)
  } else if (test_name == "AR(p=2)") {
    ar_obj <- AR(p = 2)
  } else if (test_name == "AR(c(1,3))") {
    ar_obj <- AR(p = c(1, 3))
  } else if (test_name == "AR(c(1,12,24))") {
    ar_obj <- AR(p = c(1, 12, 24))
  }
  
  cat("Constructor p value:", deparse(ar_obj$p), "\n")
  cat("Monitor params:", paste(ar_obj$monitor_params, collapse = ", "), "\n")
  cat("Expected:", paste(test_case$expected, collapse = ", "), "\n")
  
  missing <- setdiff(test_case$expected, ar_obj$monitor_params)
  extra <- setdiff(ar_obj$monitor_params[!ar_obj$monitor_params %in% "sigma_trend"], test_case$expected)
  
  if (length(missing) > 0) {
    cat("❌ Missing:", paste(missing, collapse = ", "), "\n")
  } else {
    cat("✅ All expected parameters present\n")
  }
  
  if (length(extra) > 0) {
    cat("⚠️  Extra:", paste(extra, collapse = ", "), "\n")
  }
  
  cat("\n")
}

# Now let's trace exactly what generate_ar_monitor_params does
cat("=== TRACING generate_ar_monitor_params FUNCTION ===\n")

# Test the function directly with different inputs
test_specs <- list(
  list(trend = "AR", p = 1),
  list(trend = "AR", p = 2), 
  list(trend = "AR", p = c(1, 3)),
  list(trend = "AR", p = c(1, 12, 24))
)

for (i in seq_along(test_specs)) {
  spec <- test_specs[[i]]
  cat(sprintf("Test %d: p = %s\n", i, deparse(spec$p)))
  
  # Trace the function logic step by step
  lags <- spec$p %||% spec$lags %||% 1
  cat("  lags after extraction:", deparse(lags), "\n")
  
  if (is.list(lags)) lags <- unlist(lags)
  cat("  lags after unlist:", deparse(lags), "\n")
  
  ar_params <- paste0("ar", lags, "_trend")
  cat("  Generated ar_params:", paste(ar_params, collapse = ", "), "\n")
  
  # Now call the actual function
  actual_result <- mvgam:::generate_ar_monitor_params(spec)
  cat("  Actual function result:", paste(actual_result, collapse = ", "), "\n")
  
  cat("\n")
}

cat("=== CONCLUSIONS ===\n")
cat("This will show if the generate_ar_monitor_params function is working correctly\n")
cat("or if there's a bug in how p=2 is being interpreted.\n")