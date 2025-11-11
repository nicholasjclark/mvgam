# Simple debug test
devtools::load_all()

cat("=== Simple Debug Test ===\n")

# Test the helper function
cat("Testing helper function...\n")
test_formula <- ~ RW()
result <- mvgam:::should_trend_formula_have_intercept(test_formula)
cat("Helper function result:", result, "\n")

cat("Test complete.\n")