# Debug script to test trend term extraction
library(mvgam)

# Test simple cases
test_formula <- ~ s(time) + RW()
terms_char <- attr(terms(test_formula), "term.labels")
cat("Formula terms:", paste(terms_char, collapse = ", "), "\n")

# Test find_trend_terms function
found <- mvgam:::find_trend_terms(test_formula)
cat("Found trends:", paste(found, collapse = ", "), "\n")

# Test with complex terms
complex_terms <- c("AR(p = c(1, 12, 24), ma = TRUE, cor = FALSE)")
found_complex <- mvgam:::find_trend_terms(complex_terms)
cat("Complex found:", paste(found_complex, collapse = ", "), "\n")

# Test pattern
pattern <- mvgam:::mvgam_trend_pattern()
cat("Pattern:", pattern, "\n")

# Test individual matching
cat("AR matches:", grepl("\\bAR\\s*\\(", "AR(p = c(1, 12, 24), ma = TRUE, cor = FALSE)"), "\n")