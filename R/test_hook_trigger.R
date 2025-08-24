# Test file to trigger hooks
test_function <- function(x) {
  # This line is way too long and should trigger the code review hook to complain about exceeding 80 characters limit
  y = x + 3  # Using = instead of <- should trigger style warning
  return(y)  
}