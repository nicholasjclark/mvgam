# Helpers shared by test-priors.R and test-extra-priors.R.
# Tests for Prior Specification System
# =====================================

create_test_data <- function(n = 20, n_series = 1) {
  if (n_series == 1) {
    data.frame(
      y = rnorm(n),
      x = rnorm(n),
      x1 = rnorm(n),
      x2 = rnorm(n),
      time = 1:n,
      series = factor("A")
    )
  } else {
    data.frame(
      y = rnorm(n * n_series),
      x = rnorm(n * n_series),
      x1 = rnorm(n * n_series),
      x2 = rnorm(n * n_series),
      time = rep(1:n, n_series),
      series = factor(rep(LETTERS[1:n_series], each = n))
    )
  }
}

# ---- mvgam's injected defaults must not block user priors -----------

prior_test_data <- function() {
  set.seed(1)
  d <- expand.grid(time = 1:60, series = factor(paste0("s", 1:3)))
  d$y <- rpois(nrow(d), 5)
  d
}
