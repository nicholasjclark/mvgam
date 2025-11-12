library(brms)

# Create simple model with random effects
dat <- data.frame(y = rnorm(20), x = rnorm(20), g = rep(1:4, 5))
bf_re <- bf(y ~ x + (1|g))
stancode <- make_stancode(bf_re, dat, family = gaussian())

# Print full stancode to see random effects pattern
cat("\n=== FULL STANCODE ===\n")
cat(stancode)
