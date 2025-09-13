devtools::load_all()
library(brms)

# Setup test data function (copied exactly from test-stancode-standata.R)
setup_stan_test_data <- function() {
  set.seed(42)
  n_time <- 24
  n_series <- 3

  # Simple univariate dataset
  univariate <- data.frame(
    time = 1:n_time,
    series = factor(rep("series1", n_time)),
    y = rpois(n_time, lambda = 5),
    x = rnorm(n_time),
    x2 = rnorm(n_time),
    temperature = rnorm(n_time, mean = 15, sd = 3)
  )

  # Multivariate dataset with balanced design
  multivariate <- data.frame(
    time = rep(1:n_time, n_series),
    series = factor(rep(paste0("series", 1:n_series), each = n_time)),
    count = rpois(n_time * n_series, lambda = 4),
    biomass = rlnorm(n_time * n_series, meanlog = 1, sdlog = 0.5),
    presence = rbinom(n_time * n_series, size = 1, prob = 0.7),
    x = rnorm(n_time * n_series),
    habitat = factor(sample(c("forest", "grassland"), n_time * n_series, replace = TRUE))
  )

  # Dataset with missing values
  with_missings <- univariate
  with_missings$y[c(3, 7, 15)] <- NA

  list(
    univariate = univariate,
    multivariate = multivariate,
    with_missings = with_missings
  )
}

# Get test data
test_data <- setup_stan_test_data()

# Monotonic effects
income_options <- c("below_20", "20_to_40", "40_to_100", "greater_100")
income <- factor(sample(income_options, NROW(test_data$univariate), TRUE),
                 levels = income_options, ordered = TRUE)
test_data$univariate$income <- income
make_stancode(y ~ mo(x), data = test_data$univariate)

# One-dimensional gp effects
make_stancode(y ~ gp(x), data = test_data$univariate)

# Two-dimensional gp effects
make_stancode(y ~ gp(x, x2), data = test_data$univariate)

# Approximate one-dimensional gp effects
make_stancode(y ~ gp(x, k = 6), data = test_data$univariate)

# Approximate two-dimensional gp effects with groupings
make_stancode(biomass ~ gp(x, k = 4, cov = "matern32", by = series), data = test_data$multivariate)

# Spline effects with s()
make_stancode(y ~ s(x, k = 6), data = test_data$univariate)

# Spline effects with s() with groupings
make_stancode(biomass ~ s(x, k = 6, by = series), data = test_data$multivariate)

# Spline effects with t2()
make_stancode(y ~ t2(x, k = 6), data = test_data$univariate)

# Simple random effects
make_stancode(y ~ (1 | series), data = test_data$univariate)

# Correlated random effects
make_stancode(y ~ x + (x + (1 | 1 | series)), data = test_data$univariate)
