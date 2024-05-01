context("monotonic")

# Simulations are a bit time-consuming
skip_on_cran()

# Simulate data from a monotonically increasing function
set.seed(123123)
x <- runif(80) * 4 - 1
x <- sort(x)
f <- exp(4 * x) / (1 + exp(4 * x))
y <- f + rnorm(80) * 0.1
mod_data <- data.frame(y = y, x = x, z = rnorm(80),
                       time = 1:80)

test_that("k must be an even integer for s(bs = 'moi')", {
  expect_error(mvgam(y ~ s(x, bs = 'moi', k = 11),
                data = mod_data,
                family = gaussian()),
               "Argument 'k(bs = 'moi')'  must be an even integer",
               fixed = TRUE)

  expect_error(mvgam(y ~ s(x, bs = 'moi', k = 1),
                     data = mod_data,
                     family = gaussian()),
               "Basis dimension is too small",
               fixed = TRUE)
})

test_that("monotonic only works for one dimensional smooths", {
  expect_error(mvgam(y ~ s(x, z, bs = 'moi', k = 10),
                     data = mod_data,
                     family = gaussian()),
               "Monotonic basis only handles 1D smooths",
               fixed = TRUE)
})

test_that("monotonic for observation models working properly", {
  mod <- mvgam(y ~ z + s(x, bs = 'moi', k = 18),
               data = mod_data,
               family = gaussian(),
               run_model = FALSE)

  # Monotonic indices should be in the model_data
  expect_true("b_idx_s_x_" %in% names(mod$model_data))

  # The smooth should be an MOI class
  expect_true(inherits(mod$mgcv_model$smooth[[1]], 'moi.smooth'))

  # The coefficients should be fixed to be non-negative
  expect_true(any(grepl('b[b_idx_s_x_] = abs(b_raw[b_idx_s_x_]) * 1;',
                        mod$model_file, fixed = TRUE)))

  # Repeat a check for decreasing functions
  mod <- mvgam(y ~ z + s(x, bs = 'mod', k = 18),
               data = mod_data,
               family = gaussian(),
               run_model = FALSE)

  # The smooth should be an MOD class
  expect_true(inherits(mod$mgcv_model$smooth[[1]], 'mod.smooth'))

  # The coefficients should be fixed to be non-positive
  expect_true(any(grepl('b[b_idx_s_x_] = abs(b_raw[b_idx_s_x_]) * -1;',
                        mod$model_file, fixed = TRUE)))
})

test_that("monotonic for process models working properly", {
  mod <- mvgam(y ~ 0,
               trend_formula = ~ z + s(x, bs = 'moi', k = 18),
               trend_model = RW(),
               data = mod_data,
               family = gaussian(),
               run_model = FALSE)

  # Monotonic indices should be in the model_data
  expect_true("b_trend_idx_s_x_" %in% names(mod$model_data))

  # The smooth should be an MOI class
  expect_true(inherits(mod$trend_mgcv_model$smooth[[1]], 'moi.smooth'))

  # The coefficients should be fixed to be non-negative
  expect_true(any(grepl('b_trend[b_trend_idx_s_x_] = abs(b_raw_trend[b_trend_idx_s_x_]) * 1;',
                        mod$model_file, fixed = TRUE)))

  # And for decreasing
  mod <- mvgam(y ~ 0,
               trend_formula = ~ z + s(x, bs = 'mod', k = 18),
               trend_model = RW(),
               data = mod_data,
               family = gaussian(),
               run_model = FALSE)

  # The smooth should be an MOD class
  expect_true(inherits(mod$trend_mgcv_model$smooth[[1]], 'mod.smooth'))

  # The coefficients should be fixed to be non-positive
  expect_true(any(grepl('b_trend[b_trend_idx_s_x_] = abs(b_raw_trend[b_trend_idx_s_x_]) * -1;',
                        mod$model_file, fixed = TRUE)))
})
