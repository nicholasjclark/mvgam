# Log-scale arithmetic shared by the family kernels.

test_that("log_add_exp and log_diff_exp invert each other", {
  a <- c(-3, 0, 5, 710)
  b <- c(-4, -2, 1, 709)
  s <- log_add_exp(a, b)
  expect_equal(s[1:3], log(exp(a[1:3]) + exp(b[1:3])))
  # `exp(710)` overflows to Inf, and the naive sum with it.
  expect_equal(s[4], 710 + log1p(exp(-1)))
  expect_equal(log_diff_exp(s, b), a)
  # Two empty terms sum to an empty term, where the naive shift
  # returns NaN.
  expect_identical(log_add_exp(-Inf, -Inf), -Inf)
  expect_identical(log_add_exp(-Inf, 2), 2)
})

test_that("log1mexp is accurate on both sides of its branch", {
  a <- c(1e-10, 0.5, log(2), 3, 40)
  expect_equal(log1mexp(a), log(1 - exp(-a)), tolerance = 1e-8)
  expect_identical(log1mexp(0), -Inf)
})

test_that("log_sum_exp and log_mean_exp handle empty and infinite input", {
  x <- c(-1, 0.5, 2)
  expect_equal(log_sum_exp(x), log(sum(exp(x))))
  expect_equal(log_mean_exp(x), log(mean(exp(x))))
  expect_identical(log_sum_exp(numeric(0)), -Inf)
  expect_identical(log_sum_exp(c(-Inf, -Inf)), -Inf)
  # A term of 1000 overflows `exp()` directly.
  expect_equal(log_sum_exp(c(1000, 1000)), 1000 + log(2))
})

test_that("the row helpers agree with their one-vector forms", {
  m <- rbind(c(-1, 0.5, 2), c(800, 799, 0), c(-Inf, -Inf, -Inf))
  expect_identical(row_max(m), c(2, 800, -Inf))
  lse <- row_log_sum_exp(m)
  expect_equal(lse[1:2], c(log_sum_exp(m[1, ]), log_sum_exp(m[2, ])))
  expect_identical(lse[3], -Inf)
  p <- row_softmax(m[1:2, ])
  expect_equal(rowSums(p), c(1, 1))
  expect_equal(p[1, ], softmax(m[1, ]))
})

test_that("sample_row_index draws each column at its weight", {
  set.seed(5)
  probs <- c(0.1, 0.6, 0.3)
  lw <- matrix(log(probs), nrow = 20000L, ncol = 3L, byrow = TRUE)
  idx <- sample_row_index(lw)
  expect_true(all(idx %in% 1:3))
  expect_equal(as.numeric(table(idx)) / 20000, probs, tolerance = 0.02)
  # A row with one live column always returns it.
  one <- matrix(c(-Inf, 0, -Inf), nrow = 50L, ncol = 3L, byrow = TRUE)
  expect_identical(sample_row_index(one), rep(2L, 50L))
})

test_that("the COM-Poisson series matches its direct sum", {
  mu <- c(0.4, 1.2, 3)
  shape <- c(0.6, 1.5, 2)
  series <- com_poisson_log_series(log(mu), shape)
  expect_true(series$converged)
  k <- 0:200
  for (i in seq_along(mu)) {
    w <- exp(shape[i] * (k * log(mu[i]) - lfactorial(k)))
    expect_equal(series$log_z[i], log(sum(w)))
    expect_equal(series$log_moment[i], log(sum(k * w)))
  }
  # The mean is their ratio. At shape 1 the family is Poisson.
  expect_equal(mean_com_poisson(1.2, 1), 1.2)
  expect_equal(
    mean_com_poisson(1.2, 1.5, approx = FALSE),
    exp(series$log_moment[2] - series$log_z[2])
  )
})
