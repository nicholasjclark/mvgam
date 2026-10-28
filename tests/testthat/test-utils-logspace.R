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

test_that("the COM-Poisson normaliser and mean match a brute-force sum", {
  # Direct sums cover dispersion above and below a Poisson's. The
  # asymptotic region is covered at mu = 300, shape = 1.7. At mu = 2,
  # shape = 30 an asymptotic normaliser gave a mass function summing
  # to 0.48.
  mu <- c(0.3, 9.5, 2, 300)
  shape <- c(0.2, 0.5, 30, 1.7)
  k <- 0:12000
  for (i in seq_along(mu)) {
    lw <- shape[i] * (k * log(mu[i]) - lfactorial(k))
    log_z <- log_sum_exp(lw)
    out <- com_poisson_normaliser(mu[i], shape[i])
    expect_equal(out$log_z, log_z, tolerance = 1e-10)
    expect_equal(out$mean, sum(k * exp(lw - log_z)), tolerance = 1e-10)
  }
  expect_true(com_poisson_use_gaunt(log(300), 1.7))
  expect_identical(mean_com_poisson(1.2, 1), 1.2)
  expect_error(mean_com_poisson(3, 0), "finite positive 'mu' and 'shape'")
})


test_that("the COM-Poisson mass, distribution and sampler agree", {
  mu <- 9.5
  shape <- 0.5
  k <- 0:400
  pmf <- dcom_poisson_mvgam(k, mu, shape)
  expect_equal(sum(pmf), 1, tolerance = 1e-12)
  expect_equal(pcom_poisson_mvgam(k, mu, shape), cumsum(pmf),
               tolerance = 1e-12)
  # The upper tail is summed on its own, accurate where 1 - F(k) is not
  far <- pcom_poisson_mvgam(150, mu, shape, lower.tail = FALSE, log.p = TRUE)
  expect_equal(far, log(sum(pmf[k > 150])), tolerance = 1e-10)
  expect_identical(pcom_poisson_mvgam(c(-1, Inf), mu, shape), c(0, 1))
  expect_identical(dcom_poisson_mvgam(c(-1, 2.5), mu, shape), c(0, 0))
  set.seed(3)
  draws <- rcom_poisson_mvgam(20000L, mu, shape)
  expect_lt(max(abs(tabulate(draws + 1, length(k)) / 20000 - pmf)), 0.01)
})
