# The tests of test-utils-logspace.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()

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
