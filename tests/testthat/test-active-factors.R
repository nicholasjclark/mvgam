active_factors_stub <- function(prob = 0.5) {
  structure(
    list(
      count = list(median = 4, q025 = 3, q975 = 6),
      per_factor = data.frame(
        factor          = 1:5,
        prob_active     = c(1, 1, 0.95, 0.6, 0.1),
        median_variance = c(10, 8, 6, 2, 0.1),
        median_share    = c(0.38, 0.31, 0.23, 0.08, 0.004),
        is_active       = c(TRUE, TRUE, TRUE, TRUE, FALSE)
      ),
      threshold = list(criterion = "relative", value = 0.1, prob = prob),
      n_lv = 5L
    ),
    class = "mvgam_active_factors"
  )
}

test_that("plot.mvgam_active_factors labels the count and its threshold", {
  p <- plot(active_factors_stub(prob = 0.8))
  expect_match(p$labels$title, "n_lv = 5")
  # The subtitle reports the median and interval of the count
  expect_match(p$labels$subtitle, "4.0  (95% CI 3.0, 6.0)", fixed = TRUE)
  # The reference line marks `prob`. A bar must clear it to count.
  hline <- vapply(p$layers, function(l) inherits(l$geom, "GeomHline"),
                  logical(1L))
  expect_equal(p$layers[[which(hline)]]$data$yintercept, 0.8)
})


test_that("the factor variances are the eigenvalues of Z Omega Z'", {
  set.seed(4L)
  ndraws <- 3L
  n_series <- 6L
  k <- 3L
  Z_arr <- array(rnorm(ndraws * n_series * k), c(ndraws, n_series, k))
  latent <- array(0, c(ndraws, k, k))
  for (d in seq_len(ndraws)) {
    latent[d, , ] <- crossprod(matrix(rnorm(k * k), k))
  }
  direct <- t(vapply(seq_len(ndraws), function(d) {
    cov <- Z_arr[d, , ] %*% latent[d, , ] %*% t(Z_arr[d, , ])
    eigen(cov, symmetric = TRUE, only.values = TRUE)$values[seq_len(k)]
  }, numeric(k)))
  out <- factor_eigenvalues(latent, Z_arr)
  expect_equal(out, direct, tolerance = 1e-10)

  # Rotating the factors leaves the variances unchanged. A rule on the
  # columns of `Z` would see different columns.
  R <- qr.Q(qr(matrix(rnorm(k * k), k)))
  Z_rot <- Z_arr
  latent_rot <- latent
  for (d in seq_len(ndraws)) {
    Z_rot[d, , ] <- Z_arr[d, , ] %*% R
    latent_rot[d, , ] <- t(R) %*% latent[d, , ] %*% R
  }
  expect_equal(factor_eigenvalues(latent_rot, Z_rot), out,
               tolerance = 1e-10)

  # More factors than series leave as many dimensions as series
  expect_identical(dim(factor_eigenvalues(latent, Z_arr[, 1:2, ,
                                                        drop = FALSE])),
                   c(ndraws, 2L))
})


test_that("each activity rule counts the dimensions it describes", {
  # Shares 0.6, 0.3, 0.07 and 0.03 in every draw
  variance <- matrix(c(6, 3, 0.7, 0.3), nrow = 2L, ncol = 4L,
                     byrow = TRUE)
  active <- function(...) active_factor_draws(variance, ...)[1L, ]
  expect_identical(active("relative", 0.1), c(TRUE, TRUE, TRUE, FALSE))
  expect_identical(active("relative", 0.5), c(TRUE, TRUE, FALSE, FALSE))
  expect_identical(active("cumulative", 0.95), c(TRUE, TRUE, TRUE, FALSE))
  # 0.6 + 0.3 reaches 0.9 exactly, despite floating point
  expect_identical(active("cumulative", 0.9), c(TRUE, TRUE, FALSE, FALSE))
  # One dimension reaches any cumulative share alone
  expect_true(all(active_factor_draws(variance[, 1L, drop = FALSE],
                                      "cumulative", 1)))
})


test_that("active_factors() refuses a single factor", {
  testthat::local_mocked_bindings(
    require_factor_n_lv = function(...) 1L, .package = "mvgam"
  )
  expect_error(
    active_factors(structure(list(), class = "mvgam")),
    "needs 'n_lv' >= 2", fixed = TRUE
  )
})
