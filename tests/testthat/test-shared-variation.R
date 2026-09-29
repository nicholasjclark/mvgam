test_that("shared_variation() errors on non-factor mvgam fits", {
  fake_fit <- structure(
    list(
      trend_metadata = list(trend_type = "AR")
    ),
    class = c("mvgam", "brmsfit")
  )
  expect_error(
    shared_variation(fake_fit),
    "requires a latent-factor fit"
  )
})

test_that("shared_variation S3 dispatch routes through to .mvgam method", {
  expect_true("shared_variation" %in% ls("package:mvgam") ||
              exists("shared_variation"))
  expect_true(exists("shared_variation.mvgam"))
  expect_true(exists("print.mvgam_shared_variation"))
})

test_that("shared_variation returns Z Sigma Z', not Z Z'", {
  case <- mk_factor_obj()
  testthat::local_mocked_bindings(
    as_draws_matrix = function(x, ...) x,
    .package = "posterior"
  )
  sv <- shared_variation(case$obj)
  Sigma <- diag(case$sigma_target^2, nrow = ncol(case$Z_target))
  expected <- case$Z_target %*% Sigma %*% t(case$Z_target)
  # The latent scale belongs between the loadings: a column enters
  # the trend as `sigma_trend[k] * Z[, k]`, so dropping it weights
  # every column equally when the model does not.
  expect_equal(unname(sv$delta), expected, tolerance = 1e-8)
  # The two formulas disagree on this mock, so the assertion
  # above cannot pass by coincidence.
  expect_false(isTRUE(all.equal(
    expected, tcrossprod(case$Z_target)
  )))
  expect_identical(unname(rownames(sv$delta)), case$series_levels)
})

test_that("shared_variation's printed header names the returned matrix", {
  case <- mk_factor_obj()
  testthat::local_mocked_bindings(
    as_draws_matrix = function(x, ...) x,
    .package = "posterior"
  )
  out <- capture.output(print(shared_variation(case$obj)))
  expect_match(out[1L], "Z Sigma Z'", fixed = TRUE)
})
