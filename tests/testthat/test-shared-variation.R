test_that("shared_variation() errors on non-factor mvgam fits", {
  fake_fit <- structure(
    list(
      trend_components = list(
        types = "AR",
        specs = list(list(n_lv = NULL))
      ),
      trend_metadata = list()
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

test_that("match_loadings_prior_pars matches expected names only", {
  pars <- c(
    "theta_features[1]", "theta_features[2]",
    "theta_dist_phylo", "theta_dist_cluster",
    "Psi_diag[1]", "Psi_diag[2]",
    "varrho_inv[1]", "varrho_inv[2]",
    "n_features", "Z_tilde[1,1]", "row_features[1,1]",
    "ar1_trend[1]", "sigma_trend[1]"
  )
  hits <- match_loadings_prior_pars(pars)
  expect_true(all(hits[1:6]))
  expect_false(any(hits[7:13]))
})

test_that("match_loadings_prior_pars returns logical(0) on empty input", {
  expect_equal(match_loadings_prior_pars(character(0)), logical(0))
})

test_that("match_loadings_prior_pars rejects non-character input", {
  expect_error(match_loadings_prior_pars(1:5))
})
