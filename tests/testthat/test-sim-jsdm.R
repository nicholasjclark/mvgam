# Fixture-free tests for sim_jsdm(). These check that the simulator
# emits the structure it claims: each response cell belongs to the
# site and species the frame names, the environmental slopes show up
# in the data, and species loading alike co-vary. Recovery from a
# fitted posterior is checked in tests/local/.


test_that("the frame carries the columns jsdgam() takes", {
  s <- sim_jsdm(n_species = 4L, n_sites = 20L, n_lv = 2L, seed = 1L)
  expect_s3_class(s, "mvgam_sim_jsdm")
  expect_s3_class(s, "mvgam_sim")
  # `jsdgam()` defaults to `unit = time` and `species = series`.
  # These column names are the interface contract, and a frame
  # missing one of them needs reshaping before it can be fitted.
  expect_setequal(
    names(s$data_train),
    c("site", "env", "series", "y", "time")
  )
  expect_s3_class(s$data_train$series, "factor")
  expect_identical(levels(s$data_train$series), colnames(s$y_array))
  expect_identical(s$data_train$time, s$data_train$site)
})


test_that("each response cell belongs to its own site and species", {
  # `as.numeric()` unrolls the matrix column by column, and the frame
  # is then ordered by (time, series). A transpose, or a fold pairing
  # the wrong species with a site, leaves every dimension intact and
  # teaches a fit one species' counts under another's name.
  s <- sim_jsdm(n_species = 4L, n_sites = 12L, n_lv = 2L, seed = 31L)
  d <- s$data_train
  for (k in seq_len(4L)) {
    rows <- d[d$series == colnames(s$y_array)[k], ]
    rows <- rows[order(rows$time), ]
    expect_identical(as.numeric(rows$y), as.numeric(s$y_array[, k]))
    # The covariate belongs to the site. Every species at a site
    # meets the same value.
    expect_identical(rows$env, s$truth$env)
  }
})


test_that("the simulated environmental slopes show up in the data", {
  # A species given a larger slope has a larger empirical
  # association between `env` and its response. A simulator that
  # drew `env_slopes` and never applied them satisfies every shape
  # check and fails here.
  #
  # Measured over 8 seeds, this agreement stays above 0.82 from 50
  # sites upward and above 0.84 at 20. The floor keeps margin while
  # still excluding an unused covariate.
  s <- sim_jsdm(
    family = mvn(), n_species = 5L, n_sites = 60L, n_lv = 2L,
    seed = 101L, family_pars = list(Psi = 0.5)
  )
  emp <- vapply(seq_len(5L), function(k) {
    stats::cor(s$truth$env, s$y_array[, k])
  }, numeric(1L))
  expect_gt(stats::cor(s$truth$env_slopes, emp), 0.7)
})


test_that("species loading alike co-vary once env is removed", {
  # What the latent factors are for. Two species with similar
  # loadings share a latent score. Their responses co-vary beyond
  # what the covariate explains, and a simulator that drew the
  # loadings and left them out of the predictor passes every other
  # check in this file.
  #
  # `mvn()` is the family this is asserted on. Its per-species scale
  # is 0.5 where a gaussian falls to the response simulator's default
  # of 1, and at that noise level the agreement drops as low as
  # -0.44 across seeds. At this size and family the minimum over 8
  # seeds is 0.96, and 0.5 keeps a wide margin.
  n_sites <- 100L
  s <- sim_jsdm(
    family = mvn(), n_species = 5L, n_sites = n_sites, n_lv = 2L,
    seed = 102L, family_pars = list(Psi = 0.5)
  )
  env <- s$truth$env
  resid <- vapply(seq_len(5L), function(k) {
    stats::residuals(stats::lm(s$y_array[, k] ~ env))
  }, numeric(n_sites))
  emp <- stats::cor(resid)
  implied <- s$truth$residual_cor
  off <- upper.tri(implied)
  expect_gt(stats::cor(emp[off], implied[off]), 0.5)
})


test_that("the truth list is the parameterisation drawn from", {
  s <- sim_jsdm(n_species = 5L, n_sites = 15L, n_lv = 2L, seed = 2L)
  expect_named(s$truth$intercepts, paste0("sp_", 1:5))
  expect_named(s$truth$env_slopes, paste0("sp_", 1:5))
  # The residual covariance is what the loadings imply, and the
  # correlation is its standardised form. A family with no
  # per-species scale adds nothing to the diagonal.
  expect_equal(s$truth$residual_cov, tcrossprod(s$truth$loadings))
  expect_equal(
    s$truth$residual_cor,
    stats::cov2cor(s$truth$residual_cov + diag(1e-8, 5L))
  )
})


test_that("a seed repeats the draw and restores the caller's stream", {
  a <- sim_jsdm(n_species = 3L, n_sites = 10L, seed = 7L)
  b <- sim_jsdm(n_species = 3L, n_sites = 10L, seed = 7L)
  expect_identical(a$y_array, b$y_array)
  expect_identical(a$truth$loadings, b$truth$loadings)
  # `local_seed()` puts the caller's stream back. A seeded call does
  # not decide what the caller draws next.
  set.seed(99L)
  first <- stats::rnorm(1)
  set.seed(99L)
  invisible(sim_jsdm(n_species = 3L, n_sites = 10L, seed = 7L))
  expect_identical(stats::rnorm(1), first)
})


test_that("each composition family obeys its own unit constraint", {
  # The constraint is what separates these three families. Each
  # draws a site's species together, and what a site's values sum to
  # is the family's definition.
  diri_sim <- sim_jsdm(
    family = diri(), n_species = 4L, n_sites = 12L, seed = 3L
  )
  expect_equal(rowSums(diri_sim$y_array), rep(1, 12L))
  expect_true(all(diri_sim$y_array > 0))

  multi_sim <- sim_jsdm(
    family = multi(), n_species = 4L, n_sites = 12L, seed = 3L,
    family_pars = list(unit_total = 25L)
  )
  expect_equal(rowSums(multi_sim$y_array), rep(25, 12L))
  expect_true(is.integer(multi_sim$data_train$y))

  categ_sim <- sim_jsdm(
    family = categ(), n_species = 4L, n_sites = 12L, seed = 3L
  )
  # One species per site, recorded as a one-hot row.
  expect_equal(rowSums(categ_sim$y_array), rep(1, 12L))
  expect_true(all(categ_sim$y_array %in% c(0, 1)))
  expect_true(is.integer(categ_sim$data_train$y))
})


test_that("mvt draws the heavier tails its degrees of freedom give", {
  # Same seed, same loadings, and the residual law is the only
  # difference. A Student-t at nu = 4 reaches further from its centre
  # than a normal at the same scale. Measured, the widest draw is
  # 7.20 against 5.22 at this seed; the claim is the ordering.
  a <- sim_jsdm(
    family = mvn(), n_species = 5L, n_sites = 300L, n_lv = 2L,
    seed = 11L, family_pars = list(Psi = 0.5)
  )
  b <- sim_jsdm(
    family = mvt(), n_species = 5L, n_sites = 300L, n_lv = 2L,
    seed = 11L, family_pars = list(Psi = 0.5, nu = 4)
  )
  expect_equal(a$truth$loadings, b$truth$loadings)
  expect_gt(max(abs(b$y_array)), max(abs(a$y_array)))
  # The per-species scale enters the recorded covariance, and a
  # Student-t carries nu / (nu - 2) of it.
  expect_gt(
    min(diag(b$truth$residual_cov)),
    min(diag(a$truth$residual_cov))
  )
})


test_that("a composition family centres its loadings", {
  # A softmax is invariant to a shift shared by the species, and
  # mvgam's `sum_to_zero_vector` parameterisation covers the centred
  # subspace. The simulated truth is centred for those families and
  # left unchanged for the others.
  comp <- sim_jsdm(family = diri(), n_species = 4L, n_sites = 10L,
                   n_lv = 2L, seed = 6L)
  expect_equal(colMeans(comp$truth$loadings), rep(0, 2L))

  plain <- sim_jsdm(n_species = 4L, n_sites = 10L, n_lv = 2L,
                    seed = 6L)
  expect_false(isTRUE(all.equal(
    colMeans(plain$truth$loadings), rep(0, 2L)
  )))
})


test_that("the simulator requires a design it can draw from", {
  expect_error(sim_jsdm(n_species = 1L), "n_species")
  expect_error(sim_jsdm(n_sites = 1L), "n_sites")
  expect_error(sim_jsdm(n_lv = 0L), "n_lv")
})


test_that("summary reports the design and the drawn response", {
  s <- sim_jsdm(n_species = 4L, n_sites = 20L, n_lv = 2L, seed = 5L)
  sm <- summary(s)
  expect_s3_class(sm, "mvgam_sim_jsdm_summary")
  expect_identical(sm$family, "negbinomial")
  expect_identical(sm$n_rows, 80L)
  expect_equal(sm$response_range, range(s$y_array))
  # Dispatch reaches this family's own method. The `mvgam_sim`
  # method requires fields a JSDM object does not carry, and
  # falling through to it fails here.
  expect_equal(sm$intercepts, s$truth$intercepts)
  expect_equal(sm$cor_range, range(
    s$truth$residual_cor[upper.tri(s$truth$residual_cor)]
  ))
  # Anything passed through `...` is refused.
  expect_error(summary(s, zzz_unknown = 1))
  expect_output(print(sm), "sim_jsdm")
})
