# CI-safe tests for the trait_slopes wrapper helpers
# (R/trait_slopes.R). Full end-to-end fits live in tests/local/.

test_that("trait_slopes_nlpar_spec enumerates intercept + per-slope nlpars", {
  spec <- mvgam:::trait_slopes_nlpar_spec(y ~ env1 + env2)
  expect_true(spec$has_intercept)
  expect_equal(spec$slope_terms,  c("env1", "env2"))
  expect_equal(spec$slope_nlpars, c("b1", "b2"))
  expect_equal(spec$nlpars,       c("a", "b1", "b2"))
})

test_that("trait_slopes_nlpar_spec drops the intercept nlpar when absent", {
  spec <- mvgam:::trait_slopes_nlpar_spec(y ~ 0 + env)
  expect_false(spec$has_intercept)
  expect_equal(spec$nlpars, "b1")
})

test_that("trait_slopes_nlpar_spec accepts brmsformula input", {
  spec <- mvgam:::trait_slopes_nlpar_spec(brms::bf(y ~ env))
  expect_true(spec$has_intercept)
  expect_equal(spec$slope_nlpars, "b1")
})

test_that("build_trait_slopes_formula rewrites obs into nl bf with shared RE", {
  bf_out <- mvgam:::build_trait_slopes_formula(
    obs_formula  = y ~ env,
    trait_slopes = ~ trait1,
    species_var  = "series"
  )
  expect_s3_class(bf_out, "brmsformula")
  expect_true(isTRUE(attr(bf_out$formula, "nl")))
  # Top-level RHS contains the nlpar tokens, not the env covariate.
  top_chars <- paste(deparse(bf_out$formula), collapse = " ")
  expect_match(top_chars, "a \\+ b1 \\* env")
  # Both nlpar sub-formulas regress on trait1 + carry the shared
  # |sp|series RE tag.
  for (nm in c("a", "b1")) {
    sub <- paste(deparse(bf_out$pforms[[nm]]), collapse = " ")
    expect_match(sub, "trait1")
    expect_match(sub, "\\(1 \\| sp \\| series\\)")
  }
})

test_that("default_trait_slopes_priors covers every nlpar once", {
  pri <- mvgam:::default_trait_slopes_priors(y ~ env1 + env2)
  expect_s3_class(pri, "brmsprior")
  # Each of a/b1/b2 should have a normal(0,1) b prior + student_t sd.
  expect_setequal(
    unique(pri$nlpar),
    c("a", "b1", "b2")
  )
  expect_setequal(unique(pri$class), c("b", "sd"))
})

test_that("build_trait_slopes_formula errors on a formula with no fixed terms", {
  # `y ~ -1` produces no intercept and no slope_terms, so the
  # wrapper has nothing to attach trait_slopes to. The rewrite
  # must error early with a hint pointing the user back to
  # 'formula'.
  expect_error(
    mvgam:::build_trait_slopes_formula(
      obs_formula  = y ~ -1,
      trait_slopes = ~ trait1
    ),
    "no fixed terms"
  )
})

test_that("validate_trait_slopes rejects non-formula input", {
  dat <- data.frame(
    y = 1:3, env = 1:3, species = factor(letters[1:3]),
    trait1 = 1:3
  )
  expect_error(
    mvgam:::validate_trait_slopes(
      trait_slopes = "trait1",
      obs_formula  = y ~ env, data = dat,
      species_chr  = "species"
    ),
    "must be a one-sided formula"
  )
})

test_that("validate_trait_slopes rejects missing trait columns", {
  dat <- data.frame(
    y = 1:6, env = rnorm(6),
    species = factor(rep(letters[1:3], 2))
  )
  expect_error(
    mvgam:::validate_trait_slopes(
      trait_slopes = ~ trait_missing,
      obs_formula  = y ~ env, data = dat,
      species_chr  = "species"
    ),
    "Trait columns not found"
  )
})

test_that("validate_trait_slopes rejects traits that vary within species", {
  dat <- data.frame(
    y = 1:6, env = rnorm(6),
    species = factor(rep(letters[1:3], 2)),
    bad_trait = rnorm(6)  # varies within each species
  )
  expect_error(
    mvgam:::validate_trait_slopes(
      trait_slopes = ~ bad_trait,
      obs_formula  = y ~ env, data = dat,
      species_chr  = "species"
    ),
    "varies within species"
  )
})

test_that("validate_trait_slopes rejects top-level smooths in obs_formula", {
  # trait1 must be constant within species so it does not trip the
  # per-species constancy check before reaching the smooth gate:
  # species levels are c("a","b","c","a","b","c") so trait values
  # are c(1, 2, 3, 1, 2, 3).
  dat <- data.frame(
    y = 1:6, env = rnorm(6),
    species = factor(rep(letters[1:3], 2)),
    trait1 = c(1, 2, 3, 1, 2, 3)
  )
  expect_error(
    mvgam:::validate_trait_slopes(
      trait_slopes = ~ trait1,
      obs_formula  = y ~ s(env), data = dat,
      species_chr  = "species"
    ),
    "Smooth terms in 'formula' are not compatible"
  )
})
