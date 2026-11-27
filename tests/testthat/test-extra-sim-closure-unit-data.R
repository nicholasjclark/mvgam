# The tests of test-sim-closure-unit-data.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()

test_that("the simulated frame builds the family's own likelihood", {
  # No Stan fit. The long frame goes through the closure-unit
  # preparation, and the program scores it with the occupancy
  # likelihood over the unit arrays that preparation builds.
  s <- sim_closure_unit_data(
    type = 1L, family = occ(), n_sites = 30L, seed = 13L
  )
  code <- stancode(
    mvgam_formula(brms::bf(y ~ env, p ~ tod_c)),
    data = s$data_train, family = occ()
  )
  expect_match(
    code, "target += occ_lpmf(Y | mu, p, N_unit, n_rep, Y_max, visit_idx);",
    fixed = TRUE
  )
})
