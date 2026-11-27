# The tests of test-occ-family.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()

# ------------------------------------------------------------
# Stancode + standata emission via the mvgam_formula pipeline
# ------------------------------------------------------------

test_that(
  "stancode() for occ emits the occ_lpmf signature and omits K_max", {
  d <- make_occ_data()
  mf <- mvgam_formula(bf(y ~ elev, p ~ tod))
  sc <- as.character(stancode(mf, data = d, family = occ(), validate = FALSE))
  sd <- standata(mf, data = d, family = occ())
  # The vector density and the overload brms calls for a scalar p
  expect_match(
    sc, "real occ_lpmf\\(\\s*array\\[\\] int y,\\s*vector mu,\\s*vector p,"
  )
  expect_match(sc, "rep_vector\\(p, N\\)")
  # target += occ_lpmf(Y | mu, p, N_unit, n_rep, Y_max, visit_idx)
  expect_match(sc, "occ_lpmf\\(Y \\| mu, p, N_unit, n_rep, Y_max, visit_idx\\)")
  # K_max is absent for occ (binary latent state, no truncation).
  expect_false("K_max" %in% names(sd))
  expect_true(all(c("N_unit", "n_rep", "Y_max", "visit_idx") %in% names(sd)))
  expect_true(all(sd$Y_max %in% c(0L, 1L)))
})

test_that(
  "stancode for occ declares scalar p in (0, 1) when p is unmodelled", {
  d <- make_occ_data()
  mf <- mvgam_formula(y ~ elev)
  sc <- as.character(stancode(mf, data = d, family = occ(), validate = FALSE))
  expect_match(sc, "real<lower=0, ?upper=1> p;")
})
