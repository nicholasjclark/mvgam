# Helpers shared by test-ranef-varcorr.R and test-extra-ranef-varcorr.R.
# CI-safe tests for the random-effect alias helpers
# (mvgam_ranef_aliases / apply_mvgam_aliases path) and the
# ranef.mvgam / VarCorr.mvgam S3 surface. Numerical concordance
# against brms lives in tests/local/. Here we lock in the gate
# (no-RE fits return empty maps cheaply), the map shape on a
# 2-coef correlated RE, and S3 dispatch + signature parity with
# brms.

# A fit-shaped stub for a gaussian model: the formula, its Stan data
# and the brms observation model, whose `ranef` table the aliases are
# built from.
ranef_stub <- function(formula, data) {
  family <- brms::brmsfamily("gaussian")
  structure(
    list(
      formula = formula, data = data, family = family,
      standata = as.list(brms::standata(formula, data = data,
                                        family = family)),
      obs_model = brms::brm(formula, data = data, family = family,
                            empty = TRUE, silent = 2)
    ),
    class = "mvgam"
  )
}
