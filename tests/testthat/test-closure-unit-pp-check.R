# Tests for the closure-unit pp_check helpers. Structural coverage
# of the new `closure_unit_pp_check_setup()` aggregation hook and
# the `check_closure_unit_var_unit_constant()` group / x validator;
# end-to-end MCMC integration on the cached occ fixture lives in
# tests/local/test-closure-unit-pp-check.R.

# Helpers shared with the residuals tests.
make_pp_check_grid <- function(n_unit = 4L, n_visit = 3L,
                                seed = 17L) {
  set.seed(seed)
  n_total <- n_unit * n_visit
  data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_total),
    visit  = rep(seq_len(n_visit), n_unit),
    y      = sample(c(0L, 1L), n_total, replace = TRUE),
    elev   = rep(rnorm(n_unit), each = n_visit),
    tod    = stats::runif(n_total)
  )
}

mock_pp_check_object <- function(data, family = occ()) {
  structure(
    list(data = data, formula = y ~ 1, family = family),
    class = "mvgam"
  )
}

# ------------------------------------------------------------
# closure_unit_pp_check_setup(): aggregates per-visit y / yrep to
# the per-unit grain. resid_* types arrive with yrep already at
# the unit grain from residuals.mvgam.
# ------------------------------------------------------------

test_that("closure_unit_pp_check_setup() collapses non-resid yrep to the per-unit grain", {
  d <- make_pp_check_grid(n_unit = 4L, n_visit = 3L)
  obj <- mock_pp_check_object(d)
  yrep <- matrix(
    sample(c(0L, 1L), 5L * nrow(d), replace = TRUE),
    nrow = 5L, ncol = nrow(d)
  )
  res <- closure_unit_pp_check_setup(
    obj, newdata = d,
    y = as.numeric(d$y), yrep = yrep, type = "bars"
  )
  expect_identical(length(res$y), 4L)
  expect_identical(dim(res$yrep), c(5L, 4L))
  # Each y_unit must equal sum of the visits in that unit.
  for (g in seq_len(res$arrays$N_unit)) {
    idx <- res$arrays$visit_idx[g, seq_len(res$arrays$n_rep[g])]
    expect_equal(unname(res$y[g]), sum(d$y[idx]))
  }
  # First-visits lookup carries one obs index per unit.
  expect_identical(length(res$first_visits), 4L)
})

test_that("closure_unit_pp_check_setup() preserves the per-unit yrep grain on resid_* types", {
  d <- make_pp_check_grid()
  obj <- mock_pp_check_object(d)
  # residuals.mvgam returns [ndraws x N_unit]; mimic that shape.
  per_unit_resid <- matrix(rnorm(7L * 4L), nrow = 7L, ncol = 4L)
  res <- closure_unit_pp_check_setup(
    obj, newdata = d, y = as.numeric(d$y),
    yrep = per_unit_resid, type = "resid_hist"
  )
  expect_identical(dim(res$yrep), c(7L, 4L))
  # y is zeros at unit length for resid_* (bayesplot's
  # ppc_error_hist receives y = 0 + (-1)*resid).
  expect_identical(res$y, rep(0, 4L))
})

# ------------------------------------------------------------
# Closure-unit pp_check type validation: per-row scatter and
# fitted-vs-residual types are blocked because they do not match
# the closure-unit grain.
# ------------------------------------------------------------

test_that("pp_check.mvgam blocks closure-unit-incompatible types early with a clear pointer", {
  d <- make_pp_check_grid()
  obj <- mock_pp_check_object(d)
  blocked <- c("scatter_avg", "scatter_avg_grouped",
               "error_scatter_avg", "error_scatter_avg_vs_x",
               "error_binned",
               "resid_acf", "resid_pacf", "resid_vs_fitted",
               "resid_ribbon", "resid_ribbon_grouped")
  for (t in blocked) {
    expect_error(
      pp_check(obj, type = t),
      "not available for"
    )
  }
})

# ------------------------------------------------------------
# Unit-constant validator: per-visit covariates passed to
# `group =` or `x =` must be constant within every closure
# unit. The fixture `tod` varies within a unit; `elev` does not.
# ------------------------------------------------------------

test_that("check_closure_unit_var_unit_constant() rejects per-visit-varying covariates", {
  d <- make_pp_check_grid()
  arrays <- build_closure_unit_arrays(
    d, response_var = "y", default_cap = 1L
  )
  expect_error(
    check_closure_unit_var_unit_constant(d$tod, arrays, "tod"),
    "not constant within every closure unit"
  )
})

test_that("check_closure_unit_var_unit_constant() accepts unit-constant covariates", {
  d <- make_pp_check_grid()
  arrays <- build_closure_unit_arrays(
    d, response_var = "y", default_cap = 1L
  )
  expect_silent(
    check_closure_unit_var_unit_constant(d$elev, arrays, "elev")
  )
})
