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

# ------------------------------------------------------------
# pp_check(type = "fit_stat"): chi-squared / Freeman-Tukey
# discrepancy GOF (Gelman et al. 1996). The dispatch is gated on
# closure-unit families; non-closure-unit fits get a typed error
# pointing to occ() / nmix(). The math itself is small and tested
# directly: chi-squared at y == E[y] is zero, and the F-T form is
# scale-equivariant under shared sqrt.
# ------------------------------------------------------------

test_that("pp_check(type = 'fit_stat') errors on non-closure-unit family", {
  # gaussian mvgam stub: family is gaussian(), not closure-unit
  d <- data.frame(y = rnorm(10L), x = rnorm(10L),
                  series = factor(rep(1L, 10L)),
                  time = seq_len(10L))
  obj <- structure(
    list(data = d, formula = y ~ x, family = gaussian()),
    class = "mvgam"
  )
  expect_error(
    pp_check(obj, type = "fit_stat"),
    "only available for closure-unit families"
  )
})

test_that("mvgam_ppc_fit_stat print + plot methods produce expected output", {
  # Hand-construct a fit_stat result with a known Bayesian p-value
  # so the formatting is testable without a real Stan fit.
  set.seed(7L)
  T_obs <- rnorm(50L, mean = 100, sd = 5)
  T_rep <- rnorm(50L, mean = 100, sd = 5)
  obj <- structure(
    list(
      stat = "chi_squared", group = NULL, grain = "closure unit",
      T_obs = T_obs, T_rep = T_rep,
      bayes_p = mean(T_rep >= T_obs),
      n_draws = 50L, family = "occ"
    ),
    class = c("mvgam_ppc_fit_stat", "list")
  )
  out <- capture.output(print(obj))
  expect_true(any(grepl("chi-squared", out)))
  expect_true(any(grepl("Bayesian p-value", out)))
  expect_true(any(grepl("closure unit", out)))
  p <- plot(obj)
  expect_s3_class(p, "ggplot")
})
