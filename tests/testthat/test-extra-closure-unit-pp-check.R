# The tests of test-closure-unit-pp-check.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()

# ------------------------------------------------------------
# Closure-unit pp_check type validation: per-row scatter and
# fitted-vs-residual types are blocked because they do not match
# the closure-unit grain.
# ------------------------------------------------------------

test_that(
  "pp_check.mvgam blocks closure-unit-incompatible types with a pointer", {
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
      "not supported for\\s+closure-unit"
    )
  }
})

# ------------------------------------------------------------
# pp_check(type = "fit_stat"): chi-squared / Freeman-Tukey
# discrepancy GOF (Gelman et al. 1996). The discrepancy compares a
# unit's aggregated visits against their aggregated expectation, so
# it is offered to the families that aggregate rather than to every
# family sharing the closure-unit pipeline. The math is small and
# tested directly: chi-squared at y == E[y] is zero, and the F-T
# form is scale-equivariant under shared sqrt.
# ------------------------------------------------------------

test_that("pp_check(type = 'fit_stat') is refused where no unit aggregates", {
  stub <- function(fam) {
    d <- data.frame(y = rnorm(10L), x = rnorm(10L),
                    series = factor(rep(1L, 10L)),
                    time = seq_len(10L))
    structure(list(data = d, formula = y ~ x, family = fam),
              class = "mvgam")
  }
  expect_error(
    pp_check(stub(gaussian()), type = "fit_stat"),
    "applies to the detection"
  )
  # These five share the closure-unit layout but never sum the rows of
  # a unit. A composition's rows sum to one whatever the fit says. The
  # discrepancy then has nothing to test.
  for (fam in list(diri(), multi(), categ(), mvn(), mvt())) {
    expect_error(
      pp_check(stub(fam), type = "fit_stat"),
      "one observation per row"
    )
  }
})
