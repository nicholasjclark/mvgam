# LOCAL ONLY: replicate Jim Savage's "Hierarchical VAR" tutorial
# (https://rpubs.com/jimsavage/hierarchical_var, 27 Nov 2016) in
# mvgam. Fits a 3-dim VAR(1) per country (GDP, consumption,
# investment growth) over 8 economies with hierarchical pooling of
# the country-level coefficient matrices and innovation
# correlations.
#
# Savage's headline numbers (with WDI data up to ~2015, n = 313):
#   * E[Omega_global]:
#       1.00 0.89 0.93
#       0.89 1.00 0.74
#       0.93 0.74 1.00
#   * E[rho] = 0.82 (90% CI 0.75-0.90), n_eff ~ 610, Rhat = 1.01
#
# Savage's model blends per-country innovation correlations with a
# global pool via a scalar weight `rho`. mvgam's
# `VAR(p=1, gr = country, subgr = outcome, cor = TRUE)` runs the
# closest native equivalent: per-country (A, sigma, L_Omega) with
# `alpha_cor_trend` as the global-vs-local mixing weight.
#
# Sampler tuning notes (see also `architecture/architecture-
# decisions.md`): the Heaps stationarity transform on the joint
# 24-dim Sigma is the dominant per-gradient cost. We:
#   1. z-score each outcome so the 24-dim block-diagonal Sigma
#      lives on a single scale.
#   2. tighten `Amu_trend` to `normal(0, 0.3)` (macro AR(1) prior
#      mass).
#   3. modestly shrink the global correlation toward identity with
#      `lkj_corr_cholesky(2)`.
#   4. cap `max_treedepth` at 12; first pass uses `adapt_delta = 0.85`
#      to keep step-size adaptation moving (Heaps stationarity makes
#      higher targets very slow per gradient).
#   5. thread two cores per chain.
#
# Run with: testthat::test_file("tests/local/savage_hierarchical_var.R")

library(testthat)
library(mvgam)
library(dplyr)
library(tidyr)
library(brms)


# Pull the same three WDI series Savage used and reshape to the
# long (country, outcome, time) form mvgam expects. Standardise
# each outcome to unit variance so the per-country 3x3 innovation
# covariances are on a common scale; the per-outcome (mean, sd) is
# kept as an attribute on the returned tibble so users can map
# fitted values back to the original growth-rate scale.
prepare_savage_panel <- function() {
  if (!requireNamespace("WDI", quietly = TRUE)) {
    skip("WDI package not available")
  }
  raw <- WDI::WDI(
    indicator = c("NY.GDP.MKTP.KN", "NE.CON.TOTL.KN", "NE.GDI.FTOT.KN"),
    start = 1970
  )
  wide <- raw |>
    dplyr::filter(stats::complete.cases(raw)) |>
    dplyr::rename(
      GDP  = NY.GDP.MKTP.KN,
      CONS = NE.CON.TOTL.KN,
      GFCF = NE.GDI.FTOT.KN
    ) |>
    # WDI returns zero / negative values for a handful of small economies
    # (Sierra Leone GFCF, etc.); pre-filter so `log()` never sees a
    # non-positive input and `dplyr::mutate()` does not emit a NaN warning
    # before `complete.cases` would drop the row anyway.
    dplyr::filter(GDP > 0, CONS > 0, GFCF > 0) |>
    dplyr::group_by(country) |>
    dplyr::arrange(year) |>
    dplyr::mutate(
      dl_gdp  = c(NA, diff(log(GDP))),
      dl_cons = c(NA, diff(log(CONS))),
      dl_gfcf = c(NA, diff(log(GFCF)))
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(stats::complete.cases(dplyr::across(
      c(dl_gdp, dl_cons, dl_gfcf)
    ))) |>
    dplyr::filter(country %in% c(
      "United States", "United Kingdom", "Australia",
      "New Zealand", "Chile", "Canada", "Ireland", "South Africa"
    )) |>
    dplyr::group_by(country) |>
    dplyr::arrange(year) |>
    dplyr::mutate(time = seq_len(dplyr::n())) |>
    dplyr::ungroup()

  long <- wide |>
    tidyr::pivot_longer(
      cols = c(dl_gdp, dl_cons, dl_gfcf),
      names_to = "outcome",
      values_to = "y"
    ) |>
    dplyr::mutate(
      country = factor(country),
      outcome = factor(outcome),
      series  = factor(paste(country, outcome, sep = "."))
    )

  # Per-outcome z-score: removes the 3x scale gap between
  # investment growth (sd ~ 0.09) and the other two outcomes
  # (sd ~ 0.03) that otherwise forces HMC to traverse three very
  # different geometries inside the joint 24x24 covariance.
  scales <- long |>
    dplyr::group_by(outcome) |>
    dplyr::summarise(
      mean = mean(y, na.rm = TRUE),
      sd   = stats::sd(y, na.rm = TRUE),
      .groups = "drop"
    )
  long <- long |>
    dplyr::left_join(scales, by = "outcome") |>
    dplyr::mutate(y = (y - mean) / sd) |>
    dplyr::select(-mean, -sd)

  attr(long, "outcome_scales") <- scales
  long
}


test_that("mvgam hierarchical VAR fits Savage's WDI panel", {
  long <- prepare_savage_panel()
  expect_gt(nrow(long), 900L)
  expect_equal(nlevels(long$country), 8L)
  expect_equal(nlevels(long$outcome), 3L)
  expect_equal(nlevels(long$series), 24L)

  # Replace three priors the default set leaves diffuse: the AR
  # coefficient location, the global innovation correlation, and
  # the residual observation sd. Standardising the data already
  # absorbed most of the scale problem; these tighten the parts
  # still dominated by the prior.
  custom_priors <- c(
    brms::prior(normal(0, 0.3), class = "Amu_trend"),
    brms::prior(lkj_corr_cholesky(2), class = "L_Omega_global_trend"),
    brms::prior(student_t(3, 0, 0.5), class = "sigma")
  )

  t0 <- proc.time()
  mod <- mvgam(
    formula       = y ~ 0,
    trend_formula = ~ VAR(p = 1, gr = country, subgr = outcome,
                            cor = TRUE),
    data          = long,
    family        = gaussian(),
    prior         = custom_priors,
    chains        = 4,
    cores         = 4,
    threads_per_chain = 2,
    samples       = 500,
    burnin        = 750,
    adapt_delta   = 0.85,
    max_treedepth = 12,
    silent        = 2
  )
  cat("\nfit time:", round((proc.time() - t0)[3] / 60, 1), "min\n")

  # Convergence guardrail. Savage reports max Rhat 1.01 for rho;
  # mvgam's wider model on standardised data should converge to
  # similar Rhats across the AR coefficient and variance blocks.
  diag <- bayesplot::rhat(mod$fit)
  expect_true(max(diag, na.rm = TRUE) < 1.1)

  saveRDS(mod, "/tmp/savage_hier_var_fit.rds")
})
