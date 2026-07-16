# Build the cached hierarchical-VAR fit consumed by
# `vignettes/articles/hierarchical_var.Rmd`.
#
# Run once locally with:
#   Rscript tests/local/hierarchical_var_vignette_fits.R
#
# The article reads the RDS files unconditionally when present;
# regenerate this cache whenever a relevant API or default shifts.

suppressPackageStartupMessages({
  devtools::load_all(quiet = TRUE)
  library(mvgam)
  library(dplyr)
  library(tidyr)
  library(brms)
})

cache_dir <- file.path("pkgdown", "hierarchical_var_cache")
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
}

# ---- Panel prep --------------------------------------------------------

# Pull three World Development Indicators series (real GDP, private
# consumption, gross fixed capital formation) for the eight developed
# economies Savage used, take annual log-differences, filter to years
# with all three series populated, then reshape to a long
# (country, outcome, time) frame with a per-outcome z-score. The
# per-outcome scaling collapses the 3x spread between investment (sd
# ~ 0.09) and the other two outcomes (sd ~ 0.03) so the joint 24x24
# covariance sits on a single geometry inside the sampler.
prepare_savage_panel <- function() {
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

  # Pad ragged tails so every (country, outcome, time) cell is
  # present with y = NA where the WDI series stops short. mvgam's
  # data-shape gate requires a rectangular panel over the shared
  # union of time points; NAs on 'y' are handled by the per-response
  # NA machinery.
  full_grid <- tidyr::expand_grid(
    country = levels(long$country),
    outcome = levels(long$outcome),
    time    = seq_len(max(long$time))
  ) |>
    dplyr::mutate(
      country = factor(country, levels = levels(long$country)),
      outcome = factor(outcome, levels = levels(long$outcome)),
      series  = factor(
        paste(country, outcome, sep = "."),
        levels = levels(long$series)
      )
    )
  long <- dplyr::left_join(
    full_grid, long,
    by = c("country", "outcome", "time", "series")
  )

  attr(long, "outcome_scales") <- scales
  long
}

long <- prepare_savage_panel()
saveRDS(long, file.path(cache_dir, "wdi_panel.rds"))

# ---- Fit ----------------------------------------------------------------

# Replace three priors the default set leaves diffuse: the AR
# coefficient location, the global innovation correlation, and the
# residual observation sd. Standardising the data already absorbed
# most of the scale problem; these tighten the parts still dominated
# by the prior.
custom_priors <- c(
  brms::prior(normal(0, 0.15), class = "Amu_trend"),
  brms::prior(lkj_corr_cholesky(2), class = "L_Omega_global_trend"),
  brms::prior(student_t(3, 0, 0.5), class = "sigma")
)

t0 <- proc.time()
mod_hier <- mvgam(
  formula       = y ~ 0,
  trend_formula = ~ VAR(p = 1, gr = country, subgr = outcome,
                           cor = TRUE),
  data          = long,
  family        = gaussian(),
  prior         = custom_priors,
  chains        = 4,
  cores         = 4,
  iter          = 700,
  warmup        = 500,
  adapt_delta   = 0.95,
  max_treedepth = 15,
  silent        = 2
)
cat("\nhier fit time:", round((proc.time() - t0)[3] / 60, 1), "min\n")

saveRDS(mod_hier, file.path(cache_dir, "mod_hier.rds"))
