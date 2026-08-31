# Build the cached fits + LFO results consumed by
# `vignettes/articles/forecast_evaluation.Rmd`.
#
# Run once locally with:
#   Rscript tests/local/forecast_eval_vignette_fits.R
#
# The article reads the RDS files unconditionally when present;
# regenerate this cache whenever a relevant API or default shifts.

suppressPackageStartupMessages({
  devtools::load_all(quiet = TRUE)
  library(mvgam)
})

# Cache lives under pkgdown/, which is already in .Rbuildignore
# (and so excluded from the CRAN tarball). pkgdown::build_articles()
# runs from the project root, so the article can read this path
# relative to the working directory at render time.
cache_dir <- file.path("pkgdown", "forecast_eval_cache")
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
}

data(portal_data, package = "mvgam")
train <- subset(portal_data, time <= 68L)
test  <- subset(portal_data, time >  68L)

# Matches the argument spelling the article displays, so the code
# shown beside each fit is the code that produced it.
CHAINS <- 2L
ITER   <- 1600L
WARMUP <- 1000L
SILENT <- 2L


# ---- mod_spline: smooths only, no trend ---------------------------------

cat("Fitting mod_spline ...\n")
mod_spline <- mvgam(
  captures ~ s(mintemp, k = 6) + s(ndvi_ma12, k = 6) +
    s(time, by = series, k = 8) + series,
  family = poisson(),
  data   = train,
  chains = CHAINS, iter = ITER, warmup = WARMUP,
  silent = SILENT
)
saveRDS(mod_spline, file.path(cache_dir, "mod_spline.rds"))


# ---- mod_ar: smooths + per-species AR(1) --------------------------------

cat("Fitting mod_ar ...\n")
mod_ar <- mvgam(
  captures ~ s(mintemp, k = 6) + s(ndvi_ma12, k = 6) + series,
  trend_formula = ~ AR(),
  family = poisson(),
  data   = train,
  chains = CHAINS, iter = ITER, warmup = WARMUP,
  silent = SILENT
)
saveRDS(mod_ar, file.path(cache_dir, "mod_ar.rds"))


# ---- mod_var: smooths + correlated VAR(1) -------------------------------

cat("Fitting mod_var ...\n")
mod_var <- mvgam(
  captures ~ s(mintemp, k = 6) + s(ndvi_ma12, k = 6) + series,
  trend_formula = ~ VAR(cor = TRUE),
  family = poisson(),
  data   = train,
  chains = CHAINS, iter = ITER, warmup = WARMUP,
  silent = SILENT
)
saveRDS(mod_var, file.path(cache_dir, "mod_var.rds"))


# ---- LFO-CV for each model ----------------------------------------------
# Expanding-window starting at t=60, 4-step horizons. The PSIS-LFO
# refit gate triggers when Pareto-k exceeds 0.7.

cat("LFO mod_spline ...\n")
lfo_spline <- lfo_cv(mod_spline, min_t = 60L, fc_horizon = 4L,
                      save_log_lik = TRUE, silent = SILENT)
saveRDS(lfo_spline, file.path(cache_dir, "lfo_spline.rds"))

cat("LFO mod_ar ...\n")
lfo_ar <- lfo_cv(mod_ar, min_t = 60L, fc_horizon = 4L,
                  save_log_lik = TRUE, silent = SILENT)
saveRDS(lfo_ar, file.path(cache_dir, "lfo_ar.rds"))

cat("LFO mod_var ...\n")
lfo_var <- lfo_cv(mod_var, min_t = 60L, fc_horizon = 4L,
                   save_log_lik = TRUE, silent = SILENT)
saveRDS(lfo_var, file.path(cache_dir, "lfo_var.rds"))

cat("\nAll fits + LFO results cached to ", cache_dir, "\n", sep = "")
