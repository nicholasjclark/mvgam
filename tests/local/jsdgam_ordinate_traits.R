# Local end-to-end demonstration for the new `traits` arg on
# `ordinate.jsdgam()`. Fits one small jsdgam with three species,
# eight sites and two true latent factors, then renders three
# biplots side by side:
#
#   1. Baseline biplot (traits = NULL, no overlay)
#   2. Biplot with two continuous trait arrows (body_size, fecundity)
#   3. Biplot with the same two traits at trait_arrow_scale = 0.6,
#      to show the scaling control
#
# Cached fit: /tmp/jsdgam_ordinate_traits_fit.rds
# Output PNGs: /tmp/ordinate_traits_<label>.png
#
# Run with:
#   Rscript tests/local/jsdgam_ordinate_traits.R

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(ggplot2)
})

cache <- "/tmp/jsdgam_ordinate_traits_fit.rds"
trait_cache <- "/tmp/jsdgam_ordinate_traits_data.rds"

if (file.exists(cache) && file.exists(trait_cache)) {
  fit <- readRDS(cache)
  bundle <- readRDS(trait_cache)
  cat("[CACHE] reusing", cache, "\n")
} else {
  set.seed(2026L)
  n_species <- 6L
  n_sites <- 20L
  n_visits <- 4L
  n_lv_true <- 2L
  species_levels <- paste0("sp", seq_len(n_species))

  # Simulate true cross-species covariance + per-site detections.
  Z_true <- matrix(rnorm(n_species * n_lv_true, sd = 0.6),
                   n_species, n_lv_true)
  for (k in seq_len(n_lv_true)) {
    if (Z_true[k, k] < 0) Z_true[, k] <- -Z_true[, k]
  }
  lv_true <- matrix(rnorm(n_sites * n_lv_true), n_sites, n_lv_true)
  log_lambda <- lv_true %*% t(Z_true) +
    matrix(rnorm(n_species, 1, 0.2),
           n_sites, n_species, byrow = TRUE)
  lambda <- exp(log_lambda)
  N_latent <- matrix(rpois(n_sites * n_species, lambda),
                     n_sites, n_species)
  p_det <- 0.6

  rows <- list()
  for (sp in seq_len(n_species)) {
    for (st in seq_len(n_sites)) {
      for (v in seq_len(n_visits)) {
        rows[[length(rows) + 1L]] <- data.frame(
          species = species_levels[sp],
          site = st,
          visit = v,
          y = rbinom(1L, N_latent[st, sp], p_det),
          cap = max(N_latent) + 5L
        )
      }
    }
  }
  dat <- do.call(rbind, rows)
  dat$species <- factor(dat$species, levels = species_levels)

  # Synthetic per-species traits. body_size loads positively on
  # Z_true column 1; fecundity loads negatively on Z_true column 2.
  # Adding noise so the regression isn't a perfect fit.
  set.seed(99L)
  traits <- data.frame(
    body_size = 2 * Z_true[, 1L] + rnorm(n_species, 0, 0.2),
    fecundity = -1.5 * Z_true[, 2L] + rnorm(n_species, 0, 0.2),
    row.names = species_levels
  )

  cat("Fitting jsdgam (", n_species, "species,", n_sites,
      "sites,", n_visits, "visits) ...\n", sep = " ")
  fit <- jsdgam(
    formula = y ~ 1,
    factor_formula = ~ -1,
    data = dat, unit = site, species = species,
    family = nmix(), n_lv = n_lv_true,
    chains = 2L, parallel = TRUE,
    burnin = 300L, samples = 300L,
    silent = 2, refresh = 0
  )
  bundle <- list(traits = traits, Z_true = Z_true)
  saveRDS(fit, cache)
  saveRDS(bundle, trait_cache)
  cat("[FIT] cached to", cache, "\n")
}

traits <- bundle$traits

# Build three plots.
cat("Building plots...\n")
p_baseline <- ordinate(fit, biplot = TRUE)
p_traits <- ordinate(fit, biplot = TRUE, traits = traits)
p_traits_small <- ordinate(
  fit, biplot = TRUE, traits = traits, trait_arrow_scale = 0.6
)

ggsave(
  "/tmp/ordinate_traits_baseline.png",
  plot = p_baseline + labs(title = "Baseline (traits = NULL)"),
  width = 6, height = 5, dpi = 120
)
ggsave(
  "/tmp/ordinate_traits_overlay.png",
  plot = p_traits + labs(
    title = "With trait arrows (scale = 1)"
  ),
  width = 6, height = 5, dpi = 120
)
ggsave(
  "/tmp/ordinate_traits_overlay_small.png",
  plot = p_traits_small + labs(
    title = "Trait arrows shrunk (scale = 0.6)"
  ),
  width = 6, height = 5, dpi = 120
)
cat("Wrote:\n")
cat("  /tmp/ordinate_traits_baseline.png\n")
cat("  /tmp/ordinate_traits_overlay.png\n")
cat("  /tmp/ordinate_traits_overlay_small.png\n")
