# End-to-end demonstration of kfold.mvgam() across the three
# modes (pure PSIS / hybrid / exact) on a small Gaussian fit, plus
# a closure-unit (occ) fit with leave-one-site-out CV.
#
# Caches:
#   /tmp/kfold_demo_gauss_fit.rds
#   /tmp/kfold_demo_occ_fit.rds
#   /tmp/kfold_demo_results.rds
#   /tmp/kfold_demo_plot.png
#
# Run with:
#   Rscript tests/local/kfold_grouped_cv.R

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(ggplot2)
})

# ------------------------------------------------------------
# 1) Small Gaussian fit, leave-one-site-out kfold
# ------------------------------------------------------------
gauss_cache <- "/tmp/kfold_demo_gauss_fit.rds"
if (file.exists(gauss_cache)) {
  fit_gauss <- readRDS(gauss_cache)
  cat("[CACHE] reusing", gauss_cache, "\n")
} else {
  set.seed(2026L)
  n_site <- 8L
  n_obs <- 6L
  site_eff <- rnorm(n_site, 0, 0.6)
  dat_g <- data.frame(
    site = rep(paste0("s", seq_len(n_site)), each = n_obs),
    time = rep(seq_len(n_obs), n_site),
    x = rnorm(n_site * n_obs),
    series = factor("S1")
  )
  dat_g$y <- with(dat_g, 1 + 0.5 * x + site_eff[match(site, paste0("s", 1:n_site))]) +
    rnorm(n_site * n_obs, sd = 0.5)
  cat("Fitting small mvgam (Gaussian) ...\n")
  fit_gauss <- mvgam(
    formula = y ~ x + (1 | site),
    data = dat_g, family = gaussian(),
    chains = 2L, burnin = 200L, samples = 200L,
    silent = 2, refresh = 0
  )
  saveRDS(fit_gauss, gauss_cache)
  cat("[FIT] cached to", gauss_cache, "\n")
}

cat("\n=== Gaussian: kfold(group = 'site') three modes ===\n")
results <- list()

cat("\n-- pure PSIS (pareto_k_threshold = Inf) --\n")
t_psis <- system.time({
  results$psis <- kfold(fit_gauss, group = "site",
                        pareto_k_threshold = Inf, silent = 2L)
})
print(results$psis)

cat("\n-- hybrid (refit on high Pareto-k) --\n")
t_hybrid <- system.time({
  results$hybrid <- kfold(fit_gauss, group = "site", silent = 2L)
})
print(results$hybrid)

cat("\n-- exact (refit every fold) --\n")
t_exact <- system.time({
  results$exact <- kfold(fit_gauss, group = "site",
                         exact = TRUE, silent = 2L)
})
print(results$exact)

cat("\nWall times (s): PSIS =", round(t_psis["elapsed"], 1),
    "; hybrid =", round(t_hybrid["elapsed"], 1),
    "; exact =", round(t_exact["elapsed"], 1), "\n")

cat("\nELPD agreement (all three should agree within SE):\n")
print(rbind(
  PSIS = c(elpd = results$psis$elpd_kfold,
           se = results$psis$se_elpd_kfold),
  hybrid = c(elpd = results$hybrid$elpd_kfold,
             se = results$hybrid$se_elpd_kfold),
  exact = c(elpd = results$exact$elpd_kfold,
            se = results$exact$se_elpd_kfold)
))

cat("\nplot.mvgam_kfold rendering ...\n")
p <- plot(results$hybrid) +
  ggplot2::labs(title = "kfold(fit, group = 'site') - hybrid mode")
ggsave("/tmp/kfold_demo_plot.png", plot = p,
       width = 7, height = 5, dpi = 120)
cat("Wrote /tmp/kfold_demo_plot.png\n")

# ------------------------------------------------------------
# 2) Closure-unit (occ) fit, leave-one-site-out kfold
# ------------------------------------------------------------
occ_cache <- "/tmp/kfold_demo_occ_fit.rds"
if (file.exists(occ_cache)) {
  fit_occ <- readRDS(occ_cache)
  cat("\n[CACHE] reusing", occ_cache, "\n")
} else {
  set.seed(3L)
  n_species <- 4L
  n_sites <- 12L
  n_visits <- 4L
  species_levels <- paste0("sp", seq_len(n_species))
  rows <- list()
  for (sp in seq_len(n_species)) {
    for (st in seq_len(n_sites)) {
      psi <- plogis(rnorm(1L, 0, 1))
      z <- rbinom(1L, 1L, psi)
      for (v in seq_len(n_visits)) {
        rows[[length(rows) + 1L]] <- data.frame(
          species = species_levels[sp],
          site = st, visit = v,
          y = z * rbinom(1L, 1L, 0.5)
        )
      }
    }
  }
  dat_o <- do.call(rbind, rows)
  dat_o$species <- factor(dat_o$species, levels = species_levels)
  cat("\nFitting small jsdgam(occ) ...\n")
  fit_occ <- jsdgam(
    formula = y ~ 1, factor_formula = ~ -1,
    data = dat_o, unit = site, species = species,
    family = occ(), n_lv = 2L,
    chains = 2L, parallel = TRUE,
    burnin = 200L, samples = 200L,
    silent = 2, refresh = 0
  )
  saveRDS(fit_occ, occ_cache)
  cat("[FIT] cached to", occ_cache, "\n")
}

cat("\n=== occ: kfold(group = 'site') hybrid ===\n")
results$occ_hybrid <- kfold(fit_occ, group = "site", silent = 2L)
print(results$occ_hybrid)

saveRDS(results, "/tmp/kfold_demo_results.rds")
cat("\nAll done. Results cached to /tmp/kfold_demo_results.rds\n")
