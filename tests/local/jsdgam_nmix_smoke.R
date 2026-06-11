# Local smoke: closure-unit families (nmix, occ) on jsdgam.
#
# Builds a small multi-species panel with per-(species, site, visit)
# observations and attempts to fit jsdgam() with each closure-unit
# family. Each closure unit is a (species, site) combination; the
# factor model layers a low-rank species covariance across the
# species axis. The build_closure_unit_arrays() helper already keys
# on paste(series, time) which uniquely identifies (species, site)
# in the wrapper's data layout, so the data prep should compose
# without wrapper changes.
#
# Run with:
#   Rscript tests/local/jsdgam_nmix_smoke.R

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
})

set.seed(2024L)

n_species <- 4L
n_sites <- 12L
n_visits <- 3L
n_lv_true <- 2L

species_levels <- paste0("sp", seq_len(n_species))

# Simulate latent abundance N[sp, site] with a 2-factor structure.
Z_true <- matrix(rnorm(n_species * n_lv_true, sd = 0.5),
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

# Per-visit detections.
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
cat("Simulated", nrow(dat), "rows across", n_species, "species,",
    n_sites, "sites,", n_visits, "visits each.\n")
cat("Closure units = species x sites =",
    n_species * n_sites, "\n")

cat("\n=== nmix (Poisson-binomial) jsdgam smoke ===\n")
nmix_path <- tryCatch(
  {
    fit_nmix <- jsdgam(
      formula = y ~ 1,
      factor_formula = ~ -1,
      data = dat, unit = site, species = species,
      family = nmix(), n_lv = n_lv_true,
      chains = 2L, parallel = TRUE,
      burnin = 200L, samples = 200L,
      silent = 2
    )
    list(ok = TRUE, fit = fit_nmix)
  },
  error = function(e) list(ok = FALSE, msg = conditionMessage(e))
)
if (nmix_path$ok) {
  cat("[OK] nmix jsdgam fit returned.\n")
  cat("  Class:", paste(class(nmix_path$fit), collapse = ", "), "\n")
  cat("  N_unit in standata:", nmix_path$fit$standata$N_unit, "\n")
  rcor <- residual_cor(nmix_path$fit)
  cat("  residual_cor dim:", paste(dim(rcor$cor), collapse = " x "),
      "\n")
} else {
  cat("[FAIL] nmix jsdgam errored:\n  ", nmix_path$msg, "\n")
}

cat("\n=== occ (Bernoulli occupancy) jsdgam smoke ===\n")
# Re-simulate as detection-non-detection.
dat_occ <- dat
dat_occ$y <- as.integer(dat_occ$y > 0L)
occ_path <- tryCatch(
  {
    fit_occ <- jsdgam(
      formula = y ~ 1,
      factor_formula = ~ -1,
      data = dat_occ, unit = site, species = species,
      family = occ(), n_lv = n_lv_true,
      chains = 2L, parallel = TRUE,
      burnin = 200L, samples = 200L,
      silent = 2
    )
    list(ok = TRUE, fit = fit_occ)
  },
  error = function(e) list(ok = FALSE, msg = conditionMessage(e))
)
if (occ_path$ok) {
  cat("[OK] occ jsdgam fit returned.\n")
  cat("  Class:", paste(class(occ_path$fit), collapse = ", "), "\n")
  cat("  N_unit in standata:", occ_path$fit$standata$N_unit, "\n")
  rcor <- residual_cor(occ_path$fit)
  cat("  residual_cor dim:", paste(dim(rcor$cor), collapse = " x "),
      "\n")
} else {
  cat("[FAIL] occ jsdgam errored:\n  ", occ_path$msg, "\n")
}

if (nmix_path$ok && occ_path$ok) {
  cat("\n=== Surface audit on the nmix + occ jsdgam fits ===\n")
  audit_one <- function(label, fit) {
    cat("\n--", label, "\n")
    cat("  predict(link)         : ",
        paste(dim(predict(fit, type = "link")),
              collapse = " x "), "\n")
    cat("  predict(response)     : ",
        paste(dim(predict(fit, type = "response")),
              collapse = " x "), "\n")
    cat("  posterior_epred(30)   : ",
        paste(dim(posterior_epred(fit, ndraws = 30L)),
              collapse = " x "), "\n")
    cat("  posterior_predict(30) : ",
        paste(dim(posterior_predict(fit, ndraws = 30L)),
              collapse = " x "), "\n")
    ll <- log_lik(fit, ndraws = 30L)
    cat("  log_lik(30) dim       : ",
        paste(dim(ll), collapse = " x "),
        " | all finite =", all(is.finite(ll)), "\n")
    L <- suppressWarnings(loo(fit))
    cat("  loo elpd_loo          : ",
        round(L$estimates["elpd_loo", "Estimate"], 2), "\n")
    cat("  summary()             : ok\n")
    invisible(capture.output(suppressWarnings(summary(fit))))
    cat("  print()               : ok\n")
    invisible(capture.output(print(fit)))
    cat("  pp_check(30)          : ok\n")
    suppressWarnings(pp_check(fit, ndraws = 30L))
    cat("  ordinate()            : ok\n")
    ordinate(fit, alpha = 0.7)
    cat("  conditional_effects() : ok\n")
    suppressWarnings(conditional_effects(fit))
  }
  audit_one("nmix() jsdgam", nmix_path$fit)
  audit_one("occ() jsdgam", occ_path$fit)
}

cat("\nDone.\n")
