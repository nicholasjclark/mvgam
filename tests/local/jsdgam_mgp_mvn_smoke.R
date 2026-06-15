# MGP loadings_prior smoke fit + side-by-side comparison vs the
# default iid Z prior on the mvn() closure-unit family.
#
# Motivation. The mvn / mvt conditional gllvm parameterisation
# (Z column scales * lv + per-species Psi residual) shows an
# E-BFMI < 0.3 tail under the default `student_t(3, 0, 0.5)` iid Z
# prior when `n_lv` is set above the data's true rank. The MGP
# (Bhattacharya-Dunson 2011) column-shrinkage prior is the
# Heaps-preserving fix: later columns are multiplicatively shrunk
# toward zero, removing the funnel geometry the iid prior leaves.
# This fixture measures the delta and reports active_factors()
# recovery so we can decide whether to promote MGP to the
# default Z prior for mv-continuous families.
#
# Design.
#   n_sites = 30, K = 4, true rank = 2, n_lv (ceiling) = 4.
#   Two fits on the SAME data:
#     A. loadings_prior = NULL   (default iid student_t Z)
#     B. loadings_prior = "mgp"  (pure MGP, a1 = 2, a2 = 4)
#   Reports per fit: divergence %, max-treedepth %, per-chain
#   E-BFMI, residual_cor recovery (cor vs truth on off-diagonals),
#   plus active_factors() output for fit B.
#
# Cached at /tmp/mgp_mvn_smoke_A.rds and /tmp/mgp_mvn_smoke_B.rds.
# Delete to refit. Runtime ~6-10 min per fit.

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(dplyr)
  library(tidyr)
  library(posterior)
})

set.seed(909L)

# K = 5 with n_lv = 4 = K - 1 satisfies the jsdgam validator
# (n_lv < n_species). True rank stays at 2, so MGP has two extra
# columns to shrink toward zero.
K       <- 5L
N_lv    <- 4L
true_r  <- 2L
n_sites <- 30L
species_levels <- paste0("y", seq_len(K))

# Truth: only the first `true_r` columns of Z carry signal; the
# remaining N_lv - true_r columns are zero. That mirrors the
# Bhattacharya-Dunson setting where the MGP prior is informative.
Z_true <- matrix(0, nrow = K, ncol = N_lv)
Z_true[, seq_len(true_r)] <- matrix(
  rnorm(K * true_r, sd = 0.7), nrow = K, ncol = true_r
)
psi_true   <- rep(0.5, K)
sigma_true <- tcrossprod(Z_true) + diag(psi_true^2)

env          <- rnorm(n_sites)
mu_intercept <- rnorm(K)
mu_env_slope <- rnorm(K)

L_true <- chol(sigma_true)
Y_wide <- matrix(NA_real_, nrow = n_sites, ncol = K)
for (i in seq_len(n_sites)) {
  mu_i <- mu_intercept + mu_env_slope * env[i]
  Y_wide[i, ] <- mu_i + as.numeric(crossprod(L_true, rnorm(K)))
}
colnames(Y_wide) <- species_levels

wide_dat <- as.data.frame(Y_wide)
wide_dat$site <- seq_len(n_sites)
wide_dat$env  <- env
long_dat <- pivot_longer(
  wide_dat, all_of(species_levels), names_to = "series", values_to = "y"
) |>
  mutate(
    series = factor(series, levels = species_levels),
    time   = site
  ) |>
  arrange(time, series)

cat(sprintf(
  "Simulated %d sites x %d species. True rank %d of %d ceiling.\n",
  n_sites, K, true_r, N_lv
))

fit_jsdgam_panel <- function(loadings_prior_arg, cache_path, label) {
  if (file.exists(cache_path)) {
    cat(sprintf("[cache] Loading %s fit.\n", label))
    return(readRDS(cache_path))
  }
  cat(sprintf("[fit ] %s (loadings_prior = %s)\n",
              label,
              if (is.null(loadings_prior_arg)) "NULL"
              else deparse(loadings_prior_arg)))
  fit <- jsdgam(
    formula        = y ~ env * series,
    factor_formula = ~ -1,
    data           = as.data.frame(long_dat),
    unit           = time,
    species        = series,
    family         = mvn(),
    n_lv           = N_lv,
    loadings_prior = loadings_prior_arg,
    chains         = 2L,
    parallel       = TRUE,
    burnin         = 500L,
    samples        = 500L,
    silent         = 2,
    backend        = "cmdstanr"
  )
  saveRDS(fit, cache_path)
  fit
}

# Per-chain E-BFMI via the standard energy / first-difference
# formula (Betancourt 2018, Stan reference manual). Returns one
# value per chain.
energy_bfmi <- function(fit) {
  nps <- bayesplot::nuts_params(fit$fit)
  energy <- subset(nps, Parameter == "energy__")
  chains <- sort(unique(energy$Chain))
  vapply(chains, function(ch) {
    e <- energy$Value[energy$Chain == ch]
    if (length(e) < 2L) return(NA_real_)
    de <- diff(e)
    sum(de^2) / ((length(e) - 1L) * stats::var(e))
  }, numeric(1L))
}

report_fit <- function(fit, label) {
  nps <- bayesplot::nuts_params(fit$fit)
  n_div  <- sum(subset(nps, Parameter == "divergent__")$Value)
  n_td   <- sum(subset(nps, Parameter == "treedepth__")$Value >= 10L)
  n_tot  <- nrow(subset(nps, Parameter == "divergent__"))
  bfmi   <- energy_bfmi(fit)
  cat(sprintf(
    "  divergences : %d / %d  (%.2f %%)\n",
    n_div, n_tot, 100 * n_div / n_tot
  ))
  cat(sprintf(
    "  max-treedepth: %d / %d  (%.2f %%)\n",
    n_td, n_tot, 100 * n_td / n_tot
  ))
  cat(sprintf(
    "  E-BFMI per chain: %s  (chain failure threshold 0.3)\n",
    paste(sprintf("%.3f", bfmi), collapse = ", ")
  ))
  rc <- residual_cor(fit)
  post_cov <- rc$cov
  cor_off <- stats::cor(
    sigma_true[upper.tri(sigma_true)],
    post_cov[upper.tri(post_cov)]
  )
  cat(sprintf("  cor(true off-diag, posterior off-diag): %.4f\n", cor_off))
  invisible(list(
    label       = label,
    div_pct     = 100 * n_div / n_tot,
    treedepth   = 100 * n_td / n_tot,
    bfmi        = bfmi,
    sigma_cor   = cor_off
  ))
}

cat("\n=== Fit A: default iid loadings_prior ===\n")
fit_A <- fit_jsdgam_panel(NULL, "/tmp/mgp_mvn_smoke_A.rds", "A_iid")
res_A <- report_fit(fit_A, "A_iid")

cat("\n=== Fit B: MGP loadings_prior ===\n")
fit_B <- fit_jsdgam_panel("mgp", "/tmp/mgp_mvn_smoke_B.rds", "B_mgp")
res_B <- report_fit(fit_B, "B_mgp")

cat("\n=== active_factors() on B (MGP fit) ===\n")
af <- active_factors(fit_B)
print(af)

cat("\n=== Side-by-side delta (B minus A) ===\n")
cat(sprintf(
  "  delta divergences   : %+.2f pp\n", res_B$div_pct - res_A$div_pct
))
cat(sprintf(
  "  delta max-treedepth : %+.2f pp\n", res_B$treedepth - res_A$treedepth
))
cat(sprintf(
  "  delta min(E-BFMI)   : %+.3f\n",
  min(res_B$bfmi) - min(res_A$bfmi)
))
cat(sprintf(
  "  delta sigma off-diag cor: %+.4f\n",
  res_B$sigma_cor - res_A$sigma_cor
))

cat("\nGo / no-go reading.\n")
cat(" - Min E-BFMI: B - A > 0 means MGP loosens the residual funnel.\n")
cat(" - Off-diag cor: should remain >= A; MGP should not destroy recovery.\n")
cat(" - active_factors median count: expected around the true rank (",
    true_r, ")\n", sep = "")

cat("\nDone.\n")
