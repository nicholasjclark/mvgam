# Recovery fixture for the mvn() Multivariate Normal family.
#
# Simulates K = 4 species at n_sites = 30 sites with a known
# low-rank residual covariance Sigma_true = Z_true %*% t(Z_true)
# + diag(Psi_true^2). Z_true columns are NOT centred. The
# multi_normal lpdf is shift-sensitive, so the absolute location
# of mu is identified, and the factor model does not need the
# sum-to-zero identification that the simplex families rely on.
#
# Each site's response vector is drawn from MVN(mu_i, Sigma_true)
# using the marginal form (integrating out lv).
#
# Primary go/no-go:
#   cor(off_diag(true_cor), off_diag(post_cor)) > 0.7
#
# where true_cor = cov2cor(Sigma_true) and post_cor is
# residual_cor(fit)$cor from the factor-loadings branch.
#
# Diagnostics reported:
#   - MAE on off-diagonals
#   - per-species Psi recovery (posterior mean vs truth)
#   - divergent transitions + max-treedepth saturation
#   - min / max bulk ESS on Z entries
#
# Cached at /tmp/jsdgam_mv_mvn_recovery_fit.rds.
# Delete to refit. Runtime ~3-5 min.

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(dplyr)
  library(tidyr)
  library(posterior)
})

set.seed(604L)

K <- 4L
N_lv <- 2L
n_sites <- 30L
threshold_cor <- 0.7
species_levels <- paste0("y", seq_len(K))

Z_true <- matrix(rnorm(K * N_lv, sd = 0.7), nrow = K, ncol = N_lv)
psi_true <- rep(0.5, K)
sigma_true_cov <- tcrossprod(Z_true) + diag(psi_true^2)
sigma_true_cor <- cov2cor(sigma_true_cov)

env <- rnorm(n_sites)
mu_intercept <- rnorm(K)
mu_env_slope <- rnorm(K)

L_true <- chol(sigma_true_cov)
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
  wide_dat, all_of(species_levels),
  names_to = "series", values_to = "y"
) |>
  mutate(
    series = factor(series, levels = species_levels),
    time   = site
  ) |>
  arrange(time, series)

cat("Simulated", n_sites, "sites x", K, "species.",
    "True cor off-diag range: [",
    round(min(sigma_true_cor[upper.tri(sigma_true_cor)]), 3), ",",
    round(max(sigma_true_cor[upper.tri(sigma_true_cor)]), 3), "].\n")

cache <- "/tmp/jsdgam_mv_mvn_recovery_fit.rds"
if (file.exists(cache)) {
  cat("[cache] Loading mvn recovery fit.\n")
  fit <- readRDS(cache)
} else {
  cat("[fit ] jsdgam(mvn(), n_lv = 2, ", n_sites, " sites)\n", sep = "")
  fit <- jsdgam(
    formula = y ~ env * series,
    factor_formula = ~ -1,
    data = as.data.frame(long_dat),
    unit = time, species = series,
    family = mvn(),
    n_lv = N_lv,
    chains = 2L, parallel = TRUE,
    burnin = 500L, samples = 500L,
    silent = 2,
    backend = "cmdstanr"
  )
  saveRDS(fit, cache)
}

cat("\n=== Primary recovery (correlation off-diagonals) ===\n")
res_cor <- residual_cor(fit)
post_cor <- res_cor$cor
true_off <- sigma_true_cor[upper.tri(sigma_true_cor)]
post_off <- post_cor[upper.tri(post_cor)]
cor_off <- stats::cor(true_off, post_off)
mae_off <- mean(abs(true_off - post_off))
cat(sprintf("cor(true, posterior) = %.4f  (threshold > %.2f)\n",
            cor_off, threshold_cor))
cat(sprintf("MAE                  = %.4f\n", mae_off))
cat(sprintf("PASS: %s\n", isTRUE(cor_off > threshold_cor)))

cat("\n=== Psi recovery (per-species residual SD) ===\n")
draws <- as_draws_matrix(fit$fit)
psi_cols <- grep("^Psi\\[", colnames(draws), value = TRUE)
if (length(psi_cols) == K) {
  psi_mean <- colMeans(draws[, psi_cols, drop = FALSE])
  cat("Psi posterior mean:",
      paste(round(psi_mean, 3), collapse = " "), "\n")
  cat("Psi truth         :",
      paste(round(psi_true, 3), collapse = " "), "\n")
  cat(sprintf("max abs error     : %.3f\n",
              max(abs(psi_mean - psi_true))))
} else {
  cat("WARN: expected", K, "Psi entries, found",
      length(psi_cols), "\n")
}

cat("\n=== Sampler diagnostics ===\n")
diag_df <- nuts_params(fit$fit)
n_div <- sum(subset(diag_df, Parameter == "divergent__")$Value)
n_treedepth <- sum(
  subset(diag_df, Parameter == "treedepth__")$Value >= 10L
)
n_total_trans <- nrow(subset(diag_df, Parameter == "divergent__"))
cat(sprintf("Divergent transitions:    %d / %d (%.2f%%)\n",
            n_div, n_total_trans,
            100 * n_div / n_total_trans))
cat(sprintf("Max-treedepth saturation: %d / %d (%.2f%%)\n",
            n_treedepth, n_total_trans,
            100 * n_treedepth / n_total_trans))

cat("\n=== Z-entry bulk ESS ===\n")
z_pat <- if (any(grepl("^Z_tilde\\[", colnames(draws)))) {
  "^Z_tilde\\["
} else {
  "^Z\\["
}
z_cols <- grep(z_pat, colnames(draws), value = TRUE)
z_summary <- summarise_draws(draws[, z_cols, drop = FALSE], "ess_bulk")
cat(sprintf("min ess_bulk = %.0f\n",
            min(z_summary$ess_bulk, na.rm = TRUE)))
cat(sprintf("max ess_bulk = %.0f\n",
            max(z_summary$ess_bulk, na.rm = TRUE)))

cat("\nDone.\n")
