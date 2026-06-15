# Recovery fixture for the multi() Multinomial family.
#
# Simulates K = 4 species at n_sites = 30 sites under a known
# rank-2 factor loading Z_true (column-centred to match Stan's
# sum_to_zero_vector identification), softmax-transforms to
# category probabilities per site, then draws Multinomial counts
# with per-site total N_i sampled in [40, 80].
#
# Primary go/no-go:
#   cor(off_diag(true_cor), off_diag(post_cor)) > 0.7
#
# where true_cor = cov2cor(Z_true %*% t(Z_true)) and post_cor
# is residual_cor(fit)$cor from the factor-loadings branch.
#
# Diagnostics reported:
#   - MAE on off-diagonals
#   - max|colSums(posterior_mean(Z))| (mode-1 health check)
#   - divergent transitions + max-treedepth saturation
#   - min / max bulk ESS on Z entries
#
# Cached at /tmp/jsdgam_mv_multi_recovery_fit.rds.
# Delete to refit. Runtime ~3-5 min.

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(dplyr)
  library(tidyr)
  library(posterior)
})

set.seed(602L)

K <- 4L
N_lv <- 2L
n_sites <- 30L
threshold_cor <- 0.7
species_levels <- paste0("y", seq_len(K))

Z_true <- matrix(rnorm(K * N_lv, sd = 0.7), nrow = K, ncol = N_lv)
Z_true <- scale(Z_true, center = TRUE, scale = FALSE)
attr(Z_true, "scaled:center") <- NULL

sigma_true_cov <- tcrossprod(Z_true)
sigma_true_cor <- cov2cor(sigma_true_cov + diag(1e-8, K))

env <- rnorm(n_sites)
mu_intercept <- rnorm(K)
mu_env_slope <- rnorm(K)

Y_wide <- matrix(NA_integer_, nrow = n_sites, ncol = K)
N_per_site <- sample(40:80, n_sites, replace = TRUE)
for (i in seq_len(n_sites)) {
  lv_i <- rnorm(N_lv)
  eta_i <- mu_intercept + mu_env_slope * env[i] +
             as.numeric(Z_true %*% lv_i)
  p_i <- exp(eta_i) / sum(exp(eta_i))
  Y_wide[i, ] <- as.vector(rmultinom(1L, N_per_site[i], p_i))
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
    time   = site,
    y      = as.integer(y)
  ) |>
  arrange(time, series)

cat("Simulated", n_sites, "sites x", K, "species. True cor off-diag",
    "range: [",
    round(min(sigma_true_cor[upper.tri(sigma_true_cor)]), 3), ",",
    round(max(sigma_true_cor[upper.tri(sigma_true_cor)]), 3), "].\n")

cache <- "/tmp/jsdgam_mv_multi_recovery_fit.rds"
if (file.exists(cache)) {
  cat("[cache] Loading multi recovery fit.\n")
  fit <- readRDS(cache)
} else {
  cat("[fit ] jsdgam(multi(), n_lv = 2, ", n_sites, " sites)\n", sep = "")
  fit <- jsdgam(
    formula = y ~ env * series,
    factor_formula = ~ -1,
    data = as.data.frame(long_dat),
    unit = time, species = series,
    family = multi(),
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

cat("\n=== Mode-1 diagnostic (Z column sums) ===\n")
draws <- as_draws_matrix(fit$fit)
Z_arr <- mvgam:::extract_Z_loadings(
  draws, n_obs_series = K, n_lv = N_lv
)
Z_mean <- apply(Z_arr, c(2L, 3L), mean)
col_sums_abs <- abs(colSums(Z_mean))
cat(sprintf("max|colSums(posterior_mean(Z))| = %.4f\n",
            max(col_sums_abs)))
cat("(Hard sum_to_zero_vector pins this to ~0 by construction.)\n")

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
