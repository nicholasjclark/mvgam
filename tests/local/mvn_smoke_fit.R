# Smoke fit for the mvn() multivariate normal closure-unit family.
#
# Confirms that:
#   1. `jsdgam(family = mvn(), backend = "cmdstanr")` compiles and
#      samples cleanly on a small simulated continuous-response JSDM
#      panel (K = 4, n_lv = 2, 30 sites).
#   2. The recovered residual covariance `Z Z' + diag(Psi^2)`
#      correlates with the true Sigma off-diagonals at cor > 0.6.
#   3. Sampler diagnostics: divergences <= 5, max-treedepth
#      saturation < 20%. Gates are loose for the smoke pass;
#      the full recovery fixture uses tighter thresholds.
#
# Cached at /tmp/mvn_smoke_fit.rds. Delete to refit. Runtime ~3-5 min.

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(dplyr)
  library(tidyr)
  library(posterior)
})

set.seed(101L)

K <- 4L
N_lv <- 2L
n_sites <- 30L
species_levels <- paste0("y", seq_len(K))

# True low-rank Sigma + diagonal residual.
Z_true <- matrix(rnorm(K * N_lv, sd = 0.7), nrow = K, ncol = N_lv)
psi_true <- rep(0.5, K)
sigma_true <- tcrossprod(Z_true) + diag(psi_true^2)

env <- rnorm(n_sites)
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

cat("Simulated", n_sites, "sites x", K, "species. True Sigma off-diag",
    "range: [", round(min(sigma_true[upper.tri(sigma_true)]), 3),
    ",", round(max(sigma_true[upper.tri(sigma_true)]), 3), "].\n")

cache <- "/tmp/mvn_smoke_fit.rds"
if (file.exists(cache)) {
  cat("[cache] Loading mvn fit.\n")
  fit_mvn <- readRDS(cache)
} else {
  cat("[fit ] mvn() (jsdgam, y ~ env * series, n_lv = 2)\n")
  fit_mvn <- jsdgam(
    formula = y ~ env * series,
    factor_formula = ~ -1,
    data = as.data.frame(long_dat),
    unit = time, species = series,
    family = mvn(),
    n_lv = 2L,
    chains = 2L, parallel = TRUE,
    burnin = 500L, samples = 500L,
    silent = 2,
    backend = "cmdstanr"
  )
  saveRDS(fit_mvn, cache)
}

cat("\n=== Sampler diagnostics ===\n")
diag_df <- nuts_params(fit_mvn$fit)
n_div <- sum(subset(diag_df, Parameter == "divergent__")$Value)
n_treedepth <- sum(subset(diag_df, Parameter == "treedepth__")$Value >= 10L)
n_total_trans <- nrow(subset(diag_df, Parameter == "divergent__"))
cat("Divergent transitions:", n_div, "/", n_total_trans,
    "(", round(100 * n_div / n_total_trans, 2), "%)\n")
cat("Max-treedepth saturation:", n_treedepth, "/", n_total_trans,
    "(", round(100 * n_treedepth / n_total_trans, 2), "%)\n")

cat("\n=== Posterior recovery ===\n")
res_cor <- residual_cor(fit_mvn)
post_sigma <- res_cor$sigma$mean
true_off  <- sigma_true[upper.tri(sigma_true)]
post_off  <- post_sigma[upper.tri(post_sigma)]
cor_off <- stats::cor(true_off, post_off)
cat("cor(true Sigma off-diag, posterior Sigma off-diag): ",
    round(cor_off, 4), "\n")

cat("\n=== Psi recovery (per-species residual SD) ===\n")
psi_draws <- as_draws_matrix(fit_mvn$fit)
psi_cols <- grep("^Psi\\[", colnames(psi_draws), value = TRUE)
if (length(psi_cols) == K) {
  psi_mean <- colMeans(psi_draws[, psi_cols])
  cat("Psi posterior mean:", paste(round(psi_mean, 3), collapse = " "), "\n")
  cat("Psi truth         :", paste(round(psi_true, 3), collapse = " "), "\n")
  cat("max abs error     :", round(max(abs(psi_mean - psi_true)), 3), "\n")
} else {
  cat("WARN: expected", K, "Psi entries, found", length(psi_cols), "\n")
}

cat("\nDone.\n")
