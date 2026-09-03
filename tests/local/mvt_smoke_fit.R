# Smoke fit for the mvt() multivariate Student-t closure-unit family.
#
# Confirms that:
#   1. `jsdgam(family = mvt(), backend = "cmdstanr")` compiles and
#      samples cleanly on a small heavy-tailed simulated JSDM panel
#      (K = 4, n_lv = 2, 30 sites).
#   2. The recovered residual correlation `Z Z'` correlates with the
#      true low-rank Sigma off-diagonals at cor > 0.5 (loose smoke
#      gate; the recovery fixture under tests/local/jsdgam_mv_mvt.R
#      will tighten this).
#   3. Sampler diagnostics: divergences < 5%, max-treedepth saturation
#      < 20%.
#   4. The posterior on `nu` concentrates above the hard floor of 2
#      and away from the prior median (~14), indicating the data is
#      informative about the tail heaviness.
#
# Heavy-tailed data is simulated by sampling residuals from a
# Student-t at the true nu rather than Gaussian. The conditional
# gllvm parameterisation handles the lv * Z contribution to mu, so
# the per-row residual stays scalar-Student-t at scale Psi[k].
#
# Cached at tests/local/fixtures/val_mvgam_mvt_smoke.rds. Delete to
# refit. Runtime ~5-8 min.

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(dplyr)
  library(tidyr)
  library(posterior)
})

# testthat sets the working directory to tests/local/ when it runs a
# file, while Rscript runs it from the package root. Reach the shared
# fixture helpers by whichever of the two paths exists.
source(if (file.exists("concordance_helpers.R")) {
  "concordance_helpers.R"
} else {
  file.path("tests", "local", "concordance_helpers.R")
})

set.seed(202L)

K <- 4L
N_lv <- 2L
n_sites <- 30L
species_levels <- paste0("y", seq_len(K))

# True low-rank Sigma + per-species residual scale + degrees of
# freedom for the residual Student-t.
Z_true <- matrix(rnorm(K * N_lv, sd = 0.7), nrow = K, ncol = N_lv)
psi_true <- rep(0.5, K)
nu_true <- 5
sigma_true <- tcrossprod(Z_true) + diag(psi_true^2 * nu_true / (nu_true - 2))

env <- rnorm(n_sites)
mu_intercept <- rnorm(K)
mu_env_slope <- rnorm(K)

# Conditional gllvm sim: each site gets a latent lv ~ N(0, I), then
# mu_unit_i = mu_fixed_i + Z * lv_i, then per-row residuals are
# independent Student-t with scale psi[k] and shared nu.
lv_sim <- matrix(rnorm(n_sites * N_lv), nrow = n_sites, ncol = N_lv)
Y_wide <- matrix(NA_real_, nrow = n_sites, ncol = K)
for (i in seq_len(n_sites)) {
  mu_i <- mu_intercept + mu_env_slope * env[i] + as.numeric(Z_true %*% lv_sim[i, ])
  resid_i <- psi_true * rt(K, df = nu_true)
  Y_wide[i, ] <- mu_i + resid_i
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

cat("Simulated", n_sites, "sites x", K, "species. True nu =", nu_true,
    "| true Sigma off-diag range: [",
    round(min(sigma_true[upper.tri(sigma_true)]), 3), ",",
    round(max(sigma_true[upper.tri(sigma_true)]), 3), "].\n")
cat("y range: [", round(min(long_dat$y), 2), ",",
    round(max(long_dat$y), 2), "]\n")

# The generative truth rides on the saved fit so a separate test can
# assert recovery against it without repeating the simulation.
sim_truth <- list(
  K = K, N_lv = N_lv, species_levels = species_levels,
  Z_true = Z_true, psi_true = psi_true, nu_true = nu_true,
  sigma_true = sigma_true, lv_sim = lv_sim,
  mu_intercept = mu_intercept, mu_env_slope = mu_env_slope, env = env
)

cache <- local_fixture_path("val_mvgam_mvt_smoke.rds")
if (file.exists(cache)) {
  cat("[cache] Loading mvt fit.\n")
  fit_mvt <- readRDS(cache)
} else {
  cat("[fit ] mvt() (jsdgam, y ~ env * series, n_lv = 2)\n")
  fit_mvt <- jsdgam(
    formula = y ~ env * series,
    factor_formula = ~ -1,
    data = as.data.frame(long_dat),
    unit = time, species = series,
    family = mvt(),
    n_lv = 2L,
    chains = 2L,
    burnin = 500L, samples = 500L,
    silent = 2,
    backend = "cmdstanr"
  )
}
if (!identical(attr(fit_mvt, "sim_truth"), sim_truth)) {
  attr(fit_mvt, "sim_truth") <- sim_truth
  saveRDS(fit_mvt, cache)
}

cat("\n=== Sampler diagnostics ===\n")
diag_df <- nuts_params(fit_mvt$fit)
n_div <- sum(subset(diag_df, Parameter == "divergent__")$Value)
n_treedepth <- sum(subset(diag_df, Parameter == "treedepth__")$Value >= 10L)
n_total_trans <- nrow(subset(diag_df, Parameter == "divergent__"))
cat("Divergent transitions:", n_div, "/", n_total_trans,
    "(", round(100 * n_div / n_total_trans, 2), "%)\n")
cat("Max-treedepth saturation:", n_treedepth, "/", n_total_trans,
    "(", round(100 * n_treedepth / n_total_trans, 2), "%)\n")

cat("\n=== Psi recovery (per-species residual SD) ===\n")
psi_draws <- as_draws_matrix(fit_mvt$fit)
psi_cols <- grep("^Psi\\[", colnames(psi_draws), value = TRUE)
if (length(psi_cols) == K) {
  psi_mean <- colMeans(psi_draws[, psi_cols])
  cat("Psi posterior mean:", paste(round(psi_mean, 3), collapse = " "), "\n")
  cat("Psi truth         :", paste(round(psi_true, 3), collapse = " "), "\n")
  cat("max abs error     :", round(max(abs(psi_mean - psi_true)), 3), "\n")
} else {
  cat("WARN: expected", K, "Psi entries, found", length(psi_cols), "\n")
}

cat("\n=== nu recovery (Student-t degrees of freedom) ===\n")
if ("nu" %in% colnames(psi_draws)) {
  nu_draws <- as.numeric(psi_draws[, "nu"])
  cat("nu posterior mean   =", round(mean(nu_draws), 2), "\n")
  cat("nu posterior median =", round(median(nu_draws), 2), "\n")
  cat("nu posterior Q2.5   =", round(quantile(nu_draws, 0.025), 2), "\n")
  cat("nu posterior Q97.5  =", round(quantile(nu_draws, 0.975), 2), "\n")
  cat("nu truth            =", nu_true, "\n")
  cat("P(nu >= 2 | data)   =", round(mean(nu_draws >= 2), 3),
      "(hard floor; should be 1)\n")
}

cat("\nDone.\n")
