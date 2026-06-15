# Recovery fixture for occ() + factor-model JSDM (imperfect-
# detection track). Mirrors jsdgam_mv_nmix.R but with the single-
# season Bernoulli-binomial occupancy likelihood, where the latent
# state is binary occupancy z[s, i] instead of count N[s, i]:
#
#   lv[i, :] = s(env[i], by = lv_axis())      # env-tracking factors
#                ↓ via Z[s, :]
#   logit_psi[s, i] = b_int[s] + Z %*% lv     # latent log-odds of
#                                             # occupancy
#                ↓ via per-(species, site) Bernoulli marginalisation
#   y[s, i, v] ~ Bernoulli(z[s, i] * p)       # observed detections
#                                             # (z marginalised out)
#
# Site count bumped to 50 (vs the 30 used by nmix). Single-bit
# detection per visit carries less information per closure unit than
# a Poisson count, so the recovery threshold on cor is lowered to
# 0.5, mirroring the categorical fixture's reasoning.
#
# Primary go/no-go: cor(off_diag(true_ZZ'), off_diag(post_ZZ')) > 0.5
# Diagnostics:
#   - MAE on Z Z' off-diagonals
#   - max|colSums(posterior_mean(Z))| (free Z under Heaps QR)
#   - posterior mean detection probability vs truth
#   - posterior mean occupancy probability vs truth
#   - divergent transitions + max-treedepth saturation
#   - min / max bulk ESS on Z entries
#
# Cached at /tmp/jsdgam_mv_occ_recovery_fit.rds. Delete to refit.
# Runtime ~5-8 min.

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(dplyr)
  library(posterior)
})

set.seed(607L)

K <- 4L
N_lv <- 2L
n_sites <- 50L
n_visits <- 3L
threshold_cor <- 0.5
species_levels <- paste0("sp", seq_len(K))

env <- sort(runif(n_sites, -2, 2))

lv_true <- cbind(sin(env), env^2 - mean(env^2))
lv_true <- scale(lv_true, center = TRUE, scale = apply(lv_true, 2L, sd))

Z_true <- matrix(rnorm(K * N_lv, sd = 1.0), nrow = K, ncol = N_lv)
Z_true <- scale(Z_true, center = TRUE, scale = FALSE)
attr(Z_true, "scaled:center") <- NULL

# Per-species occupancy intercept on the logit scale. Centred around
# 0 so each species occupies roughly half the sites on average.
b_int <- rnorm(K, mean = 0, sd = 0.5)

# Truth on the latent occupancy: logit_psi[s, i] = b_int[s] + Z %*% lv.
logit_psi <- matrix(NA_real_, nrow = K, ncol = n_sites)
for (s in seq_len(K)) {
  for (i in seq_len(n_sites)) {
    logit_psi[s, i] <- b_int[s] +
      sum(Z_true[s, ] * lv_true[i, ])
  }
}
psi_true <- 1 / (1 + exp(-logit_psi))
z_latent <- matrix(rbinom(K * n_sites, 1L, psi_true),
                   nrow = K, ncol = n_sites)

p_true <- 0.6
rows <- list()
for (s in seq_len(K)) {
  for (i in seq_len(n_sites)) {
    for (v in seq_len(n_visits)) {
      rows[[length(rows) + 1L]] <- data.frame(
        species = species_levels[s],
        site    = i,
        env     = env[i],
        visit   = v,
        y       = if (z_latent[s, i] == 1L) {
          rbinom(1L, 1L, p_true)
        } else {
          0L
        }
      )
    }
  }
}
dat <- do.call(rbind, rows)
dat$species <- factor(dat$species, levels = species_levels)

sigma_true_cov <- tcrossprod(Z_true)
sigma_true_cor <- cov2cor(sigma_true_cov + diag(1e-8, K))

cat("Simulated", nrow(dat), "rows: K =", K, "species,",
    n_sites, "sites,", n_visits, "visits.\n")
cat("Closure units = species x sites =", K * n_sites, ".\n")
cat("Mean true psi =", round(mean(psi_true), 3),
    "| naive detection rate =",
    round(mean(dat$y), 3), ".\n")
cat("True cor off-diag range: [",
    round(min(sigma_true_cor[upper.tri(sigma_true_cor)]), 3), ",",
    round(max(sigma_true_cor[upper.tri(sigma_true_cor)]), 3), "].\n")

cache <- "/tmp/jsdgam_mv_occ_recovery_fit.rds"
if (file.exists(cache)) {
  cat("[cache] Loading occ recovery fit.\n")
  fit <- readRDS(cache)
} else {
  cat("[fit ] jsdgam(occ(), n_lv = 2, env-driven factors,",
      n_sites, "sites)\n", sep = " ")
  fit <- jsdgam(
    formula        = y ~ species,
    factor_formula = ~ s(env, by = lv_axis(), k = 5) - 1,
    data           = dat,
    unit           = site, species = species,
    family         = occ(),
    n_lv           = N_lv,
    chains         = 2L, parallel = TRUE,
    burnin         = 500L, samples = 500L,
    silent         = 2,
    backend        = "cmdstanr"
  )
  saveRDS(fit, cache)
}

cat("\n=== Primary recovery (cor on Z Z' off-diagonals) ===\n")
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
cat("(Free Z under Heaps QR; small but not pinned to 0.)\n")

cat("\n=== Detection probability recovery ===\n")
p_cols <- grep("^b_p_Intercept$|^Intercept_p$|^p$", colnames(draws),
               value = TRUE)
if (length(p_cols) > 0L) {
  p_post <- as.numeric(draws[, p_cols[1L]])
  p_post_resp <- if (grepl("^b_", p_cols[1L]) ||
                       grepl("^Intercept", p_cols[1L])) {
    1 / (1 + exp(-p_post))
  } else {
    p_post
  }
  cat(sprintf("p posterior mean = %.3f  (truth = %.2f)\n",
              mean(p_post_resp), p_true))
} else {
  cat("WARN: detection probability column not located in posterior.\n")
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
