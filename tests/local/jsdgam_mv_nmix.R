# Recovery fixture for nmix() + factor-model JSDM (imperfect-detection
# track). Exercises the three-level hierarchy that motivates the
# closure-unit + Heaps QR composition:
#
#   lv[i, :] = s(env[i], by = lv_axis())      # env-tracking factors
#                ↓ via Z[s, :]
#   log_lambda[s, i] = b_int[s] + Z %*% lv    # latent log-abundance
#                ↓ via per-(species, site) Poisson marginalisation
#   y[s, i, v] ~ Binomial(N[s, i], p)         # observed counts (p is
#                                             # the detection probability
#                                             # on its own dpar)
#
# Each closure unit is one (species, site) combination; the unit
# carries n_visits replicate observations. The mvgam trend pipeline
# adds `Z[s, :] %*% lv[i, :]` to the latent log-lambda for that
# closure unit; the family's lpmf marginalises N analytically.
#
# Primary go/no-go: cor(off_diag(true_ZZ'), off_diag(post_ZZ')) > 0.7
# Diagnostics:
#   - MAE on Z Z' off-diagonals
#   - max|colSums(posterior_mean(Z))| (free Z under Heaps QR; should
#     be SMALL but not pinned to 0 because nmix has no simplex
#     identifiability that requires sum_to_zero_vector)
#   - posterior mean detection probability vs truth
#   - divergent transitions + max-treedepth saturation
#   - min / max bulk ESS on Z entries
#
# Cached at /tmp/jsdgam_mv_nmix_recovery_fit.rds. Delete to refit.
# Runtime ~5-10 min (nmix marginalisation loops over K_max per unit).

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(dplyr)
  library(posterior)
})

set.seed(606L)

K <- 4L
N_lv <- 2L
n_sites <- 30L
n_visits <- 3L
threshold_cor <- 0.7
species_levels <- paste0("sp", seq_len(K))

# Per-site environmental covariate. The latent factors are driven
# by smooth functions of env; each factor is one basis dimension.
env <- sort(runif(n_sites, -2, 2))

# Truth on lv: two orthogonal smooth shapes (sin and quadratic) so
# the factor model can distinguish them on env. Standardise each
# column so Z entries live on a comparable scale.
lv_true <- cbind(sin(env), env^2 - mean(env^2))
lv_true <- scale(lv_true, center = TRUE, scale = apply(lv_true, 2L, sd))

# True species loadings on each factor. Column-centring gives the
# QR-identified form the model targets. Z magnitude (sd = 0.4) keeps
# log_lambda excursions modest so the per-closure-unit truncation
# window stays small (and the lpmf marginalisation cheap).
Z_true <- matrix(rnorm(K * N_lv, sd = 0.4), nrow = K, ncol = N_lv)
Z_true <- scale(Z_true, center = TRUE, scale = FALSE)
attr(Z_true, "scaled:center") <- NULL

# True per-species log-abundance intercept. Mean 0.5 puts baseline
# lambda around exp(0.5) ~ 1.6, with peaks around exp(2) ~ 7 once
# the env-driven factor contribution is added in. The empirical
# max N_latent then sits in the low 20s, keeping possible_N
# (= K_max - max(y_visits)) small enough to fit in 5-10 min.
b_int <- rnorm(K, mean = 0.5, sd = 0.3)

# Truth on the latent state: log_lambda[s, i] = b_int[s] + Z[s, :]
# %*% lv[i, :]. Draw N[s, i] from Poisson.
log_lambda <- matrix(NA_real_, nrow = K, ncol = n_sites)
for (s in seq_len(K)) {
  for (i in seq_len(n_sites)) {
    log_lambda[s, i] <- b_int[s] +
      sum(Z_true[s, ] * lv_true[i, ])
  }
}
lambda_true <- exp(log_lambda)
N_latent <- matrix(rpois(K * n_sites, lambda_true),
                   nrow = K, ncol = n_sites)
cap_true <- max(N_latent) + 5L

# True detection probability (scalar). Observed counts per visit.
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
        y       = rbinom(1L, N_latent[s, i], p_true),
        cap     = cap_true
      )
    }
  }
}
dat <- do.call(rbind, rows)
dat$species <- factor(dat$species, levels = species_levels)

# Recovery target: cor(true Z Z' off-diag, post Z Z' off-diag).
sigma_true_cov <- tcrossprod(Z_true)
sigma_true_cor <- cov2cor(sigma_true_cov + diag(1e-8, K))

cat("Simulated", nrow(dat), "rows: K =", K, "species,",
    n_sites, "sites,", n_visits, "visits.\n")
cat("Closure units = species x sites =", K * n_sites, ".\n")
cat("True N_latent range: [", min(N_latent), ",",
    max(N_latent), "]. cap =", cap_true, ".\n")
cat("True cor off-diag range: [",
    round(min(sigma_true_cor[upper.tri(sigma_true_cor)]), 3), ",",
    round(max(sigma_true_cor[upper.tri(sigma_true_cor)]), 3), "].\n")

cache <- "/tmp/jsdgam_mv_nmix_recovery_fit.rds"
if (file.exists(cache)) {
  cat("[cache] Loading nmix recovery fit.\n")
  fit <- readRDS(cache)
} else {
  cat("[fit ] jsdgam(nmix(), n_lv = 2, env-driven factors,",
      n_sites, "sites)\n", sep = " ")
  fit <- jsdgam(
    formula        = y ~ species,
    factor_formula = ~ s(env, by = lv_axis(), k = 5) - 1,
    data           = dat,
    unit           = site, species = species,
    family         = nmix(),
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
  # nmix p is on the logit scale when emitted via b_p_*; on the
  # response scale when stored as the scalar `p`.
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
