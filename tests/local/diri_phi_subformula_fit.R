# Smoke fit exercising per-row `phi ~ ...` distributional regression
# under diri(), confirming per-row phi extraction in
# `posterior_predict()` / `log_lik()` / `predict(type = "variance")`.
#
# Response-scale calls compose the per-row phi linpred via the dpar
# pipeline + log inverse link, collapse per unit to `phi[idx[1]]` to
# mirror the Stan lpdf, and produce finite `[ndraws x N_obs]` outputs
# aligned with the per-row mu linpred.
#
# Cached at tests/local/fixtures/val_mvgam_diri_phi_subformula.rds.
# Delete to refit. Runtime ~3-5 min.

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

set.seed(404L)

K <- 4L
N_lv <- 2L
n_sites <- 30L
species_levels <- paste0("y", seq_len(K))

# Simulate from softmax + per-site phi driven by env. The mean
# composition is constant per species; phi varies across sites so
# the Dirichlet concentration tightens / loosens with env.
Z_true <- matrix(rnorm(K * N_lv, sd = 0.7), nrow = K, ncol = N_lv)
env <- rnorm(n_sites)
mu_intercept <- rnorm(K)
mu_env_slope <- rnorm(K)

phi_intercept <- 3.4    # exp(3.4) ~ 30 baseline concentration
phi_env_slope <- 1.0    # phi sweeps about 10x across env range

Y_wide <- matrix(NA_real_, nrow = n_sites, ncol = K)
for (i in seq_len(n_sites)) {
  lv_i  <- rnorm(N_lv)
  eta_i <- mu_intercept + mu_env_slope * env[i] +
             as.numeric(Z_true %*% lv_i)
  p_i   <- exp(eta_i) / sum(exp(eta_i))
  phi_i <- exp(phi_intercept + phi_env_slope * env[i])
  alpha_i <- p_i * phi_i
  gam <- rgamma(K, shape = alpha_i, rate = 1)
  Y_wide[i, ] <- gam / sum(gam)
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

# The generative truth rides on the saved fit so a separate test can
# assert recovery against it without repeating the simulation.
sim_truth <- list(
  K = K, N_lv = N_lv, species_levels = species_levels,
  Z_true = Z_true,
  phi_intercept = phi_intercept, phi_env_slope = phi_env_slope,
  mu_intercept = mu_intercept, mu_env_slope = mu_env_slope, env = env
)

cache <- local_fixture_path("val_mvgam_diri_phi_subformula.rds")
if (file.exists(cache)) {
  cat("[cache] Loading diri phi-subformula fit.\n")
  fit <- readRDS(cache)
} else {
  cat("[fit ] diri(), bf(y ~ env * series, phi ~ env)\n")
  fit <- jsdgam(
    formula = brms::bf(y ~ env * series, phi ~ env),
    factor_formula = ~ -1,
    data    = as.data.frame(long_dat),
    unit    = time,
    species = series,
    family  = diri(),
    n_lv    = 2L,
    chains  = 2L,
    warmup  = 500L,
    iter    = 1000L,
    silent  = 2,
    backend = "cmdstanr"
  )
}
if (!identical(attr(fit, "sim_truth"), sim_truth)) {
  attr(fit, "sim_truth") <- sim_truth
  saveRDS(fit, cache)
}

cat("\n=== Posterior coefficient recovery on phi ~ env ===\n")
draws <- as_draws_matrix(fit$fit)
phi_cols <- grep("^b_phi_", colnames(draws), value = TRUE)
cat("phi coefficient columns:", paste(phi_cols, collapse = ", "), "\n")
if (length(phi_cols) > 0) {
  phi_means <- round(colMeans(draws[, phi_cols, drop = FALSE]), 3)
  print(phi_means)
}
cat(sprintf(
  "Truth: phi_intercept = %.2f, phi_env_slope = %.2f\n",
  phi_intercept, phi_env_slope
))

cat("\n=== posterior_epred() ===\n")
pe <- posterior_epred(fit)
cat("shape :", paste(dim(pe), collapse = " x "), "\n")
cat("finite:", all(is.finite(pe)), "\n")
# Per-site row sum should be 1.
arrays <- mvgam:::build_closure_unit_arrays(
  fit$obs_data, response_var = "y",
  compute_y_max = FALSE, unit_grouping_vars = "time"
)
g_sum <- vapply(seq_len(arrays$N_unit), function(g) {
  idx <- arrays$visit_idx[g, seq_len(arrays$n_rep[g])]
  mean(rowSums(pe[, idx, drop = FALSE]))
}, numeric(1L))
cat(sprintf(
  "per-site epred row-sum (mean across sites): %.4f (should be ~1.0000)\n",
  mean(g_sum)
))

cat("\n=== posterior_predict() ===\n")
pp <- posterior_predict(fit)
cat("shape :", paste(dim(pp), collapse = " x "), "\n")
cat("finite:", all(is.finite(pp)), "\n")
cat("range :", round(range(pp), 4), "\n")

cat("\n=== log_lik() ===\n")
ll <- log_lik(fit)
cat("shape       :", paste(dim(ll), collapse = " x "), "\n")
cat("finite (idx[1] rows):", all(is.finite(ll[, ll[1, ] != 0])), "\n")
cat("sum of mean log-lik (joint per-site density):",
    round(sum(colMeans(ll)), 2), "\n")

cat("\n=== predict(type = 'variance') ===\n")
pv <- predict(fit, type = "variance")
cat("shape :", paste(dim(pv), collapse = " x "), "\n")
cat("finite:", all(is.finite(pv)), "\n")
cat("range :", round(range(pv), 6), "\n")
# Per-site rows in the same unit should share a single phi[idx[1]],
# so var = p(1-p) / (phi + 1) varies only through p across the unit.
# Check: ratio of var at row k to var at row idx[1] should equal
# p_k(1-p_k) / p_1(1-p_1).
example_g <- 1L
idx_ex <- arrays$visit_idx[example_g, seq_len(arrays$n_rep[example_g])]
var_ratio_emp <- pv[1, idx_ex] / pv[1, idx_ex[1L]]
p_ratio <- (pe[1, idx_ex] * (1 - pe[1, idx_ex])) /
            (pe[1, idx_ex[1L]] * (1 - pe[1, idx_ex[1L]]))
cat("max abs(var_ratio - p_ratio) for unit 1, draw 1:",
    round(max(abs(var_ratio_emp - p_ratio)), 6), "\n")

cat("\n=== loo() ===\n")
loo_out <- loo(fit)
print(loo_out)

cat("\nDone.\n")
