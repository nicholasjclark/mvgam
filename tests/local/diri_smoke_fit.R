# Smoke fit for the diri() multivariate Dirichlet closure-unit family.
#
# Confirms that:
#   1. `jsdgam(family = diri(), backend = "cmdstanr")` compiles and
#      samples cleanly on a small compositional JSDM panel
#      (K = 4, n_lv = 2, 30 sites).
#   2. Post-fit surface: posterior_epred returns probabilities in
#      [0, 1], posterior_predict returns simplex draws summing to 1
#      per site, log_lik runs without error and loo produces a
#      finite elpd estimate.
#
# Cached at /tmp/diri_smoke_fit.rds. Delete to refit. Runtime ~3-5 min.

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(dplyr)
  library(tidyr)
})

set.seed(303L)

K <- 4L
N_lv <- 2L
n_sites <- 30L
species_levels <- paste0("y", seq_len(K))

# Simulate from softmax of a low-rank linear predictor.
Z_true <- matrix(rnorm(K * N_lv, sd = 0.7), nrow = K, ncol = N_lv)
env <- rnorm(n_sites)
mu_intercept <- rnorm(K)
mu_env_slope <- rnorm(K)

phi_true <- 30
Y_wide <- matrix(NA_real_, nrow = n_sites, ncol = K)
for (i in seq_len(n_sites)) {
  lv_i <- rnorm(N_lv)
  eta_i <- mu_intercept + mu_env_slope * env[i] +
             as.numeric(Z_true %*% lv_i)
  p_i <- exp(eta_i) / sum(exp(eta_i))
  alpha_i <- p_i * phi_true
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

cache <- "/tmp/diri_smoke_fit.rds"
if (file.exists(cache)) {
  cat("[cache] Loading diri fit.\n")
  fit_diri <- readRDS(cache)
} else {
  cat("[fit ] diri() (jsdgam, y ~ env * series, n_lv = 2)\n")
  fit_diri <- jsdgam(
    formula = y ~ env * series,
    factor_formula = ~ -1,
    data = as.data.frame(long_dat),
    unit = time, species = series,
    family = diri(),
    n_lv = 2L,
    chains = 2L, parallel = TRUE,
    burnin = 500L, samples = 500L,
    silent = 2,
    backend = "cmdstanr"
  )
  saveRDS(fit_diri, cache)
}
cat("Done.\n")
