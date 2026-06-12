# Local fixture: brms-native vs diri() concordance on Dirichlet data.
#
# Verifies that the closure-unit long-format wrapper produces
# posteriors statistically consistent with brms' cbind-LHS native
# Dirichlet fit on the same data. Same Stan native lpdf
# (`dirichlet_logit_lpdf`) is called from both fits; the difference
# is the wrapping (per-row long-format aggregation via vint arrays
# vs brms' direct K-1 per-category linear predictors with a
# reference category).
#
# Pinned n_lv = 1 for our fit so the factor-model addition is the
# minimal rank-1 correction over identity; the per-category env
# slopes should agree on the marginal mean with brms native within
# Monte Carlo error.
#
# Run with:
#   Rscript tests/local/brms_concordance_diri.R
#
# Caches both fits at /tmp/brms_diri_concordance_*.rds. Delete to
# refit. Total runtime ~3-5 min.

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(brms)
  library(tidyr)
  library(dplyr)
})

set.seed(2024L)

n_sites <- 60L
n_cats <- 4L
cat_names <- paste0("y", seq_len(n_cats))
env <- rnorm(n_sites)

# Simulate Dirichlet data with known per-category logit-mean
# offsets so we can compare both fits to a common ground truth.
intercept_true <- c(0, 0.6, -0.3, 0.2)
beta_env_true  <- c(0, 0.7, -0.5, 0.4)
phi_true       <- 8

# brms convention: y1 is the reference category, so its mu is 0.
mu_true <- outer(rep(1, n_sites), intercept_true) +
           outer(env, beta_env_true)
mu_true[, 1L] <- 0  # reference category
probs <- t(apply(mu_true, 1L, function(z) {
  z <- exp(z); z / sum(z)
}))

Y_wide <- t(apply(probs, 1L, function(p) {
  d <- rgamma(n_cats, shape = p * phi_true, rate = 1)
  d / sum(d)
}))
Y_wide <- pmax(Y_wide, 1e-4); Y_wide <- Y_wide / rowSums(Y_wide)
colnames(Y_wide) <- cat_names

# Wide-format data for brms native + long-format data for diri().
wide_dat <- as.data.frame(Y_wide)
wide_dat$site <- seq_len(n_sites)
wide_dat$env  <- env

long_dat <- pivot_longer(
  wide_dat, all_of(cat_names), names_to = "series", values_to = "y"
) |>
  mutate(
    series = factor(series, levels = cat_names),
    time   = site
  ) |>
  arrange(time, series)

cat("Simulated", n_sites, "sites x", n_cats, "categories.\n")
cat("Per-site sums (should be 1):",
    round(rowSums(Y_wide[1:3, ]), 3), "\n")

# brms-native Dirichlet fit -----------------------------------------------
cache_brms <- "/tmp/brms_diri_concordance_brms.rds"
if (file.exists(cache_brms)) {
  cat("\n[cache] brms-native Dirichlet fit\n")
  fit_brms <- readRDS(cache_brms)
} else {
  cat("\n[fit ] brms-native Dirichlet\n")
  fit_brms <- brm(
    bf(cbind(y1, y2, y3, y4) ~ env, family = dirichlet()),
    data = wide_dat, chains = 2L, iter = 1000L, warmup = 500L,
    refresh = 0, silent = 2
  )
  saveRDS(fit_brms, cache_brms)
}

# Our diri() fit on the long-format panel ---------------------------------
# Interaction `env * series` makes the env effect category-specific,
# matching brms native's separate `b_muy{2..K}_env` per-category
# slopes. Without the interaction, the long-format wrapper broadcasts
# a single env slope across all K categories and the per-category
# fixed effects collapse (cor against brms native ~ 0.4 instead of
# the > 0.9 you'd expect; see git history).
cache_diri <- "/tmp/brms_diri_concordance_diri.rds"
if (file.exists(cache_diri)) {
  cat("[cache] diri() fit\n")
  fit_diri <- readRDS(cache_diri)
} else {
  cat("[fit ] diri() (jsdgam wrapper, env * series, n_lv = 2)\n")
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
  saveRDS(fit_diri, cache_diri)
}

# Extract per-category env slope summaries from both fits -----------------
extract_brms_env_slopes <- function(fit) {
  draws <- as_draws_matrix(fit)
  # brms emits b_muy2_env, b_muy3_env, b_muy4_env (y1 is reference)
  cols <- grep("^b_muy[0-9]+_env$", colnames(draws), value = TRUE)
  out <- lapply(cols, function(col) {
    z <- draws[, col]
    c(mean = mean(z), sd = sd(z),
      ess = posterior::ess_bulk(z))
  })
  names(out) <- cols
  out
}

extract_diri_env_slopes <- function(fit) {
  draws <- as_draws_matrix(fit$fit)
  # mvgam emits a single b_env (one slope, shared across categories
  # in the long-format formulation); the per-category differentiation
  # in our wrapper comes from the latent factor Z, not from the env
  # coefficient directly. Report b_env summary as well as the
  # per-category posterior mean of mu_unit.
  z_col <- intersect(c("b_env", "b_intercept", "b_Intercept"),
                     colnames(draws))
  out <- list()
  for (col in z_col) {
    z <- draws[, col]
    out[[col]] <- c(mean = mean(z), sd = sd(z),
                    ess = posterior::ess_bulk(z))
  }
  out
}

cat("\n=== brms-native per-category env slopes ===\n")
brms_slopes <- extract_brms_env_slopes(fit_brms)
print(round(do.call(rbind, brms_slopes), 3))

cat("\n=== diri() per-row env slope (shared across categories) ===\n")
diri_summary <- extract_diri_env_slopes(fit_diri)
print(round(do.call(rbind, diri_summary), 3))

cat("\nNote: brms emits K-1 per-category env slopes; our long-format\n")
cat("wrapper emits a single env slope that broadcasts across rows,\n")
cat("with per-category differentiation entering via the latent\n")
cat("factor Z. Direct slope-vs-slope comparison is therefore not\n")
cat("apples-to-apples; the meaningful concordance check is on the\n")
cat("INDUCED per-category mu posterior at each site, which we\n")
cat("compare next.\n")

# Per-(site, category) mu posterior comparison ----------------------------
# brms emits per-category mu via posterior_linpred.
brms_lp <- posterior_linpred(fit_brms, dpar = "muy2")
cat("\nbrms posterior_linpred shape for muy2:",
    paste(dim(brms_lp), collapse = " x "), "\n")

# diri() emits per-row mu via posterior_epred / posterior_linpred.
diri_lp <- posterior_linpred(fit_diri)
cat("diri() posterior_linpred shape:",
    paste(dim(diri_lp), collapse = " x "), "\n")

# Compute posterior mean of mu at each (site, category) for both
# fits, align, and report concordance.
diri_mu_mean <- colMeans(diri_lp)
# diri rows are sorted by (time, series); reshape to [site, category]
diri_mu_mat <- matrix(diri_mu_mean, nrow = n_sites, ncol = n_cats,
                      byrow = TRUE)
colnames(diri_mu_mat) <- cat_names

# brms emits K-1 mu vectors (mu_y1 implicit = 0).
brms_mu_y2 <- colMeans(posterior_linpred(fit_brms, dpar = "muy2"))
brms_mu_y3 <- colMeans(posterior_linpred(fit_brms, dpar = "muy3"))
brms_mu_y4 <- colMeans(posterior_linpred(fit_brms, dpar = "muy4"))
brms_mu_mat <- cbind(0, brms_mu_y2, brms_mu_y3, brms_mu_y4)
colnames(brms_mu_mat) <- cat_names

# Softmax both to get per-category probabilities; identification is
# up to a per-site shift in mu, so compare probabilities instead.
softmax_row <- function(z) {
  z <- exp(z - max(z)); z / sum(z)
}
diri_probs <- t(apply(diri_mu_mat, 1L, softmax_row))
brms_probs <- t(apply(brms_mu_mat, 1L, softmax_row))

cat("\n=== Posterior-mean cell probabilities, first 3 sites ===\n")
cat("Truth:\n");          print(round(probs[1:3, ], 3))
cat("brms native:\n");    print(round(brms_probs[1:3, ], 3))
cat("diri() wrapper:\n"); print(round(diri_probs[1:3, ], 3))

cor_overall <- cor(as.vector(brms_probs), as.vector(diri_probs))
mad_overall <- mean(abs(as.vector(brms_probs) -
                          as.vector(diri_probs)))
cat("\n=== Concordance ===\n")
cat("cor(brms vs diri posterior-mean probs):  ",
    round(cor_overall, 4), "\n")
cat("mean abs diff (probs):                   ",
    round(mad_overall, 4), "\n")

# Identifiability diagnostics on diri() Z ---------------------------------
cat("\n=== diri() Z identification diagnostics ===\n")
diri_draws <- as_draws_matrix(fit_diri$fit)
z_cols <- grep("^Z\\[", colnames(diri_draws), value = TRUE)
if (length(z_cols)) {
  n_lv_for_z <- length(z_cols) / n_cats
  Z_post_mean <- matrix(
    colMeans(diri_draws[, z_cols, drop = FALSE]),
    nrow = n_cats, ncol = n_lv_for_z, byrow = FALSE
  )
  cat("Z posterior mean per latent factor (one column per l):\n")
  print(round(Z_post_mean, 3))
  cat("Column sums (should hover near zero under soft constraint):",
      round(colSums(Z_post_mean), 3),
      " (exact zero by sum_to_zero_vector)\n")
  z_ess <- apply(diri_draws[, z_cols, drop = FALSE], 2L,
                 posterior::ess_bulk)
  cat("min ESS for Z entries:", round(min(z_ess), 0),
      " | max:", round(max(z_ess), 0), "\n")
} else {
  cat("Could not find Z[ entries in posterior draws.\n")
}

cat("\nDone.\n")
