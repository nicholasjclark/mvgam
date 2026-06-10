# Local fixture: jsdgam vs gllvm concordance on two compositional
# JSDM scenarios.
#   Experiment 1: Beta-distributed proportions (univariate Beta per
#     (site, species)). Both gllvm (family = "beta") and jsdgam
#     (family = Beta()) fit naturally.
#   Experiment 2: Dirichlet-distributed compositions (each site is a
#     vector summing to 1 across species). gllvm has no Dirichlet
#     family, so we use its "beta" family fitted to each marginal
#     proportion independently. jsdgam attempts a brms::dirichlet()
#     route. At present this is not part of the jsdgam wrapper's
#     vetted path, so the script captures whatever it does (fit,
#     error, or warning) for a clean post-mortem.

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(dplyr)
  library(tidyr)
  if (!requireNamespace("gllvm", quietly = TRUE)) {
    stop("Install gllvm before running this concordance fixture.")
  }
})

set.seed(2026L)

# Shared simulation knobs ---------------------------------------------------
n_sites <- 60L
n_species <- 8L
n_lv_true <- 2L
env <- rnorm(n_sites)
beta_true <- runif(n_species, -0.6, 0.6)
intercept_true <- runif(n_species, -0.2, 0.2)
Z_true <- matrix(rnorm(n_species * n_lv_true, sd = 0.5),
                 nrow = n_species, ncol = n_lv_true)
for (k in seq_len(n_lv_true)) {
  if (Z_true[k, k] < 0) Z_true[, k] <- -Z_true[, k]
}
lv_true <- matrix(rnorm(n_sites * n_lv_true), nrow = n_sites,
                  ncol = n_lv_true)
eta <- outer(rep(1, n_sites), intercept_true) +
  env %o% beta_true +
  lv_true %*% t(Z_true)

# ==========================================================================
# Experiment 1: Beta-distributed proportions
# ==========================================================================
cat("\n############# Experiment 1: Beta JSDM #############\n")
mu_beta <- plogis(eta)            # logistic link to (0, 1)
phi_beta <- 8                     # concentration
a_beta <- mu_beta * phi_beta
b_beta <- (1 - mu_beta) * phi_beta
Y_beta <- matrix(
  pmin(pmax(rbeta(n_sites * n_species, a_beta, b_beta), 1e-4), 1 - 1e-4),
  nrow = n_sites, ncol = n_species
)
colnames(Y_beta) <- paste0("sp", seq_len(n_species))

dat_beta <- tibble::as_tibble(Y_beta) %>%
  dplyr::mutate(site = seq_len(n_sites), env = env) %>%
  tidyr::pivot_longer(
    cols = dplyr::starts_with("sp"),
    names_to = "species", values_to = "y"
  ) %>%
  dplyr::mutate(species = factor(species))

cat("\n--- gllvm (family = beta) ---\n")
t_beta_gllvm <- system.time({
  mod_beta_gllvm <- gllvm::gllvm(
    y = Y_beta, X = data.frame(env = env), formula = ~ env,
    family = "beta",
    num.lv = n_lv_true, method = "EVA",
    seed = 1L
  )
})
cat("Elapsed:", round(t_beta_gllvm[["elapsed"]], 1), "s\n")
gllvm_beta_rcor <- gllvm::getResidualCor(mod_beta_gllvm)
rownames(gllvm_beta_rcor) <- colnames(gllvm_beta_rcor) <- colnames(Y_beta)

cat("\n--- jsdgam (family = Beta()) ---\n")
t_beta_jsdgam <- system.time(
  mod_beta_jsdgam <- jsdgam(
    formula = y ~ env,
    factor_formula = ~ -1,
    data = dat_beta, unit = site, species = species,
    family = Beta(), n_lv = n_lv_true,
    chains = 2L, parallel = TRUE,
    burnin = 200L, samples = 400L,
    silent = 2
  )
)
cat("Elapsed:", round(t_beta_jsdgam[["elapsed"]], 1), "s\n")
jsdgam_beta_rcor <- residual_cor(mod_beta_jsdgam)$cor

off_diag <- upper.tri(gllvm_beta_rcor, diag = FALSE)
beta_cor <- cor(
  gllvm_beta_rcor[off_diag],
  jsdgam_beta_rcor[colnames(Y_beta), colnames(Y_beta)][off_diag]
)
beta_mad <- mean(abs(
  gllvm_beta_rcor[off_diag] -
    jsdgam_beta_rcor[colnames(Y_beta), colnames(Y_beta)][off_diag]
))
cat("\nBeta off-diag cor(gllvm, jsdgam):", round(beta_cor, 3), "\n")
cat("Beta mean abs diff:              ", round(beta_mad, 3), "\n")

# ==========================================================================
# Experiment 2: Dirichlet-distributed compositions
# ==========================================================================
cat("\n############# Experiment 2: Dirichlet JSDM #############\n")
alpha_dir <- exp(eta - rowMeans(eta))   # positive concentrations
# Each row of Y_dir is a Dirichlet draw summing to 1.
Y_dir <- t(apply(alpha_dir, 1L, function(a) {
  s <- rgamma(length(a), shape = a, rate = 1)
  pmax(s / sum(s), 1e-5)
}))
Y_dir <- Y_dir / rowSums(Y_dir)
colnames(Y_dir) <- paste0("sp", seq_len(n_species))

cat("\n--- gllvm (family = beta on each marginal) ---\n")
# Note: applies the Beta likelihood marginally; the cross-species
# sum-to-1 constraint is not modelled. This is the closest gllvm
# can come to a Dirichlet JSDM.
Y_dir_clipped <- pmin(pmax(Y_dir, 1e-4), 1 - 1e-4)
t_dir_gllvm <- system.time({
  mod_dir_gllvm <- gllvm::gllvm(
    y = Y_dir_clipped, X = data.frame(env = env), formula = ~ env,
    family = "beta",
    num.lv = n_lv_true, method = "EVA",
    seed = 1L
  )
})
cat("Elapsed:", round(t_dir_gllvm[["elapsed"]], 1), "s\n")
gllvm_dir_rcor <- gllvm::getResidualCor(mod_dir_gllvm)
rownames(gllvm_dir_rcor) <- colnames(gllvm_dir_rcor) <- colnames(Y_dir)

cat("\n--- jsdgam (family = brms::dirichlet) ---\n")
# brms::dirichlet expects a multivariate (cbind-LHS) response. The
# jsdgam wrapper currently forwards long-form (one row per (site,
# species)). Capture whatever happens so we can decide whether to
# add a dirichlet-aware path in chunk 2+.
dir_wide <- tibble::as_tibble(Y_dir) %>%
  dplyr::mutate(site = seq_len(n_sites), env = env)
res_dir_jsdgam <- tryCatch(
  {
    t_dir_jsdgam <- system.time(
      mod_dir_jsdgam <- jsdgam(
        formula = as.formula(
          paste0(
            "cbind(",
            paste(colnames(Y_dir), collapse = ", "),
            ") ~ env"
          )
        ),
        factor_formula = ~ -1,
        data = dir_wide, unit = site, species = NULL,
        family = brms::dirichlet(), n_lv = n_lv_true,
        chains = 2L, burnin = 100L, samples = 200L, silent = 2
      )
    )
    list(ok = TRUE, mod = mod_dir_jsdgam, t = t_dir_jsdgam)
  },
  error = function(e) list(ok = FALSE, err = conditionMessage(e))
)
if (isTRUE(res_dir_jsdgam$ok)) {
  cat("Elapsed:", round(res_dir_jsdgam$t[["elapsed"]], 1), "s (jsdgam OK)\n")
} else {
  cat("jsdgam dirichlet path errored:\n  ",
      strsplit(res_dir_jsdgam$err, "\n")[[1L]][1L], "\n")
}

# ==========================================================================
# Save + summary
# ==========================================================================
out <- list(
  beta = list(
    Y = Y_beta, gllvm = mod_beta_gllvm, jsdgam = mod_beta_jsdgam,
    t_gllvm = t_beta_gllvm, t_jsdgam = t_beta_jsdgam,
    cor_agreement = beta_cor, mean_abs_diff = beta_mad
  ),
  dirichlet = list(
    Y = Y_dir, gllvm = mod_dir_gllvm,
    t_gllvm = t_dir_gllvm,
    jsdgam = res_dir_jsdgam
  )
)
dir.create("/tmp/chunk1_concordance", showWarnings = FALSE)
saveRDS(out, "/tmp/chunk1_concordance/gllvm_vs_jsdgam.rds")
cat("\nSaved to /tmp/chunk1_concordance/gllvm_vs_jsdgam.rds\n")
