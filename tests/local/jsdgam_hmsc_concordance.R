# Local fixture: jsdgam vs Hmsc concordance on a small Bernoulli JSDM.
#
# Runs a side-by-side fit of the same simulated presence/absence
# panel (n_species species at n_sites sites, with one site-level
# covariate and a known true loading matrix Z) under:
#   (a) jsdgam with binomial() (Bernoulli for 0/1 responses) +
#       n_lv latent factors.
#   (b) Hmsc with probit link + n_lv latent factors.
# Reports residual-correlation agreement, loading-magnitude
# agreement, and elapsed fit times.
#
# This is a local diagnostic, NOT a CI test. The user runs it
# manually to track concordance against the field standard as the
# jsdgam wrapper evolves.

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(dplyr)
  library(tidyr)
  if (!requireNamespace("Hmsc", quietly = TRUE)) {
    stop("Install Hmsc before running this concordance fixture.")
  }
})

set.seed(2026L)

# 1. Simulate Bernoulli JSDM ------------------------------------------------
n_sites <- 60L
n_species <- 8L
n_lv_true <- 2L

env <- rnorm(n_sites)
beta_true <- runif(n_species, -0.6, 0.6)
intercept_true <- runif(n_species, -0.2, 0.2)

# Free Z with positive diagonal so the column signs are pinned for
# concordance comparisons.
Z_true <- matrix(rnorm(n_species * n_lv_true, sd = 0.7),
                 nrow = n_species, ncol = n_lv_true)
for (k in seq_len(n_lv_true)) {
  if (Z_true[k, k] < 0) Z_true[, k] <- -Z_true[, k]
}
lv_true <- matrix(rnorm(n_sites * n_lv_true), nrow = n_sites,
                  ncol = n_lv_true)
eta <- outer(rep(1, n_sites), intercept_true) +
  env %o% beta_true +
  lv_true %*% t(Z_true)
p <- pnorm(eta)
Y <- matrix(rbinom(n_sites * n_species, size = 1L, prob = p),
            nrow = n_sites, ncol = n_species)
colnames(Y) <- paste0("sp", seq_len(n_species))

# Long form for jsdgam.
dat <- tibble::as_tibble(Y) %>%
  dplyr::mutate(site = seq_len(n_sites), env = env) %>%
  tidyr::pivot_longer(
    cols = dplyr::starts_with("sp"),
    names_to = "species", values_to = "y"
  ) %>%
  dplyr::mutate(species = factor(species))

# 2. Hmsc fit --------------------------------------------------------------
cat("\n=== Hmsc fit ===\n")
XData <- data.frame(env = env)
hmsc_studyDesign <- data.frame(site = factor(seq_len(n_sites)))
hmsc_rL <- Hmsc::HmscRandomLevel(units = hmsc_studyDesign$site)

mod_hmsc <- Hmsc::Hmsc(
  Y = Y, XData = XData, XFormula = ~ env,
  distr = "probit",
  studyDesign = hmsc_studyDesign,
  ranLevels = list(site = hmsc_rL)
)
t_hmsc <- system.time(
  mod_hmsc <- Hmsc::sampleMcmc(
    mod_hmsc, thin = 1L, samples = 400L, transient = 200L,
    nChains = 2L, verbose = 0L
  )
)
cat("Hmsc elapsed:", round(t_hmsc[["elapsed"]], 1), "s\n")

# 3. jsdgam fit ------------------------------------------------------------
cat("\n=== jsdgam fit ===\n")
t_jsdgam <- system.time(
  mod_jsdgam <- jsdgam(
    formula = y ~ env,
    factor_formula = ~ -1,
    data = dat, unit = site, species = species,
    family = bernoulli(), n_lv = n_lv_true,
    chains = 2L, parallel = TRUE,
    burnin = 200L, samples = 400L,
    silent = 2
  )
)
cat("jsdgam elapsed:", round(t_jsdgam[["elapsed"]], 1), "s\n")

# 4. Residual correlation comparison --------------------------------------
hmsc_post <- Hmsc::computeAssociations(mod_hmsc)
hmsc_rcor <- hmsc_post[[1L]]$mean  # site-level association

jsdgam_rcor_obj <- residual_cor(mod_jsdgam)
jsdgam_rcor <- jsdgam_rcor_obj$cor

# Match species ordering.
sp_levels <- paste0("sp", seq_len(n_species))
hmsc_rcor <- hmsc_rcor[sp_levels, sp_levels]
jsdgam_rcor <- jsdgam_rcor[sp_levels, sp_levels]

off_diag <- upper.tri(hmsc_rcor, diag = FALSE)
cor_agreement <- cor(hmsc_rcor[off_diag], jsdgam_rcor[off_diag])
cat("\n=== Residual correlation concordance ===\n")
cat("Off-diagonal cor(Hmsc, jsdgam):",
    round(cor_agreement, 3), "\n")
cat("Mean abs diff (off-diag):",
    round(mean(abs(hmsc_rcor[off_diag] - jsdgam_rcor[off_diag])), 3),
    "\n")

# 5. Summary table --------------------------------------------------------
summary_df <- data.frame(
  metric = c(
    "elapsed_sec_hmsc", "elapsed_sec_jsdgam",
    "off_diag_cor_hmsc_vs_jsdgam",
    "mean_abs_diff_hmsc_vs_jsdgam"
  ),
  value = c(
    round(t_hmsc[["elapsed"]], 1),
    round(t_jsdgam[["elapsed"]], 1),
    round(cor_agreement, 3),
    round(mean(abs(hmsc_rcor[off_diag] - jsdgam_rcor[off_diag])),
          3)
  )
)
print(summary_df)

dir.create("/tmp/chunk1_concordance", showWarnings = FALSE)
saveRDS(
  list(
    hmsc = mod_hmsc, jsdgam = mod_jsdgam,
    Y = Y, env = env, Z_true = Z_true, lv_true = lv_true,
    t_hmsc = t_hmsc, t_jsdgam = t_jsdgam,
    cor_agreement = cor_agreement
  ),
  "/tmp/chunk1_concordance/hmsc_vs_jsdgam.rds"
)
cat("\nSaved to /tmp/chunk1_concordance/hmsc_vs_jsdgam.rds\n")
