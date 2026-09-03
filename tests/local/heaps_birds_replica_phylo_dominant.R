# Local fixture: Heaps & Jermyn (2024) Sect. 6.2 variation where the
# phylogenetic kernel drives Z's row covariance and the trait kernel
# is essentially uninformative. Responses are continuous (gaussian)
# so per-cell information content is at its ceiling.
#
# Kernel orientation. Under the multiplicative Phi structure
# Phi(i, j) = exp(-d_trait(i, j) / theta_trait) *
#             exp(-d_phylo(i, j) / theta_phylo)
# a SMALLER theta gives a TIGHTER, more strongly structured kernel
# (fast decay across distance). For phylo to "dominate" Z, its theta
# must be the smaller of the two. The sim uses theta_phylo = 0.3
# (meaningful decay across phylo distance) and theta_trait = 0.9
# (kernel near 1 throughout [0, 1] body-mass-distance range).
#
# Truth-level diagnostic. Before fitting, the simulation reports
# cor(Phi_off_diag, K_phylo_off_diag) and cor(Phi_off_diag,
# K_trait_off_diag) so the dominance is visible in the data
# generating process itself (the first should be much larger).
#
# What we check after the fit:
#   1. The structured prior emission still parses and samples.
#   2. theta_phylo posterior < theta_trait posterior with high
#      probability (qualitative dominance recovered).
#   3. The recovered residual correlation Z Z' off-diagonals
#      correlate more strongly with K_phylo at the posterior-mean
#      theta_phylo than with K_trait at the posterior-mean
#      theta_trait, mirroring the truth-level diagnostic.
#
# Run with:
#   Rscript tests/local/heaps_birds_replica_phylo_dominant.R
# Caches the fit at
# tests/local/fixtures/val_mvgam_heaps_birds_phylo_dominant.rds;
# delete to refit. Runtime ~10-15 minutes for two chains.

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(ape)
})

# testthat sets the working directory to tests/local/ when it runs a
# file, while Rscript runs it from the package root. Reach the shared
# fixture helpers by whichever of the two paths exists.
source(if (file.exists("concordance_helpers.R")) {
  "concordance_helpers.R"
} else {
  file.path("tests", "local", "concordance_helpers.R")
})

cache_path <- local_fixture_path(
  "val_mvgam_heaps_birds_phylo_dominant.rds"
)

set.seed(2024L)

n_species <- 30L
n_sites <- 100L
n_lv_true <- 3L

species_levels <- paste0("sp", sprintf("%02d", seq_len(n_species)))

# 1. Trait + phylogeny ----------------------------------------------------
body_mass <- rnorm(n_species)
body_mass <- (body_mass - mean(body_mass)) / sd(body_mass)
trait_df <- data.frame(
  series = species_levels,
  body_mass = body_mass
)

tree <- ape::rcoal(n = n_species, tip.label = species_levels)
tree$edge.length <- tree$edge.length /
  max(diag(ape::vcv.phylo(tree, model = "Brownian")))

# 2. Asymmetric Phi kernel: phylo length-scale is 3x trait length-scale.
phylo_dist <- ape::cophenetic.phylo(tree)
phylo_dist <- phylo_dist / max(phylo_dist)
trait_dist <- as.matrix(stats::dist(body_mass))
trait_dist <- trait_dist / max(trait_dist)

# Kernel parameters. Under multiplicative Phi = K_trait * K_phylo with
# K(d) = exp(-d/theta), a SMALLER theta means a TIGHTER, more strongly
# structured kernel. To make phylogeny "dominate" Z's row covariance,
# theta_phylo must be SMALLER than theta_trait (so phylo's term in
# -log(Phi) varies more across pairs than trait's). Setting
# theta_trait_true = 0.9 makes K_trait near 1 across the [0, 1]
# distance range (almost uninformative), while theta_phylo_true = 0.3
# gives K_phylo a meaningful decay that drives Z's row similarity.
theta_trait_true <- 0.9
theta_phylo_true <- 0.3
K_trait_true <- exp(-trait_dist / theta_trait_true)
K_phylo_true <- exp(-phylo_dist / theta_phylo_true)
Phi <- K_trait_true * K_phylo_true
Phi <- Phi + diag(1e-6, n_species)
L_Phi <- chol(Phi)

# Truth-level dominance diagnostic: compare how strongly each
# marginal kernel correlates with Phi off-diagonals. A "phylo
# dominates" simulation should have cor(Phi_off, K_phylo_off) much
# higher than cor(Phi_off, K_trait_off).
off_idx <- upper.tri(Phi, diag = FALSE)
cor_phi_phylo_truth <- stats::cor(Phi[off_idx], K_phylo_true[off_idx])
cor_phi_trait_truth <- stats::cor(Phi[off_idx], K_trait_true[off_idx])
cat("Truth-level kernel-vs-Phi correlations (off-diagonals):\n")
cat("  cor(Phi, K_phylo) =", round(cor_phi_phylo_truth, 3), "\n")
cat("  cor(Phi, K_trait) =", round(cor_phi_trait_truth, 3), "\n")
cat("  phylo / trait ratio of correlations =",
    round(cor_phi_phylo_truth / cor_phi_trait_truth, 2), "\n\n")

Z_true <- t(L_Phi) %*% matrix(
  rnorm(n_species * n_lv_true), n_species, n_lv_true
)
lv_true <- matrix(rnorm(n_sites * n_lv_true), n_sites, n_lv_true)
eta <- lv_true %*% t(Z_true)
# Gaussian observation noise: small enough that the kernel signal
# dominates but large enough that the model has something to fit.
sigma_obs_true <- 0.5
Y <- eta + matrix(
  rnorm(n_sites * n_species, mean = 0, sd = sigma_obs_true),
  nrow = n_sites, ncol = n_species
)
colnames(Y) <- species_levels

# 3. Long-form panel ------------------------------------------------------
dat <- data.frame(
  series = factor(rep(species_levels, each = n_sites),
                  levels = species_levels),
  time = rep(seq_len(n_sites), n_species),
  y = as.numeric(as.vector(Y))
)

cat("Simulated", n_sites, "sites x", n_species, "species. y range: [",
    round(min(dat$y), 2), ",", round(max(dat$y), 2), "].\n")
cat("True theta_trait =", theta_trait_true,
    "| true theta_phylo =", theta_phylo_true, "\n")
cat("Smaller theta = tighter kernel. theta_phylo <<",
    "theta_trait so phylo carries the row structure.\n")

# The generative truth rides on the saved fit so a separate test can
# assert recovery against it without repeating the simulation.
sim_truth <- list(
  n_species = n_species, n_sites = n_sites, n_lv_true = n_lv_true,
  species_levels = species_levels, body_mass = body_mass, tree = tree,
  theta_trait_true = theta_trait_true,
  theta_phylo_true = theta_phylo_true,
  K_trait_true = K_trait_true, K_phylo_true = K_phylo_true,
  Phi = Phi, Z_true = Z_true, lv_true = lv_true,
  sigma_obs_true = sigma_obs_true
)

# 4. Fit jsdgam with the traits + phylo aliases ---------------------------
if (file.exists(cache_path)) {
  cat("Loading cached fit from", cache_path, "\n")
  fit <- readRDS(cache_path)
} else {
  cat("Fitting jsdgam with traits + phylo aliases ...\n")
  fit <- jsdgam(
    formula = y ~ 1,
    factor_formula = ~ -1,
    data = dat, unit = time, species = series,
    family = gaussian(),
    n_lv = n_lv_true,
    traits = trait_df,
    phylo = tree,
    priors = c(
      brms::prior("normal(0, 3.162)", class = "theta_features"),
      brms::prior("normal(0, 3.162)", class = "theta_dist_phylo")
    ),
    chains = 2L,
    burnin = 500L, samples = 500L,
    silent = 2
  )
}
if (!identical(attr(fit, "sim_truth"), sim_truth)) {
  attr(fit, "sim_truth") <- sim_truth
  saveRDS(fit, cache_path)
  cat("Saved fit to", cache_path, "\n")
}

# 5. Posterior summaries --------------------------------------------------
post <- as.matrix(
  fit$fit, pars = c("theta_features", "theta_dist_phylo")
)
theta_trait_post <- post[, "theta_features[1]"]
theta_phylo_post <- post[, "theta_dist_phylo"]

cat("\n=== Posterior length-scale recovery ===\n")
cmp_df <- data.frame(
  parameter = c("theta_trait", "theta_phylo"),
  posterior_mean = c(
    round(mean(theta_trait_post), 3),
    round(mean(theta_phylo_post), 3)
  ),
  posterior_median = c(
    round(median(theta_trait_post), 3),
    round(median(theta_phylo_post), 3)
  ),
  posterior_q025 = c(
    round(quantile(theta_trait_post, 0.025), 3),
    round(quantile(theta_phylo_post, 0.025), 3)
  ),
  posterior_q975 = c(
    round(quantile(theta_trait_post, 0.975), 3),
    round(quantile(theta_phylo_post, 0.975), 3)
  ),
  true_value = c(theta_trait_true, theta_phylo_true)
)
print(cmp_df)

# Phylo "dominates" => phylo kernel is tighter => theta_phylo < theta_trait.
phylo_dominates <- mean(theta_phylo_post < theta_trait_post)
cat("\nP(theta_phylo < theta_trait | data) =",
    round(phylo_dominates, 3),
    " (truth-direction recovery)\n")

# 6. Structural correlation diagnostic. Compare how strongly the
# inferred Z Z' off-diagonals correlate with the marginal phylo
# kernel at the posterior-mean theta_phylo vs the marginal trait
# kernel at the posterior-mean theta_trait. If the model picked up
# the phylo dominance, the phylo correlation should clearly exceed
# the trait correlation, mirroring the truth-level diagnostic
# computed before fitting.
rcor <- residual_cor(fit)
ZZ_off <- rcor$cor[off_idx]
K_phylo_post <- exp(-phylo_dist / mean(theta_phylo_post))
K_trait_post <- exp(-trait_dist / mean(theta_trait_post))
cor_zz_phylo_post <- stats::cor(ZZ_off, K_phylo_post[off_idx])
cor_zz_trait_post <- stats::cor(ZZ_off, K_trait_post[off_idx])
cat("\n=== Residual correlation (Z Z') diagnostics ===\n")
cat("  median off-diagonal cor =", round(median(ZZ_off), 3), "\n")
cat("  cor(Z Z' off, K_phylo at posterior theta) =",
    round(cor_zz_phylo_post, 3), "\n")
cat("  cor(Z Z' off, K_trait at posterior theta) =",
    round(cor_zz_trait_post, 3), "\n")
cat("  phylo / trait ratio of posterior correlations =",
    round(cor_zz_phylo_post / cor_zz_trait_post, 2),
    " (truth-level was ",
    round(cor_phi_phylo_truth / cor_phi_trait_truth, 2), ")\n",
    sep = "")

cat("\nPhylo-dominant Heaps replica completed.\n")
