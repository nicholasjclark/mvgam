# Local fixture: Heaps & Jermyn (2024) Section 6.2 near-replica.
#
# The published example fits a multivariate probit factor model on 50
# Finnish bird species at 22 spatial areas with:
#   - 1 continuous trait (log-standardised body mass)
#   - An ultrametric phylogeny (root-to-tip distance standardised to 1)
#   - A multiplicative Phi kernel = trait ARD x phylo distance kernel
# The bird dataset itself is not redistributed in the published code.
# This fixture simulates a near-replica with the same architecture
# (sizes / kernel / family / loadings_prior structure) so we can
# confirm that the jsdgam wrapper's `traits` + `phylo` aliases route
# through to the structured `loadings_prior` Stan emission and the
# fit returns interpretable structure.
#
# Run with:
#   Rscript tests/local/heaps_birds_replica.R
# Caches the fit at tests/local/fixtures/val_mvgam_heaps_birds.rds;
# delete to refit.

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

cache_path <- local_fixture_path("val_mvgam_heaps_birds.rds")

set.seed(2024L)

n_species <- 30L
n_sites <- 25L
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
# Standardise root-to-tip to 1 (matches Heaps Sect. 6.2 convention).
tree$edge.length <- tree$edge.length /
  max(diag(ape::vcv.phylo(tree, model = "Brownian")))

# 2. Simulate from the published-style multiplicative Phi kernel ----------
phylo_dist <- ape::cophenetic.phylo(tree)
phylo_dist <- phylo_dist / max(phylo_dist)        # max = 1 standardisation
trait_dist <- as.matrix(stats::dist(body_mass))
trait_dist <- trait_dist / max(trait_dist)

theta_trait_true <- 0.4
theta_phylo_true <- 0.4
Phi <- exp(-trait_dist / theta_trait_true) *
       exp(-phylo_dist / theta_phylo_true)
# Numerical stabilisation for the multivariate-normal Cholesky.
Phi <- Phi + diag(1e-6, n_species)
L_Phi <- chol(Phi)

# Sample n_lv columns of Z under the structured prior (matrix-normal
# with row-covariance Phi, column-covariance I, mean 0).
Z_true <- t(L_Phi) %*% matrix(
  rnorm(n_species * n_lv_true), n_species, n_lv_true
)
# Site-level latent factors and probit-linked Bernoulli.
lv_true <- matrix(rnorm(n_sites * n_lv_true), n_sites, n_lv_true)
eta <- lv_true %*% t(Z_true)
p <- pnorm(eta)
Y <- matrix(rbinom(n_sites * n_species, 1L, p),
            nrow = n_sites, ncol = n_species)
colnames(Y) <- species_levels

# 3. Long-form panel for jsdgam ------------------------------------------
dat <- data.frame(
  series = factor(rep(species_levels, each = n_sites),
                  levels = species_levels),
  time = rep(seq_len(n_sites), n_species),
  y = as.integer(as.vector(Y))
)

cat("Simulated", n_sites, "sites x", n_species, "species. Mean presence:",
    round(mean(dat$y), 3), "\n")

# The generative truth rides on the saved fits so a separate test can
# assert recovery against it without repeating the simulation.
sim_truth <- list(
  n_species = n_species, n_sites = n_sites, n_lv_true = n_lv_true,
  species_levels = species_levels, body_mass = body_mass, tree = tree,
  theta_trait_true = theta_trait_true,
  theta_phylo_true = theta_phylo_true,
  Phi = Phi, Z_true = Z_true, lv_true = lv_true
)

# 4. Fit jsdgam with the trait + phylo aliases ---------------------------
if (file.exists(cache_path)) {
  cat("Loading cached Heaps-style jsdgam fit from", cache_path, "\n")
  fit <- readRDS(cache_path)
} else {
  cat("Fitting jsdgam with traits + phylo aliases ...\n")
  fit <- jsdgam(
    formula = y ~ 1,
    factor_formula = ~ -1,
    data = dat, unit = time, species = series,
    family = bernoulli(),
    n_lv = n_lv_true,
    traits = trait_df,
    phylo = tree,
    chains = 2L,
    burnin = 400L, samples = 400L,
    silent = 2
  )
}
if (!identical(attr(fit, "sim_truth"), sim_truth)) {
  attr(fit, "sim_truth") <- sim_truth
  saveRDS(fit, cache_path)
  cat("Saved fit to", cache_path, "\n")
}

# 5. Audit the structured-prior Stan emission ----------------------------
sc <- as.character(fit$stancode)

stopifnot(grepl("theta_features", sc))
stopifnot(grepl("theta_dist_phylo", sc))
stopifnot(grepl("gp_exponential_cov", sc, fixed = TRUE))
stopifnot(grepl("multi_normal_cholesky", sc, fixed = TRUE))
cat("\n[OK] stancode contains the structured Phi kernel ",
    "(trait ARD x phylo distance).\n")

stopifnot("row_features" %in% names(fit$standata))
stopifnot("dist_phylo" %in% names(fit$standata))
cat("[OK] standata carries row_features (", nrow(fit$standata$row_features),
    " x ", ncol(fit$standata$row_features), ") and dist_phylo (",
    paste(dim(fit$standata$dist_phylo), collapse = " x "), ").\n",
    sep = "")

# 6. Posterior summaries ---------------------------------------------------
post <- as.matrix(fit$fit, pars = c("theta_features", "theta_dist_phylo"))
cat("\nPosterior mean theta_features =",
    round(mean(post[, "theta_features[1]"]), 3), "\n")
cat("Posterior mean theta_dist_phylo =",
    round(mean(post[, "theta_dist_phylo"]), 3), "\n")

# Residual correlation: implied species covariance Sigma = Z Z'.
rcor <- residual_cor(fit)
cat("\nResidual correlation diagnostic (off-diagonal summary):\n")
off_diag <- rcor$cor[upper.tri(rcor$cor, diag = FALSE)]
cat("  median off-diagonal cor =", round(median(off_diag), 3), "\n")
cat("  range of off-diagonal cor = [",
    round(min(off_diag), 3), ", ", round(max(off_diag), 3), "]\n",
    sep = "")

# Trait-explained share of variance via shared_variation().
sv <- shared_variation(fit)
cat("\nshared_variation() returned an object of class:",
    class(sv)[1L], "\n")
print(sv)

cat("\nHeaps Sect. 6.2 near-replica completed successfully.\n")

# 7. Full inherited surface audit on the structured-prior fit -------------
#
# Mirror the surface audit (tests/local/jsdgam_prediction_audit.R)
# on this fit to confirm the structured-loadings-prior path composes
# cleanly with summary / predict / posterior_epred / posterior_predict /
# log_lik / loo / print / ordinate / shared_variation /
# conditional_effects / pp_check.

cat("\n=== Surface audit on the structured-prior fit ===\n")
audit <- list()
safe_check <- function(name, expr) {
  out <- tryCatch(
    {
      val <- expr()
      list(ok = TRUE, val = val)
    },
    error = function(e) list(ok = FALSE, msg = conditionMessage(e))
  )
  audit[[name]] <<- list(
    surface = name, ok = out$ok,
    detail = if (out$ok) "ok" else out$msg
  )
}

safe_check("summary", function() {
  s <- suppressWarnings(summary(fit))
  TRUE
})
safe_check("print", function() {
  invisible(capture.output(print(fit)))
  TRUE
})
safe_check("predict_link", function() {
  lin <- predict(fit, type = "link")
  cat("  predict(link) dim:",
      paste(dim(lin), collapse = " x "), "\n")
  TRUE
})
safe_check("predict_response", function() {
  resp <- predict(fit, type = "response")
  cat("  predict(response) dim:",
      paste(dim(resp), collapse = " x "), "\n")
  TRUE
})
safe_check("posterior_epred", function() {
  ep <- posterior_epred(fit, ndraws = 30L)
  cat("  posterior_epred dim:",
      paste(dim(ep), collapse = " x "), "\n")
  TRUE
})
safe_check("posterior_predict", function() {
  pp <- posterior_predict(fit, ndraws = 30L)
  cat("  posterior_predict dim:",
      paste(dim(pp), collapse = " x "), "\n")
  TRUE
})
safe_check("log_lik", function() {
  ll <- log_lik(fit, ndraws = 30L)
  stopifnot(all(is.finite(ll)))
  cat("  log_lik dim:", paste(dim(ll), collapse = " x "), "\n")
  TRUE
})
safe_check("loo", function() {
  L <- suppressWarnings(loo(fit))
  cat("  loo elpd_loo:",
      round(L$estimates["elpd_loo", "Estimate"], 2), "\n")
  TRUE
})
safe_check("plot_smooth_or_factor", function() {
  p <- suppressWarnings(plot(fit, type = "factors"))
  stopifnot(inherits(p, "ggplot") || inherits(p, "patchwork"))
  TRUE
})
safe_check("ordinate", function() {
  p <- ordinate(fit, alpha = 0.7)
  stopifnot(inherits(p, "ggplot"))
  TRUE
})
safe_check("pp_check", function() {
  p <- suppressWarnings(pp_check(fit, ndraws = 30L))
  stopifnot(inherits(p, "ggplot"))
  TRUE
})
safe_check("conditional_effects", function() {
  ce <- suppressWarnings(conditional_effects(fit))
  cat("  conditional_effects class:", class(ce)[1L], "\n")
  TRUE
})

audit_df <- do.call(rbind, lapply(audit, function(x) {
  data.frame(
    surface = x$surface, ok = x$ok, detail = x$detail,
    stringsAsFactors = FALSE
  )
}))
rownames(audit_df) <- NULL
cat("\n=== Audit matrix (Heaps fit) ===\n")
print(audit_df)
cat("\nSummary:", sum(audit_df$ok), "/", nrow(audit_df),
    "surfaces passed on the Heaps-style fit.\n")

# 8. Heaps-style wide prior re-fit + length-scale comparison ------------
#
# The published paper uses log(theta) ~ N(0, sqrt(10)) on the
# inverse length-scales. mvgam's default is log(theta) ~ N(0, 1).
# Refit with the wider prior to give a fair length-scale comparison
# against the published Fig. 1a posterior densities.

cache_wide <- local_fixture_path("val_mvgam_heaps_birds_wide.rds")
if (file.exists(cache_wide)) {
  cat("\nLoading cached Heaps-prior fit from", cache_wide, "\n")
  fit_wide <- readRDS(cache_wide)
} else {
  cat("\nRefitting with Heaps-style log(theta) ~ N(0, sqrt(10)) prior\n")
  fit_wide <- jsdgam(
    formula = y ~ 1,
    factor_formula = ~ -1,
    data = dat, unit = time, species = series,
    family = bernoulli(),
    n_lv = n_lv_true,
    traits = trait_df,
    phylo = tree,
    priors = c(
      brms::prior("normal(0, 3.162)",
                  class = "theta_features"),
      brms::prior("normal(0, 3.162)",
                  class = "theta_dist_phylo")
    ),
    chains = 2L,
    burnin = 400L, samples = 400L,
    silent = 2
  )
}
if (!identical(attr(fit_wide, "sim_truth"), sim_truth)) {
  attr(fit_wide, "sim_truth") <- sim_truth
  saveRDS(fit_wide, cache_wide)
}

post_wide <- as.matrix(
  fit_wide$fit, pars = c("theta_features", "theta_dist_phylo")
)
cat("\nWith Heaps-style log(theta) ~ N(0, sqrt(10)) prior:\n")
cat("  theta_features posterior mean =",
    round(mean(post_wide[, "theta_features[1]"]), 3), "\n")
cat("  theta_dist_phylo posterior mean =",
    round(mean(post_wide[, "theta_dist_phylo"]), 3), "\n")

cat("\nLength-scale comparison (theta = 1 / inverse-length-scale):\n")
cmp_df <- data.frame(
  parameter = c("theta_features", "theta_dist_phylo"),
  default_prior_mean = c(
    round(mean(post[, "theta_features[1]"]), 3),
    round(mean(post[, "theta_dist_phylo"]), 3)
  ),
  heaps_prior_mean = c(
    round(mean(post_wide[, "theta_features[1]"]), 3),
    round(mean(post_wide[, "theta_dist_phylo"]), 3)
  ),
  true_value = c(theta_trait_true, theta_phylo_true)
)
print(cmp_df)
cat("\nPhylo length-scale smaller than trait length-scale =",
    cmp_df$heaps_prior_mean[2L] < cmp_df$heaps_prior_mean[1L], "\n")
cat("(Heaps Sect. 6.2.2: phylo length-scale has the smallest",
    " posterior median, indicating strongest spatial structure.)\n")
