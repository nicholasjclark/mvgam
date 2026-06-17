# Local fixture: jsdgam trait_slopes vs Hmsc fourth-corner Gamma.
#
# Closes the verification gap for #324 P3 by checking that the
# trait-on-slope coefficient mvgam recovers via the bf(nl = TRUE)
# rewrite agrees with the canonical Hmsc::Hmsc(TrFormula = ...) fit
# on the same data. Gaussian responses keep the link simple so the
# Gamma matrix indices are directly comparable.
#
# Hmsc stores the fourth-corner regression in a (n_traits + 1) x
# (n_env + 1) Gamma matrix:
#   Gamma[1, 1] : intercept-of-intercept (baseline intercept)
#   Gamma[2, 1] : trait1 effect on species intercept
#   Gamma[1, 2] : baseline env slope (mean across species)
#   Gamma[2, 2] : trait1 effect on env slope -- the fourth corner
#
# The jsdgam rewrite under trait_slopes = ~ trait1 emits two nlpars
#   a   (species intercept)
#   b1  (species slope on env)
# each regressed on `trait1 + (1 | sp | series)`. The brms parameter
# aliases used here mirror the Hmsc slots:
#   b_a_Intercept   <-> Gamma[1, 1]
#   b_a_trait1      <-> Gamma[2, 1]
#   b_b1_Intercept  <-> Gamma[1, 2]
#   b_b1_trait1     <-> Gamma[2, 2]
#
# The fixture is not a CI test (it requires Hmsc + a Stan compile);
# it is run manually to track concordance as the wrapper evolves.

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(brms)
  if (!requireNamespace("Hmsc", quietly = TRUE)) {
    stop("Install Hmsc before running this concordance fixture.")
  }
})

FIXTURE_DIR <- "tests/local/fixtures"
if (!dir.exists(FIXTURE_DIR)) {
  dir.create(FIXTURE_DIR, recursive = TRUE)
}

# 1. Simulate fourth-corner data ------------------------------------
set.seed(20260618L)
n_sites   <- 50L
n_species <- 8L

env    <- rnorm(n_sites)
trait1 <- scale(rnorm(n_species))[, 1]

gamma_intercept_baseline <- 0.4
gamma_trait1_on_intercept <- 0.25
gamma_slope_baseline     <- 0.3
gamma_trait1_on_slope    <- 0.7
sigma_alpha              <- 0.15
sigma_beta               <- 0.15

species_alpha <- gamma_intercept_baseline +
  gamma_trait1_on_intercept * trait1 +
  rnorm(n_species, 0, sigma_alpha)
species_beta  <- gamma_slope_baseline +
  gamma_trait1_on_slope * trait1 +
  rnorm(n_species, 0, sigma_beta)

Y <- matrix(NA_real_, n_sites, n_species)
for (j in seq_len(n_species)) {
  Y[, j] <- species_alpha[j] + species_beta[j] * env +
    rnorm(n_sites, 0, 0.3)
}
colnames(Y) <- paste0("sp", seq_len(n_species))

# 2. Hmsc fit ------------------------------------------------------
hmsc_path <- file.path(FIXTURE_DIR, "val_hmsc_trait.rds")
if (file.exists(hmsc_path)) {
  cat("Cached Hmsc fit.\n")
  mod_hmsc <- readRDS(hmsc_path)
} else {
  cat("=== Hmsc fit ===\n")
  XData  <- data.frame(env = env)
  TrData <- data.frame(trait1 = trait1)
  rownames(TrData) <- colnames(Y)
  t_hmsc <- system.time({
    mod_hmsc <- Hmsc::Hmsc(
      Y = Y, XData = XData, XFormula = ~ env,
      TrData = TrData, TrFormula = ~ trait1,
      distr = "normal"
    )
    mod_hmsc <- Hmsc::sampleMcmc(
      mod_hmsc, thin = 1L, samples = 500L, transient = 500L,
      nChains = 2L, verbose = 0L
    )
  })
  cat("Hmsc elapsed:", round(t_hmsc[["elapsed"]], 1), "s\n")
  saveRDS(mod_hmsc, hmsc_path)
}

# 3. jsdgam fit ----------------------------------------------------
jsdgam_path <- file.path(FIXTURE_DIR, "val_jsdgam_trait.rds")
if (file.exists(jsdgam_path)) {
  cat("Cached jsdgam fit.\n")
  mod_jsdgam <- readRDS(jsdgam_path)
} else {
  cat("=== jsdgam fit ===\n")
  trait_df <- data.frame(
    species = factor(colnames(Y)),
    trait1  = trait1
  )
  long_df <- expand.grid(
    site    = seq_len(n_sites),
    species = factor(colnames(Y))
  )
  long_df$env <- env[long_df$site]
  long_df <- merge(long_df, trait_df, by = "species")
  long_df$y <- Y[cbind(long_df$site, as.integer(long_df$species))]
  t_jsdgam <- system.time(
    mod_jsdgam <- jsdgam(
      formula        = y ~ env,
      factor_formula = ~ -1,
      data           = long_df,
      unit           = site, species = species,
      family         = gaussian(), n_lv = 2L,
      trait_slopes   = ~ trait1,
      chains         = 2L, samples = 500L, burnin = 500L,
      silent         = 2, backend = "cmdstanr", seed = 42L
    )
  )
  cat("jsdgam elapsed:", round(t_jsdgam[["elapsed"]], 1), "s\n")
  saveRDS(mod_jsdgam, jsdgam_path)
}

# 4. Extract gamma posteriors --------------------------------------
hmsc_post <- Hmsc::getPostEstimate(mod_hmsc, parName = "Gamma",
                                    q = c(0.025, 0.5, 0.975))
hmsc_mean <- hmsc_post$mean
rownames(hmsc_mean) <- c("Intercept_trait", "trait1")
colnames(hmsc_mean) <- c("Intercept_env", "env")
cat("\n=== Hmsc Gamma posterior mean ===\n")
print(round(hmsc_mean, 3))

mvgam_draws <- posterior::as_draws_df(mod_jsdgam$fit)
alias_map <- mvgam:::mvgam_beta_aliases(mod_jsdgam)
target_aliases <- c("b_a_Intercept", "b_a_trait1",
                    "b_b1_Intercept", "b_b1_trait1")
mvgam_draws_aliased <- list()
for (al in target_aliases) {
  raw <- alias_map[[al]]
  if (is.null(raw) || !(raw %in% names(mvgam_draws))) {
    stop(sprintf("alias %s -> %s missing from mvgam draws", al, raw))
  }
  mvgam_draws_aliased[[al]] <- mvgam_draws[[raw]]
}
mvgam_means <- vapply(mvgam_draws_aliased, mean,
                       FUN.VALUE = numeric(1L))
cat("\n=== mvgam aliased posterior means ===\n")
print(round(mvgam_means, 3))

# 5. Concordance summary -------------------------------------------
truth <- c(
  b_a_Intercept  = gamma_intercept_baseline,
  b_a_trait1     = gamma_trait1_on_intercept,
  b_b1_Intercept = gamma_slope_baseline,
  b_b1_trait1    = gamma_trait1_on_slope
)
hmsc_means <- c(
  b_a_Intercept  = hmsc_mean["Intercept_trait", "Intercept_env"],
  b_a_trait1     = hmsc_mean["trait1",           "Intercept_env"],
  b_b1_Intercept = hmsc_mean["Intercept_trait", "env"],
  b_b1_trait1    = hmsc_mean["trait1",           "env"]
)
summary_df <- data.frame(
  parameter   = names(truth),
  truth       = round(truth, 3),
  hmsc        = round(hmsc_means[names(truth)], 3),
  mvgam       = round(mvgam_means[names(truth)], 3)
)
summary_df$sign_match <- with(
  summary_df, sign(hmsc) == sign(mvgam)
)
summary_df$ratio <- with(summary_df, mvgam / hmsc)
cat("\n=== Fourth-corner concordance ===\n")
print(summary_df)

cat("\nGamma[2, 2] (the fourth-corner term):\n")
fc <- summary_df[summary_df$parameter == "b_b1_trait1", ]
cat(sprintf(
  "  truth = %.3f | Hmsc = %.3f | mvgam = %.3f\n",
  fc$truth, fc$hmsc, fc$mvgam
))

# 6. Species random-slope agreement --------------------------------
# Hmsc per-species beta: rows are env coefs, cols are species. Pull
# the env row (row 2 since row 1 is the intercept) so we compare the
# beta_j slopes.
hmsc_beta_post <- Hmsc::getPostEstimate(
  mod_hmsc, parName = "Beta", q = c(0.025, 0.5, 0.975)
)
hmsc_species_slopes <- hmsc_beta_post$mean[2L, ]
names(hmsc_species_slopes) <- colnames(Y)

mvgam_ranef_alias <- mvgam:::mvgam_ranef_aliases(mod_jsdgam)
mvgam_species_slope_alias <- grep(
  "^r_series__b1\\[", names(mvgam_ranef_alias), value = TRUE
)
if (length(mvgam_species_slope_alias) == 0L) {
  stop("Could not locate mvgam species-slope aliases.")
}
mvgam_species_slope_raw <- mvgam_ranef_alias[mvgam_species_slope_alias]
mvgam_species_slope_deviations <- vapply(
  mvgam_species_slope_raw,
  function(par) mean(mvgam_draws[[par]]),
  FUN.VALUE = numeric(1L)
)
# Convert deviations to absolute slopes: gamma0 + gamma1 * trait1 +
# species deviation. Use mvgam's own gamma posterior means.
mvgam_species_slopes <- mvgam_means["b_b1_Intercept"] +
  mvgam_means["b_b1_trait1"] * trait1 +
  mvgam_species_slope_deviations
names(mvgam_species_slopes) <- colnames(Y)

slope_cor <- stats::cor(
  hmsc_species_slopes[colnames(Y)],
  mvgam_species_slopes[colnames(Y)]
)
cat(sprintf(
  "\nSpecies-slope cor(Hmsc, mvgam) = %.3f\n", slope_cor
))
cat("Per-species slopes (Hmsc | mvgam | truth):\n")
print(round(data.frame(
  hmsc  = hmsc_species_slopes[colnames(Y)],
  mvgam = mvgam_species_slopes[colnames(Y)],
  truth = species_beta
), 3))

# 7. Hard assertions -----------------------------------------------
stopifnot(
  "fourth-corner sign mismatch" =
    sign(mvgam_means["b_b1_trait1"]) ==
      sign(hmsc_means["b_b1_trait1"]),
  "fourth-corner ratio out of bounds" =
    abs(mvgam_means["b_b1_trait1"] /
          hmsc_means["b_b1_trait1"] - 1) < 0.5,
  "species-slope cor below 0.85" = slope_cor > 0.85
)
cat("\nAll concordance assertions passed.\n")
