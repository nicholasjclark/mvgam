# Build the cached fits consumed by
# `vignettes/articles/jsdgam.Rmd`.
#
# Run once locally with:
#   Rscript tests/local/jsdgam_vignette_fits.R
#
# The article reads the RDS files unconditionally when present;
# regenerate this cache whenever a relevant API or default shifts.

suppressPackageStartupMessages({
  devtools::load_all(quiet = TRUE)
  library(mvgam)
  library(mvabund)
})

cache_dir <- file.path("pkgdown", "jsdgam_cache")
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
}

# Load the Australian ant assemblage data:
# 30 sites x 41 species counts + 5 environmental covariates +
# 5-column species trait table.
data(antTraits, package = "mvabund")

abund  <- antTraits$abund
env    <- antTraits$env
traits <- antTraits$traits

# Coerce Pilosity (0/1/2/3) and Polymorphism (0/1/2) ordered
# factors to numeric so the Heaps ARD kernel sees them as
# ordered continuous traits.
traits$Pilosity     <- as.numeric(as.character(traits$Pilosity))
traits$Polymorphism <- as.numeric(as.character(traits$Polymorphism))

# Reshape to long form: one row per site x species cell, with
# environment columns repeated across species rows. mvgam needs
# `unit` (site) and `species` columns plus the response and
# numeric-time index.
n_sites   <- nrow(abund)
n_species <- ncol(abund)
species_names <- colnames(abund)

ant_long <- data.frame(
  site    = factor(rep(seq_len(n_sites), times = n_species)),
  species = factor(rep(species_names, each = n_sites),
                   levels = species_names),
  time    = rep(seq_len(n_sites), times = n_species),
  counts  = as.integer(unlist(abund))
)
ant_long <- cbind(ant_long, env[as.integer(ant_long$site), ])

# Standardise the environmental covariates so the linear
# coefficients are on comparable scales.
env_cols <- colnames(env)
for (col in env_cols) {
  ant_long[[col]] <- as.numeric(scale(ant_long[[col]]))
}

ant_data <- list(
  long          = ant_long,
  traits        = traits,
  env_cols      = env_cols,
  species_names = species_names
)
saveRDS(ant_data, file.path(cache_dir, "ant_data.rds"))

# Matches the argument spelling the article displays, so the code
# shown beside each fit is the code that produced it.
CHAINS <- 2L
ITER   <- 1600L
WARMUP <- 1000L
SILENT <- 2L


# ---- mod_baseline: uninformed loadings + NB ----------------------------

cat("Fitting mod_baseline ...\n")
mod_baseline <- jsdgam(
  formula = counts ~ Bare.ground + Canopy.cover + Shrub.cover +
    Volume.lying.CWD + Feral.mammal.dung,
  factor_formula = ~ -1,
  data    = ant_long,
  family  = negbinomial(),
  n_lv    = 2L,
  unit    = time,
  species = species,
  chains  = CHAINS,
  iter    = ITER,
  warmup  = WARMUP,
  silent  = SILENT
)
saveRDS(mod_baseline, file.path(cache_dir, "mod_baseline.rds"))


# ---- mod_traits: Heaps trait-informed loadings + NB --------------------
# adapt_delta = 0.95 tightens the leapfrog step under the ARD trait
# kernel; the 41-species x 5-trait prior geometry is funnel-shaped
# and the Stan default (0.8) produced ~20% divergent transitions
# during initial cache builds.

cat("Fitting mod_traits ...\n")
mod_traits <- jsdgam(
  formula = counts ~ Bare.ground + Canopy.cover + Shrub.cover +
    Volume.lying.CWD + Feral.mammal.dung,
  factor_formula = ~ -1,
  data    = ant_long,
  family  = negbinomial(),
  n_lv    = 2L,
  traits  = traits,
  unit    = time,
  species = species,
  chains  = CHAINS,
  iter    = ITER,
  warmup  = WARMUP,
  adapt_delta = 0.95,
  silent  = SILENT
)
saveRDS(mod_traits, file.path(cache_dir, "mod_traits.rds"))


# ---- mod_mgp: MGP column shrinkage on the trait-informed prior ---------
# Combines the multiplicative-gamma-process column shrinkage with the
# trait ARD kernel by passing both inside a single `loadings_prior`
# list spec (the `traits =` alias and an explicit `loadings_prior` are
# mutually exclusive entry points).

cat("Fitting mod_mgp ...\n")
mod_mgp <- jsdgam(
  formula = counts ~ Bare.ground + Canopy.cover + Shrub.cover +
    Volume.lying.CWD + Feral.mammal.dung,
  factor_formula = ~ -1,
  data    = ant_long,
  family  = negbinomial(),
  n_lv    = 10L,
  loadings_prior = list(column_shrinkage = "mgp", features = traits),
  unit    = time,
  species = species,
  chains  = CHAINS,
  iter    = ITER,
  warmup  = WARMUP,
  silent  = SILENT
)
saveRDS(mod_mgp, file.path(cache_dir, "mod_mgp.rds"))

cat("\nAll fits cached to ", cache_dir, "\n", sep = "")
