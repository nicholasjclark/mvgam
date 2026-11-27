# Helpers shared by test-jsdgam.R and test-extra-jsdgam.R.
# Tests for the `jsdgam()` wrapper:
#   1. Argument validation: 'unit' / 'species' / 'n_lv' / 'data'.
#   2. What a prefit carries: class, unit and species columns, program.
#   3. The traits and phylo aliases for the loadings prior.
#   4. Arguments the wrapper refuses.

# Utility -----------------------------------------------------------------

build_jsdgam_toy <- function(n_time = 24L, n_species = 4L, seed = 42L) {
  set.seed(seed)
  dat <- expand.grid(
    time = seq_len(n_time),
    species = paste0("sp", seq_len(n_species))
  )
  dat$species <- factor(dat$species)
  dat$elev <- rep(rnorm(n_time), times = n_species)
  dat$y <- rpois(nrow(dat), lambda = 2)
  dat
}

# 3. traits + phylo aliases -----------------------------------------------
#
# The alias compiler, then the Stan data it gives through
# standata.mvgam_formula().

species_4 <- function() paste0("sp", 1:4)
