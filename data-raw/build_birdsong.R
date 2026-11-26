# Build script for data/birdsong.rda.
#
# Generative structure: 4 bird species x 80 weeks of Poisson
# observations with hierarchically correlated AR(1) trends. Each
# weekly count is generated from a log-linear predictor combining:
#   - a species random intercept (group mean log-abundance);
#   - a shared cyclic seasonal smooth on week within year;
#   - species-specific AR(1) trends drawn from a multivariate
#     normal innovation distribution. Two species (warbler, thrush)
#     are positively coupled; two (wren, finch) move opposite to
#     them. The 4x4 innovation correlation matrix is the
#     hierarchical structure.
#
# An independent-AR fit treats the four trajectories as unrelated
# noise. `trend_model = AR(p = 1, cor = TRUE)` recovers the cross-
# species correlation matrix, exposing the coupling.

devtools::load_all()

set.seed(20260604L)

n_species <- 4L
n_weeks <- 80L
species_labels <- c("warbler", "thrush", "wren", "finch")

# Long format: one row per (species, week).
grid <- expand.grid(
  week = seq_len(n_weeks),
  species = factor(species_labels, levels = species_labels)
)
grid <- grid[order(grid$species, grid$week), ]
grid$week_in_year <- ((grid$week - 1L) %% 52L) + 1L

# Species random intercepts (log scale), centred so the package
# fit's intercept absorbs the grand mean.
sp_int <- stats::rnorm(n_species, mean = 0, sd = 0.3)
sp_int <- sp_int - mean(sp_int)
names(sp_int) <- species_labels

# Shared seasonal smooth on week-in-year via a cyclic basis.
season_grid <- seq_len(52L)
season_sm <- sim_smooth(
  x = season_grid, k = 8L, bs = "cc", scale = 0.6
)
season_eff <- season_sm$f - mean(season_sm$f)

# Hierarchically correlated AR(1) trends. Innovation correlation
# matrix groups (warbler, thrush) and (wren, finch) into two
# opposing clusters.
rho <- 0.7
Sigma_corr <- matrix(
  c( 1,    rho, -rho, -rho,
     rho,  1,   -rho, -rho,
    -rho, -rho,  1,    rho,
    -rho, -rho,  rho,  1),
  nrow = n_species, byrow = TRUE
)
sigma <- 0.35
Sigma_innov <- (sigma^2) * Sigma_corr
phi <- 0.7

# Multivariate-normal innovations, one row per week.
innov <- rmvn(
  n = n_weeks,
  mu = rep(0, n_species),
  Sigma = Sigma_innov
)
# rmvn returns [n_series, n] when n_series > 1; transpose to
# [n_weeks, n_species] for the recursion.
if (nrow(innov) == n_species && ncol(innov) == n_weeks) {
  innov <- t(innov)
}

# Light directional drift to give two species net positive growth
# and two net negative growth.
drift <- c(warbler = 0.005, thrush = 0.004,
            wren = -0.004, finch = -0.005)

ar_trends <- matrix(0, nrow = n_weeks, ncol = n_species)
colnames(ar_trends) <- species_labels
prev <- rep(0, n_species)
for (t in seq_len(n_weeks)) {
  prev <- phi * prev + drift + innov[t, ]
  ar_trends[t, ] <- prev
}

# Assemble the linear predictor (log scale).
eta <- numeric(nrow(grid))
species_codes <- as.integer(grid$species)
for (i in seq_len(nrow(grid))) {
  sp <- species_codes[i]
  eta[i] <- 1.6 +
    sp_int[sp] +
    season_eff[grid$week_in_year[i]] +
    ar_trends[grid$week[i], sp]
}

# Sample Poisson counts.
y <- stats::rpois(length(eta), lambda = exp(eta))

birdsong <- data.frame(
  series = grid$species,
  time = as.integer(grid$week),
  y = as.integer(y),
  species = grid$species,
  week = as.integer(grid$week),
  week_in_year = as.integer(grid$week_in_year),
  count = as.integer(y)
)
rownames(birdsong) <- NULL

save(
  birdsong,
  file = "data/birdsong.rda",
  compress = "bzip2"
)

message("data/birdsong.rda written: ",
        nrow(birdsong), " rows across ",
        n_species, " species x ", n_weeks, " weeks.")
