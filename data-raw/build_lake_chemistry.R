# Build script for data/lake_chemistry.rda.
#
# Generative structure: 5 lakes x 60 monthly observations of a
# Gaussian chemistry measurement (notionally dissolved oxygen,
# centred and scaled). The dataset is state-space: a latent
# correlated random-walk drives the slow trend, the observation
# equation adds a binary treatment effect.
#
#   latent[t, lake] = latent[t-1, lake] + innov[t, lake]
#   innov[t, ] ~ MVN(0, Sigma)            # correlated across lakes
#   y[t, lake] = mu0 + lake_int[lake]
#              + latent[t, lake]
#              + treat_slope[lake] * treated[t]
#              + N(0, sigma_obs)
#
# Sigma encodes regional structure: two pairs of nearby lakes share
# positive innovation correlation, a fifth lake is independent.
# Treatment effect is heterogeneous (2 lakes positive, 3 negative).
#
# Recommended fit (state-space):
#   mvgam(
#     formula = y ~ treated,
#     trend_formula = ~ 1,
#     trend_model = RW(cor = TRUE),
#     data = lake_chemistry,
#     family = gaussian()
#   )
# A no-trend or no-correlation fit reports a near-zero aggregate
# treatment effect with wide CIs; the state-space fit recovers
# heterogeneous slopes alongside the correlated latent drift.

devtools::load_all()

set.seed(20260604L)

n_lakes <- 5L
n_months <- 60L
lake_labels <- c("alpine", "boreal", "coastal", "delta", "estuary")

# Per-lake intercepts (centred so the global intercept absorbs the
# grand mean).
lake_int <- stats::rnorm(n_lakes, mean = 0, sd = 0.5)
lake_int <- lake_int - mean(lake_int)
names(lake_int) <- lake_labels

# Correlated random-walk innovations. (alpine, boreal) and
# (coastal, delta) form two regional clusters; estuary is
# independent.
rho_regional <- 0.6
Sigma_corr <- matrix(0, nrow = n_lakes, ncol = n_lakes)
diag(Sigma_corr) <- 1
Sigma_corr[1, 2] <- Sigma_corr[2, 1] <- rho_regional
Sigma_corr[3, 4] <- Sigma_corr[4, 3] <- rho_regional
sigma_rw <- 0.25
Sigma_innov <- (sigma_rw^2) * Sigma_corr

innov <- rmvn(
  n = n_months, mu = rep(0, n_lakes), Sigma = Sigma_innov
)
if (nrow(innov) == n_lakes && ncol(innov) == n_months) {
  innov <- t(innov)
}

# Random-walk states: cumulative sum of innovations per lake.
rw_trends <- apply(innov, 2, cumsum)
colnames(rw_trends) <- lake_labels
# Centre each trend so per-lake intercept stays identifiable.
rw_trends <- sweep(rw_trends, 2, colMeans(rw_trends))

# Treatment slopes: 2 positive, 3 negative.
treat_slopes <- c(alpine = 0.8, boreal = 0.6,
                   coastal = -0.5, delta = -0.7, estuary = -0.6)

# Long format.
grid <- expand.grid(
  month = seq_len(n_months),
  lake = factor(lake_labels, levels = lake_labels)
)
grid <- grid[order(grid$lake, grid$month), ]
grid$treated <- as.integer(grid$month >= 31L)

sigma_obs <- 0.4
eta <- numeric(nrow(grid))
lake_codes <- as.integer(grid$lake)
for (i in seq_len(nrow(grid))) {
  l <- lake_codes[i]
  eta[i] <- 0.0 +
    lake_int[l] +
    rw_trends[grid$month[i], l] +
    treat_slopes[l] * grid$treated[i]
}
y <- stats::rnorm(length(eta), mean = eta, sd = sigma_obs)

lake_chemistry <- data.frame(
  series = grid$lake,
  time = as.integer(grid$month),
  y = y,
  lake = grid$lake,
  month = as.integer(grid$month),
  treated = as.integer(grid$treated),
  chemistry = y
)
rownames(lake_chemistry) <- NULL

save(
  lake_chemistry,
  file = "data/lake_chemistry.rda",
  compress = "bzip2"
)

message("data/lake_chemistry.rda written: ",
        nrow(lake_chemistry), " rows across ",
        n_lakes, " lakes x ", n_months, " months.")
