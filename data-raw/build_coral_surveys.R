# Build script for data/coral_surveys.rda.
#
# Generative structure: 3 reefs surveyed at a shared irregular
# monthly schedule (gaps ~ Uniform(1, 6) months) for a continuous
# bleaching index (Gaussian, scaled). All three reefs are sampled
# at the SAME irregular times so the long-format input satisfies
# mvgam's "each series shares the time grid" requirement; CAR
# still recovers the irregular-gap dynamics within each series.
# The dataset is state-space:
#
#   latent[t, reef] = phi^dt * latent[t-1, reef]
#                   + N(0, sigma * sqrt((1 - phi^(2*dt))/(1 - phi^2)))
#   y[t, reef] = mu0 + reef_int[reef]
#              + f(sst[t, reef])
#              + latent[t, reef]
#              + N(0, sigma_obs)
#
# SST is an observed driver on the observation equation; the
# latent CAR(1) state captures reef-specific persistence in
# continuous time. phi = 0.7, sigma = 0.5.
#
# Naive AR(1) (which treats every consecutive obs as gap = 1)
# biases phi downward toward phi^mean(dt) ~ 0.29, and the SST
# smooth absorbs trend structure that belongs to the AR.
#
# Recommended fit (state-space):
#   mvgam(
#     formula = y ~ s(sst, k = 8),
#     trend_formula = ~ CAR(),
#     data = coral_surveys,
#     family = gaussian()
#   )
# This recovers true phi ~ 0.7 and a recognisable SST smooth.

devtools::load_all()

set.seed(20260604L)

n_reefs <- 3L
n_obs_per_reef <- 50L
reef_labels <- c("flynn", "myrmidon", "pixie")

# Per-reef intercepts (centred).
reef_int <- stats::rnorm(n_reefs, mean = 0, sd = 0.4)
reef_int <- reef_int - mean(reef_int)
names(reef_int) <- reef_labels

phi <- 0.7
sigma <- 0.5

# Sample one irregular time schedule shared across all three reefs
# so the dataset satisfies mvgam's "each series shares the time
# grid" requirement while still giving CAR's irregular-gap pattern
# something to recover.
gaps <- sample(seq.int(1L, 6L), size = n_obs_per_reef - 1L,
                replace = TRUE)
times <- cumsum(c(1L, gaps))

# Propagate an independent CAR(1) latent trend per reef along the
# shared schedule. Initial state drawn from the stationary
# marginal N(0, sigma^2 / (1 - phi^2)).
all_rows <- vector("list", n_reefs)
for (r in seq_len(n_reefs)) {
  trend <- numeric(n_obs_per_reef)
  trend[1L] <- stats::rnorm(
    1L, mean = 0, sd = sigma / sqrt(1 - phi^2)
  )
  for (t in seq.int(2L, n_obs_per_reef)) {
    dt <- max(times[t] - times[t - 1L], 1e-3)
    decay <- phi^dt
    innov_sd <- sigma * sqrt((1 - phi^(2 * dt)) / (1 - phi^2))
    trend[t] <- decay * trend[t - 1L] +
      stats::rnorm(1L, mean = 0, sd = innov_sd)
  }
  all_rows[[r]] <- data.frame(
    reef = factor(reef_labels[r], levels = reef_labels),
    time = as.integer(times),
    trend_true = trend
  )
}
df <- do.call(rbind, all_rows)
rownames(df) <- NULL

# Observed SST (deg C) per row: seasonal pattern + noise. SST sits
# on the OBSERVATION equation, not the latent state.
df$sst <- 26 + 1.5 * sin(2 * pi * df$time / 12) +
  stats::rnorm(nrow(df), mean = 0, sd = 0.5)

# Smooth nonlinear obs-side effect of SST, shared across reefs.
sst_grid <- seq(min(df$sst) - 0.5, max(df$sst) + 0.5,
                 length.out = 100L)
sst_sm <- sim_smooth(x = sst_grid, k = 8L, bs = "tp", scale = 0.4)
sst_effect <- stats::approxfun(
  x = sst_grid, y = sst_sm$f - mean(sst_sm$f), rule = 2
)

sigma_obs <- 0.3
df$bleaching <- stats::rnorm(
  nrow(df),
  mean = reef_int[as.integer(df$reef)] +
    df$trend_true +
    sst_effect(df$sst),
  sd = sigma_obs
)

coral_surveys <- data.frame(
  series = df$reef,
  time = as.integer(df$time),
  y = df$bleaching,
  reef = df$reef,
  sst = df$sst,
  bleaching = df$bleaching
)
rownames(coral_surveys) <- NULL

save(
  coral_surveys,
  file = "data/coral_surveys.rda",
  compress = "bzip2"
)

message("data/coral_surveys.rda written: ",
        nrow(coral_surveys), " rows across ",
        n_reefs, " reefs x ~", n_obs_per_reef, " obs.")
