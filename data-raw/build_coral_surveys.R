# Build script for data/coral_surveys.rda.
#
# Generative structure: 3 reefs surveyed at a shared irregular
# monthly schedule (gaps ~ Uniform(1, 6) months). Each survey counts
# the bleached colonies among the colonies inspected, a binomial
# response on the logit scale. All three reefs are sampled at the
# SAME irregular times. The long-format input then satisfies mvgam's
# "each series shares the time grid" requirement. CAR still recovers
# the irregular-gap dynamics within each series.
#
#   latent[t, reef] = phi^dt * latent[t-1, reef]
#                   + N(0, sigma * sqrt((1 - phi^(2*dt))/(1 - phi^2)))
#   logit(p[t, reef]) = mu0 + reef_int[reef]
#                       + f(sst[t, reef])
#                       + latent[t, reef]
#   bleached[t, reef] ~ Binomial(colonies[t, reef], p[t, reef])
#
# SST is an observed driver on the observation equation. The latent
# CAR(1) state carries reef-specific persistence in continuous time:
# phi = 0.9, sigma = 0.3. With a median gap of three months the
# state keeps 0.9^3 = 0.73 of itself between surveys.
#
# A naive AR(1) treats every consecutive survey as one step. It
# gives a one-month gap and a six-month gap the same persistence.
# CAR decays the state by the length of each gap.
#
# Recommended fit (state-space):
#   mvgam(
#     formula = y | trials(colonies) ~ series + s(sst, k = 8),
#     trend_formula = ~ CAR(),
#     data = coral_surveys,
#     family = binomial()
#   )

devtools::load_all()

set.seed(20260604L)

n_reefs <- 3L
n_obs_per_reef <- 50L
reef_labels <- c("flynn", "myrmidon", "pixie")

# Per-reef intercepts (centred).
reef_int <- stats::rnorm(n_reefs, mean = 0, sd = 0.4)
reef_int <- reef_int - mean(reef_int)
names(reef_int) <- reef_labels

phi <- 0.9
sigma <- 0.3

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
sst_sm <- sim_smooth(x = sst_grid, k = 8L, bs = "tp", scale = 0.5)
sst_effect <- stats::approxfun(
  x = sst_grid, y = sst_sm$f - mean(sst_sm$f), rule = 2
)

# Colonies inspected per survey. Each survey records how many of them
# had bleached. The
# intercept puts a typical survey near 30% bleached.
df$colonies <- sample(seq.int(20L, 40L), nrow(df), replace = TRUE)
p_bleached <- stats::plogis(
  -0.85 + reef_int[as.integer(df$reef)] + df$trend_true +
    sst_effect(df$sst)
)
df$bleaching <- stats::rbinom(nrow(df), size = df$colonies,
                              prob = p_bleached)

coral_surveys <- data.frame(
  series = df$reef,
  time = as.integer(df$time),
  y = df$bleaching,
  reef = df$reef,
  sst = df$sst,
  bleaching = df$bleaching,
  colonies = df$colonies
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
