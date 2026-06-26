# Build the cached fits + simulated data consumed by
# `vignettes/articles/multivariate.Rmd`.
#
# Run once locally with:
#   Rscript tests/local/multivariate_vignette_fits.R
#
# The article reads the RDS files unconditionally when present;
# regenerate this cache whenever a relevant API or default shifts.

suppressPackageStartupMessages({
  devtools::load_all(quiet = TRUE)
  library(mvgam)
})

cache_dir <- file.path("pkgdown", "multivariate_cache")
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
}

# ---- Simulate the truth -------------------------------------------------
# One latent abundance trajectory x[t] observed by three parallel
# monitoring methods. Each method has its own observation family,
# its own intercept and its own per-arm covariate.

set.seed(2024)

T_  <- 80L                      # monthly time points (6.5 years)
phi <- 0.7                      # AR(1) persistence
sigma_proc <- 0.4               # AR(1) innovation SD

# Latent state under a stationary AR(1).
x <- numeric(T_)
x[1L] <- rnorm(1L, mean = 0, sd = sigma_proc / sqrt(1 - phi^2))
for (t in 2L:T_) {
  x[t] <- phi * x[t - 1L] + rnorm(1L, 0, sigma_proc)
}

# Per-arm intercepts and covariate effects.
a_count   <- 1.5
a_biomass <- 0.5
a_camera  <- -0.5
b_deploy  <- 0.05
nu        <- 2.0  # Gamma shape

# Covariates.
month_idx <- ((seq_len(T_) - 1L) %% 12L) + 1L
log_effort_count <- log(runif(T_, min = 0.8, max = 1.2))
deploy_days      <- runif(T_, min = 5, max = 20)
f_season         <- sin(2 * pi * month_idx / 12)

# Observation models.
lambda      <- exp(a_count + log_effort_count + x)
count     <- rpois(T_, lambda)

mu_biomass  <- exp(a_biomass + f_season + x)
biomass   <- rgamma(T_, shape = nu, rate = nu / mu_biomass)

p_camera    <- plogis(a_camera + b_deploy * deploy_days + x)
camera_full <- rbinom(T_, size = 1L, prob = p_camera)

# Camera schedule: drop 20 of 80 time points at random
# (missingness independent of the latent state). mvgam keeps
# the camera arm on its own non-NA rows so the shared AR(1)
# state is still informed at every time point by the count and
# biomass arms.
camera_keep <- sort(sample.int(T_, size = 60L))
camera <- rep(NA_integer_, T_)
camera[camera_keep] <- camera_full[camera_keep]

mv_data <- data.frame(
  time             = seq_len(T_),
  series           = factor(rep("pop1", T_)),
  month            = month_idx,
  log_effort       = log_effort_count,
  deploy_days      = deploy_days,
  count            = count,
  biomass          = biomass,
  camera           = camera
)

truth <- list(
  x         = x,
  phi       = phi,
  sigma     = sigma_proc,
  a_count   = a_count,
  a_biomass = a_biomass,
  a_camera  = a_camera,
  b_deploy  = b_deploy,
  nu        = nu,
  month     = month_idx,
  log_effort = log_effort_count,
  deploy_days = deploy_days,
  seed      = 2024L
)

saveRDS(mv_data, file.path(cache_dir, "mv_data.rds"))
saveRDS(truth,   file.path(cache_dir, "truth.rds"))

CHAINS  <- 2L
SAMPLES <- 1000L
SILENT  <- 2L


# ---- Univariate baseline: counts only ----------------------------------

cat("Fitting mod_uni_count ...\n")
mod_uni_count <- mvgam(
  count ~ 1 + offset(log_effort),
  trend_formula = ~ AR(p = 1),
  family = poisson(),
  data   = mv_data,
  chains = CHAINS, samples = SAMPLES, silent = SILENT
)
saveRDS(mod_uni_count, file.path(cache_dir, "mod_uni_count.rds"))


# ---- Univariate baseline: biomass only ---------------------------------

cat("Fitting mod_uni_biomass ...\n")
mod_uni_biomass <- mvgam(
  biomass ~ 1 + s(month, bs = "cc", k = 8),
  trend_formula = ~ AR(p = 1),
  family = Gamma(link = "log"),
  data   = mv_data,
  knots  = list(month = c(0.5, 12.5)),
  chains = CHAINS, samples = SAMPLES, silent = SILENT
)
saveRDS(mod_uni_biomass, file.path(cache_dir, "mod_uni_biomass.rds"))


# ---- Univariate baseline: camera only ----------------------------------
# Keep the full T_-row grid (NA-padded camera rows) so the time
# axis stays regular for the AR(1) trend; mvgam handles NA
# responses natively.

cat("Fitting mod_uni_camera ...\n")
mod_uni_camera <- mvgam(
  camera ~ 1 + deploy_days,
  trend_formula = ~ AR(p = 1),
  family = bernoulli(link = "logit"),
  data   = mv_data,
  chains = CHAINS, samples = SAMPLES, silent = SILENT
)
saveRDS(mod_uni_camera, file.path(cache_dir, "mod_uni_camera.rds"))


# ---- Joint fit: one shared AR(1), three families ------------------------

cat("Fitting mod_joint ...\n")
joint_formula <- bf(count   ~ 1 + offset(log_effort),
                          family = poisson()) +
                 bf(biomass ~ 1 + s(month, bs = "cc", k = 8),
                          family = Gamma(link = "log")) +
                 bf(camera  ~ 1 + deploy_days,
                          family = bernoulli(link = "logit"))

mod_joint <- mvgam(
  joint_formula,
  trend_formula = ~ AR(p = 1),
  data   = mv_data,
  knots  = list(month = c(0.5, 12.5)),
  chains = CHAINS, samples = SAMPLES, silent = SILENT
)
saveRDS(mod_joint, file.path(cache_dir, "mod_joint.rds"))

cat("Done. Cache files in ", cache_dir, ":\n", sep = "")
print(list.files(cache_dir, full.names = TRUE))
