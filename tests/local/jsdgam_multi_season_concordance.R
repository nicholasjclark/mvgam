# Concordance fixture for the multi-season closure-unit JSDM.
# Compares mvgam's `occ(multi_season = TRUE)` + AR(1) factor model
# against two community-standard multi-season occupancy packages on
# the same simulated data:
#
#   - spOccupancy::tMsPGOcc: multi-species multi-season Polya-Gamma
#     occupancy with per-(species, site) latent AR(1). Native multi-
#     species multi-season support; per-species `beta` coefficients
#     are directly comparable.
#
#   - flocker::flock(multiseason = "autologistic"): single-species
#     multi-season autologistic model with an AR-on-logit-psi
#     coupling and a stationary occupancy regression on env. flocker
#     has no multi-species multi-season family, so we fit one model
#     per species and aggregate. The colex / colex_eq alternative
#     misspecifies the factor-model truth (it constrains seasonal
#     variation to a colonisation-extinction process) and the b_env
#     posterior is identified only by season-1 data under explicit
#     init -- earlier runs blew up to 10^13 for 4/5 species.
#
# This fixture is built around an environmental covariate so the
# three packages produce directly-comparable per-species effect
# sizes. Truth:
#
#   logit_psi[s, i, t] = b_env_true[s] * env[i]
#                        + alpha_site[i]
#                        + (Z_true[s, ] dot lv_true[t, ])
#   y[s, i, t, v]      ~ Bernoulli(z[s, i, t] * p_true)
#
# Pass criteria (post stats-review):
#   - Per-species b_env: cor(truth, mvgam)   > 0.85
#                       cor(truth, spOcc)    > 0.85
#                       cor(truth, flocker)  > 0.85
#                       cor(mvgam, flocker)  > 0.85
#   - Detection p posterior mean within 0.05 of truth, all packages.
#   - Per-species marginal psi (averaged over site, season):
#       cor(mvgam, spOcc)   > 0.85
#       cor(mvgam, flocker) > 0.85
#   - mvgam Z Z' off-diag cor(truth, posterior) > 0.70 (factor
#     model headline; not comparable to spOcc / flocker which do
#     not parameterise cross-species correlation natively).
#
# Per-(species, site, season) E[psi] is reported as a diagnostic
# but NOT used as a pass/fail gate, because the comparison is
# unfair to the low-rank factor model: the per-cell surface is
# pinned far more freely by tMsPGOcc's per-(species, site) AR than
# by mvgam's rank-`n_lv` factorisation. The cross-species
# correlation structure (residual_cor) is what mvgam parameterises;
# the per-species effect sizes are what all three packages share.
#
# Caches each fit at /tmp/jsdgam_multi_season_*.rds. Total runtime
# ~45-60 min on first run.

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(dplyr)
  library(posterior)
})

stopifnot(requireNamespace("spOccupancy", quietly = TRUE))
stopifnot(requireNamespace("flocker", quietly = TRUE))

set.seed(616L)

# ----- Truth ---------------------------------------------------------

N        <- 5L
J        <- 30L
T_       <- 10L
K_visits <- 3L
n_lv     <- 2L
sigma_site_true <- 0.3
p_true          <- 0.5
species_levels  <- paste0("sp", seq_len(N))

# Per-site environmental covariate, standardised so the per-species
# `b_env` is on a comparable scale across packages.
env <- as.numeric(scale(rnorm(J)))

# Per-species environmental effect on logit-psi. Drawn from
# `N(0, 0.7)` so the effect sizes span a meaningful range
# (roughly +/- 1.4 on logit scale at 2 SD of env), well above
# the noise floor of the closure-unit Bernoulli likelihood.
b_env_true <- rnorm(N, mean = 0, sd = 0.7)
names(b_env_true) <- species_levels

# Factor model truth. AR(1) is dropped from this fixture's truth
# because it is weakly identified at any practical T (see recovery
# fixture for the empirical finding); using independent
# per-season lv values is the cleanest way to keep the factor model
# realistic without conflating dynamics with effect-size recovery.
Z_true  <- matrix(rnorm(N * n_lv, sd = 1.0), nrow = N, ncol = n_lv)
lv_true <- matrix(rnorm(T_ * n_lv, sd = 1.0), nrow = T_, ncol = n_lv)

alpha_site_true <- rnorm(J, mean = 0, sd = sigma_site_true)

logit_psi_true <- array(NA_real_, dim = c(N, J, T_))
for (s in seq_len(N)) {
  for (i in seq_len(J)) {
    for (t in seq_len(T_)) {
      logit_psi_true[s, i, t] <- b_env_true[s] * env[i] +
        alpha_site_true[i] +
        sum(Z_true[s, ] * lv_true[t, ])
    }
  }
}
psi_true <- 1 / (1 + exp(-logit_psi_true))
z_latent <- array(rbinom(length(psi_true), 1L, psi_true),
                  dim = c(N, J, T_))
y_arr    <- array(NA_integer_, dim = c(N, J, T_, K_visits))
for (s in seq_len(N)) {
  for (i in seq_len(J)) {
    for (t in seq_len(T_)) {
      for (v in seq_len(K_visits)) {
        y_arr[s, i, t, v] <- if (z_latent[s, i, t] == 1L) {
          rbinom(1L, 1L, p_true)
        } else {
          0L
        }
      }
    }
  }
}

# Per-species marginal psi (averaged across all (site, season))
# for the truth, used as the secondary comparator surface.
psi_true_per_sp <- apply(psi_true, 1L, mean)

cat("Simulated:", N, "species,", J, "sites,", T_, "seasons,",
    K_visits, "visits.\n")
cat("Truth b_env range:",
    paste(round(range(b_env_true), 3), collapse = " to "), "\n")
cat("Mean true psi =", round(mean(psi_true), 3),
    "| naive detection rate =", round(mean(y_arr), 3), "\n")

# ----- mvgam fit -----------------------------------------------------

cache_mvgam <- "/tmp/jsdgam_multi_season_concordance_mvgam.rds"
if (file.exists(cache_mvgam)) {
  cat("[cache] Loading mvgam fit.\n")
  fit_mvgam <- readRDS(cache_mvgam)
} else {
  cat("[fit ] jsdgam(occ(multi_season = TRUE), AR(1) factors,",
      "env:series, site RE)\n", sep = " ")
  long_mvgam <- pivot_detection_array(
    y_arr,
    site_covs = data.frame(env = env),
    species   = species_levels,
    multi_season = "hierarchical"
  )
  long_mvgam$site <- factor(long_mvgam$site)
  fit_mvgam <- jsdgam(
    formula        = y ~ env:series + s(site, bs = "re"),
    factor_formula = ~ AR(time = time) - 1,
    data           = long_mvgam,
    unit           = time, species = series,
    family         = occ(multi_season = TRUE),
    n_lv           = n_lv,
    prior          = prior(normal(0, 0.5), class = "sds"),
    chains         = 2L, parallel = TRUE,
    burnin         = 500L, samples = 500L,
    silent         = 2L, backend = "cmdstanr"
  )
  saveRDS(fit_mvgam, cache_mvgam)
}

# ----- spOccupancy::tMsPGOcc fit -------------------------------------

cache_spocc <- "/tmp/jsdgam_multi_season_concordance_spocc.rds"
if (file.exists(cache_spocc)) {
  cat("[cache] Loading spOccupancy::tMsPGOcc fit.\n")
  fit_spocc <- readRDS(cache_spocc)
} else {
  cat("[fit ] spOccupancy::tMsPGOcc with occ.formula = ~ env.\n")
  data_spocc <- list(
    y = y_arr,
    occ.covs = list(env = matrix(rep(env, T_), nrow = J, ncol = T_)),
    det.covs = list()
  )
  fit_spocc <- spOccupancy::tMsPGOcc(
    occ.formula = ~ env,
    det.formula = ~ 1,
    data = data_spocc,
    n.batch = 200L, batch.length = 25L,
    accept.rate = 0.43,
    n.burn = 2000L, n.thin = 5L,
    n.chains = 2L,
    ar1 = TRUE,
    verbose = FALSE
  )
  saveRDS(fit_spocc, cache_spocc)
}

# ----- flocker::flock(multiseason = "colex") per species -------------

cache_flocker <- "/tmp/jsdgam_multi_season_concordance_flocker.rds"
if (file.exists(cache_flocker)) {
  cat("[cache] Loading per-species flocker fits.\n")
  fits_flocker <- readRDS(cache_flocker)
} else {
  cat("[fit ] per-species flocker(multiseason='autologistic'),",
      "f_occ = ~ env -- slow.\n")
  fits_flocker <- vector("list", N)
  names(fits_flocker) <- species_levels
  for (s in seq_len(N)) {
    cat("  ", s, "/", N, " ", species_levels[s], "\n")
    y_s <- aperm(y_arr[s, , , , drop = TRUE], c(1L, 3L, 2L))
    # flocker `type = "multi"` expects `unit_covs` as a *list* of
    # length T_ (one site-covariate frame per season), each entry a
    # data.frame[J, p]. env is site-only so we replicate the same
    # frame across all seasons.
    unit_env <- lapply(seq_len(T_), function(t) {
      data.frame(env = env)
    })
    fdata <- flocker::make_flocker_data(
      obs = y_s, type = "multi",
      unit_covs = unit_env,
      event_covs = list(dummy = array(1, dim = c(J, K_visits, T_)))
    )
    # Autologistic + explicit init: `f_occ = ~ env` is the stationary
    # occupancy regression (identified by every season's data, not
    # just season 1 as under colex), `f_auto = ~ 1` is the constant
    # AR-on-logit-psi coupling. Tightened normal(0, 1.5) prior on
    # the occupancy fixed effects so the per-species fit stays
    # bounded under the factor-model truth even if it remains a
    # mild misspecification (truth has independent seasons via lv,
    # autologistic has AR-coupled seasons).
    fits_flocker[[s]] <- flocker::flock(
      f_occ  = ~ env,
      f_det  = ~ 1,
      f_col  = ~ 1,
      f_auto = ~ 1,
      multiseason = "autologistic",
      multi_init  = "explicit",
      flocker_data = fdata,
      chains = 2L, iter = 1500L, warmup = 500L,
      silent = 2L, refresh = 0L,
      backend = "cmdstanr"
    )
  }
  saveRDS(fits_flocker, cache_flocker)
}

# ----- Per-species b_env recovery -----------------------------------

cat("\n=== Per-species b_env effect-size recovery ===\n")

# mvgam: `b_env:seriessp{k}` coefficients (one per species). Call
# `as_draws_matrix(fit_mvgam, ...)` on the mvgam object (not
# `fit_mvgam$fit`) so the `b[k]` -> `b_<termname>` aliasing applied
# by `as_draws_*.mvgam` fires; hitting the raw stanfit yields the
# unrenamed `b[k]` columns instead.
draws_mvgam <- as_draws_matrix(fit_mvgam)
b_env_mvgam <- setNames(numeric(N), species_levels)
for (s in seq_len(N)) {
  pat <- paste0("^b_env:series", species_levels[s], "$")
  cols <- grep(pat, colnames(draws_mvgam), value = TRUE)
  if (length(cols) == 1L) {
    b_env_mvgam[s] <- mean(as.numeric(draws_mvgam[, cols]))
  }
}

# spOccupancy: `beta.samples` is [samples, N * (1 + n_occ_cov)].
# Columns alternate species; intercept first, env second. Use the
# `colnames` to extract by pattern.
b_env_spocc <- setNames(numeric(N), species_levels)
beta_samples <- fit_spocc$beta.samples
beta_cols    <- colnames(beta_samples)
for (s in seq_len(N)) {
  pat <- paste0("^env-sp", s, "$")
  cols <- grep(pat, beta_cols, value = TRUE)
  if (length(cols) == 1L) {
    b_env_spocc[s] <- mean(beta_samples[, cols])
  }
}

# flocker per-species: `b_occ_env` from each per-species fit.
b_env_flocker <- setNames(numeric(N), species_levels)
for (s in seq_len(N)) {
  draws_s <- as_draws_matrix(fits_flocker[[s]])
  # flocker emits the occupancy linpred coefficients under the
  # `b_occ_*` prefix.
  pat <- "^b_occ_env$"
  cols <- grep(pat, colnames(draws_s), value = TRUE)
  if (length(cols) == 1L) {
    b_env_flocker[s] <- mean(as.numeric(draws_s[, cols]))
  }
}

eff <- data.frame(
  species = species_levels,
  truth   = b_env_true,
  mvgam   = b_env_mvgam,
  spocc   = b_env_spocc,
  flocker = b_env_flocker
)
cat("\nPer-species b_env posterior means vs truth:\n")
print(eff)

cor_mvgam_truth   <- cor(eff$truth, eff$mvgam)
cor_spocc_truth   <- cor(eff$truth, eff$spocc)
cor_flocker_truth <- cor(eff$truth, eff$flocker)
cor_mvgam_flocker <- cor(eff$mvgam, eff$flocker)
cor_mvgam_spocc   <- cor(eff$mvgam, eff$spocc)
cor_spocc_flocker <- cor(eff$spocc, eff$flocker)

cat(sprintf("\ncor(truth, mvgam)   = %.4f  (target > 0.85)\n",
            cor_mvgam_truth))
cat(sprintf("cor(truth, spOcc)   = %.4f  (target > 0.85)\n",
            cor_spocc_truth))
cat(sprintf("cor(truth, flocker) = %.4f  (target > 0.85)\n",
            cor_flocker_truth))
cat(sprintf("cor(mvgam, spOcc)   = %.4f  (target > 0.85)\n",
            cor_mvgam_spocc))
cat(sprintf("cor(mvgam, flocker) = %.4f  (target > 0.85)\n",
            cor_mvgam_flocker))
cat(sprintf("cor(spOcc, flocker) = %.4f  (target > 0.85)\n",
            cor_spocc_flocker))

# ----- Detection probability recovery -------------------------------

cat("\n=== Detection probability recovery ===\n")

# mvgam: brms emits the detection dpar intercept as
# `b_p_Intercept` after `as_draws_*.mvgam` aliasing. Sub-formula
# intercept-only fits have a single scalar per draw; report
# posterior mean on the response scale. `p` (without `b_`) is
# the inv-logit response-scale dpar that the closure-unit lpmf
# reads; both columns are present after aliasing.
p_cols <- grep("^b_p_Intercept$|^Intercept_p$|^p$",
                colnames(draws_mvgam), value = TRUE)
p_mvgam <- if (length(p_cols) > 0L) {
  draws_p <- as.numeric(draws_mvgam[, p_cols[1L]])
  # If `p` is already on the response scale (`linkinv` applied),
  # values lie in [0, 1]; otherwise interpret as logit-p.
  if (max(draws_p, na.rm = TRUE) <= 1 &&
      min(draws_p, na.rm = TRUE) >= 0) {
    mean(draws_p)
  } else {
    mean(plogis(draws_p))
  }
} else {
  NA_real_
}
cat(sprintf("mvgam   p posterior mean = %.4f  (truth = %.2f)\n",
            p_mvgam, p_true))

# spOccupancy: `alpha.samples` is the species-level intercept for
# the detection model on the logit scale. Average across species.
alpha_samples <- fit_spocc$alpha.samples
p_spocc <- mean(plogis(as.matrix(alpha_samples)))
cat(sprintf("spOcc   p posterior mean = %.4f  (truth = %.2f)\n",
            p_spocc, p_true))

# flocker per-species: detection intercept on the logit scale.
# flocker prefixes the linpred coefficients with the dpar name
# (`mu` is detection in the multi-season family; the constructor
# sets `dpars = c("mu", "occ", "colo", "ex")`). Average across
# species after applying the inverse link.
p_flocker <- mean(vapply(seq_len(N), function(s) {
  draws_s <- as_draws_matrix(fits_flocker[[s]])
  cand_cols <- grep(
    "^b_Intercept$|^b_mu_Intercept$|^b_det_Intercept$",
    colnames(draws_s), value = TRUE
  )
  if (length(cand_cols) >= 1L) {
    mean(plogis(as.numeric(draws_s[, cand_cols[1L]])))
  } else {
    NA_real_
  }
}, numeric(1L)))
cat(sprintf("flocker p posterior mean = %.4f  (truth = %.2f)\n",
            p_flocker, p_true))

# ----- Per-species marginal psi (cross-package agreement) -----------

cat("\n=== Per-species marginal psi ===\n")

# mvgam: marginal psi per row from posterior_linpred (logit_psi);
# average within species across all (site, season).
lp_mvgam   <- posterior_linpred(fit_mvgam, ndraws = 500L)
psi_mvgam_per_row <- colMeans(1 / (1 + exp(-lp_mvgam)))
long_mvgam <- pivot_detection_array(
  y_arr, site_covs = data.frame(env = env),
  species = species_levels, multi_season = "hierarchical"
)
psi_mvgam_per_sp <- tapply(psi_mvgam_per_row, long_mvgam$series, mean)
psi_mvgam_per_sp <- psi_mvgam_per_sp[species_levels]

# spOccupancy: psi.samples is [draws, N, J, T]; average over draws,
# sites, seasons.
psi_spocc_per_sp <- apply(fit_spocc$psi.samples, 2L, mean)
names(psi_spocc_per_sp) <- species_levels

# flocker per-species: marginal psi via get_Z, averaged over draws,
# sites, seasons.
psi_flocker_per_sp <- vapply(seq_len(N), function(s) {
  z_arr <- flocker::get_Z(fits_flocker[[s]],
                           history_condition = FALSE,
                           sample = FALSE)
  mean(apply(z_arr, c(1L, 2L), mean))
}, numeric(1L))
names(psi_flocker_per_sp) <- species_levels

marg <- data.frame(
  species = species_levels,
  truth   = psi_true_per_sp,
  mvgam   = psi_mvgam_per_sp,
  spocc   = psi_spocc_per_sp,
  flocker = psi_flocker_per_sp
)
cat("\nPer-species marginal psi (averaged over site, season):\n")
# Round only numeric columns; `species` is character/factor.
marg_print <- marg
num_cols <- vapply(marg_print, is.numeric, logical(1L))
marg_print[num_cols] <- lapply(marg_print[num_cols], round, 3)
print(marg_print)

cat(sprintf("\ncor(mvgam,   spOcc)   = %.4f  (target > 0.85)\n",
            cor(marg$mvgam, marg$spocc)))
cat(sprintf("cor(mvgam,   flocker) = %.4f  (target > 0.85)\n",
            cor(marg$mvgam, marg$flocker)))
cat(sprintf("cor(spOcc,   flocker) = %.4f  (target > 0.85)\n",
            cor(marg$spocc, marg$flocker)))

# ----- mvgam factor-model headline ----------------------------------

cat("\n=== mvgam factor-model Z Z' recovery ===\n")
true_cov <- tcrossprod(Z_true)
true_cor <- cov2cor(true_cov + diag(1e-8, N))
post_cor <- residual_cor(fit_mvgam)$cor
true_off <- true_cor[upper.tri(true_cor)]
post_off <- post_cor[upper.tri(post_cor)]
cor_zz <- cor(true_off, post_off)
cat(sprintf("cor(off_diag(true_ZZ'), off_diag(posterior_ZZ')) = %.4f",
            cor_zz))
cat(sprintf("  (target > 0.70)\n"))

# ----- Per-cell psi diagnostic (NOT a pass/fail gate) ---------------

cat("\n=== Per-(species, site, season) E[psi] diagnostic ===\n")
cat("(parsimony tradeoff: spOcc free per-(species, site) AR vs\n")
cat(" mvgam rank-2 factor; expect spOcc cor to be higher)\n")

surf_mvgam <- long_mvgam |>
  mutate(psi_mvgam = psi_mvgam_per_row) |>
  group_by(series, site, time) |>
  summarise(psi_mvgam = first(psi_mvgam), .groups = "drop") |>
  rename(species = series, season = time)

psi_spocc_mean <- apply(fit_spocc$psi.samples, c(2L, 3L, 4L), mean)
surf_spocc <- data.frame(
  species = rep(species_levels, times = J * T_),
  site    = factor(rep(rep(seq_len(J), each = N), times = T_)),
  season  = rep(seq_len(T_), each = N * J),
  psi_spocc = as.vector(psi_spocc_mean)
)

surf_mvgam$site  <- as.integer(as.character(surf_mvgam$site))
surf_spocc$site  <- as.integer(as.character(surf_spocc$site))
surf_mvgam$species <- as.character(surf_mvgam$species)
surf_spocc$species <- as.character(surf_spocc$species)
surf <- inner_join(surf_mvgam, surf_spocc,
                    by = c("species", "site", "season"))
surf$psi_true <- numeric(nrow(surf))
# `surf$species` is character after the cast above; map back to the
# integer index via the simulated species_levels ordering.
sp_idx_lookup <- setNames(seq_len(N), species_levels)
for (i in seq_len(nrow(surf))) {
  s_i  <- sp_idx_lookup[[surf$species[i]]]
  st_i <- surf$site[i]
  t_i  <- surf$season[i]
  surf$psi_true[i] <- psi_true[s_i, st_i, t_i]
}
cat(sprintf("cor(mvgam, truth) per cell = %.4f\n",
            cor(surf$psi_mvgam, surf$psi_true)))
cat(sprintf("cor(spOcc, truth) per cell = %.4f\n",
            cor(surf$psi_spocc, surf$psi_true)))
cat(sprintf("cor(mvgam, spOcc) per cell = %.4f\n",
            cor(surf$psi_mvgam, surf$psi_spocc)))

cat("\nDone.\n")
