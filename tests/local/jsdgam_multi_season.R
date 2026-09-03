# Recovery fixture for multi-season closure-unit JSDM:
#   occ(multi_season = TRUE) + AR(1) on latent factors.
#
# Sketch of the data-generating process:
#
#   lv[t, :] ~ AR(1) with rho = 0.7 across T seasons
#                 |
#                 | Z[s, :] %*% lv[t, :]
#                 v
#   logit_psi[s, i, t] = alpha[i] + Z %*% lv[t, :]     # latent
#                                                        # occupancy
#                 |
#                 | Bernoulli(psi) marginalised per
#                 | (species, site, season)
#                 v
#   y[s, i, t, v] ~ Bernoulli(z[s, i, t] * p_true)     # observed
#                                                        # detections
#
# alpha[i] ~ N(0, sigma_site_true) is the per-site random intercept;
# the model fits the matching `s(site, bs = "re")` and is given the
# G0 stats-review-recommended `half-normal(0, 0.5)` prior on the
# random-effect SD.
#
# Empirically (2026-06-16): T = 6 is too few for Z recovery to clear
# cor > 0.7 (achieved 0.48 in a pilot fit). T = 10 clears 0.97 with
# the same priors and sample size, so the fixture pins T = 10. The
# G0 stats-review's T >= 6 prediction was for direct observation of
# `lv`; the closure-unit Bernoulli marginalisation costs more signal
# than the reviewer estimated and the empirical minimum is higher.
# AR(1) `rho` remains weakly identified even at T = 10 (posterior
# SD ~0.33 against a normal(0, 0.5) prior on the constrained
# interval), reflecting that the per-season cross-species correlation
# pattern is what the data identify, not the temporal autocorrelation
# of `lv`. Users wanting strong `rho` identification need T >= 20 or
# a tighter prior on `ar1_trend`. The roxygen on
# `occ(multi_season = TRUE)` carries this guidance.
#
# Primary go/no-go:
#   cor(off_diag(true_ZZ'), off_diag(posterior_mean_ZZ')) > 0.7
#
# Secondary recovery contracts:
#   - AR(1) rho posterior SD < prior SD (0.5), evidence of data signal
#   - divergent transitions < 5% of total
#   - min Z entry bulk ESS > 200
#   - degeneracy check: sd(rowMeans(Z_tilde %*% t(lv_tilde))) across
#     seasons > 0.1 on the logit scale (catches the failure mode
#     where the site RE absorbs Z * lv variation)
#
# Cached at tests/local/fixtures/val_mvgam_jsdgam_multi_season.rds.
# Delete to refit. Runtime ~8-15 min on cmdstanr with 2 chains.

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(dplyr)
  library(posterior)
})

# testthat sets the working directory to tests/local/ when it runs a
# file, while Rscript runs it from the package root. Reach the shared
# fixture helpers by whichever of the two paths exists.
source(if (file.exists("concordance_helpers.R")) {
  "concordance_helpers.R"
} else {
  file.path("tests", "local", "concordance_helpers.R")
})

set.seed(616L)

# ----- Truth ---------------------------------------------------------

N        <- 5L
J        <- 30L
T_       <- 10L
K_visits <- 3L
n_lv     <- 2L
rho_true       <- 0.7
sigma_site_true <- 0.3
p_true         <- 0.5
threshold_cor  <- 0.7
species_levels <- paste0("sp", seq_len(N))

# Z is K species x n_lv. Per stats-review G0, the Heaps QR
# identification is T-independent so the Z dimensioning is the same
# as in single-season fixtures.
Z_true <- matrix(rnorm(N * n_lv, sd = 1.0), nrow = N, ncol = n_lv)

# lv[t, :] follows an AR(1) across seasons with rho = 0.7. Marginal
# variance pinned to 1 so the cross-species correlation structure
# encoded in `Z Z'` is comparable to the existing single-season
# fixtures.
lv_true <- matrix(NA_real_, nrow = T_, ncol = n_lv)
sigma_innov <- sqrt(1 - rho_true^2)
for (l in seq_len(n_lv)) {
  lv_true[1L, l] <- rnorm(1L)
  for (t in 2L:T_) {
    lv_true[t, l] <- rho_true * lv_true[t - 1L, l] +
      sigma_innov * rnorm(1L)
  }
}

# Per-site random intercept on logit_psi.
alpha_site_true <- rnorm(J, mean = 0, sd = sigma_site_true)

# logit_psi[species, site, season] = alpha[site] + Z[s] %*% lv[t].
logit_psi <- array(NA_real_, dim = c(N, J, T_))
for (s in seq_len(N)) {
  for (i in seq_len(J)) {
    for (t in seq_len(T_)) {
      logit_psi[s, i, t] <- alpha_site_true[i] +
        sum(Z_true[s, ] * lv_true[t, ])
    }
  }
}
psi_true <- 1 / (1 + exp(-logit_psi))
z_latent <- array(rbinom(length(psi_true), 1L, psi_true),
                  dim = c(N, J, T_))

# Detection step: per visit y ~ Bernoulli(z * p_true). Sites with
# z = 0 contribute structural zeros; sites with z = 1 see a
# Bernoulli(p_true) draw per visit.
y_arr <- array(NA_integer_, dim = c(N, J, T_, K_visits))
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

# Pivot to long format using the hierarchical mode so `time = season`
# and `site` is a side-car covariate the obs-formula can reference.
long <- pivot_detection_array(
  y_arr, species = species_levels,
  multi_season = "hierarchical"
)
long$site <- factor(long$site)

# True residual correlation among species induced by Z Z'.
sigma_true_cov <- tcrossprod(Z_true)
sigma_true_cor <- cov2cor(sigma_true_cov + diag(1e-8, N))

cat("Simulated", nrow(long), "rows: ", N, "species,",
    J, "sites,", T_, "seasons,", K_visits, "visits.\n")
cat("Closure units = species x sites x seasons =",
    N * J * T_, ".\n")
cat("Mean true psi =", round(mean(psi_true), 3),
    "| naive detection rate =",
    round(mean(long$y), 3), ".\n")
cat("True cor off-diag range: [",
    round(min(sigma_true_cor[upper.tri(sigma_true_cor)]), 3), ",",
    round(max(sigma_true_cor[upper.tri(sigma_true_cor)]), 3), "].\n")

# ----- Fit -----------------------------------------------------------

# The generative truth rides on the saved fit so a separate test can
# assert recovery against it without repeating the simulation.
sim_truth <- list(
  N = N, J = J, T_ = T_, K_visits = K_visits, n_lv = n_lv,
  species_levels = species_levels,
  Z_true = Z_true, lv_true = lv_true, rho_true = rho_true,
  alpha_site_true = alpha_site_true,
  sigma_site_true = sigma_site_true, p_true = p_true,
  psi_true = psi_true, z_latent = z_latent,
  sigma_true_cov = sigma_true_cov, sigma_true_cor = sigma_true_cor
)

cache <- local_fixture_path("val_mvgam_jsdgam_multi_season.rds")
if (file.exists(cache)) {
  cat("[cache] Loading multi-season recovery fit.\n")
  fit <- readRDS(cache)
} else {
  cat("[fit ] jsdgam(occ(multi_season = TRUE), AR(1) factors,",
      "site RE, T =", T_, "seasons)\n", sep = " ")
  # half-normal(0, 0.5) on the site random-effect SD per stats-review
  # G0. Keeps the site RE on the logit scale modest (about +/- 1 logit
  # within 95% prior mass) so it does not absorb between-season
  # variation that the AR(1) factor structure should explain.
  site_re_prior <- prior(normal(0, 0.5), class = "sds")
  fit <- jsdgam(
    formula        = y ~ s(site, bs = "re"),
    # AR(1) on the latent factors. n_lv = 2 triggers the factor
    # model, so the AR constructor only needs the time axis. The
    # series axis is the implicit latent-factor axis; passing
    # `series = lv_axis()` is reserved for `by = lv_axis()` smooth
    # terms and is rejected inside the AR constructor.
    factor_formula = ~ AR(time = time) - 1,
    data           = long,
    # Pivot helper emits the canonical mvgam `series` column for
    # species; pass it through unchanged via species = series.
    unit           = time, species = series,
    family         = occ(multi_season = TRUE),
    n_lv           = n_lv,
    prior          = site_re_prior,
    chains         = 2L,
    burnin         = 500L, samples = 500L,
    silent         = 2,
    backend        = "cmdstanr"
  )
}
if (!identical(attr(fit, "sim_truth"), sim_truth)) {
  attr(fit, "sim_truth") <- sim_truth
  saveRDS(fit, cache)
}

# ----- Primary recovery ---------------------------------------------

cat("\n=== Primary recovery (cor on Z Z' off-diagonals) ===\n")
res_cor  <- residual_cor(fit)
post_cor <- res_cor$cor
true_off <- sigma_true_cor[upper.tri(sigma_true_cor)]
post_off <- post_cor[upper.tri(post_cor)]
cor_off  <- stats::cor(true_off, post_off)
mae_off  <- mean(abs(true_off - post_off))
cat(sprintf("cor(true, posterior) = %.4f  (threshold > %.2f)\n",
            cor_off, threshold_cor))
cat(sprintf("MAE                  = %.4f\n", mae_off))
cat(sprintf("PASS: %s\n", isTRUE(cor_off > threshold_cor)))

# ----- AR(1) rho recovery -------------------------------------------

cat("\n=== AR(1) rho recovery ===\n")
draws  <- as_draws_matrix(fit$fit)
rho_cols <- grep("^ar1_trend\\[", colnames(draws), value = TRUE)
if (length(rho_cols) == 0L) {
  rho_cols <- grep("^ar1_trend$", colnames(draws), value = TRUE)
}
if (length(rho_cols) > 0L) {
  prior_sd <- 0.5
  # Average across factors (n_lv = 2) since the AR(1) coefficient
  # is per-factor and both should reflect the same scale of data
  # signal.
  rho_mat <- as.matrix(draws[, rho_cols])
  rho_means <- colMeans(rho_mat)
  rho_sds   <- apply(rho_mat, 2L, sd)
  for (col_i in seq_along(rho_cols)) {
    cat(sprintf("%s: mean = %.3f, sd = %.3f\n",
                rho_cols[col_i], rho_means[col_i], rho_sds[col_i]))
  }
  # Recovery contract: posterior SD < prior SD on at least one
  # factor (evidence of data signal). This is a softer test than
  # "within 2 SD of truth" because the AR(1) rho is poorly
  # identified in the closure-unit-marginalised model and the
  # posterior often equals the prior. The Z structure is the
  # primary recovery target.
  cat(sprintf("PASS (any factor sd < %.2f prior sd): %s\n",
              prior_sd, isTRUE(any(rho_sds < prior_sd))))
} else {
  cat("WARN: ar1_trend column not located in posterior.\n")
}

# ----- Degeneracy check ----------------------------------------------
# Per G0 stats-review: if the site RE absorbs Z * lv variation, the
# posterior `Z * lv` will be flat across seasons. Catch this with a
# direct sd-across-seasons check on the row means of the posterior
# mean Z * lv'.

cat("\n=== Degeneracy check (Z * lv variation across seasons) ===\n")
Z_arr <- mvgam:::extract_Z_loadings(
  draws, n_obs_series = N, n_lv = n_lv
)
Z_mean <- apply(Z_arr, c(2L, 3L), mean)
# `extract_lv_trend_matrices()` returns a list of length n_lv;
# each element is `[ndraws, n_time]`. Per-factor posterior mean
# is `colMeans(lv_arr[[k]])`, then stack columns into a
# `[T_, n_lv]` matrix to compose with `Z_mean`.
lv_arr <- mvgam:::extract_lv_trend_matrices(fit, n_lv)
lv_mean <- do.call(cbind, lapply(lv_arr, colMeans))
Zlv_mean <- Z_mean %*% t(lv_mean)
# `Zlv_mean` is N x T. The per-species variation across seasons is
# what the factor structure must contribute. Aggregate over species
# to get one scalar per season, then check its sd.
season_means <- colMeans(Zlv_mean)
sd_seasons <- sd(season_means)
cat(sprintf("sd(rowMeans(Z * lv')) across seasons = %.3f\n",
            sd_seasons))
cat(sprintf("PASS (> 0.1 on logit scale): %s\n",
            isTRUE(sd_seasons > 0.1)))

# ----- Sampler diagnostics ------------------------------------------

cat("\n=== Sampler diagnostics ===\n")
diag_df <- nuts_params(fit$fit)
n_div   <- sum(subset(diag_df, Parameter == "divergent__")$Value)
n_treedepth <- sum(
  subset(diag_df, Parameter == "treedepth__")$Value >= 10L
)
n_total <- nrow(subset(diag_df, Parameter == "divergent__"))
cat(sprintf("Divergent transitions:    %d / %d (%.2f%%)\n",
            n_div, n_total, 100 * n_div / n_total))
cat(sprintf("Max-treedepth saturation: %d / %d (%.2f%%)\n",
            n_treedepth, n_total, 100 * n_treedepth / n_total))
cat(sprintf("PASS (div < 5%%): %s\n",
            isTRUE(n_div / n_total < 0.05)))

# ----- Z-entry bulk ESS ---------------------------------------------

cat("\n=== Z-entry bulk ESS ===\n")
z_pat <- if (any(grepl("^Z_tilde\\[", colnames(draws)))) {
  "^Z_tilde\\["
} else {
  "^Z\\["
}
z_cols <- grep(z_pat, colnames(draws), value = TRUE)
z_summary <- summarise_draws(
  draws[, z_cols, drop = FALSE], "ess_bulk"
)
min_ess <- min(z_summary$ess_bulk, na.rm = TRUE)
cat(sprintf("min ess_bulk = %.0f\n", min_ess))
cat(sprintf("max ess_bulk = %.0f\n",
            max(z_summary$ess_bulk, na.rm = TRUE)))
cat(sprintf("PASS (min > 200): %s\n", isTRUE(min_ess > 200)))

cat("\nDone.\n")
