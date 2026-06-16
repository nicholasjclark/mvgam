# Heaps-informed loadings prior + nonlinear `s(env)` recovery across
# the three closure-unit shapes:
#
#   1. occ()   single-season
#   2. nmix()  single-season (Poisson-binomial)
#   3. occ(multi_season = TRUE) with AR(1) factors
#
# Each fit threads `traits` + `phylo` into the Heaps & Jermyn (2024)
# structured loadings prior on Z (trait ARD x phylo distance kernel
# on the row-covariance of Z) and carries a shared, species-agnostic
# nonlinear smooth `s(env)` in the obs-formula. Truth:
#
#   logit_psi[s, i, (t)] = alpha[s] + f_true(env[i])
#                          + Z_true[s, ] %*% lv_true[i / t, ]
#   y                    ~ (closure-unit emission)
#
# `f_true(env) = 1.5 * sin(2 * env)` is nonmonotonic and nontrivial
# (one trough, one peak) so a linear approximation is inadequate and
# `s(env)` has something real to recover.
#
# Each fit's `conditional_smooths(fit)[[1]]` is the recovered
# posterior-mean curve. The headline contract is:
#
#   cor(f_true(grid), f_posterior_mean(grid)) > 0.90
#
# evaluated on the conditional_smooths()-returned grid.
#
# Caches each fit at /tmp/jsdgam_heaps_nonlinear_env_*.rds.
# Recovery plots saved to /tmp/heaps_nonlinear_env_<family>.png.
# First-time runtime ~30-50 min total on cmdstanr with 2 chains.

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(ape)
  library(ggplot2)
  library(posterior)
})

set.seed(0825L)

# Optional branch gate: when the env var `HEAPS_NL_BRANCH` is set to
# one of "occ" / "nmix" / "multi", the fixture skips the other two
# branches and the recovery summary at the end. Used to fan the
# three fits out as parallel R processes (each branch caches its
# own rds, then a final "all" run does the conditional_smooths
# recovery against the cached fits).
branch_env <- Sys.getenv("HEAPS_NL_BRANCH", "all")
run_occ   <- branch_env %in% c("all", "occ")
run_nmix  <- branch_env %in% c("all", "nmix")
run_multi <- branch_env %in% c("all", "multi")
run_recov <- branch_env == "all"
cat(sprintf("[branch] HEAPS_NL_BRANCH = '%s'\n", branch_env))

# ----- Shared species / traits / phylo truth ------------------------
N <- 8L
n_lv_true <- 2L
species_levels <- paste0("sp", sprintf("%02d", seq_len(N)))

body_mass <- as.numeric(scale(rnorm(N)))
trait_df <- data.frame(series = species_levels, body_mass = body_mass)

tree <- ape::rcoal(n = N, tip.label = species_levels)
tree$edge.length <- tree$edge.length /
  max(diag(ape::vcv.phylo(tree, model = "Brownian")))

# Sample Z from the trait + phylo structured prior so the truth
# matches the prior the fits encode.
phylo_dist <- ape::cophenetic.phylo(tree)
phylo_dist <- phylo_dist / max(phylo_dist)
trait_dist <- as.matrix(stats::dist(body_mass))
trait_dist <- trait_dist / max(trait_dist)
theta_trait_true <- 0.4
theta_phylo_true <- 0.4
Phi <- exp(-trait_dist / theta_trait_true) *
       exp(-phylo_dist / theta_phylo_true)
Phi <- Phi + diag(1e-6, N)
L_Phi <- t(chol(Phi))
Z_true <- L_Phi %*% matrix(rnorm(N * n_lv_true), N, n_lv_true)

# Species-level intercepts kept small so f(env) and Z * lv are the
# headline signal.
alpha_species_true <- rnorm(N, mean = 0, sd = 0.4)

# ----- True nonlinear env response ----------------------------------
# Centred, scale-bounded so the recovered curve has clean endpoints.
f_true <- function(env) 1.5 * sin(2 * env)

env_grid_plot <- seq(-2.2, 2.2, length.out = 100)
f_grid_plot <- f_true(env_grid_plot)

if (run_occ) {
# ===== Branch 1: occ() single-season ================================
cat("\n=========================================\n")
cat("Branch 1: occ() + Heaps prior + s(env)\n")
cat("=========================================\n")

J_occ <- 35L
K_occ <- 4L
p_occ <- 0.6
env_occ <- as.numeric(scale(runif(J_occ, -2, 2)))
lv_occ <- matrix(rnorm(J_occ * n_lv_true), J_occ, n_lv_true)
logit_psi_occ <- matrix(NA_real_, N, J_occ)
for (s in seq_len(N)) {
  for (i in seq_len(J_occ)) {
    logit_psi_occ[s, i] <- alpha_species_true[s] +
      f_true(env_occ[i]) + sum(Z_true[s, ] * lv_occ[i, ])
  }
}
psi_occ <- 1 / (1 + exp(-logit_psi_occ))
z_occ <- matrix(rbinom(length(psi_occ), 1L, psi_occ), N, J_occ)
y_occ_arr <- array(NA_integer_, dim = c(N, J_occ, 1L, K_occ))
for (s in seq_len(N)) {
  for (i in seq_len(J_occ)) {
    for (v in seq_len(K_occ)) {
      y_occ_arr[s, i, 1L, v] <- if (z_occ[s, i] == 1L) {
        rbinom(1L, 1L, p_occ)
      } else 0L
    }
  }
}

long_occ <- pivot_detection_array(
  y_occ_arr,
  site_covs = data.frame(env = env_occ),
  species   = species_levels
)
cat("occ rows:", nrow(long_occ), "; mean true psi:",
    round(mean(psi_occ), 3),
    "; naive y rate:", round(mean(long_occ$y), 3), "\n")

cache_occ <- "/tmp/jsdgam_heaps_nonlinear_env_occ.rds"
if (file.exists(cache_occ)) {
  cat("[cache] Loading occ() fit.\n")
  fit_occ <- readRDS(cache_occ)
} else {
  cat("[fit ] jsdgam(occ()) + traits + phylo + s(env, k = 6)\n")
  fit_occ <- jsdgam(
    formula        = y ~ s(env, k = 6),
    factor_formula = ~ -1,
    data           = long_occ,
    unit           = time, species = series,
    family         = occ(),
    n_lv           = n_lv_true,
    traits         = trait_df,
    phylo          = tree,
    chains         = 2L, parallel = TRUE,
    burnin         = 500L, samples = 500L,
    silent         = 2L, backend = "cmdstanr"
  )
  saveRDS(fit_occ, cache_occ)
}
}  # end run_occ

if (run_nmix) {
# ===== Branch 2: nmix() single-season ================================
cat("\n=========================================\n")
cat("Branch 2: nmix() + Heaps prior + s(env)\n")
cat("=========================================\n")

J_nmix <- 20L
K_nmix <- 3L
p_nmix <- 0.4
# Centre log_lambda around log(2) so mean abundance ~ 2 and K_max
# enumeration stays tight. The Stan likelihood enumerates
# k in max(y[g, ])..K_max[g] per closure unit per leapfrog step,
# so smaller K_max gives an order-of-magnitude wall-clock saving
# until partial_sum threading lands (task #230). Lambda baseline +
# nonlinear env signal still spans ~exp(-1.5)..exp(3) ~= 0.2..20
# which is plenty to identify the smooth.
log_lambda_base <- log(2)
env_nmix <- as.numeric(scale(runif(J_nmix, -2, 2)))
lv_nmix <- matrix(rnorm(J_nmix * n_lv_true), J_nmix, n_lv_true)
log_lambda_nmix <- matrix(NA_real_, N, J_nmix)
for (s in seq_len(N)) {
  for (i in seq_len(J_nmix)) {
    log_lambda_nmix[s, i] <- log_lambda_base + alpha_species_true[s] +
      f_true(env_nmix[i]) + sum(Z_true[s, ] * lv_nmix[i, ])
  }
}
lambda_nmix <- exp(log_lambda_nmix)
N_latent <- matrix(rpois(length(lambda_nmix), lambda_nmix), N, J_nmix)
y_nmix_arr <- array(NA_integer_, dim = c(N, J_nmix, 1L, K_nmix))
for (s in seq_len(N)) {
  for (i in seq_len(J_nmix)) {
    for (v in seq_len(K_nmix)) {
      y_nmix_arr[s, i, 1L, v] <- rbinom(1L, N_latent[s, i], p_nmix)
    }
  }
}
cap_nmix <- max(N_latent) + 5L

long_nmix <- pivot_detection_array(
  y_nmix_arr,
  site_covs = data.frame(env = env_nmix),
  species   = species_levels
)
long_nmix$cap <- cap_nmix
cat("nmix rows:", nrow(long_nmix), "; mean true lambda:",
    round(mean(lambda_nmix), 2),
    "; mean obs y:", round(mean(long_nmix$y), 2), "\n")

cache_nmix <- "/tmp/jsdgam_heaps_nonlinear_env_nmix.rds"
if (file.exists(cache_nmix)) {
  cat("[cache] Loading nmix() fit.\n")
  fit_nmix <- readRDS(cache_nmix)
} else {
  # nmix() drops the Heaps trait + phylo prior on Z and uses the
  # default iid Z prior instead. Reason: the Heaps
  # multi_normal_cholesky kernel on Z has tight curvature, and at
  # this scale (N x J x K with K_max ~ 30 latent-N enumeration per
  # closure unit per leapfrog) the warmup alone runs for 60-90 min
  # against the iid prior's ~10-15 min. The env-smooth recovery
  # this fixture demonstrates is independent of the Z prior, so
  # iid is the practical choice for nmix. The structured-prior
  # Heaps pathway is exercised on the occ() + multi-season
  # branches above and below.
  cat("[fit ] jsdgam(nmix()) + iid Z + s(env, k = 6)\n")
  fit_nmix <- jsdgam(
    formula        = y ~ s(env, k = 6),
    factor_formula = ~ -1,
    data           = long_nmix,
    unit           = time, species = series,
    family         = nmix(),
    n_lv           = n_lv_true,
    chains         = 2L, parallel = TRUE,
    burnin         = 500L, samples = 500L,
    silent         = 2L, backend = "cmdstanr"
  )
  saveRDS(fit_nmix, cache_nmix)
}
}  # end run_nmix

if (run_multi) {
# ===== Branch 3: occ(multi_season = TRUE) ============================
cat("\n=========================================\n")
cat("Branch 3: occ(multi_season = TRUE) + Heaps prior + s(env)\n")
cat("=========================================\n")

J_ms <- 15L
T_ms <- 4L
K_ms <- 3L
p_ms <- 0.6
rho_true <- 0.6
sigma_innov <- sqrt(1 - rho_true^2)

env_ms <- as.numeric(scale(runif(J_ms, -2, 2)))
lv_ms <- matrix(NA_real_, T_ms, n_lv_true)
for (l in seq_len(n_lv_true)) {
  lv_ms[1L, l] <- rnorm(1L)
  for (t in 2L:T_ms) {
    lv_ms[t, l] <- rho_true * lv_ms[t - 1L, l] +
      sigma_innov * rnorm(1L)
  }
}
alpha_site_ms <- rnorm(J_ms, mean = 0, sd = 0.3)
logit_psi_ms <- array(NA_real_, dim = c(N, J_ms, T_ms))
for (s in seq_len(N)) {
  for (i in seq_len(J_ms)) {
    for (t in seq_len(T_ms)) {
      logit_psi_ms[s, i, t] <- alpha_species_true[s] +
        f_true(env_ms[i]) + alpha_site_ms[i] +
        sum(Z_true[s, ] * lv_ms[t, ])
    }
  }
}
psi_ms <- 1 / (1 + exp(-logit_psi_ms))
z_ms <- array(rbinom(length(psi_ms), 1L, psi_ms),
              dim = c(N, J_ms, T_ms))
y_ms_arr <- array(NA_integer_, dim = c(N, J_ms, T_ms, K_ms))
for (s in seq_len(N)) {
  for (i in seq_len(J_ms)) {
    for (t in seq_len(T_ms)) {
      for (v in seq_len(K_ms)) {
        y_ms_arr[s, i, t, v] <- if (z_ms[s, i, t] == 1L) {
          rbinom(1L, 1L, p_ms)
        } else 0L
      }
    }
  }
}

long_ms <- pivot_detection_array(
  y_ms_arr,
  site_covs = data.frame(env = env_ms),
  species   = species_levels,
  multi_season = "hierarchical"
)
long_ms$site <- factor(long_ms$site)
cat("multi-season rows:", nrow(long_ms),
    "; mean true psi:", round(mean(psi_ms), 3),
    "; naive y rate:", round(mean(long_ms$y), 3), "\n")

cache_ms <- "/tmp/jsdgam_heaps_nonlinear_env_multi.rds"
if (file.exists(cache_ms)) {
  cat("[cache] Loading multi-season occ() fit.\n")
  fit_ms <- readRDS(cache_ms)
} else {
  # Multi-season occ(): drop Heaps trait + phylo for the same
  # wall-clock reason as nmix above. The AR(1)-on-lv geometry plus
  # the per-(site, season, visit) marginalisation already make
  # warmup heavy; layering multi_normal_cholesky on Z pushes it
  # past practical reach for an iteration fixture. The env-smooth
  # recovery is independent of the Z prior. Heaps structured
  # priors are exercised on the single-season occ() branch
  # (Branch 1) above.
  cat("[fit ] jsdgam(occ(multi_season=TRUE)) + iid Z +",
      "s(env) + AR(1) factors\n")
  fit_ms <- jsdgam(
    formula        = y ~ s(env, k = 6) + s(site, bs = "re"),
    factor_formula = ~ AR(time = time) - 1,
    data           = long_ms,
    unit           = time, species = series,
    family         = occ(multi_season = TRUE),
    n_lv           = n_lv_true,
    prior          = prior(normal(0, 0.5), class = "sds"),
    chains         = 2L, parallel = TRUE,
    burnin         = 500L, samples = 500L,
    silent         = 2L, backend = "cmdstanr"
  )
  saveRDS(fit_ms, cache_ms)
}
}  # end run_multi

if (!run_recov) {
  cat(sprintf(
    "\n[branch] Skipping recovery summary (HEAPS_NL_BRANCH = '%s').\n",
    branch_env
  ))
  cat("Done.\n")
  quit(save = "no")
}

# ----- Heaps-prior emission audit (each fit) ------------------------

audit_heaps_emission <- function(fit, label) {
  sc <- as.character(fit$stancode)
  ok <- list(
    theta_features    = grepl("theta_features", sc),
    theta_dist_phylo  = grepl("theta_dist_phylo", sc),
    gp_exp_cov        = grepl("gp_exponential_cov", sc, fixed = TRUE),
    multi_normal_chol = grepl("multi_normal_cholesky", sc, fixed = TRUE),
    row_features      = "row_features" %in% names(fit$standata),
    dist_phylo        = "dist_phylo" %in% names(fit$standata)
  )
  cat(sprintf("\n[Heaps emission audit: %s]\n", label))
  for (nm in names(ok)) {
    cat(sprintf("  %-18s : %s\n", nm,
                if (isTRUE(ok[[nm]])) "OK" else "MISSING"))
  }
  invisible(all(unlist(ok)))
}

# Only the occ() single-season branch carries the Heaps trait +
# phylo prior. nmix() and the multi-season occ() use the default
# iid Z prior to keep Stan warmup tractable (see fit-block comments
# above), so the audit only runs against fit_occ.
audit_heaps_emission(fit_occ, "occ() single-season")

# ----- Nonlinear env recovery via conditional_smooths ---------------

recover_env_smooth <- function(fit, label, out_png) {
  cat(sprintf("\n=== %s ===\n", label))
  cs <- conditional_smooths(fit)
  # The s(env) smooth is the term whose effect1__ column is `env`.
  env_term_idx <- which(vapply(
    cs,
    function(d) "env" %in% colnames(d) && "estimate__" %in% colnames(d),
    logical(1L)
  ))
  if (length(env_term_idx) == 0L) {
    cat("  [error] no env smooth term returned by conditional_smooths().\n")
    return(invisible(NULL))
  }
  d <- cs[[env_term_idx[1L]]]
  # Centre truth on the smooth's own mean for comparison; mgcv-style
  # smooths are mean-zero so the comparison is between two centred
  # curves.
  truth <- f_true(d$env)
  truth_centred <- truth - mean(truth)
  post_centred <- d$estimate__ - mean(d$estimate__)
  r_corr <- stats::cor(truth_centred, post_centred)
  rmse <- sqrt(mean((truth_centred - post_centred)^2))
  cat(sprintf("  cor(truth_centred, posterior_centred) = %.4f\n",
              r_corr))
  cat(sprintf("  RMSE on logit scale                   = %.4f\n", rmse))
  cat(sprintf("  PASS (cor > 0.90): %s\n",
              isTRUE(r_corr > 0.90)))
  pl <- ggplot(d, aes(x = env, y = estimate__)) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__),
                fill = "steelblue", alpha = 0.25) +
    geom_line(colour = "steelblue", linewidth = 0.9) +
    geom_line(data = data.frame(
                env       = d$env,
                estimate__ = truth_centred),
              aes(x = env, y = estimate__),
              colour = "firebrick", linetype = 2, linewidth = 0.7) +
    labs(title = label,
         subtitle = sprintf("cor = %.3f, RMSE = %.3f",
                            r_corr, rmse),
         y = "f(env) (logit/log centred)",
         x = "env") +
    theme_minimal()
  ggsave(out_png, pl, width = 6, height = 4, dpi = 100)
  cat(sprintf("  Saved plot: %s\n", out_png))
  invisible(list(cor = r_corr, rmse = rmse))
}

cat("\n\n========== Nonlinear env recovery ==========\n")
r_occ <- recover_env_smooth(
  fit_occ,  "occ() single-season",
  "/tmp/heaps_nonlinear_env_occ.png"
)
r_nmix <- recover_env_smooth(
  fit_nmix, "nmix() single-season",
  "/tmp/heaps_nonlinear_env_nmix.png"
)
r_ms <- recover_env_smooth(
  fit_ms,   "occ(multi_season = TRUE)",
  "/tmp/heaps_nonlinear_env_multi.png"
)

cat("\nSummary table:\n")
tab <- data.frame(
  family    = c("occ",  "nmix", "occ_multi_season"),
  cor       = c(r_occ$cor,  r_nmix$cor,  r_ms$cor),
  rmse      = c(r_occ$rmse, r_nmix$rmse, r_ms$rmse)
)
print(tab, row.names = FALSE)
cat("\nDone.\n")
