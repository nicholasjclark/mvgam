# Build the cached mvgam fixtures that tests/local/ reads, and run
# the recovery scripts that build their own.
#
# Run from the package root:
#   Rscript tests/local/build_fixtures.R
#
# Output: tests/local/fixtures/val_mvgam_<name>.rds for each fixture
#         below. The directory is gitignored; rebuild after a clone.
#
# Expected runtime: 15-25 minutes for the fixtures fitted here, plus a
# few hours for the recovery scripts registered at the end of this
# file. Re-runs after partial completion are incremental: only missing
# fixtures are refit.

suppressMessages(devtools::load_all())
library(brms)

CHAINS <- 2
ITER <- 1000
WARMUP <- 500
REFRESH <- 0
FIXTURE_DIR <- "tests/local/fixtures"

if (!dir.exists(FIXTURE_DIR)) {
  dir.create(FIXTURE_DIR, recursive = TRUE)
}


fit_mvgam_cached <- function(name, formula, trend_formula, data, family, ...) {
  path <- file.path(FIXTURE_DIR, paste0("val_mvgam_", name, ".rds"))
  if (file.exists(path)) {
    cat("  cached mvgam:", name, "\n")
    return(readRDS(path))
  }
  cat("  fitting mvgam:", name, "\n")
  fit <- mvgam(
    formula = formula, trend_formula = trend_formula,
    data = data, family = family,
    chains = CHAINS, iter = ITER, warmup = WARMUP,
    refresh = REFRESH, silent = 2, backend = "cmdstanr", ...
  )
  saveRDS(fit, path)
  fit
}

fit_jsdgam_cached <- function(name, formula, factor_formula, data,
                              n_lv, ...) {
  path <- file.path(FIXTURE_DIR, paste0("val_jsdgam_", name, ".rds"))
  if (file.exists(path)) {
    cat("  cached jsdgam:", name, "\n")
    return(readRDS(path))
  }
  cat("  fitting jsdgam:", name, "\n")
  fit <- jsdgam(
    formula = formula, factor_formula = factor_formula,
    data = data, n_lv = n_lv,
    chains = CHAINS, iter = ITER, warmup = WARMUP,
    refresh = REFRESH, silent = 2, backend = "cmdstanr", ...
  )
  saveRDS(fit, path)
  fit
}


cat("=== Building tests/local/fixtures/ ===\n\n")

# ----------------------------------------------------------------------
# SHARED TEST DATA
# ----------------------------------------------------------------------

# One AR(1) path, drawn from the stationary distribution so the
# series has the marginal variance its parameters imply rather than
# starting at zero and growing into it.
sim_ar1 <- function(n, ar, sd) {
  out <- numeric(n)
  out[1] <- rnorm(1, 0, sd / sqrt(1 - ar^2))
  for (t in 2:n) out[t] <- ar * out[t - 1] + rnorm(1, 0, sd)
  out
}


set.seed(42)
n_time <- 30
ar_coef <- 0.7
sigma <- 0.5
latent <- sim_ar1(n_time, ar_coef, sigma)
z <- seq(-2, 2, length.out = n_time)
z_effect <- 0.5 * sin(z * pi)
test_data <- data.frame(
  y = rpois(n_time, exp(2 + latent + z_effect)),
  x = rnorm(n_time),
  z = z,
  time = 1:n_time,
  series = factor("s1"),
  grp = factor(rep(letters[1:6], each = 5))
)


# ----------------------------------------------------------------------
# MAIN OBS-FORMULA GRID: Poisson AR(1)
# ----------------------------------------------------------------------

cat("\n[1] Intercept-only AR(1)\n")
fit_mvgam_cached("ar1_int",
  y ~ 1, ~ AR(p = 1),
  test_data, poisson())

cat("\n[2] AR(1) + fixed effect\n")
fit_mvgam_cached("ar1_fx",
  y ~ 1 + x, ~ AR(p = 1),
  test_data, poisson())

# ----------------------------------------------------------------------
# TREND-FORMULA VARIANTS (mvgam-only; brms cannot move covariates into
# the autocor block, so these test trend-side prediction logic)
# ----------------------------------------------------------------------

cat("\n[8] AR(1) + fixed (in trend)\n")
fit_mvgam_cached("ar1_fx_trend",
  y ~ 1, ~ x + AR(p = 1),
  test_data, poisson())

# ----------------------------------------------------------------------
# MULTIVARIATE mvbind + shared AR(1)
# ----------------------------------------------------------------------

cat("\n[10] Multivariate mvbind (2 responses)\n")
set.seed(789)
# x must be informative for both responses so the fixed-effect
# coefficient is well identified with a stable sign; otherwise the
# deterministic-linpred concordance check reduces to sign(b_x), which
# flips at random between fits when x carries no signal.
x_mv <- rnorm(n_time)
test_data_mv <- data.frame(
  y1 = rnorm(n_time, mean = 1 + 0.8 * x_mv + latent),
  y2 = rnorm(n_time, mean = 2 - 0.6 * x_mv + latent),
  x = x_mv,
  time = 1:n_time,
  series = factor("s1")
)
fit_mvgam_cached("mv_gauss",
  bf(mvbind(y1, y2) ~ 1 + x) + set_rescor(FALSE),
  ~ AR(p = 1),
  test_data_mv, gaussian())

cat("\n[20] Gaussian AR(1), N=150: PSIS-stable fixture\n")
# Larger N with high signal-to-noise keeps Pareto-k diagnostics in
# the stable region (<0.7), which is what lets the PSIS-weighted
# surfaces (loo_epred, loo_linpred, loo_predictive_interval) be read
# for their own values rather than for their diagnostics.
set.seed(7)
gauss_n <- 150L
gauss_ar <- 0.5
gauss_sigma <- 0.4
gauss_latent <- numeric(gauss_n)
gauss_latent[1L] <- stats::rnorm(
  1L, 0, gauss_sigma / sqrt(1 - gauss_ar^2)
)
for (t in 2:gauss_n) {
  gauss_latent[t] <- gauss_ar * gauss_latent[t - 1L] +
    stats::rnorm(1L, 0, gauss_sigma)
}
gauss_x <- stats::rnorm(gauss_n)
test_data_gauss <- data.frame(
  y = 1.0 + 1.5 * gauss_x + gauss_latent +
    stats::rnorm(gauss_n, 0, 0.3),
  x = gauss_x,
  time = seq_len(gauss_n),
  series = factor("s1")
)
fit_mvgam_cached("gauss_ar1_n150",
  y ~ 1 + x, ~ AR(p = 1),
  test_data_gauss, gaussian())

# ----------------------------------------------------------------------
# [21] VAR(p = 1) cor=TRUE on 3 series - residual_cor fixture
# ----------------------------------------------------------------------
cat("\n[21] VAR(1) 3-series correlated trend\n")
set.seed(11)
n_t <- 40L
n_sv <- 3L
A_true <- matrix(c(0.5, 0.1, 0.0,
                   0.1, 0.4, 0.1,
                   0.0, 0.1, 0.3), n_sv, n_sv, byrow = TRUE)
L_true <- t(chol(matrix(c(0.4, 0.15, 0.05,
                          0.15, 0.4, 0.1,
                          0.05, 0.1, 0.3), n_sv, n_sv, byrow = TRUE)))
lat <- matrix(0, n_t, n_sv)
for (t in 2:n_t) {
  lat[t, ] <- A_true %*% lat[t - 1L, ] + L_true %*% stats::rnorm(n_sv)
}
test_data_var <- data.frame(
  y = as.vector(rpois(n_t * n_sv, exp(1 + as.vector(lat)))),
  series = factor(rep(paste0("s", 1:n_sv), each = n_t),
                  levels = paste0("s", 1:n_sv)),
  time = rep(seq_len(n_t), times = n_sv)
)
fit_mvgam_cached("var_cor",
  y ~ 1, ~ VAR(p = 1),
  test_data_var, poisson())

# ----------------------------------------------------------------------
# [23] AR(1) factor model with fixed Z via trend_map (dense matrix).
# Four series load on two latent factors with known dense weights.
# Exercises the fixed-Z Stan path end-to-end: standata carries Z,
# Stan compiles, posterior recovers the latent trends.
# ----------------------------------------------------------------------
cat("\n[23] AR(1) factor model with fixed Z via trend_map\n")
set.seed(17)
n_t_tm <- 50L
n_series_tm <- 4L
n_lv_tm <- 2L
ar_tm <- c(0.7, 0.3)
sigma_tm <- c(0.4, 0.4)
Z_true <- matrix(
  c(1.0, 0.0,
    0.8, 0.2,
    0.0, 1.0,
    0.3, 0.7),
  nrow = n_series_tm, ncol = n_lv_tm, byrow = TRUE
)
lv_tm <- matrix(0, n_t_tm, n_lv_tm)
for (k in seq_len(n_lv_tm)) {
  for (t in 2:n_t_tm) {
    lv_tm[t, k] <- ar_tm[k] * lv_tm[t - 1L, k] +
      stats::rnorm(1L, 0, sigma_tm[k])
  }
}
mu_tm <- 1 + lv_tm %*% t(Z_true)
test_data_tm <- data.frame(
  y = as.vector(rpois(n_t_tm * n_series_tm,
                      exp(as.vector(mu_tm)))),
  series = factor(
    rep(paste0("s", seq_len(n_series_tm)), each = n_t_tm),
    levels = paste0("s", seq_len(n_series_tm))
  ),
  time = rep(seq_len(n_t_tm), times = n_series_tm)
)
attr(test_data_tm, "Z_true") <- Z_true
fit_trend_map_cached <- function(name, Z_user) {
  path <- file.path(FIXTURE_DIR, paste0("val_mvgam_", name, ".rds"))
  if (file.exists(path)) {
    cat("  cached mvgam:", name, "\n")
    return(readRDS(path))
  }
  cat("  fitting mvgam:", name, "\n")
  fit <- mvgam(
    formula = y ~ 1,
    trend_formula = ~ -1 + AR(p = 1, trend_map = Z_user, cor = TRUE),
    data = test_data_tm,
    family = poisson(),
    chains = CHAINS, iter = ITER, warmup = WARMUP,
    refresh = REFRESH, silent = 2, backend = "cmdstanr"
  )
  saveRDS(fit, path)
  fit
}
# The same four series and two factors with `Z` sampled rather than
# supplied, which is the path `residual_cor()` reads as
# `pattern = "factor_loadings"` and the one
# `tests/local/test-factor-forecast.R` forecasts through. No script
# built it before, so a clean clone could not regenerate what those
# two tests consume.
fit_mvgam_cached("lv_factor",
  y ~ 1, ~ -1 + AR(p = 1, n_lv = 2),
  test_data_tm, poisson())

# ----------------------------------------------------------------------
# STRUCTURED-PRIOR FACTOR MODEL: loadings_prior end-to-end fixture
# Eight series live on two trait clusters with a 1D continuous trait
# and a 2-level cluster indicator. Series within a cluster share factor
# loadings; the structured prior should learn this from a feature
# matrix plus a pairwise distance matrix built from the cluster split.
# ----------------------------------------------------------------------
cat("\n[24] AR(1) factor model with loadings_prior\n")
set.seed(8024L)
n_t_lp <- 60L
n_series_lp <- 8L
n_lv_lp <- 3L
ar_lp <- c(0.75, 0.55, 0.45)
sigma_lp <- rep(0.45, n_lv_lp)
series_lp <- paste0("s", seq_len(n_series_lp))
cluster_lp <- factor(rep(c("A", "B"), each = n_series_lp / 2L),
                     levels = c("A", "B"))
trait_lp <- c(seq(-1, 1, length.out = n_series_lp / 2L),
              seq(-1, 1, length.out = n_series_lp / 2L))
# Cluster A loads on factors 1 + 3; cluster B loads on factors 2 + 3;
# the shared factor 3 mixes positively for everyone. The third factor
# carries a smooth modulation by `trait_lp`.
Z_lp <- matrix(0, nrow = n_series_lp, ncol = n_lv_lp)
Z_lp[cluster_lp == "A", 1L] <- 0.9 +
  0.1 * trait_lp[cluster_lp == "A"]
Z_lp[cluster_lp == "B", 2L] <- 0.9 -
  0.1 * trait_lp[cluster_lp == "B"]
Z_lp[, 3L] <- 0.5 + 0.3 * trait_lp
rownames(Z_lp) <- series_lp
# Latent AR(1) factors.
lv_lp <- matrix(0, n_t_lp, n_lv_lp)
for (k in seq_len(n_lv_lp)) {
  lv_lp[1L, k] <- stats::rnorm(
    1L, 0, sigma_lp[k] / sqrt(1 - ar_lp[k]^2)
  )
  for (t in 2:n_t_lp) {
    lv_lp[t, k] <- ar_lp[k] * lv_lp[t - 1L, k] +
      stats::rnorm(1L, 0, sigma_lp[k])
  }
}
mu_lp <- lv_lp %*% t(Z_lp)
obs_sigma_lp <- 0.25
y_lp <- as.vector(mu_lp) +
  stats::rnorm(n_t_lp * n_series_lp, 0, obs_sigma_lp)
test_data_lp <- data.frame(
  series = factor(rep(series_lp, each = n_t_lp), levels = series_lp),
  time = rep(seq_len(n_t_lp), times = n_series_lp),
  y = y_lp
)
features_lp <- data.frame(
  series = series_lp,
  trait = trait_lp,
  cluster = cluster_lp
)
# Hierarchical distance: 0 within cluster, 1 across cluster.
d_cluster_lp <- as.matrix(
  stats::dist(as.numeric(cluster_lp), method = "manhattan")
)
rownames(d_cluster_lp) <- colnames(d_cluster_lp) <- series_lp
fit_loadings_prior_cached <- function(name) {
  path <- file.path(
    FIXTURE_DIR, paste0("val_mvgam_", name, ".rds")
  )
  if (file.exists(path)) {
    cat("  cached mvgam:", name, "\n")
    return(readRDS(path))
  }
  cat("  fitting mvgam:", name, "\n")
  fit <- mvgam(
    formula = y ~ 1,
    trend_formula = ~ AR(p = 1, n_lv = n_lv_lp),
    data = test_data_lp,
    family = gaussian(),
    data2 = list(features = features_lp, cluster = d_cluster_lp),
    loadings_prior = list(
      features = "features",
      distances = "cluster"
    ),
    chains = CHAINS, iter = ITER, warmup = WARMUP,
    refresh = REFRESH, silent = 2, backend = "cmdstanr"
  )
  saveRDS(fit, path)
  fit
}
fit_loadings_prior_cached("loadings_prior")

# ----------------------------------------------------------------------
# NON-LINEAR FORMULAS (bf(..., nl = TRUE))
# Every downstream prediction surface (linpred, epred, predict) on
# an nl fit carries its own non-linear parameters through to Stan.
# Two shapes covered: an intercept-only nl growth model and a
# trait-mediated fourth-corner model with per-species random
# effects.
# ----------------------------------------------------------------------

set.seed(20260618L)
nl_growth_data <- data.frame(
  x = seq(-2, 2, length.out = 60L)
)
nl_growth_data$y <- 0.8 * exp(0.5 * nl_growth_data$x) +
  rnorm(nrow(nl_growth_data), 0, 0.1)
nl_growth_data$time   <- seq_len(nrow(nl_growth_data))
nl_growth_data$series <- factor("s1")

cat("\n[nl-1] bf(y ~ b1 * exp(b2 * x), b1 + b2 ~ 1, nl = TRUE)\n")
nl_growth_pri <- prior(normal(1, 1), nlpar = "b1") +
  prior(normal(0, 1), nlpar = "b2")
set.seed(20260618L)
nl_S <- 5L; nl_n_site <- 12L
nl_traits <- data.frame(
  species = factor(paste0("sp", seq_len(nl_S))),
  trait1  = scale(rnorm(nl_S))[, 1]
)
nl_trait_data <- expand.grid(
  site    = factor(paste0("st", seq_len(nl_n_site))),
  species = nl_traits$species
)
nl_trait_data$env <- rnorm(nrow(nl_trait_data))
nl_trait_data <- merge(nl_trait_data, nl_traits, by = "species")
nl_trait_data$y <- with(nl_trait_data,
  0.5 + (0.3 + 0.7 * trait1) * env +
    rnorm(nrow(nl_trait_data), 0, 0.3)
)
nl_trait_data$time   <- as.integer(nl_trait_data$site)
nl_trait_data$series <- nl_trait_data$species

cat("\n[nl-2] trait-mediated fourth-corner\n")
nl_trait_pri <- prior(normal(0, 1), nlpar = "a") +
  prior(normal(0, 1), nlpar = "b") +
  prior(student_t(3, 0, 2.5), class = "sd", nlpar = "a") +
  prior(student_t(3, 0, 2.5), class = "sd", nlpar = "b")
nl_trait_form <- bf(
  y  ~ a + b * env,
  a  ~ trait1 + (1 | species),
  b  ~ trait1 + (1 | species),
  nl = TRUE
)
# ----------------------------------------------------------------------
# NORMALIZE PAIR: the same data fitted with and without the
# normalising constants. `normalize` changes only what Stan adds to
# `target`, so the two posteriors have to agree. If the GLM path
# recognised only the normalised `_lpmf` spelling when naming the
# family, `normalize = FALSE` would compute the trend but never add
# it to the linear predictor, and the fit would run clean while
# modelling no trend at all. Text checks on the Stan cannot catch
# that; two fits and a comparison can.
# ----------------------------------------------------------------------

cat("\n[19] normalize = TRUE / FALSE pair\n")
set.seed(101)
norm_n_time <- 40L
norm_n_series <- 3L
norm_latent <- as.numeric(arima.sim(list(ar = 0.8), norm_n_time)) * 0.8
norm_data <- data.frame(
  time = rep(seq_len(norm_n_time), norm_n_series),
  series = factor(rep(paste0("s", seq_len(norm_n_series)),
                      each = norm_n_time)),
  x1 = rnorm(norm_n_time * norm_n_series)
)
# Real amplitude on the latent process, so a fit that drops the trend
# cannot resemble one that keeps it.
norm_data$y <- rpois(
  nrow(norm_data),
  exp(1.0 + 0.4 * norm_data$x1 + rep(norm_latent, norm_n_series))
)
fit_mvgam_cached("normalize_on", y ~ x1, ~ AR(p = 1),
  norm_data, poisson(), normalize = TRUE, seed = 7)
fit_mvgam_cached("normalize_off", y ~ x1, ~ AR(p = 1),
  norm_data, poisson(), normalize = FALSE, seed = 7)

# ----------------------------------------------------------------------
# CLOSURE-UNIT LABELLING: two species, so the closure-unit grid has a
# series axis as well as a time axis. `hindcast(type = "latent_state")`
# built its own (series, time) grid and sorted it series-major, while
# the kernel numbers units in first-appearance order over time-major
# data. Every unit was relabelled, and a species could be handed the
# abundance belonging to another. A single-series fit cannot show it,
# because the two orderings coincide.
# ----------------------------------------------------------------------

cat("\n[20] closure-unit labelling: two-species nmix and occ\n")
set.seed(999)
nmix_truth <- list(sp_1 = c(28, 26, 23, 16, 14, 14),
                   sp_2 = c(4, 7, 15, 16, 19, 18))
nmix_p <- c(sp_1 = 0.7, sp_2 = 0.45)
nmix_data <- do.call(rbind, lapply(names(nmix_truth), function(sp) {
  data.frame(
    series = sp,
    time = sort(rep(1:6, 5)),
    visit = rep(1:5, 6),
    truth = rep(nmix_truth[[sp]], each = 5),
    y = unlist(lapply(nmix_truth[[sp]], function(N) {
      rbinom(5, N, nmix_p[[sp]])
    })),
    cap = if (sp == "sp_1") 100L else 50L
  )
}))
nmix_data$series <- factor(nmix_data$series, levels = names(nmix_truth))
nmix_data <- nmix_data[order(nmix_data$time, nmix_data$series,
                             nmix_data$visit), ]
fit_mvgam_cached("closure_labels_nmix",
  bf(y ~ s(time, k = 4, by = series) + series, p ~ series), NULL,
  nmix_data, nmix(), seed = 3)

set.seed(7)
occ_data <- do.call(rbind, lapply(
  list(list(sp = "sp_1", psi = 0.85, p = 0.7),
       list(sp = "sp_2", psi = 0.30, p = 0.5)),
  function(cfg) {
    do.call(rbind, lapply(1:8, function(tt) {
      z <- rbinom(1, 1, cfg$psi)
      data.frame(series = cfg$sp, time = tt, visit = 1:4,
                 y = z * rbinom(4, 1, cfg$p))
    }))
  }
))
occ_data$series <- factor(occ_data$series)
occ_data <- occ_data[order(occ_data$time, occ_data$series,
                           occ_data$visit), ]
fit_mvgam_cached("closure_labels_occ", bf(y ~ series, p ~ series), NULL,
  occ_data, occ(), seed = 4)

# A factor fit at the truncation ceiling. The MGP column-shrinkage
# prior is what admits `n_lv = n_series`, where the two Stan trend
# dimensions coincide and only the requested `n_lv` says the fit
# sampled a free Z. Every other cached factor fit sits below the
# ceiling, so this is the one that reaches the post-fit code paths
# choosing between latent and series grain.
set.seed(11)
n_t_mgp <- 30L
n_series_mgp <- 3L
series_mgp <- paste0("sp_", seq_len(n_series_mgp))
ar_mgp <- c(0.7, 0.4, 0.55)
Z_mgp <- matrix(
  c(0.9, 0.1, 0.2,
    0.2, 0.8, 0.1,
    0.1, 0.3, 0.7),
  nrow = n_series_mgp, byrow = TRUE
)
lv_mgp <- matrix(0, n_t_mgp, n_series_mgp)
for (k in seq_len(n_series_mgp)) {
  lv_mgp[1L, k] <- rnorm(1L, 0, 1 / sqrt(1 - ar_mgp[k]^2))
  for (t in 2:n_t_mgp) {
    lv_mgp[t, k] <- ar_mgp[k] * lv_mgp[t - 1L, k] + rnorm(1L)
  }
}
mu_mgp <- lv_mgp %*% t(Z_mgp)
mgp_data <- data.frame(
  series = factor(rep(series_mgp, each = n_t_mgp), levels = series_mgp),
  time = rep(seq_len(n_t_mgp), times = n_series_mgp),
  y = as.vector(mu_mgp) + rnorm(n_t_mgp * n_series_mgp, 0, 0.25)
)
fit_mvgam_cached(
  "mgp_ceiling",
  y ~ 1, ~ AR(p = 1, n_lv = n_series_mgp), mgp_data, gaussian(),
  loadings_prior = list(column_shrinkage = "mgp"), seed = 11
)

# ----------------------------------------------------------------------
# Five wide-frame fixtures once stood here: a JSDM with one family per
# species, and four `mvbf()` shapes covering a missing series column,
# an odd response count, a multi-series panel and gappy responses.
# Nothing read any of them. `test-mvbf-wide.R` builds the wide frame
# it tests, so the five were being fitted on every clean build to be
# stored and never opened.
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
# RECOVERY SCRIPTS
#
# Each script registered below simulates its own data from a known
# truth, fits one or more models and caches them in FIXTURE_DIR with
# the generative truth attached to the fit as its `sim_truth`
# attribute. The script is the authority on its own simulation, so the
# registry names the fixtures it produces and runs it when any of them
# is missing, rather than restating the simulation here.
#
# Each script runs in its own R process, which is the invocation its
# own header documents (`Rscript tests/local/<script>`) and which
# keeps its globals out of this one. A script whose packages are not
# installed is reported and passed over. Building the whole set from
# nothing takes a few hours; a run after a partial build fills only
# the gaps.
#
# tests/local/savage_hierarchical_var.R also caches a fit but is not
# registered here: its panel is a live WDI download rather than a
# simulation, so building it would make a fixture rebuild depend on
# network access.
# ----------------------------------------------------------------------

recovery <- function(script, fixtures, needs = character(0)) {
  list(script = script, fixtures = fixtures, needs = needs)
}

# Attached by every long-format JSDM recovery script.
JSDM_PKGS <- c("dplyr", "tidyr", "posterior", "testthat")

recovery_scripts <- list(
  # The seven long-format families are built by the file that tests
  # them, which keeps each family's simulation next to the
  # assertions that read it.
  recovery("test-jsdgam-families.R",
           paste0("val_mvgam_jsdgam_mv_",
                  c("mvn", "mvt", "nb", "beta", "diri", "categ",
                    "multi"), ".rds"),
           JSDM_PKGS),
  # The two closure-unit factor fits and the multi-season one are
  # built and cached by the file that asserts on them.
  recovery("test-closure-units.R",
           c("val_mvgam_jsdgam_mv_nmix.rds",
             "val_mvgam_jsdgam_mv_occ.rds",
             "val_mvgam_occ_multi_season.rds"),
           c("dplyr", "posterior", "testthat")),
  recovery("diri_phi_subformula_fit.R",
           "val_mvgam_diri_phi_subformula.rds", JSDM_PKGS),
  recovery("kfold_grouped_cv.R",
           c("val_mvgam_kfold_demo_gauss.rds",
             "val_mvgam_kfold_demo_occ.rds"),
           "ggplot2"),
  recovery("jsdgam_ordinate_traits.R",
           c("val_mvgam_ordinate_traits.rds",
             "val_mvgam_ordinate_traits_data.rds"),
           "ggplot2"),
  # The three structured-prior fits live in one file, which builds
  # and caches each of them itself.
  recovery("test-loadings-prior.R",
           c("val_mvgam_heaps_birds.rds",
             "val_mvgam_heaps_birds_wide.rds",
             "val_mvgam_heaps_birds_phylo_dominant.rds"),
           "ape")
)

run_recovery_script <- function(script) {
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    args = shQuote(file.path("tests", "local", script))
  )
  if (!identical(status, 0L)) {
    stop(insight::format_error(c(
      "Recovery fixture script failed.",
      x = paste0("'", script, "' exited with status ", status, "."),
      i = "Run it directly to see the failure it reported."
    )))
  }
  invisible(NULL)
}

cat("\n=== Recovery-script fixtures ===\n")
for (rec in recovery_scripts) {
  paths <- file.path(FIXTURE_DIR, rec$fixtures)
  if (all(file.exists(paths))) {
    cat("  cached recovery:", rec$script, "\n")
    next
  }
  absent <- rec$needs[
    !vapply(rec$needs, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(absent) > 0) {
    cat("  skipping recovery:", rec$script,
        "- needs", paste(absent, collapse = ", "), "\n")
    next
  }
  cat("  running recovery:", rec$script, "\n")
  run_recovery_script(rec$script)
}

cat("\n=== All fixtures present in", FIXTURE_DIR, "===\n")
cat("Files: ", length(list.files(FIXTURE_DIR, pattern = "\\.rds$")), "\n")
