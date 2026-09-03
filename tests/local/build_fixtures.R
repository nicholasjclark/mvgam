# Build the cached brms and mvgam fixtures that tests/local/ reads,
# and run the recovery scripts that build their own.
#
# Run from the package root:
#   Rscript tests/local/build_fixtures.R
#
# Output: tests/local/fixtures/val_brms_<name>.rds and
#         tests/local/fixtures/val_mvgam_<name>.rds for each fixture
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

fit_brms_cached <- function(name, formula, data, family, ...) {
  path <- file.path(FIXTURE_DIR, paste0("val_brms_", name, ".rds"))
  if (file.exists(path)) {
    cat("  cached brms:", name, "\n")
    return(readRDS(path))
  }
  cat("  fitting brms:", name, "\n")
  fit <- brm(
    formula = formula, data = data, family = family,
    chains = CHAINS, iter = ITER, warmup = WARMUP,
    refresh = REFRESH, silent = 2, backend = "cmdstanr", ...
  )
  saveRDS(fit, path)
  fit
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

# A zero-truncated draw, which is what the positive part of a hurdle
# family is: the zeros come from the hurdle, never from the count.
r_trunc <- function(draw) {
  repeat {
    v <- draw()
    if (v > 0) return(v)
  }
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

# High-signal Poisson AR(1) data (used by process_error toggle test)
set.seed(456)
hs_n <- 60
hs_x <- seq(-2, 2, length.out = hs_n)
hs_latent <- sim_ar1(hs_n, ar_coef, sigma)
test_data_hs <- data.frame(
  y = rpois(hs_n, exp(0.5 + 1.5 * hs_x + hs_latent)),
  x = hs_x,
  time = 1:hs_n,
  series = factor("s1")
)

# ----------------------------------------------------------------------
# MAIN OBS-FORMULA GRID: Poisson AR(1)
# ----------------------------------------------------------------------

cat("\n[1] Intercept-only AR(1)\n")
fit_brms_cached("ar1_int",
  y ~ 1 + ar(time = time, p = 1, cov = TRUE),
  test_data, poisson())
fit_mvgam_cached("ar1_int",
  y ~ 1, ~ AR(p = 1),
  test_data, poisson())

cat("\n[2] AR(1) + fixed effect\n")
fit_brms_cached("ar1_fx",
  y ~ 1 + x + ar(time = time, p = 1, cov = TRUE),
  test_data, poisson())
fit_mvgam_cached("ar1_fx",
  y ~ 1 + x, ~ AR(p = 1),
  test_data, poisson())

cat("\n[3] AR(1) + random intercept\n")
fit_brms_cached("ar1_re",
  y ~ 1 + x + (1 | grp) + ar(time = time, p = 1, cov = TRUE),
  test_data, poisson())
fit_mvgam_cached("ar1_re",
  y ~ 1 + x + (1 | grp), ~ AR(p = 1),
  test_data, poisson())

cat("\n[4] AR(1) + fixed + random + smooth\n")
fit_brms_cached("ar1_re_smooth",
  y ~ 1 + x + (1 | grp) + s(z) + ar(time = time, p = 1, cov = TRUE),
  test_data, poisson())
fit_mvgam_cached("ar1_re_smooth",
  y ~ 1 + x + (1 | grp) + s(z), ~ AR(p = 1),
  test_data, poisson())

cat("\n[5] AR(1) + correlated REs\n")
fit_brms_cached("ar1_cor_re",
  y ~ 1 + x + (x | grp) + ar(time = time, p = 1, cov = TRUE),
  test_data, poisson())
fit_mvgam_cached("ar1_cor_re",
  y ~ 1 + x + (x | grp), ~ AR(p = 1),
  test_data, poisson())

cat("\n[6] AR(1) + monotonic mo()\n")
test_data_mo <- test_data
test_data_mo$ord_factor <- ordered(cut(test_data_mo$z, 4))
fit_brms_cached("ar1_mo",
  y ~ 1 + mo(ord_factor) + ar(time = time, p = 1, cov = TRUE),
  test_data_mo, poisson())
fit_mvgam_cached("ar1_mo",
  y ~ 1 + mo(ord_factor), ~ AR(p = 1),
  test_data_mo, poisson())

cat("\n[7] AR(1) + GP(z)\n")
fit_brms_cached("ar1_gp",
  y ~ 1 + gp(z, k = 10) + ar(time = time, p = 1, cov = TRUE),
  test_data, poisson())
fit_mvgam_cached("ar1_gp",
  y ~ 1 + gp(z, k = 10), ~ AR(p = 1),
  test_data, poisson())

# GP fixtures with extra covariates `w` and `cat`, reconstructed
# here so fixture rebuilds reproduce the gp2_by / gp2d / gp2d_by
# pairs deterministically.
test_data_gp2 <- test_data
test_data_gp2$w <- seq(-1, 1, length.out = nrow(test_data_gp2))
test_data_gp2$cat <- factor(
  rep(c("A", "B"), length.out = nrow(test_data_gp2)),
  levels = c("A", "B")
)

cat("\n[7a] AR(1) + GP(z) + GP(w, by = cat)\n")
fit_brms_cached("ar1_gp2_by",
  y ~ 1 + gp(z, k = 5) + gp(w, by = cat, k = 5) +
    ar(time = time, p = 1, cov = TRUE),
  test_data_gp2, poisson())
fit_mvgam_cached("ar1_gp2_by",
  y ~ 1 + gp(z, k = 5) + gp(w, by = cat, k = 5), ~ AR(p = 1),
  test_data_gp2, poisson())

cat("\n[7b] AR(1) + 2D GP(z, w)\n")
fit_brms_cached("ar1_gp2d",
  y ~ 1 + gp(z, w, k = 5) + ar(time = time, p = 1, cov = TRUE),
  test_data_gp2, poisson())
fit_mvgam_cached("ar1_gp2d",
  y ~ 1 + gp(z, w, k = 5), ~ AR(p = 1),
  test_data_gp2, poisson())

cat("\n[7c] AR(1) + 2D GP(z, w, by = cat), multi-dim by-factor\n")
fit_brms_cached("ar1_gp2d_by",
  y ~ 1 + gp(z, w, by = cat, k = 5) +
    ar(time = time, p = 1, cov = TRUE),
  test_data_gp2, poisson())
fit_mvgam_cached("ar1_gp2d_by",
  y ~ 1 + gp(z, w, by = cat, k = 5), ~ AR(p = 1),
  test_data_gp2, poisson())

# ----------------------------------------------------------------------
# TREND-FORMULA VARIANTS (mvgam-only; brms cannot move covariates into
# the autocor block, so these test trend-side prediction logic)
# ----------------------------------------------------------------------

cat("\n[8] AR(1) + fixed (in trend)\n")
fit_mvgam_cached("ar1_fx_trend",
  y ~ 1, ~ x + AR(p = 1),
  test_data, poisson())

cat("\n[9] AR(1) + fixed + random + smooth (in trend)\n")
fit_mvgam_cached("ar1_re_smooth_trend",
  y ~ 1, ~ x + (1 | grp) + s(z) + AR(p = 1),
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
fit_brms_cached("mv_gauss",
  bf(mvbind(y1, y2) ~ 1 + x + ar(time = time, p = 1, cov = TRUE)) +
    set_rescor(FALSE),
  test_data_mv, gaussian())
fit_mvgam_cached("mv_gauss",
  bf(mvbind(y1, y2) ~ 1 + x) + set_rescor(FALSE),
  ~ AR(p = 1),
  test_data_mv, gaussian())

# ----------------------------------------------------------------------
# FAMILY COVERAGE
# ----------------------------------------------------------------------

cat("\n[11] Beta AR(1)\n")
# Carries a latent AR(1) and a covariate that the response actually
# depends on. Drawing `y` from a fixed Beta and regressing it on an
# unrelated `x` leaves nothing for either term to recover, so a fit
# on such data agrees with any other fit on it and the concordance
# says nothing about whether the trend or the covariate works.
set.seed(456)
n_beta <- 100
beta_latent <- sim_ar1(n_beta, ar_coef, 0.7)
beta_x <- rnorm(n_beta)
beta_phi <- 8
beta_mu <- plogis(-0.4 + 0.8 * beta_x + beta_latent)
test_data_beta <- data.frame(
  y = pmax(pmin(rbeta(n_beta, beta_mu * beta_phi,
                      (1 - beta_mu) * beta_phi), 0.999), 0.001),
  x = beta_x,
  time = 1:n_beta,
  series = factor("s1")
)
fit_brms_cached("beta_ar1",
  y ~ 1 + x + ar(time = time, p = 1, cov = TRUE),
  test_data_beta, Beta())
fit_mvgam_cached("beta_ar1",
  y ~ 1 + x, ~ AR(p = 1),
  test_data_beta, Beta())

cat("\n[12] Binomial AR(1)\n")
set.seed(789)
n_binom <- 100
trials_vec <- rep(20, n_binom)
binom_latent <- sim_ar1(n_binom, ar_coef, 0.7)
binom_x <- rnorm(n_binom)
binom_p <- plogis(-0.2 + 0.7 * binom_x + binom_latent)
test_data_binom <- data.frame(
  y = rbinom(n_binom, size = trials_vec, prob = binom_p),
  trials = trials_vec,
  x = binom_x,
  time = 1:n_binom,
  series = factor("s1")
)
fit_brms_cached("binom_ar1",
  y | trials(trials) ~ 1 + x + ar(time = time, p = 1, cov = TRUE),
  test_data_binom, binomial())
fit_mvgam_cached("binom_ar1",
  y | trials(trials) ~ 1 + x, ~ AR(p = 1),
  test_data_binom, binomial())

cat("\n[13] Ordinal (Cumulative), fixed effects only (no AR)\n")
set.seed(456)
n_ord <- 30
ord_latent <- 1.0 + 0.5 * rnorm(n_ord)
ord_cuts <- c(-Inf, -0.5, 0.5, 1.5, Inf)
test_data_ord <- data.frame(
  y = ordered(cut(ord_latent, breaks = ord_cuts,
                   labels = c("Low", "Med", "High", "VHigh"))),
  x = rnorm(n_ord),
  z = rnorm(n_ord),
  time = 1:n_ord,
  series = factor("s1")
)
fit_brms_cached("cumulative_fx",
  y ~ 1 + x + z,
  test_data_ord, cumulative())
fit_mvgam_cached("cumulative_fx",
  y ~ 1 + x + z, ~ ZMVN(),
  test_data_ord, cumulative())

cat("\n[14] Hurdle Poisson AR(1)\n")
set.seed(654)
n_hp <- 120
hp_latent <- sim_ar1(n_hp, ar_coef, 0.7)
hp_x <- rnorm(n_hp)
hp_mu <- exp(1.2 + 0.5 * hp_x + hp_latent)
hp_hu <- 0.25
test_data_hp <- data.frame(
  y = ifelse(
    runif(n_hp) < hp_hu, 0L,
    vapply(hp_mu, function(m) r_trunc(function() rpois(1, m)),
           numeric(1))
  ),
  x = hp_x,
  time = 1:n_hp,
  series = factor("s1")
)
fit_brms_cached("hurdle_poisson_ar1",
  y ~ 1 + x + ar(time = time, p = 1, cov = TRUE),
  test_data_hp, hurdle_poisson())
fit_mvgam_cached("hurdle_poisson_ar1",
  y ~ 1 + x, ~ AR(p = 1),
  test_data_hp, hurdle_poisson())

cat("\n[15] Hurdle NegBinomial AR(1)\n")
set.seed(655)
n_hnb <- 120
hnb_latent <- sim_ar1(n_hnb, ar_coef, 0.7)
hnb_x <- rnorm(n_hnb)
hnb_mu <- exp(1.0 + 0.5 * hnb_x + hnb_latent)
hnb_hu <- 0.25
test_data_hnb <- data.frame(
  y = ifelse(
    runif(n_hnb) < hnb_hu, 0L,
    vapply(hnb_mu,
           function(m) r_trunc(function() rnbinom(1, mu = m, size = 2)),
           numeric(1))
  ),
  x = hnb_x,
  time = 1:n_hnb,
  series = factor("s1")
)
fit_brms_cached("hurdle_negbinomial_ar1",
  y ~ 1 + x + ar(time = time, p = 1, cov = TRUE),
  test_data_hnb, hurdle_negbinomial())
fit_mvgam_cached("hurdle_negbinomial_ar1",
  y ~ 1 + x, ~ AR(p = 1),
  test_data_hnb, hurdle_negbinomial())

cat("\n[16] Zero-inflated Poisson AR(1)\n")
set.seed(987)
n_zip <- 120
zip_latent <- sim_ar1(n_zip, ar_coef, 0.7)
zip_x <- rnorm(n_zip)
zip_mu <- exp(1.2 + 0.5 * zip_x + zip_latent)
zip_zi <- 0.3
# A zero-inflated count keeps the family's own zeros: the inflation
# adds to them rather than replacing the distribution.
test_data_zip <- data.frame(
  y = ifelse(runif(n_zip) < zip_zi, 0L, rpois(n_zip, zip_mu)),
  x = zip_x,
  time = 1:n_zip,
  series = factor("s1")
)
fit_brms_cached("zero_inflated_poisson_ar1",
  y ~ 1 + x + ar(time = time, p = 1, cov = TRUE),
  test_data_zip, zero_inflated_poisson())
fit_mvgam_cached("zero_inflated_poisson_ar1",
  y ~ 1 + x, ~ AR(p = 1),
  test_data_zip, zero_inflated_poisson())

# ----------------------------------------------------------------------
# HIGH-SIGNAL POISSON AR(1), used by the process_error toggle test
# ----------------------------------------------------------------------

cat("\n[17] High-signal Poisson AR(1)\n")
fit_mvgam_cached("ar1_hs",
  y ~ 1 + x, ~ AR(p = 1),
  test_data_hs, poisson())

cat("\n[18] s(z, by = grp) factor-by-factor smooth\n")
test_data_sby <- test_data
test_data_sby$grp <- factor(
  rep(letters[1:3], length.out = nrow(test_data_sby)),
  levels = letters[1:3]
)
fit_brms_cached("ar1_s_by",
  y ~ 1 + s(z, by = grp) + ar(time = time, p = 1, cov = TRUE),
  test_data_sby, poisson())
fit_mvgam_cached("ar1_s_by",
  y ~ 1 + s(z, by = grp), ~ AR(p = 1),
  test_data_sby, poisson())

cat("\n[19] t2(z, w) tensor-product smooth\n")
test_data_t2 <- test_data
test_data_t2$w <- seq(-1, 1, length.out = nrow(test_data_t2))
fit_brms_cached("ar1_t2",
  y ~ 1 + t2(z, w) + ar(time = time, p = 1, cov = TRUE),
  test_data_t2, poisson())
fit_mvgam_cached("ar1_t2",
  y ~ 1 + t2(z, w), ~ AR(p = 1),
  test_data_t2, poisson())

# The intercept-free twin. `val_brms_ar1_t2_noint` was built outside
# this script and its mvgam counterpart never was, so
# `tests/local/test-marginaleffects-concordance.R` skipped every run.
# Built here, it takes `grp` like every other fixture, so the test no
# longer has to strip a `group` column marginaleffects reserves.
fit_brms_cached("ar1_t2_noint",
  y ~ 0 + t2(z, w, k = c(4, 4)) + ar(time = time, p = 1, cov = TRUE),
  test_data_t2, poisson())
fit_mvgam_cached("ar1_t2_noint",
  y ~ 0 + t2(z, w, k = c(4, 4)), ~ AR(p = 1),
  test_data_t2, poisson())

cat("\n[20] Gaussian AR(1), N=150: PSIS-stable concordance fixture\n")
# Larger N with high signal-to-noise keeps Pareto-k diagnostics in
# the stable region (<0.7), so cross-package PSIS-weighted
# predictions (loo_epred / loo_linpred / loo_predictive_interval)
# can be compared bit-for-bit between mvgam and brms.
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
fit_brms_cached("gauss_ar1_n150",
  y ~ 1 + x + ar(time = time, p = 1, cov = TRUE),
  test_data_gauss, gaussian())
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
# [22] AR(1) cor=TRUE hierarchical (gr=region, subgr=species)
# ----------------------------------------------------------------------
cat("\n[22] AR(1) hierarchical correlated trend (2 groups x 3 subgroups)\n")
set.seed(13)
n_t_h <- 30L
n_groups <- 2L
n_sub <- 3L
n_series_h <- n_groups * n_sub
ar_h <- 0.5
# Independent latent per series for simplicity; the fit learns the
# hierarchical structure regardless of the true generating mechanism.
lat_h <- matrix(0, n_t_h, n_series_h)
for (s in seq_len(n_series_h)) {
  for (t in 2:n_t_h) {
    lat_h[t, s] <- ar_h * lat_h[t - 1L, s] + stats::rnorm(1L, 0, 0.3)
  }
}
test_data_hier <- data.frame(
  y = as.vector(rpois(n_t_h * n_series_h,
                      exp(1 + as.vector(lat_h)))),
  region = factor(rep(rep(paste0("r", 1:n_groups), each = n_sub),
                      times = n_t_h)),
  species = factor(rep(rep(paste0("sp", 1:n_sub), times = n_groups),
                       times = n_t_h)),
  time = rep(seq_len(n_t_h), each = n_series_h)
)
# Built with interaction()'s default "." separator, which does not
# match the "_" form mvgam derives from gr and subgr. The mismatch is
# deliberate: it exercises the path where a supplied series column is
# superseded.
test_data_hier$series <- interaction(test_data_hier$region,
                                      test_data_hier$species,
                                      drop = TRUE)
fit_mvgam_cached("hier_ar_cor",
  y ~ 1,
  ~ AR(gr = region, subgr = species, cor = TRUE),
  test_data_hier, poisson())


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
fit_trend_map_cached("trend_map_fx", Z_true)

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
# Persist the simulation truth alongside the fit so the test can
# check loadings recovery without reproducing the simulation.
loadings_prior_truth <- list(
  Z_true = Z_lp,
  cluster = cluster_lp,
  trait = trait_lp
)
saveRDS(
  loadings_prior_truth,
  file.path(FIXTURE_DIR, "val_mvgam_loadings_prior_truth.rds")
)
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
# an nl fit must match a brms-direct fit on the same data + priors.
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
fit_brms_cached("nl_growth",
  bf(y ~ b1 * exp(b2 * x), b1 + b2 ~ 1, nl = TRUE),
  nl_growth_data, gaussian(), prior = nl_growth_pri)
fit_mvgam_cached("nl_growth",
  bf(y ~ b1 * exp(b2 * x), b1 + b2 ~ 1, nl = TRUE),
  NULL, nl_growth_data, gaussian(), prior = nl_growth_pri)

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
fit_brms_cached("nl_trait", nl_trait_form,
  nl_trait_data, gaussian(), prior = nl_trait_pri)
fit_mvgam_cached("nl_trait", nl_trait_form, NULL,
  nl_trait_data, gaussian(), prior = nl_trait_pri)

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
# [26] JSDM with one observation family per species.
# Three species share two latent factors through known loadings, and
# each is observed on its own scale: counts, presence-absence and a
# continuous measure. The responses are the species axis, so this is
# the only shape that can carry mixed data types in one joint model,
# and `Z` is recoverable because the loadings are known.
# ----------------------------------------------------------------------
cat("\n[26] JSDM, one family per species\n")
set.seed(2026L)
n_site_mv <- 60L
n_lv_mv <- 2L
Z_mv <- matrix(
  c(1.0, 0.2,
    0.3, 0.9,
    -0.8, 0.5),
  nrow = 3L, ncol = n_lv_mv, byrow = TRUE
)
lv_mv <- matrix(rnorm(n_site_mv * n_lv_mv), n_site_mv, n_lv_mv)
eta_mv <- lv_mv %*% t(Z_mv)
mv_fam_data <- data.frame(
  site = seq_len(n_site_mv),
  env = rnorm(n_site_mv),
  count = rpois(n_site_mv, exp(1.0 + 0.4 * eta_mv[, 1L])),
  seen = rbinom(n_site_mv, 1L, plogis(0.2 + eta_mv[, 2L])),
  mass = rnorm(n_site_mv, 2.0 + eta_mv[, 3L], 0.4)
)
attr(mv_fam_data, "Z_true") <- Z_mv
fit_jsdgam_cached(
  "mv_families",
  brms::bf(count ~ env, family = poisson()) +
    brms::bf(seen ~ env, family = bernoulli()) +
    brms::bf(mass ~ env, family = gaussian()),
  ~ -1, mv_fam_data, n_lv = n_lv_mv,
  unit = site, species = species
)

# ----------------------------------------------------------------------
# [27] Multivariate frame carrying no series column.
# Every cached multivariate fit names its own series, so none of them
# reaches the path that derives one. That path used to cut the rows
# into a block per response, which is a stacked frame's shape and not
# this one, and the resulting latent trend gave the first half of the
# timeline to one response and the rest to the other. It raised
# nothing. This fixture exists to be driven end to end on that shape.
# ----------------------------------------------------------------------
cat("\n[27] Multivariate, no series column\n")
set.seed(3131L)
n_mv_nc <- 60L
mv_nc_latent <- sim_ar1(n_mv_nc, ar_coef, 0.6)
mv_nc_x <- rnorm(n_mv_nc)
mv_nc_data <- data.frame(
  time = seq_len(n_mv_nc),
  x = mv_nc_x,
  cnt = rpois(n_mv_nc, exp(0.8 + 0.5 * mv_nc_x + mv_nc_latent)),
  pa = rbinom(n_mv_nc, 1L,
              plogis(-0.2 + 0.7 * mv_nc_x + mv_nc_latent))
)
attr(mv_nc_data, "latent_true") <- mv_nc_latent
fit_mvgam_cached(
  "mv_nocol",
  brms::bf(cnt ~ x, family = poisson()) +
    brms::bf(pa ~ x, family = bernoulli()),
  ~ AR(p = 1),
  mv_nc_data, NULL
)

# ----------------------------------------------------------------------
# [28] Three responses over a row count that does not divide by three.
# The rows of a wide frame count time points, so their number bears no
# relation to the number of responses. Fifty times and three responses
# is the shape a user hits following the multivariate article, and it
# is also the smallest case where the response axis carries more
# series than the model has factors, so the loadings have work to do.
# Each response is on its own scale, which is the point of giving them
# separate submodels.
# ----------------------------------------------------------------------
cat("\n[28] Three responses, rows not divisible by responses\n")
set.seed(2828L)
n_odd <- 50L
lv_odd <- cbind(sim_ar1(n_odd, 0.75, 0.5), sim_ar1(n_odd, 0.35, 0.5))
Z_odd <- matrix(c(1.0, 0.0,
                  0.4, 0.8,
                  -0.6, 0.5), nrow = 3L, ncol = 2L, byrow = TRUE)
eta_odd <- lv_odd %*% t(Z_odd)
odd_x <- rnorm(n_odd)
mv_odd_data <- data.frame(
  time = seq_len(n_odd),
  x = odd_x,
  cnt = rpois(n_odd, exp(1.0 + 0.4 * odd_x + eta_odd[, 1L])),
  pa = rbinom(n_odd, 1L, plogis(-0.3 + 0.6 * odd_x + eta_odd[, 2L])),
  mass = rgamma(n_odd, shape = 4,
                rate = 4 / exp(0.5 + 0.3 * odd_x + eta_odd[, 3L]))
)
attr(mv_odd_data, "Z_true") <- Z_odd
attr(mv_odd_data, "lv_true") <- lv_odd
fit_mvgam_cached(
  "mv_three_odd",
  brms::bf(cnt ~ x, family = poisson()) +
    brms::bf(pa ~ x, family = bernoulli()) +
    brms::bf(mass ~ x, family = Gamma(link = "log")),
  ~ ZMVN(n_lv = 2),
  mv_odd_data, NULL
)

# ----------------------------------------------------------------------
# [29] Two responses measured on each of three sites.
# Here both axes are real at once: the frame names its series and each
# row still carries every response. The series column is the user's
# statement that these responses share a site's latent state, and it
# has to keep winning over the response axis, or a three-site model
# silently becomes a two-series one. `cor = TRUE` means the recovered
# correlation between sites is a claim the fixture can be asked for.
# ----------------------------------------------------------------------
cat("\n[29] Two responses on three sites\n")
set.seed(2929L)
n_t_ms <- 40L
sites_ms <- c("north", "central", "south")
lv_ms <- vapply(seq_along(sites_ms),
                function(i) sim_ar1(n_t_ms, 0.6, 0.5),
                numeric(n_t_ms))
ms_rows <- expand.grid(time = seq_len(n_t_ms), series = sites_ms,
                       stringsAsFactors = FALSE)
ms_state <- lv_ms[cbind(ms_rows$time, match(ms_rows$series, sites_ms))]
ms_x <- rnorm(nrow(ms_rows))
mv_ms_data <- data.frame(
  time = ms_rows$time,
  series = factor(ms_rows$series, levels = sites_ms),
  x = ms_x,
  cnt = rpois(nrow(ms_rows), exp(0.9 + 0.5 * ms_x + ms_state)),
  pa = rbinom(nrow(ms_rows), 1L, plogis(-0.2 + 0.4 * ms_x + ms_state))
)
attr(mv_ms_data, "lv_true") <- lv_ms
fit_mvgam_cached(
  "mv_multiseries",
  brms::bf(cnt ~ x, family = poisson()) +
    brms::bf(pa ~ x, family = bernoulli()),
  ~ AR(p = 1, cor = TRUE),
  mv_ms_data, NULL
)

# ----------------------------------------------------------------------
# [30] Two responses observed on different occasions.
# brms drops a row with an NA in any response; mvgam keeps each
# response on its own valid rows so the shared state is informed
# wherever either was measured. On a frame whose series is derived
# rather than named, that per-response subsetting is what carries the
# series along with it, so the two features have to be exercised
# together. A random walk leaves the state unpinned by a stationary
# variance, which is the harder case for a gappy series.
# ----------------------------------------------------------------------
cat("\n[30] Two responses with per-response gaps\n")
set.seed(3030L)
n_gap <- 60L
gap_latent <- sim_ar1(n_gap, 0.9, 0.35)
gap_x <- rnorm(n_gap)
mv_gap_data <- data.frame(
  time = seq_len(n_gap),
  x = gap_x,
  cnt = rpois(n_gap, exp(1.1 + 0.5 * gap_x + gap_latent)),
  gauss = rnorm(n_gap, 0.4 + 0.7 * gap_x + gap_latent, 0.5)
)
# Disjoint gaps, so neither response can be reconstructed from the
# other's occasions and some times inform the state through one arm
# only.
mv_gap_data$cnt[c(5L, 6L, 17L, 33L, 44L)] <- NA_integer_
mv_gap_data$gauss[c(9L, 21L, 22L, 38L, 51L, 52L)] <- NA_real_
attr(mv_gap_data, "latent_true") <- gap_latent
fit_mvgam_cached(
  "mv_na_gaps",
  brms::bf(cnt ~ x, family = poisson()) +
    brms::bf(gauss ~ x, family = gaussian()),
  ~ RW(),
  mv_gap_data, NULL
)

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
  recovery("jsdgam_mv_nmix.R", "val_mvgam_jsdgam_mv_nmix.rds",
           c("dplyr", "posterior", "testthat")),
  recovery("jsdgam_mv_occ.R", "val_mvgam_jsdgam_mv_occ.rds",
           c("dplyr", "posterior", "testthat")),
  recovery("jsdgam_multi_season.R", "val_mvgam_jsdgam_multi_season.rds",
           c("dplyr", "posterior", "testthat")),
  recovery("diri_phi_subformula_fit.R",
           "val_mvgam_diri_phi_subformula.rds", JSDM_PKGS),
  recovery("zmvn_irregular_time.R", "val_mvgam_zmvn_irregular.rds",
           "broom"),
  recovery("kfold_grouped_cv.R",
           c("val_mvgam_kfold_demo_gauss.rds",
             "val_mvgam_kfold_demo_occ.rds"),
           "ggplot2"),
  recovery("jsdgam_ordinate_traits.R",
           c("val_mvgam_ordinate_traits.rds",
             "val_mvgam_ordinate_traits_data.rds"),
           "ggplot2"),
  recovery("heaps_birds_replica.R",
           c("val_mvgam_heaps_birds.rds",
             "val_mvgam_heaps_birds_wide.rds"),
           "ape"),
  recovery("heaps_birds_replica_phylo_dominant.R",
           "val_mvgam_heaps_birds_phylo_dominant.rds", "ape"),
  recovery("jsdgam_heaps_nonlinear_env.R",
           c("val_mvgam_heaps_nonlinear_env_occ.rds",
             "val_mvgam_heaps_nonlinear_env_nmix.rds",
             "val_mvgam_heaps_nonlinear_env_multi.rds"),
           c("ape", "ggplot2", "posterior"))
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
