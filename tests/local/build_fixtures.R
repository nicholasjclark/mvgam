# Build cached brms and mvgam fixtures used by
# tests/local/test-predictions-brms-concordance.R.
#
# Run from the package root:
#   Rscript tests/local/build_fixtures.R
#
# Output: tests/local/fixtures/val_brms_<name>.rds and
#         tests/local/fixtures/val_mvgam_<name>.rds for each fixture
#         below. The directory is gitignored; rebuild after a clone.
#
# Expected runtime: 15-25 minutes for the full set on a workstation.
# Re-runs after partial completion are incremental — only missing
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

cat("=== Building tests/local/fixtures/ ===\n\n")

# ----------------------------------------------------------------------
# SHARED TEST DATA
# ----------------------------------------------------------------------

set.seed(42)
n_time <- 30
ar_coef <- 0.7
sigma <- 0.5
latent <- numeric(n_time)
latent[1] <- rnorm(1, 0, sigma / sqrt(1 - ar_coef^2))
for (t in 2:n_time) {
  latent[t] <- ar_coef * latent[t - 1] + rnorm(1, 0, sigma)
}
z <- seq(-2, 2, length.out = n_time)
z_effect <- 0.5 * sin(z * pi)
test_data <- data.frame(
  y = rpois(n_time, exp(2 + latent + z_effect)),
  x = rnorm(n_time),
  z = z,
  time = 1:n_time,
  series = factor("s1"),
  group = factor(rep(letters[1:6], each = 5))
)

# High-signal Poisson AR(1) data (used by process_error toggle test)
set.seed(456)
hs_n <- 60
hs_x <- seq(-2, 2, length.out = hs_n)
hs_latent <- numeric(hs_n)
hs_latent[1] <- rnorm(1, 0, sigma / sqrt(1 - ar_coef^2))
for (t in 2:hs_n) {
  hs_latent[t] <- ar_coef * hs_latent[t - 1] + rnorm(1, 0, sigma)
}
test_data_hs <- data.frame(
  y = rpois(hs_n, exp(0.5 + 1.5 * hs_x + hs_latent)),
  x = hs_x,
  time = 1:hs_n,
  series = factor("s1")
)

# ----------------------------------------------------------------------
# MAIN OBS-FORMULA GRID — Poisson AR(1)
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
  y ~ 1 + x + (1 | group) + ar(time = time, p = 1, cov = TRUE),
  test_data, poisson())
fit_mvgam_cached("ar1_re",
  y ~ 1 + x + (1 | group), ~ AR(p = 1),
  test_data, poisson())

cat("\n[4] AR(1) + fixed + random + smooth\n")
fit_brms_cached("ar1_re_smooth",
  y ~ 1 + x + (1 | group) + s(z) + ar(time = time, p = 1, cov = TRUE),
  test_data, poisson())
fit_mvgam_cached("ar1_re_smooth",
  y ~ 1 + x + (1 | group) + s(z), ~ AR(p = 1),
  test_data, poisson())

cat("\n[5] AR(1) + correlated REs\n")
fit_brms_cached("ar1_cor_re",
  y ~ 1 + x + (x | group) + ar(time = time, p = 1, cov = TRUE),
  test_data, poisson())
fit_mvgam_cached("ar1_cor_re",
  y ~ 1 + x + (x | group), ~ AR(p = 1),
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
  y ~ 1, ~ x + (1 | group) + s(z) + AR(p = 1),
  test_data, poisson())

# ----------------------------------------------------------------------
# MULTIVARIATE mvbind + shared AR(1)
# ----------------------------------------------------------------------

cat("\n[10] Multivariate mvbind (2 responses)\n")
set.seed(789)
test_data_mv <- data.frame(
  y1 = rnorm(n_time, mean = 1 + latent),
  y2 = rnorm(n_time, mean = 2 + latent),
  x = rnorm(n_time),
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
set.seed(456)
n_beta <- 30
test_data_beta <- data.frame(
  y = pmax(pmin(rbeta(n_beta, 2, 5), 0.999), 0.001),
  x = rnorm(n_beta),
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
n_binom <- 30
trials_vec <- rep(20, n_binom)
test_data_binom <- data.frame(
  y = rbinom(n_binom, size = trials_vec, prob = 0.4),
  trials = trials_vec,
  x = rnorm(n_binom),
  time = 1:n_binom,
  series = factor("s1")
)
fit_brms_cached("binom_ar1",
  y | trials(trials) ~ 1 + x + ar(time = time, p = 1, cov = TRUE),
  test_data_binom, binomial())
fit_mvgam_cached("binom_ar1",
  y | trials(trials) ~ 1 + x, ~ AR(p = 1),
  test_data_binom, binomial())

cat("\n[13] Ordinal (Cumulative) — fixed effects only (no AR)\n")
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
n_hp <- 30
mu_true_hp <- exp(1.5 + 0.3 * rnorm(n_hp))
test_data_hp <- data.frame(
  y = ifelse(runif(n_hp) < 0.3, 0, rpois(n_hp, mu_true_hp)),
  x = rnorm(n_hp),
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
n_hnb <- 30
mu_true_hnb <- exp(1.2 + 0.4 * rnorm(n_hnb))
test_data_hnb <- data.frame(
  y = ifelse(runif(n_hnb) < 0.35, 0,
              rnbinom(n_hnb, mu = mu_true_hnb, size = 2)),
  x = rnorm(n_hnb),
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
n_zip <- 30
mu_true_zip <- exp(1.5 + 0.3 * rnorm(n_zip))
test_data_zip <- data.frame(
  y = ifelse(runif(n_zip) < 0.4, 0, rpois(n_zip, mu_true_zip)),
  x = rnorm(n_zip),
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
# HIGH-SIGNAL POISSON AR(1) — used by process_error toggle test
# ----------------------------------------------------------------------

cat("\n[17] High-signal Poisson AR(1)\n")
fit_mvgam_cached("ar1_hs",
  y ~ 1 + x, ~ AR(p = 1),
  test_data_hs, poisson())

cat("\n=== All fixtures present in", FIXTURE_DIR, "===\n")
cat("Files: ", length(list.files(FIXTURE_DIR, pattern = "\\.rds$")), "\n")
