# R reference + numerical validation for the Haines (2016)
# closed-form Royle-Nichols likelihood. Used as the design input
# for task #312 (port to Stan as a fast path for nmix("royle_nichols")
# when per-individual detection rate r is constant within each
# closure unit).
#
# Math:
#   L_g = e^{-lambda} * sum_{k=0..D} C(D, k) * (-1)^k *
#         exp(lambda * q^{k + V_0})
# where q = 1 - r, D = sum(y), V_0 = V - D. The sum has D + 1
# terms, vs the k-loop enumeration's K_max - max(y) + 1 terms.
# For typical occupancy data with D <= 5 this is 5-50x fewer
# iterations.
#
# Caveats (verified empirically):
# 1. Closed form only applies when r is CONSTANT within unit
#    (no per-visit detection covariate that varies inside the
#    unit).
# 2. Numerical stability cliff at extreme tails: when V_0 is
#    large (>= ~25 non-detection visits) AND lambda is
#    moderate-to-large, the q^{k + V_0} factor underflows to
#    near-zero for all k, so all D + 1 terms become near-equal
#    in magnitude. The alternating-sign sum then suffers
#    catastrophic cancellation and returns -Inf. The
#    brute-force k-loop sums positive terms only and stays
#    numerically stable in this regime.
# 3. Safety strategy: emit BOTH paths in Stan and dispatch at
#    runtime. Use Haines when:
#      max(p[idx]) - min(p[idx]) < eps   (constant r)
#      AND log_pos - log_neg > tolerance   (cancellation OK)
#    Otherwise fall back to the existing k-loop enumeration.
#
# Run this script standalone to re-verify the math against
# K_max = 200 brute force:
#   Rscript tests/local/haines_rn_validation.R

brute_force_rn <- function(y, lambda, r, K_max = 1000L) {
  q <- 1 - r
  D <- sum(y)
  V <- length(y)
  V_0 <- V - D
  log_terms <- vapply(0:K_max, function(N) {
    lp <- stats::dpois(N, lambda, log = TRUE)
    if (V_0 > 0L) lp <- lp + V_0 * N * log(q)
    if (D > 0L) {
      if (N == 0L) return(-Inf)
      lp <- lp + D * log1p(-q^N)
    }
    lp
  }, numeric(1L))
  matrixStats::logSumExp(log_terms)
}

haines_rn <- function(y, lambda, r) {
  q <- 1 - r
  log_q <- log(q)
  D <- sum(y)
  V <- length(y)
  V_0 <- V - D
  ks <- 0:D
  log_terms <- vapply(ks, function(k) {
    log_C <- lchoose(D, k)
    q_power <- exp((k + V_0) * log_q)
    log_C - lambda * (1 - q_power)
  }, numeric(1L))
  even_idx <- which(ks %% 2L == 0L)
  odd_idx  <- which(ks %% 2L == 1L)
  log_pos <- matrixStats::logSumExp(log_terms[even_idx])
  if (length(odd_idx) == 0L) return(log_pos)
  log_neg <- matrixStats::logSumExp(log_terms[odd_idx])
  # In-distribution: log_pos > log_neg; log_diff_exp is well-
  # defined. Near the stability cliff log_pos approaches log_neg
  # and the diff underflows to -Inf.
  log_pos + log1p(-exp(log_neg - log_pos))
}

# In-distribution cases (Haines + brute agree to machine precision)
in_dist_cases <- list(
  list(name = "all-zero, low lambda",       y = c(0, 0, 0),       lam = 1,    r = 0.3),
  list(name = "all-zero, high lambda",      y = c(0, 0, 0, 0, 0), lam = 20,   r = 0.5),
  list(name = "single detection",           y = c(1, 0, 0),       lam = 3,    r = 0.4),
  list(name = "all-detection",              y = c(1, 1, 1),       lam = 5,    r = 0.6),
  list(name = "mixed, moderate",            y = c(1, 0, 1, 0, 1), lam = 8,    r = 0.3),
  list(name = "many visits, few detect",    y = c(rep(0, 8), 1, 1), lam = 2,  r = 0.2),
  list(name = "very high r, many visits",   y = c(1, 1, 1, 1, 0), lam = 4,    r = 0.9),
  list(name = "very low r, all-zero",       y = c(0, 0, 0, 0),    lam = 50,   r = 0.05),
  list(name = "D = 10 stress (small lam)",  y = rep(1, 10),       lam = 1,    r = 0.5),
  list(name = "D = 10 stress (large lam)",  y = rep(1, 10),       lam = 30,   r = 0.5)
)

# Tail cases exposing the stability cliff: V_0 large + lambda
# moderate-to-large pushes Haines to -Inf via alternating-sign
# cancellation, while the k-loop stays finite. These are
# MCMC-improbable points where loo / log_lik may still evaluate.
tail_cases <- list(
  list(name = "V=50 D=25 lam=20 cliff",     y = c(rep(1, 25), rep(0, 25)), lam = 20,  r = 0.3),
  list(name = "V=40 D=20 lam=15 cliff",     y = c(rep(1, 20), rep(0, 20)), lam = 15,  r = 0.35)
)

cat("\nIn-distribution: Haines == brute-force to machine precision\n")
cat("============================================================\n")
all_ok <- TRUE
for (cs in in_dist_cases) {
  hp <- haines_rn(cs$y, cs$lam, cs$r)
  bp <- brute_force_rn(cs$y, cs$lam, cs$r, K_max = 200L)
  diff <- abs(hp - bp)
  ok <- diff < 1e-8
  cat(sprintf("%-32s haines=%12.6f brute=%12.6f diff=%9.2e %s\n",
              cs$name, hp, bp, diff, if (ok) "PASS" else "FAIL"))
  all_ok <- all_ok && ok
}
cat(sprintf("\nIn-distribution all-pass: %s\n", all_ok))

cat("\nTail cases: stability cliff (Haines breaks, brute stable)\n")
cat("============================================================\n")
for (cs in tail_cases) {
  hp <- haines_rn(cs$y, cs$lam, cs$r)
  bp <- brute_force_rn(cs$y, cs$lam, cs$r, K_max = 500L)
  cliff <- is.infinite(hp) && is.finite(bp)
  cat(sprintf("%-32s haines=%12.6f brute=%12.6f cliff=%s\n",
              cs$name, hp, bp, cliff))
}
cat("\nConclusion: Haines fast path requires a runtime cancellation\n")
cat("guard (e.g. log_pos - log_neg < 20 -> k-loop fallback).\n")
