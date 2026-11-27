# Helpers shared by test-irf-fevd.R and test-extra-irf-fevd.R.
## Coverage for irf.mvgam / fevd.mvgam / stability.mvgam. Three layers
## of test:
##   1. Trend-type gate (`assert_var_trend`) on a non-VAR mock.
##   2. Posterior extractor (`extract_var_posterior`) on a mock
##      `posterior::draws_matrix` with the right Stan column names.
##   3. Math helpers (`var_phi`, `var_psi`, `var_fecov`, `gen_irf`,
##      `gen_fevd`) on hand-built A / Sigma where the answer is
##      analytically known.
##
## Fitted coverage is in `tests/local/test-trend-var.R` and
## `tests/local/test-factor-var.R`.

# ---- Build a minimal mvgam stub backed by a draws_matrix ----------

# `extract_var_posterior` only needs (i) object$fit to be something
# `posterior::as_draws_matrix` accepts and (ii) the trend type and the
# Stan data to resolve K. A draws_matrix dispatches through the
# posterior generic as the identity, so we can hand one in directly
# without going through Stan.
build_var_mock <- function(K = 3L, ndraws = 50L,
                           trend_type = "VAR", seed = 1L) {
  set.seed(seed)
  cols <- character(0L)
  for (i in seq_len(K)) for (j in seq_len(K)) {
    cols <- c(cols, sprintf("Phi_trend[1,%d,%d]", i, j))
  }
  for (i in seq_len(K)) for (j in seq_len(K)) {
    cols <- c(cols, sprintf("Sigma_trend[%d,%d]", i, j))
  }
  vals <- matrix(rnorm(ndraws * length(cols), sd = 0.1),
                 nrow = ndraws, ncol = length(cols),
                 dimnames = list(NULL, cols))
  # Force the diagonal of Sigma_trend to be positive so the math
  # helpers (chol, eigen, kronecker inverse) stay numerically sane.
  for (i in seq_len(K)) {
    sig_d <- sprintf("Sigma_trend[%d,%d]", i, i)
    vals[, sig_d] <- abs(vals[, sig_d]) + 0.5
  }
  draws <- posterior::as_draws_matrix(vals)

  structure(
    list(
      fit = draws,
      trend_metadata = list(trend_type = trend_type),
      standata = list(N_lv_trend = K, N_series_trend = K)
    ),
    class = "mvgam"
  )
}

# The math kernels are reached only through `var_draw_surfaces()`,
# which hands them the process labels beside the matrices. A stub
# omitting those describes an object no caller builds.
var_kernel_input <- function(K, A, Sigma = diag(K),
                             D = array(0, c(K, K, 0L)),
                             labels = paste0("s", seq_len(K))) {
  list(K = K, A = array(A, c(K, K, length(A) / K^2)), D = D,
       Sigma = Sigma, labels = labels)
}


# ---- posterior_transition_matrix ------------------------------------
# Driven on the same draws_matrix mock as the other VAR surfaces, so
# the argument surface and the summary shape are covered without
# sampling. End-to-end coverage is in tests/local/postfit_sweep.R.

# `finalise_transition_matrix()` reports an effective sample size per
# cell, and `posterior::ess_basic()` warns when it has to cap one.
# Independent draws invite that cap, which no real chain would, so
# the mock is passed through an AR(1) filter first to give it the
# serial correlation a posterior actually has.
correlate_draws <- function(mock, rho = 0.5) {
  m <- as.matrix(mock$fit)
  for (j in seq_len(ncol(m))) {
    m[, j] <- as.numeric(
      stats::filter(m[, j], rho, method = "recursive")
    )
  }
  mock$fit <- posterior::as_draws_matrix(m)
  mock
}
