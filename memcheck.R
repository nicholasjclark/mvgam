# Memory-check script run under valgrind by
# `.github/workflows/memcheck.yaml`. Exercises both C++ trend
# kernels directly with a battery of input shapes to surface any
# leaks, invalid reads, or use-after-free issues.
#
# Run locally with:
#   R -d "valgrind --tool=memcheck --leak-check=full \
#         --track-origins=yes --show-leak-kinds=all" \
#         -f memcheck.R

suppressMessages(devtools::load_all())
library(testthat)

# Helper: call the general ARMA kernel with synthetic inputs sized to
# the new contract (innovations + linpreds must have h + max_lag rows
# where max_lag = max(max(ar_lags), max(ma_lags))).
sim_arma <- function(ar_lags, ma_lags, dim, h,
                      A_scale = 0.3, B_scale = 0.2,
                      include_linpreds = FALSE) {
  m_a <- length(ar_lags)
  m_b <- length(ma_lags)
  max_ar <- if (m_a > 0) max(ar_lags) else 0L
  max_ma <- if (m_b > 0) max(ma_lags) else 0L
  max_lag <- max(max_ar, max_ma)
  total <- h + max_lag

  drift <- rep(0, dim)
  A <- array(0, dim = c(dim, dim, max(1L, m_a)))
  for (k in seq_len(m_a)) {
    A[, , k] <- diag(A_scale / m_a, nrow = dim)
  }
  B <- array(0, dim = c(dim, dim, max(1L, m_b)))
  for (j in seq_len(m_b)) {
    B[, , j] <- diag(B_scale / max(m_b, 1L), nrow = dim)
  }
  innovations <- matrix(rnorm(total * dim), total, dim)
  linpreds <- if (include_linpreds) {
    matrix(0.1 * seq_len(total * dim), total, dim)
  } else {
    matrix(0, total, dim)
  }
  last_trends <- if (max_ar > 0) {
    matrix(rnorm(max_ar * dim), max_ar, dim)
  } else {
    matrix(0, 0, dim)
  }

  trend_arma_recursC(
    ar_lags = as.integer(ar_lags),
    ma_lags = as.integer(ma_lags),
    drift = drift,
    A = A,
    B = B,
    innovations = innovations,
    linpreds = linpreds,
    last_trends = last_trends,
    h = h
  )
}

cat("[1] AR(1) univariate\n")
out <- sim_arma(ar_lags = 1L, ma_lags = integer(), dim = 1L, h = 50L)
stopifnot(identical(dim(out), c(50L, 1L)), all(is.finite(out)))

cat("[2] AR(p = c(1, 3, 12)) sparse-lag univariate\n")
out <- sim_arma(ar_lags = c(1L, 3L, 12L), ma_lags = integer(),
                 dim = 1L, h = 60L)
stopifnot(identical(dim(out), c(60L, 1L)), all(is.finite(out)))

cat("[3] VAR(1) multivariate, dim = 5\n")
out <- sim_arma(ar_lags = 1L, ma_lags = integer(), dim = 5L, h = 30L)
stopifnot(identical(dim(out), c(30L, 5L)), all(is.finite(out)))

cat("[4] VARMA(2, 1) multivariate, dim = 3\n")
out <- sim_arma(ar_lags = c(1L, 2L), ma_lags = 1L, dim = 3L,
                 h = 40L)
stopifnot(identical(dim(out), c(40L, 3L)), all(is.finite(out)))

cat("[5] AR with non-zero linpreds (centred convention path)\n")
out <- sim_arma(ar_lags = c(1L, 2L), ma_lags = integer(),
                 dim = 4L, h = 25L, include_linpreds = TRUE)
stopifnot(identical(dim(out), c(25L, 4L)), all(is.finite(out)))

cat("[6] Pure MA(q = c(1, 12)) — no AR component\n")
out <- sim_arma(ar_lags = integer(), ma_lags = c(1L, 12L),
                 dim = 1L, h = 50L)
stopifnot(identical(dim(out), c(50L, 1L)), all(is.finite(out)))

cat("[7] High-dim VAR(1) dim = 20\n")
out <- sim_arma(ar_lags = 1L, ma_lags = integer(), dim = 20L, h = 30L)
stopifnot(identical(dim(out), c(30L, 20L)), all(is.finite(out)))

cat("[8] High-order sparse AR(p = c(1, 7, 12, 24)) dim = 2\n")
out <- sim_arma(ar_lags = c(1L, 7L, 12L, 24L), ma_lags = integer(),
                 dim = 2L, h = 100L)
stopifnot(identical(dim(out), c(100L, 2L)), all(is.finite(out)))

cat("[9] MA dominates AR: max_ma (15) > max_ar (3)\n")
out <- sim_arma(ar_lags = c(1L, 3L), ma_lags = c(1L, 15L),
                 dim = 2L, h = 50L)
stopifnot(identical(dim(out), c(50L, 2L)), all(is.finite(out)))

cat("[10] Repeated calls to surface accumulating leaks\n")
for (i in 1:50) {
  invisible(sim_arma(ar_lags = c(1L, 5L), ma_lags = integer(),
                      dim = 3L, h = 100L))
}

cat("[11] Input-shape guard: wrong innovations row count errors\n")
err <- tryCatch(
  trend_arma_recursC(
    ar_lags = 1L, ma_lags = integer(),
    drift = 0, A = array(0.3, dim = c(1, 1, 1)),
    B = array(0, dim = c(1, 1, 1)),
    innovations = matrix(rnorm(5), 5, 1),
    linpreds = matrix(0, 5, 1),
    last_trends = matrix(0, 1, 1),
    h = 10L
  ),
  error = function(e) conditionMessage(e)
)
stopifnot(grepl("innovations", err))


# Piecewise trend kernel
pw <- function(n_series, h, n_change, growth_type) {
  pw_trendC(
    t = seq_len(h) / h,
    k = rnorm(n_series, 0, 0.5),
    m = rnorm(n_series),
    delta = matrix(rnorm(n_change * n_series, 0, 0.1), n_change, n_series),
    t_change = sort(runif(n_change)),
    cap = matrix(10, h, n_series),
    growth_type = growth_type
  )
}

cat("[12] Piecewise linear trend, 3 series, 8 changepoints\n")
out <- pw(3L, 60L, 8L, "linear")
stopifnot(identical(dim(out), c(60L, 3L)), all(is.finite(out)))

cat("[13] Piecewise logistic trend, 3 series, 8 changepoints\n")
out <- pw(3L, 60L, 8L, "logistic")
stopifnot(identical(dim(out), c(60L, 3L)), all(is.finite(out)))

cat("[14] Piecewise trend without changepoints\n")
out <- pw(2L, 20L, 0L, "linear")
stopifnot(identical(dim(out), c(20L, 2L)), all(is.finite(out)))

cat("[15] Input-shape guard: wrong delta shape errors\n")
err <- tryCatch(
  pw_trendC(t = 1:5, k = 0.1, m = 0, delta = matrix(0, 2, 2),
            t_change = c(0.3, 0.6), cap = matrix(10, 5, 1),
            growth_type = "linear"),
  error = function(e) conditionMessage(e)
)
stopifnot(grepl("delta", err))

cat("[16] Repeated piecewise calls\n")
for (i in 1:50) {
  invisible(pw(5L, 100L, 10L, if (i %% 2L) "linear" else "logistic"))
}

cat("\nAll memcheck calls completed without R-level error.\n")
