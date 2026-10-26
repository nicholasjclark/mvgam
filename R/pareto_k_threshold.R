#' Adaptive Pareto k threshold for PSIS
#'
#' The rule of Vehtari, Simpson, Gelman, Yao and Gabry (2024, JMLR
#' 25:72), `min(1 - 1 / log10(S), 0.7)`. It is stricter for few draws
#' and stops at 0.7. loo computes it in the unexported
#' `ps_khat_threshold()`.
#'
#' @param S Integer number of posterior draws, at least 2. The formula
#'   divides by zero at `S = 1`.
#' @return Numeric scalar threshold
#' @noRd
mvgam_ps_khat_threshold <- function(S) {
  checkmate::assert_int(S, lower = 2L)
  min(1 - 1 / log10(S), 0.7)
}


#' The Pareto k threshold that triggers a refit
#'
#' Used by `lfo_cv()` and `kfold()`.
#'
#' @param pareto_k_threshold Numeric threshold, or `NULL` for the
#'   adaptive rule of `mvgam_ps_khat_threshold()`
#' @param n_draws Integer number of posterior draws
#' @return Numeric scalar threshold
#' @noRd
resolve_pareto_k_threshold <- function(pareto_k_threshold, n_draws) {
  checkmate::assert_number(pareto_k_threshold, null.ok = TRUE)
  if (is.null(pareto_k_threshold)) {
    mvgam_ps_khat_threshold(n_draws)
  } else {
    pareto_k_threshold
  }
}


#' The Pareto k threshold of a cross-validation result, for printing
#'
#' @param x An `mvgam_lfo` or `mvgam_kfold` object
#' @param digits Integer number of decimal places
#' @return Character scalar, marked `(adaptive)` for the default rule
#' @noRd
pareto_k_threshold_label <- function(x, digits) {
  checkmate::assert_number(x$pareto_k_threshold)
  checkmate::assert_int(digits, lower = 0L)
  paste0(
    formatC(x$pareto_k_threshold, digits = digits, format = "f"),
    if (isTRUE(x$pareto_k_threshold_adaptive)) " (adaptive)"
  )
}


#' Standard error of a sum of pointwise ELPDs
#'
#' `sqrt(n) * sd(x)` over the `n` observed values, as in
#' `loo::loo_compare()`.
#'
#' @param x Numeric vector of pointwise ELPDs or their differences
#' @return Numeric scalar
#' @noRd
se_of_sum <- function(x) {
  checkmate::assert_numeric(x)
  sqrt(sum(!is.na(x))) * stats::sd(x, na.rm = TRUE)
}


#' Diagnostic columns of a paired ELPD comparison table
#'
#' Sivula, Magnusson, Matamoros and Vehtari (2025, Bayesian Analysis,
#' \doi{10.1214/25-BA1569}) keep the normal approximation to the ELPD
#' difference, with SE `sqrt(N) * sd(pointwise_diff)`. They name three
#' conditions under which that SE understates the uncertainty.
#'
#' @param elpd_diff Numeric vector of ELPD differences from the best
#'   model
#' @param se_diff Numeric vector of their standard errors, zero for the
#'   best model
#' @param n_pointwise Integer number of pointwise contributions, one
#'   per model or shared, or `NULL`
#' @param pareto_k_list List of Pareto k vectors, one per model, or
#'   `NULL`
#' @return Data frame with columns `p_worse` (the normal probability
#'   that the difference is below zero, `NA` for the best model),
#'   `diag_diff` (`"|elpd_diff| < 4"` for a small difference) and
#'   `diag_elpd` (`"N < 100"` for few observations, `"k_psis > 0.7"` for
#'   an unreliable PSIS approximation)
#' @noRd
mvgam_loo_compare_diagnostics <- function(elpd_diff, se_diff,
                                          n_pointwise = NULL,
                                          pareto_k_list = NULL) {
  checkmate::assert_numeric(elpd_diff, any.missing = FALSE)
  checkmate::assert_numeric(se_diff, len = length(elpd_diff),
                              any.missing = FALSE, lower = 0)

  p_worse <- ifelse(
    se_diff > 0,
    stats::pnorm(0, mean = elpd_diff, sd = se_diff),
    NA_real_
  )

  diag_diff <- ifelse(
    se_diff > 0 & abs(elpd_diff) < 4,
    "|elpd_diff| < 4",
    ""
  )

  diag_elpd <- rep("", length(elpd_diff))
  if (!is.null(n_pointwise)) {
    n_pw <- if (length(n_pointwise) == 1L) {
      rep(n_pointwise, length(elpd_diff))
    } else {
      n_pointwise
    }
    small_n <- n_pw < 100L
    diag_elpd[small_n] <- "N < 100"
  }
  if (!is.null(pareto_k_list)) {
    high_k <- vapply(pareto_k_list, function(k) {
      isTRUE(any(is.finite(k) & k > 0.7))
    }, logical(1L))
    diag_elpd <- ifelse(
      high_k,
      ifelse(diag_elpd == "", "k_psis > 0.7",
             paste0(diag_elpd, "; k_psis > 0.7")),
      diag_elpd
    )
  }

  data.frame(
    p_worse = p_worse,
    diag_diff = diag_diff,
    diag_elpd = diag_elpd,
    stringsAsFactors = FALSE
  )
}
