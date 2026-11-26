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
