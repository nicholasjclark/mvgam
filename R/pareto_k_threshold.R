# Adaptive PSIS Pareto-k threshold from Vehtari, Simpson, Gelman,
# Yao & Gabry (2024, JMLR 25:72). Loo ships the same formula
# internally as `loo:::ps_khat_threshold` but does not export it,
# so mvgam re-hosts the one-liner rather than reach into loo's
# namespace via `:::`.
#
# The rule tightens the threshold when the posterior draw count
# `S` is small (PSIS is less reliable there) and clamps at 0.7
# once `S` is large enough for the classical guarantee to hold.
# Used by `lfo_cv.mvgam()` and `kfold.mvgam()` when the caller
# leaves `pareto_k_threshold = NULL`.
#
#'@noRd
mvgam_ps_khat_threshold <- function(S) {
  # Reason: at S = 1, log10(S) = 0 gives a divide-by-zero and a
  # threshold of -Inf. In practice mvgam fits have S >> 100, so
  # rejecting S < 2 keeps the formula well defined without ruling
  # out any realistic call.
  checkmate::assert_int(S, lower = 2L)
  min(1 - 1 / log10(S), 0.7)
}


# The threshold a refit gate will apply, given what the caller asked
# for and how many draws the posterior holds. `NULL` means the
# caller left the choice to the adaptive rule above.
#
# `lfo_cv()` and `kfold()` both gate their refits on this and each
# resolved it for itself, so the rule was written twice in two files
# and had to be kept in step by hand.
#'@noRd
resolve_pareto_k_threshold <- function(pareto_k_threshold, n_draws) {
  if (is.null(pareto_k_threshold)) {
    mvgam_ps_khat_threshold(n_draws)
  } else {
    pareto_k_threshold
  }
}


# The threshold a result was produced under.
#
# A result records the number its gate applied, so this reads it.
# It carried two fields instead: `pareto_k_threshold` held whatever
# the caller passed, which is `NULL` whenever the adaptive rule
# chose, and `pareto_k_threshold_used` held the number. A reader
# reaching for the documented name got the empty one. The pair is
# now one field holding the applied number, with
# `pareto_k_threshold_adaptive` saying who chose it; the older
# spelling is understood here so a result saved before the change
# still reads.
#'@noRd
pareto_k_threshold_of <- function(x) {
  x$pareto_k_threshold %||% x$pareto_k_threshold_used
}


# Whether the adaptive rule chose the threshold rather than the
# caller. An older result says so by leaving `pareto_k_threshold`
# empty, which is the spelling this replaces.
#'@noRd
pareto_k_threshold_is_adaptive <- function(x) {
  x$pareto_k_threshold_adaptive %||% is.null(x$pareto_k_threshold)
}


# Diagnostic columns for a paired ELPD comparison table,
# following Sivula, Magnusson, Matamoros & Vehtari (2025,
# Bayesian Analysis, DOI 10.1214/25-BA1569). The paper is
# diagnostic rather than corrective: the classical
# `sqrt(N) * sd(pointwise_diff)` SE is retained, but three
# named regimes indicate when it should be distrusted:
#
#   * `p_worse` = normal-CDF probability that the true elpd
#     difference is less than zero (i.e. the candidate is worse
#     than the reference), given the point estimate and SE. NA
#     for the reference row where `se_diff = 0`.
#   * `diag_diff` = "|elpd_diff| < 4" when the point estimate
#     sits inside the small-effect regime where the normal SE
#     underestimates uncertainty most.
#   * `diag_elpd` = "N < 100" when the pointwise count is too
#     small for the SE bias to shrink at the sqrt-N rate, plus
#     "k_psis > 0.7" when any Pareto-k exceeds 0.7 (unstable
#     PSIS).
#
#'@noRd
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
