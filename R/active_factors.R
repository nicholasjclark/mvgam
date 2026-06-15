# Posterior summary of how many latent factors the data actively
# uses. Only meaningful under the MGP column-shrinkage prior, where
# `n_lv` is a truncation ceiling rather than the exact factor count.
# Implements the cumulative-shrinkage-style criterion of Legramanti,
# Durante and Dunson (2020): factor h is "active" iff the posterior
# probability that its squared loading norm sits below a fraction
# of the leading column's norm is at most `prob_threshold`. Both
# the loadings AND the shrinkage uncertainty propagate through.

#' Posterior summary of active latent factors under MGP shrinkage
#'
#' For a fitted `mvgam` object with a multiplicative-gamma-process
#' (MGP) loadings prior, returns the posterior distribution of the
#' active factor count and per-factor activity probabilities. The
#' MGP prior shrinks columns of the loadings matrix `Z` toward zero
#' as the column index grows, so `n_lv` is a **truncation ceiling**
#' rather than the exact factor count; this helper reports how many
#' columns the data actually used.
#'
#' Criterion (Legramanti, Durante and Dunson 2020). For each
#' posterior draw `s`, compute the per-column squared norms
#' \deqn{c_h^{(s)} = \|Z_\text{tilde}[, h]^{(s)}\|^2,} where
#' `Z_tilde` is the QR-identified loadings matrix. The reference
#' scale is the posterior mean of the leading-column norm
#' `ref = mean_s c_1^{(s)}`, and the activity threshold is
#' `eps = fraction * ref`. Factor `h` is **active** iff
#' \deqn{P(c_h^{(s)} < \text{eps}) \le \text{prob\_threshold},}
#' i.e. the column is unlikely to be shrunk near zero. The
#' posterior median of the per-draw active count is reported as
#' the headline summary.
#'
#' Important caveat (QR-vs-MGP composition). `Psi_diag` (the MGP
#' column-shrinkage parameter) lives in the **unrotated** Z basis;
#' the post-hoc QR step re-orders columns of `Z_tilde` by
#' Gram-Schmidt pivots, NOT by the MGP prior ordering. The
#' "factor h" labelled by this function is the h-th column of
#' `Z_tilde`, NOT necessarily the h-th MGP factor. Read the result
#' as "k of `n_lv` truncation-ceiling factors carry meaningful
#' posterior signal", not as "factors 1..k under the MGP ordering
#' are active".
#'
#' @param object A fitted `mvgam` object whose trend includes
#'   latent factors (`n_lv > 0`).
#' @param fraction Numeric in `(0, 1]`. The activity threshold is
#'   `fraction * mean(||Z_tilde[, 1]||^2)`. Defaults to `0.01`
#'   (each active column carries at least 1 percent of the
#'   leading column's squared norm in posterior mean).
#' @param prob_threshold Numeric in `(0, 1)`. Factor `h` is
#'   inactive iff `P(||Z_tilde[, h]||^2 < eps)` exceeds this.
#'   Defaults to `0.5`.
#' @param ... Additional arguments for S3 dispatch (unused).
#'
#' @return A list with class `mvgam_active_factors` containing:
#'   \itemize{
#'     \item `count`: per-draw active count summary
#'       (`median`, `q025`, `q975`).
#'     \item `per_factor`: data frame with one row per column of
#'       `Z_tilde` giving `factor` index, posterior `prob_active`,
#'       posterior `median_norm_sq`, and `is_active` (TRUE iff the
#'       prob_active threshold is met).
#'     \item `threshold`: list with `fraction`, `prob_threshold`,
#'       resolved `epsilon`.
#'     \item `n_lv`: the truncation ceiling specified at fit time.
#'   }
#'
#' @references
#' Legramanti, S., Durante, D. and Dunson, D. B. (2020). Bayesian
#'   cumulative shrinkage for infinite factorizations.
#'   *Biometrika*, 107(3), 745-752.
#'   \doi{10.1093/biomet/asaa008}.
#'
#' Bhattacharya, A. and Dunson, D. B. (2011). Sparse Bayesian
#'   infinite factor models. *Biometrika*, 98(2), 291-306.
#'   \doi{10.1093/biomet/asr013}.
#'
#' @author Nicholas J Clark
#' @export
active_factors <- function(object, ...) {
  UseMethod("active_factors")
}

#' @rdname active_factors
#' @method active_factors mvgam
#' @export
active_factors.mvgam <- function(object,
                                 fraction = 0.01,
                                 prob_threshold = 0.5,
                                 ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_number(
    fraction, lower = .Machine$double.eps, upper = 1
  )
  checkmate::assert_number(
    prob_threshold,
    lower = .Machine$double.eps,
    upper = 1 - .Machine$double.eps
  )
  n_lv <- detect_factor_n_lv(object)
  if (is.null(n_lv) || n_lv < 1L) {
    stop(insight::format_error(
      "active_factors() requires a latent-factor fit (n_lv > 0)."
    ))
  }
  n_series <- length(object$trend_components$resp_names)
  if (is.null(n_series) || n_series < 1L) {
    n_series <- length(levels(object$obs_data$series))
  }
  draws_mat <- posterior::as_draws_matrix(object$fit)
  Z_arr <- extract_Z_loadings(
    draws_mat,
    n_obs_series = as.integer(n_series),
    n_lv = as.integer(n_lv)
  )
  # Z_arr: [ndraws, n_series, n_lv]. Per-column squared norm per draw.
  norm_sq <- apply(Z_arr, c(1L, 3L), function(col) sum(col^2))
  # Reference scale from the leading column posterior mean.
  ref <- mean(norm_sq[, 1L])
  eps <- fraction * ref
  prob_active <- vapply(
    seq_len(n_lv),
    function(h) mean(norm_sq[, h] >= eps),
    numeric(1L)
  )
  is_active <- prob_active >= (1 - prob_threshold)
  median_norm_sq <- vapply(
    seq_len(n_lv),
    function(h) stats::median(norm_sq[, h]),
    numeric(1L)
  )
  per_draw_count <- rowSums(norm_sq >= eps)
  qs <- stats::quantile(
    per_draw_count, probs = c(0.025, 0.5, 0.975), names = FALSE
  )
  out <- list(
    count = list(median = qs[2L], q025 = qs[1L], q975 = qs[3L]),
    per_factor = data.frame(
      factor          = seq_len(n_lv),
      prob_active     = prob_active,
      median_norm_sq  = median_norm_sq,
      is_active       = is_active
    ),
    threshold = list(
      fraction       = fraction,
      prob_threshold = prob_threshold,
      epsilon        = eps
    ),
    n_lv = n_lv
  )
  structure(out, class = "mvgam_active_factors")
}

#' @export
print.mvgam_active_factors <- function(x, ...) {
  cat("Active latent factors (Legramanti / Durante / Dunson 2020 criterion)\n")
  cat(sprintf(
    "  Truncation ceiling: n_lv = %d\n",
    x$n_lv
  ))
  cat(sprintf(
    "  Posterior median active count: %.1f (95%% CI %.1f - %.1f)\n",
    x$count$median, x$count$q025, x$count$q975
  ))
  cat(sprintf(
    "  Threshold: epsilon = %.4f (= %.3g x leading-column posterior mean norm^2)\n",
    x$threshold$epsilon, x$threshold$fraction
  ))
  cat("\nPer-column posterior probability of being active:\n")
  pf <- x$per_factor
  pf$prob_active    <- round(pf$prob_active,    3)
  pf$median_norm_sq <- round(pf$median_norm_sq, 4)
  print(pf, row.names = FALSE)
  invisible(x)
}
