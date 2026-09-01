# Posterior summary of how many latent factors the data actively
# uses. Only meaningful under the MGP column-shrinkage prior, where
# `n_lv` is a truncation ceiling rather than the exact factor count.
# Implements the cumulative-shrinkage-style criterion of Legramanti,
# Durante and Dunson (2020): factor h is "active" iff the posterior
# probability that its squared loading norm sits below a fraction
# of the leading column's norm is at most `prob_threshold`. Both
# the loadings AND the shrinkage uncertainty propagate through.

#' The per-column innovation scale, as a draws-by-column matrix
#'
#' A factor column contributes `sigma_trend[k] * Z[, k]`, so any
#' question about how much signal a column carries needs both
#' halves. Returns a `[ndraws, n_lv]` matrix of ones when the fit
#' has no per-column scale, so callers can multiply unconditionally.
#'
#' @param object A fitted `mvgam`.
#' @param draws_mat Posterior draws matrix.
#' @param n_lv Number of latent factors.
#' @return Numeric `[ndraws, n_lv]` matrix.
#' @noRd
resolve_column_scales <- function(object, draws_mat, n_lv) {
  ndraws <- nrow(draws_mat)
  cols <- paste0("sigma_trend[", seq_len(n_lv), "]")
  present <- cols %in% colnames(draws_mat)
  if (!any(present)) {
    return(matrix(1, nrow = ndraws, ncol = n_lv))
  }
  out <- matrix(1, nrow = ndraws, ncol = n_lv)
  out[, present] <- as.matrix(draws_mat[, cols[present], drop = FALSE])
  out
}


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
#' @examples
#' \dontrun{
#' set.seed(1)
#' simdat <- sim_closure_unit_data(
#'   family    = occ(),
#'   n_species = 4L,
#'   n_sites   = 50L,
#'   n_visits  = 4L,
#'   type      = 2L
#' )
#' mod <- jsdgam(
#'   formula        = bf(y ~ env, p ~ tod_c),
#'   factor_formula = ~ -1,
#'   data           = simdat$data_train,
#'   family         = occ(),
#'   n_lv           = 2L,
#'   chains         = 2,
#'   silent         = 2
#' )
#'
#' # Posterior median count of active factors under the
#' # Legramanti / Durante / Dunson (2020) criterion.
#' active_factors(mod)
#' }
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
  n_series <- loading_series_count(object)
  draws_mat <- posterior::as_draws_matrix(object$fit)
  # `resolve_Z_loadings()` returns the sampled Z / Z_tilde
  # array when the fit has free loadings, or the fully-fixed
  # trend_map matrix broadcast across draws when Z is data.
  # Fixed-Z fits get column-norm summaries that are constant
  # across draws (posterior mass on the deterministic value).
  # The model basis, not the QR-identified one: `sigma_trend[k]`
  # belongs to the column `Z[, k]` the model sampled, while
  # `Z_tilde[, k]` is a mixture of every column, so pairing the two
  # measures a quantity that belongs to neither. On a ten-factor
  # shrinkage fit the rotated norms carry a QR gradient of 51 down
  # to 28 while the sampled ones sit flat around 41, which is what
  # the unit-scale prior puts there.
  Z_arr <- resolve_Z_loadings(
    object,
    draws_mat,
    n_series = as.integer(n_series),
    n_lv = as.integer(n_lv),
    basis = "model"
  )
  # Z_arr: [ndraws, n_series, n_lv]. Per-column squared norm per
  # draw, scaled by the column's own innovation scale.
  #
  # The criterion asks how much of the latent signal a column
  # carries, and that is `sigma_trend[k] * Z[, k]`, not `Z[, k]`.
  # The distinction is the whole answer under multiplicative gamma
  # process shrinkage, which puts the shrinkage in `sigma_trend`
  # and draws `Z` at unit scale: every column's norm then has the
  # same distribution, so reading `Z` alone reports a shrunk
  # column as active. On a ten-factor MGP fit `sigma_trend` ran
  # from 2.06 to 0.013 while the raw column norms ran from 7.1 to
  # 5.2, and all ten columns were called active.
  col_scale <- resolve_column_scales(object, draws_mat, n_lv)
  norm_sq <- apply(Z_arr, c(1L, 3L), function(col) sum(col^2))
  norm_sq <- norm_sq * col_scale^2
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

#' Plot the posterior active-factor probabilities
#'
#' @description Bar plot of the posterior probability that each
#' latent factor is active under the Legramanti, Durante & Dunson
#' (2020) cumulative-shrinkage criterion. A dashed horizontal line
#' marks the active threshold (`1 - prob_threshold`, default
#' `0.5`). The subtitle reports the posterior median active count
#' with the 95% credible interval.
#'
#' @param x An `mvgam_active_factors` object returned by
#'   [active_factors()].
#' @param ... Ignored.
#'
#' @return A `ggplot` object.
#'
#' @seealso [active_factors()]
#'
#' @author Nicholas J Clark
#'
#' @method plot mvgam_active_factors
#' @export
plot.mvgam_active_factors <- function(x, ...) {
  checkmate::assert_class(x, "mvgam_active_factors")
  # Single-category fill: pick the strong tone (`[5L]`) from the
  # active bayesplot scheme via mvgam_palette() to match the
  # convention in plot_mvgam_series() / pp_check.mvgam().
  fill_col <- mvgam_palette()[5L]
  cutoff <- 1 - x$threshold$prob_threshold
  subtitle <- sprintf(
    "Posterior median active count: %.1f  (95%% CI %.1f, %.1f)",
    x$count$median, x$count$q025, x$count$q975
  )
  ggplot2::ggplot(
    data = x$per_factor,
    mapping = ggplot2::aes(x = .data$factor, y = .data$prob_active)
  ) +
    ggplot2::geom_col(fill = fill_col, alpha = 0.85) +
    ggplot2::geom_hline(
      yintercept = cutoff, linetype = "dashed", colour = "grey40"
    ) +
    ggplot2::scale_x_continuous(breaks = x$per_factor$factor) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1), expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::labs(
      x = "Factor index", y = "Posterior probability of being active",
      title = sprintf(
        "Active latent factors (n_lv ceiling = %d)", x$n_lv
      ),
      subtitle = subtitle
    ) +
    mvgam_theme()
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


#' Count the series a factor model's loadings span
#'
#' The count comes from whichever record the fit carries: multi-
#' response fits name their responses, `jsdgam()` keeps the observation
#' data, and everything else is answered by the canonical series
#' resolver. Reading only the first two is what left a factor fit from
#' `mvgam()` with a count of zero, since neither is populated there.
#'
#' @param object An `mvgam` model object
#' @return Integer count of series
#'
#' @noRd
loading_series_count <- function(object) {
  n <- length(object$trend_components$resp_names)
  if (n > 0L) {
    return(as.integer(n))
  }
  n <- length(levels(object$obs_data$series))
  if (n > 0L) {
    return(as.integer(n))
  }
  as.integer(length(resolve_series_info(object)$series_levels))
}
