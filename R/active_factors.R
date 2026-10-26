# Posterior summary of how many latent factors the data use. It is
# meaningful under the MGP column-shrinkage prior, which makes `n_lv`
# a truncation ceiling. Each draw's scaled column norms decide which
# columns are active under the chosen rule. The uncertainty in the
# loadings and in the shrinkage both enter.

#' The per-column innovation scale, as a draws-by-column matrix
#'
#' A factor column contributes `sigma_trend[k] * Z[, k]`. The signal
#' in a column depends on both terms. A fit with no per-column scale
#' returns a `[ndraws, n_lv]` matrix of ones. Callers multiply by the
#' result in every case.
#'
#' @param object A fitted `mvgam`.
#' @param draws_mat Posterior draws matrix.
#' @param n_lv Number of latent factors.
#' @return Numeric `[ndraws, n_lv]` matrix.
#' @noRd
resolve_column_scales <- function(object, draws_mat, n_lv) {
  read_draws_vector(draws_mat, "sigma_trend", n_lv, required = FALSE) %||%
    matrix(1, nrow = nrow(draws_mat), ncol = n_lv)
}


#' Posterior summary of active latent factors under MGP shrinkage
#'
#' For a fitted `mvgam` object with a multiplicative-gamma-process
#' (MGP) loadings prior, returns the posterior distribution of the
#' active factor count and per-factor activity probabilities. The
#' MGP prior shrinks the columns of the loadings matrix `Z` toward
#' zero as the column index grows. `n_lv` is then a truncation
#' ceiling. This function reports how many columns the data use.
#'
#' Each posterior draw `s` gives the per-column squared norms
#' \deqn{c_h^{(s)} = \sigma_h^{2(s)} \|Z[, h]^{(s)}\|^2,} where `Z`
#' is the loadings matrix the model sampled and \eqn{\sigma_h} that
#' column's own latent scale. A column enters the trend as
#' `sigma_trend[h] * Z[, h]`. Multiplicative gamma process shrinkage
#' acts on the scale. A rule on the loadings alone would call every
#' column active. `c_h` sums to the trace of the innovation covariance
#' the factors give the series.
#'
#' @details
#' Three rules decide whether column `h` is active in draw `s`. Each
#' tests a different question:
#' \describe{
#'   \item{`"variance"`}{Does the column explain a share of at least
#'     `threshold` of the latent innovation variance,
#'     \eqn{c_h / \sum_k c_k}? The default `threshold` is `0.05`.}
#'   \item{`"cumulative"`}{Is the column among the fewest columns
#'     whose shares sum to at least `threshold`? The columns are ranked
#'     by `c_h` within each draw. The default `threshold` is `0.95`.
#'     The count is the number of factors the latent variance needs, as
#'     in a scree plot.}
#'   \item{`"relative"`}{Does the column hold at least `threshold`
#'     times the posterior mean norm of the leading column? The default
#'     `threshold` is `0.01`. The rule follows the cumulative shrinkage
#'     criterion of Legramanti, Durante and Dunson (2020).}
#' }
#' `prob_active` is the posterior probability that a column is active.
#' A column with `prob_active >= 1 - prob_threshold` is reported
#' active. The count is summarised over the per-draw counts.
#'
#' The MGP prior expects each column to be smaller than the one
#' before. Under the default `mgp_a2 = 4` the prior median ratio of
#' successive norms is about 0.28. A column the data say nothing about
#' keeps that prior size. All three rules can then count it. A larger
#' `mgp_a2` in the `loadings_prior` argument of [mvgam()] or [jsdgam()]
#' shrinks such columns harder. Comparing `median_norm_sq` across
#' columns with the prior ratio shows whether a column holds more than
#' its prior.
#'
#' The rules use the sampled `Z`. The MGP shrinkage parameter
#' `Psi_diag` is defined on its columns. The QR-identified `Z_tilde`
#' orders its columns by Gram-Schmidt pivots. An active count of k
#' means k of the `n_lv` columns hold posterior signal. It does not
#' identify which k under the MGP ordering.
#'
#' @param object A fitted `mvgam` object whose trend includes
#'   latent factors (`n_lv > 0`).
#' @param criterion The activity rule: `"variance"` (the default),
#'   `"cumulative"` or `"relative"`. Details describes each.
#' @param threshold Numeric in `(0, 1]`, the cut-off of `criterion`.
#'   `NULL` (the default) takes `0.05` for `"variance"`, `0.95` for
#'   `"cumulative"` and `0.01` for `"relative"`.
#' @param prob_threshold Numeric in `(0, 1)`. A column is active when
#'   its posterior probability of activity is at least
#'   `1 - prob_threshold`. Defaults to `0.5`.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A list with class `mvgam_active_factors` containing:
#'   \itemize{
#'     \item `count`: per-draw active count summary
#'       (`median`, `q025`, `q975`).
#'     \item `per_factor`: data frame with one row per sampled
#'       column of `Z`. Its columns are the `factor` index, the
#'       posterior `prob_active`, the posterior `median_norm_sq` (the
#'       scaled squared norm `c_h`), the posterior `median_share` of
#'       the latent variance and `is_active`.
#'     \item `threshold`: list with `criterion`, `value` and
#'       `prob_threshold`.
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
#' @seealso [compare_loadings()] compares the loadings under two
#'   priors. [shared_variation()] reports the variance the factors
#'   share. The `r mvgam_article("jsdgam", "JSDM article")` fits an MGP
#'   prior and counts its active factors.
#'
#' @examples
#' \dontrun{
#' # Counts of 20 species at 100 sites, driven by two latent factors
#' simdat <- sim_jsdm(family = brms::negbinomial(), n_species = 20L,
#'                    n_sites = 100L, n_lv = 2L, seed = 3L)
#'
#' # Up to five factors under the multiplicative gamma process prior,
#' # which shrinks each later column of the loadings toward zero
#' mod <- jsdgam(
#'   y ~ env,
#'   data           = simdat$data_train,
#'   family         = brms::negbinomial(),
#'   n_lv           = 5L,
#'   loadings_prior = "mgp",
#'   chains         = 2,
#'   silent         = 2
#' )
#'
#' # Columns that explain at least 5 percent of the latent variance.
#' # The third column's share is close to that cut
#' af <- active_factors(mod)
#' af
#' plot(af)
#'
#' # The fewest columns that explain 95 percent of the latent variance
#' active_factors(mod, criterion = "cumulative")
#'
#' # Next: ordinate() plots the species on the leading factors
#' }
#'
#' @author Nicholas J Clark
#' @family latent dynamics
#' @export
active_factors <- function(object, ...) {
  UseMethod("active_factors")
}

#' @rdname active_factors
#' @method active_factors mvgam
#' @export
active_factors.mvgam <- function(object,
                                 criterion = c("variance", "cumulative",
                                               "relative"),
                                 threshold = NULL,
                                 prob_threshold = 0.5,
                                 ...) {
  checkmate::assert_class(object, "mvgam")
  criterion <- match.arg(criterion)
  threshold <- threshold %||% active_factor_thresholds[[criterion]]
  checkmate::assert_number(threshold, lower = .Machine$double.eps,
                           upper = 1)
  checkmate::assert_number(
    prob_threshold,
    lower = .Machine$double.eps,
    upper = 1 - .Machine$double.eps
  )
  rlang::check_dots_empty()
  n_lv <- require_factor_n_lv(object, "active_factors()")
  n_series <- loading_series_count(object)
  draws_mat <- posterior::as_draws_matrix(object$fit)
  # A fixed-Z fit gets column norms constant across draws.
  # The loadings are taken in the model basis. `sigma_trend[k]` scales
  # the column `Z[, k]` the model sampled. Each column of `Z_tilde`
  # mixes every sampled column. Pairing it with `sigma_trend[k]`
  # would measure neither column.
  Z_arr <- resolve_factor_loadings(
    object,
    draws_mat,
    n_series = as.integer(n_series),
    n_lv = as.integer(n_lv),
    basis = "model"
  )
  # Z_arr: [ndraws, n_series, n_lv]. Per-column squared norm per
  # draw, scaled by the column's own innovation scale.
  #
  # The rules measure the latent signal in a column,
  # `sigma_trend[k] * Z[, k]`. Multiplicative gamma process shrinkage
  # acts on `sigma_trend` and draws `Z` at unit scale. Every column of
  # `Z` alone then has the same norm distribution. A shrunk column is
  # visible only through its scale.
  col_scale <- resolve_column_scales(object, draws_mat, n_lv)
  norm_sq <- apply(Z_arr, c(1L, 3L), function(col) sum(col^2))
  norm_sq <- norm_sq * col_scale^2
  active <- active_factor_draws(norm_sq, criterion, threshold)
  prob_active <- colMeans(active)
  share <- norm_sq / rowSums(norm_sq)
  qs <- stats::quantile(
    rowSums(active), probs = c(0.025, 0.5, 0.975), names = FALSE
  )
  out <- list(
    count = list(median = qs[2L], q025 = qs[1L], q975 = qs[3L]),
    per_factor = data.frame(
      factor          = seq_len(n_lv),
      prob_active     = prob_active,
      median_norm_sq  = apply(norm_sq, 2L, stats::median),
      median_share    = apply(share, 2L, stats::median),
      is_active       = prob_active >= (1 - prob_threshold)
    ),
    threshold = list(
      criterion      = criterion,
      value          = threshold,
      prob_threshold = prob_threshold
    ),
    n_lv = n_lv
  )
  structure(out, class = "mvgam_active_factors")
}


#' The default cut-off of each activity rule
#' @noRd
active_factor_thresholds <- c(variance = 0.05, cumulative = 0.95,
                              relative = 0.01)


#' Which columns are active in each draw
#'
#' @param norm_sq Numeric `[ndraws, n_lv]` matrix of scaled squared
#'   column norms
#' @param criterion One of `names(active_factor_thresholds)`
#' @param threshold The rule's cut-off
#' @return Logical `[ndraws, n_lv]` matrix
#' @noRd
active_factor_draws <- function(norm_sq, criterion, threshold) {
  checkmate::assert_matrix(norm_sq, mode = "numeric", min.cols = 1L)
  checkmate::assert_choice(criterion, names(active_factor_thresholds))
  checkmate::assert_number(threshold)
  share <- norm_sq / rowSums(norm_sq)
  if (identical(criterion, "relative")) {
    return(norm_sq >= threshold * mean(norm_sq[, 1L]))
  }
  if (identical(criterion, "variance")) {
    return(share >= threshold)
  }
  # The fewest columns, largest first, whose shares reach `threshold`.
  # The tolerance keeps shares that sum to `threshold` in floating
  # point, such as 0.6 + 0.3 against 0.9, from taking another column.
  tol <- sqrt(.Machine$double.eps)
  n_needed <- apply(share, 1L, function(s) {
    reached <- cumsum(sort(s, decreasing = TRUE)) < threshold - tol
    min(length(s), sum(reached) + 1L)
  })
  ranks <- matrix(
    apply(-share, 1L, rank, ties.method = "first"),
    nrow = nrow(share), byrow = TRUE
  )
  ranks <= n_needed
}


#' Plot the posterior active-factor probabilities
#'
#' @description Bar plot of the posterior probability that each
#' latent factor is active under the rule [active_factors()] applied.
#' A dashed horizontal line
#' marks the active threshold (`1 - prob_threshold`, default
#' `0.5`). The subtitle reports the posterior median active count
#' with the 95% credible interval.
#'
#' @param x An `mvgam_active_factors` object returned by
#'   [active_factors()].
#' @param ... Unused. Anything passed here is refused.
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
  rlang::check_dots_empty()
  set_color_scheme_local()
  fill_col <- mvgam_colour("dark")
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
    reference_line(yintercept = cutoff) +
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
  rule <- switch(
    x$threshold$criterion,
    variance = "share of latent variance >= %g",
    cumulative = "fewest columns reaching %g of latent variance",
    relative = "norm >= %g x leading column's posterior mean norm"
  )
  cat("Active latent factors\n")
  cat(sprintf(paste0("  Rule: ", rule, "\n"), x$threshold$value))
  cat(sprintf("  Truncation ceiling: n_lv = %d\n", x$n_lv))
  cat(sprintf(
    "  Posterior median active count: %.1f (95%% CI %.1f - %.1f)\n",
    x$count$median, x$count$q025, x$count$q975
  ))
  cat("\nPer-column posterior probability of being active:\n")
  pf <- x$per_factor
  pf$prob_active    <- round(pf$prob_active,    3)
  pf$median_norm_sq <- signif(pf$median_norm_sq, 3)
  pf$median_share   <- round(pf$median_share,   3)
  print(pf, row.names = FALSE)
  invisible(x)
}


#' Count the series a factor model's loadings span
#'
#' The axis record counts the series. A fit without one takes the
#' count of its fitted series levels.
#'
#' @param object An `mvgam` model object
#' @return Integer count of series
#'
#' @noRd
loading_series_count <- function(object) {
  # The rows of `Z` are the series axis. The axis record counts
  # them. A column's declared levels can include a level with zero
  # rows, which would size every loadings surface one series too
  # wide.
  n <- mvgam_axes(object)$series$n
  if (!is.null(n)) {
    return(as.integer(n))
  }
  as.integer(length(fitted_series_levels(object)))
}
