# Posterior summary of how many latent factors the data use. Each
# draw's factor covariance is decomposed into principal dimensions.
# The rule chosen decides which dimensions are active in that draw.
# The uncertainty in the loadings and in the latent covariance both
# enter.

#' Posterior count of the latent factors a model uses
#'
#' A factor model fitted with `n_lv` factors may need fewer. This
#' function counts the factors whose variance the data support. It
#' suits a fit under the multiplicative gamma process (MGP) prior,
#' `loadings_prior = "mgp"` (Bhattacharya and Dunson 2011). That prior
#' shrinks surplus factors toward zero. `n_lv` then acts as a ceiling.
#' The function applies to every factor model. Under other loadings
#' priors a surplus factor keeps the variance its prior gives it. The
#' count can then exceed the number of factors the data need.
#'
#' The factors give the series the covariance \eqn{Z \Omega Z^\top},
#' where `Z` holds the loadings and \eqn{\Omega} the covariance of the
#' latent states. Its eigenvalues \eqn{\lambda_1 \ge \lambda_2 \ge
#' \dots} are the variances along its principal dimensions. Each
#' posterior draw gives one set. The eigenvalues stay the same when the
#' factors are rotated or relabelled, which the individual columns of
#' `Z` do not.
#'
#' @details
#' Two rules decide whether dimension `k` is active in a draw:
#' \describe{
#'   \item{`"relative"`}{The dimension holds at least `threshold`
#'     times the variance of the leading one,
#'     \eqn{\lambda_k \ge} `threshold` \eqn{\lambda_1}. The default
#'     `threshold` is `0.1`. The rule counts any number of factors of
#'     similar size.}
#'   \item{`"cumulative"`}{The dimension is among the fewest leading
#'     dimensions whose variance reaches a share `threshold` of the
#'     total. The default `threshold` is `0.9`. The count is the
#'     number of factors a scree plot would keep.}
#' }
#' `prob_active` is the posterior probability that a dimension is
#' active. A dimension with `prob_active >= prob` is reported active.
#' The count is summarised over the per-draw counts.
#'
#' Both rules compare the factors with one another. The leading
#' dimension is always active, which makes the count at least one. A
#' model whose factors hold almost no variance still reports one.
#' [shared_variation()] gives the size of that variance on the scale
#' of the linear predictor.
#'
#' @param object A fitted `mvgam` object whose trend includes at
#'   least two latent factors (`n_lv >= 2`).
#' @param criterion The activity rule: `"relative"` (the default) or
#'   `"cumulative"`. Details describes each.
#' @param threshold Numeric in `(0, 1]`, the cut-off of `criterion`.
#'   `NULL` (the default) takes `0.1` for `"relative"` and `0.9` for
#'   `"cumulative"`.
#' @param prob Numeric in `(0, 1)`. A dimension is reported active
#'   when its posterior probability of activity is at least `prob`.
#'   Defaults to `0.5`.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A list with class `mvgam_active_factors` containing:
#'   \itemize{
#'     \item `count`: the posterior `median`, `q025` and `q975` of the
#'       per-draw count of active dimensions.
#'     \item `per_factor`: data frame with one row per dimension,
#'       largest first. Its columns are the `factor` rank, the
#'       posterior `prob_active`, the posterior `median_variance`
#'       \eqn{\lambda_k}, the posterior `median_share` of the total
#'       variance and `is_active`.
#'     \item `threshold`: list with `criterion`, `value` and `prob`.
#'     \item `n_lv`: the number of factors the model was fitted with.
#'   }
#'
#' @references
#' Bhattacharya, A. and Dunson, D. B. (2011). Sparse Bayesian
#'   infinite factor models. *Biometrika*, 98(2), 291-306.
#'   \doi{10.1093/biomet/asr013}.
#'
#' @seealso [shared_variation()] reports the covariance the factors
#'   give the series. [compare_loadings()] compares the loadings under
#'   two priors. The `r mvgam_article("jsdgam", "JSDM article")` fits
#'   an MGP prior and counts its active factors.
#'
#' @examples
#' \dontrun{
#' # Counts of 20 species at 100 sites, driven by two latent factors.
#' # Each species has its own intercept and response to `env`.
#' simdat <- sim_jsdgam(family = brms::negbinomial(), n_species = 20L,
#'                      n_sites = 100L, n_lv = 2L, seed = 3L)
#'
#' # Up to five factors under the MGP prior, which shrinks the factors
#' # the data do not need
#' mod <- jsdgam(
#'   y ~ env * series,
#'   data           = simdat$data_train,
#'   family         = brms::negbinomial(),
#'   n_lv           = 5L,
#'   loadings_prior = "mgp",
#'   chains         = 2,
#'   silent         = 2
#' )
#'
#' # Dimensions holding at least a tenth of the leading one's variance
#' af <- active_factors(mod)
#' af
#' plot(af)
#'
#' # The fewest dimensions that explain 90 percent of the variance
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
                                 criterion = c("relative", "cumulative"),
                                 threshold = NULL,
                                 prob = 0.5,
                                 ...) {
  checkmate::assert_class(object, "mvgam")
  criterion <- match.arg(criterion)
  threshold <- threshold %||% active_factor_thresholds[[criterion]]
  checkmate::assert_number(threshold, lower = .Machine$double.eps,
                           upper = 1)
  checkmate::assert_number(prob, lower = .Machine$double.eps,
                           upper = 1 - .Machine$double.eps)
  rlang::check_dots_empty()
  n_lv <- require_factor_n_lv(object, "active_factors()")
  if (n_lv < 2L) {
    stop(insight::format_error(c(
      "'active_factors()' compares factors and needs 'n_lv' >= 2.",
      i = "Refit with more factors to count how many the data use."
    )), call. = FALSE)
  }
  parts <- factor_model_draws(object, n_lv,
                              n_series = loading_series_count(object))
  variance <- factor_eigenvalues(parts$latent, parts$Z_arr)
  active <- active_factor_draws(variance, criterion, threshold)
  prob_active <- colMeans(active)
  qs <- stats::quantile(
    rowSums(active), probs = c(0.025, 0.5, 0.975), names = FALSE
  )
  out <- list(
    count = list(median = qs[2L], q025 = qs[1L], q975 = qs[3L]),
    per_factor = data.frame(
      factor          = seq_len(ncol(variance)),
      prob_active     = prob_active,
      median_variance = apply(variance, 2L, stats::median),
      median_share    = apply(variance / rowSums(variance), 2L,
                              stats::median),
      is_active       = prob_active >= prob
    ),
    threshold = list(criterion = criterion, value = threshold,
                     prob = prob),
    n_lv = n_lv
  )
  structure(out, class = "mvgam_active_factors")
}


#' The default cut-off of each activity rule
#' @noRd
active_factor_thresholds <- c(relative = 0.1, cumulative = 0.9)


#' Variance along each principal dimension of the factor covariance
#'
#' The nonzero eigenvalues of `Z Omega Z'` equal those of the
#' `k x k` matrix `A' A`, with `A = Z V D^(1/2)` and `Omega = V D V'`.
#' Each draw then costs two `k x k` decompositions, whatever the
#' number of series.
#'
#' @param latent Numeric `[ndraws, k, k]` latent covariance draws
#' @param Z_arr Numeric `[ndraws, n_series, k]` loadings draws
#' @return Numeric `[ndraws, min(k, n_series)]` matrix of eigenvalues,
#'   largest first in each row
#' @noRd
factor_eigenvalues <- function(latent, Z_arr) {
  checkmate::assert_array(latent, mode = "numeric", d = 3L)
  checkmate::assert_array(Z_arr, mode = "numeric", d = 3L)
  ndraws <- dim(Z_arr)[1L]
  n_series <- dim(Z_arr)[2L]
  k <- dim(Z_arr)[3L]
  if (!identical(dim(latent), c(ndraws, k, k))) {
    stop_shape_fault("The latent covariance draws", dim(latent),
                     c(ndraws, k, k))
  }
  n_dim <- min(k, n_series)
  out <- matrix(0, nrow = ndraws, ncol = n_dim)
  for (d in seq_len(ndraws)) {
    omega <- eigen(matrix(latent[d, , ], nrow = k), symmetric = TRUE)
    A <- matrix(Z_arr[d, , ], nrow = n_series) %*% omega$vectors %*%
      diag(sqrt(pmax(omega$values, 0)), nrow = k)
    out[d, ] <- eigen(crossprod(A), symmetric = TRUE,
                      only.values = TRUE)$values[seq_len(n_dim)]
  }
  # Rounding can leave an eigenvalue of a rank-deficient covariance
  # just below zero
  pmax(out, 0)
}


#' Which dimensions are active in each draw
#'
#' @param variance Numeric `[ndraws, n_dim]` matrix of eigenvalues,
#'   largest first in each row
#' @param criterion One of `names(active_factor_thresholds)`
#' @param threshold The rule's cut-off
#' @return Logical `[ndraws, n_dim]` matrix
#' @noRd
active_factor_draws <- function(variance, criterion, threshold) {
  checkmate::assert_matrix(variance, mode = "numeric", min.cols = 1L)
  checkmate::assert_choice(criterion, names(active_factor_thresholds))
  checkmate::assert_number(threshold)
  if (identical(criterion, "relative")) {
    return(variance >= threshold * variance[, 1L])
  }
  # A dimension is needed while the larger ones before it fall short of
  # `threshold`. The tolerance keeps shares that sum to `threshold` in
  # floating point, such as 0.6 + 0.3 against 0.9, from taking another.
  share <- variance / rowSums(variance)
  before <- share
  before[, 1L] <- 0
  for (k in seq_len(ncol(share))[-1L]) {
    before[, k] <- before[, k - 1L] + share[, k - 1L]
  }
  before < threshold - sqrt(.Machine$double.eps)
}


#' Plot the posterior active-factor probabilities
#'
#' @description Bar plot of the posterior probability that each
#' dimension of the factor covariance is active, largest first, under
#' the rule [active_factors()] applied. A dashed line marks `prob`.
#' `print()` reports the posterior median count with its 95% credible
#' interval.
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
  cutoff <- x$threshold$prob
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
      x = "Factor, largest variance first",
      y = "Posterior probability of being active",
      title = sprintf("Active latent factors (n_lv = %d)", x$n_lv)
    ) +
    mvgam_theme()
}


#' @export
print.mvgam_active_factors <- function(x, ...) {
  rule <- switch(
    x$threshold$criterion,
    relative = "variance at least %g times the leading factor's",
    cumulative = "fewest factors reaching a %g share of the variance"
  )
  cat("Active latent factors\n")
  cat(sprintf(paste0("  Rule: ", rule, "\n"), x$threshold$value))
  cat(sprintf("  Fitted with: n_lv = %d\n", x$n_lv))
  cat(sprintf(
    "  Posterior median active count: %.1f (95%% CI %.1f - %.1f)\n",
    x$count$median, x$count$q025, x$count$q975
  ))
  cat("\nPosterior probability that each factor is active,",
      "largest first:\n")
  pf <- x$per_factor
  pf$prob_active     <- round(pf$prob_active, 3)
  pf$median_variance <- signif(pf$median_variance, 3)
  pf$median_share    <- round(pf$median_share, 3)
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
