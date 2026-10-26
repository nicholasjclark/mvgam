#' `mvgam_residcor` object description
#'
#' A \code{mvgam_residcor} object returned by function [residual_cor()].
#' Run `methods(class = "mvgam_residcor")` to see the available
#' methods.
#'
#' @return Objects of this class are structured as a `list` with the
#'   following components:
#'
#'  \item{cor, cor_se, cor_lower, cor_upper}{Posterior summaries of
#'  the \eqn{p \times p} correlation matrix: the posterior mean (the
#'  median under `robust = TRUE`), the posterior SD (the MAD under
#'  `robust = TRUE`) and the limits of the credible interval at the
#'  `probs` levels.}
#'  \item{cor_ess}{\eqn{p \times p} matrix of per-entry effective
#'  sample sizes of the correlation draws, computed on the Fisher-z
#'  scale with [posterior::ess_basic()]. The diagonal is `NA`, being
#'  fixed at 1. A low value marks a correlation the chains sampled
#'  poorly, however narrow its interval.}
#'  \item{prob_positive, prob_negative, prob_nonzero}{\eqn{p \times p}
#'  matrices of `Pr(r > 0)`, `Pr(r < 0)` and the larger of the two for
#'  every off-diagonal entry. Each measures how well the data
#'  determine the sign of a correlation.}
#'  \item{sig_cor}{The correlation matrix with every entry set to zero
#'  where `prob_nonzero` is at most `prob_threshold`.
#'  [plot.mvgam_residcor()] draws `cor`. A hard cut would draw a
#'  correlation the data leave uncertain the same as one estimated at
#'  zero.}
#'  \item{cov, cov_se, cov_lower, cov_upper, cov_ess}{Posterior
#'  summaries of the residual covariance matrix, with per-entry ESS.}
#'  \item{mean_abs_offdiag}{A `list` of `point`, `lower` and `upper`
#'  summarising the mean absolute off-diagonal correlation. The mean is
#'  taken within each draw and then summarised across draws. The
#'  interval then describes the overall strength of correlation.}
#'  \item{n_series, series_names}{Dimensions of the matrix and the
#'  series labels.}
#'  \item{pattern, hierarchical, group_label}{The residual structure
#'  that was summarised.}
#'  \item{covariance}{`"stationary"` for the covariance of the latent
#'  states, or `"innovation"` for that of one step's shocks, as
#'  `residual_cor()` was asked.}
#'  \item{probs, prob_threshold}{The credible interval levels and the
#'  `prob_nonzero` threshold of `sig_cor`.}
#'
#' Passing `partial = TRUE` to `residual_cor()` adds the partial
#' correlations, with one field for each correlation field: `prec`,
#' `prec_se`, `prec_lower`, `prec_upper`, `prec_ess`,
#' `prec_prob_positive`, `prec_prob_negative`, `prec_prob_nonzero`
#' and the thresholded `sig_prec`. Partial correlations are bounded
#' like correlations. They are summarised on the same Fisher-z scale.
#' [plot.mvgam_residcor()] with `type = "precision"` requires them.
#'
#' @details
#' In a latent factor model the residual covariance between series is
#' \eqn{\Sigma = Z \Omega Z'}, where \eqn{Z} holds the factor
#' loadings and \eqn{\Omega} is the covariance of the latent states.
#' A strong residual correlation between two species can reflect a
#' species interaction (facilitation or competition), a missing
#' covariate, or any other association the predictors in `formula`
#' leave unexplained (Hui 2016).
#'
#' The partial correlation between two series conditions on every
#' other series (Ovaskainen et al. 2016). It is computed from the
#' inverse of the correlation matrix, with the diagonal set to 1. Two
#' species with a partial correlation near zero can still be
#' correlated. They are then conditionally independent given the other
#' species. Their association runs *through* those species. A larger
#' partial correlation suggests a more direct association.
#'
#' @references
#' Francis KC Hui (2016). BORAL - Bayesian ordination and regression
#' analysis of multivariate abundance data in R. Methods in Ecology and
#' Evolution. 7, 744-750.
#' \cr
#' \cr
#' Otso Ovaskainen et al. (2016). Using latent variable models to
#' identify large networks of species-to-species associations at
#' different spatial scales. Methods in Ecology and Evolution, 7,
#' 549-555.
#' @seealso `jsdgam()`, [residual_cor()]
#' @author Nicholas J Clark
#' @name mvgam_residcor-class
NULL

#' Plot residual correlations or precisions from a `mvgam_residcor` object
#'
#' Heatmap of the residual correlations or partial correlations
#' from a Joint Species Distribution (\code{jsdgam}) or dynamic
#' factor (\code{mvgam}) model.
#'
#' @param x A `mvgam_residcor` object returned by
#'   `residual_cor(..., summary = TRUE)`.
#' @param type Character. Which matrix to plot. `"correlation"` (the
#'   default) draws `x$cor`. `"precision"` draws `x$prec`, the partial
#'   correlations (Ovaskainen et al. 2016). `residual_cor()` computes
#'   them under `partial = TRUE`.
#' @param cluster Logical. When `TRUE`, the matrix is reordered by an
#'   approximate Robinson ordering (Gruvaeus & Wainer 1972; average
#'   linkage on the `1 - cormat` distance). Clusters of positive and
#'   negative correlations then lie adjacent. Defaults to `FALSE`.
#' @param rescale Logical. By default colour spans `[-1, 1]`, the full
#'   range of a correlation. Panels from different fits are then
#'   comparable. A correlation of 0.4 looks unlike one of 1. Set
#'   `TRUE` to scale colour to the entries present, for a fit whose
#'   correlations are all small. Defaults to `FALSE`.
#' @param ... Unused. Anything passed here is refused.
#'
#' @details Only the lower triangle of off-diagonal entries is
#'   drawn; a symmetric matrix says everything once and the
#'   diagonal is `1` for any correlation. Fill shows the posterior
#'   point estimate. A black outline marks each pair whose credible
#'   interval excludes zero. [summary.mvgam_residcor()] reports each
#'   pair's interval and `prob_nonzero`.
#'
#' @return A `ggplot` object.
#'
#' @seealso [residual_cor()], [summary.mvgam_residcor()]
#'
#' @author Nicholas J Clark
#'
#' @method plot mvgam_residcor
#' @export
plot.mvgam_residcor <- function(
  x,
  type = c("correlation", "precision"),
  cluster = FALSE,
  rescale = FALSE,
  ...
) {
  type <- match.arg(type)
  checkmate::assert_flag(cluster)
  checkmate::assert_flag(rescale)
  rlang::check_dots_empty()
  set_color_scheme_local()

  scale_name <- if (type == "correlation") {
    "Posterior\ncorrelation"
  } else {
    "Posterior\npartial\ncorrelation"
  }
  residcor_heatmap(residcor_panel(x, type, cluster), scale_name, rescale)
}


#' The lower triangle of a summarised residual correlation, with the
#' pairs whose interval excludes zero marked
#'
#' @param x A summarised `mvgam_residcor`
#' @param type `"correlation"` or `"precision"`
#' @param cluster Reorder by `cluster_cormat()`
#' @return Frame from `residcor_panel_data()`
#' @noRd
residcor_panel <- function(x, type = "correlation", cluster = FALSE) {
  assert_residcor_summarised(x)
  field <- if (type == "correlation") "cor" else "prec"
  estimate <- x[[field]] %||% stop(insight::format_error(c(
    "The 'mvgam_residcor' object holds no partial correlations.",
    i = "Pass 'partial = TRUE' to 'residual_cor()' to compute them."
  )), call. = FALSE)
  residcor_panel_data(
    estimate, cluster = cluster,
    clear = interval_excludes_zero(x[[paste0(field, "_lower")]],
                                   x[[paste0(field, "_upper")]])
  )
}


#' Heatmap of one or more residual correlation panels
#'
#' @param panel Frame from `residcor_panel()`, optionally with `group`
#' @param scale_name Legend title
#' @param rescale Scale colour to the entries present
#' @param ncol Facet columns when `panel` has a `group` column
#' @return A `ggplot` object
#' @noRd
residcor_heatmap <- function(panel, scale_name, rescale, ncol = NULL) {
  lim <- if (rescale) max(abs(panel$value), na.rm = TRUE) else 1
  matrix_heatmap(
    panel,
    mvgam_diverging_scale(name = scale_name, limits = c(-lim, lim)),
    layout = "triangle", ncol = ncol
  )
}


#' Plot method for an `mvgam_residcor_list`
#'
#' Faceted heatmap panel across the per-group correlation
#' matrices returned by `residual_cor(mod, by_group = TRUE)` on a
#' hierarchical trend fit. Each panel is drawn as by
#' [plot.mvgam_residcor()], one per group, with fill showing the
#' posterior point estimate and an outline on each pair whose interval
#' excludes zero. The facets share colour limits. Panels are then
#' directly comparable. The `_global` reference matrix is included by
#' default. Drop it with `include_global = FALSE`.
#'
#' @param x An `mvgam_residcor_list` returned by
#'   `residual_cor(mod, by_group = TRUE)`.
#' @param include_global Logical. If `TRUE` (default), include
#'   the `_global` reference correlation as one panel.
#' @param ncol Optional integer. Number of facet columns; defaults
#'   to `ceiling(sqrt(n_panels))`.
#' @inheritParams plot.mvgam_residcor
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @seealso [residual_cor()], [plot.mvgam_residcor()],
#'   [plot.mvgam_var_matrix_list()].
#' @author Nicholas J Clark
#' @method plot mvgam_residcor_list
#' @export
plot.mvgam_residcor_list <- function(x, include_global = TRUE,
                                      ncol = NULL, rescale = FALSE,
                                      ...) {
  checkmate::assert_flag(include_global)
  checkmate::assert_flag(rescale)
  rlang::check_dots_empty()
  set_color_scheme_local()
  keep <- if (isTRUE(include_global)) names(x) else
    setdiff(names(x), "_global")
  panel <- do.call(rbind, lapply(keep, function(g) {
    df <- residcor_panel(x[[g]])
    df$group <- g
    df
  }))
  residcor_heatmap(panel, "Posterior\ncorrelation", rescale, ncol)
}
