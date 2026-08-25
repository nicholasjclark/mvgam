#' `mvgam_residcor` object description
#'
#' A \code{mvgam_residcor} object returned by function [residual_cor()].
#' Run `methods(class = "mvgam_residcor")` to see an overview of available methods.
#' @return Objects of this class are structured as a `list` with the following components:
#'
#'  \item{cor, cor_se, cor_lower, cor_upper}{Posterior summaries of
#'  the \eqn{p \times p} correlation matrix: median (or mean), standard
#'  error, and lower / upper limits of the credible intervals at the
#'  `probs` levels.}
#'  \item{cor_ess}{\eqn{p \times p} matrix of per-entry effective
#'  sample sizes for the correlation draws (computed on the Fisher-z
#'  scale via [posterior::ess_basic()]). Diagonal entries are `NA`
#'  because the diagonal is fixed at 1. Use this to flag
#'  correlations that are nominally precise but poorly resolved by
#'  the MCMC chains.}
#'  \item{prob_positive, prob_negative, prob_nonzero}{\eqn{p \times p}
#'  matrices of `Pr(r > 0)`, `Pr(r < 0)` and the larger of the two for
#'  every off-diagonal entry. Use these for a sign-determinability
#'  filter rather than the binary "CI does not contain zero" rule.}
#'  \item{sig_cor}{A \eqn{p \times p} correlation matrix containing
#'  only those correlations whose credible interval excludes zero. All
#'  other entries are set to zero.}
#'  \item{cov, cov_se, cov_lower, cov_upper, cov_ess}{Posterior
#'  summaries of the residual covariance matrix, with per-entry ESS
#'  matching the correlation surface.}
#'  \item{mean_abs_offdiag}{Mean of the absolute off-diagonal entries
#'  of the posterior median correlation, a scalar summary of overall
#'  correlation strength.}
#'  \item{n_series, series_names}{Dimensions of the matrix and the
#'  series labels.}
#'  \item{pattern, hierarchical, group_label}{Bookkeeping fields
#'  recording the residual structure that was summarised.}
#'  \item{probs, prob_threshold}{The credible interval and threshold
#'  used by `residual_cor()` to populate `sig_cor`.}
#'
#' Precision-matrix slots (`prec`, `prec_se`, `prec_lower`,
#' `prec_upper`, `prec_ess`) are populated when `partial = TRUE` is
#' passed to `residual_cor()`. The `sig_prec` thresholded matrix is
#' reserved for a future extension; until then,
#' [plot.mvgam_residcor()] with `type = "precision"` errors clearly
#' when called on an object built without `partial = TRUE`.
#'
#' @details
#' Hui (2016) provides an excellent description of the quantities that this function calculates, so this passage
#' is heavily paraphrased from his associated \pkg{boral} package.
#'
#' In latent factor models, the residual covariance matrix is calculated
#' based on the matrix of latent factor loading matrix \eqn{\Theta}, where the residual covariance
#' matrix \eqn{\Sigma = \Theta\Theta'}. A strong residual covariance/correlation matrix
#' between two species can be interpreted as evidence of species interactions (e.g.,
#' facilitation or competition),
#' missing covariates, as well as any additional species correlation not accounted for by shared
#' environmental captured in `formula`.
#'
#' The residual precision matrix (also known as partial correlation matrix, Ovaskainen et al., 2016)
#' is defined as the inverse of the residual correlation matrix. The precision matrix is often used to
#' identify direct or causal relationships between two species e.g., two species can have a zero
#' precision but still be correlated, which can be interpreted as saying that two species are not
#' directly associated, but they are still correlated *through* other species. In other words, they
#' are conditionally independent given the other species. It is important that the precision matrix
#' does not exhibit the exact same properties of the correlation e.g., the diagonal elements are
#' not equal to 1. Nevertheless, relatively larger values of precision may imply stronger
#' direct relationships between two species.
#'
#' In addition to the residual correlation and precision matrices, the median or mean point estimator
#' of trace of the residual covariance matrix is returned,
#' \eqn{\sum\limits_{j=1}^p [\Theta\Theta']_{jj}}. Often used in other areas of multivariate
#' statistics, the trace may be interpreted as the amount of covariation explained by the latent factors.
#' One situation where the trace may be useful is when comparing a pure latent factor model
#' (where no terms are suppled to `formula`) versus a model with latent
#' factors and some additional predictors in `formula` -- the proportional difference in trace
#' between these two models may be interpreted as the proportion of covariation between species explained
#' by the predictors in `formula`. Of course, the trace itself is random due to the MCMC sampling, and so it
#' is not always guaranteed to produce sensible answers.
#' @author Nicholas J Clark
#' @references
#' Francis KC Hui (2016). BORAL - Bayesian ordination and regression analysis of
#' multivariate abundance data in R. Methods in Ecology and Evolution. 7, 744-750.
#' \cr
#' \cr
#' Otso Ovaskainen et al. (2016). Using latent variable models to identify large networks of
#' species-to-species associations at different spatial scales. Methods in Ecology and Evolution,
#' 7, 549-555.
#' @seealso `jsdgam()`, [residual_cor()]
#' @author Nicholas J Clark
#' @name mvgam_residcor-class
NULL

#' Plot residual correlations or precisions from a `mvgam_residcor` object
#'
#' Heatmap of the significant residual correlations or precisions
#' from a Joint Species Distribution (\code{jsdgam}) or dynamic
#' factor (\code{mvgam}) model.
#'
#' @param x A `mvgam_residcor` object returned by
#'   `residual_cor(..., summary = TRUE)`.
#' @param type Character. Which matrix to plot. `"correlation"`
#'   (the default) reads `x$sig_cor`; `"precision"` reads
#'   `x$sig_prec` (the partial correlations, Ovaskainen et al.
#'   2016). Precision panels widen the colour-scale limits from
#'   the correlation default `[-1, 1]` to the entry-magnitude
#'   range of `sig_prec`.
#' @param cluster Logical. When `TRUE`, the matrix is reordered
#'   by an approximate Robinson ordering (Gruvaeus & Wainer
#'   1972; average linkage on the `1 - cormat` distance) so
#'   visually coherent positive and negative clusters sit
#'   adjacent. Defaults to `FALSE`.
#' @param ... Ignored.
#'
#' @details Only the lower triangle of off-diagonal entries is
#'   shown; the diagonal is dropped (it is `1` for any
#'   correlation and undefined for a precision matrix). Cells
#'   whose credible interval contained zero have been set to
#'   zero by `residual_cor()` and read as grey on the diverging
#'   palette.
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
  ...
) {
  type <- match.arg(type)
  checkmate::assert_flag(cluster)
  mat <- if (type == "correlation") {
    x$sig_cor
  } else {
    x$sig_prec %||% stop(insight::format_error(c(
      "The `mvgam_residcor` object does not contain a precision matrix.",
      i = paste0(
        "Pass `compute_precision = TRUE` to `residual_cor()` ",
        "to populate `sig_prec`."
      )
    )))
  }
  if (cluster) {
    idx <- cluster_cormat(mat)
    mat <- mat[idx, idx]
  }
  long <- gather_matrix(mat)
  limits <- if (type == "correlation") {
    c(-1, 1)
  } else {
    rng <- max(abs(range(long$value, na.rm = TRUE, finite = TRUE)))
    c(-rng, rng)
  }
  scale_name <- if (type == "correlation") {
    "Posterior\ncorrelation"
  } else {
    "Posterior\nprecision"
  }
  ggplot2::ggplot(
    data = long,
    mapping = ggplot2::aes(x = Var1, y = Var2, fill = value)
  ) +
    ggplot2::geom_tile(colour = "grey50") +
    mvgam_diverging_scale(name = scale_name, limits = limits) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::scale_x_discrete(
      guide = ggplot2::guide_axis(angle = 45)
    ) +
    mvgam_theme()
}


#' Plot method for an `mvgam_residcor_list`
#'
#' Faceted heatmap panel across the per-group correlation
#' matrices returned by `residual_cor(mod, groups = TRUE)` on a
#' hierarchical VAR or factor fit. Reuses the same
#' `mvgam_diverging_scale()` palette and `mvgam_theme()` as
#' [plot.mvgam_residcor()], with symmetric limits `c(-1, 1)`
#' shared across facets so the eye can compare panels directly.
#' The `_global` reference matrix is included by default; drop it
#' with `include_global = FALSE`.
#'
#' @param x An `mvgam_residcor_list` returned by
#'   `residual_cor(mod, groups = TRUE)`.
#' @param include_global Logical. If `TRUE` (default), include
#'   the `_global` reference correlation as one panel.
#' @param ncol Optional integer. Number of facet columns; defaults
#'   to `ceiling(sqrt(n_panels))`.
#' @param ... Currently unused.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @seealso [residual_cor()], [plot.mvgam_residcor()],
#'   [plot.mvgam_var_matrix_list()].
#' @author Nicholas J Clark
#' @method plot mvgam_residcor_list
#' @export
plot.mvgam_residcor_list <- function(x, include_global = TRUE,
                                      ncol = NULL, ...) {
  checkmate::assert_flag(include_global)
  keep <- if (isTRUE(include_global)) names(x) else
    setdiff(names(x), "_global")
  long <- do.call(rbind, lapply(keep, function(g) {
    df <- gather_matrix(x[[g]]$sig_cor,
                        drop_diag = FALSE, drop_upper = FALSE)
    df$group <- g
    df
  }))
  ncol <- ncol %||% ceiling(sqrt(length(keep)))
  ggplot2::ggplot(long,
                  ggplot2::aes(x = Var1, y = Var2, fill = value)) +
    ggplot2::geom_tile(colour = "grey60") +
    mvgam_diverging_scale(name = "Posterior\ncorrelation",
                          limits = c(-1, 1)) +
    ggplot2::facet_wrap(~ group, ncol = ncol) +
    ggplot2::scale_x_discrete(
      guide = ggplot2::guide_axis(angle = 45)
    ) +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::labs(x = "", y = "") +
    mvgam_theme()
}
