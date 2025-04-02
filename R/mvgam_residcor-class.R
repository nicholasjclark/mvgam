#' `mvgam_residcor` object description
#'
#' A \code{mvgam_residcor} object returned by function [residual_cor()].
#' Run `methods(class = "mvgam_residcor")` to see an overview of available methods.
#' @return Objects of this class are structured as a `list` with the following components:
#'
#'  \item{cor, cor_lower, cor_upper}{A set of \eqn{p \times p} correlation matrices,
#'  containing either the posterior median or mean estimate, plus lower and upper limits
#'  of the corresponding credible intervals supplied to `probs`}
#'  \item{sig_cor}{A \eqn{p \times p} correlation matrix containing only those correlations whose credible
#'  interval does not contain zero. All other correlations are set to zero}
#'  \item{prec, prec_lower, prec_upper}{A set of \eqn{p \times p} precision matrices,
#'  containing either the posterior median or mean estimate, plus lower and upper limits
#'  of the corresponding credible intervals supplied to `probs`}
#'  \item{sig_prec}{A \eqn{p \times p} precision matrix containing only those precisions whose credible
#'  interval does not contain zero. All other precisions are set to zero}
#'   \item{cov}{A \eqn{p \times p} posterior median or mean covariance matrix}
#'   \item{trace}{The median/mean point estimator of the trace (sum of the diagonal elements)
#'   of the residual covariance matrix `cov`}
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
#' @seealso [jsdgam()], [residual_cor()]
#' @author Nicholas J Clark
#' @name mvgam_residcor-class
NULL

#' Plot residual correlations based on latent factors
#'
#' Plot residual correlation estimates from Joint Species Distribution
#' (\code{jsdgam}) or dynamic factor (\code{mvgam}) models
#' @param x \code{list} object of class \code{mvgam_residcor} resulting from a
#' call to `residual_cor(..., summary = TRUE)`
#' @param cluster Logical. Should the variables be re-arranged within the plot
#' to group the correlation matrix into clusters of positive and negative correlations?
#' Defaults to `FALSE`
#' @param ... ignored
#' @method plot mvgam_residcor
#' @details This function plots the significant residual correlations from a
#' \code{mvgam_residcor} object, whereby the posterior mean (if `robust = FALSE`)
#' or posterior median (if `robust = TRUE`) correlations are shown
#' only those correlations whose credible interval does not contain zero. All other
#' correlations are set to zero in the returned plot
#' @return A `ggplot` object
#' @seealso [jsdgam()], [lv_correlations()], [residual_cor()]
#'
#' @author Nicholas J Clark
#'
#' @export
plot.mvgam_residcor = function(x, cluster = FALSE, ...) {
  # Extract significant correlations
  corrmat <- x$sig_cor

  # Re-order into clusters, if specified
  if (cluster) {
    idx <- cluster_cormat(corrmat)
  } else {
    idx <- 1:NROW(corrmat)
  }

  # Plot the correlation matrix
  ggplot2::ggplot(
    data = gather_matrix(corrmat[idx, idx]),
    mapping = ggplot2::aes(
      x = Var1,
      y = Var2,
      fill = correlation
    )
  ) +
    ggplot2::geom_tile(colour = 'grey50') +
    ggplot2::scale_fill_gradient2(breaks = seq(-1, 1, by = 0.5),
                                  limits = c(-1, 1)) +
    ggplot2::labs(x = '', y = '') +
    ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 45)) +
    ggplot2::theme_minimal()
}

#' Melt a symmetric matrix into a long data.frame
#' @noRd
gather_matrix <- function(mat) {
  mat[upper.tri(mat)] <- NA
  if (is.null(dimnames(mat))) {
    grid <- expand.grid(seq.int(NROW(mat)), seq.int(NCOL(mat)))
  } else {
    grid <- expand.grid(dimnames(mat))
  }
  out <- as.data.frame(cbind(grid, value = as.vector(mat)))
  colnames(out) <- c('Var1', 'Var2', 'correlation')
  return(out)
}

#' Order a symmetric correlation matrix using approximate Robinson
#' ordering, for better visualisation of "clusters"
#' Credit for these functions goes to the maintainers of the gclus R package
#' @importFrom stats hclust as.dist
#' @noRd
cluster_cormat <- function(cormat, ...) {
  dis <- -cormat
  dis_d <- as.dist(dis)
  n <- NROW(dis)
  if (n <= 2) {
    idx <- 1:n
  } else {
    clusters <- stats::hclust(dis_d, ...)
    clusters <- reorder_clusters(clusters, dis)
    idx <- clusters$order
  }
  return(idx)
}

#' @noRd
reorder_clusters <- function(x, dis, ...) {
  if (!is.matrix(dis)) dis <- as.matrix(dis)
  merges <- x$merge
  n <- NROW(merges)
  endpoints <- matrix(0, n, 2)
  dir <- matrix(1L, n, 2)
  for (i in 1L:n) {
    j <- merges[i, 1]
    k <- merges[i, 2]
    if ((j < 0) && (k < 0)) {
      endpoints[i, 1] <- -j
      endpoints[i, 2] <- -k
    } else if (j < 0) {
      j <- -j
      endpoints[i, 1] <- j
      e1 <- endpoints[k, 1]
      e2 <- endpoints[k, 2]
      if (dis[j, e1] < dis[j, e2]) {
        endpoints[i, 2] <- e2
      } else {
        endpoints[i, 2] <- e1
        dir[i, 2] <- -1
      }
    } else if (k < 0) {
      k <- -k
      endpoints[i, 2] <- k
      e1 <- endpoints[j, 1]
      e2 <- endpoints[j, 2]
      if (dis[k, e1] < dis[k, e2]) {
        endpoints[i, 1] <- e2
        dir[i, 1] <- -1
      } else {
        endpoints[i, 1] <- e1
      }
    } else {
      ek1 <- endpoints[k, 1]
      ek2 <- endpoints[k, 2]
      ej1 <- endpoints[j, 1]
      ej2 <- endpoints[j, 2]

      d11 <- dis[ej1, ek1]
      d12 <- dis[ej1, ek2]
      d21 <- dis[ej2, ek1]
      d22 <- dis[ej2, ek2]
      dmin <- min(d11, d12, d21, d22)
      if (dmin == d21) {
        endpoints[i, 1] <- ej1
        endpoints[i, 2] <- ek2
      } else if (dmin == d11) {
        endpoints[i, 1] <- ej2
        endpoints[i, 2] <- ek2
        dir[i, 1] <- -1
      } else if (dmin == d12) {
        endpoints[i, 1] <- ej2
        endpoints[i, 2] <- ek1
        dir[i, 1] <- -1
        dir[i, 2] <- -1
      } else {
        endpoints[i, 1] <- ej1
        endpoints[i, 2] <- ek1
        dir[i, 2] <- -1
      }
    }
  }
  for (i in n:2L) {
    if (dir[i, 1] == -1) {
      m <- merges[i, 1]
      if (m > 0) {
        m1 <- merges[m, 1]
        merges[m, 1] <- merges[m, 2]
        merges[m, 2] <- m1
        if (dir[m, 1] == dir[m, 2]) {
          dir[m, ] <- -dir[m, ]
        }
      }
    }
    if (dir[i, 2] == -1) {
      m <- merges[i, 2]
      if (m > 0) {
        m1 <- merges[m, 1]
        merges[m, 1] <- merges[m, 2]
        merges[m, 2] <- m1
        if (dir[m, 1] == dir[m, 2]) {
          dir[m, ] <- -dir[m, ]
        }
      }
    }
  }
  clusters <- as.list(1:n)
  for (i in 1:n) {
    j <- merges[[i, 1]]
    k <- merges[[i, 2]]
    if ((j < 0) && (k < 0)) {
      clusters[[i]] <- c(-j, -k)
    } else if (j < 0) {
      clusters[[i]] <- c(-j, clusters[[k]])
    } else if (k < 0) {
      clusters[[i]] <- c(clusters[[j]], -k)
    } else {
      clusters[[i]] <- c(clusters[[j]], clusters[[k]])
    }
  }

  x1 <- x
  x1$merge <- merges
  x1$order <- clusters[[n]]
  return(x1)
}
