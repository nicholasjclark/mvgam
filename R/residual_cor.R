#' Extract residual correlations based on latent factors from a fitted jsdgam
#'
#' Compute residual correlation estimates from Joint Species Distribution
#' \code{jsdgam} models using latent factor loadings
#'
#' @name residual_cor.jsdgam
#' @inheritParams brms::residuals.brmsfit
#' @param object \code{list} object of class \code{mvgam} resulting from a call to [jsdgam()]
#' @param robust If `FALSE` (the default) the mean is used as a measure of central tendency.
#' If `TRUE`, the median is used instead. Only used if `summary` is `TRUE`
#' @param ... ignored
#' @return If `summary = TRUE`, a `list` of class `mvgam_residcor` with the following components:
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
#'   If `summary = FALSE`, this function returns a `list` containing the following components:
#'  \item{all_cormat}{A \eqn{n_{draws} \times p \times p} `array` of posterior
#'  residual correlation matrix draws}
#'  \item{all_covmat}{A \eqn{n_{draws} \times p \times p} `array` of posterior
#'  residual covariance matrix draws}
#'  \item{all_presmat}{A \eqn{n_{draws} \times p \times p} `array` of posterior
#'  residual precision matrix draws}
#'  \item{all_trace}{A \eqn{n_{draws}} `vector` of posterior covariance trace draws}
#'
#' @details
#' Hui (2016) provides an excellent description of the quantities that this function calculates, so this passage
#' is heavily paraphrased from his associated \pkg{boral} package.
#'
#' In Joint Species Distribution Models, the residual covariance matrix is calculated
#' based on the matrix of latent factor loading matrix \eqn{\Theta}, where the residual covariance
#' matrix \eqn{\Sigma = \Theta\Theta'}. A strong residual covariance/correlation matrix
#' between two species can be interpreted as evidence of species interaction (e.g.,
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
#' @seealso [jsdgam()]
#' @export
residual_cor <- function(object, ...) {
  UseMethod("residual_cor", object)
}

#' @rdname residual_cor.jsdgam
#' @method residual_cor jsdgam
#' @export
residual_cor.jsdgam <- function(
  object,
  summary = TRUE,
  robust = FALSE,
  probs = c(0.025, 0.975),
  ...
) {
  insight::check_if_installed("corpcor")

  # Take draws of factor loadings
  n_lv <- object$n_lv
  p <- NCOL(object$ytimes)
  sp_names <- levels(object$obs_data$series)
  loadings <- as.matrix(object$model_output, 'lv_coefs')

  # Initiate array to store all posterior correlation and covariance matrices
  all_cormat <- all_covmat <- all_precmat <- array(
    0,
    dim = c(nrow(loadings), p, p)
  )
  all_trace_rescor <- numeric(NROW(loadings))

  # Calculate posterior covariance, correlation, precision and trace estimates
  for (i in 1:NROW(loadings)) {
    lv_coefs <- matrix(loadings[i, ], nrow = p, ncol = n_lv)

    lambdalambdaT <- tcrossprod(lv_coefs)
    all_covmat[i, , ] <- lambdalambdaT
    all_trace_rescor[i] <- sum(diag(lambdalambdaT))
    all_cormat[i, , ] <- cov2cor(lambdalambdaT)
    all_precmat[i, , ] <- corpcor::cor2pcor(lambdalambdaT)
  }

  if (!summary) {
    out <- list(
      all_cormat = all_cormat,
      all_covmat = all_covmat,
      all_precmat = all_precmat,
      all_trace = all_trace_rescor
    )
  } else {
    #### If summary, calculate summary statistics ####
    # Initiate summary correlation and covariance matrices
    sig_cormat <- cormat <- cormat_lower <- cormat_upper <-
      sig_precmat <- precmat <- precmat_lower <- precmat_upper <-
        covmat <- matrix(0, nrow = p, ncol = p)

    rownames(cormat) <- rownames(cormat_lower) <- rownames(cormat_upper) <-
      rownames(sig_cormat) <- rownames(precmat) <- rownames(precmat_lower) <-
        rownames(precmat_upper) <- rownames(sig_precmat) <- rownames(covmat) <-
          colnames(cormat) <- colnames(cormat_lower) <- colnames(
            cormat_upper
          ) <-
            colnames(sig_cormat) <- colnames(precmat) <- colnames(
              precmat_lower
            ) <-
              colnames(precmat_upper) <- colnames(sig_precmat) <- colnames(
                covmat
              ) <-
                sp_names

    # Calculate posterior summaries
    for (j in 1:p) {
      for (j2 in 1:p) {
        if (robust) {
          covmat[j, j2] <- median(all_covmat[, j, j2])
          cormat[j, j2] <- median(all_cormat[, j, j2])
          precmat[j, j2] <- median(all_precmat[, j, j2])
        } else {
          covmat[j, j2] <- mean(all_covmat[, j, j2])
          cormat[j, j2] <- mean(all_cormat[, j, j2])
          precmat[j, j2] <- mean(all_precmat[, j, j2])
        }

        sig_cormat[j, j2] <- cormat[j, j2]
        cormat_lower[j, j2] <- quantile(
          all_cormat[, j, j2],
          probs = min(probs),
          na.rm = TRUE
        )
        cormat_upper[j, j2] <- quantile(
          all_cormat[, j, j2],
          probs = max(probs),
          na.rm = TRUE
        )
        if (0 > cormat_lower[j, j2] & 0 < cormat_upper[j, j2]) {
          sig_cormat[j, j2] <- 0
        }

        sig_precmat[j, j2] <- precmat[j, j2]
        precmat_lower[j, j2] <- quantile(
          all_precmat[, j, j2],
          probs = min(probs),
          na.rm = TRUE
        )
        precmat_upper[j, j2] <- quantile(
          all_precmat[, j, j2],
          probs = max(probs),
          na.rm = TRUE
        )
        if (0 > precmat_lower[j, j2] & 0 < precmat_upper[j, j2]) {
          sig_precmat[j, j2] <- 0
        }
      }
    }

    if (robust) {
      final_trace <- median(all_trace_rescor)
    } else {
      final_trace <- mean(all_trace_rescor)
    }

    out <- structure(
      list(
        cor = cormat,
        cor_lower = cormat_lower,
        cor_upper = cormat_upper,
        sig_cor = sig_cormat,
        cov = covmat,
        prec = precmat,
        prec_lower = precmat_lower,
        prec_upper = precmat_upper,
        sig_prec = sig_precmat,
        trace = final_trace
      ),
      class = 'mvgam_residcor'
    )
  }
  return(out)
}

#' Plot residual correlations based on latent factors from a fitted jsdgam
#'
#' Plot residual correlation estimates from Joint Species Distribution
#' \code{jsdgam} models
#' @param x \code{list} object of class \code{mvgam_residcor} resulting from a
#' call to `residual_cor(..., summary = TRUE)`
#' @param ... ignored
#' @method plot mvgam_residcor
#' @details This function plots the significant residual correlations from a
#' \code{mvgam_residcor} object
#' @return A `ggplot` object
#' @seealso \code{\link{jsdgam}}, \code{\link{residual_cor}}
#'
#' @author Nicholas J Clark
#'
#' @export
plot.mvgam_residcor = function(x, ...) {
  # Plot the significant correlations
  ggplot2::ggplot(
    data = gather_matrix(x$sig_cor),
    mapping = ggplot2::aes(x = Var1, y = Var2, fill = correlation)
  ) +
    ggplot2::geom_tile(colour = 'grey50') +
    ggplot2::scale_fill_gradient2() +
    ggplot2::labs(x = '', y = '')
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
