#' Extract residual correlations based on latent factors
#'
#' Compute residual correlation estimates from Joint Species Distribution
#' (\code{jsdgam}) or \code{mvgam} models that either used latent factors
#' or included correlated process errors directly
#'
#' @name residual_cor.jsdgam
#' @inheritParams brms::residuals.brmsfit
#' @param object \code{list} object of class \code{mvgam} resulting from a call to [jsdgam()]
#' or a call to [mvgam()] in which either `use_lv = TRUE` or a multivariate process was used
#' with `cor = TRUE` (see [RW()] and [VAR()] for examples)
#' @param robust If `FALSE` (the default) the mean is used as a measure of central tendency.
#' If `TRUE`, the median is used instead. Only used if `summary` is `TRUE`
#' @param ... ignored
#' @return If `summary = TRUE`, a `list` of  \code{\link{mvgam_residcor-class}} with the following components:
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
#' See [mvgam_residcor] for a full description of the quantities that are
#' computed and returned by this function, along with key references.
#'
#' @seealso [jsdgam()], [lv_correlations()], \code{\link{mvgam_residcor-class}}
#' @export
residual_cor <- function(object, ...) {
  UseMethod("residual_cor", object)
}

#' @rdname residual_cor.jsdgam
#' @method residual_cor mvgam
#' @export
residual_cor.mvgam <- function(
  object,
  summary = TRUE,
  robust = FALSE,
  probs = c(0.025, 0.975),
  ...
) {
  # Only applicable if this is a dynamic factor model or a model
  # that included a process error variance-covariance matrix
  if (
    any(
      grepl(
        'Sigma',
        variables(object)$trend_pars$orig_name
      )
    ) |
      object$use_lv
  ) {
    class(object) <- c('jsdgam', 'mvgam')
    return(
      residual_cor(
        object,
        object = object,
        summary = summary,
        robust = robust,
        probs = probs,
        ...
      )
    )
  } else {
    stop(
      paste0(
        'Cannot compute residual correlations if no latent factors ',
        'or correlated process errors were modelled'
      ),
      call. = FALSE
    )
  }
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

  if (length(probs) != 2L) {
    stop("argument 'probs' must be a vector of length 2", call. = FALSE)
  }
  validate_proportional(min(probs))
  validate_proportional(max(probs))

  # Initiate objects to store all posterior correlation and covariance matrices
  p <- NCOL(object$ytimes)
  sp_names <- levels(object$obs_data$series)
  ndraws <- brms::ndraws(as_draws_array(object, variable = 'betas'))
  all_cormat <- all_covmat <- all_precmat <- array(
    0,
    dim = c(ndraws, p, p)
  )
  all_trace_rescor <- numeric(ndraws)

  # Check whether this model included a full variance-covariance matrix
  use_lv <- TRUE
  if (
    any(
      grepl(
        'Sigma',
        variables(object)$trend_pars$orig_name
      )
    )
  ) {
    # Use the factors if they were supplied; otherwise
    # use the full variance-covariance matrix
    if (object$use_lv) {
      use_lv <- TRUE
    } else {
      use_lv <- FALSE
    }
  }

  if (use_lv) {
    # Take draws of factor loadings to compute residual correlations,
    # covariances, and precisions
    n_lv <- object$n_lv
    loadings <- as.matrix(object$model_output, 'lv_coefs')

    # Calculate posterior covariance, correlation, precision and trace estimates
    for (i in 1:ndraws) {
      lv_coefs <- matrix(loadings[i, ], nrow = p, ncol = n_lv)

      lambdalambdaT <- tcrossprod(lv_coefs)
      all_covmat[i, , ] <- lambdalambdaT
      all_trace_rescor[i] <- sum(diag(lambdalambdaT))
      all_cormat[i, , ] <- cov2cor(lambdalambdaT)
      all_precmat[i, , ] <- corpcor::cor2pcor(lambdalambdaT)
    }
  } else {
    # If the model already included a variance-covariance matrix,
    # compute directly
    Sigma_post <- as.matrix(
      object,
      variable = "Sigma",
      regex = TRUE
    )

    for (i in 1:ndraws) {
      cov <- matrix(
        Sigma_post[i, ],
        nrow = p,
        ncol = p
      )
      all_covmat[i, , ] <- cov
      all_trace_rescor[i] <- sum(diag(cov))
      all_cormat[i, , ] <- cov2cor(cov)
      all_precmat[i, , ] <- corpcor::cor2pcor(cov)
    }
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
