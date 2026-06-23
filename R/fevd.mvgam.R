#' Calculate latent VAR forecast error variance decompositions
#'
#' Compute forecast error variance decompositions from
#' \code{mvgam} models with Vector Autoregressive dynamics
#'
#' @name fevd.mvgam
#'
#' @param object \code{list} object of class \code{mvgam} resulting from a call to [mvgam()]
#' that used a Vector Autoregressive latent process model (either as `VAR(cor = FALSE)` or
#' `VAR(cor = TRUE)`; see [VAR()] for details)
#'
#' @param h Positive \code{integer} specifying the forecast horizon over which to calculate
#' the IRF
#'
#' @param ... ignored
#'
#' @return See \code{\link{mvgam_fevd-class}} for a full description of the quantities that are
#' computed and returned by this function, along with key references.
#'
#' @author Nicholas J Clark
#'
#' @seealso [VAR()], [irf()], [stability()], \code{\link{mvgam_fevd-class}}
#'
#' @references Lütkepohl, H. (2007).
#' New Introduction to Multiple Time Series Analysis. 2nd ed. Springer-Verlag Berlin Heidelberg.
#'
#' @examples
#' \donttest{
#' set.seed(0)
#' simdat <- sim_mvgam(family = gaussian(), n_series = 3L,
#'                      n_timepoints = 60L, trend_model = VAR(),
#'                      prop_trend = 0.95)
#' mod <- mvgam(y ~ 1, trend_formula = ~ VAR(p = 1),
#'               data    = simdat$data_train,
#'               family  = gaussian(),
#'               chains  = 2, silent = 2)
#'
#' # Forecast-error variance decomposition over h = 6 steps.
#' fevd(mod, h = 6L)
#' }
#'
#' @export
fevd <- function(object, ...) {
  UseMethod("fevd", object)
}

#' @rdname fevd.mvgam
#' @method fevd mvgam
#' @export
fevd.mvgam <- function(object, h = 10, ...) {
  validate_pos_integer(h)
  assert_var_trend(object, surface = "fevd()")
  var_post <- extract_var_posterior(object)

  all_fevds <- lapply(seq_len(var_post$ndraws), function(draw) {
    x <- list(
      K = var_post$K,
      A = var_post$A[draw, , , drop = TRUE],
      Sigma = var_post$Sigma[draw, , , drop = TRUE],
      p = 1L
    )
    gen_fevd(x, h = h)
  })
  class(all_fevds) <- "mvgam_fevd"
  all_fevds
}

#### Functions to compute forecast error variance decompositions
# Much of this code is modified from R code generously provided in the vars
# package https://github.com/cran/vars ####
#' Forecast error variance decomposition
#' @noRd
gen_fevd <- function(x, h = 6, ...) {
  K <- x$K
  ynames <- paste0("process_", 1:K)
  msey <- var_fecov(x, h = h)
  Psi <- var_psi(x, h = h)
  mse <- matrix(NA, nrow = h, ncol = K)
  Omega <- array(0, dim = c(h, K, K))
  for (i in 1:h) {
    mse[i, ] <- diag(msey[,, i])
    temp <- matrix(0, K, K)
    for (l in 1:K) {
      for (m in 1:K) {
        for (j in 1:i) {
          temp[l, m] <- temp[l, m] + Psi[l, m, j]^2
        }
      }
    }
    temp <- temp / mse[i, ]
    for (j in 1:K) {
      Omega[i, , j] <- temp[j, ]
    }
  }
  result <- list()
  for (i in 1:K) {
    result[[i]] <- matrix(Omega[,, i], nrow = h, ncol = K)
    colnames(result[[i]]) <- ynames
  }
  names(result) <- ynames
  return(result)
}


#' Forecast error covariance matrix
#'
#' Computes the cumulative h-step forecast error covariance for a
#' VAR(1) as
#' \deqn{\Sigma_y(h) = \sum_{k=0}^{h-1} \Phi_k \, \Sigma_u \, \Phi_k'}
#' where \eqn{\Phi_0 = I} and \eqn{\Phi_k = A^k} are the MA
#' coefficients returned by `var_phi` and \eqn{\Sigma_u} is the
#' innovation covariance (`x$Sigma`). The diagonal of
#' \eqn{\Sigma_y(h)} is the FEVD denominator used in `gen_fevd`;
#' the matching numerator is built from `var_psi`, which absorbs
#' the Cholesky factor of \eqn{\Sigma_u} into the MA representation
#' so the two halves partition each response's variance.
#'
#' @noRd
var_fecov <- function(x, h) {
  sigma_yh <- array(NA, dim = c(x$K, x$K, h))
  Phi <- var_phi(x, h = h)
  Sigma_u <- x$Sigma
  sigma_yh[,, 1] <- Phi[,, 1] %*% Sigma_u %*% t(Phi[,, 1])
  if (h > 1) {
    for (i in 2:h) {
      temp <- matrix(0, nrow = x$K, ncol = x$K)
      for (j in 2:i) {
        temp <- temp + Phi[,, j] %*% Sigma_u %*% t(Phi[,, j])
      }
      sigma_yh[,, i] <- temp + sigma_yh[,, 1]
    }
  }
  return(sigma_yh)
}
