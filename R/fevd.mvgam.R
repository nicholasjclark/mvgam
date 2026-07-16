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
#' @param future \code{Logical}. When `TRUE`, per-draw FEVD
#'   computation runs under whatever
#'   \code{\link[future:plan]{future::plan()}} the caller has set;
#'   see [`irf()`] for details. Requires the \code{future} package
#'   (mvgam Suggests).
#'
#' @param ... ignored
#'
#' @return See \code{\link{mvgam_fevd-class}} for a full description of the quantities that are
#' computed and returned by this function, along with key references.
#'
#' @author Nicholas J Clark
#'
#' @seealso [VAR()], [irf()], [stability()],
#'   \code{\link{mvgam_fevd-class}}, [plot.mvgam_fevd()],
#'   [plot.mvgam_irf()], [plot.mvgam_stability()],
#'   [plot.mvgam_forecast()].
#'   For a worked article that runs `fevd()` end to end on an
#'   annual bird-count VAR, see
#'   [https://nicholasjclark.github.io/mvgam/articles/var.html](https://nicholasjclark.github.io/mvgam/articles/var.html).
#'
#' @references Lütkepohl, H. (2007).
#' New Introduction to Multiple Time Series Analysis. 2nd ed. Springer-Verlag Berlin Heidelberg.
#'
#' @examples
#' \dontrun{
#' # See `?trend_constructors` for a runnable end-to-end VAR
#' # example that exercises `fevd()` alongside `irf()`,
#' # `stability()` and `residual_cor()` on the same fit. The
#' # shape below summarises the API.
#' fevd_obj <- fevd(var_mod, h = 8L)
#' plot(fevd_obj)
#' }
#'
#' @export
fevd <- function(object, ...) {
  UseMethod("fevd", object)
}

#' @rdname fevd.mvgam
#' @method fevd mvgam
#' @export
fevd.mvgam <- function(object, h = 10, future = FALSE, ...) {
  validate_pos_integer(h)
  checkmate::assert_flag(future)
  assert_var_trend(object, surface = "fevd()")
  var_post <- extract_var_posterior(object)

  all_fevds <- mvgam_maybe_future_lapply(
    var_post$ndraws,
    function(draw) {
      x <- list(
        K = var_post$K,
        A = var_post$A[draw, , , drop = TRUE],
        Sigma = var_post$Sigma[draw, , , drop = TRUE],
        p = 1L
      )
      gen_fevd(x, h = h)
    },
    future = future
  )
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
  # Precompute the squared MA weights so the per-horizon numerator
  # is a cumulative sum along the h dimension. The earlier
  # triple-nested scalar loop cost O(h^2 * K^2) per draw; the
  # cumulative-sum form is O(h * K^2).
  psi_sq <- Psi^2
  temp_cum <- matrix(0, K, K)
  for (i in 1:h) {
    mse[i, ] <- diag(msey[,, i])
    temp_cum <- temp_cum + psi_sq[,, i]
    temp <- temp_cum / mse[i, ]
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
  Phi <- var_phi(x, h = h)
  Sigma_u <- x$Sigma
  # Compute each term `Phi_k Sigma_u Phi_k'` exactly once, then
  # take the cumulative sum along the horizon dimension. The
  # earlier form re-summed products 2..i inside a horizon loop,
  # costing O(h^2 * K^3) per draw; this version is O(h * K^3).
  terms <- array(0, dim = c(x$K, x$K, h))
  for (k in seq_len(h)) {
    terms[,, k] <- Phi[,, k] %*% Sigma_u %*% t(Phi[,, k])
  }
  sigma_yh <- array(0, dim = c(x$K, x$K, h))
  sigma_yh[,, 1] <- terms[,, 1]
  if (h > 1) {
    for (i in 2:h) {
      sigma_yh[,, i] <- sigma_yh[,, i - 1] + terms[,, i]
    }
  }
  sigma_yh
}
