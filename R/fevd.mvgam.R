#' Calculate latent VAR forecast error variance decompositions
#'
#' Compute forecast error variance decompositions from
#' \code{mvgam} models with Vector Autoregressive dynamics
#'
#' @name fevd.mvgam
#'
#'
#' @param h Positive integer. The number of forecast horizons to
#'   decompose.
#'
#' @param summary Logical; return the posterior median and interval of
#'   each decomposition (the default), or the per-draw decompositions
#'   themselves.
#' @inheritParams irf.mvgam
#'
#' @param ... Unused. Anything passed here is refused.
#'
#' @return An object of \code{\link{mvgam_fevd-class}}, which describes
#'   the quantities computed, with references.
#'
#' @author Nicholas J Clark
#'
#' @seealso [irf()] and [stability()] summarise the same VAR dynamics.
#'   The `r mvgam_article("var", "VAR article")` runs `fevd()` on a
#'   plankton community.
#'
#' @references Lutkepohl H (2005). New Introduction to Multiple Time Series
#'   Analysis. Springer, Berlin. \doi{10.1007/978-3-540-27752-1}
#'
#' @examples
#' \dontrun{
#' set.seed(0)
#' simdat <- sim_mvgam(family = gaussian(), n_series = 3L,
#'                     n_timepoints = 120L, trend_model = VAR(),
#'                     prop_trend = 0.95)
#' var_mod <- mvgam(y ~ 1, trend_formula = ~ VAR(p = 1),
#'                  data = simdat$data_train, family = gaussian(),
#'                  chains = 2, silent = 2)
#'
#' # The share of each series' forecast-error variance due to each
#' # shock, over an eight-step horizon
#' fevd_obj <- fevd(var_mod, h = 8L)
#' plot(fevd_obj)
#'
#' # Next: irf() traces each shock through the system over time
#' }
#'
#' @family latent dynamics
#' @export
fevd <- function(object, ...) {
  UseMethod("fevd", object)
}

#' @rdname fevd.mvgam
#' @method fevd mvgam
#' @export
fevd.mvgam <- function(object, h = 10, ndraws = NULL, draw_ids = NULL,
                       summary = TRUE, probs = c(0.025, 0.975),
                       future = FALSE, ...) {
  checkmate::assert_int(h, lower = 1)
  probs <- assert_var_surface_args(ndraws, draw_ids, summary, probs,
                                   future)
  rlang::check_dots_empty()
  require_fitted_model(object, "fevd")
  assert_var_trend(object, surface = "fevd()")
  var_post <- extract_var_posterior(object, ndraws, draw_ids)

  all_fevds <- var_draw_surfaces(
    var_post, function(x) gen_fevd(x, h = h), future
  )
  class(all_fevds) <- "mvgam_fevd"
  if (!summary) {
    return(all_fevds)
  }
  # As in `irf()`, the draws are one K by K matrix per horizon per
  # draw. They are summarised unless `summary = FALSE`.
  as_var_surface_summary(
    summary(all_fevds, probs = probs), "mvgam_fevd_summary",
    process_labels = var_post$labels
  )
}

#### Functions to compute forecast error variance decompositions
# Much of this code is modified from R code generously provided in the vars
# package https://github.com/cran/vars ####
#' Forecast error variance decomposition
#' @noRd
gen_fevd <- function(x, h = 6) {
  K <- x$K
  ynames <- x$labels
  msey <- var_fecov(x, h = h)
  Psi <- var_psi(x, h = h)
  mse <- matrix(NA, nrow = h, ncol = K)
  Omega <- array(0, dim = c(h, K, K))
  # The numerator at each horizon is a cumulative sum of the squared
  # orthogonalised weights, O(h * K^2) per draw.
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
#' Computes the cumulative h-step forecast error covariance
#' \deqn{\Sigma_y(h) = \sum_{k=0}^{h-1} \Phi_k \, \Sigma_u \, \Phi_k'}
#' where \eqn{\Phi_k} are the moving-average weights `var_phi()`
#' returns for a VAR(p) or VARMA, with \eqn{\Phi_0 = I}.
#' \eqn{\Sigma_u} is the innovation covariance (`x$Sigma`). The
#' diagonal of \eqn{\Sigma_y(h)} is the FEVD denominator in
#' `gen_fevd()`. Its numerator comes from `var_psi()`, which absorbs
#' the Cholesky factor of \eqn{\Sigma_u} into the weights. The two
#' then partition each response's variance.
#'
#' @noRd
var_fecov <- function(x, h) {
  Phi <- var_phi(x, h = h)
  Sigma_u <- x$Sigma
  # Each term `Phi_k Sigma_u Phi_k'` once, then a cumulative sum along
  # the horizons, O(h * K^3) per draw
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
