#' Calculate latent VAR impulse response functions
#'
#' Compute Generalized or Orthogonalized Impulse Response Functions (IRFs) from
#' \code{mvgam} models with Vector Autoregressive dynamics. An impulse
#' response traces how the latent trend of every series moves over `h`
#' time steps after a one-standard-deviation innovation to one series.
#' The default generalised responses suit most analyses. Orthogonalised
#' responses need a causal ordering of the series.
#'
#' @name irf.mvgam
#' @inheritParams posterior_epred.mvgam
#' @param object An object of class \code{mvgam} fitted with a `VAR()`
#'   latent process model
#' @param h Positive integer. The number of horizons to compute. Horizon
#'   1 is the time step of the shock.
#' @param summary Logical; return the posterior median and interval of
#'   each response (the default), or the per-draw responses themselves.
#'   The draws are one `K` by `K` matrix per horizon per draw, which on
#'   a wide panel runs to hundreds of megabytes.
#' @param probs The lower and upper percentiles of the interval reported
#'   with the median when `summary = TRUE`.
#' @param cumulative Logical. `TRUE` accumulates each response over the
#'   horizons.
#' @param orthogonal Logical. `TRUE` computes orthogonalised responses
#'   from the lower Cholesky factor of the innovation covariance. These
#'   depend on the order of the levels of the series factor. The
#'   default generalised responses do not (Pesaran and Shin 1998).
#' @param future \code{Logical}. When `TRUE`, the per-draw computation
#'   runs under the \code{\link[future:plan]{future::plan()}} the caller
#'   has set. The default \code{sequential} plan runs as base `lapply()`
#'   does. \code{plan(multisession, workers = N)} splits the draws across
#'   \code{N} R processes. Without the \pkg{future} package the draws
#'   run in turn, with a message.
#' @param ... Unused. Anything passed here is refused.
#' @details
#' See \code{\link{mvgam_irf-class}} for the quantities computed and
#' returned, with references.
#' @return An object of \code{\link{mvgam_irf-class}}. [plot.mvgam_irf()]
#'   and [summary.mvgam_irf()] take it.
#' @author Nicholas J Clark
#' @references Lutkepohl H (2005). New Introduction to Multiple Time Series
#'   Analysis. Springer, Berlin. \doi{10.1007/978-3-540-27752-1}
#'
#'   Pesaran HH and Shin Y (1998). Generalized impulse response analysis
#'   in linear multivariate models. Economics Letters 58, 17-29.
#'   \doi{10.1016/S0165-1765(97)00214-0}
#' @seealso [fevd()] and [stability()] summarise the same VAR dynamics.
#'   The `r mvgam_article("var", "VAR article")` runs `irf()` on a
#'   plankton community. The
#'   `r mvgam_article("hierarchical_var", "hierarchical VAR article")`
#'   runs it on each group of a hierarchical VAR.
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
#' # Generalised responses of every series to a shock in each
#' irf_obj <- irf(var_mod, h = 8L)
#' plot(irf_obj, series = 1)
#'
#' # Next: fevd() splits each series' forecast variance among the
#' # shocks. stability() summarises how the system returns to
#' # equilibrium
#' }
#'
#' @family latent dynamics
#' @export
irf <- function(object, ...) {
  UseMethod("irf", object)
}

#' @rdname irf.mvgam
#' @method irf mvgam
#' @export
irf.mvgam <- function(
  object,
  h = 10,
  cumulative = FALSE,
  orthogonal = FALSE,
  ndraws = NULL,
  draw_ids = NULL,
  summary = TRUE,
  probs = c(0.025, 0.975),
  future = FALSE,
  ...
) {
  checkmate::assert_int(h, lower = 1)
  checkmate::assert_flag(cumulative)
  checkmate::assert_flag(orthogonal)
  probs <- assert_var_surface_args(ndraws, draw_ids, summary, probs,
                                   future)
  rlang::check_dots_empty()
  require_fitted_model(object, "irf")
  assert_var_trend(object, surface = "irf()")
  var_post <- extract_var_posterior(object, ndraws, draw_ids)

  all_irfs <- var_draw_surfaces(
    var_post,
    function(x) {
      gen_irf(x, h = h, cumulative = cumulative,
              orthogonal = orthogonal)
    },
    future
  )
  class(all_irfs) <- "mvgam_irf"
  attr(all_irfs, "irf_type") <- ifelse(
    orthogonal,
    "Orthogonalized",
    "Generalized"
  )
  if (!summary) {
    return(all_irfs)
  }
  # The draws are one K by K matrix per horizon per draw. A wide VAR
  # makes that very large. The response is summarised over draws.
  # `summary = FALSE` returns the draws.
  as_var_surface_summary(
    summary(all_irfs, probs = probs), "mvgam_irf_summary",
    irf_type = attr(all_irfs, "irf_type"),
    process_labels = var_post$labels
  )
}

#### Functions to compute Generalized Impulse Response functions
# Much of this code is modified from R code generously provided by Clinton Watkins:
# https://www.clintonwatkins.com/posts/2021-generalised-impulse-response-function-R/ ####

#' Calculate impulse response functions
#'
#' Orthogonalised (Sims 1980; Lutkepohl 2005, Section 2.3.2) and
#' generalised (Pesaran-Shin 1998) impulse responses. Both share the
#' MA representation of A but project it differently:
#'
#' * Orthogonalised: `Psi_k e_j = (Phi_k P) e_j` where `P` is the
#'   lower Cholesky factor of `Sigma_u`. `var_psi(x, h)` returns the
#'   product `Phi_k P`. Applying the unit vector `e_j` is then a single
#'   column lookup.
#' * Generalised: `sigma_jj^{-1/2} Phi_k Sigma_u e_j`. Uses the raw
#'   MA reps (`var_phi`) and projects via the j-th column of
#'   `Sigma_u` scaled by the inverse-root of `Sigma_u[j, j]`.
#'
#' @noRd
gen_irf <- function(x, h = 6, cumulative = TRUE, orthogonal = FALSE) {
  impulse <- x$labels
  irf_array <- array(
    data = 0,
    dim = c(h, x$K, x$K),
    dimnames = list(NULL, impulse, impulse)
  )

  if (orthogonal) {
    # Psi_k = Phi_k * P. `var_psi()` includes P. The orthogonalised
    # response to shock j is the j-th column of Psi_k.
    Psi <- var_psi(x, h)
    for (jj in 1:x$K) {
      for (kk in 1:h) {
        irf_array[kk, , jj] <- Psi[, jj, kk]
      }
    }
  } else {
    Phi <- var_phi(x, h)
    sigma_u <- x$Sigma
    sig_jj <- diag(sigma_u)
    for (jj in 1:x$K) {
      scale_jj <- sig_jj[jj]^(-0.5)
      for (kk in 1:h) {
        irf_array[kk, , jj] <- scale_jj * (Phi[,, kk] %*% sigma_u[, jj])
      }
    }
  }

  idx <- length(impulse)
  irs <- list()
  for (ii in 1:idx) {
    irs[[ii]] <- matrix(irf_array[1:(h), impulse, impulse[ii]], nrow = h)
    colnames(irs[[ii]]) <- impulse
    if (cumulative) {
      # `apply()` drops a one-row or one-column result to a vector
      irs[[ii]] <- matrix(apply(irs[[ii]], 2L, cumsum), nrow = h,
                          dimnames = list(NULL, impulse))
    }
  }
  names(irs) <- impulse
  irs
}

#' The moving-average weights of a VAR or VARMA
#'
#' \eqn{\Phi_0 = I} and
#' \eqn{\Phi_n = D_n + \sum_{j = 1}^{\min(n, p)} A_j \Phi_{n - j}},
#' with \eqn{D_n = 0} beyond the moving-average order.
#'
#' @param x One draw, a list holding `A` (a `[K, K, p]` array), `D` (a
#'   `[K, K, q]` array, where `q` may be 0) and `K`
#' @param h Horizon
#' @return A `[K, K, h + 1]` array holding \eqn{\Phi_0, \ldots, \Phi_h}
#' @noRd
var_phi <- function(x, h = 10) {
  h <- abs(as.integer(h))
  K <- x$K
  p <- dim(x$A)[3L]
  q <- dim(x$D)[3L] %||% 0L
  Phi <- array(0, dim = c(K, K, h + 1L))
  Phi[,, 1L] <- diag(K)
  for (n in seq_len(h)) {
    acc <- if (n <= q) x$D[,, n] else matrix(0, K, K)
    for (j in seq_len(min(n, p))) {
      acc <- acc + x$A[,, j] %*% Phi[,, n - j + 1L]
    }
    Phi[,, n + 1L] <- acc
  }
  Phi
}

#' Convert a VAR A matrix to its orthogonalised moving average representation
#' @noRd
var_psi <- function(x, h = 10) {
  h <- abs(as.integer(h))
  Phi <- var_phi(x, h = h)
  Psi <- array(0, dim = dim(Phi))
  sigma_u <- x$Sigma
  P <- t(chol(sigma_u))
  dim3 <- dim(Phi)[3]
  for (i in 1:dim3) {
    Psi[,, i] <- Phi[,, i] %*% P
  }
  return(Psi)
}
