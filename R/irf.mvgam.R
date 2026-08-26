#' Calculate latent VAR impulse response functions
#'
#' Compute Generalized or Orthogonalized Impulse Response Functions (IRFs) from
#' \code{mvgam} models with Vector Autoregressive dynamics
#'
#' @name irf.mvgam
#' @param object \code{list} object of class \code{mvgam} resulting from a call to [mvgam()]
#' that used a Vector Autoregressive latent process model (either as `VAR(cor = FALSE)` or
#' `VAR(cor = TRUE)`; see [VAR()] for details)
#' @param h Positive \code{integer} specifying the forecast horizon over which to calculate
#' the IRF
#' @param ndraws Optional integer; the number of posterior draws to
#'   summarise over. A response is built from one transition matrix per
#'   draw, so a wide panel is worth answering from a subset. Default
#'   `NULL` uses every draw.
#' @param draw_ids Optional integer vector naming the posterior draws
#'   to use, in place of `ndraws`.
#' @param summary Logical; return the posterior median and interval of
#'   each response (the default), or the per-draw responses themselves.
#'   The draws are one `K` by `K` matrix per horizon per draw, which on
#'   a wide panel runs to hundreds of megabytes.
#' @param probs The lower and upper percentiles to report alongside the
#'   median when `summary = TRUE`.
#' @param cumulative \code{Logical} flag indicating whether the IRF should be cumulative
#' @param orthogonal \code{Logical} flag indicating whether orthogonalized IRFs should be
#' calculated. Note that the order of the variables matters when calculating these
#' @param future \code{Logical}. When `TRUE`, per-draw IRF
#'   computation runs under whatever
#'   \code{\link[future:plan]{future::plan()}} the caller has set
#'   (default \code{sequential} behaves like base `lapply` with
#'   negligible overhead; \code{plan(multisession, workers = N)}
#'   splits draws across \code{N} R processes). Requires the
#'   \code{future} package (mvgam Suggests).
#' @param ... ignored
#' @details
#' See \code{\link{mvgam_irf-class}} for a full description of the quantities that are
#' computed and returned by this function, along with key references.
#' @return An object of \code{\link{mvgam_irf-class}} containing the posterior IRFs. This
#' object can be used with the supplied S3 functions [plot.mvgam_irf()]
#' and [summary.mvgam_irf()]
#' @author Nicholas J Clark
#' @seealso \code{\link{mvgam_irf-class}}, [VAR()],
#'   [plot.mvgam_irf()], [stability()], [fevd()],
#'   [plot.mvgam_stability()], [plot.mvgam_fevd()],
#'   [plot.mvgam_forecast()].
#'   For a worked article that runs `irf()` end to end on an
#'   annual bird-count VAR, see
#'   [https://nicholasjclark.github.io/mvgam/articles/var.html](https://nicholasjclark.github.io/mvgam/articles/var.html).
#'
#' @examples
#' \dontrun{
#' # See `?trend_constructors` for a runnable end-to-end VAR
#' # example that exercises `irf()` alongside `fevd()`,
#' # `stability()` and `residual_cor()` on the same fit. The
#' # shape below summarises the API.
#' irf_obj <- irf(var_mod, h = 8L)
#' plot(irf_obj, series = 1)
#' }
#'
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
  validate_pos_integer(h)
  checkmate::assert_logical(cumulative, len = 1L)
  checkmate::assert_logical(orthogonal, len = 1L)
  checkmate::assert_int(ndraws, lower = 1L, null.ok = TRUE)
  checkmate::assert_integerish(draw_ids, lower = 1L, null.ok = TRUE)
  checkmate::assert_flag(summary)
  checkmate::assert_flag(future)
  assert_var_trend(object, surface = "irf()")
  var_post <- extract_var_posterior(object, ndraws, draw_ids)

  all_irfs <- mvgam_maybe_future_lapply(
    var_post$ndraws,
    function(draw) {
      x <- list(
        K = var_post$K,
        A = var_post$A[draw, , , drop = TRUE],
        Sigma = var_post$Sigma[draw, , , drop = TRUE],
        p = 1L
      )
      gen_irf(x, h = h, cumulative = cumulative, orthogonal = orthogonal)
    },
    future = future
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
  # A response is summarised over draws before it is returned, because
  # the draws themselves are one K by K matrix per horizon per draw and
  # a wide VAR makes that far larger than anything a reader wants in
  # hand. `summary = FALSE` keeps them for anyone who does.
  as_var_surface_summary(
    summary(all_irfs, probs = probs), "mvgam_irf_summary",
    irf_type = attr(all_irfs, "irf_type")
  )
}

#### Functions to compute Generalized Impulse Response functions
# Much of this code is modified from R code generously provided by Clinton Watkins:
# https://www.clintonwatkins.com/posts/2021-generalised-impulse-response-function-R/ ####

#' Calculate impulse response functions
#'
#' Orthogonalised (Sims 1980 / Lutkepohl 2007 §2.3.2) and
#' generalised (Pesaran-Shin 1998) impulse responses. Both share the
#' MA representation of A but project it differently:
#'
#' * Orthogonalised: `Psi_k e_j = (Phi_k P) e_j` where `P` is the
#'   lower Cholesky factor of `Sigma_u`. `var_psi(x, h)` returns
#'   `Phi_k P` pre-multiplied, so applying `e_j` (a unit vector) is
#'   a single column lookup; no further Cholesky-side multiplication.
#' * Generalised: `sigma_jj^{-1/2} Phi_k Sigma_u e_j`. Uses the raw
#'   MA reps (`var_phi`) and projects via the j-th column of
#'   `Sigma_u` scaled by the inverse-root of `Sigma_u[j, j]`.
#'
#' @noRd
gen_irf <- function(x, h = 6, cumulative = TRUE, orthogonal = FALSE) {
  impulse <- paste0("process_", 1:x$K)
  irf_array <- array(
    data = 0,
    dim = c(h, x$K, x$K),
    dimnames = list(NULL, impulse, impulse)
  )

  if (orthogonal) {
    # Psi_k = Phi_k * P; `var_psi` already absorbs P, so the OIRF
    # for shock j is just the j-th column of Psi_k.
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
      if (length(impulse) > 1) {
        irs[[ii]] <- apply(irs[[ii]], 2, cumsum)
      }
      if (length(impulse) == 1) {
        tmp <- matrix(cumsum(irs[[ii]]))
        colnames(tmp) <- impulse
        irs[[ii]] <- tmp
      }
    }
  }
  names(irs) <- impulse
  irs
}

#' Convert a VAR A matrix to its moving average representation
#' @noRd
var_phi <- function(x, h = 10) {
  h <- abs(as.integer(h))
  K <- x$K
  p <- x$p
  A <- as.array(x$A)
  if (h >= p) {
    As <- array(0, dim = c(K, K, h + 1))
    for (i in (p + 1):(h + 1)) {
      As[,, i] <- matrix(0, nrow = K, ncol = K)
    }
  } else {
    As <- array(0, dim = c(K, K, p))
  }
  As[,, 1] <- A
  Phi <- array(0, dim = c(K, K, h + 1))
  Phi[,, 1] <- diag(K)
  Phi[,, 2] <- Phi[,, 1] %*% As[,, 1]
  if (h > 1) {
    for (i in 3:(h + 1)) {
      tmp1 <- Phi[,, 1] %*% As[,, i - 1]
      tmp2 <- matrix(0, nrow = K, ncol = K)
      idx <- (i - 2):1
      for (j in 1:(i - 2)) {
        tmp2 <- tmp2 + Phi[,, j + 1] %*% As[,, idx[j]]
      }
      Phi[,, i] <- tmp1 + tmp2
    }
  }
  return(Phi)
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
