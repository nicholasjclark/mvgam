#' Calculate latent VAR impulse response functions
#'
#' Compute Generalized or Orthogonalized Impulse Response Functions (IRFs) from
#' \code{mvgam} models with Vector Autoregressive dynamics
#'
#' @name irf.mvgam
#' @param object \code{list} object of class \code{mvgam} resulting from a call to [mvgam()]
#' that used a Vector Autoregressive latent process model (either as `VAR(cor = FALSE)` or
#' `VAR(cor = TRUE)`)
#' @param h Positive \code{integer} specifying the forecast horizon over which to calculate
#' the IRF
#' @param cumulative \code{Logical} flag indicating whether the IRF should be cumulative
#' @param orthogonal \code{Logical} flag indicating whether orthogonalized IRFs should be
#' calculated. Note that the order of the variables matters when calculating these
#' @param ... ignored
#' @details Generalized or Orthogonalized Impulse Response Functions can be computed
#' using the posterior estimates of Vector Autoregressive parameters. This function
#' generates a positive "shock" for a target process at time `t = 0` and then
#' calculates how  each of the remaining processes in the latent VAR are expected
#' to respond over the forecast horizon `h`. The function computes IRFs for all
#' processes in the object and returns them in an array that can be plotted using
#' the S3 `plot` function. To inspect community-level metrics of stability using latent
#' VAR processes, you can use the related \code{\link{stability}} function.
#' @return An object of class \code{mvgam_irf} containing the posterior IRFs. This
#' object can be used with the supplied S3 functions \code{plot}
#' @author Nicholas J Clark
#' @references PH Pesaran & Shin Yongcheol (1998).
#' Generalized impulse response analysis in linear multivariate models.
#' Economics Letters 58: 17–29.
#' @seealso \code{\link{VAR}}, \code{\link{plot.mvgam_irf}}, \code{\link{stability}},
#' \code{\link{fevd}}
#' @examples
#' \donttest{
#' # Simulate some time series that follow a latent VAR(1) process
#' simdat <- sim_mvgam(
#'   family = gaussian(),
#'   n_series = 4,
#'   trend_model = VAR(cor = TRUE),
#'   prop_trend = 1
#' )
#' plot_mvgam_series(data = simdat$data_train, series = "all")
#'
#' # Fit a model that uses a latent VAR(1)
#' mod <- mvgam(y ~ -1,
#'   trend_formula = ~1,
#'   trend_model = VAR(cor = TRUE),
#'   family = gaussian(),
#'   data = simdat$data_train,
#'   chains = 2,
#'   silent = 2
#' )
#'
#' # Calulate Generalized IRFs for each series
#' irfs <- irf(mod, h = 12, cumulative = FALSE)
#'
#' # Plot them
#' plot(irfs, series = 1)
#' plot(irfs, series = 2)
#' plot(irfs, series = 3)
#'
#' # Calculate posterior median, upper and lower 90th quantiles
#' # of the impulse responses
#' summary(irfs)
#' }
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
  ...
) {
  validate_pos_integer(h)
  trend_model <- attr(object$model_data, "trend_model")
  if (!trend_model %in% c("VAR", "VARcor", "VAR1", "VAR1cor")) {
    stop(
      "Only VAR(1) models currently supported for calculating IRFs",
      call. = FALSE
    )
  }
  beta_vars <- mcmc_chains(object$model_output, "A")
  sigmas <- mcmc_chains(object$model_output, "Sigma")
  n_series <- object$n_lv

  if (is.null(n_series)) {
    n_series <- nlevels(object$obs_data$series)
  }

  all_irfs <- lapply(seq_len(NROW(beta_vars)), function(draw) {
    # Get necessary VAR parameters into a simple list format
    x <- list(
      K = n_series,
      A = matrix(
        beta_vars[draw, ],
        nrow = n_series,
        ncol = n_series,
        byrow = TRUE
      ),
      Sigma = matrix(
        sigmas[draw, ],
        nrow = n_series,
        ncol = n_series,
        byrow = TRUE
      ),
      p = 1
    )

    # Calculate the IRF
    gen_irf(x, h = h, cumulative = cumulative, orthogonal = orthogonal)
  })
  class(all_irfs) <- "mvgam_irf"
  attr(all_irfs, "irf_type") <- ifelse(
    orthogonal,
    "Orthogonalized",
    "Generalized"
  )
  return(all_irfs)
}

#### Functions to compute Generalized Impulse Response functions
# Much of this code is modified from R code generously provided by Clinton Watkins:
# https://www.clintonwatkins.com/posts/2021-generalised-impulse-response-function-R/ ####

#' Calculate impulse response functions
#' @noRd
gen_irf <- function(x, h = 6, cumulative = TRUE, orthogonal = FALSE) {
  impulse <- paste0("process_", 1:x$K)

  # Create arrays to hold calculations
  IRF_o <- array(
    data = 0,
    dim = c(h, x$K, x$K),
    dimnames = list(NULL, impulse, impulse)
  )
  IRF_g <- array(
    data = 0,
    dim = c(h, x$K, x$K),
    dimnames = list(NULL, impulse, impulse)
  )
  IRF_g1 <- array(data = 0, dim = c(h, x$K, x$K))

  # Estimation of orthogonalised or generalised IRFs
  if (orthogonal) {
    var_ma <- var_psi(x, h)
  } else {
    var_ma <- var_phi(x, h)
  }

  sigma_u <- x$Sigma
  P <- t(chol(sigma_u))
  sig_jj <- diag(sigma_u)

  for (jj in 1:x$K) {
    indx_ <- matrix(0, x$K, 1)
    indx_[jj, 1] <- 1

    for (kk in 1:h) {
      IRF_o[kk, , jj] <- var_ma[,, kk] %*% P %*% indx_ # Peseran-Shin eqn 7 (OIRF)
      IRF_g1[kk, , jj] <- var_ma[,, kk] %*% sigma_u %*% indx_
      IRF_g[kk, , jj] <- sig_jj[jj]^(-0.5) * IRF_g1[kk, , jj] # Peseran-Shin eqn 10 (GIRF)
    }
  }

  if (orthogonal == TRUE) {
    irf <- IRF_o
  } else if (orthogonal == FALSE) {
    irf <- IRF_g
  } else {
    stop("\nError! Orthogonalised or generalised IRF?\n")
  }

  idx <- length(impulse)
  irs <- list()
  for (ii in 1:idx) {
    irs[[ii]] <- matrix(irf[1:(h), impulse, impulse[ii]], nrow = h)
    colnames(irs[[ii]]) <- impulse
    if (cumulative) {
      if (length(impulse) > 1) irs[[ii]] <- apply(irs[[ii]], 2, cumsum)
      if (length(impulse) == 1) {
        tmp <- matrix(cumsum(irs[[ii]]))
        colnames(tmp) <- impulse
        irs[[ii]] <- tmp
      }
    }
  }
  names(irs) <- impulse
  return(irs)
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
