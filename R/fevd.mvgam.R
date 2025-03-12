#' Calculate latent VAR forecast error variance decompositions
#'
#' Compute forecast error variance decompositions from
#' \code{mvgam} models with Vector Autoregressive dynamics
#'
#' @name fevd.mvgam
#' @param object \code{list} object of class \code{mvgam} resulting from a call to [mvgam()]
#' that used a Vector Autoregressive latent process model (either as `VAR(cor = FALSE)` or
#' `VAR(cor = TRUE)`)
#' @param h Positive \code{integer} specifying the forecast horizon over which to calculate
#' the IRF
#' @param ... ignored
#' @details A forecast error variance decomposition is useful for quantifying the amount
#' of information each series that in a Vector Autoregression contributes to the forecast
#' distributions of the other series in the autoregression. This function calculates
#' the forecast error variance decomposition using the
#' orthogonalised impulse response coefficient matrices \eqn{\Psi_h}, which can be used to
#' quantify the contribution of series \eqn{j} to the
#' h-step forecast error variance of series \eqn{k}:
#' \deqn{
#' \sigma_k^2(h) = \sum_{j=1}^K(\psi_{kj, 0}^2 + \ldots + \psi_{kj,
#' h-1}^2) \quad
#' }
#' If the orthogonalised impulse reponses \eqn{(\psi_{kj, 0}^2 + \ldots + \psi_{kj, h-1}^2)}
#' are divided by the variance of the forecast error \eqn{\sigma_k^2(h)},
#' this yields an interpretable percentage representing how much of the
#' forecast error variance for \eqn{k} can be explained by an exogenous shock to \eqn{j}.
#' @return An object of class \code{mvgam_fevd} containing the posterior forecast error
#' variance decompositions. This
#' object can be used with the supplied S3 functions \code{plot}
#' @author Nicholas J Clark
#' @references Lütkepohl, H (2006).
#' New Introduction to Multiple Time Series Analysis. Springer, New York.
#' @seealso \code{\link{VAR}}, \code{\link{irf}}, \code{\link{stability}}
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
#' # Calulate forecast error variance decompositions for each series
#' fevds <- fevd(mod, h = 12)
#'
#' # Plot median contributions to forecast error variance
#' plot(fevds)
#'
#' # View a summary of the error variance decompositions
#' summary(fevds)
#' }
#' @export
fevd <- function(object, ...) {
  UseMethod("fevd", object)
}

#' @rdname fevd.mvgam
#' @method fevd mvgam
#' @export
fevd.mvgam <- function(object, h = 10, ...) {
  validate_pos_integer(h)
  trend_model <- attr(object$model_data, "trend_model")
  if (!trend_model %in% c("VAR", "VARcor", "VAR1", "VAR1cor")) {
    stop(
      "Only VAR(1) models currently supported for calculating FEVDs",
      call. = FALSE
    )
  }
  beta_vars <- mcmc_chains(object$model_output, "A")
  sigmas <- mcmc_chains(object$model_output, "Sigma")
  n_series <- object$n_lv

  if (is.null(n_series)) {
    n_series <- nlevels(object$obs_data$series)
  }

  all_fevds <- lapply(seq_len(NROW(beta_vars)), function(draw) {
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
    gen_fevd(x, h = h)
  })
  class(all_fevds) <- "mvgam_fevd"
  return(all_fevds)
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
#' @noRd
var_fecov <- function(x, h) {
  sigma_yh <- array(NA, dim = c(x$K, x$K, h))
  Phi <- var_phi(x, h = h)
  sigma_yh[,, 1] <- Phi[,, 1] %*% t(Phi[,, 1])
  if (h > 1) {
    for (i in 2:h) {
      temp <- matrix(0, nrow = x$K, ncol = x$K)
      for (j in 2:i) {
        temp <- temp + Phi[,, j] %*% t(Phi[,, j])
      }
      sigma_yh[,, i] <- temp + sigma_yh[,, 1]
    }
  }
  return(sigma_yh)
}
