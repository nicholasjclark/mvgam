#' Calculate measures of latent VAR community stability
#'
#' Compute reactivity, return rates and contributions of interactions to
#' stationary forecast variance from
#' \pkg{mvgam} models with Vector Autoregressive dynamics
#'
#'@name stability.mvgam
#'@param object \code{list} object of class \code{mvgam} resulting from a call to [mvgam()]
#'that used a Vector Autoregressive latent process model (either as `VAR(cor = FALSE)` or
#'`VAR(cor = TRUE)`)
#'@param ... ignored
#'@details These measures of stability can be used to assess how important inter-series
#'dependencies are to the variability of a multivariate system and to ask how systems
#'are expected to respond to environmental perturbations. Using the formula for a latent VAR(1) as:
#'\deqn{
#'\mu_t \sim \text{MVNormal}(A(\mu_{t - 1}), \Sigma) \quad
#'}
#'this function will calculate the long-term stationary forecast distribution of the system, which
#'has mean \eqn{\mu_{\infty}} and variance \eqn{\Sigma_{\infty}}, to then calculate the following quantities:
#'\itemize{
#'    \item `prop_int`: Proportion of the volume of the stationary forecast distribution
#'    that is attributable to lagged interactions (i.e. how important are the autoregressive
#'    interaction coefficients in \eqn{A} for explaining the shape of the stationary forecast distribution?):
#'     \deqn{
#'     det(A)^2 \quad
#'     }
#'    \item `prop_int_adj`: Same as `prop_int` but scaled by the number of series \eqn{p} to facilitate
#'    direct comparisons among systems with different numbers of interacting variables:
#'     \deqn{
#'     det(A)^{2/p} \quad
#'     }
#'    \item `prop_int_offdiag`: Sensitivity of `prop_int` to inter-series
#'    interactions (i.e. how important are the off-diagonals of the autoregressive coefficient
#'    matrix \eqn{A} for shaping `prop_int`?), calculated as the relative magnitude of the *off-diagonals* in
#'    the partial derivative matrix:
#'     \deqn{
#'     [2~det(A) (A^{-1})^T] \quad
#'     }
#'    \item `prop_int_diag`: Sensitivity of `prop_int` to intra-series
#'    interactions (i.e. how important are the diagonals of the autoregressive coefficient matrix \eqn{A}
#'    for shaping `prop_int`?), calculated as the relative magnitude of the *diagonals* in the partial derivative
#'    matrix:
#'     \deqn{
#'     [2~det(A) (A^{-1})^T] \quad
#'     }
#'   \item `prop_cov_offdiag`: Sensitivity of \eqn{\Sigma_{\infty}} to inter-series error correlations
#'    (i.e. how important are off-diagonal covariances in \eqn{\Sigma} for shaping
#'    \eqn{\Sigma_{\infty}}?), calculated as the relative magnitude of the *off-diagonals* in
#'    the partial derivative matrix:
#'     \deqn{
#'     [2~det(\Sigma_{\infty}) (\Sigma_{\infty}^{-1})^T] \quad
#'     }
#'   \item `prop_cov_diag`: Sensitivity of \eqn{\Sigma_{\infty}} to error variances
#'    (i.e. how important are diagonal variances in \eqn{\Sigma} for shaping
#'    \eqn{\Sigma_{\infty}}?), calculated as the relative magnitude of the *diagonals* in
#'    the partial derivative matrix:
#'     \deqn{
#'     [2~det(\Sigma_{\infty}) (\Sigma_{\infty}^{-1})^T] \quad
#'     }
#'    \item `reactivity`: A measure of the degree to which the system moves
#'    away from a stable equilibrium following a perturbation.
#'    Values `> 0` suggest the system is reactive, whereby a
#'    perturbation of the system in one period can be amplified in the next period. If
#'    \eqn{\sigma_{max}(A)} is the largest singular value of \eqn{A}, then reactivity is defined as:
#'     \deqn{
#'     log\sigma_{max}(A) \quad
#'     }
#'    \item `mean_return_rate`: Asymptotic (long-term) return rate of the mean of the transition distribution
#'    to the stationary mean, calculated using the largest eigenvalue of the matrix \eqn{A}:
#'    \deqn{
#'    max(\lambda_{A}) \quad
#'    }
#'    Lower values suggest greater stability
#'    \item `var_return_rate`: Asymptotic (long-term) return rate of the variance of the transition distribution
#'    to the stationary variance:
#'    \deqn{
#'    max(\lambda_{A \otimes{A}}) \quad
#'    }
#'    Again, lower values suggest greater stability
#'   }
#' Major advantages of using \pkg{mvgam} to compute these metrics are that well-calibrated uncertainties are
#' available and that VAR processes are forced to be stationary. These properties make it simple and
#' insightful to calculate and inspect aspects of both long-term and short-term stability.
#' But it is also possible to more directly inspect possible interactions among the
#' time series in a latent VAR process. To do so, you can calculate
#' Generalized or Orthogonalized Impulse Response Functions using the \code{\link{irf}} function,
#' or you can calculate Forecast Error Variance Decompositions using the \code{\link{fevd}} function.
#'@return A \code{data.frame} containing posterior draws for each stability metric.
#'@references AR Ives, B Dennis, KL Cottingham & SR Carpenter (2003).
#'Estimating community stability and ecological interactions from time-series data.
#'Ecological Monographs. 73, 301-330.
#'@author Nicholas J Clark
#'@seealso \code{\link{VAR}}, \code{\link{irf}}, \code{\link{fevd}}
#' @examples
#' \donttest{
#' # Simulate some time series that follow a latent VAR(1) process
#' simdat <- sim_mvgam(family = gaussian(),
#'                     n_series = 4,
#'                     trend_model = VAR(cor = TRUE),
#'                     prop_trend = 1)
#' plot_mvgam_series(data = simdat$data_train, series = 'all')
#'
#' # Fit a model that uses a latent VAR(1)
#' mod <- mvgam(y ~ -1,
#'              trend_formula = ~ 1,
#'              trend_model = VAR(cor = TRUE),
#'              family = gaussian(),
#'              data = simdat$data_train,
#'              chains = 2,
#'              silent = 2)
#'
#' # Calulate stability metrics for this system
#' metrics <- stability(mod)
#'
#' # Proportion of stationary forecast distribution
#' # attributable to lagged interactions
#' hist(metrics$prop_int,
#'      xlim = c(0, 1),
#'      xlab = 'Prop_int',
#'      main = '',
#'      col = '#B97C7C',
#'      border = 'white')
#'
#' # Within this contribution of interactions, how important
#' # are inter-series interactions (offdiagonals of the A matrix) vs
#' # intra-series density dependence (diagonals of the A matrix)?
#' layout(matrix(1:2, nrow = 2))
#' hist(metrics$prop_int_offdiag,
#'      xlim = c(0, 1),
#'      xlab = '',
#'      main = 'Inter-series interactions',
#'      col = '#B97C7C',
#'      border = 'white')
#'
#' hist(metrics$prop_int_diag,
#'      xlim = c(0, 1),
#'      xlab = 'Contribution to interaction effect',
#'      main = 'Intra-series interactions (density dependence)',
#'      col = 'darkblue',
#'      border = 'white')
#' layout(1)
#'
#' # How important are inter-series error covariances
#' # (offdiagonals of the Sigma matrix) vs
#' # intra-series variances (diagonals of the Sigma matrix) for explaining
#' # the variance of the stationary forecast distribution?
#' layout(matrix(1:2, nrow = 2))
#' hist(metrics$prop_cov_offdiag,
#'      xlim = c(0, 1),
#'      xlab = '',
#'      main = 'Inter-series covariances',
#'      col = '#B97C7C',
#'      border = 'white')
#'
#' hist(metrics$prop_cov_diag,
#'      xlim = c(0, 1),
#'      xlab = 'Contribution to forecast variance',
#'      main = 'Intra-series variances',
#'      col = 'darkblue',
#'      border = 'white')
#' layout(1)
#'
#' # Reactivity, i.e. degree to which the system moves
#' # away from a stable equilibrium following a perturbation
#' # (values > 1 suggest a more reactive, less stable system)
#' hist(metrics$reactivity,
#'      main = '',
#'      xlab = 'Reactivity',
#'      col = '#B97C7C',
#'      border = 'white',
#'      xlim = c(-1*max(abs(metrics$reactivity)),
#'               max(abs(metrics$reactivity))))
#' abline(v = 0, lwd = 2.5)
#' }
#'@export
stability <- function(object, ...) {
  UseMethod("stability", object)
}

#'@rdname stability.mvgam
#'@method stability mvgam
#'@export
stability.mvgam = function(object, ...) {
  # Check trend_model
  trend_model <- attr(object$model_data, 'trend_model')
  if (!trend_model %in% c('VAR', 'VARcor', 'VAR1', 'VAR1cor')) {
    stop(
      'Only VAR(1) models currently supported for calculating stability metrics',
      call. = FALSE
    )
  }

  # Take posterior draws of the interaction matrix
  B_post <- mcmc_chains(object$model_output, 'A')

  # Take posterior draws of Sigma
  Sigma_post <- mcmc_chains(object$model_output, 'Sigma')

  # Number of series in the VAR process
  n_series <- object$n_lv

  if (is.null(n_series)) {
    n_series <- nlevels(object$obs_data$series)
  }

  metrics <- do.call(
    rbind,
    lapply(seq_len(NROW(B_post)), function(i) {
      B <- matrix(B_post[i, ], nrow = n_series, ncol = n_series, byrow = TRUE)
      p <- dim(B)[1]

      # If we want to get the variance of the stationary distribution (Sigma_inf)
      Sigma <- matrix(
        Sigma_post[i, ],
        nrow = n_series,
        ncol = n_series,
        byrow = TRUE
      )
      vecS_inf <- solve(diag(p * p) - kronecker(B, B)) %*% as.vector(Sigma)
      Sigma_inf <- matrix(vecS_inf, nrow = p)

      # The difference in volume between Sigma_inf and Sigma is:
      # det(Sigma_inf - Sigma) = det(Sigma_inf) * det(B) ^ 2
      # according to Ives et al 2003 (eqn 24)

      # We can take partial derivatives to determine which elements of
      # Sigma_inf contribute most to rates of change in the
      # proportion of Sigma_inf that is due to process error
      # Thanks to Mark Scheuerell for providing inspirational code
      # https://github.com/mdscheuerell/safs-quant-sem-2022/blob/main/lwa_analysis.R
      int_env <- det(Sigma_inf) * t(solve(Sigma_inf))

      # Proportion of inter-series covariance to
      # to overall environmental variation contribution (i.e. how important are
      # correlated errors for controlling the shape of the stationary forecast
      # distribution?)
      dat <- data.frame(
        prop_cov_offdiag = mean(abs(int_env[lower.tri(int_env)])) /
          (mean(abs(diag(int_env))) + mean(abs(int_env[lower.tri(int_env)])))
      )

      # Proportion of error variances to stationary forecast distribution
      dat$prop_cov_diag <- 1 - dat$prop_cov_offdiag

      # Proportion of volume of Sigma_inf attributable to series interactions,
      # measuring the degree to which interactions increase
      # the variance of the stationary distribution (Sigma_inf) relative
      # to the variance of the process error (Sigma)
      # lower values = more stability
      dat$prop_int = abs(det(B))^2

      # Ives et al 2003 suggest to scale this by the number of series for more direct
      # comparisons among different studies
      dat$prop_int_adj <- abs(det(B))^(2 / p)

      # Sensitivity of the species interaction proportion to particular
      # interactions is also calculated using partial derivatives
      # (note the use of 2 here because we squared det(B) in the above eqn)
      int_sens <- 2 * det(B) * t(solve(B))

      # Proportion of interspecific contributions to
      # to overall interaction contribution
      dat$prop_int_offdiag <- mean(abs(int_sens[lower.tri(int_sens)])) /
        (mean(abs(diag(int_sens))) + mean(abs(int_sens[lower.tri(int_sens)])))

      # Proportion of density dependent contributions to
      # to overall interaction contribution
      dat$prop_int_diag <- 1 - dat$prop_int_offdiag

      # Reactivity, measuring the degree to which the system moves
      # away from a stable equilibrium following a perturbation
      # values > 0 suggest the system is reactive, whereby a
      # perturbation of the system in one period can be amplified in the next period
      # Following Neubert et al 2009 Ecology (Detecting reactivity)
      dat$reactivity <- log(max(svd(B)$d))

      # Return rate of transition distribution to the stationary distribution
      # Asymptotic return rate of the mean
      # lower values = more stability
      dat$mean_return_rate <- max(abs(eigen(B)$values))

      # Asymptotic return rate of the variance
      # lower values = more stability
      dat$var_return_rate <- max(abs(eigen(B %x% B)$values))
      dat
    })
  )
  return(metrics)
}
