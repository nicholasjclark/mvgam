#' Defining dynamic coefficients in mvgam formulae
#'
#' Set up time-varying (dynamic) coefficients for use in mvgam models. Currently, only
#' low-rank Gaussian Process smooths are available for estimating the dynamics of the
#' time-varying coefficient.
#'
#' @importFrom stats terms formula reformulate
#' @param variable The variable that the dynamic smooth will be a function of
#' @param k Optional number of basis functions for computing approximate GPs. If missing,
#' `k` will be set as large as possible to accurately estimate the nonlinear function
#' @param stationary Logical. If \code{TRUE} (the default) and `rho` is supplied,
#' the latent Gaussian Process smooth will not have a linear trend component. If \code{FALSE},
#' a linear trend in the covariate is added to the Gaussian Process smooth. Leave at \code{TRUE}
#' if you do not believe the coefficient is evolving with much trend, as the linear component of the
#' basis functions can be hard to penalize to zero. This sometimes causes divergence issues in `Stan`.
#' See \code{\link[mgcv]{gp.smooth}} for details. Ignored if `rho` is missing (in which case a
#' Hilbert space approximate GP is used)
#' @param rho Either a positive numeric stating the length scale to be used for approximating the
#' squared exponential Gaussian Process smooth (see \code{\link[mgcv]{gp.smooth}} for details)
#' or missing, in which case the length scale will be estimated by setting up a Hilbert space approximate
#' GP
#' @param scale Logical; If `TRUE` (the default) and `rho` is missing, predictors
#' are scaled so that the maximum Euclidean distance between two points is `1`. This
#' often improves sampling speed and convergence. Scaling also affects the estimated
#' length-scale parameters in that they resemble those of scaled predictors
#' (not of the original predictors) if scale is `TRUE`.
#' @details \code{mvgam} currently sets up dynamic coefficients as low-rank
#' squared exponential Gaussian Process smooths via
#' the call \code{s(time, by = variable, bs = "gp", m = c(2, rho, 2))}. These smooths, if specified with
#' reasonable values for the length scale parameter, will give more realistic out of sample forecasts
#' than standard splines such as thin plate or cubic. But the user must set the
#' value for `rho`, as there is currently no support for estimating this value in \code{mgcv}.
#' This may not be too big of a problem, as estimating latent length scales is often difficult anyway. The
#' \code{rho} parameter should be thought of as a prior on the smoothness of the latent dynamic coefficient
#' function (where higher values of \code{rho} lead to smoother functions with more temporal covariance structure.
#' Values of \code{k} are
#' set automatically to ensure enough basis functions are used to approximate the expected
#' wiggliness of the underlying dynamic function (\code{k} will increase as \code{rho} decreases)
#' @rdname dynamic
#' @return a `list` object for internal usage in 'mvgam'
#'@examples
#'\donttest{
#'# Simulate a time-varying coefficient
#'#(as a Gaussian Process with length scale = 10)
#'set.seed(1111)
#'N <- 200
#'
#'# A function to simulate from a squared exponential Gaussian Process
#'sim_gp = function(N, c, alpha, rho){
#'  Sigma <- alpha ^ 2 *
#'           exp(-0.5 * ((outer(1:N, 1:N, "-") / rho) ^ 2)) +
#'           diag(1e-9, N)
#' c + mgcv::rmvn(1,
#'                mu = rep(0, N),
#'                V = Sigma)
#'}
#'
#'beta <- sim_gp(alpha = 0.75,
#'               rho = 10,
#'               c = 0.5,
#'               N = N)
#'plot(beta, type = 'l', lwd = 3,
#'     bty = 'l', xlab = 'Time',
#'     ylab = 'Coefficient',
#'     col = 'darkred')
#'
#'# Simulate the predictor as a standard normal
#'predictor <- rnorm(N, sd = 1)
#'
#'# Simulate a Gaussian outcome variable
#'out <- rnorm(N, mean = 4 + beta * predictor,
#'             sd = 0.25)
#'time <- seq_along(predictor)
#'plot(out,  type = 'l', lwd = 3,
#'     bty = 'l', xlab = 'Time', ylab = 'Outcome',
#'     col = 'darkred')
#'
#'# Gather into a data.frame and fit a dynamic coefficient model
#'data <- data.frame(out, predictor, time)
#'
#'# Split into training and testing
#'data_train <- data[1:190,]
#'data_test <- data[191:200,]
#'
#'# Fit a model using the dynamic function
#'mod <- mvgam(out ~
#'              # mis-specify the length scale slightly as this
#'              # won't be known in practice
#'              dynamic(predictor, rho = 8, stationary = TRUE),
#'             family = gaussian(),
#'             data = data_train,
#'             chains = 2)
#'
#'# Inspect the summary
#'summary(mod)
#'
#'# Plot the time-varying coefficient estimates
#'plot(mod, type = 'smooths')
#'
#'# Extrapolate the coefficient forward in time
#'plot_mvgam_smooth(mod, smooth = 1, newdata = data)
#'abline(v = 190, lty = 'dashed', lwd = 2)
#'
#'# Overlay the true simulated time-varying coefficient
#'lines(beta, lwd = 2.5, col = 'white')
#'lines(beta, lwd = 2)
#'}
#' @author Nicholas J Clark
#' @export
dynamic = function(variable, k, rho = 5, stationary = TRUE, scale = TRUE){
  # Check that only one variable is supplied
  vars <- as.list(substitute(list(variable)))[-1]
  if(length(vars) > 1) stop("dynamic() can only handle one term at a time.")
  term <- deparse(vars[[1]])
  if (term[1]==".") stop("dynamic(.) not supported.")

  # Check rho
  if(missing(rho)){
    rho <- NULL
  } else {
    if(rho <= 0){
      stop('Argument "rho" in dynamic() must be a positive value',
           call. = FALSE)
    }
  }

  # Check k
  if(missing(k)){
    k <- NULL
  } else {
    validate_pos_integer(k)
  }

  # Gather into a structured list and return
  term <- attr(terms(reformulate(term)),"term.labels")
  out <- list(term = term, rho = rho, k = k,
              stationary = stationary,
              scale = scale)
  class(out) <- "dyncoef.spec"
  return(out)
}

