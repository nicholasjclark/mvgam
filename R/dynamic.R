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
#'@examples
#'\dontrun{
#'# Simulate a time-varying coefficient \
#'#(as a Gaussian Process with length scale = 10)
#'set.seed(1111)
#'N <- 200
#'beta <- mvgam:::sim_gp(rnorm(1),
#'                       alpha_gp = 0.75,
#'                       rho_gp = 10,
#'                       h = N) + 0.5
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
#'# Gather into a data.frame and fit a dynamic coefficient mmodel
#'data <- data.frame(out, predictor, time)
#'
#'# Split into training and testing
#'data_train <- data[1:190,]
#'data_test <- data[191:200,]
#'
#'# Fit a model using the dynamic function
#'mod <- mvgam(out ~
#'             # mis-specify the length scale slightly as this
#'             # won't be known in practice
#'             dynamic(predictor, rho = 8, stationary = TRUE),
#'            family = gaussian(),
#'            data = data_train)
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

#' Interpret the formula specified to mvgam and replace any dynamic terms
#' with the correct Gaussian Process smooth specification
#' @importFrom stats formula terms as.formula terms.formula
#' @noRd
interpret_mvgam = function(formula, N){

  facs <- colnames(attr(terms.formula(formula), 'factors'))

  # Re-arrange so that random effects always come last
  if(any(grepl('bs = \"re\"', facs, fixed = TRUE))){
    newfacs <- facs[!grepl('bs = \"re\"', facs, fixed = TRUE)]
    refacs <- facs[grepl('bs = \"re\"', facs, fixed = TRUE)]
    int <- attr(terms.formula(formula), 'intercept')

    # Preserve offset if included
    if(!is.null(attr(terms(formula(formula)), 'offset'))){
      newformula <- as.formula(paste(terms.formula(formula)[[2]], '~',
                                     grep('offset', rownames(attr(terms.formula(formula), 'factors')),
                                          value = TRUE),
                                     '+',
                                     paste(paste(newfacs, collapse = '+'),
                                           '+',
                                           paste(refacs, collapse = '+'),
                                           collapse = '+'),
                                     ifelse(int == 0, ' - 1', '')))

    } else {
      newformula <- as.formula(paste(terms.formula(formula)[[2]], '~',
                                     paste(paste(newfacs, collapse = '+'),
                                           '+',
                                           paste(refacs, collapse = '+'),
                                           collapse = '+'),
                                     ifelse(int == 0, ' - 1', '')))
    }

  } else {
    newformula <- formula
  }

  attr(newformula, '.Environment') <- attr(formula, '.Environment')

  # Check if any terms use the gp wrapper, as mvgam cannot handle
  # multivariate GPs yet
  response <- terms.formula(newformula)[[2]]
  tf <- terms.formula(newformula, specials = c("gp"))
  which_gp <- attr(tf,"specials")$gp
  if(length(which_gp) != 0L){
    gp_details <- vector(length = length(which_gp),
                          mode = 'list')
      for(i in seq_along(which_gp)){
        gp_details[[i]] <- eval(parse(text = rownames(attr(tf,
                                                            "factors"))[which_gp[i]]))
      }
    if(any(unlist(lapply(purrr::map(gp_details, 'term'), length)) > 1)){
      stop('mvgam cannot yet handle multidimensional gps',
           call. = FALSE)
    }
  }

  # Check if any terms use the dynamic wrapper
  response <- terms.formula(newformula)[[2]]
  tf <- attr(terms.formula(newformula, keep.order = TRUE),
             'term.labels')
  which_dynamics <- grep('dynamic(', tf, fixed = TRUE)

  # Update the formula to the correct Gaussian Process implementation
  if(length(which_dynamics) != 0L){
    dyn_details <- vector(length = length(which_dynamics),
                          mode = 'list')
    if(length(which_dynamics > 1)){
      for(i in seq_along(which_dynamics)){
        dyn_details[[i]] <- eval(parse(text = tf[which_dynamics[i]]))
      }
    }

    # k is set based on the number of timepoints available; want to ensure
    # it is large enough to capture the expected wiggliness of the latent GP
    # (a smaller rho will require more basis functions for accurate approximation)
    dyn_to_gpspline = function(term, N){

      if(term$rho > N - 1){
        stop('Argument "rho" in dynamic() cannot be larger than (max(time) - 1)',
             call. = FALSE)
      }

      k <- term$k
      if(is.null(k)){
        if(N > 8){
          k <- min(50, min(N, max(8, ceiling(N / (term$rho - (term$rho / 10))))))
        } else {
          k <- N
        }
      }

      paste0("s(time,by=", term$term,
             ",bs='gp',m=c(",
             ifelse(term$stationary, '-', ''),"2,",
             term$rho, ",2),",
             "k=", k, ")")
    }

    dyn_to_gphilbert = function(term, N){

      k <- term$k
      if(is.null(k)){
        if(N > 8){
          k <- min(40, min(N, max(8, N)))
        } else {
          k <- N
        }
      }

      paste0("gp(time,by=", term$term,
             ",c=5/4,",
             "k=", k, ",scale=",
             term$scale,
             ")")
    }
    # Replace dynamic terms with the correct specification
    termlabs <- attr(terms(newformula, keep.order = TRUE), 'term.labels')
    for(i in seq_along(which_dynamics)){
      if(is.null(dyn_details[[i]]$rho)){
        termlabs[which_dynamics[i]] <- dyn_to_gphilbert(dyn_details[[i]], N = N)
      } else {
        termlabs[which_dynamics[i]] <- dyn_to_gpspline(dyn_details[[i]], N = N)
      }
    }

    # Return the updated formula for passing to mgcv
    updated_formula <- reformulate(termlabs, rlang::f_lhs(newformula))
    attr(updated_formula, '.Environment') <- attr(newformula, '.Environment')

  } else {
    updated_formula <- newformula
  }

  return(updated_formula)
}
