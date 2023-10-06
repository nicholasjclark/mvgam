#' Defining dynamic coefficients in mvgam formulae
#'
#' Set up time-varying (dynamic) coefficients for use in mvgam models. Currently, only
#' low-rank Gaussian Process smooths are available for estimating the dynamics of the
#' time-varying coefficient.
#'
#' @importFrom stats terms formula reformulate
#' @param variable The variable that the dynamic smooth will be a function of
#' @param stationary logical. If \code{TRUE} (the default), the latent Gaussian Process
#' smooth will not have a linear trend component. If \code{FALSE},
#' a linear trend in the covariate is added to the Gaussian Process smooth. Leave at \code{TRUE}
#' if you do not believe the coefficient is evolving with much trend, as the linear component of the
#' basis functions can be hard to penalize to zero. This sometimes causes divergence issues in `Stan`.
#' See \code{\link[mgcv]{gp.smooth}} for details
#' @param rho Positive numeric stating the length scale to be used for approximating the
#' squared exponential Gaussian Process smooth. See \code{\link[mgcv]{gp.smooth}} for details
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
dynamic = function(variable, rho = 5, stationary = TRUE){
  # Check that only one variable is supplied
  vars <- as.list(substitute(list(variable)))[-1]
  if(length(vars) > 1) stop("dynamic() can only handle one term at a time.")
  term <- deparse(vars[[1]])
  if (term[1]==".") stop("dynamic(.) not supported.")

  # Check rho
  if(rho <= 0){
    stop('Argument "rho" in dynamic() must be a positive value',
         call. = FALSE)
  }

  # Gather into a structured list and return
  term <- attr(terms(reformulate(term)),"term.labels")
  out <- list(term = term, rho = rho, stationary = stationary)
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
  tf <- terms.formula(newformula, specials = c("dynamic"))
  which_dynamics <- attr(tf,"specials")$dynamic

  # Update the formula to the correct Gaussian Process spline
  if(length(which_dynamics) != 0L){
    dyn_details <- vector(length = length(which_dynamics),
                          mode = 'list')
    if(length(which_dynamics > 1)){
      for(i in seq_along(which_dynamics)){
        dyn_details[[i]] <- eval(parse(text = rownames(attr(tf,
                                                            "factors"))[which_dynamics[i]]))
      }
    }

    # k is set based on the number of timepoints available; want to ensure
    # it is large enough to capture the expected wiggliness of the latent GP
    # (a smaller rho will require more basis functions for accurate approximation)
    dyn_to_gp = function(term, N){

      if(term$rho > N - 1){
        stop('Argument "rho" in dynamic() cannot be larger than (max(time) - 1)',
             call. = FALSE)
      }

      if(N > 8){
        k <- min(50, min(N, max(8, ceiling(N / (term$rho - (term$rho / 10))))))
      } else {
        k <- N
      }

      paste0("s(time,by=", term$term,
             ",bs='gp',m=c(",
             ifelse(term$stationary, '-', ''),"2,",
             term$rho, ",2),",
             "k=", k, ")")
    }

    # Replace dynamic terms with the correct specification
    update_terms <- vector(length = length(which_dynamics))
    old_rhs <- as.character(newformula)[3]
    old_rhs <- gsub(" ", "", (old_rhs), fixed = TRUE)
    old_rhs <- gsub(',stationary=TRUE|,stationary=FALSE', '',
         old_rhs)
    for(i in seq_along(which_dynamics)){
      update_terms[i] <- dyn_to_gp(dyn_details[[i]], N = N)

      old_rhs <- sub(paste0('dynamic(',
                            dyn_details[[i]]$term,
                            ',rho=',
                            dyn_details[[i]]$rho, ')'),
                     update_terms[i],
                     old_rhs,
                     fixed = TRUE)
    }

    # Return the updated formula for passing to mgcv
    updated_formula <- as.formula(paste(response, '~', old_rhs))
    attr(updated_formula, '.Environment') <- attr(newformula, '.Environment')

  } else {
    updated_formula <- newformula
  }

  return(updated_formula)
}
