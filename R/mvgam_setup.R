#' Generic GAM setup function
#' @importFrom stats na.fail
#' @noRd
#'
mvgam_setup <- function(formula,
                        knots,
                        family = gaussian(),
                        data = list(),
                        na.action,
                        drop.unused.levels = FALSE,
                        maxit = 5) {

  if(missing(knots)){
    # Initialise the GAM for a few iterations to get all necessary structures for
    # generating predictions; also provides information to regularize parametric
    # effect priors for better identifiability of latent trends
    suppressWarnings(mgcv::gam(formula(formula),
                               data = data,
                               method = 'GCV.Cp',
                               family = family,
                               control = list(maxit = maxit),
                               drop.unused.levels = FALSE,
                               na.action = na.fail,
                               select = TRUE))
  } else {
    # Initialise the GAM for a few iterations to get all necessary structures for
    # generating predictions; also provides information to regularize parametric
    # effect priors for better identifiability of latent trends
    suppressWarnings(mgcv::gam(formula(formula),
                               data = data,
                               method = 'GCV.Cp',
                               family = family,
                               knots = knots,
                               control = list(maxit = maxit),
                               drop.unused.levels = FALSE,
                               na.action = na.fail,
                               select = TRUE))
  }

}

#' @noRd
trim_mgcv <- function(mgcv_model){

  mgcv_model$fitted.values <- mgcv_model$residuals <- mgcv_model$linear.predictors <-
    mgcv_model$working.weights <- mgcv_model$z <- NULL

  mgcv_model
}

#' Fill in missing observations in data_train so the size of the dataset is correct when
#' building the initial JAGS model
#' @noRd
replace_nas = function(var){
  if(all(is.na(var))){
    # Sampling from uniform[0.1,0.99] will allow all the gam models
    # to work, even though the Poisson / Negative Binomial will issue
    # warnings. This is ok as we just need to produce the linear predictor matrix
    # and store the coefficient names
    var <- runif(length(var), 0.1, 0.99)
  } else {
    # If there are some non-missing observations,
    # sample from the observed values to ensure
    # distributional assumptions are met without warnings
    var[which(is.na(var))] <-
      sample(var[which(!is.na(var))],
             length(which(is.na(var))),
             replace = TRUE)
  }
  var
}

#' @noRd
rmvn <- function(n,mu,sig) {
  L <- mgcv::mroot(sig); m <- ncol(L);
  t(mu + L%*%matrix(rnorm(m*n),m,n))
}
