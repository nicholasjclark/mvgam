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
                        maxit = 40) {

  if(missing(knots)){
    # Initialise the GAM for a few iterations to get all necessary structures for
    # generating predictions; also provides information to regularize parametric
    # effect priors for better identifiability of latent trends
    suppressWarnings(mgcv::gam(formula(formula),
                               data = data,
                               method = "REML",
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
                               method = "REML",
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
