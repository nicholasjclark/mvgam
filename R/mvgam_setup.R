#' Generic GAM setup function
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
                               drop.unused.levels = FALSE))
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
                               drop.unused.levels = FALSE))
  }

}
