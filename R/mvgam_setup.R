#' Generic GAM setup function. All credit goes to Simon Wood
#' @details A generic function for setting up a GAM object with appropriate smooths
#' @noRd
#'
mvgam_setup <- function(formula,
                        family = gaussian,
                        data = list(),
                        na.action,
                        knots = NULL,
                        drop.unused.levels = FALSE,
                        maxit = 40) {

  # Check family will work in mgcv
  if (is.character(family))
    family <- eval(parse(text = family))
  if (is.function(family))
    family <- family()
  if (is.null(family$family))
    stop("family not recognized")

  # Initialise the GAM for a few iterations to get all necessary structures for
  # generating predictions; also provides information to regularize parametric
  # effect priors for better identifiability of latent trends
  mgcv::gam(formula(formula),
            data = data,
            method = "REML",
            family = family,
            knots = knots,
            control = list(nthreads = min(4, parallel::detectCores() - 1),
                           maxit = maxit),
            drop.unused.levels = FALSE)
}
