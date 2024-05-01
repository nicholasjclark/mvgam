#' Generic GAM setup function
#' @importFrom stats na.fail
#' @noRd
#'
mvgam_setup <- function(formula,
                        knots,
                        family = gaussian(),
                        dat = list(),
                        na.action,
                        drop.unused.levels = FALSE,
                        maxit = 5) {

  if(missing(knots)){
    # Initialise the GAM for a few iterations to ensure it all works without error
    suppressWarnings(mgcv::gam(formula(formula),
                               data = dat,
                               method = 'REML',
                               family = family,
                               control = list(maxit = maxit),
                               drop.unused.levels = FALSE,
                               na.action = na.fail,
                               select = TRUE))
  } else {
    suppressWarnings(mgcv::gam(formula(formula),
                               data = dat,
                               method = 'REML',
                               family = family,
                               knots = knots,
                               control = list(maxit = maxit),
                               drop.unused.levels = FALSE,
                               na.action = na.fail,
                               select = TRUE))
  }

}

#' Generic JAGAM setup function
#' @noRd
#'
jagam_setup <- function(ss_gam, formula, data_train, family,
                        family_char, knots){

  # Change the formula to a Poisson-like formula if this is a cbind Binomial,
  # as jagam will fail if it sees that
  if(family$family %in% c('binomial', 'beta_binomial')){
    resp_terms <- as.character(terms(formula(formula))[[2]])
    if(any(grepl('cbind', resp_terms))){
      resp_terms <- resp_terms[-grepl('cbind', resp_terms)]
      out_name <- resp_terms[1]
    } else {
      stop('Binomial family requires the cbind() left-hand side formula syntax',
           call. = FALSE)
    }
    formula <- update(formula, paste(out_name, '~ .'))
    family <- poisson()
  }

  # Set file save location in tempdir
  file_name <- tempfile(pattern = 'base_gam', fileext = '.txt')
  if(length(ss_gam$smooth) == 0){
    smooths_included <- FALSE
    # If no smooth terms are included, jagam will fail; so add a fake one and remove
    # it from the model and data structures later
    data_train$fakery <- rnorm(length(data_train$y))
    form_fake <- update.formula(formula, ~ . + s(fakery, k = 3))
    fakery_names <- names(suppressWarnings(mgcv::gam(form_fake,
                                                     data = data_train,
                                                     family = family_to_mgcvfam(family),
                                                     drop.unused.levels = FALSE,
                                                     control = list(maxit = 1),
                                                     method = 'REML'))$coefficients)
    xcols_drop <- grep('s(fakery', fakery_names, fixed = TRUE)

    if(!missing(knots)){
      ss_jagam <- mgcv::jagam(form_fake,
                              data = data_train,
                              family = family_to_jagamfam(family_char),
                              file = file_name,
                              sp.prior = 'gamma',
                              diagonalize = FALSE,
                              knots = knots,
                              drop.unused.levels = FALSE)
    } else {
      ss_jagam <- mgcv::jagam(form_fake,
                              data = data_train,
                              family = family_to_jagamfam(family_char),
                              file = file_name,
                              sp.prior = 'gamma',
                              diagonalize = FALSE,
                              drop.unused.levels = FALSE)
    }
    data_train$fakery <- NULL
  } else {
    smooths_included <- TRUE
    xcols_drop <- NULL

    # If smooth terms included, use the original formula
    if(!missing(knots)){
      ss_jagam <- mgcv::jagam(formula,
                              data = data_train,
                              family = family_to_jagamfam(family_char),
                              file = file_name,
                              sp.prior = 'gamma',
                              diagonalize = FALSE,
                              knots = knots,
                              drop.unused.levels = FALSE)
    } else {
      ss_jagam <- mgcv::jagam(formula,
                              data = data_train,
                              family = family_to_jagamfam(family_char),
                              file = file_name,
                              sp.prior = 'gamma',
                              diagonalize = FALSE,
                              drop.unused.levels = FALSE)
    }
  }
  return(list(file_name = file_name,
              ss_jagam = ss_jagam,
              smooths_included = smooths_included,
              xcols_drop = xcols_drop))
}

#' @noRd
get_offset <- function(model) {
  nm1 <- names(attributes(model$terms)$dataClasses)
  if('(offset)' %in% nm1) {
    deparse(as.list(model$call)$offset)
  } else {

    sub("offset\\((.*)\\)$", "\\1", grep('offset', nm1, value = TRUE))
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
