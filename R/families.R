#' Generic family information
#' @param family \code{character}
#' @param object Fitted model of class `mvgam`
#' @noRd
#'
#'
family_to_fullname = function(family){
  if(family == 'nb'){
    'Negative Binomial'
  } else if(family == 'poisson'){
    'Poisson'
  } else if(family == 'tw'){
    'Tweedie'
  } else if(family == 'beta'){
    'Beta'
  } else if(family == 'normal'){
    'Normal'
  } else if(family == 'lognormal'){
    'LogNormal'
  } else if(family == 'student'){
    'Student-T'
  } else if(family == 'gamma'){
    'Gamma'
  }
}

# This function converts the fullnames to an appropriate family for
# fitting a reduced mgcv model to create the GAM structure and to allow
# for easier prediction post-fitting; it is needed because mgcv cannot handle all of
fullname_to_family = function(family){
  if(family == 'Negative Binomial'){
    'nb'
  } else if(family == 'Poisson'){
    'poisson'
  } else if(family == 'Tweedie'){
    'Tweedie(p=1.5)'
  } else if(family == 'Beta'){
    'betar'
  } else if(family == 'Normal'){
    'gaussian'
  } else if(family == 'LogNormal'){
    'Gamma'
  } else if(family == 'Student-T'){
    'gaussian'
  } else if(family == 'Gamma'){
    'Gamma'
  }
}


family_par_names = function(family){

  # Parameters to monitor / extract depending on the observation family
  if(family %in% c('Normal',
                   'LogNormal')){
    out <- c('sigma_obs')
  }

  if(family == 'Student-T'){
    out <- c('sigma_obs', 'nu_obs')
  }

  if(family == 'Gamma'){
    out <- c('shape')
  }

  if(family %in% c('Beta',
                   'Negative Binomial',
                   'Tweedie')){
    out <- c('phi')
  }

  if(family == 'Poisson'){
    out <- c()
  }

  out
}

family_links = function(family){
  if(family %in% c('Normal',
                   'LogNormal',
                   'Student-T')){
    out <- c('identity')
  }

  if(family %in% c('Negative Binomial',
                   'Tweedie',
                   'Poisson',
                   'Gamma')){
    out <- c('log')
  }

  if(family == 'Beta'){
    out <- 'logit'
  }

  out
}

extract_family_pars = function(family, object){

  # Get names of parameters to extract
  pars_to_extract <- family_par_names(family)

  # Extract into a named list
  if(length(pars_to_extract) > 0){
    out <- vector(mode = 'list')
    for(i in 1:length(pars_to_extract)){
      out[[i]] <- MCMCvis::MCMCchains(object$model_output,
                                      params = pars_to_extract[i])
    }
    names(out) <- pars_to_extract

  } else {
    out <- list()
  }

  # Return list of extracted posterior parameter samples
  out
}


