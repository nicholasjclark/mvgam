#' Poisson JAGS modifications
#'
#'
#' @export
#' @param model_file A template `JAGS` model file to be modified
#' @param upper_bounds Optional upper bounds for the truncated observation likelihood
#' @return A modified `JAGS` model file
add_poisson_lines = function(model_file, upper_bounds){

  odis_begin <- grep('r\\[s\\] <- ', model_file) - 4
  odis_end <- odis_begin + 7
  model_file <- model_file[-c(odis_begin:odis_end)]

  rate_begin <- grep('rate\\[i, s\\] <- ', model_file)
  rate_end <- rate_begin + 1
  model_file <- model_file[-c(rate_begin:rate_end)]

  if(missing(upper_bounds)){
    model_file[grep('y\\[i, s\\] ~', model_file)] <- '  y[i, s] ~ dpois(mu[i, s])'
    model_file[grep('ypred\\[i, s\\] ~', model_file)] <- '  ypred[i, s] ~ dpois(mu[i, s])'

  } else {
    model_file[grep('y\\[i, s\\] ~', model_file)] <- '  y[i, s] ~ dpois(mu[i, s])T(, upper_bound[s])'
    model_file[grep('ypred\\[i, s\\] ~', model_file)] <- '  ypred[i, s] ~ dpois(mu[i, s])T(, upper_bound[s])'
  }

  model_file
}
