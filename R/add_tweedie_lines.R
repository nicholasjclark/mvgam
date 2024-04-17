#' Tweedie JAGS modifications
#'
#'
#' @param model_file A template `JAGS` model file to be modified
#' @param upper_bounds Optional upper bounds for the truncated observation likelihood
#' @return A modified `JAGS` model file
#' @noRd
add_tweedie_lines = function(model_file, upper_bounds){
  rate_begin <- grep('rate\\[i, s\\] <- ', model_file)
  rate_end <- rate_begin + 1
  model_file <- model_file[-c(rate_begin:rate_end)]

  odis_begin <- grep('phi\\[s\\] <- ', model_file) - 4
  odis_end <- odis_begin + 7
  model_file <- model_file[-c(odis_begin:odis_end)]

  if(missing(upper_bounds)){
    model_file[grep('y\\[i, s\\] ~', model_file)] <- '  y[i, s] ~ dpois(linpred[i, s])\n  linpred[i, s] ~'
    model_file[grep('ypred\\[i, s\\] ~', model_file)] <- '  ypred[i, s] ~ dpois(linpred[i, s])'

  } else {
    model_file[grep('y\\[i, s\\] ~', model_file)] <- '  y[i, s] ~ dpois(linpred[i, s])T(, upper_bound[s])\n  linpred[i, s] ~'
    model_file[grep('ypred\\[i, s\\] ~', model_file)] <- '  ypred[i, s] ~ dpois(linpred[i, s])T(, upper_bound[s])'
  }

  model_file <- readLines(textConnection(model_file), n = -1)
  model_file[grep('linpred\\[i, s\\] ~', model_file)] <- '  linpred[i, s] ~ dgamma(shape[i, s, y_ind[i, s]], rate[i, s])\n  twlambda[i, s] <-'
  model_file <- readLines(textConnection(model_file), n = -1)

  model_file[grep('twlambda\\[i, s\\] <-', model_file)] <- '  twlambda[i, s] <- pow(mus[i, s], 2 - p) / (phi[s] * (2 - p))\n  N_pois[i, s] ~'
  model_file <- readLines(textConnection(model_file), n = -1)

  model_file[grep('N_pois\\[i, s\\] ~', model_file)] <- '  N_pois[i, s] ~ dpois(twlambda[i, s])T(1,)\n  shape[i, s, 1] <-'
  model_file <- readLines(textConnection(model_file), n = -1)

  model_file[grep('shape\\[i, s, 1\\] <-', model_file)] <- '  shape[i, s, 1] <- N_pois[i, s] * ((2 - p) / (p - 1))\n  shape[i, s, 2] <-'
  model_file <- readLines(textConnection(model_file), n = -1)

  model_file[grep('shape\\[i, s, 2\\] <-', model_file)] <- '  shape[i, s, 2] <- 1\n  rate[i, s] <-'
  model_file <- readLines(textConnection(model_file), n = -1)

  model_file[grep('rate\\[i, s\\] <-', model_file)] <- '  rate[i, s] <- 1 / (phi[s] * (p - 1) * pow(mus[i, s], p - 1))\n  pois_draw[i, s] ~'
  model_file <- readLines(textConnection(model_file), n = -1)

  model_file[grep('pois_draw\\[i, s\\] ~', model_file)] <- '  pois_draw[i, s] ~ dpois(mus[i, s])\n  is_zero[i, s] <-'
  model_file <- readLines(textConnection(model_file), n = -1)

  model_file[grep('is_zero\\[i, s\\] <-', model_file)] <- '  is_zero[i, s] <- equals(pois_draw[i, s], 0)\n  y_ind[i, s] <-'
  model_file <- readLines(textConnection(model_file), n = -1)

  model_file[grep('y_ind\\[i, s\\] <-', model_file)] <- '  y_ind[i, s] <- is_zero[i, s] + 1'
  model_file <- readLines(textConnection(model_file), n = -1)

  yind_begin <- grep('y_ind\\[i, s\\] <-', model_file)
  prior_line <- yind_begin + 2
  model_file[prior_line] <- '}\n\n## Tweedie power and overdispersion parameters\np <- 1.5\nfor (s in 1:n_series) {\n phi_raw[s] ~ dnorm(0, 2)T(-3.5, 3.5);\n phi[s] <- exp(phi_raw[s])\n}'
  model_file <- readLines(textConnection(model_file), n = -1)

  return(model_file)
}
