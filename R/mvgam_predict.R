#' Generic prediction function
#' @param Xp A `mgcv` linear predictor matrix
#' @param family \code{character}. Must be either 'nb' (for Negative Binomial), 'tw' (for Tweedie) or 'poisson'
#' @param betas Vector of regression coefficients of length `NCOL(Xp)`
#' @param type When this has the value \code{link} (default) the linear predictor is calculated on the log link scale.
#' When \code{response} is used, the predictions take uncertainty in the observation process into account to return
#' predictions on the outcome scale
#' @param ... Additional arguments for each specific observation process (i.e.
#' overdispersion parameter if `family == "nb"`)
#' @details A generic prediction function that will make it easier to add new
#' response distributions in future
#' @noRd
#'
#'
mvgam_predict = function(Xp, family, betas, type = 'link', ...){

  # Poisson observations
  if(family == 'Poisson'){
    if(type ==  'link'){
      out <- as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                          betas) + attr(Xp, 'model.offset'))
    } else {
      out <- rpois(n = NROW(Xp),
                   lambda = exp(((matrix(Xp, ncol = NCOL(Xp)) %*%
                                    betas)) +
                                  attr(Xp, 'model.offset')))
    }
  }

  # Negative Binomial observations (requires argument 'size')
  if(family == 'Negative Binomial'){
    if(type ==  'link'){
      out <- as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                          betas) + attr(Xp, 'model.offset'))
    } else {
      out <- rnbinom(n = NROW(Xp),
                     mu = exp(((matrix(Xp, ncol = NCOL(Xp)) %*%
                                  betas)) +
                                attr(Xp, 'model.offset')),
                     size = list(...)$size)
    }
  }

  # Tweedie observations (requires arguments 'p' and 'twdis')
  if(family == 'Tweedie'){
    if(type ==  'link'){
      out <- as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                          betas) + attr(Xp, 'model.offset'))
    } else {
      out <- rpois(n = NROW(Xp),
                   lambda = mgcv::rTweedie(
                     mu = exp(((matrix(Xp, ncol = NCOL(Xp)) %*%
                                  betas)) +
                                attr(Xp, 'model.offset')),
                     p = list(...)$p,
                     phi = list(...)$twdis))
    }
  }

  return(out)

}

#' Generic squared exponential GP simulation function
#' @param last_trends Vector of trend estimates leading up to the current timepoint
#' @param h \code{integer} specifying the forecast horizon
#' @param rho_gp length scale parameter
#' @param alpha_gp marginal variation parameter
#' @noRd
#'
#'
sim_gp = function(last_trends, h, rho_gp, alpha_gp){

  t <- 1:length(last_trends)
  t_new <- 1:(length(last_trends) + h)
  Sigma_new <- alpha_gp^2 * exp(-0.5 * ((outer(t, t_new, "-") / rho_gp) ^ 2))
  Sigma_star <- alpha_gp^2 * exp(-0.5 * ((outer(t_new, t_new, "-") / rho_gp) ^ 2)) +
    diag(1e-4, length(t_new))
  Sigma <- alpha_gp^2 * exp(-0.5 * ((outer(t, t, "-") / rho_gp) ^ 2)) +
    diag(1e-4, length(t))

  as.vector(tail(t(Sigma_new) %*% solve(Sigma, last_trends),
                 h) +
              tail(MASS::mvrnorm(1,
                                 mu = rep(0, length(t_new)),
                                 Sigma = Sigma_star - t(Sigma_new) %*% solve(Sigma, Sigma_new)),
                   h))
}

#' Generic AR3  simulation function
#' @param last_trends Vector of trend estimates leading up to the current timepoint
#' @param h \code{integer} specifying the forecast horizon
#' @param phi drift parameter
#' @param ar1 AR1 parameter
#' @param ar2 AR2 parameter
#' @param ar3 AR3 parameter
#' @param tau precision parameter
#' @noRd
#'
#'
sim_ar3 = function(phi, ar1, ar2, ar3, tau, last_trends, h){
  states <- rep(NA, length = h + 3)
  states[1] <- last_trends[1]
  states[2] <- last_trends[2]
  states[3] <- last_trends[3]
  for (t in 4:(h + 3)) {
    states[t] <- rnorm(1, phi + ar1*states[t - 1] +
                         ar2*states[t - 2] +
                         ar3*states[t - 3], sqrt(1 / tau))
  }
  states[-c(1:3)]
}
