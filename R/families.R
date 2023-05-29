#' Supported mvgam families
#' @importFrom stats make.link
#' @param link a specification for the family link function. At present these cannot
#' be changed
#' @details \code{mvgam} currently supports the following standard observation families:
#'\itemize{
#'   \item \code{\link[stats]{gaussian}} for real-valued data
#'   \item \code{\link[stats]{poisson}} for count data
#'   }
#'
#'In addition, the following extended families from the \code{mgcv} package are supported:
#'\itemize{
#'   \item \code{\link[mgcv]{betar}} for proportional data on (0,1)
#'   \item \code{\link[mgcv]{nb}} for count data
#'   }
#'
#'Finally, \code{mvgam} supports the three extended families described here:
#'\itemize{
#'   \item \code{lognormal} for non-negative real-valued data
#'   \item \code{tweedie} for count data (power parameter `p` fixed at `1.5`)
#'   \item \code{student-t} for real-valued data
#'   }
#'Note that only `poisson()`, `nb()`, and `tweedie()` are available if
#'using `JAGS`. All families, apart from `tweedie()`, are supported if
#'using `Stan`.
#' @name mvgam_families
NULL

#' @rdname mvgam_families
#' @export
lognormal = function(link = 'identity'){
  linktemp <- make.link('identity')
  structure(list(family = "lognormal", link = 'identity', linkfun = linktemp$linkfun,
                 linkinv = linktemp$linkinv, mu.eta = linktemp$mu.eta,
                 valideta = linktemp$valideta),
            class = c("extended.family","family"))
}

#' @rdname mvgam_families
#' @export
tweedie = function(link = 'log'){
  linktemp <- make.link('log')
  structure(list(family = "tweedie", link = 'log', linkfun = linktemp$linkfun,
                 linkinv = linktemp$linkinv, mu.eta = linktemp$mu.eta,
                 valideta = linktemp$valideta),
            class = c("extended.family","family"))
}

#' @rdname mvgam_families
#' @export
student_t = function(link = 'identity'){
  linktemp <- make.link('identity')
  structure(list(family = "student", link = 'identity', linkfun = linktemp$linkfun,
                 linkinv = linktemp$linkinv, mu.eta = linktemp$mu.eta,
                 valideta = linktemp$valideta),
            class = c("extended.family","family"))
}

#### Non-exported functions for performing family-specific tasks ####
# Evaluate family argument
#' @noRd
evaluate_family = function(family){

  if(is.character(family)){
    if(family == 'beta')
      family <- betar()

    family <- try(eval(parse(text = family)), silent = TRUE)

    if(inherits(family, 'try-error'))
      stop("family not recognized",
           call. = FALSE)
  }

  if(is.function(family))
    family <- family()

  if(is.null(family$family))
    stop("family not recognized",
         call. = FALSE)

  if(!inherits(family, 'family'))
    stop('family not recognized',
         call. = FALSE)

  if(family$family == 'Beta regression')
    family$family <- 'beta'

  return(family)
}

# Convert location / precision parameters to shape parameters for the beta distribution
# Original author: Andrew Heiss (https://www.andrewheiss.com/blog/2021/11/08/beta-regression-guide/)
#' @noRd
beta_shapes = function(mu, phi) {
  return(list(shape1 = mu * phi,
              shape2 = (1 - mu) * phi))
}

# Scaled Student's t distribution
# Original author: mjskay (https://github.com/mjskay/ggdist/blob/master/R/student_t.R)
#'
#' Density, distribution function, quantile function and random generation for the
#' scaled Student's t distribution, parameterized by degrees of freedom (`df`),
#' location (`mu`), and scale (`sigma`).
#'
#' @inheritParams stats::dt
#' @param mu Location parameter (median)
#' @param sigma Scale parameter
#'
#' @name student_t
#' @importFrom stats dt pt qt rt
#' @noRd
dstudent_t = function(x, df, mu = 0, sigma = 1, log = FALSE) {
  if (log) {
    dt((x - mu)/sigma, df = df, log = TRUE) - log(sigma)
  }
  else {
    dt((x - mu)/sigma, df = df) / sigma
  }
}

#' @noRd
pstudent_t = function(q, df, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE) {
  pt((q - mu)/sigma, df = df, lower.tail = lower.tail, log.p = log.p)
}

#' @noRd
qstudent_t = function(p, df, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE) {
  qt(p, df = df, lower.tail = lower.tail, log.p = log.p)*sigma + mu
}

#' @noRd
rstudent_t = function(n, df, mu = 0, sigma = 1) {
  as.vector(rt(n, df = df)*sigma + mu)
}

#' Generic prediction function
#' @param Xp A `mgcv` linear predictor matrix
#' @param family \code{character}. The `family` slot of the model's family argument
#' @param betas Vector of regression coefficients of length `NCOL(Xp)`
#' @param type When this has the value \code{link} (default) the linear predictor is calculated on the log link scale.
#' When \code{response} is used, the predictions take uncertainty in the observation process into account to return
#' predictions on the outcome scale
#' @param family_pars Additional arguments for each specific observation process (i.e.
#' overdispersion parameter if `family == "nb"`)
#' @param density logical. Rather than calculating a prediction, evaluate the log-likelihood.
#' Use this option when particle filtering
#' @param truth Observation to use for evaluating the likelihood (if `density == TRUE`)
#' @details A generic prediction function that will make it easier to add new
#' response distributions in future
#' @noRd
mvgam_predict = function(Xp, family, betas,
                         type = 'link',
                         family_pars,
                         density = FALSE,
                         truth = NULL){

  # Gaussian observations (requires family parameter 'sigma_obs')
  if(family == 'gaussian'){
    if(type ==  'link'){
      out <- as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                          betas) + attr(Xp, 'model.offset'))
      if(density){
        out <- dnorm(truth, mean = out,
                     sd = family_pars$sigma_obs,
                     log = TRUE)
      }

    } else {
      out <- rnorm(n = NROW(Xp),
                   mean = ((matrix(Xp, ncol = NCOL(Xp)) %*%
                              betas)) +
                     attr(Xp, 'model.offset'),
                   sd = family_pars$sigma_obs)
    }
  }

  # LogNormal observations (requires family parameter 'sigma_obs')
  if(family == 'lognormal'){
    if(type ==  'link'){
      out <- as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                          betas) + attr(Xp, 'model.offset'))
      if(density){
        out <- dlnorm(truth, meanlog = out,
                      sdlog = family_pars$sigma_obs,
                      log = TRUE)
      }

    } else {
      out <- rlnorm(n = NROW(Xp),
                    meanlog = ((matrix(Xp, ncol = NCOL(Xp)) %*%
                                      betas)) +
                                    attr(Xp, 'model.offset'),
                    sdlog = family_pars$sigma_obs)
    }
  }

  # Student-T observations (requires family parameters 'nu', 'sigma_obs')
  if(family == 'student'){
    if(type ==  'link'){
      out <- as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                          betas) + attr(Xp, 'model.offset'))
      if(density){
        out <- dstudent_t(truth,
                          df = family_pars$nu,
                          mu = out,
                          sigma = family_pars$sigma_obs,
                          log = TRUE)
      }

    } else {
      out <- rstudent_t(n = NROW(Xp),
                        df = family_pars$nu,
                        mu = ((matrix(Xp, ncol = NCOL(Xp)) %*%
                                 betas)) +
                          attr(Xp, 'model.offset'),
                        sigma = family_pars$sigma_obs)
    }
  }

  # Poisson observations
  if(family == 'poisson'){
    if(type ==  'link'){
      out <- as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                          betas) + attr(Xp, 'model.offset'))
      if(density){
        out <- dpois(truth, lambda = exp(out),
                     log = TRUE)
      }

    } else {
      out <- rpois(n = NROW(Xp),
                   lambda = exp(((matrix(Xp, ncol = NCOL(Xp)) %*%
                                    betas)) +
                                  attr(Xp, 'model.offset')))
    }
  }

  # Negative Binomial observations (requires argument 'phi')
  if(family == 'negative binomial'){
    if(type ==  'link'){
      out <- as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                          betas) + attr(Xp, 'model.offset'))

      if(density){
        out <- dnbinom(truth, mu = exp(out),
                       size = family_pars$phi,
                       log = TRUE)
      }

    } else {
      out <- rnbinom(n = NROW(Xp),
                     mu = exp(((matrix(Xp, ncol = NCOL(Xp)) %*%
                                  betas)) +
                                attr(Xp, 'model.offset')),
                     size = family_pars$phi)
    }
  }

  # Beta observations (requires argument 'phi')
  if(family == 'beta'){
    shape_pars <- beta_shapes(mu = plogis(as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                                                       betas) + attr(Xp, 'model.offset'))),
                              phi = family_pars$phi)
    if(type ==  'link'){
      out <- as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                          betas) + attr(Xp, 'model.offset'))

      if(density){
        out <- dbeta(truth,
                     shape1 = shape_pars$shape1,
                     shape2 = shape_pars$shape2,
                     log = TRUE)
      }

    } else {
      out <- rbeta(n = NROW(Xp),
                   shape1 = shape_pars$shape1,
                   shape2 = shape_pars$shape2)
    }
  }

  # Tweedie observations (requires argument 'phi')
  if(family == 'tweedie'){
    if(type ==  'link'){
      out <- as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                          betas) + attr(Xp, 'model.offset'))

      if(density){
        out <- mgcv::ldTweedie(y = truth,
                                   mu = exp(out),
                                   # Power parameter is fixed
                                   p = 1.5,
                                   phi = family_pars$phi,
                                   all.derivs = F)[,1]
      }

    } else {
      out <- rpois(n = NROW(Xp),
                   lambda = mgcv::rTweedie(
                     mu = exp(((matrix(Xp, ncol = NCOL(Xp)) %*%
                                  betas)) +
                                attr(Xp, 'model.offset')),
                     # Power parameter is fixed
                     p = 1.5,
                     phi = family_pars$phi))
    }
  }
  return(out)
}

#' Set which family to use when setting up the gam object
#' @noRd
family_to_mgcvfam = function(family){
  if(family$family == 'beta'){
    betar()
  } else if(family$family == 'student'){
    gaussian()
  } else if(family$family == 'lognormal'){
    Gamma()
  } else if(family$family == 'tweedie'){
    Tweedie(p=1.5)
  } else {
    family
  }
}

#' Set which family to use when setting up the jagam object
#' @noRd
family_to_jagamfam = function(family){
  if(family %in% c('gaussian', 'student')){
    gaussian()
  } else {
    poisson()
  }
}

#' Define links used for the mean
#' @noRd
family_links = function(family){
  if(family %in% c('gaussian', 'lognormal', 'student')){
    out <- 'identity'
  }
  if(family %in% c('Gamma', 'poisson', 'negative binomial', 'tweedie')){
    out <- 'log'
  }
  if(family == 'beta'){
    out <- 'logit'
  }
  out
}

#' @noRd
family_invlinks = function(family){
  if(family %in% c('gaussian', 'lognormal', 'student')){
    out <- function(x) { x }
  }
  if(family %in% c('Gamma', 'poisson', 'negative binomial', 'tweedie')){
    out <- function(x) { exp(x) }
  }
  if(family == 'beta'){
    out <- function(x) { plogis(x) }
  }
  out
}

#' Parameters to monitor / extract depending on the observation family
#' @noRd
family_par_names = function(family){


  if(family %in% c('gaussian',
                   'lognormal')){
    out <- c('sigma_obs')
  }

  if(family == 'student'){
    out <- c('sigma_obs', 'nu')
  }

  if(family == 'Gamma'){
    out <- c('shape')
  }

  if(family %in% c('beta',
                   'negative binomial',
                   'tweedie')){
    out <- c('phi')
  }

  if(family == 'poisson'){
    out <- c()
  }
  return(out)
}

#' Family specific initial value functions for Stan
#' @noRd
family_inits = function(family, trend_model,
                        smooths_included, model_data){

  # No longer specifying inits for family- or trend-specific parameters
  # as there is a risk the user will place bounds on priors that conflict
  # with the inits. Just let Stan choose reasonable and diffuse inits,
  # this is better anyway for sampling
    inits <- function() {
      list(b_raw = runif(model_data$num_basis, -2, 2))
    }
  return(inits)
}

#' Define which parameters to monitor / extract
#' @noRd
extract_family_pars = function(object){

  # Get names of parameters to extract
  pars_to_extract <- family_par_names(object$family)

  # Extract into a named list
  if(length(pars_to_extract) > 0){
    out <- vector(mode = 'list')
    for(i in 1:length(pars_to_extract)){
      out[[i]] <- mcmc_chains(object$model_output,
                                      params = pars_to_extract[i])
    }

  } else {
    out <- list()
  }

  names(out) <- pars_to_extract

  # Return list of extracted posterior parameter samples
  out
}

#' Family-specific prior information
#' @noRd
family_prior_info = function(family, use_stan, data){
  if(family == 'gaussian'){
    prior_df <- data.frame(param_name = c('vector<lower=0>[n_series] sigma_obs;'),
                          param_length = length(unique(data$series)),
                          param_info = c('observation error sd'),
                          prior = c('sigma_obs ~ student_t(3, 0, 2);'),
                          example_change = c(
                            paste0(
                              'sigma_obs ~ normal(',
                              round(runif(min = -1, max = 1, n = 1), 2),
                              ', ',
                              round(runif(min = 0.1, max = 1, n = 1), 2),
                              ');'
                            )))
  }

  if(family == 'lognormal'){
    prior_df <- data.frame(param_name = c('vector<lower=0>[n_series] sigma_obs;'),
                           param_length = length(unique(data$series)),
                           param_info = c('log(observation error sd)'),
                           prior = c('sigma_obs ~ student_t(3, 0, 1);'),
                           example_change = c(
                             paste0(
                               'sigma_obs ~ normal(',
                               round(runif(min = -1, max = 1, n = 1), 2),
                               ', ',
                               round(runif(min = 0.1, max = 1, n = 1), 2),
                               ');'
                             )))
  }

  if(family == 'student'){
    prior_df <- data.frame(param_name = c('vector<lower=0>[n_series] sigma_obs;',
                                          'vector<lower=0>[n_series] nu;'),
                           param_length = rep(length(unique(data$series)),
                                              2),
                           param_info = c('observation error sd',
                                          'observation degrees of freedom'),
                           prior = c('sigma_obs ~ student_t(3, 0, 2);',
                                     'nu ~ gamma(2, 0.1);'),
                           example_change = c(
                             paste0(
                               'sigma_obs ~ normal(',
                               round(runif(min = -1, max = 1, n = 1), 2),
                               ', ',
                               round(runif(min = 0.1, max = 1, n = 1), 2),
                               ');'
                             ),
                             paste0(
                               'nu ~ normal(',
                               round(runif(min = -1, max = 1, n = 1), 2),
                               ', ',
                               round(runif(min = 0.1, max = 1, n = 1), 2),
                               ');'
                             )))
  }

  if(family == 'beta'){
    prior_df <- data.frame(param_name = c('vector<lower=0>[n_series] phi;'),
                           param_length = length(unique(data$series)),
                           param_info = c('Beta precision parameter'),
                           prior = c('phi ~ gamma(0.01, 0.01);'),
                           example_change = c(
                             paste0(
                               'phi ~ normal(',
                               round(runif(min = -1, max = 1, n = 1), 2),
                               ', ',
                               round(runif(min = 0.1, max = 1, n = 1), 2),
                               ');'
                             )))
  }

  if(family == 'negative binomial'){
    if(use_stan){
      prior_df <- data.frame(param_name = c('vector<lower=0>[n_series] phi_inv;'),
                          param_length = length(unique(data$series)),
                          param_info = c('inverse of NB dispsersion'),
                          prior = c('phi_inv ~ student_t(3, 0, 0.1);'),
                          example_change = c(
                            paste0(
                              'phi_inv ~ normal(',
                              round(runif(min = -1, max = 1, n = 1), 2),
                              ', ',
                              round(runif(min = 0.1, max = 1, n = 1), 2),
                              ');'
                            )))
    } else {
      prior_df <- data.frame(param_name = c('vector<lower=0>[n_series] phi_inv;'),
                          param_length = length(unique(data$series)),
                          param_info = c('inverse of NB dispsersion'),
                          prior = c('phi_inv[s] ~ dexp(5)'),
                          example_change = c(
                            paste0(
                              'phi_inv[s] ~ dnorm(',
                              round(runif(min = -1, max = 1, n = 1), 2),
                              ', ',
                              round(runif(min = 0.1, max = 1, n = 1), 2),
                              ')T(0, )'
                            )))
    }

  }

  if(family == 'tweedie'){
    prior_df<- data.frame(param_name = c('vector<lower=0>[n_series] phi_raw;'),
                        param_length = length(unique(data$series)),
                        param_info = c('log of Tweedie dispsersion (for each series s)'),
                        prior = c('phi_raw[s] ~ dnorm(0, 2)T(-3.5, 3.5)'),
                        example_change = c(
                          paste0(
                            'phi_raw[s] ~ dnorm(',
                            round(runif(min = -1, max = 1, n = 1), 2),
                            ', ',
                            round(runif(min = 0.5, max = 5, n = 1), 2),
                            ')'
                          )))
  }

  if(family == 'poisson'){
    prior_df <- NULL
  }

  return(prior_df)
}

#' Family-specific Dunn-Smyth residual functions
#' @noRd
ds_resids_nb = function(truth, fitted, draw, size){
  na_obs <- is.na(truth)

  p <- size[!na_obs] / (fitted[!na_obs] + size[!na_obs])
  a_obs <- ifelse(as.vector(truth[!na_obs]) > 0,
              pbeta(p, size[!na_obs],
                    pmax(as.vector(truth[!na_obs]), 1)), 0)
  b_obs <- pbeta(p, size[!na_obs], as.vector(truth[!na_obs]) + 1)
  u_obs <- runif(n = length(draw[!na_obs]), min = a_obs, max = b_obs)

  if(any(is.na(truth))){
    u_na <- runif(n = length(draw[na_obs]),
                  min = 0, max = 1)
    u <- vector(length = length(truth))
    u[na_obs] <- u_na
    u[!na_obs] <- u_obs
  } else {
    u <- u_obs
  }
  dsres_out <- qnorm(u)
  dsres_out[is.infinite(dsres_out)] <- NaN
  dsres_out
}

#' @noRd
ds_resids_beta = function(truth, fitted, draw, precision){
  shape_pars <- beta_shapes(mu = fitted,
                            phi = precision)
  na_obs <- is.na(truth)
  a_obs <- pbeta(as.vector(truth[!na_obs]) - 1.e-6,
                 shape1 = shape_pars$shape1,
                 shape2 = shape_pars$shape2)
  b_obs <- pbeta(as.vector(truth[!na_obs]),
                 shape1 = shape_pars$shape1,
                 shape2 = shape_pars$shape2)
  u_obs <- runif(n = length(draw[!na_obs]),
                 min = pmin(a_obs, b_obs),
                 max = pmax(a_obs, b_obs))

  if(any(is.na(truth))){
    u_na <- runif(n = length(draw[na_obs]),
                  min = 0, max = 1)
    u <- vector(length = length(truth))
    u[na_obs] <- u_na
    u[!na_obs] <- u_obs
  } else {
    u <- u_obs
  }
  dsres_out <- qnorm(u)
  dsres_out[is.infinite(dsres_out)] <- NaN
  dsres_out
}

#' @noRd
ds_resids_pois = function(truth, fitted, draw){
  na_obs <- is.na(truth)
  a_obs <- ppois(as.vector(truth[!na_obs]) - 1.e-6,
                 lambda = fitted[!na_obs])
  b_obs <- ppois(as.vector(truth[!na_obs]),
                 lambda = fitted[!na_obs])
  u_obs <- runif(n = length(draw[!na_obs]),
                 min = pmin(a_obs, b_obs), max = pmax(a_obs, b_obs))

  if(any(is.na(truth))){
    u_na <- runif(n = length(draw[na_obs]),
                  min = 0, max = 1)
    u <- vector(length = length(truth))
    u[na_obs] <- u_na
    u[!na_obs] <- u_obs
  } else {
    u <- u_obs
  }
  dsres_out <- qnorm(u)
  dsres_out[is.infinite(dsres_out)] <- NaN
  dsres_out
}

#' @noRd
ds_resids_tw = function(truth, fitted, draw){
  na_obs <- is.na(truth)
  a_obs <- ppois(as.vector(truth[!na_obs]) - 1.e-6,
                 lambda = fitted[!na_obs])
  b_obs <- ppois(as.vector(truth[!na_obs]),
                 lambda = fitted[!na_obs])
  u_obs <- runif(n = length(draw[!na_obs]),
                 min = pmin(a_obs, b_obs), max = pmax(a_obs, b_obs))

  if(any(is.na(truth))){
    u_na <- runif(n = length(draw[na_obs]),
                  min = 0, max = 1)
    u <- vector(length = length(truth))
    u[na_obs] <- u_na
    u[!na_obs] <- u_obs
  } else {
    u <- u_obs
  }
  dsres_out <- qnorm(u)
  dsres_out[is.infinite(dsres_out)] <- NaN
  dsres_out
}

#' @noRd
ds_resids_gaus = function(truth, fitted, sigma, draw){
  na_obs <- is.na(truth)
  a_obs <- pnorm(as.vector(truth[!na_obs]) - 1.e-6,
                 mean = fitted[!na_obs],
                 sd = sigma)
  b_obs <- pnorm(as.vector(truth[!na_obs]),
                 mean = fitted[!na_obs],
                 sd = sigma)
  u_obs <- runif(n = length(draw[!na_obs]),
                 min = pmin(a_obs, b_obs), max = pmax(a_obs, b_obs))

  if(any(is.na(truth))){
    u_na <- runif(n = length(draw[na_obs]),
                  min = 0, max = 1)
    u <- vector(length = length(truth))
    u[na_obs] <- u_na
    u[!na_obs] <- u_obs
  } else {
    u <- u_obs
  }
  dsres_out <- qnorm(u)
  dsres_out[is.infinite(dsres_out)] <- NaN
  dsres_out
}

#' @noRd
ds_resids_lnorm = function(truth, fitted, sigma, draw){
  na_obs <- is.na(truth)
  a_obs <- plnorm(as.vector(truth[!na_obs]) - 1.e-6,
                 meanlog = fitted[!na_obs],
                 sdlog = sigma)
  b_obs <- plnorm(as.vector(truth[!na_obs]),
                 meanlog = fitted[!na_obs],
                 sd = sigma)
  u_obs <- runif(n = length(draw[!na_obs]),
                 min = pmin(a_obs, b_obs), max = pmax(a_obs, b_obs))

  if(any(is.na(truth))){
    u_na <- runif(n = length(draw[na_obs]),
                  min = 0, max = 1)
    u <- vector(length = length(truth))
    u[na_obs] <- u_na
    u[!na_obs] <- u_obs
  } else {
    u <- u_obs
  }
  dsres_out <- qnorm(u)
  dsres_out[is.infinite(dsres_out)] <- NaN
  dsres_out
}

#' @noRd
ds_resids_student = function(truth, fitted, sigma, nu, draw){
  na_obs <- is.na(truth)
  a_obs <- pstudent_t(as.vector(truth[!na_obs]) - 1,
                      df = nu,
                      mu = fitted[!na_obs],
                      sigma = sigma)
  b_obs <- pstudent_t(as.vector(truth[!na_obs]),
                      df = nu,
                      mu = fitted[!na_obs],
                      sigma = sigma)
  u_obs <- runif(n = length(draw[!na_obs]),
                 min = pmin(a_obs, b_obs), max = pmax(a_obs, b_obs))

  if(any(is.na(truth))){
    u_na <- runif(n = length(draw[na_obs]),
                  min = 0, max = 1)
    u <- vector(length = length(truth))
    u[na_obs] <- u_na
    u[!na_obs] <- u_obs
  } else {
    u <- u_obs
  }
  dsres_out <- qnorm(u)
  dsres_out[is.infinite(dsres_out)] <- NaN
  dsres_out
}

#'Residual calculations for a forecast
#' @noRd
get_forecast_resids = function(object, series, truth, preds, family,
                               sample_seq){
  if(family == 'poisson'){
    series_residuals <- do.call(rbind, lapply(sample_seq, function(x){
      suppressWarnings(ds_resids_pois(truth = truth,
                                      fitted = preds[x, ],
                                      draw = preds[x, ]))
    }))
  }

  if(family == 'negative binomial'){
    size <- mcmc_chains(object$model_output, 'phi')[,series]
    series_residuals <- do.call(rbind, lapply(sample_seq, function(x){
      suppressWarnings(ds_resids_nb(truth = truth,
                                    fitted = preds[x, ],
                                    draw = preds[x, ],
                                    size = size[x]))
    }))
  }

  if(family == 'gaussian'){
    sigma <- mcmc_chains(object$model_output, 'sigma_obs')[,series]
    series_residuals <- do.call(rbind, lapply(sample_seq, function(x){
      suppressWarnings(ds_resids_gaus(truth = truth,
                                    fitted = preds[x, ],
                                    draw = preds[x, ],
                                    sigma = sigma[x]))
    }))
  }

  if(family == 'lognormal'){
    sigma <- mcmc_chains(object$model_output, 'sigma_obs')[,series]
    series_residuals <- do.call(rbind, lapply(sample_seq, function(x){
      suppressWarnings(ds_resids_lnorm(truth = truth,
                                      fitted = preds[x, ],
                                      draw = preds[x, ],
                                      sigma = sigma[x]))
    }))
  }

  if(family == 'student'){
    sigma <- mcmc_chains(object$model_output, 'sigma_obs')[,series]
    nu <- mcmc_chains(object$model_output, 'nu')[,series]
    series_residuals <- do.call(rbind, lapply(sample_seq, function(x){
      suppressWarnings(ds_resids_student(truth = truth,
                                       fitted = preds[x, ],
                                       draw = preds[x, ],
                                       sigma = sigma[x],
                                       nu = nu[x]))
    }))
  }

  if(family == 'beta'){
    precision <- mcmc_chains(object$model_output, 'phi')[,series]
    series_residuals <- do.call(rbind, lapply(sample_seq, function(x){
      suppressWarnings(ds_resids_beta(truth = truth,
                                       fitted = preds[x, ],
                                       draw = preds[x, ],
                                       precision = precision[x]))
    }))
  }

  if(family == 'tweedie'){
    series_residuals <- do.call(rbind, lapply(sample_seq, function(x){
      suppressWarnings(ds_resids_tw(truth = truth,
                                    fitted = preds[x, ],
                                    draw = preds[x, ]))
    }))
  }

  return(series_residuals)
}


#'Residual calculations for a fitted mvgam object
#'
#' @param object \code{list} object returned from \code{mvgam}
#' @param n_cores \code{integer} specifying number of cores for generating residual distributions in parallel
#' @author Nicholas J Clark
#' @details Dunn-Smyth residual distributions are calculated for each series in the fitted object
#' @return A \code{list} of residual distributions
#' @noRd
get_mvgam_resids = function(object, n_cores = 1){

  # Check arguments
  if(sign(n_cores) != 1){
    stop('argument "n_cores" must be a positive integer',
         call. = FALSE)
  } else {
    if(n_cores%%1 != 0){
      stop('argument "n_cores" must be a positive integer',
           call. = FALSE)
    }
  }

  # Extract necessary model elements; for Stan models, expectations are
  # stored on the link scale
  if(object$fit_engine == 'stan'){
    linkfun <- family_invlinks(object$family)
    preds <- linkfun(mcmc_chains(object$model_output, 'mus'))

  } else {
    preds <- mcmc_chains(object$model_output, 'mus')
  }

  n_series <- NCOL(object$ytimes)
  obs_series <- object$obs_data$series
  series_levels <- levels(obs_series)
  family <- object$family
  obs_data <- object$obs_data
  fit_engine <- object$fit_engine

  # Create sequences of posterior draws for calculating residual distributions
  sample_seq <- 1:NROW(preds)
  draw_seq <- sample(sample_seq, length(sample_seq), replace = FALSE)

  # Family-specific parameters
  family_pars <- mvgam:::extract_family_pars(object = object)

  # Pull out starting and ending indices for each series in the object
  ends <- seq(0, dim(preds)[2],
              length.out = n_series + 1)
  starts <- ends + 1
  starts <- c(1, starts[-c(1, (n_series+1))])
  ends <- ends[-1]

  # Calculate DS residual distributions in parallel
  cl <- parallel::makePSOCKcluster(n_cores)
  setDefaultCluster(cl)
  clusterExport(NULL, c('sample_seq',
                        'draw_seq',
                        'n_series',
                        'obs_series',
                        'series_levels',
                        'family',
                        'family_pars',
                        'preds',
                        'ends',
                        'starts',
                        'obs_series',
                        'obs_data',
                        'fit_engine'),
                envir = environment())
  clusterEvalQ(cl, library(dplyr))

  pbapply::pboptions(type = "none")
  series_resids <- pbapply::pblapply(seq_len(n_series), function(series){
    if(class(obs_data)[1] == 'list'){
      n_obs <- data.frame(series = obs_series) %>%
        dplyr::filter(series == !!(series_levels[series])) %>%
        nrow()
    } else {
      n_obs <- obs_data %>%
        dplyr::filter(series == !!(series_levels[series])) %>%
        nrow()
    }

    if(fit_engine == 'stan'){
      preds <- preds[,seq(series,
                          dim(preds)[2],
                          by = n_series)]
    } else {
      preds <- preds[,starts[series]:ends[series]]
    }

    s_name <- levels(obs_data$series)[series]
    truth <- data.frame(time = obs_data$time,
                        series = obs_data$series,
                        y = obs_data$y) %>%
      dplyr::filter(series == s_name) %>%
      dplyr::select(time, y) %>%
      dplyr::distinct() %>%
      dplyr::arrange(time) %>%
      dplyr::pull(y)

    # Keep only the predictions that match the observation period
    # (not the out of sample predictions, if any were computed in the model)
    preds <- preds[,1:length(truth)]

    # Create a truth matrix for vectorised residual computation
    truth_mat <- matrix(rep(truth, NROW(preds)),
                        nrow = NROW(preds),
                        byrow = TRUE)

    if(family == 'gaussian'){
      sigma_obs <- family_pars$sigma_obs[,series]
      sigma_mat <- matrix(rep(sigma_obs,
                              NCOL(preds)),
                          ncol = NCOL(preds))
      resids <- matrix(ds_resids_gaus(truth = as.vector(truth_mat),
                                      fitted = as.vector(preds),
                                      draw = as.vector(preds[draw_seq,]),
                                      sigma= as.vector(sigma_mat)),
                       nrow = NROW(preds))
    }

    if(family == 'student'){
      sigma_obs <- family_pars$sigma_obs[,series]
      sigma_mat <- matrix(rep(sigma_obs,
                              NCOL(preds)),
                          ncol = NCOL(preds))

      nu <- family_pars$nu[,series]
      nu_mat <- matrix(rep(nu,
                           NCOL(preds)),
                       ncol = NCOL(preds))

      resids <- matrix(ds_resids_student(truth = as.vector(truth_mat),
                                         fitted = as.vector(preds),
                                         draw = as.vector(preds[draw_seq,]),
                                         sigma = as.vector(sigma_mat),
                                         nu = nu_mat),
                       nrow = NROW(preds))
    }

    if(family == 'lognormal'){
      sigma_obs <- family_pars$sigma_obs[,series]
      sigma_mat <- matrix(rep(sigma_obs,
                              NCOL(preds)),
                          ncol = NCOL(preds))
      resids <- matrix(ds_resids_lnorm(truth = as.vector(truth_mat),
                                      fitted = as.vector(preds),
                                      draw = as.vector(preds[draw_seq,]),
                                      sigma= as.vector(sigma_mat)),
                       nrow = NROW(preds))
    }

    if(family == 'poisson'){
      resids <- matrix(ds_resids_pois(truth = as.vector(truth_mat),
                                      fitted = as.vector(preds),
                                      draw = as.vector(preds[draw_seq,])),
                       nrow = NROW(preds))
    }

    if(family == 'beta'){
      precision <- family_pars$phi[,series]
      precision_mat <- matrix(rep(precision, NCOL(preds)),
                              ncol = NCOL(preds))
      resids <- matrix(ds_resids_beta(truth = as.vector(truth_mat),
                                    fitted = as.vector(preds),
                                    draw = as.vector(preds[draw_seq,]),
                                    precision = as.vector(precision_mat)),
                       nrow = NROW(preds))
    }

    if(family == 'negative binomial'){
      size <- family_pars$phi[,series]
      size_mat <- matrix(rep(size, NCOL(preds)),
                         ncol = NCOL(preds))
      resids <- matrix(ds_resids_nb(truth = as.vector(truth_mat),
                                    fitted = as.vector(preds),
                                    draw = as.vector(preds[draw_seq,]),
                                    size = as.vector(size_mat)),
                       nrow = NROW(preds))
    }

    if(family == 'tweedie'){
      resids <- matrix(ds_resids_tw(truth = as.vector(truth_mat),
                                    fitted = as.vector(preds),
                                    draw = as.vector(preds[draw_seq,])),
                       nrow = NROW(preds))
    }

    resids

  }, cl = cl)
  stopCluster(cl)
  names(series_resids) <- series_levels
  return(series_resids)
}
