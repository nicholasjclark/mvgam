#' Supported mvgam families
#' @importFrom stats make.link dgamma pgamma rgamma qnorm plnorm runif pbeta dlnorm dpois
#' @importFrom stats pnorm ppois plogis gaussian poisson Gamma dnbinom rnbinom dnorm dbeta
#' @importFrom stats binomial rbinom pbinom dbinom qbinom qlogis
#' @importFrom brms lognormal student bernoulli rstudent_t qstudent_t dstudent_t pstudent_t dbeta_binomial rbeta_binomial pbeta_binomial
#' @param link a specification for the family link function. At present these cannot
#' be changed
#' @details \code{mvgam} currently supports the following standard observation families:
#'\itemize{
#'   \item \code{\link[stats]{gaussian}} with identity link, for real-valued data
#'   \item \code{\link[stats]{poisson}} with log-link, for count data
#'   \item \code{\link[stats]{Gamma}} with log-link, for non-negative real-valued data
#'   \item \code{\link[stats]{binomial}} with logit-link, for count data when the number
#'   of trials is known (and must be supplied)
#'   }
#'
#'In addition, the following extended families from the \code{mgcv} and \code{brms} packages are supported:
#'\itemize{
#'   \item \code{\link[mgcv]{betar}} with logit-link, for proportional data on `(0,1)`
#'   \item \code{\link[mgcv]{nb}} with log-link, for count data
#'   \item \code{\link[brms]{lognormal}} with identity-link, for non-negative real-valued data
#'   \item \code{\link[brms]{bernoulli}} with logit-link, for binary data
#'   }
#'
#'Finally, \code{mvgam} supports the three extended families described here:
#'\itemize{
#'   \item \code{tweedie} with log-link, for count data (power parameter `p` fixed at `1.5`)
#'   \item `student_t()` (or \code{\link[brms]{student}}) with identity-link, for real-valued data
#'   \item \code{nmix} for count data with imperfect detection modeled via a
#'   State-Space N-Mixture model. The latent states are Poisson (with log link), capturing the 'true' latent
#'   abundance, while the observation process is Binomial to account for imperfect detection. The
#'   observation \code{formula} in these models is used to set up a linear predictor for the detection
#'   probability (with logit link). See the example below for a more detailed worked explanation
#'   of the `nmix()` family
#'   }
#' Only `poisson()`, `nb()`, and `tweedie()` are available if
#' using `JAGS`. All families, apart from `tweedie()`, are supported if
#' using `Stan`.
#' @name mvgam_families
#' @details Note that currently it is not possible to change the default link
#' functions in `mvgam`, so any call to change these will be silently ignored
#' @author Nicholas J Clark
#'
NULL

#' @rdname mvgam_families
#' @export
tweedie = function(link = 'log'){
  linktemp <- make.link('log')
  structure(list(family = "tweedie", link = 'log', linkfun = linktemp$linkfun,
                 linkinv = linktemp$linkinv, mu.eta = linktemp$mu.eta,
                 valideta = linktemp$valideta),
            class = c("extended.family", "family"))
}

#' @rdname mvgam_families
#' @export
student_t = function(link = 'identity'){
  linktemp <- make.link('identity')
  structure(list(family = "student", link = 'identity', linkfun = linktemp$linkfun,
                 linkinv = linktemp$linkinv, mu.eta = linktemp$mu.eta,
                 valideta = linktemp$valideta),
            class = c("extended.family", "family"))
}

#' @rdname mvgam_families
#' @examples
#' \donttest{
#' # An N-mixture example using family nmix()
#' # Simulate data from a Poisson-Binomial N-Mixture model
#' # True abundance is predicted by a single nonlinear function of temperature
#' # as well as a nonlinear long-term trend (as a Gaussian Process function)
#' set.seed(123)
#' gamdat <- gamSim(n = 80); N <- NROW(gamdat)
#' abund_linpred <- gamdat$y; temperature <- gamdat$x2
#'
#' # A function to simulate from a squared exponential Gaussian Process
#' sim_gp = function(N, c, alpha, rho){
#'   Sigma <- alpha ^ 2 *
#'            exp(-0.5 * ((outer(1:N, 1:N, "-") / rho) ^ 2)) +
#'            diag(1e-9, N)
#'  c + mgcv::rmvn(1,
#'                 mu = rep(0, N),
#'                 V = Sigma)
#' }
#' trend <- sim_gp(alpha = 3,
#'                 rho = 16,
#'                 c = 0,
#'                 N = N)
#' true_abund <- floor(10 + abund_linpred + trend)
#'
#' # Detection probability increases linearly with decreasing rainfall
#' rainfall <- rnorm(N)
#' detect_linpred <- 0.4 + -0.55 * rainfall
#' detect_prob <- plogis(detect_linpred)
#'
#' # Simulate observed counts
#' obs_abund <- rbinom(N, size = true_abund, prob = detect_prob)
#'
#' # Plot true and observed time series
#' plot(true_abund,
#'      type = 'l',
#'      ylab = 'Abundance',
#'      xlab = 'Time',
#'      ylim = c(0, max(true_abund)),
#'      bty = 'l',
#'      lwd = 2)
#' lines(obs_abund, col = 'darkred', lwd = 2)
#' title(main = 'True = black; observed = red')
#'
#' # Gather data into a dataframe suitable for mvgam modelling;
#' # This will require a 'cap' variable specifying the maximum K to marginalise
#' # over when estimating latent abundance (it does NOT have to be a fixed value)
#' model_dat <- data.frame(obs_abund,
#'                         temperature,
#'                         rainfall,
#'                         cap = max(obs_abund) + 20,
#'                         time = 1:N,
#'                         series = as.factor('series1'))
#'
#' # Training and testing folds
#' data_train <- model_dat %>% dplyr::filter(time <= 74)
#' data_test <- model_dat %>% dplyr::filter(time > 74)
#'
#' # Fit a model with informative priors on the two intercept parameters
#' # and on the length scale of the GP temporal trend parameter
#' # Note that the 'trend_formula' applies to the latent count process
#' # (a Poisson process with log-link), while the 'formula' applies to the
#' # detection probability (logit link)
#' mod <- mvgam(formula = obs_abund ~ rainfall,
#'              trend_formula = ~ s(temperature, k = 5) +
#'                gp(time, k = 10, c = 5/4, scale = FALSE),
#'              family = nmix(),
#'              data = data_train,
#'              newdata = data_test,
#'              priors = c(prior(std_normal(), class = '(Intercept)'),
#'                         prior(normal(2, 2), class = '(Intercept)_trend'),
#'                         prior(normal(15, 5), class = 'rho_gp_trend(time)')))
#'
#' # Model summary and diagnostics
#' summary(mod)
#' plot(mod, type = 'residuals')
#'
#' # Intercept parameters
#' mcmc_plot(mod,
#'           variable = "Intercept",
#'           regex = TRUE,
#'           type = 'hist')
#'
#' # Hindcasts and forecasts of latent abundance (with truth overlain)
#' fc <- forecast(mod, type = 'latent_N')
#' plot(fc); points(true_abund, pch = 16, cex = 0.8)
#'
#' # Latent abundance predictions are restricted based on 'cap'
#' max(model_dat$cap); range(fc$forecasts[[1]])
#'
#' # Hindcasts and forecasts of detection probability (with truth overlain)
#' fc <- forecast(mod, type = 'detection')
#' plot(fc); points(detect_prob, pch = 16, cex = 0.8)
#'
#' # Hindcasts and forecasts of observations
#' # (after accounting for detection error)
#' fc <- forecast(mod, type = 'response')
#' plot(fc)
#'
#' # Hindcasts and forecasts of response expectations
#' # (with truth overlain)
#' fc <- forecast(object = mod, type = 'expected')
#' plot(fc); points(detect_prob * true_abund, pch = 16, cex = 0.8)
#'
#' # Plot conditional effects
#' library(ggplot2)
#'
#' # Effects on true abundance can be visualised using type = 'link'
#' abund_plots <- plot(conditional_effects(mod,
#'                                         type = 'link',
#'                                         effects = c('temperature', 'time')),
#'                     plot = FALSE)
#'
#' # Effect of temperature on abundance
#' abund_plots[[1]] +
#'   ylab('Latent abundance')
#'
#' # Long-term trend in abundance
#' abund_plots[[2]] +
#'   ylab('Latent abundance')
#'
#' # Effect of rainfall on detection probability
#' det_plot <- plot(conditional_effects(mod,
#'                                      type = 'detection',
#'                                      effects = 'rainfall'),
#'                  plot = FALSE)
#' det_plot[[1]] +
#'   ylab('Pr(detection)')
#'
#' # More targeted plots can use marginaleffects capabilities;
#' # Here visualise how response predictions might change
#' # if we considered different possible 'cap' limits on latent
#' # abundance and different rainfall measurements
#' plot_predictions(mod, condition = list('temperature',
#'                                        cap = c(15, 20, 40),
#'                                        rainfall = c(-1, 1)),
#'                 type = 'response',
#'                 conf_level = 0.5) +
#'  ylab('Observed abundance') +
#'  theme_classic()
#'
#' # Example showcasing how cbind() is needed for Binomial observations
#' # Simulate two time series of Binomial trials
#' trials <- sample(c(20:25), 50, replace = TRUE)
#' x <- rnorm(50)
#' detprob1 <- plogis(-0.5 + 0.9*x)
#' detprob2 <- plogis(-0.1 -0.7*x)
#' dat <- rbind(data.frame(y = rbinom(n = 50, size = trials, prob = detprob1),
#'                         time = 1:50,
#'                         series = 'series1',
#'                         x = x,
#'                         ntrials = trials),
#'              data.frame(y = rbinom(n = 50, size = trials, prob = detprob2),
#'                         time = 1:50,
#'                         series = 'series2',
#'                         x = x,
#'                         ntrials = trials))
#' dat <- dplyr::mutate(dat, series = as.factor(series))
#' dat <- dplyr::arrange(dat, time, series)
#'
#' # Fit a model using the binomial() family; must specify observations
#' # and number of trials in the cbind() wrapper
#' mod <- mvgam(cbind(y, ntrials) ~ series + s(x, by = series),
#'              family = binomial(),
#'              data = dat)
#' summary(mod)
#'
#' }
#' @export
nmix = function(link = 'log'){
  linktemp <- make.link('log')
  structure(list(family = "nmix", link = 'log', linkfun = linktemp$linkfun,
                 linkinv = linktemp$linkinv, mu.eta = linktemp$mu.eta,
                 valideta = linktemp$valideta),
            class = c("extended.family", "family"))
}

#### Non-exported functions for performing family-specific tasks ####
#' Family options in character format
#' @noRd
family_char_choices = function(){
  c('negative binomial',
    "poisson",
    "binomial",
    'beta_binomial',
    "bernoulli",
    "nmix",
    "tweedie",
    "beta",
    "gaussian",
    "lognormal",
    "student",
    "Gamma")
}

# Convert location / precision parameters to shape parameters for the beta distribution
# Original author: Andrew Heiss (https://www.andrewheiss.com/blog/2021/11/08/beta-regression-guide/)
#' @noRd
beta_shapes = function(mu, phi) {
  return(list(shape1 = mu * phi,
              shape2 = (1 - mu) * phi))
}

# Calculate all possible Poisson log-densities for N-mixture simulation
#' @noRd
pois_dens = function(min_cap, max_cap, lambdas){
  # Identify which indices share the exact same lambda AND
  # k value so that we only need to run dpois once for each group
  data.frame(lambdas,
             min_cap,
             max_cap) %>%
    dplyr::group_by(lambdas) %>%
    dplyr::summarise(min_cap = min(min_cap, na.rm = TRUE),
                     max_cap = max(max_cap, na.rm = TRUE)) -> group_inds

  l <- mapply(`:`, group_inds$min_cap, group_inds$max_cap)

  data.frame(k = unlist(l),
             lambda = group_inds$lambdas[rep(1:nrow(group_inds),
                                             lengths(l))]) %>%
    dplyr::mutate(pois_dens = dpois(k,
                                    lambda,
                                    log = TRUE)) -> all_ks

  return(all_ks)
}


#' Generic prediction function
#' @importFrom stats predict
#' @param Xp A `mgcv` linear predictor matrix
#' @param family \code{character}. The `family` slot of the model's family argument
#' @param betas Vector of regression coefficients of length `NCOL(Xp)`
#' @param latent_lambdas Optional vector of latent abundance estimates, only used for N-Mixture models
#' @param cap Optional vector of latent abundance maximum capacities, only used for N-Mixture models
#' @param type Either `link`, `expected`, `response`, `variance`,
#' `latent_N` (only applies to N-mixture distributions) or
#' `detection` (only applies to N-mixture distributions)
#' @param family_pars Additional arguments for each specific observation process (i.e.
#' overdispersion parameter if `family == "nb"`)
#' @param density logical. Rather than calculating a prediction, evaluate the log-likelihood.
#' Use this option when particle filtering
#' @param truth Observation to use for evaluating the likelihood (if `density == TRUE`)
#' @details A generic prediction function that will make it easier to add new
#' response distributions in future. Use `type = variance` for computing family-level
#' variance as a function of the mean
#' @noRd
mvgam_predict = function(Xp,
                         family,
                         betas,
                         latent_lambdas,
                         cap,
                         min_cap,
                         type = 'link',
                         family_pars,
                         density = FALSE,
                         truth = NULL){

  if(type == 'latent_N' & family != 'nmix'){
    stop('"latent_N" type only available for N-mixture models',
         call. = FALSE)
  }

  if(type == 'detection' & family != 'nmix'){
    stop('"detection" type only available for N-mixture models',
         call. = FALSE)
  }

  # Poisson-Binomial N-Mixture (requires family parameter
  # 'cap' as well as 'latent_lambdas' argument)
  if(family == 'nmix'){
    insight::check_if_installed("extraDistr",
                                reason = 'to simulate from N-Mixture distributions')
    insight::check_if_installed("wrswoR",
                                reason = 'to simulate from N-Mixture distributions')
    # Calculate detection probability and convert to probability scale
    p <- as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                            betas) + attr(Xp, 'model.offset'))
    p <- plogis(p)

    # Latent mean of State vector
    lambdas <- as.vector(latent_lambdas)

    # User-specified cap on latent abundance
    cap <- as.vector(cap)

    if(type == 'detection'){
      out <- p
    } else if(type == 'link'){
      # 'link' predictions are expectations of the latent abundance
      out <- lambdas

      if(density){
        out <- unlist(lapply(seq_along(truth), function(i){
          if(is.na(truth[i])){
            output <- NA
          } else {
            ks <- truth[i]:cap[i]
            lik_binom <- dbinom(truth[i], size = ks,
                                prob = p[i], log = TRUE)
            lik_poisson <- dpois(x = ks,
                                 lambda = lambdas[i], log = TRUE)
            loglik <- lik_binom + lik_poisson
            output <- log_sum_exp(loglik)
          }
          output
        }), use.names = FALSE)
      }

    } else if(type == 'latent_N'){
      if(missing(min_cap)) min_cap <- 0
      min_cap <- as.vector(min_cap)
      if(missing(truth)){
        out <- extraDistr::rtpois(n = length(lambdas),
                                  lambda = lambdas,
                                  a = min_cap,
                                  b = cap)
      } else {
        # If true observed N is supplied, we can calculate the
        # most likely latent N given the covariates and the estimated
        # detection probability
        out <- unlist(lapply(seq_along(truth), function(i){
          if(is.na(truth[i])){
            output <- NA
          } else {
            ks <- min_cap[[i]]:cap[[i]]
            lik <- exp(dbinom(truth[[i]], size = ks,
                              prob = p[[i]], log = TRUE) +
                         dpois(x = ks,
                               lambda = lambdas[i], log = TRUE))
            probs <- lik / sum(lik)
            probs[!is.finite(probs)] <- 0
            output <- ks[wrswoR::sample_int_ccrank(length(ks),
                                                size = 1L,
                                                prob = probs)]
          }
          output
        }), use.names = FALSE)
      }
    } else if(type == 'response'){
      xpred <- extraDistr::rtpois(n = length(lambdas),
                                  lambda = lambdas,
                                  b = cap)
      out <- rbinom(length(lambdas), size = xpred, prob = p)

    } else if(type == 'variance'){
      xpred <- extraDistr::rtpois(n = length(lambdas),
                                  lambda = lambdas,
                                  b = cap)

      # Variance of a Binomial distribution using the
      # weights convention from stats::glm()
      mu <- p / xpred
      out <- mu * (1 - mu)
    } else {
      # Expectations
      xpred <- extraDistr::rtpois(n = length(lambdas),
                                  lambda = lambdas,
                                  b = cap)
      out <- xpred * p
    }
  }

  # Gaussian observations (requires family parameter 'sigma_obs')
  if(family == 'gaussian'){
    if(type %in% c('link', 'expected')){
      out <- as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                          betas) + attr(Xp, 'model.offset'))
      if(density){
        out <- dnorm(truth, mean = out,
                     sd = as.vector(family_pars$sigma_obs),
                     log = TRUE)
      }

    } else if(type == 'variance') {
      out <- rep.int(1, NROW(Xp))
    } else {
      out <- rnorm(n = NROW(Xp),
                   mean = ((matrix(Xp, ncol = NCOL(Xp)) %*%
                              betas)) +
                     attr(Xp, 'model.offset'),
                   sd = as.vector(family_pars$sigma_obs))
    }
  }

  # LogNormal observations (requires family parameter 'sigma_obs')
  if(family == 'lognormal'){
    if(type ==  'link'){
      out <- as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                          betas) + attr(Xp, 'model.offset'))
      if(density){
        out <- dlnorm(truth, meanlog = out,
                      sdlog = as.vector(family_pars$sigma_obs),
                      log = TRUE)
      }

    } else if(type == 'response'){
      out <- rlnorm(n = NROW(Xp),
                    meanlog = ((matrix(Xp, ncol = NCOL(Xp)) %*%
                                      betas)) +
                                    attr(Xp, 'model.offset'),
                    sdlog = as.vector(family_pars$sigma_obs))
    } else if(type == 'variance'){
      mu <- ((matrix(Xp, ncol = NCOL(Xp)) %*%
                betas)) + attr(Xp, 'model.offset')
      sd <- as.vector(family_pars$sigma_obs)
      out <- (exp((sd) ^ 2) - 1) * exp((2 * mu + sd ^ 2))

    } else {
      mu <- as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                              betas) + attr(Xp, 'model.offset'))
      out <- exp(mu + (as.vector(family_pars$sigma_obs)^2 / 2))
    }
  }

  # Student-T observations (requires family parameters 'nu', 'sigma_obs')
  if(family == 'student'){
    if(type %in%  c('link', 'expected')){
      out <- as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                          betas) + attr(Xp, 'model.offset'))
      if(density){
        out <- dstudent_t(truth,
                          df = as.vector(family_pars$nu),
                          mu = out,
                          sigma = as.vector(family_pars$sigma_obs),
                          log = TRUE)
      }

    } else if(type == 'variance') {
      out <- as.vector(family_pars$sigma_obs)^2 *
        as.vector(family_pars$nu) /
        pmax(1.01, (as.vector(family_pars$nu) - 2))

    } else {
      out <- rstudent_t(n = NROW(Xp),
                        df = family_pars$nu,
                        mu = ((matrix(Xp, ncol = NCOL(Xp)) %*%
                                 betas)) +
                          attr(Xp, 'model.offset'),
                        sigma = as.vector(family_pars$sigma_obs))
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

    } else if(type == 'response'){
      out <- rpois(n = NROW(Xp),
                   lambda = exp(((matrix(Xp, ncol = NCOL(Xp)) %*%
                                    betas)) +
                                  attr(Xp, 'model.offset')))
    } else if(type == 'variance'){
      out <- exp(as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                          betas) + attr(Xp, 'model.offset')))
      } else {
      out <- exp(as.vector(((matrix(Xp, ncol = NCOL(Xp)) %*%
                     betas)) +
                   attr(Xp, 'model.offset')))
    }
  }

  # Bernoulli observations
  if(family == 'bernoulli'){
    if(type ==  'link'){
      out <- as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                          betas) + attr(Xp, 'model.offset'))

      if(density){
        out <- dbinom(truth,
                      prob = plogis(out),
                      size = 1,
                      log = TRUE)
      }

    } else if(type == 'response'){
      out <- rbinom(n = NROW(Xp),
                    prob = plogis(((matrix(Xp, ncol = NCOL(Xp)) %*%
                                      betas)) +
                                    attr(Xp, 'model.offset')),
                    size = 1)
    } else if(type == 'variance'){
      mu <- plogis(as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                                betas) + attr(Xp, 'model.offset')))
      out <- mu * (1 - mu)
    } else {
      out <- plogis(((matrix(Xp, ncol = NCOL(Xp)) %*%
                        betas)) +
                      attr(Xp, 'model.offset'))
    }
  }


  # Binomial observations (requires argument 'trials')
  if(family == 'binomial'){
    if(type ==  'link'){
      out <- as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                          betas) + attr(Xp, 'model.offset'))

      if(density){
        out <- dbinom(truth,
                      prob = plogis(out),
                      size = as.vector(family_pars$trials),
                      log = TRUE)
      }

    } else if(type == 'response'){
      out <- rbinom(n = NROW(Xp),
                    prob = plogis(((matrix(Xp, ncol = NCOL(Xp)) %*%
                                      betas)) +
                                    attr(Xp, 'model.offset')),
                    size = as.vector(family_pars$trials))
    } else if(type == 'variance'){
      mu <- plogis(as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                                betas) + attr(Xp, 'model.offset'))) /
        as.vector(family_pars$trials)
      out <- mu * (1 - mu)
    } else {
      out <- plogis(((matrix(Xp, ncol = NCOL(Xp)) %*%
                     betas)) +
                   attr(Xp, 'model.offset')) *
        as.vector(family_pars$trials)
    }
  }

  # Beta_Binomial observations (requires arguments 'trials' and 'phi')
  if(family == 'beta_binomial'){
    if(type ==  'link'){
      out <- as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                          betas) + attr(Xp, 'model.offset'))

      if(density){
        out <- dbeta_binomial(truth,
                              mu = plogis(out),
                              phi = as.vector(family_pars$phi),
                              size = as.vector(family_pars$trials),
                              log = TRUE)
      }

    } else if(type == 'response'){
      out <- rbeta_binomial(n = NROW(Xp),
                            mu = plogis(((matrix(Xp, ncol = NCOL(Xp)) %*%
                                            betas)) +
                                          attr(Xp, 'model.offset')),
                            phi = as.vector(family_pars$phi),
                            size = as.vector(family_pars$trials))
    } else if(type == 'variance'){
      mu <- plogis(as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                                betas) + attr(Xp, 'model.offset')))
      # https://en.wikipedia.org/wiki/Beta-binomial_distribution
      alpha <- mu * as.vector(family_pars$phi)
      beta <- (1 - mu) * as.vector(family_pars$phi)
      p <- 1 / (alpha + beta + 1)
      n <- as.vector(family_pars$trials)
      out <- ((n * p) * (1 - p)) * ((alpha + beta + n) / (alpha + beta + 1))

    } else {
      out <- plogis(((matrix(Xp, ncol = NCOL(Xp)) %*%
                        betas)) +
                      attr(Xp, 'model.offset')) *
        as.vector(family_pars$trials)
    }
  }

  # Negative Binomial observations (requires argument 'phi')
  if(family == 'negative binomial'){
    if(type ==  'link'){
      out <- as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                          betas) + attr(Xp, 'model.offset'))

      if(density){
        out <- dnbinom(truth, mu = exp(out),
                       size = as.vector(family_pars$phi),
                       log = TRUE)
      }

    } else if(type == 'response'){
      out <- rnbinom(n = NROW(Xp),
                     mu = exp(((matrix(Xp, ncol = NCOL(Xp)) %*%
                                  betas)) +
                                attr(Xp, 'model.offset')),
                     size = as.vector(family_pars$phi))
    } else if(type == 'variance'){
      mu <- exp(as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                             betas) + attr(Xp, 'model.offset')))
      out <- mu + mu^2 / as.vector(family_pars$phi)
    } else {
      out <- exp(((matrix(Xp, ncol = NCOL(Xp)) %*%
                     betas)) +
                   attr(Xp, 'model.offset'))
    }
  }

  # Beta observations (requires argument 'phi')
  if(family == 'beta'){
    shape_pars <- beta_shapes(mu = plogis(as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                                                       betas) + attr(Xp, 'model.offset'))),
                              phi = as.vector(family_pars$phi))
    if(type == 'link'){
      out <- as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                          betas) + attr(Xp, 'model.offset'))

      if(density){
        out <- dbeta(truth,
                     shape1 = shape_pars$shape1,
                     shape2 = shape_pars$shape2,
                     log = TRUE)
      }

    } else if(type == 'response'){
      out <- rbeta(n = NROW(Xp),
                   shape1 = shape_pars$shape1,
                   shape2 = shape_pars$shape2)
    } else if(type == 'variance'){
      mu <- plogis(as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                               betas) + attr(Xp, 'model.offset')))
      out <- mu * (1 - mu) / (1 + as.vector(family_pars$phi))
      } else {
     out <- plogis(as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                          betas) + attr(Xp, 'model.offset')))
    }
  }

  # Gamma observations (requires argument 'shape')
  if(family == 'Gamma'){
    if(type ==  'link'){
      out <- as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                          betas) + attr(Xp, 'model.offset'))

      if(density){
        out <- dgamma(truth,
                      rate = as.vector(family_pars$shape) / exp(out),
                      shape = as.vector(family_pars$shape),
                      log = TRUE)
      }

    } else if(type == 'response'){
      out <- rgamma(n = NROW(Xp),
                    rate = as.vector(family_pars$shape) /
                      exp(as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                                       betas) + attr(Xp, 'model.offset'))),
                    shape = as.vector(family_pars$shape))
    } else if(type == 'variance'){
      out <- as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                          betas) + attr(Xp, 'model.offset')) ^ 2
      } else {
      out <- as.vector(family_pars$shape) /
        (as.vector(family_pars$shape) / exp(as.vector((matrix(Xp, ncol = NCOL(Xp)) %*%
                                              betas) + attr(Xp, 'model.offset'))))
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
                                   phi = as.vector(family_pars$phi),
                                   all.derivs = F)[,1]
      }

    } else if(type == 'response'){
      out <- rpois(n = NROW(Xp),
                   lambda = mgcv::rTweedie(
                     mu = exp(((matrix(Xp, ncol = NCOL(Xp)) %*%
                                  betas)) +
                                attr(Xp, 'model.offset')),
                     # Power parameter is fixed
                     p = 1.5,
                     phi = as.vector(family_pars$phi)))
    } else if(type == 'variance'){
      out <- (exp(((matrix(Xp, ncol = NCOL(Xp)) %*%
                     betas)) +
                   attr(Xp, 'model.offset')) ^ 1.5) *
        as.vector(family_pars$phi)
      } else {
      out <- exp(((matrix(Xp, ncol = NCOL(Xp)) %*%
                     betas)) +
                   attr(Xp, 'model.offset'))
    }
  }
  return(out)
}

#' Set which family to use when calculating default intercept priors
#' in brms
#' @noRd
family_to_brmsfam = function(family){
  if(family$family == 'beta'){
    brms::Beta()
  } else if(family$family == 'Beta regression'){
    brms::Beta()
  } else if(family$family == 'student'){
    brms::student()
  } else if(family$family == 'negative binomial'){
    brms::negbinomial()
  } else if(family$family == 'Gamma'){
    Gamma(link = 'log')
  } else {
    family
  }
}

#' Set which family to use when setting up the gam object
#' Stick to Gaussian where possible to ensure the initial setup
#' doesn't fail
#' @noRd
family_to_mgcvfam = function(family){
  if(family$family == 'beta'){
    mgcv::betar()
  } else if(family$family == 'student'){
    gaussian()
  } else if(family$family %in% c('gamma', 'Gamma', 'lognormal')){
    gaussian()
  } else if(family$family == 'tweedie'){
    mgcv::Tweedie(p = 1.5, link = 'log')
  } else if(family$family == 'nmix'){
    poisson()
  } else if(family$family %in% c('bernoulli', 'beta_binomial')){
    binomial()
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
  if(family %in% c('Gamma', 'poisson',
                   'negative binomial',
                   'tweedie', 'nmix')){
    out <- 'log'
  }
  if(family %in% c('beta',
                   'binomial',
                   'bernoulli',
                   'beta_binomial')){
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
  if(family %in% c('beta', 'binomial', 'bernoulli',
                   'beta_binomial')){
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
                   'beta_binomial',
                   'negative binomial',
                   'tweedie')){
    out <- c('phi')
  }

  if(family %in% c('poisson',
                   'binomial',
                   'bernoulli')){
    out <- c()
  }

  if(family == 'nmix'){
    out <- c('p')
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
    if(model_data$num_basis == 1){
      list(b_raw = array(runif(model_data$num_basis, -2, 2)))
    } else {
      list(b_raw = runif(model_data$num_basis, -2, 2))
    }
  }

  return(inits)
}

#' Define which parameters to monitor / extract
#' @noRd
extract_family_pars = function(object, newdata = NULL){

  # Get names of parameters to extract
  pars_to_extract <- family_par_names(object$family)

  # Extract into a named list
  if(length(pars_to_extract) > 0){
    out <- vector(mode = 'list')
    for(i in 1:length(pars_to_extract)){
      out[[i]] <- mcmc_chains(object$model_output,
                                      params = pars_to_extract[i])
      if(NCOL(out[[i]]) == 1){
        out[[i]] <- as.vector(out[[i]])
      }
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

  if(family == 'beta_binomial'){
    prior_df <- data.frame(param_name = c('vector<lower=0>[n_series] phi;'),
                           param_length = length(unique(data$series)),
                           param_info = c('Beta Binomial precision parameter'),
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

  if(family == 'Gamma'){
    prior_df <- data.frame(param_name = c('vector<lower=0>[n_series] shape;'),
                           param_length = length(unique(data$series)),
                           param_info = c('Gamma shape parameter'),
                           prior = c('shape ~ gamma(0.01, 0.01);'),
                           example_change = c(
                             paste0(
                               'shape ~ normal(',
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

  if(family %in% c('nmix', 'poisson', 'binomial', 'bernoulli')){
    prior_df <- NULL
  }

  return(prior_df)
}

#' Family-specific Dunn-Smyth residual functions
#' @noRd
ds_resids_nmix = function(truth, fitted, draw,
                          p, N){
  na_obs <- is.na(truth)
  a_obs <- pbinom(ifelse(as.vector(truth[!na_obs]) - 1.e-6 > 0,
                         as.vector(truth[!na_obs]) - 1.e-6,
                         0),
                 size = N[!na_obs],
                 prob = p[!na_obs])
  b_obs <- pbinom(as.vector(truth[!na_obs]),
                  size = N[!na_obs],
                  prob = p[!na_obs])
  u_obs <- runif(n = length(truth[!na_obs]),
                 min = pmin(a_obs, b_obs),
                 max = pmax(a_obs, b_obs))

  if(any(is.na(truth))){
    u <- vector(length = length(truth))
    u[na_obs] <- NaN
    u[!na_obs] <- u_obs
  } else {
    u <- u_obs
  }
  dsres_out <- qnorm(u)
  dsres_out[is.infinite(dsres_out)] <- NaN
  dsres_out
}

#' @noRd
ds_resids_binomial = function(truth, fitted, draw, N){
  na_obs <- is.na(truth)
  a_obs <- pbinom(ifelse(as.vector(truth[!na_obs]) - 1.e-6 > 0,
                         as.vector(truth[!na_obs]) - 1.e-6,
                         0),
                  size = N[!na_obs],
                  prob = fitted[!na_obs])
  b_obs <- pbinom(as.vector(truth[!na_obs]),
                  size = N[!na_obs],
                  prob = fitted[!na_obs])
  u_obs <- runif(n = length(truth[!na_obs]),
                 min = pmin(a_obs, b_obs),
                 max = pmax(a_obs, b_obs))

  if(any(is.na(truth))){
    u <- vector(length = length(truth))
    u[na_obs] <- NaN
    u[!na_obs] <- u_obs
  } else {
    u <- u_obs
  }
  dsres_out <- qnorm(u)
  dsres_out[is.infinite(dsres_out)] <- NaN
  dsres_out
}

#' @noRd
ds_resids_beta_binomial = function(truth, fitted, draw, N, phi){
  na_obs <- is.na(truth)
  a_obs <- pbeta_binomial(ifelse(as.vector(truth[!na_obs]) - 1.e-6 > 0,
                                 as.vector(truth[!na_obs]) - 1.e-6,
                                 0),
                          size = N[!na_obs],
                          mu = fitted[!na_obs],
                          phi = phi[!na_obs])
  b_obs <- pbeta_binomial(ifelse(as.vector(truth[!na_obs]),
                                 as.vector(truth[!na_obs]),
                                 0),
                          size = N[!na_obs],
                          mu = fitted[!na_obs],
                          phi = phi[!na_obs])
  u_obs <- runif(n = length(truth[!na_obs]),
                 min = pmin(a_obs, b_obs),
                 max = pmax(a_obs, b_obs))

  if(any(is.na(truth))){
    u <- vector(length = length(truth))
    u[na_obs] <- NaN
    u[!na_obs] <- u_obs
  } else {
    u <- u_obs
  }
  dsres_out <- qnorm(u)
  dsres_out[is.infinite(dsres_out)] <- NaN
  dsres_out
}

#' @noRd
ds_resids_nb = function(truth, fitted, draw, size){
  na_obs <- is.na(truth)

  p <- size[!na_obs] / (fitted[!na_obs] + size[!na_obs])
  a_obs <- ifelse(as.vector(truth[!na_obs]) > 0,
              pbeta(p, size[!na_obs],
                    pmax(as.vector(truth[!na_obs]), 1)), 0)
  b_obs <- pbeta(p, size[!na_obs], as.vector(truth[!na_obs]) + 1)
  u_obs <- runif(n = length(truth[!na_obs]), min = a_obs, max = b_obs)

  if(any(is.na(truth))){
    u <- vector(length = length(truth))
    u[na_obs] <- NaN
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
                 shape1 = shape_pars$shape1[!na_obs],
                 shape2 = shape_pars$shape2[!na_obs])
  b_obs <- pbeta(as.vector(truth[!na_obs]),
                 shape1 = shape_pars$shape1[!na_obs],
                 shape2 = shape_pars$shape2[!na_obs])
  u_obs <- runif(n = length(truth[!na_obs]),
                 min = pmin(a_obs, b_obs),
                 max = pmax(a_obs, b_obs))

  if(any(is.na(truth))){
    u <- vector(length = length(truth))
    u[na_obs] <- NaN
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
  u_obs <- runif(n = length(truth[!na_obs]),
                 min = pmin(a_obs, b_obs),
                 max = pmax(a_obs, b_obs))

  if(any(is.na(truth))){
    u <- vector(length = length(truth))
    u[na_obs] <- NaN
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
  u_obs <- runif(n = length(truth[!na_obs]),
                 min = pmin(a_obs, b_obs), max = pmax(a_obs, b_obs))

  if(any(is.na(truth))){
    u <- vector(length = length(truth))
    u[na_obs] <- NaN
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
  u_obs <- runif(n = length(truth[!na_obs]),
                 min = pmin(a_obs, b_obs), max = pmax(a_obs, b_obs))

  if(any(is.na(truth))){
    u <- vector(length = length(truth))
    u[na_obs] <- NaN
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
                 meanlog = log(fitted[!na_obs]),
                 sdlog = sigma[!na_obs])
  b_obs <- plnorm(as.vector(truth[!na_obs]),
                 meanlog = log(fitted[!na_obs]),
                 sdlog = sigma[!na_obs])
  u_obs <- runif(n = length(truth[!na_obs]),
                 min = pmin(a_obs, b_obs), max = pmax(a_obs, b_obs))

  if(any(is.na(truth))){
    u <- vector(length = length(truth))
    u[na_obs] <- NaN
    u[!na_obs] <- u_obs
  } else {
    u <- u_obs
  }
  dsres_out <- qnorm(u)
  dsres_out[is.infinite(dsres_out)] <- NaN
  dsres_out
}

#' @noRd
ds_resids_gamma = function(truth, fitted, shape, draw){
  na_obs <- is.na(truth)
  a_obs <- pgamma(as.vector(truth[!na_obs]) - 1.e-6,
                  shape = shape[!na_obs],
                  rate = shape[!na_obs] / fitted[!na_obs])
  b_obs <- pgamma(as.vector(truth[!na_obs]),
                  shape = shape[!na_obs],
                  rate = shape[!na_obs] / fitted[!na_obs])
  u_obs <- runif(n = length(truth[!na_obs]),
                 min = pmin(a_obs, b_obs), max = pmax(a_obs, b_obs))

  if(any(is.na(truth))){
    u <- vector(length = length(truth))
    u[na_obs] <- NaN
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
                      df = nu[!na_obs],
                      mu = fitted[!na_obs],
                      sigma = sigma[!na_obs])
  b_obs <- pstudent_t(as.vector(truth[!na_obs]),
                      df = nu[!na_obs],
                      mu = fitted[!na_obs],
                      sigma = sigma[!na_obs])
  u_obs <- runif(n = length(truth[!na_obs]),
                 min = pmin(a_obs, b_obs), max = pmax(a_obs, b_obs))

  if(any(is.na(truth))){
    u <- vector(length = length(truth))
    u[na_obs] <- NaN
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
    size <- mcmc_chains(object$model_output, 'phi')
    series_residuals <- do.call(rbind, lapply(sample_seq, function(x){
      suppressWarnings(ds_resids_nb(truth = truth,
                                    fitted = preds[x, ],
                                    draw = preds[x, ],
                                    size = size[x]))
    }))
  }

  if(family == 'gaussian'){
    sigma <- mcmc_chains(object$model_output, 'sigma_obs')
    series_residuals <- do.call(rbind, lapply(sample_seq, function(x){
      suppressWarnings(ds_resids_gaus(truth = truth,
                                    fitted = preds[x, ],
                                    draw = preds[x, ],
                                    sigma = sigma[x]))
    }))
  }

  if(family == 'lognormal'){
    sigma <- mcmc_chains(object$model_output, 'sigma_obs')
    series_residuals <- do.call(rbind, lapply(sample_seq, function(x){
      suppressWarnings(ds_resids_lnorm(truth = truth,
                                      fitted = preds[x, ],
                                      draw = preds[x, ],
                                      sigma = sigma[x]))
    }))
  }

  if(family == 'student'){
    sigma <- mcmc_chains(object$model_output, 'sigma_obs')
    nu <- mcmc_chains(object$model_output, 'nu')
    series_residuals <- do.call(rbind, lapply(sample_seq, function(x){
      suppressWarnings(ds_resids_student(truth = truth,
                                       fitted = preds[x, ],
                                       draw = preds[x, ],
                                       sigma = sigma[x],
                                       nu = nu[x]))
    }))
  }

  if(family == 'beta'){
    precision <- mcmc_chains(object$model_output, 'phi')
    series_residuals <- do.call(rbind, lapply(sample_seq, function(x){
      suppressWarnings(ds_resids_beta(truth = truth,
                                       fitted = preds[x, ],
                                       draw = preds[x, ],
                                       precision = precision[x]))
    }))
  }

  if(family == 'Gamma'){
    shapes <- mcmc_chains(object$model_output, 'shape')
    series_residuals <- do.call(rbind, lapply(sample_seq, function(x){
      suppressWarnings(ds_resids_gamma(truth = truth,
                                      fitted = preds[x, ],
                                      draw = preds[x, ],
                                      shape = shapes[x]))
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
#' @importFrom parallel clusterExport stopCluster setDefaultCluster clusterEvalQ
#' @param object \code{list} object returned from \code{mvgam}
#' @param n_cores \code{integer} specifying number of cores for generating residual distributions in parallel
#' @author Nicholas J Clark
#' @details Dunn-Smyth residual distributions are calculated for each series in the fitted object
#' @return A \code{list} of residual distributions
#' @noRd
get_mvgam_resids = function(object, n_cores = 1){

  # Check arguments
  validate_pos_integer(n_cores)

  # Extract necessary model elements; for Stan models, expectations are
  # stored on the link scale
  preds <- hindcast(object, type = 'expected')
  if(object$family == 'nmix'){
    p <- mcmc_chains(object$model_output, 'detprob')
    N <- mcmc_chains(object$model_output, 'latent_ypred')
  }
  n_series <- NCOL(object$ytimes)
  obs_series <- object$obs_data$series
  series_levels <- levels(obs_series)
  family <- object$family
  obs_data <- object$obs_data
  fit_engine <- object$fit_engine

  # Create sequences of posterior draws for calculating residual distributions
  sample_seq <- 1:NROW(preds$hindcast[[1]])
  draw_seq <- sample(sample_seq, length(sample_seq), replace = FALSE)

  # Family-specific parameters
  family_pars <- extract_family_pars(object = object)

  # Calculate DS residual distributions in sequence (parallel is no faster)
  series_resids <- lapply(seq_len(n_series), function(series){
    if(class(obs_data)[1] == 'list'){
      n_obs <- data.frame(series = obs_series) %>%
        dplyr::filter(series == !!(series_levels[series])) %>%
        nrow()
    } else {
      n_obs <- obs_data %>%
        dplyr::filter(series == !!(series_levels[series])) %>%
        nrow()
    }

    preds <- preds$hindcasts[[series]]
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

    if(family == 'nmix'){
      N <- N[,1:length(truth)]
      p <- p[,1:length(truth)]
    }

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
                                      sigma = as.vector(sigma_mat)),
                       nrow = NROW(preds))
    }

    if(family == 'nmix'){
      resids <- matrix(ds_resids_nmix(truth = as.vector(truth_mat),
                                      fitted = 1,
                                      draw = 1,
                                      N = as.vector(N),
                                      p = as.vector(p)),
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

    if(family == 'Gamma'){
      shape <- family_pars$shape[,series]
      shape_mat <- matrix(rep(shape, NCOL(preds)),
                              ncol = NCOL(preds))
      resids <- matrix(ds_resids_gamma(truth = as.vector(truth_mat),
                                      fitted = as.vector(preds),
                                      draw = as.vector(preds[draw_seq,]),
                                      shape = as.vector(shape_mat)),
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

  })
  names(series_resids) <- series_levels
  return(series_resids)
}

#' @noRd
dsresids_vec = function(object){

  family <- object$family
  obs_series <- object$obs_data$series
  series_levels <- levels(obs_series)
  fit_engine <- object$fit_engine

  # Need to know which series each observation belongs to so we can
  # pull out appropriate family-level parameters (overdispersions, shapes, etc...)
  all_dat <- data.frame(series = object$obs_data$series,
                        time = object$obs_data$index..time..index,
                        y = object$obs_data$y) %>%
    dplyr::arrange(time, series)

  truth <- all_dat$y
  last_train <- NROW(all_dat)
  series_obs <- as.numeric(all_dat$series)

  # Extract expectations and necessary generated quantities
  # and subset to only include training data
  preds <- posterior_epred(object)[,1:last_train, drop = FALSE]

  if(family == 'nmix'){
    p <- mcmc_chains(object$model_output, 'detprob')[,1:last_train, drop = FALSE]
    N <- mcmc_chains(object$model_output, 'latent_ypred')[,1:last_train, drop = FALSE]
  }

  if(family %in% c('binomial', 'beta_binomial')){
    p <- plogis(mcmc_chains(object$model_output, 'mus')[,1:last_train,
                                                        drop = FALSE])
    N <- as.vector(attr(object$mgcv_model, 'trials'))[1:length(truth)]
  }

  # Family-specific parameters
  family_pars <- extract_family_pars(object = object)
  n_series <- NCOL(object$ytimes)

  # Family parameters spread into a vector
  family_extracts <- lapply(seq_along(family_pars), function(j){
    if(is.matrix(family_pars[[j]])){
      as.vector(family_pars[[j]][, series_obs])
    } else {
      as.vector(matrix(rep(family_pars[[j]],
                           NCOL(preds)),
                       nrow = NROW(preds),
                       byrow = FALSE))
    }
  })
  names(family_extracts) <- names(family_pars)

  # Create a truth matrix for vectorised residual computation
  truth_mat <- matrix(rep(truth, NROW(preds)),
                      nrow = NROW(preds),
                      byrow = TRUE)

  # Calculate DS residual distributions
  if(family == 'gaussian'){
    resids <- matrix(ds_resids_gaus(truth = as.vector(truth_mat),
                                    fitted = as.vector(preds),
                                    draw = 1,
                                    sigma = family_extracts$sigma_obs),
                     nrow = NROW(preds))
  }

  if(family == 'binomial'){
    N_mat <- matrix(rep(N, NROW(preds)),
                    nrow = NROW(preds),
                    byrow = TRUE)
    resids <- matrix(ds_resids_binomial(truth = as.vector(truth_mat),
                                        fitted = as.vector(p),
                                        draw = 1,
                                        N = as.vector(N_mat)),
                     nrow = NROW(preds))
  }

  if(family == 'beta_binomial'){
    N_mat <- matrix(rep(N, NROW(preds)),
                    nrow = NROW(preds),
                    byrow = TRUE)
    resids <- matrix(ds_resids_beta_binomial(truth = as.vector(truth_mat),
                                             fitted = as.vector(p),
                                             draw = 1,
                                             N = as.vector(N_mat),
                                             phi = family_extracts$phi),
                     nrow = NROW(preds))
  }

  if(family == 'bernoulli'){
    resids <- matrix(ds_resids_binomial(truth = as.vector(truth_mat),
                                        fitted = as.vector(preds),
                                        draw = 1,
                                        N = rep(1, length(truth_mat))),
                     nrow = NROW(preds))
  }

  if(family == 'nmix'){
    resids <- matrix(ds_resids_nmix(truth = as.vector(truth_mat),
                                    fitted = 1,
                                    draw = 1,
                                    N = as.vector(N),
                                    p = as.vector(p)),
                     nrow = NROW(preds))
  }

  if(family == 'student'){
    if(NCOL(mcmc_chains(object$model_output, 'sigma_obs')) == 1){
      sigma_obs <- family_extracts$sigma_obs
      sigma_mat <- matrix(rep(sigma_obs,
                              NCOL(truth_mat)),
                          ncol = NCOL(truth_mat))
      sigma_obs <- as.vector(sigma_mat)

      nu <- family_extracts$nu
      nu_mat <- matrix(rep(nu,
                              NCOL(truth_mat)),
                          ncol = NCOL(truth_mat))
      nu <- as.vector(nu_mat)
    } else {
      sigma_obs <- family_extracts$sigma_obs
      nu <- family_extracts$nu
    }
    resids <- matrix(ds_resids_student(truth = as.vector(truth_mat),
                                       fitted = as.vector(preds),
                                       draw = 1,
                                       sigma = sigma_obs,
                                       nu = nu),
                     nrow = NROW(preds))
  }

  if(family == 'lognormal'){
    if(NCOL(mcmc_chains(object$model_output, 'sigma_obs')) == 1){
      sigma_obs <- family_extracts$sigma_obs
      sigma_mat <- matrix(rep(sigma_obs,
                              NCOL(truth_mat)),
                          ncol = NCOL(truth_mat))
      sigma_obs <- as.vector(sigma_mat)
    } else {
      sigma_obs <- family_extracts$sigma_obs
    }
    resids <- matrix(ds_resids_lnorm(truth = as.vector(truth_mat),
                                     fitted = as.vector(preds),
                                     draw = 1,
                                     sigma = sigma_obs),
                     nrow = NROW(preds))
  }

  if(family == 'poisson'){
    resids <- matrix(ds_resids_pois(truth = as.vector(truth_mat),
                                    fitted = as.vector(preds),
                                    draw = 1),
                     nrow = NROW(preds))
  }

  if(family == 'beta'){
    resids <- matrix(ds_resids_beta(truth = as.vector(truth_mat),
                                    fitted = as.vector(preds),
                                    draw = 1,
                                    precision = family_extracts$phi),
                     nrow = NROW(preds))
  }

  if(family == 'Gamma'){
    if(NCOL(mcmc_chains(object$model_output, 'shape')) == 1){
      shape <- family_extracts$shape
      sigma_mat <- matrix(rep(shape,
                              NCOL(truth_mat)),
                          ncol = NCOL(truth_mat))
      shape <- as.vector(sigma_mat)
    } else {
      shape <- family_extracts$shape
    }
    resids <- matrix(ds_resids_gamma(truth = as.vector(truth_mat),
                                     fitted = as.vector(preds),
                                     draw = 1,
                                     shape = shape),
                     nrow = NROW(preds))
  }

  if(family == 'negative binomial'){
    resids <- matrix(ds_resids_nb(truth = as.vector(truth_mat),
                                  fitted = as.vector(preds),
                                  draw = 1,
                                  size = family_extracts$phi),
                     nrow = NROW(preds))
  }

  if(family == 'tweedie'){
    resids <- matrix(ds_resids_tw(truth = as.vector(truth_mat),
                                  fitted = as.vector(preds),
                                  draw = 1),
                     nrow = NROW(preds))
  }

  # Convert to a list of series-level matrices and return
  series_resids <- lapply(seq_len(n_series), function(series){
    inds_keep <- which(series_obs == series)
    resids[, inds_keep]
  })
  names(series_resids) <- levels(all_dat$series)

  return(series_resids)
}
