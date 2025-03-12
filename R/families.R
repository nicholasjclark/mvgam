#' Supported \pkg{mvgam} families
#' @importFrom stats make.link dgamma pgamma rgamma qnorm plnorm runif pbeta dlnorm dpois
#' @importFrom stats pnorm ppois plogis gaussian poisson Gamma dnbinom rnbinom dnorm dbeta
#' @importFrom stats binomial rbinom pbinom dbinom qbinom qlogis
#' @importFrom brms lognormal student beta_binomial bernoulli rstudent_t qstudent_t dstudent_t pstudent_t dbeta_binomial rbeta_binomial pbeta_binomial
#' @importFrom mgcv betar nb
#' @param link a specification for the family link function. At present these cannot
#' be changed
#' @param ... Arguments to be passed to the \pkg{mgcv} version of the associated functions
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
#'   \item \code{\link[brms]{beta_binomial}} with logit-link, as for `binomial()` but allows
#'   for overdispersion
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
#' @return Objects of class `family`
#' @details Note that currently it is not possible to change the default link
#' functions in `mvgam`, so any call to change these will be silently ignored
#' @author Nicholas J Clark
#'
NULL

#' @rdname mvgam_families
#' @export
tweedie = function(link = 'log') {
  linktemp <- make.link('log')
  structure(
    list(
      family = "tweedie",
      link = 'log',
      linkfun = linktemp$linkfun,
      linkinv = linktemp$linkinv,
      mu.eta = linktemp$mu.eta,
      valideta = linktemp$valideta
    ),
    class = c("extended.family", "family")
  )
}

#' @rdname mvgam_families
#' @export
student_t = function(link = 'identity') {
  linktemp <- make.link('identity')
  structure(
    list(
      family = "student",
      link = 'identity',
      linkfun = linktemp$linkfun,
      linkinv = linktemp$linkinv,
      mu.eta = linktemp$mu.eta,
      valideta = linktemp$valideta
    ),
    class = c("extended.family", "family")
  )
}

#' @rdname mvgam_families
#' @export
betar = function(...) {
  mgcv::betar(...)
}

#' @rdname mvgam_families
#' @export
nb = function(...) {
  mgcv::nb(...)
}

#' @rdname mvgam_families
#' @export
lognormal = function(...) {
  brms::lognormal(...)
}

#' @rdname mvgam_families
#' @export
student = function(...) {
  brms::student(...)
}

#' @rdname mvgam_families
#' @export
bernoulli = function(...) {
  brms::bernoulli(...)
}

#' @rdname mvgam_families
#' @export
beta_binomial = function(...) {
  brms::beta_binomial(...)
}

#' @rdname mvgam_families
#' @examples
#' \donttest{
#' # Example showing how to set up N-mixture models
#' set.seed(999)
#'# Simulate observations for species 1, which shows a declining trend and 0.7 detection probability
#'data.frame(site = 1,
#'           # five replicates per year; six years
#'           replicate = rep(1:5, 6),
#'           time = sort(rep(1:6, 5)),
#'           species = 'sp_1',
#'           # true abundance declines nonlinearly
#'           truth = c(rep(28, 5),
#'                     rep(26, 5),
#'                     rep(23, 5),
#'                     rep(16, 5),
#'                     rep(14, 5),
#'                     rep(14, 5)),
#'           # observations are taken with detection prob = 0.7
#'           obs = c(rbinom(5, 28, 0.7),
#'                   rbinom(5, 26, 0.7),
#'                   rbinom(5, 23, 0.7),
#'                   rbinom(5, 15, 0.7),
#'                   rbinom(5, 14, 0.7),
#'                   rbinom(5, 14, 0.7))) %>%
#'  # add 'series' information, which is an identifier of site, replicate and species
#'  dplyr::mutate(series = paste0('site_', site,
#'                                '_', species,
#'                                '_rep_', replicate),
#'                time = as.numeric(time),
#'                # add a 'cap' variable that defines the maximum latent N to
#'                # marginalize over when estimating latent abundance; in other words
#'                # how large do we realistically think the true abundance could be?
#'                cap = 80) %>%
#'  dplyr::select(- replicate) -> testdat
#'
#'# Now add another species that has a different temporal trend and a smaller
#'# detection probability (0.45 for this species)
#'testdat = testdat %>%
#'  dplyr::bind_rows(data.frame(site = 1,
#'                              replicate = rep(1:5, 6),
#'                              time = sort(rep(1:6, 5)),
#'                              species = 'sp_2',
#'                              truth = c(rep(4, 5),
#'                                        rep(7, 5),
#'                                        rep(15, 5),
#'                                        rep(16, 5),
#'                                        rep(19, 5),
#'                                        rep(18, 5)),
#'                              obs = c(rbinom(5, 4, 0.45),
#'                                      rbinom(5, 7, 0.45),
#'                                      rbinom(5, 15, 0.45),
#'                                      rbinom(5, 16, 0.45),
#'                                      rbinom(5, 19, 0.45),
#'                                      rbinom(5, 18, 0.45))) %>%
#'                     dplyr::mutate(series = paste0('site_', site,
#'                                                   '_', species,
#'                                                   '_rep_', replicate),
#'                                   time = as.numeric(time),
#'                                   cap = 50) %>%
#'                     dplyr::select(-replicate))
#'
#' # series identifiers
#' testdat$species <- factor(testdat$species,
#'                           levels = unique(testdat$species))
#' testdat$series <- factor(testdat$series,
#'                          levels = unique(testdat$series))
#'
#' # The trend_map to state how replicates are structured
#' testdat %>%
#' # each unique combination of site*species is a separate process
#'dplyr::mutate(trend = as.numeric(factor(paste0(site, species)))) %>%
#'  dplyr::select(trend, series) %>%
#'  dplyr::distinct() -> trend_map
#'trend_map
#'
#' # Fit a model
#' mod <- mvgam(
#'             # the observation formula sets up linear predictors for
#'             # detection probability on the logit scale
#'             formula = obs ~ species - 1,
#'
#'             # the trend_formula sets up the linear predictors for
#'             # the latent abundance processes on the log scale
#'             trend_formula = ~ s(time, by = trend, k = 4) + species,
#'
#'             # the trend_map takes care of the mapping
#'             trend_map = trend_map,
#'
#'             # nmix() family and data
#'             family = nmix(),
#'             data = testdat,
#'
#'             # priors can be set in the usual way
#'             priors = c(prior(std_normal(), class = b),
#'                        prior(normal(1, 1.5), class = Intercept_trend)),
#'             chains = 2)
#'
#' # The usual diagnostics
#' summary(mod)
#'
#' # Plotting conditional effects
#' library(ggplot2); library(marginaleffects)
#' plot_predictions(mod, condition = 'species',
#'                  type = 'detection') +
#'      ylab('Pr(detection)') +
#'      ylim(c(0, 1)) +
#'      theme_classic() +
#'      theme(legend.position = 'none')
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
#' }
#' @export
nmix = function(link = 'log') {
  linktemp <- make.link('log')
  structure(
    list(
      family = "nmix",
      link = 'log',
      linkfun = linktemp$linkfun,
      linkinv = linktemp$linkinv,
      mu.eta = linktemp$mu.eta,
      valideta = linktemp$valideta
    ),
    class = c("extended.family", "family")
  )
}

#### Non-exported functions for performing family-specific tasks ####
#' Family options in character format
#' @noRd
family_char_choices = function() {
  c(
    'negative binomial',
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
    "Gamma"
  )
}

# Convert location / precision parameters to shape parameters for the beta distribution
# Original author: Andrew Heiss (https://www.andrewheiss.com/blog/2021/11/08/beta-regression-guide/)
#' @noRd
beta_shapes = function(mu, phi) {
  return(list(shape1 = mu * phi, shape2 = (1 - mu) * phi))
}

# Calculate all possible Poisson log-densities for N-mixture simulation
#' @noRd
pois_dens = function(min_cap, max_cap, lambdas) {
  # Identify which indices share the exact same lambda AND
  # k value so that we only need to run dpois once for each group
  data.frame(lambdas, min_cap, max_cap) %>%
    dplyr::group_by(lambdas) %>%
    dplyr::summarise(
      min_cap = min(min_cap, na.rm = TRUE),
      max_cap = max(max_cap, na.rm = TRUE)
    ) -> group_inds

  l <- mapply(`:`, group_inds$min_cap, group_inds$max_cap)

  data.frame(
    k = unlist(l),
    lambda = group_inds$lambdas[rep(1:nrow(group_inds), lengths(l))]
  ) %>%
    dplyr::mutate(pois_dens = dpois(k, lambda, log = TRUE)) -> all_ks

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
mvgam_predict = function(
  Xp,
  family,
  betas,
  latent_lambdas,
  cap,
  min_cap,
  type = 'link',
  family_pars,
  density = FALSE,
  truth = NULL
) {
  if (type == 'latent_N' & family != 'nmix') {
    stop('"latent_N" type only available for N-mixture models', call. = FALSE)
  }

  if (type == 'detection' & family != 'nmix') {
    stop('"detection" type only available for N-mixture models', call. = FALSE)
  }

  # Poisson-Binomial N-Mixture (requires family parameter
  # 'cap' as well as 'latent_lambdas' argument)
  if (family == 'nmix') {
    insight::check_if_installed(
      "extraDistr",
      reason = 'to simulate from N-Mixture distributions'
    )
    insight::check_if_installed(
      "wrswoR",
      reason = 'to simulate from N-Mixture distributions'
    )
    # Calculate detection probability and convert to probability scale
    p <- as.vector(
      (matrix(Xp, ncol = NCOL(Xp)) %*%
        betas) +
        attr(Xp, 'model.offset')
    )
    p <- plogis(p)

    # Latent mean of State vector
    lambdas <- as.vector(latent_lambdas)

    # User-specified cap on latent abundance
    cap <- as.vector(cap)

    if (type == 'detection') {
      out <- p
    } else if (type == 'link') {
      # 'link' predictions are expectations of the latent abundance
      out <- lambdas

      if (density) {
        out <- unlist(
          lapply(seq_along(truth), function(i) {
            if (is.na(truth[i])) {
              output <- NA
            } else {
              ks <- truth[i]:cap[i]
              lik_binom <- dbinom(truth[i], size = ks, prob = p[i], log = TRUE)
              lik_poisson <- dpois(x = ks, lambda = lambdas[i], log = TRUE)
              loglik <- lik_binom + lik_poisson
              output <- log_sum_exp(loglik)
            }
            output
          }),
          use.names = FALSE
        )
      }
    } else if (type == 'latent_N') {
      if (missing(min_cap)) min_cap <- 0
      min_cap <- as.vector(min_cap)
      if (missing(truth)) {
        out <- extraDistr::rtpois(
          n = length(lambdas),
          lambda = lambdas,
          a = min_cap,
          b = cap
        )
      } else {
        # If true observed N is supplied, we can calculate the
        # most likely latent N given the covariates and the estimated
        # detection probability
        out <- unlist(
          lapply(seq_along(truth), function(i) {
            if (is.na(truth[i])) {
              output <- NA
            } else {
              ks <- min_cap[[i]]:cap[[i]]
              lik <- exp(
                dbinom(truth[[i]], size = ks, prob = p[[i]], log = TRUE) +
                  dpois(x = ks, lambda = lambdas[i], log = TRUE)
              )
              probs <- lik / sum(lik)
              probs[!is.finite(probs)] <- 0
              output <- ks[wrswoR::sample_int_ccrank(
                length(ks),
                size = 1L,
                prob = probs
              )]
            }
            output
          }),
          use.names = FALSE
        )
      }
    } else if (type == 'response') {
      xpred <- extraDistr::rtpois(
        n = length(lambdas),
        lambda = lambdas,
        b = cap
      )
      out <- rbinom(length(lambdas), size = xpred, prob = p)
    } else if (type == 'variance') {
      xpred <- extraDistr::rtpois(
        n = length(lambdas),
        lambda = lambdas,
        b = cap
      )

      # Variance of a Binomial distribution using the
      # weights convention from stats::glm()
      mu <- p / xpred
      out <- mu * (1 - mu)
    } else {
      # Expectations
      xpred <- extraDistr::rtpois(
        n = length(lambdas),
        lambda = lambdas,
        b = cap
      )
      out <- xpred * p
    }
  }

  # Gaussian observations (requires family parameter 'sigma_obs')
  if (family == 'gaussian') {
    if (type %in% c('link', 'expected')) {
      out <- as.vector(
        (matrix(Xp, ncol = NCOL(Xp)) %*%
          betas) +
          attr(Xp, 'model.offset')
      )
      if (density) {
        out <- dnorm(
          truth,
          mean = out,
          sd = as.vector(family_pars$sigma_obs),
          log = TRUE
        )
      }
    } else if (type == 'variance') {
      out <- rep.int(1, NROW(Xp))
    } else {
      out <- rnorm(
        n = NROW(Xp),
        mean = ((matrix(Xp, ncol = NCOL(Xp)) %*%
          betas)) +
          attr(Xp, 'model.offset'),
        sd = as.vector(family_pars$sigma_obs)
      )
    }
  }

  # LogNormal observations (requires family parameter 'sigma_obs')
  if (family == 'lognormal') {
    if (type == 'link') {
      out <- as.vector(
        (matrix(Xp, ncol = NCOL(Xp)) %*%
          betas) +
          attr(Xp, 'model.offset')
      )
      if (density) {
        out <- dlnorm(
          truth,
          meanlog = out,
          sdlog = as.vector(family_pars$sigma_obs),
          log = TRUE
        )
      }
    } else if (type == 'response') {
      out <- rlnorm(
        n = NROW(Xp),
        meanlog = ((matrix(Xp, ncol = NCOL(Xp)) %*%
          betas)) +
          attr(Xp, 'model.offset'),
        sdlog = as.vector(family_pars$sigma_obs)
      )
    } else if (type == 'variance') {
      mu <- ((matrix(Xp, ncol = NCOL(Xp)) %*%
        betas)) +
        attr(Xp, 'model.offset')
      sd <- as.vector(family_pars$sigma_obs)
      out <- as.vector((exp((sd)^2) - 1) * exp((2 * mu + sd^2)))
    } else {
      mu <- as.vector(
        (matrix(Xp, ncol = NCOL(Xp)) %*%
          betas) +
          attr(Xp, 'model.offset')
      )
      out <- exp(mu + (as.vector(family_pars$sigma_obs)^2 / 2))
    }
  }

  # Student-T observations (requires family parameters 'nu', 'sigma_obs')
  if (family == 'student') {
    if (type %in% c('link', 'expected')) {
      out <- as.vector(
        (matrix(Xp, ncol = NCOL(Xp)) %*%
          betas) +
          attr(Xp, 'model.offset')
      )
      if (density) {
        out <- dstudent_t(
          truth,
          df = as.vector(family_pars$nu),
          mu = out,
          sigma = as.vector(family_pars$sigma_obs),
          log = TRUE
        )
      }
    } else if (type == 'variance') {
      out <- as.vector(family_pars$sigma_obs)^2 *
        as.vector(family_pars$nu) /
        pmax(1.01, (as.vector(family_pars$nu) - 2))
    } else {
      out <- rstudent_t(
        n = NROW(Xp),
        df = family_pars$nu,
        mu = ((matrix(Xp, ncol = NCOL(Xp)) %*%
          betas)) +
          attr(Xp, 'model.offset'),
        sigma = as.vector(family_pars$sigma_obs)
      )
    }
  }

  # Poisson observations
  if (family == 'poisson') {
    if (type == 'link') {
      out <- as.vector(
        (matrix(Xp, ncol = NCOL(Xp)) %*%
          betas) +
          attr(Xp, 'model.offset')
      )
      if (density) {
        out <- dpois(truth, lambda = exp(out), log = TRUE)
      }
    } else if (type == 'response') {
      out <- rpois(
        n = NROW(Xp),
        lambda = exp(
          ((matrix(Xp, ncol = NCOL(Xp)) %*%
            betas)) +
            attr(Xp, 'model.offset')
        )
      )
    } else if (type == 'variance') {
      out <- exp(as.vector(
        (matrix(Xp, ncol = NCOL(Xp)) %*%
          betas) +
          attr(Xp, 'model.offset')
      ))
    } else {
      out <- exp(as.vector(
        ((matrix(Xp, ncol = NCOL(Xp)) %*%
          betas)) +
          attr(Xp, 'model.offset')
      ))
    }
  }

  # Bernoulli observations
  if (family == 'bernoulli') {
    if (type == 'link') {
      out <- as.vector(
        (matrix(Xp, ncol = NCOL(Xp)) %*%
          betas) +
          attr(Xp, 'model.offset')
      )

      if (density) {
        out <- dbinom(truth, prob = plogis(out), size = 1, log = TRUE)
      }
    } else if (type == 'response') {
      out <- rbinom(
        n = NROW(Xp),
        prob = plogis(
          ((matrix(Xp, ncol = NCOL(Xp)) %*%
            betas)) +
            attr(Xp, 'model.offset')
        ),
        size = 1
      )
    } else if (type == 'variance') {
      mu <- plogis(as.vector(
        (matrix(Xp, ncol = NCOL(Xp)) %*%
          betas) +
          attr(Xp, 'model.offset')
      ))
      out <- mu * (1 - mu)
    } else {
      out <- plogis(as.vector(
        (matrix(Xp, ncol = NCOL(Xp)) %*%
          betas) +
          attr(Xp, 'model.offset')
      ))
    }
  }

  # Binomial observations (requires argument 'trials')
  if (family == 'binomial') {
    if (type == 'link') {
      out <- as.vector(
        (matrix(Xp, ncol = NCOL(Xp)) %*%
          betas) +
          attr(Xp, 'model.offset')
      )

      if (density) {
        out <- dbinom(
          truth,
          prob = plogis(out),
          size = as.vector(family_pars$trials),
          log = TRUE
        )
      }
    } else if (type == 'response') {
      out <- rbinom(
        n = NROW(Xp),
        prob = plogis(
          ((matrix(Xp, ncol = NCOL(Xp)) %*%
            betas)) +
            attr(Xp, 'model.offset')
        ),
        size = as.vector(family_pars$trials)
      )
    } else if (type == 'variance') {
      mu <- plogis(as.vector(
        (matrix(Xp, ncol = NCOL(Xp)) %*%
          betas) +
          attr(Xp, 'model.offset')
      )) /
        as.vector(family_pars$trials)
      out <- mu * (1 - mu)
    } else {
      out <- plogis(
        ((matrix(Xp, ncol = NCOL(Xp)) %*%
          betas)) +
          attr(Xp, 'model.offset')
      ) *
        as.vector(family_pars$trials)
    }
  }

  # Beta_Binomial observations (requires arguments 'trials' and 'phi')
  if (family == 'beta_binomial') {
    if (type == 'link') {
      out <- as.vector(
        (matrix(Xp, ncol = NCOL(Xp)) %*%
          betas) +
          attr(Xp, 'model.offset')
      )

      if (density) {
        out <- dbeta_binomial(
          truth,
          mu = plogis(out),
          phi = as.vector(family_pars$phi),
          size = as.vector(family_pars$trials),
          log = TRUE
        )
      }
    } else if (type == 'response') {
      out <- rbeta_binomial(
        n = NROW(Xp),
        mu = plogis(
          ((matrix(Xp, ncol = NCOL(Xp)) %*%
            betas)) +
            attr(Xp, 'model.offset')
        ),
        phi = as.vector(family_pars$phi),
        size = as.vector(family_pars$trials)
      )
    } else if (type == 'variance') {
      mu <- plogis(as.vector(
        (matrix(Xp, ncol = NCOL(Xp)) %*%
          betas) +
          attr(Xp, 'model.offset')
      ))
      # https://en.wikipedia.org/wiki/Beta-binomial_distribution
      alpha <- mu * as.vector(family_pars$phi)
      beta <- (1 - mu) * as.vector(family_pars$phi)
      p <- 1 / (alpha + beta + 1)
      n <- as.vector(family_pars$trials)
      out <- ((n * p) * (1 - p)) * ((alpha + beta + n) / (alpha + beta + 1))
    } else {
      out <- as.vector(
        plogis(
          ((matrix(Xp, ncol = NCOL(Xp)) %*%
            betas)) +
            attr(Xp, 'model.offset')
        ) *
          as.vector(family_pars$trials)
      )
    }
  }

  # Negative Binomial observations (requires argument 'phi')
  if (family == 'negative binomial') {
    if (type == 'link') {
      out <- as.vector(
        (matrix(Xp, ncol = NCOL(Xp)) %*%
          betas) +
          attr(Xp, 'model.offset')
      )

      if (density) {
        out <- dnbinom(
          truth,
          mu = exp(out),
          size = as.vector(family_pars$phi),
          log = TRUE
        )
      }
    } else if (type == 'response') {
      out <- rnbinom(
        n = NROW(Xp),
        mu = exp(
          ((matrix(Xp, ncol = NCOL(Xp)) %*%
            betas)) +
            attr(Xp, 'model.offset')
        ),
        size = as.vector(family_pars$phi)
      )
    } else if (type == 'variance') {
      mu <- exp(as.vector(
        (matrix(Xp, ncol = NCOL(Xp)) %*%
          betas) +
          attr(Xp, 'model.offset')
      ))
      out <- mu + mu^2 / as.vector(family_pars$phi)
    } else {
      out <- as.vector(exp(
        ((matrix(Xp, ncol = NCOL(Xp)) %*%
          betas)) +
          attr(Xp, 'model.offset')
      ))
    }
  }

  # Beta observations (requires argument 'phi')
  if (family == 'beta') {
    shape_pars <- beta_shapes(
      mu = plogis(as.vector(
        (matrix(Xp, ncol = NCOL(Xp)) %*%
          betas) +
          attr(Xp, 'model.offset')
      )),
      phi = as.vector(family_pars$phi)
    )
    if (type == 'link') {
      out <- as.vector(
        (matrix(Xp, ncol = NCOL(Xp)) %*%
          betas) +
          attr(Xp, 'model.offset')
      )

      if (density) {
        out <- dbeta(
          truth,
          shape1 = shape_pars$shape1,
          shape2 = shape_pars$shape2,
          log = TRUE
        )
      }
    } else if (type == 'response') {
      out <- rbeta(
        n = NROW(Xp),
        shape1 = shape_pars$shape1,
        shape2 = shape_pars$shape2
      )
    } else if (type == 'variance') {
      mu <- plogis(as.vector(
        (matrix(Xp, ncol = NCOL(Xp)) %*%
          betas) +
          attr(Xp, 'model.offset')
      ))
      out <- mu * (1 - mu) / (1 + as.vector(family_pars$phi))
    } else {
      out <- plogis(as.vector(
        (matrix(Xp, ncol = NCOL(Xp)) %*%
          betas) +
          attr(Xp, 'model.offset')
      ))
    }
  }

  # Gamma observations (requires argument 'shape')
  if (family == 'Gamma') {
    if (type == 'link') {
      out <- as.vector(
        (matrix(Xp, ncol = NCOL(Xp)) %*%
          betas) +
          attr(Xp, 'model.offset')
      )

      if (density) {
        out <- dgamma(
          truth,
          rate = as.vector(family_pars$shape) / exp(out),
          shape = as.vector(family_pars$shape),
          log = TRUE
        )
      }
    } else if (type == 'response') {
      out <- rgamma(
        n = NROW(Xp),
        rate = as.vector(family_pars$shape) /
          exp(as.vector(
            (matrix(Xp, ncol = NCOL(Xp)) %*%
              betas) +
              attr(Xp, 'model.offset')
          )),
        shape = as.vector(family_pars$shape)
      )
    } else if (type == 'variance') {
      out <- as.vector(
        (matrix(Xp, ncol = NCOL(Xp)) %*%
          betas) +
          attr(Xp, 'model.offset')
      )^2
    } else {
      out <- as.vector(family_pars$shape) /
        (as.vector(family_pars$shape) /
          exp(as.vector(
            (matrix(Xp, ncol = NCOL(Xp)) %*%
              betas) +
              attr(Xp, 'model.offset')
          )))
    }
  }

  # Tweedie observations (requires argument 'phi')
  if (family == 'tweedie') {
    if (type == 'link') {
      out <- as.vector(
        (matrix(Xp, ncol = NCOL(Xp)) %*%
          betas) +
          attr(Xp, 'model.offset')
      )

      if (density) {
        out <- mgcv::ldTweedie(
          y = truth,
          mu = exp(out),
          # Power parameter is fixed
          p = 1.5,
          phi = as.vector(family_pars$phi),
          all.derivs = F
        )[, 1]
      }
    } else if (type == 'response') {
      out <- rpois(
        n = NROW(Xp),
        lambda = mgcv::rTweedie(
          mu = exp(
            ((matrix(Xp, ncol = NCOL(Xp)) %*%
              betas)) +
              attr(Xp, 'model.offset')
          ),
          # Power parameter is fixed
          p = 1.5,
          phi = as.vector(family_pars$phi)
        )
      )
    } else if (type == 'variance') {
      out <- (exp(
        ((matrix(Xp, ncol = NCOL(Xp)) %*%
          betas)) +
          attr(Xp, 'model.offset')
      )^1.5) *
        as.vector(family_pars$phi)
    } else {
      out <- as.vector(exp(
        ((matrix(Xp, ncol = NCOL(Xp)) %*%
          betas)) +
          attr(Xp, 'model.offset')
      ))
    }
  }
  return(out)
}

#' Set which family to use when calculating default intercept priors
#' in brms
#' @noRd
family_to_brmsfam = function(family) {
  if (family$family == 'beta') {
    brms::Beta()
  } else if (family$family == 'Beta regression') {
    brms::Beta()
  } else if (family$family == 'student') {
    brms::student()
  } else if (family$family %in% c('tweedie', 'negative binomial')) {
    brms::negbinomial()
  } else if (family$family == 'Gamma') {
    Gamma(link = 'log')
  } else {
    family
  }
}

#' Set which family to use when setting up the gam object
#' Stick to Gaussian where possible to ensure the initial setup
#' doesn't fail
#' @noRd
family_to_mgcvfam = function(family) {
  if (family$family == 'beta') {
    mgcv::betar()
  } else if (family$family == 'student') {
    gaussian()
  } else if (family$family %in% c('gamma', 'Gamma', 'lognormal')) {
    gaussian()
  } else if (family$family == 'tweedie') {
    mgcv::Tweedie(p = 1.5, link = 'log')
  } else if (family$family == 'nmix') {
    poisson()
  } else if (family$family %in% c('bernoulli', 'beta_binomial')) {
    binomial()
  } else {
    family
  }
}

#' Set which family to use when setting up the jagam object
#' @noRd
family_to_jagamfam = function(family) {
  if (family %in% c('gaussian', 'student')) {
    gaussian()
  } else {
    poisson()
  }
}

#' Define links used for the mean
#' @noRd
family_links = function(family) {
  if (family %in% c('gaussian', 'lognormal', 'student')) {
    out <- 'identity'
  }
  if (
    family %in% c('Gamma', 'poisson', 'negative binomial', 'tweedie', 'nmix')
  ) {
    out <- 'log'
  }
  if (family %in% c('beta', 'binomial', 'bernoulli', 'beta_binomial')) {
    out <- 'logit'
  }
  out
}

#' @noRd
family_invlinks = function(family) {
  if (family %in% c('gaussian', 'lognormal', 'student')) {
    out <- function(x) {
      x
    }
  }
  if (family %in% c('Gamma', 'poisson', 'negative binomial', 'tweedie')) {
    out <- function(x) {
      exp(x)
    }
  }
  if (family %in% c('beta', 'binomial', 'bernoulli', 'beta_binomial')) {
    out <- function(x) {
      plogis(x)
    }
  }
  out
}

#' Parameters to monitor / extract depending on the observation family
#' @noRd
family_par_names = function(family) {
  if (family %in% c('gaussian', 'lognormal')) {
    out <- c('sigma_obs')
  }

  if (family == 'student') {
    out <- c('sigma_obs', 'nu')
  }

  if (family == 'Gamma') {
    out <- c('shape')
  }

  if (family %in% c('beta', 'beta_binomial', 'negative binomial', 'tweedie')) {
    out <- c('phi')
  }

  if (family %in% c('poisson', 'binomial', 'bernoulli')) {
    out <- c()
  }

  if (family == 'nmix') {
    out <- c('detprob')
  }

  return(out)
}

#' Define which parameters to monitor / extract
#' @noRd
extract_family_pars = function(object, newdata = NULL) {
  # Get names of parameters to extract
  pars_to_extract <- family_par_names(object$family)

  # Extract into a named list
  if (length(pars_to_extract) > 0) {
    out <- vector(mode = 'list')
    for (i in 1:length(pars_to_extract)) {
      out[[i]] <- mcmc_chains(object$model_output, params = pars_to_extract[i])
      if (NCOL(out[[i]]) == 1) {
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
family_prior_info = function(family, use_stan, data) {
  if (family == 'gaussian') {
    prior_df <- data.frame(
      param_name = c('vector<lower=0>[n_series] sigma_obs;'),
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
        )
      )
    )
  }

  if (family == 'lognormal') {
    prior_df <- data.frame(
      param_name = c('vector<lower=0>[n_series] sigma_obs;'),
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
        )
      )
    )
  }

  if (family == 'student') {
    prior_df <- data.frame(
      param_name = c(
        'vector<lower=0>[n_series] sigma_obs;',
        'vector<lower=0>[n_series] nu;'
      ),
      param_length = rep(length(unique(data$series)), 2),
      param_info = c('observation error sd', 'observation degrees of freedom'),
      prior = c('sigma_obs ~ student_t(3, 0, 2);', 'nu ~ gamma(2, 0.1);'),
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
        )
      )
    )
  }

  if (family == 'beta') {
    prior_df <- data.frame(
      param_name = c('vector<lower=0>[n_series] phi;'),
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
        )
      )
    )
  }

  if (family == 'beta_binomial') {
    prior_df <- data.frame(
      param_name = c('vector<lower=0>[n_series] phi;'),
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
        )
      )
    )
  }

  if (family == 'Gamma') {
    prior_df <- data.frame(
      param_name = c('vector<lower=0>[n_series] shape;'),
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
        )
      )
    )
  }

  if (family == 'negative binomial') {
    if (use_stan) {
      prior_df <- data.frame(
        param_name = c('vector<lower=0>[n_series] phi_inv;'),
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
          )
        )
      )
    } else {
      prior_df <- data.frame(
        param_name = c('vector<lower=0>[n_series] phi_inv;'),
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
          )
        )
      )
    }
  }

  if (family == 'tweedie') {
    prior_df <- data.frame(
      param_name = c('vector<lower=0>[n_series] phi_raw;'),
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
        )
      )
    )
  }

  if (family %in% c('nmix', 'poisson', 'binomial', 'bernoulli')) {
    prior_df <- NULL
  }

  return(prior_df)
}

#' Family-specific Dunn-Smyth residual functions
#' @noRd
ds_resids_nmix = function(truth, fitted, draw, p, N) {
  na_obs <- is.na(truth)
  a_obs <- pbinom(
    ifelse(
      as.vector(truth[!na_obs]) - 1.e-6 > 0,
      as.vector(truth[!na_obs]) - 1.e-6,
      0
    ),
    size = N[!na_obs],
    prob = p[!na_obs]
  )
  b_obs <- pbinom(
    as.vector(truth[!na_obs]),
    size = N[!na_obs],
    prob = p[!na_obs]
  )
  u_obs <- runif(
    n = length(truth[!na_obs]),
    min = pmin(a_obs, b_obs),
    max = pmax(a_obs, b_obs)
  )

  if (any(is.na(truth))) {
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
ds_resids_binomial = function(truth, fitted, draw, N) {
  na_obs <- is.na(truth)
  a_obs <- pbinom(
    ifelse(
      as.vector(truth[!na_obs]) - 1.e-6 > 0,
      as.vector(truth[!na_obs]) - 1.e-6,
      0
    ),
    size = N[!na_obs],
    prob = fitted[!na_obs]
  )
  b_obs <- pbinom(
    as.vector(truth[!na_obs]),
    size = N[!na_obs],
    prob = fitted[!na_obs]
  )
  u_obs <- runif(
    n = length(truth[!na_obs]),
    min = pmin(a_obs, b_obs),
    max = pmax(a_obs, b_obs)
  )

  if (any(is.na(truth))) {
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
ds_resids_beta_binomial = function(truth, fitted, draw, N, phi) {
  na_obs <- is.na(truth)
  a_obs <- pbeta_binomial(
    ifelse(
      as.vector(truth[!na_obs]) - 1.e-6 > 0,
      as.vector(truth[!na_obs]) - 1.e-6,
      0
    ),
    size = N[!na_obs],
    mu = fitted[!na_obs],
    phi = phi[!na_obs]
  )
  b_obs <- pbeta_binomial(
    ifelse(as.vector(truth[!na_obs]), as.vector(truth[!na_obs]), 0),
    size = N[!na_obs],
    mu = fitted[!na_obs],
    phi = phi[!na_obs]
  )
  u_obs <- runif(
    n = length(truth[!na_obs]),
    min = pmin(a_obs, b_obs),
    max = pmax(a_obs, b_obs)
  )

  if (any(is.na(truth))) {
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
ds_resids_nb = function(truth, fitted, draw, size) {
  na_obs <- is.na(truth)

  p <- size[!na_obs] / (fitted[!na_obs] + size[!na_obs])
  a_obs <- ifelse(
    as.vector(truth[!na_obs]) > 0,
    pbeta(p, size[!na_obs], pmax(as.vector(truth[!na_obs]), 1)),
    0
  )
  b_obs <- pbeta(p, size[!na_obs], as.vector(truth[!na_obs]) + 1)
  u_obs <- runif(n = length(truth[!na_obs]), min = a_obs, max = b_obs)

  if (any(is.na(truth))) {
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
ds_resids_beta = function(truth, fitted, draw, precision) {
  shape_pars <- beta_shapes(mu = fitted, phi = precision)
  na_obs <- is.na(truth)
  a_obs <- pbeta(
    as.vector(truth[!na_obs]) - 1.e-6,
    shape1 = shape_pars$shape1[!na_obs],
    shape2 = shape_pars$shape2[!na_obs]
  )
  b_obs <- pbeta(
    as.vector(truth[!na_obs]),
    shape1 = shape_pars$shape1[!na_obs],
    shape2 = shape_pars$shape2[!na_obs]
  )
  u_obs <- runif(
    n = length(truth[!na_obs]),
    min = pmin(a_obs, b_obs),
    max = pmax(a_obs, b_obs)
  )

  if (any(is.na(truth))) {
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
ds_resids_pois = function(truth, fitted, draw) {
  na_obs <- is.na(truth)
  a_obs <- ppois(as.vector(truth[!na_obs]) - 1.e-6, lambda = fitted[!na_obs])
  b_obs <- ppois(as.vector(truth[!na_obs]), lambda = fitted[!na_obs])
  u_obs <- runif(
    n = length(truth[!na_obs]),
    min = pmin(a_obs, b_obs),
    max = pmax(a_obs, b_obs)
  )

  if (any(is.na(truth))) {
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
ds_resids_tw = function(truth, fitted, draw) {
  na_obs <- is.na(truth)
  a_obs <- ppois(as.vector(truth[!na_obs]) - 1.e-6, lambda = fitted[!na_obs])
  b_obs <- ppois(as.vector(truth[!na_obs]), lambda = fitted[!na_obs])
  u_obs <- runif(
    n = length(truth[!na_obs]),
    min = pmin(a_obs, b_obs),
    max = pmax(a_obs, b_obs)
  )

  if (any(is.na(truth))) {
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
ds_resids_gaus = function(truth, fitted, sigma, draw) {
  na_obs <- is.na(truth)
  a_obs <- pnorm(
    as.vector(truth[!na_obs]) - 1.e-6,
    mean = fitted[!na_obs],
    sd = sigma
  )
  b_obs <- pnorm(as.vector(truth[!na_obs]), mean = fitted[!na_obs], sd = sigma)
  u_obs <- runif(
    n = length(truth[!na_obs]),
    min = pmin(a_obs, b_obs),
    max = pmax(a_obs, b_obs)
  )

  if (any(is.na(truth))) {
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
ds_resids_lnorm = function(truth, fitted, sigma, draw) {
  na_obs <- is.na(truth)
  a_obs <- plnorm(
    as.vector(truth[!na_obs]) - 1.e-6,
    meanlog = log(fitted[!na_obs]),
    sdlog = sigma[!na_obs]
  )
  b_obs <- plnorm(
    as.vector(truth[!na_obs]),
    meanlog = log(fitted[!na_obs]),
    sdlog = sigma[!na_obs]
  )
  u_obs <- runif(
    n = length(truth[!na_obs]),
    min = pmin(a_obs, b_obs),
    max = pmax(a_obs, b_obs)
  )

  if (any(is.na(truth))) {
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
ds_resids_gamma = function(truth, fitted, shape, draw) {
  na_obs <- is.na(truth)
  a_obs <- pgamma(
    as.vector(truth[!na_obs]) - 1.e-6,
    shape = shape[!na_obs],
    rate = shape[!na_obs] / fitted[!na_obs]
  )
  b_obs <- pgamma(
    as.vector(truth[!na_obs]),
    shape = shape[!na_obs],
    rate = shape[!na_obs] / fitted[!na_obs]
  )
  u_obs <- runif(
    n = length(truth[!na_obs]),
    min = pmin(a_obs, b_obs),
    max = pmax(a_obs, b_obs)
  )

  if (any(is.na(truth))) {
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
ds_resids_student = function(truth, fitted, sigma, nu, draw) {
  na_obs <- is.na(truth)
  a_obs <- pstudent_t(
    as.vector(truth[!na_obs]) - 1,
    df = nu[!na_obs],
    mu = fitted[!na_obs],
    sigma = sigma[!na_obs]
  )
  b_obs <- pstudent_t(
    as.vector(truth[!na_obs]),
    df = nu[!na_obs],
    mu = fitted[!na_obs],
    sigma = sigma[!na_obs]
  )
  u_obs <- runif(
    n = length(truth[!na_obs]),
    min = pmin(a_obs, b_obs),
    max = pmax(a_obs, b_obs)
  )

  if (any(is.na(truth))) {
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
get_forecast_resids = function(
  object,
  series,
  truth,
  preds,
  family,
  sample_seq
) {
  if (family == 'poisson') {
    series_residuals <- do.call(
      rbind,
      lapply(sample_seq, function(x) {
        suppressWarnings(ds_resids_pois(
          truth = truth,
          fitted = preds[x, ],
          draw = preds[x, ]
        ))
      })
    )
  }

  if (family == 'negative binomial') {
    size <- mcmc_chains(object$model_output, 'phi')
    series_residuals <- do.call(
      rbind,
      lapply(sample_seq, function(x) {
        suppressWarnings(ds_resids_nb(
          truth = truth,
          fitted = preds[x, ],
          draw = preds[x, ],
          size = size[x]
        ))
      })
    )
  }

  if (family == 'gaussian') {
    sigma <- mcmc_chains(object$model_output, 'sigma_obs')
    series_residuals <- do.call(
      rbind,
      lapply(sample_seq, function(x) {
        suppressWarnings(ds_resids_gaus(
          truth = truth,
          fitted = preds[x, ],
          draw = preds[x, ],
          sigma = sigma[x]
        ))
      })
    )
  }

  if (family == 'lognormal') {
    sigma <- mcmc_chains(object$model_output, 'sigma_obs')
    series_residuals <- do.call(
      rbind,
      lapply(sample_seq, function(x) {
        suppressWarnings(ds_resids_lnorm(
          truth = truth,
          fitted = preds[x, ],
          draw = preds[x, ],
          sigma = sigma[x]
        ))
      })
    )
  }

  if (family == 'student') {
    sigma <- mcmc_chains(object$model_output, 'sigma_obs')
    nu <- mcmc_chains(object$model_output, 'nu')
    series_residuals <- do.call(
      rbind,
      lapply(sample_seq, function(x) {
        suppressWarnings(ds_resids_student(
          truth = truth,
          fitted = preds[x, ],
          draw = preds[x, ],
          sigma = sigma[x],
          nu = nu[x]
        ))
      })
    )
  }

  if (family == 'beta') {
    precision <- mcmc_chains(object$model_output, 'phi')
    series_residuals <- do.call(
      rbind,
      lapply(sample_seq, function(x) {
        suppressWarnings(ds_resids_beta(
          truth = truth,
          fitted = preds[x, ],
          draw = preds[x, ],
          precision = precision[x]
        ))
      })
    )
  }

  if (family == 'Gamma') {
    shapes <- mcmc_chains(object$model_output, 'shape')
    series_residuals <- do.call(
      rbind,
      lapply(sample_seq, function(x) {
        suppressWarnings(ds_resids_gamma(
          truth = truth,
          fitted = preds[x, ],
          draw = preds[x, ],
          shape = shapes[x]
        ))
      })
    )
  }

  if (family == 'tweedie') {
    series_residuals <- do.call(
      rbind,
      lapply(sample_seq, function(x) {
        suppressWarnings(ds_resids_tw(
          truth = truth,
          fitted = preds[x, ],
          draw = preds[x, ]
        ))
      })
    )
  }
  return(series_residuals)
}

#' #'Residual calculations for a fitted mvgam object
#' @noRd
dsresids_vec = function(object) {
  family <- object$family
  obs_series <- object$obs_data$series
  series_levels <- levels(obs_series)
  fit_engine <- object$fit_engine

  # Need to know which series each observation belongs to so we can
  # pull out appropriate family-level parameters (overdispersions, shapes, etc...)
  all_dat <- data.frame(
    series = object$obs_data$series,
    time = object$obs_data$index..time..index,
    y = object$obs_data$y
  ) %>%
    dplyr::arrange(time, series)

  truth <- all_dat$y
  last_train <- NROW(all_dat)
  series_obs <- as.numeric(all_dat$series)

  # Extract expectations and necessary generated quantities
  # and subset to only include training data
  preds <- posterior_epred(object)[, 1:last_train, drop = FALSE]

  if (family == 'nmix') {
    p <- mcmc_chains(object$model_output, 'detprob')[,
      1:last_train,
      drop = FALSE
    ]
    N <- mcmc_chains(object$model_output, 'latent_ypred')[,
      1:last_train,
      drop = FALSE
    ]
  }

  if (family %in% c('binomial', 'beta_binomial')) {
    p <- plogis(mcmc_chains(object$model_output, 'mus')[,
      1:last_train,
      drop = FALSE
    ])
    N <- as.vector(attr(object$mgcv_model, 'trials'))[1:length(truth)]
  }

  # Family-specific parameters
  family_pars <- extract_family_pars(object = object)
  n_series <- NCOL(object$ytimes)

  # Family parameters spread into a vector
  family_extracts <- lapply(seq_along(family_pars), function(j) {
    if (is.matrix(family_pars[[j]])) {
      as.vector(family_pars[[j]][, series_obs])
    } else {
      as.vector(matrix(
        rep(family_pars[[j]], NCOL(preds)),
        nrow = NROW(preds),
        byrow = FALSE
      ))
    }
  })
  names(family_extracts) <- names(family_pars)

  # Create a truth matrix for vectorised residual computation
  truth_mat <- matrix(rep(truth, NROW(preds)), nrow = NROW(preds), byrow = TRUE)

  # Calculate DS residual distributions
  if (family == 'gaussian') {
    resids <- matrix(
      ds_resids_gaus(
        truth = as.vector(truth_mat),
        fitted = as.vector(preds),
        draw = 1,
        sigma = family_extracts$sigma_obs
      ),
      nrow = NROW(preds)
    )
  }

  if (family == 'binomial') {
    N_mat <- matrix(rep(N, NROW(preds)), nrow = NROW(preds), byrow = TRUE)
    resids <- matrix(
      ds_resids_binomial(
        truth = as.vector(truth_mat),
        fitted = as.vector(p),
        draw = 1,
        N = as.vector(N_mat)
      ),
      nrow = NROW(preds)
    )
  }

  if (family == 'beta_binomial') {
    N_mat <- matrix(rep(N, NROW(preds)), nrow = NROW(preds), byrow = TRUE)
    resids <- matrix(
      ds_resids_beta_binomial(
        truth = as.vector(truth_mat),
        fitted = as.vector(p),
        draw = 1,
        N = as.vector(N_mat),
        phi = family_extracts$phi
      ),
      nrow = NROW(preds)
    )
  }

  if (family == 'bernoulli') {
    resids <- matrix(
      ds_resids_binomial(
        truth = as.vector(truth_mat),
        fitted = as.vector(preds),
        draw = 1,
        N = rep(1, length(truth_mat))
      ),
      nrow = NROW(preds)
    )
  }

  if (family == 'nmix') {
    resids <- matrix(
      ds_resids_nmix(
        truth = as.vector(truth_mat),
        fitted = 1,
        draw = 1,
        N = as.vector(N),
        p = as.vector(p)
      ),
      nrow = NROW(preds)
    )
  }

  if (family == 'student') {
    resids <- matrix(
      ds_resids_student(
        truth = as.vector(truth_mat),
        fitted = as.vector(preds),
        draw = 1,
        sigma = family_extracts$sigma_obs,
        nu = family_extracts$nu
      ),
      nrow = NROW(preds)
    )
  }

  if (family == 'lognormal') {
    resids <- matrix(
      ds_resids_lnorm(
        truth = as.vector(truth_mat),
        fitted = as.vector(preds),
        draw = 1,
        sigma = family_extracts$sigma_obs
      ),
      nrow = NROW(preds)
    )
  }

  if (family == 'poisson') {
    resids <- matrix(
      ds_resids_pois(
        truth = as.vector(truth_mat),
        fitted = as.vector(preds),
        draw = 1
      ),
      nrow = NROW(preds)
    )
  }

  if (family == 'beta') {
    resids <- matrix(
      ds_resids_beta(
        truth = as.vector(truth_mat),
        fitted = as.vector(preds),
        draw = 1,
        precision = family_extracts$phi
      ),
      nrow = NROW(preds)
    )
  }

  if (family == 'Gamma') {
    resids <- matrix(
      ds_resids_gamma(
        truth = as.vector(truth_mat),
        fitted = as.vector(preds),
        draw = 1,
        shape = family_extracts$shape
      ),
      nrow = NROW(preds)
    )
  }

  if (family == 'negative binomial') {
    resids <- matrix(
      ds_resids_nb(
        truth = as.vector(truth_mat),
        fitted = as.vector(preds),
        draw = 1,
        size = family_extracts$phi
      ),
      nrow = NROW(preds)
    )
  }

  if (family == 'tweedie') {
    resids <- matrix(
      ds_resids_tw(
        truth = as.vector(truth_mat),
        fitted = as.vector(preds),
        draw = 1
      ),
      nrow = NROW(preds)
    )
  }

  # Convert to a list of series-level matrices and return
  series_resids <- lapply(seq_len(n_series), function(series) {
    inds_keep <- which(series_obs == series)
    resids[, inds_keep]
  })
  names(series_resids) <- levels(all_dat$series)

  return(series_resids)
}
