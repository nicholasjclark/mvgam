#'Fit a Bayesian multivariate GAM to a set of discrete time series
#'
#'This function estimates the posterior distribution for a multivariate GAM that includes
#'smooth seasonality and possible other smooth functions specified in the GAM formula. State-space latent trends
#'(random walks with drift)are estimated for each series, with a number of options for specifying the
#'structures of the trends
#'
#'
#'@param formula A \code{character} string specifying the GAM formula. These are exactly like the formula
#'for a GLM except that smooth terms, s, te, ti and t2, can be added to the right hand side
#'to specify that the linear predictor depends on smooth functions of predictors (or linear functionals of these).
#'@param data_train A \code{dataframe} containing the model response variable and covariates
#'required by the GAM \code{formula}. Should include columns:
#''y' (the discrete outcomes; NAs allowed)
#''series' (character or factor index of the series IDs)
#''season' (numeric index of the seasonal time point for each observation; should not have any missing)
#''year' the numeric index for year, and
#''in_season' indicator for whether the observation is in season or not. If the counts tend to go to zero
#'during the off season (as in tick counts for example), setting 'in_season' to zero cduring these seasonal periods
#'can be useful as trends won't contribute during
#'during this time but they continue to evolve, allowing the trend to continue evolving rather than forcing
#'it to zero during the off-season
#'Any other variables to be included in the linear predictor of \code{formula} must also be present
#'@param data_test A \code{dataframe} containing at least 'series', 'season', 'year' and 'in_season' for the forecast horizon, in
#'addition to any other variables included in the linear predictor of \code{formula}
#'@param prior_simulation \code{logical}. If \code{TRUE}, no observations are fed to the model, and instead
#'simulations from prior distributions are returned
#'@param use_nb \code{logical} If \code{TRUR}, use a Negative Binomial likelihood with estimated
#'overdispersion parameter r;
#'if \code{FALSE}, set r to \code{10,000} to approximate the Poisson distribution
#'@param use_mv \code{logical} If \code{TRUE} and \code{use_lv = FALSE},
#'a multivariate gaussian distribution is used for the state space trend errors. If \code{TRUE} and \code{use_lv = TRUE},
#a set of latent dynamic factors is estimated to capture possible dependencies in the state
#'space trends. If \code{FALSE}, independent gaussian distributions are used for the trend errors
#'@param use_lv \code{logical} If \code{TRUE}, use latent dynamic factors to estimate correlations in series'
#'latent trends. If \code{FALSE}, estimate covarying errors in the latent trends
#'@param n_lv \code{integer} the number of latent dynamic factors to use if \code{use_mv == TRUE}
#'and \code{use_lv == TRUE}. Defaults to \code{5}, which should provide enough information to estimate
#'correlations among latend factor loadings
#'@param n.chains \code{integer} the number of parallel chains for the model
#'@param n.adapt \code{integer} the number of iterations for adaptation. See adapt for details.
#'If \code{n.adapt} = 0 then no adaptation takes place
#'@param n.iter \code{integer} the number of iterations of the Markov chain to run
#'@param auto_update \code{logical} If \code{TRUE}, the model is run for up to \code{3} additional sets of
#'\code{n.iter} iterations, or until the lower 15th percentile of effective sample sizes reaches \code{100}
#'@param phi_prior \code{character} specifying (in JAGS syntax) the prior distributions for the drift terms in the
#'latent trends
#'@param r_prior \code{character} specifying (in JAGS syntax) the prior distribution for the Negative Binomial
#'overdispersion parameter
#'@param tau_prior \code{character} specifying (in JAGS syntax) the prior distributions for the independent gaussian
#'precisions used for the latent trends (ignored if \code{use_mv == TRUE})
#'@details A \code{\link[mcgv]{jagam}} model file is generated from \code{formula} and modified to include the latent
#'AR1 state space trends. The model parameters are esimated in a Bayesian framework using Gibbs sampling
#'in \code{\link[rjags]{jags.model}}.
#'
#'@return A \code{list} object containing JAGS model output, the text representation of the model file,
#'the mgcv model output (for easily generating simulations at
#'unsampled covariate values), the names of the smooth parameters and the matrix of indices for each series
#'
#'@export

mvjagam = function(formula,
                    data_train,
                    data_test,
                    prior_simulation = FALSE,
                    use_nb = TRUE,
                    use_mv = FALSE,
                    use_lv = FALSE,
                    n_lv = 5,
                    n.adapt = 1000,
                    n.chains = 2,
                    n.burnin = 5000,
                    n.iter = 2000,
                    thin = 2,
                    auto_update = TRUE,
                    phi_prior,
                    r_prior = 'dexp(0.001)',
                    tau_prior){

  # Fill in any NA's with arbitrary values so the observations aren't dropped when jags data is created
  if(prior_simulation){
    data_train$y <- rep(NA, length(data_train$y))
  }
  orig_y <- data_train$y
  data_train$y[which(is.na(data_train$y))] <- floor(sample(seq(0,50),
                                                             length(which(is.na(data_train$y))),
                                                             T))

  # Estimate the GAM model using mgcv so that the linear predictor matrix can be easily calculated
  # when simulating from the JAGS model later on
  ss_gam <- mgcv::gam(formula(formula),
                      data = data_train,
                      method = "REML",
                      family = poisson(),
                      drop.unused.levels = FALSE)

  # Make jags file and appropriate data structures
  ss_jagam <- mgcv::jagam(formula,
                          data = data_train,
                          family = poisson(),
                           drop.unused.levels = FALSE,
                          file = 'base_gam.txt',
                          sp.prior = 'gamma',
                          diagonalize = F)

  # Read in the base (unmodified) jags model file
  base_model <- suppressWarnings(readLines('base_gam.txt'))

  # Need to remove lines from the linear predictor section and replace with
  # the modified state space representation
  lines_remove <- c(1:grep('## response', base_model))
  base_model <- base_model[-lines_remove]

  # Any parametric effects in the gam need to get more sensible priors
  if(any(grepl('Parametric effect priors', base_model))){

    in_parenth <- regmatches(base_model[grep('Parametric effect priors',
                               base_model) + 1],
               gregexpr( "(?<=\\().+?(?=\\))", base_model[grep('Parametric effect priors',
                                                               base_model) + 1], perl = T))[[1]][1]
    n_terms <- as.numeric(sub(".*:", "", in_parenth))
    base_model[grep('Parametric effect priors',
                    base_model) + 1] <- paste0('  for (i in 1:',
                                               n_terms,
                                               ') { b[i] ~ dnorm(0, 0.1) }')
  }

  # Add replacement lines for trends and linear predictor
  if(!use_mv){
    modification <- c("model {

                                   ## GAM linear predictor
                                   eta <- X %*% b

                                   ## Mean expectations
                                   for (i in 1:n) {
                                   for (s in 1:n_series) {
                                   mu[i, s] <- exp(eta[ytimes[i, s]] + trend[i, s] * in_season[i])
                                   }
                                   }

                                   ## AR1 state space trends
                                   for(s in 1:n_series) {
                                   trend[1, s] ~ dnorm(0, tau[s])
                                   }

                                   for (i in 2:n){
                                   for (s in 1:n_series){
                                   trend[i, s] ~ dnorm(phi[s] + trend[i - 1, s], tau[s])
                                   }
                                   }

                                   for (s in 1:n_series){
                                   phi[s] ~ dnorm(0, 10)
                                   tau[s] ~ dgamma(0.05, 0.005)
                                   }

                                   ## Negative binomial likelihood functions
                                   for (i in 1:n) {
                                   for (s in 1:n_series) {
                                   y[i, s] ~ dnegbin(rate[i, s], r);
                                   rate[i, s] <- ifelse((r / (r + mu[i, s])) < min_eps, min_eps,
                                                        (r / (r + mu[i, s])))
                                   }
                                   }

                                   ## Set overdispersion parameter to 10000 if not using nb;
                                   ## this approximates the Poisson distribution
                                   r1 ~ dgamma(0.01, 0.01)
                                   r2 <- 10000
                                   r_indicator <- ifelse(use_nb > 0, 1, 0)
                                   r <- (r1 * r_indicator) + (r2 * (1 - r_indicator))

                                   ## Posterior predictions
                                   for (i in 1:n) {
                                   for (s in 1:n_series) {
                                   ypred[i, s] ~ dnegbin(rate[i, s], r)
                                   }
                                   }
                                   ")

    # Create the joined model file
    fil <- tempfile(fileext = ".xt")
    cat(c(readLines(textConnection(modification)), base_model), file = fil,
        sep = "\n")
    model_file <- readLines(fil, n = -1)

    # Update further prior distributions
    if(!missing(phi_prior)){
      model_file[grep('phi\\[s\\] ~', model_file)] <- paste0('   phi[s] ~ ', phi_prior)
    }

    if(!missing(tau_prior)){
      model_file[grep('tau\\[s\\] ~', model_file)] <- paste0('   tau[s] ~ ', tau_prior)
    }

    if(!missing(r_prior)){
      model_file[grep('r1 ~', model_file)] <- paste0('   r1 ~ ', r_prior)
    }

    model_file <- textConnection(model_file)
  }

  if(use_mv){
    if(missing(use_lv)){
      use_lv = TRUE
    }

    if(!use_lv){
      cat('Fitting a multivariate GAM with covariance for the latent trend errors...\n')
      modification <- c("model {

                        ## GAM linear predictor
                        eta <- X %*% b

                        ## Mean expectations
                        for (i in 1:n) {
                        for (s in 1:n_series) {
                        mu[i, s] <- exp(eta[ytimes[i, s]] + trend[i, s] * in_season[i])
                        }
                        }

                        ## RW + drift state space trends with possible covarying errors
                        trend[1, 1:n_series] ~ dmnorm(zeros, tau[ , ])

                        for (i in 2:n){
                        for (s in 1:n_series){
                        trend_mus[i, s] <- trend[i - 1, s] + phi[s]
                        }
                        }

                        for (i in 2:n){
                        trend[i, 1:n_series] ~ dmnorm(trend_mus[i, ], tau[ , ]);
                        }

                        # RW drift coefficients
                        for (s in 1:n_series){
                         phi[s] ~ dnorm(0, 10)
                        }

                        # Wishart is appropriate conjugate prior for dmnorm precision
                        # df = n_series + 1 sets uniform distribution on correlation parameters
                        tau[1:n_series, 1:n_series] ~ dwish(diag_mat[ , ], n_series + 1)

                        # Scaled residual correlation matrix (between -1 and 1; Gelman & Hill 2007)
                        Covar.raw[1:n_series, 1:n_series] <- inverse(tau[ , ])
                        for(i in 1:n_series){
                        for(j in 1:n_series){
                        cor[i, j] <- Covar.raw[i, j] / sqrt(Covar.raw[i, i] * Covar.raw[j, j])
                        }
                        }

                        ## Negative binomial likelihood functions
                        for (i in 1:n) {
                        for (s in 1:n_series) {
                        y[i, s] ~ dnegbin(rate[i, s], r);
                        rate[i, s] <- ifelse((r / (r + mu[i, s])) < min_eps, min_eps,
                        (r / (r + mu[i, s])))
                        }
                        }

                        ## Set overdispersion parameter to 10000 if not using nb;
                        ## this approximates the Poisson distribution
                        r1 ~ dgamma(0.01, 0.01)
                        r2 <- 10000
                        r_indicator <- ifelse(use_nb > 0, 1, 0)
                        r <- (r1 * r_indicator) + (r2 * (1 - r_indicator))

                        ## Posterior predictions
                        for (i in 1:n) {
                        for (s in 1:n_series) {
                        ypred[i, s] ~ dnegbin(rate[i, s], r)
                        }
                        }
                        ")

      # Create the joined model file
      fil <- tempfile(fileext = ".xt")
      cat(c(readLines(textConnection(modification)), base_model), file = fil,
          sep = "\n")
      model_file <- readLines(fil, n = -1)

      # Update and prior distribution choices
      if(!missing(phi_prior)){
        model_file[grep('phi\\[s\\] ~', model_file)] <- paste0('   phi[s] ~ ', phi_prior)
      }

      if(!missing(tau_prior)){
        model_file[grep('tau\\[s\\] ~', model_file)] <- paste0('   tau[s] ~ ', tau_prior)
      }

      if(!missing(r_prior)){
        model_file[grep('r1 ~', model_file)] <- paste0('   r1 ~ ', r_prior)
      }

      model_file <- textConnection(model_file)
    } else {
      #### Use the latent variable approach to estimate dependencies when n_series is large
      cat('Fitting a multivariate GAM with latent dynamic factors for the trends...\n')
      modification <- c(
        "model {

        ## GAM linear predictor
        eta <- X %*% b

        ## Mean expectations
        for (i in 1:n) {
        for (s in 1:n_series) {
        mu[i, s] <- exp(eta[ytimes[i, s]] + trend[i, s] * in_season[i])
        }
        }

        ## Latent factors evolve as RW + drift time series with shared precision
        tau_fac ~ dgamma(0.05, 0.005)
        for(j in 1:n_lv){
         LV[1, j] ~ dnorm(0, tau_fac)
        }
        for(i in 2:n){
         for(j in 1:n_lv){
          LV[i, j] ~ dnorm(LV[i - 1, j] + phi[j], tau_fac)
         }
        }

        ## Drift coefficients for latent factors
        for (s in 1:n_lv){
          phi[s] ~ dnorm(0, 10)
        }

        ## Latent factor loadings are penalized using a regularized horseshoe prior
        ## to allow loadings for entire factors to be effectively dropped, reducing overfitting
        for (j in 1:n_lv){
         penalty[j] ~ dt(0, 1, 1)T(0, )
         for (s in 1:n_series){
          lv_coefs[s, j] ~ dnorm(0, (1/lv_tau) * (1/penalty[j]))T(-1, 1);
         }
        }
        lv_tau ~ dt(0, 1, 1)T(0, )

        ## Trend evolution for the series depends on latent factors
        for (i in 1:n){
        for (s in 1:n_series){
         trend[i, s] <- inprod(lv_coefs[s,], LV[i,])
        }
        }

        ## Negative binomial likelihood functions
        for (i in 1:n) {
        for (s in 1:n_series) {
        y[i, s] ~ dnegbin(rate[i, s], r);
        rate[i, s] <- ifelse((r / (r + mu[i, s])) < min_eps, min_eps,
        (r / (r + mu[i, s])))
        }
        }

        ## Set overdispersion parameter to 10000 if not using nb;
        ## this approximates the Poisson distribution
        r1 ~ dgamma(0.01, 0.01)
        r2 <- 10000
        r_indicator <- ifelse(use_nb > 0, 1, 0)
        r <- (r1 * r_indicator) + (r2 * (1 - r_indicator))

        ## Posterior predictions
        for (i in 1:n) {
        for (s in 1:n_series) {
        ypred[i, s] ~ dnegbin(rate[i, s], r)
        }
        }
        ")

      # Create the joined model file
      fil <- tempfile(fileext = ".xt")
      cat(c(readLines(textConnection(modification)), base_model), file = fil,
          sep = "\n")
      model_file <- readLines(fil, n = -1)

      # Update further prior distributions
      if(!missing(phi_prior)){
        model_file[grep('phi\\[s\\] ~', model_file)] <- paste0('   phi[s] ~ ', phi_prior)
      }

      if(!missing(tau_prior)){
       model_file[grep('tau\\[s\\] ~', model_file)] <- paste0('   tau[s] ~ ', tau_prior)
      }

      if(!missing(r_prior)){
       model_file[grep('r1 ~', model_file)] <- paste0('   r1 ~ ', r_prior)
      }

      model_file <- textConnection(model_file)
    }

  }

  # Covariate dataframe including training and testing observations
  X <- data.frame(rbind(ss_jagam$jags.data$X,
                        predict(ss_gam, newdata = data_test, type = 'lpmatrix')))

  # Add a time variable
  X$time <- rbind(data_train, data_test[,1:ncol(data_train)]) %>%
    dplyr::left_join(rbind(data_train, data_test[,1:ncol(data_train)]) %>%
                       dplyr::select(year, season) %>%
                       dplyr::distinct() %>%
                       dplyr::arrange(year, season) %>%
                       dplyr::mutate(time = dplyr::row_number()),
                     by = c('season', 'year')) %>%
    dplyr::pull(time)

  # Add an outcome variable
  X$outcome <- c(orig_y, rep(NA, NROW(data_test)))

  # Add a series identifier variable
  X$series <- as.numeric(rbind(data_train, data_test)$series)

  # Arrange by time then by series
  X %>%
    dplyr::arrange(time, series) -> X

  # Matrix of indices in X that correspond to timepoints for each series
  ytimes <- matrix(NA, nrow = max(X$time), ncol = length(unique(X$series)))
  for(i in 1:length(unique(X$series))){
    ytimes[,i] <- which(X$series == i)
  }
  ss_jagam$jags.data$ytimes <- ytimes

  # Matrix of outcomes in X that correspond to each series at each timepoint
  ys_mat <- matrix(NA, nrow = NROW(ytimes), ncol = NCOL(ytimes))
  for(i in 1:length(unique(X$series))){
    ys_mat[,i] <- X$outcome[which(X$series == i)]
  }
  ss_jagam$jags.data$y <- ys_mat

  # Other necessary variables for JAGS
  ss_jagam$jags.data$n <- NROW(ytimes)
  ss_jagam$jags.data$n_series <- NCOL(ytimes)
  ss_jagam$jags.data$X = as.matrix(X %>%
                                     dplyr::select(-time, -series, -outcome))

  if(use_mv){
    if(!use_lv){
    # We use a Wishart prior for the state space multivariate precision matrix when
    # estimating the full covariance
    ss_jagam$jags.data$diag_mat <- diag(ss_jagam$jags.data$n_series)
    ss_jagam$jags.data$zeros <- rep(0, ss_jagam$jags.data$n_series)
    }
  }

  # Set use_nb to 0 for approximate Poisson; otherwise to 1 for Negative Binomial
  if(use_nb){
    ss_jagam$jags.data$use_nb <- 1
  } else {
    ss_jagam$jags.data$use_nb <- 0
  }

  # Machine epsilon for minimum allowable non-zero rate
  ss_jagam$jags.data$min_eps <- .Machine$double.eps

  # Ensure inits fall within prior bounds for rho
  ss_jagam$jags.ini$rho[ss_jagam$jags.ini$rho > 12] <- 11.99
  ss_jagam$jags.ini$rho[ss_jagam$jags.ini$rho < 12] <- -11.99

  # Number of latent variables to use if n_series > 5 and use_mv = TRUE
  if(use_mv){
    if(use_lv){
      if(missing(n_lv)){
        ss_jagam$jags.data$n_lv <- floor(ss_jagam$jags.data$n_series / 2)
      } else {
        ss_jagam$jags.data$n_lv <- n_lv
      }
      ss_jagam$jags.ini$tau_fac <- 1
    }
    }

  # Binary indicator of in_season
  ss_jagam$jags.data$in_season <- rbind(data_train, data_test) %>%
    dplyr::select(year, season, in_season) %>%
    dplyr::distinct() %>%
    dplyr::arrange(year, season) %>%
    dplyr::pull(in_season)

  # Initiate adaptation of the model
  load.module("glm")
  gam_mod <- jags.model(model_file,
                        data = ss_jagam$jags.data,
                        inits = ss_jagam$jags.ini,
                        n.chains = n.chains,
                        n.adapt = n.adapt)
  unlink('base_gam.txt')

  # Update the model for the burnin period
  update(gam_mod, n.burnin)

  # Gather posterior samples for the specified parameters
  if(!use_mv){
    param <- c('rho', 'b', 'mu', 'ypred',  'r', 'phi',
               'tau', 'trend')
  } else {
    if(!use_lv){
      param <- c('rho', 'b', 'mu', 'ypred',  'r', 'phi',
                 'tau', 'trend', 'cor')
    } else {
      param <- c('rho', 'b', 'mu', 'ypred',  'r', 'phi',
                 'trend', 'lv_coefs', 'tau_fac', 'penalty')
    }

  }

  out_gam_mod <- coda.samples(gam_mod,
                                     variable.names = param,
                                     n.iter = n.iter,
                                     thin = thin)
  if(auto_update){
  # Update until reasonable convergence in the form of Rhat and ESS
    if(!use_mv){
      update_params <- c('rho', 'b', 'tau')
    } else {
      update_params <- c('rho', 'b')
    }
  mod_summary <- MCMCvis::MCMCsummary(out_gam_mod, update_params)
  for(i in 1:3){
    if(quantile(mod_summary$Rhat, 0.85, na.rm = T) <= 1.2 &
       quantile(mod_summary$n.eff, 0.15) >= 100){
      break;
    }
    cat('Convergence not reached. Extending burnin...\n')
    update(gam_mod, n.burnin)
    out_gam_mod <- rjags::coda.samples(gam_mod,
                                       variable.names = param,
                                       n.iter = n.iter,
                                       thin = thin)
    mod_summary <- MCMCvis::MCMCsummary(out_gam_mod, update_params)
  }
  }

  model_file <- readLines(fil, n = -1)
  unlink(fil)

  return(list(jags_output = out_gam_mod,
              model_file = model_file,
              mgcv_model = ss_gam,
              jags_model = gam_mod,
              smooth_param_details = base_model[sort(c(grep('## prior for', base_model),
                                                       grep('## prior for', base_model)+1))],
              ytimes = ytimes))


}
