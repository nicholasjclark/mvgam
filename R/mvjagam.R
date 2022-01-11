#'Fit a Bayesian multivariate dynamic GAM to a set of discrete time series
#'
#'This function estimates the posterior distribution for a multivariate GAM that includes
#'smooth functions, specified in the GAM formula, and state-space latent trends, specified by trend_model.
#'There are currently two options for specifying the structures of the trends (either as latent
#'dynamic factors to capture trend dependencies in a reduced dimension format, or as independent trends)
#'
#'
#'@param formula A \code{character} string specifying the GAM formula. These are exactly like the formula
#'for a GLM except that smooth terms, s, te, ti and t2, can be added to the right hand side
#'to specify that the linear predictor depends on smooth functions of predictors (or linear functionals of these).
#'@param knots An optional \code{list} containing user specified knot values to be used for basis construction.
#'For most bases the user simply supplies the knots to be used, which must match up with the k value supplied
#'(note that the number of knots is not always just k). Different terms can use different numbers of knots,
#'unless they share a covariate.
#'@param data_train A \code{dataframe} containing the model response variable and covariates
#'required by the GAM \code{formula}. Should include columns:
#''y' (the discrete outcomes; \code{NA}s allowed)
#''series' (character or factor index of the series IDs)
#''season' (numeric index of the seasonal time point for each observation)
#' and 'year' the numeric index for year.
#'Any other variables to be included in the linear predictor of \code{formula} must also be present
#'@param data_test Optional \code{dataframe} of test data containing at least 'series', 'season', and 'year'
#'in addition to any other variables included in the linear predictor of \code{formula}. If included, the
#'observations in \code{data_test} will be set to \code{NA} when fitting the model so that posterior
#'simulations can be obtained
#'@param prior_simulation \code{logical}. If \code{TRUE}, no observations are fed to the model, and instead
#'simulations from prior distributions are returned
#'@param family \code{character}. Must be either 'nb' (for Negative Binomial) or 'poisson'
#'@param use_lv \code{logical}. If \code{TRUE}, use dynamic factors to estimate series'
#'latent trends in a reduced dimension format. If \code{FALSE}, estimate independent latent trends for each series
#'@param n_lv \code{integer} the number of latent dynamic factors to use if \code{use_lv == TRUE}.
#'Cannot be \code{>n_series}. Defaults arbitrarily to \code{min(5, floor(n_series / 2))}
#'@param trend_model \code{character} specifying the time series dynamics for the latent trend. Options are:
#''RW' (random walk with drift), 'AR1' (AR1 model with intercept), 'AR2' (AR2 model with intercept) or
#''AR3' (AR3 model with intercept)
#'@param n.chains \code{integer} specifying the number of parallel chains for the model
#'@param n.burnin \code{integer} specifying the number of iterations of the Markov chain to run during
#'adaptive mode to tune sampling algorithms
#'@param n.iter \code{integer} specifying the number of iterations of the Markov chain to run for sampling the posterior distribution
#'@param auto_update \code{logical}. If \code{TRUE}, the model is run for up to \code{15,000} additional
#'iterations, or until the lower 10th percentile of effective sample sizes for the GAM smoothing
#'parameters and beta coefficients reaches \code{100}
#'@param phi_prior \code{character} specifying (in JAGS syntax) the prior distribution for the drift terms/intercepts
#'in the latent trends
#'@param ar_prior \code{character} specifying (in JAGS syntax) the prior distribution for the AR terms
#'in the latent trends
#'@param r_prior \code{character} specifying (in JAGS syntax) the prior distribution for the Negative Binomial
#'overdispersion parameter. Ignored if \code{family = 'poisson'}
#'@param tau_prior \code{character} specifying (in JAGS syntax) the prior distributions for the independent gaussian
#'precisions used for the latent trends (ignored if \code{use_lv == TRUE})
#'@param upper_bounds Optional \code{vector} of \code{integer} values specifying upper limits for each series. If supplied,
#'this generates a modified likelihood where values above the bound are given a likelihood of zero. Note this modification
#'is computationally expensive in \code{JAGS} but can lead to better estimates when true bounds exist. Default is to remove
#'truncation entirely (i.e. there is no upper bound for each series)
#'@details A \code{\link[mcgv]{jagam}} model file is generated from \code{formula} and modified to include the latent
#'state space trends. Prior distributions for most important model parameters can be altered by the user to inspect model
#'sensitivities to given priors. Note that latent trends are estimated on the log scale so choose tau, AR and phi priors
#'accordingly. The model parameters are esimated in a Bayesian framework using Gibbs sampling
#'in \code{\link[rjags]{jags.model}}. When using a dynamic factor model for the trends, factor loadings are given
#'regularized horseshoe priors to theoretically allow some factors to be dropped from the model by squeezing loadings for
#'an entire factor to zero. This is done to help protect against selecting too many latent factors than are needed to
#'capture dependencies in the data, so it can often be advantageous to set n_lv to a slightly larger number. However
#'larger numbers of factors do come with additional computational costs so these should be balanced as well.
#'For the time being, all series are assumed to have the same overdispersion
#'parameter when using a negative binomial distribution, though this will be relaxed in future versions. For each
#'series, randomized quantile (i.e. Dunn-Smyth) residuals are calculated for inspecting model diagnostics using
#'medians of posterior predictions. If the fitted model is appropriate then Dunn-Smyth residuals will be
#'standard normal in distribution and no autocorrelation will be evident
#'
#'@author Nicholas J Clark
#'
#'@seealso \code{\link[rjags]{jags.model}}, \code{\link[mcgv]{jagam}}, \code{\link[mcgv]{gam}}
#'@return A \code{list} object containing JAGS model output, the text representation of the model file,
#'the mgcv model output (for easily generating simulations at
#'unsampled covariate values), the names of the smooth parameters, Dunn-Smyth residuals for each
#'series and information needed for other functions in the package
#'
#'@export

mvjagam = function(formula,
                   knots,
                    data_train,
                    data_test,
                    prior_simulation = FALSE,
                    family = 'nb',
                    use_lv = FALSE,
                    n_lv,
                    trend_model = 'RW',
                    n.chains = 2,
                    n.burnin = 5000,
                    n.iter = 2000,
                    thin = 2,
                    auto_update = TRUE,
                    phi_prior,
                    ar_prior,
                    r_prior,
                    tau_prior,
                    upper_bounds){

  # Check arguments
  trend_model <- match.arg(arg = trend_model, choices = c("RW", "AR1", "AR2", "AR3"))
  family <- match.arg(arg = family, choices = c("nb", "poisson"))

  # Fill in any NA's with arbitrary values so the observations aren't dropped when jags data is created
  orig_y <- data_train$y
  data_train$y[which(is.na(data_train$y))] <- floor(sample(seq(min(data_train$y, na.rm = T), max(data_train$y, na.rm = T)),
                                                             length(which(is.na(data_train$y))),
                                                             T))

  # Estimate the GAM model using mgcv so that the linear predictor matrix can be easily calculated
  # when simulating from the JAGS model later on
  if(!missing(knots)){
    ss_gam <- mgcv::bam(formula(formula),
                        data = data_train,
                        method = "fREML",
                        family = poisson(),
                        drop.unused.levels = FALSE,
                        knots = knots,
                        nthreads = parallel::detectCores()-1)
  } else {
    ss_gam <- mgcv::bam(formula(formula),
                        data = data_train,
                        method = "fREML",
                        family = poisson(),
                        drop.unused.levels = FALSE,
                        nthreads = parallel::detectCores()-1)
  }

  # Make jags file and appropriate data structures
  if(!missing(knots)){
    ss_jagam <- mgcv::jagam(formula,
                            data = data_train,
                            family = poisson(),
                            drop.unused.levels = FALSE,
                            file = 'base_gam.txt',
                            sp.prior = 'gamma',
                            diagonalize = F,
                            knots = knots)
  } else {
    ss_jagam <- mgcv::jagam(formula,
                            data = data_train,
                            family = poisson(),
                            drop.unused.levels = FALSE,
                            file = 'base_gam.txt',
                            sp.prior = 'gamma',
                            diagonalize = F)
  }

  # Fill with NAs if this is a simulation from the priors
  if(prior_simulation){
    data_train$y <- rep(NA, length(data_train$y))
  } else {
    data_train$y <- orig_y
  }

  # Read in the base (unmodified) jags model file
  base_model <- suppressWarnings(readLines('base_gam.txt'))

  # Remove lines from the linear predictor section
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
                                               ') { b[i] ~ dnorm(0, 1) }')
  }

  # Add replacement lines for trends and the linear predictor
  if(!use_lv){
    modification <- c("model {

                      ## GAM linear predictor
                      eta <- X %*% b

                      ## Mean expectations
                      for (i in 1:n) {
                      for (s in 1:n_series) {
                      mu[i, s] <- exp(eta[ytimes[i, s]] + trend[i, s])
                      }
                      }

                      ## State space trends
                      for(s in 1:n_series) {
                      trend[1, s] ~ dnorm(0, tau[s])
                      }

                      for(s in 1:n_series) {
                      trend[2, s] ~ dnorm(phi[s] + ar1[s]*trend[1, s], tau[s])
                      }

                      for(s in 1:n_series) {
                      trend[3, s] ~ dnorm(phi[s] + ar1[s]*trend[2, s] +
                                          ar2[s]*trend[1, s], tau[s])
                      }

                      for (i in 4:n){
                      for (s in 1:n_series){
                      trend[i, s] ~ dnorm(phi[s] + ar1[s]*trend[i - 1, s] +
                                          ar2[s]*trend[i - 2, s] +
                                          ar3[s]*trend[i - 3, s], tau[s])
                      }
                      }

                      ## AR components
                      for (s in 1:n_series){
                      phi[s] ~ dnorm(0, 10)
                      ar1[s] ~ dnorm(0, 10)
                      ar2[s] ~ dnorm(0, 10)
                      ar3[s] ~ dnorm(0, 10)
                      tau[s] ~ dgamma(0.1, 0.001)
                      }

                      ## Negative binomial likelihood functions
                      for (i in 1:n) {
                      for (s in 1:n_series) {
                      y[i, s] ~ dnegbin(rate[i, s], r)T(, upper_bound[s]);
                      rate[i, s] <- ifelse((r / (r + mu[i, s])) < min_eps, min_eps,
                      (r / (r + mu[i, s])))
                      }
                      }
                      r ~ dexp(0.001)

                      ## Posterior predictions
                      for (i in 1:n) {
                      for (s in 1:n_series) {
                      ypred[i, s] ~ dnegbin(rate[i, s], r)T(, upper_bound[s])
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

    if(!missing(ar_prior)){
      model_file[grep('ar1\\[s\\] ~', model_file)] <- paste0('   ar1[s] ~ ', ar_prior)
      model_file[grep('ar2\\[s\\] ~', model_file)] <- paste0('   ar2[s] ~ ', ar_prior)
      model_file[grep('ar3\\[s\\] ~', model_file)] <- paste0('   ar3[s] ~ ', ar_prior)
    }

    if(!missing(tau_prior)){
      model_file[grep('tau\\[s\\] ~', model_file)] <- paste0('   tau[s] ~ ', tau_prior)
    }

    if(!missing(r_prior)){
      model_file[grep('r1 ~', model_file)] <- paste0('   r1 ~ ', r_prior)
    }

    if(family == 'poisson'){
      if(missing(upper_bounds)){
        model_file[grep('y\\[i, s\\] ~', model_file)] <- 'y[i, s] ~ dpois(mu[i, s])'
        model_file[grep('ypred\\[i, s\\] ~', model_file)] <- 'ypred[i, s] ~ dpois(mu[i, s])'
      } else {
        model_file[grep('y\\[i, s\\] ~', model_file)] <- 'y[i, s] ~ dpois(mu[i, s])T(, upper_bound[s])'
        model_file[grep('ypred\\[i, s\\] ~', model_file)] <- 'ypred[i, s] ~ dpois(mu[i, s])T(, upper_bound[s])'
      }
      model_file[grep('r ~', model_file)] <- 'r <- 10000'

    } else {
      if(missing(upper_bounds)){
        model_file[grep('y\\[i, s\\] ~', model_file)] <- 'y[i, s] ~ dnegbin(rate[i, s], r)'
        model_file[grep('ypred\\[i, s\\] ~', model_file)] <- 'ypred[i, s] ~ dnegbin(rate[i, s], r)'
      }
    }

    if(trend_model == 'RW'){
      model_file[grep('ar1\\[s\\] ~', model_file)] <- 'ar1[s] <- 1'
      model_file[grep('ar2\\[s\\] ~', model_file)] <- 'ar2[s] <- 0'
      model_file[grep('ar3\\[s\\] ~', model_file)] <- 'ar3[s] <- 0'
    }

    if(trend_model == 'AR1'){
      model_file[grep('ar2\\[s\\] ~', model_file)] <- 'ar2[s] <- 0'
      model_file[grep('ar3\\[s\\] ~', model_file)] <- 'ar3[s] <- 0'
    }

    if(trend_model == 'AR2'){
      model_file[grep('ar3\\[s\\] ~', model_file)] <- 'ar3[s] <- 0'
    }

    model_file <- textConnection(model_file)
  }

  if(use_lv){

      #### Use the latent variable approach to estimate dependent trends
      cat('Fitting a multivariate GAM with latent dynamic factors for the trends...\n')
      modification <- c(
        "model {

        ## GAM linear predictor
        eta <- X %*% b

        ## Mean expectations
        for (i in 1:n) {
        for (s in 1:n_series) {
        mu[i, s] <- exp(eta[ytimes[i, s]] + trend[i, s])
        }
        }

        ## Latent factors evolve as time series with shared precision
        tau_fac ~ dgamma(0.05, 0.005)
        for(j in 1:n_lv){
         LV[1, j] ~ dnorm(0, tau_fac)
        }

        for(j in 1:n_lv){
         LV[2, j] ~ dnorm(phi[j] + ar1[j]*LV[1, j], tau_fac)
        }

        for(j in 1:n_lv){
         LV[3, j] ~ dnorm(phi[j] + ar1[j]*LV[2, j] + ar2[j]*LV[1, j], tau_fac)
        }

        for(i in 4:n){
         for(j in 1:n_lv){
          LV[i, j] ~ dnorm(phi[j] + ar1[j]*LV[i - 1, j] +
                          ar2[j]*LV[i - 2, j] + ar3[j]*LV[i - 3, j], tau_fac)
         }
        }

        ## AR components
        for (s in 1:n_lv){
        phi[s] ~ dnorm(0, 10)
        ar1[s] ~ dnorm(0, 10)
        ar2[s] ~ dnorm(0, 10)
        ar3[s] ~ dnorm(0, 10)
        }

        ## Latent factor loadings are penalized using a regularized horseshoe prior
        ## to allow loadings for entire factors to be 'dropped', reducing overfitting. Still
        ## need to impose identifiability constraints

        ## Global shrinkage penalty (half cauchy, following Gelman et
        ## al. 2008: A weakly informative default prior distribution for logistic and other
        ## regression models)
        lv_tau ~ dscaled.gamma(0.5, 5)

        ## Shrinkage penalties for each factor, which are also half cuachy to allow
        ## the entire factor's set of coefficients to be squeezed toward zero if supported by
        ## the data. The prior for individual factor penalties allows each factor to possibly
        ## have a relatively large penalty, which shrinks the prior for that factor's loadings
        ## substantially
        for (j in 1:n_lv){
         penalty[j] ~ dscaled.gamma(0.5, 0.5)
        }

        ## Upper triangle of loading matrix set to zero
        for(j in 1:(n_lv - 1)){
          for(j2 in (j + 1):n_lv){
           lv_coefs[j, j2] <- 0
          }
         }

        ## Positive constraints on loading diagonals
        for(j in 1:n_lv) {
         lv_coefs[j, j] ~ dnorm(0, lv_tau * penalty[j])T(0, 1);
        }

        ## Lower diagonal free
        for(j in 2:n_lv){
         for(j2 in 1:(j - 1)){
          lv_coefs[j, j2] ~ dnorm(0, lv_tau * penalty[j2])T(-1, 1);
         }
       }

        ## Other elements also free
        for(j in (n_lv + 1):n_series) {
         for(j2 in 1:n_lv){
          lv_coefs[j, j2] ~ dnorm(0, lv_tau * penalty[j2])T(-1, 1);
         }
        }

        ## Trend evolution for the series depends on latent factors
        for (i in 1:n){
        for (s in 1:n_series){
         trend[i, s] <- inprod(lv_coefs[s,], LV[i,])
        }
        }

        ## Negative binomial likelihood functions
        for (i in 1:n) {
        for (s in 1:n_series) {
        y[i, s] ~ dnegbin(rate[i, s], r)T(, upper_bound[s]);
        rate[i, s] <- ifelse((r / (r + mu[i, s])) < min_eps, min_eps,
                            (r / (r + mu[i, s])))
        }
        }
        r ~ dexp(0.001)

        ## Posterior predictions
        for (i in 1:n) {
        for (s in 1:n_series) {
        ypred[i, s] ~ dnegbin(rate[i, s], r)T(, upper_bound[s])
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

      if(!missing(ar_prior)){
        model_file[grep('ar1\\[s\\] ~', model_file)] <- paste0('   ar1[s] ~ ', ar_prior)
        model_file[grep('ar2\\[s\\] ~', model_file)] <- paste0('   ar2[s] ~ ', ar_prior)
        model_file[grep('ar3\\[s\\] ~', model_file)] <- paste0('   ar3[s] ~ ', ar_prior)
      }

      if(!missing(tau_prior)){
       model_file[grep('tau\\[s\\] ~', model_file)] <- paste0('   tau[s] ~ ', tau_prior)
      }

      if(!missing(r_prior)){
       model_file[grep('r1 ~', model_file)] <- paste0('   r1 ~ ', r_prior)
      }

      if(family == 'poisson'){
        if(missing(upper_bounds)){
          model_file[grep('y\\[i, s\\] ~', model_file)] <- 'y[i, s] ~ dpois(mu[i, s])'
          model_file[grep('ypred\\[i, s\\] ~', model_file)] <- 'ypred[i, s] ~ dpois(mu[i, s])'
        } else {
          model_file[grep('y\\[i, s\\] ~', model_file)] <- 'y[i, s] ~ dpois(mu[i, s])T(, upper_bound[s])'
          model_file[grep('ypred\\[i, s\\] ~', model_file)] <- 'ypred[i, s] ~ dpois(mu[i, s])T(, upper_bound[s])'
        }
        model_file[grep('r ~', model_file)] <- 'r <- 10000'

      } else {
        if(missing(upper_bounds)){
          model_file[grep('y\\[i, s\\] ~', model_file)] <- 'y[i, s] ~ dnegbin(rate[i, s], r)'
          model_file[grep('ypred\\[i, s\\] ~', model_file)] <- 'ypred[i, s] ~ dnegbin(rate[i, s], r)'
        }
      }

      if(trend_model == 'RW'){
        model_file[grep('ar1\\[s\\] ~', model_file)] <- 'ar1[s] <- 1'
        model_file[grep('ar2\\[s\\] ~', model_file)] <- 'ar2[s] <- 0'
        model_file[grep('ar3\\[s\\] ~', model_file)] <- 'ar3[s] <- 0'
      }

      if(trend_model == 'AR1'){
        model_file[grep('ar2\\[s\\] ~', model_file)] <- 'ar2[s] <- 0'
        model_file[grep('ar3\\[s\\] ~', model_file)] <- 'ar3[s] <- 0'
      }

      if(trend_model == 'AR2'){
        model_file[grep('ar3\\[s\\] ~', model_file)] <- 'ar3[s] <- 0'
      }

      model_file <- textConnection(model_file)

  }

  # Covariate dataframe including training and testing observations
  if(!missing(data_test)){
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

  } else {
    X <- data.frame(ss_jagam$jags.data$X)
    X$time <- data_train %>%
      dplyr::left_join(data_train %>%
                         dplyr::select(year, season) %>%
                         dplyr::distinct() %>%
                         dplyr::arrange(year, season) %>%
                         dplyr::mutate(time = dplyr::row_number()),
                       by = c('season', 'year')) %>%
      dplyr::pull(time)
    X$outcome <- c(orig_y)
    X$series <- as.numeric(data_train$series)
  }

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
  ss_jagam$jags.data$X <- as.matrix(X %>%
                                     dplyr::select(-time, -series, -outcome))
  if(!missing(upper_bounds)){
    ss_jagam$jags.data$upper_bound <- upper_bounds
  }

  # Machine epsilon for minimum allowable non-zero rate
  ss_jagam$jags.data$min_eps <- .Machine$double.eps

  # Ensure inits fall within prior bounds for rho
  ss_jagam$jags.ini$rho[ss_jagam$jags.ini$rho > 12] <- 11.99
  ss_jagam$jags.ini$rho[ss_jagam$jags.ini$rho < 12] <- -11.99

  # Number of latent variables to use
  if(use_lv){
      if(missing(n_lv)){
        ss_jagam$jags.data$n_lv <- min(2, floor(ss_jagam$jags.data$n_series / 2))
      } else {
        ss_jagam$jags.data$n_lv <- n_lv
      }
      if(ss_jagam$jags.data$n_lv > ss_jagam$jags.data$n_series){
        stop('Number of latent variables cannot be greater than number of series')
      }
      ss_jagam$jags.ini$tau_fac <- 1
  }

  # Initiate adaptation of the model for the full burnin period. This is necessary as JAGS
  # will take a while to optimise the samplers, so long adaptation with little 'burnin'
  # is more crucial than little adaptation but long 'burnin' https://mmeredith.net/blog/2016/Adapt_or_burn.htm
  load.module("glm")
  gam_mod <- jags.model(model_file,
                        data = ss_jagam$jags.data,
                        inits = ss_jagam$jags.ini,
                        n.chains = n.chains,
                        n.adapt = 0)
  unlink('base_gam.txt')

  # Update the model for the burnin period
  adapt(gam_mod, n.iter = n.burnin, end.adaptation = TRUE)

  # Gather posterior samples for the specified parameters
  if(!use_lv){
    param <- c('rho', 'b', 'mu', 'ypred',  'r', 'phi',
               'tau', 'trend', 'ar1', 'ar2', 'ar3')
  } else {
    param <- c('rho', 'b', 'mu', 'ypred',  'r', 'phi', 'LV',
               'trend', 'lv_coefs', 'tau_fac', 'penalty',
               'ar1', 'ar2', 'ar3')
  }

  out_gam_mod <- coda.samples(gam_mod,
                              variable.names = param,
                              n.iter = n.iter,
                              thin = thin)
  if(auto_update){
  # Update until reasonable convergence in the form of Rhat and ESS
    if(!use_lv){
      update_params <- c('rho', 'b', 'tau')
    } else {
      update_params <- c('rho', 'b')
    }
  mod_summary <- MCMCvis::MCMCsummary(out_gam_mod, update_params)
  for(i in 1:3){
    if(quantile(mod_summary$Rhat, 0.85, na.rm = T) <= 1.2 &
       quantile(mod_summary$n.eff, 0.1) >= 100){
      break;
    }
    cat('Convergence not reached. Extending burnin...\n')
    update(gam_mod, min(5000, n.burnin))
    out_gam_mod <- rjags::coda.samples(gam_mod,
                                       variable.names = param,
                                       n.iter = n.iter,
                                       thin = thin)
    mod_summary <- MCMCvis::MCMCsummary(out_gam_mod, update_params)
  }
  }

  model_file <- readLines(fil, n = -1)
  unlink(fil)

  if(missing(upper_bounds)){
    upper_bounds <- NULL
  }

  if(use_lv){
    n_lv <- ss_jagam$jags.data$n_lv
  } else {
    n_lv <- NULL
  }

  # Get Dunn-Smyth Residuals based on median predictions from the model
  ds_resids = function(truth, fitted, size){
    dsres_out <- matrix(NA, length(truth), 1)
    for(i in 1:length(truth)){
      a <- pnbinom(as.vector(truth[i]) - 1, mu = fitted[i], size = size)
      b <- pnbinom(as.vector(truth[i]), mu = fitted[i], size = size)
      u <- runif(n = 1, min = a, max = b)
      dsres_out[i, ] <- qnorm(u)
    }
    resids <- dsres_out[,1]
    resids[is.infinite(resids)] <- NaN
    resids
  }

  ends <- seq(0, dim(MCMCvis::MCMCchains(out_gam_mod, 'ypred'))[2],
              length.out = NCOL(ytimes) + 1)
  starts <- ends + 1
  starts <- c(1, starts[-c(1, (NCOL(ytimes)+1))])
  ends <- ends[-1]
  size <- MCMCvis::MCMCsummary(out_gam_mod, 'r')$mean

  series_resids <- lapply(seq_len(NCOL(ytimes)), function(series){
    n_obs <- data_train %>%
      dplyr::filter(series == !!(levels(data_train$series)[series])) %>%
      nrow()
    preds <- apply(MCMCvis::MCMCchains(out_gam_mod, 'ypred')[,starts[series]:ends[series]],
                   2, function(x) hpd(x)[2])
    suppressWarnings(ds_resids(truth = as.vector(ys_mat[1:n_obs,series]),
                               fitted = preds[1:n_obs],
                               size = size))
  })
  names(series_resids) <- levels(data_train$series)

  return(list(call = formula,
              family = ifelse(family == 'nb',
                              'Negative Binomial',
                              'Poisson'),
              pregam = ss_jagam$pregam,
              jags_output = out_gam_mod,
              model_file = model_file,
              mgcv_model = ss_gam,
              jags_model = gam_mod,
              smooth_param_details = base_model[sort(c(grep('## prior for', base_model),
                                                       grep('## prior for', base_model)+1))],
              ytimes = ytimes,
              resids = series_resids,
              use_lv = use_lv,
              n_lv = n_lv,
              upper_bounds = upper_bounds,
              obs_data = data_train))
}
