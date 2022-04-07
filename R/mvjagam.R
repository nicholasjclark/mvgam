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
#'@param data_train A \code{dataframe} or \code{list} containing the model response variable and covariates
#'required by the GAM \code{formula}. Should include columns:
#''y' (the discrete outcomes; \code{NA}s allowed)
#''series' (character or factor index of the series IDs)
#''time' (numeric index of the time point for each observation).
#'Any other variables to be included in the linear predictor of \code{formula} must also be present
#'@param data_test Optional \code{dataframe} or \code{list} of test data containing at least 'series' and 'time'
#'in addition to any other variables included in the linear predictor of \code{formula}. If included, the
#'observations in \code{data_test} will be set to \code{NA} when fitting the model so that posterior
#'simulations can be obtained
#'@param run_model \code{logical}. If \code{FALSE}, the model is not fitted but instead the function will
#'return the model file and the data / initial values that are needed to fit the \code{JAGS} model
#'@param prior_simulation \code{logical}. If \code{TRUE}, no observations are fed to the model, and instead
#'simulations from prior distributions are returned
#'@param return_jags_data \code{logical}. If \code{TRUE}, the list of data that is needed to fit the \code{JAGS}
#'model is returned, along with the initial values for smooth and AR parameters, once the model is fitted.
#'This will be helpful if users wish to modify the model file to add
#'other stochastic elements that are not currently avaiable in \code{mvgam}. Default is \code{FALSE} to reduce
#'the size of the returned object, unless \code{run_model == FALSE}
#'@param family \code{character}. Must be either 'nb' (for Negative Binomial), 'tw' (for Tweedie) or 'poisson'
#'@param use_lv \code{logical}. If \code{TRUE}, use dynamic factors to estimate series'
#'latent trends in a reduced dimension format. If \code{FALSE}, estimate independent latent trends for each series
#'@param n_lv \code{integer} the number of latent dynamic factors to use if \code{use_lv == TRUE}.
#'Cannot be \code{>n_series}. Defaults arbitrarily to \code{min(5, floor(n_series / 2))}
#'@param trend_model \code{character} specifying the time series dynamics for the latent trend. Options are:
#''None' (no latent trend component; i.e. the GAM component is all that contributes to the linear predictor,
#'and the observation process is the only source of error; similarly to what is estimated by \code{\link[mcgv]{gam}}),
#''Noise' (modelled as a Gaussian static white noise process),
#''RW' (random walk with possible drift),
#''AR1' (AR1 model with intercept),
#''AR2' (AR2 model with intercept) or
#''AR3' (AR3 model with intercept)
#'@param drift \code{logical} estimate a drift parameter in the latent trend components. Useful if the latent
#'trend is expected to broadly follow a non-zero slope. Note that if the latent trend is more or less stationary,
#'the drift parameter can become unidentifiable, especially if an intercept term is included in the GAM linear
#'predictor (which it is by default when calling \code{\link[mcgv]{jagam}}). Therefore this defaults to \code{FALSE}
#'@param chains \code{integer} specifying the number of parallel chains for the model
#'@param burnin \code{integer} specifying the number of iterations of the Markov chain to run during
#'adaptive mode to tune sampling algorithms
#'@param n_samples \code{integer} specifying the number of iterations of the Markov chain to run for
#'sampling the posterior distribution
#'@param thin Thinning interval for monitors
#'@param parallel \code{logical} specifying whether multiple cores should be used for
#'for generating \code{JAGS} simulations in parallel. If \code{TRUE}, the number of cores to use will be
#'\code{min(c(chains, parallel::detectCores() - 1))}
#'@param phi_prior \code{character} specifying (in JAGS syntax) the prior distribution for the drift terms/intercepts
#'in the latent trends
#'@param ar_prior \code{character} specifying (in JAGS syntax) the prior distribution for the AR terms
#'in the latent trends
#'@param r_prior \code{character} specifying (in JAGS syntax) the prior distribution for the Negative Binomial
#'overdispersion parameters. Note that this prior acts on the SQUARE ROOT of \code{r}, which is convenient
#'for inducing a complexity-penalising prior model whereby the observation process reduces to a Poisson
#'as the sampled parameter approaches \code{0}. Ignored if family is Poisson or Tweedie
#'@param twdis_prior \code{character} specifying (in JAGS syntax) the prior distribution for the Tweedie
#'overdispersion parameters. Ignored if family is Poisson or Negative Binomial
#'@param tau_prior \code{character} specifying (in JAGS syntax) the prior distributions for the independent gaussian
#'precisions used for the latent trends (ignored if \code{use_lv == TRUE})
#'@param upper_bounds Optional \code{vector} of \code{integer} values specifying upper limits for each series. If supplied,
#'this generates a modified likelihood where values above the bound are given a likelihood of zero. Note this modification
#'is computationally expensive in \code{JAGS} but can lead to better estimates when true bounds exist. Default is to remove
#'truncation entirely (i.e. there is no upper bound for each series)
#'@param jags_path Optional character vector specifying the path to the location of the `JAGS` executable (.exe) to use
#'for modelling. If missing, the path will be recovered from a call to \code{\link[runjags]{findjags}}
#'
#'@details A \code{\link[mcgv]{jagam}} model file is generated from \code{formula} and modified to include the latent
#'state space trends. Prior distributions for most important model parameters can be altered by the user to inspect model
#'sensitivities to given priors. Note that latent trends are estimated on the log scale so choose tau, AR and phi priors
#'accordingly. Model parameters are esimated in a Bayesian framework using Gibbs sampling
#'in \code{\link[run.jags]{runjags}}. For any smooth terms using the random effect basis (\code{\link[mcgv]{smooth.construct.re.smooth.spec}}),
#'a non-centred parameterisation is employed to avoid degeneracies that are common in hierarchical models. Note that for Tweedie
#'models, estimating the power parameter `p` alongside the overdispersion parameter
#'`twdis` and the smooth coefficients is very challenging for noisy data, introducing some difficult posterior geometries.
#'By default, the `p` parameter is fixed at `1.5` (i.e. a so-called Geometric Poisson model).
#'When using a dynamic factor model for the trends, factor precisions are given
#'regularized penalty priors to theoretically allow some factors to be dropped from the model by squeezing increasing
#'factors' variances to zero. This is done to help protect against selecting too many latent factors than are needed to
#'capture dependencies in the data, so it can often be advantageous to set n_lv to a slightly larger number. However
#'larger numbers of factors do come with additional computational costs so these should be balanced as well.
#' For each series, randomized quantile (i.e. Dunn-Smyth) residuals are calculated for inspecting model diagnostics using
#'medians of posterior predictions. If the fitted model is appropriate then Dunn-Smyth residuals will be
#'standard normal in distribution and no autocorrelation will be evident
#'
#'@author Nicholas J Clark
#'
#'@seealso \code{\link[rjags]{jags.model}}, \code{\link[mcgv]{jagam}}, \code{\link[mcgv]{gam}}
#'@return A \code{list} object containing JAGS model output, the text representation of the model file,
#'the mgcv model output (for easily generating simulations at
#'unsampled covariate values), Dunn-Smyth residuals for each series and other key information needed
#'for other functions in the package
#'
#'@export

mvjagam = function(formula,
                   knots,
                   data_train,
                   data_test,
                   run_model = TRUE,
                   prior_simulation = FALSE,
                   return_jags_data = FALSE,
                   family = 'poisson',
                   use_lv = FALSE,
                   n_lv,
                   trend_model = 'RW',
                   drift = FALSE,
                   chains = 2,
                   burnin = 5000,
                   n_samples = 2000,
                   thin = 4,
                   parallel = TRUE,
                   phi_prior,
                   ar_prior,
                   r_prior,
                   twdis_prior,
                   tau_prior,
                   upper_bounds,
                   jags_path){

  # Check arguments
  trend_model <- match.arg(arg = trend_model, choices = c("None", "Noise", "RW", "AR1", "AR2", "AR3"))
  family <- match.arg(arg = family, choices = c("nb", "poisson", "tw"))

  # If the model is to be run, make sure JAGS can be located
  if(run_model){
    if(missing(jags_path)){
      jags_path <- runjags::findjags()
    }

    # Code borrowed from the runjags package
    jags_status <- runjags::testjags(jags_path, silent = TRUE)
    if(!jags_status$JAGS.available){
      if(jags_status$os=="windows"){
        # Try it again - sometimes this helps
        Sys.sleep(0.2)
        jags_status <- runjags::testjags(jags_path, silent = TRUE)
      }

      if(!jags_status$JAGS.available){
        cat("Unable to call JAGS using '", jags_path, "'\nTry specifying the path to the JAGS binary as the jags_path argument, or installing the rjags package.\nUse the runjags::testjags() function for more detailed diagnostics.\n", sep="")
        stop("Unable to call JAGS", call. = FALSE)
      }
    }
  }


  # Add series factor variable if missing
  if(class(data_train)[1] != 'list'){
  if(!'series' %in% colnames(data_train)){
    data_train$series <- factor('series1')
    if(!missing(data_test)){
      data_test$series <- factor('series1')
    }
  }

  # Must be able to index by time; it is too dangerous to 'guess' as this could make a huge
    # impact on resulting estimates / inferences
  if(!'time' %in% colnames(data_train)){
    stop('data_train does not contain a "time" column')
  }

  }

  if(class(data_train)[1] == 'list'){
    if(!'time' %in% names(data_train)){
      stop('data_train does not contain a "time" column')
    }
  }

  # Ensure outcome is labelled 'y'
  form_terms <- terms(formula(formula))
  if(dimnames(attr(form_terms, 'factors'))[[1]][1] != 'y'){
    stop('Outcome variable must be named "y"')
  }

  # If there are missing values in y, use predictions from an initial mgcv model to fill
  # these in so that initial values can be more accurate and we maintain the true
  # size of the training dataset
  orig_y <- data_train$y

    if(!missing(knots)){

      # Estimate the GAM model using mgcv so that the linear predictor matrix can be easily calculated
      # when simulating from the JAGS model later on
      if(family == 'nb'){
        ss_gam <- mgcv::bam(formula(formula),
                            data = data_train,
                            method = "fREML",
                            family = nb(),
                            drop.unused.levels = FALSE,
                            knots = knots,
                            nthreads = parallel::detectCores()-1,
                            discrete = TRUE)
      } else if(family == 'poisson'){
        ss_gam <- mgcv::bam(formula(formula),
                            data = data_train,
                            method = "fREML",
                            family = poisson(),
                            drop.unused.levels = FALSE,
                            knots = knots,
                            nthreads = parallel::detectCores()-1,
                            discrete = TRUE)
      } else {
        ss_gam <- mgcv::bam(formula(formula),
                            data = data_train,
                            method = "fREML",
                            family = tw(),
                            drop.unused.levels = FALSE,
                            knots = knots,
                            nthreads = parallel::detectCores()-1,
                            discrete = TRUE)
      }

    } else {
      if(family == 'nb'){
        ss_gam <- mgcv::bam(formula(formula),
                            data = data_train,
                            method = "fREML",
                            family = nb(),
                            drop.unused.levels = FALSE,
                            nthreads = parallel::detectCores()-1,
                            discrete = TRUE)
      } else if(family == 'poisson'){
        ss_gam <- mgcv::bam(formula(formula),
                            data = data_train,
                            method = "fREML",
                            family = poisson(),
                            drop.unused.levels = FALSE,
                            nthreads = parallel::detectCores()-1,
                            discrete = TRUE)
      } else {
        ss_gam <- mgcv::bam(formula(formula),
                            data = data_train,
                            method = "fREML",
                            family = tw(),
                            drop.unused.levels = FALSE,
                            nthreads = parallel::detectCores()-1,
                            discrete = TRUE)
      }

    }

  # Fill in missing observations in data_train so the size of the dataset is correct when
  # building the JAGS model
  data_train$y[which(is.na(data_train$y))] <- round(predict(ss_gam,
                                                              newdata = data_train,
                                                              type = 'response'), 0)[which(is.na(data_train$y))]


  # Make jags file and appropriate data structures; note this has to use Poisson but the
  # resulting JAGS file will be modified to accomodate the specified response distribution accordingly
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

  # Update initial values of lambdas using the full estimates from the
  # fitted bam model to speed convergence; remove initial betas so that the
  # chains can start in very different regions of the parameter space
  ss_jagam$jags.ini$b <- NULL

  if(length(ss_gam$sp) == length(ss_jagam$jags.ini$lambda)){
    ss_jagam$jags.ini$lambda <- ss_gam$sp
    ss_jagam$jags.ini$lambda[log(ss_jagam$jags.ini$lambda) > 10] <- exp(10)
  }

  # Fill y with NAs if this is a simulation from the priors
  if(prior_simulation){
    data_train$y <- rep(NA, length(data_train$y))
  } else {
    if(family == 'tw'){
      # Add small offset for Tweedie so that any zeros won't cause failures based on
      # initial values
      data_train$y <- orig_y + 0.00001
    } else {
      data_train$y <- orig_y
    }
  }

  # Read in the base (unmodified) jags model file
  base_model <- suppressWarnings(readLines('base_gam.txt'))

  # Remove lines from the linear predictor section
  lines_remove <- c(1:grep('## response', base_model))
  base_model <- base_model[-lines_remove]

  # Any parametric effects in the gam (particularly the intercept) need to get more sensible priors to ensure they
  # do not directly compete with the latent trends
  if(any(grepl('Parametric effect priors', base_model))){

    in_parenth <- regmatches(base_model[grep('Parametric effect priors',
                               base_model) + 1],
               gregexpr( "(?<=\\().+?(?=\\))", base_model[grep('Parametric effect priors',
                                                               base_model) + 1], perl = T))[[1]][1]
    n_terms <- as.numeric(sub(".*:", "", in_parenth))
    ss_jagam$jags.data$p_coefs <- coef(ss_gam)[1:n_terms]

    rmvn <- function(n,mu,sig) {
      L <- mroot(sig);m <- ncol(L);
      t(mu + L%*%matrix(rnorm(m*n),m,n))
    }

    beta_sims <- rmvn(1000, coef(ss_gam), ss_gam$Vp)
    ss_jagam$jags.data$p_taus <- apply(as.matrix(beta_sims[,1:n_terms]),
                                       2, function(x) 1 / sd(x))

    base_model[grep('Parametric effect priors',
                      base_model) + 1] <- paste0('  for (i in 1:',
                                                 n_terms,
                                                 ') { b[i] ~ dnorm(p_coefs[i], p_taus[i]) }')
    base_model[grep('Parametric effect priors',
                    base_model)] <- c('  ## parametric effect priors (regularised for identifiability)')
  }

  # For any random effect smooths, use the non-centred parameterisation to avoid degeneracies
  smooth_labs <- do.call(rbind, lapply(seq_along(ss_gam$smooth), function(x){
    data.frame(label = ss_gam$smooth[[x]]$label, class = class(ss_gam$smooth[[x]])[1])
  }))

  if(any(smooth_labs$class == 'random.effect')){
    re_smooths <- smooth_labs %>%
      dplyr::filter(class == 'random.effect') %>%
      dplyr::pull(label)

    for(i in 1:length(re_smooths)){
      in_parenth <- regmatches(base_model[grep(re_smooths[i],
                                               base_model, fixed = T) + 1],
                               gregexpr( "(?<=\\().+?(?=\\))", base_model[grep(re_smooths[i],
                                                                               base_model, fixed = T) + 1],
                                         perl = T))[[1]][1]
      n_terms <- as.numeric(sub(".*:", "", in_parenth))
      n_start <- as.numeric(strsplit(sub(".*\\(", "", in_parenth), ':')[[1]][1])
      base_model[grep(re_smooths[i],
                      base_model, fixed = T) + 1] <- paste0('  for (i in ', n_start, ':',
                                                            n_terms,
                                                            ') { b_raw[i] ~ dnorm(0, 1)\n   b[i] <- b_raw[i] * ',
                                                            paste0('sigma_raw', i), ' \n  }\n  ',
                                                            paste0('sigma_raw', i), ' ~ dexp(1)')
      base_model[grep(re_smooths[i],
                      base_model, fixed = T)] <- paste0('  ## prior (non-centred) for ', re_smooths[i], '...')
    }

  }

  base_model[grep('smoothing parameter priors',
                  base_model)] <- c('   ## smoothing parameter priors...')

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


## State space trend estimates
for(s in 1:n_series) {
 trend[1, s] ~ dnorm(0, tau[s])
}

for(s in 1:n_series) {
 trend[2, s] ~ dnorm(phi[s] + ar1[s]*trend[1, s], tau[s])
}

for(s in 1:n_series) {
 trend[3, s] ~ dnorm(phi[s] + ar1[s]*trend[2, s] + ar2[s]*trend[1, s], tau[s])
}

for (i in 4:n){
 for (s in 1:n_series){
  trend[i, s] ~ dnorm(phi[s] + ar1[s]*trend[i - 1, s] + ar2[s]*trend[i - 2, s] + ar3[s]*trend[i - 3, s], tau[s])
 }
}

## AR components
for (s in 1:n_series){
 phi[s] ~ dnorm(0, 10)
 ar1[s] ~ dnorm(0, 10)
 ar2[s] ~ dnorm(0, 10)
 ar3[s] ~ dnorm(0, 10)
 tau[s] <- pow(sigma[s], -2)
 sigma[s] ~ dexp(1)
}

## Negative binomial likelihood functions
for (i in 1:n) {
 for (s in 1:n_series) {
  y[i, s] ~ dnegbin(rate[i, s], r[s])T(, upper_bound[s]);
  rate[i, s] <- ifelse((r[s] / (r[s] + mu[i, s])) < min_eps, min_eps,
                      (r[s] / (r[s] + mu[i, s])))
 }
}

## Complexity penalising prior for the overdispersion parameter;
## where the likelihood reduces to a 'base' model (Poisson) unless
## the data support overdispersion
for(s in 1:n_series){
 r[s] <- pow(r_raw[s], 2)
 r_raw[s] ~ dexp(0.05)
}


## Posterior predictions
for (i in 1:n) {
 for (s in 1:n_series) {
   ypred[i, s] ~ dnegbin(rate[i, s], r[s])T(, upper_bound[s])
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
      model_file[grep('phi\\[s\\] ~', model_file)] <- paste0(' phi[s] ~ ', phi_prior)
    }

    if(!missing(ar_prior)){
      model_file[grep('ar1\\[s\\] ~', model_file)] <- paste0(' ar1[s] ~ ', ar_prior)
      model_file[grep('ar2\\[s\\] ~', model_file)] <- paste0(' ar2[s] ~ ', ar_prior)
      model_file[grep('ar3\\[s\\] ~', model_file)] <- paste0(' ar3[s] ~ ', ar_prior)
    }

    if(!missing(tau_prior)){
      model_file[grep('tau\\[s\\] ~', model_file)] <- paste0(' tau[s] ~ ', tau_prior)
    }

    if(!missing(r_prior)){
      model_file[grep('r_raw\\[s\\] ~', model_file)] <- paste0('r_raw[s] ~ ', r_prior)
    }

    if(family == 'tw'){
      model_file[grep('## Negative binomial likelihood functions', model_file)] <- '## Tweedie likelihood functions'

      rate_begin <- grep('rate\\[i, s\\] <- ', model_file)
      rate_end <- rate_begin + 1
      model_file <- model_file[-c(rate_begin:rate_end)]

      odis_begin <- grep('r\\[s\\] <- ', model_file) - 4
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
      model_file[grep('linpred\\[i, s\\] ~', model_file)] <- '  linpred[i, s] ~ dgamma(shape[i, s, y_ind[i, s]], rate[i, s, y_ind[i, s]])\n  twlambda[i, s] <-'
      model_file <- readLines(textConnection(model_file), n = -1)

      model_file[grep('twlambda\\[i, s\\] <-', model_file)] <- '  twlambda[i, s] <- pow(mu[i, s], 2 - p) / (twdis[s] * (2 - p))\n  N_pois[i, s] ~'
      model_file <- readLines(textConnection(model_file), n = -1)

      model_file[grep('N_pois\\[i, s\\] ~', model_file)] <- '  N_pois[i, s] ~ dpois(twlambda[i, s])T(1,)\n  shape[i, s, 1] <-'
      model_file <- readLines(textConnection(model_file), n = -1)

      model_file[grep('shape\\[i, s, 1\\] <-', model_file)] <- '  shape[i, s, 1] <- N_pois[i, s] * ((2 - p) / (p - 1))\n  shape[i, s, 2] <-'
      model_file <- readLines(textConnection(model_file), n = -1)

      model_file[grep('shape\\[i, s, 2\\] <-', model_file)] <- '  shape[i, s, 2] <- 1\n  rate[i, s, 1] <-'
      model_file <- readLines(textConnection(model_file), n = -1)

      model_file[grep('rate\\[i, s, 1\\] <-', model_file)] <- '  rate[i, s, 1] <- 1 / (twdis[s] * (p - 1) * pow(mu[i, s], p - 1))\n  rate[i, s, 2] <-'
      model_file <- readLines(textConnection(model_file), n = -1)

      model_file[grep('rate\\[i, s, 2\\] <-', model_file)] <- '  rate[i, s, 2] <- exp(-twlambda[i, s])\n  pois_draw[i, s] ~'
      model_file <- readLines(textConnection(model_file), n = -1)

      model_file[grep('pois_draw\\[i, s\\] ~', model_file)] <- '  pois_draw[i, s] ~ dpois(mu[i, s])\n  is_zero[i, s] <-'
      model_file <- readLines(textConnection(model_file), n = -1)

      model_file[grep('is_zero\\[i, s\\] <-', model_file)] <- '  is_zero[i, s] <- equals(pois_draw[i, s], 1)\n  y_ind[i, s] <-'
      model_file <- readLines(textConnection(model_file), n = -1)

      model_file[grep('y_ind\\[i, s\\] <-', model_file)] <- '  y_ind[i, s] <- is_zero[i, s] + 1'
      model_file <- readLines(textConnection(model_file), n = -1)

      yind_begin <- grep('y_ind\\[i, s\\] <-', model_file)
      prior_line <- yind_begin + 2
      model_file[prior_line] <- '}\n\n## Tweedie power and overdispersion parameters\np <- 1.5\nfor (s in 1:n_series) {\n twdis_raw[s] ~ dbeta(4, 4);\n twdis[s] <- twdis_raw[s] + 0.5\n}'
      model_file <- readLines(textConnection(model_file), n = -1)

      if(!missing(twdis_prior)){
        twdis_begin <- grep('twdis_raw\\[s\\] ~', model_file)
        model_file <- model_file[-twdis_begin]
        model_file[twdis_begin] <- paste0(' twdis[s] ~ ', twdis_prior)
      }

    } else if(family == 'poisson'){
      model_file[grep('## Negative binomial likelihood functions', model_file)] <- '## Poisson likelihood functions'
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

    } else {
      if(missing(upper_bounds)){
        model_file[grep('y\\[i, s\\] ~', model_file)] <- '  y[i, s] ~ dnegbin(rate[i, s], r[s])'
        model_file[grep('ypred\\[i, s\\] ~', model_file)] <- '  ypred[i, s] ~ dnegbin(rate[i, s], r[s])'
      }
    }

    if(trend_model == 'None'){
      model_file[grep('trend\\[i, s\\] ~', model_file)] <- ' trend[i, s] <- 0'
      model_file[grep('trend\\[1, s\\] ~', model_file)] <- ' trend[1, s] <- 0'
      model_file[grep('trend\\[2, s\\] ~', model_file)] <- ' trend[2, s] <- 0'
      model_file[grep('trend\\[3, s\\] ~', model_file)] <- ' trend[3, s] <- 0'
      model_file[grep('phi\\[s\\] ~', model_file)] <- ' phi[s] <- 0'
      model_file[grep('ar1\\[s\\] ~', model_file)] <- ' ar1[s] <- 0'
      model_file[grep('ar2\\[s\\] ~', model_file)] <- ' ar2[s] <- 0'
      model_file[grep('ar3\\[s\\] ~', model_file)] <- ' ar3[s] <- 0'
    }

    if(trend_model == 'Noise'){
      model_file[grep('phi\\[s\\] ~', model_file)] <- ' phi[s] <- 0'
      model_file[grep('ar1\\[s\\] ~', model_file)] <- ' ar1[s] <- 0'
      model_file[grep('ar2\\[s\\] ~', model_file)] <- ' ar2[s] <- 0'
      model_file[grep('ar3\\[s\\] ~', model_file)] <- ' ar3[s] <- 0'
    }

    if(trend_model == 'RW'){
      model_file[grep('ar1\\[s\\] ~', model_file)] <- ' ar1[s] <- 1'
      model_file[grep('ar2\\[s\\] ~', model_file)] <- ' ar2[s] <- 0'
      model_file[grep('ar3\\[s\\] ~', model_file)] <- ' ar3[s] <- 0'
    }

    if(trend_model == 'AR1'){
      model_file[grep('ar2\\[s\\] ~', model_file)] <- ' ar2[s] <- 0'
      model_file[grep('ar3\\[s\\] ~', model_file)] <- ' ar3[s] <- 0'
    }

    if(trend_model == 'AR2'){
      model_file[grep('ar3\\[s\\] ~', model_file)] <- ' ar3[s] <- 0'
    }

    if(!drift){
      model_file[grep('phi\\[s\\] ~', model_file)] <- ' phi[s] <- 0'
    }

    # Use informative priors based on the fitted mgcv model to speed convergence
    # and eliminate searching over strange parameter spaces
    if(length(ss_gam$sp) == length(ss_jagam$jags.ini$lambda)){
       model_file[grep('lambda\\[i\\] ~', model_file)] <- '   lambda[i] ~ dexp(1/sp[i])'
    } else {
      model_file[grep('lambda\\[i\\] ~', model_file)] <- '   lambda[i] ~ dexp(0.05)'
    }

    model_file_jags <- textConnection(model_file)
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

        ## Latent factors evolve as time series with penalised precisions;
        ## the penalty terms force any un-needed factors to evolve as flat lines
        for(j in 1:n_lv){
         LV[1, j] ~ dnorm(0, penalty[j])
        }

        for(j in 1:n_lv){
         LV[2, j] ~ dnorm(phi[j] + ar1[j]*LV[1, j], penalty[j])
        }

        for(j in 1:n_lv){
         LV[3, j] ~ dnorm(phi[j] + ar1[j]*LV[2, j] + ar2[j]*LV[1, j], penalty[j])
        }

        for(i in 4:n){
         for(j in 1:n_lv){
          LV[i, j] ~ dnorm(phi[j] + ar1[j]*LV[i - 1, j] +
                          ar2[j]*LV[i - 2, j] + ar3[j]*LV[i - 3, j], penalty[j])
         }
        }

        ## AR components
        for (s in 1:n_lv){
        phi[s] ~ dnorm(0, 10)
        ar1[s] ~ dnorm(0, 10)
        ar2[s] ~ dnorm(0, 10)
        ar3[s] ~ dnorm(0, 10)
        }

        ## Shrinkage penalties for each factor squeeze the factor to a flat line and squeeze
        ## the entire factor toward a flat white noise process if supported by
        ## the data. The prior for individual factor penalties allows each factor to possibly
        ## have a relatively large penalty, which shrinks the prior for that factor's variance
        ## substantially. Penalties increase exponentially with the number of factors following
        ## Welty, Leah J., et al. Bayesian distributed lag models: estimating effects of particulate
        ## matter air pollution on daily mortality Biometrics 65.1 (2009): 282-291.
        pi ~ dunif(0, n_lv)
        X2 ~ dnorm(0, 1)T(0, )

        # eta1 controls the baseline penalty
        eta1 ~ dunif(-1, 1)

        # eta2 controls how quickly the penalties exponentially increase
        eta2 ~ dunif(-1, 1)

        for(t in 1:n_lv){
         X1[t] ~ dnorm(0, 1)T(0, )
         l.dist[t] <- max(t, pi[])
         l.weight[t] <- exp(eta2[] * l.dist[t])
         l.var[t] <- exp(eta1[] * l.dist[t] / 2) * 1
         theta.prime[t] <- l.weight[t] * X1[t] + (1 - l.weight[t]) * X2[]
         penalty[t] <- max(min_eps, theta.prime[t] * l.var[t])
        }

        ## Latent factor loadings: standard normal with identifiability constraints
        ## Upper triangle of loading matrix set to zero
        for(j in 1:(n_lv - 1)){
          for(j2 in (j + 1):n_lv){
           lv_coefs[j, j2] <- 0
          }
         }

        ## Positive constraints on loading diagonals
        for(j in 1:n_lv) {
         lv_coefs[j, j] ~ dnorm(0, 1)T(0, 1);
        }

        ## Lower diagonal free
        for(j in 2:n_lv){
         for(j2 in 1:(j - 1)){
          lv_coefs[j, j2] ~ dnorm(0, 1)T(-1, 1);
         }
       }

        ## Other elements also free
        for(j in (n_lv + 1):n_series) {
         for(j2 in 1:n_lv){
          lv_coefs[j, j2] ~ dnorm(0, 1)T(-1, 1);
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
        y[i, s] ~ dnegbin(rate[i, s], r[s])T(, upper_bound[s]);
        rate[i, s] <- ifelse((r[s] / (r[s] + mu[i, s])) < min_eps, min_eps,
                            (r[s] / (r[s] + mu[i, s])))
        }
        }

        ## Complexity penalising prior for the overdispersion parameter;
        ## where the likelihood reduces to a 'base' model (Poisson) unless
        ## the data support overdispersion
        for(s in 1:n_series){
         r[s] <- pow(r_raw[s], 2)
         r_raw[s] ~ dexp(0.05)
        }

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
        model_file[grep('phi\\[s\\] ~', model_file)] <- paste0('        phi[s] ~ ', phi_prior)
      }

      if(!missing(ar_prior)){
        model_file[grep('ar1\\[s\\] ~', model_file)] <- paste0('        ar1[s] ~ ', ar_prior)
        model_file[grep('ar2\\[s\\] ~', model_file)] <- paste0('        ar2[s] ~ ', ar_prior)
        model_file[grep('ar3\\[s\\] ~', model_file)] <- paste0('        ar3[s] ~ ', ar_prior)
      }

      if(!missing(r_prior)){
        model_file[grep('r_raw\\[s\\] ~', model_file)] <- paste0('r_raw[s] ~ ', r_prior)
      }

      if(family == 'tw'){
        model_file[grep('## Negative binomial likelihood functions', model_file)] <- '## Tweedie likelihood functions'

        rate_begin <- grep('rate\\[i, s\\] <- ', model_file)
        rate_end <- rate_begin + 1
        model_file <- model_file[-c(rate_begin:rate_end)]

        odis_begin <- grep('r\\[s\\] <- ', model_file) - 4
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
        model_file[grep('linpred\\[i, s\\] ~', model_file)] <- '  linpred[i, s] ~ dgamma(shape[i, s, y_ind[i, s]], rate[i, s, y_ind[i, s]])\n  twlambda[i, s] <-'
        model_file <- readLines(textConnection(model_file), n = -1)

        model_file[grep('twlambda\\[i, s\\] <-', model_file)] <- '  twlambda[i, s] <- pow(mu[i, s], 2 - p) / (twdis[s] * (2 - p))\n  N_pois[i, s] ~'
        model_file <- readLines(textConnection(model_file), n = -1)

        model_file[grep('N_pois\\[i, s\\] ~', model_file)] <- '  N_pois[i, s] ~ dpois(twlambda[i, s])T(1,)\n  shape[i, s, 1] <-'
        model_file <- readLines(textConnection(model_file), n = -1)

        model_file[grep('shape\\[i, s, 1\\] <-', model_file)] <- '  shape[i, s, 1] <- N_pois[i, s] * ((2 - p) / (p - 1))\n  shape[i, s, 2] <-'
        model_file <- readLines(textConnection(model_file), n = -1)

        model_file[grep('shape\\[i, s, 2\\] <-', model_file)] <- '  shape[i, s, 2] <- 1\n  rate[i, s, 1] <-'
        model_file <- readLines(textConnection(model_file), n = -1)

        model_file[grep('rate\\[i, s, 1\\] <-', model_file)] <- '  rate[i, s, 1] <- 1 / (twdis[s] * (p - 1) * pow(mu[i, s], p - 1))\n  rate[i, s, 2] <-'
        model_file <- readLines(textConnection(model_file), n = -1)

        model_file[grep('rate\\[i, s, 2\\] <-', model_file)] <- '  rate[i, s, 2] <- exp(-twlambda[i, s])\n  pois_draw[i, s] ~'
        model_file <- readLines(textConnection(model_file), n = -1)

        model_file[grep('pois_draw\\[i, s\\] ~', model_file)] <- '  pois_draw[i, s] ~ dpois(mu[i, s])\n  is_zero[i, s] <-'
        model_file <- readLines(textConnection(model_file), n = -1)

        model_file[grep('is_zero\\[i, s\\] <-', model_file)] <- '  is_zero[i, s] <- equals(pois_draw[i, s], 1)\n  y_ind[i, s] <-'
        model_file <- readLines(textConnection(model_file), n = -1)

        model_file[grep('y_ind\\[i, s\\] <-', model_file)] <- '  y_ind[i, s] <- is_zero[i, s] + 1'
        model_file <- readLines(textConnection(model_file), n = -1)

        yind_begin <- grep('y_ind\\[i, s\\] <-', model_file)
        prior_line <- yind_begin + 2
        model_file[prior_line] <- '}\n\n## Tweedie power and overdispersion parameters\np <- 1.5\nfor (s in 1:n_series) {\n twdis_raw[s] ~ dbeta(4, 4);\n twdis[s] <- twdis_raw[s] + 0.5\n}'
        model_file <- readLines(textConnection(model_file), n = -1)

        if(!missing(twdis_prior)){
          twdis_begin <- grep('twdis_raw\\[s\\] ~', model_file)
          model_file <- model_file[-twdis_begin]
          model_file[twdis_begin] <- paste0(' twdis[s] ~ ', twdis_prior)
        }

      } else if(family == 'poisson'){
        model_file[grep('## Negative binomial likelihood functions', model_file)] <- '## Poisson likelihood functions'
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

      } else {
        if(missing(upper_bounds)){
          model_file[grep('y\\[i, s\\] ~', model_file)] <- '  y[i, s] ~ dnegbin(rate[i, s], r[s])'
          model_file[grep('ypred\\[i, s\\] ~', model_file)] <- '  ypred[i, s] ~ dnegbin(rate[i, s], r[s])'
        }
      }

      if(trend_model == 'None'){
        model_file[grep('trend\\[i, s\\] ~', model_file)] <- '      trend[i, s] <- 0'
        model_file[grep('phi\\[s\\] ~', model_file)] <- '         phi[s] <- 0'
        model_file[grep('ar1\\[s\\] ~', model_file)] <- '        ar1[s] <- 0'
        model_file[grep('ar2\\[s\\] ~', model_file)] <- '        ar2[s] <- 0'
        model_file[grep('ar3\\[s\\] ~', model_file)] <- '        ar3[s] <- 0'
      }

      if(trend_model == 'Noise'){
        model_file[grep('phi\\[s\\] ~', model_file)] <- ' phi[s] <- 0'
        model_file[grep('ar1\\[s\\] ~', model_file)] <- ' ar1[s] <- 0'
        model_file[grep('ar2\\[s\\] ~', model_file)] <- ' ar2[s] <- 0'
        model_file[grep('ar3\\[s\\] ~', model_file)] <- ' ar3[s] <- 0'
      }

      if(trend_model == 'RW'){
        model_file[grep('ar1\\[s\\] ~', model_file)] <- '        ar1[s] <- 1'
        model_file[grep('ar2\\[s\\] ~', model_file)] <- '        ar2[s] <- 0'
        model_file[grep('ar3\\[s\\] ~', model_file)] <- '        ar3[s] <- 0'
      }

      if(trend_model == 'AR1'){
        model_file[grep('ar2\\[s\\] ~', model_file)] <- '        ar2[s] <- 0'
        model_file[grep('ar3\\[s\\] ~', model_file)] <- '        ar3[s] <- 0'
      }

      if(trend_model == 'AR2'){
        model_file[grep('ar3\\[s\\] ~', model_file)] <- '        ar3[s] <- 0'
      }

      if(!drift){
        model_file[grep('phi\\[s\\] ~', model_file)] <- '        phi[s] <- 0'
      }

      # Use informative priors based on the fitted mgcv model to speed convergence
      # and eliminate searching over strange parameter spaces
      if(length(ss_gam$sp) == length(ss_jagam$jags.ini$lambda)){
        model_file[grep('lambda\\[i\\] ~', model_file)] <- '   lambda[i] ~ dexp(1/sp[i])'
      } else {
        model_file[grep('lambda\\[i\\] ~', model_file)] <- '   lambda[i] ~ dexp(0.05)'
      }

      model_file_jags <- textConnection(model_file)

  }

  # Covariate dataframe including training and testing observations
  if(!missing(data_test)){
    X <- data.frame(rbind(ss_jagam$jags.data$X,
                          predict(ss_gam, newdata = data_test, type = 'lpmatrix')))

    # Add a time variable
    if(class(data_train)[1] == 'list'){
      temp_dat_train <- data.frame(time = data_train$time,
                                   series = data_train$series)
      temp_dat_test <- data.frame(time = data_test$time,
                                  series = data_test$series)

      X$time <- rbind(temp_dat_train, temp_dat_test) %>%
        dplyr::left_join(rbind(temp_dat_train, temp_dat_test) %>%
                           dplyr::select(time) %>%
                           dplyr::distinct() %>%
                           dplyr::arrange(time) %>%
                           dplyr::mutate(time = dplyr::row_number()),
                         by = c('time')) %>%
        dplyr::pull(time)

      # Add a series identifier variable
      X$series <- as.numeric(rbind(temp_dat_train, temp_dat_test)$series)

      # Add an outcome variable
      X$outcome <- c(orig_y, rep(NA, NROW(temp_dat_test)))

    } else {

    X$time <- rbind(data_train, data_test[,1:ncol(data_train)]) %>%
      dplyr::left_join(rbind(data_train, data_test[,1:ncol(data_train)]) %>%
                         dplyr::select(time) %>%
                         dplyr::distinct() %>%
                         dplyr::arrange(time) %>%
                         dplyr::mutate(time = dplyr::row_number()),
                       by = c('time')) %>%
      dplyr::pull(time)

    # Add a series identifier variable
    X$series <- as.numeric(rbind(data_train, data_test)$series)

    # Add an outcome variable
    X$outcome <- c(data_train$y, rep(NA, NROW(data_test)))
    }

  } else {
    X <- data.frame(ss_jagam$jags.data$X)

    if(class(data_train)[1] == 'list'){
      temp_dat <- data.frame(time = data_train$time)
      X$time <- temp_dat %>%
        dplyr::left_join(temp_dat %>%
                           dplyr::select(time) %>%
                           dplyr::distinct() %>%
                           dplyr::arrange(time) %>%
                           dplyr::mutate(time = dplyr::row_number()),
                         by = c('r')) %>%
        dplyr::pull(time)
    } else {
      X$time <- data_train %>%
        dplyr::left_join(data_train %>%
                           dplyr::select(time) %>%
                           dplyr::distinct() %>%
                           dplyr::arrange(time) %>%
                           dplyr::mutate(time = dplyr::row_number()),
                         by = c('time')) %>%
        dplyr::pull(time)
    }

    X$outcome <- c(data_train$y)
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

  if(length(ss_gam$sp) == length(ss_jagam$jags.ini$lambda)){
    ss_jagam$jags.data$sp <- ss_gam$sp
  }

  # Machine epsilon for minimum allowable non-zero rate
  if(family == 'nb'){
    ss_jagam$jags.data$min_eps <- .Machine$double.eps
  }

  # Number of latent variables to use
  if(use_lv){
      if(missing(n_lv)){
        ss_jagam$jags.data$n_lv <- min(2, floor(ss_jagam$jags.data$n_series / 2))
      } else {
        ss_jagam$jags.data$n_lv <- n_lv
        ss_jagam$jags.ini$X1 <- rep(1, n_lv)
        ss_jagam$jags.ini$X2 <- 1
      }
      if(ss_jagam$jags.data$n_lv > ss_jagam$jags.data$n_series){
        stop('Number of latent variables cannot be greater than number of series')
      }

  }

  # Initiate adaptation of the model for the full burnin period. This is necessary as JAGS
  # will take a while to optimise the samplers, so long adaptation with little 'burnin'
  # is more crucial than little adaptation but long 'burnin' https://mmeredith.net/blog/2016/Adapt_or_burn.htm

  # Gather posterior samples for the specified parameters
  if(!use_lv){
    if(family == 'nb'){
      param <- c('rho', 'b', 'mu', 'ypred',  'r', 'phi',
                 'tau', 'trend', 'ar1', 'ar2', 'ar3', 'sigma')
    } else if(family == 'poisson'){
      param <- c('rho', 'b', 'mu', 'ypred',  'phi',
                 'tau', 'trend', 'ar1', 'ar2', 'ar3', 'sigma')
    } else {
      param <- c('rho', 'b', 'mu', 'ypred',  'phi', 'p', 'twdis',
                 'tau', 'trend', 'ar1', 'ar2', 'ar3', 'sigma')
    }

  } else {
    if(family == 'nb'){
    param <- c('rho', 'b', 'mu', 'ypred',  'r', 'phi', 'LV',
               'trend', 'lv_coefs', 'penalty',
               'ar1', 'ar2', 'ar3')
    } else if(family == 'poisson'){
      param <- c('rho', 'b', 'mu', 'ypred', 'phi', 'LV',
                 'trend', 'lv_coefs', 'penalty',
                 'ar1', 'ar2', 'ar3')
    } else {
      param <- c('rho', 'b', 'mu', 'ypred', 'phi',
                 'p', 'twdis', 'LV',
                 'trend', 'lv_coefs', 'penalty',
                 'ar1', 'ar2', 'ar3')
    }
  }

  if(missing(upper_bounds)){
    upper_bounds <- NULL
  }

  if(use_lv){
    n_lv <- ss_jagam$jags.data$n_lv
  } else {
    n_lv <- NULL
  }

  if(!run_model){
    # Return only the model file and all data / inits needed to run the model
    # outside of mvgam
    output <- list(call = formula,
                   family = dplyr::case_when(family == 'tw' ~ 'Tweedie',
                                             family == 'poisson' ~ 'Poisson',
                                             TRUE ~ 'Negative Binomial'),
                   pregam = ss_jagam$pregam,
                   model_file = model_file,
                   jags_data = ss_jagam$jags.data,
                   jags_inits = ss_jagam$jags.ini,
                   mgcv_model = ss_gam,
                   sp_names = names(ss_jagam$pregam$lsp0),
                   ytimes = ytimes,
                   use_lv = use_lv,
                   n_lv = n_lv,
                   upper_bounds = upper_bounds,
                   obs_data = data_train)

  } else {
  initlist <- replicate(chains, ss_jagam$jags.ini,
                          simplify = FALSE)
  runjags::runjags.options(silent.jags=TRUE, silent.runjags=TRUE)
  n.burn <- burnin
  unlink('base_gam.txt')
  cat(model_file, file = 'base_gam.txt', sep = '\n', append = T)
  if(parallel){
    cl <- parallel::makePSOCKcluster(min(c(chains, parallel::detectCores() - 1)))
    setDefaultCluster(cl)
    gam_mod <- runjags::run.jags(model = 'base_gam.txt',
                                 data = ss_jagam$jags.data,
                                 modules = 'glm',
                                 inits = initlist,
                                 n.chains = chains,
                                 # Rely on long adaptation to tune samplers appropriately
                                 adapt = max(1000, n.burn - 1000),
                                 burnin = 1000,
                                 sample = n_samples,
                                 jags = jags_path,
                                 thin = thin,
                                 method = "rjparallel",
                                 monitor = param,
                                 silent.jags = TRUE,
                                 cl = cl)
    stopCluster(cl)
  } else {
    gam_mod <- runjags::run.jags(model = 'base_gam.txt',
                                 data = ss_jagam$jags.data,
                                 modules = 'glm',
                                 inits = initlist,
                                 n.chains = chains,
                                 # Rely on long adaptation to tune samplers appropriately
                                 adapt = max(1000, n.burn - 1000),
                                 burnin = 1000,
                                 sample = n_samples,
                                 jags = jags_path,
                                 thin = thin,
                                 method = "rjags",
                                 monitor = param,
                                 silent.jags = TRUE)
  }

  out_gam_mod <- coda::as.mcmc.list(gam_mod)
  unlink('base_gam.txt')
  unlink(fil)

  # Remove the small offset for Tweedie
  if(family == 'tw'){
    data_train$y <- orig_y
  }

  # Get Dunn-Smyth Residual distributions for each series
  series_resids <- get_mvgam_resids(object = list(
    jags_output = out_gam_mod,
    family = dplyr::case_when(family == 'tw' ~ 'Tweedie',
                              family == 'poisson' ~ 'Poisson',
                              TRUE ~ 'Negative Binomial'),
    obs_data = data_train,
    ytimes = ytimes),
    n_cores = min(c(parallel::detectCores() - 1, NCOL(ytimes))))

  # Get smooth penalty names in more interpretable format
  sam <- suppressMessages(jags.samples(runjags::as.jags(gam_mod, adapt = 10, quiet = T),
                                       c("b","rho"), n.iter = 100, thin = 1))
  jam <- mgcv::sim2jam(sam, ss_jagam$pregam, edf.type = 1)
  jam$sp <- exp(sam$rho)

  name_starts <- unlist(purrr:::map(jam$smooth, 'first.sp'))
  name_ends <- unlist(purrr:::map(jam$smooth, 'last.sp'))

  rho_names <- unlist(lapply(seq(1:length(ss_gam$smooth)), function(i){

    number_seq <- seq(1:(1 + name_ends[i] - name_starts[i]))
    number_seq[1] <- ''

    paste0(rep(ss_gam$smooth[[i]]$label,
               length(number_seq)),
           number_seq)
  }))


  if(return_jags_data){
    output <- list(call = formula,
                   family = dplyr::case_when(family == 'tw' ~ 'Tweedie',
                                             family == 'poisson' ~ 'Poisson',
                                             TRUE ~ 'Negative Binomial'),
                   pregam = ss_jagam$pregam,
                   jags_output = out_gam_mod,
                   model_file = model_file,
                   sp_names = rho_names,
                   jags_data = ss_jagam$jags.data,
                   jags_inits = ss_jagam$jags.ini,
                   mgcv_model = ss_gam,
                   jam_model = jam,
                   jags_model = gam_mod,
                   ytimes = ytimes,
                   resids = series_resids,
                   use_lv = use_lv,
                   n_lv = n_lv,
                   upper_bounds = upper_bounds,
                   obs_data = data_train)
  } else {
    output <- list(call = formula,
                   family = dplyr::case_when(family == 'tw' ~ 'Tweedie',
                                             family == 'poisson' ~ 'Poisson',
                                             TRUE ~ 'Negative Binomial'),
                   pregam = ss_jagam$pregam,
                   jags_output = out_gam_mod,
                   model_file = model_file,
                   sp_names = rho_names,
                   mgcv_model = ss_gam,
                   jags_model = gam_mod,
                   jam_model = jam,
                   ytimes = ytimes,
                   resids = series_resids,
                   use_lv = use_lv,
                   n_lv = n_lv,
                   upper_bounds = upper_bounds,
                   obs_data = data_train)
  }
  }

  return(output)
}
