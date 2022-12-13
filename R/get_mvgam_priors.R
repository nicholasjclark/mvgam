#'Extract information on default prior distributions for an mvgam model
#'
#'This function lists the parameters that can have their prior distributions
#'changed for a given `mvgam` model, as well listing their default distributions
#'
#'
#'@param formula A \code{character} string specifying the GAM formula. These are exactly like the formula
#'for a GLM except that smooth terms, s, te, ti and t2, can be added to the right hand side
#'to specify that the linear predictor depends on smooth functions of predictors (or linear functionals of these)
#'@param data A \code{dataframe} or \code{list} containing the model response variable and covariates
#'required by the GAM \code{formula}. Should include columns:
#''y' (the discrete outcomes; \code{NA}s allowed)
#''series' (character or factor index of the series IDs)
#''time' (numeric index of the time point for each observation).
#'Any other variables to be included in the linear predictor of \code{formula} must also be present
#'@param data_train Deprecated. Still works in place of \code{data} but users are recommended to use
#'\code{data} instead for more seamless integration into `R` workflows
#'@param family \code{character}. Must be either 'nb' (for Negative Binomial), 'tw' (for Tweedie) or 'poisson'
#'@param use_lv \code{logical}. If \code{TRUE}, use dynamic factors to estimate series'
#'latent trends in a reduced dimension format. If \code{FALSE}, estimate independent latent trends for each series
#'@param n_lv \code{integer} the number of latent dynamic factors to use if \code{use_lv == TRUE}.
#'Cannot be \code{>n_series}. Defaults arbitrarily to \code{min(2, floor(n_series / 2))}
#'@param trend_model \code{character} specifying the time series dynamics for the latent trend. Options are:
#''None' (no latent trend component; i.e. the GAM component is all that contributes to the linear predictor,
#'and the observation process is the only source of error; similarly to what is estimated by \code{\link[mcgv]{gam}}),
#''RW' (random walk with possible drift),
#''AR1' (AR1 model with intercept),
#''AR2' (AR2 model with intercept) or
#''AR3' (AR3 model with intercept) or
#''GP' (Gaussian Process with squared exponential kernel;
#'only available for estimation in \code{Stan})
#'@param drift \code{logical} estimate a drift parameter in the latent trend components. Useful if the latent
#'trend is expected to broadly follow a non-zero slope. Note that if the latent trend is more or less stationary,
#'the drift parameter can become unidentifiable, especially if an intercept term is included in the GAM linear
#'predictor (which it is by default when calling \code{\link[mcgv]{jagam}}). Therefore this defaults to \code{FALSE}
#'@param use_stan Logical. If \code{TRUE} and if \code{rstan} is installed, the model will be compiled and sampled using
#'the Hamiltonian Monte Carlo with a call to \code{\link[cmdstanr]{cmdstan_model}} or, if `cmdstanr` is not available,
#'a call to \code{\link[rstan]{stan}}. Note that this functionality is still in development and
#'not all options that are available in \code{JAGS} can be used, including: no option for a Tweedie family and no option for
#'dynamic factor trends. However, as \code{Stan} can estimate Hilbert base approximate Gaussian Processes, which
#'are much more computationally tractable than full GPs for time series with `>100` observations, estimation
#'in \code{Stan} can support latent GP trends while estimation in \code{JAGS} cannot
#'@details Users can supply a model formula, prior to fitting the model, so that default priors can be inspected and
#'altered. To make alterations, change the contents of the `prior` column and supplying this
#'\code{data.frame} to the `mvgam` function using the argument `priors`
#' @note Only the `prior` column of the output should be altered when defining
#' the user-defined priors for the `mvgam` model
#'@author Nicholas J Clark
#'@seealso \code{\link{mvgam}}
#'@return either a \code{data.frame} containing the prior definitions (if any suitable
#'priors can be altered by the user) or \code{NULL}, indicating that no priors in the model
#'can be modified through the `mvgam` interface
#'
#'@examples
#'# Simulate three integer-valued time series
#'library(mvgam)
#'dat <- sim_mvgam(trend_rel = 0.5)
#'
#'# Get a model file that uses default mvgam priors for inspection (not always necessary,
#'# but this can be useful for testing whether your updated priors are written correctly)
#'mod_default <- mvgam(y ~ s(series, bs = 're') +
#'               s(season, bs = 'cc') - 1,
#'               family = 'nb',
#'               data = dat$data_train,
#'               trend_model = 'AR2',
#'               priors = test_priors,
#'               run_model = FALSE)
#'
#'# Inspect the model file with default mvgam priors
#'mod_default$model_file
#'
#'# Look at which priors can be updated in mvgam
#'test_priors <- get_mvgam_priors(y ~ s(series, bs = 're') +
#'                                 s(season, bs = 'cc') - 1,
#'                                 family = 'nb',
#'                                 data = dat$data_train,
#'                                 trend_model = 'AR2')
#'test_priors
#'
#'# Make a few changes; first, change the population mean for the series-level
#'# random intercepts
#'test_priors$prior[1] <- 'mu_raw1 ~ dnorm(-1, 2)'
#'
#'# Now use stronger regularisation for the series-level AR2 coefficients
#'test_priors$prior[4] <- 'ar2[s] ~ dnorm(0, 20)'
#'
#'# Check whether the notation in the 'prior' column is correct (note, this function
#'# will not check that densities are correctly spelled and parameterised, that is up
#'# to the user!)
#'update_priors(model_file = mod_default$model_file,
#'               priors = test_priors)
#'
#'# No warnings, the model is ready for fitting now in the usual way with the addition
#'# of the 'priors' argument:
#'# mod <- mvgam(y ~ s(series, bs = 're') +
#'#                s(season, bs = 'cc') - 1,
#'#              family = 'nb',
#'#              data = dat$data_train,
#'#              trend_model = 'AR2',
#'#              priors = test_priors)
#'
#'# Look at what is returned when an incorrect spelling is used
#'test_priors$prior[4] <- 'ar2_bananas ~ dnorm(0, 20)'
#'update_priors(model_file = mod_default$model_file,
#'               priors = test_priors)
#'
#'@export
get_mvgam_priors = function(formula,
                            data,
                            data_train,
                            family = 'poisson',
                            use_lv = FALSE,
                            n_lv,
                            use_stan = FALSE,
                            trend_model = 'None',
                            drift = FALSE){

  # Check arguments
  trend_model <- match.arg(arg = trend_model, choices = c("None", "RW", "AR1",
                                                          "AR2", "AR3", "GP"))
  if(class(family) == 'family'){
    family <- family$family
  }
  family <- match.arg(arg = family, choices = c("nb", "poisson", "tw"))

  if(missing("data") & missing("data_train")){
    stop('Argument "data" is missing with no default')
  }

  if(!missing("data")){
    data_train <- data
  }

  # JAGS cannot support latent GP trends as there is no easy way to use Hilbert base
  # approximation to reduce the computational demands
  if(!use_stan & trend_model == 'GP'){
    warning('gaussian process trends not yet supported for JAGS; reverting to Stan')
    use_stan <- TRUE
  }

  if(use_stan & family == 'tw'){
    warning('Tweedie family not yet supported for stan; reverting to JAGS')
    use_stan <- FALSE
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
      stop('data does not contain a "time" column')
    }
  }

  if(class(data_train)[1] == 'list'){
    if(!'series' %in% names(data_train)){
      data_train$series <- factor('series1')
      if(!missing(data_test)){
        data_test$series <- factor('series1')
      }
    }

    if(!'time' %in% names(data_train)){
      stop('data does not contain a "time" column')
    }
  }

  # Number of latent variables cannot be greater than number of series
  if(use_lv){
    if(missing(n_lv)){
      n_lv <- min(2, floor(length(unique(data_train$series)) / 2))
    }
    if(n_lv > length(unique(data_train$series))){
      stop('number of latent variables cannot be greater than number of series')
    }
  }

  # No point in latent variables if trend model is None
  if(trend_model == 'None' & use_lv){
    use_lv <- FALSE
    warning('No point in latent variables if trend model is None; changing use_lv to FALSE')
  }

  # Ensure outcome is labelled 'y'
  form_terms <- terms(formula(formula))
  if(terms(formula(formula))[[2]] != 'y'){
    stop('Outcome variable must be named "y"')
  }

  # Use a small fit from mgcv to extract relevant information on smooths included
  # in the model
  if(family == 'nb'){
    ss_gam <- mgcv::gam(formula(formula),
                        data = data_train,
                        method = "REML",
                        family = nb(),
                        drop.unused.levels = FALSE,
                        control = list(nthreads = min(4, parallel::detectCores()-1),
                                       maxit = 30))
  } else if(family == 'poisson'){
    ss_gam <- mgcv::gam(formula(formula),
                        data = data_train,
                        method = "REML",
                        family = poisson(),
                        drop.unused.levels = FALSE,
                        control = list(nthreads = min(4, parallel::detectCores()-1),
                                       maxit = 30))
  } else {
    ss_gam <- mgcv::gam(formula(formula),
                        data = data_train,
                        method = "REML",
                        family = tw(),
                        drop.unused.levels = FALSE,
                        control = list(nthreads = min(4, parallel::detectCores()-1),
                                       maxit = 30))
  }


  # Check if smooth terms are included in the formula
  if(length(ss_gam$smooth) == 0){
    smooths_included <- FALSE
  } else {
    smooths_included <- TRUE
  }

  # Extract information on the number of smoothing parameters and
  # random effects
  smooth_labs <- do.call(rbind, lapply(seq_along(ss_gam$smooth), function(x){
    data.frame(label = ss_gam$smooth[[x]]$label,
               class = class(ss_gam$smooth[[x]])[1],
               nsp = ss_gam$smooth[[x]]$last.sp -
                 ss_gam$smooth[[x]]$first.sp + 1)
  }))

  # Smoothing parameter priors for non random effect smooths
  if(any(smooth_labs$class != 'random.effect')){
    n_smooth_params <- smooth_labs %>%
      dplyr::filter(class != 'random.effect') %>%
      dplyr::pull(nsp)
    nonre_smooths <- smooth_labs %>%
      dplyr::filter(class != 'random.effect') %>%
      dplyr::pull(label)


    if(use_stan){
      sp_df <- data.frame(param_name = 'lambda<lower=0>',
                          param_length = sum(smooth_labs$nsp),
                          param_info = c(paste(nonre_smooths,
                                               'smooth parameters',
                                               collapse = ', ')),
                          prior = 'lambda ~ normal(30, 25);',
                          # Add an example for changing the prior; note that it is difficult to
                          # understand how to change individual smoothing parameter priors because each
                          # one acts on a different subset of the smooth function parameter space
                          example_change = c(
                            paste0('lambda ~ exponential(',
                                   round(runif(min = 0.01, max = 1, n = 1), 2),
                                   ');'
                            )))
    } else {
      # Not recommended to alter smoothing parameter priors for JAGS as the Gibbs sampler
      # needs to have informative priors to have any hope of convergence
      sp_df <- NULL
    }

  } else {
    sp_df <- NULL
  }

  # Population mean and sd priors for random effect smooths
  if(any(smooth_labs$class == 'random.effect')){
    re_smooths <- smooth_labs %>%
      dplyr::filter(class == 'random.effect') %>%
      dplyr::pull(label)
    n_re_terms <- length(re_smooths)

    if(use_stan){
      re_df <- data.frame(param_name = c('mu_raw',
                                         'sigma_raw<lower=0>'),
                          param_length = rep(n_re_terms, 2),
                          param_info = c(paste(re_smooths, 'pop mean',
                                               collapse = ', '),
                                         paste(re_smooths, 'pop sd',
                                               collapse = ', ')),
                          prior = c('mu_raw ~ std_normal();',
                                    'sigma_raw ~ exponential(0.5);'))

      # Add example change that users could implement to put different priors
      # on each re's mean and sd
      if(n_re_terms > 1){
        re_df <- cbind(re_df,
                       data.frame(example_change = c(paste(paste0('mu_raw[', 1:n_re_terms,
                                                                  '] ~ normal(',
                                                                  round(runif(min = -1,
                                                                              max = 1,
                                                                              n = n_re_terms), 2),', ',
                                                                  round(runif(min = 0.1,
                                                                              max = 1,
                                                                              n = n_re_terms), 2),
                                                                  ');'), collapse = '\n'),
                                                     paste(paste0('sigma_raw[', 1:n_re_terms,
                                                                  '] ~ exponential(',
                                                                  round(runif(min = 0.01,
                                                                              max = 1,
                                                                              n = n_re_terms), 2),
                                                                  ');'), collapse = '\n'))))
      } else {
        re_df <- cbind(re_df,
                       data.frame(example_change = c(paste0(
                         'mu_raw ~ normal(',
                         round(runif(min = -1, max = 1, n = 1), 2),
                         ', ',
                         round(runif(min = 0.1, max = 1, n = 1), 2),
                         ');'
                       ),
                       paste0('sigma_raw ~ exponential(',
                              round(runif(min = 0.01, max = 1, n = 1), 2),
                              ');'
                       ))))
      }

    } else {
      # If using JAGS as the backend
      re_df <- data.frame(param_name = c(paste0('mu_raw', 1:n_re_terms),
                                         paste0('sigma_raw', 1:n_re_terms, '<lower=0>')),
                          param_length = 1,
                          param_info = c(paste(re_smooths, 'pop mean'),
                                         paste(re_smooths, 'pop sd')),
                          prior = c(paste0('mu_raw', 1:n_re_terms, ' ~ dnorm(0, 1)'),
                                    paste0('sigma_raw', 1:n_re_terms, ' ~ dexp(0.5)')))

      # Add example change that users could implement to put different priors
      # on each re's mean and sd
      if(n_re_terms > 1){
        re_df <- cbind(re_df,
                       data.frame(example_change = c(paste(paste0('mu_raw', 1:n_re_terms,
                                                                  ' ~ dnorm(',
                                                                  round(runif(min = -1,
                                                                              max = 1,
                                                                              n = n_re_terms), 2),', ',
                                                                  round(runif(min = 0.1,
                                                                              max = 10,
                                                                              n = n_re_terms), 2),
                                                                  ')')),
                                                     paste(paste0('sigma_raw', 1:n_re_terms,
                                                                  ' ~ dexp(',
                                                                  round(runif(min = 0.01,
                                                                              max = 1,
                                                                              n = n_re_terms), 2),
                                                                  ')')))))
      } else {
        re_df <- cbind(re_df,
                       data.frame(example_change = c(paste0(
                         'mu_raw ~ dnorm(',
                         round(runif(min = -1, max = 1, n = 1), 2),
                         ', ',
                         round(runif(min = 0.1, max = 1, n = 1), 2),
                         ')'
                       ),
                       paste0('sigma_raw ~ dexp(',
                              round(runif(min = 0.01, max = 1, n = 1), 2),
                              ')'
                       ))))
      }
    }


  } else {
    re_df <- NULL
  }

  # Extract information on priors for trend components
  if(trend_model == 'None'){
    trend_df <- NULL
  }

  if(trend_model == 'GP'){
    if(use_lv){
      trend_df <- data.frame(param_name = c('rho_gp<lower=0>'),
                             param_length = length(unique(data_train$series)),
                             param_info = c('trend length scale'),
                             prior = c('rho_gp ~ inv_gamma(4, 24);'),
                             example_change =
                               paste0('rho_gp ~ exponential(',
                                      round(runif(min = 0.01, max = 1, n = 1), 2),
                                      ');'
                               ))
    } else {
      trend_df <- data.frame(param_name = c('alpha_gp<lower=0>',
                                            'rho_gp<lower=0>'),
                             param_length = length(unique(data_train$series)),
                             param_info = c('trend amplitude',
                                            'trend length scale'),
                             prior = c('alpha_gp ~ normal(0, 0.5);',
                                       'rho_gp ~ inv_gamma(4, 24);'),
                             example_change = c(paste0(
                               'alpha_gp ~ normal(',
                               round(runif(min = -1, max = 1, n = 1), 2),
                               ', ',
                               round(runif(min = 0.1, max = 1, n = 1), 2),
                               ');'
                             ),
                             paste0('rho_gp ~ exponential(',
                                    round(runif(min = 0.01, max = 1, n = 1), 2),
                                    ');'
                             )))
    }

  }

  if(trend_model == 'RW'){
    if(use_stan){
      trend_df <- data.frame(param_name = c('sigma<lower=0>'),
                             param_length = length(unique(data_train$series)),
                             param_info = c('trend sd'),
                             prior = c('sigma ~ exponential(1);'),
                             example_change = c(
                               paste0('sigma ~ exponential(',
                                      round(runif(min = 0.01, max = 1, n = 1), 2),
                                      ');'
                               )))
    } else {
      trend_df <- data.frame(param_name = c('sigma<lower=0>'),
                             param_length = length(unique(data_train$series)),
                             param_info = 'trend sd (for each series s)',
                             prior = c('sigma[s] ~ dexp(1)T(0.075, 5)'),
                             example_change = c(
                               paste0('sigma[s] ~ dexp(',
                                      round(runif(min = 0.01, max = 1, n = 1), 2),
                                      ')'
                               )))
    }

  }

  if(trend_model == 'AR1'){
    if(use_stan){
      trend_df <- data.frame(param_name = c('ar1',
                                            'sigma<lower=0>'),
                             param_length = ifelse(use_lv,
                                                   n_lv,
                                                   length(unique(data_train$series))),
                             param_info = c('trend AR1 coefficient',
                                            'trend sd'),
                             prior = c('ar1 ~ std_normal();',
                                       'sigma ~ exponential(2);'),
                             example_change = c(paste0(
                               'ar1 ~ normal(',
                               round(runif(min = -1, max = 1, n = 1), 2),
                               ', ',
                               round(runif(min = 0.1, max = 1, n = 1), 2),
                               ');'
                             ),
                             paste0('sigma ~ exponential(',
                                    round(runif(min = 0.01, max = 1, n = 1), 2),
                                    ');'
                             )))
    } else {
      trend_df <- data.frame(param_name = c('ar1',
                                            'sigma<lower=0>'),
                             param_length = ifelse(use_lv,
                                                   n_lv,
                                                   length(unique(data_train$series))),
                             param_info = c('trend AR1 coefficient (for each series s)',
                                            'trend sd (for each series s)'),
                             prior = c('ar1[s] ~ dnorm(0, 10)',
                                       'sigma[s] ~ dexp(2)T(0.075, 5)'),
                             example_change = c(paste0(
                               'ar1[s] ~ dnorm(',
                               round(runif(min = -1, max = 1, n = 1), 2),
                               ', ',
                               round(runif(min = 0.1, max = 1, n = 1), 2),
                               ')'
                             ),
                             paste0('sigma[s] ~ dexp(',
                                    round(runif(min = 0.01, max = 1, n = 1), 2),
                                    ')'
                             )))
    }

  }

  if(trend_model == 'AR2'){
    if(use_stan){
      trend_df <- data.frame(param_name = c('ar1', 'ar2',
                                            'sigma<lower=0>'),
                             param_length = ifelse(use_lv,
                                                   n_lv,
                                                   length(unique(data_train$series))),
                             param_info = c('trend AR1 coefficient',
                                            'trend AR2 coefficient',
                                            'trend sd'),
                             prior = c('ar1 ~ std_normal();',
                                       'ar2 ~ std_normal();',
                                       'sigma ~ exponential(2);'),
                             example_change = c(paste0(
                               'ar1 ~ normal(',
                               round(runif(min = -1, max = 1, n = 1), 2),
                               ', ',
                               round(runif(min = 0.1, max = 1, n = 1), 2),
                               ');'
                             ),
                             paste0(
                               'ar2 ~ normal(',
                               round(runif(min = -1, max = 1, n = 1), 2),
                               ', ',
                               round(runif(min = 0.1, max = 1, n = 1), 2),
                               ');'
                             ),
                             paste0('sigma ~ exponential(',
                                    round(runif(min = 0.01, max = 1, n = 1), 2),
                                    ');'
                             )))
    } else {
      trend_df <- data.frame(param_name = c('ar1', 'ar2',
                                            'sigma<lower=0>'),
                             param_length = ifelse(use_lv,
                                                   n_lv,
                                                   length(unique(data_train$series))),
                             param_info = c('trend AR1 coefficient (for each series s)',
                                            'trend AR2 coefficient (for each series s)',
                                            'trend sd (for each series s)'),
                             prior = c('ar1[s] ~ dnorm(0, 10)',
                                       'ar2[s] ~ dnorm(0, 10)',
                                       'sigma[s] ~ dexp(2)T(0.075, 5)'),
                             example_change = c(paste0(
                               'ar1[s] ~ dnorm(',
                               round(runif(min = -1, max = 1, n = 1), 2),
                               ', ',
                               round(runif(min = 0.1, max = 1, n = 1), 2),
                               ')'
                             ),
                             paste0(
                               'ar2[s] ~ dnorm(',
                               round(runif(min = -1, max = 1, n = 1), 2),
                               ', ',
                               round(runif(min = 0.1, max = 1, n = 1), 2),
                               ')'
                             ),
                             paste0('sigma[s] ~ dexp(',
                                    round(runif(min = 0.01, max = 1, n = 1), 2),
                                    ')'
                             )))
    }

  }

  if(trend_model == 'AR3'){
    if(use_stan){
      trend_df <- data.frame(param_name = c('ar1', 'ar2', 'ar3',
                                            'sigma<lower=0>'),
                             param_length = ifelse(use_lv,
                                                   n_lv,
                                                   length(unique(data_train$series))),
                             param_info = c('trend AR1 coefficient',
                                            'trend AR2 coefficient',
                                            'trend AR3 coefficient',
                                            'trend sd'),
                             prior = c('ar1 ~ std_normal();',
                                       'ar2 ~ std_normal();',
                                       'ar3 ~ std_normal();',
                                       'sigma ~ exponential(2);'),
                             example_change = c(paste0(
                               'ar1 ~ normal(',
                               round(runif(min = -1, max = 1, n = 1), 2),
                               ', ',
                               round(runif(min = 0.1, max = 1, n = 1), 2),
                               ');'
                             ),
                             paste0(
                               'ar2 ~ normal(',
                               round(runif(min = -1, max = 1, n = 1), 2),
                               ', ',
                               round(runif(min = 0.1, max = 1, n = 1), 2),
                               ');'
                             ),
                             paste0(
                               'ar3 ~ normal(',
                               round(runif(min = -1, max = 1, n = 1), 2),
                               ', ',
                               round(runif(min = 0.1, max = 1, n = 1), 2),
                               ');'
                             ),
                             paste0('sigma ~ exponential(',
                                    round(runif(min = 0.01, max = 1, n = 1), 2),
                                    ');'
                             )))
    } else {
      trend_df <- data.frame(param_name = c('ar1', 'ar2', 'ar3',
                                            'sigma<lower=0>'),
                             param_length = ifelse(use_lv,
                                                   n_lv,
                                                   length(unique(data_train$series))),
                             param_info = c('trend AR1 coefficient (for each series s)',
                                            'trend AR2 coefficient (for each series s)',
                                            'trend AR3 coefficient (for each series s)',
                                            'trend sd (for each series s)'),
                             prior = c('ar1[s] ~ dnorm(0, 10)',
                                       'ar2[s] ~ dnorm(0, 10)',
                                       'ar3[s] ~ dnorm(0, 10)',
                                       'sigma[s] ~ dexp(2)T(0.075, 5)'),
                             example_change = c(paste0(
                               'ar1[s] ~ dnorm(',
                               round(runif(min = -1, max = 1, n = 1), 2),
                               ', ',
                               round(runif(min = 0.1, max = 1, n = 1), 2),
                               ')'
                             ),
                             paste0(
                               'ar2[s] ~ dnorm(',
                               round(runif(min = -1, max = 1, n = 1), 2),
                               ', ',
                               round(runif(min = 0.1, max = 1, n = 1), 2),
                               ')'
                             ),
                             paste0(
                               'ar3[s] ~ dnorm(',
                               round(runif(min = -1, max = 1, n = 1), 2),
                               ', ',
                               round(runif(min = 0.1, max = 1, n = 1), 2),
                               ')'
                             ),
                             paste0('sigma[s] ~ dexp(',
                                    round(runif(min = 0.01, max = 1, n = 1), 2),
                                    ')'
                             )))
    }

  }

  # Remove options for trend variance priors if using a dynamic factor model
  if(use_lv){
    trend_df %>%
      dplyr::filter(!grepl('sigma', param_name)) -> trend_df
  }

  # Extract drift parameter information
  if(drift){
    if(use_stan){
      drift_df <- data.frame(param_name = c('phi'),
                             param_length = ifelse(use_lv,
                                                   n_lv,
                                                   length(unique(data_train$series))),
                             param_info = c('trend drift'),
                             prior = c('phi ~ std_normal();'),
                             example_change = c(
                               paste0(
                                 'phi ~ normal(',
                                 round(runif(min = -1, max = 1, n = 1), 2),
                                 ', ',
                                 round(runif(min = 0.1, max = 1, n = 1), 2),
                                 ');'
                               )))
    } else {
      drift_df <- data.frame(param_name = c('phi'),
                             param_length = ifelse(use_lv,
                                                   n_lv,
                                                   length(unique(data_train$series))),
                             param_info = c('trend drift (for each series s)'),
                             prior = c('phi ~ dnorm(0, 10)'),
                             example_change = c(
                               paste0(
                                 'phi ~ dnorm(',
                                 round(runif(min = -1, max = 1, n = 1), 2),
                                 ', ',
                                 round(runif(min = 0.1, max = 1, n = 1), 2),
                                 ')'
                               )))
    }

  } else {
    drift_df <- NULL
  }

  # Extract information for family-specific overdispsersion parameters
  if(family == 'nb'){
    if(use_stan){
      nb_df <- data.frame(param_name = c('r_inv<lower=0>'),
                          param_length = length(unique(data_train$series)),
                          param_info = c('inverse of NB dispsersion'),
                          prior = c('r_inv ~ normal(0, 10);'),
                          example_change = c(
                            paste0(
                              'r_inv ~ normal(',
                              round(runif(min = -1, max = 1, n = 1), 2),
                              ', ',
                              round(runif(min = 0.1, max = 1, n = 1), 2),
                              ');'
                            )))
    } else {
      nb_df <- data.frame(param_name = c('r_raw<lower=0>'),
                          param_length = length(unique(data_train$series)),
                          param_info = c('inverse of NB dispsersion (for each series s)'),
                          prior = c('r_raw[s] ~ dnorm(0, 0.1)T(0, )'),
                          example_change = c(
                            paste0(
                              'r_raw[s] ~ dnorm(',
                              round(runif(min = -1, max = 1, n = 1), 2),
                              ', ',
                              round(runif(min = 0.1, max = 1, n = 1), 2),
                              ')T(0, )'
                            )))
    }

  } else {
    nb_df <- NULL
  }

  if(family == 'tw'){
    tw_df <- data.frame(param_name = c('twdis_raw'),
                        param_length = length(unique(data_train$series)),
                        param_info = c('log of Tweedie dispsersion (for each series s)'),
                        prior = c('twdis_raw[s] ~ dnorm(0, 2)T(-3.5, 3.5)'),
                        example_change = c(
                          paste0(
                            'twdis_raw[s] ~ dnorm(',
                            round(runif(min = -1, max = 1, n = 1), 2),
                            ', ',
                            round(runif(min = 0.5, max = 5, n = 1), 2),
                            ')'
                          )))
  } else {
    tw_df <- NULL
  }

  # Return the dataframe of prior information
  return(rbind(sp_df,
               re_df,
               trend_df,
               drift_df,
               nb_df,
               tw_df))
}

