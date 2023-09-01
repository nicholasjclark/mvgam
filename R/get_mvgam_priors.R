#'Extract information on default prior distributions for an mvgam model
#'
#'This function lists the parameters that can have their prior distributions
#'changed for a given `mvgam` model, as well listing their default distributions
#'
#'
#'@param formula A \code{character} string specifying the GAM formula. These are exactly like the formula
#'for a GLM except that smooth terms, s, te, ti and t2, can be added to the right hand side
#'to specify that the linear predictor depends on smooth functions of predictors (or linear functionals of these)
#'@param trend_formula An optional \code{character} string specifying the GAM process model formula. If
#'supplied, a linear predictor will be modelled for the latent trends to capture process model evolution
#'separately from the observation model. Should not have a response variable specified on the left-hand side
#'of the formula (i.e. a valid option would be `~ season + s(year)`). This feature is experimental, and is only
#'currently available for Random Walk trend models.
#'@param data A \code{dataframe} or \code{list} containing the model response variable and covariates
#'required by the GAM \code{formula}. Should include columns:
#''y' (the discrete outcomes; \code{NA}s allowed)
#''series' (character or factor index of the series IDs)
#''time' (numeric index of the time point for each observation).
#'Any other variables to be included in the linear predictor of \code{formula} must also be present
#'@param data_train Deprecated. Still works in place of \code{data} but users are recommended to use
#'\code{data} instead for more seamless integration into `R` workflows
#'@param family \code{family} specifying the exponential observation family for the series. Currently supported
#'families are:
#'\itemize{
#'   \item`nb()` for count data
#'   \item`poisson()` for count data
#'   \item`tweedie()` for count data (power parameter `p` fixed at `1.5`)
#'   \item`gaussian()` for real-valued data
#'   \item`betar()` for proportional data on `(0,1)`
#'   \item`lognormal()` for non-negative real-valued data
#'   \item`student_t()` for real-valued data
#'   \item`Gamma()` for non-negative real-valued data}
#'See \code{\link{mvgam_families}} for more details
#'@param use_lv \code{logical}. If \code{TRUE}, use dynamic factors to estimate series'
#'latent trends in a reduced dimension format. If \code{FALSE}, estimate independent latent trends for each series
#'@param n_lv \code{integer} the number of latent dynamic factors to use if \code{use_lv == TRUE}.
#'Cannot be \code{>n_series}. Defaults arbitrarily to \code{min(2, floor(n_series / 2))}
#'@param trend_model \code{character} specifying the time series dynamics for the latent trend. Options are:
#'\itemize{
#'   \item `None` (no latent trend component; i.e. the GAM component is all that contributes to the linear predictor,
#'and the observation process is the only source of error; similarly to what is estimated by \code{\link[mgcv]{gam}})
#'   \item `RW` (random walk with possible drift)
#'   \item `AR1` (with possible drift)
#'   \item `AR2` (with possible drift)
#'   \item `AR3` (with possible drift)
#'   \item `VAR1` (contemporaneously uncorrelated VAR1; only available in \code{Stan})
#'   \item `VAR1cor` (contemporaneously correlated VAR1; only available in \code{Stan})
#'   \item `GP` (Gaussian Process with squared exponential kernel;
#'only available in \code{Stan})} See [mvgam_trends] for more details
#'@param trend_map Optional `data.frame` specifying which series should depend on which latent
#'trends. Useful for allowing multiple series to depend on the same latent trend process, but with
#'different observation processes. If supplied, a latent factor model is set up by setting
#'`use_lv = TRUE` and using the mapping to set up the shared trends. Needs to have column names
#'`series` and `trend`, with integer values in the `trend` column to state which trend each series
#'should depend on. The `series` column should have a single unique entry for each series in the
#'data (names should perfectly match factor levels of the `series` variable in `data`). See examples
#'in \code{\link{mvgam}} for details
#'@param drift \code{logical} estimate a drift parameter in the latent trend components. Useful if the latent
#'trend is expected to broadly follow a non-zero slope. Note that if the latent trend is more or less stationary,
#'the drift parameter can become unidentifiable, especially if an intercept term is included in the GAM linear
#'predictor (which it is by default when calling \code{\link[mgcv]{jagam}}). Therefore this defaults to \code{FALSE}
#'@param use_stan Logical. If \code{TRUE} and if \code{rstan} is installed, the model will be compiled and sampled using
#'the Hamiltonian Monte Carlo with a call to \code{\link[cmdstanr]{cmdstan_model}} or, if `cmdstanr` is not available,
#'a call to \code{\link[rstan]{stan}}. Note that this functionality is still in development and
#'not all options that are available in \code{JAGS} can be used, including: no option for a Tweedie family and no option for
#'dynamic factor trends. However, as \code{Stan} can estimate Hilbert base approximate Gaussian Processes, which
#'are much more computationally tractable than full GPs for time series with `>100` observations, estimation
#'in \code{Stan} can support latent GP trends while estimation in \code{JAGS} cannot
#'@details Users can supply a model formula, prior to fitting the model, so that default priors can be inspected and
#'altered. To make alterations, change the contents of the `prior` column and supplying this
#'\code{data.frame} to the `mvgam` function using the argument `priors`. If using `Stan` as the backend,
#'users can also modify the parameter bounds by modifying the `new_lowerbound` and/or `new_upperbound` columns.
#'This will be necessary if using restrictive distributions on some parameters, such as a Beta distribution
#'for the trend sd parameters for example (Beta only has support on  \code{(0,1)}), so the upperbound cannot
#'be above `1`. Another option is to make use of the prior modification functions in `brms`
#'(i.e. \code{\link[brms]{prior}}) to change prior distributions and bounds (just use the name of the parameter that
#'you'd like to change as the `class` argument; see examples below)
#' @note Only the `prior`, `new_lowerbound` and/or `new_upperbound` columns of the output
#' should be altered when defining the user-defined priors for the `mvgam` model. Use only if you are
#' familiar with the underlying probabilistic programming language. There are no sanity checks done to
#' ensure that the code is legal (i.e. to check that lower bounds are smaller than upper bounds, for
#' example)
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
#'               run_model = FALSE)
#'
#'# Inspect the model file with default mvgam priors
#'code(mod_default)
#'
#'# Look at which priors can be updated in mvgam
#'test_priors <- get_mvgam_priors(y ~ s(series, bs = 're') +
#'                               s(season, bs = 'cc') - 1,
#'                               family = 'nb',
#'                               data = dat$data_train,
#'                               trend_model = 'AR2')
#'test_priors
#'
#'# Make a few changes; first, change the population mean for the series-level
#'# random intercepts
#'test_priors$prior[2] <- 'mu_raw ~ normal(0.2, 0.5);'
#'
#'# Now use stronger regularisation for the series-level AR2 coefficients
#'test_priors$prior[5] <- 'ar2 ~ normal(0, 0.25);'
#'
#'# Check that the changes are made to the model file without any warnings by
#'# setting 'run_model = FALSE'
#'mod <- mvgam(y ~ s(series, bs = 're') +
#'             s(season, bs = 'cc') - 1,
#'             family = 'nb',
#'             data = dat$data_train,
#'             trend_model = 'AR2',
#'             priors = test_priors,
#'             run_model = FALSE)
#'             code(mod)
#'
#'# No warnings, the model is ready for fitting now in the usual way with the addition
#'# of the 'priors' argument
#'
#'# The same can be done using brms functions; here we will also change the ar1 prior
#'# and put some bounds on the ar coefficients to enforce stationarity
#'brmsprior <- c(prior(normal(0.2, 0.5), class = mu_raw),
#'               prior(normal(0, 0.25), class = ar1, lb = -1, ub = 1),
#'               prior(normal(0, 0.25), class = ar2, lb = -1, ub = 1))
#'               brmsprior
#'
#'mod <- mvgam(y ~ s(series, bs = 're') +
#'             s(season, bs = 'cc') - 1,
#'           family = 'nb',
#'           data = dat$data_train,
#'           trend_model = 'AR2',
#'           priors = brmsprior,
#'           run_model = FALSE)
#'code(mod)
#'
#'# Look at what is returned when an incorrect spelling is used
#'test_priors$prior[5] <- 'ar2_bananas ~ normal(0, 0.25);'
#'mod <- mvgam(y ~ s(series, bs = 're') +
#'             s(season, bs = 'cc') - 1,
#'             family = 'nb',
#'             data = dat$data_train,
#'             trend_model = 'AR2',
#'             priors = test_priors,
#'             run_model = FALSE)
#'code(mod)
#'
#'@export
get_mvgam_priors = function(formula,
                            trend_formula,
                            data,
                            data_train,
                            family = 'poisson',
                            use_lv = FALSE,
                            n_lv,
                            use_stan = TRUE,
                            trend_model = 'None',
                            trend_map,
                            drift = FALSE){

  # Validate the data
  if(missing("data") & missing("data_train")){
    stop('Argument "data" is missing with no default')
  }

  if(!missing("data")){
    data_train <- data
  }

  # Ensure series and time variables are present
  data_train <- validate_series_time(data_train, name = 'data')

  # Validate observation formula
  orig_formula <- formula
  formula <- interpret_mvgam(formula, N = max(data_train$time))
  data_train <- validate_obs_formula(formula, data = data_train, refit = FALSE)

  # Validate the family argument
  family <- validate_family(family)
  family_char <- match.arg(arg = family$family,
                           choices = family_char_choices())

  # Validate the trend argument
  trend_model <- validate_trend_model(trend_model, drift = drift)

  # Check trend formula
  if(!missing(trend_formula)){
    if(missing(trend_map)){
      trend_map <- data.frame(series = unique(data_train$series),
                              trend = 1:length(unique(data_train$series)))
    }

    if(!trend_model %in% c('RW', 'AR1', 'AR2', 'VAR1', 'VAR1cor')){
      stop('only RW, AR1, AR2 and VAR trends currently supported for trend predictor models',
           call. = FALSE)
    }
  }

  # Check trend_map is correctly specified
  if(!missing(trend_map)){
    validate_trendmap(trend_map = trend_map, data_train = data_train,
                      trend_model = trend_model, use_stan = use_stan)

    # If trend_map correctly specified, set use_lv to TRUE for
    # most models (but not yet for VAR models, which require additional
    # modifications)
    if(trend_model == 'VAR1'){
      use_lv <- FALSE
    } else {
      use_lv <- TRUE
    }
    n_lv <- max(trend_map$trend)
  }

  # If trend_formula supplied, first run get_mvgam_priors for the observation model
  # and then modify the resulting output
  if(!missing(trend_formula)){
    validate_trend_formula(trend_formula)
    prior_df <- get_mvgam_priors(formula = formula,
                                 data = data,
                                 data_train = data_train,
                                 family = family,
                                 use_lv = FALSE,
                                 use_stan = TRUE,
                                 trend_model = trend_model,
                                 trend_map = trend_map,
                                 drift = drift)

    # Replace any terms labelled 'trend' with 'series' for creating the necessary
    # structures
    trend_formula <- formula(paste(gsub('trend', 'series',
                                        as.character(trend_formula),
                                        fixed = TRUE),
                                   collapse = " "))

    # Drop any intercept from the formula
    if(attr(terms(trend_formula), 'intercept') == 1){
      trend_formula <- update(trend_formula, trend_y  ~ . -1)
    } else {
      trend_formula <- update(trend_formula, trend_y  ~ .)
    }

    trend_train <- data_train
    trend_train$trend_y <- rnorm(length(trend_train$time))

    # Add indicators of trend names as factor levels using the trend_map
    trend_indicators <- vector(length = length(trend_train$time))
    for(i in 1:length(trend_train$time)){
      trend_indicators[i] <- trend_map$trend[which(trend_map$series ==
                                                     trend_train$series[i])]
    }
    trend_indicators <- as.factor(paste0('trend', trend_indicators))
    trend_train$series <- trend_indicators
    trend_train$y <- NULL

    # Only keep one time observation per trend
    data.frame(series = trend_train$series,
               time = trend_train$time,
               row_num = 1:length(trend_train$time)) %>%
      dplyr::group_by(series, time) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::pull(row_num) -> inds_keep

    if(inherits(trend_train, 'list')){
      trend_train <- lapply(trend_train, function(x){
        if(is.matrix(x)){
          matrix(x[inds_keep,], ncol = NCOL(x))
        } else {
          x[inds_keep]
        }

      })
    } else {
      trend_train <- trend_train[inds_keep, ]
    }

    # Now get the priors related to the trend model
    trend_prior_df <- get_mvgam_priors(trend_formula,
                                       data = trend_train,
                                       family = gaussian(),
                                       trend_model = 'None')

    # Modify some of the term names and return
    trend_prior_df[] <- lapply(trend_prior_df, function(x)
      gsub("lambda", "lambda_trend", x))
    trend_prior_df[] <- lapply(trend_prior_df, function(x)
      gsub("n_sp", "n_sp_trend", x))
    trend_prior_df[] <- lapply(trend_prior_df, function(x)
      gsub("mu_raw", "mu_raw_trend", x))
    trend_prior_df[] <- lapply(trend_prior_df, function(x)
      gsub("sigma_raw", "sigma_raw_trend", x))
    trend_prior_df[] <- lapply(trend_prior_df, function(x)
      gsub("n_series", "n_lv", x))
    trend_prior_df[] <- lapply(trend_prior_df, function(x)
      gsub("series", "trend", x))
    trend_prior_df <- trend_prior_df[!trend_prior_df$param_info ==
                                       'observation error sd',]
    out <- rbind(prior_df, trend_prior_df)
    out[] <- lapply(out, function(x)
      gsub("trend sd", "process error sd", x))
    out[] <- lapply(out, function(x)
      gsub("trend AR1", "process model AR1", x))
    out[] <- lapply(out, function(x)
      gsub("trend AR2", "process model AR2", x))
    out[] <- lapply(out, function(x)
      gsub("trend AR3", "process model AR3", x))
    out[] <- lapply(out, function(x)
      gsub("trend drift", "process model drift", x))
    out[] <- lapply(out, function(x)
      gsub("vector<lower=0>[n_series] sigma;", "vector<lower=0>[n_lv] sigma;", x,
           fixed = TRUE))

  } else {

    # JAGS cannot support latent GP or VAR trends
    if(!use_stan & trend_model %in%c ('GP', 'VAR1', 'VAR1cor')){
      warning('gaussian process and VAR trends not yet supported for JAGS; reverting to Stan')
      use_stan <- TRUE
    }

    if(use_stan & family_char == 'tweedie'){
      warning('Tweedie family not yet supported for stan; reverting to JAGS')
      use_stan <- FALSE
    }

    # Number of latent variables cannot be greater than number of series
    if(use_lv){
      if(missing(n_lv)){
        n_lv <- min(2, floor(length(unique(data_train$series)) / 2))
      }
      if(n_lv > length(unique(data_train$series))){
        stop('number of latent variables cannot be greater than number of series',
             call. = FALSE)
      }
    }

    # No point in latent variables if trend model is None
    if(trend_model == 'None' & use_lv){
      use_lv <- FALSE
      warning('No point in latent variables if trend model is None; changing use_lv to FALSE')
    }

    # Fill in missing observations in data_train so the size of the dataset is correct when
    # building the initial JAGS model.
    replace_nas = function(var){
      if(all(is.na(var))){
        # Sampling from uniform[0.1,0.99] will allow all the gam models
        # to work, even though the Poisson / Negative Binomial will issue
        # warnings. This is ok as we just need to produce the linear predictor matrix
        # and store the coefficient names
        var <- runif(length(var), 0.1, 0.99)
      } else {
        # If there are some non-missing observations,
        # sample from the observed values to ensure
        # distributional assumptions are met without warnings
        var[which(is.na(var))] <-
          sample(var[which(!is.na(var))],
                 length(which(is.na(var))),
                 replace = TRUE)
      }
      var
    }

    data_train[[terms(formula(formula))[[2]]]] <-
      replace_nas(data_train[[terms(formula(formula))[[2]]]])

    # Use a small fit from mgcv to extract relevant information on smooths included
    # in the model
    ss_gam <- try(mvgam_setup(formula = formula,
                              family = family_to_mgcvfam(family),
                              data = data_train,
                              drop.unused.levels = FALSE,
                              maxit = 30),
                  silent = TRUE)
    if(inherits(ss_gam, 'try-error')){
      if(grepl('missing values', ss_gam[1])){
        stop(paste('Missing values found in data predictors:\n',
                   attr(ss_gam, 'condition')),
             call. = FALSE)
      }
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
        sp_df <- data.frame(param_name = 'vector<lower=0>[n_sp] lambda;',
                            param_length = sum(smooth_labs$nsp),
                            param_info = c(paste(nonre_smooths,
                                                 'smooth parameters',
                                                 collapse = ', ')),
                            prior = 'lambda ~ normal(10, 25);',
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
        re_df <- data.frame(param_name = c(paste0('vector[',n_re_terms,'] mu_raw;'),
                                           paste0('vector<lower=0>[',n_re_terms,'] sigma_raw;')),
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
        trend_df <- data.frame(param_name = c('vector<lower=0>[n_lv] rho_gp;'),
                               param_length = n_lv,
                               param_info = c('trend length scale'),
                               prior = c('rho_gp ~ inv_gamma(1.499007, 5.670433);'),
                               example_change =
                                 paste0('rho_gp ~ exponential(',
                                        round(runif(min = 0.01, max = 1, n = 1), 2),
                                        ');'
                                 ))
      } else {
        trend_df <- data.frame(param_name = c('vector<lower=0>[n_series] alpha_gp;',
                                              'vector<lower=0>[n_series] rho_gp;'),
                               param_length = length(unique(data_train$series)),
                               param_info = c('trend amplitude',
                                              'trend length scale'),
                               prior = c('alpha_gp ~ normal(0, 0.5);',
                                         'rho_gp ~ inv_gamma(1.499007, 5.670433);'),
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

      trend_df <- rbind(trend_df,
                        data.frame(param_name = c('int<lower=1> num_gp_basis;'),
                                   param_length = 1,
                                   param_info = c('basis dimension for approximate GP'),
                                   prior = c('num_gp_basis = min(20, n);'),
                                   example_change = 'num_gp_basis = 12;'))
    }

    if(trend_model == 'RW'){
      if(use_stan){
        trend_df <- data.frame(param_name = c(paste0('vector<lower=0>[',
                                                     ifelse(use_lv, 'n_lv', 'n_series'),
                                                     '] sigma;')),
                               param_length = ifelse(use_lv,
                                                     n_lv,
                                                     length(unique(data_train$series))),
                               param_info = c('trend sd'),
                               prior = c('sigma ~ exponential(2);'),
                               example_change = c(
                                 paste0('sigma ~ exponential(',
                                        round(runif(min = 0.01, max = 1, n = 1), 2),
                                        ');'
                                 )))

      } else {
        trend_df <- data.frame(param_name = c('vector<lower=0>[n_series] sigma'),
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

    if(trend_model == 'VAR1'){
      trend_df <- data.frame(param_name = c('vector<lower=0>[n_series] sigma;'),
                             param_length = c(length(unique(data_train$series))),
                             param_info = c('trend sd'),
                             prior = c('sigma ~ inv_gamma(2.3693353, 0.7311319);'),
                             example_change = c(
                               paste0('sigma ~ exponential(',
                                      round(runif(min = 0.01, max = 1, n = 1), 2),
                                      ');'
                               )))
      trend_df <- rbind(trend_df,
                        data.frame(param_name = c("real es[1];",
                                                  "real es[2];",
                                                  "real<lower=0> fs[1];",
                                                  "real<lower=0> fs[2];",
                                                  "real<lower=0> gs[1];",
                                                  "real<lower=0> gs[2];",
                                                  "real<lower=0> hs[1];",
                                                  "real<lower=0> hs[2];"),
                                   param_length = 1,
                                   param_info = c('diagonal autocorrelation population mean',
                                                  'off-diagonal autocorrelation population mean',
                                                  'diagonal autocorrelation population variance',
                                                  'off-diagonal autocorrelation population variance',
                                                  'shape1 for diagonal autocorrelation precision',
                                                  'shape1 for off-diagonal autocorrelation precision',
                                                  'shape2 for diagonal autocorrelation precision',
                                                  'shape2 for off-diagonal autocorrelation precision'),
                                   prior = c("es[1] = 0;",
                                             "es[2] = 0;",
                                             "fs[1] = sqrt(0.455);",
                                             "fs[2] = sqrt(0.455);",
                                             "gs[1] = 1.365;",
                                             "gs[2] = 1.365;",
                                             "hs[1] = 0.071175;" ,
                                             "hs[2] = 0.071175;"),
                                   example_change = c("es[1] = 0.5;",
                                                      "es[2] = 0.1;",
                                                      "fs[1] = 0.6;",
                                                      "fs[2] = 0.3;",
                                                      "gs[1] = 1.1;",
                                                      "gs[2] = 1.07;",
                                                      "hs[1] = 0.08;",
                                                      "hs[2] = 0.1;")))
    }

    if(trend_model == 'VAR1cor'){
      trend_df <- data.frame(param_name = c('vector<lower=0>[n_series] sigma;'),
                             param_length = c(length(unique(data_train$series))),
                             param_info = c('trend sd'),
                             prior = c('sigma ~ inv_gamma(2.3693353, 0.7311319);'),
                             example_change = c(
                               paste0('sigma ~ exponential(',
                                      round(runif(min = 0.01, max = 1, n = 1), 2),
                                      ');'
                               )))
      trend_df <- rbind(trend_df,data.frame(param_name = c("real es[1];",
                                            "real es[2];",
                                            "real<lower=0> fs[1];",
                                            "real<lower=0> fs[2];",
                                            "real<lower=0> gs[1];",
                                            "real<lower=0> gs[2];",
                                            "real<lower=0> hs[1];",
                                            "real<lower=0> hs[2];",
                                            "real<lower=0> L_Omega;"),
                                   param_length = 1,
                                   param_info = c('diagonal autocorrelation population mean',
                                                  'off-diagonal autocorrelation population mean',
                                                  'diagonal autocorrelation population variance',
                                                  'off-diagonal autocorrelation population variance',
                                                  'shape1 for diagonal autocorrelation precision',
                                                  'shape1 for off-diagonal autocorrelation precision',
                                                  'shape2 for diagonal autocorrelation precision',
                                                  'shape2 for off-diagonal autocorrelation precision',
                                                  'LKJ prior on trend error correlations'),
                                   prior = c("es[1] = 0;",
                                             "es[2] = 0;",
                                             "fs[1] = sqrt(0.455);",
                                             "fs[2] = sqrt(0.455);",
                                             "gs[1] = 1.365;",
                                             "gs[2] = 1.365;",
                                             "hs[1] = 0.071175;" ,
                                             "hs[2] = 0.071175;",
                                             "L_Omega ~ lkj_corr_cholesky(2);"),
                                   example_change = c("es[1] = 0.5;",
                                                      "es[2] = 0.1;",
                                                      "fs[1] = 0.6;",
                                                      "fs[2] = 0.3;",
                                                      "gs[1] = 1.1;",
                                                      "gs[2] = 1.07;",
                                                      "hs[1] = 0.08;" ,
                                                      "hs[2] = 0.1;",
                                                      "L_Omega ~ lkj_corr_cholesky(4);")))
    }

    if(trend_model == 'AR1'){
      if(use_stan){
        trend_df <- data.frame(param_name = c(paste0('vector<lower=-1.5,upper=1.5>[',
                                                     ifelse(use_lv, 'n_lv', 'n_series'),
                                                     '] ar1;'),
                                              paste0('vector<lower=0>[',
                                                     ifelse(use_lv, 'n_lv', 'n_series'),
                                                     '] sigma;')),
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
        trend_df <- data.frame(param_name = c(paste0('vector<lower=-1.5,upper=1.5>[',
                                                     ifelse(use_lv, 'n_lv', 'n_series'),
                                                     '] ar1;'),
                                              paste0('vector<lower=0>[',
                                                     ifelse(use_lv, 'n_lv', 'n_series'),
                                                     '] sigma;')),
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
        trend_df <- data.frame(param_name = c(paste0('vector<lower=-1.5,upper=1.5>[',
                                                     ifelse(use_lv, 'n_lv', 'n_series'),
                                                     '] ar1;'),
                                              paste0('vector<lower=-1.5,upper=1.5>[',
                                                     ifelse(use_lv, 'n_lv', 'n_series'),
                                                     '] ar2;'),
                                              paste0('vector<lower=0>[',
                                                     ifelse(use_lv, 'n_lv', 'n_series'),
                                                     '] sigma;')),
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
        trend_df <- data.frame(param_name = c(paste0('vector<lower=-1.5,upper=1.5>[',
                                                     ifelse(use_lv, 'n_lv', 'n_series'),
                                                     '] ar1;'),
                                              paste0('vector<lower=-1.5,upper=1.5>[',
                                                     ifelse(use_lv, 'n_lv', 'n_series'),
                                                     '] ar2;'),
                                              paste0('vector<lower=0>[',
                                                     ifelse(use_lv, 'n_lv', 'n_series'),
                                                     '] sigma;')),
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
        trend_df <- data.frame(param_name = c(paste0('vector<lower=-1.5,upper=1.5>[',
                                                     ifelse(use_lv, 'n_lv', 'n_series'),
                                                     '] ar1;'),
                                              paste0('vector<lower=-1.5,upper=1.5>[',
                                                     ifelse(use_lv, 'n_lv', 'n_series'),
                                                     '] ar2;'),
                                              paste0('vector<lower=-1.5,upper=1.5>[',
                                                     ifelse(use_lv, 'n_lv', 'n_series'),
                                                     '] ar3;'),
                                              paste0('vector<lower=0>[',
                                                     ifelse(use_lv, 'n_lv', 'n_series'),
                                                     '] sigma;')),
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
        trend_df <- data.frame(param_name = c(paste0('vector<lower=-1.5,upper=1.5>[',
                                                     ifelse(use_lv, 'n_lv', 'n_series'),
                                                     '] ar1;'),
                                              paste0('vector<lower=-1.5,upper=1.5>[',
                                                     ifelse(use_lv, 'n_lv', 'n_series'),
                                                     '] ar2;'),
                                              paste0('vector<lower=-1.5,upper=1.5>[',
                                                     ifelse(use_lv, 'n_lv', 'n_series'),
                                                     '] ar3;'),
                                              paste0('vector<lower=0>[',
                                                     ifelse(use_lv, 'n_lv', 'n_series'),
                                                     '] sigma;')),
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
      if(missing(trend_map)){
        trend_df %>%
          dplyr::filter(!grepl(paste0('vector<lower=0>[',
                                      ifelse(use_lv, 'n_lv', 'n_series'),
                                      '] sigma;'), param_name,
                               fixed = TRUE)) -> trend_df
      }

      if(use_stan){
        if(missing(trend_map)){
          trend_df <- rbind(trend_df,
                            data.frame(param_name = c('vector[M] L;'),
                                       param_length = n_lv * length(unique(data_train$series)),
                                       param_info = c('factor loadings'),
                                       prior = c('L ~ student_t(5, 0, 1);'),
                                       example_change = 'L ~ std_normal();'))
        }
      }
    }

    # Extract drift parameter information
    if(drift){
      if(use_stan){
        drift_df <- data.frame(param_name = paste0('vector[',
                                                   ifelse(use_lv, 'n_lv', 'n_series'),
                                                   '] drift;'),
                               param_length = ifelse(use_lv,
                                                     n_lv,
                                                     length(unique(data_train$series))),
                               param_info = c('trend drift'),
                               prior = c('drift ~ std_normal();'),
                               example_change = c(
                                 paste0(
                                   'drift ~ normal(',
                                   round(runif(min = -1, max = 1, n = 1), 2),
                                   ', ',
                                   round(runif(min = 0.1, max = 1, n = 1), 2),
                                   ');'
                                 )))
      } else {
        drift_df <- data.frame(param_name = paste0('vector[',
                                                   ifelse(use_lv, 'n_lv', 'n_series'),
                                                   '] drift;'),
                               param_length = ifelse(use_lv,
                                                     n_lv,
                                                     length(unique(data_train$series))),
                               param_info = c('trend drift (for each series s)'),
                               prior = c('drift ~ dnorm(0, 10)'),
                               example_change = c(
                                 paste0(
                                   'drift ~ dnorm(',
                                   round(runif(min = -1, max = 1, n = 1), 2),
                                   ', ',
                                   round(runif(min = 0.1, max = 1, n = 1), 2),
                                   ')'
                                 )))
      }

    } else {
      drift_df <- NULL
    }

    # Extract information for family-specific parameters
    family_df <- family_prior_info(family = family_char,
                                   use_stan = use_stan,
                                   data = data_train)

    # Return the dataframe of prior information
    prior_df <- rbind(sp_df,
                      re_df,
                      trend_df,
                      drift_df,
                      family_df)

    prior_df$new_lowerbound <- NA
    prior_df$new_upperbound <- NA

    out <- prior_df
  }

  return(out)
}
