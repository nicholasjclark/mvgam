#'Fit a Bayesian dynamic GAM to a univariate or multivariate set of discrete time series
#'
#'This function estimates the posterior distribution for Generalised Additive Models (GAMs) that can include
#'smooth spline functions, specified in the GAM formula, as well as latent temporal processes, specified by trend_model.
#'There are currently two options for specifying the structures of the trends (either as latent
#'dynamic factors to capture trend dependencies among series in a reduced dimension format, or as independent trends)
#'
#'@importFrom parallel clusterExport stopCluster setDefaultCluster
#'@importFrom stats formula terms rnorm update.formula predict
#'@param formula A \code{character} string specifying the GAM observation model formula. These are exactly like the formula
#'for a GLM except that smooth terms, s, te, ti and t2, can be added to the right hand side
#'to specify that the linear predictor depends on smooth functions of predictors (or linear functionals of these).
#'@param trend_formula An optional \code{character} string specifying the GAM process model formula. If
#'supplied, a linear predictor will be modelled for the latent trends to capture process model evolution
#'separately from the observation model. Should not have a response variable specified on the left-hand side
#'of the formula (i.e. a valid option would be `~ season + s(year)`)
#'@param knots An optional \code{list} containing user specified knot values to be used for basis construction.
#'For most bases the user simply supplies the knots to be used, which must match up with the k value supplied
#'(note that the number of knots is not always just k). Different terms can use different numbers of knots,
#'unless they share a covariate.
#'@param data A \code{dataframe} or \code{list} containing the model response variable and covariates
#'required by the GAM \code{formula}. Should include columns:
#'`series` (character or factor index of the series IDs)
#'`time` (numeric index of the time point for each observation).
#'Any other variables to be included in the linear predictor of \code{formula} must also be present
#'@param data_train Deprecated. Still works in place of \code{data} but users are recommended to use
#'\code{data} instead for more seamless integration into `R` workflows
#'@param newdata Optional \code{dataframe} or \code{list} of test data containing at least 'series' and 'time'
#'in addition to any other variables included in the linear predictor of \code{formula}. If included, the
#'observations in variable \code{y} will be set to \code{NA} when fitting the model so that posterior
#'simulations can be obtained
#'@param data_test Deprecated. Still works in place of \code{newdata} but users are recommended to use
#'\code{newdata} instead for more seamless integration into `R` workflows
#'@param run_model \code{logical}. If \code{FALSE}, the model is not fitted but instead the function will
#'return the model file and the data / initial values that are needed to fit the model outside of \code{mvgam}
#'@param prior_simulation \code{logical}. If \code{TRUE}, no observations are fed to the model, and instead
#'simulations from prior distributions are returned
#'@param return_model_data \code{logical}. If \code{TRUE}, the list of data that is needed to fit the
#'model is returned, along with the initial values for smooth and AR parameters, once the model is fitted.
#'This will be helpful if users wish to modify the model file to add
#'other stochastic elements that are not currently avaiable in \code{mvgam}. Default is \code{FALSE} to reduce
#'the size of the returned object, unless \code{run_model == FALSE}
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
#'   \item `VAR1` (with possible drift; only available in \code{Stan})
#'   \item `GP` (Gaussian Process with squared exponential kernel;
#'only available in \code{Stan})}
#'@param trend_map Optional `data.frame` specifying which series should depend on which latent
#'trends. Useful for allowing multiple series to depend on the same latent trend process, but with
#'different observation processes. If supplied, a latent factor model is set up by setting
#'`use_lv = TRUE` and using the mapping to set up the shared trends. Needs to have column names
#'`series` and `trend`, with integer values in the `trend` column to state which trend each series
#'should depend on. The `series` column should have a single unique entry for each series in the
#'data (names should perfectly match factor levels of the `series` variable in `data`). See examples
#'for details
#'@param drift \code{logical} estimate a drift parameter in the latent trend components. Useful if the latent
#'trend is expected to broadly follow a non-zero slope. Note that if the latent trend is more or less stationary,
#'the drift parameter can become unidentifiable, especially if an intercept term is included in the GAM linear
#'predictor (which it is by default when calling \code{\link[mgcv]{jagam}}). Drift parameters will also likely
#'be unidentifiable if using dynamic factor models. Therefore this defaults to \code{FALSE}
#'@param chains \code{integer} specifying the number of parallel chains for the model
#'@param burnin \code{integer} specifying the number of warmup iterations of the Markov chain to run
#'to tune sampling algorithms
#'@param samples \code{integer} specifying the number of post-warmup iterations of the Markov chain to run for
#'sampling the posterior distribution
#'@param thin Thinning interval for monitors
#'@param parallel \code{logical} specifying whether multiple cores should be used for
#'generating MCMC simulations in parallel. If \code{TRUE}, the number of cores to use will be
#'\code{min(c(chains, parallel::detectCores() - 1))}
#'@param threads \code{integer} Experimental option to use multithreading for within-chain
#'parallelisation in \code{Stan}. We recommend its use only if you are experienced with
#'\code{Stan}'s `reduce_sum` function and have a slow running model that cannot be sped
#'up by any other means. Only available when using \code{Cmdstan} as the backend
#'@param priors An optional \code{data.frame} with prior
#'definitions (in JAGS or Stan syntax). See \code{\link{get_mvgam_priors}} and
#''Details' for more information on changing default prior distributions
#'@param refit Logical indicating whether this is a refit, called using \code{\link{update.mvgam}}. Users should leave
#'as `FALSE`
#'@param upper_bounds Optional \code{vector} of \code{integer} values specifying upper limits for each series. If supplied,
#'this generates a modified likelihood where values above the bound are given a likelihood of zero. Note this modification
#'is computationally expensive in \code{JAGS} but can lead to better estimates when true bounds exist. Default is to remove
#'truncation entirely (i.e. there is no upper bound for each series). Currently not implemented
#'in `Stan`
#'@param use_stan Logical. If \code{TRUE} and if \code{rstan} is installed, the model will be compiled and sampled using
#'the Hamiltonian Monte Carlo with a call to \code{\link[cmdstanr]{cmdstan_model}} or, if `cmdstanr` is not available,
#'a call to \code{\link[rstan]{stan}}. Note that
#'there are many more options when using `Stan` vs `JAGS` (the only "advantage" of `JAGS` is the ability
#'to use a Tweedie family).
#'@param max_treedepth positive integer placing a cap on the number of simulation steps evaluated during each iteration when
#'`use_stan == TRUE`. Default is `12`. Increasing this value can sometimes help with exploration of complex
#'posterior geometries, but it is rarely fruitful to go above a `max_treedepth` of `14`
#'@param adapt_delta positive numeric between `0` and `1` defining the target average proposal acceptance probability
#'during Stan's adaptation period, if `use_stan == TRUE`. Default is `0.8`. In general you should not need to change adapt_delta
#'unless you see a warning message about divergent transitions, in which case you can increase adapt_delta from the default
#'to a value closer to `1` (e.g. from `0.95` to `0.99`, or from `0.99` to `0.999`, etc).
#'The step size used by the numerical integrator is a function of `adapt_delta` in that increasing
#'`adapt_delta` will result in a smaller step size and fewer divergences. Increasing `adapt_delta` will
#'typically result in a slower sampler, but it will always lead to a more robust sampler.
#'@param jags_path Optional character vector specifying the path to the location of the `JAGS` executable (.exe) to use
#'for modelling if `use_stan == FALSE`. If missing, the path will be recovered from a call to \code{\link[runjags]{findjags}}
#'@details Dynamic GAMs are useful when we wish to predict future values from time series that show temporal dependence
#'but we do not want to rely on extrapolating from a smooth term (which can sometimes lead to unpredictable and unrealistic behaviours).
#'In addition, smooths can often try to wiggle excessively to capture any autocorrelation that is present in a time series,
#'which exacerbates the problem of forecasting ahead. As GAMs are very naturally viewed through a Bayesian lens, and we often
#'must model time series that show complex distributional features and missing data, parameters for `mvgam` models are estimated
#'in a Bayesian framework using Markov Chain Monte Carlo.
#'\cr
#'\cr
#'*Priors*: A \code{\link[mgcv]{jagam}} model file is generated from \code{formula} and modified to include any latent
#'temporal processes. Prior distributions for most important model parameters can be altered by the user to inspect model
#'sensitivities to given priors (see \code{\link{get_mvgam_priors}} for details). Note that latent trends are estimated on the log scale so choose tau, AR and phi priors
#'accordingly. However more control over the model specification can be accomplished by first using `mvgam` as a
#'baseline, then editing the returned model accordingly. The model file can be edited and run outside
#'of `mvgam` by setting \code{run_model = FALSE} and this is encouraged for complex modelling tasks. Note, no priors are
#'formally checked to ensure they are in the right syntax for the respective probabilistic modelling framework, so it is
#'up to the user to ensure these are correct (i.e. use `dnorm` for normal densities in `JAGS`, with the mean and precision
#'parameterisation; but use `normal` for normal densities in `Stan`, with the mean and standard deviation parameterisation)
#'\cr
#'\cr
#'*Random effects*: For any smooth terms using the random effect basis (\code{\link[mgcv]{smooth.construct.re.smooth.spec}}),
#'a non-centred parameterisation is automatically employed to avoid degeneracies that are common in hierarchical models.
#'Note however that centred versions may perform better for series that are particularly informative, so as with any
#'foray into Bayesian modelling, it is worth building an understanding of the model's assumptions and limitations by following a
#'principled workflow. Also note that models are parameterised using `drop.unused.levels = FALSE` in \code{\link[mgcv]{jagam}}
#'to ensure predictions can be made for all levels of the supplied factor variable
#'\cr
#'\cr
#'*Overdispersion parameters*: When more than one series is included in \code{data_train} and an overdispersed
#'exponential family is used, additional observation family parameters
#'(i.e. `phi` for `nb()` or `sigma` for `gaussian()`) are
#'estimated independently for each series.
#'\cr
#'\cr
#'*Factor regularisation*: When using a dynamic factor model for the trends with `JAGS` factor precisions are given
#'regularized penalty priors to theoretically allow some factors to be dropped from the model by squeezing increasing
#'factors' variances to zero. This is done to help protect against selecting too many latent factors than are needed to
#'capture dependencies in the data, so it can often be advantageous to set `n_lv` to a slightly larger number. However
#'larger numbers of factors do come with additional computational costs so these should be balanced as well. When using
#'`Stan`, all factors are parameterised with `sd = 0.1`
#'\cr
#'\cr
#'*Residuals*: For each series, randomized quantile (i.e. Dunn-Smyth) residuals are calculated for inspecting model diagnostics
#'If the fitted model is appropriate then Dunn-Smyth residuals will be standard normal in distribution and no
#'autocorrelation will be evident. When a particular observation is missing, the residual is calculated by comparing independent
#'draws from the model's posterior distribution
#'\cr
#'\cr
#'*Using Stan*: `mvgam` is primarily designed to use Hamiltonian Monte Carlo for parameter estimation
#'via the software `Stan` (using either the `cmdstanr` or `rstan` interface).
#'There are great advantages when using `Stan` over Gibbs / Metropolis Hastings samplers, which includes the option
#'to estimate smooth latent trends via [Hilbert space approximate Gaussian Processes](https://arxiv.org/abs/2004.11408).
#'This often makes sense for ecological series, which we expect to change smoothly. In `mvgam`, latent squared
#'exponential GP trends are approximated using by default \code{40} basis functions, which saves computational
#'costs compared to fitting full GPs while adequately estimating
#'GP \code{alpha} and \code{rho} parameters. Because of the many advantages of `Stan` over `JAGS`,
#'*further development of the package will only be applied to `Stan`*. This includes the planned addition
#'of more response distributions, plans to handle zero-inflation, and plans to incorporate a greater
#'variety of trend models. Users are strongly encouraged to opt for `Stan` over `JAGS` in any proceeding workflows
#'@author Nicholas J Clark
#'@references Nicholas J Clark & Konstans Wells (2020). Dynamic generalised additive models (DGAMs) for forecasting discrete ecological time series
#'Methods in Ecology and Evolution. 14:3, 771-784.
#'@seealso \code{\link[mgcv]{jagam}}, \code{\link[mgcv]{gam}}
#'@return A \code{list} object of class \code{mvgam} containing model output, the text representation of the model file,
#'the mgcv model output (for easily generating simulations at
#'unsampled covariate values), Dunn-Smyth residuals for each series and key information needed
#'for other functions in the package. See \code{\link{mvgam-class}} for details.
#'
#'@examples
#'\dontrun{
#'# Simulate a collection of three time series that have shared seasonal dynamics
# # and independent random walk trends, with a Poisson observation process
#'dat <- sim_mvgam(T = 80, n_series = 3, prop_missing = 0.1,
#'                 trend_rel = 0.6)
#'
#'# Plot key summary statistics for a single series
#'plot_mvgam_series(data = dat$data_train, series = 1)
#'
#'# Plot all series together
#'plot_mvgam_series(data = dat$data_train, series = 'all')
#'
#'# Formulate a model using Stan where series share a cyclic smooth for
#'# seasonality and each series has an independent random walk temporal process;
#'# Set run_model = FALSE to inspect the returned objects
#'mod1 <- mvgam(formula = y ~ s(season, bs = 'cc'),
#'              data = dat$data_train,
#'              trend_model = 'RW',
#'              family = 'poisson',
#'              use_stan = TRUE,
#'              run_model = FALSE)
#'
#'# View the model code in Stan language
#' code(mod1)
#'
#'
#' # Inspect the data objects needed to condition the model
#' str(mod1$model_data)
#'
#' # Inspect the initial value function used to initialise the MCMC chains
#' mod1$inits
#'
#' # The following code can be used to run the model outside of mvgam; first using rstan
#' model_data <- mod1$model_data
#' library(rstan)
#' fit <- stan(model_code = mod1$model_file,
#'            data = model_data,
#'            init = mod1$inits)
#'
#' # Now using cmdstanr
#' library(cmdstanr)
#' model_data <- mod1$model_data
#' cmd_mod <- cmdstan_model(write_stan_file(mod1$model_file),
#'                         stanc_options = list('canonicalize=deprecations,braces,parentheses'))
#' cmd_mod$print()
#' fit <- cmd_mod$sample(data = model_data,
#'                      chains = 4,
#'                      parallel_chains = 4,
#'                      refresh = 100,
#'                      init = mod1$inits)
#'
#' # Now fit the model using mvgam with the Stan backend
#' mod1 <- mvgam(formula = y ~ s(season, bs = 'cc'),
#'               data = dat$data_train,
#'               trend_model = 'RW',
#'               family = poisson(),
#'               use_stan = TRUE)
#'
#' # Extract the model summary
#' summary(mod1)
#'
#' # Plot the estimated historical trend and forecast for one series
#' plot(mod1, type = 'trend', series = 1)
#' plot(mod1, type = 'forecast', series = 1)
#'
#' # Compute the forecast using covariate information in data_test
#' plot(object = mod1, type = 'trend', newdata = dat$data_test,
#'      series = 1)
#' plot(object = mod1, type = 'forecast', newdata = dat$data_test,
#'      series = 1)
#'
#' # Plot the estimated seasonal smooth function
#'plot(mod1, type = 'smooths')
#'
#' # Plot estimated first derivatives of the smooth
#' plot(mod1, type = 'smooths', derivatives = TRUE)
#'
#' # Plot partial residuals of the smooth
#' plot(mod1, type = 'smooths', residuals = TRUE)
#'
#' # Plot posterior realisations for the smooth
#' plot(mod1, type = 'smooths', realisations = TRUE)
#'
#' # Extract observation model beta coefficient draws as a data.frame
#' beta_draws_df <- as.data.frame(mod1, variable = 'betas')
#' head(beta_draws_df)
#' str(beta_draws_df)
#'
#' # Example of supplying a trend_map so that some series can share
#' # latent trend processes
#' sim <- sim_mvgam(n_series = 3)
#' mod_data <- sim$data_train
#'
#' # Here, we specify only two latent trends; series 1 and 2 share a trend,
#' # while series 3 has it's own unique latent trend
#' trend_map <- data.frame(series = unique(mod_data$series),
#'                        trend = c(1,1,2))
#'
#' # Fit the model using AR1 trends
#' mod1 <- mvgam(y ~ s(season, bs = 'cc'),
#'               trend_map = trend_map,
#'               trend_model = 'AR1',
#'               data = mod_data,
#'               return_model_data = TRUE)
#'
#' # The mapping matrix is now supplied as data to the model in the 'Z' element
#' mod1$model_data$Z
#' code(mod1)
#'
#' # The first two series share an identical latent trend; the third is different
#' plot(mod1, type = 'trend', series = 1)
#' plot(mod1, type = 'trend', series = 2)
#' plot(mod1, type = 'trend', series = 3)
#'
#' # Example of how to use dynamic coefficients
#' # Simulate a time-varying coefficient for the effect of temperature
#' set.seed(3)
#' N = 200
#' beta_temp <- vector(length = N)
#' beta_temp[1] <- 0.4
#' for(i in 2:N){
#'   beta_temp[i] <- rnorm(1, mean = beta_temp[i - 1], sd = 0.025)
#'}
#'
#' # Simulate the temperature covariate
#' temp <- rnorm(N, sd = 1)
#' # Simulate the Gaussian observation process
#' out <- rnorm(N, mean = 4 + beta_temp * temp,
#'              sd = 0.5)
#'
#' # Gather necessary data into a data.frame; split into training / testing
#'data = data.frame(out, temp, time = seq_along(temp))
#'data_train <- data[1:180,]
#'data_test <- data[181:200,]
#'
#' # Fit the model using the dynamic() formula helper
#' mod <- mvgam(formula = out ~ dynamic(temp, rho = 8),
#'              family = gaussian(),
#'             data = data_train,
#'             newdata = data_test)
#'
#' # Inspect the model summary, forecast and time-varying coefficient distribution
#' summary(mod)
#' plot(mod, type = 'smooths')
#' plot(mod, type = 'forecast', newdata = data_test)
#'
#' # Propagating the smooth term shows how the coefficient is expected to evolve
#' plot_mvgam_smooth(mod, smooth = 1, newdata = data)
#' abline(v = 180, lty = 'dashed', lwd = 2)
#'
#' # Example showing how to incorporate an offset; simulate some count data
#' # with different means per series
#' set.seed(100)
#' dat <- sim_mvgam(trend_rel = 0, mu = c(0, 2, 2), seasonality = 'hierarchical')
#'
#' # Add offset terms to the training and testing data
#' dat$data_train$offset <- 0.5 * as.numeric(dat$data_train$series)
#' dat$data_test$offset <- 0.5 * as.numeric(dat$data_test$series)
#'
#' # Fit a model that includes the offset in the linear predictor as well as
#' # hierarchical seasonal smooths
#' mod1 <- mvgam(formula = y ~ offset(offset) +
#'          s(series, bs = 're') +
#'          s(season, bs = 'cc') +
#'          s(season, by = series, m = 1, k = 5),
#'          data = dat$data_train,
#'          trend_model = 'None',
#'          use_stan = TRUE)
#'
#' # Inspect the model file to see the modification to the linear predictor
#' # (eta)
#' mod1$model_file
#'
#' # Forecasts for the first two series will differ in magnitude
#' layout(matrix(1:2, ncol = 2))
#' plot(mod1, type = 'forecast', series = 1, newdata = dat$data_test,
#'      ylim = c(0, 75))
#' plot(mod1, type = 'forecast', series = 2, newdata = dat$data_test,
#'      ylim = c(0, 75))
#' layout(1)
#'
#' # Changing the offset for the testing data should lead to changes in
#' # the forecast
#' dat$data_test$offset <- dat$data_test$offset - 2
#' plot(mod1, 'forecast', newdata = dat$data_test)
#'
#' # Relative Risks can be computed by fixing the offset to the same value
#' # for each series
#' dat$data_test$offset <- rep(1, NROW(dat$data_test))
#' preds_rr <- predict(mod1, type = 'link', newdata = dat$data_test)
#' series1_inds <- which(dat$data_test$series == 'series_1')
#' series2_inds <- which(dat$data_test$series == 'series_2')
#'
#' # Relative Risks are now more comparable among series
#' layout(matrix(1:2, ncol = 2))
#' plot(preds_rr[1, series1_inds], type = 'l', col = 'grey75',
#'      ylim = range(preds_rr),
#'      ylab = 'Series1 Relative Risk', xlab = 'Time')
#' for(i in 2:50){
#'  lines(preds_rr[i, series1_inds], col = 'grey75')
#' }
#'
#' plot(preds_rr[1, series2_inds], type = 'l', col = 'darkred',
#'      ylim = range(preds_rr),
#'      ylab = 'Series2 Relative Risk', xlab = 'Time')
#' for(i in 2:50){
#'  lines(preds_rr[i, series2_inds], col = 'darkred')
#'  }
#' layout(1)
#' }
#'@export

mvgam = function(formula,
                 trend_formula,
                 knots,
                 data,
                 data_train,
                 newdata,
                 data_test,
                 run_model = TRUE,
                 prior_simulation = FALSE,
                 return_model_data = FALSE,
                 family = 'poisson',
                 use_lv = FALSE,
                 n_lv,
                 trend_map,
                 trend_model = 'None',
                 drift = FALSE,
                 chains = 4,
                 burnin = 500,
                 samples = 500,
                 thin = 1,
                 parallel = TRUE,
                 threads = 1,
                 priors,
                 upper_bounds,
                 refit = FALSE,
                 use_stan = TRUE,
                 max_treedepth,
                 adapt_delta,
                 jags_path){

  # Check arguments
  if(missing("data") & missing("data_train")){
    stop('Argument "data" is missing with no default')
  }

  if(!missing("data")){
    data_train <- data
  }

  if(attr(terms(formula), "response") == 0L){
    stop('response variable is missing from formula',
         call. = FALSE)
  }

  if(!as.character(terms(formula(formula))[[2]]) %in% names(data_train)){
    stop(paste0('variable ', terms(formula(formula))[[2]], ' not found in data'),
         call. = FALSE)
  }

  if(!missing("newdata")){
    data_test <- newdata
  }

  if(chains%%1 != 0){
    stop('Argument "chains" must be a positive integer',
         .call = FALSE)
  }

  if(drift && use_lv){
    warning('Cannot identify drift terms in latent factor models; setting "drift = FALSE"')
    drift <- FALSE
  }

  # Validate the family argument
  family <- evaluate_family(family)
  family_char <- match.arg(arg = family$family,
                           choices = c('negative binomial',
                                       "poisson",
                                       "tweedie",
                                       "beta",
                                       "gaussian",
                                       "lognormal",
                                       "student",
                                       "Gamma"))

  # Validate the trend arguments
  trend_model <- evaluate_trend_model(trend_model)

  if(!missing(trend_formula)){
    if(missing(trend_map)){
      trend_map <- data.frame(series = unique(data_train$series),
                              trend = 1:length(unique(data_train$series)))
    }

    if(trend_model != 'RW'){
      stop('only random walk trends currently supported for trend predictor models',
           call. = FALSE)
    }
  }

  # Check trend_map is correctly specified
  if(!missing(trend_map)){

    # No point in trend mapping if trend model is 'None'
    if(trend_model == 'None'){
      stop('cannot set up latent trends when "trend_model = None"',
           call. = FALSE)
    }

    # Trend mapping not supported by JAGS
    if(!use_stan){
      stop('trend mapping not available for JAGS',
           call. = FALSE)
    }

    # trend_map must have an entry for each unique time series
    if(!all(sort(trend_map$series) == sort(unique(data_train$series)))){
      stop('argument "trend_map" must have an entry for every unique time series in "data"',
           call. = FALSE)
    }

    # trend_map must not specify a greater number of trends than there are series
    if(max(trend_map$trend) > length(unique(data_train$series))){
      stop('argument "trend_map" specifies more latent trends than there are series in "data"',
           call. = FALSE)
    }

    # trend_map must not skip any trends
    if(!all(sort(unique(trend_map$trend)) == seq(1:max(trend_map$trend)))){
      stop('argument "trend_map" must link at least one series to each latent trend')
    }

    # If trend_map correctly specified, set use_lv to TRUE
    use_lv <- TRUE

    # Model should be set up using dynamic factors of the correct length
    n_lv <- max(trend_map$trend)
  }

  # Check MCMC arguments
  if(sign(chains) != 1){
    stop('argument "chains" must be a positive integer',
         call. = FALSE)
  } else {
    if(chains%%1 != 0){
      stop('argument "chains" must be a positive integer',
           call. = FALSE)
    }
  }

  if(sign(burnin) != 1){
    stop('argument "burnin" must be a positive integer',
         call. = FALSE)
  } else {
    if(burnin%%1 != 0){
      stop('argument "burnin" must be a positive integer',
           call. = FALSE)
    }
  }

  # If Stan is to be used, make sure it is installed
  if(use_stan & run_model){
    if(!requireNamespace('rstan', quietly = TRUE)){
      warning('rstan library not found; checking for cmdstanr library')

      if(!requireNamespace('cmdstanr', quietly = TRUE)){
        warning('cmdstanr library not found; setting run_model = FALSE')
        run_model <- FALSE
      }
    }

    }

  # JAGS cannot support latent GP or VAR trends
  if(!use_stan & trend_model %in%c ('GP', 'VAR1')){
    warning('gaussian process and VAR trends not yet supported for JAGS; reverting to Stan')
    use_stan <- TRUE
  }

  if(use_stan & family_char == 'tweedie'){
    warning('Tweedie family not supported for stan; reverting to JAGS')
    use_stan <- FALSE
  }

  # If the model is to be run in JAGS, make sure the JAGS software can be located
  if(!use_stan){
    if(run_model){
      if(!requireNamespace('runjags', quietly = TRUE)){
        warning('runjags library is required but not found; setting run_model = FALSE')
        run_model <- FALSE
      }
    }
  }

  if(!use_stan){
    if(run_model){
      if(missing(jags_path)){
        requireNamespace('runjags', quietly = TRUE)
        jags_path <- runjags::findjags()
      }

      # Code borrowed from the runjags package
      jags_status <- runjags::testjags(jags_path, silent = TRUE)
      if(!jags_status$JAGS.available){
        if(jags_status$os == "windows"){
          # Try it again - sometimes this helps
          Sys.sleep(0.2)
          jags_status <- runjags::testjags(jags_path, silent = TRUE)
        }

        if(!jags_status$JAGS.available){
          cat("Unable to call JAGS using '", jags_path,
              "'\nTry specifying the path to the JAGS binary as jags_path argument, or re-installing the rjags package.\nUse the runjags::testjags() function for more detailed diagnostics.\n", sep="")
          stop("Unable to call JAGS.\nEither use the Stan backend or follow examples in ?mvgam to generate data / model files and run outside of mvgam", call. = FALSE)
        }
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

  # Ensure each series has an observation, even if NA, for each
  # unique timepoint
  all_times_avail = function(time, min_time, max_time){
    identical(as.numeric(sort(time)),
              as.numeric(seq.int(from = min_time, to = max_time)))
  }
  min_time <- min(data_train$time)
  max_time <- max(data_train$time)
  data.frame(series = data_train$series,
             time = data_train$time) %>%
    dplyr::group_by(series) %>%
    dplyr::summarise(all_there = all_times_avail(time,
                                                 min_time,
                                                 max_time)) -> checked_times
  if(any(checked_times$all_there == FALSE)){
    stop('One or more series in "data" is missing observations for one or more timepoints',
         call. = FALSE)
  }

  if(!missing(data_test)){

    # Repeat the check that each series has an observation for each
    # unique timepoint
    min_time <- min(data_test$time)
    max_time <- max(data_test$time)
    data.frame(series = data_test$series,
               time = data_test$time) %>%
      dplyr::group_by(series) %>%
      dplyr::summarise(all_there = all_times_avail(time,
                                                   min_time,
                                                   max_time)) -> checked_times
    if(any(checked_times$all_there == FALSE)){
      stop('One or more series in "newdata" is missing observations for one or more timepoints',
           call. = FALSE)
    }
  }

  # Upper bounds needs to be same length as number of series
  if(!missing(upper_bounds)){
    if(length(upper_bounds) != length(unique(data_train$series))){
      upper_bounds <- rep(upper_bounds,
                          length(unique(data_train$series)))[1:length(unique(data_train$series))]
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

  # Check if there is an offset variable included
  if(is.null(attr(terms(formula(formula)), 'offset'))){
    offset <- FALSE
  } else {
    offset <- TRUE
  }

  # Ensure outcome is labelled 'y' when feeding data to the model for simplicity
  orig_formula <- formula
  formula <- interpret_mvgam(formula, N = max(data_train$time))
  form_terms <- terms(formula(formula))
  if(terms(formula(formula))[[2]] != 'y'){

    # Check if 'y' is in names, but only if this is not a refit
    if(!refit){
      if('y' %in% names(data_train)){
        stop('variable "y" found in data but not used as outcome. mvgam uses the name "y" when modeling so this variable should be re-named',
             call. = FALSE)
      }
    }
    data_train$y <- data_train[[terms(formula(formula))[[2]]]]
    if(!missing(data_test)){
      data_test$y <- data_test[[terms(formula(formula))[[2]]]]
    }
  }

  # If there are missing values in y, use predictions from an initial mgcv model to fill
  # these in so that initial values to maintain the true size of the training dataset
  orig_y <- data_train$y

  # Initiate the GAM model using mgcv so that the linear predictor matrix can be easily calculated
  # when simulating from the Bayesian model later on;
  ss_gam <- mvgam_setup(formula = formula,
                        family = family_to_mgcvfam(family),
                        data = data_train,
                        drop.unused.levels = FALSE,
                        maxit = 30)

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

  # Make JAGS file and appropriate data structures
  if(length(ss_gam$smooth) == 0){
    smooths_included <- FALSE
    # If no smooth terms are included, jagam will fail; so add a fake one and remove
    # it from the model and data structures later
    data_train$fakery <- rnorm(length(data_train$y))
    form_fake <- update.formula(formula, ~ . + s(fakery))
    fakery_names <- names(suppressWarnings(mgcv::gam(form_fake,
                                                     data = data_train,
                                                     family = family_to_mgcvfam(family),
                                                     drop.unused.levels = FALSE,
                                                     control = list(nthreads = min(4, parallel::detectCores()-1),
                                                                    maxit = 1)))$coefficients)
    xcols_drop <- grep('s(fakery', fakery_names, fixed = TRUE)
    if(!missing(knots)){
      ss_jagam <- mgcv::jagam(form_fake,
                              data = data_train,
                              family = family_to_jagamfam(family_char),
                              file = 'base_gam.txt',
                              sp.prior = 'gamma',
                              diagonalize = F,
                              knots = knots,
                              drop.unused.levels = FALSE)
    } else {
      ss_jagam <- mgcv::jagam(form_fake,
                              data = data_train,
                              family = family_to_jagamfam(family_char),
                              file = 'base_gam.txt',
                              sp.prior = 'gamma',
                              diagonalize = F,
                              drop.unused.levels = FALSE)
    }
    data_train$fakery <- NULL
  } else {
    smooths_included <- TRUE

  # If smooth terms included, use the original formula
  if(!missing(knots)){
    ss_jagam <- mgcv::jagam(formula,
                            data = data_train,
                            family = family_to_jagamfam(family_char),
                            file = 'base_gam.txt',
                            sp.prior = 'gamma',
                            diagonalize = FALSE,
                            knots = knots,
                            drop.unused.levels = FALSE)
  } else {
    ss_jagam <- mgcv::jagam(formula,
                            data = data_train,
                            family = family_to_jagamfam(family_char),
                            file = 'base_gam.txt',
                            sp.prior = 'gamma',
                            diagonalize = FALSE,
                            drop.unused.levels = FALSE)
  }
  }

  # Update initial values of lambdas using the full estimates from the
  # fitted gam model to speed convergence; remove initial betas so that the
  # chains can start in very different regions of the parameter space
  ss_jagam$jags.ini$b <- NULL

  if(length(ss_gam$sp) == length(ss_jagam$jags.ini$lambda)){
    ss_jagam$jags.ini$lambda <- ss_gam$sp
    ss_jagam$jags.ini$lambda[log(ss_jagam$jags.ini$lambda) > 10] <- exp(10)
  }

  if(length(ss_gam$smooth) == 0){
    ss_jagam$jags.ini$lambda <- NULL
  }

  # Fill y with NAs if this is a simulation from the priors
  if(prior_simulation){
    data_train$y <- rep(NA, length(data_train$y))
  } else {
    data_train$y <- orig_y
  }
  data_train[[terms(formula(formula))[[2]]]] <- orig_y

  # Read in the base (unmodified) jags model file
  base_model <- suppressWarnings(readLines('base_gam.txt'))

  # Remove lines from the linear predictor section
  lines_remove <- c(1:grep('## response', base_model))
  base_model <- base_model[-lines_remove]

  if(any(grepl('scale <- 1/tau', base_model, fixed = TRUE))){
    base_model <- base_model[-grep('scale <- 1/tau', base_model, fixed = TRUE)]
  }

  if(any(grepl('tau ~ dgamma(.05,.005)', base_model, fixed = TRUE))){
    base_model <- base_model[-grep('tau ~ dgamma(.05,.005)',
                                   base_model, fixed = TRUE)]
  }

  # Any parametric effects in the gam (particularly the intercept) need sensible priors to ensure they
  # do not directly compete with the latent trends
  if(any(grepl('Parametric effect priors', base_model))){

    in_parenth <- regmatches(base_model[grep('Parametric effect priors',
                               base_model) + 1],
               gregexpr( "(?<=\\().+?(?=\\))", base_model[grep('Parametric effect priors',
                                                               base_model) + 1], perl = T))[[1]][1]
    n_terms <- as.numeric(sub(".*:", "", in_parenth))
    ss_jagam$jags.data$p_coefs <- coef(ss_gam)[1:n_terms]

    rmvn <- function(n,mu,sig) {
      L <- mgcv::mroot(sig); m <- ncol(L);
      t(mu + L%*%matrix(rnorm(m*n),m,n))
    }

    # Use the initialised GAM's estimates for parametric effects, but widen them
    # substantially to allow for better exploration of possible alternative model
    # configurations
    beta_sims <- rmvn(1000, coef(ss_gam), ss_gam$Vp)
    ss_jagam$jags.data$p_taus <- apply(as.matrix(beta_sims[,1:n_terms]),
                                       2, function(x) 1 / (sd(x) ^ 2)) * 0.25

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

      # If there are multiple smooths with this label, find out where the random effect
      # smooth sits
      smooth_labs %>%
        dplyr::filter(label == re_smooths[i]) %>%
        dplyr::mutate(smooth_number = dplyr::row_number()) %>%
        dplyr::filter(class == 'random.effect') %>%
        dplyr::pull(smooth_number) -> smooth_number

      in_parenth <- regmatches(base_model[grep(re_smooths[i],
                                               base_model, fixed = T)[smooth_number] + 1],
                               gregexpr( "(?<=\\().+?(?=\\))",
                                         base_model[grep(re_smooths[i],
                                                         base_model, fixed = T)[smooth_number] + 1],
                                         perl = T))[[1]][1]
      n_terms <- as.numeric(sub(".*:", "", in_parenth))
      n_start <- as.numeric(strsplit(sub(".*\\(", "", in_parenth), ':')[[1]][1])
      base_model[grep(re_smooths[i],
                      base_model, fixed = T)[smooth_number] + 1] <- paste0('  for (i in ', n_start, ':',
                                                            n_terms,
                                                            ') {\n   b_raw[i] ~ dnorm(0, 1)\n',
                                                            'b[i] <- ',
                                                            paste0('mu_raw', i), ' + b_raw[i] * ',
                                                            paste0('sigma_raw', i), '\n  }\n  ',
                                                            paste0('sigma_raw', i), ' ~ dexp(0.5)\n',
                                                            paste0('mu_raw', i), ' ~ dnorm(0, 1)')
      base_model[grep(re_smooths[i],
                      base_model, fixed = T)[smooth_number]] <- paste0('  ## prior (non-centred) for ', re_smooths[i], '...')
    }

  }

  base_model[grep('smoothing parameter priors',
                  base_model)] <- c('   ## smoothing parameter priors...')

  # Remove the fakery lines if they were added
  if(!smooths_included){
    base_model <- base_model[-c(grep('## prior for s(fakery)',
                                     trimws(base_model), fixed = TRUE):
                                  (grep('## prior for s(fakery)',
                                        trimws(base_model), fixed = TRUE) + 7))]
  }

  # Add replacement lines for priors, trends and the linear predictor
  fil <- tempfile(fileext = ".xt")
  modification <- add_base_dgam_lines(use_lv)
  cat(c(readLines(textConnection(modification)), base_model), file = fil,
      sep = "\n")
  model_file <- trimws(readLines(fil, n = -1))

  # Modify observation distribution lines
  if(family_char == 'tweedie'){
    model_file <- add_tweedie_lines(model_file, upper_bounds = upper_bounds)

  } else if(family_char == 'poisson'){
    model_file <- add_poisson_lines(model_file, upper_bounds = upper_bounds)

  } else {
    if(missing(upper_bounds)){
      model_file[grep('y\\[i, s\\] ~', model_file)] <- '  y[i, s] ~ dnegbin(rate[i, s], phi[s])'
      model_file[grep('ypred\\[i, s\\] ~', model_file)] <- '  ypred[i, s] ~ dnegbin(rate[i, s], phi[s])'
    }
  }

  # Modify lines needed for the specified trend model
  model_file <- add_trend_lines(model_file, stan = FALSE,
                                use_lv = use_lv,
                                trend_model = if(trend_model %in% c('RW', 'VAR1')){'RW'} else {trend_model},
                                drift = drift)

  # Use informative priors based on the fitted mgcv model to speed convergence
  # and eliminate searching over strange parameter spaces
  if(length(ss_gam$sp) == length(ss_jagam$jags.ini$lambda)){
    model_file[grep('lambda\\[i\\] ~', model_file)] <- '   lambda[i] ~ dexp(1/sp[i])'
  } else {
    model_file[grep('lambda\\[i\\] ~', model_file)] <- '   lambda[i] ~ dexp(0.05)'
  }

  # Final tidying of the JAGS model for readability
  clean_up <- vector()
  for(x in 1:length(model_file)){
    clean_up[x] <- model_file[x-1] == "" & model_file[x] == ""
  }
  clean_up[is.na(clean_up)] <- FALSE
  model_file <- model_file[!clean_up]

  # Add in the offset if needed
  if(offset){
    model_file[grep('eta <- X %*% b', model_file, fixed = TRUE)] <-
      "eta <- X %*% b + offset"
    if(!missing(data_test) & !prior_simulation){

      get_offset <- function(model) {
        nm1 <- names(attributes(model$terms)$dataClasses)
        if('(offset)' %in% nm1) {
          deparse(as.list(model$call)$offset)
        } else {

          sub("offset\\((.*)\\)$", "\\1", grep('offset', nm1, value = TRUE))
        }
      }

      ss_jagam$jags.data$offset <- c(ss_jagam$jags.data$offset,
                                     data_test[[get_offset(ss_gam)]])
    }
  }

  model_file_jags <- textConnection(model_file)

  # Covariate dataframe including training and testing observations
  if(!missing(data_test) & !prior_simulation){
    suppressWarnings(lp_test  <- try(predict(ss_gam,
                                             newdata = data_test,
                                             type = 'lpmatrix'),
                                     silent = TRUE))

    if(inherits(lp_test, 'try-error')){
      testdat <- data.frame(time = data_test$time)

      terms_include <- names(ss_gam$coefficients)[which(!names(ss_gam$coefficients)
                                                        %in% '(Intercept)')]
      if(length(terms_include) > 0){
        newnames <- vector()
        newnames[1] <- 'time'
        for(i in 1:length(terms_include)){
          testdat <- cbind(testdat, data.frame(data_test[[terms_include[i]]]))
          newnames[i+1] <- terms_include[i]
        }
        colnames(testdat) <- newnames
      }
      suppressWarnings(lp_test  <- predict(ss_gam,
                                           newdata = testdat,
                                           type = 'lpmatrix'))
    }

    # Remove fakery columns from design matrix if no smooth terms were included
    if(!smooths_included){
      ss_jagam$jags.data$X <- as.matrix(ss_jagam$jags.data$X[,-c(xcols_drop)],
                                        ncol = NCOL(lp_test))
    }

    X <- data.frame(rbind(ss_jagam$jags.data$X, lp_test))

    # Add a time variable
    if(inherits(data_train, 'list')){
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

    X$time <- rbind(data_train, data_test[,1:NCOL(data_train)]) %>%
      dplyr::left_join(rbind(data_train, data_test[,1:NCOL(data_train)]) %>%
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

    # Remove fakery columns from design matrix if no smooth terms were included
    if(!smooths_included){
      X[,xcols_drop] <- NULL
    }

    if(class(data_train)[1] == 'list'){
      temp_dat <- data.frame(time = data_train$time)
      X$time <- temp_dat %>%
        dplyr::left_join(temp_dat %>%
                           dplyr::select(time) %>%
                           dplyr::distinct() %>%
                           dplyr::arrange(time) %>%
                           dplyr::mutate(time = dplyr::row_number()),
                         by = c('time')) %>%
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
  ytimes <- matrix(NA, nrow = length(unique(X$time)), ncol = length(unique(X$series)))
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
  if(NCOL(ss_jagam$jags.data$X) == 1){
    model_file[grep('eta <-', model_file, fixed = TRUE)] <- 'eta <- X * b'
  }

  if(!missing(upper_bounds)){
    ss_jagam$jags.data$upper_bound <- upper_bounds
  }

  if(length(ss_gam$sp) == length(ss_jagam$jags.ini$lambda)){
    ss_jagam$jags.data$sp <- ss_gam$sp
  }

  # Machine epsilon for minimum allowable non-zero rate
  if(family_char == 'negative binomial'){
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

  if(missing(upper_bounds)){
    upper_bounds <- NULL
  }

  if(use_lv){
    n_lv <- ss_jagam$jags.data$n_lv
  } else {
    n_lv <- NULL
  }

  if(missing(data_test)){
    data_test <- NULL
  }

  # Remove Smooth penalty matrix if no smooths were used in the formula
  if(!smooths_included){
    ss_jagam$jags.data[[grep('S.*', names(ss_jagam$jags.data))]] <- NULL
    ss_jagam$jags.data$sp <- NULL
    ss_jagam$jags.data$zero <- NULL
  }

  # Add information about the call and necessary data structures to the model file
  # Get dimensions and numbers of smooth terms
  snames <- names(ss_jagam$jags.data)[grep('S.*', names(ss_jagam$jags.data))]
  if(length(snames) == 0){
    smooth_penalty_data <- NULL
  } else {
    smooth_dims <- matrix(NA, ncol = 2, nrow = length(snames))
    for(i in 1:length(snames)){
      smooth_dims[i,] <- dim(ss_jagam$jags.data[[snames[i]]])
    }
    smooth_penalty_data <- vector()
    for(i in 1:length(snames)){
      smooth_penalty_data[i] <- paste0('matrix ',
                                       snames[i],
                                       ';  mgcv smooth penalty matrix ', snames[i])
    }
  }

  if('sp' %in% names(ss_jagam$jags.data)){
    if(length(ss_jagam$jags.data$sp) == 1){
      sp_data <- c(paste0('real sp;  inverse exponential location prior for smoothing parameter ',
                        paste0(names(ss_jagam$jags.data$sp))),
                   '___________values ranging 5 - 50 are a good start')
    } else {
      sp_data <- c(paste0('vector sp;  inverse exponential location priors for smoothing parameters: ',
                        paste0(names(ss_jagam$jags.data$sp), collapse = '; ')),
                   '___________values ranging 5 - 50 are a good start')
    }

  } else {
    sp_data <- NULL
  }

  if('p_coefs' %in% names(ss_jagam$jags.data)){
    parametric_ldata <- paste0('vector p_coefs;  vector (length = ',
                               length(ss_jagam$jags.data$p_coefs),
                               ') of prior Gaussian means for parametric effects')
  } else {
    parametric_ldata <- NULL
  }

  if('p_taus' %in% names(ss_jagam$jags.data)){
    parametric_tdata <- paste0('vector p_taus;  vector (length = ',
                               length(ss_jagam$jags.data$p_coefs),
                               ') of prior Gaussian precisions for parametric effects')
  } else {
    parametric_tdata <- NULL
  }

  # A second check for any smooth parameters
  if(any(grep('lambda', model_file, fixed = TRUE))){
    smooths_included <- TRUE
  } else {
    smooths_included <- smooths_included
  }

  if(any(grep('K.* <- ', model_file))){
    smooths_included <- TRUE
  } else {
    smooths_included <- smooths_included
  }

  # Add in additional data structure information for the model file heading
  if(family_char == 'negative binomial'){
    min_eps <- paste0('min_eps; .Machine$double.eps (smallest floating-point number x such that 1 + x != 1)\n')
  } else {
    min_eps <- NULL
  }

  if(smooths_included){
    zeros <- paste0('vector zero;  prior basis coefficient locations vector of length ncol(X)\n')
  } else {
    zeros <- NULL
  }

  if(offset){
    offset_line <- paste0('offset; offset vector of length (n x n_series)\n')
  } else {
    offset_line <- NULL
  }

  model_file <- c('JAGS model code generated by package mvgam',
                       '\n',
                       'GAM formula:',
                  gsub('\"', '', paste(formula[2],formula[3],sep=' ~ ')),
                       '\n',
                  'Trend model:',
                  trend_model,
                  '\n',
                  'Required data:',
                  'integer n;  number of timepoints per series\n',
                  'integer n_series;  number of series\n',
                  'matrix y;  time-ordered observations of dimension n x n_series (missing values allowed)\n',
                  'matrix ytimes;  time-ordered n x n_series matrix (which row in X belongs to each [time, series] observation?)\n',
                  'matrix X;  mgcv GAM design matrix of dimension (n x n_series) x basis dimension\n',
                  paste0(smooth_penalty_data),
                  offset_line,
                  zeros,
                  paste0(parametric_ldata),
                  paste0(parametric_tdata),
                  sp_data,
                  min_eps,
                  '\n',
                  model_file)

  # Get names of smoothing parameters
  if(smooths_included){
    name_starts <- unlist(purrr::map(ss_jagam$pregam$smooth, 'first.sp'))
    name_ends <- unlist(purrr::map(ss_jagam$pregam$smooth, 'last.sp'))

    rho_names <- unlist(lapply(seq(1:length(ss_gam$smooth)), function(i){

      number_seq <- seq(1:(1 + name_ends[i] - name_starts[i]))
      number_seq[1] <- ''

      paste0(rep(ss_gam$smooth[[i]]$label,
                 length(number_seq)),
             number_seq)
    }))
  } else {
    rho_names <- NA
  }

  #### Set up model file and modelling data ####
  if(use_stan){
    fit_engine <- 'stan'
    use_cmdstan <- FALSE

    # Import the base Stan model file
    modification <- add_base_dgam_lines(stan = TRUE, use_lv = use_lv)
    unlink('base_gam_stan.txt')
    cat(modification, file = 'base_gam_stan.txt', sep = '\n', append = T)
    base_stan_model <- trimws(suppressWarnings(readLines('base_gam_stan.txt')))
    unlink('base_gam_stan.txt')

    # Add necessary trend structure
    base_stan_model <- add_trend_lines(model_file = base_stan_model,
                                       stan = TRUE,
                                       trend_model = if(trend_model %in% c('RW', 'VAR1')){'RW'} else {trend_model},
                                       use_lv = use_lv,
                                       drift = drift)

    # Add remaining data, model and parameters blocks to the Stan model file;
    # gather Stan data structure
    stan_objects <- add_stan_data(jags_file = trimws(model_file),
                                  stan_file = base_stan_model,
                                  use_lv = use_lv,
                                  n_lv = n_lv,
                                  jags_data = ss_jagam$jags.data,
                                  family = family_char,
                                  upper_bounds = upper_bounds)

    if(use_lv){
      stan_objects$model_data$n_lv <- n_lv
    }

    # Set monitor parameters
    param <- get_monitor_pars(family = family_char,
                              use_lv = use_lv,
                              trend_model = trend_model,
                              smooths_included = stan_objects$smooths_included,
                              drift = drift)
    if(any(smooth_labs$class == 'random.effect')){
      param <- c(param, 'mu_raw', 'sigma_raw')
    }

    # Sensible inits needed for the betas, sigmas and overdispersion parameters
    inits <- family_inits(family = family_char, trend_model,
                          smooths_included, model_data)


    # Vectorise likelihoods
    vectorised <- vectorise_stan_lik(model_file = stan_objects$stan_file,
                                     model_data = stan_objects$model_data,
                                     family = family_char,
                                     threads = threads,
                                     trend_model = trend_model,
                                     offset = offset,
                                     drift = drift)

    if(!missing(priors)){
      vectorised$model_file <- update_priors(vectorised$model_file, priors,
                                             use_stan = TRUE)
    } else {
      priors <- NULL
    }

    if(!missing(trend_map)){
      trend_map_setup <- trend_map_mods(model_file = vectorised$model_file,
                                        model_data = vectorised$model_data,
                                        trend_map = trend_map,
                                        data_train = data_train,
                                        ytimes = ytimes,
                                        n_lv = n_lv,
                                        trend_model = trend_model)
      vectorised$model_file <- trend_map_setup$model_file
      vectorised$model_data <- trend_map_setup$model_data

      if(trend_model %in% c('RW', 'AR1', 'AR2', 'AR3')){
        param <- c(param, 'sigma')
      }

      # If trend formula specified, add the predictors for the trend models
      if(!missing(trend_formula)){
        trend_pred_setup <- add_trend_predictors(
          trend_formula = trend_formula,
          trend_map = trend_map,
          trend_model = trend_model,
          data_train = data_train,
          data_test = if(missing(data_test)){
            NULL
          } else {
            data_test
          },
          model_file = vectorised$model_file,
          model_data = vectorised$model_data,
          drift = drift)

        vectorised$model_file <- trend_pred_setup$model_file
        vectorised$model_data <- trend_pred_setup$model_data
        trend_mgcv_model <- trend_pred_setup$trend_mgcv_model

        param <- c(param, 'b_trend')

        if(trend_pred_setup$trend_smooths_included){
          param <- c(param, 'rho_trend')
        }

        if(trend_pred_setup$trend_random_included){
          param <- c(param, 'mu_raw_trend', 'sigma_raw_trend')
        }

      }
    }

  } else {
    # Set up data and model file for JAGS
    if(!smooths_included){
      inits <- NULL
    } else {
      inits <- ss_jagam$jags.ini
    }
    initlist <- replicate(chains, inits,
                          simplify = FALSE)
    inits <- initlist

    if(!missing(priors)){
      model_file <- update_priors(model_file, priors, use_stan = FALSE)
    } else {
      priors <- NULL
    }

    # Set monitor parameters and initial values
    param <- get_monitor_pars(family_char,
                              smooths_included = smooths_included,
                              use_lv, trend_model, drift)

    # Add random effect parameters for monitoring
    if(any(smooth_labs$class == 'random.effect')){
      param <- c(param, paste0('mu_raw', 1:length(re_smooths)))
      param <- c(param, paste0('sigma_raw', 1:length(re_smooths)))
    }

  }

  #### Return only the model file and all data / inits needed to run the model
  # outside of mvgam ####
  if(!run_model){
    unlink('base_gam.txt')
      output <- structure(list(call = orig_formula,
                               trend_call = if(!missing(trend_formula)){
                                 trend_formula
                               } else {
                                 NULL
                               },
                               family = family_char,
                               trend_model = trend_model,
                               drift = drift,
                               priors = priors,
                               model_file = if(use_stan){
                                 vectorised$model_file
                               } else {
                                 trimws(model_file)
                               },
                               model_data = if(use_stan){
                                 vectorised$model_data
                               } else {
                                 ss_jagam$jags.data
                               },
                               inits = inits,
                               monitor_pars = param,
                               mgcv_model = ss_gam,
                               trend_mgcv_model = if(!missing(trend_formula)){
                                 trend_mgcv_model
                               } else {
                                 NULL
                               },
                               sp_names = rho_names,
                               ytimes = ytimes,
                               use_lv = use_lv,
                               n_lv = n_lv,
                               upper_bounds = upper_bounds,
                               obs_data = data_train,
                               test_data = data_test,
                               fit_engine = if(use_stan){
                                 'stan'
                               } else {
                                 'jags'
                               },
                               max_treedepth = if(use_stan){
                                 12
                               } else {
                                 NULL
                               },
                               adapt_delta = if(use_stan){
                                 0.85
                               } else {
                                 NULL
                               }),
                          class = 'mvgam_prefit')

  #### Else if running the model, complete the setup for fitting ####
  } else {
    if(use_stan){
      # Remove data likelihood if this is a prior sampling run
      if(prior_simulation){
        vectorised$model_file <- vectorised$model_file[-c((grep('// likelihood functions',
                                                                 vectorised$model_file,
                                                                 fixed = TRUE) - 1):
                                                              (grep('generated quantities {',
                                                                    vectorised$model_file,
                                                                    fixed = TRUE) - 4))]
      }

      model_data <- vectorised$model_data

      # Check if cmdstan is accessible; if not, use rstan
      if(!requireNamespace('cmdstanr', quietly = TRUE)){
        use_cmdstan <- FALSE
      } else {
        use_cmdstan <- TRUE
        if(is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))){
          use_cmdstan <- FALSE
        }
      }

      if(use_cmdstan){
        message('Using cmdstanr as the backend')
        message()

        if(cmdstanr::cmdstan_version() >= "2.29.0"){
          if(threads > 1){
            cmd_mod <- cmdstanr::cmdstan_model(cmdstanr::write_stan_file(vectorised$model_file),
                                               stanc_options = list('O1',
                                                                    'canonicalize=deprecations,braces,parentheses'),
                                               cpp_options = list(stan_threads = TRUE))
          } else {
            cmd_mod <- cmdstanr::cmdstan_model(cmdstanr::write_stan_file(vectorised$model_file),
                                               stanc_options = list('O1',
                                                                    'canonicalize=deprecations,braces,parentheses'))
          }

        } else {
          if(threads > 1){
            cmd_mod <- cmdstanr::cmdstan_model(cmdstanr::write_stan_file(vectorised$model_file),
                                               cpp_options = list(stan_threads = TRUE))
          } else {
            cmd_mod <- cmdstanr::cmdstan_model(cmdstanr::write_stan_file(vectorised$model_file))
          }
        }

        if(missing(max_treedepth)){
          max_treedepth <- 12
        }
        if(missing(adapt_delta)){
          adapt_delta <- 0.85
        }

        # Condition the model using Cmdstan
        if(prior_simulation){
          fit1 <- cmd_mod$sample(data = model_data,
                                 chains = chains,
                                 parallel_chains = min(c(chains, parallel::detectCores() - 1)),
                                 threads_per_chain = if(threads > 1){ threads } else { NULL },
                                 refresh = 100,
                                 init = inits,
                                 max_treedepth = 12,
                                 adapt_delta = 0.8,
                                 iter_sampling = samples,
                                 iter_warmup = 200,
                                 show_messages = FALSE,
                                 diagnostics = NULL)
        } else {
          fit1 <- cmd_mod$sample(data = model_data,
                                 chains = chains,
                                 parallel_chains = min(c(chains, parallel::detectCores() - 1)),
                                 threads_per_chain = if(threads > 1){ threads } else { NULL },
                                 refresh = 100,
                                 init = inits,
                                 max_treedepth = max_treedepth,
                                 adapt_delta = adapt_delta,
                                 iter_sampling = samples,
                                 iter_warmup = burnin)
        }

        # Convert model files to stan_fit class for consistency
        out_gam_mod <- read_csv_as_stanfit(fit1$output_files(),
                                   variables = param)
        out_gam_mod <- repair_stanfit(out_gam_mod)

      } else {
        requireNamespace('rstan', quietly = TRUE)
        message('Using rstan as the backend')
        message()
        options(mc.cores = parallel::detectCores())

        # Fit the model in rstan using custom control parameters
        if(missing(max_treedepth)){
          max_treedepth <- 12
        }

        if(missing(adapt_delta)){
          adapt_delta <- 0.85
        }

        message("Compiling the Stan program...")
        message()
        if(samples <= burnin){
          samples <- burnin + samples
        }

        if(prior_simulation){
          burnin <- 200
          samples <- 600
          adapt_delta <- 0.8
          max_treedepth <- 12
        }

        stan_control <- list(max_treedepth = max_treedepth,
                             adapt_delta = adapt_delta)
        fit1 <- rstan::stan(model_code = vectorised$model_file,
                            iter = samples,
                            warmup = burnin,
                            chains = chains,
                            data = model_data,
                            cores = min(c(chains, parallel::detectCores() - 1)),
                            init = inits,
                            verbose = FALSE,
                            thin = thin,
                            control = stan_control,
                            pars = param,
                            refresh = 100)

        out_gam_mod <- fit1
      }

    }

    if(!use_stan){
      requireNamespace('runjags', quietly = TRUE)
      fit_engine <- 'jags'
      model_data <- ss_jagam$jags.data
      runjags::runjags.options(silent.jags = TRUE, silent.runjags = TRUE)

      # Initiate adaptation of the model for the full burnin period. This is necessary as JAGS
      # will take a while to optimise the samplers, so long adaptation with little 'burnin'
      # is more crucial than little adaptation but long 'burnin' https://mmeredith.net/blog/2016/Adapt_or_burn.htm
      unlink('base_gam.txt')
      cat(model_file, file = 'base_gam.txt', sep = '\n', append = T)

      message("Compiling the JAGS program...")
      message()

      if(prior_simulation){
        n_adapt <- 500
        n_burn <- 0
        samples <- 1000
        thin <- 1
      } else {
        n_burn <- burnin
        # Rely on long adaptation to tune samplers appropriately
        n_adapt <- max(1000, n_burn - 1000)
      }

      if(parallel){
        cl <- parallel::makePSOCKcluster(min(c(chains, parallel::detectCores() - 1)))
        setDefaultCluster(cl)
        gam_mod <- runjags::run.jags(model = 'base_gam.txt',
                                     data = ss_jagam$jags.data,
                                     modules = 'glm',
                                     inits = initlist,
                                     n.chains = chains,
                                     adapt = n_adapt,
                                     burnin = n_burn,
                                     sample = samples,
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
                                     adapt = n_adapt,
                                     burnin = n_burn,
                                     sample = samples,
                                     jags = jags_path,
                                     thin = thin,
                                     method = "rjags",
                                     monitor = param,
                                     silent.jags = TRUE)
      }
      out_gam_mod <- coda::as.mcmc.list(gam_mod)
    }

  unlink('base_gam.txt')
  unlink(fil)

  # Get Dunn-Smyth Residual distributions for each series
  if(prior_simulation){
    series_resids <- NULL
  } else {
    series_resids <- get_mvgam_resids(object = list(
      model_output = out_gam_mod,
      fit_engine = fit_engine,
      family = family_char,
      obs_data = data_train,
      ytimes = ytimes),
      n_cores = if(parallel){
        min(c(chains, parallel::detectCores() - 1))
        } else {
          1
        })
  }

  if(prior_simulation){
    data_train$y <- orig_y
  }

  # Add Bayesian coefficients to the mgcv model to help with plotting of
  # smooths that aren't yet supported by mvgam plotting functions
  # Extract median beta params for smooths and their covariances
  # so that uncertainty from mgcv plots is reasonably accurate
  if(run_model){
    # Use the empirical covariance matrix from the fitted coefficients
    V <- cov(mcmc_chains(out_gam_mod, 'b'))
    ss_gam$Ve <- ss_gam$Vp <- ss_gam$Vc <- V

    # Add the posterior median coefficients
    p <- mcmc_summary(out_gam_mod, 'b')[,c(4)]
    names(p) <- names(ss_gam$coefficients)
    ss_gam$coefficients <- p

    if(!missing(trend_formula)){
      V <- cov(mcmc_chains(out_gam_mod, 'b_trend'))
      trend_mgcv_model$Ve <- trend_mgcv_model$Vp <- trend_mgcv_model$Vc <- V

      p <- mcmc_summary(out_gam_mod, 'b_trend')[,c(4)]
      names(p) <- names(trend_mgcv_model$coefficients)
      trend_mgcv_model$coefficients <- p
    }
  }

  #### Return the output as class mvgam ####
  output <- structure(list(call = orig_formula,
                           trend_call = if(!missing(trend_formula)){
                             trend_formula
                           } else {
                             NULL
                           },
                           family = family_char,
                           trend_model = trend_model,
                           drift = drift,
                           priors = priors,
                           model_output = out_gam_mod,
                           model_file = if(use_stan){
                             vectorised$model_file
                           } else {
                             trimws(model_file)
                           },
                           model_data = if(return_model_data){
                             model_data
                           } else {
                             NULL
                           },
                           inits = if(return_model_data){
                             inits
                           } else {
                             NULL
                           },
                           monitor_pars = if(return_model_data){
                             param
                           } else {
                             NULL
                           },
                           sp_names = rho_names,
                           mgcv_model = ss_gam,
                           trend_mgcv_model = if(!missing(trend_formula)){
                             trend_mgcv_model
                           } else {
                             NULL
                           },
                           ytimes = ytimes,
                           resids = series_resids,
                           use_lv = use_lv,
                           n_lv = n_lv,
                           upper_bounds = upper_bounds,
                           obs_data = data_train,
                           test_data = data_test,
                           fit_engine = fit_engine,
                           max_treedepth = if(use_stan){
                             max_treedepth
                           } else {
                             NULL
                           },
                           adapt_delta = if(use_stan){
                             adapt_delta
                           } else {
                             NULL
                           }),
                      class = 'mvgam')

  }

  return(output)
}
