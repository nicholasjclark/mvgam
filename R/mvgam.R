#'Fit a Bayesian dynamic GAM to a univariate or multivariate set of time series
#'
#'This function estimates the posterior distribution for Generalised Additive Models (GAMs) that can include
#'smooth spline functions, specified in the GAM formula, as well as latent temporal processes,
#'specified by `trend_model`. Further modelling options include State-Space representations to allow covariates
#'and dynamic processes to occur on the latent 'State' level while also capturing observation-level effects.
#'Prior specifications are flexible and explicitly encourage users to apply
#'prior distributions that actually reflect their beliefs. In addition, model fits can easily be assessed and
#'compared with posterior predictive checks, forecast comparisons and leave-one-out / leave-future-out cross-validation.
#'
#'@importFrom parallel clusterExport stopCluster setDefaultCluster
#'@importFrom stats formula terms rnorm update.formula predict
#'@importFrom rlang missing_arg
#'@param formula A \code{character} string specifying the GAM observation model formula. These are exactly like the formula
#'for a GLM except that smooth terms, `s()`, `te()`, `ti()`, `t2()`, as well as time-varying
#'`dynamic()` terms, can be added to the right hand side
#'to specify that the linear predictor depends on smooth functions of predictors
#'(or linear functionals of these). In `nmix()` family models, the `formula` is used to
#'set up a linear predictor for the detection probability. Details of the formula syntax used by \pkg{mvgam}
#'can be found in \code{\link{mvgam_formulae}}
#'@param trend_formula An optional \code{character} string specifying the GAM process model formula. If
#'supplied, a linear predictor will be modelled for the latent trends to capture process model evolution
#'separately from the observation model. Should not have a response variable specified on the left-hand side
#'of the formula (i.e. a valid option would be `~ season + s(year)`). Also note that you should not use
#'the identifier `series` in this formula to specify effects that vary across time series. Instead you should use
#'`trend`. This will ensure that models in which a `trend_map` is supplied will still work consistently
#'(i.e. by allowing effects to vary across process models, even when some time series share the same underlying
#'process model). This feature is only currently available for `RW()`, `AR()` and `VAR()` trend models.
#'In `nmix()` family models, the `trend_formula` is used to set up a linear predictor for the underlying
#'latent abundance
#'@param knots An optional \code{list} containing user specified knot values to be used for basis construction.
#'For most bases the user simply supplies the knots to be used, which must match up with the k value supplied
#'(note that the number of knots is not always just `k`). Different terms can use different numbers of knots,
#'unless they share a covariate
#'@param trend_knots As for `knots` above, this is an optional \code{list} of knot values for smooth
#'functions within the `trend_formula`
#'@param data A \code{dataframe} or \code{list} containing the model response variable and covariates
#'required by the GAM \code{formula} and optional \code{trend_formula}. Should include columns:
#'`series` (a \code{factor} index of the series IDs;the number of levels should be identical
#'to the number of unique series labels (i.e. `n_series = length(levels(data$series))`))
#'`time` (\code{numeric} or \code{integer} index of the time point for each observation).
#'Any other variables to be included in the linear predictor of \code{formula} must also be present
#'@param data_train Deprecated. Still works in place of \code{data} but users are recommended to use
#'\code{data} instead for more seamless integration into `R` workflows
#'@param newdata Optional \code{dataframe} or \code{list} of test data containing at least `series` and `time`
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
#'   \item`gaussian()` for real-valued data
#'   \item`betar()` for proportional data on `(0,1)`
#'   \item`lognormal()` for non-negative real-valued data
#'   \item`student_t()` for real-valued data
#'   \item`Gamma()` for non-negative real-valued data
#'   \item`nmix()` for count data with imperfect detection modeled via a
#'   State-Space N-Mixture model. The latent states are Poisson, capturing the 'true' latent
#'   abundance, while the observation process is Binomial to account for imperfect detection.
#'   See \code{\link{mvgam_families}} for an example of how to use this family}
#'Note that only `nb()` and `poisson()` are available if using `JAGS` as the backend.
#'Default is `poisson()`.
#'See \code{\link{mvgam_families}} for more details
#'@param use_lv \code{logical}. If \code{TRUE}, use dynamic factors to estimate series'
#'latent trends in a reduced dimension format. Only available for
#'`RW()`, `AR()` and `GP()` trend models. Defaults to \code{FALSE}
#'@param n_lv \code{integer} the number of latent dynamic factors to use if \code{use_lv == TRUE}.
#'Cannot be \code{> n_series}. Defaults arbitrarily to \code{min(2, floor(n_series / 2))}
#'@param trend_model \code{character} or  \code{function} specifying the time series dynamics for the latent trend. Options are:
#'\itemize{
#'   \item `None` (no latent trend component; i.e. the GAM component is all that contributes to the linear predictor,
#'and the observation process is the only source of error; similarly to what is estimated by \code{\link[mgcv]{gam}})
#'   \item `'RW'` or `RW()`
#'   \item `'AR1'` or `AR(p = 1)`
#'   \item `'AR2'` or `AR(p = 2)`
#'   \item `'AR3'` or `AR(p = 3)`
#'   \item `'VAR1'`  or `VAR()`(only available in \code{Stan})
#'   \item `'PWlogistic`, `'PWlinear'` or `PW()` (only available in \code{Stan})
#'   \item `'GP'` or `GP()` (Gaussian Process with squared exponential kernel;
#'only available in \code{Stan})}
#'
#'For all trend types apart from `GP()` and `PW()`, moving average and/or correlated
#'process error terms can also be estimated (for example, `RW(cor = TRUE)` will set up a
#'multivariate Random Walk if `n_series > 1`). See [mvgam_trends] for more details
#'@param trend_map Optional `data.frame` specifying which series should depend on which latent
#'trends. Useful for allowing multiple series to depend on the same latent trend process, but with
#'different observation processes. If supplied, a latent factor model is set up by setting
#'`use_lv = TRUE` and using the mapping to set up the shared trends. Needs to have column names
#'`series` and `trend`, with integer values in the `trend` column to state which trend each series
#'should depend on. The `series` column should have a single unique entry for each series in the
#'data (names should perfectly match factor levels of the `series` variable in `data`). See examples
#'for details
#'@param drift \code{logical} estimate a drift parameter in the latent trend components. Useful if the latent
#'trend is expected to broadly follow a non-zero slope. Only available for
#'`RW()` and `AR()` trend models. Note that if the latent trend is more or less stationary,
#'the drift parameter can become unidentifiable, especially if an intercept term is included in the GAM linear
#'predictor (which it is by default when calling \code{\link[mgcv]{jagam}}). Drift parameters will also likely
#'be unidentifiable if using dynamic factor models. Therefore this defaults to \code{FALSE}
#'@param chains \code{integer} specifying the number of parallel chains for the model. Ignored
#'if `algorithm %in% c('meanfield', 'fullrank', 'pathfinder', 'laplace')`
#'@param burnin \code{integer} specifying the number of warmup iterations of the Markov chain to run
#'to tune sampling algorithms. Ignored
#'if `algorithm %in% c('meanfield', 'fullrank', 'pathfinder', 'laplace')`
#'@param samples \code{integer} specifying the number of post-warmup iterations of the Markov chain to run for
#'sampling the posterior distribution
#'@param thin Thinning interval for monitors.  Ignored
#'if `algorithm %in% c('meanfield', 'fullrank', 'pathfinder', 'laplace')`
#'@param parallel \code{logical} specifying whether multiple cores should be used for
#'generating MCMC simulations in parallel. If \code{TRUE}, the number of cores to use will be
#'\code{min(c(chains, parallel::detectCores() - 1))}
#'@param threads \code{integer} Experimental option to use multithreading for within-chain
#'parallelisation in \code{Stan}. We recommend its use only if you are experienced with
#'\code{Stan}'s `reduce_sum` function and have a slow running model that cannot be sped
#'up by any other means. Only available when using \code{Cmdstan} as the backend
#'@param priors An optional \code{data.frame} with prior
#'definitions (in JAGS or Stan syntax). if using Stan, this can also be an object of
#'class `brmsprior` (see. \code{\link[brms]{prior}} for details). See [get_mvgam_priors] and
#''Details' for more information on changing default prior distributions
#'@param refit Logical indicating whether this is a refit, called using [update.mvgam]. Users should leave
#'as `FALSE`
#'@param lfo Logical indicating whether this is part of a call to [lfo_cv.mvgam]. Returns a
#'lighter version of the model with no residuals and fewer monitored parameters to speed up
#'post-processing. But other downstream functions will not work properly, so users should always
#'leave this set as `FALSE`
#'@param residuals Logical indicating whether to compute series-level randomized quantile residuals and include
#'them as part of the returned object. Defaults to `TRUE`, but you can set to `FALSE` to save
#'computational time and reduce the size of the returned object (users can always add residuals to
#'an object of class `mvgam` using [add_residuals])
#'@param use_stan Logical. If \code{TRUE}, the model will be compiled and sampled using
#'Hamiltonian Monte Carlo with a call to \code{\link[cmdstanr]{cmdstan_model}} or
#'a call to \code{\link[rstan]{stan}}. Note that
#'there are many more options when using `Stan` vs `JAGS`
#'@param backend Character string naming the package to use as the backend for fitting
#'the Stan model (if `use_stan = TRUE`). Options are "cmdstanr" (the default) or "rstan". Can be set globally
#'for the current R session via the \code{"brms.backend"} option (see \code{\link{options}}). Details on
#'the rstan and cmdstanr packages are available at https://mc-stan.org/rstan/ and
#'https://mc-stan.org/cmdstanr/, respectively
#'@param algorithm Character string naming the estimation approach to use.
#'  Options are \code{"sampling"} for MCMC (the default), \code{"meanfield"} for
#'  variational inference with factorized normal distributions,
#'  \code{"fullrank"} for variational inference with a multivariate normal
#'  distribution, \code{"laplace"} for a Laplace approximation (only available
#'  when using cmdstanr as the backend) or \code{"pathfinder"} for the pathfinder
#'  algorithm (only currently available when using cmdstanr as the backend).
#'  Can be set globally for the current \R session via the
#'  \code{"brms.algorithm"} option (see \code{\link{options}}). Limited testing
#'  suggests that `"meanfield"` performs best out of the non-MCMC approximations for
#'  dynamic GAMs, possibly because of the difficulties estimating covariances among the
#'  many spline parameters and latent trend parameters. But rigorous testing has not
#'  been carried out
#'@param autoformat \code{Logical}. Use the `stanc` parser to automatically format the
#'`Stan` code and check for deprecations. Defaults to `TRUE`
#' @param save_all_pars \code{Logical} flag to indicate if draws from all
#'   variables defined in Stan's \code{parameters} block should be saved
#'   (default is \code{FALSE}).
#'@param max_treedepth positive integer placing a cap on the number of simulation steps evaluated during each iteration when
#'`use_stan == TRUE`. Default is `12`. Increasing this value can sometimes help with exploration of complex
#'posterior geometries, but it is rarely fruitful to go above a `max_treedepth` of `14`
#'@param adapt_delta positive numeric between `0` and `1` defining the target average proposal acceptance probability
#'during Stan's adaptation period, if `use_stan == TRUE`. Default is `0.8`. In general you should not need to change adapt_delta
#'unless you see a warning message about divergent transitions, in which case you can increase adapt_delta from the default
#'to a value closer to `1` (e.g. from `0.95` to `0.99`, or from `0.99` to `0.999`, etc).
#'The step size used by the numerical integrator is a function of `adapt_delta` in that increasing
#'`adapt_delta` will result in a smaller step size and fewer divergences. Increasing `adapt_delta` will
#'typically result in a slower sampler, but it will always lead to a more robust sampler
#'@param jags_path Optional character vector specifying the path to the location of the `JAGS` executable (.exe) to use
#'for modelling if `use_stan == FALSE`. If missing, the path will be recovered from a call to \code{\link[runjags]{findjags}}
#'@param ... Further arguments passed to Stan.
#'For \code{backend = "rstan"} the arguments are passed to
#'\code{\link[rstan]{sampling}} or \code{\link[rstan]{vb}}.
#'For \code{backend = "cmdstanr"} the arguments are passed to the
#'\code{cmdstanr::sample}, \code{cmdstanr::variational},
#'\code{cmdstanr::laplace} or
#'\code{cmdstanr::pathfinder} method
#'@details Dynamic GAMs are useful when we wish to predict future values from time series that show temporal dependence
#'but we do not want to rely on extrapolating from a smooth term (which can sometimes lead to unpredictable and unrealistic behaviours).
#'In addition, smooths can often try to wiggle excessively to capture any autocorrelation that is present in a time series,
#'which exacerbates the problem of forecasting ahead. As GAMs are very naturally viewed through a Bayesian lens, and we often
#'must model time series that show complex distributional features and missing data, parameters for `mvgam` models are estimated
#'in a Bayesian framework using Markov Chain Monte Carlo by default. A general overview is provided
#'in the primary vignettes: `vignette("mvgam_overview")` and `vignette("data_in_mvgam")`.
#'For a full list of available vignettes see `vignette(package = "mvgam")`
#'\cr
#'\cr
#'*Formula syntax*: Details of the formula syntax used by \pkg{mvgam} can be found in
#'\code{\link{mvgam_formulae}}. Note that it is possible to supply an empty formula where
#'there are no predictors or intercepts in the observation model (i.e. `y ~ 0` or `y ~ -1`).
#'In this case, an intercept-only observation model will be set up but the intercept coefficient
#'will be fixed at zero. This can be handy if you wish to fit pure State-Space models where
#'the variation in the dynamic trend controls the average expectation, and/or where intercepts
#'are non-identifiable (as in piecewise trends, see examples below)
#'\cr
#'\cr
#'*Families and link functions*: Details of families supported by \pkg{mvgam} can be found in
#'\code{\link{mvgam_families}}.
#'\cr
#'\cr
#'*Trend models*: Details of latent trend models supported by \pkg{mvgam} can be found in
#'\code{\link{mvgam_trends}}.
#'\cr
#'\cr
#'*Priors*: A \code{\link[mgcv]{jagam}} model file is generated from \code{formula} and
#'modified to include any latent
#'temporal processes. Default priors for intercepts and any scale parameters are generated
#'using the same practice as \pkg{brms}. Prior distributions for most important model parameters can be
#'altered by the user to inspect model
#'sensitivities to given priors (see \code{\link{get_mvgam_priors}} for details).
#'Note that latent trends are estimated on the link scale so choose priors
#'accordingly. However more control over the model specification can be accomplished by first using `mvgam` as a
#'baseline, then editing the returned model accordingly. The model file can be edited and run outside
#'of `mvgam` by setting \code{run_model = FALSE} and this is encouraged for complex
#'modelling tasks. Note, no priors are
#'formally checked to ensure they are in the right syntax for the respective
#'probabilistic modelling framework, so it is
#'up to the user to ensure these are correct (i.e. use `dnorm` for normal
#'densities in `JAGS`, with the mean and precision
#'parameterisation; but use `normal` for normal densities in `Stan`, with the mean
#'and standard deviation parameterisation)
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
#'*Observation level parameters*: When more than one series is included in \code{data} and an
#'observation family that contains more than one parameter is used, additional observation family parameters
#'(i.e. `phi` for `nb()` or `sigma` for `gaussian()`) are
#'estimated independently for each series.
#'\cr
#'\cr
#'*Factor regularisation*: When using a dynamic factor model for the trends with `JAGS` factor precisions are given
#'regularized penalty priors to theoretically allow some factors to be dropped from the model by squeezing increasing
#'factors' variances to zero. This is done to help protect against selecting too many latent factors than are needed to
#'capture dependencies in the data, so it can often be advantageous to set `n_lv` to a slightly larger number. However
#'larger numbers of factors do come with additional computational costs so these should be balanced as well. When using
#'`Stan`, all factors are parameterised with fixed variance parameters
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
#'exponential GP trends are approximated using by default \code{20} basis functions, which saves computational
#'costs compared to fitting full GPs while adequately estimating
#'GP \code{alpha} and \code{rho} parameters. Because of the many advantages of `Stan` over `JAGS`,
#'*further development of the package will only be applied to `Stan`*. This includes the planned addition
#'of more response distributions, plans to handle zero-inflation, and plans to incorporate a greater
#'variety of trend models. Users are strongly encouraged to opt for `Stan` over `JAGS` in any proceeding workflows
#'@author Nicholas J Clark
#'@references Nicholas J Clark & Konstans Wells (2020). Dynamic generalised additive models (DGAMs) for forecasting discrete ecological time series.
#'Methods in Ecology and Evolution. 14:3, 771-784.
#'@seealso \code{\link[mgcv]{jagam}}, \code{\link[mgcv]{gam}}, \code{\link[mgcv]{gam.models}},
#'@return A \code{list} object of class \code{mvgam} containing model output, the text representation of the model file,
#'the mgcv model output (for easily generating simulations at
#'unsampled covariate values), Dunn-Smyth residuals for each series and key information needed
#'for other functions in the package. See \code{\link{mvgam-class}} for details.
#'Use `methods(class = "mvgam")` for an overview on available methods.
#'
#'@examples
#'\dontrun{
#'# Simulate a collection of three time series that have shared seasonal dynamics
# # and independent random walk trends, with a Poisson observation process
#'dat <- sim_mvgam(T = 80, n_series = 3, prop_missing = 0.1,
#'                 prop_trend = 0.6)
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
#' # Inspect the data objects needed to condition the model
#' str(mod1$model_data)
#'
#' # The following code can be used to run the model outside of mvgam; first using rstan
#' model_data <- mod1$model_data
#' library(rstan)
#' fit <- stan(model_code = mod1$model_file,
#'            data = model_data)
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
#'                      refresh = 100)
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
#' # Residual diagnostics
#' plot(mod1, type = 'residuals', series = 1)
#' resids <- residuals(mod1)
#' str(resids)
#'
#' # Compute the forecast using covariate information in data_test
#' fc <- forecast(mod1, newdata = dat$data_test)
#' str(fc)
#' plot(fc)
#'
#' # Plot the estimated seasonal smooth function
#' plot(mod1, type = 'smooths')
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
#' # Plot conditional response predictions using marginaleffects
#' plot(conditional_effects(mod1), ask = FALSE)
#' plot_predictions(mod1, condition = 'season', points = 0.5)
#'
#' # Extract observation model beta coefficient draws as a data.frame
#' beta_draws_df <- as.data.frame(mod1, variable = 'betas')
#' head(beta_draws_df)
#' str(beta_draws_df)
#'
#' # Investigate model fit
#' loo(mod1)
#'
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
#' mod <- mvgam(y ~ s(season, bs = 'cc'),
#'               trend_map = trend_map,
#'               trend_model = 'AR1',
#'               data = mod_data,
#'               return_model_data = TRUE)
#'
#' # The mapping matrix is now supplied as data to the model in the 'Z' element
#' mod1$model_data$Z
#' code(mod)
#'
#' # The first two series share an identical latent trend; the third is different
#' plot(mod, type = 'trend', series = 1)
#' plot(mod, type = 'trend', series = 2)
#' plot(mod, type = 'trend', series = 3)
#'
#' # Example of how to use dynamic coefficients
#' # Simulate a time-varying coefficient for the effect of temperature
#' set.seed(3)
#' N = 200
#' beta_temp <- vector(length = N)
#' beta_temp[1] <- 0.4
#' for(i in 2:N){
#'   beta_temp[i] <- rnorm(1, mean = beta_temp[i - 1], sd = 0.025)
#' }
#'
#' # Simulate a covariate called 'temp'
#' temp <- rnorm(N, sd = 1)
#'
#' # Simulate the Gaussian observation process
#' out <- rnorm(N, mean = 4 + beta_temp * temp,
#'              sd = 0.5)
#'
#' # Gather necessary data into a data.frame; split into training / testing
#' data = data.frame(out, temp, time = seq_along(temp))
#' data_train <- data[1:180,]
#' data_test <- data[181:200,]
#'
#' # Fit the model using the dynamic() formula helper
#' mod <- mvgam(formula = out ~ dynamic(temp, rho = 8),
#'              family = gaussian(),
#'              data = data_train,
#'              newdata = data_test)
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
#'
#' # Example showing how to incorporate an offset; simulate some count data
#' # with different means per series
#' set.seed(100)
#' dat <- sim_mvgam(prop_trend = 0, mu = c(0, 2, 2), seasonality = 'hierarchical')
#'
#' # Add offset terms to the training and testing data
#' dat$data_train$offset <- 0.5 * as.numeric(dat$data_train$series)
#' dat$data_test$offset <- 0.5 * as.numeric(dat$data_test$series)
#'
#' # Fit a model that includes the offset in the linear predictor as well as
#' # hierarchical seasonal smooths
#' mod <- mvgam(formula = y ~ offset(offset) +
#'               s(series, bs = 're') +
#'               s(season, bs = 'cc') +
#'               s(season, by = series, m = 1, k = 5),
#'              data = dat$data_train,
#'              trend_model = 'None',
#'              use_stan = TRUE)
#'
#' # Inspect the model file to see the modification to the linear predictor
#' # (eta)
#' mod$model_file
#'
#' # Forecasts for the first two series will differ in magnitude
#' layout(matrix(1:2, ncol = 2))
#' plot(mod, type = 'forecast', series = 1, newdata = dat$data_test,
#'      ylim = c(0, 75))
#' plot(mod, type = 'forecast', series = 2, newdata = dat$data_test,
#'      ylim = c(0, 75))
#' layout(1)
#'
#' # Changing the offset for the testing data should lead to changes in
#' # the forecast
#' dat$data_test$offset <- dat$data_test$offset - 2
#' plot(mod, 'forecast', newdata = dat$data_test)
#'
#' # Relative Risks can be computed by fixing the offset to the same value
#' # for each series
#' dat$data_test$offset <- rep(1, NROW(dat$data_test))
#' preds_rr <- predict(mod, type = 'link', newdata = dat$data_test)
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
#'
#' # Example of logistic growth with possible changepoints
#' # Simple logistic growth model
#' dNt = function(r, N, k){
#'    r * N * (k - N)
#' }
#'
#' # Iterate growth through time
#' Nt = function(r, N, t, k) {
#' for (i in 1:(t - 1)) {
#'
#'  # population at next time step is current population + growth,
#'  # but we introduce several 'shocks' as changepoints
#'  if(i %in% c(5, 15, 25, 41, 45, 60, 80)){
#'    N[i + 1] <- max(1, N[i] + dNt(r + runif(1, -0.1, 0.1),
#'                                  N[i], k))
#'    } else {
#'    N[i + 1] <- max(1, N[i] + dNt(r, N[i], k))
#'    }
#'   }
#'  N
#' }
#'
#' # Simulate expected values
#' set.seed(11)
#' expected <- Nt(0.004, 2, 100, 30)
#' plot(expected, xlab = 'Time')
#'
#' # Take Poisson draws
#' y <- rpois(100, expected)
#' plot(y, xlab = 'Time')
#'
#' # Assemble data into dataframe and model. We set a
#' # fixed carrying capacity of 35 for this example, but note that
#' # this value is not required to be fixed at each timepoint
#' mod_data <- data.frame(y = y,
#'                        time = 1:100,
#'                        cap = 35,
#'                        series = as.factor('series_1'))
#' plot_mvgam_series(data = mod_data)
#'
#' # The intercept is nonidentifiable when using piecewise
#' # trends because the trend functions have their own offset
#' # parameters 'm'; it is recommended to always drop intercepts
#' # when using these trend models
#' mod <- mvgam(y ~ 0,
#'              trend_model = PW(growth = 'logistic'),
#'              family = poisson(),
#'              data = mod_data)
#' summary(mod)
#'
#' # Plot the posterior hindcast
#' plot(mod, type = 'forecast')
#'
#' # View the changepoints with ggplot2 utilities
#' library(ggplot2)
#' mcmc_plot(mod, variable = 'delta_trend',
#'           regex = TRUE) +
#' scale_y_discrete(labels = mod$trend_model$changepoints) +
#' labs(y = 'Potential changepoint',
#'      x = 'Rate change')
#' }
#'@export

mvgam = function(formula,
                 trend_formula,
                 knots,
                 trend_knots,
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
                 refit = FALSE,
                 lfo = FALSE,
                 residuals = TRUE,
                 use_stan = TRUE,
                 backend = getOption("brms.backend", "cmdstanr"),
                 algorithm = getOption("brms.algorithm", "sampling"),
                 autoformat = TRUE,
                 save_all_pars = FALSE,
                 max_treedepth,
                 adapt_delta,
                 jags_path,
                 ...){

  # Check data arguments
  if(missing("data") & missing("data_train")){
    stop('Argument "data" is missing with no default')
  }
  if(!missing("data")) data_train <- data
  if(!missing("newdata")) data_test <- newdata
  orig_data <- data_train

  # Ensure series and time variables are present
  data_train <- validate_series_time(data_train, name = 'data')

  # Validate the formula to convert any dynamic() terms
  formula <- interpret_mvgam(formula, N = max(data_train$time))

  # Check sampler arguments
  validate_pos_integer(chains)
  validate_pos_integer(threads)
  validate_pos_integer(burnin)
  validate_pos_integer(samples)
  validate_pos_integer(thin)

  # Check for gp terms in the validated formula
  orig_formula <- gp_terms <- NULL
  if(any(grepl('gp(', attr(terms(formula), 'term.labels'), fixed = TRUE))){

    # Check that there are no multidimensional gp terms
    formula <- interpret_mvgam(formula, N = max(data_train$time))
    orig_formula <- formula

    # Keep intercept?
    keep_intercept <- attr(terms(formula), 'intercept') == 1

    # Indices of gp() terms in formula
    gp_terms <- which_are_gp(formula)

    # Extract attributes
    gp_details <- get_gp_attributes(formula)

    # Replace with s() terms so the correct terms are included
    # in the model.frame
    formula <- gp_to_s(formula)
    if(!keep_intercept) formula <- update(formula, . ~ . - 1)
  }

  # Check for missing rhs in formula
  # If there are no terms in the observation formula (i.e. y ~ -1),
  # we will use an intercept-only observation formula and fix
  # the intercept coefficient at zero
  drop_obs_intercept <- FALSE
  if(length(attr(terms(formula), 'term.labels')) == 0 &
     !attr(terms(formula), 'intercept') == 1){
    formula_envir <- attr(formula, '.Environment')
    if(!is.null(attr(terms(formula(formula)), 'offset'))){
      formula <- formula(paste0(rlang::f_lhs(formula),
                                ' ~ ', paste(gsub(' - 1',' + 1',
                                                  rlang::f_text(formula)))))
    } else {
      formula <- formula(paste(rlang::f_lhs(formula), '~ 1'))
    }
    attr(formula, '.Environment') <- formula_envir
    drop_obs_intercept <- TRUE
  }

  if(is.null(orig_formula)) orig_formula <- formula

  # Check for brmspriors
  if(!missing(priors)){
    if(inherits(priors, 'brmsprior') & !lfo){
      priors <- adapt_brms_priors(priors = priors,
                                  formula = orig_formula,
                                  trend_formula = trend_formula,
                                  data = data_train,
                                  family = family,
                                  use_lv = use_lv,
                                  n_lv = n_lv,
                                  trend_model = trend_model,
                                  trend_map = trend_map,
                                  drift = drift,
                                  warnings = TRUE)
    }
  }

  # Ensure series and time variables are present
  data_train <- validate_series_time(data_train, name = 'data')
  if(!missing(data_test)) data_test <- validate_series_time(data_test,
                                                            name = 'newdata')

  # Lighten the final object if this is an lfo run
  if(lfo) return_model_data <- FALSE

  # Validate observation formula
  formula <- interpret_mvgam(formula, N = max(data_train$time))
  data_train <- validate_obs_formula(formula, data = data_train, refit = refit)

  if(!missing(data_test)){
    data_test <- validate_obs_formula(formula, data = data_test, refit = refit)
  }
  if(is.null(attr(terms(formula(formula)), 'offset'))){
    offset <- FALSE
  } else {
    offset <- TRUE
  }

  # Ensure fitting software can be located
  if(!use_stan & run_model) find_jags(jags_path = jags_path)
  if(use_stan & run_model) find_stan()

  # Validate the family argument
  family <- validate_family(family, use_stan = use_stan)
  family_char <- match.arg(arg = family$family, choices = family_char_choices())

  # Validate the trend arguments
  orig_trend_model <- trend_model
  trend_model <- validate_trend_model(orig_trend_model, drift = drift)

  # Check for N-mixture modifications
  add_nmix <- FALSE; nmix_trendmap <- FALSE
  if(family_char == 'nmix'){
    family <- poisson(); family_char <- 'poisson'; add_nmix <- TRUE
    if(missing(trend_formula)){
      stop('Argument "trend_formula" required for nmix models',
           call. = FALSE)
    }
    if(!missing(trend_map)){
      nmix_trendmap <- TRUE
    }
    use_lv <- TRUE
    if(trend_model == 'None'){
      trend_model <- 'RW'
    }
  }

  # Assess whether any additional moving average or correlated errors are needed
  ma_cor_adds <- ma_cor_additions(trend_model)
  trend_model <- ma_cor_adds$trend_model
  use_var1 <- ma_cor_adds$use_var1; use_var1cor <- ma_cor_adds$use_var1cor
  add_ma <- ma_cor_adds$add_ma; add_cor <- ma_cor_adds$add_cor

  if(length(unique(data_train$series)) == 1 & add_cor){
    warning('Correlated process errors not possible with only 1 series',
            call. = FALSE)
    add_cor <- FALSE
  }

  if(trend_model %in% c('PWlinear', 'PWlogistic')){
    if(attr(terms(formula), 'intercept') == 1 & !drop_obs_intercept){
      warning(paste0('It is difficult / impossible to estimate intercepts\n',
                     'and piecewise trend offset parameters. You may want to\n',
                     'consider dropping the intercept from the formula'),
              call. = FALSE)
    }

    if(use_lv) stop('Cannot estimate piecewise trends using dynamic factors',
                    call. = FALSE)
  }

  if(use_lv & (add_ma | add_cor) & missing(trend_formula)){
    stop('Cannot estimate moving averages or correlated errors for dynamic factors',
         call. = FALSE)
  }

  if(drift && use_lv){
    warning('Cannot identify drift terms in latent factor models; setting "drift = FALSE"',
            call. = FALSE)
    drift <- FALSE
  }

  if(use_lv & trend_model == 'VAR1' & missing(trend_formula)){
    stop('Cannot identify dynamic factor models that evolve as VAR processes',
         call. = FALSE)
  }

  # JAGS cannot support latent GP, VAR or piecewise trends
  if(!use_stan & trend_model %in% c('GP', 'VAR1', 'PWlinear', 'PWlogistic')){
    stop('Gaussian Process, VAR and piecewise trends not supported for JAGS',
         call. = FALSE)
  }

  # Stan cannot support Tweedie
  if(use_stan & family_char == 'tweedie'){
    stop('Tweedie family not supported for stan',
         call. = FALSE)
  }

  # Check trend formula
  if(!missing(trend_formula)){

    validate_trend_formula(trend_formula)

    if(missing(trend_map)){
      trend_map <- data.frame(series = unique(data_train$series),
                              trend = 1:length(unique(data_train$series)))
    }

    if(!trend_model %in% c('RW', 'AR1', 'AR2', 'AR3', 'VAR1')){
      stop('only RW, AR1, AR2, AR3 and VAR trends currently supported for trend predictor models',
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

  # Upper bounds no longer supported as they are fairly useless
  upper_bounds <- rlang::missing_arg()

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

  # Some general family-level restrictions can now be checked
  orig_y <- data_train$y
  validate_family_resrictions(response = orig_y, family = family)

  # Replace any NAs in the response for initial setup
  data_train[[terms(formula(formula))[[2]]]] <-
    replace_nas(data_train[[terms(formula(formula))[[2]]]])

  # Compute default priors
  def_priors <- adapt_brms_priors(c(make_default_scales(orig_y,
                                                        family),
                                    make_default_int(orig_y,
                                                     family = if(add_nmix){
                                                       nmix()
                                                     } else {
                                                       family
                                                     })),
                                  formula = orig_formula,
                                  trend_formula = trend_formula,
                                  data = orig_data,
                                  family = family,
                                  use_lv = use_lv,
                                  n_lv = n_lv,
                                  trend_model = trend_model,
                                  trend_map = trend_map,
                                  drift = drift)

  # Initiate the GAM model using mgcv so that the linear predictor matrix can be easily calculated
  # when simulating from the Bayesian model later on;
  ss_gam <- try(mvgam_setup(formula = formula,
                            knots = knots,
                            family = family_to_mgcvfam(family),
                            data = data_train),
                silent = TRUE)
  if(inherits(ss_gam, 'try-error')){
    if(grepl('missing values', ss_gam[1])){
      stop(paste('Missing values found in data predictors:\n',
                 attr(ss_gam, 'condition')),
           call. = FALSE)
    } else {
      stop(paste(ss_gam[1]),
           call. = FALSE)
    }
  }

  # Check the test data for NAs as well using predict.gam
  testdat_pred <- try(predict(ss_gam,
                              newdata = data_test,
                              na.action = na.fail),
                      silent = TRUE)
  if(inherits(testdat_pred, 'try-error')){
    if(grepl('missing values', testdat_pred[1])){
      stop(paste('Missing values found in newdata predictors:\n',
                 attr(testdat_pred, 'condition')),
           call. = FALSE)
    }
  }

  # Make JAGS file and appropriate data structures
  if(length(ss_gam$smooth) == 0){
    smooths_included <- FALSE
    # If no smooth terms are included, jagam will fail; so add a fake one and remove
    # it from the model and data structures later
    data_train$fakery <- rnorm(length(data_train$y))
    form_fake <- update.formula(formula, ~ . + s(fakery, k = 3))
    fakery_names <- names(suppressWarnings(mgcv::gam(form_fake,
                                                     data = data_train,
                                                     family = family_to_mgcvfam(family),
                                                     drop.unused.levels = FALSE,
                                                     control = list(maxit = 1)))$coefficients)
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
  if(length(ss_gam$smooth) == 0) ss_jagam$jags.ini$lambda <- NULL

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

    # Use the initialised GAM's estimates for parametric effects, but widen them
    # substantially to allow for better exploration
    beta_sims <- rmvn(100, coef(ss_gam), ss_gam$Vp)
    ss_jagam$jags.data$p_taus <- apply(as.matrix(beta_sims[,1:n_terms]),
                                       2, function(x) 1 / (sd(x) ^ 2))

    base_model[grep('Parametric effect priors',
                      base_model) + 1] <- paste0('  for (i in 1:',
                                                 n_terms,
                                                 ') { b[i] ~ dnorm(p_coefs[i], p_taus[i]) }')
    base_model[grep('Parametric effect priors',
                    base_model)] <- c('  ## parametric effect priors (regularised for identifiability)')
  }

  # For any random effect smooths, use non-centred parameterisation to avoid degeneracies
  # For monotonic smooths, need to determine which direction to place
  # coefficient constraints
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
                                trend_model = if(trend_model %in%
                                                 c('RW', 'VAR1',
                                                   'PWlinear', 'PWlogistic')){'RW'} else {trend_model},
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
    model_file[grep('eta <- X * b', model_file, fixed = TRUE)] <-
      "eta <- X * b + offset"
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

      X$index..time..index <- rbind(temp_dat_train, temp_dat_test) %>%
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

      if(NCOL(data_train) != NCOL(data_test)){
        stop('"data" and "newdata" have different numbers of columns',
             call. = FALSE)
      }

      X$index..time..index <- dplyr::bind_rows(data_train, data_test) %>%
        dplyr::left_join(dplyr::bind_rows(data_train, data_test) %>%
                           dplyr::select(time) %>%
                           dplyr::distinct() %>%
                           dplyr::arrange(time) %>%
                           dplyr::mutate(time = dplyr::row_number()),
                         by = c('time')) %>%
        dplyr::pull(time)

    # Add a series identifier variable
    X$series <- as.numeric(dplyr::bind_rows(data_train, data_test)$series)

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
      X$index..time..index <- temp_dat %>%
        dplyr::left_join(temp_dat %>%
                           dplyr::select(time) %>%
                           dplyr::distinct() %>%
                           dplyr::arrange(time) %>%
                           dplyr::mutate(time = dplyr::row_number()),
                         by = c('time')) %>%
        dplyr::pull(time)
    } else {
      X$index..time..index <- data_train %>%
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
  X %>% dplyr::arrange(index..time..index, series) -> X

  # Matrix of indices in X that correspond to timepoints for each series
  ytimes <- matrix(NA, nrow = length(unique(X$index..time..index)),
                   ncol = length(unique(X$series)))
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
                                     dplyr::select(-index..time..index, -series, -outcome))
  if(NCOL(ss_jagam$jags.data$X) == 1){
    if(offset){
      model_file[grep('eta <-', model_file, fixed = TRUE)] <- 'eta <- X * b + offset'
    } else{
      model_file[grep('eta <-', model_file, fixed = TRUE)] <- 'eta <- X * b'
    }
  }

  if(!missing(upper_bounds)){
    ss_jagam$jags.data$upper_bound <- upper_bounds
  }

  if(length(ss_gam$sp) == length(ss_jagam$jags.ini$lambda)){
    ss_jagam$jags.data$sp <- ss_gam$sp
  }

  # Machine epsilon for minimum allowable non-zero rate
  if(family_char == 'negative binomial') ss_jagam$jags.data$min_eps <- .Machine$double.eps

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

  if(missing(upper_bounds)) upper_bounds <- NULL

  if(use_lv){
    n_lv <- ss_jagam$jags.data$n_lv
  } else {
    if(missing(trend_map)){
      n_lv <- NULL
    }
  }

  if(missing(data_test)) data_test <- NULL

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
    ss_gam$off <- ss_jagam$pregam$off
    ss_gam$S <- ss_jagam$pregam$S
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
    algorithm <- match.arg(algorithm, c('sampling',
                                        'meanfield',
                                        'fullrank',
                                        'pathfinder',
                                        'laplace'))
    backend <- match.arg(backend, c('rstan',
                                    'cmdstanr'))
    fit_engine <- 'stan'
    use_cmdstan <- ifelse(backend == 'cmdstanr', TRUE, FALSE)

    # Import the base Stan model file
    modification <- add_base_dgam_lines(stan = TRUE, use_lv = use_lv)
    unlink('base_gam_stan.txt')
    cat(modification, file = 'base_gam_stan.txt', sep = '\n', append = T)
    base_stan_model <- trimws(suppressWarnings(readLines('base_gam_stan.txt')))
    unlink('base_gam_stan.txt')

    # Add necessary trend structure
    base_stan_model <- add_trend_lines(model_file = base_stan_model,
                                       stan = TRUE,
                                       trend_model = if(trend_model %in% c('RW', 'VAR1',
                                                                           'PWlinear',
                                                                           'PWlogistic')){'RW'} else {trend_model},
                                       use_lv = use_lv,
                                       drift = drift)

    # Add remaining data, model and parameters blocks to the Stan model file;
    # gather Stan data structure
    stan_objects <- add_stan_data(jags_file = trimws(model_file),
                                  stan_file = base_stan_model,
                                  ss_gam = ss_gam,
                                  use_lv = use_lv,
                                  n_lv = n_lv,
                                  jags_data = ss_jagam$jags.data,
                                  family = family_char,
                                  upper_bounds = upper_bounds)

    if(use_lv || !missing(trend_map)){
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

    # Sensible inits needed for the betas
    inits <- family_inits(family = family_char, trend_model,
                          smooths_included, model_data)

    # Include any GP term updates
    if(!is.null(gp_terms)){
      gp_additions <- make_gp_additions(gp_details = gp_details,
                                        data = data_train,
                                        newdata = data_test,
                                        model_data = stan_objects$model_data,
                                        mgcv_model = ss_gam,
                                        gp_terms = gp_terms)
      stan_objects$model_data <- gp_additions$model_data
      ss_gam <- gp_additions$mgcv_model

      gp_names <- unlist(purrr::map(gp_additions$gp_att_table, 'name'))
      gp_names <- gsub('gp(', 's(', gp_names, fixed = TRUE)
      rhos_change <- list()
      for(i in seq_along(gp_names)){
        rhos_change[[i]] <- grep(gp_names[i], rho_names, fixed = TRUE)
      }
      rho_names[c(unique(unlist(rhos_change)))] <- gsub('s(', 'gp(',
                                                      rho_names[c(unique(unlist(rhos_change)))],
                                                      fixed = TRUE)
    }

    # Vectorise likelihoods
    vectorised <- vectorise_stan_lik(model_file = stan_objects$stan_file,
                                     model_data = stan_objects$model_data,
                                     family = family_char,
                                     threads = threads,
                                     trend_model = trend_model,
                                     offset = offset,
                                     drift = drift)

    # If a VAR model is used, enforce stationarity using methods described by
    # Heaps 2022 (Enforcing stationarity through the prior in vector autoregressions)
    if(use_var1) vectorised$model_file <- stationarise_VAR(vectorised$model_file)

    if(use_var1cor){
      param <- c(param, 'L_Omega')
      vectorised$model_file <- stationarise_VARcor(vectorised$model_file)
    }

    # Add modifications for trend mapping and trend predictors, if
    # supplied
    trend_sp_names <- NA
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

        if(missing(trend_knots)) trend_knots <- missing_arg()

        trend_pred_setup <- add_trend_predictors(
          trend_formula = trend_formula,
          trend_knots = trend_knots,
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
          nmix = add_nmix,
          drift = drift)

        vectorised$model_file <- trend_pred_setup$model_file
        vectorised$model_data <- trend_pred_setup$model_data
        trend_mgcv_model <- trend_pred_setup$trend_mgcv_model

        param <- c(param, 'b_trend', 'trend_mus')

        if(trend_pred_setup$trend_smooths_included){
          param <- c(param, 'rho_trend', 'lambda_trend')
        }

        if(trend_pred_setup$trend_random_included){
          param <- c(param, 'mu_raw_trend', 'sigma_raw_trend')
        }

        trend_sp_names <- trend_pred_setup$trend_sp_names
      } else {
      }

      if(trend_model == 'VAR1'){
        param <- c(param, 'lv_coefs', 'LV')
        use_lv <- TRUE
      }
    }

    # Update default priors
    vectorised$model_file <- suppressWarnings(update_priors(vectorised$model_file,
                                                            def_priors,
                                                            use_stan = TRUE))

    # Drop observation intercept if specified
    if(drop_obs_intercept){
      if(any(grepl('// observation model basis coefficients',
                  vectorised$model_file,
                  fixed = TRUE))){
        vectorised$model_file[grep('// observation model basis coefficients',
                                   vectorised$model_file,
                                   fixed = TRUE) + 1] <-
          paste0(vectorised$model_file[grep('// observation model basis coefficients',
                                            vectorised$model_file,
                                            fixed = TRUE) + 1],
                 '\n', '// (Intercept) fixed at zero\n', "b[1] = 0;")
      } else {
        vectorised$model_file[grep('b[1:num_basis] = b_raw[1:num_basis]',
                                   vectorised$model_file,
                                   fixed = TRUE)] <-
          paste0('b[1:num_basis] = b_raw[1:num_basis];\n',
                 '// (Intercept) fixed at zero\n', "b[1] = 0;")
      }

      vectorised$model_file <- readLines(textConnection(vectorised$model_file), n = -1)
      attr(ss_gam, 'drop_obs_intercept') <- TRUE
    } else {
      attr(ss_gam, 'drop_obs_intercept') <- FALSE
    }

    # Remaining model file updates for any GP terms
    if(!is.null(gp_terms)){
     final_gp_updates <- add_gp_model_file(model_file = vectorised$model_file,
                                           model_data = vectorised$model_data,
                                           mgcv_model = ss_gam,
                                           gp_additions = gp_additions)
     vectorised$model_file <- final_gp_updates$model_file
     vectorised$model_data <- final_gp_updates$model_data
    }

    # Update monitor pars for any GP terms
    if(any(grepl('real<lower=0> alpha_gp',
                vectorised$model_file, fixed = TRUE)) &
       !lfo){
      alpha_params <- trimws(gsub(';', '', gsub('real<lower=0> ',
                                                '',
                                                grep('real<lower=0> alpha_gp',
                                                     vectorised$model_file, fixed = TRUE,
                                                     value = TRUE), fixed = TRUE)))
      rho_params <- trimws(gsub(';', '', gsub('real<lower=0> ',
                                              '',
                                              grep('real<lower=0> rho_gp',
                                                   vectorised$model_file, fixed = TRUE,
                                                   value = TRUE), fixed = TRUE)))
      param <- c(param, alpha_params, rho_params)
    }

    # Update for any monotonic term updates
    if(any(smooth_labs$class %in% c('moi.smooth', 'mod.smooth'))){
      final_mono_updates <- add_mono_model_file(model_file = vectorised$model_file,
                                                model_data = vectorised$model_data,
                                                mgcv_model = ss_gam)
      vectorised$model_file <- final_mono_updates$model_file
      vectorised$model_data <- final_mono_updates$model_data
    }

    # Update for any piecewise trends
    if(trend_model %in% c('PWlinear', 'PWlogistic')){
      pw_additions <- add_piecewise(vectorised$model_file,
                                    vectorised$model_data,
                                    data_train,
                                    data_test,
                                    orig_trend_model,
                                    family)
      vectorised$model_file <- pw_additions$model_file
      vectorised$model_data <- pw_additions$model_data
      orig_trend_model$changepoints <- pw_additions$model_data$t_change
      orig_trend_model$change_freq <- pw_additions$model_data$change_freq
      orig_trend_model$cap <- pw_additions$model_data$cap
    }

    # Add in any user-specified priors
    if(!missing(priors)){
      vectorised$model_file <- update_priors(vectorised$model_file, priors,
                                             use_stan = TRUE)
    } else {
      priors <- NULL
    }

    # Add any correlated error or moving average processes; this comes after
    # priors as currently there is no option to change priors on these parameters
    if(add_ma | add_cor){
      vectorised$model_file <- add_MaCor(vectorised$model_file,
                                         add_ma = add_ma,
                                         add_cor = add_cor,
                                         trend_model = trend_model)
    }

    # Add updates for an N-mixture model
    if(add_nmix){
      nmix_additions <- add_nmixture(vectorised$model_file,
                                     vectorised$model_data,
                                     orig_trend_model = orig_trend_model,
                                     data_train = data_train,
                                     data_test = data_test,
                                     trend_map = trend_map,
                                     nmix_trendmap = nmix_trendmap)
      vectorised$model_file <- nmix_additions$model_file
      vectorised$model_data <- nmix_additions$model_data
      family <- nmix(); family_char <- 'nmix'

      # Nmixtures don't use generated quantities because it is faster
      # to produce these in R after sampling has finished
      param <- c(param, 'p')
      param <- param[!param %in% c('ypred', 'mus', 'theta',
                                   'detprob', 'latent_ypred',
                                   'lv_coefs', 'error')]
    }

    # Tidy the representation
    vectorised$model_file <- sanitise_modelfile(vectorised$model_file)

    if(requireNamespace('cmdstanr', quietly = TRUE)){
      # Replace new syntax if this is an older version of Stan
      if(cmdstanr::cmdstan_version() < "2.26"){
        vectorised$model_file <-
          gsub('array[n, n_series] int ypred;',
               'int ypred[n, n_series];',
               vectorised$model_file, fixed = TRUE)
        vectorised$model_file <-
          gsub('array[n, n_series] real ypred;',
               'real ypred[n, n_series];',
               vectorised$model_file, fixed = TRUE)
      }

      # Auto-format the model file
      if(autoformat){
        if(requireNamespace('cmdstanr') & cmdstanr::cmdstan_version() >= "2.29.0") {
          vectorised$model_file <- .autoformat(vectorised$model_file,
                                               overwrite_file = FALSE)
        }
        vectorised$model_file <- readLines(textConnection(vectorised$model_file),
                                           n = -1)
      }

      } else {

      # Replace new syntax if this is an older version of Stan
      if(rstan::stan_version() < "2.26"){
        vectorised$model_file <-
          gsub('array[n, n_series] int ypred;',
               'int ypred[n, n_series];',
               vectorised$model_file, fixed = TRUE)
      }
      }
    attr(vectorised$model_data, 'trend_model') <- trend_model

  } else {
    # Set up data and model file for JAGS
    attr(ss_jagam$jags.data, 'trend_model') <- trend_model
    trend_sp_names <- NA
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

  # Remove lp__ from monitor params if VB is to be used
  if(algorithm %in% c('meanfield', 'fullrank', 'pathfinder', 'laplace')){
    param <- param[!param %in% 'lp__']
  }

  # Lighten up the mgcv model(s) to reduce size of the returned object
  ss_gam <- trim_mgcv(ss_gam)
  if(!missing(trend_formula)){
    trend_mgcv_model <- trim_mgcv(trend_mgcv_model)
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
                               trend_model = orig_trend_model,
                               trend_map = if(!missing(trend_map)){
                                 trend_map
                               } else {
                                 NULL
                               },
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
                               trend_sp_names = trend_sp_names,
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
                               backend = if(use_stan){
                                 backend
                               } else {
                                 'rjags'
                               },
                               algorithm = if(use_stan){
                                 algorithm
                               } else {
                                 'sampling'
                               },
                               max_treedepth = NULL,
                               adapt_delta = NULL),
                          class = 'mvgam_prefit')

  #### Else if running the model, complete the setup for fitting ####
  } else {

    # If this is a lfo_cv run, trim down parameters to monitor so post-processing
    # is faster
    if(lfo){
      to_remove <- c('trend', 'b_trend', 'b', 'b_raw', 'rho', 'sigma',
                     'alpha_gp', 'rho_gp', 'ar1', 'ar2', 'ar3',
                     'LV', 'lv_coefs', 'penalty', 'Sigma', 'theta',
                     'error')
      param <- param[!param %in% to_remove]
    }

    if(use_stan){

      # Remove data likelihood if this is a prior sampling run
      if(prior_simulation){
        vectorised$model_file <- remove_likelihood(vectorised$model_file)
      }

      model_data <- vectorised$model_data

      # Check if cmdstan is accessible; if not, use rstan
      if(backend == 'cmdstanr'){
        if(!requireNamespace('cmdstanr', quietly = TRUE)){
          warning('cmdstanr library not found. Defaulting to rstan')
          use_cmdstan <- FALSE
        } else {
          use_cmdstan <- TRUE
          if(is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))){
            warning('cmdstanr library found but Cmdstan not found. Defaulting to rstan')
            use_cmdstan <- FALSE
          }
        }
      }

      if(use_cmdstan){
        message('Using cmdstanr as the backend')
        message()
        if(cmdstanr::cmdstan_version() < "2.24.0"){
          warning('Your version of Cmdstan is < 2.24.0; some mvgam models may not work properly!')
        }

        if(algorithm == 'pathfinder'){
          if(cmdstanr::cmdstan_version() < "2.33"){
            stop('Your version of Cmdstan is < 2.33; the "pathfinder" algorithm is not available',
                 call. = FALSE)
          }

          if(utils::packageVersion('cmdstanr') < '0.6.1.9000'){
            stop('Your version of cmdstanr is < 0.6.1.9000; the "pathfinder" algorithm is not available',
                 call. = FALSE)
          }
        }

        # Prepare threading
        if(cmdstanr::cmdstan_version() >= "2.29.0"){
          if(threads > 1){
            cmd_mod <- cmdstanr::cmdstan_model(cmdstanr::write_stan_file(vectorised$model_file),
                                               stanc_options = list('O1'),
                                               cpp_options = list(stan_threads = TRUE))
          } else {
            cmd_mod <- cmdstanr::cmdstan_model(cmdstanr::write_stan_file(vectorised$model_file),
                                               stanc_options = list('O1'))
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
        if(algorithm == 'sampling'){
          if(prior_simulation){
            if(parallel){
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
                                     diagnostics = NULL,
                                     ...)
            } else {
              fit1 <- cmd_mod$sample(data = model_data,
                                     chains = chains,
                                     threads_per_chain = if(threads > 1){ threads } else { NULL },
                                     refresh = 100,
                                     init = inits,
                                     max_treedepth = 12,
                                     adapt_delta = 0.8,
                                     iter_sampling = samples,
                                     iter_warmup = 200,
                                     show_messages = FALSE,
                                     diagnostics = NULL,
                                     ...)
            }

          } else {
            if(parallel){
              fit1 <- cmd_mod$sample(data = model_data,
                                     chains = chains,
                                     parallel_chains = min(c(chains, parallel::detectCores() - 1)),
                                     threads_per_chain = if(threads > 1){ threads } else { NULL },
                                     refresh = 100,
                                     init = inits,
                                     max_treedepth = max_treedepth,
                                     adapt_delta = adapt_delta,
                                     iter_sampling = samples,
                                     iter_warmup = burnin,
                                     ...)
            } else {
              fit1 <- cmd_mod$sample(data = model_data,
                                     chains = chains,
                                     threads_per_chain = if(threads > 1){ threads } else { NULL },
                                     refresh = 100,
                                     init = inits,
                                     max_treedepth = max_treedepth,
                                     adapt_delta = adapt_delta,
                                     iter_sampling = samples,
                                     iter_warmup = burnin,
                                     ...)
            }
          }
        }

        if(algorithm %in% c('meanfield', 'fullrank')){
          param <- param[!param %in% 'lp__']
          fit1 <- cmd_mod$variational(data = model_data,
                                      threads = if(threads > 1){ threads } else { NULL },
                                      refresh = 500,
                                      output_samples = samples,
                                      algorithm = algorithm,
                                      ...)
        }

        if(algorithm %in% c('laplace')){
          param <- param[!param %in% 'lp__']
          fit1 <- cmd_mod$laplace(data = model_data,
                                     threads = if(threads > 1){ threads } else { NULL },
                                     refresh = 500,
                                     draws = samples,
                                     ...)
        }

        if(algorithm %in% c('pathfinder')){
          param <- param[!param %in% 'lp__']
          fit1 <- cmd_mod$pathfinder(data = model_data,
                                      num_threads = if(threads > 1){ threads } else { NULL },
                                      refresh = 500,
                                      draws = samples,
                                      ...)
        }

        # Convert model files to stan_fit class for consistency
        if(save_all_pars){
          out_gam_mod <- read_csv_as_stanfit(fit1$output_files())
        } else {
          out_gam_mod <- read_csv_as_stanfit(fit1$output_files(),
                                             variables = param)
        }

        out_gam_mod <- repair_stanfit(out_gam_mod)

        if(algorithm %in% c('meanfield', 'fullrank',
                            'pathfinder', 'laplace')){
          out_gam_mod@sim$iter <- samples
          out_gam_mod@sim$thin <- 1
          out_gam_mod@stan_args[[1]]$method <- 'sampling'
        }

      } else {
        requireNamespace('rstan', quietly = TRUE)
        message('Using rstan as the backend')
        message()

        if(rstan::stan_version() < "2.24.0"){
          warning('Your version of Stan is < 2.24.0; some mvgam models may not work properly!')
        }

        if(algorithm == 'pathfinder'){
          stop('The "pathfinder" algorithm is not yet available in rstan',
               call. = FALSE)
        }

        if(algorithm == 'laplace'){
          stop('The "laplace" algorithm is not yet available in rstan',
               call. = FALSE)
        }
        options(mc.cores = parallel::detectCores())

        # Fit the model in rstan using custom control parameters
        if(missing(max_treedepth)){
          max_treedepth <- 12
        }

        if(missing(adapt_delta)){
          adapt_delta <- 0.85
        }

        if(threads > 1){
          if(utils::packageVersion("rstan") >= "2.26") {
            threads_per_chain_def <- rstan::rstan_options("threads_per_chain")
            on.exit(rstan::rstan_options(threads_per_chain = threads_per_chain_def))
            rstan::rstan_options(threads_per_chain = threads)
          } else {
            stop("Threading is not supported by backend 'rstan' version ",
                  utils::packageVersion("rstan"), ".",
                 call. = FALSE)
          }
        }

        message("Compiling the Stan program...")
        message()
        stan_mod <- rstan::stan_model(model_code = vectorised$model_file,
                                      verbose = TRUE)
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

        if(algorithm == 'sampling'){
          if(parallel){
            fit1 <- rstan::sampling(stan_mod,
                                    iter = samples,
                                    warmup = burnin,
                                    chains = chains,
                                    data = model_data,
                                    cores = min(c(chains, parallel::detectCores() - 1)),
                                    init = inits,
                                    verbose = FALSE,
                                    thin = thin,
                                    control = stan_control,
                                    pars = NA,
                                    refresh = 100,
                                    save_warmup = FALSE,
                                    ...)
          } else {
            fit1 <- rstan::sampling(stan_mod,
                                    iter = samples,
                                    warmup = burnin,
                                    chains = chains,
                                    data = model_data,
                                    cores = 1,
                                    init = inits,
                                    verbose = FALSE,
                                    thin = thin,
                                    control = stan_control,
                                    pars = NA,
                                    refresh = 100,
                                    save_warmup = FALSE,
                                    ...)
          }
        }

        if(algorithm %in% c('fullrank', 'meanfield')){
          param <- param[!param %in% 'lp__']
          fit1 <- rstan::vb(stan_mod,
                            output_samples = samples,
                            data = model_data,
                            algorithm = algorithm,
                            pars = NA,
                            ...)
        }

        out_gam_mod <- fit1
        out_gam_mod <- repair_stanfit(out_gam_mod)
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

  # Add generated quantities for N-mixture models
  if(family_char == 'nmix'){
    out_gam_mod <- add_nmix_posterior(model_output = out_gam_mod,
                                      obs_data = data_train,
                                      test_data = data_test,
                                      mgcv_model = trend_mgcv_model,
                                      Z = model_data$Z,
                                      n_lv = n_lv,
                                      K_inds = model_data$K_inds_all)
  }

  # Get Dunn-Smyth Residual distributions for each series if this
  # is not a prior simulation or an lfo fit
  if(prior_simulation || lfo || !residuals){
    series_resids <- NULL
  } else {
    object = list(
      model_output = out_gam_mod,
      model_data = if(use_stan){
        vectorised$model_data
      } else {
        ss_jagam$jags.data
      },
      fit_engine = fit_engine,
      family = family_char,
      obs_data = data_train,
      test_data = data_test,
      ytimes = ytimes)
    class(object) <- 'mvgam'
    # Use the much faster vectorized residual
    # calculation function now
    series_resids <- dsresids_vec(object)
  }

  if(prior_simulation){
    data_train$y <- orig_y
  }

  # Add Bayesian coefficients to the mgcv model to help with plotting of
  # smooths that aren't yet supported by mvgam plotting functions
  # Extract median beta params for smooths and their covariances
  # so that uncertainty from mgcv plots is reasonably accurate
  if(run_model & !lfo){
    # Use the empirical covariance matrix from the fitted coefficients
    V <- cov(mcmc_chains(out_gam_mod, 'b'))
    ss_gam$Ve <- ss_gam$Vp <- ss_gam$Vc <- V

    # Add the posterior median coefficients
    p <- mcmc_summary(out_gam_mod, 'b',
                      variational = algorithm %in% c('meanfield',
                                                     'fullrank',
                                                     'pathfinder',
                                                     'laplace'))[,c(4)]
    names(p) <- names(ss_gam$coefficients)
    ss_gam$coefficients <- p

    # Compute estimated degrees of freedom for smooths
    object = list(
      model_output = out_gam_mod,
      model_data = if(use_stan){
        vectorised$model_data
      } else {
        ss_jagam$jags.data
      },
      fit_engine = fit_engine,
      family = family_char,
      obs_data = data_train,
      test_data = data_test,
      trend_model = trend_model,
      mgcv_model = ss_gam,
      ytimes = ytimes)
    class(object) <- 'mvgam'
    ss_gam <- compute_edf(ss_gam, object, 'rho', 'sigma_raw')

    # Repeat for any trend-specific mgcv model
    if(!missing(trend_formula)){
      V <- cov(mcmc_chains(out_gam_mod, 'b_trend'))
      trend_mgcv_model$Ve <- trend_mgcv_model$Vp <- trend_mgcv_model$Vc <- V

      p <- mcmc_summary(out_gam_mod, 'b_trend',
                        variational = algorithm %in% c('meanfield',
                                                       'fullrank',
                                                       'pathfinder',
                                                       'laplace'))[,c(4)]
      names(p) <- names(trend_mgcv_model$coefficients)
      trend_mgcv_model$coefficients <- p

      object$trend_mgcv_model <- trend_mgcv_model
      trend_mgcv_model <- compute_edf(trend_mgcv_model,
                                      object, 'rho_trend', 'sigma_raw_trend')
    }
  }

  if(lfo){
    ss_gam <- NULL
  }

  #### Return the output as class mvgam ####
  trim_data <- list()
  attr(trim_data, 'trend_model') <- trend_model
  output <- structure(list(call = orig_formula,
                           trend_call = if(!missing(trend_formula)){
                             trend_formula
                           } else {
                             NULL
                           },
                           family = family_char,
                           trend_model = orig_trend_model,
                           trend_map = if(!missing(trend_map)){
                             trend_map
                           } else {
                             NULL
                           },
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
                             trim_data
                           },
                           inits = if(return_model_data){
                             inits
                           } else {
                             NULL
                           },
                           monitor_pars = param,
                           sp_names = rho_names,
                           trend_sp_names = trend_sp_names,
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
                           backend = if(use_stan){
                             backend
                           } else {
                             'rjags'
                           },
                           algorithm = if(use_stan){
                             algorithm
                           } else {
                             'sampling'
                           },
                           max_treedepth = if(use_stan & algorithm == 'sampling'){
                             max_treedepth
                           } else {
                             NULL
                           },
                           adapt_delta = if(use_stan & algorithm == 'sampling'){
                             adapt_delta
                           } else {
                             NULL
                           }),
                      class = 'mvgam')

  }

  return(output)
}
