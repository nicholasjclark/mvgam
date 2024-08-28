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
#'latent abundance. Be aware that it can be very challenging to simultaneously estimate intercept parameters
#'for both the observation mode (captured by `formula`) and the process model (captured by `trend_formula`).
#'Users are recommended to drop one of these using the `- 1` convention in the formula right hand side.
#'@param knots An optional \code{list} containing user specified knot values to be used for basis construction.
#'For most bases the user simply supplies the knots to be used, which must match up with the k value supplied
#'(note that the number of knots is not always just `k`). Different terms can use different numbers of knots,
#'unless they share a covariate
#'@param trend_knots As for `knots` above, this is an optional \code{list} of knot values for smooth
#'functions within the `trend_formula`
#'@param data A \code{dataframe} or \code{list} containing the model response variable and covariates
#'required by the GAM \code{formula} and optional \code{trend_formula}. Should include columns:
#'#'\itemize{
#'   \item`series` (a \code{factor} index of the series IDs; the number of levels should be identical
#'   to the number of unique series labels (i.e. `n_series = length(levels(data$series))`))
#'   \item`time` (\code{numeric} or \code{integer} index of the time point for each observation).
#'   For most dynamic trend types available in `mvgam` (see argument `trend_model`), time should be
#'   measured in discrete, regularly spaced intervals (i.e. `c(1, 2, 3, ...)`). However you can
#'   use irregularly spaced intervals if using `trend_model = CAR(1)`, though note that any
#'temporal intervals that are exactly `0` will be adjusted to a very small number
#'(`1e-12`) to prevent sampling errors. See an example of `CAR()` trends in \code{\link{CAR}}
#'   }
#'Should also include any other variables to be included in the linear predictor of \code{formula}
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
#'other stochastic elements that are not currently available in \code{mvgam}.
#'Default is \code{FALSE} to reduce
#'the size of the returned object, unless \code{run_model == FALSE}
#'@param family \code{family} specifying the exponential observation family for the series. Currently supported
#'families are:
#'\itemize{
#'   \item`gaussian()` for real-valued data
#'   \item`betar()` for proportional data on `(0,1)`
#'   \item`lognormal()` for non-negative real-valued data
#'   \item`student_t()` for real-valued data
#'   \item`Gamma()` for non-negative real-valued data
#'   \item`bernoulli()` for binary data
#'   \item`poisson()` for count data
#'   \item`nb()` for overdispersed count data
#'   \item`binomial()` for count data with imperfect detection when the number of trials is known;
#'   note that the `cbind()` function must be used to bind the discrete observations and the discrete number
#'   of trials
#'   \item`beta_binomial()` as for `binomial()` but allows for overdispersion
#'   \item`nmix()` for count data with imperfect detection when the number of trials
#'   is unknown and should be modeled via a State-Space N-Mixture model.
#'   The latent states are Poisson, capturing the 'true' latent
#'   abundance, while the observation process is Binomial to account for
#'   imperfect detection.
#'   See \code{\link{mvgam_families}} for an example of how to use this family}
#'Note that only `nb()` and `poisson()` are available if using `JAGS` as the backend.
#'Default is `poisson()`.
#'See \code{\link{mvgam_families}} for more details
#'@param share_obs_params \code{logical}. If \code{TRUE} and the \code{family}
#'has additional family-specific observation parameters (e.g. variance components in
#'`student_t()` or `gaussian()`, or dispersion parameters in `nb()` or `betar()`),
#'these parameters will be shared across all series. This is handy if you have multiple
#'time series that you believe share some properties, such as being from the same
#'species over different spatial units. Default is \code{FALSE}.
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
#'   \item `'CAR1'` or `CAR(p = 1)`
#'   \item `'VAR1'`  or `VAR()`(only available in \code{Stan})
#'   \item `'PWlogistic`, `'PWlinear'` or `PW()` (only available in \code{Stan})
#'   \item `'GP'` or `GP()` (Gaussian Process with squared exponential kernel;
#'only available in \code{Stan})}
#'
#'For all trend types apart from `GP()`, `CAR()` and `PW()`, moving average and/or correlated
#'process error terms can also be estimated (for example, `RW(cor = TRUE)` will set up a
#'multivariate Random Walk if `n_series > 1`). See [mvgam_trends] for more details
#'@param trend_map Optional `data.frame` specifying which series should depend on which latent
#'trends. Useful for allowing multiple series to depend on the same latent trend process, but with
#'different observation processes. If supplied, a latent factor model is set up by setting
#'`use_lv = TRUE` and using the mapping to set up the shared trends. Needs to have column names
#'`series` and `trend`, with integer values in the `trend` column to state which trend each series
#'should depend on. The `series` column should have a single unique entry for each series in the
#'data (names should perfectly match factor levels of the `series` variable in `data`). Note that
#'if this is supplied, the intercept parameter in the process model will NOT be automatically suppressed.
#'See examples for details
#'@param drift Deprecated. If you wish to estimate drift parameters, include parametric fixed effects
#'of 'time' in your formulae instead.
#'@param noncentred \code{logical} Use the non-centred parameterisation for autoregressive
#'trend models? Setting to `TRUE` will reparameterise the model to avoid possible
#'degeneracies that can show up when estimating the latent dynamic random effects. For some
#'models, this can produce big gains in efficiency, meaning that fewer burnin and sampling
#'iterations are required for posterior exploration. But for other models, where the data
#'are highly informative about the latent dynamic processes, this can actually lead to worse
#'performance. Only available for certain trend models
#'(i.e. `RW()`, `AR()`, or `CAR()`, or for
#'`trend = 'None'` when using a `trend_formula`). Not yet available for moving average or
#'correlated error models
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
#'up by any other means. Only available for some families(`poisson()`, `nb()`, `gaussian()`) and
#'when using \code{Cmdstan} as the backend
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
#'`Stan` code and check for deprecations. Only for development purposes, so leave to `TRUE`
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
#'@param silent Verbosity level between `0` and `2`. If `1` (the default), most of the informational
#'messages of compiler and sampler are suppressed. If `2`, even more messages are suppressed. The
#'actual sampling progress is still printed. Set `refresh = 0` to turn this off as well. If using
#'`backend = "rstan"` you can also set open_progress = FALSE to prevent opening additional
#'progress bars.
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
#'*Trend models*: Details of latent trend dynamic models supported by \pkg{mvgam} can be found in
#'\code{\link{mvgam_trends}}.
#'\cr
#'\cr
#'*Priors*: Default priors for intercepts and any scale parameters are generated
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
#'by default estimated independently for each series. But if you wish for the series to share
#'the same observation parameters, set `share_obs_params = TRUE`
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
#'to estimate nonlinear effects via [Hilbert space approximate Gaussian Processes](https://arxiv.org/abs/2004.11408),
#'the availability of a variety of inference algorithms (i.e. variational inference, laplacian inference etc...) and
#'[capabilities to enforce stationarity for complex Vector Autoregressions](https://www.tandfonline.com/doi/full/10.1080/10618600.2022.2079648).
#'Because of the many advantages of `Stan` over `JAGS`,
#'*further development of the package will only be applied to `Stan`*. This includes the planned addition
#'of more response distributions, plans to handle zero-inflation, and plans to incorporate a greater
#'variety of trend models. Users are strongly encouraged to opt for `Stan` over `JAGS` in any proceeding workflows
#'\cr
#'\cr
#'*How to start?*: The [`mvgam` cheatsheet](https://github.com/nicholasjclark/mvgam/raw/master/misc/mvgam_cheatsheet.pdf) is a
#'good starting place if you are just learning to use the package. It gives an overview of the package's key functions and objects,
#'as well as providing a reasonable workflow that new users can follow. In general it is recommended to
#'\itemize{
#'   \item 1. Check that your time series data are in a suitable long format for `mvgam` modeling (see the [data formatting vignette](https://nicholasjclark.github.io/mvgam/articles/data_in_mvgam.html) for guidance)
#'   \item 2. Inspect features of the data using \code{\link{plot_mvgam_series}}. Now is also a good time to familiarise yourself
#'   with the package's example workflows that are detailed in the vignettes. In particular,
#'   the [getting started vignette](https://nicholasjclark.github.io/mvgam/articles/shared_states.html),
#'   the [shared latent states vignette](https://nicholasjclark.github.io/mvgam/articles/shared_states.html),
#'   the [time-varying effects vignette](https://nicholasjclark.github.io/mvgam/articles/time_varying_effects.html) and
#'   the [State-Space models vignette](https://nicholasjclark.github.io/mvgam/articles/trend_formulas.html) all provide
#'   detailed information about how to structure, fit and interrogate Dynamic Generalized Additive Models in `mvgam`. Some
#'   more specialized how-to articles include
#'   ["Incorporating time-varying seasonality in forecast models"](https://ecogambler.netlify.app/blog/time-varying-seasonality/)
#'   and ["Temporal autocorrelation in GAMs and the `mvgam` package"](https://ecogambler.netlify.app/blog/autocorrelated-gams/)
#'   \item 3. Carefully think about how to structure linear predictor effects (i.e. smooth terms using \code{\link[mgcv]{s}},
#'   \code{\link[mgcv]{te}} or \code{\link[mgcv]{ti}}, GPs using \code{\link[brms]{gp}}, dynamic time-varying effects using \code{\link{dynamic}}, and parametric terms), latent temporal trend components (see \code{\link{mvgam_trends}}) and the appropriate
#'   observation family (see \code{\link{mvgam_families}}). Use \code{\link{get_mvgam_priors}} to see default prior distributions
#'   for stochastic parameters
#'   \item 4. Change default priors using appropriate prior knowledge (see \code{\link[brms]{prior}})
#'   \item 5. Fit the model using either Hamiltonian Monte Carlo or an approximation algorithm (i.e. change the `backend` argument)
#'   and use \code{\link{summary.mvgam}}, \code{\link{conditional_effects.mvgam}}, \code{\link{mcmc_plot.mvgam}}, \code{\link{pp_check.mvgam}} and
#'   \code{\link{plot.mvgam}} to inspect / interrogate the model
#'   \item 6. Update the model as needed and use \code{\link{loo_compare.mvgam}} for in-sample model comparisons, or alternatively
#'   use \code{\link{forecast.mvgam}} and \code{\link{score.mvgam_forecast}} to compare models based on out-of-sample forecasts (see the [forecast evaluation vignette](https://nicholasjclark.github.io/mvgam/articles/forecast_evaluation.html) for guidance)
#'   \item 7. When satisfied with the model structure, use \code{\link{predict.mvgam}},
#'   \code{\link[marginaleffects]{plot_predictions}} and/or \code{\link[marginaleffects]{plot_slopes}} for
#'   more targeted inferences (see ["How to interpret and report nonlinear effects from Generalized Additive Models"](https://ecogambler.netlify.app/blog/interpreting-gams/) for some guidance on interpreting GAMs)
#'   }
#'@author Nicholas J Clark
#'@references Nicholas J Clark & Konstans Wells (2020). Dynamic generalised additive models (DGAMs) for forecasting discrete ecological time series.
#'Methods in Ecology and Evolution. 14:3, 771-784.
#'@seealso \code{\link[mgcv]{jagam}}, \code{\link[mgcv]{gam}}, \code{\link[mgcv]{gam.models}},
#'\code{\link{get_mvgam_priors}}
#'@return A \code{list} object of class \code{mvgam} containing model output, the text representation of the model file,
#'the mgcv model output (for easily generating simulations at
#'unsampled covariate values), Dunn-Smyth residuals for each series and key information needed
#'for other functions in the package. See \code{\link{mvgam-class}} for details.
#'Use `methods(class = "mvgam")` for an overview on available methods.
#'
#'@examples
#'\donttest{
#'# Simulate a collection of three time series that have shared seasonal dynamics
#'# and independent AR1 trends, with a Poisson observation process
#'set.seed(0)
#'dat <- sim_mvgam(T = 80,
#'                 n_series = 3,
#'                 mu = 2,
#'                 trend_model = AR(p = 1),
#'                 prop_missing = 0.1,
#'                 prop_trend = 0.6)
#'
#'# Plot key summary statistics for a single series
#'plot_mvgam_series(data = dat$data_train, series = 1)
#'
#'# Plot all series together
#'plot_mvgam_series(data = dat$data_train, series = 'all')
#'
#'# Formulate a model using Stan where series share a cyclic smooth for
#'# seasonality and each series has an independent AR1 temporal process.
#'# Note that 'noncentred = TRUE' will likely give performance gains.
#'# Set run_model = FALSE to inspect the returned objects
#'mod1 <- mvgam(formula = y ~ s(season, bs = 'cc', k = 6),
#'              data = dat$data_train,
#'              trend_model = AR(),
#'              family = poisson(),
#'              noncentred = TRUE,
#'              use_stan = TRUE,
#'              run_model = FALSE)
#'
#' # View the model code in Stan language
#' stancode(mod1)
#'
#' # View the data objects needed to fit the model in Stan
#' sdata1 <- standata(mod1)
#' str(sdata1)
#'
#' # Now fit the model
#' mod1 <- mvgam(formula = y ~ s(season, bs = 'cc', k = 6),
#'               data = dat$data_train,
#'               trend_model = AR(),
#'               family = poisson(),
#'               noncentred = TRUE,
#'               chains = 2)
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
#' library(marginaleffects)
#' conditional_effects(mod1)
#' plot_predictions(mod1, condition = 'season', points = 0.5)
#'
#' # Generate posterior predictive checks using bayesplot
#' pp_check(mod1)
#'
#' # Extract observation model beta coefficient draws as a data.frame
#' beta_draws_df <- as.data.frame(mod1, variable = 'betas')
#' head(beta_draws_df)
#' str(beta_draws_df)
#'
#' # Investigate model fit
#' mc.cores.def <- getOption('mc.cores')
#' options(mc.cores = 1)
#' loo(mod1)
#' options(mc.cores = mc.cores.def)
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
#'                        trend = c(1, 1, 2))
#'
#' # Fit the model using AR1 trends
#' mod <- mvgam(y ~ s(season, bs = 'cc', k = 6),
#'              trend_map = trend_map,
#'              trend_model = AR(),
#'              data = mod_data,
#'              return_model_data = TRUE,
#'              chains = 2)
#'
#' # The mapping matrix is now supplied as data to the model in the 'Z' element
#' mod$model_data$Z
#' code(mod)
#'
#' # The first two series share an identical latent trend; the third is different
#' plot(mod, type = 'trend', series = 1)
#' plot(mod, type = 'trend', series = 2)
#' plot(mod, type = 'trend', series = 3)
#'
#'
#' # Example of how to use dynamic coefficients
#' # Simulate a time-varying coefficient for the effect of temperature
#' set.seed(123)
#' N <- 200
#' beta_temp <- vector(length = N)
#' beta_temp[1] <- 0.4
#' for(i in 2:N){
#'  beta_temp[i] <- rnorm(1, mean = beta_temp[i - 1] - 0.0025, sd = 0.05)
#' }
#' plot(beta_temp)
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
#' mod <- mvgam(out ~
#'               dynamic(temp,
#'                       scale = FALSE,
#'                       k = 40),
#'              family = gaussian(),
#'              data = data_train,
#'              newdata = data_test,
#'              chains = 2)
#'
#' # Inspect the model summary, forecast and time-varying coefficient distribution
#' summary(mod)
#' plot(mod, type = 'smooths')
#' fc <- forecast(mod, newdata = data_test)
#' plot(fc)
#'
#' # Propagating the smooth term shows how the coefficient is expected to evolve
#' plot_mvgam_smooth(mod, smooth = 1, newdata = data)
#' abline(v = 180, lty = 'dashed', lwd = 2)
#' points(beta_temp, pch = 16)
#'
#'
#' # Example showing how to incorporate an offset; simulate some count data
#' # with different means per series
#' set.seed(100)
#' dat <- sim_mvgam(prop_trend = 0, mu = c(0, 2, 2),
#'                  seasonality = 'hierarchical')
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
#'              chains = 2)
#'
#' # Inspect the model file to see the modification to the linear predictor
#' # (eta)
#' code(mod)
#'
#' # Forecasts for the first two series will differ in magnitude
#' fc <- forecast(mod, newdata = dat$data_test)
#' layout(matrix(1:2, ncol = 2))
#' plot(fc, series = 1, ylim = c(0, 75))
#' plot(fc, series = 2, ylim = c(0, 75))
#' layout(1)
#'
#' # Changing the offset for the testing data should lead to changes in
#' # the forecast
#' dat$data_test$offset <- dat$data_test$offset - 2
#' fc <- forecast(mod, newdata = dat$data_test)
#' plot(fc)
#'
#' # Relative Risks can be computed by fixing the offset to the same value
#' # for each series
#' dat$data_test$offset <- rep(1, NROW(dat$data_test))
#' preds_rr <- predict(mod, type = 'link', newdata = dat$data_test,
#'                     summary = FALSE)
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
#' plot_mvgam_series(data = dat, series = 'all')
#'
#' # Fit a model using the binomial() family; must specify observations
#' # and number of trials in the cbind() wrapper
#' mod <- mvgam(cbind(y, ntrials) ~ series + s(x, by = series),
#'              family = binomial(),
#'              data = dat,
#'              chains = 2)
#' summary(mod)
#' pp_check(mod, type = "bars_grouped",
#'          group = "series", ndraws = 50)
#' pp_check(mod, type = "ecdf_overlay_grouped",
#'          group = "series", ndraws = 50)
#' conditional_effects(mod, type = 'link')
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
                 share_obs_params = FALSE,
                 use_lv = FALSE,
                 n_lv,
                 trend_map,
                 trend_model = 'None',
                 drift = FALSE,
                 noncentred = FALSE,
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
                 max_treedepth = 12,
                 adapt_delta = 0.85,
                 silent = 1,
                 jags_path,
                 ...){

  # Check data arguments
  if(missing("data") & missing("data_train")){
    stop('Argument "data" is missing with no default')
  }
  if(!missing("data")) data_train <- data
  if(!missing("newdata")) data_test <- newdata
  orig_data <- data_train

  # Validate trend_model
  if(drift &  silent < 2L)
    message('The "drift" argument is deprecated; use fixed effects of "time" instead')
  drift <- FALSE
  orig_trend_model <- trend_model
  trend_model <- validate_trend_model(orig_trend_model,
                                      drift = drift,
                                      noncentred = noncentred)

  # Ensure series and time variables are present
  data_train <- validate_series_time(data_train, name = 'data',
                                     trend_model = trend_model)

  # Validate the formula to convert any dynamic() terms
  formula <- interpret_mvgam(formula,
                             N = max(data_train$index..time..index),
                             family = family)

  # Check sampler arguments
  validate_pos_integer(chains)
  validate_pos_integer(threads)
  validate_pos_integer(burnin)
  validate_pos_integer(samples)
  validate_pos_integer(thin)
  validate_silent(silent)

  # Upper bounds no longer supported as they are fairly useless
  upper_bounds <- rlang::missing_arg()

  # Check for gp terms in the validated formula
  list2env(check_gp_terms(formula, data_train, family = family),
           envir = environment())

  # Check for missing rhs in formula
  list2env(check_obs_intercept(formula, orig_formula),
           envir = environment())

  # Check for brmspriors
  if(!missing(priors)){
    if(inherits(priors, 'brmsprior') & !lfo & use_stan){
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
                                  warnings = TRUE,
                                  knots = knots)
    }
  }

  # Ensure series and time variables are present
  data_train <- validate_series_time(data_train, name = 'data',
                                     trend_model = trend_model)
  if(!missing(data_test)){
    data_test <- validate_series_time(data_test,
                                      name = 'newdata',
                                      trend_model = trend_model)
    if(trend_model == 'CAR1'){
      data_test$index..time..index <- data_test$index..time..index +
        max(data_train$index..time..index)
    }
  }

  # Lighten the final object if this is an lfo run
  if(lfo) return_model_data <- FALSE

  # Validate observation formula
  formula <- interpret_mvgam(formula, N = max(data_train$index..time..index))
  data_train <- validate_obs_formula(formula,
                                     data = data_train,
                                     refit = refit)

  if(!missing(data_test)){
    data_test <- validate_obs_formula(formula,
                                      data = data_test,
                                      refit = refit)
  }
  if(is.null(attr(terms(formula(formula)), 'offset'))){
    offset <- FALSE
  } else {
    offset <- TRUE
  }

  # Ensure fitting software can be located
  if(!use_stan & run_model) find_jags(jags_path = jags_path)
  if(use_stan & run_model) find_stan()

  # Validate the family and threads arguments
  family <- validate_family(family, use_stan = use_stan)
  family_char <- match.arg(arg = family$family,
                           choices = family_char_choices())
  threads <- validate_threads(family_char, threads)

  # Nmixture additions?
  list2env(check_nmix(family, family_char,
                      trend_formula, trend_model,
                      trend_map, data_train), envir = environment())

  # Validate remaining trend arguments
  trend_val <- validate_trend_restrictions(trend_model = trend_model,
                                           formula = formula,
                                           trend_formula = trend_formula,
                                           trend_map = trend_map,
                                           drift = drift,
                                           drop_obs_intercept = drop_obs_intercept,
                                           use_lv = use_lv,
                                           n_lv = n_lv,
                                           data_train = data_train,
                                           use_stan = use_stan)
  list2env(trend_val, envir = environment())
  if(is.null(trend_map)) trend_map <- rlang::missing_arg()
  if(is.null(n_lv)) n_lv <- rlang::missing_arg()

  # Some general family-level restrictions can now be checked
  orig_y <- data_train$y
  if(any(!is.na(orig_y))){
    validate_family_restrictions(response = orig_y, family = family)
  }

  # Fill in missing observations in data_train so the size of the dataset is correct when
  # building the initial JAGS model
  resp_terms <- as.character(terms(formula(formula))[[2]])
  if(length(resp_terms) == 1){
    out_name <- as.character(terms(formula(formula))[[2]])
  } else {
    if(any(grepl('cbind', resp_terms))){
      resp_terms <- resp_terms[-grepl('cbind', resp_terms)]
      out_name <- resp_terms[1]
    }
  }
  data_train[[out_name]] <- replace_nas(data_train[[out_name]])

  # Compute default priors
  if(use_stan){
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
                                    drift = drift,
                                    knots = knots)
  }

  # Initiate the GAM model using mgcv so that the linear predictor matrix can be easily calculated
  # when simulating from the Bayesian model later on;
  ss_gam <- try(mvgam_setup(formula = formula,
                            knots = knots,
                            family = family_to_mgcvfam(family),
                            dat = data_train),
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
  list2env(jagam_setup(ss_gam = ss_gam,
                       formula = formula,
                       data_train = data_train,
                       family = family,
                       family_char = family_char,
                       knots = knots), envir = environment())

  # Update initial values of lambdas using the full estimates from the
  # fitted gam model to speed convergence; remove initial betas so that the
  # chains can start in very different regions of the parameter space
  ss_jagam$jags.ini$b <- NULL
  if(length(ss_gam$sp) == length(ss_jagam$jags.ini$lambda)){
    ss_jagam$jags.ini$lambda <- ss_gam$sp
    ss_jagam$jags.ini$lambda[log(ss_jagam$jags.ini$lambda) > 10] <- exp(10)
  }
  if(length(ss_gam$smooth) == 0) ss_jagam$jags.ini$lambda <- NULL

  # Fill y with NAs if this is a simulation from the priors;
  # otherwise replace with the original supplied values
  data_train <- check_priorsim(prior_simulation,
                               data_train, orig_y,
                               formula)

  # Read in the base (unmodified) jags model file
  base_model <- suppressWarnings(readLines(file_name))

  # Remove lines from the linear predictor section
  lines_remove <- c(1:grep('## response', base_model))
  base_model <- base_model[-lines_remove]

  if(any(grepl('scale <- 1/tau', base_model, fixed = TRUE))){
    base_model <- base_model[-grep('scale <- 1/tau',
                                   base_model, fixed = TRUE)]
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
    data.frame(label = ss_gam$smooth[[x]]$label,
               class = class(ss_gam$smooth[[x]])[1],
               id = ifelse(is.null(ss_gam$smooth[[x]]$id),
                           NA, ss_gam$smooth[[x]]$id))
  }))

  # Check for 'id' arguments, which are not yet supported
  if(any(!is.na(smooth_labs$id))){
    stop('smooth terms with the "id" argument not yet supported by mvgam',
         call. = FALSE)
  }

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
      testdat <- data.frame(time = data_test$index..time..index)

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
      temp_dat_train <- data.frame(index..time..index = data_train$index..time..index,
                                   series = data_train$series)
      temp_dat_test <- data.frame(index..time..index = data_test$index..time..index,
                                  series = data_test$series)

      X$index..time..index <- rbind(temp_dat_train, temp_dat_test) %>%
        dplyr::left_join(rbind(temp_dat_train, temp_dat_test) %>%
                           dplyr::select(index..time..index) %>%
                           dplyr::distinct() %>%
                           dplyr::arrange(index..time..index) %>%
                           dplyr::mutate(index..time..index = dplyr::row_number()),
                         by = c('index..time..index')) %>%
        dplyr::pull(index..time..index)

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
                           dplyr::select(index..time..index) %>%
                           dplyr::distinct() %>%
                           dplyr::arrange(index..time..index) %>%
                           dplyr::mutate(index..time..index = dplyr::row_number()),
                         by = c('index..time..index')) %>%
        dplyr::pull(index..time..index)

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

    if(inherits(data_train, 'list')){
      temp_dat <- data.frame(index..time..index = data_train$index..time..index)
      X$index..time..index <- temp_dat %>%
        dplyr::left_join(temp_dat %>%
                           dplyr::select(index..time..index) %>%
                           dplyr::distinct() %>%
                           dplyr::arrange(index..time..index) %>%
                           dplyr::mutate(index..time..index = dplyr::row_number()),
                         by = c('index..time..index')) %>%
        dplyr::pull(index..time..index)
    } else {
      X$index..time..index <- data_train %>%
        dplyr::left_join(data_train %>%
                           dplyr::select(index..time..index) %>%
                           dplyr::distinct() %>%
                           dplyr::arrange(index..time..index) %>%
                           dplyr::mutate(index..time..index = dplyr::row_number()),
                         by = c('index..time..index')) %>%
        dplyr::pull(index..time..index)
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

  # Other necessary variables
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
  if(family_char == 'negative binomial')
    ss_jagam$jags.data$min_eps <- .Machine$double.eps

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
    if(any(grepl('S.*', names(ss_jagam$jags.data)))){
      ss_jagam$jags.data[[grep('S.*', names(ss_jagam$jags.data))]] <- NULL
      ss_jagam$jags.data$sp <- NULL
    }
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

    cmdstan_avail <- insight::check_if_installed('cmdstanr',
                                                 stop = FALSE,
                                                 quietly = TRUE)

    if(isTRUE(cmdstan_avail)){
      if(is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))){
        backend <- 'rstan'
      }
    }
    fit_engine <- 'stan'
    use_cmdstan <- ifelse(backend == 'cmdstanr', TRUE, FALSE)

    # Import the base Stan model file
    modification <- add_base_dgam_lines(stan = TRUE, use_lv = use_lv)
    unlink('base_gam_stan.txt')

    stanfile_name <- tempfile(pattern = 'base_gam_stan', fileext = '.txt')
    cat(modification, file = stanfile_name, sep = '\n', append = T)
    base_stan_model <- trimws(suppressWarnings(readLines(stanfile_name)))
    unlink(stanfile_name)

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
                                  family = ifelse(family_char %in% c('binomial',
                                                                     'bernoulli',
                                                                     'beta_binomial'),
                                                  'poisson',
                                                  family_char),
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

    # Don't place inits when using Stan; may add options later for users
    # to supply them though
    inits <- NULL

    # Include any GP term updates
    if(!is.null(gp_terms)){
      gp_additions <- make_gp_additions(gp_details = gp_details,
                                        data = data_train,
                                        newdata = data_test,
                                        model_data = stan_objects$model_data,
                                        mgcv_model = ss_gam,
                                        gp_terms = gp_terms,
                                        family = family)
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
                                     use_lv = use_lv,
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

      if(trend_model %in% c('None', 'RW', 'AR1', 'AR2', 'AR3', 'CAR1')){
        param <- unique(c(param, 'trend', 'sigma'))
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
          drop_trend_int = FALSE,
          drift = drift)

        vectorised$model_file <- trend_pred_setup$model_file
        vectorised$model_data <- trend_pred_setup$model_data
        trend_mgcv_model <- trend_pred_setup$trend_mgcv_model

        param <- unique(c(param, 'trend', 'b_trend', 'trend_mus'))

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

    # Update for CAR1 trends
    if(trend_model == 'CAR1'){
      vectorised$model_data <- add_corcar(vectorised$model_data,
                                          data_train, data_test)
    }

    # Updates for Binomial and Bernoulli families
    if(family_char %in% c('binomial', 'bernoulli', 'beta_binomial')){
      bin_additions <- add_binomial(formula,
                                    vectorised$model_file,
                                    vectorised$model_data,
                                    data_train,
                                    data_test,
                                    family_char)
      vectorised$model_file <- bin_additions$model_file
      vectorised$model_data <- bin_additions$model_data
      attr(ss_gam, 'trials') <- bin_additions$trials
    }

    # Add in any user-specified priors
    if(!missing(priors)){
      vectorised$model_file <- update_priors(vectorised$model_file,
                                             priors,
                                             use_stan = TRUE)
    } else {
      priors <- NULL
    }

    # Check if non-centering can be used
    nc_check <- check_noncent(model_file = vectorised$model_file,
                              noncentred = noncentred,
                              use_lv = use_lv,
                              trend_map = trend_map,
                              add_ma = add_ma,
                              add_cor = add_cor,
                              trend_model = trend_model,
                              drift = drift,
                              silent = silent)
    vectorised$model_file <- nc_check$model_file; noncentred <- nc_check$noncentred

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

    # Updates for sharing of observation params
    if(share_obs_params){
      vectorised$model_file <- shared_obs_params(vectorised$model_file,
                                                 family_char)
    }

    # Tidy the representation
    vectorised$model_file <- sanitise_modelfile(vectorised$model_file)

    if(requireNamespace('cmdstanr', quietly = TRUE) & backend == 'cmdstanr'){
      # Replace new syntax if this is an older version of Stan
      if(cmdstanr::cmdstan_version() < "2.26"){
        warning('Your version of CmdStan is out of date. Some features of mvgam may not work')
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
        if(requireNamespace('cmdstanr') &
           cmdstanr::cmdstan_version() >= "2.29.0") {
          vectorised$model_file <- .autoformat(vectorised$model_file,
                                               overwrite_file = FALSE,
                                               backend = 'cmdstanr',
                                               silent = silent >= 1L)
        }
        vectorised$model_file <- readLines(textConnection(vectorised$model_file),
                                           n = -1)
      }

      } else {
        if(autoformat){
          vectorised$model_file <- .autoformat(vectorised$model_file,
                                               overwrite_file = FALSE,
                                               backend = 'rstan',
                                               silent = silent >= 1L)
          vectorised$model_file <- readLines(textConnection(vectorised$model_file),
                                             n = -1)
        }

      # Replace new syntax if this is an older version of Stan
      if(rstan::stan_version() < "2.26"){
        warning('Your version of rstan is out of date. Some features of mvgam may not work')
        vectorised$model_file <-
          gsub('array[n, n_series] int ypred;',
               'int ypred[n, n_series];',
               vectorised$model_file, fixed = TRUE)
      }
      }
    attr(vectorised$model_data, 'trend_model') <- trend_model

    # Remove data likelihood if this is a prior sampling run
    if(prior_simulation){
      vectorised$model_file <- remove_likelihood(vectorised$model_file)
    }

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
                               share_obs_params = share_obs_params,
                               trend_model = orig_trend_model,
                               trend_map = if(!missing(trend_map)){
                                 trend_map
                               } else {
                                 NULL
                               },
                               drift = FALSE,
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
        # Prepare threading and generate the model
        cmd_mod <- .model_cmdstanr(vectorised$model_file,
                                   threads = threads,
                                   silent = silent)

        # Condition the model using Cmdstan
        out_gam_mod <- .sample_model_cmdstanr(model = cmd_mod,
                                              algorithm = algorithm,
                                              prior_simulation = prior_simulation,
                                              data = model_data,
                                              chains = chains,
                                              parallel = parallel,
                                              silent = silent,
                                              max_treedepth = max_treedepth,
                                              adapt_delta = adapt_delta,
                                              threads = threads,
                                              burnin = burnin,
                                              samples = samples,
                                              param = param,
                                              save_all_pars = save_all_pars,
                                              ...)

      } else {
        # Condition the model using rstan
        requireNamespace('rstan', quietly = TRUE)
        out_gam_mod <- .sample_model_rstan(model = vectorised$model_file,
                                           algorithm = algorithm,
                                           prior_simulation = prior_simulation,
                                           data = model_data,
                                           chains = chains,
                                           parallel = parallel,
                                           silent = silent,
                                           max_treedepth = max_treedepth,
                                           adapt_delta = adapt_delta,
                                           threads = threads,
                                           burnin = burnin,
                                           samples = samples,
                                           thin = thin,
                                           ...)
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

  unlink(file_name)
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
      call = orig_formula,
      mgcv_model = ss_gam,
      model_data = if(use_stan){
        vectorised$model_data
      } else {
        ss_jagam$jags.data
      },
      fit_engine = fit_engine,
      family = family_char,
      share_obs_params = share_obs_params,
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
  # smooths that aren't yet supported by mvgam plotting functions; this is
  # also necessary for computing EDFs and approximate p-values of smooths
  if(!lfo){
    V <- cov(mcmc_chains(out_gam_mod, 'b'))
    ss_gam$Vp <- ss_gam$Vc <- V

    # Add the posterior median coefficients
    p <- mcmc_summary(out_gam_mod, 'b',
                      variational = algorithm %in% c('meanfield',
                                                     'fullrank',
                                                     'pathfinder',
                                                     'laplace'))[,c(4)]
    names(p) <- names(ss_gam$coefficients)
    ss_gam$coefficients <- p

    # Repeat for any trend-specific mgcv model
    if(!missing(trend_formula)){
      V <- cov(mcmc_chains(out_gam_mod, 'b_trend'))
      trend_mgcv_model$Vp <- trend_mgcv_model$Vc <- V
      p <- mcmc_summary(out_gam_mod, 'b_trend',
                        variational = algorithm %in% c('meanfield',
                                                       'fullrank',
                                                       'pathfinder',
                                                       'laplace'))[,c(4)]
      names(p) <- names(trend_mgcv_model$coefficients)
      trend_mgcv_model$coefficients <- p
    }
  }

  #### Return the output as class mvgam ####
  trim_data <- list()
  attr(model_data, 'trend_model') <- trend_model
  attr(trim_data, 'trend_model') <- trend_model
  attr(model_data, 'noncentred') <- if(noncentred) TRUE else NULL
  attr(trim_data, 'noncentred') <- if(noncentred) TRUE else NULL
  attr(model_data, 'threads') <- threads
  attr(trim_data, 'threads') <- threads

  output <- structure(list(call = orig_formula,
                           trend_call = if(!missing(trend_formula)){
                             trend_formula
                           } else {
                             NULL
                           },
                           family = family_char,
                           share_obs_params = share_obs_params,
                           trend_model = orig_trend_model,
                           trend_map = if(!missing(trend_map)){
                             trend_map
                           } else {
                             NULL
                           },
                           drift = FALSE,
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
