# mvgam 2.0.0

mvgam now uses 'brms' to build its models. The observation model takes the full 'brms' formula syntax and a separate `trend_formula` specifies the latent process. Most 1.1.x calls need rewriting: see `vignette("mvgam_overview")` and the package website for worked examples.

## Breaking changes
* Trend constructors now go inside `trend_formula`, e.g. `trend_formula = ~ AR(p = 1)`. The `trend_model` argument has been removed
* `n_lv` moved onto the trend constructors. Removed arguments: `use_lv`, `noncentred`, `prior_simulation`, `return_model_data`, `save_all_pars`, `parallel`
* Data for a model with a trend need a factor series column, even for one series
* Renamed families: `nb()` is now `negbinomial()`, `student_t()` is now `student()`, `betar()` is now `Beta()`
* Default family is now `gaussian()` in `mvgam()` and `sim_mvgam()`. `jsdgam()` defaults to `binomial()`
* `sim_mvgam()` has a new signature built around named scenarios
* Removed `GP()`, `dynamic()`, `s(bs = "moi")`, `s(bs = "mod")`. Use `gp()` and `mo()`
* `incl_autocor` now chooses the prediction surface, replacing `trend_state`. `hindcast(resample_innovations = )` is now `process_error`
* Renamed arguments: `residual_cor(by_group = )`, `score(quantile_level = )`, `forecast(coef_uncertainty = )`, `posterior_transition_matrix(groups = )`
* `score(score = "drps")` no longer takes `log = TRUE`. Use `"crps"` on the log scale
* Posterior summaries by default from `irf()`, `fevd()`, `stability()`. Pass `summary = FALSE` for draws
* Removed functions, with their replacements: `get_mvgam_priors()` (`get_prior()`), the `plot_mvgam_*()` functions (`plot()`), `compare_mvgams()` (`loo_compare()`), `eval_mvgam()` (`lfo_cv()`), `roll_eval_mvgam()` (`lfo_cv()`), `lv_correlations()` (`residual_cor()`), `add_residuals()` (`augment()`), `code()` (`stancode()`), `ppc()` (`pp_check()`). `summary(include_states = )` has been removed
* mvgam no longer re-exports `te()` and `ti()`
* `mvgam_forecast` objects store `train_times` and `test_times` as per-series lists
* Parameter names and `summary()` blocks follow 'brms', with smooths, Gaussian processes and monotonic effects named by term and trend parameters indexed by series name, as in `sigma_trend[DM]`
* `series_to_mvgam()` takes a `tsibble`, `ts`, `mts`, `msts` or wide data frame, fills gaps and returns one data frame unless `h` or `train_prop` is given. `xts` input has been removed

## New features
* Families: `tweedie()`, `beta_nb()`, `com_binomial()`, `occ()`, `diri()`, `multi()`, `categ()`, `mvn()`, `mvt()`. `nmix()` gained Royle-Nichols, Poisson-Poisson and multi-season variants
* @jbogomolovas2 contributed the `com_binomial()` mathematics and tested the bounded count families
* Multivariate responses through `mvbind()` and `mvbf()`
* Checks of a model and its data before fitting or prediction: `mvgam_formula()`, `mvgam_data()`, `check_newdata()`
* Moving average terms in `AR()` and `VAR()`, coefficient pooling in `AR()` and partially fixed `trend_map`
* `CAR()` gained `cor = TRUE`, takes series observed at their own times and takes `trend_formula` covariates on several series
* `LLT()` adds a local linear trend: a level with a time-varying slope that can be damped, plus an optional trigonometric season whose shape drifts (`season`, `harmonics`). Its level, slope and season come back from `hindcast()` and `forecast()` with `component`
* `loadings_prior` for structured factor loadings. Helpers for the fitted loadings: `active_factors()`, `shared_variation()`, `compare_loadings()`, `ordinate()`
* AR factor models with estimated loadings give every draw one factor order and orientation. `summary()` reports their loadings as `Z` and their AR coefficients per factor
* `jsdgam()` gained three arguments: `traits`, `trait_slopes`, `phylo`. `lv_axis()` in a `trend_formula` term gives each latent factor, or each series of a model without factors, its own effect
* The 'brms' post-processing methods work on mvgam fits. Examples: `posterior_predict()`, `log_lik()`, `update()`, `hypothesis()`, `as_draws_*()`
* Model comparison: `bridge_sampler()`, `bayes_factor()`, `waic()`, `loo_model_weights()`, `pp_average()`, `compare_scores()`, `compare_elpds()`
* `threads`, `init = "pathfinder"`, `mvgam_multiple()` for imputed data, and helpers to pivot wide occupancy and species data
* `methods_md()` writes a model description. `bibtex()` extracts the citation
* `pp_check(type = "resid_variogram")` checks residuals by time lag. `plot(type = "residuals")` draws it for unevenly spaced series
* On a hierarchical VAR, `stability()` reports each group and `posterior_transition_matrix()` averages the group matrices. Plots of `irf()` and `fevd()` select processes by name

## Changed defaults
* The `posterior_*()` prediction methods and `fitted()` default to `process_error = FALSE`, matching `predict()`
* `fitted()` includes the fitted trend state for the training data, as `residuals()` does
* Predictions with `incl_autocor = TRUE` and `log_lik()` refuse times outside the fitted times. Use `forecast()`
* `ZMVN()` and `PW()` forecasts require every series to share one set of forecast times
* `conditional_effects()` defaults to `type = "expected"`
* `pp_check()` draws bars for a bernoulli or ordinal response and a rootogram for counts
* Trend scale priors take the response's spread on the link scale as 'brms' defaults do. `LLT()` and `PW()` growth priors also scale with the series length
* `PW()` `changepoint_scale` (default `0.3`) is a change in growth over the whole series
* `CAR()` keeps its autoregressive parameter inside `(0, 1)` and measures time in median observation gaps
* The `com_binomial()` prior on `nu` is now `normal(1, 1)`
* A distributional regression on a `tweedie()`, `beta_nb()` or other custom family's parameter gives its intercept a `student_t(3, 0, 2.5)` prior, as 'brms' does for its own families

## Deprecations
* `samples` and `burnin` (use `iter` and `warmup`), `lfo_cv(data = )` (use `newdata`), `incl_dynamics` (use `incl_autocor`) and `by = trend` (use `by = lv_axis()`)

## Bug fixes
* Binomial predictions with varying trial counts stay within each observation's total (thanks to @jbogomolovas2)
* `update()` works when a trend constructor argument came from a variable
* `prior_summary()` reports the priors in the compiled Stan model
* `nmix()` models accept missing responses
* `ensemble()` combines `hindcast()` results
* `CAR()` process variance scales with the time lag (#107)
* `score(score = "brier")` scores the forecast probability, the mean of the draws
* `score(score = "variogram")` follows Scheuerer and Hamill (2015) through `scoringRules::vs_sample()`
* Variational, Laplace and Pathfinder fits name their algorithm in `summary()` and `glance()`
* `algorithm = "laplace"` honours `silent = 2`
* Fixes to forecast scores with one test observation, offsets with `gp()` terms, piecewise trend predictions, by-factor Gaussian Process predictions, the `max_treedepth` check in `summary()` and plots of residuals with missing values (thanks to @StefanoMezzini)

# mvgam 1.1.51

## New functionalities
* Changed default priors for scale parameters (i.e. process errors `"sigma"` and observation errors `"sigma_obs"`) to inverse gammas to provide more sensible prior regularisation away from zero
* Improved messaging in `summary()` for better guidance on how to investigate poor HMC sampler behaviours
* Converted several more plotting functions to return `ggplot` objects in place of base R plots for broader customisation
* Added four new `type`s to the `pp_check()` function to allow more targeted investigations of randomized quantile residual distributions
* Added a `plot.mvgam_residcor()` function for nicer plotting of estimated residual correlations from `jsdgam` objects 
* Added `summary()` functions to calculate useful posterior summaries from objects of class `mvgam_irf` and `mvgam_fevd` (see `?irf` and `?fevd` for examples)
* Improved efficiency of `nmix()` models with some slight restructuring of the model objects (#102)

## Bug fixes
* Bug fix to ensure piecewise trends are extrapolated the correct number of timepoints when forecasting using the `forecast()` function

# mvgam 1.1.4
## New functionalities
* Added the `how_to_cite.mvgam()` function to generate a scaffold methods description of fitted models, which can hopefully make it easier for users to fully describe their programming environment 
* Improved various plotting functions by returning `ggplot` objects in place of base plots (thanks to @mhollanders #38)
* Added the brier score (`score = 'brier'`) as an option in `score.mvgam_forecast()` for scoring forecasts of binary variables when using `family = bernoulli()` (#80)
* Added `augment()` function to add residuals and fitted values to an mvgam object's observed data (thanks to @swpease #83)
* Added support for approximate `gp()` effects with more than one covariate and with different kernel functions (#79) 
* Added function `jsdgam()` to estimate Joint Species Distribution Models in which both the latent factors and the observation model components can include any of mvgam's complex linear predictor effects. Also added a function `residual_cor()` to compute residual correlation, covariance and precision matrices from `jsdgam` models. See `?mvgam::jsdgam` and `?mvgam::residual_cor` for details
* Added a `stability.mvgam()` method to compute stability metrics from models fit with Vector Autoregressive dynamics (#21 and #76)
* Added functionality to estimate hierarchical error correlations when using multivariate latent process models and when the data are nested among levels of a relevant grouping factor (#75); see `?mvgam::AR` for an example
* Added `ZMVN()` error models for estimating Zero-Mean Multivariate Normal errors; convenient for working with non time-series data where latent residuals are expected to be correlated (such as when fitting Joint Species Distribution Models); see `?mvgam::ZMVN` for examples
* Added a `fevd.mvgam()` method to compute forecast error variance decompositions from models fit with Vector Autoregressive dynamics (#21 and #76)

## Deprecations
* Arguments removed from primary functions: `use_stan`, `jags_path`, `data_train`, `data_test`, `adapt_delta`, `max_treedepth`, `drift`. The removal streamlines documentation and reflects the package's mission to deprecate 'JAGS' as a suitable backend. Both `adapt_delta` and `max_treedepth` should now be supplied in a named `list()` to the new argument `control`

## Bug fixes
* Bug fix to ensure `marginaleffects::comparisons` functions appropriately recognise internal `rowid` variables
* Updates to ensure `ensemble` provides appropriate weighting of forecast draws (#98)
* Not necessarily a "bug fix", but this update removes several dependencies to lighten installation and improve efficiency of the workflow (#93)
* Fixed a minor bug in the way `trend_map` recognises levels of the `series` factor
* Bug fix to ensure `lfo_cv` recognises the actual times in `time`, just in case the user supplies data that doesn't start at `t = 1`. Also updated documentation to better reflect this
* Bug fix to ensure `update.mvgam` captures any `knots` or `trend_knots` arguments that were passed to the original model call

# mvgam 1.1.3
## New functionalities
* Allow intercepts to be included in process models when `trend_formula` is supplied. This breaks the assumption that the process has to be zero-centred, adding more modelling flexibility but also potentially inducing nonidentifiabilities with respect to any observation model intercepts. Thoughtful priors are a must for these models
* Added methods for better alignment with 'brms' workflows: `standata.mvgam_prefit`, `stancode.mvgam`, `stancode.mvgam_prefit`
* Added 'gratia' to *Enhancements* to allow popular methods such as `draw()` to be used for 'mvgam' models if 'gratia' is already installed
* Added an `ensemble.mvgam_forecast()` method to generate evenly weighted combinations of probabilistic forecast distributions
* Added an `irf.mvgam()` method to compute Generalized and Orthogonalized Impulse Response Functions (IRFs) from models fit with Vector Autoregressive dynamics

## Deprecations
* The `drift` argument has been deprecated. It is now recommended for users to include parametric fixed effects of "time" in their respective GAM formulae to capture any expected drift effects

## Bug fixes
* Added a new check to ensure that exception messages are only suppressed by the `silent` argument if the user's version of 'cmdstanr' is adequate
* Updated dependency for 'brms' to version >= '2.21.0'. Its `read_csv_as_stanfit` now converts 'cmdstanr' models to `stanfit` objects (#70)

# mvgam 1.1.2
## New functionalities
* Added options for silencing some of the 'Stan' compiler and modeling messages using the `silent` argument in `mvgam()`
* Moved a number of packages from 'Depends' to 'Imports' for simpler package loading and fewer potential masking conflicts
* Improved efficiency of the model initialisation by tweaking parameters of the underlying 'mgcv' `gam` object's convergence criteria, resulting in much faster model setups
* Added an option to use `trend_model = 'None'` in State-Space models, increasing flexibility by ensuring the process error evolves as white noise (#51)
* Added an option to use the non-centred parameterisation for some autoregressive trend models,
which speeds up mixing most of the time
* Multithreading now supports all observation families apart from `nmix()`
* Changed default priors on autoregressive coefficients (AR1, AR2, AR3) to enforce
stationarity, which is a much more sensible prior in the majority of contexts

## Bug fixes
* Fixed a small bug that prevented `conditional_effects.mvgam()` from handling effects with three-way interactions

# mvgam 1.1.1
## New functionalities
* Changed indexing of an internal c++ function after Prof Brian Ripley’s   
  email: Dear maintainer, Please see the problems shown on 
  https://cran.r-project.org/web/checks/check_results_mvgam.html. Please correct   before 2024-05-22 to safely retain your package on CRAN. The CRAN Team
  
# mvgam 1.1.0
* First release of `mvgam` to CRAN
