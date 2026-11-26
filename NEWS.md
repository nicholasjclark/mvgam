# mvgam 2.0.0

mvgam now builds its models through 'brms'. The observation model takes the full 'brms' formula syntax. `trend_formula` specifies the latent process. Most 1.1.x code needs rewriting. Start with `vignette("mvgam_overview")` and `vignette("data")`. The [articles](https://nicholasjclark.github.io/mvgam/articles/) work through each model class.

## Breaking changes
* Trend constructors go inside `trend_formula`, as in `trend_formula = ~ AR(p = 1)`. `n_lv` and `trend_map` are arguments of the trend constructors
* Removed arguments: `trend_model`, `use_lv`, `prior_simulation`, `return_model_data`, `save_all_pars`, `parallel`
* Data for a model with a trend need a factor series column, even for one series
* Renamed families: `nb()` is `negbinomial()`, `student_t()` is `student()`, `betar()` is `Beta()`
* The default family is `gaussian()`. `jsdgam()` defaults to `binomial()`
* Removed functions, with their replacements: `get_mvgam_priors()` (`get_prior()`), `plot_mvgam_*()` (`plot()`), `compare_mvgams()` (`loo_compare()`), `eval_mvgam()` and `roll_eval_mvgam()` (`lfo_cv()`), `lv_correlations()` (`residual_cor()`), `add_residuals()` (`augment()`), `code()` (`stancode()`), `ppc()` (`pp_check()`)
* `gp()` and `mo()` replace `GP()`, `dynamic()`, `s(bs = "moi")`, `s(bs = "mod")`
* Renamed arguments: `incl_autocor` (was `trend_state`), `process_error` (was `resample_innovations`), `centred` (was `noncentred`)
* Parameter names follow 'brms', as do `summary()`, `coef()`, `fixef()`. Trend parameters are indexed by series name, as in `sigma_trend[DM]`
* Posterior summaries by default from `irf()`, `fevd()`, `stability()`. Pass `summary = FALSE` for draws
* `mvgam_forecast` objects store `train_times` and `test_times` as per-series lists
* mvgam no longer re-exports `te()` and `ti()`. `series_to_mvgam()` no longer takes `xts` input
* `sim_mvgam()` is built around named scenarios. `series_to_mvgam()` returns one data frame unless `h` or `train_prop` is given

## New features
* `mvbind()` and `mvbf()` responses share one latent process. See the [multivariate](https://nicholasjclark.github.io/mvgam/articles/mvbf.html) and [integrated distribution model](https://nicholasjclark.github.io/mvgam/articles/idm.html) articles
* Imperfect detection through `occ()` and `nmix()`, each with a multi-season variant. `forecast(type = "latent_state")` forecasts occupancy or abundance. See the [occupancy](https://nicholasjclark.github.io/mvgam/articles/occupancy.html) and [N-mixture](https://nicholasjclark.github.io/mvgam/articles/nmix.html) articles
* `jsdgam()` takes traits, trait slopes, a phylogeny and the composition families (`diri()`, `multi()`, `categ()`). Summaries of the fit: `species_effects()`, `community()`, `residual_cor()`. See the [JSDM article](https://nicholasjclark.github.io/mvgam/articles/jsdgam.html)
* New dynamics: moving average terms and coefficient pooling in `AR()` and `VAR()`, hierarchical VARs, the local linear trend `LLT()`, an extended `CAR()`. See the [VAR](https://nicholasjclark.github.io/mvgam/articles/var.html) and [hierarchical VAR](https://nicholasjclark.github.io/mvgam/articles/hierarchical_var.html) articles
* Structured factor loadings through `loadings_prior`. Tools to interpret the loadings: `active_factors()`, `shared_variation()`, `compare_loadings()`, `ordinate()`. See `vignette("dfm")`
* New families: `tweedie()`, `beta_nb()`, `com_binomial()` (mathematics by @jbogomolovas2), `mvn()`, `mvt()`
* The 'brms' post-processing methods work on mvgam fits. Examples: `posterior_predict()`, `log_lik()`, `update()`, `hypothesis()`, `bridge_sampler()`
* `forecast_uncertainty()` splits forecast variance by source and horizon. `compare_scores()` and `compare_elpds()` compare models. See the [forecast evaluation article](https://nicholasjclark.github.io/mvgam/articles/forecast_evaluation.html)
* Checks of a model and its data before fitting or prediction: `mvgam_formula()`, `mvgam_data()`, `check_newdata()`
* `methods_md()` writes a model description. `bibtex()` extracts the citation
* Fits with 'cmdstanr' 1.0, which drops `cmdstan_model(compile = FALSE)`
* `pp_check()` given a `group` draws the type's `_grouped` variant, one panel per group (#49)

## Changed defaults
* The `posterior_*()` methods and `fitted()` default to `process_error = FALSE`, matching `predict()`
* `conditional_effects()` defaults to `type = "expected"`
* Predictions with `incl_autocor = TRUE` refuse times outside the fitted times. Use `forecast()`
* Trend scale priors follow the response's spread on the link scale, as 'brms' defaults do

## Deprecations
* `samples` and `burnin` (use `iter` and `warmup`), `lfo_cv(data = )` (use `newdata`), `incl_dynamics` (use `incl_autocor`), `by = trend` (use `by = lv_axis()`)

## Bug fixes
* Binomial predictions with varying trial counts stay within each observation's total (thanks to @jbogomolovas2)
* `CAR()` process variance scales with the time lag (#107)
* `ensemble()` combines `hindcast()` results
* `update()` works when a trend argument came from a variable
* Fixes to the Brier and variogram scores and to scores of forecasts with one test observation
* Fixes to offsets with `gp()` terms, piecewise trend predictions, by-factor Gaussian Process predictions and residual plots with missing values (thanks to @StefanoMezzini)

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
