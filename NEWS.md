# mvgam 2.0.0

This is a major release that rebuilds mvgam on top of 'brms'. The observation model now accepts the full 'brms' formula syntax and the latent process model is written as a separate `trend_formula`, with estimation still carried out in 'Stan'. Most models written for version 1.1.x will need their calls rewritten, so the breaking changes below are worth reading before upgrading.

## Breaking changes

### Specifying the latent process
* The latent process model is now specified through `trend_formula` together with the trend constructors (`AR()`, `RW()`, `VAR()`, `CAR()`, `PW()`, `ZMVN()`), for example `trend_formula = ~ AR(p = 1)`. The previous `trend_model` argument is no longer part of the `mvgam()` interface. A `trend_model` left in an old call is absorbed by `...` without complaint and the model is fitted without a latent process, so every model call needs checking
* `GP()` is no longer a trend type. Model a Gaussian Process directly with the 'brms' `gp()` term in either formula
* `dynamic()` has been removed. It expanded to a low-rank squared exponential Gaussian Process smooth, so write that smooth yourself as `s(time, by = x, bs = "gp")`, or use `gp(time, by = x)`
* In `trend_formula`, `by = trend` is deprecated in favour of `by = lv_axis()`. Old calls still work but emit a warning

### Impulse responses and variance decompositions
* `irf()` and `fevd()` now return the posterior median and interval of each shock-response pair rather than the per-draw responses. Both are built from one transition matrix per draw, so on a wide panel the draws ran to hundreds of megabytes for something read as a band: a 24-process VAR returned 382 MB where the summary is 340 KB. Pass `summary = FALSE` for the draws, whose class and plot method are unchanged
* `irf()` and `fevd()` gained `ndraws` and `draw_ids`, matching every other post-fit method. Cost grows with the number of draws times the square of the number of processes, so a wide panel is worth summarising from a subset
* `plot()` on a summarised response accepts the same `series` and `responses` arguments as the draws plot, so existing plotting code is unaffected

### Renamed observation families
* `nb()` is now `negbinomial()`, `student_t()` is now `student()` and `betar()` is now `Beta()`, following the 'brms' names. The former names have been removed rather than deprecated, so calls using them will error

### Changed argument names and defaults
* The `mvgam()` argument list has been pared back to match the 'brms' conventions. `use_lv` and `n_lv` have moved onto the trend constructors, so a dynamic factor model is now requested as `trend_formula = ~ AR(p = 1, n_lv = 3)`. `noncentred`, `prior_simulation`, `return_model_data`, `save_all_pars` and `parallel` have gone, and `priors` is retained only as an alias for the 'brms' name `prior`
* The default family in `mvgam()` is now `gaussian()` rather than `poisson()`, so count models must state `family = poisson()` explicitly
* `sim_mvgam()` has a new signature. `T` is now `n_timepoints`, scenarios are chosen with the new `type` argument, and `seasonality`, `use_lv`, `drift`, `trend_rel` and `freq` are gone. The default family is now `gaussian()`
* The default family in `jsdgam()` is now `binomial()` rather than `poisson()`, so presence-absence data no longer needs the family stated explicitly while count data does

### Removed smooth bases and constructors
* The monotonic spline bases `s(x, bs = "moi")` and `s(x, bs = "mod")` have been removed. To constrain an effect to be monotonic, use the 'brms' `mo()` term in the observation formula (see `?brms::mo`), which models a monotonic effect of an ordered predictor
* mvgam no longer re-exports the 'mgcv' smooth constructors. `s()`, `t2()`, `gp()` and `mo()` stay available because 'brms' attaches them, but `te()` and `ti()` are no longer bound at the console. Both still work inside model formulae; call them as `mgcv::te()` and `mgcv::ti()` if you need the constructor on its own

### Removed functions
* `get_mvgam_priors()` has been removed. Inspect the priors for a model with the `get_prior()` and `default_prior()` methods, which accept an `mvgam_formula` alongside the data and family
* The `plot_mvgam_*()` functions (`plot_mvgam_series()`, `plot_mvgam_fc()`, `plot_mvgam_trend()`, `plot_mvgam_smooth()`, `plot_mvgam_resids()`, `plot_mvgam_factors()`, `plot_mvgam_pterms()`, `plot_mvgam_randomeffects()` and `plot_mvgam_uncertainty()`) are no longer exported. Their output is now reached through `plot()` on the fitted model, which takes `type = "series"`, `"residuals"`, `"smooths"`, `"trend"`, `"factors"` or `"latent_state"`, and through `plot()` on the objects returned by `hindcast()`, `forecast()` and `conditional_effects()`
* `compare_mvgams()` has been removed. Rank models by predictive density with `loo_compare()`, or by out-of-sample forecast accuracy with `score()` and `compare_scores()`
* `eval_mvgam()` and `roll_eval_mvgam()` have been removed. Approximate leave-future-out cross validation is available through `lfo_cv()`, and `compare_elpds()` tabulates the result across several fits
* `lv_correlations()` has been removed; `residual_cor()` now covers the same ground and returns correlation, covariance and precision matrices
* `add_residuals()` has been removed; `augment()` appends residuals and fitted values to the model's observed data
* `code()` has been removed; use `stancode()`. `ppc()` has been removed; use `pp_check()`

### Changed object structures
* Objects of class `mvgam_forecast` now hold lists of length `n_series` in their `train_times` and `test_times` slots. This lets continuous time data be handled properly, where some series may have been sampled at different timepoints
* `summary.mvgam()` returns an object of class `mvgam_summary` that can be stored and reused, or printed with `print.mvgam_summary()` (#119)
* `conditional_smooths()` returns an object of class `mvgam_conditional_smooths` with its own `plot()` method

## New functionalities

### Observation families
* Added `tweedie()`, a compound Poisson-gamma family for positive continuous data with an exact mass at zero, such as catch per unit effort or rainfall
* Added `beta_nb()` for counts with heavier tails than a negative binomial allows
* Added `com_binomial()`, a Conway-Maxwell-binomial family covering bounded counts that are under-, over- or super-dispersed relative to a binomial
* Added `occ()` for occupancy models built from replicate detection and non-detection visits, with an opt-in multi-season variant
* Added `diri()`, `multi()` and `categ()` for compositional counts, proportions on the simplex and single-trial categorical outcomes recorded across several rows of one closure unit
* Added `mvn()` and `mvt()` for continuous multi-species responses with low-rank residual covariance, the latter carrying a degrees of freedom parameter for heavier tails
* `nmix()` gained Royle-Nichols and Poisson-Poisson variants alongside the original Poisson-binomial formulation, multi-season support, automatic `K_max` defaults and the `latent_N_saturation()` diagnostic for checking that the abundance truncation was set high enough
* Closure-unit families group replicate rows by their `series` and `time` values, so repeat visits to a site or the species making up one assemblage need no reshaping

### Model specification and fitting
* Multivariate responses are supported through the 'brms' `mvbind()` and `mvbf()` syntax, including `set_rescor()`. Post-processing methods take a `resp` argument to scope output to one response, and fan out across responses when it is left `NULL`
* Added `mvgam_formula()` to build and inspect a model's formula before fitting, and `mvgam_data()` (with its `check_mvgam_data()` alias) to check that a long-format dataset suits a proposed observation family
* Added `validate_newdata()` to check `newdata` against a fitted model's training data before predicting or forecasting
* `AR()` and `VAR()` gained `ma = TRUE` for moving average terms, giving ARMA and VARMA processes, and `AR()` gained `coef_sharing` for pooling autoregressive coefficients across series (`"none"`, `"shared"` or `"hierarchical"`)
* `trend_map` accepts `NA` entries so that part of the loading matrix can be fixed and the rest estimated
* Added a registry for user-defined trends: `custom_trend()`, `create_mvgam_trend()`, `trend_param()`, `register_trend_type()` and `register_custom_trend()` define new process models, while `list_trend_types()` and `mvgam_trend_choices()` report what is available
* Added `threads` as an argument to `mvgam()` and `jsdgam()` for within-chain parallelisation
* Added `init = "pathfinder"` to `mvgam()` for drawing starting values from Stan's Pathfinder approximation
* Added `mvgam_multiple()` to fit a model across multiply imputed datasets and pool the draws
* Added `pivot_detection_array()` and `pivot_species_matrix()` to reshape data held in the wide layouts used by 'spOccupancy', 'unmarked' and 'Hmsc' into mvgam's long format
* `sim_mvgam()` was rewritten around a catalogue of named scenarios, each checked so that a model fitted to the simulated data recovers the parameters that generated it. Added `sim_closure_unit_data()` to simulate detection data with known truth for checking closure-unit models

### Latent factor models and `jsdgam()`
* Added `loadings_prior` to `mvgam()` and `jsdgam()` for structured priors on the loading matrix, including a multiplicative gamma process that shrinks unneeded factors towards zero
* Added `active_factors()` to summarise which factors the shrinkage prior kept, `shared_variation()` to summarise the covariance those factors imply, and `compare_loadings()` to contrast the loadings of two fits
* `jsdgam()` gained `traits`, `trait_slopes` and `phylo` arguments, so species responses can be regressed on traits in a fourth-corner design or structured by a phylogeny
* Smooth terms can vary by latent factor through the `by = lv_axis()` sentinel, letting each factor carry its own nonlinear response
* Added `ordinate()` to plot two-dimensional ordinations of site and species scores from latent factor models. It applies to `jsdgam()` fits and to any factor model fitted with `mvgam()`, and offers `rotation` choices along with trait vector overlays on the biplot
* `residual_cor()` now supports models fitted with `mvgam()` in which latent factors were used or in which correlated dynamic processes were used
* `forecast()` projects factor models forward by propagating the latent factors rather than the series-level states

### Prediction, forecasting and post-processing
* Added the 'brms' post-processing surface for `mvgam` objects: `posterior_predict()`, `posterior_epred()`, `posterior_linpred()`, `predict()`, `fitted()`, `log_lik()`, `update()`, `fixef()`, `ranef()`, `VarCorr()`, `hypothesis()`, `prior_summary()`, `posterior_smooths()`, `conditional_smooths()`, `inits()`, `control_params()` and the `as_draws_*()` family
* `hindcast()` gained `type = "latent_state"` for closure-unit families, returning posterior draws of the unobserved state alongside its own print and plot methods
* `fitted()` gained `components` and `unit_level` arguments so that the response, latent state and detection parts of a closure-unit model can be extracted separately
* `loo()` gained `by_species` for per-series expected log predictive density on multi-response and closure-unit fits, and `waic()` is now available for `mvgam` objects
* Added an M-closed model comparison surface to sit alongside predictive scoring: `bridge_sampler()` estimates the log marginal likelihood and `bayes_factor()` compares two fits. `add_criterion()`, `loo_model_weights()`, `pp_average()` and `posterior_average()` support model averaging
* Added `summary.mvgam_forecast()` to return prediction intervals of posterior hindcasts and forecasts as a `data.frame`, making custom plots of those distributions easier to build (#108)
* Added `compare_scores()` and `compare_elpds()` to tabulate forecast scores and leave-future-out ELPDs across several models
* Added `posterior_transition_matrix()` to extract the posterior transition matrix of a VAR trend, and `smooths()` to enumerate the smooth terms in a fitted model
* Added a `plot.mvgam_stability()` method for the metrics returned by `stability()`
* `pp_check()` gained per-category facets for multi-response and closure-unit families

### Reporting and documentation
* Added `methods_md()`, which renders a fitted model's mathematical description as Markdown or PDF, covering smooths, Gaussian Processes, monotonic and measurement-error terms, varying slopes, nonlinear formulae, closure-unit families and the latent process
* Added `bibtex()` to pull the BibTeX block out of a `how_to_cite()` object
* Added a `mvgam_use_cases` help file to provide links to online resources that discuss how to use 'mvgam' in practice
* Added seven articles to the package website covering forecast evaluation, state-space vector autoregressions, hierarchical vector autoregressions, N-mixture models, joint species distribution models with informed loadings, integrated distribution models and multi-response models

## Changing defaults
* The `com_binomial()` default prior on `nu` is now `normal(1, 1)` rather than `normal(1, 0.5)`, which was too tight to reach the strongly under-dispersed counts that sit near `nu = 3`
* The `forecast()` method is now imported from 'generics' to help avoid conflict issues with other forecasting packages
* Changed default `type` in `conditional_effects()` to `expected` to match behaviour of 'brms'
* `CAR()` now constrains the autoregressive parameter to the strict interior of `(0, 1)`, avoiding the boundary behaviour that stalled sampling
* Exact Gaussian Process terms now emit a warning rather than failing, and the notice is issued once per term per session

## Deprecations
* `samples` and `burnin` are deprecated in `mvgam()`. Use `iter` (total iterations) and `warmup` instead; supplying both pairs on one call is an error
* `run_model = FALSE` is deprecated. Use `stancode()` and `standata()` to obtain the model code and data without fitting
* The `data` argument of `lfo_cv()` is deprecated in favour of `newdata`
* Deprecated the `incl_dynamics` argument in the `loo()` and `loo_compare()` functions to ensure better consistency in log-likelihood and resulting LOO estimates from models with different observation families

## Bug fixes
* Closure-unit families (`occ()`, `nmix()`) now accept missing responses, so occasions that were never visited can be left as `NA`
* `com_binomial()` handles missing responses correctly
* Bug fix to ensure forecast scores are properly computed when plotting objects of class `mvgam_forecast` if only a single out-of-sample observation was included in `newdata`
* Bug fix to ensure offsets supplied with `offset(...)` in formulae are correctly incorporated when using `gp()` terms
* Bug fix to ensure piecewise trends are correctly predicted when using `process_error = TRUE` in `predict()`
* Bug fix to ensure variance of continuous time autoregressive processes (using `CAR()`) scales appropriately with time lags (#107)
* Bug fix to ensure by-factor Gaussian Process predictions contribute at every level of the factor
* Bug fix to ensure `summary.mvgam()` uses the correct `max_treedepth` value when checking Stan diagnostics, rather than always assuming the default of 10 (thanks to @StefanoMezzini for reporting)
* Bug fix to ensure `NA` residual values are handled properly when plotting residuals (this occurs because response values are allowed to be missing; thanks to @StefanoMezzini for reporting)

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
* Arguments `use_stan`, `jags_path`, `data_train`, `data_test`, `adapt_delta`, `max_treedepth` and `drift` have been removed from primary functions to streamline documentation and reflect the package's mission to deprecate 'JAGS' as a suitable backend. Both `adapt_delta` and `max_treedepth` should now be supplied in a named `list()` to the new argument `control`

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
* Added `standata.mvgam_prefit`, `stancode.mvgam` and `stancode.mvgam_prefit` methods for better alignment with 'brms' workflows
* Added 'gratia' to *Enhancements* to allow popular methods such as `draw()` to be used for 'mvgam' models if 'gratia' is already installed
* Added an `ensemble.mvgam_forecast()` method to generate evenly weighted combinations of probabilistic forecast distributions
* Added an `irf.mvgam()` method to compute Generalized and Orthogonalized Impulse Response Functions (IRFs) from models fit with Vector Autoregressive dynamics

## Deprecations
* The `drift` argument has been deprecated. It is now recommended for users to include parametric fixed effects of "time" in their respective GAM formulae to capture any expected drift effects

## Bug fixes
* Added a new check to ensure that exception messages are only suppressed by the `silent` argument if the user's version of 'cmdstanr' is adequate
* Updated dependency for 'brms' to version >= '2.21.0' so that `read_csv_as_stanfit` can be imported, which should future-proof the conversion of 'cmdstanr' models to `stanfit` objects (#70)

# mvgam 1.1.2
## New functionalities
* Added options for silencing some of the 'Stan' compiler and modeling messages using the `silent` argument in `mvgam()`
* Moved a number of packages from 'Depends' to 'Imports' for simpler package loading and fewer potential masking conflicts
* Improved efficiency of the model initialisation by tweaking parameters of the underlying 'mgcv' `gam` object's convergence criteria, resulting in much faster model setups
* Added an option to use `trend_model = 'None'` in State-Space models, increasing flexibility by ensuring the process error evolves as white noise (#51)
* Added an option to use the non-centred parameterisation for some autoregressive trend models,
which speeds up mixing most of the time
* Updated support for multithreading so that all observation families (apart from `nmix()`) can now be modeled with multiple threads
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
