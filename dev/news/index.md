# Changelog

## mvgam 2.0.0

mvgam now builds its models through ‘brms’. The observation model takes
the full ‘brms’ formula syntax. `trend_formula` specifies the latent
process. Most 1.1.x code needs rewriting. Start with
[`vignette("mvgam_overview")`](https://nicholasjclark.github.io/mvgam/dev/articles/mvgam_overview.md)
and
[`vignette("data")`](https://nicholasjclark.github.io/mvgam/dev/articles/data.md).
The [articles](https://nicholasjclark.github.io/mvgam/articles/) work
through each model class.

### Breaking changes

- Trend constructors go inside `trend_formula`, as in
  `trend_formula = ~ AR(p = 1)`. `n_lv` and `trend_map` are arguments of
  the trend constructors
- Removed arguments: `trend_model`, `use_lv`, `prior_simulation`,
  `return_model_data`, `save_all_pars`, `parallel`
- Data for a model with a trend need a factor series column, even for
  one series
- Renamed families: `nb()` is
  [`negbinomial()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  `student_t()` is
  [`student()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  `betar()` is
  [`Beta()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
- The default family is
  [`gaussian()`](https://rdrr.io/r/stats/family.html).
  [`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
  defaults to [`binomial()`](https://rdrr.io/r/stats/family.html)
- Removed functions, with their replacements: `get_mvgam_priors()`
  ([`get_prior()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.md)),
  `plot_mvgam_*()`
  ([`plot()`](https://rdrr.io/r/graphics/plot.default.html)),
  `compare_mvgams()`
  ([`loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html)),
  `eval_mvgam()` and `roll_eval_mvgam()`
  ([`lfo_cv()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md)),
  `lv_correlations()`
  ([`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md)),
  `add_residuals()`
  ([`augment()`](https://generics.r-lib.org/reference/augment.html)),
  `code()`
  ([`stancode()`](https://paulbuerkner.com/brms/reference/stancode.html)),
  `ppc()`
  ([`pp_check()`](https://nicholasjclark.github.io/mvgam/dev/reference/pp_check.mvgam.md))
- `gp()` and `mo()` replace `GP()`, `dynamic()`, `s(bs = "moi")`,
  `s(bs = "mod")`
- Renamed arguments: `incl_autocor` (was `trend_state`), `process_error`
  (was `resample_innovations`), `centred` (was `noncentred`)
- Parameter names follow ‘brms’, as do
  [`summary()`](https://rdrr.io/r/base/summary.html),
  [`coef()`](https://rdrr.io/r/stats/coef.html),
  [`fixef()`](https://rdrr.io/pkg/nlme/man/fixed.effects.html). Trend
  parameters are indexed by series name, as in `sigma_trend[DM]`
- Posterior summaries by default from
  [`irf()`](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md),
  [`fevd()`](https://nicholasjclark.github.io/mvgam/dev/reference/fevd.mvgam.md),
  [`stability()`](https://nicholasjclark.github.io/mvgam/dev/reference/stability.mvgam.md).
  Pass `summary = FALSE` for draws
- `mvgam_forecast` objects store `train_times` and `test_times` as
  per-series lists
- mvgam no longer re-exports `te()` and `ti()`.
  [`series_to_mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/series_to_mvgam.md)
  no longer takes `xts` input
- [`sim_mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/sim_mvgam.md)
  is built around named scenarios.
  [`series_to_mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/series_to_mvgam.md)
  returns one data frame unless `h` or `train_prop` is given

### New features

- `mvbind()` and `mvbf()` responses share one latent process. See the
  [multivariate](https://nicholasjclark.github.io/mvgam/articles/mvbf.html)
  and [integrated distribution
  model](https://nicholasjclark.github.io/mvgam/articles/idm.html)
  articles
- Imperfect detection through
  [`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md)
  and
  [`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md),
  each with a multi-season variant. `forecast(type = "latent_state")`
  forecasts occupancy or abundance. See the
  [occupancy](https://nicholasjclark.github.io/mvgam/articles/occupancy.html)
  and
  [N-mixture](https://nicholasjclark.github.io/mvgam/articles/nmix.html)
  articles
- [`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
  takes traits, trait slopes, a phylogeny and the composition families
  ([`diri()`](https://nicholasjclark.github.io/mvgam/dev/reference/diri.md),
  [`multi()`](https://nicholasjclark.github.io/mvgam/dev/reference/multi.md),
  [`categ()`](https://nicholasjclark.github.io/mvgam/dev/reference/categ.md)).
  Summaries of the fit:
  [`species_effects()`](https://nicholasjclark.github.io/mvgam/dev/reference/species_effects.md),
  [`community()`](https://nicholasjclark.github.io/mvgam/dev/reference/community.md),
  [`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md).
  See the [JSDM
  article](https://nicholasjclark.github.io/mvgam/articles/jsdgam.html)
- New dynamics: moving average terms and coefficient pooling in
  [`AR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  and
  [`VAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md),
  hierarchical VARs, the local linear trend
  [`LLT()`](https://nicholasjclark.github.io/mvgam/dev/reference/LLT.md),
  an extended
  [`CAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md).
  See the
  [VAR](https://nicholasjclark.github.io/mvgam/articles/var.html) and
  [hierarchical
  VAR](https://nicholasjclark.github.io/mvgam/articles/hierarchical_var.html)
  articles
- Structured factor loadings through `loadings_prior`. Tools to
  interpret the loadings:
  [`active_factors()`](https://nicholasjclark.github.io/mvgam/dev/reference/active_factors.md),
  [`shared_variation()`](https://nicholasjclark.github.io/mvgam/dev/reference/shared_variation.md),
  [`compare_loadings()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_loadings.md),
  [`ordinate()`](https://nicholasjclark.github.io/mvgam/dev/reference/ordinate.md).
  See
  [`vignette("dfm")`](https://nicholasjclark.github.io/mvgam/dev/articles/dfm.md)
- New families:
  [`tweedie()`](https://nicholasjclark.github.io/mvgam/dev/reference/tweedie.md),
  [`beta_nb()`](https://nicholasjclark.github.io/mvgam/dev/reference/beta_nb.md),
  [`com_binomial()`](https://nicholasjclark.github.io/mvgam/dev/reference/com_binomial.md)
  (mathematics by [@jbogomolovas2](https://github.com/jbogomolovas2)),
  [`mvn()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvn.md),
  [`mvt()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvt.md)
- The ‘brms’ post-processing methods work on mvgam fits. Examples:
  `posterior_predict()`,
  [`log_lik()`](https://mc-stan.org/rstantools/reference/log_lik.html),
  [`update()`](https://rdrr.io/r/stats/update.html),
  [`hypothesis()`](https://paulbuerkner.com/brms/reference/hypothesis.brmsfit.html),
  [`bridge_sampler()`](https://rdrr.io/pkg/bridgesampling/man/bridge_sampler.html)
- [`forecast_uncertainty()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast_uncertainty.md)
  splits forecast variance by source and horizon.
  [`compare_scores()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_scores.md)
  and
  [`compare_elpds()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_elpds.md)
  compare models. See the [forecast evaluation
  article](https://nicholasjclark.github.io/mvgam/articles/forecast_evaluation.html)
- Checks of a model and its data before fitting or prediction:
  [`mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_formula.md),
  [`mvgam_data()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_data.md),
  [`check_newdata()`](https://nicholasjclark.github.io/mvgam/dev/reference/check_newdata.md)
- [`methods_md()`](https://nicholasjclark.github.io/mvgam/dev/reference/methods_md.md)
  writes a model description.
  [`bibtex()`](https://nicholasjclark.github.io/mvgam/dev/reference/bibtex.md)
  extracts the citation

### Changed defaults

- The `posterior_*()` methods and
  [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) default to
  `process_error = FALSE`, matching
  [`predict()`](https://rdrr.io/r/stats/predict.html)
- [`conditional_effects()`](https://paulbuerkner.com/brms/reference/conditional_effects.brmsfit.html)
  defaults to `type = "expected"`
- Predictions with `incl_autocor = TRUE` refuse times outside the fitted
  times. Use
  [`forecast()`](https://generics.r-lib.org/reference/forecast.html)
- Trend scale priors follow the response’s spread on the link scale, as
  ‘brms’ defaults do

### Deprecations

- `samples` and `burnin` (use `iter` and `warmup`), `lfo_cv(data = )`
  (use `newdata`), `incl_dynamics` (use `incl_autocor`), `by = trend`
  (use `by = lv_axis()`)

### Bug fixes

- Binomial predictions with varying trial counts stay within each
  observation’s total (thanks to
  [@jbogomolovas2](https://github.com/jbogomolovas2))
- [`CAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  process variance scales with the time lag
  ([\#107](https://github.com/nicholasjclark/mvgam/issues/107))
- [`ensemble()`](https://nicholasjclark.github.io/mvgam/dev/reference/ensemble.mvgam_forecast.md)
  combines
  [`hindcast()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md)
  results
- [`update()`](https://rdrr.io/r/stats/update.html) works when a trend
  argument came from a variable
- Fixes to the Brier and variogram scores and to scores of forecasts
  with one test observation
- Fixes to offsets with `gp()` terms, piecewise trend predictions,
  by-factor Gaussian Process predictions and residual plots with missing
  values (thanks to
  [@StefanoMezzini](https://github.com/StefanoMezzini))

## mvgam 1.1.51

CRAN release: 2025-03-14

### New functionalities

- Changed default priors for scale parameters (i.e. process errors
  `"sigma"` and observation errors `"sigma_obs"`) to inverse gammas to
  provide more sensible prior regularisation away from zero
- Improved messaging in
  [`summary()`](https://rdrr.io/r/base/summary.html) for better guidance
  on how to investigate poor HMC sampler behaviours
- Converted several more plotting functions to return `ggplot` objects
  in place of base R plots for broader customisation
- Added four new `type`s to the
  [`pp_check()`](https://nicholasjclark.github.io/mvgam/dev/reference/pp_check.mvgam.md)
  function to allow more targeted investigations of randomized quantile
  residual distributions
- Added a
  [`plot.mvgam_residcor()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_residcor-class.md)
  function for nicer plotting of estimated residual correlations from
  `jsdgam` objects
- Added [`summary()`](https://rdrr.io/r/base/summary.html) functions to
  calculate useful posterior summaries from objects of class `mvgam_irf`
  and `mvgam_fevd` (see
  [`?irf`](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md)
  and
  [`?fevd`](https://nicholasjclark.github.io/mvgam/dev/reference/fevd.mvgam.md)
  for examples)
- Improved efficiency of
  [`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md)
  models with some slight restructuring of the model objects
  ([\#102](https://github.com/nicholasjclark/mvgam/issues/102))

### Bug fixes

- Bug fix to ensure piecewise trends are extrapolated the correct number
  of timepoints when forecasting using the
  [`forecast()`](https://generics.r-lib.org/reference/forecast.html)
  function

## mvgam 1.1.4

CRAN release: 2025-02-19

### New functionalities

- Added the
  [`how_to_cite.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/how_to_cite.mvgam.md)
  function to generate a scaffold methods description of fitted models,
  which can hopefully make it easier for users to fully describe their
  programming environment
- Improved various plotting functions by returning `ggplot` objects in
  place of base plots (thanks to
  [@mhollanders](https://github.com/mhollanders)
  [\#38](https://github.com/nicholasjclark/mvgam/issues/38))
- Added the brier score (`score = 'brier'`) as an option in
  [`score.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/score.mvgam_forecast.md)
  for scoring forecasts of binary variables when using
  `family = bernoulli()`
  ([\#80](https://github.com/nicholasjclark/mvgam/issues/80))
- Added [`augment()`](https://generics.r-lib.org/reference/augment.html)
  function to add residuals and fitted values to an mvgam object’s
  observed data (thanks to [@swpease](https://github.com/swpease)
  [\#83](https://github.com/nicholasjclark/mvgam/issues/83))
- Added support for approximate `gp()` effects with more than one
  covariate and with different kernel functions
  ([\#79](https://github.com/nicholasjclark/mvgam/issues/79))
- Added function
  [`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
  to estimate Joint Species Distribution Models in which both the latent
  factors and the observation model components can include any of
  mvgam’s complex linear predictor effects. Also added a function
  [`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md)
  to compute residual correlation, covariance and precision matrices
  from `jsdgam` models. See
  [`?mvgam::jsdgam`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
  and
  [`?mvgam::residual_cor`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md)
  for details
- Added a
  [`stability.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/stability.mvgam.md)
  method to compute stability metrics from models fit with Vector
  Autoregressive dynamics
  ([\#21](https://github.com/nicholasjclark/mvgam/issues/21) and
  [\#76](https://github.com/nicholasjclark/mvgam/issues/76))
- Added functionality to estimate hierarchical error correlations when
  using multivariate latent process models and when the data are nested
  among levels of a relevant grouping factor
  ([\#75](https://github.com/nicholasjclark/mvgam/issues/75)); see
  [`?mvgam::AR`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  for an example
- Added
  [`ZMVN()`](https://nicholasjclark.github.io/mvgam/dev/reference/ZMVN.md)
  error models for estimating Zero-Mean Multivariate Normal errors;
  convenient for working with non time-series data where latent
  residuals are expected to be correlated (such as when fitting Joint
  Species Distribution Models); see
  [`?mvgam::ZMVN`](https://nicholasjclark.github.io/mvgam/dev/reference/ZMVN.md)
  for examples
- Added a
  [`fevd.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/fevd.mvgam.md)
  method to compute forecast error variance decompositions from models
  fit with Vector Autoregressive dynamics
  ([\#21](https://github.com/nicholasjclark/mvgam/issues/21) and
  [\#76](https://github.com/nicholasjclark/mvgam/issues/76))

### Deprecations

- Arguments removed from primary functions: `use_stan`, `jags_path`,
  `data_train`, `data_test`, `adapt_delta`, `max_treedepth`, `drift`.
  The removal streamlines documentation and reflects the package’s
  mission to deprecate ‘JAGS’ as a suitable backend. Both `adapt_delta`
  and `max_treedepth` should now be supplied in a named
  [`list()`](https://rdrr.io/r/base/list.html) to the new argument
  `control`

### Bug fixes

- Bug fix to ensure
  [`marginaleffects::comparisons`](https://rdrr.io/pkg/marginaleffects/man/comparisons.html)
  functions appropriately recognise internal `rowid` variables
- Updates to ensure `ensemble` provides appropriate weighting of
  forecast draws
  ([\#98](https://github.com/nicholasjclark/mvgam/issues/98))
- Not necessarily a “bug fix”, but this update removes several
  dependencies to lighten installation and improve efficiency of the
  workflow ([\#93](https://github.com/nicholasjclark/mvgam/issues/93))
- Fixed a minor bug in the way `trend_map` recognises levels of the
  `series` factor
- Bug fix to ensure `lfo_cv` recognises the actual times in `time`, just
  in case the user supplies data that doesn’t start at `t = 1`. Also
  updated documentation to better reflect this
- Bug fix to ensure `update.mvgam` captures any `knots` or `trend_knots`
  arguments that were passed to the original model call

## mvgam 1.1.3

CRAN release: 2024-09-04

### New functionalities

- Allow intercepts to be included in process models when `trend_formula`
  is supplied. This breaks the assumption that the process has to be
  zero-centred, adding more modelling flexibility but also potentially
  inducing nonidentifiabilities with respect to any observation model
  intercepts. Thoughtful priors are a must for these models
- Added methods for better alignment with ‘brms’ workflows:
  `standata.mvgam_prefit`, `stancode.mvgam`, `stancode.mvgam_prefit`
- Added ‘gratia’ to *Enhancements* to allow popular methods such as
  `draw()` to be used for ‘mvgam’ models if ‘gratia’ is already
  installed
- Added an
  [`ensemble.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/ensemble.mvgam_forecast.md)
  method to generate evenly weighted combinations of probabilistic
  forecast distributions
- Added an
  [`irf.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md)
  method to compute Generalized and Orthogonalized Impulse Response
  Functions (IRFs) from models fit with Vector Autoregressive dynamics

### Deprecations

- The `drift` argument has been deprecated. It is now recommended for
  users to include parametric fixed effects of “time” in their
  respective GAM formulae to capture any expected drift effects

### Bug fixes

- Added a new check to ensure that exception messages are only
  suppressed by the `silent` argument if the user’s version of
  ‘cmdstanr’ is adequate
- Updated dependency for ‘brms’ to version \>= ‘2.21.0’. Its
  `read_csv_as_stanfit` now converts ‘cmdstanr’ models to `stanfit`
  objects ([\#70](https://github.com/nicholasjclark/mvgam/issues/70))

## mvgam 1.1.2

CRAN release: 2024-07-01

### New functionalities

- Added options for silencing some of the ‘Stan’ compiler and modeling
  messages using the `silent` argument in
  [`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
- Moved a number of packages from ‘Depends’ to ‘Imports’ for simpler
  package loading and fewer potential masking conflicts
- Improved efficiency of the model initialisation by tweaking parameters
  of the underlying ‘mgcv’ `gam` object’s convergence criteria,
  resulting in much faster model setups
- Added an option to use `trend_model = 'None'` in State-Space models,
  increasing flexibility by ensuring the process error evolves as white
  noise ([\#51](https://github.com/nicholasjclark/mvgam/issues/51))
- Added an option to use the non-centred parameterisation for some
  autoregressive trend models, which speeds up mixing most of the time
- Multithreading now supports all observation families apart from
  [`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md)
- Changed default priors on autoregressive coefficients (AR1, AR2, AR3)
  to enforce stationarity, which is a much more sensible prior in the
  majority of contexts

### Bug fixes

- Fixed a small bug that prevented
  [`conditional_effects.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_effects.mvgam.md)
  from handling effects with three-way interactions

## mvgam 1.1.1

CRAN release: 2024-05-10

### New functionalities

- Changed indexing of an internal c++ function after Prof Brian
  Ripley’s  
  email: Dear maintainer, Please see the problems shown on
  <https://cran.r-project.org/web/checks/check_results_mvgam.html>.
  Please correct before 2024-05-22 to safely retain your package on
  CRAN. The CRAN Team

## mvgam 1.1.0

CRAN release: 2024-05-06

- First release of `mvgam` to CRAN
