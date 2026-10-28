# Changelog

## mvgam 2.0.0

mvgam now uses ‘brms’ to build its models. The observation model takes
the full ‘brms’ formula syntax and a separate `trend_formula` specifies
the latent process. Most 1.1.x calls need rewriting: see
[`vignette("mvgam_overview")`](https://nicholasjclark.github.io/mvgam/dev/articles/mvgam_overview.md)
and the package website for worked examples.

### Breaking changes

- Trend constructors now go inside `trend_formula`,
  e.g. `trend_formula = ~ AR(p = 1)`. The `trend_model` argument has
  been removed
- `n_lv` moved onto the trend constructors. Removed arguments: `use_lv`,
  `noncentred`, `prior_simulation`, `return_model_data`,
  `save_all_pars`, `parallel`
- Data for a model with a trend need a factor series column, even for
  one series
- Renamed families: `nb()` is now
  [`negbinomial()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  `student_t()` is now
  [`student()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  `betar()` is now
  [`Beta()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
- Default family is now
  [`gaussian()`](https://rdrr.io/r/stats/family.html) in
  [`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
  and
  [`sim_mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/sim_mvgam.md).
  [`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
  defaults to [`binomial()`](https://rdrr.io/r/stats/family.html)
- [`sim_mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/sim_mvgam.md)
  has a new signature built around named scenarios
- Removed `GP()`, `dynamic()`, `s(bs = "moi")`, `s(bs = "mod")`. Use
  `gp()` and `mo()`
- `incl_autocor` now chooses the prediction surface, replacing
  `trend_state`. `hindcast(resample_innovations = )` is now
  `process_error`
- Renamed arguments: `residual_cor(by_group = )`,
  `score(quantile_level = )`, `forecast(coef_uncertainty = )`,
  `posterior_transition_matrix(groups = )`
- `score(score = "drps")` no longer takes `log = TRUE`. Use `"crps"` on
  the log scale
- Posterior summaries by default from
  [`irf()`](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md),
  [`fevd()`](https://nicholasjclark.github.io/mvgam/dev/reference/fevd.mvgam.md),
  [`stability()`](https://nicholasjclark.github.io/mvgam/dev/reference/stability.mvgam.md).
  Pass `summary = FALSE` for draws
- Removed functions, with their replacements: `get_mvgam_priors()`
  ([`get_prior()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.md)),
  the `plot_mvgam_*()` functions
  ([`plot()`](https://rdrr.io/r/graphics/plot.default.html)),
  `compare_mvgams()`
  ([`loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html)),
  `eval_mvgam()`
  ([`lfo_cv()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md)),
  `roll_eval_mvgam()`
  ([`lfo_cv()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md)),
  `lv_correlations()`
  ([`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md)),
  `add_residuals()`
  ([`augment()`](https://generics.r-lib.org/reference/augment.html)),
  `code()`
  ([`stancode()`](https://paulbuerkner.com/brms/reference/stancode.html)),
  `ppc()`
  ([`pp_check()`](https://nicholasjclark.github.io/mvgam/dev/reference/pp_check.mvgam.md)).
  `summary(include_states = )` has been removed
- mvgam no longer re-exports `te()` and `ti()`
- `mvgam_forecast` objects store `train_times` and `test_times` as
  per-series lists
- Parameter names and [`summary()`](https://rdrr.io/r/base/summary.html)
  blocks follow ‘brms’, with smooths, Gaussian processes and monotonic
  effects named by term and trend parameters indexed by series name, as
  in `sigma_trend[DM]`
- [`series_to_mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/series_to_mvgam.md)
  takes a `tsibble`, `ts`, `mts`, `msts` or wide data frame, fills gaps
  and returns one data frame unless `h` or `train_prop` is given. `xts`
  input has been removed

### New features

- Families:
  [`tweedie()`](https://nicholasjclark.github.io/mvgam/dev/reference/tweedie.md),
  [`beta_nb()`](https://nicholasjclark.github.io/mvgam/dev/reference/beta_nb.md),
  [`com_binomial()`](https://nicholasjclark.github.io/mvgam/dev/reference/com_binomial.md),
  [`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md),
  [`diri()`](https://nicholasjclark.github.io/mvgam/dev/reference/diri.md),
  [`multi()`](https://nicholasjclark.github.io/mvgam/dev/reference/multi.md),
  [`categ()`](https://nicholasjclark.github.io/mvgam/dev/reference/categ.md),
  [`mvn()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvn.md),
  [`mvt()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvt.md).
  [`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md)
  gained Royle-Nichols, Poisson-Poisson and multi-season variants
- @jbogomolovas2 contributed the
  [`com_binomial()`](https://nicholasjclark.github.io/mvgam/dev/reference/com_binomial.md)
  mathematics and tested the bounded count families
- Multivariate responses through `mvbind()` and `mvbf()`
- Checks of a model and its data before fitting or prediction:
  [`mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_formula.md),
  [`mvgam_data()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_data.md),
  [`check_newdata()`](https://nicholasjclark.github.io/mvgam/dev/reference/check_newdata.md)
- Moving average terms in
  [`AR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  and
  [`VAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md),
  coefficient pooling in
  [`AR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  and partially fixed `trend_map`
- [`CAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  gained `cor = TRUE`, takes series observed at their own times and
  takes `trend_formula` covariates on several series
- [`LLT()`](https://nicholasjclark.github.io/mvgam/dev/reference/LLT.md)
  adds a local linear trend: a level with a time-varying slope that can
  be damped, plus an optional trigonometric season whose shape drifts
  (`season`, `harmonics`). Its level, slope and season come back from
  [`hindcast()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.md)
  and [`forecast()`](https://generics.r-lib.org/reference/forecast.html)
  with `component`
- `loadings_prior` for structured factor loadings. Helpers for the
  fitted loadings:
  [`active_factors()`](https://nicholasjclark.github.io/mvgam/dev/reference/active_factors.md),
  [`shared_variation()`](https://nicholasjclark.github.io/mvgam/dev/reference/shared_variation.md),
  [`compare_loadings()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_loadings.md),
  [`ordinate()`](https://nicholasjclark.github.io/mvgam/dev/reference/ordinate.md)
- AR factor models with estimated loadings give every draw one factor
  order and orientation.
  [`summary()`](https://rdrr.io/r/base/summary.html) reports their
  loadings as `Z` and their AR coefficients per factor
- [`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
  gained three arguments: `traits`, `trait_slopes`, `phylo`.
  [`lv_axis()`](https://nicholasjclark.github.io/mvgam/dev/reference/lv_axis.md)
  in a `trend_formula` term gives each latent factor, or each series of
  a model without factors, its own effect
- The ‘brms’ post-processing methods work on mvgam fits. Examples:
  `posterior_predict()`,
  [`log_lik()`](https://mc-stan.org/rstantools/reference/log_lik.html),
  [`update()`](https://rdrr.io/r/stats/update.html),
  [`hypothesis()`](https://paulbuerkner.com/brms/reference/hypothesis.brmsfit.html),
  `as_draws_*()`
- Model comparison:
  [`bridge_sampler()`](https://rdrr.io/pkg/bridgesampling/man/bridge_sampler.html),
  [`bayes_factor()`](https://rdrr.io/pkg/bridgesampling/man/bf.html),
  [`waic()`](https://mc-stan.org/loo/reference/waic.html),
  [`loo_model_weights()`](https://mc-stan.org/loo/reference/loo_model_weights.html),
  [`pp_average()`](https://paulbuerkner.com/brms/reference/pp_average.brmsfit.html),
  [`compare_scores()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_scores.md),
  [`compare_elpds()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_elpds.md)
- `threads`, `init = "pathfinder"`,
  [`mvgam_multiple()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_multiple.md)
  for imputed data, and helpers to pivot wide occupancy and species data
- [`methods_md()`](https://nicholasjclark.github.io/mvgam/dev/reference/methods_md.md)
  writes a model description.
  [`bibtex()`](https://nicholasjclark.github.io/mvgam/dev/reference/bibtex.md)
  extracts the citation
- `pp_check(type = "resid_variogram")` checks residuals by time lag.
  `plot(type = "residuals")` draws it for unevenly spaced series
- On a hierarchical VAR,
  [`stability()`](https://nicholasjclark.github.io/mvgam/dev/reference/stability.mvgam.md)
  reports each group and
  [`posterior_transition_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_transition_matrix.md)
  averages the group matrices. Plots of
  [`irf()`](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md)
  and
  [`fevd()`](https://nicholasjclark.github.io/mvgam/dev/reference/fevd.mvgam.md)
  select processes by name

### Changed defaults

- The `posterior_*()` prediction methods and
  [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) default to
  `process_error = FALSE`, matching
  [`predict()`](https://rdrr.io/r/stats/predict.html)
- [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) includes the
  fitted trend state for the training data, as
  [`residuals()`](https://rdrr.io/r/stats/residuals.html) does
- Predictions with `incl_autocor = TRUE` and
  [`log_lik()`](https://mc-stan.org/rstantools/reference/log_lik.html)
  refuse times outside the fitted times. Use
  [`forecast()`](https://generics.r-lib.org/reference/forecast.html)
- [`ZMVN()`](https://nicholasjclark.github.io/mvgam/dev/reference/ZMVN.md)
  and
  [`PW()`](https://nicholasjclark.github.io/mvgam/dev/reference/piecewise_trends.md)
  forecasts require every series to share one set of forecast times
- [`conditional_effects()`](https://paulbuerkner.com/brms/reference/conditional_effects.brmsfit.html)
  defaults to `type = "expected"`
- [`pp_check()`](https://nicholasjclark.github.io/mvgam/dev/reference/pp_check.mvgam.md)
  draws bars for a bernoulli or ordinal response and a rootogram for
  counts
- Trend scale priors take the response’s spread on the link scale as
  ‘brms’ defaults do.
  [`LLT()`](https://nicholasjclark.github.io/mvgam/dev/reference/LLT.md)
  and
  [`PW()`](https://nicholasjclark.github.io/mvgam/dev/reference/piecewise_trends.md)
  growth priors also scale with the series length
- [`PW()`](https://nicholasjclark.github.io/mvgam/dev/reference/piecewise_trends.md)
  `changepoint_scale` (default `0.3`) is a change in growth over the
  whole series
- [`CAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  keeps its autoregressive parameter inside `(0, 1)` and measures time
  in median observation gaps
- The
  [`com_binomial()`](https://nicholasjclark.github.io/mvgam/dev/reference/com_binomial.md)
  prior on `nu` is now `normal(1, 1)`
- A distributional regression on a
  [`tweedie()`](https://nicholasjclark.github.io/mvgam/dev/reference/tweedie.md),
  [`beta_nb()`](https://nicholasjclark.github.io/mvgam/dev/reference/beta_nb.md)
  or other custom family’s parameter gives its intercept a
  `student_t(3, 0, 2.5)` prior, as ‘brms’ does for its own families

### Deprecations

- `samples` and `burnin` (use `iter` and `warmup`), `lfo_cv(data = )`
  (use `newdata`), `incl_dynamics` (use `incl_autocor`) and `by = trend`
  (use `by = lv_axis()`)

### Bug fixes

- Binomial predictions with varying trial counts stay within each
  observation’s total (thanks to
  [@jbogomolovas2](https://github.com/jbogomolovas2))
- [`update()`](https://rdrr.io/r/stats/update.html) works when a trend
  constructor argument came from a variable
- [`prior_summary()`](https://mc-stan.org/rstantools/reference/prior_summary.html)
  reports the priors in the compiled Stan model
- [`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md)
  models accept missing responses
- [`ensemble()`](https://nicholasjclark.github.io/mvgam/dev/reference/ensemble.mvgam_forecast.md)
  combines
  [`hindcast()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.md)
  results
- [`CAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  process variance scales with the time lag
  ([\#107](https://github.com/nicholasjclark/mvgam/issues/107))
- `score(score = "brier")` scores the forecast probability, the mean of
  the draws
- `score(score = "variogram")` follows Scheuerer and Hamill (2015)
  through
  [`scoringRules::vs_sample()`](https://rdrr.io/pkg/scoringRules/man/scores_sample_multiv.html)
- Variational, Laplace and Pathfinder fits name their algorithm in
  [`summary()`](https://rdrr.io/r/base/summary.html) and
  [`glance()`](https://generics.r-lib.org/reference/glance.html)
- `algorithm = "laplace"` honours `silent = 2`
- Fixes to forecast scores with one test observation, offsets with
  `gp()` terms, piecewise trend predictions, by-factor Gaussian Process
  predictions, the `max_treedepth` check in
  [`summary()`](https://rdrr.io/r/base/summary.html) and plots of
  residuals with missing values (thanks to
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
  [`plot.mvgam_residcor()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_residcor.md)
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
