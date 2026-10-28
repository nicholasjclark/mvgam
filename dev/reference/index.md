# Package index

## Model fitting

Entry points that compile a state-space model and sample it.

- [`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
  : Fit a Bayesian multivariate state-space model

- [`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
  : Fit Joint Species Distribution Models in mvgam

- [`mvgam_multiple()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_multiple.md)
  : Fit mvgam models to multiply imputed datasets

- [`update(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/update.mvgam.md)
  :

  Update a fitted mvgam model

- [`mvgam-class`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam-class.md)
  :

  Fitted `mvgam` object description

- [`print(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/print.mvgam.md)
  :

  Print an mvgam object

- [`inits(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/inits.mvgam.md)
  : Initial values of a fitted mvgam model

- [`control_params(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/control_params.mvgam.md)
  : Sampler control settings of a fitted mvgam model

- [`mvgam_use_cases`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_use_cases.md)
  :

  Example use cases for mvgam

- [`mvgam-package`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam-package.md)
  : mvgam: Multivariate (Dynamic) Generalized Additive Models

## Formula and trend specification

Building the observation formula and choosing a latent trend process.

- [`mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_formula.md)
  : Pair an observation formula with a trend formula

- [`print(`*`<mvgam_formula>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/print.mvgam_formula.md)
  : Print method for mvgam_formula objects

- [`formula(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/formula.mvgam.md)
  : Extract formula from mvgam object

- [`RW()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  [`AR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  [`CAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  [`VAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  :

  Latent trend models for mvgam

- [`LLT()`](https://nicholasjclark.github.io/mvgam/dev/reference/LLT.md)
  :

  Specify local linear trends in mvgam models

- [`PW()`](https://nicholasjclark.github.io/mvgam/dev/reference/piecewise_trends.md)
  : Piecewise linear or logistic trends

- [`ZMVN()`](https://nicholasjclark.github.io/mvgam/dev/reference/ZMVN.md)
  : Correlated latent effects with no time dependence

- [`lv_axis()`](https://nicholasjclark.github.io/mvgam/dev/reference/lv_axis.md)
  : Vary a trend effect along the trend's axis

- [`is.mvgam_trend()`](https://nicholasjclark.github.io/mvgam/dev/reference/is.mvgam_trend.md)
  : Whether an object is an mvgam trend

- [`print(`*`<mvgam_trend>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/print.mvgam_trend.md)
  : Print method for mvgam trend objects

- [`list_trend_types()`](https://nicholasjclark.github.io/mvgam/dev/reference/list_trend_types.md)
  : List Available Trend Types

- [`mvgam_trend_specification`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_trend_specification.md)
  : Fields of an mvgam_trend object

## Observation families

Families mvgam adds to those of brms, for counts, compositions,
occupancy and abundance.

- [`mvgam_families`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_families.md)
  : Observation families supported by mvgam

- [`tweedie()`](https://nicholasjclark.github.io/mvgam/dev/reference/tweedie.md)
  : Tweedie compound Poisson-gamma family for mvgam

- [`beta_nb()`](https://nicholasjclark.github.io/mvgam/dev/reference/beta_nb.md)
  : Beta negative binomial family for heavy-tailed counts

- [`com_binomial()`](https://nicholasjclark.github.io/mvgam/dev/reference/com_binomial.md)
  : Conway-Maxwell-Binomial family for over-, equi-, under-, and
  super-dispersed bounded counts

- [`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md)
  : N-mixture family for repeated counts of unmarked individuals

- [`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md)
  : Occupancy family for repeated detection and non-detection

- [`diri()`](https://nicholasjclark.github.io/mvgam/dev/reference/diri.md)
  : Dirichlet family for compositions

- [`multi()`](https://nicholasjclark.github.io/mvgam/dev/reference/multi.md)
  : Multinomial family for counts over several categories

- [`categ()`](https://nicholasjclark.github.io/mvgam/dev/reference/categ.md)
  : Categorical family for one category observed per unit

- [`mvn()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvn.md)
  : Multivariate normal family for several continuous responses

- [`mvt()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvt.md)
  : Multivariate Student-t family for several continuous responses

- [`family(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/family.mvgam.md)
  : Extract family from mvgam object

- [`check_tweedie_truncation()`](https://nicholasjclark.github.io/mvgam/dev/reference/check_tweedie_truncation.md)
  :

  Diagnose whether the Tweedie series-truncation `M` is adequate

- [`latent_N_saturation()`](https://nicholasjclark.github.io/mvgam/dev/reference/latent_N_saturation.md)
  : How often each unit's latent state reaches its cap

## Priors

Inspecting the default prior set and overriding it before a fit.

- [`get_prior()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.md)
  [`default_prior()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.md)
  : The priors of a model
- [`get_prior(`*`<default>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.default.md)
  [`default_prior(`*`<default>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.default.md)
  : The priors of a model formula
- [`get_prior(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.mvgam.md)
  [`default_prior(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.mvgam.md)
  : The prior table of an mvgam model
- [`get_prior(`*`<mvgam_formula>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.mvgam_formula.md)
  [`default_prior(`*`<mvgam_formula>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.mvgam_formula.md)
  : The priors an mvgam model takes, before fitting

## Data preparation and validation

Checking that long-format data carry the columns, levels and response
shape a proposed family expects.

- [`mvgam_data()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_data.md)
  :

  Inspect long-format data before fitting an `mvgam`

- [`check_newdata()`](https://nicholasjclark.github.io/mvgam/dev/reference/check_newdata.md)
  :

  Validate `newdata` against the training data of an mvgam model

- [`series_to_mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/series_to_mvgam.md)
  : Convert time series objects to mvgam long format

- [`pivot_detection_array()`](https://nicholasjclark.github.io/mvgam/dev/reference/pivot_detection_array.md)
  : Convert a detection array to the long format of mvgam

- [`pivot_species_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/pivot_species_matrix.md)
  : Pivot a wide species matrix to mvgam long-format

## Simulation

Generative recipes that return the truths they drew from, for recovery
checks.

- [`sim_mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/sim_mvgam.md)
  :

  Simulate `mvgam`-shaped time-series data with known truth

- [`print(`*`<mvgam_sim>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/print.mvgam_sim.md)
  :

  Print method for `mvgam_sim`

- [`summary(`*`<mvgam_sim>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam_sim.md)
  :

  Summary for
  [`sim_mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/sim_mvgam.md)
  output

- [`print(`*`<mvgam_sim_summary>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/print.mvgam_sim_summary.md)
  :

  Print method for `mvgam_sim_summary`

- [`sim_closure_unit_data()`](https://nicholasjclark.github.io/mvgam/dev/reference/sim_closure_unit_data.md)
  : Simulate closure-unit detection data with known truth

- [`summary(`*`<mvgam_sim_closure_unit>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam_sim_closure_unit.md)
  : Summary method for sim_closure_unit_data() output

- [`print(`*`<mvgam_sim_closure_unit_summary>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/print.mvgam_sim_closure_unit_summary.md)
  : Print method for sim_closure_unit_data() summary output

- [`sim_jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/sim_jsdgam.md)
  : Simulate multi-species data for a joint species distribution model

- [`summary(`*`<mvgam_sim_jsdgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam_sim_jsdgam.md)
  : Summary method for sim_jsdgam() output

- [`print(`*`<mvgam_sim_jsdgam_summary>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/print.mvgam_sim_jsdgam_summary.md)
  : Print method for sim_jsdgam() summary output

## Prediction and forecasting

Forecasts carry the fitted trend forward in time. Predictions from
covariates leave the fitted trend out.

- [`predict(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/predict.mvgam.md)
  : Posterior predictions from an mvgam model

- [`fitted(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/fitted.mvgam.md)
  : Expected values of an mvgam model

- [`posterior_epred(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_epred.mvgam.md)
  : Expected values of the response for each posterior draw

- [`posterior_linpred(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_linpred.mvgam.md)
  : Linear predictor draws for each posterior draw

- [`posterior_predict(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_predict.mvgam.md)
  : Posterior predictive draws of the response

- [`hindcast()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.md)
  : Generic for posterior hindcasts

- [`hindcast(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md)
  : Posterior hindcasts for an mvgam state-space model

- [`forecast(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md)
  : Posterior forecasts for an mvgam state-space model

- [`mvgam_forecast-class`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_forecast-class.md)
  :

  `mvgam_forecast` object description

- [`print(`*`<mvgam_forecast>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/print.mvgam_forecast.md)
  :

  Compact summary of an `mvgam_forecast` object

- [`summary(`*`<mvgam_forecast>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam_forecast.md)
  : Posterior summary of hindcast and forecast objects

- [`plot(`*`<mvgam_forecast>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_forecast.md)
  : Plot posterior hindcasts and forecasts

- [`print(`*`<mvgam_latent_state>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/print.mvgam_latent_state.md)
  :

  Short print for a `mvgam_latent_state` object

- [`summary(`*`<mvgam_latent_state>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam_latent_state.md)
  :

  Per-unit posterior summary for a `mvgam_latent_state` object

- [`plot(`*`<mvgam_latent_state>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_latent_state.md)
  : Plot the posterior latent state

- [`ensemble()`](https://nicholasjclark.github.io/mvgam/dev/reference/ensemble.mvgam_forecast.md)
  :

  Combine forecasts from mvgam models into a weighted ensemble

- [`posterior_average(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_average.mvgam.md)
  [`pp_average(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_average.mvgam.md)
  :

  Model-averaged posterior draws and predictions across mvgam fits

- [`posterior_interval(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_interval.mvgam.md)
  :

  Posterior credible intervals on a fitted mvgam model

- [`predictive_interval(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/predictive_interval.mvgam.md)
  :

  Posterior predictive intervals on a fitted mvgam model

- [`predictive_error(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/predictive_error.mvgam.md)
  :

  Posterior predictive errors on a fitted mvgam model

## Model criticism and cross-validation

Posterior predictive checks, residuals, leave-one-out criteria and
cross-validation of forecasts.

- [`pp_check(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/pp_check.mvgam.md)
  :

  Posterior predictive checks for mvgam models

- [`print(`*`<mvgam_ppc_fit_stat>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/print.mvgam_ppc_fit_stat.md)
  : Print a closure-unit GOF discrepancy result

- [`plot(`*`<mvgam_ppc_fit_stat>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_ppc_fit_stat.md)
  : Plot a closure-unit GOF discrepancy result

- [`residuals(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/residuals.mvgam.md)
  :

  Posterior draws of residuals from mvgam models

- [`log_lik(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/log_lik.mvgam.md)
  : Pointwise log-likelihoods from a fitted mvgam model

- [`logLik(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/logLik.mvgam.md)
  : Log-likelihood of a fitted mvgam model

- [`loo(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/loo.mvgam.md)
  [`loo_compare(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/loo.mvgam.md)
  :

  Approximate leave-one-out cross-validation for mvgam models

- [`waic(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/waic.mvgam.md)
  : Widely applicable information criterion for mvgam models

- [`LOO(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_loo_extras.md)
  [`WAIC(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_loo_extras.md)
  [`loo_R2(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_loo_extras.md)
  [`loo_predict(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_loo_extras.md)
  [`loo_epred(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_loo_extras.md)
  [`loo_linpred(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_loo_extras.md)
  [`loo_predictive_interval(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_loo_extras.md)
  [`loo_subsample(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_loo_extras.md)
  [`loo_moment_match(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_loo_extras.md)
  [`loo_model_weights(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_loo_extras.md)
  [`add_criterion(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_loo_extras.md)
  : Leave-one-out predictions, R-squared and criteria for mvgam models

- [`bridge_sampler(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/bridge_sampler.mvgam.md)
  [`bayes_factor(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/bridge_sampler.mvgam.md)
  :

  Bridge sampling and Bayes factors for `mvgam` fits

- [`kfold(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/kfold.mvgam.md)
  :

  K-fold cross-validation for `mvgam` fits

- [`print(`*`<mvgam_kfold>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/print.mvgam_kfold.md)
  :

  Print an `mvgam_kfold` object

- [`summary(`*`<mvgam_kfold>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam_kfold.md)
  :

  Tibble summary of an `mvgam_kfold` object

- [`plot(`*`<mvgam_kfold>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_kfold.md)
  :

  Diagnostic plot for an `mvgam_kfold` object

- [`lfo_cv()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md)
  : Approximate leave-future-out cross-validation of fitted mvgam
  objects

- [`print(`*`<mvgam_lfo>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/print.mvgam_lfo.md)
  :

  Print headline summary of an `mvgam_lfo` object

- [`summary(`*`<mvgam_lfo>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam_lfo.md)
  :

  Posterior summary of an `mvgam_lfo` object

- [`plot(`*`<mvgam_lfo>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_lfo.md)
  :

  Plot Pareto-k and per-fold scores from an `mvgam_lfo` object

- [`loo_compare(`*`<mvgam_lfo>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/loo_compare.mvgam_lfo.md)
  : Compare leave-future-out evaluations across models

- [`loo_model_weights(`*`<mvgam_lfo>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/loo_model_weights.mvgam_lfo.md)
  :

  Model weights from `mvgam_lfo` objects

- [`score()`](https://nicholasjclark.github.io/mvgam/dev/reference/score.md)
  : Generic for posterior forecast scoring

- [`score(`*`<mvgam_forecast>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/score.mvgam_forecast.md)
  :

  Proper scoring rules for `mvgam_forecast` objects

- [`compare_scores()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_scores.md)
  : Tidy comparison of forecast scores across mvgam models

- [`summary(`*`<mvgam_compare_scores>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam_compare_scores.md)
  :

  Wide-format summary of a
  [`compare_scores()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_scores.md)
  table

- [`plot(`*`<mvgam_compare_scores>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_compare_scores.md)
  :

  Plot per-model score trajectories from
  [`compare_scores()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_scores.md)

- [`compare_elpds()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_elpds.md)
  : Tidy comparison of leave-future-out ELPDs across mvgam_lfo runs

- [`plot(`*`<mvgam_compare_elpds>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_compare_elpds.md)
  :

  Plot per-step LFO ELPDs from
  [`compare_elpds()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_elpds.md)

## Posterior summaries and diagnostics

Parameter summaries and convergence checks for a fit. The Stan program
and data that produced it.

- [`summary(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam.md)
  : Summary of mvgam model fits

- [`print(`*`<mvgam_summary>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/print.mvgam_summary.md)
  : Print method for mvgam_summary objects

- [`summary(`*`<mvgam_pooled>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam_pooled.md)
  [`print(`*`<mvgam_pooled_summary>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam_pooled.md)
  : Summary of a fit pooled over imputations

- [`tidy(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/tidy.mvgam.md)
  :

  Tidy an `mvgam` object's parameter posteriors

- [`glance(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/glance.mvgam.md)
  :

  Model summary for an `mvgam` fit

- [`augment(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/augment.mvgam.md)
  :

  Augment an `mvgam` object's training data with fitted values and
  residuals

- [`as.data.frame(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_draws.md)
  [`as.matrix(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_draws.md)
  [`as.array(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_draws.md)
  [`as_draws(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_draws.md)
  [`as_draws_matrix(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_draws.md)
  [`as_draws_df(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_draws.md)
  [`as_draws_array(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_draws.md)
  [`as_draws_list(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_draws.md)
  [`as_draws_rvars(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_draws.md)
  :

  Extract posterior draws from fitted mvgam objects

- [`coef(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_diagnostics.md)
  [`nuts_params(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_diagnostics.md)
  [`log_posterior(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_diagnostics.md)
  [`rhat(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_diagnostics.md)
  [`neff_ratio(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_diagnostics.md)
  [`fixef(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_diagnostics.md)
  [`bayes_R2(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_diagnostics.md)
  [`prior_summary(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_diagnostics.md)
  [`ndraws(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_diagnostics.md)
  [`nchains(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_diagnostics.md)
  [`niterations(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_diagnostics.md)
  [`nvariables(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_diagnostics.md)
  [`posterior_summary(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_diagnostics.md)
  [`getCall(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_diagnostics.md)
  [`vcov(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_diagnostics.md)
  : Posterior summaries and sampler diagnostics of mvgam models

- [`variables(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/index-mvgam.md)
  : Parameter names of a fitted mvgam model

- [`ranef(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/ranef.mvgam.md)
  :

  Group-level (random) effects of a fitted mvgam model

- [`VarCorr(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/VarCorr.mvgam.md)
  : Standard deviations and correlations of group-level effects

- [`ngrps(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/ngrps.mvgam.md)
  :

  Number of levels per grouping factor in a fitted mvgam model

- [`nobs(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/nobs.mvgam.md)
  : Extract number of observations from mvgam object

- [`model.frame(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/model.frame.mvgam.md)
  : Extract a model frame from a fitted mvgam object

- [`terms(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/terms.mvgam.md)
  : Model terms for a fitted mvgam object

- [`smooths()`](https://nicholasjclark.github.io/mvgam/dev/reference/smooths.mvgam.md)
  : The smooth terms of a fitted model

- [`posterior_smooths(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_smooths.mvgam.md)
  : Posterior draws of one smooth term

- [`conditional_smooths(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_smooths.mvgam.md)
  [`plot(`*`<mvgam_conditional_smooths>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_smooths.mvgam.md)
  [`print(`*`<mvgam_conditional_smooths>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_smooths.mvgam.md)
  : Plots of each smooth term

- [`hypothesis(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/hypothesis.mvgam.md)
  :

  Test posterior hypotheses on a fitted mvgam model

- [`stancode(`*`<mvgam_formula>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/stancode.mvgam_formula.md)
  [`standata(`*`<mvgam_formula>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/stancode.mvgam_formula.md)
  [`stancode(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/stancode.mvgam_formula.md)
  [`standata(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/stancode.mvgam_formula.md)
  : Stan code and data of an mvgam model

- [`stancode(`*`<formula>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_formula_generation.md)
  [`stancode(`*`<bform>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_formula_generation.md)
  [`standata(`*`<formula>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_formula_generation.md)
  [`standata(`*`<bform>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_formula_generation.md)
  :

  Stan program and data from a formula carrying a `trend_formula`

- [`print(`*`<mvgamstancode>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/print.mvgamstancode.md)
  : Print the Stan program of an mvgam model

## Reporting a model

Methods text for a fitted model. Each tool writes one part: the
paragraph with its references, the equations with their priors or the
BibTeX file.

- [`how_to_cite()`](https://nicholasjclark.github.io/mvgam/dev/reference/how_to_cite.mvgam.md)
  :

  A methods paragraph and references for an mvgam model

- [`methods_md()`](https://nicholasjclark.github.io/mvgam/dev/reference/methods_md.md)
  : The equations of a model, as Markdown

- [`bibtex()`](https://nicholasjclark.github.io/mvgam/dev/reference/bibtex.md)
  :

  The BibTeX entries of a `how_to_cite` object

## Marginal and conditional effects

Covariate effect summaries through the marginaleffects and insight
interfaces.

- [`conditional_effects(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_effects.mvgam.md)
  [`plot(`*`<mvgam_conditional_effects>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_effects.mvgam.md)
  [`print(`*`<mvgam_conditional_effects>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_effects.mvgam.md)
  [`as.data.frame(`*`<mvgam_conditional_effects>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_effects.mvgam.md)
  : Display conditional effects of predictors for mvgam models
- [`mvgam_marginaleffects`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_marginaleffects.md)
  : marginaleffects methods for mvgam objects
- [`mvgam_insight`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_insight.md)
  : insight S3 methods for mvgam objects

## Interrogating a fitted VAR

Impulse responses, variance decompositions and stability measures
computed from the posterior of a VAR trend.

- [`irf()`](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md)
  : Calculate latent VAR impulse response functions

- [`mvgam_irf-class`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_irf-class.md)
  :

  `mvgam_irf` object description

- [`summary(`*`<mvgam_irf>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam_irf.md)
  : Posterior summary of impulse responses

- [`plot(`*`<mvgam_irf>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_irf.md)
  :

  Plot impulse responses from an `mvgam_irf` object

- [`fevd()`](https://nicholasjclark.github.io/mvgam/dev/reference/fevd.mvgam.md)
  : Calculate latent VAR forecast error variance decompositions

- [`mvgam_fevd-class`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_fevd-class.md)
  :

  `mvgam_fevd` object description

- [`summary(`*`<mvgam_fevd>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam_fevd.md)
  : Posterior summary of forecast error variance decompositions

- [`plot(`*`<mvgam_fevd>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_fevd.md)
  :

  Plot forecast error variance decompositions from an `mvgam_fevd`
  object

- [`stability()`](https://nicholasjclark.github.io/mvgam/dev/reference/stability.mvgam.md)
  : Calculate measures of latent VAR community stability

- [`summary(`*`<mvgam_stability>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam_stability.md)
  : Summarise posterior stability metrics

- [`plot(`*`<mvgam_stability>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_stability.md)
  : Plot posterior distributions of latent VAR stability metrics

- [`plot(`*`<mvgam_stability_summary>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_stability_summary.md)
  : Plot summarised stability metrics

- [`plot(`*`<mvgam_var_surface_summary>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_var_surface_summary.md)
  : Plot a summarised impulse response or variance decomposition

- [`posterior_transition_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_transition_matrix.md)
  : Posterior transition matrix of a VAR trend

- [`print(`*`<mvgam_var_matrix>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/print.mvgam_var_matrix.md)
  : Print a VAR transition matrix

- [`print(`*`<mvgam_var_matrix_list>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/print.mvgam_var_matrix_list.md)
  : Print the VAR transition matrices of several panels

- [`plot(`*`<mvgam_var_matrix>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_var_matrix.md)
  : Plot a VAR transition matrix

- [`plot(`*`<mvgam_var_matrix_list>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_var_matrix_list.md)
  : Plot the VAR transition matrices of several panels

## Latent factors and joint species distribution models

Ordinations, loading comparisons, factor activity and the residual
correlations a factor model implies.

- [`ordinate()`](https://nicholasjclark.github.io/mvgam/dev/reference/ordinate.md)
  : Ordination biplot of a latent factor model

- [`active_factors()`](https://nicholasjclark.github.io/mvgam/dev/reference/active_factors.md)
  : Posterior count of the latent factors a model uses

- [`plot(`*`<mvgam_active_factors>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_active_factors.md)
  : Plot the posterior active-factor probabilities

- [`compare_loadings()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_loadings.md)
  : Compare posterior loadings between two latent-factor fits

- [`shared_variation()`](https://nicholasjclark.github.io/mvgam/dev/reference/shared_variation.md)
  : Posterior summary of the factor-implied shared variation matrix

- [`plot(`*`<mvgam_shared_variation>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_shared_variation.md)
  : Plot the factor-implied shared variation matrix

- [`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md)
  : Residual correlations between the series of a fitted model

- [`mvgam_residcor-class`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_residcor-class.md)
  :

  `mvgam_residcor` object description

- [`print(`*`<mvgam_residcor>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/print.mvgam_residcor.md)
  :

  Print a concise overview of an `mvgam_residcor` object

- [`summary(`*`<mvgam_residcor>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam_residcor.md)
  :

  Tidy summary of an `mvgam_residcor` object

- [`plot(`*`<mvgam_residcor>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_residcor.md)
  :

  Plot residual correlations or precisions from a `mvgam_residcor`
  object

- [`plot(`*`<mvgam_residcor_list>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_residcor_list.md)
  :

  Plot method for an `mvgam_residcor_list`

- [`trait_slopes_helpers`](https://nicholasjclark.github.io/mvgam/dev/reference/trait_slopes_helpers.md)
  :

  Trait-mediated environmental slopes for
  [`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)

- [`sample_innovations`](https://nicholasjclark.github.io/mvgam/dev/reference/sample_innovations.md)
  : Process error innovation sampling for trend models

## Plotting

Plot methods for a fitted model. mvgam_theme() styles every figure.

- [`plot(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam.md)
  :

  Default plots for mvgam models

- [`mcmc_plot(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/mcmc_plot.mvgam.md)
  : MCMC plots of mvgam parameters with bayesplot

- [`pairs(`*`<mvgam>`*`)`](https://nicholasjclark.github.io/mvgam/dev/reference/pairs.mvgam.md)
  : Pairs plot of mvgam parameters

- [`mvgam_theme()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_theme.md)
  : The ggplot theme mvgam draws with

## Datasets

Example data used across the vignettes, articles and examples.

- [`birdsong`](https://nicholasjclark.github.io/mvgam/dev/reference/birdsong.md)
  : Simulated weekly bird song counts with hierarchically correlated
  AR(1) trends
- [`portal_data`](https://nicholasjclark.github.io/mvgam/dev/reference/portal_data.md)
  : Portal Project rodent capture survey data
- [`coral_surveys`](https://nicholasjclark.github.io/mvgam/dev/reference/coral_surveys.md)
  : Simulated coral bleaching index with irregular CAR(1) state
- [`lake_chemistry`](https://nicholasjclark.github.io/mvgam/dev/reference/lake_chemistry.md)
  : Simulated lake chemistry with correlated random-walk state

## Re-exports

Generics re-exported from brms, posterior, loo and marginaleffects.

- [`reexports`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`as_draws`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`as_draws_matrix`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`as_draws_df`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`as_draws_array`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`as_draws_list`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`as_draws_rvars`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`posterior_interval`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`predictive_interval`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`ngrps`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`predictive_error`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`log_lik`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`loo`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`loo_compare`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`predictions`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`avg_predictions`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`plot_predictions`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`slopes`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`avg_slopes`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`plot_slopes`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`comparisons`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`avg_comparisons`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`plot_comparisons`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`datagrid`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`hypotheses`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`conditional_effects`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`mcmc_plot`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`posterior_smooths`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`conditional_smooths`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`posterior_average`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`pp_average`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`ranef`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`VarCorr`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`Beta`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`LOO`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`WAIC`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`add_criterion`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`as.brmsprior`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`bayes_R2`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`bayes_factor`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`bernoulli`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`beta_binomial`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`bf`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`bridge_sampler`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`brmsfamily`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`brmsformula`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`constant`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`control_params`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`custom_family`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`empty_prior`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`exponential`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`fixef`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`hypothesis`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`inits`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`is.brmsformula`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`is.brmsprior`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`is.mvbrmsformula`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`lognormal`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`loo_R2`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`loo_epred`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`loo_linpred`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`loo_predict`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`loo_predictive_interval`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`make_stancode`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`make_standata`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`mvbrmsformula`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`negbinomial`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`posterior_summary`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`prior`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`prior_`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`prior_draws`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`prior_samples`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`prior_string`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`prior_summary`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`set_prior`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`stancode`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`standata`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`stanvar`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`student`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`validate_prior`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`log_posterior`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`neff_ratio`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`nuts_params`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`forecast`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`loo_model_weights`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`loo_moment_match`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`loo_subsample`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`nchains`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`ndraws`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`niterations`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`nvariables`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`rhat`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`tidy`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`augment`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`glance`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  [`waic`](https://nicholasjclark.github.io/mvgam/dev/reference/reexports.md)
  : Objects exported from other packages
