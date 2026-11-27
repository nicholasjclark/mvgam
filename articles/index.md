# Articles

### Getting started

Prepare your data and fit a first state-space model.

- [Overview of the mvgam
  package](https://nicholasjclark.github.io/mvgam/articles/mvgam_overview.md):

  An overview of mvgam, an R package for Bayesian State-Space models
  that pairs a brms observation model with a latent dynamic process.

- [Formatting data for
  mvgam](https://nicholasjclark.github.io/mvgam/articles/data.md):

  Format data for mvgam in R: long format with one row per series and
  time, missing values, factor levels and forecast data.

### Latent dynamics

Latent trends that model dependence over time. The articles also
interpret a fitted trend.

- [State-space vector autoregressions with
  mvgam](https://nicholasjclark.github.io/mvgam/articles/var.md):

  Fit State-Space vector autoregressions to multivariate time series in
  R with mvgam, with impulse responses and variance decompositions.

- [Hierarchical vector autoregressions with
  mvgam](https://nicholasjclark.github.io/mvgam/articles/hierarchical_var.md):

  Fit hierarchical panel vector autoregressions in R with mvgam, sharing
  dynamics across groups such as countries while each group keeps its
  own.

- [Dynamic factor
  models](https://nicholasjclark.github.io/mvgam/articles/dfm.md):

  Fit dynamic factor models in R with mvgam, where a few latent trends
  drive many observed time series.

### Many responses, one process

Sharing a latent state across outcomes measured on different scales,
across species, and across survey protocols.

- [One latent process, several observation
  models](https://nicholasjclark.github.io/mvgam/articles/mvbf.md):

  Fit several observation models to one latent process in R with mvgam.
  Every monitoring method then informs a single estimate of abundance.

- [Joint species distribution models with
  mvgam](https://nicholasjclark.github.io/mvgam/articles/jsdgam.md):

  Fit joint species distribution models in R with mvgam, using latent
  factors for residual species correlations and ordination.

- [Integrated distribution models with
  mvgam](https://nicholasjclark.github.io/mvgam/articles/idm.md):

  Fit integrated distribution models in R with mvgam, linking counts,
  occupancy surveys and sightings to one shared model of abundance.

### Imperfect detection

Closure-unit families that separate what is there from what was seen.

- [Occupancy models with
  mvgam](https://nicholasjclark.github.io/mvgam/articles/occupancy.md):

  Fit multi-season occupancy models in R with mvgam for species detected
  imperfectly, with covariates on occupancy and detection.

- [N-mixture models with
  mvgam](https://nicholasjclark.github.io/mvgam/articles/nmix.md):

  Fit N-mixture models in R with mvgam to separate abundance from
  imperfect detection in repeated counts, including dynamic abundance
  over time.

### Evaluating forecasts

Scoring rules, rolling-origin cross-validation and ensembles.

- [Forecast evaluation in
  mvgam](https://nicholasjclark.github.io/mvgam/articles/forecast_evaluation.md):

  Score and compare the forecasts of Bayesian time series models in R
  with proper scoring rules, using mvgam and observations held out of
  the fit.
