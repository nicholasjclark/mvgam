# Example use cases for mvgam

New to mvgam? Start with
[`mvgam`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md),
whose Workflow section lists the steps of an analysis and whose Terms
section defines the vocabulary these pages use.
[`mvgam_data`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_data.md)
checks a data frame before fitting. The [overview
article](https://nicholasjclark.github.io/mvgam/dev/articles/mvgam_overview.html)
walks through a full analysis.

mvgam fits dynamic generalized additive models (GAMs) to one or many
time series. Smooth functions of covariates sit beside a latent
time-series process that carries the autocorrelation the covariates
leave behind. Stan samples the posterior. Every prediction and forecast
then carries the uncertainty of every parameter.

The links below lead to questions, blog posts, articles and videos where
these models answered a practical problem. They cover counts and other
non-Gaussian series, several series modelled together, and predictors
such as seasonality that changes over time, monotonic effects and
Gaussian processes.

## Details

**Non-Gaussian time series modelling and forecasting**

Counts, proportions and binary outcomes rarely suit a Gaussian model. A
latent trend on the link scale handles their autocorrelation.

- [Uncertain serial autocorrelation in GAM count model
  residuals](https://stats.stackexchange.com/questions/657495)

- [Fitting an autoregressive model and Poisson process
  interdependently](https://discourse.mc-stan.org/t/fitting-an-autoregressive-model-and-poisson-process-interdependently/37268)

- [Cyclical residual patterns and variable selection in
  GAMs](https://stats.stackexchange.com/questions/652174)

- [Causality between two binary time
  series](https://stats.stackexchange.com/questions/437125)

- [Logistic regression on time series
  data](https://stats.stackexchange.com/questions/285100)

- [Autocorrelation for unevenly spaced time
  series](https://discourse.mc-stan.org/t/autocorrelation-for-unevenly-spaced-time-series/10001)

- [Visualising autocorrelation in irregularly spaced count
  data](https://stats.stackexchange.com/questions/664160)

- [Blog post: State-Space Vector Autoregressions in
  `mvgam`](https://ecogambler.netlify.app/blog/vector-autoregressions/)

- [Article: State-space vector autoregressions with
  `mvgam`](https://nicholasjclark.github.io/mvgam/dev/articles/var.html)

- [Video tutorial: Ecological forecasting with Dynamic Generalized
  Additive Models](https://www.youtube.com/watch?v=0zZopLlomsQ&t=4s)

**Multivariate time series modelling and forecasting**

Series measured together, such as several species at one site, can share
latent trends or have trends that are correlated.

- [Ecological modelling: multivariate abundance time-series
  data](https://stats.stackexchange.com/questions/172645)

- [Relationships between species in multivariate
  models](https://discourse.mc-stan.org/t/account-for-relationships-between-species-in-a-multivariate-brms-model/32566)

- [Confirmatory factor analysis using
  `brms`](https://discourse.mc-stan.org/t/confirmatory-factor-analysis-using-brms/23139)

- [Chains stuck in a local optimum: correlated Poisson
  distributions](https://discourse.mc-stan.org/t/chains-stuck-in-a-local-optimum-correlated-poisson-distributions/37414)

- [Blog post: Hierarchical distributed lag models in `mgcv` and
  `mvgam`](https://ecogambler.netlify.app/blog/distributed-lags-mgcv/)

- [Article: One shared process, many sensors: multi-response
  `mvgam`](https://nicholasjclark.github.io/mvgam/dev/articles/mvbf.html)

- [Video tutorial: Time series in R and Stan using the `mvgam` package:
  hierarchical GAMs](https://www.youtube.com/watch?v=2POK_FVwCHk)

**Seasonality and other complex predictors**

Cyclic smooths model seasonal patterns. Gaussian processes, monotonic
smooths and hierarchical smooths model other nonlinear effects.

- [Gaussian process smoothers (`bs = "gp"`) in
  GAMs](https://stats.stackexchange.com/questions/478384)

- [Fitting a GAM with double seasonality to a daily time
  series](https://stats.stackexchange.com/questions/612312)

- [Simulating time series with different seasonal
  effects](https://stats.stackexchange.com/questions/648143)

- [Adding time as a monotone
  predictor](https://discourse.mc-stan.org/t/adding-time-as-monotne-predictor/37109)

- [Blog post: Incorporating time-varying seasonality in forecast
  models](https://ecogambler.netlify.app/blog/time-varying-seasonality/)

- [Video tutorial: Time series in R and Stan using the `mvgam` package:
  an introduction](https://www.youtube.com/watch?v=fzPJUW8x6DU)

## See also

[`mvgam`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md),
[`mvgam_data`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_data.md),
[`trend_constructors`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)

## Author

Nicholas J Clark
