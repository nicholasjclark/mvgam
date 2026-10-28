# Example use cases for mvgam

mvgam is a package for fitting dynamic generalized additive models
(GAMs) to univariate or multivariate data. It combines the flexibility
of smooth functions with latent temporal processes to model
autocorrelation, seasonality, and uncertainty. The package supports both
univariate and multivariate time series, making it especially useful for
ecological and environmental forecasting. Bayesian inference via Stan
allows for full uncertainty quantification and forecasting in complex,
non-Gaussian settings.

This help page provides external links to example applications and
discussions relevant to the use of mvgam models. These examples span
non-Gaussian time series modelling, multivariate abundance forecasting,
and the use of complex predictors such as time-varying seasonality,
monotonic nonlinear effects and Gaussian processes.

## Details

**Non-Gaussian time series modelling and forecasting**

mvgam is designed for real-world time series data that include discrete,
zero-inflated, or overdispersed observations. It supports latent dynamic
components and smooth terms to model autocorrelation, trends, and
uncertainty.

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
  `mvgam`](https://nicholasjclark.github.io/mvgam/articles/var.html)

- [Video tutorial: Ecological forecasting with Dynamic Generalized
  Additive Models](https://www.youtube.com/watch?v=0zZopLlomsQ&t=4s)

**Multivariate time series modelling and forecasting**

mvgam supports multivariate models with shared or correlated latent
trends, making it suitable for a broad range of applications that gather
data on multiple time series simultaneously.

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
  `mvgam`](https://nicholasjclark.github.io/mvgam/articles/mvbf.html)

- [Video tutorial: Time series in R and Stan using the `mvgam` package:
  hierarchical GAMs](https://www.youtube.com/watch?v=2POK_FVwCHk)

**Seasonality and other complex predictors**

mvgam allows for flexible modelling of seasonal patterns and nonlinear
effects using cyclic smooths, Gaussian processes, monotonic smooths and
hierarchical structures.

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

## Author

Nicholas J Clark
