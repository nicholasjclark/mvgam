# Default plots for mvgam models

Draws one of the standard views of a fitted model, chosen by `type`.

## Usage

``` r
# S3 method for class 'mvgam'
plot(
  x,
  type = c("residuals", "smooths", "trend", "factors", "series", "latent_state"),
  series = NULL,
  resp = NULL,
  ndraws = NULL,
  component = NULL,
  ...
)
```

## Arguments

- x:

  A fitted `mvgam` object.

- type:

  The plot to draw:

  `"residuals"` (default)

  :   Randomised quantile (Dunn-Smyth) residuals of one series, defined
      in
      [`residuals.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/residuals.mvgam.md).
      The grid plots them against the fitted values, with their Q-Q
      plot, ACF and partial ACF, as
      [`pp_check.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/pp_check.mvgam.md)
      draws them. For a model that fits, they look like independent
      standard normal draws. A series observed at unevenly spaced times
      shows a semivariogram in place of the ACF and partial ACF. An
      [`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md)
      or
      [`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md)
      fit has one residual per closure unit and draws the Q-Q plot
      alone.

  `"smooths"`

  :   The posterior of each smooth, from
      [`conditional_smooths.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_smooths.mvgam.md),
      with several smooths combined into one figure.

  `"trend"`

  :   The latent trend over the training times, from
      [`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md),
      or the state component `component` names. A
      [`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
      fit has sites in place of times and refuses it.

  `"factors"`

  :   The path of each latent factor.
      [`active_factors()`](https://nicholasjclark.github.io/mvgam/dev/reference/active_factors.md)
      reports the variance each factor carries.

  `"series"`

  :   The observed data the fit carries.
      [`mvgam_data()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_data.md)
      plots data before a fit.

  `"latent_state"`

  :   The latent state of an
      [`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md)
      or
      [`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md)
      fit drawn from the covariates alone: occupancy `psi` or abundance
      `N`, by series and time. A multi-season fit pools the sites of
      each time.

- series:

  A series index or name. `"trend"` and `"series"` also take `"all"`,
  their default. `"residuals"` draws one series, the first by default.
  `"smooths"`, `"factors"` and `"latent_state"` ignore it.

- resp:

  Character specifying which response variable for multivariate models.
  NULL (default) returns predictions for all responses.

- ndraws:

  The number of posterior draws, or `NULL` for every draw. `"residuals"`
  takes 100 by default. `"factors"` and `"series"` ignore it.

- component:

  `NULL` (the default) for the summed trend, or the names of state
  components of the trend, with `type = "trend"`. Several names give the
  sum of their components.
  [`LLT()`](https://nicholasjclark.github.io/mvgam/dev/reference/LLT.md)
  has `"level"`, the stochastic level, and `"slope"`, its growth per
  time step. A seasonal
  [`LLT()`](https://nicholasjclark.github.io/mvgam/dev/reference/LLT.md)
  adds `"season"`. `"predictor"` holds the effects of the trend
  formula's covariates. `c("level", "predictor")` gives the level with
  those effects. A factor model projects each component through its
  loadings, as it projects the trend.

- ...:

  Further arguments to the function that draws the plot, such as
  `smooths` for
  [`conditional_smooths.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_smooths.mvgam.md).

## Value

A `ggplot` object, or a `patchwork` of several.

## See also

[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md)
and its [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method
draw forecasts.
[`conditional_effects.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_effects.mvgam.md)
draws covariate effects.
[`mcmc_plot.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mcmc_plot.mvgam.md)
draws posterior diagnostics.

Other model checking:
[`mvgam_diagnostics`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_diagnostics.md),
[`pp_check.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/pp_check.mvgam.md),
[`residuals.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/residuals.mvgam.md)

## Author

Nicholas J Clark

## Examples

``` r
# \dontrun{
set.seed(13)
simdat <- sim_mvgam(family = poisson(), n_series = 1L,
                     n_timepoints = 120L, trend_model = AR())
mod <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
              data    = simdat$data_train,
              family  = poisson(),
              chains  = 2, silent = 2)
#> Warning: 5 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# Residual diagnostics, the default
plot(mod)


# The smooth of x and the latent AR(1) trend
plot(mod, type = "smooths")

plot(mod, type = "trend")


# The slope of a local linear trend
mod_llt <- update(mod, trend_formula = ~ LLT())
#> Warning: 7 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
plot(mod_llt, type = "trend", component = "slope")


# Next, plot(forecast(mod, newdata = simdat$data_test)) carries the
# trend forward
# }
```
