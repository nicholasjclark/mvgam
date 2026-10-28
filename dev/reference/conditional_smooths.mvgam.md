# Plots of each smooth term

Summarises the posterior of every smooth over a grid of its own
variables, in the shape
[`brms::conditional_smooths()`](https://paulbuerkner.com/brms/reference/conditional_smooths.brmsfit.html)
returns. Its [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
method draws a smooth of one variable as a line and ribbon, and a smooth
of two as a heatmap.

## Usage

``` r
# S3 method for class 'mvgam'
conditional_smooths(
  x,
  smooths = NULL,
  int_conditions = NULL,
  prob = 0.95,
  spaghetti = FALSE,
  surface = TRUE,
  facets = 3L,
  resolution = 100L,
  too_far = 0,
  ndraws = NULL,
  draw_ids = NULL,
  resp = NULL,
  ...
)

# S3 method for class 'mvgam_conditional_smooths'
plot(x, plot = TRUE, ask = FALSE, ...)

# S3 method for class 'mvgam_conditional_smooths'
print(x, ...)
```

## Arguments

- x:

  An object of class `mvgam_conditional_smooths` returned by
  `conditional_smooths.mvgam()`.

- smooths:

  The terms to summarise, as
  [`smooths.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/smooths.mvgam.md)
  lists them, or `NULL` for every smooth of every predictor, the trend's
  included.

- int_conditions:

  A named list setting the grid of a variable: a vector of values, or a
  function of the variable's training values, such as
  [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html).

- prob:

  A number in `[0, 1]`: the mass of the interval in `lower__` and
  `upper__`.

- spaghetti:

  Logical. `TRUE` keeps the draws of a smooth of one variable, which the
  plot draws as one line each.

- surface:

  Logical. `TRUE` evaluates a smooth of two variables on a `resolution`
  by `resolution` grid, drawn as a heatmap. `FALSE` draws lines over the
  first variable at `facets` values of the second.

- facets:

  An integer of at least 2: the number of values taken by the second
  variable and by a numeric `by` variable. They run evenly from one
  standard deviation below the mean to one above.

- resolution:

  An integer of at least 2: the number of grid points along each
  variable.

- too_far:

  A number in `[0, 1]`. On a surface, grid points further than this from
  the training data, in the unit square, are dropped by
  [`mgcv::exclude.too.far()`](https://rdrr.io/pkg/mgcv/man/exclude.too.far.html).
  `0` keeps every point.

- ndraws, draw_ids:

  The number of posterior draws, or their indices. Give at most one.
  `NULL` for both uses every draw.

- resp:

  The response whose smooths to summarise, or `NULL` for every response.

- ...:

  Unused. Anything passed here is refused.

- plot:

  Logical. `TRUE` draws each panel. `FALSE` only returns them.

- ask:

  Logical. `TRUE` prompts before each panel after the first.

## Value

A list of class `mvgam_conditional_smooths`, one data frame per smooth,
named by predictor and term. Each holds its grid with the columns
`estimate__`, `se__`, `lower__`, `upper__`, `effect1__` and `cond__`. A
smooth of two variables adds `effect2__`.

The list of `ggplot` panels, one per smooth, invisibly.

## See also

[`posterior_smooths.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_smooths.mvgam.md)
gives the draws of one smooth.
[`conditional_effects.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_effects.mvgam.md)
plots covariate effects on the response.

Other predictions:
[`conditional_effects.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_effects.mvgam.md),
[`fitted.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/fitted.mvgam.md),
[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md),
[`posterior_epred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_epred.mvgam.md),
[`posterior_linpred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_linpred.mvgam.md),
[`posterior_predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_predict.mvgam.md),
[`posterior_smooths.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_smooths.mvgam.md),
[`predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/predict.mvgam.md),
[`predictive_error.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/predictive_error.mvgam.md),
[`predictive_interval.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/predictive_interval.mvgam.md)

## Author

Nicholas J Clark

## Examples

``` r
# \dontrun{
set.seed(13)
simdat <- sim_mvgam(family = poisson(), n_series = 1L,
                     n_timepoints = 120L, trend_model = AR())
mod <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1),
              data    = simdat$data_train,
              family  = poisson(),
              chains  = 2, silent = 2)
#> Warning: 9 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# The smooth with 30 of its draws
cs <- conditional_smooths(mod, spaghetti = TRUE, ndraws = 30)
plot(cs)

head(cs[[1]])
#>           x effect1__ cond__ estimate__      se__     lower__   upper__
#> 1 -1.956267 -1.956267      1  0.2071589 0.2821661 -0.33025761 0.7337982
#> 2 -1.916330 -1.916330      1  0.2392826 0.2270119 -0.25263255 0.6775496
#> 3 -1.876393 -1.876393      1  0.2507761 0.2217439 -0.17511226 0.6599440
#> 4 -1.836457 -1.836457      1  0.2699572 0.2363200 -0.09783097 0.6580855
#> 5 -1.796520 -1.796520      1  0.3178227 0.1945313 -0.02092190 0.6572136
#> 6 -1.756583 -1.756583      1  0.3687286 0.1613741  0.05551992 0.6700141

# Next, plot(mod, type = "smooths") draws the same panels in one
# figure
# }
```
