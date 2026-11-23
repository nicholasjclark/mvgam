# Group-level (random) effects of a fitted mvgam model

Posterior draws of each level's deviation for every group-level term of
the observation formula, summarised or raw. Each element is named
`r_<group>[<level>,<coef>]`, as
[`variables.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/index-mvgam.md)
lists it.

## Usage

``` r
# S3 method for class 'mvgam'
ranef(
  object,
  summary = TRUE,
  robust = FALSE,
  probs = c(0.025, 0.975),
  pars = NULL,
  groups = NULL,
  ...
)
```

## Arguments

- object:

  A fitted `mvgam` object.

- summary:

  Logical. `TRUE` (the default) summarises each coefficient's draws.
  `FALSE` returns the draws.

- robust:

  Logical. If `FALSE` (the default), uses mean and standard deviation
  for summaries. If `TRUE`, uses median and median absolute deviation
  (MAD).

- probs:

  Two probabilities for the lower and upper bounds of the credible
  interval. Default `c(0.025, 0.975)`, a 95% interval.

- pars:

  Optional character vector of coefficient names, such as `"Intercept"`.
  `NULL` (the default) keeps every coefficient.

- groups:

  Optional character vector of grouping factors. `NULL` (the default)
  keeps every factor.

- ...:

  Unused. Anything passed here is refused.

## Value

A named list with one 3D array per grouping factor. With
`summary = TRUE` the dimensions are levels, statistics and coefficients.
The statistics are `Estimate`, `Est.Error` and one quantile per element
of `probs`. With `summary = FALSE` the dimensions are draws, levels and
coefficients, and the array carries an `nchains` attribute. A grouping
factor of a distributional parameter or of one response of several is
keyed `<group>__<parameter>`.

## Details

A group-level term in `trend_formula` is named with the `_trend` suffix,
as `r_g_trend[a,Intercept]`.
[`as_draws.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_draws.md)
returns its draws.

## See also

[`VarCorr.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/VarCorr.mvgam.md)
summarises the standard deviations and correlations.
[`fixef.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_diagnostics.md)
summarises the population-level effects.

Other posterior summaries:
[`VarCorr.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/VarCorr.mvgam.md),
[`bibtex()`](https://nicholasjclark.github.io/mvgam/dev/reference/bibtex.md),
[`how_to_cite.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/how_to_cite.mvgam.md),
[`hypothesis.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hypothesis.mvgam.md),
[`index-mvgam`](https://nicholasjclark.github.io/mvgam/dev/reference/index-mvgam.md),
[`mcmc_plot.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mcmc_plot.mvgam.md),
[`methods_md()`](https://nicholasjclark.github.io/mvgam/dev/reference/methods_md.md),
[`mvgam_draws`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_draws.md),
[`pairs.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/pairs.mvgam.md),
[`posterior_interval.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_interval.mvgam.md),
[`smooths.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/smooths.mvgam.md),
[`summary.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam.md)

## Author

Nicholas J Clark

## Examples

``` r
# \dontrun{
set.seed(14)
simdat <- sim_mvgam(family = poisson(), n_series = 4L,
                    n_timepoints = 120L, trend_model = AR())
mod <- mvgam(y ~ s(x, k = 5) + (1 | series),
             trend_formula = ~ AR(p = 1),
             data = simdat$data_train, family = poisson(),
             chains = 2, silent = 2)
#> Warning: 83 of 2000 (4.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# Each series' intercept deviation, one row per series
ranef(mod)$series[, , "Intercept"]
#>              Estimate Est.Error       Q2.5     Q97.5
#> series_1  0.003638482 0.1452967 -0.3227788 0.3215572
#> series_2 -0.043876562 0.1942125 -0.4976695 0.3037575
#> series_3 -0.079616499 0.1785262 -0.5427947 0.2452710
#> series_4  0.077633103 0.1662872 -0.2329808 0.4639085

# Next: VarCorr() gives the standard deviation of these deviations
# }
```
