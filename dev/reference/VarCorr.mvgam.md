# Standard deviations and correlations of group-level effects

Posterior draws of the standard deviation of each group-level term of
the observation formula, with the correlations and covariances of terms
that share a grouping factor.

## Usage

``` r
# S3 method for class 'mvgam'
VarCorr(
  x,
  sigma = 1,
  summary = TRUE,
  robust = FALSE,
  probs = c(0.025, 0.975),
  ...
)
```

## Arguments

- x:

  A fitted `mvgam` object.

- sigma:

  A single non-negative number. It has no effect.
  [`summary.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam.md)
  reports the observation family's scale.

- summary:

  Logical. `TRUE` (the default) summarises the draws. `FALSE` returns
  them.

- robust:

  Logical. `TRUE` summarises with the median and MAD. Defaults to
  `FALSE`.

- probs:

  Numeric vector of the two quantiles to report.

- ...:

  Unused. Anything passed here is refused.

## Value

A named list with one entry per grouping factor. Each entry is a list of
these components:

- `sd`: the standard deviation of each coefficient, a matrix of
  coefficients by statistics, or of draws by coefficients under
  `summary = FALSE`.

- `cor`: correlation matrices, present when the factor carries two or
  more correlated coefficients. The array runs over coefficients,
  statistics and coefficients, or over draws and two coefficient axes
  under `summary = FALSE`.

- `cov`: covariance matrices, shaped as `cor`.

## Details

A trend-side component is named with the `_trend` suffix, as
`sd_g__Intercept_trend`.
[`as_draws.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_draws.md)
returns its draws.

## See also

[`ranef.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/ranef.mvgam.md)
gives each level's deviation.
[`fixef.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_diagnostics.md)
summarises the population-level effects.

Other posterior summaries:
[`bibtex()`](https://nicholasjclark.github.io/mvgam/dev/reference/bibtex.md),
[`how_to_cite.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/how_to_cite.mvgam.md),
[`hypothesis.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hypothesis.mvgam.md),
[`index-mvgam`](https://nicholasjclark.github.io/mvgam/dev/reference/index-mvgam.md),
[`mcmc_plot.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mcmc_plot.mvgam.md),
[`methods_md()`](https://nicholasjclark.github.io/mvgam/dev/reference/methods_md.md),
[`mvgam_draws`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_draws.md),
[`pairs.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/pairs.mvgam.md),
[`posterior_interval.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_interval.mvgam.md),
[`ranef.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/ranef.mvgam.md),
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
mod <- mvgam(y ~ s(x) + (1 | series),
             trend_formula = ~ AR(p = 1),
             data = simdat$data_train, family = poisson(),
             chains = 2, silent = 2)
#> Warning: 76 of 2000 (4.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# The standard deviation of the series intercepts
VarCorr(mod)$series$sd
#>            Estimate Est.Error        Q2.5     Q97.5
#> Intercept 0.1984474  0.219979 0.006018553 0.7665907

# Next: ranef() gives each series' own deviation
# }
```
