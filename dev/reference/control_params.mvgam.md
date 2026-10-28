# Sampler control settings of a fitted mvgam model

The NUTS settings the model was sampled with, such as `adapt_delta`,
`max_treedepth` and `stepsize`. A fit that reported divergent
transitions can be refitted with a higher `adapt_delta`, such as
`update(fit, control = list(adapt_delta = 0.95))`. The sampler then
takes smaller steps.

## Usage

``` r
# S3 method for class 'mvgam'
control_params(x, ...)
```

## Arguments

- x:

  A fitted `mvgam` model.

- ...:

  Unused. Anything passed here is refused.

## Value

A named list, as
[`brms::control_params()`](https://paulbuerkner.com/brms/reference/control_params.html)
returns for a `brmsfit`. A fit with no NUTS settings, such as a
variational fit, gives an empty list.

## See also

[mvgam_diagnostics](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_diagnostics.md)
for the divergence counts,
[`summary.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam.md)
for convergence checks.

## Examples

``` r
# \dontrun{
set.seed(1)
simdat <- sim_mvgam(n_series = 1L, n_timepoints = 60L,
                    trend_model = AR())
mod <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
             data = simdat$data_train, family = poisson(),
             chains = 2, silent = 2)
#> Error: 'y' has values outside the support of family 'poisson'.
#>   Requires: 0 <= 'y' (integers only).
#>   Observed range: [-2.504, 2.943]. 23 of 45 values fall outside, the first
#>   at row 1.
#>   Check the column for missing-data sentinels such as -1 or -999.
control_params(mod)$adapt_delta
#> Error: object 'mod' not found
# }
```
