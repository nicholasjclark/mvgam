# Model summary for an `mvgam` fit

broom-style summary of an `mvgam` fit, one row per response. Reports the
Stan algorithm, post-warmup sample size, training-observation count,
response family, link function and (optionally) `loo` criteria. Mirrors
`broom.mixed::glance.brmsfit()`.

## Usage

``` r
# S3 method for class 'mvgam'
glance(x, looic = FALSE, resp = NULL, ...)
```

## Arguments

- x:

  A fitted `mvgam` object.

- looic:

  Logical. If `TRUE`, compute leave-one-out information criteria via
  [`loo.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo.mvgam.md)
  and add `elpd_loo`, `se_elpd_loo`, `p_loo` and `looic` columns.
  Defaults to `FALSE` because computing `loo` can be slow on large fits.

- resp:

  Character specifying which response variable for multivariate models.
  NULL (default) returns predictions for all responses.

- ...:

  Forwarded to
  [`loo.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo.mvgam.md)
  when `looic = TRUE`.

## Value

A tibble with one row, or on a multivariate fit given no `resp` one row
per response with a `resp` column. Columns:

- `algorithm` – the algorithm the fit ran, such as `"sampling"` or
  `"meanfield"`.

- `pss` – post-warmup sample size summed across chains.

- `nobs` – number of (non-missing) training observations.

- `nseries` – number of unique series in the training data.

- `family` – observation-family name.

- `link` – observation-family link.

- Optional: `elpd_loo`, `se_elpd_loo`, `p_loo`, `looic` (only when
  `looic = TRUE`).

## See also

[`tidy.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/tidy.mvgam.md),
[`augment.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/augment.mvgam.md),
[`loo.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo.mvgam.md)

Other tidiers:
[`augment.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/augment.mvgam.md),
[`tidy.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/tidy.mvgam.md)

## Examples

``` r
# \dontrun{
# A Poisson GAM without a latent trend. PSIS approximates each
# held-out count of such a model well (see Details of ?loo.mvgam)
set.seed(13)
simdat <- sim_mvgam(family = poisson(), n_series = 1L,
                    n_timepoints = 120L, trend_model = "None")
mod <- mvgam(y ~ s(x), data = simdat$data_train,
             family = poisson(), chains = 2, silent = 2)
#> Warning: 2 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
# }
# \dontrun{
# The sampler and data dimensions of the fit, in one row
glance(mod)
#> # A tibble: 1 × 6
#>   algorithm   pss  nobs nseries family  link 
#>   <chr>     <int> <int>   <int> <chr>   <chr>
#> 1 sampling   2000    90       1 poisson log  

# Leave-one-out criteria take longer to compute
glance(mod, looic = TRUE)
#> # A tibble: 1 × 10
#>   algorithm   pss  nobs nseries family  link  elpd_loo se_elpd_loo p_loo looic
#>   <chr>     <int> <int>   <int> <chr>   <chr>    <dbl>       <dbl> <dbl> <dbl>
#> 1 sampling   2000    90       1 poisson log      -209.        8.48  7.98  418.
# }
```
