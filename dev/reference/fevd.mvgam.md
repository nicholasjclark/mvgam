# Calculate latent VAR forecast error variance decompositions

Compute forecast error variance decompositions from `mvgam` models with
Vector Autoregressive dynamics

## Usage

``` r
fevd(object, ...)

# S3 method for class 'mvgam'
fevd(
  object,
  h = 10,
  ndraws = NULL,
  draw_ids = NULL,
  summary = TRUE,
  probs = c(0.025, 0.975),
  future = FALSE,
  ...
)
```

## Arguments

- object:

  An object of class `mvgam` fitted with a
  [`VAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  latent process model

- ...:

  Unused. Anything passed here is refused.

- h:

  Positive integer. The number of forecast horizons to decompose.

- ndraws:

  Positive integer specifying number of posterior draws to use. NULL
  (default) uses all available draws.

- draw_ids:

  Optional integer vector selecting a subset of posterior draw indices
  to use. NULL (default) uses all draws (subject to `ndraws`).

- summary:

  Logical; return the posterior median and interval of each
  decomposition (the default), or the per-draw decompositions
  themselves.

- probs:

  The lower and upper percentiles of the interval reported with the
  median when `summary = TRUE`.

- future:

  `Logical`. When `TRUE`, the per-draw computation runs under the
  [`future::plan()`](https://future.futureverse.org/reference/plan.html)
  the caller has set. The default `sequential` plan runs as base
  [`lapply()`](https://rdrr.io/r/base/lapply.html) does.
  `plan(multisession, workers = N)` splits the draws across `N` R
  processes. Without the future package the draws run in turn, with a
  message.

## Value

An object of
[`mvgam_fevd-class`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_fevd-class.md),
which describes the quantities computed, with references.

## References

Lutkepohl H (2005). New Introduction to Multiple Time Series Analysis.
Springer, Berlin.
[doi:10.1007/978-3-540-27752-1](https://doi.org/10.1007/978-3-540-27752-1)

## See also

[`irf()`](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md)
and
[`stability()`](https://nicholasjclark.github.io/mvgam/dev/reference/stability.mvgam.md)
summarise the same VAR dynamics. The [VAR
article](https://nicholasjclark.github.io/mvgam/articles/var.html) runs
`fevd()` on a plankton community.

Other latent dynamics:
[`active_factors()`](https://nicholasjclark.github.io/mvgam/dev/reference/active_factors.md),
[`compare_loadings()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_loadings.md),
[`irf.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md),
[`ordinate()`](https://nicholasjclark.github.io/mvgam/dev/reference/ordinate.md),
[`posterior_transition_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_transition_matrix.md),
[`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md),
[`shared_variation()`](https://nicholasjclark.github.io/mvgam/dev/reference/shared_variation.md),
[`stability.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/stability.mvgam.md)

## Author

Nicholas J Clark

## Examples

``` r
# \dontrun{
set.seed(0)
simdat <- sim_mvgam(family = gaussian(), n_series = 3L,
                    n_timepoints = 120L, trend_model = VAR(),
                    prop_trend = 0.95)
var_mod <- mvgam(y ~ 1, trend_formula = ~ VAR(p = 1),
                 data = simdat$data_train, family = gaussian(),
                 chains = 2, silent = 2)
#> Warning: 3 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 1 of 2 chains had an E-BFMI less than 0.3.
#> See https://mc-stan.org/misc/warnings for details.

# The share of each series' forecast-error variance due to each
# shock, over an eight-step horizon
fevd_obj <- fevd(var_mod, h = 8L)
plot(fevd_obj)


# Next: irf() traces each shock through the system over time
# }
```
