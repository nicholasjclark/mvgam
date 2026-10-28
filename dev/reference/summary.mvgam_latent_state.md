# Per-unit posterior summary for a `mvgam_latent_state` object

Collapses the `[ndraws, n_unit]` draw matrix to one row per unit
carrying the posterior median plus 50% and 95% credible interval
endpoints. The returned `data.frame` is the same one that
`as.data.frame.mvgam_latent_state()` returns and that
[`plot.mvgam_latent_state()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_latent_state.md)
uses internally.

## Usage

``` r
# S3 method for class 'mvgam_latent_state'
summary(object, probs = c(0.025, 0.25, 0.75, 0.975), ...)
```

## Arguments

- object:

  A `mvgam_latent_state` object.

- probs:

  Numeric vector of length 4 giving the lower / inner-lower /
  inner-upper / upper interval probabilities. Defaults to
  `c(0.025, 0.25, 0.75, 0.975)` (50% and 95% intervals).

- ...:

  Unused. Anything passed here is refused.

## Value

A `data.frame` with columns `series`, `time`, `median`, `lower_50`,
`upper_50`, `lower_95`, `upper_95`.
