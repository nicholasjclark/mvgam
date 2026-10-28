# Calculate latent VAR impulse response functions

Compute Generalized or Orthogonalized Impulse Response Functions (IRFs)
from `mvgam` models with Vector Autoregressive dynamics. An impulse
response traces how the latent trend of every series moves over `h` time
steps after a one-standard-deviation innovation to one series. The
default generalised responses suit most analyses. Orthogonalised
responses need a causal ordering of the series.

## Usage

``` r
irf(object, ...)

# S3 method for class 'mvgam'
irf(
  object,
  h = 10,
  cumulative = FALSE,
  orthogonal = FALSE,
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

  Positive integer. The number of horizons to compute. Horizon 1 is the
  time step of the shock.

- cumulative:

  Logical. `TRUE` accumulates each response over the horizons.

- orthogonal:

  Logical. `TRUE` computes orthogonalised responses from the lower
  Cholesky factor of the innovation covariance. These depend on the
  order of the levels of the series factor. The default generalised
  responses do not (Pesaran and Shin 1998).

- ndraws:

  Positive integer specifying number of posterior draws to use. NULL
  (default) uses all available draws.

- draw_ids:

  Optional integer vector selecting a subset of posterior draw indices
  to use. NULL (default) uses all draws (subject to `ndraws`).

- summary:

  Logical; return the posterior median and interval of each response
  (the default), or the per-draw responses themselves. The draws are one
  `K` by `K` matrix per horizon per draw, which on a wide panel runs to
  hundreds of megabytes.

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
[`mvgam_irf-class`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_irf-class.md).
[`plot.mvgam_irf()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_irf.md)
and
[`summary.mvgam_irf()`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam_irf.md)
take it.

## Details

See
[`mvgam_irf-class`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_irf-class.md)
for the quantities computed and returned, with references.

## References

Lutkepohl H (2005). New Introduction to Multiple Time Series Analysis.
Springer, Berlin.
[doi:10.1007/978-3-540-27752-1](https://doi.org/10.1007/978-3-540-27752-1)

Pesaran HH and Shin Y (1998). Generalized impulse response analysis in
linear multivariate models. Economics Letters 58, 17-29.
[doi:10.1016/S0165-1765(97)00214-0](https://doi.org/10.1016/S0165-1765%2897%2900214-0)

## See also

[`fevd()`](https://nicholasjclark.github.io/mvgam/dev/reference/fevd.mvgam.md)
and
[`stability()`](https://nicholasjclark.github.io/mvgam/dev/reference/stability.mvgam.md)
summarise the same VAR dynamics. The [VAR
article](https://nicholasjclark.github.io/mvgam/dev/articles/var.html)
runs `irf()` on a plankton community. The [hierarchical VAR
article](https://nicholasjclark.github.io/mvgam/dev/articles/hierarchical_var.html)
runs it on each group of a hierarchical VAR.

Other latent dynamics:
[`active_factors()`](https://nicholasjclark.github.io/mvgam/dev/reference/active_factors.md),
[`compare_loadings()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_loadings.md),
[`fevd.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/fevd.mvgam.md),
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
#> Warning: 44 of 2000 (2.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 1 of 2 chains had an E-BFMI less than 0.3.
#> See https://mc-stan.org/misc/warnings for details.

# Generalised responses of every series to a shock in each
irf_obj <- irf(var_mod, h = 8L)
plot(irf_obj, series = 1)


# Next: fevd() splits each series' forecast variance among the
# shocks. stability() summarises how the system returns to
# equilibrium
# }
```
