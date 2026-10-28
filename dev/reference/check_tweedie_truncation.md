# Diagnose whether the Tweedie series-truncation `M` is adequate

Computes the per-observation Poisson rate
`lambda = mu^(2 - theta) / ((2 - theta) * phi)` at the posterior mean of
`(mu, mphi, mtheta)` and compares it against the truncation count `M`
saved in the fit's standata. Warns if `max(lambda)` exceeds `0.7 * M`,
the rough point at which the `log_sum_exp` over `m = 1..M` starts to
omit appreciable Poisson tail mass and bias the log-density downward.

## Usage

``` r
check_tweedie_truncation(object, resp = NULL)
```

## Arguments

- object:

  An `mvgam` fit with at least one response whose family is
  [`tweedie()`](https://nicholasjclark.github.io/mvgam/dev/reference/tweedie.md).

- resp:

  Optional name of one response to check. Without it, every
  [`tweedie()`](https://nicholasjclark.github.io/mvgam/dev/reference/tweedie.md)
  response is checked.

## Value

Invisible `NULL`. Emits a message on the adequacy of `M`. It also emits
a warning if `M` looks tight.

## Details

The rate is taken for every response whose family is
[`tweedie()`](https://nicholasjclark.github.io/mvgam/dev/reference/tweedie.md).
A multivariate model can name it for one response among several. All of
them share the one `M`. `mphi` and `mtheta` are taken per observation.
This covers a parameter given a formula of its own as well as a scalar
one.

## Examples

``` r
# \dontrun{
set.seed(3)
simdat <- sim_mvgam(family = tweedie(), n_series = 1L,
                    n_timepoints = 120L, trend_model = AR())
mod <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1),
             data = simdat$data_train, family = tweedie(),
             chains = 2, silent = 2)
#> Warning: 51 of 2000 (3.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# The largest Poisson rate the fit implies, against the default M = 30
check_tweedie_truncation(mod)
#> Tweedie truncation diagnostic. M = 30, max(lambda) at posterior mean = 4.91 (ratio = 0.164).

# A warning here would call for a refit with a longer series, such
# as family = tweedie(M = 60)
# }
```
