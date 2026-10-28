# Tweedie compound Poisson-gamma family for mvgam

The Tweedie distribution with power `mtheta` between 1 and 2, the
compound Poisson-gamma. It models non-negative continuous data with a
point mass at zero, such as catch per unit effort, rainfall or biomass.
A Poisson number of events each contributes a Gamma amount. The variance
is `mphi * mu^mtheta`.

## Usage

``` r
tweedie(M = 30L)
```

## Arguments

- M:

  Positive integer number of series terms in the Stan log density.
  Defaults to `30L`. The cost grows linearly in `M`. Too small a value
  biases the likelihood down and the posteriors of `mphi` and `mtheta`
  with it.

## Value

A
[`brms::customfamily`](https://paulbuerkner.com/brms/reference/custom_family.html)
object for `mvgam(family = )`.

## Details

The family has three distributional parameters:

- `mu`:

  The mean, on a fixed log link. The compound construction needs
  `mu > 0`, which an identity link driven by a latent trend cannot
  guarantee.

- `mphi`:

  The dispersion, on a log link.

- `mtheta`:

  The power, bounded to `[1.001, 1.999]`. Near 1 the Poisson rate
  `mu^(2 - mtheta) / ((2 - mtheta) mphi)` and near 2 the Gamma shape
  `(2 - mtheta) / (mtheta - 1)` leave floating-point range.

The latent trend enters through `mu`. `mphi` and `mtheta` are scalars by
default. A formula such as `bf(y ~ x, mphi ~ site)` gives the dispersion
its own linear predictor on the log scale, whose intercept takes a
`student_t(3, 0, 2.5)` prior. `mtheta` takes no formula.

`mu`, `mphi` and `mtheta` trade off along `Var[Y] = mphi * mu^mtheta`.
Data that weakly separate Poisson-like from Gamma-like tails leave a
ridge in their joint posterior. An informative prior such as
`prior(normal(1.5, 0.3), class = mtheta)` resolves it.

The Stan log density sums `M` terms of the compound series for each
non-zero observation (Dunn and Smyth 2005). The terms past `M` are
negligible when `M` is well above the Poisson rate `lambda`, which grows
as `mtheta` approaches 2 or `mphi` approaches 0. Raise `M` when
[`check_tweedie_truncation()`](https://nicholasjclark.github.io/mvgam/dev/reference/check_tweedie_truncation.md)
reports a rate near it.
[`log_lik()`](https://mc-stan.org/rstantools/reference/log_lik.html) and
`posterior_predict()` use
[`mgcv::ldTweedie()`](https://rdrr.io/pkg/mgcv/man/ldTweedie.html) and
[`mgcv::rTweedie()`](https://rdrr.io/pkg/mgcv/man/rTweedie.html).

The Stan code adapts the `custom_family` of the Australian Institute of
Marine Science (2023), which uses the decomposition of Spinkney (2020).

## References

Australian Institute of Marine Science (2023). `open-AIMS/tweedie`: a
brms `custom_family` implementation of the Tweedie compound
Poisson-gamma distribution. GitHub,
<https://github.com/open-AIMS/tweedie>. MIT licence.

Spinkney (2020). Tweedie likelihood (compound Poisson-gamma) in Stan.
Stan Discourse,
<https://discourse.mc-stan.org/t/tweedie-likelihood-compound-poisson-gamma-in-stan/14636>

Dunn, P. K., & Smyth, G. K. (2005). Series evaluation of Tweedie
exponential dispersion model densities. *Statistics and Computing*,
15(4), 267-280.
[doi:10.1007/s11222-005-4070-y](https://doi.org/10.1007/s11222-005-4070-y)

Jorgensen, B. (1987). Exponential dispersion models. *Journal of the
Royal Statistical Society: Series B*, 49(2), 127-162.
[doi:10.1111/j.2517-6161.1987.tb01685.x](https://doi.org/10.1111/j.2517-6161.1987.tb01685.x)

## See also

Other observation families:
[`beta_nb()`](https://nicholasjclark.github.io/mvgam/dev/reference/beta_nb.md),
[`categ()`](https://nicholasjclark.github.io/mvgam/dev/reference/categ.md),
[`com_binomial()`](https://nicholasjclark.github.io/mvgam/dev/reference/com_binomial.md),
[`diri()`](https://nicholasjclark.github.io/mvgam/dev/reference/diri.md),
[`multi()`](https://nicholasjclark.github.io/mvgam/dev/reference/multi.md),
[`mvgam_families`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_families.md),
[`mvn()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvn.md),
[`mvt()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvt.md),
[`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md),
[`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md)

## Examples

``` r
# \dontrun{
set.seed(3)
simdat <- sim_mvgam(family = tweedie(), n_series = 1L,
                    n_timepoints = 120L, trend_model = AR())
mod <- mvgam(
  y ~ s(x, k = 5),
  trend_formula = ~ AR(p = 1),
  data          = simdat$data_train,
  family        = tweedie(),
  chains        = 2,
  silent        = 2
)
#> Warning: 32 of 2000 (2.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# The dispersion and power
mcmc_plot(mod, variable = c("mphi", "mtheta"), type = "areas")


# The share of zeros the model predicts against the share observed
prop_zero <- function(y) mean(y == 0)
pp_check(mod, type = "stat", stat = "prop_zero", binwidth = 0.02)


# Next: check_tweedie_truncation(mod) checks the series length M
# }
```
