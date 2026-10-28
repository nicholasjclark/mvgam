# Simulate `mvgam`-shaped time-series data with known truth

Simulates training and test data from one of seven observation-side
recipes (`type`) combined with a latent trend (`trend_model`). The
recipes cover smooths, Gaussian processes and random effects. Every
generative quantity is returned for recovery checks.

## Usage

``` r
sim_mvgam(
  type = 1L,
  family = gaussian(),
  n_series = 1L,
  n_timepoints = 80L,
  trend_model = NULL,
  prop_trend = NULL,
  proportional_train = 0.75,
  prop_missing = 0,
  family_pars = list(),
  seed = NULL
)
```

## Arguments

- type:

  Integer in `1:7` selecting the observation-side recipe. See *Details*.

- family:

  The observation family:
  [`gaussian()`](https://rdrr.io/r/stats/family.html) (the default),
  [`student()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  [`poisson()`](https://rdrr.io/r/stats/family.html),
  [`negbinomial()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  [`bernoulli()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  [`binomial()`](https://rdrr.io/r/stats/family.html),
  [`Beta()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  [`Gamma()`](https://rdrr.io/r/stats/family.html),
  [`tweedie()`](https://nicholasjclark.github.io/mvgam/dev/reference/tweedie.md),
  [`beta_nb()`](https://nicholasjclark.github.io/mvgam/dev/reference/beta_nb.md)
  or
  [`com_binomial()`](https://nicholasjclark.github.io/mvgam/dev/reference/com_binomial.md),
  or the matching
  [`brms::brmsfamily()`](https://paulbuerkner.com/brms/reference/brmsfamily.html).

- n_series:

  Integer number of series. Defaults to `1L`.

- n_timepoints:

  Integer number of time points per series. Defaults to `80L`.

- trend_model:

  A trend constructor such as `AR(p = 1)`, `AR(p = c(1, 12))`,
  `VAR(cor = TRUE)`,
  [`CAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md),
  [`ZMVN()`](https://nicholasjclark.github.io/mvgam/dev/reference/ZMVN.md)
  or
  [`PW()`](https://nicholasjclark.github.io/mvgam/dev/reference/piecewise_trends.md).
  `NULL` (the default) takes the type's own trend. `"None"` simulates no
  trend. To fit the simulated data, name the same trend inside
  `trend_formula`, as in `mvgam(y ~ s(x), trend_formula = ~ AR())`.
  [`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
  refuses a `trend_model` argument.

- prop_trend:

  Share of the link-scale variance given to the trend, in `[0, 1]`.
  `NULL` takes the type's default: 0.2 for type 5, 0.6 for types 2, 3, 4
  and 7, and 0.5 for types 1 and 6.

- proportional_train:

  Share of the time points kept in `data_train`, between 0.1 and 1. The
  remaining, later time points form `data_test`. Defaults to `0.75`.

- prop_missing:

  Share of the training responses set to `NA`, between 0 and 0.5.
  Defaults to `0`.

- family_pars:

  Optional named list of family parameters: `sigma` and `nu` (gaussian,
  student), `size` (negative binomial), `trials` (binomial,
  com_binomial), `phi` (beta, tweedie), `shape` (gamma, beta_nb),
  `power` (tweedie), `mtail` (beta_nb) and `nu` (com_binomial
  dispersion). Unset entries take defaults.

- seed:

  Optional integer seed. The session's random state is restored when the
  call returns.

## Value

A list of class `"mvgam_sim"`:

- `data_train`, `data_test`:

  Long data frames with columns `y`, `series`, `time`, the recipe's
  covariates and, for binomial families, `trials`. `data_test` is `NULL`
  when `proportional_train = 1`.

- `family`, `trend_model`, `type`:

  The simulation settings.

- `true_betas`:

  Named population-level coefficients.

- `true_smooths`:

  Named list of data frames holding each true smooth on a grid of its
  covariate, in column `f_true`.

- `true_trend`:

  `[n_timepoints, n_series]` matrix of the latent trend.

- `true_trend_sigma`:

  Standard deviation of `true_trend`.

- `true_sigma_obs`:

  The family's scale or dispersion, or `NA` for a family without one.

## Details

- `type = 1`:

  `y ~ s(x)` with an
  [`RW()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  trend.

- `type = 2`:

  `y ~ s(x) + s(z)` with an `AR(p = 1)` trend whose coefficient is 0.7.

- `type = 3`:

  `y ~ s(x) + (1 | grp)` with an `AR(p = 1)` trend whose coefficient is
  0.7. The random intercept has five levels.

- `type = 4`:

  `y ~ gp(x)` with an `AR(p = 1)` trend whose coefficient is 0.7.

- `type = 5`:

  `y ~ x` with `trend_formula = ~ s(z)` and an `AR(p = 1)` trend whose
  coefficient is 0.6. The smallest `prop_trend` default keeps the
  autoregression separable from the smooth.

- `type = 6`:

  `y ~ s(season, bs = "cc")` with a
  [`CAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  trend on irregular gaps drawn from `Uniform(1, 6)`. The
  autocorrelation over the median gap is 0.7.

- `type = 7`:

  `y ~ s(x)` with a sparse `AR(p = c(1, 12))` trend whose coefficients
  are 0.55 and 0.40. The covariate is not periodic. All of the seasonal
  recurrence comes from the lag-12 coefficient.

Covariates are drawn independently for each row. Series are independent
unless the trend correlates them, as
[`VAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md),
[`ZMVN()`](https://nicholasjclark.github.io/mvgam/dev/reference/ZMVN.md)
and `cor = TRUE` do. The trend and the observation-side terms are each
centred, then rescaled to their shares of a link-scale standard
deviation that keeps the response in a plausible range for the family.

## See also

[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
fits the simulated data.
[`sim_closure_unit_data()`](https://nicholasjclark.github.io/mvgam/dev/reference/sim_closure_unit_data.md)
simulates occupancy and N-mixture data.
[`sim_jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/sim_jsdgam.md)
simulates site-by-species data. The [overview
article](https://nicholasjclark.github.io/mvgam/dev/articles/mvgam_overview.html)
fits a model to data from `sim_mvgam()`.

Other simulation:
[`sim_closure_unit_data()`](https://nicholasjclark.github.io/mvgam/dev/reference/sim_closure_unit_data.md),
[`sim_jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/sim_jsdgam.md)

## Author

Nicholas J Clark

## Examples

``` r
# Three Poisson series with an AR(1) latent trend
simdat <- sim_mvgam(
  family       = poisson(),
  n_series     = 3L,
  n_timepoints = 120L,
  trend_model  = AR(),
  seed         = 0L
)
head(simdat$data_train)
#>    y   series time          x
#> 1 46 series_1    1  1.5867888
#> 2 11 series_1    2 -0.9379653
#> 3  5 series_1    3 -0.5115044
#> 4 14 series_1    4  0.2914135
#> 5 26 series_1    5  1.6328312
#> 6  5 series_1    6 -1.1932723
simdat
#> Simulated mvgam dataset (sim_mvgam type 1)
#>   Family       : poisson
#>   Trend        : AR
#>   Series       : 3
#>   Timepoints   : 120
#>   Rows         : 270 train, 90 test
#> 
#> True generative parameters
#>   Population coefficients: (none)
#>   Smooth functions (1): s(x)
#>   Latent trend SD: 0.566
#>   Obs noise / dispersion: NA

# The true smooth of x
plot(f_true ~ x, data = simdat$true_smooths$`s(x)`, type = "l")


# Next: mvgam(y ~ s(x), trend_formula = ~ AR(),
#   data = simdat$data_train, family = poisson())
```
