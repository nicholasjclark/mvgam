# Simulate multi-species data for a joint species distribution model

Draws a site-by-species response matrix from a latent factor model and
returns it in the long format
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
takes. Each species gets its own intercept and its own slope on a
site-level covariate. The species share `n_lv` latent factors, whose
loadings set the residual correlation between them.

This is the non-detection counterpart to
[`sim_closure_unit_data()`](https://nicholasjclark.github.io/mvgam/dev/reference/sim_closure_unit_data.md),
which simulates the same latent factor structure for the occupancy and
N-mixture families.

## Usage

``` r
sim_jsdgam(
  family = brms::negbinomial(),
  n_species = 5L,
  n_sites = 60L,
  n_lv = 2L,
  family_pars = list(),
  seed = NULL
)
```

## Arguments

- family:

  Observation family. Any family mvgam's response simulator draws from,
  plus
  [`mvn()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvn.md),
  [`mvt()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvt.md),
  [`diri()`](https://nicholasjclark.github.io/mvgam/dev/reference/diri.md),
  [`multi()`](https://nicholasjclark.github.io/mvgam/dev/reference/multi.md)
  and
  [`categ()`](https://nicholasjclark.github.io/mvgam/dev/reference/categ.md).
  Defaults to
  [`brms::negbinomial()`](https://paulbuerkner.com/brms/reference/brmsfamily.html).

- n_species:

  Number of species. Defaults to `5`.

- n_sites:

  Number of sites. Defaults to `60`.

- n_lv:

  Number of latent factors. Defaults to `2`.

- family_pars:

  Optional named list of family parameters: `Psi` (per-species scale for
  [`mvn()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvn.md)
  /
  [`mvt()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvt.md)),
  `nu` (degrees of freedom for
  [`mvt()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvt.md)),
  `phi` (concentration for
  [`diri()`](https://nicholasjclark.github.io/mvgam/dev/reference/diri.md)),
  `unit_total` (trial total per site for
  [`multi()`](https://nicholasjclark.github.io/mvgam/dev/reference/multi.md)).
  Other families take their dispersion or scale, such as `size` for a
  negative binomial or `sigma` for a gaussian.

- seed:

  Optional integer seed. The session's random state is restored when the
  call returns.

## Value

An object of class `mvgam_sim_jsdgam`, inheriting `mvgam_sim`, with
elements:

- `data_train`:

  Long `data.frame` with columns `site`, `env`, `series` (the species),
  `y` and `time` (the unit index
  [`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
  keys on).

- `data_test`:

  `NULL`, kept for consistency with the other simulators.

- `y_array`:

  `[n_sites x n_species]` response matrix.

- `family`:

  The family used.

- `n_species`, `n_sites`, `n_lv`:

  The design.

- `truth`:

  List of the generative parameters: `loadings`, `lv`, `intercepts`,
  `env_slopes`, `env`, `residual_cov`, `residual_cor` and `family_pars`.
  For
  [`mvn()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvn.md),
  [`mvt()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvt.md),
  [`diri()`](https://nicholasjclark.github.io/mvgam/dev/reference/diri.md)
  and
  [`multi()`](https://nicholasjclark.github.io/mvgam/dev/reference/multi.md)
  these include the defaults `Psi = 0.5`, `nu = 5`, `phi = 30` and
  `unit_total = 30`.

## Details

The linear predictor for species `k` at site `i` is
`intercept[k] + env_slope[k] * env[i] + Z[k, ] %*% lv[i, ]`, where `Z`
holds the loadings and `lv` the site scores. The residual covariance the
model targets is `Z Z'`, plus a diagonal of squared per-species scales
for
[`mvn()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvn.md)
and
[`mvt()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvt.md).

How that predictor becomes a response depends on the family. Families
whose species are drawn independently, such as
[`brms::negbinomial()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
or [`stats::poisson()`](https://rdrr.io/r/stats/family.html), take one
draw per species.
[`mvn()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvn.md)
and
[`mvt()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvt.md)
add a per-species residual. The composition families draw a site's
species together.
[`diri()`](https://nicholasjclark.github.io/mvgam/dev/reference/diri.md)
normalises gamma variates.
[`multi()`](https://nicholasjclark.github.io/mvgam/dev/reference/multi.md)
draws counts against a fixed site total.
[`categ()`](https://nicholasjclark.github.io/mvgam/dev/reference/categ.md)
draws one species per site as a one-hot row.

A composition family identifies its loadings only up to a shift shared
by the species, which is the subspace mvgam's `sum_to_zero_vector`
parameterisation covers. The loadings are centred for those families,
which puts the simulated truth in the same subspace a fit explores.

How much of the factor structure survives into the response depends on
the observation noise. The loadings are drawn with standard deviation
0.7.
[`mvn()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvn.md)
and
[`mvt()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvt.md)
take their per-species scale from `Psi`, which defaults to 0.5. Every
other family takes the scale its own simulator defaults to. A gaussian's
`sigma` defaults to 1, which can hide the species correlations from the
data. Pass `family_pars = list(sigma = 0.5)` for a gaussian whose
correlations a fit can recover.

## See also

[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
fits the model these data come from.
[`sim_closure_unit_data()`](https://nicholasjclark.github.io/mvgam/dev/reference/sim_closure_unit_data.md)
simulates occupancy and N-mixture data.
[`sim_mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/sim_mvgam.md)
simulates time series. The [joint species distribution
article](https://nicholasjclark.github.io/mvgam/articles/jsdgam.html)
fits and interprets models of this kind.

Other simulation:
[`sim_closure_unit_data()`](https://nicholasjclark.github.io/mvgam/dev/reference/sim_closure_unit_data.md),
[`sim_mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/sim_mvgam.md)

## Examples

``` r
simdat <- sim_jsdgam(
  family    = brms::negbinomial(),
  n_species = 4L,
  n_sites   = 30L,
  n_lv      = 2L,
  seed      = 1L
)
head(simdat$data_train)
#>   site        env series y time
#> 1    1  0.5757814   sp_1 4    1
#> 2    1  0.5757814   sp_2 1    1
#> 3    1  0.5757814   sp_3 0    1
#> 4    1  0.5757814   sp_4 1    1
#> 5    2 -0.3053884   sp_1 0    2
#> 6    2 -0.3053884   sp_2 2    2
summary(simdat)
#> Simulated joint species distribution dataset (sim_jsdgam)
#>   Family        : negbinomial
#>   Species       : 4
#>   Sites         : 30
#>   Latent factors: 2
#>   Rows          : 120
#> 
#> True generative parameters
#>   Intercepts :  1.100,  0.763, -0.165, -0.253
#>   Env slopes :  0.697,  0.557, -0.689, -0.707
#>   Residual correlations: [-0.680, 0.999]
#> 
#> Simulated response
#>   Range         : [ 0, 18]
#>   Zero fraction : 0.317

# A composition family draws each site's species together
comp <- sim_jsdgam(family = diri(), n_species = 4L, n_sites = 20L,
                   seed = 2L)
rowSums(comp$y_array)[1:3]
#> [1] 1 1 1

# Next: fit with jsdgam(), then compare residual_cor() of the fit
# with simdat$truth$residual_cor
```
