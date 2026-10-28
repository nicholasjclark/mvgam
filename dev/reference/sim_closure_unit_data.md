# Simulate closure-unit detection data with known truth

Simulates single-season data for the closure-unit families,
[`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md)
and the
[`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md)
variants. Each covariate recipe holds per-site covariates for the latent
state and per-visit covariates for detection. `data_train` is the long
frame
[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
and
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
take. `y_array` is the four-dimensional layout of
[`pivot_detection_array()`](https://nicholasjclark.github.io/mvgam/dev/reference/pivot_detection_array.md).
`truth` holds every generative parameter for recovery checks.

## Usage

``` r
sim_closure_unit_data(
  type = 1L,
  family = occ(),
  n_species = 1L,
  n_sites = 30L,
  n_visits = 4L,
  n_lv = 0L,
  K_max = 30L,
  seed = NULL
)
```

## Arguments

- type:

  Integer in `1:2` selecting the covariate recipe. See *Details*.

- family:

  A closure-unit family:
  [`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md)
  (the default),
  [`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md)
  (`"poisson_binomial"`), `nmix("royle_nichols")` or
  `nmix("poisson_poisson")`.

- n_species:

  Integer number of species. Defaults to `1L`.

- n_sites:

  Integer number of closure units per species. Defaults to `30L`.

- n_visits:

  Integer number of visits to every closure unit. Defaults to `4L`.

- n_lv:

  Integer number of latent factors shared by the species' state
  intercepts. Defaults to `0L`, independent species. Requires
  `n_species > 1L`. See *Latent factors*.

- K_max:

  Integer cap on the latent abundance `N` of an
  [`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md)
  family. Defaults to `30L`.

- seed:

  Optional integer seed. The session's random state is restored when the
  call returns.

## Value

A list of class `c("mvgam_sim_closure_unit", "mvgam_sim")`:

- `data_train`:

  Long `data.frame`. Its columns are `series` (the species), `site`,
  `time` (the site, which identifies the closure unit in a single
  season), `visit`, `y`, the site and visit covariates, then `cap`: 1
  for
  [`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md),
  `K_max` for
  [`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md).

- `data_test`:

  `NULL`. Closure-unit data have no held-out times.

- `y_array`:

  Integer array `[n_species, n_sites, 1, n_visits]` for
  [`pivot_detection_array()`](https://nicholasjclark.github.io/mvgam/dev/reference/pivot_detection_array.md).

- `family`, `type`:

  The family and recipe.

- `n_species`, `n_sites`, `n_visits`, `n_lv`:

  The simulation dimensions.

- `truth`:

  The generative parameters. See *Truth bundle*.

## Details

- `type = 1`:

  State predictor `b0 + b1 * env`, with `env ~ N(0, 1)` per site.
  Detection predictor `c0 + c1 * tod_c`, with `tod_c ~ U(-6, 6)` per
  visit, a centred time of day.

- `type = 2`:

  State predictor `b0 + b1 * env + b2 * elev`, with `env` and `elev`
  each `N(0, 1)` per site. Detection predictor
  `c0 + c1 * tod_c + c2 * effort`, with `tod_c ~ U(-6, 6)` and
  `effort ~ U(0.5, 1.5)` per visit.

Each predictor is on the scale of the family's own link. The state
predictor takes the logit for
[`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md)
and the log for every
[`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md)
variant, where it sets the Poisson abundance mean `lambda`. The
detection predictor takes the logit for
[`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md),
`nmix("poisson_binomial")` and `nmix("royle_nichols")`. It takes the log
for `nmix("poisson_poisson")`, whose encounter rate `rho` is unbounded.

Each species draws an intercept from a normal with mean zero. Each
covariate draws one community mean. Each species then draws its slope
around that mean. The standard deviations are:

|  |  |  |  |
|----|----|----|----|
| Predictor | Intercept | Community mean | Species slope |
| [`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md) state | 0.75 | 1.5 | 0.5 |
| [`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md) state | 0.5 | 0.4 | 0.3 |
| Detection | 0.5 | 0.2 | 0.15 |

The
[`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md)
state scales keep `lambda` well below `K_max`. The detection scales keep
detection away from 0 and 1 across the range of `tod_c`.

## Latent factors

With `n_lv > 0L`, the state predictor of species `k` at site `i` gains
`sum_l Z[k, l] * lv[i, l]`. The `[n_species x n_lv]` loadings `Z` are
normal with standard deviation 0.7. The `[n_sites x n_lv]` factor scores
`lv` are standard normal. `truth$loadings` and `truth$lv` return both
for comparison with
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
estimates and
[`ordinate()`](https://nicholasjclark.github.io/mvgam/dev/reference/ordinate.md).

## Truth bundle

- `psi` (occ) or `lambda` (nmix):

  `[n_species x n_sites]` matrix of occupancy probabilities or expected
  abundances.

- `z` (occ) or `N` (nmix):

  `[n_species x n_sites]` matrix of the realised latent states that
  `predict(fit, type = "latent_state")` estimates.

- `p`:

  `[n_species x n_sites x n_visits]` array of the detection parameter
  that `predict(fit, type = "detection")` estimates: the detection
  probability, the per-individual probability `r` of
  `nmix("royle_nichols")` or the encounter rate `rho` of
  `nmix("poisson_poisson")`.

- `state_coefs`, `detection_coefs`:

  The intercept and slopes of each predictor. A named vector for one
  species, an `[n_species x (1 + p)]` matrix for several.

- `loadings`, `lv`:

  `Z` and the factor scores, when `n_lv > 0L`.

## See also

[`sim_mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/sim_mvgam.md)
simulates state-space time series.
[`sim_jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/sim_jsdgam.md)
simulates site-by-species data without a detection process. The
[N-mixture
article](https://nicholasjclark.github.io/mvgam/articles/nmix.html) fits
occupancy and N-mixture models to data of this layout.

Other simulation:
[`sim_jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/sim_jsdgam.md),
[`sim_mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/sim_mvgam.md)

## Author

Nicholas J Clark

## Examples

``` r
# Four species with two site and two visit covariates
simdat <- sim_closure_unit_data(
  family    = occ(),
  n_species = 4L,
  n_sites   = 50L,
  n_visits  = 4L,
  type      = 2L,
  seed      = 1L
)
head(simdat$data_train)
#>   visit site series time y cap        env       elev      tod_c    effort
#> 1     1    1   sp_1    1 1   1 -0.6264538  0.3981059 -2.7899015 1.1588776
#> 2     2    1   sp_1    1 0   1 -0.6264538  0.3981059  3.1436863 1.4643170
#> 3     3    1   sp_1    1 1   1 -0.6264538  0.3981059  2.0845468 1.0541771
#> 4     4    1   sp_1    1 1   1 -0.6264538  0.3981059 -0.1662107 1.3708056
#> 5     1    2   sp_1    2 0   1  0.1836433 -0.6120264 -3.3762566 0.6850700
#> 6     2    2   sp_1    2 0   1  0.1836433 -0.6120264  5.1949179 0.9425101
summary(simdat)
#> Simulated closure-unit dataset (sim_closure_unit_data type 2)
#>   Family       : occ
#>   Species      : 4
#>   Sites        : 50
#>   Visits/unit  : 4
#> 
#> True generative parameters
#>   psi range: [0.000, 1.000]
#>   Detection range: [0.039, 0.959]
#>   Non-zero observation fraction: 0.330

# Next: fit these data with jsdgam(), as the shared_variation()
# example does
```
