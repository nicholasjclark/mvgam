# How often each unit's latent state reaches its cap

The sum over a unit's latent abundance stops at its cap `K_max`. A unit
whose posterior abundance often equals the cap is truncated, which
biases its abundance down. A higher value in that unit's `cap` column,
then a refit, removes the bias. Under
[`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md)
the cap is one, and a unit at its cap is one the species occupies.

## Usage

``` r
latent_N_saturation(
  object,
  newdata = NULL,
  threshold = 0.05,
  ndraws = NULL,
  draw_ids = NULL
)
```

## Arguments

- object:

  A fitted `mvgam` with an
  [`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md)
  or
  [`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md)
  family

- newdata:

  Optional `data.frame` of visits. `NULL` takes the training data.

- threshold:

  Number in `[0, 1]`. A unit whose share of draws at its cap exceeds
  `threshold` is marked `saturated`.

- ndraws, draw_ids:

  The number of posterior draws to use, or their indices. Give at most
  one. `NULL` for both uses every draw.

## Value

A `data.frame` with one row per closure unit:

- `unit`:

  The unit's index.

- `label`:

  The unit's key values, joined by `_`.

- `K_max`:

  The unit's cap.

- `p_saturated`:

  The posterior probability that the latent state equals `K_max`.

- `saturated`:

  `p_saturated > threshold`.

## See also

[`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md),
[`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md)

## Examples

``` r
# \dontrun{
set.seed(5)
simdat <- sim_closure_unit_data(family = nmix(), n_species = 1L,
                                n_sites = 50L, n_visits = 4L,
                                type = 1L)
# A cap of 3 is low for these counts
dat <- simdat$data_train
dat$cap <- pmax(3L, ave(dat$y, dat$series, dat$time, FUN = max))
mod <- mvgam(bf(y ~ env, p ~ tod_c), data = dat, family = nmix(),
             chains = 2, silent = 2)
saturation <- latent_N_saturation(mod)
saturation[saturation$saturated, ]
#>    unit   label K_max p_saturated saturated
#> 3     3  sp_1_3     3      1.0000      TRUE
#> 7     7  sp_1_7     3      0.1270      TRUE
#> 11   11 sp_1_11     3      1.0000      TRUE
#> 15   15 sp_1_15     3      1.0000      TRUE
#> 21   21 sp_1_21     3      0.1640      TRUE
#> 22   22 sp_1_22     4      1.0000      TRUE
#> 23   23 sp_1_23     3      0.0660      TRUE
#> 36   36 sp_1_36     3      0.3615      TRUE
#> 41   41 sp_1_41     4      1.0000      TRUE
#> 45   45 sp_1_45     3      0.1400      TRUE
#> 49   49 sp_1_49     4      1.0000      TRUE
#> 50   50 sp_1_50     3      0.0910      TRUE

# Next, raise 'cap' for the saturated units and refit
# }
```
