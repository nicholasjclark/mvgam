# Pivot a wide species matrix to mvgam long-format

Convert the wide `[J sites, N species]` response matrix used by `Hmsc`
(and any single-visit JSDM workflow) into the long-format
`(series, time, y, ...)` layout that
[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
/
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
with a simplex
([`diri()`](https://nicholasjclark.github.io/mvgam/dev/reference/diri.md),
[`multi()`](https://nicholasjclark.github.io/mvgam/dev/reference/multi.md),
[`categ()`](https://nicholasjclark.github.io/mvgam/dev/reference/categ.md)),
continuous-response
([`mvn()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvn.md),
[`mvt()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvt.md)),
or bernoulli / binomial / poisson family expects.

## Usage

``` r
pivot_species_matrix(
  Y,
  site_covs = NULL,
  traits = NULL,
  site_id_name = "time",
  series_name = "series",
  values_to = "y"
)
```

## Arguments

- Y:

  A `[J_sites, N_species]` matrix or `data.frame`. Column names become
  the `series` factor levels; missing column names default to
  `sp1, sp2, ...`.

- site_covs:

  Optional `data.frame` of site-level covariates with `nrow == J_sites`.
  The `Hmsc` `XData` slot maps here.

- traits:

  Optional `data.frame` of per-species traits with `nrow == N_species`.
  Broadcast across sites. The `Hmsc` `TrData` slot maps here.

- site_id_name:

  Name of the site identifier in the output. Defaults to `"time"`.

- series_name:

  Name of the species column. Defaults to `"series"`.

- values_to:

  Name of the response column. Defaults to `"y"`.

## Value

A long-format `data.frame` with `J * N` rows, each carrying `series`,
`time`, `y`, site covariates broadcast across species and species traits
broadcast across sites.

## See also

Other model fitting:
[`check_newdata()`](https://nicholasjclark.github.io/mvgam/dev/reference/check_newdata.md),
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md),
[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md),
[`mvgam_data()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_data.md),
[`mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_formula.md),
[`mvgam_multiple()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_multiple.md),
[`pivot_detection_array()`](https://nicholasjclark.github.io/mvgam/dev/reference/pivot_detection_array.md),
[`series_to_mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/series_to_mvgam.md),
[`stancode.mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/stancode.mvgam_formula.md),
[`update.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/update.mvgam.md)

## Author

Nicholas J Clark

## Examples

``` r
# Counts of three species at five sites, one column per species
Y <- matrix(c(3, 0, 5, 2, 7,
              1, 1, 0, 4, 2,
              0, 6, 2, 3, 1), nrow = 5,
            dimnames = list(NULL, c("ant", "bee", "wasp")))
long <- pivot_species_matrix(
  Y,
  site_covs = data.frame(elev = c(120, 340, 560, 780, 910)),
  traits = data.frame(body_size = c(2.1, 11.5, 16.0))
)
# One row per site and species, each carrying its site's elevation
# and its species' body size
head(long)
#>   series time y elev body_size
#> 1    ant    1 3  120       2.1
#> 2    bee    1 1  120      11.5
#> 3   wasp    1 0  120      16.0
#> 4    ant    2 0  340       2.1
#> 5    bee    2 1  340      11.5
#> 6   wasp    2 6  340      16.0

# Next, fit the long frame with jsdgam(unit = time, species = series)
```
