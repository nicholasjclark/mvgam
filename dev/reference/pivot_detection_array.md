# Convert a detection array to the long format of mvgam

Occupancy and N-mixture packages such as unmarked, ubms, flocker and
spOccupancy store detection histories as matrices or arrays, with one
dimension for sites and one for visits.
[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
and
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
with
[`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md)
or
[`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md)
take a long data frame instead, with one row per species, site, season
and visit. This function converts the one to the other and copies the
covariates onto the matching rows.

Data from one season give `time = site`. Each site is then one closure
unit. Data from several seasons take one of two layouts, chosen by
`multi_season`:

- `"fused"` (the default) gives each site in each season its own closure
  unit, with `time = (site - 1) * T + season` for \\T\\ seasons. Use it
  when the seasons are independent given the covariates.

- `"hierarchical"` keeps `time = season`. Fit it with
  `occ(multi_season = TRUE)` or `nmix(multi_season = TRUE)`. Name `site`
  in the observation formula, as in `s(site, bs = "re")`. The latent
  trend then runs over seasons. The model has no explicit colonisation
  or extinction rates.

## Usage

``` r
pivot_detection_array(
  y,
  site_covs = NULL,
  season_covs = NULL,
  site_season_covs = NULL,
  obs_covs = NULL,
  species = NULL,
  site_col = "site",
  season_col = "season",
  visit_col = "visit",
  time_col = "time",
  series_col = "series",
  y_col = "y",
  multi_season = c("fused", "hierarchical")
)
```

## Arguments

- y:

  One of four layouts:

  - **Single-species, single-season**: a `[J_sites, K_max_visits]`
    matrix of detections (0 / 1 for
    [`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md),
    non-negative counts for
    [`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md)).
    The `unmarked::unmarkedFrameOccu` / `unmarkedFramePCount` `y` slot
    and the `flocker::flock` `obs` argument with `type = "single"`.

  - **Multi-species, single-season (array)**: a
    `[N_species, J_sites, K_max_visits]` array. The
    `spOccupancy::lfMsPGOcc` / `msPGOcc` `y` slot.

  - **Multi-species, single-season (list)**: a named list of
    `[J_sites, K_max_visits]` matrices, one per species. The
    `unmarked::unmarkedFrameOccuMulti` `y` slot; list names become
    species labels.

  - **Multi-species, multi-season**: a
    `[N_species, J_sites, T_seasons, K_max_visits]` 4D array. The
    `spOccupancy::tMsPGOcc` / `svcTMsPGOcc` `y` slot. For single-species
    multi-season data, wrap in a length-1 list
    (`list(only_species = matrix_or_3d_array)`) or pass as a
    `[1, J, T, K]` array.

  `NA` entries mark missing visits / seasons and are dropped from the
  output.

- site_covs:

  Optional `data.frame` of site-level covariates with `nrow == J_sites`.
  Broadcast across visits, seasons and species. The
  `unmarked::unmarkedFrameOccu` `siteCovs` slot, the spOccupancy
  `occ.covs` and the `flocker` `unit_covs` map here.

- season_covs:

  Optional `data.frame` of season-level covariates with
  `nrow == T_seasons`. Broadcast across sites, visits and species. Use
  for year-of-study effects, season-of-year covariates, or any covariate
  that varies across primary periods but is constant within a season.

- site_season_covs:

  Optional named list of `[J_sites, T_seasons]` matrices for covariates
  that vary across the `(site, season)` interaction (e.g., site-level
  habitat that changes between seasons).

- obs_covs:

  Optional visit-level covariates:

  - Single-season: named list of `[J_sites, K_max_visits]` matrices, OR
    a long-format `data.frame` with `J_sites * K_max_visits` rows in
    site-major / visit-fastest order (the `unmarked` `obsCovs`
    convention).

  - Multi-season: named list of `[J_sites, T_seasons, K_max_visits]`
    arrays.

- species:

  Optional character vector of species names with `length == N_species`.
  Only meaningful for multi-species `y`. When `y` is a named list, list
  names take precedence. Defaults to `sp1, sp2, ...`.

- site_col:

  Name of the site identifier in the output. Defaults to `"site"`.

- season_col:

  Name of the season identifier in the output. Defaults to `"season"`.

- visit_col:

  Name of the visit identifier. Defaults to `"visit"`.

- time_col:

  Name of the time column in the output. Defaults to `"time"`. The
  Description gives its values for each layout.

- series_col:

  Name of the species column. Defaults to `"series"`.

- y_col:

  Name of the detection / count column. Defaults to `"y"`. This and the
  other `*_col` arguments must each name a different output column.

- multi_season:

  `"fused"` (the default) or `"hierarchical"`, the layout for data from
  several seasons. The Description explains both. Data from one season
  ignore it: a matrix, a `[N, J, K]` array or a four-dimensional array
  with \\T = 1\\.

## Value

A long `data.frame` with the columns `series`, `site`, `season`,
`visit`, `time` and `y`, followed by the covariates. Data from one
species have no `series` column. Data from one season have no `season`
column. Rows where `y` is `NA` are dropped. Units may then differ in
their number of visits.

## See also

[`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md)
and
[`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md)
for the families that take this layout.
[`pivot_species_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/pivot_species_matrix.md)
converts a site-by-species matrix.

Other model fitting:
[`check_newdata()`](https://nicholasjclark.github.io/mvgam/dev/reference/check_newdata.md),
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md),
[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md),
[`mvgam_data()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_data.md),
[`mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_formula.md),
[`mvgam_multiple()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_multiple.md),
[`pivot_species_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/pivot_species_matrix.md),
[`series_to_mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/series_to_mvgam.md),
[`stancode.mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/stancode.mvgam_formula.md),
[`update.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/update.mvgam.md)

## Author

Nicholas J Clark

## Examples

``` r
# Detections of one species at four sites over three visits, laid out
# as the `y` slot of an unmarked frame. NA marks a missed visit
y <- matrix(c(0, 1, 1,
              0, 0, NA,
              1, 1, 0,
              0, 0, 0), nrow = 4, byrow = TRUE)
long <- pivot_detection_array(
  y,
  site_covs = data.frame(forest = c(0.2, 0.8, 0.5, 0.1)),
  obs_covs = list(wind = matrix(seq(0.1, 1.2, by = 0.1), nrow = 4))
)
# One row per observed visit. The missed visit is dropped
long
#>    site visit time y forest wind
#> 1     1     1    1 0    0.2  0.1
#> 2     1     2    1 1    0.2  0.5
#> 3     1     3    1 1    0.2  0.9
#> 4     2     1    2 0    0.8  0.2
#> 5     2     2    2 0    0.8  0.6
#> 6     3     1    3 1    0.5  0.3
#> 7     3     2    3 1    0.5  0.7
#> 8     3     3    3 0    0.5  1.1
#> 9     4     1    4 0    0.1  0.4
#> 10    4     2    4 0    0.1  0.8
#> 11    4     3    4 0    0.1  1.2

# Three species in a [species, site, visit] array, as spOccupancy
# stores them. Each row names its species in 'series'
set.seed(1)
y3 <- array(rbinom(36, size = 1, prob = 0.4), dim = c(3, 4, 3))
head(pivot_detection_array(y3, species = c("owl", "hawk", "kite")))
#>   series site visit time y
#> 1    owl    1     1    1 0
#> 2    owl    1     2    1 1
#> 3    owl    1     3    1 0
#> 4   hawk    1     1    1 0
#> 5   hawk    1     2    1 0
#> 6   hawk    1     3    1 0

# Next, fit the long frame with mvgam(family = occ())
```
