# Convert time series objects to mvgam long format

Convert a `tsibble`, a `ts`, `mts` or `msts` object, or a wide
`data.frame` with a date column into the long `data.frame` that
[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
expects: one row per series and time, a `series` factor and a `time`
column counting steps from the first index value.

## Usage

``` r
series_to_mvgam(
  series,
  freq = NULL,
  response = NULL,
  index = NULL,
  h = NULL,
  train_prop = NULL
)
```

## Arguments

- series:

  The data to convert. One of:

  - A `tsibble`
    ([`tsibble::tsibble()`](https://tsibble.tidyverts.org/reference/tsibble.html)).
    Its key variables define the series and its index defines the time
    grid. Gaps in a regular `tsibble` become rows with a missing
    response, as `tsibble::fill_gaps(.full = TRUE)` builds them.
    Measured variables other than the response are carried over as
    covariates, with missing values on those rows. An irregular
    `tsibble` keeps its own times, for use with
    [`CAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
    or
    [`ZMVN()`](https://nicholasjclark.github.io/mvgam/dev/reference/ZMVN.md).

  - A `ts`, `mts` or `msts` object (`forecast::msts()`). Each column of
    an `mts` is a series.

  - A wide `data.frame` holding one date column and one numeric column
    per series. Column names become the series levels, in column order.

- freq:

  Optional numeric vector of seasonal periods, in time steps. Each
  period above 1 adds a column giving the position of each row within
  that period. Defaults to the periods the object carries:
  [`frequency()`](https://rdrr.io/r/stats/time.html) of a `ts`, the
  `seasonal.periods` of an `msts`, or
  [`tsibble::guess_frequency()`](https://tsibble.tidyverts.org/reference/guess_frequency.html)
  of the index.

- response:

  Name of the measured variable to use as the response of a `tsibble`.
  Optional when the `tsibble` has one measured variable. Ignored for
  other inputs, whose response column is `y`.

- index:

  Name of the date column of a wide `data.frame`. Optional when exactly
  one column has class `Date`, `POSIXt`, `yearmonth`, `yearquarter` or
  `yearweek`.

- h:

  Optional number of final time steps to hold out as test data.

- train_prop:

  Optional proportion of time steps to keep as training data, between 0
  and 1. Only one of `h` and `train_prop` may be given.

## Value

A long `data.frame`. Its columns are `series` (a factor), `time` (an
integer for regular data), the response and the original index (`index`
for a `ts`). Any `tsibble` key and covariate columns follow, then one
season column per period above 1. Rows are ordered by time, then series.
With `h` or `train_prop`, a list holding `data_train` and `data_test`
instead.

## Details

`time` counts steps from 1 at the first index value. The position within
a seasonal period counts steps from the calendar origin of the index. A
monthly index with `freq = 12` gives the month number whatever month the
data start in. A `ts` gives
[`cycle()`](https://rdrr.io/r/stats/time.html). A single period produces
a column named `season`. Several periods produce `season_<period>`
columns, such as `season_7` and `season_365.25`.

Rows for future times with a missing response, built for example with
[`tsibble::new_data()`](https://tsibble.tidyverts.org/reference/new-data.html)
and bound to the observed data, receive `time` values that continue the
grid. Converting observed and future rows together keeps the two
aligned.

Seasonal periods take the same numbers across packages. Monthly data
carry `frequency = 12` in a `ts`. A fable `season(period = "year")` on a
monthly `tsibble` resolves to 12 steps. Both take `freq = 12`. Daily
data with `seasonal.periods = c(7, 365.25)` in an `msts` take
`freq = c(7, 365.25)`.

A `season` column feeds a fixed seasonal shape, such as
`s(season, bs = "cc")`. The same periods give
[`LLT()`](https://nicholasjclark.github.io/mvgam/dev/reference/LLT.md) a
season whose shape drifts over the years, as
`trend_formula = ~ LLT(season = 12)`.

## See also

The [data
article](https://nicholasjclark.github.io/mvgam/articles/data.html)
covers the long format and its conversion in more depth.
[`LLT()`](https://nicholasjclark.github.io/mvgam/dev/reference/LLT.md)
takes the same periods for a season that drifts.

Other model fitting:
[`check_newdata()`](https://nicholasjclark.github.io/mvgam/dev/reference/check_newdata.md),
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md),
[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md),
[`mvgam_data()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_data.md),
[`mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_formula.md),
[`mvgam_multiple()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_multiple.md),
[`pivot_detection_array()`](https://nicholasjclark.github.io/mvgam/dev/reference/pivot_detection_array.md),
[`pivot_species_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/pivot_species_matrix.md),
[`stancode.mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/stancode.mvgam_formula.md),
[`update.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/update.mvgam.md)

## Author

Nicholas J Clark

## Examples

``` r
# Monthly airline passengers, holding out the final year
air <- series_to_mvgam(AirPassengers, h = 12)
head(air$data_train)
#>     series time   y    index season
#> 1 series_1    1 112 1949.000      1
#> 2 series_1    2 118 1949.083      2
#> 3 series_1    3 132 1949.167      3
#> 4 series_1    4 129 1949.250      4
#> 5 series_1    5 121 1949.333      5
#> 6 series_1    6 135 1949.417      6

# Two series in one mts
deaths <- series_to_mvgam(cbind(male = mdeaths, female = fdeaths))
levels(deaths$series)
#> [1] "male"   "female"

# A wide data.frame with a date column
wide <- data.frame(
  date = seq(as.Date("2024-01-01"), by = "day", length.out = 28),
  site_a = rpois(28, 5),
  site_b = rpois(28, 8)
)
head(series_to_mvgam(wide))
#>   series time y       date season
#> 1 site_a    1 4 2024-01-01      5
#> 2 site_b    1 7 2024-01-01      5
#> 3 site_a    2 4 2024-01-02      6
#> 4 site_b    2 6 2024-01-02      6
#> 5 site_a    3 5 2024-01-03      7
#> 6 site_b    3 6 2024-01-03      7

# Next: fit the training data, for example with
# mvgam(y ~ s(season, bs = "cc"), trend_formula = ~ AR(),
#       data = air$data_train)
```
