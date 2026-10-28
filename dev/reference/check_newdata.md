# Validate `newdata` against the training data of an mvgam model

Runs the structural checks that
[`predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/predict.mvgam.md)
and
[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md)
apply to a `newdata` frame. A misaligned frame is refused before an
expensive prediction. The trend constructor in `trend_formula` names the
time and series columns, `time` and `series` by default. Each of them
the training `data` holds must also be in `newdata`. A hierarchical
trend identifies its series by its `gr` and `subgr` columns. Those
columns are then required. Series levels absent from the training data
are refused. The series column of `newdata` is coerced to the training
levels. The prediction functions check the columns the formulas use.

## Usage

``` r
check_newdata(newdata, data, trend_formula = NULL)
```

## Arguments

- newdata:

  A `data.frame` of prediction covariates in the same shape as the
  training data.

- data:

  The training `data.frame` of the model, whose series levels define the
  valid set. A fitted model from
  [`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
  or
  [`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
  is also accepted. `newdata` is then checked against every column the
  fitted formulas use. `trend_formula` is then ignored.

- trend_formula:

  The trend formula passed to
  [`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md),
  or `NULL` for a model without a trend.

## Value

`newdata` with its series column coerced to the training factor levels,
returned invisibly, or `NULL` (invisibly) when `newdata` is `NULL`.
Checked against a fitted model, `newdata` is returned with the columns
that model predicts from.

## See also

[`predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/predict.mvgam.md),
[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md)

Other model fitting:
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md),
[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md),
[`mvgam_data()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_data.md),
[`mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_formula.md),
[`mvgam_multiple()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_multiple.md),
[`pivot_detection_array()`](https://nicholasjclark.github.io/mvgam/dev/reference/pivot_detection_array.md),
[`pivot_species_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/pivot_species_matrix.md),
[`series_to_mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/series_to_mvgam.md),
[`stancode.mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/stancode.mvgam_formula.md),
[`update.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/update.mvgam.md)

## Examples

``` r
train <- data.frame(
  species = factor(rep(c("a", "b"), each = 3L)),
  week = rep(1:3, times = 2L),
  y = rnorm(6L)
)
test_weeks <- data.frame(
  species = factor(rep(c("a", "b"), each = 2L)),
  week = rep(4:5, times = 2L)
)
check_newdata(test_weeks, train, ~ AR(time = week, series = species))

# A species the model never saw is refused.
test_weeks$species <- factor(rep(c("a", "c"), each = 2L))
try(check_newdata(test_weeks, train, ~ AR(time = week, series = species)))
#> Error : Series in 'newdata' has levels absent from the training data.
#>   Unseen: "c".
#>   Fitted levels: "a" and "b".
```
