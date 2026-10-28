# Fields of an mvgam_trend object

Every trend constructor,
[`AR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md),
[`RW()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md),
[`VAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md),
[`CAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md),
[`PW()`](https://nicholasjclark.github.io/mvgam/dev/reference/piecewise_trends.md)
and
[`ZMVN()`](https://nicholasjclark.github.io/mvgam/dev/reference/ZMVN.md),
returns an object of class `mvgam_trend`. The object records the
arguments of the call. Model building adds the fields that need the
data.

## Fields every trend has

- trend:

  The trend type, such as `"AR"`. Stan code generation and the registry
  look the type up by this name.

- time, series:

  The names of the time and series columns, `"time"` and `"series"`
  unless given.

- gr, subgr:

  The names of the grouping and subgrouping columns of a hierarchical
  trend, or `"NA"`.

- ma:

  `TRUE` for a moving-average term.

- cor:

  `TRUE` for correlated innovations. A grouped trend always has them.

- validation_rules:

  What the trend assumes about the data. `"requires_regular_intervals"`
  marks a trend that indexes its lags by position. Fitting it requires
  evenly spaced times.
  [`CAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  and
  [`ZMVN()`](https://nicholasjclark.github.io/mvgam/dev/reference/ZMVN.md)
  carry no rule.

## Fields a constructor adds

- n_lv:

  The number of latent factors, or `NULL` for none.

- trend_map:

  The loadings as supplied to the constructor.

- p:

  The autoregressive lags of
  [`AR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  and
  [`VAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md).

- coef_sharing:

  How
  [`AR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  shares its coefficients across series.

- df:

  The degrees of freedom of the innovations: `Inf` for Gaussian, `NA` to
  estimate them.

- growth, n_changepoints, changepoint_range, changepoint_scale:

  The arguments of
  [`PW()`](https://nicholasjclark.github.io/mvgam/dev/reference/piecewise_trends.md).

- damped:

  `TRUE` for an
  [`LLT()`](https://nicholasjclark.github.io/mvgam/dev/reference/LLT.md)
  whose slope is damped.

- season, harmonics:

  The seasonal periods of an
  [`LLT()`](https://nicholasjclark.github.io/mvgam/dev/reference/LLT.md)
  and the number of harmonics of each, `NULL` without a season.

- cap:

  The name of the carrying-capacity column that a logistic
  [`PW()`](https://nicholasjclark.github.io/mvgam/dev/reference/piecewise_trends.md)
  trend uses, `"cap"` unless given.

## Fields model building adds

- fixed_Z:

  The loadings matrix a `trend_map` gives, with `NA` marking a sampled
  entry. `NULL` when every loading is sampled.

- loadings_prior_spec:

  The structured loadings prior built from the `loadings_prior` argument
  of
  [`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md).

- dimensions:

  The series and time axes the data defines.

## Parameters and priors

Which parameters a trend samples depends on its fields at the time the
model is built. A fully fixed `trend_map` samples no loadings. A
loadings prior with column shrinkage derives the innovation scale.
[`get_prior()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.md)
lists the parameters for a given model.

## See also

[`list_trend_types()`](https://nicholasjclark.github.io/mvgam/dev/reference/list_trend_types.md)

## Author

Nicholas J Clark
