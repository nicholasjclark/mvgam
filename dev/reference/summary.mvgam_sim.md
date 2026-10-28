# Summary for `sim_mvgam()` output

The design of a simulated data set and its true generative parameters.

## Usage

``` r
# S3 method for class 'mvgam_sim'
summary(object, ...)
```

## Arguments

- object:

  An `mvgam_sim` object returned by
  [`sim_mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/sim_mvgam.md).

- ...:

  Unused. Anything passed here is refused.

## Value

A list with class `mvgam_sim_summary` containing:

- `type`:

  The catalog type used.

- `family`:

  Family name (string).

- `trend`:

  Trend type label (string or `"None"`).

- `n_series`:

  Number of series.

- `n_timepoints`:

  Total number of timepoints.

- `n_train`:

  Number of training rows.

- `n_test`:

  Number of test rows.

- `true_betas`:

  Named numeric vector of population coefficients.

- `n_smooths`:

  Number of true smooth functions.

- `smooth_names`:

  Character vector of true smooth names.

- `true_trend_sigma`:

  Empirical SD of the latent trend across all series x time cells.

- `true_sigma_obs`:

  Observation-family scale / dispersion parameter (NA if family is fully
  discrete).
