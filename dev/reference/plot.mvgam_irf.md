# Plot impulse responses from an `mvgam_irf` object

This function takes an `mvgam_irf` object and produces plots of Impulse
Response Functions

## Usage

``` r
# S3 method for class 'mvgam_irf'
plot(x, series = 1, responses = NULL, ...)
```

## Arguments

- x:

  `list` object of class `mvgam_irf`. See
  [`irf()`](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md)

- series:

  Position or name of the process given the shock

- responses:

  Optional positions or names of the processes drawn as response panels.
  Useful for hierarchical VAR fits where the raw K-panel grid is
  unreadable. Defaults to all processes.

- ...:

  Unused. Anything passed here is refused.

## Value

A `ggplot` object showing the expected response of each latent time
series to a shock of the focal `series`

## See also

[`irf()`](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md),
[`fevd()`](https://nicholasjclark.github.io/mvgam/dev/reference/fevd.mvgam.md),
[`stability()`](https://nicholasjclark.github.io/mvgam/dev/reference/stability.mvgam.md),
[`plot.mvgam_fevd()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_fevd.md),
[`plot.mvgam_stability()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_stability.md),
[`plot.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_forecast.md)

## Author

Nicholas J Clark
