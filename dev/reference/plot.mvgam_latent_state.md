# Plot the posterior latent state

Faceted plot of the posterior median with its 50% and 95% credible
intervals per series. Over a time axis the state is drawn as a line with
ribbons. Units without an order, such as the sites of a single-season
design, are drawn as points with interval bars.

## Usage

``` r
# S3 method for class 'mvgam_latent_state'
plot(x, series = NULL, ...)
```

## Arguments

- x:

  A `mvgam_latent_state` object.

- series:

  Names or indices of the series to plot, or `NULL` for every series

- ...:

  Unused. Anything passed here is refused.

## Value

A `ggplot` object.
