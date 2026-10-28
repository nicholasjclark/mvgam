# `mvgam_irf` object description

The objects returned by
[`irf()`](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md).
Run `methods(class = "mvgam_irf")` and
`methods(class = "mvgam_irf_summary")` to see an overview of available
methods.

## Details

Generalized or Orthogonalized Impulse Response Functions are computed
from the posterior estimates of Vector Autoregressive parameters.
[`irf()`](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md)
applies a positive shock of one standard deviation to one process and
calculates how each process in the latent VAR responds over `h`
horizons, for every process in turn. Horizon 1 is the time step of the
shock.
[`stability()`](https://nicholasjclark.github.io/mvgam/dev/reference/stability.mvgam.md)
computes community-level metrics of stability from the same parameters.

A shock response is a `K` by `K` matrix per horizon per posterior draw.
On a wide panel the draws run to hundreds of megabytes, where a median
and an interval are what a reader uses.
[`irf()`](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md)
returns the summary by default. The two forms carry different classes:

- `mvgam_irf_summary`, the default. A long-format `tibble` inheriting
  from `mvgam_var_surface_summary`, with one row per shock-response pair
  per horizon. `shock` names the pair as `"<from> -> <to>"`, taking the
  model's own series names where the latent processes are the series and
  `"Process_k"` where a factor VAR's processes are latent factors
  instead. `horizon` counts from 1, the time step of the shock. Three
  further columns hold the posterior median and the interval bounds,
  named for the percentiles asked for: `irfQ50`, `irfQ2.5` and
  `irfQ97.5` by default. An `irf_type` attribute records whether the
  responses are orthogonalised.

- `mvgam_irf`, returned under `summary = FALSE`. A `list` with one
  element per posterior draw, each itself a `list` holding one `h` by
  `K` matrix of responses per shocked process. Pass it to
  [`summary()`](https://rdrr.io/r/base/summary.html) to reach the form
  above.

Both forms take `ndraws` and `draw_ids`. A subset of the posterior
shortens the computation.

## References

Pesaran HH and Shin Y (1998). Generalized impulse response analysis in
linear multivariate models. Economics Letters 58, 17-29.
[doi:10.1016/S0165-1765(97)00214-0](https://doi.org/10.1016/S0165-1765%2897%2900214-0)

## See also

[mvgam](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md),
[VAR](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md),
[irf](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md),
[stability](https://nicholasjclark.github.io/mvgam/dev/reference/stability.mvgam.md)

## Author

Nicholas J Clark
