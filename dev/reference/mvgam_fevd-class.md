# `mvgam_fevd` object description

The objects returned by
[`fevd()`](https://nicholasjclark.github.io/mvgam/dev/reference/fevd.mvgam.md).
Run `methods(class = "mvgam_fevd")` and
`methods(class = "mvgam_fevd_summary")` to see an overview of available
methods.

## Details

A forecast error variance decomposition quantifies how much of the
forecast uncertainty in one series of a Vector Autoregression is
attributable to each of the others. It is built from the orthogonalised
impulse response coefficient matrices \\\Psi_h\\, which give the
contribution of series \\j\\ to the h-step forecast error variance of
series \\k\\: \$\$ \sigma_k^2(h) = \sum\_{j=1}^K(\psi\_{kj, 0}^2 +
\ldots + \psi\_{kj, h-1}^2) \quad \$\$ Dividing the orthogonalised
responses \\(\psi\_{kj, 0}^2 + \ldots + \psi\_{kj, h-1}^2)\\ by the
forecast error variance \\\sigma_k^2(h)\\ gives the proportion of the
forecast error variance for \\k\\ explained by an exogenous shock to
\\j\\. These objects hold that proportion.

The orthogonalisation takes the lower Cholesky factor of the innovation
covariance. The decomposition then depends on the order of the series.
The first series takes all of its own one-step forecast error variance.
Each later series shares its one-step variance with the series before
it. The order is that of the levels of the series factor.

A decomposition is a `K` by `K` matrix per horizon per posterior draw,
which on a wide panel runs to hundreds of megabytes. A reader needs only
a median and an interval from them.
[`fevd()`](https://nicholasjclark.github.io/mvgam/dev/reference/fevd.mvgam.md)
returns that summary. The two forms have different classes:

- `mvgam_fevd_summary`, the default. A long-format `tibble` inheriting
  from `mvgam_var_surface_summary`, with one row per shock-response pair
  per horizon. `shock` names the pair as `"<from> -> <to>"`, taking the
  model's own series names where the latent processes are the series and
  `"Process_k"` where a factor VAR's processes are latent factors
  instead. `horizon` counts the steps from the forecast origin. Three
  columns hold the posterior median and the interval bounds, named for
  the percentiles asked for: `fevdQ50`, `fevdQ2.5` and `fevdQ97.5` by
  default.

- `mvgam_fevd`, returned under `summary = FALSE`. A `list` with one
  element per posterior draw, each holding the decomposition matrices
  for that draw. Pass it to
  [`summary()`](https://rdrr.io/r/base/summary.html) to reach the form
  above.

Both forms take `ndraws` and `draw_ids`. A subset of the posterior
computes faster.

## References

Lutkepohl H (2005). New Introduction to Multiple Time Series Analysis.
Springer, Berlin.
[doi:10.1007/978-3-540-27752-1](https://doi.org/10.1007/978-3-540-27752-1)

## See also

[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md),
[`VAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md),
[`fevd()`](https://nicholasjclark.github.io/mvgam/dev/reference/fevd.mvgam.md),
[`irf()`](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md)

## Author

Nicholas J Clark
