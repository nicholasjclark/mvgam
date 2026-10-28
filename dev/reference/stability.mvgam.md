# Calculate measures of latent VAR community stability

Compute reactivity, return rates and contributions of interactions to
stationary forecast variance from mvgam models with Vector
Autoregressive dynamics.

## Usage

``` r
stability(object, ...)

# S3 method for class 'mvgam'
stability(
  object,
  groups = NULL,
  ndraws = NULL,
  draw_ids = NULL,
  summary = TRUE,
  probs = c(0.025, 0.975),
  future = FALSE,
  ...
)
```

## Arguments

- object:

  An object of class `mvgam` fitted with a `VAR(p = 1)` latent process
  model

- ...:

  Unused. Anything passed here is refused.

- groups:

  Hierarchical VAR fits only. The panels to summarise, by name or by
  index into the levels of `gr`. The default `NULL` summarises every
  panel. A non-hierarchical fit accepts `NULL` alone.

- ndraws:

  Positive integer specifying number of posterior draws to use. NULL
  (default) uses all available draws.

- draw_ids:

  Optional integer vector selecting a subset of posterior draw indices
  to use. NULL (default) uses all draws (subject to `ndraws`).

- summary:

  Logical. `TRUE` (the default) returns the posterior median and
  interval of each metric. `FALSE` returns the metrics of each draw.
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) draws
  histograms of either form.

- probs:

  The lower and upper percentiles of the interval reported with the
  median when `summary = TRUE`.

- future:

  `Logical`. When `TRUE`, the per-draw computation runs under the
  [`future::plan()`](https://future.futureverse.org/reference/plan.html)
  the caller has set. The default `sequential` plan runs as base
  [`lapply()`](https://rdrr.io/r/base/lapply.html) does.
  `plan(multisession, workers = N)` splits the draws across `N` R
  processes. Without the future package the draws run in turn, with a
  message.

## Value

A `data.frame` with the posterior median and interval of each stability
metric, or under `summary = FALSE` an object of class `mvgam_stability`
holding one row of metrics per posterior draw.

## Details

These measures describe how much the lagged interactions among the
series contribute to the variability of the system. They also describe
how the system responds to a perturbation (Ives et al. 2003). For a
latent VAR(1)

\$\$ x_t \sim \text{MVNormal}(A x\_{t - 1}, \Sigma) \$\$

the stationary covariance \\\Sigma\_{\infty}\\ solves \\\Sigma\_{\infty}
= A \Sigma\_{\infty} A^\top + \Sigma\\. Each posterior draw of \\A\\ and
\\\Sigma\\ gives one draw of every metric:

- `prop_int`: the proportion of the volume of the stationary
  distribution that the lagged interactions generate, \\\det(A)^2\\.

      \item `prop_int_adj`: `prop_int` scaled by the number of series
      \eqn{p}, \eqn{|\det(A)|^{2/p}}. It compares across systems of
      different size.

      \item `prop_int_offdiag`, `prop_int_diag`: the sensitivity of
      `prop_int` to each cell of \eqn{A} is
      \eqn{2 \det(A) (A^{-1})^\top}. `prop_int_offdiag` is the mean
      absolute sensitivity of the off-diagonal cells (interactions
      between series) as a share of that mean plus the mean absolute
      sensitivity of the diagonal cells (self-regulation).
      `prop_int_diag` is the remaining share.

      \item `prop_cov_offdiag`, `prop_cov_diag`: the same split for the
      sensitivity of \eqn{\det(\Sigma_{\infty})} to each cell of
      \eqn{\Sigma_{\infty}}, \eqn{\det(\Sigma_{\infty})
          (\Sigma_{\infty}^{-1})^\top}. Off-diagonal cells are counted
      where \eqn{\Sigma} is estimated.

      \item `reactivity`: the log of the largest singular value of
      \eqn{A}, \eqn{\log \sigma_{max}(A)} (Neubert et al. 2009). A
      positive value means some perturbation grows in the step after
      it occurs, before the system returns to equilibrium.

      \item `mean_return_rate`: the largest modulus of the eigenvalues
      of \eqn{A}. The distance of the mean from its stationary value
      shrinks by this factor each step, asymptotically. A value nearer
      1 means a slower return.

      \item `var_return_rate`: the same factor for the variance, the
      largest modulus of the eigenvalues of \eqn{A \otimes A}, which
      is `mean_return_rate` squared.

The prior on \\A\\ keeps every draw stationary (Heaps 2023).
\\\Sigma\_{\infty}\\ and every metric then exist in every draw.

[`irf()`](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md)
and
[`fevd()`](https://nicholasjclark.github.io/mvgam/dev/reference/fevd.mvgam.md)
describe the same dynamics as responses to shocks.

## References

AR Ives, B Dennis, KL Cottingham & SR Carpenter (2003). Estimating
community stability and ecological interactions from time-series data.
*Ecological Monographs*, 73, 301-330.

MG Neubert, H Caswell & AR Solow (2009). Detecting reactivity.
*Ecology*, 90, 2683-2688.

SE Heaps (2023). Enforcing stationarity through the prior in vector
autoregressions. *Journal of Computational and Graphical Statistics*,
32, 74-83.

## See also

[`irf()`](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md)
and
[`fevd()`](https://nicholasjclark.github.io/mvgam/dev/reference/fevd.mvgam.md)
summarise the same VAR dynamics. The [VAR
article](https://nicholasjclark.github.io/mvgam/articles/var.html) runs
`stability()` on a plankton community. The [hierarchical VAR
article](https://nicholasjclark.github.io/mvgam/articles/hierarchical_var.html)
reports the metrics for each group of a hierarchical VAR.

Other latent dynamics:
[`active_factors()`](https://nicholasjclark.github.io/mvgam/dev/reference/active_factors.md),
[`compare_loadings()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_loadings.md),
[`fevd.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/fevd.mvgam.md),
[`irf.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md),
[`ordinate()`](https://nicholasjclark.github.io/mvgam/dev/reference/ordinate.md),
[`posterior_transition_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_transition_matrix.md),
[`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md),
[`shared_variation()`](https://nicholasjclark.github.io/mvgam/dev/reference/shared_variation.md)

## Author

Nicholas J Clark

## Examples

``` r
# \dontrun{
set.seed(0)
simdat <- sim_mvgam(family = gaussian(), n_series = 3L,
                    n_timepoints = 120L, trend_model = VAR(),
                    prop_trend = 0.95)
var_mod <- mvgam(y ~ 1, trend_formula = ~ VAR(p = 1),
                 data = simdat$data_train, family = gaussian(),
                 chains = 2, silent = 2)
#> Warning: 3 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 1 of 2 chains had an E-BFMI less than 0.3.
#> See https://mc-stan.org/misc/warnings for details.

# Posterior medians and intervals of the stability metrics
stab <- stability(var_mod)
stab[stab$metric %in% c("reactivity", "mean_return_rate"), ]
#>             metric    Estimate  Est.Error       Q2.5     Q97.5
#> 7       reactivity -0.07064339 0.11252483 -0.2705272 0.3302954
#> 8 mean_return_rate  0.83169171 0.06748073  0.6661313 0.9353162

# The full posterior of each metric
plot(stability(var_mod, summary = FALSE))


# Next: irf() traces a shock through the system over time
# }
```
