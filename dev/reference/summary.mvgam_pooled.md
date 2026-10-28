# Summary of a fit pooled over imputations

The summary of a fit from
[`mvgam_multiple()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_multiple.md).
The estimates come from the pooled draws. Chains from different
imputations sample different posteriors, which raises the R-hat of the
pooled chains. Each parameter's `Rhat` is its largest over the
imputation fits instead. `Bulk_ESS` and `Tail_ESS` are the sums over
those fits. A table gives each fit's largest R-hat and smallest ESS over
its parameters.

## Usage

``` r
# S3 method for class 'mvgam_pooled'
summary(object, probs = c(0.025, 0.975), robust = FALSE, matrices = FALSE, ...)

# S3 method for class 'mvgam_pooled_summary'
print(x, digits = 2, ...)
```

## Arguments

- object:

  A fit of class `mvgam_pooled`.

- probs:

  Two probabilities for the lower and upper bounds of the credible
  interval. Default `c(0.025, 0.975)`, a 95% interval.

- robust:

  Logical. If `FALSE` (the default), uses mean and standard deviation
  for summaries. If `TRUE`, uses median and median absolute deviation
  (MAD).

- matrices:

  Logical; if `TRUE`, also print every cell of the matrices a
  correlated, VAR or hierarchical trend derives from its sampled
  parameters (`Phi_trend`, `Sigma_trend`, `Omega_trend`,
  `Phi_group_trend`, `Sigma_group_trend`). The default `FALSE` prints
  the sampled parameters alone, among them `sigma_trend`, `Amu_trend`,
  `Aomega_trend` and `alpha_cor_trend`.
  [`posterior_transition_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_transition_matrix.md)
  and
  [`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md)
  summarise the matrices a cell at a time.

- ...:

  Unused. Anything passed here is refused.

- x:

  An object of class `mvgam_pooled_summary`.

- digits:

  Integer number of decimal places to print.

## Value

[`summary()`](https://rdrr.io/r/base/summary.html) returns an object of
class `c("mvgam_pooled_summary", "mvgam_summary")`. It holds the blocks
of
[`summary.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam.md)
and `mi_diagnostics`, a list of the number of imputations, the draws per
imputation and the per-imputation convergence table.
[`print()`](https://rdrr.io/r/base/print.html) returns `x` invisibly.

## See also

[`mvgam_multiple()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_multiple.md),
[`summary.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam.md)
