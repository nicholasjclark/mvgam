# Posterior transition matrix of a VAR trend

The coefficient matrix \\A_l\\ of lag \\l\\ of a
[`VAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
trend. Row \\i\\ holds the effects of the processes at time \\t - l\\ on
process \\i\\ at time \\t\\. The diagonal holds each process's
persistence. The off-diagonal cells hold the effects of one process on
another.

## Usage

``` r
posterior_transition_matrix(
  object,
  groups = NULL,
  summary = TRUE,
  robust = FALSE,
  probs = c(0.025, 0.975),
  lag = 1L
)
```

## Arguments

- object:

  A fitted `mvgam` object whose trend is a VAR.

- groups:

  The panels of a hierarchical VAR, by level or by index, or `"all"`.
  `NULL` gives the mean of the panels' matrices. A single-panel fit
  takes `NULL` alone. One panel returns its matrix. Several, or `"all"`,
  return a list whose
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method is
  [`plot.mvgam_var_matrix_list()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_var_matrix_list.md).

- summary:

  Logical. `TRUE` returns the summary of each cell. `FALSE` returns the
  draws as a `[ndraws, K, K]` array, with the processes naming its
  second and third dimensions.

- robust:

  Logical. If `FALSE` (the default), uses mean and standard deviation
  for summaries. If `TRUE`, uses median and median absolute deviation
  (MAD).

- probs:

  Numeric vector of probabilities for quantile computation. Default is
  `c(0.025, 0.975)` for 95% credible intervals.

- lag:

  The lag of the matrix, at most the order `p` of the fitted `VAR(p)`.

## Value

With `summary = TRUE`, an `mvgam_var_matrix`: a list of the matrices
`A`, `A_se`, `A_lower`, `A_upper`, `A_ess`, `prob_positive`,
`prob_negative` and `prob_nonzero`, with `n_series`, `series_names`,
`group_label`, `probs` and `lag`. It has
[`print()`](https://rdrr.io/r/base/print.html) and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods. With
`summary = FALSE`, the array of draws. Several panels give a list of
either.

## Details

A hierarchical VAR (`VAR(gr = ...)`) has one matrix per panel. `groups`
names a panel to return its matrix. With `groups = NULL` the function
returns the mean of the panels' matrices, computed in each draw. The
hyperparameters `Amu_trend` and `Aomega_trend` pool unconstrained
matrices, which the stationarity map of Heaps (2023) turns into each
panel's matrix. Their mean is no transition matrix. The mean over panels
summarises the pool on the scale of the matrices.

Each cell is summarised as
[`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md)
summarises a correlation. The estimate is the posterior mean, or the
median with `robust = TRUE`. The interval takes the quantiles `probs`.
The effective sample size comes from
[`posterior::ess_basic()`](https://mc-stan.org/posterior/reference/ess_basic.html).
`prob_positive`, `prob_negative` and `prob_nonzero` are the shares of
draws above zero, below zero and away from zero.

## References

Savage J (2016). Hierarchical Vector Autoregression. RPubs, 27 November
2016. <https://rpubs.com/jimsavage/hierarchical_var>

Heaps SE (2023). Enforcing stationarity through the prior in vector
autoregressions. *Journal of Computational and Graphical Statistics*
32(1), 74-83.
[doi:10.1080/10618600.2022.2079648](https://doi.org/10.1080/10618600.2022.2079648)

## See also

[`VAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md),
[`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md),
[`irf()`](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md),
[`fevd()`](https://nicholasjclark.github.io/mvgam/dev/reference/fevd.mvgam.md),
[`stability()`](https://nicholasjclark.github.io/mvgam/dev/reference/stability.mvgam.md),
[`print.mvgam_var_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/print.mvgam_var_matrix.md),
[`plot.mvgam_var_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_var_matrix.md).

Other latent dynamics:
[`active_factors()`](https://nicholasjclark.github.io/mvgam/dev/reference/active_factors.md),
[`compare_loadings()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_loadings.md),
[`fevd.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/fevd.mvgam.md),
[`irf.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md),
[`ordinate()`](https://nicholasjclark.github.io/mvgam/dev/reference/ordinate.md),
[`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md),
[`shared_variation()`](https://nicholasjclark.github.io/mvgam/dev/reference/shared_variation.md),
[`stability.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/stability.mvgam.md)

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
#> Warning: 44 of 2000 (2.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 1 of 2 chains had an E-BFMI less than 0.3.
#> See https://mc-stan.org/misc/warnings for details.

# Each series' persistence and its effects on the others
A <- posterior_transition_matrix(var_mod)
A
#> Lag-1 VAR transition matrix from an mvgam fit
#>   Group      : single-panel
#>   Dimension  : 3 x 3
#>   Mean |A_ij| (i != j): 0.11
#>   Point estimate:
#>          series_1 series_2 series_3
#> series_1     0.71     0.02     0.10
#> series_2     0.10     0.54     0.16
#> series_3     0.20     0.08     0.51
#>   95% CI on each cell available in `A_lower` / `A_upper`.
plot(A)


# Next, irf(var_mod) traces how a shock to one series spreads
# }
```
