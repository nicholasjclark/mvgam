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
  prob_threshold = 0.95,
  lag = 1L
)

# S3 method for class 'mvgam_var_matrix'
print(x, digits = 2L, ...)

# S3 method for class 'mvgam_var_matrix'
plot(x, cluster = FALSE, supported_only = FALSE, ...)

# S3 method for class 'mvgam_var_matrix_list'
print(x, ...)

# S3 method for class 'mvgam_var_matrix_list'
plot(
  x,
  type = c("heatmap", "diagonal"),
  ncol = NULL,
  supported_only = FALSE,
  ...
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
  `plot.mvgam_var_matrix_list()`.

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

- prob_threshold:

  The posterior probability of its sign a cell needs to count as
  supported, at least 0.5 and below 1. Defaults to 0.95.
  `plot.mvgam_var_matrix()` outlines the supported cells.

- lag:

  The lag of the matrix, at most the order `p` of the fitted `VAR(p)`.

- x:

  An `mvgam_var_matrix` or `mvgam_var_matrix_list`.

- digits:

  Integer number of digits in the printed matrix. Defaults to 2.

- ...:

  Unused. Anything passed here is refused.

- cluster:

  Logical. `TRUE` orders the processes by a Robinson clustering of
  `1 - A`, which suits a matrix close to symmetric.

- supported_only:

  Logical. `TRUE` leaves blank every cell whose `prob_nonzero` is at
  most `prob_threshold`. Defaults to `FALSE`.

- type:

  For several panels, `"heatmap"` (the default) or `"diagonal"`.

- ncol:

  The number of facet columns of the heatmaps, or `NULL` for
  `ceiling(sqrt(length(x)))`.

## Value

With `summary = TRUE`, an `mvgam_var_matrix`: a list of the matrices
`A`, `A_se`, `A_lower`, `A_upper`, `A_ess`, `prob_positive`,
`prob_negative` and `prob_nonzero`, with `n_series`, `series_names`,
`group_label`, `probs`, `prob_threshold` and `lag`. With
`summary = FALSE`, the array of draws. Several panels give a list of
either, an `mvgam_var_matrix_list` when summarised.

[`print()`](https://rdrr.io/r/base/print.html) gives the panel, the
dimension, the mean absolute off-diagonal cell and the estimate of the
matrix. For several panels it lists each panel's mean absolute
off-diagonal cell. The panel with the largest has the strongest effects
between processes.

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) draws the
matrix as a heatmap. Row \\i\\, column \\j\\ is the effect of process
\\j\\ at time \\t - l\\ on process \\i\\ at time \\t\\. A black outline
marks each cell whose `prob_nonzero` exceeds `prob_threshold`. The
colour scale is symmetric about zero. Several panels share one scale.
Under `type = "diagonal"` they draw the persistence of each process as a
point with its interval.

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
`print.mvgam_var_matrix()`, `plot.mvgam_var_matrix()`.

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
# Ten years of monthly counts of a prey species and its predator.
# Predators suppress the prey a month later. Prey raise the
# predators a month later.
set.seed(2025)
n_months <- 120
A <- rbind(prey     = c(0.6, -0.5),
           predator = c(0.4,  0.5))
z <- matrix(0, n_months, 2)
for (t in 2:n_months) {
  z[t, ] <- A %*% z[t - 1, ] + rnorm(2, sd = 0.35)
}
predator_prey <- data.frame(
  time = rep(seq_len(n_months), 2),
  series = factor(rep(c("prey", "predator"), each = n_months),
                  levels = c("prey", "predator")),
  y = rpois(2 * n_months, exp(c(2.5 + z[, 1], 1.5 + z[, 2])))
)
var_mod <- mvgam(y ~ series, trend_formula = ~ VAR(),
                 data = predator_prey, family = poisson(),
                 chains = 2, silent = 2)
# }
# \dontrun{
# Each series' persistence and its effects on the others
A <- posterior_transition_matrix(var_mod)
A
#> Lag-1 VAR transition matrix from an mvgam fit
#>   Group      : single-panel
#>   Dimension  : 2 x 2
#>   Mean |A_ij| (i != j): 0.53
#>   Point estimate:
#>          prey predator
#> prey     0.44    -0.58
#> predator 0.47     0.50
#>   95% CI on each cell available in `A_lower` / `A_upper`.
plot(A)


# Next, irf(var_mod) traces how a shock to one series spreads
# }
```
