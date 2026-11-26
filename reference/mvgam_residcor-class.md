# `mvgam_residcor` object description

A `mvgam_residcor` object returned by function
[`residual_cor()`](https://nicholasjclark.github.io/mvgam/reference/residual_cor.md).
Run `methods(class = "mvgam_residcor")` to see the available methods.

## Usage

``` r
# S3 method for class 'mvgam_residcor'
plot(
  x,
  type = c("correlation", "precision"),
  cluster = FALSE,
  rescale = FALSE,
  supported_only = FALSE,
  ...
)

# S3 method for class 'mvgam_residcor_list'
plot(
  x,
  include_global = TRUE,
  ncol = NULL,
  rescale = FALSE,
  supported_only = FALSE,
  ...
)

# S3 method for class 'mvgam_residcor'
summary(object, ...)

# S3 method for class 'mvgam_residcor'
print(x, digits = 2L, ...)
```

## Arguments

- x:

  A summarised `mvgam_residcor` object, or for
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) an
  `mvgam_residcor_list`.

- type:

  Character. Which matrix to plot. `"correlation"` (the default) draws
  `x$cor`. `"precision"` draws `x$prec`, the partial correlations, which
  [`residual_cor()`](https://nicholasjclark.github.io/mvgam/reference/residual_cor.md)
  computes under `partial = TRUE`.

- cluster:

  Logical. `TRUE` reorders the matrix by an approximate Robinson
  ordering (Gruvaeus and Wainer 1972; average linkage on the
  `1 - cormat` distance). Clusters of positive and negative correlations
  then lie adjacent. Defaults to `FALSE`.

- rescale:

  Logical. By default colour spans `[-1, 1]`, the full range of a
  correlation, which keeps panels from different fits comparable. `TRUE`
  scales colour to the entries present, for a fit whose correlations are
  all small. Defaults to `FALSE`.

- supported_only:

  Logical. `TRUE` leaves blank every pair whose `prob_nonzero` is at
  most `prob_threshold`. Defaults to `FALSE`.

- ...:

  Unused. Anything passed here is refused.

- include_global:

  Logical, for an `mvgam_residcor_list`. `TRUE` (the default) adds the
  `_global` correlation as one panel.

- ncol:

  Optional number of facet columns. Defaults to
  `ceiling(sqrt(n_panels))`.

- object:

  A summarised `mvgam_residcor` object.

- digits:

  Integer number of significant digits. Defaults to 2.

## Value

Objects of this class are structured as a `list` with the following
components:

- cor, cor_se, cor_lower, cor_upper:

  Posterior summaries of the \\p \times p\\ correlation matrix: the
  posterior mean (the median under `robust = TRUE`), the posterior SD
  (the MAD under `robust = TRUE`) and the limits of the credible
  interval at the `probs` levels.

- cor_ess:

  \\p \times p\\ matrix of per-entry effective sample sizes of the
  correlation draws, computed on the Fisher-z scale with
  [`posterior::ess_basic()`](https://mc-stan.org/posterior/reference/ess_basic.html).
  The diagonal is `NA`, being fixed at 1. A low value marks a
  correlation the chains sampled poorly, however narrow its interval.

- prob_positive, prob_negative, prob_nonzero:

  \\p \times p\\ matrices of `Pr(r > 0)`, `Pr(r < 0)` and the larger of
  the two for every off-diagonal entry. Each measures how well the data
  determine the sign of a correlation.

- sig_cor:

  The correlation matrix with every entry set to zero where
  `prob_nonzero` is at most `prob_threshold`. `plot.mvgam_residcor()`
  draws `cor`. A hard cut would draw a correlation the data leave
  uncertain the same as one estimated at zero.

- cov, cov_se, cov_lower, cov_upper, cov_ess:

  Posterior summaries of the residual covariance matrix, with per-entry
  ESS.

- mean_abs_offdiag:

  A `list` of `point`, `lower` and `upper` summarising the mean absolute
  off-diagonal correlation. The mean is taken within each draw and then
  summarised across draws. The interval then describes the overall
  strength of correlation.

- n_series, series_names:

  Dimensions of the matrix and the series labels.

- pattern, hierarchical, group_label:

  The residual structure that was summarised.

- covariance:

  `"stationary"` for the covariance of the latent states, or
  `"innovation"` for that of one step's shocks, as
  [`residual_cor()`](https://nicholasjclark.github.io/mvgam/reference/residual_cor.md)
  was asked.

- probs, prob_threshold:

  The credible interval levels and the `prob_nonzero` threshold of
  `sig_cor`.

Passing `partial = TRUE` to
[`residual_cor()`](https://nicholasjclark.github.io/mvgam/reference/residual_cor.md)
adds the partial correlations, with one field for each correlation
field: `prec`, `prec_se`, `prec_lower`, `prec_upper`, `prec_ess`,
`prec_prob_positive`, `prec_prob_negative`, `prec_prob_nonzero` and the
thresholded `sig_prec`. Partial correlations are bounded like
correlations. They are summarised on the same Fisher-z scale.
`plot.mvgam_residcor()` with `type = "precision"` requires them.

## Details

In a latent factor model the residual covariance between series is
\\\Sigma = Z \Omega Z'\\, where \\Z\\ holds the factor loadings and
\\\Omega\\ is the covariance of the latent states. A strong residual
correlation between two species can reflect a species interaction
(facilitation or competition), a missing covariate, or any other
association the predictors in `formula` leave unexplained (Hui 2016).

The partial correlation between two series conditions on every other
series (Ovaskainen et al. 2016). It is computed from the inverse of the
correlation matrix, with the diagonal set to 1. Two species with a
partial correlation near zero can still be correlated. They are then
conditionally independent given the other species. Their association
runs *through* those species. A larger partial correlation suggests a
more direct association.

## Methods

[`print()`](https://rdrr.io/r/base/print.html) gives the source and kind
of covariance, the mean absolute correlation and the number of pairs
whose `prob_nonzero` exceeds `prob_threshold`.
[`summary()`](https://rdrr.io/r/base/summary.html) returns a tibble with
one row per pair of series, sorted by `prob_nonzero`: `series_1`,
`series_2`, `Estimate`, `Est.Error`, `Q_lower`, `Q_upper`, `ESS`, the
three sign probabilities and `sig`. A low `ESS` marks a pair the chains
sampled poorly, however tight its interval.

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) draws the lower
triangle of the correlation matrix as a heatmap of the point estimates.
A black outline marks each pair whose `prob_nonzero` exceeds
`prob_threshold`, the pairs `sig_cor` keeps. The `mvgam_residcor_list`
of `residual_cor(by_group = TRUE)` plots one facet per group, on shared
colour limits.

## References

Gruvaeus G and Wainer H (1972). Two additions to hierarchical cluster
analysis. British Journal of Mathematical and Statistical Psychology,
25, 200-206.  
  
Francis KC Hui (2016). BORAL - Bayesian ordination and regression
analysis of multivariate abundance data in R. Methods in Ecology and
Evolution. 7, 744-750.  
  
Otso Ovaskainen et al. (2016). Using latent variable models to identify
large networks of species-to-species associations at different spatial
scales. Methods in Ecology and Evolution, 7, 549-555.

## See also

[`jsdgam()`](https://nicholasjclark.github.io/mvgam/reference/jsdgam.md),
[`residual_cor()`](https://nicholasjclark.github.io/mvgam/reference/residual_cor.md)

## Author

Nicholas J Clark
