# `mvgam_residcor` object description

A `mvgam_residcor` object returned by function
[`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md).
Run `methods(class = "mvgam_residcor")` to see the available methods.

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
  `prob_nonzero` is at most `prob_threshold`.
  [`plot.mvgam_residcor()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_residcor.md)
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
  [`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md)
  was asked.

- probs, prob_threshold:

  The credible interval levels and the `prob_nonzero` threshold of
  `sig_cor`.

Passing `partial = TRUE` to
[`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md)
adds the partial correlations, with one field for each correlation
field: `prec`, `prec_se`, `prec_lower`, `prec_upper`, `prec_ess`,
`prec_prob_positive`, `prec_prob_negative`, `prec_prob_nonzero` and the
thresholded `sig_prec`. Partial correlations are bounded like
correlations. They are summarised on the same Fisher-z scale.
[`plot.mvgam_residcor()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_residcor.md)
with `type = "precision"` requires them.

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

## References

Francis KC Hui (2016). BORAL - Bayesian ordination and regression
analysis of multivariate abundance data in R. Methods in Ecology and
Evolution. 7, 744-750.  
  
Otso Ovaskainen et al. (2016). Using latent variable models to identify
large networks of species-to-species associations at different spatial
scales. Methods in Ecology and Evolution, 7, 549-555.

## See also

[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md),
[`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md)

## Author

Nicholas J Clark
