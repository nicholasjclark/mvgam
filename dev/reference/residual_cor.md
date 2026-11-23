# Residual correlations between the series of a fitted model

Posterior correlations, covariances and optional partial correlations
between the series' latent trends. Residual here means the co-variation
of the trends left once the covariate effects are accounted for.
[`residuals.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/residuals.mvgam.md)
returns the observation residuals. Three kinds of trend estimate these
correlations:

- A factor trend (`n_lv` on the trend constructor). Each draw gives the
  series covariance `Z Omega Z'`, with `Z` the loadings and `Omega` the
  covariance of the latent states.
  [`mvn()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvn.md)
  and
  [`mvt()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvt.md)
  add each series' residual variance to the diagonal: `Psi^2`, or
  `Psi^2 * nu / (nu - 2)` under
  [`mvt()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvt.md).

- A trend with correlated innovations:
  [`RW()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md),
  [`AR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md),
  [`CAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  or
  [`ZMVN()`](https://nicholasjclark.github.io/mvgam/dev/reference/ZMVN.md)
  with `cor = TRUE`, or
  [`VAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md).

- A hierarchical trend (`gr` on the trend constructor).

## Usage

``` r
residual_cor(object, ...)

# S3 method for class 'mvgam'
residual_cor(
  object,
  covariance = c("stationary", "innovation"),
  by_group = FALSE,
  partial = FALSE,
  summary = TRUE,
  robust = FALSE,
  probs = c(0.025, 0.975),
  prob_threshold = 0.95,
  ndraws = NULL,
  draw_ids = NULL,
  ...
)
```

## Arguments

- object:

  A fitted `mvgam` object.

- ...:

  Unused. Anything passed here is refused.

- covariance:

  Which covariance to summarise. `"stationary"` (the default) gives the
  covariance of the latent states over the whole series. `"innovation"`
  gives the covariance of one time step's innovations. The two coincide
  for a trend whose states keep no memory between times.

- by_group:

  Logical, for a hierarchical trend. `FALSE` (the default) returns the
  population correlation of the innovations,
  `tcrossprod(L_Omega_global)`, under either `covariance`. `TRUE` also
  returns each group's correlation,
  `alpha * tcrossprod(L_global) + (1 - alpha) * tcrossprod(L_dev[g])`.

- partial:

  Logical. `TRUE` also computes partial correlations,
  `P_ij = -inv(R)_ij / sqrt(inv(R)_ii * inv(R)_jj)`. A partial
  correlation measures the association of two series once every other
  series is held fixed. The result stores them in the fields whose names
  start with `prec`. A factor fit with fewer factors than series has a
  singular covariance of rank `n_lv` and is refused.
  [`shared_variation()`](https://nicholasjclark.github.io/mvgam/dev/reference/shared_variation.md)
  reports the variation its series share. The residual variances of
  [`mvn()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvn.md)
  and
  [`mvt()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvt.md)
  make their covariance full rank.

- summary:

  Logical. `TRUE` (the default) summarises each entry: the point
  estimate, spread, credible interval and posterior sign probabilities.
  `FALSE` returns the arrays of draws.

- robust:

  Logical. If `FALSE` (the default), uses mean and standard deviation
  for summaries. If `TRUE`, uses median and median absolute deviation
  (MAD).

- probs:

  Two probabilities for the lower and upper bounds of the credible
  interval. Default `c(0.025, 0.975)`, a 95% interval.

- prob_threshold:

  The posterior probability of its sign a correlation needs to count as
  supported, at least 0.5 and below 1. Defaults to 0.95. It sets
  `sig_cor`, the `sig` column of
  [`summary.mvgam_residcor()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_residcor-class.md)
  and the pairs
  [`plot.mvgam_residcor()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_residcor-class.md)
  outlines.

- ndraws:

  Positive integer specifying number of posterior draws to use. NULL
  (default) uses all available draws.

- draw_ids:

  Optional integer vector selecting a subset of posterior draw indices
  to use. NULL (default) uses all draws (subject to `ndraws`).

## Value

An
[mvgam_residcor](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_residcor-class.md)
object. Under `by_group = TRUE` on a hierarchical trend, an
`mvgam_residcor_list` of them: the population matrix as `"_global"` and
one per group.
[`plot.mvgam_residcor_list()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_residcor-class.md)
draws them as faceted heatmaps.

## Details

A correlated-innovation trend gives each draw the covariance
`diag(sigma) L_Omega L_Omega' diag(sigma)`. A
[`VAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
trend takes the leading block of `Omega_trend`, the stationary
covariance the Stan model derives from `Phi_trend`, `Sigma_trend` and,
under `ma = TRUE`, `Theta_trend`.

The stationary covariance describes the states over the whole series. An
`AR(cor = TRUE)` trend reports the stationary covariance of its states,
including any moving-average term. With one lag it is
`Sigma[a, b] / (1 - ar_a * ar_b)`. A
[`CAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
trend holds it at every time of an irregular grid. A draw whose
autoregression is explosive has no stationary covariance and keeps its
innovation covariance. A warning gives the count once per session.
[`ZMVN()`](https://nicholasjclark.github.io/mvgam/dev/reference/ZMVN.md)
keeps no memory between times and
[`RW()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
and
[`LLT()`](https://nicholasjclark.github.io/mvgam/dev/reference/LLT.md)
have no stationary distribution. Each reports its innovation covariance.
The level, slope and seasonal shocks of
[`LLT()`](https://nicholasjclark.github.io/mvgam/dev/reference/LLT.md)
share its correlation. A hierarchical
[`AR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
converts each group's scales to stationary scales in the same way,
before its global and group correlations are mixed.

Under `covariance = "innovation"` a
[`VAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
fit reports the contemporaneous shocks. Its transition matrix spreads
each shock across series at every step. The two correlations can differ
substantially.

Intervals and spreads of correlations are computed on the Fisher-z
scale, `atanh(r)`, then back-transformed. The intervals stay inside
`(-1, 1)`. The reported matrix summarises each entry separately. It need
not be positive semi-definite, most visibly when a few factors drive
many series. A computation that factorises the matrix, such as
[`chol()`](https://rdrr.io/r/base/chol.html), takes `summary = FALSE`,
whose per-draw matrices are valid correlation matrices. `prob_positive`
and `prob_negative` are posterior sign probabilities. `prob_nonzero` is
the larger of the two. `sig_cor` keeps the correlations whose
`prob_nonzero` exceeds `prob_threshold`. Its other entries are zero.

[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
fits the same factor model. Its species covariance follows the factor
rule above.

## References

Hui, F. K. C. (2016). boral - Bayesian Ordination and Regression
Analysis of Multivariate Abundance Data in R. *Methods in Ecology and
Evolution*, 7(6), 744-750.
[doi:10.1111/2041-210X.12514](https://doi.org/10.1111/2041-210X.12514)

## See also

[`plot.mvgam_residcor()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_residcor-class.md)
draws the matrix as a heatmap.
[`community()`](https://nicholasjclark.github.io/mvgam/dev/reference/community.md)
gives the same associations as excess co-occurrence.
[`species_effects()`](https://nicholasjclark.github.io/mvgam/dev/reference/species_effects.md)
gives the responses to the covariates these correlations are net of.
[`summary.mvgam_residcor()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_residcor-class.md)
lists the pairs.
[`ordinate()`](https://nicholasjclark.github.io/mvgam/dev/reference/ordinate.md)
draws the loadings as a biplot. The example of
[trend_constructors](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
takes the correlations of a `CAR(cor = TRUE)` trend. The [joint species
distribution
article](https://nicholasjclark.github.io/mvgam/dev/articles/jsdgam.html)
interprets species correlations from a factor model.

Other latent dynamics:
[`active_factors()`](https://nicholasjclark.github.io/mvgam/dev/reference/active_factors.md),
[`compare_loadings()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_loadings.md),
[`fevd.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/fevd.mvgam.md),
[`irf.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md),
[`ordinate()`](https://nicholasjclark.github.io/mvgam/dev/reference/ordinate.md),
[`posterior_transition_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_transition_matrix.md),
[`shared_variation()`](https://nicholasjclark.github.io/mvgam/dev/reference/shared_variation.md),
[`stability.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/stability.mvgam.md)

## Examples

``` r
# \dontrun{
# Detections of 6 species whose occupancy shares two latent factors
simdat <- sim_closure_unit_data(family = occ(), n_species = 6L,
                                n_sites = 80L, n_visits = 4L,
                                n_lv = 2L, seed = 1L)
mod <- jsdgam(formula = bf(y ~ series * env, p ~ series + tod_c),
              factor_formula = ~ -1,
              data = simdat$data_train, family = occ(),
              n_lv = 2L, chains = 2, silent = 2)
#> Warning: 4 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# Species correlations after the fixed effects
rc <- residual_cor(mod)
round(rc$cor, 2)
#>       sp_1  sp_2  sp_3 sp_4  sp_5  sp_6
#> sp_1  1.00  0.16 -0.18 0.37  0.43  0.49
#> sp_2  0.16  1.00  0.77 0.49 -0.84 -0.34
#> sp_3 -0.18  0.77  1.00 0.56 -0.73 -0.30
#> sp_4  0.37  0.49  0.56 1.00  0.01  0.34
#> sp_5  0.43 -0.84 -0.73 0.01  1.00  0.74
#> sp_6  0.49 -0.34 -0.30 0.34  0.74  1.00
plot(rc)


# Next: summary(rc) lists the pairs by posterior sign probability
# }
```
