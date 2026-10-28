# The priors an mvgam model takes, before fitting

Lists the priors of the observation and trend parts of the model an
[`mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_formula.md)
describes, with their defaults for the data given. Without a
`trend_formula` the table matches that of
[`brms::get_prior()`](https://paulbuerkner.com/brms/reference/default_prior.html).
[`default_prior()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.md)
is the same method under the newer brms name.

## Usage

``` r
# S3 method for class 'mvgam_formula'
get_prior(
  object,
  data,
  family = gaussian(),
  loadings_prior = NULL,
  data2 = NULL,
  trend_map = NULL,
  ...
)

# S3 method for class 'mvgam_formula'
default_prior(object, ...)
```

## Arguments

- object:

  An
  [`mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_formula.md),
  or an `mvgam` model fitted or built with `run_model = FALSE`.

- data:

  A data frame holding every variable the formulas use.

- family:

  A description of the response distribution and link function, given as
  a family function, a call to one or its name. Supports most brms
  families, including the ordinal families
  ([`brms::cumulative()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  [`brms::sratio()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  [`brms::cratio()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  [`brms::acat()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)).
  The brms families needing a separate linear predictor per response
  category,
  [`brms::categorical()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  [`brms::multinomial()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
  and
  [`brms::dirichlet()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  are refused: use
  [`categ()`](https://nicholasjclark.github.io/mvgam/dev/reference/categ.md),
  [`multi()`](https://nicholasjclark.github.io/mvgam/dev/reference/multi.md)
  and
  [`diri()`](https://nicholasjclark.github.io/mvgam/dev/reference/diri.md),
  which take the categories in long format and share one latent
  structure across them. A family whose mean has to stay positive needs
  a link that keeps it there once a trend enters the linear predictor:
  pair [`stats::Gamma()`](https://rdrr.io/r/stats/family.html),
  [`brms::weibull()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  [`brms::exponential()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  [`brms::frechet()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
  and [`stats::inverse.gaussian()`](https://rdrr.io/r/stats/family.html)
  with `link = "log"`. Their default links leave the mean positive only
  where the predictor is. A latent trend is centred near zero and
  reaches negative values, where the likelihood is undefined.
  [`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
  warns before fitting.
  [`brms::lognormal()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
  is the exception: its `mu` is a log-scale location that takes any
  sign.

  A family written inside a univariate
  [`brms::bf()`](https://paulbuerkner.com/brms/reference/brmsformula.html)
  is the model's family, as in brms. In a multivariate model each
  response may take its own family, in which case name it inside that
  response's
  [`brms::bf()`](https://paulbuerkner.com/brms/reference/brmsformula.html):
  `bf(count ~ x, family = poisson()) + bf(seen ~ x, family = bernoulli())`.
  Passing a list of families to this argument, as
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html)
  allows, is not supported; the family belongs to the response that
  names it. A single family given here applies to every response that
  does not name one of its own. Closure-unit and multi-response families
  ([`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md),
  [`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md),
  [`mvn()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvn.md),
  [`mvt()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvt.md),
  [`diri()`](https://nicholasjclark.github.io/mvgam/dev/reference/diri.md),
  [`multi()`](https://nicholasjclark.github.io/mvgam/dev/reference/multi.md),
  [`categ()`](https://nicholasjclark.github.io/mvgam/dev/reference/categ.md))
  lay their data out by unit. Each models its response alone and cannot
  be one response of a multivariate model.

- loadings_prior:

  Optional named list specifying a structured prior on the unconstrained
  factor loadings matrix following Heaps & Jermyn (2024). The loadings
  belong to a trend with latent factors. The argument requires a
  `trend_formula`. Supplying it without one is an error. When supplied,
  the default iid `to_vector(Z) ~ student_t(3, 0, 0.5)` prior is
  replaced with a per-column matrix-normal prior of the form
  `Z[, i] ~ multi_normal_cholesky(0, L_Phi)`, where the among-row scale
  matrix `Phi` is assembled multiplicatively from any combination of:

  - a per-series feature matrix (`features`), expanded into an ARD
    exponential kernel via `gp_exponential_cov()`

  - one or more pairwise distance matrices (`distances`), each
    contributing an `exp(-d / theta)` factor

  Accepted fields:

  - `features`: string lookup into `data2`, a numeric matrix, or a
    data.frame. Columns are encoded by type (numeric/integer/logical:
    z-score; ordered factor: z-score after
    [`as.numeric()`](https://rdrr.io/r/base/numeric.html); unordered
    factor / character: one-hot with all levels retained, following
    Heaps Sect. 6.2). Each encoded column receives its own ARD
    length-scale.

  - `distances`: string name (or character vector of names) of pairwise
    distance matrices in `data2`, or a single matrix / named list of
    matrices passed inline. Each matrix is rescaled to maximum entry one
    so the default length-scale prior is interpretable on a unit scale
    (matching Heaps Supps S4.2.1).

  - `column_shrinkage`: `"iid"` (default; no shrinkage) or `"mgp"`
    (multiplicative-gamma-process increasing shrinkage of Bhattacharya &
    Dunson 2011, used in Heaps Eq. (9)). `"mgp"` applies its column
    scale through the shared innovation scale. It is available for
    [`AR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md),
    [`RW()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
    and
    [`ZMVN()`](https://nicholasjclark.github.io/mvgam/dev/reference/ZMVN.md)
    trends.
    [`VAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
    builds its own innovation covariance and has no shared scale for it;
    pair a
    [`VAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
    factor model with `"iid"` instead.

  - `mgp_a1`, `mgp_a2`: numeric MGP hyperparameters, only used when
    `column_shrinkage = "mgp"`. Default `(2, 4)` lies within the
    `a2 in [3, 5]` range recommended for moderate-n ecology / community
    panels by Legramanti, Durante and Dunson (2020, JRSS-B) and
    Schiavon, Canale and Dunson (2022, Biometrics 78:995). Heaps Sect.
    6.3.1 used `(2, 3)` for the gas-demand application; Sect. 6.2.2 used
    `(2, 6)` for the Finnish-bird JSDM. Lower `a2` for less shrinkage
    when the data weakly identify the active rank.

  Length-scales receive a `lognormal(0, 1)` prior in the model block.
  This prior is set on the standardised distance scale. Pairwise
  distance matrices are rescaled to `max(d) = 1` inside the normaliser.
  A length-scale of 1 then corresponds to correlation `exp(-1)` between
  the two most distant series. Heaps' Finnish-birds application (Sect.
  6.2.2) used the wider `lognormal(0, sqrt(10))` instead; override the
  default by passing a
  [`prior()`](https://paulbuerkner.com/brms/reference/set_prior.html)
  for `theta_features` / `theta_dist_<name>` if your data weakly
  identify length-scales and you want less regularisation. Note: the
  `max(d) = 1` rule is the natural generic standardisation. Heaps Supps
  S4.2.1 used "root-to-tip distance = 1" for ultrametric phylogenies,
  which corresponds to `max(d) = 2` (twice the per-leaf depth);
  pre-standardise your distance matrix to match if exact parity matters.
  Identification. The structured prior applies to the sampled columns of
  `Z`. Where the QR rotation gives the reported loadings `Z_tilde`, each
  of its columns mixes the sampled columns.
  [`shared_variation()`](https://nicholasjclark.github.io/mvgam/dev/reference/shared_variation.md)
  and
  [`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md)
  use `Z Z'`, which is the same before and after the rotation. See the
  Identification section of the trend constructors.

  Compatibility with `trend_map`:

  - `trend_map = NULL` (default): compatible. `n_lv` alone triggers the
    free-Z factor model; `loadings_prior` applies to the sampled `Z`
    column-wise.

  - `trend_map = matrix(NA_real_, n_series, n_lv)` (all-NA mask, used
    internally by
    [`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)):
    compatible. Treated as a free-Z trigger upstream; `loadings_prior`
    applies as if `trend_map` were `NULL`.

  - `trend_map` with a mix of fixed entries and `NA`s: incompatible.
    Partial-Z parameterises the free entries as a vector under an iid
    `student_t(3, 0, 1)` prior, which would overlap with the
    matrix-normal structure and double-count regularisation.

  - Fully-fixed `trend_map` (no `NA`s): incompatible. The structured
    prior has no free parameters to act on.

- data2:

  A named list of objects the model uses that are not variables in
  `data`, as in
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html).

- trend_map:

  Optional fixed factor loadings, in one of three forms:

  - a numeric `n_series x n_lv` matrix of loadings. `NA` entries are
    sampled. Finite entries stay fixed on `Z` in the posterior. An
    all-`NA` matrix fixes nothing and acts as `trend_map = NULL`

  - a `data.frame(series, trend)` assigning each series to one latent
    process

  - `"identity"`, one process per series, or `"shared"`, one process for
    every series

  A factor model without a `trend_map` samples every loading. It
  identifies `Z` after sampling by a thin QR rotation (Heaps and Jermyn
  2024). A `trend_map` with any fixed entry takes no rotation. The
  argument requires a `trend_formula`. It is an alternative to the
  `trend_map` argument of the trend constructor, and supplying both is
  an error. `loadings_prior` lists the `trend_map` forms a structured
  loadings prior accepts.

- ...:

  Further arguments of
  [`brms::get_prior()`](https://paulbuerkner.com/brms/reference/default_prior.html),
  such as `knots` or `drop_unused_levels`.

## Value

A `brmsprior` data frame of the observation and trend priors, usable
with
[`brms::set_prior()`](https://paulbuerkner.com/brms/reference/set_prior.html)
and
[`brms::prior()`](https://paulbuerkner.com/brms/reference/set_prior.html).
A trend class carries the `_trend` suffix.

## Details

The observation priors are those
[`brms::get_prior()`](https://paulbuerkner.com/brms/reference/default_prior.html)
gives, with mvgam's defaults for the families it adds. The trend rows
list every parameter the trend constructor samples and the coefficients
of the trend formula.

**Default trend priors.** brms sets the scale of its default intercept,
standard deviation and residual priors to `max(2.5, mad(link(y)))`, from
the spread of the response on the link scale under the identity, log,
inverse, square-root and inverse-square links. That widens its base
scale of 2.5 by the factor \\s = \max(1, \mathrm{mad}(\mathrm{link}(y))
/ 2.5)\\. mvgam widens the trend's scale priors by the same factor,
taking the widest over several responses and \\s = 1\\ under any other
link:

- `sigma_trend` and `sigma_group_trend` take `exponential(2 / s)`, with
  mean \\s / 2\\.

- The factor loadings `Z` take `student_t(3, 0, 0.5 s)`.

- The shock scales and starting states of
  [`LLT()`](https://nicholasjclark.github.io/mvgam/dev/reference/LLT.md)
  scale with the number of time points. Its `sigma_trend` takes one of
  them. The growth priors of a linear
  [`PW()`](https://nicholasjclark.github.io/mvgam/dev/reference/piecewise_trends.md)
  also scale with it. Their help pages give the forms.

A count model on the log link rarely spreads past 2.5. Its defaults keep
their link-scale values. Coefficients without units keep fixed priors:
autoregressive and damping coefficients, correlations and degrees of
freedom. The table this returns holds the resolved values for the data
given.

A shrinkage prior on a trend-side class is written without autoscaling.
`horseshoe()` and `R2D2()` scale their global parameter by the family's
residual standard deviation. The trend's own scale is `sigma_trend`, one
per latent series. A family's residual scale has no meaning on the trend
side. Those rows take `autoscale = FALSE`. Writing `autoscale = TRUE` on
a trend-side class is refused.

## See also

[`mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_formula.md),
[`brms::prior()`](https://paulbuerkner.com/brms/reference/set_prior.html)
and
[`brms::set_prior()`](https://paulbuerkner.com/brms/reference/set_prior.html).
The [hierarchical VAR
article](https://nicholasjclark.github.io/mvgam/articles/hierarchical_var.html)
sets priors on the observation error and the trend correlations.

Other priors:
[`get_prior.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.mvgam.md)

## Examples

``` r
# \dontrun{
set.seed(1)
dat <- sim_mvgam(family = poisson(), n_series = 2L,
                 n_timepoints = 120L, trend_model = AR())$data_train

# Every prior the model takes, before any fitting. Trend classes
# carry a `_trend` suffix: `b` belongs to the observation formula
# and `b_trend` would belong to the trend formula.
mf <- mvgam_formula(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1))
get_prior(mf, data = dat, family = poisson())
#>                   prior       class        coef group resp dpar nlpar lb ub tag
#>  student_t(3, 1.4, 2.5)   Intercept                                            
#>                  (flat)           b                                            
#>                  (flat)           b        sx_1                                
#>    student_t(3, 0, 2.5)         sds                                    0       
#>    student_t(3, 0, 2.5)         sds s(x, k = 5)                        0       
#>          exponential(2) sigma_trend                                    0       
#>          normal(0, 0.5)   ar1_trend                                   -1  1    
#>        source
#>       default
#>       default
#>  (vectorized)
#>       default
#>  (vectorized)
#>       default
#>       default

# Replace three defaults. Each `prior()` call gives one row.
my_priors <- c(
  prior(std_normal(), class = b),
  prior(inv_gamma(5, 1), class = sigma_trend),
  prior(normal(0.5, 0.25), class = ar1_trend)
)

# `run_model = FALSE` builds the model without sampling. Its prior
# table marks the replaced rows as "user".
pre <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
             data = dat, family = poisson(), prior = my_priors,
             run_model = FALSE)
prior_summary(pre)
#>                   prior       class        coef group resp dpar nlpar lb ub tag
#>  student_t(3, 1.4, 2.5)   Intercept                                            
#>            std_normal()           b                                            
#>            std_normal()           b        sx_1                                
#>    student_t(3, 0, 2.5)         sds                                    0       
#>    student_t(3, 0, 2.5)         sds s(x, k = 5)                        0       
#>         inv_gamma(5, 1) sigma_trend                                    0       
#>       normal(0.5, 0.25)   ar1_trend                                   -1  1    
#>        source
#>       default
#>          user
#>          user
#>       default
#>  (vectorized)
#>          user
#>          user

# Next, drop `run_model = FALSE` to fit the model under these priors
# }
```
