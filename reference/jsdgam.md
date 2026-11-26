# Fit a joint species distribution model

Fits a joint species distribution model (JSDM) to the presences,
detections or counts of many species across many sampling units. Species
often co-occur more or less often than their responses to the measured
covariates predict. A few latent factors capture that residual
association. They need far fewer parameters than a full correlation
matrix among species. The observation model takes the smooths, Gaussian
processes and random effects of any mvgam model. The factors take their
own predictors through `factor_formula`. Write
[`lv_axis()`](https://nicholasjclark.github.io/mvgam/reference/lv_axis.md)
in one of its terms to give each factor its own effect of a covariate, a
constrained ordination.

## Usage

``` r
jsdgam(
  formula,
  factor_formula = ~-1,
  knots,
  data,
  newdata,
  family = binomial(),
  unit = time,
  species = series,
  priors,
  n_lv = 2L,
  traits = NULL,
  trait_slopes = NULL,
  phylo = NULL,
  loadings_prior = NULL,
  backend = default_backend(),
  threads = NULL,
  run_model = TRUE,
  ...
)
```

## Arguments

- formula:

  The observation model formula, in brms syntax: smooths `s()` and
  `t2()`, `gp()`, varying intercepts and slopes, `mm()`, `mo()`, `me()`,
  `mi()`, distributional and non-linear sub-formulas and
  [`offset()`](https://rdrr.io/r/stats/offset.html). A response leaves
  rows out by holding `NA` there.
  [`mvgam_formula()`](https://nicholasjclark.github.io/mvgam/reference/mvgam_formula.md)
  lists the terms each formula refuses. `gp()` takes the approximate
  Hilbert-space form (`gp(x, k = 20)`) or the exact form (`gp(x)`, no
  `k`), under any brms kernel. An exact term predicts at new covariate
  values by conditioning on its fitted values, as brms does. Its cost
  grows with the cube of the number of unique values.

- factor_formula:

  A one-sided formula for the latent factors' linear predictor. Write
  [`lv_axis()`](https://nicholasjclark.github.io/mvgam/reference/lv_axis.md)
  in a term to give each factor its own effect, as in
  `~ s(elev, by = lv_axis())` or `~ elev:lv_axis()`. Each covariate must
  take one value per sampling unit. The deprecated `by = trend` acts as
  `by = lv_axis()`. Defaults to `~ -1`, factors without predictors.

- knots:

  Optional named list of knot positions for smooth terms, by covariate,
  as `knots` of
  [`mvgam()`](https://nicholasjclark.github.io/mvgam/reference/mvgam.md).

- data:

  A `data.frame` or `list` holding the response, the covariates of both
  formulas, the `unit` column and the `species` factor.

  Data held as arrays, as unmarked, ubms, flocker, Hmsc and spOccupancy
  take them, convert to this long format.
  [`pivot_detection_array()`](https://nicholasjclark.github.io/mvgam/reference/pivot_detection_array.md)
  converts detection histories of occupancy and N-mixture data:
  `[J, K]`, `[N, J, K]`, multi-season `[N, J, T, K]` or a list of
  `[J, K]` matrices per species.
  [`pivot_species_matrix()`](https://nicholasjclark.github.io/mvgam/reference/pivot_species_matrix.md)
  converts a wide `[J, N]` species composition matrix.

- newdata:

  Optional held-out `data.frame`, stored on the fit as
  `object$test_data`. Fitting draws no forecasts.
  [`forecast()`](https://generics.r-lib.org/reference/forecast.html)
  called without `newdata` forecasts these times, carrying the fitted
  latent state forward. `plot(fit, type = "series")` overlays them.
  `posterior_predict()` on these data uses the covariate part alone. The
  section 'Which prediction function?' of
  [`predict.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/predict.mvgam.md)
  compares the two.

- family:

  The observation family every species shares, as a family object.
  [mvgam_families](https://nicholasjclark.github.io/mvgam/reference/mvgam_families.md)
  lists them. Defaults to
  [`binomial()`](https://rdrr.io/r/stats/family.html) for
  presence-absence data. Counts take a count family such as
  [`poisson()`](https://rdrr.io/r/stats/family.html) or
  [`negbinomial()`](https://paulbuerkner.com/brms/reference/brmsfamily.html).

  The factors and a family's dispersion parameter, such as `shape`,
  `phi` or `sigma`, explain the same residual variation. Where the
  factors explain it, the dispersion posterior runs toward its limit and
  the sampler can diverge there. An informative dispersion prior or a
  family without dispersion, such as
  [`poisson()`](https://rdrr.io/r/stats/family.html), settles it.

  For one family per species, write `formula` with one
  [`brms::bf()`](https://paulbuerkner.com/brms/reference/brmsformula.html)
  per species, each naming its family, as in
  `bf(count ~ env, family = poisson()) + bf(seen ~ env, family = bernoulli())`.
  The responses then form the species axis, in place of a `species`
  column. `family` applies to any response that names none.

  The simplex families
  [`diri()`](https://nicholasjclark.github.io/mvgam/reference/diri.md),
  [`multi()`](https://nicholasjclark.github.io/mvgam/reference/multi.md)
  and
  [`categ()`](https://nicholasjclark.github.io/mvgam/reference/categ.md)
  need a per-species term, as in `y ~ env * species` or
  `y ~ 0 + species + env:species`. A coefficient shared by every species
  cancels from their likelihood. In `y ~ env`, `b_Intercept` and `b_env`
  take their priors alone. mvgam warns of this once per session.
  [`diri()`](https://nicholasjclark.github.io/mvgam/reference/diri.md)
  sets out the identification.

- unit:

  The unquoted name of the numeric column indexing the sampling units,
  often `time` or `site`. Defaults to `time`.

- species:

  The unquoted name of the factor column indexing the species. Defaults
  to `series`, as in other mvgam models.

- priors:

  Optional `brmsprior` of priors to set, from
  [`brms::prior()`](https://paulbuerkner.com/brms/reference/set_prior.html).
  `prior`, the brms spelling, is accepted in `...`.
  [`get_prior()`](https://nicholasjclark.github.io/mvgam/reference/get_prior.md)
  of a model built with `run_model = FALSE` lists every class and
  coefficient with its default, the `trait_slopes` terms among them. See
  *Trait-mediated responses* for their scales. Each loading in `Z` takes
  `normal(0, s)` by default. `s` is 1, widened as 'brms' widens its
  default scale priors for responses that spread far on their link
  scale. A binary response says little about the size of a loading. The
  light tails keep chains from stalling at extreme values.
  `prior(student_t(3, 0, 1), class = "Z")` gives heavier tails.

- n_lv:

  Integer number of latent factors, from 1 to the number of species.
  Defaults to 2. The residual covariance `Z Z' + diag(Psi^2)` has rank
  at most the number of species. Further columns of `Z` add nothing.

  Under the default normal prior on `Z`, or a structured prior built
  from `traits`, `phylo` or `loadings_prior$distances`, `n_lv` is the
  number of factors in the likelihood. Under the multiplicative gamma
  process prior (`loadings_prior = "mgp"` or
  `list(column_shrinkage = "mgp", ...)`) it is a ceiling. The prior
  shrinks later columns towards zero. A larger `loadings_prior$mgp_a2`
  shrinks them harder.
  [`active_factors()`](https://nicholasjclark.github.io/mvgam/reference/active_factors.md)
  counts the factors the data support.

- traits:

  Optional species traits for a structured loadings prior: a
  `data.frame` or matrix with one row per species, or the name of an
  element of `data2`. Rows are matched to `levels(data[[species]])` by a
  `series` column, by row name or by order. Factor columns are one-hot
  encoded and numeric columns are standardised. They become
  `loadings_prior$features`. Supplying an explicit `loadings_prior` as
  well is refused.

- trait_slopes:

  Optional one-sided formula of traits, such as `~ trait1 + trait2`, for
  species responses that depend on their traits (the fourth-corner model
  of Hmsc). The traits must be columns of `data`, constant within
  species, as brms takes every variable of a formula. A prediction frame
  that omits a trait takes each species' value from the fit. The grids
  of
  [`conditional_effects()`](https://paulbuerkner.com/brms/reference/conditional_effects.brmsfit.html)
  and marginaleffects omit the traits, which draws each species at its
  own. Each term of `formula` takes a shared effect, an effect of each
  trait and a species deviation. See *Trait-mediated responses*.

- phylo:

  Optional phylogeny: an
  [`ape::phylo`](https://rdrr.io/pkg/ape/man/read.tree.html) tree, whose
  cophenetic distances are taken, or a numeric distance matrix with row
  and column names matching `levels(data[[species]])`. The distances are
  rescaled to a maximum of 1 and become
  `loadings_prior$distances$phylo`. A tree that is not ultrametric draws
  a warning once. A tree requires the ape package. Supplying an explicit
  `loadings_prior` as well is refused.

- loadings_prior:

  Optional structured prior on the loadings, as `loadings_prior` of
  [`mvgam()`](https://nicholasjclark.github.io/mvgam/reference/mvgam.md)
  takes it. Supplying `traits` or `phylo` as well is refused.

- backend:

  `"cmdstanr"` or `"rstan"`. Defaults to `getOption("brms.backend")`
  when it is set. Otherwise cmdstanr when it is installed and rstan when
  it is not.

- threads:

  Positive integer number of threads per chain, or `NULL` (the default)
  for none. The model is compiled with
  `cpp_options$stan_threads = TRUE`. cmdstanr then samples with
  `threads_per_chain = threads`. The closure-unit families
  ([`nmix()`](https://nicholasjclark.github.io/mvgam/reference/nmix.md),
  [`occ()`](https://nicholasjclark.github.io/mvgam/reference/occ.md))
  and the multi-response families
  ([`diri()`](https://nicholasjclark.github.io/mvgam/reference/diri.md),
  [`mvn()`](https://nicholasjclark.github.io/mvgam/reference/mvn.md),
  [`mvt()`](https://nicholasjclark.github.io/mvgam/reference/mvt.md),
  [`multi()`](https://nicholasjclark.github.io/mvgam/reference/multi.md),
  [`categ()`](https://nicholasjclark.github.io/mvgam/reference/categ.md))
  split their likelihood over units with `reduce_sum`, in about eight
  chunks. With 50 or more closure units, `threads = 4` samples about a
  third faster. Fewer units can sample slower, the threading overhead
  outweighing the gain. A brms family without a `trend_formula` uses the
  threading brms writes.

  A brms family with a `trend_formula` samples each chain on one thread
  whatever `threads` says. The threaded likelihood brms writes leaves no
  place to add the trend. Each such fit warns.

- run_model:

  Logical. `FALSE` builds the model without parsing, compiling or
  sampling it. The result holds `$stancode`, `$standata` and the prior
  table, with `$fit` set to `NULL`.
  [`stancode.mvgam_formula()`](https://nicholasjclark.github.io/mvgam/reference/stancode.mvgam_formula.md),
  [`standata.mvgam_formula()`](https://nicholasjclark.github.io/mvgam/reference/stancode.mvgam_formula.md)
  and
  [`get_prior.mvgam_formula()`](https://nicholasjclark.github.io/mvgam/reference/get_prior.mvgam_formula.md)
  return them. Methods that need draws, such as
  [`summary()`](https://rdrr.io/r/base/summary.html) and
  [`forecast()`](https://generics.r-lib.org/reference/forecast.html),
  refuse it. A `jsdgam()` model has no
  [`mvgam_formula()`](https://nicholasjclark.github.io/mvgam/reference/mvgam_formula.md),
  which makes `run_model = FALSE` the way to inspect one before fitting.
  Defaults to `TRUE`.

- ...:

  Further arguments of
  [`mvgam()`](https://nicholasjclark.github.io/mvgam/reference/mvgam.md),
  such as `data2` (where `traits` and `phylo` find a named element),
  `algorithm`, `chains` and `silent`.

## Value

A fitted model of class `c("mvgam", "jsdgam", "brmsfit")`. Every mvgam
method applies, such as
[`summary()`](https://rdrr.io/r/base/summary.html),
[`predict()`](https://rdrr.io/r/stats/predict.html),
[`forecast()`](https://generics.r-lib.org/reference/forecast.html) and
[`loo()`](https://mc-stan.org/loo/reference/loo.html).
[`ordinate()`](https://nicholasjclark.github.io/mvgam/reference/ordinate.md)
and
[`residual_cor()`](https://nicholasjclark.github.io/mvgam/reference/residual_cor.md)
describe the factors. `update(fit, n_lv = 3)` refits it through
`jsdgam()` with the arguments given changed.

## Trait-mediated responses

`trait_slopes = ~ size` adds terms to `formula`. Each term takes a
shared effect, an effect of each trait and a species deviation:

- Intercept:

  `size`, with the species deviation `1` in `(1 + ... | sp | series)`

- A term with design columns:

  a numeric or factor covariate, an interaction,
  [`I()`](https://rdrr.io/r/base/AsIs.html),
  [`poly()`](https://rdrr.io/r/stats/poly.html), `mo()`, `me()` or
  `mi()`: the term `env:size` and the deviation `env` in
  `(1 + env | sp | series)`. The `|sp|` tag correlates the deviations
  under one LKJ prior.

- A smooth, `s()` or `t2()`:

  `s(x, by = size)` scales the smooth by the trait.
  `s(x, by = series, m = 1)` gives each species a smooth penalised
  towards a flat line.

- A Gaussian process, `gp()`:

  `gp(x, by = size)` and `gp(x, by = series)`, one approximate process
  per species

- A group-level term:

  entered as written, as is a smooth with `bs = "re"` or `bs = "fs"`

A smooth trait, `~ s(size)`, gives the intercept `s(size)`. Each design
column of a covariate scales the trait's smooth: `env` takes
`s(size, by = env)` and a factor `hab` takes `s(size, by = habb)`. A
smooth of `x` takes the surface `t2(x, size)`. A smooth with its own
`by`, such as `s(x, by = hab)`, crosses the trait level by level. Its
species deviations keep the `by`. `mo()`, `me()` and `mi()` terms cross
linear traits alone.

A smooth or `gp()` takes one column of `data` as its `by`. A factor
level, a [`poly()`](https://rdrr.io/r/stats/poly.html) column or a `by`
scaled per species enters through a derived column. Its name follows
[`fixef.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/mvgam_diagnostics.md),
such as `habb`, `polyx21` or `env_size`. Each prediction frame
recomputes these columns from the variables they come from. A frame that
changes `env` then changes `env_size`.

A crossing of a centred smooth passes through zero at the mean of its
covariates (mgcv's `pc`). The shared term then carries the effect at the
means. mgcv leaves a smooth with a numeric `by` uncentred. Its crossing
keeps the constant, the trait's effect on that column's slope. A `t2()`
surface takes first-order margins. Their penalties separate it from the
main smooths. A margin of one covariate is a thin plate spline, or keeps
a cyclic basis. An `s()` of several covariates gives one isotropic
Duchon spline margin.

The coefficients take their brms names, such as `b_env:size`. Each
population-level coefficient takes `normal(0, r / s)` by default. `s` is
the SD of its design column. `r` is 1, or the SD of the response under
an identity link, which keeps the coefficients in its units. Every
covariate then has one prior scale on the link, whatever its units or
the basis [`poly()`](https://rdrr.io/r/stats/poly.html) gives it, as
rstanarm's autoscaled priors do. `student_t(3, 0, 2.5 r)` is the default
on the SDs of the correlated species deviations. The smooth and `gp()`
deviations take the brms defaults. `priors` overrides any of them. A
prior on class `b` without a `coef` replaces every coefficient's
default. A term of `formula` naming the species is refused: each species
already takes its own response. For some of these terms alone, write
them in `formula` directly, as in
`y ~ env + size + env:size + (1 + env | series)`.

## References

Warton, D. I., Blanchet, F. G., O'Hara, R. B., Ovaskainen, O., Taskinen,
S., Walker, S. C. and Hui, F. K. C. (2015). So many variables: joint
modeling in community ecology. *Trends in Ecology and Evolution*,
30(12):766-779.
[doi:10.1016/j.tree.2015.09.007](https://doi.org/10.1016/j.tree.2015.09.007)

Ovaskainen, O., Tikhonov, G., Norberg, A., Blanchet, F. G., Duan, L.,
Dunson, D., Roslin, T. and Abrego, N. (2017). How to make more out of
community data? *Ecology Letters*, 20(5):561-576.
[doi:10.1111/ele.12757](https://doi.org/10.1111/ele.12757)

Tikhonov, G., Opedal, O. H., Abrego, N., Lehikoinen, A., de Jonge, M. M.
J., Oksanen, J. and Ovaskainen, O. (2020). Joint species distribution
modelling with the R-package Hmsc. *Methods in Ecology and Evolution*,
11(3):442-447.
[doi:10.1111/2041-210X.13345](https://doi.org/10.1111/2041-210X.13345)

Hui, F. K. C. (2016). boral - Bayesian Ordination and Regression
Analysis of Multivariate Abundance Data in R. *Methods in Ecology and
Evolution*, 7(6):744-750.
[doi:10.1111/2041-210X.12514](https://doi.org/10.1111/2041-210X.12514)

Heaps, S. E. and Jermyn, I. H. (2024). Structured prior distributions
for the covariance matrix in latent factor models. *Statistics and
Computing*, 34:143.
[doi:10.1007/s11222-024-10454-0](https://doi.org/10.1007/s11222-024-10454-0)

## See also

[`sim_jsdgam()`](https://nicholasjclark.github.io/mvgam/reference/sim_jsdgam.md)
simulates data this function fits.
[`sim_closure_unit_data()`](https://nicholasjclark.github.io/mvgam/reference/sim_closure_unit_data.md)
simulates the
[`occ()`](https://nicholasjclark.github.io/mvgam/reference/occ.md) and
[`nmix()`](https://nicholasjclark.github.io/mvgam/reference/nmix.md)
case.
[`residual_cor()`](https://nicholasjclark.github.io/mvgam/reference/residual_cor.md),
[`ordinate()`](https://nicholasjclark.github.io/mvgam/reference/ordinate.md),
[`shared_variation()`](https://nicholasjclark.github.io/mvgam/reference/shared_variation.md),
[`active_factors()`](https://nicholasjclark.github.io/mvgam/reference/active_factors.md)
and
[`compare_loadings()`](https://nicholasjclark.github.io/mvgam/reference/compare_loadings.md)
interpret the fitted factors.
[`species_effects()`](https://nicholasjclark.github.io/mvgam/reference/species_effects.md)
gives each species' response to the covariates.
[`community()`](https://nicholasjclark.github.io/mvgam/reference/community.md)
gives richness, diversity and co-occurrence.
[`lv_axis()`](https://nicholasjclark.github.io/mvgam/reference/lv_axis.md)
gives each factor its own covariate effect.
[`how_to_cite()`](https://nicholasjclark.github.io/mvgam/reference/how_to_cite.mvgam.md)
drafts a methods paragraph for the fit and
[`methods_md()`](https://nicholasjclark.github.io/mvgam/reference/methods_md.md)
writes its equations. The [JSDM
article](https://nicholasjclark.github.io/mvgam/articles/jsdgam.html)
fits a negative binomial model with a trait-informed loadings prior,
ordination biplots and multiplicative gamma process shrinkage. The [JSDM
cheatsheet](https://nicholasjclark.github.io/mvgam/cheatsheets/jsdgam_cheatsheet.pdf)
summarises the functions on one page.

Other model fitting:
[`check_newdata()`](https://nicholasjclark.github.io/mvgam/reference/check_newdata.md),
[`mvgam()`](https://nicholasjclark.github.io/mvgam/reference/mvgam.md),
[`mvgam_data()`](https://nicholasjclark.github.io/mvgam/reference/mvgam_data.md),
[`mvgam_multiple()`](https://nicholasjclark.github.io/mvgam/reference/mvgam_multiple.md),
[`pivot_detection_array()`](https://nicholasjclark.github.io/mvgam/reference/pivot_detection_array.md),
[`pivot_species_matrix()`](https://nicholasjclark.github.io/mvgam/reference/pivot_species_matrix.md),
[`print.mvgam_formula()`](https://nicholasjclark.github.io/mvgam/reference/mvgam_formula.md),
[`series_to_mvgam()`](https://nicholasjclark.github.io/mvgam/reference/series_to_mvgam.md),
[`stancode.mvgam_formula()`](https://nicholasjclark.github.io/mvgam/reference/stancode.mvgam_formula.md),
[`update.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/update.mvgam.md)

## Examples

``` r
# \dontrun{
# Detections of 6 species at 80 sites, 4 visits each. Occupancy
# responds to the site covariate `env` and detection to the time of
# day `tod_c`. Two latent factors drive the species associations.
simdat <- sim_closure_unit_data(
  family    = occ(),
  n_species = 6L,
  n_sites   = 80L,
  n_visits  = 4L,
  n_lv      = 2L,
  seed      = 1L
)

# A joint occupancy model. Each species has its own occupancy
# intercept and `env` slope, plus its own detection intercept. Two
# latent factors take up the residual association.
mod <- jsdgam(
  formula        = bf(y ~ series * env, p ~ series + tod_c),
  factor_formula = ~ -1,
  data           = simdat$data_train,
  family         = occ(),
  n_lv           = 2L,
  chains         = 2,
  silent         = 2
)
#> Warning: 2 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

summary(mod)
#>  Family: occ 
#>   Links: mu = logit; p = logit 
#> Formula: y ~ series * env 
#>          p ~ series + tod_c 
#>    Data: simdat$data_train (Number of observations: 1920) 
#> Species: 6 
#>  Trends: 2 latent factors 
#>   Draws: 2 chains, each with iter = 2000; warmup = 1000; thin = 1; 
#>          total post-warmup draws = 2000
#> 
#> == Observation Model ==
#> Regression Coefficients:
#>                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept          1.11      0.60     0.17     2.48 1.00      570      611
#> p_Intercept       -0.22      0.18    -0.57     0.12 1.00     1013     1258
#> seriessp_2        -0.47      0.69    -1.99     0.75 1.00      618      702
#> seriessp_3        -1.33      0.66    -2.81    -0.21 1.00      603      677
#> seriessp_4        -1.38      1.57    -3.48     3.33 1.00      296      159
#> seriessp_5        -0.21      0.70    -1.80     1.06 1.00      646      793
#> seriessp_6        -3.33      0.83    -5.21    -1.93 1.00      763      993
#> env               -2.36      0.85    -4.36    -1.07 1.00      474      584
#> seriessp_2:env     1.65      0.94     0.01     3.80 1.00      523      573
#> seriessp_3:env     2.29      0.91     0.82     4.32 1.00      490      568
#> seriessp_4:env    -0.08      1.61    -4.13     2.52 1.00      347      232
#> seriessp_5:env     1.97      0.94     0.43     4.04 1.00      514      634
#> seriessp_6:env     0.33      1.09    -1.68     2.62 1.00      541      727
#> p_seriessp_2       0.56      0.24     0.09     1.02 1.00     1309     1309
#> p_seriessp_3       1.12      0.26     0.59     1.64 1.00     1419     1370
#> p_seriessp_4      -1.08      0.42    -1.98    -0.31 1.00      447      357
#> p_seriessp_5       0.52      0.23     0.06     0.97 1.00     1213     1549
#> p_seriessp_6       0.62      0.33    -0.04     1.25 1.00     1561     1570
#> p_tod_c            0.01      0.02    -0.03     0.05 1.00     3035     1251
#> 
#> == Trend Model ==
#> Factor Loadings:
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Z_tilde[sp_1,1]     1.13      0.65     0.18     2.57 1.01      569     1168
#> Z_tilde[sp_2,1]     0.27      0.89    -1.51     2.03 1.00     1021     1270
#> Z_tilde[sp_3,1]    -0.04      0.72    -1.45     1.51 1.00     1387     1204
#> Z_tilde[sp_4,1]     0.37      0.95    -1.61     2.13 1.01      880     1133
#> Z_tilde[sp_5,1]     0.51      0.98    -1.47     2.40 1.00     1083     1378
#> Z_tilde[sp_6,1]     0.43      0.89    -1.32     2.22 1.00      722     1188
#> Z_tilde[sp_2,2]     1.07      0.67     0.07     2.63 1.00      969     1594
#> Z_tilde[sp_3,2]     0.50      0.70    -1.10     1.82 1.00     1341     1242
#> Z_tilde[sp_4,2]     0.28      0.97    -1.70     2.16 1.00     1255     1482
#> Z_tilde[sp_5,2]    -0.95      0.89    -2.52     1.14 1.00     1210     1444
#> Z_tilde[sp_6,2]    -0.26      0.86    -1.98     1.54 1.00     1504     1499
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
#> 
#> Next steps:
#>   - `pp_check(fit, type = "fit_stat")`: goodness of fit of unit totals
#>   - `plot(hindcast(fit, type = "latent_state"))`: posterior occupancy of each unit
#>   - `shared_variation(fit)`: covariance the factors imply, unchanged by rotation
#>   - `residual_cor(fit)`: implied correlations among species
#>   - `ordinate(fit)`: biplot of the factor scores and loadings
#>   - `loo(fit)` / `loo_compare(...)`: model fit + comparison
#> Use `how_to_cite(fit)` for a methods paragraph and `methods_md(fit)` for its equations.

# The effect of `env` on the chance of a detection, occupancy times
# detection
conditional_effects(mod)





# Each species' response to env, outlined where its sign is clear
plot(species_effects(mod, variables = "env"))


# The residual correlations among species the factors imply
plot(residual_cor(mod))


# Site scores and species loadings on the two factors
ordinate(mod, rotation = "varimax")


# Expected richness along `env`. The second plot marks the pairs of
# species found together more often than `env` predicts.
gradient <- datagrid(model = mod, env = seq(-2, 2, length.out = 20))
cm <- community(mod, newdata = gradient)
plot(cm)

plot(cm, type = "cooccurrence")


# The draws, for any contrast: richness at the end of the gradient
# less that at its start
rv <- as_draws_rvars(cm)
rv$richness[20] - rv$richness[1]
#> rvar<2000>[1] mean ± sd:
#> [1] -3.3 ± 0.43 

# Next, compare n_lv = 1 with update(mod, n_lv = 1L) and loo()
# }
```
