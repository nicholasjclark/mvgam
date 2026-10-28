# Fit a Bayesian multivariate state-space model

Fits a state-space model in Stan. Each observation follows the
observation `family`. On the link scale its mean is the sum of the
effects in `formula` and a latent trend \\z\_{t,s}\\ for series \\s\\ at
time \\t\\. The data never record the trend directly. `trend_formula`
names how it moves from one time to the next, through a trend
constructor such as
[`AR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md).
It also takes predictors of the trend. With an `AR(p = 1)` trend:
\$\$g(\mu\_{t,s}) = \eta\_{t,s} + \eta^{(z)}\_{t,s} + z\_{t,s}, \qquad
z\_{t,s} = \phi_s z\_{t-1,s} + \epsilon\_{t,s}\$\$ Here \\g\\ is the
link, \\\eta\\ the predictor from `formula`, \\\eta^{(z)}\\ the
predictor from `trend_formula` and \\\epsilon\\ the trend's innovation
at each time. The [overview
article](https://nicholasjclark.github.io/mvgam/dev/articles/mvgam_overview.html)
walks through a full analysis.

## Usage

``` r
mvgam(
  formula,
  trend_formula = NULL,
  data = NULL,
  newdata = NULL,
  trend_map = NULL,
  loadings_prior = NULL,
  backend = getOption("brms.backend", "cmdstanr"),
  combine = TRUE,
  family = gaussian(),
  threads = NULL,
  save_pars = brms::save_pars(),
  run_model = TRUE,
  ...
)
```

## Arguments

- formula:

  Main observation model formula (supports brms syntax). Smooth specials
  `s()`, `t2()`, `gp()`, varying intercepts and slopes, monotonic
  `mo()`, measurement-error `me()`, distributional `dpar` sub-formulas
  and [`offset()`](https://rdrr.io/r/stats/offset.html) are all
  available. `gp()` accepts both the approximate Hilbert-space form
  (`gp(x, k = 20)`) and the exact full-covariance form (`gp(x)`, no
  `k`). Prediction at `newdata` requires the approximate form. An exact
  `gp()` term fits and scores in-sample. `mvgam()` says so once per
  session.

- trend_formula:

  A one-sided formula for the latent process: a trend constructor such
  as
  [`AR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  with any predictors of the latent state. Write
  [`lv_axis()`](https://nicholasjclark.github.io/mvgam/dev/reference/lv_axis.md)
  in a term to vary its effect by latent factor, or by series in a model
  without factors, as in `~ x:lv_axis() + AR()`. The series column
  itself is not supported in a term of this formula. Forecasting
  requires the approximate form of any `gp()` term here.

- data:

  Data frame in long format, one row per series and time. A model with a
  trend needs a numeric `time` column and a factor `series` column, or
  the columns a trend constructor names through its `time` and `series`
  arguments.
  [`mvgam_data()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_data.md)
  checks a frame against a family before fitting. A list of multiply
  imputed data frames, or a `mids` object from mice, is also accepted.
  [`mvgam_multiple()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_multiple.md)
  describes the imputation path.

- newdata:

  Optional held-out `data.frame`, stored on the fit as
  `object$test_data`. Fitting draws no forecasts.
  [`forecast()`](https://generics.r-lib.org/reference/forecast.html)
  called without `newdata` forecasts these times, carrying the fitted
  latent state forward. `plot(fit, type = "series")` overlays them.
  `posterior_predict()` on these data uses the covariate part alone. The
  section 'Which prediction function?' of
  [`predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/predict.mvgam.md)
  compares the two.

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
  argument requires a `trend_formula`. The trend constructor takes a
  `trend_map` argument too. Supplying both is an error. `loadings_prior`
  lists the `trend_map` forms a structured loadings prior accepts.

- loadings_prior:

  Optional named list giving a structured prior on the loadings of a
  factor model, such as species traits or a phylogeny. Most models leave
  it `NULL`. The section 'Structured loadings priors' of `mvgam()`
  describes its fields.

- backend:

  `"cmdstanr"` or `"rstan"`. Defaults to
  `getOption("brms.backend", "cmdstanr")`.

- combine:

  Logical. With imputed `data`, `TRUE` (default) pools the imputation
  fits into one and `FALSE` returns them as a list.

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
  reaches negative values, where the likelihood is undefined. `mvgam()`
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

- threads:

  Positive integer number of threads per chain, or `NULL` (the default)
  for none. The model is compiled with
  `cpp_options$stan_threads = TRUE`. cmdstanr then samples with
  `threads_per_chain = threads`. The closure-unit families
  ([`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md),
  [`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md))
  and the multi-response families
  ([`diri()`](https://nicholasjclark.github.io/mvgam/dev/reference/diri.md),
  [`mvn()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvn.md),
  [`mvt()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvt.md),
  [`multi()`](https://nicholasjclark.github.io/mvgam/dev/reference/multi.md),
  [`categ()`](https://nicholasjclark.github.io/mvgam/dev/reference/categ.md))
  split their likelihood over units with `reduce_sum`, in about eight
  chunks. With 50 or more closure units, `threads = 4` samples about a
  third faster. Fewer units can sample slower, the threading overhead
  outweighing the gain. A brms family without a `trend_formula` uses the
  threading brms writes.

  A brms family with a `trend_formula` samples each chain on one thread
  whatever `threads` says. The threaded likelihood brms writes leaves no
  place to add the trend. Each such fit warns.

- save_pars:

  A
  [`brms::save_pars()`](https://paulbuerkner.com/brms/reference/save_pars.html)
  object naming which parameters the posterior keeps. A model is built
  from working variables it also reports under another name: the
  standardised deviates a group-level block is scaled from, the unscaled
  coefficients a shrinkage prior scales, the ordered intercepts a
  mixture is identified by. Those are left out by default, as brms
  leaves them out. `save_pars(all = TRUE)` keeps every one. `group` and
  `latent` keep the per-level deviations and the latent values of a
  `me()` covariate. `manual` names individual parameters to keep.

- run_model:

  Logical. `FALSE` builds the model without parsing, compiling or
  sampling it. The result holds `$stancode`, `$standata` and the prior
  table, with `$fit` set to `NULL`.
  [`stancode.mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/stancode.mvgam_formula.md),
  [`standata.mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/stancode.mvgam_formula.md)
  and
  [`get_prior.mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.mvgam_formula.md)
  return them. Methods that need draws, such as
  [`summary()`](https://rdrr.io/r/base/summary.html) and
  [`forecast()`](https://generics.r-lib.org/reference/forecast.html),
  refuse it. A
  [`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
  model has no
  [`mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_formula.md),
  which makes `run_model = FALSE` the way to inspect one before fitting.
  Defaults to `TRUE`.

- ...:

  Further arguments for the model and its sampler.

  `prior` takes a `brmsprior` built with
  [`brms::prior()`](https://paulbuerkner.com/brms/reference/set_prior.html).
  Its rows replace the matching defaults.
  [`get_prior.mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.mvgam_formula.md)
  lists every class a model takes, with trend classes carrying a
  `_trend` suffix. The plural `priors` is accepted as well.

  `chains`, `iter`, `warmup`, `thin`, `seed` and `control` reach Stan as
  in [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html).
  `chains` defaults to 4, each running 2000 iterations of which the
  first half are warmup. Results for publication should use at least
  four. The examples use two for speed. If the sampler reports divergent
  transitions, refit with `control = list(adapt_delta = 0.95)` or
  simplify the model.
  [mvgam_diagnostics](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_diagnostics.md)
  describes the checks. `cores` sets how many chains sample at once. It
  defaults to `getOption("mc.cores")` when that option is set and
  otherwise to one fewer than the machine's cores, up to `chains`.

  `silent` sets how much the fit prints. `0` shows everything, Stan's
  own messages included. `1`, the default, shows the sampler's progress
  and mvgam's notices. `2` prints warnings alone.

  `algorithm` selects how the posterior is explored. It accepts
  `"sampling"` (the default), `"meanfield"`, `"fullrank"` or
  `"fixed_param"` on either backend, plus `"pathfinder"` and `"laplace"`
  when `backend = "cmdstanr"`.

  `init` sets the starting values. It accepts `"random"` (the default),
  `"0"`, a numeric value, a list or a function. With
  `backend = "cmdstanr"` it also accepts `"pathfinder"`. That keyword
  runs Stan's Pathfinder approximation on the compiled model and draws
  one starting value per chain from it. Sampling then proceeds as usual
  from those values. It helps a model whose chains stall at a poor
  starting point or fill warmup with divergences. On a model that
  already warms up cleanly it adds about a second and lowers effective
  samples per second.

  Five arguments shape the Stan program and reach brms unchanged:

  - `knots` gives knot positions by covariate name, for the observation
    and trend formulas alike.

  - `drop_unused_levels` decides whether a factor level with no
    observations still takes a coefficient.

  - `sample_prior` takes `"no"`, `"yes"` or `"only"`. Under `"only"` the
    observation likelihood drops out and the latent process keeps its
    prior. The draws then show what the model implies before any
    response is observed.

  - `normalize = FALSE` drops the normalising constants from the
    sampling statements. Sampling is faster and the posterior is
    unchanged.

  - `save_model` names a file to write the Stan program to.

## Value

An object of class `mvgam`.

## Structured loadings priors

A factor trend loads each series on a few latent factors through the
matrix `Z`. By default each entry of `Z` takes an independent
`student_t(3, 0, 0.5)` prior. `loadings_prior` replaces it with the
structured prior of Heaps and Jermyn (2024). Each column of `Z` then
follows a multivariate normal whose covariance `Phi` makes similar
series load alike. `Phi` is the product of a kernel over series features
and one factor `exp(-d / theta)` per distance matrix. The argument
requires a `trend_formula` whose trend has latent factors.

The list takes these fields:

- `features`:

  Per-series features, as a numeric matrix, a data frame or the name of
  an element of `data2`. Numeric, integer and logical columns are
  z-scored. An ordered factor is z-scored on its codes. An unordered
  factor or character column is one-hot encoded with every level kept
  (Heaps and Jermyn 2024, Section 6.2). Each encoded column takes its
  own length-scale.

- `distances`:

  Pairwise distance matrices, as the names of elements of `data2`, a
  matrix or a named list of matrices. Each is rescaled to a largest
  entry of 1.

- `column_shrinkage`:

  `"iid"`, the default, shrinks no column. `"mgp"` shrinks later columns
  more strongly through the multiplicative gamma process of Bhattacharya
  and Dunson (2011), applied to the shared innovation scale.
  [`AR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md),
  [`RW()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  and
  [`ZMVN()`](https://nicholasjclark.github.io/mvgam/dev/reference/ZMVN.md)
  trends accept it. A
  [`VAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  trend builds its own innovation covariance. It takes `"iid"`.

- `mgp_a1`, `mgp_a2`:

  The hyperparameters of `"mgp"`, by default 2 and 4. Heaps and Jermyn
  used an `mgp_a2` of 3 and 6 in their two applications. A lower
  `mgp_a2` shrinks less, for data that weakly identify the number of
  active factors.

Each length-scale takes a `lognormal(0, 1)` prior. Distances are
rescaled to a largest entry of 1. A length-scale of 1 then gives a
correlation of `exp(-1)` between the two most distant series. Heaps and
Jermyn used the wider `lognormal(0, sqrt(10))`. A
[`prior()`](https://paulbuerkner.com/brms/reference/set_prior.html) on
`theta_features` or `theta_dist_<name>` replaces the default. They
rescaled a phylogeny to a root-to-tip distance of 1, which gives a
largest pairwise distance of 2. Rescale a distance matrix before fitting
to match their scale.

The prior applies to the sampled columns of `Z`. The reported loadings
`Z_tilde` come from a QR rotation of them. Each column of `Z_tilde`
mixes the sampled columns.
[`shared_variation()`](https://nicholasjclark.github.io/mvgam/dev/reference/shared_variation.md)
and
[`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md)
use `Z Z'`, which the rotation leaves unchanged. The Identification
section of
[trend_constructors](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
describes the rotation.

A `trend_map` with no fixed entries, `NULL` or a matrix of `NA`, is
compatible.
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
passes the second form. A `trend_map` that fixes any loading is refused.
Its free entries take the default prior of class `Z`, which the
structured prior would duplicate.

## Terms used in this documentation

- Latent trend, latent state:

  The unobserved value \\z\_{t,s}\\ that the trend constructor evolves
  over time.

- Innovation:

  The random change the trend takes at each time step,
  \\\epsilon\_{t,s}\\ above. Other texts call it a process error or a
  shock. `cor = TRUE` correlates the innovations of different series.

- Fitted state, covariate part:

  The two versions of the trend a prediction can use. The section 'Which
  prediction function?' of
  [`predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/predict.mvgam.md)
  defines them.

- Prior:

  A distribution stating which values of a parameter are plausible
  before the data are seen.
  [`get_prior.mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.mvgam_formula.md)
  lists the defaults.

- Posterior draw:

  One joint sample of every parameter from the posterior distribution.
  Summaries and predictions average over the draws.

- Hindcast, forecast:

  Predictions at the training times and after them. See
  [`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md)
  and
  [`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md).

- Closure unit:

  A site, or a site in one season, whose true occupancy or abundance
  holds across its repeat visits. See
  [`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md)
  and
  [`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md).

- ELPD, Pareto k:

  The expected log predictive density scores predictions of held-out
  data. Higher is better. A Pareto k above 0.7 marks an observation
  whose leave-one-out approximation is unreliable. See
  [`loo.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo.mvgam.md).

## Workflow

A typical analysis takes these steps:

1.  Check the data with
    [`mvgam_data()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_data.md).

2.  Choose a family
    ([mvgam_families](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_families.md))
    and a trend
    ([trend_constructors](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)).
    Inspect the default priors with
    [`get_prior.mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.mvgam_formula.md).

3.  Fit with `mvgam()`, then check convergence with
    [`summary.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam.md)
    and
    [mvgam_diagnostics](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_diagnostics.md).

4.  Check the fit against the data with
    [`pp_check.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/pp_check.mvgam.md)
    and `plot(fit, type = "residuals")`.

5.  Interpret effects with
    [`conditional_effects.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_effects.mvgam.md).

6.  Forecast with
    [`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md).
    Compare models with
    [`lfo_cv.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md)
    or
    [`loo.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo.mvgam.md).

7.  Report the model with
    [`how_to_cite()`](https://nicholasjclark.github.io/mvgam/dev/reference/how_to_cite.mvgam.md)
    and
    [`methods_md()`](https://nicholasjclark.github.io/mvgam/dev/reference/methods_md.md).

## References

Clark, N. J. and Wells, K. (2023). Dynamic Generalized Additive Models
for forecasting discrete ecological time series. *Methods in Ecology and
Evolution*, 14:771-784.
[doi:10.1111/2041-210X.13974](https://doi.org/10.1111/2041-210X.13974)

Burkner, P.-C. (2017). brms: An R package for Bayesian multilevel models
using Stan. *Journal of Statistical Software*, 80(1):1-28.
[doi:10.18637/jss.v080.i01](https://doi.org/10.18637/jss.v080.i01)

Wood, S. N. (2017). *Generalized Additive Models: An Introduction with
R* (2nd edition). Chapman and Hall/CRC.

Heaps, S. E. and Jermyn, I. H. (2024). Structured prior distributions
for the covariance matrix in latent factor models. *Statistics and
Computing*, 34:143.
[doi:10.1007/s11222-024-10454-0](https://doi.org/10.1007/s11222-024-10454-0)

Bhattacharya, A. and Dunson, D. B. (2011). Sparse Bayesian infinite
factor models. *Biometrika*, 98(2):291-306.
[doi:10.1093/biomet/asr013](https://doi.org/10.1093/biomet/asr013)

Riutort-Mayol, G., Burkner, P.-C., Andersen, M. R., Solin, A. and
Vehtari, A. (2023). Practical Hilbert space approximate Bayesian
Gaussian processes for probabilistic programming. *Statistics and
Computing*, 33:1.
[doi:10.1007/s11222-022-10167-2](https://doi.org/10.1007/s11222-022-10167-2)

## See also

[trend_constructors](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
lists the latent trends.
[mvgam_families](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_families.md)
lists the observation families mvgam adds to those of brms.
[`how_to_cite()`](https://nicholasjclark.github.io/mvgam/dev/reference/how_to_cite.mvgam.md)
writes a methods paragraph for a fitted model.
[`methods_md()`](https://nicholasjclark.github.io/mvgam/dev/reference/methods_md.md)
writes its equations.
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
fits joint species distribution models. The [overview
vignette](https://nicholasjclark.github.io/mvgam/dev/articles/mvgam_overview.html)
introduces the package. The [data
vignette](https://nicholasjclark.github.io/mvgam/dev/articles/data.html)
sets out the data format. The [forecast evaluation
article](https://nicholasjclark.github.io/mvgam/dev/articles/forecast_evaluation.html)
scores and compares forecasts. The [multivariate formula
article](https://nicholasjclark.github.io/mvgam/dev/articles/mvbf.html)
fits several responses at once. The [integrated distribution model
article](https://nicholasjclark.github.io/mvgam/dev/articles/idm.html)
ties several observation families to one latent process.

Other model fitting:
[`check_newdata()`](https://nicholasjclark.github.io/mvgam/dev/reference/check_newdata.md),
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md),
[`mvgam_data()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_data.md),
[`mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_formula.md),
[`mvgam_multiple()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_multiple.md),
[`pivot_detection_array()`](https://nicholasjclark.github.io/mvgam/dev/reference/pivot_detection_array.md),
[`pivot_species_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/pivot_species_matrix.md),
[`series_to_mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/series_to_mvgam.md),
[`stancode.mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/stancode.mvgam_formula.md),
[`update.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/update.mvgam.md)

## Examples

``` r
# \dontrun{
# One count series with a smooth effect of a covariate x and an
# AR(1) latent trend, split into training and test times
set.seed(2)
simdat <- sim_mvgam(family = poisson(), n_timepoints = 100L,
                    trend_model = AR(), prop_trend = 0.5)
head(simdat$data_train)
#>    y   series time          x
#> 1  6 series_1    1 -1.2604710
#> 2  3 series_1    2  0.8094961
#> 3 19 series_1    3  0.2933053
#> 4  7 series_1    4 -1.3277923
#> 5  7 series_1    5  1.7753574
#> 6  4 series_1    6  1.7738998

# The parameters a model takes, with their default priors, before
# any fitting. A `prior` argument to mvgam() replaces any of them
get_prior(
  mvgam_formula(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1)),
  data   = simdat$data_train,
  family = poisson()
)
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

# A smooth of x in the observation model and an AR(1) latent trend.
# The test data are stored on the fit for forecasting
mod <- mvgam(
  y ~ s(x, k = 5),
  trend_formula = ~ AR(p = 1),
  data          = simdat$data_train,
  newdata       = simdat$data_test,
  family        = poisson(),
  chains        = 2,
  silent        = 2
)
#> Warning: 20 of 2000 (1.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
summary(mod)
#>  Family: poisson 
#>   Links: mu = log 
#> Formula: y ~ s(x, k = 5) 
#>    Data: simdat$data_train (Number of observations: 75) 
#>  Trends: AR(1) 
#>   Draws: 2 chains, each with iter = 2000; warmup = 1000; thin = 1; 
#>          total post-warmup draws = 2000
#> 
#> == Observation Model ==
#> Smoothing Spline Hyperparameters:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sds(sx_1)     4.18      1.81     1.65     9.00 1.01      774      704
#> 
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept     1.55      0.15     1.25     1.85 1.00      855      843
#> sx_1          7.17      2.77     1.64    12.60 1.00      781     1120
#> 
#> == Trend Model ==
#> Trend Specific Parameters:
#>                       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
#> sigma_trend[series_1]     0.54      0.08     0.38     0.71 1.00      837
#> ar1_trend[series_1]       0.46      0.17     0.11     0.76 1.00      522
#>                       Tail_ESS
#> sigma_trend[series_1]     1195
#> ar1_trend[series_1]        918
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
#> 
#> Next steps:
#>   - `pp_check(fit)`: posterior predictive checks
#>   - `forecast(fit, newdata = ...)`: out-of-sample forecasts
#>   - `lfo_cv(fit)`: leave-future-out model comparison
#>   - `conditional_effects(fit)`: covariate effects
#> Use `how_to_cite(fit)` for a methods paragraph and `methods_md(fit)` for its equations.

# Do draws from the model resemble the observed counts?
pp_check(mod)


# The effect of x on the expected count
conditional_effects(mod)


# Forecasts of the test times, with the held-out counts overlaid
plot(forecast(mod))


# A methods paragraph with its references
how_to_cite(mod)
#> Methods text skeleton
#> We used the R package mvgam (version 2.0.0; Clark & Wells, 2023) to
#>   construct, fit and interrogate the model. mvgam fits Bayesian
#>   state-space models that combine flexible predictor effects in both the
#>   process and observation components, using functionality from the brms
#>   (Burkner 2017) and mgcv (Wood 2017) packages. Observations followed the
#>   poisson family with a log link. The latent process followed AR(1)
#>   dynamics. The mvgam-constructed model and data were passed to Stan
#>   (Carpenter et al. 2017) via the cmdstanr interface (Gabry et al. 2024).
#>   We ran 2 Hamiltonian Monte Carlo chains for 1000 warmup iterations and
#>   1000 sampling iterations. Rank-normalised split Rhat and effective
#>   sample sizes (Vehtari et al. 2021) were used to monitor convergence.
#> 
#> Primary references
#> Burkner PC (2017). brms: An R Package for Bayesian Multilevel Models
#>   Using Stan. Journal of Statistical Software, 80(1), 1-28.
#>   https://doi.org/10.18637/jss.v080.i01
#> Carpenter B, Gelman A, Hoffman MD, Lee D, Goodrich B, Betancourt M,
#>   Brubaker M, Guo J, Li P and Riddell A (2017). Stan: A probabilistic
#>   programming language. Journal of Statistical Software, 76(1), 1-32.
#>   https://doi.org/10.18637/jss.v076.i01
#> Clark NJ and Wells K (2023). Dynamic Generalized Additive Models (DGAMs)
#>   for forecasting discrete ecological time series. Methods in Ecology and
#>   Evolution, 14(3), 771-784. https://doi.org/10.1111/2041-210X.13974
#> Gabry J, Cesnovar R, Johnson A and Bronder S (2024). cmdstanr: R
#>   Interface to 'CmdStan'. https://mc-stan.org/cmdstanr/
#> Vehtari A, Gelman A, Simpson D, Carpenter B and Burkner P (2021).
#>   Rank-normalization, folding, and localization: An improved Rhat for
#>   assessing convergence of MCMC. Bayesian Analysis, 16(2), 667-718.
#>   https://doi.org/10.1214/20-BA1221
#> Wood SN (2017). Generalized Additive Models: An Introduction with R (2nd
#>   edition). Chapman and Hall/CRC.
#> 
#> Other useful references
#> Arel-Bundock V, Greifer N and Heiss A (2024). How to interpret
#>   statistical models using marginaleffects for R and Python. Journal of
#>   Statistical Software, 111(9), 1-32.
#>   https://doi.org/10.18637/jss.v111.i09
#> Burkner PC, Gabry J and Vehtari A (2020). Approximate leave-future-out
#>   cross-validation for Bayesian time series models. Journal of Statistical
#>   Computation and Simulation, 90(14), 2499-2523.
#>   https://doi.org/10.1080/00949655.2020.1783262
#> Gabry J, Simpson D, Vehtari A, Betancourt M and Gelman A (2019).
#>   Visualization in Bayesian workflow. Journal of the Royal Statistical
#>   Society: Series A (Statistics in Society), 182(2), 389-402.
#>   https://doi.org/10.1111/rssa.12378
#> Vehtari A, Gelman A and Gabry J (2017). Practical Bayesian model
#>   evaluation using leave-one-out cross-validation and WAIC. Statistics and
#>   Computing, 27(5), 1413-1432. https://doi.org/10.1007/s11222-016-9696-4
#> 
#> Use `bibtex(x)` for a BibTeX block ready for your .bib file.

# Next: a slope that drifts over time takes `s(time, by = x)` in the
# formula. loo() and lfo_cv() compare models. The articles under
# 'See Also' fit many series, factor models and VAR trends
# }
```
