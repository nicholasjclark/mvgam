# Stan code and data of an mvgam model

The Stan program and data list of an mvgam model. For an
[`mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_formula.md)
they are generated without fitting, as
[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
would build them from the same arguments. For an `mvgam` model they are
the ones the model was built with. Pass the two to rstan or cmdstanr to
run the model outside mvgam. The program shows how the trend enters the
linear predictor.

## Usage

``` r
# S3 method for class 'mvgam_formula'
stancode(
  object,
  data,
  family = gaussian(),
  prior = NULL,
  data2 = NULL,
  sample_prior = "no",
  knots = NULL,
  drop_unused_levels = TRUE,
  backend = "rstan",
  threads = 1L,
  normalize = TRUE,
  save_model = NULL,
  silent = 1L,
  stanvars = NULL,
  validate = TRUE,
  ...
)

# S3 method for class 'mvgam_formula'
standata(
  object,
  data,
  family = gaussian(),
  prior = NULL,
  data2 = NULL,
  sample_prior = "no",
  knots = NULL,
  drop_unused_levels = TRUE,
  backend = "rstan",
  threads = 1L,
  normalize = TRUE,
  save_model = NULL,
  silent = 1L,
  stanvars = NULL,
  validate = TRUE,
  ...
)

# S3 method for class 'mvgam'
stancode(object, ...)

# S3 method for class 'mvgam'
standata(object, ...)
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

- prior:

  A `brmsprior` of priors to set, from
  [`brms::prior()`](https://paulbuerkner.com/brms/reference/set_prior.html).
  [`get_prior.mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.mvgam_formula.md)
  lists the classes.

- data2:

  A named list of objects the model uses that are not variables in
  `data`, as in
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html).

- sample_prior:

  `"no"` (the default), `"yes"` or `"only"`, as in
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html).

- knots:

  Optional named list of knot positions for spline terms, used by the
  observation and trend formulas alike.

- drop_unused_levels:

  Logical. `TRUE` (the default) drops factor levels without
  observations.

- backend:

  `"rstan"` (the default) or `"cmdstanr"`.

- threads:

  Number of threads within each chain. Defaults to 1.

- normalize:

  Logical. `TRUE` (the default) keeps the normalising constants of the
  sampling statements.

- save_model:

  File path to write the Stan program to, or `NULL` (the default) to
  write nothing.

- silent:

  Integer verbosity, as in
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html). 0
  prints Stan's exceptions and progress, 1 (the default) prints progress
  and 2 prints nothing.

- stanvars:

  Optional
  [`brms::stanvar()`](https://paulbuerkner.com/brms/reference/stanvar.html)
  object of Stan code to add to the program, merged with the code mvgam
  writes for the trend.

- validate:

  Logical. `TRUE` (the default) checks the program with the Stan parser.

- ...:

  For an
  [`mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_formula.md),
  other arguments of
  [`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md).
  A name
  [`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
  does not take is refused. For an `mvgam` model, unused. Anything
  passed is refused.

## Value

[`stancode()`](https://paulbuerkner.com/brms/reference/stancode.html)
returns the Stan program as a character string of class `mvgamstancode`,
which prints line by line.
[`standata()`](https://paulbuerkner.com/brms/reference/standata.html)
returns the named list of Stan data.

## See also

[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md),
[`mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_formula.md),
[`get_prior.mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.mvgam_formula.md).
[`methods_md()`](https://nicholasjclark.github.io/mvgam/dev/reference/methods_md.md)
writes the same model as equations for a methods section.

Other model fitting:
[`check_newdata()`](https://nicholasjclark.github.io/mvgam/dev/reference/check_newdata.md),
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md),
[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md),
[`mvgam_data()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_data.md),
[`mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_formula.md),
[`mvgam_multiple()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_multiple.md),
[`pivot_detection_array()`](https://nicholasjclark.github.io/mvgam/dev/reference/pivot_detection_array.md),
[`pivot_species_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/pivot_species_matrix.md),
[`series_to_mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/series_to_mvgam.md),
[`update.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/update.mvgam.md)

## Examples

``` r
set.seed(1)
dat <- sim_mvgam(family = poisson(), n_series = 2L,
                 n_timepoints = 40L, trend_model = AR())$data_train
mf <- mvgam_formula(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1))

# The program of the model, before any fitting. The trend's
# parameters carry a `_trend` suffix.
code <- stancode(mf, data = dat, family = poisson())
code
#> // Generated with mvgam 2.0.0 using brms 2.23.2
#> functions {
#>   
#> }
#> data {
#>   int<lower=1> N; // total number of observations
#>   array[N] int Y; // response variable
#>   
#>   // data for splines
#>   int Ks; // number of linear effects
#>   matrix[N, Ks] Xs; // design matrix for the linear effects
#>   
#>   // data for spline 1
#>   int nb_1; // number of bases
#>   array[nb_1] int knots_1; // number of knots
#>   
#>   // basis function matrices
#>   matrix[N, knots_1[1]] Zs_1_1;
#>   int prior_only; // should the likelihood be ignored?
#>   int<lower=1> N_trend; // total number of observations
#>   int<lower=1> N_series_trend;
#>   int<lower=1> N_lv_trend;
#>   int<lower=1> N_time_trend;
#>   array[N_time_trend, N_series_trend] int times_trend;
#>   array[N] int<lower=1, upper=N_time_trend> obs_trend_time;
#>   array[N] int<lower=1, upper=N_series_trend> obs_trend_series;
#> }
#> transformed data {
#>   matrix[N_series_trend, N_lv_trend] Z = diag_matrix(
#>                                                      rep_vector(1.0,
#>                                                                 N_lv_trend));
#>   array[N] int obs_trend_idx;
#>   for (n in 1 : N) {
#>     obs_trend_idx[n] = (obs_trend_series[n] - 1) * N_time_trend
#>                        + obs_trend_time[n];
#>   }
#> }
#> parameters {
#>   real Intercept; // temporary intercept for centered predictors
#>   vector[Ks] bs; // unpenalized spline coefficients
#>   
#>   // parameters for spline 1
#>   // standardized penalized spline coefficients
#>   vector[knots_1[1]] zs_1_1;
#>   vector<lower=0>[nb_1] sds_1; // SDs of penalized spline coefficients
#>   vector<lower=0>[N_lv_trend] sigma_trend;
#>   matrix[N_time_trend, N_lv_trend] innovations_trend;
#>   
#>   // AR coefficient parameters
#>   vector<lower=-1, upper=1>[N_lv_trend] ar1_trend;
#> }
#> transformed parameters {
#>   // penalized spline coefficients
#>   vector[knots_1[1]] s_1_1;
#>   
#>   // prior contributions to the log posterior
#>   real lprior = 0;
#>   lprior += student_t_lpdf(Intercept | 3, 1.6, 2.5);
#>   lprior += student_t_lpdf(sds_1 | 3, 0, 2.5)
#>             - 1 * student_t_lccdf(0 | 3, 0, 2.5);
#>   
#>   matrix[N_time_trend, N_lv_trend] scaled_innovations_trend;
#>   scaled_innovations_trend = diag_post_multiply(innovations_trend,
#>                                sigma_trend);
#>   matrix[N_time_trend, N_lv_trend] lv_trend;
#>   
#>   // Stationary start: the t = 1 innovation scaled to the
#>   // marginal SD sigma / sqrt(1 - ar1^2)
#>   lv_trend[1] = scaled_innovations_trend[1] ./ sqrt(1 - square(ar1_trend))';
#>   
#>   // AR dynamics
#>   for (i in 2 : N_time_trend) {
#>     for (j in 1 : N_lv_trend) {
#>       lv_trend[i, j] = ar1_trend[j] * lv_trend[i - 1, j]
#>                        + scaled_innovations_trend[i, j];
#>     }
#>   }
#>   
#>   // Each series' trend is the series' state
#>   matrix[N_time_trend, N_series_trend] trend = lv_trend;
#>   
#>   // compute penalized spline coefficients
#>   s_1_1 = sds_1[1] * zs_1_1;
#> }
#> model {
#>   // likelihood including constants
#>   if (!prior_only) {
#>     // initialize linear predictor term
#>     vector[N] mu = rep_vector(0.0, N);
#>     mu += Intercept + Xs * bs + Zs_1_1 * s_1_1;
#>     mu += to_vector(trend)[obs_trend_idx];
#>     target += poisson_log_lpmf(Y | mu);
#>   }
#>   
#>   // priors including constants
#>   target += lprior;
#>   // Shared innovation priors
#>   target += exponential_lpdf(sigma_trend | 2);
#>   target += std_normal_lpdf(to_vector(innovations_trend));
#>   // AR coefficient priors
#>   target += normal_lpdf(ar1_trend | 0, 0.5);
#>   target += std_normal_lpdf(zs_1_1);
#> }
#> generated quantities {
#>   // actual population-level intercept
#>   real b_Intercept = Intercept;
#> }
#> 

# The data passed to that program: 60 observations of 2 series
# over 30 time points
sdata <- standata(mf, data = dat, family = poisson())
sdata[c("N", "N_series_trend", "N_time_trend")]
#> $N
#> [1] 60
#> 
#> $N_series_trend
#> [1] 2
#> 
#> $N_time_trend
#> [1] 30
#> 

# Next, fit the model with mvgam(), which compiles this program
```
