# Pair an observation formula with a trend formula

Holds the two formulas of a model before it is fitted.
[`get_prior.mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.mvgam_formula.md),
[`stancode()`](https://paulbuerkner.com/brms/reference/stancode.html)
and
[`standata()`](https://paulbuerkner.com/brms/reference/standata.html)
take the pair, with the data and family, to show the priors, the Stan
program and the data a fit would use. Nothing is compiled or sampled.

## Usage

``` r
mvgam_formula(formula, trend_formula = NULL)
```

## Arguments

- formula:

  A `formula`, `brmsformula` or `mvbrmsformula` for the observation
  model.

- trend_formula:

  A one-sided formula for the latent trend, such as `~ AR(p = 1)`.
  `NULL`, the default, gives a model without a trend, the model brms
  itself would fit.

## Value

An object of class `mvgam_formula` holding `formula` and
`trend_formula`.

## Details

A trend formula holds one trend constructor
([trend_constructors](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)).
It can also hold covariates of the trend, smooths such as `s()` and
`t2()`, random effects such as `(1 | group)` and Gaussian processes
through `gp()`.

Some terms describe how the response was observed. They belong in the
observation formula. A trend formula refuses them:

- addition terms such as
  [`weights()`](https://rdrr.io/r/stats/weights.html), `se()`, `cens()`,
  [`trunc()`](https://rdrr.io/r/base/Round.html), `trials()`, `rate()`,
  [`subset()`](https://rdrr.io/r/base/subset.html) and `thres()`

- [`offset()`](https://rdrr.io/r/stats/offset.html)

- `mi()`, which brms imputes as a response of the model

- the brms autocorrelation terms
  [`ar()`](https://rdrr.io/r/stats/ar.html), `ma()`, `arma()`, `cosy()`,
  `unstr()` and `autocor()`. The trend constructor models dependence
  over time in their place.

## Multivariate models

A multivariate model, written with `mvbind()` or `bf() + bf()`, takes
one trend constructor for all of its responses. Each response can take
its own family and its own observation terms, as in
`bf(count ~ x, family = poisson()) + bf(biomass ~ x, family = Gamma(link = "log"))`.
The `gr` argument of the trend constructor pools the trends of groups.
Responses that need different trend types need separate models.

## See also

[`get_prior.mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.mvgam_formula.md)
lists the priors of the pair.
[`stancode()`](https://paulbuerkner.com/brms/reference/stancode.html)
and
[`standata()`](https://paulbuerkner.com/brms/reference/standata.html)
return its Stan program and data.
[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
fits it.

Other model fitting:
[`check_newdata()`](https://nicholasjclark.github.io/mvgam/dev/reference/check_newdata.md),
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md),
[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md),
[`mvgam_data()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_data.md),
[`mvgam_multiple()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_multiple.md),
[`pivot_detection_array()`](https://nicholasjclark.github.io/mvgam/dev/reference/pivot_detection_array.md),
[`pivot_species_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/pivot_species_matrix.md),
[`series_to_mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/series_to_mvgam.md),
[`stancode.mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/stancode.mvgam_formula.md),
[`update.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/update.mvgam.md)

## Examples

``` r
f <- mvgam_formula(y ~ s(x), trend_formula = ~ AR(p = 1))
f
#> mvgam_formula object
#> Observation formula: y ~ s(x)
#> Trend formula: ~AR(p = 1)

# Observation weights belong in the observation formula
mvgam_formula(y ~ x + weights(w), trend_formula = ~ AR(p = 1))
#> mvgam_formula object
#> Observation formula: y ~ x + weights(w)
#> Trend formula: ~AR(p = 1)
try(mvgam_formula(y ~ x, trend_formula = ~ AR() + weights(w)))
#> Error : brms addition terms are not supported in 'trend_formula'.
#>   Found: 'weights(w)'.
#>   Addition terms change the observation model.
#>   Write them in the observation 'formula'.

# \dontrun{
simdat <- sim_mvgam(family = poisson(), n_series = 1L,
                    trend_model = AR())
# The priors and the Stan program a fit would use
get_prior(f, data = simdat$data_train, family = poisson())
#>                   prior       class coef group resp dpar nlpar lb ub tag
#>  student_t(3, 1.6, 2.5)   Intercept                                     
#>                  (flat)           b                                     
#>                  (flat)           b sx_1                                
#>    student_t(3, 0, 2.5)         sds                             0       
#>    student_t(3, 0, 2.5)         sds s(x)                        0       
#>          exponential(2) sigma_trend                             0       
#>          normal(0, 0.5)   ar1_trend                            -1  1    
#>        source
#>       default
#>       default
#>  (vectorized)
#>       default
#>  (vectorized)
#>       default
#>       default
stancode(f, data = simdat$data_train, family = poisson())
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
# }
```
