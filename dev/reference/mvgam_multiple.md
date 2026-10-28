# Fit mvgam models to multiply imputed datasets

Fits one model to each of several imputed datasets and pools the draws,
as
[`brms::brm_multiple()`](https://paulbuerkner.com/brms/reference/brm_multiple.html)
does. The pooled posterior holds the uncertainty the imputations add.
`mvgam(data = )` takes the same list and fits the same models.

## Usage

``` r
mvgam_multiple(formula, trend_formula = NULL, data_list, combine = TRUE, ...)
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
  `gp()` term fits and scores in-sample.
  [`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
  says so once per session.

- trend_formula:

  A one-sided formula for the latent process: a trend constructor such
  as
  [`AR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  with any predictors of the latent state. Write
  [`lv_axis()`](https://nicholasjclark.github.io/mvgam/dev/reference/lv_axis.md)
  in a term to vary its effect by latent factor, or by series in a model
  without factors, as in `~ x:lv_axis() + AR()`. The series column
  itself is not supported in a term of this formula. An exact `gp()`
  term here also predicts in-sample alone. Forecasting a model that
  holds one requires the approximate form.

- data_list:

  A list of at least two imputed data frames, or a `mids` object from
  mice.

- combine:

  Logical. With imputed `data`, `TRUE` (default) pools the imputation
  fits into one and `FALSE` returns them as a list.

- ...:

  Further arguments to
  [`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md),
  such as `family`, `backend`, `newdata`, `chains` or `iter`. Every
  imputation fit stores `newdata`.

## Value

With `combine = TRUE`, a pooled fit of class `mvgam_pooled`, which
inherits the class of the imputation fits. Its attribute
`individual_fits` holds those fits. With `combine = FALSE`, the list of
imputation fits. With `run_model = FALSE`, one unfitted model built from
the first dataset. Pooling needs draws, which an unfitted model lacks.

## Details

The imputations must hold the same columns, of the same classes, in the
same order, with the same number of rows. The columns that place each
row on the time and series axes must match exactly. Impute only the
response and the covariates.

[`rstan::sflist2stanfit()`](https://mc-stan.org/rstan/reference/sflist2stanfit.html)
concatenates the chains of every imputation fit. The pooled draws then
hold both the within- and the between-imputation variance. Because
chains from different imputations sample different posteriors, the R-hat
of the pooled chains measures disagreement between imputations.
[`summary()`](https://rdrr.io/r/base/summary.html) reports each
parameter's largest R-hat and summed ESS over the imputation fits. It
also tabulates the convergence of each fit.

## See also

[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md),
[`summary.mvgam_pooled()`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam_pooled.md)

Other model fitting:
[`check_newdata()`](https://nicholasjclark.github.io/mvgam/dev/reference/check_newdata.md),
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md),
[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md),
[`mvgam_data()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_data.md),
[`mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_formula.md),
[`pivot_detection_array()`](https://nicholasjclark.github.io/mvgam/dev/reference/pivot_detection_array.md),
[`pivot_species_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/pivot_species_matrix.md),
[`series_to_mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/series_to_mvgam.md),
[`stancode.mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/stancode.mvgam_formula.md),
[`update.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/update.mvgam.md)

## Examples

``` r
# \dontrun{
# A covariate with ten missing values, filled three ways. An
# imputation model would draw the fills. This example draws them
# from a normal distribution.
set.seed(1)
dat <- sim_mvgam(family = gaussian(), n_series = 1L,
                 n_timepoints = 60L, trend_model = AR())$data_train
missing_x <- sample(nrow(dat), 10L)
dat$x[missing_x] <- NA
imputed_list <- lapply(1:3, function(i) {
  filled <- dat
  filled$x[missing_x] <- rnorm(10L, mean(dat$x, na.rm = TRUE),
                               sd(dat$x, na.rm = TRUE))
  filled
})

# One fit per imputation, with the posteriors pooled
fit_pooled <- mvgam_multiple(
  formula = y ~ x,
  trend_formula = ~ AR(p = 1),
  data_list = imputed_list,
  family = gaussian(),
  chains = 2,
  silent = 2
)
#> Warning: 47 of 2000 (2.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 220 of 2000 (11.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 1 of 2 chains had an E-BFMI less than 0.3.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 110 of 2000 (6.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# The summary adds the convergence of each imputation fit
summary(fit_pooled)
#>  Family: gaussian 
#>   Links: mu = identity; sigma = log 
#> Formula: y ~ x 
#>    Data: imputed_list (Number of observations: 45) 
#>  Trends: AR(1) 
#>   Draws: 6 chains, each with iter = 2000; warmup = 1000; thin = 1; 
#>          total post-warmup draws = 6000
#> 
#> == Observation Model ==
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept     0.08      0.19    -0.29     0.46 1.01     1643     2284
#> x             0.44      0.19     0.11     0.82 1.03      848     1945
#> 
#> Further Distributional Parameters:
#>       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sigma     1.08      0.22     0.59     1.46 1.08      214      174
#> 
#> == Trend Model ==
#> Trend Specific Parameters:
#>                       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
#> sigma_trend[series_1]     0.41      0.33     0.01     1.16 1.07      192
#> ar1_trend[series_1]       0.09      0.37    -0.70     0.73 1.02     2282
#>                       Tail_ESS
#> sigma_trend[series_1]      174
#> ar1_trend[series_1]       3099
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
#> 
#> Multiple imputation: 3 fits of 2000 draws each, pooled.
#> Rhat is each parameter's largest and each ESS its sum over the fits.
#>    Imputation Max_Rhat Min_Bulk_ESS Min_Tail_ESS
#>             1    1.019           97           54
#>             2    1.087           17           17
#>             3    1.024           73           18

# Next, `combine = FALSE` returns the three fits as a list
# }
```
