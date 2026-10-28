# Posterior predictive errors on a fitted mvgam model

Posterior draws of the prediction error, observed minus predicted, of
each observation on the response scale. The arguments are those of
[`brms::predictive_error.brmsfit()`](https://paulbuerkner.com/brms/reference/predictive_error.brmsfit.html).

## Usage

``` r
# S3 method for class 'mvgam'
predictive_error(
  object,
  newdata = NULL,
  re_formula = NULL,
  re.form = NULL,
  method = "posterior_predict",
  resp = NULL,
  ndraws = NULL,
  draw_ids = NULL,
  sort = FALSE,
  ...
)
```

## Arguments

- object:

  A fitted mvgam object from
  [`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md).

- newdata:

  Optional data frame with covariates for prediction. If NULL, uses
  original training data stored in the model object.

- re_formula:

  Group-level terms to include: `NULL` (the default) for every one, `NA`
  for none. A formula choosing some of them is not supported.

- re.form:

  The lme4 name for `re_formula`.

- method:

  `"posterior_predict"` (the default) takes the errors from predictive
  draws, `"posterior_epred"` from expected values.

- resp:

  Character specifying which response variable for multivariate models.
  NULL (default) returns predictions for all responses.

- ndraws:

  Positive integer specifying number of posterior draws to use. NULL
  (default) uses all available draws.

- draw_ids:

  Optional integer vector selecting a subset of posterior draw indices
  to use. NULL (default) uses all draws (subject to `ndraws`).

- sort:

  Logical. `TRUE` orders the columns by the observed response. Defaults
  to `FALSE`.

- ...:

  Further arguments of the `method` function, such as `incl_autocor` or
  `allow_new_levels`.

## Value

An `[ndraws, nobs]` matrix of observed minus predicted values.

## Details

The errors are those
[`residuals.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/residuals.mvgam.md)
returns with `type = "ordinary"`. In sample, each prediction carries the
latent trend state the model inferred at that time. A prediction for
`newdata` integrates over the trend dynamics.

## See also

[`brms::predictive_error.brmsfit()`](https://paulbuerkner.com/brms/reference/predictive_error.brmsfit.html),
[`residuals.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/residuals.mvgam.md),
[`posterior_predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_predict.mvgam.md),
[`posterior_epred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_epred.mvgam.md).

Other predictions:
[`conditional_effects.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_effects.mvgam.md),
[`conditional_smooths.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_smooths.mvgam.md),
[`fitted.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/fitted.mvgam.md),
[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md),
[`posterior_epred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_epred.mvgam.md),
[`posterior_linpred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_linpred.mvgam.md),
[`posterior_predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_predict.mvgam.md),
[`posterior_smooths.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_smooths.mvgam.md),
[`predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/predict.mvgam.md),
[`predictive_interval.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/predictive_interval.mvgam.md)

## Author

Nicholas J Clark

## Examples

``` r
# \dontrun{
set.seed(13)
simdat <- sim_mvgam(family = poisson(), n_series = 1L,
                    n_timepoints = 120L, trend_model = AR())
mod <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1),
             data = simdat$data_train, family = poisson(),
             chains = 2, silent = 2)
#> Warning: 9 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# Observed minus predicted counts, one row per draw
err <- predictive_error(mod, draw_ids = 1:200)
dim(err)
#> [1] 200  90

# The three observations with the largest mean absolute error
mae <- colMeans(abs(err))
simdat$data_train[order(mae, decreasing = TRUE)[1:3], c("time", "y")]
#>    time  y
#> 71   71 30
#> 84   84 23
#> 89   89 28

# Next: residuals(mod) returns randomised quantile residuals
# }
```
