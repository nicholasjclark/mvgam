# marginaleffects methods for mvgam objects

These methods let
[`marginaleffects::predictions()`](https://rdrr.io/pkg/marginaleffects/man/predictions.html),
[`marginaleffects::slopes()`](https://rdrr.io/pkg/marginaleffects/man/slopes.html)
and
[`marginaleffects::comparisons()`](https://rdrr.io/pkg/marginaleffects/man/comparisons.html)
take an mvgam fit. `get_predict()` returns the posterior draws in the
`posterior_draws` attribute. marginaleffects computes every summary from
those draws. The uncertainty comes from the draws alone. marginaleffects
also calls `get_coef()`, `get_vcov()` and `set_coef()`. Their output
leaves every result unchanged.

## Prediction scale

mvgam registers three `type` values with marginaleffects:

- `"expected"`: the expectation `E[Y]`. Most other model classes call
  this `"response"`.

- `"response"`: predictive draws with observation noise, on the outcome
  scale. A count family gives integers.

- `"link"`: the linear predictor.

marginaleffects takes `"response"` as the default. Pass
`type = "expected"` for the expectation.
[`comparisons()`](https://rdrr.io/pkg/marginaleffects/man/comparisons.html)
and [`slopes()`](https://rdrr.io/pkg/marginaleffects/man/slopes.html)
replace `"response"` with `"expected"`. A contrast differences two
predictions. Observation noise in each would widen it. A slope also
divides that noise by a small step. The result keeps the label
`"response"`.
[`predictions()`](https://rdrr.io/pkg/marginaleffects/man/predictions.html)
on `"response"` keeps the observation noise. marginaleffects summarises
the draws by their posterior median. For a right-skewed predictive
distribution the median falls below the mean.

## The latent trend

By default (`process_error = FALSE`) each prediction takes the covariate
part of the trend: the effects in its formula, without its innovations.
Each posterior draw keeps its own coefficients. The result shows the
effect of the covariates with the latent process at its mean. Its
credible band leaves out the uncertainty of the latent state.

Under a nonlinear link this falls short of the model's own `E[Y]`.
Averaging over the innovations of an AR(1) trend raises a log-link
expectation by the factor \\\exp(\sigma^2 / (2(1 - \rho^2)))\\.
`process_error = TRUE` adds the innovations. Slopes and comparisons then
describe `E[Y]`, with wider bands.

When the trend explains most of the variation in a series, the
covariates have little left to explain under either setting.
[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md)
and
[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md)
return the state the model inferred.

## References

Arel-Bundock, V., Greifer, N. and Heiss, A. (2024). How to interpret
statistical models using marginaleffects for R and Python. *Journal of
Statistical Software*, 111(9):1-32.
[doi:10.18637/jss.v111.i09](https://doi.org/10.18637/jss.v111.i09)

## See also

[`conditional_effects.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_effects.mvgam.md)
for quick effect plots,
[`posterior_epred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_epred.mvgam.md)
for the predictions these methods take.

## Examples

``` r
# \dontrun{
library(marginaleffects)
set.seed(1)
simdat <- sim_mvgam(n_series = 1L, n_timepoints = 60L,
                    trend_model = AR())
mod <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
             data = simdat$data_train, family = poisson(),
             chains = 2, silent = 2)
#> Error: 'y' has values outside the support of family 'poisson'.
#>   Requires: 0 <= 'y' (integers only).
#>   Observed range: [-2.504, 2.943]. 23 of 45 values fall outside, the first
#>   at row 1.
#>   Check the column for missing-data sentinels such as -1 or -999.

# Average change in the expected count per unit of x
avg_slopes(mod, variables = "x", type = "expected")
#> Error: object 'mod' not found

# The same slope with the trend's innovations averaged in
avg_slopes(mod, variables = "x", type = "expected",
           process_error = TRUE)
#> Error: object 'mod' not found

# Expected counts across the range of x
plot_predictions(mod, condition = "x", type = "expected")
#> Error: object 'mod' not found
# }
```
