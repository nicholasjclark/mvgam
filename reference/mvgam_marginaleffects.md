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

An [`occ()`](https://nicholasjclark.github.io/mvgam/reference/occ.md) or
[`nmix()`](https://nicholasjclark.github.io/mvgam/reference/nmix.md) fit
takes two more types. `"detection"` gives the detection probability of
each visit. `"latent_N"` gives the latent state that mvgam's own methods
call `"latent_state"`. marginaleffects checks `type` against its own
list, which records that state as `"latent_N"`. Each row of a grid from
[`marginaleffects::datagrid()`](https://rdrr.io/pkg/marginaleffects/man/datagrid.html)
becomes its own closure unit.

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
[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/hindcast.mvgam.md)
and
[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/forecast.mvgam.md)
return the state the model inferred.

## References

Arel-Bundock, V., Greifer, N. and Heiss, A. (2024). How to interpret
statistical models using marginaleffects for R and Python. *Journal of
Statistical Software*, 111(9):1-32.
[doi:10.18637/jss.v111.i09](https://doi.org/10.18637/jss.v111.i09)

## See also

[`conditional_effects.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/conditional_effects.mvgam.md)
for quick effect plots,
[`posterior_epred.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/posterior_epred.mvgam.md)
for the predictions these methods take.

## Examples

``` r
# \dontrun{
library(marginaleffects)
set.seed(1)
simdat <- sim_mvgam(family = poisson(), n_series = 1L,
                    n_timepoints = 60L, trend_model = AR())
mod <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
             data = simdat$data_train, family = poisson(),
             chains = 2, seed = 2, silent = 2)
#> Warning: 27 of 2000 (1.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# Average change in the expected count per unit of x
avg_slopes(mod, variables = "x", type = "expected")
#> 
#>  Estimate 2.5 % 97.5 %
#>       4.1     2   7.58
#> 
#> Term: x
#> Type: expected
#> Comparison: dY/dX
#> 

# The same slope with the trend's innovations averaged in
avg_slopes(mod, variables = "x", type = "expected",
           process_error = TRUE)
#> Warning: These arguments are not known to be supported for models of class `mvgam`: process_error. All arguments are still passed to the model-specific prediction function, but users are encouraged to check if the argument is indeed supported by their modeling package. Please file a request on Github if you believe that an unknown argument should be added to the `marginaleffects` white list of known arguments, in order to avoid raising this warning: https://github.com/vincentarelbundock/marginaleffects/issues
#> 
#>  Estimate 2.5 % 97.5 %
#>      4.69  2.25   9.68
#> 
#> Term: x
#> Type: expected
#> Comparison: dY/dX
#> 

# Expected counts across the range of x
plot_predictions(mod, condition = "x", type = "expected")

# }
```
