# marginaleffects methods for mvgam objects

These methods let
[`marginaleffects::predictions()`](https://rdrr.io/pkg/marginaleffects/man/predictions.html),
[`marginaleffects::slopes()`](https://rdrr.io/pkg/marginaleffects/man/slopes.html)
and
[`marginaleffects::comparisons()`](https://rdrr.io/pkg/marginaleffects/man/comparisons.html)
take an mvgam fit. `get_predict()` returns the posterior draws in the
`posterior_draws` attribute. marginaleffects computes every summary from
those draws. `get_coef()`, `get_vcov()` and `set_coef()` complete the
methods marginaleffects calls. The draws carry the uncertainty. None of
the three changes a result.

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

With `process_error = FALSE`, the default, the trend contributes its
deterministic submodel. Each draw keeps its own coefficients. The result
is a covariate effect with the latent process held out. Its credible
band excludes the uncertainty of the latent state.

A slope or comparison taken this way is biased low against the model's
own `E[Y]`. Its expectation leaves out the trend's stationary spread.
Under a log link and an AR(1) trend the factor is \\\exp(\sigma^2 /
(2(1 - \rho^2)))\\. `process_error = TRUE` gives the unbiased effect.
Its band is wider because it includes the uncertainty of the latent
state.

When an autoregressive process carries most of the series-level
variation, a covariate effect shows little under either setting.
[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md)
and
[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md)
return the state the model inferred.

## References

Arel-Bundock, V., Greifer, N. and Heiss, A. (2024). How to interpret
statistical models using marginaleffects for R and Python. *Journal of
Statistical Software*, 111(9):1-32.
[doi:10.18637/jss.v111.i09](https://doi.org/10.18637/jss.v111.i09)
