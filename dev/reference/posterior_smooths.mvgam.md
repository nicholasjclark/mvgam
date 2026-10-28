# Posterior draws of one smooth term

Evaluates one smooth at the rows of `newdata` for each posterior draw,
in the shape
[`brms::posterior_smooths()`](https://paulbuerkner.com/brms/reference/posterior_smooths.brmsfit.html)
returns. The smooth can be any `s()` or `t2()` term of the observation
formula, a distributional or non-linear parameter's formula, or the
trend formula, `by` factors included.

## Usage

``` r
# S3 method for class 'mvgam'
posterior_smooths(
  object,
  smooth,
  newdata = NULL,
  resp = NULL,
  dpar = NULL,
  nlpar = NULL,
  ndraws = NULL,
  draw_ids = NULL,
  ...
)
```

## Arguments

- object:

  A fitted `mvgam` object.

- smooth:

  The smooth term as the formula writes it, such as `"s(x)"`,
  `"s(z, by = group)"` or `"t2(x, y)"`. Spacing is ignored.
  [`smooths.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/smooths.mvgam.md)
  lists the terms.

- newdata:

  A `data.frame` holding the smooth's own variables, or `NULL` for the
  training data.

- resp:

  The response the smooth belongs to, required on a model with several.

- dpar, nlpar:

  The distributional or non-linear parameter the smooth belongs to. Name
  at most one. With both `NULL` the smooth belongs to the mean.

- ndraws, draw_ids:

  The number of posterior draws, or their indices. Give at most one.
  `NULL` for both uses every draw.

- ...:

  Unused. Anything passed here is refused.

## Value

A numeric matrix with one row per draw and one column per row of
`newdata`: the smooth's contribution to the linear predictor.

## Details

The smooth's contribution is its unpenalised basis times its fixed
coefficients plus each penalised basis times its random coefficients,
per draw. The bases come from
[`brms::standata()`](https://paulbuerkner.com/brms/reference/standata.html)
evaluated on `newdata`. The coefficients come from the draws.
[`brms::posterior_smooths()`](https://paulbuerkner.com/brms/reference/posterior_smooths.brmsfit.html)
adds a predictor's offsets to each of its smooths. This method leaves
them out.

A term written in both the observation and the trend formula is taken
from the observation formula.

## See also

[`conditional_smooths.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_smooths.mvgam.md)
summarises every smooth.
[`smooths.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/smooths.mvgam.md)
lists the terms.

Other predictions:
[`conditional_effects.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_effects.mvgam.md),
[`conditional_smooths.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_smooths.mvgam.md),
[`fitted.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/fitted.mvgam.md),
[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md),
[`posterior_epred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_epred.mvgam.md),
[`posterior_linpred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_linpred.mvgam.md),
[`posterior_predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_predict.mvgam.md),
[`predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/predict.mvgam.md),
[`predictive_error.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/predictive_error.mvgam.md),
[`predictive_interval.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/predictive_interval.mvgam.md)

## Author

Nicholas J Clark

## Examples

``` r
# \dontrun{
set.seed(13)
simdat <- sim_mvgam(family = poisson(), n_series = 1L,
                     n_timepoints = 120L, trend_model = AR())
mod <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
              data    = simdat$data_train,
              family  = poisson(),
              chains  = 2, silent = 2)
#> Warning: 5 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# Twenty draws of s(x) over a grid of x
grid <- data.frame(x = seq(-2, 2, length.out = 50))
sm <- posterior_smooths(mod, smooth = "s(x, k = 5)", newdata = grid)
draws <- data.frame(x = rep(grid$x, each = 20),
                    value = as.vector(sm[1:20, ]),
                    draw = rep(1:20, times = 50))
library(ggplot2)
ggplot(draws, aes(x, value, group = draw)) +
  geom_line(colour = "#8F2727", alpha = 0.4) +
  labs(y = "s(x)") +
  mvgam_theme()


# Next, conditional_smooths(mod) summarises every smooth in one call
# }
```
