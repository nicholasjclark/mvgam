# The priors of a model

Methods:

- [`get_prior.mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.mvgam_formula.md)
  lists the priors of a model before it is fitted.

- [`get_prior.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.mvgam.md)
  returns the prior table of a model.

- [`get_prior.default()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.default.md)
  takes a `formula` or `brmsformula` with the arguments of
  [`get_prior.mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.mvgam_formula.md).

`default_prior()` is the same generic under the newer brms name.

A prior states the plausible values of a parameter before the data are
seen. Each row of the table gives the prior of one parameter class. A
`_trend` suffix marks the classes of the latent trend. The defaults are
weakly informative. Change one when outside knowledge narrows a
parameter, or when the sampler struggles in a region the data cannot
inform. Pass the new rows, built with
[`brms::prior()`](https://paulbuerkner.com/brms/reference/set_prior.html),
to the `prior` argument of
[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md).

## Usage

``` r
get_prior(object, ...)

default_prior(object, ...)
```

## Arguments

- object:

  An
  [`mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_formula.md),
  an `mvgam` model, or a model formula.

- ...:

  Arguments of the method.

## Value

A `brmsprior` data frame.

## See also

[`mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_formula.md),
[`brms::get_prior()`](https://paulbuerkner.com/brms/reference/default_prior.html)

## Examples

``` r
# \dontrun{
set.seed(13)
simdat <- sim_mvgam(family = poisson(), n_series = 1L,
                    n_timepoints = 120L, trend_model = AR())

# The default priors of a Poisson GAM with an AR(1) trend, before
# any fitting
get_prior(
  mvgam_formula(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1)),
  data = simdat$data_train,
  family = poisson()
)
#>                   prior       class        coef group resp dpar nlpar lb ub tag
#>  student_t(3, 1.6, 2.5)   Intercept                                            
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

# Outside knowledge that the series is strongly persistent narrows
# the AR(1) coefficient
persistent <- prior(normal(0.7, 0.15), class = ar1_trend)
mod <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
             data = simdat$data_train, family = poisson(),
             prior = persistent, chains = 2, silent = 2)
#> Warning: 15 of 2000 (1.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# The fitted model's prior table records the change
get_prior(mod)
#>                   prior       class        coef group resp dpar nlpar lb ub tag
#>  student_t(3, 1.6, 2.5)   Intercept                                            
#>                  (flat)           b                                            
#>                  (flat)           b        sx_1                                
#>    student_t(3, 0, 2.5)         sds                                    0       
#>    student_t(3, 0, 2.5)         sds s(x, k = 5)                        0       
#>          exponential(2) sigma_trend                                    0       
#>       normal(0.7, 0.15)   ar1_trend                                   -1  1    
#>        source
#>       default
#>       default
#>  (vectorized)
#>       default
#>  (vectorized)
#>         mvgam
#>          user
# }
```
