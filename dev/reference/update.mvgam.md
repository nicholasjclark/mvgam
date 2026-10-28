# Update a fitted mvgam model

Refit a fitted `mvgam` object with new data, an updated formula, new
priors, different sampler settings or any other argument
[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
accepts. The signature follows
[`brms::update.brmsfit()`](https://paulbuerkner.com/brms/reference/update.brmsfit.html).

## Usage

``` r
# S3 method for class 'mvgam'
update(object, formula. = NULL, newdata = NULL, recompile = NULL, ...)
```

## Arguments

- object:

  A fitted `mvgam` object.

- formula.:

  A new observation formula, or an update to the fitted one such as
  `~ . + new_term`. `NULL` (the default) keeps `object$formula`.
  [`stats::update.formula()`](https://rdrr.io/r/stats/update.formula.html)
  applies the update. `~ . + z`, `~ . - z`, `y2 ~ x` and `. ~ .` all
  work.

- newdata:

  An optional new training data frame. `NULL` (the default) keeps
  `object$data`. A `data` argument in `...` is an error.

- recompile:

  Optional logical. `NULL` (the default) and `TRUE` refit through
  [`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md).
  Both backends cache compiled models by their Stan code. A refit with
  unchanged code skips compilation. `FALSE` builds the new Stan code
  first and compares it with `object$stancode`. A difference is an
  error. A refit that passes the check compiles nothing.

- ...:

  Any other argument
  [`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
  accepts, or
  [`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
  for a `jsdgam` fit (e.g. `trend_formula`, `family`, `prior`, `chains`,
  `iter`, `warmup`, `cores`, `threads`, `algorithm`, `backend`,
  `silent`, `seed`, `init`, `control`). Each value overrides the one
  inherited from `object`. A `prior` joins the priors the original fit
  set, as Details describes.

## Value

A refit `mvgam` object.

## Details

Every
[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
argument comes from `object` unless `...` overrides it.
`update(fit, iter = 4000)` keeps the original formula, data, family,
trend, prior and backend. Only the iteration count changes.

Sampler settings follow
[`brms::update.brmsfit()`](https://paulbuerkner.com/brms/reference/update.brmsfit.html).
`chains`, `iter`, `warmup`, `thin` and `control` carry over when the
refit keeps the original `algorithm`. `control` also needs the original
`backend`. A new `iter` without a `warmup` takes the default warmup of
`iter / 2`. `control` merges entry by entry:
`update(fit, adapt_delta = 0.99)` keeps the original `max_treedepth`.

New training data passed as `newdata` keeps the fit's held-out data when
that lies after the new training times. Held-out data overlapping them
is dropped with a message.

A refit reuses the fitted prior table. The adaptive priors of brms, such
as the Intercept prior centred on the response, keep the constants of
the original fit. Because the constants stay fixed, a refit on a
different `newdata` window has the same Stan code and reuses the
compiled model.
[`lfo_cv()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md)
and other workflows that refit many times then repeat only the sampling.
An inherited row for a parameter the refit lacks, such as the smooth of
a dropped term, is left out of the refit. A `prior` passed to
[`update()`](https://rdrr.io/r/stats/update.html) joins the priors the
user set on the original fit, as in
[`brms::update.brmsfit()`](https://paulbuerkner.com/brms/reference/update.brmsfit.html).
A new row replaces an old one for the same parameter:

    new_priors <- c(
      prior(std_normal(), class = "b"),
      prior(exponential(1), class = "sigma")
    )
    refit <- update(mod, prior = new_priors)

A prior change that alters the Stan code recompiles the model. With
`recompile = FALSE` it is an error. `stancode(refit)` shows the model
the refit compiled.

Multivariate fits take a complete new formula as `formula.`. A formula
update written with `.` is refused, as brms refuses it.

A
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
fit refits through
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
with the arguments the original call evaluated, including
`factor_formula`, `n_lv`, `traits`, `trait_slopes` and `phylo`. `...`
overrides any of them.

[`update()`](https://rdrr.io/r/stats/update.html) refuses
multiple-imputation fits from `mvgam_multiple(combine = TRUE)`. Refit
each imputation with
[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
and pool them again.

## See also

[`loo.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo.mvgam.md)
and
[`loo_compare.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo.mvgam.md)
compare a refit with the original.
[`stancode.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/stancode.mvgam_formula.md)
shows the compiled model. The [forecast evaluation
article](https://nicholasjclark.github.io/mvgam/dev/articles/forecast_evaluation.html)
compares competing models by their forecasts.

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
[`stancode.mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/stancode.mvgam_formula.md)

## Author

Nicholas J Clark

## Examples

``` r
# \dontrun{
# A Poisson GAM without a latent trend. PSIS approximates each
# held-out count of such a model well (see Details of ?loo.mvgam)
set.seed(13)
simdat <- sim_mvgam(family = poisson(), n_series = 1L,
                    n_timepoints = 120L, trend_model = "None")
mod <- mvgam(y ~ s(x), data = simdat$data_train,
             family = poisson(), chains = 2, silent = 2)
#> Warning: 3 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
# }
# \dontrun{
# Swap the family for a negative binomial. The formula, priors, data
# and sampler settings carry over
upd <- update(mod, family = negbinomial())
#> Warning: 17 of 2000 (1.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# Compare the two fits by approximate leave-one-out cross-validation.
# The better model comes first, with elpd_diff = 0. An elpd_diff
# smaller than 4 in size means the two predict about equally well.
# p_worse is the probability that a model predicts worse than the best
loo_compare(mod, upd)
#> Warning: Some Pareto k diagnostic values are too high. See help('pareto-k-diagnostic') for details.
#>  model elpd_diff se_diff p_worse diag_diff      diag_elpd
#>    mod       0.0     0.0      NA                         
#>    upd      -0.7     0.7    0.82   N < 100 1 k_psis > 0.7
#> 
#> Diagnostic flags present.
#> See ?`loo-glossary` (sections `diag_diff` and `diag_elpd`)
#> or https://mc-stan.org/loo/reference/loo-glossary.html.

# A change of sampler settings alone reuses the compiled model
longer <- update(mod, iter = 3000, recompile = FALSE)
#> Warning: 50 of 3000 (2.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
# }
```
