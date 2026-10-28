# Test posterior hypotheses on a fitted mvgam model

The `mvgam` method of
[`brms::hypothesis()`](https://paulbuerkner.com/brms/reference/hypothesis.brmsfit.html),
with the arguments and parameter names of
[`brms::hypothesis.brmsfit()`](https://paulbuerkner.com/brms/reference/hypothesis.brmsfit.html).
By default a hypothesis names population-level coefficients without
their `b_` prefix, as `"x > 0"` tests `b_x`. `class = NULL` names any
parameter by the full name
[`variables.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/index-mvgam.md)
lists, such as `ar1_trend[series_1]` or `sigma`. That help page sets out
how the names are formed.

## Usage

``` r
# S3 method for class 'mvgam'
hypothesis(
  x,
  hypothesis,
  class = "b",
  group = "",
  alpha = 0.05,
  robust = FALSE,
  seed = NULL,
  ...
)
```

## Arguments

- x:

  A fitted `mvgam` object.

- hypothesis:

  Character vector of hypothesis strings in the
  [`brms::hypothesis()`](https://paulbuerkner.com/brms/reference/hypothesis.brmsfit.html)
  grammar, such as `"x > 0"` or `"(x - z) = 0"`.

- class:

  The class prefix the names in `hypothesis` omit. Defaults to `"b"`.
  `NULL` or `""` takes every parameter by its full name.

- group:

  The grouping factor of a group-level class, as
  `class = "sd", group = "site"` tests `sd_site__Intercept` as
  `Intercept`.

- alpha:

  One minus the credible-interval mass. Defaults to `0.05` (90%-CI for
  one-sided, 95%-CI for two-sided).

- robust:

  Logical. If `FALSE` (the default), uses mean and standard deviation
  for summaries. If `TRUE`, uses median and median absolute deviation
  (MAD).

- seed:

  Optional integer seed for the random draws of an evidence ratio. The
  session's random state is restored when the call returns.

- ...:

  Forwarded to
  [`brms::hypothesis`](https://paulbuerkner.com/brms/reference/hypothesis.brmsfit.html).

## Value

A `brmshypothesis` object. See
[`brms::hypothesis()`](https://paulbuerkner.com/brms/reference/hypothesis.brmsfit.html).

## See also

[`brms::hypothesis()`](https://paulbuerkner.com/brms/reference/hypothesis.brmsfit.html),
[`variables.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/index-mvgam.md).

Other posterior summaries:
[`VarCorr.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/VarCorr.mvgam.md),
[`bibtex()`](https://nicholasjclark.github.io/mvgam/dev/reference/bibtex.md),
[`how_to_cite.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/how_to_cite.mvgam.md),
[`index-mvgam`](https://nicholasjclark.github.io/mvgam/dev/reference/index-mvgam.md),
[`mcmc_plot.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mcmc_plot.mvgam.md),
[`methods_md()`](https://nicholasjclark.github.io/mvgam/dev/reference/methods_md.md),
[`mvgam_draws`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_draws.md),
[`pairs.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/pairs.mvgam.md),
[`posterior_interval.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_interval.mvgam.md),
[`ranef.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/ranef.mvgam.md),
[`smooths.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/smooths.mvgam.md),
[`summary.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam.md)

## Author

Nicholas J Clark

## Examples

``` r
# \dontrun{
set.seed(13)
simdat <- sim_mvgam(family = poisson(), n_series = 1L,
                     n_timepoints = 120L, trend_model = AR())

mod <- mvgam(y ~ x,
              trend_formula = ~ AR(p = 1),
              data    = simdat$data_train,
              family  = poisson(),
              chains  = 2, silent = 2)

# A population-level coefficient, named as fixef() names it.
hypothesis(mod, "x > 0")
#> Hypothesis Tests for class :
#>   Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
#> 1    (x) > 0    -0.35      0.07    -0.48    -0.23          0         0     
#> ---
#> 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
#> '*': For one-sided hypotheses, the posterior probability exceeds 95%;
#> for two-sided hypotheses, the value tested against lies outside the 95%-CI.
#> Posterior probabilities of point hypotheses assume equal prior probabilities.

# Any parameter by its full name.
hypothesis(mod, c("ar1_trend[series_1] > 0",
                  "sigma_trend[series_1] < 1"),
           class = NULL)
#> Hypothesis Tests for class :
#>                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
#> 1 (ar1_trend[series... > 0     0.41      0.15     0.17     0.66        499
#> 2 (sigma_trend[seri... < 0    -0.36      0.08    -0.48    -0.22        Inf
#>   Post.Prob Star
#> 1         1    *
#> 2         1    *
#> ---
#> 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
#> '*': For one-sided hypotheses, the posterior probability exceeds 95%;
#> for two-sided hypotheses, the value tested against lies outside the 95%-CI.
#> Posterior probabilities of point hypotheses assume equal prior probabilities.
# }
```
