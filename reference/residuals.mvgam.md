# Posterior draws of residuals from mvgam models

Posterior draws of the residuals of a fitted `mvgam` model, of two
types:

- `"quantile"` (default): randomised quantile residuals (Dunn and Smyth
  1996), also called Dunn-Smyth or PIT residuals. Each observation
  passes through the distribution function of its predictive
  distribution, then through the standard normal quantile function.
  Under a correctly specified model the residuals are independent
  N(0, 1) draws for any response family.

- `"ordinary"`: the predictive error, `y` minus a
  [`posterior_predict.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/posterior_predict.mvgam.md)
  draw, per draw.

In sample, both types compare each observation with predictions that
condition on the latent trend state the model inferred at that time, the
state
[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/hindcast.mvgam.md)
returns. For `newdata` the fit has no such state. The predictions then
use the covariate part of the model, without the trend's innovations.
Passing `process_error = TRUE` adds them.
[`pp_check.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/pp_check.mvgam.md)
and
[`predictive_error.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/predictive_error.mvgam.md)
use the same predictions.

Pearson residuals are not supported. A Pearson residual of a count or a
bounded response is skewed and discrete even under a correctly specified
model.

## Usage

``` r
# S3 method for class 'mvgam'
residuals(
  object,
  newdata = NULL,
  type = c("quantile", "ordinary"),
  ndraws = NULL,
  draw_ids = NULL,
  summary = TRUE,
  robust = FALSE,
  probs = c(0.025, 0.975),
  ...
)
```

## Arguments

- object:

  A fitted `mvgam` object.

- newdata:

  Optional `data.frame` to compute residuals on. Defaults to the
  training data.

- type:

  Character. Either `"quantile"` (default) or `"ordinary"`.

- ndraws:

  Positive integer specifying number of posterior draws to use. NULL
  (default) uses all available draws.

- draw_ids:

  Optional integer vector selecting a subset of posterior draw indices
  to use. NULL (default) uses all draws (subject to `ndraws`).

- summary:

  Logical. `TRUE` (the default) returns a summary per observation.
  `FALSE` returns the residual draws.

- robust:

  Logical. If `FALSE` (the default), uses mean and standard deviation
  for summaries. If `TRUE`, uses median and median absolute deviation
  (MAD).

- probs:

  Two probabilities for the lower and upper quantile columns of the
  summary. Default `c(0.025, 0.975)`, a 95% interval.

- ...:

  Further arguments of
  [`posterior_predict.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/posterior_predict.mvgam.md),
  such as `resp` or `re_formula`.

## Value

With `summary = FALSE`, an `[ndraws, nobs]` matrix of residual draws.
With `summary = TRUE`, an `[nobs, 2 + length(probs)]` matrix. Its
columns are `Estimate` (the mean, or the median with `robust = TRUE`),
`Est.Error` (the standard deviation, or the MAD) and one quantile per
`probs`.

## Details

Each draw places an observation in a PIT interval `[lower, upper]`. The
residual is `qnorm(u)` for a `u` drawn uniformly from that interval. Two
routes give the interval.

The analytic route evaluates the family's distribution function at each
draw's parameters. A discrete family gives the interval of the observed
atom, `[F(y - 1), F(y)]`. A continuous family gives the point `F(y)`. A
[`tweedie()`](https://nicholasjclark.github.io/mvgam/reference/tweedie.md)
zero gives `[0, F(0)]`. Censored rows span their censoring interval. A
truncated model renormalises over its truncation bounds.
[`log_lik.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/log_lik.mvgam.md)
takes its density from the same parameterisation. The route covers
`gaussian`, `student`, `asym_laplace`, `lognormal`, `gamma`, `weibull`,
`exponential`, `beta`, `bernoulli`, `binomial`, `beta_binomial`,
`poisson`, `negbinomial`, `geometric`,
[`com_binomial()`](https://nicholasjclark.github.io/mvgam/reference/com_binomial.md),
[`beta_nb()`](https://nicholasjclark.github.io/mvgam/reference/beta_nb.md),
[`tweedie()`](https://nicholasjclark.github.io/mvgam/reference/tweedie.md),
the ordinal families, and the multivariate
[`mvn()`](https://nicholasjclark.github.io/mvgam/reference/mvn.md),
[`mvt()`](https://nicholasjclark.github.io/mvgam/reference/mvt.md),
[`diri()`](https://nicholasjclark.github.io/mvgam/reference/diri.md),
[`multi()`](https://nicholasjclark.github.io/mvgam/reference/multi.md)
and
[`categ()`](https://nicholasjclark.github.io/mvgam/reference/categ.md).
A composition family uses the marginal of one component: a Beta for
[`diri()`](https://nicholasjclark.github.io/mvgam/reference/diri.md), a
Binomial against the site's own total for
[`multi()`](https://nicholasjclark.github.io/mvgam/reference/multi.md)
and a Bernoulli for
[`categ()`](https://nicholasjclark.github.io/mvgam/reference/categ.md).

Every other family takes the empirical route, among them the
zero-inflated, hurdle and mixture families,
[`occ()`](https://nicholasjclark.github.io/mvgam/reference/occ.md) and
[`nmix()`](https://nicholasjclark.github.io/mvgam/reference/nmix.md).
Under the model the observation and the `n` pooled
[`posterior_predict.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/posterior_predict.mvgam.md)
draws are exchangeable. The rank of the observation among those `n + 1`
values, with ties broken at random, is then uniform. The bounds are
`sum(yrep < y) / (n + 1)` and `(sum(yrep <= y) + 1) / (n + 1)`. An
observation beyond every draw takes the top interval,
`[n / (n + 1), 1]`. The pooled predictive carries the posterior
uncertainty in the latent state. Its residuals are narrower than N(0,
1). A QQ-plot of them is conservative.

The PIT value is clamped to `[eps, 1 - eps]`, with
`eps = .Machine$double.eps`, to keep the residual finite. The clamp
bounds a residual at about +/-8.13. On the analytic route one draw can
place an observation beyond that bound. Such isolated values describe
that draw. A misfit shifts the residual across most draws.

## Multivariate families ([`mvn()`](https://nicholasjclark.github.io/mvgam/reference/mvn.md), [`mvt()`](https://nicholasjclark.github.io/mvgam/reference/mvt.md), [`diri()`](https://nicholasjclark.github.io/mvgam/reference/diri.md))

Each row of a unit gets its own residual. Residuals of
[`mvn()`](https://nicholasjclark.github.io/mvgam/reference/mvn.md) and
[`mvt()`](https://nicholasjclark.github.io/mvgam/reference/mvt.md)
condition on the latent factor scores and test the observation model.
They stay N(0, 1) when the fit has too few factors.
[`active_factors()`](https://nicholasjclark.github.io/mvgam/reference/active_factors.md)
and
[`residual_cor.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/residual_cor.md)
judge the number of factors.

A [`diri()`](https://nicholasjclark.github.io/mvgam/reference/diri.md)
residual is the exact marginal of one component,
`Beta(alpha_j, alpha_0 - alpha_j)`. Each is N(0, 1) on its own. Because
the shares of a unit sum to one, the residuals of a unit of `K` species
are negatively dependent, with `K - 1` degrees of freedom. With `K = 2`
the two residuals are exactly antithetic. Judge a QQ-plot or an ACF at
the unit grain.

## Closure-unit families ([`nmix()`](https://nicholasjclark.github.io/mvgam/reference/nmix.md), [`occ()`](https://nicholasjclark.github.io/mvgam/reference/occ.md))

Visits within a closure unit (site by season) share the latent state,
`N` for
[`nmix()`](https://nicholasjclark.github.io/mvgam/reference/nmix.md) and
`z` for
[`occ()`](https://nicholasjclark.github.io/mvgam/reference/occ.md). The
visits are independent given that state, but dependent marginally.
Because residuals per visit would carry that dependence into every
diagnostic, [`residuals()`](https://rdrr.io/r/stats/residuals.html)
returns one residual per closure unit. The observed counts or detections
are summed within each unit. The visit-level
[`posterior_predict.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/posterior_predict.mvgam.md)
draws are summed the same way. Columns (`summary = FALSE`) or rows
(`summary = TRUE`) are labelled by the unit's grouping values joined
with `_`.

Each draw simulates the latent state from `lambda` or `psi` without
conditioning on the unit's detection history. The residuals then test
the model across the population of units.
`predict(type = "latent_state")` gives the state given each unit's
detection history.

Under [`occ()`](https://nicholasjclark.github.io/mvgam/reference/occ.md)
a unit sum takes values in `0, ..., n_rep`. With few visits the
empirical PIT has coarse support. QQ-plot tails then look heavier than
N(0, 1) under a correctly specified model. A once-per-session warning is
raised when any unit has 4 or fewer visits. Compare QQ-plots with
envelopes simulated from N(0, 1) in that case.

## References

Dunn, P. K. and Smyth, G. K. (1996). Randomized quantile residuals.
*Journal of Computational and Graphical Statistics*, 5(3), 236-244.

## See also

[`augment.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/augment.mvgam.md),
[`posterior_predict.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/posterior_predict.mvgam.md),
[`pp_check.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/pp_check.mvgam.md)

Other model checking:
[`mvgam_diagnostics`](https://nicholasjclark.github.io/mvgam/reference/mvgam_diagnostics.md),
[`plot.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/plot.mvgam.md),
[`pp_check.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/pp_check.mvgam.md)

## Author

Nicholas J Clark

## Examples

``` r
# \dontrun{
set.seed(13)
simdat <- sim_mvgam(family = poisson(), n_series = 1L,
                    n_timepoints = 120L, trend_model = AR())
mod <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
             data = simdat$data_train, family = poisson(),
             chains = 2, silent = 2)
#> Warning: 8 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# Quantile residuals summarised per observation
r <- residuals(mod)
head(r)
#>         Estimate Est.Error       Q2.5      Q97.5
#> [1,]  0.04840697 0.6264913 -1.2157560  1.2922130
#> [2,]  0.44370997 0.8772811 -1.3087376  2.0783713
#> [3,]  0.09107709 0.7799975 -1.5336390  1.5901343
#> [4,] -1.66416080 0.6160145 -2.8894128 -0.4715048
#> [5,]  0.75776088 0.7458402 -0.7902042  2.1528948
#> [6,]  0.07961790 0.7294528 -1.3844870  1.4286155

# One draw of the residuals should look like N(0, 1) draws
r1 <- residuals(mod, summary = FALSE, draw_ids = 1)
qqnorm(r1[1, ])
qqline(r1[1, ])


# Next: plot(mod, type = "residuals") draws the QQ-plot, ACF and
# residuals against fitted values over many draws. A series observed
# at unevenly spaced times gets a semivariogram in place of the ACF
# }
```
