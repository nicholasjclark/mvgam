# Observation families supported by mvgam

[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
and
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
accept the observation families that brms and stats define. mvgam adds
families for data that ecological and epidemiological series tend to
produce. They cover counts observed with imperfect detection,
compositions and counts whose dispersion a negative binomial cannot
reach.

Pass a family as you would to
[`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html) or
[`stats::glm()`](https://rdrr.io/r/stats/glm.html), for example
`mvgam(y ~ x, trend_formula = ~ AR(p = 1), family = nmix())`. The
[N-mixture
article](https://nicholasjclark.github.io/mvgam/dev/articles/nmix.html)
fits
[`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md).
The [JSDM
article](https://nicholasjclark.github.io/mvgam/dev/articles/jsdgam.html)
fits a joint species distribution model.

## Families mvgam adds

Each fixes its link.
[`com_binomial()`](https://nicholasjclark.github.io/mvgam/dev/reference/com_binomial.md)
takes a `link` argument that accepts `"logit"` alone. The table lists
every argument.

|  |  |  |
|----|----|----|
| **Family** | **Link** | **Arguments** |
| [`tweedie()`](https://nicholasjclark.github.io/mvgam/dev/reference/tweedie.md) | log | `M`, the truncation point of the compound Poisson sum |
| [`beta_nb()`](https://nicholasjclark.github.io/mvgam/dev/reference/beta_nb.md) | log | none |
| [`com_binomial()`](https://nicholasjclark.github.io/mvgam/dev/reference/com_binomial.md) | logit | `link`, `"logit"` only |
| [`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md) | log (state), logit (detection) | `type`, one of `"poisson_binomial"`, `"royle_nichols"` or `"poisson_poisson"`; `multi_season` |
| [`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md) | logit (state and detection) | `multi_season` |
| [`diri()`](https://nicholasjclark.github.io/mvgam/dev/reference/diri.md) | softmax over each unit's categories | none |
| [`multi()`](https://nicholasjclark.github.io/mvgam/dev/reference/multi.md) | softmax over each unit's categories | none |
| [`categ()`](https://nicholasjclark.github.io/mvgam/dev/reference/categ.md) | softmax over each unit's categories | none |
| [`mvn()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvn.md) | identity | none |
| [`mvt()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvt.md) | identity | none |

## Choosing by the shape of the response

- Real-valued:

  [`stats::gaussian()`](https://rdrr.io/r/stats/family.html),
  [`brms::student()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  [`brms::skew_normal()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  [`brms::exgaussian()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  [`brms::asym_laplace()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
  and
  [`brms::von_mises()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
  for angles.

- Non-negative real-valued:

  [`stats::Gamma()`](https://rdrr.io/r/stats/family.html),
  [`brms::lognormal()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  [`brms::exponential()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  [`brms::weibull()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  [`brms::frechet()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
  and
  [`stats::inverse.gaussian()`](https://rdrr.io/r/stats/family.html).
  [`tweedie()`](https://nicholasjclark.github.io/mvgam/dev/reference/tweedie.md)
  takes continuous data with a point mass at zero. With a trend, give
  these families `link = "log"`. Their mean must stay positive. Under
  the `identity`, `inverse` or `1/mu^2` link it stays positive only
  where the linear predictor is positive on every row. A latent trend is
  centred near zero and takes negative values, where the likelihood is
  undefined. [`stats::Gamma()`](https://rdrr.io/r/stats/family.html)
  defaults to the `inverse` link.
  [`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
  warns when a trend meets one of these links.
  [`lognormal()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
  is the exception. Its `mu` is a log-scale location that takes any
  sign. `identity` is the right link for it.

- A single proportion on `(0, 1)`:

  [`Beta()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  with
  [`brms::zero_inflated_beta()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
  or
  [`brms::zero_one_inflated_beta()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
  when the bounds are attainable.

- Compositions and choices among categories:

  [`diri()`](https://nicholasjclark.github.io/mvgam/dev/reference/diri.md)
  for proportions summing to one,
  [`multi()`](https://nicholasjclark.github.io/mvgam/dev/reference/multi.md)
  for counts across categories and
  [`categ()`](https://nicholasjclark.github.io/mvgam/dev/reference/categ.md)
  for one chosen category per unit.

- Binary:

  [`brms::bernoulli()`](https://paulbuerkner.com/brms/reference/brmsfamily.html).

- Counts out of a known number of trials:

  [`stats::binomial()`](https://rdrr.io/r/stats/family.html).
  [`brms::beta_binomial()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
  takes overdispersed trials.
  [`com_binomial()`](https://nicholasjclark.github.io/mvgam/dev/reference/com_binomial.md)
  takes trials that are under- or over-dispersed.

- Unbounded counts:

  [`stats::poisson()`](https://rdrr.io/r/stats/family.html),
  [`brms::negbinomial()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  [`brms::geometric()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
  and their zero-inflated and hurdle counterparts (see
  [`brms::brmsfamily()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)).
  [`beta_nb()`](https://nicholasjclark.github.io/mvgam/dev/reference/beta_nb.md)
  takes tails heavier than a negative binomial reaches.

- Repeat visits to a site:

  [`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md)
  estimates latent abundance from repeated counts.
  [`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md)
  estimates latent occupancy from repeated detection histories.

- Several responses measured together:

  [`mvn()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvn.md)
  and
  [`mvt()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvt.md),
  whose residual correlations come from a latent factor structure.

- Ordered:

  [`brms::cumulative()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  [`brms::sratio()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  [`brms::cratio()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
  and
  [`brms::acat()`](https://paulbuerkner.com/brms/reference/brmsfamily.html).

## Link functions

A brms or stats family takes a `link` argument as usual.
`poisson(link = "sqrt")` and `Gamma(link = "log")` both work. The
families mvgam adds each have the one link the table gives.

## Data layout

[`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md)
and
[`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md)
are closure-unit families. A closure unit is a site, or a site in one
season, whose true occupancy or abundance holds across its repeat
visits. The families expect several rows per closure unit, one per
visit. They model the state and the detection process with separate
linear predictors.
[`bf()`](https://paulbuerkner.com/brms/reference/brmsformula.html)
supplies the second, as in `bf(y ~ elev, p ~ tod)`.
[`sim_closure_unit_data()`](https://nicholasjclark.github.io/mvgam/dev/reference/sim_closure_unit_data.md)
simulates the layout they take.
[`pivot_detection_array()`](https://nicholasjclark.github.io/mvgam/dev/reference/pivot_detection_array.md)
reshapes a site-by-visit array into it.
[`diri()`](https://nicholasjclark.github.io/mvgam/dev/reference/diri.md),
[`multi()`](https://nicholasjclark.github.io/mvgam/dev/reference/multi.md),
[`categ()`](https://nicholasjclark.github.io/mvgam/dev/reference/categ.md),
[`mvn()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvn.md)
and
[`mvt()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvt.md)
also take several rows per unit, one per category or response.

## What the family changes after fitting

A closure-unit family adds `predict(type = "latent_state")` and
`predict(type = "detection")`. It supports
`pp_check(type = "fit_stat")`. It refuses the
[`pp_check()`](https://nicholasjclark.github.io/mvgam/dev/reference/pp_check.mvgam.md)
types that assume one observation per row.

[`brms::dirichlet()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
[`brms::multinomial()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
and
[`brms::categorical()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
are refused with a pointer to
[`diri()`](https://nicholasjclark.github.io/mvgam/dev/reference/diri.md),
[`multi()`](https://nicholasjclark.github.io/mvgam/dev/reference/multi.md)
and
[`categ()`](https://nicholasjclark.github.io/mvgam/dev/reference/categ.md).
These fit the same responses in mvgam's long-format layout.
[`brms::logistic_normal()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
is refused with a pointer to
[`mvn()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvn.md),
which fits the additive log-ratios of the proportions.

## See also

[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md),
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md),
[`brms::brmsfamily()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
[`get_prior()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.md),
[`posterior_predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_predict.mvgam.md),
[`sim_closure_unit_data()`](https://nicholasjclark.github.io/mvgam/dev/reference/sim_closure_unit_data.md)

Other observation families:
[`beta_nb()`](https://nicholasjclark.github.io/mvgam/dev/reference/beta_nb.md),
[`categ()`](https://nicholasjclark.github.io/mvgam/dev/reference/categ.md),
[`com_binomial()`](https://nicholasjclark.github.io/mvgam/dev/reference/com_binomial.md),
[`diri()`](https://nicholasjclark.github.io/mvgam/dev/reference/diri.md),
[`multi()`](https://nicholasjclark.github.io/mvgam/dev/reference/multi.md),
[`mvn()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvn.md),
[`mvt()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvt.md),
[`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md),
[`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md),
[`tweedie()`](https://nicholasjclark.github.io/mvgam/dev/reference/tweedie.md)

## Examples

``` r
# Families are constructed, then passed to `family`
nmix(type = "royle_nichols")
#> 
#> Custom family: nmix_royle_nichols 
#> Link function: log 
#> Parameters: mu, p 
#> 
com_binomial()
#> 
#> Custom family: com_binomial 
#> Link function: logit 
#> Parameters: mu, nu 
#> 
tweedie(M = 40L)
#> 
#> Custom family: tweedie 
#> Link function: log 
#> Parameters: mu, mphi, mtheta 
#> 

# A brms or stats family still takes its own link
poisson(link = "sqrt")
#> 
#> Family: poisson 
#> Link function: sqrt 
#> 

# The layout occ() takes: one row per visit to each site
head(sim_closure_unit_data(family = occ())$data_train)
#>   visit site series time y cap        env      tod_c
#> 1     1    1   sp_1    1 1   1 -0.2228471  4.0020204
#> 2     2    1   sp_1    1 0   1 -0.2228471  4.6148115
#> 3     3    1   sp_1    1 1   1 -0.2228471  4.3712082
#> 4     4    1   sp_1    1 0   1 -0.2228471 -0.8113259
#> 5     1    2   sp_1    2 1   1 -0.3107994  2.2083795
#> 6     2    2   sp_1    2 0   1 -0.3107994  2.8288534

# Next, get_prior() lists the priors a family's parameters take
```
