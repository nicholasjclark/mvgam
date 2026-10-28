# Testing mvgam against real fitted models

`tests/testthat/` checks that the package builds the right Stan
program. The files here test what the package does once a real
posterior exists, which covers most of it. Each of these needs a
fitted model before it can run at all:

- forecasting with `forecast()` and `hindcast()`
- every prediction route
- cross-validation with `loo()` and `lfo_cv()`
- impulse responses from `irf()`, with `fevd()` and `stability()`
- `residual_cor()`
- the tidiers, the plotting methods and `summary()`
- `conditional_effects()` and the hooks that insight and
  marginaleffects call

CI builds models without sampling them, which keeps it within CRAN's
time budget. These files run outside it.

Each file first checks parameter recovery. On a posterior that missed
the truth, a disagreement between two methods is no evidence against
either. Once recovery holds, each file uses the fit to check that two
routes to one quantity agree. It also checks that every method indexes
the axis the sampler was given, that an accepted argument was honoured
and that each refusal names something a user can act on.

## When to run this

Run these files occasionally:

- after a change to how a model is assembled, indexed, predicted from
  or reported on, once the unit tests pass, to confirm the fitted
  results have not moved
- before a CRAN submission, as the last check that nothing in the
  post-fit methods has gone wrong unnoticed

A cold run fits every model and takes hours. A second run loads the
cached fits and takes minutes.

## How to run it

One file:

```bash
TESTTHAT_MAX_FAILS=1000 Rscript -e \
  "devtools::load_all('.'); testthat::test_file('tests/local/test-trend-var.R')"
```

Every file, each writing its result to its own log. A truncated
terminal view loses the summary line that names each failure:

```bash
for f in tests/local/test-*.R; do
  TESTTHAT_MAX_FAILS=1000 Rscript -e \
    "devtools::load_all('.'); testthat::test_file('$f')" \
    > "/tmp/$(basename $f .R).log" 2>&1
done
```

`TESTTHAT_MAX_FAILS` is required here. While a defect is open, a file
here can fail many times. After ten failures testthat abandons a file,
leaving every block after the tenth unrun and unreported. testthat
sets the limit when it builds the reporter, before it sources a file.
A file cannot raise it from inside.

Fits cache under `tests/local/fixtures/`, which git ignores and the
first run creates. Delete a file there to refit that model. Delete the
directory to refit everything. There is no build step and no shared
fixture. Each file owns the models it fits and runs alone from a clean
clone. The `helper-*.R` files hold the ground truth that several files
share. Before each file here, testthat sources them.

## An assertion states a promise

Each assertion states what the documentation or the neighbouring
method promises. A failing one marks a defect, which is fixed in the
package. An assertion is never weakened to make a file pass.

## What each file fits, and why the model has to be fitted

Most defects found here had nothing to do with the trend that exposed
them: a method indexing the wrong axis, an argument accepted then
dropped, two routes to one number disagreeing. Given that, every file
drives the same broad battery of post-fit checks. The files differ in
the structure that makes a particular defect visible. Each entry below
describes that structure. A model earns its place when it exposes a
class of mistake the others cannot. Running one battery on every model
then tests something different each time.

### Trend kernels

**test-trend-var.R** fits one model: `VAR(cor = TRUE)` over three
gaussian series on sixty occasions, with `y ~ elev * region +
(1 | block)` on the observation side. A VAR differs from a bank of
independent AR(1)s only in the off-diagonal entries of the transition
matrix. A diagonal `A` fits comparably well. Only recovering `A` entry
by entry separates the two. The one-step forecast links `A` to what a
user receives. Because the forecast mixes the series, a recursion that
transposed `A` or ran each series alone gives a different forecast
that is still finite and correctly shaped.

**test-trend-ar-multilag.R** fits `AR(p = c(1, 3, 12))` on two gaussian
series over ninety-six occasions, with `gp(x)` on the observation side.
The vector requests lags 1, 3 and 12 alone. A model that took the
vector's maximum would emit twelve coefficients and fit at least as
well. Only a fitted posterior shows which lags exist.

Two smaller fits cover how AR coefficients are parameterised.
`AR(p = 2)` takes the contiguous case, where partial autocorrelations
are sampled and the coefficients derived, making every draw stationary
by construction. `AR(p = 1, coef_sharing = "shared")` samples one
coefficient and broadcasts it to both series. Two series that each
draw an independent coefficient from one narrow posterior give
matching estimates, dimensions and summaries. Only within a single
draw do the two models differ, where shared coefficients are
identical. That fit drives the prediction routes, the forecast
recursion, the hindcast blocks and the tidiers. The same section
checks that `get_prior()` names the parameter each sharing mode
samples across contiguous and sparse lag sets.

**test-trend-ar-correlated.R** fits `AR(p = 2, cor = TRUE)` on three
gaussian series over 150 occasions. Every other `AR()` fit here gives
each series an independent innovation, which leaves the trend
covariance diagonal. Combining an order above one with a correlation
between series makes the initial state a joint quantity. The first two
latent states come from the stationary covariance of the companion
state. No elementwise formula reproduces that covariance. The
coefficients are spread widely across the three series. That spread
separates the stationary cross-covariance `1 / (1 - ar_i * ar_j)` from
the geometric mean of each series' own factor by 0.099 in correlation
at the median. Ground truth is a Neumann sum written from the
definition, which leaves every assertion testing the package against
that identity. Three wrong implementations that keep every value
finite and every dimension right land between 0.73 and 0.90 away from
the draws: scaling the innovations by `Sigma` alone, the one-lag
closed form applied to two lags and the two states filled in the
opposite time order. The file first asserts that the cached fit ran
the program the package generates today, which turns a stale cache
into a failure.

**test-stationary-start-functions.R** fits nothing. It compiles every
start block an `AR()` trend emits, wrapped as functions and exposed to
R: the scalar closed forms at one lag, `ar_stationary_init()` for
independent series above one lag and the joint start over every
series, one group and each series alone, with and without a
moving-average term, together with `ar_diag_joint_var()`. Each start is
evaluated at every unit variate. The covariance that map implies for
the state at `t = p` must equal a Neumann sum over the full companion,
on random shapes of one to five series, a shape whose AR and MA
polynomials cancel and two near-unit-root shapes, the second at a
partial autocorrelation of 0.9999 where the factored covariance is
least well conditioned. Two checks show the comparison can fail. One
swaps the two series' companions, which transposes every cross-series
block. The other drops the moving-average term.

**test-trend-ar-stationary-start.R** fits six small gaussian models
over six series in two groups: `AR(p = 2)` grouped, `AR(p = 1,
ma = TRUE)` correlated and grouped, and `AR(p = 2, ma = TRUE)`
independent, correlated and grouped. The functions file checks the
distribution. This file checks the wiring around it: which innovation
rows fill which variates, which coefficients reach which series and
which series make up a group. Each fit rebuilds `lv_trend[1:p, ]` from
the draw's own parameters against a Neumann sum. A gate reverses each
group's member order and requires the rebuild to fail. The same fits
then drive the marginal covariance of each shape, a one-step forecast
against the draw's conditional mean and the hindcast's series keys.
They also drive `posterior_epred()` and the marginal
`posterior_predict()`. `print()`, the plots and `residual_cor()` run
on them too.

**test-trend-arma.R** fits `AR(p = 1, ma = TRUE)` on two gaussian
series over eighty occasions, under a two-dimensional `gp(x1, x2)`.
There is no `ARMA()` constructor. The moving-average term is an
argument that can be accepted and dropped. The file's claim is the
contrast between the same model with and without it. The term also
decides where the trend starts: a stationary ARMA(1, 1) has
`cov(lv_1, eps_1) = sigma^2`, which makes the conditional mean of
`lv_1` the innovation and the conditional standard deviation
`(ar1 + theta1) * sigma / sqrt(1 - ar1^2)`. This fit checks that
construction with independent innovations, draw by draw against a
ground truth written from the definition.

**test-trend-pw.R** fits `PW(n_changepoints = 8)` on two Poisson series
over sixty occasions, under an offset and a two-dimensional smooth. A
piecewise trend is defined by its changepoint locations and the rate
adjustment at each one. Those are stored in `t_change_trend` and
`delta_trend`. The fitted line alone does not identify them.

**test-trend-car-irregular.R** fits `CAR()` and `ZMVN()` over nine
Poisson series, on an irregular grid of twenty-six occasions with gaps
of one to four. `CAR()` is the one trend that uses the time axis as a
quantity: it raises its damping to the power of the gap. On a regular
grid the rank of an occasion equals its value. Only an irregular grid
exposes a derivation that counts occasions where it should measure the
gaps between them.

**test-trend-hierarchical.R** fits `AR(gr = region, subgr = species,
cor = TRUE)` over two regions of three Poisson species on ninety
occasions. Under grouping, mvgam derives the series identifier from
`gr` and `subgr`. That derived value has to agree with the levels
recorded at fit time. When it did not, every post-fit method failed at
the level validator while `summary()` kept working.

**test-trend-map.R** fits `AR(p = 1, trend_map = Z)` on four Poisson
series loading two latent factors through a known `Z`. On this route
alone the loadings reach Stan as data. Known loadings let the row
order of `Z` be checked against values. Every other factor fit can
check it against the declaration text alone. The identity
`trend[t, s] = Z[s, ] . lv_trend[t, ]` is verified here against the
`Z` the data supplied.

**test-trend-llt.R** fits five `LLT()` models on Poisson counts. Three
series take correlated shocks under a damped slope. Five series load
two factors. One series with an offset, still growing when its data
end, is fitted under a damped slope and again under an undamped one.
Four series in two regions take shocks that correlate with opposite
signs. A local linear trend holds two states, a level and its slope.
Only the level enters the summed trend. The recursion rebuilds the
stored states from the raw draws. The level and the slope that
`component` reports match the stored paths over the fitted times. Each
forecast lead, for the trend and for the slope, matches its closed
form. The damped pair tests the claim the trend exists for: a series
still growing when its data end is forecast to level off.

### Latent factor structure

**test-factor-lv-axis.R** fits `s(elev, by = lv_axis()) - 1 +
ZMVN(cor = TRUE)` over five gaussian species and sixty occasions, with
rows shuffled and unbalanced. `by = lv_axis()` moves the trend design
onto the latent-factor axis, which is the one case where the second
dimension of `times_trend` means something other than a series.

**test-factor-loadings-prior.R** fits four models under the Heaps and
Jermyn row covariance on the loadings: a two-cluster AR(1) model, a
thirty-species presence-absence model under mvgam's default prior and
again under the wider one the paper uses, then a continuous-response
model whose kernel is tilted so the phylogeny carries the row
structure. Whether a structured prior recovers the structure it was
given is a statement about a posterior. The program text cannot settle
it.

**test-factor-var.R** fits `VAR(cor = TRUE, n_lv = 2)` over four
gaussian series on a hundred occasions, under `y ~ x`. Every other VAR
in this directory gives the transition matrix the series axis, where
`n_lv` and `n_series` coincide and neither can be told from the other.
Four series loading two factors separate the axis the dynamics run on
from the axis that indexes the observations. That separation makes two
claims testable. First, `irf()` returns four shock pairs where a
series-axis VAR over four series returns sixteen, while
`residual_cor()` and the forecasts stay keyed by series. Second, a
latent process that no series names takes its index as a label. A
series-axis VAR never reaches that path. Because the loadings are
sampled here, both sides of `trend[t, s] = Z[s, ] . lv_trend[t, ]`
come from one iteration when the identity is checked.

### Observation families

**test-family-tweedie.R** fits `tweedie()` with an AR(1) trend over
sixty occasions at `p = 1.4`. Tweedie is the only exported family with
its own Stan code, attached to the family as
`attr(fam, "mvgam_stanvars")`. This fit runs that path end to end. The
claim is the mass at zero. The closed form, the fitted density and the
draws each reach it by a different route.

**test-family-mixture.R** fits three models: a hurdle Poisson with a
scalar hurdle, a hurdle Poisson with `hu ~ z` on a logit link and a
zero-inflated Poisson at a low rate. A hurdle density written with a
zero-inflated branch returns finite numbers of the right shape, yet it
is a different distribution. To catch that, each fit is checked
against its own closed form and required to fail the other's.

**test-family-com-binomial.R** fits one Conway-Maxwell binomial over
two series and fifty occasions, under

```r
bf(y | trials(n_trials) ~ s(x, k = 5) + series + (1 | site) + mo(dose),
   nu ~ z)
```

with an AR(1) trend. The family is a custom brms family with its own
Stan code and a second distributional parameter. Its denominator comes
through a `trials()` aterm.

The aterm is why the fit exists. `find_predictors()` treats a trial
count as a denominator and leaves it out of the term list. Three
routes build a frame from that list: the smooth grid, the
marginaleffects grid and the padded forecast grid. None of them has a
denominator to put in the frame. Each fails in its own way. A family
whose response needs no denominator reaches none of these failures.

Each remaining term drives a method that no other file here drives.
`mo(dose)` is the only monotonic effect in the directory. Its simplex
of increments and the monotone ordering it imposes are checked here
alone. `(1 | site)` is the only group-level effect outside the VAR
fit. `ranef()` refuses a fit without a grouping. So do `VarCorr()` and
`ngrps()`. This fit gives all three a grouping to report. `nu ~ z`
puts a sub-formula on the second distributional parameter of a custom
family. Every other distributional formula in the directory targets a
built-in family. The same fit also drives `predictive_error()` and
`how_to_cite()`, along with the interval methods
`predictive_interval()` and `posterior_interval()`. No other file
calls them.

**test-family-jsdgam.R** fits the same latent factor structure under
three observation families: negative binomial, multivariate Student-t
and Dirichlet. The checks every family shares run once per family.
Each family also gets its own checks: the dispersion, the tail and the
simplex a composition has to respect.

### Index grain

**test-grain-closure-units.R** fits seven models around the closure
unit, which is the `(series, time)` cell holding one latent state
while a visit is a row inside it. Twenty-five sites over three
occasions with four visits make units, series and rows three different
counts. A method that computes on the wrong grain cannot pass by
accident. The seven cover occupancy, abundance, a factor model over
each, a complete and a gappy visit schedule, then a multi-season fit
whose grouping runs over three axes.

**test-grain-mvbf-wide.R** fits one `mvbf()` model whose Poisson,
Bernoulli and gaussian responses share one AR trend over sixty
occasions. In a wide frame each occasion is one row and each response
one column. The series of an observation is set by the pair
`(row, response)`. A data-frame attribute cannot express that shape.
Treating the series as a frame attribute once produced a defect that
left every count, dimension and bound correct. With three families in
one fit, each family-level claim is also tested three times.

### Observation-side terms

**test-obs-empty-formula.R** fits `y ~ -1` twice, once with a trend
covariate and no intercept and once with `lv_axis()` per-series
intercepts. An empty observation formula reaches brms as a
placeholder column, `.mvgam_empty_obs`. The tests check what that
column adds to the linear predictor. They also check that the name
stays out of the frame, the term list and the summary. Every other
fixture here has an observation intercept, which bypasses the
placeholder.

**test-obs-trend-confounded.R** fits `y ~ 1` with
`~ lv_axis() + AR(p = 1)`, where the observation intercept and the
trend's series columns span one direction twice. mvgam names the
overlap and continues. The fit is the evidence for a notice in place
of a refusal. The model samples and every fitted value is sound. The
shared coefficient alone has no interpretation.

**test-obs-smooth-surfaces.R** fits one Poisson model with
`s(z, by = grp) + t2(z, w) + gp(w, by = cat)`. Three smooth shapes in
one model make the claim possible. `smooths()` has to pick the right
term out of three. So do `posterior_smooths()` and
`conditional_smooths()`. A method that returned the first term
whatever it was asked for passes on any single-smooth model.

### The method surface

**test-backend-fits.R** fits small Poisson AR models through rstan,
through cmdstanr's sampler and through each of its approximate
algorithms. It also fits a pooled model over two imputations. The
claims are that sampler arguments reach the backend and that every
algorithm returns a posterior the post-fit methods take. A pooled fit
must also report its convergence per imputation.

**test-dots-refusal.R** tests every `mvgam` method in the package's S3
registry for one property: a method that names every argument it uses
must refuse one it does not. A misspelt argument lands in `...` and
leaves its default in force. Because the list comes from the
registry, a method added without the guard fails here.

**test-linpred-parity.R** fits seven models that hold every predictor
term type under every brms suffix: a distributional parameter, a
response of an `mvbf()` model and a non-linear parameter. Each linear
predictor mvgam composes from the Stan data is held against the one
brms computes from the same draws.

**test-newdata-guards.R** fits one model with a single series and one
with several. A prediction frame without its time column, or without
the series column a multi-series fit needs, must be refused. A frame
for a one-series fit may omit the series column.

### Draw alignment

**test-draws-alignment.R** fits four models, three Poisson AR(1)s of
increasing complexity and one ordinal fit under `ZMVN()`. A post-fit
result is assembled from several extractions of the posterior. They
describe the same model only if they come from the same iterations.
When a method passed each extraction a draw count where it should have
passed draw indices, each extraction subsampled on its own. The terms
then added together came from different iterations, which left every
row finite, plausible and mismatched. The four models differ in how
many extractions have to line up. The more extractions a result
combines, the more places a mismatch can occur.
