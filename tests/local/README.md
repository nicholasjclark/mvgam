# Testing mvgam against real fitted models

`tests/testthat/` answers whether the package builds the right Stan
program. These files answer what the package does once a real
posterior exists, which is most of it. Everything a user reaches for
after `mvgam()` returns needs a fitted model to run on at all:
`forecast()` and `hindcast()`, every prediction route, `loo()` and
`lfo_cv()`, `irf()`, `fevd()` and `stability()`, `residual_cor()`,
the tidiers, the plotting methods, `summary()`,
`conditional_effects()` and the hooks that insight and marginaleffects
call. Not one of those can be reached without sampling first, which is
why they are here and not in CI.

Parameter recovery is a part of that rather than the point of it. It
is how a file establishes that its fit is worth asserting against: on
a posterior that did not find the truth, a disagreement between two
methods says nothing about either. Once recovery holds, the fit
becomes the instrument, and the questions asked of it are whether two
routes to one quantity agree, whether every method reads the axis the
sampler was given, whether an argument that was accepted was also
honoured, and whether a refusal names something a user can act on.

## When to run this

Occasionally, not routinely:

- after a change that touches how a model is assembled, indexed,
  predicted from or reported on, where the unit tests pass and you
  want to know the answers did not move
- before a CRAN submission, as the last check across the whole
  post-fit surface that nothing has gone quietly wrong

A cold run fits every model and takes hours. Fits are cached, so a
second run loads them and takes minutes.

## How to run it

One file:

```bash
TESTTHAT_MAX_FAILS=1000 Rscript -e \
  "devtools::load_all('.'); testthat::test_file('tests/local/test-trend-var.R')"
```

Everything, writing each result to its own log rather than to the
terminal, because a truncated view hides the summary line that says
what failed:

```bash
for f in tests/local/test-*.R; do
  TESTTHAT_MAX_FAILS=1000 Rscript -e \
    "devtools::load_all('.'); testthat::test_file('$f')" \
    > "/tmp/$(basename $f .R).log" 2>&1
done
```

`TESTTHAT_MAX_FAILS` is not optional here. Files in this directory are
expected to fail, and testthat abandons a file after ten failures,
which leaves every block below the tenth unrun and reported as
nothing. The limit is read when the reporter is built, before a file
is sourced, so it cannot be set from inside one.

Fits cache under `tests/local/fixtures/`, which is gitignored and
created on demand. Delete a file there to refit that model; delete the
directory to refit everything. No build step and no shared fixture:
each file owns the models it fits, so a file can be run on its own
from a clean clone.

## Failing assertions are the point

Some assertions in these files fail, and they are meant to. Each one
states what the documentation or the neighbouring method promises, and
fails because the package does something else. `FINDINGS.md` records
every one of them with the measurement behind it. An assertion is
never weakened to make a file green: a red test with a finding beside
it is the record that the defect is still open.

## What each file fits, and why the model has to be fitted

Every file drives the same broad post-fit battery, because most defects
found here were not specific to the trend that exposed them: a method
reading the wrong axis, an argument accepted and dropped, two routes to
one number disagreeing. What differs between files is the structure
that makes a particular defect visible, and that is what each entry
below describes. A model earns its place here when it makes some class
of mistake findable that the others cannot, so asking the same
questions of each one is not asking the same test repeatedly.

### Trend kernels

**test-trend-var.R** carries one model: `VAR(cor = TRUE)` over three
gaussian series on sixty occasions, with `y ~ elev * region +
(1 | block)` on the observation side. A VAR differs from a bank of
independent AR(1)s only in the off-diagonal entries of the transition
matrix, and a diagonal `A` fits comparably well, so nothing short of
recovering `A` entry by entry separates them. The one-step forecast is
the claim that ties `A` to what a user receives: it mixes the series,
so a recursion that transposed `A` or ran each series alone lands
elsewhere while staying finite and correctly shaped.

**test-trend-ar-multilag.R** fits `AR(p = c(1, 3, 12))` on two gaussian
series over ninety-six occasions, with `gp(x)` on the observation side.
Passing a vector asks for three specific lags rather than the first
twelve. A model that read the vector's maximum would emit twelve
coefficients and fit at least as well, so only a fitted posterior says
which lags exist.

**test-trend-arma.R** fits `AR(p = 1, ma = TRUE)` on two gaussian
series over eighty occasions, under a two-dimensional `gp(x1, x2)`.
There is no `ARMA()` constructor, so the moving-average term is an
argument that can be accepted and dropped. The file's claim is the
contrast between the same model with and without it.

**test-trend-pw.R** fits `PW(n_changepoints = 8)` on two Poisson series
over sixty occasions, under an offset and a two-dimensional smooth. A
piecewise trend is decided by where its changepoints sit and how the
rate adjustments attach to them, which lives in `t_change_trend` and
`delta_trend` rather than in the fitted line.

**test-trend-car-irregular.R** fits two models on an irregular grid,
`CAR()` and `ZMVN()`, over nine Poisson series and twenty-six
occasions with gaps of one to four. `CAR()` is the one trend that
reads the time axis as a quantity rather than an index: it raises its
damping to the power of the gap. On a regular grid a rank and a value
coincide and the difference cannot be seen, so this is the only file
where a derivation that counted occasions instead of measuring them
fails.

**test-trend-hierarchical.R** fits `AR(gr = region, subgr = species,
cor = TRUE)` over two regions of three Poisson species on ninety
occasions. Grouping makes mvgam derive the series identifier rather
than read a column, and that derived value has to agree with the
levels recorded at fit time. When it did not, every post-fit method
failed at the level validator while `summary()` kept working.

**test-trend-map.R** fits `AR(p = 1, trend_map = Z)` on four Poisson
series loading two latent factors through a known `Z`. This is the
only route where the loadings reach Stan as data rather than as a
parameter, so it is the only place the row order of `Z` can be checked
on values instead of on declaration text, and the only place
`trend[t, s] = Z[s, ] . lv_trend[t, ]` can be verified from both sides
of the identity.

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
given is a statement about a posterior and cannot be read off a
program.

### Observation families

**test-family-tweedie.R** fits `tweedie()` with an AR(1) trend over
sixty occasions at `p = 1.4`. Tweedie is the only exported family that
carries its own Stan code, hung off the family as
`attr(fam, "mvgam_stanvars")`, so this is the only place that path
runs end to end. The claim is the mass at zero, which the closed form,
the fitted density and the draws each reach by a different route.

**test-family-mixture.R** fits three models: a hurdle Poisson with a
scalar hurdle, a hurdle Poisson with `hu ~ z` on a logit link and a
zero-inflated Poisson at a low rate. A hurdle density written with a
zero-inflated branch returns finite numbers of the right shape and is
a different distribution, so each fit is checked against its own
closed form and required to fail the other's.

**test-family-com-binomial.R** fits one Conway-Maxwell binomial over
two series and fifty occasions, under

```r
bf(y | trials(n_trials) ~ s(x, k = 5) + series + (1 | site) + mo(dose),
   nu ~ z)
```

with an AR(1) trend. The family is a custom brms family carrying its
own Stan code and a second distributional parameter, and it takes its
denominator through a `trials()` aterm.

The aterm is why the fit exists. A trial count is a denominator rather
than a predictor, so `find_predictors()` leaves it out of the term
list, and the three routes that build a frame from that list have no
denominator to put in it: the smooth grid, the marginaleffects grid
and the padded forecast grid. Each fails in its own way, and none of
them is reachable from a family whose response needs no denominator.

The rest of the predictor is there because each term reaches a method
nothing else here drives. `mo(dose)` is the only monotonic effect in
the directory, so its simplex of increments and the monotone ordering
it imposes are checked nowhere else. `(1 | site)` is the only
group-level effect outside the VAR fit, which makes it the only place
`ranef()`, `VarCorr()` and `ngrps()` can be asked anything: elsewhere
they refuse for want of a grouping. `nu ~ z` puts a sub-formula on the
second distributional parameter of a custom family, where the only
other dpar coverage sits on a built-in one. The same fit is the one
place `predictive_error()`, `predictive_interval()`,
`posterior_interval()` and `how_to_cite()` are driven at all.

**test-family-jsdgam.R** fits the same latent factor structure under
three observation families, negative binomial, multivariate Student-t
and Dirichlet. The shared questions are asked once per family, and
what belongs to one family alone sits beside it: the dispersion, the
tail and the simplex a composition has to respect.

### Index grain

**test-grain-closure-units.R** fits seven models around the closure
unit, which is the `(series, time)` cell holding one latent state
while a visit is a row inside it. Twenty-five sites over three
occasions with four visits makes units, series and rows three
different counts, so a method answering on the wrong grain cannot be
right by accident. The seven cover occupancy, abundance, a factor
model over each, a complete and a gappy visit schedule, then a
multi-season fit whose grouping runs over three axes.

**test-grain-mvbf-wide.R** fits one `mvbf()` model with a Poisson, a
Bernoulli and a gaussian arm sharing one AR trend over sixty
occasions. A wide frame holds one row per occasion and one column per
response, so the series an observation sits on belongs to the
`(row, response)` pair. That is the one shape a data-frame attribute
cannot express, and the defect it produced left every count, dimension
and bound correct. Three families in one fit also makes each
family-level claim three times over.

### Observation-side terms

**test-obs-smooth-surfaces.R** fits one Poisson model carrying
`s(z, by = grp) + t2(z, w) + gp(w, by = cat)`. Three smooth shapes in
one model is what makes the claim possible: `smooths()`,
`posterior_smooths()` and `conditional_smooths()` each have to pick
the right term out of three, and a method that returned the first term
whatever it was asked for passes on any single-smooth model.

### Draw alignment

**test-draws-alignment.R** fits four models, three Poisson AR(1)s of
increasing complexity and one ordinal fit under `ZMVN()`. A post-fit
answer is assembled from several reads of the posterior, and they
describe the same model only if they describe the same iterations.
Handed a draw count rather than draw indices, each read subsampled on
its own and the terms then added together came from different
iterations, which leaves every row finite, plausible and mismatched.
The four differ in how many reads have to line up, because the failure
needs somewhere to hide.
