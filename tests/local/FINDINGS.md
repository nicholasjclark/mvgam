# Defects the local fixture assertions found

Every entry here is still open. A finding is deleted once its fix is
verified by the assertion that caught it, so the file shrinks as the
work lands rather than accumulating a record of what used to be
wrong.

Each entry names the failing file and line. It then states what the
assertion claims, against what the package does instead. Line numbers
move as the files grow, so each entry also names the test by its
description.

## mvn()

**5. The mvn fixture asks for a split the data cannot identify.**

The fixture declares four species and two latent factors. A factor
model separates a per-species residual scale from the factor
covariance only when `(K - m)^2 >= K + m`, which at `K = 4, m = 2`
reads `4 >= 6` and fails. `Psi` and the diagonal of `Z Sigma Z'`
therefore trade against each other along a ridge, and the file's "Psi
recovers the simulated residual scale" assertion reads one arbitrary
point on it. Psi posterior means came back at 0.556, 1.194, 0.448 and
0.475 against a truth of 0.5 throughout.

Diagnosing this separated three mechanisms, and only the first is what
the recovery assertion meets.

- The bound above. `jsdgam()` defaults to `n_lv = 2`, so three and
  four species fail it by default. `mvgam()` now warns at fit time and
  names the largest `n_lv` the species count admits.
- The divergences are a funnel at small `Psi` rather than the scale
  ridge. Locating the divergent draws in the geometry puts them at log
  min `Psi` -1.44 sd, against +0.44 sd along the ridge direction.
- `Z` and `sigma_trend` enter the trend only as a product, verified to
  4e-16 on a posterior draw. That redundancy is exact and is not what
  either diagnostic above measures.

The "13 per cent divergences" this entry first recorded belong to an
`mvn()` fit no file holds; the cached `mvt` fixture has none.

Three things remain. Respecify the fixture at a `(K, m)` pair the
bound admits, so its recovery assertions test an identified quantity.
Give `Psi` a prior class, since it is hard-coded in a stanvar today
and a user who knows their response scale cannot set the one thing
that moves the posterior. On a truth of 2.0, `gamma(4, 2)` gives 34
divergences at Psi rhat 1.009; `exponential(1)` gives 225 at 1.084;
`gamma(4, 8)` gives 335 at 1.396, with two species pulled to 0.9 by a
prior centred on 0.5. No fixed
constant suits every response scale, so a new default needs
calibrating over a grid of true `Psi` and factor share before it is
chosen. And give the parameter an interpretable home, the variance
decomposition `Psi_i^2 / (Psi_i^2 + (Z Sigma Z')_ii)`, so the raw
scale is not what a reader acts on.

**7. One post-fit method guards against a prefit. Seventeen do not.**

No file covers this. `run_model = FALSE` returns an object of class
`mvgam_prefit` whose `$fit` is `NULL`, so anything needing a posterior
has to refuse. `summary()` does it properly:

    No fitted model found in mvgam object.
    summary() requires a fitted Stan model and an unfitted stub was
    supplied (`run_model = FALSE`).
    Use `stancode()` ...

It names the state, names the argument that produced it and points at
what does work. Every other method needing draws instead falls through
to `posterior::as_draws_matrix()` failing on the empty slot:

    Don't know how to transform an object of class 'NULL' to any
    supported draws format.

Measured across the post-fit surface, that is what comes back from
`posterior_epred`, `posterior_predict`, `posterior_linpred`,
`predict`, `fitted`, `residuals`, `log_lik`, `hindcast`, `forecast`,
`loo`, `variables`, `tidy`, `augment`, `plot`, `pp_check` and
`mcmc_plot`. `conditional_effects()` differs only in failing further
out, inside `insight::get_data()`. `stancode()` and `standata()` both
answer, as they must: reading them is why the mode exists at all.

The pattern to copy already exists in the package, which is what makes
this worth fixing rather than tolerating: one guard on the class,
raised where a method starts, would replace seventeen internal errors
that all say the same unhelpful thing.

The test holds every method to the message `summary()` already
produces, so it fails until they meet it. It was written after the
first version of this entry described the defect in a markdown file
and quietly moved the assertion onto a fitted object, where it passed
and proved nothing.

## VAR()

**8. `irf()` and `fevd()` label their shocks `Process_k`.**

`test-trend-var.R`, "irf and fevd name the series, not Process_k". Both
tables come back keyed by strings of the form `Process_1 -> Process_2`
on a fit whose series are `willow`, `ash` and `rowan`. The mapping from
`Process_2` to a series is positional and appears nowhere in the
output.

This is the failure the axis work exists to end, in its mildest form:
the numbers are right and the reader cannot tell which series they
belong to. An impulse response is read to decide which series drives
which, so a label nobody can resolve makes the whole table
unusable without knowing the internal ordering.

`posterior_transition_matrix()` does the same, in `series_names` and
in the dimnames of every block it returns. That one matters most of
the three. `?posterior_transition_matrix` presents it as the direct
route to `A`, so a reader goes there first, and what they find is
correct values under labels that resolve to nothing.

`residual_cor()` carries the series names on both margins and the
hindcast and forecast arms are named. These three VAR summaries are
the exception.

## pp_check

**10. `intervals` and `ribbon` raise a deprecation on every call, and
that costs `x` its coverage.**

`pp_check(fit, type = "intervals")` and `type = "ribbon"` reach
`bayesplot::ppc_intervals`, which builds its layer with
`geom_linerange(size = )`. ggplot2 deprecated that in 3.4.0, so every
call raises

    Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    Please use `linewidth` instead.

mvgam forwards only `y`, `yrep` and `x`, so the call is bayesplot's
and so is the repair. It is recorded here because it reaches every
user of those two types, on any fit, with or without an `x`.

The consequence for this suite is that the `x` argument has no test.
These two are the only types that take one: the third,
`error_scatter_avg_vs_x`, is deprecated inside bayesplot itself.
Reaching an assertion means silencing a notice every user receives,
which is a worse trade than leaving the argument uncovered, so
`test-trend-var.R` covers `group` and states why it stops there.

A lifecycle notice is also raised once per session, so an
`expect_warning()` on it passes or fails on what ran before the file
rather than on anything the package did.

## loo()

**11. Every fit with a latent trend breaks PSIS-loo, and the numbers
belong on the record rather than under a suppression.**

Unmasking `suppressWarnings(loo(fit))` across the seven fits written
in this pass gives, per fit, the share of observations whose Pareto-k
crosses the thresholds:

| fit | n | max k | > 0.7 | >= 1 |
|---|---|---|---|---|
| var_trend | 180 | 1.14 | 22.8% | 1.1% |
| pw_trend | 120 | 1.98 | 10.0% | 5.8% |
| ar_multilag | 192 | 1.09 | 35.9% | 1.0% |
| arma_trend | 160 | 1.12 | 62.5% | 1.2% |
| car_irregular | 78 | 1.52 | 50.0% | 5.1% |
| by_lv_axis | 276 | 0.93 | 1.4% | 0% |
| hier_trend | 240 | 0.99 | 9.2% | 0% |

This is the expected behaviour of a state-space model rather than a
defect: dropping an observation moves the latent state it is being
scored against, so the importance ratios have no finite variance.
A fit of the same family with no trend satisfies `all(k < 1)` on the
same assertion, which is what makes the table above read as a
property of the trend rather than of the family.

It is recorded because it bears on how `loo()` should be read on
these models, and because two of the three published comparisons in
the package rank trend models by `elpd_loo`. `lfo_cv()` is the tool
that answers the question these fits are being asked, and nothing in
`loo()`'s output on a trend fit says so.

## Families

**4a. Three families are classified as closure-unit, not one.**

`test-grain-mvbf-wide.R`, "only the closure-unit families are classified as
such". Finding 4 recorded `mvn()` reaching an occupancy-only code
path. Asked of the family table rather than of one fit,
`is_closure_unit_family()` answers `TRUE` for `mvn()`, `mvt()` and
`diri()` as well as for `occ()` and `nmix()`.

None of the three models a detection process over repeat visits to a
closed unit. `mvn()` and `mvt()` are multivariate observation models
and `diri()` is a composition, so every path this predicate guards is
reached by three families it was never written for.

One attribute answers two questions. A caller may mean "does this
family run through the same data preparation". It may instead mean
"does this family model detection over a closed unit", which is true
of `occ()` and `nmix()` alone. Both are spelled the same way, so
each of the thirty-odd call sites has to be read to learn which it
asks. `needs_closure_unit_aggregation()` and
`is_simplex_response_family()` each name a piece of the difference.
The wire format has no name of its own and borrows this one.

The assertion asks the registry directly, so it covers every family
at once.

**14. `marginaleffects` does not know mvgam accepts `resp` or
`process_error`.**

Same block. Naming a response raises

    These arguments are not known to be supported for models of
    class `mvgam`: resp.

marginaleffects keeps a whitelist per model class and mvgam has not
registered `resp` on it, so every user of a multivariate fit meets
this on every call that names an arm. The argument is forwarded and
honoured; only the notice is wrong.

`process_error` is on the same footing, and reaches further. It is
mvgam's own argument, it sits in the signature of
`get_predict.mvgam()` and it decides whether a marginal prediction
carries the trend's innovations. Every call that sets it
raises the notice, on a univariate fit as much as a multivariate
one. Seen in `test-draws-alignment.R`, "process_error moves a
marginal prediction", where the two calls that establish the
argument does something both warn that nothing is known about it.

## trend_map

**16. A matrix `trend_map` ignores its rownames, then writes the
declared ones over them.**

`test-trend-map.R`, "a matrix map keys its rows by the names the
user gave". A matrix carries rownames, and a user who supplies them
is saying which series each row of loadings belongs to. They are
dropped: rows are taken in position order against the frame's
declared series levels.

Measured on a four-series frame declaring `delta, alpha, charlie,
bravo`. One map names its rows in that order, another names the same
contents `alpha, delta, bravo, charlie`. Both emit an identical `Z`,
and both come back carrying the rownames `delta, alpha, charlie,
bravo`.

So the emitted matrix asserts the assignment the user asked for while
holding another series' numbers. Two series load on each other's
factors. Nothing raises, every dimension agrees and reading `Z` back
confirms the mistake rather than revealing it. On this one route the
loadings are the user's own statement of which series loads on what,
which is what makes the silence costly.

The data-frame form is unaffected: it names its series in a column
and a stranger there is refused.

## Gaps closed rather than found

Two things the plan names as untested now have coverage, and the
package passes both. Three families that had none now have it too.

The axis record carries the user's own time values rather than their
ranks. `test-factor-lv-axis.R` numbers its occasions from 3 and
a jsdgam fit numbering its sites from 3, so a function returning the
index where it was asked for the value becomes visible. Substituting
ranks for values inside `build_training_arms()` fails the assertion, so
it bites.

A frame is also mapped to trend cells and back to labels with no
posterior in hand. `axis_row_series()`,
`validate_prediction_factor_levels()`, `build_training_arms()` and
`resolve_forecast_grid()` all run on a `run_model = FALSE` prefit. The frames
they are given include a shuffled copy of the training data and one
naming a species the model never had. Two more probe the horizon: one
reaching past the training grid, one wholly inside it.

Tweedie is now fitted. It was the only exported family nothing in
`tests/local` ever fitted, and the only one carrying its own Stan
functions through `attr(family, "mvgam_stanvars")`.
`test-family-tweedie.R` reaches `P(Y = 0) = exp(-mu^(2-p)/(phi(2-p)))`
three ways -- the closed form, `exp(log_lik())` at the zero rows, and
the fraction of zeros among the draws -- and all three agree, the
first two exactly. Checking the density itself would have been
circular, since mvgam's post-fit `log_lik` calls `mgcv::ldTweedie`.
The prefit carries `M` as data, so `standata()$M` is 30 or 40 as
asked while the code is byte-identical.

Hurdle and zero-inflated Poisson are covered the same way, each
against its own closed form and against the other's, so neither
mixture can stand in for the other.

Every `\seealso` link in the package resolves. Resolved across all
195 man pages, against the package's own aliases for a bare
`\link{}` and against the named package for a `\link[pkg]{}`, none
is broken. Every function `?jsdgam` lists was separately called and
each one answers, which finding 75 records. On that route a reader
following the documentation reaches working code.

## Fits still worth adding

### The remaining shapes

The wide `mvbf()` frame, the fixed-loading `trend_map`, `lfo_cv()` on
a trend fit and `update()` are all fitted now, so what remains is two
shapes and one family argument.

### A hierarchical VAR

    trend_formula = ~ VAR(gr = region, subgr = species, cor = TRUE)

Two structures that are only tested apart. `A_trend` comes back as
`[N_groups, N_subgroups, N_subgroups]`, so the transition matrix is
per group over subgroups rather than over all series, which is the
claim `test-trend-hierarchical.R` already makes about the correlation
block and has never made about `A`.

It is also the worst case for finding 8. `irf()` labels its shocks
`Process_k`, and on a derived `gr` / `subgr` axis there is no column
to compare those labels against at all.

Fit it twice, once on a frame carrying a superseded `series` column
and once on a frame with no series column whatever, since a frame of
the second kind is refused at four separate layers and no fit starts
from one.

### The `AR()` arguments nothing exercises

    AR(p = 2, coef_sharing = ..., df = 4)

`coef_sharing` appears in no local file. `df` appears only on
observation families, never on the trend.

Finding 6 and finding 58 are the reason to care: a factor request is
accepted and silently saturated on two trend types, so an argument
read and dropped is a mistake this package makes. The test that
settles it is the contrast `test-trend-arma.R` uses for `ma`: build
with and without, and require the programs to differ by the machinery
the argument names.

Student-t innovations also give the trend a tail, so a fit with a
small `df` should absorb an outlying occasion into the innovation
rather than into the level. That is a claim about a value.

### `score()` and `ensemble()` over two fits

Both need two fits on one frame, which also gives `loo_compare()`
something to rank. Assert the scores are keyed by the series axis,
since a per-series score under permuted names is finding 8 in a place
a user acts on.

## The insight surface

**44. A random-effect grouping factor is reported as a fixed
predictor, and `find_random()` finds nothing.**

`test-trend-var.R`, "find_predictors reports a series column that
varies". insight splits a model's terms so that a consumer knows
which of them carry a population slope. mvgam does not make the
split:

| call | brms `y ~ elev * region + (1 \| block)` | mvgam, same terms |
|---|---|---|
| `find_predictors()$conditional` | elev, region | elev, region, block, time, series |
| `find_predictors(effects = "all")$random` | block | absent |
| `find_random()` | block | `NULL` |

`lme4::lmer` on the same formula answers as brms does, so this is
insight's contract and not a brms convention. Reproduced on the
random-effects fits as well, where `grp` appears among the
conditional terms and `find_random()` is again `NULL`, so it
belongs to the method rather than to one fit.

What it costs is the term list every downstream package builds from
it. `marginaleffects` reads `$conditional` to decide what can be
contrasted, so it offers a grouping factor as a term to take a
slope or a comparison over. The grouping itself stays invisible:
nothing reading an mvgam fit through insight can discover that the
model has a random effect at all.

The `effects` argument is not honoured either. Asked for `"all"`,
the method returns the same one-element list it returns by default,
so a caller who asks for the split explicitly is told nothing about
why it did not happen. `find_variables()` carries no `random`
element for the same reason.

The hierarchical fit shows the same fault with nothing left to be
right about. Its observation formula is `y ~ 1`. The model has no
predictor whatever. `find_predictors()$conditional` answers
`time, series, region, species`. Two of these name the axis. The other
two name the grouping the trend is built on. Every term offered to a consumer is
one no user can take a meaningful slope over, and the model's real
term list is empty.

The consequence is measurable rather than hypothetical, and it
reaches the plotting surface too. `conditional_effects()` on the VAR
fit returns four panels, and one of them is `block`: six shrunk
group deviations drawn as an effect, spanning -0.349 to 1.117, which
is a wider range than the `elev` panel beside it. A reader is given
no sign that those six levels are exchangeable draws rather than
categories.

`avg_slopes()` on a fit with a random effect returns one contrast row
per non-reference level of the grouping:

| fit | grp contrasts | range of the estimates |
|---|---|---|
| ar1_re | b-a to f-a | -0.17 to -7.80 |
| ar1_re_smooth | b-a to f-a | -0.03 to 1.03 |
| ar1_cor_re | b-a to f-a | -0.18 to -7.88 |

Those are group-level deviations, shrunk toward zero by the prior
on `sd_grp`, presented as population contrasts a reader could act
on. Nothing in the table says the levels are exchangeable draws
rather than fixed categories. `test-trend-var.R` and
`test-family-com-binomial.R` each assert the term list on a fit with
a grouping, so both fail until the split exists.

## Prefit modes

**46. `chains = 0` samples anyway, and the diagnostics warn about
the chain it ran.**

No file covers this. Two Stan-emission blocks reached it, each Both ask for a
program without a posterior, spelled

```r
mvgam(y ~ elev, family = nmix("royle_nichols"), data = d,
      algorithm = "sampling", chains = 0)
```

and both then read `stancode()` off the result. What happens in
between is a compile and a two-iteration run:

```
Running MCMC with 1 chain...
Chain 1 WARNING: No variance estimation is performed for
                 num_warmup < 20
Chain 1 Iteration: 1 / 2 [ 50%]  (Warmup)
Chain 1 Iteration: 2 / 2 [100%]  (Sampling)
```

testthat then records a warning against each block:

    E-BFMI not computed because it is undefined for posterior
    chains of length less than 3.

So a request for no chains produces one chain of two draws, and the
sampler diagnostics run against it and report that they cannot. The
returned object carries a posterior a user could read and summarise,
of two iterations with no warmup behind them.

`run_model = FALSE` is the mode that does what these calls were
asking for, and `stancode()` answers on it. The two spellings should
not both exist and disagree about whether sampling happens. The
blocks are left as they are so the warning keeps arriving, rather
than being spelled around in the test.

## Aterms and the prediction grid

**47. `conditional_effects()` cannot run on a model with a
`trials()` aterm.**

Found by driving the model rather than by an assertion. On a plain
`binomial()` fit of `y | trials(trials) ~ s(x)`:

    Unable to compute predicted values with this model ...
    The following variables can neither be found in 'data' nor in
    'data2': 'trials'

The column is not missing from the fit. Asked four ways, it is
there:

| call | trials present |
|---|---|
| `fit$data` | yes |
| `insight::get_data()` | yes |
| `model.frame()` | yes |
| `find_predictors()` | no, correctly |
| `datagrid(x = 0)` | no |

The last two rows are the whole of it. A trial count is a
denominator rather than a predictor. `find_predictors()` is right to
leave it out, and `datagrid()` builds its grid from that list. The
denominator therefore never reaches the grid, and brms refuses a
prediction without it. `posterior_epred()` on the same fit answers
normally, so the fault is in how the grid is built and not in the
prediction.

There is no way round it from the outside. Naming the column
explicitly works, as `datagrid(x = 0, trials = c(10, 50, 100))`
does, but that is a call the user has to construct. `conditional_effects()` builds its own grid, and
supplying one is refused:

    Cannot pass 'newdata' through `...`. These are set by
    conditional_effects.mvgam; pass via the named arguments instead.

So every binomial model written with the aterm brms requires loses
`conditional_effects()` entirely. Reproduced on `com_binomial()` as
well, so it belongs to the aterm and not to one family.

The grid needs to carry aterm columns at a representative value, the
way it carries a covariate held at its mean.

**Not a defect, recorded because it was checked.**
`predict(type = "variance")` refuses `com_binomial()` and names both
the families it supports and what to do instead. `diri()` is
accepted, and returns `[ndraws x nobs]`. Both are built through
`brms::custom_family()`, and the dispatch that separates them reads
the name mvgam records alongside the family object.

## Sampler settings

**49. A bare `adapt_delta` or `max_treedepth` is accepted and
discarded.**

Found by reading a vignette that asks for one and checking whether it
arrived. `mvgam()` takes both through `...` and never reads them.
`R/mvgam_core.R:889` reads `control <- dots$control %||% NULL`, and
nothing anywhere in `R/` lifts a top-level `adapt_delta` into it.

Asked for `adapt_delta = 0.99, max_treedepth = 15` on one frame,
reading back `fit$fit@stan_args[[1]]$control`:

| call | recorded | warning |
|---|---|---|
| `mvgam(..., adapt_delta = 0.99, max_treedepth = 15)` | 0.8, 10 | none |
| `mvgam(..., control = list(adapt_delta = 0.99, max_treedepth = 15))` | 0.99, 15 | none |
| neither argument | 0.8, 10 | none |

So the bare spelling runs at Stan's defaults and says nothing. This is
finding 6's shape on an argument every user reaches for: a divergent
fit is the usual reason to raise `adapt_delta`, and the raise is what
gets dropped, so the sampler keeps diverging and the call looks like
it addressed the problem.

Three articles on this branch were written with the bare spelling and
their prose states the tighter setting was used. `idm.Rmd` uses
`control = list(...)` and is the only one that got what it asked for.

`jsdgam()` forwards to `mvgam()`, so it behaves the same way.

## Refusals that name an internal

**52. A missing covariate value stops on a checkmate assertion about
`eta`.**

`test-trend-var.R`. One `NA` in a covariate column of a `newdata`
frame ends the prediction with

    Assertion on 'eta' failed: Contains missing values (row 1, col 1).

`eta` is the linear predictor mvgam builds internally. The caller
never supplied it, cannot see it and cannot map "row 1, col 1" back
to a row of the frame they passed, so nothing in the message says
which column carried the gap or what to do about it.

The same fit refuses other malformed frames well: an unknown series
names the level and lists the ones the model has, and a gapped
forecast frame names the series, the last observed time and the times
it expected. So the standard is set within the same object.

Reproduced by setting one cell of `elev` to `NA` on an otherwise
valid frame. Whether a missing covariate should be an error at all is
a separate question, since a missing response is handled by the
likelihood; what is recorded here is that if it is an error, it
should name the user's column.

## Documentation that contradicts the code

**53. The Royle-Nichols detection predictor is documented on the log
scale and built on the logit scale.**

`?sim_closure_unit_data` describes the detection predictor for
`royle_nichols` as being on the log scale. Both the family and the
simulator disagree: `nmix()` declares `links = c("log", "logit")`, so
detection takes the logit, and `R/sim_closure_unit_data.R:512` builds
the probability with `plogis(det_lp)`.

A reader following the documentation sets a detection intercept on the
wrong scale, and the value they choose is silently a different
probability from the one they meant. Found while enabling the
`royle_nichols` chunk of `vignettes/articles/nmix.Rmd`, whose own
table repeated the documented scale.

**54. `conditional_effects()` and `conditional_smooths()` answer in
different column spellings.**

`test-trend-ar-multilag.R`. Both are drawn views of a fitted term and
they name their columns differently:

| method | estimate | interval |
|---|---|---|
| `conditional_effects()` | `estimate` | `conf.low`, `conf.high` |
| `conditional_smooths()` | `estimate__` | `lower__`, `upper__` |

brms uses the trailing-underscore spelling for both. mvgam renames one
and not the other, so code written against a `conditional_effects()`
frame fails on a `conditional_smooths()` frame from the same fit, and
the failure is a missing column rather than a message.

Neither spelling is documented as the contract, so the assertions
resolve whichever is present rather than fixing one.

## com_binomial and the trials aterm

**61. The smooth grid omits a distributional parameter's covariate.**

Same fit. `plot(type = "smooths")` stops with

    The following variables can neither be found in 'data' nor in
    'data2': 'z'

`z` is the predictor of `nu`, not of the mean. The grid the smooth is
drawn over backfills the columns the mean's formula names and holds
them at representative values, and a covariate that appears only in a
dpar sub-formula is not among them, so brms is handed a frame missing
a variable the model needs.

The denominator reaches the same grid correctly, so the backfill
handles aterm columns and not dpar ones.

**A difference worth recording, for the family work to settle.** The
lower bound on `nu` differs between the two spellings: scalar `nu` is
truncated, reaching Stan as
`normal_lpdf(nu | 1, 1) - normal_lccdf(-5 | 1, 1)`, while a modelled
`nu` gets a plain `normal_lpdf(Intercept_nu | 1, 1)`. Whether an
intercept on the identity scale should carry the scalar's bound is a
question for the family rather than for the axis work.

**Also checked and correct.** A user prior reaches the program on
every class it can be set on: `b`, `Intercept`, `sd`, `sds`,
`sigma_trend` and `ar1_trend` each arrive carrying the user's own
constant. `get_prior()` on an `mvgam_formula()` lists the trend
classes alongside the observation ones and reports the same `nu`
prior the program uses.

## Introspection

**64. `terms()` has no method, on any fit.**

Every other frame accessor answers. `model.frame()` returns the
78-row training frame, `formula()` the formula and
`insight::get_data()` the data. `terms()` raises R's own

    no terms component nor attribute

`terms()` is how a caller discovers a model's structure without
knowing the class, so a package that answers `model.frame()` and not
`terms()` breaks the pair. Seen on the CAR, ARMA, wide and
hierarchical fits, which between them cover a univariate trend, a
multivariate one, a response-keyed axis and a derived one, so it
belongs to the class rather than to any model.

## Two documents, two contracts

**75. `?jsdgam` states an `n_lv` constraint the package does not
have, and does not want.**

`test-family-jsdgam.R`, "n_lv reaches the ceiling the validator sets".
`?jsdgam` documents the bound on the number of latent factors as
depending on the loadings prior. Two branches share one rule there:
the default iid prior, and any structured prior whose kernel comes
from `traits` or `phylo`. For both, `n_lv` "must be strictly less than
the number of species". The man page gives the reason. At
`n_lv = n_species` the matrix `Z Z'` saturates the
residual covariance, per-species residual variance loses
identifiability under HMC and the sampler meets a heavy funnel.

Measured on four species, that bound is enforced nowhere:

| prior | `n_lv = 4` on 4 species | `n_lv = 5` |
|---|---|---|
| default iid | accepted, `N_lv_trend` 4 | refused |
| `traits` | accepted, `N_lv_trend` 4 | refused |
| `phylo` | accepted, `N_lv_trend` 4 | refused |
| `"mgp"` | accepted, `N_lv_trend` 4 | refused |

Only the MGP rule is implemented, and it is applied to every prior.
`validate_n_lv_ceiling()` at `R/validations.R:1396` refuses
`n_lv > n_species` and nothing else.

The code is right and the man page is wrong, which is what makes this
worth recording rather than fixing with a guard. The validator's own
roxygen says so deliberately, four lines above the function:

> `n_lv = n_series` is allowed: the loadings prior is what decides
> whether that boundary samples well, and saying so here would refuse
> a model the prior makes admissible.

So one package documents two contradictory contracts for one argument,
and the one a user reads is the one that is not true. Sampling the
saturated model settles which is right on the evidence: fitted at
`n_lv = n_species = 4`, it returns max r-hat 1.021 with 0 of 255
parameters above 1.05. The funnel `?jsdgam` warns of does not appear,
so refusing the model would have cost a user a fit that works.

`test-family-jsdgam.R` pins the behaviour the validator intends. A
later reader following `?jsdgam` would otherwise add the guard and
break a model that samples.

**Checked and correct.** Every surface `?jsdgam` lists under
`seealso` answers on a fitted jsdgam: `residual_cor()`, `ordinate()`,
`shared_variation()`, `active_factors()`, `compare_loadings()`,
`methods_md()` and `how_to_cite()`. The `traits` and `phylo` aliases
are refused alongside an explicit `loadings_prior`, as documented.

## Arguments nothing reads

**88. `standata()` on a fitted model returns the training data
whatever `newdata` says.**

`standata.mvgam(object, ...)` reads its `...` for nothing and
returns `object$standata`, the list built at fit time. brms's
method for the same generic rebuilds from five arguments this one
does not take, `newdata` among them. Writing
`standata(fit, newdata = nd)` against the brms API is therefore an
ordinary thing to do. On an mvgam fit it answers with the training
data: a well-formed list of the right shape describing another
frame entirely.

Measured on an `mvn()` fit whose frame was reordered within each
site. `standata(fit, newdata = reordered)$visit_idx[1, ]` comes back
as the training layout `1,2,3,4`, while
`closure_unit_arrays_for(fit, reordered)` reads `4,3,2,1` for that
same frame. Two accounts of one question. The method a brms reader
reaches for is the one that ignores the argument.

Its roxygen says `...` is "currently unused; present for S3 generic
dispatch", so the behaviour is deliberate and documented. What is
missing is the refusal: an argument the method cannot honour should
be rejected rather than dropped, which is the rule finding 76
records for the rest of this surface.

**76. Two methods of fifteen refuse an argument that reaches no
one.**

Found by misspelling one. `ordinate()` selects its ordination axes
with `which_lvs`, and a call written `ordinate(fit, axes = c(1, 5))`
returns a plot. The plot is of factors 1 and 2, labelled 1 and 2,
because `axes` reached `...` where nothing reads it. `which_lvs` is
faultless. Passing `c(2, 1)` transposes the picture and relabels it,
and a factor past `n_lv` is refused with the constraint named.

mvgam has already decided this is a defect. `R/forecast.mvgam.R:172`
carries the reasoning and the cure:

> Both methods take every argument by name after `...`, so a
> misspelling lands in `...` and the method proceeds on the default it
> was trying to override. That is how `incl_autocor` went unnoticed on
> `posterior_predict()`, and it is silent by construction, so refuse
> what nothing reads.

`rlang::check_dots_empty()` appears twice in the whole of `R/`, in
`forecast.mvgam.R` and `hindcast.mvgam.R`. Probed with
`zzz_unknown = 1`:

| behaviour | methods |
|---|---|
| refuses | `forecast()`, `hindcast()` |
| warns, from bayesplot rather than mvgam | `pp_check()` |
| accepts in silence | `ordinate()`, `residual_cor()`, `shared_variation()`, `active_factors()`, `summary()`, `posterior_epred()`, `posterior_predict()`, `predict()`, `fitted()`, `residuals()`, `log_lik()`, `plot()`, `posterior_smooths()`, `conditional_smooths()` |

The cost is measured rather than imagined. Writing `axes` for
`which_lvs` produced a picture that looked like an answer to the
question asked. Reading it as one is how a wrong axis pair reaches a
paper. The same shape reaches values as readily as pictures:
`incl_autocor` misspelled on `posterior_predict()` returns a marginal
prediction where a conditional one was asked for, every number finite
and plausible.

**What a unified check would have to respect.** The guard cannot be
applied everywhere, because `...` on this package's surface carries
two different meanings.

- Arguments that stop at mvgam. Every post-fit method above names
  each of its arguments and forwards none of them onward, so anything
  left in `...` is dead by definition and can be refused outright.
  This is the whole of the table above.
- Arguments that pass through. `mvgam()` and `jsdgam()` document `...`
  as the route to `data2`, `algorithm`, `chains`, `silent` and the
  rest of the brms and Stan surface. `jsdgam()` forwards to `mvgam()`
  in turn. A blanket refusal here would reject legitimate calls, and
  the set to allow belongs to brms rather than to mvgam.

So the check belongs where a method's argument list is closed and
known, which is the whole post-fit surface. The forwarding layer needs
a different treatment: an allowed set drawn from the callee's own
formals, or no check at all. Finding 49 is the pass-through half of this and shows
the cost of leaving it alone, since `adapt_delta` is read by neither
mvgam nor Stan when spelled bare and the sampler runs at its default
while the call looks like it addressed the problem.

`pp_check()` is the useful exception, and it is worth being precise
about what it does and does not settle. It says "The following
arguments were unrecognized and ignored: zzz_unknown", which names the
argument and is the behaviour this entry asks for. Two caveats sit on
it. The notice comes from bayesplot checking its own dots, so mvgam
contributes nothing and the notice leaves if the route to bayesplot
changes. And a warning is the weaker half of what `forecast()` does.
The plot is still returned, built on the default the caller was
overriding. A warning inside a loop or a knitted document is easily
missed.

`test-grain-mvbf-wide.R` pins the notice rather than demanding an
error there, so the one call on this surface that speaks cannot go
quiet unnoticed.

One method per release is not the way out. The two that guard were
fixed because a specific bug was traced to them, and thirteen more
carry the same hole today.

A baseline run of `tests/local` is what added the two smooth methods
to the table. This entry was first written from the methods that came
to mind. Every fixture file added since has turned up another.

## A notice about a parameter that was not used

**78. A warning is raised on a computation that was right.**

Found while chasing it as a suspected wrong answer. Predicting from a
gaussian fit with a factor trend raises

    Parameter 'sigma' has 2 columns but 600 observations.
    Using first column (scalar behavior).

The fit carries an observation `sigma` of one column and a
`sigma_trend` of two, one per latent factor, so a two-column sigma is
the trend's. The notice says the first column was taken, which on a
gaussian would put the trend's innovation scale where the residual
scale belongs and make every predictive interval too narrow.

It does not. For a gaussian, a draw is the expectation plus
`Normal(0, sigma)`, so the spread between the two says which parameter
was used:

| quantity | value | ratio to the observed spread |
|---|---|---|
| `sd(posterior_predict - posterior_epred)` | 0.3061 | |
| observation `sigma` | 0.2859 | 1.07 |
| `sigma_trend[1]` | 0.1600 | 1.91 |
| `sigma_trend[2]` | 0.1603 | 1.91 |

The draws were made with the observation sigma. The prediction is
correct and the warning describes something that did not happen.

Recorded because of what it costs rather than what it breaks. It
reaches the ordinary prediction path of a gaussian factor model. It
names a parameter the user never set and asserts a fallback that was
not taken.

It cannot be asserted where it appears. The notice carries "displayed
once per session". A block written against it therefore reports what
ran before it rather than anything the package did, which is the trap
finding 10 records for the ggplot2 lifecycle notice. A reader who
checks this one finds nothing wrong. The next warning on the same
surface is the one they will skip.

## One model, two observation counts

**87. `nobs()` counts the rows supplied rather than the rows fitted,
and its two branches disagree.**

Found while checking what `summary()` prints against what the model
was given. `nobs.mvgam()` returns `nrow(object$data)` and falls back
to `standata$N` when the frame is absent, so the same function
answers with either quantity depending on which slot the object
happens to carry. On a frame with no unobserved cell the two
coincide, which is why this went unseen.

Measured on the two cached fits whose frames carry unobserved cells:

| fit | rows | fitted (`standata$N`) | `nobs()` | `summary()` prints |
|---|---|---|---|---|
| by_lv_axis | 300 | 276 | 300 | 300 |
| occ_visits_gappy | 300 | 250 | 300 | 300 |

`summary.mvgam()` reads `nobs()` for the line a reader takes to be
the size of the analysis, and the comment above that call states the
intent the code does not meet: "How many rows the model was fitted
to, which is what `nobs()` answers". It answers the other one.

brms is the convention mvgam mirrors elsewhere and it counts the
fitted rows: `nobs.brmsfit` is `nrow(model.frame(object))`, and brms
drops the rows whose response is missing. Its signature also takes
`resp`, which mvgam's does not, so a wide fit cannot be asked for one
arm's count.

What is not settled, and is why this is recorded rather than changed:
mvgam requires the frame to be rectangular so the trend grid is
complete, so an unobserved cell is part of the design in a way it is
not for brms. Whether "Number of observations" should name the design
or the likelihood is a decision for whoever owns the printed summary.
What is wrong either way is that one function gives both answers and
its own comment claims the one it does not give.

## What reading a rendered article shows

**80. The VAR article's fit asks for four chains and reports three.**

Found by reading the rendered `vignettes/articles/var.Rmd` rather than
by checking that it rendered. The article knits in 14.1 minutes with
no error and no warning.

The chunk at `var.Rmd:133` reads `chains = 4`, and the `summary()`
printed underneath it says "Draws: 3 chains" with 4500 post-warmup
draws, which is 3 x 1500 exactly. So three chains are what the numbers
rely on. `summary()` is not miscounting: fitted at 2, 3 and 4 chains
it reports 2, 3 and 4 and `ndraws()` agrees each time. A chain was
therefore lost during this fit and nothing said so, with `silent = 2`
covering whatever was raised. A quarter of a posterior leaving without
a word is worth a message the caller cannot suppress by asking for a
quiet fit. Settling it needs the article re-rendered.

**82. `posterior_summary()` prints each arm's intercept twice, and
the mvbf article corrects a sign nothing makes indeterminate.**

Both were found by reading the rendered `vignettes/articles/mvbf.Rmd`.

`posterior_summary()` on a multivariate fit carries two spellings of
every arm's intercept, `Intercept_<r>` and `b_<r>_Intercept`, with
nothing to say they sit on different scales. That is brms's centred
parameterisation rather than an mvgam fault. `Intercept_<r>` is the
intercept at the covariate mean and `b_<r>_Intercept` the intercept at
zero, so the two part company by the slope times the covariate mean.
On the article's camera arm, whose covariate `deploy_days` is drawn
`Unif(5, 20)` and not centred, the two read 0.007 and -0.882 against
a truth of -0.5. The article now reads only the `b_` spelling and
says why, but a reader of the table the package prints still has two
rows and no guide.

`recovery_summary()` multiplies each posterior by
`sign(cor(med, truth$x))`, explaining that "latent factor models
identify the trend only up to sign". None of the four fits is a
factor model. Each is an AR(1) state with an identified intercept, so
the sign is identified and the correction has nothing to fix. What it
does instead is guarantee a non-negative correlation with the truth
for every fit in the table, which can only move RMSE downward. The
comparison it feeds is the article's headline claim that the joint
fit recovers the state best.

## An article that does not build

**85. `plot()` on an `fevd()` refuses one documented argument and
ignores the other, and the hierarchical VAR article fails on it.**

`vignettes/articles/hierarchical_var.Rmd` stops after 29.5 minutes at
its `fevd` chunk:

    Quitting from hierarchical_var.Rmd:459-462 [fevd]
    Assertion on 'series' failed: Must have length 1.

The call is `plot(fevds, series = 1:3, contributing = 1:3)`, and the
prose above it explains why: a 24-dimensional hierarchical VAR would
otherwise draw 24 target panels over 24 sources, so the article keeps
the display to Australia. Both arguments are documented and neither
works.

`fevd()` and `irf()` return objects that both inherit
`mvgam_var_surface_summary`, so both dispatch to one plot method.
Three roxygen blocks describe its `series` argument and no two agree:

| source | says |
|---|---|
| `R/mvgam_fevd-class.R:129` | "Optional integer vector selecting which target processes should be shown as facets" |
| `R/mvgam_irf-class.R:142` | "`integer` specifying which process series should be given the shock" |
| `R/var_surface_summary.R:61` | "Optional integer naming the process the shock originates in" |

The third is the one that runs, and it asserts
`checkmate::assert_int(series, lower = 1L, null.ok = TRUE)`. So a
vector is refused, on the argument whose own documentation offers a
vector and names the hierarchical VAR as the reason to want one.

Measured on the three-series VAR fixture:

| call | result |
|---|---|
| `plot(fv, series = 1)` | draws |
| `plot(fv, series = 1:2)` | refused, "Must have length 1" |
| `plot(ir, series = 1:2)` | refused, the same |
| `plot(fv, contributing = 1:2)` | draws, unchanged |

The scalar that is accepted keeps the wrong end of the pair.
`plot(fv, series = 1)` retains the three pairs whose left end is
`Process_1`. That is the shock's source. `?plot.mvgam_fevd` says `series` selects the target
processes shown as facets. The method filters `from == series`
because that is what the shock-origin reading means, and the fevd
documentation describes the opposite end.

`contributing` is worse, because it does not fail. It is absent from
the signature of the method that runs, so it lands in `...`, which
that method documents as ignored. Passing `contributing = 1` on a
nine-pair fevd leaves all nine pairs. The article's renormalisation
claim, that "the retained shares get renormalised per (target,
horizon)", describes something no code performs.

`responses` is the spelling the running method does read, and it
takes a vector. So the vector selection exists and is reachable under
another name at the other end of the pair.

This is finding 76 with a shipped article as the evidence: an
argument accepted and dropped, silent by construction. The difference
is that `contributing` is not a misspelling a user invented. It is
documented. It is motivated by a named use case. The package's own
vignette calls it.

## Debt the code carries in recognisable shapes

**89. Ten shapes account for the defects found so far, and a scan
counts six of them.**

The defects fixed while consolidating the response accessor were all
found by reading. Read together, they follow a small number of
shapes. Each shape leaves a mark in the source that a scan can find. `tests/local/debt_scan.R` reads parse data and counts the marks
in `R/`. Several shapes include false positives: the unused-argument
scan counts dispatch kernels that share a signature (`log_lik_*`
taking `trials`) and generics such as `methods_md()`, and each hit is
read before anything is removed.

| shape | the mark it leaves | count |
|---|---|---|
| an error turned into a default | `try()` and `tryCatch()` | 0, from 32 |
| one condition raised twice | `warning()` or `rlang::warn()` around `insight::format_warning()`, which raises its own | 0, from 7 |
| one fact, several derivers | raw `[[series_var]]` / `[[time_var]]` reads; `sort(unique(...))` axis rebuilds; `inherits(..., "mvbrmsformula")` asked in place of the question meant | 53, 40, 58 |
| a literal standing in for a missing value | `%||% "y"`, `%||% "series"`, `%||% "explicit"` | 118 |
| a missing column skipped | `intersect(x, names(data))`, `if (!col %in% names(df)) next` | 21 |
| a warning silenced, not traced | `suppressWarnings()`, `suppressMessages()` | 12 |
| an argument nothing reads | accepted, asserted, never used; the scan also flags `df` on `AR()`, `RW()`, `CAR()` and `ZMVN()`, finding 6's shape if it holds | 113 non-S3 functions |
| a stored copy of a derivable fact | object slots and metadata fields written once and read in a few places | not counted |
| one condition, several refusals | the same fault refused with different wording at different layers | not counted |
| a proxy for the question meant | "the frame has no series column" standing for "the responses are the series"; `length(x) > 1` standing for "multivariate" | found by reading |

The first two passes are done. Clearing the caught errors was not
mechanical: each one hid a defect of its own, and those were fixed
with the catch.

- `bf(y ~ 0)` under poisson failed the build, as did an `mvbf()`
  whose every response declined its terms. A caught error hid the
  same failure from `get_prior()`.
- A `trend_param()` condition that failed to evaluate went to a
  handler whose assignment never left it. The parameter was dropped
  while the comment beside it said it was kept.
- `get_prior(fit)` rebuilt the prior table from the formula inside a
  caught error, and could describe a different model from
  `prior_summary(fit)`.
- The smooth readers evaluated each `s()` and `gp()` term. `k = kk`
  failed there: one reader reported `kk` as a covariate and another
  refused the formula as invalid syntax.
- `methods_md()` reported the Stan and package versions of the
  session describing a fit, and under rstan it gave the rstan package
  version as the version of Stan. The fit now records its own.
- `validate_multivariate_trend_constraints()` could never run: every
  formula it was handed carried a response, which the parser refuses,
  and the caught error returned before any check.

Removing the placeholder catch exposed two older code-generation
faults in `mvbf()` models. The GLM rewrite matched response keys of
letters alone. Every response whose key holds a digit, `y1` or `y2`,
kept its original likelihood call and fitted with its trend computed
and never used. An arm written without an intercept did not compile.

The tests carry the same debt. Stubs that fake a class, such as
`structure(y ~ x, class = c("brmsformula", "formula"))`, or that
carry slots a real fit no longer has, let an assertion pass on an
object no user could build. Assertions that compare counts or use
`expect_setequal()` pass where the claim being tested is an order or
a value.

The scans are cheap, and a count falls only when code is deleted.
Each remaining shape gets one pass. A pass removes the rival, the
fallback or the proxy, adds an assertion that fails before the change
and records the count before and after.

## Which documents have actually been built

Recorded because a knit that skips every chunk reports success. The
three package vignettes gate their chunks on `params$EVAL`, and only
`rmarkdown::render()` supplies `params`, so `knitr::knit()` runs none
of them and returns in seconds. Everything below was built with
`render()` under `NOT_CRAN=true`, one document per R session.

| document | result |
|---|---|
| `vignettes/data.Rmd` | OK, 1.4 min |
| `vignettes/dfm.Rmd` | OK, 2.5 min |
| `vignettes/mvgam_overview.Rmd` | OK, 3.4 min |
| `articles/nmix.Rmd` | OK, 5.3 min |
| `articles/jsdgam.Rmd` | OK, 7.4 min |
| `articles/mvbf.Rmd` | OK, 4.2 min |
| `articles/var.Rmd` | OK, 14.1 min |
| `articles/idm.Rmd` | OK, 6.5 min |
| `articles/hierarchical_var.Rmd` | ERROR at 29.5 min, finding 85 |
| `articles/forecast_evaluation.Rmd` | not built |

`forecast_evaluation.Rmd` is the one gap, and it is the longest of
the articles. Nothing here has run it to completion, so its row is
blank rather than green.

Three of the eight that build carried a defect the build cannot see:
`var.Rmd`, `mvbf.Rmd` and `jsdgam.Rmd`, whose paragraph of stale
numbers is repaired. What is still open in the first two is under
findings 80 and 82. A knit reports whether the code ran. The faults
reading found sat in what the code printed and in the prose beside
it.
