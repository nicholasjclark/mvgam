# Defects the local fixture assertions found

Fixed entries keep their description so the assertion that caught them
stays legible. Open entries are the ones still to act on.

Each entry names the failing file and line. It then states what the
assertion claims, against what the package does instead. Line numbers
move as the files grow, so each entry also names the test by its
description.

## by = lv_axis()

**1. FIXED (documentation). `incl_latent_state` documented a mode it
does not have.**

`extract_component_linpred(component = "trend")` returns identical
values for `incl_latent_state = TRUE` and `FALSE`: the mean absolute
difference is exactly zero over 20 draws and 300 rows.

The behaviour turns out to be right and the documentation wrong.
`predict_*` reads no time index, and marginalises over the trend. A
latent state reaches a prediction as the envelope that
`sample_process_errors()` adds, and one draw at a time only through
`extract_trend_latent_states()`, which the hindcast and forecast
paths call. The standard kernel treats the argument the same way, so
the two grains agree with each other.

What was wrong was the roxygen on `compose_by_lv_trend_linpred()`. It
described two return modes in detail and promised that `TRUE` adds
`sum_k Z * lv_trend`. A comment nearby claimed the branch hands over
to the conditional path. It never does. Both are corrected in
`R/predictions.R`. The assertion now states the real contract: the
two agree here, and the conditional state is reached by the route
that owns it.

**2. FIXED. A factor-grain smooth could not be evaluated or drawn.**

Seen on two fixtures, so this is not particular to one route. One is
`test-by-lv-axis-cached-fits.R`, a gaussian fit built through
`mvgam()` with `ZMVN()`. The other is `jsdgam_mv_occ.R`, an occupancy
fit built through `jsdgam()`. In both, `smooths(fit)`
names the smooth and passing that name straight back to
`posterior_smooths()` reaches brms with a frame that carries no
`.trend` column:

```
The following variables can neither be found in 'data' nor in 'data2':
'.trend'
```

Root cause: both places that needed a frame reached for the
observation one. A smooth was evaluated over it and drawn over it.
`mvgam_smooth_eta()` defaulted `newdata` to `object$data`, and
`build_smooth_grid()` took `mf <- x$data`. The trend side runs on its
own grid, which under `by = lv_axis()` is one row per
(time, latent factor) and carries the `.trend` factor the smooth is
split by; the observation frame has neither. So `posterior_smooths()`
was refused by brms for a variable it could not find, and
`conditional_smooths()` built a grid with no rows.

The second symptom was hidden. `conditional_smooths()` sits on the
line after `posterior_smooths()` in the fixture files, so it never ran
while that errored. The test asserted only that its result was not
`NULL`. A result with the right name and an empty frame
passes that.

Fixed in `R/posterior_smooths.mvgam.R` by `mvgam_side_data()`. It answers
which frame a side was fitted on in one place, and both call sites
read it. The observation side keeps `object$data`, which is
its own frame; only the trend side is redirected. Verified on
the three `by = lv_axis()` fixtures. `posterior_smooths()` returns one
column per trend-grid row. `conditional_smooths()` returns two
distinct curves, one per latent factor. Their intervals are ordered
and lie inside the covariate range. `tests/testthat/test-posterior-smooths.R` goes
from 82 passing with 2 errors to 86 passing.

The fixture assertions were rewritten at the same time, since the
ones that were there could not have caught either symptom.

## mvn()

**3. `forecast(type = "response")` refuses a family `hindcast()` draws.**

`jsdgam_mv_mvn.R`, "forecast is keyed by the species axis".
`hindcast(fit, type = "response")` returns draws; `forecast(fit,
newdata, type = "response")` raises "Posterior predictive sampling is
unavailable for family mvn". These are one quantity reached over the
training grid and over its extension, so exactly one of the two is
wrong. The assertion claims they agree and fails until they do, rather
than enshrining either answer.

**4. The post-fit methods treat `mvn` as an occupancy family.**

Same file. Two symptoms, one classification:

- `plot(fit, type = "residuals")` routes to
  `pp_check(type = "resid_vs_fitted")`, which then refuses the call:
  "not available for closure-unit family 'mvn'".
- `augment(fit)` raises "Closure-unit families require column 'cap' to
  be present in 'data'".

`mvn()` is a multivariate normal observation model. It has no closure
units and no detection process. `is_closure_unit_family()`
in `R/families.R:1339` is what these two paths gate on, and the
plotting method asks for a type its own family will reject.

**5. The mvn fixture samples poorly, and the recovery check now says so.**

Same file, "Psi recovers the simulated residual scale". Psi posterior
means come back at 0.556, 1.194, 0.448 and 0.475 against a truth of 0.5
throughout, putting species 2 out by 0.69. The same run reports 130
divergent transitions in 1000 (13 per cent). The residual correlation
still recovers well (cor 0.947), which is why this went unremarked
while the file only printed its numbers: a correlation between
off-diagonals is insensitive to a species scale that has drifted.

Whether the threshold or the fixture wants changing is a judgment for
the mvn work rather than the axis work. It is recorded here because a
posterior with 13 per cent divergences will not support the recovery
numbers the file reports off it.

## CAR()

**6. A factor CAR is refused by two routes and granted by a third.**

`test-car-irregular-time.R`, "a CAR asked for fewer factors than
series gets n_series". A continuous-time trend evolves per series and
has no factor decomposition, which the trend registry records against
the type. Two of the three ways to ask for one meet that refusal:

    mvgam(trend_map = matrix(NA, 3, 2), trend_formula = ~ CAR())
    jsdgam(factor_formula = ~ -1 + CAR(), n_lv = 2)

both raise "CAR trends do not support factor models (n_lv <
n_series)". The third does not. Written as

    mvgam(trend_formula = ~ CAR(), n_lv = 2)

the model is accepted. `N_lv_trend` then comes back as 3, the series
count. A
user who asked for two latent factors is handed a saturated trend and
told nothing. Nothing in the fitted object records that the request
was raised, so the only way to notice is to read `N_lv_trend` back and
compare it against what was asked for.

All three routes are asserted against the same refusal, so the test
fails on the third until it behaves like the other two. The first version of this test pinned it to what it currently does.
That version passed while the defect stood, which is no use to
anyone.

**7. One post-fit method guards against a prefit. Seventeen do not.**

`test-prefit-guard.R`. `run_model = FALSE` returns an object of class
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

`test-var-trend.R`, "irf and fevd name the series, not Process_k". Both
tables come back keyed by strings of the form `Process_1 -> Process_2`
on a fit whose series are `willow`, `ash` and `rowan`. The mapping from
`Process_2` to a series is positional and appears nowhere in the
output.

This is the failure the axis work exists to end, in its mildest form:
the numbers are right and the reader cannot tell which series they
belong to. An impulse response is read to decide which series drives
which, so a label nobody can resolve makes the whole table
unusable without knowing the internal ordering.

Every other post-fit table on the same fit is labelled properly.
`residual_cor()` carries the series names on both margins, and the
hindcast and forecast arms are named. These two are the exception.

## PW()

**9. The logistic-growth refusal names a remedy that does not work.**

`test-pw-trend.R`, "the cap the refusal names as sufficient is
sufficient". Asking for `PW(growth = "logistic")` without a carrying
capacity raises

    Logistic growth models require a cap variable.
    Either provide cap argument or ensure 'cap' column exists in data.
    Example: PW(cap = carrying_capacity, growth = 'logistic')

Adding a column called `cap` to the data does not satisfy it. The same
error is raised again, so a user following the second clause is sent
in a circle. Only `PW(cap = <column>)` builds, which is what the
example line shows and what the first clause says.

Either the column route should work, or the message should stop
offering it. The test asserts the message's own promise, so it fails
until the two agree.

## Gaps closed rather than found

Two things the plan names as untested now have coverage, and the
package passes both.

The axis record carries the user's own time values rather than their
ranks. `test-by-lv-axis-cached-fits.R` numbers its occasions from 3 and
`jsdgam_mv_mvn.R` numbers its sites from 3, so a function returning the
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

## Test defects fixed along the way

These are faults in the fixture files themselves, so they were
repaired in place.

`jsdgam_mv_occ.R`, "each row reads the latent cell the sampler drew
for it", passed the frame the simulation built instead of the one the
fit kept. `unit = site` makes mvgam synthesise its own `time` column,
so the call raised on a missing `time` rather than comparing anything.
The check that carries the most weight in the file was erroring in
place of running, and the surrounding tests already used
`fit$obs_data` for exactly this reason.

## Six local fits still worth adding

Chosen against the defect classes above, which show which mistakes
this package actually makes, and against the parts of the plan that
still have no fit behind them. Each is one self-contained file, built
in the shape the recent files use: simulate, fit, then assert.

Three of the defects found are `jsdgam` faults rather than trend
faults, and they shape what these fits have to check. Finding 4 has
`mvn` classified as a closure-unit family, which is a question about
the family table and not about that one fit. The marginaleffects fault
returns a predictive median where the expectation was asked for, which
is family-independent and was reproduced on three separate fixtures.
Finding 3 has `forecast()` and `hindcast()` disagreeing about whether
`mvn` can be drawn from at all. All three are properties of a family,
so a fit that spans several families in one model is the efficient
place to pin them, which is why the first sketch below carries that
weight rather than being only about the wide frame.

### 1. A wide `mvbf()` frame, with a different family per response

    mvbf(count ~ x, presence ~ x, gauge ~ x)
    families: poisson(), bernoulli(), gaussian()
    trend_formula = ~ VAR(cor = TRUE)

The plan opens with this defect: a wide frame cut into a row-block per
response, so each response owned a stretch of the timeline instead of a
series. Nothing among the recent files covers it. A wide frame is the
one shape where the series is a property of the (row, response) pair
rather than of the row, so `axis_row_series()` answers `NULL` here by
design and the response axis is read instead. That branch has no fit
exercising it.

What it should assert: the record's series source is the response
keying rather than a column, and each response's `train_observations`
come from its own column instead of a stretch of rows. A forecast has
to cover every response at every occasion the frame supplies.
`posterior_predict` has to respect each response's own family. The
bernoulli arm should land in `{0, 1}`, which the gaussian arm will
not. The row-block defect puts one response's observations against
another's occasions, and every shape check passes on it.

It is also the cheapest place to close three `jsdgam` defects at once,
because one fit here holds several families side by side.

Finding 4 becomes a table check rather than an anecdote. Assert that
`is_closure_unit_family()` answers `TRUE` for `occ()` and `nmix()` and
`FALSE` for every other family the package offers, `mvn()` included.
That is one assertion over the family registry, and it catches the
whole class rather than the one member that happened to be noticed.
Then assert what the misclassification broke: `augment()` returns a
frame without demanding a `cap` column, and `plot(type = "residuals")`
renders instead of routing to a `pp_check` type its own family
refuses.

Finding 3 becomes a per-response claim. For each response, `forecast()`
and `hindcast()` at `type = "response"` have to agree about whether
drawing is possible. One refusing while the other returns draws is the
disagreement recorded there, and a frame with three families makes it
three checks for the price of one fit.

The marginaleffects fault is the same shape. `predictions(type =
"response")` has to equal `colMeans(posterior_epred())` for every
response, and on the poisson and bernoulli arms the estimates must not
all be whole numbers, which is what a predictive median returns.

A fixture named `val_mvgam_mv_multiseries.rds` already exists with no
file reading it.

### 2. A hierarchical VAR

    trend_formula = ~ VAR(gr = region, subgr = species, cor = TRUE)

Two structures meet here that are only tested apart. `A_trend` comes
back as `[N_groups, N_subgroups, N_subgroups]`, so the transition
matrix is per group over subgroups rather than over all series, which
is the same claim the hierarchical file already makes about the
correlation block and has never made about `A`.

It is also the worst case for finding 8. `irf()` labels its shocks
`Process_k`, and on a derived `gr` / `subgr` axis a reader has no way
back to a series at all: there is no column to compare against.

Fit it twice, once on a frame carrying a superseded `series` column and
once on a frame with no series column whatever. The plan records that
such a frame was refused at four separate layers, and no fit currently
starts from one.

### 3. A `trend_map` that fixes some loadings

    trend_map: a matrix mixing fixed values with NA cells

The plan's step 1 says `trend_map` has no coverage and calls it the
only path where `Z` reaches Stan as data rather than as a parameter.
That makes it the only place the loadings' row order can be checked on
values instead of on declaration text. `R/validations.R:1084` reorders
the map by the series levels, and whether those are the levels
`obs_trend_series` indexes decides whether every series loads on the
right factor.

The by-lv file passes a map of all NA cells. That leaves `Z` free, so
it never reaches this path.

Assert that `Z` appears in `standata` at `[n_series, n_lv]` and that
the fixed cells arrive at the positions the user wrote them. Then
permute the map's rows and require a different `Z`, since a fit that
merely relabelled the same matrix would load every series on another's
factor.

### 4. A univariate CAR carrying trend covariates, scored by `lfo_cv()`

    single series, irregular times, trend_formula = ~ s(temp) + CAR()

`test-car-irregular-time.R` establishes that a multivariate CAR refuses
every trend covariate, so the covariate path for this trend is only
reachable with one series. That case is asserted to build and is never
fitted.

It is also the natural home for `lfo_cv()`, which the plan singles out
as the case separating the two axis questions. Its evaluation frame
reaches past the training grid, so its time grid is the union and not
the fit's, while its series split is the fit's. Re-pointing the time
read there once produced a grid one short of the frame. Nothing in the
recent files calls `lfo_cv()` at all.

### 5. The two `AR()` arguments nothing exercises

    AR(p = 2, coef_sharing = ..., df = 4)

`coef_sharing` appears in no local file anywhere. `df` appears only in
files about heavy-tailed observation families, never on the trend.

Finding 6 is the reason to care. A CAR asked for fewer factors than
series is accepted and silently given `n_series`, so an argument that
is read and dropped is a mistake this package demonstrably makes. Both
of these are prime candidates, and the test that settles it is the
contrast the ARMA file uses for `ma`: build with and without, and
require the programs to differ by the machinery the argument names.

Student-t innovations also give the trend a tail, so a fit with `df`
small should absorb an outlying occasion into the innovation rather
than into the level, which is a value claim and not a shape one.

### 6. Two fits compared, and one refitted

    score(), ensemble(), loo_compare(), update()

No file written in this pass calls `score()`, `ensemble()` or
`update()`. The plan lists them as untouched, and they still are.

`update()` matters most. A refit rebuilds the object, so it is exactly
where an axis can be resolved a second time and disagree with the
first. Fit a model, update it with a changed prior or an added term,
then assert the series levels, the time values and the recorded grain
come back identical, and that a prediction from the updated fit still
reads the cell its own `obs_trend_series` names.

`score()` and `ensemble()` need two fits over one frame, which also
gives `loo_compare()` something to rank. Assert the scores are keyed by
the series axis, since a per-series score under permuted names is the
same defect as finding 8 in a place a user is more likely to act on.


## Existing local files that assert nothing

Separate from the fits above. Forty-three files in `tests/local/`
carry fewer than a dozen expectations. Twenty-eight hold no
`test_that()` block at all. They fall into four groups, and only the
first is fine as it stands.

**Artefact builders, correctly assertion-free.** `var_vignette_fits.R`,
`hierarchical_var_vignette_fits.R`, `forecast_eval_vignette_fits.R`,
`jsdgam_vignette_fits.R`, `mvbf_vignette_fits.R` and
`test-methods-md-pdf-gallery.R` exist to write caches and figures for
articles. Several download data. Assertions do not belong in them.

**The sweep the plan treats as a gate.** `postfit_sweep.R` is 996
lines with no `test_that()` in it. The plan's verification section
requires it green "including the two invariants that compare what
post-fit derives against what `standata` recorded", but green is not a
state this file can be in: it prints and returns. Whatever those
invariants are, nothing enforces them. This is the single largest
gap in the directory, and the plan's verification relies on it.

**Concordance files that compare and never check.** These compare
mvgam against another implementation, which is the one thing they
exist for, and then assert nothing about the comparison:
`brms_concordance_diri.R` (261 lines), `jsdgam_gllvm_concordance.R`,
`jsdgam_hmsc_concordance.R`, `jsdgam_hmsc_trait_concordance.R`,
`jsdgam_spoccupancy_concordance.R` and
`jsdgam_multi_season_concordance.R` (515 lines). A concordance that
silently degrades is worse than none, because the file's existence
implies the check is being made. Each needs its agreement threshold
written down as an expectation.

**Fits that only prove they ran.** `jsdgam_prediction_audit.R` is
named for an audit and performs none. `zmvn_irregular_time.R` covers
ZMVN on an irregular grid, which is the structure
`test-car-irregular-time.R` shows is worth real assertions.
`kfold_grouped_cv.R` is the only file touching grouped k-fold.
`smoke_check_apis.R` is 323 lines of API surface with nothing checked.
The remaining smoke files (`diri_smoke_fit.R`, `mvn_smoke_fit.R`,
`mvt_smoke_fit.R`, `jsdgam_nmix_smoke.R`, `jsdgam_mgp_mvn_smoke.R`,
`mvn_preflight.R`) each fit a model and print.

`mvn_preflight.R` and `mvn_smoke_fit.R` are worth doing first among
those, since finding 4 lives in exactly the family they cover and
neither would have caught it.
