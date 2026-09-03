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

A third symptom shows where the misclassification leads.
`hindcast(fit, type = "latent_state")` on an mvn fit answers with a
message written for whoever maintains the package:

    Closure-unit dispatch missing for family 'mvn' method
    'latent_state'. Add a 'mvn = switch(method_kind, ...)' branch to
    dispatch_closure_unit_method() in R/families.R.

The same call on a beta fit is refused properly, naming the family,
the types it does offer and the families that would enable the one
asked for. So mvn enters closure-unit dispatch, finds no branch, and
hands the user an instruction to edit the source.

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
`test-var-trend.R` covers `group` and states why it stops there.

A lifecycle notice is also raised once per session, so an
`expect_warning()` on it passes or fails on what ran before the file
rather than on anything the package did.

## Test defects fixed along the way

**A vacuous assertion that became false the moment it was tested.**
`test-hierarchical-trends.R` claimed the derived series identifier
was "lexically ordered" and checked it with

    expect_equal(levels(vals), sort(levels(vals)))

The fixture it ran against named its regions `r1`, `r2` and its
species `sp1`, `sp2`, `sp3`, so the levels were already alphabetical
and `sort()` was the identity. The expectation held without
constraining anything. Refitting the file on regions `south`, `north`
and species `sp_c`, `sp_a`, `sp_b` shows the real contract is the
order each column declares, grouping first, a group's subgroups
adjacent, which `test-axis-ordering.R` states for its `hier3` cell
and which the fitted axis confirms. The assertion now says that, and
adds that the order is not the sorted one, so a frame whose levels
happen to sort correctly can no longer satisfy it.

**Six suppressed Pareto-k warnings.** `suppressWarnings(loo(fit))`
appeared in every file written in this pass. `loo()` warns when a
Pareto-k crosses its threshold, which is the one diagnostic saying
whether the approximation can be trusted; suppressing it discards
exactly the signal a test exists to catch. Each is now captured and
turned into claims: the estimate is finite, every k is below one, and
any warning raised has to be the k notice those numbers account for,
so an unrelated warning fails rather than passing unseen.

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
`com_binomial_fitting.R` fits the same family with no trend and
satisfies `all(k < 1)` on the same assertion, which is what makes the
table above read as a property of the trend.

It is recorded because it bears on how `loo()` should be read on
these models, and because two of the three published comparisons in
the package rank trend models by `elpd_loo`. `lfo_cv()` is the tool
that answers the question these fits are being asked, and nothing in
`loo()`'s output on a trend fit says so.

**12. WITHDRAWN. `type = "response"` is the outcome scale, and was
behaving correctly.**

Recorded here as a claim that `predictions(type = "response")`
handed back a predictive median where an expectation was asked for,
seen on three fixtures and then a fourth. It does return a predictive
median, and that is the documented contract: `?forecast.mvgam` says
`"response"` samples from the observation family while `"expected"`
returns the family's mean. Checked across the three types on one
poisson fit:

| call | value |
|---|---|
| `predictions(type = "expected")` | 14.202 |
| `colMeans(posterior_epred())` | 14.213 |
| `predictions(type = "response")` | 14.000 |
| `median(posterior_predict())` | 14.000 |
| `predictions(type = "link")` | 2.653 = log(14.2) |

Every type answers with what it names. The assertion was what was
wrong: it compared `"response"` against `posterior_epred()`, which
are different quantities, so a whole number read as a symptom when it
was the contract. The assumption came from brms and marginaleffects,
which both spell the expectation `"response"`. mvgam parts company
from them deliberately by carrying a separate `"expected"`.

The tests now pin all three types to their own meanings, so a type
that quietly answered with another's quantity fails.

## Hierarchical trends

**13. `forecast()` fails outright when the frame carries a superseded
`series` column.**

`test-hierarchical-trends.R`, "a hierarchical fit forecasts on its
own axis". A `gr` / `subgr` model derives its own series identifier,
and that spelling differs from any `series` column the frame also
holds. The grouping gives `south_sp_c`, joined by an underscore and
ordered region first. `interaction()` gives `south.sp_c`, joined by
a dot and ordered species first. So the two disagree on the
separator and on the order.

`build_forecast_arms()` cuts the training tail by the raw column, so
nothing matches a derived label, and an empty frame reaches
`get_observation_structure()`:

    Assertion on 'newdata' failed: Must have at least 1 rows,
    but has 0 rows.

Isolated by removing the column from the fit and repeating the
identical call, which succeeds. The failure therefore follows the
column's presence and has nothing to do with the newdata: any
hierarchical model whose frame happens to carry a `series` column
cannot be forecast. It at least stops rather than returning a
number.

## Families

**4a. Three families are classified as closure-unit, not one.**

`test-mvbf-wide.R`, "only the closure-unit families are classified as
such". Finding 4 recorded `mvn()` reaching an occupancy-only code
path. Asked of the family table rather than of one fit,
`is_closure_unit_family()` answers `TRUE` for `mvn()`, `mvt()` and
`diri()` as well as for `occ()` and `nmix()`.

None of the three models a detection process over repeat visits to a
closed unit. `mvn()` and `mvt()` are multivariate observation models
and `diri()` is a composition, so every path this predicate guards is
reached by three families it was never written for. That is what
sends an `mvn` fit into a `pp_check` type its own family refuses and
makes `augment()` demand a `cap` column with no meaning for it.

The assertion asks the registry directly, so it covers every family
at once.

**14. `marginaleffects` does not know mvgam accepts `resp`.**

Same block. Naming a response raises

    These arguments are not known to be supported for models of
    class `mvgam`: resp.

marginaleffects keeps a whitelist per model class and mvgam has not
registered `resp` on it, so every user of a multivariate fit meets
this on every call that names an arm. The argument is forwarded and
honoured; only the notice is wrong.

## Multivariate log_lik

**15. The joint density drops every response at an occasion where any
one of them is missing.**

`test-mvbf-wide.R`, "log_lik is per response, and the joint is their
sum". The frame gives each of three responses its own gaps: three
occasions for `count`, two for `seen`, five for `mass`, disjoint, so
no occasion is missing from all three.

`log_lik(fit)` nonetheless returns ten columns of `NA`, which is the
union of the three gap sets. An occasion where `mass` was not
recorded loses the `count` and `seen` densities along with it. Summed
over draws the joint reads -302.6 where the arms add to -330.3.

`loo()` is computed from these numbers, so a wide fit whose responses
were not all measured on the same occasions is being compared on a
likelihood that omits observations it holds. The disjoint gaps are
what make it visible: a frame whose responses go missing together
gives the same answer either way.

## trend_map

**16. A matrix `trend_map` ignores its rownames, then writes the
declared ones over them.**

`test-trend-map-fit.R`, "a matrix map keys its rows by the names the
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

**17. The factor plot draws the occasion's rank where every other
plot draws its time.**

`test-trend-map-fit.R`, "every plot draws the occasions the user
supplied". The fit is numbered from three, so its occasions run 3 to
52 and their ranks run 1 to 50. Measured off the built plots:

| call | x range |
|---|---|
| `plot(type = "factors")` | 1 to 50 |
| `plot(type = "trend")` | 3 to 52 |
| `plot(type = "series")` | 3 to 52 |
| `plot(hindcast(fit), series = 1)` | 3 to 52 |

The axis is labelled `Time` in all four. Three of them mean it.

A reader comparing a factor trajectory against a series trajectory,
or against anything dated, is off by the offset between the two
numberings, and on a frame that starts at one the two coincide and
nothing shows. Reading the rendered plot is what turned it up, so
the check now measures the x values the plot draws.

Reproduced on the `jsdgam()` route as well, so this belongs to the
plotting method rather than to one trend. The multivariate-normal
fixture numbers its sites from 3, giving occasions 3 to 32:

| call | x range |
|---|---|
| `plot(type = "factors")` | 1 to 30 |
| `plot(type = "trend")` | 3 to 32 |
| `plot(type = "series")` | 3 to 32 |
| `plot(hindcast(fit), series = 1)` | 3 to 32 |

`test-jsdgam-families.R` measures the axis for every family, so the
one frame numbered from three carries the failure and the six
numbered from one pass.

## Closure-unit families

**18. Every `occ()` and `nmix()` compile prints a Stan warning about
integer division.**

mvgam writes its own threading block for closure units, and the
grainsize line in it reads

    int grainsize = N_unit >= 8 ? N_unit / 8 : 1;

`grainsize` is an `int`, so rounding is what is wanted, but `/`
between two integers makes stanc say so at every compile:

    Found int division:
        N_unit / 8
    Values will be rounded towards zero. If rounding is not desired
    you can write the division as N_unit / 8.0
    If rounding is intended please use the integer division operator
    %/%.

The arithmetic is right and the notice is cosmetic, but it reaches
every user who fits a closure-unit model, and it is the kind of
notice that teaches people to read past compiler output. `%/%` says
what the line means and silences it.

**19. `latent_N_saturation()` names its units by index.**

`test-occ-closure-units.R`, "the saturation table names its units".
The table comes back with a `label` column reading `1_1`, `1_2`,
`1_3`, which is the series index joined to the occasion index. The
units it describes are `site_01` at times 3, 4 and 5.

So a reader has no way from the table back to a site or a date, and
the mapping is positional and undocumented. It is the fault behind
finding 8 in a place a reader is more likely to act on: a saturated
unit is one whose latent state is pinned at the ceiling, which is
what says the survey effort there was insufficient. Nobody can go
back and revisit a unit called `12_2`.

Every other per-unit surface on the same fit is keyed properly, and
the frame carries both columns the label would need.

## Distributional parameters

**20. FIXED. `conditional_effects()` offered no covariate that
belonged to a distributional parameter.**

`detect_conditional_effects()` reached into `$pforms`, where a
parameter's own formula lives, only when the model was non-linear. A
distributional model puts one there too, so on
`bf(y ~ x, hu ~ z)` the default term list was
`x` alone and `z` appeared nowhere. What a user saw was a plot of the
mean's covariate with no sign that a second covariate existed.
Naming it as `effects = "z"` worked throughout, so the panel was
never unreachable, only never offered.

brms builds its default list from the whole formula:
`get_all_effects(brmsterms(bf(y ~ x, hu ~ z, hurdle_poisson())))`
returns `x` and `z`. mvgam now returns that same pair. Terms come
from `$pforms` in either case, and parameter names are pruned from
the result only when the model is non-linear, since the RHS above
them then holds names rather than data. The multivariate branch took
the same fix, because a response may carry a formula of its own.

`test-mixture-family-density.R` covers it, and checks the panel
against `posterior_epred()` at the same grid point rather than
against its shape, so a panel drawn on the wrong predictor fails
instead of merely looking odd.


## Residuals

**21. A continuous family's quantile residuals carry no posterior
spread.**

`residuals()` defaults to `type = "quantile"`. For the families with
an analytic CDF -- gaussian, student, lognormal, Gamma, beta --
`quantile_family_specs` evaluates it per draw, so the residual moves
with the draw's own parameters. Every other family falls through to
`compute_quantile_residuals_empirical()`, which pools `yrep` over all
draws to form one `lower`/`upper` per observation and then, where the
two coincide, repeats `qnorm(lower[i])` across every row.

For a continuous response there are no ties, so they always coincide.
On the tweedie fit 54 of 60 columns come back constant: the 6 that
vary are the zero rows, where the atom creates ties. `Est.Error` is
then 0 and `Q2.5 == Estimate == Q97.5` for nine observations in ten.
`type = "ordinary"` is unaffected (0 of 60), as are hurdle and
zero-inflated Poisson (0 of 80), whose discreteness supplies ties.

The DHARMa formulation is behaving as written, since an empirical PIT
is one number for each observation. But the roxygen promises that the
matrix returned for each draw "carries the full posterior uncertainty
in the residual distribution", and it is the documented input to
`DHARMa::createDHARMa()`. Either the analytic path grows a tweedie
entry, or the documentation says which families get spread. That is a
choice about what the residual means, so it is recorded rather than
taken.

`test-tweedie-family.R` now asserts the documented behaviour and
fails on it, so the sweep reports it rather than leaving it in this
file alone. The companion assertion on `type = "ordinary"` passes,
which places the fault in the quantile path and not in the fit.

Measured again across the jsdgam families, the reach is wider than
tweedie and, where it bites, total:

| family | `type = "quantile"` | `type = "ordinary"` |
|---|---|---|
| mvn | 120 / 120 constant | 0 / 120 |
| mvt | 120 / 120 constant | 0 / 120 |
| diri | 120 / 120 constant | 0 / 120 |
| nb | 40 / 300 constant | 4 / 300 |
| beta | 0 / 300 constant | 0 / 300 |

Beta escapes through its analytic entry, the discrete families are
partly rescued by ties, and the continuous families without an entry
lose every column. Three of the seven return residuals carrying no
uncertainty at all while their ordinary residuals carry full spread,
which places the fault in the quantile path rather than in any fit.
`test-jsdgam-families.R` asserts the documented behaviour for each
family that reaches `residuals()` and fails on three of them.

## pp_check diagnostics

**24. `resid_vs_fitted` plots a conditional residual against a
marginal fitted value.**

The two axes of the panel come from different surfaces, and each
matches its own exactly. On a poisson AR(1) fit, `draw_ids = 1:50`:

| axis | is | error | other surface |
|---|---|---|---|
| `resids` | median of conditional residual draws | 0 | 7.97 |
| `preds` | median of marginal `posterior_epred` | 0 | 33.36 |

`pp_check()` replaces a `NULL` `newdata` with
`mvgam_training_data(object)` early on. `diagnostic_surface_args()`
adds `incl_autocor = TRUE` only when `newdata` is `NULL`. By the time
the fitted values are drawn, a call on the training data therefore
looks like a call on new data, and `posterior_epred()` keeps its own
default of `FALSE`. The residual draws are built before that and stay
conditional. The comment above the call says the fitted values "name
their surface through the same helper the residuals above them used,
so one panel plots one picture of the fit"; they do not.

What it costs is the plot. The fitted axis spans 7.44 to 10.52 while
the outcome spans 0 to 48 and the conditional fitted values span 1.83
to 42.97. The panel is read for structure across the range of
the fit, and every point is compressed into a band that sits nowhere
near where the model predicts. `mvgam_resid_panel()` shows the same
axis, since it calls this type.

Covered in `test-pp-check-resids.R`, which asserts the two axes read
one surface and fails on it.

## CAR on a continuous time grid

**25. A CAR model whose times are not whole numbers cannot be
forecast.**

`build_training_arms()` records the training times truncated to
integers while the data frame keeps the values the user supplied.
On `sim_mvgam(type = 6)`, which draws cumulative `Unif(1, 6)` gaps,
the two disagree on 89 of 90 occasions:

| source | first four times |
|---|---|
| `training$times[[lv]]` | 0, 4, 7, 11 |
| `training$data$time` | 0, 4.590626, 7.072973, 11.73063 |

Measured, `times == floor(data)` exactly. `build_training_tail_data()`
then selects the tail with `data[[time_var]] %in% tail_ts`, matches
nothing, and hands back a frame of no rows;
`get_observation_structure()` asserts at least one row and stops with
"Must have at least 1 rows, but has 0 rows".

So `forecast()` raises on every CAR fit with a continuous time grid,
which is the case CAR exists for. `mvgam()` itself is unaffected: the
model fits, and only the forecast arm fails. Reproduced on seed 501
with 90 training rows over times 0 to 330.13 and 30 test rows over
335.84 to 439.71, a well-formed forward forecast with no overlap.

One axis derived twice is the class this plan is written against.
Here it stops rather than returning a wrong number, so nothing
silently depends on the answer.

`test-forecast-recovery.R` covers it. The three CAR seeds error there
until the axis carries the times it was given. The failure was
invisible before because the bundles were cached from a run that
predates it, so `forecast()` was never called again.

## Leave-future-out cross-validation

**26. `lfo_cv()` cannot compute a forecast-based score.**

`score = "elpd"` runs; `score = "crps"` errors, alone or beside
`elpd`, on the same fit and the same window:

```
'newdata' must continue the training series for a 'AR' trend.
Series 'series_1' was observed to time 30, so the next 1 times are
31 to 31; got 32 to 32.
```

The elpd path reads the density directly, while a proper score needs
a forecast, and the frame `scores_at_window()` builds for it starts
one occasion late. The refusal it trips is the one added for users
who hand `forecast()` a gapped frame, so the guard is working and the
caller is at fault. Reproduced at `min_t = 30` on a Poisson AR(1) and
at `min_t = 28` on a two-series AR(2), skipping one occasion each
time.

Every documented multi-score example is therefore unavailable, and
`elpd` is the only rule `lfo_cv()` can currently report.

**27. The threshold `lfo_cv()` reports is not the one it used.**

`mvgam_lfo` carries both `pareto_k_threshold` and
`pareto_k_threshold_used`. The first is `NULL`; the second held
0.6970642 on a 800-draw fit, a threshold that moves with the number
of draws rather than the nominal 0.7. The field a reader reaches for
is the empty one, and `summary()` reports the run from the other.

Both are covered in `test-forecast-recovery.R`, which asserts the
documented behaviour and fails on it.

## Gaps closed rather than found

Two things the plan names as untested now have coverage, and the
package passes both. Three families that had none now have it too.

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

Tweedie is now fitted. It was the only exported family nothing in
`tests/local` ever fitted, and the only one carrying its own Stan
functions through `attr(family, "mvgam_stanvars")`.
`test-tweedie-family.R` reaches `P(Y = 0) = exp(-mu^(2-p)/(phi(2-p)))`
three ways -- the closed form, `exp(log_lik())` at the zero rows, and
the fraction of zeros among the draws -- and all three agree, the
first two exactly. Checking the density itself would have been
circular, since mvgam's post-fit `log_lik` calls `mgcv::ldTweedie`.
The prefit carries `M` as data, so `standata()$M` is 30 or 40 as
asked while the code is byte-identical.

Hurdle and zero-inflated Poisson are covered the same way, each
against its own closed form and against the other's, so neither
mixture can stand in for the other.

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

## The jsdgam family sweep

Found while folding the nine `jsdgam_mv_*.R` files into one
battery. Each was reached by driving a surface the nine files
called but never checked the value of.

**28. `forecast(type = "expected")` returns the link scale on every
softmax family.**

`test-jsdgam-families.R`, "forecast is keyed by the species axis".
The expectation of a composition is a probability, and over the
training grid `posterior_epred()` returns one. Over the extension
of that grid it does not:

| family | `posterior_epred()` | `forecast(type = "expected")` | `type = "link"` |
|---|---|---|---|
| categ | 0.000 to 0.994 | -6.192 to 11.056 | -14.342 to 11.986 |
| diri | 0.000 to 0.985 | -7.008 to 19.721 | -8.888 to 9.033 |
| multi | 0.013 to 63.119 | -10.418 to 9.174 | -7.431 to 6.474 |
| beta | 0.072 to 0.913 | 0.039 to 0.961 | -4.435 to 3.260 |
| nb | 0.036 to 53.068 | 0.008 to 84.165 | -3.532 to 5.557 |

Beta and the negative binomial answer correctly, so this is not the
forecast arm in general: it is the three families whose inverse link
needs the shared softmax normaliser. The returned values are not the
link arm repeated either, so something is applied per species
without the normaliser rather than nothing being applied at all.

A multinomial expectation coming back negative is the clearest of
the three, since no normalisation convention makes an expected count
below zero.

This is one quantity reached two ways, and the two disagree. It went
unseen because the forecast test in all nine files asserted only the
arm names, the dimensions and `is.finite()`. A link-scale value
satisfies every one of those. The assertion now compares the arm
against the scale `posterior_epred()` occupies for that family.

**29. `tidy()` reports what the diagnostics hide and omits what they
expose.**

`test-jsdgam-families.R`, "the draws and the tidiers keep this fit's
row order". On every jsdgam checked, `variables()`,
`posterior_summary()` and `rhat()` agree on what a reader should
see. They hide the raw `Z[i,j]` block and `L_Omega_trend`, because a
factor model's loadings have no fixed value under rotation and
neither parameter means anything on its own. They expose `Z_tilde`,
which is the identified block. `tidy()` agrees with none of it.

    variables()          Z_tilde present, L_Omega_trend hidden
    posterior_summary()  Z_tilde present, L_Omega_trend hidden
    rhat()               Z_tilde present, L_Omega_trend hidden
    tidy()               Z_tilde absent,  L_Omega_trend present

So the tidy table carries four `L_Omega_trend` entries, typed
`trend_random_effect_group_level`, which is neither a random effect
nor a group level, and no loadings at all. `?tidy.mvgam` documents
`effects = "all"` as returning every parameter.

`tidy()` is also reading raw Stan names where the other three read
aliases: its terms include `b[1]` through `b[9]` alongside
`b_Intercept`, and `variables()` lists none of the bracketed ones.
One cause explains both: the tidier reads the stanfit directly
rather than through the filter and alias pass the other three go
through.

Reproduced identically on the beta, mvn and categ fits.

**30. `hindcast()` and `conditional_effects()` return a constant on
the composition families.**

`test-jsdgam-families.R`, "hindcast arms are the species, in order,
and distinct" and "pp_check, plotting and conditional_effects
render". Measured on the cached diri, categ and multi fits:

| call | diri | categ | multi |
|---|---|---|---|
| `hindcast()` arm means | 1, 1, 1, 1 | -- | -- |
| identical arm pairs | all six | -- | -- |
| `conditional_effects()` estimate | 1 | 1 | 0 |
| its `conf.low` / `conf.high` | 1 / 1 | 1 / 1 | 0 / 0 |

Every panel is a flat line at a constant with an interval of zero
width, across all three effects the model offers. The Dirichlet
hindcast hands back 1 for every species at every site. Four species
sharing a simplex average about a quarter each, and four arms that
agree exactly leave nothing for the composition to distribute.

`posterior_epred()` on the same fits is correct: it lands inside
[0, 1] and sums to one per site, which the family blocks below
already check. So the fault is in what the plotting and hindcast
arms are built from rather than in the fit.

The beta, negative binomial and multivariate normal fits draw
proper panels on the same code path, which is what makes this
specific to the shared softmax normaliser rather than general.

`jsdgam_mv_diri.R` already failed the identical-arms half before
this consolidation, at 6 failures and 91 warnings against 102
passing. The conditional-effects half was never asserted: the file
checked that each panel's intervals were ordered, and a constant
satisfies `conf.low <= estimate <= conf.high`.

**31. `loo()` reports a Pareto diagnostic for one row in K on a
composition.**

Same file. A composition's density is one number per site, spread
across the K rows that site occupies, so `loo()` comes back with
`n` equal to the row count and the other K-1 entries missing:

| family | rows | non-finite pareto_k | share |
|---|---|---|---|
| diri | 120 | 90 | 75% |
| multi | 120 | 90 | 75% |
| categ | 400 | 300 | 75% |

75 per cent is exactly (K-1)/K in each case. `loo()` also prints
"Replacing NAs in `r_eff` with 1s" and then reports an ELPD off the
remaining quarter without saying so.

Whether the density belongs at the site grain is a question for the
families. What is wrong either way is the presentation: the object
claims `n` observations, three quarters of its diagnostics cannot
be read, and the notice about it is about `r_eff` rather than about
the missing k values.

Distinct from finding 11, which is about the k values a trend fit
produces being too high to trust. Here they are absent.

**32. Three fixtures asked a composition a question it cannot
answer.**

The package is right here and four assertions were wrong, so this
belongs with the test defects below.

`jsdgam_mv_diri.R`, `jsdgam_mv_multi.R` and `jsdgam_mv_categ.R` each
asserted that `posterior_epred()` on a frame holding one species
matches the corresponding columns of the full-frame answer. It does
not, and it should not: these families share a softmax normaliser
across the species at a site, so a frame carrying one of them has a
different denominator. Measured, the disagreement is total -- up to
1.0 on a probability for categ and diri, and up to 20.5 on counts
for multi -- while beta, nb, mvn and mvt agree to zero.

The claim the fixtures were reaching for is real, and survives in a
form every family can answer: subset whole sites instead. Half the
sites, all species, agrees to exactly zero on all seven families,
and still fails on a prediction that places rows by position rather
than by content. `test-jsdgam-families.R` asks it that way.

**33. `residual_cor(partial = TRUE)` cannot run on a factor model.**

Found by calling it rather than by an assertion, so nothing in the
suite reported it. On every jsdgam checked -- beta, mvn and categ --
the call stops with a bare LAPACK message:

    system is computationally singular: reciprocal condition
    number = 1.14307e-17

A partial correlation is read off the inverse of the covariance, and
a rank-2 factor model over four or five species implies a covariance
of rank 2. It is singular by construction, so the inverse this asks
for does not exist for any factor fit at all, whatever the data.

`residual_cor(partial = TRUE)` is documented and works elsewhere:
the VAR fixture asserts it returns a `prec` block with a unit
diagonal. On a factor fit it can never work, and it reports that
through `solve()`. A reader gets a condition number where a sentence
would have told them partial correlations need a full-rank residual
covariance, and pointed them at `shared_variation()`, which answers
the question they were asking.

Either the factor path takes a pseudo-inverse or a ridge, or the
method refuses with an explanation. Choosing between those belongs
to the jsdm work, so `test-jsdgam-families.R` leaves it unasserted
and this entry carries it.
