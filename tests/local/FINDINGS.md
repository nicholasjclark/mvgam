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

**3. `forecast(type = "response")` refuses a family `hindcast()` draws.**

No file fits `mvn()` any more, so nothing covers this. The claim
belongs wherever that family is next fitted.
`hindcast(fit, type = "response")` returns draws; `forecast(fit,
newdata, type = "response")` raises "Posterior predictive sampling is
unavailable for family mvn". These are one quantity reached over the
training grid and over its extension, so exactly one of the two is
wrong. The assertion claims they agree and fails until they do, rather
than enshrining either answer.

**4. The post-fit methods treat `mvn` as an occupancy family.**

Seen on the same `mvn()` fit, which no file now holds. Two symptoms,
one classification:

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

**5. The mvn fixture samples poorly.**

On the `mvn()` fit no file now holds, "Psi recovers the simulated
residual scale". Psi posterior
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
reached by three families it was never written for. That is what
sends an `mvn` fit into a `pp_check` type its own family refuses and
makes `augment()` demand a `cap` column with no meaning for it.

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

## Multivariate log_lik

**15. The joint density drops every response at an occasion where any
one of them is missing.**

`test-grain-mvbf-wide.R`, "log_lik is per response, and the joint is their
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

`test-family-tweedie.R` now asserts the documented behaviour and
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
`test-family-jsdgam.R` asserts the documented behaviour for each
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

## The jsdgam family sweep

Found while folding the nine `jsdgam_mv_*.R` files into one
battery. Each was reached by driving a surface the nine files
called but never checked the value of.

**28. `forecast(type = "expected")` returns the link scale on every
softmax family.**

`test-family-jsdgam.R`, "forecast is keyed by the species axis".
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

**30. `hindcast()` and `conditional_effects()` return a constant on
the composition families.**

`test-family-jsdgam.R`, "hindcast arms are the species, in order,
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

The conditional-effects half is what an ordering check cannot see: a
constant satisfies `conf.low <= estimate <= conf.high`, so the panel
has to be required to move.

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

The Dirichlet, multinomial and categorical fits each
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
than by content. `test-family-jsdgam.R` asks it that way.

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
to the jsdm work, so `test-family-jsdgam.R` leaves it unasserted
and this entry carries it.

## Uncovered families

**43. Lognormal quantile residuals are NaN in part and off-scale in
the rest.**

Found by fitting the three exported families nothing in
`tests/local` fits: `student()`, `lognormal()` and
`beta_binomial()`. A lognormal AR(1) over 60 positive observations
gives, at 50 draws:

| quantity | value |
|---|---|
| NaN cells in `residuals(type = "quantile")` | 390 of 3000, 13 per cent |
| range of the cells that are finite | 5.84 to 8.13 |
| NA rows in the summarised table | 4 of 60 |
| `residuals(type = "ordinary")` | no NA at all |

Two things are wrong and only one of them is visible as a warning.
The warning is `In log(mu) : NaNs produced`, and nothing in the data
can produce it: the response runs 0.34 to 4.56 and
`posterior_epred()` runs 0.364 to 4.688, never reaching zero. So
whatever is being passed to `log()` is not the mean.

The second is the scale. A randomised quantile residual is
standard normal by construction, and the cells that survive sit
between six and eight standard deviations out. A reader
checking a lognormal fit would see a QQ-plot of impossible values
with a seventh of the points missing.

`student()` on the same data and the same call returns 3000 finite
cells, so this belongs to the lognormal entry rather than to the
quantile path in general. Both families are named in the analytic
list finding 21 describes, which is what makes the contrast
informative: the analytic route works for one and not the other.

`beta_binomial()` fits and answers on every surface driven here,
with one constant quantile-residual column of sixty, which is the
tie behaviour finding 21 already accounts for on discrete
families.

## Introspection

**40. `family()` answers "custom" on an occupancy fit while two
other methods answer "occ".**

`test-grain-closure-units.R`, "multi-season: the fit describes its own
specification". Asked of the same fit three ways:

| call | answer |
|---|---|
| `family(fit)$family` | `custom` |
| `summary(fit)` | `Family: occ` |
| `glance(fit)$family` | `occ` |

The fit with one season reproduces it, so this belongs to the
family and not to the path that seasons take. The gaussian VAR fixture
answers `gaussian` on the same method. `family()` is therefore not
broken at large, and what the two groups share is narrower and wider
than occupancy: `test-family-tweedie.R` answers `custom` for
`family(fit)$family` and `tweedie` for `glance()$family`, and tweedie
models no closure unit at all. `com_binomial()` and `diri()` do the
same. Every one of them is built through `brms::custom_family()`, which
is the route rather than the subject matter.

`family()` is the accessor other packages reach for, so this is the
one of the three that matters most. A custom brms family carries
`custom` as its `$family` element, and mvgam stores that object
whole, so the implementation detail reaches the surface where the name
should be. `glance()` reads the name mvgam recorded alongside it and
answers correctly, which is what says the name is on the object. It is
finding 4 seen from the other side: there a family was sorted into the
closure-unit group when it did not belong, and here a family will not
give its name at all.

**41. `getCall()` returns the function itself where a call names
it.**

Same block. `getCall(fit)[[1]]` is the `jsdgam` closure rather than
the symbol `jsdgam`, so `deparse(getCall(fit))` prints the whole of
`jsdgam`'s source in place of the call that made the fit. The rest
of the call is well formed: its names run `formula`,
`trend_formula`, `data`, `backend`, `family` and so on.

Seen on the single-season fit too. It reaches `mvgam()` fits
as well: the tweedie fixture's `getCall(fit)[[1]]` is the `mvgam` closure, so the
head position holds a function on both constructors rather than on
`jsdgam()` alone. `?getCall` describes the return as a call
`update()` can modify and re-evaluate. A user also prints it to see
how a fit was made. A closure there defeats both readings.

## The structured loadings prior

**36. Three recovery claims were aimed at a target no fit can hit,
and their thresholds had been lowered to match.**

The package is right here and the assertions were wrong, so this is
a test defect. It is recorded because the mistake is the one that
nearly shipped a wrong answer on the ZMVN fit, in a place where the
numbers looked reasonable rather than absurd.

`test-factor-loadings-prior.R` checked the recovered species covariance
against `Phi`, the kernel the loadings were drawn from. Reading the
generated Stan settles what `Phi` is:

```stan
target += multi_normal_cholesky_lpdf(Z[ : , i_z] | rep_vector(0.0,
                    N_series_trend), L_Phi_loadings);
```

Each column of `Z` gets that prior, so with three latent factors the
covariance a dataset carries is a Wishart around `Phi` on three
degrees of freedom, and not `Phi` itself. `residual_cor()` estimates the
former: it builds `Z * Latent * t(Z)` from the fitted loadings.

Drawing 2000 fresh `Z` from the same `Phi` measures what agreement
with `Phi` is even available:

| n_lv | mean cor(realised, Phi) |
|---|---|
| 3 | 0.399, sd 0.195, 5th pct 0.086, 95th pct 0.724 |
| 10 | 0.665 |
| 50 | 0.913 |
| 200 | 0.976 |

At the rank these fits use, the target moves with the simulation
seed over most of the unit interval and barely moves with the fit at
all. That is why the assertion could only be written as
`agree > 0.1`, and why the phylogeny check came down to a margin of
0.04 between 0.156 and 0.116.

Against the covariance the realised loadings imply, the same three
fits read:

| fit | family | pearson | spearman | middle 80% | permutation null |
|---|---|---|---|---|---|
| phylo | gaussian, 100 sites | 0.978 | 0.978 | 0.975 | max 0.166 |
| birds | bernoulli, 25 sites | 0.837 | 0.839 | 0.832 | max 0.169 |

So the loadings were being recovered nearly exactly while the file
reported it as barely distinguishable from nothing. Neither number
is carried by a few extreme pairs, and both sit more than five
standard deviations outside a 500-permutation null of the species
axis, which is what makes them a claim about which species loads on
which factor rather than about the spread of the numbers.

Two further pieces of the file were measuring themselves. The
premise check compared `Phi` against its own two kernels, which
restates how `Phi` was built and holds whatever data comes out of
it; it now checks that the realised draw kept the ordering, which at
rank 3 is not automatic. And the check on the wider prior asked that
one of two length-scales widen, which over two parameters is a coin
flip: measured, the phylogenetic scale's spread grows by 89 per cent
while the trait one's falls by 38, so the honest claim is that both
move.

A caution found alongside it. Averaging the raw `Z` draws and
comparing the covariance that implies gives 0.663 where the same
comparison through `residual_cor()` gives 0.978. Raw `Z` is
rotation-indeterminate, so its posterior mean is not a loadings
matrix and nothing should be computed from it. Two routes are invariant to rotation, the
QR-identified block and the covariance taken within each draw, and
they agree.

## by = lv_axis() smooths, drawn

**34. `plot(conditional_smooths())` runs the factors together into
one series.**

`test-grain-closure-units.R`, "the env smooth is drawn once per latent
factor". `s(env, by = lv_axis())` gives one curve per latent factor,
and `conditional_smooths()` returns them correctly blocked: 100 rows
at `cond__ = 1`, then 100 at `cond__ = 2`. The renderer ignores the
column. Read off the built plot:

    layers = 2 ; panels = 1 ; facet = FacetNull
    layer 1 rows = 200 ; distinct groups = 1
    aesthetic mapping: x

So both factors land in one group. The line runs the width of `env`,
returns to the left edge and runs it again, which draws as a sawtooth
across the whole panel rather than as two curves. The ribbon spans
both factors' uncertainty at every x, giving a band from -8 to +5
around estimates whose own range is -0.10 to 0.24.

`conditional_effects()` facets properly on the same fit, so this is
the smooth renderer rather than the plotting layer in general.
Finding 2 fixed `conditional_smooths()` returning a grid with no rows
in it; this is the half after that, where the grid is right and the
picture is not.

Reading the rendered plot is what turned it up. The assertion that
stood here asked only that the two curves differ, which they do.

**35. The second factor's smooth is identically zero.**

Same file and same call, on both the occupancy and the abundance
fit. Split by `cond__`:

| factor | n | sd(estimate) | estimate range | mean interval width |
|---|---|---|---|---|
| 1 | 100 | 0.0558 | -0.097 to 0.238 | 7.00 |
| 2 | 100 | 0 | 0.000 to 0.000 | 0.00 |

A smooth the data did not support would shrink toward zero and keep
its posterior width. This one has no width at all, which means no
draw moves it: the coefficients are not reaching the grid rows that
belong to the second factor.

The design itself is right. `Xs_trend` comes out block-complementary
across the two factors, and `Zs_2_1_trend` carries values on exactly
the rows `times_trend[, 2]` names and zeros everywhere else. The same
test asserts both. So the fault sits downstream of the design, where
the grid is evaluated.

The signature points at the evaluation reading factor 2's rows
against factor 1's column, since `X[r2, 1]` is zero by construction
and would return exactly this.

Recorded rather than fixed: which of the two grids is wrong is a
question for the smooth work. `test-grain-closure-units.R` now requires
each curve to move and to carry an interval, so a curve pinned at
zero fails instead of satisfying "the two curves differ".

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
accepted, and returns `[ndraws x nobs]`. Both families report
`family(fit)$family` as `"custom"`, which is finding 40, so the two
are told apart by something other than that string.

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

## Plotted output

**55. `plot(type = "trend")` orders its panels alphabetically while
every other per-series surface uses the model's order.**

Found by rendering the plots and looking at them. Two fits whose
series are declared out of alphabetical order:

| surface | arma, declared `kappa, beta` | var, declared `willow, ash, rowan` |
|---|---|---|
| `plot(type = "trend")` | beta, kappa | ash, rowan, willow |
| `plot(type = "series")` | kappa, beta | willow, ash, rowan |
| `hindcast()` arms | kappa, beta | willow, ash, rowan |
| `plot(hindcast(), series = 1)` | kappa | willow |

So the trend plot is the one that sorts. Placed beside the series
plot, which is the obvious comparison to make, its first panel holds
a different series, and both are labelled only by name so nothing on
either picture says the order changed. `series = 1` agrees with the
series plot and disagrees with the trend plot.

This is the plan's own class reaching the output a reader looks at
rather than a number they compute: one axis, two orders, every label
correct in isolation.

The hierarchical fit is the worst case, because its axis is derived
and cannot be recovered by sorting anything. Declared `south_sp_c,
south_sp_a, south_sp_b, north_sp_c, north_sp_a, north_sp_b`, the trend
panels come back `north_sp_a, north_sp_b, north_sp_c, south_sp_a,
south_sp_b, south_sp_c`. All six positions differ, so no panel in the
trend plot holds the series the series plot puts in the same place.

**56. `print()` shows an environment address and names an ARMA as an
AR.**

Two problems in the first thing a user calls. Printing any fit emits
the formula environments:

```
GAM observation formula:
y ~ gp(x1, x2, k = 8)
<environment: 0x6352aee840d0>
```

Two such lines per fit, on every fit checked, carrying a pointer that
changes between sessions and means nothing to a reader.

The same output reports the trend as

```
Trend model:
AR
```

on a fit whose call is `~ AR(p = 1, ma = TRUE)`. The lag order and
the moving-average term are both dropped, so the two models this file
exists to tell apart print identically. `summary()` reports
`theta1_trend` and does distinguish them, so the information is
available to the method that omits it.

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

## A wide fit describing itself

**65. `family()` reports one arm of three, and ignores `resp`.**

`test-grain-mvbf-wide.R`, "the fit reports the family of every arm it
was given". The fit carries a poisson, a bernoulli and a gaussian
response. Asked which family it has:

| call | answer |
|---|---|
| `family(fit)` | gaussian |
| `family(fit, resp = "count")` | gaussian |
| `family(fit, resp = "seen")` | gaussian |
| `glance(fit)$family` | poisson, bernoulli, gaussian |

`family.mvgam()` already holds the right behaviour and cannot reach
it. `R/print.mvgam.R:181` returns `object$family` when that is set,
and falls through to a branch that reads one family per response off
`object$formula$forms` otherwise. Its comment says a model written
with `brms::mvbf()` has no single family so none is stored. One is
stored. `fit$family` holds the gaussian, being the last arm, so the
branch written for this case is unreachable and the comment above it
describes something that does not happen.

`resp` is the second half. `family.mvgam(object, ...)` takes no such
argument. Naming a response lands it in `...` where nothing reads it,
so the caller is answered about a different response without being
told. Findings 6, 49 and 58 record the same shape, an argument taken
and then dropped, on a constructor rather than on an accessor.

`glance()` reads the formula and gets all three, so the information is
on the object and only this accessor loses it. `family()` is what
other packages call, which is what makes it the one of the two that
matters.

**66. `model.frame()` returns a wide fit's predictors and none of its
responses.**

Same file. The frame comes back 60 by 2, holding `x` and `time`. The
three responses are absent, while `fit$data` and
`insight::get_data()` each return all five columns.

`model.frame.mvgam()` at `R/insight.mvgam.R:230` builds its column
list as the response plus the predictors:

```r
response <- all.vars(mvgam_obs_formula(formula)[[2L]])
```

On an `mvbrmsformula` that subscript is not a language object. It is
the character vector `c(count = "count", seen = "seen", "mass")`, and
`all.vars()` of a character vector is `character(0)`. So the responses
are dropped and the intersection keeps the predictors alone.

`names(formula$formula$forms)` holds the three names, so the answer is
on the object. A univariate fit is unaffected, since there the
subscript is a symbol and `all.vars()` reads it.

`model.frame()` is the standard route to a fitted model's data, and a
frame with no response cannot be used for anything it is normally
reached for. Finding 64 records `terms()` raising on the same object,
so the two accessors a caller pairs are broken together.

## A wide fit, drawn

**70. `plot(type = "trend")` draws one response's latent state in
every panel.**

`test-grain-mvbf-wide.R`, "each trend panel draws its own response's
latent state". The fit holds three responses with three latent
columns. The sampler separates them: the posterior means of
`trend[, 1]`, `trend[, 2]` and `trend[, 3]` differ by up to 1.73 and
carry standard deviations of 0.504, 0.161 and 0.270.

The plot draws three panels whose strips read `count`, `mass` and
`seen`. Reading the line layers off the built object, on every x the
three panels share:

| pair | shared x | max abs difference |
|---|---|---|
| count vs seen | 55 | 0 |
| count vs mass | 52 | 0 |
| seen vs mass | 53 | 0 |

One trajectory, drawn three times. The first five drawn values are
0.1714, -0.0227, -0.0454 in all three panels, against the sampler's
own 0.1333, 0.0868, 0.1441 for `seen` and -0.1149, 0.1097, 0.0639 for
`mass`.

The fault hides because the panels are otherwise right. Each is
truncated to its own response's observed length, 57, 58 and 55, so the
three pictures are different widths and no two are pixel identical.
The strips are correct, the counts are correct and the content is one
series repeated.

The panel order is wrong in the same picture. The responses are
declared `count, seen, mass` and the strips read `count, mass, seen`,
which is finding 55 on a fourth fit.

Driven again after the conditional read was fixed, the panels are
still identical on every shared x, and the reason is now located. The
hindcast arms the plot is built from do differ once each response
reads its own `trend[t, s]` column: across the three elements of the
fan-out they separate by 1.19, 1.73 and 0.95. Within any one element
they do not. `build_hindcast_arms()` (`R/forecast.mvgam.R:740`) loops
the series levels, which on a response-keyed fit are the responses,
and hands `hindcast_one_series()` the caller's single `resp` for every
one of them, so all three arms of an element read that response's
state. The plot then draws one element's three arms.

So the fault is not in the plotting layer, and looking for it there
will not find it. It is that a response-keyed fit fans out twice: once
in `hindcast.mvgam()` through `mv_resp_fan_out()` and again in the
series loop, which is already a loop over the responses. Reconciling
those two removes a loop rather than adding an argument.

`plot(type = "series")` on the same object is the other half. It draws
a single panel whose strip reads `NA`. Its one layer holds 60 rows and
its y axis is labelled `count`. Two of the three responses are not
drawn at all and the one that is carries no name. Every other
per-response surface on this fit answers correctly, `glance()` and
`augment()` included, which places both faults in the plotting layer
rather than in the fit.

The two halves have different reach, and the hierarchical fit settles
which is which. `test-trend-hierarchical.R` draws six series on a
derived axis:

| call | on the hierarchical fit |
|---|---|
| `plot(type = "trend")` | six panels, six distinct trajectories, named |
| `plot(type = "series")` | one panel, strip `NA`, all six overplotted |

So the trend panels are drawn correctly wherever the axis is a series,
and the repetition in the wide fit belongs to the response-keyed axis
alone. The series panel collapses on both, which makes it a property
of any axis the frame has no column for, derived or response-keyed.
Six series drawn over one another read as noise rather than as a
series, so nothing about the picture invites a second look.

**71. A gaussian arm reads another response's latent state.**

Same fit, and the control is what makes it a claim about `mvbf()`
rather than about the data. A randomised quantile residual is standard
normal by construction. Each response was refitted on its own, same
rows, same covariate, same AR(1) trend:

| arm | family | in the wide fit | fitted alone |
|---|---|---|---|
| count | poisson | sd 0.500 | sd 0.453 |
| seen | bernoulli | sd 1.005 | sd 0.963 |
| mass | gaussian | **sd 2.699** | **sd 0.984** |

This entry originally read the 8.13 bound as the signature of a PIT
taken against a pooled predictive. That was wrong, and driving it
settles what is really happening. Making the PIT per-draw moved the
poisson arm from 0.500 to 0.998 and the bernoulli arm from 1.005 to
1.033, and left the gaussian arm at 2.849. So the pooled PIT was one
fault and it was not this one.

Measured on the wide fit, each arm's conditional linear predictor
minus its marginal one is the latent state that arm read:

| pair | max absolute difference |
|---|---|
| count against mass | 4.4e-16 |
| count against seen | 6.7e-16 |
| sampler's series 1 against series 3 | 3.436 |

All three arms read one state while the sampler holds three. The
reason is one line: `extract_trend_latent_states()` takes the series
index from `get_observation_structure()$series_int`, which on a wide
frame is 1 for all sixty rows, because a row of a wide frame is a time
and not a series. Every arm therefore reads `trend[t, 1]`. The
residual code is faultless; `sd(y - colMeans(mu))` is 0.896 against a
fitted `sigma` of 0.370, which is what a predictor built from another
response's trajectory gives.

That places this entry with finding 65, which records the same
substitution reaching `plot(type = "trend")`. It is wider than a plot:
the conditional read is what `fitted()`, `posterior_epred()`,
`log_lik()` and every information criterion take on a wide fit. The
poisson and bernoulli arms survive it because a count PIT and a binary
PIT are coarse enough to absorb a wrong mean; the gaussian arm is the
one that shows it.

**72. Poisson quantile residuals are half as wide as they should be,
on any fit.**

Found by the control above. The poisson arm reads sd 0.500 in the wide
fit and 0.453 fitted alone, and no value in either reaches three
standard deviations. A standard normal puts 0.27 per cent beyond
three. A third poisson AR(1) on unrelated data, the one
`test-draws-alignment.R` fits, reads 0.454 with 0.05 per cent beyond
three, so the number is the family's rather than any one design's.

What it is not is the empirical path as such. `quantile_family_specs`
at `R/residuals.mvgam.R:396` holds five entries, all continuous, so
every count family in this directory falls through to
`compute_quantile_residuals_empirical()`. Measured across four of
them, the ones that fall through do not agree:

| family | quantile sd | beyond three | constant columns |
|---|---|---|---|
| poisson | 0.454 | 0.05% | 0 of 30 |
| bernoulli | 1.005 | -- | -- |
| hurdle_poisson | 0.947 | 0.19% | 0 of 80 |
| zero_inflated_poisson | 0.954 | 0.16% | 0 of 80 |

Two count families with a large atom at zero answer correctly on the
route poisson takes. The shared machinery is therefore not enough to
explain the compression. Finding 21 attributes the fault to
`qnorm(lower[i])` being returned wherever the pooled `lower` and
`upper` coincide. That mechanism accounts for a continuous family,
where the two always coincide. It does not separate these four. All
four are discrete and only poisson is compressed. Which step
of the empirical PIT treats poisson differently is not resolved here.
Guessing at it would put a mechanism in this file that nothing
measured.

So a poisson fit's residual QQ plot is too narrow to show a departure
that is really there, and a hurdle or zero-inflated fit of the same
counts is not. Which of the two the empirical PIT should be made to
match is a question for the residual work; what is recorded here is
that they disagree and only one can be right.

**73. Some methods class the list they fan out, and some leave it
bare.**

Same fit. Every method that answers per response returns a list keyed
by the response name. Three of them class that list and three leave it
bare:

| call | class of the fan-out |
|---|---|
| `hindcast()`, `forecast()` | `mvgam_forecast` |
| `conditional_effects()` | `mvgam_conditional_effects` |
| `plot()`, `pp_check()`, `predict()` | `list` |

The consequence is visible at the console. `pp_check(fit)` holds three
ggplots and prints as a list, so the reader gets `$count`, a plot,
`$seen`, a plot, `$mass`, a plot, rather than one figure. `plot(fit)`
does the same. On a univariate fit both return a single object that
renders, so the wide fit is where the two behaviours part.

Naming a response sidesteps it, since `pp_check(fit, resp = "count")`
returns a plain ggplot. Recorded because the default call is the one a
reader makes first.

**Checked and correct.** `pp_check(fit, resp = )` draws the right data
for each arm: the plotted x range covers 2 to 47 for the poisson arm,
0 to 1 for the bernoulli and 0.05 to 3.34 for the gaussian, matching
each response's own observations.

## A wide fit and the evaluation surface

**67. `score()` refuses the object `forecast()` gave it.**

`test-grain-mvbf-wide.R`, "a wide forecast can be scored". A wide fit
fans out per response, so `forecast()` returns an `mvgam_forecast`
carrying one element per response rather than the arms directly. Each
element is complete: for `count` the `count` arm holds a 50 by 5
matrix of finite draws and `test_times` names the five held-out
occasions.

`score()` on that object answers

    'object' contains no held-out forecasts to score.
    Pass 'newdata' covering held-out times to 'forecast()'.

which is what produced the object. Indexing the wrapper first works,
so `score(fc[["count"]])` returns a scored list and the forecasts were
there throughout. The method reads `$forecasts` off the outer object.
A fan-out wrapper keeps nothing there, so the absence is reported as
the user's mistake.

This is finding 9's shape a third time, a refusal whose stated remedy
has already been followed. It reaches every multivariate fit, since
the fan-out is how `mvbf()` and `jsdgam()` both answer.

**69. `kfold()` on a wide fit fails on a count that finding 15
produces.**

Same fit. `kfold(fit, K = 2)` stops with

    Could not align log-lik columns with rows of data.
    log_lik has 50 cols; data has 60 rows.

The frame holds 60 occasions and each response is missing on a
different handful, ten rows in total carrying at least one gap.
`log_lik()` returns 60 columns of which those ten are entirely `NA`,
which is finding 15: a joint density that drops every response at an
occasion where any one of them is missing. Fifty is what survives.

So the alignment guard is reading a symptom rather than a cause, and
its message describes an arithmetic mismatch instead of the missing
data behind it. Finding 15 records the wrong likelihood; this records
that the same defect also removes `kfold()` from a wide fit.

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

## Visits that never happened

**77. A closure-unit fit with unmade visits loses two prediction
methods and half its likelihood.**

`test-grain-closure-units.R`, "a visit that never happened is still
predicted" and "the unit arrays cover the visits that happened". The
file fits one occupancy model on a complete visit schedule and one
with every sixth visit unmade, spread so that no unit loses all of
its visits. 300 rows, 250 of them observed, 75 units throughout.

The Stan data is right. `N` is 250, `sum(n_rep)` is 250 and
`max(visit_idx)` is 250, so every index stays inside the response and
the unit count is unchanged. What the fit is given is correct and
what comes back is not:

| call | complete schedule | every sixth visit unmade |
|---|---|---|
| `posterior_epred()` | 10 x 300 | error, "non-conformable arrays" |
| `fitted()` | 300 x 4 | error, "non-conformable arrays" |
| `posterior_predict()` | 10 x 300 | 10 x 300 |
| `residuals()` | 75 x 4 | 75 x 4 |
| `log_lik()` | 10 x 75 | 10 x 75, 42 columns entirely `NA` |
| `kfold(K = 2)` | runs | error, alignment mismatch |

Three things follow from one cause. `posterior_epred()` and `fitted()`
stop on a message naming neither a column nor a row, and they are the
two a reader reaches for first. `log_lik()` answers at the right width
and empties 42 of its 75 units, which is 56 per cent of the
likelihood on a frame where no unit lost all of its visits. `kfold()`
then reports "log_lik has 33 columns". Since 75 minus 42 is 33, the
cross-validation failure is those missing densities arriving one layer
down.

`posterior_predict()` and `residuals()` answer correctly on the same
fit. That places the fault in how the two grains are reconciled, a
visit against a unit, rather than in the fit or in the Stan data. A frame
with no missing visits hides all of it, which is why the pair of
schedules is what the file needs.

Finding 15 is the same shape on a wide frame, where an occasion
missing one response drops the density for all of them. Here a unit
missing one visit drops the density for the whole unit.

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

## A narrowed likelihood paired with the whole frame

**79. A missing response removes `kfold()` and `loo(by_series = TRUE)`
from a fit, and the attribute that would prevent it is already set.**

`test-factor-lv-axis.R`, "the likelihood covers the rows the frame
holds". The gaussian factor fit holds 300 rows of which 24 carry no
response. `log_lik()` answers at the full width and empties those 24
columns, which is correct: a row with no response contributes no
density.

`clean_ll()` then drops them, and records what it dropped. Its own
roxygen at `R/loo.mvgam.R:412` states the contract:

> Which columns survived is recorded on the result: anything paired
> with the scored matrix afterwards ... has to be narrowed to the same
> columns or the two describe different observations.

Two consumers pair it with the unnarrowed frame anyway:

| call | on 300 rows, 24 unobserved | on 30 rows, none unobserved |
|---|---|---|
| `log_lik()` | 300 columns, 24 all `NA` | 30 columns |
| `clean_ll()` | 276, `scored_columns` 276 | 30 |
| `loo()` | answers on 276 | answers on 30 |
| `waic()` | answers on 276 | answers on 30 |
| `kfold(K = 2)` | refuses, 276 against 300 | refuses, finding 74 |
| `loo(by_series = TRUE)` | refuses, 276 against 300 | answers |

`kfold.mvgam()` calls `clean_ll()` at `R/kfold.mvgam.R:190` and hands
`NCOL(loglik_full)` to `map_loglik_cols_to_groups()` two lines later
alongside the full `data`, never reading `scored_columns`.
`per_series_ic()` at `R/loo.mvgam.R:330` does the same and says so in
its own message: "by_series = TRUE assumes clean_ll() did not drop any
columns." So one of the two knows the assumption it is making and
neither acts on it.

`loo()` and `waic()` are the control. They read the same narrowed
matrix and answer, which places the fault in the pairing rather than
in the narrowing. The second column is the other control: with no
missing response the widths coincide and `loo(by_series = TRUE)`
answers, so this follows the gap and not the fit.

The refusals are also miscast. Both report an arithmetic mismatch and
neither names the 24 unobserved rows behind it, so a user is told the
counts disagree without being told why or that the answer they wanted
is available on the rows that were observed.

This is the plan's own class in its plainest form. How many columns
the likelihood has is derived twice within four lines, then the two
answers are compared without either being converted to the other. Finding 15 is the same defect one layer up, where the
likelihood loses columns it should have kept; here the columns are
dropped correctly and the count is not carried.

**Finding 74 takes a seventh fit.** `kfold(K = 2)` on the
single-series poisson AR(1) of `test-draws-alignment.R` refuses with
"Irregular time intervals detected in time. Interval range: 1 to 5".
The frame is 30 consecutive occasions on one series with no gaps at
all, so the irregularity is entirely the fold split's, which is
finding 74's diagnosis on the simplest frame in the directory.

## What reading a rendered article shows

**80. `vignettes/articles/var.Rmd` publishes four faults that a
successful knit cannot see.**

Found by reading the rendered output rather than by checking that it
rendered. The article knits in 14.1 minutes with no error and no
warning, and every one of these is in the page a reader gets.

**The figure captions name the wrong regions.** The frame declares
`regions <- c("BC", "Alb", "Sask")` and then builds the series axis
with `as.factor(region)`, so the levels sort to `Alb, BC, Sask` and
`region_order <- levels(train$series)` is that order. The inline text
is computed from `region_order` and is right. The captions were
written by hand from the declaration order and are not:

| call | caption says | the text below it says |
|---|---|---|
| `plot(irfs, series = 1)` | region 1 (British Columbia) | "A shock to Alb" |
| `plot(irfs, series = 2)` | region 2 (Alberta) | "a shock to BC" |
| `plot(irfs, series = 3)` | region 3 (Saskatchewan) | Saskatchewan |

Two of the three contradict the sentence directly beneath them, and
the third agrees only because `Sask` is third in both orders. This is
finding 55's permutation, alphabetical against declared, reaching a
published page. Finding 8 is why it happened: `irf()` labels its
shocks `Process_k`. The article therefore maps them back by hand at
`var.Rmd:336`. A positional mapping written out twice is one that can
be inverted once. The entry predicted that a label nobody
can resolve makes the table unusable without knowing the internal
ordering. Here the package's own article got it wrong.

**A computed value contradicts the prose around it.**
`cross_resolved` counts the cross-region impulse responses whose 95%
interval excludes zero. It renders as 2. The sentence it sits in
continues "the intervals do not support telling it", and the
paragraph closes "any spillover is too small for this series to pin
down". The prose was written for a zero that the fit did not produce,
so the article states a conclusion its own number refutes.

**The fit asks for four chains and reports three.** The chunk at
`var.Rmd:133` reads `chains = 4`, and the `summary()` printed
underneath it says "Draws: 3 chains" with 4500 post-warmup draws,
which is 3 x 1500 exactly. So three chains are what the numbers rely
on. `summary()` is not miscounting: fitted at 2, 3 and 4 chains it
reports 2, 3 and 4 and `ndraws()` agrees each time. A chain was
therefore lost during this fit and nothing said so, with `silent = 2`
covering whatever was raised. A quarter of a posterior leaving
without a word is worth a message the caller cannot suppress by
asking for a quiet fit.

**A citation disagrees with its own reference list.** The text cites
Heaps [2022] twice, at `var.Rmd:38` and `var.Rmd:172`. The reference
list gives Heaps SE (2023), JCGS 32(1), 74-83, under the same DOI.
The list is right.

**82. `vignettes/articles/mvbf.Rmd` reports one intercept twice and
promises a call that is refused.**

Found the same way as finding 80, by reading the rendered page.

**The recovery table gives the camera arm two intercepts.** The
printed table carries both spellings of every arm's intercept:

| name | Estimate | truth |
|---|---|---|
| `Intercept_count` | 1.559 | 1.5 |
| `b_count_Intercept` | 1.559 | 1.5 |
| `Intercept_biomass` | 0.580 | 0.5 |
| `b_biomass_Intercept` | 0.580 | 0.5 |
| `Intercept_camera` | 0.007 | -0.5 |
| `b_camera_Intercept` | -0.882 | -0.5 |

The prose beneath says "every intercept ... sit close to their
generating values". Two of the six do not, and a reader has no way
to tell which of the two camera rows to read against `a_camera`.

The mechanism is brms's centred parameterisation rather than an
mvgam fault. `Intercept_<r>` is the intercept at the covariate mean
and `b_<r>_Intercept` the intercept at zero. The two therefore part
company by the slope times the covariate mean. Only the camera arm
carries an uncentred covariate: `deploy_days` is drawn
`Unif(5, 20)`, and 0.077 x 12.5 is 0.96, which is the gap. Checked on the wide fixture,
where `x` is drawn `rnorm` and centred already, the two spellings
agree to 0.008 on all three arms, which is what confirms the
mechanism.

So the package is behaving as brms does and the article is reading
it wrongly. It is recorded for two reasons. The table is a recovery
check, which is the one thing a reader of a simulation study acts on.
And `posterior_summary()` prints both names with nothing to say that
they sit on different scales.

**A promise the package refuses.** The article closes the forecast
section with "The same forecast objects feed straight into
`score()`". Finding 67 records what `score()` answers on the forecast of a wide
fit: "'object' contains no held-out forecasts to score". The fan-out
wrapper keeps nothing in `$forecasts` and the method reads that slot.
The article never runs the call, so the knit cannot catch it. A
reader who follows the sentence meets a refusal telling them to do
what they already did.

**A sign flip applied where nothing is indeterminate.**
`recovery_summary()` multiplies each posterior by
`sign(cor(med, truth$x))`, explaining that "latent factor models
identify the trend only up to sign". None of the four fits is a
factor model. Each is an AR(1) state with an identified intercept,
so the sign is identified and the correction has nothing to fix.
What it does instead is guarantee a non-negative correlation with
the truth for every fit in the table, which can only move RMSE
downward. The comparison it feeds is the article's headline claim
that the joint fit recovers the state best.

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

Three of the eight that build carry a defect the build cannot see.
`var.Rmd` and `mvbf.Rmd` under findings 80 and 82, and `jsdgam.Rmd`
under a paragraph of stale numbers. All three are repaired. A knit
reports whether the code ran. The faults reading found sat in what
the code printed and in the prose beside it.
