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
produces, so it fails until they meet it.

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

**52. A missing covariate value is refused nowhere, and the
prediction hands back `NA`.**

`test-trend-var.R`, "a missing covariate value is refused by name".
One `NA` in a covariate column of a `newdata` frame does not stop the
prediction: `posterior_epred(fit, newdata = nd, draw_ids = 1:5)`
returns a matrix and the gap reaches the caller as `NA` cells.

Nothing on the prediction path looks for it. No `eta` assertion in
`R/` carries `any.missing`, and the only `NA` guard there is the
series-index check at `R/predictions.R:603`.

The same fit refuses other malformed frames well: an unknown series
names the level and lists the ones the model has, and a gapped
forecast frame names the series, the last observed time and the times
it expected. The standard is set within the same object, and what is
wanted is a refusal naming the user's own column.

## com_binomial and the trials aterm

**A difference worth recording, for the family work to settle.** The
lower bound on `nu` differs between the two spellings: scalar `nu` is
truncated, reaching Stan as
`normal_lpdf(nu | 1, 1) - normal_lccdf(-5 | 1, 1)`, while a modelled
`nu` gets a plain `normal_lpdf(Intercept_nu | 1, 1)`. Whether an
intercept on the identity scale should carry the scalar's bound is a
question for the family rather than for the axis work.

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

## Arguments nothing reads

**76. `pp_check()` still takes an argument that reaches no one.**

Every other closed method on the post-fit surface refuses one, and
`tests/local/test-dots-refusal.R` derives that set from the S3
registry at runtime, so a method added later without the guard fails
there. `pp_check()` hands `...` to bayesplot, which names the argument
in a warning and returns the plot built on the default the caller was
overriding. The notice is bayesplot's and leaves if the route to it
changes. `test-grain-mvbf-wide.R` pins it.

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

## A smooth the design cannot identify

**91. The unpenalised part of a smooth is rank deficient, and the band
drawn around it is a hundred times the effect.**

`val_mvgam_smooth_surfaces`, poisson,
`y ~ s(z, by = grp, k = 5) + t2(z, w, k = c(4, 4)) + gp(w, by = cat, k = 5)`
over 60 rows. `standata()$Xs` is 60 by 6 with rank 4. `bs_sz:grpa_1`
reads mean -0.77, sd 123.8; `bs_t2zw_1` and `bs_t2zw_2` sit at 137.4
and -137.7 and sum to -0.3. Every `s_*` beside them has sd 1 to 4.

`conditional_smooths()` therefore draws a band 1.03 wide at the centre
of `z` and 109.7 at its edge, around an estimate spanning -6.9 to 6.5,
with width tracking `|z|` at R^2 0.99998.

`test-obs-smooth-surfaces.R` passes on it: the assertions are
ordering, a non-zero width and `sd(estimate__) > 1e-6`, all of which a
hundredfold band satisfies. A prefit rank check on the stacked
observation and trend design would name the pairing.

## Debt the code carries in recognisable shapes

**89. Ten shapes account for the defects found so far, and a scan
counts six of them.**

Each shape leaves a mark in the source that a scan can find.
`tests/local/debt_scan.R` reads parse data to count the marks in
`R/`. Several shapes include false positives: the unused-argument
scan counts dispatch kernels that share a signature (`log_lik_*`
taking `trials`) and generics such as `methods_md()`, and each hit is
read before anything is removed.

| shape | the mark it leaves | count |
|---|---|---|
| one fact, several derivers | raw `[[series_var]]` / `[[time_var]]` reads; `sort(unique(...))` axis rebuilds; `inherits(..., "mvbrmsformula")` asked in place of the question meant | 53, 40, 58 |
| a literal standing in for a missing value | `%||% "y"`, `%||% "series"`, `%||% "explicit"` | 118 |
| a missing column skipped | `intersect(x, names(data))`, `if (!col %in% names(df)) next` | 21 |
| a warning silenced, not traced | `suppressWarnings()`, `suppressMessages()` | 12 |
| an argument nothing reads | accepted, asserted, never used; the scan also flags `df` on `AR()`, `RW()`, `CAR()` and `ZMVN()`, finding 6's shape if it holds | 113 non-S3 functions |
| a stored copy of a derivable fact | object slots and metadata fields written once and read in a few places | not counted |
| one condition, several refusals | the same fault refused with different wording at different layers | not counted |
| a proxy for the question meant | "the frame has no series column" standing for "the responses are the series"; `length(x) > 1` standing for "multivariate" | found by reading |

The tests carry the same debt. A stub that fakes a class with
`structure(y ~ x, class = c("brmsformula", "formula"))` lets an
assertion pass on an object no user could build. So does a stub
carrying slots a real fit no longer has. Assertions that compare
counts or use
`expect_setequal()` pass where the claim being tested is an order or
a value.

The scans are cheap, and a count falls only when code is deleted.
Each remaining shape gets one pass. A pass removes the rival, the
fallback or the proxy, adds an assertion that fails before the change
and records the count before and after.


