# Dev Tasks: mvgam 2.0 release survey

**Source:** post-fit sweep over the cached fixtures plus three
read-only survey agents. No TRD.
**Generated:** 2026-08-25

## Relevant Files

**Created** — `tests/local/postfit_sweep.R` drives every post-fit
method against every cached fit and logs a status and shape per call
(`--fixtures=`, `--groups=` to filter).
`tests/local/test-hierarchical-trends.R` covers `gr`/`subgr` fits end
to end. `R/mvgam_families.R` restores the family hub topic.
`R/stan_source.R` holds the rules for reading the Stan source brms
generated. `tests/local/test-new-levels.R`,
`tests/local/test-update-trend-args.R`,
`tests/testthat/test-multi-response-kernels.R` and
`tests/testthat/test-mvgam-core.R` cover what the sweep could not
reach. `mvgam_codegen_options()` in `R/brms_integration.R` carries the
brms code-generation settings from one place to every generator that
needs them, and `tests/testthat/test-stancode-standata.R` pins each of
their journeys.

**Notes** — long commands run in the background and write to a log
that is then read. Fixture-dependent tests live in `tests/local/`; CI
tests never skip on a missing fixture and the CI sweep stays under ten
minutes. Cached fits are read once, never re-fitted to inspect.

## Tasks

- [x] **0.0 Fixed during the survey**
  > Each was reproduced against a cached fit before being changed, and
  > re-checked after, with the CI suite green.

  - [x] 0.1 `get_safe_dummy_value()` read only the lower bound, so Beta
    got a dummy of 1 and brms rejected any newdata carrying `NA`
    responses, which no bounded family could forecast past, since
    forecasting always carries them.
  - [x] 0.2 Four sites built `chain_id` from a fractional per-chain
    count, so every `loo_pit*` check failed under `ndraws =`.
  - [x] 0.3 Four `loo_*` methods gave an internal error on
    multivariate fits instead of naming `resp`; one guard now serves
    six methods.
  - [x] 0.4 `avg_slopes()` aborted on any single-series fit, because
    `find_predictors` offered a constant `series` as contrastable.
  - [x] 0.5 `predictive_error()` accepted `resp` and ignored it.
  - [x] 0.6 `predict(type = "variance")` failed on the path its own
    error recommended.
  - [x] 0.7 `parnames()` / `nsamples()` disagreed with `variables()` /
    `ndraws()` by falling through to the `brmsfit` methods.
  - [x] 0.8 `R/sysdata.rda` shipped 1.7 MB of pre-2.0 fits nothing
    referenced. Tarball 3.8 MB → 2.2 MB against CRAN's 5 MB.
  - [x] 0.9 Citations: a JSS DOI in `inst/CITATION`, a NEWS reference
    to issue `#111` that 404s, a misquoted title and two wrong years.
  - [x] 0.10 Every post-fit method failed on a hierarchical
    (`gr`/`subgr`) fit: the trend derives its series id with `_` and
    records those levels, but the validator compared them against the
    `series` column the data still carried. `summary()` worked, which
    is why nothing caught it. The derivation now lives in one helper,
    the validator checks the derived value, and a superseded `series`
    column is reported once.
  - [x] 0.11 `insight::model_info()` answered `"custom"` with every
    type flag FALSE for all ten mvgam-native families, never fired
    `is_proportion`, hard-coded `is_mixed = FALSE`, and called a
    univariate binomial fit multivariate.
  - [x] 0.12 Deferred-work and internal framing in shipped text: three
    `TODO`s, two "not yet implemented" errors, and roxygen and an
    error message citing "this branch".
  - [x] 0.13 `tikhonov_hmsc_2020` was defined twice in the citation
    registry; the later entry silently won.
  - [x] 0.14 Prose linter: 34 of 103 files under `R/` failed, now
    zero. 38 em-dashes repunctuated, 14 machine-nouns and 8 "DRY"
    references rewritten, two verbatim published-title lines marked
    with the linter's own exemption.
  - [x] 0.15 `mvgam_families` was a 1.x topic the rebuild dropped,
    leaving `README.Rmd`, `index.Rmd`, `R/jsdgam.R` and an error
    message pointing at nothing. Restored covering 36 families against
    1.x's 11, keeping the per-family link, adding each family's
    arguments, and correcting the 1.x claim that links can never be
    changed.
  - [x] 0.16 Fifteen sites emitted two warnings per event:
    `insight::format_warning()` raises one as a side effect *and*
    returns the string, so `rlang::warn(format_warning(...))` fires
    twice. The superseded-series warning also honours `silent >= 2`.

- [x] **1.0 Score `weights()`, `cens()` and `trunc()` correctly**
  > `R/log_lik_addition_terms.R` applies all three to the matrix
  > `log_lik()` returns, so `loo()`, `waic()`, `lfo_cv()` and
  > `kfold()` inherit them. Order follows brms: censor, then
  > truncate, then weight. Discrete truncation uses `lb - 1`, which
  > is what brms's Stan samples under even though its own R code does
  > not. Agreement with `brms::log_lik()` on a fit carrying all three
  > is exact for weights and censoring and at machine precision for
  > truncation. `family_dist_spec()` now parameterises each family
  > once for both the density and the distribution function,
  > replacing 13 duplicated `stats::d*()` calls. A term recorded at a
  > grain the likelihood does not share is refused rather than
  > silently dropped.

- [x] **2.0 Fix `log_lik()` draw subsampling on closure-unit fits**
  > `log_lik(fit, ndraws = n)` errored on an `nmix()` or `occ()` fit,
  > and the shape mismatch was the visible half: the detection
  > probability and the latent state were sampled independently, so
  > they could come from different iterations. `ndraws` is now
  > resolved to concrete `draw_ids` before either extraction, reusing
  > the helper `posterior_epred()` already used.

  - [x] 0.17 A missing response aborted `log_lik()`, `waic()`,
    `loo()`, `residuals()` and `pp_check()` for the hurdle,
    zero-inflated and ordinal families. Those kernels branch on the
    response value, so `if (NA)` errored; the guard now sits in the
    shared applier and the ordinal kernel routes through it too.
    Separately, an ordered-factor response could not take a numeric
    dummy, and `waic()` did not drop all-NA columns the way `loo()`
    does.

- [x] **7.0 Fix what a user's second round of testing turned up**
  > Six reports against 2.0.0, each reproduced before being changed.

  - [x] 7.1 A denominator of zero broke `posterior_epred()`, `loo()`,
    `pp_check()` and both residual plots at once. brms accepts zero
    trials when fitting, and it is the natural padding for a cell the
    likelihood never saw, so prediction has to accept it coming back
    out. Two of mvgam's three trials assertions already allowed it.
  - [x] 7.2 `summary()` dropped every population-level slope, on every
    fit: it read the raw stanfit, where brms keeps coefficients as an
    unnamed `b[1]`, and only the brmsfit method resolves those to
    `b_x`. Reported against a distributional fit, but `y ~ 1 + x`
    showed the intercept alone. Distributional coefficients were also
    printed twice, once mislabelled, and neither the sub-formula nor
    the links of anything but the mean appeared in the header.
  - [x] 7.3 One `mvgam()` call raised brms's dropped-rows warning three
    times, once per pass through the code generator, and a residual
    panel raised its own four times, once per panel, naming
    `pp_check()` to a user who called `plot()`. One helper now reports
    each distinct message once per user-facing call.
  - [x] 7.4 A distributional sub-formula fit and sampled correctly and
    then failed on every post-fit surface, for every family, because
    the extractor looked for a scalar the model never produces. One
    resolver now covers both cases and serves all four prediction
    paths. Two defects surfaced underneath it: `stats::gaussian()`
    records no link for `sigma`, so the parameter came back on the log
    scale as a negative standard deviation, and a multivariate fit
    lost the row count its predictor is sized by.
  - [x] 7.5 `posterior_linpred(dpar = )` accepted the argument and
    answered for the mean.
  - [x] 7.6 `com_binomial()` was slow at large denominators. Folding
    out the terms that cancel between numerator and normaliser, and
    bounding the normalising sum the way the `nmix()` families already
    bound their latent-`N` loop, gives 6.3x at a denominator of 1466
    and 1.35x at 20. The lookup table holds log factorials rather than
    every binomial coefficient, so it is linear in the denominator
    rather than square. Checked against exact enumeration over 1287
    cells spanning both branches: agreement to 6.7e-11, and
    probabilities summing to one within 1.8e-12.

  - [x] 7.7 Found while fixing 7.4: a detection probability given a
    sub-formula was squashed through `plogis()` for every closure-unit
    family, but `nmix("poisson_poisson")` models `p` as an encounter
    rate on a log link, and its own kernel uses it as a Poisson rate.
    Rates above one were silently capped.

  - [x] 7.8 A sweep for the pattern behind 7.4 across every extraction
    in the package. A post-fit answer is assembled from several reads
    of the posterior, and each of them subsampled on its own when
    handed a count rather than indices, so the pieces that were then
    added together came from different iterations. Nothing about the
    output showed it. The worst instance was not in the distributional
    path at all: `get_combined_linpred()` drew the observation
    predictor, the trend predictor and the process errors separately,
    so on a fit with a trend-side formula, asking for the whole
    posterior returned rows of which one in a thousand paired a
    predictor with its own trend. Resolving a count to indices is now
    one function, called once per entry point and again inside each
    extractor, and it yields indices even for a count covering
    everything, since the extractors draw at random and would
    otherwise return the posterior shuffled. Verified by checking that
    every row of a subsampled prediction is a row the fully specified
    prediction also produces.
  - [x] 7.9 Five more instances of the same pattern, found by sweeping
    every extraction rather than only the one the report pointed at.
    An ordinal fit read its thresholds and its `disc` from the leading
    rows of the posterior while the predictor they cut came from a
    random subsample. A multivariate response family asked for a
    random subsample of the mean and then read `Psi` from the first
    rows. `predict(type = "variance")` ignored `ndraws` altogether on
    those families. The innovations had a rule of their own, taking
    the first draws rather than a sample. Seven further sites each
    restated the rule correctly but separately, which is how the
    versions drifted apart in the first place. There is now one
    function deciding which draws, and a reader that applies it to a
    draws matrix; twenty-two call sites go through them and no
    bespoke selection remains outside.
  - [x] 7.10 Made the pattern unreachable rather than merely absent. A
    draw count stops at the boundary a user calls through: every entry
    point turns it into indices, and the extractors underneath accept
    indices alone, so a caller written later cannot pass a count down
    to be resolved twice. Two more instances turned up while doing it.
    A seventh private copy of the selection rule sat behind
    `posterior_smooths()`, unsorted and shuffling when asked for
    everything. And `conditional_smooths()` called its extractor once
    per smooth term inside a loop, so the panels of one figure were
    drawn from different iterations of the same posterior. The
    invariant now has a test of its own: every row a subsampled
    prediction produces has to be a row the fully specified prediction
    also produces, since subsetting draws may drop rows and reorder
    them but cannot invent a pairing no single draw gives.

- [x] **8.0 Hierarchical VAR fits had no post-fit surface**
  > A grouped trend carries its correlations as a population Cholesky
  > factor plus per-group deviations whatever the trend constructor
  > was, so parameter extraction aliased a hierarchical `VAR()` to the
  > hierarchical Cholesky case. The innovation transform asked a
  > narrower question and did not, so such a fit was handed the
  > parameters of one structure and then asked for the covariance of
  > another. Every surface that samples process error failed together:
  > `posterior_epred()`, `posterior_predict()`, `log_lik()`,
  > `fitted()`, `residuals()`, the `loo_*` family, every `pp_check()`
  > type, `bayes_R2()` and `plot(type = "residuals")`. One helper now
  > names the structure for both readers.

- [x] **9.0 Sweep harness, and the response surfaces it stalled on**
  > The harness judged a call by whether it ran. It now also asks
  > whether what came back can be true: shapes against draws and rows,
  > the same draws twice giving the same answer, a subsampled
  > prediction being made of rows the full one also produces, `epred`
  > agreeing with the inverse link of `linpred`, draws lying inside
  > the family's support, predictions spanning every row rather than
  > the fitted ones, and the summary reporting every coefficient the
  > design matrix carries. It separates maintained fixtures from
  > pkgdown caches, since a cache that predates a change fails for
  > reasons of its own and buried two hundred spurious results in the
  > last run. Rows are written as they are produced, and a call may
  > exceed a time budget without stalling the sweep.
  >
  > It stalled on a twenty-four process VAR because `irf()` and
  > `fevd()` returned one transition matrix per draw per horizon, 382
  > MB for a figure read as a band, with no way to ask for fewer
  > draws. Both now report the posterior median and interval, keep the
  > draws behind `summary = FALSE`, and take `ndraws` / `draw_ids`
  > like every other post-fit method. The summary is 340 KB against
  > 382 MB, and a subset of draws answers roughly seven times faster.
  >
  > `stability()` was the last of the three VAR surfaces still handing
  > back its draws, so it reports each metric once as well, keeps them
  > behind `summary = FALSE`, and takes the same draw arguments: each
  > draw costs a Lyapunov solve, so a subset answers about four times
  > faster. A survey of every other post-fit surface found no further
  > case. `residual_cor()` and `posterior_transition_matrix()` already
  > summarise and answer in under two seconds on the same fit, and the
  > `posterior_*` and `loo_*` families return draws by design.
  >
  > Summarising a stability metric to a midpoint and an interval threw
  > away what it is usually read for, since reactivity is asked about
  > for whether its mass crosses zero. The summary carries each
  > metric's binned posterior instead, so the histogram is drawn from
  > the counts rather than the draws: the same breaks, the same
  > counts, at 10.8 KB against 283 KB.

- [x] **10.0 The response scale meant two things**
  > Found by driving every family through the dispatch rather than by
  > a fixture, since no fixture reaches most of them. `E[Y]` was the
  > inverse link of the predictor for twelve families whose mean is
  > not that: every hurdle and zero-inflated family returned the base
  > distribution's parameter with none of the mass moved to zero, and
  > `lognormal` errored, because the dispersion its Jensen correction
  > needs was never resolved. The means were already written and
  > unit-tested, copied from brms, and nothing called them. Each is
  > now read from the one function defining it, found by name the way
  > `log_lik.mvgam()` finds its densities, with the parameters it
  > needs named once and resolved before dispatch. Six samplers made
  > the mirror-image mistake and transformed what they were handed as
  > though it were `E[Y]`: two turned 45% of a lognormal fit's draws
  > into `NaN`, three divided a probability by the trial count, and
  > one called a beta-binomial generator with arguments it does not
  > take. A test averages the draws against the mean, so neither
  > layer can change its mind about what it was given.
  >
  > `posterior_linpred(transform = TRUE)` answered with `E[Y]`, which
  > is neither what its documentation claims nor what brms does: brms
  > sets `dpar = "mu"` and answers on that parameter's scale, so a
  > binomial gives a probability rather than a count.
  >
  > `hurdle_negbinomial` remains about five percent below its
  > analytic mean. Its sampler is brms's, which reaches a truncated
  > negative binomial by a tilt exact only for the Poisson; matching
  > brms is what the concordance tests want, so the difference is
  > recorded rather than removed.

- [x] **11.0 A grouped trend could not be forecast**
  > The covariance of a grouped trend is a population factor pulled
  > towards each group's own, and the forecast reader asked for the
  > flat parameterisation such a fit does not carry. It now reads the
  > grouped structure through the helper the innovation transform
  > already used, and both name their parameters through one list,
  > since spelling a name two ways is what let the readers disagree in
  > the first place. A single-series correlated trend failed for a
  > different reason: `diag()` builds an identity matrix from a
  > length-one vector rather than a one-by-one matrix holding it.
  >
  > `Sigma_trend` was the scaled Cholesky factor on the general trend
  > path and the covariance on the VAR path, under a label calling it
  > a covariance. It is the covariance on both. Cached fixtures still
  > hold the old value until they are refit.

- [x] **12.0 The ordinal post-fit surface**
  > An ordinal fit predicts a probability per category, which the
  > shared summariser could not take, so `predict()`, `fitted()` and
  > `augment()` failed together. The summary keeps that margin the way
  > brms does, `augment()` reports the expected ordered level,
  > `predict(type = "variance")` answers with the variance of the
  > category `posterior_predict()` draws, and `plot(type = "series")`
  > draws against the ordered level rather than refusing the factor.
  > Separately, an addition term was counted as a response, so
  > `y | trials(n)` named two, which broke every forecast that pads
  > the response with `NA`.

- [x] **3.0 Close the post-fit coverage gaps**
  > What the sweep found so far, each reproduced on a
  > cached fit before being changed.
  >
  > `summary(include_states =)` never did anything. No summary block
  > claims the trend's time-indexed states: every `match_*` predicate
  > is narrower, and `match_trend_specific_pars()` excludes them by
  > name, so `TRUE` kept rows that were then discarded unclaimed and
  > the two calls returned `identical()` objects. 1.1.x had no such
  > argument, and six routes to the states already work
  > (`hindcast(type = "trend")`, `plot(type = "trend")`,
  > `as.data.frame(regex)`, `variables()`, `as_draws_df()`,
  > `mcmc_plot(regex)`), so it was removed rather than implemented.
  >
  > Predicting a grouping level the model never saw crashed with
  > `subscript out of bounds`, reached by following brms's own advice
  > to set `allow_new_levels = TRUE`. brms extends the grouping index
  > to cover the new level while the posterior holds a coefficient
  > only per fitted level, so `population_random_pred()` indexed past
  > the end of the draws. Drawing those coefficients is brms's
  > `get_new_rdraws()`, 107 lines covering three `sample_new_levels`
  > semantics, `by` variables and correlated-RE covariance, and it is
  > unexported so `:::` is not open to a CRAN package. The limitation
  > is now named, with the counts and `re_formula = NA` in the
  > message. Everything else on that fit was already fine: 13 of 14
  > surfaces answered, including every call without `newdata` and
  > every call on known levels.
  >
  > `sample_new_levels = "old_levels"` was documented and accepted at
  > three entry points, matching brms's `snl_options`, then refused by
  > two validators underneath that took only the first two. Both now
  > match the documented surface.
  >
  > Two of the survey's premises were stale and are recorded as such
  > rather than acted on. The `posterior_*` trio and
  > `resolve_forecast_grid()` already agree on the series axis: both
  > refuse an unseen series whatever `allow_new_levels` says. And
  > `conditional_effects()` was not stripping offsets in place of
  > holding them at a reference value, because `stats::terms()` files
  > an offset under the "offset" attribute rather than in
  > "term.labels", so the filter could never match. It matched brms's
  > `get_all_effects()` already; the dead filter is gone and a test
  > pins the behaviour.
  >
  > Fixtures: three had no builder anywhere, not six.
  > `val_mvgam_lv_factor` is consumed by two tests and now has one.
  > `val_mvgam_gauss_ar1_na` and `val_sbc_recovery_ar1_ranks` were
  > consumed by nothing and have been deleted. Separately,
  > `val_mvgam_ar1_t2_noint` is consumed by
  > `test-marginaleffects-concordance.R` and did not exist, so that
  > test skipped every run; a builder now produces it with `grp`
  > rather than the `group` column marginaleffects reserves, which
  > retires the workaround that test carries.
  >
  > `posterior_transition_matrix()` had no CI coverage at all and now
  > has four tests on the existing VAR draws mock, covering the
  > summary shape, `summary = FALSE`, `robust`, the collapsed `groups`
  > argument and the trend-type gate.
  >
  > The five multi-response families had no kernel test at all. Every
  > fixture that fits one drives `residual_cor()` and
  > `as.data.frame()` on the result and nothing else, so the densities
  > themselves were never checked. `diri`, `multi`, `categ`, `mvn` and
  > `mvt` now have 21 tests against references the kernels play no
  > part in: a two-category Dirichlet against a Beta, three categories
  > against `extraDistr::ddirichlet`, `multi` against
  > `stats::dmultinom`, `categ` against the same at size one, `mvn`
  > summed over a unit against `mvtnorm::dmvnorm` with a diagonal
  > covariance, and `mvt` per element against `mvtnorm::dmvt`. All
  > five agree; the gap was coverage rather than correctness. The
  > `mvn` check also settles independently that its covariance is
  > `diag(Psi^2)`.
  >
  > `tweedie()` and `beta_nb()` are recorded above as needing the
  > same. They already carry 18 and 22 tests covering density, epred,
  > RNG, distribution function and stancode.
  >
  > The last four are where they belong. `plot_slopes`,
  > `plot_comparisons` and `hypotheses` are bare re-exports carrying no
  > mvgam code of their own: they reach the package only through
  > `get_predict.mvgam`, `get_coef.mvgam` and `set_coef.mvgam`, which
  > CI already drives, and nothing else about them could break without
  > breaking those. `latent_N_saturation()` needs a posterior it cannot
  > obtain without a fit, and its refusal on a non-closure-unit family
  > is the one part reachable without one, which CI covers. All four
  > keep their end-to-end run in `tests/local/postfit_sweep.R`, which
  > is where a fixture-dependent test belongs.

- [x] **3.1 A fitted model reported a prior it never sampled under**
  > Found by chasing the two `update(recompile = FALSE)` errors 4.0
  > had attributed to stale fixtures. The stancode a fixture stores
  > and the stancode `HEAD` emits differ by one line:
  > `sigma_trend ~ exponential(2)` against
  > `sigma_trend ~ student_t(3, 0, 2.5)`. Rebuilding was the obvious
  > fix and would have buried the bug, because a fixture built
  > minutes earlier carried the same fault.
  >
  > The trend submodel goes to brms as a gaussian, so brms hands back
  > a `sigma` row carrying its own `student_t(3, 0, 2.5)`. That is a
  > residual scale, and the trend's process noise is `sigma_trend`,
  > which the Stan generator samples under `common_trend_priors`.
  > `add_trend_suffix_to_priors()` appended `_trend` to every class it
  > was given, filing brms's value under mvgam's name. The same rule
  > sits 650 lines earlier with the guard in place, excluding `sigma`
  > and then dropping it, so this was one rule written twice with one
  > copy incomplete.
  >
  > The defect sat on the fitted object alone. `get_prior()` on a
  > specification and the emitted Stan agree on `exponential(2)`; the
  > table stored on the fit is the one `prior_summary()` prints and
  > `update()` inherits, so a refit re-specified the model and the
  > guard refused it. The prior surface reports what the model samples
  > on the specification path and now on the fitted one too.
  >
  > The table is now read from the compiled model rather than
  > reassembled beside it, so it cannot disagree with what the sampler
  > ran. `lift_mvgam_stanvar_priors()` scanned for four parameters by
  > name; one scanner now takes every `x ~ dist(args)` and
  > `dist_lpdf(x | args)` statement naming an mvgam parameter, which
  > subsumes those four and supplies `sigma_trend` and `ar1_trend`.
  > The latter was sampled under `normal(0, 0.5)` and reported by
  > nothing. A left-hand side that is a function call, as in
  > `to_vector(innovations_trend) ~ std_normal()`, is a non-centring
  > device and stays out.
  >
  > `suffix_trend_prior_classes()` is the one implementation of the
  > `_trend` suffix rule, reached by both callers. It drops brms's
  > residual-scale row, keeps a `sigma` scoped to a coefficient, and
  > leaves an empty or already-suffixed class alone.
  >
  > CI tests cover the bookkeeping row, both guards, other classes
  > being left untouched, and a coef-scoped `sigma` from a
  > distributional sub-formula not being mistaken for it; the scanner
  > keeps the emission-site tests it already had.
  > `val_mvgam_ar1_fx`, `val_mvgam_ar1_t2_noint` and
  > `val_mvgam_lv_factor` were rebuilt, since a fixture stores its
  > prior table at fit time and no code fix reaches back into one.

- [x] **3.2 `update()` could not rebuild a trend call naming a variable**
  > Found while checking that the Stan the code generator writes is
  > unchanged by a refactor: two of the 32 cached fixtures could not
  > have their specification reconstructed at all, and the reason was
  > not the refactor.
  >
  > A formula holds the expression and the environment it was written
  > in, not the values it names. `~ AR(p = 1, trend_map = Z)` written
  > against a local `Z`, then saved and read back, names something out
  > of scope, so `update()` failed with `object 'Z' not found` before
  > reaching `mvgam()`. `update(recompile = FALSE)` runs the same
  > rebuild, so a user could not even ask whether a refit needed
  > recompiling. It reaches `trend_map` and `n_lv`, which is the
  > natural way to write either.
  >
  > The fit already carries the resolved values under
  > `trend_metadata`, so `restore_trend_call_env()` binds them into a
  > child of the formula's own environment. The expression stays as
  > the user wrote it and a name the fit holds no value for is left
  > alone, reaching `mvgam()` to fail against the user's own argument
  > rather than part-way through the rebuild.
  >
  > The local test asserts more than the absence of the error: the
  > rebuilt Stan code has to equal what the fixture was built from,
  > since putting a value back must reproduce the model rather than
  > merely get past the failure.

- [x] **3.3 A refit was not the model that was fitted**
  > `update.mvgam()` rebuilds the fitting call from
  > `mvgam_update_inheritance`, which named six arguments while eleven
  > model-defining ones were absent. A refit of a fit carrying a
  > structured loadings prior lost the whole thing, the Stan code
  > dropping `row_features`, `dist_cluster`, `theta_features` and the
  > `multi_normal_cholesky` prior on `Z`. Only the
  > `recompile = FALSE` guard made it visible.
  >
  > `newdata`, `threads`, `trend_map` and `loadings_prior` are now
  > inherited, the last through `denormalise_loadings_prior()`, since
  > `normalise_loadings_prior()` allow-lists the user-facing names and
  > will not take the resolved spec back. The table gained getters for
  > the nested paths, and skips an argument whose getter finds
  > nothing, because absent and `NULL` differ.
  >
  > `mvgam_update_uninherited` names the rest with a reason:
  > `knots`, `sample_prior`, `sparse`, `normalize`,
  > `drop_unused_levels` and `stan_funs` are stored nowhere;
  > `stanvars` is stored with mvgam's own mixed in, so re-passing
  > double-injects; `data2`, `combine` and `run_model` do not apply;
  > `save_model`, `silent` and `validate` change nothing about the
  > model. A test asserts every argument reaching the code generator
  > appears in one list or the other, that none appears in both, and
  > that every reason says something. It caught three arguments within
  > minutes of existing.
  >
  > Two getters were wrong when first written and the local test
  > caught both. `threads` is a `brmsthreads` object rather than a
  > count, so handing it back tripped an integer assertion on every
  > refit. And a `trend_map` written on the trend constructor already
  > travels inside `trend_call`, so supplying it at the top level too
  > is a collision `mvgam()` refuses; it is withheld when
  > `trend_call_names_arg()` finds it there. A name-level guard sees
  > neither, which is why the fixture-driven test earns its place.
  >
  > Six arguments still cannot be carried, so a refit of a fit that
  > set `knots` or `sparse` still differs silently. Closing that needs
  > them stored at fit time, which is 3.8.

- [x] **3.4 `update()` on a `jsdgam` returned something else entirely**
  > There is no `update.jsdgam`, so `update.mvgam` ran on a
  > `c("mvgam", "jsdgam")` object and ended at
  > `do.call(mvgam, call_args)`, where none of `factor_formula`,
  > `n_lv`, `traits`, `trait_slopes`, `phylo`, `species` or `unit`
  > survives. On `val_jsdgam_trait` the rebuilt trend call is `~ -1`,
  > so the refit carried no factors, no traits and no phylogeny.
  >
  > `update()` now refuses a `jsdgam`, naming what cannot be recovered
  > and pointing at `jsdgam()`. It does not make the refit work:
  > `jsdgam_call` records `formula`, `factor_formula`, `data`,
  > `family`, `unit`, `species` and `trait_slopes` as the symbols the
  > user wrote, so there is nothing to replay. A working
  > `update.jsdgam` needs 3.8.

- [x] **3.5 Multiple imputation dropped the family**
  > The imputation path forwarded eight names. `family`, `trend_map`,
  > `loadings_prior`, `threads` and `run_model` are formals of
  > `mvgam()` rather than dots, so they reached neither the forwarding
  > call nor `...`, and
  > `mvgam(y ~ x, data = <imputed frames>, family = poisson())` fitted
  > gaussian on every imputation.
  >
  > `mvgam_imputation_forwarded` names the set and the call builds
  > from it with `mget()`. `threads` still travels only when set,
  > since an explicit `NULL` trips the integer assertion downstream. A
  > test asserts the set equals `formals(mvgam)` minus the two the
  > call supplies itself.

- [x] **3.6 Three documented `jsdgam()` arguments do nothing**
  > All three were forwarded to names `mvgam()` has no formal for, so
  > each landed in `...` and was dropped without a word.
  >
  > `share_obs_params` was a 1.x argument for a 1.x model. Master gave
  > every series its own family parameter and used the flag to collapse
  > them; brms gives one shared parameter and reaches per-series ones
  > through a distributional sub-formula, so the argument inverts the
  > 2.0 default and has nothing to implement.
  >
  > `factor_knots` was forwarded as `trend_knots`, which master had as
  > a real formal and 2.0 does not. Knot values are named by covariate
  > rather than by formula, so the split the two arguments encoded is
  > not one 2.0 needs. Removed. Checking that `knots` covers the
  > smooths on both formulas is what turned up 3.11.
  >
  > The pinned `trend_model = ZMVN(cor = TRUE, subgr = "series")` was
  > the luckiest of the three. Had it ever landed the fit would have
  > failed outright, since `subgr` without `gr` is refused at
  > `validate_grouping_arguments()`. It also asked for nothing the
  > default gives: `factor_formula = ~ -1` resolves to `ZMVN()`, whose
  > `cor` cannot be anything but `TRUE`.
  >
  > Nine 1.x arguments were being swallowed this way, `trend_model`
  > among them, which is the one a user is most likely to write.
  > `mvgam_removed_args` names each with the 2.0 way of asking for the
  > same thing and `reject_removed_args()` refuses them in `mvgam()`,
  > the call every fitting path funnels through, `jsdgam()` included.
  > A test asserts no name in the table is still a formal of either
  > function, since that would make the refusal unreachable.

- [x] **3.7 The specification prior table has blank rows**
  > Reproduced first: `PW()` reported nothing for `delta_trend` while
  > the Stan sampled it under `double_exponential(0, 0.05)`, and
  > `VAR(ma = TRUE)` reported nothing for `Amu_trend`, `Aomega_trend`,
  > `Dmu_trend` or `Domega_trend` against the normal and gamma priors
  > it emits. Same defect `sigma_trend` carried in 3.1, on the mirror
  > path: a reader deciding what to override saw nothing to override.
  >
  > The cause was two resolver chains rather than a missing entry.
  > `get_trend_parameter_prior()` read defaults for the Stan generator
  > and `get_default_trend_parameter_prior()` built the table the user
  > is shown, and each emission site carried its own literal fallback
  > the table knew nothing about. There is one chain now: the codegen
  > reader takes the user's prior where there is one and asks the
  > table's resolver for everything else, so the two surfaces cannot
  > answer differently.
  >
  > The four VARMA hyperpriors are registry entries. `delta_trend` is
  > not, because its scale is the user's own `changepoint_scale`
  > argument; it resolves through `get_pw_parameter_prior()`, the same
  > `get_<trend>_parameter_prior()` convention `AR()` and `CAR()`
  > already use for their bounds. Seven literal fallbacks went with
  > them, two of which restated registry entries that happened still
  > to agree. Two more sat beside those, defaulting `n_changepoints`
  > to 5 and `changepoint_scale` to 0.1 where `PW()` sets 10 and 0.05;
  > both were dead, and both are gone rather than corrected, since the
  > constructor is the one source.
  >
  > Found while testing the fix: `init_trend` was reaching fitted
  > prior tables. It holds the states before the first observed time
  > and is drawn from the stationary distribution the autoregression
  > implies, so its statement is a function of `A_trend` and
  > `Sigma_trend`, not a prior anyone can set. It is a state and now
  > sits with the others the scanner excludes. The list carries tests
  > for what it lifts and, until now, none for what it leaves, so any
  > new state ending in `_trend` walks straight through. One test now
  > asserts every name on the list stays out even when the Stan hands
  > it a sampling statement.
  >
  > The test that guarded this was a loop over the shared defaults
  > asserting the two resolvers agreed, which one chain makes
  > tautological, plus a reported-equals-sampled check on a single
  > `AR()` fit. Both are replaced by one loop running that check over
  > nine trend configurations, parsing the Stan with the package's own
  > `mvgam_stancode_prior_rows()` rather than a regex written a second
  > time for the test. A trend type is where this defect hides, so the
  > coverage had to be per trend type.

- [x] **3.8 A fit did not record what it was built from**
  > What 3.3 and 3.4 both stopped at. The object kept the values the
  > code generator needed and not the arguments the user gave, so
  > several of `mvgam()`'s could not be carried into a refit.
  > `object$call` does not rescue it: `match.call()` over a `do.call`
  > frame leaves every argument a bare symbol or `..1`.
  >
  > 3.11 made this cheap and shrank it. Six arguments were withheld
  > from `update()` on the grounds that no fit stores them; two of the
  > six are gone, and the other four are worth storing only now that
  > they reach brms. The fit keeps the one options list it was
  > generated under, and `update()` reads all four off that slot
  > through one loop rather than four near-identical registry entries.
  > A fit made before the slot existed returns NULL and takes the
  > `mvgam()` default, which is what it did before.

- [x] **4.0 The likelihood was scored on the wrong surface**
  > The `loo_R2` failure and the `p_loo` question were one cause.
  > `log_lik()` integrated over the trend dynamics instead of reading
  > the state the model inferred at each time, so every ELPD built on
  > it described a series the model never saw. On the fixed-effect
  > Poisson pair that put `p_loo` at 784 against 30 observations,
  > `elpd` at -898 against brms's -90, and `loo_R2` on its clamp at
  > -1 against 0.82. Decision 22 already assigned model comparison to
  > the conditional surface, and the likelihood was the one member of
  > that group still on the marginal one, with `residuals()` and
  > `pp_check()` beside it already reading the state. Conditioning
  > brings `elpd` to -90.35 against -89.78, `p_loo` to 19.9 against
  > 19.2, and `loo_R2` to 0.79 with an interval covering brms's. The
  > argument is spelled `incl_autocor` as brms spells it;
  > `process_error` and `incl_dynamics` are still accepted and lose
  > to it when both are given.
  >
  > Two defects surfaced underneath. The trend state was looked up by
  > position within whatever frame it was handed, so scoring a later
  > window of a series read the state of an earlier one, at the right
  > shape and without complaint; the lookup now runs on raw times
  > against the fitted grid. And `posterior_epred()` and
  > `posterior_predict()` each drew a second, independent set of
  > innovations on top of the set `get_combined_linpred()` had already
  > composed, so a marginal prediction carried twice the process
  > variance and, through a non-identity link, a mean biased upward
  > with it. One place samples them now, which moved `bayes_R2` from
  > 0.63 to 0.899 against brms's 0.897.
  >
  > `loo_predict()`, `loo_epred()` and `loo_linpred()` reweight a
  > prediction by importance weights, so they had the same pairing to
  > answer for. brms hands one set of arguments to both halves and
  > lets each carry its autocorrelation term, so mvgam takes the
  > prediction under the state the weights were built from. Against
  > the brms twin `loo_predict()` correlates 0.921 with the
  > observations where brms reaches 0.929, and the two answers sit
  > 1.4 apart on counts spanning 0 to 48.
  >
  > The two `update(recompile = FALSE)` errors were read here as the
  > guard working on a stale fixture, and as clearing on a rebuild.
  > Both readings were wrong; see 3.1. The stancode diff is one line
  > and it is a prior, not the initial state, and a fixture built
  > after this note reproduced the fault exactly.

- [x] **13.0 One idea, one name**
  > A survey of the exported surface against the conventions the
  > architecture doc records. The prior work fixed the cases where a
  > name hid a wrong number; these were the ones where it hid a
  > second meaning.
  >
  > The prediction surface was selected by two independent things
  > wearing five names between them. One axis picks whether a
  > prediction reads the state the model inferred or answers from the
  > two submodels' covariate structure; the other picks whether the
  > second of those samples innovations. `incl_autocor` is now the
  > first everywhere it can be chosen and `process_error` the second,
  > with `trend_state` demoted to the internal name
  > `get_combined_linpred()` reads and one function translating
  > between them. `predict()` and `fitted()` had hidden the surface
  > in `...`; both now name it.
  >
  > The defaults disagreed where it mattered most. `predict()`
  > defaulted `process_error = FALSE` and forwarded to
  > `posterior_predict()`, which defaulted `TRUE`, so one fit gave
  > two answers with no argument given. All five prediction entry
  > points now default `FALSE`, which is the counterfactual reading
  > decision 22 assigns them; the ELPD surfaces keep
  > `incl_autocor = TRUE`, which is the conditional one. A test
  > asserts both sets of defaults, so a sixth entry point cannot be
  > added on its own terms.
  >
  > Four documented behaviours were not the code's. `fitted()`,
  > `predict()`, `conditional_effects()` and the marginaleffects
  > section each said `process_error = FALSE` fixes the trend at its
  > posterior mean; it removes the latent state and leaves every
  > coefficient varying. `posterior_linpred()` said innovations are
  > added only by `posterior_predict()`. `obs_formula` was documented
  > in `lv_axis.R` and `data_helpers.R` as an argument neither
  > `mvgam()` nor `jsdgam()` has. The docs now also say when the
  > default surface has little to show, and point at `hindcast()` and
  > `forecast()` for a fit whose covariates carry little signal.
  >
  > The rest were one token carrying two meanings.
  > `summary(include_states =)` became `include_trend_states` and
  > `is_latent_state_param()` became `is_trend_state_param()`, since
  > `latent_state` is the closure-unit quantity everywhere else. A
  > `trend_state` local in `forecast.mvgam()` became
  > `fitted_states`. `residual_cor(groups =)` became `by_group`,
  > being a logical where `groups` is a character vector elsewhere.
  > `posterior_transition_matrix()` had `group` and `groups` as
  > mutually exclusive spellings and now takes `groups` alone,
  > returning one matrix for one panel and the classed list for
  > several. `score(alpha =)` became `quantile_level`.
  > `forecast(b_uncertainty =)` became `coef_uncertainty`, since it
  > fixes smooths, random effects and GP bases as well as `b`.
  >
  > Two collisions were left alone on purpose, both being another
  > package's contract rather than mvgam's: `ranef(groups =)` and
  > `hypothesis(alpha =)` are spelled and used exactly as brms
  > spells them, and `ordinate(alpha =)` follows the BORAL
  > convention its own documentation cites.
  >
  > The architecture doc's decision 22 records the two axes and the
  > single translation. Decisions 4, 5 and 8 showed pre-split
  > lowercase Stan dimensions in their code blocks; those now match
  > what the assembler emits.

- [ ] **13.1 The migration the names still need**
  > Deferred from 13.0 because each renames something a fitted object
  > carries, so cached fixtures stop being readable until they are
  > rebuilt. Belongs with 5.0's refit rather than ahead of it.
  >
  > `trend_model` means three things. It is a constructor when passed
  > (`sim_mvgam(trend_model = AR())`), a character type name on a
  > trend spec, and a `brmsfit` on the fitted object: `class(
  > fit$trend_model)` is `"brmsfit"`, the trend-side prefit, across
  > 74 read sites. `tidier_methods.R:170` reaches
  > `x$trend_model$trend_model`, which is the collision in one
  > expression. `trend_prefit` is the name that describes the slot.
  >
  > `process_error` still means two things. 13.0 gave it the
  > innovation axis everywhere, but `log_lik()` keeps it as the
  > superseded spelling of the surface, where innovations are pinned
  > off. Back-compatibility is the reason and `incl_dynamics` is the
  > 1.x name that would serve instead.
  >
  > `trend_arg_metadata` names the trend-constructor arguments whose
  > values a fit stores, `trend_map` and `n_lv`, and
  > `mvgam_update_inheritance` names the top-level ones. Those are two
  > lists of one idea, an argument whose value the object can give
  > back, and nothing makes them agree. A constructor naming any other
  > out-of-scope variable, `AR(p = 1, gr = my_grouping)` say, still
  > fails on refit.
  >
  > Three Stan names still break the `_trend` suffix rule:
  > `time_dis`, `theta_features` and `varrho_inv`. `time_dis` reaches
  > into the compiled C++ signatures in `RcppExports.R`, and the
  > other two are parameters, so renaming them changes posterior
  > column names and every fit carrying a structured loadings prior
  > stops being readable. `N_free_Z` puts its qualifier before the
  > noun where every sibling puts it after.

- [x] **3.11 Seven documented arguments never reached brms**
  > `build_stan_components()` threaded `knots`, `sample_prior`,
  > `sparse`, `normalize`, `drop_unused_levels`, `stan_funs` and
  > `save_model` into `setup_brms_lightweight()`, whose `brms::brm()`
  > call names every argument it passes and reads the dots for
  > `threads` alone, so all seven were dropped. Reproduced before
  > changing anything: the basis matrices were byte-identical with and
  > without `knots`, `prior_only` stayed 0 under
  > `sample_prior = "only"`, `normalize = FALSE` left the program
  > untouched, `drop_unused_levels = FALSE` still gave `K = 2`, and
  > `save_model` wrote nothing.
  >
  > The same list has to reach four brms calls, `brm()`,
  > `make_stancode()`, `make_standata()` and `validate_prior()`, and
  > repeating it four times is how it would drift again. One list is
  > built by `mvgam_codegen_options()`, carried on the setup object
  > beside `data2` and `threads`, and spliced into each call by
  > `codegen_args_for()`, which takes only the names that generator
  > declares. The prior table is merged under the same options, since
  > it is keyed by the coefficients the design matrix holds:
  > `drop_unused_levels = FALSE` prices a level the default table has
  > no row for, and brms rejects the whole table.
  >
  > Two of the seven are deprecated by brms itself, which reads
  > `sparse` off `bf(y ~ x, sparse = TRUE)` and replaced `stan_funs`
  > with `stanvars`. Resurrecting either would mean carrying an
  > argument brms warns about, so both are named in
  > `mvgam_removed_args` with a pointer instead, and
  > `reject_removed_args()` now runs at the `stancode()` and
  > `standata()` boundaries as well as `mvgam()`'s. `save_model` writes
  > the assembled program rather than the brms one it starts from,
  > since the assembled one is what gets compiled and the only one that
  > names the trend.
  >
  > A defect surfaced underneath. `get_stan_reserved_words()` held
  > every `_lpdf` and `_lpmf` Stan density and no `_lupdf` or `_lupmf`,
  > and three patterns in `filter_block_content()` had the same gap.
  > Under `normalize = FALSE` brms emits the unnormalised spelling
  > throughout, so brms's copy of the trend `sigma` prior survived the
  > filter and the renamer suffixed the function rather than the
  > parameter, giving `student_t_lupdf_trend(sigma | 3, 0, 2.5)`, which
  > Stan refuses. The unnormalised names are derived from the
  > normalised ones and the density-call fragment is written once.
  >
  > Making `normalize` live then exposed a second copy of the same
  > gap, in the GLM path. brms folds the linear predictor into a
  > `*_glm_lupmf` call under `normalize = FALSE`, and mvgam's detection
  > spelled `_l(pdf|pmf)` by hand at thirteen sites across three files,
  > so the injector could not find the likelihood and assembly aborted
  > on any fit with a trend and a predictor. All thirteen go through
  > one pattern builder, and a rewritten call now keeps the spelling it
  > replaced rather than a fixed `_lpdf`, since writing the normalised
  > form back would restore the constants the user turned off.
  >
  > Three sibling gaps came out of the same survey. The Stan data was
  > generated without the `prior` its program was generated with, so a
  > `horseshoe()` or `R2D2()` fit with a trend declared six data
  > variables nothing supplied; `threads` was missing for the same
  > reason and would have declared `grainsize` without it. And
  > `get_prior()` built its trend half without the options its
  > observation half was given, so the table named coefficients the fit
  > would not estimate, and omitted ones it would. The per-response
  > expansion deliberately takes no prior: it rebuilds one `bf()` arm at
  > a time, and the combined prior names parameters a single arm has no
  > parameter for.
  >
  > Verified across 56 trend-by-family-by-`normalize` programs, the
  > multivariate arm, `jsdgam(knots = )`, and the trend-free path,
  > which takes its program and data straight off the mock fit. The
  > mock fit stores its basis, so `posterior_smooths()` rebuilding a
  > prediction grid reuses the knots rather than a default basis.

- [x] **3.9 The class pages described the wrong objects**
  > `?mvgam-class` was a 1.x page. Seventeen of its twenty-two slots
  > were absent from a fit and twenty-three a fit carries went
  > unmentioned, two of the seventeen described in terms of a
  > `return_model_data` argument 2.0 does not take. It is rewritten
  > from the constructor: the fit and its data, the Stan program, the
  > specification prediction reads, how it was fitted, and the four
  > slots `jsdgam()` adds on top.
  >
  > `?mvgam_irf-class` and `?mvgam_fevd-class` described the draws
  > alone, where both functions return a summary and keep the draws
  > behind `summary = FALSE`. Both now name the class of each form,
  > the columns of the summary and the argument that reaches the
  > draws. `?mvgam_residcor-class` called `mean_abs_offdiag` a
  > scalar where it is a point and an interval.
  >
  > `plot(type = "precision")` was offered and could not run: it
  > reads `sig_prec`, which nothing populated, and its error named
  > `compute_precision`, an argument `residual_cor()` does not take.
  > The partial correlations were being summarised on the native
  > scale though they are bounded like any correlation, so they took
  > neither the Fisher-z interval the correlations take nor the
  > thresholding that produces `sig_prec`. One summariser now serves
  > both, which supplies the missing slot and the evidence fields
  > beside it.
  >
  > A test reads the bullets out of each class page and checks them
  > against objects built from cached fits, so a slot added later
  > cannot go undocumented in silence.

- [x] **3.12 The figures did not agree on a theme or a palette**
  > Four of roughly twenty plot methods applied `theme_bw()` or
  > `theme_classic()` in place of `mvgam_theme()`, so an impulse
  > response drawn from its summary and one drawn from its draws came
  > out under different looks. Twelve hex codes across four files
  > were bayesplot scheme entries written by hand; `mvgam_colour()`
  > reads them by role instead. Three of those files then had no
  > `set_color_scheme_local("red")` where the other twelve plot
  > methods do, which would have left them the only figures following
  > a user's own scheme, so they pin it too.
  >
  > The hindcast arm of a forecast plot rebuilt the ribbon
  > `mvgam_band_layer()` already builds, differing only in wanting
  > one flat fill; the helper takes a `fill` and the arm is one call,
  > with the bounds checked identical. Two locals fell dead with it,
  > and a third, `ribbon_outer` in the latent-state plot, was
  > computed and read by nothing.
  >
  > Two tests hold the line: one fails if any file outside
  > `plot_helpers.R` applies a ggplot2 theme, the other if any of
  > them writes a colour that a bayesplot scheme carries.

- [x] **3.13 The correlation heatmap drew a threshold, not an estimate**
  > `plot.mvgam_residcor()` drew `sig_cor`, where every pair whose
  > `Pr(sign)` misses 0.95 is set to zero, under a legend reading
  > "Posterior correlation". On `val_mvgam_var_cor` a correlation of
  > 0.399 came out white, indistinguishable from zero. On
  > `val_jsdgam_trait` all 28 species pairs were zeroed and the panel
  > was blank, while the model estimates a largest absolute
  > correlation of 0.433.
  >
  > The surrounding packages were read before choosing a
  > replacement. `Hmsc::computeAssociations()` returns the mean and
  > the support and thresholds nothing, leaving the cut to user code.
  > `gllvm::getResidualCor()` returns the matrix untouched.
  > `corrplot` offers five treatments for an unsupported entry and
  > defaults to keeping the estimate and marking it, its hiding
  > option blanking the glyph rather than repainting it at the
  > midpoint. mvgam was the only one whose plot applied the cut with
  > no way to reach the estimate.
  >
  > bayesplot settled it. Across 165 exports it carries no
  > significance machinery at all, and encodes uncertainty as nested
  > intervals whose defaults deliberately avoid 95%. Every other
  > mvgam figure already follows that through
  > `mvgam_band_layer(probs = c(0.5, 0.8, 0.95))`; the heatmap was
  > the one surface that dichotomised.
  >
  > Fill now carries the posterior median and opacity carries
  > `Pr(sign)`, cut at 0.75 and 0.95 into three ordered levels the
  > way `bayesplot::mcmc_rhat()` cuts a diagnostic at 1.05 and 1.1.
  > The evidence legend reads on its own greyscale, since it grades
  > confidence rather than correlation, and sits below the colour
  > bar. Only the lower triangle is drawn. `rescale` swaps the fixed
  > `[-1, 1]` for the data range; the fixed default keeps panels
  > comparable and stops 0.4 looking like 1. `sig_cor` stays on the
  > object.
  >
  > `plot.mvgam_residcor_list()` reads the same way, through shared
  > helpers, so the two heatmaps cannot drift apart.
  >
  > Two mechanics were not obvious. ggplot builds a legend key from
  > the rows a layer holds, so on a fit where nothing clears 0.75 the
  > other two levels drew labels with no swatch; neither
  > `drop = FALSE` nor naming the scale's `limits` fixes it, and an
  > invisible row per level does. And dropping the upper triangle
  > takes one level off each axis, so both are held open or the panel
  > silently loses a row and a column.

- [x] **3.10 `info =` in expectations**
  > Forty sites across seven files, not the twenty across six the
  > survey estimated, and testthat takes `info` on only some of the
  > expectations they appear on. Where the message was static prose it
  > became a comment above the expectation. Where a loop needed to name
  > the failing case a comment cannot, so it became `label =`, which
  > `test-marginaleffects-insight.R`, `test-loo-extras.R` and
  > `test-pp-average.R` already use for exactly that.

- [ ] **3.14 The codegen path's remaining duplication**
  > Found by surveying the code-generation path for the shape 3.11
  > fixed, two or more places that must agree with nothing keeping them
  > in step. Everything reachable through 3.11's arguments was fixed
  > there; what follows is not, and belongs in its own commit.
  >
  > The six GLM function names are listed four times over:
  > `glm_patterns` in `detect_glm_usage()`, the byte-identical vector in
  > `detect_glm_patterns()`, and the type branch in each of
  > `transform_glm_call()` and `transform_single_glm_call()`. Adding a
  > seventh form to one gives detection without transformation, or the
  > reverse. The first two are the same function under two names, and
  > both delegate to the same `map_responses_to_glm()`.
  >
  > The two `setup_brms_lightweight()` calls in
  > `build_stan_components()` name ten arguments each and differ in
  > five. That is the shape 3.11 was, one layer up: an argument added to
  > the observation call and not the trend one builds the two submodels
  > under different settings and says nothing. `codegen` is now the only
  > thing they cannot disagree about.
  >
  > `stancode.mvgam_formula()` and `standata.mvgam_formula()` restate
  > `build_stan_components()`'s defaults rather than inheriting them,
  > and disagree about which arguments exist. `standata()` has no
  > `validate` formal, so it parse-validates on every call while
  > `stancode(validate = FALSE)` does not.
  >
  > `silent` is declared as a logical defaulting to `TRUE` in
  > `R/stan_polish.R` and `validate_stan_code()`, and as an integer
  > everywhere else. The callers pass integers into the logical
  > parameters, which is harmless because the value is finally read as
  > `silent > 0L` and R counts `TRUE` as one, so both spellings pick
  > the same branch. Nothing prints from any of them now, so this is
  > naming rather than behaviour. `validate_time_series_for_trends()`
  > declares a `silent` it never reads, and three `insight` methods
  > declare a `verbose` they never read.
  >
  > Thirteen functions have no caller anywhere in `R/` or `tests/`:
  > five in the nonlinear-model block of `R/brms_integration.R`,
  > `transform_glm_calls_post_processing()` and `transform_glm_call()`
  > in `R/stan_assembly.R`, `map_trend_priors()` and
  > `get_trend_prior_spec()` in `R/priors.R`, and
  > `validate_stan_code_structure()`, `are_braces_balanced()`,
  > `parse_data_declarations()` and `validate_autocor_separation()` in
  > `R/validations.R`. `transform_glm_calls_post_processing()` would
  > error if it were called: its body reads a free variable that is
  > neither a parameter nor bound locally. Deleting these needs a check
  > against dynamic dispatch first, since the trend generators and the
  > per-trend prior getters are reached by name through `paste0()`.

- [x] **3.15 The generator narrated itself, and could not be told not to**
  > "Validating combined Stan code..." printed before a step that takes
  > no perceptible time, on every `mvgam()`, `jsdgam()`, `stancode()`,
  > `standata()` and `update()` call, and no `silent` value reached it:
  > the caller hardcodes `silent = 1`. A ten-fold `kfold()` printed 41
  > lines, of which `silent = 2` removed 31.
  >
  > The rule is brms parity, so each survivor was checked against
  > brms's own namespace rather than argued for. brms prints two lines
  > during a fit, "Compiling Stan program..." and "Start sampling", and
  > nothing at all when it generates code or a prior table. Deleted:
  > the validation notice, the compile and sampling announcements
  > mvgam made a second time on top of the backend's, `pp_check()`'s
  > six draw-count notices, the registration notice
  > `register_custom_trend()` printed to say it was doing what it was
  > asked, and a `cat()` writing loop bookkeeping to stdout where
  > `suppressMessages()` could not reach it.
  >
  > Two went the other way. Multiple imputation's progress lines were
  > dead: `insight::format_message()` returns its string without
  > printing, unlike `format_warning()`, so a pooled fit ran silently
  > while the source looked instrumented. `brms::brm_multiple()`
  > reports each imputed fit and gates it at `silent < 2`, so this now
  > does the same. And `stancode()` documented the levels backwards,
  > promising that 0 was quiet when 0 is the noisiest setting.
  >
  > `silent` now travels on the fit, so a refit is as quiet as the call
  > it rebuilds rather than reverting to the default.
  >
  > Verified by capturing both streams separately, since a `cat()`
  > survives `suppressMessages()` and would not show up otherwise.
  > Code generation is silent; `mvgam()` and `jsdgam()` print "Start
  > sampling" by default and nothing under `silent = 2`; `update()`
  > with no `silent` named inherits the quiet of the fit it came from;
  > and `pp_check()`, `residual_cor()`, `posterior_predict()` and
  > `summary()` say nothing. `loo_predict()` still announces PSIS,
  > which is deliberate: brms announces it too and gives no argument to
  > turn it off, so neither does this.

- [x] **3.16 Two tests never turned on the thing they tested**
  > `posterior_predict()` defaults to `process_error = FALSE`. Both
  > process-error tests in
  > `tests/local/test-predictions-brms-concordance.R` asked for the
  > default and then for `FALSE`, so each compared the marginal surface
  > against itself and rested on the two seeds. One asserted a strict
  > inequality and had been failing on noise; the other took a ratio
  > wide enough to pass either way. The flag itself is sound, and
  > turning it on raises the mean predictive variance from 218 to 3993
  > on the cached Poisson fit.
  >
  > Third instance this release of a test passing without exercising
  > its subject, after `normalize = FALSE` on an intercept-only formula
  > that never reaches the GLM path, and the class-documentation parser
  > that matched no slots. Worth a pass at 6.0 over any test whose name
  > names a toggle.

- [ ] **5.0 Rebuild every vignette and the pkgdown site**
  > Caches date from June and July, before the prior and default
  > changes. Roughly 60 numeric claims need re-checking, and
  > `nmix.Rmd` has never rendered its chunks at all.

- [ ] **6.0 Final release verification**
  > Clean `document()`, clean test sweep, `R CMD check --as-cran`,
  > tarball under the size limit, sweep green.

## Behaviour confirmed, worth documenting

`posterior_epred()` and `posterior_predict()` default to
`process_error = FALSE` and `incl_autocor = FALSE`, so the trend
contributes its deterministic submodel and the answer is the same on
every call. Passing the training data back as `newdata` is an exact
no-op. Under `process_error = TRUE` innovations are drawn afresh, so
two calls on one fit and the same `draw_ids` differ, by as much as
160 units on a cached Poisson fit; reproducible output there needs a
seed.
