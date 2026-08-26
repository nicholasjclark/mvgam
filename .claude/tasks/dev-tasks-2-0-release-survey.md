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

**Still to modify** — `R/log_lik.mvgam.R`, `R/residuals.mvgam.R`,
`R/loo.mvgam.R` for the addition terms; `tests/local/build_fixtures.R`
for the six fixtures no builder regenerates.

**Notes** — long commands run in the background and write to a log
that is then read. Fixture-dependent tests live in `tests/local/`; CI
tests never skip on a missing fixture and the CI sweep stays under ten
minutes. Cached fits are read once, never re-fitted to inspect.

## Tasks

- [x] **0.0 Fixed during the survey**
  > Each was reproduced against a cached fit before being changed, and
  > re-checked after. Full CI suite: 6180 pass, 0 fail.

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

- [ ] **3.0 Close the post-fit coverage gaps**
  > `plot_slopes`, `plot_comparisons`, `hypotheses`,
  > `posterior_transition_matrix`, `latent_N_saturation` and
  > `compare_elpds` are never called on a fitted model anywhere;
  > `tweedie()` is never fitted; the five multi-response families are
  > fitted but barely inspected; offsets and new-level prediction have
  > no coverage. Six fixtures have no builder.

- [ ] **4.0 Resolve the pre-existing local-suite failures**
  > Three reproduce on a clean checkout of `HEAD`: a `loo_R2`
  > concordance gap against brms and two `update(recompile = FALSE)`
  > errors. Separately, `p_loo` of 669 against n = 30 on a cached fit
  > wants a statistical opinion.

- [ ] **5.0 Rebuild every vignette and the pkgdown site**
  > Caches date from June and July, before the prior and default
  > changes. Roughly 60 numeric claims need re-checking, and
  > `nmix.Rmd` has never rendered its chunks at all.

- [ ] **6.0 Final release verification**
  > Clean `document()`, clean test sweep, `R CMD check --as-cran`,
  > tarball under the size limit, sweep green.

## Behaviour confirmed, worth documenting

`posterior_epred()` and `posterior_predict()` default to
`process_error = TRUE`, marginalising over the trend by drawing fresh
innovations, so two calls on the same fit and the same `draw_ids`
differ — by as much as 160 units on a cached Poisson fit. brms's
equivalents are deterministic, so this will surprise people;
reproducible output needs a seed. On the deterministic path, passing
the training data back as `newdata` is an exact no-op.
