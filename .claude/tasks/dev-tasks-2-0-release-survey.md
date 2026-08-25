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
