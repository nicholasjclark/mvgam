# Plan: gate brms-native threading on a trend formula being absent

**Tracks:** task #411 — _threads_per_chain is a no-op for VAR/AR/RW fits (no reduce_sum codegen)_

## Why this is a real problem

Today, a user who writes
```r
mvgam(y ~ 0, trend_formula = ~ VAR(...),
      data = ..., family = gaussian(),
      threads_per_chain = 2)
```
gets one of two bad outcomes, depending on how the obs-formula collapses:

1. **Silent no-op.** `threads_per_chain = 2` flows down to brms, brms emits
   `partial_log_lik_lpmf` + `reduce_sum` in the `functions {}` block. mvgam's
   trend rewriter (`inject_trend_into_model_block_for_response()`,
   `R/stan_assembly.R:1571-1602`) goes looking for `mu[n] = ...` in the
   `model {}` block, falls into the `y ~ 0` placeholder branch, and prepends
   a `for (n in 1:N) mu[n] = trend[...]` loop after `vector[N] mu =
   rep_vector(...)` in the model block. With threading on, that declaration
   has been moved into the `partial_log_lik_lpmf` function, so the loop is
   inserted into a model block that never declares `mu`. Whether this
   compiles depends on the exact order of stanvars; in the Savage case it
   compiled but `reduce_sum` was never called from a parallel context, so
   `threads_per_chain = 2` ran 1 LWP per chain.
2. **Hard crash.** A non-zero obs formula (`y ~ x`) plus a trend plus
   `threads = 2` errors immediately with
   `"No mu[n] assignment patterns found in nonlinear model block.
    Expected pattern: mu[n] = <expression>;"`
   because the rewriter's main path can't find the assignment that brms
   moved into the function.

Both modes are user-hostile. Until the rewriter understands brms's
threaded code shape, we should detect the combination and refuse to ask
brms for threading.

## Decision

Option A (chosen): when threading is requested on a brms-native fit that
carries a trend formula, emit a one-time warning explaining the
limitation and force unthreaded codegen. Do not change closure-unit
families (their own `partial_sum_<family>_lpmf` is independent of the
brms partial-log-lik path).

## Where to gate

Single chokepoint: `R/brms_integration.R:230-239`, the block that maps
`raw_threads -> brm_threads`. Today:
```r
brm_threads <- if (is.numeric(raw_threads) &&
                     isTRUE(raw_threads > 1)) {
  as.integer(raw_threads)
} else {
  NULL
}
```

After:
```r
brm_threads <- if (is.numeric(raw_threads) &&
                     isTRUE(raw_threads > 1)) {
  as.integer(raw_threads)
} else {
  NULL
}
if (!is.null(brm_threads) && !is.null(trend_formula) &&
    !is_closure_unit_family(family)) {
  if (!identical(Sys.getenv("TESTTHAT"), "true")) {
    rlang::warn(
      paste0(
        "`threads_per_chain` is ignored for brms-native families ",
        "with a `trend_formula`. mvgam's trend rewriter and brms's ",
        "`partial_log_lik_lpmf` placement are not compatible; ",
        "see issue #411. Closure-unit families ",
        "(`occ()`, `nmix()`) thread independently and are unaffected."
      ),
      class = "mvgam_threading_no_op",
      .frequency = "once",
      .frequency_id = "mvgam_threading_no_op"
    )
  }
  brm_threads <- NULL
}
```

`is_closure_unit_family()` already exists in `R/make_stan.R:102`; expose
it via `@noRd` or use `mvgam:::` (it is internal, so the bare name is
fine inside the package).

## Wiring required

`setup_brms_lightweight()` already receives `trend_formula` and `family`
as named args. No new plumbing.

## Tests

Add three blocks to `tests/testthat/test-stancode-standata.R`:

1. **brms-native + trend + threads suppresses threading.**
   `mvgam_formula(y ~ x, trend_formula = ~ AR(p = 1))` + `threads = 2L`.
   - `expect_warning(stancode(...), class = "mvgam_threading_no_op")`
     (outside the testthat env-suppressed path; this test sets
     `Sys.setenv(TESTTHAT = "")` temporarily, or test the predicate
     directly).
   - `expect_false(any(grepl("reduce_sum", code)))`.
2. **brms-native + no trend + threads still threads.**
   `mvgam_formula(y ~ x)` + `threads = 2L` -> stancode contains
   `reduce_sum`.
3. **Closure-unit + trend + threads still threads.** Pick an existing
   closure-unit fixture (e.g. `nmix("poisson_binomial")`) +
   `trend_formula = ~ AR(p = 1)` + `threads = 2L` -> stancode contains
   mvgam's own `partial_sum_*_lpmf` AND `reduce_sum`.

## Documentation

1. `R/mvgam_core.R` `threads` roxygen (line ~177): one-sentence note that
   threading is currently suppressed for brms-native families with a
   `trend_formula`, with a forward reference to issue #411.
2. `R/jsdgam.R` mirror clause for jsdgam.
3. `architecture/architecture-decisions.md`: add a short section under
   "Threading" describing why the gate exists and the conditions for
   removing it (rewriter learns to splice into `partial_log_lik_lpmf`).

## Why we don't extend the rewriter (Option B) right now

Splicing trend into `partial_log_lik_lpmf` requires:
1. Detecting that brms put `mu` and the linpred assignments in the
   function block.
2. Computing the slicing offset (`start:end` for the partial slice).
3. Threading `obs_trend_time[start:end]` and `obs_trend_series[start:end]`
   through the `partial_log_lik_lpmf` signature (which brms hardcodes).
4. Repeating the work for every brms family path that we want to thread.

That is at least a day of work plus extensive test coverage; it is the
right long-term answer (and is the only way to actually speed up
gaussian + s() + AR fits, which we expect to be a common shape for
ecological vignette readers), but it does not belong in the same patch
as the silent-no-op fix.

## Why threading won't help Savage anyway

The Savage hierarchical-VAR likelihood is dominated by
`multi_normal_lpdf(lv_trend[t, :]' | mu_t_trend[t], Sigma_trend)`
inside a sequential `for (t in 1:N_time_trend)` loop
(`R/stan_assembly.R:5050-5071`). `reduce_sum` parallelises across
observation rows; for VAR the heavy work is per-time-step, not
per-observation, so even a correct rewriter port wouldn't help the
Heaps stationarity + 24x24 Sigma cost that dominates the Savage fit
per gradient.

## Out of scope here

- Option B (teach rewriter to inject into `partial_log_lik_lpmf`).
- Option C (mvgam-side `partial_sum_trend_lpmf` for brms-native).
- Any change to closure-unit threading.

## Done definition

- One-time `mvgam_threading_no_op` warning fires on the gating combo.
- `brm_threads` is forced to `NULL` in that case.
- Three new tests pass; full test sweep stays green.
- Roxygen + architecture-decisions updated.
- Code-reviewer agent passes.
- Commit message references #411 and notes "force-N-1, not partial port".
