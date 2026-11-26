## mvgam

An R package for Bayesian multivariate state-space models. brms builds the
observation model's Stan code and data. mvgam adds trend and factor blocks
(`R/stan_assembly.R`) and its own post-fit methods. `architecture/` holds the
design decisions and `architecture/quick-reference.md` the current syntax.
Work happens on `feature/brms-integration`. Use `gh` for GitHub.

## Commands

- One test file: `Rscript -e "devtools::load_all(); testthat::test_file('path')"`.
  Select tests with `desc`. `test_file()` takes no filter.
- Full suite: `TESTTHAT_PARALLEL=true TESTTHAT_CPUS=4 Rscript -e "devtools::test()"`.
- Documentation: `Rscript -e "devtools::document()"`.
- Write test and build output to a log file and read the file. Never pipe it
  into `tail`, `head` or `grep`, and never rerun a test to see its summary.

## Non-negotiable

- No test errors or warnings and no `document()` warnings. Trace every warning
  to its cause. Never mute it with `suppressWarnings()`.
- Tests never use `try()`, `tryCatch()` or `skip()`. `skip_on_cran()` may gate
  a heavy sweep whose core cases still run on CRAN. The CI suite stays under
  10 minutes.
- CI tests never fit Stan models or depend on cached fixtures. Fits live in
  `tests/local/`.
- Never patch package code with wrappers, placeholders or `tryCatch()` to make
  a test pass.

## Code

- Tidyverse style, snake_case, 80-character lines, 2-space roxygen
  continuation lines. Internal functions take `@noRd`. Export only what users
  call.
- Validate every argument with `checkmate::assert_*()`.
- Errors use `insight::format_error()` and per-call warnings
  `insight::format_warning()`. Once-per-session conditions use `warn_once()` and
  `inform_once()` (`R/utils-conditions.R`), which are quiet under testthat. A
  test of one sets `withr::local_envvar(TESTTHAT = "")`. `silent = 2` suppresses
  messages. Warnings always show.
- Comments, roxygen and messages describe current behaviour. Never mention
  planning docs, task IDs, `tasks/` paths, refactor history ("legacy", "new"),
  future work ("planned", "TODO"), process words ("DRY") or a convention's
  author. Never use empty praise words ("robust", "comprehensive",
  "efficient").
- Never claim parity with brms before reading the brms source.
- Write no new helper before reading every related surface for one that
  already does the job. That reading also finds the duplicated logic and
  tech debt the change should fold in. Extend or consolidate what exists.

### Messages

- The first element is the problem in one sentence. Parameter names take
  single quotes: `'n_lv'`.
- A multi-line message uses `c()` with named bullets: `x =` for details, `i =`
  for the fix. Positional strings merge into one paragraph.
- One plain sentence per bullet. A bullet must add a value, a reason or a
  fix the main line lacks. Unsupported features say "X is not supported."

```r
stop(insight::format_error(c(
  "Parameter 'ndraws' exceeds available draws.",
  x = paste0("Requested: ", ndraws, ", available: ", total, "."),
  i = "Use a smaller value or set ndraws = NULL to use all draws."
)))
```

## Working rules

- Never scope a feature down by refusing reasonable cases. Build the general
  solution.
- Verify by exactness: recompute Stan's stored quantities from raw draws in R,
  compare with brms or closed forms, read the output and open the plots. Run
  no recovery or coverage studies unless asked. Ignore ESS, Rhat and divergence
  noise in local fixtures and examples.
- A task closes only when predictions, forecasts, summaries, plots and scores
  work on several fixtures of different types.
- Fit each probe once and cache it. A cache key holds the data, formulas,
  family, seed and response. Use `run_model = FALSE` to check code alone. Run
  every article chunk on cached fits before a knit.
- Each new behaviour gets one biting assertion, in an existing test block.
  Update existing fixtures before adding new ones. Prose-lint flags in test
  files can be ignored.
- Run the full suite once, as the final gate. No commit until the task's open
  items are fixed.
- Run long tasks in the background and wait for the completion notice without
  polling. Parallel jobs are fine with combined cores, the reporting-pipeline
  session's included, at 18 or fewer.
- Do the work in this session. Spawn no subagents unless asked.
- `tasks/` roadmaps are not logs. Delete an item once it is verified.

## Documentation

- NEWS holds one line per change against the released 1.x.
- Fitting examples stay in `\dontrun{}`. A few fast models may run unguarded.
- `forecast()` propagates the fitted state. `posterior_predict()` marginalises
  over the trend. Docs keep the two apart.
- Examples put covariates in `trend_formula` and pass `type = "expected"` to
  marginaleffects.

## Commits

```
Brief description of changes (50 chars max)

- What changed
- Why it was necessary
```

Never mention Claude as a co-author or link to Claude Code.
