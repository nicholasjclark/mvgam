# Local Test Fixtures

This directory is gitignored. It holds cached brms and mvgam model
fits used by `tests/local/test-predictions-brms-concordance.R` for
numerical concordance checks.

## How to populate

From the package root:

```
Rscript tests/local/build_fixtures.R
```

The script fits 17 brms + mvgam fixture pairs and caches them as
`val_brms_<name>.rds` and `val_mvgam_<name>.rds`. Expected runtime is
15-25 minutes on a workstation. Re-runs after partial completion are
incremental — only missing fixtures are refit.

## How to use

After the cache is populated:

```r
testthat::test_file("tests/local/test-predictions-brms-concordance.R")
```

Tests whose required fixture is absent skip with an informative
message pointing at `build_fixtures.R`.

## Why gitignored

These `.rds` files total roughly 50-100 MB and rebuild
deterministically from `build_fixtures.R`. Storing them in git would
bloat clones for contributors who do not need to run the local
concordance suite. Re-fitting on demand is straightforward.
