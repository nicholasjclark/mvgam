# Test Model Fixtures

This directory contains fitted mvgam model objects used for development and
testing of the prediction system.

## Purpose

These fixtures enable rapid iteration during development by:

- Avoiding repeated model fitting during testing cycles
- Providing consistent test models across development sessions
- Enabling exploration of brms internals without compilation delays
- Supporting integration testing for prediction workflows

## Contents

The fixtures represent 9 different model patterns from
`tests/local/test-models-single.R`:

- **fit1.rds**: Basic RW model (univariate, Poisson)
- **fit2.rds**: Multivariate shared RW (2 responses, Gaussian)
- **fit3.rds**: VARMA with smooths (multivariate, VAR trends)
- **fit4.rds**: Factor AR model (3 responses, mixed families, n_lv = 2)
- **fit5.rds**: Piecewise trend model (PW with changepoints)
- **fit6.rds**: CAR trends with GP (irregular time, GP covariates)
- **fit7.rds**: CAR with monotonic effects (ordered factors)
- **fit8.rds**: Seasonal AR model (AR with multiple lags)
- **fit9.rds**: Nonlinear with AR trends (nonlinear formula)

## Regenerating Fixtures

If you need to regenerate the fixtures (e.g., after updating mvgam internals):

```r
# From package root directory
source("tasks/fit_and_save_models.R")
```

This script will:
1. Load all dependencies via `tests/local/setup_tests_local.R`
2. Fit all 9 models using the same specifications as integration tests
3. Save each as `tasks/fixtures/fit{1-9}.rds`
4. Report timing and file sizes

**Note**: Fitting all models takes approximately 10 minutes.

## Version Control

These .rds files are **excluded from git** (see `.gitignore`) because:

- They are large binary files (0.3-0.9 MB each)
- They are user-specific (contain compiled Stan models)
- They can be regenerated from source code
- They would bloat the repository history

Each developer should generate their own fixtures locally using the script above.

## Usage in Development

Load fixtures in exploration scripts or local tests:

```r
# Load package functions
devtools::load_all()

# Load a fixture
fit1 <- readRDS("tasks/fixtures/fit1.rds")

# Verify it works
print(fit1)
summary(fit1)
variables(fit1)

# Use for development/exploration
# ... your exploration code here ...
```

## Testing Basic Functionality

After regenerating fixtures, verify they load correctly:

```r
# Test all fixtures load without errors
for (i in 1:9) {
  fit <- readRDS(sprintf("tasks/fixtures/fit%d.rds", i))
  print(fit)
  stopifnot(inherits(fit, "mvgam"))
  stopifnot(!is.null(variables(fit)))
}
```
