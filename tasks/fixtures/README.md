# Test Model Fixtures

This directory contains fitted mvgam model objects used for development and
testing of the GLM refactoring system (Task 5.9 in dev-tasks-trd-glm-mu-refactor.md).

## Purpose

These fixtures enable rapid iteration during development by:

- Avoiding repeated model fitting during testing cycles
- Providing consistent test models across development sessions
- Enabling exploration of brms internals without compilation delays
- Supporting integration testing for GLM transformation and trend injection

## Contents

The fixtures represent 13 different model patterns from
`tasks/fit_and_save_models.R` covering all critical mvgam functionality:

- **fit1.rds**: Basic RW model (univariate, Poisson)
- **fit2.rds**: Multivariate shared RW (2 responses, Gaussian)
- **fit3.rds**: VARMA with smooths (multivariate, VAR trends)
- **fit4.rds**: Factor AR model (3 responses, mixed families, n_lv = 2)
- **fit5.rds**: Piecewise trend model (PW with changepoints)
- **fit6.rds**: CAR trends with GP (irregular time, GP covariates)
- **fit7.rds**: CAR with monotonic effects (ordered factors)
- **fit8.rds**: Seasonal AR model (AR with multiple lags)
- **fit9.rds**: Nonlinear with AR trends (nonlinear formula)
- **fit10.rds**: Nested random effects in observation formula
- **fit11.rds**: Correlated random effects in trend formula
- **fit12.rds**: RW model with offset term
- **fit13.rds**: Distributional regression (sigma ~ predictor)

## Agent Validation Protocol

### CRITICAL: Multi-Agent Validation Required

Deploy up to 3 general-purpose agents to validate these models according to the GLM refactoring requirements (dev-tasks-trd-glm-mu-refactor.md). Each agent should focus on different aspects:

#### Agent 1: Stan Code Correctness Validation
```r
# For each model, verify:
fit <- readRDS("tasks/fixtures/fitN.rds")

# 1. Stan code structure
cat(fit$stancode)
# CHECK: GLM calls properly transformed to mu format when trends present
# CHECK: Trend effects correctly injected via mu[n] += trend[...]
# CHECK: No infinite recursion patterns or duplicate processing
# CHECK: Parameter naming consistency with _trend suffix

# 2. GLM optimization preservation
# For models WITHOUT trends, verify GLM calls retained
# For models WITH trends, verify proper mu transformation
```

#### Agent 2: Model Summary and Functional Validation
```r
# For each model, verify:
fit <- readRDS("tasks/fixtures/fitN.rds")

# 1. Model summaries are sensible
summary(fit)
# CHECK: Convergence (Rhat < 1.01 for all parameters)
# CHECK: Effective sample size > 100
# CHECK: No divergent transitions

# 2. Variables and parameter extraction
variables(fit)
# CHECK: All expected parameters present
# CHECK: Trend parameters properly suffixed with _trend
# CHECK: Factor loadings (Z matrix) for factor models

# 3. Model structure
print(fit)
# CHECK: Formula components correct
# CHECK: Trend specification preserved
# CHECK: Family and link functions appropriate
```

#### Agent 3: Integration and Edge Case Testing
```r
# For each model, verify:
fit <- readRDS("tasks/fixtures/fitN.rds")

# 1. Special features work correctly:
# - fit1: Basic univariate works as baseline
# - fit2: Multivariate responses handled properly
# - fit3: VARMA with MA components generated
# - fit4: Factor model with n_lv < n_series
# - fit5: Piecewise changepoint detection
# - fit6/fit7: CAR models with irregular time
# - fit8: Seasonal AR with multiple lags (1, 12)
# - fit9: Nonlinear formula transformation
# - fit10: Nested RE in observation formula
# - fit11: Correlated RE in trend formula
# - fit12: Offset term properly handled
# - fit13: Distributional regression (sigma modeling)

# 2. Cross-model consistency checks
# - Parameter naming conventions consistent
# - Stan code patterns follow architecture decisions
# - No code duplication across similar models
```

### Validation Checklist

Each agent should verify ALL models pass these criteria:

- [ ] **Stan Code Generation**: Valid Stan code without syntax errors
- [ ] **Compilation Success**: Models compile and sample without errors  
- [ ] **GLM Handling**: 
  - GLM optimization preserved when beneficial
  - Proper transformation to mu format when trends present
  - No infinite recursion or duplicate processing
- [ ] **Trend Integration**:
  - Trend effects correctly added to linear predictor
  - Response-specific trends for multivariate models
  - Factor models with appropriate Z matrix
- [ ] **Parameter Structure**:
  - Consistent _trend suffix convention
  - Proper parameter categorization (observation vs trend)
  - All expected parameters present and accessible
- [ ] **Edge Cases**:
  - Nonlinear formulas work correctly
  - Random effects in both observation and trend formulas
  - Offset terms handled properly
  - Distributional parameters modeled correctly

### Failure Reporting

If ANY model fails validation:
1. Document the specific model and failure type
2. Provide the exact Stan code section causing issues
3. Compare against expected patterns from architecture decisions
4. Note any regression from previous behavior

## Regenerating Fixtures

If you need to regenerate the fixtures (e.g., after updating mvgam internals):

```r
# From package root directory
source("tasks/fit_and_save_models.R")
```

This script will:
1. Load all dependencies via `tests/local/setup_tests_local.R`
2. Fit all 13 models using the same specifications as integration tests
3. Save each as `tasks/fixtures/fit{1-13}.rds`
4. Report timing and file sizes

**Note**: Fitting all models takes approximately 15-20 minutes.

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
for (i in 1:13) {
  fit <- readRDS(sprintf("tasks/fixtures/fit%d.rds", i))
  print(fit)
  stopifnot(inherits(fit, "mvgam"))
  stopifnot(!is.null(variables(fit)))
  cat(sprintf("Model %d loaded successfully\n", i))
}
```

## Success Metrics for Task 5.9

Per dev-tasks-trd-glm-mu-refactor.md, validation is complete when:

1. **All 13 models generate valid Stan code** - No syntax errors or compilation failures
2. **GLM optimization preserved** - Models without trends retain GLM calls for performance
3. **Proper mu transformation** - Models with trends correctly transform GLM to standard parameterization
4. **No infinite recursion** - Linear pipeline eliminates all recursive processing bugs
5. **Functional equivalence** - Same posterior distributions and model behavior as before refactoring
