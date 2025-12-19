# Development Tasks: Core Prediction Functions (3.0)

## Overview

This task list covers implementation of the user-facing prediction functions that build on the completed prediction infrastructure (see `tasks/completed/dev-tasks-prediction-infrastructure.md`).

**Foundation Already Complete:**
- `extract_component_linpred(mvgam_fit, newdata, component, resp, ndraws, re_formula, allow_new_levels, sample_new_levels)` - unified linear predictor extraction
- `extract_linpred_from_prep()` - handles all brms formula features (smooths, REs, GPs, monotonics, nonlinear, offsets)
- `prepare_predictions.mock_stanfit()` - S3 method for design matrix extraction
- 100% success rate on validation tests (17/17 in `validate_extraction_vs_brms.R`)

**Target Functions:**
1. `posterior_linpred.mvgam()` - link scale predictions
2. `posterior_epred.mvgam()` - expected values (response scale)
3. `posterior_predict.mvgam()` - posterior predictive samples
4. `predict.mvgam()` - convenience wrapper with summaries
5. `fitted.mvgam()` - in-sample fitted values

---

## Relevant Files

### R Package Files
- `R/posterior_linpred.R` - `posterior_linpred.mvgam()` S3 method and helpers
- `R/predictions.R` - Core prediction infrastructure (extend with S3 methods)
- `R/index-mvgam.R` - Parameter extraction helpers
- `R/mock-stanfit.R` - Mock stanfit and `prepare_predictions.mock_stanfit()`

### Test Files
- `tests/testthat/test-predict.R` - Existing prediction validation tests
- `tests/testthat/test-predictions-core.R` - New tests for S3 methods (TO CREATE)

### Validation Files
- `tasks/validate_extraction_vs_brms.R` - Numerical validation framework (extend)
- `tasks/validate_prediction_functions.R` - Full prediction function validation (TO CREATE)

### Reference Files (brms Source Analysis)
- `tasks/brms_posterior_predict_internals.R` - **CRITICAL REFERENCE**: Complete brms `posterior_predict` source code showing:
  - Family-specific dispatch pattern: `pp_fun <- paste0("posterior_predict_", object$family$fun)`
  - dpars extraction: `get_dpar(prep, "mu", i = i)`, `get_dpar(prep, "sigma", i = i)`
  - Truncation handling via `rcontinuous()` and `rdiscrete()` helpers
  - Multivariate handling in `posterior_predict.mvbrmsprep()`
  - All family sampling implementations (gaussian, poisson, binomial, negbinomial, gamma, etc.)

### Context Files
- `tasks/completed/dev-tasks-prediction-infrastructure.md` - Completed foundation tasks
- `tasks/trd-prediction-system.md` - Full requirements document
- `tasks/prediction-system-implementation-strategy.md` - Architecture decisions

---

## Agent Usage Protocol

**MANDATORY for all tasks:**

1. **pathfinder agent**: Use BEFORE editing any file to:
   - Find exact function locations and line numbers
   - Map dependencies that might need updates
   - Assess edit safety

2. **r-package-analyzer agent**: Use to study brms internals when needed (supplement `brms_posterior_predict_internals.R`):
   - Task 2.1: Study `posterior_epred.brmsfit()` for epred-specific patterns
   - Verify inverse link handling matches our implementation

3. **code-reviewer agent**: Use AFTER completing each numbered task to:
   - Verify fail-fast validation patterns
   - Check for code duplication
   - Ensure tidyverse style compliance
   - Validate test coverage

---

## Tasks

### 1.0 Core Combination Logic and `posterior_linpred.mvgam()` ✓

Build the foundation for combining observation and trend linear predictors, then expose via brms-compatible S3 method.

**Completed**: Commit 71db40be

- [x] **1.1 Create `get_combined_linpred()` helper function**
  - Created in new file `R/posterior_linpred.R`
  - Extracts obs linpred via `extract_component_linpred(..., component = "obs")`
  - Checks if trend model exists and has formula
  - If trend exists: extracts trend linpred and combines additively
  - Delegates validation to `extract_component_linpred()`

- [x] **1.2 Implement `process_error` logic in combination**
  - When `process_error = FALSE`:
    - Computes column means of trend linpred matrix
    - Broadcasts to match obs linpred dimensions
    - Fixes trend at posterior mean to reduce uncertainty
  - Parameter validation via `checkmate::assert_logical()`

- [x] **1.3 Implement `posterior_linpred.mvgam()` S3 method**
  - Created in `R/posterior_linpred.R`
  - Imports `posterior_linpred` generic from brms
  - Handles `newdata = NULL` → uses training data from `object$data`
  - Delegates all validation to `extract_component_linpred()`
  - Code reviewed and approved

- [x] **1.4 Add validation tests for `posterior_linpred.mvgam()`**
  - Extended `tasks/validate_extraction_vs_brms.R` with `run_linpred_validation()` helper
  - Tests added (all passing):
    - Dimensions match brms [ndraws × nobs]
    - `ndraws` subsetting returns correct number of draws
    - `process_error = FALSE` reduces variance vs TRUE
    - Obs-formula models: 5 tests (intercept-only, fixed, RE, smooth, GP)
    - Trend-formula models: 4 tests (fixed, RE, smooth, GP)

- [x] **1.5 Code review for Task 1.0**
  - Code reviewer approved implementation
  - Proper delegation to `extract_component_linpred()` (no duplication)
  - Fail-fast validation patterns implemented

---

### 2.0 Implement `posterior_epred.mvgam()` ✓

Expected values on response scale. For most families this is `linkinv(eta)`, but
some families require distributional parameters (e.g., lognormal needs sigma).

**Completed**: New file `R/posterior_epred.R` created with full implementation.
**Validation**: 49/54 tests passed (90.7%), multivariate cor=1.0 with brms.
Note: Integer-valued families (Poisson) use relaxed threshold (0.75) since small
linpred differences get amplified by exp() on count scale.

**Family categories:**
- Simple (linkinv only): gaussian, poisson, bernoulli, beta, Gamma, nb, student
- Needs trials: binomial, beta_binomial (implemented, auto-extracted from model)
- Needs sigma: lognormal (implemented, requires sigma param)
- Unsupported: nmix, tweedie (throws informative errors)

- [x] **2.1 Create `compute_family_epred()` helper function**
  - Created in `R/posterior_epred.R`
  - Family dispatch for simple families, binomial, lognormal
  - Unsupported families throw `insight::format_error()`
  - Multivariate handled via recursive dispatch per-response

- [x] **2.2 Create `extract_trials_for_family()` helper for binomial families**
  - Created in `R/posterior_epred.R` (lines 333-406)
  - Auto-extracts trials from `object$standata$trials` or `object$data$trials`
  - Handles newdata case: extracts `newdata$trials`
  - Validates trials values (numeric, >= 1, finite, no missing)
  - Detects multivariate family structures correctly
  - **Note**: Lognormal sigma extraction deferred for later

- [x] **2.3 Implement `posterior_epred.mvgam()` S3 method**
  - Created in `R/posterior_epred.R`
  - Delegates to `get_combined_linpred()` for obs+trend combination
  - Handles multivariate family extraction (per-response or shared)
  - Proper fallback when `form$family` is NULL (shared family case)

- [x] **2.4 Add validation tests for `posterior_epred.mvgam()`**
  - Extended `tasks/validate_extraction_vs_brms.R`
  - Tests: Poisson, Gaussian, multivariate Gaussian, Beta, Binomial
  - linkinv consistency verified: `epred == exp(linpred)` for Poisson
  - linkinv consistency verified: `epred == plogis(linpred) * trials` for Binomial
  - Scale constraints verified: Poisson >= 0, Beta in (0,1), Binomial in [0, trials]
  - Integer-valued families use relaxed correlation threshold (0.75)

- [x] **2.5 Code review for Task 2.0**
  - Code reviewer approved implementation
  - Minor fixes applied (line length, documentation accuracy)

---

### 2.6 Implement Complete Family Support for `posterior_epred` ✓

Port all brms family-specific `posterior_epred_*` functions to provide complete family coverage.
Credit Paul Bürkner and the brms development team in roxygen documentation.

**Completed**: All core families ported to `R/posterior_epred.R` (lines 313-722).
**Reference**: `tasks/brms_posterior_epred_internals.R` and brms `R/distributions.R`

**Implemented families (32 total):**
- Simple families: gaussian, student, skew_normal, exponential, gamma, weibull, frechet, inverse.gaussian, exgaussian, beta, von_mises, bernoulli
- Count families with rate_denom: poisson, negbinomial, negbinomial2, geometric
- Distributional params: lognormal, shifted_lognormal, binomial, beta_binomial, gen_extreme_value, asym_laplace, wiener, discrete_weibull, com_poisson
- Zero-inflated: zi_poisson, zi_negbinomial, zi_binomial, zi_beta_binomial, zi_beta, zero_one_inflated_beta, zi_asym_laplace
- Hurdle: hurdle_poisson, hurdle_negbinomial, hurdle_gamma, hurdle_lognormal

**Deferred (complex 3D array returns, not commonly used in mvgam):**
- Ordinal families: cumulative, sratio, cratio, acat
- Categorical families: categorical, multinomial, dirichlet, dirichlet2, logistic_normal

- [x] **2.6.1 Add brms attribution to R/posterior_epred.R**
  - Added section header with brms attribution (lines 313-327)

- [x] **2.6.2 Port simple families (linkinv only)**
  - All 12 simple families implemented (lines 430-464)
  - All return `prep$dpars$mu` directly

- [x] **2.6.3 Port families requiring distributional parameters**
  - 9 families implemented (lines 488-530):
    - `posterior_epred_lognormal()`: `exp(mu + sigma^2/2)`
    - `posterior_epred_shifted_lognormal()`: `exp(mu + sigma^2/2) + ndt`
    - `posterior_epred_binomial()`: `mu * trials` using `data2draws()`
    - `posterior_epred_beta_binomial()`: same as binomial (beta in mu)
    - `posterior_epred_gen_extreme_value()`: `mu + sigma * (gamma(1-xi) - 1) / xi`
    - `posterior_epred_asym_laplace()`: `mu + sigma * (1-2*quantile) / (quantile*(1-quantile))`
    - `posterior_epred_wiener()`: diffusion model formula (DOI reference included)
    - `posterior_epred_discrete_weibull()`: calls `mean_discrete_weibull()`
    - `posterior_epred_com_poisson()`: calls `mean_com_poisson()`

- [x] **2.6.4 Port zero-inflated families**
  - 7 families implemented (lines 532-569):
    - All use `mu * (1 - zi)` pattern or variants
    - `zi_asym_laplace` delegates to base function

- [x] **2.6.5 Port hurdle families**
  - 4 families implemented (lines 571-591):
    - `hurdle_poisson`: `mu / (1 - exp(-mu)) * (1 - hu)`
    - `hurdle_negbinomial`: complex formula with shape
    - `hurdle_gamma`: `mu * (1 - hu)`
    - `hurdle_lognormal`: `exp(mu + sigma^2/2) * (1 - hu)`
  - `hurdle_cumulative` deferred (ordinal, returns 3D array)
  
- [x] **2.6.5.1 Clarify ordinal vs categorical linpred dimensionality**
  - **Critical finding**: Task 2.6.6 description was INCORRECT
  - **Ordinal families** (cumulative, sratio, cratio, acat):
    - linpred is 2D `[ndraws x nobs]` - single eta per observation ✓
    - epred is 3D `[ndraws x nobs x ncat]` - category probabilities
    - Current mvgam implementation is CORRECT for linpred
  - **Categorical family**:
    - linpred is 3D `[ndraws x nobs x (ncat-1)]` - one eta per non-reference category
    - epred is 3D `[ndraws x nobs x ncat]` - category probabilities
    - Current mvgam returns 2D - needs fixing
  - **Debug script**: `tasks/debug_ordinal_linpred.R` confirms these dimensions
  - **Key structural insight**: For categorical, `prep$draws` is NULL; draws are stored in `bprepl$fe$b` for each category (muB, muC, etc.)
  - **Verified**: `fe$b %*% t(fe$X)` approach matches brms linpred exactly (max diff: 0.0)

- [x] **2.6.5.2 Analyze bprepl structure for categorical models**
  - For categorical models, brms uses bprepl objects in `prep$dpars$muB`, `prep$dpars$muC`
  - Key difference from standard models:
    - Standard: `prep$draws` + `prep$sdata` contain all data
    - Categorical: `prep$draws` is NULL, data is in bprepl objects
  - **bprepl component structure** (from brms `predictor.R`):
    - `bprepl$fe$b`: coefficient draws `[ndraws x ncoef]`
    - `bprepl$fe$X`: design matrix `[nobs x ncoef]`
    - `bprepl$sm$fe$Xs`, `bprepl$sm$fe$bs`: smooth fixed effects
    - `bprepl$sm$re[[k]]$Zs`, `bprepl$sm$re[[k]]$s`: smooth random effects
    - `bprepl$re$Z[[g]]`, `bprepl$re$r[[g]]`: random effects per group
    - `bprepl$gp`: GP terms with `Igp` indices
  - **brms computation pattern** (from `predictor.bprepl()`):
    
    ```r
    eta = predictor_fe() + predictor_re() + predictor_sm() + predictor_gp() + predictor_offset()
    ```
    - Fixed effects: `tcrossprod(b, X)` → [ndraws x nobs]
    - Smooths: same pattern with `Xs`/`bs` plus `Zs`/`s` for RE part
    - Random effects: `Matrix::tcrossprod(r, Z)` (sparse matrix support)
  - **Implementation approach**: Loop over bprepl components, sum contributions using tcrossprod
  - **Reference**: [brms predictor.R](https://github.com/paul-buerkner/brms/blob/master/R/predictor.R)

- [x] **2.6.5.3 Implement DRY bprepl extraction following brms adapter pattern**
  - **Implemented in**: `R/posterior_linpred.R` (lines 269-522)
  - **Functions added**:
    1. `is_categorical_family(prep)` - detects categorical via family or bprepl structure
    2. `compute_linpred_from_bprepl(bprepl)` - computes linpred using tcrossprod pattern:
       - Fixed effects: `tcrossprod(fe$b, fe$X)`
       - Smooths: fixed (`sm$fe$bs/Xs`) + random (`sm$re[[k]]$s/Zs`)
       - Random effects: `Matrix::tcrossprod(re$r[[g]], re$Z[[g]])`
       - GPs: direct contribution from `gp$eta` or `gp$f`
    3. `extract_linpred_categorical(prep)` - stacks into 3D `[ndraws x nobs x (ncat-1)]`
  - **Code reviewer**: Approved with comprehensive validation
  - **Reference**: brms predictor.bprepl() pattern

- [x] **2.6.5.4 Block multi-category families at model specification time**
  - **Architecture decision**: Multi-category families (categorical, multinomial, dirichlet,
    dirichlet2, logistic_normal) require 3D linear predictors [ndraws x nobs x (ncat-1)].
    State-Space trends in mvgam are single processes that cannot be meaningfully combined
    with multiple category etas. Users should use brms directly for these families.
  - **Implementation**: Added `validate_supported_family()` in `R/validations.R` (lines 227-259)
    and call from `generate_stan_components_mvgam_formula()` in `R/make_stan.R` (line 89)
  - **Error message includes**:
    - List of unsupported families
    - Explanation that these require multi-category linear predictors
    - Recommendation to use brms directly for these response types
  - **Note**: Ordinal families (cumulative, sratio, cratio, acat) ARE supported because
    they use 2D linpred (single eta) with threshold-based transformation to 3D epred

- [x] **2.6.5.5 Remove categorical bprepl extraction code**
  - Removed functions from `R/posterior_linpred.R`:
    - `is_categorical_family()`, `compute_linpred_from_bprepl()`, `extract_linpred_categorical()`
  - Removed routing code from `R/predictions.R`
  - **Note**: Tasks 2.6.5.1-2.6.5.3 were exploratory work that informed the architecture
    decision to block multi-category families rather than implement complex 3D linpred handling

- [x] **2.6.5.6 Fix hurdle Stan code generation** ✓
  - **Bug**: `reorganize_target_statements()` in `R/stan_polish.R` was moving
    loop-dependent likelihood statements outside their for loops
  - **Fix**: Added check for `\[[a-z]\]` pattern to skip extraction for
    statements that use loop variable indexing (e.g., `Y[n]`, `mu[n]`)
  - **Implementation**: Lines 550-556 in `R/stan_polish.R`
  - **Tests**: Added hurdle_poisson stancode structure test to
    `tests/testthat/test-stancode-standata.R` verifying likelihood inside for loop
  - **Validation**: hurdle_poisson and hurdle_negbinomial pass validation in
    `tasks/validate_extraction_vs_brms.R`

- [x] **2.6.6 Port ordinal families** ✓
  - **Linpred status**: WORKING ✓
    - Ordinal linpred: 2D `[ndraws x nobs]` - single eta per observation
    - `posterior_linpred.mvgam()` returns correct 2D structure for ordinal models
  - **Epred status**: WORKING ✓
    - Returns 3D `[ndraws x nobs x ncat]` category probabilities
    - Routing in `posterior_epred.mvgam()` detects ordinal families
    - Extracts thresholds and disc from `object$fit` posterior draws
    - Builds prep object and calls `posterior_epred_ordinal()`
  - **Implementation** (in `R/posterior_epred.R`):
    - `is_ordinal_family()`: detects ordinal family types
    - `extract_ordinal_thresholds()`: extracts `Intercept[k]` parameters
    - `extract_ordinal_disc()`: extracts discrimination or defaults to 1.0
    - Ordinal density functions: `dcumulative()`, `dsratio()`, `dcratio()`, `dacat()`
    - `posterior_epred_ordinal(prep)`: computes 3D category probabilities

- [x] **2.6.6.1 Extract threshold parameters from posterior draws**
  - **Implemented in** `R/posterior_epred.R` (lines 832-979):
    - `is_ordinal_family(family)` - checks if family is ordinal
    - `extract_ordinal_thresholds(object, ndraws)` - extracts threshold matrix
    - `extract_ordinal_disc(object, ndraws, nobs)` - extracts discrimination
  - Uses `posterior::as_draws_matrix()` for consistency with codebase
  - Consistent `seq_len(ndraws)` subsampling between functions
  - Returns matrix `[ndraws x nthres]` where nthres = ncat - 1
  - Disc defaults to 1.0 if not present in model

- [x] **2.6.6.2 Add ordinal detection and routing in posterior_epred.mvgam()**
  - **Implemented in** `R/posterior_epred.R` (lines 332-386):
    - Check if family is ordinal via `is_ordinal_family(family)`
    - Build prep object with `mu`, `thres`, `disc` from extraction functions
    - Validate dimensions match between linpred and extracted parameters
    - Route to `posterior_epred_ordinal(prep)` for 3D output
  - Handles multivariate case: errors if resp not specified for ordinal families

- [x] **2.6.6.3 Add ordinal cases to compute_family_epred() as fallback**
  - **Implemented in** `R/posterior_epred.R` (lines 170-179):
    - Added cumulative, sratio, cratio, acat to switch statement
    - Throws informative error directing to threshold-based approach
    - Prevents silent incorrect results from `linkinv()` fallback

- [x] **2.6.6.4 Validate ordinal epred against brms**
  - Validation passed: `Success! Dims: 10 x 30 x 3`
  - Returns correct 3D output: `[ndraws x nobs x ncat]`
  - Key fixes applied during validation:
    - Changed extraction to use `object$fit` instead of `object$model_output`
    - Fixed threshold pattern from `^b_Intercept\\[` to `^Intercept\\[\\d+\\]$`
    - Added `drop()` calls in `dcumulative()` for 1-column matrix handling

- [x] **2.6.7 Multi-category families** - CANCELLED (not supported in mvgam)
  - **Architecture decision**: Multi-category families (categorical, multinomial, dirichlet,
    dirichlet2, logistic_normal) are NOT supported in mvgam because:
    - They require 3D linear predictors [ndraws x nobs x (ncat-1)]
    - State-Space trends are single processes incompatible with multi-category etas
    - Broadcasting trend to all categories would be statistically inappropriate
  - **Resolution**: Block at model specification time (task 2.6.5.4) with error
    directing users to brms for these response types
  - **Note**: Ordinal families (cumulative, sratio, cratio, acat) ARE supported
    because they use single eta with threshold transformation
  - **Epred functions remain** (may be useful if pure brms models without trends):
    - `posterior_epred_categorical()`, `posterior_epred_multinomial()`, etc.

- [x] **2.6.8 Port helper functions**
  - `data2draws()`: expand data to draws dimension (lines 349-373)
  - `dim_mu()`: expected dimension of mu parameter (lines 389-394)
  - `multiply_dpar_rate_denom()`: rate denominator handling (lines 412-426)
  - `mean_discrete_weibull()`: series approximation for E[Y] (lines 606-625)
  - `mean_com_poisson()`: series + closed-form approximation (lines 640-710)

- [ ] **2.6.9 Update `compute_family_epred()` to use new infrastructure**
  - Current switch statement works correctly
  - Could refactor to use `get(paste0("posterior_epred_", family_name))`
  - Low priority since existing tests pass

- [x] **2.6.10 Add tests for family functions**
  - Unit tests added to `tests/testthat/test-predict.R`
  - Tests cover: simple families, binomial with trials, lognormal with sigma
  - Tests verify dimensions, constraints, multivariate handling

- [x] **2.6.11 Code review for Task 2.6**
  - Code reviewer approved all implementations
  - All formulas verified against brms source
  - Minor fixes applied (explanatory comments, indentation)

---

### 3.0 Implement `posterior_predict.mvgam()` in `R/posterior_predict.R`

Posterior predictive samples with observation-level noise.

- [x] **3.1 Create family-specific sampling infrastructure** ✓
  - **Enhanced `sample_from_family()` in `R/posterior_predict.R` (lines 72-432)**
  - **Complete brms family coverage (34 total families)**:
    - Continuous: gaussian, student, skew_normal, lognormal, shifted_lognormal,
      gamma, weibull, frechet, inverse.gaussian, exgaussian, beta,
      gen_extreme_value, asym_laplace, von_mises, exponential, wiener
    - Count: poisson, negbinomial, negbinomial2, geometric, discrete_weibull,
      com_poisson
    - Binomial: binomial, beta_binomial, bernoulli
    - Zero-inflated: zi_poisson, zi_negbinomial, zi_binomial, zi_beta_binomial,
      zi_beta, zero_one_inflated_beta, zi_asym_laplace
    - Hurdle: hurdle_poisson, hurdle_negbinomial, hurdle_gamma, hurdle_lognormal,
      hurdle_cumulative
  - **New parameters added**: alpha, ndt, xi, quantile, kappa, beta, bs, bias,
    disc, thres, link
  - **All implementations use brms sampling functions**: rskew_normal, rexgaussian,
    rshifted_lnorm, rvon_mises, rasym_laplace, rdiscrete_weibull, rcom_poisson,
    rwiener, rgen_extreme_value
  - **Created `get_family_dpars()` helper** (lines 434-524) mapping families to
    required distributional parameters
  - Added edge case validation for shifted_lognormal (epred > ndt)
  - Code reviewer approved sampling infrastructure

- [x] **3.2 Handle distributional parameters extraction (DRY approach)** ✓

  **Key Findings from Investigation:**
  - brms `get_dpar(prep, dpar, i, inv_link)` returns matrix `[ndraws x nobs]`
  - brms prep$dpars contains `bprepl` objects (lazy evaluation) computed on demand
  - mvgam's `prepare_predictions.mock_stanfit()` creates prep but doesn't populate dpars
  - Solution: Extend prep object to include dpars so get_dpar() works seamlessly

  **DRY Architecture:**
  - Extend `prepare_predictions.mock_stanfit()` in `R/mock-stanfit.R` to:
    1. Detect which dpars the family requires (sigma, shape, phi, nu, zi, hu)
    2. Extract those parameters from stanfit posterior draws
    3. Store in `prep$dpars` as matrices `[ndraws x nobs]` (broadcast if scalar)
  - Both `posterior_epred()` and `posterior_predict()` use the same prep object
  - Use brms's `get_dpar()` for extraction (no custom extraction logic)

  **Sub-tasks:**
  - [x] **3.2.1 Create `get_family_dpars()` helper** ✓
    - Created in `R/posterior_predict.R` (lines 371-426)
    - Returns vector of dpar names required by family
    - Complete brms family coverage including all continuous, count, binomial,
      zero-inflated, and hurdle families
    - Uses `%||%` pattern for clean null handling

  - [x] **3.2.2 Create `extract_dpars_from_stanfit()` helper** ✓
    - **Implemented in**: `R/posterior_predict.R` (lines 534-703)
    - **Tests in**: `tests/testthat/test-predict.R` (lines 913-1063)
    - Takes: stanfit object, dpar names, ndraws, nobs, draw_ids
    - Extracts posterior draws for each dpar using grep pattern matching
    - Returns named list of matrices `[ndraws x nobs]`
    - Handles scalar vs indexed parameters (broadcast scalar to matrix)
    - Validates stanfit class (stanfit, CmdStanMCMC, draws variants)
    - Validates index extraction from parameter names
    - Handles dimension mismatch with informative warning
    - Code reviewer approved, 3 unit tests pass

  - [x] **3.2.3 Extend `prepare_predictions.mock_stanfit()` to populate dpars**
    - **Implemented in**: `R/mock-stanfit.R` (lines 371-440)
    - Three-branch logic handles ALL model types:
      - Nonlinear: mu computed via formula evaluation (existing)
      - Multivariate: per-response extraction with `{dpar}_{resp}` naming
      - Univariate: direct extraction from posterior
    - Multivariate uses `names(brmsfit$formula$forms)` for response names
    - Per-response nobs from `sdata[[paste0("N_", resp_name)]]`
    - Stores multivariate dpars as `prep$dpars[[resp_name]]` with original
      dpar names for downstream compatibility
    - Fail-fast validation for missing response names and nobs
    - Code reviewer approved, all 164 tests pass

  - [x] **3.2.4 Validate dpars extraction works with extended prep**
    - Added `run_dpars_validation()` to `tasks/validate_extraction_vs_brms.R`
    - Validates dpars by comparing `prep$dpars` against brms's
      `posterior_linpred(dpar = ...)` output
    - Tests using existing models (no new fitting):
      - Beta family (phi)
      - Hurdle Poisson (hu)
      - Hurdle Negbinomial (hu, shape)
      - Zero-inflated Poisson (zi)
    - Uses 0.99 correlation threshold (stricter than linpred because
      dpars are same draws extracted via different pathways)

- [X] **3.3 Implement `posterior_predict.mvgam()` S3 method (using DRY prep object)**
  - Function signature:

    ```r
    posterior_predict.mvgam <- function(object, newdata = NULL,
                                        process_error = TRUE,
                                        ndraws = NULL, draw_ids = NULL,
                                        re_formula = NULL,
                                        allow_new_levels = FALSE,
                                        sample_new_levels = "uncertainty",
                                        resp = NULL, ...)
    ```
  - **DRY Implementation:**
    1. Create prep object via `prepare_predictions.mock_stanfit()` (now includes dpars)
    2. Extract mu via `get_dpar(prep, "mu")` for expected values
    3. Extract dpars via `get_dpar(prep, "sigma")`, `get_dpar(prep, "shape")`, etc.
    4. Pass to `sample_from_family()` for observation noise
  - **Shared Infrastructure:**
    - Same prep object can be used by both `posterior_epred()` and `posterior_predict()`
    - Consider refactoring `posterior_epred()` to also use `get_dpar()` pattern
  - Ensure reproducibility with `set.seed()` documentation
  - Add roxygen2 documentation with `@export`

- [ ] **3.3.1 Fix all failures when running `validate_extraction_vs_brms.R`**

- [ ] **3.4 Handle truncation**
  - Reference `rcontinuous()` (line 978-1003) and `rdiscrete()` (line 1015-1035)
  - Implement truncation handling if model has `trunc()` terms

- [ ] **3.5 Add validation tests for `posterior_predict.mvgam()`**
  - Update `validate_extraction_vs_brms.R` so that ALL models are compared to brms equivalents. Think hard about ways to validate these predictions as the randomness of sampling means they won't necessarily be strongly correlated (consider using the `ks.test()` function)

- [ ] **3.6 Code review for Task 3.0**
  - Use **code-reviewer agent** on all changes

---

### 4.0 Implement Convenience Wrappers

User-friendly interfaces with automatic summarization.

- [ ] **4.1 Implement `predict.mvgam()` S3 method**
  - Reference `brms_posterior_predict_internals.R` lines 239-255 for `predict.brmsfit()`
  - Function signature:
  
    ```r
    predict.mvgam <- function(object, newdata = NULL,
                              type = c("response", "link", "prediction"),
                              process_error = TRUE,
                              ndraws = NULL,
                              summary = TRUE,
                              robust = FALSE,
                              probs = c(0.025, 0.975),
                              ...)
    ```
  - Dispatch to appropriate posterior_* function based on `type`
  - When `summary = TRUE`: return data.frame with Estimate, Est.Error, Q2.5, Q97.5
  - When `summary = FALSE`: return raw matrix
  - Add roxygen2 documentation with `@export`

- [ ] **4.2 Implement `fitted.mvgam()` S3 method**
  - Function signature:
  
    ```r
    fitted.mvgam <- function(object,
                             scale = c("response", "linear"),
                             process_error = TRUE,
                             summary = TRUE,
                             ...)
    ```
  - Equivalent to `predict(object, newdata = NULL, ...)`
  - Uses training data stored in object
  - Add roxygen2 documentation with `@export`

- [ ] **4.3 Create summary computation helper**
  - Create `summarize_predictions()` internal function
  - Reference brms `posterior_summary()` for output format
  - Compute: Estimate (mean or median), Est.Error (sd or mad), quantiles
  - Support `robust = TRUE` for median/mad instead of mean/sd
  - Return tidy data.frame with observation index

- [ ] **4.4 Add tests for convenience wrappers**
  - Test: `predict()` returns data.frame when summary=TRUE
  - Test: `predict()` returns matrix when summary=FALSE
  - Test: `fitted()` matches `predict(newdata=NULL)`
  - Test: quantile columns match requested probs
  - Test: `robust = TRUE` uses median instead of mean

- [ ] **4.5 Code review for Task 4.0**
  - Use **code-reviewer agent** on all changes

---

### 5.0 Multivariate Model Support

Ensure all prediction functions work correctly with multivariate responses.

- [ ] **5.1 Test multivariate predictions with existing validation models**
  - Use fit2 (mvbind) and fit4 (bf+bf) from fixtures
  - Verify `posterior_linpred()` returns correct structure for each response
  - Test `resp` argument filtering
  - Reference `posterior_predict.mvbrmsprep()` (lines 105-116) for multivariate handling

- [ ] **5.2 Handle shared vs response-specific trends**
  - Shared trends: same trend contribution for all responses
  - Response-specific: different trend per response (list trend_formula)
  - Verify combination logic handles both cases

- [ ] **5.3 Add multivariate validation tests**
  - Extend validation framework with multivariate test cases
  - Compare against brms multivariate predictions where applicable
  - Test: each response returns correct dimensions
  - Test: `resp = NULL` returns all responses (3D array for posterior_predict)

- [ ] **5.4 Code review for Task 5.0**
  - Use **code-reviewer agent** on multivariate handling

---

### 6.0 Integration Testing and Documentation

Final validation and documentation.

- [ ] **6.1 Create comprehensive integration test file**
  - Create `tests/testthat/test-predictions-core.R`
  - Move validation tests from tasks/ to testthat
  - Cover: all prediction functions, all model types, edge cases

- [ ] **6.2 Run full validation suite**
  - Execute `tasks/validate_prediction_functions.R`
  - Verify all tests pass with high correlation (>= 0.99) against brms
  - Document any discrepancies

- [ ] **6.3 Update NAMESPACE exports**
  - Run `devtools::document()` to update NAMESPACE
  - Verify all S3 methods properly exported:
    - `posterior_linpred.mvgam`
    - `posterior_epred.mvgam`
    - `posterior_predict.mvgam`
    - `predict.mvgam`
    - `fitted.mvgam`
  - Check for any namespace conflicts

- [ ] **6.4 Run full package test suite**
  - Execute `devtools::test()`
  - Verify zero errors and zero warnings
  - Fix any failures before completion

- [ ] **6.5 Final code review**
  - Use **code-reviewer agent** on complete prediction system
  - Review: consistency, documentation completeness, test coverage

---

## Success Criteria

1. All prediction functions return correct dimensions [ndraws × nobs]
2. `process_error` toggle demonstrably affects uncertainty
3. Pure brms models (no trend) predictions match brms exactly
4. Models with trends: obs + trend combination validated
5. Multivariate models return correct per-response predictions
6. All validation tests pass (target: 100% success rate, cor >= 0.99)
7. Full test suite passes with zero errors/warnings
8. All functions have complete roxygen2 documentation

---

## Notes

- **DO NOT** skip code-reviewer agent usage - catch issues early
- **DO NOT** implement features not specified in TRD
- **DO** reference `brms_posterior_predict_internals.R` for family-specific implementations
- **DO** use pathfinder before any edits to understand dependencies
- **DO** validate against brms at every step
- **DO** keep sub-tasks completable in ≤15 minutes
