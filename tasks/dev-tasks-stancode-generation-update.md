# TRD-stancode-generation-update Development Tasks

## Overview
This document tracks implementation progress for the stancode generation update feature, implementing comprehensive prior system integration and Stan code inspection capabilities for mvgam trend models.

## Task Status
- ✅ **Completed**: All foundation and core prior systems (sections 1.0-2.6)
- 🔄 **In Progress**: Parameter standardization (section 2.7) 
- ⏳ **Blocked**: All subsequent tasks depend on parameter standardization completion

---

## COMPLETED SECTIONS

### 1.0 Build Foundation Components and Infrastructure ✅
- [x] 1.1 Create prior helper functions using brmsprior class in `R/priors.R` - COMPLETED
- [x] 1.2 Build validation utility functions in `R/validations.R` - COMPLETED
- [x] 1.3 Enhance setup_brms_lightweight() in `R/brms_integration.R` - COMPLETED
- [x] 1.4 Stan assembly helper functions - COMPLETED
- [x] 1.5 Write comprehensive unit tests - COMPLETED

### 2.0 Implement Core Prior and Stan Generation Systems ✅
- [x] 2.1 Create prior extraction system in `R/priors.R` - COMPLETED
- [x] 2.2 Build prior combination logic in `R/priors.R` - COMPLETED
- [x] 2.3 Stan data generation - COMPLETED
- [x] 2.4 Stan code generation pipeline - COMPLETED
- [x] 2.5 Integrate prior generation into trend dispatcher system - COMPLETED
- [x] 2.6 Implement prior flow to trend stanvar generators - COMPLETED: All 18 sub-tasks completed including theta1_trend addition to common_trend_priors, centralized prior system implementation, and comprehensive test coverage (121 tests passing at 100% success rate)

---

## 🚨 CRITICAL ISSUE: VAR/VARMA Hierarchical Grouping Support

**PRIORITY**: HIGH - Must be addressed immediately after likelihood implementation (tasks 2.7.8.10-2.7.8.16)

**PROBLEM**: The current VAR/VARMA implementation **DOES NOT SUPPORT** hierarchical/grouped models correctly. While basic Stan code generation works, grouped data will fail because:

1. **Missing Group-Specific Covariance**: Single `Sigma_trend` matrix instead of `Sigma_trend[g]` for each group
2. **No Group Indexing**: `lv_trend` matrix lacks proper group indexing structure  
3. **Missing Hierarchical Correlations**: No integration with hierarchical correlation infrastructure
4. **Incomplete Group Validation**: Group-specific parameter validation missing

**IMPACT**: VAR/VARMA models with `gr` parameter will:
- Generate invalid Stan code for grouped scenarios
- Fail to properly model group-specific process correlations  
- Produce incorrect parameter estimates for grouped data
- Break mvgam fitting for hierarchical VAR/VARMA models

**SOLUTION**: Tasks 2.7.8.17-2.7.8.19 implement:
- Group-specific `Sigma_trend[n_groups]` arrays
- Proper group indexing in all VAR/VARMA parameters
- Integration with existing hierarchical correlation infrastructure
- Group-level hyperprior specifications

---

## NEXT TASK: Parameter Standardization and Non-Centered Parameterization

**Context**: The current system uses inconsistent parameter names across trend generators (raw_innovations vs innovations_trend, LV vs lv_trend, L_Omega vs L_Omega_trend). We need to standardize on a three-layer architecture:

1. **`innovations_trend`** (parameters block) - raw std_normal innovations  
2. **`scaled_innovations_trend`** (transformed parameters) - after applying sigma_trend scaling and correlations
3. **`lv_trend`** (transformed parameters) - final latent states using trend-specific dynamics

**Structural Pattern**: Each trend generator must follow the 3-stanvar pattern for clarity:
- **`{trend}_parameters_stanvar`** (block = "parameters") - Declares all trend-specific parameters
- **`{trend}_tparameters_stanvar`** (block = "tparameters") - Computes lv_trend dynamics
- **`{trend}_model_stanvar`** (block = "model") - Only priors, no computations

**Why this task is next**: All current tests pass with existing names, providing a stable foundation. Parameter standardization must happen before implementing trend registry prior specifications (section 3.0) to avoid name conflicts and ensure consistency.

**Testing strategy**: Each phase is independently testable with 15-minute completion targets to enable fail-fast validation.

### 2.7 Parameter Standardization and Non-Centered Parameterization 🔄
- [x] 2.7.1 Phase 1.1: Update shared innovation system parameter names (raw_innovations → innovations_trend, L_Omega → L_Omega_trend) - COMPLETED
- [x] 2.7.2 Phase 1.2: Update shared innovation system to generate scaled_innovations_trend in transformed parameters (apply sigma_trend and correlations) - COMPLETED
- [x] 2.7.3 Phase 1.3: Test shared innovation system in isolation with simple test case - COMPLETED: Updated tests resolved 7 failures, parameter standardization working
- [x] 2.7.4 Phase 2.1: Update RW generator to use scaled_innovations_trend → lv_trend pattern (simplest case) - COMPLETED: Established ma_innovations_trend naming convention for MA transformations, implemented 3-stanvar pattern (rw_parameters, rw_tparameters, rw_model), added validation fixes for helper function and dimension checking
- [x] 2.7.5 Phase 2.2: Refactor AR generator - implement 3-stanvar pattern (ar_parameters, ar_tparameters, ar_model) and use standardized parameter names - COMPLETED: Implemented clean 3-stanvar pattern with proper validation, supports multiple AR lags (including discontinuous lags), MA combinations, factor models, uses standardized parameter names (ar{lag}_trend, lv_trend, ma_innovations_trend, scaled_innovations_trend), added comprehensive tests covering unit testing, integration testing, edge cases, and system consistency
- [x] 2.7.6 Phase 2.3: Refactor CAR generator - implement 3-stanvar pattern and standardized parameter names - COMPLETED: Implemented clean 3-stanvar pattern (car_parameters, car_tparameters, car_model), updated to use standardized parameter names (ar1_trend, lv_trend, scaled_innovations_trend), removed mu_trend from dynamics for consistency with RW/AR generators, added proper validation, maintains CAR-specific features (time_dis, continuous-time AR evolution)  
- [x] 2.7.7 Phase 2.4: Refactor ZMVN generator - convert to non-centered parameterization with 3-stanvar pattern - COMPLETED: Implemented clean 3-stanvar pattern (zmvn_parameters, zmvn_tparameters, zmvn_model), uses shared innovation system for non-centered parameterization with direct transformation lv_trend = scaled_innovations_trend, works for both n_lv=1 and n_lv>1 cases, properly handles hierarchical and factor model cases, added comprehensive tests covering univariate/multivariate, correlations, factor models, hierarchical structures, edge cases, validation, standardized naming (11/11 tests passing)
- [ ] 2.7.8 Phase 2.5: Refactor VAR/VARMA generator - implement complete Heaps 2022 stationary VARMA(p,q) with 3-stanvar pattern (VAR exempt from shared innovations)
  - [x] 2.7.8.1 Parameter standardization design (15 min): Map all Heaps parameters to _trend naming convention, document VAR vs VARMA conditional logic based on ma_lags parameter, design 3-stanvar pattern (var_parameters, var_tparameters, var_model) - COMPLETED: Established comprehensive parameter mapping from Heaps 2022 to mvgam standards (A→A_trend, D→D_trend, phi→phi_trend, theta→theta_trend, Sigma→Sigma_trend, Omega→Omega_trend, init→init_trend, yfull→lv_full_trend, epsinit→eps_init_trend, mut→mu_t_trend, Amu→Amu_trend, Aomega→Aomega_trend, Dmu→Dmu_trend, Domega→Domega_trend). Documented conditional logic: VAR(p) when ma_lags=0 (core parameters: A_trend[lags], Amu_trend[2], Aomega_trend[2], Sigma_trend, lv_trend[n_trend], init_trend[lags*n_lv_trend]) vs VARMA(p,q) when ma_lags>0 (additional: D_trend[ma_lags], Dmu_trend[2], Domega_trend[2], extended init_trend[(lags+ma_lags)*n_lv_trend]). Designed 3-stanvar pattern: var_parameters (all parameter declarations with conditional VARMA), var_tparameters (stationary coefficient computation: phi_trend, theta_trend, Omega_trend, working arrays), var_model (VARMA likelihood + hierarchical priors). Fixed hyperparameters in transformed data: es_trend, fs_trend, gs_trend, hs_trend as constants not user inputs. Input validation: lags≥1, ma_lags≥0, factor model when n_lv<n_series.
  - [x] 2.7.8.2 Functions block implementation (15 min): Implement sqrtm(), kronecker_prod(), AtoP(), and update array syntax to modern Stan in functions block stanvar - COMPLETED: Implemented var_functions_stanvar with 5 mathematical functions using modern Stan syntax: (1) sqrtm() for matrix square root with numerical stability checks, (2) AtoP() for partial autocorrelation transformation (renamed from P_realtoP), (3) kronecker_prod() for Kronecker products in companion matrix approach, (4) rev_mapping() for stationary coefficient computation with forward/reverse passes, (5) initial_joint_var() for VARMA covariance using companion matrix and Lyapunov equation. Updated array syntax to modern Stan (array[] matrix[,]). Added comprehensive validation and error handling. Tested successfully with various VAR/VARMA parameter combinations.
  - [x] 2.7.8.3 Reverse mapping function (15 min): Implement rev_mapping() function with updated array syntax, ensure proper return type for stationary VAR/VARMA coefficient computation - COMPLETED: Added rev_mapping() function to var_functions_stanvar implementing complete Heaps 2022 reverse mapping algorithm with modern Stan array syntax (array[,] matrix[,]), forward and reverse passes for phi coefficient computation, proper handling of matrix operations with numerical stability.
  - [x] 2.7.8.4 Initial joint variance function (15 min): Implement initial_joint_var() function for VARMA(p,q) stationary initialization using companion matrix approach - COMPLETED: Implemented companion matrix approach for stationary VARMA covariance computation, constructs proper companion matrix with VAR (phi) and MA (theta) components, solves Lyapunov equation using Kronecker product formulation, handles cross-covariances between y_t and eps_t innovations, ensures numerical symmetry.
  - [x] 2.7.8.5 Transformed data setup (15 min): Create transformed data stanvar with trend_zeros, scale_mat_trend, and hyperparameter constants (es_trend, fs_trend, gs_trend, hs_trend) - put hyperparameters in tdata not as user inputs - COMPLETED: Implemented var_tdata_stanvar with comprehensive transformed data block including trend_zeros vector, scale_mat_trend for inverse Wishart priors, complete hyperparameter arrays (es_trend, fs_trend, gs_trend, hs_trend) for A_trend coefficients, conditional MA hyperparameters (es_ma_trend, fs_ma_trend, gs_ma_trend, hs_ma_trend) when ma_lags > 0, df_trend for inverse Wishart degrees of freedom, all as constants not user inputs. Tested successfully with both VAR and VARMA cases.
  - [x] 2.7.8.6 Core parameters block (15 min): Implement VAR parameters: A_trend[lags], Sigma_trend, Amu_trend[2], Aomega_trend[2], lv_trend[n_trend], init_trend for joint initialization - COMPLETED: Implemented var_parameters_stanvar with core VAR coefficient matrices A_trend[lags] using modern Stan array syntax, innovation covariance Sigma_trend with cov_matrix constraint, hierarchical hyperparameters Amu_trend[2] and Aomega_trend[2] with array[2] for diagonal/off-diagonal distinction, conditional init_trend vector sizing (lags*n_lv for VAR, (lags+ma_lags)*n_lv for VARMA), lv_trend matrix for latent variable time series. Proper constraints applied (cov_matrix for Sigma_trend, lower=0 for Aomega_trend). Code reviewer approved implementation with praise for Stan syntax, constraints, and conditional logic. Tested successfully with both VAR and VARMA parameter combinations.
  - [x] 2.7.8.7 MA parameters block (15 min): Add conditional VARMA parameters when ma_lags > 0: D_trend[ma_lags], Dmu_trend[2], Domega_trend[2], handle VAR-only case - COMPLETED: Implemented conditional var_ma_parameters_stanvar that is only created when is_varma=TRUE (ma_lags > 0), follows same hierarchical hyperparameter structure as VAR parameters with array[2] for diagonal/off-diagonal distinction, D_trend naming follows Heaps 2022 convention for MA coefficients, added logical consistency validation between is_varma flag and ma_lags value using checkmate assertions, improved documentation explaining conditional behavior and component list structure (3 stanvars for VAR, 4 for VARMA), fixed Stan code indentation for consistency, tested successfully with ma_lags=0,1,2 cases. Code reviewer initially requested validation improvements which were addressed.
  - [x] 2.7.8.8 Stationary coefficient computation (15 min): Implement transformed parameters: compute phi_trend and theta_trend from A_trend and D_trend using rev_mapping(), handle both VAR and VARMA cases - COMPLETED: Implemented var_tparameters_stanvar with transformed parameters block that computes stationary coefficients (phi_trend, theta_trend) from partial autocorrelations (A_trend, D_trend) using rev_mapping() function. Uses modern Stan array syntax, handles both VAR(p) and VARMA(p,q) cases with conditional theta_trend computation, applies negative sign to theta coefficients following Heaps 2022 convention, added clear documentation explaining result array structure (row 1=coefficients, row 2=Gamma matrices), removed nested block structure for better scoping, integrated properly into 4-stanvar (VAR) and 5-stanvar (VARMA) component lists. Package loads successfully with implementation. Code reviewer provided feedback on validation (already present in parent function) and documentation improvements (addressed).
  - [x] 2.7.8.9 Initial covariance computation (15 min): Compute Omega_trend using initial_joint_var() for VARMA, implement VAR-only covariance computation, set up lv_full_trend and eps_init_trend arrays - COMPLETED: Added Omega_trend computation to var_tparameters_stanvar using initial_joint_var() function for both VAR and VARMA cases, implemented conditional logic with proper VAR-only handling (empty theta arrays), set up lv_full_trend[n_trend + lags] working array for extended time series (y_{{1-lags}},...,y_N), added eps_init_trend[ma_lags] array for VARMA initial MA errors, implemented proper array initialization from init_trend vector with correct indexing for both VAR and VARMA parameter layouts, fixed glue template parsing issues with pre-computed conditional code blocks, tested successfully with VAR(1), VAR(2), VARMA(1,1), VARMA(2,1) cases including grouping support, all Stan code generation tests passing. Ready for next task (likelihood implementation).
  - [ ] 2.7.8.10 VARMA likelihood - initialization (15 min): **NEXT TASK** - Implement joint initial distribution for first p+q observations using multi_normal with Omega_trend, set up mu_init_trend vector
  - [ ] 2.7.8.11 VARMA likelihood - conditional means (15 min): Implement complex conditional mean computation for VARMA dynamics: mu_t_trend[1] with VAR and MA components, handle eps_init_trend
  - [ ] 2.7.8.12 VARMA likelihood - full time series (15 min): Complete conditional mean computation for t=2 to q (transition period) and t=q+1 to N (full VARMA), handle lagged errors properly
  - [ ] 2.7.8.13 VAR coefficient priors (15 min): Implement hierarchical priors for A_trend matrices: diagonal and off-diagonal elements with Amu_trend and Aomega_trend hyperparameters
  - [ ] 2.7.8.14 MA coefficient priors (15 min): Add conditional MA priors for D_trend matrices when ma_lags > 0, implement Dmu_trend and Domega_trend hyperpriors
  - [ ] 2.7.8.15 Innovation covariance priors (15 min): Implement Sigma_trend and consider integrating with shared innovation system, integrate with centralized prior system using generate_trend_priors_stanvar()
  - [ ] 2.7.8.16 System integration and validation (15 min): Integrate factor model support, add trend computation stanvars, implement input validation for lags and ma_lags parameters, test with simple VAR(1) case
  - [ ] 2.7.8.17 Hierarchical grouping support (20 min): **CRITICAL** - Implement hierarchical process correlations for grouped VAR/VARMA models. Add group-specific Sigma_trend matrices, integrate with hierarchical correlation infrastructure, handle group indexing in lv_trend, and ensure proper group-level parameter estimation. **CONTEXT**: VAR/VARMA models need hierarchical Sigma_trend[g] for each group g, with global/group-specific correlation structures following the pattern used in other generators. This is essential for proper grouped model functionality.
  - [ ] 2.7.8.18 Group-specific covariance computation (15 min): Update initial_joint_var() usage to handle group-specific Sigma_trend matrices in Omega_trend computation, ensure group indexing consistency across init_trend and lv_trend parameters
  - [ ] 2.7.8.19 Hierarchical prior integration (15 min): Connect VAR/VARMA hierarchical parameters with centralized prior system, ensure group-level hyperpriors for Amu_trend, Aomega_trend, Dmu_trend, Domega_trend follow established patterns
- [ ] 2.7.9 Phase 2.6: Update PW generator if needed (may not use shared innovations)
- [ ] 2.7.10 Phase 3.1: Update all tests to expect new parameter names (innovations_trend, lv_trend, etc.)
- [ ] 2.7.11 Phase 3.2: Update common_trend_priors to remove LV and use consistent naming
- [ ] 2.7.12 Phase 3.3: Run full test suite and verify all trend types work with new standardization
- [ ] 2.7.13 Phase 3.4: **CRITICAL** - Test hierarchical VAR/VARMA models with grouped data to ensure proper group-specific parameter estimation, validate group indexing, and confirm hierarchical correlation functionality

---

## PENDING SECTIONS (After Parameter Standardization)

### 3.0 Complete Prior Specification System ⏳
- [ ] 3.1 Add prior_spec to RW trend registration - reference common_trend_priors.sigma_trend in .onLoad()
- [ ] 3.2 Add prior_spec to AR trend registration - use build_ar_prior_spec() for dynamic lag handling in .onLoad()
- [ ] 3.3 Add prior_spec to VAR trend registration - include only sigma_trend (process variances), A_trend matrix handled by stationarity constraints
- [ ] 3.4 Add prior_spec to CAR trend registration - reference common_trend_priors.sigma_trend and ar1_trend
- [ ] 3.5 Add prior_spec to ZMVN trend registration - minimal spec, mostly uses defaults
- [ ] 3.6 Add prior_spec to PW trend registration - include k_trend, m_trend, delta_trend specs
- [ ] 3.7 Update trend dispatcher in stan_assembly.R - pass prior argument through dispatch chain to all generators
- [ ] 3.8 Update tests in test-trend_system.R - add prior = NULL to all generate_*_trend_stanvars() calls
- [ ] 3.9 Update tests in test-stan_assembly.R - add prior = NULL to generate_combined_stancode() calls
- [ ] 3.10 Update tests in test-setup-brms.R - verify prior parameter flows through setup_brms_lightweight()
- [ ] 3.11 Create test for prior flow through generate_combined_stancode() - verify priors reach generators
- [ ] 3.12 Create test for AR lag-specific priors - test ar1_trend, ar12_trend, ar24_trend prior application
- [ ] 3.13 Create test for common_trend_priors inheritance - verify sigma_trend shared across RW, AR, CAR
- [ ] 3.14 Create test for default prior fallback - verify defaults used when no user prior specified
- [ ] 3.15 Write core system tests in `tests/testthat/test-priors.R` - test prior extraction, combination logic, and Stan generation with various trend types and multivariate scenarios

### 4.0 Implement Fixed Z Matrix Support ⏳
- [ ] 4.1 Implement fixed Z matrix support for dimension-reduced factor models - enhance prior system to detect fixed Z matrix specifications, convert Z from stochastic parameter to data object in Stan model, enable shared state models and user-specified factor structures with proper validation and Stan code generation updates

### 5.0 Create User-Facing Inspection Functions ⏳
- [ ] 5.1 Implement get_prior() function in `R/priors.R` - accept same parameters as mvgam(), use setup_brms_lightweight() and prior extraction system, return brmsprior object with proper validation
- [ ] 5.2 Implement make_stancode() function in `R/priors.R` - accept same parameters plus prior argument, use existing generate_combined_stancode() pipeline, return character string with complete Stan model code
- [ ] 5.3 Implement make_standata() function in `R/priors.R` - accept same parameters as make_stancode(), use existing combine_stan_data() system, return named list compatible with Stan
- [ ] 5.4 Create set_prior() and prior() extensions in `R/priors.R` - enhance brms functions to handle trend-specific parameters with proper class validation and parameter bounds
- [ ] 5.5 Implement prior_summary() function in `R/priors.R` - work with both fitted mvgam objects and model specifications, show actual vs planned priors with all/non-default filtering
- [ ] 5.6 Add roxygen2 documentation in `R/priors.R` - comprehensive @param, @return, @examples following TRD specifications with cross-references and proper @export tags
- [ ] 5.7 Write user interface tests in `tests/testthat/test-make_stan.R` - test all user-facing functions with realistic workflows, edge cases, and error conditions matching TRD examples

### 6.0 Comprehensive Testing and Integration Validation ⏳
- [ ] 6.1 Create brms equivalence tests in `tests/testthat/test-brms-equivalence.R` - verify that when trend_formula = NULL, generated Stan code exactly matches brm() output for various model types
- [ ] 6.2 Build complete workflow tests in `tests/testthat/test-complete-workflows.R` - test entire get_prior() -> modify -> make_stan*() -> mvgam() -> prior_summary() workflows with validation
- [ ] 6.3 Test multivariate scenarios in `tests/testthat/test-multivariate-inspection.R` - validate inspection functions work correctly with multivariate models, different trends per response, and factor models
- [ ] 6.4 Validate statistical correctness in `tests/testthat/test-statistical-validation.R` - ensure modified priors in inspection functions correctly apply in fitted models using small test datasets
- [ ] 6.5 Test edge cases and error handling in `tests/testthat/complete-workflows.R` - invalid formulas, incompatible trends, malformed priors, with verification of helpful error messages
- [ ] 6.6 Update package documentation in `README.md` and `vignettes/` - add inspection workflow examples, update package overview to highlight new capabilities following TRD requirements

---

## Notes
- Each sub-task should be completable within 15 minutes for fail-fast validation
- Use `code-reviewer` agent for all R code changes (non-negotiable)
- All tests must pass before moving to the next section
- Parameter standardization (2.7) is the critical path blocking all subsequent work

## VAR/VARMA Implementation Context

**Key Architectural Decisions for New Developer:**
1. **No Shared Innovations**: VAR/VARMA generator is exempt from shared innovation system used by RW/AR/CAR/ZMVN. VAR handles its own parameter and prior system completely independently.
2. **Conditional Structure**: Single generator handles both VAR(p) and VARMA(p,q) based on ma_lags parameter (0 = VAR only, >0 = VARMA).
3. **Function Location**: Working in generate_var_trend_stanvars() in R/stan_assembly.R starting around line 2246 - this entire function needs replacement.
4. **Reference Implementation**: Original Heaps 2022 VARMA model provided below for mathematical reference, but array syntax needs updating to modern Stan.
5. **3-Stanvar Pattern**: Must follow var_parameters, var_tparameters, var_model naming pattern like other generators.
6. **Parameter Validation**: Use checkmate::assert_* for input validation following patterns in other generators.
7. **Helper Functions**: Use append_if_not_null() helper and do.call(combine_stanvars, components) for assembly.
8. **Testing Strategy**: Each 15-minute step should be immediately testable in isolation before proceeding.
9. **🚨 CRITICAL HIERARCHICAL SUPPORT**: Current implementation is **INCOMPLETE** for hierarchical/grouped models. Tasks 2.7.8.17-2.7.8.19 are **ESSENTIAL** before VAR/VARMA can be used with grouped data. The generator currently lacks group-specific Sigma_trend matrices, hierarchical correlation structures, and proper group indexing - this will cause **failures** in grouped model scenarios.

**Next Step**: Implement var_functions_stanvar with sqrtm(), kronecker_prod(), AtoP() functions using modern Stan array syntax.

## Original Heaps VARMA parameterisation (uses out of date array syntax)
functions {
  /* Function to compute the matrix square root */
  matrix sqrtm(matrix A) {
    int m = rows(A);
    vector[m] root_root_evals = sqrt(sqrt(eigenvalues_sym(A)));
    matrix[m, m] evecs = eigenvectors_sym(A);
    matrix[m, m] eprod = diag_post_multiply(evecs, root_root_evals);
    return tcrossprod(eprod);
  }
  /* Function to compute Kronecker product */
  matrix kronecker_prod(matrix A, matrix B) {
    matrix[rows(A) * rows(B), cols(A) * cols(B)] C;
    int m = rows(A);
    int n = cols(A);
    int p = rows(B);
    int q = cols(B);
    for (i in 1:m) {
      for (j in 1:n) {
        int row_start = (i - 1) * p + 1;
        int row_end = (i - 1) * p + p;
        int col_start = (j - 1) * q + 1;
        int col_end = (j - 1) * q + q;
        C[row_start:row_end, col_start:col_end] = A[i, j] * B;
      }
    }
    return C;
  }
  /* Function to transform A to P (inverse of part 2 of reparameterisation) */
  matrix AtoP(matrix A) {
    int m = rows(A);
    matrix[m, m] B = tcrossprod(A);
    for(i in 1:m) B[i, i] += 1.0;
    return mdivide_left_spd(sqrtm(B), A);
  }
  /* Function to perform the reverse mapping from the Appendix. The details of
     how to perform Step 1 are in Section S1.3 of the Supplementary Materials.
     Returned: a length-p array of (m x m) matrices; the i-th component
               of the array is phi_i */
  matrix[] rev_mapping(matrix[] P, matrix Sigma) {
    int p = size(P);
    int m = rows(Sigma);
    matrix[m, m] phi_for[p, p];   matrix[m, m] phi_rev[p, p];
    matrix[m, m] Sigma_for[p+1];  matrix[m, m] Sigma_rev[p+1];
    matrix[m, m] S_for;           matrix[m, m] S_rev;
    matrix[m, m] S_for_list[p+1];
    // Step 1:
    Sigma_for[p+1] = Sigma;
    S_for_list[p+1] = sqrtm(Sigma);
    for(s in 1:p) {
      // In this block of code S_rev is B^{-1} and S_for is a working matrix
      S_for = - tcrossprod(P[p-s+1]);
      for(i in 1:m) S_for[i, i] += 1.0;
      S_rev = sqrtm(S_for);
      S_for_list[p-s+1] = mdivide_right_spd(mdivide_left_spd(S_rev, 
                              sqrtm(quad_form_sym(Sigma_for[p-s+2], S_rev))), S_rev);
      Sigma_for[p-s+1] = tcrossprod(S_for_list[p-s+1]);
    }
    // Step 2:
    Sigma_rev[1] = Sigma_for[1];
    for(s in 0:(p-1)) {
      S_for = S_for_list[s+1];
      S_rev = sqrtm(Sigma_rev[s+1]);
      phi_for[s+1, s+1] = mdivide_right_spd(S_for * P[s+1], S_rev);
      phi_rev[s+1, s+1] = mdivide_right_spd(S_rev * P[s+1]', S_for);
      if(s>=1) {
        for(k in 1:s) {
          phi_for[s+1, k] = phi_for[s, k] - phi_for[s+1, s+1] * phi_rev[s, s-k+1];
          phi_rev[s+1, k] = phi_rev[s, k] - phi_rev[s+1, s+1] * phi_for[s, s-k+1];
        }
      }
      Sigma_rev[s+2] = Sigma_rev[s+1] - quad_form_sym(Sigma_for[s+1], 
                                                      phi_rev[s+1, s+1]');
    }
    return phi_for[p];
  }
  /* Function to compute the joint (stationary) distribution of 
     (y_0, ..., y_{1-p}, eps_0, ..., eps_{1-q}). Details of the underpinning
     ideas are given in Section S7 of the Supplementary Materials. */
  matrix initial_joint_var(matrix Sigma, matrix[] phi, matrix[] theta) {
    int p = size(phi);
    int q = size(theta);
    int m = rows(Sigma);
    matrix[(p+q)*m, (p+q)*m] companion_mat = rep_matrix(0.0, (p+q)*m, (p+q)*m);
    matrix[(p+q)*m, (p+q)*m] companion_var = rep_matrix(0.0, (p+q)*m, (p+q)*m);
    matrix[(p+q)*m*(p+q)*m, (p+q)*m*(p+q)*m] tmp = diag_matrix(rep_vector(1.0, 
                                                             (p+q)*m*(p+q)*m));
    matrix[(p+q)*m, (p+q)*m] Omega;
    // Construct phi_tilde:
    for(i in 1:p) {
      companion_mat[1:m, ((i-1)*m+1):(i*m)] = phi[i];
      if(i>1) {
        for(j in 1:m) {
          companion_mat[(i-1)*m+j, (i-2)*m+j] = 1.0;
        }
      }
    }
    for(i in 1:q) {
      companion_mat[1:m, ((p+i-1)*m+1):((p+i)*m)] = theta[i];
    }
    if(q>1) {
      for(i in 2:q) {
        for(j in 1:m) {
          companion_mat[(p+i-1)*m+j, (p+i-2)*m+j] = 1.0;
        }
      }
    }
    // Construct Sigma_tilde:
    companion_var[1:m, 1:m] = Sigma;
    companion_var[(p*m+1):((p+1)*m), (p*m+1):((p+1)*m)] = Sigma;
    companion_var[1:m, (p*m+1):((p+1)*m)] = Sigma;
    companion_var[(p*m+1):((p+1)*m), 1:m] = Sigma;
    // Compute Gamma0_tilde
    tmp -= kronecker_prod(companion_mat, companion_mat);
    Omega = to_matrix(tmp \ to_vector(companion_var), (p+q)*m, (p+q)*m);
    // Ensure Omega is symmetric:
    for(i in 1:(rows(Omega)-1)) {
      for(j in (i+1):rows(Omega)) {
        Omega[j, i] = Omega[i, j];
      }
    }
    return Omega;
  }
}
data {
  int<lower=1> m; // Dimension of observation vector
  int<lower=1> p; // Order of VAR component
  int<lower=1> q; // Order of VMA component
  int<lower=1> N; // Length of time series
  vector[m] y[N]; // Time series
  // Hyperparameters in exchangeable prior for the A_i (component 1 of arrays) 
  // and D_i (component 2 of arrays). See Section 3.2 of the paper.
  vector[2] es[2];
  vector<lower=0>[2] fs[2];
  vector<lower=0>[2] gs[2];
  vector<lower=0>[2] hs[2];
  // Hyperparameters in exchangeable inverse Wishart prior for Sigma
  real<lower=0> scale_diag;                    // Diagonal element in scale matrix
  real<lower=-scale_diag/(m-1)> scale_offdiag; /* Off-diagonal element in scale 
                                                  matrix */
  real<lower=m+3> df;                          /* Degrees of freedom (limit ensures 
                                                  finite variance) */
}
transformed data {
  vector[m] mu = rep_vector(0.0, m); // (Zero)-mean of VARMA process
  matrix[m, m] scale_mat;            // Scale-matrix in prior for Sigma
  for(i in 1:m) {
    for(j in 1:m) {
      if(i==j) scale_mat[i, j] = scale_diag;
      else scale_mat[i, j] = scale_offdiag;
    }
  }
}
parameters {
  vector[m*(p+q)] init; // (y_0^T, ..., y_{1-p}^T, eps_0^T, ..., eps_{1-q}^T)^T
  matrix[m, m] A[p];    // The A_i
  matrix[m, m] D[q];    // The D_i
  cov_matrix[m] Sigma;  // Error variance, Sigma
  // Means and precisions in top-level prior for the diagonal and off-diagonal
  // elements in the A_i
  vector[p] Amu[2];
  vector<lower=0>[p] Aomega[2];
  // Means and precisions in top-level prior for the diagonal and off-diagonal
  // elements in the D_i
  vector[q] Dmu[2];
  vector<lower=0>[q] Domega[2];
}
transformed parameters {
  matrix[m, m] phi[p];       // The phi_i
  matrix[m, m] theta[q];     // The theta_i
  cov_matrix[(p+q)*m] Omega; // Variance in initial distribution, i.e. Gamma0_tilde
  vector[m] yfull[N+p];      // (y_{1-p}^T, ..., y_{N}^T)^T
  vector[m] epsinit[q];      // (eps_0^T, ..., eps_{1-q}^T)^T
  {
    matrix[m, m] P[p];
    matrix[m, m] R[q];
    for(i in 1:p) P[i] = AtoP(A[i]);
    for(i in 1:q) R[i] = AtoP(D[i]);
    phi = rev_mapping(P, Sigma);
    theta = rev_mapping(R, Sigma);
    for(i in 1:q) theta[i] = -theta[i];
    Omega = initial_joint_var(Sigma, phi, theta);
    for(i in 1:p) {
      yfull[i] = init[((p-i)*m+1):((p-i+1)*m)]; // y[1-p],...,y[0]
    }
    yfull[(p+1):(p+N)] = y;
    for(i in 1:q) {
      epsinit[i] = init[(p*m+(i-1)*m+1):(p*m+i*m)]; // eps[0],...,eps[1-q]
    }
  }
}
model {
  vector[(p+q)*m] mut_init; /* Marginal mean of 
                               (y_0, ..., y_{1-p}, eps_0, ..., eps_{1-q}) */
  vector[m] mut[N];         // Conditional means of y_{1}, ..., y_{N}
  // (Complete data) likelihood:
  for(t in 1:p) mut_init[((t-1)*m+1):(t*m)] = mu;
  mut_init[(p*m+1):((p+q)*m)] = rep_vector(0.0, q*m);
  mut[1] = mu;
  for(i in 1:p) {
    mut[1] += phi[i] * (yfull[p+1-i] - mu);
  }
  for(i in 1:q) {
    mut[1] += theta[i] * epsinit[i];
  }
  if(q>1) {
    for(t in 2:q) {
      mut[t] = mu;
      for(i in 1:p) {
        mut[t] += phi[i] * (yfull[p+t-i] - mu);
      }
      for(i in 1:(t-1)) {
        mut[t] += theta[i] * (yfull[p+t-i] - mut[t-i]);
      }
      for(i in t:q) {
        mut[t] += theta[i] * epsinit[i-t+1];
      }
    }
  }
  for(t in (q+1):N) {
    mut[t] = mu;
    for(i in 1:p) {
      mut[t] += phi[i] * (yfull[p+t-i] - mu);
    }
    for(i in 1:q) {
      mut[t] += theta[i] * (yfull[p+t-i] - mut[t-i]);
    }
  }
  init ~ multi_normal(mut_init, Omega);
  y ~  multi_normal(mut, Sigma);
  // Prior:
  Sigma ~ inv_wishart(df, scale_mat);
  for(s in 1:p) {
    diagonal(A[s]) ~ normal(Amu[1, s], 1 / sqrt(Aomega[1, s]));
    for(i in 1:m) {
      for(j in 1:m) {
        if(i != j) A[s, i, j] ~ normal(Amu[2, s], 1 / sqrt(Aomega[2, s]));
      }
    }
  }
  for(s in 1:q) {
    diagonal(D[s]) ~ normal(Dmu[1, s], 1 / sqrt(Domega[1, s]));
    for(i in 1:m) {
      for(j in 1:m) {
        if(i != j) D[s, i, j] ~ normal(Dmu[2, s], 1 / sqrt(Domega[2, s]));
      }
    }
  }
  // Hyperprior:
  for(i in 1:2) {
    Amu[i] ~ normal(es[1,i], fs[1,i]);
    Aomega[i] ~ gamma(gs[1,i], hs[1,i]); 
    Dmu[i] ~ normal(es[2,i], fs[2,i]);
    Domega[i] ~ gamma(gs[2,i], hs[2,i]); 
  }
}
