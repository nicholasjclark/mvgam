# Development Tasks: Prediction System Implementation

**Generated**: 2025-11-05
**Source TRD**: `tasks/trd-prediction-system.md`
**Implementation Strategy**: `tasks/prediction-system-implementation-strategy.md`
**Architecture**: Empirically verified approach using `brms:::prepare_predictions()`

---

## Relevant Files

### R Package Files (New)
- `R/predictions.R` - Core prediction infrastructure and helpers
- `R/mock-stanfit.R` - S3 mock stanfit for parameter subsetting
- `R/posterior-linpred.R` - `posterior_linpred.mvgam()` implementation
- `R/posterior-epred.R` - `posterior_epred.mvgam()` implementation
- `R/posterior-predict.R` - `posterior_predict.mvgam()` implementation
- `R/posterior-smooths.R` - `posterior_smooths.mvgam()` implementation
- `R/posterior-average.R` - `posterior_average.mvgam()` implementation
- `R/predict.R` - `predict.mvgam()` and `fitted.mvgam()` wrappers
- `R/marginaleffects.R` - marginaleffects S3 methods
- `R/conditional_effects.R` - Enhanced `conditional_effects.mvgam()`

### R Package Files (Modified)
- `R/mvgam_core.R` - Add obs_model/trend_model storage (line ~280)
- `R/brms_integration.R` - Utility functions for parameter extraction
- `R/validations.R` - Input validation helpers

### Rcpp Files (New)
- `src/prediction_funs.cpp` - Efficient trend computation functions

### Test Files (New)
- `tests/testthat/test-mock-stanfit.R` - Mock stanfit S3 tests
- `tests/testthat/test-parameter-identification.R` - Parameter categorization tests
- `tests/testthat/test-prep-extraction.R` - Design matrix extraction tests
- `tests/testthat/test-posterior-linpred.R` - Linear predictor tests
- `tests/testthat/test-posterior-epred.R` - Expected value tests
- `tests/testthat/test-posterior-predict.R` - Posterior predictive tests
- `tests/testthat/test-posterior-smooths.R` - Smooth extraction tests
- `tests/testthat/test-predict-fitted.R` - Wrapper function tests
- `tests/testthat/test-marginaleffects.R` - marginaleffects integration tests
- `tests/testthat/test-predictions-integration.R` - End-to-end workflow tests
- `tests/testthat/test-predictions-multivariate.R` - Multivariate model tests
- `tests/testthat/test-predictions-edge-cases.R` - Edge case tests

### Context Files
- `architecture/architecture-decisions.md` - Lazy categorization system
- `architecture/dependency-graph.md` - Package structure reference
- `tests/testthat/setup.R` - Test helpers (SW, SM wrappers)

### Package Structure Files
- `NAMESPACE` - Export declarations (auto-generated via roxygen2)
- `DESCRIPTION` - May need marginaleffects in Suggests

### Notes
- Use `devtools::load_all()` before testing
- Run tests with `Rscript -e "devtools::load_all();testthat::test_file('path/to/test')"`
- Standard test settings: `chains = 2, iter = 500, silent = 2`
- Pre-fitted models available: `mvgam_example1-4` in `sysdata.rda`
- Test data helper: `setup_stan_test_data()` in test files

---

## Tasks

- [ ] **1.0 Foundation Infrastructure**
  - [x] 1.1 Add obs_model and trend_model storage to mvgam objects
  - [x] 1.2 Create S3 mock stanfit class and as_draws_matrix method
  - [x] 1.3 Create .categorize_parameters() internal helper
    - [ ] 1.3.1 Plan alias column implementation (map Stan to brms names)
  - [ ] 1.4 Update validate_variables() to use .categorize_parameters()
  - [ ] 1.5 Update tidy.mvgam() to use .categorize_parameters()
  - [ ] 1.6 Update mcmc_plot.mvgam() and pairs.mvgam() to use helper
  - [ ] 1.7 Write unit tests for .categorize_parameters()
  - [ ] 1.8 Verify all affected functions work correctly

- [ ] **2.0 Core Linear Predictor Computation**
  - [ ] 2.1 Create extract_linpred_from_prep() for observation model
  - [ ] 2.2 Create extract_linpred_from_prep() for trend model
  - [ ] 2.3 Implement parameter subset extraction from combined fit
  - [ ] 2.4 Create prep object preparation helper
  - [ ] 2.5 Write unit tests for linpred extraction (compare to brms)
  - [ ] 2.6 Implement posterior_linpred.mvgam() with all parameters
  - [ ] 2.7 Add input validation for posterior_linpred.mvgam()
  - [ ] 2.8 Write integration tests for posterior_linpred.mvgam()

- [ ] **3.0 Scale Transformations and Prediction Types**
  - [ ] 3.1 Implement inverse link transformation helper
  - [ ] 3.2 Implement posterior_epred.mvgam() using posterior_linpred
  - [ ] 3.3 Create family-specific observation noise sampling functions
  - [ ] 3.4 Implement posterior_predict.mvgam() with noise addition
  - [ ] 3.5 Add distributional parameter extraction (sigma, shape, etc.)
  - [ ] 3.6 Write tests for scale transformations (link → epred → predict)
  - [ ] 3.7 Test posterior_predict with multiple families
  - [ ] 3.8 Verify integer outputs for discrete families

- [ ] **4.0 Specialized Prediction Functions**
  - [ ] 4.1 Implement smooth term extraction from prep objects
  - [ ] 4.2 Implement posterior_smooths.mvgam() for single smooth
  - [ ] 4.3 Extend posterior_smooths.mvgam() for all smooths
  - [ ] 4.4 Implement posterior_average.mvgam() for categorical families
  - [ ] 4.5 Add multivariate model detection and handling
  - [ ] 4.6 Implement list return format for multivariate predictions
  - [ ] 4.7 Handle response-specific trends in multivariate models
  - [ ] 4.8 Write tests for specialized prediction functions

- [ ] **5.0 Convenience Wrappers**
  - [ ] 5.1 Implement predict.mvgam() dispatcher
  - [ ] 5.2 Add summary statistics computation to predict.mvgam()
  - [ ] 5.3 Implement fitted.mvgam() wrapper
  - [ ] 5.4 Add ndraws and draw_ids handling
  - [ ] 5.5 Implement custom quantile computation
  - [ ] 5.6 Add proper S3 method registration
  - [ ] 5.7 Write tests for predict.mvgam() with all type options
  - [ ] 5.8 Write tests for fitted.mvgam()

- [ ] **6.0 marginaleffects Integration**
  - [ ] 6.1 Research required S3 methods from marginaleffects docs
  - [ ] 6.2 Implement get_predict.mvgam() method
  - [ ] 6.3 Implement model.frame.mvgam() method
  - [ ] 6.4 Implement vcov.mvgam() method (if needed)
  - [ ] 6.5 Implement get_coef.mvgam() method
  - [ ] 6.6 Implement family.mvgam() enhancement for marginaleffects
  - [ ] 6.7 Test marginaleffects::predictions() with mvgam
  - [ ] 6.8 Test marginaleffects::slopes() with mvgam
  - [ ] 6.9 Test marginaleffects::comparisons() with mvgam
  - [ ] 6.10 Test marginaleffects::hypotheses() with mvgam
  - [ ] 6.11 Add process_error argument integration
  - [ ] 6.12 Handle series variable in marginal effects

- [ ] **7.0 Enhanced conditional_effects()**
  - [ ] 7.1 Review current conditional_effects.mvgam() implementation
  - [ ] 7.2 Add series argument for series-specific effects
  - [ ] 7.3 Add process_error argument integration
  - [ ] 7.4 Maintain backward compatibility with existing code
  - [ ] 7.5 Handle trend component contributions correctly
  - [ ] 7.6 Write tests for series-specific conditional effects
  - [ ] 7.7 Test backward compatibility with existing plots
  - [ ] 7.8 Add examples to documentation

- [ ] **8.0 Testing and Validation**
  - [ ] 8.1 Write brms comparison tests (trend_formula = NULL)
  - [ ] 8.2 Test with mvgam_example1 (univariate RW)
  - [ ] 8.3 Test with mvgam_example2 (trend_map)
  - [ ] 8.4 Test with mvgam_example3 (multivariate VAR)
  - [ ] 8.5 Test with mvgam_example4 (GP factor model)
  - [ ] 8.6 Test process_error toggle (TRUE vs FALSE)
  - [ ] 8.7 Test ndraws subsampling
  - [ ] 8.8 Test random effects handling (re_formula)
  - [ ] 8.9 Test new random effect levels
  - [ ] 8.10 Test edge cases (intercept-only, trend-only, no trends)
  - [ ] 8.11 Test distributional models
  - [ ] 8.12 Test CAR trends with irregular time
  - [ ] 8.13 Test hierarchical trends (gr/subgr)
  - [ ] 8.14 Test factor models (n_lv)
  - [ ] 8.15 Create performance benchmarks

- [ ] **9.0 Documentation and Polish**
  - [ ] 9.1 Write roxygen2 docs for posterior_linpred.mvgam()
  - [ ] 9.2 Write roxygen2 docs for posterior_epred.mvgam()
  - [ ] 9.3 Write roxygen2 docs for posterior_predict.mvgam()
  - [ ] 9.4 Write roxygen2 docs for posterior_smooths.mvgam()
  - [ ] 9.5 Write roxygen2 docs for posterior_average.mvgam()
  - [ ] 9.6 Write roxygen2 docs for predict.mvgam()
  - [ ] 9.7 Write roxygen2 docs for fitted.mvgam()
  - [ ] 9.8 Create "Prediction and Forecasting" vignette outline
  - [ ] 9.9 Write vignette section: Introduction and basic workflow
  - [ ] 9.10 Write vignette section: Three prediction scales
  - [ ] 9.11 Write vignette section: Process error and uncertainty
  - [ ] 9.12 Write vignette section: Random effects handling
  - [ ] 9.13 Write vignette section: Multivariate models
  - [ ] 9.14 Create "marginaleffects Integration" vignette outline
  - [ ] 9.15 Write marginaleffects vignette content
  - [ ] 9.16 Add prediction examples to README
  - [ ] 9.17 Run R CMD check and fix any issues
  - [ ] 9.18 Final review and cleanup

---

## Detailed Sub-Tasks

### 1.0 Foundation Infrastructure

#### 1.1 Add obs_model and trend_model storage to mvgam objects
**File**: `R/mvgam_core.R`
**Location**: `create_mvgam_from_combined_fit()` function at line ~280
**Objective**: Store lightweight brmsfit objects for prediction use

**Steps**:
1. Open `R/mvgam_core.R` and locate `create_mvgam_from_combined_fit()` at line 250
2. Find the section where mvgam object components are assigned (around line 266-298)
3. After line 280 (around where `trend_metadata` is assigned), add:
   ```r
   obs_model = obs_setup$brmsfit,
   trend_model = if (!is.null(trend_setup)) trend_setup$brmsfit else NULL,
   ```
4. Save the file

**Validation**:
- Fit a simple test model and verify `fit$obs_model` exists and is class "brmsfit"
- Check `fit$trend_model` exists for models with trends, NULL otherwise
- Run existing tests to ensure no breakage: `devtools::test()`

**Time estimate**: 5 minutes

---

#### 1.2 Create S3 mock stanfit class and as_draws_matrix method
**File**: `R/mock-stanfit.R` (new file)
**Objective**: Create mock stanfit object that returns specific parameter subset

**Steps**:
1. Create new file `R/mock-stanfit.R`
2. Write the constructor function:
   ```r
   #' Create mock stanfit for parameter subsetting
   #'
   #' @param draws_matrix A draws_matrix with subset of parameters
   #' @return S3 object of class "mock_stanfit"
   #' @noRd
   create_mock_stanfit <- function(draws_matrix) {
     checkmate::assert_class(draws_matrix, "draws_matrix")

     structure(
       list(draws_cache = draws_matrix),
       class = c("mock_stanfit", "stanfit")
     )
   }
   ```
3. Write the S3 method:
   ```r
   #' Extract draws from mock stanfit
   #' @export
   as_draws_matrix.mock_stanfit <- function(x, ...) {
     x$draws_cache
   }
   ```
4. Add roxygen2 tag for export
5. Save the file

**Validation**:
- Create a test draws_matrix
- Create mock_stanfit and verify `as_draws_matrix()` returns original draws
- Check class inheritance with `inherits(mock, "stanfit")`

**Time estimate**: 10 minutes

---

#### 1.3 Implement observation parameter identification function
**File**: `R/predictions.R` (new file)
**Objective**: Identify observation model parameters from combined fit

**Steps**:
1. Create new file `R/predictions.R`
2. Write the function using patterns from implementation strategy:
   ```r
   #' Identify observation model parameters
   #' @param param_names Character vector of all parameter names
   #' @return Character vector of observation parameter names
   #' @noRd
   identify_obs_params <- function(param_names) {

     # Observation parameter patterns (from brms conventions)
     obs_patterns <- c(
       "^b_",           # Fixed effects: b_Intercept, b_x, b_x:z
       "^bs_",          # Spline coefficients: bs_sx_1, bs_sx_2
       "^bcs_",         # Categorical spline
       "^bmo_",         # Monotonic effects
       "^bme_",         # Measurement error
       "^bsp_",         # Special terms
       "^r_",           # Random effects: r_group[1,Intercept]
       "^sd_",          # Random effect SDs: sd_group__Intercept
       "^cor_",         # Correlations
       "^L_",           # Cholesky factors (correlation matrices)
       "^sigma$",       # Residual SD
       "^sigma_",       # Distributional sigma (sigma_x)
       "^shape",        # Negative binomial, beta
       "^nu",           # Student-t df
       "^phi",          # Beta, zero-inflated
       "^zi_",          # Zero-inflation parameters
       "^hu_"           # Hurdle parameters
     )

     # Trend parameter patterns (EXCLUDE these from obs params)
     trend_patterns <- c(
       "_trend$",       # Parameters ending in _trend
       "^ar\\d+_trend", # AR coefficients: ar1_trend, ar2_trend
       "^ma\\d+_trend", # MA coefficients
       "^A\\d+_trend",  # VAR coefficient matrices
       "^Z\\[",         # Factor loadings: Z[1,1], Z[1,2]
       "^lv_trend",     # Latent trends: lv_trend[1,1]
       "^mu_trend"      # Trend intercepts
     )

     # Match observation patterns, exclude trend patterns
     obs_params <- param_names[
       grepl(paste(obs_patterns, collapse = "|"), param_names) &
       !grepl(paste(trend_patterns, collapse = "|"), param_names)
     ]

     # Always include lp__ for compatibility
     if ("lp__" %in% param_names && !"lp__" %in% obs_params) {
       obs_params <- c(obs_params, "lp__")
     }

     obs_params
   }
   ```
3. Add checkmate validation for input
4. Save the file

**Validation**: Create test with known parameter names and verify correct classification

**Time estimate**: 12 minutes

---

#### 1.4 Implement trend parameter identification function
**File**: `R/predictions.R`
**Objective**: Identify trend model parameters from combined fit

**Steps**:
1. Open `R/predictions.R`
2. Add the trend identification function:
   ```r
   #' Identify trend model parameters
   #' @param param_names Character vector of all parameter names
   #' @return Character vector of trend parameter names
   #' @noRd
   identify_trend_params <- function(param_names) {

     # Trend parameter patterns (based on mvgam Stan code generation)
     trend_patterns <- c(
       "_trend$",         # Parameters ending in _trend
       "^ar\\d+_trend",   # AR coefficients: ar1_trend, ar2_trend
       "^ma\\d+_trend",   # MA coefficients
       "^A\\d+_trend",    # VAR coefficient matrices
       "^Z\\[",           # Factor loadings: Z[1,1], Z[1,2]
       "^Z_raw",          # Raw factor loadings
       "^lv_trend",       # Latent trends
       "^lv_coefs",       # Latent variable coefficients
       "^mu_trend",       # Trend intercepts/means
       "^sigma_trend",    # Innovation standard deviations
       "^L_Omega_trend",  # Cholesky factors for correlations
       "^Sigma_trend",    # Covariance matrices
       "^innovations_trend", # Raw innovations
       "^scaled_innovations_trend" # Scaled innovations
     )

     trend_params <- param_names[
       grepl(paste(trend_patterns, collapse = "|"), param_names)
     ]

     # Always include lp__ for compatibility
     if ("lp__" %in% param_names && !"lp__" %in% trend_params) {
       trend_params <- c(trend_params, "lp__")
     }

     trend_params
   }
   ```
3. Save the file

**Validation**: Test with known multivariate parameter names including response suffixes

**Time estimate**: 10 minutes

---

#### 1.5 Write unit tests for parameter identification
**File**: `tests/testthat/test-parameter-identification.R` (new file)
**Objective**: Validate parameter identification functions

**Steps**:
1. Create new file `tests/testthat/test-parameter-identification.R`
2. Write tests:
   ```r
   test_that("identify_obs_params separates observation parameters", {
     params <- c(
       "b_Intercept", "b_x", "b_z", "sigma",
       "ar1_trend", "Z[1,1]", "lv_trend[1,1]",
       "sd_group__Intercept", "r_group[1,Intercept]",
       "lp__"
     )

     obs <- identify_obs_params(params)

     # Should include observation params
     expect_true("b_Intercept" %in% obs)
     expect_true("b_x" %in% obs)
     expect_true("sigma" %in% obs)
     expect_true("sd_group__Intercept" %in% obs)
     expect_true("lp__" %in% obs)

     # Should NOT include trend params
     expect_false("ar1_trend" %in% obs)
     expect_false("Z[1,1]" %in% obs)
     expect_false("lv_trend[1,1]" %in% obs)
   })

   test_that("identify_trend_params separates trend parameters", {
     params <- c(
       "b_Intercept", "b_x", "sigma",
       "ar1_trend", "sigma_trend", "Z[1,1]", "Z[1,2]",
       "lv_trend[1,1]", "mu_trend[1]", "lp__"
     )

     trend <- identify_trend_params(params)

     # Should include trend params
     expect_true("ar1_trend" %in% trend)
     expect_true("sigma_trend" %in% trend)
     expect_true("Z[1,1]" %in% trend)
     expect_true("lv_trend[1,1]" %in% trend)
     expect_true("mu_trend[1]" %in% trend)
     expect_true("lp__" %in% trend)

     # Should NOT include observation params
     expect_false("b_Intercept" %in% trend)
     expect_false("b_x" %in% trend)
     expect_false("sigma" %in% trend)
   })

   test_that("parameter identification handles multivariate models", {
     params <- c(
       "b_Intercept_count", "b_x_count", "sigma_count",
       "b_Intercept_biomass", "b_x_biomass", "sigma_biomass",
       "ar1_trend_count", "sigma_trend_count",
       "ar1_trend_biomass", "sigma_trend_biomass",
       "lp__"
     )

     obs <- identify_obs_params(params)
     trend <- identify_trend_params(params)

     # Observation params should include response-specific names
     expect_true("b_Intercept_count" %in% obs)
     expect_true("sigma_biomass" %in% obs)

     # Trend params should include response-specific trend names
     expect_true("ar1_trend_count" %in% trend)
     expect_true("sigma_trend_biomass" %in% trend)

     # No overlap except lp__
     overlap <- intersect(setdiff(obs, "lp__"), setdiff(trend, "lp__"))
     expect_length(overlap, 0)
   })
   ```
3. Run tests: `devtools::load_all(); testthat::test_file("tests/testthat/test-parameter-identification.R")`

**Validation**: All tests pass

**Time estimate**: 15 minutes

---

#### 1.6 Write unit tests for mock stanfit pattern
**File**: `tests/testthat/test-mock-stanfit.R` (new file)
**Objective**: Validate mock stanfit S3 pattern

**Steps**:
1. Create new file `tests/testthat/test-mock-stanfit.R`
2. Write tests:
   ```r
   test_that("create_mock_stanfit creates correct structure", {
     # Create test draws_matrix
     draws <- matrix(rnorm(100), nrow = 50, ncol = 2)
     colnames(draws) <- c("b_x", "sigma")
     class(draws) <- c("draws_matrix", "draws", "matrix", "array")

     # Create mock
     mock <- create_mock_stanfit(draws)

     # Check structure
     expect_s3_class(mock, "mock_stanfit")
     expect_s3_class(mock, "stanfit")
     expect_true(inherits(mock, "stanfit"))
     expect_true(!is.null(mock$draws_cache))
   })

   test_that("as_draws_matrix.mock_stanfit returns cached draws", {
     draws <- matrix(rnorm(100), nrow = 50, ncol = 2)
     colnames(draws) <- c("b_Intercept", "b_x")
     class(draws) <- c("draws_matrix", "draws", "matrix", "array")

     mock <- create_mock_stanfit(draws)
     result <- as_draws_matrix(mock)

     # Should return identical draws
     expect_identical(result, draws)
     expect_equal(dim(result), c(50, 2))
     expect_equal(colnames(result), c("b_Intercept", "b_x"))
   })

   test_that("mock stanfit validates input", {
     # Should error on non-draws_matrix input
     expect_error(
       create_mock_stanfit(matrix(1:10)),
       "draws_matrix"
     )
   })
   ```
3. Run tests

**Validation**: All tests pass

**Time estimate**: 12 minutes

---

### 2.0 Core Linear Predictor Computation

#### 2.1 Create extract_linpred_from_prep() for observation model
**File**: `R/predictions.R`
**Objective**: Extract linear predictor from brms prep object

**Steps**:
1. Open `R/predictions.R`
2. Add the function (based on verified implementation strategy):
   ```r
   #' Extract linear predictor from prep object
   #'
   #' Manually reconstructs linear predictor from bprepdict object.
   #' Verified to match brms::posterior_linpred() to numerical precision.
   #'
   #' @param prep bprepdict object from brms:::prepare_predictions()
   #' @param draws draws_matrix with parameters
   #' @return matrix [n_draws × n_obs] of linear predictor values
   #' @noRd
   extract_linpred_from_prep <- function(prep, draws) {

     checkmate::assert_class(prep, "bprepdict")
     checkmate::assert_class(draws, "draws_matrix")

     n_draws <- nrow(draws)
     n_obs <- nrow(prep$dpars$mu$fe$X)

     # Initialize with zeros
     linpred <- matrix(0, nrow = n_draws, ncol = n_obs)

     # Add intercept
     if (prep$dpars$mu$int) {
       if (!"b_Intercept" %in% colnames(draws)) {
         insight::format_error(
           "Intercept required but {.field b_Intercept} not found in draws."
         )
       }
       linpred <- linpred + draws[, "b_Intercept"]
     }

     # Add fixed effects
     if (!is.null(prep$dpars$mu$fe)) {
       X <- prep$dpars$mu$fe$X
       fe_params <- prep$dpars$mu$fe$vars

       if (length(fe_params) > 0) {
         b_fe <- draws[, fe_params, drop = FALSE]
         linpred <- linpred + b_fe %*% t(X)
       }
     }

     # Add smooth terms
     if (!is.null(prep$dpars$mu$sm)) {
       for (i in seq_along(prep$dpars$mu$sm)) {
         Xs <- prep$dpars$mu$sm[[i]]$Xs
         s_params <- prep$dpars$mu$sm[[i]]$vars
         b_s <- draws[, s_params, drop = FALSE]
         linpred <- linpred + b_s %*% t(Xs)
       }
     }

     # Add random effects
     if (!is.null(prep$dpars$mu$re)) {
       for (i in seq_along(prep$dpars$mu$re)) {
         Z <- prep$dpars$mu$re[[i]]$Z
         r_params <- prep$dpars$mu$re[[i]]$vars
         b_r <- draws[, r_params, drop = FALSE]
         linpred <- linpred + b_r %*% t(Z)
       }
     }

     return(linpred)
   }
   ```
3. Save file

**Validation**: Test with simple brms model, compare to brms::posterior_linpred()

**Time estimate**: 15 minutes

---

#### 2.2 Create extract_linpred_from_prep() for trend model
**File**: `R/predictions.R`
**Objective**: Handle trend-specific prep extraction (same as obs, but document separately)

**Steps**:
1. Review if trend models need different handling
2. For now, trend models use same `extract_linpred_from_prep()` function
3. Add note in documentation that trend linear predictors are on Gaussian scale
4. Consider adding wrapper if trend-specific logic needed later

**Note**: Trend models are always Gaussian (architectural constraint), so same extraction logic applies.

**Validation**: Verify trend prep objects work with existing function

**Time estimate**: 5 minutes (documentation only)

---

#### 2.3 Implement parameter subset extraction from combined fit
**File**: `R/predictions.R`
**Objective**: Extract observation or trend parameters with proper subsetting

**Steps**:
1. Open `R/predictions.R`
2. Add helper function:
   ```r
   #' Extract parameter subset from combined mvgam fit
   #'
   #' @param mvgam_fit mvgam object with combined stanfit
   #' @param component Character: "obs" or "trend"
   #' @param ndraws Integer: number of draws to sample (NULL = all)
   #' @return draws_matrix with subset of parameters
   #' @noRd
   extract_parameter_subset <- function(mvgam_fit,
                                        component = c("obs", "trend"),
                                        ndraws = NULL) {

     component <- match.arg(component)
     checkmate::assert_class(mvgam_fit, "mvgam")
     checkmate::assert_count(ndraws, positive = TRUE, null.ok = TRUE)

     # Get all parameter names
     all_params <- posterior::variables(mvgam_fit$fit)

     # Identify subset
     param_subset <- if (component == "obs") {
       identify_obs_params(all_params)
     } else {
       identify_trend_params(all_params)
     }

     # Extract draws
     draws <- posterior::as_draws_matrix(
       mvgam_fit$fit,
       variable = param_subset
     )

     # Subsample if requested
     if (!is.null(ndraws)) {
       if (ndraws > nrow(draws)) {
         insight::format_error(
           "{.field ndraws} ({ndraws}) exceeds available samples ({nrow(draws)})."
         )
       }
       draws <- posterior::subset_draws(draws, draw = sample(nrow(draws), ndraws))
     }

     draws
   }
   ```
3. Save file

**Validation**: Test extraction with fitted mvgam model

**Time estimate**: 12 minutes

---

#### 2.4 Create prep object preparation helper
**File**: `R/predictions.R`
**Objective**: Prepare brms prediction objects with mock stanfit

**Steps**:
1. Open `R/predictions.R`
2. Add function:
   ```r
   #' Prepare brms prediction object with parameter subset
   #'
   #' @param mvgam_fit mvgam object
   #' @param newdata data.frame for predictions
   #' @param component Character: "obs" or "trend"
   #' @param ndraws Integer: number of draws
   #' @param re_formula Formula for random effects
   #' @param allow_new_levels Logical
   #' @param sample_new_levels Character
   #' @return bprepdict object from brms:::prepare_predictions()
   #' @noRd
   prepare_mvgam_predictions <- function(mvgam_fit,
                                         newdata,
                                         component = c("obs", "trend"),
                                         ndraws = NULL,
                                         re_formula = NULL,
                                         allow_new_levels = FALSE,
                                         sample_new_levels = "uncertainty") {

     component <- match.arg(component)

     # Get appropriate brmsfit object
     brmsfit_obj <- if (component == "obs") {
       mvgam_fit$obs_model
     } else {
       mvgam_fit$trend_model
     }

     if (is.null(brmsfit_obj)) {
       insight::format_error(
         "No {component} model found in mvgam object.",
         "This may be an older mvgam object. Refit the model."
       )
     }

     # Extract parameter subset
     draws_subset <- extract_parameter_subset(
       mvgam_fit,
       component = component,
       ndraws = ndraws
     )

     # Create mock stanfit
     mock_fit <- create_mock_stanfit(draws_subset)

     # Replace stanfit in brmsfit
     brmsfit_obj$fit <- mock_fit

     # Call brms prepare_predictions
     prep <- brms:::prepare_predictions(
       object = brmsfit_obj,
       newdata = newdata,
       re_formula = re_formula,
       allow_new_levels = allow_new_levels,
       sample_new_levels = sample_new_levels
     )

     return(prep)
   }
   ```
3. Save file

**Validation**: Test with simple model and newdata

**Time estimate**: 15 minutes

---

#### 2.5 Write unit tests for linpred extraction (compare to brms)
**File**: `tests/testthat/test-prep-extraction.R` (new file)
**Objective**: Validate linpred extraction matches brms exactly

**Steps**:
1. Create `tests/testthat/test-prep-extraction.R`
2. Write comparison test:
   ```r
   test_that("extract_linpred_from_prep matches brms::posterior_linpred", {
     skip_on_cran()
     skip_if_not_installed("brms")

     # Fit simple brms model
     data <- data.frame(
       y = rnorm(30),
       x = rnorm(30)
     )

     fit <- SW(SM(brms::brm(
       y ~ x,
       data = data,
       chains = 2,
       iter = 500,
       silent = 2,
       refresh = 0
     )))

     newdata <- data.frame(x = c(-1, 0, 1))

     # Get prep object
     prep <- brms:::prepare_predictions(fit, newdata = newdata)
     draws <- posterior::as_draws_matrix(fit$fit)

     # Manual computation
     manual <- extract_linpred_from_prep(prep, draws)

     # brms computation
     brms_result <- brms::posterior_linpred(fit, newdata = newdata)

     # Should match to numerical precision
     expect_equal(manual, brms_result, tolerance = 1e-10)
     expect_equal(dim(manual), dim(brms_result))
   })
   ```
3. Run test

**Validation**: Test passes with tolerance < 1e-10

**Time estimate**: 15 minutes

---

#### 2.6 Implement posterior_linpred.mvgam() with all parameters
**File**: `R/posterior-linpred.R` (new file)
**Objective**: Core posterior_linpred S3 method

**Steps**:
1. Create `R/posterior-linpred.R`
2. Implement function (see detailed code below - too long for this summary)
3. Include all parameters: transform, process_error, ndraws, draw_ids, re_formula, etc.
4. Handle both univariate and multivariate models
5. Add comprehensive input validation using checkmate

**Key components**:
- Input validation
- Observation prep + linpred extraction
- Trend prep + linpred extraction (if trend_formula present)
- Combination on link scale
- Optional transform to response scale

**Validation**: Test with simple univariate model

**Time estimate**: 15 minutes (initial implementation, will refine in testing)

---

#### 2.7 Add input validation for posterior_linpred.mvgam()
**File**: `R/posterior-linpred.R`
**Objective**: Comprehensive validation of all arguments

**Steps**:
1. Add validation helper:
   ```r
   #' Validate prediction inputs
   #' @noRd
   validate_prediction_inputs <- function(object, newdata, ndraws, draw_ids,
                                          process_error, allow_new_levels,
                                          sample_new_levels) {

     checkmate::assert_class(object, "mvgam")
     checkmate::assert_data_frame(newdata, null.ok = TRUE)
     checkmate::assert_count(ndraws, positive = TRUE, null.ok = TRUE)
     checkmate::assert_integerish(draw_ids, null.ok = TRUE)
     checkmate::assert_logical(process_error, len = 1)
     checkmate::assert_logical(allow_new_levels, len = 1)
     checkmate::assert_choice(sample_new_levels, c("uncertainty", "gaussian"))

     # Check ndraws vs draw_ids conflict
     if (!is.null(ndraws) && !is.null(draw_ids)) {
       insight::format_error(
         "Cannot specify both {.field ndraws} and {.field draw_ids}.",
         "Use {.field ndraws} for random subsampling or {.field draw_ids} for specific draws."
       )
     }

     # TODO: Add required variable checking

     invisible(TRUE)
   }
   ```
2. Call at start of posterior_linpred.mvgam()

**Validation**: Test error messages trigger correctly

**Time estimate**: 12 minutes

---

#### 2.8 Write integration tests for posterior_linpred.mvgam()
**File**: `tests/testthat/test-posterior-linpred.R` (new file)
**Objective**: End-to-end tests with fitted mvgam models

**Steps**:
1. Create test file
2. Write tests using pre-fitted models:
   ```r
   test_that("posterior_linpred works with mvgam_example1", {
     skip_on_cran()

     fit <- mvgam_example1
     newdata <- mvgam_examp_dat$data_test

     pred <- posterior_linpred(fit, newdata = newdata)

     # Check dimensions
     expect_true(is.matrix(pred))
     expect_equal(ncol(pred), nrow(newdata))
     expect_true(all(is.finite(pred)))
   })

   test_that("posterior_linpred respects ndraws", {
     fit <- mvgam_example1
     newdata <- mvgam_examp_dat$data_test

     pred_100 <- posterior_linpred(fit, newdata = newdata, ndraws = 100)

     expect_equal(nrow(pred_100), 100)
   })
   ```
3. Run tests

**Validation**: Tests pass

**Time estimate**: 15 minutes

---

### 3.0 Scale Transformations and Prediction Types

#### 3.1 Implement inverse link transformation helper
**File**: `R/predictions.R`
**Objective**: Apply family-specific inverse link functions

**Steps**:
1. Open `R/predictions.R`
2. Add helper:
   ```r
   #' Apply inverse link function
   #'
   #' @param linpred Matrix of linear predictor values
   #' @param family brmsfamily object
   #' @return Matrix of values on response scale
   #' @noRd
   apply_inverse_link <- function(linpred, family) {

     checkmate::assert_matrix(linpred)

     # Get inverse link function from family
     inv_link <- family$linkinv

     if (is.null(inv_link)) {
       insight::format_error(
         "Family {.field {family$family}} has no inverse link function."
       )
     }

     # Apply to entire matrix
     inv_link(linpred)
   }
   ```
3. Save file

**Validation**: Test with different families (gaussian, poisson, binomial)

**Time estimate**: 8 minutes

---

#### 3.2 Implement posterior_epred.mvgam() using posterior_linpred
**File**: `R/posterior-epred.R` (new file)
**Objective**: Expected value predictions with inverse link

**Steps**:
1. Create `R/posterior-epred.R`
2. Implement:
   ```r
   #' Posterior Expected Values for mvgam Models
   #'
   #' @param object mvgam object
   #' @param newdata data.frame with covariates
   #' @param process_error Logical, include trend uncertainty
   #' @param ndraws Integer, number of draws
   #' @param draw_ids Integer vector of specific draws
   #' @param re_formula Formula for random effects
   #' @param allow_new_levels Logical
   #' @param sample_new_levels Character
   #' @param resp Character, response for multivariate
   #' @param ... Additional arguments
   #' @return Matrix of expected values
   #' @export
   posterior_epred.mvgam <- function(object,
                                     newdata = NULL,
                                     process_error = TRUE,
                                     ndraws = NULL,
                                     draw_ids = NULL,
                                     re_formula = NULL,
                                     allow_new_levels = FALSE,
                                     sample_new_levels = "uncertainty",
                                     resp = NULL,
                                     ...) {

     # Get linear predictor
     linpred <- posterior_linpred(
       object = object,
       newdata = newdata,
       transform = FALSE,  # Keep on link scale
       process_error = process_error,
       ndraws = ndraws,
       draw_ids = draw_ids,
       re_formula = re_formula,
       allow_new_levels = allow_new_levels,
       sample_new_levels = sample_new_levels,
       resp = resp,
       ...
     )

     # Apply inverse link
     epred <- apply_inverse_link(linpred, object$family)

     return(epred)
   }
   ```
3. Add roxygen2 documentation
4. Save file

**Validation**: Test epred = inv_link(linpred) for Poisson

**Time estimate**: 12 minutes

---

#### 3.3 Create family-specific observation noise sampling functions
**File**: `R/predictions.R`
**Objective**: Sample from family-specific distributions

**Steps**:
1. Open `R/predictions.R`
2. Add sampling dispatcher:
   ```r
   #' Add observation noise for posterior predictive
   #'
   #' @param epred Matrix of expected values
   #' @param family brmsfamily object
   #' @param dpars List of distributional parameters (sigma, shape, etc.)
   #' @return Matrix of sampled values
   #' @noRd
   add_observation_noise <- function(epred, family, dpars = list()) {

     family_name <- family$family

     # Dispatch to family-specific sampler
     ppred <- switch(
       family_name,

       "gaussian" = {
         if (is.null(dpars$sigma)) {
           insight::format_error("Gaussian family requires {.field sigma} parameter.")
         }
         sigma <- dpars$sigma
         epred + matrix(
           rnorm(length(epred), 0, rep(sigma, ncol(epred))),
           nrow = nrow(epred)
         )
       },

       "poisson" = {
         matrix(
           rpois(length(epred), lambda = as.vector(epred)),
           nrow = nrow(epred)
         )
       },

       "binomial" = {
         trials <- dpars$trials %||% 1
         matrix(
           rbinom(length(epred), size = trials, prob = as.vector(epred)),
           nrow = nrow(epred)
         )
       },

       "bernoulli" = {
         matrix(
           rbinom(length(epred), size = 1, prob = as.vector(epred)),
           nrow = nrow(epred)
         )
       },

       "negative_binomial" = {
         if (is.null(dpars$shape)) {
           insight::format_error("Negative binomial requires {.field shape} parameter.")
         }
         shape <- dpars$shape
         matrix(
           rnbinom(
             length(epred),
             mu = as.vector(epred),
             size = rep(shape, ncol(epred))
           ),
           nrow = nrow(epred)
         )
       },

       # Add more families as needed

       insight::format_error("Family {.field {family_name}} not yet supported for posterior_predict.")
     )

     return(ppred)
   }
   ```
3. Save file

**Validation**: Test sampling produces correct types (integers for discrete)

**Time estimate**: 15 minutes

---

#### 3.4 Implement posterior_predict.mvgam() with noise addition
**File**: `R/posterior-predict.R` (new file)
**Objective**: Posterior predictive samples with observation variability

**Steps**:
1. Create `R/posterior-predict.R`
2. Implement function:
   ```r
   #' Posterior Predictive Samples for mvgam Models
   #'
   #' @inheritParams posterior_epred.mvgam
   #' @return Matrix of posterior predictive samples
   #' @export
   posterior_predict.mvgam <- function(object,
                                       newdata = NULL,
                                       process_error = TRUE,
                                       ndraws = NULL,
                                       draw_ids = NULL,
                                       re_formula = NULL,
                                       allow_new_levels = FALSE,
                                       sample_new_levels = "uncertainty",
                                       resp = NULL,
                                       ...) {

     # Get expected values
     epred <- posterior_epred(
       object = object,
       newdata = newdata,
       process_error = process_error,
       ndraws = ndraws,
       draw_ids = draw_ids,
       re_formula = re_formula,
       allow_new_levels = allow_new_levels,
       sample_new_levels = sample_new_levels,
       resp = resp,
       ...
     )

     # Extract distributional parameters if needed
     dpars <- extract_distributional_parameters(object, ndraws)

     # Add observation noise
     ppred <- add_observation_noise(epred, object$family, dpars)

     return(ppred)
   }
   ```
3. Add roxygen2 docs
4. Save file

**Validation**: Test that ppred has more variance than epred

**Time estimate**: 12 minutes

---

#### 3.5 Add distributional parameter extraction (sigma, shape, etc.)
**File**: `R/predictions.R`
**Objective**: Extract family-specific parameters from posterior

**Steps**:
1. Open `R/predictions.R`
2. Add function:
   ```r
   #' Extract distributional parameters from mvgam fit
   #'
   #' @param object mvgam object
   #' @param ndraws Number of draws to extract
   #' @return List of distributional parameters
   #' @noRd
   extract_distributional_parameters <- function(object, ndraws = NULL) {

     family_name <- object$family$family
     dpars <- list()

     # Get available parameters
     all_params <- posterior::variables(object$fit)

     # Extract based on family
     if (family_name == "gaussian") {
       if ("sigma" %in% all_params) {
         sigma_draws <- posterior::as_draws_matrix(object$fit, variable = "sigma")
         if (!is.null(ndraws)) {
           sigma_draws <- posterior::subset_draws(sigma_draws, draw = sample(nrow(sigma_draws), ndraws))
         }
         dpars$sigma <- as.vector(sigma_draws)
       }
     }

     if (family_name == "negative_binomial") {
       if ("shape" %in% all_params) {
         shape_draws <- posterior::as_draws_matrix(object$fit, variable = "shape")
         if (!is.null(ndraws)) {
           shape_draws <- posterior::subset_draws(shape_draws, draw = sample(nrow(shape_draws), ndraws))
         }
         dpars$shape <- as.vector(shape_draws)
       }
     }

     # Add more as needed (nu, phi, etc.)

     return(dpars)
   }
   ```
3. Save file

**Validation**: Test extraction with Gaussian and negative binomial models

**Time estimate**: 12 minutes

---

#### 3.6 Write tests for scale transformations (link → epred → predict)
**File**: `tests/testthat/test-posterior-epred.R` (new file)
**Objective**: Validate transformation pipeline

**Steps**:
1. Create test file
2. Write transformation tests:
   ```r
   test_that("posterior_epred applies inverse link correctly", {
     skip_on_cran()

     # Use Poisson example (log link)
     fit <- mvgam_example1  # Assuming this is Poisson
     newdata <- mvgam_examp_dat$data_test

     linpred <- posterior_linpred(fit, newdata = newdata)
     epred <- posterior_epred(fit, newdata = newdata)

     # For Poisson: epred should equal exp(linpred)
     expect_equal(epred, exp(linpred), tolerance = 1e-10)
   })

   test_that("posterior_predict adds observation noise", {
     fit <- mvgam_example1
     newdata <- mvgam_examp_dat$data_test

     epred <- posterior_epred(fit, newdata = newdata)
     ppred <- posterior_predict(fit, newdata = newdata)

     # Predictive should have more variance
     expect_true(sd(ppred) >= sd(epred))

     # For Poisson: predictions should be integers
     expect_true(all(ppred == floor(ppred)))
   })
   ```
3. Run tests

**Validation**: Tests pass

**Time estimate**: 12 minutes

---

#### 3.7 Test posterior_predict with multiple families
**File**: `tests/testthat/test-posterior-predict.R` (new file)
**Objective**: Validate family-specific sampling

**Steps**:
1. Create test file
2. Test multiple families (will need to fit or use different examples):
   ```r
   test_that("posterior_predict works with Poisson family", {
     # Test with Poisson model
     # Verify integer outputs
   })

   test_that("posterior_predict works with Gaussian family", {
     # Test with Gaussian model
     # Verify continuous outputs
   })

   test_that("posterior_predict works with Bernoulli family", {
     # Test with Bernoulli model
     # Verify 0/1 outputs
   })
   ```
3. Run tests

**Validation**: All family-specific tests pass

**Time estimate**: 15 minutes

---

#### 3.8 Verify integer outputs for discrete families
**File**: `tests/testthat/test-posterior-predict.R`
**Objective**: Ensure discrete families produce integer samples

**Steps**:
1. Add to existing test file:
   ```r
   test_that("discrete families produce integer samples", {
     # Poisson
     fit_pois <- mvgam_example1  # Assuming Poisson
     pred_pois <- posterior_predict(fit_pois, newdata = mvgam_examp_dat$data_test)
     expect_true(all(pred_pois == floor(pred_pois)))

     # Binomial (if we have example)
     # fit_binom <- ...
     # pred_binom <- posterior_predict(fit_binom, ...)
     # expect_true(all(pred_binom == floor(pred_binom)))
   })
   ```
2. Run test

**Validation**: Test passes

**Time estimate**: 8 minutes

---

### 4.0 Specialized Prediction Functions

#### 4.1 Implement smooth term extraction from prep objects
**File**: `R/predictions.R`
**Objective**: Extract individual smooth contributions

**Steps**:
1. Open `R/predictions.R`
2. Add function:
   ```r
   #' Extract smooth term contribution from prep object
   #'
   #' @param prep bprepdict object
   #' @param draws draws_matrix with parameters
   #' @param smooth_index Integer index of smooth to extract
   #' @return Matrix of smooth contributions
   #' @noRd
   extract_smooth_from_prep <- function(prep, draws, smooth_index) {

     if (is.null(prep$dpars$mu$sm)) {
       insight::format_error("No smooth terms found in model.")
     }

     if (smooth_index > length(prep$dpars$mu$sm)) {
       insight::format_error(
         "Smooth index {smooth_index} exceeds number of smooths ({length(prep$dpars$mu$sm)})."
       )
     }

     # Extract smooth basis and parameters
     Xs <- prep$dpars$mu$sm[[smooth_index]]$Xs
     s_params <- prep$dpars$mu$sm[[smooth_index]]$vars
     b_s <- draws[, s_params, drop = FALSE]

     # Compute contribution
     smooth_contrib <- b_s %*% t(Xs)

     return(smooth_contrib)
   }
   ```
3. Save file

**Validation**: Test with model containing smooths

**Time estimate**: 10 minutes

---

#### 4.2 Implement posterior_smooths.mvgam() for single smooth
**File**: `R/posterior-smooths.R` (new file)
**Objective**: Extract specific smooth term

**Steps**:
1. Create `R/posterior-smooths.R`
2. Implement function:
   ```r
   #' Extract Posterior Smooth Term Contributions
   #'
   #' @param object mvgam object
   #' @param smooth Character name of smooth term (e.g., "s(x)")
   #' @param newdata data.frame (optional)
   #' @param ndraws Integer
   #' @param ... Additional arguments
   #' @return Matrix of smooth contributions
   #' @export
   posterior_smooths.mvgam <- function(object,
                                       smooth = NULL,
                                       newdata = NULL,
                                       ndraws = NULL,
                                       ...) {

     checkmate::assert_class(object, "mvgam")
     checkmate::assert_character(smooth, len = 1, null.ok = TRUE)

     # Get prep object
     prep <- prepare_mvgam_predictions(
       object,
       newdata = newdata %||% object$data,
       component = "obs",
       ndraws = ndraws
     )

     # Extract draws
     draws <- extract_parameter_subset(object, "obs", ndraws)

     # Get all smooth names
     if (is.null(prep$dpars$mu$sm)) {
       insight::format_error("Model contains no smooth terms.")
     }

     smooth_names <- sapply(prep$dpars$mu$sm, function(x) x$label)

     if (is.null(smooth)) {
       # Return all smooths
       result <- lapply(seq_along(prep$dpars$mu$sm), function(i) {
         extract_smooth_from_prep(prep, draws, i)
       })
       names(result) <- smooth_names
       return(result)
     } else {
       # Return specific smooth
       smooth_idx <- which(smooth_names == smooth)
       if (length(smooth_idx) == 0) {
         insight::format_error(
           "Smooth {.field {smooth}} not found.",
           "Available smooths: {paste(smooth_names, collapse = ', ')}"
         )
       }
       return(extract_smooth_from_prep(prep, draws, smooth_idx))
     }
   }
   ```
3. Add roxygen2 docs
4. Save file

**Validation**: Test extraction with mvgam_example1 (has s(season))

**Time estimate**: 15 minutes

---

#### 4.3 Extend posterior_smooths.mvgam() for all smooths
**File**: `R/posterior-smooths.R`
**Objective**: Return list when smooth = NULL

**Note**: Already implemented in 4.2 with NULL check

**Validation**: Test that smooth = NULL returns named list

**Time estimate**: 5 minutes (testing only)

---

#### 4.4 Implement posterior_average.mvgam() for categorical families
**File**: `R/posterior-average.R` (new file)
**Objective**: Category-weighted averages for ordered/categorical outcomes

**Steps**:
1. Create `R/posterior-average.R`
2. Implement function:
   ```r
   #' Posterior Average for Categorical Models
   #'
   #' @inheritParams posterior_epred.mvgam
   #' @return Matrix of category-weighted averages
   #' @export
   posterior_average.mvgam <- function(object,
                                       newdata = NULL,
                                       process_error = TRUE,
                                       ndraws = NULL,
                                       ...) {

     # Check family is categorical/ordered
     if (!object$family$family %in% c("categorical", "cumulative", "sratio", "cratio", "acat")) {
       insight::format_error(
         "posterior_average only applicable to categorical/ordinal families.",
         "Current family: {.field {object$family$family}}"
       )
     }

     # Get category probabilities
     # (This will need special handling - placeholder for now)
     insight::format_error(
       "posterior_average not yet implemented for categorical families.",
       "This is a placeholder for future development."
     )
   }
   ```
3. Save file (implementation TBD based on brms patterns)

**Note**: This is lower priority and may need deeper investigation of brms categorical handling

**Time estimate**: 10 minutes (placeholder only)

---

#### 4.5 Add multivariate model detection and handling
**File**: `R/predictions.R`
**Objective**: Detect and route multivariate predictions

**Steps**:
1. Open `R/predictions.R`
2. Add helper:
   ```r
   #' Check if mvgam object is multivariate
   #' @param object mvgam object
   #' @return Logical
   #' @noRd
   is_multivariate_mvgam <- function(object) {
     !is.null(object$response_names) && length(object$response_names) > 1
   }

   #' Get response names from mvgam object
   #' @param object mvgam object
   #' @return Character vector of response names
   #' @noRd
   get_response_names <- function(object) {
     if (is_multivariate_mvgam(object)) {
       object$response_names
     } else {
       NULL
     }
   }
   ```
3. Save file

**Validation**: Test with mvgam_example3 (multivariate)

**Time estimate**: 8 minutes

---

#### 4.6 Implement list return format for multivariate predictions
**File**: `R/posterior-linpred.R` (modify)
**Objective**: Return named list for multivariate models

**Steps**:
1. Open `R/posterior-linpred.R`
2. Modify posterior_linpred.mvgam to check for multivariate:
   ```r
   # At end of function, before return:
   if (is.null(resp) && is_multivariate_mvgam(object)) {
     # Split into list per response
     response_names <- get_response_names(object)

     result <- lapply(response_names, function(r) {
       posterior_linpred(
         object = object,
         newdata = newdata,
         resp = r,
         ...
       )
     })
     names(result) <- response_names
     return(result)
   }
   ```
3. Save file

**Validation**: Test multivariate prediction returns list

**Time estimate**: 12 minutes

---

#### 4.7 Handle response-specific trends in multivariate models
**File**: `R/predictions.R` (modify prepare_mvgam_predictions)
**Objective**: Extract correct trend parameters for specific response

**Steps**:
1. Modify `prepare_mvgam_predictions()` to handle resp argument
2. Filter trend parameters by response suffix when applicable
3. This requires investigating how response-specific trend parameters are named (from pathfinder findings: `_count`, `_biomass` suffixes)
4. Update parameter identification to handle response suffixes

**Note**: This is complex and ties into findings from pathfinder about multivariate structure

**Validation**: Test with response-specific trend model

**Time estimate**: 15 minutes

---

#### 4.8 Write tests for specialized prediction functions
**File**: `tests/testthat/test-posterior-smooths.R` (new file)
**Objective**: Validate smooth extraction and multivariate handling

**Steps**:
1. Create test file
2. Write tests:
   ```r
   test_that("posterior_smooths extracts single smooth", {
     fit <- mvgam_example1  # Has s(season)

     smooth <- posterior_smooths(fit, smooth = "s(season)")

     expect_true(is.matrix(smooth))
     expect_equal(ncol(smooth), nrow(mvgam_examp_dat$data_train))
   })

   test_that("posterior_smooths returns all smooths as list", {
     fit <- mvgam_example1

     smooths <- posterior_smooths(fit)

     expect_true(is.list(smooths))
     expect_true("s(season)" %in% names(smooths))
   })

   test_that("multivariate predictions return named list", {
     fit <- mvgam_example3  # Multivariate VAR
     newdata <- mvgam_examp_dat$data_test

     pred <- posterior_epred(fit, newdata = newdata)

     expect_true(is.list(pred))
     expect_equal(length(pred), length(fit$response_names))
   })
   ```
3. Run tests

**Validation**: All tests pass

**Time estimate**: 15 minutes

---

### 5.0 Convenience Wrappers

#### 5.1 Implement predict.mvgam() dispatcher
**File**: `R/predict.R` (new file)
**Objective**: S3 predict method with type argument

**Steps**:
1. Create `R/predict.R`
2. Implement:
   ```r
   #' Predictions for mvgam Models
   #'
   #' @param object mvgam object
   #' @param newdata data.frame (optional)
   #' @param type Character: "link", "response", or "prediction"
   #' @param process_error Logical
   #' @param ndraws Integer
   #' @param summary Logical, return summary statistics
   #' @param probs Numeric vector of quantiles
   #' @param ... Additional arguments
   #' @return Matrix or data.frame depending on summary argument
   #' @export
   predict.mvgam <- function(object,
                             newdata = NULL,
                             type = c("response", "link", "prediction"),
                             process_error = TRUE,
                             ndraws = NULL,
                             summary = TRUE,
                             probs = c(0.025, 0.975),
                             ...) {

     type <- match.arg(type)
     checkmate::assert_logical(summary, len = 1)
     checkmate::assert_numeric(probs, lower = 0, upper = 1, min.len = 1)

     # Dispatch to appropriate function
     preds <- switch(
       type,
       "link" = posterior_linpred(
         object,
         newdata = newdata,
         transform = FALSE,
         process_error = process_error,
         ndraws = ndraws,
         ...
       ),
       "response" = posterior_epred(
         object,
         newdata = newdata,
         process_error = process_error,
         ndraws = ndraws,
         ...
       ),
       "prediction" = posterior_predict(
         object,
         newdata = newdata,
         process_error = process_error,
         ndraws = ndraws,
         ...
       )
     )

     # Return draws or summary
     if (!summary) {
       return(preds)
     }

     # Compute summary statistics
     compute_prediction_summary(preds, probs)
   }
   ```
3. Add roxygen2 docs
4. Save file

**Validation**: Test all three type options

**Time estimate**: 15 minutes

---

#### 5.2 Add summary statistics computation to predict.mvgam()
**File**: `R/predict.R`
**Objective**: Compute mean and quantiles

**Steps**:
1. Add helper function:
   ```r
   #' Compute prediction summary statistics
   #'
   #' @param preds Matrix of predictions
   #' @param probs Quantile probabilities
   #' @return data.frame with summary
   #' @noRd
   compute_prediction_summary <- function(preds, probs) {

     # Compute mean
     estimate <- colMeans(preds)

     # Compute quantiles
     quantiles <- apply(preds, 2, quantile, probs = probs)

     # Build data.frame
     result <- data.frame(
       Estimate = estimate
     )

     # Add quantile columns
     for (i in seq_along(probs)) {
       col_name <- paste0("Q", probs[i] * 100)
       result[[col_name]] <- quantiles[i, ]
     }

     result
   }
   ```
2. Save file

**Validation**: Test summary output format

**Time estimate**: 10 minutes

---

#### 5.3 Implement fitted.mvgam() wrapper
**File**: `R/predict.R`
**Objective**: In-sample fitted values

**Steps**:
1. Add to `R/predict.R`:
   ```r
   #' Fitted Values for mvgam Models
   #'
   #' @param object mvgam object
   #' @param scale Character: "response" or "linear"
   #' @param process_error Logical
   #' @param summary Logical
   #' @param ... Additional arguments
   #' @return Matrix or data.frame of fitted values
   #' @export
   fitted.mvgam <- function(object,
                            scale = c("response", "linear"),
                            process_error = TRUE,
                            summary = TRUE,
                            ...) {

     scale <- match.arg(scale)

     # Call predict with original data
     type <- if (scale == "response") "response" else "link"

     predict(
       object,
       newdata = NULL,  # Use training data
       type = type,
       process_error = process_error,
       summary = summary,
       ...
     )
   }
   ```
2. Add roxygen2 docs
3. Save file

**Validation**: Test fitted values match training data predictions

**Time estimate**: 8 minutes

---

#### 5.4 Add ndraws and draw_ids handling
**File**: `R/predict.R` (already handled in posterior_linpred, verify here)
**Objective**: Ensure wrappers pass through arguments correctly

**Steps**:
1. Review predict.mvgam() ensures ndraws is passed through
2. Add draw_ids support if not already present
3. Test both parameters work as expected

**Validation**: Test subsetting works

**Time estimate**: 5 minutes

---

#### 5.5 Implement custom quantile computation
**File**: `R/predict.R`
**Objective**: Allow user-specified quantiles

**Note**: Already implemented in compute_prediction_summary() via probs argument

**Validation**: Test with probs = c(0.1, 0.5, 0.9)

**Time estimate**: 5 minutes (testing only)

---

#### 5.6 Add proper S3 method registration
**File**: `NAMESPACE` (auto-generated)
**Objective**: Ensure all S3 methods exported correctly

**Steps**:
1. Run `devtools::document()` to update NAMESPACE
2. Verify exports:
   - `S3method(predict, mvgam)`
   - `S3method(fitted, mvgam)`
   - `S3method(posterior_linpred, mvgam)`
   - `S3method(posterior_epred, mvgam)`
   - `S3method(posterior_predict, mvgam)`
   - `S3method(posterior_smooths, mvgam)`
3. Check for any warnings

**Validation**: No NAMESPACE conflicts

**Time estimate**: 5 minutes

---

#### 5.7 Write tests for predict.mvgam() with all type options
**File**: `tests/testthat/test-predict-fitted.R` (new file)
**Objective**: Comprehensive wrapper tests

**Steps**:
1. Create test file
2. Write tests:
   ```r
   test_that("predict.mvgam works with type='link'", {
     fit <- mvgam_example1

     pred <- predict(fit, type = "link", summary = FALSE)

     expect_true(is.matrix(pred))
   })

   test_that("predict.mvgam works with type='response'", {
     fit <- mvgam_example1

     pred_resp <- predict(fit, type = "response", summary = FALSE)
     pred_link <- predict(fit, type = "link", summary = FALSE)

     # Response should be exp(link) for Poisson
     expect_equal(pred_resp, exp(pred_link), tolerance = 1e-10)
   })

   test_that("predict.mvgam works with type='prediction'", {
     fit <- mvgam_example1

     pred <- predict(fit, type = "prediction", summary = FALSE)

     # Should be integers for Poisson
     expect_true(all(pred == floor(pred)))
   })

   test_that("predict.mvgam returns summary by default", {
     fit <- mvgam_example1

     pred <- predict(fit)

     expect_s3_class(pred, "data.frame")
     expect_true("Estimate" %in% names(pred))
     expect_true("Q2.5" %in% names(pred))
     expect_true("Q97.5" %in% names(pred))
   })

   test_that("predict.mvgam respects custom probs", {
     fit <- mvgam_example1

     pred <- predict(fit, probs = c(0.1, 0.5, 0.9))

     expect_true("Q10" %in% names(pred))
     expect_true("Q50" %in% names(pred))
     expect_true("Q90" %in% names(pred))
   })
   ```
3. Run tests

**Validation**: All tests pass

**Time estimate**: 15 minutes

---

#### 5.8 Write tests for fitted.mvgam()
**File**: `tests/testthat/test-predict-fitted.R`
**Objective**: Test in-sample predictions

**Steps**:
1. Add to existing test file:
   ```r
   test_that("fitted.mvgam returns in-sample predictions", {
     fit <- mvgam_example1

     fitted_vals <- fitted(fit, summary = FALSE)

     expect_true(is.matrix(fitted_vals))
     expect_equal(ncol(fitted_vals), nrow(mvgam_examp_dat$data_train))
   })

   test_that("fitted.mvgam respects scale argument", {
     fit <- mvgam_example1

     fitted_resp <- fitted(fit, scale = "response", summary = FALSE)
     fitted_link <- fitted(fit, scale = "linear", summary = FALSE)

     # Should match transformation
     expect_equal(fitted_resp, exp(fitted_link), tolerance = 1e-10)
   })
   ```
2. Run tests

**Validation**: Tests pass

**Time estimate**: 10 minutes

---

### 6.0 marginaleffects Integration

#### 6.1 Research required S3 methods from marginaleffects docs
**File**: Documentation review
**Objective**: Understand exact requirements for marginaleffects support

**Steps**:
1. Review https://marginaleffects.com/vignettes/extensions.html
2. Identify required methods:
   - `get_predict()` - Core prediction interface
   - `model.frame()` - Data extraction
   - `get_coef()` - Coefficient extraction (optional)
   - `vcov()` - Variance-covariance matrix (if applicable)
   - `family()` - Family object extraction
3. Document findings
4. Create implementation checklist

**Deliverable**: List of required methods with signatures

**Time estimate**: 15 minutes

---

#### 6.2 Implement get_predict.mvgam() method
**File**: `R/marginaleffects.R` (new file)
**Objective**: Core prediction interface for marginaleffects

**Steps**:
1. Create `R/marginaleffects.R`
2. Implement based on marginaleffects requirements:
   ```r
   #' Get predictions for marginaleffects
   #'
   #' @param model mvgam object
   #' @param newdata data.frame
   #' @param type Character (not used, marginaleffects controls this)
   #' @param ... Additional arguments
   #' @return Numeric vector of predictions
   #' @export
   get_predict.mvgam <- function(model, newdata, type = "response", ...) {

     # marginaleffects always needs response scale
     pred <- posterior_epred(
       model,
       newdata = newdata,
       ...
     )

     # Return mean predictions (marginaleffects handles uncertainty separately)
     colMeans(pred)
   }
   ```
3. Save file

**Note**: This is initial implementation, may need refinement based on marginaleffects behavior

**Validation**: Test with marginaleffects::predictions()

**Time estimate**: 12 minutes

---

#### 6.3 Implement model.frame.mvgam() method
**File**: `R/marginaleffects.R`
**Objective**: Extract model data for marginaleffects

**Steps**:
1. Add to `R/marginaleffects.R`:
   ```r
   #' Extract model frame for mvgam
   #'
   #' @param formula mvgam object
   #' @param ... Additional arguments
   #' @return data.frame with model data
   #' @export
   model.frame.mvgam <- function(formula, ...) {

     # Return original data with response and all predictors
     # Note: formula argument is actually the mvgam object (S3 quirk)
     object <- formula

     checkmate::assert_class(object, "mvgam")

     # Return training data
     object$data
   }
   ```
2. Save file

**Validation**: Test that marginaleffects can extract data

**Time estimate**: 8 minutes

---

#### 6.4 Implement vcov.mvgam() method (if needed)
**File**: `R/marginaleffects.R`
**Objective**: Variance-covariance matrix (may not be needed for Bayesian models)

**Steps**:
1. Research if marginaleffects needs vcov for Bayesian models
2. If needed, implement extraction from posterior covariance
3. If not needed, document why not

**Note**: Bayesian models may not need vcov method if marginaleffects uses draws directly

**Deliverable**: Decision on whether to implement, with justification

**Time estimate**: 10 minutes

---

#### 6.5 Implement get_coef.mvgam() method
**File**: `R/marginaleffects.R`
**Objective**: Coefficient extraction for marginaleffects

**Steps**:
1. Add to `R/marginaleffects.R`:
   ```r
   #' Extract coefficients for marginaleffects
   #'
   #' @param model mvgam object
   #' @param ... Additional arguments
   #' @return Named numeric vector of coefficients
   #' @export
   get_coef.mvgam <- function(model, ...) {

     # Extract fixed effects (posterior means)
     obs_params <- identify_obs_params(posterior::variables(model$fit))
     fixed_effects <- grep("^b_", obs_params, value = TRUE)

     draws <- posterior::as_draws_matrix(model$fit, variable = fixed_effects)
     coefs <- colMeans(draws)

     # Remove "b_" prefix for standard naming
     names(coefs) <- gsub("^b_", "", names(coefs))

     coefs
   }
   ```
2. Save file

**Validation**: Test coefficient extraction

**Time estimate**: 10 minutes

---

#### 6.6 Implement family.mvgam() enhancement for marginaleffects
**File**: `R/print.mvgam.R` (modify existing family.mvgam)
**Objective**: Ensure family extraction works for marginaleffects

**Steps**:
1. Open `R/print.mvgam.R`
2. Check existing `family.mvgam()` at line 589
3. Verify it returns proper family object
4. Test with marginaleffects

**Note**: family.mvgam likely already exists and works

**Validation**: Confirm marginaleffects recognizes family

**Time estimate**: 5 minutes

---

#### 6.7 Test marginaleffects::predictions() with mvgam
**File**: `tests/testthat/test-marginaleffects.R` (new file)
**Objective**: Validate predictions() function

**Steps**:
1. Create test file
2. Write tests:
   ```r
   test_that("marginaleffects::predictions works with mvgam", {
     skip_if_not_installed("marginaleffects")

     fit <- mvgam_example1
     newdata <- mvgam_examp_dat$data_test

     preds <- marginaleffects::predictions(fit, newdata = newdata)

     expect_s3_class(preds, "predictions")
     expect_equal(nrow(preds), nrow(newdata))
   })
   ```
3. Run test

**Validation**: Test passes

**Time estimate**: 10 minutes

---

#### 6.8 Test marginaleffects::slopes() with mvgam
**File**: `tests/testthat/test-marginaleffects.R`
**Objective**: Validate marginal effects computation

**Steps**:
1. Add to test file:
   ```r
   test_that("marginaleffects::slopes works with mvgam", {
     skip_if_not_installed("marginaleffects")

     fit <- mvgam_example1

     slopes <- marginaleffects::slopes(fit, variables = "season")

     expect_s3_class(slopes, "slopes")
     expect_true(nrow(slopes) > 0)
   })
   ```
2. Run test

**Validation**: Test passes

**Time estimate**: 10 minutes

---

#### 6.9 Test marginaleffects::comparisons() with mvgam
**File**: `tests/testthat/test-marginaleffects.R`
**Objective**: Validate contrasts/comparisons

**Steps**:
1. Add test:
   ```r
   test_that("marginaleffects::comparisons works with mvgam", {
     skip_if_not_installed("marginaleffects")

     fit <- mvgam_example1

     comps <- marginaleffects::comparisons(
       fit,
       variables = list(season = c(1, 4))
     )

     expect_s3_class(comps, "comparisons")
   })
   ```
2. Run test

**Validation**: Test passes

**Time estimate**: 10 minutes

---

#### 6.10 Test marginaleffects::hypotheses() with mvgam
**File**: `tests/testthat/test-marginaleffects.R`
**Objective**: Validate hypothesis testing

**Steps**:
1. Add test:
   ```r
   test_that("marginaleffects::hypotheses works with mvgam", {
     skip_if_not_installed("marginaleffects")

     fit <- mvgam_example1

     # Test simple hypothesis
     hyp <- marginaleffects::hypotheses(
       fit,
       hypothesis = "b1 = 0"
     )

     expect_s3_class(hyp, "hypotheses")
   })
   ```
2. Run test

**Validation**: Test passes

**Time estimate**: 10 minutes

---

#### 6.11 Add process_error argument integration
**File**: `R/marginaleffects.R`
**Objective**: Pass process_error through to predictions

**Steps**:
1. Modify `get_predict.mvgam()` to handle process_error:
   ```r
   get_predict.mvgam <- function(model, newdata, type = "response",
                                 process_error = TRUE, ...) {

     pred <- posterior_epred(
       model,
       newdata = newdata,
       process_error = process_error,
       ...
     )

     colMeans(pred)
   }
   ```
2. Document process_error in roxygen
3. Test that it affects marginal effects

**Validation**: Verify process_error changes results

**Time estimate**: 8 minutes

---

#### 6.12 Handle series variable in marginal effects
**File**: `R/marginaleffects.R`
**Objective**: Ensure series variable included in predictions

**Steps**:
1. Verify newdata always includes time and series for State-Space models
2. Document requirement in get_predict.mvgam()
3. Add validation if series missing for trend models

**Validation**: Test marginal effects with series variable

**Time estimate**: 10 minutes

---

### 7.0 Enhanced conditional_effects()

#### 7.1 Review current conditional_effects.mvgam() implementation
**File**: Search for existing implementation
**Objective**: Understand current state

**Steps**:
1. Search for `conditional_effects.mvgam` in codebase
2. If exists, read implementation
3. If doesn't exist, note that we'll create from scratch
4. Document current behavior

**Deliverable**: Summary of existing implementation or confirmation it doesn't exist

**Time estimate**: 10 minutes

---

#### 7.2 Add series argument for series-specific effects
**File**: `R/conditional_effects.R` (new or modified)
**Objective**: Allow series-specific conditional effects

**Steps**:
1. Create or modify `R/conditional_effects.R`
2. Add series argument:
   ```r
   #' Conditional Effects for mvgam Models
   #'
   #' @param x mvgam object
   #' @param effects Character vector of effects to plot
   #' @param series Character, specific series to show (optional)
   #' @param process_error Logical
   #' @param ... Additional arguments passed to brms::conditional_effects
   #' @return conditional_effects object
   #' @export
   conditional_effects.mvgam <- function(x,
                                         effects = NULL,
                                         series = NULL,
                                         process_error = TRUE,
                                         ...) {

     # Delegate to brms for observation model effects
     # (Implementation depends on whether we modify brmsfit or use custom logic)

     # If series specified, filter to that series
     # If process_error = FALSE, modify predictions

     # Placeholder for now
     insight::format_error("conditional_effects.mvgam not yet implemented")
   }
   ```
3. Save file

**Note**: Full implementation requires deeper investigation of brms conditional_effects patterns

**Time estimate**: 15 minutes (initial structure)

---

#### 7.3 Add process_error argument integration
**File**: `R/conditional_effects.R`
**Objective**: Control trend uncertainty in conditional effects

**Steps**:
1. Pass process_error to prediction calls
2. Document behavior in roxygen
3. Test that it affects plot uncertainty bands

**Validation**: Visual check of uncertainty bands

**Time estimate**: 10 minutes

---

#### 7.4 Maintain backward compatibility with existing code
**File**: `R/conditional_effects.R`
**Objective**: Ensure existing mvgam code doesn't break

**Steps**:
1. Test with existing models
2. Ensure default behavior unchanged
3. Add deprecation warnings if needed

**Validation**: Run existing examples and tests

**Time estimate**: 10 minutes

---

#### 7.5 Handle trend component contributions correctly
**File**: `R/conditional_effects.R`
**Objective**: Include trend effects in conditional effects plots

**Steps**:
1. Ensure predictions include trend contributions
2. Document which components are shown
3. Add option to show obs-only vs obs+trend

**Validation**: Check plots show expected patterns

**Time estimate**: 12 minutes

---

#### 7.6 Write tests for series-specific conditional effects
**File**: `tests/testthat/test-conditional-effects.R` (new file)
**Objective**: Validate series argument

**Steps**:
1. Create test file
2. Write tests:
   ```r
   test_that("conditional_effects respects series argument", {
     skip("conditional_effects not yet fully implemented")

     fit <- mvgam_example3  # Multivariate

     ce <- conditional_effects(fit, series = "series1")

     # Check that only series1 included
   })
   ```
3. Run tests (skipped until implementation complete)

**Validation**: Tests pass when implementation ready

**Time estimate**: 10 minutes

---

#### 7.7 Test backward compatibility with existing plots
**File**: `tests/testthat/test-conditional-effects.R`
**Objective**: Ensure no regression

**Steps**:
1. Add test with existing models
2. Check output format unchanged
3. Verify plots still work

**Validation**: Existing examples work

**Time estimate**: 10 minutes

---

#### 7.8 Add examples to documentation
**File**: `R/conditional_effects.R`
**Objective**: Document series and process_error usage

**Steps**:
1. Add roxygen2 examples showing:
   - Default usage
   - Series-specific effects
   - Process error toggle
2. Ensure examples run

**Validation**: Examples work in documentation

**Time estimate**: 12 minutes

---

### 8.0 Testing and Validation

#### 8.1 Write brms comparison tests (trend_formula = NULL)
**File**: `tests/testthat/test-predictions-integration.R` (new file)
**Objective**: Verify exact brms compatibility

**Steps**:
1. Create test file
2. Write comparison test:
   ```r
   test_that("mvgam predictions match brms when trend_formula = NULL", {
     skip_on_cran()
     skip_if_not_installed("brms")

     data <- data.frame(
       y = rpois(50, 10),
       x = rnorm(50)
     )

     # Fit same model in both
     brms_fit <- SW(SM(brms::brm(
       y ~ x,
       data = data,
       family = poisson(),
       chains = 2,
       iter = 500,
       silent = 2,
       refresh = 0
     )))

     mvgam_fit <- SW(SM(mvgam(
       y ~ x,
       trend_formula = NULL,
       data = data,
       family = poisson(),
       chains = 2,
       iter = 500,
       silent = 2
     )))

     newdata <- data.frame(x = c(-1, 0, 1))

     # Compare predictions
     pred_brms <- brms::posterior_epred(brms_fit, newdata = newdata)
     pred_mvgam <- posterior_epred(mvgam_fit, newdata = newdata)

     expect_equal(pred_mvgam, pred_brms, tolerance = 1e-10)
   })
   ```
3. Run test

**Validation**: Test passes with very low tolerance

**Time estimate**: 15 minutes

---

#### 8.2 Test with mvgam_example1 (univariate RW)
**File**: `tests/testthat/test-predictions-integration.R`
**Objective**: Validate with pre-fitted model

**Steps**:
1. Add test using mvgam_example1
2. Test all prediction types
3. Check dimensions and output format

**Validation**: All predictions work correctly

**Time estimate**: 12 minutes

---

#### 8.3 Test with mvgam_example2 (trend_map)
**File**: `tests/testthat/test-predictions-integration.R`
**Objective**: Validate with trend_map model

**Steps**:
1. Add test with mvgam_example2
2. Verify trend contributions
3. Check trend_map handling

**Validation**: Predictions work with trend_map

**Time estimate**: 12 minutes

---

#### 8.4 Test with mvgam_example3 (multivariate VAR)
**File**: `tests/testthat/test-predictions-multivariate.R` (new file)
**Objective**: Validate multivariate handling

**Steps**:
1. Create test file for multivariate tests
2. Test with mvgam_example3
3. Verify list return format
4. Check each response separately

**Validation**: Multivariate predictions work

**Time estimate**: 15 minutes

---

#### 8.5 Test with mvgam_example4 (GP factor model)
**File**: `tests/testthat/test-predictions-integration.R`
**Objective**: Validate factor model predictions

**Steps**:
1. Test with mvgam_example4
2. Verify factor loadings used correctly
3. Check latent variable predictions

**Validation**: Factor model predictions work

**Time estimate**: 15 minutes

---

#### 8.6 Test process_error toggle (TRUE vs FALSE)
**File**: `tests/testthat/test-predictions-integration.R`
**Objective**: Validate process_error behavior

**Steps**:
1. Add test:
   ```r
   test_that("process_error affects prediction uncertainty", {
     fit <- mvgam_example1
     newdata <- mvgam_examp_dat$data_test

     pred_with <- posterior_epred(fit, newdata = newdata, process_error = TRUE)
     pred_without <- posterior_epred(fit, newdata = newdata, process_error = FALSE)

     # Uncertainty should be larger with process error
     sd_with <- apply(pred_with, 2, sd)
     sd_without <- apply(pred_without, 2, sd)

     expect_true(all(sd_with >= sd_without))
   })
   ```
2. Run test

**Validation**: Test passes

**Time estimate**: 10 minutes

---

#### 8.7 Test ndraws subsampling
**File**: `tests/testthat/test-predictions-integration.R`
**Objective**: Validate draw subsampling

**Steps**:
1. Add test:
   ```r
   test_that("ndraws correctly subsamples posterior", {
     fit <- mvgam_example1
     newdata <- mvgam_examp_dat$data_test

     pred_all <- posterior_epred(fit, newdata = newdata)
     pred_100 <- posterior_epred(fit, newdata = newdata, ndraws = 100)

     expect_equal(nrow(pred_100), 100)
     expect_true(nrow(pred_all) > 100)

     # Means should be similar
     expect_equal(
       colMeans(pred_all),
       colMeans(pred_100),
       tolerance = 0.1
     )
   })
   ```
2. Run test

**Validation**: Test passes

**Time estimate**: 10 minutes

---

#### 8.8 Test random effects handling (re_formula)
**File**: `tests/testthat/test-predictions-integration.R`
**Objective**: Validate re_formula argument

**Steps**:
1. Need model with random effects
2. Test re_formula = NULL (include all)
3. Test re_formula = NA (exclude all)
4. Compare predictions

**Note**: May need to fit model with random effects for testing

**Validation**: re_formula changes predictions correctly

**Time estimate**: 15 minutes

---

#### 8.9 Test new random effect levels
**File**: `tests/testthat/test-predictions-integration.R`
**Objective**: Validate allow_new_levels

**Steps**:
1. Create newdata with new group level
2. Test that error occurs by default
3. Test that allow_new_levels = TRUE works
4. Check uncertainty is appropriate for new levels

**Validation**: New level handling works

**Time estimate**: 12 minutes

---

#### 8.10 Test edge cases (intercept-only, trend-only, no trends)
**File**: `tests/testthat/test-predictions-edge-cases.R` (new file)
**Objective**: Validate minimal models

**Steps**:
1. Create test file
2. Test intercept-only observation model
3. Test trend-only model (y ~ 1 with complex trend)
4. Test no trends (trend_formula = NULL)
5. Test single observation prediction

**Validation**: All edge cases work

**Time estimate**: 15 minutes

---

#### 8.11 Test distributional models
**File**: `tests/testthat/test-predictions-integration.R`
**Objective**: Validate distributional parameter predictions

**Steps**:
1. Need model with distributional formula (sigma ~ x)
2. Test predictions include distributional effects
3. Verify trend only on mu

**Note**: May need to fit distributional model for testing

**Validation**: Distributional predictions work

**Time estimate**: 15 minutes

---

#### 8.12 Test CAR trends with irregular time
**File**: `tests/testthat/test-predictions-integration.R`
**Objective**: Validate CAR-specific handling

**Steps**:
1. Test with CAR trend model
2. Verify irregular time handled correctly
3. Check distance matrix usage

**Note**: May need CAR example model

**Validation**: CAR predictions work

**Time estimate**: 12 minutes

---

#### 8.13 Test hierarchical trends (gr/subgr)
**File**: `tests/testthat/test-predictions-integration.R`
**Objective**: Validate hierarchical structure

**Steps**:
1. Test with hierarchical trend model
2. Verify group-specific predictions
3. Check partial pooling effects

**Note**: May need hierarchical example

**Validation**: Hierarchical predictions work

**Time estimate**: 12 minutes

---

#### 8.14 Test factor models (n_lv)
**File**: `tests/testthat/test-predictions-integration.R`
**Objective**: Validate reduced-rank predictions

**Steps**:
1. Use mvgam_example4 (has n_lv = 2)
2. Verify Z matrix used correctly
3. Check latent variable mapping

**Validation**: Factor predictions work

**Time estimate**: 12 minutes

---

#### 8.15 Create performance benchmarks
**File**: `tests/testthat/test-predictions-performance.R` (new file)
**Objective**: Ensure reasonable performance

**Steps**:
1. Create benchmark tests:
   ```r
   test_that("predictions complete in reasonable time", {
     fit <- mvgam_example1
     newdata <- do.call(rbind, replicate(100, mvgam_examp_dat$data_test, simplify = FALSE))

     # Should complete in < 10 seconds for 100 samples x 1000 obs
     expect_lt(
       system.time(posterior_epred(fit, newdata = newdata, ndraws = 100))["elapsed"],
       10
     )
   })
   ```
2. Run benchmarks
3. Document performance

**Deliverable**: Performance baseline

**Time estimate**: 15 minutes

---

### 9.0 Documentation and Polish

#### 9.1 Write roxygen2 docs for posterior_linpred.mvgam()
**File**: `R/posterior-linpred.R`
**Objective**: Complete documentation

**Steps**:
1. Add comprehensive roxygen2 block (see TRD section 10 for template)
2. Include all parameters with detailed descriptions
3. Add Details section explaining State-Space integration
4. Add examples
5. Run `devtools::document()`

**Validation**: Documentation builds without errors

**Time estimate**: 15 minutes

---

#### 9.2 Write roxygen2 docs for posterior_epred.mvgam()
**File**: `R/posterior-epred.R`
**Objective**: Complete documentation

**Steps**:
1. Add roxygen2 documentation
2. Explain inverse link transformation
3. Add examples comparing to posterior_linpred
4. Run `devtools::document()`

**Validation**: Documentation builds

**Time estimate**: 15 minutes

---

#### 9.3 Write roxygen2 docs for posterior_predict.mvgam()
**File**: `R/posterior-predict.R`
**Objective**: Complete documentation

**Steps**:
1. Add roxygen2 documentation
2. Explain observation noise addition
3. Add examples showing discrete outputs for Poisson
4. Run `devtools::document()`

**Validation**: Documentation builds

**Time estimate**: 15 minutes

---

#### 9.4 Write roxygen2 docs for posterior_smooths.mvgam()
**File**: `R/posterior-smooths.R`
**Objective**: Complete documentation

**Steps**:
1. Add roxygen2 documentation
2. Explain smooth term extraction
3. Add examples for single and all smooths
4. Run `devtools::document()`

**Validation**: Documentation builds

**Time estimate**: 12 minutes

---

#### 9.5 Write roxygen2 docs for posterior_average.mvgam()
**File**: `R/posterior-average.R`
**Objective**: Complete documentation (placeholder)

**Steps**:
1. Add roxygen2 documentation noting future implementation
2. Explain intended use for categorical models
3. Run `devtools::document()`

**Validation**: Documentation builds

**Time estimate**: 10 minutes

---

#### 9.6 Write roxygen2 docs for predict.mvgam()
**File**: `R/predict.R`
**Objective**: Complete documentation

**Steps**:
1. Add roxygen2 documentation
2. Explain type argument options
3. Explain summary vs draws return
4. Add examples showing all type options
5. Run `devtools::document()`

**Validation**: Documentation builds

**Time estimate**: 15 minutes

---

#### 9.7 Write roxygen2 docs for fitted.mvgam()
**File**: `R/predict.R`
**Objective**: Complete documentation

**Steps**:
1. Add roxygen2 documentation
2. Explain in-sample vs out-of-sample predictions
3. Add examples
4. Run `devtools::document()`

**Validation**: Documentation builds

**Time estimate**: 10 minutes

---

#### 9.8 Create "Prediction and Forecasting" vignette outline
**File**: `vignettes/predictions.Rmd` (new file)
**Objective**: Structure vignette content

**Steps**:
1. Create vignette file with YAML header
2. Add outline:
   - Introduction
   - Three prediction scales
   - Working with newdata
   - Process error and uncertainty
   - Random effects
   - Multivariate models
   - Performance considerations
   - Integration with visualization
3. Add code chunks placeholders

**Deliverable**: Vignette structure

**Time estimate**: 12 minutes

---

#### 9.9 Write vignette section: Introduction and basic workflow
**File**: `vignettes/predictions.Rmd`
**Objective**: Complete introduction

**Steps**:
1. Write introduction explaining prediction system
2. Add basic workflow example with mvgam_example1
3. Show simple posterior_epred call
4. Add summary statistics computation

**Validation**: Vignette builds

**Time estimate**: 15 minutes

---

#### 9.10 Write vignette section: Three prediction scales
**File**: `vignettes/predictions.Rmd`
**Objective**: Explain link/response/prediction scales

**Steps**:
1. Write explanation of three scales
2. Add example comparing all three
3. Show transformation relationships
4. Visualize uncertainty differences

**Validation**: Vignette builds

**Time estimate**: 15 minutes

---

#### 9.11 Write vignette section: Process error and uncertainty
**File**: `vignettes/predictions.Rmd`
**Objective**: Explain process_error argument

**Steps**:
1. Write explanation of trend parameter uncertainty
2. Add example comparing process_error = TRUE vs FALSE
3. Show uncertainty band differences
4. Discuss when to use each

**Validation**: Vignette builds

**Time estimate**: 15 minutes

---

#### 9.12 Write vignette section: Random effects handling
**File**: `vignettes/predictions.Rmd`
**Objective**: Explain re_formula and new levels

**Steps**:
1. Write explanation of random effects in predictions
2. Add example with re_formula = NA
3. Show allow_new_levels usage
4. Discuss uncertainty for new groups

**Validation**: Vignette builds

**Time estimate**: 15 minutes

---

#### 9.13 Write vignette section: Multivariate models
**File**: `vignettes/predictions.Rmd`
**Objective**: Explain multivariate prediction workflow

**Steps**:
1. Write explanation of multivariate predictions
2. Add example with mvgam_example3
3. Show list return format
4. Demonstrate response-specific predictions

**Validation**: Vignette builds

**Time estimate**: 15 minutes

---

#### 9.14 Create "marginaleffects Integration" vignette outline
**File**: `vignettes/marginaleffects.Rmd` (new file)
**Objective**: Structure marginaleffects vignette

**Steps**:
1. Create vignette file
2. Add outline:
   - Introduction to marginal effects
   - Computing slopes
   - Computing comparisons
   - Average marginal effects
   - Custom hypotheses
   - Series-specific effects
   - Visualization
3. Add placeholder code chunks

**Deliverable**: Vignette structure

**Time estimate**: 10 minutes

---

#### 9.15 Write marginaleffects vignette content
**File**: `vignettes/marginaleffects.Rmd`
**Objective**: Complete marginaleffects vignette

**Steps**:
1. Write introduction
2. Add examples for each marginaleffects function
3. Show visualization with plot()
4. Demonstrate series-specific effects
5. Add comparison with manual computation

**Validation**: Vignette builds and runs

**Time estimate**: 15 minutes

---

#### 9.16 Add prediction examples to README
**File**: `README.md`
**Objective**: Update README with prediction examples

**Steps**:
1. Open README.md
2. Add "Predictions" section after model fitting
3. Include brief examples of:
   - posterior_epred()
   - predict() with summary
   - marginaleffects integration
4. Link to vignettes for details

**Validation**: README renders correctly

**Time estimate**: 10 minutes

---

#### 9.17 Run R CMD check and fix any issues
**File**: Package-wide
**Objective**: Ensure package passes checks

**Steps**:
1. Run `devtools::check()`
2. Review output for errors, warnings, notes
3. Fix any issues:
   - Missing imports
   - Undocumented functions
   - Example errors
   - Test failures
4. Re-run check until clean

**Validation**: R CMD check passes with no errors/warnings

**Time estimate**: 15 minutes (assuming minimal issues)

---

#### 9.18 Final review and cleanup
**File**: All prediction files
**Objective**: Polish and finalize

**Steps**:
1. Review all prediction code for:
   - Consistent style (tidyverse, 80 char lines)
   - Clear comments
   - Proper error messages
   - Complete documentation
2. Remove any debugging code
3. Check for TODO comments
4. Ensure consistent naming
5. Final test run

**Validation**: Clean, professional code

**Time estimate**: 15 minutes

---

## Notes

### Implementation Priority

The tasks are ordered for sequential implementation:

1. **Foundation (Task 1)** must be completed first - everything depends on it
2. **Core computation (Task 2)** builds the prediction engine
3. **Transformations (Task 3)** extends to all prediction types
4. **Specialized functions (Task 4)** adds advanced features
5. **Wrappers (Task 5)** provides user-friendly interface
6. **marginaleffects (Task 6)** enables ecosystem integration
7. **conditional_effects (Task 7)** enhances visualization
8. **Testing (Task 8)** validates everything
9. **Documentation (Task 9)** makes it usable

### Dependencies Between Tasks

- Task 2 depends on Task 1 (needs mock stanfit and parameter ID)
- Task 3 depends on Task 2 (uses posterior_linpred)
- Task 4 depends on Task 2 (uses same prep infrastructure)
- Task 5 depends on Task 2-3 (wraps core functions)
- Task 6 depends on Task 2-3 (uses posterior_epred)
- Task 8 depends on Tasks 1-7 (tests everything)
- Task 9 can be done in parallel with testing

### Testing Strategy

- Write tests alongside implementation (not after)
- Use pre-fitted models (mvgam_example1-4) for fast integration tests
- Use `setup_stan_test_data()` for unit tests requiring fresh data
- Standard settings: `chains = 2, iter = 500, silent = 2`
- Wrap with `SW(SM(...))` to suppress output

### Time Estimates

- Total estimated time: ~30-35 hours of focused development
- Foundation: ~3 hours
- Core computation: ~4 hours
- Transformations: ~4 hours
- Specialized functions: ~4 hours
- Wrappers: ~2.5 hours
- marginaleffects: ~3.5 hours
- conditional_effects: ~2.5 hours
- Testing: ~6 hours
- Documentation: ~5 hours

### Key Architectural Decisions

From pathfinder investigations:

1. **obs_model and trend_model already exist** during fitting but aren't stored - just need to add storage
2. **Parameter identification** uses "_trend" suffix pattern
3. **Factor loadings (Z)** stored in parameters block for factor models, identity matrix for non-factor
4. **Multivariate models** use single brmsfit with response suffixes
5. **brms:::prepare_predictions()** is the verified approach for design matrices
6. **process_error** implemented by using posterior means vs full posterior of trend parameters

### Success Criteria

Implementation complete when:
- All 9 parent tasks completed
- All tests pass (no errors or warnings)
- R CMD check passes cleanly
- Both vignettes build successfully
- marginaleffects functions work without errors
- Documentation complete for all exported functions
- README updated with examples
