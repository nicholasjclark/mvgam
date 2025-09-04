# Stan Data Flow Pipeline Documentation

## Overview

The mvgam package uses a two-stage assembly system that combines brms for observation modeling with custom trend extensions. The pipeline processes user input (formulas and data) through multiple stages before generating a unified Stan model.

## Pipeline Stages

### Stage 1: Input Processing
- **Entry point**: `mvgam()` in `R/mvgam_core.R`
- **Input**: User formula, trend_formula, data, family, and additional parameters
- **Processing**: Input validation using checkmate, multiple imputation detection, multivariate trend parsing via `parse_multivariate_trends()`
- **Output**: Validated inputs and multivariate specification (mv_spec)
- **Available data structures**: Raw user data with original row ordering

### Stage 2: Formula Parsing and Validation
- **Entry point**: `parse_multivariate_trends()` in `R/brms_integration.R`
- **Input**: Main formula and trend_formula specifications
- **Processing**: Validates brms compatibility, extracts response names for multivariate models, creates trend specifications structure
- **Output**: Structured mv_spec object containing trend_specs and formula information
- **Available data structures**: Formula objects, response names, trend type information

### Stage 3: Data Validation and Comprehensive Time Series Analysis
- **Entry point**: `validate_time_series_for_trends()` in `R/validations.R`
- **Input**: Raw data, parsed trend specifications, and response variable names
- **Processing**: 
  - **Centralized Analysis**: Calls `extract_time_series_dimensions(response_vars)` for complete time series processing
  - Creates bidirectional mappings: `stan_to_original` and `original_to_stan` indices
  - Generates time/series index mappings for trend matrix structure
  - **Mapping Generation**: Creates observation-to-trend mapping arrays for ALL response variables in one pass
  - Validates factor levels and time series structure
- **Output**: Enhanced dimensions object with embedded mapping arrays
- **Available data structures**: 
  - `dimensions$n_time`, `dimensions$n_series`, `dimensions$n_obs`
  - `dimensions$ordering$stan_to_original` (maps Stan row index → Original row index)
  - `dimensions$ordering$original_to_stan` (maps Original row index → Stan row index)
  - `dimensions$ordering$time_indices` and `dimensions$ordering$series_indices`
  - **`dimensions$mappings`**: Contains pre-generated observation-to-trend mapping arrays for each response variable

### Stage 4: brms Setup
- **Entry point**: `setup_brms_lightweight()` in `R/brms_integration.R`
- **Input**: Validated formulas and data
- **Processing**: 
  - Creates brms mock fit using `backend = "mock"` for rapid setup
  - Generates base Stan code and data through brms pipeline
  - brms internally handles missing data and reorders observations
  - Extracts stancode, standata, priors, and brmsterms
- **Output**: obs_setup and trend_setup objects with brms components
- **Available data structures**: 
  - `obs_setup$stancode` (brms-generated Stan code for observations)
  - `obs_setup$standata` (brms-ordered Stan data - DIFFERENT ORDER than original)
  - `trend_setup$stancode` and `trend_setup$standata` (for trend model)
  - brmsfit objects for prediction compatibility

### Stage 5: Stanvar Generation with GLM Detection
- **Entry point**: `extract_trend_stanvars_from_setup()` in `R/stan_assembly.R`
- **Input**: trend_setup, trend specifications with embedded dimensions and mappings, and obs_setup for GLM detection
- **Processing**:
  - **GLM Detection and Optimization**: Analyzes observation model stancode for GLM function usage
    - **Detection Method**: Uses `detect_glm_usage()` to identify GLM functions (poisson_log_glm, normal_id_glm, etc.)
    - **Automatic Enhancement**: If GLM detected, adds `mu_ones` data stanvar for GLM compatibility
    - **GLM Support**: Handles all brms GLM types including poisson, normal, negative binomial, bernoulli, and ordered logistic
  - Extracts brms trend parameters using `extract_and_rename_stan_blocks()` with `_trend` suffix
    - **Block Extraction**: Uses line-by-line parsing with proper boundary detection
    - **Parameter Filtering**: Excludes `Y_trend`, `prior_only_trend`, and `N_trend` to avoid duplication
    - **Duplicate Prevention**: Uses `filter_block_content()` to remove duplicate lprior declarations
  - Creates `times_trend` matrix using `generate_times_trend_matrices()`
  - The `times_trend[i,j]` matrix maps time i, series j to time indices
  - **Simplified Mapping Integration**: Extracts pre-generated mapping arrays from `dimensions.mappings`
    - **Architecture Benefit**: Eliminates complex parameter threading and separate mapping generation
    - **Source**: Mappings were generated during Stage 3 alongside dimension calculation
    - **Structure**: `dimensions.mappings` contains `obs_trend_time` and `obs_trend_series` arrays for each response
    - **Validation**: Arrays are pre-validated during dimension extraction stage
  - Converts mapping arrays to Stan data block stanvars with appropriate response suffixes
  - Generates trend-specific Stan variables via `generate_trend_specific_stanvars()`:
    - **Common dimensions**: `n_trend`, `n_series_trend`, `n_lv_trend` (injected for all trend models)
    - **Shared innovations**: Creates unified innovation parameters and sampling statements
    - **Trend-specific parameters**: Generated based on trend type (AR, RW, VAR, etc.)
    - **Parameter coordination**: Ensures no duplicate declarations across stanvar sources
- **Output**: Comprehensive stanvar objects for trend model including mapping arrays and GLM compatibility stanvars
- **Available data structures**:
  - `times_trend` matrix [n_time, n_series] with time indexing
  - `obs_trend_time` array [N] mapping each non-missing observation to its time index
  - `obs_trend_series` array [N] mapping each non-missing observation to its series index
  - `mu_ones` data stanvar [1] containing value 1 for GLM beta parameter (when GLM detected)
  - Trend parameters (`sigma_trend`, `Intercept_trend`, etc.)
  - Innovation matrices and state vectors
  - Common dimension variables for trend matrix structure

### Stage 6: Stan Code Assembly with Dependency Ordering
- **Entry point**: `generate_combined_stancode()` in `R/stan_assembly.R`
- **Input**: obs_setup, trend_setup, and generated stanvars
- **Processing**:
  - **🏆 CRITICAL**: Calls `sort_stanvars()` to reorder stanvars by dependency priority before injection
  - **Dependency Resolution**: Ensures Stan variables declared before use:
    - **Priority 1**: Dimension variables (`n_trend`, `n_series_trend`, `n_lv_trend`)
    - **Priority 2**: Arrays referencing dimensions (`times_trend`, `obs_trend_time`, etc.)
    - **Priority 3**: All other stanvars
  - Combines brms observation code with trend stanvars using `generate_base_stancode_with_stanvars()`
  - Merges Stan data from both models
  - Validates combined code structure
- **Output**: Base Stan code with properly ordered trend variables ready for injection
- **Available data structures**: Combined Stan code with both observation and trend components in correct declaration order

### Stage 7: GLM-Compatible Trend Injection
- **Entry point**: `inject_trend_into_linear_predictor()` in `R/stan_assembly.R`
- **Input**: Base Stan code and trend stanvars (including mapping arrays and GLM compatibility stanvars)
- **Processing**: 
  - **Adaptive Approach**: Automatically selects optimal trend injection method based on GLM detection
  - **GLM-Optimized Path**: When GLM functions detected (e.g., `poisson_log_glm_lpmf`):
    - **Parameter Parsing**: Uses `parse_glm_parameters()` with regex patterns to extract Y variable, design matrix, intercept, and coefficients
    - **Efficient Computation**: Creates `vector[N] mu = Xc * b` using matrix multiplication for base linear predictor
    - **Trend Integration**: Adds intercept and trend in loop: `mu[n] += Intercept + trend[obs_trend_time[n], obs_trend_series[n]]`
    - **GLM Preservation**: Transforms GLM call to `glm_function(Y | to_matrix(mu), 0.0, mu_ones, ...)` preserving optimization
    - **Type Safety**: Uses correct lpdf/lpmf suffix based on distribution (continuous vs discrete)
  - **Standard Path**: When no GLM optimization detected:
    - **Traditional Injection**: Direct insertion into transformed parameters block
    - **Compatibility**: Works with all non-GLM likelihood specifications
  - **Key Innovation**: Replaces missing `obs_ind` references with explicit mapping arrays
  - **Validation**: Ensures mapping arrays exist in stanvars before attempting injection
  - **Missing Data Handling**: Works correctly even with missing observations since brms and mapping arrays are aligned
  - **Universal Pattern**: All models use `trend[i,s] = dot_product(Z[s,:], LV[i,:]) + mu_trend[times_trend[i,s]]` for trend computation
- **Output**: Complete Stan code ready for compilation with optimal computation strategy:
  1. **Trend Computation**: `trend` matrix values using universal state-space formula
  2. **GLM Path**: Efficient matrix-based `mu` construction with GLM-compatible function calls
  3. **Standard Path**: Direct trend injection into linear predictor
- **Available data structures**: All previous structures including observation-to-trend mappings and GLM compatibility variables

## Critical Data Structures

### Observation Data
- **Structure**: Data frame ordered by brms (potentially different from original)
- **Ordering**: brms may reorder for missing data handling and computational efficiency
- **Available at stages**: Raw form in stages 1-3, brms-reordered form in stages 4-7
- **Key insight**: brms standata uses its own internal ordering that may not match original data order

### Trend Data
- **Structure**: Matrix[n_time, n_series] representing trend values over time
- **Creation point**: Generated in stage 5 via stanvar creation
- **Available at stages**: Stages 5-7
- **Indexing**: Uses `times_trend[i,j]` matrix for time/series to index mapping

### Observation-to-Trend Mapping
- **Purpose**: Map each observation in brms-ordered data to its corresponding position in the trend matrix
- **Problem solved**: brms excludes NA observations but doesn't provide `obs_ind` array for mapping back to trend positions
- **Structure**: Two integer arrays created during stanvar generation:
  - `obs_trend_time[N]`: Time index for each of N non-missing observations
  - `obs_trend_series[N]`: Series index for each of N non-missing observations
- **Usage**: Access trend values via `trend[obs_trend_time[n], obs_trend_series[n]]` for observation n
- **Creation**: `generate_obs_trend_mapping()` in `R/validations.R` implements the mapping logic:
  - Identifies non-missing observations: `which(!is.na(data[[response_var]]))`
  - Maps time values: `match(obs_data[[time_var]], sorted_unique_times)`
  - Maps series values: `match(obs_data[[series_var]], sorted_unique_series)`  
  - Uses dimensions object from earlier validation for consistent ordering
- **Validation**: Comprehensive bounds checking ensures all indices are within [1, n_time] and [1, n_series]
- **Metadata**: Includes `n_obs_non_missing` and `has_missing` flags for downstream processing
- **Stan Integration**: Arrays added as "data" block stanvars with appropriate response suffixes

### GLM Optimization System
- **Purpose**: Preserves brms GLM optimization while enabling trend injection into linear predictors
- **Detection**: Automatic identification of GLM functions in observation model during stanvar generation
- **Supported GLM Types**: All brms GLM functions including:
  - `poisson_log_glm_lpmf`: Poisson regression with log link
  - `normal_id_glm_lpdf`: Normal regression with identity link  
  - `neg_binomial_2_log_glm_lpmf`: Negative binomial regression with log link
  - `bernoulli_logit_glm_lpmf`: Bernoulli regression with logit link
  - `ordered_logistic_glm_lpmf`: Ordered logistic regression
  - `categorical_logit_glm_lpmf`: Categorical regression with logit link
- **Parameter Parsing**: Regex-based extraction of GLM function parameters:
  - **Y variable**: Response variable name from GLM call
  - **Design matrix**: Matrix or vector containing predictors (e.g., `Xc`)
  - **Intercept**: Scalar intercept parameter (e.g., `Intercept`)
  - **Coefficients**: Vector of regression coefficients (e.g., `b`)
  - **Other parameters**: Additional parameters like `sigma`, `shape` for specific distributions
- **Efficiency Optimizations**:
  - **Matrix multiplication**: `vector[N] mu = Xc * b` for base linear predictor computation
  - **Minimal looping**: Only loop for intercept and trend addition per observation
  - **GLM preservation**: Maintains Stan's optimized GLM implementations
- **Type compatibility**: 
  - **Design matrix conversion**: Uses `to_matrix(mu)` to convert vector to required matrix type
  - **Beta parameter**: `mu_ones` stanvar provides required vector[1] for GLM beta parameter
  - **Distribution-specific suffixes**: Automatically uses `_lpdf` for continuous, `_lpmf` for discrete distributions

## Function Call Hierarchy

```
mvgam() →
  ├─ parse_multivariate_trends() →                       [EXTRACTS response_names FROM FORMULA]
  │   └─ returns mv_spec with response_names →
  ├─ validate_time_series_for_trends(data, trend_specs, response_vars) →
  │   └─ extract_time_series_dimensions(data, time_var, series_var, trend_type, response_vars) →
  │       ├─ calculate dimensions (n_time, n_series, n_obs) →
  │       ├─ create ordering mappings →
  │       └─ FOR EACH response_var IN response_vars:
  │           └─ generate_obs_trend_mapping(data, response_var, time_var, series_var, dimensions) →
  │               ├─ which(!is.na(data[[response_var]])) →    [IDENTIFIES NON-MISSING OBS]
  │               ├─ match(obs_times, sorted_unique_times) →   [MAPS TO TIME INDICES]
  │               ├─ match(obs_series, sorted_unique_series) → [MAPS TO SERIES INDICES]
  │               └─ returns {obs_trend_time, obs_trend_series} arrays →
  ├─ setup_brms_lightweight() →                          [BRMS PROCESSES DATA INDEPENDENTLY]
  ├─ generate_combined_stancode() →
  │   ├─ extract_trend_stanvars_from_setup(trend_setup, trend_specs, response_suffix, response_name, obs_setup) →
  │   │   ├─ detect_glm_usage(obs_setup$stancode) →     [GLM DETECTION AND OPTIMIZATION]
  │   │   ├─ create mu_ones stanvar (if GLM detected) →
  │   │   ├─ extract_and_rename_trend_parameters() →
  │   │   ├─ filter_block_content() →                   [REMOVES DUPLICATE DECLARATIONS]
  │   │   ├─ dimensions$mappings[[response_name]] →     [RETRIEVES PRE-GENERATED MAPPING]
  │   │   ├─ create stanvar for obs_trend_time →
  │   │   ├─ create stanvar for obs_trend_series →
  │   │   └─ generate_trend_specific_stanvars() →
  │   ├─ sort_stanvars() →                              [🏆 CRITICAL: DEPENDENCY-BASED REORDERING]
  │   └─ inject_trend_into_linear_predictor() →         [GLM-COMPATIBLE TREND INJECTION]
  │       ├─ detect_glm_usage() →                       [DETERMINES INJECTION APPROACH]
  │       ├─ inject_trend_into_glm_predictor() →        [GLM-OPTIMIZED PATH]
  │       │   ├─ parse_glm_parameters() →               [EXTRACTS GLM FUNCTION PARAMETERS]
  │       │   ├─ generate efficient mu construction →    [MATRIX MULTIPLICATION + LOOP]
  │       │   └─ transform_glm_call() →                 [GLM FUNCTION TRANSFORMATION]
  │       └─ standard trend injection →                 [TRADITIONAL PATH FOR NON-GLM]
  └─ Stan compilation and fitting
```
