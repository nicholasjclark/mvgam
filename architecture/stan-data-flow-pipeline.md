# Stan Data Flow Pipeline Documentation

## Overview

The mvgam package uses a two-stage assembly system that combines brms for observation modeling with mvgam's own trend extensions. The pipeline processes user input (formulas and data) through multiple stages before generating a unified Stan model.

## Pipeline Stages

### Stage 1: Input Processing and Convergence
- **Entry points**: 
  - `mvgam()` in `R/mvgam_core.R` (for model fitting)
  - `stancode.mvgam_formula()` in `R/make_stan.R` (for code inspection)
  - `standata.mvgam_formula()` in `R/make_stan.R` (for data inspection)
- **Convergence point**: All entry points converge at `generate_stan_components_mvgam_formula()` in `R/make_stan.R`
- **Input**: User formula, trend_formula, data, family, and additional parameters
- **Processing**: 
  - Creates `mvgam_formula` object for shared processing
  - Single source of truth for Stan code generation eliminating duplicate logic
  - Input validation using checkmate, multiple imputation detection, multivariate trend parsing via `parse_multivariate_trends()`
- **Output**: Structured components containing all elements needed for downstream processing
- **Available data structures**: Raw user data with original row ordering, complete component set

### Stage 2: Formula Parsing and Validation
- **Entry point**: `parse_multivariate_trends()` in `R/brms_integration.R`
- **Input**: Main formula and trend_formula specifications
- **Processing**: Validates brms compatibility, extracts response names for multivariate models, creates trend specifications structure
- **Output**: Structured mv_spec object containing trend_specs and formula information
- **Available data structures**: Formula objects, response names, trend type information

### Stage 3: Data Validation and Comprehensive Time Series Analysis
- **Entry point**: `extract_and_validate_trend_components()` in `R/validations.R`
- **Input**: Raw data, parsed trend specifications, and response variable names
- **Processing**: 
  - Calls `extract_time_series_dimensions(response_vars)`, which resolves the series, time and factor axes once and records them
  - Builds the observation-to-trend mapping arrays for every response variable in one pass
  - Validates factor levels and time series structure
- **Output**: A dimensions object carrying the axis record and the mapping arrays
- **Available data structures**: 
  - `dimensions$n_time`, `dimensions$n_series`, `dimensions$n_obs`
  - `dimensions$axes`, the record Stan assembly and every post-fit method read. `axes$series` holds the ordered levels, how they were arrived at, their count, the group each belongs to and the last occasion each was observed on; `axes$time` holds the user's own ordered times, the integer index and the step; `axes$factor` holds `n_lv`; `axes$grain` names what the second dimension of `times_trend` indexes; `axes$vars` names the columns a row is placed by
  - `dimensions$mappings`: the `obs_trend_time` and `obs_trend_series` arrays for each response variable

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
  - **Trend predictor**: `extract_and_rename_stan_blocks()` builds `mu_trend` from the brms trend program, using `extract_mu_construction_with_classification()` (`R/mu_expression_analysis.R`) to find the statements that construct its `mu`
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

### Stage 6: Stan Code Assembly with Dependency Ordering and Deduplication
- **Entry point**: `generate_combined_stancode()` in `R/stan_assembly.R`
- **Input**: obs_setup, trend_setup, and generated stanvars
- **Processing**:
  - **CRITICAL**: Calls `sort_stanvars()` to reorder stanvars by dependency priority before injection
  - **Dependency Resolution**: Ensures Stan variables declared before use:
    - **Priority 1**: Dimension variables (`n_trend`, `n_series_trend`, `n_lv_trend`)
    - **Priority 2**: Arrays referencing dimensions (`times_trend`, `obs_trend_time`, etc.)
    - **Priority 3**: All other stanvars
  - Combines brms observation code with trend stanvars using `generate_base_stancode_with_stanvars()`
  - Merges Stan data from both models
  - **Deduplication System**: Applied after initial combination to prevent compilation errors:
    - **Function deduplication**: `deduplicate_stan_functions()` removes duplicate function definitions
  - Validates combined code structure
- **Output**: Base Stan code with properly ordered and deduplicated trend variables ready for injection
- **Available data structures**: Combined Stan code with both observation and trend components in correct declaration order and no duplicates

### Stage 6.5: Integrated Stan Code Polishing (Post-Assembly)
- **Entry point**: `polish_generated_stan_code()` called from `generate_stan_components_mvgam_formula()` in `R/make_stan.R`
- **Input**: Raw combined Stan code from assembly stage
- **Processing**:
  - **Single polishing point**: Applied once in shared infrastructure ensures consistency
  - **Automatic formatting**: Removes empty lines, trims whitespace, removes duplicates
  - **Consistent output**: Both `mvgam()` and `stancode()` get identically polished code
- **Output**: Polished Stan code ready for compilation or inspection
- **Available data structures**: Final polished Stan code with consistent formatting

### Stage 7: Trend Injection
- **Entry point**: `inject_trend_into_linear_predictors(base_stancode, resps)` in `R/stan_assembly.R`. `resps` is `""` for a univariate model and the brms response keys for a multivariate one; `<sfx>` below is `""` or `_<resp>`
- **Per response**, `inject_trend_for_response()` adds `mu<sfx>[n] += trend[obs_trend_time<sfx>[n], obs_trend_series<sfx>[n]]` in a loop:
  - **GLM likelihood on `Y<sfx>`**: `rewrite_glm_for_trend()` declares `mu<sfx> = design * coefs`, adds the intercept and trend and rewrites the call to take `to_matrix(mu<sfx>)` and `mu_ones<sfx>`. brms writes an offset model's GLM call with the declared `mu<sfx>` as its intercept, and that call takes the next path unchanged
  - **Built predictor**: `add_trend_to_built_predictor()` places the loop before the first statement that transforms `mu<sfx>` (an inverse link, or the skew-normal mean shift), outside any loop enclosing it. Without a transform, the loop follows the last statement that builds `mu<sfx>`
- **Faults**: an unknown GLM family, or a model block with no single correct place for the trend, raises `stop_mvgam_fault()`. `resolve_observation_family()` has already refused the families mvgam does not support, `mixture()` among them

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
- **Supported GLM Types**: the entries of `glm_call_layout` (`R/glm_analysis.R`): `normal_id_glm`, `poisson_log_glm`, `neg_binomial_2_log_glm`, `bernoulli_logit_glm` and `ordered_logistic_glm`. `categorical_logit_glm` has no entry because mvgam refuses `brms::categorical()`
- **Parameter Parsing**: `parse_glm_parameters_from_line()` maps each argument to its role in the family's `glm_call_layout` entry:
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
[MULTIPLE ENTRY POINTS - DRY CONSOLIDATION ARCHITECTURE]

├─ mvgam() in R/mvgam_core.R →                          [MODEL FITTING PATH]
│   ├─ mvgam_single_dataset() →
│   │   └─ generate_stan_components_mvgam_formula() →   [CONVERGES HERE]
│   └─ fit_mvgam_model() + create_mvgam_from_combined_fit()
│
├─ stancode.mvgam_formula() in R/make_stan.R →          [CODE INSPECTION PATH]
│   └─ generate_stan_components_mvgam_formula() →       [CONVERGES HERE]
│
├─ standata.mvgam_formula() in R/make_stan.R →          [DATA INSPECTION PATH]
│   └─ generate_stan_components_mvgam_formula() →       [CONVERGES HERE]
│
└─ generate_stan_components_mvgam_formula() →           [SHARED INFRASTRUCTURE - SINGLE SOURCE OF TRUTH]
    ├─ parse_multivariate_trends() →                   [EXTRACTS response_names FROM FORMULA]
    │   └─ returns mv_spec with response_names →
    ├─ extract_and_validate_trend_components(data, mv_spec, response_vars) →
    │   └─ extract_time_series_dimensions(data, time_var, series_var, trend_type, response_vars) →
    │       ├─ calculate dimensions (n_time, n_series, n_obs) →
    │       ├─ create ordering mappings →
    │       └─ FOR EACH response_var IN response_vars:
    │           └─ generate_obs_trend_mapping(data, response_var, time_var, series_var, dimensions) →
    │               ├─ which(!is.na(data[[response_var]])) →    [IDENTIFIES NON-MISSING OBS]
    │               ├─ match(obs_times, sorted_unique_times) →   [MAPS TO TIME INDICES]
    │               ├─ match(obs_series, sorted_unique_series) → [MAPS TO SERIES INDICES]
    │               └─ returns {obs_trend_time, obs_trend_series} arrays →
    ├─ setup_brms_lightweight() →                      [BRMS PROCESSES DATA INDEPENDENTLY]
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
    │   ├─ sort_stanvars() →                            [DEPENDENCY-BASED REORDERING]
    │   └─ inject_trend_into_linear_predictors() →      [ONE PASS PER RESPONSE]
    │       └─ inject_trend_for_response() →
    │           ├─ rewrite_glm_for_trend() →            [GLM LIKELIHOOD ON Y<sfx>]
    │           │   ├─ parse_glm_parameters_from_line()
    │           │   └─ build_glm_call_on_mu()
    │           └─ add_trend_to_built_predictor() →     [EVERY OTHER LIKELIHOOD]
    ├─ polish_generated_stan_code() →                   [SINGLE POLISHING POINT]
    └─ returns {combined_components, obs_setup, trend_setup, mv_spec} →
        ├─ mvgam() path: extract stancode/standata + create mvgam object
        ├─ stancode() path: extract and return polished stancode
        └─ standata() path: extract and return standata
```
