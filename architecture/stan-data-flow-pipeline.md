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
- **Input**: Raw data, parsed trend specifications, and response variable information
- **Processing**: 
  - **Centralized Analysis**: Calls `extract_time_series_dimensions(response_vars)` for complete time series processing
  - Creates bidirectional mappings: `stan_to_original` and `original_to_stan` indices
  - Generates time/series index mappings for trend matrix structure
  - **Mapping Generation**: Creates observation-to-trend mapping arrays for all response variables
  - Validates factor levels and time series structure
- **Output**: Enhanced dimensions object with embedded mapping arrays
- **Available data structures**: 
  - `dimensions$n_time`, `dimensions$n_series`, `dimensions$n_obs`
  - `dimensions$ordering$stan_to_original` (maps Stan row index → Original row index)
  - `dimensions$ordering$original_to_stan` (maps Original row index → Stan row index)
  - `dimensions$ordering$time_indices` and `dimensions$ordering$series_indices`
  - **`dimensions$mappings`**: Contains observation-to-trend mapping arrays for each response variable

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

### Stage 5: Stanvar Generation
- **Entry point**: `extract_trend_stanvars_from_setup()` in `R/stan_assembly.R`
- **Input**: trend_setup and trend specifications with embedded dimensions and mappings
- **Processing**:
  - Extracts brms trend parameters and renames with `_trend` suffix
  - Creates `times_trend` matrix using `generate_times_trend_matrices()`
  - The `times_trend[i,j]` matrix maps time i, series j to time indices
  - **Simplified Mapping Integration**: Extracts pre-generated mapping arrays from `dimensions.mappings`
    - **Architecture Benefit**: Eliminates complex parameter threading and separate mapping generation
    - **Source**: Mappings were generated during Stage 3 alongside dimension calculation
    - **Structure**: `dimensions.mappings` contains `obs_trend_time` and `obs_trend_series` arrays for each response
    - **Validation**: Arrays are pre-validated during dimension extraction stage
  - Converts mapping arrays to Stan data block stanvars with appropriate response suffixes
  - Generates trend-specific Stan variables (innovations, parameters, etc.)
- **Output**: Comprehensive stanvar objects for trend model including mapping arrays
- **Available data structures**:
  - `times_trend` matrix [n_time, n_series] with time indexing
  - `obs_trend_time` array [N] mapping each non-missing observation to its time index
  - `obs_trend_series` array [N] mapping each non-missing observation to its series index
  - Trend parameters (`sigma_trend`, `Intercept_trend`, etc.)
  - Innovation matrices and state vectors

### Stage 6: Stan Code Assembly
- **Entry point**: `generate_combined_stancode()` in `R/stan_assembly.R`
- **Input**: obs_setup, trend_setup, and generated stanvars
- **Processing**:
  - Combines brms observation code with trend stanvars using `generate_base_stancode_with_stanvars()`
  - Merges Stan data from both models
  - Validates combined code structure
- **Output**: Base Stan code with embedded trend variables but no trend injection yet
- **Available data structures**: Combined Stan code with both observation and trend components

### Stage 7: Trend Injection
- **Entry point**: `inject_trend_into_linear_predictor()` in `R/stan_assembly.R`
- **Input**: Base Stan code and trend stanvars (including mapping arrays)
- **Processing**: 
  - Injects trend effects into observation linear predictor
  - **Key Innovation**: Replaces missing `obs_ind` references with explicit mapping arrays
  - **Implementation**: Generates Stan code: `mu[n] += trend[obs_trend_time[n], obs_trend_series[n]]` for observation n
  - **Validation**: Ensures mapping arrays exist in stanvars before attempting injection
  - **Missing Data Handling**: Works correctly even with missing observations since brms and mapping arrays are aligned
  - **Universal Pattern**: All models use `trend[i,s] = dot_product(Z[s,:], LV[i,:]) + mu_trend[times_trend[i,s]]` for trend computation
- **Output**: Complete Stan code ready for compilation with two-stage computation:
  1. Stage 1: Compute `trend` matrix values using universal formula
  2. Stage 2: Add trend effects to brms `mu`: `mu[n] += trend[obs_trend_time[n], obs_trend_series[n]]`
- **Available data structures**: All previous structures including observation-to-trend mappings

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

## Function Call Hierarchy

```
mvgam() →
  ├─ parse_multivariate_trends() →
  ├─ validate_time_series_for_trends() →
  │   └─ extract_time_series_dimensions(response_vars) → [CENTRALIZED: DIMENSIONS + MAPPINGS]
  │       ├─ which(!is.na(response)) →                   [IDENTIFIES NON-MISSING OBS]
  │       ├─ match(time_vals, unique_times) →            [MAPS TO TIME INDICES]
  │       ├─ match(series_vals, unique_series) →         [MAPS TO SERIES INDICES]
  │       └─ returns {dimensions + mappings} →           [COMPLETE TIME SERIES ANALYSIS]
  ├─ setup_brms_lightweight() →                          [BRMS REORDERS DATA]
  ├─ generate_combined_stancode() →
  │   ├─ extract_trend_stanvars_from_setup() →
  │   │   ├─ generate_times_trend_matrices() →
  │   │   └─ extract mappings from dimensions.mappings →
  │   └─ inject_trend_into_linear_predictor() →         [USES obs_trend_time/obs_trend_series ARRAYS]
  └─ Stan compilation succeeds
```
