# Implementation Plan: Shared Gaussian Innovation System

## Context: mvgam Stan Compilation Pipeline Refactoring

You are an expert R package developer working on the mvgam R package which is undergoing a major refactoring to integrate with the brms ecosystem. The package generates Bayesian state-space models by combining brms observation models with custom trend specifications using Stan.

## Overview
Please read the context files in the "active/" directory. Your task is to create a unified system for handling Gaussian innovation parameters (sigma_trend, Sigma_trend, LV_innovations) that are common across most trend types, separating them from trend-specific parameters handled by individual constructors.

## Current State Analysis

### **Trend Types and Their Innovation Requirements:**
- **RW, AR, CAR, ZMVN**: Use Gaussian innovations (need shared system)  
- **VAR**: Innovations handled differently to maintain stationarity
- **PW**: Uses changepoint_scale (does NOT use shared system)
- **GP**: needs to be fully deprecated

### **Shared Parameters Needed:**
1. **`sigma_trend`**: Univariate innovation standard deviations
2. **`Sigma_trend`**: Multivariate innovation covariance matrix  
3. **`L_Omega_trend`**: Cholesky factor for correlation matrix
4. **`LV_innovations`**: Raw innovations (monitored only for MA models)

## Implementation Plan

### **Phase 1: Core Infrastructure**

#### **1.1 Create Shared Innovation Functions**
**File**: `R/stan_assembly.R` - Add in dedicated section for shared innovation functions

```r
# =============================================================================
# SHARED GAUSSIAN INNOVATION SYSTEM
# =============================================================================
# WHY: Most trend types (RW, AR, VAR, CAR, ZMVN) use Gaussian innovations with
# common parameters (sigma_trend, Sigma_trend, LV_innovations). This system 
# provides unified generation of these shared stanvars to avoid duplication
# across trend-specific generators and ensure consistent naming/structure.

#' Generate Shared Gaussian Innovation Parameters
#'
#' Creates stanvar objects for parameters common to Gaussian innovation trends.
#' Handles univariate/multivariate cases and factor models automatically.
#'
#' @param n_lv Number of latent variables  
#' @param n_series Number of time series
#' @param cor Logical, whether to include correlation parameters
#' @param ma Logical, whether to monitor LV_innovations (for MA models)
#' @param factor_model Logical, whether this is a factor model
#' @return List of stanvar objects for shared parameters
#' @noRd
generate_shared_innovation_stanvars <- function(n_lv, n_series, cor = FALSE, 
                                               ma = FALSE, factor_model = FALSE) {
  
  stanvars <- list()
  
  # Determine effective dimension for innovations
  effective_dim <- if (factor_model) n_lv else n_series
  
  # 1. sigma_trend - always needed (array form for consistency)
  if (effective_dim == 1) {
    sigma_code <- "vector<lower=0>[1] sigma_trend;"
  } else {
    sigma_code <- paste0("vector<lower=0>[", effective_dim, "] sigma_trend;")
  }
  
  stanvars$sigma_trend <- brms::stanvar(
    name = "sigma_trend",
    scode = sigma_code,
    block = "parameters"
  )
  
  # 2. Correlation parameters (only if cor = TRUE and multivariate)
  if (cor && effective_dim > 1) {
    # Cholesky factor for correlation matrix
    stanvars$L_Omega_trend <- brms::stanvar(
      name = "L_Omega_trend", 
      scode = paste0("cholesky_factor_corr[", effective_dim, "] L_Omega_trend;"),
      block = "parameters"
    )
    
    # Derived covariance matrix in transformed parameters
    sigma_matrix_code <- paste0(
      "matrix[", effective_dim, ", ", effective_dim, "] Sigma_trend = ",
      "diag_pre_multiply(sigma_trend, L_Omega_trend);"
    )
    
    stanvars$Sigma_trend <- brms::stanvar(
      name = "Sigma_trend",
      scode = sigma_matrix_code, 
      block = "transformed parameters"
    )
  }
  
  # 3. LV_innovations - raw innovations (monitoring controlled by ma flag)
  monitor_innovations <- ma  # Only monitor for MA models
  
  stanvars$LV_innovations <- brms::stanvar(
    name = "LV_innovations",
    scode = paste0("matrix[n, ", effective_dim, "] LV_innovations;"),
    block = "parameters",
    monitor = monitor_innovations
  )
  
  return(stanvars)
}

#' Generate Standard Priors for Gaussian Innovations
#'
#' Creates standard priors for shared Gaussian innovation parameters.
#'
#' @param n_lv Number of latent variables
#' @param cor Logical, whether correlation parameters exist
#' @return Stanvar object with prior code
#' @noRd
generate_innovation_priors <- function(n_lv, cor = FALSE) {
  
  prior_code <- c(
    "// Shared Gaussian innovation priors",
    "sigma_trend ~ exponential(1);"
  )
  
  if (cor && n_lv > 1) {
    prior_code <- c(prior_code,
      "L_Omega_trend ~ lkj_corr_cholesky(2);"
    )
  }
  
  prior_code <- c(prior_code,
    "to_vector(LV_innovations) ~ std_normal();"
  )
  
  brms::stanvar(
    name = "innovation_priors",
    scode = paste(prior_code, collapse = "\n  "),
    block = "model"
  )
}
```

#### **1.2 Update Stan Assembly Dispatcher**
**File**: `R/stan_assembly.R` - Update main dispatcher to integrate shared innovations

```r
# Update existing generate_trend_injection_stanvars() function
generate_trend_injection_stanvars <- function(trend_spec, data_info) {
  
  # Get trend-specific generator
  trend_info <- get_trend_info(trend_spec$trend)
  
  # Call trend-specific generator  
  stanvars <- trend_info$generator(trend_spec, data_info)
  
  # Add shared innovations if needed (all trends except PW)
  if (trend_spec$shared_innovations %||% TRUE) {  # Default TRUE except for PW
    shared_stanvars <- generate_shared_innovation_stanvars(
      n_lv = data_info$n_lv,
      n_series = data_info$n_series,
      cor = trend_spec$cor,
      ma = trend_spec$ma,
      factor_model = !is.null(trend_spec$n_lv) && trend_spec$n_lv < data_info$n_series
    )
    
    # Add shared priors
    prior_stanvars <- generate_innovation_priors(
      n_lv = data_info$n_lv,
      cor = trend_spec$cor
    )
    
    shared_stanvars$priors <- prior_stanvars
    stanvars <- combine_stanvars(stanvars, shared_stanvars)
  }
  
  return(stanvars)
}
```

### **Phase 2: Integration with Existing Constructors**

#### **2.1 Update Constructor Pattern**
**File**: `R/trend_system.R` - Update constructors to use shared innovation flag

Each constructor will call the shared system and combine with trend-specific parameters:

```r
# Example: Updated RW constructor pattern
RW <- function(...) {
  # Process trend-specific parameters (MA terms only)
  param_specs <- if (ma) {
    trend_param("theta1", bounds = c(-1, 1), label = "moving_average_coefficient_lag_1")
  } else {
    NULL
  }
  
  processed <- process_trend_params(param_specs)
  
  # Build trend object (shared innovation params added later by generators)
  structure(list(
    trend = "RW",
    # ... other fields ...
    tpars = processed$tpars,  # Only trend-specific parameters
    bounds = processed$bounds,
    shared_innovations = TRUE  # Flag for stan_assembly.R generators
  ), class = "mvgam_trend")
}
```

#### **2.2 Update Individual Stan Generators**
**File**: `R/trend_injection_generators.R` - Modify existing generators to remove duplicate innovation code

```r
# Example: Updated RW generator pattern  
generate_rw_stanvars <- function(trend_spec, data_info) {
  
  # Only generate RW-specific dynamics
  # Shared innovations (sigma_trend, LV_innovations, etc.) handled by stan_assembly.R
  
  result_stanvars <- list()
  
  # Add RW-specific dynamics (if any)
  if (trend_spec$ma) {
    # Add MA-specific Stan code here
    ma_code <- "// RW with MA(1) dynamics
    for (i in 2:n) {
      for (j in 1:n_lv) {
        LV[i, j] = LV[i-1, j] + LV_innovations[i, j] + theta1_trend * LV_innovations[i-1, j];
      }
    }"
    
    result_stanvars$rw_ma_dynamics <- brms::stanvar(
      name = "rw_ma_dynamics",
      scode = ma_code,
      block = "transformed parameters"
    )
  } else {
    # Basic RW dynamics
    basic_rw_code <- "// Basic RW dynamics
    for (i in 2:n) {
      for (j in 1:n_lv) {
        LV[i, j] = LV[i-1, j] + LV_innovations[i, j];
      }
    }"
    
    result_stanvars$rw_dynamics <- brms::stanvar(
      name = "rw_dynamics", 
      scode = basic_rw_code,
      block = "transformed parameters"
    )
  }
  
  return(result_stanvars)
}
```

### **Phase 3: PW Exception Handling**

#### **3.1 PW Constructor Flag**
**File**: `R/trend_system.R` - Update PW constructor to explicitly opt out

```r
PW <- function(...) {
  # PW uses changepoint_scale, not shared Gaussian innovations
  structure(list(
    trend = if (growth == "linear") "PWlinear" else "PWlogistic",
    # ... other fields ...
    shared_innovations = FALSE,  # PW doesn't use shared system
    tpars = character(0),        # No trend-specific parameters beyond changepoint_scale
    bounds = list()
  ), class = "mvgam_trend")
}
```

#### **3.2 PW Generator**
**File**: `R/trend_injection_generators.R` - Ensure PW generator handles its own parameters

```r
generate_pw_stanvars <- function(trend_spec, data_info) {
  # PW handles its own changepoint_scale parameter
  # Does not use shared Gaussian innovation system
  
  # Generate PW-specific parameters and dynamics
  # ... existing PW implementation ...
}
```

#### **3.2 VAR Constructors and Generators**
Similar to PW above, should opt out of share innovations

### **Phase 4: Testing Integration (30 min)**

#### **4.1 Add Tests for Shared System**
**File**: `tests/testthat/test-stan-assembly.R` - Create new test file for stan_assembly functions

```r
test_that("shared Gaussian innovation system works correctly", {
  # Test shared parameter generation
  shared_stanvars <- mvgam:::generate_shared_innovation_stanvars(
    n_lv = 3, n_series = 2, cor = TRUE, ma = FALSE, factor_model = TRUE
  )
  
  expect_true("sigma_trend" %in% names(shared_stanvars))
  expect_true("L_Omega_trend" %in% names(shared_stanvars))
  expect_true("Sigma_trend" %in% names(shared_stanvars))
  expect_true("LV_innovations" %in% names(shared_stanvars))
  
  # Test MA monitoring flag
  shared_ma <- mvgam:::generate_shared_innovation_stanvars(
    n_lv = 2, n_series = 2, cor = FALSE, ma = TRUE, factor_model = FALSE
  )
  
  # LV_innovations should be monitored for MA models
  expect_true(shared_ma$LV_innovations$monitor)
})

test_that("shared innovation priors generate correctly", {
  # Test basic priors
  priors_basic <- mvgam:::generate_innovation_priors(n_lv = 1, cor = FALSE)
  expect_s3_class(priors_basic, "stanvar")
  expect_true(grepl("sigma_trend.*exponential", priors_basic$scode))
  
  # Test correlated priors  
  priors_cor <- mvgam:::generate_innovation_priors(n_lv = 3, cor = TRUE)
  expect_true(grepl("L_Omega_trend.*lkj_corr_cholesky", priors_cor$scode))
})
```

#### **4.2 Update Existing Tests**
**File**: `tests/testthat/test-trend-registry.R` - Add tests for shared_innovations flag

```r
test_that("constructors set shared_innovations flag correctly", {
  suppressWarnings({
    # Most trends should use shared innovations
    rw_trend <- RW()
    expect_true(rw_trend$shared_innovations %||% TRUE)
    
    ar_trend <- AR(p = 1)
    expect_true(ar_trend$shared_innovations %||% TRUE)
    
    var_trend <- VAR(p = 1)
    expect_true(var_trend$shared_innovations %||% TRUE)
    
    # PW should opt out
    pw_trend <- PW(growth = "linear")
    expect_false(pw_trend$shared_innovations %||% TRUE)
  })
})
```

## Implementation Order

### **Priority 1: Core Functions**
1. Add shared innovation functions to `R/stan_assembly.R`
2. Update main dispatcher in `R/stan_assembly.R`
3. Add basic tests in `tests/testthat/test-stan-assembly.R`

### **Priority 2: Constructor Integration**
1. Update RW constructor as prototype in `R/trend_system.R`
2. Update AR constructor in `R/trend_system.R`
3. Update VAR constructor in `R/trend_system.R`  
4. Update CAR constructor in `R/trend_system.R`
5. Add shared_innovations flag tests

### **Priority 3: Generator Updates**
1. Update individual generators in `R/trend_injection_generators.R`
2. Remove duplicate innovation code from existing generators
3. Add PW exception handling

### **Priority 4: Testing & Validation**
1. Run comprehensive tests
2. Verify parameter naming consistency  
3. Check Stan code compilation
4. Validate monitoring flags work correctly

## File Structure Changes

### **New/Modified Files:**
- **`R/stan_assembly.R`**: Add shared innovation functions and update dispatcher
- **`R/trend_system.R`**: Update constructors with shared_innovations flag
- **`R/trend_injection_generators.R`**: Update individual generators  
- **`tests/testthat/test-stan-assembly.R`**: New test file for shared system
- **`tests/testthat/test-trend-registry.R`**: Add shared_innovations flag tests

### **Key Functions Added:**
- **`generate_shared_innovation_stanvars()`**: Core shared parameter generation
- **`generate_innovation_priors()`**: Standard priors for shared parameters
- **Updated `generate_trend_injection_stanvars()`**: Integration point for shared system

## Benefits of This Design

1. **📦 Consolidation**: All Gaussian innovation parameters in one place (`R/stan_assembly.R`)
2. **🔧 Maintainability**: Changes to shared parameters affect all trends automatically  
3. **⚡ Performance**: Consistent parameter structure across trend types
4. **🧪 Testability**: Shared system can be tested independently
5. **📈 Extensibility**: New trend types get shared parameters automatically
6. **🎯 Clarity**: Clear separation between trend-specific and shared parameters
7. **🏗️ Architecture**: Follows existing pattern of Stan code in `stan_assembly.R`

## Risk Mitigation

- **Backward Compatibility**: No changes to user-facing constructor APIs
- **Gradual Rollout**: Can implement one constructor at a time
- **Testing Safety**: Comprehensive test coverage for each change  
- **PW Exception**: Clear handling for non-Gaussian innovation trends
- **File Organization**: Follows established pattern of Stan code in dedicated file

This plan provides a structured approach to implementing the shared Gaussian innovation system while maintaining the excellent test coverage and architecture we've established, with all Stan code properly organized in `R/stan_assembly.R`.
