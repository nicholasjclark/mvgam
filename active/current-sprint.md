# Current Sprint: Comprehensive Stan Compilation Validation (Week 7)

**Status**: Week 6 COMPLETED ✅  
**Goal**: Systematic validation of Stan compilation across all trend types with complex brms integration patterns
**Foundation**: Registry-based Stan assembly system operational with 98.8% test pass rate

## Previous Sprint Summary (Weeks 5-6) ✅ **COMPLETED**

### Major Achievements
- **Two-Stage Stan Assembly System**: Complete Stan code generation operational
- **Registry Architecture**: Centralized trend registration with factor compatibility validation  
- **CAR Integration**: Continuous-time autoregressive model fully implemented
- **Code Consolidation**: 13 files consolidated into 4 thematic modules for developer onboarding
- **Unified Validation**: Single `validate_stan_code()` function using `rstan::stanc()` directly

### Core Infrastructure Complete ✅
- Single-fit dual-object system with brms ecosystem integration
- Multiple imputation with Rubin's rules pooling  
- Formula validation with autocorrelation separation
- Factor model architecture with shared utility functions
- Hierarchical correlation support across compatible trends

**Consistent, standardized stanvar generators for trend types**:
- ✅ `generate_rw_trend_stanvars()`
- ✅ `generate_var_trend_stanvars()`
- ✅ `generate_ar_trend_stanvars()`
- ✅ `generate_car_trend_stanvars()`
- ✅ `generate_zmvn_trend_stanvars()`
- ✅ `generate_pw_trend_stanvars()`

## 🚨 CRITICAL: Stan Compilation Test Results (January 2025)

### Test Execution Summary
**Status**: 58% Pass Rate (26/45 tests passing)  
**Critical Finding**: Systematic failures in Stan code generation pipeline

### High Priority Issues Identified

#### 1. **Duplicate Parameter Names** (CRITICAL - Blocks all RW models)
- **Issue**: RW trend declares `sigma` conflicting with brms gaussian family's `sigma`
- **Error**: "Identifier 'sigma' is already in use"
- **Fix**: Rename to `sigma_trend` or `sigma_lv` per architecture decisions
- **Impact**: All RW trend models with gaussian family failing

#### 2. **Invalid Nested Parameters Blocks** (CRITICAL - Structural issue)
- **Issue**: Stan assembly creating `parameters { ... parameters { ... } }` nesting
- **Error**: "Invalid nested 'parameters' blocks"
- **Root Cause**: Stanvar injection logic incorrectly handling block boundaries
- **Impact**: AR, VAR full pipeline tests failing

#### 3. **Missing Data in Stanvars** (HIGH - Blocks ZMVN/hierarchical)
- **Issue**: Data block stanvars missing required `x` parameter
- **Error**: "Argument 'x' is required if block = 'data'"
- **Fix**: Add actual data values when creating data block stanvars
- **Impact**: ZMVN trends and hierarchical correlations broken

#### 4. **Invalid Stanvars Objects** (HIGH - Basic functionality)
- **Issue**: `generate_trend_injection_stanvars()` returning invalid objects
- **Error**: "Argument 'stanvars' is invalid"
- **Root Cause**: Stanvar combination not preserving brms class requirements
- **Impact**: 6 basic trend tests failing

#### 5. **Stanvar Class Validation** (MEDIUM - Factor models)
- **Issue**: `combine_stanvars()` rejecting valid combinations
- **Error**: "All stanvars must have class 'stanvar' or 'stanvars'"
- **Fix**: Handle mixed stanvar/stanvars objects properly
- **Impact**: Factor model tests failing

### Immediate Action Plan
1. **Fix parameter naming**: Update all `sigma` → `sigma_trend` in trend generators
2. **Debug block nesting**: Fix stanvar injection to prevent nested blocks
3. **Fix data stanvars**: Add proper `x` parameters with data
4. **Review combine_stanvars**: Ensure proper class handling

## Week 7 Deliverables: Systematic Stan Compilation Testing

### Phase 1: Core Trend Type Validation (BLOCKED BY ABOVE ISSUES)
**Objective**: Validate Stan compilation for all registered trend types with comprehensive formula patterns

**1.1 Univariate Model Testing**
- [ ] Simple intercept models: `y ~ 1, trend_formula = ~ [TREND]()`
- [ ] Covariate integration: `y ~ s(x1) + x2, trend_formula = ~ [TREND]()`
- [ ] Family compatibility: `gaussian()`, `poisson()`, `binomial()` families
- [ ] All trend types: AR, RW, VAR, CAR, ZMVN, PW

**1.2 Factor Model Compatibility Validation**
- [ ] Compatible trends (AR, RW, VAR, ZMVN): Test with `n_lv = 2, 3`
- [ ] Matrix Z patterns: Factor vs non-factor model Stan code generation
- [ ] Incompatible trends (CAR, PW): Verify rejection with informative errors
- [ ] Edge case: `n_lv >= n_series` (diagonal Z matrix validation)

**1.3 Hierarchical Correlation Testing**
- [ ] Supported trends (AR, VAR, CAR, ZMVN): Test with `gr` parameter
- [ ] Group-specific correlation matrices with partial pooling
- [ ] Non-supported trends: Verify warnings about ignoring correlations

### Phase 2: brms Integration Pattern Validation
**Objective**: Test Stan compilation with complex brms features and formula combinations

**2.1 Distributional Model Integration**
- [ ] Trends only on `mu`: `bf(y ~ s(x), sigma ~ s(z)), trend_formula = ~ AR(p = 1)`
- [ ] Auxiliary parameter rejection: Verify trends rejected on `sigma`, `zi`, `hu`
- [ ] Complex distributional families: Zero-inflated, hurdle, beta models

**2.2 Multivariate Model Patterns**
- [ ] Response-specific trends: `bf(count = ~ AR(p = 1), biomass = ~ RW())`
- [ ] Mixed specifications: Some responses with trends, others without
- [ ] Different families per response with proper parameter naming

**2.3 Observation-Level Correlation Separation**
- [ ] Combined patterns: `y ~ Trt + unstr(visit, patient), trend_formula = ~ AR(p = 1)`
- [ ] Autocorrelation conflict detection: Prevent brms `ar()` in trend formulas
- [ ] Residual vs state-space correlation verification

### Phase 3: Edge Cases and Performance Benchmarking
**Objective**: Validate challenging scenarios and measure compilation performance

**3.1 Missing Data and Irregular Time Series**
- [ ] Missing observation patterns across series
- [ ] Irregular time intervals with CAR models
- [ ] Trend evolution over all timesteps including missing data

**3.2 Complex Formula Integration**
- [ ] Multiple smooth terms: `y ~ s(x1) + s(x2) + te(x3, x4)`
- [ ] Random effects: `y ~ (1|group) + s(time)`
- [ ] Interaction terms: `y ~ x1 * x2 + s(x3, by = factor)`

**3.3 Performance Validation**
- [ ] Stan compilation times across trend types
- [ ] Registry lookup performance (<1ms target)
- [ ] Memory usage with large datasets
- [ ] Two-stage assembly efficiency benchmarks

## Implementation Strategy

### Testing Framework Architecture
**Test Data**: Use `mvgam:::example_data` for consistent baseline testing
**Validation**: Leverage `validate_stan_code()` for comprehensive Stan compilation checks
**Coverage**: Systematic matrix of trend types × formula patterns × model types
**Performance**: Benchmark compilation times and registry lookup efficiency

### Success Criteria
- [ ] **Universal Compilation**: All trend types compile with basic formulas
- [ ] **Factor Model Validation**: Proper rejection/acceptance patterns confirmed
- [ ] **brms Integration**: Complex brms features work with trend injection
- [ ] **Performance Targets**: Registry <1ms, compilation efficiency maintained
- [ ] **Edge Case Handling**: Missing data and irregular timing supported

## ⚠️ DISCOVERED: Parameter Naming Conflict Issue (January 2025)

### Issue Summary
**Status**: IDENTIFIED - Requires fixing in all trend generators  
**Priority**: HIGH - Causes Stan compilation failures  
**Discovery**: Found during Stan compilation validation testing

### Problem Description
Trend generators are declaring variance parameters (e.g., `sigma`) that conflict with brms family parameters, causing Stan compilation errors: "Identifier 'sigma' is already in use."

### Required Fix
**Naming Convention** - All trend variance parameters must use descriptive suffixes:
- `sigma` → `sigma_trend` or `sigma_lv`
- `Sigma` → `Sigma_trend`
- Existing good patterns: `sigma_lv`, `car_sigma`, `L_Omega`

**Action Items**:
1. Update RW generator: `sigma` → `sigma_trend`
2. Review all trend generators for parameter naming conflicts
3. Update tests to catch naming conflicts early
4. Document standard parameter naming in developer guide

### Impact
- Tests currently failing for RW trend with gaussian family
- Similar conflicts likely exist in other trend/family combinations
- Easy fix but requires systematic review of all generators

## Core Architecture (Operational ✅)

### Key Components
- **Two-Stage Stan Assembly**: brms base + mvgam trend injection system
- **Registry Architecture**: Centralized trend type management with factor compatibility
- **Validation Framework**: Unified `validate_stan_code()` using `rstan::stanc()`
- **Shared Utilities**: Code deduplication for factor models and hierarchical correlations

### Consolidated File Structure
- `R/trend_system.R` - Complete trend infrastructure
- `R/stan_assembly.R` - Two-stage Stan assembly orchestration  
- `R/brms_integration.R` - Enhanced brms setup and integration
- `R/mvgam_core.R` - Fitting, dual-object system, multiple imputation

## Next Phase Preview (Week 8+)
**Post-Validation Priorities**:
- User extension system documentation and examples
- Performance optimization based on benchmarking results
- Advanced edge case handling (complex missing data patterns)
- Real-world model fitting validation with large datasets
