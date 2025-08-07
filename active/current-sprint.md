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

## Week 7 Objectives

### Phase 1: Testing Simple Models

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
