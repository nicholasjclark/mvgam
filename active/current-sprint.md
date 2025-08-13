# Current Sprint: Comprehensive Stan Compilation Validation (Week 7)

## Objectives

### Phase 1: Testing Simple Models

**Pre-Extension Function Updates**
- [ ] **PRIORITY**: Trend pipeline infrastructure dispatch simplification - trend constructors should return ALL necessary information for pointing to the correct trend stanvar functions. This should trigger:
    1. The correct time, series, gr and subgr validation to use
    2. Whether to use hierarchical trend components, factors and shared gaussian innovations
    3. Whether to add response suffixes (if for example the constructor is called in a named list of formulae)
    4. Storage that can be used by the final `mvgam` model to determine what parameters to monitor, what forecasting function to use, what labels to use in summaries and any other post-processing steps
- [ ] **PRIORITY**: Implement `fit_mvgam_model()` - currently placeholder with `stop("This function is not yet operational")`
- [ ] **PRIORITY**: Implement `subset_stanfit_parameters()` - currently placeholder, needed for dual object system  
- [ ] **PRIORITY**: Implement `extract_posterior_samples()` - missing function called by multiple imputation system

**1.1 Univariate Model Testing**
- [x] Architecture validated: Tests now reach Stan template generation phase (67% pass rate)
- [x] Dimension validation: "Missing dimension information" errors eliminated
- [x] Trend type normalization: AR1 → AR, VAR2 → VAR mapping working
- [ ] Stan template syntax: Fix glue template `Expecting '}'` errors in AR trends
- [ ] Variable scoping: Resolve `n_trend`, `n`, `n_lv_trend` declaration issues in generated Stan code

**1.2 Factor Model Compatibility Validation**  
- [x] Registry integration working with proper trend detection
- [ ] Complete n_lv parameter validation in factor model tests
- [ ] Incompatible trends (CAR, PW): Verify rejection with informative errors

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
**Objective**: Validate challenging scenarios

**3.1 Missing Data and Irregular Time Series**
- [ ] Missing observation patterns across series
- [ ] Irregular time intervals with CAR models
- [ ] Trend evolution over all timesteps including missing data

**3.2 Complex Formula Integration**
- [ ] Multiple smooth terms: `y ~ s(x1) + s(x2) + te(x3, x4)`
- [ ] Random effects: `y ~ (1|group) + s(time)`
- [ ] Interaction terms: `y ~ x1 * x2 + s(x3, by = factor)`

## Implementation Strategy

### Testing Framework Architecture
**Test Data**: Use `mvgam:::example_data` for consistent baseline testing
**Validation**: Leverage `validate_stan_code()` for comprehensive Stan compilation checks
**Coverage**: Systematic matrix of trend types × formula patterns × model types
**Performance**: Benchmark compilation times and registry lookup efficiency

### Success Criteria
- [x] **Architectural Foundation**: Standardized trend_specs structure and dimension validation pipeline (67% test pass rate achieved)
- [ ] **Stan Template Fixes**: Resolve glue syntax errors and variable scoping issues
- [ ] **Universal Compilation**: All trend types compile with basic formulas  
- [x] **Factor Model Validation**: Registry integration and compatibility checking working
- [x] **brms Integration**: Tests reach Stan generation phase, core pipeline functional (89% assembly test pass rate)
- [x] **Performance Targets**: Registry and validation performing efficiently  
- [ ] **Edge Case Handling**: Missing data and irregular timing supported

## ✅ RESOLVED: Formula Parsing Pipeline Break

**Solution**: `parse_multivariate_trends()` now calls `parse_trend_formula()` to return proper `mvgam_trend` objects instead of raw formulas

**Current Focus**: Stan template syntax errors and variable scoping issues in generated code

## Next Phase Preview (Week 8+)
**Post-Template-Fix Priorities**:
- Complete placeholder function implementations (`fit_mvgam_model()`, `subset_stanfit_parameters()`, `extract_posterior_samples()`)
- User extension system documentation and examples
- Performance optimization based on benchmarking results
- Advanced edge case handling (complex missing data patterns)
- Real-world model fitting validation with large datasets: update `fit_mvgam_model()` to actually fit models and `extract_trend_parameters()` to monitor the correct parameters
