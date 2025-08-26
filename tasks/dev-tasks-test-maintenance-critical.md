# TRD-test-maintenance-critical Development Tasks

## Overview
Critical remaining test failures that must be resolved to achieve 100% test pass rate. After fixing 13 of 22 failures, 9 critical issues remain that require investigation and proper fixes.

## Current Status
- ✅ **Major Test Fixes Complete**: 22 failures → 9 failures (59% reduction, 98.3% pass rate)
- ❗ **Remaining Critical Issues**: 9 test failures requiring proper implementation fixes

---

## REMAINING CRITICAL FAILURES (9 total)

### 1.0 AR Validation Error Message Fix
**Status**: ❌ **CRITICAL - Implementation Gap**
**Issue**: `generate_ar_trend_stanvars()` validation changed but test expects old error message
**Location**: `tests/testthat/test-priors.R:1046`
**Error**: Expected "not >= 1" but gets "Must have length 1"

**Root Cause**: Validation now checks vector length before value range
**Required Fix**: Either:
- [ ] Update validation to provide expected error message for negative values
- [ ] Update test to expect correct error message from current validation

### 2.0 ZMVN Transformed Parameters Structure Issue  
**Status**: ❌ **CRITICAL - Architecture Change**
**Issue**: `result[["zmvn_tparameters"]]` is not an S3 object as expected
**Location**: `tests/testthat/test-priors.R:1208` 
**Error**: `tparams_stanvar` fails `expect_s3_class(*, "stanvar")`

**Root Cause**: ZMVN generator may have changed stanvar structure during 3-stanvar pattern migration
**Required Investigation**:
- [ ] Examine actual `generate_zmvn_trend_stanvars()` output structure
- [ ] Verify if zmvn_tparameters should be stanvar object or different structure
- [ ] Update test expectations to match current architecture

### 3.0 Trend Constructor Evaluation Failures (4 failures)
**Status**: ❌ **CRITICAL - Missing Implementation**  
**Issue**: "Failed to evaluate trend constructor" errors during `extract_trend_priors()`
**Locations**: Lines 1962, 1970, 2127, 2134
**Error**: Generic evaluation failure in trend parsing system

**Root Cause Analysis Needed**:
- [ ] **Identify failing trend types**: Which specific trend constructors in test lists fail?
- [ ] **Parameter mismatch investigation**: Do trend constructors expect different parameters?
- [ ] **Missing trend generators**: Are some trend types partially implemented?
- [ ] **Constructor signature changes**: Have trend constructor APIs changed?

**Investigation Tasks**:
- [ ] Run individual trend type tests to isolate failures
- [ ] Check `eval_trend_constructor()` function for issues
- [ ] Verify trend constructor parameter compatibility
- [ ] Check if CAR, PW, or hierarchical trend variations have issues

### 4.0 NULL Family Parameter Integration Issues (2 failures)
**Status**: ❌ **CRITICAL - Integration Bug**
**Issue**: Family parameter is NULL instead of expected family class
**Locations**: Lines 2222, 2225
**Error**: `family` parameter validation fails in integration tests

**Root Cause**: Complex integration test scenarios not properly passing family parameters
**Required Investigation**:
- [ ] Trace family parameter flow in `extract_trend_priors()` integration scenarios
- [ ] Verify family parameter handling in multivariate contexts
- [ ] Check if family defaults are properly applied
- [ ] Ensure family parameter compatibility with trend model integration

---

## IMPLEMENTATION PRIORITIES

### Priority 1: CRITICAL (Must Fix Immediately)
1. **Trend Constructor Evaluation** (4 failures) - Core functionality broken
2. **ZMVN Structure Issue** (1 failure) - Architecture validation problem
3. **NULL Family Integration** (2 failures) - Integration system broken

### Priority 2: HIGH (Fix Before Release)
4. **AR Validation Message** (1 failure) - User-facing error message consistency

---

## INVESTIGATION STRATEGY

### Phase 1: Diagnostic Deep Dive
1. **Run Individual Trend Tests**: Isolate which specific trend types fail constructor evaluation
2. **ZMVN Structure Analysis**: Examine actual output of `generate_zmvn_trend_stanvars()`
3. **Family Parameter Tracing**: Debug family parameter flow in integration scenarios

### Phase 2: Targeted Fixes
1. **Fix Missing/Broken Trend Constructors**: Address identified constructor issues
2. **Update ZMVN Test Expectations**: Align tests with current architecture
3. **Repair Family Parameter Flow**: Ensure proper family handling in all scenarios
4. **Standardize Error Messages**: Update AR validation for consistency

### Phase 3: Verification
1. **Complete Test Suite**: Achieve 100% pass rate
2. **Integration Testing**: Verify fixes don't break other functionality
3. **Regression Testing**: Ensure embedded family support (Sub-task 1E) still works

---

## SUCCESS CRITERIA

- [ ] **0 test failures** in `test-priors.R`
- [ ] **All trend constructors functional**: RW, AR, VAR, CAR, ZMVN, PW working
- [ ] **ZMVN architecture compliance**: Tests match current 3-stanvar pattern
- [ ] **Family parameter flow**: Proper handling in all integration scenarios
- [ ] **Error message consistency**: User-facing errors are clear and accurate

## RISK ASSESSMENT

**HIGH RISK**: Trend constructor evaluation failures suggest core functionality gaps that could impact user-facing features.

**MEDIUM RISK**: ZMVN and family parameter issues indicate architectural inconsistencies that need resolution.

**LOW RISK**: AR error message is cosmetic but important for user experience.

---

## COMPLETION TIMELINE

**Target**: All failures resolved within 1-2 development sessions
**Estimated Effort**: 3-4 hours total
- Investigation: 1 hour
- Implementation: 2 hours  
- Testing/Verification: 1 hour

This task list ensures no functionality is skipped and all test failures receive proper investigation and fixes.