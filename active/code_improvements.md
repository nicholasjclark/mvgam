# Code Improvements

## Purpose
Track medium and low priority improvements identified during code review that should be addressed in future refactoring passes.

## HIGH PRIORITY Items
*Critical issues that must be resolved immediately - NON-NEGOTIABLE*

(No active high priority items - resolved embedded family support issues as of 2025-08-26)

## Medium Priority Items
*Items that affect maintainability or could cause issues in edge cases*

### Trend Constructor Improvements (2025-08-15)
1. **CAR() parameter logic**: The `p` parameter in CAR() is constrained to only accept value 1. Consider removing the parameter entirely if CAR only supports p=1, or document why this constraint exists.
2. ~~**Function naming convention**: Trend constructors (CAR, ZMVN, AR, etc.) use UPPERCASE naming which conflicts with snake_case standard.~~ **RESOLVED**: UPPERCASE names are intentional for trend constructors as part of the API design.
3. **Roxygen2 documentation**: Simplified trend constructors need complete roxygen2 documentation with @param, @return, @export, and @examples tags.
4. **Test coverage**: Ensure existing tests are updated to reflect simplified constructor behavior.

### Validation Layer Enhancements (2025-08-15)
1. **Function decomposition**: `validate_and_process_trend_parameters()` has complex nested logic that could benefit from breaking down into smaller, focused functions.
2. **Test coverage expansion**: New validation functions need comprehensive test coverage for all parameter processing scenarios.
3. **Documentation enhancement**: Add more detailed roxygen2 documentation explaining parameter processing logic.

### Automated Registry System (2025-08-16)
1. **Edge case handling**: `auto_register_trend_types()` should handle empty R directory or missing function definitions more gracefully.
2. **Validation completeness**: `validate_trend_properties()` needs more comprehensive validation of properties list structure elements.
3. **Stan integration verification**: Verify registry-enhanced error messages integrate properly with existing Stan assembly code.

## Low Priority Items  
*Style improvements and minor optimizations*

### Prior System Enhancements (2025-08-16)
1. **Consider caching default priors**: Cache commonly used default prior specifications to avoid repeated computation for same trend types.
2. **Add internal bounds validation helper**: Create reusable function for validating prior parameter bounds across all prior specification functions.
3. **Improve variable naming**: Use more descriptive names in internal functions. Consider renaming generic variables like 'spec' to be more specific.

### Trend Constructor Enhancements (2025-08-15)
1. **Extract hardcoded values**: Consider extracting hardcoded `ma = FALSE`, `cor = FALSE/TRUE` to named constants if these patterns repeat across constructors.
2. **Inline documentation**: Add comments explaining why certain parameters are hardcoded (e.g., why CAR has `cor = FALSE`).

### Validation Layer Style (2025-08-15)
1. **Line length compliance**: Some lines in new validation functions exceed 80 character limit per tidyverse style guide.
2. **Documentation style**: Consider more detailed roxygen2 documentation explaining parameter processing logic.

### Automated Registry System Style (2025-08-16)
1. **Line length compliance**: Multiple lines in registry functions exceed 80-character limit per project standards.
2. **Documentation enhancement**: Registry functions need complete roxygen2 documentation with @param, @return, and @examples.
3. **Performance optimization**: Consider caching registry results to avoid repeated file system scans during auto-discovery.

### Prior System Improvements (2025-08-16)
1. **Add conflict validation in combine_obs_trend_priors()**: Check for duplicate class/coef combinations when combining priors. Warn or error if conflicts detected.
2. **Add bounds checking for prior parameters**: Validate distribution parameters (e.g., sigma > 0) when implementing set_prior().
3. **Expand roxygen2 documentation**: Add @examples sections to user-facing functions when implemented. Provide more detailed @param descriptions with types and constraints.
4. **Create comprehensive unit tests**: Test all helper functions with valid/invalid inputs, trend-specific prior generators, and prior combination logic.

### Test File Standards (2025-08-16)
1. **NEVER use library() calls in test files**: Test files should not include `library(testthat)` or `library(mvgam)` calls. The testthat framework and package under test are automatically available during testing.
2. **Test isolation**: Each test should be self-contained without relying on package loading statements.
3. **Setup dependencies**: Use the `tests/testthat/setup.R` file for any necessary test-wide configuration, not individual test files.
4. **NEVER use skip() in tests**: This is NON-NEGOTIABLE. All tests must run and pass. If functionality doesn't exist, write tests that reflect the current state or fix the underlying code.

### Parameter Standardization Architecture (2025-08-20)
1. **Dimension Parameter Behavior**: The current architecture for dimension parameters is INTENTIONAL and should be maintained:
   - **Factor models**: Use both `n_series_trend` (observed series) and `n_lv_trend` (latent factors) where `n_lv_trend < n_series_trend`
   - **Non-factor models**: Use `n_lv_trend` for all trend parameters with `n_lv_trend = n_series_trend` for consistency
   - **Final trend matrix**: Always `matrix[n_trend, n_series_trend]` to match observed series in observation model
   - **Matrix Z mapping**: `matrix[n_series_trend, n_lv_trend]` maps latent factors to observed series
   - **Rationale**: This provides consistent parameter indexing across all trend generators while maintaining proper dimensional relationships for factor vs non-factor models.

## Completed Improvements
*Archive of addressed items for reference*

### Embedded Family Support Resolution (2025-08-26)
1. **✅ RESOLVED**: API stability risk - current implementation uses only public brms APIs (`brms::get_prior()`)
2. **✅ RESOLVED**: Parameter validation - comprehensive `checkmate::assert_*()` validation implemented in all helper functions
3. **✅ RESOLVED**: Internal API usage - no `brms:::` usage found in current implementation
4. **✅ RESOLVED**: Logic robustness - no `mapply()` usage found, using safe alternatives