# Code Improvements

## Purpose
Track medium and low priority improvements identified during code review that should be addressed in future refactoring passes.

## Medium Priority Items
*Items that affect maintainability or could cause issues in edge cases*

### Trend Constructor Improvements (2025-08-15)
1. **CAR() parameter logic**: The `p` parameter in CAR() is constrained to only accept value 1. Consider removing the parameter entirely if CAR only supports p=1, or document why this constraint exists.
2. ~~**Function naming convention**: Trend constructors (CAR, ZMVN, AR, etc.) use UPPERCASE naming which conflicts with snake_case standard.~~ **RESOLVED**: UPPERCASE names are intentional for trend constructors as part of the API design.
3. **Roxygen2 documentation**: Simplified trend constructors need complete roxygen2 documentation with @param, @return, @export, and @examples tags.
4. **Test coverage**: Ensure existing tests are updated to reflect simplified constructor behavior.

## Low Priority Items  
*Style improvements and minor optimizations*

### Trend Constructor Enhancements (2025-08-15)
1. **Extract hardcoded values**: Consider extracting hardcoded `ma = FALSE`, `cor = FALSE/TRUE` to named constants if these patterns repeat across constructors.
2. **Inline documentation**: Add comments explaining why certain parameters are hardcoded (e.g., why CAR has `cor = FALSE`).

## Completed Improvements
*Archive of addressed items for reference*

(To be populated as items are completed)