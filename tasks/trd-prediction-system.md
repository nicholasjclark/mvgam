# Task Requirements Document: mvgam Prediction System

## 1. Task Overview

**Purpose**: Implement a comprehensive prediction system for mvgam that provides full compatibility with brms prediction methods while properly handling State-Space trend components. This enables users to generate predictions, expected values, and posterior predictive samples with proper uncertainty quantification.

**Scope**: This task covers the core prediction infrastructure for mvgam models:
- Model prediction functions (posterior_linpred, posterior_epred, posterior_predict)
- Specialized extraction functions (posterior_smooths, posterior_average)
- Convenience wrappers (predict.mvgam, fitted.mvgam)
- Integration with marginaleffects package
- Enhancement of conditional_effects() for State-Space models

**User Type**: R users ranging from intermediate (familiar with brms and Bayesian workflows) to advanced (implementing custom prediction workflows). Users should understand basic State-Space modeling concepts but should not need to understand Stan or the internal architecture.

---

## 2. User Journey

### Step 1: Fit a State-Space Model
Users have already fitted an mvgam model with observation and trend components:
```r
# Example fitted model
fit <- mvgam(
  formula = count ~ temperature + s(day_of_year),
  trend_formula = ~ AR(p = 1, cor = TRUE),
  data = my_data,
  family = poisson()
)
```

### Step 2: Generate Predictions
Users want predictions for new data or in-sample fitted values:

**Decision Point A**: What type of prediction?
- **Linear predictor** (link scale) → Use `posterior_linpred()`
- **Expected value** (response scale) → Use `posterior_epred()`
- **New observations** (with sampling variability) → Use `posterior_predict()`

**Decision Point B**: Include trend uncertainty?
- **Yes** (default): `process_error = TRUE` accounts for uncertainty in trend parameters
- **No**: `process_error = FALSE` uses posterior mean trend parameters (faster)

**Decision Point C**: How many posterior draws?
- **All draws** (default): `ndraws = NULL` uses all MCMC samples
- **Subset**: `ndraws = 100` randomly samples 100 draws for speed

### Step 3: Prepare newdata
Users create a data frame with all required covariates:
```r
newdata <- data.frame(
  temperature = c(20, 25, 30),
  day_of_year = c(150, 151, 152),
  time = c(51, 52, 53),        # Required for State-Space models
  series = c("A", "A", "A")     # Required if series variable used
)
```

**Common Mistake**: Forgetting time/series variables for State-Space models
**System Response**: Clear error message indicating required variables

### Step 4: Generate and Use Predictions
```r
# Generate predictions
pred <- posterior_epred(fit, newdata = newdata, ndraws = 500)

# Result is a matrix: 500 draws × 3 observations
# Users can compute summaries
mean_pred <- colMeans(pred)
ci_pred <- apply(pred, 2, quantile, probs = c(0.025, 0.975))
```

### Step 5: Advanced Workflows
**Alternative Path A: marginaleffects Integration**
```r
library(marginaleffects)

# Compute marginal effects automatically
mfx <- slopes(fit, newdata = newdata, variables = "temperature")
plot(mfx)
```

**Alternative Path B: Visualization**
```r
# Enhanced conditional effects
ce <- conditional_effects(fit, effects = "temperature")
plot(ce)
```

**Alternative Path C: Random Effects Manipulation**
```r
# Predictions without random effects (population-level)
pred_pop <- posterior_epred(fit, newdata = newdata, re_formula = NA)

# Allow predictions for new groups
new_groups <- data.frame(..., site = "new_site", ...)
pred_new <- posterior_epred(
  fit,
  newdata = new_groups,
  allow_new_levels = TRUE,
  sample_new_levels = "uncertainty"
)
```

---

## 3. Function Specifications

### 3.1 `posterior_linpred.mvgam()`

**Purpose**: Extract linear predictor values (link scale) combining observation and trend effects.

**Parameters**:
- `object`: mvgam object (fitted model)
- `newdata`: data.frame with all covariates from observation and trend formulas (required)
- `transform`: logical, if TRUE applies inverse link function (default FALSE)
- `process_error`: logical, if TRUE includes uncertainty in trend parameters (default TRUE)
- `ndraws`: positive integer, number of posterior draws to use (NULL = all)
- `draw_ids`: integer vector specifying exact draws (alternative to ndraws)
- `re_formula`: formula for random effects (NULL = include all, NA = exclude all)
- `allow_new_levels`: logical, allow new factor levels in random effects (default FALSE)
- `sample_new_levels`: character, how to sample new levels ("uncertainty" or "gaussian")
- `resp`: character, which response variable for multivariate models (NULL = all)
- `...`: additional arguments

**Return Value**:
- Matrix with dimensions `ndraws × nrow(newdata)`
- Each row is one posterior draw
- Each column is one observation from newdata
- Values on link scale (log, logit, etc.) unless `transform = TRUE`

**Side Effects**: None (pure function)

**Implementation Details**:
- Must validate that all covariates present in newdata
- Must handle both univariate and multivariate models
- Must properly combine observation linear predictor with trend effects
- Respects lazy parameter categorization for extraction

---

### 3.2 `posterior_epred.mvgam()`

**Purpose**: Extract expected values (expectation scale) after applying inverse link function.

**Parameters**:
- `object`: mvgam object (fitted model)
- `newdata`: data.frame with all covariates (required)
- `process_error`: logical, include trend parameter uncertainty (default TRUE)
- `ndraws`: positive integer, number of draws (NULL = all)
- `draw_ids`: integer vector for specific draws
- `re_formula`: formula for random effects handling
- `allow_new_levels`: logical for new factor levels
- `sample_new_levels`: character, new level sampling method
- `resp`: character, response variable for multivariate models
- `...`: additional arguments

**Return Value**:
- Matrix with dimensions `ndraws × nrow(newdata)`
- Values on response scale (counts for Poisson, probabilities for Bernoulli, etc.)
- Represents E[Y | newdata, posterior]

**Side Effects**: None (pure function)

**Implementation Details**:
- Internally calls `posterior_linpred()` then applies inverse link
- Must handle distributional parameters correctly (sigma, shape, etc.)
- For families with multiple parameters, uses appropriate inverse link

---

### 3.3 `posterior_predict.mvgam()`

**Purpose**: Generate samples from posterior predictive distribution including observation-level variability.

**Parameters**:
- `object`: mvgam object (fitted model)
- `newdata`: data.frame with all covariates (required)
- `process_error`: logical, include trend uncertainty (default TRUE)
- `ndraws`: positive integer, number of draws (NULL = all)
- `draw_ids`: integer vector for specific draws
- `re_formula`: formula for random effects
- `allow_new_levels`: logical for new levels
- `sample_new_levels`: character, new level method
- `resp`: character for multivariate models
- `...`: additional arguments

**Return Value**:
- Matrix with dimensions `ndraws × nrow(newdata)`
- Values are random draws from the posterior predictive distribution
- Includes both parameter uncertainty and sampling variability
- Same scale/type as observed response (integers for Poisson, 0/1 for Bernoulli)

**Side Effects**: Uses R's random number generator (set.seed for reproducibility)

**Implementation Details**:
- Calls `posterior_epred()` to get expected values
- Adds observation noise using family-specific sampling (rpois, rbinom, etc.)
- Must handle truncation, censoring if present in model

---

### 3.4 `posterior_smooths.mvgam()`

**Purpose**: Extract smooth term contributions from GAM components.

**Parameters**:
- `object`: mvgam object
- `smooth`: character, name of smooth term (NULL = all smooths)
- `newdata`: data.frame (optional, if NULL uses training data)
- `process_error`: logical (default FALSE for smooths)
- `ndraws`: positive integer (NULL = all)
- `...`: additional arguments

**Return Value**:
- If `smooth` specified: matrix of `ndraws × nrow(newdata)` for that smooth
- If `smooth = NULL`: list of matrices, one per smooth term
- Values represent smooth contribution to linear predictor

**Side Effects**: None

**Implementation Details**:
- Extracts smooth basis matrices from model
- Multiplies by posterior samples of smooth coefficients
- Does not include intercept or other fixed effects

---

### 3.5 `posterior_average.mvgam()`

**Purpose**: Compute posterior predictions averaged over response categories (for categorical models).

**Parameters**:
- `object`: mvgam object (must have categorical family)
- `newdata`: data.frame with covariates (required)
- `process_error`: logical (default TRUE)
- `ndraws`: positive integer (NULL = all)
- `...`: additional arguments

**Return Value**:
- Matrix with `ndraws × nrow(newdata)`
- Values are category-weighted averages

**Side Effects**: None

**Implementation Details**:
- Only applicable to ordered/categorical families
- Computes weighted average across categories
- Weights are posterior predictive probabilities

---

### 3.6 `predict.mvgam()`

**Purpose**: Convenience wrapper matching base R predict() generic.

**Parameters**:
- `object`: mvgam object
- `newdata`: data.frame (optional, if NULL uses training data)
- `type`: character, one of "link", "response", "prediction" (default "response")
- `process_error`: logical (default TRUE)
- `ndraws`: positive integer (NULL = all)
- `summary`: logical, if TRUE returns summary statistics instead of draws (default TRUE)
- `probs`: numeric vector of quantiles for summary (default c(0.025, 0.975))
- `...`: passed to posterior_* functions

**Return Value**:
- If `summary = TRUE`: data.frame with columns `Estimate`, `Q2.5`, `Q97.5` (or custom probs)
- If `summary = FALSE`: matrix of posterior draws

**Side Effects**: None

**Implementation Details**:
- Dispatches to `posterior_linpred()` (type="link"), `posterior_epred()` (type="response"), or `posterior_predict()` (type="prediction")
- Automatically computes quantiles if summary=TRUE

---

### 3.7 `fitted.mvgam()`

**Purpose**: Convenience wrapper for in-sample fitted values (matching stats::fitted generic).

**Parameters**:
- `object`: mvgam object
- `scale`: character, "response" or "linear" (default "response")
- `process_error`: logical (default TRUE)
- `summary`: logical (default TRUE)
- `...`: additional arguments

**Return Value**:
- If `summary = TRUE`: data.frame with fitted value summaries
- If `summary = FALSE`: matrix of posterior draws
- Same length/order as training data

**Side Effects**: None

**Implementation Details**:
- Equivalent to `predict(object, newdata = NULL)`
- Uses original training data for predictions

---

## 4. Functional Requirements

1. The system must extract posterior samples from the combined mvgam fit object (`object$fit`) using the lazy categorization system to identify observation vs. trend parameters.

2. The system must create design matrices for both observation and trend formulas using brms's `prepare_predictions()` infrastructure by passing extracted parameter subsets via the `draws` argument.

3. The system must compute trend predictions on the response scale (Gaussian, since all State-Space trends are Gaussian) from the trend model first, then combine with observation-level effects.

4. The system must support all three prediction scales exactly as brms does:
   - Link scale: linear predictor before inverse link
   - Expectation scale: expected value after inverse link
   - Outcome scale: posterior predictive samples with observation noise

5. The system must handle the `process_error` argument to control inclusion of trend parameter uncertainty:
   - `process_error = TRUE`: Sample trend parameters from posterior
   - `process_error = FALSE`: Use posterior mean of trend parameters (faster)

6. The system must support the `ndraws` parameter following brms conventions:
   - `ndraws = NULL`: Use all posterior draws (default)
   - `ndraws = 100`: Randomly subsample 100 draws
   - Validate that ndraws ≤ total available draws

7. The system must support all brms random effects arguments:
   - `re_formula`: Include/exclude random effects
   - `allow_new_levels`: Permit new factor levels in predictions
   - `sample_new_levels`: Method for sampling new levels ("uncertainty" or "gaussian")

8. The system must require all covariates from both observation and trend formulas in `newdata`, producing clear error messages for missing variables.

9. The system must always include time and series variables in the model frame to enable series-specific and time-specific marginal effects.

10. The system must handle multivariate models with both shared trends and response-specific trends:
    - Shared trends: Predict once, apply to all responses
    - Response-specific: Predict separately per response, apply appropriately

11. The system must integrate with the marginaleffects package by providing required S3 methods:
    - `get_predict.mvgam()`
    - `get_vcov.mvgam()` (if applicable)
    - Other methods as needed for full compatibility

12. The system must support marginaleffects functions:
    - Core: `predictions()`, `comparisons()`, `slopes()`
    - Advanced: `avg_predictions()`, `avg_comparisons()`, `avg_slopes()`, `hypotheses()`

13. The system must pass the `process_error` argument through to marginaleffects predictions, with trend covariates always included in predictions.

14. The system must enhance `conditional_effects.mvgam()` to:
    - Preserve current brms delegation pattern
    - Support series-specific conditional effects via `series` argument
    - Properly handle trend component contributions
    - Maintain compatibility with existing mvgam 1.5 plotting methods

15. The system must handle edge cases:
    - Models with `trend_formula = NULL` (pure brms models)
    - Models with no covariates in observation formula (trend-only)
    - Models with no covariates in trend formula (observation-only)
    - Distributional models (trends only on mu parameter)

16. The system must validate newdata structure:
    - Check for required variables
    - Validate factor levels against training data
    - Handle missing data according to re_formula specification

17. The system must be efficient for large posteriors:
    - Use lazy extraction of only needed parameters
    - Support chunked processing for very large prediction tasks
    - Minimize memory footprint where possible

18. The system must maintain exact compatibility with brms behavior when `trend_formula = NULL`:
    - Predictions match brms exactly
    - All arguments work identically
    - Error messages consistent

---

## 5. Data Flow & Dependencies

### High-Level Data Flow

```
User Calls posterior_epred(mvgam_fit, newdata)
    ↓
┌─────────────────────────────────────────────────┐
│ 1. Validate Inputs                              │
│    - Check newdata structure                    │
│    - Validate ndraws, process_error, re_formula │
│    - Ensure time/series variables present       │
└────────────────┬────────────────────────────────┘
                 ↓
┌─────────────────────────────────────────────────┐
│ 2. Extract Posterior Parameters                 │
│    - Use lazy categorization:                   │
│      obs_pars ← categorize_parameters("obs")    │
│      trend_pars ← categorize_parameters("trend")│
│    - Extract only needed parameters from fit    │
│    - Optionally subsample to ndraws             │
└────────────────┬────────────────────────────────┘
                 ↓
┌─────────────────────────────────────────────────┐
│ 3. Create Design Matrices                       │
│    - Observation model:                         │
│      prep_obs ← brms:::prepare_predictions(     │
│        object$model_obs,                        │
│        newdata,                                 │
│        draws = obs_draws                        │
│      )                                          │
│    - Trend model:                               │
│      prep_trend ← brms:::prepare_predictions(   │
│        object$model_trend,                      │
│        newdata,                                 │
│        draws = trend_draws                      │
│      )                                          │
└────────────────┬────────────────────────────────┘
                 ↓
┌─────────────────────────────────────────────────┐
│ 4. Compute Trend Predictions (Sequential)       │
│    - Extract trend design matrices              │
│    - Compute trend linear predictor             │
│    - Trend values on response scale (Gaussian)  │
│    - Handle factor models (Z matrices)          │
│    - Handle hierarchical trends (gr, subgr)     │
└────────────────┬────────────────────────────────┘
                 ↓
┌─────────────────────────────────────────────────┐
│ 5. Compute Observation Predictions              │
│    - Extract observation design matrices        │
│    - Compute observation linear predictor       │
│    - Combine with trend effects:                │
│      linpred = obs_linpred + trend_effects      │
└────────────────┬────────────────────────────────┘
                 ↓
┌─────────────────────────────────────────────────┐
│ 6. Apply Scale Transformation                   │
│    - posterior_linpred: Return as-is            │
│    - posterior_epred: Apply inverse link        │
│    - posterior_predict: Add observation noise   │
└────────────────┬────────────────────────────────┘
                 ↓
┌─────────────────────────────────────────────────┐
│ 7. Return Predictions Matrix                    │
│    - Format: ndraws × nrow(newdata)             │
│    - Optionally compute summaries               │
└─────────────────────────────────────────────────┘
```

### Sequential Prediction Architecture

The key architectural decision is **sequential prediction from two brmsfit objects**:

```r
# Pseudo-code for prediction workflow
predict_mvgam <- function(object, newdata, scale = "epred") {
  # Step 1: Extract parameters using lazy categorization
  obs_params <- extract_obs_parameters(object$fit)
  trend_params <- extract_trend_parameters(object$fit)

  # Step 2: Prepare trend predictions
  # (using brms infrastructure with external draws)
  trend_prep <- brms:::prepare_predictions(
    object = object$model_trend,  # May be mock object
    newdata = newdata,
    draws = trend_params,         # From object$fit
    re_formula = re_formula
  )

  # Step 3: Compute trend effects (Gaussian response scale)
  trend_effects <- compute_trend_linpred(trend_prep, object$trend_metadata)

  # Step 4: Prepare observation predictions
  obs_prep <- brms:::prepare_predictions(
    object = object$model_obs,    # May be mock object
    newdata = newdata,
    draws = obs_params,           # From object$fit
    re_formula = re_formula
  )

  # Step 5: Compute observation linear predictor
  obs_linpred <- compute_obs_linpred(obs_prep)

  # Step 6: Combine (additive on link scale)
  combined_linpred <- obs_linpred + trend_effects

  # Step 7: Apply scale transformation
  if (scale == "linpred") {
    return(combined_linpred)
  } else if (scale == "epred") {
    return(inv_link(combined_linpred, family))
  } else if (scale == "predict") {
    epred <- inv_link(combined_linpred, family)
    return(add_observation_noise(epred, family, params))
  }
}
```

### Dependencies

**External R Packages**:
- `brms`: Core prediction infrastructure (prepare_predictions, design matrices)
- `posterior`: Posterior draws manipulation
- `checkmate`: Input validation
- `insight`: Error/warning formatting
- `marginaleffects`: Integration for marginal effects
- `mgcv`: Smooth term handling (via brms)
- `Matrix`: Sparse random effects matrices (via brms)

**Internal mvgam Dependencies**:
- `R/trend_system.R`: Trend metadata and dispatch
- `R/validations.R`: Data validation, time/series handling
- `R/stan_assembly.R`: Parameter naming conventions
- Lazy categorization system for parameter extraction
- mvgam object structure (`$fit`, `$model_obs`, `$model_trend`, `$trend_metadata`)

**Data Dependencies**:
- `object$fit`: Combined posterior from Stan
- `object$model_obs`: brmsfit for observation model (may be mock)
- `object$model_trend`: brmsfit for trend model (may be mock)
- `object$trend_metadata`: Stored trend specifications for prediction
- `object$exclude`: Parameters to exclude from predictions
- Training data factor levels and contrasts (stored in brmsfit objects)

---

### R Idioms and Best Practices

**DO**:
- Use `checkmate::assert_*()` for all input validation
- Use `insight::format_error()` for user-facing error messages
- Follow tidyverse style guide (80 char line limit, 2-space indents)
- Write modular helper functions (single responsibility)
- Use meaningful variable names (no abbreviations like `n`, `x`, `df`)
- Document all helper functions with comments
- Use lazy evaluation where possible (don't extract unless needed)

**DON'T**:
- Don't duplicate brms functionality (leverage existing infrastructure)
- Don't use base R `stop()` or `warning()` (use insight functions)
- Don't hardcode parameter names (use categorization system)
- Don't assume posterior structure (use generic extraction functions)
- Don't ignore edge cases (test with minimal/maximal models)
- Don't use `try()` or `tryCatch()` to silence errors in tests

### When to Ask for Help

- **Architecture questions**: If you're unsure how components fit together
- **brms integration**: If brms functions behave unexpectedly
- **Performance issues**: If predictions are unacceptably slow
- **Test failures**: If tests fail and you don't understand why
- **Design decisions**: If you encounter a choice between multiple approaches
