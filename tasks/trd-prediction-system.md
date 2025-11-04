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

## 6. User Interface Requirements

### Function Signatures

All prediction functions must follow this signature pattern (matching brms):

```r
posterior_linpred.mvgam <- function(
  object,
  newdata = NULL,
  transform = FALSE,
  process_error = TRUE,
  ndraws = NULL,
  draw_ids = NULL,
  re_formula = NULL,
  allow_new_levels = FALSE,
  sample_new_levels = "uncertainty",
  resp = NULL,
  ...
)
```

### Parameter Naming Conventions

- Use `ndraws` (not `n_draws`, `n.draws`, `nsamp`)
- Use `newdata` (not `new_data`, `data`)
- Use `process_error` for trend uncertainty (mvgam-specific addition)
- Use `re_formula` for random effects (matching brms exactly)
- Use `draw_ids` as alias for `ndraws` (advanced users)

### Default Behaviors

- `ndraws = NULL`: Use all posterior draws (most accurate)
- `process_error = TRUE`: Include trend parameter uncertainty (default safer, more conservative)
- `allow_new_levels = FALSE`: Error on new random effect levels (safe default)
- `sample_new_levels = "uncertainty"`: Sample from hyperprior if new levels allowed
- `re_formula = NULL`: Include all random effects (match model specification)
- `newdata = NULL`: Use training data (in-sample fitted values)

### Progress Indicators

For very large prediction tasks (many draws × many observations):
```r
# Optional progress bar for long-running predictions
if (nrow(newdata) * ndraws > 1e6) {
  message("Computing predictions for large dataset...")
  # Consider using cli::cli_progress_bar()
}
```

---

## 7. Error Handling & Validation

### Input Validation Requirements

All functions must validate inputs using `checkmate::assert_*()`:

```r
# Example validation block
checkmate::assert_class(object, "mvgam")
checkmate::assert_data_frame(newdata, null.ok = TRUE)
checkmate::assert_count(ndraws, positive = TRUE, null.ok = TRUE)
checkmate::assert_logical(process_error, len = 1)
checkmate::assert_logical(allow_new_levels, len = 1)
checkmate::assert_choice(sample_new_levels, c("uncertainty", "gaussian"))
```

### Error Messages (Using insight::format_error)

**Missing Required Variables**:
```r
if (!all(required_vars %in% names(newdata))) {
  missing <- setdiff(required_vars, names(newdata))
  insight::format_error(
    "{.field newdata} is missing required variables: {.field {missing}}.",
    "The model requires all covariates from both observation and trend formulas.",
    "Add these columns to {.field newdata}: {paste(missing, collapse = ', ')}"
  )
}
```

**Time/Series Variables Missing (State-Space Models)**:
```r
if (has_trend_model(object) && !has_time_var(newdata)) {
  insight::format_error(
    "State-Space models require {.field time} variable in {.field newdata}.",
    "The fitted model uses trend_formula and needs time indexing.",
    "Add a column named '{object$trend_metadata$time_var}' to newdata."
  )
}
```

**Invalid ndraws**:
```r
if (ndraws > nsamples(object)) {
  insight::format_error(
    "{.field ndraws} ({ndraws}) exceeds available posterior samples ({nsamples(object)}).",
    "Set {.field ndraws} to a value between 1 and {nsamples(object)}, or use NULL for all draws."
  )
}
```

**New Random Effect Levels Without Permission**:
```r
if (has_new_levels && !allow_new_levels) {
  insight::format_error(
    "New levels detected in random effects: {.field {new_levels}}.",
    "Set {.field allow_new_levels = TRUE} to enable predictions for new groups.",
    "Or provide data only for existing levels: {paste(existing_levels, collapse = ', ')}"
  )
}
```

**Conflicting Arguments**:
```r
if (!is.null(ndraws) && !is.null(draw_ids)) {
  insight::format_error(
    "Cannot specify both {.field ndraws} and {.field draw_ids}.",
    "Use {.field ndraws} for random subsampling or {.field draw_ids} for specific draws."
  )
}
```

### Warnings (Using insight::format_warning)

**Large Prediction Tasks**:
```r
if (nrow(newdata) * ndraws > 5e6) {
  insight::format_warning(
    "Prediction task is very large ({nrow(newdata)} obs × {ndraws} draws = {nrow(newdata) * ndraws} values).",
    "Consider reducing {.field ndraws} or processing in chunks for better memory efficiency."
  )
}
```

**process_error = FALSE with Uncertainty Quantification**:
```r
if (!process_error && computing_intervals) {
  insight::format_warning(
    "Using {.field process_error = FALSE} may underestimate prediction uncertainty.",
    "Trend parameter uncertainty is excluded. Set {.field process_error = TRUE} for conservative intervals."
  )
}
```

### Graceful Degradation

**No Trend Model Present**:
```r
# When trend_formula = NULL, delegate entirely to brms
if (is.null(object$trend_formula)) {
  message("No trend model detected, using pure brms predictions.")
  return(brms::posterior_epred(object$model_obs, newdata = newdata, ...))
}
```

**Missing Optional Arguments**:
```r
# Handle edge cases gracefully
if (is.null(newdata)) {
  message("Using training data for in-sample predictions.")
  newdata <- object$data
}
```

---

## 8. Examples & Usage Patterns

### Example 1: Basic Usage (Most Common Case)

```r
library(mvgam)

# Fit model
fit <- mvgam(
  formula = count ~ temperature,
  trend_formula = ~ RW(cor = TRUE),
  data = training_data,
  family = poisson()
)

# Create newdata
newdata <- data.frame(
  temperature = c(15, 20, 25),
  time = c(51, 52, 53),
  series = c("A", "A", "A")
)

# Get expected values (most common)
pred_epred <- posterior_epred(fit, newdata = newdata)
# Returns 4000 draws × 3 observations matrix

# Compute summaries
pred_summary <- data.frame(
  temperature = newdata$temperature,
  mean = colMeans(pred_epred),
  median = apply(pred_epred, 2, median),
  lower = apply(pred_epred, 2, quantile, 0.025),
  upper = apply(pred_epred, 2, quantile, 0.975)
)
```

### Example 2: Using Convenience predict() Method

```r
# Simple interface with automatic summaries
pred <- predict(fit, newdata = newdata, type = "response")
# Returns data.frame with Estimate, Q2.5, Q97.5

# Get full posterior draws instead
pred_draws <- predict(
  fit,
  newdata = newdata,
  type = "response",
  summary = FALSE
)
# Returns matrix of draws

# Custom quantiles
pred_custom <- predict(
  fit,
  newdata = newdata,
  probs = c(0.1, 0.5, 0.9)
)
# Returns Estimate, Q10, Q50, Q90
```

### Example 3: Controlling Posterior Draws

```r
# Use all draws (default, slowest but most accurate)
pred_full <- posterior_epred(fit, newdata = newdata)

# Subsample for speed
pred_fast <- posterior_epred(fit, newdata = newdata, ndraws = 100)

# Use specific draws (e.g., every 10th)
pred_thinned <- posterior_epred(
  fit,
  newdata = newdata,
  draw_ids = seq(1, 4000, by = 10)
)
```

### Example 4: Process Error Control

```r
# Include trend uncertainty (default, conservative)
pred_with_uncertainty <- posterior_epred(
  fit,
  newdata = newdata,
  process_error = TRUE
)

# Faster predictions using point estimates of trend parameters
pred_fast <- posterior_epred(
  fit,
  newdata = newdata,
  process_error = FALSE
)

# Compare uncertainty width
sd_with <- apply(pred_with_uncertainty, 2, sd)
sd_without <- apply(pred_fast, 2, sd)
# sd_with will be larger (more conservative)
```

### Example 5: Random Effects Handling

```r
# Model with random effects
fit_re <- mvgam(
  formula = count ~ temperature + (1 | site),
  trend_formula = ~ AR(p = 1),
  data = training_data
)

# Population-level predictions (exclude random effects)
pred_pop <- posterior_epred(fit_re, newdata = newdata, re_formula = NA)

# Include all random effects (default)
pred_all <- posterior_epred(fit_re, newdata = newdata)

# Predictions for new sites
newdata_newsite <- data.frame(
  temperature = 20,
  site = "new_site",
  time = 51,
  series = "A"
)

pred_new <- posterior_epred(
  fit_re,
  newdata = newdata_newsite,
  allow_new_levels = TRUE,
  sample_new_levels = "uncertainty"  # Sample from hyperprior
)
```

### Example 6: Multivariate Models

```r
# Multivariate model with shared trend
fit_mv <- mvgam(
  formula = mvbind(count, biomass) ~ temperature,
  trend_formula = ~ AR(p = 1, cor = TRUE),
  data = mv_data,
  family = c(poisson(), gaussian())
)

# Predictions for all responses
pred_all_resp <- posterior_epred(fit_mv, newdata = newdata)
# Returns list with pred_all_resp$count and pred_all_resp$biomass

# Predictions for specific response
pred_count <- posterior_epred(
  fit_mv,
  newdata = newdata,
  resp = "count"
)
```

### Example 7: Different Prediction Scales

```r
# Link scale (before inverse link)
pred_link <- posterior_linpred(fit, newdata = newdata)
# For Poisson: log scale values

# Expectation scale (after inverse link)
pred_epred <- posterior_epred(fit, newdata = newdata)
# For Poisson: expected counts (λ values)

# Posterior predictive (with observation noise)
pred_predict <- posterior_predict(fit, newdata = newdata)
# For Poisson: actual count draws (integer values)

# Compare uncertainty
plot(density(pred_epred[, 1]), main = "Uncertainty comparison")
lines(density(pred_predict[, 1]), col = "red")
# Predictive distribution has heavier tails
```

### Example 8: Smooth Term Extraction

```r
# Model with smooths
fit_smooth <- mvgam(
  formula = count ~ s(temperature) + s(day_of_year),
  trend_formula = ~ RW(),
  data = training_data
)

# Extract all smooth contributions
smooths <- posterior_smooths(fit_smooth, newdata = newdata)
# Returns list: smooths$`s(temperature)`, smooths$`s(day_of_year)`

# Extract specific smooth
temp_effect <- posterior_smooths(
  fit_smooth,
  smooth = "s(temperature)",
  newdata = newdata
)
# Returns matrix of just temperature smooth contribution
```

### Example 9: Integration with marginaleffects

```r
library(marginaleffects)

# Compute marginal effects automatically
mfx <- slopes(fit, variables = "temperature", newdata = newdata)

# Average marginal effects
avg_mfx <- avg_slopes(fit, variables = "temperature")

# Comparisons (contrasts)
comp <- comparisons(
  fit,
  variables = list(temperature = c(15, 25)),
  newdata = newdata
)

# Custom hypotheses
hyp <- hypotheses(
  fit,
  hypothesis = "b1 + b2 = 0",  # Linear combination of effects
  newdata = newdata
)
```

### Example 10: Enhanced conditional_effects()

```r
# Basic conditional effects (current behavior)
ce <- conditional_effects(fit, effects = "temperature")
plot(ce)

# Series-specific effects (new feature)
ce_series <- conditional_effects(
  fit,
  effects = "temperature",
  series = "Species_A"  # Show effects for this series only
)

# Multiple effects with trend uncertainty
ce_multi <- conditional_effects(
  fit,
  effects = c("temperature", "day_of_year"),
  process_error = TRUE  # Include trend uncertainty in intervals
)
plot(ce_multi)
```

### Example 11: Posterior Predictive Checks

```r
# Generate posterior predictive samples
y_rep <- posterior_predict(fit, newdata = NULL)  # In-sample

# Use with bayesplot
library(bayesplot)
ppc_dens_overlay(y = fit$data$count, yrep = y_rep[1:50, ])

# Custom predictive check
ppc_intervals(
  y = fit$data$count,
  yrep = y_rep,
  x = fit$data$temperature
)
```

### Example 12: Edge Cases

```r
# Pure observation model (no trends)
fit_no_trend <- mvgam(
  formula = count ~ temperature,
  trend_formula = NULL,  # No trend
  data = training_data
)

# Predictions delegate to brms
pred <- posterior_epred(fit_no_trend, newdata = newdata)
# Identical to brms::posterior_epred()

# Trend-only model
fit_trend_only <- mvgam(
  formula = count ~ 1,  # Intercept only
  trend_formula = ~ s(habitat) + AR(p = 1),
  data = training_data
)

# Predictions dominated by trend component
pred_trend <- posterior_epred(fit_trend_only, newdata = newdata)
```

---

## 9. Testing Requirements

### Unit Tests for Each Function

**Test Organization**: Create separate test files for each major component
- `tests/testthat/test-posterior_linpred.R`
- `tests/testthat/test-posterior_epred.R`
- `tests/testthat/test-posterior_predict.R`
- `tests/testthat/test-marginaleffects.R`
- `tests/testthat/test-conditional_effects.R`

### Test-Driven Development Approach

The user will provide specific use cases to guide test development. Tests should be written **before** implementation to clarify requirements.

**Test Template**:
```r
test_that("posterior_epred handles basic univariate Gaussian model", {
  # Setup: Fit minimal model
  data <- data.frame(
    y = rnorm(50),
    x = rnorm(50),
    time = rep(1:10, 5),
    series = rep(1:5, each = 10)
  )

  fit <- mvgam(
    formula = y ~ x,
    trend_formula = ~ RW(),
    data = data,
    family = gaussian(),
    chains = 2,
    iter = 500,
    silent = 2
  )

  # Test: Generate predictions
  newdata <- data.frame(
    x = c(0, 1, -1),
    time = c(11, 12, 13),
    series = c(1, 1, 1)
  )

  pred <- posterior_epred(fit, newdata = newdata)

  # Assertions
  expect_matrix(pred)
  expect_equal(ncol(pred), 3)  # 3 observations
  expect_equal(nrow(pred), nsamples(fit))
  expect_true(all(is.finite(pred)))

  # Check trend contribution
  pred_no_trend <- posterior_epred(fit, newdata = newdata, re_formula = NA)
  # Trend should add variability
  expect_true(sd(pred) > sd(pred_no_trend))
})
```

### Critical Test Scenarios

**1. Prediction Scale Accuracy**:
```r
test_that("prediction scales transform correctly", {
  # Poisson model (log link)
  fit <- mvgam(..., family = poisson())

  linpred <- posterior_linpred(fit, newdata = newdata)
  epred <- posterior_epred(fit, newdata = newdata)

  # epred should equal exp(linpred) for Poisson
  expect_equal(epred, exp(linpred), tolerance = 1e-10)

  # predict should be integers
  yrep <- posterior_predict(fit, newdata = newdata)
  expect_true(all(yrep == floor(yrep)))  # All integers
})
```

**2. brms Compatibility (No Trends)**:
```r
test_that("predictions match brms exactly when trend_formula = NULL", {
  # Fit same model in brms and mvgam
  brms_fit <- brms::brm(y ~ x, data = data, family = gaussian())
  mvgam_fit <- mvgam(y ~ x, trend_formula = NULL, data = data, family = gaussian())

  # Predictions should be identical
  newdata <- data.frame(x = c(0, 1, 2))

  pred_brms <- brms::posterior_epred(brms_fit, newdata = newdata)
  pred_mvgam <- posterior_epred(mvgam_fit, newdata = newdata)

  expect_equal(pred_mvgam, pred_brms, tolerance = 1e-10)
})
```

**3. Process Error Toggle**:
```r
test_that("process_error controls trend uncertainty", {
  fit <- mvgam(..., trend_formula = ~ AR(p = 1))

  pred_with <- posterior_epred(fit, newdata = newdata, process_error = TRUE)
  pred_without <- posterior_epred(fit, newdata = newdata, process_error = FALSE)

  # Uncertainty should be larger with process error
  sd_with <- apply(pred_with, 2, sd)
  sd_without <- apply(pred_without, 2, sd)

  expect_true(all(sd_with >= sd_without))
})
```

**4. Random Effects Handling**:
```r
test_that("random effects excluded correctly with re_formula = NA", {
  fit <- mvgam(y ~ x + (1 | group), trend_formula = ~ RW(), data = data)

  pred_all <- posterior_epred(fit, newdata = newdata)
  pred_pop <- posterior_epred(fit, newdata = newdata, re_formula = NA)

  # Population predictions should have less variability across groups
  # (test with multiple groups in newdata)
  expect_true(sd(pred_pop) < sd(pred_all))
})
```

**5. New Random Effect Levels**:
```r
test_that("new levels handled according to allow_new_levels", {
  fit <- mvgam(y ~ x + (1 | site), ...)
  newdata <- data.frame(x = 1, site = "new_site", time = 11, series = 1)

  # Should error by default
  expect_error(
    posterior_epred(fit, newdata = newdata, allow_new_levels = FALSE),
    "New levels detected"
  )

  # Should work when allowed
  pred <- posterior_epred(fit, newdata = newdata, allow_new_levels = TRUE)
  expect_matrix(pred)
  expect_equal(ncol(pred), 1)
})
```

**6. Multivariate Models**:
```r
test_that("multivariate predictions return list of matrices", {
  fit <- mvgam(
    mvbind(y1, y2) ~ x,
    trend_formula = ~ AR(p = 1),
    data = data
  )

  pred <- posterior_epred(fit, newdata = newdata)

  expect_list(pred)
  expect_named(pred, c("y1", "y2"))
  expect_matrix(pred$y1)
  expect_matrix(pred$y2)
  expect_equal(nrow(pred$y1), nrow(pred$y2))  # Same number of draws
})
```

**7. ndraws Subsampling**:
```r
test_that("ndraws correctly subsamples posterior", {
  fit <- mvgam(...)  # Model with 4000 samples

  pred_all <- posterior_epred(fit, newdata = newdata)
  pred_100 <- posterior_epred(fit, newdata = newdata, ndraws = 100)

  expect_equal(nrow(pred_all), 4000)
  expect_equal(nrow(pred_100), 100)

  # Means should be similar
  expect_equal(
    colMeans(pred_all),
    colMeans(pred_100),
    tolerance = 0.1  # Some sampling variability expected
  )
})
```

**8. Input Validation**:
```r
test_that("missing required variables produce clear errors", {
  fit <- mvgam(y ~ x + z, trend_formula = ~ RW(), data = data)

  # Missing covariate
  expect_error(
    posterior_epred(fit, newdata = data.frame(x = 1, time = 11, series = 1)),
    "missing required variables.*z"
  )

  # Missing time variable
  expect_error(
    posterior_epred(fit, newdata = data.frame(x = 1, z = 1, series = 1)),
    "time.*variable"
  )
})
```

**9. marginaleffects Integration**:
```r
test_that("marginaleffects functions work with mvgam objects", {
  fit <- mvgam(y ~ x + z, trend_formula = ~ AR(p = 1), data = data)

  # Core functions should work
  expect_s3_class(
    marginaleffects::predictions(fit, newdata = newdata),
    "predictions"
  )

  expect_s3_class(
    marginaleffects::slopes(fit, variables = "x"),
    "slopes"
  )

  expect_s3_class(
    marginaleffects::comparisons(fit, variables = "x"),
    "comparisons"
  )
})
```

**10. Edge Cases**:
```r
test_that("predictions work with minimal models", {
  # Intercept-only observation
  fit1 <- mvgam(y ~ 1, trend_formula = ~ AR(p = 1), data = data)
  pred1 <- posterior_epred(fit1, newdata = data.frame(time = 11, series = 1))
  expect_matrix(pred1)

  # No trend
  fit2 <- mvgam(y ~ x, trend_formula = NULL, data = data)
  pred2 <- posterior_epred(fit2, newdata = data.frame(x = 1))
  expect_matrix(pred2)

  # Single observation prediction
  fit3 <- mvgam(y ~ x, trend_formula = ~ RW(), data = data)
  pred3 <- posterior_epred(fit3, newdata = data.frame(x = 1, time = 11, series = 1))
  expect_equal(ncol(pred3), 1)
})
```

### Integration Tests

**Workflow Tests**: End-to-end workflows that users would actually run

```r
test_that("complete prediction workflow runs successfully", {
  # Fit model
  fit <- mvgam(
    formula = count ~ temperature + s(day_of_year),
    trend_formula = ~ AR(p = 1, cor = TRUE),
    data = training_data,
    family = poisson()
  )

  # Create forecast data
  newdata <- expand.grid(
    temperature = seq(10, 30, by = 5),
    day_of_year = 1:365,
    time = 51,
    series = unique(training_data$series)
  )

  # Generate predictions
  pred <- posterior_epred(fit, newdata = newdata, ndraws = 100)

  # Compute summaries
  pred_summary <- data.frame(
    newdata,
    mean = colMeans(pred),
    sd = apply(pred, 2, sd),
    lower = apply(pred, 2, quantile, 0.025),
    upper = apply(pred, 2, quantile, 0.975)
  )

  # Assertions
  expect_data_frame(pred_summary)
  expect_true(all(pred_summary$mean > 0))  # Poisson counts
  expect_true(all(pred_summary$lower <= pred_summary$upper))
})
```

### Performance Tests

```r
test_that("predictions complete within reasonable time", {
  fit <- mvgam(...)  # Moderate-sized model
  newdata <- data.frame(...)  # 1000 observations

  # Should complete in < 10 seconds
  expect_lt(
    system.time(posterior_epred(fit, newdata = newdata, ndraws = 100))["elapsed"],
    10
  )
})
```

---

## 10. Documentation Requirements

### roxygen2 Documentation Standards

Every exported function must have complete roxygen2 documentation:

```r
#' Posterior Linear Predictor for mvgam Models
#'
#' Extract linear predictor values combining observation and trend effects.
#' This function computes the linear predictor on the link scale (before
#' applying the inverse link function).
#'
#' @param object An object of class \code{mvgam}.
#' @param newdata A \code{data.frame} containing all covariates required by
#'   both the observation formula and trend formula. Must include time and
#'   series variables for State-Space models. If \code{NULL}, uses the
#'   original training data for in-sample predictions.
#' @param transform Logical. If \code{TRUE}, applies the inverse link function
#'   to return values on the response scale. If \code{FALSE} (default), returns
#'   values on the link scale. Equivalent to \code{posterior_epred} when
#'   \code{TRUE}.
#' @param process_error Logical. If \code{TRUE} (default), includes uncertainty
#'   in trend parameters by sampling from their posterior distribution. If
#'   \code{FALSE}, uses posterior mean of trend parameters for faster
#'   computation with reduced uncertainty. See Details.
#' @param ndraws Positive integer indicating how many posterior draws should
#'   be used. If \code{NULL} (the default), all draws are used. Ignored if
#'   \code{draw_ids} is not \code{NULL}.
#' @param draw_ids An integer vector specifying the posterior draws to be used.
#'   If \code{NULL} (the default), all draws are used.
#' @param re_formula Formula for random effects to include. Use \code{NULL}
#'   (default) to include all random effects from the model. Use \code{NA} or
#'   \code{~0} to exclude all random effects (population-level predictions).
#'   Follows \code{brms} syntax exactly.
#' @param allow_new_levels Logical. If \code{TRUE}, allows predictions for new
#'   levels of random effects not present in training data. Requires
#'   \code{sample_new_levels} to specify how new levels are handled. Default
#'   is \code{FALSE}.
#' @param sample_new_levels Character string specifying how to sample new
#'   random effect levels. Options are \code{"uncertainty"} (default, samples
#'   from the random effects hyperprior) or \code{"gaussian"} (uses the
#'   empirical Bayes estimate). Only used if \code{allow_new_levels = TRUE}.
#' @param resp Character string specifying which response variable to predict
#'   for multivariate models. If \code{NULL} (default), predictions for all
#'   response variables are returned as a named list.
#' @param ... Additional arguments (currently unused).
#'
#' @return A matrix with dimensions \code{ndraws × nrow(newdata)} containing
#'   posterior samples of the linear predictor. Each row represents one
#'   posterior draw, each column represents one observation from \code{newdata}.
#'   Values are on the link scale (e.g., log scale for Poisson family).
#'   For multivariate models with \code{resp = NULL}, returns a named list
#'   with one matrix per response variable.
#'
#' @details
#' \subsection{State-Space Model Integration}{
#'   For models with \code{trend_formula} specified, this function combines
#'   the observation model linear predictor with trend effects. Trend
#'   predictions are computed first on the response scale (Gaussian for all
#'   State-Space trends), then added to the observation linear predictor.
#' }
#'
#' \subsection{Process Error}{
#'   The \code{process_error} argument controls whether uncertainty in trend
#'   parameters (e.g., AR coefficients, innovation variances) is included:
#'   \itemize{
#'     \item \code{TRUE}: Full posterior uncertainty, more conservative
#'     \item \code{FALSE}: Conditional on posterior means, faster but narrower
#'   }
#'   Setting \code{process_error = FALSE} can substantially speed up predictions
#'   for large models but may underestimate uncertainty.
#' }
#'
#' \subsection{Compatibility with brms}{
#'   When \code{trend_formula = NULL}, this function delegates to
#'   \code{brms::posterior_linpred} and produces identical results.
#' }
#'
#' @seealso
#'   \code{\link{posterior_epred.mvgam}} for predictions on the expectation scale,
#'   \code{\link{posterior_predict.mvgam}} for posterior predictive samples,
#'   \code{\link{predict.mvgam}} for a convenient wrapper with automatic summaries.
#'
#' @examples
#' \donttest{
#' # Fit a simple State-Space model
#' data <- data.frame(
#'   y = rpois(100, 20),
#'   x = rnorm(100),
#'   time = rep(1:20, 5),
#'   series = rep(1:5, each = 20)
#' )
#'
#' fit <- mvgam(
#'   formula = y ~ x,
#'   trend_formula = ~ RW(),
#'   data = data,
#'   family = poisson()
#' )
#'
#' # Create newdata
#' newdata <- data.frame(
#'   x = c(-1, 0, 1),
#'   time = c(21, 22, 23),
#'   series = c(1, 1, 1)
#' )
#'
#' # Link scale predictions (log scale for Poisson)
#' linpred <- posterior_linpred(fit, newdata = newdata)
#' dim(linpred)  # ndraws × 3
#'
#' # Transform to response scale (equivalent to posterior_epred)
#' epred <- posterior_linpred(fit, newdata = newdata, transform = TRUE)
#'
#' # Faster predictions with process_error = FALSE
#' linpred_fast <- posterior_linpred(
#'   fit,
#'   newdata = newdata,
#'   process_error = FALSE
#' )
#'
#' # Subsample posterior
#' linpred_sub <- posterior_linpred(fit, newdata = newdata, ndraws = 100)
#' }
#'
#' @export
posterior_linpred.mvgam <- function(object, ...) {
  # Implementation
}
```

### Vignette Requirements

**Vignette 1: "Prediction and Forecasting with mvgam"**

Structure:
1. Introduction to prediction workflows
2. The three prediction scales (link, expectation, outcome)
3. Working with newdata
4. Process error and uncertainty quantification
5. Random effects in predictions
6. Multivariate model predictions
7. Performance considerations (ndraws, chunking)
8. Integration with visualization packages

**Vignette 2: "marginaleffects Integration"**

Structure:
1. Introduction to marginal effects
2. Computing slopes and comparisons
3. Average marginal effects
4. Custom hypotheses and contrasts
5. Series-specific effects
6. Visualization of marginal effects
7. Comparison with manual computation

**Quick Start Guide in README**

Add section to package README:
```markdown
## Predictions

Generate predictions from fitted mvgam models:

```r
# Expected values
pred <- posterior_epred(fit, newdata = newdata)

# Posterior predictive samples
yrep <- posterior_predict(fit, newdata = newdata)

# Convenience wrapper with summaries
pred_summary <- predict(fit, newdata = newdata)
```

See `vignette("predictions")` for detailed examples.
```

---

## 11. Implementation Notes for Developers

### Recommended Implementation Order

**Phase 1: Foundation (Weeks 1-2)**
1. Implement parameter extraction using lazy categorization
2. Create `prepare_predictions()` integration layer
3. Build `posterior_linpred()` (simplest function)
4. Implement comprehensive input validation
5. Write unit tests for Phase 1 functions

**Phase 2: Core Predictions (Weeks 3-4)**
1. Implement `posterior_epred()` (builds on linpred)
2. Implement `posterior_predict()` (adds observation noise)
3. Add `predict.mvgam()` and `fitted.mvgam()` wrappers
4. Handle multivariate models
5. Write unit tests for Phase 2 functions

**Phase 3: Specialized Functions (Week 5)**
1. Implement `posterior_smooths()`
2. Implement `posterior_average()`
3. Add series-specific functionality
4. Write unit tests for Phase 3 functions

**Phase 4: Integration (Weeks 6-7)**
1. Implement marginaleffects S3 methods
2. Test marginaleffects core functions
3. Enhance `conditional_effects.mvgam()`
4. Write integration tests
5. Performance optimization

**Phase 5: Documentation & Polish (Week 8)**
1. Complete all roxygen2 documentation
2. Write vignettes
3. Create examples for all functions
4. Final testing and bug fixes
5. Update README

### Code Organization Strategy

**File Structure**:
```
R/
  predictions.R              # Core prediction infrastructure
    ├─ prepare_mvgam_predictions()
    ├─ extract_obs_parameters()
    ├─ extract_trend_parameters()
    └─ combine_predictions()

  posterior_linpred.R        # posterior_linpred.mvgam()
  posterior_epred.R          # posterior_epred.mvgam()
  posterior_predict.R        # posterior_predict.mvgam()
  posterior_smooths.R        # posterior_smooths.mvgam()
  posterior_average.R        # posterior_average.mvgam()

  predict.R                  # predict.mvgam(), fitted.mvgam()

  marginaleffects.R          # marginaleffects integration
    ├─ get_predict.mvgam()
    ├─ get_vcov.mvgam()
    └─ other required methods

  conditional_effects.R      # Enhanced conditional_effects.mvgam()

tests/testthat/
  test-posterior_linpred.R
  test-posterior_epred.R
  test-posterior_predict.R
  test-marginaleffects.R
  test-conditional_effects.R
  test-predictions-integration.R
```

### Key Design Patterns

**Pattern 1: Lazy Parameter Extraction**

Use existing categorization system efficiently:
```r
extract_obs_parameters <- function(mvgam_fit, ndraws = NULL) {
  # Get observation parameter names via lazy categorization
  obs_pars <- categorize_parameters(mvgam_fit, category = "observation")

  # Extract only these parameters from fit
  draws <- as_draws_matrix(mvgam_fit$fit, variable = obs_pars)

  # Optionally subsample
  if (!is.null(ndraws)) {
    draws <- subset_draws(draws, ndraws = ndraws)
  }

  draws
}
```

**Pattern 2: brms Integration via prepare_predictions**

Leverage brms infrastructure without duplication:
```r
prepare_mvgam_predictions <- function(mvgam_fit, newdata, component = "obs", ...) {
  # Extract appropriate brmsfit object (may be mock)
  brms_obj <- if (component == "obs") {
    mvgam_fit$model_obs
  } else {
    mvgam_fit$model_trend
  }

  # Extract parameters for this component
  draws <- if (component == "obs") {
    extract_obs_parameters(mvgam_fit, ...)
  } else {
    extract_trend_parameters(mvgam_fit, ...)
  }

  # Use brms infrastructure with external draws
  prep <- brms:::prepare_predictions(
    object = brms_obj,
    newdata = newdata,
    draws = draws,  # KEY: Pass extracted parameters
    ...
  )

  prep
}
```

**Pattern 3: Sequential Prediction Combination**

Compute trend first, then observation:
```r
compute_combined_linpred <- function(mvgam_fit, newdata, ...) {
  # Step 1: Prepare trend predictions
  prep_trend <- prepare_mvgam_predictions(
    mvgam_fit,
    newdata,
    component = "trend",
    ...
  )

  # Step 2: Compute trend linear predictor (Gaussian response scale)
  trend_linpred <- compute_trend_contribution(prep_trend, mvgam_fit)

  # Step 3: Prepare observation predictions
  prep_obs <- prepare_mvgam_predictions(
    mvgam_fit,
    newdata,
    component = "obs",
    ...
  )

  # Step 4: Compute observation linear predictor
  obs_linpred <- compute_obs_contribution(prep_obs, mvgam_fit)

  # Step 5: Combine (additive on link scale)
  combined <- obs_linpred + trend_linpred

  combined
}
```

**Pattern 4: Validation Helper**

Reusable validation function:
```r
validate_prediction_inputs <- function(object, newdata, ndraws, draw_ids, ...) {
  # Standard checks
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_data_frame(newdata, null.ok = TRUE)

  # Handle ndraws/draw_ids alias
  ndraws <- use_alias(ndraws, draw_ids)
  ndraws <- validate_ndraws(ndraws, nsamples(object), draw_ids)

  # Check required variables
  if (!is.null(newdata)) {
    required_vars <- get_required_variables(object)
    missing <- setdiff(required_vars, names(newdata))

    if (length(missing) > 0) {
      insight::format_error(
        "{.field newdata} missing required variables: {.field {missing}}.",
        "Add these columns: {paste(missing, collapse = ', ')}"
      )
    }
  }

  list(ndraws = ndraws, validated = TRUE)
}
```

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

### Performance Considerations

**Memory Optimization**:
```r
# BAD: Extract all parameters then subset
all_draws <- as_draws_matrix(fit$fit)  # Large!
obs_draws <- all_draws[, obs_param_names]

# GOOD: Extract only needed parameters
obs_draws <- as_draws_matrix(fit$fit, variable = obs_param_names)
```

**Computation Optimization**:
```r
# For large prediction tasks, consider chunking
predict_chunked <- function(object, newdata, chunk_size = 1000, ...) {
  n_obs <- nrow(newdata)
  chunks <- split(1:n_obs, ceiling(1:n_obs / chunk_size))

  results <- lapply(chunks, function(idx) {
    chunk_data <- newdata[idx, , drop = FALSE]
    posterior_epred(object, newdata = chunk_data, ...)
  })

  do.call(cbind, results)  # Combine chunks
}
```

### Debugging Approaches

**Strategy 1: Minimal Reproducible Model**
```r
# Create smallest possible model for debugging
debug_data <- data.frame(
  y = rnorm(20),
  x = rnorm(20),
  time = rep(1:10, 2),
  series = rep(1:2, each = 10)
)

debug_fit <- mvgam(
  y ~ x,
  trend_formula = ~ RW(),
  data = debug_data,
  chains = 1,
  iter = 100,  # Fast!
  silent = 2
)

# Test prediction functions
pred <- posterior_epred(debug_fit, newdata = debug_data[1:2, ])
```

**Strategy 2: Component Testing**
```r
# Test each component separately
test_parameter_extraction <- function() {
  obs_draws <- extract_obs_parameters(fit)
  print(dim(obs_draws))
  print(head(colnames(obs_draws)))
}

test_design_matrices <- function() {
  prep <- prepare_mvgam_predictions(fit, newdata)
  print(str(prep$data))
}

test_trend_computation <- function() {
  trend_effects <- compute_trend_contribution(prep_trend, fit)
  print(dim(trend_effects))
  print(summary(c(trend_effects)))
}
```

**Strategy 3: Comparison Testing**
```r
# Compare against known good results
test_against_brms <- function() {
  # Fit same model in brms (no trends)
  brms_fit <- brms::brm(y ~ x, data = data)
  mvgam_fit <- mvgam(y ~ x, trend_formula = NULL, data = data)

  # Should be identical
  pred_brms <- brms::posterior_epred(brms_fit, newdata = newdata)
  pred_mvgam <- posterior_epred(mvgam_fit, newdata = newdata)

  max_diff <- max(abs(pred_brms - pred_mvgam))
  cat("Maximum difference:", max_diff, "\n")
  stopifnot(max_diff < 1e-10)
}
```

---

## 12. Non-Goals (Explicit Boundaries)

This task will **NOT** include:

1. **Forecasting to Future Time Points**: While predictions for new covariate values are supported, forecasting beyond the observed time range (requiring simulation of future latent states) will be addressed in a separate task. The current system assumes predictions are for time points within or near the training range.

2. **Custom User-Defined Families**: Integration only covers brms-supported families. Custom family extensions will require separate design work.

3. **Cross-Validation Infrastructure**: While predictions enable cross-validation, the actual CV infrastructure (LOO, K-fold) is a separate task.

4. **Posterior Predictive Checks**: While `posterior_predict()` generates samples for PPCs, dedicated PPC plotting functions and diagnostics are separate.

5. **Automated Model Comparison**: Predictions enable manual comparison, but automated comparison metrics (WAIC, LOOIC, Bayes factors) are out of scope.

6. **Parallel Processing**: Initial implementation will be serial. Parallel optimization (across chains, draws, or observations) is a future enhancement.

7. **Custom Trend Forecasting Logic**: The system uses existing trend dispatch infrastructure. New trend types requiring special prediction handling need separate registration.

8. **Multiple Imputation Predictions**: Predictions for models fitted to multiply-imputed data require additional design for pooling uncertainty.

9. **Predictions for jsdgam Objects**: The nmix() family and jsdgam-specific predictions are explicitly excluded from this task.

10. **Interactive Prediction Tools**: Shiny apps or interactive prediction interfaces are not part of this scope.

11. **Automated Visualization**: While conditional_effects() is enhanced, automated prediction plotting (like `plot.predictions()`) is a future feature.

12. **Trend Decomposition Plots**: While trend contributions are computed internally, user-facing functions to visualize decomposed components (obs vs. trend) are separate.

---

## 13. Success Criteria

The implementation will be considered complete and successful when:

### Functional Completeness

1. ✅ All five core prediction functions are implemented and working:
   - `posterior_linpred.mvgam()`
   - `posterior_epred.mvgam()`
   - `posterior_predict.mvgam()`
   - `posterior_smooths.mvgam()`
   - `posterior_average.mvgam()`

2. ✅ Convenience wrappers are implemented:
   - `predict.mvgam()`
   - `fitted.mvgam()`

3. ✅ All required marginaleffects S3 methods are implemented:
   - `get_predict.mvgam()`
   - Other methods as needed for full integration

4. ✅ Enhanced `conditional_effects.mvgam()` supports:
   - Series-specific effects via `series` argument
   - Process error control
   - Compatibility with existing plotting methods

### Compatibility & Integration

5. ✅ Predictions exactly match brms when `trend_formula = NULL`
   - Verified through comparison tests
   - All arguments behave identically

6. ✅ marginaleffects core functions work without errors:
   - `predictions()`, `comparisons()`, `slopes()`
   - `avg_predictions()`, `avg_comparisons()`, `avg_slopes()`
   - `hypotheses()`

7. ✅ All brms random effects arguments work correctly:
   - `re_formula` for inclusion/exclusion
   - `allow_new_levels` for new groups
   - `sample_new_levels` for sampling methods

### Testing & Quality

8. ✅ Comprehensive test coverage:
   - Unit tests for all exported functions
   - Integration tests for complete workflows
   - Edge case tests (minimal models, no trends, etc.)
   - Comparison tests against brms
   - All tests pass without errors or warnings

9. ✅ No memory leaks or performance regressions:
   - Predictions complete in reasonable time
   - Memory usage scales appropriately
   - No unbounded memory growth

### Documentation

10. ✅ Complete roxygen2 documentation for all functions:
    - All parameters described
    - Return values specified
    - Examples provided and tested
    - Cross-references to related functions

11. ✅ Vignettes written and passing R CMD check:
    - "Prediction and Forecasting with mvgam"
    - "marginaleffects Integration"

12. ✅ README updated with prediction examples

### User Experience

13. ✅ Clear, helpful error messages for common mistakes:
    - Missing required variables
    - Invalid ndraws values
    - New random effect levels without permission
    - All using `insight::format_error()`

14. ✅ User testing confirms intuitive interface:
    - Functions work as expected for typical use cases
    - Arguments behave predictably
    - No surprising behaviors

### R CMD Check

15. ✅ Package passes R CMD check with no errors, warnings, or notes
16. ✅ All examples run successfully
17. ✅ All tests pass on multiple platforms (Windows, macOS, Linux)

---

## 14. Reference Test Cases

The prediction system must work with all model configurations in the existing test suite:

**Target Models**: `target_generation.R` defines 9 comprehensive test cases covering:
- Basic RW, multivariate shared RW, VARMA with smooths
- Factor AR models (n_lv with Z matrices)
- Piecewise trends, CAR with GP, CAR with monotonic effects
- Seasonal AR, nonlinear formulas with AR

**Fitted Test Models**: `tests/local/test-models-single.R` provides pre-fitted models (fit1-fit9) for validation

**Multiple Imputation**: `tests/local/test-models-multiple.R` covers mvgam_pooled objects with combine=TRUE/FALSE

**Test Data**: `setup_stan_test_data()` provides univariate and multivariate datasets with proper structure

**Implementation Strategy**: Use these existing models for test-driven development. Each prediction function should be validated against all 9 targets before considering it complete.

---

## 15. Open Questions

These technical questions require research and decision-making during implementation:

### Architecture & Design

**Q1: Mock Object Strategy for prepare_predictions()**
- Can we use `model_obs` and `model_trend` as minimal mock objects with `draws` argument?
- Do these objects need special structure or can they be lightweight?
- What's the minimal brmsfit object content needed for design matrix creation?
- **Research needed**: Prototype mock object creation and test with prepare_predictions()

**Q2: Parameter Extraction Optimization**
- Should we extract all parameters once and cache, or extract on-demand per component?
- What's the memory vs. speed tradeoff for large posteriors?
- Can we use sparse matrices for factor models with many series?
- **Research needed**: Benchmark extraction strategies with large models

**Q3: Trend Contribution Computation**
- How exactly should trend effects be mapped to observation indices?
- How do we handle multivariate models with response-specific trends?
- Should we reuse existing Stan data structures (obs_trend_time, obs_trend_series)?
- **Research needed**: Study Stan code generation for trend integration patterns

### brms Integration Depth

**Q4: Direct Method Calls vs. Custom Implementation**
- Should we call brms S3 methods directly or reimplement with brms utilities?
- Which approach gives more control over trend integration?
- Are there version compatibility risks with private brms functions?
- **Decision needed**: Test both approaches and compare maintainability

**Q5: Design Matrix Reuse**
- Can we compute design matrices once and reuse across prediction scales?
- Or do different scales require different matrix transformations?
- **Research needed**: Study brms internal caching strategies

### marginaleffects Integration

**Q6: Required S3 Methods**
- Beyond `get_predict()`, what other methods does marginaleffects need?
- Do we need `get_vcov()` for uncertainty propagation?
- Are there mvgam-specific methods for trend handling?
- **Research needed**: Study marginaleffects source code and vignettes

**Q7: Series-Specific Marginal Effects**
- How should marginaleffects handle the `series` variable?
- Should effects be averaged across series by default?
- Can users request series-specific slopes/comparisons?
- **Decision needed**: Consult marginaleffects documentation and examples

### Performance & Scalability

**Q8: Chunking Strategy**
- At what size should we automatically chunk predictions?
- Should chunking be automatic or user-controlled?
- How do we handle chunking with random effect sampling?
- **Research needed**: Benchmark with large models (1000+ observations × 4000 draws)

**Q9: Parallel Processing Readiness**
- Should the architecture support future parallelization?
- What would be the parallelization points (draws, observations, both)?
- Are there thread-safety issues with brms functions?
- **Decision needed**: Design with or without parallel in mind

### Edge Cases & Validation

**Q10: Distributional Models**
- How are predictions for auxiliary parameters (sigma, shape) handled?
- Should users be able to predict only auxiliary parameters?
- How does `process_error` interact with distributional parameters?
- **Research needed**: Study brms distributional prediction behavior

**Q11: Time/Series Variable Requirements**
- Should time/series be required even for pure observation models?
- Can we auto-detect when they're needed vs. optional?
- What's the fallback behavior if they're missing but not strictly needed?
- **Decision needed**: Balance strictness vs. flexibility

### Testing Strategy

**Q12: Test Data Generation**
- Should we create standard test fixtures for all tests?
- How do we ensure tests run quickly (< 5 sec per test file)?
- Can we use pre-fitted models or must we fit during tests?
- **Decision needed**: Test infrastructure setup

**Q13: Comparison Test Tolerances**
- What numerical tolerance for brms comparison tests?
- Do tolerances differ by family (discrete vs. continuous)?
- How do we handle stochastic differences from sampling?
- **Research needed**: Study brms test suite patterns

### Documentation Approach

**Q14: Example Complexity**
- Should examples use real datasets or simulated data?
- How do we balance realism with computation time?
- Can examples be cached for vignettes?
- **Decision needed**: Example data strategy

**Q15: Vignette Scope**
- Should vignettes cover all families or focus on common cases?
- How much statistical background should we assume?
- Should we include troubleshooting sections?
- **Decision needed**: Target audience and depth

---

## Research Tasks for Initial Phase

Before implementation begins, conduct research on:

1. **brms prepare_predictions() internals**: Detailed study of how design matrices are created and how the `draws` argument works with mock objects.

2. **marginaleffects requirements**: Exact S3 methods needed for full compatibility, study of existing implementations for reference models.

3. **Parameter extraction patterns**: Study mvgam's lazy categorization system and determine optimal extraction strategy for obs vs. trend parameters.

4. **Trend contribution computation**: Review Stan code generation to understand how trend effects are integrated with observation linear predictors.

5. **Performance benchmarking**: Create test cases with varying sizes (small, medium, large) to establish performance baselines and identify bottlenecks.

6. **User workflow analysis**: Interview users or review common use cases to prioritize features and ensure intuitive interface design.

---

## Notes for Junior Developers

### Getting Started

1. **Read the architecture documentation first**: Familiarize yourself with `architecture-decisions.md` and `quick-reference.md`

2. **Study brms prediction functions**: Install brms and read the source code for `posterior_epred.brmsfit()` to understand the pattern

3. **Examine mvgam object structure**: Fit a simple model and use `str(fit)` to see what's stored

4. **Start with tests**: Write tests for simple cases before implementing

5. **Ask questions early**: Don't spend hours stuck on something—open an issue or ask for clarification

### Common Pitfalls to Avoid

- **Don't hardcode parameter names**: Use the categorization system
- **Don't assume posterior structure**: Always use generic extraction functions
- **Don't skip validation**: Invalid inputs should fail fast with clear errors
- **Don't optimize prematurely**: Get it working correctly first, then optimize
- **Don't forget edge cases**: Test with minimal models, no trends, etc.

### Useful Debugging Commands

```r
# Explore mvgam object
str(fit, max.level = 2)
names(fit)
class(fit)

# Check parameter names
variables(fit$fit)
categorize_parameters(fit, "observation")
categorize_parameters(fit, "trend")

# Examine brms components
str(fit$model_obs, max.level = 1)
str(fit$model_trend, max.level = 1)

# Test extraction
obs_draws <- as_draws_matrix(fit$fit, variable = "b_*")
dim(obs_draws)
head(colnames(obs_draws))
```

### When to Ask for Help

- **Architecture questions**: If you're unsure how components fit together
- **brms integration**: If brms functions behave unexpectedly
- **Performance issues**: If predictions are unacceptably slow
- **Test failures**: If tests fail and you don't understand why
- **Design decisions**: If you encounter a choice between multiple approaches

Remember: This is a complex task touching many parts of the package. It's expected that implementation will uncover additional questions and require iterative refinement. Document your findings and keep communication open.
