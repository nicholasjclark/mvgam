# Task Requirements Document: Model Fitting Verification and Post-Processing Methods

## 1. Task Overview

### Purpose
Verify and complete the model fitting functions (`mvgam_multiple` and `mvgam_single_dataset`), rename `mvgam_single_dataset` to `mvgam_single`, implement parameter discovery infrastructure, and create comprehensive diagnostic, summary, and print methods for fitted mvgam objects following brms conventions while accommodating State-Space model specifics.

### Scope
- **Verification**: Ensure `mvgam_multiple` and `mvgam_single_dataset` compile, run correctly, and produce proper output structure
- **Renaming**: Update `mvgam_single_dataset` → `mvgam_single` throughout codebase
- **Testing**: Create comprehensive local tests based on existing test patterns
- **Parameter Discovery**: Implement `variables.mvgam()` with metadata-rich parameter information
- **Diagnostics**: Implement MCMC diagnostic functions matching brms conventions
- **Summary/Print**: Create `summary.mvgam()` and `print.mvgam()` methods following brms patterns
- **Multiple Imputation**: Handle `mvgam_multiple` objects with Rubin's rules diagnostics

### User Type
R users familiar with brms workflow expecting similar interface, plus mvgam-specific State-Space functionality.

## 2. User Journey

### Primary Workflow (Single Dataset)
1. **Model Fitting**: User calls `mvgam(formula, trend_formula, data, family, ...)`
2. **Quick Inspection**: User calls `print(fit)` to see basic model specification and estimates
3. **Comprehensive Summary**: User calls `summary(fit)` to see detailed parameter estimates with diagnostics
4. **Parameter Discovery**: User calls `variables(fit)` to see all available parameters
5. **Diagnostic Checks**: User examines Rhat, ESS, and convergence warnings automatically displayed
6. **Parameter Extraction**: User extracts specific parameters via `as.data.frame(fit, variable = "trend_params")`

### Multiple Imputation Workflow
1. **Model Fitting**: User calls `mvgam(formula, trend_formula, data_list, combine = TRUE)`
2. **Pooled Summary**: User calls `summary(fit)` to see Rubin's rules pooled estimates
3. **MI Diagnostics**: User examines FMI (Fraction Missing Information) and relative increase in variance
4. **Individual Fits**: User optionally inspects `attr(fit, "individual_fits")` for imputation-specific details

### Verification Workflow (Developer)
1. **Test Execution**: Developer runs `tests/local/tests-models1.R` and new tests
2. **Output Verification**: Tests verify correct object structure and method outputs
3. **Diagnostic Validation**: Tests confirm MCMC diagnostics are computed correctly
4. **Multiple Imputation**: Tests verify pooling via Rubin's rules

## 3. Function Specifications

### 3.1 `mvgam_single()` (Renamed from `mvgam_single_dataset`)

**Purpose**: Core single-dataset fitting function (internal helper)

**Current Status**: Implemented in `R/mvgam_core.R` as `mvgam_single_dataset()`

**Required Changes**:
- Rename function to `mvgam_single()`
- Update all internal calls from `mvgam_single_dataset()` to `mvgam_single()`
- Update documentation
- Verify correct object structure creation

**Parameters**: (Already defined in mvgam_core.R:81-82)
- `formula`: Main formula
- `trend_formula`: Trend formula
- `data`: Single data frame
- `backend`: Stan backend
- `family`: Family specification
- `...`: Additional arguments

**Return Value**: mvgam object with class `c("mvgam", "brmsfit")`

**Verification Requirements**:
1. Successfully compiles and runs Stan models
2. Returns properly structured mvgam object with all required components
3. Handles errors gracefully with informative messages
4. Works with all trend types (RW, AR, VAR, CAR, PW, ZMVN)
5. Works with multivariate models
6. Compatible with downstream methods (print, summary, plot)

### 3.2 `mvgam_multiple()`

**Purpose**: Multiple imputation fitting function

**Current Status**: Implemented in `R/mvgam_core.R:641`

**Verification Requirements**:
1. Correctly fits models to each imputed dataset
2. Pools results using Rubin's rules when `combine = TRUE`
3. Returns list of individual fits when `combine = FALSE`
4. Computes within and between-imputation variance correctly
5. Returns proper `mvgam_pooled` class object
6. Stores individual fits as attribute for inspection

**Parameters**: (Already defined in mvgam_core.R:641-642)
- `formula`: Main observation model formula
- `trend_formula`: Trend formula specification
- `data_list`: List of multiply imputed datasets
- `backend`: Stan backend to use
- `combine`: Logical, whether to pool results using Rubin's rules
- `...`: Additional arguments passed to mvgam()

**Return Value**:
- If `combine = TRUE`: Object of class `c("mvgam_pooled", "mvgam", "brmsfit")`
- If `combine = FALSE`: List of mvgam objects

**Pooled Object Structure**:
```r
structure(
  list(
    # Standard mvgam components (based on template from first imputation)
    obs_fit = ...,
    trend_fit = ...,
    formula = ...,
    # ... other components

    # Multiple imputation specific
    pooled_estimates = list(
      observation = list(mean, variance, se, within_variance,
                        between_variance, relative_increase,
                        degrees_freedom, n_imputations),
      trend = list(...)
    ),
    pooling_diagnostics = list(
      observation = list(n_imputations, avg_relative_increase,
                        avg_degrees_freedom, fraction_missing_info),
      trend = list(...)
    )
  ),
  individual_fits = list(fit1, fit2, ..., fitM),
  n_imputations = M,
  pooling_method = "rubins_rules",
  class = c("mvgam_pooled", "mvgam", "brmsfit")
)
```

### 3.3 `variables.mvgam()`

**Purpose**: Discover and categorize all available parameters in fitted model

**File Location**: `R/variables.mvgam.R`

**Design Approach**: Follow brms pattern with mvgam-specific extensions

**Function Signature**:
```r
variables.mvgam <- function(x, ...)
```

**Return Structure** (Simple version matching brms):
```r
# Character vector of all parameter names
c("b_Intercept", "b_serieseries2", "sigma", "sigma_trend",
  "ar1_trend", "trend[1,1]", "trend[2,1]", ...)
```

**Return Structure** (Extended version with metadata - for future enhancement):
```r
# List with categorized parameters and metadata
list(
  observation = list(
    fixed_effects = list(
      names = c("b_Intercept", "b_serieseries2"),
      aliases = c("Intercept", "serieseries2"),
      dimensions = c(1, 1),
      type = "population_level"
    ),
    family_params = list(
      names = c("sigma", "shape"),
      aliases = c("sigma", "shape"),
      dimensions = c(1, 1),
      type = "special"
    ),
    smooth_params = list(
      names = c("s_1_1", "s_1_2", ...),
      aliases = c("s(season).1", "s(season).2", ...),
      dimensions = c(5, 5, ...),
      type = "smooth"
    )
  ),
  trend = list(
    dynamics = list(
      names = c("ar1_trend", "sigma_trend"),
      aliases = c("AR(1) coefficient", "Process SD"),
      dimensions = c(3, 3),  # n_series
      type = "trend_parameter"
    ),
    states = list(
      names = c("trend"),
      dimensions = c(60, 3),  # n_time x n_series
      type = "latent_state"
    ),
    fixed_effects = list(
      names = c("b_trend_Intercept"),
      aliases = c("Trend intercept"),
      dimensions = c(1),
      type = "population_level"
    )
  ),
  all_names = c(...)  # All parameter names as simple character vector
)
```

**Implementation Approach**:
```r
variables.mvgam <- function(x, ...) {
  # Phase 1: Simple delegation to underlying fit (matches brms exactly)
  variables(x$model_output, ...)

  # Phase 2 (future): Enhanced metadata structure
  # - Extract from mvgam object components
  # - Categorize by observation vs trend
  # - Add human-readable aliases
  # - Include dimension information
}
```

**Helper Functions** (for future metadata enhancement):
```r
# Extract observation parameters
extract_observation_parameter_info <- function(mvgam_obj) {
  # Returns list with parameter names, types, dimensions
}

# Extract trend parameters
extract_trend_parameter_info <- function(mvgam_obj) {
  # Returns list with parameter names, types, dimensions
}

# Create parameter aliases
create_parameter_aliases <- function(param_names, mvgam_obj) {
  # Maps Stan names to user-friendly names
}
```

### 3.4 `print.mvgam()`

**Purpose**: Quick interactive display of model specification and basic estimates

**File Location**: `R/print.mvgam.R` (extend existing file)

**Design Pattern**: Follow brms conventions exactly

**Function Signature**:
```r
print.mvgam(x, digits = 2, ...)
```

**Parameters**:
- `x`: mvgam object
- `digits`: Number of decimal places for numeric output
- `...`: Additional arguments (passed to formatting helpers)

**Output Sections** (in order):
```
 Family: [family_name]
   Link: [link_function]
Formula: [observation_formula]
Trend Formula: [trend_formula]  # Only if trend_formula present
Trend Model: [trend_specification]  # e.g., "AR(p = 1, cor = TRUE)"
   Data: [data_name] (Number of observations: [nobs])
 Draws: [chains] chains, each with iter = [iter]; warmup = [warmup]; thin = [thin]
        total post-warmup draws = [total_draws]

Population-Level Effects:
          Estimate Est.Error
Intercept     0.12      0.05
series2       0.45      0.08

Trend Parameters:
              Estimate Est.Error
sigma_trend       0.25      0.03
ar1_trend         0.68      0.09

Family Specific Parameters:
       Estimate Est.Error
sigma      0.15      0.02

Draws were sampled using [algorithm].
```

**Key Differences from `summary()`**:
- No credible intervals
- No MCMC diagnostics (no Rhat, ESS)
- Only Estimate and Est.Error columns
- Faster computation
- Suitable for quick interactive inspection

**Implementation Pattern**:
```r
print.mvgam <- function(x, digits = 2, ...) {
  # 1. Print family and link
  cat(" Family: ", family_name(x$family), "\n")
  cat("   Link: ", family_link(x$family), "\n")

  # 2. Print formulas
  cat("Formula: ")
  print(x$formula)
  if (!is.null(x$trend_formula)) {
    cat("Trend Formula: ")
    print(x$trend_formula)
  }

  # 3. Print trend model specification
  if (!is.null(x$trend_components)) {
    cat("Trend Model: ")
    print_trend_specification(x$trend_components)
  }

  # 4. Print data info
  cat("   Data: ", data_name(x), " (Number of observations: ", nobs(x), ")\n")

  # 5. Print sampling info
  print_sampling_info(x, detailed = FALSE)

  # 6. Print parameter tables (no CIs, no diagnostics)
  cat("\nPopulation-Level Effects:\n")
  print_parameter_table(x, component = "observation",
                       type = "fixed", detailed = FALSE, digits = digits)

  if (!is.null(x$trend_components)) {
    cat("\nTrend Parameters:\n")
    print_parameter_table(x, component = "trend",
                         type = "dynamics", detailed = FALSE, digits = digits)
  }

  cat("\nFamily Specific Parameters:\n")
  print_parameter_table(x, component = "observation",
                       type = "special", detailed = FALSE, digits = digits)

  # 7. Print algorithm info
  cat("\nDraws were sampled using ", algorithm_name(x), ".\n")

  invisible(x)
}
```

**Helper Functions Required**:
```r
family_name.mvgam()         # Extract family name
family_link.mvgam()         # Extract link function
data_name.mvgam()           # Extract data object name
print_trend_specification() # Format trend model info
print_sampling_info()       # Format MCMC sampling details
print_parameter_table()     # Format parameter estimate tables
algorithm_name.mvgam()      # Extract algorithm name
```

### 3.5 `summary.mvgam()`

**Purpose**: Comprehensive parameter summary with full MCMC diagnostics

**File Location**: `R/summary.mvgam.R`

**Design Pattern**: Follow brms conventions with mvgam State-Space extensions

**Function Signature**:
```r
summary.mvgam(object, priors = FALSE, prob = 0.95,
              robust = FALSE, mc_se = FALSE, ...)
```

**Parameters**:
- `object`: mvgam fitted object
- `priors`: Logical, whether to include prior specifications
- `prob`: Probability mass for credible intervals (default 0.95)
- `robust`: Logical, use median/MAD instead of mean/SD
- `mc_se`: Logical, include Monte Carlo standard errors
- `...`: Additional arguments

**Return Structure** (mvgam_summary S3 class):
```r
structure(
  list(
    # Model specification
    formula = ...,
    trend_formula = ...,
    data.name = ...,
    family = ...,

    # Dimensions
    nobs = ...,
    ngrps = ...,  # If random effects present

    # MCMC info
    chains = ...,
    iter = ...,
    warmup = ...,
    thin = ...,
    algorithm = ...,

    # Parameter summaries (data frames with diagnostics)
    observation = list(
      fixed = data.frame(
        Estimate, Est.Error, l-95% CI, u-95% CI, Rhat, Bulk_ESS, Tail_ESS
      ),
      random = list(...),  # By grouping factor
      special = data.frame(...),  # sigma, shape, etc.
      smooth_params = data.frame(...)  # If smooths present
    ),

    trend = list(
      dynamics = data.frame(
        Estimate, Est.Error, l-95% CI, u-95% CI, Rhat, Bulk_ESS, Tail_ESS
      ),  # ar1_trend, sigma_trend, etc.
      fixed = data.frame(...),  # If trend_formula present
      smooth_params = data.frame(...)  # If smooths in trend_formula
    ),

    # Optional components
    prior = ...,  # If priors = TRUE
    WAIC = ...,   # If computed
    LOO = ...,    # If computed
    R2 = ...      # If computed
  ),
  class = "mvgam_summary"
)
```

**Output Format** (printed):
```
 Family: [family_name]
   Link: [link_function]
Formula: [observation_formula]
Trend Formula: [trend_formula]
Trend Model: [trend_specification]
   Data: [data_name] (Number of observations: [nobs])
 Draws: [chains] chains, each with iter = [iter]; warmup = [warmup]; thin = [thin]
        total post-warmup draws = [total_draws]

Observation Model - Population-Level Effects:
          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
Intercept     0.12      0.05    0.02     0.22 1.00     3245     2987
series2       0.45      0.08    0.30     0.60 1.01     3102     3234

Trend Model - Dynamics Parameters:
              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
sigma_trend       0.25      0.03    0.20     0.31 1.00     3456     3123
ar1_trend[1]      0.68      0.09    0.50     0.84 1.00     2987     3001
ar1_trend[2]      0.72      0.08    0.56     0.87 1.01     3123     2876
ar1_trend[3]      0.65      0.10    0.46     0.83 1.00     3234     3098

Family Specific Parameters:
       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
sigma      0.15      0.02    0.11     0.19 1.00     3567     3234

Draws were sampled using [algorithm]. For each parameter, Bulk_ESS
and Tail_ESS are effective sample size measures, and Rhat is the potential
scale reduction factor on split chains (at convergence, Rhat = 1).
```

**Warning System** (automatic):
```r
# Automatically checks and warns about:
- Rhat > 1.05: "The model has not converged (Rhat > 1.05). Consider increasing iterations or chains."
- ESS < 400: "The model contains low effective sample sizes. Consider thinning or increasing iterations."
- Divergent transitions: "There were X divergent transitions. This may indicate model misspecification."
- Max treedepth hits: "Maximum treedepth was reached. Consider increasing max_treedepth."
```

**Implementation Pattern**:
```r
summary.mvgam <- function(object, priors = FALSE, prob = 0.95,
                         robust = FALSE, mc_se = FALSE, ...) {
  # 1. Extract posterior draws using posterior package
  draws <- as_draws_df(object$model_output)

  # 2. Compute summaries via posterior::summarise_draws()
  summ <- summarise_draws(
    draws,
    mean, sd,
    ~quantile(.x, probs = c((1-prob)/2, 1-(1-prob)/2)),
    posterior::default_convergence_measures()  # Rhat, ESS
  )

  # 3. Organize by parameter category (observation vs trend)
  obs_summ <- extract_observation_summaries(summ, object)
  trend_summ <- extract_trend_summaries(summ, object)

  # 4. Create summary object
  out <- structure(
    list(
      formula = object$formula,
      trend_formula = object$trend_formula,
      # ... other metadata
      observation = obs_summ,
      trend = trend_summ,
      # ... optional components
    ),
    class = "mvgam_summary"
  )

  # 5. Check diagnostics and issue warnings
  check_convergence(out)

  return(out)
}
```

**Print Method**:
```r
print.mvgam_summary <- function(x, digits = 2, ...) {
  # Print model specification
  print_model_info(x)

  # Print observation model summaries
  print_summary_tables(x$observation, component = "Observation Model")

  # Print trend model summaries
  if (!is.null(x$trend)) {
    print_summary_tables(x$trend, component = "Trend Model")
  }

  # Print footer with interpretation notes
  print_diagnostic_footer()

  invisible(x)
}
```

### 3.6 `summary.mvgam_pooled()` (Multiple Imputation)

**Purpose**: Summary for multiple imputation with Rubin's rules diagnostics

**File Location**: `R/summary.mvgam.R`

**Function Signature**:
```r
summary.mvgam_pooled(object, priors = FALSE, prob = 0.95, ...)
```

**Output Structure** (extends mvgam_summary):
```r
structure(
  list(
    # Standard mvgam_summary components
    # ... (from base summary)

    # Multiple imputation specific
    mi_diagnostics = list(
      n_imputations = M,
      observation = list(
        avg_relative_increase = ...,  # Average RIV
        avg_fmi = ...,                # Average FMI
        avg_df = ...                  # Average degrees of freedom
      ),
      trend = list(...)
    ),
    pooling_method = "rubins_rules"
  ),
  class = c("mvgam_pooled_summary", "mvgam_summary")
)
```

**Output Format** (extends standard summary):
```
[... standard summary output ...]

Multiple Imputation Diagnostics (Rubin's Rules):
  Number of imputations: 10

  Average Fraction Missing Information (FMI): 0.23
  Average Relative Increase in Variance (RIV): 0.30
  Average Degrees of Freedom: 28.5

Note: Estimates and standard errors incorporate both within and between-imputation variability.
```

**Implementation**:
```r
summary.mvgam_pooled <- function(object, priors = FALSE, prob = 0.95, ...) {
  # Start with standard summary structure
  base_summary <- NextMethod()  # Calls summary.mvgam

  # Add multiple imputation diagnostics
  base_summary$mi_diagnostics <- extract_mi_diagnostics(object)
  base_summary$pooling_method <- attr(object, "pooling_method")

  class(base_summary) <- c("mvgam_pooled_summary", "mvgam_summary")
  return(base_summary)
}

print.mvgam_pooled_summary <- function(x, ...) {
  # Print standard summary
  NextMethod()  # Calls print.mvgam_summary

  # Add MI diagnostics section
  cat("\nMultiple Imputation Diagnostics (Rubin's Rules):\n")
  print_mi_diagnostics(x$mi_diagnostics)
  cat("\nNote: Estimates and standard errors incorporate both within and\n")
  cat("      between-imputation variability.\n")

  invisible(x)
}
```

### 3.7 Diagnostic Functions

**File Location**: `R/diagnostics.mvgam.R`

**Functions to Implement** (following brms patterns):

#### `rhat.mvgam()`
```r
rhat.mvgam <- function(x, pars = NULL, ...) {
  # Delegate to underlying fit object
  rhat(x$model_output, pars = pars, ...)
}
```

#### `neff_ratio.mvgam()`
```r
neff_ratio.mvgam <- function(x, pars = NULL, ...) {
  # Delegate to underlying fit object
  neff_ratio(x$model_output, pars = pars, ...)
}
```

#### `nuts_params.mvgam()`
```r
nuts_params.mvgam <- function(x, pars = NULL, ...) {
  # Extract NUTS-specific diagnostics
  # Returns data frame with divergent__, treedepth__, energy__, stepsize__
  nuts_params(x$model_output, pars = pars, ...)
}
```

#### `log_posterior.mvgam()`
```r
log_posterior.mvgam <- function(x, ...) {
  # For diagnostic plots
  log_posterior(x$model_output, ...)
}
```

#### Internal Warning Functions

**`check_convergence.mvgam()`**:
```r
check_convergence <- function(x) {
  # Check Rhat values
  rhats <- rhat(x)
  if (any(rhats > 1.05, na.rm = TRUE)) {
    warning(
      "The model has not converged (some Rhat > 1.05). ",
      "Consider increasing the number of iterations or chains.",
      call. = FALSE
    )
  }

  # Check effective sample sizes
  ess <- neff_ratio(x)
  if (any(ess < 0.001, na.rm = TRUE)) {  # < 400 for typical 400k draws
    warning(
      "The model contains very low effective sample sizes. ",
      "Consider thinning or increasing iterations.",
      call. = FALSE
    )
  }

  # Check for divergences (if NUTS used)
  if (algorithm_is_nuts(x)) {
    np <- nuts_params(x)
    if (any(np$divergent__)) {
      warning(
        "There were ", sum(np$divergent__), " divergent transitions after warmup. ",
        "Increasing adapt_delta above ", attr(x, "adapt_delta"),
        " may help. See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup",
        call. = FALSE
      )
    }

    # Check treedepth saturation
    max_td <- attr(x, "max_treedepth") %||% 10
    if (any(np$treedepth__ >= max_td)) {
      warning(
        sum(np$treedepth__ >= max_td), " of ", length(np$treedepth__),
        " (", round(100 * mean(np$treedepth__ >= max_td), 1), "%) ",
        "transitions hit the maximum treedepth limit of ", max_td, ". ",
        "Increasing max_treedepth may improve convergence.",
        call. = FALSE
      )
    }
  }
}
```

## 4. Functional Requirements

### FR1: Fitting Function Verification
The system must:
1. Successfully compile Stan models generated by `generate_combined_stancode()`
2. Execute MCMC sampling without errors for all trend types
3. Return properly structured mvgam objects with all required components
4. Handle edge cases (no trends, multivariate, multiple imputation) gracefully
5. Provide informative error messages for invalid configurations

### FR2: Object Structure Consistency
The system must create mvgam objects containing:
1. `obs_fit`: brmsfit-like object for observation model parameters
2. `trend_fit`: brmsfit-like object for trend model parameters (if trends present)
3. `combined_fit`: Full Stan fit object from backend
4. `formula`: Observation model formula
5. `trend_formula`: Trend model formula (if present)
6. `family`: Observation family specification
7. `data`: Original data used for fitting
8. `stancode`: Stan model code used
9. `standata`: Stan data list used
10. `mv_spec`: Multivariate specification
11. `trend_components`: Trend-specific metadata
12. `series_info`: Time series structure information
13. `time_info`: Temporal structure information
14. `trend_metadata`: Metadata for prediction validation

### FR3: Parameter Discovery
The system must provide:
1. `variables()` method returning all parameter names (Phase 1: simple character vector)
2. Parameter categorization helpers (observation vs trend)
3. Future extensibility for metadata-rich parameter information

### FR4: Print Method
The system must display:
1. Family and link function
2. Observation and trend formulas
3. Trend model specification (type and parameters)
4. Data information (name, number of observations)
5. MCMC sampling information (chains, iterations, warmup)
6. Population-level effects (Estimate, Est.Error only)
7. Trend parameters (Estimate, Est.Error only)
8. Family-specific parameters
9. Algorithm information
10. Fast computation (no credible intervals, no diagnostics)

### FR5: Summary Method
The system must display:
1. All information from print method
2. Credible intervals (default 95%, adjustable via prob argument)
3. MCMC diagnostics (Rhat, Bulk_ESS, Tail_ESS) for all parameters
4. Organized parameter tables (observation model, trend model, special parameters)
5. Optional Monte Carlo standard errors (mc_se = TRUE)
6. Optional robust estimates (median/MAD with robust = TRUE)
7. Optional prior specifications (priors = TRUE)
8. Automatic convergence warnings
9. Diagnostic footer explaining Rhat and ESS

### FR6: Multiple Imputation Support
The system must:
1. Pool parameter estimates using Rubin's rules
2. Compute within and between-imputation variance
3. Calculate relative increase in variance (RIV)
4. Calculate fraction of missing information (FMI)
5. Adjust degrees of freedom appropriately
6. Display pooled estimates with adjusted standard errors
7. Report multiple imputation diagnostics in summary
8. Store individual fits as attribute for inspection

### FR7: MCMC Diagnostics
The system must provide:
1. `rhat()` method for convergence diagnostics
2. `neff_ratio()` method for effective sample size
3. `nuts_params()` method for sampler diagnostics
4. `log_posterior()` method for diagnostic plotting
5. Automatic warning system checking:
   - Rhat > 1.05
   - ESS < 400 (absolute, not ratio)
   - Divergent transitions
   - Treedepth saturation
6. Informative warning messages with remediation suggestions

### FR8: Testing Infrastructure
The system must include:
1. Local tests covering univariate models (gaussian, poisson, beta)
2. Local tests covering multivariate models
3. Local tests covering multiple imputation workflow
4. Tests for all trend types (RW, AR, VAR, CAR, PW, ZMVN)
5. Tests verifying print/summary output structure
6. Tests verifying diagnostic computation
7. Tests verifying parameter extraction
8. Tests not run on CRAN (in tests/local directory)

## 5. Data Flow & Dependencies

### Input Dependencies
```
mvgam() entry point
  ↓
mvgam_formula object (from formula processing)
  ↓
generate_combined_stancode() (Stan code generation)
  ↓
combine_stan_data() (Stan data preparation)
  ↓
mvgam_single() or mvgam_multiple() (fitting)
  ↓
create_mvgam_from_combined_fit() (object creation)
  ↓
mvgam object ready for methods
```

### Method Dependencies
```
mvgam object
  ├─ print.mvgam() → Quick display
  ├─ summary.mvgam() → Detailed summary with diagnostics
  ├─ variables.mvgam() → Parameter discovery
  ├─ rhat.mvgam() → Convergence diagnostics
  ├─ neff_ratio.mvgam() → Effective sample size
  ├─ nuts_params.mvgam() → NUTS diagnostics
  └─ as_draws.mvgam() → Posterior package integration
```

### Package Dependencies
```r
# Current dependencies (already in mvgam)
Imports:
  brms (>= 2.20.0)
  cmdstanr (>= 0.5.0) or rstan (>= 2.26.0)
  insight (>= 0.19.0)

# Should add for full functionality
Imports:
  posterior (>= 1.0.0)    # For as_draws(), summarise_draws()

Suggests:
  bayesplot (>= 1.8.0)    # For diagnostic plots (future)
  loo (>= 2.4.0)          # For model comparison (future)
```

### Helper Function Dependencies

**Formatting Helpers**:
- `format_number()`: Format numeric output with controlled decimals
- `format_ci()`: Format credible intervals
- `format_formula()`: Pretty-print formulas
- `format_table()`: Format parameter tables

**Extraction Helpers**:
- `extract_observation_summaries()`: Get observation parameter summaries
- `extract_trend_summaries()`: Get trend parameter summaries
- `extract_mi_diagnostics()`: Get MI diagnostics from pooled object
- `extract_sampling_info()`: Get MCMC sampling information

**Computation Helpers**:
- `compute_summary_stats()`: Compute mean, SD, quantiles, etc.
- `compute_convergence_diagnostics()`: Compute Rhat, ESS
- `compute_rubins_rules()`: Pool MI estimates

## 6. User Interface Requirements

### Parameter Naming Conventions
```r
# Observation parameters (brms conventions)
"b_Intercept"           # Fixed effects
"b_serieseries2"
"sd_series__Intercept"  # Random effects SDs
"r_series[1,Intercept]" # Random effects
"s_1_1"                 # Smooth terms
"sigma"                 # Family parameters
"shape"
"nu"

# Trend parameters (mvgam conventions with _trend suffix)
"sigma_trend"           # Process variance
"ar1_trend"             # AR coefficients (vector)
"A1_trend"              # VAR matrices
"L_Omega_trend"         # Correlation matrices (Cholesky)
"Z"                     # Factor loadings
"trend"                 # Latent states (matrix)
"mu_trend"              # Trend linear predictors
"b_trend_Intercept"     # Trend formula fixed effects
"s_1_1_trend"           # Trend formula smooths
```

### Default Behaviors
1. **print()**: Shows estimates without CIs or diagnostics (fast, interactive use)
2. **summary()**: Shows 95% credible intervals with diagnostics (comprehensive reporting)
3. **Warnings**: Automatically displayed on print/summary if issues detected
4. **Digits**: Default to 2 decimal places (adjustable)

### Progress Indicators
For long-running operations:
```r
# Multiple imputation fitting
"Fitting mvgam models to 10 imputed datasets..."
"Fitting imputation 1 of 10..."
"Fitting imputation 2 of 10..."
...
"Pooling results using Rubin's rules..."
```

## 7. Error Handling & Validation

### Input Validation Requirements

**mvgam_single()**:
```r
checkmate::assert_formula(formula)
checkmate::assert_data_frame(data)
checkmate::assert_character(backend, len = 1)
checkmate::assert_class(family, "family")
# Validate trend_formula if present
if (!is.null(trend_formula)) {
  checkmate::assert_formula(trend_formula)
}
```

**mvgam_multiple()**:
```r
checkmate::assert_formula(formula)
checkmate::assert_list(data_list, min.len = 2, types = "data.frame")
checkmate::assert_logical(combine, len = 1)
# Validate dataset consistency
validate_multiple_imputation_datasets(data_list)
```

**summary.mvgam()**:
```r
checkmate::assert_class(object, "mvgam")
checkmate::assert_number(prob, lower = 0, upper = 1)
checkmate::assert_logical(priors, len = 1)
checkmate::assert_logical(robust, len = 1)
checkmate::assert_logical(mc_se, len = 1)
```

### Error Messages

**Missing required components**:
```r
if (is.null(object$model_output)) {
  insight::format_error(
    "Stan fit not found in mvgam object.",
    "The object may be corrupted or from an incompatible version."
  )
}
```

**Incompatible operations**:
```r
if (is.null(object$trend_formula) && variable == "trend_params") {
  insight::format_error(
    "No trend parameters in model.",
    "The model was fitted without a {.field trend_formula}."
  )
}
```

**Convergence issues** (warnings, not errors):
```r
warning(
  "The model has not converged (some Rhat > 1.05).\n",
  "  Parameters with Rhat > 1.05: ", paste(bad_params, collapse = ", "), "\n",
  "  Consider:\n",
  "    - Increasing 'iter' (current: ", object$iter, ")\n",
  "    - Increasing 'chains' (current: ", object$chains, ")\n",
  "    - Checking for model misspecification\n",
  "  See http://mc-stan.org/misc/warnings.html#r-hat for details.",
  call. = FALSE
)
```

### Graceful Degradation

**Missing diagnostics** (e.g., variational inference):
```r
# If Rhat not available (e.g., algorithm = "meanfield")
if (algorithm_has_diagnostics(object)) {
  # Show full diagnostics
} else {
  # Show estimates without Rhat/ESS columns
  cat("Note: MCMC diagnostics not available for algorithm '",
      algorithm_name(object), "'.\n")
}
```

**Partial output on error**:
```r
# If trend summary fails, still show observation summary
tryCatch({
  trend_summ <- extract_trend_summaries(...)
}, error = function(e) {
  warning("Could not compute trend summaries: ", e$message)
  trend_summ <- NULL
})
```

## 8. Examples & Usage Patterns

### Basic Usage (Single Dataset)

**Simple model fit and inspection**:
```r
# Fit model
fit <- mvgam(
  y ~ s(season, bs = 'cc'),
  trend_formula = ~ s(habitat),
  data = dat,
  family = poisson(),
  chains = 4,
  iter = 2000
)

# Quick inspection
print(fit)

# Comprehensive summary
summary(fit)

# Check specific diagnostics
rhat(fit)
neff_ratio(fit)

# Discover parameters
variables(fit)

# Extract specific parameters
trend_pars <- as.matrix(fit, variable = "trend_params")
```

### Multiple Imputation Usage

**MI workflow**:
```r
# Create list of imputed datasets
library(mice)
imp <- mice(dat_with_missing, m = 10)
data_list <- lapply(1:10, function(i) complete(imp, i))

# Fit with pooling
fit_pooled <- mvgam(
  y ~ x1 + x2,
  trend_formula = ~ AR(p = 1),
  data = data_list,
  family = gaussian(),
  combine = TRUE
)

# Summary shows pooled estimates with MI diagnostics
summary(fit_pooled)

# Access individual fits if needed
individual_fits <- attr(fit_pooled, "individual_fits")
summary(individual_fits[[1]])  # First imputation
```

### Diagnostic Workflow

**Checking convergence**:
```r
# Automatic warnings on summary
summ <- summary(fit)

# Manual diagnostic checks
rhats <- rhat(fit)
max(rhats, na.rm = TRUE)  # Should be < 1.05

ess_ratios <- neff_ratio(fit)
min(ess_ratios, na.rm = TRUE)  # Should be > 0.001 (for typical setup)

# NUTS-specific diagnostics
np <- nuts_params(fit)
sum(np$divergent__)  # Count divergences
table(np$treedepth__)  # Check treedepth distribution
```

### Integration with Existing Functions

**Parameter extraction for plotting**:
```r
# Extract for custom plots
library(bayesplot)
draws <- as_draws_array(fit, variable = "trend_params")
mcmc_trace(draws)

# Extract observation parameters
obs_draws <- as.matrix(fit, variable = "betas")

# Extract trend states
trend_states <- as.matrix(fit, variable = "trend", regex = TRUE)
```

## 9. Testing Requirements

### Test Organization

**File**: `tests/local/test-fitting-and-methods.R`

**Test Categories**:
1. Fitting verification tests
2. Object structure tests
3. Print method tests
4. Summary method tests
5. Diagnostic method tests
6. Multiple imputation tests
7. Parameter extraction tests

### Unit Tests for Fitting Functions

**Test: mvgam_single() basic functionality**:
```r
test_that("mvgam_single fits gaussian AR model", {
  # Use simple simulated data
  sim_data <- sim_mvgam(family = gaussian(), T = 60,
                       trend_model = 'AR1', n_series = 2)

  fit <- mvgam_single(
    formula = y ~ s(season, bs = 'cc', k = 5),
    trend_formula = ~ -1,
    data = sim_data$data_train,
    backend = "cmdstanr",
    family = gaussian(),
    iter = 500,
    chains = 2
  )

  # Check object structure
  expect_s3_class(fit, "mvgam")
  expect_s3_class(fit, "brmsfit")
  expect_true(!is.null(fit$model_output))
  expect_true(!is.null(fit$formula))
  expect_true(!is.null(fit$trend_formula))
  expect_true(!is.null(fit$family))
})
```

**Test: mvgam_single() multivariate model**:
```r
test_that("mvgam_single fits multivariate model", {
  # Multivariate data
  sim_data <- sim_mvgam(family = c(poisson(), gaussian()),
                       T = 60, n_series = 2)

  fit <- mvgam_single(
    formula = mvbind(count, biomass) ~ temp,
    trend_formula = ~ AR(p = 1, cor = TRUE),
    data = sim_data$data_train,
    backend = "cmdstanr",
    family = c(poisson(), gaussian()),
    iter = 500,
    chains = 2
  )

  expect_s3_class(fit, "mvgam")
  expect_length(fit$response_names, 2)
  expect_equal(fit$response_names, c("count", "biomass"))
})
```

**Test: mvgam_multiple() basic pooling**:
```r
test_that("mvgam_multiple pools MI results correctly", {
  # Create simple imputed datasets
  sim_data <- sim_mvgam(family = gaussian(), T = 60,
                       trend_model = 'AR1', prop_missing = 0.2)

  # Create 5 imputations (simplified for testing)
  library(mice)
  imp <- mice(sim_data$data_train, m = 5, printFlag = FALSE)
  data_list <- lapply(1:5, function(i) complete(imp, i))

  fit <- mvgam_multiple(
    formula = y ~ s(season, bs = 'cc'),
    trend_formula = ~ -1,
    data_list = data_list,
    backend = "cmdstanr",
    family = gaussian(),
    combine = TRUE,
    iter = 500,
    chains = 2
  )

  # Check pooled object structure
  expect_s3_class(fit, "mvgam_pooled")
  expect_s3_class(fit, "mvgam")
  expect_true(!is.null(fit$pooled_estimates))
  expect_true(!is.null(attr(fit, "individual_fits")))
  expect_equal(attr(fit, "n_imputations"), 5)

  # Check Rubin's rules components
  expect_true(!is.null(fit$pooled_estimates$observation$within_variance))
  expect_true(!is.null(fit$pooled_estimates$observation$between_variance))
  expect_true(!is.null(fit$pooled_estimates$observation$relative_increase))
})
```

### Unit Tests for Print/Summary Methods

**Test: print.mvgam() output structure**:
```r
test_that("print.mvgam produces correct output", {
  # Fit simple model
  sim_data <- sim_mvgam(family = poisson(), T = 60, trend_model = 'AR1')
  fit <- mvgam_single(
    y ~ s(season, bs = 'cc'),
    trend_formula = ~ -1,
    data = sim_data$data_train,
    family = poisson(),
    iter = 500, chains = 2
  )

  # Capture output
  output <- capture.output(print(fit))

  # Check key sections present
  expect_true(any(grepl("Family:", output)))
  expect_true(any(grepl("Formula:", output)))
  expect_true(any(grepl("Trend Formula:", output)))
  expect_true(any(grepl("Population-Level Effects:", output)))
  expect_true(any(grepl("Trend Parameters:", output)))

  # Check no credible intervals (just Estimate and Est.Error)
  param_section <- output[grepl("Population-Level", output):length(output)]
  expect_false(any(grepl("CI", param_section)))
  expect_false(any(grepl("Rhat", param_section)))
})
```

**Test: summary.mvgam() includes diagnostics**:
```r
test_that("summary.mvgam includes MCMC diagnostics", {
  # Fit simple model
  sim_data <- sim_mvgam(family = gaussian(), T = 60, trend_model = 'AR1')
  fit <- mvgam_single(
    y ~ s(season, bs = 'cc'),
    trend_formula = ~ -1,
    data = sim_data$data_train,
    family = gaussian(),
    iter = 500, chains = 2
  )

  # Get summary
  summ <- summary(fit)

  # Check structure
  expect_s3_class(summ, "mvgam_summary")
  expect_true(!is.null(summ$observation))
  expect_true(!is.null(summ$trend))

  # Check diagnostics present in tables
  expect_true("Rhat" %in% names(summ$observation$fixed))
  expect_true("Bulk_ESS" %in% names(summ$observation$fixed))
  expect_true("Tail_ESS" %in% names(summ$observation$fixed))

  # Check credible intervals present
  expect_true(any(grepl("CI", names(summ$observation$fixed))))

  # Capture printed output
  output <- capture.output(print(summ))
  expect_true(any(grepl("Rhat", output)))
  expect_true(any(grepl("ESS", output)))
})
```

**Test: summary.mvgam() prob argument works**:
```r
test_that("summary.mvgam respects prob argument", {
  sim_data <- sim_mvgam(family = gaussian(), T = 60, trend_model = 'AR1')
  fit <- mvgam_single(
    y ~ 1,
    trend_formula = ~ -1,
    data = sim_data$data_train,
    family = gaussian(),
    iter = 500, chains = 2
  )

  # 90% intervals
  summ_90 <- summary(fit, prob = 0.90)
  expect_true(any(grepl("l-90%", names(summ_90$observation$fixed))))
  expect_true(any(grepl("u-90%", names(summ_90$observation$fixed))))

  # 99% intervals
  summ_99 <- summary(fit, prob = 0.99)
  expect_true(any(grepl("l-99%", names(summ_99$observation$fixed))))
  expect_true(any(grepl("u-99%", names(summ_99$observation$fixed))))
})
```

### Integration Tests with Test Models

**Use existing test models**:
```r
# Based on tests/local/tests-models1.R patterns
test_that("methods work with gaussian AR model from test suite", {
  gaus_data <- sim_mvgam(
    family = gaussian(),
    T = 60,
    trend_model = 'AR1',
    seasonality = 'shared',
    mu = c(-1, 0, 1),
    prop_trend = 0.5
  )

  fit <- mvgam(
    y ~ s(series, bs = 're') + s(season, bs = 'cc', k = 5) - 1,
    trend_formula = ~ -1,
    data = gaus_data$data_train,
    family = gaussian(),
    iter = 500,
    chains = 2
  )

  # All methods should work
  expect_output(print(fit))
  expect_s3_class(summary(fit), "mvgam_summary")
  expect_type(variables(fit), "character")
  expect_type(rhat(fit), "double")
  expect_type(neff_ratio(fit), "double")
})

test_that("methods work with CAR model from test suite", {
  # Use CAR test pattern from tests-models1.R
  dat <- create_car_test_data()  # Helper from test setup

  fit <- mvgam(
    formula = y ~ s(season, bs = 'cc', k = 5, by = series),
    trend_formula = ~ -1,
    trend_model = CAR(),
    data = dat,
    family = gaussian(),
    iter = 500,
    chains = 2
  )

  expect_output(print(fit))
  expect_s3_class(summary(fit), "mvgam_summary")

  # CAR-specific parameters should be present
  vars <- variables(fit)
  expect_true(any(grepl("ar1_trend", vars)))
  expect_true(any(grepl("sigma_trend", vars)))
})
```

### Diagnostic Function Tests

**Test: rhat.mvgam() works correctly**:
```r
test_that("rhat.mvgam returns valid convergence diagnostics", {
  sim_data <- sim_mvgam(family = gaussian(), T = 60, trend_model = 'AR1')
  fit <- mvgam_single(
    y ~ 1,
    trend_formula = ~ -1,
    data = sim_data$data_train,
    family = gaussian(),
    iter = 1000, chains = 4  # More chains for valid Rhat
  )

  rhats <- rhat(fit)

  # Check structure
  expect_type(rhats, "double")
  expect_true(length(rhats) > 0)
  expect_true(all(names(rhats) %in% variables(fit)))

  # Check validity (should be close to 1 for converged model)
  expect_true(all(rhats < 1.1, na.rm = TRUE))
})
```

**Test: Warning system triggers correctly**:
```r
test_that("convergence warnings trigger appropriately", {
  # Fit poorly (too few iterations)
  sim_data <- sim_mvgam(family = gaussian(), T = 60, trend_model = 'AR1')

  expect_warning({
    fit <- mvgam_single(
      y ~ 1,
      trend_formula = ~ -1,
      data = sim_data$data_train,
      family = gaussian(),
      iter = 50,  # Very low - should not converge
      chains = 2
    )
    summary(fit)
  }, regexp = "not converged|Rhat")
})
```

## 10. Documentation Requirements

### Roxygen2 Documentation

**print.mvgam()**:
```r
#' Print method for mvgam objects
#'
#' @description
#' Prints a concise summary of a fitted \code{mvgam} model including model
#' specification, parameter estimates without credible intervals, and sampling
#' information. For more detailed output including MCMC diagnostics, use
#' \code{\link{summary.mvgam}}.
#'
#' @param x An object of class \code{mvgam}.
#' @param digits Integer indicating the number of decimal places to display.
#'   Default is 2.
#' @param ... Additional arguments (currently unused).
#'
#' @return The \code{mvgam} object is returned invisibly.
#'
#' @details
#' The print method displays:
#' \itemize{
#'   \item Family and link function
#'   \item Observation and trend formulas
#'   \item Trend model specification
#'   \item Data information
#'   \item MCMC sampling details
#'   \item Parameter estimates (Estimate and Est.Error only)
#' }
#'
#' Unlike \code{summary.mvgam()}, this method does not compute credible
#' intervals or MCMC diagnostics, making it faster for interactive use.
#'
#' @seealso \code{\link{summary.mvgam}}, \code{\link{mvgam}}
#'
#' @examples
#' \donttest{
#' # Fit simple model
#' sim_data <- sim_mvgam(family = gaussian(), T = 60)
#' fit <- mvgam(y ~ s(season, bs = 'cc'),
#'              trend_formula = ~ -1,
#'              data = sim_data$data_train,
#'              family = gaussian())
#'
#' # Quick print
#' print(fit)
#'
#' # Equivalent to just typing the object name
#' fit
#' }
#'
#' @export
```

**summary.mvgam()**:
```r
#' Comprehensive summary of mvgam model fits
#'
#' @description
#' Provides detailed summary statistics for fitted \code{mvgam} models including
#' parameter estimates, credible intervals, and MCMC convergence diagnostics.
#'
#' @param object An object of class \code{mvgam}.
#' @param priors Logical; if \code{TRUE}, include prior specifications in the
#'   summary. Default is \code{FALSE}.
#' @param prob Numeric between 0 and 1 specifying the probability mass for
#'   credible intervals. Default is 0.95 (95\% intervals).
#' @param robust Logical; if \code{TRUE}, use median and MAD instead of mean
#'   and SD for parameter summaries. Default is \code{FALSE}.
#' @param mc_se Logical; if \code{TRUE}, include Monte Carlo standard errors.
#'   Default is \code{FALSE}.
#' @param ... Additional arguments (currently unused).
#'
#' @return An object of class \code{mvgam_summary} containing:
#' \describe{
#'   \item{observation}{List with observation model parameter summaries}
#'   \item{trend}{List with trend model parameter summaries (if present)}
#'   \item{formula}{Model formula}
#'   \item{family}{Response distribution family}
#'   \item{nobs}{Number of observations}
#'   \item{chains, iter, warmup}{MCMC sampling information}
#' }
#'
#' @details
#' Parameter summary tables include:
#' \itemize{
#'   \item \code{Estimate}: Posterior mean (or median if \code{robust = TRUE})
#'   \item \code{Est.Error}: Posterior SD (or MAD if \code{robust = TRUE})
#'   \item Credible interval bounds (based on \code{prob})
#'   \item \code{Rhat}: Gelman-Rubin convergence diagnostic
#'   \item \code{Bulk_ESS}: Bulk effective sample size
#'   \item \code{Tail_ESS}: Tail effective sample size
#' }
#'
#' The summary automatically checks convergence and issues warnings if:
#' \itemize{
#'   \item Any Rhat > 1.05
#'   \item Any effective sample size < 400
#'   \item Divergent transitions detected (for NUTS)
#'   \item Maximum treedepth reached (for NUTS)
#' }
#'
#' @seealso \code{\link{print.mvgam}}, \code{\link{mvgam}},
#'   \code{\link{rhat.mvgam}}, \code{\link{neff_ratio.mvgam}}
#'
#' @examples
#' \donttest{
#' # Fit model
#' sim_data <- sim_mvgam(family = poisson(), T = 60)
#' fit <- mvgam(y ~ s(season, bs = 'cc'),
#'              trend_formula = ~ AR(p = 1),
#'              data = sim_data$data_train,
#'              family = poisson())
#'
#' # Detailed summary
#' summary(fit)
#'
#' # 90% credible intervals
#' summary(fit, prob = 0.90)
#'
#' # Robust estimates
#' summary(fit, robust = TRUE)
#'
#' # Include priors
#' summary(fit, priors = TRUE)
#' }
#'
#' @export
```

**variables.mvgam()**:
```r
#' Extract parameter names from mvgam objects
#'
#' @description
#' Returns the names of all parameters in a fitted \code{mvgam} model. This
#' follows the \pkg{brms} convention and delegates to the underlying Stan fit
#' object.
#'
#' @param x An object of class \code{mvgam}.
#' @param ... Additional arguments passed to the underlying \code{variables}
#'   method.
#'
#' @return A character vector of parameter names.
#'
#' @details
#' Parameter names follow these conventions:
#' \describe{
#'   \item{Observation model}{Standard \pkg{brms} naming (e.g., \code{b_},
#'     \code{sigma}, \code{s_})}
#'   \item{Trend model}{Parameters suffixed with \code{_trend} (e.g.,
#'     \code{sigma_trend}, \code{ar1_trend})}
#' }
#'
#' Use \code{\link{as.data.frame.mvgam}} or \code{\link{as.matrix.mvgam}} with
#' the \code{variable} argument to extract specific parameter types.
#'
#' @seealso \code{\link{as.data.frame.mvgam}}, \code{\link{as.matrix.mvgam}}
#'
#' @examples
#' \donttest{
#' # Fit model
#' sim_data <- sim_mvgam(family = gaussian(), T = 60)
#' fit <- mvgam(y ~ s(season, bs = 'cc'),
#'              trend_formula = ~ AR(p = 1),
#'              data = sim_data$data_train,
#'              family = gaussian())
#'
#' # Get all parameter names
#' all_pars <- variables(fit)
#' head(all_pars)
#'
#' # Find trend parameters
#' trend_pars <- all_pars[grepl("_trend", all_pars)]
#' trend_pars
#' }
#'
#' @export
```

### Internal Function Documentation

All helper functions should have \code{@noRd} and clear documentation:

```r
#' Extract observation model parameter summaries
#'
#' @param summ_df Data frame from posterior::summarise_draws()
#' @param object mvgam object
#' @return List with categorized observation parameter summaries
#' @noRd
extract_observation_summaries <- function(summ_df, object) {
  # Implementation
}

#' Format parameter table for printing
#'
#' @param params Data frame with parameter summaries
#' @param digits Number of decimal places
#' @param detailed Logical, include CIs and diagnostics?
#' @return Formatted character matrix for printing
#' @noRd
format_parameter_table <- function(params, digits = 2, detailed = TRUE) {
  # Implementation
}
```

## 11. Implementation Notes for Developers

### Code Organization Principles

1. **Separate files for each major method** (as specified by user):
   - `R/summary.mvgam.R`: All summary-related functions
   - `R/diagnostics.mvgam.R`: All diagnostic functions
   - `R/variables.mvgam.R`: Parameter discovery functions
   - `R/print.mvgam.R`: Print methods (extend existing file)

2. **Helper functions in appropriate files**:
   - Formatting helpers: In same file as method that uses them
   - Extraction helpers: In file corresponding to component (observation/trend)
   - Shared utilities: In existing `R/helpers.R` or `R/utils.R`

3. **Consistent naming conventions**:
   - S3 methods: `method.mvgam()`
   - Helpers: `extract_*()`, `format_*()`, `print_*()`, `check_*()`, `compute_*()`
   - Internal: All helpers marked `@noRd`

### Dependency Management

**Import from posterior package**:
```r
#' @importFrom posterior as_draws_df as_draws_array as_draws_matrix
#'   summarise_draws default_convergence_measures
```

**Import from insight package**:
```r
#' @importFrom insight format_error format_warning format_message
```

**Conditional imports for diagnostics**:
```r
# Only import if actually used
if (requireNamespace("bayesplot", quietly = TRUE)) {
  # Use bayesplot functions
}
```

### Performance Considerations

1. **Lazy computation**: Don't compute summaries until needed
2. **Caching**: Store summary objects to avoid recomputation
3. **Efficient extraction**: Use `posterior::summarise_draws()` (optimized)
4. **Minimal overhead in print()**: No credible intervals or diagnostics

### Debugging Approaches

**Enable verbose output during development**:
```r
options(mvgam.verbose = TRUE)  # Enable debug messages

# In functions
if (getOption("mvgam.verbose", FALSE)) {
  message("Computing observation summaries...")
}
```

**Validation checks**:
```r
# Use checkmate::assert_* for all inputs
# Use stopifnot() for internal invariants
# Use testthat::expect_* in tests
```

## 12. Non-Goals (Explicit Boundaries)

This task will **NOT** include:

1. **Backward compatibility**: Breaking changes are acceptable - no support for old mvgam object structures
2. **Model comparison methods**: LOO, WAIC integration deferred to future work
3. **Enhanced plotting**: Integration with bayesplot deferred to future work
4. **Custom diagnostic plots**: Focus on numeric diagnostics only
5. **Metadata-rich variables()**: Phase 1 is simple character vector, enhanced version is future work
6. **predict() method**: Out of scope for this task
7. **forecast() method verification**: Out of scope (existing functionality)
8. **Cross-validation methods**: Out of scope for this task
9. **Documentation website updates**: Only roxygen2 docs required

## 13. Success Criteria

### Verification Success
1. ✅ `mvgam_single()` successfully renamed from `mvgam_single_dataset()`
2. ✅ All test models from `tests/local/tests-models1.R` run without errors
3. ✅ New multiple imputation test passes
4. ✅ Object structure matches specification in FR2
5. ✅ No test errors or warnings in local test suite

### Method Implementation Success
1. ✅ `print.mvgam()` displays all required sections (FR4)
2. ✅ `summary.mvgam()` includes diagnostics and CIs (FR5)
3. ✅ `summary.mvgam_pooled()` shows MI diagnostics (FR6)
4. ✅ `variables.mvgam()` returns parameter names (FR3)
5. ✅ Diagnostic methods (`rhat`, `neff_ratio`, etc.) work (FR7)
6. ✅ Warning system triggers on convergence issues (FR7)

### Testing Success
1. ✅ All unit tests pass
2. ✅ Integration tests with existing test models pass
3. ✅ Coverage of all trend types (RW, AR, VAR, CAR, PW, ZMVN)
4. ✅ Coverage of multivariate models
5. ✅ Coverage of multiple imputation workflow

### Documentation Success
1. ✅ All exported functions have roxygen2 documentation
2. ✅ Examples run without errors
3. ✅ Help pages build correctly
4. ✅ Internal functions have `@noRd` with clear comments

### User Acceptance Criteria
1. User can fit models and immediately inspect with `print()`
2. User can get detailed diagnostics with `summary()`
3. User receives automatic warnings for convergence issues
4. User can discover parameters with `variables()`
5. User can extract parameters for custom analysis
6. Multiple imputation workflow matches brms patterns
7. All outputs are clearly formatted and informative

## 14. Open Questions

### Technical Decisions
1. **Question**: Should `variables.mvgam()` include `use_alias` parameter like `as.data.frame.mvgam()`?
   - **Impact**: Would allow returning user-friendly names instead of Stan names
   - **Recommendation**: Defer to Phase 2 (metadata enhancement)

2. **Question**: Should summary cache computed diagnostics in the mvgam object?
   - **Impact**: Faster subsequent calls but larger object size
   - **Recommendation**: No caching initially; add if performance issues arise

3. **Question**: Should we implement `print.mvgam_summary()` or rely on default?
   - **Impact**: Custom print method provides better formatting control
   - **Recommendation**: Yes, implement custom print method (matches brms)

### User Experience Questions
1. **Question**: What level of detail should warnings provide?
   - **Current approach**: Follow brms conventions with remediation suggestions
   - **Alternative**: More verbose with links to documentation

2. **Question**: Should `summary()` always check diagnostics (slower) or have option to skip?
   - **Recommendation**: Always check (matches brms), performance should be acceptable

### Implementation Uncertainties
1. **Question**: How to handle variational inference (no Rhat/ESS)?
   - **Recommendation**: Graceful degradation - show estimates without diagnostics, add note

2. **Question**: Should we validate that all test models actually converge?
   - **Recommendation**: Yes, use `iter = 1000+` and `chains = 4` in tests to ensure valid diagnostics

## 15. Timeline and Phasing

### Phase 1: Verification and Renaming (Priority 1)
- Rename `mvgam_single_dataset()` → `mvgam_single()`
- Verify both fitting functions work correctly
- Create basic tests for object structure
- **Deliverable**: Working fitting functions with verified output

### Phase 2: Print Method (Priority 1)
- Implement `print.mvgam()`
- Implement helper functions for formatting
- Add tests for print output
- **Deliverable**: Working print method

### Phase 3: Summary Method (Priority 1)
- Implement `summary.mvgam()`
- Implement `print.mvgam_summary()`
- Add convergence checking and warnings
- Add tests for summary output
- **Deliverable**: Working summary method with diagnostics

### Phase 4: Multiple Imputation (Priority 2)
- Implement `summary.mvgam_pooled()`
- Implement `print.mvgam_pooled_summary()`
- Add MI diagnostic extraction
- Add MI tests
- **Deliverable**: Full MI support

### Phase 5: Supporting Methods (Priority 2)
- Implement `variables.mvgam()`
- Implement diagnostic methods (`rhat`, `neff_ratio`, etc.)
- Add tests for all methods
- **Deliverable**: Complete method suite

### Phase 6: Testing and Documentation (Priority 1)
- Create comprehensive test suite in `tests/local/`
- Complete all roxygen2 documentation
- Verify all examples run
- **Deliverable**: Fully tested and documented implementation

### Phase 7: Future Enhancements (Deferred)
- Enhanced `variables()` with metadata
- Integration with bayesplot
- LOO/WAIC integration
- Custom diagnostic plots
