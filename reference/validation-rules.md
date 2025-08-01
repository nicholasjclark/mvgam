# MVGam Validation Rules

**Purpose**: Comprehensive validation framework for mvgam-brms integration

## Formula Validation

### 1. Autocorrelation Conflict Detection
```r
validate_autocor_separation <- function(formula, trend_formula) {
  # Extract brms autocorrelation terms
  obs_autocor <- extract_brms_autocor_terms(formula)
  trend_autocor <- extract_brms_autocor_terms(trend_formula)
  
  # Allow observation-level autocorr
  if (length(obs_autocor) > 0) {
    insight::format_message(
      "Using brms autocorrelation for observation-level residual structure.",
      "This complements State-Space trends in trend_formula."
    )
  }
  
  # Prevent trend-level conflicts
  if (length(trend_autocor) > 0) {
    conflicting_terms <- paste(trend_autocor, collapse = ", ")
    stop(insight::format_error(
      sprintf("brms autocorrelation terms not allowed in trend_formula: %s", 
              conflicting_terms),
      "Use mvgam trend types instead:",
      "  ar(p = 1) → AR(p = 1)",
      "  ma(q = 1) → MA(q = 1)",
      "  arma(p, q) → ARMA(p = p, q = q)"
    ))
  }
}

# Pattern matching for brms autocorr terms
extract_brms_autocor_terms <- function(formula) {
  formula_str <- deparse(formula)
  autocor_patterns <- c("ar\\(", "ma\\(", "arma\\(", "cosy\\(", "unstr\\(")
  found_terms <- character(0)
  
  for (pattern in autocor_patterns) {
    if (grepl(pattern, formula_str)) {
      # Extract specific term for error message
      matches <- regmatches(formula_str, gregexpr(paste0(pattern, "[^)]*\\)"), formula_str))
      found_terms <- c(found_terms, unlist(matches))
    }
  }
  
  return(found_terms)
}
```

### 2. Distributional Model Validation
```r
validate_distributional_trends <- function(bterms, trend_spec) {
  # Extract distributional parameters
  dist_pars <- names(bterms$dpars)
  main_par <- bterms$rescor  # Usually "mu"
  aux_pars <- setdiff(dist_pars, main_par)
  
  # Check if trends specified for auxiliary parameters
  trend_targets <- extract_trend_targets(trend_spec)
  invalid_targets <- intersect(trend_targets, aux_pars)
  
  if (length(invalid_targets) > 0) {
    stop(insight::format_error(
      sprintf("Trends not allowed on auxiliary parameters: %s", 
              paste(invalid_targets, collapse = ", ")),
      sprintf("Trends only supported for main parameter: %s", main_par),
      "For distributional regression:",
      "  ✅ bf(y ~ s(x) + AR(p = 1), sigma ~ s(z))",
      "  ❌ bf(y ~ s(x), sigma ~ s(z) + AR(p = 1))"
    ))
  }
}
```

### 3. Multivariate Formula Validation
```r
validate_multivariate_trends <- function(mvbf_formula, trend_formula) {
  # Extract response variables
  responses <- extract_response_vars(mvbf_formula)
  
  # Extract trend specifications
  trend_responses <- extract_trend_response_targets(trend_formula)
  
  # Check for undefined responses in trends
  undefined_responses <- setdiff(trend_responses, responses)
  if (length(undefined_responses) > 0) {
    stop(insight::format_error(
      sprintf("Trend specified for undefined responses: %s", 
              paste(undefined_responses, collapse = ", ")),
      sprintf("Available responses: %s", paste(responses, collapse = ", "))
    ))
  }
  
  # Validate response-specific trend syntax
  for (resp in trend_responses) {
    trend_spec <- extract_response_trend_spec(trend_formula, resp)
    validate_single_trend_specification(trend_spec, resp)
  }
}
```

## Data Validation

### 1. Time Series Structure Validation
```r
validate_time_series_structure <- function(data, time_var, series_var = NULL) {
  # Check time variable exists and is appropriate type
  if (!time_var %in% names(data)) {
    stop(insight::format_error(
      sprintf("Time variable '%s' not found in data", time_var)
    ))
  }
  
  time_values <- data[[time_var]]
  if (!is.numeric(time_values) && !inherits(time_values, c("Date", "POSIXt"))) {
    stop(insight::format_error(
      sprintf("Time variable '%s' must be numeric, Date, or POSIXt", time_var)
    ))
  }
  
  # Validate series structure if specified
  if (!is.null(series_var)) {
    validate_series_structure(data, time_var, series_var)
  }
  
  # Check for regular time spacing within series
  # if useing AR, VAR, RW, PW; not relevant for CAR models
}
```

### 2. Multiple Imputation Data Validation
```r
validate_multiple_imputation_data <- function(data_list) {
  if (!is.list(data_list)) {
    stop(insight::format_error(
      "Multiple imputation data must be a list of data.frames"
    ))
  }
  
  if (length(data_list) < 2) {
    stop(insight::format_error(
      "Multiple imputation requires at least 2 datasets"
    ))
  }
  
  # Check structure consistency
  reference_names <- names(data_list[[1]])
  reference_dims <- dim(data_list[[1]])
  
  for (i in seq_along(data_list)) {
    dataset <- data_list[[i]]
    
    # Check variable names
    if (!identical(names(dataset), reference_names)) {
      stop(insight::format_error(
        sprintf("Dataset %d has different variable names", i),
        "All imputed datasets must have identical structure"
      ))
    }
    
    # Check dimensions
    if (!identical(dim(dataset), reference_dims)) {
      stop(insight::format_error(
        sprintf("Dataset %d has different dimensions", i),
        sprintf("Expected: %s, Got: %s", 
                paste(reference_dims, collapse = " x "),
                paste(dim(dataset), collapse = " x "))
      ))
    }
  }
  
  # Check time series consistency
  validate_imputation_time_consistency(data_list)
}

validate_imputation_time_consistency <- function(data_list) {
  # Time structure should be identical across imputations
  reference_time_structure <- extract_time_structure(data_list[[1]])
  
  for (i in 2:length(data_list)) {
    current_structure <- extract_time_structure(data_list[[i]])
    
    if (!identical(reference_time_structure, current_structure)) {
      stop(insight::format_error(
        sprintf("Dataset %d has different time structure", i),
        "Time variables must be identical across all imputations"
      ))
    }
  }
}
```

## Trend Validation

### 1. Factor Model Compatibility
```r
validate_factor_model_compatibility <- function(trend_type, n_lv, n_series) {
  # Define compatibility rules
  FACTOR_COMPATIBLE <- c("AR", "RW", "VAR", "ZMVN")
  FACTOR_INCOMPATIBLE <- c("PW", "CAR")
  
  # Only check if factor model is specified
  if (n_lv >= n_series) {
    return(TRUE)  # Not a factor model
  }
  
  if (trend_type %in% FACTOR_INCOMPATIBLE) {
    stop(insight::format_error(
      sprintf("Trend type '%s' incompatible with factor models", trend_type),
      sprintf("Factor model detected: n_lv (%d) < n_series (%d)", n_lv, n_series),
      "Compatible trends for factor models:",
      paste("  ", FACTOR_COMPATIBLE, collapse = "\n"),
      "Reason: Series-specific dynamics conflict with factor structure"
    ))
  }
  
  if (!trend_type %in% FACTOR_COMPATIBLE) {
    insight::format_warning(
      sprintf("Trend type '%s' compatibility with factor models not verified", 
              trend_type),
      "Proceed with caution or use tested trends:",
      paste("  ", FACTOR_COMPATIBLE, collapse = "\n")
    )
  }
}
```

### 2. Trend Parameter Validation
```r
validate_trend_parameters <- function(trend_spec) {
  # Ensure trend parameters are valid using appropriate
  # lower level functions
}

validate_ar_parameters <- function(trend_spec) {
  p <- extract_parameter(trend_spec, "p", default = 1)
  
  if (!is.numeric(p) || p < 1 || p != round(p)) {
    stop(insight::format_error(
      sprintf("AR order 'p' must be positive integer, got: %s", p)
    ))
  }
}

validate_var_parameters <- function(trend_spec) {
  p <- extract_parameter(trend_spec, "p", default = 1)
  
  if (!is.numeric(p) || p < 1 || p != round(p)) {
    stop(insight::format_error(
      sprintf("VAR order 'p' must be positive integer, got: %s", p)
    ))
  }
}
```

## Stan Code Validation

### 1. stanvars Conflict Detection
```r
validate_stanvars_compatibility <- function(trend_stanvars, brms_stanvars = NULL) {
  # Extract names from trend stanvars
  trend_names <- extract_stanvar_names(trend_stanvars)
  
  # Check for brms reserved names
  brms_reserved <- get_brms_reserved_names()
  conflicts <- intersect(trend_names, brms_reserved)
  
  if (length(conflicts) > 0) {
    stop(insight::format_error(
      sprintf("stanvar names conflict with brms reserved names: %s", 
              paste(conflicts, collapse = ", ")),
      "Use mvgam prefix: 'mvgam_' for all custom stanvars"
    ))
  }
  
  # Check for user stanvars conflicts
  if (!is.null(brms_stanvars)) {
    user_names <- extract_stanvar_names(brms_stanvars)
    user_conflicts <- intersect(trend_names, user_names)
    
    if (length(user_conflicts) > 0) {
      stop(insight::format_error(
        sprintf("stanvar names conflict with user stanvars: %s", 
                paste(user_conflicts, collapse = ", ")),
        "Rename conflicting stanvars or use different names"
      ))
    }
  }
}

get_brms_reserved_names <- function() {
  c("N", "Y", "X", "Z", "J", "K", "M", "nresp", "nrescor", "Kc", 
    "Xc", "intercept", "sigma", "nu", "phi", "kappa", "zi", "hu",
    "disc", "mu", "lp", "lprior", "temp_intercept")
}
```

### 2. Generated Stan Code Validation
```r
validate_generated_stan_code <- function(stan_code) {
  # Basic syntax validation
  validate_stan_syntax(stan_code)
  
  # Check for required blocks
  validate_stan_blocks(stan_code)
  
  # Validate parameter declarations
  validate_stan_parameters(stan_code)
  
  # Check for common Stan errors
  validate_stan_common_errors(stan_code)
}

validate_stan_syntax <- function(stan_code) {
  # Check bracket matching
  open_brackets <- stringr::str_count(stan_code, "\\{")
  close_brackets <- stringr::str_count(stan_code, "\\}")
  
  if (open_brackets != close_brackets) {
    stop(insight::format_error(
      "Unmatched brackets in generated Stan code",
      sprintf("Open: %d, Close: %d", open_brackets, close_brackets)
    ))
  }
  
  # Check semicolon requirements in statements
  validate_stan_semicolons(stan_code)
}

validate_stan_semicolons <- function(stan_code) {
  # Split into lines and check statement endings
  lines <- strsplit(stan_code, "\n")[[1]]
  statement_lines <- grep("^\\s*[^/{}]*[^;{}\\s]\\s*$", lines)
  
  if (length(statement_lines) > 0) {
    problematic_lines <- lines[statement_lines[1:min(3, length(statement_lines))]]
    insight::format_warning(
      "Potential missing semicolons in generated Stan code:",
      paste("  Line", statement_lines[1:min(3, length(statement_lines))], ":", 
            problematic_lines, collapse = "\n")
    )
  }
}

validate_stan_blocks <- function(stan_code) {
  required_blocks <- c("data", "parameters", "model")
  
  for (block in required_blocks) {
    pattern <- paste0(block, "\\s*\\{")
    if (!grepl(pattern, stan_code)) {
      stop(insight::format_error(
        sprintf("Missing required Stan block: %s", block)
      ))
    }
  }
}
```

## LFO-CV Validation

### 1. Time Series Cross-Validation Setup
```r
validate_lfo_setup <- function(data, time_var, L_future, ...) {
  # Validate time structure
  validate_time_series_structure(data, time_var)
  
  # Check forecast horizon feasibility
  n_timepoints <- length(unique(data[[time_var]]))
  
  if (L_future >= n_timepoints) {
    stop(insight::format_error(
      sprintf("Forecast horizon (L_future = %d) too large", L_future),
      sprintf("Maximum L_future: %d (total timepoints - 1)", n_timepoints - 1)
    ))
  }
  
  if (L_future < 1) {
    stop(insight::format_error(
      "Forecast horizon (L_future) must be positive integer"
    ))
  }
  
  # Check minimum training data
  min_train <- n_timepoints - L_future
  if (min_train < 3) {
    stop(insight::format_error(
      sprintf("Insufficient training data (%d timepoints)", min_train),
      "Minimum 3 training timepoints required"
    ))
  }
  
  return(list(
    n_timepoints = n_timepoints,
    min_train = min_train,
    ...
  ))
}
```

### 2. Evaluation Metrics Validation
```r
validate_lfo_metrics <- function(metrics, family) {
  available_metrics <- get_available_lfo_metrics(family)
  
  invalid_metrics <- setdiff(metrics, names(available_metrics))
  if (length(invalid_metrics) > 0) {
    stop(insight::format_error(
      sprintf("Invalid metrics for family '%s': %s", 
              family$family, paste(invalid_metrics, collapse = ", ")),
      "Available metrics:",
      paste("  ", names(available_metrics), collapse = "\n")
    ))
  }
  
  # Check metric-specific requirements
  for (metric in metrics) {
    validator <- available_metrics[[metric]]$validator
    if (!is.null(validator)) {
      validator(family)
    }
  }
}

get_available_lfo_metrics <- function(family) {
  base_metrics <- list(
    "elpd" = list(
      description = "Expected log predictive density",
      validator = NULL
    ),
    "rmse" = list(
      description = "Root mean squared error", 
      validator = function(fam) {
        if (!fam$family %in% c("gaussian", "student", "skew_normal")) {
          insight::format_warning(
            sprintf("RMSE may not be appropriate for %s family", fam$family)
          )
        }
      }
    ),
    "mae" = list(
      description = "Mean absolute error",
      validator = NULL
    ),
    "coverage" = list(
      description = "Prediction interval coverage",
      validator = NULL
    )
  )
  
  # Add family-specific metrics
  if (family$family %in% c("gaussian", "student")) {
    base_metrics$crps <- list(
      description = "Continuous ranked probability score",
      validator = NULL
    )
  }
  
  if (family$family %in% c("poisson", "negbinomial", "binomial")) {
    base_metrics$energy_score <- list(
      description = "Energy score for discrete distributions",
      validator = NULL
    )
  }
  
  return(base_metrics)
}
```

## Registry Validation

### 1. Custom Trend Registration Validation
```r
validate_trend_registration <- function(name, generator_fn, validator_fn, 
                                       factor_compatible = TRUE) {
  # Check name format
  if (!is.character(name) || length(name) != 1 || nchar(name) == 0) {
    stop(insight::format_error(
      "Trend name must be non-empty character string"
    ))
  }
  
  if (grepl("[^A-Za-z0-9_]", name)) {
    stop(insight::format_error(
      "Trend name must contain only letters, numbers, and underscores"
    ))
  }
  
  # Check function signatures
  validate_generator_function(generator_fn, name)
  validate_validator_function(validator_fn, name)
  
  # Check for conflicts with built-in trends
  builtin_trends <- c("AR", "RW", "VAR", "CAR", "PW", "ZMVN")
  if (name %in% builtin_trends) {
    stop(insight::format_error(
      sprintf("Cannot override built-in trend type: %s", name),
      "Use different name for custom trend"
    ))
  }
  
  # Check factor compatibility specification
  if (!is.logical(factor_compatible) || length(factor_compatible) != 1) {
    stop(insight::format_error(
      "factor_compatible must be TRUE or FALSE"
    ))
  }
}

validate_generator_function <- function(generator_fn, trend_name) {
  if (!is.function(generator_fn)) {
    stop(insight::format_error(
      sprintf("Generator for trend '%s' must be a function", trend_name)
    ))
  }
  
  # Check function arguments
  expected_args <- c("trend_spec", "data_info")
  actual_args <- names(formals(generator_fn))
  
  missing_args <- setdiff(expected_args, actual_args)
  if (length(missing_args) > 0) {
    stop(insight::format_error(
      sprintf("Generator function missing required arguments: %s", 
              paste(missing_args, collapse = ", ")),
      "Required signature: function(trend_spec, data_info, ...)"
    ))
  }
}

validate_validator_function <- function(validator_fn, trend_name) {
  if (!is.function(validator_fn)) {
    stop(insight::format_error(
      sprintf("Validator for trend '%s' must be a function", trend_name)
    ))
  }
  
  # Test validator with dummy input
  test_spec <- list(type = trend_name, parameters = list())
  
  tryCatch({
    result <- validator_fn(test_spec)
    if (!is.logical(result) || length(result) != 1) {
      stop("Validator must return single logical value")
    }
  }, error = function(e) {
    stop(insight::format_error(
      sprintf("Validator function failed test: %s", e$message),
      "Ensure validator handles basic trend specifications"
    ))
  })
}
```

## Error Message Standards

### 1. Consistent Error Formatting
```r
create_validation_error <- function(main_message, ..., 
                                   .call = rlang::caller_env()) {
  details <- list(...)
  
  if (length(details) > 0) {
    formatted_details <- paste(details, collapse = "\n")
    full_message <- paste(main_message, formatted_details, sep = "\n")
  } else {
    full_message <- main_message
  }
  
  # Use insight for consistent formatting
  insight::format_error(full_message, .envir = .call)
}

create_validation_warning <- function(main_message, ..., 
                                     .frequency = "once",
                                     .call = rlang::caller_env()) {
  details <- list(...)
  
  if (length(details) > 0) {
    formatted_details <- paste(details, collapse = "\n")
    full_message <- paste(main_message, formatted_details, sep = "\n")
  } else {
    full_message <- main_message
  }
  
  insight::format_warning(full_message, .frequency = .frequency, .envir = .call)
}
```

### 2. Context-Specific Error Messages
```r
get_autocorr_conflict_message <- function(conflicting_terms) {
  paste(
    sprintf("brms autocorrelation terms found in trend_formula: %s", 
            paste(conflicting_terms, collapse = ", ")),
    "",
    "mvgam separates observation-level correlation from State-Space dynamics:",
    "",
    "✅ Correct usage:",
    "  mvgam(",
    "    y ~ x + unstr(time, subject),  # Observation-level correlation", 
    "    trend_formula = ~ AR(p = 1),   # State-Space dynamics",
    "    data = data",
    "  )",
    "",
    "❌ Incorrect - conflicts with State-Space trends:",
    "  trend_formula = ~ s(time) + ar(p = 1)",
    "",
    "Use mvgam trend syntax instead:",
    "  ar(p = 1) → AR(p = 1)",
    "  ma(q = 1) → MA(q = 1)", 
    "  arma(p, q) → ARMA(p = p, q = q)",
    sep = "\n"
  )
}

get_distributional_trend_error <- function(invalid_parameters, main_parameter) {
  paste(
    sprintf("Trends not supported on auxiliary parameters: %s", 
            paste(invalid_parameters, collapse = ", ")),
    "",
    sprintf("State-Space trends only apply to main parameter: %s", main_parameter),
    "",
    "✅ Correct for distributional models:",
    "  bf(y ~ s(x) + AR(p = 1), sigma ~ s(z))",
    "",
    "❌ Incorrect - trend on auxiliary parameter:",
    "  bf(y ~ s(x), sigma ~ s(z) + AR(p = 1))",
    "",
    "Rationale: State-Space models describe temporal evolution of the mean process",
    sep = "\n"
  )
}
```
