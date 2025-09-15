#' Validate Nonlinear Trend Compatibility
#'
#' @description
#' Validates that trend specifications are compatible with nonlinear model structure.
#'
#' @param nl_components List of nonlinear components
#' @param trend_specs Trend specification
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_nonlinear_trend_compatibility <- function(nl_components, trend_specs) {
  checkmate::assert_list(nl_components)
  checkmate::assert_list(trend_specs, null.ok = TRUE)

  if (is.null(trend_specs)) {
    return(invisible(TRUE))
  }

  # Check that trend type is compatible with nonlinear models
  incompatible_trends <- c()  # Currently all trends should work

  if (trend_specs$type %in% incompatible_trends) {
    stop(insight::format_error(
      "Trend type '{trend_specs$type}' is not compatible with nonlinear models.",
      "Consider using different trend specification."
    ))
  }

  # Warn about potential complexity
  if (length(nl_components$nonlinear_params) > 2) {
    insight::format_warning(
      "Complex nonlinear model with {length(nl_components$nonlinear_params)} parameters detected.",
      "Trend effects will be applied to main parameter only.",
      "Consider model simplification if convergence issues occur."
    )
  }

  invisible(TRUE)
}

#' Apply Rule-Based Validation Dispatch
#'
#' @description
#' Applies validation rules specified in trend objects to automatically
#' dispatch to appropriate validation functions. Makes adding new trends
#' trivial - just specify validation rules in constructor.
#'
#' @param trend_specs Trend specification(s)
#' @param data Data frame with time series data
#' @return Enhanced trend specs with processed validation
#' @noRd
apply_validation_rules <- function(trend_specs, data) {

  # Handle univariate vs multivariate specs
  if (is_multivariate_trend_specs(trend_specs)) {
    # Process each response specification
    for (response_name in names(trend_specs)) {
      trend_specs[[response_name]] <- process_trend_validation_rules(
        trend_specs[[response_name]], data
      )
    }
  } else {
    # Process single trend specification
    trend_specs <- process_trend_validation_rules(trend_specs, data)
  }

  return(trend_specs)
}

#' Process Validation Rules for Single Trend
#'
#' @description
#' Processes validation rules for a single trend specification using
#' rule-based dispatch.
#'
#' @param trend_spec Single trend specification
#' @param data Data frame with time series data
#' @return Enhanced trend specification
#' @noRd
process_trend_validation_rules <- function(trend_spec, data) {

  # Get validation rules from trend object
  validation_rules <- trend_spec$validation_rules %||% character(0)

  # Apply each validation rule
  for (rule in validation_rules) {
    trend_spec <- dispatch_validation_rule(rule, trend_spec, data)
  }

  return(trend_spec)
}

#' Dispatch Single Validation Rule
#'
#' @description
#' Dispatches a single validation rule to the appropriate validation function.
#' Uses rule-to-function mapping for clean, extensible architecture.
#'
#' @param rule Validation rule name
#' @param trend_spec Trend specification
#' @param data Data frame with time series data
#' @return Enhanced trend specification
#' @noRd
dispatch_validation_rule <- function(rule, trend_spec, data) {

  # Rule-to-function mapping
  rule_functions <- get_validation_rule_dispatch_table()

  if (!rule %in% names(rule_functions)) {
    warning(paste("Unknown validation rule:", rule), call. = FALSE)
    return(trend_spec)
  }

  # Call the appropriate validation function
  validation_function <- rule_functions[[rule]]
  result <- validation_function(trend_spec, data)

  return(result)
}

#' Get Validation Rule Dispatch Table
#'
#' @description
#' Returns mapping from validation rule names to validation functions.
#' Central dispatch table for rule-based validation.
#'
#' @return Named list mapping rules to functions
#' @noRd
get_validation_rule_dispatch_table <- function() {
  list(
    "requires_grouping_validation" = validate_trend_grouping,
    "supports_correlation" = validate_trend_correlation,
    "requires_regular_intervals" = validate_trend_time_intervals,
    "supports_factors" = validate_trend_factor_compatibility,
    "supports_hierarchical" = validate_trend_hierarchical_structure,
    "requires_parameter_processing" = validate_and_process_trend_parameters
  )
}

#' Validate Trend Grouping
#'
#' @description
#' Validates and processes grouping arguments for trends that support them.
#'
#' @param trend_spec Trend specification
#' @param data Data frame with time series data
#' @return Enhanced trend specification
#' @noRd
validate_trend_grouping <- function(trend_spec, data) {

  # Process grouping arguments if present
  if (!is.null(trend_spec$gr) && trend_spec$gr != 'NA') {
    groupings <- validate_grouping_arguments(trend_spec$gr, trend_spec$subgr)
    trend_spec$gr <- groupings$gr
    trend_spec$subgr <- groupings$subgr

    # Validate grouping variables exist in data
    if (!trend_spec$gr %in% colnames(data)) {
      stop(insight::format_error(
        "Grouping variable '{.field {trend_spec$gr}}' not found in data.",
        "Available variables: {.field {paste(colnames(data), collapse = ', ')}}"
      ))
    }

    if (!is.null(trend_spec$subgr) && trend_spec$subgr != 'NA' &&
        !trend_spec$subgr %in% colnames(data)) {
      stop(insight::format_error(
        "Subgrouping variable '{.field {trend_spec$subgr}}' not found in data.",
        "Available variables: {.field {paste(colnames(data), collapse = ', ')}}"
      ))
    }
  }

  return(trend_spec)
}

#' Validate Trend Correlation
#'
#' @description
#' Validates and processes correlation requirements for trends that support them.
#'
#' @param trend_spec Trend specification
#' @param data Data frame with time series data
#' @return Enhanced trend specification
#' @noRd
validate_trend_correlation <- function(trend_spec, data) {

  # Process correlation requirements based on grouping
  if (!is.null(trend_spec$cor)) {
    trend_spec$cor <- validate_correlation_requirements(trend_spec$gr, trend_spec$cor)
  }

  return(trend_spec)
}

#' Validate Trend Time Intervals
#'
#' @description
#' Validates regular time intervals for trends that require them.
#'
#' @param trend_spec Trend specification
#' @param data Data frame with time series data
#' @return Enhanced trend specification
#' @noRd
validate_trend_time_intervals <- function(trend_spec, data) {

  # Extract time variable
  time_var <- trend_spec$time %||% "time"

  if (time_var %in% colnames(data)) {
    # Use existing validation function
    validate_regular_time_intervals(data[[time_var]], time_var)
  }

  return(trend_spec)
}

#' Validate Trend Factor Compatibility
#'
#' @description
#' Validates factor model requirements for trends that support them.
#'
#' @param trend_spec Trend specification
#' @param data Data frame with time series data
#' @return Enhanced trend specification
#' @noRd
validate_trend_factor_compatibility <- function(trend_spec, data) {

  # Validate factor model requirements
  if (!is.null(trend_spec$n_lv)) {
    # Get series count from data
    series_var <- trend_spec$series %||% "series"
    if (series_var %in% colnames(data)) {
      n_series <- length(unique(data[[series_var]]))

      if (trend_spec$n_lv >= n_series) {
        stop(insight::format_error(
          "Factor model requires {.field n_lv < n_series}.",
          "You specified {.field n_lv = {trend_spec$n_lv}} but data has {n_series} series.",
          "Reduce n_lv or increase number of series."
        ))
      }
    }
  }

  return(trend_spec)
}

#' Validate Trend Hierarchical Structure
#'
#' @description
#' Validates hierarchical grouping structure for trends that support it.
#'
#' @param trend_spec Trend specification
#' @param data Data frame with time series data
#' @return Enhanced trend specification
#' @noRd
validate_trend_hierarchical_structure <- function(trend_spec, data) {

  # Additional hierarchical structure validation can be added here
  # For now, this is handled by validate_trend_grouping

  return(trend_spec)
}

#' Validate Factor Compatibility
#'
#' @description
#' Validates that a trend specification is compatible with factor models.
#' Uses the trend registry to check factor support.
#'
#' @param trend_spec List with trend specification including trend_model name
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_factor_compatibility <- function(trend_spec) {
  checkmate::assert_list(trend_spec)

  if (is.null(trend_spec$n_lv) || trend_spec$n_lv == 0) {
    return(invisible(TRUE))  # No factor model requested
  }

  trend_name <- trend_spec$trend_model %||% "Unknown"

  # Check if trend type is registered
  if (!exists(trend_name, envir = trend_registry)) {
    stop(insight::format_error(
      "Unknown trend type: '{trend_name}'",
      "Available types: {paste(ls(trend_registry), collapse = ', ')}"
    ))
  }

  # Get trend info from registry
  trend_info <- get(trend_name, envir = trend_registry)

  # Check factor support
  if (!trend_info$supports_factors) {
    stop(insight::format_error(
      "Factor models (n_lv > 0) not supported for {trend_name} trends.",
      trend_info$incompatibility_reason
    ))
  }

  invisible(TRUE)
}

#' Validate Grouping Arguments
#'
#' @description
#' Validates hierarchical grouping structure and returns processed arguments.
#'
#' @param gr Grouping variable name or NULL/'NA'
#' @param subgr Subgrouping variable name or NULL/'NA'
#' @return List with processed gr and subgr values
#' @noRd
validate_grouping_arguments <- function(gr, subgr) {
  # Process gr argument
  if (is.null(gr) || is.na(gr) || gr == "NA") {
    gr <- NULL
  } else {
    checkmate::assert_string(gr, min.chars = 1)
  }

  # Process subgr argument
  if (is.null(subgr) || is.na(subgr) || subgr == "NA") {
    subgr <- NULL
  } else {
    checkmate::assert_string(subgr, min.chars = 1)
  }

  # Validation logic
  if (!is.null(gr) && is.null(subgr)) {
    stop(insight::format_error(
      "Hierarchical grouping requires subgrouping variable.",
      "If you specify {.field gr = '{gr}'}, you must also specify {.field subgr}.",
      "For simple grouping, use {.field series} parameter instead."
    ))
  }

  if (!is.null(subgr) && is.null(gr)) {
    stop(insight::format_error(
      "Subgrouping requires main grouping variable.",
      "Cannot specify {.field subgr = '{subgr}'} without {.field gr}."
    ))
  }

  if (!is.null(gr) && !is.null(subgr) && subgr == "series") {
    stop(insight::format_error(
      "Invalid subgrouping for hierarchical models.",
      "Cannot use 'series' as {.field subgr} when {.field gr = '{gr}'}.",
      "Use a different subgrouping variable that nests within '{gr}'."
    ))
  }

  return(list(gr = gr, subgr = subgr))
}

#' Validate Correlation Requirements
#'
#' @description
#' Validates correlation settings based on grouping structure.
#'
#' @param gr Grouping variable name
#' @param cor Correlation setting
#' @return Processed correlation setting
#' @noRd
validate_correlation_requirements <- function(gr, cor) {
  if (is.null(cor)) {
    return(FALSE)  # Default to no correlation
  }

  # If grouping is specified, correlation should generally be FALSE
  # unless explicitly requested for cross-group correlation
  if (!is.null(gr) && cor) {
    if (!identical(Sys.getenv("TESTTHAT"), "true")) {
      rlang::warn(
        insight::format_warning(
          "Correlation enabled with grouping variable '{gr}'.",
          "This models cross-group correlations which may be computationally intensive.",
          "Consider {.field cor = FALSE} for within-group correlations only."
        ),
        .frequency = "once",
        .frequency_id = "correlation_with_grouping"
      )
    }
  }

  return(cor)
}

#' Validate Time Variable
#'
#' @description
#' Validates and processes time variable specification.
#'
#' @param time_var Time variable name or expression
#' @return Processed time variable name
#' @noRd
validate_time_variable <- function(time_var) {
  if (is.null(time_var)) {
    if (!identical(Sys.getenv("TESTTHAT"), "true")) {
      rlang::warn(
        "Using default 'time' variable. Specify time parameter explicitly for clarity.",
        .frequency = "once",
        .frequency_id = "default_time_variable"
      )
    }
    return("time")
  }

  # Convert symbol/expression to string
  if (is.name(time_var) || is.call(time_var)) {
    time_var <- deparse(time_var)
  }

  checkmate::assert_string(time_var, min.chars = 1)
  return(time_var)
}

#' Validate Series Variable
#'
#' @description
#' Validates and processes series variable specification.
#'
#' @param series_var Series variable name or expression
#' @return Processed series variable name
#' @noRd
validate_series_variable <- function(series_var) {
  if (is.null(series_var)) {
    if (!identical(Sys.getenv("TESTTHAT"), "true")) {
      rlang::warn(
        "Using default 'series' variable. Specify series parameter explicitly for clarity.",
        .frequency = "once",
        .frequency_id = "default_series_variable"
      )
    }
    return("series")
  }

  # Convert symbol/expression to string
  if (is.name(series_var) || is.call(series_var)) {
    series_var <- deparse(series_var)
  }

  checkmate::assert_string(series_var, min.chars = 1)
  return(series_var)
}

#' Validate Regular Time Intervals
#'
#' @description
#' Validates that time series has regular intervals for trends that require it.
#'
#' @param time_values Vector of time values
#' @param time_var Name of time variable (for error messages)
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_regular_time_intervals <- function(time_values, time_var = "time") {
  checkmate::assert_numeric(time_values, min.len = 2)

  # Calculate intervals between consecutive time points
  intervals <- diff(sort(unique(time_values)))

  # Check for regular intervals (allowing small numerical tolerance)
  tolerance <- 1e-10
  interval_range <- range(intervals)
  is_regular <- abs(interval_range[2] - interval_range[1]) < tolerance

  if (!is_regular) {
    stop(insight::format_error(
      "Irregular time intervals detected in '{time_var}'.",
      "Some trends require regular time spacing.",
      "Interval range: {min(intervals)} to {max(intervals)}",
      "Consider using CAR() for irregular intervals or interpolate data."
    ))
  }

  # Check for series-specific irregular intervals
  # This would need series information to implement fully
  # Only warn in interactive sessions, not during testing
  if (!identical(Sys.getenv("TESTTHAT"), "true")) {
    rlang::warn(
      insight::format_warning(
        "Time interval validation performed on combined data.",
        "Ensure each series has regular intervals individually.",
        "Some trends may not support series-specific irregular time intervals."
      ),
      .frequency = "once",
      .frequency_id = "series_interval_warning"
    )
  }

  invisible(TRUE)
}

#' Utility function equivalent to base::deparse0
#'
#' @description
#' Provides deparse0 functionality for compatibility with older R versions.
#'
#' @param expr Expression to deparse
#' @param ... Additional arguments passed to deparse
#' @return Character string representation of the expression
#' @noRd
deparse0 <- function(expr, ...) {
  paste(deparse(expr, ...), collapse = "")
}

#' Check if Formula is Nonlinear
#'
#' @description
#' Determines if a brms formula specifies a nonlinear model.
#'
#' @param formula brms formula object
#' @return Logical indicating if formula is nonlinear
#' @noRd
is_nonlinear_formula <- function(formula) {
  checkmate::assert_formula(formula)

  # Check for brms bf() structure with nl = TRUE
  if (inherits(formula, "brmsterms")) {
    return(formula$nl)
  }

  if (inherits(formula, "brmsformula")) {
    return(attr(formula, "nl") %||% FALSE)
  }

  # Check formula structure for nonlinear indicators
  formula_str <- deparse(formula)

  # Look for bf() with nl = TRUE
  has_nl_true <- grepl("nl\\s*=\\s*TRUE", formula_str, ignore.case = TRUE)

  # Look for nonlinear parameter specifications
  has_nl_params <- grepl("\\b[a-zA-Z]+\\s*~", formula_str) &&
                   grepl("\\bnl\\s*=", formula_str)

  return(has_nl_true || has_nl_params)
}

#' @noRd
validate_brms_formula <- function(formula) {
  issues <- character(0)

  # Check if formula is NULL
  if (is.null(formula)) {
    return(list(valid = FALSE, issues = "Formula cannot be NULL"))
  }

  # Check formula class
  valid_classes <- c("formula", "brmsformula", "bform")
  if (!any(sapply(valid_classes, function(cls) inherits(formula, cls)))) {
    issues <- c(issues, paste(
      "Formula must be of class:", paste(valid_classes, collapse = ", "),
      "but got:", class(formula)[1]
    ))
  }

  # Try to validate with existing brms validation function
  validation_result <- try({
    validate_obs_formula_brms(formula)
    NULL  # No issues if validation succeeds
  }, silent = TRUE)

  if (inherits(validation_result, "try-error")) {
    error_msg <- attr(validation_result, "condition")$message
    issues <- c(issues, paste("brms validation failed:", error_msg))
  }

  return(list(
    valid = length(issues) == 0,
    issues = issues
  ))
}

# Utility function to extract formula string using brms pattern
formula2str_mvgam <- function(formula, space = "trim") {
  if (is.null(formula)) {
    return(NULL)
  }

  # Handle complex brms formula objects (bf, distributional, nonlinear)
  if (inherits(formula, c("brmsformula", "bform"))) {
    # Extract all formula components for comprehensive string representation
    formula_strings <- character(0)

    # Main formula
    if (!is.null(formula$formula)) {
      main_str <- deparse(formula$formula)
      formula_strings <- c(formula_strings, main_str)
    }

    # Distributional parameter formulas (sigma, nu, phi, etc.)
    if (!is.null(formula$pforms)) {
      pform_strings <- sapply(formula$pforms, function(pf) {
        if (!is.null(pf$formula)) {
          deparse(pf$formula)
        } else {
          deparse(pf)
        }
      })
      formula_strings <- c(formula_strings, pform_strings)
    }

    # Nonlinear parameter formulas
    if (!is.null(formula$nlpars)) {
      nlpar_strings <- sapply(formula$nlpars, function(nlp) {
        if (!is.null(nlp$formula)) {
          deparse(nlp$formula)
        } else {
          deparse(nlp)
        }
      })
      formula_strings <- c(formula_strings, nlpar_strings)
    }

    # If we couldn't extract components, use the whole object
    if (length(formula_strings) == 0) {
      formula_strings <- deparse(formula)
    }

    # Combine all components
    x <- paste(formula_strings, collapse = " ")
  } else {
    # Standard formula handling
    formula <- as.formula(formula)
    x <- Reduce(paste, deparse(formula))
  }

  # Clean up whitespace
  x <- gsub("[\t\r\n]+", " ", x, perl = TRUE)

  if (space == "trim") {
    x <- trimws(x)  # Use base R trimws for simplicity
  }

  return(x)
}

#' Get Dynamic Trend Validation Patterns
#'
#' @description
#' Dynamically generates regex patterns for detecting trend constructors
#' based on the current trend registry. This ensures validation works
#' with custom trend types without hard-coding patterns.
#'
#' @return Named character vector of regex patterns
#' @noRd
get_trend_validation_patterns <- function() {

  # Access trend registry from mvgam namespace
  trend_registry <- get("trend_registry", envir = asNamespace("mvgam"))
  trend_types <- ls(trend_registry)

  # Generate patterns dynamically
  patterns <- character(length(trend_types))
  names(patterns) <- paste0("\\b", trend_types, "\\s*\\(")

  # Create the values (display names)
  for (i in seq_along(trend_types)) {
    patterns[i] <- paste0(trend_types[i], "()")
  }

  return(patterns)
}

#' Validate observation formula excludes mvgam trend constructors
#'
#' Main validation for observation formulas - ensures they're clean for brms
#' processing by checking they don't contain mvgam trend constructors.
#'
#' @param formula Observation formula (formula, brmsformula, or bform)
#' @return The original formula unchanged (brms handles processing)
#' @noRd
validate_obs_formula_brms <- function(formula) {
  if (is.null(formula)) return(NULL)

  # Accept any brms-compatible formula class
  checkmate::assert(
    inherits(formula, "formula") ||
    inherits(formula, "brmsformula") ||
    inherits(formula, "bform"),
    .var.name = "formula"
  )

  # Extract string representation for pattern matching
  formula_str <- formula2str_mvgam(formula)

  # Check for mvgam trend constructors using dynamic registry lookup
  trend_patterns <- get_trend_validation_patterns()

  detected_trends <- character(0)
  for (pattern in names(trend_patterns)) {
    if (grepl(pattern, formula_str, perl = TRUE)) {
      detected_trends <- c(detected_trends, trend_patterns[[pattern]])
    }
  }

  if (length(detected_trends) > 0) {
    stop(insight::format_error(
      "mvgam trend constructors found in observation {.field formula}:",
      paste("Found:", paste(unique(detected_trends), collapse = ", ")),
      "Trend constructors belong in {.field trend_formula}, not {.field formula}.",
      "Use: {.code mvgam(y ~ x, trend_formula = ~ RW())}"
    ))
  }

  # Issue informational warning about brms autocorrelation if present
  check_brms_autocor_usage(formula_str)

  # Check offset usage (informational - offsets are allowed in observation formulas)
  validate_offsets_in_obs(formula)

  # Return original formula unchanged - brms handles all other validation
  return(formula)
}

#' Validate trend formula for State-Space compatibility
#'
#' Validates trend formulas supporting bf() objects, named lists, and single
#' formulas. Ensures compatibility with mvgam State-Space dynamics.
#'
#' @param trend_formula Trend specification (formula, bf object, or named list)
#' @return The validated trend formula
#' @noRd
validate_trend_formula_brms <- function(trend_formula) {
  if (is.null(trend_formula)) return(NULL)

  # Handle bf() objects for multivariate trend specifications
  if (inherits(trend_formula, c("brmsformula", "bform"))) {
    return(validate_bf_trend_formula(trend_formula))
  }

  # Handle named list for multivariate (alternative to bf())
  if (is.list(trend_formula) && !inherits(trend_formula, "formula")) {
    return(validate_list_trend_formula(trend_formula))
  }

  # Single trend formula validation
  if (inherits(trend_formula, "formula")) {
    return(validate_single_trend_formula(trend_formula))
  }

  # Invalid type
  stop(insight::format_error(
    "Invalid {.field trend_formula} type: {class(trend_formula)}",
    "Must be formula, bf() object, or named list."
  ))
}

#' Validate bf() trend formula objects
#'
#' @param bf_obj A brmsformula or bform object
#' @noRd
validate_bf_trend_formula <- function(bf_obj) {
  checkmate::assert_class(bf_obj, c("brmsformula", "bform"))

  # For bf() objects in trend context, response variables are allowed
  # because they identify which trend belongs to which response

  # Extract and validate all formula components from bf() object
  all_formulas <- extract_all_bf_formulas(bf_obj)

  # Validate each formula component
  for (i in seq_along(all_formulas)) {
    formula_component <- all_formulas[[i]]
    context_name <- names(all_formulas)[i] %||% paste("bf() component", i)

    if (inherits(formula_component, "formula")) {
      validate_single_trend_formula(
        formula_component,
        context = context_name,
        allow_response = TRUE
      )
    }
  }

  return(bf_obj)
}

#' Extract all formula components from a bf() object
#'
#' Helper function to comprehensively extract all formulas from brmsformula objects
#' for validation purposes.
#'
#' @param bf_obj A brmsformula or bform object
#' @return Named list of all formula components
#' @noRd
extract_all_bf_formulas <- function(bf_obj) {
  formulas <- list()

  # Main formula
  if (!is.null(bf_obj$formula)) {
    formulas[["main"]] <- bf_obj$formula
  }

  # Distributional parameter formulas (pforms)
  if (!is.null(bf_obj$pforms)) {
    for (i in seq_along(bf_obj$pforms)) {
      pform <- bf_obj$pforms[[i]]
      param_name <- names(bf_obj$pforms)[i] %||% paste("pform", i)

      if (!is.null(pform$formula)) {
        formulas[[paste("pform", param_name)]] <- pform$formula
      } else if (inherits(pform, "formula")) {
        formulas[[paste("pform", param_name)]] <- pform
      }
    }
  }

  # Nonlinear parameter formulas (nlpars)
  if (!is.null(bf_obj$nlpars)) {
    for (i in seq_along(bf_obj$nlpars)) {
      nlpar <- bf_obj$nlpars[[i]]
      param_name <- names(bf_obj$nlpars)[i] %||% paste("nlpar", i)

      if (!is.null(nlpar$formula)) {
        formulas[[paste("nlpar", param_name)]] <- nlpar$formula
      } else if (inherits(nlpar, "formula")) {
        formulas[[paste("nlpar", param_name)]] <- nlpar
      }
    }
  }

  # Additional formulas (if any other slots exist)
  # This is a fallback for any other formula-containing slots
  other_slots <- setdiff(names(bf_obj), c("formula", "pforms", "nlpars", "family", "autocor", "loop"))
  for (slot_name in other_slots) {
    slot_content <- bf_obj[[slot_name]]
    if (inherits(slot_content, "formula")) {
      formulas[[paste("other", slot_name)]] <- slot_content
    }
  }

  return(formulas)
}

#' Validate named list trend formula
#'
#' @param formula_list Named list of formulas
#' @noRd
validate_list_trend_formula <- function(formula_list) {
  if (is.null(names(formula_list))) {
    stop(insight::format_error(
      "Multivariate {.field trend_formula} must be a named list.",
      "Use: {.code trend_formula = list(resp1 = ~ AR(), resp2 = ~ RW())}"
    ))
  }

  # Validate each component formula
  validated_list <- lapply(names(formula_list), function(name) {
    if (is.null(formula_list[[name]])) {
      return(NULL)  # Allow NULL for responses without trends
    }
    validate_single_trend_formula(formula_list[[name]], context = paste("response", name))
  })
  names(validated_list) <- names(formula_list)

  return(validated_list)
}

#' Validate a single trend formula
#'
#' @param formula Single trend formula
#' @param context Optional context for error messages
#' @param allow_response Logical; whether to allow response variables (TRUE for multivariate identification)
#' @noRd
validate_single_trend_formula <- function(formula, context = NULL, allow_response = FALSE) {
  if (is.null(formula)) return(NULL)

  checkmate::assert_class(formula, "formula")

  # Check for response variable handling
  if (length(formula) == 3) {
    if (!allow_response) {
      context_msg <- if (!is.null(context)) paste("in", context) else ""
      stop(insight::format_error(
        "Trend formula {context_msg} should not have a response variable.",
        "Use: {.code trend_formula = ~ RW()}, not {.code trend_formula = y ~ RW()}",
        "For multivariate models, use: {.code trend_formula = bf(y1 ~ AR(), y2 ~ RW())}"
      ))
    }
    # If response variables are allowed, continue with validation but note it's for multivariate
  }

  # Extract string for validation
  formula_str <- formula2str_mvgam(formula)

  # Check for offset terms (not allowed in trend formulas)
  validate_no_offsets_in_trends(formula)

  # Forbid brms autocorrelation in trend formulas (conflicts with State-Space)
  validate_no_brms_autocor_in_trends(formula_str)

  # Forbid brms addition-terms in trend formulas (conflicts with State-Space)
  validate_no_addition_terms_in_trends(formula_str)

  # Forbid multiple trend constructors in single formula
  validate_no_multiple_trend_constructors(formula_str)

  return(formula)
}

#' Check for brms autocorrelation in observation formulas (informational)
#'
#' @param formula_str String representation of formula
#' @noRd
check_brms_autocor_usage <- function(formula_str) {
  autocor_patterns <- c(
    "\\bar\\s*\\(", "\\bma\\s*\\(", "\\barma\\s*\\(",
    "\\bcosy\\s*\\(", "\\bunstr\\s*\\("
  )

  has_autocor <- any(sapply(autocor_patterns, function(p) grepl(p, formula_str, perl = TRUE)))

  if (has_autocor) {
    rlang::warn(
      insight::format_warning(
        "Using brms autocorrelation in observation formula.",
        "This models residual correlation and complements State-Space trends.",
        "For temporal dynamics, use {.field trend_formula} with mvgam constructors."
      ),
      .frequency = "once",
      .frequency_id = "brms_autocor_obs"
    )
  }

  return(invisible(NULL))
}

#' Validate no brms autocorrelation in trend formulas
#'
#' @param formula_str String representation of trend formula
#' @noRd
validate_no_brms_autocor_in_trends <- function(formula_str) {
  autocor_patterns <- c(
    "\\bar\\s*\\(" = "ar()",
    "\\bma\\s*\\(" = "ma()",
    "\\barma\\s*\\(" = "arma()",
    "\\bcosy\\s*\\(" = "cosy()",
    "\\bunstr\\s*\\(" = "unstr()",
    "\\bautocor\\s*\\(" = "autocor()"
  )

  detected_autocor <- character(0)
  for (pattern in names(autocor_patterns)) {
    if (grepl(pattern, formula_str, perl = TRUE)) {
      detected_autocor <- c(detected_autocor, autocor_patterns[[pattern]])
    }
  }

  if (length(detected_autocor) > 0) {
    stop(insight::format_error(
      "brms autocorrelation terms not allowed in {.field trend_formula}:",
      paste("Found:", paste(unique(detected_autocor), collapse = ", ")),
      "These conflict with mvgam State-Space dynamics.",
      "Use mvgam trend types instead: {.code ar(p = 1)} → {.code AR(p = 1)}"
    ))
  }

  return(invisible(NULL))
}

#' Validate no brms addition-terms in trend formulas
#'
#' Validates that trend formulas do not contain brms addition-terms like
#' weights(), cens(), trunc(), mi(), trials(), rate(), vreal(), vint(),
#' subset(), index(). These terms are allowed in observation formulas but
#' not in trend formulas since they modify observation model behavior.
#'
#' @param formula_str String representation of trend formula
#' @noRd
validate_no_addition_terms_in_trends <- function(formula_str) {
  # Complete list of brms addition-terms from brms source code analysis
  addition_terms <- c("weights", "cens", "trunc", "mi", "trials",
                     "rate", "vreal", "vint", "subset", "index",
                     "cov_ranef")

  # Use same pattern as brms: term_name followed by opening parenthesis
  addition_term_pattern <- paste0("\\b(", paste(addition_terms, collapse = "|"), ")\\s*\\(")

  # Extract any found addition-terms
  matches <- regmatches(formula_str, gregexpr(addition_term_pattern, formula_str, perl = TRUE))[[1]]

  if (length(matches) > 0) {
    # Extract just the function names (remove parentheses and arguments)
    detected_terms <- gsub("\\s*\\(.*", "", matches)
    detected_terms <- paste0(unique(detected_terms), "()")

    stop(insight::format_error(
      "brms addition-terms not allowed in {.field trend_formula}:",
      paste("Found:", paste(detected_terms, collapse = ", ")),
      "These terms modify observation model behavior, not State-Space dynamics.",
      "Include these terms in the observation {.field formula} instead."
    ))
  }

  return(invisible(NULL))
}

#' Validate no multiple trend constructors in trend formulas
#'
#' Trend formulas should contain exactly one trend constructor (AR, RW, VAR, etc.)
#' Multiple trend constructors like ~AR() + RW() are not allowed.
#'
#' @param formula_str String representation of trend formula
#' @noRd
validate_no_multiple_trend_constructors <- function(formula_str) {
  # Use existing infrastructure to detect trend constructors
  trend_patterns <- get_trend_validation_patterns()

  detected_trends <- character(0)
  for (pattern in names(trend_patterns)) {
    if (grepl(pattern, formula_str, perl = TRUE)) {
      detected_trends <- c(detected_trends, trend_patterns[[pattern]])
    }
  }

  if (length(detected_trends) > 1) {
    stop(insight::format_error(
      "Multiple trend constructors found in single {.field trend_formula}:",
      paste("Found:", paste(unique(detected_trends), collapse = " + ")),
      "Each trend formula must contain exactly one trend constructor.",
      "For multiple trends, use response-specific formulas:",
      "{.code trend_formula = list(y1 = ~ AR(), y2 = ~ RW())}"
    ))
  }

  return(invisible(NULL))
}

#' Validate no offset terms in trend formulas
#'
#' Offsets in trend formulas would interfere with State-Space dynamics,
#' so they should be included in observation formulas instead.
#'
#' @param formula Trend formula to check
#' @noRd
validate_no_offsets_in_trends <- function(formula) {
  if (is.null(formula)) return(invisible(NULL))

  # Check for offset() function calls in formula string
  formula_str <- formula2str_mvgam(formula)
  has_offset_function <- grepl("\\boffset\\s*\\(", formula_str, perl = TRUE)

  # Check for offset attribute in terms
  has_offset_attr <- FALSE
  if (inherits(formula, "formula")) {
    terms_obj <- terms(formula)
    has_offset_attr <- !is.null(attr(terms_obj, 'offset'))
  }

  if (has_offset_function || has_offset_attr) {
    stop(insight::format_error(
      "Offset terms not allowed in {.field trend_formula}.",
      "Offsets interfere with State-Space dynamics.",
      "Include offsets in the main observation {.field formula} instead:",
      "Use {.code formula = y ~ x + offset(log_exposure)} not {.code trend_formula = ~ RW() + offset(z)}"
    ))
  }

  return(invisible(NULL))
}

#' Validate observation formula handles offsets appropriately
#'
#' Checks that observation formulas properly handle offsets (which should be
#' allowed and processed by brms) and provides informational guidance.
#'
#' @param formula Observation formula to check
#' @noRd
validate_offsets_in_obs <- function(formula) {
  if (is.null(formula)) return(invisible(NULL))

  # Extract formula string for pattern matching
  formula_str <- formula2str_mvgam(formula)

  # Check for offset usage patterns
  has_offset <- grepl("\\boffset\\s*\\(", formula_str, perl = TRUE)

  if (has_offset) {
    # Provide informational message about offset handling
    rlang::warn(
      insight::format_warning(
        "Offset terms detected in observation formula.",
        "These will be handled by brms and are complementary to State-Space trends.",
        "Offsets model known exposure/effort, while trends capture latent dynamics."
      ),
      .frequency = "once",
      .frequency_id = "offset_obs_usage"
    )
  }

  return(invisible(NULL))
}

#'
#' Ensures that multivariate models with separate trends per response only use
#' basic temporal dynamics without advanced features (factors, correlations, groupings).
#'
#' @param trend_formula Formula for a single response trend
#' @param response_name Name of the response variable
#' @noRd
validate_multivariate_trend_constraints <- function(trend_formula, response_name) {
  checkmate::assert_formula(trend_formula)
  checkmate::assert_string(response_name)

  # Parse the trend formula to extract trend constructors
  parsed <- try(mvgam:::parse_trend_formula(trend_formula), silent = TRUE)
  if (inherits(parsed, "try-error")) {
    return(invisible(NULL))  # Let parse_trend_formula handle the error
  }

  # Check each trend component for advanced features
  for (trend_component in parsed$trend_components) {
    # Check for factor models (n_lv parameter)
    if (!is.null(trend_component$n_lv) && trend_component$n_lv > 0) {
      stop(insight::format_error(
        "Factor models not allowed in multivariate response trends.",
        "Response '{response_name}' has n_lv = {trend_component$n_lv}.",
        "Remove n_lv parameter for basic temporal dynamics only."
      ))
    }

    # Check for correlations (cor parameter)
    if (!is.null(trend_component$cor) && trend_component$cor) {
      stop(insight::format_error(
        "Correlation structures not allowed in multivariate response trends.",
        "Response '{response_name}' has cor = TRUE.",
        "Remove cor parameter for basic temporal dynamics only."
      ))
    }

    # Check for hierarchical grouping (gr, subgr parameters)
    if (!is.null(trend_component$gr)) {
      stop(insight::format_error(
        "Hierarchical grouping not allowed in multivariate response trends.",
        "Response '{response_name}' has gr = '{trend_component$gr}'.",
        "Remove gr parameter for basic temporal dynamics only."
      ))
    }

    if (!is.null(trend_component$subgr)) {
      stop(insight::format_error(
        "Hierarchical grouping not allowed in multivariate response trends.",
        "Response '{response_name}' has subgr = '{trend_component$subgr}'.",
        "Remove subgr parameter for basic temporal dynamics only."
      ))
    }
  }

  invisible(NULL)
}

#' Main validation function for autocorrelation separation
#'
#' Validates proper separation between observation-level (brms) and trend-level
#' (mvgam) autocorrelation handling.
#'
#' @param obs_formula Observation formula (any brms-compatible type)
#' @param trend_formula Trend specification (formula, bf(), or named list)
#' @return List with validated formulas
#' @noRd
validate_autocor_separation <- function(obs_formula, trend_formula = NULL) {
  # Validate observation formula (minimal - let brms handle most validation)
  validated_obs <- validate_obs_formula_brms(obs_formula)

  # Validate trend formula (comprehensive - mvgam State-Space requirements)
  validated_trend <- validate_trend_formula_brms(trend_formula)

  return(list(
    obs_formula = validated_obs,
    trend_formula = validated_trend
  ))
}

#' Validate Setup Components
#' @param components List of setup components
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_setup_components <- function(components) {
  checkmate::assert_list(components, names = "named")

  required_components <- c("formula", "data", "family", "stancode", "standata")
  missing_components <- setdiff(required_components, names(components))

  if (length(missing_components) > 0) {
    stop(insight::format_error(
      "Missing required setup components:",
      paste(missing_components, collapse = ", ")
    ))
  }

  # Validate Stan code is not empty
  if (is.null(components$stancode) ||
      (is.character(components$stancode) && nchar(components$stancode) == 0)) {
    stop(insight::format_error(
      "Stan code extraction failed.",
      "Could not obtain valid Stan model code from brms setup."
    ))
  }

  # Validate Stan data is not empty
  if (is.null(components$standata) || length(components$standata) == 0) {
    stop(insight::format_error(
      "Stan data extraction failed.",
      "Could not obtain valid Stan data from brms setup."
    ))
  }

  invisible(TRUE)
}

#' Validate Time Series Structure for Trends
#'
#' @description
#' Ensures time series data is compatible with specified trend models.
#' Leverages existing mvgam validation functions.
#'
#' @param data Data to validate (data.frame or list)
#' @param trend_specs Trend specification containing trend model info
#' @param silent Verbosity level
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_time_series_for_trends <- function(data, trend_specs, silent = 1, response_vars = NULL) {
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_list(trend_specs)
  # Validate response_vars parameter if provided
  if (!is.null(response_vars)) {
    checkmate::assert_character(response_vars, min.len = 1, any.missing = FALSE, null.ok = TRUE)
  }

  # Extract variable names from trend specification
  # Standardized structure: univariate=direct, multivariate=named list
  if (is_multivariate_trend_specs(trend_specs)) {
    # Multivariate: extract from first response spec
    first_spec <- trend_specs[[1]]
    time_var <- first_spec$time_var %||% first_spec$time %||% "time"
    series_var <- first_spec$series_var %||% first_spec$series %||% "series"
    trend_type <- first_spec$trend_type %||% first_spec$trend_model %||% first_spec$trend
  } else {
    # Univariate: extract directly from trend object
    time_var <- trend_specs$time_var %||% trend_specs$time %||% "time"
    series_var <- trend_specs$series_var %||% trend_specs$series %||% "series"
    trend_type <- trend_specs$trend_type %||% trend_specs$trend_model %||% trend_specs$trend
  }

  # Use new dimension extraction function (validates structure + returns dimensions + mappings)
  dimensions <- extract_time_series_dimensions(
    data = data,
    time_var = time_var,
    series_var = series_var,
    trend_type = trend_type,
    response_vars = response_vars  # Pass response variables for mapping generation
  )

  # Additional validation using existing mvgam infrastructure
  is_multivariate <- is_multivariate_trend_specs(trend_specs)
  validated_data <- validate_series_time(
    data = data,
    name = "data",
    time_var = time_var,
    series_var = series_var,
    check_levels = TRUE,
    check_times = (trend_type != "CAR"),  # CAR doesn't need regular time checks
    is_multivariate = is_multivariate
  )


  # Return both validated data and dimensions for downstream use
  invisible(list(
    data = validated_data,
    dimensions = dimensions
  ))
}

#' Check if object is a mvgam trend
#'
#' Tests whether an object is a valid mvgam trend specification.
#'
#' @param x Object to test
#' @return Logical indicating if x is a mvgam trend
#' @export
is.mvgam_trend <- function(x) {
  inherits(x, "mvgam_trend")
}

#' Validate trend components for conflicts
#'
#' Checks for conflicting trend specifications like multiple dynamic factor models
#' or incompatible correlation structures using brms-inspired validation patterns.
#'
#' @param trend_components List of trend components to validate
#'
#' @noRd
validate_trend_components <- function(trend_components) {

  # Check for multiple trend types - only one trend type allowed per formula
  if (length(trend_components) > 1) {
    trend_types <- sapply(trend_components, function(x) x$trend_type)
    stop(insight::format_error(
      "Multiple trend types detected in single formula.",
      paste("Found:", paste(trend_types, collapse = ", ")),
      "Only one trend constructor is allowed per trend_formula.",
      "Use separate models or combine into a single trend type."
    ))
  }

  # Check for multiple dynamic factor models
  n_lv_models <- sum(sapply(trend_components, function(x) !is.null(x$n_lv) && x$n_lv > 0))
  if (n_lv_models > 1) {
    stop(insight::format_error(
      "Multiple dynamic factor models specified.",
      "Only one trend component can have {.field n_lv > 0}.",
      "Consider combining factor structures or removing one factor model."
    ))
  }

  # Check for conflicting correlation structures
  cor_settings <- sapply(trend_components, function(x) x$cor %||% FALSE)
  if (any(cor_settings) && !all(cor_settings)) {
    insight::format_warning(
      "Mixed correlation settings detected.",
      "Some trend components have correlation enabled while others don't.",
      "This may lead to unexpected interactions."
    )
  }

  invisible(NULL)
}

#' Extract Time Series Dimensions from Data
#'
#' @description
#' Extracts core time series dimensions directly from data and validates
#' structure based on trend type. This is the single source of truth for
#' all time series dimensions used throughout the system.
#'
#' @param data Data frame containing time series
#' @param time_var Name of time variable (default: "time")
#' @param series_var Name of series variable (default: "series")
#' @param trend_type Type of trend model ("CAR" allows irregular intervals)
#' @param trend_specs Optional trend specification list for enhanced metadata
#' @return List with time series dimensions and optional enhanced metadata
#' @noRd
extract_time_series_dimensions <- function(data, time_var = "time", series_var = "series", trend_type = NULL, trend_specs = NULL, response_vars = NULL) {
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_string(time_var)
  checkmate::assert_string(series_var)
  if (!is.null(trend_specs)) {
    checkmate::assert_list(trend_specs)
  }
  # Validate response_vars parameter if provided
  if (!is.null(response_vars)) {
    checkmate::assert_character(response_vars, min.len = 1, any.missing = FALSE)
  }

  # Validate required variables exist
  if (!time_var %in% colnames(data)) {
    stop(insight::format_error(
      "Time variable '{time_var}' not found in data.",
      "Available variables: {paste(colnames(data), collapse = ', ')}"
    ), call. = FALSE)
  }

  if (!series_var %in% colnames(data)) {
    stop(insight::format_error(
      "Series variable '{series_var}' not found in data.",
      "Available variables: {paste(colnames(data), collapse = ', ')}"
    ), call. = FALSE)
  }

  # Calculate core dimensions from data
  unique_times <- unique(data[[time_var]])
  unique_series <- unique(data[[series_var]])
  min_time <- min(data[[time_var]], na.rm = TRUE)
  max_time <- max(data[[time_var]], na.rm = TRUE)

  # Create data ordering mappings for Stan-required sorted order
  ordering_df <- data.frame(
    original_row = seq_len(nrow(data)),
    time_val = data[[time_var]],
    series_val = data[[series_var]],
    stringsAsFactors = FALSE
  )

  # Sort for Stan: first by time, then by series (matches times_trend matrix structure)
  sorted_ordering <- ordering_df[order(ordering_df$time_val, ordering_df$series_val), ]

  # Create bidirectional mappings
  stan_to_original <- sorted_ordering$original_row  # Maps Stan row index -> Original row index
  original_to_stan <- integer(nrow(data))
  original_to_stan[stan_to_original] <- seq_len(nrow(data))  # Maps Original row index -> Stan row index

  # Create time/series index mappings (for times_trend matrix interpretation)
  sorted_unique_times <- sort(unique_times)
  sorted_unique_series <- sort(unique_series)
  time_indices <- match(sorted_ordering$time_val, sorted_unique_times)
  series_indices <- match(sorted_ordering$series_val, sorted_unique_series)

  # Calculate per-series time information for forecasting
  if (requireNamespace("dplyr", quietly = TRUE)) {
    series_time_info <- data %>%
      dplyr::group_by(.data[[series_var]]) %>%
      dplyr::summarise(
        first_time = min(.data[[time_var]], na.rm = TRUE),
        last_time = max(.data[[time_var]], na.rm = TRUE),
        n_obs_series = dplyr::n(),
        time_span = max(.data[[time_var]], na.rm = TRUE) - min(.data[[time_var]], na.rm = TRUE),
        .groups = "drop"
      )

    # Create named vectors for quick access (ordered by sorted unique_series)
    last_times <- setNames(
      series_time_info$last_time[match(sorted_unique_series, series_time_info[[1]])],
      sorted_unique_series
    )
    first_times <- setNames(
      series_time_info$first_time[match(sorted_unique_series, series_time_info[[1]])],
      sorted_unique_series
    )
    series_lengths <- setNames(
      series_time_info$n_obs_series[match(sorted_unique_series, series_time_info[[1]])],
      sorted_unique_series
    )
  } else {
    # Fallback without dplyr
    series_time_info <- NULL
    last_times <- NULL
    first_times <- NULL
    series_lengths <- NULL
  }

  # Backward compatible structure: Original fields maintained
  dimensions <- list(
    n_time = length(unique_times),          # Number of unique time points
    n_series = length(unique_series),       # Number of series
    n_obs = nrow(data),                    # Total observations
    time_range = c(min_time, max_time),    # Time range
    time_var = time_var,                   # Variable names for downstream use
    series_var = series_var,
    unique_times = sorted_unique_times,    # Sorted unique time points
    unique_series = sorted_unique_series   # Sorted unique series
  )

  # Generate observation-to-trend mappings if response variables provided
  # This centralizes mapping generation with dimension calculation for consistency
  if (!is.null(response_vars)) {
    dimensions$mappings <- list()

    for (resp_var in response_vars) {
      # Validate response variable exists in data
      if (!resp_var %in% colnames(data)) {
        stop(insight::format_error(
          "Response variable {.field {resp_var}} not found in data.",
          "Available variables: {paste(colnames(data), collapse = ', ')}"
        ), call. = FALSE)
      }

      # Generate mapping for this response variable using existing function
      mapping <- generate_obs_trend_mapping(
        data = data,
        response_var = resp_var,
        time_var = time_var,
        series_var = series_var,
        dimensions = dimensions  # Pass already-calculated dimensions
      )

      # Store mapping with response variable name as key
      dimensions$mappings[[resp_var]] <- mapping
    }
  }

  # Enhanced metadata: Comprehensive information for post-processing
  if (!is.null(trend_specs)) {
    dimensions$metadata <- list(
      # Variable identification
      variables = list(
        time_var = time_var,
        series_var = series_var,
        gr_var = trend_specs$gr %||% 'NA',
        subgr_var = trend_specs$subgr %||% 'NA',
        has_grouping = !is.null(trend_specs$gr) && trend_specs$gr != 'NA'
      ),

      # Data dimensions
      dimensions = list(
        n_obs = nrow(data),
        n_time = length(unique_times),
        n_series = length(unique_series),
        n_groups = trend_specs$n_groups %||% 1,
        n_subgroups = trend_specs$n_subgroups %||% 1,
        n_lv = trend_specs$n_lv %||% NULL,
        time_range = c(min_time, max_time)
      ),

      # Unique values (sorted for Stan)
      levels = list(
        unique_times = sorted_unique_times,
        unique_series = sorted_unique_series,
        unique_groups = if (!is.null(trend_specs$gr) && trend_specs$gr != 'NA')
                          sort(unique(data[[trend_specs$gr]])) else NULL,
        unique_subgroups = if (!is.null(trend_specs$subgr) && trend_specs$subgr != 'NA')
                             sort(unique(data[[trend_specs$subgr]])) else NULL
      ),

      # Per-series time information (key for forecasting)
      time_info = list(
        # Quick access vectors (ordered by sorted unique_series)
        last_times = last_times,
        first_times = first_times,
        series_lengths = series_lengths,

        # Global time information
        global_last_time = max_time,
        global_first_time = min_time,
        max_forecast_horizon = if (!is.null(last_times)) max_time - min(last_times, na.rm = TRUE) else 0,

        # Panel structure information
        is_balanced = if (!is.null(series_lengths)) length(unique(series_lengths)) == 1 else FALSE,
        series_time_info = series_time_info  # Full details by series
      ),

      # Data ordering mappings
      ordering = list(
        # Core mappings between original data and Stan-required order
        stan_to_original = stan_to_original,
        original_to_stan = original_to_stan,

        # Index mappings for times_trend matrix interpretation
        time_indices = time_indices,
        series_indices = series_indices,
        group_indices = if (!is.null(trend_specs$gr) && trend_specs$gr != 'NA')
                          match(data[[trend_specs$gr]], sort(unique(data[[trend_specs$gr]]))) else NULL,
        subgroup_indices = if (!is.null(trend_specs$subgr) && trend_specs$subgr != 'NA')
                             match(data[[trend_specs$subgr]], sort(unique(data[[trend_specs$subgr]]))) else NULL,

        # Efficiency flags
        requires_reordering = !identical(seq_len(nrow(data)), stan_to_original)
      ),

      # Trend model metadata
      trend = if (!is.null(trend_specs)) list(
        trend_type = trend_specs$trend %||% trend_specs$trend_model %||% NULL,
        has_trend = TRUE,
        is_factor_model = !is.null(trend_specs$n_lv) && trend_specs$n_lv < length(unique_series),
        correlation_structure = list(
          cor = trend_specs$cor %||% FALSE,
          ma = trend_specs$ma %||% FALSE,
          lags = trend_specs$lags %||% 1
        ),
        trend_specs = trend_specs
      ) else list(has_trend = FALSE),

      # Validation flags
      validation = list(
        data_complete = !any(is.na(data[c(time_var, series_var)])),
        validation_timestamp = Sys.time()
      )
    )
  }

  # Validate regular intervals for non-CAR trends
  if (!is.null(trend_type) && trend_type != "CAR") {
    validate_regular_time_intervals(data[[time_var]], time_var)
  }

  return(dimensions)
}

#' Generate Observation to Trend Mapping
#'
#' @description
#' Creates mapping arrays that align each observation in brms-ordered data
#' to its corresponding position in the trend matrix. This solves the problem
#' where brms excludes NA observations but doesn't provide an obs_ind array.
#'
#' @param data Data frame with observations (may include NAs)
#' @param response_var Name of response variable to check for missing values
#' @param time_var Name of time variable
#' @param series_var Name of series variable
#' @param dimensions List from extract_time_series_dimensions with time series structure
#' @return List containing obs_trend_time and obs_trend_series arrays for Stan
#' @noRd
generate_obs_trend_mapping <- function(data, response_var, time_var = "time",
                                      series_var = "series", dimensions = NULL) {
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_string(response_var)
  checkmate::assert_string(time_var)
  checkmate::assert_string(series_var)

  # Validate dimensions parameter when provided
  if (!is.null(dimensions)) {
    checkmate::assert_list(dimensions)
    required_fields <- c("unique_times", "unique_series", "n_time", "n_series")
    missing_fields <- setdiff(required_fields, names(dimensions))
    if (length(missing_fields) > 0) {
      stop(insight::format_error(
        "Dimensions list missing required fields: {paste(missing_fields, collapse = ', ')}"
      ), call. = FALSE)
    }
  }

  # Validate required columns exist
  if (!response_var %in% colnames(data)) {
    stop(insight::format_error(
      "Response variable {.field {response_var}} not found in data.",
      "Available variables: {paste(colnames(data), collapse = ', ')}"
    ), call. = FALSE)
  }

  if (!time_var %in% colnames(data)) {
    stop(insight::format_error(
      "Time variable {.field {time_var}} not found in data.",
      "Available variables: {paste(colnames(data), collapse = ', ')}"
    ), call. = FALSE)
  }

  if (!series_var %in% colnames(data)) {
    stop(insight::format_error(
      "Series variable {.field {series_var}} not found in data.",
      "Available variables: {paste(colnames(data), collapse = ', ')}"
    ), call. = FALSE)
  }

  # Extract dimensions if not provided
  if (is.null(dimensions)) {
    dimensions <- extract_time_series_dimensions(data, time_var, series_var)
  }

  # Identify non-missing observations (brms will only use these)
  non_missing_idx <- which(!is.na(data[[response_var]]))

  # Handle edge case where all observations are missing
  if (length(non_missing_idx) == 0) {
    stop(insight::format_error(
      "All observations are missing for response variable {.field {response_var}}.",
      "Cannot create observation mappings without any valid data."
    ), call. = FALSE)
  }

  # Extract time and series indices for non-missing observations
  obs_data <- data[non_missing_idx, , drop = FALSE]

  # Get sorted unique values from dimensions
  sorted_unique_times <- dimensions$unique_times
  sorted_unique_series <- dimensions$unique_series

  # Create time and series index mappings
  # These map each observation to its position in the trend matrix
  obs_trend_time <- match(obs_data[[time_var]], sorted_unique_times)
  obs_trend_series <- match(obs_data[[series_var]], sorted_unique_series)

  # Validate the mappings
  if (any(is.na(obs_trend_time))) {
    stop(insight::format_error(
      "Failed to map some observations to time indices.",
      "This indicates a data structure problem."
    ), call. = FALSE)
  }

  if (any(is.na(obs_trend_series))) {
    stop(insight::format_error(
      "Failed to map some observations to series indices.",
      "This indicates a data structure problem."
    ), call. = FALSE)
  }

  # Validate bounds
  if (any(obs_trend_time < 1 | obs_trend_time > dimensions$n_time)) {
    stop(insight::format_error(
      "Time indices out of bounds: must be in [1, {dimensions$n_time}].",
      "Found indices: [{min(obs_trend_time)}, {max(obs_trend_time)}]"
    ), call. = FALSE)
  }

  if (any(obs_trend_series < 1 | obs_trend_series > dimensions$n_series)) {
    stop(insight::format_error(
      "Series indices out of bounds: must be in [1, {dimensions$n_series}].",
      "Found indices: [{min(obs_trend_series)}, {max(obs_trend_series)}]"
    ), call. = FALSE)
  }

  return(list(
    obs_trend_time = as.integer(obs_trend_time),
    obs_trend_series = as.integer(obs_trend_series),
    n_obs_non_missing = length(non_missing_idx),
    has_missing = length(non_missing_idx) < nrow(data)
  ))
}

warn_default_time_variable <- function() {
  if (!identical(Sys.getenv("TESTTHAT"), "true")) {
    rlang::warn(
      insight::format_warning(
        "Using default time variable 'time'.",
        "Specify {.field time = your_time_var} if your time variable has a different name."
      ),
      .frequency = "once",
      .frequency_id = "mvgam_default_time_var"
    )
  }
}

warn_default_series_variable <- function() {
  if (!identical(Sys.getenv("TESTTHAT"), "true")) {
    rlang::warn(
      insight::format_warning(
        "Using default series variable 'series'.",
        "Specify {.field series = your_series_var} if your series variable has a different name."
      ),
      .frequency = "once",
      .frequency_id = "mvgam_default_series_var"
    )
  }
}

#' Validate mvgam_trend object structure
#' @param trend_obj mvgam_trend object to validate
#' @return Logical TRUE if valid, stops with error if not
#' @noRd
validate_mvgam_trend <- function(trend_obj) {
  checkmate::assert_class(trend_obj, "mvgam_trend")
  checkmate::assert_list(trend_obj, min.len = 1)
  checkmate::assert_string(trend_obj$trend, min.chars = 1)

  # Validate required fields exist
  required_fields <- c("trend", "time", "series")
  missing_fields <- setdiff(required_fields, names(trend_obj))
  if (length(missing_fields) > 0) {
    stop(insight::format_error(
      "Missing required fields in mvgam_trend object: {.field {missing_fields}}"
    ), call. = FALSE)
  }

  invisible(TRUE)
}

#' Validate proportional values (0-1 range)
#' @param x Numeric value to validate
#' @param name Parameter name for error messages
#' @return Logical TRUE if valid, stops with error if not
#' @noRd
validate_proportional <- function(x, name = deparse(substitute(x))) {
  checkmate::assert_number(x, lower = 0, upper = 1, .var.name = name)
  invisible(TRUE)
}

#' Validate positive integers
#' @param x Integer value to validate
#' @param name Parameter name for error messages
#' @return Logical TRUE if valid, stops with error if not
#' @noRd
validate_pos_integer <- function(x, name = deparse(substitute(x))) {
  checkmate::assert_int(x, lower = 1, .var.name = name)
  invisible(TRUE)
}

#' Evaluate an expression without printing output or messages
#' @param expr expression to be evaluated
#' @param type type of output to be suppressed (see ?sink)
#' @param try wrap evaluation of expr in 'try' and
#'   not suppress outputs if evaluation fails?
#' @param silent actually evaluate silently?
#' @noRd
eval_silent <- function(
    expr,
    type = "output",
    try = FALSE,
    silent = TRUE,
    ...
) {
  try <- as_one_logical(try)
  silent <- as_one_logical(silent)
  type <- match.arg(type, c("output", "message"))
  expr <- substitute(expr)
  envir <- parent.frame()
  if (silent) {
    if (try && type == "message") {
      try_out <- try(utils::capture.output(
        out <- eval(expr, envir),
        type = type,
        ...
      ))
      if (is_try_error(try_out)) {
        # try again without suppressing error messages
        out <- eval(expr, envir)
      }
    } else {
      utils::capture.output(out <- eval(expr, envir), type = type, ...)
    }
  } else {
    out <- eval(expr, envir)
  }
  out
}

#' Check if x is a try-error resulting from try()
is_try_error <- function(x) {
  inherits(x, "try-error")
}

#' Check if trend_specs represents multivariate trends
#'
#' @description
#' Determines whether trend specifications represent a multivariate model
#' (named list of trend specifications) or a univariate model (single trend).
#'
#' @param trend_specs Trend specifications to check
#' @return Logical indicating if multivariate
#' @noRd
is_multivariate_trend_specs <- function(trend_specs) {
  if (is.null(trend_specs)) {
    return(FALSE)
  }

  # Standardized structure check:
  # Multivariate: named list without trend fields (response names as keys)
  # Univariate: direct trend object with trend field
  if (is.list(trend_specs) && !is.null(names(trend_specs))) {
    # Check if this is a direct trend object (has trend field) or multivariate (response names)
    has_trend_field <- any(c("trend", "trend_type", "trend_model") %in% names(trend_specs))
    return(!has_trend_field)  # Multivariate if no trend field at top level
  }

  # Univariate case: direct trend specification object
  return(FALSE)
}

#' Validate Factor Levels
#'
#' @description
#' Checks for unused factor levels that could cause Stan indexing issues.
#' Issues warnings for validation phase, allows auto-dropping in preparation phase.
#'
#' @param data Data frame containing the factor variable
#' @param var_name Name of the factor variable to check
#' @param data_name Name of the data object (for error messages)
#' @param auto_drop Whether to automatically drop unused levels
#' @return Modified data if auto_drop=TRUE, otherwise original data
#' @noRd
validate_factor_levels <- function(data, var_name, data_name = "data", auto_drop = FALSE) {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(var_name)
  checkmate::assert_flag(auto_drop)

  if (!var_name %in% names(data)) {
    return(data)  # Variable doesn't exist, will be caught elsewhere
  }

  if (!is.factor(data[[var_name]])) {
    return(data)  # Not a factor, will be caught elsewhere
  }

  var_data <- data[[var_name]]
  used_levels <- unique(var_data)
  all_levels <- levels(var_data)
  unused_levels <- setdiff(all_levels, used_levels)

  if (length(unused_levels) > 0) {
    if (auto_drop) {
      # Auto-drop unused levels
      data[[var_name]] <- droplevels(var_data)
    } else {
      # Warn about unused levels
      rlang::warn(
        message = insight::format_warning(
          "Factor variable '{var_name}' in {data_name} has unused levels: {paste(unused_levels, collapse = ', ')}.",
          "Consider using droplevels() to remove unused factor levels.",
          "This may cause indexing issues in Stan model compilation."
        ),
        .frequency = "once"
      )
    }
  }

  return(data)
}

# Stan Code Structure and Syntax Validation Utilities
# ===================================================

#' Validate Stan Code Structure
#'
#' @description
#' Validates that Stan code contains required blocks (data, parameters, model).
#'
#' @param stan_code Character string containing Stan model code
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_stan_code_structure <- function(stan_code) {
  checkmate::assert_string(stan_code, min.chars = 1)

  # Required Stan blocks
  required_blocks <- c("data", "parameters", "model")

  # Check for each required block
  missing_blocks <- character(0)

  for (block in required_blocks) {
    # Pattern to match block declaration
    block_pattern <- paste0("\\b", block, "\\s*\\{")

    if (!grepl(block_pattern, stan_code, ignore.case = FALSE)) {
      missing_blocks <- c(missing_blocks, block)
    }
  }

  if (length(missing_blocks) > 0) {
    stop(insight::format_error(
      "Missing required Stan block{?s}: {.field {missing_blocks}}",
      "Stan models must contain data, parameters, and model blocks."
    ))
  }

  invisible(TRUE)
}

#' Check if Braces are Balanced
#'
#' @description
#' Checks if opening and closing braces are properly balanced in Stan code.
#'
#' @param stan_code Character string containing Stan model code
#' @return Logical indicating whether braces are balanced
#' @noRd
are_braces_balanced <- function(stan_code) {
  checkmate::assert_string(stan_code)

  # Split into individual characters
  chars <- unlist(strsplit(stan_code, "", fixed = TRUE))

  # Track brace depth
  depth <- 0

  for (char in chars) {
    if (char == "{") {
      depth <- depth + 1
    } else if (char == "}") {
      depth <- depth - 1

      # If depth goes negative, we have unmatched closing brace
      if (depth < 0) {
        return(FALSE)
      }
    }
  }

  # Return TRUE only if depth is exactly 0 (all braces matched)
  return(depth == 0)
}

#' Validate Stan Code Fragment
#'
#' @description
#' Validates individual Stan code fragments (from stanvars) without requiring
#' complete model structure. Checks syntax, braces, and basic content without
#' enforcing the presence of all Stan blocks.
#'
#' @param fragment Character string containing Stan code fragment
#' @param expected_content Optional character vector of strings expected in fragment
#' @param expected_block Optional character string of expected block type ("functions", "data", etc.)
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_stan_code_fragment <- function(fragment, expected_content = NULL, expected_block = NULL) {
  checkmate::assert_string(fragment, min.chars = 1)

  # Basic syntax validation: check for balanced braces
  if (!are_braces_balanced(fragment)) {
    stop(insight::format_error(
      "Stan code fragment has unbalanced braces.",
      "Check for missing opening or closing braces in fragment."
    ))
  }

  # Check for expected content if provided
  if (!is.null(expected_content)) {
    missing_content <- character(0)
    for (content in expected_content) {
      if (!grepl(content, fragment, fixed = TRUE)) {
        missing_content <- c(missing_content, content)
      }
    }

    if (length(missing_content) > 0) {
      stop(insight::format_error(
        "Stan code fragment missing expected content:",
        paste(missing_content, collapse = ", "),
        "Fragment may be incomplete or incorrectly generated."
      ))
    }
  }

  # Check for expected block declaration if provided
  if (!is.null(expected_block)) {
    block_pattern <- paste0(expected_block, "\\s*\\{")
    if (!grepl(block_pattern, fragment)) {
      stop(insight::format_error(
        "Stan code fragment missing expected block: {.field {expected_block}}",
        "Fragment should contain '{expected_block} {{' declaration."
      ))
    }
  }

  # Basic content validation: should not be just whitespace
  if (nchar(trimws(fragment)) == 0) {
    stop(insight::format_error(
      "Stan code fragment is empty or contains only whitespace."
    ))
  }

  invisible(TRUE)
}

# Comprehensive Stan Code Validation
# ==================================

#' Validate Combined Stan Code Result
#'
#' @description
#' Performs comprehensive validation of a combined Stan code result including
#' syntax checking, data-code compatibility, and structure validation.
#'
#' @param result List containing stancode, standata, and has_trends fields
#' @param silent Logical indicating whether to suppress validation messages
#' @return List with validation results (valid, syntax_valid, data_valid)
#' @noRd
validate_combined_stancode <- function(result, silent = FALSE) {
  checkmate::assert_list(result, names = "named")
  checkmate::assert_flag(silent)

  # Initialize validation results
  validation <- list(
    valid = FALSE,
    syntax_valid = FALSE,
    data_valid = FALSE,
    errors = character(0)
  )

  # Check required fields
  required_fields <- c("stancode", "standata", "has_trends")
  missing_fields <- setdiff(required_fields, names(result))

  if (length(missing_fields) > 0) {
    validation$errors <- c(validation$errors,
                           paste("Missing required fields:",
                                 paste(missing_fields, collapse = ", ")))
    return(validation)
  }

  # Validate Stan code syntax - let errors bubble up naturally
  validate_stan_code_structure(result$stancode)
  validate_stan_code(result$stancode)  # Use unified validation function
  validation$syntax_valid <- TRUE

  # Validate data-code compatibility - let errors bubble up naturally
  validate_data_code_compatibility(result$stancode, result$standata)
  validation$data_valid <- TRUE

  # Overall validation status
  validation$valid <- validation$syntax_valid && validation$data_valid

  return(validation)
}


#' Validate Data-Code Compatibility
#'
#' @description
#' Checks that all data variables declared in Stan code are present in standata
#' and that data types are compatible.
#'
#' @param stan_code Character string containing Stan model code
#' @param stan_data List containing Stan data
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_data_code_compatibility <- function(stan_code, stan_data) {
  checkmate::assert_string(stan_code, min.chars = 1)
  checkmate::assert_list(stan_data, names = "named")

  # Extract data block from Stan code
  data_block <- extract_stan_block_content(stan_code, "data")

  # Parse variable declarations from data block
  required_vars <- parse_data_declarations(data_block)

  # Check that all required variables are present in stan_data
  missing_vars <- setdiff(required_vars, names(stan_data))

  if (length(missing_vars) > 0) {
    stop(insight::format_error(
      "Missing required data variables: {.field {missing_vars}}",
      "Stan data block declares variables not provided in standata.",
      "Add missing variables to standata or remove from Stan data block."
    ))
  }

  invisible(TRUE)
}

#' Parse Data Declarations from Stan Data Block
#'
#' @description
#' Extracts variable names from Stan data block declarations.
#' This is a simplified parser for basic variable declarations.
#'
#' @param data_block Character string containing Stan data block content
#' @return Character vector of declared variable names
#' @noRd
parse_data_declarations <- function(data_block) {
  checkmate::assert_string(data_block)

  if (nchar(data_block) == 0) {
    return(character(0))
  }

  # Split into lines and clean up
  lines <- strsplit(data_block, "\n")[[1]]
  lines <- trimws(lines)
  lines <- lines[nchar(lines) > 0]  # Remove empty lines
  lines <- lines[!grepl("^//", lines)]  # Remove comment lines

  var_names <- character(0)

  for (line in lines) {
    # Look for variable declarations (simplified pattern)
    # Pattern: type<constraints> variable_name;
    # Examples: "int N;", "vector[N] y;", "real<lower=0> sigma;"

    # Remove inline comments
    line <- sub("//.*$", "", line)
    line <- trimws(line)

    if (nchar(line) == 0) next

    # Basic pattern for variable declarations
    # This is a simplified approach - a full parser would be more robust
    if (grepl(";\\s*$", line)) {  # Line ends with semicolon
      # Extract variable name (last word before semicolon)
      clean_line <- gsub(";\\s*$", "", line)  # Remove semicolon
      tokens <- strsplit(clean_line, "\\s+")[[1]]

      if (length(tokens) >= 2) {
        # Variable name is typically the last token
        var_name <- tokens[length(tokens)]

        # Remove array subscripts if present: variable[N] -> variable
        var_name <- gsub("\\[.*\\]", "", var_name)

        # Remove any remaining special characters
        var_name <- gsub("[^a-zA-Z0-9_]", "", var_name)

        if (nchar(var_name) > 0) {
          var_names <- c(var_names, var_name)
        }
      }
    }
  }

  return(unique(var_names))
}

#' Check Semicolons in a Stan Code Block
#'
#' @description
#' Checks for missing semicolons in variable declarations within a Stan block.
#'
#' @param block_content Character string containing Stan block content
#' @return Logical indicating whether semicolon syntax is correct
#' @noRd
check_block_semicolons <- function(block_content) {
  checkmate::assert_string(block_content)

  if (nchar(block_content) == 0) {
    return(TRUE)
  }

  # Split into lines and clean up
  lines <- strsplit(block_content, "\n")[[1]]
  lines <- trimws(lines)
  lines <- lines[nchar(lines) > 0]  # Remove empty lines
  lines <- lines[!grepl("^//", lines)]  # Remove comment-only lines

  for (line in lines) {
    # Remove inline comments
    line <- sub("//.*$", "", line)
    line <- trimws(line)

    if (nchar(line) == 0) next

    # Skip lines that are just braces or control structures
    if (grepl("^[{}]\\s*$", line)) next

    # Check if line looks like a variable declaration or statement
    # Basic pattern: looks like a type declaration or assignment
    if (grepl("\\b(int|real|vector|matrix|array)\\b", line, ignore.case = FALSE) ||
        grepl("\\w+\\s*[~=]", line)) {

      # This line should end with a semicolon
      if (!grepl(";\\s*$", line)) {
        return(FALSE)
      }
    }
  }

  return(TRUE)
}

#' @noRd
validate_stan_code <- function(stan_code, backend = "rstan", silent = TRUE, ...) {
  checkmate::assert_string(stan_code)
  checkmate::assert_choice(backend, c("rstan", "cmdstanr"))

  # Handle empty string case - always error for empty code
  if (nchar(stan_code) == 0) {
    stop(insight::format_error(
      "Empty Stan code provided.",
      "Stan code must contain at least one character."
    ))
  }

  # Primary validation using rstan::stanc() (most comprehensive and up-to-date)
  if (backend == "rstan") {
    if (!requireNamespace("rstan", quietly = TRUE)) {
      stop(insight::format_error(
        "Package {.pkg rstan} is required for Stan code validation.",
        "Install rstan or use cmdstanr backend."
      ))
    }

    # rstan::stanc() doesn't accept silent parameter, so filter it out
    # Always let rstan::stanc() errors show directly - no masking
    rstan::stanc(model_code = stan_code, verbose = FALSE, ...)
    invisible(TRUE)
  } else {
    # cmdstanr backend (fallback) - pass silent through
    return(parse_model_cmdstanr(stan_code, silent = silent, ...))
  }
}

#' Parse Stan Model Code with cmdstanr
#'
#' @description
#' Validates Stan model code using cmdstanr::cmdstan_model without compilation.
#' Based on existing mvgam patterns in backends.R.
#'
#' @param model Stan model code
#' @param silent Numeric indicating verbosity level
#' @param ... Additional arguments passed to cmdstanr functions
#' @return Validated Stan model code
#' @noRd
parse_model_cmdstanr <- function(model, silent = 1, ...) {
  checkmate::assert_string(model, min.chars = 1)

  # Check if cmdstanr is available
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop(insight::format_error(
      "Package {.pkg cmdstanr} is required for Stan code validation.",
      "Install cmdstanr or use rstan backend."
    ))
  }

  # Write Stan model to temporary file - let errors bubble up
  temp_file <- cmdstanr::write_stan_file(model)

  # Validate using cmdstan_model without compilation, using existing eval_silent
  out <- eval_silent(
    cmdstanr::cmdstan_model(temp_file, compile = FALSE, ...),
    type = "message",
    try = TRUE,
    silent = silent > 0L
  )

  if (inherits(out, "try-error")) {
    stop(insight::format_error(
      "Stan code validation failed with cmdstanr backend.",
      "Check Stan syntax and model structure.",
      "Error details: {attr(out, 'condition')$message}"
    ))
  }

  # Check syntax and return code - let errors bubble up
  out$check_syntax(quiet = TRUE)
  return(paste(out$code(), collapse = "\n"))
}

#'Argument validation functions
#'@param data Data to be validated (list or data.frame)
#'@noRd
validate_series_time = function(
    data,
    name = 'data',
    time_var,
    series_var = NULL,
    check_levels = TRUE,
    check_times = TRUE,
    is_multivariate = FALSE
) {
  # Input validation (NON-NEGOTIABLE per CLAUDE.md)
  checkmate::assert_data_frame(data, .var.name = name)
  checkmate::assert_string(name)
  checkmate::assert_string(time_var)
  checkmate::assert_string(series_var, null.ok = is_multivariate)  # Allow NULL for multivariate
  checkmate::assert_logical(check_levels, len = 1)
  checkmate::assert_logical(check_times, len = 1)
  checkmate::assert_logical(is_multivariate, len = 1)

  # Ungroup any grouped data
  data %>%
    dplyr::ungroup() -> data

  # Common validation: time variable must exist
  if (!time_var %in% colnames(data)) {
    stop(insight::format_error(
      "{.field {name}} does not contain a '{time_var}' variable."
    ), call. = FALSE)
  }

  # Dispatch to appropriate validation strategy
  if (is_multivariate) {
    data <- validate_multivariate_series_time(data, name, time_var, check_times)
  } else {
    data <- validate_univariate_series_time(data, name, time_var, series_var, check_levels, check_times)
  }

  return(data)
}

#' Validate multivariate series time structure
#' @param data Data frame to validate
#' @param name Name for error messages
#' @param time_var Time variable name
#' @param check_times Whether to check time completeness
#' @return Validated data frame
#' @noRd
validate_multivariate_series_time <- function(data, name, time_var, check_times) {
  # Input validation
  checkmate::assert_data_frame(data)
  checkmate::assert_string(name)
  checkmate::assert_string(time_var)
  checkmate::assert_logical(check_times, len = 1)

  # For multivariate models, check time completeness at global level
  if (check_times) {
    min_time <- as.numeric(min(data[[time_var]]))
    max_time <- as.numeric(max(data[[time_var]]))

    unique_times_in_data <- sort(unique(data[[time_var]]))
    expected_times <- seq.int(from = min_time, to = max_time)

    if (!identical(as.numeric(unique_times_in_data), as.numeric(expected_times))) {
      stop(insight::format_error(
        "Time series in {.field {name}} is missing observations for one or more timepoints.",
        "Multivariate models require complete time sampling for all responses."
      ), call. = FALSE)
    }
  }

  return(data)
}

#' Validate univariate series time structure
#' @param data Data frame to validate
#' @param name Name for error messages
#' @param time_var Time variable name
#' @param series_var Series variable name
#' @param check_levels Whether to check factor levels
#' @param check_times Whether to check time completeness
#' @return Validated data frame
#' @noRd
validate_univariate_series_time <- function(data, name, time_var, series_var, check_levels, check_times) {
  # Input validation
  checkmate::assert_data_frame(data)
  checkmate::assert_string(name)
  checkmate::assert_string(time_var)
  checkmate::assert_string(series_var)
  checkmate::assert_logical(check_levels, len = 1)
  checkmate::assert_logical(check_times, len = 1)

  # Check that series variable exists and is a factor
  if (!series_var %in% colnames(data)) {
    stop(insight::format_error(
      "{.field {name}} does not contain a '{series_var}' variable."
    ), call. = FALSE)
  }

  if (!is.factor(data[[series_var]])) {
    stop(insight::format_error(
      "Variable {.field {series_var}} must be a factor.",
      "Convert to factor using: data${series_var} <- factor(data${series_var})"
    ), call. = FALSE)
  }

  # Check for unused factor levels in series variable
  data <- validate_factor_levels(data, series_var, name, auto_drop = FALSE)

  # Series factor must have all unique levels present if this is a forecast check
  if (check_levels) {
    if (!all(levels(data[[series_var]]) %in% unique(data[[series_var]]))) {
      stop(insight::format_error(
        "Mismatch between factor levels of {.field {series_var}} and unique values.",
        "Use setdiff(levels(data${series_var}), unique(data${series_var})) for guidance."
      ), call. = FALSE)
    }
  }

  # Check time completeness for each series
  if (check_times) {
    all_times_avail = function(time, min_time, max_time) {
      identical(
        as.numeric(sort(time)),
        as.numeric(seq.int(from = min_time, to = max_time))
      )
    }

    min_time <- as.numeric(min(data[[time_var]]))
    max_time <- as.numeric(max(data[[time_var]]))

    data.frame(series = data[[series_var]], time = data[[time_var]]) %>%
      dplyr::group_by(series) %>%
      dplyr::summarise(
        all_there = all_times_avail(time, min_time, max_time),
        .groups = 'drop'
      ) -> checked_times

    if (any(checked_times$all_there == FALSE)) {
      stop(insight::format_error(
        "One or more series in {.field {name}} is missing observations for one or more timepoints."
      ), call. = FALSE)
    }
  }

  return(data)
}

#'@noRd
as_one_logical = function(x, allow_na = FALSE) {
  s <- substitute(x)
  x <- as.logical(x)
  if (length(x) != 1L || anyNA(x) && !allow_na) {
    s <- deparse0(s, max_char = 100L)
    stop("Cannot coerce '", s, "' to a single logical value.", call. = FALSE)
  }
  x
}

#' Validate Grouping Structure
#'
#' @description
#' Validates that grouping variables (gr, subgr) exist, are factors, and have
#' proper hierarchical structure for trend models that use grouping.
#'
#' @param data Data frame to validate
#' @param trend_model mvgam_trend object with grouping specifications
#' @param name Name of data object for error messages
#' @return Original data (validation only, no transformation)
#' @noRd
validate_grouping_structure = function(data, trend_model, name = 'data') {
  checkmate::assert_data_frame(data)

  # Extract variable names from trend_model
  gr_var <- if (!is.null(trend_model$gr) && trend_model$gr != 'NA') trend_model$gr else NULL
  subgr_var <- if (!is.null(trend_model$subgr) && trend_model$subgr != 'NA') trend_model$subgr else NULL

  # If no grouping is used, return early
  if (is.null(gr_var) && is.null(subgr_var)) {
    return(data)
  }

  # Validate grouping variables if they exist
  if (!is.null(gr_var)) {
    # Check gr variable exists and is factor
    if (!gr_var %in% names(data)) {
      stop(insight::format_error(
        "{name} does not contain grouping variable '{gr_var}'.",
        "The grouping variable '{gr_var}' was specified in the trend constructor but is missing from the data."
      ), call. = FALSE)
    }

    if (!is.factor(data[[gr_var]])) {
      stop(insight::format_error(
        "Grouping variable '{gr_var}' must be a factor.",
        "Convert to factor using: data${gr_var} <- factor(data${gr_var})"
      ), call. = FALSE)
    }

    # Check for unused factor levels in gr variable
    data <- validate_factor_levels(data, gr_var, name, auto_drop = FALSE)
  }

  if (!is.null(subgr_var)) {
    # Check subgr variable exists and is factor
    if (!subgr_var %in% names(data)) {
      stop(insight::format_error(
        "{name} does not contain subgrouping variable '{subgr_var}'.",
        "The subgrouping variable '{subgr_var}' was specified in the trend constructor but is missing from the data."
      ), call. = FALSE)
    }

    if (!is.factor(data[[subgr_var]])) {
      stop(insight::format_error(
        "Subgrouping variable '{subgr_var}' must be a factor.",
        "Convert to factor using: data${subgr_var} <- factor(data${subgr_var})"
      ), call. = FALSE)
    }

    # Check for unused factor levels in subgr variable
    data <- validate_factor_levels(data, subgr_var, name, auto_drop = FALSE)
  }

  # If both gr and subgr are specified, validate hierarchical structure
  if (!is.null(gr_var) && !is.null(subgr_var)) {
    validate_complete_grouping(data, gr_var, subgr_var, name)
  }

  return(data)
}

#' Validate and Process Trend Parameters
#'
#' @description
#' Validates and processes complex trend parameters that require data context.
#' This function handles parameter processing that was moved from constructors
#' to provide better data context for validation.
#'
#' @param trend_spec Trend specification
#' @param data Data frame with time series data
#' @return Enhanced trend specification with processed parameters
#' @noRd
validate_and_process_trend_parameters <- function(trend_spec, data) {
  # Input validation with checkmate
  checkmate::assert_list(trend_spec, min.len = 1)
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_string(trend_spec$trend, min.chars = 1)

  # Process lag parameters for AR/VAR models that have complex lag logic
  if (!is.null(trend_spec$p) && trend_spec$trend %in% c("AR", "VAR")) {
    trend_spec$p <- process_lag_parameters(trend_spec$p, trend_spec$trend)
  }

  # Process capacity parameter for PW models with data context
  if (trend_spec$trend == "PW" && !is.null(trend_spec$cap)) {
    trend_spec$cap <- process_capacity_parameter(trend_spec$cap, data)
  }

  # Add dimension information for downstream processing
  if (!is.null(trend_spec$dimensions)) {
    # Dimensions already extracted, validate consistency with parameters
    if (!is.null(trend_spec$n_lv) && trend_spec$n_lv >= trend_spec$dimensions$n_series) {
      stop(insight::format_error(
        "Factor model requires {.field n_lv < n_series}.",
        "You specified {.field n_lv = {trend_spec$n_lv}} with {trend_spec$dimensions$n_series} series."
      ))
    }
  }

  return(trend_spec)
}

#' Process Lag Parameters
#'
#' @description
#' Processes lag parameters for AR/VAR models, handling complex lag structures.
#'
#' @param p Lag parameter(s)
#' @param trend_type Trend type for context
#' @return Processed lag parameter(s)
#' @noRd
process_lag_parameters <- function(p, trend_type) {
  # Input validation with checkmate
  checkmate::assert_string(trend_type, min.chars = 1)

  if (is.null(p)) {
    return(1L)  # Default lag
  }

  # Validate parameter type and range
  checkmate::assert_integerish(p, lower = 1, min.len = 1, any.missing = FALSE)

  # Convert to integer
  p <- as.integer(p)

  # Additional validation for edge cases
  if (any(p <= 0) || any(!is.finite(p))) {
    stop(insight::format_error(
      "Lag parameters must be positive integers.",
      "You specified {.field p = {paste(p, collapse = ', ')}} for {.field {trend_type}} model."
    ))
  }

  # Sort and remove duplicates for consistent processing
  p <- sort(unique(p))

  return(p)
}

#' Process Capacity Parameter
#'
#' @description
#' Processes capacity parameter for piecewise (PW) models with data context validation.
#'
#' @param cap Capacity parameter
#' @param data Data frame for context
#' @return Processed capacity parameter
#' @noRd
process_capacity_parameter <- function(cap, data) {
  # Input validation with checkmate
  checkmate::assert_data_frame(data, min.rows = 1)

  if (is.null(cap)) {
    return(NULL)
  }

  # If character, validate it's a column in data
  if (is.character(cap)) {
    checkmate::assert_string(cap, min.chars = 1)
    if (!cap %in% colnames(data)) {
      stop(insight::format_error(
        "Capacity variable '{.field {cap}}' not found in data.",
        "Available variables: {.field {paste(colnames(data), collapse = ', ')}}"
      ))
    }
    return(cap)
  }

  # If numeric, validate it's positive
  if (is.numeric(cap)) {
    checkmate::assert_number(cap, lower = 0, finite = TRUE)
    if (cap <= 0) {
      stop(insight::format_error(
        "Capacity must be a positive finite number.",
        "You specified {.field cap = {cap}}."
      ))
    }
    return(cap)
  }

  stop(insight::format_error(
    "Capacity parameter must be either a positive number or a column name.",
    "You specified {.field cap = {cap}} of type {.field {class(cap)}}."
  ))
}

#' Validate Factor + Hierarchical Restriction
#'
#' @description
#' Validates that factor models and hierarchical grouping are not used together.
#' This restriction applies to all trends that support both features.
#'
#' @param trend_specs Trend specification list containing n_lv, gr, subgr parameters
#' @param n_series Number of series from data_info
#' @param trend_name Name of the trend type for error messages (e.g., "RW", "AR", "VAR")
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_no_factor_hierarchical <- function(trend_specs, n_series, trend_name) {
  checkmate::assert_list(trend_specs, names = "named")
  checkmate::assert_int(n_series, lower = 1)
  checkmate::assert_string(trend_name)

  # Check if this is a factor model
  n_lv <- trend_specs$n_lv
  is_factor_model <- !is.null(n_lv) && n_lv < n_series

  # Check if hierarchical grouping is requested
  use_grouping <- !is.null(trend_specs$gr) && trend_specs$gr != 'NA'

  # Factor models are incompatible with hierarchical grouping
  if (use_grouping && is_factor_model) {
    stop(insight::format_error(
      glue::glue("Hierarchical {trend_name} models cannot use factor models"),
      "Use {.field n_lv = n_series} or remove {.field gr}/{.field subgr} parameters"
    ))
  }

  return(invisible(TRUE))
}

# Formula Parsing Helpers
# =======================
# These functions provide structure-preserving formula manipulation using rlang
# to avoid reformulate() issues with complex formula structures like (1|series)

#' Parse base formula by removing trend constructor terms (structure-preserving)
#'
#' @description
#' Safely removes trend constructor terms from a formula while preserving
#' complex structures like (1|group) random effects. Uses rlang AST manipulation
#' to avoid reformulate() issues.
#'
#' @param trend_formula A formula object containing potential trend terms
#' @param trend_terms Character vector of trend constructor patterns to remove
#' @return A formula object with trend terms removed, preserving structure
#' @noRd
parse_base_formula_safe <- function(trend_formula, trend_terms) {
  # Input validation - non-negotiable per CLAUDE.md
  checkmate::assert_class(trend_formula, "formula")
  checkmate::assert_character(trend_terms, min.len = 0)

  if (length(trend_terms) == 0) {
    return(trend_formula)
  }

  # Extract and clean right-hand side expression using rlang
  rhs_expr <- rlang::f_rhs(trend_formula)
  cleaned_expr <- remove_trend_expressions(rhs_expr, trend_terms, depth = 0)

  # Handle case where all terms were removed
  if (is.null(cleaned_expr)) {
    cleaned_expr <- quote(1)  # Default to intercept-only
  }

  # Reconstruct formula preserving environment (no lhs for trend formulas)
  rlang::new_formula(lhs = NULL, rhs = cleaned_expr, env = rlang::f_env(trend_formula))
}

#' Recursively remove trend expressions from AST
#'
#' @description
#' Walks the expression tree to remove trend constructor calls while preserving
#' the overall formula structure. Handles nested expressions safely.
#'
#' @param expr Expression to process
#' @param trend_patterns Character vector of trend patterns to match
#' @param depth Current recursion depth for protection
#' @return Cleaned expression or NULL if expression should be removed
#' @noRd
remove_trend_expressions <- function(expr, trend_patterns, depth = 0) {
  # Input validation
  checkmate::assert_character(trend_patterns)
  checkmate::assert_int(depth, lower = 0)

  # Recursion depth protection
  if (depth > 50) {
    stop(insight::format_error(
      "Formula nesting too deep (>50 levels).",
      "Simplify the trend formula structure."
    ))
  }

  if (rlang::is_call(expr, "+")) {
    args <- rlang::call_args(expr)
    lhs <- remove_trend_expressions(args[[1]], trend_patterns, depth + 1)
    rhs <- remove_trend_expressions(args[[2]], trend_patterns, depth + 1)

    # Handle removal logic preserving valid expressions
    if (is.null(lhs) && is.null(rhs)) {
      return(NULL)
    } else if (is.null(lhs)) {
      return(rhs)
    } else if (is.null(rhs)) {
      return(lhs)
    } else {
      return(rlang::expr(!!lhs + !!rhs))
    }
  } else {
    # Check if this expression is a trend term
    if (is_trend_term(expr, trend_patterns)) {
      return(NULL)
    } else {
      return(expr)
    }
  }
}

#' Check if brmsfit is Multivariate
#' @param brmsfit brms model fit object
#' @return Logical indicating if model is multivariate
#' @noRd
is_multivariate_brmsfit <- function(brmsfit) {
  checkmate::assert_class(brmsfit, "brmsfit")

  # Check if formula has multivariate structure
  if (!is.null(brmsfit$formula) && inherits(brmsfit$formula, "mvbrmsformula")) {
    return(TRUE)
  }

  # Check brmsterms for multivariate structure
  if (!is.null(brmsfit$formula)) {
    terms_obj <- try(brms::brmsterms(brmsfit$formula), silent = TRUE)
    if (!inherits(terms_obj, "try-error") && inherits(terms_obj, "mvbrmsterms")) {
      return(TRUE)
    }
  }

  return(FALSE)
}

#' Check if expression matches trend term patterns
#'
#' @description
#' Determines if an expression represents a trend constructor term that
#' should be removed from the base formula.
#'
#' @param expr Expression to check
#' @param trend_patterns Character vector of trend patterns to match against
#' @return Logical indicating if expression is a trend term
#' @noRd
is_trend_term <- function(expr, trend_patterns) {
  # Input validation
  checkmate::assert_character(trend_patterns)

  expr_text <- rlang::expr_text(expr)
  any(vapply(trend_patterns, function(pattern) {
    grepl(pattern, expr_text, fixed = TRUE)
  }, logical(1)))
}
