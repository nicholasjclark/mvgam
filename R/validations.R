#' Validate Nonlinear Trend Compatibility
#' 
#' @description
#' Validates that trend specifications are compatible with nonlinear model structure.
#' 
#' @param nl_components List of nonlinear components
#' @param trend_spec Trend specification
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_nonlinear_trend_compatibility <- function(nl_components, trend_spec) {
  checkmate::assert_list(nl_components)
  checkmate::assert_list(trend_spec, null.ok = TRUE)
  
  if (is.null(trend_spec)) {
    return(invisible(TRUE))
  }
  
  # Check that trend type is compatible with nonlinear models
  incompatible_trends <- c()  # Currently all trends should work
  
  if (trend_spec$type %in% incompatible_trends) {
    stop(insight::format_error(
      "Trend type '{trend_spec$type}' is not compatible with nonlinear models.",
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
  
  # Check for mvgam trend constructors
  trend_patterns <- c(
    "\\bRW\\s*\\(" = "RW()",
    "\\bAR\\s*\\(" = "AR()",
    "\\bVAR\\s*\\(" = "VAR()",
    "\\bGP\\s*\\(" = "GP()",
    "\\bCAR\\s*\\(" = "CAR()",
    "\\bZMVN\\s*\\(" = "ZMVN()"
  )
  
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
  
  # Check for 'series' identifier (should use 'trend' instead)
  if (grepl('\\bseries\\b', formula_str)) {
    stop(insight::format_error(
      "Trend formulas should use {.field trend} instead of {.field series}.",
      "For varying effects, use: {.code s(time, by = trend)} not {.code s(time, by = series)}"
    ))
  }
  
  # Check for offset terms (not allowed in trend formulas)
  validate_no_offsets_in_trends(formula)
  
  # Forbid brms autocorrelation in trend formulas (conflicts with State-Space)
  validate_no_brms_autocor_in_trends(formula_str)
  
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
    "\\bunstr\\s*\\(" = "unstr()"
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

                            #' Validate Multivariate Trend Constraints
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
                         
#' Validate trend order parameter
#'
#' Standardized validation for trend order parameters (p). Provides consistent
#'   error messages across trend types.
#'
#' @param p Order parameter to validate
#' @param max_order Maximum allowed order for this trend type
#' @param trend_type Name of trend type for error messages
#'
#' @noRd
validate_trend_order <- function(p, max_order, trend_type) {
  checkmate::assert_int(p, lower = 1, upper = max_order)

  if (p > max_order) {
    insight::format_error(
      "{.field {trend_type}} order too high.",
      "You specified {.field p = {p}} but maximum allowed is {.field {max_order}}.",
      "Please reduce the order or consider a different trend type."
    )
  }
}

#' Validate grouping arguments for trend constructors
#'
#' Validates and processes gr and subgr arguments with consistent error handling.
#'   Handles the logic for hierarchical groupings and series creation.
#'
#' @param gr Grouping variable (as character from deparse0)
#' @param subgr Subgrouping variable (as character from deparse0)
#'
#' @return List with validated gr and subgr values
#' @noRd
validate_time_variable <- function(time, data = NULL) {
  # Default to 'time' if NA specified (like brms)
  if (time == 'NA') {
    time <- 'time'
  }

  # If data is provided, validate that time variable exists and is appropriate
  if (!is.null(data)) {
    checkmate::assert_data_frame(data)

    if (!time %in% names(data)) {
      stop(insight::format_error(
        "Time variable {.field {time}} not found in data.",
        "Available variables: {.field {paste(names(data), collapse = ', ')}}.",
        "Please specify a valid time variable or ensure your data contains a 'time' column."
      ), call. = FALSE)
    }

    time_var <- data[[time]]
    if (!is.numeric(time_var) && !is.integer(time_var)) {
      stop(insight::format_error(
        "Time variable {.field {time}} must be numeric or integer.",
        "Found type: {.field {class(time_var)[1]}}.",
        "Time series models require ordered numeric time indices."
      ), call. = FALSE)
    }

    # Warning for non-standard time variable names
    if (time != 'time') {
      rlang::warn(
        insight::format_warning(
          "Using {.field {time}} as time variable instead of 'time'.",
          "This follows brms conventions for flexible time variable naming."
        ),
        .frequency = "once",
        .frequency_id = paste0("mvgam_custom_time_var_", time)
      )
    }
  }

  return(time)
}

warn_default_time_variable <- function() {
  rlang::warn(
    insight::format_warning(
      "Using default time variable 'time'.",
      "Specify {.field time = your_time_var} if your time variable has a different name."
    ),
    .frequency = "once",
    .frequency_id = "mvgam_default_time_var"
  )
}

warn_default_series_variable <- function() {
  rlang::warn(
    insight::format_warning(
      "Using default series variable 'series'.",
      "Specify {.field series = your_series_var} if your series variable has a different name."
    ),
    .frequency = "once",
    .frequency_id = "mvgam_default_series_var"
  )
}

validate_series_variable <- function(series, data = NULL) {
  # Default to 'series' if NA specified (like mvgam convention)
  if (series == 'NA') {
    series <- 'series'
  }

  # If data is provided, validate that series variable exists and is appropriate
  if (!is.null(data)) {
    checkmate::assert_data_frame(data)

    if (!series %in% names(data)) {
      stop(insight::format_error(
        "Series variable {.field {series}} not found in data.",
        "Available variables: {.field {paste(names(data), collapse = ', ')}}.",
        "Please specify a valid series variable or ensure your data contains a 'series' column."
      ), call. = FALSE)
    }

    series_var <- data[[series]]
    if (!is.character(series_var) && !is.factor(series_var)) {
      stop(insight::format_error(
        "Series variable {.field {series}} must be character or factor.",
        "Found type: {.field {class(series_var)[1]}}.",
        "Series identifiers should be categorical variables."
      ), call. = FALSE)
    }

    # Warning for non-standard series variable names
    if (series != 'series') {
      rlang::warn(
        insight::format_warning(
          "Using {.field {series}} as series variable instead of 'series'.",
          "This follows mvgam conventions for flexible series variable naming."
        ),
        .frequency = "once",
        .frequency_id = paste0("mvgam_custom_series_var_", series)
      )
    }
  }

  return(series)
}

validate_grouping_arguments <- function(gr, subgr) {

  # Set default subgr if no grouping specified
  if (gr == 'NA') {
    subgr <- 'series'
  }

  # Validate hierarchical grouping requirements
  if (gr != 'NA') {
    if (subgr == 'NA') {
      insight::format_error(
        'Hierarchical grouping requires subgrouping specification.',
        'You specified {.field gr = "{gr}"} but {.field subgr = NA}.',
        'Please provide a valid subgrouping variable.'
      )
    } else if (subgr == 'series') {
      insight::format_error(
        'Invalid subgrouping for hierarchical models.',
        'You cannot use {.field subgr = "series"} with {.field gr = "{gr}"}.',
        'The series variable is created internally from gr and subgr.'
      )
    }
  }

  return(list(gr = gr, subgr = subgr))
}

#' Validate correlation requirements
#'
#' Checks and auto-corrects correlation settings for hierarchical models.
#'   Issues one-time warnings when auto-correction occurs.
#'
#' @param gr Grouping variable
#' @param cor Correlation setting
#'
#' @return Corrected correlation setting (logical)
#' @noRd
validate_correlation_requirements <- function(gr, cor) {
  if (gr != 'NA' && !cor) {
    rlang::warn(
      insight::format_warning(
        "Hierarchical grouping specified without correlation.",
        "Setting {.field cor = TRUE} automatically for {.field gr = '{gr}'}."
      ),
      .frequency = "once",
      .frequency_id = "mvgam_hierarchical_correlation"
    )
    return(TRUE)  # Force cor = TRUE
  }
  return(cor)
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

#' @noRd
validate_trend <- function(trend_obj, ...) {

  # Basic validation
  checkmate::assert_class(trend_obj, "mvgam_trend")
  checkmate::assert_string(trend_obj$trend, min.chars = 1)

  # Optional components validation (only if present)
  if (!is.null(trend_obj$tpars)) {
    checkmate::assert_character(trend_obj$tpars)  # Allow empty tpars for trends with no trend-specific parameters
  }

  if (!is.null(trend_obj$forecast_fun)) {
    checkmate::assert_string(trend_obj$forecast_fun, min.chars = 1)
  }

  if (!is.null(trend_obj$stancode_fun)) {
    checkmate::assert_string(trend_obj$stancode_fun, min.chars = 1)
  }

  if (!is.null(trend_obj$bounds)) {
    checkmate::assert_list(trend_obj$bounds)
  }

  if (!is.null(trend_obj$characteristics)) {
    checkmate::assert_list(trend_obj$characteristics)
  }

  # Dynamic factor model constraints (only if n_lv is present)
  if (!is.null(trend_obj$n_lv)) {
    validate_dynamic_factor_constraints(trend_obj)
  }

  return(trend_obj)
}

#' Validate dynamic factor model constraints
#'
#' Checks identifiability constraints for dynamic factor models (n_lv > 0).
#'   Uses insight for user-friendly error messages and rlang for session
#'   warnings.
#'
#' @param trend_obj A trend object to validate
#'
#' @noRd
validate_dynamic_factor_constraints <- function(trend_obj) {

  n_lv <- trend_obj$n_lv

  # Check if this is a dynamic factor model
  if (!is.null(n_lv) && n_lv > 0) {

    # Validate n_lv parameter
    checkmate::assert_int(n_lv, lower = 1, upper = 20)

    # Constraint 1: No groupings allowed with dynamic factors
    if (trend_obj$gr != 'NA') {
      insight::format_error(
        "Dynamic factor models cannot use hierarchical groupings.",
        "You specified {.field n_lv = {n_lv}} and {.field gr = '{trend_obj$gr}'}.",
        "The factor structure provides the grouping mechanism.",
        "Please set {.field gr = 'NA'} or remove {.field n_lv}."
      )
    }

    if (trend_obj$subgr != 'series') {
      insight::format_error(
        "Dynamic factor models cannot use custom subgroupings.",
        "You specified {.field n_lv = {n_lv}} and {.field subgr = '{trend_obj$subgr}'}.",
        "The factor structure handles series relationships.",
        "Please use {.field subgr = 'series'} or remove {.field n_lv}."
      )
    }

    # Constraint 2: No moving average terms allowed
    if (trend_obj$ma) {
      insight::format_error(
        "Dynamic factor models cannot include moving average terms.",
        "You specified {.field n_lv = {n_lv}} and {.field ma = TRUE}.",
        "MA terms create identifiability issues with factor structures.",
        "Please set {.field ma = FALSE} or remove {.field n_lv}."
      )
    }

    # Constraint 3: Trend type must support factors
    supports_factors <- trend_obj$param_info$characteristics$supports_factors %||%
                       trend_obj$characteristics$supports_factors %||%
                       FALSE
    if (!supports_factors) {
      insight::format_error(
        "Trend type {.field {trend_obj$trend}} does not support dynamic factor models.",
        "You specified {.field n_lv = {n_lv}} with an incompatible trend type.",
        "Supported trend types: {.field RW}, {.field AR}, {.field VAR}."
      )
    }

    # One-time warning about variance constraints
    rlang::warn(
      insight::format_warning(
        "Dynamic factor model detected ({.field n_lv = {n_lv}}).",
        "Trend variance will be fixed for identifiability.",
        "Factor loadings provide the primary source of variation."
      ),
      .frequency = "once",
      .frequency_id = "mvgam_dynamic_factor_variance"
    )
  }

  # Additional constraint: Groupings require correlation
  if (trend_obj$gr != 'NA' && !trend_obj$cor) {
    insight::format_error(
      "Hierarchical groupings require correlation structure.",
      "You specified {.field gr = '{trend_obj$gr}'} but {.field cor = FALSE}.",
      "Please set {.field cor = TRUE} when using groupings."
    )
  }
}

#' Validate Factor Model Compatibility
#'
#' @description
#' Check if a trend specification is compatible with factor models.
#'
#' @param trend_spec Trend specification object
#' @return Invisibly returns TRUE if compatible, stops with error if not
#' @noRd
validate_factor_compatibility <- function(trend_spec) {
  # Only validate if n_lv is specified (indicating factor model intent)
  if (is.null(trend_spec$n_lv)) {
    return(invisible(TRUE))
  }

  # Check if trend type supports factor models
  # Handle both trend_type and trend field names for compatibility
  trend_type <- trend_spec$trend_type %||% trend_spec$trend
  if (is.null(trend_type)) {
    stop(insight::format_error(
      "trend_spec must contain {.field trend_type} or {.field trend} field"
    ))
  }
  trend_info <- get_trend_info(trend_type)

  if (!trend_info$supports_factors) {
    stop(insight::format_error(
      paste0("Factor models ({.field n_lv}) not supported for {.field ", trend_type, "} trends."),
      trend_info$incompatibility_reason,
      paste0("Remove {.field n_lv} parameter or use factor-compatible trends: {.val ", paste(get_factor_compatible_trends(), collapse = ", "), "}")
    ))
  }

  invisible(TRUE)
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

  # Extract data block from Stan code (split into lines first)
  code_lines <- strsplit(stan_code, "\n")[[1]]
  data_block <- extract_code_block(code_lines, "data")

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

#' Check Basic Semicolon Syntax in Stan Code
#'
#' @description
#' Performs basic checks for missing semicolons in Stan variable declarations.
#' This is a simplified check focusing on obvious syntax errors.
#'
#' @param stan_code Character string containing Stan model code
#' @return Logical indicating whether semicolon syntax appears correct
#' @noRd
check_semicolon_syntax <- function(stan_code) {
  checkmate::assert_string(stan_code)

  # Extract code blocks that should have semicolon-terminated statements (split into lines first)
  code_lines <- strsplit(stan_code, "\n")[[1]]
  data_block <- extract_code_block(code_lines, "data")
  param_block <- extract_code_block(code_lines, "parameters")

  # Check each block for syntax issues
  blocks_to_check <- list(
    data = data_block,
    parameters = param_block
  )

  for (block_name in names(blocks_to_check)) {
    block_content <- blocks_to_check[[block_name]]

    if (nchar(block_content) > 0) {
      if (!check_block_semicolons(block_content)) {
        return(FALSE)
      }
    }
  }

  return(TRUE)
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
validate_stan_code <- function(stan_code, backend = "rstan", ...) {
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

    # Always let rstan::stanc() errors show directly - no masking
    rstan::stanc(model_code = stan_code, verbose = FALSE, ...)
    invisible(TRUE)
  } else {
    # cmdstanr backend (fallback)
    return(parse_model_cmdstanr(stan_code, ...))
  }
}


#' Validate Stan Data Structure
#'
#' @description
#' Validates that Stan data has proper structure and types.
#'
#' @param stan_data List containing Stan data
#' @return Invisible TRUE if valid, stops/warns for issues
#' @noRd
validate_stan_data_structure <- function(stan_data) {
  checkmate::assert_list(stan_data)

  # Check for empty data
  if (length(stan_data) == 0) {
    stop(insight::format_error(
      "Empty Stan data provided.",
      "Stan models require at least some data elements."
    ))
  }

  # Check data types (Stan expects numeric, integer, or array types)
  valid_types <- c("numeric", "integer", "logical", "matrix", "array")

  for (name in names(stan_data)) {
    data_type <- class(stan_data[[name]])[1]

    if (!data_type %in% valid_types && !is.numeric(stan_data[[name]])) {
      insight::format_warning(
        "Unexpected data type for Stan element '{name}': {data_type}",
        "Stan typically expects numeric, integer, or matrix types."
      )
    }
  }

  invisible(TRUE)
}

#' Check if Object is Valid Stanvar
#'
#' @description
#' Validates that an object has the structure expected for a brms stanvar.
#'
#' @param stanvar Object to validate
#' @return Logical indicating whether object is a valid stanvar
#' @noRd
is_valid_stanvar <- function(stanvar) {
  # Handle NULL input
  if (is.null(stanvar)) {
    return(FALSE)
  }

  # Must be a list
  if (!is.list(stanvar)) {
    return(FALSE)
  }

  # Check if it's a brms stanvar object (has required brms components)
  required_components <- c("scode", "block")
  if (!all(required_components %in% names(stanvar))) {
    return(FALSE)
  }

  # scode must be character string (can be empty for data block stanvars)
  if (!is.character(stanvar$scode) || length(stanvar$scode) != 1) {
    return(FALSE)
  }

  # block must be valid Stan block name (brms uses abbreviated forms)
  valid_blocks <- c("data", "tdata", "parameters", "tparameters",
                   "model", "genquant",
                   "transformed_data", "transformed_parameters", "generated_quantities")
  if (!is.character(stanvar$block) || length(stanvar$block) != 1 ||
      !stanvar$block %in% valid_blocks) {
    return(FALSE)
  }

  return(TRUE)
}

#' Validate Trend Specification
#'
#' Checks if a trend specification is valid for code generation.
#'
#' @param trend_spec Trend specification object
#' @return Logical indicating validity
#' @noRd
validate_trend_spec <- function(trend_spec) {
  if (!is.list(trend_spec)) return(FALSE)
  if (is.null(trend_spec$trend_type)) return(FALSE)
  if (!trend_spec$trend_type %in% list_trend_types()$trend_type) return(FALSE)

  TRUE
}

#' Validate Time Series Structure for Trends
#'
#' @description
#' Ensures time series data is compatible with specified trend models.
#' Leverages existing mvgam validation functions.
#'
#' @param data Data to validate (data.frame or list)
#' @param trend_spec Trend specification containing trend model info
#' @param silent Verbosity level
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_time_series_for_trends <- function(data, trend_spec, silent = 1) {
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_list(trend_spec)

  # Use existing mvgam validation function
  # Pass the complete trend_spec (mvgam_trend object) for variable names
  validated_data <- validate_series_time(
      data = data,
      name = "data",
      trend_model = trend_spec,
      check_levels = TRUE,
      check_times = TRUE
    )
  
  # Additional brms-specific validations for time series
  # Only validate temporal structure for trends that require it
  if (trend_model %in% c("RW", "AR", "VAR")) {
    check_regular_time_intervals(data, silent = silent)
  }

  if (silent < 2) {
    message("Time series structure validated for trend model: ", trend_model)
  }

  invisible(TRUE)
}

#'Argument validation functions
#'@param data Data to be validated (list or data.frame)
#'@noRd
validate_series_time = function(
    data,
    name = 'data',
    trend_model,
    check_levels = TRUE,
    check_times = TRUE
) {
  # Extract variable names from the mvgam_trend object
  time_var <- trend_model$time
  series_var <- trend_model$series

  # Now we only need the character trend_model string
  trend_model_type <- trend_model$trend_model

  # Validate any grouping structure
  data <- validate_grouping_structure(
    data = data,
    trend_model = trend_model,
    name = name
  )

  # brms only accepts data.frames, so validate that first
  checkmate::assert_data_frame(data, .var.name = name)

  # Ungroup any grouped data
  data %>%
    dplyr::ungroup() -> data

  # Check that series variable exists and is a factor
  if (!series_var %in% colnames(data)) {
    stop(glue::glue("{name} does not contain a '{series_var}' variable"), call. = FALSE)
  }

  if (!is.factor(data[[series_var]])) {
    stop(insight::format_error(
      "Variable '{series_var}' must be a factor.",
      "Convert to factor using: data${series_var} <- factor(data${series_var})"
    ), call. = FALSE)
  }

  # Check for unused factor levels in series variable
  data <- validate_factor_levels(data, series_var, name, auto_drop = FALSE)

  # Check that time variable exists
  if (!time_var %in% colnames(data)) {
    stop(glue::glue("{name} does not contain a '{time_var}' variable"), call. = FALSE)
  }

  # Series factor must have all unique levels present if this is a
  # forecast check
  if (check_levels) {
    if (!all(levels(data[[series_var]]) %in% unique(data[[series_var]]))) {
      stop(
        glue::glue(
          'Mismatch between factor levels of "{series_var}" and unique values of "{series_var}"\n',
          'Use\n  `setdiff(levels(data${series_var}), unique(data${series_var}))` \nand\n',
          '  `intersect(levels(data${series_var}), unique(data${series_var}))`\nfor guidance'
        ),
        call. = FALSE
      )
    }
  }

  # Ensure each series has an observation, even if NA, for each
  # unique timepoint (only for trend models that require discrete time with
  # regularly spaced sampling intervals)
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
        all_there = all_times_avail(time, min_time, max_time)
      ) -> checked_times
    if (any(checked_times$all_there == FALSE)) {
      stop(
        "One or more series in ",
        name,
        " is missing observations for one or more timepoints",
        call. = FALSE
      )
    }
  }

  return(data)
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

#' Validate Complete Grouping Structure
#'
#' @description
#' For hierarchical models, checks that each level of the grouping variable
#' contains observations for all levels of the subgrouping variable.
#'
#' @param data Data frame
#' @param gr_var Name of the grouping variable
#' @param subgr_var Name of the subgrouping variable
#' @param data_name Name of the data object (for error messages)
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_complete_grouping <- function(data, gr_var, subgr_var, data_name = "data") {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(gr_var)
  checkmate::assert_string(subgr_var)

  # Check that each level of gr contains all possible levels of subgr
  grouping_summary <- data %>%
    dplyr::group_by(!!rlang::sym(gr_var)) %>%
    dplyr::summarise(
      n_subgroups = dplyr::n_distinct(!!rlang::sym(subgr_var)),
      .groups = "drop"
    )

  total_subgroups <- dplyr::n_distinct(data[[subgr_var]])

  if (any(grouping_summary$n_subgroups != total_subgroups)) {
    stop(insight::format_error(
      "Incomplete grouping structure in {data_name}.",
      "Some levels of '{gr_var}' do not contain all levels of '{subgr_var}'.",
      "Each group must contain observations for all subgroups."
    ), call. = FALSE)
  }

  invisible(TRUE)
}

#' Check Regular Time Intervals
#'
#' @description
#' Ensures time series has regular sampling intervals for State-Space models.
#'
#' @param data Data to check
#' @param silent Verbosity level
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
check_regular_time_intervals <- function(data, silent = 1) {
  if (!"time" %in% names(data)) {
    return(invisible(TRUE))  # Will be caught by main validation
  }

  # Check for regular intervals within each series
  if ("series" %in% names(data)) {
    data %>%
      dplyr::group_by(series) %>%
      dplyr::summarise(
        time_diffs = list(diff(sort(unique(time)))),
        regular = length(unique(diff(sort(unique(time))))) <= 1,
        .groups = "drop"
      ) -> time_checks

    irregular_series <- time_checks$series[!time_checks$regular]

    if (length(irregular_series) > 0) {
      stop(insight::format_error(
        "Irregular time intervals detected in series: {paste(irregular_series, collapse = ', ')}",
        "State-Space models require regular sampling intervals.",
        "Consider using CAR models for irregular time series."
      ))
    }
  } else {
    # Single series case
    time_diffs <- diff(sort(unique(data$time)))
    if (length(unique(time_diffs)) > 1) {
      stop(insight::format_error(
        "Irregular time intervals detected in time series.",
        "State-Space models require regular sampling intervals.",
        "Time differences: {paste(unique(time_diffs), collapse = ', ')}"
      ))
    }
  }

  invisible(TRUE)
}

#' Validate Combined Stan Data
#'
#' @description
#' Validates that combined Stan data is consistent and contains required elements.
#'
#' @param combined_data List of combined Stan data
#' @param obs_data List of observation Stan data
#' @param trend_data List of trend Stan data
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_combined_standata <- function(combined_data, obs_data, trend_data) {
  checkmate::assert_list(combined_data, names = "named")
  checkmate::assert_list(obs_data, names = "named")
  checkmate::assert_list(trend_data, names = "named")

  # Check that essential observation data elements are preserved
  essential_obs <- c("N", "K", "y", "X")
  missing_obs <- setdiff(essential_obs, names(combined_data))
  missing_obs <- missing_obs[missing_obs %in% names(obs_data)]

  if (length(missing_obs) > 0) {
    stop(insight::format_error(
      "Essential observation data elements missing from combined data:",
      paste(missing_obs, collapse = ", ")
    ))
  }

  # Check for dimensional consistency
  if ("N" %in% names(combined_data) && "n" %in% names(combined_data)) {
    if (combined_data$N != combined_data$n) {
      insight::format_warning(
        "Potential dimension mismatch between N ({combined_data$N}) and n ({combined_data$n}).",
        "Check observation and trend data compatibility."
      )
    }
  }

  # Check that trend data makes sense
  if ("n_series" %in% names(combined_data)) {
    if (combined_data$n_series < 1) {
      stop(insight::format_error(
        "Invalid number of series: {combined_data$n_series}",
        "Must be at least 1 for mvgam models."
      ))
    }
  }

  invisible(TRUE)
}

#'@noRd
deparse_variable = function(...) {
  deparse0(substitute(...))
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

#'@noRd
as_one_integer <- function(x, allow_na = FALSE) {
  s <- substitute(x)
  x <- suppressWarnings(as.integer(x))
  if (length(x) != 1L || anyNA(x) && !allow_na) {
    s <- deparse0(s, max_char = 100L)
    stop("Cannot coerce '", s, "' to a single integer value.", call. = FALSE)
  }
  x
}

#'@noRd
deparse0 <- function(x, max_char = NULL, ...) {
  out <- collapse(deparse(x, ...))
  if (isTRUE(max_char > 0)) {
    out <- substr(out, 1L, max_char)
  }
  out
}

#'@noRd
collapse <- function(..., sep = "") {
  paste(..., sep = sep, collapse = "")
}

#'@noRd
validate_silent <- function(silent) {
  silent <- as_one_integer(silent)
  if (silent < 0 || silent > 2) {
    stop("'silent' must be between 0 and 2.", call. = FALSE)
  }
  silent
}

#'@noRd
validate_gr_subgr = function(gr, subgr, cor) {
  gr <- deparse0(substitute(gr))
  subgr <- deparse0(substitute(subgr))

  if (gr != 'NA') {
    if (subgr == 'NA') {
      stop(
        'argument "subgr" must be supplied if "gr" is also supplied',
        call. = FALSE
      )
    }
  }

  if (subgr != 'NA') {
    if (gr == 'NA') {
      stop(
        'argument "gr" must be supplied if "subgr" is also supplied',
        call. = FALSE
      )
    } else {
      cor <- TRUE
    }
  }

  list(.group = gr, .subgroup = subgr, .cor = cor)
}

#'@noRd
validate_proportional = function(x) {
  s <- substitute(x)
  x <- base::suppressWarnings(as.numeric(x))
  if (length(x) != 1L || anyNA(x)) {
    stop("Argument '", s, "' must be a single numeric value", call. = FALSE)
  }

  if (x < 0 || x > 1) {
    stop(
      "Argument '",
      s,
      "' must be a proportion ranging from 0 to 1, inclusive",
      call. = FALSE
    )
  }
}

#'@noRd
validate_equaldims = function(x, y) {
  s <- substitute(x)
  q <- substitute(y)

  if (NCOL(x) != NCOL(y)) {
    stop(
      "Argument '",
      s,
      "' and argument '",
      q,
      "' must have equal dimensions",
      call. = FALSE
    )
  }

  if (NROW(x) != NROW(y)) {
    stop(
      "Argument '",
      s,
      "' and argument '",
      q,
      "' must have equal dimensions",
      call. = FALSE
    )
  }
}

#'@noRd
validate_pos_integer = function(x) {
  s <- substitute(x)
  x <- base::suppressWarnings(as.numeric(x))
  if (length(x) != 1L || anyNA(x)) {
    stop("Argument '", s, "' must be a single numeric value", call. = FALSE)
  }

  if (sign(x) != 1) {
    stop("Argument '", s, "' must be a positive integer", call. = FALSE)
  } else {
    if (x %% 1 != 0) {
      stop("Argument '", s, "' must be a positive integer", call. = FALSE)
    }
  }
}

#'@noRd
validate_pos_integers = function(x) {
  s <- substitute(x)

  val_pos = function(y, s) {
    y <- base::suppressWarnings(as.numeric(y))
    if (sign(y) != 1) {
      stop("Negative values in ", s, " detected", call. = FALSE)
    } else {
      if (y %% 1 != 0) {
        stop("Non-integer values in ", s, " detected", call. = FALSE)
      }
    }
  }
  res <- lapply(seq_along(x), function(i) val_pos(x[i], s))
}

#'@noRd
validate_even <- function(x) {
  s <- substitute(x)
  x <- base::suppressWarnings(as.numeric(x))
  if (x %% 2 != 0) {
    stop("Argument '", s, "'  must be an even integer", call. = FALSE)
  }
}

#'@noRd
validate_pos_real = function(x) {
  s <- substitute(x)
  x <- base::suppressWarnings(as.numeric(x))
  if (length(x) != 1L || anyNA(x)) {
    stop("Argument '", s, "' must be a single numeric value", call. = FALSE)
  }

  if (sign(x) != 1) {
    stop("Argument '", s, "' must be a positive real value", call. = FALSE)
  }
}

#'@noRd
as_one_character <- function(x, allow_na = FALSE) {
  s <- substitute(x)
  x <- as.character(x)
  if (length(x) != 1L || anyNA(x) && !allow_na) {
    s <- deparse0(s, max_char = 100L)
    stop("Cannot coerce '", s, "' to a single character value.", call. = FALSE)
  }
  x
}
