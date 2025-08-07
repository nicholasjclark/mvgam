#' brms Ecosystem Integration for mvgam
#'
#' @description
#' Complete brms ecosystem integration and validation for mvgam models.
#' This file consolidates brms setup, formula validation, multivariate
#' parsing, and nonlinear model support.
#'
#' @section Architecture:
#' The brms integration system provides seamless compatibility:
#' - **Setup Layer**: Lightweight brms model setup for rapid prototyping
#' - **Validation Layer**: Comprehensive formula validation and syntax checking
#' - **Parsing Layer**: Multivariate and nonlinear formula interpretation
#' - **Extension Layer**: Enhanced functionality beyond base brms capabilities

# =============================================================================
# SECTION 1: BRMS LIGHTWEIGHT SETUP SYSTEM
# =============================================================================
# WHY: Lightweight brms setup enables rapid model prototyping and validation
# without full compilation overhead. This is essential for mvgam's two-stage
# assembly system where brms provides the foundation and mvgam adds trends
# through stanvars injection without modifying brms internals.#' @noRd
setup_brms_lightweight <- function(formula, data, family = gaussian(),
                                   stanvars = NULL, ...) {
  checkmate::assert_formula(formula)
  checkmate::assert_data_frame(data, min.rows = 1)

  # Handle trend formulas without response variables
  # Check if formula lacks response variable (e.g., ~ 1, ~ x + y) 
  formula_chr <- deparse(formula)
  if (!grepl("~.*~", formula_chr) && grepl("^\\s*~", formula_chr)) {
    # This is a trend formula without response variable
    # Add fake trend_y response variable following mvgam pattern
    data <- data
    data$trend_y <- rnorm(nrow(data))
    
    # Update formula to include trend_y response
    if (attr(terms(formula), 'intercept') == 1) {
      # Has intercept: trend_y ~ . - 1 (drop intercept for identifiability)
      formula <- update(formula, trend_y ~ . - 1)
    } else {
      # No intercept: trend_y ~ .
      formula <- update(formula, trend_y ~ .)
    }
  }

  # Validate brms formula compatibility
  formula_validation <- mvgam:::validate_brms_formula(formula)
  if (!formula_validation$valid) {
    stop(insight::format_error(
      "Invalid brms formula structure:",
      paste(formula_validation$issues, collapse = "\n")
    ))
  }

  # Use mock backend for rapid setup (creates brmsfit object needed for prediction)
  # Let brms errors bubble up naturally - no masking
  mock_setup <- brms::brm(
    formula = formula,
    data = data,
    family = family,
    stanvars = stanvars,
    backend = "mock",
    mock_fit = 1,
    rename = FALSE
  )

  # Extract key components for mvgam integration
  setup_components <- list(
    formula = formula,
    data = data,
    family = family,
    stanvars = stanvars,
    stancode = brms::stancode(mock_setup),
    standata = brms::standata(mock_setup),
    prior = extract_prior_from_setup(mock_setup),
    brmsterms = extract_brmsterms_from_setup(mock_setup),
    brmsfit = mock_setup,  # Keep the mock brmsfit for prediction
    setup_time = system.time({})[["elapsed"]] # Track performance
  )

  # Validate extracted components
  validate_setup_components(setup_components)

  return(setup_components)
}



#' Extract Prior Information from brms Setup
#' @param setup_object brms setup object
#' @return Data frame of prior specifications
#' @noRd
extract_prior_from_setup <- function(setup_object) {
  # Extract prior information - let errors bubble up
  if (!is.null(setup_object$prior)) {
    prior_info <- setup_object$prior
  } else {
    # Reconstruct from formula and data
    prior_info <- brms::get_prior(setup_object$formula, setup_object$data, setup_object$family)
  }

  return(prior_info)
}

#' Extract brms Terms from Setup
#' @param setup_object brms setup object
#' @return brmsterms object
#' @noRd
extract_brmsterms_from_setup <- function(setup_object) {
  # Extract brms terms - let errors bubble up
  if (!is.null(setup_object$formula)) {
    terms_info <- brms::brmsterms(setup_object$formula)
  } else {
    terms_info <- NULL
  }

  return(terms_info)
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

# =============================================================================
# SECTION 2: COMPREHENSIVE FORMULA VALIDATION
# =============================================================================
# WHY: Formula validation prevents runtime errors and ensures compatibility
# between mvgam extensions and brms syntax. This layer catches malformed
# formulas early and provides clear error messages, essential for user
# experience when combining observation and trend model specifications.
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

# =============================================================================
# SECTION 3: MULTIVARIATE TRENDS PARSING
# =============================================================================
# WHY: Multivariate models require sophisticated formula parsing to handle
# response-specific trends and cross-series dependencies. This system enables
# flexible trend specifications while maintaining brms compatibility for
# multivariate response families and distributional modeling.#' @noRd
parse_multivariate_trends <- function(formula, trend_formula = NULL) {
  checkmate::assert_formula(formula)
  
  # Handle missing trend formula
  if (is.null(trend_formula)) {
    return(list(
      has_trends = FALSE,
      is_multivariate = FALSE,
      response_names = NULL,
      trend_specs = NULL,
      base_formula = NULL
    ))
  }
  
  # Validate trend formula structure
  trend_validation <- validate_trend_formula_brms(trend_formula)
  
  # Check if main formula is multivariate
  is_mv_main <- is_multivariate_formula(formula)
  
  # Parse response names from main formula
  response_names <- if (is_mv_main) {
    extract_response_names(formula)
  } else {
    NULL
  }
  
  # Handle response-specific trend formulas
  if (inherits(trend_formula, "brmsformula") ||
      inherits(trend_formula, "brmsterms") || 
      inherits(trend_formula, "mvbrmsterms")) {
    
    # Extract response-specific trend specifications
    trend_specs <- extract_response_trends(trend_formula, response_names, validate_separate = TRUE)
    
    # Create base formula for brms setup
    base_formula <- create_trend_base_formula(trend_specs)
    
  } else {
    # Single trend formula applied to all responses
    trend_specs <- if (is_mv_main && !is.null(response_names)) {
      # Apply same trend to all responses
      setNames(
        replicate(length(response_names), trend_formula, simplify = FALSE),
        response_names
      )
    } else {
      list(main = trend_formula)
    }
    
    base_formula <- trend_formula
  }
  
  return(list(
    has_trends = TRUE,
    is_multivariate = is_mv_main,
    response_names = response_names,
    trend_specs = trend_specs,
    base_formula = base_formula,
    validation = trend_validation
  ))
}

#' Check if Formula is Multivariate
#' @param formula Formula to check
#' @return Logical indicating if formula contains multivariate response
#' @noRd
is_multivariate_formula <- function(formula) {
  checkmate::assert_formula(formula)
  
  # Check for mvbind() or cbind() in response
  response_terms <- as.character(formula)[2]
  grepl("mvbind\\(|cbind\\(", response_terms, perl = TRUE)
}

#' Extract Response Names from Multivariate Formula
#' @param formula Multivariate formula
#' @return Character vector of response variable names
#' @noRd
extract_response_names <- function(formula) {
  checkmate::assert_formula(formula)
  
  response_side <- as.character(formula)[2]
  
  # Extract from mvbind() or cbind()
  if (grepl("mvbind\\(", response_side)) {
    # Parse mvbind(y1, y2, y3) structure
    inner <- gsub(".*mvbind\\((.*)\\).*", "\\1", response_side)
  } else if (grepl("cbind\\(", response_side)) {
    # Parse cbind(y1, y2) structure
    inner <- gsub(".*cbind\\((.*)\\).*", "\\1", response_side)
  } else {
    return(NULL)
  }
  
  # Split by commas and clean whitespace
  responses <- trimws(strsplit(inner, ",")[[1]])
  
  # Remove any function calls or transformations
  responses <- gsub("\\s*\\(.*\\)\\s*", "", responses)
  responses <- gsub("\\s+", "", responses)
  
  return(responses)
}

#' Extract Response-Specific Trend Specifications
#' @param trend_formula brms formula object with response-specific trends
#' @param response_names Character vector of response names
#' @return Named list of trend specifications per response
#' @noRd
extract_response_trends <- function(trend_formula, response_names, validate_separate = FALSE) {
  checkmate::assert_character(response_names, null.ok = TRUE)
  checkmate::assert_logical(validate_separate, len = 1)
  
  # Check if trend_formula is already processed brms terms
  if (inherits(trend_formula, c("brmsterms", "mvbrmsterms"))) {
    trend_terms <- trend_formula
  } else {
    # Use brms internal functions to parse the formula structure
    trend_terms <- try(brms::brmsterms(trend_formula), silent = TRUE)
    
    if (inherits(trend_terms, "try-error")) {
      stop(insight::format_error(
        "Could not parse trend_formula structure.",
        "Ensure proper bf() syntax for response-specific trends."
      ))
    }
  }
  
  # Extract terms for each response
  trend_specs <- list()
  
  if (inherits(trend_terms, "mvbrmsterms")) {
    # Multivariate terms - extract each response
    for (i in seq_along(trend_terms$terms)) {
      resp_name <- names(trend_terms$terms)[i]
      if (is.null(resp_name) && i <= length(response_names)) {
        resp_name <- response_names[i]
      }
      
      if (!is.null(resp_name)) {
        resp_formula <- trend_terms$terms[[i]]$formula
        # Validate multivariate trends don't use advanced features (only for separate response trends)
        if (validate_separate) {
          validate_multivariate_trend_constraints(resp_formula, resp_name)
        }
        trend_specs[[resp_name]] <- resp_formula
      }
    }
  } else {
    # Single response trend - apply to main or first response
    resp_name <- if (!is.null(response_names)) response_names[1] else "main"
    trend_specs[[resp_name]] <- trend_terms$formula
  }
  
  return(trend_specs)
}

#' Create Base Formula for Trend Setup
#' @param trend_specs Named list of trend specifications
#' @return Formula object suitable for brms setup
#' @noRd
create_trend_base_formula <- function(trend_specs) {
  checkmate::assert_list(trend_specs, min.len = 1)
  
  # Use the first trend specification as base
  base_spec <- trend_specs[[1]]
  
  if (inherits(base_spec, "formula")) {
    return(base_spec)
  } else if (inherits(base_spec, "brmsterms")) {
    return(base_spec$formula)
  } else {
    # Fallback: create minimal trend formula
    return(~ 1)
  }
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

# =============================================================================
# SECTION 4: NONLINEAR MODEL SUPPORT
# =============================================================================
# WHY: Nonlinear models require specialized handling to determine where trend
# effects should be injected into the parameter structure. This system enables
# mvgam trends to work with brms nonlinear modeling capabilities while
# maintaining proper identifiability and parameter interpretation.#' @noRd
handle_nonlinear_model <- function(formula, trend_spec = NULL) {
  checkmate::assert_formula(formula)
  checkmate::assert_list(trend_spec, null.ok = TRUE)
  
  # Check if this is a nonlinear model
  is_nonlinear <- is_nonlinear_formula(formula)
  
  if (!is_nonlinear) {
    # Standard linear model, return as-is
    return(list(
      formula = formula,
      is_nonlinear = FALSE,
      nl_components = NULL,
      trend_injection_point = "mu"
    ))
  }
  
  # Process nonlinear formula structure
  nl_components <- extract_nonlinear_components(formula)
  
  # Determine where trends should be injected
  trend_injection_point <- determine_trend_injection_point(nl_components, trend_spec)
  
  # Validate trend compatibility with nonlinear structure
  validate_nonlinear_trend_compatibility(nl_components, trend_spec)
  
  return(list(
    formula = formula,
    is_nonlinear = TRUE,
    nl_components = nl_components,
    trend_injection_point = trend_injection_point
  ))
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

#' Extract Nonlinear Components
#' 
#' @description
#' Extracts components from nonlinear brms formula for processing.
#' 
#' @param formula Nonlinear brms formula
#' @return List of nonlinear model components
#' @noRd
extract_nonlinear_components <- function(formula) {
  checkmate::assert_formula(formula)
  
  # Use brms internal functions if available
  if (requireNamespace("brms", quietly = TRUE)) {
    tryCatch({
      # Try to parse with brms
      bf_terms <- brms::brmsterms(formula)
      
      return(list(
        response = bf_terms$respform,
        predictors = bf_terms$pforms,
        nonlinear_params = names(bf_terms$nlpars),
        family = bf_terms$family
      ))
    }, error = function(e) {
      # Fallback to manual parsing
      return(parse_nonlinear_manually(formula))
    })
  } else {
    return(parse_nonlinear_manually(formula))
  }
}

#' Parse Nonlinear Formula Manually
#' 
#' @description
#' Manual parsing of nonlinear formula when brms functions unavailable.
#' 
#' @param formula Nonlinear formula to parse
#' @return List of parsed components
#' @noRd
parse_nonlinear_manually <- function(formula) {
  formula_str <- deparse(formula, wide.cutoff = 500)
  
  # Basic parsing - would need more sophisticated implementation
  # for production use
  components <- list(
    response = extract_response_from_formula(formula),
    predictors = list(),
    nonlinear_params = character(0),
    family = NULL
  )
  
  # Look for parameter specifications like "a ~ 1", "b ~ x"
  param_matches <- gregexpr("\\b[a-zA-Z_][a-zA-Z0-9_]*\\s*~[^,)]+", formula_str)
  if (param_matches[[1]][1] != -1) {
    param_specs <- regmatches(formula_str, param_matches)[[1]]
    
    for (spec in param_specs) {
      param_name <- sub("\\s*~.*", "", spec)
      param_name <- trimws(param_name)
      components$nonlinear_params <- c(components$nonlinear_params, param_name)
      components$predictors[[param_name]] <- spec
    }
  }
  
  return(components)
}

#' Determine Trend Injection Point
#' 
#' @description
#' Determines where trend effects should be injected in nonlinear model.
#' Trends typically affect the main response parameter.
#' 
#' @param nl_components List of nonlinear components
#' @param trend_spec Trend specification
#' @return Character string indicating injection point
#' @noRd
determine_trend_injection_point <- function(nl_components, trend_spec) {
  checkmate::assert_list(nl_components)
  checkmate::assert_list(trend_spec, null.ok = TRUE)
  
  if (is.null(trend_spec)) {
    return("mu")  # Default injection point
  }
  
  # For nonlinear models, trends should typically affect the
  # main response parameter (often the first or most important parameter)
  if (length(nl_components$nonlinear_params) > 0) {
    # Use first nonlinear parameter as default
    main_param <- nl_components$nonlinear_params[1]
    
    # Check if trend specification indicates specific parameter
    if (!is.null(trend_spec$target_parameter)) {
      if (trend_spec$target_parameter %in% nl_components$nonlinear_params) {
        return(trend_spec$target_parameter)
      } else {
        insight::format_warning(
          "Specified trend target parameter '{trend_spec$target_parameter}' not found.",
          "Using main parameter '{main_param}' instead."
        )
      }
    }
    
    return(main_param)
  }
  
  return("mu")  # Fallback to standard linear predictor
}

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

#' Extract Response from Formula
#' 
#' @description
#' Helper function to extract response variable from formula.
#' 
#' @param formula Formula object
#' @return Character string of response variable
#' @noRd
extract_response_from_formula <- function(formula) {
  if (length(formula) >= 2) {
    return(deparse(formula[[2]]))
  }
  return(NULL)
}

#' Modify Stan Code for Nonlinear Models
#' 
#' @description
#' Modifies Stan code generation for nonlinear models with trends.
#' Ensures trends are injected at the correct point in nonlinear predictors.
#' 
#' @param stancode Character string of base Stan code
#' @param nl_info List containing nonlinear model information
#' @param trend_spec Trend specification
#' @return Character string of modified Stan code
#' @noRd
modify_stancode_for_nonlinear <- function(stancode, nl_info, trend_spec) {
  checkmate::assert_string(stancode)
  checkmate::assert_list(nl_info)
  checkmate::assert_list(trend_spec, null.ok = TRUE)
  
  if (!nl_info$is_nonlinear || is.null(trend_spec)) {
    return(stancode)
  }
  
  # Split Stan code into lines
  code_lines <- strsplit(stancode, "\n")[[1]]
  
  # Find where the nonlinear parameter is defined
  target_param <- nl_info$trend_injection_point
  param_pattern <- paste0("\\b", target_param, "\\s*=")
  param_lines <- grep(param_pattern, code_lines)
  
  if (length(param_lines) == 0) {
    insight::format_warning(
      "Could not find nonlinear parameter '{target_param}' in Stan code.",
      "Trend injection may not work correctly for nonlinear model."
    )
    return(stancode)
  }
  
  # Generate trend injection template for nonlinear parameter
  trend_injection <- glue::glue("
    // mvgam nonlinear trend injection for parameter {target_param}
    {target_param}_trend = trend_effects_for_{target_param};
    {target_param} = {target_param}_base + {target_param}_trend;
  ")
  
  # Insert trend injection after parameter definition
  insert_position <- param_lines[1] + 1
  
  modified_lines <- c(
    code_lines[1:(insert_position - 1)],
    strsplit(trend_injection, "\n")[[1]],
    code_lines[insert_position:length(code_lines)]
  )
  
  return(paste(modified_lines, collapse = "\n"))
}

#' Integration with Stan Assembly System
#' 
#' @description
#' Integrates nonlinear model support with the main Stan assembly system.
#' 
#' @param obs_setup Observation model setup
#' @param trend_spec Trend specification
#' @return Modified setup with nonlinear information
#' @noRd
integrate_nonlinear_with_assembly <- function(obs_setup, trend_spec) {
  checkmate::assert_list(obs_setup)
  checkmate::assert_list(trend_spec, null.ok = TRUE)
  
  # Process formula for nonlinear structure
  nl_info <- handle_nonlinear_model(obs_setup$formula, trend_spec)
  
  # Add nonlinear information to setup
  obs_setup$nonlinear_info <- nl_info
  
  # Modify trend specification if needed for nonlinear models
  if (nl_info$is_nonlinear && !is.null(trend_spec)) {
    trend_spec$target_parameter <- nl_info$trend_injection_point
    trend_spec$is_nonlinear_model <- TRUE
  }
  
  return(list(
    obs_setup = obs_setup,
    trend_spec = trend_spec
  ))
}