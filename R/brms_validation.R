#' Formula Validation for brms Integration
#'
#' Validation functions for mvgam-brms integration that ensure proper separation
#' between observation formulas (handled by brms) and trend formulas (handled by
#' mvgam State-Space dynamics). Supports bf() wrappers for consistency.
#'
#' Key design principle: Trends apply ONLY to the main response parameter (mu),
#' not to distributional parameters (sigma, nu, phi, etc.) to maintain
#' identifiability in State-Space models.
#'
#' Supported advanced brms patterns:
#' - Distributional regression: bf(y ~ x, sigma ~ z)
#' - Nonlinear models: bf(y ~ a * exp(b * x), a + b ~ 1, nl = TRUE)  
#' - Multivariate models: bf(mvbind(y1, y2) ~ x) + set_rescor(TRUE)
#' - Mixed patterns: combinations of above
#'
#' Multivariate trend specification: Response variables ARE allowed in trend_formula
#' when using bf() objects to identify which trend belongs to which response:
#' - trend_formula = bf(count ~ AR(p = 1), biomass ~ RW(cor = TRUE))
#' - Single formulas still should not have response variables: trend_formula = ~ RW()
#'
#' @importFrom checkmate assert_class assert
#' @importFrom insight format_error format_warning
#' @importFrom rlang warn
#' @name brms_validation
#' @keywords internal
NULL

# Null-coalescing operator helper
`%||%` <- function(x, y) if (is.null(x)) y else x

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