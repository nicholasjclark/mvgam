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
                                   trend_formula = NULL, stanvars = NULL, 
                                   ...) {
  # Accept both regular formulas and brms formula objects
  checkmate::assert(
    checkmate::check_formula(formula),
    checkmate::check_class(formula, "mvbrmsformula"),
    checkmate::check_class(formula, "bform"),
    checkmate::check_class(formula, "brmsformula"),
    combine = "or"
  )
  checkmate::assert_data_frame(data, min.rows = 1)
  if (!is.null(trend_formula)) {
    checkmate::assert(
      inherits(trend_formula, "formula") ||
      inherits(trend_formula, "brmsformula") || 
      inherits(trend_formula, "bform") ||
      is.list(trend_formula),
      .var.name = "trend_formula"
    )
  }

  # Handle trend formulas without response variables
  # Only apply this logic to regular formula objects, not brms formula objects
  if (inherits(formula, "formula") && !inherits(formula, c("brmsformula", "mvbrmsformula", "bform"))) {
    # Check if formula lacks response variable (e.g., ~ 1, ~ x + y) 
    formula_chr <- deparse(formula)
    if (!grepl("~.*~", formula_chr) && grepl("^\\s*~", formula_chr)) {
      # This is a trend formula without response variable
      # Add fake trend_y response variable following mvgam pattern
      data <- data
      data$trend_y <- rnorm(nrow(data))
      
      # Update formula to include trend_y response
      if (attr(terms(formula), 'intercept') == 1) {
        # Has intercept: keep intercept to get Intercept_trend prior
        formula <- update(formula, trend_y ~ .)
      } else {
        # No intercept: explicitly remove intercept
        formula <- update(formula, trend_y ~ . - 1)
      }
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
  
  # Parse and validate trend formula if provided
  trend_specs <- NULL
  if (!is.null(trend_formula)) {
    trend_specs <- parse_multivariate_trends(formula, trend_formula)
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
    trend_formula = trend_formula,
    data = data,
    family = family,
    stanvars = stanvars,
    stancode = brms::stancode(mock_setup),
    standata = brms::standata(mock_setup),
    prior = extract_prior_from_setup(mock_setup),
    brmsterms = extract_brmsterms_from_setup(mock_setup),
    brmsfit = mock_setup,  # Keep the mock brmsfit for prediction
    trend_specs = trend_specs,  # Include parsed trend specifications
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

# =============================================================================
# SECTION 2: MULTIVARIATE TRENDS PARSING
# =============================================================================
# WHY: Multivariate models require sophisticated formula parsing to handle
# response-specific trends and cross-series dependencies. This system enables
# flexible trend specifications while maintaining brms compatibility for
# multivariate response families and distributional modeling.#' @noRd
parse_multivariate_trends <- function(formula, trend_formula = NULL) {
  # Validate inputs - accept either regular formula or brms formula objects
  # Note: We don't extract underlying formulas as the helper functions
  # (is_multivariate_formula, extract_response_names) already handle all types
  checkmate::assert(
    checkmate::check_formula(formula),
    checkmate::check_class(formula, "mvbrmsformula"),
    checkmate::check_class(formula, "bform"),
    checkmate::check_class(formula, "brmsformula"),
    combine = "or"
  )
  
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
  
  # Parse response names from main formula using enhanced function
  response_names <- extract_response_names(formula)
  
  # Handle response-specific trend formulas
  if (inherits(trend_formula, "brmsformula") ||
      inherits(trend_formula, "brmsterms") || 
      inherits(trend_formula, "mvbrmsterms")) {
    
    # Extract response-specific trend specifications
    trend_specs <- extract_response_trends(trend_formula, response_names, validate_separate = TRUE)
    
    # Create base formula for brms setup
    base_formula <- create_trend_base_formula(trend_specs)
    
  } else if (is.list(trend_formula) && !is.null(names(trend_formula))) {
    # Handle response-specific trends as validated lists
    if (!is_mv_main) {
      stop(insight::format_error(
        "List {.field trend_formula} requires multivariate main formula.",
        "Use mvbind() or bf() for multiple responses."
      ))
    }
    
    # Validate response names match
    missing_responses <- setdiff(names(trend_formula), response_names)
    if (length(missing_responses) > 0) {
      stop(insight::format_error(
        paste("Unknown responses in {.field trend_formula}:", paste(missing_responses, collapse = ", ")),
        paste("Available responses:", paste(response_names, collapse = ", "))
      ))
    }
    
    # Parse each trend formula
    trend_specs <- lapply(names(trend_formula), function(resp) {
      if (is.null(trend_formula[[resp]])) return(NULL)
      parse_trend_formula(trend_formula[[resp]])$trend_model
    })
    names(trend_specs) <- names(trend_formula)
    
    # Create base formula from first non-NULL trend
    non_null_trends <- which(!sapply(trend_formula, is.null))
    if (length(non_null_trends) > 0) {
      base_formula <- trend_formula[[non_null_trends[1]]]
    } else {
      base_formula <- ~ 1  # Fallback if all trends are NULL
    }
    
  } else {
    # Single trend formula applied to all responses
    # Parse the trend formula to extract trend objects instead of storing raw formulas
    parsed_trend <- parse_trend_formula(trend_formula)
    
    trend_specs <- if (is_mv_main && !is.null(response_names)) {
      # Apply same parsed trend to all responses
      setNames(
        replicate(length(response_names), parsed_trend$trend_model, simplify = FALSE),
        response_names
      )
    } else {
      # Univariate case: return trend_model directly (no wrapper)
      parsed_trend$trend_model
    }
    
    base_formula <- parsed_trend$base_formula
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

#' Check if Formula Object is Multivariate
#'
#' @description
#' Detects multivariate formula specifications across all brms patterns:
#' mvbind(), bf() with multiple responses, mvbf(), and combined bf() objects.
#' Uses brms-compatible structural validation for robust detection.
#' 
#' Note: cbind() is NOT considered multivariate per brms standards - it creates
#' binomial trial specifications (univariate models with trials structure).
#'
#' @param formula Formula object to check. Can be formula, brmsformula, 
#'   mvbrmsformula, or bform class
#' @return Logical indicating if formula contains multiple response variables
#'
#' @details
#' Supports all major brms multivariate patterns:
#' \itemize{
#'   \item mvbrmsformula class objects (combined bf() formulas)
#'   \item brmsformula objects with additional responses (bf(y1~x, y2~z))
#'   \item formula objects with mvbind() response binding
#'   \item All patterns validated using brms-compatible structure checks
#' }
#'
#' @examples
#' \dontrun{
#' # Pattern 1: mvbind formula (multivariate)
#' is_multivariate_formula(mvbind(y1, y2) ~ x)  # TRUE
#' 
#' # Pattern 2: bf with multiple responses (multivariate)
#' is_multivariate_formula(bf(count ~ temp, biomass ~ precip))  # TRUE
#' 
#' # Pattern 3: Combined bf objects (multivariate)
#' is_multivariate_formula(bf(y1 ~ x, family = poisson()) + bf(y2 ~ x))  # TRUE
#' 
#' # Pattern 4: cbind formula (NOT multivariate - binomial trials)
#' is_multivariate_formula(cbind(success, failure) ~ x)  # FALSE
#' }
#'
#' @seealso \code{\link{extract_response_names}}, \code{\link{parse_multivariate_trends}}
#' @noRd
is_multivariate_formula <- function(formula) {
  # Parameter validation - support all brms formula types
  checkmate::assert(
    inherits(formula, c("formula", "brmsformula", "mvbrmsformula", "bform")),
    .var.name = "formula"
  )

  # Case 1: mvbrmsformula class - always multivariate (brms standard)
  # This covers: bf() + bf() combinations and mvbf() objects
  if (inherits(formula, "mvbrmsformula")) {
    return(TRUE)
  }
  
  # Case 2: brmsformula with additional responses (brms bf() pattern)
  # This covers: bf(y1 ~ x, y2 ~ z) with pforms field
  if (inherits(formula, "brmsformula")) {
    return(!is.null(formula$pforms) && length(formula$pforms) > 0)
  }
  
  # Case 3: Standard formula with mvbind binding ONLY (corrected)
  # This covers: mvbind(y1, y2) ~ x (cbind REMOVED - not multivariate per brms)
  if (inherits(formula, "formula")) {
    return(has_mvbind_response(formula))
  }
  
  # Default case: univariate
  return(FALSE)
}

#' Check for mvbind Response in Formula
#'
#' @description
#' Helper function that checks formula response side for mvbind() binding
#' using robust expression parsing. cbind() is explicitly excluded as it
#' creates binomial trial specifications, not multivariate models.
#'
#' @param formula Formula object to check
#' @return Logical indicating presence of mvbind response binding
#'
#' @details
#' Uses safe expression parsing rather than fragile regex patterns.
#' Validates formula structure and ensures mvbind() is well-formed.
#'
#' @noRd
has_mvbind_response <- function(formula) {
  checkmate::assert_formula(formula)
  
  # Validate formula has response side
  if (length(formula) < 3) {
    return(FALSE)
  }
  
  # Get response expression (left side of ~)
  response_expr <- formula[[2]]
  
  # Check if response expression is a call to mvbind
  if (!is.call(response_expr)) {
    return(FALSE)
  }
  
  # Extract function name from call
  call_name <- as.character(response_expr[[1]])
  
  # Check if call is to mvbind (not cbind)
  if (call_name != "mvbind") {
    return(FALSE)
  }
  
  # Validate mvbind has arguments (at least 2 responses for multivariate)
  if (length(response_expr) < 3) {
    stop(insight::format_error(
      "Invalid mvbind() specification in formula.",
      "mvbind() requires at least 2 response variables for multivariate models.",
      "Ensure syntax: mvbind(y1, y2, ...) ~ predictors"
    ), call. = FALSE)
  }
  
  return(TRUE)
}

#' Extract Response Names from Any Formula
#'
#' @description
#' Extracts response variable names from both univariate and multivariate brms
#' formula objects. Handles all major multivariate patterns plus univariate cases
#' with fail-fast error handling. Never returns NULL - always succeeds or fails.
#' 
#' Note: cbind() responses are NOT extracted as cbind() creates binomial trial
#' specifications, not true multivariate models per brms standards.
#'
#' @param formula Formula object. Can be formula, brmsformula, mvbrmsformula, or bform
#' @return Character vector of response variable names (never NULL/empty)
#'
#' @details
#' Handles all brms formula patterns with fail-fast behavior:
#' \itemize{
#'   \item mvbrmsformula objects: extracts from $responses field
#'   \item brmsformula with pforms: combines main response with additional responses
#'   \item formula with mvbind(): parses expression tree safely
#'   \item formula univariate: extracts single response using all.vars()
#'   \item Fail-fast errors: throws informative errors instead of returning NULL
#' }
#'
#' @examples
#' \dontrun{
#' # Multivariate patterns
#' extract_response_names(mvbind(y1, y2) ~ x)  # c("y1", "y2")
#' extract_response_names(bf(count ~ temp, biomass ~ precip))  # c("count", "biomass")
#' extract_response_names(bf(y1 ~ x) + bf(y2 ~ z))  # c("y1", "y2")
#' 
#' # Univariate patterns (NEW - no longer returns NULL)
#' extract_response_names(y ~ x)  # "y"
#' extract_response_names(count ~ temp + precip)  # "count"
#' 
#' # Error cases (fail-fast behavior)
#' extract_response_names(~ x)  # ERROR: no response variable
#' }
#'
#' @seealso \code{\link{is_multivariate_formula}}, \code{\link{parse_multivariate_trends}}
#' @noRd
extract_response_names <- function(formula) {
  # Parameter validation - support all brms formula types
  checkmate::assert(
    inherits(formula, c("formula", "brmsformula", "mvbrmsformula", "bform")),
    .var.name = "formula"
  )

  # Case 1: mvbrmsformula - use $responses field directly (brms standard)
  # This covers: bf() + bf() combinations and mvbf() objects
  if (inherits(formula, "mvbrmsformula")) {
    if (!is.null(formula$responses)) {
      return(formula$responses)
    }
  }
  
  # Case 2: brmsformula with pforms - combine main + additional responses
  # This covers: bf(y1 ~ x, y2 ~ z) pattern
  if (inherits(formula, "brmsformula")) {
    if (!is.null(formula$pforms) && length(formula$pforms) > 0) {
      main_resp <- formula$resp
      additional_resp <- names(formula$pforms)
      return(c(main_resp, additional_resp))
    }
  }
  
  # Case 3: Standard formula with mvbind binding ONLY (corrected)
  # This covers: mvbind(y1, y2) ~ x (cbind EXCLUDED - not multivariate per brms)
  if (inherits(formula, "formula")) {
    mvbind_result <- extract_mvbind_responses(formula)
    if (!is.null(mvbind_result)) {
      return(mvbind_result)
    }
    
    # Handle univariate formula case - extract single response with fail-fast
    if (length(formula) < 3) {
      stop(insight::format_error(
        "Formula has no response variable (left-hand side).",
        "Provide a formula with the form {.code response ~ predictors}."
      ), call. = FALSE)
    }
    
    response_terms <- all.vars(formula[[2]])
    if (length(response_terms) == 0) {
      stop(insight::format_error(
        "Could not extract response variable from formula left-hand side.",
        "Ensure the response variable is a valid R variable name."
      ), call. = FALSE)
    }
    
    return(response_terms)
  }
  
  # Handle brmsformula objects that aren't multivariate
  if (inherits(formula, "brmsformula")) {
    if (!is.null(formula$resp) && nchar(formula$resp) > 0) {
      return(formula$resp)
    }
    
    # Extract from the formula component if resp field is missing  
    if (!is.null(formula$formula)) {
      return(extract_response_names(formula$formula))
    }
  }
  
  # Should not reach here with proper validation, but fail fast if we do
  stop(insight::format_error(
    "Could not extract response variable names from formula.",
    "Formula type {.cls {class(formula)}} may not be supported.",
    "Supported types: formula, brmsformula, mvbrmsformula, bform."
  ), call. = FALSE)
}

#' Extract Response Names from mvbind Expression
#'
#' @description
#' Helper function that extracts response variable names from mvbind()
#' expressions using safe expression parsing. Handles complex expressions
#' and transformations robustly.
#'
#' @param formula Formula object with potential mvbind response
#' @return Character vector of response names, or NULL if no mvbind found
#'
#' @details
#' Uses expression tree parsing instead of regex for robust handling of:
#' - Simple variables: mvbind(y1, y2)
#' - Transformed variables: mvbind(log(y1), sqrt(y2))  
#' - Complex expressions: mvbind(y1 + offset, scale(y2))
#'
#' @noRd
extract_mvbind_responses <- function(formula) {
  checkmate::assert_formula(formula)
  
  # Validate formula has response side
  if (length(formula) < 3) {
    return(NULL)
  }
  
  # Get response expression (left side of ~)
  response_expr <- formula[[2]]
  
  # Check if response expression is a call to mvbind
  if (!is.call(response_expr) || as.character(response_expr[[1]])[1] != "mvbind") {
    return(NULL)
  }
  
  # Extract arguments from mvbind call (skip the function name)
  response_args <- response_expr[-1]
  
  # Validate we have arguments
  if (length(response_args) == 0) {
    return(NULL)
  }
  
  # Extract variable names from each argument
  response_names <- character(length(response_args))
  
  for (i in seq_along(response_args)) {
    arg <- response_args[[i]]
    
    # Extract the primary variable name from expression
    var_name <- extract_variable_name(arg)
    
    if (is.null(var_name) || nchar(var_name) == 0) {
      stop(insight::format_error(
        "Could not extract response variable name from mvbind() argument {i}.",
        "Argument: {deparse(arg)}",
        "Ensure all mvbind() arguments reference valid variable names."
      ), call. = FALSE)
    }
    
    response_names[i] <- var_name
  }
  
  # Filter out any empty names and return
  response_names <- response_names[nchar(response_names) > 0]
  
  return(if (length(response_names) > 0) response_names else NULL)
}

#' Extract Primary Variable Name from Expression
#'
#' @description
#' Extracts the primary variable name from potentially complex expressions.
#' Handles simple variables, function calls, and arithmetic operations.
#'
#' @param expr R expression object
#' @return Character string with variable name, or NULL if cannot extract
#' @noRd
extract_variable_name <- function(expr) {
  # Simple variable name
  if (is.name(expr)) {
    return(as.character(expr))
  }
  
  # Function call - extract first argument that's a variable
  if (is.call(expr)) {
    # For function calls like log(y1), sqrt(y2), extract the main argument
    for (arg in expr[-1]) {  # Skip function name
      if (is.name(arg)) {
        return(as.character(arg))
      }
      # Recursively check nested calls
      nested_var <- extract_variable_name(arg)
      if (!is.null(nested_var)) {
        return(nested_var)
      }
    }
  }
  
  # Could not extract variable name
  return(NULL)
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

# =============================================================================
# SECTION 4: NONLINEAR MODEL SUPPORT
# =============================================================================
# WHY: Nonlinear models require specialized handling to determine where trend
# effects should be injected into the parameter structure. This system enables
# mvgam trends to work with brms nonlinear modeling capabilities while
# maintaining proper identifiability and parameter interpretation.#' @noRd
handle_nonlinear_model <- function(formula, trend_specs = NULL) {
  checkmate::assert_formula(formula)
  checkmate::assert_list(trend_specs, null.ok = TRUE)
  
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
  trend_injection_point <- determine_trend_injection_point(nl_components, trend_specs)
  
  # Validate trend compatibility with nonlinear structure
  validate_nonlinear_trend_compatibility(nl_components, trend_specs)
  
  return(list(
    formula = formula,
    is_nonlinear = TRUE,
    nl_components = nl_components,
    trend_injection_point = trend_injection_point
  ))
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
#' @param trend_specs Trend specification
#' @return Character string indicating injection point
#' @noRd
determine_trend_injection_point <- function(nl_components, trend_specs) {
  checkmate::assert_list(nl_components)
  checkmate::assert_list(trend_specs, null.ok = TRUE)
  
  if (is.null(trend_specs)) {
    return("mu")  # Default injection point
  }
  
  # For nonlinear models, trends should typically affect the
  # main response parameter (often the first or most important parameter)
  if (length(nl_components$nonlinear_params) > 0) {
    # Use first nonlinear parameter as default
    main_param <- nl_components$nonlinear_params[1]
    
    # Check if trend specification indicates specific parameter
    if (!is.null(trend_specs$target_parameter)) {
      if (trend_specs$target_parameter %in% nl_components$nonlinear_params) {
        return(trend_specs$target_parameter)
      } else {
        insight::format_warning(
          "Specified trend target parameter '{trend_specs$target_parameter}' not found.",
          "Using main parameter '{main_param}' instead."
        )
      }
    }
    
    return(main_param)
  }
  
  return("mu")  # Fallback to standard linear predictor
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
#' @param trend_specs Trend specification
#' @return Character string of modified Stan code
#' @noRd
modify_stancode_for_nonlinear <- function(stancode, nl_info, trend_specs) {
  checkmate::assert_string(stancode)
  checkmate::assert_list(nl_info)
  checkmate::assert_list(trend_specs, null.ok = TRUE)
  
  if (!nl_info$is_nonlinear || is.null(trend_specs)) {
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
#' @param trend_specs Trend specification
#' @return Modified setup with nonlinear information
#' @noRd
integrate_nonlinear_with_assembly <- function(obs_setup, trend_specs) {
  checkmate::assert_list(obs_setup)
  checkmate::assert_list(trend_specs, null.ok = TRUE)
  
  # Process formula for nonlinear structure
  nl_info <- handle_nonlinear_model(obs_setup$formula, trend_specs)
  
  # Add nonlinear information to setup
  obs_setup$nonlinear_info <- nl_info
  
  # Modify trend specification if needed for nonlinear models
  if (nl_info$is_nonlinear && !is.null(trend_specs)) {
    trend_specs$target_parameter <- nl_info$trend_injection_point
    trend_specs$is_nonlinear_model <- TRUE
  }
  
  return(list(
    obs_setup = obs_setup,
    trend_specs = trend_specs
  ))
}
