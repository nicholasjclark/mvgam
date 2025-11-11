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
# HELPER FUNCTIONS
# =============================================================================

#' Determine if trend formula should include intercept based on mvgam conventions
#' 
#' @description
#' Analyzes trend formulas to determine intercept inclusion following mvgam 
#' conventions rather than R's default formula behavior. Trend constructors
#' like RW() and AR() default to no intercept unless explicitly specified.
#' 
#' @param formula A formula object for trend specification
#' @return Logical indicating whether intercept should be included
#' @noRd
should_trend_formula_have_intercept <- function(formula) {
  # Input validation following mvgam standards
  checkmate::assert_formula(formula)
  
  # Get formula terms for analysis
  formula_terms <- terms(formula)
  
  # Check R's built-in intercept attribute
  has_r_intercept <- attr(formula_terms, 'intercept') == 1
  
  if (!has_r_intercept) {
    # Explicitly excluded via ~ -1 or ~ 0 
    return(FALSE)
  }
  
  # For formulas with R intercept = 1, check if intercept was explicit
  # Parse right-hand side of formula to detect explicit "+1" vs default behavior
  rhs_terms <- attr(formula_terms, "term.labels")
  
  # If no terms on RHS beyond intercept, this is ~ 1 (explicit intercept only)
  if (length(rhs_terms) == 0) {
    return(TRUE)
  }
  
  # Check if "1" is explicitly listed as a term (~ 1 + RW())
  if ("1" %in% rhs_terms) {
    return(TRUE)
  }
  
  # For pure trend constructor formulas (~ RW(), ~ AR()) with default R intercept,
  # follow mvgam convention of no intercept for trend innovations
  return(FALSE)
}

# =============================================================================
# SECTION 1: BRMS LIGHTWEIGHT SETUP SYSTEM
# =============================================================================
# WHY: Lightweight brms setup enables rapid model prototyping and validation
# without full compilation overhead. This is essential for mvgam's two-stage
# assembly system where brms provides the foundation and mvgam adds trends
# through stanvars injection without modifying brms internals.
#' Lightweight brms Setup with Prior Support
#'
#' @param formula Formula or brms formula object for model specification
#' @param data Data frame containing model variables
#' @param family Response distribution family (default: gaussian())
#' @param trend_formula Optional trend formula specification (default: NULL)
#' @param stanvars Optional brms stanvars object (default: NULL)
#' @param prior A brmsprior object or NULL. Prior specifications
#'   for model parameters. Defaults to NULL.
#' @param is_trend_setup Logical. If TRUE, validates trend covariates and
#'   reduces data to one row per (time, series) combination. Default: FALSE.
#' @param response_vars Character vector of response variable names for 
#'   trend validation. Required when is_trend_setup = TRUE.
#' @param time_var Character name of time variable. Default: "time".
#' @param series_var Character name of series variable. Default: "series".
#' @param ... Additional arguments passed to brms functions
#' @noRd
setup_brms_lightweight <- function(formula, data, family = gaussian(),
                                   trend_formula = NULL, stanvars = NULL,
                                   prior = NULL,
                                   is_trend_setup = FALSE,
                                   response_vars = NULL,
                                   time_var = "time", 
                                   series_var = "series",
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
  checkmate::assert(
    checkmate::check_null(prior), 
    checkmate::check_class(prior, "brmsprior"),
    combine = "or"
  )
  # Validation for new parameters
  checkmate::assert_logical(is_trend_setup, len = 1)
  checkmate::assert_character(response_vars, null.ok = TRUE)
  checkmate::assert_string(time_var)
  checkmate::assert_string(series_var)
  if (!is.null(trend_formula)) {
    checkmate::assert(
      inherits(trend_formula, "formula") ||
      inherits(trend_formula, "brmsformula") ||
      inherits(trend_formula, "bform") ||
      is.list(trend_formula),
      .var.name = "trend_formula"
    )
  }

  # Trend context handling - validate and reduce data if this is trend setup
  if (is_trend_setup && !is.null(trend_formula)) {
    # Use consolidated validation and data extraction with metadata capture
    result <- extract_trend_data(data, trend_formula, time_var, series_var,
                                response_vars = response_vars, .return_metadata = TRUE)
    data <- result$trend_data
    trend_metadata <- result$metadata
    
    # Use trend formula as main formula for brms processing
    formula <- trend_formula
  }

  # Handle trend formulas without response variables
  # Only apply this logic to regular formula objects, not brms formula objects
  if (inherits(formula, "formula") && !inherits(formula, c("brmsformula", "mvbrmsformula", "bform"))) {
    # Check if formula lacks response variable (e.g., ~ 1, ~ x + y)
    formula_chr <- deparse(formula)
    if (!grepl("~.*~", formula_chr) && grepl("^\\s*~", formula_chr)) {
      # DEBUG: Add debug output to trace execution
      cat("DEBUG: Processing trend formula:", formula_chr, "\n")
      
      # This is a trend formula without response variable
      # Add fake trend_y response variable following mvgam pattern
      data <- data
      data$trend_y <- rnorm(nrow(data))

      # Update formula to include trend_y response
      # Use mvgam-aware intercept detection for trend formulas
      has_intercept_check <- should_trend_formula_have_intercept(formula)
      cat("DEBUG: should_have_intercept =", has_intercept_check, "\n")
      
      if (has_intercept_check) {
        # Has intercept: keep intercept to get Intercept_trend prior
        cat("DEBUG: Keeping intercept - using trend_y ~ .\n")
        formula <- update(formula, trend_y ~ .)
      } else {
        # No intercept: explicitly remove intercept
        cat("DEBUG: Removing intercept - using trend_y ~ . - 1\n")
        formula <- update(formula, trend_y ~ . - 1)
      }
      
      cat("DEBUG: Final formula:", deparse(formula), "\n")
    } else {
      cat("DEBUG: Skipping trend formula processing - not a simple trend formula\n")
    }
  } else {
    cat("DEBUG: Skipping trend formula processing - wrong formula type\n")
  }

  # Validate brms formula compatibility
  formula_validation <- validate_brms_formula(formula)
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
  mock_setup <- brms::brm(
    formula = formula,
    data = data,
    family = family,
    stanvars = stanvars,
    prior = prior,
    backend = "mock",
    mock_fit = 1,
    rename = FALSE
  )

  # Add version metadata to prevent restructure() from calling update()
  # standata() and prepare_predictions() call restructure() which checks version
  # and attempts update() if version is NULL or < "1.0". For multivariate models,
  # update() throws an error. Adding current brms version prevents this.
  mock_setup$version <- list(brms = utils::packageVersion("brms"))

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
    trend_metadata = if (exists("trend_metadata")) trend_metadata else NULL,  # Include trend metadata if available
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
# multivariate response families and distributional modeling.
#' @noRd
parse_multivariate_trends <- function(formula, trend_formula = NULL) {
  # Validate inputs - accept either regular formula or brms formula objects
  checkmate::assert(
    checkmate::check_formula(formula),
    checkmate::check_class(formula, "mvbrmsformula"),
    checkmate::check_class(formula, "bform"),
    checkmate::check_class(formula, "brmsformula"),
    combine = "or"
  )

  # Cache formula metadata for efficient validation filtering (DRY principle)
  formula <- cache_formula_latent_params(formula)
  if (!is.null(trend_formula)) {
    trend_formula <- cache_formula_latent_params(trend_formula)
  }

  # Handle missing trend formula
  if (is.null(trend_formula)) {
    return(list(
      has_trends = FALSE,
      is_multivariate = FALSE,
      response_names = NULL,
      trend_specs = NULL,
      base_formula = NULL,
      cached_formulas = list(
        formula = formula,
        trend_formula = NULL
      )
    ))
  }

  # Validate trend formula structure
  trend_validation <- validate_trend_formula_brms(trend_formula)

  # Check if main formula is multivariate
  is_mv_main <- is_multivariate_formula(formula)

  # Parse response names from main formula using enhanced function
  response_names <- extract_response_names(formula)
  
  if (Sys.getenv("MVGAM_DEBUG") == "TRUE") {
    cat("\n[DEBUG] parse_multivariate_trends:\n")
    cat("  - extracted response_names:", if(!is.null(response_names)) paste(response_names, collapse=", ") else "NULL", "\n")
  }

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
    validation = trend_validation,
    cached_formulas = list(
      formula = formula,
      trend_formula = trend_formula
    )
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
    # For nonlinear formulas, pforms contain parameter definitions, not responses
    if (is_nonlinear_formula(formula)) {
      return(FALSE)
    }

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
#' # Univariate patterns
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
  checkmate::assert_multi_class(formula, c("formula", "brmsformula", "mvbrmsformula", "bform"))

  # Case 1: mvbrmsformula - use $responses field directly (brms standard)
  # This covers: bf() + bf() combinations and mvbf() objects
  if (inherits(formula, "mvbrmsformula")) {
    if (!is.null(formula$responses)) {
      return(formula$responses)
    }
  }

  # Case 2: brmsformula - handle both regular and nonlinear formulas
  if (inherits(formula, "brmsformula")) {
    # Check for nonlinear formula using existing detection
    if (!is.null(formula$pforms) && length(formula$pforms) > 0) {
      # For nonlinear formulas, pforms contain parameter definitions, not responses
      # Return only the main response variable
      if (is.null(formula$resp)) {
        stop(insight::format_error(
          "Nonlinear formula missing response variable.",
          "Ensure the formula has a valid response on the left-hand side."
        ), call. = FALSE)
      }
      return(formula$resp)
    }
    # Regular brmsformula - return response
    if (is.null(formula$resp)) {
      stop(insight::format_error(
        "brmsformula missing response variable.",
        "Ensure the formula has a valid response specification."
      ), call. = FALSE)
    }
    return(formula$resp)
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

  # Check if response expression is a call to mvbind (handle namespaced calls)
  if (!is.call(response_expr)) {
    return(NULL)
  }

  # Extract function name, handling namespaced calls like mvgam::mvbind
  call_name <- if (is.name(response_expr[[1]])) {
    as.character(response_expr[[1]])
  } else if (is.call(response_expr[[1]]) && identical(response_expr[[1]][[1]], quote(`::`))) {
    as.character(response_expr[[1]][[3]])
  } else {
    ""
  }

  if (call_name != "mvbind") {
    return(NULL)
  }

  # Extract arguments from mvbind call (skip the function name)
  response_args <- response_expr[-1]

  # Validate we have arguments
  if (length(response_args) == 0) {
    stop(insight::format_error(
      "mvbind() call contains no arguments.",
      "Provide at least one response variable: {.code mvbind(response1, response2, ...)}"
    ), call. = FALSE)
  }

  # Extract variable names from each argument
  response_names <- character(length(response_args))

  for (i in seq_along(response_args)) {
    arg <- response_args[[i]]

    # Extract the primary variable name from expression
    var_name <- extract_variable_name(arg)

    if (is.null(var_name) || nchar(var_name) == 0) {
      arg_text <- deparse(arg)
      stop(insight::format_error(
        paste0("Could not extract response variable name from mvbind() argument ", i, "."),
        paste0("Argument: ", arg_text),
        "Ensure all mvbind() arguments reference valid variable names."
      ), call. = FALSE)
    }

    response_names[i] <- var_name
  }

  # Validate extracted names are valid R identifiers
  invalid_names <- !grepl("^[a-zA-Z][a-zA-Z0-9_.]*$", response_names)
  if (any(invalid_names)) {
    invalid_list <- response_names[invalid_names]
    stop(insight::format_error(
      "Invalid variable names extracted from mvbind():",
      paste0("Invalid names: ", paste(invalid_list, collapse = ", ")),
      "Use valid R variable names in mvbind() arguments."
    ), call. = FALSE)
  }

  return(response_names)
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
extract_variable_name <- function(expr, max_depth = 10) {
  # Protect against infinite recursion
  if (max_depth <= 0) {
    return(NULL)
  }

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
      # Recursively check nested calls with decremented depth
      nested_var <- extract_variable_name(arg, max_depth - 1)
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
# maintaining proper identifiability and parameter interpretation.
#' @noRd
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
