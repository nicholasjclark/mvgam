#' Nonlinear Model Support for mvgam
#'
#' @description
#' Handles nonlinear model complexity (bf(nl = TRUE)) in the brms integration,
#' ensuring proper trend injection into nonlinear predictors.

#' Handle Nonlinear Model Complexity
#' 
#' @description
#' Processes nonlinear brms formulas to ensure trend effects are properly
#' integrated with nonlinear predictors. Handles bf(nl = TRUE) specifications.
#' 
#' @param formula brms formula object, possibly with nonlinear specification
#' @param trend_spec List containing trend specification
#' @return List containing processed formula information
#' @noRd
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