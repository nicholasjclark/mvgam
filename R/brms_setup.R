#' Lightweight brms Model Setup Using Mock Backend
#'
#' @description
#' Performs rapid brms model setup using backend = "mock" for maximum speed.
#' Extracts necessary model components without full compilation or sampling.
#'
#' @param formula brms formula object
#' @param data Data frame for model setup
#' @param family Family specification (defaults to gaussian())
#' @param stanvars Additional stanvars for model extension
#' @param ... Additional arguments passed to brms
#' @return List containing brms setup components
#' @noRd
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
    stancode = extract_stancode_from_setup(mock_setup),
    standata = extract_standata_from_setup(mock_setup),
    brmsfit = mock_setup,  # Keep the mock brmsfit for prediction
    setup_time = system.time({})[["elapsed"]] # Track performance
  )

  # Validate extracted components
  validate_setup_components(setup_components)

  return(setup_components)
}


#' Extract Stan Code from brms Setup
#' @param setup_object brms setup object
#' @return Character string of Stan code
#' @noRd
extract_stancode_from_setup <- function(setup_object) {
  if (methods::hasMethod("stancode", class(setup_object))) {
    return(brms::stancode(setup_object))
  } else if (!is.null(setup_object$model)) {
    return(setup_object$model)
  } else {
    # Extract from internal structure - let errors bubble up
    code <- setup_object@stancode
    return(code)
  }
}

#' Extract Stan Data from brms Setup
#' @param setup_object brms setup object
#' @return List of Stan data components
#' @noRd
extract_standata_from_setup <- function(setup_object) {
  if (methods::hasMethod("standata", class(setup_object))) {
    return(brms::standata(setup_object))
  } else if (!is.null(setup_object$data)) {
    return(setup_object$data)
  } else {
    # Extract from internal structure - let errors bubble up
    data <- setup_object@standata
    return(data)
  }
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
