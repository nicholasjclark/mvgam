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

  # Validate brms formula compatibility
  formula_validation <- mvgam:::validate_brms_formula(formula)
  if (!formula_validation$valid) {
    stop(insight::format_error(
      "Invalid brms formula structure:",
      paste(formula_validation$issues, collapse = "\n")
    ))
  }

  # Use mock backend for rapid setup (creates brmsfit object needed for prediction)
  mock_setup <- try({
    brms::brm(
      formula = formula,
      data = data,
      family = family,
      stanvars = stanvars,
      backend = "mock",
      mock_fit = 1,
      rename = FALSE
    )
  }, silent = TRUE)

  if (inherits(mock_setup, "try-error")) {
    # Mock backend should never fail - this indicates a bug in our code
    error_msg <- attr(mock_setup, "condition")$message
    stop(insight::format_error(
      "Mock backend failed unexpectedly - this indicates a bug in mvgam.",
      paste("brms error:", error_msg),
      "Check formula, data, family, and stanvars for issues.",
      paste("Formula:", deparse(formula)),
      paste("Family:", deparse(substitute(family))),
      paste("Data dimensions:", paste(dim(data), collapse = " x ")),
      paste("Stanvars provided:", !is.null(stanvars))
    ))
  }

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
    # Extract from internal structure
    code <- try(setup_object@stancode, silent = TRUE)
    if (inherits(code, "try-error")) {
      return(NULL)
    }
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
    # Extract from internal structure
    data <- try(setup_object@standata, silent = TRUE)
    if (inherits(data, "try-error")) {
      return(NULL)
    }
    return(data)
  }
}

#' Extract Prior Information from brms Setup
#' @param setup_object brms setup object
#' @return Data frame of prior specifications
#' @noRd
extract_prior_from_setup <- function(setup_object) {
  # Try to extract prior information
  prior_info <- try({
    if (!is.null(setup_object$prior)) {
      setup_object$prior
    } else {
      # Reconstruct from formula and data
      brms::get_prior(setup_object$formula, setup_object$data, setup_object$family)
    }
  }, silent = TRUE)

  if (inherits(prior_info, "try-error")) {
    return(NULL)
  }

  return(prior_info)
}

#' Extract brms Terms from Setup
#' @param setup_object brms setup object
#' @return brmsterms object
#' @noRd
extract_brmsterms_from_setup <- function(setup_object) {
  terms_info <- try({
    if (!is.null(setup_object$formula)) {
      brms::brmsterms(setup_object$formula)
    } else {
      NULL
    }
  }, silent = TRUE)

  if (inherits(terms_info, "try-error")) {
    return(NULL)
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
