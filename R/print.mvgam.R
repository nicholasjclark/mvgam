#' Print a fitted \pkg{mvgam} object
#'
#' This function takes a fitted \code{mvgam} or \code{jsdgam} object and prints
#' a quick summary.
#'
#' @param x \code{list} object returned from \code{mvgam}
#' @param ... Ignored
#' @details A brief summary of the model's call is printed
#' @return A \code{list} is printed on-screen
#' @author Nicholas J Clark
#'
#' @export
print.mvgam = function(x, ...) {
  object <- x

  # Use shared extractor functions to eliminate code duplication
  model_spec <- extract_model_spec(object)
  sampling_info <- extract_sampling_info(object)

  # Print model specification with simplified logic for print.mvgam
  print_model_specification_simple(model_spec)

  # Print sampling information using shared helper
  print_sampling_information(sampling_info)
}

#' Print method for mvgam_formula objects
#'
#' @param x An mvgam_formula object
#' @param ... Ignored
#' @return The object invisibly
#' @export
print.mvgam_formula <- function(x, ...) {
  cat("mvgam_formula object\n")
  cat("Observation formula: ")
  print(x$formula)
  if (!is.null(x$trend_formula)) {
    cat("Trend formula: ")
    print(x$trend_formula)
  } else {
    cat("Trend formula: NULL (no trend component)\n")
  }
  invisible(x)
}

#' Print model specification section for print.mvgam (simplified version)
#' @param model_spec Model specification from extract_model_spec
#' @noRd
print_model_specification_simple <- function(model_spec) {
  # Print formulas
  if (!is.null(model_spec$formulas$process)) {
    cat("GAM observation formula:\n")
    print(model_spec$formulas$observation)
    cat("\nGAM process formula:\n")
    print(model_spec$formulas$process)
  } else {
    cat("GAM formula:\n")
    print(model_spec$formulas$observation)
  }

  # Print family and link
  cat("\nFamily:\n")
  cat(paste0(model_spec$family, '\n'))

  cat("\nLink function:\n")
  cat(paste0(model_spec$link, '\n'))

  # Print trend model
  if (!model_spec$is_jsdgam) {
    cat("\nTrend model:\n")
    if (is.call(model_spec$trend_model)) {
      print(model_spec$trend_model)
      cat('\n')
    } else {
      cat(paste0(model_spec$trend_model, '\n'))
    }
  }

  # Print latent variable info (simplified - always "latent factors" for print.mvgam)
  if (!is.null(model_spec$latent_variables)) {
    cat("\nN latent factors:\n")
    cat(model_spec$latent_variables$count, '\n')
  }

  # Print dimensions
  if (model_spec$is_jsdgam) {
    cat('\nN species:\n')
    cat(model_spec$dimensions$n_species, '\n')
    cat('\nN sites:\n')
    cat(model_spec$dimensions$n_sites, '\n')
  } else {
    cat('\nN series:\n')
    cat(model_spec$dimensions$n_series, '\n')
    cat('\nN timepoints:\n')
    cat(model_spec$dimensions$n_timepoints, '\n')
  }

  # Print upper bounds if present
  if (!is.null(model_spec$upper_bounds)) {
    cat('\nUpper bounds:\n')
    cat(model_spec$upper_bounds, '\n')
  }
}


#'@export
print.mvgam_prefit = function(x, ...) {
  object <- x

  # Use shared extractor function for model specification
  model_spec <- extract_model_spec(object)

  # Print model specification using shared helper
  print_model_specification(model_spec)

  # Add prefit-specific status message
  cat('\nStatus:\n')
  cat('Not fitted', '\n')
}

#' Extract Stan Code from mvgam Objects
#'
#' Extract the Stan model code used to fit mvgam objects. This function
#' follows brms conventions and returns a character vector with \code{stancode} class.
#'
#' @param object A fitted \code{mvgam} object or \code{mvgam_prefit} object.
#' @param ... Currently unused.
#'
#' @return A character string containing the Stan model code with class
#'   \code{c("stancode", "character")}.
#'
#' @details This method extracts the Stan code that was used to fit the model.
#'   The returned object has the \code{stancode} class, making it compatible
#'   with brms workflows and allowing users to inspect, modify, or save the
#'   generated Stan code.
#'
#' @examples
#' \dontrun{
#' # Fit a simple mvgam model
#' data(portal_data)
#' mod <- mvgam(count ~ s(time),
#'              trend_formula = ~ RW(),
#'              data = portal_data,
#'              family = poisson())
#'
#' # Extract and inspect Stan code
#' code <- stancode(mod)
#' cat(code)
#'
#' # Check class
#' class(code)
#' }
#'
#' @export
stancode.mvgam <- function(object, ...) {
  checkmate::assert_class(object, "mvgam")

  if (is.null(object$stancode)) {
    insight::format_error(
      "Stan code not found in mvgam object.",
      "The model may have been fitted with an older version that didn't store Stan code."
    )
  }

  # Add mvgam-specific stancode class with brms compatibility
  code <- object$stancode
  class(code) <- c("mvgamstancode", "stancode", "character")
  return(code)
}

#' @rdname stancode.mvgam
#' @export
stancode.mvgam_prefit <- function(object, ...) {
  checkmate::assert_class(object, "mvgam_prefit")

  if (is.null(object$stancode)) {
    insight::format_error(
      "Stan code not found in mvgam_prefit object.",
      "The prefit object may not have been properly generated."
    )
  }

  # Add mvgam-specific stancode class with brms compatibility
  code <- object$stancode
  class(code) <- c("mvgamstancode", "stancode", "character")
  return(code)
}

#' Print mvgam Stan Code Objects
#'
#' Print method for mvgamstancode objects that displays Stan model code in a
#' clean, readable format.
#'
#' @param x A mvgamstancode object.
#' @param ... Currently unused.
#'
#' @return The mvgamstancode object is returned invisibly.
#'
#' @export
print.mvgamstancode <- function(x, ...) {
  cat(x, sep = '\n')
  invisible(x)
}
