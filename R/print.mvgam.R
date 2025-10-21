#' Print a fitted \pkg{mvgam} object
#'
#' @param x \code{mvgam} object returned from \code{mvgam()}
#' @param digits Integer for decimal places. Currently unused for
#'   consistency with brms.
#' @param ... Additional arguments (unused)
#'
#' @return The \code{mvgam} object is returned invisibly.
#'
#' @seealso \code{\link{summary.mvgam}}, \code{\link{mvgam}}
#'
#' @export
print.mvgam <- function(x, digits = 2, ...) {
  checkmate::assert_class(x, "mvgam")
  checkmate::assert_int(digits, lower = 0)

  # Section 1: Formulas (distinguish observation vs process)
  if (!is.null(x$trend_formula)) {
    cat("GAM observation formula:\n")
    print(formula(x))
    cat("\nGAM process formula:\n")
    print(x$trend_formula)
  } else {
    cat("GAM formula:\n")
    print(formula(x))
  }

  # Section 2: Family and link
  fam <- family(x)
  cat("\n\nFamily:\n")
  cat(fam$family, '\n')
  cat("\nLink function:\n")
  cat(fam$link, '\n')

  # Section 3: Trend model (if present)
  if (!is.null(x$trend_formula)) {
    cat("\n\nTrend model:\n")
    trend_comps <- x$trend_components
    if (!is.null(trend_comps) && !is.null(trend_comps$types)) {
      cat(trend_comps$types[[1]], '\n')
    } else {
      cat("None\n")
    }
  }

  # Section 4: N series
  if (!is.null(x$series_info) &&
      !is.null(x$series_info$n_series)) {
    cat('\n\nN series:\n')
    cat(x$series_info$n_series, '\n')
  }

  # Section 5: N timepoints
  if (!is.null(x$time_info) &&
      !is.null(x$time_info$n_timepoints)) {
    cat('\n\nN timepoints:\n')
    cat(x$time_info$n_timepoints, '\n')
  }

  # Section 6: Sampling status
  cat('\n\nStatus:\n')
  sim_info <- extract_mcmc_info(x)
  cat(sim_info$chains, 'chains, each with iter =', sim_info$iter, '\n')
  cat('  Total post-warmup draws =', sim_info$total_draws, '\n')

  invisible(x)
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

#' Extract family from mvgam object
#'
#' @param object mvgam object
#' @param ... Additional arguments (unused)
#' @return Family object
#' @export
family.mvgam <- function(object, ...) {
  checkmate::assert_class(object, "mvgam")

  if (is.null(object$family)) {
    insight::format_error(
      "Family not found in mvgam object.",
      "The object may be corrupted or from an incompatible version."
    )
  }

  return(object$family)
}

#' Extract formula from mvgam object
#'
#' @param x mvgam object
#' @param ... Additional arguments (unused)
#' @return Formula object
#' @export
formula.mvgam <- function(x, ...) {
  checkmate::assert_class(x, "mvgam")

  if (is.null(x$formula)) {
    insight::format_error(
      "Formula not found in mvgam object.",
      "The object may be corrupted or from an incompatible version."
    )
  }

  return(x$formula)
}

#' Extract number of observations from mvgam object
#'
#' @param object mvgam object
#' @param ... Additional arguments (unused)
#' @return Integer number of observations
#' @export
nobs.mvgam <- function(object, ...) {
  checkmate::assert_class(object, "mvgam")

  if (!is.null(object$data)) {
    return(nrow(object$data))
  } else if (!is.null(object$standata) &&
             !is.null(object$standata$N)) {
    return(object$standata$N)
  } else {
    insight::format_error(
      "Cannot determine number of observations.",
      paste("Neither {.field data} nor {.field standata$N}",
            "found in mvgam object.")
    )
  }
}

#' Extract MCMC sampling information from mvgam object
#'
#' Uses posterior package for robust extraction across backends. Returns
#' only information that can be reliably extracted (chains, post-warmup
#' iterations, total draws).
#'
#' @param mvgam_obj mvgam object
#' @return List with chains (integer), iter (post-warmup iterations per
#'   chain), and total_draws (total across all chains)
#' @noRd
extract_mcmc_info <- function(mvgam_obj) {
  checkmate::assert_class(mvgam_obj, "mvgam")

  if (is.null(mvgam_obj$fit)) {
    insight::format_error(
      "Stan fit not found in mvgam object.",
      "The object may be corrupted or incomplete."
    )
  }

  # Use posterior package (backend-independent)
  draws_obj <- posterior::as_draws(mvgam_obj$fit)

  return(list(
    chains = posterior::nchains(draws_obj),
    iter = posterior::niterations(draws_obj),
    total_draws = posterior::ndraws(draws_obj)
  ))
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
