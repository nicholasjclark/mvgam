#' Print an \pkg{mvgam} object
#'
#' @param x \code{mvgam} object returned from \code{mvgam()}, fitted or
#'   built with \code{run_model = FALSE}
#' @param ... Unused. Anything passed here is refused.
#'
#' @return The \code{mvgam} object is returned invisibly.
#'
#' @seealso \code{\link{summary.mvgam}}, \code{\link{mvgam}}
#'
#' @export
print.mvgam <- function(x, ...) {
  checkmate::assert_class(x, "mvgam")
  # This method prints structure: the formulas, the family, the axis
  # counts and the sampler's state. No estimate is shown, so there is
  # nothing for a `digits` argument to round; `summary()` prints the
  # estimates and takes one.
  rlang::check_dots_empty()

  # Section 1: Formulas (distinguish observation vs process)
  if (!is.null(x$trend_formula)) {
    cat("GAM observation formula:\n")
    print_model_formula(formula(x))
    cat("\nGAM process formula:\n")
    print_model_formula(x$trend_formula)
  } else {
    cat("GAM formula:\n")
    print_model_formula(formula(x))
  }

  # Section 2: Family and links, one pair per response
  cat("\n")
  print_family_links(x)

  # Section 3: Trend model (if present)
  if (!is.null(x$trend_formula)) {
    cat("\n\nTrend model:\n")
    cat(printed_trend_label(x), "\n")
  }

  # Section 4: N series
  counts <- printed_axis_counts(x)
  if (!is.null(counts$n_series)) {
    cat('\n\nN series:\n')
    cat(counts$n_series, '\n')
  }

  # Section 5: N timepoints
  if (!is.null(counts$n_timepoints)) {
    cat('\n\nN timepoints:\n')
    cat(counts$n_timepoints, '\n')
  }

  # Section 6: Sampling status. A `run_model = FALSE` model has no
  # draws to count.
  cat('\n\nStatus:\n')
  if (is.null(x$fit)) {
    cat("Not fitted", "\n")
  } else {
    draws <- posterior::as_draws(x$fit)
    cat(posterior::nchains(draws), 'chains, each with iter =',
        posterior::niterations(draws), '\n')
    cat('  Total post-warmup draws =', posterior::ndraws(draws), '\n')
  }

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
  print_model_formula(x$formula)
  if (!is.null(x$trend_formula)) {
    cat("Trend formula: ")
    print_model_formula(x$trend_formula)
  } else {
    cat("Trend formula: NULL (no trend component)\n")
  }
  invisible(x)
}

# Internal: print a model formula without its environment.
#
# `print.formula()` appends `<environment: 0x...>` for any formula
# not built in the global environment, which is every formula that
# reaches a fit. The address changes between sessions and says
# nothing about the model, so no formula this package prints carries
# one.
#' @noRd
print_model_formula <- function(f) {
  print(f, showEnv = FALSE)
  invisible(f)
}


# Internal: the trend line `print()` shows.
#
# `trend_order_label()` already renders the order a trend was fitted
# at: `ARMA(1, 1)` where an AR carries moving-average lags, `AR(3)`
# where it carries three, `PW(logistic)` where a piecewise trend is
# not the default one. Printing `trend_components$types` instead
# dropped every one of them, so an `AR(p = 3)` and an
# `AR(p = 1, ma = TRUE)` printed as the same bare `AR` as a plain
# AR(1) -- the two models a whole fixture file exists to tell apart.
#
# The bare type is the fallback for an object carrying no trend
# metadata, which is the only case the label cannot render.
#' @noRd
printed_trend_label <- function(x) {
  label <- trend_order_label(x)
  if (nzchar(label %||% "")) {
    return(label)
  }
  types <- x$trend_components$types
  if (!is.null(types)) {
    return(types[[1L]])
  }
  "None"
}


#' How many series and time points a model was built on
#'
#' Read from the axes the fit records, which a prefit carries as well
#' as a fitted model, so the two print alike. `series_info` and
#' `time_info` answer for a model built before the record existed and
#' for one with no trend, where no axes are resolved.
#'
#' @param x An `mvgam` or `mvgam_prefit` object
#' @return List with `n_series` and `n_timepoints`, either possibly
#'   `NULL`
#' @noRd
printed_axis_counts <- function(x) {
  axes <- mvgam_axes(x)
  list(
    n_series = axes$series$n %||% x$series_info$n_series,
    n_timepoints = axes$time$n %||% x$time_info$n_timepoints
  )
}

# Internal: a family whose `$family` is the name mvgam records.
#
# `brms::custom_family()` writes the placeholder "custom" there, so
# `family(fit)$family` answered "custom" for `occ()`, `nmix()`,
# `tweedie()`, `com_binomial()` and `diri()` while `glance()`, which
# runs the same object through `resolve_family_name()`, answered the
# family's own name. `family()` is the accessor other packages call,
# so it is the one that has to be right; the stored object keeps the
# placeholder, which is what brms reads.
#' @noRd
named_family <- function(fam) {
  if (is.null(fam)) return(fam)
  fam$family <- resolve_family_name(fam)
  fam
}


#' Extract family from mvgam object
#'
#' @param object mvgam object
#' @param resp Optional name of one response of a multivariate model.
#'   Without it, a model with several responses answers with one
#'   family per response.
#' @param ... Unused. Anything passed here is refused.
#' @return A family object, or a list of them named by response
#' @importFrom stats family
#' @export
family.mvgam <- function(object, resp = NULL, ...) {
  checkmate::assert_class(object, "mvgam")
  rlang::check_dots_empty()
  # `$family` holds one family even on an `mvbf()` model, the last
  # arm's, and reading it answered every response with that one.
  fams <- model_families(object, resp)
  if (inherits(fams, "family")) named_family(fams) else {
    lapply(fams, named_family)
  }
}

#' Extract formula from mvgam object
#'
#' @param x mvgam object
#' @param ... Unused. Anything passed here is refused.
#' @return Formula object
#' @export
formula.mvgam <- function(x, ...) {
  checkmate::assert_class(x, "mvgam")
  rlang::check_dots_empty()

  if (is.null(x$formula)) {
    stop(insight::format_error(c(
      "Formula not found in mvgam object.",
      i = "The object may be corrupted or from an incompatible version."
    )))
  }

  return(x$formula)
}

#' Extract number of observations from mvgam object
#'
#' @param object mvgam object
#' @param ... Unused. Anything passed here is refused.
#' @return Integer count of the rows the model was given, matching
#'   `nrow(model.frame(object))`. A frame mvgam fits is rectangular,
#'   so an unobserved cell is part of the design and is counted.
#' @export
nobs.mvgam <- function(object, ...) {
  checkmate::assert_class(object, "mvgam")
  rlang::check_dots_empty()

  # `standata$N` counts the rows contributing a density, a different
  # quantity: 276 of 300 on a frame with unobserved cells, and on a
  # wide frame one response's count rather than the model's (58,
  # where the three responses have 57, 58 and 55). Using it as a
  # fallback made one function report two quantities, chosen by
  # which slot the object happened to carry.
  if (is.null(object$data)) {
    stop(insight::format_error(c(
      "No data found in mvgam object.",
      i = "Refit the model and ensure the data is stored on the object."
    )), call. = FALSE)
  }
  nrow(object$data)
}

#' Extract Stan Code from mvgam Objects
#'
#' Extract the Stan model code used to fit mvgam objects. This
#' function follows brms conventions and returns a character vector
#' with \code{stancode} class.
#'
#' @param object A fitted \code{mvgam} object or \code{mvgam_prefit} object.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A character string containing the Stan model code with class
#'   \code{c("stancode", "character")}.
#'
#' @details This method extracts the Stan code that was used to fit the model.
#'   The returned object has the \code{stancode} class, making it compatible
#'   with brms workflows and allowing users to inspect, modify, or save the
#'   generated Stan code.
#'
#' @export
stancode.mvgam <- function(object, ...) {
  checkmate::assert_class(object, "mvgam")
  rlang::check_dots_empty()

  if (is.null(object$stancode)) {
    stop(insight::format_error(c(
      "Stan code not found in mvgam object.",
      i = paste(
        "The model may have been fitted with a version that did not",
        "store Stan code."
      )
    )))
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
