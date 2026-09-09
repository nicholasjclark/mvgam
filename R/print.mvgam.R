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
    print_model_formula(formula(x))
    cat("\nGAM process formula:\n")
    print_model_formula(x$trend_formula)
  } else {
    cat("GAM formula:\n")
    print_model_formula(formula(x))
  }

  # Section 2: Family and link. A model written with `brms::mvbf()`
  # declares one family per response, so `family()` answers with a
  # named list and each response is named beside its own.
  fam <- family(x)
  if (is.null(fam$family)) {
    cat("\n\nFamilies:\n")
    for (resp in names(fam)) {
      cat(resp, ": ", fam[[resp]]$family, " (", fam[[resp]]$link, ")\n",
          sep = "")
    }
  } else {
    cat("\n\nFamily:\n")
    cat(fam$family, '\n')
    cat("\nLink function:\n")
    cat(fam$link, '\n')
  }

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
  print_model_formula(x$formula)
  if (!is.null(x$trend_formula)) {
    cat("Trend formula: ")
    print_model_formula(x$trend_formula)
  } else {
    cat("Trend formula: NULL (no trend component)\n")
  }
  invisible(x)
}

#' Print an unfitted \pkg{mvgam} object
#'
#' @param x \code{mvgam_prefit} object returned from \code{mvgam()} with
#'   \code{run_model = FALSE}
#' @param ... Additional arguments (unused)
#' @return The \code{mvgam_prefit} object is returned invisibly.
#' @export
print.mvgam_prefit <- function(x, ...) {
  # Formulas (distinguish observation vs process)
  if (!is.null(x$trend_formula)) {
    cat("GAM observation formula:\n")
    print_model_formula(x$formula)
    cat("\nGAM process formula:\n")
    print_model_formula(x$trend_formula)
  } else {
    cat("GAM formula:\n")
    print_model_formula(x$formula)
  }

  # Family and link
  if (!is.null(x$family)) {
    cat("\n\nFamily:\n")
    cat(x$family$family, "\n")
    cat("\nLink function:\n")
    cat(x$family$link, "\n")
  }

  # Trend model (if present)
  if (!is.null(x$trend_formula)) {
    cat("\n\nTrend model:\n")
    cat(printed_trend_label(x), "\n")
  }

  # N series
  counts <- printed_axis_counts(x)
  if (!is.null(counts$n_series)) {
    cat("\n\nN series:\n")
    cat(counts$n_series, "\n")
  }

  # N timepoints
  if (!is.null(counts$n_timepoints)) {
    cat("\n\nN timepoints:\n")
    cat(counts$n_timepoints, "\n")
  }

  # Sampling status
  cat("\n\nStatus:\n")
  cat("Not fitted", "\n")

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
#' @param ... Additional arguments (unused)
#' @return Family object
#' @importFrom stats family
#' @export
family.mvgam <- function(object, ...) {
  checkmate::assert_class(object, "mvgam")

  if (!is.null(object$family)) {
    return(named_family(object$family))
  }

  # A model written with `brms::mvbf()` gives each response its own
  # family, so no single family describes it and none is stored. The
  # families are on the formula, one per response, and are returned
  # as a named list the way brms answers the same question.
  forms <- object$formula$forms
  if (!is.null(forms)) {
    fams <- lapply(forms, function(form) form$family)
    if (!any(vapply(fams, is.null, logical(1)))) {
      return(lapply(fams, named_family))
    }
  }

  stop(insight::format_error(c(
    "Family not found in mvgam object.",
    i = "The object may be corrupted or from an incompatible version."
  )))
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
    stop(insight::format_error(c(
      "Cannot determine number of observations.",
      x = cli::format_inline(
        "Neither {.field data} nor {.field standata$N} found in mvgam object."
      )
    )))
  }
}

#' Extract MCMC sampling information from mvgam object
#'
#' Uses posterior package for consistent extraction across backends. Returns
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
    stop(insight::format_error(c(
      "Stan fit not found in mvgam object.",
      i = "The object may be corrupted or incomplete."
    )))
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
#' Extract the Stan model code used to fit mvgam objects. This
#' function follows brms conventions and returns a character vector
#' with \code{stancode} class.
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
#' @export
stancode.mvgam <- function(object, ...) {
  checkmate::assert_class(object, "mvgam")

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

#' @rdname stancode.mvgam
#' @export
stancode.mvgam_prefit <- function(object, ...) {
  checkmate::assert_class(object, "mvgam_prefit")

  if (is.null(object$stancode)) {
    stop(insight::format_error(c(
      "Stan code not found in mvgam_prefit object.",
      i = "The prefit object may not have been properly generated."
    )))
  }

  # Add mvgam-specific stancode class with brms compatibility
  code <- object$stancode
  class(code) <- c("mvgamstancode", "stancode", "character")
  return(code)
}

#' @rdname standata.mvgam
#' @export
standata.mvgam_prefit <- function(object, ...) {
  checkmate::assert_class(object, "mvgam_prefit")
  if (is.null(object$standata)) {
    stop(insight::format_error(c(
      "Stan data not found in mvgam_prefit object.",
      i = "The prefit object may not have been properly generated."
    )))
  }
  object$standata
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
