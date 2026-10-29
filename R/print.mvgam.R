#' Print an \pkg{mvgam} object
#'
#' @param x \code{mvgam} object returned from \code{mvgam()}, fitted or
#'   built with \code{run_model = FALSE}
#' @param ... Unused. Anything passed here is refused.
#'
#' @return The \code{mvgam} object is returned invisibly.
#'
#' @seealso [summary.mvgam()] summarises the posterior of each
#'   parameter. [methods_md()] writes the model as equations.
#'   [how_to_cite()] drafts its methods paragraph.
#'
#' @export
print.mvgam <- function(x, ...) {
  checkmate::assert_class(x, "mvgam")
  # This method prints structure: the formulas, the family, the axis
  # counts and the sampler's state. It prints no estimate and takes no
  # `digits` argument. `summary()` prints the estimates and takes one.
  rlang::check_dots_empty()

  # Section 1: the observation and process formulas. The trend line
  # below names the constructor. A process formula with no predictor
  # is left out.
  if (!is.null(x$trend_formula)) {
    cat("GAM observation formula:\n")
    print_model_formula(formula(x))
    predictors <- trend_predictors(x)
    if (!is.null(predictors)) {
      cat("\nGAM process formula:\n", predictors, "\n", sep = "")
    }
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

  # Sections 4 and 5: the sizes of the series and time axes
  counts <- printed_axis_counts(x)
  nouns <- axis_nouns(x)
  if (!is.null(counts$n_series)) {
    cat('\n\nN ', nouns$series, ':\n', sep = "")
    cat(counts$n_series, '\n')
  }
  if (!is.null(counts$n_timepoints)) {
    cat('\n\nN ', nouns$times, ':\n', sep = "")
    cat(counts$n_timepoints, '\n')
  }

  # Section 6: Sampling status. A `run_model = FALSE` model has no
  # draws to count.
  cat('\n\nStatus:\n')
  if (is.null(x$fit)) {
    cat("Not fitted", "\n")
  } else {
    cat(format_draws_line(draw_counts(x)), "\n")
  }

  invisible(x)
}

#' Print method for mvgam_formula objects
#'
#' @param x An mvgam_formula object
#' @param ... Ignored.
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

#' Print a model formula as `summary()` shows it
#'
#' Prints one line per response and per distributional parameter. The
#' output omits the placeholder term `format_model_formula()` strips and
#' the environment `print.formula()` appends.
#'
#' @param f Formula, `brmsformula` or `mvbrmsformula`
#' @return `f`, invisibly
#' @noRd
print_model_formula <- function(f) {
  checkmate::assert(
    checkmate::check_formula(f),
    checkmate::check_list(f)
  )
  cat(format_model_formula(f), sep = "\n")
  invisible(f)
}


#' The trend line `print()` shows
#'
#' Names the order a trend was fitted at, such as `ARMA(1, 1)` for an AR
#' with a moving-average term, `AR(3)` for three lags or `PW(logistic)`
#' for logistic growth. A model without a trend prints `None`. The
#' latent factors of a `jsdgam()` fit are its trend, as in
#' `2 latent factors`. Factors with dynamics name them, as in
#' `2 latent AR(1) factors`.
#'
#' @param x Fitted `mvgam` object
#' @return Character scalar trend label
#' @noRd
printed_trend_label <- function(x) {
  checkmate::assert_class(x, "mvgam")
  label <- trend_order_label(x)
  if (!nzchar(label)) label <- get_trend_type(x)
  n_lv <- detect_factor_n_lv(x)
  if (!inherits(x, "jsdgam") || is.null(n_lv)) return(label)
  paste0(n_lv, " latent ",
         if (!identical(label, "ZMVN")) paste0(label, " "),
         ngettext(n_lv, "factor", "factors"))
}


#' How many series and time points a model holds
#'
#' Counted on the axes the fit records. A prefit records the same axes
#' and prints the same counts.
#'
#' @param x An `mvgam` or `mvgam_prefit` object
#' @return List with `n_series` and `n_timepoints`, either possibly
#'   `NULL`
#' @noRd
printed_axis_counts <- function(x) {
  axes <- mvgam_axes(x)
  list(
    n_series = axes$series$n,
    n_timepoints = axes$time$n
  )
}

#' The nouns that name a model's series and time axes
#'
#' A `jsdgam()` fit indexes species across sampling units. Every other
#' fit indexes series through time.
#'
#' @param x An `mvgam` or `mvgam_prefit` object
#' @return List of `series` (singular and plural alike), `time` (one
#'   point of the time axis), `times` (its count noun) and `index` (what
#'   `t` indexes)
#' @noRd
axis_nouns <- function(x) {
  checkmate::assert_class(x, "mvgam")
  if (inherits(x, "jsdgam")) {
    return(list(series = "species", time = "sampling unit",
                times = "sampling units", index = "sampling units"))
  }
  list(series = "series", time = "time", times = "time points",
       index = "time")
}

#' A family whose `family` element holds the name mvgam records
#'
#' `brms::custom_family()` stores the placeholder `"custom"` in that
#' element for `occ()`, `nmix()`, `tweedie()`, `com_binomial()` and
#' `diri()`. Other packages call `family()` to learn the family name.
#' The stored object keeps the placeholder brms expects.
#'
#' @param fam Family object, or `NULL`
#' @return `fam` with `family` set by `resolve_family_name()`, or `NULL`
#' @noRd
named_family <- function(fam) {
  checkmate::assert_list(fam, null.ok = TRUE)
  if (is.null(fam)) return(fam)
  fam$family <- resolve_family_name(fam)
  fam
}


#' Extract family from mvgam object
#'
#' @param object mvgam object
#' @param resp Optional name of one response of a multivariate model.
#'   Without it, a model with several responses returns one family
#'   per response.
#' @param ... Unused. Anything passed here is refused.
#' @return A family object, or a list of them named by response
#' @importFrom stats family
#' @export
family.mvgam <- function(object, resp = NULL, ...) {
  checkmate::assert_class(object, "mvgam")
  rlang::check_dots_empty()
  # `$family` holds one family on an `mvbf()` model, that of its last
  # response. `model_families()` gives each response its own.
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

  # `standata$N` counts the rows that contribute a density. A frame
  # with unobserved cells gives 276 of its 300 rows. A wide frame
  # gives one response's count: 58, where the three responses have
  # 57, 58 and 55. This method reports the rows of the data alone.
  if (is.null(object$data)) {
    stop(insight::format_error(c(
      "No data found in mvgam object.",
      i = "Refit the model and ensure the data is stored on the object."
    )), call. = FALSE)
  }
  nrow(object$data)
}

#' Print the Stan program of an mvgam model
#'
#' Writes the program of [stancode.mvgam_formula()] one line at a time.
#'
#' @param x An `mvgamstancode` object.
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#'
#' @export
print.mvgamstancode <- function(x, ...) {
  cat(x, sep = '\n')
  invisible(x)
}
