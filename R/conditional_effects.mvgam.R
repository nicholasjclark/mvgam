#' Display conditional effects of predictors for mvgam models
#'
#' Plot the conditional effects of one or more numeric / categorical
#' predictors in fitted `mvgam` models, including up-to-three-way
#' interactions. A thin wrapper around
#' [marginaleffects::plot_predictions()] that auto-enumerates the
#' model's term labels (across both observation and trend submodels)
#' and produces one ggplot per term.
#'
#' @param x A fitted `mvgam` object.
#' @param effects Optional character vector of effects to plot. If
#'   `NULL` (the default), all main effects and interactions in the
#'   formula are detected automatically.
#' @param type Scale of predictions. One of `"response"` (outcome
#'   scale with observation-process noise), `"link"` (link-scale
#'   linear predictor) or `"expected"` (E[Y]; default).
#' @param points Logical or numeric. If `TRUE` (or a non-zero alpha
#'   between 0 and 1) and `type = "response"`, raw observations are
#'   overlaid on the plot.
#' @param rug Logical. If `TRUE` and `type = "response"`, rug tick
#'   marks are drawn along the axes.
#' @param process_error Logical. Passed to `plot_predictions()` /
#'   `get_predict.mvgam`. Defaults to `FALSE` so the latent trend
#'   collapses to its posterior mean; set `TRUE` to include per-draw
#'   latent-state uncertainty.
#' @param ... Additional arguments forwarded to
#'   [marginaleffects::plot_predictions()].
#'
#' @return An object of class `mvgam_conditional_effects`: a named
#'   list with one `ggplot` per effect, drawn by the `plot` and
#'   `print` methods.
#'
#' @seealso [marginaleffects::plot_predictions()],
#'   [marginaleffects::plot_slopes()]
#'
#' @author Nicholas J Clark
#' @method conditional_effects mvgam
#' @export
#' @importFrom brms conditional_effects
conditional_effects.mvgam <- function(x,
                                      effects = NULL,
                                      type = "expected",
                                      points = FALSE,
                                      rug = FALSE,
                                      process_error = FALSE,
                                      ...) {
  checkmate::assert_class(x, "mvgam")
  checkmate::assert_character(effects, null.ok = TRUE)
  checkmate::assert_logical(process_error, len = 1L)
  type <- match.arg(type, c("response", "link", "expected"))

  # Observation rugs and overlaid points only make sense on response
  # scale, and only for univariate fits (multivariate `mvbind`
  # responses don't have a single column to anchor points on).
  on_response <- identical(type, "response")
  is_mv <- brms::is.mvbrmsformula(x$formula)
  points_alpha <- 0
  if (on_response && !is_mv) {
    if (isTRUE(points)) {
      points_alpha <- 0.5
    } else if (is.numeric(points) && length(points) == 1L &&
               points > 0 && points <= 1) {
      points_alpha <- as.numeric(points)
    }
  }
  if (!on_response || is_mv) rug <- FALSE

  cond_labs <- if (is.null(effects)) {
    detect_conditional_effects(x)
  } else {
    lapply(as.character(effects), function(e) strsplit(e, ":")[[1L]])
  }

  if (any(lengths(cond_labs) > 3L)) {
    stop(insight::format_error(c(
      "Effects of order higher than 3 are not supported by conditional_effects.",
      i = "Use {.fn marginaleffects::plot_predictions} directly to build the higher-order plot."
    )))
  }

  if (length(cond_labs) == 0L) {
    out <- list()
    class(out) <- "mvgam_conditional_effects"
    return(out)
  }

  out <- lapply(cond_labs, function(cond) {
    marginaleffects::plot_predictions(
      x,
      condition = cond,
      draw = TRUE,
      type = type,
      points = points_alpha,
      rug = rug,
      process_error = process_error,
      ...
    ) +
      ggplot2::scale_fill_discrete(label = round_legend_labels) +
      ggplot2::scale_colour_discrete(label = round_legend_labels) +
      ggplot2::theme_classic()
  })
  names(out) <- vapply(cond_labs, paste, FUN.VALUE = character(1L),
                       collapse = ":")
  class(out) <- "mvgam_conditional_effects"
  out
}


#' Plot or print a mvgam_conditional_effects object
#' @param x An object of class `mvgam_conditional_effects`.
#' @param plot Logical. If `TRUE` (default), draws each plot;
#'   otherwise returns the list invisibly so callers can post-process.
#' @param ask Logical. If `TRUE`, prompts before each new plot.
#' @param ... Ignored.
#' @return Invisibly returns the list of ggplot objects.
#' @rdname conditional_effects.mvgam
#' @export
plot.mvgam_conditional_effects <- function(x, plot = TRUE, ask = FALSE,
                                           ...) {
  if (length(x) == 0L) return(invisible(x))
  if (isTRUE(plot)) {
    default_ask <- grDevices::devAskNewPage()
    on.exit(grDevices::devAskNewPage(default_ask))
    grDevices::devAskNewPage(ask = isTRUE(ask))
    for (p in x) graphics::plot(p)
  }
  invisible(x)
}


#' @rdname conditional_effects.mvgam
#' @export
print.mvgam_conditional_effects <- function(x, ...) plot(x, ...)


# Enumerate the conditional-effects term labels for an mvgam fit:
# walks the observation formula plus the trend formula (when
# present), splits smooth / interaction terms, and returns a list of
# unique up-to-3-way variable groupings.
detect_conditional_effects <- function(x) {
  obs_f <- if (inherits(x$formula, "brmsformula")) {
    x$formula$formula
  } else {
    x$formula
  }
  termlabs <- attr(stats::terms(obs_f, keep.order = TRUE), "term.labels")
  if (!is.null(x$trend_formula)) {
    termlabs <- c(
      termlabs,
      attr(stats::terms(x$trend_formula, keep.order = TRUE),
           "term.labels")
    )
  }
  termlabs <- termlabs[!grepl("^offset\\(", termlabs)]
  cond <- unlist(lapply(termlabs, split_term_labels),
                 recursive = FALSE)
  # Drop duplicates while preserving order
  keys <- vapply(cond, paste, FUN.VALUE = character(1L), collapse = ":")
  cond[!duplicated(keys)]
}


# Split a single formula term label into a list of variable
# groupings suitable for plot_predictions(condition = ...). Handles
# `:` / `*` interactions, smooth wrappers (`s`, `te`, `t2`, `ti`,
# `gp`, `mo`) and bare terms. For three-variable smooths returns the
# three pairwise marginal groupings (matches master's split_termlabs
# semantics).
split_term_labels <- function(lab) {
  if (grepl(":", lab, fixed = TRUE)) {
    return(list(strsplit(lab, ":", fixed = TRUE)[[1L]]))
  }
  if (grepl("*", lab, fixed = TRUE)) {
    return(list(strsplit(lab, "*", fixed = TRUE)[[1L]]))
  }
  smooth_starts <- c("s(", "te(", "t2(", "ti(", "gp(", "mo(")
  if (any(vapply(smooth_starts, grepl, FUN.VALUE = logical(1L),
                 x = lab, fixed = TRUE))) {
    parsed <- tryCatch(eval(rlang::parse_expr(lab)),
                       error = function(e) NULL)
    if (is.null(parsed) || is.null(parsed$term)) {
      return(list(all.vars(rlang::parse_expr(lab))))
    }
    by_var <- if (!is.null(parsed$by) && !identical(parsed$by, "NA")) {
      parsed$by
    } else {
      NULL
    }
    if (length(parsed$term) <= 2L) {
      return(list(c(all.vars(parse(text = parsed$term)), by_var)))
    }
    list(
      c(all.vars(parse(text = parsed$term[1:2])), by_var),
      c(all.vars(parse(text = parsed$term[c(1, 3)])), by_var),
      c(all.vars(parse(text = parsed$term[c(2, 3)])), by_var)
    )
  } else {
    list(all.vars(rlang::parse_expr(lab)))
  }
}


# Tidy ggplot legend labels: keep integers untouched, round numeric
# values to a small fixed number of decimal places so legends don't
# carry 8+ digit floating-point noise.
round_legend_labels <- function(x) {
  numeric_x <- suppressWarnings(as.numeric(x))
  if (all(is.na(numeric_x))) {
    return(x)
  }
  decimals <- vapply(numeric_x, function(v) {
    if (is.na(v)) {
      return(0L)
    }
    if (abs(v - round(v)) <= .Machine$double.eps^0.5) {
      return(0L)
    }
    parts <- strsplit(sub("0+$", "", format(v, scientific = FALSE)),
                      ".", fixed = TRUE)[[1L]]
    if (length(parts) < 2L) 0L else nchar(parts[2L])
  }, integer(1L))
  if (all(decimals == 0L)) {
    return(format(numeric_x, scientific = FALSE))
  }
  if (all(decimals <= 1L)) {
    return(sprintf("%.1f", numeric_x))
  }
  sprintf("%.4f", numeric_x)
}
