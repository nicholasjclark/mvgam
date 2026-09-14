# Summaries of the VAR response surfaces (`irf()`, `fevd()`).
#
# Both surfaces are built one transition matrix per posterior draw, so
# their draws are a `K` by `K` matrix per horizon per draw. On a wide
# panel that is hundreds of megabytes for something a reader looks at
# as a median and an interval, so both return the summary and keep the
# draws behind `summary = FALSE`. The summary carries a class of its
# own so it can be plotted and printed without being mistaken for the
# draws it came from.


#' Tag a summarised VAR surface with its class
#'
#' @param x Long-format summary from `summary.mvgam_irf()` or
#'   `summary.mvgam_fevd()`
#' @param cls Class to prepend
#' @param ... Attributes to carry through, such as the impulse type
#' @return `x` with `cls` prepended to its class
#'
#' @noRd
as_var_surface_summary <- function(x, cls, ...) {
  extra <- list(...)
  for (nm in names(extra)) {
    attr(x, nm) <- extra[[nm]]
  }
  class(x) <- c(cls, "mvgam_var_surface_summary", class(x))
  x
}


#' Quantile column names of a summarised VAR surface
#'
#' The summarisers name their columns after the percentiles asked for,
#' so the median sits on a fixed name and the interval bounds do not.
#' Reading them by position within the surface's own prefix keeps the
#' plot and print methods working whatever `probs` was.
#'
#' @param x A summarised VAR surface
#' @return Named list with `median`, `lower` and `upper` column names
#'
#' @noRd
var_surface_columns <- function(x) {
  quant <- grep("Q[0-9.]+$|Q50$", names(x), value = TRUE)
  median_col <- grep("Q50$", quant, value = TRUE)[1L]
  bounds <- setdiff(quant, median_col)
  pct <- as.numeric(sub("^.*Q", "", bounds))
  list(median = median_col,
       lower = bounds[which.min(pct)],
       upper = bounds[which.max(pct)])
}


#' Plot a summarised impulse response or variance decomposition
#'
#' Draws the posterior median and interval of each shock-response pair
#' against the forecast horizon. Plotting the summary rather than the
#' draws costs nothing in fidelity: the bands a draws plot builds are
#' these same quantiles.
#'
#' @param x A `mvgam_irf_summary` or `mvgam_fevd_summary`
#' @param series Integer vector of processes to keep, read at the end
#'   of the pair the surface is about: the process a shock originates
#'   in on an impulse response, the target whose forecast error is
#'   decomposed on a variance decomposition. Default keeps every
#'   process.
#' @param responses Impulse responses only. Integer vector of the
#'   processes whose response to the shock is drawn. Default keeps
#'   every process.
#' @param contributing Variance decompositions only. Integer vector of
#'   the source processes whose contributions are drawn. Each share was
#'   computed before the surface was summarised, so a selection here
#'   chooses the pairs drawn and leaves every share at its value.
#' @param shocks Optional character vector naming shock-response pairs
#'   directly, for a selection the index arguments cannot express.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A `ggplot` object
#'
#' @method plot mvgam_var_surface_summary
#' @export
plot.mvgam_var_surface_summary <- function(x, series = NULL,
                                           responses = NULL,
                                           contributing = NULL,
                                           shocks = NULL, ...) {
  checkmate::assert_class(x, "mvgam_var_surface_summary")
  checkmate::assert_character(shocks, null.ok = TRUE, min.len = 1L)
  rlang::check_dots_empty()
  cols <- var_surface_columns(x)
  dat <- as.data.frame(x)

  # Both surfaces label a pair `<from> -> <to>`, naming each process
  # the way the fit names it, so the selection reads off the two ends
  # of the label. Which end each argument names is the surface's own
  # convention, and it is the one `plot.mvgam_irf()` and
  # `plot.mvgam_fevd()` use on the draws.
  ends <- strsplit(dat$shock, " -> ", fixed = TRUE)
  left <- vapply(ends, `[`, character(1), 1L)
  right <- vapply(ends, `[`, character(1), 2L)
  # The surface carries the labels in the order the VAR numbers its
  # processes, so a selection by index is a position in that vector.
  # Reading the index out of the label itself only worked while every
  # label ended in its own number.
  labels <- attr(x, "process_labels") %||% unique(left)
  from <- match(left, labels)
  to <- match(right, labels)
  n_proc <- length(labels)
  is_fevd <- inherits(x, "mvgam_fevd_summary")

  # A decomposition is read from its target inwards and an impulse
  # response from its shock outwards, so each surface takes one of the
  # two names and refuses the other.
  unusable <- if (is_fevd) "responses" else "contributing"
  usable <- if (is_fevd) "contributing" else "responses"
  if (!is.null(if (is_fevd) responses else contributing)) {
    stop(insight::format_error(c(
      paste0("'", unusable, "' does not select on this surface."),
      x = paste0(
        "Supplied on a ",
        if (is_fevd) "variance decomposition" else "impulse response",
        "."
      ),
      i = paste0("Use '", usable, "' to name the other end of the pair.")
    )))
  }
  keep <- rep(TRUE, nrow(dat))
  if (is_fevd) {
    keep <- keep &
      to %in% validate_var_plot_ids(series, n_proc, "series") &
      from %in% validate_var_plot_ids(
        contributing, n_proc, "contributing"
      )
  } else {
    keep <- keep &
      from %in% validate_var_plot_ids(series, n_proc, "series") &
      to %in% validate_var_plot_ids(responses, n_proc, "responses")
  }
  if (!is.null(shocks)) {
    unknown <- setdiff(shocks, unique(dat$shock))
    if (length(unknown)) {
      stop(insight::format_error(c(
        "Unknown shock-response pairs requested.",
        x = paste0("Not present: ", paste(unknown, collapse = ", "), "."),
        i = paste0("Available: ",
                   paste(utils::head(unique(dat$shock), 6L),
                         collapse = ", "), ".")
      )))
    }
    keep <- keep & dat$shock %in% shocks
  }
  if (!any(keep)) {
    stop(insight::format_error(c(
      "No shock-response pairs match the selection.",
      i = paste0("The fitted VAR has ", max(c(from, to)), " processes.")
    )))
  }
  dat <- dat[keep, , drop = FALSE]
  set_color_scheme_local("red")
  ggplot2::ggplot(
    dat,
    ggplot2::aes(x = .data$horizon, y = .data[[cols$median]])
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data[[cols$lower]], ymax = .data[[cols$upper]]),
      fill = mvgam_colour("mid"), alpha = 0.5
    ) +
    ggplot2::geom_line(
      colour = mvgam_colour("dark_highlight"), linewidth = 0.7
    ) +
    ggplot2::facet_wrap(~ shock, scales = "free_y") +
    ggplot2::labs(
      x = "Horizon",
      y = if (inherits(x, "mvgam_fevd_summary")) {
        "Proportion of forecast error variance"
      } else {
        "Impulse response"
      }
    ) +
    mvgam_theme()
}


#' @method print mvgam_var_surface_summary
#' @export
print.mvgam_var_surface_summary <- function(x, ...) {
  type <- attr(x, "irf_type")
  cat(
    if (inherits(x, "mvgam_irf_summary")) {
      paste0(type %||% "Impulse", " impulse responses")
    } else {
      "Forecast error variance decomposition"
    },
    "\n"
  )
  cat("Shock-response pairs:", length(unique(x$shock)),
      " Horizons:", max(x$horizon), "\n\n")
  print(tibble::as_tibble(unclass_var_surface(x)), ...)
  invisible(x)
}


#' Strip the summary classes so the tibble prints as itself
#'
#' @noRd
unclass_var_surface <- function(x) {
  class(x) <- setdiff(
    class(x),
    c("mvgam_irf_summary", "mvgam_fevd_summary",
      "mvgam_var_surface_summary")
  )
  x
}


#' @method summary mvgam_var_surface_summary
#' @export
summary.mvgam_var_surface_summary <- function(object, ...) {
  rlang::check_dots_empty()
  # Already a summary; asking again returns the same table rather than
  # attempting to summarise quantiles a second time.
  unclass_var_surface(object)
}
