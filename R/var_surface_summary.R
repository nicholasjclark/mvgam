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
#' @param series Optional integer naming the process the shock
#'   originates in, as in `plot.mvgam_irf()`. Default draws every
#'   originating process.
#' @param responses Optional integer vector naming the responding
#'   processes to draw. Default draws all of them.
#' @param shocks Optional character vector naming shock-response pairs
#'   directly, for a selection `series` and `responses` cannot express.
#' @param ... ignored
#'
#' @return A `ggplot` object
#'
#' @method plot mvgam_var_surface_summary
#' @export
plot.mvgam_var_surface_summary <- function(x, series = NULL,
                                           responses = NULL,
                                           shocks = NULL, ...) {
  checkmate::assert_class(x, "mvgam_var_surface_summary")
  checkmate::assert_int(series, lower = 1L, null.ok = TRUE)
  checkmate::assert_integerish(responses, lower = 1L, null.ok = TRUE)
  checkmate::assert_character(shocks, null.ok = TRUE, min.len = 1L)
  cols <- var_surface_columns(x)
  dat <- as.data.frame(x)

  # `series` and `responses` select the same way they do on the draws
  # plot, so code written against that one keeps working when the
  # summary becomes what `irf()` hands back. Both surfaces label a pair
  # `Process_<from> -> Process_<to>`, so the selection reads off the
  # two ends of the label.
  ends <- strsplit(dat$shock, " -> ", fixed = TRUE)
  from <- as.integer(sub("^\\D*", "", vapply(ends, `[`, character(1), 1L)))
  to <- as.integer(sub("^\\D*", "", vapply(ends, `[`, character(1), 2L)))
  keep <- rep(TRUE, nrow(dat))
  if (!is.null(series)) keep <- keep & from == series
  if (!is.null(responses)) keep <- keep & to %in% responses
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
  # Already a summary; asking again returns the same table rather than
  # attempting to summarise quantiles a second time.
  unclass_var_surface(object)
}
