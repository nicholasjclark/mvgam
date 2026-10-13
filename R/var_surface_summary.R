# Summaries of the VAR response surfaces (`irf()`, `fevd()`).
#
# Both surfaces are built from one transition matrix per posterior
# draw. Their draws are a `K` by `K` matrix per horizon per draw, which
# on a wide panel runs to hundreds of megabytes. A reader needs only
# a median and an interval from them. Both functions return that
# summary. `summary = FALSE` returns the draws. The summary has a
# class of its own. Its plot and print methods then never take it for
# the draws.


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
#' The summarisers name their columns after the percentiles asked for.
#' The median column has a fixed name and the interval bounds do not.
#' This function finds the bounds by percentile within the surface's
#' prefix. The plot and print methods then work for any `probs`.
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
#' An impulse response draws the posterior median and interval of each
#' shock-response pair against the forecast horizon, with a dashed
#' line at zero. A variance decomposition stacks the median shares of
#' its sources in one panel per target, as [plot.mvgam_fevd()] does.
#'
#' @param x A `mvgam_irf_summary` or `mvgam_fevd_summary`
#' @param series Integer vector of processes to keep. On an impulse
#'   response these are the processes a shock originates in. On a
#'   variance decomposition they are the targets whose forecast error
#'   is decomposed. Default keeps every process.
#' @param responses Impulse responses only. Integer vector of the
#'   processes whose response to the shock is drawn. Default keeps
#'   every process.
#' @param contributing Variance decompositions only. Integer vector of
#'   the source processes stacked in each bar. The shares of the
#'   sources drawn are renormalised to sum to one.
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

  # Both surfaces label a pair `<from> -> <to>` with the fit's names
  # for the processes. The selection splits the label at the arrow.
  # Each argument names the same end as it does in `plot.mvgam_irf()`
  # and `plot.mvgam_fevd()` on the draws.
  ends <- strsplit(dat$shock, " -> ", fixed = TRUE)
  left <- vapply(ends, `[`, character(1), 1L)
  right <- vapply(ends, `[`, character(1), 2L)
  # The `process_labels` attribute lists the processes in the order
  # the VAR numbers them. A selection by index is a position in it. An
  # index parsed from the label worked only while every label ended in
  # its own number.
  labels <- attr(x, "process_labels") %||% unique(left)
  from <- match(left, labels)
  to <- match(right, labels)
  n_proc <- length(labels)
  is_fevd <- inherits(x, "mvgam_fevd_summary")

  # A decomposition selects its sources and an impulse response its
  # responses. Each surface refuses the other's argument.
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
      i = paste0("Use '", usable, "' to choose the other end of the pair.")
    )), call. = FALSE)
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
      )), call. = FALSE)
    }
    keep <- keep & dat$shock %in% shocks
  }
  if (!any(keep)) {
    stop(insight::format_error(c(
      "No shock-response pairs match the selection.",
      i = paste0("The fitted VAR has ", n_proc, " processes.")
    )), call. = FALSE)
  }
  dat <- dat[keep, , drop = FALSE]
  from <- from[keep]
  to <- to[keep]
  # A decomposition stacks its sources' median shares in one panel per
  # target, as the draws do in `plot.mvgam_fevd()`
  if (is_fevd) {
    shares <- data.frame(horizon = dat$horizon, Series = labels[from],
                         target = labels[to], share = dat[[cols$median]])
    return(plot_fevd_shares(shares, labels[sort(unique(to))],
                            labels[sort(unique(from))]))
  }
  # The panels follow the order the VAR numbers its processes, shock
  # first. A character facet sorted them alphabetically.
  pair_order <- order(from, to)
  dat$shock <- factor(dat$shock, levels = unique(dat$shock[pair_order]))
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
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        colour = "black") +
    ggplot2::facet_wrap(~ shock, scales = "free_y") +
    scale_x_steps() +
    ggplot2::labs(y = irf_axis_label(attr(x, "irf_type"))) +
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
  # The object is already a summary. Asking again returns the same
  # table. Its quantiles cannot be summarised a second time.
  unclass_var_surface(object)
}
