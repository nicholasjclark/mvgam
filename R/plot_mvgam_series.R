# Internal: exploratory plots of the observed time series carried
# on a fitted `mvgam` object. Called by `plot.mvgam(x, type =
# "series")`. The function is intentionally fit-only and not
# exported; user-side EDA without a fit goes through plain
# ggplot or `forecast::ggtsdisplay` / `feasts::gg_tsdisplay`.

#' Observed time series exploratory plot
#'
#' Single-series: a 4-panel patchwork showing time series, ACF,
#' empirical CDF, and histogram of the response. Multi-series:
#' one faceted time series plot. When `newdata` is supplied (or
#' carried on the fit as `object$test_data`), the test arm is
#' overlaid in black with a dashed cut at the boundary.
#'
#' @param object A fitted `mvgam` object.
#' @param newdata Optional data frame / list of future
#'   observations with the same `time` and `series` columns as
#'   the training data. When omitted the function uses
#'   `object$test_data`.
#' @param series `NULL` (default), `"all"`, or a positive
#'   integer indexing the series levels. `NULL` resolves to
#'   `"all"` for multi-series fits and `1` for single-series.
#' @param resp Optional response of a multivariate fit. Where the
#'   responses are the series it narrows the plot to that response's
#'   series; otherwise it names the response every series is drawn for.
#' @param lines Logical. Plot lines (default) or points.
#' @param n_bins Optional histogram bin count. Defaults to the
#'   number returned by `hist(..., plot = FALSE)$breaks` (at
#'   least 20).
#' @param log_scale Logical. Plot the response as
#'   `log(y + 1)` (multi-series view, where different scales
#'   are common).
#'
#' @return A `ggplot` (multi-series) or `patchwork`
#'   (single-series 4-panel) object.
#'
#' @noRd
plot_mvgam_series <- function(
  object,
  newdata = NULL,
  series = NULL,
  resp = NULL,
  lines = TRUE,
  n_bins = NULL,
  log_scale = FALSE
) {
  checkmate::assert_class(object, "mvgam")
  resolve_resp(object, resp)
  time_var <- object$trend_metadata$variables$time_var %||% "time"
  series_levels <- resolve_series_info(object)$series_levels
  test <- newdata %||% object$test_data
  if (!is.null(test) && !is.null(object$trend_metadata)) {
    validate_prediction_factor_levels(test, object$trend_metadata)
  }

  # Where the responses are the series, a row is one occasion carrying
  # every response, so each series is its own response's column and
  # the frame is stacked one response at a time. Everywhere else a row
  # names its series the way the fit identified it, and every series
  # is drawn for the one response in scope. Reading the raw series
  # column instead put a derived axis in a single panel labelled NA,
  # and reading one response column drew one response of three.
  keyed <- is_response_keyed(object)
  if (keyed && !is.null(resp)) {
    series_levels <- resp
  }
  labels <- if (keyed) {
    vapply(stats::setNames(series_levels, series_levels),
           function(key) response_column(object, key), character(1L))
  } else {
    response_column(object, resp)
  }
  long_of <- function(df, label) {
    if (is.null(df)) {
      return(NULL)
    }
    if (keyed) {
      return(do.call(rbind, lapply(series_levels, function(key) {
        series_long_df(df, labels[[key]], key, time_var, label)
      })))
    }
    series_long_df(df, labels, axis_row_series(object, df, required = TRUE),
                   time_var, label)
  }

  series_obs_plot(
    dat           = rbind(long_of(mvgam_training_data(object), "train"),
                          long_of(test, "validate")),
    labels        = labels,
    series_levels = series_levels,
    series        = series,
    lines         = lines,
    n_bins        = n_bins,
    log_scale     = log_scale
  )
}


# Internal: shared observed-series plot body. Builds the same
# faceted multi-series plot (when `series = "all"`) or 4-panel
# patchwork (when `series` resolves to one index) regardless of
# whether the input comes from a fitted `mvgam` object
# (`plot_mvgam_series()` / `plot.mvgam(type = "series")`) or a
# raw long-format data frame (`mvgam_data()` pre-fit). Both
# callers pass the long frame `series_long_df()` builds, so this
# helper does no class dispatch.
#
# `labels` names what the y axis measures: one response column, or
# one per series where each series is a different response. A panel
# of one series takes its own; a facet over several responses takes
# a label none of them can claim alone.
#'@noRd
series_obs_plot <- function(dat, labels, series_levels, series = NULL,
                             lines = TRUE, n_bins = NULL,
                             log_scale = FALSE) {
  checkmate::assert_flag(lines)
  checkmate::assert_flag(log_scale)
  checkmate::assert_integerish(n_bins, lower = 1L, len = 1L,
                                null.ok = TRUE)
  set_color_scheme_local("red")

  series_idx <- resolve_series_index(series, length(series_levels))
  dat$series <- factor(dat$series, levels = series_levels)
  s_name <- if (identical(series_idx, "all")) NULL else {
    series_levels[series_idx]
  }
  response <- if (!is.null(s_name) && length(labels) > 1L) {
    labels[[s_name]]
  } else if (length(unique(labels)) == 1L) {
    labels[[1L]]
  } else {
    "Observed value"
  }

  # An ordinal response arrives as an ordered factor, which none of the
  # four panels can take. Its level index is the representation the rest
  # of the package already predicts on, so the panels are drawn against
  # that and the axis says which levels the numbers stand for.
  ordinal_levels <- NULL
  if (is.factor(dat$y)) {
    if (!is.ordered(dat$y)) {
      stop(insight::format_error(c(
        paste0("Response '", response,
               "' is an unordered factor, which has no series to plot."),
        i = paste0("A histogram, an autocorrelation and an empirical CDF ",
                   "all need the response to be ordered.")
      )))
    }
    ordinal_levels <- levels(dat$y)
    dat$y <- as.integer(dat$y)
  }

  ylab <- if (log_scale) paste0("log(", response, " + 1)") else response
  if (!is.null(ordinal_levels)) {
    ylab <- paste0(ylab, " (", paste(ordinal_levels, collapse = " < "), ")")
  }
  if (log_scale) dat$y <- log(dat$y + 1)

  if (is.null(s_name)) {
    return(series_time_plot(dat, ylab, lines))
  }
  dat_s <- dat[as.character(dat$series) == s_name, , drop = FALSE]
  if (nrow(dat_s) == 0L) {
    stop(insight::format_error(c(
      "No observations found for the requested series.",
      x = paste0("Series: '", s_name, "'.")
    )))
  }
  patchwork::wrap_plots(
    series_time_plot(dat_s, ylab, lines, facet = FALSE),
    series_hist_panel(dat_s$y, ylab, n_bins),
    series_acf_panel(dat_s$y),
    series_ecdf_panel(dat_s$y, ylab),
    ncol = 2L, nrow = 2L, byrow = TRUE
  )
}


# Internal: coerce the user-facing `series` arg to either the
# literal string `"all"` or a 1-based integer index into the
# series levels. `NULL` defaults to `"all"` for multi-series
# fits and `1` for single-series.
#'@noRd
resolve_series_index <- function(series, n_series) {
  if (is.null(series)) {
    return(if (n_series > 1L) "all" else 1L)
  }
  if (is.character(series) && length(series) == 1L &&
        series == "all") {
    return("all")
  }
  ok <- is.numeric(series) && length(series) == 1L &&
    !is.na(series) && series == as.integer(series) && series >= 1L
  if (!ok || series > n_series) {
    stop(insight::format_error(c(
      "'series' must be 'all' or a positive integer index.",
      x = paste0("Got: ", deparse(series), "."),
      i = paste0("Available series: 1..", n_series, ".")
    )))
  }
  as.integer(series)
}


# Internal: one long `(time, y, series, data)` block from a training
# or test frame. The caller says which column is the response and
# which series each row is on, because that is a question about the
# model rather than about the frame. Returns NULL when `df` is NULL so
# callers can `rbind` it unconditionally.
#'@noRd
series_long_df <- function(df, response, series, time_var, label) {
  if (is.null(df)) {
    return(NULL)
  }
  if (!response %in% names(df)) {
    stop(insight::format_error(c(
      "Response variable not found in data.",
      x = paste0("Expected: '", response, "'."),
      i = paste0("Got columns: ",
                 paste0("'", names(df), "'", collapse = ", "), ".")
    )))
  }
  data.frame(
    time = df[[time_var]],
    y = df[[response]],
    series = as.character(series),
    data = label,
    stringsAsFactors = FALSE
  )
}


# Internal: observed series over time. Training observations in the
# active palette, test observations (when present) in black with a
# dashed cut at the boundary. `facet` gives one panel per series; the
# single-series grid calls it without one.
#
# An unobserved occasion is a missing `y`, which breaks the line where
# it falls. One at the end of a series has nothing to break, and
# ggplot2 then reports removing it on every render, so missing values
# are dropped without that notice: the gap is the information and it
# is already drawn.
#'@noRd
series_time_plot <- function(dat, ylab, lines, facet = TRUE) {
  palette <- mvgam_palette()
  cut_t <- if (any(dat$data == "validate")) {
    min(dat$time[dat$data == "validate"], na.rm = TRUE)
  } else {
    NA_real_
  }
  geom_obs <- if (lines) {
    ggplot2::geom_line(linewidth = 0.75, na.rm = TRUE)
  } else {
    ggplot2::geom_point(na.rm = TRUE)
  }
  p <- ggplot2::ggplot(
    dat, ggplot2::aes(x = time, y = y, colour = data)
  ) +
    geom_obs +
    ggplot2::scale_colour_manual(
      values = c(train = palette[5L], validate = "black"),
      guide = "none"
    ) +
    mvgam_cut_layer(cut_t) +
    ggplot2::labs(title = if (!facet) "Time series",
                  x = "Time", y = ylab) +
    mvgam_theme()
  if (facet) p + mvgam_facet_series() else p
}


#'@noRd
series_hist_panel <- function(y, ylab, n_bins = NULL) {
  if (is.null(n_bins)) {
    n_bins <- max(
      length(graphics::hist(y, plot = FALSE)$breaks), 20L
    )
  }
  ggplot2::ggplot(data.frame(y = y), ggplot2::aes(x = y)) +
    ggplot2::geom_histogram(
      bins = n_bins, fill = mvgam_palette()[5L], colour = "white"
    ) +
    ggplot2::labs(title = "Histogram", x = ylab, y = "Count") +
    mvgam_theme()
}


#'@noRd
series_acf_panel <- function(y) {
  acf_y <- stats::acf(y, plot = FALSE, na.action = stats::na.pass)
  df <- data.frame(
    lag = as.numeric(acf_y$lag[, 1, 1]),
    acf = as.numeric(acf_y$acf[, , 1])
  )
  ci <- stats::qnorm(0.975) / sqrt(acf_y$n.used)
  ggplot2::ggplot(df, ggplot2::aes(x = lag, y = 0, yend = acf)) +
    ggplot2::geom_hline(
      yintercept = c(-1, 1) * ci, linetype = "dashed"
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      colour = mvgam_palette()[5L], linewidth = 0.25
    ) +
    ggplot2::geom_segment(
      colour = mvgam_palette()[5L], linewidth = 1
    ) +
    ggplot2::labs(title = "ACF", x = "Lag", y = "Autocorrelation") +
    mvgam_theme()
}


#'@noRd
series_ecdf_panel <- function(y, ylab) {
  y_clean <- y[!is.na(y)]
  rng <- range(y_clean)
  df <- data.frame(x = seq(rng[1L], rng[2L], length.out = 100L))
  df$y <- stats::ecdf(y_clean)(df$x)
  ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line(
      colour = mvgam_palette()[5L], linewidth = 0.75
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(title = "CDF", x = ylab, y = "Empirical CDF") +
    mvgam_theme()
}
