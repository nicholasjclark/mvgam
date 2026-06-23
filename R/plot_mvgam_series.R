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
#'   `object$test_data` if persisted (forward-compatible with
#'   `mvgam()` newdata persistence; currently always NULL).
#' @param series `NULL` (default), `"all"`, or a positive
#'   integer indexing the series levels. `NULL` resolves to
#'   `"all"` for multi-series fits and `1` for single-series.
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
  lines = TRUE,
  n_bins = NULL,
  log_scale = FALSE
) {
  checkmate::assert_class(object, "mvgam")

  meta <- object$trend_metadata$variables %||%
    list(time_var = "time", series_var = "series")
  resp <- (object$response_names %||% "y")[1L]
  series_levels <- resolve_series_info(object)$series_levels

  series_obs_plot(
    train         = mvgam_training_data(object),
    test          = newdata %||% object$test_data,
    response      = resp,
    meta          = meta,
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
# callers pass already-resolved metadata so this helper does no
# class dispatch.
#'@noRd
series_obs_plot <- function(train, test, response, meta,
                             series_levels, series = NULL,
                             lines = TRUE, n_bins = NULL,
                             log_scale = FALSE) {
  checkmate::assert_flag(lines)
  checkmate::assert_flag(log_scale)
  checkmate::assert_integerish(n_bins, lower = 1L, len = 1L,
                                null.ok = TRUE)
  set_color_scheme_local("red")

  series_idx <- resolve_series_index(series, length(series_levels))

  dat <- rbind(
    series_long_df(train, response, meta, label = "train"),
    series_long_df(test,  response, meta, label = "validate")
  )
  dat$series <- factor(dat$series, levels = series_levels)

  ylab <- if (log_scale) paste0("log(", response, " + 1)") else response
  if (log_scale) dat$y <- log(dat$y + 1)

  if (identical(series_idx, "all")) {
    return(series_all_plot(dat, ylab, lines))
  }
  s_name <- series_levels[series_idx]
  dat_s <- dat[as.character(dat$series) == s_name, , drop = FALSE]
  if (nrow(dat_s) == 0L) {
    stop(insight::format_error(c(
      "No observations found for the requested series.",
      x = paste0("Series: '", s_name, "'.")
    )))
  }
  patchwork::wrap_plots(
    series_ts_panel(dat_s, ylab, lines),
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


# Internal: pull (time, y, series, data) from a training or test
# data frame using the canonical column names recorded on the
# fit. Returns NULL when `df` is NULL so callers can `rbind` it
# unconditionally.
#'@noRd
series_long_df <- function(df, resp, meta, label) {
  if (is.null(df)) {
    return(NULL)
  }
  if (!resp %in% names(df)) {
    stop(insight::format_error(c(
      "Response variable not found in data.",
      x = paste0("Expected: '", resp, "'."),
      i = paste0("Got columns: ",
                 paste0("'", names(df), "'", collapse = ", "), ".")
    )))
  }
  series_vec <- if (!is.null(meta$series_var) &&
                     meta$series_var %in% names(df)) {
    df[[meta$series_var]]
  } else {
    factor(rep("series1", length(df[[resp]])))
  }
  data.frame(
    time = df[[meta$time_var %||% "time"]],
    y = df[[resp]],
    series = series_vec,
    data = label,
    stringsAsFactors = FALSE
  )
}


# Internal: faceted multi-series time-series plot. One panel
# per series; training observations in the active palette,
# test observations (when present) in black with a dashed cut.
#'@noRd
series_all_plot <- function(dat, ylab, lines) {
  palette <- mvgam_palette()
  cut_t <- if (any(dat$data == "validate")) {
    min(dat$time[dat$data == "validate"], na.rm = TRUE)
  } else {
    NA_real_
  }
  geom_obs <- if (lines) {
    ggplot2::geom_line(linewidth = 0.75)
  } else {
    ggplot2::geom_point()
  }
  ggplot2::ggplot(
    dat, ggplot2::aes(x = time, y = y, colour = data)
  ) +
    ggplot2::facet_wrap(~series) +
    geom_obs +
    ggplot2::scale_colour_manual(
      values = c(train = palette[5L], validate = "black"),
      guide = "none"
    ) +
    mvgam_cut_layer(cut_t) +
    ggplot2::labs(x = "Time", y = ylab) +
    mvgam_theme()
}


# Internal: per-series time-series panel (used in the 4-panel
# single-series view). Same colour rules as `series_all_plot`
# but no facet.
#'@noRd
series_ts_panel <- function(dat, ylab, lines) {
  palette <- mvgam_palette()
  cut_t <- if (any(dat$data == "validate")) {
    min(dat$time[dat$data == "validate"], na.rm = TRUE)
  } else {
    NA_real_
  }
  geom_obs <- if (lines) {
    ggplot2::geom_line(linewidth = 0.75)
  } else {
    ggplot2::geom_point()
  }
  ggplot2::ggplot(
    dat, ggplot2::aes(x = time, y = y, colour = data)
  ) +
    geom_obs +
    ggplot2::scale_colour_manual(
      values = c(train = palette[5L], validate = "black"),
      guide = "none"
    ) +
    mvgam_cut_layer(cut_t) +
    ggplot2::labs(title = "Time series", x = "Time", y = ylab) +
    mvgam_theme()
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
