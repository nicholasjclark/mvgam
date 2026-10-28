#' Plot posterior hindcasts and forecasts
#'
#' Plot method for `mvgam_forecast` objects returned by
#' [hindcast.mvgam()] and [forecast.mvgam()]. Draws one panel per
#' series (faceted by default) with nested quantile ribbons for the
#' forecast, a single grey ribbon for the hindcast, posterior median
#' lines and the observations as points. A dashed vertical line marks
#' the boundary between training and forecast times when both are
#' drawn.
#'
#' @param x An `mvgam_forecast` object.
#' @param series Optional integer vector, character vector of
#'   series names, or `"all"`. When `NULL` (the default), every
#'   series is plotted and the panels are faceted via
#'   [ggplot2::facet_wrap()].
#' @param probs Numeric vector of credible interval widths for
#'   the forecast ribbon. Defaults to `c(0.5, 0.8, 0.95)` which
#'   draws three nested bands from the outer 95% to the inner
#'   50%. Hindcast ribbons always use the widest entry.
#' @param hindcast,forecast Logical. Whether the hindcast and the
#'   forecast are drawn. Both default to `TRUE`. Setting one to `FALSE`
#'   draws the other alone.
#' @param newdata_obs Logical. When `TRUE` (the default) and the
#'   object carries `test_observations`, the test observations
#'   are overlaid on the forecast region.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A `ggplot` object.
#'
#' @details The forecast takes the shades of the red bayesplot scheme.
#'   The hindcast band is light grey, which separates in-sample
#'   uncertainty from the nested forecast bands.
#'
#'   [forecast.mvgam()] and [hindcast.mvgam()] return the objects this
#'   method draws. Predictions that leave out the fitted trend state
#'   come from [posterior_predict.mvgam()] and
#'   [posterior_epred.mvgam()].
#'
#' @seealso [forecast.mvgam()], [hindcast.mvgam()],
#'   [score.mvgam_forecast()], [summary.mvgam_forecast()]
#'
#' @template ex-forecast-models
#' @templateVar seed 1
#' @examples
#' \dontrun{
#' fc <- forecast(mod_ar, newdata = simdat$data_test)
#'
#' # The grey hindcast band covers the training times. The red bands
#' # are the 80% and 95% forecast intervals, with the held-out counts
#' # as points
#' plot(fc, probs = c(0.8, 0.95))
#'
#' # The forecast alone
#' plot(fc, hindcast = FALSE)
#' }
#'
#' @author Nicholas J Clark
#'
#' @method plot mvgam_forecast
#' @export
plot.mvgam_forecast <- function(
  x,
  series = NULL,
  probs = c(0.5, 0.8, 0.95),
  hindcast = TRUE,
  forecast = TRUE,
  newdata_obs = TRUE,
  ...
) {
  checkmate::assert_class(x, "mvgam_forecast")
  checkmate::assert_numeric(
    probs,
    lower = 0, upper = 1, min.len = 1L, any.missing = FALSE
  )
  checkmate::assert_flag(hindcast)
  checkmate::assert_flag(forecast)
  checkmate::assert_flag(newdata_obs)
  rlang::check_dots_empty()

  if (!hindcast && !forecast) {
    stop(insight::format_error(
      "At least one of 'hindcast' or 'forecast' must be TRUE."
    ), call. = FALSE)
  }

  # The forecasts of a long-frame multivariate fit differ by response.
  # They stack into one figure, one titled plot per response.
  parts <- forecast_parts(x)
  if (is_forecast_list(parts)) {
    return(stack_response_plots(lapply(parts, function(fc) {
      plot(fc, series = series, probs = probs, hindcast = hindcast,
           forecast = forecast, newdata_obs = newdata_obs)
    })))
  }
  x <- parts
  if (isTRUE(x$site_units)) {
    stop(insight::format_error(c(
      "Plotting a jsdgam() fit along time is not supported.",
      x = "The units of a jsdgam() fit are sites, in arbitrary order.",
      i = "Use ordinate(), residual_cor() or pp_check() to view the fit."
    )), call. = FALSE)
  }

  series_levels <- as.character(x$series_names)
  series_idx <- resolve_series(series, series_levels)
  plotted_series <- series_levels[series_idx]

  have_fc <- !is.null(x$forecasts) && forecast
  have_hc <- !is.null(x$hindcasts) && hindcast

  if (!have_fc && !have_hc) {
    stop(insight::format_error(c(
      "Nothing to plot: object has neither hindcasts nor forecasts.",
      i = paste0(
        "Was the source object constructed by hindcast() / ",
        "forecast()?"
      )
    )), call. = FALSE)
  }

  # Single training-grid cut value (same across series in the
  # typical case; max captures any irregularity).
  t_cut <- if (have_fc && have_hc) {
    max(unlist(x$train_times[plotted_series]))
  } else {
    NA_real_
  }

  # The axis names the scale of the draws. A trend's state component
  # names itself.
  y_label <- if (!is.null(x$component)) {
    paste0("Latent ", component_label(x$component))
  } else {
    switch(x$type %||% "response",
      link = "Linear predictor",
      expected = "Expected value",
      trend = "Latent trend",
      "Predictions"
    )
  }
  # A plot of one series out of several names it on the axis. A fit
  # with one series has nothing to tell apart.
  if (length(plotted_series) == 1L && length(series_levels) > 1L) {
    y_label <- paste0(y_label, " for ", plotted_series)
  }

  set_color_scheme_local()
  fc_layers <- build_forecast_layers(
    x,
    plotted = plotted_series,
    probs = sort(probs, decreasing = TRUE),
    hindcast = have_hc,
    forecast = have_fc,
    newdata_obs = newdata_obs
  )
  p <- ggplot2::ggplot() +
    fc_layers +
    mvgam_cut_layer(t_cut) +
    ggplot2::labs(x = "Time", y = y_label) +
    mvgam_theme()
  if (length(plotted_series) > 1L) {
    p <- p + mvgam_facet_series()
  }
  p
}


# Resolve the user-facing series argument to an integer vector
# of indices into series_levels.
#'@noRd
resolve_series <- function(series, series_levels) {
  if (is.null(series) ||
        (length(series) == 1L &&
           is.character(series) && series == "all")) {
    return(seq_along(series_levels))
  }
  if (is.numeric(series)) {
    checkmate::assert_integerish(series, any.missing = FALSE, min.len = 1L)
    bad <- series[series < 1L | series > length(series_levels)]
    if (length(bad) > 0L) {
      stop(insight::format_error(c(
        "Series index out of range.",
        x = paste0("Got: ", paste(bad, collapse = ", "), "."),
        i = paste0(
          "The fit has ", length(series_levels), " series: ",
          paste0("'", series_levels, "'", collapse = ", "), "."
        )
      )))
    }
    return(as.integer(series))
  }
  if (is.character(series)) {
    bad <- setdiff(series, series_levels)
    if (length(bad) > 0L) {
      stop(insight::format_error(c(
        "Unknown series name(s) supplied to 'series'.",
        x = paste0(
          "Got: ", paste0("'", bad, "'", collapse = ", "), "."
        ),
        i = paste0(
          "Available: ",
          paste0("'", series_levels, "'", collapse = ", "), "."
        )
      )))
    }
    return(match(series, series_levels))
  }
  stop(insight::format_error(
    "'series' must be NULL, numeric, character or 'all'."
  ))
}


# Assemble per-series ribbon / median / observation layers.
# Returns a flat list of layer objects suitable for `+`.
#'@noRd
build_forecast_layers <- function(
  x, plotted, probs, hindcast, forecast, newdata_obs
) {
  hindcast_fill <- "grey70"
  outer_prob <- max(probs)
  layers <- list()

  for (s in plotted) {
    # A factor in the order the object holds its series. A facet over
    # a character column sorts it. The trend panels then came back
    # alphabetical while the series plot kept the model's own order.
    panel <- factor(s, levels = plotted)
    hc_mat <- if (hindcast) x$hindcasts[[s]] else NULL
    fc_mat <- if (forecast) x$forecasts[[s]] else NULL
    hc_times <- if (hindcast) x$train_times[[s]] else NULL
    fc_times <- if (forecast) x$test_times[[s]] else NULL

    # Hindcast: one grey ribbon at the widest interval with its median
    if (!is.null(hc_mat)) {
      layers <- c(layers, mvgam_band_layer(
        hc_mat, hc_times, probs = outer_prob,
        group = panel, fill = hindcast_fill
      ))
      layers <- c(
        layers,
        list(mvgam_median_layer(
          hc_mat, hc_times, colour = "grey25", group = panel
        ))
      )
    }

    # Forecast: nested ribbons with their median. With a hindcast drawn,
    # they start from its last occasion and join it.
    if (!is.null(fc_mat) && !is.null(hc_mat) && length(hc_times) > 0L) {
      last <- hc_times == max(hc_times)
      fc_mat <- cbind(hc_mat[, last, drop = FALSE], fc_mat)
      fc_times <- c(hc_times[last], fc_times)
    }
    if (!is.null(fc_mat)) {
      layers <- c(
        layers,
        mvgam_band_layer(fc_mat, fc_times, probs = probs, group = panel),
        list(mvgam_median_layer(fc_mat, fc_times, group = panel))
      )
    }

    # Observations: train + (optionally) test, on the scales they
    # share with the ribbon.
    show_obs <- on_observation_scale(x$type)
    obs_times <- c()
    obs_y <- c()
    if (!is.null(hc_mat) && show_obs) {
      obs_times <- c(obs_times, x$train_times[[s]])
      obs_y <- c(obs_y, x$train_observations[[s]])
    }
    if (!is.null(fc_mat) && newdata_obs && show_obs &&
          !is.null(x$test_observations[[s]])) {
      obs_times <- c(obs_times, x$test_times[[s]])
      obs_y <- c(obs_y, x$test_observations[[s]])
    }
    if (length(obs_y) > 0L) {
      layers <- c(
        layers,
        mvgam_obs_layer(
          times = obs_times,
          y = obs_y,
          group = rep(panel, length(obs_y))
        )
      )
    }
  }
  layers
}
