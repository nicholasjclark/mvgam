#' Plot posterior hindcasts and forecasts
#'
#' Plot method for `mvgam_forecast` objects returned by
#' [hindcast.mvgam()] and [forecast.mvgam()]. Renders one panel
#' per series (faceted by default) with a multi-quantile ribbon
#' for the forecast arm, a single grey ribbon for the hindcast
#' arm, posterior median lines, and observed-points overlays.
#' A dashed vertical line marks the training / forecast boundary
#' when both arms are present.
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
#' @param hindcast,forecast Logical toggles controlling which
#'   arm is drawn. Both default to `TRUE`; setting `forecast =
#'   FALSE` produces a hindcast-only panel and vice versa.
#' @param newdata_obs Logical. When `TRUE` (the default) and the
#'   object carries `test_observations`, the test observations
#'   are overlaid on the forecast region.
#' @param ... Currently unused; reserved for future arguments.
#'
#' @return A `ggplot` object.
#'
#' @details The plot palette is read lazily from
#'   `bayesplot::color_scheme_get()` so a user-side
#'   `bayesplot::color_scheme_set()` call composes, so changing the
#'   bayesplot scheme to retheme every panel without touching
#'   the call. The hindcast band is plotted in light grey to
#'   distinguish in-sample uncertainty from the multi-band
#'   forecast region.
#'
#'   For state-extrapolating prediction over arbitrary newdata
#'   see [forecast.mvgam()] and [hindcast.mvgam()]; for
#'   marginal-MC prediction see [posterior_predict.mvgam()] /
#'   [posterior_epred.mvgam()].
#'
#' @seealso [forecast.mvgam()], [hindcast.mvgam()],
#'   [score.mvgam_forecast()], [summary.mvgam_forecast()],
#'   [irf()], [fevd()], [stability()]
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

  if (!hindcast && !forecast) {
    stop(insight::format_error(
      "At least one of 'hindcast' or 'forecast' must be TRUE."
    ))
  }

  # Multi-response fan-out wrapper: `x` is a named list keyed
  # by response, each element itself an `mvgam_forecast`.
  #
  # On a wide frame the responses are the series and each holds its
  # own latent state, so the elements are the panels of one plot and
  # are drawn as one forecast over the response axis. Taking the
  # first element as standing for all of them drew one response's
  # state in every panel.
  #
  # On a long frame every response reads the one state its series
  # has, so a trend-scale wrapper does repeat one trajectory and the
  # first arm is drawn alone. Response-scale arms differ per outcome;
  # when `patchwork` is available we return a side-by-side stack,
  # otherwise we print each per-arm plot in turn and invisibly return
  # the raw list.
  if (isTRUE(attr(x, "response_keyed"))) {
    return(plot(response_axis_forecast(x), series = series,
                probs = probs, hindcast = hindcast,
                forecast = forecast, newdata_obs = newdata_obs, ...))
  }
  if (isTRUE(attr(x, "mv_wrapper"))) {
    plot_arm <- function(arm, title = NULL) {
      p <- plot(arm, series = series, probs = probs,
                 hindcast = hindcast, forecast = forecast,
                 newdata_obs = newdata_obs, ...)
      if (is.null(title)) p else p + ggplot2::labs(title = title)
    }
    if (is_trend_forecast(x[[1L]])) return(plot_arm(x[[1L]]))
    per_arm <- Map(plot_arm, x, names(x))
    if (requireNamespace("patchwork", quietly = TRUE)) {
      return(Reduce(`+`, per_arm))
    }
    for (p in per_arm) print(p)
    return(invisible(per_arm))
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
    )))
  }

  # Single training-grid cut value (same across series in the
  # typical case; max captures any irregularity).
  t_cut <- if (have_fc && have_hc) {
    max(unlist(x$train_times[plotted_series]))
  } else {
    NA_real_
  }

  y_label <- if (length(plotted_series) == 1L) {
    paste0("Predictions for ", plotted_series)
  } else {
    "Predictions"
  }

  # Build the plot under the mvgam palette so the ribbon and
  # median colours are read from the 'red' scheme rather than
  # bayesplot's default 'blue'. The prior scheme is restored when
  # this function returns so user-side `color_scheme_set` calls
  # compose.
  set_color_scheme_local("red")
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


# Return TRUE when the forecast object is on the latent trend
# scale, i.e. `hindcast(fit, type = "trend")` or
# `forecast(fit, type = "trend")`. Used by the mv wrapper and
# by the per-series observation-overlay guard.
#'@noRd
is_trend_forecast <- function(x) identical(x$type, "trend")


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
    checkmate::assert_integerish(
      series,
      lower = 1L, upper = length(series_levels),
      any.missing = FALSE, min.len = 1L
    )
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
    "'series' must be NULL, numeric, character, or 'all'."
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
    # A factor in the order the object holds its series, since a facet
    # over a character column sorts it: the trend panels came back
    # alphabetical beside a series plot in the model's own order.
    panel <- factor(s, levels = plotted)
    hc_mat <- if (hindcast) x$hindcasts[[s]] else NULL
    fc_mat <- if (forecast) x$forecasts[[s]] else NULL
    hc_times <- if (hindcast) x$train_times[[s]] else NULL
    fc_times <- if (forecast) x$test_times[[s]] else NULL

    # Hindcast arm: single outer-prob grey ribbon plus median.
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

    # Forecast arm: full multi-band ribbon plus median.
    if (!is.null(fc_mat)) {
      layers <- c(
        layers,
        mvgam_band_layer(fc_mat, fc_times, probs = probs, group = panel),
        list(mvgam_median_layer(fc_mat, fc_times, group = panel))
      )
    }

    # Observations: train + (optionally) test.
    # Skipped for trend hindcasts / forecasts because the
    # ribbon is on the latent-process scale, but the stored
    # `train_observations` / `test_observations` are on the
    # response scale; overlaying them produces a scale
    # mismatch that squashes the ribbon flat against the axis.
    show_obs <- !is_trend_forecast(x)
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
