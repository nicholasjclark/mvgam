#' Seasonal patterns of a fitted local linear trend
#'
#' @description
#' Lays out the posterior season of an [LLT()] trend by position
#' within its period. It reports the size of the seasonal swing and
#' where in the period it peaks, cycle by cycle. With `newdata` the
#' season continues into the forecast horizon. The season is part of
#' the latent trend, on the link scale.
#'
#' @param object A fitted `mvgam` object whose trend is
#'   `LLT(season = ...)`.
#' @param newdata Optional data frame of future times, as
#'   [forecast.mvgam()] takes it. The forecast season then extends each
#'   series.
#' @param ndraws,draw_ids The posterior draws to use, as
#'   [hindcast.mvgam()] takes them. `NULL` for both uses every draw.
#' @param ... Unused. Anything passed here is refused.
#'
#' @details
#' *Positions*. A position counts steps within one period. Data from
#' [series_to_mvgam()] carry a `season` column, or one `season_<period>`
#' column per period, counted from the calendar. Position 1 of a
#' twelve-month period is then January. A twelve-step period is
#' labelled by month and a four-step one by quarter. Without the
#' column, position 1 is the first fitted time and later times continue
#' from it.
#'
#' *Cycles*. A cycle is one pass through the period. A cycle that holds
#' every position is complete. Only complete cycles are summarised.
#'
#' *Amplitude and peak*. The amplitude of a cycle is half the range of
#' the season within it. Its peak is the position of the largest
#' value. Both are computed draw by draw. A change between the first
#' and the last complete cycle then comes with its posterior
#' probability.
#'
#' A trend with several periods, such as `LLT(season = c(7, 365.25))`,
#' reports each period's season separately.
#'
#' @return An object of class `mvgam_seasonality`. `print()` gives each
#'   series' amplitude and peak in its first and last complete cycles.
#'   `summary()` returns four tables: `profile`, `amplitude`, `peak` and
#'   `change`, the last comparing the first and last complete cycles.
#'   `plot()` draws the profile by position, or one panel per position
#'   over the cycles.
#'
#' @seealso [LLT()] for the seasonal model, [series_to_mvgam()] for
#'   the calendar positions, [forecast.mvgam()] with
#'   `component = "season"` for the season along time
#'
#' @examples
#' \dontrun{
#' # Monthly airline passengers, the final year held out
#' air <- series_to_mvgam(AirPassengers, h = 12)
#' mod <- mvgam(
#'   y ~ 1,
#'   trend_formula = ~ LLT(season = 12, harmonics = 3),
#'   data = air$data_train,
#'   family = poisson(),
#'   chains = 2,
#'   silent = 2
#' )
#'
#' # Amplitude and peak month, first and last complete year
#' s <- seasonality(mod, newdata = air$data_test)
#' s
#'
#' # The season by month in the first, last and forecast years
#' plot(s)
#'
#' # Each month's effect over the years and into the forecast
#' plot(s, type = "positions")
#'
#' # How the swing and its peak changed between the first and last year
#' summary(s)$change
#' }
#'
#' @author Nicholas J Clark
#' @export
seasonality <- function(object, ...) {
  UseMethod("seasonality")
}


#' @rdname seasonality
#' @method seasonality mvgam
#' @export
seasonality.mvgam <- function(object, newdata = NULL, ...,
                              ndraws = NULL, draw_ids = NULL) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_data_frame(newdata, null.ok = TRUE)
  rlang::check_dots_empty()
  validate_draw_selectors(ndraws, draw_ids)
  require_fitted_model(object, "seasonality")
  require_series_axis(object, "seasonality")
  periods <- require_llt_season(object)
  draw_ids <- resolve_draw_indices(posterior::ndraws(object), ndraws,
                                   draw_ids)
  time_var <- mvgam_axes(object)$vars$time_var

  # Each period takes one hindcast. With newdata it takes one forecast.
  per_period <- lapply(seq_len(nrow(periods)), function(p) {
    cmp <- periods$component[p]
    hc <- hindcast(object, type = "trend", component = cmp,
                   draw_ids = draw_ids)
    fc <- if (!is.null(newdata)) {
      forecast(object, newdata = newdata, type = "trend", component = cmp,
               draw_ids = draw_ids)
    }
    fitted <- shared_series_times(hc$train_times)
    times <- c(fitted, if (!is.null(fc)) shared_series_times(fc$test_times))
    list(
      draws = lapply(stats::setNames(nm = names(hc$hindcasts)), function(s) {
        cbind(hc$hindcasts[[s]], fc$forecasts[[s]])
      }),
      layout = season_layout(times, n_fitted = length(fitted),
                             period = periods$period[p],
                             frames = list(object$data, newdata),
                             column = cmp, time_var = time_var)
    )
  })
  names(per_period) <- periods$component
  structure(
    list(
      periods = periods[c("period", "component")],
      series = names(per_period[[1L]]$draws),
      seasons = lapply(per_period, `[[`, "draws"),
      layouts = lapply(per_period, `[[`, "layout"),
      draw_ids = draw_ids
    ),
    class = "mvgam_seasonality"
  )
}


#' The seasonal periods of a fit, or an error naming what is needed
#'
#' @param object A fitted `mvgam` object
#' @return Data frame from `llt_season_periods()`
#' @noRd
require_llt_season <- function(object) {
  periods <- if (identical(get_trend_type(object), "LLT")) {
    llt_season_periods(first_trend_spec(object))
  }
  if (NROW(periods) == 0L) {
    stop(insight::format_error(c(
      "'seasonality()' requires an 'LLT()' trend with a season.",
      i = "Fit with a season, as in 'trend_formula = ~ LLT(season = 12)'."
    )), call. = FALSE)
  }
  periods
}


#' The times every series shares
#'
#' A local linear trend steps every series on one regular grid.
#'
#' @param times List of time vectors, one per series
#' @return Numeric vector of times
#' @noRd
shared_series_times <- function(times) {
  checkmate::assert_list(times, min.len = 1L)
  first <- times[[1L]]
  if (!all(vapply(times, identical, logical(1L), first))) {
    stop_mvgam_fault(
      "Every series of a local linear trend must share its times.",
      "The hindcast or forecast holds series with different times."
    )
  }
  as.numeric(first)
}


#' The position, cycle and label of each time within a period
#'
#' A frame holding the period's calendar column gives the position of
#' each of its times. Other times continue from the nearest earlier
#' position, one step per time step. Without the column anywhere,
#' position 1 is the first time.
#'
#' @param times Sorted numeric times, the fitted ones first
#' @param n_fitted Number of fitted times
#' @param period The period in time steps
#' @param frames List of data frames that may hold the calendar column
#' @param column The calendar column's name
#' @param time_var The time column's name
#' @return Data frame of `time`, `position`, `label`, `cycle`,
#'   `complete` and `horizon`, with a `labels` attribute holding the
#'   label of every position in order
#' @noRd
season_layout <- function(times, n_fitted, period, frames, column,
                          time_var) {
  checkmate::assert_numeric(times, any.missing = FALSE, min.len = 1L,
                            sorted = TRUE)
  checkmate::assert_int(n_fitted, lower = 1L, upper = length(times))
  checkmate::assert_number(period, lower = 2)
  checkmate::assert_string(column)
  checkmate::assert_string(time_var)
  n_pos <- ceiling(period)
  known <- do.call(rbind, lapply(Filter(Negate(is.null), frames), function(f) {
    if (!all(c(column, time_var) %in% names(f))) return(NULL)
    unique(data.frame(time = as.numeric(f[[time_var]]),
                      position = as.integer(f[[column]])))
  }))
  calendar <- NROW(known) > 0L
  position <- rep(NA_integer_, length(times))
  if (calendar) {
    position <- known$position[match(times, known$time)]
  }
  # A time the frames do not place continues one step on from the last
  # placed time, or starts at position 1
  for (i in seq_along(times)) {
    if (!is.na(position[i])) next
    position[i] <- if (i == 1L) {
      1L
    } else {
      as.integer((position[i - 1L] + times[i] - times[i - 1L] - 1) %%
                   n_pos + 1)
    }
  }
  labels <- season_position_labels(n_pos, calendar)
  cycle <- 1L + cumsum(c(0L, diff(position) < 0L))
  # A non-integer period skips its last position in some cycles
  holds <- tapply(position, cycle, function(p) length(unique(p)))
  out <- data.frame(
    time = times,
    position = position,
    label = factor(labels[position], levels = labels),
    cycle = cycle,
    complete = as.vector(holds[as.character(cycle)]) >= floor(period),
    horizon = factor(ifelse(seq_along(times) <= n_fitted, "fitted",
                            "forecast"),
                     levels = c("fitted", "forecast"))
  )
  attr(out, "labels") <- labels
  out
}


#' The label of each position within a period
#'
#' A twelve-step period whose positions come from a calendar column
#' takes month labels. A four-step one takes quarters. Other periods
#' take their position numbers.
#'
#' @param n_pos Number of positions
#' @param calendar Whether the positions come from a calendar column
#' @return Character vector of `n_pos` labels
#' @noRd
season_position_labels <- function(n_pos, calendar) {
  checkmate::assert_count(n_pos, positive = TRUE)
  checkmate::assert_flag(calendar)
  if (calendar && n_pos == 12L) return(month.abb)
  if (calendar && n_pos == 4L) return(paste0("Q", 1:4))
  as.character(seq_len(n_pos))
}


#' The amplitude and peak of each complete cycle, draw by draw
#'
#' The amplitude is half the range of the season within a cycle. The
#' peak is the position of its largest value.
#'
#' @param draws `[ndraws, n_times]` matrix of the season
#' @param layout Frame from `season_layout()`
#' @return List of `[ndraws, n_complete_cycles]` matrices `amplitude`
#'   and `peak`, columns named by cycle
#' @noRd
cycle_amplitude_peak <- function(draws, layout) {
  checkmate::assert_matrix(draws, ncols = nrow(layout))
  cycles <- unique(layout$cycle[layout$complete])
  amplitude <- peak <- matrix(NA_real_, nrow(draws), length(cycles),
                              dimnames = list(NULL, cycles))
  for (j in seq_along(cycles)) {
    cols <- which(layout$cycle == cycles[j])
    sub <- draws[, cols, drop = FALSE]
    top <- max.col(sub, ties.method = "first")
    amplitude[, j] <- (sub[cbind(seq_len(nrow(sub)), top)] +
                         row_max(-sub)) / 2
    peak[, j] <- layout$position[cols][top]
  }
  list(amplitude = amplitude, peak = peak)
}


#' The most probable position and its posterior probability
#'
#' @param pos Numeric vector of positions over draws
#' @return List of `position` and `prob`
#' @noRd
modal_position <- function(pos) {
  tab <- table(pos)
  list(position = as.integer(names(tab)[which.max(tab)]),
       prob = max(tab) / length(pos))
}


#' The signed steps from one position to another, the shorter way
#' around a period
#'
#' @param from,to Numeric vectors of positions
#' @param n_pos Number of positions in the period
#' @return Numeric vector, positive when `to` falls later
#' @noRd
circular_shift <- function(from, to, n_pos) {
  half <- n_pos / 2
  ((to - from + half) %% n_pos) - half
}


#' The draws, amplitudes and peaks of every series and period
#'
#' `summary()` and `print()` take every table from this one pass.
#'
#' @param x An `mvgam_seasonality` object
#' @return Nested list by period then series. Each element holds the
#'   season's `draws`, the `[ndraws, n_cycles]` matrices `amplitude` and
#'   `peak` and the period's `layout`.
#' @noRd
season_cycle_draws <- function(x) {
  lapply(stats::setNames(nm = x$periods$component), function(cmp) {
    layout <- x$layouts[[cmp]]
    lapply(x$seasons[[cmp]], function(draws) {
      c(list(draws = draws, layout = layout),
        cycle_amplitude_peak(draws, layout))
    })
  })
}


#' One block of rows per series and period, stacked into a tibble
#'
#' @param cycle_draws Output of `season_cycle_draws()`
#' @param f Function of one element of `cycle_draws`, returning a data
#'   frame
#' @return A tibble led by `series` and `period` columns
#' @noRd
stack_season_rows <- function(cycle_draws, f) {
  blocks <- unlist(lapply(names(cycle_draws), function(cmp) {
    lapply(names(cycle_draws[[cmp]]), function(s) {
      out <- f(cycle_draws[[cmp]][[s]])
      cbind(series = rep(s, nrow(out)), period = rep(cmp, nrow(out)), out)
    })
  }), recursive = FALSE)
  tibble::as_tibble(do.call(rbind, blocks))
}


#' The cycle, start time and horizon of each column of a cycle statistic
#' @noRd
cycle_rows <- function(stat, layout) {
  cycles <- as.integer(colnames(stat))
  first <- match(cycles, layout$cycle)
  data.frame(cycle = cycles, start = layout$time[first],
             horizon = layout$horizon[first])
}


#' The most probable peak of each complete cycle
#' @noRd
season_peak_rows <- function(el) {
  modes <- lapply(seq_len(ncol(el$peak)), function(j) {
    modal_position(el$peak[, j])
  })
  positions <- vapply(modes, `[[`, 1L, "position")
  cbind(cycle_rows(el$peak, el$layout),
        peak = attr(el$layout, "labels")[positions],
        prob = vapply(modes, `[[`, numeric(1L), "prob"))
}


#' The change from the first to the last complete cycle
#'
#' The peak's shift takes the shorter way around the period. A
#' flat-topped season peaks at neighbouring positions in different
#' draws. The median and interval of the shift, with the probabilities
#' of an earlier and of a later peak, carry that spread.
#'
#' @param el One element of `season_cycle_draws()`
#' @param probs Interval probabilities
#' @return One-row data frame
#' @noRd
season_change_row <- function(el, probs) {
  amp <- el$amplitude
  j <- ncol(amp)
  shift <- circular_shift(el$peak[, 1L], el$peak[, j],
                          length(attr(el$layout, "labels")))
  cbind(
    data.frame(from_cycle = as.integer(colnames(amp)[1L]),
               to_cycle = as.integer(colnames(amp)[j])),
    median_interval_cols(amp[, j, drop = FALSE] / amp[, 1L, drop = FALSE],
                         probs, "ratio_"),
    data.frame(prob_larger = mean(amp[, j] > amp[, 1L])),
    median_interval_cols(matrix(shift), probs, "shift_"),
    data.frame(prob_earlier = mean(shift < 0), prob_later = mean(shift > 0))
  )
}


#' @param probs Interval probabilities of the tables. Defaults to
#'   `c(0.025, 0.975)`.
#' @rdname seasonality
#' @method summary mvgam_seasonality
#' @export
summary.mvgam_seasonality <- function(object, probs = c(0.025, 0.975),
                                      ...) {
  checkmate::assert_numeric(probs, lower = 0, upper = 1, len = 2L,
                            any.missing = FALSE)
  rlang::check_dots_empty()
  cycle_draws <- season_cycle_draws(object)
  list(
    profile = stack_season_rows(cycle_draws, function(el) {
      cbind(el$layout[c("time", "cycle", "label", "horizon")],
            median_interval_cols(el$draws, probs, ""))
    }),
    amplitude = stack_season_rows(cycle_draws, function(el) {
      cbind(cycle_rows(el$amplitude, el$layout),
            median_interval_cols(el$amplitude, probs, ""))
    }),
    peak = stack_season_rows(cycle_draws, season_peak_rows),
    change = stack_season_rows(cycle_draws, function(el) {
      season_change_row(el, probs)
    })
  )
}


#' @param digits Number of decimal places to print. Defaults to `2`.
#' @rdname seasonality
#' @method print mvgam_seasonality
#' @export
print.mvgam_seasonality <- function(x, digits = 2L, ...) {
  checkmate::assert_count(digits)
  cycle_draws <- season_cycle_draws(x)
  num <- function(v) format(round(v, digits), nsmall = digits)
  cat("Seasonal pattern of the latent trend (link scale), ",
      length(x$draw_ids), " draws\n", sep = "")
  cat("First and last complete cycles. summary() gives every cycle.\n")
  for (cmp in names(cycle_draws)) {
    period <- x$periods$period[x$periods$component == cmp]
    cat("\nPeriod ", format(period), " (", cmp, ")\n", sep = "")
    for (s in names(cycle_draws[[cmp]])) {
      el <- cycle_draws[[cmp]][[s]]
      amp <- apply(el$amplitude, 2L, stats::median)
      pk <- season_peak_rows(el)
      ch <- season_change_row(el, c(0.025, 0.975))
      ends <- c(1L, length(amp))
      cat("  ", s, "\n",
          "    amplitude ", paste(num(amp[ends]), collapse = " -> "),
          ", P(larger) = ", num(ch$prob_larger), "\n",
          "    peak ", paste0(pk$peak[ends], " (", num(pk$prob[ends]), ")",
                            collapse = " -> "),
          ", P(earlier) = ", num(ch$prob_earlier),
          ", P(later) = ", num(ch$prob_later), "\n",
          sep = "")
    }
  }
  invisible(x)
}


#' @param x An `mvgam_seasonality` object from [seasonality()].
#' @param type `"profile"` (the default) draws the season against
#'   position for a few cycles, one panel per series. `"positions"`
#'   draws one panel per position, the season over the cycles.
#' @param period Which period to draw when the trend has several, by
#'   its name such as `"season_7"`. Defaults to the first.
#' @param series For `type = "positions"`, the series to draw, by name
#'   or index. Defaults to the first.
#' @param cycles For `type = "profile"`, the cycles to draw. Defaults to
#'   the first and last complete fitted cycles and the first complete
#'   forecast cycle.
#' @param prob For `type = "profile"`, the width of the posterior
#'   interval. Defaults to `0.9`. `type = "positions"` draws the nested
#'   intervals of [plot.mvgam_forecast()].
#' @rdname seasonality
#' @method plot mvgam_seasonality
#' @export
plot.mvgam_seasonality <- function(x, type = c("profile", "positions"),
                                   period = NULL, series = NULL,
                                   cycles = NULL, prob = 0.9, ...) {
  type <- match.arg(type)
  checkmate::assert_choice(period, x$periods$component, null.ok = TRUE)
  checkmate::assert_integerish(cycles, lower = 1L, any.missing = FALSE,
                               min.len = 1L, null.ok = TRUE)
  checkmate::assert_number(prob, lower = 0, upper = 1)
  rlang::check_dots_empty()
  cmp <- period %||% x$periods$component[1L]
  layout <- x$layouts[[cmp]]
  set_color_scheme_local()
  if (type == "profile") {
    probs <- c((1 - prob) / 2, 1 - (1 - prob) / 2)
    return(season_profile_plot(x, cmp, layout, cycles, probs))
  }
  s <- x$series[resolve_series(series %||% 1L, x$series)]
  if (length(s) != 1L) {
    stop(insight::format_error(
      "'type = \"positions\"' draws one series at a time."
    ), call. = FALSE)
  }
  season_positions_plot(x$seasons[[cmp]][[s]], layout, s)
}


#' The season against position, one line per chosen cycle
#' @noRd
season_profile_plot <- function(x, cmp, layout, cycles, probs) {
  complete <- unique(layout[layout$complete, c("cycle", "horizon")])
  fitted <- complete$cycle[complete$horizon == "fitted"]
  ahead <- complete$cycle[complete$horizon == "forecast"]
  cycles <- cycles %||% unique(c(utils::head(fitted, 1L),
                                 utils::tail(fitted, 1L),
                                 utils::head(ahead, 1L)))
  unknown <- setdiff(cycles, complete$cycle)
  if (length(unknown)) {
    stop(insight::format_error(c(
      "Each of 'cycles' must hold every position of the period.",
      x = paste0("Cycle ", unknown[1L], " misses a position."),
      i = paste0("Complete cycles: ",
                 paste(complete$cycle, collapse = ", "), ".")
    )), call. = FALSE)
  }
  df <- do.call(rbind, lapply(x$series, function(s) {
    do.call(rbind, lapply(cycles, function(k) {
      cols <- which(layout$cycle == k)
      q <- median_interval_cols(x$seasons[[cmp]][[s]][, cols, drop = FALSE],
                                probs, "")
      names(q) <- c("med", "lower", "upper")
      ahead <- layout$horizon[cols[1L]] == "forecast"
      cbind(series = factor(s, levels = x$series),
            label = layout$label[cols],
            cycle = paste0("Cycle ", k, if (ahead) " (forecast)"),
            q)
    }))
  }))
  df$cycle <- factor(df$cycle, levels = unique(df$cycle))
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$label,
                                        group = .data$cycle)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower,
                                      ymax = .data$upper,
                                      fill = .data$cycle),
                         alpha = 0.25) +
    ggplot2::geom_line(ggplot2::aes(y = .data$med, colour = .data$cycle),
                       linewidth = 1) +
    mvgam_model_colour_scale(nlevels(df$cycle), name = NULL,
                             aesthetics = c("colour", "fill")) +
    reference_line(yintercept = 0) +
    ggplot2::labs(x = "Position in period", y = "Latent season") +
    mvgam_theme()
  if (length(x$series) > 1L) {
    p <- p + mvgam_facet_series()
  }
  p
}


#' One panel per position: the season over the cycles
#'
#' Each position is drawn as `plot.mvgam_forecast()` draws a series,
#' with the cycles as its times: a grey band over the fitted cycles,
#' nested bands over the forecast cycles and a dashed line between.
#'
#' @param draws `[ndraws, n_times]` matrix of one series' season
#' @param layout Frame from `season_layout()`
#' @param series The series' name, for the axis
#' @return A `ggplot` object
#' @noRd
season_positions_plot <- function(draws, layout, series) {
  labels <- levels(layout$label)
  by_position <- function(horizon) {
    lapply(stats::setNames(nm = labels), function(l) {
      draws[, layout$label == l & layout$horizon == horizon, drop = FALSE]
    })
  }
  cycles_of <- function(horizon) {
    lapply(stats::setNames(nm = labels), function(l) {
      layout$cycle[layout$label == l & layout$horizon == horizon]
    })
  }
  ahead <- any(layout$horizon == "forecast")
  panels <- list(
    type = "trend",
    hindcasts = by_position("fitted"),
    train_times = cycles_of("fitted"),
    forecasts = if (ahead) by_position("forecast"),
    test_times = if (ahead) cycles_of("forecast")
  )
  t_cut <- if (ahead) max(unlist(panels$train_times))
  ggplot2::ggplot() +
    build_forecast_layers(panels, plotted = labels,
                          probs = c(0.95, 0.8, 0.5), hindcast = TRUE,
                          forecast = ahead, newdata_obs = FALSE) +
    mvgam_cut_layer(t_cut) +
    mvgam_facet_series(scales = "fixed") +
    scale_x_steps(name = "Cycle") +
    ggplot2::labs(y = paste0("Latent season for ", series)) +
    mvgam_theme()
}
