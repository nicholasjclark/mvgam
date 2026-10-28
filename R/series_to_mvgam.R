#' Convert time series objects to mvgam long format
#'
#' Convert a `tsibble`, a `ts`, `mts` or `msts` object, or a wide
#' `data.frame` with a date column into the long `data.frame` that
#' [mvgam()] expects: one row per series and time, a `series` factor
#' and a `time` column counting steps from the first index value.
#'
#' @param series The data to convert. One of:
#'   \itemize{
#'     \item A `tsibble` (`tsibble::tsibble()`). Its key variables
#'       define the series and its index defines the time grid. Gaps in
#'       a regular `tsibble` become rows with a missing response, as
#'       `tsibble::fill_gaps(.full = TRUE)` builds them. Measured
#'       variables other than the response are carried over as
#'       covariates, with missing values on those rows. An irregular
#'       `tsibble` keeps its own times, for use with `CAR()` or
#'       `ZMVN()`.
#'     \item A `ts`, `mts` or `msts` object (`forecast::msts()`). Each
#'       column of an `mts` is a series.
#'     \item A wide `data.frame` holding one date column and one
#'       numeric column per series. Column names become the series
#'       levels, in column order.
#'   }
#' @param freq Optional numeric vector of seasonal periods, in time
#'   steps. Each period above 1 adds a column giving the position of
#'   each row within that period. Defaults to the periods the object
#'   carries: `frequency()` of a `ts`, the `seasonal.periods` of an
#'   `msts`, or `tsibble::guess_frequency()` of the index.
#' @param response Name of the measured variable to use as the response
#'   of a `tsibble`. Optional when the `tsibble` has one measured
#'   variable. Ignored for other inputs, whose response column is `y`.
#' @param index Name of the date column of a wide `data.frame`. Optional
#'   when exactly one column has class `Date`, `POSIXt`, `yearmonth`,
#'   `yearquarter` or `yearweek`.
#' @param h Optional number of final time steps to hold out as test
#'   data.
#' @param train_prop Optional proportion of time steps to keep as
#'   training data, between 0 and 1. Only one of `h` and `train_prop`
#'   may be given.
#'
#' @details `time` counts steps from 1 at the first index value. The
#'   position within a seasonal period counts steps from the calendar
#'   origin of the index. A monthly index with `freq = 12` gives the
#'   month number whatever month the data start in. A `ts` gives
#'   `cycle()`. A single period produces a column named `season`. Several
#'   periods produce `season_<period>` columns, such as `season_7` and
#'   `season_365.25`.
#'
#'   Rows for future times with a missing response, built for example
#'   with `tsibble::new_data()` and bound to the observed data, receive
#'   `time` values that continue the grid. Converting observed and
#'   future rows together keeps the two aligned.
#'
#'   Seasonal periods take the same numbers across packages. Monthly
#'   data carry `frequency = 12` in a `ts`. A fable
#'   `season(period = "year")` on a monthly `tsibble` resolves to 12
#'   steps. Both take `freq = 12`. Daily data with
#'   `seasonal.periods = c(7, 365.25)` in an `msts` take
#'   `freq = c(7, 365.25)`.
#'
#'   A `season` column feeds a fixed seasonal shape, such as
#'   `s(season, bs = "cc")`. The same periods give [LLT()] a season
#'   whose shape drifts over the years, as
#'   `trend_formula = ~ LLT(season = 12)`.
#'
#' @return A long `data.frame`. Its columns are `series` (a factor),
#'   `time` (an integer for regular data), the response and the original
#'   index (`index` for a `ts`). Any `tsibble` key and covariate columns
#'   follow, then one season column per period above 1. Rows are ordered
#'   by time,
#'   then series. With `h` or `train_prop`, a list holding `data_train`
#'   and `data_test` instead.
#'
#' @examples
#' # Monthly airline passengers, holding out the final year
#' air <- series_to_mvgam(AirPassengers, h = 12)
#' head(air$data_train)
#'
#' # Two series in one mts
#' deaths <- series_to_mvgam(cbind(male = mdeaths, female = fdeaths))
#' levels(deaths$series)
#'
#' # A wide data.frame with a date column
#' wide <- data.frame(
#'   date = seq(as.Date("2024-01-01"), by = "day", length.out = 28),
#'   site_a = rpois(28, 5),
#'   site_b = rpois(28, 8)
#' )
#' head(series_to_mvgam(wide))
#'
#' # Next: fit the training data, for example with
#' # mvgam(y ~ s(season, bs = "cc"), trend_formula = ~ AR(),
#' #       data = air$data_train)
#'
#' @seealso The
#'   `r mvgam_article("data", "data article")` covers the long format
#'   and its conversion in more depth. [LLT()] takes the same periods
#'   for a season that drifts.
#'
#' @author Nicholas J Clark
#' @family model fitting
#' @export
series_to_mvgam <- function(series,
                            freq = NULL,
                            response = NULL,
                            index = NULL,
                            h = NULL,
                            train_prop = NULL) {
  checkmate::assert_numeric(
    freq, lower = 1, finite = TRUE, any.missing = FALSE,
    min.len = 1, unique = TRUE, null.ok = TRUE
  )
  checkmate::assert_string(response, null.ok = TRUE)
  checkmate::assert_string(index, null.ok = TRUE)
  checkmate::assert_count(h, positive = TRUE, null.ok = TRUE)
  checkmate::assert_number(
    train_prop, lower = 0, upper = 1, null.ok = TRUE
  )
  if (!is.null(h) && !is.null(train_prop)) {
    stop(insight::format_error(
      "Supply only one of 'h' and 'train_prop'."
    ), call. = FALSE)
  }

  out <- if (inherits(series, "tbl_ts")) {
    tsibble_to_long(series, response = response, freq = freq)
  } else if (stats::is.ts(series)) {
    ts_to_long(series, freq = freq)
  } else if (is.data.frame(series)) {
    wide_to_long(series, index = index, freq = freq)
  } else {
    stop(insight::format_error(c(
      "Argument 'series' has an unsupported class.",
      x = paste0(
        "Got class ", paste(class(series), collapse = "/"), "."
      ),
      i = "Supply a 'tsibble', a 'ts', 'mts' or 'msts', or a data.frame."
    )), call. = FALSE)
  }
  rownames(out) <- NULL
  split_by_time(out, h = h, train_prop = train_prop)
}

# Long frame from a ts, mts or msts. The time values of a ts are in
# units of its frequency, so `round(time * frequency)` counts steps
# from the calendar origin and its remainder modulo `frequency` is
# `cycle() - 1`.
#
# @noRd
ts_to_long <- function(series, freq) {
  n_time <- NROW(series)
  n_series <- NCOL(series)
  labels <- colnames(series) %||% paste0("series_", seq_len(n_series))
  if (anyDuplicated(labels)) {
    stop(insight::format_error(c(
      "Column names of 'series' must be unique.",
      x = paste0(
        "Duplicated: ",
        paste(unique(labels[duplicated(labels)]), collapse = ", "), "."
      )
    )), call. = FALSE)
  }
  freq <- freq %||% attr(series, "msts") %||% stats::frequency(series)
  ts_time <- as.numeric(stats::time(series))
  steps <- round(ts_time * stats::frequency(series))

  # Time-major order: every series at the first time, then the next.
  row_time <- rep(seq_len(n_time), each = n_series)
  row_series <- rep(seq_len(n_series), times = n_time)
  values <- matrix(as.numeric(series), nrow = n_time)
  out <- data.frame(
    series = factor(labels[row_series], levels = labels),
    time = row_time,
    y = values[cbind(row_time, row_series)],
    index = ts_time[row_time]
  )
  cbind(out, season_columns(steps[row_time], freq, names(out)))
}

# Long frame from a wide data.frame: one date column, one numeric
# column per series. The rows are handed to `tsibble_to_long()`, which
# owns the time grid.
#
# @noRd
wide_to_long <- function(series, index, freq) {
  insight::check_if_installed(
    "tsibble", reason = "to convert a data.frame with a date column"
  )
  date_classes <- c("Date", "POSIXt", "yearmonth", "yearquarter",
                    "yearweek")
  if (is.null(index)) {
    is_date <- vapply(
      series, function(x) inherits(x, date_classes), logical(1L)
    )
    if (sum(is_date) != 1L) {
      stop(insight::format_error(c(
        "Argument 'index' is needed to name the date column.",
        x = paste0(
          "Found ", sum(is_date), " columns of class ",
          paste(date_classes, collapse = ", "), "."
        )
      )), call. = FALSE)
    }
    index <- names(series)[is_date]
  }
  checkmate::assert_choice(index, names(series), .var.name = "index")
  labels <- setdiff(names(series), index)
  if (!length(labels)) {
    stop(insight::format_error(
      "The data.frame holds only its date column."
    ), call. = FALSE)
  }
  is_num <- vapply(series[labels], is.numeric, logical(1L))
  if (!all(is_num)) {
    stop(insight::format_error(c(
      "Every column other than the date column must be a numeric series.",
      x = paste0(
        "Columns of other classes: ",
        paste0("'", labels[!is_num], "'", collapse = ", "), "."
      ),
      i = "Drop those columns, or convert covariates through a 'tsibble'."
    )), call. = FALSE)
  }
  if (anyDuplicated(series[[index]])) {
    stop(insight::format_error(
      paste0("Date column '", index, "' holds repeated values.")
    ), call. = FALSE)
  }

  long <- data.frame(
    series = rep(labels, each = nrow(series)),
    y = unlist(series[labels], use.names = FALSE)
  )
  long[[index]] <- rep(series[[index]], times = length(labels))
  long_ts <- tsibble::as_tsibble(long, key = "series", index = index)
  out <- tsibble_to_long(long_ts, response = "y", freq = freq)
  out$series <- factor(out$series, levels = labels)
  out[order(out$time, out$series), , drop = FALSE]
}

# Long frame from a tsibble. A regular tsibble is filled to its full
# grid and `time` counts grid steps. An irregular one keeps its own
# times, counted from 1 at the first index value in the index's units.
#
# @noRd
tsibble_to_long <- function(series, response, freq) {
  insight::check_if_installed(
    "tsibble", reason = "to convert a tsibble"
  )
  index_var <- tsibble::index_var(series)
  key_vars <- tsibble::key_vars(series)
  measured <- tsibble::measured_vars(series)
  if (is.null(response)) {
    if (length(measured) != 1L) {
      stop(insight::format_error(c(
        "Argument 'response' is needed to choose the response.",
        x = paste0(
          "Measured variables: ",
          paste0("'", measured, "'", collapse = ", "), "."
        )
      )), call. = FALSE)
    }
    response <- measured
  }
  checkmate::assert_choice(response, measured, .var.name = "response")
  refuse_reserved_columns(
    intersect(c("series", "time"), setdiff(names(series), key_vars))
  )

  regular <- tsibble::is_regular(series)
  observed <- unique(series[[index_var]])
  if (regular) {
    series <- tsibble::fill_gaps(series, .full = TRUE)
  }
  grid <- sort(unique(series[[index_var]]))
  grid_num <- as.numeric(grid)
  step <- if (length(grid) > 1L) grid_num[2L] - grid_num[1L] else 1
  if (regular) {
    # A calendar period stored in a Date column, such as the first of
    # each month, has uneven day counts. tsibble then takes one day as
    # the interval. Filling gaps would add a row for every day.
    min_gap <- min(diff(sort(as.numeric(observed))), Inf)
    if (min_gap > step) {
      stop(insight::format_error(c(
        paste0(
          "Index '", index_var, "' is spaced more widely than its ",
          "interval."
        ),
        x = paste0(
          "Interval: ", format(tsibble::interval(series)),
          ", smallest observed gap: ", min_gap, " units."
        ),
        i = paste0(
          "Convert monthly, quarterly or weekly dates with ",
          "tsibble::yearmonth(), yearquarter() or yearweek()."
        )
      )), call. = FALSE)
    }
  }
  index_num <- as.numeric(series[[index_var]])
  time <- if (regular) {
    match(index_num, grid_num)
  } else {
    index_num - grid_num[1L] + 1
  }

  if (length(key_vars)) {
    key_frame <- as.data.frame(series)[key_vars]
    key_frame[] <- lapply(key_frame, as.character)
    label <- do.call(paste, c(key_frame, sep = "_"))
  } else {
    label <- rep("series_1", nrow(series))
  }
  out <- data.frame(
    series = factor(label, levels = sort(unique(label))),
    time = time
  )
  frame <- as.data.frame(series)
  out[[response]] <- frame[[response]]
  out[[index_var]] <- frame[[index_var]]
  for (key in setdiff(key_vars, "series")) {
    out[[key]] <- frame[[key]]
  }
  if (regular) {
    freq <- freq %||% tsibble::guess_frequency(grid)
    out <- cbind(
      out, season_columns(round(index_num / step), freq, names(frame))
    )
  } else if (!is.null(freq)) {
    stop(insight::format_error(c(
      "Argument 'freq' needs a regular tsibble.",
      i = "Seasonal positions count steps on a regular grid."
    )), call. = FALSE)
  }
  for (covariate in setdiff(measured, response)) {
    out[[covariate]] <- frame[[covariate]]
  }
  out[order(out$time, out$series), , drop = FALSE]
}

# Position within each seasonal period, counted from the calendar
# origin of the steps. Periods of 1 or less carry no season. One period
# gives a `season` column, several give `season_<period>` columns.
#
# @noRd
season_columns <- function(steps, periods, taken) {
  periods <- periods[periods > 1]
  if (!length(periods)) {
    return(data.frame(row.names = seq_along(steps)))
  }
  labels <- if (length(periods) == 1L) {
    "season"
  } else {
    paste0("season_", as.character(periods))
  }
  refuse_reserved_columns(intersect(labels, taken))
  out <- lapply(periods, function(p) as.integer(floor(steps %% p) + 1))
  names(out) <- labels
  as.data.frame(out)
}

# Refuse input columns that clash with columns the output creates.
#
# @noRd
refuse_reserved_columns <- function(clash) {
  checkmate::assert_character(clash)
  if (length(clash)) {
    stop(insight::format_error(c(
      "The data hold columns the output reserves.",
      x = paste0(
        "Reserved: ", paste0("'", clash, "'", collapse = ", "), "."
      ),
      i = "Rename them before converting."
    )), call. = FALSE)
  }
  invisible(NULL)
}

# Split a long frame at a time cut. Without 'h' or 'train_prop' the
# frame is returned whole.
#
# @noRd
split_by_time <- function(data, h, train_prop) {
  if (is.null(h) && is.null(train_prop)) {
    return(data)
  }
  times <- sort(unique(data$time))
  n_train <- if (is.null(h)) {
    floor(length(times) * train_prop)
  } else {
    length(times) - h
  }
  if (n_train < 1L || n_train >= length(times)) {
    stop(insight::format_error(c(
      "The split leaves the training or the test data empty.",
      x = paste0(
        "The split keeps ", n_train, " of ", length(times),
        " time points for training."
      )
    )), call. = FALSE)
  }
  cut <- times[n_train]
  list(
    data_train = data[data$time <= cut, , drop = FALSE],
    data_test = data[data$time > cut, , drop = FALSE]
  )
}
