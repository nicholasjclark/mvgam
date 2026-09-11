# The series and time axes of a fitted model, resolved in one place.
#
# Generated Stan reads the two together:
#
#   trend[t, s] = Z[s, ] * lv_trend[t, ] + mu_trend[times_trend[t, s]]
#   mu[n]      += trend[obs_trend_time[n], obs_trend_series[n]]
#
# so one question, which latent state an observation reads, decides
# every fitted value, forecast, residual and label. Answering it in
# more than one place is how two answers to it came to disagree.

#' The grouping variables a trend specification names
#'
#' A specification reaches its consumers in two shapes. Some callers
#' hold it flat, with `gr` and `subgr` at the top level; others hold it
#' nested under `$trend_model`. Reading one spelling makes a
#' hierarchical model look ungrouped everywhere the other is passed,
#' which is how a single frame acquired two series axes within one
#' `standata()` build.
#'
#' @param spec Trend specification in either shape, or `NULL`
#' @return List with `gr` and `subgr`, each a column name or `NULL`
#' @noRd
spec_groupings <- function(spec) {
  spec <- trend_spec_head(spec)
  if (is.null(spec)) {
    return(list(gr = NULL, subgr = NULL))
  }
  pick <- function(field) {
    value <- spec_field(spec, field)
    if (named_var(value)) as.character(value) else NULL
  }
  list(gr = pick("gr"), subgr = pick("subgr"))
}

#' One field of a trend specification, at either depth
#'
#' A specification carries its fields at the top level or nested
#' under `$trend_model`, so both are looked in. `[[` on a name a
#' list does not carry raises rather than answering `NULL`, so the
#' name is tested first: a caller that happens to pass a
#' specification carrying every field never sees the difference,
#' which is why this held until a spec spelled by hand reached it.
#'
#' @param spec A single trend specification
#' @param field Name of the field to read
#' @return The field's value, or `NULL` where neither depth has it
#' @noRd
spec_field <- function(spec, field) {
  at <- function(x, name) {
    if (is.list(x) && name %in% names(x)) x[[name]] else NULL
  }
  at(spec, field) %||% at(at(spec, "trend_model"), field)
}

#' One trend specification, whatever shape it arrives in
#'
#' A multivariate model carries one specification per response, so the
#' object handed round is sometimes that list and sometimes a single
#' specification. Every response shares one trend axis, so the first
#' answers for all of them. Reading the list as though it were a
#' specification finds none of its fields and reports a model with no
#' grouping and no factors, which is the shape of defect this record
#' exists to end.
#'
#' @param spec A trend specification, a list of them, or `NULL`
#' @return A single specification, or `NULL`
#' @noRd
trend_spec_head <- function(spec) {
  if (is.null(spec)) {
    return(NULL)
  }
  checkmate::assert_list(spec)
  # A list of specifications holds specifications, one per response.
  # `is_multivariate_trend_specs()` decides by the absence of a
  # trend field at the top level, which also describes a single
  # specification written without one: taking its first element
  # then reads a column name as though it were a model, and reports
  # a grouped trend as ungrouped without a word. Requiring the
  # entries to be lists tells the two apart.
  looks_multivariate <- is_multivariate_trend_specs(spec) &&
    length(spec) > 0L &&
    all(vapply(spec, is.list, logical(1L)))
  if (looks_multivariate) spec[[1L]] else spec
}

#' The number of latent factors a specification names
#'
#' @param spec A trend specification, in any of its shapes
#' @return Integer count, or `NULL` where the trend names none
#' @noRd
spec_n_lv <- function(spec) {
  value <- spec_field(trend_spec_head(spec), "n_lv")
  if (is.null(value)) NULL else as.integer(value)
}

#' The group each series on the axis belongs to
#'
#' Answers in the axis's own order, taken from the rows the axis was
#' built from. Stan subscripts `group_inds_trend` with the trend's
#' series index, so any other order correlates the wrong series
#' together while staying in range.
#'
#' @param data Data frame the axis was built from
#' @param spec Trend specification, in either spelling
#' @param series_vals Per-row series identifiers
#' @param series_axis The series axis, in order
#' @return Group value per axis entry, or `NULL` where the trend names
#'   no grouping
#' @noRd
axis_group_values <- function(data, spec, series_vals, series_axis) {
  gr_var <- spec_groupings(spec)$gr
  if (is.null(gr_var) || !gr_var %in% names(data)) {
    return(NULL)
  }
  rows <- match(as.character(series_axis), as.character(series_vals))
  if (anyNA(rows)) {
    stop(insight::format_error(c(
      "The series axis names a series the data holds no rows for.",
      x = cli::format_inline(
        "Unmatched: {.val {series_axis[is.na(rows)]}}."
      ),
      i = "This means two parts of the fit disagree about the axis."
    )), call. = FALSE)
  }
  as.character(data[[gr_var]])[rows]
}

#' The axes a fit was built on
#'
#' The single post-fit reading of which series and which times the
#' model was given. Post-processing asks this rather than rebuilding
#' the axes from the training frame, because a rebuild answers with a
#' permutation that stays in range and so raises nothing.
#'
#' A fit stored before the record existed still names its series in
#' metadata, so the series half is assembled from that here. Doing it
#' in one place is what stops each consumer growing a fallback of its
#' own, which is the shape the package is being brought out of. Such
#' a fit records nothing about its times, so `$time` is absent rather
#' than empty.
#'
#' @param object A fitted `mvgam` object or an `mvgam_prefit`
#' @return The axes record, or `NULL` when the object names no
#'   series. `$time` is `NULL` on a fit that predates the record.
#' @noRd
mvgam_axes <- function(object) {
  axes <- axes_from_metadata(object$trend_metadata)
  if (!is.null(axes)) {
    return(axes)
  }
  # An object whose metadata names no series at all: a fit saved
  # before any of this was recorded, or one assembled by hand. The
  # count still exists elsewhere on it, so the fallback lives here
  # rather than at each reader, where every reader would need its
  # own and they would drift.
  n <- object$series_info$n_series %||%
    object$standata$N_series_trend %||%
    object$trend_components$n_trends
  if (is.null(n)) {
    return(NULL)
  }
  list(series = list(levels = NULL, source = NULL,
                     n = as.integer(n), groups = NULL),
       time = NULL)
}

#' Whether a fit's series are its responses
#'
#' A wide `mvbf()` frame holds one row per occasion and one column per
#' response, so the series an observation sits on is the response it
#' was measured for rather than anything the row names. Its series
#' column, where there is one, is a single constant standing in for
#' all of them. Every post-fit path that maps a row to a trend column
#' has to know this, and the axis record states it, so this is the one
#' test they ask. Rebuilding the answer from a frame instead cost a
#' preparation pass per call and gave the question four spellings.
#'
#' @param object A fitted `mvgam` object
#' @return A single logical
#' @noRd
is_response_keyed <- function(object) {
  identical(mvgam_axes(object)$series$source, "multivariate")
}

#' The axes a stored metadata list describes
#'
#' The one place that knows a model saved before the record existed
#' spelled its series as `levels$series` and `series_source`. Every
#' reader goes through here, so the older spelling is understood in
#' one place rather than tested for at each of them.
#'
#' @param meta A fit's `trend_metadata`
#' @return The axes record, or `NULL` when it names no series
#' @noRd
axes_from_metadata <- function(meta) {
  if (!is.null(meta$axes)) {
    return(meta$axes)
  }

  levs <- as.character(meta$levels$series %||% character(0L))
  if (!length(levs)) {
    return(NULL)
  }
  # Only the series half survives in an older fit's metadata: the
  # stored levels name the series but nothing there names the times.
  # The time half is left absent rather than filled with an empty
  # vector, so a reader gets nothing instead of a grid of length
  # zero that looks like an answer.
  list(
    series = list(
      levels = levs,
      source = meta$series_source %||% "explicit",
      n = length(levs),
      groups = NULL
    ),
    time = NULL
  )
}

#' Per-row series identity, as the fit resolved it
#'
#' Returns a factor whose levels are the axis itself, so the order of
#' `levels()` is the order the trend matrix numbers its columns and a
#' row matches the series it was fitted on. A frame whose `series`
#' column was superseded by a grouping is read through the grouping,
#' which is what the model read; reading the column instead compares
#' the supplanted spelling against the derived one and matches
#' nothing.
#'
#' @param object A fitted `mvgam` object
#' @param data Frame to identify the rows of
#' @param required Whether a frame naming no series and carrying no
#'   grouping is refused rather than answered with `NULL`
#' @return A factor with one entry per row, or `NULL` when the frame
#'   names no series and carries no grouping. A frame keyed by
#'   response answers `NULL`: there the series is a property of the
#'   `(row, response)` pair, which one value per row cannot state,
#'   and the caller reads the response axis instead.
#' @noRd
axis_row_series <- function(object, data, required = FALSE) {
  checkmate::assert_data_frame(data)
  checkmate::assert_flag(required)
  # Answering for a response-keyed frame with its series constant
  # would put every row on the first series, so the question is
  # refused and the caller reads the response axis instead.
  if (is_response_keyed(object)) {
    return(NULL)
  }
  meta <- object$trend_metadata
  levs <- mvgam_axes(object)$series$levels
  gr_var <- meta$variables$gr_var
  subgr_var <- meta$variables$subgr_var

  # `factor()` given no levels takes the values' own, which is the
  # right answer for an object recording no axis and the wrong one
  # for an object recording a different order, so the record is used
  # wherever it exists.
  as_axis <- function(values) {
    values <- as.character(values)
    if (is.null(levs)) factor(values) else factor(values, levels = levs)
  }

  if (named_var(gr_var) && named_var(subgr_var) &&
      all(c(gr_var, subgr_var) %in% names(data))) {
    return(as_axis(hierarchical_series_values(data, gr_var, subgr_var)))
  }

  series_var <- meta$variables$series_var %||% "series"
  if (series_var %in% names(data)) {
    return(as_axis(data[[series_var]]))
  }
  if (required) {
    stop(insight::format_error(c(
      "The frame names no series this model was fitted on.",
      x = paste0(
        "Got columns: ", paste(names(data), collapse = ", "), "."
      ),
      i = paste0(
        "Supply the column the model reads or the grouping columns ",
        "that together name a series."
      )
    )), call. = FALSE)
  }
  NULL
}

#' Name the grain the trend design runs on
#'
#' Everything else about the axes is settled where they are resolved.
#' Whether the design is indexed by series or by latent factor depends
#' on a `by = lv_axis()` term, which is only known once the trend
#' formula has been walked, so the record is completed here rather
#' than built a second time.
#'
#' @param axes The axes record, or `NULL`
#' @param has_by_lv Whether the emitted trend takes the factor grain
#' @param had_by_lv Whether the user wrote `by = lv_axis()`, kept for
#'   the non-factor rewrite where the emitted trend does not
#' @return The record with `grain` named, or `NULL`
#' @noRd
complete_axes_grain <- function(axes, has_by_lv, had_by_lv) {
  if (is.null(axes)) {
    return(NULL)
  }
  axes$grain <- if (isTRUE(has_by_lv) || isTRUE(had_by_lv)) {
    "lv"
  } else {
    "series"
  }
  axes
}

#' The last time each series on the axis was observed at
#'
#' Answers in the axis's own order. `CAR()` forecasts forward from
#' these, so an answer in any other order starts each series from
#' another's last observation.
#'
#' @param data The frame the axis was built from
#' @param series_vals Per-row series identifiers
#' @param series_axis The series axis, in order
#' @param times Per-row times, in the units the record carries
#' @param response_axis The response axis where the responses are the
#'   series, from `mvgam_response_axis()`, and `NULL` otherwise
#' @param response_vars The response columns named by response key, from
#'   `response_columns()`. They say which column a response-keyed series
#'   is read from, and tell an observation from a padding row on a
#'   stacked frame.
#' @return One time per axis entry, `NA` where a series has no rows
#' @noRd
axis_last_times <- function(data, series_vals, series_axis, times,
                            response_axis = NULL,
                            response_vars = NULL) {
  # A response-keyed frame carries every response on every row, so a
  # row's series is not in its values: they are one constant. Each
  # response's last occasion is the last row at which that response
  # was observed, which is what its own column says.
  #
  # The test is whether the axis *is* the responses, not whether the
  # model has several. A wide frame naming its own series column has
  # one series that every response is measured on, and there the
  # levels are not column names at all.
  if (!is.null(response_axis)) {
    return(vapply(as.character(series_axis), function(key) {
      seen <- times[!is.na(data[[response_vars[[key]]]])]
      if (!length(seen)) NA_real_ else max(as.numeric(seen))
    }, numeric(1L), USE.NAMES = FALSE))
  }

  # When a series was last *observed*, which is not the same as the
  # last row it has. mvgam asks a panel whose series end at
  # different times to be padded with `NA`, so a padded series has
  # rows to the end of the grid and observations only to its own
  # end. Reading the rows dated it from the padding, and a `CAR()`
  # forecast then started from an occasion the series was never
  # seen at. The response branch above has always asked the right
  # question; this asks the same one of a stacked frame.
  labels <- as.character(series_vals)
  observed <- rep(TRUE, length(labels))
  for (resp in response_vars) {
    if (!is.null(data[[resp]])) {
      observed <- observed & !is.na(data[[resp]])
    }
  }
  vapply(as.character(series_axis), function(lv) {
    seen <- times[labels == lv & observed]
    if (!length(seen)) NA_real_ else max(as.numeric(seen), na.rm = TRUE)
  }, numeric(1L), USE.NAMES = FALSE)
}
