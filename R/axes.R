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
  if (is.null(spec)) {
    return(list(gr = NULL, subgr = NULL))
  }
  checkmate::assert_list(spec)

  pick <- function(field) {
    value <- spec[[field]] %||% spec$trend_model[[field]]
    if (named_var(value)) as.character(value) else NULL
  }
  list(gr = pick("gr"), subgr = pick("subgr"))
}

#' Resolve the series and time axes of a frame
#'
#' The single answer to which latent state a row reads. Every axis
#' question is put to this record rather than re-derived from the
#' frame's columns, so a consumer cannot hold an order that another
#' consumer contradicts.
#'
#' The time axis is carried as its ordered original values. The integer
#' index is then `match()` and the gaps `CAR()` and the Gaussian
#' processes need are `diff()`, which is one representation where there
#' were three.
#'
#' @param data Data frame the model is being fitted to, or predicted on
#' @param spec Trend specification, in either spelling
#' @param time_var Name of the time column
#' @param series_var Name of the series column, where one is used
#' @param response_vars Response names, for a frame keyed by response
#' @param metadata Stored metadata, in a prediction context
#' @return List with `series` (levels, source, n, values) and `time`
#'   (values, n, index)
#' @noRd
resolve_axes <- function(data, spec = NULL, time_var = "time",
                         series_var = "series", response_vars = NULL,
                         metadata = NULL) {
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_string(time_var)
  checkmate::assert_string(series_var)
  checkmate::assert_names(names(data), must.include = time_var)
  if (!is.null(response_vars)) {
    checkmate::assert_character(
      response_vars, min.len = 1, any.missing = FALSE
    )
  }

  list(
    series = resolve_series_axis(
      data, spec, series_var, response_vars, metadata
    ),
    time = resolve_time_axis(data, time_var)
  )
}

#' The time axis, ordered
#'
#' @param data Data frame carrying the time column
#' @param time_var Name of the time column
#' @return List with the ordered original values, their count, and the
#'   per-row index into them
#' @noRd
resolve_time_axis <- function(data, time_var) {
  values <- data[[time_var]]
  if (anyNA(values)) {
    stop(insight::format_error(c(
      cli::format_inline("Missing values in time column {.field {time_var}}."),
      x = "Every row must state the time it was observed at.",
      i = "Drop the rows or supply their times before fitting."
    )), call. = FALSE)
  }
  ordered <- sort(unique(values))
  list(
    values = ordered,
    n = length(ordered),
    index = match(values, ordered)
  )
}

#' The series axis, and where it came from
#'
#' Four sources, tried in the order that decides them: a stored axis in
#' a prediction context, a hierarchical grouping, the responses of a
#' frame keyed by response, and a series column. `source` records which
#' answered, so a later reader need not guess from the shape.
#'
#' @param data Data frame being resolved
#' @param spec Trend specification, in either spelling
#' @param series_var Name of the series column, where one is used
#' @param response_vars Response names, for a frame keyed by response
#' @param metadata Stored metadata, in a prediction context
#' @return List with `levels`, `source`, `n` and per-row `values`
#' @noRd
resolve_series_axis <- function(data, spec, series_var, response_vars,
                                metadata) {
  groupings <- spec_groupings(spec)

  # A prediction frame takes the axis the fit recorded, so a series the
  # training data never held is a refusal rather than a new column.
  if (!is.null(metadata)) {
    stored <- resolve_stored_series_axis(data, metadata, series_var)
    if (!is.null(stored)) {
      return(stored)
    }
  }

  if (!is.null(groupings$gr) && !is.null(groupings$subgr) &&
      !identical(groupings$subgr, series_var)) {
    assert_grouping_columns(data, groupings$gr, groupings$subgr)
    values <- hierarchical_series_values(
      data, groupings$gr, groupings$subgr
    )
    return(series_axis_record(levels(values), "hierarchical", values))
  }

  if (series_var %in% names(data)) {
    values <- data[[series_var]]
    return(series_axis_record(
      observed_series_levels(values), "explicit", values
    ))
  }

  # A frame written with `brms::mvbf()` holds one row per time and one
  # column per response, so the series an observation sits on is a
  # property of the (row, response) pair. A per-row vector cannot say
  # that, and the axis is the responses in formula order instead.
  if (!is.null(response_vars) && length(response_vars) > 1L) {
    return(series_axis_record(response_vars, "response", NULL))
  }

  if (!is.null(response_vars) && length(response_vars) == 1L) {
    return(series_axis_record(response_vars, "single", NULL))
  }

  stop(insight::format_error(c(
    "No series axis could be resolved for this data.",
    x = cli::format_inline(
      "There is no {.field {series_var}} column, no grouping ",
      "variables and no responses to take one from."
    ),
    i = cli::format_inline(
      "Add a {.field {series_var}} column, or name {.arg gr} and ",
      "{.arg subgr} in the trend."
    )
  )), call. = FALSE)
}

#' The axis a fit recorded, rebuilt on a prediction frame
#'
#' @param data Prediction frame
#' @param metadata Stored metadata from the fit
#' @param series_var Name of the series column, where one is used
#' @return A series axis record, or `NULL` where the stored source does
#'   not settle it and the fitting rules apply instead
#' @noRd
resolve_stored_series_axis <- function(data, metadata, series_var) {
  checkmate::assert_list(metadata, names = "named")
  source <- metadata$series_source %||% "explicit"

  if (identical(source, "hierarchical")) {
    gr_var <- metadata$variables$gr_var
    subgr_var <- metadata$variables$subgr_var
    if (named_var(gr_var) && named_var(subgr_var)) {
      assert_grouping_columns(data, gr_var, subgr_var)
      values <- hierarchical_series_values(data, gr_var, subgr_var)
      return(series_axis_record(
        metadata$levels$series %||% levels(values), source, values
      ))
    }
  }

  if (source %in% c("response", "multivariate") &&
      !is.null(metadata$response_vars)) {
    return(series_axis_record(
      metadata$response_vars, "response", NULL
    ))
  }

  NULL
}

#' Assemble one series axis record
#'
#' @param levels Series names, in axis order
#' @param source One of explicit, hierarchical, response or single
#' @param values Per-row series values, or `NULL` where the frame is
#'   keyed by response and the rows do not carry them
#' @return List with `levels`, `source`, `n` and `values`
#' @noRd
series_axis_record <- function(levels, source, values) {
  levels <- as.character(levels)
  checkmate::assert_character(
    levels, min.len = 1, any.missing = FALSE, unique = TRUE
  )
  checkmate::assert_choice(
    source, c("explicit", "hierarchical", "response", "single")
  )
  list(
    levels = levels,
    source = source,
    n = length(levels),
    values = if (is.null(values)) NULL else factor(values, levels = levels)
  )
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
