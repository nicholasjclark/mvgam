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
  if (is_trend_spec_list(spec)) spec[[1L]] else spec
}

#' Apply a function to every copy of the trend specification
#'
#' A multivariate model holds one copy of the specification per
#' response. A change made to one copy has to reach them all, and the
#' result keeps the shape it arrived in.
#'
#' @param spec A trend specification or a list of them
#' @param fn A function taking and returning one specification
#' @return `spec` with `fn` applied to each specification
#' @noRd
map_trend_specs <- function(spec, fn) {
  if (is.null(spec)) {
    return(NULL)
  }
  checkmate::assert_list(spec)
  checkmate::assert_function(fn)
  if (is_trend_spec_list(spec)) lapply(spec, fn) else fn(spec)
}

#' Does this object hold one trend specification per response?
#'
#' A per-response list is named by response and carries no `trend`
#' field of its own. A specification written without a `trend` field
#' matches that description too, and taking its first element returns
#' a column name in place of a model. Requiring every entry to be a
#' list tells the two apart. `trend_model` marks the wrapper that
#' `ensure_mvgam_variables()` receives.
#'
#' @param spec A trend specification or a list of them
#' @return Logical scalar
#' @noRd
is_trend_spec_list <- function(spec) {
  is.list(spec) &&
    length(spec) > 0L &&
    !is.null(names(spec)) &&
    !any(c("trend", "trend_model") %in% names(spec)) &&
    all(vapply(spec, is.list, logical(1L)))
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

#' The time axis: each distinct time once, earliest first
#'
#' A trend recursion steps along the index this defines. The index
#' runs in the order the times do, whatever order the rows arrive in.
#'
#' @param times Per-row times
#' @return The distinct times, sorted
#' @noRd
time_axis_values <- function(times) {
  sort(unique(times))
}

#' The series axis of a frame that names its series in a column
#'
#' The observed levels, from `observed_levels()`: a factor keeps its
#' declared order, and a character column is sorted.
#'
#' @param series_vals Per-row series identifiers
#' @return The distinct series, in axis order
#' @noRd
series_axis_values <- function(series_vals) {
  observed_levels(series_vals)
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
    stop_mvgam_fault(
      "The series axis lists a series absent from the data.",
      cli::format_inline("Unmatched: {.val {series_axis[is.na(rows)]}}.")
    )
  }
  as.character(data[[gr_var]])[rows]
}

#' The axes a fit was built on
#'
#' The single post-fit reading of which series and which times the
#' model was given. Post-processing asks this in place of rebuilding
#' the axes from the training frame. A rebuild can return a
#' permutation that stays in range and raises nothing.
#'
#' @param object A fitted `mvgam` object or an `mvgam_prefit`
#' @return The axes record, or `NULL` for a model whose frame names no
#'   axis
#' @noRd
mvgam_axes <- function(object) {
  object$trend_metadata$axes
}


#' The columns a row is placed by
#'
#' The axes record names them where the axes are resolved. A model
#' whose frame names no axis carries no record, and this function
#' then returns the literal defaults. A caller spelling its own default could name a
#' different column from the one the model was fitted on.
#'
#' @param object A fitted `mvgam` object
#' @return A list naming `time_var`, `series_var`, `gr_var` and
#'   `subgr_var`. The two grouping entries are `NULL` on a fit with no
#'   hierarchy.
#' @noRd
axis_vars <- function(object) {
  rec <- mvgam_axes(object)$vars
  list(
    time_var = rec$time_var %||% "time",
    series_var = rec$series_var %||% "series",
    gr_var = rec$gr_var,
    subgr_var = rec$subgr_var
  )
}


#' The columns a trend spec names
#'
#' A spec carries the pair under two spellings, `time_var` and
#' `time`, at either of two depths, flat or under `$trend_model`.
#' `spec_field()` settles the depth and both spellings are taken
#' here. This function holds the literal defaults for the layers
#' running before a fit exists, where `axis_vars()` has no object to
#' take them from.
#'
#' @param spec A trend spec, a list of them, or `NULL`
#' @return A list naming `time_var` and `series_var`.
#' @noRd
spec_axis_vars <- function(spec) {
  spec <- trend_spec_head(spec)
  list(
    time_var = spec_field(spec, "time_var") %||%
      spec_field(spec, "time") %||% "time",
    series_var = spec_field(spec, "series_var") %||%
      spec_field(spec, "series") %||% "series"
  )
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

#' Give a frame the `series` column a derived trend axis is named by
#'
#' A trend whose series come from a grouping has no series column in
#' the data, and its design names them `series`. A term written with
#' `lv_axis()` uses that column, and a prediction frame takes it from
#' the grouping here. Any other fit returns the frame unchanged.
#'
#' @param object A fitted `mvgam` object
#' @param data Frame to predict for
#' @return `data`, with `series` set to the derived series where the
#'   trend formula uses it
#' @noRd
derived_series_column <- function(object, data) {
  if (!mvgam_had_by_lv(object) ||
        identical(mvgam_axes(object)$series$source, "explicit")) {
    return(data)
  }
  rows <- axis_row_series(object, data)
  if (!is.null(rows)) {
    data$series <- rows
  }
  data
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
  vars <- axis_vars(object)
  levs <- mvgam_axes(object)$series$levels
  gr_var <- vars$gr_var
  subgr_var <- vars$subgr_var

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

  if (vars$series_var %in% names(data)) {
    return(as_axis(data[[vars$series_var]]))
  }
  if (required) {
    stop(insight::format_error(c(
      "The data lack the series this model was fitted on.",
      x = paste0(
        "Got columns: ", paste(names(data), collapse = ", "), "."
      ),
      i = "Supply the series column or the 'gr' and 'subgr' columns."
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

#' The times each series on the axis was observed at
#'
#' One vector per axis entry, in the axis's own order, holding the
#' times at which that series has a response.
#'
#' @param data The frame the axis was built from
#' @param series_vals Per-row series identifiers
#' @param series_axis The series axis, in order
#' @param times Per-row times, in the units the record holds
#' @param response_axis The response axis where the responses are the
#'   series, from `mvgam_response_axis()`, and `NULL` otherwise
#' @param response_vars The response columns named by response key, from
#'   `response_columns()`. They name the column a response-keyed series
#'   takes its values from, and tell an observation from a padding row
#'   on a stacked frame.
#' @return A list of numeric vectors, empty where a series has no
#'   response
#' @noRd
axis_observed_times <- function(data, series_vals, series_axis, times,
                                response_axis = NULL,
                                response_vars = NULL) {
  # A response-keyed frame has every response on every row, and a
  # response is observed where its own column holds a value. The test
  # is whether the axis is the responses. A wide frame naming its own
  # series column has one series that every response is measured on.
  if (!is.null(response_axis)) {
    return(lapply(as.character(series_axis), function(key) {
      as.numeric(times[!is.na(data[[response_vars[[key]]]])])
    }))
  }

  # A series padded with `NA` responses has rows to the end of the
  # grid and observations to its own end.
  labels <- as.character(series_vals)
  observed <- rep(TRUE, length(labels))
  for (resp in response_vars) {
    if (!is.null(data[[resp]])) {
      observed <- observed & !is.na(data[[resp]])
    }
  }
  lapply(as.character(series_axis), function(lv) {
    seen <- as.numeric(times[labels == lv & observed])
    seen[!is.na(seen)]
  })
}

#' The last time each series on the axis was observed at
#'
#' @param observed_times The list `axis_observed_times()` returns
#' @return One time per axis entry, `NA` where a series has no response
#' @noRd
axis_last_times <- function(observed_times) {
  vapply(observed_times, function(seen) {
    if (!length(seen)) NA_real_ else max(seen)
  }, numeric(1L))
}

#' The typical gap between two observations of one series
#'
#' The median, over every series, of the gaps between consecutive
#' times a series was observed at. `CAR()` measures its gaps in this
#' unit. A time that another series adds to the grid, or a row with a
#' missing response, leaves it unchanged.
#'
#' @param observed_times The list `axis_observed_times()` returns
#' @return A positive number, 1 where no series has two observed times
#' @noRd
axis_observation_gap <- function(observed_times) {
  gaps <- unlist(lapply(observed_times, function(seen) {
    diff(sort(unique(seen)))
  }), use.names = FALSE)
  if (length(gaps) == 0L) {
    return(1)
  }
  stats::median(gaps)
}
