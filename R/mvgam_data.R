# Pre-fit data inspection. `mvgam_data()` runs the data checks of
# `mvgam()` without building a model. `series_obs_plot()` draws its
# plot and the plot of `plot(fit, type = "series")`.


#' Inspect long-format data before fitting an `mvgam`
#'
#' Runs the checks `mvgam()` applies to its data before compiling a
#' model. It tests the columns, factor levels, time spacing and
#' response support of a proposed model. A failure raises the error
#' `mvgam()` would raise. An optional plot shows each observed series.
#'
#' @param data Long-format `data.frame` (or list) carrying at
#'   least the response column and a `time` column. A latent trend
#'   also needs each row's series: a `series` column, the column
#'   the trend constructor's `series` argument names, or the `gr`
#'   and `subgr` columns of a hierarchical trend. Closure-unit
#'   families (`occ()`, `nmix()`) always require `series` and
#'   `visit` columns. A family without a default `cap` also requires
#'   a `cap` column.
#' @param y Optional name of the response column. `NULL` (default)
#'   takes the response of `formula`, or `"y"` when `formula` is
#'   `NULL`. A name that differs from the response of `formula` is
#'   refused.
#' @param family A `family`, `brmsfamily` or mvgam family object.
#'   Defaults to `gaussian()`. The response must lie in the family's
#'   support, such as non-negative integers for `poisson()` or
#'   `(0, 1)` for `Beta()`. A `cap` column must hold integers no
#'   smaller than the response and stay constant within a closure
#'   unit. Multi-response families (`diri()`,
#'   `multi()`, `categ()`, `mvn()`, `mvt()`) are refused. [mvgam()]
#'   and [jsdgam()] check their data at fit time.
#' @param formula Optional observation formula, as a `formula` or a
#'   `brmsformula`. A closure-unit family takes a detection formula
#'   in the same `bf()`. Every column the formula uses must be in
#'   `data`. These include covariates, offsets, random-effect
#'   grouping factors and the columns of addition terms such as
#'   `trials()` and `weights()`. Those columns may not hold `NA`. A
#'   `trials()` count may not fall below its response. A missing
#'   response is allowed. `mvgam()` keeps its row on the time grid.
#' @param trend_formula Optional trend formula (e.g.
#'   `~ AR(time = week, series = species) + s(env)`), as passed to
#'   [mvgam()]. Its trend constructor names the time and series
#'   columns and decides whether times must be evenly spaced:
#'   `CAR()` and `ZMVN()` accept gaps, most others refuse them.
#'   `NULL` checks the data for a model without a trend, which
#'   needs no series column. Its covariates, offsets and grouping
#'   factors get the same checks as those of `formula`.
#' @param data2 Optional named list of further model data, as passed
#'   to [mvgam()]. A formula variable found here, such as a matrix
#'   covariate, need not be a column of `data`.
#' @param plot Logical. `TRUE` (default) draws the observed series
#'   with the panels of `plot(fit, type = "series")`. Several series
#'   draw as one facet each. One series draws four panels: the time
#'   series, a histogram, the ACF and the empirical CDF.
#' @param newdata Optional held-out data in the long format of
#'   `data`. The time series panels draw it in black after a dashed
#'   line at the first test time. The histogram, ACF and CDF of one
#'   series pool the training and test values.
#' @param series Series to plot: `NULL`, `"all"`, a series name or
#'   a positive index into the series levels. `NULL` draws every
#'   series of multi-series data and the one series otherwise. Data
#'   without a series column plot as one series named after the
#'   response.
#' @param lines Logical. Plot lines (default) or points.
#' @param n_bins Optional histogram bin count.
#' @param log_scale Logical. Plot the response as `log(y + 1)`.
#'
#' @return Invisibly, an `mvgam_data` list with elements `data` (the
#'   validated data), `family` (the resolved family), `series_levels`
#'   (`NULL` when the data name no series), `n_series`, `time_range`,
#'   `n_obs`, `n_missing` (the count of missing responses) and `plot`
#'   (the `ggplot` or `patchwork` object when `plot = TRUE`, else
#'   `NULL`). With `plot = TRUE` the plot is also drawn on the active
#'   device.
#'
#' @author Nicholas J Clark
#'
#' @seealso [mvgam()], [mvgam_formula()], [jsdgam()],
#'   [sim_mvgam()], [sim_closure_unit_data()],
#'   [get_prior.mvgam_formula()].
#'
#' @examples
#' set.seed(1)
#' simdat <- sim_mvgam(family = poisson(), n_series = 3L,
#'                     n_timepoints = 60L)
#'
#' # Check the frame for a Poisson trend model and plot every series
#' mvgam_data(simdat$data_train, family = poisson(),
#'            trend_formula = ~ AR())
#'
#' # One series in four panels: time series, histogram, ACF and ECDF.
#' # The test data follow the dashed line.
#' mvgam_data(simdat$data_train, family = poisson(), series = 1L,
#'            newdata = simdat$data_test)
#'
#' # A response the family cannot take is refused before any Stan
#' # compile. The error names the column, the values and the support.
#' counts <- simdat$data_train
#' counts$y[3L] <- -1
#' try(mvgam_data(counts, family = poisson(), plot = FALSE))
#'
#' # AR() assumes evenly spaced times. CAR() models the gaps.
#' gappy <- simdat$data_train[simdat$data_train$time != 5L, ]
#' try(mvgam_data(gappy, family = poisson(), trend_formula = ~ AR(),
#'                plot = FALSE))
#' mvgam_data(gappy, family = poisson(), trend_formula = ~ CAR(),
#'            plot = FALSE)
#'
#' @family model fitting
#' @export
mvgam_data <- function(data,
                       y = NULL,
                       family = gaussian(),
                       formula = NULL,
                       trend_formula = NULL,
                       data2 = NULL,
                       plot = TRUE,
                       newdata = NULL,
                       series = NULL,
                       lines = TRUE,
                       n_bins = NULL,
                       log_scale = FALSE) {
  checkmate::assert_string(y, null.ok = TRUE)
  checkmate::assert_flag(plot)
  checkmate::assert_formula(trend_formula, null.ok = TRUE)
  checkmate::assert_list(data2, names = "named", null.ok = TRUE)
  as_frame <- function(x) {
    if (is.list(x) && !is.data.frame(x)) as.data.frame(x) else x
  }
  data <- as_frame(data)
  newdata <- as_frame(newdata)
  checkmate::assert_data_frame(data, min.rows = 1L)
  checkmate::assert_data_frame(newdata, min.rows = 1L, null.ok = TRUE)
  # The family and the trend are resolved as mvgam() resolves them, so
  # a model this check passes is one mvgam() accepts.
  obs_formula <- formula %||% stats::reformulate("1", response = y %||% "y")
  resolved <- resolve_observation_family(obs_formula, family)
  obs_formula <- resolved$formula
  family <- resolved$family
  y <- mvgam_data_response(obs_formula, y)

  if (is_multi_response_family(family)) {
    stop(insight::format_error(c(
      paste0(
        "Multi-response families are not supported by ",
        "'mvgam_data()'."
      ),
      i = "mvgam() and jsdgam() validate these data themselves."
    )), call. = FALSE)
  }

  # The trend constructor names the axis columns and the spacing rule.
  # Without a trend, the default names apply and the rows need no
  # series.
  trend_spec <- if (!is.null(trend_formula)) {
    trend_spec_head(
      parse_multivariate_trends(obs_formula, trend_formula)$trend_specs
    )
  }
  axis <- spec_axis_vars(trend_spec)
  time_var <- axis$time_var
  series_var <- axis$series_var
  if (!is.null(trend_spec)) {
    trend_spec <- resolve_trend_groupings(trend_spec, data, series_var)
  }

  if (uses_closure_unit_layout(family)) {
    # The closure-unit validator checks the response, the axis, visit
    # and cap columns and the minimum of two closure units.
    validate_closure_unit_data(
      data, family,
      response_var = y,
      series_var   = series_var,
      time_var     = time_var
    )
  } else {
    validate_required_variables(data, y)
    assert_axis_column(data, time_var, "time")
    validate_response_shapes(data, obs_formula, family)
  }
  if (!is.null(newdata)) {
    assert_axis_column(newdata, time_var, "time", frame = "newdata")
  }

  # Each row's series, resolved the way mvgam() resolves it: from the
  # hierarchical grouping, else the series column. A trend refuses
  # data that name no series. Without a trend and without the column,
  # the rows plot as one series labelled by the response.
  has_series <- !is.null(trend_spec) || series_var %in% names(data)
  row_series <- function(df, frame) {
    if (!has_series) {
      return(rep(y, nrow(df)))
    }
    attr(ensure_mvgam_variables(df, trend_spec, time_var, series_var,
                                frame = frame),
         "mvgam_series")
  }
  train_series <- row_series(data, "data")
  new_series <- if (!is.null(newdata)) row_series(newdata, "newdata")
  # `mvgam()` drops a level no row holds. The check reports the series
  # the model will fit.
  series_levels <- if (has_series) {
    levels(droplevels(as.factor(train_series)))
  }

  if (trend_requires_regular_intervals(trend_spec)) {
    validate_regular_time_intervals(data[[time_var]], time_var)
  }

  # The column checks `build_stan_components()` runs before any
  # program is built.
  validate_model_columns(data, obs_formula, family, trend_formula, data2)

  out <- list(
    data          = data,
    family        = family,
    series_levels = series_levels,
    n_series      = length(series_levels),
    time_range    = range(data[[time_var]], na.rm = TRUE),
    n_obs         = nrow(data),
    n_missing     = sum(is.na(data[[y]])),
    plot          = NULL
  )
  class(out) <- "mvgam_data"
  rlang::inform(c(
    "v" = paste0("Data check passed for family '",
                 mvgam_data_family_label(family), "'."),
    stats::setNames(mvgam_data_summary_lines(out), rep("*", 3L))
  ))

  if (isTRUE(plot)) {
    p <- series_obs_plot(
      dat           = rbind(
        series_long_df(data, y, train_series, time_var, "train"),
        series_long_df(newdata, y, new_series, time_var, "validate")
      ),
      labels        = y,
      series_levels = series_levels %||% y,
      series        = series,
      lines         = lines,
      n_bins        = n_bins,
      log_scale     = log_scale
    )
    print(p)
    out$plot <- p
  }

  invisible(out)
}


#' The one response column a data check inspects
#'
#' @param formula Observation formula after family resolution
#' @param y The `y` argument of `mvgam_data()`, or `NULL`
#' @return Character name of the response column
#' @noRd
mvgam_data_response <- function(formula, y) {
  columns <- unname(response_columns(formula))
  if (length(columns) > 1L) {
    stop(insight::format_error(c(
      "'mvgam_data()' checks one response at a time.",
      x = paste0("The formula names ", length(columns), " responses: ",
                 paste0("'", columns, "'", collapse = ", "), "."),
      i = "Call it once per response with that response's formula."
    )), call. = FALSE)
  }
  if (!is.null(y) && !identical(y, columns)) {
    stop(insight::format_error(c(
      paste0("'y' names '", y, "' but the formula's response is '",
             columns, "'."),
      i = "Drop 'y' to check the formula's response."
    )), call. = FALSE)
  }
  columns
}


#' The family and link label the data check prints
#'
#' @param family Family object holding `link`
#' @return Character scalar such as `"poisson (link = log)"`
#' @noRd
mvgam_data_family_label <- function(family) {
  checkmate::assert_list(family)
  fam_name <- resolve_family_name(family)
  if (is.null(family$link)) {
    return(fam_name)
  }
  paste0(fam_name, " (link = ", family$link, ")")
}


#' The series, time and observation lines of a data check
#'
#' `mvgam_data()` reports them after the check and
#' `print.mvgam_data()` prints them again.
#'
#' @param x Object of class `mvgam_data`
#' @return Character vector of three summary lines
#' @noRd
mvgam_data_summary_lines <- function(x) {
  checkmate::assert_class(x, "mvgam_data")
  c(
    if (is.null(x$series_levels)) {
      "series: all rows as one"
    } else {
      paste0("series: ", x$n_series,
             ngettext(x$n_series, " level", " levels"))
    },
    paste0("time:   ", x$time_range[1L], " to ", x$time_range[2L]),
    paste0("n obs:  ", x$n_obs, " (", x$n_missing, " NA)")
  )
}


#' @method print mvgam_data
#' @export
print.mvgam_data <- function(x, ...) {
  cat(
    paste0("Validated data for family '",
           mvgam_data_family_label(x$family), "'."),
    paste0("  ", mvgam_data_summary_lines(x)),
    sep = "\n"
  )
  invisible(x)
}

