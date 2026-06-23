# Pre-fit data inspection. Composes the same validators mvgam()
# runs at fit time (`validate_required_variables()`,
# `validate_regular_time_intervals()`, `validate_closure_unit_data()`,
# `validate_response_for_family()`) so users can verify their
# long-format data has the columns / levels / response shape a
# proposed family expects, without paying for a Stan compile.
# Optionally returns the same observed-series plot that
# plot.mvgam(type = "series") renders on a fitted object, via
# the shared `series_obs_plot()` body in R/plot_mvgam_series.R.


#' Inspect long-format data before fitting an `mvgam`
#'
#' Validates that `data` carries the columns, factor levels, and
#' response shape `mvgam()` would expect for a given observation
#' family, and optionally returns an exploratory time-series plot.
#' Use this to verify data structure for a proposed observation
#' family without paying for a Stan compile, and to surface the
#' same friendly errors `mvgam()` raises at fit time. Mirrors the
#' role of [mvgam_formula()] for the data side.
#'
#' @param data Long-format `data.frame` (or list) carrying at
#'   least the response column, a `time` column, and (for
#'   multi-series fits) a `series` factor column. Closure-unit
#'   families (`occ()`, `nmix()`) additionally require `visit`
#'   and (where the family needs it) `cap`.
#' @param y Character. Name of the response column. Defaults to
#'   `"y"`.
#' @param family A `family`, `brmsfamily`, or mvgam-specific
#'   family object. Defaults to `gaussian()`. Closure-unit
#'   families dispatch to the same closure-unit validator
#'   `mvgam()` uses internally (`visit` / `cap` columns, integer
#'   / non-negative / binary checks, cap >= y, cap constant
#'   within unit). Univariate non-closure-unit families run a
#'   response-shape check against the family's support (e.g.
#'   non-negative integers for Poisson, `(0, 1)` for Beta,
#'   strictly positive for Gamma). Multi-response families
#'   (`diri()`, `multi()`, `categ()`, `mvn()`, `mvt()`) are not
#'   supported here; pass the data directly to [mvgam()] /
#'   [jsdgam()].
#' @param trend_model Optional trend constructor (e.g. `AR()`,
#'   `RW()`, `CAR()`). When supplied, regular time spacing is
#'   enforced for trends that require it; `CAR()` skips that
#'   check.
#' @param plot Logical. If `TRUE` (default), draw the
#'   observed-series plot using the same panels as
#'   `plot(fit, type = "series")`. Multi-series data render as a
#'   faceted time-series plot; a single-series request returns
#'   the 4-panel patchwork (time series, histogram, ACF, ECDF).
#' @param newdata Optional held-out long-format `data.frame` /
#'   list. Plotted overlaid in black with a dashed cut at the
#'   training/test boundary.
#' @param series One of `NULL`, `"all"`, or a positive integer
#'   index into the `series` factor levels. `NULL` resolves to
#'   `"all"` for multi-series data and `1L` for single-series.
#' @param lines Logical. Plot lines (default) or points.
#' @param n_bins Optional histogram bin count.
#' @param log_scale Logical. Plot the response as `log(y + 1)`
#'   on the multi-series view, where different scales are common.
#'
#' @return Invisibly, an `mvgam_data` list with elements
#'   `data` (the validated data), `family` (resolved family
#'   object), `series_levels`, `n_series`, `time_range`, and
#'   `plot` (the `ggplot` / `patchwork` object when
#'   `plot = TRUE`, else `NULL`). When `plot = TRUE` the plot is
#'   also drawn to the active device.
#'
#' @author Nicholas J Clark
#'
#' @seealso [mvgam()], [mvgam_formula()], [jsdgam()],
#'   [sim_mvgam()], [sim_closure_unit_data()],
#'   [default_prior.mvgam_formula()].
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' simdat <- sim_mvgam(family = poisson(), n_series = 3L,
#'                      n_timepoints = 40L)
#'
#' # Verify structure for a Poisson fit and view the multi-series
#' # plot.
#' mvgam_data(
#'   simdat$data_train,
#'   y           = "y",
#'   family      = poisson(),
#'   trend_model = AR()
#' )
#'
#' # 4-panel single-series view (time series, histogram, ACF,
#' # ECDF).
#' mvgam_data(simdat$data_train, family = poisson(), series = 1L)
#' }
#'
#' @export
mvgam_data <- function(data,
                       y = "y",
                       family = gaussian(),
                       trend_model = NULL,
                       plot = TRUE,
                       newdata = NULL,
                       series = NULL,
                       lines = TRUE,
                       n_bins = NULL,
                       log_scale = FALSE) {
  checkmate::assert_string(y)
  checkmate::assert_flag(plot)
  if (is.list(data) && !is.data.frame(data)) {
    data <- as.data.frame(data)
  }
  checkmate::assert_data_frame(data, min.rows = 1L)
  family <- validate_family(family)

  if (is_multi_response_family(family)) {
    stop(insight::format_error(c(
      paste0(
        "Multi-response families are not supported by ",
        "'mvgam_data()'."
      ),
      i = paste0(
        "Pass the data directly to mvgam() or jsdgam(); ",
        "validation runs there with the full multi-column ",
        "response pipeline."
      )
    )))
  }

  # Series column is optional for single-series data; mvgam()
  # synthesises one internally if missing, so mirror that here
  # before composing the shared validators.
  if (!"series" %in% names(data)) {
    data$series <- factor(rep("series_1", nrow(data)))
  } else if (!is.factor(data$series)) {
    stop(insight::format_error(c(
      "'series' must be a factor.",
      x = paste0("Got class '", class(data$series)[1L], "'."),
      i = "Convert with factor() before calling mvgam_data()."
    )))
  }

  is_cu <- is_closure_unit_family(family)
  if (is_cu) {
    # validate_closure_unit_data() already enforces required
    # columns (response + visit + cap when needed), integer /
    # non-negative / binary checks, cap >= y, cap-constant-within-
    # unit, and the >= 2 closure-unit minimum.
    binary <- isTRUE(attr(family, "mvgam_binary_response",
                           exact = TRUE))
    cap_required <- is.null(closure_unit_default_cap(family))
    validate_closure_unit_data(
      data           = data,
      response_var   = y,
      binary_y_check = binary,
      cap_required   = cap_required
    )
  } else {
    # Univariate non-closure-unit path. Compose the same
    # column-presence validator mvgam() runs.
    validate_required_variables(
      data, required_vars = c(y, "time", "series"),
      context = "mvgam_data"
    )
    validate_response_for_family(data[[y]], family, y_name = y)
  }

  # Time regularity. CAR is the only constructor that handles
  # irregular intervals; everything else uses the same validator
  # mvgam() runs at fit time.
  if (!identical(get_trend_name(trend_model), "CAR")) {
    validate_regular_time_intervals(data$time, "time")
  }

  series_levels <- levels(data$series)
  n_series <- length(series_levels)

  fam_name  <- resolve_family_name(family)
  fam_label <- if (!is.null(family$link)) {
    paste0(fam_name, " (link = ", family$link, ")")
  } else {
    fam_name
  }
  cli::cli_inform(c(
    "v" = paste0("Data check passed for family '", fam_label, "'."),
    "*" = paste0("series: ", n_series, " level(s)"),
    "*" = paste0("time:   ", min(data$time, na.rm = TRUE),
                 " to ", max(data$time, na.rm = TRUE)),
    "*" = paste0("n obs:  ", length(data[[y]]),
                 " (", sum(is.na(data[[y]])), " NA)")
  ))

  out <- list(
    data          = data,
    family        = family,
    series_levels = series_levels,
    n_series      = n_series,
    time_range    = range(data$time, na.rm = TRUE),
    plot          = NULL
  )

  if (isTRUE(plot)) {
    p <- series_obs_plot(
      train         = data,
      test          = newdata,
      response      = y,
      meta          = list(time_var = "time", series_var = "series"),
      series_levels = series_levels,
      series        = series,
      lines         = lines,
      n_bins        = n_bins,
      log_scale     = log_scale
    )
    print(p)
    out$plot <- p
  }

  class(out) <- "mvgam_data"
  invisible(out)
}


#' @method print mvgam_data
#' @export
print.mvgam_data <- function(x, ...) {
  cli::cli_inform(c(
    paste0("Validated data for family '",
           resolve_family_name(x$family), "'."),
    "*" = paste0("series: ", x$n_series, " level(s)"),
    "*" = paste0("time:   ", x$time_range[1L],
                 " to ", x$time_range[2L]),
    "*" = paste0("n obs:  ", length(x$data[[1L]]))
  ))
  invisible(x)
}
