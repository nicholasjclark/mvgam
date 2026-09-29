# Pre-fit data inspection. Composes the same validators mvgam()
# runs at fit time (`ensure_mvgam_variables()` for the trend axes,
# `validate_regular_time_intervals()`, `validate_closure_unit_data()`,
# `validate_response_for_family()`) so users can verify their
# long-format data has the columns / levels / response shape a
# proposed model requires, without paying for a Stan compile.
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
#'   least the response column and a `time` column. A latent trend
#'   also needs each row's series: a `series` column, the column
#'   the trend constructor's `series` argument names, or the `gr`
#'   and `subgr` columns of a hierarchical trend. Closure-unit
#'   families (`occ()`, `nmix()`) always require `series` and
#'   `visit`, and `cap` where the family needs it.
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
#' @param formula Optional observation formula (a `formula`,
#'   `brmsformula`, or `bf()` two-arm specification for
#'   closure-unit fits). When supplied, every covariate the
#'   formula references is checked for `NA` and the call errors
#'   if any are present. Response `NA`s are always allowed and
#'   are preserved by `mvgam()` to maintain the time grid.
#' @param trend_formula Optional trend formula (e.g.
#'   `~ AR(time = week, series = species) + s(env)`), as passed to
#'   [mvgam()]. Its trend constructor names the time and series
#'   columns and decides whether times must be evenly spaced:
#'   `CAR()` and `ZMVN()` accept gaps, most others refuse them.
#'   `NULL` checks the data for a model without a trend, which
#'   needs no series column. Covariates the formula references
#'   get the same `NA` check as `formula`.
#' @param plot Logical. If `TRUE` (default), draw the
#'   observed-series plot using the same panels as
#'   `plot(fit, type = "series")`. Multi-series data render as a
#'   faceted time-series plot; a single-series request returns
#'   the 4-panel patchwork (time series, histogram, ACF, ECDF).
#' @param newdata Optional held-out long-format `data.frame` /
#'   list. Plotted overlaid in black with a dashed cut at the
#'   training/test boundary.
#' @param series One of `NULL`, `"all"`, or a positive integer
#'   index into the series levels. `NULL` resolves to `"all"` for
#'   multi-series data and `1L` for single-series. Data without a
#'   series column plot as one series named after the response.
#' @param lines Logical. Plot lines (default) or points.
#' @param n_bins Optional histogram bin count.
#' @param log_scale Logical. Plot the response as `log(y + 1)`
#'   on the multi-series view, where different scales are common.
#'
#' @return Invisibly, an `mvgam_data` list with elements
#'   `data` (the validated data), `family` (resolved family
#'   object), `series_levels` (`NULL` when the data name no
#'   series), `n_series`, `time_range`, `n_obs`, `n_missing`
#'   (the count of missing responses), and
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
#'                      n_timepoints = 120L)
#'
#' # Verify structure for a Poisson fit and view the multi-series
#' # plot.
#' mvgam_data(
#'   simdat$data_train,
#'   y             = "y",
#'   family        = poisson(),
#'   trend_formula = ~ AR()
#' )
#'
#' # 4-panel single-series view (time series, histogram, ACF,
#' # ECDF).
#' mvgam_data(simdat$data_train, family = poisson(), series = 1L)
#'
#' # The point of checking first: a response the family cannot
#' # take is refused here, naming the column, the offending values
#' # and the support, rather than after a Stan compile.
#' counts <- simdat$data_train
#' counts$y[3L] <- -1
#' try(mvgam_data(counts, family = poisson(), plot = FALSE))
#'
#' # The same column read as a proportion. Beta excludes both
#' # endpoints, so an exact zero has to be modelled rather than
#' # ignored, and the error names the family that does so.
#' props <- simdat$data_train
#' props$y <- props$y / (max(props$y) + 1)
#' try(mvgam_data(props, family = Beta(), plot = FALSE))
#'
#' # Irregular time is refused for a trend that assumes even
#' # spacing, and accepted for CAR(), which models the gaps.
#' gappy <- simdat$data_train[simdat$data_train$time != 5L, ]
#' try(mvgam_data(gappy, family = poisson(), trend_formula = ~ AR(),
#'                 plot = FALSE))
#' mvgam_data(gappy, family = poisson(), trend_formula = ~ CAR(),
#'             plot = FALSE)
#'
#' # A trend needs each row's series, even for a single series.
#' one <- simdat$data_train[simdat$data_train$series == "series_1", ]
#' one$series <- NULL
#' try(mvgam_data(one, family = poisson(), trend_formula = ~ AR(),
#'                 plot = FALSE))
#' one$series <- factor("series_1")
#' mvgam_data(one, family = poisson(), trend_formula = ~ AR(),
#'             plot = FALSE)
#' }
#'
#' @export
mvgam_data <- function(data,
                       y = "y",
                       family = gaussian(),
                       formula = NULL,
                       trend_formula = NULL,
                       plot = TRUE,
                       newdata = NULL,
                       series = NULL,
                       lines = TRUE,
                       n_bins = NULL,
                       log_scale = FALSE) {
  checkmate::assert_string(y)
  checkmate::assert_flag(plot)
  checkmate::assert_formula(trend_formula, null.ok = TRUE)
  if (is.list(data) && !is.data.frame(data)) {
    data <- as.data.frame(data)
  }
  checkmate::assert_data_frame(data, min.rows = 1L)
  # The family and the trend are resolved as mvgam() resolves them, so
  # a model this check passes is one mvgam() accepts.
  obs_formula <- formula %||% stats::reformulate("1", response = y)
  family <- resolve_observation_family(obs_formula, family)$family

  if (is_multi_response_family(family)) {
    stop(insight::format_error(c(
      paste0(
        "Multi-response families are not supported by ",
        "'mvgam_data()'."
      ),
      i = "mvgam() and jsdgam() validate these data themselves."
    )))
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

  if (uses_closure_unit_layout(family)) {
    # validate_closure_unit_data() already enforces required
    # columns (response + series + time + visit + cap when needed),
    # integer / non-negative / binary checks, cap >= y,
    # cap-constant-within-unit, and the >= 2 closure-unit minimum.
    validate_closure_unit_data(
      data           = data,
      response_var   = y,
      series_var     = series_var,
      time_var       = time_var,
      binary_y_check = is_binary_response_family(family),
      cap_required   = is.null(closure_unit_default_cap(family))
    )
  } else {
    validate_required_variables(
      data, required_vars = c(y, time_var), context = "mvgam_data"
    )
    validate_response_for_family(data[[y]], family, y_name = y)
  }

  # Each row's series, resolved the way mvgam() resolves it: from the
  # hierarchical grouping, else the series column. A trend refuses
  # data that name no series. Without a trend and without the column,
  # the rows plot as one series labelled by the response.
  has_series <- !is.null(trend_spec) || series_var %in% names(data)
  row_series <- function(df) {
    if (!has_series) {
      return(rep(y, nrow(df)))
    }
    attr(ensure_mvgam_variables(df, trend_spec, time_var, series_var),
         "mvgam_series")
  }
  train_series <- row_series(data)
  series_levels <- if (has_series) levels(as.factor(train_series))

  if (trend_requires_regular_intervals(trend_spec)) {
    validate_regular_time_intervals(data[[time_var]], time_var)
  }

  # Covariate NA check. Runs only when at least one formula was
  # supplied; mirrors the pre-fit guard wired into mvgam_core().
  if (!is.null(formula) || !is.null(trend_formula)) {
    validate_no_covariate_nas(
      data           = data,
      formulas       = list(formula, trend_formula),
      response_vars  = y,
      context        = "data"
    )
  }

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
    new_series <- if (!is.null(newdata)) row_series(newdata)
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


# Internal: the family and its link, as the data check names them.
#'@noRd
mvgam_data_family_label <- function(family) {
  fam_name <- resolve_family_name(family)
  if (is.null(family$link)) {
    return(fam_name)
  }
  paste0(fam_name, " (link = ", family$link, ")")
}


# Internal: the series, time and observation lines that the check
# reports and print.mvgam_data() repeats.
#'@noRd
mvgam_data_summary_lines <- function(x) {
  c(
    if (is.null(x$series_levels)) {
      "series: all rows as one"
    } else {
      paste0("series: ", x$n_series, " level(s)")
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

