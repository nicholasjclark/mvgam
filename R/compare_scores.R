#' Tidy comparison of forecast scores across mvgam models
#'
#' @description Bundles per-series-per-horizon scores from several
#'   `mvgam_forecast` objects into one long-format `tibble`, with
#'   one row per model x series x horizon combination. For joint
#'   scores (`"energy"`, `"variogram"`, `"twenergy"`) the per-series
#'   scores are `NA` and the joint score is the row with
#'   `series == "all_series"`.
#'
#' @param ... Two or more `mvgam_forecast` objects (the same shape
#'   `score.mvgam_forecast()` consumes). Argument names are taken
#'   from the call and used as the `model` column. Bare expressions
#'   like `compare_scores(fc1, fc2)` produce `model` levels
#'   `"fc1"` and `"fc2"`.
#' @param score Character. Which proper scoring rule to evaluate.
#'   Forwarded to [score.mvgam_forecast()]; see its documentation
#'   for the supported values.
#' @param model_names Optional character vector of model labels;
#'   overrides the names inferred from the call.
#' @param score_args Optional named list of extra arguments passed
#'   to [score.mvgam_forecast()] (for example
#'   `score_args = list(interval_width = 0.6)`).
#'
#' @return A `tibble` with columns:
#' \itemize{
#'   \item `model` -- factor with one level per supplied forecast.
#'   \item `series` -- factor of series names (plus an
#'     `"all_series"` level when a joint score is requested).
#'   \item `eval_horizon` -- integer horizon within the test
#'     window (1, 2, ...).
#'   \item `score` -- the requested score value.
#'   \item `in_interval` -- coverage indicator carried through
#'     from [score.mvgam_forecast()].
#' }
#'
#' @author Nicholas J Clark
#'
#' @seealso [score.mvgam_forecast()],
#'   [plot.mvgam_compare_scores()], [compare_elpds()],
#'   [forecast.mvgam()], [ensemble.mvgam_forecast()]
#'
#' @template ex-forecast-models
#' @templateVar seed 1
#' @examples
#' \dontrun{
#' fc_gam <- forecast(mod_gam, newdata = simdat$data_test)
#' fc_ar <- forecast(mod_ar, newdata = simdat$data_test)
#'
#' # One row per model and horizon, scored by the discrete ranked
#' # probability score (lower is better)
#' scores <- compare_scores(fc_gam, fc_ar, score = "drps",
#'                          model_names = c("GAM", "AR"))
#' summary(scores)
#'
#' # The AR model's DRPS minus the GAM's. Negative values favour the
#' # AR model
#' plot(scores, relative = "GAM")
#'
#' # Next, ensemble() combines the two forecasts
#' }
#'
#' @family forecasting
#' @export
compare_scores <- function(..., score = "crps", model_names = NULL,
                            score_args = list()) {
  checkmate::assert_string(score)
  checkmate::assert_list(score_args)

  forecasts <- list(...)
  assert_compared_objects(forecasts, "mvgam_forecast", "compare_scores")

  model_names <- compared_object_names(forecasts, substitute(...()),
                                       model_names)

  per_model <- lapply(seq_along(forecasts), function(i) {
    # The local `score` argument shadows the generic of the same
    # name. The namespaced call dispatches on the forecast class.
    sc <- do.call(
      mvgam::score,
      c(list(forecasts[[i]], score = score), score_args)
    )
    # Joint scores keep their per-series rows with `NA` scores. Every
    # model's frame then has the same series levels to facet on.
    rows <- lapply(names(sc), function(s) {
      df <- sc[[s]]
      if (nrow(df) == 0L) return(NULL)
      n <- nrow(df)
      # Per-series rows from joint scores ("energy" / "variogram")
      # carry coverage diagnostics but no score; the all_series row
      # for univariate scores omits in_interval. Pad missing columns
      # with NAs to keep the rbind row-counts consistent.
      pad <- function(col, fill) {
        if (is.null(col)) rep(fill, n) else col
      }
      data.frame(
        model        = rep(model_names[i], n),
        series       = rep(s, n),
        eval_horizon = df$eval_horizon,
        score        = pad(df$score,       NA_real_),
        in_interval  = pad(df$in_interval, NA_real_),
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, rows)
  })
  out <- do.call(rbind, per_model)

  out$model  <- factor(out$model,  levels = model_names)
  out$series <- factor(out$series, levels = unique(out$series))
  rownames(out) <- NULL
  # Carry the score type as an attribute so plot.mvgam_compare_scores
  # can dispatch univariate vs joint without re-inspecting the rows.
  attr(out, "score") <- score
  class(out) <- c("mvgam_compare_scores", "tbl_df", "tbl",
                  "data.frame")
  out
}


#' Tidy comparison of leave-future-out ELPDs across mvgam_lfo runs
#'
#' @description Stacks the per-step ELPDs from several `mvgam_lfo`
#'   objects into one long-format `tibble`, with one row per model
#'   and evaluation time.
#'
#' @inheritParams compare_scores
#' @param ... Two or more `mvgam_lfo` objects from [lfo_cv()].
#'   Argument names are taken from the call.
#'
#' @return A `tibble` with columns:
#' \itemize{
#'   \item `model` -- factor with one level per supplied LFO run.
#'   \item `eval_time` -- the rolling-origin evaluation timepoint.
#'   \item `elpd` -- per-step expected log predictive density.
#'   \item `pareto_k` -- the Pareto-k diagnostic at that step.
#'   \item `refit_here` -- logical, TRUE when the step triggered
#'     a fresh refit (Pareto-k above threshold or initial fit).
#' }
#'
#' @author Nicholas J Clark
#'
#' @seealso [lfo_cv()], [loo_compare.mvgam_lfo()],
#'   [loo_model_weights.mvgam_lfo()],
#'   [plot.mvgam_compare_elpds()], [compare_scores()]
#'
#' @template ex-forecast-models
#' @templateVar seed 1
#' @examples
#' \dontrun{
#' # Each origin after t = 70 forecasts the next time step. The
#' # pointwise log densities feed stacking weights
#' lfo_gam <- lfo_cv(mod_gam, min_t = 70L, save_log_lik = TRUE)
#' lfo_ar <- lfo_cv(mod_ar, min_t = 70L, save_log_lik = TRUE)
#'
#' # One row per model and origin
#' elpds <- compare_elpds(lfo_gam, lfo_ar, model_names = c("GAM", "AR"))
#' head(elpds)
#'
#' # The running sum of the AR model's ELPD minus the GAM's. Hollow
#' # points mark the origins where the model was refitted
#' plot(elpds, relative = "GAM", cumulative = TRUE)
#'
#' # Next, loo_compare() sums the ELPDs with a standard error
#' }
#'
#' @family model comparison
#' @export
compare_elpds <- function(..., model_names = NULL) {
  lfos <- list(...)
  assert_compared_objects(lfos, "mvgam_lfo", "compare_elpds")

  assert_aligned_lfo(lfos)

  model_names <- compared_object_names(lfos, substitute(...()),
                                       model_names)

  rows <- lapply(seq_along(lfos), function(i) {
    m <- lfos[[i]]
    data.frame(
      model       = model_names[i],
      eval_time   = m$eval_timepoints,
      elpd        = m$elpds,
      pareto_k    = m$pareto_ks,
      refit_here  = m$refit_triggered,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out$model <- factor(out$model, levels = model_names)
  rownames(out) <- NULL
  class(out) <- c("mvgam_compare_elpds", "tbl_df", "tbl",
                  "data.frame")
  out
}


#' Plot per-model score trajectories from `compare_scores()`
#'
#' @description Draws a `ggplot2` line + point chart of forecast
#'   scores from a `mvgam_compare_scores` object. The shape
#'   adapts to the score type:
#'   \itemize{
#'     \item Univariate scores (`"crps"`, `"drps"`, `"logs"`,
#'       `"brier"`, `"sis"`, `"dss"`, `"qs"`, `"twcrps"`) plot
#'       per-series with one facet panel per series; toggle off
#'       with `facet = FALSE` to overlay every series on one
#'       panel.
#'     \item Joint multivariate scores (`"energy"`,
#'       `"variogram"`, `"twenergy"`) plot a single panel from
#'       the `all_series` rows.
#'   }
#'   An ensemble passed to `compare_scores()` is plotted as one
#'   more model and compared with its components.
#'
#' @param x A `mvgam_compare_scores` object from
#'   [compare_scores()].
#' @param relative Optional character name of a baseline model
#'   already in `x$model`. When supplied, scores are differenced
#'   against that model at each series x horizon cell and the
#'   plot adds a dashed zero line; negative values then mean
#'   "better than the baseline". The baseline model itself is
#'   dropped from the plot since its values are identically
#'   zero.
#' @param facet Logical. When `TRUE` (default) univariate scores
#'   are faceted by series; when `FALSE` series are overlaid on
#'   one panel with linetype distinguishing them. Ignored for
#'   joint scores.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A `ggplot` object.
#'
#' @seealso [compare_scores()], [score.mvgam_forecast()],
#'   [plot.mvgam_compare_elpds()],
#'   [ensemble.mvgam_forecast()]
#'
#' @method plot mvgam_compare_scores
#' @author Nicholas J Clark
#' @export
plot.mvgam_compare_scores <- function(x, relative = NULL,
                                       facet = TRUE, ...) {
  rlang::check_dots_empty()
  set_color_scheme_local()
  score_type <- attr(x, "score") %||% "score"
  is_joint <- score_type %in% joint_forecast_scores
  df <- if (is_joint) {
    subset(x, series == "all_series")
  } else {
    subset(x, series != "all_series")
  }
  if (nrow(df) == 0L) {
    stop(insight::format_error(c(
      "No rows to plot for the requested score.",
      x = paste0("score = '", score_type, "'."),
      i = "Check the source forecasts have the expected series."
    )))
  }
  y_lab <- score_label(score_type)
  if (!is.null(relative)) {
    df <- difference_from_baseline(df, relative, "score",
                                   c("series", "eval_horizon"))
    y_lab <- paste0("\u0394 ", y_lab, " vs ", relative)
  }
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$eval_horizon, y = .data$score,
                 colour = .data$model)
  ) +
    # A held-out occasion with a missing response has no score. The
    # line breaks at that occasion without a notice.
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::geom_point(size = 1.6, na.rm = TRUE) +
    scale_x_steps("Forecast horizon") +
    ggplot2::labs(y = y_lab, colour = NULL) +
    mvgam_theme() +
    mvgam_model_colour_scale(nlevels(df$model))
  if (!is.null(relative)) {
    p <- p + reference_line(yintercept = 0)
  }
  if (!is_joint && isTRUE(facet)) {
    p <- p + ggplot2::facet_wrap(~ series, scales = "free_y")
  } else if (!is_joint && !isTRUE(facet)) {
    p <- p + ggplot2::aes(linetype = .data$series)
  }
  p
}


#' Plot per-step LFO ELPDs from `compare_elpds()`
#'
#' @description Draws a `ggplot2` line + point chart of
#'   leave-future-out ELPDs over the rolling-origin evaluation
#'   grid. Each model gets a coloured line. Refit timepoints
#'   (where Pareto-k crossed the threshold and the model was
#'   refit) are highlighted with a hollow marker so the reader
#'   can see where mvgam stopped trusting the PSIS
#'   approximation.
#'
#' @param x A `mvgam_compare_elpds` object from
#'   [compare_elpds()].
#' @param relative Optional character name of a baseline model
#'   already in `x$model`. When supplied, per-step ELPDs are
#'   differenced against that model at each evaluation
#'   timepoint and the plot adds a dashed zero line; positive
#'   values then mean "better than the baseline". The baseline
#'   itself is dropped from the plot.
#' @param cumulative Logical. When `TRUE` (default `FALSE`) the
#'   y-axis becomes the cumulative ELPD up to each evaluation
#'   timepoint, which is the quantity `loo_compare()` sums
#'   across folds.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A `ggplot` object.
#'
#' @seealso [compare_elpds()], [lfo_cv()],
#'   [loo_compare.mvgam_lfo()],
#'   [plot.mvgam_compare_scores()]
#'
#' @method plot mvgam_compare_elpds
#' @author Nicholas J Clark
#' @export
plot.mvgam_compare_elpds <- function(x, relative = NULL,
                                      cumulative = FALSE, ...) {
  rlang::check_dots_empty()
  set_color_scheme_local()
  df <- as.data.frame(x)
  if (!is.null(relative)) {
    df <- difference_from_baseline(df, relative, "elpd", "eval_time")
  }
  if (isTRUE(cumulative)) {
    df <- df[order(df$model, df$eval_time), , drop = FALSE]
    df$elpd <- stats::ave(df$elpd, df$model, FUN = cumsum)
  }
  y_lab <- if (isTRUE(cumulative)) "Cumulative ELPD"
            else "Per-step ELPD"
  if (!is.null(relative)) {
    y_lab <- paste0("\u0394 ", y_lab, " vs ", relative)
  }
  # The legend labels each point shape by what happened at that step.
  df$Refit <- factor(
    ifelse(!is.na(df$refit_here) & df$refit_here,
            "refit", "PSIS reused"),
    levels = c("PSIS reused", "refit")
  )
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$eval_time, y = .data$elpd,
                 colour = .data$model)
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point(
      ggplot2::aes(shape = .data$Refit), size = 2.2
    ) +
    ggplot2::scale_shape_manual(
      values = c("PSIS reused" = 16L, "refit" = 1L)
    ) +
    scale_x_steps("Evaluation time",
                  whole = all(df$eval_time == round(df$eval_time))) +
    ggplot2::labs(y = y_lab, colour = NULL, shape = NULL) +
    mvgam_theme() +
    mvgam_model_colour_scale(nlevels(df$model))
  if (!is.null(relative)) {
    p <- p + reference_line(yintercept = 0)
  }
  p
}


#' Each model's values minus a baseline model's
#'
#' `relative` in the plots of [compare_scores()] and [compare_elpds()]
#' takes this route. The baseline's own rows are dropped, being zero.
#'
#' @param df Data frame holding a `model` factor
#' @param relative Character name of the baseline model
#' @param value Character name of the column to difference
#' @param by Character names of the columns that identify a cell
#' @return `df` without the baseline's rows, `value` differenced
#' @noRd
difference_from_baseline <- function(df, relative, value, by) {
  checkmate::assert_data_frame(df)
  checkmate::assert_string(relative)
  checkmate::assert_string(value)
  checkmate::assert_character(by, min.len = 1L)
  if (!relative %in% levels(df$model)) {
    stop(insight::format_error(c(
      "'relative' must name a model in the comparison.",
      x = paste0("Got '", relative, "'."),
      i = paste0("Available: ", paste(levels(df$model), collapse = ", "),
                 ".")
    )), call. = FALSE)
  }
  base <- df[df$model == relative, c(by, value), drop = FALSE]
  names(base)[names(base) == value] <- ".baseline"
  df <- merge(df, base, by = by, all.x = TRUE)
  df[[value]] <- df[[value]] - df$.baseline
  df <- df[df$model != relative, setdiff(names(df), ".baseline"),
           drop = FALSE]
  df$model <- droplevels(df$model)
  df
}


#' Wide-format summary of a `compare_scores()` table
#'
#' @description Pivots the long-format `mvgam_compare_scores`
#'   tibble to one row per evaluation cell (per series x horizon
#'   for univariate scores, per horizon for joint scores) and one
#'   column per model.
#'
#' @param object A `mvgam_compare_scores` object from
#'   [compare_scores()].
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A `tibble` with one row per series x horizon cell
#'   (univariate scores) or per horizon (joint scores), one
#'   column of scores per model.
#'
#' @seealso [compare_scores()], [plot.mvgam_compare_scores()]
#'
#' @method summary mvgam_compare_scores
#' @author Nicholas J Clark
#' @export
summary.mvgam_compare_scores <- function(object, ...) {
  rlang::check_dots_empty()
  score_type <- attr(object, "score") %||% "score"
  is_joint <- score_type %in% joint_forecast_scores
  df <- if (is_joint) {
    subset(object, series == "all_series",
            select = c("model", "eval_horizon", "score"))
  } else {
    subset(object, series != "all_series",
            select = c("model", "series", "eval_horizon", "score"))
  }
  idvars <- if (is_joint) "eval_horizon"
              else c("series", "eval_horizon")
  wide <- stats::reshape(
    as.data.frame(df),
    idvar     = idvars,
    timevar   = "model",
    direction = "wide",
    sep       = "_"
  )
  # `reshape()` names the new columns "score_<model>". The prefix is
  # dropped to name each column by its model.
  names(wide) <- sub("^score_", "", names(wide))
  rownames(wide) <- NULL
  class(wide) <- c("tbl_df", "tbl", "data.frame")
  wide
}


#' A label for each compared object
#'
#' `model_names` takes precedence. Each object without one takes the
#' name it was passed under, then its expression in the call.
#'
#' @param objects List of the compared objects
#' @param exprs List or pairlist of the unevaluated argument
#'   expressions, one per object
#' @param model_names Character vector of labels, one per object, or
#'   `NULL`
#' @return Character vector of labels, one per object
#' @noRd
compared_object_names <- function(objects, exprs, model_names = NULL) {
  checkmate::assert_list(objects)
  checkmate::assert(
    checkmate::check_list(exprs),
    checkmate::check_class(exprs, "pairlist"),
    checkmate::check_null(exprs)
  )
  if (!is.null(model_names)) {
    checkmate::assert_character(model_names, len = length(objects),
                                any.missing = FALSE)
    return(model_names)
  }
  given <- names(objects) %||% character(length(objects))
  ifelse(nzchar(given), given, vapply(exprs, deparse1, character(1L)))
}


#' Refuse a comparison without enough objects of one class
#'
#' Weighting accepts `min_n = 1`, giving one model all the weight.
#'
#' @param objects List of the compared objects
#' @param cls Character class every object must inherit from
#' @param fn Character name of the calling function, for the message
#' @param min_n Integer minimum number of objects
#' @return `TRUE`, invisibly, or an error
#' @noRd
assert_compared_objects <- function(objects, cls, fn, min_n = 2L) {
  checkmate::assert_list(objects)
  checkmate::assert_string(cls)
  checkmate::assert_string(fn)
  checkmate::assert_int(min_n, lower = 1L)
  if (length(objects) < min_n) {
    stop(insight::format_error(c(
      paste0("'", fn, "()' compares at least ", min_n, " '", cls,
             "' objects."),
      x = paste0("Received ", length(objects), "."),
      i = "Pass each object as a separate argument."
    )), call. = FALSE)
  }
  for (i in seq_along(objects)) {
    if (!inherits(objects[[i]], cls)) {
      stop(insight::format_error(c(
        paste0("Argument ", i, " must be an '", cls, "' object."),
        x = paste0("Got class '", class(objects[[i]])[1L], "'.")
      )), call. = FALSE)
    }
  }
  invisible(TRUE)
}
