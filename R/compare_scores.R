#' Tidy comparison of forecast scores across mvgam models
#'
#' @description Bundles per-series-per-horizon scores from several
#'   `mvgam_forecast` objects into one long-format `tibble`. The
#'   returned frame has one row per model x series x horizon
#'   combination and is the natural input for `ggplot2` facets,
#'   side-by-side score plots, and per-horizon score-difference
#'   summaries. For joint scores (`"energy"`, `"variogram"`,
#'   `"twenergy"`) the per-series rows are NA-valued and the joint
#'   score lives in `series == "all_series"`.
#'
#'   Replaces the manual `data.frame` stitching that would
#'   otherwise be needed when comparing many forecasts against each
#'   other.
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
#' @export
compare_scores <- function(..., score = "crps", model_names = NULL,
                            score_args = list()) {
  checkmate::assert_string(score)
  checkmate::assert_list(score_args)

  forecasts <- list(...)
  if (length(forecasts) < 2L) {
    stop(insight::format_error(c(
      "compare_scores() needs at least two mvgam_forecast objects.",
      x = paste0("Received ", length(forecasts), "."),
      i = "Pass each forecast as a separate argument."
    )))
  }
  for (i in seq_along(forecasts)) {
    if (!inherits(forecasts[[i]], "mvgam_forecast")) {
      stop(insight::format_error(c(
        paste0("Argument ", i, " is not an mvgam_forecast object."),
        x = paste0("Got class '", class(forecasts[[i]])[1L], "'.")
      )))
    }
  }

  if (is.null(model_names)) {
    model_names <- vapply(substitute(...()), deparse, character(1L))
  }
  checkmate::assert_character(model_names, len = length(forecasts),
                              any.missing = FALSE)

  per_model <- lapply(seq_along(forecasts), function(i) {
    # The local `score` arg shadows the mvgam generic of the same
    # name, so call it via the namespace to dispatch on the
    # mvgam_forecast class rather than try to invoke the score
    # string as a function.
    sc <- do.call(
      mvgam::score,
      c(list(forecasts[[i]], score = score), score_args)
    )
    # For joint scores ("energy" / "variogram" / "twenergy") the
    # per-series rows have NA scores and the joint value lives in
    # `series == "all_series"`. We retain the NA rows so users can
    # filter/facet uniformly; document that contract in the help.
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
        in_interval  = pad(df$in_interval, NA_integer_),
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
#'   objects into one long-format `tibble`. The returned frame is
#'   the natural input for ggplot trajectories that show how each
#'   model's predictive density evolves across the rolling-origin
#'   evaluation grid.
#'
#' @param ... Two or more `mvgam_lfo` objects from [lfo_cv()].
#'   Argument names are taken from the call.
#' @param model_names Optional character vector of model labels;
#'   overrides the names inferred from the call.
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
#' @export
compare_elpds <- function(..., model_names = NULL) {
  lfos <- list(...)
  if (length(lfos) < 2L) {
    stop(insight::format_error(c(
      "compare_elpds() needs at least two mvgam_lfo objects.",
      x = paste0("Received ", length(lfos), "."),
      i = "Pass each LFO result as a separate argument."
    )))
  }
  for (i in seq_along(lfos)) {
    if (!inherits(lfos[[i]], "mvgam_lfo")) {
      stop(insight::format_error(c(
        paste0("Argument ", i, " is not an mvgam_lfo object."),
        x = paste0("Got class '", class(lfos[[i]])[1L], "'.")
      )))
    }
  }

  ref_times <- lfos[[1L]]$eval_timepoints
  for (i in seq_along(lfos)) {
    if (!identical(lfos[[i]]$eval_timepoints, ref_times)) {
      stop(insight::format_error(c(
        "Cannot compare: eval_timepoints differ across LFO runs.",
        x = paste0("Run ", i, " has a different evaluation grid ",
                   "than run 1."),
        i = paste0("Re-run lfo_cv() with the same min_t and ",
                   "fc_horizon against the same data.")
      )))
    }
  }

  if (is.null(model_names)) {
    model_names <- vapply(substitute(...()), deparse, character(1L))
  }
  checkmate::assert_character(model_names, len = length(lfos),
                              any.missing = FALSE)

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
#'   Ensembles included in the `compare_scores()` call appear
#'   alongside the component models, so the same plot compares
#'   individuals and ensembles in one go.
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
#' @param ... Ignored.
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
  score_type <- attr(x, "score") %||% "score"
  joint_set <- c("energy", "variogram", "twenergy")
  is_joint <- score_type %in% joint_set
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
  y_lab <- toupper(score_type)
  if (!is.null(relative)) {
    checkmate::assert_string(relative)
    if (!relative %in% levels(df$model)) {
      stop(insight::format_error(c(
        "Baseline model not found in the compare_scores frame.",
        x = paste0("'relative' = '", relative,
                   "' is not in x$model."),
        i = paste0("Available: ",
                   paste(levels(df$model), collapse = ", "))
      )))
    }
    base <- df[df$model == relative,
               c("series", "eval_horizon", "score"),
               drop = FALSE]
    names(base)[3L] <- "base_score"
    df <- merge(df, base, by = c("series", "eval_horizon"),
                all.x = TRUE)
    df$score <- df$score - df$base_score
    df <- df[df$model != relative, , drop = FALSE]
    df$model <- droplevels(df$model)
    y_lab <- paste0("Δ ", y_lab, " vs ", relative)
  }
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$eval_horizon, y = .data$score,
                 colour = .data$model)
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 1.6) +
    ggplot2::labs(
      x = "Forecast horizon",
      y = y_lab,
      colour = NULL
    ) +
    mvgam_theme() +
    mvgam_model_colour_scale(nlevels(df$model))
  if (!is.null(relative)) {
    p <- p + ggplot2::geom_hline(yintercept = 0,
                                  linetype = "dashed",
                                  colour = "grey40")
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
#'   across folds. Useful for spotting when one model pulls
#'   ahead of another over the rolling-origin window.
#' @param ... Ignored.
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
  df <- as.data.frame(x)
  if (!is.null(relative)) {
    checkmate::assert_string(relative)
    if (!relative %in% levels(df$model)) {
      stop(insight::format_error(c(
        "Baseline model not found in the compare_elpds frame.",
        x = paste0("'relative' = '", relative,
                   "' is not in x$model."),
        i = paste0("Available: ",
                   paste(levels(df$model), collapse = ", "))
      )))
    }
    base <- df[df$model == relative,
               c("eval_time", "elpd"), drop = FALSE]
    names(base)[2L] <- "base_elpd"
    df <- merge(df, base, by = "eval_time", all.x = TRUE)
    df$elpd <- df$elpd - df$base_elpd
    df <- df[df$model != relative, , drop = FALSE]
    df$model <- droplevels(df$model)
  }
  if (isTRUE(cumulative)) {
    df <- df[order(df$model, df$eval_time), , drop = FALSE]
    df$elpd <- stats::ave(df$elpd, df$model, FUN = cumsum)
  }
  y_lab <- if (isTRUE(cumulative)) "Cumulative ELPD"
            else "Per-step ELPD"
  if (!is.null(relative)) {
    y_lab <- paste0("Δ ", y_lab, " vs ", relative)
  }
  # Recode refit_here into a readable factor so shape ends up in
  # the legend with explanatory labels rather than as raw TRUE/FALSE.
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
    ggplot2::labs(
      x = "Forecast origin (timepoint)",
      y = y_lab,
      colour = NULL,
      shape  = NULL
    ) +
    mvgam_theme() +
    mvgam_model_colour_scale(nlevels(df$model))
  if (!is.null(relative)) {
    p <- p + ggplot2::geom_hline(yintercept = 0,
                                  linetype = "dashed",
                                  colour = "grey40")
  }
  p
}


#' Wide-format summary of a `compare_scores()` table
#'
#' @description Pivots the long-format `mvgam_compare_scores`
#'   tibble to one row per evaluation cell (per series x horizon
#'   for univariate scores, per horizon for joint scores) and one
#'   column per model. The result is what you would write by hand
#'   when reading a model-vs-model score table off a page.
#'   Replaces ad-hoc `reshape()` or `pivot_wider()` calls when
#'   you want a side-by-side comparison rather than a plot.
#'
#' @param object A `mvgam_compare_scores` object from
#'   [compare_scores()].
#' @param ... Ignored.
#'
#' @return A `tibble` with one row per series x horizon cell
#'   (univariate scores) or per horizon (joint scores), one
#'   column per model carrying that model's score at the cell.
#'
#' @seealso [compare_scores()], [plot.mvgam_compare_scores()]
#'
#' @method summary mvgam_compare_scores
#' @author Nicholas J Clark
#' @export
summary.mvgam_compare_scores <- function(object, ...) {
  score_type <- attr(object, "score") %||% "score"
  joint_set <- c("energy", "variogram", "twenergy")
  is_joint <- score_type %in% joint_set
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
  # `reshape()` prefixes the new columns with "score_<model>";
  # strip the prefix so columns read as plain model names.
  names(wide) <- sub("^score_", "", names(wide))
  rownames(wide) <- NULL
  class(wide) <- c("tbl_df", "tbl", "data.frame")
  wide
}
