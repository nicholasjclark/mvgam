#' Display conditional effects of predictors for mvgam models
#'
#' Plot the conditional effects of one or more numeric / categorical
#' predictors in fitted `mvgam` models, including up-to-three-way
#' interactions. A thin wrapper around
#' [marginaleffects::plot_predictions()] that auto-enumerates the
#' model's term labels (across both observation and trend submodels)
#' and produces one ggplot per term.
#'
#' @param x A fitted `mvgam` object.
#' @param effects Optional character vector of effects to plot. If
#'   `NULL` (the default), all main effects and interactions in the
#'   formula are detected automatically.
#' @param type Scale of predictions. One of `"response"` (outcome
#'   scale with observation-process noise), `"link"` (link-scale
#'   linear predictor), `"expected"` (E\[Y\]; default), or one of
#'   the family-specific scales handled by [predict.mvgam()]:
#'   `"variance"`, `"latent_state"`, `"detection"`. For closure-unit
#'   families (`occ()`, `nmix()` and variants), `"latent_state"`
#'   returns the family-aware latent state on the response scale
#'   (latent occupancy `psi` for `occ()`, latent abundance `N` for
#'   `nmix()`), which is the natural marginal-effects display for
#'   the ecological quantity of interest separate from detection
#'   probability.
#' @param points Logical or numeric. If `TRUE` (or a non-zero alpha
#'   between 0 and 1), raw observations are overlaid on the plot.
#'   The overlay applies where the drawn quantity shares the
#'   observations' scale, which covers `type = "response"` and
#'   `type = "expected"`.
#' @param rug Logical. If `TRUE`, rug tick marks are drawn along the
#'   axes, on the same scales the points overlay applies to.
#' @param process_error Logical. Passed to `plot_predictions()` /
#'   `get_predict.mvgam`. With the default `FALSE` the trend
#'   contributes its deterministic submodel. The panel then shows the
#'   covariate effect with the latent process held out. `TRUE`
#'   integrates over the trend's dynamics, which widens the band by
#'   the latent state's own spread.
#' @param series Which series the panels show. `NULL` (the default)
#'   draws one panel per effect at the reference series, as brms does
#'   for a grouping factor. A factor model whose `trend_formula` uses
#'   [lv_axis()] gives each series its own effect. `NULL` then draws a
#'   panel per series. `"all"` draws a panel per series for any model.
#'   A single integer or character value picks one series and filters
#'   the prediction grid to its observations.
#' @inheritParams forecast.mvgam
#' @param ... Additional arguments forwarded to
#'   [marginaleffects::plot_predictions()].
#'
#' @return An object of class `mvgam_conditional_effects`: a named
#'   list with one `ggplot` per effect, drawn by the `plot` and
#'   `print` methods.
#'
#' @seealso
#'   [marginaleffects::plot_predictions()] for the underlying
#'     conditional-effect engine,
#'   [marginaleffects::predictions()] for the same content as a
#'     `data.frame`,
#'   [marginaleffects::avg_slopes()],
#'   [marginaleffects::avg_comparisons()] for marginal-effect tables,
#'   [posterior_epred.mvgam()] for the prediction primitive that
#'     `plot_predictions` calls,
#'   [pp_check.mvgam()] for posterior predictive checks,
#'   \[mvgam_diagnostics\] for parameter-level diagnostics
#'   (`fixef`, `rhat`, `bayes_R2`, ...),
#'   \[mvgam_draws\] for raw draws extraction
#'
#' @references
#' Arel-Bundock, V., Greifer, N. and Heiss, A. (2024). How to
#' interpret statistical models using marginaleffects for R and
#' Python. \emph{Journal of Statistical Software}, 111(9):1-32.
#' \doi{10.18637/jss.v111.i09}
#'
#' @examples
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR())
#' mod <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1),
#'               data    = simdat$data_train,
#'               family  = poisson(),
#'               chains  = 2, silent = 2)
#'
#' # Marginal effect of x on the response scale.
#' conditional_effects(mod)
#' }
#'
#' @author Nicholas J Clark
#' @method conditional_effects mvgam
#' @export
#' @importFrom brms conditional_effects
conditional_effects.mvgam <- function(x,
                                      effects = NULL,
                                      type = "expected",
                                      points = FALSE,
                                      rug = FALSE,
                                      process_error = FALSE,
                                      series = NULL,
                                      resp = NULL,
                                      ...) {
  checkmate::assert_class(x, "mvgam")
  require_fitted_model(x, "conditional_effects")
  checkmate::assert_character(effects, null.ok = TRUE)
  checkmate::assert_logical(process_error, len = 1L)
  # `resp` selects one response of a multivariate fit. Univariate fits
  # ignore it.
  checkmate::assert_string(resp, null.ok = TRUE)

  # `resp = NULL` on a multivariate fit returns a named list with one
  # `mvgam_conditional_effects` per response, as residuals.mvgam and
  # pp_check.mvgam do. The wrapper takes the same class. `plot()`,
  # `print()` and `as.data.frame()` then accept either shape.
  fan <- mv_resp_fan_out(x, resp, class = "mvgam_conditional_effects")
  if (!is.null(fan)) {
    # Each response draws the effects its own formula and the trend
    # formula hold. A response with no such effect is left out.
    kept <- lengths(fan) > 0L
    if (!any(kept) && !is.null(effects)) {
      stop(insight::format_error(c(
        "The named effects appear in none of the model's formulas.",
        x = paste0("Named: ", paste(effects, collapse = ", "), "."),
        i = "'conditional_effects(fit)' draws every effect of each response."
      )), call. = FALSE)
    }
    attrs <- attributes(fan)
    fan <- fan[kept]
    attributes(fan) <- c(attributes(fan),
                         attrs[setdiff(names(attrs), "names")])
    return(fan)
  }
  type <- match.arg(
    type,
    c("response", "link", "expected",
      "variance", "latent_state", "detection")
  )
  # Family-specific types are valid only for the families that
  # register them on `attr(family, "mvgam_predict_types")`.
  # predict.mvgam lists a family's types when it refuses one. A family
  # without closure units is refused here, before marginaleffects
  # reports the failure in its own words.
  if (type %in% c("latent_state", "detection")) {
    require_closure_unit_predict_type(x$family, type)
  }
  # `series` is polymorphic (NULL / "all" / character / integer) so a
  # single checkmate::assert_* call cannot validate it; the resolver
  # owns the per-branch validation and surfaces typed errors.
  series_mode <- resolve_series_arg(series, x)

  # Observation rugs and overlaid points apply where the drawn
  # quantity shares the observations' scale. The fan-out above leaves
  # one response in scope on a multivariate fit, which supplies the
  # column the points come from.
  obs_scale <- on_observation_scale(type)
  points_alpha <- 0
  if (obs_scale) {
    if (isTRUE(points)) {
      points_alpha <- 0.5
    } else if (is.numeric(points) && length(points) == 1L &&
               points > 0 && points <= 1) {
      points_alpha <- as.numeric(points)
    }
  }
  if (!obs_scale) rug <- FALSE

  # Each response of a multivariate fit offers the effects of its own
  # formula and of the shared trend formula
  detected <- detect_conditional_effects(x, resp = resp)
  cond_labs <- if (is.null(effects)) {
    detected
  } else {
    lapply(as.character(effects), function(e) {
      with_series_grouping(strsplit(e, ":")[[1L]], detected,
                           axis_vars(x)$series_var)
    })
  }
  # One response of a multivariate fit draws the named effects its own
  # formula and the trend formula hold. An effect it lacks would draw
  # a flat line at the response's mean.
  if (!is.null(effects) && !is.null(resp)) {
    own_vars <- c(unlist(detected, use.names = FALSE),
                  axis_vars(x)$series_var)
    cond_labs <- cond_labs[vapply(cond_labs, function(g) {
      all(g %in% own_vars)
    }, logical(1L))]
  }

  if (any(lengths(cond_labs) > 3L)) {
    stop(insight::format_error(c(
      "Effects of order higher than 3 are not supported by conditional_effects.",
      i = cli::format_inline(paste0(
        "Use {.fn marginaleffects::plot_predictions} directly to ",
        "build the higher-order plot."
      ))
    )))
  }

  if (length(cond_labs) == 0L) {
    out <- list()
    class(out) <- "mvgam_conditional_effects"
    return(out)
  }

  # marginaleffects caps `condition` at three variables. Faceting by
  # series adds a fourth to a three-variable effect.
  if (identical(series_mode$kind, "all") &&
        any(lengths(cond_labs) >= 3L)) {
    stop(insight::format_error(c(
      "Cannot facet by series when an effect already has three variables.",
      i = "Drop one effect or omit 'series'."
    )))
  }

  # Reject collisions between named arguments mvgam controls and any
  # the user passes through `...`. Without this, R's positional-merge
  # in do.call() silently drops the user's override. Only args that
  # mvgam injects into pp_args but does NOT take as formal parameters
  # are vulnerable here: `type`, `points`, `rug` and `process_error` are
  # already consumed by the signature and cannot reach `...`.
  reserved <- c("condition", "draw", "newdata")
  dot_names <- names(list(...))
  clash <- intersect(dot_names, reserved)
  if (length(clash) > 0L) {
    stop(insight::format_error(c(
      paste0(
        "Cannot pass ",
        paste(shQuote(clash), collapse = ", "),
        " through `...`."
      ),
      i = "conditional_effects() sets these itself."
    )))
  }

  # The collision check above covers the names mvgam sets itself. The
  # rest of `...` goes to `marginaleffects::plot_predictions`, which
  # drops a name it does not use. A misspelt argument then drew the
  # panel on the default the caller meant to override.
  refuse_unread_dots(
    list(...),
    names(formals(marginaleffects::plot_predictions)),
    "conditional_effects"
  )

  # marginaleffects validates `type` against its shipped mvgam
  # `type_dictionary`, which names the latent state `latent_N`.
  # mvgam's user-facing token is `latent_state`; translate it to the
  # wire token so the upstream check passes. get_predict.mvgam maps
  # `latent_N` back to predict(type = "latent_state").
  wire_type <- if (identical(type, "latent_state")) "latent_N" else type
  # marginaleffects warns on each argument its per-class list omits,
  # and the list has no entry for mvgam. The response and the trend
  # setting go to get_predict.mvgam on this copy of the model. The
  # response makes the multivariate draws a single matrix.
  model <- x
  attr(model, "mvgam_predict_args") <- list(
    resp = resp, process_error = process_error
  )
  # With `lv_axis()` in a factor model the loadings give each series
  # its own effect of every trend term. The panels facet by series
  # unless the caller chose otherwise, wherever the data names the
  # series and the effect leaves room for one more variable.
  series_var <- axis_vars(x)$series_var
  facet_by_default <- identical(series_mode$kind, "none") &&
    isTRUE(x$trend_metadata$has_by_lv) && series_var %in% names(x$data)
  # `jsdgam()` keeps the user's species column as a second name for
  # the series. An effect of either name already shows every series.
  series_names <- c(
    series_var, jsdgam_axis_names(x)$species
  )
  out <- lapply(cond_labs, function(cond) {
    pp_args <- list(
      condition = cond,
      draw = TRUE,
      type = wire_type,
      points = points_alpha,
      rug = rug
    )
    if (identical(series_mode$kind, "all")) {
      # marginaleffects validates `condition` against the model's own
      # variables. A column added to the frame here never reaches it.
      # A fit deriving its series from `gr` and `subgr` has no such
      # variable. The refusal names that cause.
      if (!series_var %in% names(x$data)) {
        stop(insight::format_error(c(
          "Faceting by series needs a series column in the data.",
          x = "The fit derives its series from the trend grouping.",
          i = paste0(
            "Choose one series with 'series = <name>', or add a '",
            series_var, "' column to the data."
          )
        )), call. = FALSE)
      }
      pp_args$condition <- c(cond, series_var)
    } else if (facet_by_default && length(cond) < 3L &&
                 !any(series_names %in% cond)) {
      pp_args$condition <- c(cond, series_var)
    } else if (identical(series_mode$kind, "one")) {
      # Restrict the prediction grid to one series's observations.
      rows <- axis_row_series(x, x$data) %||% x$data[[series_var]]
      pp_args$newdata <- x$data[
        !is.na(rows) & rows == series_mode$level, , drop = FALSE
      ]
    }
    p <- style_effect_panel(
      do.call(marginaleffects::plot_predictions,
              c(list(model), pp_args, list(...))),
      pp_args$condition, series_var
    )
    # marginaleffects labels the y axis with the model's first
    # response. On a multivariate fit the label names `resp`.
    if (!is.null(resp)) {
      p <- p + ggplot2::labs(y = resp)
      # The overlay inside `plot_predictions()` takes the
      # observation column from `insight::find_response()`, which
      # names every response on a wide fit. `resp` names the one
      # column the overlay needs. The layer is added here, on the
      # frame the panel was given.
      if (points_alpha > 0) {
        pts <- pp_args$newdata %||% x$data
        p <- p + ggplot2::geom_point(
          data = pts,
          mapping = ggplot2::aes(
            x = .data[[cond[1L]]],
            y = .data[[response_column(x, resp)]]
          ),
          alpha = points_alpha,
          inherit.aes = FALSE
        )
      }
    }
    p
  })
  # User-visible list names hide the internal `series` / `.trend`
  # rewrite tokens that `detect_and_rewrite_by_lv()` swaps in for the
  # original `by = lv_axis()` argument. The tokens stay in the
  # `condition` passed to marginaleffects above so per-series facets
  # still render. `mvgam_had_by_lv()` is the single accessor for the
  # display-only marker persisted on `trend_metadata`;
  # `strip_by_lv_rewrite_tokens()` does the strip with the
  # empty-grouping guard.
  display_labs <- strip_by_lv_rewrite_tokens(
    cond_labs, mvgam_had_by_lv(x), axis_vars(x)$series_var
  )
  names(out) <- vapply(display_labs, paste, FUN.VALUE = character(1L),
                       collapse = ":")
  class(out) <- "mvgam_conditional_effects"
  out
}


#' Plot or print a mvgam_conditional_effects object
#' @param x An object of class `mvgam_conditional_effects`.
#' @param plot Logical. If `TRUE` (default), draws each plot;
#'   otherwise returns the list invisibly so callers can post-process.
#' @param ask Logical. If `TRUE`, prompts before each new plot.
#' @param ... Ignored.
#' @return Invisibly, a flat named list of ggplot objects, as
#'   `brms::conditional_effects()` plots return one. A multivariate
#'   fit's panels are named `<response>.<effect>`, response by
#'   response.
#' @rdname conditional_effects.mvgam
#' @export
plot.mvgam_conditional_effects <- function(x, plot = TRUE, ask = FALSE,
                                           ...) {
  plots <- conditional_effects_panels(x)
  if (isTRUE(plot) && length(plots) > 0L) {
    default_ask <- grDevices::devAskNewPage()
    on.exit(grDevices::devAskNewPage(default_ask))
    grDevices::devAskNewPage(ask = isTRUE(ask))
    for (p in plots) graphics::plot(p)
  }
  invisible(plots)
}


#' The panels of a conditional-effects object as one list
#'
#' A multivariate fit holds one set of panels per response. Each
#' panel's name takes its response as a prefix.
#'
#' @param x Object of class `mvgam_conditional_effects`
#' @return Flat named list of panels
#' @noRd
conditional_effects_panels <- function(x) {
  checkmate::assert_list(x)
  if (!isTRUE(attr(x, "mv_wrapper"))) {
    return(unclass(x))
  }
  panels <- lapply(names(x), function(r) {
    p <- conditional_effects_panels(x[[r]])
    stats::setNames(p, paste0(r, ".", names(p)))
  })
  do.call(c, panels)
}


#' @rdname conditional_effects.mvgam
#' @export
print.mvgam_conditional_effects <- function(x, ...) plot(x, ...)


#' @rdname conditional_effects.mvgam
#' @param row.names Ignored, present for S3 signature compatibility.
#' @param optional Ignored, present for S3 signature compatibility.
#' @description
#' `as.data.frame.mvgam_conditional_effects()` returns the underlying
#' prediction grid as a long-format `data.frame` (one row per grid
#' point, per effect, per response). Columns are `resp` (present only
#' when the fit is multivariate), `effect` (the primary conditioning
#' variable name), `estimate__`, `lower__`, `upper__`, followed by
#' whichever additional grid columns marginaleffects populated. The
#' three estimate columns take brms's names, as in the frame
#' [conditional_smooths.mvgam()] returns.
#'
#' Use this to reach the raw numbers when you want to build a custom
#' plot, overlay several fits, or compare against a known truth. It
#' removes the need to know whether the fit was univariate or
#' multivariate before extracting.
#' @export
#' @method as.data.frame mvgam_conditional_effects
as.data.frame.mvgam_conditional_effects <- function(x,
                                                    row.names = NULL,
                                                    optional = FALSE,
                                                    ...) {
  rlang::check_dots_empty()
  if (length(x) == 0L) return(data.frame())
  # For each entry, build a per-piece DF tagged with a label
  # column, then rbind under a shared column union. mv wrapper and
  # univariate shape only differ in what label they contribute
  # (`resp` vs `effect`) and how they resolve the entry's raw DF.
  is_mv <- isTRUE(attr(x, "mv_wrapper"))
  label_col <- if (is_mv) "resp" else "effect"
  labels <- names(x) %||% as.character(seq_along(x))
  entry_df <- function(entry) {
    if (is_mv) {
      as.data.frame.mvgam_conditional_effects(entry)
    } else {
      d <- entry$data
      if (is.null(d) || nrow(d) == 0L) return(data.frame())
      d$rowid <- NULL
      # brms names the estimate and its interval with a trailing
      # double underscore, as `conditional_smooths.mvgam()` does. The
      # ggplot keeps marginaleffects' own names, which its layers map
      # to aesthetics.
      spellings <- c(estimate = "estimate__", conf.low = "lower__",
                     conf.high = "upper__")
      hit <- intersect(names(spellings), names(d))
      names(d)[match(hit, names(d))] <- spellings[hit]
      d
    }
  }
  dfs <- Map(function(entry, lab) {
    d <- entry_df(entry)
    if (nrow(d) == 0L) return(d)
    cbind(setNames(list(lab), label_col), d)
  }, x, labels)
  dfs <- Filter(function(d) nrow(d) > 0L, dfs)
  if (length(dfs) == 0L) return(data.frame())
  # Union column sets so effects / responses with heterogeneous
  # grid columns still rbind cleanly.
  all_cols <- unique(unlist(lapply(dfs, colnames)))
  dfs <- lapply(dfs, function(d) {
    for (m in setdiff(all_cols, colnames(d))) d[[m]] <- NA
    d[, all_cols, drop = FALSE]
  })
  out <- do.call(rbind, dfs)
  rownames(out) <- NULL
  out
}


# The variable groupings `conditional_effects()` draws, one panel
# each, from the observation formula and the trend formula. `resp`
# limits the observation terms to one response of a multivariate fit.
#
# `stats::terms()` records an offset under the "offset" attribute,
# outside "term.labels". The offset reaches no grouping, as in brms.
# Its value still enters the prediction, held at the reference the
# conditioning grid gives it.
detect_conditional_effects <- function(x, resp = NULL) {
  mvgam_term_list(x, resp = resp)$groupings
}


#' A named effect, paired with the series where the model pairs it
#'
#' An effect that varies by series, such as `x:lv_axis()`, groups with
#' the series in the model's own term list. Naming the effect alone
#' drew it at the reference series. The grouping the model detects
#' takes its place. The panels then facet by series, as the default
#' call draws them.
#'
#' @param cond Character vector of the variables the caller named
#' @param detected The groupings `detect_conditional_effects()` returns
#' @param series_var The name of the series variable
#' @return `cond`, or the detected grouping that adds the series to it
#' @noRd
with_series_grouping <- function(cond, detected, series_var) {
  checkmate::assert_character(cond, min.len = 1L)
  if (series_var %in% cond) {
    return(cond)
  }
  for (grouping in detected) {
    if (series_var %in% grouping &&
        setequal(setdiff(grouping, series_var), cond)) {
      return(grouping)
    }
  }
  cond
}


# Split a single formula term label into a list of variable
# groupings suitable for plot_predictions(condition = ...). Handles
# `:` / `*` interactions, smooth wrappers (`s`, `te`, `t2`, `ti`,
# `gp`, `mo`) and bare terms. For three-variable smooths returns the
# three pairwise marginal groupings.
split_term_labels <- function(lab) {
  if (grepl(":", lab, fixed = TRUE)) {
    return(list(strsplit(lab, ":", fixed = TRUE)[[1L]]))
  }
  # A label `terms()` wrote always parses.
  expr <- rlang::parse_expr(lab)
  # `terms()` expands a formula's `a * b` into its main effects and
  # `a:b` before the label arrives here. A `*` that remains belongs to
  # an expression such as `exp(b2 * x)`, which names one covariate.
  # Splitting that label on the text gave `exp(b2 ` and ` x)`, neither
  # of them a column. The term was dropped and the model offered no
  # effects at all. Other calls go to `all.vars()` below, which
  # returns `x` from the call.
  if (is.call(expr) && identical(expr[[1L]], as.name("*"))) {
    return(list(vapply(as.list(expr)[-1L], deparse, character(1L))))
  }
  if (!is.call(expr) || !is.name(expr[[1L]]) ||
        !as.character(expr[[1L]]) %in% c(MVGAM_SMOOTH_CALLS, "mo")) {
    return(list(all.vars(expr)))
  }
  # A smooth names its covariates as unnamed arguments and its
  # grouping as `by`; every other argument is a setting. The function
  # takes both from the unevaluated call. Evaluating the call needed
  # every setting to resolve: `k = kk` failed and reported `kk` as a
  # covariate. Evaluation also returned `by` as deparsed text.
  # `by = interaction(a, b)` then offered a column named after the
  # call.
  args <- as.list(expr)[-1L]
  arg_names <- names(args) %||% character(length(args))
  by_vars <- if ("by" %in% arg_names) all.vars(args[["by"]]) else NULL
  term_vars <- lapply(args[!nzchar(arg_names)], all.vars)
  if (length(term_vars) <= 2L) {
    return(list(c(unlist(term_vars), by_vars)))
  }
  lapply(list(1:2, c(1L, 3L), 2:3), function(pair) {
    c(unlist(term_vars[pair]), by_vars)
  })
}


# Resolve the user-facing `series` argument into a structured mode
# the dispatch loop can branch on. Returns a list with
#   kind   = "none" | "all" | "one"
#   level  = NA_character_ | <resolved series factor level>
# Validates against the model's series factor levels and surfaces a
# targeted error when a non-existent level is requested.
resolve_series_arg <- function(series, x) {
  if (is.null(series)) {
    return(list(kind = "none", level = NA_character_))
  }
  # The axis record names the series. A fit whose series comes from
  # `gr` and `subgr` has no series column. The record holds its
  # series all the same.
  series_levels <- mvgam_axes(x)$series$levels
  if (!length(series_levels)) {
    stop(insight::format_error(c(
      "'series' selects among the series a model records.",
      x = "The fit records an empty series axis.",
      i = "Drop the 'series' argument for a fit with one time series."
    )))
  }
  if (length(series) != 1L) {
    stop(insight::format_error(
      "'series' must be NULL, 'all', a series name or a 1-based index."
    ))
  }
  if (identical(series, "all")) {
    return(list(kind = "all", level = NA_character_))
  }
  if (is.numeric(series)) {
    n_levels <- length(series_levels)
    checkmate::assert_integerish(series, len = 1L)
    if (series < 1L || series > n_levels) {
      stop(insight::format_error(c(
        "'series' index is out of range.",
        x = paste0("Got ", series, ", valid indices run from 1 to ",
                   n_levels, "."),
        i = paste0(
          "Available levels: ",
          paste(shQuote(series_levels), collapse = ", "), "."
        )
      )))
    }
    return(list(kind = "one", level = series_levels[as.integer(series)]))
  }
  if (is.character(series)) {
    if (!series %in% series_levels) {
      stop(insight::format_error(c(
        "'series' is not one of the model's series levels.",
        x = paste0("Got: '", series, "'."),
        i = paste0(
          "Available: ",
          paste(shQuote(series_levels), collapse = ", "), "."
        )
      )))
    }
    return(list(kind = "one", level = series))
  }
  stop(insight::format_error(
    "'series' must be NULL, 'all', a series name or a 1-based index."
  ))
}


# Draw a `plot_predictions()` panel as the other mvgam effect plots
# are drawn. marginaleffects colours a second conditioning variable's
# levels on one panel. Where that variable is the series, each series
# is a panel of its own with its own y scale, since series counts can
# differ by an order of magnitude. Any other second variable keeps its
# colours, taken from the active scheme.
#'@noRd
style_effect_panel <- function(p, condition, series_var) {
  set_color_scheme_local("red")
  group_var <- if (length(condition) >= 2L) condition[[2L]] else NULL
  by_series <- identical(group_var, series_var)
  for (layer in p$layers) {
    if (by_series) {
      layer$mapping[c("colour", "fill")] <- NULL
    }
    # An occasion with a missing response has no point to draw.
    if (inherits(layer$geom, "GeomPoint")) {
      layer$geom_params$na.rm <- TRUE
    }
    if (any(c("colour", "fill") %in% names(layer$mapping))) {
      next
    }
    if (inherits(layer$geom, "GeomRibbon")) {
      layer$aes_params$fill <- mvgam_colour("mid")
      layer$aes_params$alpha <- 0.5
    } else if (inherits(layer$geom, "GeomPoint") &&
                 !inherits(layer$geom, "GeomPointrange")) {
      layer$aes_params$colour <- "black"
    } else {
      layer$aes_params$colour <- mvgam_colour("dark")
    }
  }
  if (by_series) {
    # marginaleffects labels the colour and fill it mapped. The facet
    # strips name the series instead. A label without a mapping draws
    # a notice.
    p$labels[c("colour", "fill")] <- NULL
    return(p + ggplot2::facet_wrap(ggplot2::vars(.data[[series_var]]),
                                   scales = "free_y") +
             mvgam_theme())
  }
  if (!is.null(group_var)) {
    p <- p + mvgam_model_colour_scale(
      length(unique(p$data[[group_var]])), name = group_var,
      aesthetics = c("colour", "fill"), labels = round_legend_labels
    )
  }
  p + mvgam_theme()
}


# Legend labels for a numeric grouping. Integers print as they are.
# Other values round to the fewest decimals, at most four, that tell
# the labels apart. Floating-point noise then stays out of the legend.
round_legend_labels <- function(x) {
  numeric_x <- utils::type.convert(as.character(x), as.is = TRUE)
  if (!is.numeric(numeric_x)) {
    return(x)
  }
  decimals <- vapply(numeric_x, function(v) {
    if (is.na(v) ||
        abs(v - round(v)) <= .Machine$double.eps^0.5) {
      return(0L)
    }
    parts <- strsplit(sub("0+$", "", format(v, scientific = FALSE)),
                      ".", fixed = TRUE)[[1L]]
    if (length(parts) < 2L) 0L else nchar(parts[2L])
  }, integer(1L))
  n_dec <- min(max(decimals, na.rm = TRUE), 4L)
  if (n_dec == 0L) {
    return(format(numeric_x, scientific = FALSE))
  }
  sprintf(paste0("%.", n_dec, "f"), numeric_x)
}
